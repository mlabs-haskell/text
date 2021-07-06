#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <emmintrin.h>
#include <smmintrin.h>

static inline bool is_ascii_sse2 (uint8_t const * const src,
                                  size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 16 bytes times 8 = 128.
  size_t const big_strides = len / 128;
  size_t const small_strides = len % 128;
  uint8_t const * ptr = (uint8_t const *)src;
  for (size_t i = 0; i < big_strides; i++) {
    __m128i const * big_ptr = (__m128i const *)ptr;
    // A non-ASCII byte will have its MSB set. Given that bitwise OR preserves
    // 1-bits, we will only have the MSB set in any lane if that lane in any of
    // our inputs had a set MSB.
    __m128i const results = 
        _mm_or_si128(_mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr),
                                               _mm_loadu_si128(big_ptr + 1)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 2),
                                               _mm_loadu_si128(big_ptr + 3))),
                     _mm_or_si128(_mm_or_si128(_mm_loadu_si128(big_ptr + 4),
                                               _mm_loadu_si128(big_ptr + 5)),
                                  _mm_or_si128(_mm_loadu_si128(big_ptr + 6),
                                               _mm_loadu_si128(big_ptr + 7))));
    // Due to the above, we can evacuate the MSBs directly. If we end up with
    // 0x0000, we found only ASCII bytes.
    if (_mm_movemask_epi8(results) != 0) {
      return false;
    }
    ptr += 128;
  }
  // Finish what's left slowly.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return false;
    }
    ptr++;
  }
  // If we made it this far, we're valid.
  return true;
}

static inline ptrdiff_t find_invalid_utf8_fallback (uint8_t const * const src,
                                                    size_t const len) {
  uint8_t const * ptr = (uint8_t const *)src;
  // This is 'one past the end' to make loop termination and bounds checks
  // easier.
  uint8_t const * const end = ptr + len;
  while (ptr < end) {
    uint8_t const byte = *ptr;
    // Check for ASCII.
    if (byte <= 0x7F) {
      ptr++;
    }
    // Check for a valid 2-byte sequence.
    //
    // We use a signed comparison to avoid an extra comparison with 0x80, since
    // _signed_ 0x80 is -128.
    else if (ptr + 1 < end && byte >= 0xC2 && byte <= 0xDF &&
             ((int8_t)*(ptr + 1)) <= (int8_t)0xBF) {
      ptr += 2;
    }
    // Check for a valid 3-byte sequence.
    else if (ptr + 2 < end) {
      uint8_t const byte2 = *(ptr + 1);
      bool byte2_valid = (int8_t)byte2 <= (int8_t)0xBF;
      bool byte3_valid = ((int8_t)*(ptr + 2)) <= (int8_t)0xBF;
      if (byte2_valid && byte3_valid && 
          // E0, A0..BF, 80..BF
          ((byte == 0xE0 && byte2 >= 0xA0) ||
          // E1..EC, 80..BF, 80..BF
           (byte >= 0xE1 && byte <= 0xEC) ||
          // ED, 80..9F, 80..BF
           (byte == 0xED && byte2 <= 0x9F) ||
          // EE..EF, 80..BF, 80..BF
           (byte >= 0xEE && byte <= 0xEF))) {
        ptr += 3;
      }
      else {
        return ptr - src;
      }
    }
    // Check for a valid 4-byte sequence
    else if (ptr + 3 < end) {
      uint8_t const byte2 = *(ptr + 1);
      bool byte2_valid = (int8_t)byte2 <= (int8_t)0xBF;
      bool byte3_valid = ((int8_t)*(ptr + 2)) <= (int8_t)0xBF;
      bool byte4_valid = ((int8_t)*(ptr + 3)) <= (int8_t)0xBF;
      if (byte2_valid && byte3_valid && byte4_valid &&
          // F0, 90..BF, 80..BF, 80..BF
          ((byte == 0xF0 && byte2 >= 0x90) ||
          // F1..F3, 80..BF, 80..BF, 80..BF
           (byte >= 0xF1 && byte <= 0xF3) ||
          // F4, 80..8F, 80..BF, 80..BF
           (byte == 0xF4 && byte2 <= 0x8F))) {
        ptr += 4;
      }
      else {
        return ptr - src;
      }
    }
    // Otherwise, invalid.
    else {
      return ptr - src;
    }
  }
  // If we got this far, we're valid.
  return -1;
}

static int8_t const first_len_lookup[16] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
};

static int8_t const first_range_lookup[16] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
};

static int8_t const range_min_lookup[16] = {
  0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
  0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F,
};

static int8_t const range_max_lookup[16] = {
  0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
  0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

static int8_t const df_ee_lookup[16] = {
  0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
};

static int8_t const ef_fe_lookup[16] = {
  0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

__attribute__((target("ssse3")))
static inline ptrdiff_t find_invalid_utf8_ssse3 (uint8_t const * const src,
                                                 size_t const len) {
  // We step two SIMD registers at a time.
  size_t const strides = len / 32;
  size_t const remaining_len = len % 32;
  uint8_t const * ptr = (uint8_t const *)src;
  __m128i prev_input = _mm_setzero_si128();
  __m128i prev_first_len = _mm_setzero_si128();
  // Load our lookup tables.
  __m128i const first_len_tbl = 
    _mm_loadu_si128((__m128i const *)first_len_lookup);
  __m128i const first_range_tbl = 
    _mm_loadu_si128((__m128i const *)first_range_lookup);
  __m128i const range_min_tbl = 
    _mm_loadu_si128((__m128i const *)range_min_lookup);
  __m128i const range_max_tbl = 
    _mm_loadu_si128((__m128i const *)range_max_lookup);
  __m128i const df_ee_tbl = 
    _mm_loadu_si128((__m128i const *)df_ee_lookup);
  __m128i const ef_fe_tbl = 
    _mm_loadu_si128((__m128i const *)ef_fe_lookup);
  __m128i errors = _mm_setzero_si128();
  for (size_t i = 0; i < strides; i++) {
    const __m128i input_a = _mm_loadu_si128((const __m128i *)ptr);
    __m128i high_nibbles =
      _mm_and_si128(_mm_srli_epi16(input_a, 4), _mm_set1_epi8(0x0F));
    __m128i first_len_a = _mm_shuffle_epi8(first_len_tbl, high_nibbles);
    __m128i range_a = _mm_shuffle_epi8(first_range_tbl, high_nibbles);
    range_a = _mm_or_si128(
        range_a, _mm_alignr_epi8(first_len_a, prev_first_len, 15));
    __m128i tmp;
    tmp = _mm_alignr_epi8(first_len_a, prev_first_len, 14);
    tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(1));
    range_a = _mm_or_si128(range_a, tmp);
    tmp = _mm_alignr_epi8(first_len_a, prev_first_len, 13);
    tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(2));
    range_a = _mm_or_si128(range_a, tmp);
    __m128i shift1, pos, range2;
    shift1 = _mm_alignr_epi8(input_a, prev_input, 15);
    pos = _mm_sub_epi8(shift1, _mm_set1_epi8(0xEF));
    tmp = _mm_subs_epu8(pos, _mm_set1_epi8(0xF0));
    range2 = _mm_shuffle_epi8(df_ee_tbl, tmp);
    tmp = _mm_adds_epu8(pos, _mm_set1_epi8(0x70));
    range2 = _mm_add_epi8(range2, _mm_shuffle_epi8(ef_fe_tbl, tmp));
    range_a = _mm_add_epi8(range_a, range2);
    __m128i minv = _mm_shuffle_epi8(range_min_tbl, range_a);
    __m128i maxv = _mm_shuffle_epi8(range_max_tbl, range_a);
    tmp = _mm_or_si128(_mm_cmplt_epi8(input_a, minv),
                       _mm_cmpgt_epi8(input_a, maxv));
    errors = _mm_or_si128(errors, tmp);
    const __m128i input_b = _mm_loadu_si128((const __m128i *)(ptr+16));
    high_nibbles = _mm_and_si128(_mm_srli_epi16(input_b, 4), _mm_set1_epi8(0x0F));
    __m128i first_len_b = _mm_shuffle_epi8(first_len_tbl, high_nibbles);
    __m128i range_b = _mm_shuffle_epi8(first_range_tbl, high_nibbles);
    range_b = _mm_or_si128(range_b, 
        _mm_alignr_epi8(first_len_b, first_len_a, 15));
    tmp = _mm_alignr_epi8(first_len_b, first_len_a, 14);
    tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(1));
    range_b = _mm_or_si128(range_b, tmp);
    tmp = _mm_alignr_epi8(first_len_b, first_len_a, 13);
    tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(2));
    range_b = _mm_or_si128(range_b, tmp);
    shift1 = _mm_alignr_epi8(input_b, input_a, 15);
    pos = _mm_sub_epi8(shift1, _mm_set1_epi8(0xEF));
    tmp = _mm_subs_epu8(pos, _mm_set1_epi8(0xF0));
    range2 = _mm_shuffle_epi8(df_ee_tbl, tmp);
    tmp = _mm_adds_epu8(pos, _mm_set1_epi8(0x70));
    range2 = _mm_add_epi8(range2, _mm_shuffle_epi8(ef_fe_tbl, tmp));
    range_b = _mm_add_epi8(range_b, range2);
    minv = _mm_shuffle_epi8(range_min_tbl, range_b);
    maxv = _mm_shuffle_epi8(range_max_tbl, range_b);
    tmp = _mm_or_si128(_mm_cmplt_epi8(input_b, minv),
                       _mm_cmpgt_epi8(input_b, maxv));
    errors = _mm_or_si128(errors, tmp);
    // Save our last results, advance.
    prev_input = input_b;
    prev_first_len = first_len_b;
    ptr += 32;
  }
  uint64_t results[2];
  _mm_storeu_si128((__m128i*)results, errors);
  // If we have non-zero anywhere, then it's an error, dig manually.
  if (results[0] != 0 || results[1] != 0) {
    return find_invalid_utf8_fallback(src, len);
  }
  // 'Roll back' our pointer a little to prepare for a slow search of the rest.
  int16_t tokens[2];
  tokens[0] = _mm_extract_epi16(prev_input, 6);
  tokens[1] = _mm_extract_epi16(prev_input, 7);
  int8_t const * token_ptr = (int8_t const*)tokens;
  ptrdiff_t lookahead = 0;
  if (token_ptr[3] > (int8_t)0xBF) {
    lookahead = 1;
  }
  else if (token_ptr[2] > (int8_t)0xBF) {
    lookahead = 2;
  }
  else if (token_ptr[1] > (int8_t)0xBF) {
    lookahead = 3;
  }
  uint8_t const * const small_ptr = ptr - lookahead;
  size_t const small_len = remaining_len + lookahead;
  ptrdiff_t small_result = find_invalid_utf8_fallback(small_ptr, small_len);
  if (small_result == -1) {
    return -1;
  }
  return (small_ptr - src) + small_result;
}

// Returns an index into src where the invalid UTF-8 sequence started, or -1 if
// no such sequence was found.
ptrdiff_t find_invalid_utf8 (uint8_t const * const src,
                             size_t const len) {
  __builtin_cpu_init();
  if (is_ascii_sse2(src, len)) {
    return -1;
  }
  if (__builtin_cpu_supports("ssse3")) {
    return find_invalid_utf8_ssse3(src, len);
  }
  return find_invalid_utf8_fallback(src, len);
}
