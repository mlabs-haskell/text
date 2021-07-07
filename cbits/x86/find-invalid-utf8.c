#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>

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

__attribute__((target("avx,avx2")))
static inline bool is_ascii_avx2 (uint8_t const * const src,
                                  size_t const len) {
  // We process 8 SIMD registers' worth of data at once.
  // That's 32 bytes times 8 = 256.
  size_t const big_strides = len / 256;
  size_t const small_strides = len % 256;
  uint8_t const * ptr = (uint8_t const *)src;
  __m256i high_bit_masks = _mm256_set1_epi8(0x80);
  for (size_t i = 0; i < big_strides; i++) {
    __m256i const * big_ptr = (__m256i const *)ptr;
    // A non-ASCII byte will have its MSB set. Given that bitwise OR preserves
    // 1-bits, we will only have the MSB set in any lane if that lane in any of
    // our inputs had a set MSB.
    __m256i const results = 
        _mm256_or_si256(_mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr),
                                                        _mm256_lddqu_si256(big_ptr + 1)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 2),
                                                        _mm256_lddqu_si256(big_ptr + 3))),
                        _mm256_or_si256(_mm256_or_si256(_mm256_lddqu_si256(big_ptr + 4),
                                                        _mm256_lddqu_si256(big_ptr + 5)),
                                        _mm256_or_si256(_mm256_lddqu_si256(big_ptr + 6),
                                                        _mm256_lddqu_si256(big_ptr + 7))));
    // Check if any MSB is set. testz returns 1 if the AND of its arguments is
    // 0, and 0 otherwise. If we AND with 0x80 in every lane, we will only get 0
    // if there are no set MSBs anywhere.
    if (_mm256_testz_si256(results, high_bit_masks) == 0) {
      return false;
    }
    ptr += 256;
  }
  // Finish what's left slowly.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) > 0x7F) {
      return false;
    }
    ptr++;
  }
  // If we made it this far, we're valid.
  return -1;
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

/*
 * Map high nibble of "First Byte" to legal character length minus 1
 * 0x00 ~ 0xBF --> 0
 * 0xC0 ~ 0xDF --> 1
 * 0xE0 ~ 0xEF --> 2
 * 0xF0 ~ 0xFF --> 3
 */
static int8_t const _first_len_tbl[32] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3,
};

/* Map "First Byte" to 8-th item of range table (0xC2 ~ 0xF4) */
static int8_t const _first_range_tbl[32] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8,
};

/*
 * Range table, map range index to min and max values
 * Index 0    : 00 ~ 7F (First Byte, ascii)
 * Index 1,2,3: 80 ~ BF (Second, Third, Fourth Byte)
 * Index 4    : A0 ~ BF (Second Byte after E0)
 * Index 5    : 80 ~ 9F (Second Byte after ED)
 * Index 6    : 90 ~ BF (Second Byte after F0)
 * Index 7    : 80 ~ 8F (Second Byte after F4)
 * Index 8    : C2 ~ F4 (First Byte, non ascii)
 * Index 9~15 : illegal: i >= 127 && i <= -128
 */
static int8_t const _range_min_tbl[32] = {
    0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
    0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F,
    0x00, 0x80, 0x80, 0x80, 0xA0, 0x80, 0x90, 0x80,
    0xC2, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F,
};

static int8_t const _range_max_tbl[32] = {
    0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
    0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x7F, 0xBF, 0xBF, 0xBF, 0xBF, 0x9F, 0xBF, 0x8F,
    0xF4, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

/*
 * Tables for fast handling of four special First Bytes(E0,ED,F0,F4), after
 * which the Second Byte are not 80~BF. It contains "range index adjustment".
 * +------------+---------------+------------------+----------------+
 * | First Byte | original range| range adjustment | adjusted range |
 * +------------+---------------+------------------+----------------+
 * | E0         | 2             | 2                | 4              |
 * +------------+---------------+------------------+----------------+
 * | ED         | 2             | 3                | 5              |
 * +------------+---------------+------------------+----------------+
 * | F0         | 3             | 3                | 6              |
 * +------------+---------------+------------------+----------------+
 * | F4         | 4             | 4                | 8              |
 * +------------+---------------+------------------+----------------+
 */
/* index1 -> E0, index14 -> ED */
static int8_t const _df_ee_tbl[32] = {
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
    0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
};
/* index1 -> F0, index5 -> F4 */
static int8_t const _ef_fe_tbl[32] = {
    0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

__attribute__((target("avx,avx2")))
static inline __m256i push_last_byte_of_a_to_b(__m256i const a, 
                                               __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 15);
}

__attribute__((target("avx,avx2")))
static inline __m256i push_last_2bytes_of_a_to_b(__m256i const a, 
                                                 __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 14);
}

__attribute__((target("avx,avx2")))
static inline __m256i push_last_3bytes_of_a_to_b(__m256i const a, 
                                                 __m256i const b) {
  return _mm256_alignr_epi8(b, _mm256_permute2x128_si256(a, b, 0x21), 13);
}

__attribute__((target("avx,avx2")))
static inline ptrdiff_t find_invalid_utf8_avx2 (uint8_t const * const src,
                                                size_t const len) {
  // We don't bother unrolling this, as AVX registers are twice as wide anyway.
  size_t const strides = len / 32;
  size_t const remaining_len = len % 32;
  uint8_t const * ptr = (uint8_t const *)src;
  __m256i prev_input = _mm256_setzero_si256();
  __m256i prev_first_len = _mm256_setzero_si256();
  // Load our lookup tables.
  __m256i const first_len_tbl = 
    _mm256_loadu_si256((__m256i const *)_first_len_tbl);
  __m256i const first_range_tbl =
    _mm256_loadu_si256((__m256i const *)_first_range_tbl);
  __m256i const range_min_tbl =
    _mm256_loadu_si256((__m256i const *)_range_min_tbl);
  __m256i const range_max_tbl =
    _mm256_loadu_si256((__m256i const *)_range_max_tbl);
  __m256i const df_ee_tbl =
    _mm256_loadu_si256((__m256i const *)_df_ee_tbl);
  __m256i const ef_fe_tbl =
    _mm256_loadu_si256((__m256i const *)_ef_fe_tbl);
  __m256i errors1 = _mm256_setzero_si256();
  __m256i errors2 = _mm256_setzero_si256();
  for (size_t i = 0; i < strides; i++) {
    __m256i const input = _mm256_lddqu_si256((__m256i const *)ptr);
    __m256i const high_nibbles = 
      _mm256_and_si256(_mm256_srli_epi16(input, 4), _mm256_set1_epi8(0x0F));
    // Look up in our tables using the high nibbles of the input.
    __m256i first_len = _mm256_shuffle_epi8(first_len_tbl, high_nibbles);
    __m256i range = _mm256_shuffle_epi8(first_range_tbl, high_nibbles);
    // Second byte: set range index to first_len
    // 0 for 00~7F, 1 for C0~DF, 2 for E0~EF, 3 for F0~FF
    range = _mm256_or_si256(
        range, push_last_byte_of_a_to_b(prev_first_len, first_len));
    // Third byte: set range index to saturate_sub(first_len, 1)
    // 0 for 00~7F, 0 for C0~DF, 1 for E0~EF, 2 for F0~FF
    __m256i tmp1 = push_last_2bytes_of_a_to_b(prev_first_len, first_len);
    __m256i tmp2 = _mm256_subs_epu8(tmp1, _mm256_set1_epi8(1));
    range = _mm256_or_si256(range, tmp2);
    // Fourth byte: set range index to saturate_sub(first_len, 2)
    // 0 for 00~7F, 0 for C0~DF, 0 for E0~EF, 1 for F0~FF
    tmp1 = push_last_3bytes_of_a_to_b(prev_first_len, first_len);
    tmp2 = _mm256_subs_epu8(tmp1, _mm256_set1_epi8(2));
    range = _mm256_or_si256(range, tmp2);
    // Adjust second byte range for special first bytes (E0,ED,F0,F4).
    // Overlaps lead to indices from 9 to 15, which are illegal in the range table.
    __m256i shift1 = push_last_byte_of_a_to_b(prev_input, input);
    __m256i pos = _mm256_sub_epi8(shift1, _mm256_set1_epi8(0xEF));
    tmp1 = _mm256_subs_epu8(pos, _mm256_set1_epi8(240));
    __m256i range2 = _mm256_shuffle_epi8(df_ee_tbl, tmp1);
    tmp2 = _mm256_adds_epu8(pos, _mm256_set1_epi8(112));
    range2 = _mm256_add_epi8(range2, _mm256_shuffle_epi8(ef_fe_tbl, tmp2));
    range = _mm256_add_epi8(range, range2);
    // Load our calculated min and max ranges.
    __m256i minv = _mm256_shuffle_epi8(range_min_tbl, range);
    __m256i maxv = _mm256_shuffle_epi8(range_max_tbl, range);
    // Check we're in range, accumulating errors if not.
    errors1 = _mm256_or_si256(errors1, _mm256_cmpgt_epi8(minv, input));
    errors2 = _mm256_or_si256(errors2, _mm256_cmpgt_epi8(input, maxv));
    // Save for next step.
    prev_input = input;
    prev_first_len = first_len;
    ptr += 32;
  }
  // Collect our error values, then check if we have a non-zero. If we do,
  // something went wrong and we have to find the position manually.
  __m256i errors = _mm256_or_si256(errors1, errors2);
  if (_mm256_testz_si256(errors, errors) != 1) {
    return find_invalid_utf8_fallback(src, len);
  }
  // 'Roll back' our pointer a little to prepare for a slow search of the rest.
  uint32_t tokens_blob = _mm256_extract_epi32(prev_input, 7);
  int8_t const * tokens = (int8_t const *)&tokens_blob;
  ptrdiff_t lookahead = 0;
  if (tokens[3] > (int8_t)0xBF) {
    lookahead = 1;
  }
  else if (tokens[2] > (int8_t)0xBF) {
    lookahead = 2;
  }
  else if (tokens[1] > (int8_t)0xBF) {
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
  if (__builtin_cpu_supports("avx2")) {
    if (is_ascii_avx2(src, len)) {
      return -1;
    }
    return find_invalid_utf8_avx2(src, len);
  }
  if (is_ascii_sse2(src, len)) {
    return -1;
  }
  if (__builtin_cpu_supports("ssse3")) {
    return find_invalid_utf8_ssse3(src, len);
  }
  return find_invalid_utf8_fallback(src, len);
}
