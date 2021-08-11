#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <emmintrin.h>
#include <tmmintrin.h>

// Fallback (SSE2)

static inline int is_valid_utf8_fallback (uint8_t const * const src,
                                          size_t const len) {
  uint8_t const * ptr = (uint8_t const*)src;
  __m128i const high_bits_mask = _mm_set1_epi8(0x80);
  // This is 'one past the end' to make loop termination and bounds checks
  // easier.
  uint8_t const * const end = ptr + len;
  while (ptr < end) {
    uint8_t const byte = *ptr;
    // Check if the byte is ASCII.
    if (byte <= 0x7F) {
      ptr++;
      // If we saw one ASCII byte, as long as it's not whitespace, it's quite
      // likely we'll see more.
      bool is_not_whitespace = byte > 32;
      // If possible, do a block-check ahead.
      if ((ptr + 64 < end) && is_not_whitespace) {
        __m128i const * big_ptr = (__m128i const *)ptr;
        // Non-ASCII bytes have a set MSB. Thus, if we AND with 0x80 in every
        // lane, we will get 0x00 in the corresponding lane if it's an ASCII
        // byte, and 0x80 otherwise.
        //
        // We then evacuate the MSBs.
        uint16_t result = _mm_movemask_epi8(
            _mm_and_si128(high_bits_mask, _mm_loadu_si128(big_ptr)));
        if (result == 0) {
          ptr += 16;
          // Try one more.
          result = _mm_movemask_epi8(
              _mm_and_si128(high_bits_mask, _mm_loadu_si128(big_ptr + 1)));
          if (result == 0) {
            ptr += 16;
            // And one more.
            result = _mm_movemask_epi8(
                _mm_and_si128(high_bits_mask, _mm_loadu_si128(big_ptr + 2)));
            if (result == 0) {
              ptr += 16;
              // Last one.
              result = _mm_movemask_epi8(
                  _mm_and_si128(high_bits_mask, _mm_loadu_si128(big_ptr + 3)));
              if (result == 0) {
                ptr += 16;
              }
              else {
                ptr += __builtin_ctz(result);
              }
            }
            else {
              ptr += __builtin_ctz(result);
            }
          }
          else {
            ptr += __builtin_ctz(result);
          }
        }
        else {
          ptr += __builtin_ctz(result);
        }
      }
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
        return 0;
      }
    }
    // Check for a valid 4-byte sequence.
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
        return 0;
      }
    }
    // Otherwise, invalid.
    else {
      return 0;
    }
  }
  // If we got this far, we're valid.
  return 1;
}

// SSSE3

// Lookup tables

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
static inline bool is_ascii_sse2 (__m128i const * src) {
  // OR together everything, then check for a high bit anywhere.
  __m128i const ored = _mm_or_si128(_mm_or_si128(src[0], src[1]),
                                    _mm_or_si128(src[2], src[3]));
  return (_mm_movemask_epi8(ored) == 0);
}

__attribute__((target("ssse3")))
static inline __m128i check_block_sse3 (__m128i* prev_input_ptr,
                                        __m128i* prev_first_len_ptr,
                                        __m128i const errors,
                                        __m128i const first_len_tbl,
                                        __m128i const first_range_tbl,
                                        __m128i const range_min_tbl,
                                        __m128i const range_max_tbl,
                                        __m128i const df_ee_tbl,
                                        __m128i const ef_fe_tbl,
                                        __m128i const input) {
  // Get the high 4-bits of the input.
  __m128i const high_nibbles = 
    _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0F));
  // Compute first lengths.
  // This is 0 for [00, 7F], 1 for [C0, DF], 2 for [E0, EF], 3 for [F0, FF].
  // We do this by table lookup.
  __m128i const first_len = _mm_shuffle_epi8(first_len_tbl, high_nibbles);
  // Set range index to 8 for bytes in [C0, FF] by lookup (first byte).
  __m128i range = _mm_shuffle_epi8(first_range_tbl, high_nibbles);
  // Reduce the range index based on first_len (second byte)
  // This is 0 for [00, 7F], 1 for [C0, DF], 2 for [E0, EF], 3 for [F0, FF].
  range = _mm_or_si128(
      range, _mm_alignr_epi8(first_len, (*prev_first_len_ptr), 15));
  // Set range index to the saturation of (first_len - 1) (third byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 1 for [E0, EF], 2 for [F0, FF].
  __m128i tmp = _mm_alignr_epi8(first_len, (*prev_first_len_ptr), 14);
  tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(1));
  range = _mm_or_si128(range, tmp);
  // Set range index to the saturation of (first_len - 2) (fourth byte).
  // This is 0 for [00, 7F], 0 for [C0, DF], 0 for [E0, EF] and 1 for [F0, FF].
  tmp = _mm_alignr_epi8(first_len, (*prev_first_len_ptr), 13);
  tmp = _mm_subs_epu8(tmp, _mm_set1_epi8(2));
  range = _mm_or_si128(range, tmp);
  // At this stage, we have calculated range indices correctly, except for
  // special cases for first bytes (E0, ED, F0, F4). We repair this to avoid
  // missing in the range table.
  __m128i const shift1 = _mm_alignr_epi8(input, (*prev_input_ptr), 15);
  __m128i const pos = _mm_sub_epi8(shift1, _mm_set1_epi8(0xEF));
  tmp = _mm_subs_epu8(pos, _mm_set1_epi8(0xF0));
  __m128i range2 = _mm_shuffle_epi8(df_ee_tbl, tmp);
  tmp = _mm_adds_epu8(pos, _mm_set1_epi8(0x70));
  range2 = _mm_add_epi8(range2, _mm_shuffle_epi8(ef_fe_tbl, tmp));
  range = _mm_add_epi8(range, range2);
  // We can now load minimum and maximum values from our tables based on the
  // calculated indices.
  __m128i const minv = _mm_shuffle_epi8(range_min_tbl, range);
  __m128i const maxv = _mm_shuffle_epi8(range_max_tbl, range);
  // Calculate the error (if any).
  tmp = _mm_or_si128(
      _mm_cmplt_epi8(input, minv),
      _mm_cmpgt_epi8(input, maxv));
  // Ensure our state is carried forward.
  *prev_input_ptr = input;
  *prev_first_len_ptr = first_len;
  // Accumulate error.
  return _mm_or_si128(errors, tmp);
}

__attribute__((target("ssse3")))
static inline int is_valid_utf8_ssse3 (uint8_t const * const src,
                                       size_t const len) {
  // We stride 64 bytes at a time.
  size_t const big_strides = len / 64;
  size_t const remaining = len % 64;
  uint8_t const * ptr = (uint8_t const *)src;
  // Tracking state.
  __m128i prev_input = _mm_setzero_si128();
  __m128i prev_first_len = _mm_setzero_si128();
  __m128i errors = _mm_setzero_si128();
  // Pre-load tables.
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
  for (size_t i = 0; i < big_strides; i++) {
    // Load 64 bytes.
    __m128i const * big_ptr = (__m128i const *)ptr;
    __m128i const inputs[4] = {
      _mm_loadu_si128(big_ptr),
      _mm_loadu_si128(big_ptr + 1),
      _mm_loadu_si128(big_ptr + 2),
      _mm_loadu_si128(big_ptr + 3)
    };
    // Check if we have ASCII. 
    if (__builtin_expect(!!(is_ascii_sse2(inputs)), 1)) {
      // Set prev_input and prev_first_len based on last block. Errors can stay
      // as-are.
      prev_input = inputs[3];
      __m128i const high_nibbles =
        _mm_and_si128(_mm_srli_epi16(inputs[3], 4), _mm_set1_epi8(0x0F));
      prev_first_len = _mm_shuffle_epi8(first_len_tbl, high_nibbles);
    }
    else {
      // Check four blocks, propagating everything as-needed.
      #pragma GCC unroll 4
      for (size_t j = 0; j < 4; j++) {
        errors = check_block_sse3(&prev_input,
                                  &prev_first_len,
                                  errors,
                                  first_len_tbl,
                                  first_range_tbl,
                                  range_min_tbl,
                                  range_max_tbl,
                                  df_ee_tbl,
                                  ef_fe_tbl,
                                  inputs[j]);
      }
    }
    // Advance.
    ptr += 64;
  }
  // Write out the error, check if it's OK.
  uint64_t results[2];
  _mm_storeu_si128((__m128i*)results, errors);
  if (results[0] != 0 || results[1] != 0) {
    return false;
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
  size_t const small_len = remaining + lookahead;
  return is_valid_utf8_fallback(small_ptr, small_len);
}

int is_valid_utf8 (uint8_t const * const src,
                   size_t const len) {
  __builtin_cpu_init();
  if (__builtin_cpu_supports("ssse3")) {
    return is_valid_utf8_ssse3(src, len);
  }
  return is_valid_utf8_fallback(src, len);
}
