#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// 0x80 in every 'lane'.
static uint64_t const high_bits_mask = 0x8080808080808080ULL;

int is_valid_utf8 (uint8_t const * const src,
                   size_t const len) {
  uint8_t const * ptr = (uint8_t const *)src;
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
      if ((ptr + 32 < end) && is_not_whitespace) {
        uint64_t const * big_ptr = (uint64_t const *)ptr;
        // Non-ASCII bytes have a set MSB. Thus, if we AND with 0x80 in every
        // 'lane', we will get 0 if everything is ASCII, and something else
        // otherwise.
        uint64_t results[4] = {
          (*big_ptr) & high_bits_mask,
          (*(big_ptr + 1)) & high_bits_mask,
          (*(big_ptr + 2)) & high_bits_mask,
          (*(big_ptr + 3)) & high_bits_mask
        };
        if (results[0] == 0) {
          ptr += 8;
          if (results[1] == 0) {
            ptr += 8;
            if (results[2] == 0) {
              ptr += 8;
              if (results[3] == 0) {
                ptr += 8;
              }
              else {
                ptr += (__builtin_ctzl(results[3]) / 8);
              }
            }
            else {
              ptr += (__builtin_ctzl(results[2]) / 8);
            }
          }
          else {
            ptr += (__builtin_ctzl(results[1]) / 8);
          }
        }
        else {
          ptr += (__builtin_ctzl(results[0]) / 8);
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