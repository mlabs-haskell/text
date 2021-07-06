#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// Fills each 8-bit 'lane' with the same byte.
static inline uint64_t broadcast (uint8_t const byte) {
  return byte * 0x0101010101010101ULL;
}

static inline bool is_ascii (uint8_t const * const src,
                             size_t const len) {
  uint8_t const * ptr = (uint8_t const *)src;
  // We step four 64-bit words at a time.
  size_t const big_strides = len / 32;
  size_t const small_strides = len % 32;
  uint8_t const byte_mask = 0x80;
  uint64_t const high_bit_mask = broadcast(byte_mask);
  for (size_t i = 0; i < big_strides; i++) {
    uint64_t const * big_ptr = (uint64_t const *)ptr;
    // If a byte is non-ASCII, it'll have its MSB set. By ANDing with 0x80 in
    // every 'lane', we get 0x80 in 'lanes' with have non-ASCII bytes, and 0x00
    // otherwise. If we accumulate with bitwise OR, we preserve any set MSBs.
    uint64_t result = (*big_ptr) | 
                      (*(big_ptr + 1)) | 
                      (*(big_ptr + 2)) |
                      (*(big_ptr + 3));
    // ANDing with 0x80 in every 'lane' will get us 0x00 if that 'lane' had only
    // ASCII bytes, and 0x80 if it had any non-ASCII ones.
    if ((result & high_bit_mask) != 0) {
      return false;
    }
    ptr += 32;
  }
  // Finish the rest the slow way.
  for (size_t i = 0; i < small_strides; i++) {
    if ((*ptr) >= byte_mask) {
      return false;
    }
    ptr++;
  }
  // If we got this far, we found nothing amiss.
  return true;
}

// Returns an index into src where the invalid UTF-8 sequence started, or -1 if
// no such sequence was found.
ptrdiff_t find_invalid_utf8 (uint8_t const * const src,
                             size_t const len) {
  if (is_ascii(src, len)) {
    return -1;
  }
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
