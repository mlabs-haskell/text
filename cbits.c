#include <stdint.h>
#include <stdio.h>

uint64_t _hs_bar(const uint64_t x){
  return __builtin_popcountll(x)
}
