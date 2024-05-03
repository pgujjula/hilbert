#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

int64_t num_sum_of_squares_le(int64_t n, int64_t sq, int64_t sq2) {
  if (n < 0) return 0;

  int64_t c = 0;
  int64_t x = 0;
  int64_t y = sq;
  int64_t nmx2 = 0;

  for ( ; x <= sq2 ; x++) {
    nmx2 = n - x*x;
    while (y*y > nmx2) {
      y--;
    }
    c += y;
  }

  return 2*(c + x) - (sq2 + 1)*(sq2 + 1);
}
