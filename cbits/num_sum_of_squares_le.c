#include <stdint.h>

int64_t num_sum_of_squares_le(int64_t n, int64_t sq) {
  if (n < 0) return 0;

  int64_t c = 0;
  int64_t y = sq;

  int64_t nmx2 = 0;
  for (int64_t x = 0; x <= sq; x++) {
    nmx2 = n - x*x;
    while (y*y > nmx2) {
      y--;
    }
    c += y + 1;
  }

  return c;
}
