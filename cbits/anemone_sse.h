#ifndef __ANEMONE_SSE_H
#define __ANEMONE_SSE_H

#ifndef __SSE4_2__
#error "You must compile with SSE enabled!"
#endif

#include "anemone_base.h"

#include <x86intrin.h>
#include <smmintrin.h>

__m128i INLINE anemone_sse_load128(const void* mem)
{
    return _mm_loadu_si128((__m128i*)mem);
}

/*
  load some bytes, but be careful not to read past "end"
*/
__m128i INLINE anemone_sse_load_bytes128(const char* start, const char* end)
{
  if (LIKELY(end - start >= 16)) {
    return anemone_sse_load128(start);
  } else {

    /* it doesn't matter what */
    char b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14;
    switch (end - start) {
      case 15:
        b14 = start[14];
      case 14:
        b13 = start[13];
      case 13:
        b12 = start[12];
      case 12:
        b11 = start[11];
      case 11:
        b10 = start[10];
      case 10:
        b9 = start[9];
      case 9:
        b8 = start[8];
      case 8:
        b7 = start[7];
      case 7:
        b6 = start[6];
      case 6:
        b5 = start[5];
      case 5:
        b4 = start[4];
      case 4:
        b3 = start[3];
      case 3:
        b2 = start[2];
      case 2:
        b1 = start[1];
      case 1:
        b0 = start[0];
    }

    return _mm_setr_epi8
        (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, 0);
  }
}


uint64_t INLINE anemone_sse_sum1_epi32(const __m128i a)
{
    const __m128i sum1 = _mm_hadd_epi32(a, a);
    const __m128i sum2 = _mm_hadd_epi32(sum1, sum1);

    return _mm_extract_epi32(sum2, 0);
}

/* return a[0] + a[1] + a[2] + a[3] + b[0] + b[1] + b[2] + b[3] */
uint64_t INLINE anemone_sse_sum2_epi32(const __m128i a, const __m128i b)
{
    const __m128i sum1 = _mm_hadd_epi32(a, b);
    const __m128i sum2 = _mm_hadd_epi32(sum1, sum1);
    const __m128i sum3 = _mm_hadd_epi32(sum2, sum2);

    return _mm_extract_epi32(sum3, 0);
}

unsigned int INLINE anemone_sse_first_nondigit(const __m128i m)
{
    const __m128i digit_range = _mm_setr_epi8('0', '9', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    const int index = _mm_cmpestri(digit_range, 2, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_RANGES | _SIDD_NEGATIVE_POLARITY);
    return index;
}


#endif//__ANEMONE_SSE_H


