#ifndef __ANEMONE_SSE_H
#define __ANEMONE_SSE_H

#ifndef __SSE4_2__
#error "You must compile with SSE enabled!"
#endif

#include "anemone_base.h"

#include <x86intrin.h>

ANEMONE_INLINE
__m128i anemone_sse_load128 (const void *mem)
{
    return _mm_loadu_si128((__m128i*)mem);
}

/*
  load some bytes, but be careful not to read past "end"
*/
ANEMONE_INLINE
__m128i anemone_sse_load_bytes128 (const char *start, const char *end)
{
  if (ANEMONE_LIKELY(end - start >= 16)) {
    return anemone_sse_load128(start);
  } else {
    /* there is not enough to load all 16 bytes, as it would read past the end */
    __m128i m = _mm_setzero_si128();
    /* end - start < 16 */
    switch (end - start) {
      /* load in single-byte at a time, dropping through */
      case 15:
        m = _mm_insert_epi8(m, start[14], 14);
      case 14:
        m = _mm_insert_epi8(m, start[13], 13);
      case 13:
        m = _mm_insert_epi8(m, start[12], 12);
      case 12:
        m = _mm_insert_epi8(m, start[11], 11);
      case 11:
        m = _mm_insert_epi8(m, start[10], 10);
      case 10:
        m = _mm_insert_epi8(m, start[9], 9);
      case 9:
        m = _mm_insert_epi8(m, start[8], 8);

      /* when there are eight left, we can load a whole 64-bit and finish */
      case 8:
        m = _mm_insert_epi64(m, ((uint64_t*)start)[0], 0);
        break;

      case 7:
        m = _mm_insert_epi8(m, start[6], 6);
      case 6:
        m = _mm_insert_epi8(m, start[5], 5);
      case 5:
        m = _mm_insert_epi8(m, start[4], 4);

      /* load 32-bits and finish */
      case 4:
        m = _mm_insert_epi32(m, ((uint32_t*)start)[0], 0);
        break;

      case 3:
        m = _mm_insert_epi8(m, start[2], 2);
      case 2:
        m = _mm_insert_epi8(m, start[1], 1);
      case 1:
        m = _mm_insert_epi8(m, start[0], 0);

      default:
        break;
    }

    return m;
  }
}

ANEMONE_INLINE
uint64_t anemone_sse_sum1_epi32 (const __m128i a)
{
    const __m128i sum1 = _mm_hadd_epi32(a, a);
    const __m128i sum2 = _mm_hadd_epi32(sum1, sum1);

    return _mm_extract_epi32(sum2, 0);
}

/* return a[0] + a[1] + a[2] + a[3] + b[0] + b[1] + b[2] + b[3] */
ANEMONE_INLINE
uint64_t anemone_sse_sum2_epi32 (const __m128i a, const __m128i b)
{
    const __m128i sum1 = _mm_hadd_epi32(a, b);
    const __m128i sum2 = _mm_hadd_epi32(sum1, sum1);
    const __m128i sum3 = _mm_hadd_epi32(sum2, sum2);

    return _mm_extract_epi32(sum3, 0);
}

ANEMONE_INLINE
unsigned int anemone_sse_first_nondigit (const __m128i m)
{
    const __m128i digit_range = _mm_setr_epi8('0', '9', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    const int index = _mm_cmpestri(digit_range, 2, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_RANGES | _SIDD_NEGATIVE_POLARITY);
    return index;
}

/* 
compute indices at the same time: index of the first non-digit (anything other than '0'-'9') and index of the first non-zero.
idea is if first non-zero is 0 (ideal case), shouldn't need to cut off trailing zeros.
*/
ANEMONE_INLINE
void anemone_sse_first_nondigit_first_nonzero (const __m128i m, unsigned int* out_first_nondigit, unsigned int* out_first_nonzero)
{
    const __m128i digit_range = _mm_setr_epi8('0', '9', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    unsigned int nondigit = _mm_cmpestri(digit_range, 2, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_RANGES | _SIDD_NEGATIVE_POLARITY);
    unsigned int nonzero  = _mm_cmpestri(digit_range, 1, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_EQUAL_ANY | _SIDD_NEGATIVE_POLARITY);
    *out_first_nondigit = nondigit;
    *out_first_nonzero  = nonzero;
}

#endif//__ANEMONE_SSE_H
