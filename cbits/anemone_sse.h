#ifndef __ANEMONE_SSE_H
#define __ANEMONE_SSE_H

#ifndef __SSE4_2__
#error "You must compile with SSE enabled!"
#endif

#include <x86intrin.h>
#include <smmintrin.h>

__m128i INLINE anemone_sse_load128(const void* mem)
{
    return _mm_loadu_si128((__m128i*)mem);
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


