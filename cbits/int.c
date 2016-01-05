#if 0
#define __SSE4_1__
#define __SSE4_2__
#define __AVX__
#endif

#include <x86intrin.h>
#include <stdint.h>
#include <stdio.h>


static const uint32_t powers_of_ten_multipliers[]
 = { 10000000, 1000000, 100000, 10000
   , 1000    , 100    , 10    , 1
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   };

static const uint32_t powers_of_ten[]
 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};


static __m128i strip_ws(const char* in, const char** out)
{
    const __m128i white_range = _mm_setr_epi8(' ', '\t', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    const __m128i m = _mm_loadu_si128((__m128i*)in);

    const int index = _mm_cmpestri(white_range, 2, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_EQUAL_ANY | _SIDD_NEGATIVE_POLARITY);

    if (__builtin_expect(index == 0, 1)) {
        return m;
    } else if (index < 16) {
        const char* in_new = in + index;
        *out = in_new;
        return _mm_loadu_si128((__m128i*)in_new);
    } else {
        return strip_ws(in + 16, out);
    }
}

static unsigned int first_nondigit(const __m128i m)
{
    const __m128i digit_range = _mm_setr_epi8('0', '9', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    const int index = _mm_cmpestri(digit_range, 2, m, 16, _SIDD_UBYTE_OPS | _SIDD_CMP_RANGES | _SIDD_NEGATIVE_POLARITY);
    return index;
}


static unsigned int read64(const __m128i m, unsigned int index)
{
    const __m128i lows = _mm_cvtepu8_epi32(m);
    const __m128i m_shifted = _mm_srli_si128(m, 4);
    const __m128i highs = _mm_cvtepu8_epi32(m_shifted);


    const uint32_t zero_words[] = { 48, 48, 48, 48 };
    const __m128i zeros = _mm_loadu_si128((__m128i*)zero_words);

    const __m128i low_digits = lows - zeros;
    const __m128i hi_digits = highs - zeros;

    const int pow_ten_offset = (index < 8) ? 8 - index : 0;

    const __m128i lo_muls = _mm_loadu_si128((__m128i*)(powers_of_ten_multipliers + pow_ten_offset));
    const __m128i hi_muls = _mm_loadu_si128((__m128i*)(powers_of_ten_multipliers + pow_ten_offset + 4));

    const __m128i lo_mulled = _mm_mullo_epi32(lo_muls, low_digits);
    const __m128i hi_mulled = _mm_mullo_epi32(hi_muls, hi_digits);

    const __m128i sum1 = _mm_hadd_epi32(lo_mulled, hi_mulled);
    const __m128i sum2 = _mm_hadd_epi32(sum1, sum1);
    const __m128i sum3 = _mm_hadd_epi32(sum2, sum2);

    return _mm_extract_epi32(sum3, 0);
}

void read_int(const char* in, const char** out_end, uint64_t* out_val)
{
    __m128i m = strip_ws(in, &in);

    uint64_t sign = 1;
    if (_mm_extract_epi8(m, 0) == '-') {
        in++;
        m = _mm_loadu_si128((__m128i*)in);
        sign = -1;
    } else if (_mm_extract_epi8(m, 0) == '+') {
        in++;
        m = _mm_loadu_si128((__m128i*)in);
        sign = 1;
    }

    unsigned int index = first_nondigit(m);

    uint64_t int_out = read64(m, index);

    if (__builtin_expect(index > 8, 0)) {
        const __m128i m_shift = _mm_srli_si128(m, 8);
        uint64_t i2 = read64(m_shift, index - 8);
        uint64_t mul = powers_of_ten[index - 8];
        int_out = int_out * mul + i2;

        if (__builtin_expect(index == 16, 0)) {
            in += 16;
            const __m128i m_left = _mm_loadu_si128((__m128i*)in);
            index = first_nondigit(m_left);
            if (__builtin_expect(index > 3, 0)) {
                // fprintf(stderr, "ERROR NUMBER TOO BIG\n");
            }

            uint64_t i3 = read64(m_left, index);
            int_out = int_out * powers_of_ten[index] + i3;
        }

    }

    *out_val = int_out * sign;
    *out_end = in + index;
}

