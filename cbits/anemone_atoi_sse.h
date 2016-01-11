#include "anemone_base.h"
#include "anemone_sse.h"
#include "anemone_twiddle.h"


const uint32_t powers_of_ten_multipliers[]
 = { 10000000, 1000000, 100000, 10000
   , 1000    , 100    , 10    , 1
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   , 0       , 0      , 0     , 0
   };

const uint32_t powers_of_ten[]
 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};



/*
 compute digit of first eight chars in m.
 m = [ c0, c1, c2, c3, c4, c5, c6, c7, ... ]

 index is index of first non-digit; that is, where the number ends

 something like this:

     return c0 * pow10[index-1]
          + c1 * pow10[index-2]
          + ...
          + c7 * pow10[index-8];

 where pow10[0] = 1, pow10[<0] = 0, pow10[1] = 10, ...
 */
static
unsigned int INLINE
anemone_string_to_i64_v128_first_eight(const __m128i m, unsigned int index)
{
    const __m128i lows       = _mm_cvtepu8_epi32(m);
    const __m128i m_shifted  = _mm_srli_si128(m, 4);
    const __m128i highs      = _mm_cvtepu8_epi32(m_shifted);

    const __m128i zeros      = _mm_setr_epi32(48, 48, 48, 48);
    const __m128i low_digits = lows - zeros;
    const __m128i hi_digits  = highs - zeros;

    const int pow_ten_offset = (index < 8) ? 8 - index : 0;

    const __m128i lo_muls    = anemone_sse_load128(powers_of_ten_multipliers + pow_ten_offset);
    const __m128i hi_muls    = anemone_sse_load128(powers_of_ten_multipliers + pow_ten_offset + 4);

    const __m128i lo_mulled  = _mm_mullo_epi32(lo_muls, low_digits);
    const __m128i hi_mulled  = _mm_mullo_epi32(hi_muls, hi_digits);

    uint64_t sum             = anemone_sse_sum2_epi32(hi_mulled, lo_mulled);
    return sum;
}


/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
/* temporary: do not inline this so it's a fair comparison with atoi */
bool __attribute__((noinline))
anemone_string_to_i64_v128(char** pp, char* pe, int64_t* out_val)
{
    char* in = *pp;

    __m128i m                = anemone_sse_load128(in);
    int64_t sign             = 1;

    if (_mm_extract_epi8(m, 0) == '-') {
        in++;
        m                    = anemone_sse_load128(in);
        sign                 = -1;
    } else if (_mm_extract_epi8(m, 0) == '+') {
        in++;
        m                    = anemone_sse_load128(in);
        sign                 = 1;
    }

    unsigned int index       = anemone_sse_first_nondigit(m);

    uint64_t int_out         = anemone_string_to_i64_v128_first_eight(m, index);

    if (UNLIKELY(index > 8)) {
        m                    = _mm_srli_si128(m, 8);
        const uint64_t i2    = anemone_string_to_i64_v128_first_eight(m, index - 8);
        const uint64_t mul2  = powers_of_ten[index - 8];
        int_out              = int_out * mul2 + i2;

        if (UNLIKELY(index == 16)) {
            in              += 16;
            m                = anemone_sse_load128(in);
            index            = anemone_sse_first_nondigit(m);
            if (UNLIKELY(index > 3)) {
                // fprintf(stderr, "ERROR NUMBER TOO BIG\n");
                return 1;
            }

            const uint64_t i3   = anemone_string_to_i64_v128_first_eight(m, index);
            const uint64_t mul3 = powers_of_ten[index];
            int_out = int_out * mul3 + i3;
        }

    }

    *out_val = int_out * sign;
    *pp      = in + index;
    return 0;
}


