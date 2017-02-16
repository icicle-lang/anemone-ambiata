#include "anemone_atoi.h"
#include "anemone_sse.h"


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



/*
 compute digit of first eight chars in m.

 m = [ c0, c1, c2, c3, c4, c5, c6, c7, ... c15 ]@8

 index is index of first non-digit; that is, where the number ends

 something like this:

     return c0 * pow10[index-1]
          + c1 * pow10[index-2]
          + ...
          + c7 * pow10[index-8];

 where pow10[0] = 1, pow10[<0] = 0, pow10[1] = 10, ...
 */
ANEMONE_STATIC
ANEMONE_INLINE
uint64_t anemone_string_to_i64_v128_first_eight (const __m128i m, unsigned int index)
{
    // m      = [ c0, c1, c2, c3, c4, c5, c6, c7, ... c15 ]@8

    // Extend bytes c0-c7 into 32-bit integers, storing in two vectors:
    // lows   = [ c0, c1, c2, c3 ]@32
    // highs  = [ c4, c5, c6, c7 ]@32
    const __m128i lows       = _mm_cvtepu8_epi32(m);
    const __m128i m_shifted  = _mm_srli_si128(m, 4);
    const __m128i highs      = _mm_cvtepu8_epi32(m_shifted);

    // Compute power of ten multipliers for each digit
    const int pow_ten_offset = (index < 8) ? 8 - index : 0;

    const __m128i lo_muls    = anemone_sse_load128(powers_of_ten_multipliers + pow_ten_offset);
    const __m128i hi_muls    = anemone_sse_load128(powers_of_ten_multipliers + pow_ten_offset + 4);

    // Multiply each 32-bit extended integer against its power of ten
    const __m128i lo_mulled  = _mm_mullo_epi32(lo_muls, lows);
    const __m128i hi_mulled  = _mm_mullo_epi32(hi_muls, highs);

    // Sum all together. The result fits in a 32-bit.
    uint64_t sum             = anemone_sse_sum2_epi32(hi_mulled, lo_mulled);
    return sum;
}

/*
  parse string to unsigned 64-bit integer.
  does not strip whitespace.
  does not allow '-' or '+' prefixes.
  this does not allow the full range of uint64, instead only up to 10^19, which is a little above 2^63
  the return value is 0 if successful parse, <0 on error, and >0 when the number won't fit in 10^19.
 */
int64_t anemone_string_to_ui64_v128_floating (char **pp, char *pe, uint64_t *out_val)
{
    char* in = *pp;
    uint64_t buffer_size = pe - in;
    if (buffer_size == 0) {
        return -1;
    }

    // Load string into vector: vector stores 16 bytes.
    // This is a *safe* read and anything past the end of the buffer will be null
    // Which means it will be automatically ignored by the nondigit check
    // > m = [ '1', '0', 'x', ... ]@8
    __m128i m                = anemone_sse_load_bytes128(in, pe);

    // Find first non-digit:
    // > anemone_sse_first_nondigit( [ '1', '0', 'x', ... ]) = 2
    unsigned int index       = anemone_sse_first_nondigit(m);
    in                      += index;

    // > zeros = ['0', '0', '0', '0', ... ]@8
    const __m128i zeros      = _mm_set1_epi8(48);
    // > m     = [ 1, 0, garbage, garbage, ...]@8
    m                        = _mm_subs_epi8(m, zeros);

    // Convert first 8 bytes (first half) of vector
    // > int_out = 10
    uint64_t int_out         = anemone_string_to_i64_v128_first_eight(m, index);

    // If we have a bigger number than can fit in a uint64, we keep track of how much bigger it is
    uint64_t exponent_spill  = 0;

    // If index is 0, there are no numbers here
    if (ANEMONE_UNLIKELY(index == 0)) {
        return -1;
    }
    // If index > 8, we need to convert the second half of the vector
    if (ANEMONE_UNLIKELY(index > 8)) {
        // Shift right to get second half, and convert it
        m                    = _mm_srli_si128(m, 8);
        const uint64_t i2    = anemone_string_to_i64_v128_first_eight(m, index - 8);
        // Fix up and join the two halves
        const uint64_t mul2  = powers_of_ten[index - 8];
        int_out              = int_out * mul2 + i2;


        // If index == 16, it means the vector contains only digits.
        // So we need to load the next 16 bytes to find the end of the number
        if (ANEMONE_UNLIKELY(index == 16)) {
            m                = anemone_sse_load_bytes128(in, pe);
            index            = anemone_sse_first_nondigit(m);
            in              += index;

            // We can only hold 19 digits (16+3 == 19)
            // So if it's any more we would overflow - count how many extra digits there are
            if (index > 3) {
                // Count how many extra digits and throw them away
                exponent_spill = index - 3;
                while (index == 16) {
                    __m128i throw = anemone_sse_load_bytes128(in, pe);
                    index         = anemone_sse_first_nondigit(throw);
                    in           += index;
                    exponent_spill += index;
                }
                // Finally, pretend there were only 19 digits.
                // Floating point parse can still use the first 19.
                index = 3;
            }

            // For total digits > 16 and <= 19, we just need to perform a final fixup and join
            m                        = _mm_subs_epi8(m, zeros);
            const uint64_t i3        = anemone_string_to_i64_v128_first_eight(m, index);
            const uint64_t mul3 = powers_of_ten[index];
            int_out = int_out * mul3 + i3;

        }

    }

    *out_val = int_out;
    *pp      = in;
    return exponent_spill;
}

/*
  parse unsigned 64-bit integer.
 */
ANEMONE_INLINE
ANEMONE_STATIC
error_t anemone_string_to_ui64_v128 (char **pp, char *pe, uint64_t *out_val)
{
    char* pp_ = *pp;
    int64_t ret = anemone_string_to_ui64_v128_floating (&pp_, pe, out_val);

    if (ret == 0) {
        *pp = pp_;
        return 0;
    } else {
        return 1;
    }
}

/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
error_t anemone_string_to_i64_v128 (char **pp, char *pe, int64_t *out_val)
{
    char* in = *pp;
    uint64_t buffer_size = pe - in;
    if (buffer_size == 0) {
        return 1;
    }

    int64_t sign             = 1;

    if (in[0] == '-') {
        in++;
        sign                 = -1;
    }
    uint64_t int_out;
    if (anemone_string_to_ui64_v128(&in, pe, &int_out)) {
      return 1;
    }

    // Check whether it would overflow in converting to a signed int64
    if (ANEMONE_UNLIKELY(sign == 1 && int_out > INT64_MAX)) {
        return 1;
    } else if (ANEMONE_UNLIKELY(sign == -1 && int_out > (uint64_t)INT64_MAX + 1)) {
        return 1;
    }

    *pp = in;
    *out_val = int_out * sign;
    return 0;
}
