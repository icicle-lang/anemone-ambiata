#include "anemone_base.h"
#include "anemone_sse.h"
#include "anemone_twiddle.h"
/* Anemone memory comparison functions.
 * These functions can be somewhat faster than standard memcmp because we cheat by reading past the end of the buffer.
 * Standard memcmp must be very careful about not reading past the end, in case it produces a segfault.
 * These functions assume that the buffers are padded so that reading 8 or 16 bytes past them will not segfault.
 * The value of the padding is ignored and does not affect the comparison but it is still read.
 */

#ifndef __ANEMONE_MEMCMP_H
#define __ANEMONE_MEMCMP_H

/* Memory comparison functions:
 * Compare two buffers of equal length.
 * Return 0 if equal, <0 if as < bs, >0 if as > bs
 */

/* Compare eight bits at a time (one byte)
 * This ought to be the slowest */
int64_t anemone_memcmp8 (const char *as, const char* bs, uint64_t len);
/* Compare 64 bits at a time (eight bytes, one word)
 * This is often the fastest */
int64_t anemone_memcmp64 (const char *as, const char* bs, uint64_t len);
/* Compare 128 bits at a time (sixteen bytes)
 * Uses SSE/SIMD registers */
int64_t anemone_memcmp128(const char *as, const char* bs, uint64_t len);

/* Use the "best" memory comparison */
int64_t anemone_memcmp (const char *as, const char* bs, uint64_t len);


/* Memory equality functions:
 * Compare if two buffers of equal length are equal.
 * Return 0 if equal, nonzero if not equal.
 * This can end up being somewhat faster than comparing for order in the larger bit cases.
 */

/* Compare eight bits at a time (one byte) */
int64_t anemone_memeq8 (const char *as, const char* bs, uint64_t len);
/* Compare 64 bits (eight bytes, one word) */
int64_t anemone_memeq64 (const char *as, const char* bs, uint64_t len);
/* Compare 128 bits (sixteen bytes) */
int64_t anemone_memeq128(const char *as, const char* bs, uint64_t len);

/* Best memory equality */
int64_t anemone_memeq (const char *as, const char* bs, uint64_t len);



/* Memory comparison functions */

INLINE
int64_t
anemone_memcmp8 (const char *as, const char* bs, uint64_t len)
{
    for (uint64_t i = 0; i != len; ++i) {
        char a = as[i];
        char b = bs[i];
        if (a != b) {
            /* We are doing an unsigned comparison, so:
             * make sure we cast to an unsigned char
             * then move into a signed 64-bit to hold negative result */
            int64_t a_i = (uint8_t)a;
            int64_t b_i = (uint8_t)b;
            return a_i - b_i;
        }
    }

    return 0;
}

INLINE
int64_t
anemone_memcmp64 (const char *as, const char* bs, uint64_t len)
{
    uint64_t rem = len;
    while (rem > 8) {
        uint64_t a = *(uint64_t*)as;
        uint64_t b = *(uint64_t*)bs;

        if (a != b) {
            /* if they are different, we need to know which is bigger */
            uint64_t a_s = anemone_bswap64(a);
            uint64_t b_s = anemone_bswap64(b);
            if (a_s > b_s) {
              return 1;
            } else {
              return -1;
            }
        }

        rem -= 8;
        as += 8;
        bs += 8;
    }

    uint64_t mask = anemone_remainder_mask64(rem);

    uint64_t a = anemone_bswap64(*(uint64_t*)as & mask);
    uint64_t b = anemone_bswap64(*(uint64_t*)bs & mask);
    if (a == b)
       return 0;
    else if (a > b)
       return 1;
    else return -1;

    /* We lose the "magnitude" of the difference by doing these comparisons.
     * We cannot directly return (a - b) because we end up with overflow issues.
     * Perhaps something like this would be possible:
     * By separating into 32-bits and comparing, we avoid overflow

        int64_t diff1 = (a >> 4) - (b >> 4);
        int64_t diff2 = (a & 0x00000000FFFFFFFF) - (a & 0x00000000FFFFFFFF);
        if (diff1) {
          return diff1;
        }
        return diff2;
    */
}

INLINE
int64_t
anemone_memcmp128 (const char* buf1, const char* buf2, uint64_t len)
{
    /* Loop through the buffers in chunks of 16 */
    while (len > 0) {
        /* Take 16, or if this is the last chunk, the remnants */
        size_t current_chunk = (len < 16) ? len : 16;

        /* Load the values into registers */
        __m128i m1 = _mm_loadu_si128((__m128i*)buf1);
        __m128i m2 = _mm_loadu_si128((__m128i*)buf2);

        /* Find the index of the first byte where the two chunks differ */
        unsigned int index = _mm_cmpestri(m1, current_chunk, m2, current_chunk
               , _SIDD_UBYTE_OPS            /* Compare unsigned bytes */
               | _SIDD_CMP_EQUAL_EACH       /* Pair-wise equality */
               | _SIDD_NEGATIVE_POLARITY);  /* Not equal instead of equal */

        /* If the index is 16 or current_chunk, they are equal */
        if (index < current_chunk) {
            /* If the index is less than current chunk, buf1[index] and buf2[index] are different */
            /* Get characters as ints so we can do signed subtract */
            int char1 = (uint8_t)buf1[index];
            int char2 = (uint8_t)buf2[index];
            /* Return the difference of the two */
            return char1 - char2;
        }

        /* Move the buffers ahead by 16 */
        buf1 += 16;
        buf2 += 16;
        /* Subtract the length. current_chunk <= len, so if last chunk, len = 0*/
        len -= current_chunk;
    }

    /* Got to the end with no differences */
    return 0;
}

/* Default to the 64 bit version */
INLINE
int64_t
anemone_memcmp (const char *as, const char* bs, uint64_t len)
{
    return anemone_memcmp64(as, bs, len);
}


/* Memory equality functions */

INLINE
int64_t
anemone_memeq8 (const char *as, const char* bs, uint64_t len)
{
    /* I don't think we can improve upon memcmp8 for this case */
    return anemone_memcmp8(as, bs, len);
}

INLINE
int64_t
anemone_memeq64 (const char *as, const char* bs, uint64_t len)
{
    /* Very similar to memcmp64, but don't endian swap */
    uint64_t rem = len;
    while (rem > 8) {
        int64_t a = *(uint64_t*)as;
        int64_t b = *(uint64_t*)bs;
        if (a != b) {
            return a - b;
        }
        rem -= 8;
        as += 8;
        bs += 8;
    }

    uint64_t mask = anemone_remainder_mask64(rem);

    int64_t a = *(uint64_t*)as & mask;
    int64_t b = *(uint64_t*)bs & mask;
    return a - b;
}

INLINE
int64_t
anemone_memeq128 (const char* buf1, const char* buf2, uint64_t len)
{
    /* Very similar to memcmp128 but if a difference is found, return 1 */
    while (len > 0) {
        /* Take 16, or if this is the last chunk, the remnants */
        size_t current_chunk = (len < 16) ? len : 16;

        /* Load the values into registers */
        __m128i m1 = _mm_loadu_si128((__m128i*)buf1);
        __m128i m2 = _mm_loadu_si128((__m128i*)buf2);

        /* Find the index of the first byte where the two chunks differ */
        unsigned int index = _mm_cmpestri(m1, current_chunk, m2, current_chunk
               , _SIDD_UBYTE_OPS            /* Compare unsigned bytes */
               | _SIDD_CMP_EQUAL_EACH       /* Pair-wise equality */
               | _SIDD_NEGATIVE_POLARITY);  /* Not equal instead of equal */

        /* If the index is 16 or current_chunk, they are equal */
        if (index < current_chunk) {
            /* If the index is less than current chunk, buf1[index] and buf2[index] are different */
            /* We don't care what the difference is, just return 1 */
            return 1;
        }

        /* Move the buffers ahead by 16 */
        buf1 += 16;
        buf2 += 16;
        /* Subtract the length. current_chunk <= len, so if last chunk, len = 0*/
        len -= current_chunk;
    }

    /* Got to the end with no differences */
    return 0;
}

/* Default to the 64 bit version */
INLINE
int64_t
anemone_memeq (const char* buf1, const char* buf2, uint64_t len)
{
    return anemone_memeq64(buf1, buf2, len);
}

#endif//__ANEMONE_MEMCMP_H
