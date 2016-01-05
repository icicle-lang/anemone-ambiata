#ifdef __SSE4_2__

#include <smmintrin.h>
#include <stdint.h>


/*
 Compare memory, 16 bytes at a time
 Memory > len will be ignored for the comparison,
 but if len is not a multiple of 16, bytes past len
 *will* be read.
 */
static INLINE __attribute__((always_inline)) int
memcmp16(const char* buf1, const char* buf2, size_t len)
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
            int char1 = buf1[index];
            int char2 = buf2[index];
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

#else

#define memcmp16 memcmp

#endif
