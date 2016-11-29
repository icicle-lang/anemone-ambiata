#ifndef __ANEMONE_MEMCMP_H
#define __ANEMONE_MEMCMP_H

/* Anemone memory comparison functions.
 * If you are only comparing a small number of bytes (say, a few hundred)
 * rather than kilobytes or gigabytes, this may be faster than libc memcmp.
 * 
 * This file contains the "blessed" memory comparison function.
 * Other implementations for benchmarking against are in anemone_memcmp_zoo.
 */

#include "anemone_base.h"
#include "anemone_twiddle.h"

/* "Give me the best version" */
ANEMONE_INLINE
int anemone_memcmp (const void *as, const void *bs, size_t len);


ANEMONE_INLINE
int anemone_memcmp64 (const void *as, const void *bs, size_t len)
{
    uint64_t rem = len;
    while (rem >= 8) {
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

    uint8_t *a8 = (uint8_t*)as;
    uint8_t *b8 = (uint8_t*)bs;

    int diff;

    switch (rem) {
    case 7:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 6:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 5:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 4:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 3:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 2:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    case 1:
        diff = *a8 - *b8;
        if (diff) return diff;
        a8++; b8++;
    }
    return 0;
}

ANEMONE_INLINE
int anemone_memcmp (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp64(as, bs, len);
}


#endif//__ANEMONE_MEMCMP_H
