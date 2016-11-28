#ifndef __ANEMONE_TWIDDLE_H
#define __ANEMONE_TWIDDLE_H

/* Anemone bit twiddling functions.
 * These are some fairly boring functions that can come in handy.
 * They aren't likely to be useful generally, which is why they aren't exposed to Haskell.
 */


#include "anemone_base.h"

/* Byte and endian swapping
 */

ANEMONE_INLINE
uint16_t anemone_bswap16 (uint16_t x)
{
    return (x >> 8) | (x << 8);
}

ANEMONE_INLINE
uint32_t anemone_bswap32 (uint32_t x)
{
    return __builtin_bswap32 (x);
}

ANEMONE_INLINE
uint64_t anemone_bswap64 (uint64_t x)
{
    return __builtin_bswap64(x);
}


/* Loading and masking.
   When you have a uint8 buffer you don't want to read past the end of.
 */

/* If you only want the first <8 bytes of a buffer but still want a single load, use this mask.
 * This is only safe if you own the memory past the end of the buffer and know it won't segfault.
 */
ANEMONE_INLINE
uint64_t anemone_remainder_mask64 (uint64_t remainder)
{
    switch (remainder) {
    case 0:
        return 0x0000000000000000;
    case 1:
        return 0x00000000000000FF;
    case 2:
        return 0x000000000000FFFF;
    case 3:
        return 0x0000000000FFFFFF;
    case 4:
        return 0x00000000FFFFFFFF;
    case 5:
        return 0x000000FFFFFFFFFF;
    case 6:
        return 0x0000FFFFFFFFFFFF;
    case 7:
        return 0x00FFFFFFFFFFFFFF;
    case 8:
        return 0xFFFFFFFFFFFFFFFF;
    }
    return 0x0000000000000000;
}

/* Load the first <=8 bytes of a buffer into a 64-bit word.
 * This is safe, in contrast to loading and masking above.
 */
ANEMONE_INLINE
uint64_t anemone_partial_load64 (const void *ptr, size_t len)
{
    if (len >= 8) {
        return *(uint64_t*)ptr;
    }

    uint8_t *ptr8 = (uint8_t*)ptr;
    uint32_t *ptr32 = (uint32_t*)ptr;
    uint64_t into = 0;

    switch (len) {
    case 0:
        break;
    case 1:
        into = *ptr8;
        break;
    case 2:
        into = *ptr8 | (uint64_t)*(ptr8+1) << 8;
        break;
    case 3:
        into = *ptr8 | (uint64_t)*(ptr8+1) << 8 | (uint64_t)*(ptr8+2) << 16;
        break;
    case 4:
        into = *ptr32;
        break;
    case 5:
        into = *ptr32 | (uint64_t)*(ptr8+4) << 32;
        break;
    case 6:
        into = *ptr32 | (uint64_t)*(ptr8+4) << 32 | (uint64_t)*(ptr8+5) << 40;
        break;
    case 7:
        into = *ptr32 | (uint64_t)*(ptr8+4) << 32 | (uint64_t)*(ptr8+5) << 40 | (uint64_t)*(ptr8+6) << 48;
        break;
    }

    return into;
}

#endif//__ANEMONE_TWIDDLE_H
