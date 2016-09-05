#ifndef __ANEMONE_TWIDDLE_H
#define __ANEMONE_TWIDDLE_H

#include "anemone_base.h"

#include <stdint.h>

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

#endif//__ANEMONE_TWIDDLE_H
