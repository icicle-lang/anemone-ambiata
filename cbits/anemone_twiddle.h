#ifndef __ANEMONE_TWIDDLE_H
#define __ANEMONE_TWIDDLE_H

INLINE
uint64_t anemone_bswap64(int64_t x)
{
    return __builtin_bswap64(x);
}

INLINE
uint64_t anemone_remainder_mask64(uint64_t remainder)
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

