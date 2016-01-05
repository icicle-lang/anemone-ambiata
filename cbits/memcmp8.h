#include "base.h"


static int64_t INLINE memcmp8 (const char *as, const char* bs, uint64_t len)
{
    uint64_t rem = len;
    while (rem > 8) {
        int64_t a = *(uint64_t*)as;
        int64_t b = *(uint64_t*)bs;
        if (a != b) {
            return __builtin_bswap64(a) - __builtin_bswap64(b);
        }
        rem -= 8;
        as += 8;
        bs += 8;
    }

    uint64_t mask;
    switch (rem) {
    case 0:
        mask = 0x0000000000000000;
        break;
    case 1:
        mask = 0x00000000000000FF;
        break;
    case 2:
        mask = 0x000000000000FFFF;
        break;
    case 3:
        mask = 0x0000000000FFFFFF;
        break;
    case 4:
        mask = 0x00000000FFFFFFFF;
        break;
    case 5:
        mask = 0x000000FFFFFFFFFF;
        break;
    case 6:
        mask = 0x0000FFFFFFFFFFFF;
        break;
    case 7:
        mask = 0x00FFFFFFFFFFFFFF;
        break;
    case 8:
        mask = 0xFFFFFFFFFFFFFFFF;
        break;
    }

    int64_t a = __builtin_bswap64(*(uint64_t*)as & mask);
    int64_t b = __builtin_bswap64(*(uint64_t*)bs & mask);
    return a - b;
}

/* this version is much faster, but the value it returns only tells you whether the two strings are equal or not. */
static int64_t INLINE memcmp8_eq_only (const char *as, const char* bs, uint64_t len)
{
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

    uint64_t mask;
    switch (rem) {
    case 0:
        mask = 0x0000000000000000;
        break;
    case 1:
        mask = 0x00000000000000FF;
        break;
    case 2:
        mask = 0x000000000000FFFF;
        break;
    case 3:
        mask = 0x0000000000FFFFFF;
        break;
    case 4:
        mask = 0x00000000FFFFFFFF;
        break;
    case 5:
        mask = 0x000000FFFFFFFFFF;
        break;
    case 6:
        mask = 0x0000FFFFFFFFFFFF;
        break;
    case 7:
        mask = 0x00FFFFFFFFFFFFFF;
        break;
    case 8:
        mask = 0xFFFFFFFFFFFFFFFF;
        break;
    }

    int64_t a = *(uint64_t*)as & mask;
    int64_t b = *(uint64_t*)bs & mask;
    return a - b;
}

