#include "anemone_vint.h"
#include "anemone_twiddle.h"

#include <string.h>

//
// Serializes an integer to a binary stream with zero-compressed encoding.
//
// For -112 <= i <= 127, only one byte is used with the actual value.
//
// For other values of i, the first byte value indicates whether the
// integer is positive or negative, and the number of bytes that follow.
//
// If the first byte value v is between -113 and -116, the following integer
// is positive, with number of bytes that follow are -(v+112).
//
// If the first byte value v is between -121 and -124, the following integer
// is negative, with number of bytes that follow are -(v+120).
//
// Bytes are stored in the high-non-zero-byte-first order.
//
// ^ comment taken from 'org.apache.hadoop.io.WritableUtils.writeVLong'
// (see https://github.com/apache/hadoop/blob/trunk/hadoop-common-project/hadoop-common/src/main/java/org/apache/hadoop/io/WritableUtils.java)
//
ANEMONE_STATIC
ANEMONE_INLINE
uint8_t* vint_write (int64_t v, uint8_t *pout)
{
    if (v >= -112 && v <= 127) {
        *(int8_t *)pout = v;
        pout++;
    } else {
        int8_t base;

        if (v >= 0) {
            base = -113;
        } else {
            base = -121;
            v = ~v;
        }

        uint64_t value = v;
        int8_t n_bytes = (64 - __builtin_clzll (value)) >> 3;

        *(int8_t *)pout = base - n_bytes;

        switch (n_bytes) {
            case 0:
                *(uint8_t  *)(pout + 1) = value;
                pout += 2;
                break;

            case 1:
                *(uint8_t  *)(pout + 1) = value >> 8;
                *(uint8_t  *)(pout + 2) = value;
                pout += 3;
                break;

            case 2:
                *(uint8_t  *)(pout + 1) = value >> 16;
                *(uint8_t  *)(pout + 2) = value >> 8;
                *(uint8_t  *)(pout + 3) = value;
                pout += 4;
                break;

            case 3:
                *(uint32_t *)(pout + 1) = anemone_bswap32 (value);
                pout += 5;
                break;

            case 4:
                *(uint32_t *)(pout + 1) = anemone_bswap32 (value >> 8);
                *(uint8_t  *)(pout + 5) = value;
                pout += 6;
                break;

            case 5:
                *(uint32_t *)(pout + 1) = anemone_bswap32 (value >> 16);
                *(uint8_t  *)(pout + 5) = value >> 8;
                *(uint8_t  *)(pout + 6) = value;
                pout += 7;
                break;

            case 6:
                *(uint32_t *)(pout + 1) = anemone_bswap32 (value >> 24);
                *(uint8_t  *)(pout + 5) = value >> 16;
                *(uint8_t  *)(pout + 6) = value >> 8;
                *(uint8_t  *)(pout + 7) = value;
                pout += 8;
                break;

            default:
                *(uint64_t *)(pout + 1) = anemone_bswap64 (value);
                pout += 9;
                break;
        }
    }

    return pout;
}

ANEMONE_STATIC
ANEMONE_INLINE
bool_t vint_single (int8_t first)
{
    return (first & 0b11110000) != 0b10000000;
}

ANEMONE_STATIC
ANEMONE_INLINE
int vint_remaining (int8_t first)
{
    return (~first & 0b0000111) + 1;
}

ANEMONE_STATIC
ANEMONE_INLINE
bool_t vint_negative (int8_t first)
{
    return (~first & 0b0001000);
}

ANEMONE_STATIC
ANEMONE_INLINE
error_t vint_read (const uint8_t **pp, const uint8_t *pe, int64_t *out)
{
#define ENSURE_BYTES(n) if (p + n > pe) return 1

    const uint8_t *p = *pp;

    ENSURE_BYTES(1);

    int8_t first = *p;
    p++;

    if (vint_single (first)) {
        *pp = p;
        *out = first;
        return 0;
    }

    int remaining = vint_remaining (first);
    int64_t x = 0;

    ENSURE_BYTES(remaining);

    for (int i = 0; i < remaining; i++) {
        x = x << 8 | p[i];
    }

    *pp = p + remaining;
    *out = vint_negative (first) ? ~x : x;

    return 0;
}

uint8_t* anemone_write_vint (int64_t x, uint8_t *pout)
{
    return vint_write (x, pout);
}

uint8_t* anemone_write_vint_array (int64_t n, const int64_t *p, uint8_t *pout)
{
    for (int i = 0; i < n; i++) {
        pout = vint_write (p[i], pout);
    }

    return pout;
}

error_t anemone_read_vint (const uint8_t **pp, const uint8_t *pe, int64_t *pout)
{
    return vint_read (pp, pe, pout);
}

error_t anemone_read_vint_array (const uint8_t **pp, const uint8_t *pe, int64_t n, int64_t *pout)
{
    const uint8_t *p = *pp;
    error_t err = 0;

    for (int i = 0; i < n; i++) {
        err = vint_read (&p, pe, pout + i);
        if (err) goto done;
    }

done:
    *pp = p;
    return err;
}
