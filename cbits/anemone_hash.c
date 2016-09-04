/* The MIT License

   Copyright (C) 2012 Zilong Tan (eric.zltan@gmail.com)

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

#include "anemone_hash.h"

// Compression function for Merkle-Damgard construction.
#define MIX(v) ({ (v) ^= (v) >> 23; \
                  (v) *= 0x2127599bf4325c37ULL; \
                  (v) ^= (v) >> 47; })

static uint64_t __attribute__((always_inline)) fasthash (uint64_t seed, const uint8_t *buf, size_t len)
{
    const uint64_t m = 0x880355f21e6d1965ULL;

    const uint64_t *pos64 = (const uint64_t *) buf;
    const uint64_t *end64 = pos64 + (len / 8);

    uint64_t h = seed ^ (len * m);
    uint64_t v;

    while (pos64 != end64) {
        v = *pos64;
        pos64++;

        h ^= MIX(v);
        h *= m;
    }

    const uint8_t *pos8 = (const uint8_t *) pos64;
    v = 0;

    switch (len & 7) {
        case 7: v ^= (uint64_t) pos8[6] << 48;
        case 6: v ^= (uint64_t) pos8[5] << 40;
        case 5: v ^= (uint64_t) pos8[4] << 32;
        case 4: v ^= (uint64_t) pos8[3] << 24;
        case 3: v ^= (uint64_t) pos8[2] << 16;
        case 2: v ^= (uint64_t) pos8[1] << 8;
        case 1: v ^= (uint64_t) pos8[0];
                h ^= MIX(v);
                h *= m;
    }

    return MIX(h);
}

uint64_t anemone_fasthash64 (uint64_t seed, const uint8_t *buf, size_t len)
{
    return fasthash (seed, buf, len);
}

uint32_t anemone_fasthash32 (uint64_t seed, const uint8_t *buf, size_t len)
{
    // The following trick converts the 64-bit hashcode to Fermat residue,
    // which shall retain information from both the higher and lower parts of
    // hashcode.

    uint64_t h64 = fasthash (seed, buf, len);

    return h64 - (h64 >> 32);
}
