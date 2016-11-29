#include "anemone_memcmp_zoo.h"
#include <string.h>
#include <stdio.h>

const char* rubbish
 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

#define NUM_BENCHES 10000000
#define MKBENCH(memcmp_f)                                           \
int64_t memcmp_f##_simple_bench (uint64_t len)                      \
{                                                                   \
    const uint64_t mod_by = strlen(rubbish) - len;                  \
    uint64_t as = 0;                                                \
    uint64_t bs = 1;                                                \
    int64_t ret = 0;                                                \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {                   \
        ret += memcmp_f(rubbish+as, rubbish+bs, len);               \
        as = (as + 1) % mod_by;                                     \
        bs = (bs + 2) % mod_by;                                     \
    }                                                               \
    return ret;                                                     \
}                                                                   \
                                                                    \
int64_t memcmp_f##_regs_bench (uint64_t len)                        \
{                                                                   \
    const uint64_t mod_by = strlen(rubbish) - len;                  \
    uint64_t as = 0;                                                \
    uint64_t bs = 1;                                                \
    uint64_t c = 0;                                                 \
    uint64_t d = 0;                                                 \
    uint64_t e = 0;                                                 \
    uint64_t f = 0;                                                 \
    uint64_t g = 0;                                                 \
    uint64_t h = 0;                                                 \
    uint64_t j = 0;                                                 \
    uint64_t k = 0;                                                 \
    uint64_t l = 0;                                                 \
    uint64_t m = 0;                                                 \
    uint64_t n = 0;                                                 \
    int64_t ret = 0;                                                \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {                   \
        ret += memcmp_f(rubbish+as, rubbish+bs, len);               \
        as = (as + 1) % mod_by;                                     \
        bs = (bs + 2) % mod_by;                                     \
                                                                    \
        c  = (c  + 3) % mod_by;                                     \
        d  = (d  + 4) % mod_by;                                     \
        e  = (e  + 5) % mod_by;                                     \
        f  = (f  + 6) % mod_by;                                     \
        g  = (g  + 7) % mod_by;                                     \
        h  = (h  + 8) % mod_by;                                     \
        j  = (j  + 10) % mod_by;                                    \
        k  = (k  + 11) % mod_by;                                    \
        l  = (l  + 12) % mod_by;                                    \
        m  = (m  + 13) % mod_by;                                    \
        n  = (n  + 14) % mod_by;                                    \
    }                                                               \
    return ret                                                      \
      + c + d + e + f + g + h + j + k + l + m + n;                  \
}


MKBENCH(anemone_memcmp8)
MKBENCH(anemone_memcmp64)
MKBENCH(anemone_memcmp128_unsafe)
MKBENCH(anemone_memcmp_partial_load64)
MKBENCH(anemone_memcmp)

static
int64_t anemone_std_memcmp(const char *as, const char *bs, uint64_t len)
{
    return memcmp(as, bs, len);
}

MKBENCH(anemone_std_memcmp)


