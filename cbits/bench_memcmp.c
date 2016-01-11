#include "anemone_memcmp.h"
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

#define NUM_BENCHES 1000000
#define MKBENCH(f)                                                  \
int64_t f##_bench (uint64_t len)                                    \
{                                                                   \
    const uint64_t mod_by = strlen(rubbish) - len;                  \
    uint64_t as = 0;                                                \
    uint64_t bs = 1;                                                \
    int64_t ret = 0;                                                \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {                   \
        ret += f(rubbish+as, rubbish+bs, len);                                      \
        as = (as + 1) % mod_by;                     \
        bs = (bs + 2) % mod_by;                     \
    }                                                               \
    return ret;                                                     \
}

MKBENCH(anemone_memcmp8)
MKBENCH(anemone_memcmp64)
MKBENCH(anemone_memcmp128)
MKBENCH(anemone_memcmp)

int64_t anemone_std_memcmp(const char *as, const char *bs, uint64_t len)
{
    return memcmp(as, bs, len);
}
MKBENCH(anemone_std_memcmp)

MKBENCH(anemone_memeq8)
MKBENCH(anemone_memeq64)
MKBENCH(anemone_memeq128)
MKBENCH(anemone_memeq)


