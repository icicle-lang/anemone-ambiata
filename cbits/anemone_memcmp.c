#include "anemone_memcmp.h"

#define NUM_BENCHES 1000000
#define MKBENCH(f)                                                  \
int64_t f##_bench (const char *as, const char* bs, uint64_t len)    \
{                                                                   \
    int64_t ret = 0;                                                \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {                   \
        ret += f(as, bs, len);                                      \
    }                                                               \
    return ret;                                                     \
}

MKBENCH(anemone_memcmp8)
MKBENCH(anemone_memcmp64)
MKBENCH(anemone_memcmp128)
MKBENCH(anemone_memcmp)

MKBENCH(anemone_memeq8)
MKBENCH(anemone_memeq64)
MKBENCH(anemone_memeq128)
MKBENCH(anemone_memeq)

