#include "anemone_strtod.h"

#define NUM_BENCHES 1000000
#define MKBENCH(f) \
double f##_bench(char* str, uint64_t len)                   \
{                                                           \
    uint64_t num = 0;                                       \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {           \
        char* start = str;                                  \
        char* end = str + len;                              \
        double out;                                         \
        num += f(&start, end, &out);                        \
    }                                                       \
    return num;                                             \
}

MKBENCH(anemone_strtod)

/* do not inline this so the C compiler cannot do any fancy tricks */
int64_t __attribute__((noinline)) anemone_std_strtod(char **pp, char *pe, double *output_ptr)
{
    double out = atoi(*pp);
    *output_ptr = out;
    return out;
}
MKBENCH(anemone_std_strtod)
