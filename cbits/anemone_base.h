#ifndef __ANEMONE_BASE_H
#define __ANEMONE_BASE_H

#include <stdint.h>
typedef uint64_t bool;

#define INLINE inline __attribute__((always_inline))

#define LIKELY(x) __builtin_expect(x, 1)
#define UNLIKELY(x) __builtin_expect(x, 0)

#endif//__ANEMONE_BASE_H
