#ifndef __ANEMONE_BASE_H
#define __ANEMONE_BASE_H

#include <stdint.h>
#include <stdlib.h>

typedef int bool_t;
typedef int error_t;

#define ANEMONE_STATIC static
#define ANEMONE_INLINE inline __attribute__((always_inline))

#define ANEMONE_FALSE ((bool_t) 0)
#define ANEMONE_TRUE ((bool_t) 1)

#define ANEMONE_LIKELY(x) __builtin_expect(x, 1)
#define ANEMONE_UNLIKELY(x) __builtin_expect(x, 0)

#endif//__ANEMONE_BASE_H
