#ifndef __ANEMONE_BASE_H
#define __ANEMONE_BASE_H

#define INLINE inline __attribute__((always_inline))

#define LIKELY(x) __builtin_expect(x, 1)
#define UNLIKELY(x) __builtin_expect(x, 0)

#endif//__ANEMONE_BASE_H
