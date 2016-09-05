#ifndef __ANEMONE_HASH_H
#define __ANEMONE_HASH_H

#include <stdint.h>
#include <stdio.h>

uint32_t anemone_fasthash32 (uint64_t seed, const uint8_t *buf, size_t len);

uint64_t anemone_fasthash64 (uint64_t seed, const uint8_t *buf, size_t len);

#endif // __ANEMONE_HASH_H
