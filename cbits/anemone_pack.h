#ifndef __ANEMONE_PACK_H
#define __ANEMONE_PACK_H

#include <stdint.h>

void anemone_pack64_64 (uint64_t blocks, const uint64_t bits, const uint64_t *in, uint8_t *out);

void anemone_unpack64_64 (uint64_t blocks, const uint64_t bits, const uint8_t *in, uint64_t *out);

#endif//__ANEMONE_PACK_H

