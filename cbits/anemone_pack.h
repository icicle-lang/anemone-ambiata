#ifndef __ANEMONE_PACK_H
#define __ANEMONE_PACK_H

#include "anemone_base.h"

error_t anemone_pack64_64 (uint64_t blocks, const uint64_t bits, const uint64_t *in, uint8_t *out);

error_t anemone_unpack64_64 (uint64_t blocks, const uint64_t bits, const uint8_t *in, uint64_t *out);

#endif//__ANEMONE_PACK_H

