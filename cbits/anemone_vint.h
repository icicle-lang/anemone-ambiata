#ifndef __ANEMONE_VINT_H
#define __ANEMONE_VINT_H

#include "anemone_base.h"

#define ANEMONE_MAX_VINT_SIZE 9

//
// Packs an array of Hadoop-style zero-compressed integers.
//
// n:
//   number of integers to pack
//
// p:
//   pointer to start of integers
//
// pout:
//   pointer to buffer large enough to hold packed integers (n * ANEMONE_MAX_VINT_SIZE)
//
// *returns*
//   number of bytes used to pack the integers
//
size_t anemone_pack_vint (int64_t n, const int64_t *p, uint8_t *pout);

//
// Unpacks an array of Hadoop-style zero-compressed integers.
//
// n:
//   number of integers to unpack
//
// p:
//   pointer to start of packed integers
//
// pe:
//   pointer to end of packed integers
//
// pout:
//   pointer to buffer large enough to hold unpacked integers.
//
// *returns*
//   zero on success, non-zero if the input was corrupt.
//
error_t anemone_unpack_vint (int64_t n, const uint8_t *p, const uint8_t *pe, int64_t *pout);

#endif//__ANEMONE_VINT_H
