#ifndef __ANEMONE_VINT_H
#define __ANEMONE_VINT_H

#include "anemone_base.h"

#define ANEMONE_MAX_VINT_SIZE 9

//
// Writes a single Hadoop-style zero-compressed integer.
//
// x:
//   an integer to pack
//
// pout:
//   pointer to buffer large enough to hold a packed integer (ANEMONE_MAX_VINT_SIZE)
//
// *returns*
//   pointer to the part of the 'pout' buffer which is unused
//   (number of bytes used = return value - pout)
//
uint8_t* anemone_write_vint (int64_t x, uint8_t *pout);

//
// Writes an array of Hadoop-style zero-compressed integers.
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
//   pointer to the part of the 'pout' buffer which is unused
//   (number of bytes used = return value - pout)
//
uint8_t* anemone_write_vint_array (int64_t n, const int64_t *p, uint8_t *pout);

//
// Reads a single Hadoop-style zero-compressed integer.
//
// pp:
//   pointer to start of buffer
//
// pe:
//   pointer to end of buffer
//
// pout:
//   pointer to an integer for output
//
// *returns*
//   zero on success, non-zero if the input was corrupt.
//
error_t anemone_read_vint (const uint8_t **pp, const uint8_t *pe, int64_t *pout);

//
// Reads an array of Hadoop-style zero-compressed integers.
//
// pp:
//   pointer to start of buffer
//
// pe:
//   pointer to end of buffer
//
// n:
//   number of integers to read
//
// pout:
//   pointer to buffer large enough to hold unpacked integers.
//
// *returns*
//   zero on success, non-zero if the input was corrupt.
//
error_t anemone_read_vint_array (const uint8_t **pp, const uint8_t *pe, int64_t n, int64_t *pout);

#endif//__ANEMONE_VINT_H
