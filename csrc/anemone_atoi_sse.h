#ifndef __ANEMONE_ATOI_SSE_H
#define __ANEMONE_ATOI_SSE_H

#include "anemone_base.h"

/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
error_t anemone_string_to_i64_v128 (char **pp, char *pe, int64_t *out_val);


/*
  parse string unsigned 64-bit integer, but don't fail so hard on really long numbers.
  this does not allow the full range of uint64, instead only up to 10^19, which is a little above 2^63
  the return value is 0 if successful parse, <0 on error, and >0 when the number won't fit in 10^19.
 */
int64_t anemone_string_to_ui64_v128_floating (char **pp, char *pe, uint64_t *out_val);

#endif//__ANEMONE_ATOI_SSE_H
