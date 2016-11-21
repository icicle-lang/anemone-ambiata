#ifndef __ANEMONE_ATOI_H
#define __ANEMONE_ATOI_H

#include "anemone_base.h"

ANEMONE_INLINE
bool_t anemone_is_digit (char c)
{
    return c >= '0' && c <= '9';
}

error_t anemone_string_to_i64 (char **pp, char *pe, int64_t *output_ptr);

#endif//__ANEMONE_ATOI_H
