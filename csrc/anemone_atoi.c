#include "anemone_atoi.h"

error_t anemone_string_to_i64 (char **pp, char *pe, int64_t *output_ptr)
{
    char  *p           = *pp;
    uint64_t buffer_size = pe - p;

    /* handle negative */
    int sign      = 1;
    int sign_size = 0;
    if (buffer_size > 0 && p[0] == '-') {
        sign      = -1;
        sign_size = 1;
        p++;
        buffer_size--;
    }

    /* validate digits */
    uint64_t digits = 0;
    while (digits < buffer_size) {
        if (!anemone_is_digit (p[digits]))
            break;
        digits++;
    }

    if (digits == 0)
        return 1;

    uint64_t value = 0;

    /* handle up to 19 digits, assume we're 64-bit */
    switch (digits) {
        case 19:  value += (p[digits-19] - '0') * 1000000000000000000LLU;
        case 18:  value += (p[digits-18] - '0') * 100000000000000000LLU;
        case 17:  value += (p[digits-17] - '0') * 10000000000000000LLU;
        case 16:  value += (p[digits-16] - '0') * 1000000000000000LLU;
        case 15:  value += (p[digits-15] - '0') * 100000000000000LLU;
        case 14:  value += (p[digits-14] - '0') * 10000000000000LLU;
        case 13:  value += (p[digits-13] - '0') * 1000000000000LLU;
        case 12:  value += (p[digits-12] - '0') * 100000000000LLU;
        case 11:  value += (p[digits-11] - '0') * 10000000000LLU;
        case 10:  value += (p[digits-10] - '0') * 1000000000LLU;
        case  9:  value += (p[digits- 9] - '0') * 100000000LLU;
        case  8:  value += (p[digits- 8] - '0') * 10000000LLU;
        case  7:  value += (p[digits- 7] - '0') * 1000000LLU;
        case  6:  value += (p[digits- 6] - '0') * 100000LLU;
        case  5:  value += (p[digits- 5] - '0') * 10000LLU;
        case  4:  value += (p[digits- 4] - '0') * 1000LLU;
        case  3:  value += (p[digits- 3] - '0') * 100LLU;
        case  2:  value += (p[digits- 2] - '0') * 10LLU;
        case  1:  value += (p[digits- 1] - '0');
        /* ^ fall through */
            *output_ptr = ((int64_t) value) * sign;
            *pp += digits + sign_size;
            return 0;

        default:
            return 1;
    }
}
