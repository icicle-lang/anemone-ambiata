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
        if (!anemone_text_is_digit (p[digits]))
            break;
        digits++;
    }

    if (digits == 0)
        return 1;

    int64_t value = 0;

    /* handle up to 19 digits, assume we're 64-bit */
    switch (digits) {
        case 19:  value += (p[digits-19] - '0') * 1000000000000000000LL;
        case 18:  value += (p[digits-18] - '0') * 100000000000000000LL;
        case 17:  value += (p[digits-17] - '0') * 10000000000000000LL;
        case 16:  value += (p[digits-16] - '0') * 1000000000000000LL;
        case 15:  value += (p[digits-15] - '0') * 100000000000000LL;
        case 14:  value += (p[digits-14] - '0') * 10000000000000LL;
        case 13:  value += (p[digits-13] - '0') * 1000000000000LL;
        case 12:  value += (p[digits-12] - '0') * 100000000000LL;
        case 11:  value += (p[digits-11] - '0') * 10000000000LL;
        case 10:  value += (p[digits-10] - '0') * 1000000000LL;
        case  9:  value += (p[digits- 9] - '0') * 100000000LL;
        case  8:  value += (p[digits- 8] - '0') * 10000000LL;
        case  7:  value += (p[digits- 7] - '0') * 1000000LL;
        case  6:  value += (p[digits- 6] - '0') * 100000LL;
        case  5:  value += (p[digits- 5] - '0') * 10000LL;
        case  4:  value += (p[digits- 4] - '0') * 1000LL;
        case  3:  value += (p[digits- 3] - '0') * 100LL;
        case  2:  value += (p[digits- 2] - '0') * 10LL;
        case  1:  value += (p[digits- 1] - '0');
        /* ^ fall through */
            value *= sign;
            *output_ptr = value;
            *pp += digits + sign_size;
            return 0;

        default:
            return 1;
    }
}
