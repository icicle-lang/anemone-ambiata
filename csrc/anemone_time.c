#include "anemone_atoi.h"
#include "anemone_time.h"

ANEMONE_STATIC
ANEMONE_INLINE
int64_t gregorian_to_mjd (int64_t year, int64_t month, int64_t day)
{
    // Derived from the FORTRAN formula here: http://aa.usno.navy.mil/faq/docs/JD_Formula.php
    int64_t julian =
        day - 32075 +
        1461 * (year + 4800 + (month - 14) / 12) / 4 +
        367 * (month - 2 - (month - 14) / 12 * 12) / 12 -
        3 * ((year + 4900 + (month - 14) / 12) / 100) / 4;

    return julian - 2400001;
}

error_t anemone_string_to_mjd (const char **pp, const char *pe, int64_t *output_ptr)
{
                               /* p + 0123456789 */
    const size_t date_size = sizeof ("yyyy-mm-dd") - 1 /* nul */;

    const char *p = *pp;
    const size_t size = pe - p;

    if (size >= date_size &&
        anemone_is_digit (p[0]) &&
        anemone_is_digit (p[1]) &&
        anemone_is_digit (p[2]) &&
        anemone_is_digit (p[3]) &&
                   '-' == p[4]  &&
        anemone_is_digit (p[5]) &&
        anemone_is_digit (p[6]) &&
                   '-' == p[7]  &&
        anemone_is_digit (p[8]) &&
        anemone_is_digit (p[9])) {

        const int64_t year =
            (p[0] - '0') * 1000 +
            (p[1] - '0') * 100 +
            (p[2] - '0') * 10 +
            (p[3] - '0');

        const int64_t month =
            (p[5] - '0') * 10 +
            (p[6] - '0');

        const int64_t day =
            (p[8] - '0') * 10 +
            (p[9] - '0');

        *pp = p + date_size;
        *output_ptr = gregorian_to_mjd (year, month, day);

        return 0;
    }

    return 1;
}
