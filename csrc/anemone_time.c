#include "anemone_atoi.h"
#include "anemone_time.h"

ANEMONE_STATIC
ANEMONE_INLINE
error_t inline_parse_gregorian (
    const char **pp
  , const char *pe
  , int64_t *out_year
  , int64_t *out_month
  , int64_t *out_day
  )
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

        *out_year = year;
        *out_month = month;
        *out_day = day;
        *pp = p + date_size;

        if (anemone_gregorian_valid (year, month, day)) {
            return ANEMONE_TIME_SUCCESS;
        }

        return ANEMONE_TIME_INVALID_DATE;
    }

    *out_year = 0;
    *out_month = 0;
    *out_day = 0;

    return ANEMONE_TIME_PARSE_ERROR;
}

error_t anemone_parse_gregorian (
    const char **pp
  , const char *pe
  , int64_t *out_year
  , int64_t *out_month
  , int64_t *out_day
  )
{
    return inline_parse_gregorian (pp, pe, out_year, out_month, out_day);
}

error_t anemone_parse_gregorian_as_modified_julian (
    const char **pp
  , const char *pe
  , int64_t *out_modified_julian
  )
{
    int64_t year, month, day;

    error_t err = inline_parse_gregorian (pp, pe, &year, &month, &day);
    *out_modified_julian = anemone_gregorian_to_modified_julian (year, month, day);

    return err;
}

//
// Functions to be called from Haskell only, the other side will assume we've
// consumed 10 bytes if the call succeeds, this avoids a call to 'alloca' on
// the haskell side for 'pp' which costs about 6-7ns once we've poked/peeked
// it. We avoid 'alloca' for the return values by packing them all in to a
// single int64.
//

int64_t anemone_parse_gregorian_hs (const char *p, const char *pe)
{
    int64_t year0, month0, day0;
    int64_t err = inline_parse_gregorian (&p, pe, &year0, &month0, &day0);

    int64_t year = (int16_t) year0;
    int64_t month = (int8_t) month0;
    int64_t day = (int8_t) day0;

    return (err & 0xFFFFFFFF) | (year << 48) | (month << 40) | (day << 32);
}

int64_t anemone_parse_gregorian_as_modified_julian_hs (const char *p, const char *pe)
{
    int64_t year0, month0, day0;
    int64_t err = inline_parse_gregorian (&p, pe, &year0, &month0, &day0);

    if (err) {
        int64_t year = (int16_t) year0;
        int64_t month = (int8_t) month0;
        int64_t day = (int8_t) day0;

        return (err & 0xFFFFFFFF) | (year << 48) | (month << 40) | (day << 32);
    }

    int64_t modified_julian0 = anemone_gregorian_to_modified_julian (year0, month0, day0);

    // truncate but keep sign
    int64_t modified_julian = (int32_t) modified_julian0;

    // err == 0
    return modified_julian << 32;
}
