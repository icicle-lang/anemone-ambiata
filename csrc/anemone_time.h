#ifndef __ANEMONE_TIME_H
#define __ANEMONE_TIME_H

#include "anemone_base.h"

#include <stdio.h>

#define ANEMONE_TIME_SUCCESS 0x0
#define ANEMONE_TIME_INVALID_DATE 0x1
#define ANEMONE_TIME_PARSE_ERROR 0x2

ANEMONE_INLINE
bool_t anemone_is_leap_year (int64_t year)
{
    int64_t div4 = (year % 4) == 0;
    int64_t div100 = (year % 100) == 0;
    int64_t div400 = (year % 400) == 0;

    return div4 && (!div100 || div400);
}

ANEMONE_INLINE
int64_t anemone_days_of_month (int64_t year, int64_t month)
{
    //
    // The only month affected by a leap year is February, and leap year
    // calculation is expensive as it requires a division, so only do it if
    // necessary.
    //

    if (ANEMONE_UNLIKELY(month == 2)) {
        return anemone_is_leap_year (year) ? 29 : 28;
    }

    //
    // We only need 2-bits per month as the number of days per month only
    // varies between 28-31, so all months are 28 + a 2-bit number.
    //
    // Standard Year:
    //   Days = 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    //   Bits = 11, 00, 11, 10, 11, 10, 11, 11, 10, 11, 10, 11
    //
    //   0b110011101110111110111011
    //   0xCEEFBB
    //
    // Leap Year:
    //   Days = 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    //   Bits = 11, 01, 11, 10, 11, 10, 11, 11, 10, 11, 10, 11
    //
    //   0b110111101110111110111011
    //   0xDEEFBB
    //

    //
    // We construct a bit pattern using the above information, which requires
    // one shift and one mask to extract the number of days.
    //
    // Because we've dealt with february / leap years already, we only need to
    // worry about the bit pattern for a standard year.
    //

    const int64_t days = 0xCEEFBB;

    int64_t shift = 2 * (12 - month);
    int64_t offset = (days >> shift) & 0x3;

    return 28 + offset;
}

ANEMONE_INLINE
bool_t anemone_gregorian_valid (int64_t year, int64_t month, int64_t day)
{
    if (month < 1 || 12 < month || day < 1) {
        return ANEMONE_FALSE;
    }

    if (day <= 28) {
        return ANEMONE_TRUE;
    }

    return day <= anemone_days_of_month (year, month);
}

ANEMONE_INLINE
int64_t anemone_gregorian_to_julian (int64_t year, int64_t month, int64_t day)
{
    //
    // Derived from the FORTRAN formula here:
    //   http://aa.usno.navy.mil/faq/docs/JD_Formula.php
    //
    int64_t month14 = month - 14;
    int64_t month14_12 = month14 / 12;
    return
        day - 32075 +
        1461 * (year + 4800 + month14_12) / 4 +
        367 * (month - 2 - month14_12 * 12) / 12 -
        3 * ((year + 4900 + month14_12) / 100) / 4;
}

ANEMONE_INLINE
int64_t anemone_gregorian_to_modified_julian (int64_t year, int64_t month, int64_t day)
{
    return anemone_gregorian_to_julian (year, month, day) - 2400001;
}

error_t anemone_parse_gregorian (
    const char **pp
  , const char *pe
  , int64_t *out_year
  , int64_t *out_month
  , int64_t *out_day
  );

error_t anemone_parse_gregorian_as_modified_julian (
    const char **pp
  , const char *pe
  , int64_t *out_modified_julian
  );

#endif//__ANEMONE_ATOI_H
