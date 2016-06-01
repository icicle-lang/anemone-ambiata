#ifndef __ANEMONE_ATOI_SSE_H
#define __ANEMONE_ATOI_SSE_H



/*
  parse string to unsigned 64-bit integer.
  does not strip whitespace.
 */
bool anemone_string_to_ui64_v128(char** pp, char* pe, uint64_t* out_val);

/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
bool anemone_string_to_i64_v128(char** pp, char* pe, int64_t* out_val);


#endif//__ANEMONE_ATOI_SSE_H
