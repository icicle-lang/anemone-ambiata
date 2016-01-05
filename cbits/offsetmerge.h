#include <stdint.h>

char* offset_merge
  ( int output_fd
  , uint64_t num_inputs
  , int* input_fds, uint64_t* input_offsets, uint64_t* input_lengths);

