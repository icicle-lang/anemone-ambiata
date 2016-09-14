#include "anemone_mempool.h"

#include <errno.h>
#include <stdio.h>
#include <sys/resource.h>


// This function "works" on Linux and Darwin, but returns kilobytes in Linux and bytes in Darwin.
// We wrap it with an OS-specific get_mem_usage_bytes.
long get_mem_usage_undefined_units ()
{
  struct rusage usage;
  if (getrusage (RUSAGE_SELF, &usage)) {
    fprintf (stderr, "call to getrusage failed. errno = %d\n", errno);
    exit (1);
  }

  return usage.ru_maxrss;
}

#if __APPLE__

long get_mem_usage_bytes ()
{
  return get_mem_usage_undefined_units ();
}

#elif __gnu_linux__

long get_mem_usage_bytes ()
{
  // Convert from kilobytes to bytes
  return get_mem_usage_undefined_units () * 1024;
}

#else
// We don't use BSD or Solaris, and Windows is not a thing.
#error unknown operating system
#endif


// Test that the memory pool is freeing memory as it should be.
// On failure, prints to stderr and returns false.
bool_t test_mempool_free(uint64_t outer_iterations, uint64_t inner_iterations, size_t max_bytes)
{
  // After the first outer iteration, we will store the last heap size in here
  long first_size = -1;

  for (uint64_t outer = 0; outer != outer_iterations; ++outer) {
    anemone_mempool_t *pool = anemone_mempool_create();


    for (uint64_t inner = 0; inner != inner_iterations; ++inner) {
      // Allocate, make sure it's not null
      void *value = anemone_mempool_alloc(pool, max_bytes);
      if (!value) {
        fprintf (stderr, "anemone_mempool_alloc returned null, outer iteration %llu, inner iteration %llu\n\ttried to allocate %ld bytes", outer, inner, max_bytes);
        return 0;
      }
    }

    anemone_mempool_free(pool);

    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage)) {
      fprintf (stderr, "call to getrusage failed, iteration %llu. errno = %d\n", outer, errno);
      return 0;
    }

    long new_size = get_mem_usage_bytes ();
    if (first_size == -1) {
      first_size = new_size;
    } else {
      // Give it a megabyte of leeway so that any unexpected allocations from GHC don't get in the way.
      long check_size = first_size + 1024*1024;
      if (new_size > check_size) {
        fprintf (stderr, "iteration %llu\n\tnew_size > first_size + 1MB\n\t%ld (bytes) > %ld (bytes)\n", outer, new_size, first_size);
        return 0;
      }
    }
  }


  return 1;
}


// Test that the memory pool allocates non-overlapping memory
// On failure, prints to stderr and returns false.
bool_t test_mempool_nonoverlap(uint64_t iterations, size_t max_bytes)
{
  uint8_t* pointers[iterations];
  anemone_mempool_t *pool = anemone_mempool_create();

  // Allocate a bunch of pointers, fill every byte with current iteration mod 256
  for (uint64_t i = 0; i != iterations; ++i) {
    uint8_t *value = anemone_mempool_alloc(pool, max_bytes);
    if (!value) {
      fprintf (stderr, "anemone_mempool_alloc returned null");
      return 0;
    }
    // Set the bytes
    for (uint64_t byte = 0; byte != max_bytes; ++byte) {
      value[byte] = (uint8_t)i;
    }

    pointers[i] = value;
  }

  for (uint64_t i = 0; i != iterations; ++i) {
    uint8_t *value = pointers[i];
    uint8_t count_byte = (uint8_t)i;

    for (uint64_t byte = 0; byte != max_bytes; ++byte) {
      if (value[byte] != count_byte) {
        fprintf (stderr, "at iteration %llu, the %llu-th byte expected to be %d but got %d", i, byte, count_byte, value[byte]);
        return 0;
      }
    }
  }

  anemone_mempool_free(pool);

  return 1;
}

