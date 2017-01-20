#include "anemone_base.h"

#include <string.h>

#if ANEMONE_MEMPOOL_DEBUG
#include <stdio.h>
#include <inttypes.h>
#define ANEMONE_MEMPOOL_WHEN_DEBUG(f) f
#else
#define ANEMONE_MEMPOOL_WHEN_DEBUG(f)
#endif

static const size_t anemone_block_size = 1 * 1024 * 1024;

// A single block containing some allocated data.
// It keeps a link to the previous block so it can be freed.
typedef struct anemone_block {
  // Payload data
  // ptr != 0
  void * const ptr;
  // Previous block
  struct anemone_block * prev;
} anemone_block_t;

// A memory pool which contains the current block, and how much of the block is used
// We store usage information here because it is only relevant for the current block.
typedef struct {
  // last != 0
  anemone_block_t *last;
  int64_t   total_alloc_size;
  // last->ptr <= current_ptr < maximum_ptr */
  void     *current_ptr;
  // maxmimum_ptr - last->ptr >= anemone_block_size
  void     *maximum_ptr;
} anemone_mempool_t;

/* This has to be a macro as when it's a function we end up with an extra
 * conditional because we need to return something to indicate whether we
 * succeeded or not. */
#define ANEMONE_MEMPOOL_TRY_ALLOC(fn_name)                                                             \
  void *ptr  = pool->current_ptr;                                                                      \
  void *next = ptr + num_bytes;                                                                        \
                                                                                                       \
  if (next <= pool->maximum_ptr) {                                                                     \
    ANEMONE_MEMPOOL_WHEN_DEBUG(fprintf (stderr, #fn_name ": %p (allocated %zu bytes)\n", ptr, num_bytes)); \
    pool->current_ptr = next;                                                                          \
    return ptr;                                                                                        \
  }

// Create a new memory pool
anemone_mempool_t * anemone_mempool_create ();

// Free the memory pool and all memory that was allocated from it
void anemone_mempool_free (anemone_mempool_t *pool);

// Allocate data using a new block. This is for internal use only, but must be exposed for inlining.
void * anemone_mempool_alloc_block (anemone_mempool_t *pool, size_t num_bytes);

// Allocate data into the pool.
ANEMONE_INLINE
void * anemone_mempool_alloc (anemone_mempool_t *pool, size_t num_bytes)
{
    ANEMONE_MEMPOOL_TRY_ALLOC(anemone_mempool_alloc);

    return anemone_mempool_alloc_block (pool, num_bytes);
}

// Allocate data into the pool, zeroing its contents
ANEMONE_INLINE
void * anemone_mempool_calloc (anemone_mempool_t *pool, size_t num_items, size_t num_bytes)
{
    size_t total = num_items * num_bytes;
    void *ret = anemone_mempool_alloc (pool, total);
    memset (ret, 0, total);
    return ret;
}

// Get the total alloc size - this is only necessary for calling from Haskell
int64_t anemone_mempool_total_alloc_size (anemone_mempool_t *pool);

