#include "anemone_mempool.h"
#include <string.h>

// Allocate a new block with given predecessor 
ANEMONE_STATIC
anemone_block_t * anemone_block_create (anemone_block_t *prev, size_t num_bytes)
{
  void     *ptr = malloc (num_bytes);
  anemone_block_t  src = { ptr, prev };

  anemone_block_t *dst = malloc (sizeof (anemone_block_t));
  memcpy (dst, &src, sizeof (anemone_block_t));

  ANEMONE_MEMPOOL_WHEN_DEBUG(fprintf (stderr, "iblock_create: %p\n", dst->ptr));

  return dst;
}

// Free a block and all its predecessors
ANEMONE_STATIC
void anemone_block_free (anemone_block_t *block)
{
  if (block == 0) return;

  ANEMONE_MEMPOOL_WHEN_DEBUG(fprintf (stderr, "iblock_free: %p\n", block->ptr));

  free (block->ptr);
  anemone_block_t *prev = block->prev;
  free (block);
  anemone_block_free (prev);
}

// Allocate a new block into a pool
ANEMONE_STATIC
void anemone_mempool_add_block (anemone_mempool_t *pool)
{
  anemone_block_t *next = anemone_block_create (pool->last, anemone_block_size);

  pool->last        = next;
  pool->current_ptr = next->ptr;
  pool->maximum_ptr = next->ptr + anemone_block_size;
}

// Allocate a new block after initial bump allocation has failed.
// This happens when there is not enough space in the current block for the requested bytes.
void * anemone_mempool_alloc_block (anemone_mempool_t *pool, size_t num_bytes)
{
  // Allocating into the current block failed, so we need a new block.
  // Will the requested amount fit in a single block?
  if (num_bytes <= anemone_block_size) {
    // If so, add a new block as usual, then set it as the current block
    anemone_mempool_add_block (pool);
    ANEMONE_MEMPOOL_TRY_ALLOC(anemone_mempool_alloc_block);

    return 0;
  } else {
    // The requested size is larger than a normal block.
    // We will allocate a single block of the exact size and inject it before the current block.
    // That way, any leftover parts in the current block can be used by the next allocation
    anemone_block_t *last = pool->last;

    // anemone_mempool_t.last is always non-null, so dereference is safe
    anemone_block_t *prev = last->prev;
    // Create a new block that points to the previous block
    anemone_block_t *inject = anemone_block_create (prev, num_bytes);
    // and replace current previous with our new one
    last->prev = inject;
    return inject->ptr;
  }
}


// External function for allocating from Haskell.
// Because anemone_mempool_alloc is marked inline, it is not exposed via the linker.
void * hs_anemone_mempool_alloc (anemone_mempool_t *pool, size_t num_bytes)
{
  return anemone_mempool_alloc(pool, num_bytes);
}

// Create a new memory pool
anemone_mempool_t * anemone_mempool_create ()
{
  anemone_mempool_t *pool = calloc(1, sizeof(anemone_mempool_t));

  ANEMONE_MEMPOOL_WHEN_DEBUG(fprintf (stderr, "anemone_mempool_create: %p\n", pool));

  anemone_mempool_add_block (pool);

  return pool;
}


#if ANEMONE_MEMPOOL_DEBUG
// Debug function for printing out the different block sizes
void anemone_mempool_debug_block_usage (anemone_mempool_t *pool)
{
  anemone_block_t *block = pool->last;
  uint64_t size = 0;
  uint64_t blocks = 0;
  while (block != 0) {
    size += anemone_block_size;
    blocks++;
    block = block->prev;
  }
  fprintf (stderr, "anemone_mempool_debug_block_usage: %llu blocks, %llu bytes\n", blocks, size);
}
#endif

// Free a memory pool and all its resources
void anemone_mempool_free (anemone_mempool_t *pool)
{
  ANEMONE_MEMPOOL_WHEN_DEBUG(fprintf (stderr, "anemone_mempool_free: %p\n", pool));
  ANEMONE_MEMPOOL_WHEN_DEBUG(anemone_mempool_debug_block_usage (pool));

  anemone_block_free (pool->last);
  free(pool);
}
