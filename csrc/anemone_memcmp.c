#include "anemone_memcmp.h"

int hs_anemone_memcmp8 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp8 (as, bs, len);
}

int hs_anemone_memcmp64 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp64 (as, bs, len);
}

int hs_anemone_memcmp128 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp128 (as, bs, len);
}

int hs_anemone_memcmp (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp (as, bs, len);
}

int hs_anemone_memeq8 (const void *as, const void *bs, size_t len)
{
    return anemone_memeq8 (as, bs, len);
}

int hs_anemone_memeq64 (const void *as, const void *bs, size_t len)
{
    return anemone_memeq64 (as, bs, len);
}

int hs_anemone_memeq128 (const void *as, const void *bs, size_t len)
{
    return anemone_memeq128 (as, bs, len);
}

int hs_anemone_memeq (const void *as, const void* bs, size_t len)
{
    return anemone_memeq (as, bs, len);
}
