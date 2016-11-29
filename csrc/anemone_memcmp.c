#include "anemone_memcmp.h"
#include "anemone_memcmp_zoo.h"

int hs_anemone_memcmp8 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp8 (as, bs, len);
}

int hs_anemone_memcmp64 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp64 (as, bs, len);
}

int hs_anemone_memcmp_partial_load64 (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp_partial_load64 (as, bs, len);
}

int hs_anemone_memcmp128_unsafe (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp128_unsafe (as, bs, len);
}

int hs_anemone_memcmp (const void *as, const void *bs, size_t len)
{
    return anemone_memcmp (as, bs, len);
}

