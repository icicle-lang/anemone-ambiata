#include "memcmp16.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

void print128(__m128i value) {
    int64_t *v64 = (int64_t*) &value;
    printf("%.16llx %.16llx\n", v64[1], v64[0]);
}

static void test_both(const char* buf1, const char* buf2, uint64_t len)
{
    printf("Comparing '%s' against '%s' with length %lld\n", buf1, buf2, len);
    printf("Memcmp %d, memcmp16 %d\n", memcmp(buf1, buf2, len), memcmp16(buf1, buf2, len));

}

int main()
{
#define TEST(a,b) test_both(a, b, sizeof(a) < sizeof(b) ? sizeof(a) : sizeof(b));
    TEST("a", "b");
    TEST("monkey man", "monkey gun");
    TEST("guerilla man", "monkey gun");
    TEST("gwhareif geias n", "gwhareif geias n");
    TEST("gwhareif geias gwhareif geias n", "gwhareif geias bleeding from");
    int ret = memcmp16("a", "b", 1);
    printf("Hello %d\n", ret);
}

