#include <stdlib.h>
#include <stdio.h>
#include "tests/testutils.h"

void *
verbose_malloc(size_t size)
{
    void *p = malloc(size);
    printf("Allocated %p.\n", p);
    return p;
}
