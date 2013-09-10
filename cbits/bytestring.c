#include "bytestring.h"

/* update a character inside a string */
void bytestring_update(unsigned char *p, size_t index, unsigned char w) {
    p[index] = w;
}
