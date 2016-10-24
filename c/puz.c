#include <stdio.h>
#include <stdlib.h>

unsigned short cksum_region(unsigned char *, int, unsigned short);

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: puz <path/to.puz>\n");
        return 1;
    }

    FILE *puzFile = fopen(argv[1], "r");
    unsigned char buf[1024];
    size_t bytesRead = fread(buf, 1, 1024, puzFile);
    if (ferror(puzFile)) {
        fprintf(stderr, "Error reading file (%s)\n", argv[1]);
        return 1;
    }

    unsigned short ck = cksum_region(buf + 0x2C, 8, 0);

    printf("Bytes: [ ");
    for (unsigned char *c = buf + 0x2C; c < buf + 0x2C + 8; c++) {
        printf("%hhu, ", *c);
    }
    printf("]\n");
    printf("Checksum: %hu\n", ck);
    return 0;
}

unsigned short cksum_region(unsigned char *base, int len, unsigned short cksum) {
    int i;
    for (i = 0; i < len; i++) {
        printf("TOP: %hu\t", cksum);
        if (cksum & 0x0001)
            cksum = (cksum >> 1) + 0x8000;
        else
            cksum = cksum >> 1;
        cksum += *(base+i);
        printf("BOTTOM: %hu\n", cksum);
    }

    printf("RETURN: %hu\n", cksum);
    return cksum;
}
