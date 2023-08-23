#include <sys/stat.h>
#include <stdint.h>
#include <unistd.h>
#include <malloc.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

unsigned short get_u16(unsigned char *buf, int offset) {
    return * (unsigned short *) (buf + offset);
}

int main(int argc, char **argv) {
    if (argc < 3) {
	printf("usage: pcx2h [PCX-file] [C-header-file]\n");
	return 0;
    }
    struct stat st;
    if (stat(argv[1], &st) != 0) {
	printf("ERROR while opening PCX-file \"%s\"\n", argv[1]);
	return -ENOENT;
    }
    unsigned char *buf = malloc(st.st_size);
    int in = open(argv[1], O_RDONLY);
    read(in, buf, st.st_size);
    close(in);

    int x = get_u16(buf, 0x8) + 1;
    int y = get_u16(buf, 0xa) + 1;
    unsigned char *pixels = malloc(x * y / 2);

    int i = 128, j = 0;
    while (i < st.st_size) {
	if ((buf[i] & 0xc0) == 0xc0) {
	    int count = buf[i++] & 0x3f;
	    while (count-- > 0) {
		pixels[j++] = buf[i];
	    }
	    i++;
	}
	else {
	    pixels[j++] = buf[i++];
	}
    }

    remove(argv[2]);
    int out = open(argv[2], O_CREAT);
    close(out);
    free(buf);
    return 0;
}
