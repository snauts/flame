#include <arpa/inet.h>
#include <sys/stat.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

unsigned char ym2612[2][256];

int main(int argc, char **argv) {
    unsigned char c, r, d;
    if (argc < 2) goto error;
    memset(ym2612, 0, sizeof(ym2612));
    int in = open(argv[1], O_RDONLY);
    if (in < 0) goto error;
    lseek(in, 0x80, SEEK_SET);
    do {
	read(in, &c, 1);
	switch (c) {
	case 0x50:
	    read(in, &d, 1);
	    break;
	case 0x52:
	case 0x53:
	    read(in, &r, 1);
	    read(in, &d, 1);
	    ym2612[c & 1][r] = d;
	    break;
	case 0x62:
	    break;
	default:
	    printf("ERROR: unknown CMD (0x%02x)\n", c);
	    close(in);
	    return -EINVAL;
	}
    }
    while (c != 0x62);
    printf("    0x%02x, 0x%02x,\n", ym2612[0][0xb0], ym2612[0][0xb4]);
    close(in);

    for (r = 0x30; r < 0xa0; r += 0x4) {
	if ((r & 0xf) == 0) printf("    ");
	printf("0x%02x,", ym2612[0][r]);
	printf((r & 0xf) == 0xc ? "\n" : " ");
    }

    return 0;
  error:
    printf("vgm-dump [filename]\n");
    return -ENOENT;
}
