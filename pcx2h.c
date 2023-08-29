#include <arpa/inet.h>
#include <sys/stat.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

unsigned short get_u16(unsigned char *buf, int offset) {
    return * (unsigned short *) (buf + offset);
}

int scale_color(unsigned char *buf, int i, int j) {
    return buf[(i * 3) + 16 + j] / 32;
}

unsigned short get_color(unsigned char *buf, int i) {
    int r = scale_color(buf, i, 0);
    int g = scale_color(buf, i, 1);
    int b = scale_color(buf, i, 2);
    return (b << 9) | (g << 5) | (r << 1);
}

int total_pixels = 0;
int pixel_amount = 0;
unsigned char current_pixel = 0;

void output_byte(int out, unsigned char byte) {
    dprintf(out, "0x%02x,", byte);
    if ((total_pixels++ & 0xf) == 0xf) {
	dprintf(out, "\n");
    }
}

void output_data(int out) {
    if (pixel_amount > 1 || (current_pixel & 0xc0) == 0xc0) {
	output_byte(out, pixel_amount | 0xc0);
	output_byte(out, current_pixel);
    }
    else {
	output_byte(out, current_pixel);
    }
    pixel_amount = 0;
}

void add_pixel(int out, unsigned char pixel) {
    if (pixel_amount == 0x3f || (current_pixel != pixel && pixel_amount > 0)) {
	output_data(out);
    }
    current_pixel = pixel;
    pixel_amount++;
}

void save_tile(int out, unsigned char *pixel, int x, int i, int j) {
    int y;
    for (y = 0; y < 8; y++) {
	int offset = (i * 4) + y * (x / 2) + j * 4 * x;
	for (int n = 0; n < 4; n++) {
	    add_pixel(out, pixel[offset + n]);
	}
    }
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

    char str[strlen(argv[2])];
    int trunc = sizeof("images/") - 1;
    int offset = strstr(argv[2], ".h") - argv[2];
    memcpy(str, argv[2] + trunc, offset - trunc);
    str[offset - trunc] = 0;

    remove(argv[2]);
    int out = open(argv[2], O_CREAT | O_RDWR, 0644);
    dprintf(out, "const u16 %s_palette[] = {\n", str);
    for (i = 0; i < 16; i++) {
	dprintf(out, "0x%04x,", get_color(buf, i));
	if ((i & 7) == 7) dprintf(out, "\n");
    }
    dprintf(out, "};\n");

    dprintf(out, "const byte %s_tiles[] = {\n", str);
    for (i = 0; i < (x / 8); i++) {
	for (j = 0; j < (y / 8); j++) {
	    save_tile(out, pixels, x, i, j);
	}
    }
    output_data(out);
    if (total_pixels & 0xf != 0) {
	dprintf(out, "\n");
    }
    dprintf(out, "};\n");

    close(out);
    free(pixels);
    free(buf);
    return 0;
}
