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

static bool is_8bit(unsigned char *buf) {
    return (buf[3] == 8);
}

unsigned short get_u16(unsigned char *buf, int offset) {
    return * (unsigned short *) (buf + offset);
}

static int palette_offset = 16;

int scale_color(unsigned char *buf, int i, int j) {
    return buf[(i * 3) + palette_offset + j] / 32;
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

static int output_size = 0;
void output_byte(int out, unsigned char byte) {
    output_size++;
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

void save_tile_4bit(int out, unsigned char *pixel, int x, int i, int j) {
    int y;
    for (y = 0; y < 8; y++) {
	int offset = (i * 4) + y * (x / 2) + j * 4 * x;
	for (int n = 0; n < 4; n++) {
	    add_pixel(out, pixel[offset + n]);
	}
    }
}

void save_tile_8bit(int out, unsigned char *pixel, int x, int i, int j) {
    int y;
    unsigned char nibble;
    for (y = 0; y < 8; y++) {
	int offset = (i * 8) + y * x + j * 8 * x;
	for (int n = 0; n < 8; n++) {
	    if ((n & 0x1) == 0) {
		nibble = (pixel[offset + n] & 0xf) << 4;
	    }
	    else {
		add_pixel(out, nibble | (pixel[offset + n] & 0xf));
	    }
	}
    }
}

static char *input = NULL;
static char *output = NULL;

bool save_pallete = true;

void parse_args(int argc, char **argv) {
    int i;
    for (i = 1; i < argc; i++) {
	if (argv[i][0] != '-') {
	    if (input == NULL) {
		input = argv[i];
	    }
	    else if (output == NULL) {
		output = argv[i];
	    }
	    else {
		printf("ERROR: garbage arguments\n");
	    }
	}
	else {
	    for (int j = 1; j < strlen(argv[i]); j++) {
		switch(argv[i][j]) {
		case 'p':
		    save_pallete = false;
		    break;
		default:
		    printf("ERROR: unknown flag\n");
		    break;
		}
	    }
	}
    }
}

int main(int argc, char **argv) {
    if (argc < 3) {
	printf("usage: pcx2h [PCX-file] [C-header-file]\n");
	return 0;
    }
    parse_args(argc, argv);
    struct stat st;
    if (stat(input, &st) != 0) {
	printf("ERROR while opening PCX-file \"%s\"\n", input);
	return -ENOENT;
    }
    unsigned char *buf = malloc(st.st_size);
    int in = open(input, O_RDONLY);
    read(in, buf, st.st_size);
    close(in);

    int x = get_u16(buf, 0x8) + 1;
    int y = get_u16(buf, 0xa) + 1;
    int unpacked_size = x * y / (is_8bit(buf) ? 1 : 2);
    unsigned char *pixels = malloc(unpacked_size);

    int i = 128, j = 0;
    while (j < unpacked_size) {
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

    char str[strlen(output)];
    int trunc = sizeof("images/") - 1;
    int offset = strstr(output, ".h") - output;
    memcpy(str, output + trunc, offset - trunc);
    str[offset - trunc] = 0;

    remove(output);
    int out = open(output, O_CREAT | O_RDWR, 0644);

    if (save_pallete) {
	if (is_8bit(buf)) {
	    palette_offset = st.st_size - 768;
	}
	dprintf(out, "static const u16 %s_palette[] = {\n", str);
	for (i = 0; i < 16; i++) {
	    dprintf(out, "0x%04x,", get_color(buf, i));
	    if ((i & 7) == 7) dprintf(out, "\n");
	}
	dprintf(out, "};\n");
    }

    dprintf(out, "static const byte %s_tiles[] = {\n", str);
    for (i = 0; i < (x / 8); i++) {
	for (j = 0; j < (y / 8); j++) {
	    if (is_8bit(buf)) {
		save_tile_8bit(out, pixels, x, i, j);
	    }
	    else {
		save_tile_4bit(out, pixels, x, i, j);
	    }
	}
    }
    output_data(out);
    if (total_pixels & 0xf != 0) {
	dprintf(out, "\n");
    }
    dprintf(out, "};\n");

    dprintf(out, "const Image %s_img = {\n", str);
    dprintf(out, ".palette = %s_palette,\n", str);
    dprintf(out, ".tiles = %s_tiles,\n", str);
    dprintf(out, ".size = %d,\n", output_size);
    dprintf(out, "};\n");

    close(out);
    free(pixels);
    free(buf);
    return 0;
}
