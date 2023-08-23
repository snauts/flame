#include "main.h"

#include "images/canyon.h"

void paint_background(int x, int y, int w, int h, int i, int n) {
    int dx, dy;
    for (dx = 0; dx < w; dx++) {
	for (dy = 0; dy < h; dy++) {
	    poke_VRAM(VRAM_PLANE_A + ((x + dx) * 2) + ((y + dy) * 128), i);
	    i += 1;
	}
	i += n;
    }
}

const byte cloud_data[] = {
    3,  5, 1,
    8,  8, 7,
    12, 2, 3,
    19, 7, 5,
    24, 4, 7,
    30, 8, 3,
    36, 1, 1,
    42, 6, 5,
    47, 3, 7,
    51, 7, 1,
    56, 2, 3,
};

static void paint_cloud(int x, int y, int i) {
    paint_background(x, y, 8, 2, i, 6);
}

static void draw_clouds(void) {
    int i;
    for (i = 0; i < ARRAY_SIZE(cloud_data); i += 3) {
	paint_cloud(cloud_data[i + 0], cloud_data[i + 1], cloud_data[i + 2]);
    }
}

const byte horizon_data[] = {
    65, 66, 67, 68, 67, 66, 65, 67, 65, 68, 66, 68, 65, 67, 68, 66
};

static void draw_horizon(void) {
    int x, i = 0;
    for (x = 0; x <= 60; x += 4) {
	paint_background(x, 12, 4, 1, horizon_data[i++], 7);
    }
}

const byte cacti[] = { 97, 105, 113, 121 };

static void draw_vegetation(void) {
    int i = 0x700, j = 10, k = 0;
    while (i < 0xd00) {
	poke_VRAM(VRAM_PLANE_A + i, cacti[k++ & 3]);
	i += j;
	j += 2;
    }
}

void display_canyon(void) {
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    fill_VRAM(VRAM_PLANE_A + 0x000,  1, 0x300);
    fill_VRAM(VRAM_PLANE_A + 0x680, 69, 0x3c0);
    fill_VRAM(VRAM_PLANE_A + 0x680, 70, 0x40);

    draw_clouds();
    draw_horizon();
    draw_vegetation();
}
