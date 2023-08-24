#include "main.h"

#include "images/canyon.h"

void paint_background(int x, int y, int w, int h, int i, int n) {
    int dx, dy;
    for (dx = 0; dx < w; dx++) {
	for (dy = 0; dy < h; dy++) {
	    poke_VRAM(VRAM_PLANE_B + ((x + dx) * 2) + ((y + dy) * 128), i);
	    i += 1;
	}
	i += n;
    }
}

const byte cloud_data[] = {
    3, 5, 1, 8, 8, 7, 12, 2, 3, 19, 7, 5, 24, 4, 7, 30, 8, 3,
    36, 1, 1, 42, 6, 5, 47, 3, 7, 51, 7, 1, 56, 2, 3,
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

const byte cacti[] = { 69, 77, 85, 93, 70, 86, 78, 94 };

const byte cacti_spacing[] = {
    2, 10, 8, 12, 6, 6, 18, 10, 6, 28, 10, 20, 8, 10, 24, 6, 26, 8, 8, 12,
    10, 6, 6, 18, 14, 6, 22, 6, 6, 10, 8, 10, 34, 18, 18, 12, 22, 20, 14,
    8, 22, 16, 20, 16, 6, 10, 24, 20, 6, 16, 16, 26, 24, 6, 78, 14, 56,
    6, 20, 38, 28, 22, 26, 30, 42, 22, 16, 56, 26, 22, 48, 114, 28,
};

static void draw_vegetation(void) {
    int i, offset = 0x700, tile = 0;
    for (i = 0; i < ARRAY_SIZE(cacti_spacing); i++) {
	offset += cacti_spacing[i];
	poke_VRAM(VRAM_PLANE_B + offset, cacti[(tile + offset) & 7]);
	tile++;
    }
}

const byte sand[] = { 72, 80, 88, 71, 79, 87, 95, 72 };

static void draw_sand(void) {
    int i;
    seed = 1;
    for (i = 0x700; i < 0xe00; i += 2) {
	poke_VRAM(VRAM_PLANE_B + i, sand[random() & 7]);
    }
}

void display_canyon(void) {
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    fill_VRAM(VRAM_PLANE_B + 0x000,  1, 0x300);
    fill_VRAM(VRAM_PLANE_B + 0x680, 96, 0x40);

    draw_sand();
    draw_clouds();
    draw_horizon();
    draw_vegetation();
}
