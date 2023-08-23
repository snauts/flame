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

static void paint_cloud(int x, int y, int i) {
    paint_background(x, y, 8, 2, i, 6);
}

void display_canyon(void) {
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    fill_VRAM(VRAM_PLANE_A + 0x000,  1, 0x300);
    fill_VRAM(VRAM_PLANE_A + 0x600, 65, 0x400);
    paint_cloud( 3, 5, 1);
    paint_cloud( 8, 8, 7);
    paint_cloud(12, 2, 3);
    paint_cloud(19, 7, 5);
    paint_cloud(24, 4, 7);
    paint_cloud(30, 8, 3);
    paint_cloud(36, 1, 1);
    paint_cloud(42, 6, 5);
    paint_cloud(47, 3, 7);
    paint_cloud(51, 7, 1);
    paint_cloud(56, 2, 3);
}
