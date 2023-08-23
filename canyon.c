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

void display_canyon(void) {
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    fill_VRAM(VRAM_PLANE_A + 0x000, 1, 0x800);
    paint_background( 3, 5, 8, 2, 1, 6);
    paint_background( 8, 8, 8, 2, 7, 6);
    paint_background(12, 2, 8, 2, 3, 6);
    paint_background(19, 7, 8, 2, 5, 6);
    paint_background(24, 4, 8, 2, 7, 6);
    paint_background(30, 8, 8, 2, 3, 6);
    paint_background(36, 1, 8, 2, 1, 6);
    paint_background(42, 6, 8, 2, 5, 6);
    paint_background(47, 3, 8, 2, 7, 6);
    paint_background(51, 7, 8, 2, 1, 6);
    paint_background(56, 2, 8, 2, 3, 6);
}
