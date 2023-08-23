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

    fill_VRAM(VRAM_PLANE_A + 0x000, 3, 0x800);
    paint_background( 5, 5, 9, 3, 1, 3);
    paint_background(24, 4, 9, 3, 1, 3);
    paint_background(20, 7, 9, 3, 4, 3);
    paint_background(12, 2, 9, 3, 4, 3);
}
