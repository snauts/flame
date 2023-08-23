#include "main.h"

#include "images/canyon.h"

void display_canyon(void) {
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    fill_VRAM(VRAM_PLANE_A + 0x000, 1, 0x200);
    fill_VRAM(VRAM_PLANE_A + 0x400, 2, 0x080);
    fill_VRAM(VRAM_PLANE_A + 0x480, 3, 0x600);
}
