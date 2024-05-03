#include "main.h"

#include "images/rain.h"

#include "forest.inc"

static void sky_piece(u16 x, u16 y, u16 dx, u16 dy) {
    paint_background(x, y, 2, 1, dx * 14 + dy + 1, 6);
}

static void draw_sky(void) {
    u16 i = 0;
    for (u16 x = 0; x < 64; x += 2) {
	sky_piece(x, 0, i, 0);
	i = (i + (random() % 3) + 1) & 3;
    }
}

static void display_soviet(const Level *level) {
    set_seed(1905);
    load_soldier_tiles(4);

    load_image(&rain_img, 1, 0);

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    prepare_level(level);

    void music_katyusha(void);
    music_katyusha();
}

void display_forest(void) {
    display_soviet(&forest_level);
}
