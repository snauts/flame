#include "main.h"

#include "images/town.h"

#define POS(x, y) ((0x80 * (y)) + ((x) << 1))

static void draw_sky(void) {
    for (u16 i = 0; i < 11; i++) {
	fill_VRAM(0x80 * i, TILE(0, i + 1), 0x040);
    }
    static const u16 stars[] = {
	POS(62, 1), 12, POS(25, 1), 12, POS(21, 2), 13,
	POS( 6, 3), 14, POS(32, 3), 14, POS(13, 4), 15,
	POS(44, 4), 15, POS(37, 5), 16, POS(50, 6), 17,
	POS( 8, 7), 18, POS(23, 8), 19, POS(42, 8), 19,
	POS(57, 9), 20, POS(17, 9), 20,
    };
    for (u16 i = 0; i < ARRAY_SIZE(stars); i += 2) {
	poke_VRAM(stars[i], stars[i + 1]);
    }
}

static void display_french(Function prepare_level) {
    /* load tiles */

    update_palette(town_palette, 0, ARRAY_SIZE(town_palette));
    update_tiles(town_tiles, 1, ARRAY_SIZE(town_tiles));

    load_soldier_tiles(3);
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

   /* foreground */
    clear_DMA_buffer(0, 0x1000);

    prepare_level();

    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_onions(void);
    music_onions();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}

void display_town(void) {
    void prepare_town_level(void);
    display_french(&prepare_town_level);
}
