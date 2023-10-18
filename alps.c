#include "main.h"

#include "images/alps.h"

static u16 draw_one_mountain(u16 x, byte tile) {
    if (tile == 1 || tile == 33) {
	paint_background(x, 10, 4, 2, tile, 6);
	x += 4;
    }
    else {
	/* 3, 19, 35, 51 */
	paint_background(x, 11, 2, 1, tile, 7);
	x += 2;
    }
    return x;
}

static const byte mountain_tiles[] = {
    1, 3, 19, 51, 33, 1, 35, 33, 51, 35, 3, 1,
    19, 35, 33, 3, 35, 1, 51, 19, 33, 19, 3, 51,
};

static void draw_mountains(void) {
    u16 x = 0, i = 0;
    while (x < 64) {
	x = draw_one_mountain(x, mountain_tiles[i++]);
    }
}

void display_mountains(void) {
    /* load tiles */
    update_palette(alps_palette, 0, ARRAY_SIZE(alps_palette));
    update_tiles(alps_tiles, 1, ARRAY_SIZE(alps_tiles));

    load_soldier_tiles(1);
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0x000, 12, 0x300);
    fill_VRAM(0x600,  4, 0x500);

    draw_mountains();

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    clear_DMA_buffer(0, 0x1000);

    prepare_mountain_level();

    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_erika(void);
    music_erika();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}
