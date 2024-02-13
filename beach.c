#include "main.h"
#include "images/beach.h"

static void draw_sky(void) {
    for (u16 i = 0; i < 28; i++) {
	u16 tile;
	if (i <= 12) {
	    tile = i + 1;
	}
	else {
	    tile = 13 + (i % 3);
	}
	fill_VRAM(0x80 * i, TILE(0, tile), 0x040);
    }
}

static const u16 sea_palette[][3] = {
    { 0x0aa6, 0x0aa4, 0x0aa2 },
    { 0x0aa4, 0x0aa2, 0x0aa6 },
    { 0x0aa2, 0x0aa6, 0x0aa4 },
};

static void sea_rotate(u16 i) {
    update_palette(sea_palette[i], 4, ARRAY_SIZE(sea_palette[i]));
    upload_palette(0);

    callback(&sea_rotate, 4, i < 2 ? i + 1 : 0);
}

void display_nippon(Function prepare_level) {
    update_palette(beach_palette, 0, ARRAY_SIZE(beach_palette));
    update_tiles(beach_tiles, 1, ARRAY_SIZE(beach_tiles));

    /* load tiles */
    load_soldier_tiles(2);
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
    fill_bottom_row();
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_battotai(void);
    music_battotai();

    callback(&fade_in, 0, 6);
    callback(&sea_rotate, 30, 0);
    switch_frame(&update_game);
}

void display_beach(void) {
    void prepare_beach_level(void);
    display_nippon(&prepare_beach_level);
}
