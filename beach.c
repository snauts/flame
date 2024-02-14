#include "main.h"
#include "images/beach.h"

static void trail_could(u16 x, u16 y, u16 l1, u16 l2) {
    for (u16 i = 0; i < 8; i++) {
	u16 count = 1;
	if (i == 2) count = l1;
	if (i == 3) count = l2;
	for (u16 j = 0; j < count; j++) {
	    paint_background(x, y, 1, 2, 71 + (i << 3), 6);
	    x++;
	}
    }
}

static void draw_sky(void) {
    for (u16 i = 0; i < 11; i++) {
	fill_VRAM(0x80 * i, TILE(0, i + 1), 0x040);
    }
    paint_background(15, 2, 8, 2, 65, 6);
    paint_background(56, 2, 8, 2, 65, 6);
    paint_background(25, 4, 8, 2, 67, 6);
    paint_background( 5, 5, 8, 2, 67, 6);
    paint_background(44, 7, 8, 2, 69, 6);

    trail_could(10, 9, 5, 8);
    trail_could(35, 9, 7, 9);
    trail_could(30, 8, 2, 1);
}

static void draw_sea(void) {
    u16 i, k = 0;
    for (i = 0; i < 8; i++) {
	for (u16 n = 0; n < 0x80; n += 2) {
	    k += (random() & 1) + 1;
	    if (k >= 3) k = k - 3;
	    poke_VRAM(((i + 11) << 7) + n, (i + 17) + (k << 3));
	}
    }
}

static void draw_sand(void) {
    for (u16 i = 0; i < 9; i++) {
	fill_VRAM(0x80 * (i + 19), TILE(0, 12), 0x040);
    }
}

static const u16 sea_palette[][4] = {
    { 0x0aa6, 0x0aa4, 0x0aa2, 0x0cca },
    { 0x0aa4, 0x0aa2, 0x0aa6, 0x06ac },
    { 0x0aa2, 0x0aa6, 0x0aa4, 0x08ce },
};

static void sea_rotate(u16 i) {
    update_palette(sea_palette[i], 4, ARRAY_SIZE(sea_palette[i]));
    upload_palette(0);

    callback(&sea_rotate, 12, i < 2 ? i + 1 : 0);
}

void display_nippon(Function prepare_level) {
    set_seed(1877);

    update_palette(beach_palette, 0, ARRAY_SIZE(beach_palette));
    update_tiles(beach_tiles, 1, ARRAY_SIZE(beach_tiles));

    /* load tiles */
    load_soldier_tiles(2);
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    draw_sea();
    draw_sand();

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
