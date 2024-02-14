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
    paint_background(44, 6, 8, 2, 69, 6);

    trail_could(10, 8, 5, 8);
    trail_could(35, 8, 7, 9);
    trail_could(30, 7, 1, 1);
    trail_could(60, 7, 1, 1);
}

static void draw_sea(void) {
    static const byte tiles[] = {
	17, 18, 19, 20, 21, 22, 23, 24,		/* sea */
	41, 42, 42, 43, 43, 43, 44, 44, 44	/* sand */
    };
    u16 i, k = 0;
    for (i = 0; i < 17; i++) {
	for (u16 n = 0; n < 0x80; n += 2) {
	    k += (random() & 1) + 1;
	    if (k >= 3) k = k - 3;
	    poke_VRAM(((i + 11) << 7) + n, tiles[i] + (k << 3));
	}
    }
}

static u16 flip(u16 tile, u16 condition) {
    return condition ? tile | BIT(11) : tile;
}

static void decoration(byte x, byte y, byte type) {
    switch(type) {
    case 0:
    case 1:
	paint_background(x, y, 2 + type, 2, type == 0 ? 53 : 47, 6);
	break;
    case 2:
    case 3:
	paint_background(x, y, 1, 2, flip(45, type == 3), 6);
	break;
    default:
	paint_background(x, y, 1, 1, flip(15 + (type & 1), type >= 6), 0);
	break;
    }
}

static void draw_bones(void) {
    u16 tile = 12, offset = 0x80 * 20;
    while (offset < 0x80 * 21) {
	u16 y = random() & 1;
	poke_VRAM(offset + y * 0x80, TILE(0, tile));
	tile = (tile == 14) ? 12 : tile + 1;
	offset += 2 * (3 + (random() & 3));
    }

    static const byte data[] = {
	 5, 25, 2, 15, 24, 0, 17, 24, 1, 27, 25, 2, 35, 23, 0,
	37, 23, 2, 34, 23, 3, 46, 25, 3, 55, 24, 2, 56, 24, 1,
	59, 24, 3, 10, 24, 5, 24, 23, 4, 31, 22, 5, 41, 24, 6,
	42, 24, 7, 49, 24, 6, 52, 23, 4, 64, 22, 7,
    };
    u16 i = 0;
    while (i < ARRAY_SIZE(data)) {
	const byte *ptr = data + i;
	decoration(ptr[0], ptr[1], ptr[2]);
	i += 3;
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

static void display_nippon(Function prepare_level) {
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
    draw_bones();

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
