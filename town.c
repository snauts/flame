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

static u16 draw_middle_object(u16 x, u16 id) {
    u16 dx = 1, dy = 1;
    switch (id) {
    case 99:
	id = BIT(11) | 33;
	break;
    case 55:
    case 73:
    case 75:
	dx = 2;
	/* falls through */
    case 71:
	dy = 2;
	break;
    case 52:
	dx = 2;
	/* falls through */
    case 68:
	dy = 3;
	break;
    case 49:
	dx = dy = 3;
	break;
    }
    paint_background(x, 16 - dy, dx, dy, id, 8 - dy);
    return dx;
}

static void draw_middle_houses(void) {
    u16 x = 0;
    static const byte houses[] = {
	68, 33, 41, 99, 71, 33, 41,
	99, 73, 75, 33, 41, 41, 99,
	52, 68, 52, 75, 68, 52, 68,
	71, 55, 71, 68, 71, 49, 71,
	33, 41, 41, 99, 71, 49, 71,
	75, 55, 75, 71, 52, 49, 68,
	73, 71, 75,
    };
    for (u16 i = 0; i < ARRAY_SIZE(houses); i++) {
	x += draw_middle_object(x, houses[i]);
    }
    fill_VRAM(0x80 * 16, TILE(0, 25), 0x140);
}

static void draw_bottom_houses(void) {
    for (u16 i = 0; i < 7; i++) {
	fill_VRAM(0x80 * (21 + i), BIT(12) | TILE(0, 7 - i), 0x40);
    }
}

static void draw_houses(void) {
    static const byte horizon[] = {
	20, 28, 36, 44, 45, 37, 37, 47, 38, 46, 37, 38, 46, 37, 38, 46,
	 1, 38, 46, 47, 35, 36, 44, 45, 37, 20, 28, 47, 47, 35,  2,  0,
	43, 47, 47, 35, 25, 33, 25, 41, 33, 41, 25, 33, 41, 39, 25, 41,
	33, 47, 35, 34, 42, 47, 35, 41, 33, 25, 36, 44, 37, 20, 28, 45,
    };
    for (u16 i = 0; i < ARRAY_SIZE(horizon); i++) {
	byte tile = horizon[i];
	switch (tile) {
	case 0:
	    /* empty */
	    break;
	case 1:
	    paint_background(i, 10, 1, 2, 27, 0);
	    break;
	case 2:
	    paint_background(i, 9, 2, 3, 22, 5);
	    break;
	default:
	    poke_VRAM(0x80 * 11 + (i << 1), TILE(0, 1 + tile));
	    break;
	}
    }
    fill_VRAM(0x80 * 12, TILE(0, 1), 0x100);
    draw_middle_houses();
    draw_bottom_houses();
}

static u16 *scroll_buf = NULL;
static void update_town(void) {
    if (!update_scroll(0)) {
	u16 half = -(window >> 1);
	u16 third = -(window / 3);
	for (u16 i = 0; i < 28; i++) {
	    u16 back = i <= 11 ? third : (i <= 15 ? half : (third << 1));
	    scroll_buf[(i << 4) + 0] = -window;
	    scroll_buf[(i << 4) + 1] = back;
	}
    }
    copy_to_VRAM_ptr(VRAM_SCROLL_A, 0x380, scroll_buf);
}

static void display_french(Function prepare_level) {
    scroll_buf = malloc(0x380);

    /* load tiles */
    update_palette(town_palette, 0, ARRAY_SIZE(town_palette));
    update_tiles(town_tiles, 1, ARRAY_SIZE(town_tiles));

    load_soldier_tiles(3);
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    draw_houses();

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
    switch_scroll(&update_town, 0x02);
}

void display_town(void) {
    void prepare_town_level(void);
    display_french(&prepare_town_level);
}
