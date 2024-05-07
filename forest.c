#include "main.h"

#include "images/rain.h"

#include "forest.inc"

static void sky_piece(u16 x, u16 y, u16 dx, u16 dy) {
    paint_background(x, y, 2, 1, dx * 16 + dy + 1, 7);
}

static const byte sky_layers[] = {
    0, 1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 5, 6, 7
};

static void draw_sky(void) {
    for (u16 y = 0; y < ARRAY_SIZE(sky_layers); y++) {
	u16 i = y;
	set_seed(2077);
	for (u16 x = 0; x <= 64; x += 2) {
	    sky_piece(x, y, i, sky_layers[y]);
	    i = (i + (random() % 3) + 1) & 3;
	}
    }
}

static void update_forest(void) {
    if (update_frame()) {
	u16 *ptr = scroll_buf;
	u16 invert = -window;
	u16 third = -(window / 3);
	u16 shift = third << 1;
	for (u16 row = 0; row < 28; row++) {
	    switch (row) {
	    case 4:
		shift = invert >> 1;
		break;
	    case 8:
		shift = third;
		break;
	    }
	    ptr[0] = invert;
	    ptr[1] = shift;
	    ptr += 16;
	}
	update_scroll_buffer();
    }
}

static const u16 rain_colors[] = {
    1, 0x3, 0xaaa,
    1, 0x2, 0x888,
    1, 0x1, 0x666,
    2, 0x3, 0xcca, 0xa, 0xaaa,
    2, 0x2, 0xaa8, 0x9, 0x888,
    2, 0x1, 0x886, 0x8, 0x666,
    1, 0xa, 0xcca,
    1, 0x9, 0xaa8,
    1, 0x8, 0x886,
    0,
};

static const u16 *rain_ptr;
static void rain_palette_rotate(u16 i) {
    u16 count = *(rain_ptr++);
    for (u16 i = 0; i < count; i++) {
	u16 index = *(rain_ptr++);
	u16 color = *(rain_ptr++);
	update_color(index, color);
    }
    if (*rain_ptr == 0) rain_ptr = rain_colors;
    schedule(&rain_palette_rotate, 0);
}

static void display_soviet(const Level *level) {
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

    init_scrolling(&update_forest);
    rain_ptr = rain_colors;
    rain_palette_rotate(0);
}

void display_forest(void) {
    display_soviet(&forest_level);
}
