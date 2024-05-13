#include "main.h"

#include "images/mud.h"
#include "images/rain.h"
#include "images/forest.h"
#include "images/mosquito.h"

#include "forest.inc"

static void sky_piece(u16 x, u16 y, u16 dx, u16 dy) {
    if (dy == 9) {
	static const byte middle_map[] = { 73, 75, 89, 91 };
	paint_background(x, y, 2, 2, middle_map[dx], 6);
    }
    else {
	paint_background(x, y, 2, 1, dx * 18 + dy + 1, 8);
    }
}

static const byte sky_layers[] = {
    0, 1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 5, 6, 7, 8, 9
};

static void fill_line(u16 y, u16 dy) {
    u16 i = y;
    if (dy < 6) set_seed(2077);
    for (u16 x = 0; x <= 64; x += 2) {
	sky_piece(x, y, i, dy);
	i = (i + (random() % 3) + 1) & 3;
    }
}

static void draw_sky(void) {
    for (u16 y = 0; y < ARRAY_SIZE(sky_layers); y++) {
	fill_line(y, sky_layers[y]);
    }
    fill_line(17, 7);
    fill_line(18, 8);
    fill_line(23, 7);
    fill_line(24, 8);
}

static void draw_forest(void) {
    u16 i = 0, x = 0;
    while (x < 64) {
	if (x == 62) i = 0;
	u16 w = (i & 2) + 2;
	static const byte bottom_map[] = { 105, 121, 77, 109 };
	paint_background(x, 19, w, 4, bottom_map[i], 4);
	i = (i + (random() % 3) + 1) & 3;
	x += w;
    }
    fill_VRAM(0xC80, 73, 0xC0);
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
	    case 15:
		shift = invert >> 1;
		break;
	    case 8:
		shift = third;
		break;
	    case 19:
		shift = third << 1;
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
    0x3, 0xaaa, 0xb, 0x462,
    0x2, 0x888, 0x7, 0x440,
    0x1, 0x666, 0xe, 0x220,
    0x3, 0xcca, 0xa, 0xaaa,
    0x2, 0xaa8, 0x9, 0x888,
    0x1, 0x886, 0x8, 0x666,
    0xa, 0xcca, 0xb, 0x242,
    0x9, 0xaa8, 0x7, 0x220,
    0x8, 0x886, 0xe, 0x440,
    0,
};

static const u16 *rain_ptr;
static void rain_palette_rotate(u16 i) {
    for (u16 i = 0; i < 2; i++) {
	u16 index = *(rain_ptr++);
	u16 color = *(rain_ptr++);
	update_color(index, color);
    }
    if (*rain_ptr == 0) rain_ptr = rain_colors;
    schedule(&rain_palette_rotate, 0);
}

typedef struct Mosquito {
    char v_dir;
} Mosquito;

static Mosquito *m_obj;

#define MOSQUITO(obj) ((Mosquito *) (obj->private))

static void move_mosquito(Object *obj) {
    u16 palette = 2;
    Sprite *sprite = obj->sprite;

    obj->x += obj->direction;
    obj->y += MOSQUITO(obj)->v_dir;

    if (mob_move(obj, 10)) {
	obj->frame = obj->life & 3;
	palette = 3;
    }

    sprite->cfg = TILE(palette, 265 + 4 * obj->frame);

    mob_adjust_sprite_dir(obj);
}

static Object *setup_mosquito(short x, short y) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(2, 2));
    mob_fn(obj, &move_mosquito);

    obj->private = m_obj + mob_index(obj);
    obj->flags |= O_PERSISTENT;
    obj->death = 4;

    MOSQUITO(obj)->v_dir = 0;

    return obj;
}

void emit_mosquito(u16 i) {
    setup_mosquito(soldier.x + 256, 160);
    schedule(&emit_mosquito, 50);
}

static void display_soviet(const Level *level) {
    load_soldier_tiles(4);

    load_tiles(&rain_img, 1);
    load_image(&mud_img, 137, 1);
    load_image(&forest_img, 73, 0);
    load_image(&mosquito_img, 265, 3);

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    draw_forest();
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    prepare_level(level);

    void music_katyusha(void);
    music_katyusha();

    init_scrolling(&update_forest);
    rain_ptr = rain_colors;
    rain_palette_rotate(0);

    m_obj = malloc(sizeof(Mosquito) * MAX_MOBS);
}

void display_forest(void) {
    display_soviet(&forest_level);
}
