#include "main.h"

#include "images/mud.h"
#include "images/rain.h"
#include "images/forest.h"
#include "images/mosquito.h"

#include "forest.inc"

#define SKY	1
#define TREE	(SKY + 72)
#define MUD	(TREE + 64)
#define GNAT	(MUD + 128)
#define BURN	(GNAT + 16)
#define DROP	(BURN + 32)

static void sky_piece(u16 x, u16 y, u16 dx, u16 dy) {
    if (dy == 9) {
	static const byte middle_map[] = { 0, 2, 16, 18 };
	paint_background(x, y, 2, 2, TREE + middle_map[dx], 6);
    }
    else {
	paint_background(x, y, 2, 1, SKY + dx * 18 + dy, 8);
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
	static const byte bottom_map[] = { 32, 48, 4, 36 };
	paint_background(x, 19, w, 4, TREE + bottom_map[i], 4);
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
    Object *drop;
    byte release;
    char v_dir;
} Mosquito;

static Mosquito *m_obj;

#define MOSQUITO(obj) ((Mosquito *) (obj->private))

static void release_drop(Mosquito *private) {
    Object *drop = private->drop;
    if (drop != NULL) {
	drop->private = NULL;
	private->drop = NULL;
    }
}

static void move_mosquito(Object *obj) {
    u16 palette = 2;
    Sprite *sprite = obj->sprite;
    Mosquito *private = MOSQUITO(obj);

    obj->x += obj->direction;
    obj->y += MOSQUITO(obj)->v_dir;

    char is_alive = mob_move(obj, 10);

    if (!is_alive || obj->life == private->release) {
	release_drop(private);
    }

    if (is_alive) {
	obj->frame = obj->life & 3;
	palette = 3;
    }

    sprite->cfg = TILE(palette, GNAT + 4 * obj->frame);

    mob_adjust_sprite_dir(obj);
}

static Object *setup_mosquito(short x, short y) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(2, 2));
    mob_fn(obj, &move_mosquito);

    obj->private = m_obj + mob_index(obj);
    obj->flags |= O_PERSISTENT;
    obj->death = 4;

    Mosquito *private = MOSQUITO(obj);
    private->drop = NULL;
    private->v_dir = 0;

    return obj;
}

static Object *setup_gnat(short x, short y, char angle, byte release) {
    Object *drop = setup_projectile(0, 0, angle);
    Object *gnat = setup_mosquito(x, y);
    drop->private = gnat;
    drop->gravity = 8;

    Mosquito *private = MOSQUITO(gnat);
    private->release = release;
    private->drop = drop;

    return gnat;
}

void emit_mosquito(u16 i) {
    Object *obj = setup_mosquito(window + 320, 96);
    MOSQUITO(obj)->v_dir = 1;

    setup_gnat(window + 320, 80, 45, 50);
}

static void move_emerger(Object *obj) {
    move_mosquito(obj);
    if (obj->life >= obj->gravity) {
	Mosquito *private = MOSQUITO(obj);
	obj->direction = -3;
	private->v_dir = 0;
    }
}

void hole_emergers(u16 x) {
    static const byte height[] = { 28, 24, 32, 20 };
    Object *obj = setup_mosquito(x - 22, 240);
    Mosquito *private = MOSQUITO(obj);
    mob_fn(obj, &move_emerger);
    obj->gravity = height[x & 3];
    obj->direction = 0;
    private->v_dir = -2;
    if (x - soldier.x > 96) {
	x = (x & ~3) + ((x + 1) & 3);
	callback(&hole_emergers, 20, x);
    }
}

extern const Image spit_img;
extern u16 spit_tile;

static void display_soviet(const Level *level) {
    load_soldier_tiles(4);

    load_tiles(&rain_img, SKY);
    load_image(&mud_img, MUD, 1);
    load_image(&forest_img, TREE, 0);
    load_image(&mosquito_img, GNAT, 3);
    load_tiles(&spit_img, DROP);
    spit_tile = TILE(3, DROP);
    load_burn_tiles(BURN);

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

    set_projectile_offset(-2, 12);
}

void display_forest(void) {
    display_soviet(&forest_level);
}
