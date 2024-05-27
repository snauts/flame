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
    u16 release;
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

static char move_mosquito_alive(Object *obj) {
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

    return is_alive;
}

static void move_mosquito(Object *obj) {
    move_mosquito_alive(obj);
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

static void move_bomber(Object *obj) {
    move_mosquito(obj);
    Mosquito *private = MOSQUITO(obj);
    if (private->drop == NULL) {
	obj->direction = -2;
	private->v_dir = 0;
    }
}

void emit_bombers(u16 x) {
    u16 pos = x & ~7;
    Object *obj = setup_gnat(window + 320, 64, 42 + (x & 7), 30);
    Mosquito *private = MOSQUITO(obj);
    mob_fn(obj, &move_bomber);
    if (x & 1) {
	obj->direction = -1;
	private->v_dir = 2;
    }
    else {
	obj->direction = -2;
	private->v_dir = 1;
    }

    if (soldier.x < pos - 32) {
	x = pos + ((x + 1) & 7);
	callback(&emit_bombers, 20, x);
    }
}

static void move_jerker(Object *obj) {
    move_mosquito(obj);
    if ((obj->life & 0xf) == 0) {
	Mosquito *private = MOSQUITO(obj);
	signed char dx = clamp(soldier.x - obj->x + 12, 1);
	signed char dy = clamp(soldier.y - obj->y - 20, 1);
	if (dx != obj->direction) {
	    obj->direction = dx;
	}
	if (dy != private->v_dir) {
	    private->v_dir = dy;
	}
	byte rnd = random() & 3;
	if (rnd < 2 && obj->x > window + 64 && obj->x < window + 256) {
	    obj->direction = -obj->direction;
	}
	if (rnd == 3) {
	    private->v_dir = -private->v_dir;
	}
    }
}

static void setup_jerker(short x, short y) {
    Object *obj = setup_mosquito(x, y);
    obj->direction = clamp(soldier.x - x, 1);
    mob_fn(obj, &move_jerker);

    Mosquito *private = MOSQUITO(obj);
    private->v_dir = 1;
}

void emit_jerkers(u16 x) {
    set_seed(1240);
    static const short pos[] = {
	-16, 64, -16, 64, 320, 64, 320, 128,
	64, -16, 128, -16, 192, -16, 256, -16
    };
    for (u16 i = 0; i < ARRAY_SIZE(pos); i += 2) {
	setup_jerker(window + pos[i], pos[i + 1]);
    }
}

static void move_diver(Object *obj) {
    move_mosquito(obj);
    byte i = obj->x & 7;
    Mosquito *private = MOSQUITO(obj);
    if (private->v_dir == -2 && obj->y <= 96 + (i << 3)) {
	if (i & 1) {
	    obj->direction = -2;
	    private->v_dir = 1;
	}
	else {
	    obj->direction = -1;
	    private->v_dir = 2;
	}
    }
}

void emit_divers(u16 x) {
    Object *obj = setup_mosquito(x, 200);
    mob_fn(obj, &move_diver);
    obj->direction = 0;

    Mosquito *private = MOSQUITO(obj);
    private->v_dir = -2;
    if (soldier.x < x - 64) {
	x = (x & ~7) + ((x + 1) & 7);
	callback(&emit_divers, 25, x);
    }
}

extern const char tiny_circle[128];
static void rotate_mosquito(Object *obj) {
    u16 index = obj->life << 1;
    obj->x -= tiny_circle[(index - 2) & 0x7f];
    obj->y -= tiny_circle[(index - 1) & 0x7f];
    obj->x += tiny_circle[(index + 0) & 0x7f];
    obj->y += tiny_circle[(index + 1) & 0x7f];

    Mosquito *private = MOSQUITO(obj);
    if (move_mosquito_alive(obj) && private->drop == NULL) {
	Object *drop = setup_projectile(0, 0, 40);
	drop->private = obj;
	drop->gravity = 8;

	private->release = obj->life + 32;
	private->drop = drop;
    }
}

static void setup_rotor(short x, short y, byte offset) {
    u16 index = offset << 1;
    Object *obj = setup_mosquito(x, y);
    mob_fn(obj, &rotate_mosquito);
    obj->x += tiny_circle[(index + 0) & 0x7f];
    obj->y += tiny_circle[(index + 1) & 0x7f];
    obj->direction = 0;
    obj->life = offset;
}

void emit_rotors(u16 x) {
    setup_rotor(x + 36, 140, 0);
    setup_rotor(x + 100, 132, 32);
    setup_rotor(x + 164, 148, 16);
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
