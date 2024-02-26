#include "main.h"
#include "images/crab.h"
#include "images/spit.h"
#include "images/beach.h"
#include "images/dunes.h"

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

static const u16 sea_palette[][5] = {
    { 0x0aa6, 0x0aa2, 0x0aa4, 0x0ccc, 0x0aa8 },
    { 0x0aa2, 0x0aa4, 0x0aa6, 0x06ac, 0x0aa4 },
    { 0x0aa4, 0x0aa6, 0x0aa2, 0x08ce, 0x0ccc },
};

static void sea_rotate(u16 i) {
    extern byte total_dimming;
    if (total_dimming == 0) {
	update_palette(sea_palette[i], 4, ARRAY_SIZE(sea_palette[i]));
	upload_palette(0);
    }
    callback(&sea_rotate, 12, i < 2 ? i + 1 : 0);
}

typedef struct Crab {
    char (*throw)(Object *obj);
    Object *self;
    Object *spit;
    char counter;
    byte force;
    byte rate;
    byte hold;
    u16 pA, pB;
} Crab;

Crab *c_obj;

#define CRAB(obj) ((Crab *) (obj->private))

static void spit_cleanup(Object *obj) {
    Crab *crab = CRAB(obj);
    if (crab->spit != NULL) {
	kill_mob(crab->spit);
	crab->spit = NULL;
    }
}

static void move_crab(Object *obj) {
    u16 palette = 2;
    Sprite *sprite = obj->sprite;

    obj->x += obj->direction;
    advance_obj(obj, 8, 12);

    if (mob_move(obj, 14)) {
	if (obj->direction != 0) {
	    obj->frame = ((obj->life >> 2) % 6);
	}
	palette = 3;
    }
    else {
	spit_cleanup(obj);
    }

    sprite->cfg = TILE(palette, 257 + 4 * obj->frame);
    mob_adjust_sprite_dir(obj);
}

static Object *setup_crab(short x, short y) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(2, 2));
    mob_fn(obj, &move_crab);

    obj->private = c_obj + mob_index(obj);
    obj->flags |= O_PERSISTENT;
    obj->death = 6;

    return obj;
}

static inline u16 spittle_animation(u16 life, Object *parent) {
    return parent ? clamp(3, (life >> 2)) : 4 + ((life >> 2) & 3);
}

static char sentinel_throw(Object *obj) {
    Object *parent = obj->private;
    parent->frame = 2 - parent->frame;
    obj->direction = parent->frame != 0 ? -1 : 1;
    return 1;
}

static char left_throw(Object *obj) {
    obj->direction = -1;
    return 1;
}

static char edge_throw(Object *obj) {
    Object *parent = obj->private;
    Crab *crab = CRAB(parent);
    if (parent->x == crab->pA) {
	obj->direction = -1;
	return 1;
    }
    if (parent->x == crab->pB) {
	obj->direction = 1;
	return 1;
    }
    return 0;
}

struct Shoot {
    byte mx, my;
    char dir_x;
    char dir_y;
    byte len;
};

static const struct Shoot shoot[] = {
    { mx: 0x0, my: 0x7, dir_x: -1, dir_y:  1, len: 3 },
    { mx: 0x1, my: 0x7, dir_x: -1, dir_y:  1, len: 3 },
    { mx: 0x3, my: 0x7, dir_x: -1, dir_y:  1, len: 3 },
    { mx: 0x7, my: 0x7, dir_x: -1, dir_y:  1, len: 3 },
    { mx: 0x1, my: 0x7, dir_x:  1, dir_y:  1, len: 3 },
    { mx: 0x7, my: 0x0, dir_x: -1, dir_y:  1, len: 3 },
    { mx: 0x7, my: 0x1, dir_x: -1, dir_y: -1, len: 3 },

    { mx: 0xAA, my: 0xFF, dir_x: -1, dir_y: 1, len: 8 },
    { mx: 0x94, my: 0xFF, dir_x: -1, dir_y: 1, len: 8 },
    { mx: 0x11, my: 0xFF, dir_x: -1, dir_y: 1, len: 8 },
    { mx: 0x01, my: 0xFF, dir_x: -1, dir_y: 1, len: 8 },
    { mx: 0x00, my: 0xFF, dir_x:  1, dir_y: 1, len: 8 },
    { mx: 0x01, my: 0xFF, dir_x:  1, dir_y: 1, len: 8 },
    { mx: 0x11, my: 0xFF, dir_x:  1, dir_y: 1, len: 8 },
    { mx: 0x94, my: 0xFF, dir_x:  1, dir_y: 1, len: 8 },
    { mx: 0xAA, my: 0xFF, dir_x:  1, dir_y: 1, len: 8 },
};

static void shoot_move(Object *obj) {
    const struct Shoot *this = shoot + obj->direction;
    u16 dx = (this->mx >> obj->velocity) & 1;
    u16 dy = (this->my >> obj->velocity) & 1;
    obj->x = obj->x + dx * this->dir_x;
    obj->y = obj->y + dy * this->dir_y;
    if (++obj->velocity == this->len) {
	obj->velocity = 0;
    }
    if (obj->y > SCR_HEIGHT) {
	kill_mob_silently(obj);
    }
}

static void throw_spittle(Object *obj, Object *parent) {
    if (parent != NULL) {
	obj->x = parent->x + 4;
	obj->y = parent->y - 4;

	Crab *crab = CRAB(parent);
	if (obj->life >= crab->hold && crab->throw(obj)) {
	    obj->private = NULL;
	    crab->spit = NULL;
	}
    }
    else if (obj->flags & O_NO_GRAVITY) {
	shoot_move(obj);
    }
    else {
	obj->x += obj->direction;
	advance_y(obj, 12);
    }
}

static void move_spit(Object *obj) {
    Object *parent = obj->private;

    if (is_mob_alive(obj)) {
	throw_spittle(obj, parent);
    }

    if (mob_move(obj, 12)) {
	obj->frame = spittle_animation(obj->life, parent);
    }

    obj->sprite->cfg = TILE(3, 313 + obj->frame);
}

static Object *setup_spit(Object *parent, byte force) {
    Object *obj = setup_obj(0, 0, SPRITE_SIZE(1, 1));
    mob_fn(obj, &move_spit);
    obj->flags |= O_PROJECTILE | O_PERSISTENT;
    obj->private = parent;
    obj->velocity = force;
    obj->death = 8;
    return obj;
}

static void create_spit(u16 i) {
    Object *obj = get_mob(i);
    if (is_mob_alive(obj)) {
	Crab *crab = CRAB(obj);
	crab->spit = setup_spit(obj, crab->force);
    }
}

static void spit_crab(Object *obj) {
    Crab *crab = CRAB(obj);
    crab->counter++;
    if (is_mob_alive(obj) && crab->counter >= crab->rate) {
	if (crab->spit == NULL) {
	    callback(&create_spit, 0, mob_index(obj));
	}
	crab->counter = 0;
    }
    move_crab(obj);
}

static void sentinel_crab(Object *obj) {
    spit_crab(obj);
    if (obj->sprite->x == ON_SCREEN) {
	mob_fn(obj, &move_crab);
	obj->direction = 1;
	spit_cleanup(obj);
    }
}

static void patrol_crab(Object *obj) {
    Crab *crab = CRAB(obj);
    if (obj->x <= crab->pA) {
	obj->direction = 1;
    }
    else if (obj->x >= crab->pB) {
	obj->direction = -1;
    }
    spit_crab(obj);
}

static Crab *emit_spitter(u16 x, char dir, Operator updater) {
    Object *obj = setup_crab(x, get_top(x));
    Crab *crab = CRAB(obj);
    mob_fn(obj, updater);
    crab->spit = NULL;
    crab->self = obj;
    crab->hold = 24;
    obj->direction = dir;
    return crab;
}

void emit_sentinel(u16 x) {
    Crab *crab = emit_spitter(x, 0, &sentinel_crab);
    crab->throw = &sentinel_throw;
    crab->counter = 40;
    crab->force = 2;
    crab->rate = 48;
}

static Crab *emit_juggler(u16 x, byte counter) {
    Crab *crab = emit_spitter(x, 0, &sentinel_crab);
    crab->throw = &sentinel_throw;
    crab->counter = counter;
    crab->force = 3;
    crab->rate = 32;
    return crab;
}

void emit_twins(u16 x) {
    for (u16 i = 0; i < 2; i++) {
	Crab *crab = emit_juggler(x, i << 4);
	crab->self->frame = (i << 1);
	x = x + 32;
    }
}

static void emit_squad_member(u16 x, u16 i) {
    Crab *crab = emit_spitter(x, 0, &spit_crab);
    crab->throw = &left_throw;
    crab->counter = 40 - (i << 2);
    crab->force = 3;
    crab->rate = 96;
}

static void emit_patrol_crab(u16 x, char dir, u16 pA, u16 pB) {
    Crab *crab = emit_spitter(x, dir, &patrol_crab);
    crab->throw = &edge_throw;
    crab->counter = 48;
    crab->force = 3;
    crab->rate = 64;
    crab->pA = pA;
    crab->pB = pB;
}

void emit_stalk_patrol(u16 x1) {
    for (u16 i = 0; i < 2; i++) {
	u16 w = i * 24;
	u16 x2 = x1 + 192;
	emit_patrol_crab(x1 + w,  1, x1, x2);
	emit_patrol_crab(x2 - w, -1, x1, x2);
    }
}

void emit_crab_squad(u16 x) {
    for (u16 i = 0; i < 6; i++) {
	emit_squad_member(x, i);
	x += 18;
    }
}

void emit_dirty_trio(u16 x) {
    u16 x1 = x + 8, x2 = x1 + 64;
    emit_patrol_crab(x1 + 16,  1, x1, x2);
    emit_patrol_crab(x2 - 16, -1, x1, x2);
    emit_juggler(x1 + 32, 0);
}

static void drop_bear_drop(Object *obj) {
    if (get_snap(obj->x + 8, obj->y, obj->y)) {
	obj->direction = clamp(get_soldier()->x - obj->x, 1);
	mob_fn(obj, &move_crab);
    }
    move_crab(obj);
}

static void delay_drop(u16 i) {
    Object *obj = get_mob(i);
    mob_fn(obj, &drop_bear_drop);
    obj->y++;
}

static void delay_attack(u16 i) {
    CRAB(get_mob(i))->rate = 1;
    callback(&delay_drop, 80, i);
}

static void drop_bear_attack(Object *obj) {
    Crab *crab = CRAB(obj);
    if (crab->pA == window + SCR_WIDTH - 208) {
	u16 timeout = crab->pB > 2 ? (crab->pB - 3) : (2 - crab->pB);
	callback(&delay_attack, timeout << 2, mob_index(obj));
    }
    move_crab(obj);
}

static void drop_bear_prepare(Object *obj) {
    if (is_mob_alive(obj) && obj->x == window + SCR_WIDTH - 32) {
	Crab *crab = CRAB(obj);
	if (crab->spit == NULL) {
	    crab->spit = setup_spit(obj, crab->force);
	    mob_fn(obj, &drop_bear_attack);
	}
    }
    move_crab(obj);
}

static char drop_bear_throw(Object *obj) {
    Object *parent = obj->private;
    obj->direction = 2 * (1 - parent->frame);
    return CRAB(parent)->rate;
}

void emit_drop_bears(u16 x) {
    x += 64;
    u16 pos = x + 24;
    for (u16 i = 0; i < 6; i++) {
	Crab *crab = emit_spitter(x, 0, &drop_bear_prepare);
	crab->throw = &drop_bear_throw;
	crab->counter = 24;
	crab->force = 1;
	crab->rate = 0;
	crab->pA = pos;
	crab->pB = i;

	crab->self->frame = i > 2 ? 2 : 0;

	x += (i == 2 ? 64 : 20);
    }
}

static void falling_crab(Object *obj) {
    if (get_snap(obj->x + 8, obj->y, obj->y)) {
	if (obj->direction == 0) {
	    Crab *crab = CRAB(obj);
	    obj->direction = 2 * (crab->counter & 1) - 1;
	    crab->counter = crab->counter >> 1;
	}
    }
    else {
	obj->direction = 0;
    }
    move_crab(obj);
}

static byte crab_num;
static void emit_next_crab(u16 x) {
    static const char walk_mask[] = { 15, 0, 1, 14, 3, 12, 7, 8 };
    byte mask = walk_mask[crab_num];

    Crab *crab = emit_spitter(x + mask + 136, 0, &falling_crab);
    callback(&emit_next_crab, 28, x);
    crab->self->y = 0;
    crab_num = (crab_num + 1) & 7;
    crab->counter = mask;
}

void emit_falling_crabs(u16 x) {
    crab_num = 0;
    emit_next_crab(x);
}

static void gunner_crab(Object *obj) {
    spit_crab(obj);
    if (obj->sprite->x == ON_SCREEN) {
	mob_fn(obj, &move_crab);
	spit_cleanup(obj);
	kill_mob(obj);
    }
}

static const char five[] = { 5, 1, 2, 3, 4, 0 };
static const char cone[] = { 3, 1, 0, 4 };
static const char even[] = { 1, 5 };
static const char fast[] = { 3, -32, 0, 2 };
static const char saw[] = { 2, 1, 3 };
static const char ray[] = { 2, 5, 6 };

static const char rain[] = {
    18, 15, 14, 13, 12, 11, 10, 9, 8, 7, 7, 8, 9, 10, 11, 12, 13, 14, -40,
};

static const char pain[] = {
    18, 15, 14, 13, 12, 11, 10, 9, 8, 7, 8, 9, 10, 11, 12, 13, 14, -40, 15,
};

static const char *patterns[] = {
    five, cone, even, fast, saw, ray, rain, pain
};

static char gunner_throw(Object *obj) {
    obj->flags |= O_NO_GRAVITY;

    Object *parent = obj->private;
    parent->frame = 2 - parent->frame;

    Crab *crab = CRAB(parent);
    const char *data = patterns[crab->pA];

    do {
	obj->direction = data[crab->pB + 1];

	if (++crab->pB >= data[0]) {
	    crab->pB = 0; /* wrap */
	}

	if (obj->direction < 0) {
	    crab->counter = obj->direction;
	}
    } while (obj->direction < 0);

    return 1;
}

static Crab *create_gunner_crab(u16 x, u16 id, byte rate, char start) {
    Crab *crab = emit_spitter(x, 0, &gunner_crab);
    crab->throw = &gunner_throw;
    crab->counter = start;
    crab->rate = rate;
    crab->force = 0;
    crab->hold = 16;
    crab->pA = id;
    crab->pB = 0;
    return crab;
}

void emit_gunner(u16 x) {
    create_gunner_crab(x, 0, 12, 12);
}

void emit_marksman(u16 x) {
    create_gunner_crab(x, 1, 16, 12);
}

void emit_sniper(u16 x) {
    create_gunner_crab(x, 3, 16, 12);
    create_gunner_crab(x + 32, 2, 96, 84)->self->y = 0xd8;
}

void emit_crossfire(u16 x) {
    create_gunner_crab(x, 4, 48, 24);
    create_gunner_crab(x + 48, 5, 48, 48)->self->y = 0xd8;
}

void emit_grenadiers(u16 x) {
    create_gunner_crab(x, 1, 28, 28);
    create_gunner_crab(x + 56, 1, 28, -28);
}

void emit_highway(u16 x) {
    create_gunner_crab(x, 6, 16, 16);
    create_gunner_crab(x + 56, 7, 16, 16);
}

static void display_nippon(Function prepare_level) {
    set_seed(1877);

    update_palette(beach_palette, 0, ARRAY_SIZE(beach_palette));
    update_tiles(beach_tiles, 1, ARRAY_SIZE(beach_tiles));

    update_palette(dunes_palette, 16, ARRAY_SIZE(dunes_palette));
    update_tiles(dunes_tiles, 129, ARRAY_SIZE(dunes_tiles));

    update_palette(crab_palette, 48, ARRAY_SIZE(crab_palette));
    update_tiles(crab_tiles, 257, ARRAY_SIZE(crab_tiles));

    load_burn_tiles(281);

    update_tiles(spit_tiles, 313, ARRAY_SIZE(spit_tiles));

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

    fill_VRAM(0, 0, 0x800);
    prepare_level();
    fill_bottom_row();
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_battotai(void);
    music_battotai();

    callback(&fade_in, 0, 6);
    callback(&sea_rotate, 30, 0);
    switch_frame(&update_game);

    c_obj = malloc(sizeof(Crab) * MAX_MOBS);
}

void display_beach(void) {
    void prepare_beach_level(void);
    display_nippon(&prepare_beach_level);
}

void display_dunes(void) {
    void prepare_dunes_level(void);
    display_nippon(&prepare_dunes_level);
}
