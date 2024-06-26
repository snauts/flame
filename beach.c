#include "main.h"
#include "images/crab.h"
#include "images/spit.h"
#include "images/beach.h"
#include "images/dunes.h"

#include "images/hermit_shell.h"
#include "images/hermit_eyes.h"
#include "images/hermit_legs.h"

#include "beach.inc"

u16 spit_tile;

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
    update_palette(sea_palette[i], 4, ARRAY_SIZE(sea_palette[i]));
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
	if (obj->direction != 0 && (obj->life & 3) == 0) {
	    obj->frame = obj->frame == 5 ? 0 : obj->frame + 1;
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

static inline u16 spittle_animation(u16 life, u16 is_growing) {
    return is_growing ? clamp(3, (life >> 2)) : 4 + ((life >> 2) & 3);
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

extern const Ray rays[];
static void shoot_move(Object *obj) {
    const struct Ray *this = rays + obj->direction;
    obj->x = obj->x + this->dx[obj->velocity];
    obj->y = obj->y + this->dy[obj->velocity];
    obj->velocity = (obj->velocity + 1) & 0x1f;
    if (obj->y > SCR_HEIGHT - (byte) obj->gravity) {
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

static void animate_spit(Object *obj, u16 is_growing) {
    if (mob_move(obj, 12)) {
	obj->frame = spittle_animation(obj->life, is_growing);
    }

    obj->sprite->cfg = spit_tile + obj->frame;
}

static void move_spit(Object *obj) {
    Object *parent = obj->private;

    if (is_mob_alive(obj)) {
	throw_spittle(obj, parent);
    }

    animate_spit(obj, parent != NULL);
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
	obj->direction = clamp(soldier.x - obj->x, 1);
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

static const char five[] = { 5, A(250), A(235), A(225), A(290), A(270) };
static const char cone[] = { 3, A(250), A(270), A(290) };
static const char fast[] = { 3,    -32, A(270), A(235) };
static const char saw[] =  { 2, A(250), A(225) };
static const char ray[] =  { 2, A(180), A(160) };
static const char even[] = { 1, A(180) };

static const char rain[] = {
    18,
    A(295), A(290), A(285), A(275), A(270), A(265),
    A(255), A(250), A(245), A(245), A(250), A(255),
    A(265), A(270), A(275), A(285), A(290),    -40
};

static const char pain[] = {
    18,
    A(295), A(290), A(285), A(275), A(270), A(265),
    A(255), A(250), A(245), A(250), A(255), A(265),
    A(270), A(275), A(285), A(290),    -40, A(295),
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
    create_gunner_crab(x, 1, 32, 32);
    create_gunner_crab(x + 56, 1, 32, -28);
}

void emit_highway(u16 x) {
    create_gunner_crab(x, 6, 16, 16);
    create_gunner_crab(x + 56, 7, 16, 16);
}

#define BURN_TILES 281

static void display_nippon(const Level *level) {
    set_seed(1877);

    load_image(&beach_img, 1, 0);
    load_image(&dunes_img, 129, 1);
    load_image(&crab_img, 257, 3);

    load_burn_tiles(BURN_TILES);

    load_tiles(&spit_img, 313);
    spit_tile = TILE(3, 313);

    /* load tiles */
    load_soldier_tiles(2);

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    draw_sea();
    draw_bones();

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    prepare_level(level);

    void music_battotai(void);
    music_battotai();

    callback(&sea_rotate, 30, 0);

    c_obj = malloc(sizeof(Crab) * MAX_MOBS);
}

void display_beach(void) {
    display_nippon(&beach_level);
}

void display_dunes(void) {
    display_nippon(&dunes_level);
}

static signed char projectile_dx, projectile_dy;
void set_projectile_offset(signed char dx, signed char dy) {
    projectile_dx = dx;
    projectile_dy = dy;
}

static void move_projectile(Object *obj) {
    u16 is_growing = (obj->life < 16);
    if (is_mob_alive(obj)) {
	Object *parent = obj->private;
	if (parent != NULL) {
	    obj->x = parent->x + projectile_dx;
	    obj->y = parent->y + projectile_dy;
	}
	else if (!is_growing) {
	    shoot_move(obj);
	}
    }

    animate_spit(obj, is_growing);
}

static char is_hermit_alive(void);

Object *setup_projectile(u16 x, u16 y, char pattern) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(1, 1));
    BUG(pattern >= 72, "BAD-PATTERN %d", pattern);
    mob_fn(obj, &move_projectile);
    obj->flags |= O_PROJECTILE;
    obj->direction = pattern;
    obj->death = 8;
    return obj;
}

static void setup_boss_spit(u16 x, u16 y, char pattern) {
    if (is_hermit_alive()) setup_projectile(x, y, pattern);
}

static Object **hermit;

#define HERMIT_PARTS	8

#define BASE		0
#define TIME		1
#define EYES		2
#define LEGS		3

#define HERMIT_HP	hermit[BASE]->life
#define HERMIT_STATE	hermit[BASE]->frame
#define HERMIT_TIME	hermit[TIME]->life

#define PANIC_BOUND	hermit[TIME]->x
#define HERMIT_RUNS	hermit[TIME]->y
#define HERMIT_INDEX	hermit[TIME]->gravity
#define HERMIT_JERKS	hermit[TIME]->velocity

#define WALK_L		BIT(0)
#define WALK_R		BIT(1)
#define ANGRY		BIT(2)
#define DO_ARC		BIT(3)

static const Layout right[HERMIT_PARTS] = {
    { x: 56, y:  0, size:SPRITE_SIZE(1, 4), tile:FLIP(3, 325) },
    { x: 56, y: 32, size:SPRITE_SIZE(1, 2), tile:FLIP(3, 357) },
    { x: 56, y: -2, size:SPRITE_SIZE(3, 4), tile:FLIP(3, 389) },
    { x: 53, y: 30, size:SPRITE_SIZE(4, 4), tile:FLIP(3, 437) },
    { x: 32, y:  0, size:SPRITE_SIZE(3, 4), tile:FLIP(3, 329) },
    { x: 32, y: 32, size:SPRITE_SIZE(3, 3), tile:FLIP(3, 361) },
    { x: 16, y: 16, size:SPRITE_SIZE(2, 2), tile:FLIP(3, 343) },
    { x:  0, y: 32, size:SPRITE_SIZE(4, 4), tile:FLIP(3, 373) },
};

static const Layout left[HERMIT_PARTS] = {
    { x:  0, y:  0, size:SPRITE_SIZE(1, 4), tile:TILE(3, 325) },
    { x:  0, y: 32, size:SPRITE_SIZE(1, 2), tile:TILE(3, 357) },
    { x:-15, y: -2, size:SPRITE_SIZE(3, 4), tile:TILE(3, 389) },
    { x:-20, y: 30, size:SPRITE_SIZE(4, 4), tile:TILE(3, 437) },
    { x:  8, y:  0, size:SPRITE_SIZE(3, 4), tile:TILE(3, 329) },
    { x:  8, y: 32, size:SPRITE_SIZE(3, 3), tile:TILE(3, 361) },
    { x: 32, y: 16, size:SPRITE_SIZE(2, 2), tile:TILE(3, 343) },
    { x: 32, y: 32, size:SPRITE_SIZE(4, 4), tile:TILE(3, 373) },
};

static char is_hermit_alive(void) {
    return HERMIT_HP > 0;
}

static void animate_part(Object *obj, u16 tile, u16 mask, u16 wrap, u16 inc) {
    if ((HERMIT_TIME & mask) == 0) {
	obj->frame = obj->frame >= wrap ? 0 : obj->frame + inc;
    }
    obj->sprite->cfg = tile + obj->frame;
}

static inline u16 is_state(u16 mask) {
    return HERMIT_STATE & mask;
}

static inline u16 is_staying(void) {
    return !is_state(WALK_L | WALK_R);
}

struct BossSpit {
    u16 x;
    byte count;
    byte ids[];
};

static const struct BossSpit spit_fan_L = {
    .x = 96, .count = 5, .ids = { A(90), A(65), A(45), A(25), A(0) },
};
static const struct BossSpit spit_fan_R = {
    .x = 342, .count = 5, .ids = { A(90), A(115), A(135), A(155), A(180) },
};
static const struct BossSpit *spit_data[] = {
    &spit_fan_L, &spit_fan_R,
};

static void hermit_start_walking(Object *obj) {
    HERMIT_STATE |= obj->direction < 0 ? WALK_L : WALK_R;
}

static char bar_thirds(u16 i) {
    return HERMIT_HP < ((BAR_HEALTH / 3) << (i - 1));
}

static char at_edge(Object *obj) {
    return obj->x < 128 || obj->x > 384;
}

static char lay_position(Object *obj) {
    return (bar_thirds(2) ? (obj->x % 48) : (obj->x & 0x3f)) == 0;
}

static void hermit_jump(u16 initiate) {
    Object *obj = hermit[BASE];
    if (initiate > 0) {
	obj->velocity = initiate;
	obj->gravity = 0;
    }

    advance_y(obj, 6);
    if (is_hermit_alive() && (obj->velocity > 0 || obj->y < 284)) {
	schedule(&hermit_jump, 0);
    }
    else {
	obj->y = 284; /* snap */
    }
}

static void spit_fan(u16 x);

static void hermit_idle(u16 x) {
    if (is_state(ANGRY)) {
	hermit_start_walking(hermit[BASE]);
	if (bar_thirds(1)) HERMIT_STATE |= DO_ARC;
    }
    else if (x == 0) {
	callback(&hermit_idle, 0, x);
    }
    else {
	spit_fan(x);
    }
}

static void spit_arc(u16 y) {
    static const byte type[] = {
	A(180), A(155), A(135), A(115), A(90), A(65), A(45), A(25), A(0)
    };
    static const byte height[] = {
	0, 4, 6, 8, 10, 8, 6, 4, 0
    };
    for (u16 i = 0; i < ARRAY_SIZE(type); i++) {
	setup_boss_spit(224 + (i - 4) * 8, y - height[i], type[i]);
    }
}

static void spit_fan(u16 x) {
    u16 delay = x >> 8, i = x & 0xf;
    const struct BossSpit *data = spit_data[(x >> 4) & 0xf];
    setup_boss_spit(data->x, 192 + (i << 2), data->ids[i]);
    if (i < data->count - 1) {
	callback(&spit_fan, delay, x + 1);
    }
    else {
	callback(&hermit_idle, 96, x & ~0xf);
    }
}

static void spit_storm(u16 i) {
    static const byte height[] = { 220, 190, 220, 205, 190, 205, 220, 190 };
    char dir = hermit[BASE]->direction > 0;
    u16 x = dir ? spit_fan_L.x : spit_fan_R.x;
    setup_boss_spit(x, height[i], dir ? A(0) : A(180));

    if (is_state(ANGRY)) {
	callback(&hermit_idle, 96, 0);
    }
    else {
	callback(&spit_storm, 64, (i + 1) & 7);
    }
}

static void arc_and_resume_walk(u16 x) {
    callback(&hermit_idle, 32, 0);
    play_sfx(SFX_PERISH);
    spit_arc(x);
}

static void hermit_stop(void) {
    HERMIT_STATE &= ~(WALK_R | WALK_L);
}

static void perform_jump_and_arc(void) {
    callback(&arc_and_resume_walk, 44, 220);
    hermit_jump(4);
    hermit_stop();
}

static char arc_position(Object *obj) {
    return obj->x == ((obj->direction < 0) ? 288 : 232);
}

static void produce_corner_pattern(char right) {
    if ((HERMIT_RUNS & 3) > (HERMIT_HP >> 6)) {
	spit_storm(0);
    }
    else {
	u16 delay = 4 + (HERMIT_HP >> 3);
	spit_fan((right ? 0x10 : 0x00) | (delay << 8));
    }
}

static void produce_spit_fan(Object *obj, char right, char stop) {
    if (stop) {
	produce_corner_pattern(right);
    }
    else if (is_state(DO_ARC)) {
	if (arc_position(obj)) perform_jump_and_arc();
    }
    else if (!at_edge(obj) && lay_position(obj)) {
	setup_boss_spit(obj->x - (right ? 0 : 72), 220, A(90));
    }
}

static void animate_legs(Object *part, u16 tile) {
    if (is_staying()) {
	static const byte stand[] = { 80, 96, 112, 96 };
	part->sprite->cfg = tile + stand[(HERMIT_TIME >> 3) & 3];
    }
    else {
	animate_part(part, tile, 3, 11 * 16, 16);
    }
}

static const Pos *get_sway(void) {
    static const Pos sway[] = {
	{ x: 0, y: 0 },
	{ x: 1, y: 0 },
	{ x: 1, y: 1 },
	{ x: 0, y: 1 },
    };
    return sway + ((HERMIT_TIME >> 3) & 3);
}

static const Layout *get_layout(Object *obj) {
    return obj->direction > 0 ? right : left;
}

static void hermit_animate(Object *obj) {
    const Pos *delta = get_sway();
    const Layout *layout = get_layout(obj);
    for (u16 i = 0; i < HERMIT_PARTS; i++) {
	Object *part = hermit[i];
	if (part->flags & O_ANIHILATED) {
	    continue;
	}
	Sprite *sprite = part->sprite;
	u16 tile = layout[i].tile;
	sprite->x = obj->x + layout[i].x;
	sprite->y = obj->y + layout[i].y;
	if (i == EYES) {
	    animate_part(part, tile, 7, 3 * 12, 12);
	}
	else if (i == LEGS) {
	    animate_legs(part, tile);
	}
	else {
	    sprite->x += delta->x;
	    sprite->y += delta->y;
	    sprite->cfg = tile;
	}
    }
}

static void remove_spit(Object *obj) {
    if (obj->flags & O_PROJECTILE) {
	kill_mob(obj);
    }
}

static void cycle_flip(Sprite *sprite) {
    u16 flip = (HERMIT_TIME & 3) << 11;
    sprite->cfg = (sprite->cfg & ~(3 << 11)) | flip;
}

static char is_part_off_screen(Sprite *sprite) {
    return sprite->y <= ON_SCREEN - 32
	|| sprite->x <= ON_SCREEN - 32
	|| sprite->x >= ON_SCREEN + SCR_WIDTH;
}

struct Explode { signed char id, dx, dy; };

static const struct Explode explode[] = {
    { .id = 2, .dx =  2, .dy = 4 },
    { .id = 0, .dx = -4, .dy = 2 },
    { .id = 4, .dx =  4, .dy = 2 },
    { .id = 6, .dx = -2, .dy = 4 },
    { .id = 3, .dx =  4, .dy = 2 },
    { .id = 1, .dx = -2, .dy = 4 },
    { .id = 5, .dx =  2, .dy = 4 },
    { .id = 7, .dx = -4, .dy = 2 },
};

static void hermit_dismember(Object *obj) {
    const struct Explode *rip = explode + HERMIT_INDEX;
    Object *this = hermit[rip->id];
    Sprite *sprite = this->sprite;

    if (!is_part_off_screen(sprite)) {
	this->flags |= O_ANIHILATED;
	sprite->x = sprite->x - rip->dx;
	sprite->y = sprite->y - rip->dy;
	cycle_flip(sprite);
    }
    else {
	HERMIT_INDEX++;
	if (HERMIT_INDEX == HERMIT_PARTS) {
	    schedule(&finish_level, 128);
	    fade_music(0);
	    free_mob(obj);
	}
    }
    u16 rem = HERMIT_TIME & 3;
    if (rem == 0) {
	obj->direction = -obj->direction;
    }
    else {
	obj->y ^= rem << 1;
    }
    hermit_animate(obj);
    HERMIT_TIME++;
}

static const u16 ash_palette[][14] = {
    { 0x0222,0x024a,0x0228,0x0226,0x082c,0x0a8c,0x062a,
      0x0688,0x0224,0x0222,0x0000,0x0466,0x08aa,0x0244 },
    { 0x0222,0x0448,0x0226,0x0224,0x084a,0x088a,0x0628,
      0x0668,0x0224,0x0222,0x0000,0x0446,0x088a,0x0224 },
    { 0x0222,0x0444,0x0222,0x0222,0x0888,0x0888,0x0666,
      0x0666,0x0222,0x0222,0x0000,0x0444,0x0888,0x0222 },
};

static void upload_ash_palette(u16 i) {
    update_palette(ash_palette[i], 49, ARRAY_SIZE(ash_palette[i]));
}

static void hermit_shell_burn(u16 i);

static void hermit_jerk(Object *obj) {
    if ((HERMIT_TIME & 0x1f) == 0) {
	obj->direction = -obj->direction;
	if (HERMIT_JERKS > 0) {
	    HERMIT_JERKS--;
	}
	else {
	    mob_fn(obj, &hermit_dismember);
	    upload_ash_palette(2);
	}
    }
    hermit_animate(obj);
    HERMIT_TIME += 2;
}

static void flip_panic_direction(Object *obj) {
    PANIC_BOUND += (PANIC_BOUND < 64 || obj->direction < 0) ? 32 : 0;
    obj->direction = -obj->direction;
}

static void hermit_panic_run(Object *obj) {
    obj->x += obj->direction;
    if (obj->direction < 0 && obj->x < 128 + PANIC_BOUND) {
	obj->x = 96 + PANIC_BOUND;
	flip_panic_direction(obj);
    }
    else if (obj->direction > 0 && obj->x > 384 - PANIC_BOUND) {
	obj->x = 352 - PANIC_BOUND;
	flip_panic_direction(obj);
    }
    if (PANIC_BOUND >= 128) {
	mob_fn(obj, &hermit_jerk);
	upload_ash_palette(1);
	HERMIT_TIME = 0;
	obj->x = 256;
    }
}

static void hermit_death(Object *obj) {
    hermit_panic_run(obj);
    hermit_animate(obj);
    HERMIT_TIME += 2;
}

static u16 current_burn(u16 i) {
    return clamp(HERMIT_INDEX + (i & 1), HERMIT_PARTS - 1);
}

static void hermit_shell_burn(u16 i) {
    extern Object **burns;
    Object *burn = burns[i];
    init_burn(burn);

    Object *obj;
    if (HERMIT_JERKS > 0) {
	obj = hermit[BASE];
	burn->x = (random() & 0x1f) - 20;
	burn->y = (random() & 0x1f);
    }
    else {
	obj = hermit[explode[current_burn(i)].id];
	short adj_x = 12 - ((obj->sprite->size << 0) & 12);
	short adj_y = 12 - ((obj->sprite->size << 2) & 12);
	burn->x = (random() & 0xf) - adj_x;
	burn->y = (random() & 0xf) - adj_y;
    }
    burn->direction = -obj->direction;
    burn->sprite->x = obj->sprite->x + burn->x;
    burn->sprite->y = obj->sprite->y + burn->y;

    callback(&hermit_shell_burn, 1, i >= 11 ? 0 : i + 1);
}

static void hermit_burn_sound(u16 x) {
    if (HERMIT_INDEX < HERMIT_PARTS) {
	u16 delay;
	if (HERMIT_JERKS == 0) {
	    delay = 12 - HERMIT_INDEX;
	}
	else if (HERMIT_JERKS < 3) {
	    delay = 13 + HERMIT_JERKS;
	}
	else {
	    delay = 24 - (PANIC_BOUND >> 4);
	}
	schedule(&hermit_burn_sound, delay);
	play_sfx(SFX_PERISH);
    }
}

static void hermit_final_burn(void) {
    free_burns();
    set_seed(1337);
    setup_burns(12, BURN_TILES);
    hermit_shell_burn(0);
    hermit_burn_sound(0);
}

static void hermit_dies(u16 x) {
    Object *obj = hermit[BASE];
    apply_to_all_mobs(&remove_spit);
    mob_fn(obj, &hermit_death);
    soldier_fist_pump();
    obj->direction <<= 1;
    upload_ash_palette(0);
    HERMIT_STATE = WALK_R | WALK_L;
    HERMIT_JERKS = 3;
    HERMIT_INDEX = 0;
    HERMIT_TIME = 0;
    PANIC_BOUND = 0;
    hermit_final_burn();
}

const Rectangle hL_box[] = {
    { x1: -4, y1:  4, x2: 8, y2: 64 },
    { x1:  8, y1: 16, x2:16, y2: 64 },
    { x1: 16, y1: 32, x2:48, y2: 64 },
};

const Rectangle hR_box[] = {
    { x1: 56, y1:  4, x2:68, y2: 64 },
    { x1: 48, y1: 16, x2:56, y2: 64 },
    { x1: 16, y1: 32, x2:48, y2: 64 },
};

static void hermit_hitbox(Object *obj) {
    u16 size = ARRAY_SIZE(hL_box);
    const Rectangle *box = obj->direction < 0 ? hL_box : hR_box;
    if (is_hermit_alive() && boss_hitbox(obj, box, size, size)) {
	if (HERMIT_HP == 0) {
	    schedule(&hermit_dies, 0);
	}
	else {
	    HERMIT_STATE |= ANGRY;
	}
    }
}

static void hermit_walk(Object *obj, u16 stop_condition) {
    char right = obj->direction > 0;
    obj->x += obj->direction;
    if (stop_condition) {
	obj->x -= right ? -32 : 32;
	obj->direction = -obj->direction;
	HERMIT_STATE = 0;
	HERMIT_RUNS++;
    }
    produce_spit_fan(obj, right, stop_condition);
}

static void hermit_action(Object *obj) {
    if (is_state(WALK_L)) {
	hermit_walk(obj, obj->x < 128);
    }
    else if (is_state(WALK_R)) {
	hermit_walk(obj, obj->x > 384);
    }
}

static void hermit_update(Object *obj) {
    hermit_action(obj);
    hermit_animate(obj);
    hermit_hitbox(obj);
    HERMIT_TIME++;
}

static void setup_hermit(u16 i) {
    hermit = malloc(HERMIT_PARTS * sizeof(Object*));
    for (u16 i = 0; i < HERMIT_PARTS; i++) {
	hermit[i] = alloc_mob();
	Sprite *sprite = hermit[i]->sprite;
	sprite->size = left[i].size;
	hermit[i]->frame = 0;
	hermit[i]->flags = 0;
    }

    setup_burns(4, BURN_TILES);

    hermit[BASE]->x = 416;
    hermit[BASE]->y = 284;
    hermit[BASE]->direction = -1;

    HERMIT_RUNS = 0;
    HERMIT_TIME = 0;
    HERMIT_STATE = 0;
    HERMIT_HP = BAR_HEALTH;
    mob_fn(hermit[BASE], &hermit_update);
    hermit_idle(0);
}

void display_hermit(void) {
    display_nippon(&hermit_level);

    load_image(&hermit_shell_img, 325, 3);
    load_tiles(&hermit_eyes_img, 389);
    load_tiles(&hermit_legs_img, 437);

    display_progress_bar();
    lock_screen(1);

    schedule(&setup_hermit, 0);
}
