#include "main.h"

#include "images/rat.h"
#include "images/town.h"
#include "images/street.h"
#include "images/crown.h"

#include "images/king_head.h"
#include "images/king_body.h"
#include "images/king_legs.h"

#include "town.inc"

#define RAT_TILES	257
#define BURN_TILES	(RAT_TILES + 9 * 4)
#define SLIME_TILES	(BURN_TILES + 8 * 4)
#define CROWN_TILES	(SLIME_TILES + 12)
#define KING_TILES	(CROWN_TILES + 2)
#define LEGS_TILES	(KING_TILES + 3 * 16)
#define BODY_TILES	(LEGS_TILES + 16)

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

static u16 draw_bottom_object(u16 x, u16 id) {
    u16 dx = 1, dy = 1;
    switch (id) {
    case 77:
	dx = 2;
	dy = 4;
	break;
    case 113:
	dx = 2;
	/* falls through */
    case 94:
	dy = 3;
	break;
    case 89:
	dx = dy = 3;
	break;
    case 102:
	dx = 4;
	dy = 3;
	break;
    case 200:
	id = 100 | BIT(11);
	break;
    case 116:
	dx = 2;
	/* falls through */
    case 108:
	dy = 2;
	break;
    }
    paint_background(x, 21 - dy, dx, dy, id, 8 - dy);
    return dx;
}

static void draw_bottom_houses(void) {
    u16 x = 0;
    static const byte houses[] = {
	116, 108,  77, 108, 113,  77, 108,  77,
	 94,  77, 113,  77,  94,  93,  92,  94,
	 89,  77, 108,  89,  94, 113,  93,  92,
	102,  92,  77,  94, 100, 101, 101, 200,
	 93,  92, 108, 100, 101, 101, 200,  94,
	116,  77, 113
    };
    for (u16 i = 0; i < ARRAY_SIZE(houses); i++) {
	x += draw_bottom_object(x, houses[i]);
    }
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

u16 *scroll_buf;

void update_scroll_buffer(void) {
    copy_to_VRAM_ptr(VRAM_SCROLL_A, 0x380, scroll_buf);
}

void init_scrolling(Function update) {
    scroll_type(0x02);
    scroll_buf = malloc(0x380);
    switch_frame(update);
}

static void update_town(void) {
    if (update_frame()) {
	u16 *ptr = scroll_buf;
	u16 invert = -window;
	u16 third = -(window / 3);
	u16 shift = third;
	for (u16 row = 0; row < 28; row++) {
	    switch (row) {
	    case 12:
		shift = invert >> 1;
		break;
	    case 16:
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

typedef struct Rat {
    Object *self;
    u16 was_ground;
    u16 cookie;
    Callback fn;
} Rat;

Rat *r_obj;

static u16 rat_counter;
static u16 dead_rats;

#define RAT(obj) ((Rat *) (obj->private))

extern const Image spit_img;

static void display_french(const Level *level) {
    /* load tiles */
    extern u16 spit_tile;
    load_image(&town_img, 1, 0);
    load_image(&street_img, 129, 1);
    load_image(&rat_img, RAT_TILES, 3);
    load_tiles(&spit_img, SLIME_TILES);
    spit_tile = TILE(0, SLIME_TILES);

    load_burn_tiles(BURN_TILES);

    load_soldier_tiles(3);

    /* background */
    fill_VRAM(0, 0, 0x800);
    draw_sky();
    draw_houses();

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    prepare_level(level);

    void music_onions(void);
    music_onions();

    init_scrolling(&update_town);

    r_obj = malloc(sizeof(Rat) * MAX_MOBS);
    rat_counter = 0;
    dead_rats = 0;
}

static char rat_diff(Object *obj) {
    return soldier.x - obj->x + 16 > 0 ? 1 : -1;
}

static char is_rat_at_screen_edge(Object *obj) {
    return obj->direction == -1 && obj->sprite->x < ON_SCREEN - 8;
}

static u16 live_rats(void) {
    return rat_counter - dead_rats;
}

static u16 advance_rat(Object *obj) {
    obj->x += obj->direction;
    if ((obj->life & 7) == 0) {
	obj->x += obj->direction;
    }
    return advance_obj(obj, 8, 8);
}

static void rat_change_direction(Object *obj, u16 old_x, char diff) {
    obj->x = old_x;
    obj->direction = diff;
    if (soldier.y > obj->y) {
	obj->y++;
    }
}

static void rat_at_platform_edge(Object *obj, u16 old_x) {
    char diff = rat_diff(obj);
    if (diff != obj->direction) {
	rat_change_direction(obj, old_x, diff);
    }
    else {
	obj->velocity = 2;
    }
}

static void rat_in_air(Object *obj, u16 old_x) {
    obj->frame = 3;
    if (RAT(obj)->was_ground) {
	rat_at_platform_edge(obj, old_x);
    }
}

static void animate_rat_run(Object *obj) {
    if ((obj->life & 3) == 0) {
	obj->frame = obj->frame == 8 ? 3 : obj->frame + 1;
	if (is_rat_at_screen_edge(obj) && live_rats() < 4) {
	    obj->direction = 1;
	}
    }
}

static void rat_death_follow_up(Object *obj) {
    Rat *rat = RAT(obj);
    if (rat->fn != NULL) {
	rat->fn(rat->cookie);
	rat->fn = NULL;
    }
    if (obj->place == -1) {
	dead_rats++;
    }
}

static void move_rat(Object *obj) {
    u16 land = 0, palette = 2;
    Sprite *sprite = obj->sprite;
    char emerged = (obj->frame >= 3);
    short old_x = obj->x;

    if (emerged) land = advance_rat(obj);

    if (mob_move(obj, 17)) {
	if (emerged && !land) {
	    rat_in_air(obj, old_x);
	}
	else {
	    animate_rat_run(obj);
	}
	palette = 3;
    }
    else {
	rat_death_follow_up(obj);
    }

    sprite->cfg = TILE(palette, RAT_TILES + 4 * obj->frame);
    RAT(obj)->was_ground = land;
    mob_adjust_sprite_dir(obj);
}

static Rat *setup_rat(short x, short y) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(2, 2));
    Rat *rat = r_obj + mob_index(obj);
    mob_fn(obj, &move_rat);

    obj->death = 9;
    obj->direction = rat_diff(obj);
    obj->flags |= O_PERSISTENT;
    obj->private = rat;
    rat->self = obj;
    rat->fn = NULL;

    rat_counter++;
    return rat;
}

static void follow_up_rat(u16 x) {
    setup_rat(x - 40, 115);
    setup_rat(x - 216, 179);
}

void emit_rat(u16 x) {
    Rat *rat = setup_rat(x - 40, 176);
    rat->fn = &follow_up_rat;
    rat->cookie = x;
}

void emit_house_block_rat(u16 x);

static void bottom_window_rat(u16 x) {
    setup_rat(x, 176);
}

static void same_rat(u16 x) {
    if (rat_counter & 2) {
	setup_rat(x - 180, 115);
    }
    else {
	callback(&emit_house_block_rat, 50, x + 36);
	bottom_window_rat(x - 212);
    }
}

void emit_house_block_rat(u16 x) {
    x = x - 36;
    if (soldier.x + 80 < x) {
	Rat *rat = setup_rat(x, 176);
	rat->fn = &same_rat;
	rat->cookie = x;
    }
}

static void top_window_rat(u16 x) {
    setup_rat(x, 48);
}

static void house_bravo_follow_up(u16 x) {
    callback(&top_window_rat, 50, x - 108);
    top_window_rat(x - 164);
}

void emit_house_block_rat_bravo(u16 x) {
    Rat *rat = setup_rat(x - 108, 112);
    rat->fn = house_bravo_follow_up;
    rat->cookie = x;
}

static void house_charlie_follow_up(u16 x) {
    callback(&top_window_rat, 25, x - 16);
    top_window_rat(x - 160);
}

static void house_charlie_front_follow_up(u16 x) {
#ifdef HARDER
    callback(&bottom_window_rat, 100, x - 206);
#endif
    top_window_rat(x - 16);
}

void emit_house_block_rat_charlie(u16 x) {
    Rat *rat = setup_rat(x - 216, 112);
    rat->fn = house_charlie_follow_up;
    rat->cookie = x;

    rat = setup_rat(x - 16, 176);
    rat->fn = house_charlie_front_follow_up;
    rat->cookie = x;
}

static void house_delta_follow_up(u16 x) {
    Rat *rat = setup_rat(x - 60, 112);
    rat->fn = top_window_rat;
    rat->cookie = x - 60;
}

void emit_house_block_rat_delta(u16 x) {
    Rat *rat = setup_rat(x - 60, 176);
    rat->fn = house_delta_follow_up;
    rat->cookie = x;

    rat = setup_rat(x - 252, 112);
    rat->fn = top_window_rat;
    rat->cookie = x - 252;
}

static void dumpster_wave(u16 x) {
    if ((x & 7) < 5) {
	setup_rat(x, 163);
	callback(&dumpster_wave, 15, x + 17);
    }
}

void emit_dumpster_wave(u16 x) {
    setup_rat(x - 268, 176);
    callback(&dumpster_wave, 50, x - 80);
}

static void reverse_wave(u16 x) {
    if ((x & 7) < 5) {
	setup_rat(x, 163);
	callback(&reverse_wave, 15, x - 15);
    }
}

void emit_reverse_wave(u16 x) {
    reverse_wave(x - 224);
    setup_rat(x - 52, 176);
#ifdef HARDER
    setup_rat(x - 72, 51);
#endif
}

static void emit_arch_window_wave(u16 x) {
    setup_rat(x - 40, 112);
    setup_rat(x - 104, 112);
}

void emit_arch_alpha_wave(u16 x) {
    emit_arch_window_wave(x);
#ifdef HARDER
    setup_rat(x - 72, 164);
#endif
}

void emit_arch_bravo_wave(u16 x) {
    emit_arch_window_wave(x);
#ifdef HARDER
    setup_rat(x - 168, 112);
#endif
    setup_rat(x - 232, 176);
}

static void arch_final(u16 x) {
    setup_rat(x - 120, 51);
#ifdef HARDER
    setup_rat(x - 120, 164);
#endif
}

static void arch_charlie_follow_up(u16 x) {
    setup_rat(x - 40, 176);
#ifdef HARDER
    setup_rat(x - 120, 51);
#endif
    callback(&arch_final, 100, x);
}

void emit_arch_charlie_wave(u16 x) {
    emit_arch_window_wave(x);
#ifdef HARDER
    setup_rat(x - 168, 112);
#endif
    callback(&arch_charlie_follow_up, 150, x);
}

static void pillar_back_stab(u16 x) {
#ifdef HARDER
    setup_rat(x - 252, 176);
#endif
}

void emit_pillar_alpha(u16 x) {
    setup_rat(x - 28, 163);
    setup_rat(x - 100, 179);
    callback(&pillar_back_stab, 30, x);
}

static void emit_back_striker(u16 x) {
    setup_rat(x - 188, 179);
}

static void emit_pillar_follow_up(u16 x) {
    callback(&emit_back_striker, 90, x);
}

void emit_pillar_bravo(u16 x) {
    Rat *rat = setup_rat(x - 28, 179);
    rat->fn = emit_pillar_follow_up;
    rat->cookie = x;
}

static void emit_pillar_middle(u16 x) {
#ifdef HARDER
    setup_rat(x - 230, 163);
#endif
}

void emit_bridge_alpha(u16 x) {
    setup_rat(x - 40, 131);
    setup_rat(x + 88, 204);
    callback(&emit_pillar_middle, 30, x);
}

void emit_bridge_beta(u16 x) {
    setup_rat(x - 40, 204);
    setup_rat(x - 60, 99);
}

static void emit_bridge_three(u16 x) {
    if ((x & 7) < 3) {
	short diff = soldier.x - x;
	callback(&emit_bridge_three, 10, diff > 0 ? x - 15 : x + 17);
	setup_rat(x & ~7, 99);
    }
}

void emit_bridge_gamma(u16 x) {
    callback(&emit_bridge_three, 75, x - 80);
    emit_bridge_three(x - 256);
}

void emit_bridge_kappa(u16 x) {
    emit_bridge_gamma(x);
    callback(&emit_bridge_three, 225, x - 80);
    callback(&emit_bridge_three, 150, x - 256);
}

static void town_last_rat(u16 x) {
    setup_rat(x - 28, 204);
}

void emit_bridge_delta(u16 x) {
    Rat *rat = setup_rat(x - 58, 163);
    rat->fn = town_last_rat;
    rat->cookie = x;
}

void display_town(void) {
    display_french(&town_level);
}

#define RAMP_OFFSET(n, i) (24 * 8 + (400 * (n)) + (12 * 8 * (i)) + 19)

static void ramp_pattern_group(const byte *data, byte n, byte size) {
    u16 i = 0;
    while (i < size) {
	u16 x = RAMP_OFFSET(n, data[i]);
	Object *obj = setup_projectile(x, data[i + 1],  data[i + 2]);
	obj->gravity = data[i + 3];
	i += 4;
    }
}

static void ramp_pattern_sandwich(u16 i) {
    setup_projectile(RAMP_OFFSET(0, i + 1), 83, A(245))->gravity = 30;
    setup_projectile(RAMP_OFFSET(0, i + 1), 83, A(285))->gravity = 30;
    callback(&ramp_pattern_sandwich, 24, i < 2 ? i + 1 : 0);
}

static void ramp_pattern_M_is_for_murder(u16 i) {
    static const byte data[] = {
	1,  51, A(245), 62, 1,  51, A(270), 62,
	1,  51, A(295), 62, 2, 187, A(65),   0,
	2, 187, A(115),  0, 3,  51, A(245), 62,
	3,  51, A(270), 62, 3,  51, A(295), 62,
    };
    ramp_pattern_group(data, 1, 32);
    callback(&ramp_pattern_M_is_for_murder, 80, 0);
}

static void ramp_pattern_W_is_for_walrus(u16 i) {
    if (i == 0) {
	static const byte data[] = {
	    1, 155, A(65), 0, 3, 155, A(115), 0, 2, 51, A(270), 94
	};
	ramp_pattern_group(data, 2, 12);
    }
    else {
	static const byte data[] = {
	    1, 155, A(115),  0, 3, 155,  A(65), 0,
	    2,  51, A(240), 94, 2,  51, A(300), 94
	};
	ramp_pattern_group(data, 2, 16);
    }
    callback(&ramp_pattern_W_is_for_walrus, 40, !i);
}

static void ramp_pattern_P_is_for_poison(u16 i) {
    static const byte data[] = {
	1, 51, A(245), 126, 3, 123, A(65),  0,
	1, 51, A(270), 126, 3, 123, A(90),  0,
	1, 51, A(295), 126, 3, 123, A(115), 0,
	2, 51, A(245), 126, 1, 123, A(65),  0,
	2, 51, A(270), 126, 1, 123, A(90),  0,
	2, 51, A(295), 126, 1, 123, A(115), 0,
	3, 51, A(245), 126, 2, 123, A(65),  0,
	3, 51, A(270), 126, 2, 123, A(90),  0,
	3, 51, A(295), 126, 2, 123, A(115), 0,
    };
    ramp_pattern_group(data + i, 3, 8);
    callback(&ramp_pattern_P_is_for_poison, 20, i < 64 ? i + 8 : 0);
}

static void ramp_pattern_D_is_for_daisies(u16 i) {
    static const byte data[] = {
	1, 35, A(0),   1, 91, A(90), 2, 91, A(90), 3, 91, A(90),
	3, 35, A(180), 1, 91, A(90), 2, 91, A(90), 3, 91, A(90),
    };
    setup_projectile(RAMP_OFFSET(4, data[i]), data[i + 1], data[i + 2]);
    callback(&ramp_pattern_D_is_for_daisies, 25, i >= 21 ? 0 : i + 3);
}

extern Callback generator;

#define RAMP_NR(i) (400 * (i))

void emit_ramp(u16 x) {
    if (generator) cancel_timer(generator);
    apply_to_all_mobs(&kill_mob_silently);
    switch (x) {
    case RAMP_NR(1):
	generator = &ramp_pattern_sandwich;
	break;
    case RAMP_NR(2):
	generator = &ramp_pattern_M_is_for_murder;
	break;
    case RAMP_NR(3):
	generator = &ramp_pattern_W_is_for_walrus;
	break;
    case RAMP_NR(4):
	generator = &ramp_pattern_P_is_for_poison;
	break;
    case RAMP_NR(5):
	generator = &ramp_pattern_D_is_for_daisies;
	break;
    default:
	error("EMIT_RAMP x:%d\n", x);
	break;
    }
    callback(generator, 16, 0);
}

void display_ramp(void) {
    display_french(&ramp_level);
    generator = NULL;
}

static void brick_fly(Object *obj) {
    if (obj->y > ON_SCREEN + SCR_HEIGHT) {
	free_mob(obj);
    }
    else {
	obj->sprite->x = obj->x;
	obj->sprite->y = obj->y;
	obj->x += obj->direction;
	advance_y(obj, 10);
	obj->sprite->cfg = TILE(1, 209 + obj->frame);
	if ((obj->life++ & 3) == 0) {
	    obj->frame = (obj->frame + obj->direction) & 3;
	}
    }
}

static Object *setup_brick(short x, short y, char dir, char vel) {
    Object *obj = setup_obj(x, y, SPRITE_SIZE(1, 1));
    mob_fn(obj, &brick_fly);
    obj->direction = dir;
    obj->velocity = vel;
    return obj;
}

static void put_tile(short x, short y, u16 tile) {
    UPDATE_VRAM_WORD(VRAM_PLANE_A + (y << 7)  + (x << 1), tile);
}

static void bottom_bricks(u16 i) {
    u16 x = 266 + i * 4;
    u16 force = 2 + ((i >> 1) & 1);
    u16 direction = (i & 1) ? -1 : 1;
    Object *obj = setup_brick(x, 208, direction, force);
    if (i < 5) callback(&bottom_bricks, 0, i + 1);
    obj->frame = i & 3;
}

static void window_damage(char type) {
    if (type == 0) {
	bottom_bricks(0);
	put_tile(25, 10, TILE(1, 213));
	put_tile(26, 10, TILE(0, 1));
	put_tile(27, 10, TILE(0, 1));
	put_tile(28, 10, TILE(1, 219));
    }
    else if (type < 0) {
	setup_brick(256, 186, -1, 1);
	setup_brick(256, 190, -1, 2)->frame = 2;
	put_tile(24, 7, TILE(1, 218));
	put_tile(24, 8, TILE(1, 220));
    }
    else if (type > 0) {
	setup_brick(296, 194,  1, 1);
	setup_brick(296, 198,  1, 2)->frame = 2;
	put_tile(29, 8, TILE(1, 217));
	put_tile(29, 9, TILE(1, 221));
    }
}

static Object **king;
static Object *crown;
static byte show_parts;

#define KING_PARTS	7

#define KING_HP		crown->life
#define KING_STATE	crown->flags

enum {
    K_WINDOW = 0,
    K_SPITING,
    K_BREAK_OUT,
    K_JUMPING,
    K_STANDING,
    K_RECOVER,
    K_WIGGLE,
};

static void king_set_state(u16 state) {
    KING_STATE = state;
}

static void king_head_frame(u16 frame) {
    Object *head = king[1];
    head->frame = frame;
    if (frame != 0) {
	frame = frame < 32 ? frame + 16 : 0;
	callback(&king_head_frame, 6, frame);
    }
}

static const byte L_arc[] = {  5, 40, A(225), A(215), A(200), A(180) };
static const byte R_arc[] = {  5, 40, A(315), A(325), A(340), A(0) };

static const byte L_saw[] = { 13, 15,
    A(240), A(245), A(250), A(255), A(265), A(270),
    A(275), A(285), A(290), A(295), A(300), A(305)
};
static const byte R_saw[] = { 13, 15,
    A(305), A(300), A(295), A(290), A(285), A(275),
    A(270), A(265), A(255), A(250), A(245), A(240),
};

static const byte *pattern;

static short spit_x(void) {
    return crown->x - (crown->direction > 0 ? 52 : 68);
}

static short spit_y(void) {
    return crown->y - 90;
}

static void king_projectile(short x, short y, char pattern) {
    if (KING_HP > 0) setup_projectile(x, y, pattern)->gravity = 6;
}

static void king_spits(u16 i) {
    king_projectile(spit_x(), spit_y(), pattern[i]);
    callback(&king_head_frame, 6, 16);
    if (i < pattern[0]) {
	callback(&king_spits, pattern[1], i + 1);
    }
    else {
	u16 next = crown->y == 176 ? K_WINDOW : K_STANDING;
	callback(&king_set_state, 30, next);
	pattern = NULL;
    }
}

static void select_window_pattern(Object *obj) {
    short three_quarters = KING_HP < BAR_HEALTH * 5 / 6;
    short soldier_middle = 128 < soldier.x && soldier.x < 256;
    if (three_quarters && soldier_middle) {
	pattern = obj->direction < 0 ? L_saw : R_saw;
    }
    else if (three_quarters || soldier.y < 152) {
	pattern = obj->direction < 0 ? L_arc : R_arc;
    }
}

static void start_spitting(u16 delay) {
    if (pattern != NULL) {
	callback(&king_spits, delay, 2);
	king_set_state(K_SPITING);
    }
}

static void king_do_jump(short x, short y, char vel, char flip, char pull) {
    Object *head = king[1];
    king_set_state(K_JUMPING);
    crown->velocity = vel;
    head->direction = flip;
    head->gravity = pull;
    head->x = x;
    head->y = y;
}

static void king_break_out(u16 stage) {
    if (stage <= 2) {
	set_mob_order(-1);
	play_sfx(SFX_PERISH);
	king_set_state(K_BREAK_OUT);
	callback(&king_break_out, 30, stage + 1);
	if (stage == 0) {
	    window_damage(crown->direction);
	    crown->x += crown->direction * 16;
	    show_parts += 1;
	}
	else if (stage == 1) {
	    window_damage(-crown->direction);
	    crown->y -= 16;
	    show_parts += 2;
	}
	else if (stage == 2) {
	    window_damage(0);
	    crown->y -= 7;
	    show_parts += 2;
	}
    }
    else {
	crown->gravity = 3;
	crown->direction *= 2;
	if (crown->direction < 0) {
	    king_do_jump(140, 204, 4, 1, 6);
	}
	else {
	    king_do_jump(420, 204, 4, 1, 7);
	}
	set_mob_order(1);
    }
}

static void king_in_window(Object *obj) {
    if (soldier.sprite->x < 260) {
	obj->direction = -1;
    }
    if (soldier.sprite->x > 276) {
	obj->direction = 1;
    }
    if (KING_HP < BAR_HEALTH * 2 / 3) {
	king_break_out(0);
	return;
    }
    select_window_pattern(obj);
    start_spitting(30);
}

static void king_flip(u16 i) {
    crown->direction = -crown->direction;
    crown->x += (crown->direction < 0) ? -32 : 32;
}

static char is_king_in_middle(Object *obj) {
    return obj->x == 256 || obj->x == 288;
}

static void bottom_spit(Object *obj, char pattern) {
    short x = obj->x - (obj->direction < 0 ? 32 : 88);
    king_projectile(x, obj->y - 56, pattern);
}

static void king_jumping(Object *obj) {
    Object *head = king[1];
    advance_y(obj, head->gravity);
    if (obj->y > head->y && obj->velocity < 0) {
	obj->y = head->y;
    }
    if (obj->x != head->x) {
	obj->x += obj->direction;
    }
    else if (obj->y == head->y) {
	king_set_state(K_RECOVER);
	callback(&king_set_state, 30, K_STANDING);
	if (head->direction) schedule(&king_flip, 15);
    }
}

static void leave_spit_trail(u16 x) {
    short n = (x & 0xff) - 2;
    if (n < 6) {
	bottom_spit(crown, A(270) + ((x & BIT(8)) ? -n : n));
	callback(&leave_spit_trail, 4 + n, x + 1);
    }
}

static void small_rat_help(void) {
    if (KING_HP < BAR_HEALTH / 5 && live_rats() < 1 && KING_HP > 0) {
	setup_rat(208, 88);
    }
}

static void king_next_jump(Object *obj) {
    switch (crown->x) {
    case 172:
	callback(&leave_spit_trail, 0, 0);
	king_do_jump(288, 220, 4, 0, 7);
	crown->gravity = 3;
	break;
    case 256:
	king_do_jump(140, 204, 4, 1, 8);
	crown->gravity = 4;
	small_rat_help();
	break;
    case 288:
	king_do_jump(420, 204, 4, 1, 8);
	crown->gravity = 7;
	small_rat_help();
	break;
    case 388:
	callback(&leave_spit_trail, 0, 0 | BIT(8));
	king_do_jump(256, 220, 4, 0, 7);
	crown->gravity = 7;
	break;
    }
}

static void king_shoot_arc(Object *obj) {
    static const byte right[] = { A(-30), A(0), A(30) };
    static const byte left[]  = { A(210), A(180), A(150) };
    const byte *ptr = (obj->direction > 0 ? left : right);
    for (u16 i = 0; i < ARRAY_SIZE(right); i++) {
	bottom_spit(obj, ptr[i]);
    }
}

static void king_wiggle(u16 i) {
    if (i == 0) {
	king_next_jump(crown);
    }
    else {
	king_shoot_arc(crown);
	schedule(&king_flip, 30);
	callback(&king_wiggle, 60, i - 1);
    }
}

static u16 sprite_w(Object *obj) {
    return ((obj->sprite->size & 0xc) + 4) * 2;
}

static u16 sprite_h(Object *obj) {
    return ((obj->sprite->size & 0x3) + 1) * 8;
}

static void blow_off_part(u16 i);
static void assign_burns_to_parts(u16 i);

static void move_fragment(Object *obj) {
    advance_y(obj, 8);
    obj->x += obj->direction;
    obj->sprite->x = obj->x;
    obj->sprite->y = obj->y;
    if (obj->y > ON_SCREEN + SCR_HEIGHT) {
	free_mob(obj);
    }
}

static void disintegrate_part(Object *obj) {
    u16 w = sprite_w(obj);
    u16 h = sprite_h(obj);
    u16 next = obj->frame + 1;
    u16 cfg = obj->sprite->cfg;
    assign_burns_to_parts(next);
    callback(&blow_off_part, 20, next);
    for (short x = w - 8; x >= 0; x -= 8) {
	for (u16 y = 0; y < h; y += 8) {
	    u16 rnd = random();
	    u16 frag_x = obj->sprite->x + x;
	    u16 frag_y = obj->sprite->y + y;
	    Object *frag = setup_obj(frag_x, frag_y, SPRITE_SIZE(1, 1));
	    frag->sprite->x = frag_x;
	    frag->sprite->y = frag_y;
	    frag->direction = (rnd & 2) - 1;
	    frag->velocity = 1 + (rnd & 1);
	    frag->gravity = (rnd >> 2) & 7;
	    mob_fn(frag, &move_fragment);
	    frag->sprite->cfg = cfg;
	    cfg = cfg + 1;
	}
    }
}

static void move_part(Object *obj) {
    u16 y = 336 - sprite_h(obj);

    if (obj->y < y) {
	advance_y(obj, 8);
	obj->x += obj->direction;
	obj->sprite->x = obj->x;
	obj->sprite->y = obj->y;
    }
    else {
	obj->y = y;
	mob_fn(obj, NULL);
	play_sfx(SFX_PERISH);
	disintegrate_part(obj);
	free_mob(obj);
    }
}

#define FINAL_BURNS 8
extern Object **burns;
static void king_burns(u16 i) {
    for (i = 0; i < FINAL_BURNS; i++) {
	Object *burn = burns[i];
	Sprite *sprite = burn->sprite;
	Object *parent = (Object *) burn->private;
	if (parent != NULL && burn->frame >= 8) {
	    u16 rnd = random();
	    burn->x = (rnd & 0x1f) - 8;
	    burn->y = sprite_h(burn->private) - 8;
	    sprite->x = parent->sprite->x + burn->x;
	    sprite->y = parent->sprite->y + burn->y;
	    burn->direction = (rnd & 16) - 8;
	    if (i < 2) play_sfx(SFX_PERISH);
	    init_burn(burn);
	}
	burn->y = burn->y - 1;
    }
    schedule(&king_burns, 0);
}

static void assign_burns_to_parts(u16 i) {
    for (u16 n = 0; n < FINAL_BURNS; n++) {
	Object *burn = burns[n];
	u16 index = i + (n >> 1);
	burn->private = index < KING_PARTS ? king[index] : NULL;
	if (burn->private != NULL) {
	    burn->frame = (((n & 1) ? 3 : 0) + n) & 7;
	}
	else {
	    hide_sprite(burn->sprite);
	    burn->frame = 8;
	}
    }
}

static void crown_lands(u16 i) {
    short y = soldier.sprite->y - 3;
    crown->sprite->y += 4;
    if (crown->sprite->y < y) {
	schedule(&crown_lands, 0);
    }
    else {
	schedule(&finish_level, 128);
	crown->sprite->y = y;
	fade_music(0);
    }
}

static void blow_off_part(u16 i) {
    if (i < KING_PARTS) {
	Object *part = king[i];
	part->frame = i;
	part->gravity = 0;
	part->velocity = 2;
	part->x = part->sprite->x;
	part->y = part->sprite->y;
	part->direction = (i & 1) ? 1 : -1;
	mob_fn(part, &move_part);
	play_sfx(SFX_PERISH);
    }
    else {
	u16 offset = soldier.direction > 0 ? 4 : 3;
	crown->sprite->x = soldier.sprite->x + offset;
	crown_lands(0);
    }
}

static void crown_flies_away(u16 i) {
    if (crown->sprite->y > 120) {
	crown->sprite->y -= 4;
	schedule(&crown_flies_away, 0);
    }
}

static void king_starts_to_burn(void) {
    free_burns();
    set_mob_order(-1);
    setup_burns(FINAL_BURNS, BURN_TILES);
    assign_burns_to_parts(1);
    king_burns(0);
}

static void king_death(Object *obj) {
    if (obj->direction < 0) king_flip(0);
    callback(&blow_off_part, 60, 1);
    schedule(&crown_flies_away, 30);
    mob_fn(crown, NULL);
}

static void king_standing(Object *obj) {
    if (is_king_in_middle(obj) && KING_HP < 2 * BAR_HEALTH / 5) {
	if (KING_HP > 0) {
	    king_set_state(K_WIGGLE);
	    king_wiggle(2);
	}
	else {
	    king_death(obj);
	}
    }
    else {
	king_next_jump(obj);
    }
}

static void king_action(Object *obj) {
    switch (KING_STATE) {
    case K_WINDOW:
	king_in_window(obj);
	break;
    case K_JUMPING:
	king_jumping(obj);
	break;
    case K_STANDING:
	king_standing(obj);
	break;
    }
}

static const Layout left[] = {
    { x:  0, y:  0, size:SPRITE_SIZE(2, 1), tile:TILE(2, CROWN_TILES) },
    { x: -8, y:  0, size:SPRITE_SIZE(4, 4), tile:TILE(3, KING_TILES) },
    { x: 24, y: 16, size:SPRITE_SIZE(4, 2), tile:TILE(3, BODY_TILES) },
    { x:  0, y: 32, size:SPRITE_SIZE(3, 2), tile:TILE(3, BODY_TILES + 8) },
    { x: 24, y: 32, size:SPRITE_SIZE(4, 2), tile:TILE(3, BODY_TILES + 14) },
    { x:  8, y: 48, size:SPRITE_SIZE(4, 2), tile:TILE(3, BODY_TILES + 22) },
    { x: 40, y: 48, size:SPRITE_SIZE(2, 2), tile:TILE(3, BODY_TILES + 30) },
};

static const Layout right[] = {
    { x: -1, y:  0, size:SPRITE_SIZE(2, 1), tile:TILE(2, CROWN_TILES) },
    { x: -8, y:  0, size:SPRITE_SIZE(4, 4), tile:FLIP(3, KING_TILES) },
    { x:-40, y: 16, size:SPRITE_SIZE(4, 2), tile:FLIP(3, BODY_TILES) },
    { x: -8, y: 32, size:SPRITE_SIZE(3, 2), tile:FLIP(3, BODY_TILES + 8) },
    { x:-40, y: 32, size:SPRITE_SIZE(4, 2), tile:FLIP(3, BODY_TILES + 14) },
    { x:-24, y: 48, size:SPRITE_SIZE(4, 2), tile:FLIP(3, BODY_TILES + 22) },
    { x:-40, y: 48, size:SPRITE_SIZE(2, 2), tile:FLIP(3, BODY_TILES + 30) },
};

static char is_king_legs_back(Object *obj) {
    return KING_STATE == K_JUMPING && obj->x != king[1]->x;
}

static void king_animate(Object *obj) {
    const Layout *layout = obj->direction < 0 ? left : right;

    for (u16 i = 0; i < show_parts; i++) {
	Object *part = king[i];
	Sprite *sprite = part->sprite;
	const Layout *place = layout + i;
	sprite->x = obj->x + place->x;
	sprite->y = obj->y + place->y;
	if (i == 5 && is_king_legs_back(obj)) {
	    sprite->cfg = TILE(3, LEGS_TILES);
	    sprite->size = SPRITE_SIZE(4, 4);
	    if (obj->direction > 0) sprite->cfg |= BIT(11);
	}
	else {
	    sprite->cfg = place->tile + part->frame;
	    sprite->size = place->size;
	}
    }
}

const Rectangle left_box[] = {
    { x1:  4, y1:  8, x2: 12, y2: 28 },
    { x1: 12, y1: 28, x2: 24, y2: 44 },
    { x1: 20, y1: 44, x2: 40, y2: 60 },
};

const Rectangle right_box[] = {
    { x1:  4, y1:  8, x2: 12, y2: 28 },
    { x1: -8, y1: 28, x2:  4, y2: 44 },
    { x1:-24, y1: 44, x2: -4, y2: 60 },
};

static short tile_in_range(Object *obj, u16 from, u16 range) {
    u16 tile = TILE_ID(obj->sprite->cfg);
    return from <= tile && tile < from + range;
}

static short is_small_rat(Object *obj) {
    return tile_in_range(obj, RAT_TILES, 9 * 4);
}

static short is_spittle(Object *obj) {
    return tile_in_range(obj, SLIME_TILES, 12);
}

static void remove_rat_or_spit(Object *obj) {
    if (is_small_rat(obj) || is_spittle(obj)) {
	kill_mob_silently(obj);
    }
}

static void king_hitbox(Object *obj) {
    u16 size = show_parts >> 1;
    const Rectangle *box = obj->direction < 0 ? left_box : right_box;
    if (KING_HP > 0 && boss_hitbox(obj, box, size, size)) {
	if (KING_HP == 0) {
	    apply_to_all_mobs(&remove_rat_or_spit);
	    king_starts_to_burn();
	    soldier_fist_pump();
	}
    }
}

static void king_update(Object *obj) {
    king_action(obj);
    king_animate(obj);
    king_hitbox(obj);
}

static void setup_king(u16 i) {
    setup_burns(4, BURN_TILES);

    show_parts = 2;
    king = malloc(KING_PARTS * sizeof(Object*));
    for (u16 i = 0; i < KING_PARTS; i++) {
	king[i] = setup_obj(0, 0, left[i].size);
    }

    crown = king[0];
    crown->x = 272;
    crown->y = 176;
    crown->direction = -1;

    pattern = NULL;
    KING_HP = BAR_HEALTH;
    mob_fn(crown, &king_update);
}

void display_king(void) {
    set_seed(8008);
    display_french(&king_level);

    load_tiles(&crown_img, CROWN_TILES);
    load_tiles(&king_head_img, KING_TILES);
    load_tiles(&king_legs_img, LEGS_TILES);
    load_image(&king_body_img, BODY_TILES, 3);

    display_progress_bar();
    lock_screen(1);

    schedule(&setup_king, 0);
    set_mob_order(1);
}
