#include "main.h"

#include "images/rat.h"
#include "images/town.h"
#include "images/street.h"

#include "town.inc"

#define RAT_TILES	257
#define BURN_TILES	(RAT_TILES + 9 * 4)

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

static u16 *scroll_buf = NULL;

static void update_scroll_buffer(void) {
    copy_to_VRAM_ptr(VRAM_SCROLL_A, 0x380, scroll_buf);
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

static void display_french(const Level *level) {
    /* load tiles */
    load_image(&town_img, 1, 0);
    load_image(&street_img, 129, 1);
    load_image(&rat_img, RAT_TILES, 3);

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

    scroll_type(0x02);
    scroll_buf = malloc(0x380);
    switch_frame(&update_town);

    r_obj = malloc(sizeof(Rat) * MAX_MOBS);
    rat_counter = 0;
    dead_rats = 0;
}

static char rat_diff(Object *obj) {
    return soldier.x - obj->x + 16 > 0 ? 1 : -1;
}

static char is_rat_at_edge(Object *obj) {
    return obj->direction == -1 && obj->sprite->x < ON_SCREEN - 8;
}

static u16 live_rats(void) {
    return rat_counter - dead_rats;
}

static void move_rat(Object *obj) {
    u16 land = 0, palette = 2;
    Sprite *sprite = obj->sprite;
    char emerged = (obj->frame >= 3);
    short old_x = obj->x;

    if (emerged) {
	obj->x += obj->direction;
	if ((counter & 7) == 0) {
	    obj->x += obj->direction;
	}
	land = advance_obj(obj, 8, 8);
    }

    if (mob_move(obj, 17)) {
	if (emerged && !land) {
	    obj->frame = 3;
	    if (RAT(obj)->was_ground) {
		char diff = rat_diff(obj);
		if (diff != obj->direction) {
		    obj->x = old_x;
		    obj->direction = diff;
		    if (soldier.y > obj->y) {
			obj->y++;
		    }
		}
		else {
		    obj->velocity = 2;
		}
	    }
	}
	else if ((obj->life & 3) == 0) {
	    obj->frame = obj->frame == 8 ? 3 : obj->frame + 1;
	    if (is_rat_at_edge(obj) && live_rats() < 4) {
		obj->direction = 1;
	    }
	}
	palette = 3;
    }
    else {
	Rat *rat = RAT(obj);
	if (rat->fn != NULL) {
	    rat->fn(rat->cookie);
	    rat->fn = NULL;
	}
	if (obj->place == -1) {
	    dead_rats++;
	}
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
    callback(&bottom_window_rat, 100, x - 206);
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

void display_town(void) {
    display_french(&town_level);
}

void display_ramp(void) {
    display_french(&ramp_level);
}
