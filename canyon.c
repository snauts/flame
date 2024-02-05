#include "main.h"

#include "images/canyon.h"
#include "images/desert.h"
#include "images/cliffs.h"
#include "images/hopper.h"

#include "images/mantis_body.h"
#include "images/mantis_claw.h"
#include "images/mantis_wing.h"
#include "images/mantis_leg.h"

void paint_background(u16 x, u16 y, u16 w, u16 h, u16 i, u16 n) {
    u16 dx, dy;
    for (dx = 0; dx < w; dx++) {
	for (dy = 0; dy < h; dy++) {
	    poke_VRAM(((x + dx) * 2) + ((y + dy) * 128), i);
	    i += 1;
	}
	i += n;
    }
}

const byte cloud_data[] = {
    3, 5, 1, 8, 8, 7, 12, 2, 3, 19, 7, 5, 24, 4, 7, 30, 8, 3,
    36, 1, 1, 42, 6, 5, 47, 3, 7, 51, 7, 1, 56, 2, 3,
};

static void paint_cloud(u16 x, u16 y, u16 i) {
    paint_background(x, y, 8, 2, i, 6);
}

static void draw_clouds(void) {
    u16 i;
    for (i = 0; i < ARRAY_SIZE(cloud_data); i += 3) {
	paint_cloud(cloud_data[i + 0], cloud_data[i + 1], cloud_data[i + 2]);
    }
}

const byte horizon_data[] = {
    65, 66, 67, 68, 67, 66, 65, 67, 65, 68, 66, 68, 65, 67, 68, 66
};

static void draw_horizon(void) {
    u16 x, i = 0;
    for (x = 0; x <= 60; x += 4) {
	paint_background(x, 12, 4, 1, horizon_data[i++], 7);
    }
}

const byte cacti[] = { 69, 77, 85, 93, 70, 86, 78, 94 };

const byte cacti_spacing[] = {
    2, 10, 8, 12, 6, 6, 18, 10, 6, 28, 10, 20, 8, 10, 24, 6, 26, 8, 8, 12,
    10, 6, 6, 18, 14, 6, 22, 6, 6, 10, 8, 10, 34, 18, 18, 12, 22, 20, 14,
    8, 22, 16, 20, 16, 6, 10, 24, 20, 6, 16, 16, 26, 24, 6, 78, 14, 56,
    6, 20, 38, 28, 22, 26, 30, 42, 22, 16, 56, 26, 22, 48, 114, 28,
};

static void draw_vegetation(byte more_bones) {
    u16 i, offset = 0x700, tile = 0;
    for (i = 0; i < ARRAY_SIZE(cacti_spacing); i++) {
	offset += cacti_spacing[i];
	u16 tile = cacti[(tile + offset) & 7];
	if (!more_bones) {
	    if (i == 60 || i == 40 || i == 20) tile = 95;
	}
	else if (tile == 69 || tile == 77 || tile == 93) {
	    tile = 95;
	}
	else if (tile == 85) {
	    tile = 86;
	}
	poke_VRAM(offset, tile);
	tile++;
    }
}

u16 cacti_spacing_size(void) {
    return ARRAY_SIZE(cacti_spacing);
}

const byte sand[] = { 72, 80, 88, 71, 79, 87, 71, 72 };

static void draw_sand(void) {
    u16 i;
    set_seed(1);
    for (i = 0x700; i < 0xf00; i += 2) {
	poke_VRAM(i, sand[random() & 7]);
    }
}

typedef struct Hopper {
    u16 patrol_start;
    u16 patrol_end;
    byte jump_amount;
    byte persistent;
} Hopper;

Hopper *h_obj;

#define HOPPER(obj) ((Hopper *) (obj->private))

static u16 is_hopper_alive(Object *obj) {
    return obj->frame < 9;
}

static void hopper_die(Object *obj) {
    obj->frame = 9;
    obj->life = 0;
    perish_sfx();
}

static void move_hopper(Object *obj) {
    Sprite *sprite = obj->sprite;

    obj->life++;
    obj->x += obj->direction;
    u16 land = advance_obj(obj, 4, 12);

    sprite->x = SCREEN_X(obj->x);
    sprite->y = obj->y + ON_SCREEN - 16;

    if (!is_hopper_alive(obj)) {
	if ((obj->life & 3) == 0) obj->frame++;
    }
    else if (should_small_mob_burn(sprite)) {
	hopper_die(obj);
    }
    else {
	if (land) {
	    obj->frame = 3 + ((obj->life >> 2) % 6);
	}
	else {
	    obj->frame = 1 + ((obj->life >> 1) & 1);
	}
	small_mob_attack(obj);
    }

    sprite->cfg = TILE(2, 289 + 4 * obj->frame);
    mob_adjust_sprite_dir(obj);

    if (obj->frame == 17) {
	free_mob(obj);
    }
    else {
	small_mob_end(obj, HOPPER(obj)->persistent);
    }
}

static void jump_hopper(Object *obj, u16(*jump_condition)(Object *)) {
    if (is_hopper_alive(obj) && jump_condition(obj)) {
	obj->velocity = HOPPER(obj)->jump_amount;
    }
    move_hopper(obj);
}

static u16 is_period(Object *obj) {
    return (obj->life & 0x3F) == 0;
}

static u16 is_long_period(Object *obj) {
    return (obj->life & 0x7F) == 0;
}

static u16 is_on_ground(Object *obj) {
    return get_snap(obj->x + 4, obj->y, obj->y);
}

static void periodic_hopper(Object *mob) {
    jump_hopper(mob, &is_period);
}

static void periodic_high_hopper(Object *mob) {
    jump_hopper(mob, &is_long_period);
}

static void immediate_hopper(Object *mob) {
    jump_hopper(mob, &is_on_ground);
}

static Object *setup_hopper(short x, short y, u16 life) {
    Object *obj = alloc_mob();
    if (obj != NULL) {
	obj->x = x;
	obj->y = y;
	obj->frame = 0;
	obj->gravity = 0;
	obj->velocity = 0;
	obj->life = life;
	obj->direction = -1;
	obj->private = h_obj + mob_index(obj);
	obj->sprite->size = SPRITE_SIZE(2, 2);
	mob_fn(obj, &move_hopper);

	HOPPER(obj)->persistent = 0;
    }
    return obj;
}

static byte squad_members;
static void emit_hopper_squad_next(u16 i) {
    Object *mob = get_mob(i);
    if (mob == NULL || mob->sprite->x <= SCR_WIDTH + ON_SCREEN - 16) {
	mob = setup_hopper(window + SCR_WIDTH, 208, i * 4);
	squad_members--;
    }
    if (mob) {
	mob_fn(mob, &periodic_hopper);
	HOPPER(mob)->jump_amount = 2;
	if (squad_members > 0) {
	    callback(&emit_hopper_squad_next, 0, mob_index(mob));
	}
    }
}

void emit_hopper_squad(u16 i) {
    squad_members = 8;
    emit_hopper_squad_next(0);
}

static u16 stream_x;
void emit_next_hopper_stream(u16 i) {
    Object *mob = get_mob(i);
    if (mob == NULL || mob->sprite->x <= SCR_WIDTH + ON_SCREEN - 32) {
	mob = setup_hopper(window + SCR_WIDTH, 208, 112);
    }
    if (mob) {
	mob_fn(mob, &periodic_high_hopper);
	HOPPER(mob)->jump_amount = 4;
    }
    if (window < stream_x - 160) {
	callback(&emit_next_hopper_stream, 0, mob_index(mob));
    }
}

void emit_hopper_stream(u16 pos_x) {
    stream_x = pos_x;
    emit_next_hopper_stream(0);
}

static u16 hole_x;
static void emit_next_hole_hopper(u16 count) {
    u16 delay = 0;
    if (window < hole_x - 112) {
	Object *mob = setup_hopper(hole_x, SCR_HEIGHT + 16, 0);
	if (mob != NULL) {
	    mob->velocity = 4;
	    delay = (count == 7 ? 128 : 24);
	    count++;
	}
	callback(&emit_next_hole_hopper, delay, count & 7);
    }
}

void emit_hole_hoppers(u16 pos_x) {
    hole_x = pos_x + 18;
    emit_next_hole_hopper(0);
}

static void emit_row_of_sky_hoppers(u16 pos_x) {
    for (u16 i = 0; i < 4; i++) {
	setup_hopper(pos_x + (i << 4), -16 + (i << 2), 0);
    }
}

static u16 sky_x;
static void emit_next_sky_hopper(u16 delay) {
    if (window < sky_x - 112) {
	emit_row_of_sky_hoppers(sky_x);
	callback(&emit_next_sky_hopper, delay, 384 - delay);
    }
}

void emit_sky_hoppers(u16 pos_x) {
    sky_x = pos_x - 40;
    emit_next_sky_hopper(64);
}

static void patrolling_hopper(Object *obj) {
    if (obj->x <= HOPPER(obj)->patrol_start) {
	obj->direction = 1;
    }
    else if (obj->x >= HOPPER(obj)->patrol_end) {
	obj->direction = -1;
    }
    move_hopper(obj);
}

void emit_plateau_patrollers(u16 pos_x) {
    for (u16 y = 160; y >= 64; y -= 48) {
	for (u16 x = 16; x <= 48; x += 32) {
	    Object *mob = setup_hopper(pos_x + x, y, 0);
	    HOPPER(mob)->persistent = 1;
	    HOPPER(mob)->patrol_start = pos_x;
	    HOPPER(mob)->patrol_end = pos_x + 64;
	    mob->direction = ((y == 112) ? -1 : 1) * (2 - (x >> 4));
	    mob_fn(mob, &patrolling_hopper);
	}
    }
}

static void emit_charging_hoppers(u16 pos_x) {
    if (window < pos_x + 152) {
	setup_hopper(window + SCR_WIDTH, 32, 0);
	callback(&emit_charging_hoppers, 48, pos_x);
    }
}

void emit_chasing_hoppers(u16 pos_x) {
    purge_mobs();
    pos_x -= SCR_WIDTH;
    for (short i = 0; i <= 32; i += 16) {
	Object *mob = setup_hopper(pos_x + i, -16, 0);
	mob->direction = 1;
    }
    emit_charging_hoppers(pos_x);
}

void emit_marta_platform_patrollers(u16 pos_x) {
    for (u16 y = 184; y >= 88; y -= 24) {
	for (u16 x = 0; x <= 32; x += 16) {
	    u16 skew = y & BIT(3);
	    u16 offset = pos_x + (skew ? 64 : 32);
	    Object *mob = setup_hopper(offset + x, y, 0);
	    HOPPER(mob)->persistent = 1;
	    HOPPER(mob)->patrol_start = offset - 16;
	    HOPPER(mob)->patrol_end = offset + 48;
	    mob->direction = skew ? -1 : 1;
	    mob_fn(mob, &patrolling_hopper);
	}
    }
}

void emit_down_stair_guards(u16 pos_x) {
    purge_mobs();
    u16 x = pos_x + 48, y = 88;
    emit_row_of_sky_hoppers(pos_x - 24);
    for (u16 i = 0; i < 6; i++) {
	Object *mob = setup_hopper(x, y, 0);
	HOPPER(mob)->persistent = 1;
	HOPPER(mob)->patrol_start = x - 40;
	HOPPER(mob)->patrol_end = x + 32;
	mob_fn(mob, &patrolling_hopper);
	mob->direction = 1;
	x += 96;
	y += 24;
    }
}

#define SWARM_SIZE 5
static char swarm_on;
static void chasing_swarm(u16 info) {
    byte n = info & 0xff;
    byte index = info >> 8;
    Object *mob = get_mob(index);
    if (swarm_on) {
	if (mob == NULL || mob->place < 0) {
	    mob = setup_hopper(window + 16 * n, -16, 0);
	}
	mob->direction = 1;
	mob_fn(mob, &immediate_hopper);
	HOPPER(mob)->jump_amount = 2 + ((counter + n) & 3);
	callback(&chasing_swarm, 0, (mob_index(mob) << 8) | n);
    }
    else if (mob->place >= 0) {
	hopper_die(mob);
    }
}

void emit_chasing_swarm(u16 pos_x) {
    purge_mobs();
    swarm_on = 1;
    for (u16 n = 0; n < SWARM_SIZE; n++) {
	chasing_swarm((MAX_MOBS << 8) | n);
    }
}

void ignite_swarm(u16 pos_x) {
    if (pos_x > 0) {
	schedule(&ignite_swarm, 80);
    }
    else {
	swarm_on = 0;
    }
}

void display_desert(Function prepare_level, byte more_bones) {
    /* load tiles */
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    load_soldier_tiles(0);
    reset_mobs();
    reset_window();

    /* background */
    fill_VRAM(0x000,  1, 0x300);
    fill_VRAM(0x680, 96, 0x40);

    draw_sand();
    draw_clouds();
    draw_horizon();
    draw_vegetation(more_bones);

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    update_palette(desert_palette, 16, ARRAY_SIZE(desert_palette));
    update_tiles(desert_tiles, 97, ARRAY_SIZE(desert_tiles));
    update_tiles(cliffs_tiles, 161, ARRAY_SIZE(cliffs_tiles));

    clear_DMA_buffer(0, 0x1000);

    prepare_level();

    fill_VRAM(0xe00, TILE(1, 1) | BIT(15), 0x80);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_johnny(void);
    music_johnny();

    update_tiles(hopper_tiles, 289, ARRAY_SIZE(hopper_tiles));

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);

    h_obj = malloc(sizeof(Hopper) * MAX_MOBS);
}

void display_canyon(void) {
    display_desert(&prepare_desert_level, 0);
}

void display_rusty(void) {
    display_desert(&prepare_rusty_level, 0);
}

#define MANTIS_PARTS	8
#define BURN_COUNT	4
static Object **mantis;
extern Object **burns;

typedef struct Mantis {
    char x, y, size;
    u16 tile;
} Mantis;

#define MANTIS_HP	mantis[0]->life
#define IS_AGITATED	mantis[1]->life
#define FLICKERING	mantis[1]->frame
#define MANTIS_WALK	mantis[1]->velocity
#define VERTICAL	mantis[1]->direction
#define DETACHED	mantis[2]->life

#define MANTIS_MIN_X	144
#define MANTIS_MAX_X	336
#define MANTIS_MIN_Y	152
#define MANTIS_MAX_Y	272

const Mantis mantis_layout[] = {
    { x:  0, y:  0, size: SPRITE_SIZE(4, 2), tile: TILE(3, 357) },
    { x:  8, y: 16, size: SPRITE_SIZE(2, 2), tile: TILE(3, 365) },
    { x: 16, y: 32, size: SPRITE_SIZE(4, 2), tile: TILE(3, 369) },
    { x: 48, y: 32, size: SPRITE_SIZE(4, 2), tile: TILE(3, 377) },
    { x:-16, y: 16, size: SPRITE_SIZE(4, 4), tile: TILE(3, 385) },
    { x: 16, y: 48, size: SPRITE_SIZE(4, 2), tile: TILE(3, 449) },
    { x: 48, y: 48, size: SPRITE_SIZE(4, 2), tile: TILE(3, 473) | BIT(11) },
    { x: 25, y:  1, size: SPRITE_SIZE(4, 4), tile: TILE(3, 545) },
};

static u16 mantis_2nd_stage(void) {
    return MANTIS_HP < BAR_HEALTH / 2;
}

static void set_claw_sprite(Object *obj) {
    set_sprite_tile(obj->sprite, TILE(3, 385 + 16 * obj->frame));
}

static void animate_claw(void) {
    mantis[4]->frame = (mantis[4]->life++ >> 3) & 3;
    set_claw_sprite(mantis[4]);
}

static void set_leg_sprite(Object *obj) {
    set_sprite_tile(obj->sprite, TILE(3, 449 + 8 * obj->frame));
}

static void animate_legs(void) {
    if (++mantis[5]->life >= 5) {
	mantis[5]->life = 0;
	mantis[5]->frame = mantis[5]->frame == 11 ? 0 : mantis[5]->frame + 1;
	mantis[6]->frame = mantis[6]->frame == 0 ? 11 : mantis[6]->frame - 1;
    }
    if (!MANTIS_WALK) {
	mantis[5]->frame = 0;
	mantis[6]->frame = 1;
    }
    set_leg_sprite(mantis[5]);
    set_leg_sprite(mantis[6]);
}

static void mantis_start_flying(void) {
    if (VERTICAL == 0) VERTICAL = mantis[0]->y >= MANTIS_MAX_Y ? -1 : 1;
}

static void animate_wing(Object *obj) {
    if (!mantis_2nd_stage()) {
	obj->sprite->x = obj->sprite->y = 0;
    }
    else if (MANTIS_WALK) {
	set_sprite_tile(obj->sprite, TILE(3, 545));
    }
    else {
	u16 flip = obj->frame & 1;
	u16 tile = TILE(3, 545 + 16 * flip);
	if (flip) {
	    obj->sprite->x -= 8 * mantis[0]->direction;
	    obj->sprite->y += 1;
	}
	set_sprite_tile(obj->sprite, tile);
	if (obj->life-- == 0) {
	    obj->life = obj->frame;
	    if (obj->frame < 2) {
		mantis_start_flying();
		obj->frame = 2;
	    }
	    else {
		obj->frame--;
	    }
	}
    }
}

static u16 is_mantis_neck(u16 i) {
    return mantis_layout[i].size == SPRITE_SIZE(2, 2);
}

static short mantis_sprite_x(u16 i, u16 flip) {
    if (flip) {
	return 64 + (is_mantis_neck(i) ? 16 : 0) - mantis_layout[i].x;
    }
    else {
	return mantis_layout[i].x;
    }
}

static void flip_sprite(Sprite *sprite, u16 should_flip) {
    if (should_flip) {
	sprite->cfg &= ~BIT(11);
    }
    else {
	sprite->cfg |= BIT(11);
    }
}

static void place_mantis(u16 x, u16 y, u16 flip) {
    for (u16 i = 0; i < MANTIS_PARTS; i++) {
	Sprite *sprite = mantis[i]->sprite;
	sprite->x = x + mantis_sprite_x(i, flip);
	sprite->y = y + mantis_layout[i].y;
	flip_sprite(sprite, (flip == 0 ^ i == 6));
    }
}

const Rectangle f_box[] = {
    { x1:  8, y1:  8, x2: 16, y2: 20 },
    { x1: -4, y1: 20, x2: 16, y2: 48 },
    { x1: 32, y1: 40, x2: 72, y2: 64 },
};

const Rectangle b_box[] = {
    { x1: 80, y1:  8, x2: 88, y2: 20 },
    { x1: 80, y1: 20, x2:100, y2: 48 },
    { x1: 24, y1: 40, x2: 64, y2: 64 },
};

static void mantis_flicker_color(u16 upd) {
    update_color(54 + FLICKERING, mantis_body_palette[FLICKERING + 6] + upd);
}

static void adjust_manits_position(Object *obj) {
    if (MANTIS_WALK) obj->x += obj->direction;

    if (obj->x <= MANTIS_MIN_X && obj->direction < 0) {
	obj->direction = 1;
    }
    if (obj->x >= MANTIS_MAX_X && obj->direction > 0) {
	obj->direction = -1;
    }
}

static void adjust_mantis_height(Object *obj) {
    obj->y += VERTICAL;
    if (obj->y <= MANTIS_MIN_Y && VERTICAL < 0) {
	obj->y = MANTIS_MIN_Y;
	MANTIS_WALK = 1;
	VERTICAL = 0;
    }
    if (obj->y >= MANTIS_MAX_Y && VERTICAL > 0) {
	obj->y = MANTIS_MAX_Y;
	MANTIS_WALK = 1;
	VERTICAL = 0;
    }
}

static void emit_mantis_burn(u16 i) {
    init_burn(burns[i]);
    if (!burns[i]->life) {
	burns[i]->private = mantis[2];
	u16 dx = (mantis[0]->direction < 0) ? -8 : -40;
	burns[i]->direction = (i & 1) ? -1 : 1;
	burns[i]->x = (random() & 0x3F) + dx;
	burns[i]->y = (random() & 0x07) - 4;
    }
    callback(&emit_mantis_burn, 4, i >= (BURN_COUNT - 1) ? 0 : i + 1);
    if (i == 0 && mantis[5]->sprite->y != 0) perish_sfx();
}

static void mantis_agony_jerk(u16 delay) {
    mantis[5]->frame = random() % 12;
    set_leg_sprite(mantis[5]);
    mantis[6]->frame = random() % 12;
    set_leg_sprite(mantis[6]);

    callback(&mantis_agony_jerk, delay, 7 - delay);
}

static void start_agonizing(void) {
    mantis[4]->frame = 0;
    mantis[5]->frame = 0;
    mantis[6]->frame = 8;
    mantis_agony_jerk(3);
}

static const u16 ash_palette[][13] = {
    { 0x00a2,0x02a4,0x04a6,0x0462,0x02a8,0x02a6,0x04a4,
      0x046c,0x0008,0x02aa,0x0288,0x0484,0x06c6, },
    { 0x008a,0x0286,0x0486,0x0268,0x028a,0x0288,0x0488,
      0x046a,0x0006,0x0288,0x0268,0x0466,0x06a8, },
    { 0x0468,0x0466,0x0468,0x0244,0x0248,0x0246,0x0466,
      0x0446,0x0002,0x0446,0x0446,0x0446,0x08a8, },
    { 0x0444,0x0444,0x0666,0x0222,0x0222,0x0444,0x0666,
      0x0444,0x0000,0x0444,0x0444,0x0444,0x0888, },
};

static void upload_ash_palette(u16 i) {
    update_palette(ash_palette[i], 49, ARRAY_SIZE(ash_palette[i]));
    upload_palette(0);
}

static void mantis_part_explode(u16 i) {
    Object *part = mantis[i];
    advance_y(part, 4);
    part->sprite->x += 4 * part->direction;
    part->sprite->y = part->y;
    if (part->frame++ > 4) {
	part->sprite->cfg ^= BIT(11);
	part->frame = 0;
    }
    if (part->y < SCR_HEIGHT + ON_SCREEN) {
	callback(&mantis_part_explode, 0, i);
    }
    else {
	part->sprite->y = part->y = 0;
	Object *burn = part->private;
	burn->life = 0;
    }
}

static u16 get_burn_x_offset(u16 i) {
    u16 dir = mantis[0]->direction;
    switch (i) {
    case 0:
	return 8 + dir * 8;
    case 1:
	return 0;
    default:
	return 8;
    }
}

static u16 get_burn_y_offset(Object *obj) {
    return (obj->sprite->size & 3) == 3 ? 8 : 0;
}

static void mantis_blow_off_part(u16 i, u16 time, u16 b_index, char dir) {
    Object *obj = mantis[i];
    Object *burn = burns[b_index];
    obj->direction = mantis[0]->direction * dir;
    obj->y = obj->sprite->y;
    obj->private = burn;
    obj->velocity = 6;

    burn->life = 1;
    burn->private = obj;
    burn->x = get_burn_x_offset(i);
    burn->y = get_burn_y_offset(obj);

    callback(&mantis_part_explode, time, i);
}

static void blow_off_legs_and_body(u16 i) {
    mantis_blow_off_part(2, 10, 0, 1);
    mantis_blow_off_part(3, 10, 1, -1);
    mantis_blow_off_part(5, 40, 3, 1);
    mantis_blow_off_part(6, 40, 2, -1);
    schedule(&finish_level, 200);
    schedule(&fade_music, 100);
}

static void mantis_turn_to_ash(u16 i) {
    upload_ash_palette(i);
    if (i < ARRAY_SIZE(ash_palette) - 1) {
	callback(&mantis_turn_to_ash, 25, i + 1);
    }
    else {
	mantis_blow_off_part(0, 25, 0, 1);
	mantis_blow_off_part(4, 50, 1, 1);
	mantis_blow_off_part(7, 50, 3, -1);
	mantis_blow_off_part(1, 75, 2, -1);
	schedule(&blow_off_legs_and_body, 150);
	DETACHED = 1;
    }
}

static void mantis_pepsi(u16 n) {
    set_seed(1984);
    soldier_fist_pump();
    emit_mantis_burn(0);
    start_agonizing();
    mantis_turn_to_ash(0);
}

static void mantis_check_hitbox(Object *obj) {
    const Rectangle *box_offset = obj->direction > 0 ? b_box : f_box;

    mantis_flicker_color(0);
    if (boss_hitbox(obj, box_offset, ARRAY_SIZE(f_box), 2)) {
	if (!MANTIS_HP) {
	    schedule(&mantis_pepsi, 0);
	}
	else {
	    FLICKERING = (FLICKERING + 1) & 1;
	    mantis_flicker_color(0x222);
	}
	IS_AGITATED = 1;
    }
}

static u16 mantis_vertical_attack(Object *obj, Sprite *soldier) {
    return abs(obj->sprite->x - soldier->x) < 4
	|| (obj->x >= (MANTIS_MAX_X - 1) && soldier->x > MANTIS_MAX_X)
	|| (obj->x <= (MANTIS_MIN_X + 1) && soldier->x < MANTIS_MIN_X);
}

static void mantis_gets_angry(Object *obj, Sprite *soldier) {
    if (IS_AGITATED && MANTIS_WALK) {
	short side = clamp(obj->sprite->x - soldier->x, 1);
	if (mantis_2nd_stage() && mantis_vertical_attack(obj, soldier)) {
	    mantis[7]->frame = mantis[7]->life = 10;
	    MANTIS_WALK = 0;
	    IS_AGITATED = 0;
	}
	else if (side == obj->direction) {
	    obj->direction = -obj->direction;
	    obj->x -= 40 * obj->direction;
	    IS_AGITATED = 0;
	}
    }
}

static void mantis_fall_down(Object *obj) {
    if (!DETACHED) {
	if (obj->y >= MANTIS_MAX_Y) {
	    obj->y = MANTIS_MAX_Y;
	}
	else {
	    advance_y(obj, 6);
	}
    }
}

static void walk_mantis(Object *obj) {
    Sprite *soldier = get_soldier()->sprite;
    if (!DETACHED) {
	place_mantis(obj->x, obj->y, obj->direction > 0);
    }

    if (!MANTIS_HP) {
	mantis_fall_down(obj);
	return; /* pepsi */
    }

    animate_claw();
    animate_legs();
    animate_wing(mantis[7]);

    adjust_manits_position(obj);
    adjust_mantis_height(obj);

    mantis_check_hitbox(obj);
    mantis_gets_angry(obj, soldier);
}

static void setup_mantis(u16 i) {
    setup_burns(BURN_COUNT, 577);

    mantis = malloc(MANTIS_PARTS * sizeof(Object*));
    for (u16 i = 0; i < MANTIS_PARTS; i++) {
	mantis[i] = alloc_mob();
	Sprite *sprite = mantis[i]->sprite;
	sprite->size = mantis_layout[i].size;
	sprite->cfg = mantis_layout[i].tile;
	mantis[i]->frame = 0;
	mantis[i]->life = 0;
    }

    VERTICAL = 0;
    MANTIS_WALK = 1;
    mantis[0]->x = ON_SCREEN + 320;
    mantis[0]->y = MANTIS_MAX_Y;
    mantis[0]->life = BAR_HEALTH;
    mantis[0]->direction = -1;
    mantis[0]->velocity = 0;
    mantis[0]->gravity = 0;
    mob_fn(mantis[0], &walk_mantis);
}

void display_mantis(void) {
    update_palette(mantis_body_palette, 48, ARRAY_SIZE(mantis_body_palette));

    update_tiles(mantis_body_tiles, 357, ARRAY_SIZE(mantis_body_tiles));
    update_tiles(mantis_claw_tiles, 385, ARRAY_SIZE(mantis_claw_tiles));
    update_tiles(mantis_leg_tiles, 449, ARRAY_SIZE(mantis_leg_tiles));
    update_tiles(mantis_wing_tiles, 545, ARRAY_SIZE(mantis_wing_tiles));

    load_burn_tiles(577);

    display_desert(&prepare_mantis_level, 1);
    display_progress_bar();
    lock_screen(1);

    schedule(&setup_mantis, 0);
}
