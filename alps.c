#include "main.h"

#include "images/alps.h"
#include "images/rocks.h"
#include "images/queen.h"
#include "images/burn.h"
#include "images/bee.h"

#define BEE_TILES	193
#define BURN_TILES	(BEE_TILES + 8)
#define QUEEN_TILES	(BURN_TILES + 32)

static u16 draw_one_mountain(u16 x, byte tile) {
    if (tile == 1 || tile == 33) {
	paint_background(x, 10, 4, 2, tile, 6);
	x += 4;
    }
    else {
	/* 3, 19, 35, 51 */
	paint_background(x, 11, 2, 1, tile, 7);
	x += 2;
    }
    return x;
}

static const byte mountain_tiles[] = {
    1, 3, 19, 51, 33, 1, 35, 33, 35, 51, 3, 1,
    19, 35, 1, 35, 3, 33, 51, 19, 33, 19, 3, 51,
};

static void draw_mountains(void) {
    u16 x = 0, i = 0;
    while (x < 64) {
	x = draw_one_mountain(x, mountain_tiles[i++]);
    }
}

static const byte sky[] = {
    29, 21, 13, 5, 60, 52, 44, 36, 28, 20, 12
};
static void draw_sky(void) {
    for (u16 i = 0; i < 11; i++) {
	fill_VRAM(0x80 * i, sky[i], 0x040);
    }
}

static u16 flowers(u16 tile) {
    return ((random() & 1) << 5);
}

static void draw_vegetation(void) {
    set_seed(2);
    byte tile = 0;
    for (u16 i = 0; i < 0x40; i++) {
	poke_VRAM(0x600 + (i << 1), 37 + (tile << 3));
	tile = (tile + (random() % 3) + 1) & 3;
    }
    for (u16 i = 0x680; i < 0xf00; i += 2) {
	poke_VRAM(i, 6 + (tile << 3) + flowers(tile));
	tile = (tile + (random() % 3) + 1) & 3;
    }
}

extern u16 cacti_spacing_size(void);
extern const byte cacti_spacing[];

void draw_alpine_bones(void) {
    u16 offset = 0x680;
    for (u16 i = 0; i < cacti_spacing_size(); i++) {
	u16 tile = ((i / 6) + (random() & 3)) & 0xF;
	offset += cacti_spacing[i];
	poke_VRAM(offset, 7 + (tile >> 3) + 8 * (tile & 7));
    }
}

typedef struct Bee {
    char v_direction;
    char relinquish;
    char persistent;
    char dx, dy;
} Bee;

static Bee *b_obj;

#define BEE(obj) ((Bee *) (obj->private))

static void burn_bee(Object *obj) {
    obj->frame = 2;
    perish_sfx();
}

static u16 is_bee_alive(Object *obj) {
    return obj->frame < 2;
}

static void animate_bee(Object *obj) {
    u16 palette = 2;
    Sprite *sprite = obj->sprite;

    obj->life++;

    sprite->x = SCREEN_X(obj->x + BEE(obj)->dx);
    sprite->y = obj->y + BEE(obj)->dy + ON_SCREEN - 16;

    if (!is_bee_alive(obj)) {
	if ((obj->life & 3) == 0) obj->frame++;
    }
    else if (should_small_mob_burn(sprite)) {
	burn_bee(obj);
    }
    else {
	obj->frame = ((obj->life >> 1) & 1);
	small_mob_attack(obj);
	palette = 3;
    }

    sprite->cfg = TILE(palette, BEE_TILES + 4 * obj->frame);

    if (obj->frame >= 10) {
	free_mob(obj);
    }
    else {
	small_mob_end(obj, BEE(obj)->persistent);
    }
}

static void flip_bee(Object *obj) {
    animate_bee(obj);
    mob_adjust_sprite_dir(obj);
}

static void move_bee(Object *obj) {
    obj->x += obj->direction;
    obj->y += BEE(obj)->v_direction;
    flip_bee(obj);
}

static Object *setup_bee(short x, short y, u16 life) {
    Object *obj = alloc_mob();
    if (obj != NULL) {
	obj->x = x;
	obj->y = y;
	obj->frame = 0;
	obj->gravity = 0;
	obj->velocity = 0;
	obj->life = life;
	obj->direction = -1;
	obj->private = b_obj + mob_index(obj);
	obj->sprite->size = SPRITE_SIZE(2, 2);
	mob_fn(obj, &move_bee);
	BEE(obj)->v_direction = 0;
	BEE(obj)->persistent = 1;
	BEE(obj)->dx = 0;
	BEE(obj)->dy = 0;
    }
    return obj;
}

void emit_bee_block(u16 x) {
    for (u16 y = 0; y < 3; y++) {
	for (u16 x = 0; x < 3; x++) {
	    setup_bee(window + SCR_WIDTH + 16 * x + 12 * y, 168 + y * 16, 0);
	}
    }
}

extern const char small_circle[256];
static void circling_bee(Object *obj) {
    u16 index = (2 * obj->life * obj->direction) & 0xFF;
    BEE(obj)->dx = small_circle[index + 0];
    BEE(obj)->dy = small_circle[index + 1];
    animate_bee(obj);
}

static void emit_bee_circle(u16 x, u16 y, u16 offset, char dir) {
    for (u16 i = 0; i <= 96; i += 32) {
	Object *bee = setup_bee(x + 96, y, i + offset);
	mob_fn(bee, &circling_bee);
	bee->direction = dir;
    }
}

void emit_bee_circles(u16 x) {
    emit_bee_circle(x + 0x00, 140, 0,  1);
    emit_bee_circle(x + 0x40, 120, 4, -1);
    emit_bee_circle(x + 0x80, 140, 8,  1);
}

static void emit_static_bee(u16 x, u16 y, char dir) {
    Object *bee = setup_bee(x, y, 0);
    mob_fn(bee, &flip_bee);
    bee->direction = dir;
}

void emit_static_garden_bees(u16 x) {
    emit_static_bee(x + 64, 186, 1);
    emit_static_bee(x + 80, 192, -1);

    emit_static_bee(x + 112, 200, 1);
    emit_static_bee(x + 130, 196, -1);

    emit_static_bee(x + 184, 178, 1);
    emit_static_bee(x + 202, 184, -1);
}

void load_burn_tiles(u16 where) {
    update_tiles(burn_tiles, where, ARRAY_SIZE(burn_tiles));
}

void emit_xonix_bees(u16 x) {
    emit_static_bee(x + 0x00, 20,  -2);
    emit_static_bee(x + 0x14, 216, -2);
    emit_static_bee(x + 0x2C, 216,  2);
    emit_static_bee(x + 0x40, 20,   2);
}

static void diagonal_bee(Object *obj) {
    Sprite *sprite = obj->sprite;
    move_bee(obj);
    if (sprite->x >= SCR_WIDTH + ON_SCREEN - 16) {
	obj->direction = -abs(obj->direction);
    }
    if (sprite->y <= ON_SCREEN) {
	BEE(obj)->v_direction = 1;
    }
    if (!BEE(obj)->relinquish) {
	if (sprite->x <= ON_SCREEN) {
	    obj->direction = abs(obj->direction);
	}
	if (sprite->y >= SCR_HEIGHT + ON_SCREEN - 16) {
	    BEE(obj)->v_direction = -1;
	}
    }
}

static void relinquish_bee(Object *obj) {
    BEE(obj)->relinquish = 1;
}

void relinquish_all_bees(u16 x) {
    apply_to_all_mobs(&relinquish_bee);
}

static void kick_bees(Object *obj) {
    BEE(obj)->relinquish = 0;
    BEE(obj)->v_direction = -1;
    mob_fn(obj, &diagonal_bee);
}

static void kick_single_xonix_bee(u16 i) {
    while (i < MAX_MOBS) {
	Object *bee = get_mob(i++);
	if (bee->place != -1) {
	    callback(&kick_single_xonix_bee, 16, i);
	    kick_bees(bee);
	    break;
	}
    }
}

void kick_xonix_bees(u16 x) {
    kick_single_xonix_bee(0);
}

static void emit_single_xonix_bee(u16 x, u16 y, char dir) {
    Object *bee = setup_bee(x, y, 0);
    mob_fn(bee, &diagonal_bee);
    BEE(bee)->v_direction = dir;
    bee->direction = -2;
}

static void emit_xonix_stream_bee(u16 left) {
    u16 y = 32 + 96 * (left & 1) + 16 * (left >> 1);
    emit_single_xonix_bee(window + SCR_WIDTH, y, (y >= 128) ? 1 : -1);
    if (left < 8) {
	callback(&emit_xonix_stream_bee, 24, left + 1);
    }
}

void emit_xonix_stream(u16 x) {
    relinquish_all_bees(0);
    emit_xonix_stream_bee(0);
}

static void piston_bee(Object *obj) {
    u16 index = (2 * obj->life) & 0xFF;
    BEE(obj)->dy = small_circle[index + 1];
    (obj->life >= 128 ? free_mob : animate_bee)(obj);
}

static void emit_piston_bee(u16 x) {
    Object *bee = setup_bee(x, SCR_HEIGHT - 20, 0);
    mob_fn(bee, &piston_bee);
    bee->direction = 0;
}

void emit_bee_upstream(u16 pos_x) {
    emit_piston_bee(pos_x);
    if (get_soldier()->x < pos_x) {
	callback(emit_bee_upstream, 63, pos_x);
    }
}

void level_done_burn_bees(void) {
    apply_to_all_mobs(&burn_bee);
    void level_done(u16);
    level_done(0);
}

void display_alps(Function prepare_level) {
    /* load tiles */
    update_palette(alps_palette, 0, ARRAY_SIZE(alps_palette));
    update_tiles(alps_tiles, 1, ARRAY_SIZE(alps_tiles));

    load_soldier_tiles(1);
    reset_mobs();
    reset_window();

    /* background */
    fill_VRAM(0x600, 4, 0x80);

    draw_sky();
    draw_mountains();
    draw_vegetation();
    draw_alpine_bones();

    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    update_palette(rocks_palette, 16, ARRAY_SIZE(rocks_palette));
    update_tiles(rocks_tiles, 65, ARRAY_SIZE(rocks_tiles));

    clear_DMA_buffer(0, 0x1000);

    fill_VRAM(0, 0, 0x800);
    prepare_level();

    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_erika(void);
    music_erika();

    update_palette(bee_palette, 48, ARRAY_SIZE(bee_palette));
    update_tiles(bee_tiles, BEE_TILES, ARRAY_SIZE(bee_tiles));
    load_burn_tiles(BURN_TILES);

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);

    b_obj = malloc(sizeof(Bee) * MAX_MOBS);
}

void display_mountains(void) {
    display_alps(&prepare_mountain_level);
}

static Object *prev;
void display_plateau(void) {
    void prepare_plateau_level(void);
    display_alps(&prepare_plateau_level);
    prev = NULL;
}

static void emit_bee_at_height(u16 y) {
    u16 x = window + SCR_WIDTH;
    if (prev == NULL || x - prev->x > 32) {
	prev = setup_bee(x, y, 0);
    }
}

void emit_bee_row(u16 i) {
    emit_bee_at_height(204);
    schedule(&emit_bee_row, 0);
}

void bee_head_row(u16 i) {
    emit_bee_at_height(188);
    schedule(&bee_head_row, 0);
}

void emit_bee_head(u16 i) {
    cancel_timer(&emit_bee_row);
    bee_head_row(0);
}

#define QUEEN_PARTS	4
#define QUEEN_SLICES	16
#define QUEEN_GRAVITY	12

static Object **queen;

#define QUEEN_HP	queen[0]->life
#define QUEEN_TIME	queen[1]->life
#define QUEEN_STAGE	queen[2]->life
#define QUEEN_FLICK	queen[3]->life

struct Queen {
    char x, y;
    u16 frame[2];
};

#define QUEEN_FRAME(offset, flip) \
    ((flip) | TILE(3, QUEEN_TILES + (offset)))

struct Queen queen_layout[QUEEN_PARTS] = {
    { x:-32, y:-32, frame: { QUEEN_FRAME( 0, 0), QUEEN_FRAME(16, BIT(11)) } },
    { x:  0, y:-32, frame: { QUEEN_FRAME(16, 0), QUEEN_FRAME( 0, BIT(11)) } },
    { x:-32, y:  0, frame: { QUEEN_FRAME(32, 0), QUEEN_FRAME(48, BIT(11)) } },
    { x:  0, y:  0, frame: { QUEEN_FRAME(48, 0), QUEEN_FRAME(32, BIT(11)) } },
};

const Rectangle q_box[] = {
    { x1:  -6, y1: -28, x2:  6, y2: 28 },
    { x1: -16, y1:  -8, x2: 16, y2:  8 },
};

static void queen_flicker_color(u16 flicker) {
    u16 color;
    if (!flicker) {
	color = bee_palette[QUEEN_FLICK + 2];
    }
    else {
	QUEEN_FLICK = (QUEEN_FLICK + 2) & 3;
	color = QUEEN_FLICK ? 0x26e : 0x2ae;
    }
    update_color(50 + QUEEN_FLICK, color);
}

static const u16 ash_palette[][7] = {
    { 0x0224,0x0246,0x0268,0x028a,0x02ac,0x068a,0x022c },
    { 0x0244,0x0266,0x0288,0x04aa,0x04cc,0x08ac,0x0668 },
    { 0x0222,0x0444,0x0466,0x0688,0x06aa,0x0acc,0x0444 },
    { 0x0000,0x0222,0x0444,0x0666,0x0888,0x0aaa,0x0222 },
};

static void upload_ash_palette(u16 i) {
    update_palette(ash_palette[i], 49, ARRAY_SIZE(ash_palette[i]));
    upload_palette(0);
}

static void queen_animate(Object *obj) {
    for (u16 i = 0; i < QUEEN_PARTS; i++) {
	u16 frame = (queen[i]->frame >> (4 - QUEEN_STAGE)) & 1;
	Sprite *sprite = queen[i]->sprite;
	sprite->x = obj->x + queen_layout[i].x;
	sprite->y = obj->y + queen_layout[i].y;
	sprite->cfg = queen_layout[i].frame[frame];
	queen[i]->frame++;
    }
}

static const byte fall_order[] = {
    2, 3, 7, 6, 15, 0, 11, 10, 1, 4, 14, 9, 5, 8, 13, 12
};

static char cinder_dance[] = {
     0,  1,  0,  1,  1,  0,  1,  0,
     0, -1,  0, -1, -1,  0, -1,  0,
};

static void queen_piece_falling(Object *obj) {
    if (obj->sprite->y >= ON_SCREEN + SCR_HEIGHT + 32) {
	free_mob(obj);
	if (obj == queen[fall_order[15]]) {
	    schedule(&finish_level, 100);
	    fade_music(0);
	}
    }
    else if (obj->velocity < 0) {
	advance_y(obj, QUEEN_GRAVITY);
	obj->x += cinder_dance[(obj->frame >> 1) & 0xf];
	obj->sprite->y = obj->y;
	obj->sprite->x = obj->x;
	obj->frame++;
    }
}

struct Queen dying_layout[QUEEN_SLICES] = {
    { x:-32, y:-32, frame: { QUEEN_FRAME( 0, 0), SPRITE_SIZE(2, 4) } },
    { x: 16, y:-32, frame: { QUEEN_FRAME(24, 0), SPRITE_SIZE(2, 4) } },
    { x:-32, y:  0, frame: { QUEEN_FRAME(32, 0), SPRITE_SIZE(2, 4) } },
    { x: 16, y:  0, frame: { QUEEN_FRAME(56, 0), SPRITE_SIZE(2, 4) } },

    { x:-16, y:-32, frame: { QUEEN_FRAME( 8, 0), SPRITE_SIZE(1, 4) } },
    { x:  8, y:-32, frame: { QUEEN_FRAME(20, 0), SPRITE_SIZE(1, 4) } },
    { x:-16, y:  0, frame: { QUEEN_FRAME(40, 0), SPRITE_SIZE(1, 4) } },
    { x:  8, y:  0, frame: { QUEEN_FRAME(52, 0), SPRITE_SIZE(1, 4) } },

    { x: -8, y:-32, frame: { QUEEN_FRAME(12, 0), SPRITE_SIZE(1, 2) } },
    { x: -8, y:-16, frame: { QUEEN_FRAME(14, 0), SPRITE_SIZE(1, 2) } },
    { x: -8, y:  0, frame: { QUEEN_FRAME(44, 0), SPRITE_SIZE(1, 2) } },
    { x: -8, y: 16, frame: { QUEEN_FRAME(46, 0), SPRITE_SIZE(1, 2) } },

    { x:  0, y:-32, frame: { QUEEN_FRAME(16, 0), SPRITE_SIZE(1, 2) } },
    { x:  0, y:-16, frame: { QUEEN_FRAME(18, 0), SPRITE_SIZE(1, 2) } },
    { x:  0, y:  0, frame: { QUEEN_FRAME(48, 0), SPRITE_SIZE(1, 2) } },
    { x:  0, y: 16, frame: { QUEEN_FRAME(50, 0), SPRITE_SIZE(1, 2) } },
};

static void trigger_falldown(u16 i) {
    if (i < QUEEN_SLICES) {
	u16 j = fall_order[i];
	queen[j]->velocity = -1;
	queen[j]->gravity = QUEEN_GRAVITY;
	callback(&trigger_falldown, 8, ++i);
    }
}

static void emit_queen_burn(u16 i);
static void queen_falls_to_pieces(Object *obj) {
    free_burns();
    u16 x = obj->x;
    u16 y = obj->y;
    for (u16 i = QUEEN_PARTS; i < QUEEN_SLICES; i++) {
	queen[i] = alloc_mob();
    }
    for (u16 i = 0; i < QUEEN_SLICES; i++) {
	struct Queen *layout = dying_layout + i;
	Sprite *sprite = queen[i]->sprite;
	queen[i]->x = x + layout->x;
	queen[i]->y = y + layout->y;
	queen[i]->velocity = 0;
	queen[i]->frame = i;
	sprite->x = queen[i]->x;
	sprite->y = queen[i]->y;
	sprite->cfg = layout->frame[0];
	sprite->size = layout->frame[1];
	mob_fn(queen[i], &queen_piece_falling);
    }
    callback(&trigger_falldown, 0, 0);
}

static void dying_update(Object *obj) {
    queen_animate(obj);
    obj->x += ((obj->frame & 1) << 2) - 2;
    if (obj->y > ON_SCREEN + 48) obj->y--;

    if (QUEEN_TIME > 0x7F) {
	queen_falls_to_pieces(obj);
    }
    else {
	if (QUEEN_TIME == 112) {
	    cancel_timer(&emit_queen_burn);
	}
	upload_ash_palette(QUEEN_TIME >> 5);
	QUEEN_TIME++;
    }
}

static void burn_drones(Object *obj) {
    if (obj->private != NULL) {
	burn_bee(obj);
    }
}

static void emit_queen_burn(u16 i) {
    extern Object **burns;
    init_burn(burns[i]);
    if (!burns[i]->life) {
	burns[i]->private = queen[0];
	burns[i]->direction = (i & 1) ? -1 : 1;
	burns[i]->x = (random() % 48);
	burns[i]->y = (random() % 48);
    }
    callback(&emit_queen_burn, 4, i >= 3 ? 0 : i + 1);
    if (i == 0) perish_sfx();
}

static void queen_dies(u16 i) {
    set_seed(1792);
    soldier_fist_pump();
    mob_fn(queen[0], &dying_update);
    apply_to_all_mobs(&burn_drones);
    emit_queen_burn(0);
    QUEEN_TIME = 0;
}

static void queen_check_hitbox(Object *obj) {
    u16 size = ARRAY_SIZE(q_box);
    queen_flicker_color(0);
    if (QUEEN_HP > 0 && boss_hitbox(obj, q_box, size, size)) {
	if (QUEEN_HP == 0) {
	    schedule(&queen_dies, 0);
	}
	else {
	    queen_flicker_color(1);
	}
    }
}

extern const char larger_circle[512];
static void queen_fly_around(Object *obj) {
    u16 time = (QUEEN_TIME & 0x1FF);
    obj->x = ON_SCREEN + 160 + larger_circle[time + 0];
    obj->y = ON_SCREEN + 120 + larger_circle[time + 1];
}

static Object *emit_drone(Object *parent, char dx, char dy) {
    short x = parent->x - window;
    short y = parent->y - ON_SCREEN + 32;
    Object *obj = setup_bee(x, y, 0);
    if (obj != NULL) {
	obj->direction = dx;
	BEE(obj)->v_direction = dy;
	BEE(obj)->persistent = 0;
    }
    return obj;
}

static void drone_dive(Object *obj) {
    move_bee(obj);
    u16 bx = obj->sprite->x;
    u16 sx = get_soldier()->sprite->x;
    if (bx >= sx + 2 && bx <= sx + 6) {
	obj->direction = 0;
	BEE(obj)->v_direction = 1;
	mob_fn(obj, &move_bee);
    }
}

static void emit_diving_drone(Object *parent, char dx, char dy) {
    Object *obj = emit_drone(parent, dx, dy);
    if (obj != NULL) mob_fn(obj, &drone_dive);
}

static void first_stage_attack(Object *obj) {
    Sprite *soldier = get_soldier()->sprite;
    if (QUEEN_TIME == 0) {
	emit_drone(obj, -1, 0);
	emit_drone(obj,  1, 0);
    }
    else if (QUEEN_TIME == 256) {
	emit_diving_drone(obj, -1, 0);
	emit_diving_drone(obj,  1, 0);
    }
}

static void queen_first_stage(Object *obj) {
    if (QUEEN_HP < BAR_HEALTH) {
	first_stage_attack(obj);
    }
    queen_fly_around(obj);
    QUEEN_TIME = (QUEEN_TIME + 2) & 0x1FF;
}

static void queen_second_stage(Object *obj) {
    if (obj->x >= ON_SCREEN + 320 - 24) {
	obj->direction = -1;
    }
    else if (obj->x <= ON_SCREEN + 24) {
	obj->direction = 1;
    }
    obj->x += obj->direction;

    if (QUEEN_TIME == 0) {
	u16 bx = obj->sprite->x;
	u16 sx = get_soldier()->sprite->x;
	if (bx >= sx - 30 && bx <= sx - 26) {
	    emit_drone(obj, -1, -1);
	    emit_drone(obj,  0, -1);
	    emit_drone(obj,  1, -1);
	    QUEEN_TIME = 32;
	}
    }
    else {
	QUEEN_TIME--;
    }
}

static u16 queen_in_center(Object *obj) {
    return obj->x == ON_SCREEN + 160 && obj->y > ON_SCREEN + 120;
}

static void queen_third_stage(Object *obj) {
    if (obj->x >= ON_SCREEN + 320 - 24) {
	emit_drone(obj, -1, -1);
	emit_drone(obj,  0, -1);
	obj->direction = -1;
    }
    else if (obj->x <= ON_SCREEN + 24) {
	emit_drone(obj,  1, -1);
	emit_drone(obj,  0, -1);
	obj->direction = 1;
    }
    if (QUEEN_TIME <= ARRAY_SIZE(larger_circle)) {
	queen_fly_around(obj);
	QUEEN_TIME += 2 * obj->direction;
    }
    else {
	obj->x += 2 * obj->direction;
	if (queen_in_center(obj)) {
	    QUEEN_TIME += 2 * obj->direction;
	}
    }
}

static char opposite_direction(Object *obj) {
    return get_soldier()->x < obj->x - window ? 1 : -1;
}

static void queen_stages(Object *obj) {
    const u16 third = BAR_HEALTH / 3;
    if (QUEEN_STAGE < 3 && QUEEN_HP <= third && queen_in_center(obj)) {
	obj->direction = opposite_direction(obj);
	QUEEN_TIME = obj->direction > 0 ? 0 : ARRAY_SIZE(larger_circle);
	QUEEN_STAGE = 3;
    }
    else if (QUEEN_STAGE < 2 && QUEEN_HP <= 2 * third && QUEEN_TIME == 0) {
	obj->direction = opposite_direction(obj);
	QUEEN_STAGE = 2;
    }

    switch (QUEEN_STAGE) {
    case 1:
	queen_first_stage(obj);
	break;
    case 2:
	queen_second_stage(obj);
	break;
    case 3:
	queen_third_stage(obj);
	break;
    default:
	break;
    }
}

static void queen_update(Object *obj) {
    queen_stages(obj);
    queen_check_hitbox(obj);
    queen_animate(obj);
}

static void setup_queen(u16 i) {
    setup_burns(4, BURN_TILES);

    queen = malloc(QUEEN_SLICES * sizeof(Object*));
    for (u16 i = 0; i < QUEEN_PARTS; i++) {
	queen[i] = alloc_mob();
	Sprite *sprite = queen[i]->sprite;
	sprite->size = SPRITE_SIZE(4, 4);
	sprite->cfg = TILE(3, QUEEN_TILES + (i << 4));
	queen[i]->frame = i;
	queen[i]->life = 0;
    }

    QUEEN_STAGE = 1;
    QUEEN_HP = BAR_HEALTH;
    mob_fn(queen[0], &queen_update);
}

void display_queen(void) {
    update_tiles(queen_tiles, QUEEN_TILES, ARRAY_SIZE(queen_tiles));

    display_alps(&prepare_queen_level);
    display_progress_bar();
    lock_screen(1);

    schedule(&setup_queen, 0);
}
