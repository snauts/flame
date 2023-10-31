#include "main.h"

#include "images/alps.h"
#include "images/rocks.h"
#include "images/burn.h"
#include "images/bee.h"

#define BEE_TILES	193
#define BURN_TILES	(BEE_TILES + 8)

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

    sprite->x = SCREEN_X(obj->x);
    sprite->y = obj->y + ON_SCREEN - 16;

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
	small_mob_end(obj, 1);
    }
}

static void flip_bee(Object *obj) {
    animate_bee(obj);
    mob_adjust_sprite_dir(obj);
}

static void move_bee(Object *obj) {
    obj->x += obj->direction;
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
    char dx = small_circle[index + 0];
    char dy = small_circle[index + 1];
    obj->x += dx;
    obj->y += dy;
    animate_bee(obj);
    obj->x -= dx;
    obj->y -= dy;
}

static void emit_bee_circle(u16 x, u16 y, u16 offset, char dir) {
    for (u16 i = 0; i <= 96; i += 32) {
	Object *bee = setup_bee(x + SCR_WIDTH + 96, y, i + offset);
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
    emit_static_bee(x + SCR_WIDTH + 64, 186, 1);
    emit_static_bee(x + SCR_WIDTH + 80, 192, -1);

    emit_static_bee(x + SCR_WIDTH + 112, 200, 1);
    emit_static_bee(x + SCR_WIDTH + 130, 196, -1);

    emit_static_bee(x + SCR_WIDTH + 184, 178, 1);
    emit_static_bee(x + SCR_WIDTH + 202, 184, -1);
}

void load_burn_tiles(u16 where) {
    update_tiles(burn_tiles, where, ARRAY_SIZE(burn_tiles));
}

void emit_xonix_bees(u16 x) {
    emit_static_bee(x + SCR_WIDTH + 0x00, 20,  -2);
    emit_static_bee(x + SCR_WIDTH + 0x14, 216, -2);
    emit_static_bee(x + SCR_WIDTH + 0x2C, 216,  2);
    emit_static_bee(x + SCR_WIDTH + 0x40, 20,   2);
}

static void diagonal_bee(Object *obj) {
    Sprite *sprite = obj->sprite;
    obj->y += BEE(obj)->v_direction;
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
    emit_single_xonix_bee(window + SCR_WIDTH, y, (y >= 128) ? -1 : 1);
    if (left < 8) {
	callback(&emit_xonix_stream_bee, 24, left + 1);
    }
}

void emit_xonix_stream(u16 x) {
    relinquish_all_bees(0);
    emit_xonix_stream_bee(0);
}

void display_mountains(void) {
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
    prepare_mountain_level();

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
