#include "main.h"

#include "images/alps.h"
#include "images/rocks.h"
#include "images/burn.h"
#include "images/bee.h"

#define BURN_TILES	129
#define BEE_TILES	(BURN_TILES + 32)

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

static void burn_bee(Object *obj) {
    obj->life = 0;
    perish_sfx();
}

static void move_bee(Object *obj) {
    Sprite *sprite = obj->sprite;

    obj->life++;
    obj->x += obj->direction;

    sprite->x = SCREEN_X(obj->x);
    sprite->y = obj->y + ON_SCREEN - 16;
    obj->frame = ((obj->life >> 1) & 1);

    if (should_small_mob_burn(sprite)) {
	burn_bee(obj);
    }
    else if (should_small_mob_bite(sprite, obj->direction)) {
	u16 offset = 8 * (obj->direction + 1);
	bite_soldier(sprite->x + offset, sprite->y - 2);
    }

    sprite->cfg = TILE(3, BEE_TILES + 4 * obj->frame);
    if (obj->direction > 0) sprite->cfg |= BIT(11);

    if (is_small_mob_off_screen(sprite) || obj->life == 0) {
	free_mob(obj);
    }
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
	obj->sprite->size = SPRITE_SIZE(2, 2);
	mob_fn(obj, &move_bee);
    }
    return obj;
}

void emit_bee_stream(u16 x) {
    setup_bee(window + SCR_WIDTH, 200, 0);
}

void display_mountains(void) {
    /* load tiles */
    update_palette(alps_palette, 0, ARRAY_SIZE(alps_palette));
    update_tiles(alps_tiles, 1, ARRAY_SIZE(alps_tiles));

    load_soldier_tiles(1);
    reset_window();
    reset_mobs();

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
    update_tiles(burn_tiles, BURN_TILES, ARRAY_SIZE(burn_tiles));

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}
