#include "main.h"

#include "images/canyon.h"
#include "images/desert.h"
#include "images/cliffs.h"
#include "images/hopper.h"

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

static void draw_vegetation(void) {
    u16 i, offset = 0x700, tile = 0;
    for (i = 0; i < ARRAY_SIZE(cacti_spacing); i++) {
	offset += cacti_spacing[i];
	u16 tile = cacti[(tile + offset) & 7];
	if (i == 60 || i == 40 || i == 20) tile = 95;
	poke_VRAM(offset, tile);
	tile++;
    }
}

const byte sand[] = { 72, 80, 88, 71, 79, 87, 71, 72 };

static void draw_sand(void) {
    u16 i;
    set_seed(1);
    for (i = 0x700; i < 0xf00; i += 2) {
	poke_VRAM(i, sand[random() & 7]);
    }
}

static u16 is_hopper_off_screen(Sprite *sprite) {
    return sprite->x < ON_SCREEN - 16
	|| sprite->y > ON_SCREEN + 224;
}

static void advance_obj(Object *obj) {
    u16 snap, prev = obj->y;
    advance_y(obj, 6);
    snap = get_snap(obj->x, prev, obj->y);
    if (snap != 0) {
	obj->y = snap;
	obj->gravity = 0;
	obj->velocity = 0;
    }
}

static void hopper(Mob *mob) {
    Object *obj = &mob->obj;
    Sprite *sprite = mob->sprite;
    u16 frame = 4 * (3 + ((obj->life >> 2) % 6));
    sprite->cfg = TILE(2, 289 + frame);
    obj->life++;

    obj->x--;
    advance_obj(obj);

    sprite->x = obj->x - window + ON_SCREEN - 4;
    sprite->y = obj->y + ON_SCREEN - 16;
    if (is_hopper_off_screen(sprite)) {
       free_mob(mob->index);
    }
}

static void setup_hopper(Mob *mob) {
    Object *obj = &mob->obj;

    obj->life = 0;
    obj->gravity = 0;
    obj->velocity = 0;
    obj->x = window + 320;
    obj->y = 128;

    mob->sprite->size = SPRITE_SIZE(2, 2);
}

static void emit_mobs(void) {
    static u16 wait;
    if (wait++ > 48) {
	Mob *mob = alloc_mob(2, hopper);
	if (mob) setup_hopper(mob);
	wait = 0;
    }
}

static void update_canyon(void) {
    emit_mobs();
    advance_sprites();
    level_scroll();
}

void display_canyon(void) {
    /* load tiles */
    update_palette(canyon_palette, 0, ARRAY_SIZE(canyon_palette));
    update_tiles(canyon_tiles, 1, ARRAY_SIZE(canyon_tiles));

    load_soldier_tiles();
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0x000,  1, 0x300);
    fill_VRAM(0x680, 96, 0x40);

    draw_sand();
    draw_clouds();
    draw_horizon();
    draw_vegetation();

    upload_palette(4);
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    update_palette(desert_palette, 16, ARRAY_SIZE(desert_palette));
    update_tiles(desert_tiles, 97, ARRAY_SIZE(desert_tiles));
    update_tiles(cliffs_tiles, 161, ARRAY_SIZE(cliffs_tiles));

    clear_DMA_buffer(0, 0x1000);

    prepare_desert_level();

    upload_palette(2);
    fill_VRAM(0xe00, TILE(1, 1) | BIT(15), 0x80);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_johnny(void);
    music_johnny();

    update_tiles(hopper_tiles, 289, ARRAY_SIZE(hopper_tiles));

    upload_palette(0);
    switch_frame(&update_canyon);
}
