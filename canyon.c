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
    return sprite->x >= MAX_POSITION
	|| sprite->x < ON_SCREEN - 16
	|| sprite->y > ON_SCREEN + SCR_HEIGHT;
}

static u16 should_hopper_burn(Sprite *sprite) {
    Rectangle r;
    r.x1 = sprite->x + 4;
    r.y1 = sprite->y + 4;
    r.x2 = sprite->x + 12;
    r.y2 = sprite->y + 12;
    return flame_collision(&r);
}

static u16 should_hopper_bite(Sprite *sprite) {
    Rectangle r;
    r.x1 = sprite->x + 2;
    r.y1 = sprite->y + 4;
    r.x2 = sprite->x + 6;
    r.y2 = sprite->y + 8;
    return soldier_collision(&r);
}

static u16 is_hopper_alive(Object *obj) {
    return obj->frame < 9;
}

static void hopper_die(Object *obj) {
    void perish_sfx(void);
    obj->frame = 9;
    obj->life = 0;
    perish_sfx();
}

static void move_hopper(Mob *mob) {
    Object *obj = &mob->obj;
    Sprite *sprite = mob->sprite;

    obj->x--;
    obj->life++;
    u16 land = advance_obj(obj, 4, 12);

    sprite->x = SCREEN_X(obj->x);
    sprite->y = obj->y + ON_SCREEN - 16;

    if (!is_hopper_alive(obj)) {
	if ((obj->life & 3) == 0) obj->frame++;
    }
    else {
	if (should_hopper_burn(sprite)) {
	    hopper_die(obj);
	}
	else {
	    if (land) {
		obj->frame = 3 + ((obj->life >> 2) % 6);
	    }
	    else {
		obj->frame = 1 + ((obj->life >> 1) & 1);
	    }
	    if (should_hopper_bite(sprite)) {
		bite_soldier(sprite->x, sprite->y - 2);
	    }
	}
    }

    sprite->cfg = TILE(2, 289 + 4 * obj->frame);

    if (is_hopper_off_screen(sprite) || obj->frame == 17) {
	free_mob(mob);
    }
}

static void jump_hopper(Mob *mob, u16(*jump_condition)(u16)) {
    Object *obj = &mob->obj;
    if (is_hopper_alive(obj) && jump_condition(obj->life)) {
	obj->velocity = 2;
    }
    move_hopper(mob);
}

static u16 is_period(u16 life) {
    return (life & 0x7F) == 0;
}

static void periodic_hopper(Mob *mob) {
    jump_hopper(mob, &is_period);
}

static u16 is_near_hole(u16 life) {
    return (life == 80 || life == 180);
}

static void hole_hopper(Mob *mob) {
    jump_hopper(mob, &is_near_hole);
}

static Mob *setup_hopper(u16 x, u16 y, u16 life) {
    Mob *mob = alloc_mob(2);
    if (mob != NULL) {
	Object *obj = &mob->obj;

	obj->x = x;
	obj->y = y;
	obj->frame = 0;
	obj->gravity = 0;
	obj->velocity = 0;
	obj->life = life;

	mob->sprite->size = SPRITE_SIZE(2, 2);
	mob->fn = &move_hopper;
    }
    return mob;
}

const byte variation[] = {
    60, 52, 42, 97, 15, 86, 65, 39
};
void emit_hopper_squad(u16 i) {
    Mob *mob = get_mob(i);
    if (mob == NULL || mob->sprite->x <= SCR_WIDTH + ON_SCREEN - 16) {
	mob = setup_hopper(window + SCR_WIDTH, 208, variation[i & 7]);
    }
    if (mob) {
	mob->fn = &periodic_hopper;
	callback(&emit_hopper_squad, 0, mob->index);
    }
}

void emit_hole_hoppers(u16 pos_x) {
    u16 x = pos_x + SCR_WIDTH + 16;
    u16 y = SCR_HEIGHT + 16;
    Mob *mob = setup_hopper(x, y, 0);
    if (mob != NULL) {
	mob->obj.velocity = 4;
	mob->fn = &hole_hopper;
    }
    if (window < pos_x + SCR_WIDTH - 96) {
	callback(&emit_hole_hoppers, mob ? 24 : 0, pos_x);
    }
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
    switch_frame(&update_game);
}
