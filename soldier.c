#include "main.h"

#include "images/soldier.h"
#include "images/flame_up.h"
#include "images/flame.h"
#include "images/blood.h"
#include "images/walk.h"
#include "images/gun.h"

#define SOLDIER_TOP	1024
#define SOLDIER_POISON	(SOLDIER_TOP + 27)
#define SOLDIER_LEG	(SOLDIER_TOP + (3 * 12))

#define FLAME		(SOLDIER_LEG + (25 * 3 * 2))
#define FLAME_UP	(FLAME + (32 * 2 * 1))
#define BLOOD		(FLAME_UP + (32 * 2 * 1))
#define WEAPON		(BLOOD + (8 * 2 * 2))

#define SOLDIER_MIN_X	(ON_SCREEN + 16)
#define SOLDIER_MAX_X	(ON_SCREEN + 128)
#define SOLDIER_AHEAD	28

#define FLAME_OFFSET	8
#define SOLDIER_BASE	5
#define BLOOD_SPRITE	3

static Object soldier;
static char is_dead;

static Sprite sprite[80];
static Sprite *blood;

extern byte first_mob_sprite;

#define BUTTON_A(x) ((x) & BIT(12))
#define BUTTON_B(x) ((x) & BIT(4))
#define BUTTON_C(x) ((x) & BIT(5))

#define BUTTON_UP(x) ((x) & BIT(0))
#define BUTTON_DOWN(x) ((x) & BIT(1))
#define BUTTON_LEFT(x) ((x) & BIT(2))
#define BUTTON_RIGHT(x) ((x) & BIT(3))

#define BUTTON_START(x) ((x) & BIT(13))

static u16 read_gamepad(void) {
    u16 button_state;
    BYTE(GAMEPAD_A_DATA) = 0;
    asm("nop");
    asm("nop");
    button_state = BYTE(GAMEPAD_A_DATA) << 8;
    BYTE(GAMEPAD_A_DATA) = BIT(6);
    asm("nop");
    asm("nop");
    button_state = button_state | BYTE(GAMEPAD_A_DATA);
    return ~button_state;
}

static Rectangle s_rect;

static void update_soldier_rectangle(void) {
    s_rect.x1 = soldier.sprite->x + 4;
    s_rect.y1 = soldier.sprite->y + 4;
    s_rect.x2 = soldier.sprite->x + 16;
    s_rect.y2 = soldier.sprite->y + 36;
}

static u16 is_soldier_on_screen(void) {
    return soldier.sprite->y >= 200 + ON_SCREEN;
}

static void should_sink(void) {
    /* if we are bitten in mid air and fall into pit, do the sinking */
    if ((is_dead == 0 || is_dead == 2) && is_soldier_on_screen()) {
	soldier.sprite->cfg = TILE(2, SOLDIER_TOP + 18);
	is_dead = 1;
    }
}

static void soldier_sprite_update(void) {
    soldier.sprite->x = soldier.x - window + SOLDIER_MIN_X;
    soldier.sprite->y = soldier.y + ON_SCREEN - 40;

    soldier.sprite[-1].x = soldier.sprite->x + 8;
    soldier.sprite[-1].y = soldier.sprite->y + 8;

    soldier.sprite[1].x = soldier.sprite->x;
    soldier.sprite[1].y = soldier.sprite->y + 24;

    soldier.sprite[2].x = soldier.sprite->x + soldier.direction * 16 + 8;
    soldier.sprite[2].y = soldier.sprite->y + 21;

    update_soldier_rectangle();
    should_sink();
}

static u16 on_ground(void) {
    static u16 when;
    static u16 what;
    if (counter != when) {
	what = get_snap(soldier.x + SOLDIER_AHEAD, soldier.y, soldier.y);
	when = counter;
    }
    return what;
}

static void initiate_jump(u16 down, char velocity) {
    soldier.gravity = 0;
    if (down && soldier.y < platform_bottom()) {
	soldier.y++;
    }
    else {
	soldier.velocity = velocity;
    }
}

static void advance_y(Object *obj, char gravity) {
    obj->y -= obj->velocity;
    if (obj->gravity == 0) {
	obj->gravity = gravity;
	obj->velocity--;
    }
    obj->gravity--;
}

u16 advance_obj(Object *obj, u16 offset, u16 gravity) {
    u16 snap, prev = obj->y;
    advance_y(obj, gravity);
    snap = get_snap(obj->x + offset, prev, obj->y);
    if (snap != 0) {
	obj->y = snap;
	obj->gravity = 0;
	obj->velocity = 0;
    }
    return snap;
}

static void soldier_jump(u16 start, u16 down) {
    if (start && on_ground()) {
	initiate_jump(down, 4);
    }
    advance_obj(&soldier, SOLDIER_AHEAD, 6);
}

static short animate_walking(short cycle, short prev) {
    if (prev == soldier.x) {
	if (cycle >= 0) {
	    cycle = (cycle < 6) ? -1 : -2; /* stop walking frame */
	}
    }
    else {
	if (cycle < 0) {
	    cycle = (cycle == -1) ? 2 : 8; /* start walking frame */
	}
	if ((soldier.x & 3) == 1) {
	    if (cycle == 11) cycle = 0; else cycle++;
	}
    }
    return cycle;
}

static void select_torso(u16 aim_up) {
    if (aim_up) {
	soldier.sprite[0].cfg = TILE(2, SOLDIER_TOP + 9);
	soldier.sprite[2].cfg = TILE(2, 0);
    }
    else {
	soldier.sprite[0].cfg = TILE(2, SOLDIER_TOP);
	soldier.sprite[2].cfg = TILE(2, WEAPON + 4);
    }
}

static void soldier_flip_sprites(void) {
    if (soldier.direction < 0) {
	for (char i = -1; i <= 2; i++) {
	    soldier.sprite[i].cfg |= BIT(11);
	}
    }
}

static void soldier_yelling(byte state);
static void soldier_animate(short prev, u16 aim_up, u16 fire) {
    static short cycle;
    u16 soldier_frame;

    soldier_yelling(fire);
    select_torso(aim_up);
    if (on_ground()) {
	cycle = animate_walking(cycle, prev);
	soldier_frame = aim_up ? 18 + cycle : cycle + 2;
    }
    else {
	soldier_frame = (cycle >= 6 || cycle == -2) ? 14 : 15;
	if (aim_up) soldier_frame += 9;
    }
    soldier_frame = SOLDIER_LEG + 6 * soldier_frame;
    soldier.sprite[1].cfg = TILE(2, soldier_frame);
    soldier_flip_sprites();
}

#define FLAME_COUNT	8
#define FLAME_LIFE	64

typedef struct Flame {
    Object obj;
    Pos emit;
} Flame;

static u16 cooldown;
static u16 head, tail;
static Flame flame[FLAME_COUNT];
const byte decople_table[FLAME_LIFE];

static u16 next_flame(u16 index) {
    return (index + 1) & (FLAME_COUNT - 1);
}

#define FIRE_FRAME(x) TILE(2, FLAME + (2 * (x)))

static short clamp(short value, short max) {
    if (value > max) {
	return max;
    }
    else if (value < -max) {
	return -max;
    }
    else {
	return value;
    }
}

static void update_flame_sprite(Object *f) {
    Pos *p = &CONTAINER_OF(f, Flame, obj)->emit;
    byte decople = decople_table[f->life];
    u16 animation = f->life & ((FLAME_LIFE - 1) << 1);
    f->sprite->cfg = TILE(2, f->frame + animation);

    f->sprite->x = soldier.sprite->x + (f->x >> 4)
	+ clamp(p->x - soldier.sprite->x, decople >> 1);
    f->sprite->y = soldier.sprite->y + (f->y >> 4)
	+ clamp(p->y - soldier.sprite->y, decople);

    if (f->life > 48) {
	p->y += clamp(soldier.sprite->y - p->y, 1);
    }
    else {
	p->y = (7 * p->y + soldier.sprite->y) >> 3;
    }
    if (f->sprite->x > SCR_WIDTH + ON_SCREEN) {
	f->sprite->x = f->sprite->y = 1;
    }
}

static void emit_flame(u16 index, u16 aim_up) {
    Object *f = &flame[index].obj;
    u16 offset_y, offset_x;
    if (!aim_up) {
	offset_x = 4 + 22 * soldier.direction;
	offset_y = 20;
	f->velocity = 0;
	f->frame = FLAME;
    }
    else {
	offset_x = 4 + 16 * soldier.direction;
	offset_y = 3;
	f->velocity = 16;
	f->frame = FLAME_UP;
    }

    Pos *p = &flame[index].emit;
    p->x = soldier.sprite->x;
    p->y = soldier.sprite->y;
    f->direction = soldier.direction;
    f->x = offset_x << 4;
    f->y = offset_y << 4;
    f->gravity = 4;
    f->life = 0;

    if (soldier.direction < 0) {
	f->frame |= BIT(11);
    }

    f->sprite->size = SPRITE_SIZE(2, 1);
    update_flame_sprite(f);
}

static char is_good_flame(u16 index) {
    return is_good_object(&flame[index].obj);
}

static Sprite *flame_sprite(u16 i) {
    return flame[i].obj.sprite;
}

static void throw_flames(u16 aim_up) {
    if (!is_good_flame(head)) {
	emit_flame(head, aim_up);
	head = next_flame(head);
    }
}

static void remove_oldest_flame(void) {
    if (is_good_flame(head)) {
	destroy_object(&flame[tail].obj);
	tail = next_flame(tail);
    }
}

static u16 flame_expired(Object *f) {
    return (++f->life) >= FLAME_LIFE;
}

static char is_horizontal_flame(Object *f) {
    return TILE_ID(f->frame) == FLAME;
}

static byte button_down;
static void advance_flame(Object *f) {
    char move_x, move_y;
    if (is_horizontal_flame(f)) {
	move_x = button_down ? 11 : 22;
	move_y = 8;
    }
    else {
	move_x = 16;
	move_y = 4;
    }
    f->x += move_x * f->direction;
    advance_y(f, move_y);
}

static Rectangle f_rect;
static void clear_rectangle(Rectangle *r) {
    r->x1 = r->x2 = r->y1 = r->y2 = 0;
}

static u16 intersect_segment(u16 a1, u16 a2, u16 b1, u16 b2) {
    return !(a2 < b1 || b2 < a1);
}

static u16 intersect(Rectangle *r1, Rectangle *r2) {
    return intersect_segment(r1->x1, r1->x2, r2->x1, r2->x2)
	&& intersect_segment(r1->y1, r1->y2, r2->y1, r2->y2);
}

static void update_total_rectange(u16 index) {
    u16 x = flame_sprite(index)->x;
    u16 y = flame_sprite(index)->y;
    if (index == tail) {
	f_rect.x1 = x;
	f_rect.y1 = y;
	f_rect.x2 = f_rect.x1;
	f_rect.y2 = f_rect.y1;
    }
    else {
	if (x < f_rect.x1) f_rect.x1 = x;
	if (x > f_rect.x2) f_rect.x2 = x;
	if (y < f_rect.y1) f_rect.y1 = y;
	if (y > f_rect.y2) f_rect.y2 = y;
    }
}

static byte after_flame(void) {
    return blood->x > 0 ? BLOOD_SPRITE : first_mob_sprite;
}

static void manage_flames(void) {
    u16 index = tail;
    byte previous = after_flame();
    clear_rectangle(&f_rect);
    while (is_good_flame(index)) {
	Object *f = &flame[index].obj;
	if (flame_expired(f)) {
	    destroy_object(f);
	    index = next_flame(index);
	    tail = index;
	    continue;
	}
	advance_flame(f);
	update_flame_sprite(f);
	update_total_rectange(index);
	f->sprite->next = previous;
	previous = index + FLAME_OFFSET;
	index = next_flame(index);
	if (index == head) break;
    }
    /* add sprite size */
    f_rect.x2 += 16;
    f_rect.y2 += 8;
    /* start with flames */
    sprite[0].next = previous;
    if (cooldown > 0) {
	cooldown--;
    }
}

static Rectangle flame_rectangle(Rectangle *r, Object *f) {
    r->x1 = f->sprite->x + 1;
    r->x2 = r->x1 + 14;
    r->y1 = f->sprite->y + 2;
    r->y2 = r->y1 + 4;
}

u16 flame_collision(Rectangle *r) {
    Rectangle f_single;
    if (intersect(r, &f_rect)) {
	for (u16 index = 0; index < FLAME_COUNT; index++) {
	    Object *f = &flame[index].obj;
	    if (is_good_object(f)) {
		flame_rectangle(&f_single, f);
		if (intersect(r, &f_single)) {
		    return 1;
		}
	    }
	}
    }
    return 0;
}

u16 soldier_collision(Rectangle *r) {
    return intersect(r, &s_rect);
}

static void update_color(u16 idx, u16 color) {
    UPDATE_CRAM_WORD(2 * idx, color);
    update_palette(&color, idx, 1);
}

static void flicker_color(u16 index, u16 deviate) {
    u16 color = soldier_palette[index] + deviate;
    update_color(32 + index, color);
}

static void soldier_flicker(u16 deviate) {
    flicker_color(6, deviate);
    flicker_color(7, deviate);
}

static byte face;
static void flame_noise(u16 off) {
    psg_noise(7, face && !off ? 0x4 : 0xf);
}

static void soldier_yelling(byte state) {
    soldier.sprite[-1].cfg = TILE(2, state ? WEAPON : 0);
    if (face != state) {
	soldier_flicker(0);
	face = state;
	flame_noise(0);
    }
    if (state) {
	soldier_flicker(counter & 2);
    }
}

static byte locked;
void lock_screen(byte state) {
    locked = state;
}

static void soldier_forward(void) {
    soldier.direction = 1;
    soldier.x++;
}

static void soldier_backward(void) {
    soldier.direction = -1;
    soldier.x--;
}

static void move_forward(void) {
    if (locked) {
	if (soldier.sprite->x < SCR_WIDTH + ON_SCREEN - 24) {
	    soldier_forward();
	}
    }
    else if (soldier.sprite->x >= SOLDIER_MAX_X && !is_rightmost()) {
	update_window(1);
	soldier_forward();
    }
    else if (soldier.sprite->x < SOLDIER_MAX_X) {
	soldier_forward();
    }
}

static void move_backward(void) {
    if (locked) {
	if (soldier.sprite->x > ON_SCREEN) {
	    soldier_backward();
	}
    }
    else if (soldier.sprite->x <= SOLDIER_MIN_X && !is_leftmost()) {
	update_window(-1);
	soldier_backward();
    }
    else if (soldier.sprite->x > SOLDIER_MIN_X) {
	soldier_backward();
    }
}

static byte pause;
static u16 last_state;
static u16 button_state;

static u16 pause_toggle(void) {
    return BUTTON_START(last_state) == 0 && BUTTON_START(button_state);
}

static void game_paused(void) {
    upload_palette(pause << 1);
    music_toggle(pause);
    flame_noise(pause);
}

void update_game(void) {
    button_state = read_gamepad();
    if (pause_toggle()) {
	pause = !pause;
	game_paused();
    }
    if (!pause) {
	manage_timers();
	advance_sprites();
	level_scroll();
    }
    last_state = button_state;
}

static void soldier_march(void) {
    short prev = soldier.x;

    u16 aim_up = 0;
    if (BUTTON_RIGHT(button_state)) {
	move_forward();
    }
    else if (BUTTON_LEFT(button_state)) {
	move_backward();
    }
    else if (BUTTON_UP(button_state)) {
	aim_up = 1;
    }
    update_height_map(soldier.x);

    u16 fire = BUTTON_B(button_state);
    if (fire && cooldown == 0) {
	throw_flames(aim_up);
	cooldown = 8;
    }

    u16 jump = BUTTON_C(button_state) && BUTTON_C(last_state) == 0;
    button_down = BUTTON_DOWN(button_state);
    soldier_jump(jump, button_down);
    soldier_animate(prev, aim_up, fire);
    soldier_sprite_update();
}

static void hide_all_sprites(void) {
    for (u16 i = 0; i < ARRAY_SIZE(sprite); i++) {
	sprite[i].x = 0;
    }
}

#define FADE_SPEED 3
static void fade_and_restart(u16 fade) {
    if (fade < 8) {
	upload_palette(fade);
	callback(&fade_and_restart, FADE_SPEED, fade + 1);
    }
    else {
	hide_all_sprites();
	restart_level();
    }
}

void fade_in(u16 fade) {
    upload_palette(fade);
    if (fade > 0) callback(&fade_in, FADE_SPEED, fade - 1);
}

void wiggle_sfx(void);
static void soldier_sinking(u16 cookie) {
    soldier.y++;
    soldier.sprite->cfg ^= BIT(11);
    soldier_sprite_update();
    schedule(&soldier_sinking, 10);
    if ((soldier.sprite->cfg >> 11) & 1) wiggle_sfx();
}

static void soldier_sink(void) {
    if (is_dead == 1) {
	schedule(&fade_and_restart, 150);
	soldier_sinking(0);
	soldier_yelling(0);
	is_dead = -1;
    }
}

static void spill_blood(u16 cookie) {
    blood->cfg += 4;
    if (blood->cfg >= TILE(2, BLOOD + (4 * 8))) {
	blood->x = blood->y = 0;
    }
    else {
	schedule(&spill_blood, 2);
	blood->x++;
	blood->y--;
    }
}

void slash_sfx(void);
static void do_bite(u16 x, u16 y) {
    slash_sfx();
    blood->x = x;
    blood->y = y;
    blood->cfg = TILE(2, BLOOD);
    blood->size = SPRITE_SIZE(2, 2);
    schedule(&spill_blood, 2);
    remove_oldest_flame();
    if (!is_dead) is_dead = 2;
}

static void manage_blood(void) {
    blood->next = first_mob_sprite;
}

void bite_soldier(u16 x, u16 y) {
    if (blood->x == 0) do_bite(x, y);
}

static void soldier_kneel(u16 cookie) {
    soldier.sprite[0].y++;
    soldier.sprite[1].cfg += 6;
    if (TILE_ID(soldier.sprite[1].cfg) < SOLDIER_LEG + 22 * 6) {
	schedule(&soldier_kneel, 6);
    }
    else {
	schedule(&fade_and_restart, 25);
    }
}

static void soldier_poison(void) {
    if (!on_ground()) {
	advance_obj(&soldier, SOLDIER_AHEAD, 6);
	soldier_sprite_update();
    }
    else if (TILE_ID(soldier.sprite[0].cfg) != SOLDIER_POISON) {
	update_color(39, 0x668);
	update_color(40, 0x446);
	soldier.sprite[-1].size = SPRITE_SIZE(4, 1);
	soldier.sprite[-1].cfg = TILE(2, WEAPON + 1);
	soldier.sprite[-1].x = soldier.sprite[0].x - 4;
	soldier.sprite[-1].y = soldier.sprite[2].y;

	soldier.sprite[0].cfg = TILE(2, SOLDIER_POISON);
	soldier.sprite[1].cfg = TILE(2, SOLDIER_LEG + 18 * 6);
	soldier.sprite[2].x = soldier.sprite[2].y = 0;
	soldier_flip_sprites();
    }
    else if (soldier.sprite[-1].y < soldier.sprite[1].y + 12) {
	soldier.sprite[-1].y++;
    }
    else {
	schedule(&soldier_kneel, 6);
	is_dead = -1;
    }
}

static void soldier_complete(void) {
    u16 prev = soldier.x;
    if (soldier.x < window + SCR_WIDTH - 16) {
	soldier_forward();
    }
    else {
	is_dead = -1;
	next_level();
	fade_and_restart(0);
    }
    advance_obj(&soldier, SOLDIER_AHEAD, 6);
    soldier_animate(prev, 0, 0);
    soldier_sprite_update();
}

void level_done(u16 x) {
    is_dead = 3;
}

void advance_sprites(void) {
    switch (is_dead) {
    case 0:
	soldier_march();
	break;
    case 1:
	soldier_sink();
	break;
    case 2:
	soldier_poison();
	break;
    case 3:
	soldier_complete();
	break;
    default:
	/* whoops */
	break;
    }

    /* manage mobs first because manage_flames uses first_mob_sprite */
    manage_mobs();
    manage_blood();
    manage_flames();

    copy_to_VRAM_ptr(VRAM_SPRITE, sizeof(sprite), sprite);
}

static void put_soldier(u16 x, u16 y) {
    soldier.sprite = get_sprite(SOLDIER_BASE);
    soldier.direction = 1;
    soldier.x = window + x;
    soldier.y = y;

    soldier.sprite[-1].cfg = TILE(2, 0);
    soldier.sprite[-1].size = SPRITE_SIZE(1, 1);
    soldier.sprite[-1].next = SOLDIER_BASE;

    soldier.sprite[0].cfg = TILE(2, SOLDIER_TOP);
    soldier.sprite[0].size = SPRITE_SIZE(3, 3);
    soldier.sprite[0].next = SOLDIER_BASE + 1;

    soldier.sprite[1].cfg = TILE(2, SOLDIER_LEG);
    soldier.sprite[1].size = SPRITE_SIZE(3, 2);
    soldier.sprite[1].next = SOLDIER_BASE + 2;

    soldier.sprite[2].cfg = TILE(2, WEAPON + 4);
    soldier.sprite[2].size = SPRITE_SIZE(1, 1);
    soldier.sprite[2].next = 0;

    soldier_sprite_update();
}

void load_soldier_tiles(void) {
    update_palette(soldier_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(walk_tiles, SOLDIER_LEG, ARRAY_SIZE(walk_tiles));
    update_tiles(soldier_tiles, SOLDIER_TOP, ARRAY_SIZE(soldier_tiles));

    update_tiles(flame_tiles, FLAME, ARRAY_SIZE(flame_tiles));
    update_tiles(flame_up_tiles, FLAME_UP, ARRAY_SIZE(flame_up_tiles));

    update_tiles(blood_tiles, BLOOD, ARRAY_SIZE(blood_tiles));
    update_tiles(gun_tiles, WEAPON, ARRAY_SIZE(gun_tiles));
}

Sprite *get_sprite(u16 offset) {
    return sprite + offset;
}

static void setup_flame_sprites(void) {
    for (u16 i = 0; i < FLAME_COUNT; i++) {
	flame[i].obj.sprite = get_sprite(FLAME_OFFSET) + i;
    }
}

void setup_soldier_sprites(void) {
    head = tail = cooldown = 0;
    blood = get_sprite(BLOOD_SPRITE);
    memset(&soldier, 0, sizeof(soldier));
    memset(sprite, 0, sizeof(sprite));
    put_soldier(0, platform_bottom());
    clear_rectangle(&f_rect);
    clear_rectangle(&s_rect);
    setup_flame_sprites();
    button_down = 0;
    is_dead = 0;
    locked = 0;
}
