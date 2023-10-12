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

#define FLAME		(SOLDIER_LEG + (37 * 3 * 2))
#define FLAME_UP	(FLAME + (32 * 2 * 1))
#define BLOOD		(FLAME_UP + (32 * 2 * 1))
#define WEAPON		(BLOOD + (8 * 2 * 2))

#define SOLDIER_MIN_X	(ON_SCREEN + 16)
#define SOLDIER_MAX_X	(ON_SCREEN + 128)
#define SOLDIER_AHEAD	28

#define FLAME_OFFSET	8
#define BLOOD_SPRITE	3
#define FLAME_DECOPLE	24

static Object soldier;
static char is_dead;

static Sprite sprite[80];
static Sprite *blood;

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
	soldier_frame = cycle + 2;
    }
    else {
	soldier_frame = (cycle >= 6 || cycle == -2) ? 14 : 15;
    }
    if (aim_up) soldier_frame += 16;
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
static char available_flames;
static Flame flame[FLAME_COUNT];
static char free_flames[FLAME_COUNT];
extern const byte decople_table[FLAME_LIFE];

#define FIRE_FRAME(x) TILE(2, FLAME + (2 * (x)))

static void update_flame_sprite(Object *f) {
    Pos *p = &CONTAINER_OF(f, Flame, obj)->emit;
    u16 animation = f->life & ((FLAME_LIFE - 1) << 1);
    f->sprite->cfg = TILE(2, f->frame + animation);

    f->sprite->x = SCREEN_X(soldier.x + (f->x >> 4));
    f->sprite->y = (f->y >> 4);
    if (f->life < FLAME_DECOPLE) {
	f->sprite->y += soldier.sprite->y;
    }
    else if (f->life == FLAME_DECOPLE) {
	f->y += (soldier.sprite->y << 4);
    }

    if (f->sprite->x > SCR_WIDTH + ON_SCREEN) {
	f->sprite->x = f->sprite->y = 1;
    }
}

static void emit_flame(u16 index, u16 aim_up) {
    Object *f = &flame[index].obj;
    u16 offset_y, offset_x;
    if (!aim_up) {
	offset_x = 20 + 22 * soldier.direction;
	offset_y = 20;
	f->velocity = 0;
	f->frame = FLAME;
    }
    else {
	offset_x = 20 + 16 * soldier.direction;
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
    f->place = available_flames;
    update_flame_sprite(f);
}

static void remove_flame(Object *obj) {
    char index = free_flames[obj->place];
    char other = free_flames[available_flames];
    free_flames[available_flames++] = index;
    free_flames[obj->place] = other;
    flame[other].obj.place = obj->place;
}

static void throw_flames(u16 aim_up) {
    if (available_flames > 0) {
	emit_flame(free_flames[--available_flames], aim_up);
    }
}

static void remove_one_flame(void) {
    if (available_flames == 0) {
	remove_flame(&flame[free_flames[0]].obj);
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

static void update_total_rectange(Object *f) {
    u16 x = f->sprite->x;
    u16 y = f->sprite->y;
    if (f_rect.x1 == 0 && f_rect.y1 == 0) {
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

static void remove_old_flames(void) {
    for (char i = available_flames; i < FLAME_COUNT; i++) {
	u16 index = free_flames[i];
	Object *f = &flame[index].obj;
	if (flame_expired(f)) {
	    remove_flame(f);
	}
    }
}

byte update_next_sprite(byte new_value) {
    static byte next_sprite;
    byte old_value = next_sprite;
    next_sprite = new_value;
    return old_value;
}

static void manage_flames(void) {
    remove_old_flames();
    clear_rectangle(&f_rect);
    for (char i = available_flames; i < FLAME_COUNT; i++) {
	u16 index = free_flames[i];
	Object *f = &flame[index].obj;
	advance_flame(f);
	update_flame_sprite(f);
	update_total_rectange(f);
	f->sprite->next = update_next_sprite(index + FLAME_OFFSET);
    }
    /* add sprite size */
    f_rect.x2 += 16;
    f_rect.y2 += 8;
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

Object *flame_collision(Rectangle *r) {
    Rectangle f_single;
    if (intersect(r, &f_rect)) {
	for (byte i = available_flames; i < FLAME_COUNT; i++) {
	    u16 index = free_flames[i];
	    Object *f = &flame[index].obj;
	    flame_rectangle(&f_single, f);
	    if (intersect(r, &f_single)) {
		remove_flame(f);
		return f;
	    }
	}
    }
    return NULL;
}

u16 soldier_collision(Rectangle *r) {
    return intersect(r, &s_rect);
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

#define JUST_PRESS(button) \
    (BUTTON_##button(button_state) && BUTTON_##button(last_state) == 0)

static u16 pause_toggle(void) {
    return JUST_PRESS(START);
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
    if (BUTTON_UP(button_state)) {
	aim_up = 1;
    }
    update_height_map(soldier.x);

    u16 fire = BUTTON_B(button_state);
    if (fire && cooldown == 0) {
	throw_flames(aim_up);
	cooldown = 8;
    }

    button_down = BUTTON_DOWN(button_state);
    soldier_jump(JUST_PRESS(C), button_down);
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

void fade_to_next_level(void) {
    fade_and_restart(0);
    next_level();
}

static byte started;
static void wait_for_start_loop(void) {
    manage_timers();
    if (!started && BUTTON_START(read_gamepad())) {
	fade_to_next_level();
	started = 1;
    }
}

void wait_for_start(void) {
    started = 0;
    reset_mobs();
    callback(&fade_in, 0, 6);
    switch_frame(&wait_for_start_loop);
}

#define BAR_OFFSET (16 + 40 - (BAR_SIZE + 2))

static u16 progress;
void display_progress_bar(void) {
    poke_VRAM(0, TILE(2, WEAPON + 5));
    fill_VRAM(2, TILE(2, WEAPON + 6), BAR_SIZE);
    poke_VRAM((BAR_SIZE + 1) << 1, TILE(2, WEAPON + 15));
    copy_to_VRAM(VRAM_PLANE_A + BAR_OFFSET, (BAR_SIZE + 2) << 1);
    progress = BAR_SIZE << 3;
}

u16 decrement_progress_bar(void) {
    if (progress > 0) progress--;
    u16 tile = TILE(2, WEAPON + 14 - (progress & 7));
    u16 offset = ((progress >> 2) & ~1) + 2;
    UPDATE_VRAM_WORD(VRAM_PLANE_A + BAR_OFFSET + offset, tile);
    return progress;
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
    remove_one_flame();
    if (!is_dead) is_dead = 2;
}

static void manage_blood(void) {
    blood->next = update_next_sprite(BLOOD_SPRITE);
}

void bite_soldier(u16 x, u16 y) {
    if (blood->x == 0) do_bite(x, y);
}

static void soldier_kneel(u16 cookie) {
    soldier.sprite[0].y++;
    soldier.sprite[1].cfg += 6;
    if (TILE_ID(soldier.sprite[1].cfg) < SOLDIER_LEG + 36 * 6) {
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
	soldier.sprite[1].cfg = TILE(2, SOLDIER_LEG + 32 * 6);
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
	fade_to_next_level();
    }
    advance_obj(&soldier, SOLDIER_AHEAD, 6);
    soldier_animate(prev, 0, 0);
    soldier_sprite_update();
}

void level_done(u16 x) {
    is_dead = 3;
}



void advance_sprites(void) {
    update_next_sprite(SOLDIER_BASE - 1);

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

    sprite[0].next = update_next_sprite(0);

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
    cooldown = 0;
    available_flames = FLAME_COUNT;
    for (u16 i = 0; i < FLAME_COUNT; i++) {
	flame[i].obj.sprite = get_sprite(FLAME_OFFSET) + i;
	free_flames[i] = i;
    }
}

void reset_sprite_table(void) {
    memset(sprite, 0, sizeof(sprite));
    copy_to_VRAM_ptr(VRAM_SPRITE, sizeof(sprite), sprite);
}

void setup_soldier_sprites(void) {
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
