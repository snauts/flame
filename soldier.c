#include "main.h"

#include "images/soldier.h"
#include "images/hiroshi.h"
#include "images/french.h"
#include "images/flame_up.h"
#include "images/flame.h"
#include "images/blood.h"
#include "images/walk.h"
#include "images/hans.h"
#include "images/gun.h"

#define SOLDIER_TOP	1024
#define SOLDIER_POISON	(SOLDIER_TOP + 27)
#define SOLDIER_LEG	(SOLDIER_TOP + (6 * 3 * 3))

#define FLAME		(SOLDIER_LEG + (39 * 3 * 2))
#define FLAME_UP	(FLAME + (32 * 2 * 1))
#define BLOOD		(FLAME_UP + (32 * 2 * 1))
#define WEAPON		(BLOOD + (8 * 2 * 2))

#define SOLDIER_MIN_X	(ON_SCREEN + 16)
#define SOLDIER_MAX_X	(ON_SCREEN + 128)
#define SOLDIER_AHEAD	28

#define SOLDIER_BASE	5
#define FLAME_OFFSET	8
#define BLOOD_SPRITE	3
#define FLAME_DECOPLE	24

enum { S_MARCH = 0, S_SINKING, S_POISON, S_COMPLETE, S_FIST_PUMP };

static Object soldier;

Object *get_soldier(void) {
    return &soldier;
}

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
    s_rect.x1 = soldier.sprite->x + 8;
    s_rect.y1 = soldier.sprite->y + 4;
    s_rect.x2 = soldier.sprite->x + 16;
    s_rect.y2 = soldier.sprite->y + 36;
}

static u16 is_soldier_off_screen(void) {
    return soldier.sprite->y >= 200 + ON_SCREEN;
}

static u16 is_sinkable_state(void) {
    return soldier.life == S_MARCH
	|| soldier.life == S_POISON
	|| soldier.life == S_FIST_PUMP;
}

static void should_sink(void) {
    /* if we are bitten in mid air and fall into pit, do the sinking */
    if (is_soldier_off_screen() && is_sinkable_state()) {
	soldier.sprite->cfg = TILE(2, SOLDIER_TOP + 18);
	soldier.life = S_SINKING;
    }
}

static void soldier_sprite_update(byte offset) {
    Sprite *sprite = soldier.sprite;
    sprite->x = soldier.x - window + SOLDIER_MIN_X;
    sprite->y = soldier.y + ON_SCREEN - 40 + offset;

    sprite[-1].x = sprite->x + 8;
    sprite[-1].y = sprite->y + 8;

    sprite[1].x = sprite->x;
    sprite[1].y = sprite->y + 24;

    sprite[2].x = sprite->x + soldier.direction * 16 + 8;
    sprite[2].y = sprite->y + 21;

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

void advance_y(Object *obj, char gravity) {
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
	for (signed char i = -1; i <= 2; i++) {
	    soldier.sprite[i].cfg |= BIT(11);
	}
    }
}

static void soldier_yelling(byte state);
static void soldier_animate(short prev, u16 aim_up, u16 fire, byte crouch) {
    static short cycle;
    u16 soldier_frame;
    byte side = (cycle >= 6 || cycle == -2);

    soldier_yelling(fire);
    select_torso(aim_up);
    if (crouch) {
	soldier_frame = side ? 37 : 38;
    }
    else if (!on_ground()) {
	soldier_frame = side ? 14 : 15;
    }
    else {
	cycle = animate_walking(cycle, prev);
	soldier_frame = cycle + 2;
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
    byte off;
    Pos emit;
} Flame;

static u16 cooldown;
static signed char available_flames;
static Flame flame[FLAME_COUNT];
static signed char free_flames[FLAME_COUNT];
extern const byte decople_table[FLAME_LIFE];

#define FIRE_FRAME(x) TILE(2, FLAME + (2 * (x)))

static inline void update_flame_pos(Object *f) {
    f->sprite->x = (f->x >> 4);
    f->sprite->y = (f->y >> 4);
}

static void update_flame_sprite(Object *f) {
    byte decople = decople_table[f->life];
    Flame *this = CONTAINER_OF(f, Flame, obj);
    u16 animation = f->life & ((FLAME_LIFE - 1) << 1);
    f->sprite->cfg = TILE(2, f->frame + animation);

    u16 sx = soldier.sprite->x;
    u16 sy = soldier.sprite->y;

    u16 dx = sx + clamp(this->emit.x - sx, decople >> 1);
    u16 dy = sy + clamp(this->emit.y - sy, decople);

    update_flame_pos(f);

    if (!this->off) {
	if (f->direction != soldier.direction) {
	    this->off = 1;
	    f->x += (dx << 4);
	    if (f->life < FLAME_DECOPLE) f->y += (dy << 4);

	    update_flame_pos(f);
	}
	else {
	    f->sprite->x += dx;
	    if (f->life < FLAME_DECOPLE) {
		f->sprite->y += dy;
	    }
	    else if (f->life == FLAME_DECOPLE) {
		f->y += (dy << 4);
	    }
	}
    }

    if (f->sprite->x > SCR_WIDTH + ON_SCREEN) {
	hide_sprite(f->sprite);
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
    flame[index].off = 0;

    f->direction = soldier.direction;
    f->velocity -= soldier.velocity;
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
    signed char index = free_flames[obj->place];
    signed char other = free_flames[available_flames];
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

static void advance_flame(Object *f) {
    char move_x, move_y;
    if (is_horizontal_flame(f)) {
	move_x = 22;
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
    for (signed char i = available_flames; i < FLAME_COUNT; i++) {
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
    for (signed char i = available_flames; i < FLAME_COUNT; i++) {
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

static void flame_rectangle(Rectangle *r, Object *f) {
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

static const u16 *palette;
static void flicker_color(u16 index, u16 deviate) {
    u16 color = palette[index] + deviate;
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

static byte yell_face;
static void soldier_yelling(byte state) {
    soldier.sprite[-1].cfg = TILE(2, state ? WEAPON + yell_face : 0);
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

void fill_bottom_row(void) {
    fill_VRAM(0xe00, TILE(3, WEAPON + 19) | BIT(15), 0x80);
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

byte update_frame(void) {
    button_state = read_gamepad();
    if (pause_toggle()) {
	pause = !pause;
	game_paused();
    }
    if (!pause) {
	manage_timers();
	advance_sprites();
    }
    last_state = button_state;
    return !pause;
}

void update_game(void) {
    if (update_frame()) {
	level_scroll();
    }
}

static inline u16 only_down(void) {
    return (button_state & 0xF) == BIT(1);
}

static u16 last_pressed = 0;
static void get_last_pressed_button() {
    u16 just = button_state & ~last_state;
    if (just != 0) last_pressed = just;
    last_pressed &= button_state & 0xF;
}

static void soldier_march(void) {
    short prev = soldier.x;
    u16 down, crouch, aim_up = 0;
    down = BUTTON_DOWN(button_state);

    get_last_pressed_button();

    crouch = on_ground() && (only_down() || BUTTON_DOWN(last_pressed));

    if (!crouch) {
	aim_up = BUTTON_UP(button_state);

	if (BUTTON_LEFT(button_state)) {
	    move_backward();
	}
	if (BUTTON_RIGHT(button_state)) {
	    move_forward();
	}
    }

    update_height_map(soldier.x + SOLDIER_AHEAD);

    u16 fire = BUTTON_B(button_state);
    if (fire && cooldown == 0) {
	throw_flames(aim_up);
	cooldown = 8;
    }

    soldier_jump(JUST_PRESS(C), down);
    soldier_animate(prev, aim_up, fire, crouch);
    soldier_sprite_update(crouch ? 4 : 0);
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

void finish_level(u16 i) {
    if (soldier.life == S_FIST_PUMP) fade_to_next_level();
}

static byte started;
static void wait_for_start_loop(void) {
    manage_timers();
    if (!started && BUTTON_START(read_gamepad())) {
	fade_to_next_level();
	fade_music(0);
	started = 1;
    }
}

void wait_for_start(byte disable_start_button) {
    reset_mobs();
    callback(&fade_in, 0, 6);
    switch_frame(&wait_for_start_loop);
    started = disable_start_button;
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
    soldier_sprite_update(0);
    schedule(&soldier_sinking, 10);
    if ((soldier.sprite->cfg >> 11) & 1) wiggle_sfx();
}

static void soldier_sink(void) {
    if (soldier.life == S_SINKING) {
	schedule(&fade_and_restart, 150);
	soldier_sinking(0);
	soldier_yelling(0);
	soldier.life = -1;
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
    if (!soldier.life) soldier.life = S_POISON;
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
    if (get_palette_color(40) != 0x446) {
	update_color(39, 0x668);
	update_color(40, 0x446);
    }
    if (!on_ground()) {
	advance_obj(&soldier, SOLDIER_AHEAD, 6);
	soldier_sprite_update(0);
    }
    else if (TILE_ID(soldier.sprite[0].cfg) != SOLDIER_POISON) {
	soldier_sprite_update(0);
	soldier.sprite[-1].size = SPRITE_SIZE(4, 1);
	soldier.sprite[-1].cfg = TILE(2, WEAPON + 1);
	soldier.sprite[-1].x = soldier.sprite[0].x - 4;
	soldier.sprite[-1].y = soldier.sprite[2].y;

	soldier.sprite[0].cfg = TILE(2, SOLDIER_POISON);
	soldier.sprite[1].cfg = TILE(2, SOLDIER_LEG + 32 * 6);
	soldier.sprite[2].x = soldier.sprite[2].y = 0;
	soldier_flip_sprites();
	flame_noise(1);
    }
    else if (soldier.sprite[-1].y < soldier.sprite[1].y + 12) {
	soldier.sprite[-1].y++;
    }
    else {
	schedule(&soldier_kneel, 6);
	soldier.life = -1;
    }
}

static void soldier_update(u16 prev, u16 aim_up) {
    advance_obj(&soldier, SOLDIER_AHEAD, 6);
    soldier_animate(prev, aim_up, 0, 0);
    soldier_sprite_update(0);
}

static void soldier_complete(void) {
    u16 prev = soldier.x;
    if (soldier.x < window + SCR_WIDTH - 16) {
	soldier_forward();
    }
    else {
	soldier.life = -1;
	fade_to_next_level();
    }
    soldier_update(prev, 0);
}

void level_done(u16 x) {
    soldier.life = S_COMPLETE;
}

void set_sprite_tile(Sprite *sprite, u16 tile) {
    sprite->cfg = (sprite->cfg & ~0x67FF) | tile;
}

void soldier_fist_pump() {
    flame_noise(1);
    soldier.life = S_FIST_PUMP;
    soldier_update(soldier.x, 1);
    if (soldier.life != S_SINKING) {
	u16 frame = 36 + 9 * ((soldier.frame >> 4) & 1);
	set_sprite_tile(soldier.sprite, TILE(2, SOLDIER_TOP + frame));
	set_sprite_tile(soldier.sprite - 1, TILE(2, WEAPON + yell_face));
	soldier.frame++;
    }
}

static void upload_sprite_data(void) {
    sprite[0].next = update_next_sprite(0);
    copy_to_VRAM_ptr(VRAM_SPRITE, sizeof(sprite), sprite);
}

void advance_sprites(void) {
    update_next_sprite(SOLDIER_BASE - 1);

    switch (soldier.life) {
    case S_MARCH:
	soldier_march();
	break;
    case S_SINKING:
	soldier_sink();
	break;
    case S_POISON:
	soldier_poison();
	break;
    case S_COMPLETE:
	soldier_complete();
	break;
    case S_FIST_PUMP:
	soldier_fist_pump();
	break;
    default:
	/* whoops */
	break;
    }

    /* manage mobs first because manage_flames uses first_mob_sprite */
    manage_mobs();
    manage_blood();
    manage_flames();
    upload_sprite_data();
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

    soldier_sprite_update(0);
}

static const byte yell_map[] = { 0, 16, 17, 18 };
static void load_soldier_tiles_at_offset(u16 id, u16 offset) {
    switch (id) {
    case 0:
	UPDATE_TILES(soldier_tiles, offset);
	palette = soldier_palette;
	break;
    case 1:
	UPDATE_TILES(hans_tiles, offset);
	palette = hans_palette;
	break;
    case 2:
	UPDATE_TILES(hiroshi_tiles, offset);
	palette = hiroshi_palette;
	break;
    case 3:
	UPDATE_TILES(french_tiles, offset);
	palette = french_palette;
	break;
    }

    yell_face = yell_map[id];

    update_palette(palette, 32, ARRAY_SIZE(soldier_palette));
    update_tiles(walk_tiles, SOLDIER_LEG, ARRAY_SIZE(walk_tiles));

    update_tiles(flame_tiles, FLAME, ARRAY_SIZE(flame_tiles));
    update_tiles(flame_up_tiles, FLAME_UP, ARRAY_SIZE(flame_up_tiles));

    update_tiles(blood_tiles, BLOOD, ARRAY_SIZE(blood_tiles));
    update_tiles(gun_tiles, WEAPON, ARRAY_SIZE(gun_tiles));
}

void load_soldier_tiles(u16 id) {
    load_soldier_tiles_at_offset(id, SOLDIER_TOP);
}

static void march(u16 n) {
    Sprite *army = get_sprite(SOLDIER_BASE);
    army[11].next = update_next_sprite(SOLDIER_BASE);

    u16 id;
    for (id = 2; id < 12; id += 3) {
	u16 color = (army[id].cfg >> 13) & 3;
	army[id].cfg = TILE(color, SOLDIER_LEG + (18 + n) * 6);
    }

    upload_sprite_data();
    callback(&march, 4, n < 11 ? n + 1 : 0);
}

void all_soldiers_march(void) {
    Sprite *sprite;
    update_palette(soldier_palette, 16, ARRAY_SIZE(soldier_palette));

    u16 id, offset = SOLDIER_BASE;
    for (id = 0; id < 4; id++) {
	u16 top = 64 + (id << 6);
	u16 color = 1 + (id & 1);
	load_soldier_tiles_at_offset(id, top);

	sprite = get_sprite(offset);
	sprite->x = ON_SCREEN + 124 + 8 + id * 16;
	sprite->y = ON_SCREEN + 128 + 8;
	sprite->cfg = TILE(color, 0);
	sprite->size = SPRITE_SIZE(1, 1);
	sprite->next = ++offset;

	sprite = get_sprite(offset);
	sprite->x = sprite[-1].x - 8;
	sprite->y = sprite[-1].y - 8;
	sprite->cfg = TILE(color, top + 9);
	sprite->size = SPRITE_SIZE(3, 3);
	sprite->next = ++offset;

	sprite = get_sprite(offset);
	sprite->x = sprite[-1].x;
	sprite->y = sprite[-1].y + 24;
	sprite->cfg = TILE(color, SOLDIER_LEG + 18 * 6);
	sprite->size = SPRITE_SIZE(3, 2);
	sprite->next = ++offset;
    }
    march(0);
}

void soldiers_sing(int sing) {
    for (u16 id = 0; id < 4; id++) {
	Sprite *face = get_sprite(SOLDIER_BASE + id * 3);
	face->cfg = (face->cfg & ~0x7FF) | (sing ? WEAPON + yell_map[id] : 0);
    }
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
    soldier.life = S_MARCH;
    last_pressed = 0;
    locked = 0;
    face = 0;
}
