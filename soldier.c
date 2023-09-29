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

#define FLAME		(SOLDIER_LEG + (23 * 3 * 2))
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
static Sprite *base;
static Sprite *blood;

extern byte first_mob_sprite;

#define BUTTON_A(x) ((x) & BIT(12))
#define BUTTON_B(x) ((x) & BIT(4))
#define BUTTON_C(x) ((x) & BIT(5))

#define BUTTON_UP(x) ((x) & BIT(0))
#define BUTTON_DOWN(x) ((x) & BIT(1))
#define BUTTON_LEFT(x) ((x) & BIT(2))
#define BUTTON_RIGHT(x) ((x) & BIT(3))

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
    s_rect.x1 = base->x + 4;
    s_rect.y1 = base->y + 4;
    s_rect.x2 = base->x + 16;
    s_rect.y2 = base->y + 36;
}

static void should_sink(void) {
    /* if we are bitten in mid air and fall into pit, do the sinking */
    if ((is_dead == 0 || is_dead == 2) && base->y >= 200 + ON_SCREEN) {
	base->cfg = TILE(2, SOLDIER_TOP + 18);
	is_dead = 1;
    }
}

static void soldier_sprite_update(void) {
    base->x = soldier.x - window + SOLDIER_MIN_X;
    base->y = soldier.y + ON_SCREEN - 40;

    base[-1].x = base->x + 8;
    base[-1].y = base->y + 8;

    base[1].x = base->x;
    base[1].y = base->y + 24;

    base[2].x = base->x + 24;
    base[2].y = base->y + 21;

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
    if (!down) {
	soldier.velocity = velocity;
    }
    else if (soldier.y < platform_bottom()) {
	soldier.y++;
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

static short animate_walking(short cycle, u16 prev) {
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
	    if (soldier.x < prev) {
		if (cycle == 0) cycle = 11; else cycle--;
	    }
	    else {
		if (cycle == 11) cycle = 0; else cycle++;
	    }
	}
    }
    return cycle;
}

static void select_torso(u16 aim_up) {
    if (aim_up) {
	base[0].cfg = TILE(2, SOLDIER_TOP + 9);
	base[2].cfg = TILE(2, 0);
    }
    else {
	base[0].cfg = TILE(2, SOLDIER_TOP);
	base[2].cfg = TILE(2, WEAPON + 4);
    }
}

static void soldier_animate(u16 prev, u16 aim_up) {
    static short cycle;
    u16 soldier_frame;

    select_torso(aim_up);
    if (on_ground()) {
	cycle = animate_walking(cycle, prev);
	soldier_frame = aim_up ? 18 + cycle : cycle + 2;
    }
    else {
	soldier_frame = (cycle >= 6 || cycle == -2) ? 14 : 15;
    }
    soldier_frame = SOLDIER_LEG + 6 * soldier_frame;
    base[1].cfg = TILE(2, soldier_frame);
}

static Sprite *flame;
static u16 head, tail;
static u16 cooldown;
static Object f_obj[8];

static u16 next_flame(u16 index) {
    return (index + 1) & 7;
}

#define FIRE_FRAME(x) TILE(2, FLAME + (2 * (x)))

static void update_flame_sprite(u16 index) {
    u16 animation = f_obj[index].life & 0xFE;
    flame[index].cfg = TILE(2, f_obj[index].frame + animation);
    flame[index].x = SCREEN_X(soldier.x + (f_obj[index].x >> 4));
    flame[index].y = (f_obj[index].y >> 4);
}

static void emit_flame(u16 index, u16 aim_up) {
    u16 offset_y, offset_x;
    if (!aim_up) {
	offset_x = 42;
	offset_y = 20;
	f_obj[index].velocity = 0;
	f_obj[index].frame = FLAME;
    }
    else {
	offset_x = 36;
	offset_y = 3;
	f_obj[index].velocity = 16;
	f_obj[index].frame = FLAME_UP;
    }

    u16 flame_y = base->y + offset_y;
    f_obj[index].x = offset_x << 4;
    f_obj[index].y = flame_y << 4;
    f_obj[index].gravity = 4;
    f_obj[index].life = 0;

    update_flame_sprite(index);
    flame[index].size = SPRITE_SIZE(2, 1);
}

static void throw_flames(u16 aim_up) {
    if (flame[head].x == 0) {
	emit_flame(head, aim_up);
	head = next_flame(head);
    }
}

static void remove_oldest_flame(void) {
    if (flame[head].x > 0) {
	flame[tail].x = flame[tail].y = 0;
	tail = next_flame(tail);
    }
}

static u16 flame_expired(u16 index) {
    return f_obj[index].life >= 64;
}

static void advance_flame(u16 index) {
    if (f_obj[index].frame == FLAME) {
	f_obj[index].x += 22;
	advance_y(f_obj + index, 8);
    }
    else {
	f_obj[index].x += 16;
	advance_y(f_obj + index, 4);
    }
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
    u16 x = flame[index].x;
    u16 y = flame[index].y;
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
    while (flame[index].x > 0) {
	f_obj[index].life++;
	if (flame_expired(index)) {
	    flame[index].x = flame[index].y = 0;
	    index = next_flame(index);
	    tail = index;
	    continue;
	}
	advance_flame(index);
	update_flame_sprite(index);
	update_total_rectange(index);
	flame[index].next = previous;
	previous = index + FLAME_OFFSET;
	index = next_flame(index);
	if (index == head) break;
    }
    /* add sprite size */
    f_rect.x2 += 16;
    f_rect.y2 += 8;
    /* link last soldier sprite to first flame */
    sprite[0].next = previous;
    if (cooldown > 0) {
	cooldown--;
    }
}

static Rectangle flame_rectangle(Rectangle *r, u16 index) {
    r->x1 = flame[index].x + 1;
    r->x2 = r->x1 + 14;
    r->y1 = flame[index].y + 2;
    r->y2 = r->y1 + 4;
}

u16 flame_collision(Rectangle *r) {
    Rectangle f_single;
    if (intersect(r, &f_rect)) {
	for (u16 index = 0; index < 8; index++) {
	    if (flame[index].x > 0) {
		flame_rectangle(&f_single, index);
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

static void soldier_yelling(byte state) {
    static byte face;
    if (face != state) {
	psg_noise(7, state ? 0x4 : 0xf);
	base[-1].cfg = TILE(2, state ? WEAPON : 0);
	soldier_flicker(0);
	face = state;
    }
    if (state) {
	soldier_flicker(counter & 2);
    }
}

static void move_forward(void) {
    if (base->x >= SOLDIER_MAX_X && !is_rightmost()) {
	update_window(1);
	soldier.x++;
    }
    else if (base->x < SOLDIER_MAX_X) {
	soldier.x++;
    }
}

static void move_backward(void) {
    if (base->x <= SOLDIER_MIN_X && !is_leftmost()) {
	update_window(-1);
	soldier.x--;
    }
    else if (base->x > SOLDIER_MIN_X) {
	soldier.x--;
    }
}

static void soldier_march(void) {
    u16 button_state = read_gamepad();
    static u16 last_state;
    u16 prev = soldier.x;

    u16 aim_up = 0;
    if (BUTTON_RIGHT(button_state)) {
	move_forward();
    }
    else if (BUTTON_LEFT(button_state)) {
	move_backward();
    }
    else if (BUTTON_UP(button_state) && on_ground()) {
	aim_up = 1;
    }
    update_height_map(soldier.x);

    u16 fire = BUTTON_B(button_state) && on_ground();
    soldier_yelling(fire);
    if (fire && cooldown == 0) {
	throw_flames(aim_up);
	cooldown = 8;
    }

    u16 jump = BUTTON_C(button_state) && BUTTON_C(last_state) == 0;
    soldier_jump(jump, BUTTON_DOWN(button_state));
    soldier_animate(prev, aim_up);
    soldier_sprite_update();

    last_state = button_state;
}

static void hide_all_sprites(void) {
    for (u16 i = 0; i < ARRAY_SIZE(sprite); i++) {
	sprite[i].x = 0;
    }
}

static void restart_level(void) {
    hide_all_sprites();
    switch_frame(&display_canyon);
}

#define FADE_SPEED 3
static void fade_and_restart(u16 fade) {
    if (fade < 8) {
	upload_palette(fade);
	callback(&fade_and_restart, FADE_SPEED, fade + 1);
    }
    else {
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
    base->cfg ^= BIT(11);
    soldier_sprite_update();
    schedule(&soldier_sinking, 10);
    if ((base->cfg >> 11) & 1) wiggle_sfx();
}

static void soldier_sink(void) {
    if (is_dead == 1) {
	schedule(&fade_and_restart, 150);
	soldier_sinking(0);
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
    base[0].y++;
    base[1].cfg += 6;
    if (base[1].cfg < TILE(2, SOLDIER_LEG + 22 * 6)) {
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
    else if (base[0].cfg != TILE(2, SOLDIER_POISON)) {
	update_color(39, 0x668);
	update_color(40, 0x446);
	base[-1].size = SPRITE_SIZE(4, 1);
	base[-1].cfg = TILE(2, WEAPON + 1);
	base[-1].x = base[0].x - 4;
	base[-1].y = base[2].y;

	base[0].cfg = TILE(2, SOLDIER_POISON);
	base[1].cfg = TILE(2, SOLDIER_LEG + 18 * 6);
	base[2].x = base[2].y = 0;
    }
    else if (base[-1].y < base[1].y + 12) {
	base[-1].y++;
    }
    else {
	schedule(&soldier_kneel, 6);
	is_dead = -1;
    }
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
    soldier.x = window + x;
    soldier.y = y;

    base[-1].cfg = TILE(2, 0);
    base[-1].size = SPRITE_SIZE(1, 1);
    base[-1].next = SOLDIER_BASE;

    base[0].cfg = TILE(2, SOLDIER_TOP);
    base[0].size = SPRITE_SIZE(3, 3);
    base[0].next = SOLDIER_BASE + 1;

    base[1].cfg = TILE(2, SOLDIER_LEG);
    base[1].size = SPRITE_SIZE(3, 2);
    base[1].next = SOLDIER_BASE + 2;

    base[2].cfg = TILE(2, WEAPON + 4);
    base[2].size = SPRITE_SIZE(1, 1);
    base[2].next = 0;

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

void setup_soldier_sprites(void) {
    head = tail = cooldown = 0;
    base = get_sprite(SOLDIER_BASE);
    blood = get_sprite(BLOOD_SPRITE);
    flame = get_sprite(FLAME_OFFSET);
    memset(&soldier, 0, sizeof(soldier));
    memset(sprite, 0, sizeof(sprite));
    put_soldier(0, platform_bottom());
    clear_rectangle(&f_rect);
    clear_rectangle(&s_rect);
    is_dead = 0;
}
