#include "main.h"

#include "images/soldier.h"
#include "images/flame_up.h"
#include "images/flame.h"
#include "images/walk.h"

#define SOLDIER_TOP	512
#define SOLDIER_LEG	(SOLDIER_TOP + (3 * 10))

#define FLAME		(SOLDIER_LEG + (18 * 3 * 2))
#define FLAME_UP	(FLAME + (32 * 2 * 1))

#define ON_SCREEN	128

#define SOLDIER_MIN_X	(ON_SCREEN + 16)
#define SOLDIER_MAX_X	(ON_SCREEN + 128)
#define SOLDIER_AHEAD	34

#define FLAME_OFFSET	4
#define SOLDIER_BASE	1

extern u16 window;
static Object soldier;
static char is_dead;

Sprite sprite[80];

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

static void soldier_sprite_update(void) {
    sprite[1].x = soldier.x - window + SOLDIER_MIN_X;
    sprite[1].y = soldier.y + ON_SCREEN - 40;

    sprite[0].x = sprite[1].x + 8;
    sprite[0].y = sprite[1].y + 8;

    sprite[2].x = sprite[1].x;
    sprite[2].y = sprite[1].y + 24;

    sprite[3].x = sprite[1].x + 24;
    sprite[3].y = sprite[1].y + 21;

    if (!is_dead && sprite[1].y >= 200 + ON_SCREEN) {
	sprite[SOLDIER_BASE].cfg = TILE(2, SOLDIER_TOP + 21);
	is_dead = 1;
    }
}

static u16 on_ground(void) {
    static u16 when;
    static u16 what;
    if (counter != when) {
	what = get_snap(soldier.y, soldier.y);
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

static void snap_jump(u16 snap) {
    if (snap != 0) {
	soldier.y = snap;
	soldier.velocity = 0;
	soldier.gravity = 0;
    }
}

static void soldier_jump(u16 start, u16 down) {
    if (start && on_ground()) {
	initiate_jump(down, 4);
    }
    u16 prev = soldier.y;
    advance_y(&soldier, 6);
    snap_jump(get_snap(prev, soldier.y));
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
	sprite[SOLDIER_BASE + 0].cfg = TILE(2, SOLDIER_TOP + 12);
	sprite[SOLDIER_BASE + 2].cfg = TILE(2, 0);
    }
    else {
	sprite[SOLDIER_BASE + 0].cfg = TILE(2, SOLDIER_TOP);
	sprite[SOLDIER_BASE + 2].cfg = TILE(2, SOLDIER_TOP + 9);
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
    sprite[SOLDIER_BASE + 1].cfg = TILE(2, soldier_frame);
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
    flame[index].x = soldier.x + (f_obj[index].x >> 4) - window + ON_SCREEN;
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

    u16 flame_y = sprite[SOLDIER_BASE].y + offset_y;
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

static void manage_flames(void) {
    u16 index = tail;
    extern byte first_mob_sprite;
    byte previous = first_mob_sprite;
    while (flame[index].x > 0) {
	f_obj[index].life++;
	if (flame_expired(index)) {
	    flame[index].x = 0;
	    index = next_flame(index);
	    tail = index;
	    continue;
	}
	advance_flame(index);
	update_flame_sprite(index);
	flame[index].next = previous;
	previous = index + FLAME_OFFSET;
	index = next_flame(index);
	if (index == head) break;
    }
    /* link last soldier sprite to first flame */
    sprite[SOLDIER_BASE + 2].next = previous;
    if (cooldown > 0) {
	cooldown--;
    }
}

static void flicker_color(u16 index, u16 deviate) {
    u16 color = soldier_palette[index] + deviate;
    UPDATE_CRAM_WORD(2 * (32 + index), color);
}

static void soldier_flicker(u16 deviate) {
    flicker_color(6, deviate);
    flicker_color(7, deviate);
}

static void soldier_yelling(byte state) {
    static byte face;
    if (face != state) {
	psg_noise(7, state ? 0x0 : 0xf);
	sprite[0].cfg = TILE(2, state ? SOLDIER_TOP + 10 : 0);
	soldier_flicker(0);
	face = state;
    }
    if (state) {
	soldier_flicker(counter & 2);
    }
}

static void move_forward(void) {
    if (sprite[SOLDIER_BASE].x >= SOLDIER_MAX_X && !is_rightmost()) {
	update_window(1);
	soldier.x++;
    }
    else if (sprite[SOLDIER_BASE].x < SOLDIER_MAX_X) {
	soldier.x++;
    }
}

static void move_backward(void) {
    if (sprite[SOLDIER_BASE].x <= SOLDIER_MIN_X && !is_leftmost()) {
	update_window(-1);
	soldier.x--;
    }
    else if (sprite[SOLDIER_BASE].x > SOLDIER_MIN_X) {
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
    update_height_map(soldier.x + SOLDIER_AHEAD);

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

static void soldier_die(void) {
    static char ticks;
    if (ticks++ == 10) {
	sprite[SOLDIER_BASE].cfg ^= BIT(11);
	soldier.y++;
	ticks = 0;
    }
    short diff = 224 + ON_SCREEN - sprite[1].y;
    if (diff < 0) {
	hide_all_sprites();
	switch_frame(&display_canyon);
    }
    else if (diff <= 7) {
	upload_palette(7 - diff);
    }
    soldier_sprite_update();
}

void advance_sprites(void) {
    is_dead ? soldier_die() : soldier_march();

    /* manage mobs first because manage_flames uses first_mob_sprite */
    manage_mobs();
    manage_flames();

    copy_to_VRAM_ptr(VRAM_SPRITE, sizeof(sprite), sprite);
}

static void put_soldier(u16 x, u16 y) {
    soldier.x = window + x;
    soldier.y = y;

    sprite[0].cfg = TILE(2, 0);
    sprite[0].size = SPRITE_SIZE(1, 1);
    sprite[0].next = 1;

    sprite[1].cfg = TILE(2, SOLDIER_TOP);
    sprite[1].size = SPRITE_SIZE(3, 3);
    sprite[1].next = 2;

    sprite[2].cfg = TILE(2, SOLDIER_LEG);
    sprite[2].size = SPRITE_SIZE(3, 2);
    sprite[2].next = 3;

    sprite[3].cfg = TILE(2, SOLDIER_TOP + 9);
    sprite[3].size = SPRITE_SIZE(1, 1);
    sprite[3].next = 0;

    soldier_sprite_update();
}

void load_soldier_tiles(void) {
    update_palette(soldier_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(walk_tiles, SOLDIER_LEG, ARRAY_SIZE(walk_tiles));
    update_tiles(soldier_tiles, SOLDIER_TOP, ARRAY_SIZE(soldier_tiles));

    update_tiles(flame_tiles, FLAME, ARRAY_SIZE(flame_tiles));
    update_tiles(flame_up_tiles, FLAME_UP, ARRAY_SIZE(flame_up_tiles));
}

Sprite *get_sprite(u16 offset) {
    return sprite + offset;
}

void setup_soldier_sprites(void) {
    flame = get_sprite(FLAME_OFFSET);
    put_soldier(0, platform_bottom());
    is_dead = 0;
}
