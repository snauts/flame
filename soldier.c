#include "main.h"

#include "images/soldier.h"
#include "images/flame.h"
#include "images/walk.h"

#define SOLDIER_TOP	512
#define SOLDIER_LEG	534
#define SOLDIER_FIRE	642

#define SOLDIER_MIN_X	150
#define SOLDIER_MAX_X	250
#define SOLDIER_AHEAD	34

#define FLAME_OFFSET	4
#define SOLDIER_BASE	1

extern u16 window;
static Object soldier;
static u16 button_state;

static Sprite sprite[80];

#define BUTTON_A(x) ((x) & BIT(12))
#define BUTTON_B(x) ((x) & BIT(4))
#define BUTTON_C(x) ((x) & BIT(5))

#define BUTTON_UP(x) ((x) & BIT(0))
#define BUTTON_DOWN(x) ((x) & BIT(1))
#define BUTTON_LEFT(x) ((x) & BIT(2))
#define BUTTON_RIGHT(x) ((x) & BIT(3))

static u16 read_gamepad(void) {
    BYTE(GAMEPAD_A_DATA) = 0;
    asm("nop");
    asm("nop");
    button_state = BYTE(GAMEPAD_A_DATA) << 8;
    BYTE(GAMEPAD_A_DATA) = BIT(6);
    asm("nop");
    asm("nop");
    button_state = ~(button_state | BYTE(GAMEPAD_A_DATA));
}

static void soldier_sprite_update(void) {
    sprite[1].x = soldier.x - window + SOLDIER_MIN_X;
    sprite[1].y = (soldier.y >> Y_FACTOR) + 128 - 40;

    sprite[0].x = sprite[1].x + 8;
    sprite[0].y = sprite[1].y + 8;

    sprite[2].x = sprite[1].x;
    sprite[2].y = sprite[1].y + 24;

    sprite[3].x = sprite[1].x + 24;
    sprite[3].y = sprite[1].y + 21;

    if (sprite[1].y >= 240 + 128) soldier.y = 0;
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

static void advance_position(Object *obj, char gravity) {
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
    advance_position(&soldier, 6);
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

#define FIRE_FRAME(x) TILE(2, SOLDIER_FIRE + (2 * (x)))

static void emit_flame(u16 index, u16 aim_up) {
    flame[index].x = sprite[SOLDIER_BASE].x + 24;
    flame[index].y = sprite[SOLDIER_BASE].y + 20;
    flame[index].cfg = FIRE_FRAME(0);
    flame[index].size = SPRITE_SIZE(2, 1);

    f_obj[index].x = aim_up;
    f_obj[index].y = flame[index].y << 4;
    f_obj[index].velocity = 0;
    f_obj[index].gravity = 4;
}

static void throw_flames(u16 aim_up) {
    if (flame[head].x == 0) {
	emit_flame(head, aim_up);
	head = next_flame(head);
    }
}

static u16 flame_expired(u16 index) {
    return flame[index].cfg >= FIRE_FRAME(32);
}

static void flame_gravity(u16 index) {
    advance_position(f_obj + index, 8);
    flame[index].y = f_obj[index].y >> 4;
}

static void manage_flames(void) {
    u16 index = tail;
    byte previous = 0;
    while (flame[index].x > 0) {
	flame_gravity(index);
	if ((counter & 1) == 0) {
	    flame[index].cfg += 2;
	}
	if (flame_expired(index)) {
	    flame[index].x = 0;
	    index = next_flame(index);
	    tail = index;
	    continue;
	}
	flame[index].x += 2;
	flame[index].next = previous;
	previous = index + FLAME_OFFSET;
	index = next_flame(index);
	if (index == head) break;
    }
    sprite[FLAME_OFFSET - 1].next = previous;
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

void soldier_march(void) {
    u16 prev = soldier.x;
    u16 last = button_state;

    read_gamepad();

    u16 aim_up = 0;
    if (BUTTON_UP(button_state) && on_ground()) {
	// aim_up = 1;
    }
    else if (BUTTON_RIGHT(button_state)) {
	move_forward();
    }
    else if (BUTTON_LEFT(button_state)) {
	move_backward();
    }
    update_height_map(soldier.x + SOLDIER_AHEAD);

    u16 fire = BUTTON_B(button_state) && on_ground();
    soldier_yelling(fire);
    if (fire && cooldown == 0) {
	throw_flames(aim_up);
	cooldown = 8;
    }

    u16 jump = BUTTON_C(button_state) && BUTTON_C(last) == 0;
    soldier_jump(jump, BUTTON_DOWN(button_state));
    soldier_animate(prev, aim_up);
    soldier_sprite_update();
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
    update_palette(flame_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(walk_tiles, SOLDIER_LEG, ARRAY_SIZE(walk_tiles));
    update_tiles(flame_tiles, SOLDIER_FIRE, ARRAY_SIZE(flame_tiles));
    update_tiles(soldier_tiles, SOLDIER_TOP, ARRAY_SIZE(soldier_tiles));
}

void setup_soldier_sprites(void) {
    flame = sprite + FLAME_OFFSET;
    put_soldier(0, platform_bottom());
}
