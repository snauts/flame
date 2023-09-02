#include "main.h"

#include "images/soldier.h"
#include "images/flame.h"
#include "images/walk.h"

#define SOLDIER_TOP	512
#define SOLDIER_LEG	524
#define SOLDIER_FIRE	620

static Pos soldier;
static u16 platform_h;
static u16 button_state;

static Sprite sprite[80];

#define BUTTON_A(x) ((x) & BIT(12))
#define BUTTON_B(x) ((x) & BIT(4))
#define BUTTON_C(x) ((x) & BIT(5))

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

static void update_soldier_y(void) {
    sprite[0].y = soldier.y;
    sprite[1].y = soldier.y + 24;
    sprite[2].y = soldier.y + 21;
}

static u16 on_ground(void) {
    return soldier.y == platform_h;
}

void soldier_jump(u16 start) {
    static short gravity;
    static short velocity;
    if (start && on_ground()) {
	velocity = 5;
	gravity = 0;
    }

    soldier.y -= velocity;
    if (gravity == 0) {
	gravity = 5;
	velocity--;
    }
    gravity--;

    if (soldier.y >= platform_h) {
	soldier.y = platform_h;
	velocity = 0;
	gravity = 0;
    }
    update_soldier_y();
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

static void soldier_animate(u16 prev) {
    static short cycle;
    u16 soldier_frame;
    if (soldier.y == platform_h) {
	cycle = animate_walking(cycle, prev);
	soldier_frame = 6 * (cycle + 2) + SOLDIER_LEG;
    }
    else {
	soldier_frame = (cycle >= 6 || cycle == -2) ? 608 : 614;
    }
    sprite[1].cfg = TILE(2, soldier_frame);
}

static Sprite *flame;
static u16 head, tail;
static u16 cooldown;

static u16 next_flame(u16 index) {
    return (index + 1) & 7;
}

#define FIRE_FRAME(x) TILE(2, SOLDIER_FIRE + (2 * (x)))

static void emit_flame(u16 index) {
    flame[index].x = 204;
    flame[index].y = soldier.y + 20;
    flame[index].cfg = FIRE_FRAME(0);
    flame[index].size = SPRITE_SIZE(2, 1);
}

static void throw_flames(void) {
    if (flame[head].x == 0) {
	emit_flame(head);
	head = next_flame(head);
    }
}

static u16 flame_expired(u16 index) {
    return flame[index].cfg >= FIRE_FRAME(16);
}

static void flame_gravity(u16 index) {
    if (flame[index].cfg >= FIRE_FRAME(12)) {
	flame[index].y += 2;
    }
    else if (flame[index].cfg >= FIRE_FRAME(6)) {
	flame[index].y += 1;
    }
}

static void manage_flames(void) {
    u16 index = tail;
    byte previous = 0;
    while (flame[index].x > 0) {
	if ((counter & 3) == 0) {
	    flame[index].cfg += 2;
	    flame_gravity(index);
	}
	if (flame_expired(index)) {
	    flame[index].x = 0;
	    index = next_flame(index);
	    tail = index;
	    continue;
	}
	flame[index].x += 2;
	flame[index].next = previous;
	previous = index + 3;
	index = next_flame(index);
	if (index == head) break;
    }
    sprite[2].next = previous;
    if (cooldown > 0) {
	cooldown--;
    }
}

u16 soldier_march(void) {
    u16 prev = soldier.x;
    u16 last = button_state;

    read_gamepad();
    if (BUTTON_RIGHT(button_state)) {
	soldier.x++;
    }
    else if (BUTTON_LEFT(button_state) && soldier.x > 0) {
	soldier.x--;
    }
    if (BUTTON_B(button_state) && on_ground() && cooldown == 0) {
	throw_flames();
	cooldown = 8;
    }

    soldier_jump(BUTTON_C(button_state) && BUTTON_C(last) == 0);
    soldier_animate(prev);
    manage_flames();

    copy_to_VRAM_ptr(VRAM_SPRITE, sizeof(sprite), sprite);
    return soldier.x;
}

static void put_soldier(u16 x, u16 y) {
    sprite[0].x = x;
    sprite[0].y = y;
    sprite[0].cfg = TILE(2, SOLDIER_TOP);
    sprite[0].size = SPRITE_SIZE(3, 3);
    sprite[0].next = 1;

    sprite[1].x = x;
    sprite[1].y = y + 24;
    sprite[1].cfg = TILE(2, SOLDIER_LEG);
    sprite[1].size = SPRITE_SIZE(3, 2);
    sprite[1].next = 2;

    sprite[2].x = x + 24;
    sprite[2].y = y + 21;
    sprite[2].cfg = TILE(2, 521);
    sprite[2].size = SPRITE_SIZE(1, 1);
    sprite[2].next = 0;
}

void load_soldier_tiles(void) {
    update_palette(flame_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(soldier_tiles, SOLDIER_TOP, ARRAY_SIZE(soldier_tiles));
    update_tiles(flame_tiles, SOLDIER_FIRE, ARRAY_SIZE(flame_tiles));
    update_tiles(walk_tiles, SOLDIER_LEG, ARRAY_SIZE(walk_tiles));
}

void setup_soldier_sprites(void) {
    platform_h = 296;
    flame = sprite + 3;
    soldier.y = platform_h;
    put_soldier(176, soldier.y);
}
