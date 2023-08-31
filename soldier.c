#include "main.h"

#include "images/soldier.h"
#include "images/walk.h"

static u16 soldier_y;
static u16 platform_h;
static u16 button_state;

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
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x00, soldier_y);
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x08, soldier_y + 24);
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x10, soldier_y + 21);
}

void soldier_jump(u16 start) {
    static short gravity;
    static short velocity;
    if (start && soldier_y == platform_h) {
	velocity = 5;
	gravity = 0;
    }

    soldier_y -= velocity;
    if (gravity == 0) {
	gravity = 5;
	velocity--;
    }
    gravity--;

    if (soldier_y >= platform_h) {
	soldier_y = platform_h;
	velocity = 0;
	gravity = 0;
    }
    update_soldier_y();
}

static short animate_walking(short cycle, u16 prev, u16 scroll) {
    if (prev == scroll) {
	if (cycle >= 0) {
	    cycle = (cycle < 6) ? -1 : -2; /* stop walking frame */
	}
    }
    else {
	if (cycle < 0) {
	    cycle = (cycle == -1) ? 2 : 8; /* start walking frame */
	}
	if ((scroll & 3) == 1) {
	    if (scroll < prev) {
		if (cycle == 0) cycle = 11; else cycle--;
	    }
	    else {
		if (cycle == 11) cycle = 0; else cycle++;
	    }
	}
    }
    return cycle;
}

static void soldier_animate(u16 prev, u16 scroll) {
    static short cycle;
    u16 soldier_frame;
    if (soldier_y == platform_h) {
	cycle = animate_walking(cycle, prev, scroll);
	soldier_frame = 6 * (cycle + 2) + 524;
    }
    else {
	soldier_frame = (cycle >= 6 || cycle == -2) ? 608 : 614;
    }
    UPDATE_VRAM_WORD(VRAM_SPRITE + 12, TILE(2, soldier_frame));
}

u16 soldier_march(void) {
    static u16 scroll;
    u16 prev = scroll;
    u16 last = button_state;

    read_gamepad();
    if (BUTTON_RIGHT(button_state)) {
	scroll++;
    }
    else if (BUTTON_LEFT(button_state)) {
	scroll--;
    }

    soldier_jump(BUTTON_C(button_state) && BUTTON_C(last) == 0);
    soldier_animate(prev, scroll);

    if (BUTTON_B(button_state)) {
	scroll += (scroll < prev) ? -4 : 4;
    }

    return scroll;
}

static void put_soldier(u16 x, u16 y) {
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x00, y);
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x02, SPRITE(3, 3, 1));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x04, TILE(2, 512));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x06, x);

    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x08, y + 24);
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x0a, SPRITE(3, 2, 2));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x0c, TILE(2, 524));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x0e, x);

    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x10, y + 21);
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x12, SPRITE(1, 1, 0));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x14, TILE(2, 521));
    UPDATE_VRAM_WORD(VRAM_SPRITE + 0x16, x + 24);
}

void setup_soldier_sprites(void) {
    update_palette(soldier_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(soldier_tiles, 512, ARRAY_SIZE(soldier_tiles));
    update_tiles(walk_tiles, 524, ARRAY_SIZE(walk_tiles));

    platform_h = 296;
    soldier_y = platform_h;
    put_soldier(176, soldier_y);
}