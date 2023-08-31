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

u16 soldier_march(void) {
    static u16 scroll;
    static short cycle;
    u16 frame, prev = scroll;
    u16 last = button_state;

    read_gamepad();
    if (BUTTON_RIGHT(button_state)) {
	scroll++;
    }
    else if (BUTTON_LEFT(button_state)) {
	scroll--;
    }

    if (BUTTON_C(button_state) && BUTTON_C(last) == 0) {
	update_soldier_y();
    }

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
	    cycle = cycle + 1;
	    if (cycle == 12) cycle = 0;
	}
    }

    if (BUTTON_B(button_state)) {
	scroll += 4;
    }

    frame = 6 * (cycle + 2) + 524;
    UPDATE_VRAM_WORD(VRAM_SPRITE + 12, TILE(2, frame));
    return scroll;
}

static void put_soldier(u16 x, u16 y) {
    LONG(VDP_CTRL) = VDP_CTRL_VALUE(VDP_VRAM_WRITE, VRAM_SPRITE);

    WORD(VDP_DATA) = y;
    WORD(VDP_DATA) = SPRITE(3, 3, 1);
    WORD(VDP_DATA) = TILE(2, 512);
    WORD(VDP_DATA) = x;

    WORD(VDP_DATA) = y + 24;
    WORD(VDP_DATA) = SPRITE(3, 2, 2);
    WORD(VDP_DATA) = TILE(2, 524);
    WORD(VDP_DATA) = x;

    WORD(VDP_DATA) = y + 21;
    WORD(VDP_DATA) = SPRITE(1, 1, 0);
    WORD(VDP_DATA) = TILE(2, 521);
    WORD(VDP_DATA) = x + 24;
}

void setup_soldier_sprites(void) {
    update_palette(soldier_palette, 32, ARRAY_SIZE(soldier_palette));

    update_tiles(soldier_tiles, 512, ARRAY_SIZE(soldier_tiles));
    update_tiles(walk_tiles, 524, ARRAY_SIZE(walk_tiles));

    platform_h = 296;
    soldier_y = platform_h;
    put_soldier(176, soldier_y);
}
