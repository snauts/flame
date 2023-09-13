#include "main.h"
#include "level.inc"

#define WINDOW_MIN	64
#define HEIGHT_DATA	2

u16 window;

static short column;
static const u16 *back;
static const u16 *front;

static u16 next_platform;
static u16 prev_platform;
static u16 platform_count;
static const byte *height;

static const u16 *advance(const u16 *ptr) {
    short count = *ptr & 0xff;
    return ptr + count + 1;
}

static const u16 *recede(const u16 *ptr) {
    short count = *ptr >> 8;
    return ptr - count - 1;
}

static const u16 *fill_column(const u16 *ptr, void (*poke)(u16, u16)) {
    u16 addr = 0xd80 + column;
    short count = *(ptr++) & 0xff;
    while (addr > 0x80) {
	poke(addr, count > 0 ? *(ptr++) : 0);
	addr = addr - 0x80;
	count--;
    }
    return ptr;
}

static void update_VRAM(u16 addr, u16 data) {
    UPDATE_VRAM_WORD(VRAM_PLANE_A + addr, data);
}

static void update_column_forward(void (*poke)(u16, u16)) {
    front = fill_column(front, poke);
    column = (column + sizeof(u16)) & 0x7f;
    back = advance(back);
}

static void update_column_backward(void) {
    back = recede(back);
    column = (column - sizeof(u16)) & 0x7f;
    fill_column(back, &update_VRAM);
    front = recede(front);
}

u16 is_rightmost(void) {
    return (*front & 0xff) == 0;
}

u16 is_leftmost(void) {
    return (*back >> 8) == 0 && (window <= WINDOW_MIN);
}

byte platform_bottom(void) {
    return platform_count > 0 ? height[1 + platform_count] : 0;
}

static void forward_platform(void) {
    platform_count = (height[0] & 0xf) - 1;
    prev_platform = next_platform;
    next_platform += 8 * height[1];
}

static void backward_platform(void) {
    platform_count = (height[0] & 0xf) - 1;
    next_platform = prev_platform;
    prev_platform -= 8 * height[1];
}

void update_height_map(u16 pos_x) {
    if (pos_x >= next_platform) {
	height += (*height & 0xf) + 1;
	forward_platform();
    }
    else if (pos_x < prev_platform) {
	height -= (*height >> 4) + 1;
	backward_platform();
    }
}

static void prepare_level(const u16 *level, const byte *map) {
    column = 0;
    height = map;
    front = level;
    for (u16 x = 0; x < 64; x++) {
	update_column_forward(&poke_VRAM);
    }
    back = level; /* reset back */
    next_platform = 0;
    forward_platform();
}

void prepare_desert_level(void) {
    prepare_level(desert_level, desert_level_height);
}

void level_scroll(void) {
    UPDATE_VRAM_WORD(VRAM_SCROLL_A, -window);
    UPDATE_VRAM_WORD(VRAM_SCROLL_B, -(window >> 1));
}

void reset_window(void) {
    window = WINDOW_MIN;
    level_scroll();
}

static u16 boundary(u16 x) {
    return x & ~0x7;
}

void update_window(short direction) {
    u16 next, prev;
    prev = boundary(window);
    window += direction;
    next = boundary(window);
    if (next > prev) {
	update_column_forward(&update_VRAM);
    }
    if (prev > next) {
	update_column_backward();
    }
}

byte get_snap(byte prev, byte next) {
    for (u16 i = 0; i < platform_count; i++) {
	byte snap = height[HEIGHT_DATA + i];
	if (prev <= snap && snap <= next) {
	    return snap;
	}
    }
    return 0;
}
