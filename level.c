#include "main.h"
#include "level.inc"

u16 window;

static u16 column;
static const u16 *ptr;

static u16 next_column(u16 x) {
    return (x + 2) & 0x7f;
}

static void fill_column(void (*poke)(u16, u16)) {
    u16 addr = 0xd80 + column;
    short count = *(ptr++) & 0xff;
    while (addr > 0x80) {
	poke(addr, count > 0 ? *(ptr++) : 0);
	addr = addr - 0x80;
	count--;
    }
    column = next_column(column);
}

static void update_VRAM(u16 addr, u16 data) {
    UPDATE_VRAM_WORD(VRAM_PLANE_A + addr, data);
}

void update_column_forward(void) {
    fill_column(&update_VRAM);
}

u16 is_rightmost(void) {
    return (*ptr & 0xff) == 0;
}

u16 is_leftmost(void) {
    return (*ptr >> 8) == 0;
}

void fill_level(const u16 *level) {
    column = 0;
    ptr = level;
    for (u16 x = 0; x < 64; x++) {
	fill_column(&poke_VRAM);
    }
}

void level_scroll(void) {
    UPDATE_VRAM_WORD(VRAM_SCROLL_A, -window);
    UPDATE_VRAM_WORD(VRAM_SCROLL_B, -(window >> 1));
}

void reset_window(void) {
    window = 64;
    level_scroll();
}

void update_window(short dir) {
    u16 last = window & ~0x7;
    window += dir;
    if ((window & ~0x7) > last) {
	update_column_forward();
    }
}
