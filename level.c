#include "main.h"
#include "level.inc"

const u16 *level_ptr;

static void fill_column(u16 addr) {
    u16 count = *(level_ptr++) & 0xff;
    while (count > 0) {
	poke_VRAM(addr, *(level_ptr++));
	addr = addr - 0x80;
	count--;
    }
}

void fill_level(const u16 *level) {
    level_ptr = level;
    for (u16 x = 0; x < 128; x += 2) {
	fill_column(0xd80 + x);
    }
}
