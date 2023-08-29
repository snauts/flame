#include "main.h"

#include "images/soldier.h"

void put_soldier_column(u16 x, u16 y, u16 s) {
    static u16 next;
    static u16 offset;
    poke_VRAM(VRAM_SPRITE + offset + 0, y);
    poke_VRAM(VRAM_SPRITE + offset + 2, 0x300 | ++next);
    poke_VRAM(VRAM_SPRITE + offset + 4, 0x4000 | s);
    poke_VRAM(VRAM_SPRITE + offset + 6, x);
    offset += 8;

    poke_VRAM(VRAM_SPRITE + offset + 0, y + 32);
    poke_VRAM(VRAM_SPRITE + offset + 2, ++next);
    poke_VRAM(VRAM_SPRITE + offset + 4, 0x4000 | (s + 4));
    poke_VRAM(VRAM_SPRITE + offset + 6, x);
    offset += 8;
}

void put_soldier(u16 x, u16 y) {
    u16 i, s = 512;
    for (i = 0; i < 4; i++) {
	put_soldier_column(x, y, s);
	x += 8;
	s += 5;
    }
}

void setup_soldier_sprites(void) {
    update_palette(soldier_palette, 32, ARRAY_SIZE(soldier_palette));
    update_tiles(soldier_tiles, 512, ARRAY_SIZE(soldier_tiles));
    put_soldier(176, 296);
}
