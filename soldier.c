#include "main.h"

#include "images/soldier.h"
#include "images/walk.h"

void soldier_march(void) {
    u16 frame = 6 * (((counter >> 2) % 12) + 1) + 524;
    UPDATE_VRAM_WORD(VRAM_SPRITE + 12, TILE(2, frame));
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

    put_soldier(176, 296);
}
