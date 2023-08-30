#include "main.h"

#include "images/soldierT.h"
#include "images/soldierB.h"

void put_soldier(u16 x, u16 y) {
    LONG(VDP_CTRL) = VDP_CTRL_VALUE(VDP_VRAM_WRITE, VRAM_SPRITE);

    WORD(VDP_DATA) = y;
    WORD(VDP_DATA) = SPRITE(4, 2, 1);
    WORD(VDP_DATA) = TILE(2, 512);
    WORD(VDP_DATA) = x;

    WORD(VDP_DATA) = y + 16;
    WORD(VDP_DATA) = SPRITE(4, 3, 0);
    WORD(VDP_DATA) = TILE(2, 520);
    WORD(VDP_DATA) = x;
}

void setup_soldier_sprites(void) {
    update_palette(soldierT_palette, 32, ARRAY_SIZE(soldierT_palette));

    update_tiles(soldierT_tiles, 512, ARRAY_SIZE(soldierT_tiles));
    update_tiles(soldierB_tiles, 520, ARRAY_SIZE(soldierB_tiles));

    put_soldier(176, 296);
}
