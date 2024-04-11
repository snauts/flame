#include "main.h"

#include "forest.inc"

static void display_soviet(const Level *level) {
    load_soldier_tiles(4);

    /* background */
    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    prepare_level(level);

    void music_katyusha(void);
    music_katyusha();
}

void display_forest(void) {
    display_soviet(&forest_level);
}
