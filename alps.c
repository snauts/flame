#include "main.h"

#include "images/alps.h"

void display_mountains(void) {
    /* load tiles */
    update_palette(alps_palette, 0, ARRAY_SIZE(alps_palette));
    update_tiles(alps_tiles, 1, ARRAY_SIZE(alps_tiles));

    load_soldier_tiles();
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

    /* foreground */
    clear_DMA_buffer(0, 0x1000);

    prepare_mountain_level();

    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_erika(void);
    music_erika();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}
