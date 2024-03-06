#include "main.h"

static void display_french(Function prepare_level) {
    /* load tiles */
    load_soldier_tiles(3);
    reset_window();
    reset_mobs();

    /* background */
    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);

   /* foreground */
    clear_DMA_buffer(0, 0x1000);

    prepare_level();

    fill_VRAM(0, 0, 0x800);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);

    setup_soldier_sprites();
    void music_onions(void);
    music_onions();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}

void display_town(void) {
    void prepare_town_level(void);
    display_french(&prepare_town_level);
}
