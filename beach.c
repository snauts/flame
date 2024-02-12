#include "main.h"
#include "images/beach.h"

void display_nippon(Function prepare_level) {
    update_palette(beach_palette, 0, ARRAY_SIZE(beach_palette));
    update_tiles(beach_tiles, 1, ARRAY_SIZE(beach_tiles));

    /* load tiles */
    load_soldier_tiles(2);
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
    void music_battotai(void);
    music_battotai();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}

void display_beach(void) {
    void prepare_beach_level(void);
    display_nippon(&prepare_beach_level);
}
