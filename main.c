#include "main.h"

static void addr_VDP(u32 flags, u16 addr) {
    LONG(VDP_CTRL) = flags | ((addr & 0x3fff) << 16) | (addr >> 14);
}

static void tmss(void) {
    if ((BYTE(HW_VER) & 0xf) != 0) {
	LONG(TMSS_ADDR) = 0x53454741; /* "SEGA" */
    }
}

const byte VDP_regs[] = {
    0x14, 0x74, 0x30, 0x00, 0x07, 0x78, 0x00, 0x00,
    0x00, 0x00, 0x08, 0x00, 0x81, 0x3F, 0x00, 0x02,
    0x01, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x80
};

static void init_VDP(void) {
    char i;
    for (i = 0; i < ARRAY_SIZE(VDP_regs); i++) {
	WORD(VDP_CTRL) = 0x8000 | (i << 8) | VDP_regs[i];
    }
}

static u16 is_vblank(void) {
    return WORD(VDP_CTRL) & BIT(3);
}

static void wait_for_vblank(void) {
    while (!is_vblank()) { }
}

static void wait_for_draw(void) {
    while (is_vblank()) { }
}

void skip_frames(u16 count) {
    while (count > 0) {
	wait_for_vblank();
	wait_for_draw();
	count--;
    }
}

void update_palette(const u16 *buf, int offset, int count) {
    int i;
    addr_VDP(VDP_CRAM_WRITE, 2 * offset);
    for (i = 0; i < count; i++) {
	WORD(VDP_DATA) = buf[i];
    }
}

void poke_VRAM(u16 addr, u16 data) {
    addr_VDP(VDP_VRAM_WRITE, addr);
    WORD(VDP_DATA) = data;
}

void fill_VRAM(u16 addr, u16 data, u16 count) {
    int i;
    addr_VDP(VDP_VRAM_WRITE, addr);
    for (i = 0; i < count; i++) {
	WORD(VDP_DATA) = data;
    }
}

void update_tiles(const u32 *buf, int offset, int count) {
    int i;
    addr_VDP(VDP_VRAM_WRITE, 32 * offset);
    for (i = 0; i < count; i++) {
	LONG(VDP_DATA) = buf[i];
    }
}

static void clear_zero_tile(void) {
    int i;
    addr_VDP(VDP_VRAM_WRITE, 0);
    for (i = 0; i < 16; i++) {
	WORD(VDP_DATA) = 0;
    }
}

u32 seed;
u32 random(void) {
    seed ^= seed << 13;
    seed ^= seed >> 17;
    seed ^= seed << 5;
    return seed;
}

u16 counter;
static void setup_game(void) {
    void display_canyon(void);
    display_canyon();
    clear_zero_tile();
    counter = 0;
}

static void red_alert(void) {
    int i;
    addr_VDP(VDP_CRAM_WRITE, 0);
    for (i = 0; i < 64; i++) {
	WORD(VDP_DATA) = 0x000e;
    }
}

static void advance_game(void) {
    counter++;
    if (is_vblank()) red_alert(); else wait_for_vblank();
}

static void display_update(void) {
    poke_VRAM(VRAM_SCROLL, -counter);
    WORD(VDP_DATA) = -(counter >> 1);
    if (!is_vblank()) red_alert(); else wait_for_draw();
}

void _start(void) {
    tmss();
    init_VDP();
    setup_game();
    wait_for_draw();
    for (;;) {
	advance_game();
	display_update();
    }
}
