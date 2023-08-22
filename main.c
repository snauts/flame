#include "main.h"

#define HW_VER		0xa10001
#define TMSS_ADDR	0xa14000
#define VDP_DATA	0xc00000
#define VDP_CTRL	0xc00004

#define VDP_VRAM_WRITE	0x40000000
#define VDP_CRAM_WRITE	0xc0000000
#define VDP_SRAM_WRITE	0x40000010

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

static void update_palette(const u16 *buf, int offset, int count) {
    int i;
    addr_VDP(VDP_CRAM_WRITE, offset);
    for (i = 0; i < count; i++) {
	WORD(VDP_DATA) = buf[i];
    }
}

void _start(void) {
    tmss();
    init_VDP();
    for (;;) {
	wait_for_vblank();
    }
}
