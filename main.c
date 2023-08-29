#include "main.h"

static void (*game_frame)(void);

static void addr_VDP(u32 flags, u16 addr) {
    LONG(VDP_CTRL) = VDP_CTRL_VALUE(flags, addr);
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
    if (WORD(VDP_CTRL) & 0x1) WORD(VDP_CTRL) = 0x817C; /* PAL/NTSC */
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

void update_palette(const u16 *buf, u16 offset, u16 count) {
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

static void update_byte(byte octet) {
    static byte flip;
    static byte save;
    if (flip) {
	WORD(VDP_DATA) = (save << 8) | octet;
    }
    else {
	save = octet;
    }
    flip = !flip;
}

void update_tiles(const byte *buf, u16 offset, u16 count) {
    u16 i = 0;
    addr_VDP(VDP_VRAM_WRITE, 32 * offset);
    while (i < count) {
	byte pixel = buf[i++];
	if ((pixel & 0xc0) == 0xc0) {
	    byte repeat = buf[i++];
	    byte times = pixel & 0x3f;
	    for (u16 j = 0; j < times; j++) {
		update_byte(repeat);
	    }
	}
	else {
	    update_byte(pixel);
	}
    }
}

static void clear_zero_tile(void) {
    int i;
    addr_VDP(VDP_VRAM_WRITE, 0);
    for (i = 0; i < 16; i++) {
	WORD(VDP_DATA) = 0;
    }
}

static u16 seed;

void set_seed(u16 new) {
    seed = new;
}

u16 random(void) {
    seed ^= seed << 7;
    seed ^= seed >> 9;
    seed ^= seed << 8;
    return seed;
}

u16 counter;
static void setup_game(void) {
    void display_canyon(void);
    game_frame = &display_canyon;
    clear_zero_tile();
    counter = 0;
}

static void alert(u16 color) {
    int i;
    addr_VDP(VDP_CRAM_WRITE, 0);
    for (i = 0; i < 64; i++) {
	WORD(VDP_DATA) = color;
    }
    for (;;) { } /* hang */
}

static u16 vram_idx;
static u32 vram_addr[VRAM_BUF_SIZE];
static u16 vram_data[VRAM_BUF_SIZE];

void update_VDP_word(u32 ctrl, u16 data) {
    if (vram_idx < VRAM_BUF_SIZE) {
	vram_addr[vram_idx] = ctrl;
	vram_data[vram_idx] = data;
	vram_idx++;
    }
    else {
	alert(0x0080);
    }
}

void switch_frame(void (*fn)(void)) {
    game_frame = fn;
    wait_for_draw();
}

static void panic_on_vblank(void) {
    if (is_vblank()) alert(0x0008); else wait_for_vblank();
}

static void advance_game(void) {
    counter++;
    vram_idx = 0;
    wait_for_draw();
    game_frame();
    panic_on_vblank();
}

static void panic_on_draw(void) {
    if (!is_vblank()) alert(0x000e); else wait_for_draw();
}

static void display_update(void) {
    u16 index = 0;
    while (index < vram_idx) {
	LONG(VDP_CTRL) = vram_addr[index];
	WORD(VDP_DATA) = vram_data[index];
	index++;
    }
    panic_on_draw();
}

void _start(void) {
    tmss();
    init_VDP();
    setup_game();
    for (;;) {
	advance_game();
	display_update();
    }
}
