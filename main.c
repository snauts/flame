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
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80
};

static u16 is_PAL(void) {
    return WORD(VDP_CTRL) & BIT(0);
}

static void init_sys(void) {
    char i;
    for (i = 0; i < ARRAY_SIZE(VDP_regs); i++) {
	WORD(VDP_CTRL) = VDP_CTRL_REG(i, VDP_regs[i]);
    }
    if (is_PAL()) WORD(VDP_CTRL) = VDP_CTRL_REG(0x01, 0x7C); /* PAL/NTSC */

    /* init gamepad a */
    BYTE(GAMEPAD_A_CTRL) = BIT(6);
}

static u16 is_vblank(void) {
    return WORD(VDP_CTRL) & BIT(3);
}

static u16 is_DMA(void) {
    return WORD(VDP_CTRL) & BIT(1);
}

static volatile byte vblank_done;
void wait_vblank_done(void) {
    vblank_done = 0;
    enable_interrupts();
    while (!vblank_done);
}

void update_palette(const u16 *buf, u16 offset, u16 count) {
    u16 i;
    for (i = 0; i < count; i++) {
	UPDATE_CRAM_WORD(2 * (offset + i), buf[i]);
    }
}

static u16 dma_dst, dma_len;
static byte dma_buf[DMA_BUF_SIZE];

void copy_to_VRAM_async(u16 dst, u16 len) {
    dma_dst = dst;
    dma_len = len;
}

void copy_to_VRAM(u16 dst, u16 len) {
    copy_to_VRAM_async(dst, len);
    wait_vblank_done();
}

Sprite *get_sprite_buf(void) {
    return (Sprite *) dma_buf;
}

static void copy_using_DMA(void) {
    if (dma_len > 0) {
	u32 dma_src = ((u32) dma_buf);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x13, (dma_len >>  1) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x14, (dma_len >>  9));
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x15, (dma_src >>  1) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x16, (dma_src >>  9) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x17, (dma_src >> 17) & 0x7F);
	LONG(VDP_CTRL) = VDP_CTRL_VALUE(VDP_VRAM_DMA, dma_dst);
	dma_dst += dma_len;
	dma_len = 0;
	while (is_DMA());
    }
}

void poke_VRAM(u16 addr, u16 data) {
    * (u16 *) (dma_buf + addr) = data;
}

void fill_VRAM(u16 addr, u16 data, u16 count) {
    u16 *ptr = (u16 *) (dma_buf + addr);
    for (u16 i = 0; i < count; i++) {
	ptr[i] = data;
    }
}

void clear_DMA_buffer(u16 data) {
    fill_VRAM(0, data, DMA_BUF_SIZE >> 1);
}

void update_tiles(const byte *buf, u16 offset, u16 count) {
    u16 i = 0, n = 0;
    dma_dst = 32 * offset;
    while (i < count) {
	byte times = 1;
	byte pixel = buf[i++];
	if ((pixel & 0xc0) == 0xc0) {
	    times = pixel & 0x3f;
	    pixel = buf[i++];
	}
	for (byte j = 0; j < times; j++) {
	    dma_buf[n++] = pixel;
	    if (n >= DMA_BUF_SIZE) {
		copy_to_VRAM(dma_dst, n);
		n = 0;
	    }
	}
    }
    copy_to_VRAM(dma_dst, n);
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

static void transparent_tile(void) {
    for (u16 i = 0; i < 32; i += 2) {
	UPDATE_VRAM_WORD(i, 0);
    }
}

u16 counter;
static void setup_game(void) {
    void display_canyon(void);
    game_frame = &display_canyon;
    while (!is_vblank());
    wait_vblank_done();
    transparent_tile();
    counter = 0;
}

static void alert(u16 color) {
    u16 i;
    for (;;) {
	addr_VDP(VDP_CRAM_WRITE, 0);
	for (i = 0; i < 64; i++) {
	    WORD(VDP_DATA) = color;
	}
    }
}

static u16 vram_idx;
static u32 vram_addr[VRAM_BUF_SIZE];
static u16 vram_data[VRAM_BUF_SIZE];

void update_VDP_word(u32 ctrl, u16 data) {
    if (vram_idx >= VRAM_BUF_SIZE) {
	wait_vblank_done();
    }
    vram_addr[vram_idx] = ctrl;
    vram_data[vram_idx] = data;
    vram_idx++;
}

void switch_frame(void (*fn)(void)) {
    game_frame = fn;
}

static void panic_on_draw(void) {
    if (!is_vblank()) alert(7 << 1);
}

void vblank_interrupt(void) {
    u16 index = 0;
    copy_using_DMA();
    while (index < vram_idx) {
	LONG(VDP_CTRL) = vram_addr[index];
	WORD(VDP_DATA) = vram_data[index];
	index++;
    }
    panic_on_draw();
    vblank_done = 1;
    vram_idx = 0;
    counter++;
}

void _start(void) {
    tmss();
    init_sys();
    setup_game();
    for (;;) {
	game_frame();
	wait_vblank_done();
    }
}
