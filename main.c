#include "main.h"

static Function game_frame;

static const byte z80[] = {
#include "z80.hex"
};

u16 strlen(const char *str) {
    u16 len = 0;
    while (str[len] != 0) { len++; }
    return len;
}

short abs(short value) {
    return value < 0 ? -value : value;
}

short clamp(short value, short max) {
    if (value > max) {
	return max;
    }
    else if (value < -max) {
	return -max;
    }
    else {
	return value;
    }
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

void execute_nops(u32 nops) {
    while (nops-- > 0) { asm("nop"); }
}

static void z80_request_bus(void) {
    WORD(Z80_BUS) = BIT(8);
    while (WORD(Z80_BUS) & BIT(8));
}

static void z80_release_bus(void) {
    WORD(Z80_BUS) = 0;
}

static void z80_reset(byte state) {
    WORD(Z80_RST) = state ? BIT(8) : 0;
}

void do_z80_bus(void (*z80_bus_access)(void)) {
    z80_request_bus();
    z80_bus_access();
    z80_release_bus();
}

void z80_word(u16 addr, u16 data) {
    z80_request_bus();
    BYTE(Z80_RAM + addr + 1) = data >> 8;
    BYTE(Z80_RAM + addr + 0) = data & 0xff;
    z80_release_bus();
}

void z80_poke(u16 addr, byte data) {
    z80_request_bus();
    BYTE(Z80_RAM + addr) = data;
    z80_release_bus();
}

static void init_z80(void) {
    z80_reset(1);
    z80_request_bus();
    memcpy((void *) Z80_RAM, z80, sizeof(z80));
    z80_release_bus();
    z80_reset(0);
    execute_nops(80);
    z80_reset(1);
}

static void init_sys(void) {
    byte i;
    for (i = 0; i < ARRAY_SIZE(VDP_regs); i++) {
	WORD(VDP_CTRL) = VDP_CTRL_REG(i, VDP_regs[i]);
    }
    if (is_PAL()) WORD(VDP_CTRL) = VDP_CTRL_REG(0x01, 0x7C); /* PAL/NTSC */

    /* init gamepad a */
    BYTE(GAMEPAD_A_CTRL) = BIT(6);

    init_z80();
    init_ym2612();
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
    do {
	wait_for_interrupt();
    } while (!vblank_done);
}

static u16 palette[64];
static u16 color_base[64];

u16 get_palette_color(u16 i) {
    return color_base[i];
}

void update_palette(const u16 *buf, u16 offset, u16 count) {
    u16 i;
    for (i = 0; i < count; i++) {
	color_base[offset + i] = buf[i];
    }
}

static u16 dim_color(u16 color, u16 dim) {
    dim = dim << 1;
    for (u16 mask = 0xf; mask != 0xf000; mask <<= 4) {
	color = (color & mask) > dim ? color - dim : color & ~mask;
	dim <<= 4;
    }
    return color;
}

/* dimming is expensive, use with care */
static byte total_dimming;
void dim_palette(u16 dim) {
    total_dimming = dim;
}

static void upload_palette(void) {
    u16 *buffer;
    if (total_dimming == 0) {
	buffer = color_base;
    }
    else {
	buffer = palette;
	for (u16 i = 0; i < 64; i++) {
	    buffer[i] = dim_color(color_base[i], total_dimming);
	}
    }
    copy_to_VRAM_ptr(0, sizeof(palette) | BIT(0), buffer);
}

void update_color(u16 idx, u16 color) {
    color_base[idx] = color;
}

typedef struct DMA_Chunk {
    u16 dst;
    u16 len;
    void *ptr;
} DMA_Chunk;

static byte dma_buf[DMA_BUF_SIZE];
static DMA_Chunk chunk[DMA_CHUNKS];
static u16 chunk_idx, buf_offset;

void copy_to_VRAM_ptr(u16 dst, u16 len, void *ptr) {
    chunk[chunk_idx].dst = dst;
    chunk[chunk_idx].len = len;
    chunk[chunk_idx].ptr = ptr;
    chunk_idx++;
}

void *copy_to_VRAM_async(u16 dst, u16 len) {
    void *buf = dma_buf + buf_offset;
    copy_to_VRAM_ptr(dst, len, buf);
    buf_offset += len;
    return buf;
}

void copy_to_VRAM(u16 dst, u16 len) {
    copy_to_VRAM_async(dst, len);
    wait_vblank_done();
}

static u16 is_palette(DMA_Chunk *chunk) {
    return chunk->len & BIT(0);
}

static void copy_using_DMA(void) {
    for (u16 i = 0; i < chunk_idx; i++) {
	u32 dma_src = ((u32) chunk[i].ptr);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x13, (chunk[i].len >>  1) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x14, (chunk[i].len >>  9));
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x15, (dma_src >>  1) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x16, (dma_src >>  9) & 0xFF);
	WORD(VDP_CTRL) = VDP_CTRL_REG(0x17, (dma_src >> 17) & 0x7F);
	u32 addr = is_palette(chunk + i) ? VDP_CRAM_DMA : VDP_VRAM_DMA;
	LONG(VDP_CTRL) = VDP_CTRL_VALUE(addr, chunk[i].dst);
	while (is_DMA());
    }

    /* reset chunks */
    buf_offset = 0;
    chunk_idx = 0;
}

void *buffer_ptr(u16 addr) {
    return dma_buf + buf_offset + addr;
}

void poke_VRAM(u16 addr, u16 data) {
    * (u16 *) buffer_ptr(addr) = data;
}

void fill_VRAM(u16 addr, u16 data, u16 count) {
    u16 *ptr = buffer_ptr(addr);
    for (u16 i = 0; i < count; i++) {
	ptr[i] = data;
    }
}

void clear_DMA_buffer(u16 data, u16 len) {
    fill_VRAM(0, data, len >> 1);
}

void update_tiles(const byte *buf, u16 offset, u16 count) {
    byte *ptr = buffer_ptr(0);
    u16 i = 0, n = 0;
    offset *= 32;
    while (i < count) {
	byte times = 1;
	byte pixel = buf[i++];
	if ((pixel & 0xc0) == 0xc0) {
	    times = pixel & 0x3f;
	    pixel = buf[i++];
	}
	for (byte j = 0; j < times; j++) {
	    ptr[n++] = pixel;
	    if (n >= DMA_BUF_SIZE) {
		copy_to_VRAM(offset, n);
		offset += n;
		n = 0;
	    }
	}
    }
    if (n > 0) copy_to_VRAM(offset, n);
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

static u16 free_mem;
static byte heap[HEAP_SIZE];
void *malloc(u16 amount) {
    void *ptr = heap + free_mem;
    free_mem += amount;
    memset(ptr, 0, amount);
    if (free_mem > HEAP_SIZE) {
	error("MALLOC-FAIL");
    }
    return ptr;
}

void reset_heap(void) {
    free_mem = 0;
}

void memcpy(void *dst, const void *src, int amount) {
    for (int i = 0; i < amount; i++) ((byte *) dst)[i] = ((byte *) src)[i];
}

void memset(void *ptr, byte c, int amount) {
    for (int i = 0; i < amount; i++) ((byte *) ptr)[i] = c;
}

static const Function loader_table[] = {
    &display_title,

    &announce_johnny,
    &display_canyon,
    &display_rusty,
    &display_mantis,

    &announce_hans,
    &display_mountains,
    &display_plateau,
    &display_queen,

    &announce_hiroshi,
    &display_beach,
    &display_dunes,
    &display_hermit,

//    &announce_emile,
//    &display_town,

    &display_ending,
    NULL,
};
const Function *loader;

void next_level(void) {
    loader = (loader[1] == NULL ? loader_table : loader + 1);
}

void scroll_type(byte value) {
    WORD(VDP_CTRL) = VDP_CTRL_REG(0xB, value);
}

void restart_level(void) {
    reset_heap();
    dim_palette(8);
    scroll_type(0x00);
    switch_frame(*loader);
}

u16 counter;
static void init_variables(void) {
    extern byte bss_start, bss_end;
    memset(&bss_start, 0, (u32) (&bss_end - &bss_start));
    loader = loader_table;
    restart_level();
    counter = 0;
}

static void setup_game(void) {
    init_variables();
    transparent_tile();
    while (!is_vblank());
    wait_vblank_done();
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

void switch_frame(Function fn) {
    game_frame = fn;
}

static void panic_on_draw(void) {
    if (!is_vblank()) error("VBLANK");
}

static void transfer_to_VRAM(void) {
    u16 index = 0;
    while (index < vram_idx) {
	LONG(VDP_CTRL) = vram_addr[index];
	WORD(VDP_DATA) = vram_data[index];
	index++;
    }
    vram_idx = 0;
}

void vblank_interrupt(void) {
    copy_using_DMA();
    transfer_to_VRAM();
    panic_on_draw();
    vblank_done = 1;
    counter++;
}

void _start(void) {
    tmss();
    init_sys();
    setup_game();
    for (;;) {
	game_frame();
	upload_palette();
	wait_vblank_done();
    }
}
