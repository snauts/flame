#define HW_VER		0xA10001
#define TMSS_ADDR	0xA14000
#define VDP_DATA	0xC00000
#define VDP_CTRL	0xC00004

#define VDP_VRAM_WRITE	0x40000000
#define VDP_CRAM_WRITE	0xC0000000
#define VDP_SRAM_WRITE	0x40000010
#define VDP_DMA_FILL	0x40000080

#define VRAM_TILES	0x0000
#define VRAM_PLANE_A	0xC000
#define VRAM_PLANE_B	0xE000
#define VRAM_SPRITE	0xF000
#define VRAM_SCROLL_A	0xFC00
#define VRAM_SCROLL_B	0xFC02

#define VRAM_BUF_SIZE	64

#define GAMEPAD_A_CTRL	0xA10009
#define GAMEPAD_A_DATA	0xA10003
#define GAMEPAD_B_CTRL	0xA1000B
#define GAMEPAD_B_DATA	0xA10005

#define BIT(x) (1 << (x))

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

typedef unsigned char byte;
typedef unsigned short u16;
typedef unsigned int u32;

#define BYTE(x) (* (volatile byte *) (x))
#define WORD(x) (* (volatile u16 *) (x))
#define LONG(x) (* (volatile u32 *) (x))

#define TILE(p, i) (((p) << 13) | (i))
#define SPRITE(x, y, n) ((((x) - 1) << 10) | (((y) - 1) << 8) | (n))

#define VDP_CTRL_REG(reg, val) (BIT(15) | ((reg) << 8) | (val))

#define VDP_CTRL_VALUE(flags, addr) \
    ((flags) | (((addr) & 0x3fff) << 16) | ((addr) >> 14))

#define UPDATE_VRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_VRAM_WRITE, addr), data);

#define UPDATE_CRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_CRAM_WRITE, addr), data);

void poke_VRAM(u16 addr, u16 data);
void fill_VRAM(u16 addr, u16 data, u16 count);

void update_tiles(const byte *buf, u16 offset, u16 count);
void update_palette(const u16 *buf, u16 offset, u16 count);
void update_VDP_word(u32 ctrl, u16 data);
void switch_frame(void (*fn)(void));
void wait_for_interrupt(void);
void enable_interrupts(void);

u16 random(void);
void set_seed(u16);

u16 soldier_march(void);

extern u16 counter;
