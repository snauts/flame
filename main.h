#define HW_VER		0xa10001
#define TMSS_ADDR	0xa14000
#define VDP_DATA	0xc00000
#define VDP_CTRL	0xc00004

#define VDP_VRAM_WRITE	0x40000000
#define VDP_CRAM_WRITE	0xc0000000
#define VDP_SRAM_WRITE	0x40000010

#define VRAM_TILES	0x0000
#define VRAM_PLANE_A	0xC000
#define VRAM_PLANE_B	0xE000
#define VRAM_SPRITE	0xF000
#define VRAM_SCROLL	0xFC00

#define VRAM_BUF_SIZE	128

#define BIT(x) (1 << (x))

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

typedef unsigned char byte;
typedef unsigned short u16;
typedef unsigned int u32;

#define BYTE(x) (* (volatile byte *) (x))
#define WORD(x) (* (volatile u16 *) (x))
#define LONG(x) (* (volatile u32 *) (x))

#define VDP_CTRL_VALUE(flags, addr) \
    ((flags) | (((addr) & 0x3fff) << 16) | ((addr) >> 14))

void poke_VRAM(u16 addr, u16 data);
void fill_VRAM(u16 addr, u16 data, u16 count);

void update_tiles(const u32 *buf, int offset, int count);
void update_palette(const u16 *buf, int offset, int count);
void update_VRAM_word(u16 addr, u16 data);
void switch_frame(void (*fn)(void));

u32 random(void);
void set_seed(u32);

extern u16 counter;
