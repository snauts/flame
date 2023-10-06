#define HW_VER		0xA10001
#define TMSS_ADDR	0xA14000
#define VDP_DATA	0xC00000
#define VDP_CTRL	0xC00004
#define PSG_ADDR	0xC00011

#define VDP_VRAM_WRITE	0x40000000
#define VDP_CRAM_WRITE	0xC0000000
#define VDP_SRAM_WRITE	0x40000010
#define VDP_VRAM_DMA	0x40000080
#define VDP_CRAM_DMA	0xC0000080

#define VRAM_TILES	0x0000
#define VRAM_PLANE_A	0xC000
#define VRAM_PLANE_B	0xE000
#define VRAM_SPRITE	0xF000
#define VRAM_SCROLL_A	0xFC00
#define VRAM_SCROLL_B	0xFC02

#define HEAP_SIZE	4096
#define VRAM_BUF_SIZE	128
#define DMA_BUF_SIZE	4096
#define DMA_CHUNKS	8
#define ON_SCREEN	128
#define SCR_WIDTH	320
#define SCR_HEIGHT	224
#define MAX_MOBS	16
#define MAX_POSITION	512

#define GAMEPAD_A_CTRL	0xA10009
#define GAMEPAD_A_DATA	0xA10003
#define GAMEPAD_B_CTRL	0xA1000B
#define GAMEPAD_B_DATA	0xA10005

#define Z80_RAM		0xA00000
#define Z80_BUS		0xA11100
#define Z80_RST		0xA11200

#define YM2612_REG	0xA04000

#define BIT(x) (1 << (x))

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

#define CONTAINER_OF(ptr, type, member) \
    ({ ((type *)(((void *)(ptr)) - __builtin_offsetof(type, member))); })

typedef unsigned char byte;
typedef unsigned short u16;
typedef unsigned int u32;

typedef struct Sprite {
    u16 y;
    byte size;
    byte next;
    u16 cfg;
    u16 x;
} Sprite;

typedef struct Object {
    short x, y;
    char gravity;
    char velocity;
    char direction;
    Sprite *sprite;
    void *private;
    u16 frame;
    u16 life;
} Object;

typedef void(*Callback)(u16);
typedef void(*Function)(void);

typedef struct Trigger {
    u16 distance;
    Callback fn;
} Trigger;

typedef struct Rectangle {
    u16 x1, y1, x2, y2;
} Rectangle;

typedef struct Pos {
    short x, y;
} Pos;

#define NULL ((void *) 0)

#define BYTE(x) (* (volatile byte *) (x))
#define WORD(x) (* (volatile u16 *) (x))
#define LONG(x) (* (volatile u32 *) (x))

#define TILE(p, i) (((p) << 13) | (i))
#define SPRITE_SIZE(x, y) ((((x) - 1) << 2) | ((y) - 1))

#define VDP_CTRL_REG(reg, val) (BIT(15) | ((reg) << 8) | (val))

#define VDP_CTRL_VALUE(flags, addr) \
    ((flags) | (((addr) & 0x3fff) << 16) | ((addr) >> 14))

#define UPDATE_VRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_VRAM_WRITE, addr), data);

#define UPDATE_CRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_CRAM_WRITE, addr), data);

#define SCREEN_X(x) ((x) - window + ON_SCREEN)

static inline char is_good_object(Object *o) {
    return o->sprite->x > 0;
}

static inline void destroy_object(Object *o) {
    o->sprite->x = o->sprite->y = 0;
}

void poke_VRAM(u16 addr, u16 data);
void fill_VRAM(u16 addr, u16 data, u16 count);

void memset(void *ptr, byte c, int amount);
void memcpy(void *dst, const void *src, int amount);
void update_tiles(const byte *buf, u16 offset, u16 count);
void update_palette(const u16 *buf, u16 offset, u16 count);
void update_VDP_word(u32 ctrl, u16 data);
void copy_to_VRAM_ptr(u16 dst, u16 len, void *ptr);
void *copy_to_VRAM_async(u16 dst, u16 len);
void copy_to_VRAM(u16 dst, u16 len);
void clear_DMA_buffer(u16 data, u16 len);
u16 dim_color(u16 color, u16 dim);
void upload_palette(u16 dim);
void wait_for_interrupt(void);
void wait_vblank_done(void);
void *buffer_ptr(u16 addr);
void *malloc(u16 amount);
void update_game(void);
void reset_heap(void);

void switch_frame(Function fn);
void restart_level(void);
void next_level(void);

void init_ym2612(void);
void execute_nops(u32 nops);
void do_z80_bus(void (*)(void));
void z80_word(u16 addr, u16 data);
void z80_poke(u16 addr, byte data);
void psg_noise(byte type, byte vol);
void music_toggle(byte state);

u16 random(void);
void set_seed(u16);

void advance_sprites(void);
void load_soldier_tiles(void);
void setup_soldier_sprites(void);
u16 advance_obj(Object *obj, u16 offset, u16 gravity);
u16 soldier_collision(Rectangle *r);
u16 flame_collision(Rectangle *r);
void bite_soldier(u16 x, u16 y);
Sprite *get_sprite(u16 offset);
void lock_screen(byte state);
void fade_in(u16 fade);

void display_canyon(void);
void display_rusty(void);
void display_mantis(void);
void prepare_desert_level(void);
void prepare_rusty_level(void);
void prepare_mantis_level(void);

u16 platform_bottom(void);
void update_height_map(u16 pos_x);
void update_window(short direction);
u16 get_snap(u16 pos_x, u16 prev, u16 next);
void reset_window(void);
void level_scroll(void);
u16 is_rightmost(void);
u16 is_leftmost(void);

Object *alloc_mob(byte cost);
void schedule(Callback, u16 ticks);
void callback(Callback, u16 timeout, u16 cookie);
void mob_fn(Object *obj, void (*fn)(Object *));
void manage_timers(void);
void manage_mobs(void);
void purge_mobs(void);
void reset_mobs(void);
void free_mob(Object *obj);
u16 mob_index(Object *obj);
Object *get_mob(u16 index);

extern u16 window;
extern u16 counter;
