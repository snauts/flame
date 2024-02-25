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
#define MAX_MOBS	32
#define MAX_POSITION	496
#define BAR_SIZE	24
#define BAR_HEALTH	(8 * BAR_SIZE)

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
    signed char place;
    signed char gravity;
    signed char velocity;
    signed char direction;
    Sprite *sprite;
    void *private;
    byte death;
    u16 flags;
    u16 frame;
    u16 life;
} Object;

#define O_PERSISTENT	BIT(0)
#define O_PROJECTILE	BIT(1)

typedef void(*Callback)(u16);
typedef void(*Function)(void);
typedef void(*Operator)(Object *);

typedef struct Trigger {
    u16 distance;
    Callback fn;
} Trigger;

typedef struct Rectangle {
    short x1, y1, x2, y2;
} Rectangle;

typedef struct Pos {
    short x, y;
} Pos;

#define NULL ((void *) 0)

#define BYTE(x) (* (volatile byte *) (x))
#define WORD(x) (* (volatile u16 *) (x))
#define LONG(x) (* (volatile u32 *) (x))

#define TILE(p, i) (((p) << 13) | (i))
#define TILE_ID(tile_cfg) ((tile_cfg) & 0x7FF)
#define SPRITE_SIZE(x, y) ((((x) - 1) << 2) | ((y) - 1))

#define VDP_CTRL_REG(reg, val) (BIT(15) | ((reg) << 8) | (val))

#define VDP_CTRL_VALUE(flags, addr) \
    ((flags) | (((addr) & 0x3fff) << 16) | ((addr) >> 14))

#define UPDATE_VRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_VRAM_WRITE, addr), data);

#define UPDATE_CRAM_WORD(addr, data) \
    update_VDP_word(VDP_CTRL_VALUE(VDP_CRAM_WRITE, addr), data);

#define UPDATE_TILES(tile_set, offset) \
    update_tiles(tile_set, offset, ARRAY_SIZE(tile_set));

#define SCREEN_X(x) ((x) - window + ON_SCREEN)

void poke_VRAM(u16 addr, u16 data);
void fill_VRAM(u16 addr, u16 data, u16 count);

short abs(short value);
u16 strlen(const char *str);
short clamp(short value, short max);
void memset(void *ptr, byte c, int amount);
void memcpy(void *dst, const void *src, int amount);
void update_tiles(const byte *buf, u16 offset, u16 count);
void update_palette(const u16 *buf, u16 offset, u16 count);
void update_VDP_word(u32 ctrl, u16 data);
void copy_to_VRAM_ptr(u16 dst, u16 len, void *ptr);
void *copy_to_VRAM_async(u16 dst, u16 len);
void copy_to_VRAM(u16 dst, u16 len);
void clear_DMA_buffer(u16 data, u16 len);
void update_color(u16 idx, u16 color);
u16 dim_color(u16 color, u16 dim);
u16 get_palette_color(u16 i);
void upload_palette(u16 dim);
void wait_for_interrupt(void);
void wait_vblank_done(void);
void wait_for_start(byte);
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
void fade_music(u16 i);
void music_none(void);
void perish_sfx(void);

u16 random(void);
void set_seed(u16);

Object *get_soldier(void);
void advance_sprites(void);
void fade_to_next_level(void);
void soldier_fist_pump(void);
void soldiers_sing(int state);
void all_soldiers_march(void);
void load_soldier_tiles(u16 id);
void reset_sprite_table(void);
void setup_soldier_sprites(void);
byte update_next_sprite(byte new_value);
void advance_y(Object *obj, char gravity);
void set_sprite_tile(Sprite *sprite, u16 tile);
u16 advance_obj(Object *obj, u16 offset, u16 gravity);
u16 soldier_collision(Rectangle *r);
Object *flame_collision(Rectangle *r);
void display_progress_bar(void);
u16 decrement_progress_bar(void);
void bite_soldier(u16 x, u16 y);
Sprite *get_sprite(u16 offset);
void lock_screen(byte state);
void fill_bottom_row(void);
void finish_level(u16 i);
void fade_in(u16 fade);

void display_title(void);
void display_ending(void);
void display_canyon(void);
void display_rusty(void);
void display_mantis(void);
void display_mountains(void);
void display_plateau(void);
void display_queen(void);
void display_beach(void);
void display_dunes(void);
void announce_johnny(void);
void announce_hans(void);
void announce_hiroshi(void);
void prepare_desert_level(void);
void prepare_rusty_level(void);
void prepare_mantis_level(void);
void prepare_mountain_level(void);
void prepare_queen_level(void);

u16 platform_bottom(void);
void load_burn_tiles(u16 where);
void update_height_map(u16 pos_x);
void update_window(short direction);
void paint_background(u16, u16, u16, u16, u16, u16);
u16 get_snap(u16 pos_x, u16 prev, u16 next);
void error(const char *str);
void num_error(u32 num);
void reset_window(void);
void level_scroll(void);
u16 get_top(u16 pos_x);
u16 is_rightmost(void);
u16 is_leftmost(void);

Object *alloc_mob(void);
void schedule(Callback, u16 ticks);
void callback(Callback, u16 timeout, u16 cookie);
void mob_fn(Object *obj, void (*fn)(Object *));
void apply_to_all_mobs(void (*fn)(Object *));
void cancel_timer(Callback fn);
void manage_timers(void);
void manage_mobs(void);
void purge_mobs(void);
void reset_mobs(void);
void free_mob(Object *obj);
u16 mob_index(Object *obj);
Object *get_mob(u16 index);

Object *setup_obj(short x, short y, byte size);
void update_hitbox(Object *obj, Rectangle *dst, const Rectangle *src, u16 n);
u16 boss_hitbox(Object *obj, const Rectangle *base, u16 size, u16 skip);
char mob_cycle(Object *obj, u16 last_frame);
char mob_move(Object *obj, u16 last_frame);
void mob_adjust_sprite_dir(Object *obj);
void setup_burns(u16 count, u16 tiles);
void flame_burn(Object *obj, u16 i);
char is_mob_alive(Object *obj);
void kill_mob(Object *obj);
void init_burn(Object *obj);
void free_burns(void);

extern u16 window;
extern u16 counter;
