#include "main.h"

#define WINDOW_MIN	64
#define HEIGHT_DATA	2

u16 window;

const Trigger *trigger;

static short column;
static const u16 *back;
static const u16 *front;

static u16 next_platform;
static u16 prev_platform;
static const byte *height;

static const u16 *advance(const u16 *ptr) {
    short count = *ptr & 0xff;
    return ptr + count + 1;
}

static const u16 *recede(const u16 *ptr) {
    short count = *ptr >> 8;
    return ptr - count - 1;
}

static const u16 *fill_column(const u16 *ptr, void (*poke)(u16, u16)) {
    u16 addr = 0xd80 + column;
    short count = *(ptr++) & 0xff;
    while (addr > 0x80) {
	poke(addr, count > 0 ? *(ptr++) : 0);
	addr = addr - 0x80;
	count--;
    }
    return ptr;
}

static void update_VRAM(u16 addr, u16 data) {
    UPDATE_VRAM_WORD(VRAM_PLANE_A + addr, data);
}

static void update_column_forward(void (*poke)(u16, u16)) {
    front = fill_column(front, poke);
    column = (column + sizeof(u16)) & 0x7f;
    back = advance(back);
}

static void update_column_backward(void) {
    back = recede(back);
    column = (column - sizeof(u16)) & 0x7f;
    fill_column(back, &update_VRAM);
    front = recede(front);
}

u16 is_rightmost(void) {
    return (*front & 0xff) == 0;
}

u16 is_leftmost(void) {
    return (*back >> 8) == 0 && (window <= WINDOW_MIN);
}

static u16 get_platform_count(const byte *map) {
    return (map[0] & 0xf) - 1;
}

u16 platform_bottom(void) {
    u16 platform_count = get_platform_count(height);
    return platform_count > 0 ? height[1 + platform_count] : 0;
}

static void forward_platform(void) {
    prev_platform = next_platform;
    next_platform += 8 * height[1];
}

static void backward_platform(void) {
    next_platform = prev_platform;
    prev_platform -= 8 * height[1];
}

void update_height_map(u16 pos_x) {
    if (pos_x >= next_platform) {
	height += (*height & 0xf) + 1;
	forward_platform();
    }
    else if (pos_x < prev_platform) {
	height -= (*height >> 4) + 1;
	backward_platform();
    }
}

void prepare_level(const Level *level) {
    clear_DMA_buffer(0, 0x1000);
    trigger = level->triggers;
    height = level->height;

    column = 0;
    front = level->tiles;
    for (u16 x = 0; x < 64; x++) {
	update_column_forward(&poke_VRAM);
    }
    back = level->tiles; /* reset back */
    next_platform = 0;
    forward_platform();
    fill_bottom_row();

    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);
    setup_soldier_sprites();

    callback(&fade_in, 0, 6);
    switch_frame(&update_game);
}

void level_scroll(void) {
    UPDATE_VRAM_WORD(VRAM_SCROLL_A, -window);
    UPDATE_VRAM_WORD(VRAM_SCROLL_B, -(window >> 1));
}

static void execute_triggers(u16 x) {
    x = window + SCR_WIDTH;
    while (x > trigger->distance && trigger->fn != NULL) {
	trigger->fn(trigger->distance);
	trigger++;
    }
}

void reset_window(void) {
    schedule(&execute_triggers, 0);
    window = WINDOW_MIN;
    level_scroll();
}

static u16 boundary(u16 x) {
    return x & ~0x7;
}

void update_window(short direction) {
    u16 next, prev;
    prev = boundary(window);
    window += direction;
    next = boundary(window);
    if (next > prev) {
	update_column_forward(&update_VRAM);
    }
    if (prev > next) {
	update_column_backward();
    }
    execute_triggers(0);
}

static u16 get_snap_from_height(u16 prev, u16 next, const byte *map) {
    u16 count = get_platform_count(map);
    for (u16 i = 0; i < count; i++) {
	u16 snap = map[HEIGHT_DATA + i];
	if (prev <= snap && snap <= next) {
	    return snap;
	}
    }
    return 0;
}

const byte *find_height(u16 pos_x) {
    u16 next = next_platform;
    u16 prev = prev_platform;
    const byte *map = height;
    while (pos_x < prev) {
	map -= (*map >> 4) + 1;
	next = prev;
	prev -= 8 * map[1];
    }
    while (pos_x >= next) {
	map += (*map & 0xf) + 1;
	prev = next;
	next += 8 * map[1];
    }
    return map;
}

u16 get_snap(u16 pos_x, u16 prev, u16 next) {
    return get_snap_from_height(prev, next, find_height(pos_x));
}

u16 get_top(u16 pos_x) {
    return get_snap(pos_x, 0, SCR_HEIGHT);
}

#include "images/font.h"
#include "images/title.h"

static void load_font_tiles(void) {
    load_image(&font_img, 1, 0);
}

static void clear_screen_to_black(void) {
    clear_DMA_buffer(0, 0x1000);
    copy_to_VRAM(VRAM_PLANE_B, DMA_BUF_SIZE);
    copy_to_VRAM(VRAM_PLANE_A, DMA_BUF_SIZE);
}

const char special[] = "-!:";

static void display_text_plane(const char *text, u16 x, u16 y, u16 plane) {
    u16 i = 0, offset = (y << 7)  + (x << 1);
    while (text[i] != 0) {
	u16 tile = 0;
	char c = text[i];
	if ('A' <= c && c <= 'Z') {
	    tile = c - 'A' + 1;
	}
	else if ('a' <= c && c <= 'z') {
	    tile = c - 'a' + 1;
	}
	else if ('0' <= c && c <= '9') {
	    tile = c - '0' + 26 + 1;
	}
	else {
	    for (u16 j = 0; j < ARRAY_SIZE(special); j++) {
		if (c == special[j]) {
		    tile = j + 26 + 10 + 1;
		    break;
		}
	    }
	}
	poke_VRAM(i << 1, tile);
	i++;
    }
    copy_to_VRAM(plane + offset, 2 * strlen(text));
}

static void display_text(const char *text, u16 x, u16 y) {
    display_text_plane(text, x, y, VRAM_PLANE_A);
}

static void simple_screen(Function paint_screen, u16 offset, byte start) {
    window = offset;
    level_scroll();
    music_none();
    load_font_tiles();
    reset_sprite_table();
    clear_screen_to_black();
    paint_screen();
    wait_for_start(start);
}

static void flammenwerfer_text(void) {
    load_image(&title_img, 256, 1);

    clear_DMA_buffer(0, 0x1000);
    paint_background(0, 0, 40, 8, TILE(1, 256), 0);
    copy_to_VRAM(VRAM_PLANE_A + 0x400, 80 * 8 * 2);

    display_text("PRESS START", 15, 16);
    display_text("SATIRICAL ODDITY IN FOUR PARTS", 5, 27);
}

void display_title(void) {
    simple_screen(&flammenwerfer_text, 0, 0);
}

static void end_game_text(void) {
    display_text("GAME OVER", 16, 12);
    display_text("BEER FOREVER!", 14, 14);
}

static const byte sing_intervals[] = {
    32 + 32,
    48, 16 + 32 + 32,
    48, 16 + 16 + 16 + 32,
    48, 16 + 32 + 32 + 64,
    48, 16, 48, 16,
    16, 16, 16, 16,
    48, 16 + 32 + 32,
    48, 16 + 32 + 32 + 48 + 16 + 8,
};

static void lip_sync_doves(u16 i) {
    soldiers_sing(i & 1);
    u16 wait = sing_intervals[i] - 1;
    i = (i == ARRAY_SIZE(sing_intervals) - 1 ? 0 : i + 1);
    callback(&lip_sync_doves, wait, i);
}

void display_ending(void) {
    simple_screen(&end_game_text, 4, 0);
    extern void music_doves(void);
    all_soldiers_march();
    lip_sync_doves(0);
    music_doves();
}

static void start_level(u16 tmp) {
    fade_to_next_level();
    fade_music(0);
}

static void announcement(Function paint_screen) {
    simple_screen(paint_screen, 0, 1);
    schedule(&start_level, 150);
}

static void johnny_text(void) {
    display_text("- PART 1 -", 15, 3);
    display_text("JOHNNY", 17, 13);
}

void announce_johnny(void) {
    announcement(&johnny_text);
}

static void hans_text(void) {
    display_text("- PART 2 -", 15, 3);
    display_text("HANS", 18, 13);
}

void announce_hans(void) {
    announcement(&hans_text);
}

static void hiroshi_text(void) {
    display_text("- PART 3 -", 15, 3);
    display_text_plane("HIROSHI", 16, 13, VRAM_PLANE_B);
    UPDATE_VRAM_WORD(VRAM_SCROLL_B, 4);
}

void announce_hiroshi(void) {
    announcement(&hiroshi_text);
}

static void emile_text(void) {
    display_text("- PART 4 -", 15, 3);
    display_text_plane("EMILE", 17, 13, VRAM_PLANE_B);
    UPDATE_VRAM_WORD(VRAM_SCROLL_B, 4);
}

void announce_emile(void) {
    announcement(&emile_text);
}

const char *error_str;
static void error_text(void) {
    display_text("ERROR:", 2, 0);
    display_text(error_str, 9, 0);
}

static void display_error(void) {
    simple_screen(&error_text, 4, 0);
}

void error(const char *str) {
    error_str = str;
    dim_palette(8);
    switch_frame(display_error);
}

void num_error(u32 num) {
    static char num_str[9];
    for (u16 i = 0; i < 8; i++) {
	char hex = (num & 0xf);
	num_str[7 - i] = (hex < 10 ? '0' : 'A'- 10) + hex;
	num = num >> 4;
    }
    num_str[8] = 0;
    error(num_str);
}
