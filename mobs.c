#include "main.h"

#define MAX_BUDGET	16
#define MAX_MOBS	8
#define MOB_OFFSET	16
#define NEXT_GROUP	4

byte first_mob_sprite;
static Mob m_obj[MAX_MOBS];
static char free[MAX_MOBS];
static char available;
static Sprite *sprite;
static char mob_head;
static byte budget;

#define MAX_TIMERS	8

typedef struct Timer {
    u16 cookie;
    u16 timeout;
    void (*fn)(u16);
} Timer;

static char available_timers;
static Timer timers[MAX_TIMERS];
static char free_timers[MAX_TIMERS];

static void init_mob(Mob *mob) {
    if (mob_head >= 0) {
	m_obj[mob_head].previous = mob->index;
    }
    mob->sprite->x = 1;
    mob->sprite->next = first_mob_sprite;
    first_mob_sprite = mob->index + MOB_OFFSET;
    mob_head = mob->index;
    budget -= mob->price;
    mob->previous = -1;
}

Mob *alloc_mob(byte cost) {
    Mob *mob = NULL;
    if (budget >= cost && available > 0) {
	mob = m_obj + free[--available];
	mob->price = cost;
	init_mob(mob);
    }
    return mob;
}

void free_mob(Mob *mob) {
    Sprite *img = mob->sprite;
    char next, i = mob->index;
    free[available++] = i;
    img->x = img->y = 0;
    budget += mob->price;
    next = img->next - MOB_OFFSET;
    if (mob->previous < 0) {
	first_mob_sprite = img->next;
	mob_head = next;
    }
    else {
	sprite[mob->previous].next = img->next;
    }
    if (next >= 0) {
	m_obj[next].previous = mob->previous;
    }
}

void reset_mobs(void) {
    mob_head = -1;
    budget = MAX_BUDGET;
    available = MAX_MOBS;
    first_mob_sprite = NEXT_GROUP;
    sprite = get_sprite(MOB_OFFSET);
    for (char i = 0; i < MAX_MOBS; i++) {
	m_obj[i].sprite = sprite + i;
	m_obj[i].index = i;
	sprite[i].x = 0;
	free[i] = i;
    }

    available_timers = MAX_TIMERS;
    for (char i = 0; i < available_timers; i++) {
	free_timers[i] = i;
    }
}

void manage_mobs(void) {
    for (char i = 0; i < MAX_MOBS; i++) {
	if (sprite[i].x > 0) m_obj[i].fn(m_obj + i);
    }
}

void callback(void (*fn)(u16), u16 timeout, u16 cookie) {
    if (available_timers > 0) {
	char i = free_timers[--available_timers];
	timers[i].timeout = timeout;
	timers[i].cookie = cookie;
	timers[i].fn = fn;
    }
}

void schedule(void (*fn)(u16), u16 ticks) {
    callback(fn, ticks, 0);
}

static void free_timer(char i, char n) {
    char j = free_timers[available_timers];
    free_timers[available_timers++] = i;
    free_timers[n] = j;
}

void manage_timers(void) {
    for (u16 n = available_timers; n < MAX_TIMERS; n++) {
	char i = free_timers[n];
	Timer *timer = timers + i;
	if (timer->timeout > 0) {
	    timer->timeout--;
	}
	else {
	    free_timer(i, n);
	    timer->fn(timer->cookie);
	}
    }
}
