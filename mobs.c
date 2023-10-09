#include "main.h"

#define MAX_BUDGET	32
#define MOB_OFFSET	16
#define NEXT_GROUP	4

#define MAX_TIMERS	8

typedef struct Mob {
    Object obj;
    byte index;
    char in_pool;
    void (*fn)(Object *);
} Mob;

byte first_mob_sprite;

static char available_mobs;
static Mob mobs[MAX_MOBS];
static char free_mobs[MAX_MOBS];

typedef struct Timer {
    u16 cookie;
    u16 timeout;
    Callback fn;
} Timer;

static char available_timers;
static Timer timers[MAX_TIMERS];
static char free_timers[MAX_TIMERS];

Object *get_mob(u16 index) {
    return (index < MAX_MOBS) ? &mobs[index].obj : NULL;
}

u16 mob_index(Object *obj) {
    Mob *mob = CONTAINER_OF(obj, Mob, obj);
    return mob->index;
}

void mob_fn(Object *obj, void (*fn)(Object *)) {
    Mob *mob = CONTAINER_OF(obj, Mob, obj);
    mob->fn = fn;
}

Object *alloc_mob(void) {
    Object *obj = NULL;
    if (available_mobs > 0) {
	Mob *mob = mobs + free_mobs[--available_mobs];
	mob->in_pool = available_mobs;
	obj = &mob->obj;
    }
    return obj;
}

void free_mob(Object *obj) {
    Mob *mob = CONTAINER_OF(obj, Mob, obj);
    char other = free_mobs[available_mobs];
    free_mobs[available_mobs++] = mob->index;
    free_mobs[mob->in_pool] = other;
    mobs[other].in_pool = mob->in_pool;
}

void purge_mobs(void) {
    for (u16 i = available_mobs; i < MAX_MOBS; i++) {
	free_mob(&mobs[free_mobs[i]].obj);
    }
}

void reset_mobs(void) {
    available_mobs = MAX_MOBS;
    first_mob_sprite = NEXT_GROUP;
    for (char i = 0; i < MAX_MOBS; i++) {
	mobs[i].obj.sprite = get_sprite(MOB_OFFSET) + i;
	mobs[i].index = i;
	free_mobs[i] = i;
    }

    available_timers = MAX_TIMERS;
    for (char i = 0; i < available_timers; i++) {
	free_timers[i] = i;
    }
}

static void call_mob_functions(void) {
    for (char i = available_mobs; i < MAX_MOBS; i++) {
	Mob *mob = mobs + free_mobs[i];
	mob->fn(&mob->obj);
    }
}

void manage_mobs(void) {
    u16 next = NEXT_GROUP;
    call_mob_functions();
    for (char i = available_mobs; i < MAX_MOBS; i++) {
	Mob *mob = mobs + free_mobs[i];
	mob->obj.sprite->next = next;
	next = mob->index + MOB_OFFSET;
    }
    first_mob_sprite = next;
}

void callback(Callback fn, u16 timeout, u16 cookie) {
    if (available_timers > 0) {
	char i = free_timers[--available_timers];
	timers[i].timeout = timeout;
	timers[i].cookie = cookie;
	timers[i].fn = fn;
    }
}

void schedule(Callback fn, u16 ticks) {
    callback(fn, ticks, 0);
}

static void release_timer(char i, char n) {
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
	    release_timer(i, n);
	    timer->fn(timer->cookie);
	}
    }
}
