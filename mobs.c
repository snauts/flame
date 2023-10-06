#include "main.h"

#define MAX_BUDGET	32
#define MOB_OFFSET	16
#define NEXT_GROUP	4

#define MAX_TIMERS	8

typedef struct Mob {
    Object obj;
    byte index;
    byte price;
    char previous;
    void (*fn)(Object *);
} Mob;

byte first_mob_sprite;
static char mob_head;
static byte budget;

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

static void init_mob(Mob *mob) {
    if (mob_head >= 0) {
	mobs[mob_head].previous = mob->index;
    }
    mob->obj.sprite->x = 1;
    mob->obj.sprite->next = first_mob_sprite;
    first_mob_sprite = mob->index + MOB_OFFSET;
    mob_head = mob->index;
    budget -= mob->price;
    mob->previous = -1;
}

Object *alloc_mob(byte cost) {
    Object *obj = NULL;
    if (budget >= cost && available_mobs > 0) {
	Mob *mob = mobs + free_mobs[--available_mobs];
	mob->price = cost;
	obj = &mob->obj;
	init_mob(mob);
    }
    return obj;
}

void free_mob(Object *obj) {
    Mob *mob = CONTAINER_OF(obj, Mob, obj);
    Sprite *sprite = obj->sprite;
    char next = sprite->next - MOB_OFFSET;
    free_mobs[available_mobs++] = mob->index;
    destroy_object(obj);
    budget += mob->price;
    if (mob->previous < 0) {
	first_mob_sprite = sprite->next;
	mob_head = next;
    }
    else {
	mobs[mob->previous].obj.sprite->next = sprite->next;
    }
    if (next >= 0) {
	mobs[next].previous = mob->previous;
    }
}

void purge_mobs(void) {
    for (u16 i = 0; i < MAX_MOBS; i++) {
	Object *obj = &mobs[i].obj;
	if (is_good_object(obj)) free_mob(obj);
    }
}

void reset_mobs(void) {
    mob_head = -1;
    budget = MAX_BUDGET;
    available_mobs = MAX_MOBS;
    first_mob_sprite = NEXT_GROUP;
    for (char i = 0; i < MAX_MOBS; i++) {
	mobs[i].obj.sprite = get_sprite(MOB_OFFSET) + i;
	destroy_object(&mobs[i].obj);
	mobs[i].index = i;
	free_mobs[i] = i;
    }

    available_timers = MAX_TIMERS;
    for (char i = 0; i < available_timers; i++) {
	free_timers[i] = i;
    }
}

void manage_mobs(void) {
    for (char i = 0; i < MAX_MOBS; i++) {
	Mob *mob = mobs + i;
	Object *obj = &mob->obj;
	if (is_good_object(obj)) mob->fn(obj);
    }
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
