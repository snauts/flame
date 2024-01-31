#include "main.h"

#define MAX_BUDGET	32
#define MOB_OFFSET	16

#define MAX_TIMERS	16

typedef struct Mob {
    Object obj;
    void (*fn)(Object *);
} Mob;

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
    return free_mobs[obj->place];
}

void mob_fn(Object *obj, void (*fn)(Object *)) {
    Mob *mob = CONTAINER_OF(obj, Mob, obj);
    mob->fn = fn;
}

Object *alloc_mob(void) {
    Object *obj = NULL;
    if (available_mobs > 0) {
	Mob *mob = mobs + free_mobs[--available_mobs];
	mob->obj.place = available_mobs;
	mob->fn = NULL;
	obj = &mob->obj;
    }
    return obj;
}

void free_mob(Object *obj) {
    char index = free_mobs[obj->place];
    char other = free_mobs[available_mobs];
    free_mobs[available_mobs++] = index;
    free_mobs[obj->place] = other;
    mobs[other].obj.place = obj->place;
    obj->place = -1;
}

void purge_mobs(void) {
    for (u16 i = available_mobs; i < MAX_MOBS; i++) {
	free_mob(&mobs[free_mobs[i]].obj);
    }
}

void reset_mobs(void) {
    available_mobs = MAX_MOBS;
    for (char i = 0; i < MAX_MOBS; i++) {
	mobs[i].obj.sprite = get_sprite(MOB_OFFSET) + i;
	mobs[i].obj.place = -1;
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
	if (mob->fn) mob->fn(&mob->obj);
    }
}

void apply_to_all_mobs(void (*fn)(Object *)) {
    for (char i = available_mobs; i < MAX_MOBS; i++) {
	fn(&mobs[free_mobs[i]].obj);
    }
}

void manage_mobs(void) {
    call_mob_functions();
    for (char i = available_mobs; i < MAX_MOBS; i++) {
	u16 index = free_mobs[i];
	Mob *mob = mobs + index;
	mob->obj.sprite->next = update_next_sprite(index + MOB_OFFSET);
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

static u16 is_small_mob_off_screen(Sprite *sprite) {
    return sprite->x >= MAX_POSITION
	|| sprite->x < ON_SCREEN - 16
	|| sprite->y > ON_SCREEN + SCR_HEIGHT;
}


void small_mob_end(Object *obj, byte persistent) {
    Sprite *sprite = obj->sprite;
    if (is_small_mob_off_screen(sprite)) {
	if (persistent && sprite->x >= MAX_POSITION) {
	    sprite->x = 1;
	    sprite->y = 1;
	}
	else {
	    free_mob(obj);
	}
    }
}

u16 should_small_mob_burn(Sprite *sprite) {
    Rectangle r;
    r.x1 = sprite->x + 4;
    r.y1 = sprite->y + 4;
    r.x2 = sprite->x + 12;
    r.y2 = sprite->y + 12;
    return flame_collision(&r) != NULL;
}

static u16 should_small_mob_bite(Sprite *sprite, char dir) {
    Rectangle r;
    dir = 4 * (dir + 1);
    r.x1 = sprite->x + 2 + dir;
    r.y1 = sprite->y + 4;
    r.x2 = sprite->x + 6 + dir;
    r.y2 = sprite->y + 8;
    return soldier_collision(&r);
}

void small_mob_attack(Object *obj) {
    Sprite *sprite = obj->sprite;
    if (should_small_mob_bite(sprite, obj->direction)) {
	u16 offset = 8 * (obj->direction + 1);
	bite_soldier(sprite->x + offset, sprite->y - 2);
    }
}

void mob_adjust_sprite_dir(Object *obj) {
    if (obj->direction > 0) obj->sprite->cfg |= BIT(11);
}

void update_hitbox(Object *obj, Rectangle *dst, const Rectangle *src, u16 n) {
    for (u16 i = 0; i < n; i++) {
	dst[i].x1 = obj->x + src[i].x1;
	dst[i].y1 = obj->y + src[i].y1;
	dst[i].x2 = obj->x + src[i].x2;
	dst[i].y2 = obj->y + src[i].y2;
    }
}

static u16 burn_count;
static u16 burn_tiles;
static Object **burns;

static void update_burns(u16 i) {
    for (i = 0; i < burn_count; i++) {
	Object *burn = burns[i];
	Object *parent = (Object *) burn->private;
	if (parent != NULL) {
	    burn->sprite->x = parent->sprite->x + burn->x;
	    burn->sprite->y = parent->sprite->y + burn->y;
	}
	if (burn->frame >= 8) {
	    burn->sprite->x = burn->sprite->y = 0;
	}
	else {
	    burn->frame++;
	}
	u16 tile = TILE(2, burn_tiles + 4 * burn->frame);
	if (burn->direction < 0) tile |= BIT(11);
	set_sprite_tile(burn->sprite, tile);
    }
    schedule(&update_burns, 2);
}

void setup_burns(u16 count, u16 tiles) {
    burn_tiles = tiles;
    burn_count = count;
    burns = malloc(count * sizeof(Object*));
    for (u16 i = 0; i < count; i++) {
	burns[i] = alloc_mob();
	burns[i]->sprite->size = SPRITE_SIZE(2, 2);
	burns[i]->private = NULL;
	burns[i]->life = 0;
    }
    schedule(&update_burns, 0);
}

void flame_burn(Object *obj, u16 i) {
    Object *burn = burns[i];
    burn->frame = 0;
    burn->private = NULL;
    burn->direction = obj->direction;
    burn->sprite->x = obj->sprite->x;
    burn->sprite->y = obj->sprite->y - 4;
    set_sprite_tile(burn->sprite, TILE(2, burn_tiles));
}
