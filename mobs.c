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

static void init_mob(char i, byte cost, void *fn) {
    if (mob_head >= 0) {
	m_obj[mob_head].previous = i;
    }
    sprite[i].next = first_mob_sprite;
    first_mob_sprite = i + MOB_OFFSET;
    m_obj[i].previous = -1;
    m_obj[i].price = cost;
    m_obj[i].fn = fn;
    budget -= cost;
    mob_head = i;
    sprite[i].x = 1;
}

Mob *alloc_mob(byte cost, void *fn) {
    Mob *mob = NULL;
    if (budget >= cost && available > 0) {
	char i = free[--available];
	init_mob(i, cost, fn);
	mob = m_obj + i;
    }
    return mob;
}

void free_mob(char i) {
    char next;
    free[available++] = i;
    sprite[i].x = sprite[i].y = 0;
    budget += m_obj[i].price;
    next = sprite[i].next - MOB_OFFSET;
    if (m_obj[i].previous < 0) {
	first_mob_sprite = sprite[i].next;
	mob_head = next;
    }
    else {
	sprite[m_obj[i].previous].next = sprite[i].next;
    }
    if (next >= 0) {
	m_obj[next].previous = m_obj[i].previous;
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
}

void manage_mobs(void) {
    for (char i = 0; i < MAX_MOBS; i++) {
	if (sprite[i].x > 0) m_obj[i].fn(m_obj + i);
    }
}

void manage_timers(void) {
}
