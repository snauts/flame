#include "main.h"

typedef struct Mob {
    Object obj;
    byte price;
    char previous;
} Mob;

#define MAX_BUDGET	16
#define MAX_MOBS	8
#define MOB_OFFSET	12 /* soldier sprites (4) + flame sprites (8) */

byte first_mob_sprite;
Mob m_obj[MAX_MOBS];
char free[MAX_MOBS];
char available;
char mob_head;
Sprite *mob;
byte budget;

static void init_mob(char i, byte cost) {
    if (mob_head >= 0) {
	m_obj[mob_head].previous = i;
    }
    mob[i].next = first_mob_sprite;
    first_mob_sprite = i + MOB_OFFSET;
    m_obj[i].previous = -1;
    m_obj[i].price = cost;
    budget -= cost;
    mob_head = i;
    mob[i].x = 1;
}

static char alloc_mob(byte cost) {
    char i = -1;
    if (budget >= cost && available > 0) {
	i = free[--available];
	init_mob(i, cost);
    }
    return i;
}

static void free_mob(char i) {
    free[available++] = i;
    mob[i].x = mob[i].y = 0;
    budget += m_obj[i].price;
    if (m_obj[i].previous < 0) {
	first_mob_sprite = mob[i].next;
	mob_head = mob[i].next - MOB_OFFSET;
    }
    else {
	mob[m_obj[i].previous].next = mob[i].next;
    }
}

void reset_mobs(void) {
    mob_head = -1;
    budget = MAX_BUDGET;
    available = MAX_MOBS;
    first_mob_sprite = 0;
    mob = get_sprite(MOB_OFFSET);
    for (char i = 0; i < MAX_MOBS; i++) {
	mob[i].x = 0;
	free[i] = i;
    }
}

void manage_mobs(void) {
}
