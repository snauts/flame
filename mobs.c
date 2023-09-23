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
char mob_head;
Sprite *mob;
byte budget;

char alloc_mob(byte cost) {
    if (budget >= cost) {
	for (char i = 0; i < MAX_MOBS; i++) {
	    if (mob[i].x == 0) {
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
		return i;
	    }
	}
    }
    return -1;
}

void free_mob(char i) {
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
    first_mob_sprite = 0;
    mob = get_sprite(MOB_OFFSET);
    for (u16 i = 0; i < MAX_MOBS; i++) {
	mob[i].x = 0;
    }
}

void manage_mobs(void) {
}
