#include "main.h"

#define HW_VER		0xa10001
#define TMSS_ADDR	0xa14000

void tmss(void) {
    if ((BYTE(HW_VER) & 0xf) != 0) {
	LONG(TMSS_ADDR) = 0x53454741; /* "SEGA" */
    }
}

void _start(void) {
    tmss();
    for (;;) {
    }
}
