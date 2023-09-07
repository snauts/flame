#include "main.h"
#include "music.inc"

#define YM2612(part, x) BYTE(YM2612_REG + (part) + (x))
void ym2612_write(byte part, byte reg, byte data) {
    part <<= 1;
    while (YM2612(part, 0) & BIT(7));
    YM2612(part, 0) = reg;
    execute_nops(1);
    YM2612(part, 1) = data;
}

const byte ym2612_reg_init[] = {
    0x22, 0x0,
    0x27, 0x0,
    0x28, 0x0,
    0x28, 0x1,
    0x28, 0x2,
    0x28, 0x4,
    0x28, 0x5,
    0x28, 0x6,
    0x2B, 0x0,
};

static void setup_ym2612_regs(void) {
    for (int i = 0; i < ARRAY_SIZE(ym2612_reg_init); i += 2) {
	const byte *ptr = ym2612_reg_init + i;
	ym2612_write(0, ptr[0], ptr[1]);
    }
}

void init_ym2612(void) {
    do_z80_bus(&setup_ym2612_regs);
}

const byte drums[] = {
    0x2c, 0xc0,
    0x44, 0x44, 0x44, 0x44,
    0x01, 0x01, 0x01, 0x01,
    0xdf, 0xdf, 0xdf, 0xdf,
    0x3c, 0x3c, 0x3c, 0x3c,
    0x14, 0x14, 0x14, 0x14,
    0x1f, 0x1f, 0x1f, 0x1f,
    0x00, 0x00, 0x00, 0x00,
};

const byte guitar[] = {
    0x12, 0xc0,
    0x72, 0x72, 0x72, 0x72,
    0x10, 0x10, 0x10, 0x10,
    0x8f, 0x8f, 0x8f, 0x8f,
    0x06, 0x06, 0x06, 0x06,
    0x02, 0x02, 0x02, 0x02,
    0x16, 0x16, 0x16, 0x16,
    0x00, 0x00, 0x00, 0x00,
};

const byte flute[] = {
    0x14, 0xc0,
    0x1e, 0x2e, 0x37, 0x47,
    0x14, 0x14, 0x14, 0x14,
    0x50, 0x4e, 0x4c, 0x4a,
    0x0c, 0x0c, 0x0c, 0x0c,
    0x01, 0x02, 0x03, 0x04,
    0xf6, 0xf6, 0xf6, 0xf6,
    0x00, 0x00, 0x00, 0x00,
};

const byte bong[] = {
    0x14, 0xc0,
    0x12, 0x22, 0x32, 0x42,
    0x0f, 0x0e, 0x0d, 0x0c,
    0x8f, 0x8f, 0x8f, 0x8f,
    0x18, 0x18, 0x18, 0x18,
    0x0a, 0x0a, 0x0a, 0x0a,
    0x16, 0x16, 0x16, 0x16,
    0x00, 0x00, 0x00, 0x00,
};

static void setup_ym2612_channel(byte channel, const byte *instrument) {
    u16 i = 0;
    byte part = channel >> 2;
    byte offset = channel & 3;
    ym2612_write(part, 0xb0 + offset, instrument[i++]);
    ym2612_write(part, 0xb4 + offset, instrument[i++]);
    for (byte reg = 0x30; reg < 0xa0; reg += 0x4) {
	ym2612_write(part, reg + offset, instrument[i++]);
    }
    ym2612_write(0, 0x28, channel);
}

static void load_score(u16 offset, const byte *ptr, u16 size) {
    memcpy((void *) Z80_RAM + offset, ptr, size);
    byte hi = offset >> 8;
    byte lo = offset & 0xff;
    BYTE(Z80_RAM + 0x15) = hi;
    BYTE(Z80_RAM + 0x14) = lo;
    BYTE(Z80_RAM + 0x13) = hi;
    BYTE(Z80_RAM + 0x12) = lo;
    BYTE(Z80_RAM + 0x11) = 1;
    BYTE(Z80_RAM + 0x10) = 0;
}

static void setup_johnny_intruments(void) {
    setup_ym2612_channel(0, guitar);
    setup_ym2612_channel(1, flute);
    setup_ym2612_channel(2, drums);
    for (byte i = 4; i <= 6; i++) {
	setup_ym2612_channel(i, bong);
    }
    load_score(0x1000, johnny_score, ARRAY_SIZE(johnny_score));
}

void music_johnny(void) {
    do_z80_bus(&setup_johnny_intruments);
}

void psg_noise(byte type, byte vol) {
    BYTE(PSG_ADDR) = 0xf0 | vol;
    BYTE(PSG_ADDR) = 0xe0 | type;
}