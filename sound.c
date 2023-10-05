#include "main.h"
#include "music.inc"

static byte music;

#define MUSIC_NONE	0
#define MUSIC_JOHNNY	1

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
    0x3c, 0xc0,
    0x50, 0x10, 0x00, 0x60,
    0x00, 0x00, 0x0a, 0x0a,
    0x5f, 0x5f, 0x5f, 0x5f,
    0x1f, 0x14, 0x0d, 0x13,
    0x0b, 0x14, 0x0a, 0x10,
    0xf8, 0x0a, 0x0c, 0x0c,
    0x00, 0x00, 0x00, 0x00,
};

const byte guitar[] = {
    0x2a, 0xc0,
    0x21, 0x31, 0x39, 0x73,
    0x1c, 0x29, 0x12, 0x0a,
    0x1e, 0x1f, 0x1f, 0x1f,
    0x17, 0x02, 0x1b, 0x0d,
    0x00, 0x03, 0x08, 0x0b,
    0x34, 0x00, 0x31, 0x66,
    0x00, 0x00, 0x00, 0x00,
};

const byte flute[] = {
    0x3b, 0xc0,
    0x06, 0x63, 0x36, 0x33,
    0x28, 0x26, 0x29, 0x0a,
    0xdf, 0xd0, 0x54, 0x8f,
    0x09, 0x0b, 0x07, 0x04,
    0x03, 0x00, 0x00, 0x00,
    0xe5, 0x25, 0xf5, 0x08,
    0x00, 0x00, 0x00, 0x00,
};

const byte bong[] = {
    0x20, 0xc0,
    0x6f, 0x60, 0x64, 0x60,
    0x1c, 0x15, 0x29, 0x0a,
    0xdf, 0x9f, 0xdf, 0x9f,
    0x0f, 0x08, 0x0a, 0x02,
    0x1c, 0x03, 0x0f, 0x02,
    0x25, 0x15, 0x14, 0xf5,
    0x00, 0x00, 0x00, 0x00,
};

static void setup_ym2612_channel(byte channel, const byte *instrument) {
    u16 i = 0;
    byte part = channel >> 2;
    byte offset = channel & 3;
    ym2612_write(0, 0x28, channel);
    ym2612_write(part, 0xb0 + offset, instrument[i++]);
    ym2612_write(part, 0xb4 + offset, instrument[i++]);
    for (byte reg = 0x30; reg < 0xa0; reg += 0x4) {
	ym2612_write(part, reg + offset, instrument[i++]);
    }
}

static void z80_word_raw(u16 addr, u16 data) {
    BYTE(Z80_RAM + addr + 1) = data >> 8;
    BYTE(Z80_RAM + addr + 0) = data & 0xff;
}

static void load_score(u16 offset, const byte *ptr, u16 size) {
    memcpy((void *) Z80_RAM + offset, ptr, size);
    z80_word_raw(0x14, offset);
    z80_word_raw(0x12, offset);
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

#define PSG_SFX_CH0	0x18
#define PSG_SFX_CH1	0x1A
#define PSG_SFX_CH2	0x1C

#define SFX_BASE	0xF00
enum {
    SFX_PERISH = 0,
    SFX_WIGGLE,
    SFX_SLASH,
    SFX_LASTONE,
};

static u16 sfx[SFX_LASTONE];

static void load_sfx(u16 i, const byte *ptr, u16 size) {
    memcpy((void *) Z80_RAM + sfx[i], ptr, size);
    sfx[i + 1] = sfx[i] + size;
}

static void load_z80_sfx(void) {
    sfx[0] = 0xF00;
    load_sfx(SFX_PERISH, perish, sizeof(perish));
    load_sfx(SFX_WIGGLE, wiggle, sizeof(wiggle));
    load_sfx(SFX_SLASH, slash, sizeof(slash));
}

void perish_sfx(void) {
    z80_word(PSG_SFX_CH0, sfx[SFX_PERISH]);
}

void wiggle_sfx(void) {
    z80_word(PSG_SFX_CH1, sfx[SFX_WIGGLE]);
}

void slash_sfx(void) {
    z80_word(PSG_SFX_CH1, sfx[SFX_SLASH]);
}

static void mute_ym2612(void) {
    for (byte ch = 0; ch <= 6; ch++) {
	if (ch != 3) ym2612_write(0, 0x28, ch);
    }
}

void music_toggle(byte state) {
    z80_poke(0x10, state);
    if (state) do_z80_bus(&mute_ym2612);
}

void music_johnny(void) {
    if (music != MUSIC_JOHNNY) {
	do_z80_bus(&setup_johnny_intruments);
	do_z80_bus(&load_z80_sfx);
	music = MUSIC_JOHNNY;
    }
}

void psg_noise(byte type, byte vol) {
    BYTE(PSG_ADDR) = 0xf0 | vol;
    BYTE(PSG_ADDR) = 0xe0 | type;
}
