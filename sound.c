#include "main.h"
#include "music.inc"

#define MUSIC_BASE 0x1000

enum Music {
    MUSIC_NONE,
    MUSIC_JOHNNY,
    MUSIC_ERIKA,
    MUSIC_BATTOTAI,
    MUSIC_ONIONS,
    MUSIC_DOVES,
};

static enum Music music;

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
    for (byte i = 0; i < ARRAY_SIZE(ym2612_reg_init); i += 2) {
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

const byte tuba[] = {
    0x10, 0xc0,
    0x12, 0x70, 0x70, 0x31,
    0x28, 0x12, 0x32, 0x12,
    0x9f, 0x15, 0x9f, 0x1f,
    0x04, 0x09, 0x04, 0x04,
    0x06, 0x05, 0x05, 0x03,
    0x17, 0x17, 0x17, 0x17,
    0x00, 0x00, 0x00, 0x00,
};

const byte horn[] = {
    0x34, 0xc0,
    0x31, 0x31, 0x31, 0x31,
    0x1f, 0x1f, 0x0a, 0x0a,
    0x10, 0x10, 0x10, 0x10,
    0x01, 0x01, 0x02, 0x02,
    0x00, 0x00, 0x00, 0x00,
    0x18, 0x18, 0x18, 0x18,
    0x00, 0x00, 0x00, 0x00,
};

const byte clarinet[] = {
    0x3b, 0xc0,
    0x31, 0x34, 0x32, 0x31,
    0x27, 0x33, 0x24, 0x0a,
    0x5f, 0x5f, 0x13, 0x53,
    0x00, 0x0f, 0x0b, 0x0f,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x09, 0x4b, 0x09,
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

const byte hi_hat[] = {
    0x3c, 0xc0,
    0x1f, 0x3f, 0x79, 0x3f,
    0x00, 0x04, 0x0a, 0x0b,
    0x1f, 0x1f, 0x13, 0x1f,
    0x1f, 0x0b, 0x11, 0x16,
    0x05, 0x1f, 0x13, 0x1f,
    0x2f, 0xaf, 0x5f, 0x8f,
    0x00, 0x00, 0x00, 0x00,
};

static byte *get_level(byte part, byte reg) {
    static byte total_levels[32];
    return total_levels + (part << 4) + (reg & 0xf);
}

static void setup_ym2612_channel(byte channel, const byte *instrument) {
    u16 i = 0;
    byte part = channel >> 2;
    byte offset = channel & 3;
    ym2612_write(0, 0x28, channel);
    ym2612_write(part, 0xb0 + offset, instrument[i++]);
    ym2612_write(part, 0xb4 + offset, instrument[i++]);
    for (byte reg = 0x30; reg < 0xa0; reg += 0x4) {
	byte value = instrument[i];
	byte where = reg + offset;
	ym2612_write(part, where, value);
	if ((where & 0xf0) == 0x40) {
	    *get_level(part, where) = value;
	}
	i++;
    }
}

static void z80_word_raw(u16 addr, u16 data) {
    BYTE(Z80_RAM + addr + 1) = data >> 8;
    BYTE(Z80_RAM + addr + 0) = data & 0xff;
}

static void load_score(const byte *ptr, u16 size) {
    const u16 offset = MUSIC_BASE;
    z80_copy(offset, ptr, size);
    z80_word_raw(0x14, offset);
    z80_word_raw(0x12, offset);
    BYTE(Z80_RAM + 0x11) = 1;
    BYTE(Z80_RAM + 0x10) = 0;
}

#define LOAD_SCORE(x) \
    load_score((x), ARRAY_SIZE(x));

static void setup_johnny(void) {
    setup_ym2612_channel(0, guitar);
    setup_ym2612_channel(1, flute);
    setup_ym2612_channel(2, hi_hat);
    for (byte i = 4; i <= 6; i++) {
	setup_ym2612_channel(i, bong);
    }
    LOAD_SCORE(johnny_score);
}

static void setup_erika(void) {
    setup_ym2612_channel(0, flute);
    setup_ym2612_channel(1, tuba);
    setup_ym2612_channel(2, drums);
    setup_ym2612_channel(4, hi_hat);
    setup_ym2612_channel(5, bong);
    setup_ym2612_channel(6, horn);
    LOAD_SCORE(erika_score);
}

static void setup_doves(void) {
    setup_ym2612_channel(0, flute);
    setup_ym2612_channel(1, hi_hat);
    setup_ym2612_channel(2, tuba);
    LOAD_SCORE(doves_score);
}

static void setup_battotai(void) {
    setup_ym2612_channel(0, flute);
    setup_ym2612_channel(1, hi_hat);
    setup_ym2612_channel(2, tuba);
    setup_ym2612_channel(4, horn);
    setup_ym2612_channel(5, tuba);
    setup_ym2612_channel(6, bong);
    LOAD_SCORE(battotai_score);
}

static void setup_onions(void) {
    setup_ym2612_channel(0, flute);
    setup_ym2612_channel(1, horn);
    setup_ym2612_channel(2, tuba);
    setup_ym2612_channel(4, bong);
    setup_ym2612_channel(5, bong);
    setup_ym2612_channel(6, hi_hat);
    LOAD_SCORE(onions_score);
}

#define PSG_SFX_CH0	0x18
#define PSG_SFX_CH1	0x1A
#define PSG_SFX_CH2	0x1C

#define SFX_BASE	0xF00

static u16 sfx[SFX_LAST];
static u16 sfx_next;
static u16 channel;

static const byte silent[] = { 0x00, 0x0f };

static void load_sfx(u16 i, const byte *ptr, u16 size) {
    z80_copy(sfx_next, ptr, size);
    sfx[i] = sfx_next;
    sfx_next += size;
}

static void load_z80_sfx(void) {
    sfx_next = SFX_BASE;
    channel = PSG_SFX_CH0;
    load_sfx(SFX_SILENT, silent, sizeof(silent));
    load_sfx(SFX_PERISH, perish, sizeof(perish));
    load_sfx(SFX_WIGGLE, wiggle, sizeof(wiggle));
    load_sfx(SFX_SLASH,  slash,  sizeof(slash));
}

void play_sfx(u16 index) {
    z80_word(channel, sfx[index]);
    channel = channel < PSG_SFX_CH2 ? channel + 2 : PSG_SFX_CH0;
}

static void mute_sound(void) {
    byte ch;
    for (ch = 0; ch <= 6; ch++) {
	if (ch != 3) ym2612_write(0, 0x28, ch);
    }
    for (ch = 0; ch <= 3; ch++) {
	BYTE(PSG_ADDR) = 0x90 | (ch << 5) | 0xf;
    }
}

static void mute_all_psg_channels(void) {
    for (u16 i = 0; i <= 2; i++) play_sfx(SFX_SILENT);
}

void music_toggle(byte state) {
    z80_poke(0x10, state);
    if (state) {
	mute_all_psg_channels();
	do_z80_bus(&mute_sound);
    }
}

static void update_total_levels(void) {
    for (byte part = 0; part < 2; part++) {
	for (byte reg = 0x40; reg < 0x50; reg++) {
	    if ((reg & 3) < 3) {
		byte *ptr = get_level(part, reg);
		if (*ptr < 0x7f) (*ptr)++;
		ym2612_write(part, reg, *ptr);
	    }
	}
    }
}

void fade_music(u16 i) {
    if (i < 0x7f) {
	do_z80_bus(&update_total_levels);
	callback(&fade_music, 1, i + 1);
    }
}

static void setup_music(enum Music id, Function setup) {
    if (music != id) {
	do_z80_bus(setup);
	do_z80_bus(&load_z80_sfx);
	music = id;
    }
}

void music_johnny(void) {
    setup_music(MUSIC_JOHNNY, &setup_johnny);
}

void music_erika(void) {
    setup_music(MUSIC_ERIKA, &setup_erika);
}

void music_doves(void) {
    setup_music(MUSIC_DOVES, &setup_doves);
}

void music_battotai(void) {
    setup_music(MUSIC_BATTOTAI, &setup_battotai);
}

void music_onions(void) {
    setup_music(MUSIC_ONIONS, &setup_onions);
}

void music_none(void) {
    music_toggle(1);
    music = MUSIC_NONE;
}

void psg_noise(byte type, byte vol) {
    BYTE(PSG_ADDR) = 0xf0 | vol;
    BYTE(PSG_ADDR) = 0xe0 | type;
}
