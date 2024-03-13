NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -T flame.ld
ASFLAGS = -m68000 --register-prefix-optional
CWARN	= -Wextra -Wall -Wno-unused-parameter
CFLAGS  = $(CWARN) -fomit-frame-pointer -fno-builtin
IFLAGS	= --noinform --no-sysinit --no-userinit --disable-debugger
CSRC	= main.c level.c sound.c mobs.c soldier.c \
	  canyon.c alps.c beach.c town.c
OBJS	= rom_header.O $(subst .c,.o,$(CSRC))
SHA	= $(shell git show --format="%h" --no-patch)
DATE	= $(shell date +"%F")

ifneq ($(LTO),)
CFLAGS	+= -flto
LDLAGS	+= -flto
endif

ifneq ($(OPTIONS),)
CFLAGS	+= $(OPTIONS)
else
CFLAGS	+= -Os -DDEBUG
endif

all:	build

build:
	@$(PREFIX)gcc $(CFLAGS) $(CSRC) -MM -MG > dep.inc
	@make -s $(NAME).bin

clean:
	@echo Clean $(NAME).bin
	@rm -f $(OBJS) $(NAME)* cksum pcx2h images/*.h \
		z80.rom z80.hex *.inc *.fasl

disasm: build
	$(PREFIX)objdump -D -b binary -m 68000 $(NAME).bin | less

run:	build
	blastem $(NAME).bin

mame:	build
	mame -w -r 640x480 genesis -cart $(NAME).bin

release:
	make clean
	LTO=yes OPTIONS="-O3 -DVERSION='\"$(DATE) $(SHA)\"'" make build

debug:	build
	rlwrap blastem -d $(NAME).bin

$(NAME).bin: $(OBJS) cksum
	@echo Link $(NAME).bin
ifneq ($(LTO),)
	@$(PREFIX)gcc $(LDFLAGS) $(OBJS) -o $(NAME).elf
	@$(PREFIX)objcopy $(NAME).elf -S -O binary $@
else
	@$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@
endif
	@./cksum $(NAME).bin

cksum: cksum.c
	@echo Make cksum
	@$gcc cksum.c -o cksum

pcx2h: pcx2h.c
	@echo Make pcx2h
	@$gcc pcx2h.c -o pcx2h

z80.hex: z80.asm
	@echo Compile $<
	@zasm -v0 -l0 $<
	@xxd -i < z80.rom > $@

%.inc: %.lisp
	@echo Prepare $@
	@sbcl $(IFLAGS) --load $< --eval "(save-and-quit)"

%.o: %.c
	@echo Compile $<
	@$(PREFIX)gcc $(CFLAGS) -c $< -o $@

%.O: %.S
	@echo Compile $<
	@$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@

%.h: %.pcx pcx2h
	@echo Convert $<
	@./pcx2h $< $@

-include dep.inc
