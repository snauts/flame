NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -T flame.ld
ASFLAGS = -m68000 --register-prefix-optional
CFLAGS	= -fomit-frame-pointer -fno-builtin
OBJS	= rom_header.O main.o canyon.o soldier.o sound.o
HEADS	= +canyon.h +desert.h +soldier.h +walk.h +flame.h
PICS 	= $(subst +,images/,$(HEADS))

all:	$(NAME).bin

clean:
	@echo Clean $(NAME).bin
	@rm -f $(OBJS) $(PICS) $(NAME)*.bin cksum pcx2h \
		z80.rom z80.inc music.inc *.fasl

disasm:	$(NAME).bin
	$(PREFIX)objdump -D -b binary -m 68000 $(NAME).bin | less

run:	$(NAME).bin
	blastem $(NAME).bin

mame:	$(NAME).bin
	mame genesis -cart $(NAME).bin

release: $(NAME).bin
	cp $(NAME).bin $(NAME)-$(shell date +"%F").bin

debug:	$(NAME).bin
	rlwrap blastem -d $(NAME).bin

$(NAME).bin: $(PICS) $(OBJS) cksum
	@echo Link $(NAME).bin
	@$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@
	@./cksum $(NAME).bin

cksum: cksum.c
	@echo Make cksum
	@$gcc cksum.c -o cksum

pcx2h: pcx2h.c
	@echo Make pcx2h
	@$gcc pcx2h.c -o pcx2h

z80.inc: z80.asm
	@echo Compile z80.asm
	@zasm -v0 -l0 z80.asm
	@xxd -i < z80.rom > z80.inc

music.inc: music.lisp
	@echo Prepare music
	@sbcl --noinform --load music.lisp --eval "(save-and-quit)"

%.o: %.c main.h music.inc z80.inc $(PICS)
	@echo Compile $<
	@$(PREFIX)gcc $(CFLAGS) -Os -c $< -o $@

%.O: %.S
	@echo Compile $<
	@$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@

%.h: %.pcx pcx2h
	@echo Convert $<
	@./pcx2h $< $@
