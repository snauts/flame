NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -T flame.ld
ASFLAGS = -m68000 --register-prefix-optional
CFLAGS	= -fomit-frame-pointer
OBJS	= rom_header.O main.o canyon.o soldier.o
HEADS	= +canyon.h +desert.h +soldierT.h +soldierB.h
PICS 	= $(subst +,images/,$(HEADS))

all:	$(NAME).bin

clean:
	@echo Clean $(NAME).bin
	@rm -f $(OBJS) $(PICS) $(NAME).bin cksum pcx2h *.fasl

disasm:
	$(PREFIX)objdump -D -b binary -m 68000 $(NAME).bin

run:	$(NAME).bin
	kega-fusion $(NAME).bin

mame:	$(NAME).bin
	mame genesis -cart flammenwerfer.bin

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

%.o: %.c main.h $(PICS)
	@echo Compile $<
	@$(PREFIX)gcc $(CFLAGS) -Os -c $< -o $@

%.O: %.S
	@echo Compile $<
	@$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@

%.h: %.pcx pcx2h
	@echo Convert $<
	@./pcx2h $< $@

images/soldierT.pcx: images/soldier.pcx
	@convert $< -crop x16+0+0 $@

images/soldierB.pcx: images/soldier.pcx
	@convert $< -crop x32+0+16 $@
