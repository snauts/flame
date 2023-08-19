NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -T flame.ld
ASFLAGS = -m68000 --register-prefix-optional
OBJS	= rom_header.O main.o

all:	$(NAME).bin

clean:
	rm -f *.o *.O $(NAME).bin

disasm:
	$(PREFIX)objdump -D -b binary -m 68000 $(NAME).bin

run:
	kega-fusion $(NAME).bin

$(NAME).bin: $(OBJS) cksum.c
	@echo Link $(NAME).bin
	@$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@
	gcc cksum.c -o cksum
	./cksum $(NAME).bin

%.o: %.c
	@echo Compile $<
	@$(PREFIX)gcc -Os -c $< -o $@

%.O: %.S
	@echo Compile $<
	@$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@
