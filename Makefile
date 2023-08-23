NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -T flame.ld
ASFLAGS = -m68000 --register-prefix-optional
OBJS	= rom_header.O main.o

all:	$(NAME).bin

clean:
	rm -f *.o *.O $(NAME).bin cksum

disasm:
	$(PREFIX)objdump -D -b binary -m 68000 $(NAME).bin

run:
	kega-fusion $(NAME).bin

$(NAME).bin: $(OBJS) cksum
	@echo Link $(NAME).bin
	@$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@
	@$ ./cksum $(NAME).bin

cksum: cksum.c
	@echo Make cksum
	@$gcc cksum.c -o cksum

%.o: %.c
	@echo Compile $<
	@$(PREFIX)gcc -Os -c $< -o $@

%.O: %.S
	@echo Compile $<
	@$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@
