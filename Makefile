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

$(NAME).bin: $(OBJS)
	$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@

%.o: %.c
	$(PREFIX)gcc -Os -c $< -o $@

%.O: %.S
	$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@
