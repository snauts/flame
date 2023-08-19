NAME	= flammenwerfer
PREFIX	= m68k-elf-
LDFLAGS = -static -nostdlib -Tbss 0xff0000
ASFLAGS = -m68000 --register-prefix-optional
OBJS	= rom_header.O main.o

all:	$(NAME).bin

clean:
	rm -f *.o *.O $(NAME).bin

$(NAME).bin: $(OBJS)
	$(PREFIX)ld $(LDFLAGS) $(OBJS) --oformat binary -o $@

%.o: %.c
	$(PREFIX)gcc -c $< -o $@

%.O: %.S
	$(PREFIX)as $(ASFLAGS) --bitwise-or $< -o $@
