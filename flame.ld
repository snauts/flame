SECTIONS
{
. = 0x000000;
.text   : { *(.text) }
.rodata : { *(.rodata) }
.data   : { *(.data) }
rom_end = .;
. = 0xff0000;
bss_start = .;
.bss    : { *(.bss) }
bss_end = .;
}
