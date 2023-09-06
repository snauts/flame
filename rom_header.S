rom_start:
	dc.l	0x00ffe000
	dc.l	_start
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	cpu_exception
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_hinterrupt
	dc.l	int_null
	dc.l	int_vinterrupt
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null
	dc.l	int_null

	.ascii	"SEGA MEGA DRIVE"
	.org	0x110
	.ascii	"Snauts"
	.org	0x120
	.ascii	"Flammenwerfer"
	.org	0x150
	.ascii	"Flammenwerfer"
	.org	0x180
	.ascii	"GM 00000000-00"
	dc.w	0x0000
	.ascii	"J"
	.org	0x1a0
	dc.l	rom_start
	dc.l	rom_end - 1
	dc.l	0x00ff0000
	dc.l	0x00ff0000 + 0x0000ffff
	.org	0x1F0
	.ascii	"JUE"
	.org	0x200

cpu_exception:
	rte
int_null:
	rte
int_hinterrupt:
	rte
int_vinterrupt:
	movem.l %d0-%d1/%a0-%a1,-(%sp)
	jsr	vblank_interrupt
	movem.l (%sp)+,%d0-%d1/%a0-%a1
	rte

.global wait_for_interrupt
wait_for_interrupt:
	stop	#0x2500
	rts
