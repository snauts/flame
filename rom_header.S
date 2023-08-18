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

	.ascii	"SEGA MEGA DRIVE "
	.ascii	"Snauts          "
	.ascii	"Flammenwerfer                                   "
	.ascii	"Flammenwerfer                                   "
	.ascii	"GM 00000000-00"
	dc.w	0x0000
	.ascii	"J               "
	dc.l	rom_start
	dc.l	rom_end-1
	dc.l	0x00ff0000
	dc.l	0x00ff0000 + 0x0000ffff
	dc.l	0x00000000
	dc.l	0x00000000
	dc.l	0x00000000
	dc.l	0x00000000
	dc.l	0x00000000
	dc.l	0x00000000
	.ascii	"                                        "
	.ascii	"JUE             "

SEGA_STRING:	.ascii	"SEGA"

.global _start
_start:
	jsr	tmss
	jsr	c_code

tmss:
	move.b	0xa10001, d0
	andi.b	#0x0f, d0
	beq	1f
	move.l	#SEGA_STRING, a1
	move.l	(a1), 0xa14000
1:	rts

.global wait_vblank_start
wait_vblank_start:
	move.w	0xc00004, d0
	btst.b	#4-1, d0
	beq	wait_vblank_start
	rts

.global wait_vblank_end
wait_vblank_end:
	move.w	0xc00004, d0
	btst.b	#4-1, d0
	bne	wait_vblank_end
	rts

cpu_exception:
	rte
int_null:
	rte
int_hinterrupt:
	rte
int_vinterrupt:
	rte
rom_end:
