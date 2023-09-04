	org	0x0000
start:
	di
	im	1
	ld	sp, 0x2000
	call	init
	ei

spin:	jr      spin

	org	0x0038
vblank:
	call	tick
	ei
	reti
init:
	ret
tick:
	ret
	align	2
