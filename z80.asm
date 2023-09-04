	org	0x0000
start:
	di
	im	1
	ld	sp, 0x2000
	ei

spin:	jr      spin

	org	0x0038
vblank:
	ei
	reti
	align	2
