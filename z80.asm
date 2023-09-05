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

ym2612_write:
	;; a - part, b - addr, c - data
	sla	a
	ld	ixl, a
	ld	ixh, 0x40
1$:	ld	a, (ix)
	and	a, 0x80
	jp	nz, 1$
	ld	(ix+0), b
	nop
	ld	(ix+1), c
	ret
	align	2
