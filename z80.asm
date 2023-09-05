	org	0x0000
start:
	di
	im	1
	ld	sp, 0x2000
	ei

spin:	jr      spin

	.org	0x0010
stop:
	.byte	1
wait:
	.byte	1
next:
	.word	0

	org	0x0038
vblank:
	call	tick
	ei
	reti

tick:
	ld	a, (stop)
	and	a
	ret	nz

	ld	a, (wait)
	dec	a
	jp	z, play_note
	ld	(wait), a
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

play_note:
	ret

	align	2
