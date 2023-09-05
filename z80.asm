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
	push	af
	sla	a
	sla	a
	ld	ixl, a
	ld	ixh, 0x40
1$:	ld	a, (ix)
	and	a, 0x80
	jp	nz, 1$
	ld	(ix+0), b
	nop
	ld	(ix+1), c
	pop	af
	ret

ym2612_note:
	;; a - channel, de - note
	push	af
	ld	c, a
	sra	a
	sra	a
	ld	b, 0x28
	call	ym2612_write

	push	af
	ld	a, c
	and	a, 3
	add	a, 0xa4
	ld	b, a
	pop	af
	ld	c, e
	call	ym2612_write

	push	af
	ld	a, b
	sub	a, 4
	ld	b, a
	pop	af
	ld	c, d
	call	ym2612_write

	pop	af
	and	a, 3
	or	a, 0xf0
	ld	c, a
	ld	b, 0x28
	call	ym2612_write
	ret

play_note:
	ld	hl, (next)
	ld	a, (hl)
	and	a
	ret	z
	inc	hl

	ld	de, (hl)
	inc	hl
	inc 	hl

	call	ym2612_note

	ld	a, (hl)
	inc	hl

	ld	(wait), a
	ld	(next), hl

	ret

	align	2
