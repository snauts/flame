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
	push	hl
	ld	h, a
	sra	a
	sra	a
	ld	l, a

	ld	a, 0
	ld	b, 0x28
	ld	c, h
	call	ym2612_write

	ld	a, h
	and	a, 3
	add	a, 0xa4
	ld	b, a
	ld	c, e
	ld	a, l
	call	ym2612_write

	ld	a, b
	sub	a, 4
	ld	b, a
	ld	c, d
	ld	a, l
	call	ym2612_write

	ld	a, h
	or	a, 0xf0
	ld	c, a
	ld	a, 0
	ld	b, 0x28
	call	ym2612_write

	pop	hl
	ret

play_note:
	ld	hl, (next)
	ld	a, (hl)
	and	a
	ret	z		; music ends
	inc	hl

	ld	b, a
	ld	c, 0
	ld	a, 1
1$:
	push	af
	and	b
	jp	z, 2$

	push	af
	push	bc

	ld	a, c
	ld	de, (hl)
	inc	hl
	inc 	hl

	call	ym2612_note

	pop	bc
	pop	af
2$:
	inc	c
	pop	af
	sla	a
	jp	nz, 1$

	ld	a, (hl)
	inc	hl

	ld	(wait), a
	ld	(next), hl

	ret

	align	2
