	org	0x0000
start:
	di
	im	1
	ld	sp, 0x2000
	ei

	jp      main_loop

	.org	0x0010
stop:
	.byte	1
wait:
	.byte	1
next:
	.word	0
jump:
	.word	0
tick:
	.byte	0
last:
	.byte	0

	org	0x0038
vblank:
	push	af
	ld	a, (tick)
	inc	a
	ld	(tick), a
	pop	af
	ei
	reti

halt:
	halt
	;; falls through to main_loop

main_loop:
	ld	a, (stop)
	cp	a
	jp	nz, halt

	ld	a, (tick)
	ld	b, a
	ld	a, (last)
	cp	b
	jp	z, halt

	ld	a, b
	ld	(last), a

	ld	a, (wait)
	dec	a
	call	z, play_note
	ld	(wait), a

	jp	halt

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
	;; should return time to wait for next note in a
	ld	hl, (next)
	ld	a, (hl)
	and	a
	call 	z, music_end
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

	ld	(next), hl

	ret

music_end:
	ld	hl, (jump)
	ld	a, (hl)
	ret

	align	2		; this sould be at the end
