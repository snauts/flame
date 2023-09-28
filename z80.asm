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
psg:
	.word	silent
	.word	silent
	.word	silent
silent:
	.word	0x0f00

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

	ld	a, 0
	call	psg_play
	ld	a, 1
	call	psg_play
	ld	a, 2
	call	psg_play

	jp	halt

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
	push	ix

	sla	a
	ld	ixl, a
	ld	ixh, 0x40
1$:	ld	a, (ix)
	and	a, 0x80
	jp	nz, 1$
	ld	(ix+0), b
	nop
	ld	(ix+1), c

	pop	ix
	pop	af
	ret

psg_write:
	;; a - channel, bc - frequency/volume
	push	af
	push	de
	push	ix
	ld	ix, 0x7f11

	rrca
	rrca
	rrca
	or	a, 0x80
	ld	d, a
	ld	a, c
	srl	a
	srl	a
	srl	a
	srl	a
	or	a, d
	ld	(ix), a
	ld	(ix), b

	ld	a, c
	and	0x0f
	or	a, d
	or	a, 0x10
	ld	(ix), a

	pop	ix
	pop	de
	pop	af
	ret

psg_play:
	;; a - channel
	push	bc
	push	hl
	push	ix

	push	af

	ld	hl, psg
	sla	a
	add	a, l
	ld	l, a

	ld	bc, (hl)
	ld	ix, bc
	ld	b, (ix + 0)
	ld	c, (ix + 1)

	ld	a, c
	and	a, 0xf
	cp	0xf
	jp	z, sfx_end
	inc	(hl)
	inc	(hl)
sfx_end:
	pop	af

	call	psg_write

	pop	ix
	pop	hl
	pop	bc
	ret

key_off:
	;; c - channel
	push	af
	push	bc

	ld	a, 0
	ld	b, 0x28
	call	ym2612_write

	pop	bc
	pop	af

	ret

key_on:
	;; c - channel
	push	af
	push	bc

	ld	a, c
	or	a, 0xf0
	ld	c, a
	ld	a, 0
	ld	b, 0x28
	call	ym2612_write

	pop	bc
	pop	af

	ret

write_frequency:
	;; c - channel, b - register, e - data
	push	af
	push	bc

	ld	a, c
	and	0x3
	add	b
	ld	b, a

	ld	a, c
	srl	a
	srl	a

	ld	c, d

	call	ym2612_write

	pop	bc
	pop	af

	ret

ym2612_note:
	;; c - channel, e - note
	push	bc
	push	de

	call	key_off

	call	fetch_note

	ld	b, 0xa4
	call	write_frequency

	ld	d, e
	ld	b, 0xa0
	call	write_frequency

	call	key_on

	pop	de
	pop	bc
	ret

frequency_table:
	.word	617
	.word	653
	.word	692
	.word	733
	.word	777
	.word	823
	.word	872
	.word	924
	.word	979
	.word	1037
	.word	1099
	.word	1164

fetch_note:
	;; e - note, de - return frequency
	push	af
	push	bc
	push	hl

	ld	a, e
	and	0xf
	sla	a
	ld	b, 0
	ld	c, a

	ld	hl, frequency_table
	add	hl, bc

	ld	a, e
	and	0x70
	srl	a

	ld	de, (hl)

	or	a, d
	ld	d, a

	pop	hl
	pop	bc
	pop	af
	ret

play_note:
	;; a - return wait time
	push	bc
	push	hl
	push	de

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

	ld	e, (hl)
	inc 	hl

	ld	a, e
	and	0x80
	call	nz, key_off
	call	z, ym2612_note
2$:
	inc	c
	pop	af
	sla	a
	jp	nz, 1$

	ld	a, (hl)
	inc	hl
	ld	(next), hl

	pop	de
	pop	hl
	pop	bc

	ret

music_end:
	ld	hl, (jump)
	ld	a, (hl)
	ret

	align	2		; this sould be at the end
