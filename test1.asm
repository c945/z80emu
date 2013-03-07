	cpu	z80

	org	0

	ld	sp,0100h
	sub	a
	jp	pe,cpu8080
	call	puts
	db	"Z80 CPU\n\r",0
	jp	end
cpu8080
	call	puts
	db	"8080 CPU\n\r",0
	jp	end

end
	ld	a,'E'
	out	(0ffh),a
	IN	a,(0ffh);
	halt

puts	ex	(sp),hl
puts_loop
	ld	a,(hl)
	inc	hl
	and	a
	jr	z,puts_fin
	out	(0ffh),a
	jp	puts_loop
puts_fin
	ex	(sp),hl
	ret

