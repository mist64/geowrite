; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  shared code for converting a string to integer
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

stringToInt:
	ldy     #0
        sty     r2H
        sty     r1L
        sty     r1H
@loop:	lda     fnBuffer,y
        beq     @end
        cmp     #' '
        beq     @skip
        jsr     checkDigit
        bcs     @bad
        sta     r4L
	AslW    r1      ; -> n * 2
        bcs     @bad
	MoveW   r1, r3  ; save
	AslW    r1      ; -> n * 4
        bcs     @bad
	AslW    r1      ; -> n * 8
        bcs     @bad
	AddW_   r3, r1  ; -> n * 8 + n * 2
        bcs     @bad
	AddBW   r4L, r1 ; add new digit
        bcs     @bad
        dec     r2H     ; flag at least one digit
@skip:	iny
        bra     @loop

@end:
	bit     r2H     ; at least one digit?
        bmi     @good
@bad:
	sec
        rts

@good:
	clc
        lda     r1H
        rts

checkDigit:
	sec
        sbc     #'0'
        bcc     @1
        cmp     #10
        rts
@1:     sec
        rts

