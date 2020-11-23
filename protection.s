; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  copy protection block
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

.include "geosmac.inc"
.include "jumptab.inc"
.include "const.inc"

; ----------------------------------------------------------------------------

.segment "LINK"

        .byte   $00,$FF

.segment "ENTRY"

        jmp     start

.segment "DISK"

;---------------------------------------------------------------
; checkProtection
;
; Function:  Check whether the disk contains the signature bytes
;            in the padding after the sector headers and the
;            sector data on sectors 0-15 of the current track.
;
; Returns:   $00  =1: success
;                 =2: failure
;---------------------------------------------------------------
checkProtection:
        lda     $180F			; data port A
        pha
        and     #%11011111
        sta     $180F			; 1571: switch to 1 MHz
        jsr     checkProtection2
        pla
        sta     $180F			; restore clock speed
        rts

checkProtection2:
	tsx				; save current stack pointer
        stx     $49			; ("savsp"; see symbol "lcc" in 1541 source)
        lda     #%11101110
        sta     $1C0C			; make sure we're in read mode
        lda     #>(buffer-$8000+$0700)
        sta     $33
        lda     #<(buffer-$8000+$0700)
        sta     $32			; set pointer to track/sector for ROM call
        lda     $22			; current track number
        sta     $07F5			; (sector is 0)
        jsr     $F510			; ROM call: find and read block header
        ldy     #2
        sty     $00			; set default error code 2: "READ ERROR"
        jsr     skipBytes		; skip 2 bytes
        ldx     #16
        bne     @1			; check 16 headers and sectors

@loop:  ldy     #<$100			; skip a total of
        jsr     skipBytes		; 325 GCR bytes
        ldy     #$45			; = 260 data bytes
        jsr     skipBytes		; = marker + full block + checksum + filler
        jsr     checkSignature		; check signature after data block
        ldy     #10
        jsr     skipBytes		; skip full header
@1:	jsr     checkSignature		; check signature after header
        dex
        bne     @loop			; repeat
        inx
        stx     $00			; set error code 1: "OK"
        rts

;---------------------------------------------------------------
; skipBytes
;
; Function:  Read and skip bytes coming from disk.
;
; Pass:      y   number of bytes
;---------------------------------------------------------------
skipBytes:
	bvc     skipBytes		; wait for byte
        clv
        lda     $1C01			; read it
        dey
        bne     skipBytes		; y times
        rts


foundSync:
	lda     $1C01			; read value
        clv
        rts

;---------------------------------------------------------------
; checkSignature
;
; Function:  Check whether all byte until the next SYNC mark
;            are either $55 or $67.
;---------------------------------------------------------------
checkSignature:
	ldy     $1C00			; if we found the SYNC mark,
        bpl     foundSync		; the check is ok and we're done

        bvc     checkSignature		; wait until byte ready
        clv

        lda     $1C01			; get byte
        cmp     #$55
        beq     checkSignature		; has to be either signature byte $55
        cmp     #$67
        beq     checkSignature		; or signature byte $67

        pla				; magic not found
        pla				; -> return to main code
        rts				; (error code remains at "2")

; ----------------------------------------------------------------------------

.segment "MAIN"

r0		= 2
r0L		= 2
r0H		= 3
curDrive	= $8489
driveType	= $848e

start:  lda     NewDisk+2		; find the code that is pointed
        sta     r0H			; to by the NewDisk API
        lda     NewDisk+1		; by reading the operand of the
        sta     r0L			; direct/indirect JMP at the
        lda     NewDisk			; entry point
        cmp     #$4C  			; direct or indirect JMP?
        beq     @direct			; direct, then we found the code

        ldy     #0			; indirect jump, so
        lda     (r0),y			; we need to read the vector
        tax
        iny
        lda     (r0),y
        sta     r0H			; and we have a pointer to the code
        stx     r0L

@direct:
	ldx     #STRUCT_MISMAT		; default error code:
        ldy     curDrive
        lda     driveType-8,y		; what kind of drive is this?
        and     #$BF			; ignore the shadow bit (drive cache)
        cmp     #$02			; 2: 1571
        bcc     @is1541			; less, then 1541!
        bne     @rts			; not 1571, then return with error (X != 0)

;---------------------------------------------------------------
; 1571: find
;---------------------------------------------------------------
        tay
@loop1:	dey
@loop2:	dey
@loop3:	lda     (r0),y
        iny
        cmp     #$20			; search for $20, $5C, $C2
        bne     @loop3			; (JSR InitForIO)
        lda     (r0),y
        iny
        cmp     #$5C
        bne     @loop2
        lda     (r0),y
        iny
        cmp     #$C2
        bne     @loop1

@loop4:	lda     (r0),y
        iny
        cmp     #$20
        bne     @loop4

        dey
        bne     @cont

;---------------------------------------------------------------
; 1541
;---------------------------------------------------------------
@is1541:
	ldy     #$FF
@loop5:	iny
@loop6:	lda     (r0),y			; search for $85 $8B
        cmp     #$85			; (STA $8B)
        bne     @loop5			; in NewDisk code
        iny
        lda     (r0),y
        cmp     #$8B
        bne     @loop6

        iny

@cont:	ldx     #$00
@loop7:	lda     (r0),y			; extract 6 bytes
        sta     @code,x			; copy into this code
        iny
        inx
        cpx     #6
        bne     @loop7

        jsr     EnterTurbo
        jsr     InitForIO

        lda     #<checkProtection	; $0705 ptr in 1541 RAM
        sta     $8B			; to execute
        ldx     #>checkProtection	; (this sector is at $0700!)
        stx     $8C
@code:  .byte   0,0,0			; jsr SendExecuteWithTrkSec
	.byte   0,0,0			; jsr GetDOSError
        jsr     DoneWithIO
@rts:	rts

buffer = @code

; ----------------------------------------------------------------------------
        .byte   $D7			; checksum (XOR of bytes at offsets $02-$FE)
