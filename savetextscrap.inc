; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  shared code for saving a text scrap
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

saveTextscrap:
	; fill size in file header
	AddW3   textscrap_fhdr+O_GHST_ADDR, textScrapSize, textscrap_fhdr+O_GHEND_ADDR
	AddVW_  2, textscrap_fhdr+O_GHEND_ADDR

	; delete existing
	jsr     setAppDrive
	LoadW   r0, fn_textscrap
        jsr     swapUserZp
        jsr     DeleteFile

	; save new
	LoadW   textscrap_fhdr, fn_textscrap
	LoadW   r9, textscrap_fhdr
        LoadB   r10L, 0
        jsr     SaveFile
        jsr     swapUserZp
        beqx    @1
        lda     #0
        sta     textScrapSize
        sta     textScrapSize+1
        sta     textScrapOnDisk
        rts
@1:	LoadB   textScrapOnDisk, $FF
        rts

; ----------------------------------------------------------------------------
textscrap_fhdr:
	.word   0
	.byte   $03,$15
	.byte   $BF

	.byte %11111111,%11111111,%11111111
	.byte %10000000,%01000000,%00000001
	.byte %10000000,%01100000,%00000001
	.byte %10000000,%01100000,%00000001
	.byte %10000000,%01100000,%00001101
	.byte %10000000,%01010000,%00110101
	.byte %10000000,%01010000,%11011001
	.byte %10000000,%01010011,%00100001
	.byte %10000000,%01011100,%11000001
	.byte %10000000,%01011001,%00000001
	.byte %10000111,%11001110,%00000001
	.byte %10011111,%11001000,%00000001
	.byte %10111000,%11111000,%00000001
	.byte %10110001,%11111100,%00000001
	.byte %10111111,%01101110,%00000001
	.byte %10011100,%01100110,%00000001
	.byte %10000000,%01100110,%00000001
	.byte %10000000,%01101100,%00000001
	.byte %10000000,%00111000,%00000001
	.byte %10000000,%00000000,%00000001
	.byte %11111111,%11111111,%11111111

        .byte   $83
        .byte   SYSTEM
        .byte   SEQUENTIAL
	.word   0 			; start - will be filled
	.word   0 			; end   - will be filled
	.word   0 			; init

        .byte   "Text  Scrap V2.0"

        .res    44, 0
