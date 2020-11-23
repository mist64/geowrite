; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  shared code for loading the printer driver
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

loadPrinter:
	jsr     setAppDrive
        ldx     #0
        lda     PrntFilename
        beq     @2
	LoadW   r6, PrntFilename
        LoadB   r0L, 0
        jsr     swapUserZp
        jsr     GetFile
        jsr     swapUserZp
        bit     a9L
        bpl     @rts
        beqx    @rts
        cpx     #FILE_NOT_FOUND
        bne     @1
	LoadW   r5, PrntFilename
        lda     #<dlgbox_cant_find_printer
        ldx     #>dlgbox_cant_find_printer
        jsr     doDlgBox
        ldx     #$FF
@rts:	rts
@1:	lda     #<txt_reading_printer_driver
        ldy     #>txt_reading_printer_driver
        jmp     showIOError
@2:	ldx     #$FF
        bit     a9L
        bpl     @rts
        lda     #<txt_no_printer
        ldy     #>txt_no_printer
        jmp     showError

; ----------------------------------------------------------------------------
dlgbox_cant_find_printer:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   16
	.byte   16
	.word   txt_cant_find_printer
	.byte   DBVARSTR
	.byte   16
.if LANG=LANG_DE
        .byte   48
.else
        .byte   32
.endif
        .byte   12 ; r12
        .byte   OK
        .byte   17
        .byte   72
        .byte   NULL

