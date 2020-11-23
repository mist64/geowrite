; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  shared code for looking for a text scrap on disk
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

;---------------------------------------------------------------
; scanTextScrap
;
; Function:  Check whether there is a compatible text scrap on
;            disk and set textScrapOnDisk accordingly.
;---------------------------------------------------------------
scanTextScrap:
	jsr     setAppDrive
	LoadW   r6, fn_textscrap
        jsr     _FindFile
        bnex    @no
	PushW   r5
        jsr     testFileVersion
        bcc     @ok
        beq     @ok
        tya
        bne     @no
        pla
        pla
        lda     #<txt_textscrap_incompat
        ldy     #>txt_textscrap_incompat
        jsr     showError
        bra     @no

@ok:	PopW    r5
        tya
        bne     @no
        ldy     #OFF_DE_TR_SC
        lda     (r5),y ; track
        sta     r1L
        iny
        lda     (r5),y ; sector
        sta     r1H
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock		; read first block
        beqx    @2
        lda     #<txt_reading_textscrap
        ldy     #>txt_reading_textscrap
        jsr     showIOError
        bra     @no

@2:	MoveW   diskBlkBuf+2, textScrapSize
	LoadB   textScrapOnDisk, $FF
        rts

@no:	LoadW_  textScrapSize, 0
        sta     textScrapOnDisk
        rts


