; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  03 - cut, copy, paste
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------

.segment        "CODE3": absolute

        jmp     cut                 ; 0
        jmp     copy                ; 1
        jmp     pasteText           ; 2
        jmp     addPhotoScrapToDoc  ; 3
        jmp     hidePictures        ; 4
        jmp     splitTooBigPage     ; 5
        jmp     diskNearFullExit    ; 6
        jmp     showTooManyPages    ; 7
        jmp     showCantAddPages    ; 8

; ----------------------------------------------------------------------------
hasSavedBytes:
	.byte   0
isFirstBlock = hasSavedBytes
savedBytes:
	.byte   0,0,0,0
; ----------------------------------------------------------------------------
cut:
	jsr     GotoFirstMenu
        jsr     invertSelectionIfNeeded	; clear selection
        LoadB   a4L, 0
        jsr     cmpCursor0_1Ptr
        beq     @rts			; no selection

        jsr     copyRange
        jsr     deleteSelection
        jsr     copyCursor0To1		; collapse range to point

@rts:	rts

; ----------------------------------------------------------------------------
copy:
	jsr     GotoFirstMenu
        jsr     cmpCursor0_1Ptr
        beq     @rts			; no selection
        jsr     copyRange
@rts:	rts

; ----------------------------------------------------------------------------
pasteText:
	LoadB   a4L, 0
        jsr     GotoFirstMenu
        jsr     invertSelectionIfNeeded	; clear selection
        jsr     cmpCursor0_1Ptr
        beq     :+
        jsr     deleteSelection		; delete selection
:	lda     textScrapSize
        ora     textScrapSize+1
        bne     :+			; no text scrap -> done
        rts
:	jsr     setDirty
        jsr     reinsertCurrentRuler	; put copy of current ruler *after* inserted text (if necessary)
        jsr     moveCursor0Ptr_r15
	MoveW   cursor0+cursor::ptr, a6
        MoveB   cursor0+cursor::srcline, a4H

        jsr     isEndOfCardSet		; put copy of current carset *after* inserted text
        bne     @skip			; (if necessary)
	LoadW	r3, 4
        jsr     makeSpace
	MoveW	r15, r7
        jsr     storeNewCardSetFromCursor0

@skip:	bit     textScrapOnDisk
        bpl     @2
					; *** text scrap from disk
        jsr     insertTextScrapFromDisk
        bra     @3
					; *** text scrap from memory
@2:	MoveW   textScrapSize, r3
        jsr     makeSpace
	MoveW	r15, r1
	LoadW   r0, textScrapData
	MoveW   textScrapSize, r2
        jsr     MoveData
	AddW_   textScrapSize, r15

@3:	MoveW   r15, cursor0+cursor::ptr
        bit     cursor0+cursor::srcline
        bmi     :+
        bvc     @4
:	lda     a4L
        ora     #A4L_20
        sta     a4L
        LoadB   a4H, $FF
@4:	lda     a4L
        ora     #A4L_02
        sta     a4L
        jmp     copyCursor0To1		; deselect

; ----------------------------------------------------------------------------
; c =0: success
addPhotoScrapToDoc:
	jsr     tmpCloseDocFile
        jsr     setAppDrive
	LoadW   r6, fn_photoscrap
        jsr     _FindFile
        beqx    @ok
@none:	lda     #<txt_no_photoscrap	; not found
        ldy     #>txt_no_photoscrap
        jsr     showError
        jsr     reopenDocFile
        sec
        rts

@ok:	PushW   r5
        jsr     testFileVersion
        bcc     @1
        beq     @1
	PopW    r5
        tya
        bne     @none
        lda     #<txt_photoscrap_too_new; incorrect version
        ldy     #>txt_photoscrap_too_new
@error:	jsr     showError
        jsr     reopenDocFile
        sec				; failure
        rts
@1:	PopW    r5
        tya
        bne     @none
        ldy     #OFF_DE_TR_SC
        lda     (r5),y			; track
        pha
        sta     r1L
        iny
        lda     (r5),y			; sector
        pha
        sta     r1H
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock		; read 1st block
        beqx    @2

        jsr     showErrorCopyingPhotoscrap
        jsr     reopenDocFile
        sec
        rts

@2:	MoveB   diskBlkBuf+2, photoWidth
	MoveW   diskBlkBuf+3, photoHeight
	CmpWI   photoHeight, 145
	bcc     @3

        pla
        pla
        lda     #<txt_photoscrap_too_big
        ldy     #>txt_photoscrap_too_big
        bra     @error

@3:	jsr     reopenDocFile
        jsr     findEmptyImageRecord
        bcc     @4

        pla
        pla
        lda     #<txt_picture_area_full
        ldy     #>txt_picture_area_full
        bra     @error

@4:	sta     tmpPicRecord		; [XXX unused!]
        jsr     PointRecord
        PopB    r1H			; sector
        PopB    r1L			; and track of first block
        jsr     copyPhotoScrapData	; copy data block by block
        txa
        pha
        jsr     _UpdateRecordFile
        pla
        bne     showErrorCopyingPhotoscrap
        clc				; success
        rts

showErrorCopyingPhotoscrap:
	lda     #<txt_copying_photscrap
        ldy     #>txt_copying_photscrap
        jsr     showIOError
        sec				; failure
        rts

; ----------------------------------------------------------------------------
findEmptyImageRecord:
	lda     #PAGE_FIRST_IMAGE
@loop:  pha
        jsr     PointRecord
        tya
        beq     @found
        pla
        add     #1
        bne     @loop
        sec				; failure
        rts

@found: pla
        clc				; success
        rts

; ----------------------------------------------------------------------------
hidePictures:
	jsr     GotoFirstMenu
        jsr     clearSelection
        ldx     #txt_show-txt_showhide
        lda     showPicturesFlag
        eor     #$FF
        sta     showPicturesFlag
        bpl     @1

        ldx     #txt_hide-txt_showhide	; update menu text
@1:	ldy     #0
@loop:  lda     txt_showhide,x
        sta     txt_hide_pictures,y
        inx
        iny
        cpy     #txt_show_len		; length of both "show" and "hide"
        bne     @loop

        jsr     L15D0                           ; 3433 20 D0 15                  ..
        jmp     setPromptFontMetricsUpdateRulerUI                           ; 3436 4C 7A 1D                 Lz.

; ----------------------------------------------------------------------------
; this inserts the text scrap file block by block, making sure it always
; inserts a complete legal sequence as opposed to a part of an escape
; sequence, to avoid corruption in case of a read error of the next block.
insertTextScrapFromDisk:
	jsr     tmpCloseDocFile
        jsr     setAppDrive
	LoadW   r6, fn_textscrap
        jsr     _FindFile		; shouldn't fail [XXX but can]
        ldy     #OFF_DE_TR_SC
        lda     (r5),y
        sta     r1L			; start track
        iny
        lda     (r5),y
        sta     r1H			; start sector
	PushW   r1
        jsr     reopenDocFile
        PopW    r1
        LoadB   isFirstBlock, $FF
        LoadB   overhang, 0
@loop1:	jsr     setAppDrive
        jsr     @1
        lda     r1L			; track
        bne     @loop1			; repeat
        rts

@1:	jsr     ldR4DiskBlkBuf		; *** read block from disk
        jsr     _GetBlock		; read block into buffer
        PushW   diskBlkBuf		; link bytes - will be popped as r1 in last instruction


        lda     #0			; *** copy block into textScrapData
        bit     isFirstBlock
        bpl     :+
        lda     #2			; skip 2 bytes of first block
:	sta     cursor3+cursor::mode	; save
        LoadB_x isFirstBlock, 0
        add     #<(diskBlkBuf+2)	; source = data in buffer
        sta     r0L
        lda     #0
        adc     #>(diskBlkBuf+2)
        sta     r0H
        lda     #<textScrapData		; data = textScrapData + overhang
        add     overhang
        sta     r1L
        lda     #>textScrapData
        adc     #0
        sta     r1H
	LoadW   r2, 254			; size: max number of bytes (ok to copy too much)
        jsr     MoveData		; copy block

        ldy     diskBlkBuf		; more blocks?
        bne     @2
					; *** last block
        lda     diskBlkBuf+1		; r3 = block size + overhang [- 2, if first block]
        sub     #1
        sub     cursor3+cursor::mode
        add     overhang
        sta     r3L
        lda     #0
        adc     #0
        sta     r3H
        LoadB   overhang, 0
        bra     @4
					; *** middle block
@2:	lda     #<(textScrapData+254)	; end = textScrapData + 254 + overhang [- 2, first block]
        add     overhang
        sta     cursor3+cursor::ptr
        lda     #>(textScrapData+254)
        add     #0
        sta     cursor3+cursor::ptr+1
        lda     cursor3+cursor::ptr
        sub     cursor3+cursor::mode
        sta     cursor3+cursor::ptr
        bcs     :+
        dec     cursor3+cursor::ptr+1
:	jsr     pushR15

	LoadW	r15, textScrapData
@loop2:	jsr     CmpR15Cursor3Ptr	; skipped end?
        beq     :+
        bcs     @break			; then break
:	MoveW   r15, cursor3+cursor::font	; save checkpoint
        jsr     CmpR15Cursor3Ptr	; hit end?
        beq     @break			; then break, saving ptr
        ldy     #0
        lda     (r15),y
        cmp     #NEWCARDSET		
        bne     @3
	AddVW	5, r15			; skip NEWCARDSET
	bra     @loop2
@3:	jsr     skipEscRulerEscGraphics	; skip all other escapes
        bra     @loop2

@break:	jsr     popR15
	SubVW2_	textScrapData, cursor3+cursor::font, r3	; r3 = length until last atom
        sec
        lda     cursor3+cursor::ptr	; overhang = size of incomplete escape sequence
        sbc     cursor3+cursor::font
        sta     overhang

@4:	PushW   cursor3+cursor::font
        jsr     makeSpace
        PopW	r14			; incomplete escape sequence
	LoadW	r0, textScrapData	; src
	MoveW	r15, r1			; dst
	MoveW	r3, r2			; size, without the incomplete escape sequence
	PushW	r3
        jsr     MoveData		; copy into page
        ldy     overhang
        beq     @skip

@loop:  lda     (r14),y			; copy incomplete escape sequence to start
        sta     textScrapData,y
        dey
        bpl     @loop

@skip:  PopW    r3
	AddW_	r3, r15
        PopW	r1			; next t/s
        rts

; ----------------------------------------------------------------------------
copyRange:
	jsr     doesRangeContainImage
        bcs     @rts			; if true, has already shown error dialog

	MoveW   cursor0+cursor::ptr, r0	; start of range
	PushW   r0			; [XXX unnecessary]
        jsr     removeTrailingRulerFromRange
	PopW    r0
	SubW3	r15, r0, r2		; r2 = size of range
        jsr     prependNewCardSet
	MoveW   r2, textScrapSize
	CmpWI   r2, MAX_TEXTSCRAP_SIZE+1
	bcs     @1			; copy does not fit in memory

	LoadW   r1, textScrapData	; small? make a copy of the whole range
	PushW   r0
        jsr     MoveData
        LoadB   textScrapOnDisk, 0
        bra     @2

@1:	PushW   r0			; big? save to disk
        jsr     saveRangeAsTextScrap

@2:	PopW    r0
        jsr     restoreSavedBytes
@rts:	rts

; ----------------------------------------------------------------------------
doesRangeContainImage:
	jsr     moveCursor0Ptr_r15	; start with cursor0

@loop:  jsr     CmpR15Cursor1Ptr	; up to cursor1
        bcs     @ok
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics
        cmp     #ESC_GRAPHICS		; did we skip an image?
        bne     @loop

        lda     #<txt_cant_copy_pic
        ldy     #>txt_cant_copy_pic
        jsr     showError
        sec
        rts

@ok:	clc
        rts

; --------------------------------------------
saveRangeAsTextScrap:
	lda     r0L
        sub     #2
        sta     r0L
        sta     textscrap_fhdr + O_GHST_ADDR	; used as argument for saveTextscrap
        lda     r0H
        sbc     #0
        sta     r0H
        sta     textscrap_fhdr + O_GHST_ADDR+1
        ldy     #0
        lda     (r0),y
        pha
        lda     textScrapSize		; prepend range in memory with size (word)
        sta     (r0),y
        iny
        lda     (r0),y
        pha
        lda     textScrapSize+1
        sta     (r0),y
        lda     r0H
        pha
        lda     r0L
        pha
        jsr     tmpCloseDocFile
        jsr     saveTextscrap		; save
        jsr     reopenDocFile
	PopW	r0
        ldy     #1			; restore 2 bytes
        pla
        sta     (r0),y
        dey
        pla
        sta     (r0),y
        jmp     testDiskNotFull2	; the disk may now be slightly fuller, check whether ok to continue

; ----------------------------------------------------------------------------
; replace 4 bytes before start of range with NEWCARDSET escape and save original 4 bytes
prependNewCardSet:
	LoadB_y	hasSavedBytes, 0
        lda     (r0),y			; does the range start with
        cmp     #NEWCARDSET		; NEWCARDSET?
        beq     @rts			; yes, perfect!
	SubVW	4, r0
	AddVW   4, r2
	dec     hasSavedBytes
        ldy     #0
        lda     (r0),y
        sta     savedBytes
        lda     #NEWCARDSET
        sta     (r0),y
        iny
        lda     (r0),y
        sta     savedBytes+1
        lda     cursor0+cursor::font
        sta     (r0),y
        iny
        lda     (r0),y
        sta     savedBytes+2
        lda     cursor0+cursor::font+1
        sta     (r0),y
        iny
        lda     (r0),y
        sta     savedBytes+3
        lda     cursor0+cursor::mode
        sta     (r0),y
@rts:	rts

; ----------------------------------------------------------------------------
restoreSavedBytes:
	bit     hasSavedBytes
        bpl     @rts

        ldy     #3
@loop:  lda     savedBytes,y
        sta     (r0),y
        dey
        bpl     @loop

@rts:	rts

; ----------------------------------------------------------------------------
removeTrailingRulerFromRange:
	jsr     LoadR15_MEM_PAGE	; start at beginning of page [XXX why not start of range?]

@loop:  jsr     getByteIntpNewCardSet
        tax
        jsr     CmpR15Cursor1Ptr	; end of range reached?
        bcs     @break			; yes
        jsr     incWR15
        cpx     #ESC_RULER
        bne     @loop

        ldx     #r15			; r15 points at ESC_RULER
        jsr     Ddec
        jsr     addRulerSizeToR15	; r15 points to after ruler data [XXX too much work]
        jsr     CmpR15Cursor1Ptr	; end of range reached?
        bne     @loop			; no
	SubVW	.sizeof(ruler)+1, r15	; r15 points to ESC_RULER again
        rts

@break:	MoveW   cursor1+cursor::ptr, r15	; end of range
        rts

; ----------------------------------------------------------------------------
; copy photo scrap data (at t/s r1) to current record, block by block
copyPhotoScrapData:
	jsr     setDocDrive
        PushW	r1			; t/s
        LoadW_	r2, 0
        jsr     swapUserZp
        jsr     WriteRecord		; write 0 bytes
        jsr     swapUserZp
        PopW	r1			; t/s
        bnex    @rts
        jsr     @1
        bnex    @rts
        lda     r2L
        add     fileSize
        sta     fileSize
        bcc     :+
        inc     fileSize+1
:	lda     curRecord		; fill track/sector into index table
        asl     a
        tay
        lda     r1L
        sta     fileHeader+2,y
        lda     r1H
        sta     fileHeader+3,y
@rts:	rts

@1:	lda     #1
        sta     r3L			; start track
        lda     #0
        sta     r3H			; start sector
        sta     r2L
        jsr     swapUserZp
        jsr     SetNextFree		; allocate
        jsr     swapUserZp
        PushW	r3			; allocated block
        bnex    @end
        inc     r2L			; = 1

@loop:  PushB   r2L
        jsr     setAppDrive
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock		; read first block of photo scrap
        PopB    r2L
	MoveW   diskBlkBuf, r5		; first two bytes
        bnex    @end
	MoveW   r3, r1			; allocated block
        lda     r5L			; track
        bne     @2
        sta     diskBlkBuf		; 0
        lda     r5H
        sta     diskBlkBuf+1		; block len
        bra     @3

@2:	jsr     swapUserZp
        jsr     SetNextFree		; allocate
        jsr     swapUserZp
        bnex    @end
        inc     r2L
	MoveW   r3, diskBlkBuf		; set link t/s

@3:	PushB   r2L
        jsr     setDocDrive
        jsr     ldR4DiskBlkBuf
        jsr     _PutBlock		; write
        PopB    r2L
        MoveB   r5H, r1H
        MoveB   r5L, r1L		; track
        bne     @loop			; more blocks

@end:	PopW    r1
        rts

;---------------------------------------------------------------
; splitTooBigPage
;
; Function:  .
;---------------------------------------------------------------
splitTooBigPage:
	lda     r14L
        cmp     #CR
        bne     @skip
					; store SPACE in byte before
        ldx     #cursor3+cursor::ptr
        jsr     Ddec
        ldy     #0
        lda     #' '
        sta     (cursor3+cursor::ptr),y
	IncW    cursor3+cursor::ptr

@skip:	MoveW   cursor3+cursor::ptr, r0
	AddVW2  1, r0, r1
	SubW3   pageEndPtr2, cursor3+cursor::ptr, r2
	IncW    r2
	jsr     MoveData		; move 1 byte up

        ldy     #0			; insert page break
        lda     #PAGE_BREAK
        sta     (cursor3+cursor::ptr),y
	IncW    pageEndPtr2

	lda     #<txt_page_too_big_inserting_break
        ldy     #>txt_page_too_big_inserting_break
        jsr     showError
        jmp     setDirty

; ----------------------------------------------------------------------------
diskNearFullExit:
	lda     #<txt_disk_near_full
        ldy     #>txt_disk_near_full
        jsr     showError
        jmp     _exitToDesktop

; ----------------------------------------------------------------------------
showTooManyPages:
	lda     #<txt_too_many_pages
        ldy     #>txt_too_many_pages
        jmp     showError

; ----------------------------------------------------------------------------
showCantAddPages:
	lda     #<txt_cant_add_pages
        ldy     #>txt_cant_add_pages
        jmp     showError

; ----------------------------------------------------------------------------

.include "savetextscrap.inc"
.include "fileversion.inc"

; ----------------------------------------------------------------------------
reinsertCurrentRuler:
	jsr     getLastRuler		; get current ruler at cursor1
        jsr     moveCursor1Ptr_r15

@again:	jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics
        tax
        beq     @rts			; end of document
        cmp     #PAGE_BREAK
        beq     @rts			; end of page
        cmp     #CR
        bne     @again			; skip paragraphs

        jsr     pushR15
        jsr     getByteIntpNewCardSet	; read byte again
        tax
        jsr     popR15			; rewind
        cpx     #ESC_RULER
        beq     @rts			; there already is a ruler: done!

        MoveB   cursor0+cursor::srcline, a4H
        jsr     makeSpaceForRuler
        ldy     #.sizeof(ruler)+1-1
@loop:  lda     (r14),y			; copy effective ruler
        sta     (r15),y
        dey
        bpl     @loop
@rts:	rts

; ----------------------------------------------------------------------------

	.include "strings4a.inc"

; ----------------------------------------------------------------------------

fn_photoscrap:
        .byte   "Photo Scrap",0

; ----------------------------------------------------------------------------

	.include "strings4b.inc"

; ----------------------------------------------------------------------------

.include "textscrap_filename.inc"
