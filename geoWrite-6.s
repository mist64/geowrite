; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  06 navigation, search/replace, header/footer, repaginate
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------

.segment        "CODE6": absolute

        jmp     previousPage                 ; 0
        jmp     nextPage                     ; 1
        jmp     gotoPage                     ; 2
        jmp     streamBlock                  ; 3
        jmp     readNextPage                 ; 4
        jmp     deleteAcrossPages            ; 5
        jmp     updateDocument               ; 6
        jmp     syncDocToDisk    ; 7
        jmp     update                       ; 8
        jmp     readPage                     ; 9
        jmp     openHeader                   ; 10
        jmp     openFooter                   ; 11
        jmp     setFirstPage                 ; 12
        jmp     titlePage                    ; 13
        jmp     updateTitleNlqMenuSelection  ; 14
        jmp     search                       ; 15
        jmp     findNext                     ; 16
        jmp     change                       ; 17
        jmp     nlqSpacing                   ; 18
        jmp     updatePageSizes              ; 19

; ----------------------------------------------------------------------------
previousPage:
	jsr     pageChangeCommon
        lda     curPage
        bne     @1

        lda     #<txt_no_prev_page
        ldy     #>txt_no_prev_page
        jsr     showError
        bcs     @2			; always

@1:	dec     curPage

@2:	jmp     readPage

; ----------------------------------------------------------------------------
pageChangeCommon:
	jsr     GotoFirstMenu
        lda     curPage
        cmp     #PAGE_HEADER
        bcs     @1			; ignore while editing header/footer

        jsr     clearSelection
        jsr     syncPageToDisk
        jmp     testDiskNotFull2

@1:	pla
        pla
        rts

; ----------------------------------------------------------------------------
nextPage:
	jsr     pageChangeCommon
        lda     curPage
        sta     r15L
@loop:  inc     curPage
        lda     curPage
        cmp     #PAGE_LAST_USABLE+1
        bcs     @error
        jsr     PointRecord
        tya
        bne     @break			; exists
        beq     @loop			; skip empty records

@error:	lda     #<txt_no_next_page
        ldy     #>txt_no_next_page
        jsr     showError
        dec     r15L

@break:	ldy     r15L
        iny
        sty     curPage
        jmp     readPage

; ----------------------------------------------------------------------------
gotoPage:
	jsr     pageChangeCommon
        lda     #<dlgbox_enter_page
        ldx     #>dlgbox_enter_page
        jsr     doDlgBoxInput
        lda     r0L
        cmp     #CANCEL
        beq     @cancl

        jsr     stringToInt
        bcs     @error
        lda     r1L
        sub     privFHData+privfhdata::startPageNo
        tax
        lda     r1H
        sbc     privFHData+privfhdata::startPageNo+1
        bcc     @error
        cpx     #PAGE_LAST_USABLE+1
        bcs     @error
        PushB   curPage
        txa
        sta     curPage
        jsr     PointRecord
        pla
        cpy     #0
        beq     @ok
@cancl:	jmp     readPage

@ok:	sta     curPage
        jsr     PointRecord
@error:	lda     #<txt_cannot_go_to_page
        ldy     #>txt_cannot_go_to_page
        jsr     showError
        jmp     readPage

dlgbox_enter_page:
	.byte   DEF_DB_POS|1
        .byte   DBTXTSTR
        .byte   16
        .byte   16
        .word   txt_enter_page_to_go_to
        .byte   DBGETSTRING
        .byte   16
        .byte   32
        .byte   r5
        .byte   3  ; max chars
        .byte   CANCEL
        .byte   17
        .byte   72
        .byte   NULL

;---------------------------------------------------------------
; streamBlock
;
; Function:  Reads blocks from the page following the current
;            one, one by one.
;
; Note:      Calling this repeatedly will keep adding one block
;            of the document's remainder at a time to the end
;            of the page data in memory, crossing pages and
;            skipping empty records.
;---------------------------------------------------------------
streamBlock:
	jsr     setDocDrive
        LoadB   r14L, 0
        bit     nextPageOpen
        bmi     @1

@loop:  ldx     curPage
        inx				; next page
        txa
        jsr     PointRecord		; does it exist?
        tya
        bne     @found			; yes
        jsr     findNonEmptyRecord	; all pages after are empty?
        bcs     @eof			; yes
        jsr     deleteNextPage		; delete empty page
        bra     @loop			; repeat

@found:	LoadB   nextPageOpen, $FF
        bne     @cont			; always

@eof:	IncW    pageEndPtr2
	ldy     #0
        tya
        sta     (pageEndPtr2),y		; terminating NULL
        rts

@1:	MoveW_y_ tmpTrkSec, r1		; ???

@cont:  sta     r3L			; original value of !nextPageOpen
        lda     pageEndPtr2		; r4 (buffer) = pageEndPtr2 - 1
        sub     #1
        sta     r4L
        lda     pageEndPtr2+1
        sbc     #0
        sta     r4H
        ldy     #0
        lda     (r4),y			; save last two bytes to save
        pha				; them from overwriting by block's
        iny				; link bytes
        lda     (r4),y
        pha
        jsr     _GetBlock		; read one block of record
        beqx    :+
        jmp     showErrorReadingPage
:	LoadB   r2L, $FE
        ldy     #0
        lda     (r4),y			; end of chain?
        bne     @3			; no

        LoadB   nextPageOpen, 0
        iny
        lda     (r4),y			; number of valid bytes
        sub     #1
        sta     r2L
        pha
        PushW	r4
        PushB   r3L			; we read all of it, so
        jsr     deleteNextPage		; delete the page
	PopB	r3L
	PopW	r4
	PopB	r2L			; number of valid bytes

@3:	sta     tmpTrkSec		; read and save link t/s
        ldy     #1
        lda     (r4),y
        sta     tmpTrkSec+1
        pla
        sta     (r4),y			; restore 2 bytes from before
        tax				; that were overwritten
        dey				; by link bytes
        pla
        sta     (r4),y

        bit     r3L
        bpl     @4
        cpx     #CR
        beq     @4
					; first block of record
        ldx     #r4
        stx     r14L
        lda     #2
        jsr     addWIToZp		; r4 += 2
        AddVW2	27, r4, r3		; r3: skipped ruler
        sec
        lda     r2L
        sbc     #26
        sta     r2H
        ldy     #0
@loop2: lda     (r3),y			; copy everything up,
        sta     (r4),y			; overwriting ruler
        iny
        cpy     r2H
        bne     @loop2
        sec
        lda     r2L
        sbc     #27
        sta     r2L

@4:	lda     pageEndPtr2		; add block of bytes to
        add     r2L			; end pointer
        sta     pageEndPtr2
        lda     pageEndPtr2+1
        adc     #0
        sta     pageEndPtr2+1
rts0:	rts

;---------------------------------------------------------------
; deleteNextPage
;
; Function:  Deletes the next page and moves all further pages
;            up.
;---------------------------------------------------------------
deleteNextPage:
	lda     curPage
        add     #1
        jsr     PointRecord
        jsr     _DeleteRecord
        lda     #PAGE_LAST_USABLE
        jsr     PointRecord
        jsr     _InsertRecord
        jsr     _UpdateRecordFile
        LoadB   zp_DDb, 0
        jmp     setDirty

; ----------------------------------------------------------------------------
loopingRepaginateAll:
	LoadB   curPage, 0

@loop:  lda     curPage
        jsr     PointRecord
        tya
        beq     @skip			; empty record, skip to next non-empty
        jsr     repaginateFromPage

@loopingRepaginateNext:
	MoveB	cursor3+cursor::srcline, curPage
@skip:  jsr     findNonEmptyRecord	; next valid page
        bcs     rts0			; end reached
        inc     curPage
        bne     @loop			; loop always

loopingRepaginateNext = @loopingRepaginateNext

; ----------------------------------------------------------------------------
deleteAcrossPages:
	lda     curPage
        cmp     #PAGE_HEADER
        bcs     @rts

        lda     #<dlgbox_del_last_char	; allow user to cancel
        ldx     #>dlgbox_del_last_char	; the delete across pages
        jsr     doDlgBox
        lda     #0
        ldx     sysDBData
        cpx     #CANCEL
        beq     @rts

        jsr     syncPageToDisk
        jsr     testDiskNotFull2
        dec     curPage
        jsr     readPage
	AddVW2  RULER_ESC_LENGTH, pageEndPtr2, cursor0+cursor::ptr
        jsr     copyCursor0To1
        jsr     setDirty
        jsr     streamBlock
        lda     r14L
        beq     @1
	SubVW   27, cursor0+cursor::ptr
@1:	lda     #$FF
        sta     a4H
        sta     cursor0+cursor::srcline
        sta     cursor1+cursor::srcline
	AddVW2  -134, usablePageHeight, pagePosY
        lda     #$FF
@rts:	rts

; ----------------------------------------------------------------------------
dlgbox_del_last_char:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   $10
	.byte   $10
	.word   txt_del_last_char
	.byte   DBTXTSTR
	.byte   $10
        .byte   $20
        .word   txt_of_prev_page
        .byte   OK
        .byte   $01
        .byte   $48
        .byte   CANCEL
        .byte   17
        .byte   $48
        .byte   NULL

; ----------------------------------------------------------------------------
update:	jsr     GotoFirstMenu
	jsr     clearSelection

updateDocument:
	LoadB	a4H, $FF                            ; 34B6 A9 FF                    ..
        jsr     pushR15                         ; 34BA 20 6C 28                  l(
        jsr     syncPageToDisk                           ; 34BD 20 1F 35                  .5
        jsr     testDiskNotFull2                           ; 34C0 20 42 0E                  B.
        jsr     popR15                          ; 34C3 20 7F 28                  .(
@loop:  jsr     pushR15                         ; 34C6 20 6C 28                  l(
        jsr     readPage                           ; 34C9 20 58 37                  X7
        jsr     popR15                          ; 34CC 20 7F 28                  .(
	CmpW    pageEndPtr2, r15
	bcs     @break                           ; 34D9 B0 1F                    ..
	SubW    zp_F2w, r15L                        ; 34DE E5 F2                    ..
        inc     curPage                        ; 34E8 E6 D3                    ..
	SubVW2_	MEM_PAGE, pageEndPtr2, zp_F2w
        bra     @loop                           ; 34F8 50 CC                    P.

@break:	CmpW    pageEndPtr2, cursor0+cursor::ptr
	bcs     L3513                           ; 3504 B0 0D                    ..

copy_XXX1:
	SubW	zp_F2w, cursor0+cursor::ptr
L3513:  jmp     copyCursor0To1                           ; 3513 4C C0 28                 L.(

;---------------------------------------------------------------
; syncDocToDisk
;
; Function:  Update the document on disk to contain all state
;            currently in memory. Can be called editing a page,
;            the header or the footer.
;---------------------------------------------------------------
syncDocToDisk:
	lda     curPage
        cmp     #PAGE_HEADER
        bcc     syncPageToDisk
        jmp     closeHeaderFooter

;---------------------------------------------------------------
; syncPageToDisk
;
; Function:  Update the document on disk to contain all state
;            currently in memory. Can only be called when editing
;            a page, not when editing the header or footer.
;---------------------------------------------------------------
syncPageToDisk:
	lda     hasPagePropMetadata
        beq     repaginateFromLoadedPage

        PushB   dirty
        jsr     repaginateFromLoadedPage
        pla
        bpl     @rts
	PushW   zp_F2w
        PushB   curPage
        jsr     loopingRepaginateNext
        PopB    curPage
        PopW    zp_F2w
@rts:	rts

; ----------------------------------------------------------------------------
repaginateFromPage:
	jsr     readPage
        jsr     setDirty		; first run flag???

repaginateFromLoadedPage:
	PushB   curPage
        bit     dirty
        bpl     @5			; then skip repaginate, just update metadata

	LoadW_  zp_F2w, 0

@loop:  jsr     measurePage		; read bytes across records until page full
        jsr     writePage		; write back page
	CmpW    pageEndPtr2, pageEndPtr1
	php
        jsr     setPageHeader		; create header for new page
        bcc     @1			; more data? yes

        bit     nextPageOpen
        bpl     @2			; no more data from record pending [XXX already taken care of?]

@1:	plp
        inc     curPage			; we're on the next page!
        lda     curPage
        cmp     #PAGE_LAST_USABLE + 1	; overflow?
        bne     :+
        dec     curPage			; yes, go back; error will be caught again
:	jsr     setDocDrive
        jsr     calcusablePageHeight
        lda     #PAGE_LAST_USABLE
        jsr     PointRecord		; is document full?
        tya
        beq     :+			; we can extend by a page
        jsr     _showTooManyPages	; "text at the end will be lost" - we continue anyway
:	jsr     _DeleteRecord
        lda     curPage
        jsr     PointRecord
        jsr     _InsertRecord		; clear last usable page
        bra     @loop

					; *** write remainder
@2:	plp				; last page written has reached end?
        beq     @5			; yes
        inc     curPage
        lda     curPage
        cmp     #PAGE_LAST_USABLE + 1
        beq     @3
        jsr     PointRecord
        tya
        bne     @4			; exists
	MoveW   pageEndPtr2, pageEndPtr1
        jsr     writePage
        bra     @4

@3:	jsr     _showTooManyPages	; "text at the end will be lost" - we continue anyway

@4:	dec     curPage

@5:	MoveB   curPage, cursor3+cursor::srcline
        PopB    curPage
        jsr     cleardirty_nextPageOpen
        LoadB   zp_DDb, $FF
        jsr     setDocDrive
        jsr     _UpdateRecordFile	; update time stamp

UpdateDocFileHeader:
	jsr     setDocDrive
	MoveW   docFileHeaderTrkSec, r1L
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock
        jsr     i_MoveData
		.word   privFHData
		.word   diskBlkBuf + O_GHP_PRIVATE
		.word   9
        MoveW   docFileHeaderTrkSec, r1
        jsr     ldR4DiskBlkBuf
        jmp     _PutBlock

; ----------------------------------------------------------------------------
setPageHeader:
	jsr     setPageHeadRuler
        ldy     #<PAGE_CARDSET
        ldx     #>PAGE_CARDSET
        sty     r1L
        stx     r1H
	AddVW2  1, pageEndPtr1, r0
	CmpW    r0, pageEndPtr2
	bcc     @1
        beq     @1			; pageEndPtr1+1 <= pageEndPtr2

        LoadB   PAGE_CARDSET+cardset::magic, 0
        ldy     #<PAGE_CARDSET
        sty     pageEndPtr2
        stx     pageEndPtr2+1
        jsr     calcDiff_XXX1
        sec				; bad
        rts

@1:	SubW3   pageEndPtr2, pageEndPtr1, r2
	AddVW2  MEM_PAGE+page::ruler+ruler::unused3, r2, pageEndPtr2
        ldy     #0
        lda     (r0),y
        cmp     #ESC_GRAPHICS
        bne     @2

        jsr     setPageHeadCardSet
        jsr     incWR1
        IncW    pageEndPtr2
	lda     #CR
        sta     MEM_PAGE+page::text
        bne     @end

@2:	cmp     #ESC_RULER
        bne     @3

	LoadW   r1, MEM_PAGE
	SubVW   27, pageEndPtr2
        ldy     #page::ruler+ruler::justification
        lda     rulerData1+ruler::justification
        and     #$10
        sta     r4L
        lda     (r0),y
        and     #$EF
        ora     r4L
        sta     (r0),y
        bra     @end

@3:	cmp     #NEWCARDSET
        beq     @end

        cmp     #PAGE_BREAK
        beq     @end

	CmpW    cursor0+cursor::ptr, r0
	bne     :+
        jsr     calcDiff_XXX1
:	jsr     setPageHeadCardSet

@end:	jsr     calcDiff_XXX1
        jsr     MoveData
        clc				; good
        rts

; ----------------------------------------------------------------------------
calcDiff_XXX1:
	lda     zp_F2w
        ora     zp_F2w+1		; zp_F2W == 0?
        bne     @rts			; no

	SubW3	r0, r1, zp_F2w		; zp_F2w = r0 - r1

@rts:	rts

;---------------------------------------------------------------
; setPageHeadCardSet
;
; Function:  Fill the header of the current page in memory
;            with the current cardset.
;---------------------------------------------------------------
setPageHeadCardSet:
	LoadB   PAGE_CARDSET+cardset::magic, NEWCARDSET
        MoveW   curFont, PAGE_CARDSET+cardset::fontid
        MoveB   currentMode, PAGE_CARDSET+cardset::style
        ldx     #r1
        jsr     addWI4ToZp		; r1 += 4
        ldx     #pageEndPtr2
        jmp     addWI4ToZp		; pageEndPtr2 += 4

;---------------------------------------------------------------
; setPageHeadRuler
;
; Function:  Fill the header of the current page in memory
;            with the current ruler data.
;---------------------------------------------------------------
setPageHeadRuler:
	ldy     #.sizeof(ruler)-1
@loop:  lda     rulerData1,y
        sta     MEM_PAGE+page::ruler,y
        dey
        bpl     @loop

        lda     PAGE_RULER+ruler::justification
        and     #$3F			; clear private bits
        sta     PAGE_RULER+ruler::justification

setRulerMagic:
	lda     #ESC_RULER		; set magic
        sta     MEM_PAGE+page::magic
rts4:	rts

;---------------------------------------------------------------
; writePage
;
; Function:  Writes a page into the document. If the record
;            already exists, overwrite it.
;
; Pass:      curPage  page number
;---------------------------------------------------------------
writePage:
	jsr     drawPageNumber
        jsr     setDocDrive
        lda     curPage
        jsr     PointRecord
	SubVW2  MEM_PAGE-1, pageEndPtr1, r2	; length
        jsr     LoadR7MemPage
        jsr     swapUserZp
        jsr     WriteRecord
        jsr     swapUserZp
        cpx     #INSUFF_SPACE
        beq     @error
        beqx    rts4
        lda     #<txt_writing_page
        ldy     #>txt_writing_page
        jsr     showIOError
        bcs     @end

@error:	lda     #<txt_insufficient_disk_space	; this really shouldn't happen, because the app
        ldy     #>txt_insufficient_disk_space	; always checks whether there are at least
        jsr     showError			; 6 KB free before performing anything like this
@end:	jsr     _CloseRecordFile
        jsr     UpdateDocFileHeader
        jmp     restart

; ----------------------------------------------------------------------------
readNextPage:
	jsr     syncPageToDisk                           ; 3747 20 1F 35                  .5
        jsr     testDiskNotFull2                           ; 374A 20 42 0E                  B.
        lda     curPage                        ; 374D A5 D3                    ..
        cmp     #PAGE_LAST_USABLE                            ; 374F C9 3C                    .<
        beq     readPage                           ; 3751 F0 05                    ..
        jsr     copy_XXX1                           ; 3753 20 06 35                  .5
        inc     curPage                        ; 3756 E6 D3                    ..

;---------------------------------------------------------------
; readPage
;
; Function:  Reads a page into memory. If the record does not
;            exist, create an empty page in memory.
;
; Pass:      curPage  page number
;---------------------------------------------------------------
readPage:
	jsr     calcusablePageHeight	; calc for curPage
        lda     loadOpt
        and     #ST_PR_DATA
        bne     :+
        jsr     drawPageNumber		; update UI
:	jsr     setDocDrive
        lda     curPage
        jsr     PointRecord
        bnex    pageReadError
        tya
        beq     createEmptyPage		; if empty record
        jsr     LoadR7MemPage
	LoadW   r2, MEM_SIZE_PAGE	; max num bytes
        jsr     swapUserZp
        jsr     ReadRecord		; read
        jsr     swapUserZp
        bnex    pageReadError
	SubVW2  1, r7, pageEndPtr2	; pointer to last byte
        lda     curPage
        cmp     #PAGE_LAST_USABLE
        bne     cleardirty_nextPageOpen

					; add NULL at the end
        ldy     #0
        lda     (pageEndPtr2),y		; last byte of file
        beq     cleardirty_nextPageOpen
        cmp     #PAGE_BREAK
        beq     :+			; overwrite page break with NULL
	IncW    pageEndPtr2
:	tya
        sta     (pageEndPtr2),y

cleardirty_nextPageOpen:
	lda     #0
        sta     dirty
        sta     nextPageOpen
        rts

pageReadError:
	lda     loadOpt
        and     #ST_PR_DATA
        bne     _exitToDesktop2		; print? then exit to desktop
        cpx     #BFR_OVERFLOW
        bne     showErrorReadingPage

        lda     #<txt_page_too_big	; geoWrite limits pages to 7000 bytes; this
        ldy     #>txt_page_too_big	; can only happen if anther app wrote the document
        jsr     showError		; (maybe old versions of geoWrite had a higher limit?)
        bcs     createEmptyPage		; always

showErrorReadingPage:
	lda     #<txt_reading_page
        ldy     #>txt_reading_page
        jsr     showIOError
        jmp     restart

; ----------------------------------------------------------------------------
_exitToDesktop2:
	jmp     _exitToDesktop

; ----------------------------------------------------------------------------
LoadR7MemPage:
	LoadW   r7, MEM_PAGE
        rts

; ----------------------------------------------------------------------------
createEmptyPage:
	; clear page header
	jsr     i_FillRam
		.word   34
		.word   MEM_PAGE
		.byte   0
        jsr     setRulerMagic
	; set right margin and all 8 tabs to pageWidth1
        ldx     #2 * (8 + 1) - 1
@1:	lda     pageWidth1+1
        sta     PAGE_RULER+ruler::right_margin,x
        dex
        lda     pageWidth1
        sta     PAGE_RULER+ruler::right_margin,x
        dex
        bpl     @1

        LoadB   PAGE_RULER+ruler::justification, $10 ; left aligned, single spaced
	LoadW   pageEndPtr2, PAGE_CARDSET
        jsr     findNonEmptyRecord
        bcs     L381B
        jsr     i_MoveData
		.word   empty_page
		.word   PAGE_CARDSET
		.word   5
        LoadB   pageEndPtr2, <PAGE_CARDSET+4
L381B:  jmp     setDirty

empty_page:
	.byte   NEWCARDSET
	.word   SYSTEM_FONT_ID
	.byte   SET_PLAINTEXT
	.byte   CR

;---------------------------------------------------------------
; findNonEmptyRecord
;
; Function:  Find the first non-empty record after the current
;            page.
;
; Return:    c   =0: found
;                    x   number of non-empty record
;                =1: end of pages reached and none found
;                    or: current page is header or footer
;---------------------------------------------------------------
findNonEmptyRecord:
	ldx     curPage
        cpx     #PAGE_HEADER
        bcs     @fail

@loop:  cpx     #PAGE_LAST_USABLE
        beq     @fail
        inx
        stx     r2L
        txa
        jsr     PointRecord
        ldx     r2L
        tya
        beq     @loop			; loop if empty record

        clc
        rts

@fail:  sec
        rts

; ----------------------------------------------------------------------------
drawPageNumber:
	lda     currentMode
        pha
	LoadW   r1, SYSTEM_FONT_ID
        jsr     setFontFromFile
        jsr     i_GraphicsString
		.byte   NEWPATTERN
		.byte   0
		.byte   MOVEPENTO
		.word   193
		.byte   1
		.byte   RECTANGLETO
		.word   206
		.byte   14
		.byte   MOVEPENTO
		.word   207
		.byte   0
		.byte   LINETO
		.word   207
		.byte   15
		.byte   NEWPATTERN
		.byte   2
		.byte   MOVEPENTO
		.word   208
		.byte   0
		.byte   RECTANGLETO
		.word   211
		.byte   13
		.byte   NULL
        LoadB   currentMode, 0
        lda     curPage
        cmp     #PAGE_HEADER
        bcs     @skip
					; logical page number -> r0
        add     privFHData+privfhdata::startPageNo
        sta     r0L
        lda     #0
        adc     privFHData+privfhdata::startPageNo+1
        sta     r0H
					; calc x pos for centering
        LoadB   r11H, 0 ; x pos hi
        ldx     #193
        lda     r0H
        bne     @1
        lda     r0L
        cmp     #100
        bcs     @1
        ldx     #195
        cmp     #10
        bcs     @1
        ldx     #197
@1:	stx     r11L			; x pos
        LoadB   r1H, 11			; y pos
	LoadW   rightMargin, 212
        lda     #SET_LEFTJUST | SET_SURPRESS | 13	; left justify, no leading zeros [XXX "13" has no function]
        jsr     PutDecimal
	LoadW   rightMargin, SC_PIX_WIDTH-1
@skip:  pla
        sta     currentMode
        rts

;---------------------------------------------------------------
; calcusablePageHeight
;
; Function:  Calculate the height of the page minus
;            header/footer for the current page.
;
; Pass:      curPage  page number
;
; Returns:   usablePageHeight       result
;            usablePageHeightDiv13  result / 13
;---------------------------------------------------------------
calcusablePageHeight:
	MoveW   pageWidth1, pageWidth2
	LoadW   usablePageHeight, MAX_HEADER_FOOTER_HEIGHT
        ldx     curPage
        cpx     #PAGE_HEADER
        bcs     @2

        lda     hasPagePropMetadata
        beq     @1

        lda     widthForPage_lo,x	; get data from metadata table
        sta     pageWidth2
        lda     widthForPage_hi,x
        sta     pageWidth2+1
        lda     heightForPage_lo,x
        sta     usablePageHeight
        lda     heightForPage_hi,x
        sta     usablePageHeight+1
        bpl     @2

					; calculate usable page height (page height - header - footer)
@1:	lda     privFHData+privfhdata::headerHeight
        add     privFHData+privfhdata::footerHeight
        sta     r0L
        lda     privFHData+privfhdata::headerHeight+1
        adc     privFHData+privfhdata::footerHeight+1
        sta     r0H
        lda     privFHData+privfhdata::pageHeight
        sub     r0L
        sta     usablePageHeight
        lda     privFHData+privfhdata::pageHeight+1
        sbc     r0H
        sta     usablePageHeight+1
					; special case title page
        lda     privFHData+privfhdata::titleNlqFlag
        bpl     @2			; no title page
        lda     curPage
        bne     @2			; not first page
	CmpWI   privFHData+privfhdata::startPageNo, 1
	bne     @2			; numbering doesn't start with 1
	MoveW   privFHData+privfhdata::pageHeight, usablePageHeight	; override usable page height with full page height

@2:	MoveW   usablePageHeight, usablePageHeightDiv13
	LoadW   r1, 13
        ldx     #usablePageHeightDiv13
        ldy     #r1
        jmp     Ddiv ; divide by 13

; ----------------------------------------------------------------------------

	.include "stringtoint.inc"

; ----------------------------------------------------------------------------

	.include "strings6.inc"

; ----------------------------------------------------------------------------
setFirstPage:
	LoadB   a4L, 0
        jsr     GotoFirstMenu
        lda     #<dlgbox_enter_page_number_for_first_page
        ldx     #>dlgbox_enter_page_number_for_first_page
        jsr     doDlgBoxInput
        lda     r0L
        cmp     #CANCEL
        beq     rts1
        jsr     stringToInt
        bcs     showBadValueError
	MoveW   r1, privFHData+privfhdata::startPageNo
        jsr     drawPageNumber
        lda     privFHData+privfhdata::titleNlqFlag
        bpl     rts1
        jsr     repaginate_XXX2
        lda     #A4L_20
        sta     a4L
rts1:	rts

showBadValueError:
	lda     #<txt_bad_value
        ldy     #>txt_bad_value
        jmp     showError

; ----------------------------------------------------------------------------
doDlgBoxInput:
	LoadW_y r5, fnBuffer		; default input to empty string
        LoadB_y fnBuffer, 0
        jmp     doDlgBox

; ----------------------------------------------------------------------------
dlgbox_enter_page_number_for_first_page:
	.byte   DEF_DB_POS|1
        .byte   DBTXTSTR
        .byte   16
        .byte   16
        .word   txt_enter_page_number
        .byte   DBTXTSTR
        .byte   16
        .byte   32
        .word   txt_for_first_page
        .byte   DBGETSTRING
        .byte   16
        .byte   48
        .byte   r5
        .byte   3  ; max chars
        .byte   CANCEL
        .byte   17
        .byte   72
        .byte   NULL
; ----------------------------------------------------------------------------
; toggle title page
titlePage:
	jsr     GotoFirstMenu
        lda     curPage
        cmp     #PAGE_HEADER
        bcs     rts1

        lda     privFHData+privfhdata::titleNlqFlag
        eor     #$80
        sta     privFHData+privfhdata::titleNlqFlag
        lda     privFHData+privfhdata::headerHeight
        ora     privFHData+privfhdata::headerHeight+1
        ora     privFHData+privfhdata::footerHeight
        ora     privFHData+privfhdata::footerHeight+1
        beq     updateTitleNlqMenuSelection

	CmpWI   privFHData+privfhdata::startPageNo, 1
	bne     updateTitleNlqMenuSelection

        lda     curPage
        bne     @1

        jsr     setDirty
        jsr     calcusablePageHeight
        bra     updateTitleNlqMenuSelection

@1:	jsr     repaginate_XXX2

updateTitleNlqMenuSelection:
	ldy     #'*'
        bit     privFHData+privfhdata::titleNlqFlag
        bmi     L3B44
        ldy     #' '
L3B44:  sty     txt_title_page
        ldy     #'*'
        bit     privFHData+privfhdata::titleNlqFlag
        bvs     L3B4F
        ldy     #' '
L3B4F:  sty     txt_nlq_spacing
        rts

; ----------------------------------------------------------------------------
nlqSpacing:
	jsr     GotoFirstMenu
        jsr     syncPageToDisk
        lda     privFHData+privfhdata::titleNlqFlag
        eor     #$40
        sta     privFHData+privfhdata::titleNlqFlag
        jsr     updateTitleNlqMenuSelection

updatePageSizes:
	ldy     #<DEFAULT_PAGE_HEIGHT
        ldx     #>DEFAULT_PAGE_HEIGHT
        lda     hasPagePropMetadata
        eor     #$FF
        beq     @2			; yes, take default

        bit     privFHData+privfhdata::titleNlqFlag
        bvs     @1			; NLQ, then compare to default height

        ldy     printerPageHeight	; not NLQ: compare to printer height
        ldx     printerPageHeight+1
@1:	cpx     privFHData+privfhdata::pageHeight+1
        bne     @2
        cpy     privFHData+privfhdata::pageHeight
@2:	php				; save result
        sty     privFHData+privfhdata::pageHeight
        stx     privFHData+privfhdata::pageHeight+1

					; set bit 4 in page 0 ruler reserved byte [PRG 443]
        jsr     setDocDrive
        lda     #0
        jsr     PointRecord		; first record
        tya
        beq     @skip			; empty record

        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock		; read first block
        lda     diskBlkBuf+O_RULER_ESC_RESERVED
        tax
        and     #$10
        bne     @skip			; already set, don't write back
        txa
        ora     #$10
        sta     diskBlkBuf+O_RULER_ESC_RESERVED
        jsr     _PutBlock
        plp
        bra     @3

@skip:  plp
        beq     @rts			; same size as max, or has metadata

@3:	PushB   curPage
        jsr     measureHeaderFooterHeight
        jsr     loopingRepaginateAll
        PopB    curPage
@rts:	rts

; ----------------------------------------------------------------------------
repaginate_XXX2:
	jsr     syncPageToDisk
        PushB   curPage
        LoadB   curPage, 0
        jsr     repaginateFromPage
	PopB    curPage
        jsr     PointRecord		; does current page still exist?
        tya
        bne     @1			; yes
        dec     curPage			; the one before probably does [XXX really?]
@1:     jmp     readPage

; ----------------------------------------------------------------------------
openHeader:
	lda     #PAGE_HEADER
        .byte   $2C
openFooter:
	lda     #PAGE_FOOTER
        pha
        jsr     GotoFirstMenu
        jsr     killPrompt
        pla
        ldx     hasPagePropMetadata
        bne     rts3

        cmp     curPage
        beq     @1			; close

        pha
        LoadB   windowBottom, SC_PIX_HEIGHT-1
        jsr     syncPageToDisk
        pla
        sta     curPage
        jsr     drawHeaderFooterModeUI
        jsr     updateHeaderFooterOpenCloseMenuItem
        LoadB   windowBottom, SC_PIX_HEIGHT-17
        bne     @2			; always

@1:	jsr     closeHeaderFooter

@2:	jmp     readPage

; ----------------------------------------------------------------------------
drawHeaderFooterModeUI:
	lda     #36
        jsr     clearPartOfScreen
        jsr     i_GraphicsString	; draw 3px high horizontal line
		.byte   NEWPATTERN	; at bottom of the screen
		.byte   1
		.byte   MOVEPENTO
		.word   0
		.byte   SC_PIX_HEIGHT-16
		.byte   RECTANGLETO
		.word   SC_PIX_WIDTH-1
		.byte   SC_PIX_HEIGHT-14
		.byte   NULL
        PushB   currentMode
        jsr     useSystemFont
        LoadB	currentMode, SET_BOLD | SET_OUTLINE
        ldy     #<txt_keyword_header
        ldx     #>txt_keyword_header
        lda     curPage
        cmp     #PAGE_HEADER
        beq     L3C31
        ldy     #<txt_keyword_footer
        ldx     #>txt_keyword_footer
L3C31:  sty     r0L
        stx     r0H
        LoadB	r1H, 196		; y
	LoadW	r11, 140		; x
        jsr     PutString
        PopB    currentMode
rts3:	rts

; ----------------------------------------------------------------------------
closeHeaderFooter:
	LoadB   windowBottom, SC_PIX_HEIGHT-1
        jsr     repaginateFromLoadedPage
        lda     privFHData+privfhdata::headerHeight		; add header and footer height BEFORE
        add     privFHData+privfhdata::footerHeight
        pha
        lda     privFHData+privfhdata::headerHeight+1
        adc     privFHData+privfhdata::footerHeight+1
        pha
        jsr     measureHeaderFooterHeight
        pla
        sta     r0H
        pla
        sta     r0L
        lda     privFHData+privfhdata::headerHeight		; add header and footer height AFTER
        add     privFHData+privfhdata::footerHeight
        tax
        lda     privFHData+privfhdata::headerHeight+1
        adc     privFHData+privfhdata::footerHeight+1
        cpx     r0L
        bne     :+
        cmp     r0H
:	beq     @1
        jsr     loopingRepaginateAll	; different header/footer height, then repaginate
@1:	lda     #0
        sta     curPage

updateHeaderFooterOpenCloseMenuItem:
	ldy     #PAGE_HEADER
        jsr     getOpenCloseString
        ldy     #txt_open_len - 1
@loop1:	lda     (r0),y
        sta     txt_open_header,y
        dey
        bpl     @loop1

        ldy     #PAGE_FOOTER
        jsr     getOpenCloseString
        ldy     #txt_open_len - 1
@loop2:	lda     (r0),y
        sta     txt_open_footer,y
        dey
        bpl     @loop2
        rts

; ----------------------------------------------------------------------------
getOpenCloseString:
	lda     #<txt_open
        ldx     #>txt_open
        cpy     curPage
        bne     @1
        lda     #<txt_close
        ldx     #>txt_close
@1:	sta     r0L
        stx     r0H
        rts

; ----------------------------------------------------------------------------
measureHeaderFooterHeight:
	lda     #PAGE_HEADER
        jsr     measurePageHeight
        sta     privFHData+privfhdata::headerHeight
        stx     privFHData+privfhdata::headerHeight+1
        lda     #PAGE_FOOTER
        jsr     measurePageHeight
        sta     privFHData+privfhdata::footerHeight
        stx     privFHData+privfhdata::footerHeight+1
        rts

; ----------------------------------------------------------------------------
measurePageHeight:
	sta     curPage
        jsr     readPage
        jsr     LoadR15_MEM_PAGE
	LoadW   pageTextHeight, 0
        LoadB   rulerData2+ruler::justification, $20
        jsr     cmpPageEndPtr2PageCardSet
        beq     @break
@loop:  jsr     measureLine
        php
        jsr     addLineHeightToPageHeight
        jsr     moveCursor3Ptr_r15
        plp
        bcc     @loop

@break:
	; MIN(pageTextHeight, MAX_HEADER_FOOTER_HEIGHT)
	lda     pageTextHeight
        ldx     pageTextHeight+1
        cpx     #>MAX_HEADER_FOOTER_HEIGHT
        bne     :+
        cmp     #<MAX_HEADER_FOOTER_HEIGHT
:	bcc     :+
        lda     #<MAX_HEADER_FOOTER_HEIGHT
        ldx     #>MAX_HEADER_FOOTER_HEIGHT
:	rts

; ----------------------------------------------------------------------------
search:
	jsr     L3D76                           ; 3CF5 20 76 3D                  v=
        jsr     showDlgBoxSearchReplace                           ; 3CF8 20 D5 3E                  .>
        cmp     #CANCEL                            ; 3CFB C9 02                    ..
        beq     @cancel                           ; 3CFD F0 3F                    .?

        ldy     inputString1                            ; 3CFF AC 00 02                 ...
        beq     @1                           ; 3D02 F0 34                    .4

        ldy     inputString2                        ; 3D04 AC 1A 02                 ...
        beq     findNext2                           ; 3D07 F0 57                    .W

        cmp     #$FF                            ; 3D09 C9 FF                    ..
        bne     findNext2                           ; 3D0B D0 53                    .S
        jsr     L3D80                           ; 3D0D 20 80 3D                  .=
        LoadB   a9H, 0
@loop:  jsr     L3DB2                           ; 3D14 20 B2 3D                  .=
        bcc     @break                           ; 3D17 90 09                    ..
        jsr     L3E6E                           ; 3D19 20 6E 3E                  n>
        LoadB   a9H, $FF
        bne     @loop                           ; 3D20 D0 F2                    ..

@break:	bit     a9H                             ; 3D22 24 7F                    $.
        bmi     @found                           ; 3D24 30 07                    0.

@showErrorNotFound:
	lda     #<txt_string_not_found
        ldy     #>txt_string_not_found
        jsr     showError                       ; 3D2A 20 52 24                  R$

@found:	MoveW   a7, cursor0+cursor::ptr
        jsr     copyCursor0To1                           ; 3D35 20 C0 28                  .(
@1:	LoadB   streamingMode, $FF
        rts                                     ; 3D3D 60                       `

@cancel:
	lda     #0                            ; 3D3E A9 00                    ..
        sta     inputString1                            ; 3D40 8D 00 02                 ...
        sta     inputString2                        ; 3D43 8D 1A 02                 ...
        beq     @1                           ; 3D46 F0 F0                    ..

showErrorNotFound = @showErrorNotFound

; ----------------------------------------------------------------------------
change:
	jsr     cmpCursor0_1Ptr
        php                                     ; 3D4B 08                       .
        jsr     L3D76                           ; 3D4C 20 76 3D                  v=
        plp                                     ; 3D4F 28                       (
        beq     findNext2                           ; 3D50 F0 0E                    ..
        ldy     inputString2                        ; 3D52 AC 1A 02                 ...
        beq     findNext2                           ; 3D55 F0 09                    ..
        jsr     L3E6E                           ; 3D57 20 6E 3E                  n>
        jmp     findNext2                           ; 3D5A 4C 60 3D                 L`=

; ----------------------------------------------------------------------------
findNext:
	jsr     L3D76                           ; 3D5D 20 76 3D                  v=

findNext2:
	lda     inputString1                            ; 3D60 AD 00 02                 ...
        beq     L3D6D                           ; 3D63 F0 08                    ..
        jsr     L3D80                           ; 3D65 20 80 3D                  .=
        jsr     L3DB2                           ; 3D68 20 B2 3D                  .=
        bcc     L3D73                           ; 3D6B 90 06                    ..
L3D6D:  LoadB   streamingMode, $FF
        rts                                     ; 3D72 60                       `

L3D73:  jmp     showErrorNotFound                           ; 3D73 4C 26 3D                 L&=

; ----------------------------------------------------------------------------
L3D76:  jsr     GotoFirstMenu                   ; 3D76 20 BD C1                  ..
        LoadB   a4L, 0
        jmp     clearSelection                           ; 3D7D 4C 5C 08                 L\.

; ----------------------------------------------------------------------------
L3D80:  lda     dirty                        ; 3D80 A5 E1                    ..
        beq     L3D8A                           ; 3D82 F0 06                    ..
        jsr     syncPageToDisk                           ; 3D84 20 1F 35                  .5
        jsr     readPage                           ; 3D87 20 58 37                  X7
L3D8A:  LoadB_y streamingMode, 0
        dey                                     ; 3D8F 88                       .
        sty     a4H                             ; 3D90 84 75                    .u
        lda     curPage                        ; 3D92 A5 D3                    ..
        sta     L41E2                           ; 3D94 8D E2 41                 ..A
        MoveW   cursor0+cursor::ptr, a7
        jsr     moveCursor0Ptr_r15                           ; 3D9F 20 A1 27                  .'
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3DA2 20 9E 14                  ..
        MoveW   r15L, cursor0+cursor::ptr
        lda     #A4L_80                            ; 3DAD A9 80                    ..
        sta     a4L                             ; 3DAF 85 74                    .t
L3DB1:  rts                                     ; 3DB1 60                       `

; ----------------------------------------------------------------------------
L3DB2:  jsr     L3DF5                           ; 3DB2 20 F5 3D                  .=
        bcs     L3DB1                           ; 3DB5 B0 FA                    ..
        bmi     L3DB1                           ; 3DB7 30 F8                    0.
        bit     searchOptions                        ; 3DB9 24 F1                    $.
        bvc     L3DEF                           ; 3DBB 50 32                    P2
        ldy     curPage                        ; 3DBD A4 D3                    ..
        cpy     #PAGE_HEADER                            ; 3DBF C0 3D                    .=
        bcs     L3DEF                           ; 3DC1 B0 2C                    .,
        iny                                     ; 3DC3 C8                       .
        cpy     #PAGE_HEADER                            ; 3DC4 C0 3D                    .=
        beq     L3DD1                           ; 3DC6 F0 09                    ..
        sty     r15L                            ; 3DC8 84 20                    . 
        tya                                     ; 3DCA 98                       .
        jsr     PointRecord                     ; 3DCB 20 80 C2                  ..
        tya                                     ; 3DCE 98                       .
        bne     L3DD5                           ; 3DCF D0 04                    ..
L3DD1:  lda     #$00                            ; 3DD1 A9 00                    ..
        sta     r15L                            ; 3DD3 85 20                    . 
L3DD5:  lda     r15L                            ; 3DD5 A5 20                    . 
        cmp     curPage                        ; 3DD7 C5 D3                    ..
        beq     L3DEF                           ; 3DD9 F0 14                    ..
        pha                                     ; 3DDB 48                       H
        LoadB   streamingMode, $FF
        jsr     syncPageToDisk                           ; 3DE1 20 1F 35                  .5
        LoadB   streamingMode, 0
        pla                                     ; 3DE9 68                       h
        sta     curPage                        ; 3DEA 85 D3                    ..
        jsr     readPage                           ; 3DEC 20 58 37                  X7
L3DEF:  jsr     loadCursor0PtrPageCardset                           ; 3DEF 20 76 27                  v'
        bra     L3DB2                           ; 3DF3 50 BD                    P.

L3DF5:  lda     #$FF                            ; 3DF5 A9 FF                    ..
        sta     r11L                            ; 3DF7 85 18                    ..
        jsr     moveCursor0Ptr_r15                           ; 3DF9 20 A1 27                  .'
L3DFC:  lda     r15H                            ; 3DFC A5 21                    .!
        cmp     a7H                             ; 3DFE C5 7B                    .{
        bne     L3E06                           ; 3E00 D0 04                    ..
        lda     r15L                            ; 3E02 A5 20                    . 
        cmp     a7L                             ; 3E04 C5 7A                    .z
L3E06:  bne     L3E0F                           ; 3E06 D0 07                    ..
        lda     curPage                        ; 3E08 A5 D3                    ..
        cmp     L41E2                           ; 3E0A CD E2 41                 ..A
        beq     L3E6A                           ; 3E0D F0 5B                    .[
L3E0F:  jsr     cmpPageEndPtr2R15                           ; 3E0F 20 14 28                  .(
        bcc     L3E66                           ; 3E12 90 52                    .R
        beq     L3E66                           ; 3E14 F0 50                    .P
        MoveW   r15L, cursor0+cursor::ptr                           ; 3E1C 85 80                    ..
        jsr     getByteIntpNewCardSet                           ; 3E1E 20 5A 14                  Z.
        tay                                     ; 3E21 A8                       .
        lda     r11L                            ; 3E22 A5 18                    ..
        sty     r11L                            ; 3E24 84 18                    ..
        jsr     isAlphanumeric                           ; 3E26 20 7F 1E                  ..
        bcc     L3E2F                           ; 3E29 90 04                    ..
        bit     searchOptions                        ; 3E2B 24 F1                    $.
        bmi     L3E3D                           ; 3E2D 30 0E                    0.
L3E2F:  ldy     #0                            ; 3E2F A0 00                    ..
L3E31:  lda     inputString1,y                          ; 3E31 B9 00 02                 ...
        beq     L3E43                           ; 3E34 F0 0D                    ..
        cmp     (r15),y                        ; 3E36 D1 20                    . 
        bne     L3E3D                           ; 3E38 D0 03                    ..
        iny                                     ; 3E3A C8                       .
        bne     L3E31                           ; 3E3B D0 F4                    ..
L3E3D:  jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3E3D 20 9E 14                  ..
        bra     L3DFC                           ; 3E41 50 B9                    P.

L3E43:  lda     (r15),y                        ; 3E43 B1 20                    .
        cmp     #$17                            ; 3E45 C9 17                    ..
        bne     L3E4F                           ; 3E47 D0 06                    ..
        iny                                     ; 3E49 C8                       .
        iny                                     ; 3E4A C8                       .
        iny                                     ; 3E4B C8                       .
        iny                                     ; 3E4C C8                       .
        lda     (r15),y                        ; 3E4D B1 20                    . 
L3E4F:  jsr     isAlphanumeric                           ; 3E4F 20 7F 1E                  ..
        bcc     L3E58                           ; 3E52 90 04                    ..
        bit     searchOptions                        ; 3E54 24 F1                    $.
        bmi     L3E3D                           ; 3E56 30 E5                    0.
L3E58:  tya                                     ; 3E58 98                       .
        add     r15L                            ; 3E5A 65 20                    e
        sta     cursor1+cursor::ptr                        ; 3E5C 85 8A                    ..
        lda     #$00                            ; 3E5E A9 00                    ..
        adc     r15H                            ; 3E60 65 21                    e!
        sta     cursor1+cursor::ptr+1                        ; 3E62 85 8B                    ..
        sec                                     ; 3E64 38                       8
        rts                                     ; 3E65 60                       `

L3E66:  clc                                     ; 3E66 18                       .
        lda     #$00                            ; 3E67 A9 00                    ..
        rts                                     ; 3E69 60                       `

L3E6A:  clc                                     ; 3E6A 18                       .
        lda     #$FF                            ; 3E6B A9 FF                    ..
        rts                                     ; 3E6D 60                       `

; ----------------------------------------------------------------------------
L3E6E:  jsr     setDirty                           ; 3E6E 20 36 1E                  6.
        jsr     moveCursor0Ptr_r15                           ; 3E71 20 A1 27                  .'
        jsr     getByteIntpNewCardSet                           ; 3E74 20 5A 14                  Z.
        jsr     L3EBF                           ; 3E77 20 BF 3E                  .>
        jsr     L3E99                           ; 3E7A 20 99 3E                  .>
        jsr     L4189                           ; 3E7D 20 89 41                  .A
        ldy     #0                            ; 3E80 A0 00                    ..
L3E82:  lda     inputString2,y                      ; 3E82 B9 1A 02                 ...
        beq     L3E8C                           ; 3E85 F0 05                    ..
        sta     (r15),y                        ; 3E87 91 20                    . 
        iny                                     ; 3E89 C8                       .
        bne     L3E82                           ; 3E8A D0 F6                    ..
L3E8C:  tya                                     ; 3E8C 98                       .
        add     r15L                            ; 3E8E 65 20                    e
        sta     cursor0+cursor::ptr                           ; 3E90 85 80                    ..
        lda     #$00                            ; 3E92 A9 00                    ..
        adc     r15H                            ; 3E94 65 21                    e!
        sta     cursor0+cursor::ptr+1                        ; 3E96 85 81                    ..
        rts                                     ; 3E98 60                       `

; ----------------------------------------------------------------------------
L3E99:  tay                                     ; 3E99 A8                       .
        lda     curPage                        ; 3E9A A5 D3                    ..
        cmp     L41E2                           ; 3E9C CD E2 41                 ..A
        bne     L3EBD                           ; 3E9F D0 1C                    ..
        lda     a7H                             ; 3EA1 A5 7B                    .{
        cmp     r15H                            ; 3EA3 C5 21                    .!
        bne     L3EAB                           ; 3EA5 D0 04                    ..
        lda     a7L                             ; 3EA7 A5 7A                    .z
        cmp     r15L                            ; 3EA9 C5 20                    . 
L3EAB:  bcc     L3EBD                           ; 3EAB 90 10                    ..
        ldx     #$00                            ; 3EAD A2 00                    ..
        tya                                     ; 3EAF 98                       .
        bpl     L3EB3                           ; 3EB0 10 01                    ..
        dex                                     ; 3EB2 CA                       .
L3EB3:  add     a7L                             ; 3EB4 65 7A                    ez
        sta     a7L                             ; 3EB6 85 7A                    .z
        txa                                     ; 3EB8 8A                       .
        adc     a7H                             ; 3EB9 65 7B                    e{
        sta     a7H                             ; 3EBB 85 7B                    .{
L3EBD:  tya                                     ; 3EBD 98                       .
        rts                                     ; 3EBE 60                       `

; ----------------------------------------------------------------------------
L3EBF:  ldy     #$FF
@loop1: iny
        lda     inputString2,y		; strlen
        bne     @loop1

        ldx     #0
@loop2: lda     inputString1,x
        beq     @end
        inx
        dey
        bra     @loop2

@end:	tya
        rts

; ----------------------------------------------------------------------------
showDlgBoxSearchReplace:
	lda     #<dlgbox_search_replace
        ldx     #>dlgbox_search_replace
        jsr     doDlgBox                        ; 3ED9 20 56 26                  V&
        lda     r0L                             ; 3EDC A5 02                    ..
        rts                                     ; 3EDE 60                       `

; ----------------------------------------------------------------------------
setAppMainFunc1:
        lda     #<appMainFunc1                            ; 3EDF A9 F9                    ..
        ldx     #>appMainFunc1                            ; 3EE1 A2 3E                    .>
setAppMain:
	sta     appMain                         ; 3EE3 8D 9B 84                 ...
        stx     appMain+1                           ; 3EE6 8E 9C 84                 ...
rts5:	rts                                     ; 3EE9 60                       `

; ----------------------------------------------------------------------------
otherKeyVector:
	bit     zp_BAb                       ; 3EEA 24 BA                    $.
        bmi     rts5                           ; 3EEC 30 FB                    0.

callbackUserIcon7:
        jsr     L3FDA                           ; 3EEE 20 DA 3F                  .?
        lda     #1                            ; 3EF1 A9 01                    ..

staSysDBDataRstrFrmDialogue:
	sta     sysDBData                       ; 3EF3 8D 1D 85                 ...
        jmp     RstrFrmDialogue                 ; 3EF6 4C BF C2                 L..

; ----------------------------------------------------------------------------
appMainFunc1:
        lda     #<appMainFunc2                            ; 3EF9 A9 EA                    ..
        ldx     #>appMainFunc2                            ; 3EFB A2 3F                    .?
        jsr     setAppMain                           ; 3EFD 20 E3 3E                  .>
        lda     #0                            ; 3F00 A9 00                    ..
        sta     iconSelFlg		; disable icon flashing
        sta     currentMode                     ; 3F05 85 2E                    ..
        sta     a9H                             ; 3F07 85 7F                    ..
        sta     a9L                             ; 3F09 85 7E                    .~
        LoadW   rightMargin, 246
        jsr     callbackTextbox2                           ; 3F13 20 BF 3F                  .?
        jsr     callbackTextbox1                           ; 3F16 20 91 3F                  .?
        bit     searchOptions                        ; 3F19 24 F1                    $.
        bpl     L3F23                           ; 3F1B 10 06                    ..
        jsr     callbackWholeWord                           ; 3F1D 20 5F 3F                  _?
        bra     L3F26                           ; 3F21 50 03                    P.

L3F23:  jsr     callbackPartialWord                           ; 3F23 20 85 3F                  .?
L3F26:  bit     searchOptions                        ; 3F26 24 F1                    $.
        bvc     callbackThisPageOnly                           ; 3F28 50 03                    P.
        jmp     callbackAllPages                           ; 3F2A 4C 39 3F                 L9?

; ----------------------------------------------------------------------------
callbackThisPageOnly:
	lda     #$FF-$40
        and     searchOptions
        sta     searchOptions
        ldy     #0			; white
        lda     #1			; black
        bne     L3F43

; ----------------------------------------------------------------------------
callbackAllPages:
	lda     #$40
        ora     searchOptions
        sta     searchOptions
        ldy     #1			; black
        lda     #0			; white
L3F43:  pha
        tya
        jsr     SetPattern
        jsr     i_Rectangle
		.byte   92,98		; top, bottom
		.word   73,78		; left, right
        pla
        jsr     SetPattern
        jsr     i_Rectangle
		.byte   92,98		; top, bottom
		.word   153,158		; left, right
        rts

; ----------------------------------------------------------------------------
callbackWholeWord:
	lda     #$80			; set flag
        ora     searchOptions
        sta     searchOptions
        ldy     #1
        lda     #0
updateCheckboxes:
	pha
        tya
        jsr     SetPattern
        jsr     i_Rectangle		; clear/fill first checkbox
		.byte   82,88
		.word   73,78
        pla
        jsr     SetPattern
        jsr     i_Rectangle		; clear/fill second checkbox
		.byte   82,88
		.word   153,158
        rts

callbackPartialWord:
	lda     #$FF-$80		; clear flag
        and     searchOptions
        sta     searchOptions
        ldy     #0
        lda     #1
        bne     updateCheckboxes

; ----------------------------------------------------------------------------
callbackTextbox1:
	jsr     L3FDA
        lda     #<inputString1
        ldx     #>inputString1
        ldy     #51			; y pos
L3F9A:  sta     r0L
        stx     r0H
        sty     r1H			; y pos of input
        LoadB   r1L, 0			; flags
        sta     zp_BAb
	LoadB   r2L, 25			; max chars
	LoadW   r11, 147		; x pos of input
	LoadW   keyVector, otherKeyVector
        jmp     GetString

callbackTextbox2:
	jsr     L3FDA
        LoadB   a9H, $FF
        lda     #<inputString2
        ldx     #>inputString2
        ldy     #67			; y pos
        bne     L3F9A

; ----------------------------------------------------------------------------
callKeyVector:
	lda     keyVector
        ora     keyVector+1
        beq     :+
        jmp     (keyVector)
:	rts

; ----------------------------------------------------------------------------
L3FDA:  LoadB   a9H, 0
        sei
        dec     zp_BAb
        LoadB   keyData, CR		; simulate CR key press
        jsr     callKeyVector		; [see HHG p.118]
        cli

appMainFunc2:
	jsr     L4032
        cmp     a9L
        beq     rts2
        sta     a9L
        tax
        bmi     @1
        jsr     i_GraphicsString	; clear "All" button
		.byte   NEWPATTERN
		.byte   0
		.byte   MOVEPENTO
		.word   136
		.byte   104
		.byte   RECTANGLETO
		.word   183
		.byte   119
		.byte   NULL
        rts

@1:	jsr     i_BitmapUp		; draw "All" button
		.word   graph_all
		.byte	17		; x
		.byte	104		; y
		.byte	6		; width
		.byte	16		; height
rts2:	rts

; ----------------------------------------------------------------------------
L400F:
        jsr     L4032
        bpl     rts2
        ldx     #L402C_len
L4016:  lda     L402C-1,x
        sta     r2L-1,x
        dex
        bne     L4016
        jsr     IsMseInRegion
        tax
        beq     rts2			; no
        jsr     L3FDA
        lda     #$FF
	jmp     staSysDBDataRstrFrmDialogue

L402C:  .byte   104			; top
	.byte	119			; bottom
	.word   136			; left
	.word	183			; right
L402C_len = * - L402C

; ----------------------------------------------------------------------------
L4032:  bit     a9H
        bmi     L403E
        lda     inputString2
        bne     L404C
L403B:  lda     #0
        rts

L403E:  CmpWI   stringX, 147
	beq     L403B
L404C:  lda     #$80
        rts

; ----------------------------------------------------------------------------
dlgbox_search_replace:
	.byte   DEF_DB_POS|1

        .byte   DBTXTSTR
        .byte   56
        .byte   12
        .word   txt_search_replace

        .byte   DBTXTSTR
        .byte   5
        .byte   28
        .word   txt_search_for

        .byte   DBTXTSTR
        .byte   5
        .byte   44
        .word   txt_replace_with

        .byte   DBTXTSTR
        .byte   18
        .byte   56
        .word   txt_whole_word

        .byte   DBTXTSTR
        .byte   18
        .byte   66
        .word   txt_all_pages

        .byte   DBUSRICON
        .byte   10
        .byte   17
        .word   usericon_textfield1

        .byte   DBUSRICON
        .byte   10
        .byte   33
        .word   usericon_textfield2

        .byte   DBUSRICON
        .byte   1
        .byte   49
        .word   usericon_checkbox1a	; checkbox: whole word

        .byte   DBUSRICON
        .byte   11
        .byte   49
        .word   usericon_checkbox1b	; checkbox: partial word

        .byte   DBUSRICON
        .byte   1
        .byte   59
        .word   usericon_checkbox2a	; checkbox: all pages

        .byte   DBUSRICON
        .byte   11
        .byte   59
        .word   usericon_checkbox2b	; checkbox: this page only

        .byte   DBUSRICON
        .byte   1
        .byte   72
        .word   usericon_next

        .byte   CANCEL
        .byte   17
        .byte   72

        .byte   DBOPVEC
        .word   L400F
        .byte   DB_USR_ROUT
        .word   setAppMainFunc1

        .byte   NULL

usericon_textfield1:
        .word   graph_box_104x14
        .byte   0
        .byte   0
        .byte   13
        .byte   14
        .word   callbackTextbox1
usericon_textfield2:
        .word   graph_box_104x14
        .byte   0
        .byte   0
        .byte   13
        .byte   14
        .word   callbackTextbox2
usericon_checkbox1a:
        .word   graph_checkbox_unchecked
        .byte   0
        .byte   0
        .byte   1
        .byte   9
        .word   callbackWholeWord
usericon_checkbox1b:
        .word   graph_checkbox_unchecked
        .byte   0
        .byte   0
        .byte   1
        .byte   9
        .word   callbackPartialWord
usericon_checkbox2a:
        .word   graph_checkbox_unchecked
        .byte   0
        .byte   0
        .byte   1
        .byte   9
        .word   callbackAllPages
usericon_checkbox2b:
        .word   graph_checkbox_unchecked
        .byte   0			; x
        .byte   0			; y
        .byte   1			; width
        .byte   9			; height
        .word   callbackThisPageOnly
usericon_next:
        .word   graph_next
        .byte   0
        .byte   0
        .byte   6
        .byte   16
        .word   callbackUserIcon7

graph_box_104x14:
        ; ********************************************************************************************************
        ; *......................................................................................................*
        ; *......................................................................................................*
        ; *......................................................................................................*
        ; *......................................................................................................*
        ; *......................................................................................................*
        ; *......................................................................................................*
        ; ********************************************************************************************************
        .byte   13,%11111111
        .byte	$DC+6,12
		.byte	1, %10000000
		.byte	11,%00000000
		.byte	1, %00000001
        .byte	13,%11111111
graph_checkbox_unchecked:
        ; ********
        ; *......*
        ; *......*
        ; *......*
        ; *......*
        ; *......*
        ; *......*
        ; *......*
        ; ********
        .byte   1,%11111111
        .byte	7,%10000001
        .byte	1,%11111111

; graph_next
	.include "graph5.s"

; ----------------------------------------------------------------------------
L4189:  tax                                     ; 4189 AA                       .
        beq     L41E1                           ; 418A F0 55                    .U
        ldy     #$00                            ; 418C A0 00                    ..
        tax                                     ; 418E AA                       .
        bpl     L4192                           ; 418F 10 01                    ..
        dey                                     ; 4191 88                       .
L4192:  sty     r5H                             ; 4192 84 0D                    ..
        sta     r5L                             ; 4194 85 0C                    ..
        tya                                     ; 4196 98                       .
        bpl     L41AD                           ; 4197 10 14                    ..
        lda     r15L                            ; 4199 A5 20                    . 
        sta     r1L                             ; 419B 85 04                    ..
        sub     r5L                             ; 419E E5 0C                    ..
        sta     r0L                             ; 41A0 85 02                    ..
        lda     r15H                            ; 41A2 A5 21                    .!
        sta     r1H                             ; 41A4 85 05                    ..
        sbc     r5H                             ; 41A6 E5 0D                    ..
        sta     r0H                             ; 41A8 85 03                    ..
        bra     L41BE                           ; 41AB 50 11                    P.

L41AD:  lda     r15L                            ; 41AD A5 20                    .
        sta     r0L                             ; 41AF 85 02                    ..
        add     r5L                             ; 41B2 65 0C                    e.
        sta     r1L                             ; 41B4 85 04                    ..
        lda     r15H                            ; 41B6 A5 21                    .!
        sta     r0H                             ; 41B8 85 03                    ..
        adc     r5H                             ; 41BA 65 0D                    e.
        sta     r1H                             ; 41BC 85 05                    ..
L41BE:  lda     pageEndPtr2                        ; 41BE A5 D0                    ..
        add     r5L                             ; 41C1 65 0C                    e.
        sta     pageEndPtr2                        ; 41C3 85 D0                    ..
        lda     pageEndPtr2+1                        ; 41C5 A5 D1                    ..
        adc     r5H                             ; 41C7 65 0D                    e.
        sta     pageEndPtr2+1                        ; 41C9 85 D1                    ..
        lda     pageEndPtr2                        ; 41CB A5 D0                    ..
        sub     r1L                             ; 41CE E5 04                    ..
        sta     r2L                             ; 41D0 85 06                    ..
        lda     pageEndPtr2+1                        ; 41D2 A5 D1                    ..
        sbc     r1H                             ; 41D4 E5 05                    ..
        sta     r2H                             ; 41D6 85 07                    ..
        inc     r2L                             ; 41D8 E6 06                    ..
        bne     L41DE                           ; 41DA D0 02                    ..
        inc     r2H                             ; 41DC E6 07                    ..
L41DE:  jsr     MoveData                        ; 41DE 20 7E C1                  ~.
L41E1:  rts                                     ; 41E1 60                       `

; ----------------------------------------------------------------------------
L41E2 = * ; related to curPage
