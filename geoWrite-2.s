; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  02 core text editing
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------

.segment        "CODE2": absolute

        jmp     appKeyVector        	; 0
        jmp     appOtherPressVec    	; 1
        jmp     process2        	; 2
        jmp     process2b       	; 3
        jmp     pageBreak           	; 4
        jmp     pastePicture        	; 5
        jmp     render_XXX		; 6
        jmp     selectionProcess     	; 7
        jmp     mouseScrollUpDown      	; 8
        jmp     callbackJustification	; 9
        jmp     selectPage          	; 10
        jmp     analyzeSelectionFont	; 11
        jmp     blockSelectionProcess 	; 12

; ----------------------------------------------------------------------------
pageBreak:
	jsr     GotoFirstMenu
        lda     #PAGE_LAST_USABLE
        jsr     PointRecord
        tya
        beq     :+
        jmp     _showCantAddPages

:	lda     curPage
        cmp     #PAGE_HEADER
        bcs     rts8
        bit     cursor0+cursor::srcline
        bmi     rts8
        bvc     :+
rts8:	rts

:	lda     #PAGE_BREAK		; inject page break char into keyboard
        sta     kbdString
        lda     #1
        sta     kbdStringCnt
        jsr     L35BB
        lda     #$FF

L3294:  sta     a4H
        jsr     setDirty
        jmp     insertKeyBuf

; ----------------------------------------------------------------------------
pastePicture:
	jsr     GotoFirstMenu
        bit     cursor0+cursor::srcline
        bmi     rts8
        bvs     rts8

        jsr     _addPhotoScrapToDoc
        bcs     rts8

        LoadB   zp_DDb, 0
        LoadB   kbdString, ESC_GRAPHICS
        LoadB   kbdStringCnt, 5
        lda     #A4L_08
        sta     a4L
        lda     cursor0+cursor::srcline
        jsr     L3294			; ???
        jmp     testDiskNotFull2

; ----------------------------------------------------------------------------
deleteInsertFromKbd:
	LoadB   a4L, 0
        MoveB   cursor0+cursor::srcline, a4H
        jsr     cmpCursor0_1Ptr		; is there a selection?
        beq     @1			; no
        jsr     invertSelectionIfNeeded
        jsr     deleteSelection
        lda     delCount
        beq     @2			; no del key? delete
        dec     delCount
        bra     @2			; del key? then consume del key

@1:	bit     cursor0+cursor::srcline
        bmi     rts7			; ignore if bit 7 || bit 6
        bvc     @2
@rts7:	rts

@2:	lda     delCount		; more del keys?
        beq     insertKeyBuf		; no

        jsr     cmpCursor0PtrPageCardset	; at start of page?
        bne     @3			; no

        lda     curPage			; first page?
        beq     insertKeyBuf		; yes

        jsr     _deleteAcrossPages
        cmp     #$FF			; do it?
        bne     insertKeyBuf		; no

        lda     a4L
        ora     #A4L_80
        sta     a4L

@3:	jsr     deleteChar
        dec     delCount
        bra     @2			; maybe more!

rts7 = @rts7

insertKeyBuf:
	lda     kbdStringCnt
	beq     render_XXX              ; kbd buffer empty
	sta     r3L			; # keys
	lda     #0
	sta     r3H
	LoadW   r0, kbdString
	jsr     insertText

; ----------------------------------------------------------------------------
; render after text deletion/insertion?
render_XXX:
	lda     a4L                             ; 331D A5 74                    .t
        beq     rts7                           ; 331F F0 C4                    ..

@loop:  ldx     a4H                             ; 3321 A6 75                    .u
        jsr     L3CF9                           ; 3323 20 F9 3C                  .<
        pha                                     ; 3326 48                       H
        bcc     @break                           ; 3327 90 3A                    .:
        lda     curPage                        ; 3329 A5 D3                    ..
        cmp     #PAGE_LAST_USABLE                            ; 332B C9 3C                    .<
        bcc     @1                           ; 332D 90 06                    ..
        jsr     loadCursor0PtrPageCardset                           ; 332F 20 76 27                  v'
        jsr     copyCursor0To1                           ; 3332 20 C0 28                  .(
@1:	jsr     L35B8                           ; 3335 20 B8 35                  .5
	LoadW_	pagePosY, 0
        lda     curPage                        ; 333E A5 D3                    ..
        cmp     #PAGE_HEADER                            ; 3340 C9 3D                    .=
        bcs     @2                           ; 3342 B0 15                    ..
        jsr     _readNextPage                           ; 3344 20 2F 10                  /.
	CmpW    pageEndPtr2, cursor0+cursor::ptr
	bcs     @2                           ; 3351 B0 06                    ..
        jsr     loadCursor0PtrPageCardset                           ; 3353 20 76 27                  v'
        jsr     copyCursor0To1                           ; 3356 20 C0 28                  .(
@2:	pla                                     ; 3359 68                       h
        ldx     #$FF                            ; 335A A2 FF                    ..
        stx     a4H                             ; 335C 86 75                    .u
        jsr     L35B8                           ; 335E 20 B8 35                  .5
        bne     @loop                           ; 3361 D0 BE                    ..

@break:	cpx     #0
        bpl     :+
        jsr     L35BB
:	pla
        cmp     sideFlippingOffset
        beq     @6
        sta     sideFlippingOffset
        cmp     #0
        bne     @3
        lda     #<(SC_PIX_WIDTH-6)	; max mouse x = SC_PIX_WIDTH-6
        ldx     #>(SC_PIX_WIDTH-6)
        cpx     mouseXPos+1
        bne     :+
        cmp     mouseXPos
:	bcs     @5
        sta     mouseXPos
        stx     mouseXPos+1
@3:	lda     mouseXPos+1		; min mouse x = 6
        cmp     #0
        bne     @4
        lda     mouseXPos
        cmp     #6
@4:	bcs     @5
        LoadB   mouseXPos+1, 0
        LoadB   mouseXPos, 6
@5:	lda     a4L                             ; 339B A5 74                    .t
        ora     #A4L_10                            ; 339D 09 10                    ..
        sta     a4L                             ; 339F 85 74                    .t
@6:	lda     a4L                             ; 33A1 A5 74                    .t
        and     #A4L_80                            ; 33A3 29 80                    ).
        beq     @7                           ; 33A5 F0 06                    ..
        jsr     L3CC1                           ; 33A7 20 C1 3C                  .<
        jmp     L079C                           ; 33AA 4C 9C 07                 L..

@7:	lda     a4L                             ; 33AD A5 74                    .t
        and     #A4L_20                            ; 33AF 29 20                    )
        beq     @8                           ; 33B1 F0 06                    ..
        jsr     L065D                           ; 33B3 20 5D 06                  ].
        jmp     L079C                           ; 33B6 4C 9C 07                 L..

@8:	lda     a4L                             ; 33B9 A5 74                    .t
        and     #A4L_10                            ; 33BB 29 10                    ).
        beq     @9                           ; 33BD F0 06                    ..
        jsr     L15CA                           ; 33BF 20 CA 15                  ..
        jmp     L079C                           ; 33C2 4C 9C 07                 L..

@9:	lda     a4L                             ; 33C5 A5 74                    .t
        and     #A4L_08                            ; 33C7 29 08                    ).
        beq     @10                           ; 33C9 F0 25                    .%
        MoveB	a4H, curLine
        MoveB_x a4H, curLine		; [XXX]
        lda     lintab_y,x                         ; 33D3 BD 60 2B                 .`+
        sta     r1H                             ; 33D6 85 05                    ..
        jsr     moveLintabR15                           ; 33D8 20 71 18                  q.
        jsr     readLintab                           ; 33DB 20 55 18                  U.
        jsr     L1B5E                           ; 33DE 20 5E 1B                  ^.
        MoveB_x a4H, curLine                        ; 33E3 86 D2                    ..
        jsr     getLineRuler                           ; 33E5 20 C9 1D                  ..
        lda     #$FF                            ; 33E8 A9 FF                    ..
        jsr     L15D8                           ; 33EA 20 D8 15                  ..
        jmp     L079C                           ; 33ED 4C 9C 07                 L..

; ----------------------------------------------------------------------------
@10:	lda     a4L                             ; 33F0 A5 74                    .t
        and     #A4L_04                            ; 33F2 29 04                    ).
        beq     @11                           ; 33F4 F0 0C                    ..
        lda     a4H                             ; 33F6 A5 75                    .u
        sta     curLine                        ; 33F8 85 D2                    ..
        lda     #$00                            ; 33FA A9 00                    ..
        sta     a6L                             ; 33FC 85 78                    .x
        sta     a6H                             ; 33FE 85 79                    .y
        beq     @12                           ; 3400 F0 0A                    ..
@11:	lda     a4L                             ; 3402 A5 74                    .t
        and     #A4L_02                            ; 3404 29 02                    ).
        beq     rts9                           ; 3406 F0 4E                    .N
        lda     a4H                             ; 3408 A5 75                    .u
        sta     curLine                        ; 340A 85 D2                    ..
@12:	ldx     a4H                             ; 340C A6 75                    .u
        jsr     getLineRuler                           ; 340E 20 C9 1D                  ..
        jsr     L3AD9                           ; 3411 20 D9 3A                  .:
        jmp     L079C                           ; 3414 4C 9C 07                 L..

; ----------------------------------------------------------------------------
; this is triggered by the main loop's keyVector call and processes incoming
; keys:
; * get all keys from the system's keyboard buffer into kbdString
; * optimization: if DEL key is encountered:
;	* if buffer is not empty, remove one key from the buffer
;	* else, increment DEL counter
; * if it's a control code, break, return it (handle remaining keys next time)
; -> caller must:
; 1. execute deletes
; 2. execute inserts
; 3. execute control code

processKbdQueue:
	lda     #0
        sta     delCount		; no excess DEL keys so far
        sta     kbdStringCnt		; kbd string empty
        sta     curControlKey		; no control key

        lda     keyData			; first character, as passed into keyVector

@again:	bmi     @ctrl			; "C=" shortcut, so stop processing

        cmp     #KEY_DELETE		; DEL key?
        beq     @del			; yes
        cmp     #KEY_INSERT		; SHIFT+DEL?
        beq     @del			; yes (same as DEL in GEOS)

        cmp     #CR			; return key?
        beq     @nctrl			; yes, does not count as a control key
        cmp     #TAB			; TAB key?
        beq     @nctrl			; yes, does not count as a control key

        cmp     #$20			; below $20, i.e. non-printable
        bcc     @ctrl			; yes, it's a control key, stop processing

@nctrl:	ldx     kbdStringCnt
        sta     kbdString,x		; add to kbd string
        inc     kbdStringCnt
@next:	jsr     GetNextChar		; are there more characters in the kbd queue?
        tax
        beq     rts9			; no, return
        bne     @again			; yes, repeat

@del:	ldx     kbdStringCnt		; are there characters in the kbd string?
        beq     @excss			; no, no characters to delete

        dec     kbdStringCnt		; remove previous character
        bra     @next			; and continue

@excss:	inc     delCount		; count excess delete characters
        bne     @next			; and continue

@ctrl:	sta     curControlKey		; save control key
rts9:	rts

; ----------------------------------------------------------------------------
appKeyVector:
	jsr     processKbdQueue		; convert queue into #DELs, string and control code
        lda     kbdStringCnt		; string present?
        ora     delCount		; or DELs presend?
        beq     @skip			; no, skip
        jsr     setDirty		; page is dirty, will need sync to disk
        jsr     deleteInsertFromKbd	; process the DELs and the string
@skip:
	lda     curControlKey		; convert shortcuts to upper case
        cmp     #$80+'a'
        bcc     @2
        cmp     #$80+'z'+1
        bcs     @2
        sec
        sbc     #'a'-'A'
@2:
	ldy     #shortcutTable_len-1
@loop:  cmp     shortcutTable,y
        beq     @found
        dey
        bpl     @loop
        rts				; no control key matched
@found: ldx     shortcutPtrsHi,y
	lda     shortcutPtrsLo,y
        jmp     CallRoutine

; ----------------------------------------------------------------------------
shortcutTable:
	.byte   $80+'A'			; align: left			| 0 | these match the 7
	.byte   $80+'E'			; align: center			| 1 | buttons defined by
	.byte   $80+'R'			; align: right			| 2 | the jst_checkbox_x
	.byte   $80+'J'			; align: full			| 3 | table
	.byte   $80+'K'			; space: single (1)		| 4 |
	.byte   $80+'M'			; space: one and a half (1 1/2)	| 5 |
	.byte   $80+'D'			; space: double (2)		| 6 |
	.byte   $80+'X'			; cut
	.byte   $80+'C'			; copy
	.byte   $80+'T'			; paste text
	.byte   $80+'W'			; paste picture
	.byte   $80+KEY_LARROW		; prev page
	.byte   $80+'+'			; next page
	.byte   $80+'G'			; goto page
	.byte   $80+'L'			; page break
	.byte   $80+'P'			; plain text
	.byte   $80+'B'			; bold
	.byte   $80+'I'			; italics
	.byte   $80+'O'			; outline
	.byte   $80+'U'			; underline
	.byte   $80+'<'			; subscript
	.byte   $80+'>'			; superscript
	.byte   $80+','			; subscript
	.byte   $80+'.'			; superscript
	.byte   $80+'H'			; open header
	.byte   $80+'F'			; open footer
	.byte   $80+'S'			; search
	.byte   $80+'N'			; find next
	.byte   $80+'Y'			; change
	.byte   $80+'V'			; X select page
	.byte   KEY_LEFT
	.byte   KEY_RIGHT
	.byte   KEY_UP
	.byte   KEY_DOWN
shortcutTable_len = * - shortcutTable

.define shortcutPtrs callbackRuler,callbackRuler,callbackRuler,callbackRuler,callbackRuler,callbackRuler,callbackRuler,_cut,_copy,_pasteText,pastePicture,_previousPage,_nextPage,_gotoPage,pageBreak,plainText,bold,italics,outline,underline,subscript,superscript,subscript,superscript,_openHeader,_openFooter,_search,_findNext,_change,selectPage,keyLeft,keyRight,keyUp,keyDown

shortcutPtrsLo:  .lobytes shortcutPtrs
shortcutPtrsHi:  .hibytes shortcutPtrs

deleteChar:
	lda     cursor0+cursor::ptr                            ; 34ED A5 80                    ..
        sta     r15L                            ; 34EF 85 20                    . 
        sta     tmpRangeEnd                             ; 34F1 85 B5                    ..
        lda     cursor0+cursor::ptr+1                        ; 34F3 A5 81                    ..
L34F5:  sta     r15H                            ; 34F5 85 21                    .!
L34F7:  sta     tmpRangeEnd+1                             ; 34F7 85 B6                    ..
        jsr     isEndOfCardSet                           ; 34F9 20 27 1C                  '.
L34FC:  sta     a9L                             ; 34FC 85 7E                    .~
        jsr     L354D                           ; 34FE 20 4D 35                  M5
        ldy     #$01                            ; 3501 A0 01                    ..
        lda     (r15),y                        ; 3503 B1 20                    . 
        cmp     #$11                            ; 3505 C9 11                    ..
        bne     L3515                           ; 3507 D0 0C                    ..
        jsr     pushR15                         ; 3509 20 6C 28                  l(
        jsr     L41B0                           ; 350C 20 B0 41                  .A
        jsr     popR15                          ; 350F 20 7F 28                  .(
        jsr     L35B8                           ; 3512 20 B8 35                  .5
L3515:  lda     r15L                            ; 3515 A5 20                    . 
        sta     cursor0+cursor::ptr                           ; 3517 85 80                    ..
        sta     a6L                             ; 3519 85 78                    .x
        lda     r15H                            ; 351B A5 21                    .!
        sta     cursor0+cursor::ptr+1                        ; 351D 85 81                    ..
        sta     a6H                             ; 351F 85 79                    .y
        jsr     isEndOfCardSet                           ; 3521 20 27 1C                  '.
        and     a9L                             ; 3524 25 7E                    %~
        bne     L352B                           ; 3526 D0 03                    ..
	jsr     getByteIntpNewCardSet
L352B:  lda     r15H                            ; 352B A5 21                    .!
        sta     tmpRangeStart+1                             ; 352D 85 B8                    ..
        lda     r15L                            ; 352F A5 20                    . 
        sta     tmpRangeStart                             ; 3531 85 B7                    ..
        jsr     deleteTextAndPicturesinRange                           ; 3533 20 4A 1C                  J.
        jsr     updatelintab_txtPtrIfNeeded_XXX                           ; 3536 20 08 1D                  ..
        lda     a4L                             ; 3539 A5 74                    .t
        ora     #A4L_02                            ; 353B 09 02                    ..
        sta     a4L                             ; 353D 85 74                    .t
        jsr     cmpPageEndPtr2PageCardSet                           ; 353F 20 09 28                  .(
        bne     L354A                           ; 3542 D0 06                    ..
        lda     a4L                             ; 3544 A5 74                    .t
        ora     #A4L_04                            ; 3546 09 04                    ..
        sta     a4L                             ; 3548 85 74                    .t
L354A:  jmp     copyCursor0To1                           ; 354A 4C C0 28                 L.(

; ----------------------------------------------------------------------------
L354D:  jsr     L3581                           ; 354D 20 81 35                  .5
L3550:  jsr     cmpR15Cursor0Ptr                           ; 3550 20 F3 27                  .'
        bcs     L3574                           ; 3553 B0 1F                    ..
        jsr     moveWR15R14                           ; 3555 20 D1 27                  .'
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3558 20 9E 14                  ..
        cmp     #$0D                            ; 355B C9 0D                    ..
        beq     L3568                           ; 355D F0 09                    ..
        cmp     #$0C                            ; 355F C9 0C                    ..
        bne     L3550                           ; 3561 D0 ED                    ..
        jsr     incWR15                           ; 3563 20 1E 27                  .'
        bcc     L3550                           ; 3566 90 E8                    ..
L3568:  jsr     getByteFromPage                           ; 3568 20 7A 14                  z.
        cmp     #$11                            ; 356B C9 11                    ..
        bne     L3550                           ; 356D D0 E1                    ..
        jsr     addRulerSizeToR15                           ; 356F 20 34 27                  4'
        bcc     L3550                           ; 3572 90 DC                    ..
L3574:  lda     r14H                            ; 3574 A5 1F                    ..
        sta     r15H                            ; 3576 85 21                    .!
        lda     r14L                            ; 3578 A5 1E                    ..
        sta     r15L                            ; 357A 85 20                    . 
        rts                                     ; 357C 60                       `

; ----------------------------------------------------------------------------
L357D:  jsr     popR15                          ; 357D 20 7F 28                  .(
        rts                                     ; 3580 60                       `

; ----------------------------------------------------------------------------
L3581:  ldx     a4H                             ; 3581 A6 75                    .u
        bmi     L35B5                           ; 3583 30 30                    00
        jsr     moveLintabR15                           ; 3585 20 71 18                  q.
        jsr     pushR15                         ; 3588 20 6C 28                  l(
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 358B 20 9E 14                  ..
        jsr     cmpR15Cursor0Ptr                           ; 358E 20 F3 27                  .'
        bcc     L357D                           ; 3591 90 EA                    ..
        jsr     popR15                          ; 3593 20 7F 28                  .(
        lda     a4L                             ; 3596 A5 74                    .t
        ora     #A4L_04                            ; 3598 09 04                    ..
        sta     a4L                             ; 359A 85 74                    .t
        dec     a4H                             ; 359C C6 75                    .u
        bpl     L3581                           ; 359E 10 E1                    ..
	SubVW	$3C, pagePosY
        bcs     L35B5                           ; 35AD B0 06                    ..
	LoadW_	pagePosY, 0
L35B5:  jsr     LoadR15_MEM_PAGE                           ; 35B5 20 BF 26                  .&
L35B8:  lda     a4L                             ; 35B8 A5 74                    .t
        .byte   $2C                             ; 35BA 2C                       ,
L35BB:  lda     #0                            ; 35BB A9 00                    ..
        ora     #A4L_20                            ; 35BD 09 20                    .
        sta     a4L                             ; 35BF 85 74                    .t
        rts                                     ; 35C1 60                       `

; ----------------------------------------------------------------------------
; r0: text, r3: length
insertText:
	PushW	r3                              ; 35C7 48                       H
        jsr     killPrompt                      ; 35C8 20 6E 1D                  n.
	PopW	r3
        lda     cursor0+cursor::ptr                           ; 35D1 A5 80                    ..
        sta     r15L                            ; 35D3 85 20                    . 
        sta     a6L                             ; 35D5 85 78                    .x
        sta     r1L                             ; 35D7 85 04                    ..
        add     r3L                             ; 35DA 65 08                    e.
        sta     cursor0+cursor::ptr                           ; 35DC 85 80                    ..
        sta     r7L                             ; 35DE 85 10                    ..
        lda     cursor0+cursor::ptr+1                        ; 35E0 A5 81                    ..
        sta     r15H                            ; 35E2 85 21                    .!
        sta     a6H                             ; 35E4 85 79                    .y
        sta     r1H                             ; 35E6 85 05                    ..
        adc     r3H                             ; 35E8 65 09                    e.
        sta     cursor0+cursor::ptr+1                        ; 35EA 85 81                    ..
        sta     r7H                             ; 35EC 85 11                    ..
        ldy     #$00                            ; 35EE A0 00                    ..
        lda     (r15),y                        ; 35F0 B1 20                    . 
        cmp     #$10                            ; 35F2 C9 10                    ..
        bne     L35FF                           ; 35F4 D0 09                    ..
        jsr     pushR15                         ; 35F6 20 6C 28                  l(
        jsr     L3581                           ; 35F9 20 81 35                  .5
        jsr     popR15                          ; 35FC 20 7F 28                  .(
L35FF:  lda     r3H                             ; 35FF A5 09                    ..
        sta     r2H                             ; 3601 85 07                    ..
        lda     r3L                             ; 3603 A5 08                    ..
        sta     r2L                             ; 3605 85 06                    ..
        lda     #$00                            ; 3607 A9 00                    ..
        sta     r8L                             ; 3609 85 12                    ..
	CmpWI	r15, PAGE_CARDSET                            ; 360D C9 43                    .C
	beq     L3629                           ; 3615 F0 12                    ..
        CmpW    curFont, cursor0+cursor::font
	bne     L3629                           ; 3621 D0 06                    ..
        lda     currentMode                     ; 3623 A5 2E                    ..
        cmp     cursor0+cursor::mode                        ; 3625 C5 89                    ..
        beq     L3648                           ; 3627 F0 1F                    ..
L3629:  inc     r8L                             ; 3629 E6 12                    ..
        jsr     addWI4ToR3                           ; 362B 20 3A 27                  :'
        ldx     #r7                            ; 362E A2 10                    ..
        jsr     addWI4ToZp                           ; 3630 20 3C 27                  <'
        jsr     L3667                           ; 3633 20 67 36                  g6
        ldy     #0                            ; 3636 A0 00                    ..
        lda     (r15),y                        ; 3638 B1 20                    . 
        jsr     isPageBreakOrNull                           ; 363A 20 8C 13                  ..
        bcs     L3648                           ; 363D B0 09                    ..
        cmp     #NEWCARDSET                            ; 363F C9 17                    ..
        beq     L3648                           ; 3641 F0 05                    ..
        jsr     addWI4ToR3                           ; 3643 20 3A 27                  :'
        inc     r8L                             ; 3646 E6 12                    ..
L3648:  jsr     makeSpace                           ; 3648 20 9F 1C                  ..
        jsr     MoveData                        ; 364B 20 7E C1                  ~.
        lda     r8L                             ; 364E A5 12                    ..
        beq     L365E                           ; 3650 F0 0C                    ..
        jsr     L3671                           ; 3652 20 71 36                  q6
        lda     r8L                             ; 3655 A5 12                    ..
        cmp     #$02                            ; 3657 C9 02                    ..
        bne     L365E                           ; 3659 D0 03                    ..
        jsr     storeNewCardSetFromCursor0                           ; 365B 20 3B 1C                  ;.
L365E:  lda     a4L                             ; 365E A5 74                    .t
        ora     #A4L_02                            ; 3660 09 02                    ..
        sta     a4L                             ; 3662 85 74                    .t
        jmp     copyCursor0To1                           ; 3664 4C C0 28                 L.(

; ----------------------------------------------------------------------------
L3667:  ldx     #r1                            ; 3667 A2 04                    ..
        jsr     addWI4ToZp                           ; 3669 20 3C 27                  <'
        ldx     #cursor0+cursor::ptr                            ; 366C A2 80                    ..
        jmp     addWI4ToZp                           ; 366E 4C 3C 27                 L<'

; ----------------------------------------------------------------------------
L3671:  ldy     #$00                            ; 3671 A0 00                    ..
        lda     #NEWCARDSET                            ; 3673 A9 17                    ..
        sta     (r15),y                        ; 3675 91 20                    . 
        jsr     L3680                           ; 3677 20 80 36                  .6
        iny                                     ; 367A C8                       .
L367B:  lda     currentMode                     ; 367B A5 2E                    ..
        sta     (r15),y                        ; 367D 91 20                    . 
        rts                                     ; 367F 60                       `

; ----------------------------------------------------------------------------
L3680:  iny                                     ; 3680 C8                       .
        lda     curFont                             ; 3681 A5 72                    .r
        sta     (r15),y                        ; 3683 91 20                    . 
        iny                                     ; 3685 C8                       .
        lda     curFont+1                             ; 3686 A5 73                    .s
        sta     (r15),y                        ; 3688 91 20                    . 
        rts                                     ; 368A 60                       `

; ----------------------------------------------------------------------------
L368B:  jsr     moveCursor0Ptr_r15                           ; 368B 20 A1 27                  .'
L368E:  ldy     #0                            ; 368E A0 00                    ..
        lda     (r15),y                        ; 3690 B1 20                    . 
        cmp     #NEWCARDSET                            ; 3692 C9 17                    ..
        bne     L36A8                           ; 3694 D0 12                    ..
        bit     a9L                             ; 3696 24 7E                    $~
        bpl     L36A0                           ; 3698 10 06                    ..
        jsr     L3680                           ; 369A 20 80 36                  .6
        bra     L36A5                           ; 369E 50 05                    P.

L36A0:  ldy     #3                            ; 36A0 A0 03                    ..
        jsr     L367B                           ; 36A2 20 7B 36                  {6
L36A5:  jsr     addWI3R15                           ; 36A5 20 18 27                  .'
L36A8:  jsr     incWR15                           ; 36A8 20 1E 27                  .'
        jsr     CmpR15Cursor1Ptr                           ; 36AB 20 1F 28                  .(
        bne     L368E                           ; 36AE D0 DE                    ..
L36B0:  rts                                     ; 36B0 60                       `

; ----------------------------------------------------------------------------
L36B1:  LoadW	r3, 4
        jsr     moveCursor1Ptr_r15                           ; 36B9 20 AA 27                  .'
        jsr     isEndOfCardSet                           ; 36BC 20 27 1C                  '.
        bne     L36CB                           ; 36BF D0 0A                    ..
        lda     cursor1+cursor::srcline                      ; 36C1 A5 8E                    ..
        sta     a4H                             ; 36C3 85 75                    .u
        jsr     makeSpace                           ; 36C5 20 9F 1C                  ..
        jsr     storeNewCardSetFromCursor1                           ; 36C8 20 4C 1E                  L.
L36CB:  jsr     moveCursor0Ptr_r15                           ; 36CB 20 A1 27                  .'
        jsr     isEndOfCardSet                           ; 36CE 20 27 1C                  '.
        bne     L36B0                           ; 36D1 D0 DD                    ..
        lda     cursor0+cursor::srcline                        ; 36D3 A5 84                    ..
        sta     a4H                             ; 36D5 85 75                    .u
        jsr     makeSpace                           ; 36D7 20 9F 1C                  ..
        jsr     L3671                           ; 36DA 20 71 36                  q6
        ldx     #cursor1+cursor::ptr                            ; 36DD A2 8A                    ..
        jmp     addWI4ToZp                           ; 36DF 4C 3C 27                 L<'

; ----------------------------------------------------------------------------
mouseScrollUpDown:
	lda     #0
        sta     vars+$39
        sta     a9H
        lda     mouseYPos
        bne     :+
        jmp     mouseScrollUp		; mouse is at top of screen
:	cmp     #SC_PIX_HEIGHT-1
        bne     rts5
					; mouse is at bottom of screen
mouseScrollDown:
	jsr     pushRulerData
	AddVW2	-104, usablePageHeight, r0
	CmpW	pagePosY, r0
	bcs     @1			; already at the bottom
        ldx     #0
        jsr     getLineRuler		; ruler of line 0
        ldx     #0
        jsr     setCursor3AndJustificationFromLine
        jsr     measureLine		; measure top line; page end?
        bcc     _2			; no
@1:	jsr     popRulerData

rts5:	rts

_2:	jsr     killPrompt                      ; 3723 20 6E 1D                  n.
        jsr     CmpCursor2_0Ptr                           ; 3726 20 40 28                  @(
        bne     :+                           ; 3729 D0 03                    ..
        dec     vars+$39                        ; 372B CE 39 02                 .9.
:	bit     cursor1+cursor::srcline                      ; 372E 24 8E                    $.
        bvc     :+                           ; 3730 50 02                    P.
        dec     a9H                             ; 3732 C6 7F                    ..
:	jsr     L3831                           ; 3734 20 31 38                  18
        jsr     L3893                           ; 3737 20 93 38                  .8
	SubB_	r0L, cursor2+cursor::py                        ; 373F 85 99                    ..
        jsr     L39C9                           ; 3741 20 C9 39                  .9
        jsr     L38BA                           ; 3744 20 BA 38                  .8
        stx     curLine                        ; 3747 86 D2                    ..
        txa                                     ; 3749 8A                       .
        pha                                     ; 374A 48                       H
        bpl     @3                           ; 374B 10 16                    ..

        LoadB	curLine, 0
        tax                                     ; 3751 AA                       .
        jsr     getLineRuler                           ; 3752 20 C9 1D                  ..
        ldx     #0                            ; 3755 A2 00                    ..
	MoveW	topScrTxtPtr, r15
        lda     #36                            ; 375F A9 24                    .$
        bne     @4                           ; 3761 D0 0B                    ..

@3:	jsr     getLineRuler                           ; 3763 20 C9 1D                  ..
        ldx     curLine                        ; 3766 A6 D2                    ..
        jsr     moveLintabR15                           ; 3768 20 71 18                  q.
        lda     lintab_y,x                         ; 376B BD 60 2B                 .`+

@4:	sta     r1H                             ; 376E 85 05                    ..
        jsr     readLintab                           ; 3770 20 55 18                  U.
        jsr     L1B5E                           ; 3773 20 5E 1B                  ^.
        pla                                     ; 3776 68                       h
        tax                                     ; 3777 AA                       .
        bmi     @5                           ; 3778 30 11                    0.

        lda     lintab_justification,x                         ; 377A BD DE 2B                 ..+
        bmi     @6			; JST_80

        inx                                     ; 377F E8                       .
        lda     lintab_height,x                         ; 3780 BD 75 2B                 .u+
        bne     @6                           ; 3783 D0 08                    ..

        jsr     L15FC                           ; 3785 20 FC 15                  ..
        bra     @7                           ; 3789 50 0C                    P.

@5:	ldx     #0                            ; 378B A2 00                    ..
@6:	stx     curLine                        ; 378D 86 D2                    ..
        jsr     getLineRuler                           ; 378F 20 C9 1D                  ..
        lda     #0                            ; 3792 A9 00                    ..
        jsr     L15D8                           ; 3794 20 D8 15                  ..

@7:	ldx     #$FF                            ; 3797 A2 FF                    ..
        lda     #$FF                            ; 3799 A9 FF                    ..

L379B:  sta     r0L                             ; 379B 85 02                    ..
        bit     cursor0+cursor::srcline                        ; 379D 24 84                    $.
        bmi     @1                           ; 379F 30 07                    0.
        bvs     @1                           ; 37A1 70 05                    p.

        add     cursor0+cursor::srcline                        ; 37A4 65 84                    e.
        sta     cursor0+cursor::srcline                        ; 37A6 85 84                    ..

@1:	bit     cursor1+cursor::srcline                      ; 37A8 24 8E                    $.
        bmi     @2                           ; 37AA 30 09                    0.
        bvs     @2                           ; 37AC 70 07                    p.

        clc                                     ; 37AE 18                       .
        lda     cursor1+cursor::srcline                      ; 37AF A5 8E                    ..
        adc     r0L                             ; 37B1 65 02                    e.
        sta     cursor1+cursor::srcline                      ; 37B3 85 8E                    ..
@2:	jsr     L385F                           ; 37B5 20 5F 38                  _8
        jsr     popRulerData                           ; 37B8 20 93 26                  .&
        jsr     setPromptFontMetricsUpdateRulerUI                           ; 37BB 20 7A 1D                  z.
        jsr     updatePageIndicator                           ; 37BE 20 97 05                  ..
rts6:	rts                                     ; 37C1 60                       `

; ----------------------------------------------------------------------------
; mouse is at top of screen
mouseScrollUp:
	CmpWI	topScrTxtPtr, MEM_PAGE                            ; 37C4 C9 43                    .C
	beq     rts6                           ; 37CC F0 F3                    ..
        jsr     pushRulerData                           ; 37CE 20 73 26                  s&
        jsr     killPrompt                      ; 37D1 20 6E 1D                  n.
        CmpW    cursor2+cursor::ptr, cursor1+cursor::ptr
	bne     :+                           ; 37DE D0 03                    ..
        dec     vars+$39                        ; 37E0 CE 39 02                 .9.
:	bit     cursor0+cursor::srcline                        ; 37E3 24 84                    $.
        bpl     :+                           ; 37E5 10 02                    ..
        dec     a9H                             ; 37E7 C6 7F                    ..
:	jsr     L3831                           ; 37E9 20 31 38                  18
	MoveW	topScrTxtPtr, a7
        jsr     L398D                           ; 37F4 20 8D 39                  .9
        jsr     L39F1                           ; 37F7 20 F1 39                  .9
        jsr     L38FB                           ; 37FA 20 FB 38                  .8
	AddB___ lintab_height, cursor2+cursor::py                        ; 3803 85 99                    ..
        lda     #0                            ; 3805 A9 00                    ..
        sta     a6L                             ; 3807 85 78                    .x
        sta     a6H                             ; 3809 85 79                    .y
        jsr     L15A7                           ; 380B 20 A7 15                  ..
        jsr     moveR15Lintab                           ; 380E 20 8D 1B                  ..
        lda     curFont                             ; 3811 A5 72                    .r
        sta     lintab_font_lo,x                         ; 3813 9D 8A 2B                 ..+
        lda     curFont+1                             ; 3816 A5 73                    .s
        sta     lintab_font_hi,x                         ; 3818 9D 9F 2B                 ..+
        lda     currentMode                     ; 381B A5 2E                    ..
        sta     lintab_mode,x                         ; 381D 9D B4 2B                 ..+
        jsr     L3AD9                           ; 3820 20 D9 3A                  .:
        ldx     zp_B9b                             ; 3823 A6 B9                    ..
        stx     curLine                        ; 3825 86 D2                    ..
        jsr     L15FC                           ; 3827 20 FC 15                  ..
        ldx     #0                            ; 382A A2 00                    ..
        lda     #1                            ; 382C A9 01                    ..
        jmp     L379B                           ; 382E 4C 9B 37                 L.7

; ----------------------------------------------------------------------------
L3831:  jsr     L384C                           ; 3831 20 4C 38                  L8
        bcc     L384B                           ; 3834 90 15                    ..
        PushW   cursor3+cursor::ptr                        ; 3839 A5 9E                    ..
        PushB   cursor3+cursor::height                        ; 383C A5 A4                    ..
        jsr     invertSelectionIfNeeded                           ; 383F 20 6E 08                  n.
        PopB    cursor3+cursor::height                        ; 3843 85 A4                    ..
        PopW    cursor3+cursor::ptr                        ; 3846 85 9E                    ..
L384B:  rts                                     ; 384B 60                       `

; ----------------------------------------------------------------------------
L384C:  lda     vars+$39                        ; 384C AD 39 02                 .9.
        and     zp_D9b                        ; 384F 25 D9                    %.
        bne     @1                           ; 3851 D0 08                    ..
        lda     zp_D9b                        ; 3853 A5 D9                    ..
        eor     #$FF                            ; 3855 49 FF                    I.
        and     a9H                             ; 3857 25 7F                    %.
        beq     @2                           ; 3859 F0 02                    ..
@1:	sec                                     ; 385B 38                       8
        rts                                     ; 385C 60                       `

@2:	clc                                     ; 385D 18                       .
        rts                                     ; 385E 60                       `

; ----------------------------------------------------------------------------
L385F:  lda     vars+$39                        ; 385F AD 39 02                 .9.
        eor     #$FF                            ; 3862 49 FF                    I.
        and     zp_D9b                        ; 3864 25 D9                    %.
        bne     L386E                           ; 3866 D0 06                    ..
        lda     a9H                             ; 3868 A5 7F                    ..
        ora     zp_D9b                        ; 386A 05 D9                    ..
        bne     L3884                           ; 386C D0 16                    ..

L386E:  ldx     #0			; push cursor2 [XXX reuse function pushCursor2]
@loop1:	lda     cursor2+cursor::ptr,x
        pha
        inx
        cpx     #10
        bne     @loop1

        jsr     L083C

        ldx     #9			; pop cursor2
@loop2:	pla
        sta     cursor2+cursor::ptr,x
        dex
        bpl     @loop2

L3883:  rts                                     ; 3883 60                       `

L3884:  txa                                     ; 3884 8A                       .
        pha                                     ; 3885 48                       H
        jsr     L0862                           ; 3886 20 62 08                  b.
        pla                                     ; 3889 68                       h
        beq     L3883                           ; 388A F0 F7                    ..
        ldx     #cursor0-userzp                            ; 388C A2 00                    ..
        ldy     #cursor2-userzp                            ; 388E A0 14                    ..
        jmp     copyCursor                           ; 3890 4C D0 28                 L.(

; ----------------------------------------------------------------------------
L3893:  MoveW   cursor3+cursor::ptr, topScrTxtPtr
        MoveW   curFont, tmpFont3                        ; 389D 85 C6                    ..
        MoveB   currentMode, tmpMode3                        ; 38A5 85 C7                    ..
        jsr     copyRuler1To2                           ; 38A7 20 8E 28                  .(
        lda     cursor3+cursor::height                        ; 38AA A5 A4                    ..
        sta     r0L                             ; 38AC 85 02                    ..
        add     pagePosY                             ; 38AF 65 76                    ev
        sta     pagePosY                             ; 38B1 85 76                    .v
        lda     #0                            ; 38B3 A9 00                    ..
        adc     pagePosY+1                             ; 38B5 65 77                    ew
        sta     pagePosY+1                             ; 38B7 85 77                    .w
        rts                                     ; 38B9 60                       `

; ----------------------------------------------------------------------------
L38BA:  lda     lintab_justification                           ; 38BA AD DE 2B                 ..+
        sta     rulerData2+ruler::justification                           ; 38BD 8D 9E 2F                 ../
        lda     cursor0+cursor::height                        ; 38C0 A5 86                    ..
        sec                                     ; 38C2 38                       8
        sbc     r0L                             ; 38C3 E5 02                    ..
        sta     cursor0+cursor::height                        ; 38C5 85 86                    ..
        bcc     @1                           ; 38C7 90 04                    ..
        cmp     #36                            ; 38C9 C9 24                    .$
        bcs     @2                           ; 38CB B0 04                    ..
@1:	LoadB   cursor0+cursor::srcline, $80
@2:	jsr     L3982                           ; 38D1 20 82 39                  .9

@loop2:	ldy     #1                            ; 38D4 A0 01                    ..
@loop1:	lda     (r5),y                         ; 38D6 B1 0C                    ..
        dey                                     ; 38D8 88                       .
        sta     (r5),y                         ; 38D9 91 0C                    ..
        iny                                     ; 38DB C8                       .
        iny                                     ; 38DC C8                       .
        cpy     #21                            ; 38DD C0 15                    ..
        bne     @loop1                           ; 38DF D0 F5                    ..
        jsr     addR5_21                           ; 38E1 20 75 39                  u9
        bne     @loop2                           ; 38E4 D0 EE                    ..

        ldx     #0                            ; 38E6 A2 00                    ..
@loop3:	lda     lintab_y,x                         ; 38E8 BD 60 2B                 .`+
        sec                                     ; 38EB 38                       8
        sbc     r0L                             ; 38EC E5 02                    ..
        sta     lintab_y,x                         ; 38EE 9D 60 2B                 .`+
        lda     lintab_height,x                         ; 38F1 BD 75 2B                 .u+
        beq     @break                           ; 38F4 F0 03                    ..
        inx                                     ; 38F6 E8                       .
        bne     @loop3                           ; 38F7 D0 EF                    ..

@break:	dex                                     ; 38F9 CA                       .
        rts                                     ; 38FA 60                       `

; ----------------------------------------------------------------------------
L38FB:  jsr     L3982                           ; 38FB 20 82 39                  .9

@loop1:	ldy     #19                            ; 38FE A0 13                    ..
@loop2:	lda     (r5),y                         ; 3900 B1 0C                    ..
        iny                                     ; 3902 C8                       .
        sta     (r5),y                         ; 3903 91 0C                    ..
        dey                                     ; 3905 88                       .
        dey                                     ; 3906 88                       .
        bpl     @loop2                           ; 3907 10 F7                    ..
        jsr     addR5_21                           ; 3909 20 75 39                  u9
        bne     @loop1                           ; 390C D0 F0                    ..

        lda     r0L                             ; 390E A5 02                    ..
        sta     lintab_height                           ; 3910 8D 75 2B                 .u+
        ldx     #1                            ; 3913 A2 01                    ..
L3915:  lda     lintab_y,x                         ; 3915 BD 60 2B                 .`+
        add     r0L                             ; 3919 65 02                    e.
        sta     lintab_y,x                         ; 391B 9D 60 2B                 .`+
        clc                                     ; 391E 18                       .
        adc     lintab_height,x                         ; 391F 7D 75 2B                 }u+
        bcs     L3933                           ; 3922 B0 0F                    ..
        cmp     windowBottom                    ; 3924 C5 34                    .4
        beq     L392A                           ; 3926 F0 02                    ..
        bcs     L3933                           ; 3928 B0 09                    ..
L392A:  lda     lintab_height,x                         ; 392A BD 75 2B                 .u+
        beq     L393E                           ; 392D F0 0F                    ..
        inx                                     ; 392F E8                       .
        bra     L3915                           ; 3931 50 E2                    P.

L3933:  lda     lintab_justification,x                         ; 3933 BD DE 2B                 ..+
        bpl     L3939			; JST_80
        inx                                     ; 3938 E8                       .
L3939:  lda     #0                            ; 3939 A9 00                    ..
        sta     lintab_height,x                         ; 393B 9D 75 2B                 .u+
L393E:  lda     lintab_y-1,x                         ; 393E BD 5F 2B                 ._+
        clc                                     ; 3941 18                       .
        adc     lintab_height-1,x                         ; 3942 7D 74 2B                 }t+
        bcs     rts4                           ; 3945 B0 3A                    .:
        cmp     windowBottom                    ; 3947 C5 34                    .4
        beq     :+                           ; 3949 F0 02                    ..
        bcs     rts4                           ; 394B B0 34                    .4
:	dex                                     ; 394D CA                       .
        pha                                     ; 394E 48                       H
        jsr     L384C                           ; 394F 20 4C 38                  L8
        pla                                     ; 3952 68                       h
        bcs     L396F                           ; 3953 B0 1A                    ..
        bit     cursor0+cursor::srcline                        ; 3955 24 84                    $.
        bvs     L396F                           ; 3957 70 16                    p.
        cpx     cursor0+cursor::srcline                        ; 3959 E4 84                    ..
        beq     L396F                           ; 395B F0 12                    ..
        bit     cursor1+cursor::srcline                      ; 395D 24 8E                    $.
        bvs     L3972                           ; 395F 70 11                    p.
        cpx     cursor1+cursor::srcline                      ; 3961 E4 8E                    ..
        beq     L3972                           ; 3963 F0 0D                    ..
        bit     zp_D9b                        ; 3965 24 D9                    $.
        bpl     L396F                           ; 3967 10 06                    ..
        cpx     cursor1+cursor::srcline                      ; 3969 E4 8E                    ..
        bcc     L3972                           ; 396B 90 05                    ..
        beq     L3972                           ; 396D F0 03                    ..
L396F:  jmp     clearPartOfScreen                           ; 396F 4C 6D 16                 Lm.

L3972:  jmp     fillPartOfScreen                           ; 3972 4C 69 16                 Li.

; ----------------------------------------------------------------------------
addR5_21:
	AddVW	21, r5
	dex                                     ; 3980 CA                       .
rts4:	rts                                     ; 3981 60                       `

; ----------------------------------------------------------------------------
L3982:  LoadW   r5, lintab_txtPtr_lo                            ; 3986 A9 36                    .6
        ldx     #$0C                            ; 398A A2 0C                    ..
        rts                                     ; 398C 60                       `

; ----------------------------------------------------------------------------
L398D:  jsr     resetToPageStart                           ; 398D 20 B1 26                  .&
        lda     #JST_20
        sta     rulerData2+ruler::justification                           ; 3992 8D 9E 2F                 ../
L3995:  jsr     measureLine                           ; 3995 20 7A 10                  z.
        lda     cursor3+cursor::ptr+1                        ; 3998 A5 9F                    ..
        cmp     a7H                             ; 399A C5 7B                    .{
        bne     L39A2                           ; 399C D0 04                    ..
        lda     cursor3+cursor::ptr                        ; 399E A5 9E                    ..
        cmp     a7L                             ; 39A0 C5 7A                    .z
L39A2:  bcs     L39AA                           ; 39A2 B0 06                    ..
        jsr     L27BC                           ; 39A4 20 BC 27                  .'
        bra     L3995                           ; 39A8 50 EB                    P.

L39AA:  MoveW   pageTextHeight, pagePosY
	MoveW   cursor3+cursor::font, tmpFont3                        ; 39B8 85 C5                    ..
        lda     cursor3+cursor::mode                        ; 39BA A5 A7                    ..
        sta     tmpMode3                        ; 39BC 85 C7                    ..
	MoveW	r15, topScrTxtPtr
        jmp     copyRuler1To2                           ; 39C6 4C 8E 28                 L.(

; ----------------------------------------------------------------------------
L39C9:  lda     #$24                            ; 39C9 A9 24                    .$
        sta     r1L                             ; 39CB 85 04                    ..
        lda     lintab_height+1                           ; 39CD AD 76 2B                 .v+
        beq     L39E5                           ; 39D0 F0 13                    ..
L39D2:  lda     r1L                             ; 39D2 A5 04                    ..
        add     r0L                             ; 39D5 65 02                    e.
        cmp     windowBottom                    ; 39D7 C5 34                    .4
        beq     L39DD                           ; 39D9 F0 02                    ..
        bcs     L39E5                           ; 39DB B0 08                    ..
L39DD:  jsr     copyScanline                           ; 39DD 20 0C 3A                  .:
        inc     r1L                             ; 39E0 E6 04                    ..
        bra     L39D2                           ; 39E3 50 ED                    P.

L39E5:  lda     r0L                             ; 39E5 A5 02                    ..
        pha                                     ; 39E7 48                       H
        lda     r1L                             ; 39E8 A5 04                    ..
        jsr     clearPartOfScreen                           ; 39EA 20 6D 16                  m.
        pla                                     ; 39ED 68                       h
        sta     r0L                             ; 39EE 85 02                    ..
L39F0:  rts                                     ; 39F0 60                       `

; ----------------------------------------------------------------------------
L39F1:  lda     cursor3+cursor::height                        ; 39F1 A5 A4                    ..
        sta     r0L                             ; 39F3 85 02                    ..
        lda     windowBottom                    ; 39F5 A5 34                    .4
        sta     r1L                             ; 39F7 85 04                    ..
@loop1:	lda     r1L                             ; 39F9 A5 04                    ..
        sec                                     ; 39FB 38                       8
        sbc     r0L                             ; 39FC E5 02                    ..
        bcc     L39F0                           ; 39FE 90 F0                    ..
        cmp     #$24                            ; 3A00 C9 24                    .$
        bcc     L39F0                           ; 3A02 90 EC                    ..
        jsr     copyScanline                           ; 3A04 20 0C 3A                  .:
        dec     r1L                             ; 3A07 C6 04                    ..
        bra     @loop1                           ; 3A0A 50 ED                    P.

; ----------------------------------------------------------------------------
; source: x, destination: r1L
copyScanline:
	tax
        jsr     GetScanLine
        MoveW	r5, r4
        ldx     r1L
        jsr     GetScanLine
        ldy     #0
        ldx     #SC_BYTE_WIDTH
@loop:  lda     (r4),y
        sta     (r5),y
        tya
        add     #8
        tay
        bcc     :+
        inc     r4H
        inc     r5H
:	dex
        bne     @loop
        rts

; ----------------------------------------------------------------------------
keyLeft:
	jsr     setPromptPosition                           ; 3A34 20 92 1D                  ..
        bcs     L39F0                           ; 3A37 B0 B7                    ..
        jsr     cmpCursor0PtrPageCardset                           ; 3A39 20 FE 27                  .'
        beq     L39F0                           ; 3A3C F0 B2                    ..
        jsr     moveCursor0Ptr_r15                           ; 3A3E 20 A1 27                  .'
        lda     cursor0+cursor::srcline                        ; 3A41 A5 84                    ..
        sta     a4H                             ; 3A43 85 75                    .u
        jsr     L354D                           ; 3A45 20 4D 35                  M5
L3A48:  lda     cursor0+cursor::srcline                        ; 3A48 A5 84                    ..
        sta     r3L                             ; 3A4A 85 08                    ..
        jsr     L08CD                           ; 3A4C 20 CD 08                  ..
        bcc     L3A74                           ; 3A4F 90 23                    .#
        bit     cursor3+cursor::srcline                        ; 3A51 24 A2                    $.
        bmi     keyUp                           ; 3A53 30 43                    0C
        bvs     keyDown                           ; 3A55 70 6F                    po
        lda     r10H                            ; 3A57 A5 17                    ..
        sta     r11H                            ; 3A59 85 19                    ..
        lda     r10L                            ; 3A5B A5 16                    ..
        sta     r11L                            ; 3A5D 85 18                    ..
        jsr     L3DA9                           ; 3A5F 20 A9 3D                  .=
        sta     sideFlippingOffset                           ; 3A62 8D BC 2F                 ../
        lda     cursor3+cursor::ptr+1                        ; 3A65 A5 9F                    ..
        pha                                     ; 3A67 48                       H
        lda     cursor3+cursor::ptr                        ; 3A68 A5 9E                    ..
        pha                                     ; 3A6A 48                       H
        jsr     L15CA                           ; 3A6B 20 CA 15                  ..
        jsr     popR15                          ; 3A6E 20 7F 28                  .(
        bra     L3A48                           ; 3A72 50 D4                    P.

L3A74:  jsr     copyCursor3To0                           ; 3A74 20 CC 28                  .(
        jsr     copyCursor0To1                           ; 3A77 20 C0 28                  .(
        jmp     L0781                           ; 3A7A 4C 81 07                 L..

; ----------------------------------------------------------------------------
keyRight:
	jsr     setPromptPosition                           ; 3A7D 20 92 1D                  ..
        bcs     L3AC5                           ; 3A80 B0 43                    .C
        jsr     moveCursor0Ptr_r15                           ; 3A82 20 A1 27                  .'
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3A85 20 9E 14                  ..
        cmp     #$00                            ; 3A88 C9 00                    ..
        beq     L3AC5                           ; 3A8A F0 39                    .9
        jsr     getByteFromPage                           ; 3A8C 20 7A 14                  z.
        cmp     #$11                            ; 3A8F C9 11                    ..
        bne     L3A48                           ; 3A91 D0 B5                    ..
        jsr     addRulerSizeToR15                           ; 3A93 20 34 27                  4'
        bcc     L3A48                           ; 3A96 90 B0                    ..
keyUp:
	jsr     L3AAE                           ; 3A98 20 AE 3A                  .:
        bne     L3AA3                           ; 3A9B D0 06                    ..
        jsr     mouseScrollUp                           ; 3A9D 20 C2 37                  .7
        jmp     L3EFF                           ; 3AA0 4C FF 3E                 L.>

; ----------------------------------------------------------------------------
L3AA3:  ldx     cursor0+cursor::srcline                        ; 3AA3 A6 84                    ..
        ldy     lintab_y-1,x                         ; 3AA5 BC 5F 2B                 ._+
L3AA8:  iny                                     ; 3AA8 C8                       .
        sty     mouseYPos                       ; 3AA9 84 3C                    .<
        jmp     L3EFF                           ; 3AAB 4C FF 3E                 L.>

; ----------------------------------------------------------------------------
L3AAE:  jsr     setPromptPosition                           ; 3AAE 20 92 1D                  ..
        bcs     L3AC3                           ; 3AB1 B0 10                    ..
	MoveW   cursor0+cursor::px, mouseXPos                       ; 3AB9 85 3A                    .:
        ldy     cursor0+cursor::py                        ; 3ABB A4 85                    ..
        iny                                     ; 3ABD C8                       .
        sty     mouseYPos                       ; 3ABE 84 3C                    .<
        ldx     cursor0+cursor::srcline                        ; 3AC0 A6 84                    ..
        rts                                     ; 3AC2 60                       `

; ----------------------------------------------------------------------------
L3AC3:  pla                                     ; 3AC3 68                       h
        pla                                     ; 3AC4 68                       h
L3AC5:  rts                                     ; 3AC5 60                       `

; ----------------------------------------------------------------------------
keyDown:
	jsr     L3AAE                           ; 3AC6 20 AE 3A                  .:
        lda     lintab_height+1,x                         ; 3AC9 BD 76 2B                 .v+
        bne     L3AD4                           ; 3ACC D0 06                    ..
        jsr     mouseScrollDown                           ; 3ACE 20 F4 36                  .6
        jmp     L3EFF                           ; 3AD1 4C FF 3E                 L.>

; ----------------------------------------------------------------------------
L3AD4:  ldy     lintab_y+1,x                         ; 3AD4 BC 61 2B                 .a+
        bne     L3AA8                           ; 3AD7 D0 CF                    ..
L3AD9:  jsr     countLines                           ; 3AD9 20 7C 18                  |.
        ldx     curLine                        ; 3ADC A6 D2                    ..
        lda     lintab_y,x                         ; 3ADE BD 60 2B                 .`+
        sta     r1H                             ; 3AE1 85 05                    ..
        lda     #$01                            ; 3AE3 A9 01                    ..
        bne     L3AE9                           ; 3AE5 D0 02                    ..
L3AE7:  lda     #$00                            ; 3AE7 A9 00                    ..
L3AE9:  jsr     L3BE4                           ; 3AE9 20 E4 3B                  .;
        php                                     ; 3AEC 08                       .
        pha                                     ; 3AED 48                       H
        clc                                     ; 3AEE 18                       .
        jsr     L1BAF                           ; 3AEF 20 AF 1B                  ..
        tax                                     ; 3AF2 AA                       .
        pla                                     ; 3AF3 68                       h
        cpx     #$00                            ; 3AF4 E0 00                    ..
        bne     L3B43                           ; 3AF6 D0 4B                    .K
        ldy     cursor3+cursor::ptr+1                        ; 3AF8 A4 9F                    ..
        sty     zp_ADw+1                             ; 3AFA 84 AE                    ..
        ldy     cursor3+cursor::ptr                        ; 3AFC A4 9E                    ..
        sty     zp_ADw                             ; 3AFE 84 AD                    ..
        ldy     curFont+1                             ; 3B00 A4 73                    .s
        sty     tmpFont1+1                             ; 3B02 84 B0                    ..
        ldy     curFont                             ; 3B04 A4 72                    .r
        sty     tmpFont1                             ; 3B06 84 AF                    ..
        ldy     currentMode                     ; 3B08 A4 2E                    ..
        sty     tmpMode1                             ; 3B0A 84 B1                    ..
        tay                                     ; 3B0C A8                       .
        beq     L3B29                           ; 3B0D F0 1A                    ..
        cmp     #$02                            ; 3B0F C9 02                    ..
        beq     L3B2B                           ; 3B11 F0 18                    ..
        jsr     L3B76                           ; 3B13 20 76 3B                  v;
        inc     curLine                        ; 3B16 E6 D2                    ..
        plp                                     ; 3B18 28                       (
        bcc     L3AE7                           ; 3B19 90 CC                    ..
        ldy     #$00                            ; 3B1B A0 00                    ..
        ldx     curLine                        ; 3B1D A6 D2                    ..
        jsr     L3C83                           ; 3B1F 20 83 3C                  .<
        beq     L3B25                           ; 3B22 F0 01                    ..
        iny                                     ; 3B24 C8                       .
L3B25:  tya                                     ; 3B25 98                       .
        bra     L3B44                           ; 3B27 50 1B                    P.

L3B29:  plp                                     ; 3B29 28                       (
L3B2A:  rts                                     ; 3B2A 60                       `

; ----------------------------------------------------------------------------
L3B2B:  plp                                     ; 3B2B 28                       (
        lda     curLine                        ; 3B2C A5 D2                    ..
        pha                                     ; 3B2E 48                       H
        tax                                     ; 3B2F AA                       .
        lda     lintab_y,x                         ; 3B30 BD 60 2B                 .`+
        sta     r1H                             ; 3B33 85 05                    ..
        jsr     setMetricsFromCursor3                           ; 3B35 20 E1 11                  ..
        jsr     L1B5E                           ; 3B38 20 5E 1B                  ^.
        pla                                     ; 3B3B 68                       h
        sta     curLine                        ; 3B3C 85 D2                    ..
        lda     #$FF                            ; 3B3E A9 FF                    ..
        jmp     L15D8                           ; 3B40 4C D8 15                 L..

; ----------------------------------------------------------------------------
L3B43:  plp                                     ; 3B43 28                       (
L3B44:  pha                                     ; 3B44 48                       H
        ldx     curLine                        ; 3B45 A6 D2                    ..
        jsr     L3C90                           ; 3B47 20 90 3C                  .<
        lda     #$00                            ; 3B4A A9 00                    ..
        sta     lintab_height,x                         ; 3B4C 9D 75 2B                 .u+
        cpx     zp_B9b                             ; 3B4F E4 B9                    ..
        beq     L3B71                           ; 3B51 F0 1E                    ..
        pla                                     ; 3B53 68                       h
        bcs     L3B2A                           ; 3B54 B0 D4                    ..
L3B56:  lda     lintab_y,x                         ; 3B56 BD 60 2B                 .`+
        cpx     #$00                            ; 3B59 E0 00                    ..
        beq     L3B6B                           ; 3B5B F0 0E                    ..
        lda     lintab_y-1,x                         ; 3B5D BD 5F 2B                 ._+
        clc                                     ; 3B60 18                       .
        adc     lintab_height-1,x                         ; 3B61 7D 74 2B                 }t+
        bcc     L3B68                           ; 3B64 90 02                    ..
        lda     #$FF                            ; 3B66 A9 FF                    ..
L3B68:  sta     lintab_y,x                         ; 3B68 9D 60 2B                 .`+
L3B6B:  jsr     clearPartOfScreen                           ; 3B6B 20 6D 16                  m.
        jmp     L15FC                           ; 3B6E 4C FC 15                 L..

; ----------------------------------------------------------------------------
L3B71:  pla                                     ; 3B71 68                       h
        beq     L3B2A                           ; 3B72 F0 B6                    ..
        bne     L3B56                           ; 3B74 D0 E0                    ..
L3B76:  lda     r9L                             ; 3B76 A5 14                    ..
        cmp     tmpBaselineOffset                        ; 3B78 C5 A8                    ..
        bne     L3B9F                           ; 3B7A D0 23                    .#
        lda     justification                        ; 3B7C A5 A9                    ..
        and     #MASK_JST_JUST                            ; 3B7E 29 03                    ).
        beq     L3B8E                           ; 3B80 F0 0C                    ..
        cmp     #$03                            ; 3B82 C9 03                    ..
        bne     L3B9F                           ; 3B84 D0 19                    ..
        lda     r9H                             ; 3B86 A5 15                    ..
        and     justification                        ; 3B88 25 A9                    %.
        and     #JST_20                            ; 3B8A 29 20                    )
        beq     L3B9F                           ; 3B8C F0 11                    ..
L3B8E:  lda     cursor3+cursor::ptr+1                        ; 3B8E A5 9F                    ..
        cmp     a6H                             ; 3B90 C5 79                    .y
        bne     L3B98                           ; 3B92 D0 04                    ..
        lda     cursor3+cursor::ptr                        ; 3B94 A5 9E                    ..
        cmp     a6L                             ; 3B96 C5 78                    .x
L3B98:  bcc     L3B9F                           ; 3B98 90 05                    ..
        jsr     L3C9B                           ; 3B9A 20 9B 3C                  .<
        bcc     L3BA5                           ; 3B9D 90 06                    ..
L3B9F:  lda     #0                            ; 3B9F A9 00                    ..
        sta     a6L                             ; 3BA1 85 78                    .x
        sta     a6H                             ; 3BA3 85 79                    .y
L3BA5:  jsr     L3BB8                           ; 3BA5 20 B8 3B                  .;
        ldx     curLine                        ; 3BA8 A6 D2                    ..
        jsr     justifyText2                           ; 3BAA 20 18 17                  ..
        lda     zp_ADw+1                             ; 3BAD A5 AE                    ..
        sta     cursor3+cursor::ptr+1                        ; 3BAF 85 9F                    ..
        lda     zp_ADw                             ; 3BB1 A5 AD                    ..
        sta     cursor3+cursor::ptr                        ; 3BB3 85 9E                    ..
        jmp     L1784                           ; 3BB5 4C 84 17                 L..

; ----------------------------------------------------------------------------
L3BB8:  ldx     curLine                        ; 3BB8 A6 D2                    ..
        lda     lintab_txtPtr_lo,x                         ; 3BBA BD 36 2B                 .6+
        cmp     a6L                             ; 3BBD C5 78                    .x
        bne     L3BC8                           ; 3BBF D0 07                    ..
        lda     lintab_txtPtr_hi,x                         ; 3BC1 BD 4B 2B                 .K+
        cmp     a6H                             ; 3BC4 C5 79                    .y
        beq     L3BCE                           ; 3BC6 F0 06                    ..
L3BC8:  lda     a6L                             ; 3BC8 A5 78                    .x
        ora     a6H                             ; 3BCA 05 79                    .y
        bne     L3BE3                           ; 3BCC D0 15                    ..
L3BCE:  lda     lintab_y,x                         ; 3BCE BD 60 2B                 .`+
        sta     r2L                             ; 3BD1 85 06                    ..
        clc                                     ; 3BD3 18                       .
        adc     lintab_height,x                         ; 3BD4 7D 75 2B                 }u+
        sta     r2H                             ; 3BD7 85 07                    ..
        dec     r2H                             ; 3BD9 C6 07                    ..
        jsr     loadWR3Zero                           ; 3BDB 20 52 27                  R'
        lda     #0			; white
        jsr     rectangleUntilRightBorder                           ; 3BE0 20 4C 18                  L.
L3BE3:  rts                                     ; 3BE3 60                       `

; ----------------------------------------------------------------------------
L3BE4:  pha                                     ; 3BE4 48                       H
        ldx     curLine                        ; 3BE5 A6 D2                    ..
        cmp     #$00                            ; 3BE7 C9 00                    ..
        bne     L3C1B                           ; 3BE9 D0 30                    .0
        jsr     L3C83                           ; 3BEB 20 83 3C                  .<
        bne     L3C05                           ; 3BEE D0 15                    ..
        lda     lintab_font_lo,x                         ; 3BF0 BD 8A 2B                 ..+
        cmp     tmpFont1                             ; 3BF3 C5 AF                    ..
        bne     L3C05                           ; 3BF5 D0 0E                    ..
        lda     lintab_font_hi,x                         ; 3BF7 BD 9F 2B                 ..+
        cmp     tmpFont1+1                             ; 3BFA C5 B0                    ..
        bne     L3C05                           ; 3BFC D0 07                    ..
        lda     lintab_mode,x                         ; 3BFE BD B4 2B                 ..+
        cmp     tmpMode1                             ; 3C01 C5 B1                    ..
        beq     L3C1B                           ; 3C03 F0 16                    ..
L3C05:  pla                                     ; 3C05 68                       h
        lda     #$01                            ; 3C06 A9 01                    ..
        pha                                     ; 3C08 48                       H
        jsr     L3C90                           ; 3C09 20 90 3C                  .<
        lda     tmpFont1                             ; 3C0C A5 AF                    ..
        sta     lintab_font_lo,x                         ; 3C0E 9D 8A 2B                 ..+
        lda     tmpFont1+1                             ; 3C11 A5 B0                    ..
        sta     lintab_font_hi,x                         ; 3C13 9D 9F 2B                 ..+
        lda     tmpMode1                             ; 3C16 A5 B1                    ..
        sta     lintab_mode,x                         ; 3C18 9D B4 2B                 ..+
L3C1B:  jsr     moveLintabR15                           ; 3C1B 20 71 18                  q.
        jsr     readLintab                           ; 3C1E 20 55 18                  U.
        jsr     getJustification_XXX                           ; 3C21 20 98 1B                  ..
        jsr     measureLine                           ; 3C24 20 7A 10                  z.
        pla                                     ; 3C27 68                       h
        php                                     ; 3C28 08                       .
        pha                                     ; 3C29 48                       H
        ldx     curLine                        ; 3C2A A6 D2                    ..
        lda     lintab_height,x                         ; 3C2C BD 75 2B                 .u+
        cmp     cursor3+cursor::height                        ; 3C2F C5 A4                    ..
        bne     L3C6F                           ; 3C31 D0 3C                    .<
        pla                                     ; 3C33 68                       h
        bne     L3C6B                           ; 3C34 D0 35                    .5
        lda     lintab_justification,x                         ; 3C36 BD DE 2B                 ..+
        cmp     justification                        ; 3C39 C5 A9                    ..
        bne     L3C6B                           ; 3C3B D0 2E                    ..
        lda     lintab_baselineOffset,x                         ; 3C3D BD C9 2B                 ..+
        cmp     tmpBaselineOffset                        ; 3C40 C5 A8                    ..
        bne     L3C6B                           ; 3C42 D0 27                    .'
        lda     lintab_txtPtr_lo+1,x                         ; 3C44 BD 37 2B                 .7+
        cmp     cursor3+cursor::ptr                        ; 3C47 C5 9E                    ..
        bne     L3C6B                           ; 3C49 D0 20                    . 
        lda     lintab_txtPtr_hi+1,x                         ; 3C4B BD 4C 2B                 .L+
        cmp     cursor3+cursor::ptr+1                        ; 3C4E C5 9F                    ..
        bne     L3C6B                           ; 3C50 D0 19                    ..
        lda     lintab_font_lo,x                         ; 3C52 BD 8A 2B                 ..+
        cmp     cursor3+cursor::font                        ; 3C55 C5 A5                    ..
        bne     L3C6B                           ; 3C57 D0 12                    ..
        lda     lintab_font_hi,x                         ; 3C59 BD 9F 2B                 ..+
        cmp     cursor3+cursor::font+1                        ; 3C5C C5 A6                    ..
        bne     L3C6B                           ; 3C5E D0 0B                    ..
        lda     lintab_mode,x                         ; 3C60 BD B4 2B                 ..+
        cmp     cursor3+cursor::mode                        ; 3C63 C5 A7                    ..
        bne     L3C6B                           ; 3C65 D0 04                    ..
        lda     #$00                            ; 3C67 A9 00                    ..
        beq     L3C72                           ; 3C69 F0 07                    ..
L3C6B:  lda     #$01                            ; 3C6B A9 01                    ..
        bne     L3C72                           ; 3C6D D0 03                    ..
L3C6F:  pla                                     ; 3C6F 68                       h
        lda     #$02                            ; 3C70 A9 02                    ..
L3C72:  pha                                     ; 3C72 48                       H
        lda     lintab_baselineOffset,x                         ; 3C73 BD C9 2B                 ..+
        sta     r9L                             ; 3C76 85 14                    ..
        lda     lintab_justification,x                         ; 3C78 BD DE 2B                 ..+
        sta     r9H                             ; 3C7B 85 15                    ..
        jsr     moveCursor3Lintab                           ; 3C7D 20 0D 1C                  ..
        pla                                     ; 3C80 68                       h
        plp                                     ; 3C81 28                       (
        rts                                     ; 3C82 60                       `

; ----------------------------------------------------------------------------
L3C83:  lda     lintab_txtPtr_lo,x                         ; 3C83 BD 36 2B                 .6+
        cmp     zp_ADw                             ; 3C86 C5 AD                    ..
        bne     L3C8F                           ; 3C88 D0 05                    ..
        lda     lintab_txtPtr_hi,x                         ; 3C8A BD 4B 2B                 .K+
        cmp     zp_ADw+1                             ; 3C8D C5 AE                    ..
L3C8F:  rts                                     ; 3C8F 60                       `

; ----------------------------------------------------------------------------
L3C90:  lda     zp_ADw                             ; 3C90 A5 AD                    ..
        sta     lintab_txtPtr_lo,x                         ; 3C92 9D 36 2B                 .6+
        lda     zp_ADw+1                             ; 3C95 A5 AE                    ..
        sta     lintab_txtPtr_hi,x                         ; 3C97 9D 4B 2B                 .K+
        rts                                     ; 3C9A 60                       `

; ----------------------------------------------------------------------------
L3C9B:  ldx     #14                             ; 3C9B A2 0E                    ..
@loop:  lda     rulerData1+ruler::tabs+1,x                         ; 3C9D BD 73 2F                 .s/
        bmi     L3CA8                           ; 3CA0 30 06                    0.
        dex                                     ; 3CA2 CA                       .
        dex                                     ; 3CA3 CA                       .
        bpl     @loop                           ; 3CA4 10 F7                    ..
        clc                                     ; 3CA6 18                       .
        rts                                     ; 3CA7 60                       `

; ----------------------------------------------------------------------------
L3CA8:  jsr     pushR15                         ; 3CA8 20 6C 28                  l(
L3CAB:  jsr     CmpR15Cursor3Ptr                           ; 3CAB 20 2A 28                  *(
        bcs     L3CBC                           ; 3CAE B0 0C                    ..
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics
        cmp     #TAB                            ; 3CB3 C9 09                    ..
        bne     L3CAB                           ; 3CB5 D0 F4                    ..
        jsr     popR15                          ; 3CB7 20 7F 28                  .(
        sec                                     ; 3CBA 38                       8
        rts                                     ; 3CBB 60                       `

; ----------------------------------------------------------------------------
L3CBC:  jsr     popR15                          ; 3CBC 20 7F 28                  .(
        clc                                     ; 3CBF 18                       .
        rts                                     ; 3CC0 60                       `

; ----------------------------------------------------------------------------
L3CC1:  jsr     resetToPageStart                           ; 3CC1 20 B1 26                  .&
L3CC4:  jsr     measureLine                           ; 3CC4 20 7A 10                  z.
        bcs     L3CDB                           ; 3CC7 B0 12                    ..
	CmpW    cursor0+cursor::ptr, cursor3+cursor::ptr
	bcc     L3CDB                           ; 3CD3 90 06                    ..
        jsr     L27BC                           ; 3CD5 20 BC 27                  .'
        bra     L3CC4                           ; 3CD9 50 E9                    P.

L3CDB:  SubVW2_	$3C, pageTextHeight, pagePosY
        bcs     L3CF0                           ; 3CE8 B0 06                    ..
	LoadW	pagePosY, 0
L3CF0:  jsr     L3D98                           ; 3CF0 20 98 3D                  .=
        sta     sideFlippingOffset                           ; 3CF3 8D BC 2F                 ../
        jmp     L065D                           ; 3CF6 4C 5D 06                 L].

; ----------------------------------------------------------------------------
L3CF9:  txa                                     ; 3CF9 8A                       .
        bpl     @1                           ; 3CFA 10 0F                    ..
        jsr     resetToPageStart                           ; 3CFC 20 B1 26                  .&
        lda     a4L                             ; 3CFF A5 74                    .t
        ora     #A4L_80                            ; 3D01 09 80                    ..
        sta     a4L                             ; 3D03 85 74                    .t
        lda     #0                            ; 3D05 A9 00                    ..
        sta     r1H                             ; 3D07 85 05                    ..
        beq     @2                           ; 3D09 F0 0E                    ..

@1:	lda     lintab_y,x                         ; 3D0B BD 60 2B                 .`+
        sta     r1H                             ; 3D0E 85 05                    ..
        jsr     moveLintabR15                           ; 3D10 20 71 18                  q.
        jsr     readLintab                           ; 3D13 20 55 18                  U.
        jsr     getJustification_XXX                           ; 3D16 20 98 1B                  ..
@2:	lda     #0                            ; 3D19 A9 00                    ..
        sta     a9H                             ; 3D1B 85 7F                    ..

L3D1D:  jsr     measureLine                           ; 3D1D 20 7A 10                  z.
        php                                     ; 3D20 08                       .
        lda     r1L                             ; 3D21 A5 04                    ..
        sta     zp_BAb                       ; 3D23 85 BA                    ..
        lda     r1H                             ; 3D25 A5 05                    ..
        beq     L3D59                           ; 3D27 F0 30                    .0
        sec                                     ; 3D29 38                       8
        jsr     L1BAF                           ; 3D2A 20 AF 1B                  ..
        sta     r4L                             ; 3D2D 85 0A                    ..
        bit     r4L                             ; 3D2F 24 0A                    $.
        bpl     L3D36                           ; 3D31 10 03                    ..
        plp                                     ; 3D33 28                       (
        sec                                     ; 3D34 38                       8
        rts                                     ; 3D35 60                       `

L3D36:  bvc     L3D59                           ; 3D36 50 21                    P!
L3D38:  lda     #mouseYPos                            ; 3D38 A9 3C                    .<
        ldx     #118                            ; 3D3A A2 76                    .v
        jsr     addWIToZp                           ; 3D3C 20 3E 27                  >'
        lda     r2L                             ; 3D3F A5 06                    ..
        sec                                     ; 3D41 38                       8
        sbc     #60                            ; 3D42 E9 3C                    .<
        sta     r2L                             ; 3D44 85 06                    ..
        bcs     L3D4A                           ; 3D46 B0 02                    ..
        dec     r2H                             ; 3D48 C6 07                    ..
L3D4A:  jsr     CmpR2WindowBottom                           ; 3D4A 20 56 28                  V(
        beq     L3D51                           ; 3D4D F0 02                    ..
        bcs     L3D38                           ; 3D4F B0 E7                    ..
L3D51:  lda     r2L                             ; 3D51 A5 06                    ..
        sta     r1H                             ; 3D53 85 05                    ..
        lda     #$FF                            ; 3D55 A9 FF                    ..
        sta     a9H                             ; 3D57 85 7F                    ..
L3D59:  CmpW    cursor3+cursor::ptr, cursor0+cursor::ptr                           ; 3D61 C5 80                    ..
	beq     L3D8A                           ; 3D63 F0 25                    .%
        bcs     L3D90                           ; 3D65 B0 29                    .)
L3D67:  plp                                     ; 3D67 28                       (
        bcs     L3D70                           ; 3D68 B0 06                    ..
        jsr     moveCursor3Ptr_r15                           ; 3D6A 20 BF 27                  .'
        bra     L3D1D                           ; 3D6E 50 AD                    P.

L3D70:  lda     cursor3+cursor::ptr+1                        ; 3D70 A5 9F                    ..
        cmp     pageEndPtr2+1                        ; 3D72 C5 D1                    ..
        bne     L3D7A                           ; 3D74 D0 04                    ..
        lda     cursor3+cursor::ptr                        ; 3D76 A5 9E                    ..
        cmp     pageEndPtr2                        ; 3D78 C5 D0                    ..
L3D7A:  bne     L3D88                           ; 3D7A D0 0C                    ..
        CmpW    cursor0+cursor::ptr, pageEndPtr2                        ; 3D84 C5 D0                    ..
	beq     L3D91                           ; 3D86 F0 09                    ..
L3D88:  sec                                     ; 3D88 38                       8
        rts                                     ; 3D89 60                       `

; ----------------------------------------------------------------------------
L3D8A:  lda     zp_BAb                       ; 3D8A A5 BA                    ..
        cmp     #$0D                            ; 3D8C C9 0D                    ..
        beq     L3D67                           ; 3D8E F0 D7                    ..
L3D90:  plp                                     ; 3D90 28                       (
L3D91:  jsr     L3D98                           ; 3D91 20 98 3D                  .=
        ldx     a9H                             ; 3D94 A6 7F                    ..
        clc                                     ; 3D96 18                       .
        rts                                     ; 3D97 60                       `

; ----------------------------------------------------------------------------
L3D98:  MoveW   cursor0+cursor::ptr, cursor3+cursor::ptr                        ; 3D9E 85 9E                    ..
        jsr     setMetricsFromCursor3                           ; 3DA0 20 E1 11                  ..
        jsr     justifyText                           ; 3DA3 20 1B 17                  ..
        jsr     L0927                           ; 3DA6 20 27 09                  '.
L3DA9:  lda     sideFlippingOffset                           ; 3DA9 AD BC 2F                 ../
        asl     a                               ; 3DAC 0A                       .
        asl     a                               ; 3DAD 0A                       .
        tay                                     ; 3DAE A8                       .
        dey                                     ; 3DAF 88                       .
L3DB0:  iny                                     ; 3DB0 C8                       .
	SubVW	160, r11
        bcs     L3DB0                           ; 3DBE B0 F0                    ..
        lda     L3DC4,y                         ; 3DC0 B9 C4 3D                 ..=
        rts                                     ; 3DC3 60                       `
L3DC4:  .byte   0,0,1,2,0,1,1,2,0,1,2,2

; ----------------------------------------------------------------------------
selectionProcess:
	jsr     pushRulerData
        jsr     L3F1D
        jsr     popRulerData
        jsr     loadFontMetrics
        jsr     CmpCursor2_3Ptr
        beq     rts1
        jsr     invertSelection2
        jsr     L28A9
        jsr     sortCursor0_1Ptr
        jsr     copyCursor3To2
        jsr     cmpCursor0_1Ptr		; is there a selection
        bne     L3DFA			; yes

        lda     #$FF
        sta     setFontMarkFlag
        sta     setStyleCheckmarkFlag
        bne     L3E00

L3DFA:  jsr     killPrompt		; no cursor if there is a selection
        jsr     analyzeSelectionFont

L3E00:  jmp     setPromptFontMetricsUpdateRulerUI

; ----------------------------------------------------------------------------
; make sure cursor[0].ptr <= cursor[1].ptr
sortCursor0_1Ptr:
	jsr     cmpCursor0_1Ptr
        bcc     rts1
        beq     rts1
        ldx     #9
L3E0C:  lda     cursor0+cursor::ptr,x
        tay
        lda     cursor1+cursor::ptr,x
        sta     cursor0+cursor::ptr,x
        tya
        sta     cursor1+cursor::ptr,x
        dex
        bpl     L3E0C
rts1:	rts

; ----------------------------------------------------------------------------
process2:
	MoveW	r1, curFont
        jsr     cmpCursor0_1Ptr		; no selection, return
        beq     rts1
        MoveB   cursor0+cursor::mode, currentMode
        lda     #$FF
        bne     L3E39			; always

process2b:
	jsr     cmpCursor0_1Ptr
        beq     rts1			; no selection, return
        jsr     moveCursor0FontCurFont
        lda     #0

L3E39:  sta     a9L                             ; 3E39 85 7E                    .~
        jsr     setDirty                           ; 3E3B 20 36 1E                  6.
        jsr     invertSelectionIfNeeded                           ; 3E3E 20 6E 08                  n.
        jsr     L368B                           ; 3E41 20 8B 36                  .6
        jsr     L36B1                           ; 3E44 20 B1 36                  .6
        ldx     #A4L_08                            ; 3E47 A2 08                    ..
        lda     cursor0+cursor::srcline                        ; 3E49 A5 84                    ..
        cmp     #$40                            ; 3E4B C9 40                    .@
        bcc     L3E53                           ; 3E4D 90 04                    ..
        ldx     #A4L_20                            ; 3E4F A2 20                    .
        lda     #$FF                            ; 3E51 A9 FF                    ..
L3E53:  sta     a4H                             ; 3E53 85 75                    .u
        stx     a4L                             ; 3E55 86 74                    .t
        jsr     render_XXX                           ; 3E57 20 1D 33                  .3

analyzeSelectionFont:
	jsr     cmpCursor0_1Ptr
        beq     @rts			; no selection, return

        jsr     moveCursor0Ptr_r15
        ldy     #0
        lda     (r15),y
        cmp     #NEWCARDSET
        beq     @1

        jsr     moveCursor0FontCurFont	; load current font and style
        lda     cursor0+cursor::mode
        sta     currentMode
        bra     @2

@1:	iny
        lda     (r15),y
        sta     curFont			; read font into cursor1
        sta     cursor1+cursor::font
        iny
        lda     (r15),y
        sta     curFont+1
        sta     cursor1+cursor::font+1
        iny
        lda     (r15),y
        sta     currentMode		; read mode into cursor1
        sta     cursor1+cursor::mode
        jsr     addWI4R15		; and skyp bytes

@2:	lda     #$FF
        sta     setFontMarkFlag
        sta     setStyleCheckmarkFlag
        ldx     #0
@loop:  ldy     #0
        lda     (r15),y
        cmp     #NEWCARDSET
        bne     @3

        iny
        lda     (r15),y
        sta     cursor1+cursor::font
        cmp     curFont
        beq     :+
        stx     setFontMarkFlag		; font is different
:	iny
        lda     (r15),y
        sta     cursor1+cursor::font+1
        cmp     curFont+1
        beq     :+
        stx     setFontMarkFlag		; font is different
:	iny
        lda     (r15),y
        sta     cursor1+cursor::mode
        cmp     currentMode
        beq     :+
        stx     setStyleCheckmarkFlag	; mode is different
        stx     currentMode
:	jsr     addWI3R15
@3:	jsr     incWR15
        jsr     CmpR15Cursor1Ptr
        bne     @loop

        bit     setFontMarkFlag
        bmi     @rts
	LoadW   curFont, SYSTEM_FONT_ID
@rts:	rts

; ----------------------------------------------------------------------------
appOtherPressVec:
	lda     mouseData                       ; 3ED7 AD 05 85                 ...
        bmi     blockSelectionProcess                           ; 3EDA 30 38                    08
        jsr     clearSelection                           ; 3EDC 20 5C 08                  \.
        jsr     killPrompt                      ; 3EDF 20 6E 1D                  n.
        lda     dblClickCount                   ; 3EE2 AD 15 85                 ...
        beq     L3EF1                           ; 3EE5 F0 0A                    ..
        jsr     L3FFB                           ; 3EE7 20 FB 3F                  .?
        lda     #$00                            ; 3EEA A9 00                    ..
        sta     dblClickCount                   ; 3EEC 8D 15 85                 ...
        beq     blockSelectionProcess                           ; 3EEF F0 23                    .#
L3EF1:  lda     #$1E                            ; 3EF1 A9 1E                    ..
        sta     dblClickCount                   ; 3EF3 8D 15 85                 ...
        ldx     #PROCESS_SELECTION                            ; 3EF6 A2 00                    ..
        jsr     RestartProcess                  ; 3EF8 20 06 C1                  ..
        LoadB   zp_D9b, $FF
L3EFF:  lda     #$FF                            ; 3EFF A9 FF                    ..
        sta     setFontMarkFlag                        ; 3F01 85 CE                    ..
        sta     setStyleCheckmarkFlag                        ; 3F03 85 CF                    ..
        jsr     L3F1D                           ; 3F05 20 1D 3F                  .?
        jsr     copyCursor3To0                           ; 3F08 20 CC 28                  .(
        jsr     copyCursor0To1                           ; 3F0B 20 C0 28                  .(
        jsr     setPromptFontMetricsUpdateRulerUI                           ; 3F0E 20 7A 1D                  z.
        jmp     copyCursor3To2                           ; 3F11 4C C6 28                 L.(

; ----------------------------------------------------------------------------
blockSelectionProcess:
	lda     #0                            ; 3F14 A9 00                    ..
        sta     zp_D9b                        ; 3F16 85 D9                    ..
        ldx     #PROCESS_SELECTION                            ; 3F18 A2 00                    ..
        jmp     BlockProcess                    ; 3F1A 4C 0C C1                 L..

; ----------------------------------------------------------------------------
L3F1D:  jsr     getTextLineUnderMouse
        stx     r3L
        jsr     getLineRuler
        ldx     r3L
        jsr     justifyText2
        lda     justification
        bpl     L3F39			; JST_80

        jsr     moveR15_cursor3Ptr
	LoadW_	r11, 0
        beq     L3F3C

L3F39:  jsr     L3F45

L3F3C:  MoveW   r11, cursor3+cursor::px
        rts

; ----------------------------------------------------------------------------
L3F45:  ldx     #cursor3+cursor::px                            ; 3F45 A2 A0                    ..
        jsr     addSideFlippingPixelOffset                           ; 3F47 20 E5 26                  .&
L3F4A:  MoveW	r15, a7                         ; 3F50 85 7A                    .z
        jsr     L0902                           ; 3F52 20 02 09                  ..
        jsr     L14BD                           ; 3F55 20 BD 14                  ..
        bcc     L3F64                           ; 3F58 90 0A                    ..

        cmp     #CR                            ; 3F5A C9 0D                    ..
        beq     L3FAD                           ; 3F5C F0 4F                    .O
        cmp     #ESC_GRAPHICS                            ; 3F5E C9 10                    ..
        beq     L3FAD                           ; 3F60 F0 4B                    .K
        bne     L3F9F                           ; 3F62 D0 3B                    .;

L3F64:  CmpW    r2, cursor3+cursor::px
	bcs     L3F76                           ; 3F6E B0 06                    ..
        jsr     moveWR2R11                           ; 3F70 20 C8 27                  .'
        bra     L3F4A                           ; 3F74 50 D4                    P.

L3F76:  CmpW    cursor3+cursor::px, r11L                            ; 3F7E C5 18                    ..
	bcc     L3FAD                           ; 3F80 90 2B                    .+
	SubW3   r2, cursor3+cursor::px, r3
        asl     r3L                             ; 3F8F 06 08                    ..
        rol     r3H                             ; 3F91 26 09                    &.
        lda     r3H                             ; 3F93 A5 09                    ..
        cmp     r10H                            ; 3F95 C5 17                    ..
        bne     L3F9D                           ; 3F97 D0 04                    ..
        lda     r3L                             ; 3F99 A5 08                    ..
        cmp     r10L                            ; 3F9B C5 16                    ..
L3F9D:  bcs     L3FAD                           ; 3F9D B0 0E                    ..
L3F9F:  jsr     moveWR2R11                           ; 3F9F 20 C8 27                  .'
        jsr     L0902                           ; 3FA2 20 02 09                  ..
        lda     r15H                            ; 3FA5 A5 21                    .!
        sta     a7H                             ; 3FA7 85 7B                    .{
        lda     r15L                            ; 3FA9 A5 20                    . 
        sta     a7L                             ; 3FAB 85 7A                    .z
L3FAD:  lda     a7H                             ; 3FAD A5 7B                    .{
        sta     cursor3+cursor::ptr+1                        ; 3FAF 85 9F                    ..
        lda     a7L                             ; 3FB1 A5 7A                    .z
        sta     cursor3+cursor::ptr                        ; 3FB3 85 9E                    ..
        jsr     screenXFromPageX                           ; 3FB5 20 0F 09                  ..
        ldy     #0                            ; 3FB8 A0 00                    ..
        lda     (r15),y                        ; 3FBA B1 20                    . 
        cmp     #ESC_RULER                            ; 3FBC C9 11                    ..
        bne     rts2                           ; 3FBE D0 32                    .2
        jmp     addRulerSizeToR15                           ; 3FC0 4C 34 27                 L4'

; ----------------------------------------------------------------------------
getTextLineUnderMouse:
	MoveW   mouseXPos, cursor3+cursor::px

        ldx     #0
        lda     mouseYPos
@loop:	cmp     lintab_y,x			; compare mouse with Y coordinate of top of line
        bcc     @1			; it's below
        inx
        ldy     lintab_height,x
        bne     @loop			; until entry with 0 height (terminator)

        lda     lintab_y-1,x
        clc
        adc     lintab_height-1,x		; bottom y of last line on screen
        cmp     mouseYPos
        bcs     @1			; yes, mouse is within

	LoadW   cursor3+cursor::px, 999	; = below all lines

@1:	dex
        bmi     @above			; mouse was above text area

@end:	stx     cursor3+cursor::srcline	; return line number
@rts2:	rts

@above:	ldx     #0			; = above all lines
        stx     cursor3+cursor::px
        stx     cursor3+cursor::px+1
        beq     @end			; always

rts2 = @rts2

; ----------------------------------------------------------------------------
L3FFB:  bit     cursor0+cursor::srcline                        ; 3FFB 24 84                    $.
        bmi     L404F                           ; 3FFD 30 50                    0P
        bvs     L404F                           ; 3FFF 70 4E                    pN
        jsr     moveCursor0Ptr_r15                           ; 4001 20 A1 27                  .'
L4004:  MoveW   r15, cursor1+cursor::ptr                        ; 400A 85 8A                    ..
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 400C 20 9E 14                  ..
        jsr     isAlphanumeric                           ; 400F 20 7F 1E                  ..
        bcs     L4004                           ; 4012 B0 F0                    ..
        ldx     cursor0+cursor::srcline                        ; 4014 A6 84                    ..
        jsr     moveLintabR15                           ; 4016 20 71 18                  q.
        lda     lintab_justification,x                         ; 4019 BD DE 2B                 ..+
        and     #JST_STARTS_WITH_RULER                            ; 401C 29 40                    )@
        beq     L4023                           ; 401E F0 03                    ..
        jsr     addRulerSizeToR15                           ; 4020 20 34 27                  4'
L4023:  jsr     moveWR15R14                           ; 4023 20 D1 27                  .'
L4026:  jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 4026 20 9E 14                  ..
        jsr     isAlphanumeric                           ; 4029 20 7F 1E                  ..
        bcs     L4031                           ; 402C B0 03                    ..
        jsr     moveWR15R14                           ; 402E 20 D1 27                  .'
L4031:  jsr     cmpR15Cursor0Ptr                           ; 4031 20 F3 27                  .'
        bcc     L4026                           ; 4034 90 F0                    ..
	MoveW   r14L, cursor0+cursor::ptr
	jsr 	cmpCursor0_1Ptr
        bcs     L4049                           ; 4041 B0 06                    ..
        jsr     L0781                           ; 4043 20 81 07                  ..
        jmp     analyzeSelectionFont                           ; 4046 4C 5A 3E                 LZ>

; ----------------------------------------------------------------------------
L4049:  jsr     copyCursor0To1                           ; 4049 20 C0 28                  .(
        jsr     L0781                           ; 404C 20 81 07                  ..
L404F:  rts                                     ; 404F 60                       `

; ----------------------------------------------------------------------------
selectPage:
	jsr     GotoFirstMenu                   ; 4050 20 BD C1                  ..
        jsr     cmpPageEndPtr2PageCardSet                           ; 4053 20 09 28                  .(
        beq     L404F                           ; 4056 F0 F7                    ..
        jsr     clearSelection                           ; 4058 20 5C 08                  \.
        jsr     killPrompt                      ; 405B 20 6E 1D                  n.
        jsr     loadCursor0PtrPageCardset                           ; 405E 20 76 27                  v'
        MoveW   pageEndPtr2, cursor1+cursor::ptr
        bit     nextPageOpen                        ; 4069 24 DE                    $.
        bpl     L4078                           ; 406B 10 0B                    ..
        jsr     measurePage                           ; 406D 20 9F 07                  ..
	MoveW   pageEndPtr1, cursor1+cursor::ptr                        ; 4076 85 8A                    ..
L4078:  jsr     i_MoveData                      ; 4078 20 B7 C1                  ..
		.word	MEM_PAGE+page::ruler
		.word	rulerData1
		.word	.sizeof(ruler)
        jsr     L0781                           ; 4081 20 81 07                  ..
        jmp     analyzeSelectionFont                           ; 4084 4C 5A 3E                 LZ>

; ----------------------------------------------------------------------------
callbackJustification:
	ldx     #<jst_checkbox_x_count-1
@loop:  lda     mouseXPos
        sec
        sbc     jst_checkbox_x_lo,x	; get distance to justification buffon
        sta     r15L
        lda     mouseXPos+1
        sbc     jst_checkbox_x_hi,x
        sta     r15H
        jsr     addWI4R15
        lda     r15H
        bne     @no
        lda     r15L
        cmp     #10			; within 10 px of the target?
        bcc     L40AB
@no:	dex
        bpl     @loop
        rts

callbackRuler:
	tya
        tax
L40AB:  cpx     #4
        bcc     @just

        txa				; spacing
        sec
        sbc     #4
        asl     a
        asl     a
        sta     r0L
        lda     rulerData1+ruler::justification
        and     #JST_10+MASK_JST_JUST
        ora     r0L
        sta     rulerData1+ruler::justification
        lda     #A4L_01
        sta     a4L
        bne     L4100

@just:  txa				; justification
        sta     r0L
        lda     rulerData1+ruler::justification
        and     #JST_10+MASK_JST_SPACING
        ora     r0L
        sta     rulerData1+ruler::justification
        lda     #0
        sta     a4L
        jmp     L4100

; ----------------------------------------------------------------------------
.define funcPtrs L40DF,L40EE
funcPtrs_lo:
	.lobytes funcPtrs
funcPtrs_hi:
	.hibytes funcPtrs



; ----------------------------------------------------------------------------
L40DF:	ldy     #$17                            ; 40DF A0 17                    ..
        lda     (r15),y                        ; 40E1 B1 20                    . 
        and     #$FC                            ; 40E3 29 FC                    ).
        sta     (r15),y                        ; 40E5 91 20                    . 
        lda     rulerData1+ruler::justification
        and     #MASK_JST_JUST                            ; 40EA 29 03                    ).
        bpl     L40FB			; always

L40EE:	ldy     #$17                            ; 40EE A0 17                    ..
        lda     (r15),y                        ; 40F0 B1 20                    . 
        and     #$F3                            ; 40F2 29 F3                    ).
        sta     (r15),y                        ; 40F4 91 20                    . 
        lda     rulerData1+ruler::justification
        and     #MASK_JST_SPACING                            ; 40F9 29 0C                    ).
L40FB:  ora     (r15),y                        ; 40FB 11 20                    . 
        sta     (r15),y                        ; 40FD 91 20                    . 
        rts                                     ; 40FF 60                       `

; ----------------------------------------------------------------------------
L4100:  jsr     pushRulerData                           ; 4100 20 73 26                  s&
        jsr     setDirty                           ; 4103 20 36 1E                  6.
        jsr     killPrompt                      ; 4106 20 6E 1D                  n.
        jsr     L41B0                           ; 4109 20 B0 41                  .A
        jsr     L4188                           ; 410C 20 88 41                  .A
        jsr     L416B                           ; 410F 20 6B 41                  kA
        jsr     popRulerData                           ; 4112 20 93 26                  .&
        jsr     pushRulerData                           ; 4115 20 73 26                  s&
        jsr     moveCursor3Ptr_r15                           ; 4118 20 BF 27                  .'
        ldy     #$00                            ; 411B A0 00                    ..
        lda     (r15),y                        ; 411D B1 20                    . 
        cmp     #$11                            ; 411F C9 11                    ..
        beq     L4152                           ; 4121 F0 2F                    ./
        jsr     cmpR15Cursor0Ptr                           ; 4123 20 F3 27                  .'
        beq     L412A                           ; 4126 F0 02                    ..
        bcs     L412F                           ; 4128 B0 05                    ..
L412A:  ldx     #cursor0+cursor::ptr                            ; 412A A2 80                    ..
        jsr     addRulerSizeToZp                           ; 412C 20 36 27                  6'
L412F:  jsr     CmpR15Cursor1Ptr                           ; 412F 20 1F 28                  .(
        beq     L4136                           ; 4132 F0 02                    ..
        bcs     L413B                           ; 4134 B0 05                    ..
L4136:  ldx     #cursor1+cursor::ptr                            ; 4136 A2 8A                    ..
        jsr     addRulerSizeToZp                           ; 4138 20 36 27                  6'
L413B:  jsr     makeSpaceForRuler                           ; 413B 20 97 1C                  ..
        lda     rulerData1+ruler::justification
        and     #JST_10+MASK_JST_SPACING+MASK_JST_JUST
        sta     rulerData1+ruler::justification
        ldy     #$00                            ; 4146 A0 00                    ..
        lda     #$11                            ; 4148 A9 11                    ..
        sta     (r15),y                        ; 414A 91 20                    . 
        jsr     L415E                           ; 414C 20 5E 41                  ^A
        bra     L4155                           ; 4150 50 03                    P.

L4152:  jsr     L41A5                           ; 4152 20 A5 41                  .A
L4155:  jsr     L065D                           ; 4155 20 5D 06                  ].
        jsr     popRulerData                           ; 4158 20 93 26                  .&
        jmp     L0781                           ; 415B 4C 81 07                 L..

; ----------------------------------------------------------------------------
L415E:  ldy     #1                            ; 415E A0 01                    ..
@loop:  lda     rulerData1-1,y                         ; 4160 B9 6D 2F                 .m/
        sta     (r15),y                        ; 4163 91 20                    . 
        iny                                     ; 4165 C8                       .
        cpy     #.sizeof(ruler)+1                            ; 4166 C0 1B                    ..
        bne     @loop                           ; 4168 D0 F6                    ..
        rts                                     ; 416A 60                       `

; ----------------------------------------------------------------------------
L416B:  jsr     LoadR15_MEM_PAGE                           ; 416B 20 BF 26                  .&
        jsr     moveR15_cursor3Ptr                           ; 416E 20 B3 27                  .'
L4171:  jsr     getByteIntpNewCardSet                           ; 4171 20 5A 14                  Z.
        tax                                     ; 4174 AA                       .
        jsr     cmpR15Cursor0Ptr                           ; 4175 20 F3 27                  .'
        bcs     L41A4                           ; 4178 B0 2A                    .*
        txa                                     ; 417A 8A                       .
        jsr     skipEscRulerEscGraphics                           ; 417B 20 A1 14                  ..
        cpx     #$0D                            ; 417E E0 0D                    ..
        bne     L4171                           ; 4180 D0 EF                    ..
        jsr     moveR15_cursor3Ptr                           ; 4182 20 B3 27                  .'
        bra     L4171                           ; 4186 50 E9                    P.

L4188:  jsr     moveCursor0Ptr_r15                           ; 4188 20 A1 27                  .'
L418B:  jsr     CmpR15Cursor1Ptr                           ; 418B 20 1F 28                  .(
        bcs     L41A4                           ; 418E B0 14                    ..
        jsr     getByteIntpNewCardSet                           ; 4190 20 5A 14                  Z.
        cmp     #$11                            ; 4193 C9 11                    ..
        bne     L419F                           ; 4195 D0 08                    ..
        jsr     L41A5                           ; 4197 20 A5 41                  .A
        jsr     addRulerSizeToR15                           ; 419A 20 34 27                  4'
        bcc     L418B                           ; 419D 90 EC                    ..
L419F:  jsr     incWR15                           ; 419F 20 1E 27                  .'
        bcc     L418B                           ; 41A2 90 E7                    ..
L41A4:  rts                                     ; 41A4 60                       `

; ----------------------------------------------------------------------------
L41A5:  ldy     a4L                             ; 41A5 A4 74                    .t
        lda     funcPtrs_lo,y                         ; 41A7 B9 DB 40                 ..@
        ldx     funcPtrs_hi,y                         ; 41AA BE DD 40                 ..@
        jmp     CallRoutine                     ; 41AD 4C D8 C1                 L..

; ----------------------------------------------------------------------------
L41B0:  jsr     getLastRuler                           ; 41B0 20 5B 1E                  [.
        jsr     moveCursor1Ptr_r15                           ; 41B3 20 AA 27                  .'
L41B6:  jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 41B6 20 9E 14                  ..
        tax                                     ; 41B9 AA                       .
        beq     @rts                           ; 41BA F0 26                    .&
        cmp     #PAGE_BREAK                            ; 41BC C9 0C                    ..
        beq     @rts                           ; 41BE F0 22                    ."
        cmp     #CR                            ; 41C0 C9 0D                    ..
        bne     L41B6                           ; 41C2 D0 F2                    ..
        jsr     pushR15                         ; 41C4 20 6C 28                  l(
        jsr     getByteIntpNewCardSet                           ; 41C7 20 5A 14                  Z.
        tax                                     ; 41CA AA                       .
        jsr     popR15                          ; 41CB 20 7F 28                  .(
        cpx     #ESC_RULER                            ; 41CE E0 11                    ..
        beq     @rts                           ; 41D0 F0 10                    ..
        lda     cursor0+cursor::srcline                        ; 41D2 A5 84                    ..
        sta     a4H                             ; 41D4 85 75                    .u
        jsr     makeSpaceForRuler                           ; 41D6 20 97 1C                  ..
        ldy     #26                            ; 41D9 A0 1A                    ..
@loop:  lda     (r14),y                        ; 41DB B1 1E                    ..
        sta     (r15),y                        ; 41DD 91 20                    . 
        dey                                     ; 41DF 88                       .
        bpl     @loop                           ; 41E0 10 F9                    ..

@rts:	rts                                     ; 41E2 60                       `

; ----------------------------------------------------------------------------
