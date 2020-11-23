; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  00 - MAIN: library code, core text editing
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"
	.include "geoWrite-2.inc"
	.include "geoWrite-3.inc"
	.include "geoWrite-4.inc"
	.include "geoWrite-5.inc"
	.include "geoWrite-6.inc"
	.include "geoWrite-7.inc"

; ----------------------------------------------------------------------------

.segment        "CODE0": absolute

        jmp     appInit

; ----------------------------------------------------------------------------
        jmp     MEM_OVERLAY                           ; $0403 [XXX unused?]

;---------------------------------------------------------------
; 8x5 bitmaps "1"-"8", used by the ruler
;---------------------------------------------------------------
bitmap_1:
	.byte   $80 + 5
	.byte	%00100000		; "1"
	.byte	%01100000
	.byte	%00100000
	.byte	%00100000
	.byte	%01110001
bitmap_2:
	.byte   $80 + 5
	.byte	%00110000		; "2"
	.byte	%01001000
	.byte	%00010000
	.byte	%00100000
	.byte	%01111001
bitmap_3:
	.byte   $80 + 5
	.byte	%01110000		; "3"
	.byte	%00001000
	.byte	%00110000
	.byte	%00001000
	.byte	%01110001
bitmap_4:
	.byte   $80 + 5
	.byte	%00010000		; "4"
	.byte	%00110000
	.byte	%01010000
	.byte	%01111000
	.byte	%00010001
bitmap_5:
	.byte   $80 + 5
	.byte	%01111000		; "5"
	.byte	%01000000
	.byte	%01110000
	.byte	%00001000
	.byte	%01110001
bitmap_6:
	.byte   $80 + 5
	.byte	%00110000		; "6"
	.byte	%01000000
	.byte	%01110000
	.byte	%01001000
	.byte	%00110001
bitmap_7:
	.byte   $80 + 5
	.byte	%01111000		; "7"
	.byte	%00001000
	.byte	%00010000
	.byte	%00100000
	.byte	%00100001
bitmap_8:
	.byte   $80 + 5
	.byte	%01111000		; "8"
	.byte	%01001000
	.byte	%01111000
	.byte	%01001000
	.byte	%01111000

;---------------------------------------------------------------
; clearMarginbar
;
; Function: Clear the margin bar, which is the 320x5 pixel
;           area below the ruler that contains the M, P
;           and tab stop symbols.
;---------------------------------------------------------------
clearMarginbar:
	jsr     i_BitmapUp
		.addr   graph_marginbar
		.byte   0		; x
		.byte   24		; y
		.byte   40		; width
		.byte   5		; height
rts4:	rts

;---------------------------------------------------------------
; updateRulerUI
;
; Function: Draw the ruler, the margin bar and the
;           justification/spacing UI according to the current
;           state.
;---------------------------------------------------------------
updateRulerUI:
	ldx     #0
        lda     sideFlippingOffset
        cmp     tmpSideFlippingOffset
        beq     :+
        dex
:	sta     tmpSideFlippingOffset

        ldy     #.sizeof(ruler)-1
@loop:  lda     rulerData1,y
        eor     tmpRulerData,y
        cpy     #ruler::justification
        bne     :+
        and     #MASK_JST_SPACING+MASK_JST_JUST
:	cmp     #0
        beq     :+
        dex				; difference -> update UI later
:	lda     rulerData1,y
        sta     tmpRulerData,y
        dey
        bpl     @loop

        txa
        bpl     rts4			; nothing to update

        jsr     drawInchMarks
        jsr     clearMarginbar
        jsr     drawMarginbar
        jsr     updatePageIndicator
        jsr     i_GraphicsString	; clear all justification checkboxes
		.byte NEWPATTERN
		.byte 0
		.byte MOVEPENTO
		.word $1A
		.byte $1E
		.byte RECTANGLETO
		.word $1C
		.byte $20
		.byte MOVEPENTO
		.word $41
		.byte $1E
		.byte RECTANGLETO
		.word $43
		.byte $20
		.byte MOVEPENTO
		.word $60
		.byte $1E
		.byte RECTANGLETO
		.word $62
		.byte $20
		.byte MOVEPENTO
		.word $7C
		.byte $1E
		.byte RECTANGLETO
		.word $7E
		.byte $20
		.byte MOVEPENTO
		.word $10F
		.byte $1E
		.byte RECTANGLETO
		.word $111
		.byte $20
		.byte MOVEPENTO
		.word $124
		.byte $1E
		.byte RECTANGLETO
		.word $126
		.byte $20
		.byte MOVEPENTO
		.word $135
		.byte $1E
		.byte RECTANGLETO
		.word $137
		.byte $20
		.byte NEWPATTERN	; fill next one
		.byte 1
		.byte NULL
        lda     rulerData1+ruler::justification
        and     #MASK_JST_JUST
        jsr     @draw
        lda     rulerData1+ruler::justification
        and     #MASK_JST_SPACING
        lsr     a
        lsr     a
        add     #4

@draw:  tax
        lda     jst_checkbox_x_hi,x
        sta     @left+1
        sta     @right+1
        lda     jst_checkbox_x_lo,x
        sta     @left
        add     #2
        sta     @right
        jsr     i_Rectangle
        	.byte   30		; top
        	.byte   32		; bottom
@left:  	.word   0		; left
@right:		.word   0		; right
        rts

; x offsets of checkboxes below ruler:
;                left center right full  1   1.5   2
.define jst_checkbox_x 26,   65,   96,  124, 271, 292, 309
jst_checkbox_x_lo:  .lobytes jst_checkbox_x
jst_checkbox_x_hi:  .hibytes jst_checkbox_x
jst_checkbox_x_count = * - jst_checkbox_x_hi

;---------------------------------------------------------------
; drawInchMarks
;
; Function: Draw the numbers in the ruler indicating the inch
;           position.
;---------------------------------------------------------------
drawInchMarks:
	ldx     #0			; offset 0
        ldy     sideFlippingOffset
        beq     @1
        ldx     #2			; offset 2
        dey
        beq     @1
        ldx     #4			; offset 4
@1:	ldy     pageWidth1+1
        dey
        bne     @2
        inx
@2:	stx     r14L
        lda     #8
        sta     @xpos
        lda     #4
        sta     r14H			; 4 numbers
@3:	ldx     r14L
        lda     inchmark_ptrs,x
        sta     @bitmap
        jsr     i_BitmapUp
@bitmap:  	.word   bitmap_2
@xpos:  	.byte   8		; x
		.byte   17		; y
		.byte   1		; width
		.byte   5		; height
        lda     @xpos
        add     #10
        sta     @xpos			; + 10 cards
        inc     r14L
        dec     r14H
        bne     @3
        rts

inchmark_ptrs:
	.byte <bitmap_1, <bitmap_2, <bitmap_3, <bitmap_4, <bitmap_5, <bitmap_6, <bitmap_7, <bitmap_8
	.assert >bitmap_1 = >bitmap_8, error
;---------------------------------------------------------------
; drawMarginbar
;
; Function: Draw the P, M and tab stop symbols into the margin
;           bar.
;---------------------------------------------------------------
drawMarginbar:
	ldy     #0
@loop:  lda     rulerData1+ruler::left_margin,y
        sta     r11L
        lda     rulerData1+ruler::left_margin+1,y
        and     #$7F
        sta     r11H
        iny
        iny
        cpy     #ruler::paragraph_margin+4
        beq     @rts
        jsr     screenXFromPageX
        bcs     @loop			; is left of screen, ignore

        lda     r11L
        and     #7
        beq     :+
	IncW	r11
:	ldx     #rulerM-rulerIcons	; "M"
        cpy     #ruler::tabs+2
        bcc     @3			; left/right margin

        lda     rulerData1-2,y
        cmp     rulerData1+ruler::right_margin
        bne     @1
        lda     rulerData1-1,y
        cmp     rulerData1+ruler::right_margin+1
        beq     @loop			; ignore

@1:	ldx     #rulerTab-rulerIcons
        lda     rulerData1-1,y
        bpl     @2

        ldx     #rulerDTab-rulerIcons

@2:	cpy     #ruler::paragraph_margin+2
        bcc     @3

        ldx     #rulerP-rulerIcons

@3:     tya
        pha
        jsr     drawMarginBarItem1
        pla
        tay
        jmp     @loop

@rts:	rts

;---------------------------------------------------------------
; updatePageIndicator
;
; Function:  Update the page location indicator sprite position.
;
; Pass:      a5  vertical position on page
;---------------------------------------------------------------
updatePageIndicator:
	LoadB   r3L, 6			; page indicator sprite
	LoadW   r4, 192			; x coordinate
        ldy     sideFlippingOffset
        beq     @2			; at the very left

        lda     #5
        ldx     pageWidth1+1
        dex
        beq     @1

        lda     #4			; add 4 pixels
        dey
        beq     @1

        lda     #8			; add 8 pixels

@1:	ldx     #r4
        jsr     addWIToZp		; add to r4

@2:	MoveW   pagePosY, r0
        jsr     @divHeight		; [XXX]
        txa				; [XXX]
        add     #0			; [XXX]
        sta     r5L
        jsr     PosSprite
        jmp     EnablSprite

@divHeight:
	ldx     #r0
        ldy     #usablePageHeightDiv13
        jsr     Ddiv
        ldx     r0L
        rts

;---------------------------------------------------------------
; drawMarginBarItem1
;
; Function:  Draw one of the M, P, Tab, DTab icons into the
;            margin bar.
;
; Pass:      x   index of icon * 10
;
; Notes:     * graphics data is 16x5 pixels
;            * if the MSB of the first byte is inverted, the
;              graphics data is ANDed, otherwise ORed
;---------------------------------------------------------------
drawMarginBarItem1:
	txa
        add     #<rulerIcons
        sta     r0L
        lda     #0
        adc     #>rulerIcons
        sta     r0H

;---------------------------------------------------------------
; drawMarginBarItem2
;
; Function:  Draw one of the M, P, Tab, DTab icons into the
;            margin bar.
;
; Pass:      r0  pointer to icon
;---------------------------------------------------------------
drawMarginBarItem2:
	lda     #8
        ldx     #r11
        jsr     subWBI			; r11 -= 8

        ldy     #0
        lda     (r0),y			; first byte has flag in MSB
        sta     r2L
        ldx     #24			; first line of margin bar

@loop:  jsr     GetScanLine		; r5 = scanline ptr + r11
        AddW    r11, r5
        ldy     #0
	CmpWI	r11, -8
	beq     @3			; clip on the left

        lda     (r0),y			; get first byte of row
        bit     r2L
        bmi     @1			; negative: clear pixels, positive: set pixels

        ora     (r5),y			; set pixels
        bra     @2

@1:	and     (r5),y			; clear pixels
@2:	sta     (r5),y
@3:	AddVW	8, r5			; next 8 pixels
	jsr     incWr0			; next input byte
	CmpWI	r11, SC_PIX_WIDTH-8
	beq     @6			; clip on the right

        lda     (r0),y			; get second byte of row
        bit     r2L
        bmi     @4			; negative: clear pixels, positive: set pixels

        ora     (r5),y			; set pixels
        bra     @5

@4:	and     (r5),y			; clear pixels
@5:	sta     (r5),y
@6:	jsr     incWr0
        inx
        cpx     #24+5			; 5 lines
        bmi     @loop

        ldx     #r11
        lda     #8
        jmp     addWIToZp

; ----------------------------------------------------------------------------
L065D:  jsr     removeRedundantRulersAndCardsets                           ; 065D 20 9C 06                  ..
        jsr     resetToPageStart                           ; 0660 20 B1 26                  .&
L0663:  jsr     measureLine                           ; 0663 20 7A 10                  z.
        bcs     L067A                           ; 0666 B0 12                    ..
        CmpW    pageTextHeight, pagePosY
	bcs     L067A                           ; 0672 B0 06                    ..
        jsr     L27BC                           ; 0674 20 BC 27                  .'
        bra     L0663                           ; 0678 50 E9                    P.

L067A:  MoveW   pageTextHeight, pagePosY
        MoveW   cursor3+cursor::font, tmpFont3                        ; 0688 85 C5                    ..
        lda     cursor3+cursor::mode                        ; 068A A5 A7                    ..
        sta     tmpMode3                        ; 068C 85 C7                    ..
	MoveW	r15, topScrTxtPtr
        jsr     copyRuler1To2                           ; 0696 20 8E 28                  .(
        jmp     L15CA                           ; 0699 4C CA 15                 L..

;---------------------------------------------------------------
; removeRedundantRulersAndCardsets
;
; Function:  Interpret page until the end, removing all ruler
;            and cardset escapes that were identical to the
;            previous one.
;
; Pass:      r15  start pointer
;---------------------------------------------------------------
removeRedundantRulersAndCardsets:
	jsr     LoadR15_MEM_PAGE
        ldy     #0
        sty     r6L			; start with illegal font
        sty     r6H
        dey				; and illegal ruler
        sty     rulerData1+ruler::left_margin

@again:	jsr     cmpPageEndPtr2R15	; beyond end, then done
        bcc     rts7
        beq     rts7

        ldy     #0
        lda     (r15),y
        cmp     #ESC_GRAPHICS
        beq     @graph			; then skip 5
        cmp     #ESC_RULER
        beq     @ruler
        cmp     #NEWCARDSET
        bne     @char

        jsr     copyCardsetToRegsCmp	; compare cardset to r6: font, r7L: mode
        cpx     #0
        bmi     @1			; cardset was different, skip 4

        LoadB   r8L, 4
        jsr     deleteRange		; delete redundant cardset from page
        bra     @again

@graph:	jsr     incWR15			; skip 5
@1:	jsr     addWI3R15		; skip 4
@char:	jsr     incWR15			; skip 1
        bcc     @again

@ruler:	jsr     copyToRuler1Cmp
        txa
        bmi     @2
        LoadB   r8L, .sizeof(ruler)+1
        jsr     deleteRange
        bra     @again

@2:	jsr     addRulerSizeToR15
        bcc     @again			; always


copyCardsetToRegsCmp:
	ldy     #3
        ldx     #0
@loop:  lda     (r15),y
        cmp     r6L-1,y
        beq     @skip
        dex
@skip:  sta     r6L-1,y
        dey
        bne     @loop
rts7:	rts

; ----------------------------------------------------------------------------
_mouseScrollUpDown:
	bit     zp_DBb                        ; 0704 24 DB                    $.
        bmi     rts7                           ; 0706 30 FB                    0.
        jmp     J2_mouseScrollUpDown                       ; 0708 4C 5C 32                 L\2

; ----------------------------------------------------------------------------
copyToRuler1Cmp:
	ldy     #.sizeof(ruler)
        ldx     #0
@loop:  lda     (r15),y
        cpy     #ruler::justification+1
        bne     @1
        and     #$0F
@1:	cmp     rulerData1-1,y
        beq     @2
        dex
@2:	sta     rulerData1-1,y
        dey
        bne     @loop
        rts

;---------------------------------------------------------------
; deleteRange
;
; Function:  Deletes a range of bytes on the page and updates
;            the cursor/selection pointers to be consistent.
;
; Pass:      r15  start of the range
;            r8L  length
;---------------------------------------------------------------
deleteRange:
	lda     r15L			; tmpRangeStart = r15
        sta     tmpRangeStart		; tmpRangeEnd   = r15 + r8L
        add     r8L
        sta     tmpRangeEnd
        lda     r15H
        sta     tmpRangeStart+1
        adc     #0
        sta     tmpRangeEnd+1

        PushW_r15			; delete range
        jsr     deleteTextAndPicturesinRange
        PopW_r15

        jsr     cmpR15Cursor0Ptr	; if r15 < selection start
        bcs     @skip

        lda     cursor0+cursor::ptr	; then selection start -= r8L
        sub     r8L
        sta     cursor0+cursor::ptr
        bcs     @skip
        dec     cursor0+cursor::ptr+1

@skip:	jsr     CmpR15Cursor1Ptr	; if r15 < selection end?
        bcs     @rts

        lda     cursor1+cursor::ptr	; then selection end -= r8L
        sub     r8L
        sta     cursor1+cursor::ptr
        bcs     @rts
        dec     cursor1+cursor::ptr+1

@rts:	rts

; ----------------------------------------------------------------------------
editor:
	lda     #$FF                            ; 075F A9 FF                    ..
        sta     zp_DDb                        ; 0761 85 DD                    ..
        LoadW_	pagePosY, 0                           ; 0767 85 77                    .w
        sta     nextPageOpen                        ; 0769 85 DE                    ..
        sta     sideFlippingOffset                           ; 076B 8D BC 2F                 ../
        sta     zp_D9b                        ; 076E 85 D9                    ..
        jsr     loadCursor0PtrPageCardset                           ; 0770 20 76 27                  v'
        jsr     copyCursor0To1                           ; 0773 20 C0 28                  .(
        jsr     L065D                           ; 0776 20 5D 06                  ].
        jsr     J2_analyzeSelectionFont                       ; 0779 20 65 32                  e2
        ldx     #0                            ; 077C A2 00                    ..
        jsr     getLineRuler                           ; 077E 20 C9 1D                  ..

L0781:  sec                                     ; 0781 38                       8
L0782:  lda     #$FF                            ; 0782 A9 FF                    ..
        sta     cursor0+cursor::srcline                        ; 0784 85 84                    ..
        sta     cursor1+cursor::srcline                      ; 0786 85 8E                    ..
        bcc     L0796                           ; 0788 90 0C                    ..

        jsr     pushRulerData                           ; 078A 20 73 26                  s&
        jsr     L0862                           ; 078D 20 62 08                  b.
        jsr     popRulerData                           ; 0790 20 93 26                  .&
        jmp     setPromptFontMetricsUpdateRulerUI                           ; 0793 4C 7A 1D                 Lz.

L0796:  jsr     L0862                           ; 0796 20 62 08                  b.
        jmp     setPromptFontMetricsUpdateRulerUI                           ; 0799 4C 7A 1D                 Lz.

; ----------------------------------------------------------------------------
L079C:  clc                                     ; 079C 18                       .
        bcc     L0782                           ; 079D 90 E3                    ..

;---------------------------------------------------------------
; measurePage
;
; Function:  Iterate over all characters until the end of the
;            page and collect metrics information.
;---------------------------------------------------------------
measurePage:
	lda     curPage
        cmp     #PAGE_HEADER
        bcc     @1
	MoveW   pageEndPtr2, pageEndPtr1
        rts

@1:	jsr     resetToPageStart

@loop:  MoveW   r15, zp_ADw
	DecW    zp_ADw			; zp_ADw = page start - 1
	MoveW   curFont, tmpFont1
        MoveB   currentMode, tmpMode1
        MoveB   justification, tmpJustification1
        jsr     measureLine		; <---------------!
        php
        jsr     moveCursor3Ptr_r15
        jsr     addLineHeightToPageHeight
	CmpW    pageTextHeight, usablePageHeight
	bcc     @2
					; line does not fit on page
        plp
	MoveW   zp_ADw, pageEndPtr1
	MoveW   tmpFont1, curFont
        MoveB   tmpMode1, currentMode
        lda     tmpJustification1
        and     #$FF-JST_20-JST_10
        sta     r4L
	lda     tmpJustification1
        and     #JST_20
        lsr     a
        ora     r4L
        sta     rulerData1+ruler::justification
        jmp     loadFontMetrics

@2:	plp				; page end?
        bcc     @loop			; no, continue

        lda     justification
        sta     rulerData1+ruler::justification
	MoveW   cursor3+cursor::ptr, pageEndPtr1

        lda     curPage
        cmp     #PAGE_LAST_USABLE
        bne     @rts
        ldy     #0
        lda     (pageEndPtr2),y
        beq     @rts			; last page && does not end on NULL?

	MoveW   pageEndPtr1, pageEndPtr2
        tya
        sta     (pageEndPtr2),y
        sta     nextPageOpen
        jsr     _showTooManyPages	; then show error

@rts:	rts

; ----------------------------------------------------------------------------
L083C:  jsr     copyCursor1To3                           ; 083C 20 B7 28                  .(
        jsr     moveCursor1Ptr_r15                           ; 083F 20 AA 27                  .'
        lda     cursor1+cursor::srcline                      ; 0842 A5 8E                    ..
        sta     r3L                             ; 0844 85 08                    ..
        jsr     L08CD                           ; 0846 20 CD 08                  ..
        jsr     copyCursor3To1                           ; 0849 20 C9 28                  .(
        jsr     copyCursor0ToY                           ; 084C 20 BD 28                  .(
        jsr     moveCursor0Ptr_r15                           ; 084F 20 A1 27                  .'
        lda     cursor0+cursor::srcline                        ; 0852 A5 84                    ..
        sta     r3L                             ; 0854 85 08                    ..
        jsr     L08CD                           ; 0856 20 CD 08                  ..
        jmp     copyCursor3To0                           ; 0859 4C CC 28                 L.(

;---------------------------------------------------------------
; clearSelection
;
; Function:  Clears the current selection if there is one.
;---------------------------------------------------------------
clearSelection:
	jsr     invertSelectionIfNeeded
        jmp     copyCursor0To1

; ----------------------------------------------------------------------------
L0862:  jsr     L083C                           ; 0862 20 3C 08                  <.
        jsr     cmpCursor0_1Ptr                           ; 0865 20 E8 27                  .'
        beq     rts3                           ; 0868 F0 03                    ..
        jsr     invertSelection                           ; 086A 20 73 08                  s.
rts3:	rts                                     ; 086D 60                       `

;---------------------------------------------------------------
; invertSelectionIfNeeded
;
; Function:  Inverts the current selection if there is one.
;---------------------------------------------------------------
invertSelectionIfNeeded:
	jsr     cmpCursor0_1Ptr
        beq     rts3

;---------------------------------------------------------------
; invertSelection
;
; Function:  Inverts the current.
;---------------------------------------------------------------
invertSelection:
	ldx     #cursor0-userzp		; copy range (cursor0:cursor1) to cursor3:cursor2
        ldy     #cursor3-userzp
        jsr     copyCursor
        ldx     #cursor1-userzp
        ldy     #cursor2-userzp
        jsr     copyCursor

invertSelection2:
	ldx     #0
        ldy     #4
        jsr     CmpCursor2_3Ptr		; backwards?
        bcs     :+
        ldx     #4
        ldy     #0
:	lda     cursor3+cursor::px		; r12 = X
        sta     r12L,x
        lda     cursor3+cursor::px+1
        sta     r12H,x
        lda     cursor3+cursor::py		; r13L = Y
        sta     r13L,x
        lda     cursor3+cursor::height	; r13H = height-1
        sub     #1
        sta     r13H,x

        lda     cursor2+cursor::px		; r14 = X
        sta     r12L,y
        lda     cursor2+cursor::px+1
        sta     r12H,y
        lda     cursor2+cursor::py		; r15L = Y
        sta     r13L,y
        lda     cursor2+cursor::height	; r15H = height-1
        sub     #1
        sta     r13H,y

	IncW	r12			; X += 1

	lda     r13L			; compare Y
        cmp     r15L
        beq     @1

        jsr     invertSelection_XXX1
        jmp     invertSelection_XXX2

@1:	jmp     invertSelection_XXX3

; ----------------------------------------------------------------------------
L08CD:  jsr     L0937                           ; 08CD 20 37 09                  7.
        stx     cursor3+cursor::srcline                        ; 08D0 86 A2                    ..
        jsr     moveR15_cursor3Ptr                           ; 08D2 20 B3 27                  .'
        bcs     L08F9                           ; 08D5 B0 22                    ."
        PushW_r15                               ; 08D7 20 6C 28                  l(
        jsr     getLineRuler                           ; 08DA 20 C9 1D                  ..
        ldx     cursor3+cursor::srcline                        ; 08DD A6 A2                    ..
        jsr     justifyText2                           ; 08DF 20 18 17                  ..
        PopW    cursor3+cursor::ptr                        ; 08E3 85 9E                    ..
        jsr     L0927                           ; 08E8 20 27 09                  '.
	MoveW	r11, r10
        jsr     screenXFromPageX                           ; 08F3 20 0F 09                  ..
        jsr     L0902                           ; 08F6 20 02 09                  ..
L08F9:  MoveW	r11, cursor3+cursor::px
        rts                                     ; 0901 60                       `

L0902:  MoveB   currentMode, cursor3+cursor::mode                        ; 0904 85 A7                    ..
	MoveW   curFont, cursor3+cursor::font                        ; 090C 85 A5                    ..
L090E:  rts                                     ; 090E 60                       `

;---------------------------------------------------------------
; screenXFromPageX
;
; Function:  Convert a page-relative X coordinate to screen-
;            relative. If its outside the screen, return the
;            leftmost or rightmost screen X coordinate.
;
; Pass:      r11 page x coordinate
;
; Return:    c    =0: ok, =1: underflow
;            r11  screen x coordinate (if not underflow)
;---------------------------------------------------------------
screenXFromPageX:
	ldx     #r11
        jsr     subSideFlippingPixelOffset
        lda     r11H
        bpl     @1
        jsr     loadWR11Zero		; underflow, left of screen -> 0
        sec
        rts

@1:	jsr     CmpR11ScrWidth
        bcc     @2			; is on screen
        jmp     loadR11ScreenWidthMinus1; clamp to right screen margin

@2:	clc
        rts

; ----------------------------------------------------------------------------
L0927:  jsr     CmpR15Cursor3Ptr                           ; 0927 20 2A 28                  *(
        bcs     L090E                           ; 092A B0 E2                    ..
        jsr     L14BD                           ; 092C 20 BD 14                  ..
        bcs     L090E                           ; 092F B0 DD                    ..
        jsr     moveWR2R11                           ; 0931 20 C8 27                  .'
        bra     L0927                           ; 0935 50 F0                    P.

L0937:  CmpW	r15, topScrTxtPtr                         ; 093F C5 7C                    .|
	bcs     L0953                           ; 0941 B0 10                    ..
        jsr     loadWR11Zero                           ; 0943 20 88 27                  .'
        lda     #36                            ; 0946 A9 24                    .$
        sta     cursor3+cursor::py                        ; 0948 85 A3                    ..
        ldx     #$80                            ; 094A A2 80                    ..
        lda     lintab_height                           ; 094C AD 75 2B                 .u+
        sta     cursor3+cursor::height                        ; 094F 85 A4                    ..
        sec                                     ; 0951 38                       8
        rts                                     ; 0952 60                       `

; ----------------------------------------------------------------------------
L0953:  jsr     copyRuler2To1                           ; 0953 20 94 28                  .(
        ldx     #$00                            ; 0956 A2 00                    ..
L0958:  lda     lintab_height,x                         ; 0958 BD 75 2B                 .u+
        beq     L09A9                           ; 095B F0 4C                    .L
        inx                                     ; 095D E8                       .
        lda     r15H                            ; 095E A5 21                    .!
        cmp     lintab_txtPtr_hi,x                         ; 0960 DD 4B 2B                 .K+
        bne     L096A                           ; 0963 D0 05                    ..
        lda     r15L                            ; 0965 A5 20                    . 
        cmp     lintab_txtPtr_lo,x                       ; 0967 DD 36 2B                 .6+
L096A:  bcc     L09A6                           ; 096A 90 3A                    .:
        bne     L0958                           ; 096C D0 EA                    ..
        lda     r15H                            ; 096E A5 21                    .!
        sta     r14H                            ; 0970 85 1F                    ..
        cmp     lintab_txtPtr_hi-1,x                         ; 0972 DD 4A 2B                 .J+
        bne     L097E                           ; 0975 D0 07                    ..
        lda     r15L                            ; 0977 A5 20                    . 
        cmp     lintab_txtPtr_lo-1,x                         ; 0979 DD 35 2B                 .5+
        beq     L09A6                           ; 097C F0 28                    .(
L097E:  ldy     r15L                            ; 097E A4 20                    . 
        bne     L0984                           ; 0980 D0 02                    ..
        dec     r14H                            ; 0982 C6 1F                    ..
L0984:  dey                                     ; 0984 88                       .
        sty     r14L                            ; 0985 84 1E                    ..
        lda     lintab_justification-1,x
        bmi     L0958			; JST_80
        ldy     #$00                            ; 098C A0 00                    ..
        lda     (r14),y                        ; 098E B1 1E                    ..
        cmp     #$0D                            ; 0990 C9 0D                    ..
        beq     L0958                           ; 0992 F0 C4                    ..
        ldy     #$00                            ; 0994 A0 00                    ..
        lda     (r15),y                        ; 0996 B1 20                    .
        jsr     isPageBreakOrNull                           ; 0998 20 8C 13                  ..
        bcs     L09A6                           ; 099B B0 09                    ..
        lda     lintab_height,x                         ; 099D BD 75 2B                 .u+
        beq     L09A6                           ; 09A0 F0 04                    ..
        cpx     r3L                             ; 09A2 E4 08                    ..
        beq     L0958                           ; 09A4 F0 B2                    ..
L09A6:  dex                                     ; 09A6 CA                       .
        clc                                     ; 09A7 18                       .
        rts                                     ; 09A8 60                       `

; ----------------------------------------------------------------------------
L09A9:  lda     windowBottom                    ; 09A9 A5 34                    .4
        sta     cursor3+cursor::py                        ; 09AB 85 A3                    ..
        ldx     #$40                            ; 09AD A2 40                    .@

loadR11ScreenWidthMinus1:  sec                                     ; 09AF 38                       8
	LoadW   r11, SC_PIX_WIDTH-1
        rts                                     ; 09B8 60                       `

; ----------------------------------------------------------------------------
invertSelection_XXX1:
	jsr     copyR12R3_R14R4
        ldx     #>(SC_PIX_WIDTH-1)
        stx     r4H
        ldy     #<(SC_PIX_WIDTH-1)
        sty     r4L
        cpx     r3H
        bne     :+
        cpy     r3L
:	bcc     @3
	MoveB   r13L, r2L
        add     r13H
        bcs     @1
        cmp     windowBottom
        bcc     @2
@1:	lda     windowBottom
@2:	sta     r2H
        jsr     InvertRectangle
@3:	jsr     copyR12R3_R14R4
        jsr     loadWR3Zero
        lda     r15L
        sta     r2L
        add     r15H
        bcs     @4
        cmp     windowBottom
        bcc     @5
@4:	lda     windowBottom
@5:	sta     r2H
        jmp     InvertRectangle

; ----------------------------------------------------------------------------
invertSelection_XXX2:
	jsr     loadWR3Zero		; left = 0
        jsr     LoadWR4ScPixWidthMinus1	; right = max x
        ldy     r15L
        dey
        sty     r2H			; bottom
        lda     r13L
        sec
        adc     r13H
        bcs     @1			; overflow? clamp
        cmp     windowBottom
        bcc     @2			; above max y? clamp
@1:	lda     windowBottom
@2:	sta     r2L			; top
        cmp     r2H
        beq     @3
        bcs     rts6			; negatve height, skip
@3:	jmp     InvertRectangle

; ----------------------------------------------------------------------------
invertSelection_XXX3:
	jsr     copyR12R3_R14R4		; get left and right
	CmpW	r4, r3			; negative width? skip
	bcc     rts6
        MoveB   r13L, r2L		; top
        add     r13H			; + height
        bcs     @2			; overflow? clamp
        cmp     windowBottom
        bcc     @3			; above max y? clamp
@2:	lda     windowBottom
@3:	sta     r2H			; bottom
        jmp     InvertRectangle

; ----------------------------------------------------------------------------
copyR12R3_R14R4:
	MoveW	r12, r3
	MoveW	r14, r4
rts6:	rts

;---------------------------------------------------------------

	.include "strings1.inc"

;---------------------------------------------------------------
; The app's menu
;---------------------------------------------------------------
menu_main:
	.byte   0			; top
	.byte   14			; bottom
	.word   0			; left
.if LANG=LANG_DE
	.word	183			; right
.else
	.word	191
.endif
	.byte   HORIZONTAL | UN_CONSTRAINED | M_ITEMS
menu_main_items:
	.word   txt_geos
        .byte   DYN_SUB_MENU
        .word   callbackGeos

        .word   txt_file
        .byte   DYN_SUB_MENU
        .word   callbackFile

        .word   txt_edit
        .byte   DYN_SUB_MENU
        .word   callbackEdit

        .word   txt_options
        .byte   DYN_SUB_MENU
        .word   callbackOptions

        .word   txt_page
        .byte   DYN_SUB_MENU
        .word   callbackPage

        .word   txt_font
        .byte   DYN_SUB_MENU
        .word   callbackFont

        .word   txt_style
        .byte   DYN_SUB_MENU
        .word   callbackStyle
M_ITEMS = (* - menu_main_items)/MISIZE

menu_geos:
	.byte   SUBMENU_Y		; top
menu_geos_bottom:
	.byte   SUBMENU_Y + 1 + 14 * 1	; bottom (will be overwritten)
	.word   0			; left
	.word   79			; right
menu_geos_count:
	.byte   VERTICAL | UN_CONSTRAINED | 1	; will be overwritten

	.word   txt_geowrite_info
        .byte   MENU_ACTION
	.word   _showAboutDialog

	.word   deskAccNames + 17 * 0
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 1
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 2
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 3
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 4
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 5
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 6
        .byte   MENU_ACTION
        .word   _loadDeskAcc

        .word   deskAccNames + 17 * 7
        .byte   MENU_ACTION
        .word   _loadDeskAcc

menu_file:
	.byte   SUBMENU_Y		; top
	.byte   SUBMENU_Y + 1 + 14 * 7	; bottom
        .word   28			; left
.if LANG=LANG_EN
        .word   75			; right
.elseif LANG=LANG_DE
	.word	106
.endif
	.byte   VERTICAL | UN_CONSTRAINED | 7

        .word   txt_close
        .byte   MENU_ACTION
        .word   callbackClose

        .word   txt_update
        .byte   MENU_ACTION
        .word   callbackUpdate

        .word   txt_preview
        .byte   MENU_ACTION
        .word   callbackPreview

        .word   txt_recover
        .byte   MENU_ACTION
        .word   callbackRecover

        .word   txt_rename
        .byte   MENU_ACTION
        .word   callbackRename

        .word   txt_print
        .byte   MENU_ACTION
        .word   callbackPrint

        .word   txt_quit
        .byte   MENU_ACTION
        .word   callbackQuit

menu_edit:
	.byte   SUBMENU_Y		; top
	.byte   SUBMENU_Y + 1 + 14 * 3	; bottom
.if LANG=LANG_EN
	.word   48			; left
	.word   103			; right
.elseif LANG=LANG_DE
	.word   56
	.word   147
.endif
	.byte   VERTICAL | UN_CONSTRAINED | 3

	.word   txt_cut
        .byte   MENU_ACTION
	.word   _cut

	.word   txt_copy
        .byte   MENU_ACTION
	.word   _copy

	.word   txt_paste
	.byte   DYN_SUB_MENU
	.word   callbackPaste

menu_paste:
	.byte   SUBMENU_Y		; top
	.byte   SUBMENU_Y + 1 + 14 * 2	; bottom
.if LANG=LANG_EN
        .word   104			; left
        .word   167			; right
.elseif LANG=LANG_DE
        .word   148
        .word   211
.endif
	.byte   VERTICAL | UN_CONSTRAINED | 2

        .word   txt_text
        .byte   MENU_ACTION
        .word   _pasteText

        .word   txt_picture
        .byte   MENU_ACTION
        .word   J2_pastePicture

menu_options:
	.byte   SUBMENU_Y		; top
menu_options_bottom:
        .byte   SUBMENU_Y + 1 + 14 * 8 ; bottom
.if LANG=LANG_EN
        .word   71			; left
        .word   182			; right
.elseif LANG=LANG_DE
        .word   78
        .word   189
.endif
menu_options_count:
	.byte   VERTICAL | UN_CONSTRAINED | 8

        .word   txt_search
        .byte   MENU_ACTION
        .word   _search

        .word   txt_find_next
        .byte   MENU_ACTION
        .word   _findNext

        .word   txt_change
        .byte   MENU_ACTION
        .word   _change

        .word   txt_hide_pictures
        .byte   MENU_ACTION
        .word   _hidePictures

        .word   txt_open_header
        .byte   MENU_ACTION
        .word   _openHeader

        .word   txt_open_footer
        .byte   MENU_ACTION
        .word   _openFooter

        .word   txt_select_page
        .byte   MENU_ACTION
        .word   J2_selectPage

        .word   txt_make_full_page_wide
        .byte   MENU_ACTION
        .word   callbackMakeFullPageWide

menu_page:
	.byte   SUBMENU_Y		; top
	.byte   SUBMENU_Y + 1 + 14 * 7	; bottom
.if LANG=LANG_EN
        .word   110			; left
        .word   205			; right
.elseif LANG=LANG_DE
        .word   100
        .word   195
.endif
	.byte   VERTICAL | UN_CONSTRAINED | 7

        .word   txt_previous_page
        .byte   MENU_ACTION
        .word   _previousPage

        .word   txt_next_page
        .byte   MENU_ACTION
        .word   _nextPage

        .word   txt_goto_page
        .byte   MENU_ACTION
        .word   _gotoPage

        .word   txt_page_break
        .byte   MENU_ACTION
        .word   J2_pageBreak

        .word   txt_set_first_page
        .byte   MENU_ACTION
        .word   _setFirstPage

        .word   txt_title_page
        .byte   MENU_ACTION
        .word   _titlePage

        .word   txt_nlq_spacing
        .byte   MENU_ACTION
        .word   callbackNLQSpacing

menu_fontnames:
	.byte   SUBMENU_Y		; top
menu_fontnames_bottom:
	.byte   SUBMENU_Y + 1 + 14 * 1	; bottom
.if LANG=LANG_EN
	.word   138			; left
	.word   213			; right
.elseif LANG=LANG_DE
	.word   128
	.word   203
.endif
menu_fontnames_items:
	.byte   VERTICAL | UN_CONSTRAINED | 1	; will be overwritten

	.word   fontNames + 19 * 0
	.byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 1
	.byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 2
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 3
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 4
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 5
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 6
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

        .word   fontNames + 19 * 7
        .byte   DYN_SUB_MENU
        .word   callbackFontSelected

menu_fontpoints:
	.byte   SUBMENU_Y		; top
menu_fontpoints_bottom:
	.byte   SUBMENU_Y + 1 + 14 * 1	; bottom
.if LANG=LANG_EN
	.word   214			; left
	.word   274			; right
.elseif LANG=LANG_DE
	.word   204
	.word   264
.endif
menu_fontpoints_items:
	.byte   VERTICAL | UN_CONSTRAINED | 1	; will be overwritten

	.word   txt_xx_point + txt_xx_point_len * 0
	.byte   MENU_ACTION
	.word   callbackFontPoint

	.word   txt_xx_point + txt_xx_point_len * 1
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 2
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 3
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 4
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 5
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 6
        .byte   MENU_ACTION
        .word   callbackFontPoint

        .word   txt_xx_point + txt_xx_point_len * 7
        .byte   MENU_ACTION
        .word   callbackFontPoint

menu_style:
	.byte SUBMENU_Y			; top
	.byte SUBMENU_Y + 1 + 14 * 7	; bottom
.if LANG=LANG_EN
	.word	163			; left
	.word	248			; right
.elseif LANG=LANG_DE
	.word	162
	.word	259
.endif
	.byte VERTICAL | UN_CONSTRAINED | 7	; will be overwritten

        .word txt_plain_text
        .byte MENU_ACTION
        .word plainText

        .word txt_bold
        .byte MENU_ACTION
        .word bold

        .word txt_italic
        .byte MENU_ACTION
        .word italics

        .word txt_outline
        .byte MENU_ACTION
        .word outline

        .word txt_underline
        .byte MENU_ACTION
        .word underline

        .word txt_superscript
        .byte MENU_ACTION
        .word superscript

        .word txt_subscript
        .byte MENU_ACTION
        .word subscript

;---------------------------------------------------------------
; callbackClose
;
; Function:  Callback for "file -> close". Closes the file and
;            returns to the main menu.
;---------------------------------------------------------------
callbackClose:
        jsr     closeMenuCloseFile
        jsr     testDiskNotFull
        jmp     restart

;---------------------------------------------------------------
; closeMenuCloseFile
;
; Function:  Update and close the current file.
;
; Note:      This can only be called from a menu callback, since
;            it also does GotoFirstMenu.
;---------------------------------------------------------------
closeMenuCloseFile:
	jsr     GotoFirstMenu
        jsr     clearSelection
        jsr     _syncDocToDisk
        jmp     _CloseRecordFile

;---------------------------------------------------------------
; tmpCloseDocFile
;
; Function:  Close the current document file temporarily. This
;            is called when reading/writing text and photo
;            scraps in order to make the GEOS KERNAL's global
;            VLIR file state available for a different file.
;---------------------------------------------------------------
tmpCloseDocFile:
	jsr     setDocDrive
        MoveB   curRecord, tmpCurRecord
        jmp     _CloseRecordFile

;---------------------------------------------------------------
; reopenDocFile
;
; Function:  Reopen the document file that was closed with
;            tmpCloseDocFile.
;---------------------------------------------------------------
reopenDocFile:
	jsr     setDocDrive
	LoadW   r0, otherFnBuffer
        jsr     swapUserZp
        jsr     OpenRecordFile
        jsr     swapUserZp
        lda     tmpCurRecord
        jmp     PointRecord

;---------------------------------------------------------------
; testDiskNotFull
;
; Function:  Make sure there at least 6 KB of free space on the
;            document disk. Otherwise, show a message and quit
;            the application.
;            geoWrite is designed to never experience a
;            disk-full error. If an operation caused disk-full,
;            it would be very tricky to recover from it without
;            the document in an inconsistent state. Therefore,
;            the app checks whether there is enough space
;            at a safe checkpoint before it starts an operation
;            may will allocate space.
;
; Note:      This does not sync the document back to disk if
;            the disk is indeed near full and the app exits.
;            The caller must call "syncPageToDisk" beforehand,
;            if the state in memory is dirty.
;---------------------------------------------------------------
testDiskNotFull:
	jsr     setDocDrive
        jsr     swapUserZp
        jsr     GetDirHead
        jsr     swapUserZp
testDiskNotFull2:
	LoadW   r5, curDirHead
        jsr     CalcBlksFree
        lda     r4H
        bne     @rts
        lda     r4L
        cmp     #6*4 ; < 6 KB = disk near full
        bcs     @rts
        jmp     _diskNearFullExit
@rts:	rts

;---------------------------------------------------------------
; setAppDrive
;
; Function:  Switch to the drive that contains the app.
;---------------------------------------------------------------
setAppDrive:
	lda     appDrive
        bne     _1			; always
;---------------------------------------------------------------
; setDocDrive
;
; Function:  Switch to the drive that contains the document.
;---------------------------------------------------------------
setDocDrive:
	lda     docDrive
_1:	jsr     swapUserZp
        jsr     SetDevice
        jmp     swapUserZp

; ----------------------------------------------------------------------------
syncAndloadCode78:
	jsr     clearSelection
        jsr     killPrompt
        jsr     _syncDocToDisk
loadCode78:
	LoadW_y r7, MEM_OVERLAY_PRT
        lda     #RECORD_8
        jsr     loadAppRecord

        lda     #RECORD_7
        .byte   $2C
loadCode6:
	lda     #RECORD_6
        .byte   $2C
loadCode5:
	lda     #RECORD_5
        .byte   $2C
; $0E89 [XXX unused]
        lda     #RECORD_3
        .byte   $2C
loadCode4:
	lda     #RECORD_4
loadCode:
	LoadW_y r7, MEM_OVERLAY		; target address
loadAppRecord:
	MoveW_y r7, r10
        cmp     curCodeRecord
        beq     @rts 			; already loaded
        sta     curCodeRecord
        jsr     setAppDrive
        lda     curCodeRecord
@again: asl     a
        tay
        lda     appIndexTable,y
        sta     r1L
        lda     appIndexTable+1,y
        sta     r1H
	LoadW   r2, MEM_SIZE_OVERLAY
        jsr     _ReadFile
        bnex    @error
@rts:   rts
@error: PushW   r10
        jsr     GotoFirstMenu
        lda     #<txt_swapping_geowrite
        ldy     #>txt_swapping_geowrite
        jsr     showIOError
	PopW    r7
        lda     curCodeRecord
        jmp     @again ; try again

rts1 = @rts

;---------------------------------------------------------------
; appPrint2
;
; Function:  Jumped to by record 1 init code if app is opened
;            for printing a document.
;---------------------------------------------------------------
appPrint2:
	jsr     loadCode5
        jsr     J5_readPageProperties
        jsr     loadCode78
        jmp     J7_2

;---------------------------------------------------------------
; callbackPrint
;
; Function:  Callback for "file -> print".
;---------------------------------------------------------------
callbackPrint:
        jsr     GotoFirstMenu
        lda     curPage
        cmp     #PAGE_HEADER
        bcs     rts1
        jsr     pushRulerData
        jsr     syncAndloadCode78
        jsr     J7_print
printPreviewCommon:
	jsr     _readPage
        jsr     L15CA
        jsr     popRulerData
        jmp     L0781

;---------------------------------------------------------------
; callbackPreview
;
; Function:  Callback for "file -> preview".
;---------------------------------------------------------------
callbackPreview:
        jsr     GotoFirstMenu
        lda     curPage
        cmp     #PAGE_HEADER
        bcs     rts1
        jsr     pushRulerData
        jsr     syncAndloadCode78
        jsr     J7_preview
        jmp     printPreviewCommon

;---------------------------------------------------------------
; callbackRename
;
; Function:  Callback for "file -> rename".
;---------------------------------------------------------------
callbackRename:
        ldy     #<J5_renameDocument
        .byte   $2C
;---------------------------------------------------------------
; _showAboutDialog
;
; Function:  Callback for "geos -> info". Shows the about
;            dialog.
;---------------------------------------------------------------
_showAboutDialog:
        ldy     #<J5_showAboutDialog
        .byte   $2C
;---------------------------------------------------------------
; _loadDeskAcc
;
; Function:  Callback for "geos -> <desk accessory>". Loads and
;            executes the selected desk accessory.
;---------------------------------------------------------------
_loadDeskAcc:
	ldy     #<J5_loadDeskAcc
        ldx     #RECORD_5
        jmp     callLoadCode2

;---------------------------------------------------------------
; callbackRecover
;
; Function:  Callback for "file -> recover"
;---------------------------------------------------------------
callbackRecover:
        jsr     loadCode5
        jsr     J5_recover
        bit     zp_DDb
        bmi     readPageAndRestartEditor
        jmp     loadCode2

;---------------------------------------------------------------
; restart
;
; Function:  Show startup menu.
;---------------------------------------------------------------
restart:
	sei
        ldx     #$FF
        txs
        lda     #<(Panic-1)		; if app ever returns
        pha
        lda     #>(Panic-1)
        pha
        cli
        jsr     loadCode5
        jsr     J5_showStartupMenu
repaginateAndRestartEditor:
	jsr     loadCode6
        jsr     J6_updatePageSizes
        jsr     J6_updateTitleNlqMenuSelection
readPageAndRestartEditor:
	jsr     _readPage
        jmp     editor

;---------------------------------------------------------------
; openDocumentRepaginateEdit
;
; Function:  ...
;---------------------------------------------------------------
openDocumentRepaginateEdit:
	jsr     loadCode5
        jsr     J5_openDocument
        jmp     repaginateAndRestartEditor

;---------------------------------------------------------------
; callbackRecover
;
; Function:  Callback for "file -> quit"
;---------------------------------------------------------------
callbackQuit:
        jsr     closeMenuCloseFile
_exitToDesktop:
	jsr     loadCode5
        jmp     J5_exitToDesktop

;---------------------------------------------------------------
; callbackMakeFullPageWide
;
; Function:  Callback for "options -> make full page wide"
;---------------------------------------------------------------
callbackMakeFullPageWide:
        jsr     GotoFirstMenu
        CmpBI   curPage, PAGE_HEADER
        bcs     rts5
        jsr     _syncDocToDisk
        jsr     loadCode5
        jsr     J5_makeFullPageWide
        jsr     _readPage
        jmp     readPageAndRestartEditor

;---------------------------------------------------------------
; Record 3 Wrappers
;   Call code on record 3, load code 2.
;---------------------------------------------------------------
_diskNearFullExit:
	ldy     #<J3_diskNearFullExit
        .byte   $2C
_copy:
        ldy     #<J3_copy
        .byte   $2C
_hidePictures:
        ldy     #<J3_hidePictures
        .byte   $2C
_addPhotoScrapToDoc:
	ldy     #<J3_addPhotoScrapToDoc
        ldx     #RECORD_3
        jmp     callLoadCode2

;---------------------------------------------------------------
; Record 3 Wrappers
;   Call code on record 3, load code 2, re-render
;---------------------------------------------------------------
_pasteText:
        ldy     #<J3_pasteText
        .byte   $2C
_cut:
        ldy     #<J3_cut
        ldx     #RECORD_3
        jmp     executeThenRender

;---------------------------------------------------------------
; callbackRuler
;
; Function:  Callback for mouse clicks on the ruler tick marks.
;---------------------------------------------------------------
callbackRuler:
	bit     zp_DBb
        bpl     rts5
        jsr     loadCode4
        jsr     J4_callbackRuler

record4Finished:
	bit     zp_DBb
        bmi     rts5
        jsr     setKeyVector
	LoadW   otherPressVec, J2_appOtherPressVec
        jsr     loadCode2
rts5:	rts

;---------------------------------------------------------------
; callbackMarginBar
;
; Function:  Callback for mouse clicks into the ruler area that
;            contains the M, P and tab symbols.
;---------------------------------------------------------------
callbackMarginBar:
	jsr     cmpCursor0_1Ptr		; selection?
        bne     @1

        bit     cursor0+cursor::srcline
        bmi     rts5
        bvs     rts5

@1:	jsr     loadCode4
        jsr     J4_callbackMarginBar
        jmp     record4Finished

;---------------------------------------------------------------
; _editPageIndicator
;
; Function:  Callback for mouse clicks into the page indicator.
;            Moved the window into the page shown on the screen.
;---------------------------------------------------------------
_editPageIndicator:
	jsr     loadCode4
        jsr     J4_editPageIndicator
        jmp     record4Finished

;---------------------------------------------------------------
; Record 3 Wrappers
;---------------------------------------------------------------
_showCantAddPages:
	ldy     #<J3_showCantAddPages
        .byte   $2C
_showTooManyPages:
	ldy     #<J3_showTooManyPages
        .byte   $2C
_splitTooBigPage:
	ldy     #<J3_splitTooBigPage
        ldx     #RECORD_3
callRestore:
	lda     curCodeRecord
        pha
        sty     @1
        txa
        jsr     loadCode
@1 = * + 1
        jsr     MEM_OVERLAY
        pla
        jmp     loadCode

;---------------------------------------------------------------
; Record 6 Wrappers
;---------------------------------------------------------------
_previousPage:
        ldy     #<J6_previousPage
        .byte   $2C
_nextPage:
        ldy     #<J6_nextPage
        .byte   $2C
_gotoPage:
        ldy     #<J6_gotoPage
        .byte   $2C
_titlePage:
        ldy     #<J6_titlePage
        .byte   $2C
_openHeader:
        ldy     #<J6_openHeader
        .byte   $2C
_openFooter:
        ldy     #<J6_openFooter
        jsr     call6LoadCode2
        jmp     editor

_search:
        ldy     #<J6_search
        .byte   $2C
_change:
        ldy     #<J6_change
        .byte   $2C
_findNext:
        ldy     #<J6_findNext
        .byte   $2C
_setFirstPage:
        ldy     #<J6_setFirstPage
        ldx     #RECORD_6
executeThenRender:
	sty     @1
        txa
        jsr     loadCode
@1 = * + 1
        jsr     MEM_OVERLAY
        jsr     loadCode2
        jsr     J2_render_XXX
        jmp     J2_analyzeSelectionFont

;---------------------------------------------------------------
; Record 2 Wrappers
;---------------------------------------------------------------
_readNextPage:
	ldy     #<J6_readNextPage
        .byte   $2C
_readPage:
	ldy     #<J6_readPage
        .byte   $2C
_deleteAcrossPages:
	ldy     #<J6_deleteAcrossPages
call6LoadCode2:
	ldx     #RECORD_6
callLoadCode2:
	pha
        sty     @1
        txa
        jsr     loadCode
        pla
@1 = * + 1
        jsr     MEM_OVERLAY
loadCode2:
	pha
        php
        lda     #RECORD_2
        jsr     loadCode
        plp
        pla
        rts

;---------------------------------------------------------------
; callbackUpdate
;
; Function:  Callback for "file -> update"
;---------------------------------------------------------------
callbackUpdate:
        jsr     loadCode6
        jsr     pushRulerData
        jsr     J6_update
        jsr     popRulerData
        jsr     loadCode2
        jmp     L0781

;---------------------------------------------------------------
; callbackNLQSpacing
;
; Function:  Callback for "page -> NLQ spacing"
;---------------------------------------------------------------
callbackNLQSpacing:
        jsr     loadCode6
        jsr     J6_NLQSpacing
        jmp     repaginateAndRestartEditor

;---------------------------------------------------------------
; Misc Record Wrappers
;---------------------------------------------------------------
_streamBlock:
	ldy     #<J6_streamBlock                    ; 106A A0 4D                    .M
        .byte   $2C                             ; 106C 2C                       ,
_updateDocument:
	ldy     #<J6_updateDocument                   ; 106D A0 56                    .V
        ldx     #RECORD_6                         ; 106F A2 06                    ..
        jmp     callRestore                     ; 1071 4C E6 0F                 L..

_syncDocToDisk:
	jsr     loadCode6                       ; 1074 20 83 0E                  ..
        jmp     J6_syncDocToDisk                       ; 1077 4C 59 32                 LY2

;---------------------------------------------------------------
; measureLine
;
; Function:  Iterate over all characters until the end of the
;            line (or page) and collect metrics information.
;
; Returns:   c  =1: page end encountered
;---------------------------------------------------------------
measureLine:
	lda     #0
        sta     zp_BAb

@loop1: MoveB	justification, tmpJustification2
        PushW_r15
        jsr     copyIntoCursor3_XXX
        jsr     getByteIntpNewCardSet
        cmp     #ESC_RULER
        bne     @skip1

        jsr     readRuler		; read and set ruler

@skip1:	lda     tmpJustification2
        and     #JST_20
        beq     @skip2

        lda     #JST_10		; flag: first line of paragraph
        ora     justification
        sta     justification

@skip2:	jsr     getEffectiveLeftMargin
        lda     r11L
        sta     zp_BBw
        sta     zp_AAw
        lda     r11H
        sta     zp_BBw+1
        sta     zp_AAw+1
        jsr     getByteIntpNewCardSet
        cmp     #ESC_GRAPHICS
        bne     @loop2

        jsr     measurePicture
        PopW_r15
        lda     #ESC_GRAPHICS
        sta     r1L
        clc
        rts				; ESC_GRAPHICS -> rts

@loop2:	jsr     getByteIntpNewCardSet	; printable character
        sta     r1L
        jsr     measureChar   		; <---------------!
        lda     r1L
        jsr     isSpaceOrControl
        bcs     @1			; yes, don't check for line overflow

        ldx     #r3
        jsr     getEffectiveRightMargin
        CmpW	r11, r3
	beq     :+
        bcs     @8			; right margin reached -> return
:	MoveW   r11, zp_BBw

@1:	bit     zp_BAb
        bmi     @2

        lda     r1L
        jsr     isSpaceOrControl
        bcc     @3			; printable? increment and loop

@2:	MoveW   zp_BBw, zp_AAw
        MoveW   curFont, tmpFont2
        MoveB   currentMode, tmpMode2
        MoveB   lineMaxFontHeight, cursor3+cursor::height
        MoveB   lineMaxBaselineOffset, tmpBaselineOffset
        jsr     moveR15_cursor3Ptr
        lda     r1L
        beq     @9
        cmp     #PAGE_BREAK
        beq     @9			; page end
        cmp     #ESC_GRAPHICS
        beq     @7
        cmp     #GOTOX			; [XXX impossible, see isSpaceOrControl filter]
        beq     @4

	IncW    cursor3+cursor::ptr
	lda     r1L
        jsr     incWR15

        cmp     #CR
        beq     @5
        jmp     @loop2			; unknown non-printable -> next char

@3:	jsr     incWR15			; printable: advance to next char
        jmp     @loop2
					; *** GOTOX
@4:	ldy     #1
        lda     (r15),y			; get argument -> ???
        beq     @6
					; *** CR
@5:	jsr     @setJustificationFlag
@6:	PopW_r15
        clc				; flag: line end
        php
        bcc     @10
					; ESC_GRAPHICS
@7:	jsr     @setJustificationFlag
					; *** right margin reached
@8:	PopW_r15
        clc				; flag: line end
        php
        jsr     CmpR15Cursor3Ptr
        bne     @10			; not end of page data

        plp				; -> ???
        jsr     setMetricsFromCursor3
        MoveB   tmpJustification2, justification
        LoadB   zp_BAb, $FF
        jmp     @loop1
					; *** page end
@9:	jsr     @setJustificationFlag
        PopW_r15
        sec				; flag: page end
        php
					; *** line end
@10:	PushB   r1L
        ldx     #r2
        jsr     getEffectiveRightMargin
        lda     r2L			; zp_AAw = r2 - zp_AAw
        sub     zp_AAw
        sta     zp_AAw
        lda     r2H
        sbc     zp_AAw+1
        sta     zp_AAw+1
        jsr     countLineChars
        jsr     calcSpacedLineHeight
	MoveW   tmpFont2, curFont
        MoveB   tmpMode2, currentMode
        jsr     loadFontMetrics
        lda     justification
        and     #JST_10
        asl     a
        sta     r0L
        lda     justification
        and     #$FF-JST_20
        ora     r0L
        sta     rulerData1+ruler::justification
        PopB    r1L
        plp
        bcs     @rts			; page end -> rts

	CmpWI   cursor3+cursor::ptr, MEM_PAGE + MEM_SIZE_PAGE - 400
	bcc     @rts			; close to end of page buffer?

        bit     streamingMode
        bpl     @rts

        jsr     pushR0ToR14
        PushW_r15
        MoveB   r1L, r14L
        jsr     _splitTooBigPage
        PopW_r15
        jsr     popR0ToR14
        sec
        LoadB   r1L, ' '

@rts:	rts

@setJustificationFlag:
	lda     #JST_20
        ora     justification
        sta     justification
        rts

setMetricsFromCursor3:
	MoveW   cursor3+cursor::font, curFont
        MoveB   cursor3+cursor::mode, currentMode
        jmp     loadFontMetrics

;---------------------------------------------------------------
; readRuler
;
; Function:  Read ruler data from page into ruler1.
;
; Pass:      r15     page pointer
;
; Return:    ruler1  ruler data
;---------------------------------------------------------------
readRuler:
	jsr     incWR15
        PushW_r15			; original ruler pointer in text
        ldy     #0
@loop:	tya				; read margin and tabs into rulerData1
        pha
        jsr     getByteFromPageInc
        tax
        pla
        tay
        txa
        sta     rulerData1,y
        iny
        cpy     #ruler::justification
        bne     @loop

        jsr     getByteFromPageInc	; justification
        and     #$FF-JST_STARTS_WITH_RULER-JST_20
        pha
        CmpWI   r15, PAGE_CARDSET	; was this the *page* ruler?
	pla
        bcc     :+
        and     #$FF-JST_10
:	ora     #JST_STARTS_WITH_RULER
        sta     rulerData1+ruler::justification
        lda     justification
        and     #$FF-MASK_JST_SPACING-MASK_JST_JUST
        ora     rulerData1+ruler::justification
        sta     justification
        sta     rulerData1+ruler::justification
        jsr     fixupMargins
        PopW	r2			; original ruler pointer in text
        ldy     #ruler::left_margin	; store left margin in text
        jsr     moveWRulerDataYR2
        ldy     #ruler::paragraph_margin; store paragraph margin in text
        jsr     moveWRulerDataYR2

        jsr     getByteFromPageInc	; text color (unused)
        jsr     getByteFromPageInc	; unused
					; and below: unused
;---------------------------------------------------------------
; getByteFromPageInc
;
; Function:  Get a single byte from the page data without
;            doing any interpretation; advance page pointer.
;
; Pass:      r15  page pointer
;
; Return:    a    byte
;---------------------------------------------------------------
getByteFromPageInc:
	jsr     getByteFromPage
        jmp     incWR15

;---------------------------------------------------------------
moveWRulerDataYR2:
	jsr     moveBRulerDataYR2
moveBRulerDataYR2:
	lda     rulerData1,y
        sta     (r2),y
        iny
        rts

;---------------------------------------------------------------
; fixupMargins
;
; Function:  If left or paragraph margins are further to the
;            right than the right margin, adjust them.
;
; Pass:      ruler1  ruler to work on
;---------------------------------------------------------------
fixupMargins:
	ldx     #r2
        jsr     getEffectiveRightMargin
        lda     r2L
        sub     #80
        and     #$F8			; round down to 8 dots
        tax
        lda     r2H
        sub     #0
        cmp     rulerData1+ruler::left_margin+1
        bne     :+
        cpx     rulerData1+ruler::left_margin
:	bcs     @1			; right margin >= left margin -> skip

        sta     rulerData1+ruler::left_margin+1	; otherwise left margin = right margin
        stx     rulerData1+ruler::left_margin

@1:	cmp     rulerData1+ruler::paragraph_margin+1
        bne     :+
        cpx     rulerData1+ruler::paragraph_margin
:	bcs     @rts			; right margin >= paragraph margin -> skip

        sta     rulerData1+ruler::paragraph_margin+1	; paragraph margin = right margin
        stx     rulerData1+ruler::paragraph_margin
@rts:	rts

;---------------------------------------------------------------
; measurePicture
;
; Function:  Read graphics escape from text, return width and
;            height.
;
; Return:    tmpLineWidth          width
;            cursor3.cursor::height  height
;---------------------------------------------------------------
measurePicture:
	jsr     incWR15
        jsr     getByteFromPageInc	; width
        sta     tmpLineWidth
        jsr     getByteFromPageInc	; height lo
        sta     cursor3+cursor::height
        jsr     getByteFromPageInc	; height hi - ignore (max supported height is 144)
        jsr     getByteFromPage		; record number
        sta     zp_AAw
	AddVW2	1, r15, cursor3+cursor::ptr
        lda     justification
        ora     #JST_80
        and     #$FF-JST_10
        sta     justification
        rts

;---------------------------------------------------------------
copyIntoCursor3_XXX:
	jsr     moveR15_cursor3Ptr
        lda     curFont
        sta     cursor3+cursor::font
        sta     tmpFont2
        lda     curFont+1
        sta     cursor3+cursor::font+1
        sta     tmpFont2+1
        MoveB   currentMode, cursor3+cursor::mode
        sta     tmpMode2
        lda     #0
        sta     cursor3+cursor::height
        sta     tmpBaselineOffset
        sta     lineMaxFontHeight
        sta     lineMaxBaselineOffset
        sta     tmpLineWidth
        lda     rulerData1+ruler::justification
        and     #MASK_JST_SPACING+MASK_JST_JUST
        sta     justification
        rts

;---------------------------------------------------------------
; measureChar
;
; Function:  Advance cursor position based on character and
;            mode, keep track of max. baselineOffset and height
;            of the current line.
;
; Pass:      r1L  character code
;            r11  x position
;
; Return:    r11  new x position
;---------------------------------------------------------------
measureChar:
	ldx     currentMode
        jsr     calcEffectiveBaselineOffset2
        cmp     lineMaxBaselineOffset
        bcc     @1
        sta     lineMaxBaselineOffset	; remember largest seen baselineOffset
@1:	lda     r1L
        ldx     currentMode
        jsr     calcEffectiveFontHeight
        cmp     lineMaxFontHeight
        bcc     @2
        sta     lineMaxFontHeight	; remember largest seen fontHeight
@2:	lda     r1L			; character
        cmp     #TAB
        bne     @4

        jsr     incWR15			; advance to tab stop
        jsr     getNextTabStop
	DecW	r15
        bra     @rts

@4:	cmp     #$20
        bcc     @rts			; not printable

        ldx     currentMode		; add character width to r11
        jsr     getCharWidth
        add     r11L
        sta     r11L
        bcc     @rts
        inc     r11H
@rts:	rts

;---------------------------------------------------------------
; countLineChars
;
; Function:  Counts the number of characters in a region of text
;            until the last space.
;
; Pass:      r15                   start
;            cursor3.cursor::ptr  end
;
; Return:    tmpLineWidth          result
;---------------------------------------------------------------
countLineChars:
	PushW_r15
        lda     #0
        sta     r11L
        sta     tmpLineWidth
        lda     justification
        and     #JST_20
        bne     @end

@loop:  jsr     CmpR15Cursor3Ptr
        bcs     @end
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics
        cmp     #' '
        bne     @1
        inc     r11L
        bra     @loop

@1:	lda     r11L
        sta     tmpLineWidth
        bra     @loop

@end:	PopW_r15
        rts

;---------------------------------------------------------------
; calcSpacedLineHeight
;
; Function:  Apply line spacing to a line's height, i.e.
;            calculate a line's effective height from the line's
;            character height.
;---------------------------------------------------------------
calcSpacedLineHeight:
	inc     tmpBaselineOffset	; baselineOffset += 2
        inc     cursor3+cursor::height	; height += 2
        inc     tmpBaselineOffset
        inc     cursor3+cursor::height
        lda     justification
        and     #MASK_JST_SPACING
        beq     @2			; 1.0
        cmp     #1 << SHIFT_JST_SPACING
        beq     @1			; 1.5
        lda     cursor3+cursor::height
        bne     @3			; 2

@1:	lda     cursor3+cursor::height
        lsr     a
        bne     @3

@2:	lda     #0

@3:	sta     r0L
        add     cursor3+cursor::height
        sta     cursor3+cursor::height
	AddB__  r0L, tmpBaselineOffset
        rts

;---------------------------------------------------------------
; isSpaceOrControl
;
; Function:  Test whether a character is one of:
;            * SPACE, TAB, CR
;            * ESC_GRAPHICS, GOTOX
;            * PAGE_BREAK
;            * NULL
;
; Pass:      a  character
;
; Return:    c  =1: yes
;---------------------------------------------------------------
isSpaceOrControl:
	cmp     #' '
        beq     secRts0
;---------------------------------------------------------------
; isControlCharacter
;
; Function:  Test whether a character is one of:
;            * TAB, CR
;            * ESC_GRAPHICS, GOTOX
;            * PAGE_BREAK
;            * NULL
;
; Pass:      a  character
;
; Return:    c  =1: yes
;---------------------------------------------------------------
isControlCharacter:
	cmp     #TAB
        beq     secRts0
        cmp     #ESC_GRAPHICS
        beq     secRts0
        cmp     #GOTOX
        beq     secRts0
;---------------------------------------------------------------
; isCrPageBreakOrNull
;
; Function:  Test whether a character is one of:
;            * CR
;            * PAGE_BREAK
;            * NULL
;
; Pass:      a  character
;
; Return:    c  =1: yes
;---------------------------------------------------------------
isCrPageBreakOrNull:
	cmp     #CR
        beq     secRts0
;---------------------------------------------------------------
; isPageBreakOrNull
;
; Function:  Test whether a character is one of:
;            * PAGE_BREAK
;            * NULL
;
; Pass:      a  character
;
; Return:    c  =1: yes
;---------------------------------------------------------------
isPageBreakOrNull:
	cmp     #0
        beq     secRts0
        cmp     #PAGE_BREAK
        beq     secRts0
        clc
        rts

secRts0:
	sec
        rts

;---------------------------------------------------------------
; getNextTabStop
;
; Function:  Get the position of the next tab stop from a
;            given position.
;
; Pass:      r11 position
;
; Return:    r11 position of next tab stop
;---------------------------------------------------------------
getNextTabStop:
	lda     rulerData1+ruler::justification
        and     #MASK_JST_JUST
        bne     @rts			; only left aligned honors tab stops

        ldx     #0			; find first tab stop less than r11
@loop1:	lda     rulerData1+ruler::tabs+1,x
        and     #$7F
        cmp     r11H
        bne     @1
        lda     rulerData1+ruler::tabs,x
        cmp     r11L
@1:	beq     @2
        bcs     @found
@2:	inx
        inx
        cpx     #16
        bne     @loop1
        rts

@found:	txa				; tab stop index
        pha
        ldx     #r3
        jsr     getEffectiveRightMargin
        pla
        tax
        lda     rulerData1+ruler::tabs+1,x
        and     #$7F
        cmp     r3H
        bne     @3
        lda     rulerData1+ruler::tabs,x
        cmp     r3L
@3:	bcs     @rts			; tab is beyond right margin, ignore

        lda     rulerData1+ruler::tabs+1,x
        bmi     @decimal		; tab stop type = decimal

        sta     r11H
        lda     rulerData1+ruler::tabs,x
        sta     r11L
@rts:	rts

; decimal tab stop
@decimal:
	lda     rulerData1+ruler::tabs,x; r2 = distance to tab stop
        sub     r11L
        sta     r2L
        lda     rulerData1+ruler::tabs+1,x
        and     #$7F			; kill tab stop type flag in bit 7
        sbc     r11H
        sta     r2H
        PushW	curFont
        PushB   currentMode
        PushW_r15
@loop2:	PushW	r2
        PushB   r1L
        jsr     getByteIntpNewCardSet
        jsr     incWR15
        sta     r3L
        PopB	r1L
        PopW	r2
        lda     r3L
        jsr     isControlCharacter
        beq     @done

        cmp     #DECIMAL_SEPARATOR
        beq     @done

        ldx     currentMode
        jsr     getCharWidth
        sta     r3L
        lda     r2L			; distance -= char width
        sub     r3L
        sta     r2L
        lda     r2H
        sbc     #0
        sta     r2H
        bcs     @loop2

        LoadW	r2, 0			; left of decimal point overflowed available space

@done:  AddW	r2, r11			; skip this many pixels
        PopW_r15
        PopB    currentMode
        PopW	curFont
        jmp     loadFontMetrics

;---------------------------------------------------------------
; getByteIntpNewCardSet
;
; Function:  Get a single byte from the page data without
;            advancing the page pointer. NEWCARDSET control code
;            will be interpreted, ruler and graphics escape
;            codes will be returned verbatim.
;
; Pass:      r15  page pointer
;
; Return:    a    byte
;---------------------------------------------------------------
getByteIntpNewCardSet:
	jsr     getByteFromPage
        cmp     #NEWCARDSET
        beq     @1
        rts

@1:	jsr     incWR15
        jsr     getByteFromPageInc
        sta     curFont			; font id lo
        jsr     getByteFromPageInc
        sta     curFont+1		; font id hi
        jsr     getByteFromPageInc
        sta     currentMode		; mode
        jsr     loadFontMetrics
        bra     getByteIntpNewCardSet	; again

;---------------------------------------------------------------
; getByteFromPage
;
; Function:  Get a single byte from the page data without
;            doing any interpretation; don't advance page
;            pointer.
;
; Pass:      r15  page pointer
;
; Return:    a    byte
;---------------------------------------------------------------
getByteFromPage:
	jsr     cmpPageEndPtr2R15	; end reached
        bcc     @end			; yes
        ldy     #0
        lda     (r15),y			; read byte
        rts

@end:	bit     streamingMode
        bpl     @skip

        jsr     pushR0ToR14		; push *all* registers
        PushW_r15
        jsr	_streamBlock
        PopW_r15
        jsr     popR0ToR14

        bra     getByteFromPage		; try again

@skip:  lda     #0
        rts

; ----------------------------------------------------------------------------
getByteIntpNewCardSetSkipEscRulerEscGraphics:
	jsr     getByteIntpNewCardSet

skipEscRulerEscGraphics:
	cmp     #ESC_RULER
        beq     @1
        jsr     isPageBreakOrNull
        bcs     @rts
        jsr     incWR15
        cmp     #ESC_GRAPHICS
        bne     @rts
        jsr     addWI4R15		; skip graphics escape
        lda     #ESC_GRAPHICS
@rts:	rts

@1:	jsr     addRulerSizeToR15	; skip ruler escape
        lda     #ESC_RULER
        rts

; ----------------------------------------------------------------------------
L14BD:  jsr     CmpR15Cursor3Ptr                           ; 14BD 20 2A 28                  *(
        bcs     @1                           ; 14C0 B0 19                    ..
        jsr     getByteFromPage                           ; 14C2 20 7A 14                  z.
        cmp     #NEWCARDSET                            ; 14C5 C9 17                    ..
        bne     @2                           ; 14C7 D0 1C                    ..
        lda     r15L                            ; 14C9 A5 20                    . 
        add     #4                            ; 14CC 69 04                    i.
        ldy     r15H                            ; 14CE A4 21                    .!
        bcc     :+                           ; 14D0 90 01                    ..
        iny                                     ; 14D2 C8                       .
:	cpy     cursor3+cursor::ptr+1                        ; 14D3 C4 9F                    ..
        bne     :+                           ; 14D5 D0 02                    ..
        cmp     cursor3+cursor::ptr                        ; 14D7 C5 9E                    ..
:	bcc     @2                           ; 14D9 90 0A                    ..

@1:	MoveW_x	r11, r2                         ; 14E1 86 06                    ..
        sec                                     ; 14E3 38                       8
        rts                                     ; 14E4 60                       `

@2:	jsr     getByteIntpNewCardSet
        sta     r1L                             ; 14E8 85 04                    ..
        ldx     #0                            ; 14EA A2 00                    ..
        stx     r10L                            ; 14EC 86 16                    ..
        stx     r10H                            ; 14EE 86 17                    ..
        jsr     incWR15                           ; 14F0 20 1E 27                  .'
        cmp     #CR                            ; 14F3 C9 0D                    ..
        beq     @1                           ; 14F5 F0 E4                    ..
        cmp     #TAB                            ; 14F7 C9 09                    ..
        beq     L1568                           ; 14F9 F0 6D                    .m
        ldx     #0                            ; 14FB A2 00                    ..
        cmp     #$20                            ; 14FD C9 20                    . 
        bcc     @4                           ; 14FF 90 2D                    .-
        pha                                     ; 1501 48                       H
        ldx     currentMode                     ; 1502 A6 2E                    ..
        jsr     getCharWidth                           ; 1504 20 C3 25                  .%
        sta     r10L                            ; 1507 85 16                    ..
        pla                                     ; 1509 68                       h
        tax                                     ; 150A AA                       .
        cpx     #$20                            ; 150B E0 20                    . 
        bne     @4                           ; 150D D0 1F                    ..
        lda     r10L                            ; 150F A5 16                    ..
        add     zp_BDw                        ; 1512 65 BD                    e.
        sta     r10L                            ; 1514 85 16                    ..
        lda     r10H                            ; 1516 A5 17                    ..
        adc     #0                            ; 1518 69 00                    i.
        sta     r10H                            ; 151A 85 17                    ..
        lda     zp_BDw+1                        ; 151C A5 BE                    ..
        beq     @3                           ; 151E F0 08                    ..
        inc     r10L                            ; 1520 E6 16                    ..
        bne     :+                           ; 1522 D0 02                    ..
        inc     r10H                            ; 1524 E6 17                    ..
:	dec     zp_BDw+1                        ; 1526 C6 BE                    ..
@3:	bit     currentMode                     ; 1528 24 2E                    $.
        bmi     @4                           ; 152A 30 02                    0.
        ldx     #0                            ; 152C A2 00                    ..
@4:	AddW3	r11, r10, r2
        txa                                     ; 153B 8A                       .
        pha                                     ; 153C 48                       H
        ldx     #r3                            ; 153D A2 08                    ..
        jsr     getEffectiveRightMargin                           ; 153F 20 F6 26                  .&
        pla                                     ; 1542 68                       h
        tax                                     ; 1543 AA                       .
	CmpW	r3, r2
	bcs     :+                           ; 154E B0 15                    ..
	MoveW	r3, r2
	SubW3	r2, r11, r10
:	clc                                     ; 1565 18                       .
        txa                                     ; 1566 8A                       .
        rts                                     ; 1567 60                       `

; ----------------------------------------------------------------------------
L1568:  PushW	r11                             ; 156D 48                       H
        jsr     getNextTabStop                           ; 156E 20 98 13                  ..
        MoveW   r11, r2
        PopW	r11
	SubW3	r2, r11, r10
        clc                                     ; 158C 18                       .
        lda     #0                            ; 158D A9 00                    ..
        rts                                     ; 158F 60                       `

;---------------------------------------------------------------
; getEffectiveLeftMargin
;
; Function:  Returns the effective left margin for this line,
;            considering whether it's the first line of the
;            paragraph.
;
; Returns:   r11  left margin
;---------------------------------------------------------------
getEffectiveLeftMargin:
	ldx     rulerData1+ruler::left_margin
        ldy     rulerData1+ruler::left_margin+1
        lda     justification
        and     #JST_10			; first line of paragraph?
        beq     @1
        ldx     rulerData1+ruler::paragraph_margin
        ldy     rulerData1+ruler::paragraph_margin+1
@1:	stx     r11L
        sty     r11H
        rts

; ----------------------------------------------------------------------------
L15A7:  MoveW   topScrTxtPtr, r15
        LoadB   r1H, $24
        MoveB   tmpMode3, currentMode                     ; 15B5 85 2E                    ..
        MoveW   tmpFont3, curFont
        jsr     copyRuler2To1                           ; 15BF 20 94 28                  .(
        jsr     loadFontMetrics                           ; 15C2 20 D9 24                  .$

setCurLineZero:
	LoadB_x curLine, 0
        rts                                     ; 15C9 60                       `

; ----------------------------------------------------------------------------
L15CA:  jsr     L15A7                           ; 15CA 20 A7 15                  ..
        jsr     L1B5E                           ; 15CD 20 5E 1B                  ^.

L15D0:  jsr     updatePageIndicator                           ; 15D0 20 97 05                  ..
        jsr     setCurLineZero                           ; 15D3 20 C5 15                  ..
        lda     #$FF                            ; 15D6 A9 FF                    ..

L15D8:  ldx     #0                            ; 15D8 A2 00                    ..
        stx     a6L                             ; 15DA 86 78                    .x
        stx     a6H                             ; 15DC 86 79                    .y
        tax                                     ; 15DE AA                       .
        bpl     L15EE                           ; 15DF 10 0D                    ..

        ldx     curLine                        ; 15E1 A6 D2                    ..
        jsr     getLineRuler                           ; 15E3 20 C9 1D                  ..
        ldx     curLine                        ; 15E6 A6 D2                    ..
        lda     lintab_y,x                         ; 15E8 BD 60 2B                 .`+
        jsr     clearPartOfScreen                           ; 15EB 20 6D 16                  m.

L15EE:  ldx     curLine                        ; 15EE A6 D2                    ..
        lda     lintab_height,x                         ; 15F0 BD 75 2B                 .u+
        beq     L15FC                           ; 15F3 F0 07                    ..
        jsr     L177F                           ; 15F5 20 7F 17                  ..
        inc     curLine                        ; 15F8 E6 D2                    ..
        bne     L15EE                           ; 15FA D0 F2                    ..
L15FC:  ldx     curLine                        ; 15FC A6 D2                    ..
        jsr     moveLintabR15                           ; 15FE 20 71 18                  q.
        ldy     #0                            ; 1601 A0 00                    ..
        lda     (r15),y                        ; 1603 B1 20                    .
        cmp     #PAGE_BREAK                            ; 1605 C9 0C                    ..
        bne     L1632                           ; 1607 D0 29                    .)
        dex                                     ; 1609 CA                       .
        bmi     L1632                           ; 160A 30 26                    0&
        jsr     setCursor3AndJustificationFromLine                           ; 160C 20 D7 16                  ..
        jsr     measureLine                           ; 160F 20 7A 10                  z.
        bcc     L1632                           ; 1612 90 1E                    ..
        ldx     curLine                        ; 1614 A6 D2                    ..
        lda     lintab_y-1,x                         ; 1616 BD 5F 2B                 ._+
        clc                                     ; 1619 18                       .
        adc     lintab_height-1,x                         ; 161A 7D 74 2B                 }t+
        bcs     L1668                           ; 161D B0 49                    .I
        cmp     windowBottom                    ; 161F C5 34                    .4
        beq     L1625                           ; 1621 F0 02                    ..
        bcs     L1668                           ; 1623 B0 43                    .C
L1625:  sta     r11L			; y pos
        jsr     loadWR3Zero		; while width
        jsr     LoadWR4ScPixWidthMinus1
        lda     #%10101010
        jsr     HorizontalLine                  ; 162F 20 18 C1                  ..
L1632:  lda     usablePageHeight                        ; 1632 A5 ED                    ..
        add     #36                            ; 1635 69 24                    i$
        sta     r2L                             ; 1637 85 06                    ..
        lda     usablePageHeight+1                        ; 1639 A5 EE                    ..
        adc     #0                            ; 163B 69 00                    i.
        sta     r2H                             ; 163D 85 07                    ..
        SubW	pagePosY, r2                          ; 164A 85 07                    ..
        jsr     CmpR2WindowBottom                           ; 164C 20 56 28                  V(
        beq     L1653                           ; 164F F0 02                    ..
        bcs     L1668                           ; 1651 B0 15                    ..
L1653:  lda     r2L                             ; 1653 A5 06                    ..
        add     #7                            ; 1656 69 07                    i.
        cmp     windowBottom                    ; 1658 C5 34                    .4
        bcc     L165E                           ; 165A 90 02                    ..
        lda     windowBottom                    ; 165C A5 34                    .4
L165E:  sta     r2H                             ; 165E 85 07                    ..
        jsr     loadWR3Zero                           ; 1660 20 52 27                  R'
        lda     #2			; 50% stipple                            ; 1663 A9 02                    ..
        jsr     rectangleUntilRightBorder                           ; 1665 20 4C 18                  L.
L1668:  rts                                     ; 1668 60                       `

;---------------------------------------------------------------
; fillPartOfScreen/clearPartOfScreen
;
; Function:  Fills/Clears a rectangle with a given point as the
;            top left, and the right and bottom borders as the
;            bottom right.
;
; Pass:      a   top
;            r3  left
;---------------------------------------------------------------
fillPartOfScreen:
	ldx     #$FF			; black
        bne     __1

clearPartOfScreen:
	ldx     #0			; white
__1:	cmp     windowBottom
        beq     :+
        bcs     @rts

:	pha
        stx     r14L			; flag
        sta     r2L
        and     #7
        beq     @skip			; already at 8 line boundary
        lda     r2L
        ora     #7
        sta     r2H
        jsr     loadWR3Zero
        lda     #0			; white
        bit     r14L
        bpl     :+
        lda     #1			; black
:	jsr     rectangleUntilRightBorder
        pla
        add     #8
        and     #$F8
        pha
@skip:  pla
        cmp     windowBottom
        beq     :+
        bcs     @rts

:	tax
        jsr     GetScanLine
        MoveW   r5, r1
	LoadW   r0, SCREEN_BASE+25*320
        lda     windowBottom
        cmp     #SC_PIX_HEIGHT-1
        beq     :+
	LoadW   r0, SCREEN_BASE+23*320
:	SubW    r1, r0
        MoveB   r14L, r2L
        jsr     FillRam
@rts:	rts

; ----------------------------------------------------------------------------
setCursor3AndJustificationFromLine:
	jsr     setCursor3FromLine                           ; 16D7 20 E3 16                  ..
        lda     lintab_justification,x                         ; 16DA BD DE 2B                 ..+
        and     #JST_10                            ; 16DD 29 10                    ).
        asl     a                               ; 16DF 0A                       .
        sta     justification                        ; 16E0 85 A9                    ..
        rts                                     ; 16E2 60                       `

; ----------------------------------------------------------------------------
; x: line number
setCursor3FromLine:
	stx     cursor3+cursor::srcline	; line number
        jsr     moveLintabR15		; get text pointer
        lda     lintab_height,x
        sta     cursor3+cursor::height	; put height into cursor3
        lda     lintab_baselineOffset,x
        sta     tmpBaselineOffset
        lda     lintab_AAw_lo,x
        sta     zp_AAw
        lda     lintab_AAw_hi,x
        sta     zp_AAw+1
        lda     lintab_width,x
        sta     tmpLineWidth
        lda     lintab_y,x
        sta     r1H
        sta     cursor3+cursor::py
        jsr     readLintab
        ldx     cursor3+cursor::srcline
        lda     lintab_txtPtr_lo+1,x
        sta     cursor3+cursor::ptr
        lda     lintab_txtPtr_hi+1,x
        sta     cursor3+cursor::ptr+1
        rts

; ----------------------------------------------------------------------------
justifyText2:
	jsr     setCursor3FromLine

justifyText:
	bit     justification
        bvc     @1			; JST_STARTS_WITH_RULER
        jsr     readRuler
@1:	lda     #0
        sta     zp_BDw
        sta     zp_BDw+1
        sta     r2H
        jsr     getEffectiveLeftMargin
        MoveW   zp_AAw, r3
        lda     justification
        and     #MASK_JST_JUST
        cmp     #3			; fully justified
        beq     @3
        cmp     #2			; right justified
        beq     @2
        cmp     #1			; left justified
        bne     @rts			; -> done
        lsr     r3H			; centered
        ror     r3L			; -> / 2
@2:	AddW_    r3, r11		; add left margin
@rts:	rts

@3:	PushB   r1H
        lda     tmpLineWidth
        beq     @4
        sta     r2L
        MoveW   zp_AAw, r1
        ldx     #r1
        ldy     #r2
        jsr     Ddiv
        MoveW   r1, zp_BDw 		; [XXX high byte will be overwritten in next line]
        MoveB   r8L, zp_BDw+1
@4:	PopB    r1H
        rts

; ----------------------------------------------------------------------------
L177F:  ldx     curLine                        ; 177F A6 D2                    ..
        jsr     justifyText2                           ; 1781 20 18 17                  ..

L1784:	lda     justification                        ; 1784 A5 A9                    ..
        bpl     @1                           ; 1786 10 0C                    ..

        PushB   r1H                             ; 1788 A5 05                    ..
        jsr     drawPicture                           ; 178B 20 47 19                  G.
        PopB    r1H                             ; 178F 85 05                    ..
        bra     @2                           ; 1792 50 03                    P.

@1:	jsr     whiteRectangle_XXX                           ; 1794 20 A2 17                  ..

@2:	ldx     curLine                        ; 1797 A6 D2                    ..
        lda     lintab_height,x                         ; 1799 BD 75 2B                 .u+
        add     r1H                             ; 179D 65 05                    e.
        sta     r1H                             ; 179F 85 05                    ..
        rts                                     ; 17A1 60                       `

; ----------------------------------------------------------------------------
whiteRectangle_XXX:
	jsr     L14BD                           ; 17A2 20 BD 14                  ..
        sta     r0L                             ; 17A5 85 02                    ..
        ldy     #0                            ; 17A7 A0 00                    ..
        bcs     L17D7                           ; 17A9 B0 2C                    .,

        CmpW    r15, a6
	bcc     L17D1                           ; 17B5 90 1A                    ..
        beq     L17D1                           ; 17B7 F0 18                    ..

        ldy     #$FF                            ; 17B9 A0 FF                    ..
        jsr     L17D7                           ; 17BB 20 D7 17                  ..
        lda     r0L                             ; 17BE A5 02                    ..
        beq     L17D1                           ; 17C0 F0 0F                    ..

        PushW	r2                              ; 17C7 48                       H
        jsr     putChar_XXX                           ; 17C8 20 87 18                  ..
        PopW	r2

L17D1:  jsr     moveWR2R11                           ; 17D1 20 C8 27                  .'
        bra     whiteRectangle_XXX                           ; 17D5 50 CB                    P.

L17D7:  lda     a6L                             ; 17D7 A5 78                    .x
        ora     a6H                             ; 17D9 05 79                    .y
        beq     L184B                           ; 17DB F0 6E                    .n

	PushB	r0L
	PushB	r1H
	PushW	r2
	PushW	r11
        MoveW   r11, r3
        ldx     #r3                            ; 17F7 A2 08                    ..
        jsr     subSideFlippingPixelOffset                           ; 17F9 20 C8 26                  .&
        lda     r3H                             ; 17FC A5 09                    ..
        bpl     L1806                           ; 17FE 10 06                    ..
        LoadW_  r3, 0
L1806:  tya                                     ; 1806 98                       .
        bpl     L1817                           ; 1807 10 0E                    ..

        lda     currentMode                     ; 1809 A5 2E                    ..
        and     #SET_ITALIC                            ; 180B 29 10                    ).
        beq     L1817                           ; 180D F0 08                    ..

        lda     cursor3+cursor::height                        ; 180F A5 A4                    ..
        lsr     a                               ; 1811 4A                       J
        ldx     #r3                            ; 1812 A2 08                    ..
        jsr     addWIToZp                           ; 1814 20 3E 27                  >'

L1817:  CmpWI   r3, SC_PIX_WIDTH
	bcs     L1833                           ; 1821 B0 10                    ..

        lda     r1H                             ; 1823 A5 05                    ..
        sta     r2L                             ; 1825 85 06                    ..
        add     cursor3+cursor::height                        ; 1828 65 A4                    e.
        sta     r2H                             ; 182A 85 07                    ..
        dec     r2H                             ; 182C C6 07                    ..
        lda     #0			; white
        jsr     rectangleUntilRightBorder                           ; 1830 20 4C 18                  L.

L1833:  LoadW_   a6, 0
	PopW	r11
	PopW	r2
	PopB	r1H
	PopB	r0L
L184B:  rts                                     ; 184B 60                       `

; ----------------------------------------------------------------------------
rectangleUntilRightBorder:
	jsr     SetPattern
        jsr     LoadWR4ScPixWidthMinus1		; right side
        jmp     Rectangle

; ----------------------------------------------------------------------------
readLintab:
	txa
        pha
        lda     lintab_justification,x
        sta     justification
        lda     lintab_mode,x
        sta     currentMode
        lda     lintab_font_lo,x
        sta     curFont
        lda     lintab_font_hi,x
        sta     curFont+1
        jsr     loadFontMetrics
        pla
        tax
        rts

; ----------------------------------------------------------------------------
moveLintabR15:
	lda     lintab_txtPtr_lo,x
        sta     r15L
        lda     lintab_txtPtr_hi,x
        sta     r15H
        rts

; ----------------------------------------------------------------------------
; count number of lines on screen, return in zp_B9b
countLines:
	ldx     #$FF
@loop:  inx
        lda     lintab_height,x
        bne     @loop			; until zero
        stx     zp_B9b
        rts

; ----------------------------------------------------------------------------

r0_encrypted_start:

;---------------------------------------------------------------
; putChar_XXX
;
; Function:  Print a single character
;
; Pass:      r0L  character code
;            r2   x limit???
;            r11  x pos
;            curFont   font id
;---------------------------------------------------------------
putChar_XXX:
	CmpW    curFont, activeFont		; switch font?
	beq     @1			; no

	PushB   r0L
	PushB   r1H
	PushW   r2
        jsr     moveCurFontR1
        jsr     _setFontFromFile	; load font
        PopW    r2
        PopB    r1H
        PopB    r0L

@1:	PushW   r11
        ldx     #r2
        jsr     subSideFlippingPixelOffset
        ldx     #r11			; x pos
        jsr     subSideFlippingPixelOffset
        CmpW    rightMargin, r2
	bcc     @end			; beyond right margin
        CmpW    leftMargin, r11
	beq     @2
        bcs     @end			; beyond left margin

@2:	CmpW    sideFlippingLeft_XXX, r11
	bcc     @end
        CmpW    sideFlippingRight_XXX, r2
	beq     @3
        bcs     @end

@3:	PushB   r1H
        ldx     currentMode
        lda     currentMode
        and     #SET_SUPERSCRIPT
        beq     @5

        lda     justification
        and     #MASK_JST_SPACING
        beq     @4

        tax
        lda     cursor3+cursor::height
        lsr     a
        sub     #1

        cpx     #8
        beq     @4			; n / 2 - 1

        ldy     #$FF
        lda     cursor3+cursor::height
@loop:  iny				; n / 3
        sub     #3
        bcs     @loop
        tya

@4:	sta     r5L
        jsr     calcEffectiveBaselineOffset
        add     r5L
        bra     @7

@5:	lda     currentMode
        and     #SET_SUBSCRIPT
        beq     @6
        jsr     calcEffectiveBaselineOffset
        lsr     a

@6:	add     tmpBaselineOffset			; y pos related

@7:	adc     r1H
        sta     r1H			; y pos
        lda     r0L
        jsr     SmallPutChar
        PopB    r1H

@end:	PopW    r11
        rts
r0_encrypted_end:


; ----------------------------------------------------------------------------
drawPicture:
	PushB	r1H

        jsr     centerObjectOnScreen
					; is it visible at all?
        lda     r2L			; width to draw
        bmi     @end
        beq     @end
        lda     sideFlippingRight_XXX
        beq     @1
        lda     r1L			; x pos
        add     r2L			; width to draw
        cmp     #SC_BYTE_WIDTH / 2 + 1
        bcc     @end
@1:	lda     r1L			; x pos
        ldx     sideFlippingLeft_XXX+1
        bne     @2
        cmp     #SC_BYTE_WIDTH / 2
        bcs     @end
@2:	cmp     #SC_BYTE_WIDTH
        bcs     @end

        PopB    r1H			; y pos
        pha
        jsr     calcClippingY
        pla
        pha

        bit     showPicturesFlag
        bpl     @3

        jsr     drawClippedPictureFromDisk
        bra     @end

@3:	jsr     drawImagePlaceholderRect

@end:	PopB    r1H
        rts

;---------------------------------------------------------------
; drawClippedPictureFromDisk
;
; Function:  Draw a picture from disk, with clipping.
;
; Pass:      zp_AAw  index ($40-$7E)
;            r1L     x pos (cards)
;            r1H     y pos (lines)
;            r2L     width to draw (cards)
;            r2H     height to draw (lines)
;            r11L    left skip
;            r11H    right skip
;            r12     top skip
;
; Note:      fileHeader needs to contain the document index
;            table.
;---------------------------------------------------------------
drawClippedPictureFromDisk:
	jsr     setDocDrive
        PushW	r1
	PushW	r4
        lda     zp_AAw
        asl     a
        tay
        lda     fileHeader+2,y		; track
        sta     r1L
        lda     fileHeader+3,y		; sector
        sta     r1H
        jsr     ldR4DiskBlkBuf
	LoadW_  r5, 0			; initial call to ReadByte
        jsr     _ReadByte		; skip 3 bytes
        jsr     _ReadByte
        jsr     _ReadByte
        MoveW   r1, cursor3+cursor::px
        MoveW   r5, zp_AAw
	PopW	r4
	PopW	r1
        jsr     @loadR0MemScrRecv	; use screen recovery buffer for temp
	LoadW   r13, @getByte
	LoadW   r14, @loadR0MemScrRecv	; (API leftover)
        jmp     BitOtherClip

@getByte:
	PushW	r1
	PushW	r4
	PushW	r5
        MoveW   cursor3+cursor::px, r1	; load r1 (track/sector)
        jsr     ldR4DiskBlkBuf
        MoveW   zp_AAw, r5		; load r5 (buffer pointer)

        cmp     r5H 			; (compare with r5L)
        beq     @1
        jsr     ReadByte
        bra     @2

@1:	jsr     _ReadByte

@2:	ldy     #0
        sta     (r0),y
        MoveW   r5, zp_AAw		; write back r5 (buffer pointer)
        MoveW   r1, cursor3+cursor::px	; write back r1 (track/sector)
	PopW    r5
	PopW	r4
	PopW	r1
        rts

@loadR0MemScrRecv:
	LoadW   r0, MEM_SCRRECV
        rts

;---------------------------------------------------------------
; drawImagePlaceholderRect
;
; Function:  Draw a rectangle in the 50% stipple pattern,
;            with the same arguments as BitOtherClip, to
;            be used as a placeholder when picture rendering
;            is turned off.
;
; Pass:      r1L  x pos (cards)
;            r1H  y pos (lines)
;            r2L  width (cards)
;            r2H  height (lines)
;---------------------------------------------------------------
drawImagePlaceholderRect:
	lda     #0
        sta     r3H
        sta     r4H

        lda     r1L			; left = r1L * 8
        asl     a
        rol     r3H
        asl     a
        rol     r3H
        asl     a
        rol     r3H
        sta     r3L

        lda     r1L			; ((right = r1L + r2L - 1) * 8) | 7
        add     r2L
        sub     #1
        asl     a
        rol     r4H
        asl     a
        rol     r4H
        asl     a
        rol     r4H
        ora     #$07
        sta     r4L

        lda     r1H
        sta     r2L			; top = r1H
        add     r2H
        sub     #1
        sta     r2H			; botom = r1H + r2H - 1

        lda     #2
        jsr     SetPattern		; 50% stipple
        jmp     Rectangle

; ----------------------------------------------------------------------------
centerObjectOnScreen:
	jsr     centerObject
        ldx     sideFlippingOffset
        beq     @skip

@loop:  SubVB   20, r1H			; for every flipping step,
        SubVB   20, r1L			; move picture 20 cards to the left
        bcs     @1			; no overflow
	SubB_   r1L, r11L
	AddB__  r1L, r2L
        bmi     @rts
        LoadB   r1L, 0
@1:	dex
        bne     @loop

@skip:  lda     r1H			; right x coord
        sub     #40			; - 40 cards (= 1 screen)
        bcc     @rts
        beq     @rts			; overflow, return

        sta     r5L
        add     r11H
        sta     r11H
        SubB_   r5L, r2L
@rts:	rts

;---------------------------------------------------------------
; centerObject
;
; Function:  Calculate the left and right position of a centered
;            object in the line. If it doesn't fit, limit it to
;            the margins.
;
; Pass:      tmpLineWidth  width of object
;
; Return:    r1L  leftmost position of object (cards)
;            r1H  rightmost position of object (cards)
;            r2L  (adjusted) object width
;            r11  size reduction of object
;---------------------------------------------------------------
centerObject:
	MoveB   tmpLineWidth, r2L		; object width
        LoadW_	r11, 0
        lda     rulerData1+ruler::left_margin
        sta     r5L
        lda     rulerData1+ruler::left_margin+1
        lsr     a
        ror     r5L
        lsr     a
        ror     r5L
        lsr     a
        ror     r5L			; left margin / 8

        ldx     #r5H			; r5H and r6L!!
        jsr     getEffectiveRightMargin
        inc     r5H			; right margin + 1
        bne     :+
        inc     r6L
:	ldy     #3
        ldx     #r5H			; r5H and r6L!!
        jsr     DShiftRight		; (right margin + 1) / 8

        lda     r5H			; right
        sub     r5L			; - left
        sbc     r2L			; - object width
        bcc     @1			; does not fit

        lsr     a			; diff / 2
        add     r5L			; + left
        sta     r1L			; result1 = left center point
        adc     r2L			; + object width
        sta     r1H			; result2 = right center point
        rts

@1:	lda     r5L			; left
        sta     r1L			; result1 = left
        lda     r5H			; right
        sta     r1H			; result2 = right
        sub     r5L			; - left
        sta     r2L			; = object width
        sec
        lda     tmpLineWidth			; original object width
        sbc     r2L			; - new object width
        sta     r11H			; object width difference
        rts

;---------------------------------------------------------------
; calcClippingY
;
; Function:  Calculate the y-related arguments to BitOtherClip
;            from the current y position on screen.
;
; Pass:      r1H  y pos on screen
;
; Return:    r1H  y pos
;            r2H  height to draw
;            r12  top skip
;---------------------------------------------------------------
calcClippingY:
	LoadW   r12, 0			; r12 = 0 (top skip)
        lda     r1H
        cmp     windowTop
        bcs     @1			; if r1H >= windowTop then skip

        sec
        lda     windowTop
        sbc     r1H
        sta     r12L			; r12 = windowTop - r1H (top skip)
        lda     windowTop
        sta     r1H			; r1H = windowTop (y pos)

@1:	lda     cursor3+cursor::height
        sub     r12L
        add     r1H			; cursor::height - top skip + windowTop
        bcs     @3

        cmp     windowBottom
        beq     @2
        bcs     @3

@2:	sec
        lda     cursor3+cursor::height
        sbc     r12L
        sta     r2H			; r2H = cursor::height - top skip (height to draw)
        rts

@3:	sec
        lda     windowBottom
        sbc     r1H
        sta     r2H
        inc     r2H			; r2H = windowBottom - top skip + 1 (height to draw)
        rts

; ----------------------------------------------------------------------------
L1B5E:  ldx     curLine
        jsr     moveR15Lintab
        jsr     getJustification_XXX
        jsr     measureLine
        php				; save whether we hit the end of the page
        jsr     moveCursor3Lintab
        jsr     moveCursor3Ptr_r15
        clc
        jsr     L1BAF
        tax
        bne     @1
        inc     curLine
        plp				; end of page?
        bcc     L1B5E			; no

        ldx     curLine
        jsr     moveR15Lintab
        bra     @2

@1:	plp				; end of page (ignored)
@2:	ldx     curLine
        lda     #0
        sta     lintab_height,x
        rts

moveR15Lintab:
	lda     r15L
        sta     lintab_txtPtr_lo,x
        lda     r15H
        sta     lintab_txtPtr_hi,x
        rts

; ----------------------------------------------------------------------------
getJustification_XXX:
	lda     rulerData2+ruler::justification
        cpx     #0
        beq     :+
        lda     lintab_justification-1,x	; take it from previous line
:	and     #JST_20
        sta     r0L
        lda     justification
        and     #$FF-JST_20
        ora     r0L
        sta     justification
        rts

; ----------------------------------------------------------------------------
L1BAF:  php                                     ; 1BAF 08                       .
        lda     #$40                            ; 1BB0 A9 40                    .@
        ldx     r1H                             ; 1BB2 A6 05                    ..
        dex                                     ; 1BB4 CA                       .
        cpx     windowBottom                    ; 1BB5 E4 34                    .4
        beq     @end                           ; 1BB7 F0 52                    .R
        ldy     #0                            ; 1BB9 A0 00                    ..
        lda     r1H                             ; 1BBB A5 05                    ..
        add     cursor3+cursor::height                        ; 1BBE 65 A4                    e.
        sta     r2L                             ; 1BC0 85 06                    ..
        bcc     :+                           ; 1BC2 90 01                    ..
        iny                                     ; 1BC4 C8                       .
:	sty     r2H                             ; 1BC5 84 07                    ..
        LoadB   r4L, 0
        MoveB   r2L, r1H
        jsr     CmpR2WindowBottom                           ; 1BCF 20 56 28                  V(
        bcc     @2                           ; 1BD2 90 11                    ..
        ldy     windowBottom                    ; 1BD4 A4 34                    .4
        iny                                     ; 1BD6 C8                       .
        sty     r1H                             ; 1BD7 84 05                    ..
        plp                                     ; 1BD9 28                       (
        php                                     ; 1BDA 08                       .
        bcs     @1                           ; 1BDB B0 04                    ..
        lda     justification                        ; 1BDD A5 A9                    ..
        bmi     @2			; JST_80
@1:	lda     #$40                            ; 1BE1 A9 40                    .@
        sta     r4L                             ; 1BE3 85 0A                    ..
@2:	AddW3   pagePosY, r2, r3                             ; 1BEA 85 08                    ..
        lda     #36                            ; 1BF2 A9 24                    .$
        ldx     #r3                            ; 1BF4 A2 08                    ..
        jsr     subWBI                           ; 1BF6 20 D6 26                  .&
	CmpW    r3, usablePageHeight
	lda     r4L                             ; 1C03 A5 0A                    ..
        bcc     @end                           ; 1C05 90 04                    ..
        ora     #$80                            ; 1C07 09 80                    ..
        sta     r4L                             ; 1C09 85 0A                    ..
@end:	plp                                     ; 1C0B 28                       (
        rts                                     ; 1C0C 60                       `

; ----------------------------------------------------------------------------
moveCursor3Lintab:
	ldx     curLine
        lda     r1H
        sta     lintab_y,x		; fill Y
        ldy     #0
@loop:  lda     cursor3+cursor::height,y	; copy cursor3 into line
        sta     lintab_height,x
        txa
        add     #21
        tax
        iny
        cpy     #9			; ??? cursor only has 4 more bytes, not 9!
        bne     @loop
        rts

; ----------------------------------------------------------------------------
; Check whether at r15, the cardset is invalid. This is true when there is
; a new cardset defined, or if it's the end of the page or document.
isEndOfCardSet:
	ldy     #0
        lda     (r15),y
        beq     @yes
        cmp     #PAGE_BREAK
        beq     @yes
        cmp     #NEWCARDSET
        beq     @yes
        lda     #$00
        rts

@yes:	lda     #$80
        rts

; ----------------------------------------------------------------------------
storeNewCardSetFromCursor0:
	ldy     #3
@loop:  lda     cursor0+cursor::font-1,y
        sta     (r7),y
        dey
        bne     @loop
        lda     #NEWCARDSET
        sta     (r7),y
        rts

; ----------------------------------------------------------------------------
deleteTextAndPicturesinRange:
	jsr     deletePicturesInRange
        MoveW   tmpRangeStart, r1
        MoveB_y tmpRangeEnd+1, r0H
        lda     tmpRangeEnd
        sta     r0L
        sub     r1L
        sta     r4L
        tya
        sbc     r1H
        sta     r4H
	SubW    r4L, pageEndPtr2
        lda     #0
        sub     r4L
        sta     r3L
        lda     #0
        sbc     r4H
        sta     r3H
        SubW3   pageEndPtr2, r1, r2
        IncW    r2L
        jmp     MoveData

; ----------------------------------------------------------------------------
makeSpaceForRuler:
	LoadW   r3, .sizeof(ruler)+1

makeSpace:
	jsr     setDirty
        PushW   pageEndPtr2

        lda     r15L			; r4 = r15 - pageEndPtr2 + 1
        clc
        sbc     pageEndPtr2
        sta     r4L
        lda     r15H
        sbc     pageEndPtr2+1
        sta     r4H

        lda     pageEndPtr2		; r5 = pageEndPtr2
        sta     r5L			; r6          = pageEndPtr2 + r3
        add     r3L			; pageEndPtr2 = pageEndPtr2 + r3
        sta     r6L
        sta     pageEndPtr2
        lda     pageEndPtr2+1
        sta     r5H
        adc     r3H
        sta     r6H
        sta     pageEndPtr2+1

	CmpWI   pageEndPtr2, MEM_FONT
	bcc     @1
	PopW    pageEndPtr2
        jsr     pushRulerData
        jsr     pushR0ToR14
        jsr     _updateDocument
        jsr     popR0ToR14
        jsr     popRulerData
        bra     makeSpace

@1:	pla
        pla
        ldy     #1
@loop:  dey
        lda     (r5),y
        sta     (r6),y
        inc     r4L
        bne     :+
        inc     r4H
:	beq     updatelintab_txtPtrIfNeeded_XXX
        tya
        bne     @loop
        dec     r5H
        dec     r6H
        bne     @loop

updatelintab_txtPtrIfNeeded_XXX:
	bit     a4H
        bmi     rts0
        bvs     rts0

        ldx     a4H
@loop:  inx
        lda     lintab_txtPtr_lo,x
        add     r3L
        sta     lintab_txtPtr_lo,x
        lda     lintab_txtPtr_hi,x
        adc     r3H
        sta     lintab_txtPtr_hi,x
        lda     lintab_height,x
        bne     @loop
rts0:	rts

;---------------------------------------------------------------
; deletePicturesInRange
;
; Function:  Remove pictures in range ruler0:ruler1 from the
;            document.
;---------------------------------------------------------------
deletePicturesInRange:
	MoveW   tmpRangeStart, r15
@loop:  jsr     getByteIntpNewCardSet
        CmpW_x  r15, tmpRangeEnd
	bcs     rts0			; finished

        cmp     #ESC_RULER
        beq     @2
        cmp     #ESC_GRAPHICS
        bne     @1

        ldy     #4
        lda     (r15),y			; record number of picture
        jsr     PointRecord
        jsr     setDocDrive
        jsr     _DeleteRecord		; delete picture
        jsr     _InsertRecord		; insert empty link
        jsr     addWI4R15		; skip 4 bytes (plus one more later)
        LoadB   zp_DDb, 0
        lda     a4L
        ora     #A4L_20
        sta     a4L

@1:	jsr     incWR15			; skip character
        bcc     @loop			; always

@2:	jsr     addRulerSizeToR15	; skip ruler
        bcc     @loop			; always

;---------------------------------------------------------------
; killPrompt
;
; Function:  Wrapper around PromptOff. [HHG p.17]
;---------------------------------------------------------------
killPrompt:
	php				; save I status
        sei				; disable interrupts
        jsr     PromptOff		; prompt = off
        LoadB   alphaFlag, 0		; clear alpha flag
        plp				; restore I status
        rts				; exit

;---------------------------------------------------------------
; setPromptFontMetricsUpdateRulerUI
;
; Function:  Set prompt position, update ruler UI.
;---------------------------------------------------------------
setPromptFontMetricsUpdateRulerUI:
	jsr     setPromptPosition
        bcs     @skip			; skip if prompt off

        lda     #$FF
        sta     setFontMarkFlag
        sta     setStyleCheckmarkFlag
        MoveB   cursor0+cursor::mode, currentMode
        jsr     moveCursor0FontCurFont	; load font metrics from cursor0
        jsr     loadFontMetrics

@skip:  jmp     updateRulerUI

;---------------------------------------------------------------
; setPromptPosition
;
; Function:  Set prompt position from cursor0; turn off if
;            active selection.
;
; Pass:      cursor0, cursor1  cursor or selection
;---------------------------------------------------------------
setPromptPosition:
	jsr     cmpCursor0_1Ptr		; selection?
        bne     @1			; yes
        bit     cursor0+cursor::srcline
        bmi     @1
        bvs     @1
        lda     cursor0+cursor::px		; set prompt position X
        sta     stringX
        lda     cursor0+cursor::px+1
        sta     stringX+1
        ora     stringX			; and decrement (unless already 0)
        beq     :+
	DecW    stringX
:	lda     cursor0+cursor::py
        sta     stringY			; set prompt position Y
        jsr     initTextPrompt
        jsr     PromptOn
        clc				; =on
        rts

@1:	jsr     killPrompt
        sec				; =off
        rts

;---------------------------------------------------------------
; getLineRuler
;
; Function:  Get a screen line's ruler.
;
; Pass:      x  screen line
;
; Return:    ruler1  ruler
;---------------------------------------------------------------
getLineRuler:
	lda     lintab_justification,x	; find line that starts with ruler
        and     #JST_STARTS_WITH_RULER
        bne     @1
        dex				; then line above
        bpl     getLineRuler		; exists, continue there

        jmp     copyRuler2To1

@1:	jsr     moveLintabR15		; get text pointer

;---------------------------------------------------------------
; getRulerFromText
;
; Function:  Get a ruler from the text.
;
; Pass:      r15  text pointer
;
; Return:    ruler1  ruler
;---------------------------------------------------------------
getRulerFromText:
	ldy     #1
@loop:  lda     (r15),y
        sta     rulerData1-1,y
        iny
        cpy     #.sizeof(ruler)+1
        bne     @loop
        rts

;---------------------------------------------------------------
; deleteSelection
;
; Function:  Delete the range between cursor0 and cursor1.
;---------------------------------------------------------------
deleteSelection:
	jsr     pushRulerData
        MoveB   cursor0+cursor::srcline, a4H
        lda     cursor0+cursor::ptr
        sta     tmpRangeStart
        sta     a6L
        lda     cursor0+cursor::ptr+1
        sta     tmpRangeStart+1
        sta     a6H
        lda     cursor1+cursor::ptr
        sta     r15L
        sta     tmpRangeEnd
        lda     cursor1+cursor::ptr+1
        sta     r15H
        sta     tmpRangeEnd+1

        jsr     isEndOfCardSet		; cardset invalid at end of range?
        bne     @2			; yes

        CmpW    cursor0+cursor::font, cursor1+cursor::font
	bne     @1			; different font

        CmpB    cursor0+cursor::mode, cursor1+cursor::mode
        bne     @1			; different mode

        jsr     cmpCursor0PtrPageCardset
        bne     @2

@1:	jsr     addNewCardSetFromCursor1

@2:	jsr     deleteTextAndPicturesinRange
        jsr     updatelintab_txtPtrIfNeeded_XXX
        lda     a4L
        ora     #A4L_02
        sta     a4L
        jsr     popRulerData
        jsr     copyCursor0To1

setDirty:
	LoadB   dirty, $FF
        rts

;---------------------------------------------------------------
; addNewCardSetFromCursor1
;
; Function:  Store cursor1 font and mode as NEWCARDSET escape
;            to location tmpRangeStart; add 4 to tmpRangeStart.
;
; Pass:      cursor1  contains font and mode
;---------------------------------------------------------------
addNewCardSetFromCursor1:
	lda     tmpRangeStart		; tmpRangeStart += 4
        sta     r15L
        add     #4
        sta     tmpRangeStart
        lda     tmpRangeStart+1
        sta     r15H
        adc     #0
        sta     tmpRangeStart+1

;---------------------------------------------------------------
; storeNewCardSetFromCursor1
;
; Function:  Store cursor1 font and mode as NEWCARDSET escape
;            into text.
;
; Pass:      cursor1  contains font and mode
;            r15      text pointer
;---------------------------------------------------------------
storeNewCardSetFromCursor1:
	ldy     #3
@loop:  lda     cursor1+cursor::font-1,y
        sta     (r15),y
        dey
        bne     @loop
        lda     #NEWCARDSET
        sta     (r15),y
        rts

;---------------------------------------------------------------
; getLastRuler
;
; Function:  Returns pointer to last ruler escape in range
;            MEM_PAGE to cursor1.
;
; Pass:      cursor1  upper limit
;
; Return:    r14  pointer to last ruler
;---------------------------------------------------------------
getLastRuler:
	jsr     LoadR15_MEM_PAGE	; start at page start
@loop:  jsr     CmpR15Cursor1Ptr
        bcs     @rts			; finished on range end
        jsr     getByteIntpNewCardSet
        cmp     #ESC_RULER
        beq     @1
        jsr     incWR15
        cmp     #ESC_GRAPHICS
        bne     @loop
        jsr     addWI4R15		; skip graphics
        bcc     @loop
@1:	jsr     moveWR15R14		; remember ruler pointer
        jsr     addRulerSizeToR15	; skip ruler
        bcc     @loop			; always
@rts:	rts

;---------------------------------------------------------------
; isAlphanumeric
;
; Function:  Test whether a given character is
;            'A'-'Z', 'a'-'z', '0'-'9' or '_'.
;
; Pass:      a   character code
;
; Return:    c   =1: yes
;---------------------------------------------------------------
isAlphanumeric:
	cmp     #'0'
        bcc     @1
        cmp     #'9'+1
        bcc     @yes
@1:
.if CHAR_ENCODING=CHAR_ENCODING_ASCII
	cmp     #'A'
.elseif CHAR_ENCODING=CHAR_ENCODING_DE
	cmp     #'@'			; [XXX '§' in German GEOS, not alphanumeric!]
.endif
        bcc     @2
.if CHAR_ENCODING=CHAR_ENCODING_ASCII
        cmp     #'Z'+1
.elseif CHAR_ENCODING=CHAR_ENCODING_DE
        cmp     #']'+1
.endif
        bcc     @yes
@2:	cmp     #'a'
        bcc     @3
.if CHAR_ENCODING=CHAR_ENCODING_ASCII
        cmp     #'z'+1
.elseif CHAR_ENCODING=CHAR_ENCODING_DE
        cmp     #'~'+1
.endif
        bcc     @yes
@3:	cmp     #'_'
        beq     @yes
        clc
        rts

@yes:	sec
        rts

;---------------------------------------------------------------
; fontNameFromIndex
;
; Function:  Get the name of a font from its disk fonts index.
;
; Pass:      a   font index
;
; Return:    r0  font name string
;---------------------------------------------------------------
fontNameFromIndex:
	sta     r3L
        LoadB   r0L, FONT_NAME_SIZE
        ldy     #r3
        ldx     #r0
        jsr     BBMult
	AddVW__ fontNames+2, r0		; skip "  "/"* " prefix
        rts

;---------------------------------------------------------------
; fontSelected2
;
; Function:  Worker function for font callback.
;
; Pass:      a   index of font file (0-7)
;---------------------------------------------------------------
fontSelected2:
	asl     a
        asl     a
        asl     a
        asl     a                       ; * 16
        sta     lintab_fontIndex
        tay
        bne     @1

        jsr     loadR1SystemFont        ; entry 0: system font
        jsr     fillFontSizeMenuTexts
        ldy     #2
        bne     @done

@1:	lda     diskFontIds,y
        sta     r1L
        lda     diskFontIds+1,y
        sta     r1H
        ora     r1L
        beq     @done                   ; empty = end of table
        tya
        pha
        jsr     fillFontSizeMenuTexts
        pla
        tay
        iny
        iny
        tya
        and     #$0F
        bne     @1			; up to 8 entries

@done:	tya
        and     #$0F
        lsr     a
        sta     r1L
        ora     #VERTICAL | UN_CONSTRAINED
        sta     menu_fontpoints_items
        LoadB	r2L, 14
        ldy     #r2
        ldx     #r1
        jsr     BBMult
        lda     r1L
        add     #SUBMENU_Y + 1
        sta     menu_fontpoints_bottom ; num * 14 + 16
        lda     #<menu_fontpoints
        ldx     #>menu_fontpoints
        jmp     storeAXR0


;---------------------------------------------------------------
; fillFontSizeMenuTexts
;
; Function:  Populate point sizes in font point size menu,
;            set marking on selected item.
;
; Pass:      y   offset in point size table
;---------------------------------------------------------------
fillFontSizeMenuTexts:
	tya
        and     #$0F
        lsr     a
        sta     r3L			; font file index

        lda     #txt_xx_point_len
        sta     r4L
        ldy     #r4
        ldx     #r3
        jsr     BBMult
	AddVW__ txt_xx_point, r3
        lda     #' '
	CmpW_y  r1, curFont
	bne     @skip
        bit     setFontMarkFlag
        bpl     @skip

        lda     #'*'			; selection mark
@skip:  ldy     #0
        sta     (r3),y
        lda     #'0'
        sta     r4L
        lda     r1L
        and     #FONT_SIZE_MASK
@loop:  cmp     #10
        bcc     @done
        sub     #10
        inc     r4L
        bra     @loop

@done:  add     #'0'
        ldy     #3
        sta     (r3),y
        dey
        lda     r4L
        cmp     #'0'
        bne     :+
        lda     #' '
:	sta     (r3),y
        rts

;---------------------------------------------------------------
callbackFontPoint:
	pha				; menu index
        jsr     GotoFirstMenu
        lda     lintab_fontIndex
        bne     @1
        pla				; menu index
        jsr     loadR1SystemFont
        jmp     J2_process2

@1:	pla				; menu index
        asl     a
        add     lintab_fontIndex
        tay
        lda     diskFontIds,y
        sta     r1L
        lda     diskFontIds+1,y
        sta     r1H
        jmp     J2_process2


;---------------------------------------------------------------
; Core Font Management
;---------------------------------------------------------------

;---------------------------------------------------------------
; findFontIdOnDisk
;
; Function:  Search whether a given font/size is on disk.
;
; Pass:      r0  font ID
;
; Return:    c   =0: found
;                    x    index for info in various arrays
;                    r0   font name
;
; Note:      This function does not access disk at all, but
;            checks cached data structures read by
;            readFontMetadata.
;---------------------------------------------------------------
findFontIdOnDisk:
	jsr     CmpR1SystemFont
        bne     @notdefault
	LoadW   r0, fontNames+2
        lda     #SYSTEM_FONT_SIZE
        clc
        rts

@notdefault:
	ldx     #16 * 1			; skip system font
@loop:  lda     diskFontIds,x
        cmp     r1L
        bne     :+
        lda     diskFontIds+1,x
        cmp     r1H
        beq     @found
:       inx
        inx
        cpx     #16 * MAX_FONT_FILES
        bne     @loop
        sec
        rts

@found: txa
        pha
        lsr     a
        lsr     a
        lsr     a
        lsr     a     ; / 16
        jsr     fontNameFromIndex
        pla
        tax
        lda     r1L
        and     #FONT_SIZE_MASK  	; extract point size
        clc
        rts


;---------------------------------------------------------------
; Font style callbacks
;---------------------------------------------------------------
italics:
	lda     #SET_ITALIC
        .byte   $2C
outline:
	lda     #SET_OUTLINE
        .byte   $2C
underline:
	lda     #SET_UNDERLINE
        .byte   $2C
bold:
	lda     #SET_BOLD
        eor     currentMode
___1:  sta     currentMode
        jsr     GotoFirstMenu
        jmp     J2_process2b

plainText:
	lda     #SET_PLAINTEXT		; 0
        beq     ___1			; always

superscript:
	lda     currentMode
        eor     #SET_SUPERSCRIPT
        and     #<(~SET_SUBSCRIPT)
        bra     ___1

subscript:
	lda     currentMode
        eor     #SET_SUBSCRIPT
        and     #<(~SET_SUPERSCRIPT)
        bra     ___1

;---------------------------------------------------------------
; setupStyleMenu
;
; Function:  Sets the mark before the items in the style menu
;            that correspond to currentMode.
;
; Returns:   x:a  pointer to style menu
;---------------------------------------------------------------
setupStyleMenu:
	lda     #' '
        sta     txt_plain_text
        sta     txt_bold
        sta     txt_italic
        sta     txt_outline
        sta     txt_underline
        sta     txt_superscript
        sta     txt_subscript

        bit     setStyleCheckmarkFlag
        bpl     @end

        ldx     #'*'                    ; selection mark
        lda     currentMode
        bne     @1

        stx     txt_plain_text
        beq     @end

@1:	bpl     @2

        stx     txt_underline
@2:	asl     a
        bpl     @3

        stx     txt_bold

@3:	asl     a
        asl     a
        bpl     @4

        stx     txt_italic

@4:	asl     a
        bpl     @5

        stx     txt_outline

@5:	asl     a
        bpl     @6

        stx     txt_superscript

@6:	asl     a
        bpl     @end

        stx     txt_subscript

@end:	lda     #<menu_style
        ldx     #>menu_style
        jmp     storeAXR0

;---------------------------------------------------------------
; setupFontMenu
;
; Function:  Sets the mark before the one item in the font menu
;            that corresponds to the given font.
;
; Returns:   x:a  pointer to font menu
;---------------------------------------------------------------
setupFontMenu:
	LoadW   r0, fontNames		; reset selection marks for all fonts in the font menu
        ldx     #MAX_FONT_FILES
@loop:  lda     #' '
        ldy     #0
        sta     (r0),y
	AddVW   FONT_NAME_SIZE, r0
	dex
        bne     @loop

        bit     setFontMarkFlag
        bpl     @1

        jsr     moveCurFontR1
        jsr     findFontIdOnDisk  	; returns pointer to name
        bcs     @1

	SubVW   2, r0             	; point to 2 spaces earlier
        lda     #'*'
        ldy     #0
        sta     (r0),y            	; selection mark

@1:	lda     #<menu_fontnames
        ldx     #>menu_fontnames
        jmp     storeAXR0


;---------------------------------------------------------------
; Core Font Management
;---------------------------------------------------------------

;---------------------------------------------------------------
; setFontFromFile
;
; Function:  Set font. If necessary, load from disk and cache it.
;
; Pass:      r1  font ID
;
; Return:    c   =0: success
;                =1: fail, system font was loaded instead
;---------------------------------------------------------------
setFontFromFile:
	jsr     CmpR1SystemFont		; system font is part of KERNAL, and always in
        beq     @sysfont		; RAM; doesn't need to be managed
	CmpW    r1, activeFont
	beq     @set			; nothing to do [XXX could jump to one line further]
        bne     @find

@sysfont:
	jsr     UseSystemFont		; just switch to it
@set:	MoveW   r1, activeFont
        clc
        rts

@find:	jsr     findLoadedFont		; is it already loaded?
        bcs     @load			; not found
        jsr     updateLoadedFontLruId	; mark it as the latest one that was used
        lda     loadedFontPtrsHi,x
        sta     r0H
        lda     loadedFontPtrsLo,x
        sta     r0L
        jsr     LoadCharSet		; switch to it
        bra     @set

@load:  jsr     findFontIdOnDisk	; does the font exist on disk?
        bcs     useSystemFont		; no, quietly use system font instead
        lda     diskFontRecordTrkSec,x	; does point size exist?
        beq     useSystemFont		; no, quietly use system font instead
        txa
        pha
        lda     diskFontRecordSize,x	; r3 = size of font data
        sta     r3L
        lda     diskFontRecordSize+1,x
        sta     r3H
        jsr     allocateFontBufferSpace	; kick out least recently used font(s) if needed
        jsr     updateLoadedFontLruId	; mark it as the latest one that was used
        lda     r1L
        sta     loadedFontIdsLo,x	; save the ID in the table so the font
        lda     r1H			; can be found in RAM again
        sta     loadedFontIdsHi,x
        lda     loadedFontPtrsHi,x	; r7 = allocated location in RAM
        sta     r7H
        lda     loadedFontPtrsLo,x
        sta     r7L
        pla
        tax
	PushW   r1			; save ID
	PushW   r7			; save RAM location
        lda     diskFontRecordTrkSec,x	; location on disk
        sta     r1L
        lda     diskFontRecordTrkSec+1,x
        sta     r1H
	LoadW   r2, MEM_SIZE_FONTS	; maximum file size [XXX should be remaining RAM size]
        jsr     setAppDrive
        jsr     _ReadFile		; load font data into font buffer
	PopW    r0			; read RAM location into r0
	PopW    activeFont			; read ID into activeFont
        cpx     #0
        bne     useSystemFont        	; read error
        jsr     LoadCharSet
        clc				; success
        rts

useSystemFont:
	jsr     UseSystemFont
	LoadW   activeFont, SYSTEM_FONT_ID
        sec				; fail: it's not the font we wanted
        rts

;---------------------------------------------------------------
; updateLoadedFontLruId
;
; Function:  Mark a given font as most recently used.
;
; Pass:      x   font index
;---------------------------------------------------------------
updateLoadedFontLruId:
	lda     fontLruCounter
        sta     loadedFontLruIdLo,x
        lda     fontLruCounter+1
        sta     loadedFontLruIdHi,x
        IncW    fontLruCounter
	bne     @rts

	; 16 bit overflow: clear LRU ID for all fonts
        ldy     #0
        tya
@loop:	sta     loadedFontLruIdLo,y
        sta     loadedFontLruIdHi,y
        iny
        cpy     loadedFontsCount
        bne     @loop

@rts:	rts

;---------------------------------------------------------------
; allocateFontBufferSpace
;
; Function:  Allocate buffer space for a new font.
;
; Pass:      r3  size of font data
;
; Note:      This function cannot fail: It will remove fonts
;            using an LRU strategy until there is space.
;---------------------------------------------------------------
allocateFontBufferSpace:
	ldx     loadedFontsCount      	; no fonts loaded?
        beq     @first               	; then load it to start of buffer

        cpx     #MAX_FONTS_LOADED
        beq     @remove               	; too many fonts loaded, remove one

        lda     loadedFontPtrsLo-1,x	; check for r3 bytes of spaces in font buffer
        clc				; (last font pointer + last font size + required size)
        adc     loadedfontDataSizeLo-1,x
        tay
        lda     loadedFontPtrsHi-1,x
        adc     loadedfontDataSizeHi-1,x
        tax
        tya
        add     r3L
        tay
        txa
        adc     r3H
        cmp     #>MEM_SCRRECV
        bne     :+
        cpy     #<MEM_SCRRECV
:	bcc     @add               	; it fits
        beq     @add

@remove:
	PushW   r1                  	; does not fit
        jsr     unloadLruFont       	; remove one
        PopW    r1
        bra     allocateFontBufferSpace	; try again

@first:	lda     #>MEM_FONT           	; load first font to start
        ldy     #<MEM_FONT           	; of font buffer
        bne     @set                	; [XXX dangerous, not taken if <MEM_FONT == 0!]
	.assert <MEM_FONT <> 0, error

@add:	ldx     loadedFontsCount       	; new ptr = last ptr + size
        lda     loadedFontPtrsLo-1,x
        clc
        adc     loadedfontDataSizeLo-1,x
        tay
        lda     loadedFontPtrsHi-1,x
        adc     loadedfontDataSizeHi-1,x
@set:	ldx     loadedFontsCount
        sta     loadedFontPtrsHi,x      ; store new ptr
        tya
        sta     loadedFontPtrsLo,x
        lda     r3L
        sta     loadedfontDataSizeLo,x  ; new size
        lda     r3H
        sta     loadedfontDataSizeHi,x
        inc     loadedFontsCount       	; one font more
        rts

;---------------------------------------------------------------
; unloadLruFont
;
; Function:  Unload the least recently used font and compress
;            the font buffer.
;---------------------------------------------------------------
unloadLruFont:
					; find lowest LRU ID
	ldy     #0			; candidate for lowest
        ldx     #1
@loop1: cpx     loadedFontsCount
        beq     @end1			; done iterating
        lda     loadedFontLruIdHi,x
        cmp     loadedFontLruIdHi,y
        bne     @1
        lda     loadedFontLruIdLo,x
        cmp     loadedFontLruIdLo,y
@1:	bcs     @2
        txa				; current one is lower
        tay				; -> update candidate
@2:	inx
        bra     @loop1

@end1:  tya
        tax				; lowest index to X
        jmp     @loop2 			; [XXX no-op]

@loop2:	inx
        cpx     loadedFontsCount	; is it the last one?
        beq     @end2			; then we're done

        dex
        lda     loadedfontDataSizeHi+1,x; count: size of the one after
        sta     r2H
        lda     loadedfontDataSizeLo+1,x
        sta     r2L
        lda     loadedFontPtrsHi+1,x	; source: address of the one after
        sta     r0H
        lda     loadedFontPtrsLo+1,x
        sta     r0L
        lda     loadedFontPtrsHi,x	; target: address of the current one
        sta     r1H
        lda     loadedFontPtrsLo,x
        sta     r1L
        txa
        pha
        jsr     MoveData		; move the next font down
        pla
        tax
        lda     loadedFontIdsLo+1,x	; move loadedFontIds
        sta     loadedFontIdsLo,x
        lda     loadedFontIdsHi+1,x
        sta     loadedFontIdsHi,x
        lda     loadedFontLruIdLo+1,x	; move FontLru
        sta     loadedFontLruIdLo,x
        lda     loadedFontLruIdHi+1,x
        sta     loadedFontLruIdHi,x
        lda     loadedfontDataSizeLo+1,x
        sta     loadedfontDataSizeLo,x	; move loadedfontDataSize
        clc
        adc     loadedFontPtrsLo,x	; update fontPtrs
        sta     loadedFontPtrsLo+1,x
        lda     loadedfontDataSizeHi+1,x
        sta     loadedfontDataSizeHi,x
        adc     loadedFontPtrsHi,x
        sta     loadedFontPtrsHi+1,x
        inx
        bra     @loop2			; repeat for all fonts above removed one

@end2:	dec     loadedFontsCount
        rts

;---------------------------------------------------------------
; findLoadedFont
;
; Function:  Search for font in font buffer.
;
; Pass:      r1  font ID
;
; Return:    c   =0: found
;                    x   index
;---------------------------------------------------------------
findLoadedFont:
	ldx     #0
@loop:	cpx     loadedFontsCount
        beq     @notfound
        lda     loadedFontIdsLo,x
        cmp     r1L
        bne     @1
        lda     loadedFontIdsHi,x
        cmp     r1H
        beq     @found
@1:	inx
        bra     @loop

@notfound:
	sec
        rts
@found:
	clc
        rts


;---------------------------------------------------------------
; menu callbacks
;---------------------------------------------------------------
callbackGeos:
	ldx     #scrrecvtab_geos-scrrecvtabs
        jsr     prepareMenu
        lda     #<menu_geos
        ldx     #>menu_geos
storeAXR0:
	sta     r0L
        stx     r0H
        rts

callbackFile:
	ldx     #scrrecvtab_file-scrrecvtabs
        jsr     prepareMenu
        lda     #<menu_file
        ldx     #>menu_file
        bne     storeAXR0

callbackEdit:
	ldx     #scrrecvtab_edit-scrrecvtabs
        jsr     prepareMenu
        lda     #<menu_edit
        ldx     #>menu_edit
        bne     storeAXR0

callbackFont:
	pha
        ldx     #scrrecvtab_font-scrrecvtabs
        jsr     prepareMenu
        pla
        jmp     setupFontMenu

callbackStyle:
	pha
        ldx     #scrrecvtab_style-scrrecvtabs
        jsr     prepareMenu
        pla
        jmp     setupStyleMenu

callbackOptions:
	ldx     #scrrecvtab_options-scrrecvtabs
        jsr     prepareMenu
        lda     #<menu_options
        ldx     #>menu_options
        bne     storeAXR0

callbackPage:
	ldx     #scrrecvtab_page-scrrecvtabs
        jsr     prepareMenu
        lda     #<menu_page
        ldx     #>menu_page
        bne     storeAXR0

callbackFontSelected:
        pha				; menu item
        lda     a2L
        sta     a2H
        ldx     #scrrecvtab_fontsize-scrrecvtabs
        stx     a2L
        jsr     screenSave
        pla				; menu item
        jmp     fontSelected2

callbackPaste:
        MoveB   a2L, a2H		; move recover offset A to B
        ldx     #scrrecvtab_paste-scrrecvtabs
        stx     a2L
        jsr     screenSave
        lda     #<menu_paste
        ldx     #>menu_paste
        bne     storeAXR0

;---------------------------------------------------------------
; prepareMenu
;
; Function:  Common code to be run before showing a submenu.
;            disables document interactivity and saves the
;            screen rectangle under the menu.
;
; Pass:      x   offset into scrrecvtabs
;---------------------------------------------------------------
prepareMenu:
	stx     a2L
        ldx     #PROCESS_SIDE_FLIPPING
        jsr     BlockProcess		; disabe side flipping
        jsr     J2_blockSelectionProcess
        jsr     clearRecoverOffsetB
        LoadW_  keyVector, 0		; disable keyboard shortcuts
        jsr     killPrompt
        ldx     a2L
        jmp     screenSave

;---------------------------------------------------------------
; appRecoverVector
;
; Function:  RecoverVector function; recovers last saved screen
;            rectangle und re-enables document interactivity.
;---------------------------------------------------------------
appRecoverVector:
	ldx     a2L			; recover offset A set up?
        cpx     #$FF
        beq     clearRecoverOffsetB	; no

        jsr     screenRecover
        MoveB   a2H, a2L		; move recover offset B to A
        cmp     #$FF
        bne     clearRecoverOffsetB	; still some menu visible

        jsr     setKeyVector		; re-enable keyboard shortcuts
        ldx     #PROCESS_SIDE_FLIPPING		; side flipping
        jsr     UnblockProcess
        jsr     setPromptPosition

clearRecoverOffsetB:
	LoadB   a2H, $FF
        rts

;---------------------------------------------------------------
; screenSave
;
; Function:  Save a screen rectangle
;
; Pass:      x   offset into scrrecvtabs
;
; Note:      geoWrite needs the whole background bitmap
;            ($6000-7F40) for its own data structures, so it
;            does not use the GEOS KERNAL's screen save/restore
;            functionality at all. Instead, it has its complete
;            reimplementation that only saves/restores a
;            rectangle.
;            Any function that wants save/restore functionality
;            needs to add an entry to scrrecvtabs with the
;            rectangle and the memory address of the saved data.
;---------------------------------------------------------------
screenSave:
	lda     #0
        bra     :+
;---------------------------------------------------------------
; screenRecover
;
; Function:  Recover a screen rectangle
;
; Pass:      x   offset into scrrecvtabs
;---------------------------------------------------------------
screenRecover:
	lda     #$FF
:	sta     r4H
        jsr     loadScrRecoverlintab_yEntry
@loop1: ldx     r2H
        jsr     GetScanLine
        lda     r2L
        asl     a
        asl     a
        asl     a
        bcc     :+
        inc     r5H
:       tay
        MoveB   r3L, r4L
@loop2: bit     r4H
        bpl     @1
        jsr     @recv
        bra     @2
@1:     jsr     @save
@2:     jsr     incWR1
        add     #8
        bcc     :+
        inc     r5H
:       tay
        dec     r4L
        bne     @loop2
        inc     r2H
        dec     r3H
        bne     @loop1
        rts

@save:	lda     (r5),y
        tax
        tya
        pha
        ldy     #0
        txa
        sta     (r1),y
        pla
        rts

@recv:	tya
        pha
        ldy     #0
        lda     (r1),y
        tax
        pla
        tay
        txa
        sta     (r5),y
        tya
        rts

loadScrRecoverlintab_yEntry:
	ldy     #0
@loop:  lda     scrrecvtabs,x
        sta     r1,y
        inx
        iny
        cpy     #6
        bne     @loop
        rts

.if LANG=LANG_EN
;              r1L/r1H  r2L  r2H  r3L  r3H
;                ptr      x    y wdth hght (x and width in cards, y and height in pixels)
scrrecvtabs:
scrrecvtab_geos:
	.word   MEM_SCRRECV
	.byte             0,  15,  10, 128
scrrecvtab_file:
	.word   MEM_SCRRECV
	.byte             3,  15,   7, 100
scrrecvtab_edit:
	.word   MEM_SCRRECV
	.byte             6,  15,   7,  44
scrrecvtab_paste:
	.word   MEM_SCRRECV + 1408		; XXX 7*44=308 should be enough
	.byte            13,  15,   8,  30
scrrecvtab_options:
	.word   MEM_SCRRECV
	.byte             8,  15,  15, 114
scrrecvtab_page:
	.word   MEM_SCRRECV
	.byte            13,  15,  13, 100
scrrecvtab_font:
	.word   MEM_SCRRECV
	.byte            17,  15,  10, 114
scrrecvtab_fontsize:
	.word   MEM_SCRRECV + 1408		; XXX 10*114=1140 should be enough
	.byte            26,  15,   9, 114
scrrecvtab_style:
	.word   MEM_SCRRECV
	.byte            20,  15,  12, 100
scrrecvtab_dlgbox:
	.word   MEM_SCRRECV
	.byte             8,  32,  25, 104	; biggest one, takes up 2600 bytes
scrrecvtab_deskacc:
	.word   MEM_SCRRECV
	.byte             0,   0,  40,  36	; for deskaccs, save top 36 lines: menu and ruler
scrrecvtab_preview:
	.word   MEM_SCRRECV + 808		; XXX 7*100=700 should be enough
	.byte             0,   0,  40,  36
.elseif LANG=LANG_DE
;              r1L/r1H  r2L  r2H  r3L  r3H
;                ptr      x    y wdth hght (x and width in cards, y and height in pixels)
scrrecvtabs:
scrrecvtab_geos:
	.word   MEM_SCRRECV
	.byte             0,  14,  10, 128
scrrecvtab_file:
	.word   MEM_SCRRECV
	.byte             3,  14,  11, 100
scrrecvtab_edit:
	.word   MEM_SCRRECV
	.byte             7,  14,  12,  44
scrrecvtab_paste:
	.word   MEM_SCRRECV + 1408
	.byte            18,  14,   9,  30
scrrecvtab_options:
	.word   MEM_SCRRECV
	.byte             9,  14,  15, 114
scrrecvtab_page:
	.word   MEM_SCRRECV
	.byte            12,  14,  13, 100
scrrecvtab_font:
	.word   MEM_SCRRECV
	.byte            16,  14,  10, 114
scrrecvtab_fontsize:
	.word   MEM_SCRRECV + 1408
	.byte            25,  14,   9, 114
scrrecvtab_style:
	.word   MEM_SCRRECV
	.byte            20,  14,  13, 100
scrrecvtab_dlgbox:
	.word   MEM_SCRRECV
	.byte             8,  32,  25, 104
scrrecvtab_deskacc:
	.word   MEM_SCRRECV
	.byte             0,   0,  40,  36
scrrecvtab_preview:
	.word   MEM_SCRRECV + 808
	.byte             0,   0,  40,  36
.endif

;---------------------------------------------------------------
; Main Process Table
;---------------------------------------------------------------
processTable:
	;       function               divisor
	.word   J2_selectionProcess,   1
	.word   J2_appOtherPressVec,   1
        .word   J2_process2,           1
        .word   mouseScrollingProcess, 1
NUM_PROCESSES = (* - processTable) / 4

;---------------------------------------------------------------
; DoIcons Data
;---------------------------------------------------------------
doicons_data:
	.byte   4			; number of icons
	.word   5			; mouse x
	.byte   5			; mouse y

doicons_graphdata_ptr1:			; ruler
	.word   0			; graphic data - filled by init code: graph_ruler
	.byte   0     			; x
	.byte   16    			; y
        .byte   40    			; width
        .byte   8     			; height
        .word   callbackRuler		; service routine

        .word   graph_marginbar		; margin bar below ruler
        .byte   0     			; x
        .byte   24    			; y
        .byte   40    			; width
        .byte   5     			; height
        .word   callbackMarginBar	; service routine

doicons_graphdata_ptr2:			; ruler buttons (justification/spacing)
	.word   0     			; graphic data - filled by init code: graph_rulerbuttons
	.byte   0     			; x
	.byte   29    			; y
        .byte   40    			; width
        .byte   7     			; height
        .word   J2_callbackJustification; service routine

doicons_graphdata_ptr3:			; page indicator box
	.word   0     			; graphic data - filled by init code: graph_pageindicatorbox
	.byte   24    			; x
	.byte   0     			; y
        .byte   2     			; width
        .byte   16    			; height
        .word   _editPageIndicator 	; service routine

;---------------------------------------------------------------
; Ruler Graphics Data
;---------------------------------------------------------------
graph_marginbar:
	.byte   100, %00000000		; 5 pixel lines
	.byte   100, %00000000		; clear pixels

;---------------------------------------------------------------
rulerIcons:
rulerM:	.byte   %0001000,%00100000; ___█_____█_____
	.byte   %0001100,%01100000; ___██___██_____
        .byte   %0001010,%10100000; ___█_█_█_█_____
        .byte   %0001001,%00100000; ___█__█__█_____
        .byte   %0001000,%00100000; ___█_____█_____
rulerTab:
        .byte   %0000001,%00000000; ______█________
        .byte   %0000010,%10000000; _____█_█_______
        .byte   %0000100,%01000000; ____█___█______
        .byte   %0000100,%01000000; ____█___█______
        .byte   %0000111,%11000000; ____█████______
rulerDTab:
        .byte   %0000001,%00000000; ______█________
        .byte   %0000011,%10000000; _____███_______
        .byte   %0000111,%11000000; ____█████______
        .byte   %0000111,%11000000; ____█████______
        .byte   %0000111,%11000000; ____█████______
rulerP:
        .byte   %0000011,%11000000; _____████______
        .byte   %0000010,%01000000; _____█__█______
        .byte   %0000011,%11000000; _____████______
        .byte   %0000010,%00000000; _____█_________
        .byte   %0000010,%00000000; _____█_________

;---------------------------------------------------------------
; initTextPrompt
;
; Function:  Wrapper around InitTextPrompt KERNAL API that
;            takes the current font size and makes the prompt
;            two pixels wide.
;---------------------------------------------------------------
initTextPrompt:
	LoadB   CPU_DATA, IO_IN

        lda     #2			; prompt on top
        ora     mobprior
        sta     mobprior

        lda     cursor0+cursor::height
        bne     @1

        lda     #SYSTEM_FONT_SIZE
@1:	cmp     #MAX_FONT_SIZE
        bcc     @2
        lda     #MAX_FONT_SIZE
@2:	jsr     InitTextPrompt

        ldx     #62			; make prompt 2 pixels wide		l
@loop:  lda     spr1pic,x		; [XXX why not use VIC-II x-expand?]
        cmp     #%10000000
        bne     @skip
        lda     #%11000000
        sta     spr1pic,x
@skip:	dex
        bpl     @loop

        LoadB   CPU_DATA, RAM_64K
        rts

;---------------------------------------------------------------
; showError
;
; Function:  Show an error dialog with a custom message.
;
; Pass:      y:a  pointer to error message string
;---------------------------------------------------------------
showError:
	sta     r5L
        sty     r5H
        txa
        pha
	LoadW   r14, txt_empty
showError2:
	lda     #<@dlgbox_error
        ldx     #>@dlgbox_error
        jsr     doDlgBox
        pla
        tax
        sec
        rts

@dlgbox_error:
	.byte   DEF_DB_POS|1
	.byte   DBVARSTR
	.byte   16
	.byte   16
	.byte   r5			; first line
	.byte   DBVARSTR
	.byte   16
	.byte   32
	.byte   r14			; second line
	.byte   OK
	.byte   17
	.byte   72
txt_empty:
	.byte   0			; doubles as empty string

;---------------------------------------------------------------
; showIOError
;
; Function:  Show an I/O error dialog with an error code and
;            message.
;
; Pass:      x    error code ($00-$FF)
;            y:a  pointer to error message string
;---------------------------------------------------------------
showIOError:
	sta     r14L
        sty     r14H
        txa				; code
        pha
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        jsr     @digitToHex
        sta     txt_ioerror_errcode
        txa
        and     #$0F
        jsr     @digitToHex
        sta     txt_ioerror_errcode+1
	LoadW   r5, txt_ioerror
        jmp     showError2		; common code to show two line dialog

@digitToHex:
	cmp     #10
        bcs     @1
        add     #'0'
        bne     @rts
@1:	add     #'A'-10
@rts:	rts

;---------------------------------------------------------------
; pushR0ToR14
;
; Function:  Push register r0 through r14 onto the stack.
;---------------------------------------------------------------
pushR0ToR14:
	PopW    returnAddress
        ldx     #0
@loop:  lda     r0L,x
        pha
        inx
        cpx     #2 * 15
        bne     @loop
        bra     pushWReturnAddress

;---------------------------------------------------------------
; popR0ToR14
;
; Function:  Pop register r0 through r14 from the stack.
;---------------------------------------------------------------
popR0ToR14:
	PopW    returnAddress
        ldx     #2 * 15 - 1
@loop:  pla
        sta     r0L,x
        dex
        bpl     @loop

pushWReturnAddress:
	PushW   returnAddress
        rts

;---------------------------------------------------------------
; Font Metrics Cache
;---------------------------------------------------------------
setSystemFont:
	LoadW   curFont, SYSTEM_FONT_ID

;---------------------------------------------------------------
; loadFontMetrics
;
; Function:  Prepare cached font metrics for use.
;
; Pass:      curFont  font ID
;---------------------------------------------------------------
loadFontMetrics:
	CmpWI   curFont, SYSTEM_FONT_ID
	bne     @nsys

        PushB   r1H
        jsr     loadR1SystemFont
        jsr     _setFontFromFile
        PopB    r1H

@end1:	LoadB   curFontHeight, SYSTEM_FONT_SIZE
        LoadB   curBaselineOffset, SYSTEM_FONT_BASELINE
        LoadB   metricsUseSystem, $FF
        rts

@nsys:  ldx     #0			; find font id in metricsIds
@loop:	lda     metricsIds,x
        tay
        ora     metricsIds+8,x
        beq     @nfound
        cpy     curFont
        bne     @no
        lda     metricsIds+8,x
        cmp     curFont+1
        beq     @found
@no:	inx
        cpx     #MAX_FONTS_LOADED
        bne     @loop

        jsr     getNextIndexModulus8

	; not found in metrics cache
@nfound:
	PushB   r1H
        jsr     moveCurFontR1		; r1 = font id
        txa
        pha
        jsr     _setFontFromFile	; set font
        pla
        tax				; mod8 index
        PopB    r1H
        CmpWI   activeFont, SYSTEM_FONT_ID
	beq     @end1
        lda     curFont                     ; store font id in metricsIds
        sta     metricsIds,x
        lda     curFont+1
        sta     metricsIds+8,x
        lda     curHeight		; store height in table
        sta     metricsHeights,x
        lda     baselineOffset
        sta     metricsBaselineOffsets,x

        jsr     @getCachedFontMetrics
        jsr     calcCharWidths
        bra     @end2

@found:	jsr     @getCachedFontMetrics

@end2:  LoadB   metricsUseSystem, 0
        rts

@getCachedFontMetrics:
	lda     cachedFontCharWidthsPtrsLo,x
        sta     metricsWidths
        lda     cachedFontCharWidthsPtrsHi,x
        sta     metricsWidths+1
        lda     metricsHeights,x
        sta     curFontHeight
        lda     metricsBaselineOffsets,x
        sta     curBaselineOffset
        rts

;---------------------------------------------------------------
; _setFontFromFile
;
; Function:  Variant of _setFontFromFile that preserved r11, 15.
;            (0-7).
;---------------------------------------------------------------
_setFontFromFile:
	PushW_r15
        PushW   r11
        jsr     setFontFromFile
        PopW    r11
        PopW_r15
        rts

;---------------------------------------------------------------
; getNextIndexModulus8
;
; Function:  Return next index for the metrics cache mod 8
;            (0-7).
;
; Note:      While the font data manager can cache up to 8 fonts
;            based on an LRU strategy, but no more than 7000
;            bytes, the metrics cache keeps track of the font
;            metrics (height, baseline, widths) of the 8 least
;            recently loaded fonts.
;---------------------------------------------------------------
getNextIndexModulus8:
	ldx     metricsFifoCounter
        inx
        cpx     #MAX_FONTS_LOADED
        bne     @1
        ldx     #0
@1:	stx     metricsFifoCounter
        rts

;---------------------------------------------------------------
; calcCharWidths
;
; Function:  Create table that stores the width of each
;            character, from the font's character offset table.
;
; Pass:      curIndexTable  pointer to offset table (source)
;            metricsWidths  pointer to width table (dest)
;---------------------------------------------------------------
calcCharWidths:
	MoveW   metricsWidths, r0
        lda     #$7F - $20		; all characters in font
        sta     r2H
        asl     a
        sta     r2L
@loop:  ldy     r2L
        iny
        iny
        lda     (curIndexTable),y	; char n+1 index
        dey
        dey
        sec
        sbc     (curIndexTable),y	; minus char n index
        ldy     r2H
        sta     (r0),y			; = character width
        dec     r2L
        dec     r2L
        dec     r2H
        bpl     @loop
        rts

;---------------------------------------------------------------
; getCharWidth
;
; Function:  Get the width of a specified char of the currently
;            active metrics set (-> loadFontMetrics).
;
; Pass:      a   character
;            x   currentMode
;
; Return:    a   width
;---------------------------------------------------------------
getCharWidth:
	bit     metricsUseSystem
        bpl     @cache
        jsr     GetRealSize		; ask the OS
        tya
        rts

@cache:	sub     #$20			; ASCII -> table index
        MoveW_y metricsWidths, r14
        tay
        lda     (r14),y			; width
        sta     metricsTmp
        txa				; mode
        and     #SET_BOLD
        beq     :+
        inc     metricsTmp		; add one
:	txa
        and     #SET_OUTLINE
        beq     :+
        inc     metricsTmp		; add 3
        inc     metricsTmp
:	lda     metricsTmp
        rts

;---------------------------------------------------------------
; calcEffectiveFontHeight
;
; Function:  Calculate the font height, with currentMode
;            applied.
;
; Pass:      x  currentMode
;
; Return:    a  effective font height
;---------------------------------------------------------------
calcEffectiveFontHeight:
	ldy     curFontHeight
        txa
        and     #SET_OUTLINE
        beq     @1
        iny				; outline: +2 pixels
        iny
@1:	txa
        and     #SET_SUPERSCRIPT | SET_SUBSCRIPT
        beq     @2
        tya
        asl     a
        adc     curFontHeight		; super/sub: + 1/2 height
        lsr     a
        rts

@2:	tya
        rts

;---------------------------------------------------------------
; calcEffectiveBaselineOffset2
;
; Function:  Calculate the baseline offset, with currentMode
;            applied. This accounts for superscript.
;
; Pass:      x  currentMode
;
; Return:    a  effective baseline offset
;---------------------------------------------------------------
calcEffectiveBaselineOffset2:
	jsr     calcEffectiveBaselineOffset
        tay
        txa
        and     #SET_SUPERSCRIPT
        beq     @1
        tya
        asl     a
        adc     curBaselineOffset	; add 1/2 baseline offset
        lsr     a
        tay
@1:	tya
        rts

;---------------------------------------------------------------
; calcEffectiveBaselineOffset
;
; Function:  Calculate the baseline offset, with currentMode
;            applied. This does not account for superscript.
;
; Pass:      x  currentMode
;
; Return:    a  effective baseline offset
;---------------------------------------------------------------
calcEffectiveBaselineOffset:
	ldy     curBaselineOffset
        txa
        and     #SET_OUTLINE
        beq     @1
        iny
        iny
@1:	tya
        rts

;---------------------------------------------------------------

.define cachedFontCharWidthsPtrs cachedFontCharWidths0,cachedFontCharWidths1,cachedFontCharWidths2,cachedFontCharWidths3,cachedFontCharWidths4,cachedFontCharWidths5,cachedFontCharWidths6,cachedFontCharWidths7
cachedFontCharWidthsPtrsLo:
	.lobytes cachedFontCharWidthsPtrs
cachedFontCharWidthsPtrsHi:
	.hibytes cachedFontCharWidthsPtrs

;---------------------------------------------------------------
; swapUserZp
;
; Function:  Swap the user zero page ($80-$FF) with the buffer.
;
; Note:      This needs to be called immediately before and
;            after any KERNAL API call that may access the
;            disk driver and asll well as all printer calls.
;            [XXX $FB-$FF would not need to be swapped]
;---------------------------------------------------------------
swapUserZp:
	php
        pha
        txa
        pha
        tya
        pha
        ldx     #$7F
@loop:  ldy     userzp,x		; 4+p
        lda     userzp_copy,x		; 4
        sta     userzp,x		; 4
        tya				; 2
        sta     userzp_copy,x		; 5
        dex				; 2
        bpl     @loop			; 3
        pla				;---
        tay				; 24+p
        pla				;      * 128 ~= 3000
        tax
        pla
        plp
        rts

;---------------------------------------------------------------
; doDlgBox
;
; Function:  DoDlgBox wrapper with custom screen save/recover.
;
; Pass:      x:a  pointer to dialog box data
;---------------------------------------------------------------
doDlgBox:
	sta     r0L
        stx     r0H
        jsr     clearRecoverOffsetB
        ldx     #scrrecvtab_dlgbox-scrrecvtabs
        stx     a2L
	PushW   r5
        jsr     screenSave
	PopW    r5
        jmp     DoDlgBox

;---------------------------------------------------------------
; pushRulerData
;
; Function:  Push all of rulerData1 onto the stack.
;---------------------------------------------------------------
pushRulerData:
	PopW    r0
        jsr     incWr0			; return address
        ldx     #0
@loop:  lda     rulerData1,x		; push 26 bytes
        pha
        inx
        cpx     #.sizeof(ruler)
        bne     @loop
	PushW   curFont			; also curFont
        PushB   currentMode		; and currentMode
        jmp     (r0)			; return

;---------------------------------------------------------------
; popRulerData
;
; Function:  Pop all of rulerData1 from the stack
;---------------------------------------------------------------
popRulerData:
	PopW    r0
        jsr     incWr0			; return address
        PopB    currentMode		; restore currentMode
        PopW    curFont			; restore curFont
        ldx     #.sizeof(ruler)-1
@loop:  pla
        sta     rulerData1,x		; restore 26 bytes
        dex
        bpl     @loop
        jmp     (r0)

;---------------------------------------------------------------
; resetToPageStart
;
; Function:  Reset pointers to allow iterating over page from
;            the start.
;---------------------------------------------------------------
resetToPageStart:
	LoadW   pageTextHeight, 0
        lda     PAGE_RULER+ruler::justification
        sta     rulerData1+ruler::justification
        sta     justification

LoadR15_MEM_PAGE:
	LoadW   r15, MEM_PAGE
        rts

;---------------------------------------------------------------
; subSideFlippingPixelOffset
;
; Function:  Subtract 0, 160, or 320 based on sideFlippingOffset
;            = 0, 1, 2.
;
; Pass:      x  zero page address for argument/result
;---------------------------------------------------------------
subSideFlippingPixelOffset:
	lda     sideFlippingOffset
        beq     rts2
        cmp     #1
        beq     @1
        jsr     @1
@1:	lda     #160
;---------------------------------------------------------------
; subWBI
;
; Function:  Subtract 8 value from 16 bit register.
;
; Pass:      a  value to be subtracted
;            x  zero page address to be subtracted from
;---------------------------------------------------------------
subWBI:	sta     @sbc+1
        lda     0,x
        sec
@sbc:	sbc     #0
        sta     0,x
        bcs     rts2
        dec     1,x
rts2:	rts

;---------------------------------------------------------------
; addSideFlippingPixelOffset
;
; Function:  Add 0, 160, or 320 based on sideFlippingOffset
;            = 0, 1, 2.
;
; Pass:      x  zero page address for argument/result
;---------------------------------------------------------------
addSideFlippingPixelOffset:
	lda     sideFlippingOffset
        beq     rts2
        cmp     #1
        beq     @1
        jsr     @1
@1:	lda     #160
        jmp     addWIToZp

;---------------------------------------------------------------
; getEffectiveRightMargin
;
; Function:  Return right margin or page width, whichever is
;            smaller.
;
; Pass:      x  zero page address for result
;---------------------------------------------------------------
getEffectiveRightMargin:
	lda     rulerData1+ruler::right_margin
        sta     0,x
        lda     rulerData1+ruler::right_margin+1
        sta     1,x
        lda     1,x
        cmp     pageWidth2+1
        bne     @1
        lda     0,x
        cmp     pageWidth2
@1:	bcc     @rts
        lda     pageWidth2
        sta     0,x
        lda     pageWidth2+1
        sta     1,x
@rts:	rts

;---------------------------------------------------------------
; 16 Bit Helpers
;
; These small functions perform frequently used 16 bit
; operations and help keep the code size small.
;---------------------------------------------------------------
addWI4R15:
	jsr     incWR15
addWI3R15:
	jsr     incWR15
        jsr     incWR15
incWR15:
	IncW    r15
	clc
        rts

incWr0:
	IncW    r0
	rts
;---------------------------------------------------------------
incWR1:
	IncW    r1
	rts
;---------------------------------------------------------------
addRulerSizeToR15:
	ldx     #r15
addRulerSizeToZp:
	lda     #.sizeof(ruler)+1	; 27
        bne     addWIToZp

addWI4ToR3:
	ldx     #r3
addWI4ToZp:
	lda     #4
addWIToZp:
	clc
        adc     0,x
        sta     0,x
        bcc     @end
        inc     1,x
        clc
@end:	rts
;---------------------------------------------------------------
loadR0FnBuffer:
	LoadW   r0, fnBuffer
        rts
;---------------------------------------------------------------
loadWR3Zero:
	LoadW   r3, 0
        rts
;---------------------------------------------------------------
LoadWR4ScPixWidthMinus1:
	LoadW   r4, SC_PIX_WIDTH-1
        rts
;---------------------------------------------------------------
ldR4DiskBlkBuf:
	LoadW   r4, diskBlkBuf
        rts
;---------------------------------------------------------------
setKeyVector:
	LoadW   keyVector, J2_appKeyVector
        rts
;---------------------------------------------------------------
loadCursor0PtrPageCardset:
	LoadW   cursor0+cursor::ptr, PAGE_CARDSET
        rts
;---------------------------------------------------------------
loadR1SystemFont:
	LoadW  r1, SYSTEM_FONT_ID
        rts
;---------------------------------------------------------------
loadWR11Zero:
	LoadW   r11, 0
        rts
;---------------------------------------------------------------
moveCurFontR1:
	MoveW   curFont, r1
        rts
;---------------------------------------------------------------
moveCursor0FontCurFont:
	MoveW   cursor0+cursor::font, curFont
        rts
;---------------------------------------------------------------
moveCursor0Ptr_r15:
	MoveW   cursor0+cursor::ptr, r15
        rts
;---------------------------------------------------------------
moveCursor1Ptr_r15:
	MoveW   cursor1+cursor::ptr, r15
        rts
;---------------------------------------------------------------
moveR15_cursor3Ptr:
	MoveW   r15, cursor3+cursor::ptr
        rts
;---------------------------------------------------------------
L27BC:  jsr     addLineHeightToPageHeight
moveCursor3Ptr_r15:
	MoveW   cursor3+cursor::ptr, r15
        rts
;---------------------------------------------------------------
moveWR2R11:
	MoveW   r2, r11
        rts
;---------------------------------------------------------------
moveWR15R14:
	MoveW   r15, r14
        rts
;---------------------------------------------------------------
addLineHeightToPageHeight:
	lda     pageTextHeight
        add     cursor3+cursor::height
        sta     pageTextHeight
        lda     pageTextHeight+1
        adc     #0
        sta     pageTextHeight+1
        rts
;---------------------------------------------------------------
; effectively returns whether there is currently a selection
; Z=1: no
cmpCursor0_1Ptr:
	CmpW    cursor0+cursor::ptr, cursor1+cursor::ptr
	rts
;---------------------------------------------------------------
cmpR15Cursor0Ptr:
	CmpW    r15, cursor0+cursor::ptr
	rts
;---------------------------------------------------------------
cmpCursor0PtrPageCardset:
	CmpWI   cursor0+cursor::ptr, PAGE_CARDSET
	rts
;---------------------------------------------------------------
cmpPageEndPtr2PageCardSet:
	CmpWI   pageEndPtr2, PAGE_CARDSET
	rts
;---------------------------------------------------------------
cmpPageEndPtr2R15:
	CmpW    pageEndPtr2, r15
	rts
;---------------------------------------------------------------
CmpR15Cursor1Ptr:
	CmpW    r15, cursor1+cursor::ptr
	rts
;---------------------------------------------------------------
CmpR15Cursor3Ptr:
	CmpW    r15, cursor3+cursor::ptr
	rts
;---------------------------------------------------------------
CmpCursor2_3Ptr:
	CmpW    cursor2+cursor::ptr, cursor3+cursor::ptr
	rts
;---------------------------------------------------------------
CmpCursor2_0Ptr:
	CmpW    cursor2+cursor::ptr, cursor0+cursor::ptr
	rts
;---------------------------------------------------------------
CmpR1SystemFont:
	CmpWI r1, SYSTEM_FONT_ID
	rts
;---------------------------------------------------------------
CmpR2WindowBottom:
	CmpBI   r2H, 0
        bne     @1
        CmpB    r2L, windowBottom
@1:	rts
;---------------------------------------------------------------
CmpR11ScrWidth:
	CmpWI   r11, SC_PIX_WIDTH	; [XXX only used once]
	rts

;---------------------------------------------------------------
; pushR15/popR15
;
; These functions push and pop r15. It's way less efficient,
; but saves a few bytes every time.
;---------------------------------------------------------------
pushR15:
	PopW    returnAddress
	PushW   r15
pp15ret:
	PushW   returnAddress
        rts

popR15:
	PopW    returnAddress
	PopW    r15
        bra     pp15ret

;---------------------------------------------------------------
; Ruler Copy
;
; These functions copies between the two ruler structs.
;---------------------------------------------------------------
copyRuler1To2:
	ldx     #rulerData1-rulerData1
        ldy     #rulerData2-rulerData1
        bne     copyRuler

copyRuler2To1:
	ldx     #rulerData2-rulerData1
        ldy     #rulerData1-rulerData1
copyRuler:
	LoadB   r0L, .sizeof(ruler)
@loop:  lda     rulerData1,x
        sta     rulerData1,y
        inx
        iny
        dec     r0L
        bne     @loop
        rts

;---------------------------------------------------------------
; Cursor Copy
;
; This is a collection of functions that copies between the
; four instances of the "cursor" datastructure.
;---------------------------------------------------------------
L28A9:  CmpW    cursor1+cursor::ptr, cursor2+cursor::ptr
L28B3:  bne     copyCursor3To0
        beq     copyCursor3To1

copyCursor1To3:
	ldy     #cursor3-userzp
        ldx     #cursor1-userzp
        bne     copyCursor

copyCursor0ToY:
	ldx     #cursor0-userzp ; [XXX this ships ldy, then overwrites X ->!?]
        .byte   $2C
copyCursor0To1:
	ldy     #cursor1-userzp
        ldx     #cursor0-userzp
        beq     copyCursor
	.assert cursor0-userzp = 0, error

copyCursor3To2:
	ldy     #cursor2-userzp
        .byte   $2C
copyCursor3To1:
	ldy     #cursor1-userzp
        .byte   $2C
copyCursor3To0:
	ldy     #cursor0-userzp
        ldx     #cursor3-userzp
copyCursor:
	LoadB   r0L, .sizeof(cursor)
@loop:  lda     userzp,x
        sta     userzp,y
        inx
        iny
        dec     r0L
        bne     @loop
        rts

;---------------------------------------------------------------
; Disk access wrappers
;
; These are wrappers around the most common KERNAL calls that
; access disk. They swap the zero page before and after the
; call.
;---------------------------------------------------------------
_ReadFile:
	lda     #ReadFile-GetBlock
        .byte   $2C
_ReadByte:
	lda     #ReadByte-GetBlock
        .byte   $2C
_CloseRecordFile:
	lda     #CloseRecordFile-GetBlock
        .byte   $2C
_InsertRecord:
	lda     #InsertRecord-GetBlock
        .byte   $2C
_DeleteRecord:
	lda     #DeleteRecord-GetBlock
        .byte   $2C
_AppendRecord:
        lda     #AppendRecord-GetBlock
        .byte   $2C
_UpdateRecordFile:
        lda     #UpdateRecordFile-GetBlock
        .byte   $2C
_OpenDisk:
        lda     #OpenDisk-GetBlock
        .byte   $2C
_FindFile:
	lda     #FindFile-GetBlock
        .byte   $2C
_GetBlock:
	lda     #GetBlock-GetBlock
        .byte   $2C
_PutBlock:
	lda     #PutBlock-GetBlock
        add     #<GetBlock
        sta     @jmp+1
        lda     #0
        adc     #>GetBlock
        sta     @jmp+2
        jsr     swapUserZp
@jmp:   jsr     GetBlock
        jmp     swapUserZp

;---------------------------------------------------------------
; mouseScrollingProcess
;
; Function:  60 Hz process to
;            * switch between left and right half of 480px/640px
;              wide document, if mouse hits left or right of
;              screen
;            * scroll up and down, if mouse hits top or bottom
;              of screen
;---------------------------------------------------------------
mouseScrollingProcess:
	lda     mouseXPos		; mouse at leftmost column
        ora     mouseXPos+1
        bne     @1			; no

        lda     sideFlippingOffset	; already at the left?
        bne     @left			; no

        jmp     _mouseScrollUpDown

@1:	cmp     #<(SC_PIX_WIDTH-1)	; mouse at rightmost column?
        bne     @2
        lda     mouseXPos+1
        cmp     #>(SC_PIX_WIDTH-1)
        bne     @2			; no

        ldy     sideFlippingOffset	; where are we flipping-wise?
        beq     @right			; very left, we can flip
        dey
        bne     @2			; > 1, we can't flip

        ldx     pageWidth1+1
        dex
        bne     @right

@2:	jmp     _mouseScrollUpDown	; [XXX line further up could be reused]

@right:	jsr     pushCursor2
        jsr     pushRulerData
        jsr     killPrompt
        jsr     invertSelectionIfNeeded
        jsr     flipScreenRTL
	LoadW   sideFlippingRight_XXX, 160
        inc     sideFlippingOffset
        bne     @cont			; always

@left:	jsr     pushCursor2
        jsr     pushRulerData
        jsr     killPrompt
        jsr     invertSelectionIfNeeded
        jsr     flipScreenLTR
	LoadW   sideFlippingLeft_XXX, 160
        dec     sideFlippingOffset

@cont:	LoadW   mouseXPos, 160		; put mouse at horizontal center
        jsr     updatePageIndicator
        jsr     copyRuler2To1
        LoadB   curLine, 0
        jsr     L15D8			; ???
	LoadW_  sideFlippingRight_XXX, 0
	LoadW   sideFlippingLeft_XXX, SC_PIX_WIDTH-1
        jsr     L0862
        jsr     popRulerData
        jsr     setPromptFontMetricsUpdateRulerUI

        ldx     #9			; pop cursor2
@loop:	pla
        sta     cursor2,x
        dex
        bpl     @loop

        bit     zp_D9b
        bpl     @4

        MoveW	cursor2+cursor::ptr, r15
        MoveB   cursor2+cursor::srcline, r3L
        jsr     L08CD
        jsr     copyCursor3To2
        jsr     CmpCursor2_0Ptr
        bne     @3

        jsr     copyCursor3To0
        jmp     _mouseScrollUpDown

@3:	jsr     copyCursor3To1
@4:	jmp     _mouseScrollUpDown

;---------------------------------------------------------------
; pushCursor2
;
; Function:  Push all of cursor2 onto the stack.
;---------------------------------------------------------------
pushCursor2:
	PopW    r0			; get return address
        jsr     incWr0			; fix up
        ldx     #0
@loop:  lda     cursor2,x		; push all of cursor2
        pha
        inx
        cpx     #10
        bne     @loop
        jmp     (r0)			; return

;---------------------------------------------------------------
; flipScreenLTR/flipScreenRTL
;
; Function:  Move the left half of the screen to the right,
;            or the right half to the left. The source half
;            will be cleared.
;---------------------------------------------------------------
flipScreenLTR:
	ldy     #r6
        .byte   $2C

flipScreenRTL:
	ldy     #r5
        sty     r4L
        ldx     #36			; start line 36
@loop1:	jsr     getScanLineAdd160
        ldy     #0
@loop2:	lda     (r5),y			; copy
        sta     (r6),y
        lda     #0
        sta     (r5),y			; clear source
        tya
        add     #8			; next pixel
        tay
        cmp     #160			; half a screen's width?
        bne     @loop2
        inx
        cpx     #40
        bne     @loop1			; end line 40

        ldx     #40			; start line 40
@loop3:	jsr     getScanLineAdd160
        ldy     #160
@loop4:	dey
        lda     (r5),y			; copy
        sta     (r6),y
        lda     #0
        sta     (r5),y			; clear source
        tya
        bne     @loop4			; 160 bytes
        txa
        add     #8
        tax
        cmp     windowBottom
        bcc     @loop3			; until bottom of screen
        rts

getScanLineAdd160:
	jsr     GetScanLine
        ldy     r4L			; add 160 to register
        lda     0,y
        add     #160
        sta     0,y
        lda     1,y
        adc     #0
        sta     1,y
        rts

;---------------------------------------------------------------
; Strings
;---------------------------------------------------------------

	.include "strings2.inc"

;---------------------------------------------------------------
; appInit
;
; Function:  getWrite app entry point; loads record 1, decrypts
;            and executes it
;---------------------------------------------------------------
appInit:
	jsr     swapUserZp
	jsr     findDocDevice		; r2 = doc disk name
        lda     r0L
        sta     loadOpt
	MoveW   r3, argFilename

	; init app record loader
	LoadW   r6, fnBuffer
        lda     #APPLICATION
        sta     r7L
        lda     #1 ; max number
        sta     r7H
	LoadW   r10, appname
	jsr     swapUserZp
        jsr     FindFTypes
        jsr     loadR0FnBuffer
        jsr     OpenRecordFile
        jsr     i_MoveData		; copy index table
		.word   fileHeader+2
		.word   appIndexTable
		.word   2 * NUM_APP_RECORDS
	jsr     swapUserZp
        LoadB   curCodeRecord, $FF

	; load app record #1
        lda     #RECORD_1
        jsr     loadCode
        jsr     setKeyVector
	LoadW   otherPressVec, J2_appOtherPressVec

	; decrypt app record #1
        lda     #$EB
        eor     #$35
        sta     @2
	LoadW   r0, MEM_OVERLAY
	LoadW   r1, -4000 		; [XXX too much]
        ldy     #0
@1:	lda     (r0),y
@2 = * + 1
        eor     #$00
        sta     (r0),y
	IncW	r0
	IncW	r1
	bne     @1
        jmp     MEM_OVERLAY		; jump to code record #1

appname:
	.byte   "geoWrite    V2.1",0


;---------------------------------------------------------------
; findDocDevice
;
; Function:  Set docDrive according to document's disk name.
;
; Pass:      r2  name of disk that contains document
;---------------------------------------------------------------
findDocDevice:
	jsr     swapUserZp		; [XXX not necessary; curDrive is at $8xxx]
        lda     curDrive
        jsr     swapUserZp
        sta     appDrive
        sta     docDrive
	lda     r0L ; loadOpt
        and     #ST_LD_DATA | ST_PR_DATA
        beq     @rts
        ldy     #15
@loop:  lda     DrACurDkNm,y ; cmp disk A name
        cmp     (r2),y       ; r2 = doc disk name
        bne     @break
        dey
        bpl     @loop
        lda     #8
        sta     docDrive   ; yes, then drive 8
	rts

@break:	ldy     NUMDRV
        dey
        beq     @error
        lda     #9           ; else must be on drv 9
        sta     docDrive
@rts:	rts

@error:	lda     #<txt_same_disk
        ldy     #>txt_same_disk
        jsr     showError
	jsr     swapUserZp
        jmp     EnterDeskTop

	.include "strings3.inc"
