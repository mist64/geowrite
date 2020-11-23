; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  05 - startup/about, create, open, version conversion, paste text
;       run desk accessory
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------

; filename box
FNBOX_TOP_INSET		= 3
FNBOX_RIGHT_INSET	= 5
FNBOX_WIDTH		= 100
FNBOX_HEIGHT		= 10
FNBOX_INNER_INSET	= 5

DRIVE			= 21

.segment        "CODE5": absolute

        jmp     recover            ; 0
        jmp     showStartupMenu    ; 1
        jmp     renameDocument     ; 2
        jmp     openDocument       ; 3
        jmp     showAboutDialog    ; 4
        jmp     loadDeskAcc        ; 5
        jmp     exitToDesktop      ; 6
        jmp     readPageProperties ; 7
        jmp     makeFullPageWide   ; 8

; ----------------------------------------------------------------------------
showStartupMenu:                                ; 325F
	jsr     killPrompt
        jsr     drawFilenameBox
	; fill work area with 50% pattern
        jsr     i_GraphicsString
		.byte   NEWPATTERN
		.byte   2
		.byte   MOVEPENTO
		.word   0
		.byte   36
		.byte   RECTANGLETO
		.word   SC_PIX_WIDTH-1
		.byte   SC_PIX_HEIGHT-1
		.byte   NULL
        jsr     drawCopyrightFooter
        lda     #<dlgbox_startup
        ldx     #>dlgbox_startup
        jsr     doDlgBox
        lda     r0L
        cmp     #OK
        bne     @1
        jmp     createNewDocument
@1:	cmp     #OPEN
        bne     @2
        jmp     pickDocument
@2:	jmp     _exitToDesktop		; XXX code 5 already running

; ----------------------------------------------------------------------------
dlgbox_startup:
.if LANG=LANG_DE
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   16
	.byte   16
	.word   txt_please_select_option
	.byte   DBUSRICON
	.byte   12
        .byte   24
        .word   usericon_create
        .byte   DBTXTSTR
        .byte   16
        .byte   34
        .word   txt_new_document
        .byte   OPEN
        .byte   16
        .byte   48
        .byte   DBTXTSTR
        .byte   16
        .byte   58
        .word   txt_existing_document
        .byte   DBUSRICON
        .byte   11
        .byte   72
        .word   usericon_exit
        .byte   DBTXTSTR
        .byte   16
        .byte   82
        .word   txt_to_desktop
        .byte   NULL
.else
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   16
	.byte   16
	.word   txt_please_select_option
	.byte   DBUSRICON
	.byte   2
        .byte   24
        .word   usericon_create
        .byte   DBTXTSTR
        .byte   71
        .byte   34
        .word   txt_new_document
        .byte   OPEN
        .byte   2
        .byte   48
        .byte   DBTXTSTR
        .byte   71
        .byte   58
        .word   txt_existing_document
        .byte   DBUSRICON
        .byte   2
        .byte   72
        .word   usericon_exit
        .byte   DBTXTSTR
        .byte   71
        .byte   82
        .word   txt_to_desktop
        .byte   NULL
.endif

usericon_create:
	.word   graph_create
	.byte   0
	.byte   0
.if LANG=LANG_DE
        .byte   7
.else
	.byte   6
.endif
	.byte   16
	.word   callbackCreate
usericon_exit:
	.word   graph_exit
	.byte   0
	.byte   0
.if LANG=LANG_DE
        .byte   7
.else
	.byte   6
.endif
        .byte   16
	.word   callbackExit

; ----------------------------------------------------------------------------

callbackDrive:
        lda     #DRIVE
        bne     _1

callbackCreate:
	lda     #OK
        bne     _1

callbackExit:
	lda     #CANCEL
_1:	sta     sysDBData
        jmp     RstrFrmDialogue

; ----------------------------------------------------------------------------

; graph_create
; graph_exit

	.include "graph2.s"

; ----------------------------------------------------------------------------
drawCopyrightFooter:
	lda     showCopyrightFlag	; only show once
        bne     @rts
        dec     showCopyrightFlag
	LoadW   r1, SYSTEM_FONT_ID
        jsr     setFontFromFile
        LoadB   currentMode, 0
        jsr     i_GraphicsString
		.byte   NEWPATTERN
		.byte   0
		.byte   MOVEPENTO
		.word   68
		.byte   148
		.byte   RECTANGLETO
		.word   251
		.byte   191
		.byte   MOVEPENTO
		.word   68
		.byte   148
		.byte   FRAME_RECTO
		.word   251
		.byte   191
		.byte   NULL
        jsr     i_PutString
		.word   128
		.byte   161
		.byte   BOLDON,"- geoWrite -"
		.byte   GOTOXY
		.word   82
		.byte   183
		.byte   PLAINTEXT,"Copyright 1987 Berkeley Softworks",0
@rts:	rts

; ----------------------------------------------------------------------------
createNewDocument:
	lda     #$FF
        jsr     queryNewFilename
        cmp     #CANCEL
        beq     @3
	LoadW   r6, fnBuffer
        jsr     _FindFile
        cpx     #FILE_NOT_FOUND
        beq     @1
        bnex    @2
        lda     #<txt_file_exists
        ldy     #>txt_file_exists
        jsr     showError
        bra     createNewDocument

@1:	jsr     L37DC
        bnex    @2
        stx     nextPageOpen
        stx     curPage
        stx     currentMode
        stx     privFHData+privfhdata::titleNlqFlag
        stx     privFHData+privfhdata::headerHeight
        stx     privFHData+privfhdata::headerHeight+1
        stx     privFHData+privfhdata::footerHeight
        stx     privFHData+privfhdata::footerHeight+1
        stx     privFHData+privfhdata::startPageNo+1
        inx
        stx     privFHData+privfhdata::startPageNo
	MoveW   printerPageHeight, privFHData+privfhdata::pageHeight
	LoadW   pageWidth1, PAGE_WIDTH_NARROW-1
        jsr     setupOptionsMenu
        LoadB   hasPagePropMetadata, 0
        jsr     setSystemFont
        jsr     setupPageIndicatorSprite
        jmp     showFilenameInBox
@2:	lda     #<txt_creating_file
        ldy     #>txt_creating_file
        jsr     showIOError
@3:	jmp     showStartupMenu

; ----------------------------------------------------------------------------
pickDocument:
	LoadW   usericon_drive, 0
        jsr     getDiskIcon                           ; 345B 20 A0 36                  .6
        tax                                     ; 345E AA                       .
        beq     L346B                           ; 345F F0 0A                    ..
	LoadW   usericon_drive, graph_drive
L346B:  LoadB   L35B8, 0
        jsr     isDocOnAppDiskOrRamDisk                           ; 3470 20 91 36                  .6
        bcc     L347A                           ; 3473 90 05                    ..
        LoadB   L35B8, 6
L347A:  jsr     setDocDrive                           ; 347A 20 5F 0E                  _.
        jsr     L36C4                           ; 347D 20 C4 36                  .6
        LoadB   fnBuffer, 0
	LoadW   r7, 7
	LoadW   r10, txt_writeimage
	LoadW   r5, fnBuffer
        lda     #<dlgbox_getfiles                            ; 349D A9 9F                    ..
        ldx     #>dlgbox_getfiles                            ; 349F A2 35                    .5
        jsr     swapUserZp                        ; 34A1 20 39 26                  9&
        jsr     doDlgBox                        ; 34A4 20 56 26                  V&
        jsr     swapUserZp                        ; 34A7 20 39 26                  9&
        lda     r0L                             ; 34AA A5 02                    ..
        cmp     #CANCEL                         ; 34AC C9 02                    ..
        beq     L34CB                           ; 34AE F0 1B                    ..
        cmp     #DRIVE                            ; 34B0 C9 15                    ..
        bne     L34BA                           ; 34B2 D0 06                    ..
        jsr     swapDrive                       ; 34B4 20 AC 36                  .6
        bra     pickDocument                    ; 34B8 50 99                    P.

L34BA:  cmp     #DISK                           ; 34BA C9 06                    ..
        bne     openDocument                           ; 34BC D0 10                    ..
        lda     #<txt_insert_new_disk
        ldy     #>txt_insert_new_disk
        jsr     showError                       ; 34C2 20 52 24                  R$
        jsr     _OpenDisk                       ; 34C5 20 F5 28                  .(
        bra     pickDocument                    ; 34C9 50 88                    P.

L34CB:  jmp     showStartupMenu2                ; 34CB 4C 86 35                 L.5

openDocument:
	jsr     setDocDrive
        lda     fnBuffer
        bne     :+
        jmp     showStartupMenu2	; nothing selected
:	LoadW   r6, fnBuffer
        jsr     _FindFile
        beqx    :+
        jmp     showErrorOpeningFile
:	MoveW   dirEntryBuf+OFF_GHDR_PTR, docFileHeaderTrkSec
        ldy     #0
        lda     (r5),y ; file type
        and     #$40   ; write protected
        beq     @skip

	PushW   r5
        lda     #<dlgbox_warning_write_protected
        ldx     #>dlgbox_warning_write_protected
        jsr     doDlgBox
	PopW    r5
        lda     r0L
        cmp     #CANCEL
        beq     showStartupMenu2

@skip:	LoadW   pageWidth1, PAGE_WIDTH_WIDE-1
        jsr     testFileVersion
        php
        cpy     #0
        bne     @error
        plp
        beq     @3
        bcc     @1
        lda     #<txt_document_version_error
        ldy     #>txt_document_version_error
        jsr     showError
        bra     showStartupMenu2
					; convert 1.x file if necessary
@1:	LoadW_y pageWidth1, PAGE_WIDTH_NARROW-1
        cmp     #'2'
        bne     @2
        cpx     #'0'
        beq     @3
@2:	ldy     #'0'
        jsr     convertFile
        bnex    showStartupMenu2
        beq     @4

@3:	jsr     getPrivFHData

@4:	jsr     loadR0FnBuffer
        jsr     swapUserZp
        jsr     OpenRecordFile
        jsr     swapUserZp
        bnex    showErrorOpeningFile
        jsr     setupOptionsMenu
        jsr     readPageProperties
        lda     #0
        sta     curPage
        sta     nextPageOpen
        lda     #SC_PIX_HEIGHT-1
        sta     windowBottom
        jsr     setupPageIndicatorSprite
        jmp     showFilenameInBox

@error:
	plp
        tya
        tax
showErrorOpeningFile:
	lda     #<txt_opening_file
        ldy     #>txt_opening_file
        jsr     showIOError
showStartupMenu2: ; XXX extra jmp
	jmp     showStartupMenu

; ----------------------------------------------------------------------------
getPrivFHData:
	jsr     i_MoveData
		.word   fileHeader+O_GHP_FNAME+20
		.word	privFHData
		.word	9
        rts

; ----------------------------------------------------------------------------
txt_writeimage:
	.byte   "Write Image",0

; ----------------------------------------------------------------------------
dlgbox_getfiles:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte	$82
	.byte	$0A
	.word	txt_on_disk
	.byte	$0B
	.byte	$82
        .byte   $14
	.word	curDiskName
	.byte	DBGETFILES
	.byte	$04
	.byte	$04
	.byte	OPEN
	.byte	$11
        .byte   $19
        .byte	DBUSRICON
        .byte	$11
        .byte	$3B
        .word	usericon_drive
        .byte	CANCEL
	.byte	$11
        .byte   $4C
L35B8:  .byte   DISK
	.byt	$11
	.byte	$2A
	.byte	NULL

usericon_drive:
	.word   graph_drive
	.byte	0
	.byte	0
	.byte	6
	.byte	16
	.word	callbackDrive

; ----------------------------------------------------------------------------

; graph_drive

	.include "graph3.s"

; ----------------------------------------------------------------------------

dlgbox_warning_write_protected:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   16
	.byte   16
	.word   txt_warning
        .byte   DBTXTSTR
        .byte   16
        .byte   32
        .word   txt_file_is_write_protected
        .byte   DBUSRICON
        .byte   1
        .byte   72
        .word   usericon_write_protected
        .byte   CANCEL
        .byte   17
        .byte   72
        .byte   NULL

usericon_write_protected:
	.word   graph_write_protected
        .byte   0
        .byte	0
.if LANG=LANG_DE
        .byte	7
.else
        .byte	6
.endif
        .byte	16
        .word	L3633

L3633:	LoadB	sysDBData, $FF
	jmp	RstrFrmDialogue

; ----------------------------------------------------------------------------

; graph_write_protected

	.include "graph4.s"

; ----------------------------------------------------------------------------
isDocOnAppDiskOrRamDisk:
	ldy     docDrive
        cpy     appDrive
        beq     @ok			; same, ok
        lda     _driveType,y
        bmi     @ok			; RAM disk, ok
        sec
        rts

@ok:	clc
        rts

; ----------------------------------------------------------------------------
getDiskIcon:
	ldy     NUMDRV
        dey
        bne     @1
        lda     #0
        rts
@1:	lda     #DISK
        rts

; ----------------------------------------------------------------------------
swapDrive:
	ldx     docDrive                        ; 36AC A6 D8                    ..
        inx                                     ; 36AE E8                       .
        cpx     #9                              ; 36AF E0 09                    ..
        beq     L36B5                           ; 36B1 F0 02                    ..
        ldx     #8                              ; 36B3 A2 08                    ..
L36B5:  stx     docDrive                        ; 36B5 86 D8                    ..
        txa                                     ; 36B7 8A                       .
        jsr     swapUserZp                        ; 36B8 20 39 26                  9&
        jsr     SetDevice                       ; 36BB 20 B0 C2                  ..
        jsr     swapUserZp                        ; 36BE 20 39 26                  9&
        jmp     _OpenDisk                       ; 36C1 4C F5 28                 L.(

; ----------------------------------------------------------------------------
L36C4:  lda     #$84                            ; 36C4 A9 84                    ..
        sta     r0H                             ; 36C6 85 03                    ..
        lda     #$1E                            ; 36C8 A9 1E                    ..
        sta     r0L                             ; 36CA 85 02                    ..
        lda     docDrive                        ; 36CC A5 D8                    ..
        cmp     #$08                            ; 36CE C9 08                    ..
        beq     L36DA                           ; 36D0 F0 08                    ..
        lda     #$84                            ; 36D2 A9 84                    ..
        sta     r0H                             ; 36D4 85 03                    ..
        lda     #$30                            ; 36D6 A9 30                    .0
        sta     r0L                             ; 36D8 85 02                    ..
L36DA:  ldy     #$00                            ; 36DA A0 00                    ..
L36DC:  lda     (r0),y                         ; 36DC B1 02                    ..
        cmp     #$A0                            ; 36DE C9 A0                    ..
        beq     L36E8                           ; 36E0 F0 06                    ..
        sta     curDiskName,y                         ; 36E2 99 62 41                 .bA
        iny                                     ; 36E5 C8                       .
        bne     L36DC                           ; 36E6 D0 F4                    ..
L36E8:  lda     #$00                            ; 36E8 A9 00                    ..
        sta     curDiskName,y                         ; 36EA 99 62 41                 .bA
        rts                                     ; 36ED 60                       `

; ----------------------------------------------------------------------------
recover:
	jsr     GotoFirstMenu                   ; 36EE 20 BD C1                  ..
        bit     zp_DDb                        ; 36F1 24 DD                    $.
        bpl     @error                           ; 36F3 10 0C                    ..
        jsr     i_MoveData                      ; 36F5 20 B7 C1                  ..
		.word   otherFnBuffer
		.word	fnBuffer
		.word	17
        jmp     openDocument                           ; 36FE 4C CE 34                 L.4

@error:	lda     #<txt_cannot_recover
        ldy     #>txt_cannot_recover
        jmp     showError                       ; 3705 4C 52 24                 LR$

; ----------------------------------------------------------------------------
renameDocument:
	jsr     GotoFirstMenu
        lda     #0			; don't show DISK icon
        jsr     queryNewFilename	; ask for new name
        cmp     #CANCEL
        beq     @rts

	LoadW   r6, fnBuffer		; already exists?
        jsr     _FindFile
        cpx     #FILE_NOT_FOUND
        bne     @error			; yes

	LoadW   r6, otherFnBuffer	; rename
        jsr     loadR0FnBuffer
        jsr     swapUserZp
        jsr     RenameFile
        jsr     swapUserZp

        jsr     showFilenameInBox	; update name in UI
@end:	jsr     setupPageIndicatorSprite
@rts:	rts

@error:
	lda     #<txt_filename_exists
        ldy     #>txt_filename_exists
        jsr     showError
        bra     @end

queryNewFilename:
	sta     r0L
	LoadW   usericon_drive, 0
        lda     r0L
        beq     @1			; don't offer DISK icon
        jsr     getDiskIcon
        tax
        beq     @1			; single drive system, skip
	LoadW   usericon_drive, graph_drive
@1:	LoadB   dlgbox_create_new_disk, 0	; disable DISK icon
        lda     r0L
        beq     @2
        jsr     isDocOnAppDiskOrRamDisk
        bcc     @2
        lda     #DISK
        sta     dlgbox_create_new_disk
@2:	jsr     setDocDrive
        jsr     L36C4
	LoadW   r5, fnBuffer
	LoadB   fnBuffer, 0
        lda     #<dlgbox_create_new
        ldx     #>dlgbox_create_new
        jsr     doDlgBox
        lda     r0L
        cmp     #DRIVE
        bne     @3
        jsr     swapDrive
        bra     queryNewFilename
@3:	cmp     #DISK
        bne     @4
        lda     #<txt_insert_new_disk
        ldy     #>txt_insert_new_disk
        jsr     showError
        jsr     _OpenDisk
        bra     queryNewFilename
@4:	lda     fnBuffer
        bne     @5
        lda     #CANCEL
        rts

@5:	lda     r0L
        rts

; ----------------------------------------------------------------------------
dlgbox_create_new:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte   $10
	.byte   $10
	.word   txt_on_disk
	.byte   DBTXTSTR
	.byte   $3D
        .byte   $10
        .word   curDiskName
        .byte   DBTXTSTR
        .byte   $10
        .byte   $20
        .word   txt_enter_filename
        .byte   DBGETSTRING
        .byte   $10
        .byte   $30
        .byte   r5
        .byte   16 ; chars max
        .byte   DBUSRICON
        .byte   $09
        .byte   72
        .word   usericon_drive
        .byte   CANCEL
        .byte   $11
        .byte   72
dlgbox_create_new_disk:			; icon can be disabled
	.byte   DISK
	.byte   $01
	.byte   72
	.byte   NULL
; ----------------------------------------------------------------------------
L37DC:	MoveW   printerPageHeight, priv_header_page_height
	LoadW   fhdr_document, fnBuffer
	LoadW   r9, fhdr_document
	LoadB   r10L, 0
        jsr     swapUserZp                        ; 37FE 20 39 26                  9&
        jsr     SaveFile                        ; 3801 20 ED C1                  ..
        jsr     swapUserZp                        ; 3804 20 39 26                  9&
        bnex    @rts                           ; 3808 D0 44                    .D
	LoadW   r6, fnBuffer
        jsr     _FindFile                       ; 3812 20 F8 28                  .(
        bnex    @rts                           ; 3816 D0 36                    .6
        jsr     testFileVersion                 ; 3818 20 AB 3E                  .>
        tya                                     ; 381B 98                       .
        tax                                     ; 381C AA                       .
        bne     @rts                           ; 381D D0 2F                    ./
	MoveW   dirEntryBuf+OFF_GHDR_PTR, docFileHeaderTrkSec
        jsr     loadR0FnBuffer                    ; 382B 20 49 27                  I'
        jsr     swapUserZp                        ; 382E 20 39 26                  9&
        jsr     OpenRecordFile                  ; 3831 20 74 C2                  t.
        jsr     swapUserZp                        ; 3834 20 39 26                  9&
        bnex    @rts                           ; 3838 D0 14                    ..
        lda     #$80                            ; 383A A9 80                    ..
@loop:  pha                                     ; 383C 48                       H
        jsr     _AppendRecord                   ; 383D 20 EF 28                  .(
        pla                                     ; 3840 68                       h
        sec                                     ; 3841 38                       8
        sbc     #1                            ; 3842 E9 01                    ..
        bne     @loop                           ; 3844 D0 F6                    ..
        jsr     _UpdateRecordFile               ; 3846 20 F2 28                  .(
        lda     #0                            ; 3849 A9 00                    ..
        jmp     PointRecord                     ; 384B 4C 80 C2                 L..

@rts:  rts                                     ; 384E 60                       `

; ----------------------------------------------------------------------------
fhdr_document:
	.word   0
	.byte   $03,$15
	.byte   $BF
	.byte   $FF,$FF,$FF,$80,$00,$01,$8F,$FF
	.byte   $01,$88,$01,$01,$8B,$FF,$C1,$8A
	.byte   $00,$41,$8A,$FF,$F1,$8A,$80,$11
	.byte   $8A,$8E,$11,$8A,$80,$11,$8A,$BF
	.byte   $91,$8A,$80,$11,$8A,$9F,$11,$8A
	.byte   $80,$11,$8A,$BF,$91,$8E,$80,$11
	.byte   $82,$BF,$91,$83,$80,$11,$80,$80
	.byte   $11,$80,$FF,$F1,$FF,$FF,$FF

        .byte   $83
        .byte   APPL_DATA
        .byte   VLIR
        .word   0     ; start
        .word   $FFFF ; end
        .word   0     ; init

        .byte   "Write Image V2.0"

        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0
        .byte   0,0,0,0,0,0,0,0

        .byte   "geoWrite    V2.1",0,0,0,0

priv_header_data:
	.word	1 			; privFHData+privfhdata::startPageNo
	.byte	0			; privFHData+privfhdata::titleNlqFlag
	.word	0			; privFHData+privfhdata::headerHeight
	.word	0			; privFHData+privfhdata::footerHeight
priv_header_page_height:
	.word   DEFAULT_PAGE_HEIGHT

; ----------------------------------------------------------------------------
makeFullPageWide:
	jsr     tmpCloseDocFile
        jsr     i_MoveData
		.word   otherFnBuffer,fnBuffer,17
	LoadW   r6, fnBuffer
        jsr     _FindFile
        jsr     testFileVersion
        ldy     #'1'
        jsr     convertFile
        bnex    @1
	LoadW_y pageWidth1, PAGE_WIDTH_WIDE-1
@1:	jsr     reopenDocFile
        jsr     setupOptionsMenu
        jsr     readPageProperties
        jsr     setupPageIndicatorSprite
        rts

; ----------------------------------------------------------------------------
setupOptionsMenu:
	LoadB   menu_options_bottom, SUBMENU_Y + 1 + 14 * 8
	LoadB   menu_options_count, $80 | 8
        ldx     pageWidth1+1
        dex
        beq     @skip
					; hide "make full page width"
	LoadB   menu_options_bottom, SUBMENU_Y + 1 + 14 * 7
	LoadB   menu_options_count, $80 | 7
@skip:  rts

; ----------------------------------------------------------------------------
convertFile:
	sty     txt_converting_file_digit
        ldx     #0
        cmp     #'1'			; current version = 1?
        beq     :+
        dex
:	stx     fileConversionFlag	; indicates whether current version is 2
        lda     #<dlgbox_converting_file
        ldx     #>dlgbox_converting_file
        jsr     doDlgBox
        lda     sysDBData
        cmp     #CANCEL
        bne     :+
        jmp     @error2
:	lda     #'2'
        sta     fileHeader+$5A
        lda     txt_converting_file_digit
        sta     fileHeader+$5C
        bit     fileConversionFlag
        bmi     @1			; already version 2, don't overwrite metadata
        jsr     i_MoveData		; copy default geoWrite metadata into header
		.word   priv_header_data
		.word	fileHeader+O_GHP_FNAME+20
		.word	9
@1:	jsr     getPrivFHData
        MoveW   docFileHeaderTrkSec, r1
	LoadW   r4, fileHeader
        jsr     _PutBlock		; update header with new version
        bnex     @error
        jsr     loadR0FnBuffer
        jsr     swapUserZp
        jsr     OpenRecordFile
        jsr     swapUserZp
        bnex    @error
        lda     #0
        jsr     PointRecord		; start with record 0
        bnex    @error

@loop:  jsr     convertPage		; convert
        bnex    @error
        jsr     NextRecord
        tya
        bne     @loop			; until empty record

        lda     #PAGE_HEADER
        jsr     PointRecord
        tya
        beq     @2
        jsr     convertPage		; convert header
        bnex    @error
@2:	lda     #PAGE_FOOTER
        jsr     PointRecord
        tya
        beq     @3
        jsr     convertPage		; convert footer
        bnex    @error
@3:	jsr     _CloseRecordFile
        bnex    @error
        rts

@error:	lda     #<txt_converting_error
        ldy     #>txt_converting_error
        jsr     showIOError                     ; 39D1 20 78 24                  x$
        jsr     _CloseRecordFile                ; 39D4 20 E6 28                  .(
@error2:
	ldx     #$FF                            ; 39D7 A2 FF                    ..
        rts                                     ; 39D9 60                       `

; ----------------------------------------------------------------------------
dlgbox_converting_file:
	.byte   DEF_DB_POS|1
	.byte   DBTXTSTR
	.byte	16
	.byte	16
	.word	txt_converting_file
	.byte	OK
	.byte	1
        .byte   72
        .byte	CANCEL
        .byte	17
        .byte	72
        .byte	NULL
; ----------------------------------------------------------------------------
convertPage:
	LoadW   r7, MEM_PAGE                            ; 39EB A9 10                    ..
	LoadW   r2, MEM_SIZE_PAGE                            ; 39F3 A9 58                    .X
        bit     fileConversionFlag                           ; 39F7 2C 73 41                 ,sA
        bmi     L3A0C                           ; 39FA 30 10                    0.
	LoadW   r7, MEM_PAGE+7
	LoadW   r2, MEM_SIZE_PAGE-7
L3A0C:  jsr     swapUserZp                        ; 3A0C 20 39 26                  9&
        jsr     ReadRecord                      ; 3A0F 20 8C C2                  ..
        jsr     swapUserZp                        ; 3A12 20 39 26                  9&
        bnex    L3A48                           ; 3A16 D0 30                    .0
        bit     fileConversionFlag                           ; 3A18 2C 73 41                 ,sA
        bmi     L3A20                           ; 3A1B 30 03                    0.
        jsr     L3A49                           ; 3A1D 20 49 3A                  I:
L3A20:  lda     txt_converting_file_digit                           ; 3A20 AD 28 40                 .(@
        cmp     #'0'                            ; 3A23 C9 30                    .0
        beq     L3A2A                           ; 3A25 F0 03                    ..
        jsr     L3A78                           ; 3A27 20 78 3A                  x:
L3A2A:  sec                                     ; 3A2A 38                       8
        lda     r7L                             ; 3A2B A5 10                    ..
        sbc     #<MEM_PAGE                            ; 3A2D E9 10                    ..
        sta     r2L                             ; 3A2F 85 06                    ..
        lda     r7H                             ; 3A31 A5 11                    ..
        sbc     #>MEM_PAGE                            ; 3A33 E9 43                    .C
        sta     r2H                             ; 3A35 85 07                    ..
	LoadW   r7, MEM_PAGE
        jsr     swapUserZp                        ; 3A3F 20 39 26                  9&
        jsr     WriteRecord                     ; 3A42 20 8F C2                  ..
        jsr     swapUserZp                        ; 3A45 20 39 26                  9&
L3A48:  rts                                     ; 3A48 60                       `

; ----------------------------------------------------------------------------
L3A49:  ldy     #0                            ; 3A49 A0 00                    ..
L3A4B:  lda     MEM_PAGE+7,y                         ; 3A4B B9 17 43                 ..C
        sta     PAGE_RULER+ruler::left_margin,y                         ; 3A4E 99 11 43                 ..C
        iny                                     ; 3A51 C8                       .
        cpy     #20                            ; 3A52 C0 14                    ..
        bne     L3A4B                           ; 3A54 D0 F5                    ..
        LoadB   MEM_PAGE+page::magic, ESC_RULER
        MoveW   PAGE_RULER+ruler::left_margin, PAGE_RULER+ruler::paragraph_margin                           ; 3A64 8D 25 43                 .%C
        LoadB   PAGE_RULER+ruler::justification, 0 ; left, single line
        LoadB   PAGE_RULER+ruler::unused1, 0
        sta     PAGE_RULER+ruler::unused2                           ; 3A71 8D 29 43                 .)C
        sta     PAGE_RULER+ruler::unused3                           ; 3A74 8D 2A 43                 .*C
        rts                                     ; 3A77 60                       `

; ----------------------------------------------------------------------------
L3A78:  lda     r7H                             ; 3A78 A5 11                    ..
        sta     pageEndPtr2+1                        ; 3A7A 85 D1                    ..
        lda     r7L                             ; 3A7C A5 10                    ..
        sta     pageEndPtr2                        ; 3A7E 85 D0                    ..
        jsr     LoadR15_MEM_PAGE                           ; 3A80 20 BF 26                  .&
L3A83:  jsr     cmpPageEndPtr2R15                           ; 3A83 20 14 28                  .(
        bcc     L3AAF                           ; 3A86 90 27                    .'
        beq     L3AAF                           ; 3A88 F0 25                    .%
        ldy     #$00                            ; 3A8A A0 00                    ..
        lda     (r15),y                        ; 3A8C B1 20                    . 
        cmp     #$11                            ; 3A8E C9 11                    ..
        beq     L3AA6                           ; 3A90 F0 14                    ..
        cmp     #$10                            ; 3A92 C9 10                    ..
        bne     L3A99                           ; 3A94 D0 03                    ..
        jsr     incWR15                           ; 3A96 20 1E 27                  .'
L3A99:  cmp     #$17                            ; 3A99 C9 17                    ..
        bne     L3AA0                           ; 3A9B D0 03                    ..
        jsr     addWI3R15                           ; 3A9D 20 18 27                  .'
L3AA0:  jsr     incWR15                           ; 3AA0 20 1E 27                  .'
        bra     L3A83                           ; 3AA4 50 DD                    P.
L3AA6:  jsr     L3AB0                           ; 3AA6 20 B0 3A                  .:
        jsr     addRulerSizeToR15                           ; 3AA9 20 34 27                  4'
        bra     L3A83                           ; 3AAD 50 D4                    P.
L3AAF:  rts                                     ; 3AAF 60                       `

; ----------------------------------------------------------------------------
L3AB0:  ldy     #$15                            ; 3AB0 A0 15                    ..
L3AB2:  lda     (r15),y                        ; 3AB2 B1 20                    . 
        sta     r1L                             ; 3AB4 85 04                    ..
        iny                                     ; 3AB6 C8                       .
        lda     (r15),y                        ; 3AB7 B1 20                    . 
        sta     r1H                             ; 3AB9 85 05                    ..
        lda     r1H                             ; 3ABB A5 05                    ..
        cmp     #$01                            ; 3ABD C9 01                    ..
        bne     L3AC5                           ; 3ABF D0 04                    ..
        lda     r1L                             ; 3AC1 A5 04                    ..
        cmp     #$DF                            ; 3AC3 C9 DF                    ..
L3AC5:  bne     L3ACF                           ; 3AC5 D0 08                    ..
        lda     #$01                            ; 3AC7 A9 01                    ..
        sta     r1H                             ; 3AC9 85 05                    ..
        lda     #$E0                            ; 3ACB A9 E0                    ..
        sta     r1L                             ; 3ACD 85 04                    ..
L3ACF:  clc                                     ; 3ACF 18                       .
        lda     #$50                            ; 3AD0 A9 50                    .P
        adc     r1L                             ; 3AD2 65 04                    e.
        sta     r1L                             ; 3AD4 85 04                    ..
        bcc     L3ADA                           ; 3AD6 90 02                    ..
        inc     r1H                             ; 3AD8 E6 05                    ..
L3ADA:  lda     r1H                             ; 3ADA A5 05                    ..
        sta     (r15),y                        ; 3ADC 91 20                    . 
        dey                                     ; 3ADE 88                       .
        lda     r1L                             ; 3ADF A5 04                    ..
        sta     (r15),y                        ; 3AE1 91 20                    . 
        dey                                     ; 3AE3 88                       .
        dey                                     ; 3AE4 88                       .
        bpl     L3AB2                           ; 3AE5 10 CB                    ..
        rts                                     ; 3AE7 60                       `

; ----------------------------------------------------------------------------
showFilenameInBox:
	jsr     i_MoveData
		.word   fnBuffer
		.word	otherFnBuffer
		.word	17
        jsr     useSystemFont
        LoadB   currentMode, 0
        jsr     drawFilenameBox
	LoadW   r0, otherFnBuffer
        jsr     measureFilenameWidth
	CmpWI   r1, FNBOX_WIDTH - 2*FNBOX_INNER_INSET
	bcc     @1
	LoadW   r1, FNBOX_WIDTH - 2*FNBOX_INNER_INSET
@1:	lda     #<(FNBOX_WIDTH - 2*FNBOX_INNER_INSET)
        sub     r1L
        sta     r11L
        lda     #>(FNBOX_WIDTH - 2*FNBOX_INNER_INSET)
        sbc     r1H
        sta     r11H
        lsr     r11L
        LoadB   r11H, 0
	AddVW   SC_PIX_WIDTH - FNBOX_WIDTH - FNBOX_RIGHT_INSET + 3, r11
	lda     #10
        sta     r1H
	LoadW   rightMargin, SC_PIX_WIDTH - FNBOX_RIGHT_INSET
        lda     #' '
        jsr     PutChar
	LoadW   r0, otherFnBuffer
        jsr     PutString
        lda     #' '
        jsr     PutChar
	LoadW   rightMargin, SC_PIX_WIDTH-1
        rts

; ----------------------------------------------------------------------------
measureFilenameWidth:
	PushB   r2L
        lda     #0
        sta     r1L
        sta     r1H
        sta     r2L
        sta     r13L
@1:	ldy     r13L
        lda     (r0),y
        beq     @3
        inc     r2L
        jsr     GetCharWidth
        add     r1L
        sta     r1L
        bcc     @2
        inc     r1H
@2:	inc     r13L
        bne     @1
@3:	ldx     r2L
        PopB    r2L
        rts

; ----------------------------------------------------------------------------
drawFilenameBox:
	jsr     i_GraphicsString
		.byte   MOVEPENTO
		.word   SC_PIX_WIDTH-FNBOX_WIDTH-FNBOX_RIGHT_INSET
		.byte   FNBOX_TOP_INSET
		.byte   NEWPATTERN
		.byte   9 ; stripes
		.byte   RECTANGLETO
		.word   SC_PIX_WIDTH-5
		.byte   FNBOX_TOP_INSET+FNBOX_HEIGHT
		.byte   NEWPATTERN
		.byte   0
		.byte   NULL
        rts

; ----------------------------------------------------------------------------
showErrorDeskAcc:
	pla
        lda     #<txt_cannot_run_accessory
        ldy     #>txt_cannot_run_accessory
        jmp     showError

;---------------------------------------------------------------
; loadDeskAcc
;
; Function:  Load and execute a desk accessory, saving and
;            restoring all necessary state.
;
; Pass:      a  number of desk accessory, 1-based
;
; Note:      The top 36 lines of the screen will be saved and
;            restored by the app; the remainder of the screen
;            will be redrawn.
;---------------------------------------------------------------
loadDeskAcc:
	pha				; accessory index, 1-based
        jsr     GotoFirstMenu

        lda     curPage			; can't run it while editing header or footer
        cmp     #PAGE_HEADER
        bcs     showErrorDeskAcc

        jsr     clearSelection
        jsr     tmpCloseDocFile
        jsr     saveTextscrapIfNeeded

        jsr     i_MoveData		; save sprites 2 through 5
		.word   spr2pic
		.word	L7F00
		.word	$0100

        ldx     #<(scrrecvtab_deskacc-scrrecvtabs)
        jsr     screenSave		; save menu and ruler

        pla
        sta     r6L
        asl     a			; * 17
        asl     a
        asl     a
        asl     a
        clc
        adc     r6L
        adc     #<(deskAccNames-17)
        sta     r6L
        lda     #0
        adc     #>(deskAccNames-17)
        sta     r6H

        MoveB   CPU_DATA, r15L		; save and clear sprite double height register
        LoadB   CPU_DATA, IO_IN
        PushB   $D017
        LoadB   $D017, 0
        MoveB   r15L, CPU_DATA

	PushW   L4C95			; ???
        lda     #0
        sta     r0L			; flag: not necessary to save/restore bitmap
        sta     r10L			; API requirement to be 0
        jsr     setAppDrive
        jsr     swapUserZp
        jsr     GetFile			; run desk accessory
        jsr     swapUserZp
	PopW    L4C95			; ???

        MoveB   CPU_DATA, r15L		; restore sprite double height register
        LoadB   CPU_DATA, IO_IN
        PopB    $D017
        MoveB   r15L, CPU_DATA

        txa				; save error code
        pha

        ldx     #<(scrrecvtab_deskacc-scrrecvtabs)
        jsr     screenRecover		; restore menu and ruler

        jsr     i_MoveData		; restore sprites 2 through 5
		.word   L7F00
		.word	spr2pic
		.word	$0100

        pla
        tax
        beq     @ok

        cpx     #INSUFF_SPACE
        bne     @1
        lda     #<txt_accessory_disk_full
        ldy     #>txt_accessory_disk_full
        jsr     showError
        bra     @ok

@1:	lda     #<txt_running_accessory
        ldy     #>txt_running_accessory
        jsr     showIOError

@ok:	jsr     initTextPrompt
        jsr     scanTextScrap
        jsr     reopenDocFile

        MoveB   screencolors, r2L	; restore color RAM
	LoadW	r1, COLOR_MATRIX
	LoadW	r0, 1000
        jsr     FillRam

        jsr     pushRulerData
        jsr     L15D0
        jsr     popRulerData
        jsr     setupPageIndicatorSprite
        jsr     setPromptFontMetricsUpdateRulerUI
        rts

; ----------------------------------------------------------------------------
saveTextscrapIfNeeded:
	bit     textScrapOnDisk
        bmi     @rts

        lda     textScrapSize
        ora     textScrapSize+1
        beq     @rts

	LoadW   textscrap_fhdr+O_GHST_ADDR, textScrapSize
        jsr     saveTextscrap

@rts:	rts

;---------------------------------------------------------------
; exitToDesktop
;
; Function:  Clean up and exit the application. Writes the text
;            scrap in memory out to disk if necessary.
;---------------------------------------------------------------
exitToDesktop:
	jsr     saveTextscrapIfNeeded
        jsr     setAppDrive
        jsr     swapUserZp		; swap back to KERNAL zp
        jmp     EnterDeskTop

; ----------------------------------------------------------------------------
; sets up the sprite used to indicate the currenly visible region on the page
setupPageIndicatorSprite:
	START_IO
        LoadB   $D02D, 0		; sprite 6 color
	END_IO
        jsr     i_FillRam
		.word   64,spr6pic
		.byte   0
        jsr     i_MoveData
		.word   spr_wide_box
		.word	spr6pic
		.word	11
        ldx     pageWidth1+1
        dex
        beq     @1			; narrow page -> wide box
        jsr     i_MoveData
		.word   spr_narrow_box
		.word	spr6pic
		.word	11
@1:	rts

; ----------------------------------------------------------------------------
spr_wide_box:
	.byte   %11111111,%11100000,%00000000
	.byte   %10000000,%00100000,%00000000
	.byte   %10000000,%00100000,%00000000
	.byte   %11111111,%11100000

spr_narrow_box:
	.byte   %11111111,%00000000,%00000000
	.byte   %10000001,%00000000,%00000000
	.byte   %10000001,%00000000,%00000000
	.byte   %11111111,%00000000

; ----------------------------------------------------------------------------
; [XXX where is this ever written?]
readPageProperties:
	LoadB   hasPagePropMetadata, 0
        lda     #PAGE_RESERVED
        jsr     PointRecord
        tya
        beq     @rts			; empty
        bnex    @rts			; error

					; read pageProperties and widthForPage
	LoadW   r7, heightForPage_lo
	LoadW   r2, MAX_NUM_PAGES*4	; length: 4 bytes per page ($F4 bytes)
        jsr     swapUserZp
        jsr     ReadRecord
        jsr     swapUserZp
        cpx     #BFR_OVERFLOW
        beq     :+			; acceptable error
        bnex    @rts
:	LoadB   hasPagePropMetadata, $FF

					; constrain all entries to page width
        ldy     #MAX_NUM_PAGES-1
@loop:  lda     widthForPage_hi,y
        cmp     pageWidth1+1
        bne     @1
        lda     widthForPage_lo,y
        cmp     pageWidth1
@1:	bcc     @2
        lda     pageWidth1
        sta     widthForPage_lo,y
        lda     pageWidth1+1
        sta     widthForPage_hi,y
@2:	dey
        bpl     @loop
@rts:	rts

; ----------------------------------------------------------------------------
showAboutDialog:
	jsr     GotoFirstMenu
        lda     #<dlgbox_about
        ldx     #>dlgbox_about
        jmp     doDlgBox

; ----------------------------------------------------------------------------
dlgbox_about:
	.byte   DEF_DB_POS|1
	.byte   DBSYSOPV
	.byte   DBTXTSTR
	.byte   16
	.byte   16
        .word   aboutText
        .byte   0
; ----------------------------------------------------------------------------

.include "scantextscrap.inc"
.include "savetextscrap.inc"
.include "fileversion.inc"

; ----------------------------------------------------------------------------

.include "strings5.inc"

;---------------------------------------------------------------

.include "textscrap_filename.inc"

;---------------------------------------------------------------
; Bug in the German localization:
; The following strings are not localized in this copy, but they
; are in the other copy in reocrd 1.

.include "en/scantextscrap_strings.inc"

;---------------------------------------------------------------

curDiskName		= *
fileConversionFlag	= * + 17
