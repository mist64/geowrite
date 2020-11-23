; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  01 - init, copy protection
;
; This record is loaded and run once, immediately after startup. It passes
; execution back to record 0, which never loads this record again.
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

;---------------------------------------------------------------

	; needed by checksum.py
        .global protExecTrack
        .global protExecSector
        .global protSerialTrack
        .global protSerialSector
        .global serial

;---------------------------------------------------------------

.segment        "CODE1": absolute

CODE1:
        lda     #>(@1-1)		; an obfuscated way to say
        pha				; "jsr initApp"
        lda     #<(@1-1)
        pha
	LoadW   r15, initApp
        jmp     (r15L)

@1:	jsr     getprinterPageHeight
        jsr     decryptR0

        lda     loadOpt
        and     #ST_PR_DATA
	bne     appPrint

        jsr     testDiskNotFull
        jsr     initTextPrompt
        jsr     initFonts
        jsr     drawMenuAndFilenameBox
					; fill graphics data for ruler
					; which is only drawn once
	LoadW   doicons_graphdata_ptr1, graph_ruler
	LoadW   doicons_graphdata_ptr2, graph_rulerbuttons
	LoadW   doicons_graphdata_ptr3, graph_pageindicatorbox
	LoadW   r0, doicons_data
        jsr     DoIcons

        LoadB   iconSelFlg, 0		; disable icon flashing

        jsr     findDeskAccs
        jsr     scanTextScrap
        LoadB   unusedFlag, $FF
	LoadW   r0, processTable
        lda     #<NUM_PROCESSES
        jsr     InitProcesses
        ldx     #PROCESS_SIDE_FLIPPING
        jsr     RestartProcess
        jmp     openOrRestart

appPrint:
	jsr     initFonts
        jsr     openForPrinting
        LoadB   tmpCurRecord, 0		; [XXX doesn't need init]
        jmp     appPrint2

;---------------------------------------------------------------
; drawMenuAndFilenameBox
;
; Function:  Draw the menu as well as the filename box at the
;            (empty) top right of the screen.
;---------------------------------------------------------------
drawMenuAndFilenameBox:
	jsr     i_GraphicsString
		.byte   NEWPATTERN
		.byte   0
		.byte   MOVEPENTO
		.word   0
		.byte   0
		.byte   RECTANGLETO
		.word   319
		.byte   35
		.byte   NULL
	LoadW   r0, menu_main
        lda     #0
        jsr     DoMenu
        jsr     i_GraphicsString
		.byte   NEWPATTERN
		.byte   2
		.byte   MOVEPENTO
.if LANG=LANG_DE
		.word   184
.else
		.word   192
.endif
		.byte   0
		.byte   RECTANGLETO	; 50% stipple, right of menu
		.word   319
		.byte   14
		.byte   MOVEPENTO
		.word   0
		.byte   36
		.byte   RECTANGLETO	; 50% stipple, work area
		.word   319
		.byte   199
		.byte   NEWPATTERN
		.byte   9
		.byte   MOVEPENTO
		.word   216
		.byte   3
		.byte   RECTANGLETO	; horizontal stripes, filename box top
		.word   314
		.byte   4
		.byte   MOVEPENTO
		.word   213
		.byte   4
		.byte   RECTANGLETO	; horizontal stripes, filename box remainder
		.word   317
		.byte   14
		.byte   MOVEPENTO
		.word   318
		.byte   14
		.byte   LINETO		; filename box outline
		.word   318
		.byte   5
		.byte   LINETO
		.word   314
		.byte   2
		.byte   LINETO
		.word   216
		.byte   2
		.byte   LINETO
		.word   212
		.byte   5
		.byte   LINETO
		.word   212
		.byte   14
		.byte   LINETO
		.word   318
		.byte   14
		.byte   NEWPATTERN
		.byte   1
		.byte   MOVEPENTO
		.word   0
		.byte   14
		.byte   RECTANGLETO	; separator line below menu
		.word   319		; [xxx LINETO]
		.byte   14
		.byte   NULL
        rts

;---------------------------------------------------------------
; findDeskAccs
;
; Function:  Enumerate desk accessories on disk and add them to
;            the "geos" menu.
;---------------------------------------------------------------
findDeskAccs:
	LoadW   r6, deskAccNames
	LoadB   r7L, DESK_ACC
	LoadB   r7H, 8			; num
	LoadW__ r10, 0			; no fn matching
        jsr     swapUserZp
        jsr     FindFTypes
        jsr     swapUserZp
        lda     #8
        sub     r7H			; num found
        beq     @rts
        sta     r2H
        add     #1
        pha
        ora     #VERTICAL | UN_CONSTRAINED
        sta     menu_geos_count
        pla
        sta     r0L
        lda     #14
        sta     r1L
        ldy     #r0
        ldx     #r1
        jsr     BBMult			; (num + 1) * 14
        add     #16
        sta     menu_geos_bottom
@rts:	rts

;---------------------------------------------------------------
; initFonts
;
; Function:  Enumerate fonts and init metrics cache.
;---------------------------------------------------------------
initFonts:
	jsr     UseSystemFont
        LoadB   metricsFifoCounter, 0
        ldx     #0
@loop:  sta     metricsIds,x
        sta     metricsIds+8,x
        inx
        cpx     #8
        bne     @loop
        jsr     enumerateFonts
        jmp     setSystemFont

;---------------------------------------------------------------
; getprinterPageHeight
;
; Function:  Ask the current printer driver for its page height,
;            store it in printerPageHeight (in dots).
;---------------------------------------------------------------
getprinterPageHeight:
	jsr     setAppDrive		; [XXX redundant]
	LoadW   printerPageHeight, DEFAULT_PAGE_HEIGHT
        LoadB   a9L, 0
        jsr     loadPrinter
        bnex    @rts
        jsr     GetDimensions
        sty     printerPageHeight
        lda     #0
        sta     printerPageHeight+1
        asl     printerPageHeight	; * 8
        rol     printerPageHeight+1
        asl     printerPageHeight
        rol     printerPageHeight+1
        asl     printerPageHeight
        rol     printerPageHeight+1
@rts:	rts

;---------------------------------------------------------------

graph_ruler:				; 320x8
	.byte   40,%00000000
	.byte	$DC+6,8
		.byte	7,%00000000
		.byte	1,%00000001
		.byte	2,%00000000
	.byte	$DC+6,16
		.byte	2,%00000000
		.byte	1,%00000001
		.byte	2,%00000000
        .byte	80,%00000001
        .byte	40,%11111110

graph_pageindicatorbox:			; 16x16
	.byte   2,%11111111
	.byte	$DC+3,14
		.byte	$80+2,%10000000,%00000001
	.byte	2,%11111111

;---------------------------------------------------------------

; graph_rulerbuttons
	.include "graph1.s"

;---------------------------------------------------------------

.include "scantextscrap_strings.inc"

;---------------------------------------------------------------
; enumerateFonts
;
; Function:  Enumerate fonts on disk, read in their point sizes,
;            the track/sector pointers of the point size records,
;            and their file sizes, and fill the fonts menu.
;---------------------------------------------------------------
enumerateFonts:
	jsr     setAppDrive
        LoadB   setStyleCheckmarkFlag, $FF
	LoadW   curFont, SYSTEM_FONT_ID
        LoadB   setFontMarkFlag, $FF
        jsr     i_FillRam
		.word	$0160-$10 	; count
		.word   diskFontIds+$10 ; address
		.byte   0
        jsr     i_MoveData
		.word   txt_bsw
		.word   fontNames
		.word   txt_bsw_size
	LoadW   r6, fontNames + FONT_NAME_SIZE
        LoadB   r7L, FONT		; file type
        LoadB   r7H, MAX_FONT_FILES - 1	; system font is #0
	LoadW__ r10, 0 			; no name filter
        jsr     swapUserZp
        jsr     FindFTypes		; get font files
        jsr     swapUserZp
        jsr     convertFontNamesToMenuItems
        lda     #8
        sub     r7H 			; num found
        sta     numFontFiles
        cmp     #1
        beq     @just1
        lda     #1
@loop:  pha
        jsr     fontNameFromIndex
        pla
        pha
        jsr     extractFontMetadata
        pla
        add     #1
        cmp     numFontFiles
        bne     @loop
@just1: lda     numFontFiles
        ora     #VERTICAL | UN_CONSTRAINED
        sta     menu_fontnames_items
        MoveB   numFontFiles, r1L
        LoadB   r2L, 14
        ldy     #r2
        ldx     #r1
        jsr     BBMult
        lda     r1L
        add     #16
        sta     menu_fontnames_bottom ; num * 14 + 16
        lda     #0
        sta     loadedFontsCount
        sta     fontLruCounter
        sta     fontLruCounter+1
        sta     currentMode
	LoadW   activeFont, SYSTEM_FONT_ID
        jmp     UseSystemFont

txt_bsw:
	.byte "  " 			; first char for selected indicator
	.byte "BSW",0
txt_bsw_size = * - txt_bsw

;---------------------------------------------------------------
; extractFontMetadata
;
; Function:  Read point sizes, track/sector pointers and data
;            sizes for a font file from its file header
;
; Pass:      a   font index (0-7)
;            r0  font filename
;---------------------------------------------------------------
extractFontMetadata:
	pha
        MoveW   r0, r6
        jsr     _FindFile		; get file
	LoadW   r9, dirEntryBuf
        jsr     swapUserZp
        jsr     GetFHdrInfo		; read file header
        jsr     swapUserZp
	MoveW   dirEntryBuf+OFF_DE_TR_SC, r1
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock       	; read index block
        pla                     	; font index (0-7)
        asl     a
        asl     a
        asl     a
        asl     a 			; * 16
        tay
        ldx     #0
@loop:  jsr     extractFontIdTrkSec
        jsr     extractDiskFontRecordSize
        iny
        iny
        inx
        inx
        cpx     #FONTS_PER_FONTFILE
        bne     @loop
        rts

;---------------------------------------------------------------
; extractFontIdTrkSec
;
; Function:  Copy a point size ID and its track/sector pointer
;            from the font file header and the index block
;            into the app's data structures.
;
; Pass:      x   font index within font file (0-15)
;            y   fontfile * 16 + fontindex * 2
;---------------------------------------------------------------
extractFontIdTrkSec:
	lda     fileHeader+OFF_GHPOINT_SIZES,x
        sta     diskFontIds,y
        and     #FONT_SIZE_MASK
        sta     r6L           		; point size
        lda     fileHeader+OFF_GHPOINT_SIZES+1,x
        sta     diskFontIds+1,y
        ora     diskFontIds,y
        beq     @rts         		; skip empty records
        txa
        pha
        lda     r6L           		; point size
        asl     a
        tax
        lda     diskBlkBuf+2,x 		; track
        sta     diskFontRecordTrkSec,y
        lda     diskBlkBuf+3,x 		; sector
        sta     diskFontRecordTrkSec+1,y
        pla
        tax
@rts:   rts

;---------------------------------------------------------------
; extractDiskFontRecordSize
;
; Function:  Copy a font's data size from the font file header
;            into the app's data structures.
;
; Pass:      x   font index within file (0-15)
;            y   fontfile * 16 + fontindex * 2
;---------------------------------------------------------------
extractDiskFontRecordSize:
	lda     fileHeader+OFF_GHSET_LENGTHS,x
        sta     diskFontRecordSize,y
        sta     r2L
        lda     fileHeader+OFF_GHSET_LENGTHS+1,x
        sta     diskFontRecordSize+1,y
        sta     r2H

	CmpWI   r2, MEM_SIZE_FONTS	; data size too big?
	bcc     @rts
        beq     @rts
        lda     #0
        sta     diskFontRecordTrkSec,y	; then pretend it doesn't exist

@rts:   rts

;---------------------------------------------------------------
; convertFontNamesToMenuItems
;
; Function:  Convert an array of 17 char font filenames (as
;            returned by FindFTypes) to an array of 19 char
;            strings, with two spaces prepended.
;
; Note:      This is needed to show the font names in a menu
;            and replace the first space with a '*' character
;            if the menu item is selected
;---------------------------------------------------------------
convertFontNamesToMenuItems:
	LoadW   r1, fontNames + 7 * 17 +2
	LoadW   r2, fontNames + 7 * 19
	LoadB   r3L, 7
@loop1: ldy     #17 - 1
@loop2: lda     (r1),y
        iny
        iny
        sta     (r2),y			; store offset by 2
        dey
        dey
        dey
        bpl     @loop2			; copy full filename loop
        iny
        lda     #' '
        sta     (r2),y    		; prepend two spaces
        iny
        sta     (r2),y
	SubVW   17, r1
	SubVW   FONT_NAME_SIZE, r2
        dec     r3L
        bne     @loop1
        rts

;---------------------------------------------------------------

.include "loadprinter_strings.inc"

;---------------------------------------------------------------

	.include "strings_init.inc"

;---------------------------------------------------------------

.include "textscrap_filename.inc"
.include "fileversion.inc"

;---------------------------------------------------------------
; decryptR0
;
; Function:  Decrypt the decryptR0 function in record 0 using
;            the checksum of record 1 (this code).
;---------------------------------------------------------------
decryptR0:
	LoadW   r0, MEM_OVERLAY		; checksum code record #1
	LoadW   r1, CODE1_END-CODE1
	LoadB   r2L, 0
        ldy     #0
@loop1: lda     (r0),y
        add     r2L
        sta     r2L
	IncW    r0
	ldx     #r1
        jsr     Ddec
        bne     @loop1

        lda     r2L			; remove variable bytes from checksum
        sub     serial
        sub     serial+1
        sub     protExecTrack
        sub     protExecSector
        sub     protSerialTrack
        sub     protSerialSector
        add     #$DE
        add     #$DE
        add     #$DE
        add     #$DE
        sta     r2L			; geoWrite 2.1 en: $4F

	LoadW   r0, r0_encrypted_start
	LoadW   r1, r0_encrypted_start-r0_encrypted_end
        ldy     #0
@loop2:	lda     (r0),y
        eor     r2L			; decrypt
        sta     (r0),y
	IncW    r0
	IncW    r1
	bne     @loop2
        rts

;---------------------------------------------------------------
; checkSerialOrInstall
;
; Function:  Perform the copy protection check. If the app is
;            not installed, execute the 1541/1571 code to
;            verify the authenticity of the disk and install
;            the app. If the app is installed, make sure the
;            serial number matches.
;---------------------------------------------------------------
checkSerialOrInstall:
	lda     serial
        ora     serial+1		; does app have a serial?
        beq     @install		; no, then install

        lda     #<GetSerialNumber
        ldx     #>GetSerialNumber
        jsr     CallRoutine
	CmpW    serial, r0		; does the app serial match the system's?
	beq     @rts			; yes, return

        lda     #<txt_serial_mismatch
        ldy     #>txt_serial_mismatch
        jsr     showError		; no, tell the user
        jsr     swapUserZp
        jmp     EnterDeskTop		; and exit

@rts:	rts

@install:
protExecTrack = * + 1
	lda     #PROT_EXEC_TRACK	; protection track (stamped in by build system)
        sta     r1L
protExecSector = * + 1
	lda     #PROT_EXEC_SECTOR	; protection sector (stamped in by build system)
        sta     r1H
        jsr     executeDiskBlock
        beqx    @ok 			; no error

        lda     #<txt_copy_protection	; installing a non-original disk?
        ldy     #>txt_copy_protection	; then show a non-informative message
        bra     showErrorAndExit	; and exit to deskTop

@ok:	lda     #<GetSerialNumber
        ldx     #>GetSerialNumber
        jsr     CallRoutine		; get OS serial number to put into app
	MoveW   r0, serial
	LoadW   r4, diskBlkBuf
protSerialTrack = * + 1
	lda     #PROT_SERIAL_TRACK	; serial track (stamped in by build system)
        sta     r1L
protSerialSector = * + 1
	lda     #PROT_SERIAL_SECTOR	; serial sector (stamped in by build system)
        sta     r1H
        jsr     _GetBlock		; read sector that contains code with serial
        bnex    @ierror

@offset = (serial-CODE1) .mod 254 + 2
        lda     serial			; get serial low
        eor     #$DE			; "encrypt"
        sta     diskBlkBuf+@offset
        lda     serial+1		; get serial high
        eor     #$DE			; "encrypt"
        sta     diskBlkBuf+@offset+1

	LoadW   r4, diskBlkBuf
        jsr     _PutBlock		; write back block
        beqx    installOk

        cpx     #WR_PR_ON
        beq     @wperr
@ierror:
	lda     #<txt_error_installing
        ldy     #>txt_error_installing
        bra     showErrorAndExit

@wperr:	lda     #<txt_install_write_protected
        ldy     #>txt_install_write_protected

showErrorAndExit:
	jsr     showError
        jsr     swapUserZp
        jmp     EnterDeskTop

serial:
	.word   SERIAL

installOk:
	asl     serial			; cycle serial left to obfuscate
        rol     serial+1		; serial = serial[14..0,15]
        lda     serial
        adc     #0
        sta     serial

        jsr     swapUserZp
        jsr     GetDirHead		; read BAM block
        jsr     swapUserZp		; [XXX is this fails but writing succeeds, BAM is trashed!]

	MoveW   serial, curDirHead+$BE	; store serial after "GEOS format V1.x"

        jsr     swapUserZp
        jsr     PutDirHead		; write BAM block
        jsr     swapUserZp

        lda     #<txt_installed
        ldy     #>txt_installed		; show success
        jsr     showError
        jsr     swapUserZp
        jmp     EnterDeskTop		; and exit

;---------------------------------------------------------------
; executeDiskBlock
;
; Function:  Load a block from disk, verify the checksum and
;            execute it.
;
; Pass:      r1  track and sector of block
;---------------------------------------------------------------
executeDiskBlock:
	PushW   r1
        jsr     swapUserZp
        jsr     NewDisk
        jsr     swapUserZp
	PopW    r1
        bnex    @rts			; I/O error -> fail

	LoadW   r4, diskBlkBuf		; read block
        jsr     _GetBlock
        bnex    @rts			; I/O error -> fail

        lda     #0
        ldy     #2
@loop:  clc
        adc     diskBlkBuf,y		; checksum bytes $02-$FE
        iny
        cpy     #$FF
        bne     @loop
        cmp     diskBlkBuf+$FF 		; checksum at offset $FF
        beq     @ok

        ldx     #$FF			; fail
@rts:	rts

@ok:	jsr     swapUserZp
        jsr     diskBlkBuf+2 		; execute block
        jsr     swapUserZp
        rts

;---------------------------------------------------------------

.include "loadprinter.inc"

;---------------------------------------------------------------
; openOrRestart
;
; Function:  Open file if document name argument was passed
;            to the app, otherwise show the main dialog.
;
; Pass:      r1  track and sector of block
;---------------------------------------------------------------
openOrRestart:
	lda     loadOpt
        and     #ST_LD_DATA
        beq     @restart

	MoveW   argFilename, r0		; filename was passed
        ldy     #0			; -> open file
@loop:  lda     (r0),y
        sta     fnBuffer,y
        iny
        cmp     #0
        bne     @loop
        jmp     openDocumentRepaginateEdit

@restart:
	jmp     restart

;---------------------------------------------------------------
; openForPrinting
;
; Function:  Open document whose file was passed as an argument
;            to the app and print it.
;---------------------------------------------------------------
openForPrinting:
	MoveW   argFilename, r0

        ldy     #0
@loop:  lda     (r0),y
        sta     fnBuffer,y
        sta     otherFnBuffer,y
        iny
        cmp     #0
        bne     @loop

        jsr     setDocDrive
	LoadW   r6, fnBuffer
        jsr     _FindFile
        bnex    @exit
	LoadW   pageWidth1, PAGE_WIDTH_WIDE-1
        jsr     testFileVersion
        beq     @1
        cpy     #0
        bne     @exit			; error
	LoadW_y pageWidth1, PAGE_WIDTH_NARROW-1
        cmp     #'2'
        bne     @error
        cpx     #'0'
        beq     @2
@error:	lda     #<txt_needs_conversion
        ldy     #>txt_needs_conversion
        jsr     showError
@exit:  jmp     _exitToDesktop

@1:	tya
        bne     @exit
@2:	jsr     i_MoveData
		.word   fileHeader + O_GHP_PRIVATE
		.word   privFHData
		.word   9
        bit     privFHData+privfhdata::titleNlqFlag
        bvc     @3
	CmpWI   privFHData+privfhdata::pageHeight, DEFAULT_PAGE_HEIGHT
	bne     @error
        bra     @4

@3:	CmpW    printerPageHeight, privFHData+privfhdata::pageHeight
	bne     @error
@4:	jsr     loadR0FnBuffer
        jsr     swapUserZp
        jsr     OpenRecordFile
        jsr     swapUserZp
        bnex    @exit2
        lda     fileHeader+3
        sta     r1H
        lda     fileHeader+2
        sta     r1L
        beq     @5
        jsr     ldR4DiskBlkBuf
        jsr     _GetBlock
        bnex    @error
        lda     diskBlkBuf+O_RULER_ESC_RESERVED
        and     #$10
        beq     @error
@5:	lda     #0
        sta     curPage
        sta     nextPageOpen
        rts

@exit2:	jmp     _exitToDesktop		; [XXX redundant, see @exit]

;---------------------------------------------------------------
; initApp
;
; Function:  App initialization: set defaults of global
;            variables and perform copy protection check.
;---------------------------------------------------------------
initApp:
	LoadW_  textScrapSize, 0
        sta     textScrapOnDisk
        sta     dirty
        sta     nextPageOpen
        sta     zp_DBb
        sta     inputString1
        sta     inputString2
        sta     showCopyrightFlag
        sta     sideFlippingRight_XXX
        sta     sideFlippingRight_XXX+1
	LoadW   sideFlippingLeft_XXX, SC_PIX_WIDTH-1
        jsr     checkSerialOrInstall
        jsr     swapUserZp
        jsr     CloseRecordFile		; close app
        jsr     swapUserZp
        LoadB   streamingMode, $FF
        LoadB   searchOptions, $C0
	LoadB   dispBufferOn, ST_WR_FORE
	LoadW   RecoverVector, appRecoverVector
	LoadB   showPicturesFlag, $FF
	START_IO
        LoadB   $D015, 1
	END_IO
        rts

.include "scantextscrap.inc"

CODE1_END:
