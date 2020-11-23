.include "jumptab.inc"
.include "config.inc"

.feature string_escapes

CPU_DDR         := $0000
CPU_DATA        := $0001
r0              := $0002
r0L             := $0002
r0H             := $0003
r1              := $0004
r1L             := $0004
r1H             := $0005
r2              := $0006
r2L             := $0006
r2H             := $0007
r3              := $0008
r3L             := $0008
r3H             := $0009
r4              := $000A
r4L             := $000A
r4H             := $000B
r5              := $000C
r5L             := $000C
r5H             := $000D
r6              := $000E
r6L             := $000E
r6H             := $000F
r7              := $0010
r7L             := $0010
r7H             := $0011
r8              := $0012
r8L             := $0012
r8H             := $0013
r9              := $0014
r9L             := $0014
r9H             := $0015
r10             := $0016
r10L            := $0016
r10H            := $0017
r11             := $0018
r11L            := $0018
r11H            := $0019
r12             := $001A
r12L            := $001A
r12H            := $001B
r13             := $001C
r13L            := $001C
r13H            := $001D
r14             := $001E
r14L            := $001E
r14H            := $001F
r15             := $0020
r15L            := $0020
r15H            := $0021
curPattern      := $0022
string          := $0024
baselineOffset  := $0026
curSetWidth     := $0027
curHeight       := $0029
curIndexTable   := $002A
cardDataPntr    := $002C
currentMode     := $002E
dispBufferOn    := $002F
mouseOn         := $0030
msePicPtr       := $0031
windowTop       := $0033
windowBottom    := $0034
leftMargin      := $0035
rightMargin     := $0037
pressFlag       := $0039
mouseXPos       := $003A
mouseYPos       := $003C
returnAddress   := $003D
graphMode       := $003F
a2              := $0070
a2L             := $0070
a2H             := $0071
curFont         := $0072
a4              := $0074
a4L             := $0074		; byte, several flags
a4H             := $0075		; index, related to cursor::srcline
pagePosY        := $0076		; a5, word, y position of screen on page
a6              := $0078
a6L             := $0078
a6H             := $0079
a7              := $007A
a7L             := $007A
a7H             := $007B
topScrTxtPtr    := $007C ; a8, txtPtr of the first line currently on the screen
a9              := $007E
a9L             := $007E
a9H             := $007F
vars            := $0200
BACK_SCR_BASE   := $6000
PRINTBASE       := $7900
InitForPrint    := $7900
StartPrint      := $7903
PrintBuffer     := $7906
StopPrint       := $7909
GetDimensions   := $790c
PrintASCII      := $790f
StartASCII      := $7912
SetNLQ          := $7915
diskBlkBuf      := $8000
fileHeader      := $8100
curDirHead      := $8200
fileTrScTab     := $8300
dirEntryBuf     := $8400
DrACurDkNm      := $841E
DrBCurDkNm      := $8430
dataFileName    := $8442
dataDiskName    := $8453
PrntFilename    := $8465
PrntDiskName    := $8476
_driveType      := $8486
curDrive        := $8489
diskOpenFlg     := $848A
isGEOS          := $848B
interleave      := $848C
NUMDRV          := $848D
driveType       := $848E
turboFlags      := $8492
curRecord       := $8496
usedRecords     := $8497
fileWritten     := $8498
fileSize        := $8499
appMain         := $849B
intTopVector    := $849D
intBotVector    := $849F
mouseVector     := $84A1
keyVector       := $84A3
inputVector     := $84A5
mouseFaultVec   := $84A7
otherPressVec   := $84A9
StringFaultVec  := $84AB
alarmTmtVector  := $84AD
BRKVector       := $84AF
RecoverVector   := $84B1
selectionFlash  := $84B3
alphaFlag       := $84B4
iconSelFlg      := $84B5
faultData       := $84B6
menuNumber      := $84B7
mouseTop        := $84B8
mouseBottom     := $84B9
mouseLeft       := $84BA
mouseRight      := $84BC
stringX         := $84BE
stringY         := $84C0
mousePicData    := $84C1
maxMouseSpeed   := $8501
minMouseSpeed   := $8502
mouseAccel      := $8503
keyData         := $8504
mouseData       := $8505
inputData       := $8506
mouseSpeed      := $8507
random          := $850A
savelintab_font     := $850C
dblClickCount   := $8515
year            := $8516
month           := $8517
day             := $8518
hour            := $8519
minutes         := $851A
seconds         := $851B
alarmSetFlag    := $851C
sysDBData       := $851D
screencolors    := $851E
dlgBoxRamBuf    := $851F
realSize        := $8807
savedmoby2      := $88BB
scr80polar      := $88BC
scr80colors     := $88BD
vdcClrMode      := $88BE
driveData       := $88BF
ramExpSize      := $88C3
sysRAMFlg       := $88C4
firstBoot       := $88C5
curType         := $88C6
ramBase         := $88C7
inputDevNam     := $88CB
DrCCurDkNm      := $88DC
DrDCurDkNm      := $88EE
dir2Head        := $8900
spr0pic         := $8A00
spr1pic         := $8A40
spr2pic         := $8A80
spr3pic         := $8AC0
spr4pic         := $8B00
spr5pic         := $8B40
spr6pic         := $8B80
spr7pic         := $8BC0
COLOR_MATRIX    := $8C00
sprp            := $8FE8
DISK_BASE       := $9000
SCREEN_BASE     := $A000
OS_ROM          := $C000
EXP_BASE        := $DF00
MOUSE_JMP       := $FE80
config          := $FF00
NMI_VECTOR      := $FFFA
RESET_VECTOR    := $FFFC
IRQ_VECTOR      := $FFFE

; GEOS symbol additions
MISIZE               := 5		; menu item size
mobprior             := $D01B
O_GHP_PRIVATE        := O_GHP_FNAME + 20
CBM_LOGO             := $80
O_RULER_ESC_RESERVED := 25		; offset of 2 res. bytes in ruler escape string
OFF_GHFONTID         := $80		; 2 bytes  - unique font id
OFF_GHPOINT_SIZES    := $82		; 32 bytes - font id for each character set
OFF_GHSET_LENGTHS    := $61		; 32 bytes - size in bytes for each character set
FONT_SIZE_MASK       := $3F
FONTS_PER_FONTFILE   := 16
MASK_JST_JUST        := $03
MASK_JST_SPACING     := $0C
SHIFT_JST_SPACING    := 2

; non-standard justification flags
JST_80                := $80		; flag: related to picture
JST_STARTS_WITH_RULER := $40
JST_20                := $20
JST_10                := $10		; flag: first line of paragraph

; a4L flags
A4L_01			= $01
A4L_02			= $02
A4L_04			= $04
A4L_08			= $08
A4L_10			= $10
A4L_20			= $20
A4L_80			= $80

;;;;

inputString1           = vars+$00	; 25 chars, NULL
inputString2           = vars+$1A	; 25 chars, NULL

metricsFifoCounter     = vars+$34	; byte
metricsUseSystem       = vars+$35	; byte
metricsWidths          = vars+$36	; word
metricsTmp             = vars+$38	; byte

showPicturesFlag       = vars+$3A       ; byte: flag whether to show pictures
curCodeRecord          = vars+$3B	; byte: currently loaded overlay record
streamingMode              = vars+$3C	; flag: read across pages when reading bytes
metricsIds             = vars+$3D 	; 16 bytes
metricsHeights         = vars+$4D 	; 8 bytes

cachedFontCharWidths4  = vars+$134	; 96 bytes
cachedFontCharWidths5  = vars+$194	; 96 bytes

metricsBaselineOffsets = vars+$1F4	; 8 bytes


; one for each line visible on the screen: (200 - 32) / 8
lintab_txtPtr_lo       = appInit +   0; $2B36; 21
lintab_txtPtr_hi       = appInit +  21; $2B4B; 21
lintab_y               = appInit +  42; $2B60; 21	; from here on, the entries are
lintab_height          = appInit +  63; $2B75; 21	; in the same order as O_CSR_*
lintab_font_lo         = appInit +  84; $2B8A; 21	; (see moveCursor3Lintab)
lintab_font_hi         = appInit + 105; $2B9F; 21
lintab_mode            = appInit + 126; $2BB4; 21
lintab_baselineOffset  = appInit + 147; $2BC9; 21
lintab_justification   = appInit + 168; $2BDE; 21
lintab_AAw_lo          = appInit + 189; $2BF3; 42 [XXX ???]
lintab_AAw_hi          = appInit + 210; $2C08; 21
lintab_width           = appInit + 231; $2C1D; 21

; one for each page of the document
heightForPage_lo       = appInit + 252; $2C32; 61 entries
heightForPage_hi       = appInit + 313; $2C6F; 61 entries
widthForPage_lo        = appInit + 374; $2CAC; 61 entries
widthForPage_hi        = appInit + 435; $2CE9; 61 entries

cachedFontCharWidths0  = appInit + 496 ; 96 bytes
cachedFontCharWidths1  = appInit + 592 ; 96 bytes
cachedFontCharWidths2  = appInit + 688 ; 96 bytes
cachedFontCharWidths3  = appInit + 784 ; 96 bytes


kbdString              = appInit + 880; $2EA6 ; 20 chars
  photoWidth           = appInit + 881; $2EA7 ; byte [XXX never read!]    | overlaps with kbdString,
  photoHeight          = appInit + 882; $2EA8 ; word [XXX not necessary!] | which is okay
  tmpPicRecord         = appInit + 884; $2EAA ; byte [XXX never read!]    |
pageWidth1             = appInit + 910; $2EC4
showCopyrightFlag      = appInit + 912; $2EC6
printerPageHeight      = appInit + 913; $2EC7
docFileHeaderTrkSec    = appInit + 915; $2EC9
hasPagePropMetadata    = appInit + 917; $2ECB
otherFnBuffer          = appInit + 918; $2ECC
fnBuffer               = appInit + 935; $2EDD
userzp_copy            = appInit + 952; $2EEE ; 128 bytes
rulerData1             = appInit + 1080; $2F6E; 26 bytes
rulerData2             = appInit + 1106; $2F88; 26 bytes (-> $2FA1)

tmpRulerData           = appInit + 1132; $2FA2
unusedFlag             = appInit + 1133; $2FA3 ; flag [XXX never read!]
sideFlippingOffset     = appInit + 1158; $2FBC ; 0: 0px, 1: 160px, 2: 320px
tmpSideFlippingOffset  = appInit + 1159; $2FBD
L2FBE                  = appInit + 1160; $2FBE
L2FBF                  = appInit + 1161; $2FBF
L2FC0                  = appInit + 1162; $2FC0
fontNames              = appInit + 1163; $2FC1 ; FONT_NAME_SIZE=19 bytes per font (*8)

diskFontIds            = appInit + 1299; $3049	; 8 fonts with 8 ID words each (until $30C8)

_diskFontRecordTrkSec  = appInit + 1427; $30C9  ; 7 fonts with 8 track/sector words each (1-7)
diskFontRecordTrkSec   = _diskFontRecordTrkSec - 16	; never used for font #0 (system), so shifted up

diskFontRecordSize     = appInit + 1523; $3129  ; word * 8

deskAccNames           = appInit + 1651; $31A9
appIndexTable          = appInit + 1787; $3231

CODE_MAIN              = $0400

MEM_OVERLAY            = MEM_TEXTSCRAP - MEM_SIZE_OVERLAY   ; $3244 - overlay code record
MEM_TEXTSCRAP          = MEM_PAGE      - MEM_SIZE_TEXTSCRAP ; $41E4 -
MEM_PAGE               = MEM_FONT      - MEM_SIZE_PAGE      ; $4310 - holds current page
MEM_FONT               = MEM_SCRRECV   - MEM_SIZE_FONTS     ; $5E68 - font buffer
MEM_SCRRECV            = $8000         - MEM_SIZE_SCRRECV   ; $75D8 - memory region for saving/restoring screen contents

L7F00                  = $8000 - $100 ; top page

MEM_PRINTDATA          = $7680		; 8 lines of data to be printed (640 bytes)
MEM_PRINTWORKBUF       = $0400		; work buffer for printer driver (3*640 bytes)

MEM_OVERLAY_PRT        = $0680

; $2B36 globals1 (appInit)
; $3244 overlay
; $41E4 globals2
; $4310 page
; $5E68 font buffer
; $75D8 screen recover buffer

cachedFontCharWidths6  = spr2pic+$00	; $8A80
cachedFontCharWidths7  = spr2pic+$60	; $8AE0

; information about currently loaded fonts - all of these these are 8 bytes each
loadedFontIdsLo        = spr5pic+$00 	; $8B40
loadedFontIdsHi        = spr5pic+$08
loadedFontPtrsLo       = spr5pic+$10
loadedFontPtrsHi       = spr5pic+$18
loadedfontDataSizeLo   = spr5pic+$20
loadedfontDataSizeHi   = spr5pic+$28
loadedFontLruIdLo      = spr5pic+$30
loadedFontLruIdHi      = spr5pic+$38

textScrapSize          = MEM_TEXTSCRAP + 0	; $41E4
textScrapData          = MEM_TEXTSCRAP + 2	; $41E6, 298 bytes, up to $4310
MAX_TEXTSCRAP_SIZE     = MEM_SIZE_TEXTSCRAP - 2

; geoWrite V2.0 Page Format
;  0-26  Ruler escape string
;        0     constant ESC_RULER
;        1-2   left margin
;        3-4   right margin
;        5-20  tabs
;        21-22 paragraph margin
;        23    justification
;              internal additions:
;                $20: ???
;                $10: first line of paragraph
;        24    text color (unused)
;        25-26 unused
; 27-30  font/style escape
;        0    constant NEWCARDSET
;        1-2  font ID
;        3    style byte
; 31-..  Text of document

.struct ruler
	left_margin      .word
	right_margin     .word
	tabs             .res 16
	paragraph_margin .word
	justification    .byte
	unused1          .byte
	unused2          .byte
	unused3          .byte
.endstruct

.struct cardset
	magic		.byte
	fontid		.word
	style		.byte
.endstruct

.struct page
	magic		.byte
	ruler		.res .sizeof(ruler)	; offset 1
	cardset		.res .sizeof(cardset)	; offset 27
	text		.byte			; offset 31
.endstruct

PAGE_RULER	= MEM_PAGE+page::ruler
PAGE_CARDSET	= MEM_PAGE+page::cardset	; $432B

; $4310: ESC_RULER
; $4311: LEFT_MARGIN
; $4313: RIGHT_MARGIN
; $4325: PARAGRAPH_MARGIN
; $4327: JUSTIFICATION
; $4328: UNUSED1
; $4329: UNUSED2
; $432A: UNUSED3

L4C95                  = $4C95 ; ???

NUM_APP_RECORDS  = 9

MEM_SIZE_MAIN      = 1920		; for re-loading only
MEM_SIZE_OVERLAY   = 4000
MEM_SIZE_TEXTSCRAP = 300
MEM_SIZE_FONTS     = 6000
MEM_SIZE_PAGE      = 7000
MEM_SIZE_SCRRECV   = 2600		; 25 cards, 104 lines, -> scrrecvtab_dlgbox

MAX_NUM_PAGES    = $3D
PAGE_LAST_USABLE = $3C
PAGE_HEADER      = $3D
PAGE_FOOTER      = $3E
PAGE_RESERVED    = $3F
PAGE_FIRST_IMAGE = $40

RECORD_1 = 1
RECORD_2 = 2
RECORD_3 = 3
RECORD_4 = 4
RECORD_5 = 5
RECORD_6 = 6
RECORD_7 = 7
RECORD_8 = 8

PROCESS_SELECTION     = 0
PROCESS_1             = 1
PROCESS_2             = 2
PROCESS_SIDE_FLIPPING = 3

SYSTEM_FONT_ID = $0009
SYSTEM_FONT_SIZE = SYSTEM_FONT_ID & $3F
SYSTEM_FONT_BASELINE = 6
MAX_FONT_SIZE = 42
MAX_HEADER_FOOTER_HEIGHT = 220 ; size of header/footer virtual pages in pixels
PAGE_WIDTH_NARROW = 480
PAGE_WIDTH_WIDE = 640
DEFAULT_PAGE_HEIGHT = 752
RULER_ESC_LENGTH = 28
MAX_FONTS_LOADED = 8
MAX_FONT_FILES = 8
FONT_NAME_SIZE = 19

.macro PushW_r15
	jsr     pushR15
.endmacro
.macro PopW_r15
	jsr     popR15
.endmacro

.macro LoadB_y dest, value
	ldy #value
	sty dest
.endmacro

.macro LoadB_x dest, value
	ldx #value
	stx dest
.endmacro

.macro LoadW_y dest, value
	LoadB_y dest+1, >(value)
	.if (.not .const(value)) .or (>(value))<>(<(value))
	ldy #<(value)
	.endif
	sty dest+0
.endmacro

.macro LoadW_x dest, value
	LoadB_x dest+1, >(value)
	.if (.not .const(value)) .or (>(value))<>(<(value))
	ldx #<(value)
	.endif
	stx dest+0
.endmacro

.macro MoveB_y source, dest
	ldy source
	sty dest
.endmacro

.macro MoveB_x source, dest
	ldx source
	stx dest
.endmacro

.macro MoveW_x source, dest
	MoveB_x source+1, dest+1
	MoveB_x source+0, dest+0
.endmacro

.macro MoveW_y source, dest
	MoveB_y source+1, dest+1
	MoveB_y source+0, dest+0
.endmacro

.macro MoveW_y_ source, dest
	MoveB_y source+0, dest+0
	MoveB_y source+1, dest+1
.endmacro

.macro AddW_ source, dest
	lda dest
	add source
	sta dest
	lda dest+1
	adc source+1
	sta dest+1
.endmacro

.macro AslW addr
	asl addr
	rol addr+1
.endmacro

.macro AddBW source, dest
	lda dest
	clc
	adc source
	sta dest
	lda dest+1
	adc #0
	sta dest+1
.endmacro

.macro AddVW2 value, source, dest
	lda source
	add #<(value)
	sta dest
	lda source+1
	adc #>(value)
	sta dest+1
.endmacro

.macro SubVW2 value, source, dest
	lda source
	sub #<(value)
	sta dest
	lda source+1
	sbc #>(value)
	sta dest+1
.endmacro

.macro SubVW2_ value, source, dest
	sec
	lda source
	sbc #<(value)
	sta dest
	lda source+1
	sbc #>(value)
	sta dest+1
.endmacro

.macro SubW3 source1, source2, dest
	lda source1
	sub source2
	sta dest
	lda source1+1
	sbc source2+1
	sta dest+1
.endmacro

.macro AddW3 source1, source2, dest
	lda source1
	add source2
	sta dest
	lda source1+1
	adc source2+1
	sta dest+1
.endmacro

.macro AddVW__ value, dest
	.local @j
	lda #<(value)
	add dest
	sta dest
	lda #>(value)
	adc dest + 1
	sta dest + 1
.endmacro

.macro CmpB_x source, dest
	ldx source
	cpx dest
.endmacro

.macro CmpB_y source, dest
	ldy source
	cpy dest
.endmacro

.macro CmpW_x source, dest
	.local @j
	CmpB_x source+1, dest+1
	bne @j
	CmpB_x source+0, dest+0
@j:
.endmacro

.macro CmpW_y source, dest
	.local @j
	CmpB_y source+1, dest+1
	bne @j
	CmpB_y source+0, dest+0
@j:
.endmacro

.macro SubVB value, dest
	sec
	lda dest
	sbc #<(value)
	sta dest
.endmacro

.macro SubB_ source, dest
	sec
	lda dest
	sbc source
	sta dest
.endmacro

.macro AddB__ source, dest
	clc
	lda dest
	adc source
	sta dest
.endmacro

.macro AddB___ source, dest
	lda dest
	add source
	sta dest
.endmacro

.macro DecW addr
	.local @j
	lda addr
	bne @j
	dec addr+1
@j:	dec addr
.endmacro
