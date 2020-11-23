; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  zero page layout
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

.include "zeropage.inc"

.segment "USERZP" : zp

userzp:

cursor0:		.res .sizeof(cursor); 10 bytes - see O_CSR_*
cursor1:		.res .sizeof(cursor); 10 bytes
cursor2:		.res .sizeof(cursor); 10 bytes
cursor3:		.res .sizeof(cursor); 10 bytes
tmpBaselineOffset:	.byte 0		; related to (max) baselineOffset
justification:		.byte 0		; flags
zp_AAw:			.word 0
tmpLineWidth:		.byte 0
zp_ADw:			.word 0
tmpFont1:		.word 0
tmpMode1:		.byte 0
tmpJustification1:	.byte 0
pageTextHeight:		.word 0
tmpRangeEnd:		.word 0
tmpRangeStart:		.word 0
zp_B9b:			.byte 0
zp_BAb:			.byte 0	; flag
zp_BBw:			.word 0
zp_BDw:			.word 0
tmpFont2:		.word 0
tmpMode2:		.byte 0	; related to currentMode
lineMaxFontHeight:	.byte 0		; max font height of this line to know offset of next line
lineMaxBaselineOffset:	.byte 0		; max baseline offset, so baselines of different fonts can be aligned
tmpJustification2:	.byte 0
tmpFont3:		.word 0		; related to font
tmpMode3:		.byte 0	; related to currentMode
numFontFiles:		.byte 0		; as used in init code
lintab_fontIndex = numFontFiles		; as used in runtime code
activeFont:		.word 0
fontLruCounter:		.word 0
loadedFontsCount:	.byte 0
setFontMarkFlag:	.byte 0		; if false, skip adding the '*' to current font in menu
setStyleCheckmarkFlag:	.byte 0		; if false, skip adding the '*' to current style in menu
pageEndPtr2:		.word 0
curLine:		.byte 0
curPage:		.byte 0
loadOpt:		.byte 0	; ST_LD_DATA, ST_PR_DATA
argFilename:		.word 0
appDrive:		.byte 0
docDrive:		.byte 0
zp_D9b:			.byte 0
overhang:		.byte 0
zp_DAb = overhang
zp_DBb:			.byte 0
curControlKey:		.byte 0
zp_DDb:			.byte 0
nextPageOpen:		.byte 0		; record for next page is open and sectors are read one by one
tmpTrkSec:		.word 0
dirty:			.byte 0
privFHData:		.res .sizeof(privfhdata); will be copied into private area of the doc's file header [PRG p.446]
pageWidth2:		.word 0
usablePageHeight:	.word 0
usablePageHeightDiv13:	.word 0
searchOptions:		.byte 0		; $80: whole word, $40: all pages
zp_F2w:			.word 0		; some length in pageptr/r15 space
pageEndPtr1:		.word 0	; points to last byte of page in memory
tmpCurRecord:		.byte 0
textScrapOnDisk:	.byte 0
delCount:		.byte 0
kbdStringCnt:		.byte 0
curFontHeight:		.byte 0
curBaselineOffset:	.byte 0
sideFlippingRight_XXX:	.word 0
sideFlippingLeft_XXX:	.word 0
