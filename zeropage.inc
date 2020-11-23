.struct cursor
	ptr		.word ; pointer to character in buffer
	px		.word ; X coordinate of cursor on screen
	srcline		.byte ; screen line number + flags $00, $80, $FF; also used as page record number
	py		.byte ; Y coordinate of cursor on screen
	height		.byte ; height of line
	font		.word ; font id     | font and mode
	mode		.byte ; currentMode | must be consecutive
.endstruct

.struct privfhdata
	startPageNo	.word
	titleNlqFlag	.byte	; $80: title, $40: NLQ
	headerHeight	.word
	footerHeight	.word
	pageHeight	.word
.endstruct

.globalzp userzp
.globalzp cursor0
.globalzp cursor1
.globalzp cursor2
.globalzp cursor3
.globalzp tmpBaselineOffset
.globalzp justification
.globalzp zp_AAw
.globalzp tmpLineWidth
.globalzp zp_ADw
.globalzp tmpFont1
.globalzp tmpMode1
.globalzp tmpJustification1
.globalzp pageTextHeight
.globalzp tmpRangeEnd
.globalzp tmpRangeStart
.globalzp zp_B9b
.globalzp zp_BAb
.globalzp zp_BBw
.globalzp zp_BDw
.globalzp tmpFont2
.globalzp tmpMode2
.globalzp lineMaxFontHeight
.globalzp lineMaxBaselineOffset
.globalzp tmpJustification2
.globalzp tmpFont3
.globalzp tmpMode3
.globalzp numFontFiles
.globalzp lintab_fontIndex
.globalzp activeFont
.globalzp fontLruCounter
.globalzp loadedFontsCount
.globalzp setFontMarkFlag
.globalzp setStyleCheckmarkFlag
.globalzp pageEndPtr2
.globalzp curLine
.globalzp curPage
.globalzp loadOpt
.globalzp argFilename
.globalzp appDrive
.globalzp docDrive
.globalzp zp_D9b
.globalzp overhang
.globalzp zp_DAb
.globalzp zp_DBb
.globalzp curControlKey
.globalzp zp_DDb
.globalzp nextPageOpen
.globalzp tmpTrkSec
.globalzp dirty
.globalzp privFHData
.globalzp pageWidth2
.globalzp usablePageHeight
.globalzp usablePageHeightDiv13
.globalzp searchOptions
.globalzp zp_F2w
.globalzp pageEndPtr1
.globalzp tmpCurRecord
.globalzp textScrapOnDisk
.globalzp delCount
.globalzp kbdStringCnt
.globalzp curFontHeight
.globalzp curBaselineOffset
.globalzp sideFlippingRight_XXX
.globalzp sideFlippingLeft_XXX

