; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  file header
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

.include "sym.inc"
.include "const.inc"
.include "geoWrite-0.inc"

.segment "FHDR"

	.byte $00,$ff ; sector link

	.byte $03,$15 ; width/height of icon
	.byte $bf     ; bitmap data type
	.byte $ff,$ff,$ff,$80,$00,$01,$b0,$80,$7d,$88,$81,$cd,$98,$87,$19,$a0
	.byte $8c,$31,$ba,$98,$61,$80,$30,$c1,$80,$69,$81,$80,$d3,$01,$80,$ae
	.byte $01,$81,$98,$01,$81,$70,$79,$82,$c0,$85,$83,$00,$79,$82,$00,$bd
	.byte $84,$01,$7f,$84,$3d,$ff,$8b,$e0,$fd,$80,$00,$01,$ff,$ff,$ff

	.byte $83         ; C64 file type
	.byte APPLICATION ; GEOS file type
	.byte VLIR

	.word $0400 ; start
	.word $03ff ; end
	.word appInit ; init

	.byte "geoWrite    V2.1",0,0,0,0
	.byte "Tony Requist",0,0,0,0,0,0,0,0

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$67

.if LANG=LANG_DE
	.byte "geoWrite (64 Version) ist eine WYSIWYG-Textverarbeitung.",0

	.byte $e1,$35,$a9,$36,$85,$03,$a9,$c5,$85,$02,$20,$56,$c2,$a5,$02,$c9
	.byte $05,$d0,$19,$a9,$00,$85,$02,$85,$06,$85,$07,$85,$08,$85,$09,$85
	.byte $16,$a9,$04,$85,$11,$a9,$00
.else
	.byte "geoWrite (64 version) is a WYSIWYG word processor.",0

	.byte $db,$35,$a9,$36,$85,$03,$a9,$bf,$85,$02,$20,$56,$c2
	.byte $a5,$02,$c9,$05,$d0,$19,$a9,$00,$85,$02,$85,$06,$85,$07,$85,$08
	.byte $85,$09,$85,$16,$a9,$04,$85,$11,$a9,$00,$85,$10,$4c,$2f,$c2,$4c
.endif
