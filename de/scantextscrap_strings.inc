txt_textscrap_incompat:
	.byte   BOLDON
	.byte	"\"Text Scrap\""
        .byte   " nicht"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "f}r V2.1"
	.byte	0

txt_reading_textscrap:
	.byte   "lese textScrap",0
