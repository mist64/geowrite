txt_showhide:
txt_show:
	.byte   PLAINTEXT,PLAINTEXT
txt_show_len = * - txt_show
txt_hide:
	.byte	"* "
.assert * - txt_hide = txt_show_len, error; need to be the same size

txt_photoscrap_too_new:
	.byte   BOLDON
	.byte	"\"photo Scrap\" nicht"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "f}r V2.1"
        .byte   0

txt_picture_area_full:
        .byte   BOLDON
        .byte   "Bildbereich der Datei voll."
        .byte   0

txt_no_photoscrap:
	.byte   BOLDON
        .byte   "Keine \"photoScrap\""
        .byte	GOTOXY
        .word   80
        .byte   64
        .byte   "Datei auf Diskette"
        .byte   0

txt_photoscrap_too_big:
	.byte   BOLDON
        .byte   "photoScrap zu gro~."
        .byte   0
