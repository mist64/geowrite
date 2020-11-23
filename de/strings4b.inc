txt_cant_copy_pic:
	.byte   BOLDON
        .byte   "Bild nicht in"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Scrap zu }bernehmen."
        .byte   0

txt_copying_photscrap:
        .byte   "kopiere photoScrap in"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "Datendatei"
        .byte   0

txt_page_too_big_inserting_break:
	.byte   BOLDON
        .byte   "Seite zu gro~ --"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "F}ge neue Seitengrenze ein"
        .byte   0

txt_disk_near_full:
	.byte   BOLDON
        .byte   "WARNUNG!"
        .byte   PLAINTEXT,GOTOXY
        .word   80
        .byte   64
        .byte   "Diskette fast voll:"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "zuerst Dateien von Diskette"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "l|schen/kopieren."
        .byte   0

txt_too_many_pages:
	.byte   BOLDON
        .byte   "Wegen zu vieler Seiten"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "geht das Textende verloren."
        .byte   0

txt_cant_add_pages:
	.byte   BOLDON
        .byte   "Kein Platz f}r weitere"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Seiten: Seitengrenze wird"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "nicht gesetzt."
        .byte   0
