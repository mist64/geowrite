txt_needs_conversion:
	.byte   BOLDON
	.byte	"Datei ist vor Ausdruck"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "von geoWrite zu |ffnen!"
	.byte	0

txt_serial_mismatch:
	.byte	BOLDON
	.byte	"Bitte neu starten."
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Gleiche Diskette verwenden"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "wie bei Installation von"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "geoWrite."
	.byte	0

txt_install_write_protected:
        .byte   BOLDON
	.byte	"geoWrite nicht auf"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "schreibgesch}tzter Diskette"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "installierbar."
	.byte	0

txt_error_installing:
        .byte   BOLDON
	.byte	"Installationsfehler"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "bei geoWrite"
	.byte	0

txt_copy_protection:
	.byte   BOLDON
	.byte	"geoWrite nicht auf dieser"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Disk installierbar.  Bitte"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "Anleitung lesen!"
	.byte	0

txt_installed:
        .byte   BOLDON
	.byte	"geoWrite 2.1 installiert."
	.byte	0
