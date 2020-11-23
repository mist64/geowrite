txt_same_disk:
	.byte   BOLDON
	.byte	"Bei nur 1 Laufwerk"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "m}ssen geoWrite und Dokument"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "auf selber Disk stehen."
        .byte	0
