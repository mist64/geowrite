txt_same_disk:
	.byte   BOLDON
	.byte	"Both geoWrite and data file"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "must be on the same disk"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "in a one drive system."
        .byte	0
