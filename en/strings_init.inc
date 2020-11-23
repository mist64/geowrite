txt_needs_conversion:
	.byte   BOLDON
	.byte	"This file must be opened by"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "geoWrite before it can"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "be printed."
	.byte	0

txt_serial_mismatch:
	.byte	BOLDON
	.byte	"Please reboot your system"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "using the same GEOS disk"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "that was first used to run"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "geoWrite."
	.byte	0

txt_install_write_protected:
        .byte   BOLDON
	.byte	"Cannot install geoWrite on a"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "write protected disk."
	.byte	0

txt_error_installing:
        .byte   BOLDON
	.byte	"Error installing geoWrite"
	.byte	0

txt_copy_protection:
	.byte   BOLDON
	.byte	"Cannot install geoWrite on"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "this disk.  Please refer to"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "installation directions in"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "the manual."
	.byte	0

txt_installed:
        .byte   BOLDON
	.byte	"geoWrite 2.1 installed."
	.byte	0
