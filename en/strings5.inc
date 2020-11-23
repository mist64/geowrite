aboutText:
	.byte   BOLDON,"geoWrite by Tony Requist"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Version 2.1 for the C64"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "Copyright 1985, 1986, 1987"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "Berkeley Softworks",0

txt_accessory_disk_full:
	.byte   BOLDON
        .byte   "Not enough free space on"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "disk for desk accessory."
	.byte	0

txt_please_select_option:
        .byte   BOLDON
        .byte	"Please Select Option:"
        .byte	PLAINTEXT,0

txt_new_document:
        .byte   "new document"
        .byte	0

txt_existing_document:
        .byte   "existing document"
        .byte	0

txt_to_desktop:
        .byte   "to deskTop"
        .byte	0

txt_insert_new_disk:
	.byte   BOLDON
        .byte	"Insert new disk into disk drive."
        .byte	0

txt_file_exists:
	.byte   BOLDON
        .byte	"File exists, choose another.",PLAINTEXT
        .byte	0

txt_document_version_error:
	.byte   BOLDON
        .byte	"Data file is beyond V2.1"
        .byte	0

txt_converting_file:
        .byte	BOLDON
        .byte	"Converting earlier file to "
        .byte	"V2."
txt_converting_file_digit:
	.byte   "1"
        .byte   0

txt_converting_error:
	.byte   "converting file."
        .byte	0

txt_on_disk:
	.byte   BOLDON
        .byte	"On disk:"
        .byte	PLAINTEXT,0

txt_warning:
	.byte   BOLDON
        .byte	"WARNING!"
        .byte	0

txt_file_is_write_protected:
        .byte   "This file is write protected."
        .byte	0

txt_cannot_recover:
	.byte   BOLDON
        .byte   "Cannot recover from file."
        .byte	0

txt_enter_filename:
	.byte   BOLDON
        .byte	"Please enter new filename:"
        .byte	PLAINTEXT,0

txt_opening_file:
	.byte   "opening file"
        .byte	0

txt_creating_file:
	.byte   "creating file"
        .byte	0

txt_running_accessory:
	.byte   "running desk accessory"
        .byte	0

txt_cannot_run_accessory:
	.byte   BOLDON
        .byte	"Cannot run desk accessory"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "while in header or footer"
        .byte	0

txt_filename_exists:
	.byte   BOLDON
        .byte	"File name already exists"
        .byte	0
