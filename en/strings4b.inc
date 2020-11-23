txt_cant_copy_pic:
	.byte   BOLDON
        .byte   "Can't copy picture to scrap"
        .byte   0

txt_copying_photscrap:
        .byte   "copying photoScrap into"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "data file."
        .byte   0

txt_page_too_big_inserting_break:
	.byte   BOLDON
        .byte   "Page too big --"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Inserting page break"
        .byte   0

txt_disk_near_full:
	.byte   BOLDON
        .byte   "WARNING!"
        .byte   PLAINTEXT,GOTOXY
        .word   80
        .byte   64
        .byte   "Disk near full."
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "Please move/delete files"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "from disk to continue."
        .byte   0

txt_too_many_pages:
	.byte   BOLDON
        .byte   "There are too many pages in"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "this file.  Text at the end"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "is being lost."
        .byte   0

txt_cant_add_pages:
	.byte   BOLDON
        .byte   "Cannot add any more pages"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "to this file.  Cannot insert"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "page break."
        .byte   0
