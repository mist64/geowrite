txt_hide_pictures:
        .byte   "hide pictures",0

txt_open_header:
        .byte   "open",PLAINTEXT	; 5 bytes, will be overwritten by "open\x1b" or "close"
        .byte   " header"
        .byte   GOTOX
        .word   161
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "H"
        .byte   PLAINTEXT,0

txt_open_footer:
        .byte   "open",PLAINTEXT	; 5 bytes, will be overwritten by "open\x1b" or "close"
        .byte   " footer"
        .byte   GOTOX
        .word   161
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "F"
        .byte   PLAINTEXT,0

txt_select_page:
        .byte   "select page"
        .byte   GOTOX
        .word   $A1
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "V"
        .byte   PLAINTEXT,0

txt_make_full_page_wide:
        .byte   "make full page wide",0

txt_title_page:
        .byte   "  title page",0

txt_nlq_spacing:
        .byte   "  NLQ spacing",0

txt_swapping_geowrite:
	.byte   "swapping geoWrite",0

txt_ioerror:
	.byte   BOLDON,"Error I:"

txt_ioerror_errcode:
	.byte   "  "
	.byte   " ",0

txt_insufficient_disk_space:
        .byte   BOLDON
        .byte	"Insufficient disk space",0

txt_page_too_big:
        .byte   BOLDON
        .byte	"Page too big",0

txt_reading_page:
        .byte   "reading page",0

txt_writing_page:
        .byte   "writing page",0

txt_no_prev_page:
        .byte   BOLDON
        .byte   "No previous page",0

txt_keyword_header:
        .byte   "HEADER",0

txt_keyword_footer:
        .byte   "FOOTER",0
