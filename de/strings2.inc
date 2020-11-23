txt_hide_pictures:
        .byte   "* Bilder",0

txt_open_header:
        .byte   PLAINTEXT, PLAINTEXT	; [XXX]
        .byte   "Kopfzeilen"
        .byte   GOTOX
        .word   $A8
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "H"
        .byte   PLAINTEXT,0

txt_open_footer:
        .byte   PLAINTEXT,PLAINTEXT
        .byte	"Fu~zeilen"
        .byte   GOTOX
        .word   $A8
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "F"
        .byte   PLAINTEXT,0

txt_select_page:
        .byte   "Seite w{hlen"
        .byte   GOTOX
        .word   $A8
        .byte   $80 ; C=
        .byte   BOLDON
        .byte   "V"
        .byte   PLAINTEXT,0

txt_make_full_page_wide:
        .byte   "Volle Breite",0

txt_title_page:
        .byte   "  Titel",0

txt_nlq_spacing:
        .byte   "  NLQ-Abst{nde",0

txt_swapping_geowrite:
	.byte   "tauscht geoWrite aus",0

txt_ioerror:
	.byte   BOLDON,"Fehler I:"

txt_ioerror_errcode:
	.byte   "  "
	.byte   " ",0

txt_insufficient_disk_space:
        .byte   BOLDON
        .byte	"Disk voll",0

txt_page_too_big:
        .byte   BOLDON
        .byte	"Seite zu gro~",0

txt_reading_page:
        .byte   "bei Lesen",0

txt_writing_page:
        .byte   "bei Schreiben",0

txt_no_prev_page:
        .byte   BOLDON
        .byte   "Geht nicht",0

txt_keyword_header:
        .byte   "KOPFZEILE",0

txt_keyword_footer:
        .byte   "FUSSZEILE",0
