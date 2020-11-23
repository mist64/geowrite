txt_geowrite_info:
	.byte   "geoWrite info",0
txt_geos:
	.byte   "geos",0
txt_file:
	.byte   "file",0
txt_edit:
        .byte   "edit",0
txt_font:
        .byte   "font",0
txt_style:
        .byte   "style",0
txt_options:
        .byte   "options",0
txt_page:
        .byte   "page",0
txt_close:
        .byte   "close",0
txt_update:
        .byte   "update",0
txt_preview:
        .byte   "preview",0
txt_recover:
        .byte   "recover",0
txt_rename:
        .byte   "rename",0
txt_print:
        .byte   "print",0
txt_quit:
        .byte   "quit",0

txt_cut:
        .byte   "cut"
        .byte   GOTOX
        .word   $52
        .byte   CBM_LOGO,BOLDON,"X",PLAINTEXT,0

txt_copy:
        .byte   "copy"
        .byte   GOTOX
        .word   $52
        .byte   CBM_LOGO,BOLDON,"C",PLAINTEXT,$00

txt_paste:
        .byte   "paste",0

txt_text:
        .byte   "text"
        .byte   GOTOX
        .word   $92
        .byte   CBM_LOGO,BOLDON,"T",PLAINTEXT,0

txt_picture:
        .byte   "picture"
        .byte   GOTOX
        .word   $92
        .byte   CBM_LOGO,BOLDON,"W",PLAINTEXT,0

txt_search:
        .byte   "search"
        .byte   GOTOX
        .word   $A1
        .byte   CBM_LOGO,BOLDON,"S",PLAINTEXT,0

txt_find_next:
        .byte   "find next"
        .byte   GOTOX
        .word   $A1
        .byte   CBM_LOGO,BOLDON,"N",PLAINTEXT,0

txt_change:
        .byte   "change, then find"
        .byte   GOTOX
        .word   $A1
        .byte   CBM_LOGO,BOLDON,"Y",PLAINTEXT,0

txt_previous_page:
        .byte   "previous page"
        .byte   GOTOX
        .word   $B8
        .byte   CBM_LOGO
        .byte   ESC_GRAPHICS
        .byte   MOVEPENTO
        .word   $C7
        .byte   21
        .byte   LINETO
        .word   $C1
        .byte   21
        .byte   LINETO
        .word   $C3
        .byte   $13
        .byte   LINETO
        .word   $C3
        .byte   $17
        .byte   LINETO
        .word   $C1
        .byte   21
        .byte   NULL

txt_next_page:
        .byte   "next page"
        .byte   GOTOX
        .word   $B8
        .byte   CBM_LOGO,BOLDON,"+",PLAINTEXT,0

txt_goto_page:
        .byte   "goto page"
        .byte   GOTOX
        .word   $B8
        .byte   CBM_LOGO,BOLDON,"G",PLAINTEXT,0

txt_page_break:
        .byte   "page break"
        .byte   GOTOX
        .word   $B8
        .byte   CBM_LOGO,BOLDON,"L",PLAINTEXT,0

txt_set_first_page:
        .byte   "set first page",0

txt_plain_text:
	.byte   "  plain text"
        .byte   GOTOX
	.word   $E3
        .byte   CBM_LOGO,BOLDON,"P",PLAINTEXT,$00

txt_bold:
	.byte   "  ",BOLDON
	.byte   "bold"
	.byte   PLAINTEXT,GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,"B",PLAINTEXT,$00

txt_italic:
	.byte   "  ",ITALICON
	.byte   "italic"
	.byte   PLAINTEXT,GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,"I",PLAINTEXT,$00

txt_outline:
	.byte   "  ",OUTLINEON
	.byte   "outline"
	.byte   PLAINTEXT,GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,"O",PLAINTEXT,$00

txt_underline:
	.byte   "  ",ULINEON
	.byte   "underline"
	.byte   PLAINTEXT,GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,"U",PLAINTEXT,$00

txt_superscript:
	.byte   "  superscript"
	.byte   GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,">",PLAINTEXT,$00

txt_subscript:
	.byte   "  subscript"
	.byte   GOTOX
	.word   $E3
	.byte   CBM_LOGO,BOLDON,"<",PLAINTEXT,$00

txt_xx_point:
        .byte   "  xx point",0
txt_xx_point_len = * - txt_xx_point
        .byte   "  xx point",0
        .byte   "  xx point",0
        .byte   "  xx point",0
        .byte   "  xx point",0
        .byte   "  xx point",0
        .byte   "  xx point",0
        .byte   "  xx point",0
