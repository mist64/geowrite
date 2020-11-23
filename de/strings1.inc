txt_geowrite_info:
	.byte   "geoWrite Info",0
txt_geos:
	.byte   "geos",0
txt_file:
	.byte   "Datei",0
txt_edit:
        .byte   "Edit",0
txt_font:
        .byte   "Schrift",0
txt_style:
        .byte   "Stil",0
txt_options:
        .byte   "Opt",0
txt_page:
        .byte   "Seite",0
txt_close:
        .byte   "schlie~en",0
txt_update:
        .byte   "aktualisieren",0
txt_preview:
        .byte   "]bersicht",0
txt_recover:
        .byte   "wiederherstellen",0
txt_rename:
        .byte   "umbenennen",0
txt_print:
        .byte   "drucken",0
txt_quit:
        .byte   "Ende",0

txt_cut:
        .byte   "ausschneiden"
        .byte   GOTOX
        .word   $7E
        .byte   CBM_LOGO,BOLDON,"X",PLAINTEXT,0

txt_copy:
        .byte   "kopieren"
        .byte   GOTOX
        .word   $7E
        .byte   CBM_LOGO,BOLDON,"C",PLAINTEXT,$00

txt_paste:
        .byte   "einkleben",0

txt_text:
        .byte   "Text"
        .byte   GOTOX
        .word   $BE
        .byte   CBM_LOGO,BOLDON,"T",PLAINTEXT,0

txt_picture:
        .byte   "Bild"
        .byte   GOTOX
        .word   $BE
        .byte   CBM_LOGO,BOLDON,"W",PLAINTEXT,0

txt_search:
        .byte   "suchen"
        .byte   GOTOX
        .word   $A8
        .byte   CBM_LOGO,BOLDON,"S",PLAINTEXT,0

txt_find_next:
        .byte   "weiter suchen"
        .byte   GOTOX
        .word   $A8
        .byte   CBM_LOGO,BOLDON,"N",PLAINTEXT,0

txt_change:
        .byte   "ersetzen->suchen"
        .byte   GOTOX
        .word   $A8
        .byte   CBM_LOGO,BOLDON,"Y",PLAINTEXT,0

txt_previous_page:
        .byte   "zur}ck"
        .byte   GOTOX
        .word   $AE
        .byte   CBM_LOGO
        .byte   ESC_GRAPHICS
        .byte   MOVEPENTO
        .word   $BF
        .byte   21
        .byte   LINETO
        .word   $B9
        .byte   21
        .byte   LINETO
        .word   $BB
        .byte   $13
        .byte   LINETO
        .word   $BB
        .byte   $17
        .byte   LINETO
        .word   $B9
        .byte   21
        .byte   NULL

txt_next_page:
        .byte   "vor"
        .byte   GOTOX
        .word   $AE
        .byte   CBM_LOGO,BOLDON,"+",PLAINTEXT,0

txt_goto_page:
        .byte   "gehe auf Seite"
        .byte   GOTOX
        .word   $AE
        .byte   CBM_LOGO,BOLDON,"G",PLAINTEXT,0

txt_page_break:
        .byte   "Seitenende"
        .byte   GOTOX
        .word   $AE
        .byte   CBM_LOGO,BOLDON,"L",PLAINTEXT,0

txt_set_first_page:
        .byte   "erste Seiten",0

txt_plain_text:
	.byte   "  normal"
        .byte   GOTOX
	.word   $EE
        .byte   CBM_LOGO,BOLDON,"P",PLAINTEXT,$00

txt_bold:
	.byte   "  ",BOLDON
	.byte   "fett"
	.byte   PLAINTEXT,GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,"B",PLAINTEXT,$00

txt_italic:
	.byte   "  ",ITALICON
	.byte   "kursiv"
	.byte   PLAINTEXT,GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,"I",PLAINTEXT,$00

txt_outline:
	.byte   "  ",OUTLINEON
	.byte   "Kontur"
	.byte   PLAINTEXT,GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,"O",PLAINTEXT,$00

txt_underline:
	.byte   "  ",ULINEON
	.byte   "unterstrichen"
	.byte   PLAINTEXT,GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,"U",PLAINTEXT,$00

txt_superscript:
	.byte   "  hoch"
	.byte   GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,">",PLAINTEXT,$00

txt_subscript:
	.byte   "  tief"
	.byte   GOTOX
	.word   $EE
	.byte   CBM_LOGO,BOLDON,"<",PLAINTEXT,$00

txt_xx_point:
        .byte   "  xx Punkt",0
txt_xx_point_len = * - txt_xx_point
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0
        .byte   "  xx Punkt",0

