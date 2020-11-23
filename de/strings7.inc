txt_printer_is_inaccessible:
        .byte   BOLDON
        .byte	"Drucker nicht ansprechbar"
        .byte	PLAINTEXT,0

txt_printing:
        .byte   BOLDON
        .byte	" Drucke..."
        .byte	0

txt_previewing:
        .byte  BOLDON
        .byte	"]bersicht..."
        .byte	0

txt_print:
        .byte BOLDON
        .byte	"Drucke..."
        .byte	0

txt_high_draft_nlq:
        .byte   "Hoch"
        .byte   GOTOX
        .word   $9F
        .byte   "Entwf"
        .byte   GOTOX
        .word   $D7
        .byte   "NLQ"
        .byte	0

txt_from_to_page:
        .byte   "Von Seite"
        .byte   GOTOX
        .word   $AA
        .byte   "bis Seite"
        .byte	0

txt_single_tractor:
        .byte   "Einzelblatt"
        .byte   GOTOX
        .word   $BA
        .byte   "Endlospapier"
        .byte	0

txt_insert_next_sheet:
        .byte BOLDON
        .byte	"Bitte legen Sie das"
        .byte	0

txt_to_continue_printing:
        .byte   "n{chste Blatt Papier ein, um"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "den Ausdruck fortzusetzen."
        .byte	0

txt_date_token:
	.byte   "DATUM"
txt_date_token_len = * - txt_date_token
	.byte	0

txt_time_token:
	.byte   "ZEIT"
txt_time_token_len = * - txt_time_token
	.byte	0

txt_page_token:
	.byte   "SEITE"
txt_page_token_len = * - txt_page_token
	.byte	0

txt_jan:	.byte   "Januar",0
txt_feb:        .byte   "Februar",0
txt_mar:        .byte   "M{rz",0
txt_apr:        .byte   "April",0
txt_may:        .byte   "Mai",0
txt_jun:        .byte   "Juni",0
txt_jul:        .byte   "Juli",0
txt_aug:        .byte   "August",0
txt_sep:        .byte   "September",0
txt_oct:        .byte   "Oktober",0
txt_nov:        .byte   "November",0
txt_dec:        .byte   "Dezember",0

