txt_printer_is_inaccessible:
        .byte   BOLDON
        .byte	"Printer is inaccessable."
        .byte	PLAINTEXT,0

txt_printing:
        .byte   BOLDON
        .byte	" Printing..."
        .byte	0

txt_previewing:
        .byte  BOLDON
        .byte	"Previewing..."
        .byte	0

txt_print:
        .byte BOLDON
        .byte	"Print..."
        .byte	0

txt_high_draft_nlq:
        .byte   "High"
        .byte   GOTOX
        .word   $9F
        .byte   "Draft"
        .byte   GOTOX
        .word   $D7
        .byte   "NLQ"
        .byte	0

txt_from_to_page:
        .byte   "From Page"
        .byte   GOTOX
        .word   $AA
        .byte   "To Page"
        .byte	0

txt_single_tractor:
        .byte   "Single Sheet"
        .byte   GOTOX
        .word   $BA
        .byte   "Tractor Feed"
        .byte	0

txt_insert_next_sheet:
        .byte BOLDON
        .byte	"Please insert the next sheet"
        .byte	0

txt_to_continue_printing:
        .byte   "of paper to continue printing"
        .byte	0

txt_date_token:
	.byte   "DATE"
txt_date_token_len = * - txt_date_token
	.byte	0

txt_time_token:
	.byte   "TIME"
txt_time_token_len = * - txt_time_token
	.byte	0

txt_page_token:
	.byte   "PAGE"
txt_page_token_len = * - txt_page_token
	.byte	0

txt_jan:	.byte   "January",0
txt_feb:        .byte   "February",0
txt_mar:        .byte   "March",0
txt_apr:        .byte   "April",0
txt_may:        .byte   "May",0
txt_jun:        .byte   "June",0
txt_jul:        .byte   "July",0
txt_aug:        .byte   "August",0
txt_sep:        .byte   "September",0
txt_oct:        .byte   "October",0
txt_nov:        .byte   "November",0
txt_dec:        .byte   "December",0
