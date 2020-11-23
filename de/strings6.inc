txt_no_next_page:
        .byte   BOLDON
        .byte	"Letzte Seite"
        .byte	0

txt_cannot_go_to_page:
        .byte   BOLDON
        .byte	"Seite nicht erreichbar"
        .byte	0

txt_enter_page_to_go_to:
	.byte   BOLDON
        .byte	"Wechsel auf Seite:"
        .byte	PLAINTEXT,0

txt_enter_page_number:
        .byte   BOLDON
        .byte	"Seitenzahl f}r"
        .byte	0

txt_for_first_page:
        .byte   "erste Seite"
        .byte	PLAINTEXT,0

txt_bad_value:
        .byte   BOLDON
        .byte	"Falscher Wert"
        .byte	0

txt_del_last_char:
        .byte   BOLDON
        .byte	"Letztes Zeichen der"
        .byte	0

txt_of_prev_page:
        .byte   "vorigen Seite l|schen?"
        .byte	0

txt_open:
        .byte   PLAINTEXT,PLAINTEXT
txt_open_len = * - txt_open
txt_close:
        .byte   "* "
.assert * - txt_close = txt_open_len, error; need to be the same size

txt_string_not_found:
        .byte   BOLDON
        .byte	"nicht gefunden"
        .byte	0

txt_search_replace:
        .byte   BOLDON
        .byte	"Suchen / Ersetzen"
        .byte	0

txt_search_for:
        .byte   "Suche nach"
        .byte	0

txt_replace_with:
        .byte   "Ersetze mit"
        .byte	0

txt_whole_word:
        .byte   "ganzes Wort"
        .byte   GOTOX
        .word   162
        .byte   "Wortteil"
        .byte	0

txt_all_pages:
        .byte   "Alle Seiten"
        .byte   GOTOX
        .word   162
        .byte   "Nur diese Seite"
        .byte	0
