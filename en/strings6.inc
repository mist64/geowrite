txt_no_next_page:
        .byte   BOLDON
        .byte	"No next page"
        .byte	0

txt_cannot_go_to_page:
        .byte   BOLDON
        .byte	"Cannot go to page"
        .byte	0

txt_enter_page_to_go_to:
	.byte   BOLDON
        .byte	"Enter page to go to:"
        .byte	PLAINTEXT,0

txt_enter_page_number:
        .byte   BOLDON
        .byte	"Enter page number"
        .byte	0

txt_for_first_page:
        .byte   "for first page:"
        .byte	PLAINTEXT,0

txt_bad_value:
        .byte   BOLDON
        .byte	"Bad value"
        .byte	0

txt_del_last_char:
        .byte   BOLDON
        .byte	"Delete last character"
        .byte	0

txt_of_prev_page:
        .byte   "of the prev page?"
        .byte	0

txt_open:
        .byte   "open",PLAINTEXT
txt_open_len = * - txt_open
txt_close:
        .byte   "close"
.assert * - txt_close = txt_open_len, error; need to be the same size

txt_string_not_found:
        .byte   BOLDON
        .byte	"String not found"
        .byte	0

txt_search_replace:
        .byte   BOLDON
        .byte	"Search / Replace"
        .byte	0

txt_search_for:
        .byte   "Search For"
        .byte	0

txt_replace_with:
        .byte   "Replace With"
        .byte	0

txt_whole_word:
        .byte   "Whole Word"
        .byte   GOTOX
        .word   162
        .byte   "Partial Word"
        .byte	0

txt_all_pages:
        .byte   "All Pages"
        .byte   GOTOX
        .word   162
        .byte   "This Page Only"
        .byte	0
