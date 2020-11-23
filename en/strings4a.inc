txt_showhide:
txt_show:
	.byte   "show"
txt_show_len = * - txt_show
txt_hide:
	.byte   "hide"
.assert * - txt_hide = txt_show_len, error; need to be the same size

txt_photoscrap_too_new:
	.byte   BOLDON
        .byte   "photoScrap is beyond V2.1"
        .byte   0

txt_picture_area_full:
        .byte   BOLDON
        .byte   "File's picture area full."
        .byte   0

txt_no_photoscrap:
	.byte   BOLDON
        .byte   "No photoScrap file on disk."
        .byte   0

txt_photoscrap_too_big:
	.byte   BOLDON
        .byte   "photoScrap too big."
        .byte   0
