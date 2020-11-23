txt_no_printer:
        .byte   BOLDON
.if LANG=LANG_DE
        .byte   "Kein Druckertreiber aktiv."
.else
        .byte   "No printer driver is selected."
.endif
        .byte   GOTOXY
        .word   80
	.byte   64
.if LANG=LANG_DE
        .byte   "Verwenden Sie 'Drucker w{hlen'"
.else
        .byte   "Use 'select printer' on the"
.endif
        .byte   GOTOXY
        .word   80
        .byte   80
.if LANG=LANG_DE
        .byte   "unter deskTop."
.else
        .byte   "deskTop to choose a printer."
.endif
	.byte	0

txt_reading_printer_driver:
.if LANG=LANG_DE
        .byte   "bez}glich Druckertreiber"
.else
	.byte   BOLDON
        .byte   "reading printer driver."
.endif
        .byte	0

txt_cant_find_printer:
.if LANG=LANG_DE
        .byte   "Druckertreiber nicht"
        .byte   GOTOXY
        .byte   80, 0
        .byte   "@zu finden:"
.else
	.byte   "Can't find printer driver named:"
.endif
	.byte	BOLDON,0
