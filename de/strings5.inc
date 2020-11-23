aboutText:
	.byte   BOLDON,"geoWrite von Tony Requist"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Version 2.1 f}r den C64"
        .byte   GOTOXY
        .word   80
        .byte   80
        .byte   "Copyright 1985, 1986, 1987"
        .byte   GOTOXY
        .word   80
        .byte   96
        .byte   "Berkeley Softworks",0

txt_accessory_disk_full:
	.byte   BOLDON
        .byte   "Kein Diskspeicher frei"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "f}r Hilfsmittel."
	.byte	0

txt_please_select_option:
        .byte   BOLDON
        .byte	"Bitte Option ausw{hlen:"
        .byte	PLAINTEXT,0

txt_new_document:
        .byte   "Neues Dokument"
        .byte	0

txt_existing_document:
        .byte   "Existierendes Dokument"
        .byte	0

txt_to_desktop:
        .byte   "Nach deskTop"
        .byte	0

txt_insert_new_disk:
	.byte   BOLDON
        .byte   "Neue Diskette in"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "Laufwerk einlegen."
        .byte	0

txt_file_exists:
	.byte   BOLDON
        .byte   "Dateiname schon"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "vergeben"
        .byte	0

txt_document_version_error:
	.byte   BOLDON
        .byte	"Datenfile von neuerer Version als V2.1"
        .byte	0

txt_converting_file:
        .byte	BOLDON
        .byte	"Konvertiere Datei f}r "
        .byte	"V2."
txt_converting_file_digit:
	.byte   "1"
        .byte   0

txt_converting_error:
	.byte   "bei Dateikonvertierung"
        .byte	0

txt_on_disk:
	.byte   BOLDON
        .byte	"Auf Disk:"
        .byte	PLAINTEXT,0

txt_warning:
	.byte   BOLDON
        .byte	"WARNUNG!"
        .byte	0

txt_file_is_write_protected:
        .byte   "Diese Datei ist schreibgesch}tzt."
        .byte	0

txt_cannot_recover:
	.byte   BOLDON
        .byte   "Datei nicht lesbar"
        .byte	0

txt_enter_filename:
	.byte   BOLDON
        .byte	"Neuer Dateiname:"
        .byte	PLAINTEXT,0

txt_opening_file:
	.byte   "|ffne Datei"
        .byte	0

txt_creating_file:
	.byte   "Datei entsteht"
        .byte	0

txt_running_accessory:
	.byte   "starte Hilfsmittel"
        .byte	0

txt_cannot_run_accessory:
	.byte   BOLDON
        .byte	"Hilfsmittel nicht in Kopf-"
        .byte   GOTOXY
        .word   80
        .byte   64
        .byte   "oder Fu~zeile verf}gbar"
        .byte	0

txt_filename_exists:
	.byte   BOLDON
        .byte	"Dateiname schon vergeben"
        .byte	0
