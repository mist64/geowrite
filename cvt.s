
; t&s (record block, info block) and timestamp matching
; geoWrite_en_2.1_1988-07-06

.SEGMENT "DIRENTRY"

        .byte 131
        .byte $06, $0b		; t&s of record block, irrelevant in CVT
        .byte "GEOWRITE"
        .res  (16 - 8), $a0
        .byte $06, $03		; t&s of info block, irrelevant in CVT
        .byte 1 ; VLIR structure
        .byte 6 ; APPLICATION
        .byte 88, 7, 6, 13, 16

        .word 141		; size in 254-byte blocks
        .byte "PRG formatted GEOS file"

.segment "FILEINFO"
.incbin "build/current/geoWrite-fhdr.bin", 2

.segment "RECORDS"

.import __VLIR0_START__, __VLIR0_LAST__
.import __VLIR1_START__, __VLIR1_LAST__
.import __VLIR2_START__, __VLIR2_LAST__
.import __VLIR3_START__, __VLIR3_LAST__
.import __VLIR4_START__, __VLIR4_LAST__
.import __VLIR5_START__, __VLIR5_LAST__
.import __VLIR6_START__, __VLIR6_LAST__
.import __VLIR7_START__, __VLIR7_LAST__
.import __VLIR8_START__, __VLIR8_LAST__

	.byte .lobyte ((__VLIR0_LAST__ - __VLIR0_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR0_LAST__ - __VLIR0_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR1_LAST__ - __VLIR1_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR1_LAST__ - __VLIR1_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR2_LAST__ - __VLIR2_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR2_LAST__ - __VLIR2_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR3_LAST__ - __VLIR3_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR3_LAST__ - __VLIR3_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR4_LAST__ - __VLIR4_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR4_LAST__ - __VLIR4_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR5_LAST__ - __VLIR5_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR5_LAST__ - __VLIR5_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR6_LAST__ - __VLIR6_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR6_LAST__ - __VLIR6_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR7_LAST__ - __VLIR7_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR7_LAST__ - __VLIR7_START__ - 1) .MOD 254) + 2

	.byte .lobyte ((__VLIR8_LAST__ - __VLIR8_START__ - 1) /    254) + 1
	.byte .lobyte ((__VLIR8_LAST__ - __VLIR8_START__ - 1) .MOD 254) + 2

.segment "VLIR0"
.incbin "build/current/geoWrite-0.bin"

.segment "VLIR1"
.incbin "build/current/geoWrite-1.bin"

.segment "VLIR2"
.incbin "build/current/geoWrite-2.bin"

.segment "VLIR3"
.incbin "build/current/geoWrite-3.bin"

.segment "VLIR4"
.incbin "build/current/geoWrite-4.bin"

.segment "VLIR5"
.incbin "build/current/geoWrite-5.bin"

.segment "VLIR6"
.incbin "build/current/geoWrite-6.bin"

.segment "VLIR7"
.incbin "build/current/geoWrite-7.bin"

.segment "VLIR8"
.incbin "build/current/geoWrite-8.bin"

