; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  08 print settings
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.import appInit
	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------


; from #7
.import L375E
.import byteToDecimal
.import stringToInt
.import L3F7C
.import L4184
.import L4185
.import L4187
.import L4188
.import L4189
.import L418A
.import L418C
.import L41CD
.import L41CE

; from #7
.import txt_single_tractor
.import txt_from_to_page
.import txt_high_draft_nlq
.import txt_print

; for #7
.if CHAR_ENCODING=CHAR_ENCODING_DE
.export convertToCp437
.endif
.export showPrintSettings

.import loadR0FnBuffer

.segment        "CODE8": absolute

        lda     r15H                            ; 0680 A5 21                    .!
        cmp     pageEndPtr2+1                        ; 0682 C5 D1                    ..
        bne     L068A                           ; 0684 D0 04                    ..
        lda     r15L                            ; 0686 A5 20                    .
        cmp     pageEndPtr2                        ; 0688 C5 D0                    ..
L068A:  beq     L06A7                           ; 068A F0 1B                    ..
        jsr     L075F                           ; 068C 20 5F 07                  _.
        jsr     measureLine                           ; 068F 20 7A 10                  z.
        jsr     justifyText                           ; 0692 20 1B 17                  ..
        jsr     L07C4                           ; 0695 20 C4 07                  ..
        jsr     L06AF                           ; 0698 20 AF 06                  ..
        jsr     L07B5                           ; 069B 20 B5 07                  ..
        lda     cursor3+cursor::ptr+1                        ; 069E A5 9F                    ..
        sta     r15H                            ; 06A0 85 21                    .!
        lda     cursor3+cursor::ptr                        ; 06A2 A5 9E                    ..
        sta     r15L                            ; 06A4 85 20                    .
        rts                                     ; 06A6 60                       `

; ----------------------------------------------------------------------------
L06A7:  lda     #$01                            ; 06A7 A9 01                    ..
        sta     sysDBData                       ; 06A9 8D 1D 85                 ...
        jmp     RstrFrmDialogue                 ; 06AC 4C BF C2                 L..

; ----------------------------------------------------------------------------
L06AF:  lda     #$00                            ; 06AF A9 00                    ..
        sta     L418C                           ; 06B1 8D 8C 41                 ..A
        lda     r11L                            ; 06B4 A5 18                    ..
        clc                                     ; 06B6 18                       .
        adc     L4189                           ; 06B7 6D 89 41                 m.A
        sta     r10L                            ; 06BA 85 16                    ..
        lda     r11H                            ; 06BC A5 19                    ..
        adc     L418A                           ; 06BE 6D 8A 41                 m.A
        sta     r10H                            ; 06C1 85 17                    ..
        jsr     L0743                           ; 06C3 20 43 07                  C.
L06C6:  jsr     L14BD                           ; 06C6 20 BD 14                  ..
        bcs     L0728                           ; 06C9 B0 5D                    .]
        tax                                     ; 06CB AA                       .
        beq     L06E2                           ; 06CC F0 14                    ..
.if CHAR_ENCODING=CHAR_ENCODING_DE
	jsr     convertToCp437
.endif
        ldy     L418C                           ; 06CE AC 8C 41                 ..A
        sta     MEM_PRINTDATA,y                         ; 06D1 99 80 76                 ..v
        inc     L418C                           ; 06D4 EE 8C 41                 ..A
        lda     r2H                             ; 06D7 A5 07                    ..
        sta     r11H                            ; 06D9 85 19                    ..
        lda     r2L                             ; 06DB A5 06                    ..
        sta     r11L                            ; 06DD 85 18                    ..
        bra     L06C6                           ; 06E0 50 E4                    P.

L06E2:  lda     r1L                             ; 06E2 A5 04                    ..
        cmp     #$09                            ; 06E4 C9 09                    ..
        beq     L071A                           ; 06E6 F0 32                    .2
        lda     r10H                            ; 06E8 A5 17                    ..
        cmp     #$00                            ; 06EA C9 00                    ..
        bne     L06F2                           ; 06EC D0 04                    ..
        lda     r10L                            ; 06EE A5 16                    ..
        cmp     #$08                            ; 06F0 C9 08                    ..
L06F2:  bcc     L071D                           ; 06F2 90 29                    .)
        sec                                     ; 06F4 38                       8
        lda     r10L                            ; 06F5 A5 16                    ..
        sbc     #$08                            ; 06F7 E9 08                    ..
        sta     r10L                            ; 06F9 85 16                    ..
        lda     r10H                            ; 06FB A5 17                    ..
        sbc     #$00                            ; 06FD E9 00                    ..
        sta     r10H                            ; 06FF 85 17                    ..
        asl     r10L                            ; 0701 06 16                    ..
        rol     r10H                            ; 0703 26 17                    &.
        asl     r10L                            ; 0705 06 16                    ..
        rol     r10H                            ; 0707 26 17                    &.
        asl     r10L                            ; 0709 06 16                    ..
        rol     r10H                            ; 070B 26 17                    &.
        lda     r10L                            ; 070D A5 16                    ..
        clc                                     ; 070F 18                       .
        adc     #$08                            ; 0710 69 08                    i.
        sta     r10L                            ; 0712 85 16                    ..
        lda     r10H                            ; 0714 A5 17                    ..
        adc     #$00                            ; 0716 69 00                    i.
        sta     r10H                            ; 0718 85 17                    ..
L071A:  jsr     L0743                           ; 071A 20 43 07                  C.
L071D:  lda     r2H                             ; 071D A5 07                    ..
        sta     r11H                            ; 071F 85 19                    ..
        lda     r2L                             ; 0721 A5 06                    ..
        sta     r11L                            ; 0723 85 18                    ..
        bra     L06C6                           ; 0726 50 9E                    P.

L0728:  clc                                     ; 0728 18                       .
        lda     #$0C                            ; 0729 A9 0C                    ..
        adc     pageTextHeight                             ; 072B 65 B3                    e.
        sta     pageTextHeight                             ; 072D 85 B3                    ..
        bcc     L0733                           ; 072F 90 02                    ..
        inc     pageTextHeight+1                             ; 0731 E6 B4                    ..
L0733:  ldy     L418C                           ; 0733 AC 8C 41                 ..A
        lda     #CR                            ; 0736 A9 0D                    ..
        sta     MEM_PRINTDATA,y                         ; 0738 99 80 76                 ..v
        lda     #0                            ; 073B A9 00                    ..
        sta     MEM_PRINTDATA+1,y                         ; 073D 99 81 76                 ..v
        jmp     L3F7C                           ; 0740 4C 7C 3F                 L|?

; ----------------------------------------------------------------------------
L0743:  ldy     #$03                            ; 0743 A0 03                    ..
        ldx     #$16                            ; 0745 A2 16                    ..
        jsr     DShiftRight                     ; 0747 20 62 C2                  b.
L074A:  lda     r10L                            ; 074A A5 16                    ..
        beq     L075E                           ; 074C F0 10                    ..
        ldy     L418C                           ; 074E AC 8C 41                 ..A
        lda     #' '                            ; 0751 A9 20                    .
        sta     MEM_PRINTDATA,y                         ; 0753 99 80 76                 ..v
        inc     L418C                           ; 0756 EE 8C 41                 ..A
        dec     r10L                            ; 0759 C6 16                    ..
        bra     L074A                           ; 075C 50 EC                    P.

L075E:  rts                                     ; 075E 60                       `

; ----------------------------------------------------------------------------
L075F:  lda     pageTextHeight+1                             ; 075F A5 B4                    ..
        sta     r14H                            ; 0761 85 1F                    ..
        lda     pageTextHeight                             ; 0763 A5 B3                    ..
        sta     r14L                            ; 0765 85 1E                    ..
        ldy     #$00                            ; 0767 A0 00                    ..
        lda     (r15),y                        ; 0769 B1 20                    .
        cmp     #$14                            ; 076B C9 14                    ..
        bne     L07B4                           ; 076D D0 45                    .E
        iny                                     ; 076F C8                       .
        iny                                     ; 0770 C8                       .
        lda     (r15),y                        ; 0771 B1 20                    .
        sta     pageTextHeight                             ; 0773 85 B3                    ..
        iny                                     ; 0775 C8                       .
        lda     (r15),y                        ; 0776 B1 20                    .
        sta     pageTextHeight+1                             ; 0778 85 B4                    ..
        clc                                     ; 077A 18                       .
        lda     #$04                            ; 077B A9 04                    ..
        adc     r15L                            ; 077D 65 20                    e
        sta     r15L                            ; 077F 85 20                    .
        bcc     L0785                           ; 0781 90 02                    ..
        inc     r15H                            ; 0783 E6 21                    .!
L0785:  jsr     L375E                           ; 0785 20 5E 37                  ^7
        lda     pageTextHeight                             ; 0788 A5 B3                    ..
        sub     r14L                            ; 078B E5 1E                    ..
        sta     r14L                            ; 078D 85 1E                    ..
        lda     pageTextHeight+1                             ; 078F A5 B4                    ..
        sbc     r14H                            ; 0791 E5 1F                    ..
        sta     r14H                            ; 0793 85 1F                    ..
        bcc     L07B4                           ; 0795 90 1D                    ..
L0797:  lda     r14L                            ; 0797 A5 1E                    ..
        ora     r14H                            ; 0799 05 1F                    ..
        beq     L07B4                           ; 079B F0 17                    ..
        lda     #$00                            ; 079D A9 00                    ..
        sta     L418C                           ; 079F 8D 8C 41                 ..A
        jsr     L0728                           ; 07A2 20 28 07                  (.
        sec                                     ; 07A5 38                       8
        lda     r14L                            ; 07A6 A5 1E                    ..
        sbc     #$0C                            ; 07A8 E9 0C                    ..
        sta     r14L                            ; 07AA 85 1E                    ..
        lda     r14H                            ; 07AC A5 1F                    ..
        sbc     #$00                            ; 07AE E9 00                    ..
        sta     r14H                            ; 07B0 85 1F                    ..
        bcs     L0797                           ; 07B2 B0 E3                    ..
L07B4:  rts                                     ; 07B4 60                       `

; ----------------------------------------------------------------------------
L07B5:  lda     cursor3+cursor::height                        ; 07B5 A5 A4                    ..
        cmp     #$0D                            ; 07B7 C9 0D                    ..
        bcc     L07C3                           ; 07B9 90 08                    ..
        lda     #$00                            ; 07BB A9 00                    ..
        sta     L418C                           ; 07BD 8D 8C 41                 ..A
        jsr     L0728                           ; 07C0 20 28 07                  (.
L07C3:  rts                                     ; 07C3 60                       `

; ----------------------------------------------------------------------------
L07C4:  lda     r1H                             ; 07C4 A5 05                    ..
        pha                                     ; 07C6 48                       H
        lda     #$00                            ; 07C7 A9 00                    ..
        sta     zp_BDw                        ; 07C9 85 BD                    ..
        sta     zp_BDw+1                        ; 07CB 85 BE                    ..
        lda     rulerData1+ruler::justification                           ; 07CD AD 84 2F                 ../
        and     #$03                            ; 07D0 29 03                    ).
        cmp     #$03                            ; 07D2 C9 03                    ..
        bne     L07FE                           ; 07D4 D0 28                    .(
        lda     tmpLineWidth                             ; 07D6 A5 AC                    ..
        beq     L07FE                           ; 07D8 F0 24                    .$
        sta     r2L                             ; 07DA 85 06                    ..
        lda     zp_AAw+1                             ; 07DC A5 AB                    ..
        sta     r1H                             ; 07DE 85 05                    ..
        lda     zp_AAw                             ; 07E0 A5 AA                    ..
        sta     r1L                             ; 07E2 85 04                    ..
        ldy     #$03                            ; 07E4 A0 03                    ..
        ldx     #$04                            ; 07E6 A2 04                    ..
        jsr     DShiftRight                     ; 07E8 20 62 C2                  b.
        ldx     #$04                            ; 07EB A2 04                    ..
        ldy     #$06                            ; 07ED A0 06                    ..
        jsr     Ddiv                            ; 07EF 20 69 C1                  i.
        lda     r1H                             ; 07F2 A5 05                    ..
        sta     zp_BDw+1                        ; 07F4 85 BE                    ..
        lda     r1L                             ; 07F6 A5 04                    ..
        sta     zp_BDw                        ; 07F8 85 BD                    ..
        lda     r8L                             ; 07FA A5 12                    ..
        sta     zp_BDw+1                        ; 07FC 85 BE                    ..
L07FE:  pla                                     ; 07FE 68                       h
        sta     r1H                             ; 07FF 85 05                    ..
        rts                                     ; 0801 60                       `

; ----------------------------------------------------------------------------

.if CHAR_ENCODING=CHAR_ENCODING_DE

convertToCp437:
	ldy     #8
@loop:  cmp     X0814-1,y
        beq     @found
        dey
        bne     @loop
        rts
@found:	lda     $081C-1,y
 	rts

X0814:	.byte '@','[','\',']','{','|','}','~'	; source: GEOS_de
L081C:  .byte $EB,$8E,$99,$9A,$84,$94,$81,$E1	; target: CP437
;             'δ','Ä','Ö','Ü','ä','ö','ü','ß'

.endif

; ----------------------------------------------------------------------------

showPrintSettings:
	LoadW   r0, dlgbox_print_settings
        jsr     DoDlgBox                        ; 080A 20 56 C2                  V.
        lda     r0L                             ; 080D A5 02                    ..
        rts                                     ; 080F 60                       `

; ----------------------------------------------------------------------------
L0810:  LoadW appMain, L081B                            ; 0815 A9 1B                    ..
        rts                                     ; 081A 60                       `

; ----------------------------------------------------------------------------
L081B:	lda     #0                            ; 081B A9 00                    ..
        sta     appMain                         ; 081D 8D 9B 84                 ...
        sta     appMain+1                           ; 0820 8D 9C 84                 ...
        sta     iconSelFlg		; disable icon flashing
        sta     L4187                           ; 0826 8D 87 41                 ..A
        sta     zp_BAb                       ; 0829 85 BA                    ..
        jsr     L0884                           ; 082B 20 84 08                  ..
        lda     a9H                             ; 082E A5 7F                    ..
        sta     L4188                           ; 0830 8D 88 41                 ..A
        jsr     i_BitmapUp                      ; 0833 20 AB C1                  ..
.if LANG=LANG_DE
		.byte   $38
.else
		.byte   $16
.endif
		.byte	$0B
		.byte	$18
		.byte	$3C
		.byte	$02
		.byte	$10
		.byte	$20
.if LANG=LANG_DE
		.byte   $C1
.else
		.byte	$9F
.endif
		.byte   $08                             ; 083E 08                       .
        jsr     L0930                           ; 083F 20 30 09                  0.
        jsr     L093B                           ; 0842 20 3B 09                  ;.
        lda     mouseVector+1                   ; 0845 AD A2 84                 ...
        sta     L41CE                           ; 0848 8D CE 41                 ..A
        lda     mouseVector                     ; 084B AD A1 84                 ...
        sta     L41CD                           ; 084E 8D CD 41                 ..A
	LoadW	mouseVector, X085C
        rts                                     ; 085B 60                       `

; ----------------------------------------------------------------------------
X085C:	lda     zp_BAb                       ; 085C A5 BA                    ..
        beq     X0881                           ; 085E F0 21                    .!
        lda     #$71                            ; 0860 A9 71                    .q
        sta     r2L                             ; 0862 85 06                    ..
        lda     #$80                            ; 0864 A9 80                    ..
        sta     r2H                             ; 0866 85 07                    ..
        lda     #$00                            ; 0868 A9 00                    ..
        sta     r3H                             ; 086A 85 09                    ..
        lda     #$58                            ; 086C A9 58                    .X
        sta     r3L                             ; 086E 85 08                    ..
        lda     #$00                            ; 0870 A9 00                    ..
        sta     r4H                             ; 0872 85 0B                    ..
        lda     #$87                            ; 0874 A9 87                    ..
        sta     r4L                             ; 0876 85 0A                    ..
        jsr     IsMseInRegion                   ; 0878 20 B3 C2                  ..
        tax                                     ; 087B AA                       .
        bpl     X0881                           ; 087C 10 03                    ..
        jsr     simulateCrPress                           ; 087E 20 E3 09                  ..
X0881:  jmp     (L41CD)                         ; 0881 6C CD 41                 l.A

; ----------------------------------------------------------------------------
L0884:  lda     #$01                            ; 0884 A9 01                    ..
L0886:  cmp     #PAGE_HEADER                            ; 0886 C9 3D                    .=
        bcs     L0899                           ; 0888 B0 0F                    ..
        pha                                     ; 088A 48                       H
        jsr     PointRecord                     ; 088B 20 80 C2                  ..
        pla                                     ; 088E 68                       h
        cpy     #$00                            ; 088F C0 00                    ..
        beq     L0899                           ; 0891 F0 06                    ..
        clc                                     ; 0893 18                       .
        adc     #$01                            ; 0894 69 01                    i.
        bra     L0886                           ; 0897 50 ED                    P.

L0899:  sub     #$01                            ; 089A E9 01                    ..
        sta     a9H                             ; 089C 85 7F                    ..
        rts                                     ; 089E 60                       `

; ----------------------------------------------------------------------------
L089F:  lda     #$00                            ; 089F A9 00                    ..
        sta     L4184                           ; 08A1 8D 84 41                 ..A
        tax                                     ; 08A4 AA                       .
        ldy     #$02                            ; 08A5 A0 02                    ..
L08A7:  pha                                     ; 08A7 48                       H
        txa                                     ; 08A8 8A                       .
        pha                                     ; 08A9 48                       H
        tya                                     ; 08AA 98                       .
        jsr     SetPattern                      ; 08AB 20 39 C1                  9.
        jsr     i_Rectangle                     ; 08AE 20 9F C1                  ..
		.byte   $3D,$4A                         ; 08B1 3D 4A                    =J
		.word   $0051,$005E                     ; 08B3 51 00 5E 00              Q.^.
        pla                                     ; 08B7 68                       h
        jsr     SetPattern                      ; 08B8 20 39 C1                  9.
        jsr     i_Rectangle                     ; 08BB 20 9F C1                  ..
		.byte   $3D,$4A                         ; 08BE 3D 4A                    =J
		.word   $00C1,$00CE                     ; 08C0 C1 00 CE 00              ....
        pla                                     ; 08C4 68                       h
        jsr     SetPattern                      ; 08C5 20 39 C1                  9.
        jsr     i_Rectangle                     ; 08C8 20 9F C1                  ..
		.byte   $3D,$4A                         ; 08CB 3D 4A                    =J
		.word   $0089,$0096                     ; 08CD 89 00 96 00              ....
        rts                                     ; 08D1 60                       `

; ----------------------------------------------------------------------------
L08D2:  lda     #$01                            ; 08D2 A9 01                    ..
        sta     L4184                           ; 08D4 8D 84 41                 ..A
        lda     #$02                            ; 08D7 A9 02                    ..
        ldy     #$00                            ; 08D9 A0 00                    ..
        ldx     #$00                            ; 08DB A2 00                    ..
        beq     L08A7                           ; 08DD F0 C8                    ..
L08DF:  lda     #$FF                            ; 08DF A9 FF                    ..
        sta     L4184                           ; 08E1 8D 84 41                 ..A
        lda     #$00                            ; 08E4 A9 00                    ..
        tay                                     ; 08E6 A8                       .
        ldx     #$02                            ; 08E7 A2 02                    ..
        bne     L08A7                           ; 08E9 D0 BC                    ..
L08EB:  lda     #$3C                            ; 08EB A9 3C                    .<
        sta     r2L                             ; 08ED 85 06                    ..
        lda     #$4B                            ; 08EF A9 4B                    .K
        sta     r2H                             ; 08F1 85 07                    ..
        lda     #$00                            ; 08F3 A9 00                    ..
        sta     r3H                             ; 08F5 85 09                    ..
        lda     #$C0                            ; 08F7 A9 C0                    ..
        sta     r3L                             ; 08F9 85 08                    ..
        lda     #$00                            ; 08FB A9 00                    ..
        sta     r4H                             ; 08FD 85 0B                    ..
        lda     #$CF                            ; 08FF A9 CF                    ..
        sta     r4L                             ; 0901 85 0A                    ..
        jsr     IsMseInRegion                   ; 0903 20 B3 C2                  ..
        tax                                     ; 0906 AA                       .
        beq     L090C                           ; 0907 F0 03                    ..
        jsr     L08DF                           ; 0909 20 DF 08                  ..
L090C:  rts                                     ; 090C 60                       `

; ----------------------------------------------------------------------------
L090D:  lda     #$00                            ; 090D A9 00                    ..
        sta     L4185                           ; 090F 8D 85 41                 ..A
        ldy     #$02                            ; 0912 A0 02                    ..
L0914:  pha                                     ; 0914 48                       H
        tya                                     ; 0915 98                       .
        jsr     SetPattern                      ; 0916 20 39 C1                  9.
        jsr     i_Rectangle                     ; 0919 20 9F C1                  ..
		.byte   $5F,$6C                         ; 091C 5F 6C                    _l
		.word   $0051,$005E                     ; 091E 51 00 5E 00              Q.^.
        pla                                     ; 0922 68                       h
        jsr     SetPattern                      ; 0923 20 39 C1                  9.
        jsr     i_Rectangle                     ; 0926 20 9F C1                  ..
		.byte   $5F,$6C                         ; 0929 5F 6C                    _l
		.word   $00A9,$00B6                     ; 092B A9 00 B6 00              ....
        rts                                     ; 092F 60                       `

; ----------------------------------------------------------------------------
L0930:  lda     #$01                            ; 0930 A9 01                    ..
        sta     L4185                           ; 0932 8D 85 41                 ..A
        lda     #$02                            ; 0935 A9 02                    ..
        ldy     #$00                            ; 0937 A0 00                    ..
        beq     L0914                           ; 0939 F0 D9                    ..
L093B:  lda     dispBufferOn                    ; 093B A5 2F                    ./
        beq     L0983                           ; 093D F0 44                    .D
        jsr     L0984                           ; 093F 20 84 09                  ..
        jsr     L098E                           ; 0942 20 8E 09                  ..
        lda     privFHData+privfhdata::startPageNo                        ; 0945 A5 E2                    ..
        clc                                     ; 0947 18                       .
        adc     L4187                           ; 0948 6D 87 41                 m.A
        sta     r0L                             ; 094B 85 02                    ..
        lda     privFHData+privfhdata::startPageNo+1                        ; 094D A5 E3                    ..
        adc     #$00                            ; 094F 69 00                    i.
        sta     r0H                             ; 0951 85 03                    ..
        lda     #$57                            ; 0953 A9 57                    .W
        sta     r1H                             ; 0955 85 05                    ..
        lda     #$00                            ; 0957 A9 00                    ..
        sta     r11H                            ; 0959 85 19                    ..
        lda     #$8E                            ; 095B A9 8E                    ..
        sta     r11L                            ; 095D 85 18                    ..
        lda     #$CF                            ; 095F A9 CF                    ..
        jsr     PutDecimal                      ; 0961 20 84 C1                  ..
        lda     privFHData+privfhdata::startPageNo                        ; 0964 A5 E2                    ..
        clc                                     ; 0966 18                       .
        adc     L4188                           ; 0967 6D 88 41                 m.A
        sta     r0L                             ; 096A 85 02                    ..
        lda     privFHData+privfhdata::startPageNo+1                        ; 096C A5 E3                    ..
        adc     #$00                            ; 096E 69 00                    i.
        sta     r0H                             ; 0970 85 03                    ..
        lda     #$57                            ; 0972 A9 57                    .W
        sta     r1H                             ; 0974 85 05                    ..
        lda     #$00                            ; 0976 A9 00                    ..
        sta     r11H                            ; 0978 85 19                    ..
        lda     #$DE                            ; 097A A9 DE                    ..
        sta     r11L                            ; 097C 85 18                    ..
        lda     #$CF                            ; 097E A9 CF                    ..
        jsr     PutDecimal                      ; 0980 20 84 C1                  ..
L0983:  rts                                     ; 0983 60                       `

; ----------------------------------------------------------------------------
L0984:  jsr     i_BitmapUp                      ; 0984 20 AB C1                  ..
.if LANG=LANG_DE
		.byte   $41
.else
		.byte   $1F
.endif
		.byte	$0B
		.byte	$11
		.byte	$4D
		.byte	$04
		.byte	$10
        rts                                     ; 098D 60                       `

; ----------------------------------------------------------------------------
L098E:  jsr     i_BitmapUp                      ; 098E 20 AB C1                  ..
.if LANG=LANG_DE
		.byte   $41
.else
		.byte   $1F
.endif
		.byte	$0B
		.byte	$1B
		.byte	$4D
		.byte	$04
		.byte	$10
        rts                                     ; 0997 60                       `

; ----------------------------------------------------------------------------
L0998:  lda     zp_BAb                       ; 0998 A5 BA                    ..
        beq     L09A1                           ; 099A F0 05                    ..
        bpl     L09E2                           ; 099C 10 44                    .D
        jsr     simulateCrPress                           ; 099E 20 E3 09                  ..
L09A1:  jsr     L0984                           ; 09A1 20 84 09                  ..
        lda     #$01                            ; 09A4 A9 01                    ..
        sta     zp_BAb                       ; 09A6 85 BA                    ..
	LoadW   keyVector, printKeyVector1
        jsr     loadR0FnBuffer                           ; 09B2 20 49 27                  I'
        lda     privFHData+privfhdata::startPageNo                        ; 09B5 A5 E2                    ..
        clc                                     ; 09B7 18                       .
        adc     L4187                           ; 09B8 6D 87 41                 m.A
        sta     r3L                             ; 09BB 85 08                    ..
        lda     privFHData+privfhdata::startPageNo+1                        ; 09BD A5 E3                    ..
        adc     #$00                            ; 09BF 69 00                    i.
        sta     r3H                             ; 09C1 85 09                    ..
        lda     #$00                            ; 09C3 A9 00                    ..
        jsr     byteToDecimal                           ; 09C5 20 CD 3C                  .<
        jsr     loadR0FnBuffer                           ; 09C8 20 49 27                  I'
        lda     #$00                            ; 09CB A9 00                    ..
        sta     r1L                             ; 09CD 85 04                    ..
        lda     #$51                            ; 09CF A9 51                    .Q
        sta     r1H                             ; 09D1 85 05                    ..
        lda     #$03                            ; 09D3 A9 03                    ..
        sta     r2L                             ; 09D5 85 06                    ..
        lda     #$00                            ; 09D7 A9 00                    ..
        sta     r11H                            ; 09D9 85 19                    ..
        lda     #$8E                            ; 09DB A9 8E                    ..
        sta     r11L                            ; 09DD 85 18                    ..
        jsr     GetString                       ; 09DF 20 BA C1                  ..
L09E2:  rts                                     ; 09E2 60                       `

; ----------------------------------------------------------------------------
simulateCrPress:			; [see HHG p.118]
	sei
        lda     #CR
        sta     keyData
        jsr     callKeyVector
        cli
        rts

; ----------------------------------------------------------------------------
callKeyVector:
	jmp     (keyVector)                     ; 09EE 6C A3 84                 l..

; ----------------------------------------------------------------------------
printKeyVector1:
	jsr     L0A4D                           ; 09F1 20 4D 0A                  M.
L09F4:  jsr     L093B                           ; 09F4 20 3B 09                  ;.
        LoadB   zp_BAb, 0
        rts                                     ; 09FB 60                       `

; ----------------------------------------------------------------------------
L09FC:  lda     zp_BAb                       ; 09FC A5 BA                    ..
        beq     L0A05                           ; 09FE F0 05                    ..
        bmi     L0A46                           ; 0A00 30 44                    0D
        jsr     simulateCrPress                           ; 0A02 20 E3 09                  ..
L0A05:  jsr     L098E                           ; 0A05 20 8E 09                  ..
        lda     #$FF                            ; 0A08 A9 FF                    ..
        sta     zp_BAb                       ; 0A0A 85 BA                    ..
	LoadW   keyVector, printKeyVector2
        jsr     loadR0FnBuffer                           ; 0A16 20 49 27                  I'
        lda     privFHData+privfhdata::startPageNo                        ; 0A19 A5 E2                    ..
        clc                                     ; 0A1B 18                       .
        adc     L4188                           ; 0A1C 6D 88 41                 m.A
        sta     r3L                             ; 0A1F 85 08                    ..
        lda     privFHData+privfhdata::startPageNo+1                        ; 0A21 A5 E3                    ..
        adc     #0                            ; 0A23 69 00                    i.
        sta     r3H                             ; 0A25 85 09                    ..
        lda     #0                            ; 0A27 A9 00                    ..
        jsr     byteToDecimal                           ; 0A29 20 CD 3C                  .<
        jsr     loadR0FnBuffer                           ; 0A2C 20 49 27                  I'
	LoadB	r1L, 0
	LoadB	r1H, 81
        LoadB   r2L, 3
	LoadW	r11, $DE
        jsr     GetString                       ; 0A43 20 BA C1                  ..
L0A46:  rts                                     ; 0A46 60                       `

; ----------------------------------------------------------------------------
printKeyVector2:
	jsr     L0A70                           ; 0A47 20 70 0A                  p.
        jmp     L09F4                           ; 0A4A 4C F4 09                 L..

; ----------------------------------------------------------------------------
L0A4D:  jsr     stringToInt                           ; 0A4D 20 1F 3D                  .=
        bcs     L0A6F                           ; 0A50 B0 1D                    ..
        lda     r1L                             ; 0A52 A5 04                    ..
        sub     privFHData+privfhdata::startPageNo                        ; 0A55 E5 E2                    ..
        sta     r1L                             ; 0A57 85 04                    ..
        lda     r1H                             ; 0A59 A5 05                    ..
        sbc     privFHData+privfhdata::startPageNo+1                        ; 0A5B E5 E3                    ..
        sta     r1H                             ; 0A5D 85 05                    ..
        bcc     L0A6F                           ; 0A5F 90 0E                    ..
        jsr     L0A93                           ; 0A61 20 93 0A                  ..
        sta     L4187                           ; 0A64 8D 87 41                 ..A
        cmp     L4188                           ; 0A67 CD 88 41                 ..A
        bcc     L0A6F                           ; 0A6A 90 03                    ..
        sta     L4188                           ; 0A6C 8D 88 41                 ..A
L0A6F:  rts                                     ; 0A6F 60                       `

; ----------------------------------------------------------------------------
L0A70:  jsr     stringToInt                           ; 0A70 20 1F 3D                  .=
        bcs     L0A92                           ; 0A73 B0 1D                    ..
        lda     r1L                             ; 0A75 A5 04                    ..
        sub     privFHData+privfhdata::startPageNo                        ; 0A78 E5 E2                    ..
        sta     r1L                             ; 0A7A 85 04                    ..
        lda     r1H                             ; 0A7C A5 05                    ..
        sbc     privFHData+privfhdata::startPageNo+1                        ; 0A7E E5 E3                    ..
        sta     r1H                             ; 0A80 85 05                    ..
        bcc     L0A92                           ; 0A82 90 0E                    ..
        jsr     L0A93                           ; 0A84 20 93 0A                  ..
        sta     L4188                           ; 0A87 8D 88 41                 ..A
        cmp     L4187                           ; 0A8A CD 87 41                 ..A
        bcs     L0A92                           ; 0A8D B0 03                    ..
        sta     L4187                           ; 0A8F 8D 87 41                 ..A
L0A92:  rts                                     ; 0A92 60                       `

; ----------------------------------------------------------------------------
L0A93:  lda     r1H                             ; 0A93 A5 05                    ..
        bne     L0A9D                           ; 0A95 D0 06                    ..
        lda     r1L                             ; 0A97 A5 04                    ..
        cmp     a9H                             ; 0A99 C5 7F                    ..
        bcc     L0A9F                           ; 0A9B 90 02                    ..
L0A9D:  lda     a9H                             ; 0A9D A5 7F                    ..
L0A9F:  rts                                     ; 0A9F 60                       `

; ----------------------------------------------------------------------------
dlgbox_print_settings:
	.byte   SET_DB_POS | 1
        .byte   $28
        .byte   $87
        .word   $40
        .word   $FF
        .byte   DBTXTSTR
        .byte   $50
        .byte   $0D
        .word   txt_print
        .byte   DBTXTSTR
        .byte   $28
        .byte   $1E
        .word   txt_high_draft_nlq
        .byte   DBTXTSTR
        .byte   $0D
        .byte   $2F
        .word   txt_from_to_page
        .byte   DBTXTSTR
        .byte   $23
        .byte   $40
        .word   txt_single_tractor
        .byte   DBUSRICON
        .byte   $02
        .byte   $14
        .word   usricon1
        .byte   DBUSRICON
        .byte   $09
        .byte   $14
        .word   usricon2
        .byte   DBUSRICON
        .byte   $09
        .byte   $25
        .word   usricon3
        .byte   DBUSRICON
        .byte   $13
        .byte   $25
        .word   usricon4
        .byte   DBUSRICON
        .byte   $02
        .byte   $36
        .word   usricon5
        .byte   DBUSRICON
        .byte   $0D
        .byte   $36
        .word   usricon6
        .byte   OK
        .byte   $03
        .byte   $49
        .byte   CANCEL
        .byte   $0F
        .byte   $49
        .byte   DBOPVEC
        .word   L08EB
        .byte   DB_USR_ROUT
        .word   L0810
        .byte   NULL

usricon1:
	.word   graph_rect16x16
	.byte   0
	.byte   0
	.byte   2
	.byte   16
	.word   L089F

usricon2:
	.word   graph_rect16x16
	.byte   0
	.byte   0
	.byte   2
	.byte   16
	.word   L08D2

usricon3:
	.word   graph_rect32x16
	.byte   0
	.byte   0
	.byte   4
	.byte   16
	.word   L0998

usricon4:
	.word   graph_rect32x16
	.byte   0
	.byte   0
	.byte   4
	.byte   16
	.word   L09FC

usricon5:
	.word   graph_rect16x16
	.byte   0
	.byte   0
	.byte   2
	.byte   16
	.word   L090D

usricon6:
	.word   graph_rect16x16
	.byte   0
	.byte   0
	.byte   2
	.byte   16
	.word   L0930

graph_rect16x16:
	.byte   2,%11111111
	.byte	$DC+3,14
		.byte	$80+2,%10000000,%00000001
	.byte	2,%11111111

graph_rect32x16:
	.byte   4,%11111111
	.byte	$DC+5,14
		.byte	$80+4,%10000000,%00000000,%00000000,%00000001
        .byte   4,%11111111
