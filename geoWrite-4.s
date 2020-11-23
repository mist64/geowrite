; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  04 ruler editing
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------

.segment        "CODE4": absolute

        jmp     editPageIndicator	; 0
        jmp     L3594			; 1
        jmp     L389C			; 2
        jmp     callbackRuler		; 3
        jmp     callbackMarginBar	; 4

; ----------------------------------------------------------------------------
saveMaxMouseSpeed:
	.byte   $00
saveMinMouseSpeed:
	.byte   $00
L3255:  .word   $FFFF
; ----------------------------------------------------------------------------
; called when user clicks on ruler
callbackRuler:
	MoveW   pageWidth1, r11
        lda     zp_DAb                        ; 3261 A5 DA                    ..
        beq     @1                           ; 3263 F0 04                    ..
        cmp     #20                            ; 3265 C9 14                    ..
        bne     @2                           ; 3267 D0 08                    ..
@1:	LoadW__ r11, 0                           ; 326F 85 18                    ..
@2:	jsr     L35B6                           ; 3271 20 B6 35                  .5
        LoadB   zp_DBb, 0
        MoveW   r11, L2FBE
        ldx     zp_DAb                        ; 3282 A6 DA                    ..
        jsr     L33A5                           ; 3284 20 A5 33                  .3
        jmp     L390E                           ; 3287 4C 0E 39                 L.9

; ----------------------------------------------------------------------------
; called when user clicks on margin bar
callbackMarginBar:
	jsr     L35CD                           ; 328A 20 CD 35                  .5
        bit     zp_DBb                        ; 328D 24 DB                    $.
        bpl     :+                           ; 328F 10 03                    ..
        jmp     L336D                           ; 3291 4C 6D 33                 Lm3
:	LoadB   L2FC0, 0
        PushW   r11                             ; 329E 48                       H
        ldx     #$18                            ; 329F A2 18                    ..
        jsr     addSideFlippingPixelOffset                           ; 32A1 20 E5 26                  .&
        jsr     L35E1                           ; 32A4 20 E1 35                  .5
        PopW    r11                            ; 32AB 85 19                    ..
        bcc     L3309                           ; 32AD 90 5A                    .Z
        stx     zp_DAb                        ; 32AF 86 DA                    ..
        lda     rulerData1+ruler::left_margin,x                         ; 32B1 BD 6E 2F                 .n/
        sta     L3255                           ; 32B4 8D 55 32                 .U2
        lda     rulerData1+ruler::left_margin+1,x                         ; 32B7 BD 6F 2F                 .o/
        sta     L3255+1                           ; 32BA 8D 56 32                 .V2
        and     #$80                            ; 32BD 29 80                    ).
        sta     L2FC0                           ; 32BF 8D C0 2F                 ../
        LoadB   zp_DBb, $FF

        LoadW   r0, rulerM                            ; 32CA A9 3C                    .<
        cpx     #$04                            ; 32CE E0 04                    ..
        bcc     L32F3                           ; 32D0 90 21                    .!

        LoadW   r0, rulerTab                            ; 32D6 A9 46                    .F
        lda     rulerData1+ruler::left_margin+1,x                         ; 32DA BD 6F 2F                 .o/
        bpl     L32E7                           ; 32DD 10 08                    ..

	LoadW   r0, rulerDTab                            ; 32E3 A9 50                    .P
L32E7:  cpx     #$14                            ; 32E7 E0 14                    ..
        bcc     L32F3                           ; 32E9 90 08                    ..

	LoadW   r0, rulerP                            ; 32EF A9 5A                    .Z

L32F3:  jsr     drawMarginBarItem2                           ; 32F3 20 E9 05                  ..
        jsr     initRulerEditing                           ; 32F6 20 3F 35                  ?5
        jsr     clearKeyVectorOtherPressVector                           ; 32F9 20 19 38                  .8
	LoadW   keyVector, rulerKeyVector
        bra     L3348                           ; 3307 50 3F                    P?

L3309:  PushW   r11                             ; 330E 48                       H
        MoveW   rulerData1+ruler::right_margin, r11
        jsr     L35E7                           ; 3319 20 E7 35                  .5
        PopW    r11                            ; 3320 85 19                    ..
        bcc     L336C                           ; 3322 90 48                    .H
        stx     zp_DAb                        ; 3324 86 DA                    ..
        PushW   r11                             ; 332B 48                       H
        txa                                     ; 332C 8A                       .
        pha                                     ; 332D 48                       H
        ldx     #$18                            ; 332E A2 18                    ..
        jsr     addSideFlippingPixelOffset                           ; 3330 20 E5 26                  .&
        MoveW   r11, L2FBE                           ; 333A 8D BE 2F                 ../
        pla                                     ; 333D 68                       h
        tax                                     ; 333E AA                       .
        PopW    r11                            ; 3343 85 19                    ..
        jsr     L339C                           ; 3345 20 9C 33                  .3
L3348:  lda     #0                            ; 3348 A9 00                    ..
        ldx     zp_DAb                        ; 334A A6 DA                    ..
        cpx     #$02                            ; 334C E0 02                    ..
        bcc     L335E                           ; 334E 90 0E                    ..
        lda     #A4L_01                            ; 3350 A9 01                    ..
        cpx     #$04                            ; 3352 E0 04                    ..
        bcc     L335E                           ; 3354 90 08                    ..
        lda     #A4L_02+A4L_01                            ; 3356 A9 03                    ..
        cpx     #$14                            ; 3358 E0 14                    ..
        bcc     L335E                           ; 335A 90 02                    ..
        lda     #A4L_02                            ; 335C A9 02                    ..
L335E:  sta     a4L                             ; 335E 85 74                    .t
        bit     zp_DBb                        ; 3360 24 DB                    $.
        bmi     L336C                           ; 3362 30 08                    0.
        jsr     L390E                           ; 3364 20 0E 39                  .9
        LoadB   unusedFlag, $FF                           ; 3369 8D A3 2F                 ../
L336C:  rts                                     ; 336C 60                       `

; ----------------------------------------------------------------------------
L336D:  jsr     L35B6                           ; 336D 20 B6 35                  .5
        PushW   r11
        ldx     #$18                            ; 3376 A2 18                    ..
        jsr     addSideFlippingPixelOffset                           ; 3378 20 E5 26                  .&
        MoveW   r11, L2FBE
        PopW    r11
        ldx     zp_DAb                        ; 338B A6 DA                    ..
        jsr     L339C                           ; 338D 20 9C 33                  .3
        LoadB   zp_DBb, 0
        LoadB   unusedFlag, $FF
        jmp     L390E                           ; 3399 4C 0E 39                 L.9

; ----------------------------------------------------------------------------
L339C:  txa                                     ; 339C 8A                       .
        pha                                     ; 339D 48                       H
        ldx     #$18                            ; 339E A2 18                    ..
        jsr     addSideFlippingPixelOffset                           ; 33A0 20 E5 26                  .&
        pla                                     ; 33A3 68                       h
        tax                                     ; 33A4 AA                       .

L33A5:  jsr     L33ED                           ; 33A5 20 ED 33                  .3
	CmpW    r11L, pageWidth1
	bcc     :+                           ; 33B4 90 0A                    ..
        MoveW   pageWidth1, r11
:	lda     zp_DAb                        ; 33C0 A5 DA                    ..
        cmp     #2                            ; 33C2 C9 02                    ..
        beq     @1                           ; 33C4 F0 14                    ..
	CmpW    r11, pageWidth2
	bcc     @1                           ; 33D0 90 08                    ..
	MoveW   pageWidth2, r11L
@1:	lda     r11L                            ; 33DA A5 18                    ..
        sta     rulerData1+ruler::left_margin,x                         ; 33DC 9D 6E 2F                 .n/
        lda     r11H                            ; 33DF A5 19                    ..
        ora     L2FC0                           ; 33E1 0D C0 2F                 ../
        sta     rulerData1+ruler::left_margin+1,x                         ; 33E4 9D 6F 2F                 .o/
        jsr     L3469                           ; 33E7 20 69 34                  i4
        jmp     L34F8                           ; 33EA 4C F8 34                 L.4

; ----------------------------------------------------------------------------
L33ED:  beqx    L3438                           ; 33EE F0 48                    .H
        cpx     #$14                            ; 33F0 E0 14                    ..
        beq     L3438                           ; 33F2 F0 44                    .D
        cpx     #$02                            ; 33F4 E0 02                    ..
        bne     L3468                           ; 33F6 D0 70                    .p
	AddVW2  80, rulerData1, r2
	CmpW    r2L, r11
	bcc     L341B                           ; 3411 90 08                    ..
        MoveW   r2, r11
L341B:  AddVW2  80, rulerData1+ruler::paragraph_margin, r2
	CmpW    r2, r11
	bcs     L3460                           ; 3434 B0 2A                    .*
        bcc     L3468                           ; 3436 90 30                    .0
L3438:  txa                                     ; 3438 8A                       .
        pha                                     ; 3439 48                       H
        ldx     #r2                            ; 343A A2 06                    ..
        jsr     getEffectiveRightMargin                           ; 343C 20 F6 26                  .&
        pla                                     ; 343F 68                       h
        tax                                     ; 3440 AA                       .
	SubVW   80, r2
        lda     r2L                             ; 344E A5 06                    ..
        and     #$F8                            ; 3450 29 F8                    ).
        sta     r2L                             ; 3452 85 06                    ..
        CmpW    r2, r11
	bcs     L3468                           ; 345E B0 08                    ..
L3460:  MoveW   r2, r11
L3468:  rts                                     ; 3468 60                       `

; ----------------------------------------------------------------------------
L3469:  ldx     #r12                            ; 3469 A2 1A                    ..
        jsr     getEffectiveRightMargin                           ; 346B 20 F6 26                  .&
        MoveW   rulerData1, r13
        CmpW    rulerData1+ruler::paragraph_margin, rulerData1
	bcs     L3492                           ; 3486 B0 0A                    ..
        MoveW   rulerData1+ruler::paragraph_margin, r13
L3492:  ldx     #$00                            ; 3492 A2 00                    ..
L3494:  lda     rulerData1+ruler::tabs,x                         ; 3494 BD 72 2F                 .r/
        sta     r0L                             ; 3497 85 02                    ..
        lda     rulerData1+ruler::tabs+1,x                         ; 3499 BD 73 2F                 .s/
        and     #$7F                            ; 349C 29 7F                    ).
        sta     r0H                             ; 349E 85 03                    ..
        CmpW    r13, r0
	bcs     L34E5                           ; 34AA B0 39                    .9
        CmpW    r12, r0
	bcc     L34E5                           ; 34B6 90 2D                    .-
        beq     L34E5                           ; 34B8 F0 2B                    .+
        lda     zp_DAb                        ; 34BA A5 DA                    ..
        beq     L34F1                           ; 34BC F0 33                    .3
        cmp     #$14                            ; 34BE C9 14                    ..
        beq     L34F1                           ; 34C0 F0 2F                    ./
        sub     #$04                            ; 34C3 E9 04                    ..
        sta     r2L                             ; 34C5 85 06                    ..
        cpx     r2L                             ; 34C7 E4 06                    ..
        beq     L34F1                           ; 34C9 F0 26                    .&
        CmpW    r0, L3255
	beq     L34E5                           ; 34D7 F0 0C                    ..
        CmpW    r0, r11
	bne     L34F1                           ; 34E3 D0 0C                    ..
L34E5:  lda     rulerData1+ruler::right_margin                           ; 34E5 AD 70 2F                 .p/
        sta     rulerData1+ruler::tabs,x                         ; 34E8 9D 72 2F                 .r/
        lda     rulerData1+ruler::right_margin+1                           ; 34EB AD 71 2F                 .q/
        sta     rulerData1+ruler::tabs+1,x                         ; 34EE 9D 73 2F                 .s/
L34F1:  inx                                     ; 34F1 E8                       .
        inx                                     ; 34F2 E8                       .
        cpx     #$10                            ; 34F3 E0 10                    ..
        bmi     L3494                           ; 34F5 30 9D                    0.
        rts                                     ; 34F7 60                       `

; ----------------------------------------------------------------------------
L34F8:  lda     #$00                            ; 34F8 A9 00                    ..
        sta     r2L                             ; 34FA 85 06                    ..
        ldx     #$00                            ; 34FC A2 00                    ..
L34FE:  lda     rulerData1+ruler::tabs+1,x                         ; 34FE BD 73 2F                 .s/
        and     #$7F                            ; 3501 29 7F                    ).
        sta     r2H                             ; 3503 85 07                    ..
        lda     rulerData1+ruler::tabs+3,x                         ; 3505 BD 75 2F                 .u/
        and     #$7F                            ; 3508 29 7F                    ).
        cmp     r2H                             ; 350A C5 07                    ..
        bne     L3514                           ; 350C D0 06                    ..
        lda     rulerData1+ruler::tabs+2,x                         ; 350E BD 74 2F                 .t/
        cmp     rulerData1+ruler::tabs,x                         ; 3511 DD 72 2F                 .r/
L3514:  bcs     L3534                           ; 3514 B0 1E                    ..
        ldy     rulerData1+ruler::tabs+1,x                         ; 3516 BC 73 2F                 .s/
        lda     rulerData1+ruler::tabs+3,x                         ; 3519 BD 75 2F                 .u/
        sta     rulerData1+ruler::tabs+1,x                         ; 351C 9D 73 2F                 .s/
        tya                                     ; 351F 98                       .
        sta     rulerData1+ruler::tabs+3,x                         ; 3520 9D 75 2F                 .u/
        ldy     rulerData1+ruler::tabs,x                         ; 3523 BC 72 2F                 .r/
        lda     rulerData1+ruler::tabs+2,x                         ; 3526 BD 74 2F                 .t/
        sta     rulerData1+ruler::tabs,x                         ; 3529 9D 72 2F                 .r/
        tya                                     ; 352C 98                       .
        sta     rulerData1+ruler::tabs+2,x                         ; 352D 9D 74 2F                 .t/
        lda     #$FF                            ; 3530 A9 FF                    ..
        sta     r2L                             ; 3532 85 06                    ..
L3534:  inx                                     ; 3534 E8                       .
        inx                                     ; 3535 E8                       .
        cpx     #$0D                            ; 3536 E0 0D                    ..
        bmi     L34FE                           ; 3538 30 C4                    0.
        bit     r2L                             ; 353A 24 06                    $.
        bmi     L34F8                           ; 353C 30 BA                    0.
        rts                                     ; 353E 60                       `

; ----------------------------------------------------------------------------
initRulerEditing:
	LoadB   mouseTop, 16    ; contrain mouse in ruler
        LoadB   mouseBottom, 28
	START_IO
        MoveB   $D027, $D02E ; color sprite 0 -> 7                           ; 3553 8D 2E D0                 ...
	END_IO
        ldx     #PROCESS_1                              ; 3559 A2 01                    ..
        jsr     RestartProcess                  ; 355B 20 06 C1                  ..
L355E:  LoadB   r3L, 7
        LoadW   r4, sprite_margin                            ; 3566 A9 3C                    .<
        ldx     zp_DAb                        ; 356A A6 DA                    ..
        cpx     #$04                            ; 356C E0 04                    ..
        bcc     L3591                           ; 356E 90 21                    .!
        LoadW   r4, sprite_tab                            ; 3574 A9 7C                    .|
        lda     L2FC0                           ; 3578 AD C0 2F                 ../
        bpl     L3585                           ; 357B 10 08                    ..
        LoadW   r4, sprite_decimal_tab                            ; 3581 A9 BC                    ..
L3585:  cpx     #20                            ; 3585 E0 14                    ..
        bcc     L3591                           ; 3587 90 08                    ..
        LoadW   r4, sprite_paragraph_margin                            ; 358D A9 FC                    ..
L3591:  jmp     DrawSprite                      ; 3591 4C C6 C1                 L..

; ----------------------------------------------------------------------------
L3594:  sec                                     ; 3594 38                       8
        lda     mouseXPos                       ; 3595 A5 3A                    .:
        sbc     #$04                            ; 3597 E9 04                    ..
        sta     r4L                             ; 3599 85 0A                    ..
        lda     mouseXPos+1                             ; 359B A5 3B                    .;
        sbc     #$00                            ; 359D E9 00                    ..
        sta     r4H                             ; 359F 85 0B                    ..
        sec                                     ; 35A1 38                       8
        lda     mouseYPos                       ; 35A2 A5 3C                    .<
        sbc     #$03                            ; 35A4 E9 03                    ..
        sta     r5L                             ; 35A6 85 0C                    ..
        lda     #$07                            ; 35A8 A9 07                    ..
        sta     r3L                             ; 35AA 85 08                    ..
        jsr     PosSprite                       ; 35AC 20 CF C1                  ..
        lda     #$07                            ; 35AF A9 07                    ..
        sta     r3L                             ; 35B1 85 08                    ..
        jmp     EnablSprite                     ; 35B3 4C D2 C1                 L..

; ----------------------------------------------------------------------------
L35B6:  ldx     #PROCESS_1
        jsr     BlockProcess		; disable mouse handler
        lda     #7
        sta     r3L
        jsr     DisablSprite		; disable ruler item sprite
        LoadB   mouseTop, 0
        LoadB   mouseBottom, SC_PIX_HEIGHT-1
        rts

; ----------------------------------------------------------------------------
L35CD:  lda     mouseXPos                       ; 35CD A5 3A                    .:
        add     #$03                            ; 35D0 69 03                    i.
        sta     r11L                            ; 35D2 85 18                    ..
        lda     mouseXPos+1                             ; 35D4 A5 3B                    .;
        adc     #$00                            ; 35D6 69 00                    i.
        sta     r11H                            ; 35D8 85 19                    ..
        lda     r11L                            ; 35DA A5 18                    ..
        and     #$F8                            ; 35DC 29 F8                    ).
        sta     r11L                            ; 35DE 85 18                    ..
        rts                                     ; 35E0 60                       `

; ----------------------------------------------------------------------------
L35E1:  ldx     #$00                            ; 35E1 A2 00                    ..
        lda     #$16                            ; 35E3 A9 16                    ..
        bne     L35EB                           ; 35E5 D0 04                    ..
L35E7:  lda     #$14                            ; 35E7 A9 14                    ..
        ldx     #$04                            ; 35E9 A2 04                    ..
L35EB:  sta     r2L                             ; 35EB 85 06                    ..
        CmpW    r11, pageWidth1
	bcc     L3605                           ; 35F9 90 0A                    ..
        MoveW   pageWidth1, r11
L3605:  lda     rulerData1+ruler::left_margin,x                         ; 3605 BD 6E 2F                 .n/
        sta     r0L                             ; 3608 85 02                    ..
        lda     rulerData1+ruler::left_margin+1,x                         ; 360A BD 6F 2F                 .o/
        and     #$7F                            ; 360D 29 7F                    ).
        sta     r0H                             ; 360F 85 03                    ..
        CmpW    r0, r11
	bne     L3634                           ; 361B D0 17                    ..
        txa                                     ; 361D 8A                       .
        bne     L3632                           ; 361E D0 12                    ..
        CmpW    rulerData1, rulerData1+ruler::paragraph_margin
	bne     L3632                           ; 362E D0 02                    ..
        ldx     #$14                            ; 3630 A2 14                    ..
L3632:  sec                                     ; 3632 38                       8
        rts                                     ; 3633 60                       `

; ----------------------------------------------------------------------------
L3634:  inx                                     ; 3634 E8                       .
        inx                                     ; 3635 E8                       .
        cpx     r2L                             ; 3636 E4 06                    ..
        bcc     L3605                           ; 3638 90 CB                    ..
        clc                                     ; 363A 18                       .
        rts                                     ; 363B 60                       `

; ----------------------------------------------------------------------------
; [XXX huge waste of space!]
sprite_margin:
	.byte   %10000010,%00000000,%00000000
	.byte   %11000110,%00000000,%00000000
	.byte   %10101010,%00000000,%00000000
	.byte   %10010010,%00000000,%00000000
	.byte   %10000010,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   $85

sprite_tab:
	.byte   %00010000,%00000000,%00000000
	.byte   %00101000,%00000000,%00000000
	.byte   %01000100,%00000000,%00000000
	.byte   %01000100,%00000000,%00000000
	.byte   %01111100,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   $85

sprite_decimal_tab:
	.byte   %00010000,%00000000,%00000000
	.byte   %00111000,%00000000,%00000000
	.byte   %01111100,%00000000,%00000000
	.byte   %01111100,%00000000,%00000000
	.byte   %01111100,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   $85

sprite_paragraph_margin:
	.byte   %00111100,%00000000,%00000000
	.byte   %00100100,%00000000,%00000000
	.byte   %00111100,%00000000,%00000000
	.byte   %00100000,%00000000,%00000000
	.byte   %00100000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   %00000000,%00000000,%00000000
	.byte   $85

; drawMarginBarItem1 data [XXX inverted versions of rulerIcons in record 0]
rulerM:
	.byte %11110111,%11011111; ████_█████_█████
	.byte %11110011,%10011111; ████__███__█████
	.byte %11110101,%01011111; ████_█_█_█_█████
	.byte %11110110,%11011111; ████_██_██_█████
	.byte %11110111,%11011111; ████_█████_█████
rulerTab:
	.byte %11111110,%11111111; ███████_████████
	.byte %11111101,%01111111; ██████_█_███████
	.byte %11111011,%10111111; █████_███_██████
	.byte %11111011,%10111111; █████_███_██████
	.byte %11111000,%00111111; █████_____██████
rulerDTab:
	.byte %11111110,%11111111; ███████_████████
	.byte %11111100,%01111111; ██████___███████
	.byte %11111000,%00111111; █████_____██████
	.byte %11111000,%00111111; █████_____██████
	.byte %11111000,%00111111; █████_____██████
rulerP:
	.byte %11111100,%00111111; ██████____██████
	.byte %11111101,%10111111; ██████_██_██████
	.byte %11111100,%00111111; ██████____██████
	.byte %11111101,%11111111; ██████_█████████
	.byte %11111101,%11111111; ██████_█████████

; ----------------------------------------------------------------------------
rulerKeyVector:
	ldx     zp_DAb                        ; 3764 A6 DA                    ..
        cpx     #$04                            ; 3766 E0 04                    ..
        bcc     @rts                           ; 3768 90 16                    ..
        cpx     #$14                            ; 376A E0 14                    ..
        bcs     @rts                           ; 376C B0 12                    ..
        lda     keyData                         ; 376E AD 04 85                 ...
        cmp     #' '                            ; 3771 C9 20                    .
        bne     @rts                           ; 3773 D0 0B                    ..
        lda     L2FC0                           ; 3775 AD C0 2F                 ../
        eor     #$80                            ; 3778 49 80                    I.
        sta     L2FC0                           ; 377A 8D C0 2F                 ../
        jsr     L355E                           ; 377D 20 5E 35                  ^5
@rts:	rts                                     ; 3780 60                       `

; ----------------------------------------------------------------------------

.define ptrs L3789,L378C,L378F,L37A8
L3781:  .lobytes ptrs
L3785:  .hibytes ptrs

; ----------------------------------------------------------------------------
L3789:  ldx     #$00                            ; 3789 A2 00                    ..
	.byte   $2C
L378C:
	ldx     #$02
	.byte   $2C
L378F:
	ldx     #$14
        stx     zp_DAb                        ; 3791 86 DA                    ..
        MoveW   L2FBE, r11
        jsr     getRulerFromText                           ; 379D 20 D9 1D                  ..
        ldx     zp_DAb                        ; 37A0 A6 DA                    ..
        jsr     L33A5                           ; 37A2 20 A5 33                  .3
        jmp     L396C                           ; 37A5 4C 6C 39                 Ll9

; ----------------------------------------------------------------------------
L37A8:  MoveW   L2FBE, r11
        jsr     getRulerFromText                           ; 37B2 20 D9 1D                  ..
        ldx     zp_DAb                        ; 37B5 A6 DA                    ..
        jsr     L33A5                           ; 37B7 20 A5 33                  .3
        jmp     L396C                           ; 37BA 4C 6C 39                 Ll9

; ----------------------------------------------------------------------------
editPageIndicator:
	LoadB   a9H, 0
        bit     zp_DBb
        bmi     finishedEditingPageIndicator

        LoadB   zp_D9b, 0
        ldx     #PROCESS_SELECTION
        jsr     BlockProcess
        MoveB   maxMouseSpeed, saveMaxMouseSpeed
        MoveB   minMouseSpeed, saveMinMouseSpeed
        lda     #4
        sta     maxMouseSpeed
        sta     minMouseSpeed
        LoadB   mouseTop, 0		; constrain to page indicator
        LoadB   mouseBottom, 15
	LoadW   mouseLeft, 192
	LoadW   mouseRight, 207
	START_IO
        MoveB   $D027, $D02D		; color sprite 0 -> 7 to make look active
	END_IO
        ldx     #PROCESS_2
        jsr     RestartProcess
        LoadB   zp_DBb, $FF
clearKeyVectorOtherPressVector:
	lda     #0
        sta     keyVector
        sta     keyVector+1
        sta     otherPressVec
        sta     otherPressVec+1
        rts

finishedEditingPageIndicator:
	MoveB   saveMaxMouseSpeed, maxMouseSpeed	; restore mouse
        MoveB   saveMinMouseSpeed, minMouseSpeed
        LoadB   mouseBottom, SC_PIX_HEIGHT-1
        lda     #0
        sta     mouseTop
        sta     mouseLeft
        sta     mouseLeft+1
        LoadW   mouseRight, SC_PIX_WIDTH-1
        ldx     #PROCESS_2
        jsr     BlockProcess
        LoadB   zp_DBb, 0
	START_IO
        LoadB   $D02D, 0 ; color sprite 6
        lda     $D00D ; sprite 6 Y
        sub     #50
        cmp     #12
        bcc     :+
        lda     #12
:	tax
        jsr     L388A
	END_IO
        MoveW   r0, pagePosY
        jsr     pushRulerData
        jsr     L065D
        jsr     popRulerData
        jmp     L0781

; ----------------------------------------------------------------------------
L388A:  stx     r1L                             ; 388A 86 04                    ..
        MoveW   usablePageHeightDiv13, r0
        ldy     #r1                            ; 3894 A0 04                    ..
        ldx     #r0                            ; 3896 A2 02                    ..
        jsr     BMult                           ; 3898 20 63 C1                  c.
        rts                                     ; 389B 60                       `

; ----------------------------------------------------------------------------
L389C:  jsr     L38A9                           ; 389C 20 A9 38                  .8
        jsr     L38B4                           ; 389F 20 B4 38                  .8
        lda     #6                            ; 38A2 A9 06                    ..
        sta     r3L                             ; 38A4 85 08                    ..
        jmp     PosSprite                       ; 38A6 4C CF C1                 L..

; ----------------------------------------------------------------------------
L38A9:  lda     mouseYPos                       ; 38A9 A5 3C                    .<
        cmp     #12                            ; 38AB C9 0C                    ..
        bmi     L38B1                           ; 38AD 30 02                    0.
        lda     #12                            ; 38AF A9 0C                    ..
L38B1:  sta     r5L                             ; 38B1 85 0C                    ..
        rts                                     ; 38B3 60                       `

; ----------------------------------------------------------------------------
L38B4:  LoadW   r4, 192
        LoadB   sideFlippingOffset, 0
        ldx     pageWidth1+1                           ; 38C1 AE C5 2E                 ...
        dex                                     ; 38C4 CA                       .
        bne     L38DF                           ; 38C5 D0 18                    ..
        CmpWI   mouseXPos, 200                       ; 38CD A5 3A                    .:
	bcc     L38DE                           ; 38D1 90 0B                    ..
        inc     sideFlippingOffset                           ; 38D3 EE BC 2F                 ../
        LoadW   r4, 197
L38DE:  rts                                     ; 38DE 60                       `

; ----------------------------------------------------------------------------
L38DF:  CmpWI   mouseXPos, 198
	bcc     L390D                           ; 38E9 90 22                    ."
        inc     sideFlippingOffset                           ; 38EB EE BC 2F                 ../
        LoadW   r4, $C4                            ; 38F2 A9 C4                    ..
        CmpWI   mouseXPos, 203                            ; 38FE C9 CB                    ..
	bcc     L390D                           ; 3900 90 0B                    ..
        inc     sideFlippingOffset                           ; 3902 EE BC 2F                 ../
        LoadW   r4, 200                            ; 3909 A9 C8                    ..
L390D:  rts                                     ; 390D 60                       `

; ----------------------------------------------------------------------------
L390E:  jsr     pushRulerData                           ; 390E 20 73 26                  s&
        jsr     setDirty                           ; 3911 20 36 1E                  6.
        jsr     killPrompt                      ; 3914 20 6E 1D                  n.
        jsr     L39BE                           ; 3917 20 BE 39                  .9
        jsr     L3996                           ; 391A 20 96 39                  .9
        jsr     L3979                           ; 391D 20 79 39                  y9
        jsr     popRulerData                           ; 3920 20 93 26                  .&
        jsr     pushRulerData                           ; 3923 20 73 26                  s&
        jsr     moveCursor3Ptr_r15                           ; 3926 20 BF 27                  .'
        ldy     #$00                            ; 3929 A0 00                    ..
        lda     (r15),y                        ; 392B B1 20                    . 
        cmp     #$11                            ; 392D C9 11                    ..
        beq     L3960                           ; 392F F0 2F                    ./
        jsr     cmpR15Cursor0Ptr                           ; 3931 20 F3 27                  .'
        beq     L3938                           ; 3934 F0 02                    ..
        bcs     L393D                           ; 3936 B0 05                    ..
L3938:  ldx     #cursor0+cursor::ptr                            ; 3938 A2 80                    ..
        jsr     addRulerSizeToZp                           ; 393A 20 36 27                  6'
L393D:  jsr     CmpR15Cursor1Ptr                           ; 393D 20 1F 28                  .(
        beq     L3944                           ; 3940 F0 02                    ..
        bcs     L3949                           ; 3942 B0 05                    ..
L3944:  ldx     #cursor1+cursor::ptr                            ; 3944 A2 8A                    ..
        jsr     addRulerSizeToZp                           ; 3946 20 36 27                  6'
L3949:  jsr     makeSpaceForRuler                           ; 3949 20 97 1C                  ..
        lda     rulerData1+ruler::justification                           ; 394C AD 84 2F                 ../
        and     #$1F                            ; 394F 29 1F                    ).
        sta     rulerData1+ruler::justification                           ; 3951 8D 84 2F                 ../
        ldy     #$00                            ; 3954 A0 00                    ..
        lda     #$11                            ; 3956 A9 11                    ..
        sta     (r15),y                        ; 3958 91 20                    . 
        jsr     L396C                           ; 395A 20 6C 39                  l9
        bra     L3963                           ; 395E 50 03                    P.

L3960:  jsr     L39B3                           ; 3960 20 B3 39                  .9
L3963:  jsr     L065D                           ; 3963 20 5D 06                  ].
        jsr     popRulerData                           ; 3966 20 93 26                  .&
        jmp     L0781                           ; 3969 4C 81 07                 L..

; ----------------------------------------------------------------------------
L396C:  ldy     #1                            ; 396C A0 01                    ..
@loop:  lda     rulerData1-1,y
        sta     (r15),y                        ; 3971 91 20                    . 
        iny                                     ; 3973 C8                       .
        cpy     #.sizeof(ruler)+1                            ; 3974 C0 1B                    ..
        bne     @loop                           ; 3976 D0 F6                    ..
        rts                                     ; 3978 60                       `

; ----------------------------------------------------------------------------
L3979:  jsr     LoadR15_MEM_PAGE                           ; 3979 20 BF 26                  .&
        jsr     moveR15_cursor3Ptr                           ; 397C 20 B3 27                  .'
L397F:  jsr     getByteIntpNewCardSet                           ; 397F 20 5A 14                  Z.
        tax                                     ; 3982 AA                       .
        jsr     cmpR15Cursor0Ptr                           ; 3983 20 F3 27                  .'
        bcs     L39B2                           ; 3986 B0 2A                    .*
        txa                                     ; 3988 8A                       .
        jsr     skipEscRulerEscGraphics                           ; 3989 20 A1 14                  ..
        cpx     #$0D                            ; 398C E0 0D                    ..
        bne     L397F                           ; 398E D0 EF                    ..
        jsr     moveR15_cursor3Ptr                           ; 3990 20 B3 27                  .'
        bra     L397F                           ; 3994 50 E9                    P.

L3996:  jsr     moveCursor0Ptr_r15                           ; 3996 20 A1 27                  .'
L3999:  jsr     CmpR15Cursor1Ptr                           ; 3999 20 1F 28                  .(
        bcs     L39B2                           ; 399C B0 14                    ..
        jsr     getByteIntpNewCardSet                           ; 399E 20 5A 14                  Z.
        cmp     #$11                            ; 39A1 C9 11                    ..
        bne     L39AD                           ; 39A3 D0 08                    ..
        jsr     L39B3                           ; 39A5 20 B3 39                  .9
        jsr     addRulerSizeToR15                           ; 39A8 20 34 27                  4'
        bcc     L3999                           ; 39AB 90 EC                    ..
L39AD:  jsr     incWR15                           ; 39AD 20 1E 27                  .'
        bcc     L3999                           ; 39B0 90 E7                    ..
L39B2:  rts                                     ; 39B2 60                       `

; ----------------------------------------------------------------------------
L39B3:  ldy     a4L                             ; 39B3 A4 74                    .t
        lda     L3781,y                         ; 39B5 B9 81 37                 ..7
        ldx     L3785,y                         ; 39B8 BE 85 37                 ..7
        jmp     CallRoutine                     ; 39BB 4C D8 C1                 L..

; ----------------------------------------------------------------------------
L39BE:  jsr     getLastRuler                           ; 39BE 20 5B 1E                  [.
        jsr     moveCursor1Ptr_r15                           ; 39C1 20 AA 27                  .'
L39C4:  jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 39C4 20 9E 14                  ..
        tax                                     ; 39C7 AA                       .
        beq     L39F0                           ; 39C8 F0 26                    .&
        cmp     #$0C                            ; 39CA C9 0C                    ..
        beq     L39F0                           ; 39CC F0 22                    ."
        cmp     #$0D                            ; 39CE C9 0D                    ..
        bne     L39C4                           ; 39D0 D0 F2                    ..
        jsr     pushR15                         ; 39D2 20 6C 28                  l(
        jsr     getByteIntpNewCardSet                           ; 39D5 20 5A 14                  Z.
        tax                                     ; 39D8 AA                       .
        jsr     popR15                          ; 39D9 20 7F 28                  .(
        cpx     #$11                            ; 39DC E0 11                    ..
        beq     L39F0                           ; 39DE F0 10                    ..
        lda     cursor0+cursor::srcline                        ; 39E0 A5 84                    ..
        sta     a4H                             ; 39E2 85 75                    .u
        jsr     makeSpaceForRuler                           ; 39E4 20 97 1C                  ..
        ldy     #$1A                            ; 39E7 A0 1A                    ..
L39E9:  lda     (r14),y                        ; 39E9 B1 1E                    ..
        sta     (r15),y                        ; 39EB 91 20                    . 
        dey                                     ; 39ED 88                       .
        bpl     L39E9                           ; 39EE 10 F9                    ..
L39F0:  rts                                     ; 39F0 60                       `

; ----------------------------------------------------------------------------
