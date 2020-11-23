; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  07 - printing
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

	.include "sym.inc"
	.include "geosmac.inc"
	.include "const.inc"
	.include "zeropage.inc"
	.include "geoWrite-0.inc"

; ----------------------------------------------------------------------------
SECOND          := $FF93
CIOUT           := $FFA8
UNLSN           := $FFAE
LISTEN          := $FFB1
; ----------------------------------------------------------------------------

; for #8
.export txt_single_tractor
.export txt_from_to_page
.export txt_high_draft_nlq
.export txt_print
.export L375E
.export byteToDecimal
.export stringToInt
.export L3F7C
.export L4184
.export L4185
.export L4187
.export L4188
.export L4189
.export L418A
.export L418C
.export L41CD
.export L41CE

; from #8
.import convertToCp437
.import showPrintSettings

.segment        "CODE7": absolute

        jmp     print   ; 0
        jmp     preview ; 1
        jmp     L3333   ; 2

; ----------------------------------------------------------------------------
L324D:  lda     #$76                            ; 324D A9 76                    .v
        sta     r0H                             ; 324F 85 03                    ..
        lda     #$80                            ; 3251 A9 80                    ..
        sta     r0L                             ; 3253 85 02                    ..
        lda     L417E                           ; 3255 AD 7E 41                 .~A
        sta     r1H                             ; 3258 85 05                    ..
        lda     L417D                           ; 325A AD 7D 41                 .}A
        sta     r1L                             ; 325D 85 04                    ..
        lda     #$14                            ; 325F A9 14                    ..
        sta     r3L                             ; 3261 85 08                    ..
L3263:  lda     #$04                            ; 3263 A9 04                    ..
        sta     r3H                             ; 3265 85 09                    ..
        lda     #$80                            ; 3267 A9 80                    ..
        sta     r2L                             ; 3269 85 06                    ..
L326B:  jsr     L32DA                           ; 326B 20 DA 32                  .2
        lsr     r2L                             ; 326E 46 06                    F.
        jsr     L32F8                           ; 3270 20 F8 32                  .2
        asl     r2L                             ; 3273 06 06                    ..
        clc                                     ; 3275 18                       .
        lda     #4                            ; 3276 A9 04                    ..
        adc     r0L                             ; 3278 65 02                    e.
        sta     r0L                             ; 327A 85 02                    ..
        bcc     L3280                           ; 327C 90 02                    ..
        inc     r0H                             ; 327E E6 03                    ..
L3280:  inc     r1L                             ; 3280 E6 04                    ..
        bne     L3286                           ; 3282 D0 02                    ..
        inc     r1H                             ; 3284 E6 05                    ..
L3286:  jsr     L32DA                           ; 3286 20 DA 32                  .2
        lsr     r2L                             ; 3289 46 06                    F.
        jsr     L32F8                           ; 328B 20 F8 32                  .2
        lsr     r2L                             ; 328E 46 06                    F.
        clc                                     ; 3290 18                       .
        lda     #4                            ; 3291 A9 04                    ..
        adc     r0L                             ; 3293 65 02                    e.
        sta     r0L                             ; 3295 85 02                    ..
        bcc     L329B                           ; 3297 90 02                    ..
        inc     r0H                             ; 3299 E6 03                    ..
L329B:  ldx     #$04                            ; 329B A2 04                    ..
        jsr     Ddec                            ; 329D 20 75 C1                  u.
        dec     r3H                             ; 32A0 C6 09                    ..
        bne     L326B                           ; 32A2 D0 C7                    ..
        clc                                     ; 32A4 18                       .
        lda     #$08                            ; 32A5 A9 08                    ..
        adc     r1L                             ; 32A7 65 04                    e.
        sta     r1L                             ; 32A9 85 04                    ..
        bcc     L32AF                           ; 32AB 90 02                    ..
        inc     r1H                             ; 32AD E6 05                    ..
L32AF:  dec     r3L                             ; 32AF C6 08                    ..
        bne     L3263                           ; 32B1 D0 B0                    ..
        clc                                     ; 32B3 18                       .
        lda     #$02                            ; 32B4 A9 02                    ..
        adc     L417D                           ; 32B6 6D 7D 41                 m}A
        sta     L417D                           ; 32B9 8D 7D 41                 .}A
        bcc     L32C1                           ; 32BC 90 03                    ..
        inc     L417E                           ; 32BE EE 7E 41                 .~A
L32C1:  lda     L417D                           ; 32C1 AD 7D 41                 .}A
        and     #$07                            ; 32C4 29 07                    ).
        bne     L32D9                           ; 32C6 D0 11                    ..
        clc                                     ; 32C8 18                       .
        lda     #$38                            ; 32C9 A9 38                    .8
        adc     L417D                           ; 32CB 6D 7D 41                 m}A
        sta     L417D                           ; 32CE 8D 7D 41                 .}A
        lda     #$01                            ; 32D1 A9 01                    ..
        adc     L417E                           ; 32D3 6D 7E 41                 m~A
        sta     L417E                           ; 32D6 8D 7E 41                 .~A
L32D9:  rts                                     ; 32D9 60                       `

; ----------------------------------------------------------------------------
L32DA:  ldy     #3                            ; 32DA A0 03                    ..
        LoadB   r2H, 0
L32E0:  lda     (r0),y                         ; 32E0 B1 02                    ..
        beq     L32F3                           ; 32E2 F0 0F                    ..
        lsr     a                               ; 32E4 4A                       J
        lsr     a                               ; 32E5 4A                       J
        lsr     a                               ; 32E6 4A                       J
        lsr     a                               ; 32E7 4A                       J
        tax                                     ; 32E8 AA                       .
        lda     L3323,x                         ; 32E9 BD 23 33                 .#3
        bmi     L331A                           ; 32EC 30 2C                    0,
        add     r2H                             ; 32EF 65 07                    e.
        sta     r2H                             ; 32F1 85 07                    ..
L32F3:  dey                                     ; 32F3 88                       .
        bpl     L32E0                           ; 32F4 10 EA                    ..
        bmi     L3312                           ; 32F6 30 1A                    0.
L32F8:  ldy     #3                            ; 32F8 A0 03                    ..
        LoadB   r2H, 0                             ; 32FC 85 07                    ..
L32FE:  lda     (r0),y                         ; 32FE B1 02                    ..
        beq     L330F                           ; 3300 F0 0D                    ..
        and     #$0F                            ; 3302 29 0F                    ).
        tax                                     ; 3304 AA                       .
        lda     L3323,x                         ; 3305 BD 23 33                 .#3
        bmi     L331A                           ; 3308 30 10                    0.
        add     r2H                             ; 330B 65 07                    e.
        sta     r2H                             ; 330D 85 07                    ..
L330F:  dey                                     ; 330F 88                       .
        bpl     L32FE                           ; 3310 10 EC                    ..
L3312:  lda     #0                            ; 3312 A9 00                    ..
        ldy     r2H                             ; 3314 A4 07                    ..
        cpy     #3                            ; 3316 C0 03                    ..
        bcc     L331C                           ; 3318 90 02                    ..
L331A:  lda     r2L                             ; 331A A5 06                    ..
L331C:  ldy     #0                            ; 331C A0 00                    ..
        ora     (r1),y                         ; 331E 11 04                    ..
        sta     (r1),y                         ; 3320 91 04                    ..
        rts                                     ; 3322 60                       `

; ----------------------------------------------------------------------------
L3323:  .byte   $00,$01,$01,$02,$01,$02,$02,$83 ; 3323 00 01 01 02 01 02 02 83  ........
        .byte   $01,$02,$02,$83,$02,$83,$83,$84 ; 332B 01 02 02 83 02 83 83 84  ........
; ----------------------------------------------------------------------------
L3333:  jsr     L3392                           ; 3333 20 92 33                  .3
        jmp     _exitToDesktop                         ; 3336 4C 68 0F                 Lh.

; ----------------------------------------------------------------------------
print:
	jsr     L3392                           ; 3339 20 92 33                  .3
        jsr     testDiskNotFull2                           ; 333C 20 42 0E                  B.

; swap main code back in
reloadMainCode:
	LoadB   streamingMode, $FF
        jsr     setAppDrive                       ; 3344 20 5B 0E                  [.
@again: MoveW_  appIndexTable, r1
        LoadW   r7, CODE_MAIN                         ; 3357 85 10                    ..
	LoadW   r2, MEM_SIZE_MAIN                       ; 335F 85 06                    ..
        jsr     _ReadFile                       ; 3361 20 E0 28                  .(
        cpx     #BFR_OVERFLOW                            ; 3364 E0 0B                    ..
        bne     @error                           ; 3366 D0 1D                    ..
        jsr     ldR4DiskBlkBuf                  ; 3368 20 62 27                  b'
        jsr     _GetBlock                       ; 336B 20 FB 28                  .(
        bnex    @error                           ; 336F D0 14                    ..
	LoadW   r0, diskBlkBuf+2
	MoveW   r7, r1
        jsr     MoveData                        ; 3381 20 7E C1                  ~.
        rts                                     ; 3384 60                       `

@error: jsr     GotoFirstMenu                   ; 3385 20 BD C1                  ..
        lda     #<txt_swapping_geowrite
        ldy     #>txt_swapping_geowrite
        jsr     showIOError                     ; 338C 20 78 24                  x$
        bra     @again                           ; 3390 50 B5                    P.

; ----------------------------------------------------------------------------
L3392:  PushB   curPage                        ; 3392 A5 D3                    ..
        jsr     createTimeDateStrings                           ; 3395 20 B3 3B                  .;
	LoadW_  RecoverVector, 0
        jsr     showPrintSettings                           ; 33A0 20 02 08                  ..
	LoadW_x RecoverVector, appRecoverVector
        cmp     #$02                            ; 33AD C9 02                    ..
        bne     L33B4                           ; 33AF D0 03                    ..
        jmp     L3497                           ; 33B1 4C 97 34                 L.4
L33B4:  lda     L4187                           ; 33B4 AD 87 41                 ..A
        sta     curPage                        ; 33B7 85 D3                    ..
        jsr     tmpCloseDocFile                           ; 33B9 20 12 0E                  ..
        lda     #$FF                            ; 33BC A9 FF                    ..
        sta     a9L                             ; 33BE 85 7E                    .~
        jsr     loadPrinter                           ; 33C0 20 85 40                  .@
        txa                                     ; 33C3 8A                       .
        beq     L33C9                           ; 33C4 F0 03                    ..
        jmp     L349E                           ; 33C6 4C 9E 34                 L.4
L33C9:  jsr     L35E2                           ; 33C9 20 E2 35                  .5
        jsr     reopenDocFile                           ; 33CC 20 1D 0E                  ..
        bit     L4186                           ; 33CF 2C 86 41                 ,.A
        bmi     L344B                           ; 33D2 30 77                    0w
        LoadW_  RecoverVector, 0
        lda     #$FF                            ; 33DC A9 FF                    ..
        sta     L4183                           ; 33DE 8D 83 41                 ..A
        jsr     setPrintArguments                           ; 33E1 20 47 37                  G7
        jsr     swapUserZp                        ; 33E4 20 39 26                  9&
        jsr     PRINTBASE                       ; 33E7 20 00 79                  .y
        jsr     swapUserZp                        ; 33EA 20 39 26                  9&
	LoadW   RecoverVector, appRecoverVector
L33F7:  bit     L4186                           ; 33F7 2C 86 41                 ,.A
        bmi     L344B                           ; 33FA 30 4F                    0O
        lda     L4184                           ; 33FC AD 84 41                 ..A
        beq     L3432                           ; 33FF F0 31                    .1
        jsr     setPrintArguments                           ; 3401 20 47 37                  G7
        jsr     swapUserZp                        ; 3404 20 39 26                  9&
        jsr     StartASCII                      ; 3407 20 12 79                  .y
        jsr     swapUserZp                        ; 340A 20 39 26                  9&
        txa                                     ; 340D 8A                       .
        bne     L3441                           ; 340E D0 31                    .1
        lda     L4184                           ; 3410 AD 84 41                 ..A
        bpl     L3421                           ; 3413 10 0C                    ..
        jsr     setPrintArguments                           ; 3415 20 47 37                  G7
        jsr     swapUserZp                        ; 3418 20 39 26                  9&
        jsr     SetNLQ                          ; 341B 20 15 79                  .y
        jsr     swapUserZp                        ; 341E 20 39 26                  9&
L3421:  jsr     swapUserZp                        ; 3421 20 39 26                  9&
        jsr     GetDimensions                   ; 3424 20 0C 79                  .y
        jsr     swapUserZp                        ; 3427 20 39 26                  9&
        ldx     #$50                            ; 342A A2 50                    .P
        jsr     L34AD                           ; 342C 20 AD 34                  .4
        bra     L344B                           ; 3430 50 19                    P.

L3432:  jsr     setPrintArguments                           ; 3432 20 47 37                  G7
        jsr     swapUserZp                        ; 3435 20 39 26                  9&
        jsr     StartPrint                      ; 3438 20 03 79                  .y
        jsr     swapUserZp                        ; 343B 20 39 26                  9&
        txa                                     ; 343E 8A                       .
        beq     L3448                           ; 343F F0 07                    ..
L3441:  lda     #<txt_printer_is_inaccessible
        ldy     #>txt_printer_is_inaccessible
        bra     L349B                           ; 3446 50 53                    PS

L3448:  jsr     L34A4                           ; 3448 20 A4 34                  .4
L344B:  LoadW   r5, txt_printing                            ; 344F A9 FD                    ..
	LoadW   r6, $70
        jsr     L34ED                           ; 345B 20 ED 34                  .4
        lda     r0L                             ; 345E A5 02                    ..
        pha                                     ; 3460 48                       H
        bit     L4186                           ; 3461 2C 86 41                 ,.A
        bmi     L3478                           ; 3464 30 12                    0.
        jsr     setPrintArguments                           ; 3466 20 47 37                  G7
        jsr     swapUserZp                        ; 3469 20 39 26                  9&
        jsr     StopPrint                       ; 346C 20 09 79                  .y
        jsr     swapUserZp                        ; 346F 20 39 26                  9&
        jsr     setDocDrive                           ; 3472 20 5F 0E                  _.
        bra     L347B                           ; 3476 50 03                    P.

L3478:  jsr     L3F2B                           ; 3478 20 2B 3F                  +?
L347B:  pla                                     ; 347B 68                       h
        cmp     #$02                            ; 347C C9 02                    ..
        beq     L3497                           ; 347E F0 17                    ..
        inc     curPage                        ; 3480 E6 D3                    ..
        lda     curPage                        ; 3482 A5 D3                    ..
        cmp     L4188                           ; 3484 CD 88 41                 ..A
        beq     L348B                           ; 3487 F0 02                    ..
        bcs     L3497                           ; 3489 B0 0C                    ..
L348B:  jsr     PointRecord                     ; 348B 20 80 C2                  ..
        tya                                     ; 348E 98                       .
        beq     L3497                           ; 348F F0 06                    ..
        jsr     L3602                           ; 3491 20 02 36                  .6
        jmp     L33F7                           ; 3494 4C F7 33                 L.3

; ----------------------------------------------------------------------------
L3497:  pla                                     ; 3497 68                       h
        sta     curPage                        ; 3498 85 D3                    ..
        rts                                     ; 349A 60                       `

; ----------------------------------------------------------------------------
L349B:  jsr     showError                       ; 349B 20 52 24                  R$
L349E:  pla                                     ; 349E 68                       h
        sta     curPage                        ; 349F 85 D3                    ..
        jmp     reopenDocFile                           ; 34A1 4C 1D 0E                 L..

; ----------------------------------------------------------------------------
L34A4:  jsr     swapUserZp                        ; 34A4 20 39 26                  9&
        jsr     GetDimensions                   ; 34A7 20 0C 79                  .y
        jsr     swapUserZp                        ; 34AA 20 39 26                  9&
L34AD:  sty     L418D                           ; 34AD 8C 8D 41                 ..A
        stx     r2L                             ; 34B0 86 06                    ..
	AddVW2 1, pageWidth1, r1
        ldy     #3                            ; 34C1 A0 03                    ..
        ldx     #r1                            ; 34C3 A2 04                    ..
        jsr     DShiftRight                     ; 34C5 20 62 C2                  b.
        lda     r2L                             ; 34C8 A5 06                    ..
        sub     r1L                             ; 34CB E5 04                    ..
        bcc     L34D3                           ; 34CD 90 04                    ..
        lsr     a                               ; 34CF 4A                       J
        bra     L34D5                           ; 34D1 50 02                    P.

L34D3:  lda     #0                            ; 34D3 A9 00                    ..
L34D5:  sta     L418B                           ; 34D5 8D 8B 41                 ..A
        LoadB_x	L418A, 0
        asl     a                               ; 34DD 0A                       .
        rol     L418A                           ; 34DE 2E 8A 41                 ..A
        asl     a                               ; 34E1 0A                       .
        rol     L418A                           ; 34E2 2E 8A 41                 ..A
        asl     a                               ; 34E5 0A                       .
        rol     L418A                           ; 34E6 2E 8A 41                 ..A
        sta     L4189                           ; 34E9 8D 89 41                 ..A
        rts                                     ; 34EC 60                       `

; ----------------------------------------------------------------------------
L34ED:  PushW   r5
	PushW   r6
        jsr     L398F                           ; 34F9 20 8F 39                  .9
	PopW    r6
	PopW    r5
        lda     r6L                             ; 3508 A5 0E                    ..
        sta     L35D3                           ; 350A 8D D3 35                 ..5
        add     #$57                            ; 350E 69 57                    iW
        sta     L35D5                           ; 3510 8D D5 35                 ..5
        lda     r6H                             ; 3513 A5 0F                    ..
        sta     L35D4                           ; 3515 8D D4 35                 ..5
        adc     #$00                            ; 3518 69 00                    i.
        sta     L35D6                           ; 351A 8D D6 35                 ..5
        lda     #$68                            ; 351D A9 68                    .h
        sta     mouseYPos                       ; 351F 85 3C                    .<
        LoadW_  RecoverVector, 0
        sta     streamingMode                        ; 3529 8D 3C 02                 .<.
	LoadW   r0, L35D0                            ; 3530 A9 D0                    ..
        jsr     DoDlgBox                        ; 3534 20 56 C2                  V.
        lda     r0L                             ; 3537 A5 02                    ..
        pha                                     ; 3539 48                       H
        jsr     setSystemFont                  ; 353A 20 D1 24                  .$
        pla                                     ; 353D 68                       h
        sta     r0L                             ; 353E 85 02                    ..
        LoadW   RecoverVector, appRecoverVector
        rts                                     ; 354A 60                       `

; ----------------------------------------------------------------------------
	LoadW	appMain, L3556		; 354B
        rts

; ----------------------------------------------------------------------------
L3556:	jsr     setSystemFont                  ; 3556 20 D1 24                  .$
        lda     #0                            ; 3559 A9 00                    ..
        sta     appMain                         ; 355B 8D 9B 84                 ...
        sta     appMain+1                           ; 355E 8D 9C 84                 ...
        sta     L4181                           ; 3561 8D 81 41                 ..A
        sta     L4182                           ; 3564 8D 82 41                 ..A
        sta     iconSelFlg		; disable icon flashing
        lda     #96                            ; 356A A9 60                    .`
        sta     mouseTop                        ; 356C 8D B8 84                 ...
        lda     #111                            ; 356F A9 6F                    .o
        sta     mouseBottom                     ; 3571 8D B9 84                 ...
        lda     L35D3                           ; 3574 AD D3 35                 ..5
        add     #$10                            ; 3578 69 10                    i.
        sta     mouseLeft                       ; 357A 8D BA 84                 ...
        lda     L35D4                           ; 357D AD D4 35                 ..5
        adc     #$00                            ; 3580 69 00                    i.
        sta     mouseLeft+1                           ; 3582 8D BB 84                 ...
        lda     L35D3                           ; 3585 AD D3 35                 ..5
        add     #$3F                            ; 3589 69 3F                    i?
        sta     mouseRight                      ; 358B 8D BC 84                 ...
        lda     L35D4                           ; 358E AD D4 35                 ..5
        adc     #0                              ; 3591 69 00                    i.
        sta     mouseRight+1                    ; 3593 8D BD 84                 ...
        lda     L35D3                           ; 3596 AD D3 35                 ..5
        add     #$28                            ; 359A 69 28                    i(
        sta     mouseXPos                       ; 359C 85 3A                    .:
        lda     L35D4                           ; 359E AD D4 35                 ..5
        adc     #$00                            ; 35A1 69 00                    i.
        sta     mouseXPos+1                             ; 35A3 85 3B                    .;
        lda     #$68                            ; 35A5 A9 68                    .h
        sta     mouseYPos                       ; 35A7 85 3C                    .<
        jsr     resetToPageStart                           ; 35A9 20 B1 26                  .&
	LoadW   r0, pProcessTable
        lda     #NUM_PPROCESSES                            ; 35B4 A9 03                    ..
        jsr     InitProcesses                   ; 35B6 20 03 C1                  ..
        ldx     L4184                           ; 35B9 AE 84 41                 ..A
        bpl     L35C0                           ; 35BC 10 02                    ..
        ldx     #2                            ; 35BE A2 02                    ..
L35C0:  jsr     RestartProcess                  ; 35C0 20 06 C1                  ..
        rts                                     ; 35C3 60                       `

; ----------------------------------------------------------------------------
pProcessTable:
	;       function         divisor
	.word   pProcess0_XXX,   2
	.word	pProcess1_XXX,   2
        .word   MEM_OVERLAY_PRT, 2
NUM_PPROCESSES  = (* - pProcessTable) / 4


L35D0:  .byte   $01,$48,$77     ; 35CC 80 06 02 00 01 48 77     .....Hw
L35D3:  .byte   $08                             ; 35D3 08                       .
L35D4:  .byte   $00                             ; 35D4 00                       .
L35D5:  .byte   $5F                             ; 35D5 5F                       _
L35D6:  .byte   $00,$0C,$0C,$0D,$0C,$02,$02,$18 ; 35D6 00 0C 0C 0D 0C 02 02 18  ........
        .byte   $13,$4B,$35,$00                 ; 35DE 13 4B 35 00              .K5.
; ----------------------------------------------------------------------------
L35E2:  lda     #$00                            ; 35E2 A9 00                    ..
        sta     L4186                           ; 35E4 8D 86 41                 ..A
        lda     L4184                           ; 35E7 AD 84 41                 ..A
        beq     L3601                           ; 35EA F0 15                    ..
        lda     fileHeader+$5A                  ; 35EC AD 5A 81                 .Z.
        cmp     #$32                            ; 35EF C9 32                    .2
        beq     L35F5                           ; 35F1 F0 02                    ..
        bcs     L3601                           ; 35F3 B0 0C                    ..
L35F5:  bne     L35FE                           ; 35F5 D0 07                    ..
        lda     fileHeader+$5C                  ; 35F7 AD 5C 81                 .\.
        cmp     #$30                            ; 35FA C9 30                    .0
        bcs     L3601                           ; 35FC B0 03                    ..
L35FE:  dec     L4186                           ; 35FE CE 86 41                 ..A
L3601:  rts                                     ; 3601 60                       `

; ----------------------------------------------------------------------------
L3602:  lda     L4185                           ; 3602 AD 85 41                 ..A
        bne     L3624                           ; 3605 D0 1D                    ..
	LoadW   RecoverVector, 0
	LoadW   r0, dlgbox_insert_next_sheet                            ; 3613 A9 3E                    .>
        jsr     DoDlgBox                        ; 3617 20 56 C2                  V.
	LoadW   RecoverVector, appRecoverVector
L3624:  rts                                     ; 3624 60                       `

; ----------------------------------------------------------------------------
L3625:  LoadW   appMain, 0
        LoadW   mouseXPos, $E0
        LoadB   returnAddress, 0
        LoadB   mouseYPos, $72
        rts                                     ; 363D 60                       `

; ----------------------------------------------------------------------------
dlgbox_insert_next_sheet:
	.byte   SET_DB_POS | 1
	.byte   64
	.byte   135
	.word   64
	.word   255
	.byte   DBTXTSTR
	.byte   16
	.byte   16
	.word   txt_insert_next_sheet
	.byte   DBTXTSTR
	.byte   16
	.byte   32
	.word   txt_to_continue_printing
	.byte   OK
	.byte   17
	.byte   42
	.byte   DB_USR_ROUT
	.word   L3625
	.byte   NULL
; ----------------------------------------------------------------------------
pProcess0_XXX:
	CmpW    r15, pageEndPtr2
	bcc     L3665                           ; 3660 90 03                    ..
        jmp     L373E                           ; 3662 4C 3E 37                 L>7

L3665:  jsr     L38C8                           ; 3665 20 C8 38                  .8
L3668:  lda     L4181                           ; 3668 AD 81 41                 ..A
        add     #$08                            ; 366C 69 08                    i.
        sta     r0L                             ; 366E 85 02                    ..
        lda     L4182                           ; 3670 AD 82 41                 ..A
        adc     #$00                            ; 3673 69 00                    i.
        sta     r0H                             ; 3675 85 03                    ..
        lda     pageTextHeight+1                             ; 3677 A5 B4                    ..
        cmp     r0H                             ; 3679 C5 03                    ..
        bne     L3681                           ; 367B D0 04                    ..
        lda     pageTextHeight                             ; 367D A5 B3                    ..
        cmp     r0L                             ; 367F C5 02                    ..
L3681:  bcs     L36D7                           ; 3681 B0 54                    .T
        ldy     #$00                            ; 3683 A0 00                    ..
        lda     (r15),y                        ; 3685 B1 20                    . 
        cmp     #$14                            ; 3687 C9 14                    ..
        bne     L36B0                           ; 3689 D0 25                    .%
        iny                                     ; 368B C8                       .
        iny                                     ; 368C C8                       .
        lda     (r15),y                        ; 368D B1 20                    . 
        sta     pageTextHeight                             ; 368F 85 B3                    ..
        iny                                     ; 3691 C8                       .
        lda     (r15),y                        ; 3692 B1 20                    . 
        sta     pageTextHeight+1                             ; 3694 85 B4                    ..
        lda     pageTextHeight+1                             ; 3696 A5 B4                    ..
        cmp     r0H                             ; 3698 C5 03                    ..
        bne     L36A0                           ; 369A D0 04                    ..
        lda     pageTextHeight                             ; 369C A5 B3                    ..
        cmp     r0L                             ; 369E C5 02                    ..
L36A0:  bcs     L36D7                           ; 36A0 B0 35                    .5
        clc                                     ; 36A2 18                       .
        lda     #4                            ; 36A3 A9 04                    ..
        adc     r15L                            ; 36A5 65 20                    e 
        sta     r15L                            ; 36A7 85 20                    . 
        bcc     L36AD                           ; 36A9 90 02                    ..
        inc     r15H                            ; 36AB E6 21                    .!
L36AD:  jsr     L375E                           ; 36AD 20 5E 37                  ^7
L36B0:  jsr     L3773                           ; 36B0 20 73 37                  s7
        lda     L4182                           ; 36B3 AD 82 41                 ..A
        cmp     pageTextHeight+1                             ; 36B6 C5 B4                    ..
        bne     L36BF                           ; 36B8 D0 05                    ..
        lda     L4181                           ; 36BA AD 81 41                 ..A
        cmp     pageTextHeight                             ; 36BD C5 B3                    ..
L36BF:  bcs     L36D1                           ; 36BF B0 10                    ..
        lda     sideFlippingOffset                           ; 36C1 AD BC 2F                 ../
        pha                                     ; 36C4 48                       H
        lda     #$00                            ; 36C5 A9 00                    ..
        sta     sideFlippingOffset                           ; 36C7 8D BC 2F                 ../
        jsr     L37BF                           ; 36CA 20 BF 37                  .7
        pla                                     ; 36CD 68                       h
        sta     sideFlippingOffset                           ; 36CE 8D BC 2F                 ../
L36D1:  jsr     moveCursor3Ptr_r15                           ; 36D1 20 BF 27                  .'
        jmp     L3668                           ; 36D4 4C 68 36                 Lh6

; ----------------------------------------------------------------------------
L36D7:  bit     L4183                           ; 36D7 2C 83 41                 ,.A
        bmi     L36E2                           ; 36DA 30 06                    0.
        jsr     L324D                           ; 36DC 20 4D 32                  M2
        bra     L36EE                           ; 36E0 50 0C                    P.

L36E2:  jsr     setPrintArguments                           ; 36E2 20 47 37                  G7
        jsr     swapUserZp                        ; 36E5 20 39 26                  9&
        jsr     PrintBuffer                     ; 36E8 20 06 79                  .y
        jsr     swapUserZp                        ; 36EB 20 39 26                  9&
L36EE:  dec     L418D                           ; 36EE CE 8D 41                 ..A
        bne     L36FD                           ; 36F1 D0 0A                    ..
	MoveW   privFHData+privfhdata::pageHeight, L4181
L36FD:  clc                                     ; 36FD 18                       .
        lda     #$08                            ; 36FE A9 08                    ..
        adc     L4181                           ; 3700 6D 81 41                 m.A
        sta     L4181                           ; 3703 8D 81 41                 ..A
        bcc     L370B                           ; 3706 90 03                    ..
        inc     L4182                           ; 3708 EE 82 41                 ..A
L370B:  lda     zp_ADw+1                             ; 370B A5 AE                    ..
        sta     r15H                            ; 370D 85 21                    .!
        lda     zp_ADw                             ; 370F A5 AD                    ..
        sta     r15L                            ; 3711 85 20                    . 
        lda     tmpFont1+1                             ; 3713 A5 B0                    ..
        sta     curFont+1                             ; 3715 85 73                    .s
        lda     tmpFont1                             ; 3717 A5 AF                    ..
        sta     curFont                             ; 3719 85 72                    .r
        lda     tmpMode1                             ; 371B A5 B1                    ..
        sta     currentMode                     ; 371D 85 2E                    ..
        lda     tmpJustification1                             ; 371F A5 B2                    ..
        sta     justification                        ; 3721 85 A9                    ..
        lda     L4180                           ; 3723 AD 80 41                 ..A
        sta     pageTextHeight+1                             ; 3726 85 B4                    ..
        lda     L417F                           ; 3728 AD 7F 41                 ..A
        sta     pageTextHeight                             ; 372B 85 B3                    ..
        jsr     loadFontMetrics                           ; 372D 20 D9 24                  .$
	CmpW    L4181, privFHData+privfhdata::pageHeight
	bcc     L3746                           ; 373C 90 08                    ..
L373E:  lda     #$01                            ; 373E A9 01                    ..
        sta     sysDBData                       ; 3740 8D 1D 85                 ...
        jmp     RstrFrmDialogue                 ; 3743 4C BF C2                 L..

; ----------------------------------------------------------------------------
L3746:  rts                                     ; 3746 60                       `

; ----------------------------------------------------------------------------
setPrintArguments:
	LoadW	r0, MEM_PRINTDATA                            ; 374B A9 80                    ..
	LoadW	r1, MEM_PRINTWORKBUF
	LoadW	r2, 0
        rts                                     ; 375D 60                       `

; ----------------------------------------------------------------------------
L375E:  ldy     #0                            ; 375E A0 00                    ..
        lda     (r15),y                        ; 3760 B1 20                    . 
        cmp     #ESC_RULER                            ; 3762 C9 11                    ..
        bne     @rts                           ; 3764 D0 0C                    ..
        ldy     #page::ruler+ruler::justification
        lda     (r15),y                        ; 3768 B1 20                    . 
        and     #$FF-JST_STARTS_WITH_RULER-JST_20                            ; 376A 29 9F                    ).
        asl     a			; ???
        sta     justification                        ; 376D 85 A9                    ..
        sta     rulerData1+ruler::justification
@rts:	rts                                     ; 3772 60                       `

; ----------------------------------------------------------------------------
L3773:  lda     r15H                            ; 3773 A5 21                    .!
        sta     zp_ADw+1                             ; 3775 85 AE                    ..
        lda     r15L                            ; 3777 A5 20                    . 
        sta     zp_ADw                             ; 3779 85 AD                    ..
        lda     curFont+1                             ; 377B A5 73                    .s
        sta     tmpFont1+1                             ; 377D 85 B0                    ..
        lda     curFont                             ; 377F A5 72                    .r
        sta     tmpFont1                             ; 3781 85 AF                    ..
        lda     currentMode                     ; 3783 A5 2E                    ..
        sta     tmpMode1                             ; 3785 85 B1                    ..
        lda     justification                        ; 3787 A5 A9                    ..
        sta     tmpJustification1                             ; 3789 85 B2                    ..
        lda     pageTextHeight+1                             ; 378B A5 B4                    ..
        sta     L4180                           ; 378D 8D 80 41                 ..A
        lda     pageTextHeight                             ; 3790 A5 B3                    ..
        sta     L417F                           ; 3792 8D 7F 41                 ..A
        jsr     loadFontMetrics                           ; 3795 20 D9 24                  .$
        jsr     measureLine                           ; 3798 20 7A 10                  z.
        lda     L4181                           ; 379B AD 81 41                 ..A
        sub     pageTextHeight                             ; 379F E5 B3                    ..
        sta     r12L                            ; 37A1 85 1A                    ..
        lda     L4182                           ; 37A3 AD 82 41                 ..A
        sbc     pageTextHeight+1                             ; 37A6 E5 B4                    ..
        sta     r12H                            ; 37A8 85 1B                    ..
        sec                                     ; 37AA 38                       8
        lda     #$90                            ; 37AB A9 90                    ..
        sbc     r12L                            ; 37AD E5 1A                    ..
        sta     r1H                             ; 37AF 85 05                    ..
        lda     pageTextHeight                             ; 37B1 A5 B3                    ..
        add     cursor3+cursor::height                        ; 37B4 65 A4                    e.
        sta     pageTextHeight                             ; 37B6 85 B3                    ..
        lda     pageTextHeight+1                             ; 37B8 A5 B4                    ..
        adc     #$00                            ; 37BA 69 00                    i.
        sta     pageTextHeight+1                             ; 37BC 85 B4                    ..
        rts                                     ; 37BE 60                       `

; ----------------------------------------------------------------------------
L37BF:  LoadB	dispBufferOn, ST_WR_BACK; ???
        LoadB   windowTop, 144
        LoadB   windowBottom, 151
        jsr     justifyText
        lda     justification
        bpl     L37D8			; JST_80
        jsr     drawCenteredImage
        jmp     L3892

L37D8:	LoadW   rightMargin, PAGE_WIDTH_WIDE-1
	LoadW   sideFlippingLeft_XXX, PAGE_WIDTH_WIDE-1
        lda     r11L                            ; 37E8 A5 18                    ..
        add     L4189                           ; 37EB 6D 89 41                 m.A
        sta     r11L                            ; 37EE 85 18                    ..
        lda     r11H                            ; 37F0 A5 19                    ..
        adc     L418A                           ; 37F2 6D 8A 41                 m.A
        sta     r11H                            ; 37F5 85 19                    ..
        jsr     setMetricsFromCursor3                           ; 37F7 20 E1 11                  ..
L37FA:  lda     windowTop                       ; 37FA A5 33                    .3
        cmp     #$90                            ; 37FC C9 90                    ..
        bne     L381B                           ; 37FE D0 1B                    ..
        lda     r11H                            ; 3800 A5 19                    ..
        cmp     #$01                            ; 3802 C9 01                    ..
        bne     L380A                           ; 3804 D0 04                    ..
        lda     r11L                            ; 3806 A5 18                    ..
        cmp     #$45                            ; 3808 C9 45                    .E
L380A:  bcc     L381B                           ; 380A 90 0F                    ..
        clc                                     ; 380C 18                       .
        lda     r1H                             ; 380D A5 05                    ..
        adc     #$08                            ; 380F 69 08                    i.
        sta     r1H                             ; 3811 85 05                    ..
        lda     #$98                            ; 3813 A9 98                    ..
        sta     windowTop                       ; 3815 85 33                    .3
        lda     #$9F                            ; 3817 A9 9F                    ..
        sta     windowBottom                    ; 3819 85 34                    .4
L381B:  lda     r11L                            ; 381B A5 18                    ..
        sub     L4189                           ; 381E ED 89 41                 ..A
        sta     r11L                            ; 3821 85 18                    ..
        lda     r11H                            ; 3823 A5 19                    ..
        sbc     L418A                           ; 3825 ED 8A 41                 ..A
        sta     r11H                            ; 3828 85 19                    ..
        jsr     L14BD                           ; 382A 20 BD 14                  ..
        php                                     ; 382D 08                       .
        sta     r0L                             ; 382E 85 02                    ..
        lda     r11L                            ; 3830 A5 18                    ..
        add     L4189                           ; 3833 6D 89 41                 m.A
        sta     r11L                            ; 3836 85 18                    ..
        lda     r11H                            ; 3838 A5 19                    ..
        adc     L418A                           ; 383A 6D 8A 41                 m.A
        sta     r11H                            ; 383D 85 19                    ..
        lda     r2L                             ; 383F A5 06                    ..
        add     L4189                           ; 3842 6D 89 41                 m.A
        sta     r2L                             ; 3845 85 06                    ..
        lda     r2H                             ; 3847 A5 07                    ..
        adc     L418A                           ; 3849 6D 8A 41                 m.A
        sta     r2H                             ; 384C 85 07                    ..
        plp                                     ; 384E 28                       (
        bcs     L3882                           ; 384F B0 31                    .1
        lda     r0L                             ; 3851 A5 02                    ..
        beq     L3877                           ; 3853 F0 22                    ."
        lda     windowTop                       ; 3855 A5 33                    .3
        cmp     #$90                            ; 3857 C9 90                    ..
        beq     L3868                           ; 3859 F0 0D                    ..
        sec                                     ; 385B 38                       8
        lda     r11L                            ; 385C A5 18                    ..
        sbc     #$40                            ; 385E E9 40                    .@
        sta     r11L                            ; 3860 85 18                    ..
        lda     r11H                            ; 3862 A5 19                    ..
        sbc     #$01                            ; 3864 E9 01                    ..
        sta     r11H                            ; 3866 85 19                    ..
L3868:  lda     r2H                             ; 3868 A5 07                    ..
        pha                                     ; 386A 48                       H
        lda     r2L                             ; 386B A5 06                    ..
        pha                                     ; 386D 48                       H
        jsr     putChar_XXX                           ; 386E 20 87 18                  ..
        pla                                     ; 3871 68                       h
        sta     r2L                             ; 3872 85 06                    ..
        pla                                     ; 3874 68                       h
        sta     r2H                             ; 3875 85 07                    ..
L3877:  lda     r2H                             ; 3877 A5 07                    ..
        sta     r11H                            ; 3879 85 19                    ..
        lda     r2L                             ; 387B A5 06                    ..
        sta     r11L                            ; 387D 85 18                    ..
        jmp     L37FA                           ; 387F 4C FA 37                 L.7

; ----------------------------------------------------------------------------
L3882:	LoadW   rightMargin, SC_PIX_WIDTH-1
	LoadW   sideFlippingLeft_XXX, SC_PIX_WIDTH-1
L3892:  lda     #0                              ; 3892 A9 00                    ..
        sta     windowTop                       ; 3894 85 33                    .3
        lda     #SC_PIX_HEIGHT-1                ; 3896 A9 C7                    ..
        sta     windowBottom                    ; 3898 85 34                    .4
        lda     #ST_WR_FORE                            ; 389A A9 80                    ..
        sta     dispBufferOn                    ; 389C 85 2F                    ./
        rts                                     ; 389E 60                       `

; ----------------------------------------------------------------------------
drawCenteredImage:
	lda     r1H                             ; 389F A5 05                    ..
        add     cursor3+cursor::height                        ; 38A2 65 A4                    e.
        bcs     @1                           ; 38A4 B0 06                    ..
        cmp     windowTop                       ; 38A6 C5 33                    .3
        bcc     @rts                           ; 38A8 90 1D                    ..
        beq     @rts                           ; 38AA F0 1B                    ..

@1:	PushB   r1H                             ; 38AC A5 05                    ..
        jsr     centerObject                           ; 38AF 20 CF 1A                  ..
        lda     r1L                             ; 38B2 A5 04                    ..
        add     L418B                           ; 38B5 6D 8B 41                 m.A
        sta     r1L                             ; 38B8 85 04                    ..
        PopB    r1H                             ; 38BB 85 05                    ..
        pha                                     ; 38BD 48                       H
        jsr     calcClippingY                           ; 38BE 20 25 1B                  %.
        jsr     drawClippedPictureFromDisk                           ; 38C1 20 89 19                  ..
        PopB    r1H                             ; 38C5 85 05                    ..
@rts:	rts                                     ; 38C7 60                       `

; ----------------------------------------------------------------------------
L38C8:  lda     #$02                            ; 38C8 A9 02                    ..
        sta     r0H                             ; 38CA 85 03                    ..
        lda     #$80                            ; 38CC A9 80                    ..
        sta     r0L                             ; 38CE 85 02                    ..
        lda     #$76                            ; 38D0 A9 76                    .v
        sta     r1H                             ; 38D2 85 05                    ..
        lda     #$80                            ; 38D4 A9 80                    ..
        sta     r1L                             ; 38D6 85 04                    ..
        lda     #$00                            ; 38D8 A9 00                    ..
        sta     r2L                             ; 38DA 85 06                    ..
        jsr     FillRam                         ; 38DC 20 7B C1                  {.
        rts                                     ; 38DF 60                       `

; ----------------------------------------------------------------------------
preview:
	ldx     #<(scrrecvtab_preview-scrrecvtabs)                            ; 38E0 A2 42                    .B
        jsr     screenSave                           ; 38E2 20 0E 23                  .#
	LoadB   L4183, 0
        LoadB   L4184, 0
        jsr     L3964                           ; 38EF 20 64 39                  d9
	LoadW	L417D, $A08C
        ldx     #$50                            ; 38FC A2 50                    .P
        ldy     #$5E                            ; 38FE A0 5E                    .^
        jsr     L34AD                           ; 3900 20 AD 34                  .4
        jsr     createTimeDateStrings                           ; 3903 20 B3 3B                  .;
	LoadW   r5, txt_previewing
	LoadW	r6, 16
        jsr     L34ED                           ; 3916 20 ED 34                  .4
        lda     r0L                             ; 3919 A5 02                    ..
        cmp     #2                            ; 391B C9 02                    ..
        beq     @skip                           ; 391D F0 0F                    ..
        LoadB   a2L, $FF
	LoadW	r0, L3956                       ; 3929 85 02                    ..
        jsr     DoDlgBox                        ; 392B 20 56 C2                  V.
@skip:  ldx     #<(scrrecvtab_preview-scrrecvtabs)                            ; 392E A2 42                    .B
        jsr     screenRecover                           ; 3930 20 13 23                  .#
        jmp     reloadMainCode                           ; 3933 4C 3F 33                 L?3

; ----------------------------------------------------------------------------
L3936:	LoadW	appMain, L3941
        rts

; ----------------------------------------------------------------------------
L3941:	LoadW   appMain, 0
        LoadB   mouseYPos, 152
	LoadW   mouseXPos, 52
        rts

; ----------------------------------------------------------------------------
L3956:  .byte   SET_DB_POS|1
	.byte	$8C			; top
	.byte	$A5			; bottom
	.word	$18			; left
	.word	$54			; right

	.byte	DB_USR_ROUT
        .word   L3936

        .byte	OK
        .byte	$01			; x
        .byte	$04			; y
        .byte	$00
; ----------------------------------------------------------------------------
L3964:  jsr     i_GraphicsString                ; 3964 20 A8 C1                  ..
		.byte   MOVEPENTO
		.word	130
		.byte	2
		.byte	NEWPATTERN
		.byte	0
		.byte	RECTANGLETO
		.word   301
		.byte   193
		.byte	MOVEPENTO
		.word	130
		.byte	2
		.byte	NEWPATTERN
		.byte	0
		.byte   FRAME_RECTO
		.word	296
		.byte	193
		.byte	NEWPATTERN
		.byte	1
		.byte	MOVEPENTO
		.word	296
		.byte   10
		.byte	RECTANGLETO
		.word	304
		.byte	199
		.byte	MOVEPENTO
		.word	138
		.byte   193
		.byte	RECTANGLETO
		.word	296
		.byte	199
		.byte	NULL
        rts

; ----------------------------------------------------------------------------
L398F:  MoveW   pageWidth1, pageWidth2                        ; 3997 85 EB                    ..
        lda     hasPagePropMetadata                           ; 3999 AD CB 2E                 ...
        beq     L39AE                           ; 399C F0 10                    ..
        ldx     curPage                        ; 399E A6 D3                    ..
        cpx     #PAGE_HEADER                            ; 39A0 E0 3D                    .=
        bcs     L39CD                           ; 39A2 B0 29                    .)
        lda     widthForPage_lo,x                         ; 39A4 BD AC 2C                 ..,
        sta     pageWidth2                        ; 39A7 85 EB                    ..
        lda     widthForPage_hi,x                         ; 39A9 BD E9 2C                 ..,
        sta     pageWidth2+1                        ; 39AC 85 EC                    ..
L39AE:  jsr     createPageNoStrings                           ; 39AE 20 33 3C                  3<
	LoadW	pageEndPtr2, MEM_PAGE
        lda     curPage                        ; 39B9 A5 D3                    ..
        bne     L39CD                           ; 39BB D0 10                    ..

	CmpWI   privFHData+privfhdata::startPageNo, 1
	bne     L39CD                           ; 39C7 D0 04                    ..

        lda     privFHData+privfhdata::titleNlqFlag                        ; 39C9 A5 E4                    ..
        bmi     L39DD                           ; 39CB 30 10                    0.

L39CD:  lda     #PAGE_HEADER                            ; 39CD A9 3D                    .=
        jsr     L3A1E                           ; 39CF 20 1E 3A                  .:
	MoveW   privFHData+privfhdata::headerHeight, r0
        jsr     L3B5C                           ; 39DA 20 5C 3B                  \;
L39DD:  lda     curPage                        ; 39DD A5 D3                    ..
        jsr     L3A1E                           ; 39DF 20 1E 3A                  .:
        lda     privFHData+privfhdata::pageHeight                        ; 39E2 A5 E9                    ..
        sub     privFHData+privfhdata::footerHeight                        ; 39E5 E5 E7                    ..
        sta     r0L                             ; 39E7 85 02                    ..
        lda     privFHData+privfhdata::pageHeight+1                        ; 39E9 A5 EA                    ..
        sbc     privFHData+privfhdata::footerHeight+1                        ; 39EB E5 E8                    ..
        sta     r0H                             ; 39ED 85 03                    ..
        lda     privFHData+privfhdata::footerHeight                        ; 39EF A5 E7                    ..
        ora     privFHData+privfhdata::footerHeight+1                        ; 39F1 05 E8                    ..
        bne     L39FB                           ; 39F3 D0 06                    ..
	LoadW   r0, 0
L39FB:  jsr     L3B5C                           ; 39FB 20 5C 3B                  \;
        lda     privFHData+privfhdata::footerHeight                        ; 39FE A5 E7                    ..
        ora     privFHData+privfhdata::footerHeight+1                        ; 3A00 05 E8                    ..
        beq     L3A1D                           ; 3A02 F0 19                    ..
        lda     curPage                        ; 3A04 A5 D3                    ..
        bne     L3A18                           ; 3A06 D0 10                    ..
	CmpWI   privFHData+privfhdata::startPageNo, 1
	bne     L3A18                           ; 3A12 D0 04                    ..
        lda     privFHData+privfhdata::titleNlqFlag                        ; 3A14 A5 E4                    ..
        bmi     L3A1D                           ; 3A16 30 05                    0.
L3A18:  lda     #PAGE_FOOTER                            ; 3A18 A9 3E                    .>
        jsr     L3A1E                           ; 3A1A 20 1E 3A                  .:
L3A1D:  rts                                     ; 3A1D 60                       `

; ----------------------------------------------------------------------------
L3A1E:  sta     cursor3+cursor::srcline                        ; 3A1E 85 A2                    ..
        jsr     PointRecord                     ; 3A20 20 80 C2                  ..
        tya                                     ; 3A23 98                       .
        bne     L3A27                           ; 3A24 D0 01                    ..
        rts				; does not exist

L3A27:  jsr     setDocDrive                           ; 3A27 20 5F 0E                  _.
        MoveW   pageEndPtr2, r7
        MoveW	r7, r15                         ; 3A38 85 20                    .
        lda     #<MEM_FONT                            ; 3A3A A9 68                    .h
        sub     r7L                             ; 3A3D E5 10                    ..
        sta     r2L                             ; 3A3F 85 06                    ..
        lda     #>MEM_FONT                            ; 3A41 A9 5E                    .^
        sbc     r7H                             ; 3A43 E5 11                    ..
        sta     r2H                             ; 3A45 85 07                    ..
        PushW	r7                              ; 3A4C 48                       H
        jsr     swapUserZp                        ; 3A4D 20 39 26                  9&
        jsr     ReadRecord                      ; 3A50 20 8C C2                  ..
        jsr     swapUserZp                        ; 3A53 20 39 26                  9&
        PopW	r2
        lda     r7L                             ; 3A5C A5 10                    ..
        sub     r2L                             ; 3A5F E5 06                    ..
        sta     r2L                             ; 3A61 85 06                    ..
        lda     r7H                             ; 3A63 A5 11                    ..
        sbc     r2H                             ; 3A65 E5 07                    ..
        sta     r2H                             ; 3A67 85 07                    ..
	CmpWI	r2, $1C
	bne     L3A7C                           ; 3A73 D0 07                    ..
        lda     #0                            ; 3A75 A9 00                    ..
        tay                                     ; 3A77 A8                       .
        sta     (pageEndPtr2),y                    ; 3A78 91 D0                    ..
        beq     L3A89                           ; 3A7A F0 0D                    ..
L3A7C:  sec                                     ; 3A7C 38                       8
        lda     r7L                             ; 3A7D A5 10                    ..
        sbc     #1                            ; 3A7F E9 01                    ..
        sta     pageEndPtr2                        ; 3A81 85 D0                    ..
        lda     r7H                             ; 3A83 A5 11                    ..
        sbc     #0                            ; 3A85 E9 00                    ..
        sta     pageEndPtr2+1                        ; 3A87 85 D1                    ..
L3A89:  ldy     #0                            ; 3A89 A0 00                    ..
        lda     (pageEndPtr2),y                    ; 3A8B B1 D0                    ..
        sta     cursor3+cursor::py                        ; 3A8D 85 A3                    ..
        cmp     #0                            ; 3A8F C9 00                    ..
        beq     L3AA2                           ; 3A91 F0 0F                    ..
        cmp     #PAGE_BREAK                            ; 3A93 C9 0C                    ..
        beq     L3AA2                           ; 3A95 F0 0B                    ..
        iny                                     ; 3A97 C8                       .
        lda     #0                            ; 3A98 A9 00                    ..
        sta     (pageEndPtr2),y                    ; 3A9A 91 D0                    ..
        inc     pageEndPtr2                        ; 3A9C E6 D0                    ..
        bne     L3AA2                           ; 3A9E D0 02                    ..
        inc     pageEndPtr2+1                        ; 3AA0 E6 D1                    ..
L3AA2:  lda     cursor3+cursor::srcline                        ; 3AA2 A5 A2                    ..
        cmp     #PAGE_LAST_USABLE+1                            ; 3AA4 C9 3D                    .=
        bcc     L3AAB                           ; 3AA6 90 03                    ..
        jsr     L3AAC                           ; 3AA8 20 AC 3A                  .:
L3AAB:  rts                                     ; 3AAB 60                       `

; ----------------------------------------------------------------------------
L3AAC:  jsr     cmpPageEndPtr2R15
        bcc     @rts
        beq     @rts
        jsr     getByteIntpNewCardSet
        jsr     handleDateToken
        bcs     L3AAC
        jsr     handleTimeToken
        bcs     L3AAC
        jsr     handlePageToken
        bcs     L3AAC
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics
        bra     L3AAC

@rts:	rts

; ----------------------------------------------------------------------------
handleDateToken:
	ldy     #0
@loop:  lda     txt_date_token,y
        beq     @match
        cmp     (r15),y
        bne     @no
        iny
        bra     @loop

@match:	lda     dateDiff
        jsr     moveRelative

        ldy     #0			; copy date into buffer
@loop2:	lda     dateString,y
        beq     @done
        sta     (r15),y
        iny
        bra     @loop2

@done:  tya
        add     r15L
        sta     r15L
        bcc     :+
        inc     r15H
:	sec
        rts

@no:	clc
        rts

; ----------------------------------------------------------------------------
handleTimeToken:
	ldy     #0
@loop1:	lda     txt_time_token,y
        beq     @match
        cmp     (r15),y
        bne     @no
        iny
        bra     @loop1

@match:	lda     timeDiff
        jsr     moveRelative

        ldy     #0
@loop2:	lda     timeString,y
        beq     @done
        sta     (r15),y
        iny
        bra     @loop2

@done:  tya
        add     r15L
        sta     r15L
        bcc     :+
        inc     r15H
:	sec
        rts

@no:	clc
        rts

; ----------------------------------------------------------------------------
handlePageToken:
	ldy     #0
@loop1: lda     txt_page_token,y
        beq     @match
        cmp     (r15),y
        bne     @no
        iny
        bra     @loop1

@match: lda     pageDiff
        jsr     moveRelative
        ldy     #0
@loop2: lda     pageString,y
        beq     @done
        sta     (r15),y
        iny
        bra     @loop2

@done:  tya
        add     r15L
        sta     r15L
        bcc     :+
        inc     r15H
:	sec
        rts

@no:	clc
        rts

; ----------------------------------------------------------------------------
L3B5C:  CmpWI	pageEndPtr2, MEM_PAGE
	beq     L3BB2                           ; 3B66 F0 4A                    .J

        ldy     #0                            ; 3B68 A0 00                    ..
        lda     L4184                           ; 3B6A AD 84 41                 ..A
        bmi     L3B71                           ; 3B6D 30 02                    0.
        bne     L3BA6                           ; 3B6F D0 35                    .5

L3B71:  lda     #$14                            ; 3B71 A9 14                    ..
        sta     (pageEndPtr2),y                    ; 3B73 91 D0                    ..
        iny                                     ; 3B75 C8                       .
        lda     cursor3+cursor::srcline                        ; 3B76 A5 A2                    ..
        cmp     #PAGE_LAST_USABLE+1                            ; 3B78 C9 3D                    .=
        bcs     L3B87                           ; 3B7A B0 0B                    ..
        lda     cursor3+cursor::py                        ; 3B7C A5 A3                    ..
        jsr     isCrPageBreakOrNull                           ; 3B7E 20 88 13                  ..
        beq     L3B87                           ; 3B81 F0 04                    ..
        lda     #$00                            ; 3B83 A9 00                    ..
        beq     L3B89                           ; 3B85 F0 02                    ..
L3B87:  lda     #$FF                            ; 3B87 A9 FF                    ..
L3B89:  sta     (pageEndPtr2),y                    ; 3B89 91 D0                    ..
        iny                                     ; 3B8B C8                       .
        lda     r0L                             ; 3B8C A5 02                    ..
        sta     (pageEndPtr2),y                    ; 3B8E 91 D0                    ..
        iny                                     ; 3B90 C8                       .
        lda     r0H                             ; 3B91 A5 03                    ..
        sta     (pageEndPtr2),y                    ; 3B93 91 D0                    ..
        iny                                     ; 3B95 C8                       .
        lda     #$00                            ; 3B96 A9 00                    ..
        sta     (pageEndPtr2),y                    ; 3B98 91 D0                    ..
        clc                                     ; 3B9A 18                       .
        lda     #4                            ; 3B9B A9 04                    ..
        adc     pageEndPtr2                        ; 3B9D 65 D0                    e.
        sta     pageEndPtr2                        ; 3B9F 85 D0                    ..
        bcc     L3BA5                           ; 3BA1 90 02                    ..
        inc     pageEndPtr2+1                        ; 3BA3 E6 D1                    ..
L3BA5:  rts                                     ; 3BA5 60                       `

; ----------------------------------------------------------------------------
L3BA6:  lda     #$0D                            ; 3BA6 A9 0D                    ..
        ldy     #$00                            ; 3BA8 A0 00                    ..
        sta     (pageEndPtr2),y                    ; 3BAA 91 D0                    ..
        inc     pageEndPtr2                        ; 3BAC E6 D0                    ..
        bne     L3BB2                           ; 3BAE D0 02                    ..
        inc     pageEndPtr2+1                        ; 3BB0 E6 D1                    ..
L3BB2:  rts                                     ; 3BB2 60                       `

; ----------------------------------------------------------------------------
createTimeDateStrings:
	sei				; prevent race when reading time/date

	; date
	LoadW   r0, dateString
.if DATE_FORMAT_US=1
        jsr     getMonthName
        jsr     getDay
.else
        jsr     getDay
        jsr     getMonthName
.endif
        jsr     getYear
        lda     r0L
        sub     #<(dateString+txt_date_token_len)
        sta     dateDiff

	; time
	LoadW	r0, timeString

        lda     hour
.if DATE_FORMAT_US=1			; AM/PM
        cmp     #12
        bcc     :+			; >= 12?
        sub     #12			; then subtract 12
:	cmp     #0
        bne     :+			; == 0
        lda     #12			; then it's 12
:
.endif
	sta     r3L
        LoadB   r3H, 0

        jsr     byteToDecimal		; hours
        ldy     #0
        lda     #':'
        sta     (r0),y			; ':'
        jsr     IncWR0R1

        lda     minutes
        sta     r3L
        LoadB   r3H, 0
        lda     #1
        jsr     byteToDecimal		; minutes

        ldy     #0
        lda     #' '
        sta     (r0),y			; space
        jsr     IncWR0R1

.if DATE_FORMAT_US=1			; AM/PM
        lda     #'A'
        ldx     hour
        cpx     #12
        bcc     :+
        lda     #'P'
:	sta     (r0),y
        jsr     IncWR0R1
        lda     #'M'
        sta     (r0),y
        jsr     IncWR0R1
.endif

        lda     #0
        sta     (r0),y			; terminating 0

        lda     r0L
        sub     #<(timeString+txt_time_token_len)
        sta     timeDiff

        cli
        rts

; ----------------------------------------------------------------------------
createPageNoStrings:
	lda     privFHData+privfhdata::startPageNo		; effective page no = start no + index
        add     curPage
        sta     r3L
        lda     privFHData+privfhdata::startPageNo+1
        adc     #0
        sta     r3H
	LoadW   r0, pageString
        lda     #0
        jsr     byteToDecimal

        lda     r0L
        sub     #<(pageString+txt_page_token_len)
        sta     pageDiff
        rts

; ----------------------------------------------------------------------------
getMonthName:
	ldy     month
        lda     monthNamePtrsLo-1,y
        sta     r1L
        lda     monthNamePtrsHi-1,y
        sta     r1H
        ldy     #0
@loop:  lda     (r1),y
        sta     (r0),y
        beq     @done
        jsr     IncWR0R1
        bra     @loop

@done:  lda     #' '
        sta     (r0),y
        jsr     IncWR0R1
        rts

; ----------------------------------------------------------------------------
IncWR0R1:
	IncW r0
	IncW r1
	rts                                     ; 3C85 60                       `

; ----------------------------------------------------------------------------

.define monthNamePtrs txt_jan,txt_feb,txt_mar,txt_apr,txt_may,txt_jun,txt_jul,txt_aug,txt_sep,txt_oct,txt_nov,txt_dec
monthNamePtrsLo:   .lobytes monthNamePtrs
monthNamePtrsHi:   .hibytes monthNamePtrs

; ----------------------------------------------------------------------------
getDay:
	MoveB   day, r3L
	LoadB   r3H, 0
        jsr     byteToDecimal
        ldy     #0
.if DATE_FORMAT_US=1
        lda     #','
.else
        lda     #'.'
.endif
        sta     (r0),y
        jsr     IncWR0R1
        lda     #' '
        sta     (r0),y
        jmp     IncWR0R1

; ----------------------------------------------------------------------------
getYear:
	lda     year
        add     #<1900
        sta     r3L
        lda     #>1900
        adc     #0
        sta     r3H
        lda     #0
        jmp     byteToDecimal

; --------------------------------------------
byteToDecimal:
	sta     r2L
        lda     #4
        sta     r2H
L3CD3:  ldy     #0
        ldx     r2H
L3CD7:  lda     r3L
	sec
        sbc     decimal_tab_lo,x
        sta     r3L
        lda     r3H
        sbc     decimal_tab_hi,x
        bcc     L3CEC
        sta     r3H
        iny
        bra     L3CD7

L3CEC:  lda     r3L
        adc     decimal_tab_lo,x
        sta     r3L
        tya
        bne     L3CFE
        ldy     r2L
        bmi     L3CFE
        cpx     r2L
        bne     L3D0B
L3CFE:  ora     #'0'
        ldy     #0
        sta     (r0),y
        jsr     IncWR0R1
        lda     #$FF
        sta     r2L
L3D0B:  dec     r2H
        bpl     L3CD3
        lda     #0
        tay
        sta     (r0),y
        rts

; ----------------------------------------------------------------------------

.define decimal_tab 1,10,100,1000,10000
decimal_tab_lo:  .lobytes decimal_tab
decimal_tab_hi:  .hibytes decimal_tab

; ----------------------------------------------------------------------------

	.include "stringtoint.inc"

; ----------------------------------------------------------------------------
moveRelative:
	tax				; 8 bit diff
        beq     @rts			; = 0 -> nothing to do

        ldy     #0			; sign extend to 16 bit
        tax
        bpl     :+
        dey
:	sty     r5H			; save 1 16 bit diff
        sta     r5L
        tya
        bpl     @1			; positive diff

	; diff < 0
        lda     r15L			; src = r15 - diff
        sta     r1L			; dst = r15
        sub     r5L
        sta     r0L
        lda     r15H
        sta     r1H
        sbc     r5H
        sta     r0H
        bra     @2

	; diff >= 0
@1:	lda     r15L			; src = r15
        sta     r0L			; dst = r15 + diff
        add     r5L
        sta     r1L
        lda     r15H
        sta     r0H
        adc     r5H
        sta     r1H

@2:	AddW_   r5, pageEndPtr2	; pageEndPtr2 += diff

	SubW3	pageEndPtr2, r1, r2	; count = pageEndPtr2 - dst + 1
	IncW	r2

	jsr     MoveData

@rts:	rts

; ----------------------------------------------------------------------------

	.include "strings7.inc"

; ----------------------------------------------------------------------------
pProcess1_XXX:
	lda     r15H                            ; 3F08 A5 21                    .!
        cmp     pageEndPtr2+1                        ; 3F0A C5 D1                    ..
        bne     L3F12                           ; 3F0C D0 04                    ..
        lda     r15L                            ; 3F0E A5 20                    . 
        cmp     pageEndPtr2                        ; 3F10 C5 D0                    ..
L3F12:  beq     L3F23                           ; 3F12 F0 0F                    ..
        jsr     L3F90                           ; 3F14 20 90 3F                  .?
        jsr     L3F38                           ; 3F17 20 38 3F                  8?
        lda     cursor3+cursor::ptr+1                        ; 3F1A A5 9F                    ..
        sta     r15H                            ; 3F1C 85 21                    .!
        lda     cursor3+cursor::ptr                        ; 3F1E A5 9E                    ..
        sta     r15L                            ; 3F20 85 20                    . 
        rts                                     ; 3F22 60                       `

; ----------------------------------------------------------------------------
L3F23:  lda     #1                            ; 3F23 A9 01                    ..
        sta     sysDBData                       ; 3F25 8D 1D 85                 ...
        jmp     RstrFrmDialogue                 ; 3F28 4C BF C2                 L..

; ----------------------------------------------------------------------------
L3F2B:  lda     #PAGE_BREAK                            ; 3F2B A9 0C                    ..
        sta     MEM_PRINTDATA                           ; 3F2D 8D 80 76                 ..v
        lda     #$00                            ; 3F30 A9 00                    ..
        sta     MEM_PRINTDATA+1                           ; 3F32 8D 81 76                 ..v
        jmp     L3FF5                           ; 3F35 4C F5 3F                 L.?

; ----------------------------------------------------------------------------
L3F38:  lda     #$00                            ; 3F38 A9 00                    ..
        sta     r11L                            ; 3F3A 85 18                    ..
L3F3C:  jsr     CmpR15Cursor3Ptr                           ; 3F3C 20 2A 28                  *(
        bcs     L3F70                           ; 3F3F B0 2F                    ./
        jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3F41 20 9E 14                  ..
        cmp     #TAB                            ; 3F44 C9 09                    ..
        beq     L3F5E                           ; 3F46 F0 16                    ..
        cmp     #ESC_GRAPHICS                            ; 3F48 C9 10                    ..
        beq     L3F3C                           ; 3F4A F0 F0                    ..
        cmp     #ESC_RULER                            ; 3F4C C9 11                    ..
        beq     L3F3C                           ; 3F4E F0 EC                    ..
        cmp     #CR                            ; 3F50 C9 0D                    ..
        beq     L3F3C                           ; 3F52 F0 E8                    ..
.if CHAR_ENCODING=CHAR_ENCODING_DE
	jsr     convertToCp437
.endif
        ldy     r11L                            ; 3F54 A4 18                    ..
        sta     MEM_PRINTDATA,y                         ; 3F56 99 80 76                 ..v
        inc     r11L                            ; 3F59 E6 18                    ..
        bra     L3F3C                           ; 3F5C 50 DE                    P.

L3F5E:  lda     #' '                            ; 3F5E A9 20                    .
        ldy     r11L                            ; 3F60 A4 18                    ..
        sta     MEM_PRINTDATA,y                         ; 3F62 99 80 76                 ..v
        inc     r11L                            ; 3F65 E6 18                    ..
        lda     r11L                            ; 3F67 A5 18                    ..
        and     #$07                            ; 3F69 29 07                    ).
        bne     L3F5E                           ; 3F6B D0 F1                    ..
        bra     L3F3C                           ; 3F6E 50 CC                    P.

L3F70:  ldy     r11L                            ; 3F70 A4 18                    ..
        lda     #CR                            ; 3F72 A9 0D                    ..
        sta     MEM_PRINTDATA,y                         ; 3F74 99 80 76                 ..v
        lda     #0                            ; 3F77 A9 00                    ..
        sta     MEM_PRINTDATA+1,y                         ; 3F79 99 81 76                 ..v
L3F7C:  bit     L4186                           ; 3F7C 2C 86 41                 ,.A
        bmi     L3F8D                           ; 3F7F 30 0C                    0.
        jsr     setPrintArguments                           ; 3F81 20 47 37                  G7
        jsr     swapUserZp                        ; 3F84 20 39 26                  9&
        jsr     PrintASCII                      ; 3F87 20 0F 79                  .y
        jmp     swapUserZp                        ; 3F8A 4C 39 26                 L9&

; ----------------------------------------------------------------------------
L3F8D:  jmp     L3FF5                           ; 3F8D 4C F5 3F                 L.?

; ----------------------------------------------------------------------------
L3F90:  lda     #$00                            ; 3F90 A9 00                    ..
        sta     zp_BAb                       ; 3F92 85 BA                    ..
L3F94:  jsr     pushR15                         ; 3F94 20 6C 28                  l(
        jsr     moveR15_cursor3Ptr                           ; 3F97 20 B3 27                  .'
        lda     #$00                            ; 3F9A A9 00                    ..
        sta     r11L                            ; 3F9C 85 18                    ..
L3F9E:  jsr     getByteIntpNewCardSetSkipEscRulerEscGraphics                           ; 3F9E 20 9E 14                  ..
        sta     r0L                             ; 3FA1 85 02                    ..
        cmp     #$0C                            ; 3FA3 C9 0C                    ..
        beq     L3FEE                           ; 3FA5 F0 47                    .G
        cmp     #$00                            ; 3FA7 C9 00                    ..
        beq     L3FEE                           ; 3FA9 F0 43                    .C
        cmp     #$10                            ; 3FAB C9 10                    ..
        beq     L3FEE                           ; 3FAD F0 3F                    .?
        cmp     #$0D                            ; 3FAF C9 0D                    ..
        beq     L3FEE                           ; 3FB1 F0 3B                    .;
        cmp     #$11                            ; 3FB3 C9 11                    ..
        beq     L3F9E                           ; 3FB5 F0 E7                    ..
        bit     zp_BAb                       ; 3FB7 24 BA                    $.
        bmi     L3FC0                           ; 3FB9 30 05                    0.
        jsr     isSpaceOrControl                           ; 3FBB 20 78 13                  x.
        bcc     L3FC3                           ; 3FBE 90 03                    ..
L3FC0:  jsr     moveR15_cursor3Ptr                           ; 3FC0 20 B3 27                  .'
L3FC3:  lda     r0L                             ; 3FC3 A5 02                    ..
        cmp     #$09                            ; 3FC5 C9 09                    ..
        bne     L3FD3                           ; 3FC7 D0 0A                    ..
        lda     r11L                            ; 3FC9 A5 18                    ..
        add     #$08                            ; 3FCC 69 08                    i.
        and     #$F8                            ; 3FCE 29 F8                    ).
        bra     L3FD8                           ; 3FD1 50 05                    P.

L3FD3:  lda     r11L                            ; 3FD3 A5 18                    ..
        add     #$01                            ; 3FD6 69 01                    i.
L3FD8:  sta     r11L                            ; 3FD8 85 18                    ..
        cmp     #$50                            ; 3FDA C9 50                    .P
        bne     L3F9E                           ; 3FDC D0 C0                    ..
        jsr     popR15                          ; 3FDE 20 7F 28                  .(
        jsr     CmpR15Cursor3Ptr                           ; 3FE1 20 2A 28                  *(
        bne     L3FED                           ; 3FE4 D0 07                    ..
        lda     #$FF                            ; 3FE6 A9 FF                    ..
        sta     zp_BAb                       ; 3FE8 85 BA                    ..
        bra     L3F94                           ; 3FEB 50 A7                    P.

L3FED:  rts                                     ; 3FED 60                       `

; ----------------------------------------------------------------------------
L3FEE:  jsr     moveR15_cursor3Ptr                           ; 3FEE 20 B3 27                  .'
        jsr     popR15                          ; 3FF1 20 7F 28                  .(
        rts                                     ; 3FF4 60                       `

; ----------------------------------------------------------------------------
L3FF5:  jsr     swapUserZp                        ; 3FF5 20 39 26                  9&
        lda     #4                            ; 3FF8 A9 04                    ..
        jsr     SetDevice                       ; 3FFA 20 B0 C2                  ..
        jsr     InitForIO                       ; 3FFD 20 5C C2                  \.
        jsr     L405A                           ; 4000 20 5A 40                  Z@
        jsr     L4076                           ; 4003 20 76 40                  v@
        ldy     #0                            ; 4006 A0 00                    ..
L4008:  lda     MEM_PRINTDATA,y                         ; 4008 B9 80 76                 ..v
        beq     L401D                           ; 400B F0 10                    ..
        tax                                     ; 400D AA                       .
        tya                                     ; 400E 98                       .
        pha                                     ; 400F 48                       H
        txa                                     ; 4010 8A                       .
        jsr     L402A                           ; 4011 20 2A 40                  *@
        jsr     CIOUT                           ; 4014 20 A8 FF                  ..
        pla                                     ; 4017 68                       h
        tay                                     ; 4018 A8                       .
        iny                                     ; 4019 C8                       .
        bra     L4008                           ; 401B 50 EB                    P.

L401D:  jsr     L4081                           ; 401D 20 81 40                  .@
        jsr     L4068                           ; 4020 20 68 40                  h@
        jsr     DoneWithIO                      ; 4023 20 5F C2                  _.
        jsr     swapUserZp                        ; 4026 20 39 26                  9&
        rts                                     ; 4029 60                       `

; ----------------------------------------------------------------------------
L402A:  cmp     #$41                            ; 402A C9 41                    .A
        bcc     L403D                           ; 402C 90 0F                    ..
        cmp     #$5A                            ; 402E C9 5A                    .Z
        bcc     L403A                           ; 4030 90 08                    ..
        cmp     #$61                            ; 4032 C9 61                    .a
        bcc     L403D                           ; 4034 90 07                    ..
        cmp     #$7B                            ; 4036 C9 7B                    .{
        bcs     L403D                           ; 4038 B0 03                    ..
L403A:  eor     #$20                            ; 403A 49 20                    I 
        rts                                     ; 403C 60                       `

; ----------------------------------------------------------------------------
L403D:  ldy     #$06                            ; 403D A0 06                    ..
L403F:  cmp     L404C,y                         ; 403F D9 4C 40                 .L@
        beq     L4048                           ; 4042 F0 04                    ..
        dey                                     ; 4044 88                       .
        bpl     L403F                           ; 4045 10 F8                    ..
        rts                                     ; 4047 60                       `

; ----------------------------------------------------------------------------
L4048:  lda     L4053,y                         ; 4048 B9 53 40                 .S@
        rts                                     ; 404B 60                       `

; ----------------------------------------------------------------------------
L404C:  .byte   $7B,$7D,$7E,$60,$5F,$7C,$5C     ; 404C 7B 7D 7E 60 5F 7C 5C     {}~`_|\
L4053:  .byte   $28,$29,$20,$27,$60,$7D,$2F     ; 4053 28 29 20 27 60 7D 2F     () '`}/
; ----------------------------------------------------------------------------
L405A:  lda     #4                            ; 405A A9 04                    ..
        jsr     LISTEN                           ; 405C 20 B1 FF                  ..
        lda     #$F7                            ; 405F A9 F7                    ..
        jsr     SECOND                          ; 4061 20 93 FF                  ..
        jsr     UNLSN                           ; 4064 20 AE FF                  ..
        rts                                     ; 4067 60                       `

; ----------------------------------------------------------------------------
L4068:  lda     #4                            ; 4068 A9 04                    ..
        jsr     LISTEN                           ; 406A 20 B1 FF                  ..
        lda     #$E7                            ; 406D A9 E7                    ..
        jsr     SECOND                          ; 406F 20 93 FF                  ..
        jsr     UNLSN                           ; 4072 20 AE FF                  ..
        rts                                     ; 4075 60                       `

; ----------------------------------------------------------------------------
L4076:  lda     #4                            ; 4076 A9 04                    ..
        jsr     LISTEN                           ; 4078 20 B1 FF                  ..
        lda     #$67                            ; 407B A9 67                    .g
        jsr     SECOND                          ; 407D 20 93 FF                  ..
        rts                                     ; 4080 60                       `

; ----------------------------------------------------------------------------
L4081:  jsr     UNLSN                           ; 4081 20 AE FF                  ..
        rts                                     ; 4084 60                       `

; ----------------------------------------------------------------------------

.include "loadprinter.inc"
.include "loadprinter_strings.inc"

L417D = *
L417E = * + 1
L417F = * + 2
L4180 = * + 3
L4181 = * + 4
L4182 = * + 5
L4183 = * + 6
L4184 = * + 7
L4185 = * + 8
L4186 = * + 9
L4187 = * + 10
L4188 = * + 11
L4189 = * + 12
L418A = * + 13
L418B = * + 14
L418C = * + 15
L418D = * + 16
dateDiff = * + 17
timeDiff = * + 18
pageDiff = * + 19
dateString = * + 20
timeString = * + 40
pageString = * + 60
L41CD = * + 80
L41CE = * + 81
