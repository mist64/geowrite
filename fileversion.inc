; ----------------------------------------------------------------------------
; geoWrite V2.1 (C64)
;  shared code for testing a file's version
; ----------------------------------------------------------------------------
; reverse-engineered by Michael Steil, www.pagetable.com
; ----------------------------------------------------------------------------

testFileVersion:
	MoveW   r5, r9 ; dir entry
        jsr     swapUserZp
        jsr     GetFHdrInfo
        jsr     swapUserZp
        txa
        tay
        bne     @1
        lda     fileHeader+O_GHFNAME+13
        ldx     fileHeader+O_GHFNAME+15
        cmp     #'2'
        bne     @1
        cpx     #'1'
@1:     rts


