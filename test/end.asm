
    * = $1000

    jmp blub
    .byte 1, 2, 3, 4
blub
    lda #0
    sta bla+1
    nop
bla
    ldx #0
    stx $d020
    rts

    .end ; terminate assembly here

    this is ignored
