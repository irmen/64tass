
    * = $1000

    jmp blub
    .byte 1, 2, 3, 4
blub
    rts
    .eor $ff
    .byte 0, 1, 2 ,3
    .eor $00
    .byte 0, 1, 2, 3

