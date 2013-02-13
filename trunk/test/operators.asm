
    .warn (1,)
    .warn ()
    .warn min()
    .warn [1,]
    .warn []

; check all operators

; bit or
    .byte $0a | $b0
; bit xor
    .byte $0a ^ $ba

; bool not
    lda !*

; force 8bit addr
    lda @b $ff
; force 16bit addr
    lda @w $ff

    .cpu 65816

; force 24bit addr
    lda @l $ff
