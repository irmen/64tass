; check all operators

; bit or
    .byte $0a | $b0
; bit xor
    .byte $0a ^ $ba
; bool not
    lda !*
