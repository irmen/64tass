; check old operators, enabled by -O, --compatible-ops

; bit or
    .byte $0a . $b0
    .byte $0a | $b0
; bit xor
    .byte $0a : $ba
    .byte $0a ^ $ba
; force 16 bit addr
    lda !*
