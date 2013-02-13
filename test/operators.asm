
; "logical" operators

    .cerror ("" && []) != ""
    .cerror ("" && (1,)) != ""
    .cerror (2 && "") != ""
    .cerror (1 && 2) != 2

    .cerror (() || "") != ""
    .cerror ("" || 2) != 2
    .cerror (2 || "") != 2
    .cerror (1 || [2,]) != 1

    .cerror ("" ^^ []) != 0
    .cerror ("" ^^ 2) != 2
    .cerror (1 ^^ "") != 1
    .cerror (1 ^^ 2) != 0

    .cerror 1 ? 0 : unknown
    .cerror 0 ? unknown : 0
    .cerror 0 && unknown
    .cerror !(1 || unknown)

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
