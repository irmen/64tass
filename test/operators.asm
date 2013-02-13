
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

    .cerror 1==0
    .cerror 0!=0
    .cerror 1 < 0
    .cerror 0 > 1
    .cerror 0 >= 1
    .cerror 1 <= 0
    .cerror 2*2!=4
    .cerror 6/2!=3
    .cerror 6%4!=2
    .cerror 2+2!=4
    .cerror 2-3!=-1
    .cerror 2&6!=2
    .cerror 2|4!=6
    .cerror 7^12!=11
    .cerror 1 << 3 != 8
    .cerror -8 >> 3 != -1
    .cerror -8 >>> 3 == -1
    .cerror ~0!=-1
    .cerror (^2049)!="2049"
    .cerror 2**3 != 8
    .cerror 2**0 != 1
    .cerror "a"=="b"
    .cerror "a"!="a"
    .cerror "a">"b"
    .cerror "b"<"a"
    .cerror "a">="b"
    .cerror "b"<="a"
    .cerror 2.5+25e-1!=5
    .cerror [1,2]!=[1,2]
    .cerror (1,2)!=(1,2)

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
