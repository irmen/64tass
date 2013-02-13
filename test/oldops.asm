
    .cerror 1+3*4/2-8
    .cerror 1+(3*4)-13
    .cerror ($ba & $ff) - $ba  ; and
    .cerror ($0a . $b2) - $ba  ; or
    .cerror ($0a : $ba) - $b0  ; xor
    .cerror <$1234-$34
    .cerror >$1234-$1200

; force 16 bit addr
    lda !*
