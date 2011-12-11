
    .cpu 6502

    * = $1234

    .text "{clr}{up}{return}abc"
    .text "{clr}abc"
    .text "{up}abc{return}"
    .text "abc{up}abc"
    .byte 0

;    .text "{}" ; error "petascii symbol expected"
;    .text "{foo}" ; error "petascii symbol expected"
;    .text "{abcdefghijklmnopqrstuvwxyz}" ; error "constant too large"
