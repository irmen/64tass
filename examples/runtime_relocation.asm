
; This example demonstrates how to do runtime relocation of code to an unknown
; address. To simplify matters the granuality is per-page but can be extended
; to per byte.

; The idea is to compile the code twice to different addresses and then check
; which bytes differ by the shift amount. Those will be the ones which need
; relocation.

; For this to work the code should not change it's structure (or size) when
; compiled multiple times but that's usually not difficult.

*               = $0801                 ; C64 BASIC header
                .word (+), 2023
                .null $9e, format("%4d", start)
+               .word 0

start
                lda #>$1000             ; the destination isn't known at compile time
                jsr relocate            ; so this routine does the relocation
                jmp $1000               ; jump to the relocated code

; To not repeat it twice the code to be relocated is put in this macro. It doesn't
; do anything useful but at least has 3 instruction which need relocation.

relocated_code  .macro
                ldx #0
lp              lda counter             ; this will need relocation
                sta $400,x
                inx
                dec counter             ; this will need relocation
                beq end
                jmp lp                  ; this will need relocation
end             rts

counter         .byte 10
                .endm

; Compile twice. For the second time a page further up in memory. And so all
; high bytes will be different by one.

code1           .logical $100           ; compile to $100. Not zero page or else
                #relocated_code         ; there's a risk of using zeropage
                .here                   ; addressing modes for data access!

code2           .virtual $200           ; virtual only as the result doesn't
                #relocated_code         ; need saving in the image
                .endv

; Now let's compare the two compilations and collect what needs to be relocated.

; This function takes two lists (the two versions of code) and returns the
; offsets of differences. It makes sure that the difference is exactly 1 as that
; was the amount of pages it was shifted in memory. If it goes wrong check
; the listing file at the reported address.

list_differences .function _code1, _code2
                .cerror len(_code1) != len(_code2), "Both should have the same size!"
_differences    := []                   ; start with empty list
                .for _i in range(len(_code1))
                .continueif _code1[_i] == _code2[_i]; same?
                .cwarn <(_code2[_i] - _code1[_i]) != 1, "Can't relocate at ", code1 + _i
_differences    ..= [_i]                ; add offset to list
                .next
                .endf _differences

; This table holds the offsets which need relocation. For this simple short
; example byte offsets are sufficient.

relocation_table .byte list_differences(code1[::], code2[::])

; Please note that for now the bytes of the two codes are put in lists using
; [::] which may change in the future. That's why the function does not rely on
; being able to access code bytes through indexing.

; This routine copies the relocated code and fixes its instructions up.
; Destination page is given in the accumulator.

relocate        .proc
dstp            = $fb                   ; a zeropage pointer used for copying

                sta dstp+1              ; code will go to $xx00
                ldy #0
                sty dstp
_lp
                lda code1,y
                sta (dstp),y            ; do actual copy to destination
                iny
                cpy #size(code1)
                blt _lp

                ldx #0
_lp2                                    ; this loop fixes instruction address bytes
                ldy relocation_table,x  ; who's offsets were collected earlier
                lda (dstp),y
                clc
                adc dstp+1              ; add high byte of destination address
                sec
                sbc #>$100              ; one less as it was compiled to $100!
                sta (dstp),y
                inx
                cpx #size(relocation_table)
                blt _lp2
                rts
                .pend

