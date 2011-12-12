
; in the original turbo assembler "normal" references
; always refer to a "value" and can only be used in
; place of one (64tass lifts "some" of this limitation)
some1	.macro
	.byte \1
	.byte \2
	.endm

; text references on the other hand work everywhere,
; including quoted text.

; this doesnt work because \1 is passed as a value
;some2	.macro
;	.text \1
;	.endm
; this doesnt work because \1 is not recognized in quotes
;some2	.macro
;	.text "\1"
;	.endm
; with a text reference it works
some2	.macro
	.text "@1"
	.endm
	
; with a text reference also opcodes can be replaced
some3	.macro
	b@1 skip@1
skip@1
	.endm
	
	sei

	; normal references
	#some1 $01, $02

	; text references
        #some2 "sometext"
        
        .byte 0
        #some3 ne
        
        .byte 1
        #some3 eq
        
        .byte 2
