
                *= $1000

; Static relocation is easy, just use .logical/.here and copy the code to the
; already know destination for execution.

                ldx #0
_lp
                lda inram,x
                sta reloc,x
                inx
                cpx #size(reloc)
                bcc _lp

; Patching it at the destination is simple as well. As all labels point to
; destination memory it can be directly overwritten.

                sta reloc.patchme + 1

; But what if the code has to be transmitted in some way to somewhere else for
; execution? Then the only way is to patch it before the transmission in RAM.
; But all the available labels contain the "wrong" addresses for that and can't
; be used as-is.

; Normally one calculates the in memory address by adjusting it with the
; difference:

                sta inram + reloc.patchme - reloc + 1

; When patching many locations it's less error prone to move the calculation to
; a function:

                sta patch(reloc.patchme) + 1
                rts

; Function to calculate in memory address. May be a good idea to put it inside
; the "reloc" scope but that's not done now.

patch           .sfunction _adr, inram + _adr - reloc

; Example relocated routine, compiled here but needs to execute at $2000

inram           .logical $2000
reloc           .block                  ; in separate scope as a reminder
patchme         lda #0                  ; that labels inside are relocated
                inc patchme+1
                .bend
                .here

;-----------------------
; Not recommended at all but if one is careful it's possible to put all
; relocated code into a macro and compile it twice just to get direct labels
; which carry the in memory address:

                sta virt.patchme + 1

; For the virtual double compile hack the code in the macro must be written so
; that it compiles the same way every time!
; And it's really easy to get this wrong. For example when relocating to zero
; page the second in memory address compile might use absolute addressing while
; the first did use zero page for an opcode. These are different size and so
; the second compile will have useless shifted addresses from there on. You
; have been warned.

drvcode         .macro
patchme         lda #0
                inc patchme+1
                .endm

; Actual in memory code relocated to $0300.

inram2          .logical $300
                #drvcode
                .here

; Virtual compile a second time to have the right labels for patching

                .virtual inram2
virt            #drvcode
                .endv

; Sanity check, not perfect but better than nothing. Change the relocation from
; $300 to $30 see it triggered

                .cerror size(virt) != size(reloc), "Different size, how?!"
