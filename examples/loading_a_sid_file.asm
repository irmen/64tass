;
; In most cases where the SID file is known this might work:
;
                .comment
                *= $1000
music           .binary "music.sid", $7e; skip header and load

                *= $0801                ; C64 BASIC header
                .word (+), 2021
                .null $9e, format("%4d", start)
+               .word 0

start           lda #0
                jsr music               ; initialize

lp              bit $d011
                bpl lp
-               bit $d011
                bmi -
                jsr music+3             ; call play every frame
                jmp lp
                .endc
;
; However SID files can have all sorts of load/init/play addresses
; and might not always have the same length of header either.
;
; So the following function tries to figure these details out.

loadsid         .function _data : binary; use binary() for this parameter
_data_at        := _data[[$7,$6]]       ; data offset (big endian)
_load           := _data[[$9,$8]]       ; load address (big endian)
_init           := _data[[$b,$a]]       ; init address (big endian)
_play           := _data[[$d,$c]]       ; play address (big endian)

; dump music data after the header (and the optional loading address)
_sid            .text _data[_data_at + (_load != 0 ? 0 : 2):]

; if load address is zero then it's the first 2 bytes of music data
_sid.load       = _load != 0 ? _load : _data[_data_at + [0, 1]]
_sid.init       = _init != 0 ? _init : _load
_sid.play       = _play                 ; put results into constants
                .endf _sid              ; and return them

; There's a lot more information in the header, however that would make
; this example unnecessary complicated so it's left as an exercise
; to the reader.
; 
                *= music.load           ; set PC to load address
music           #loadsid "music.sid"    ; load this file as music

*               = $0801                ; C64 BASIC header
                .word (+), 2021
                .null $9e, format("%4d", start)
+               .word 0

start           lda #0
                jsr music.init          ; initialize

lp              bit $d011
                bpl lp
-               bit $d011
                bmi -
                jsr music.play          ; call play every frame
                jmp lp
