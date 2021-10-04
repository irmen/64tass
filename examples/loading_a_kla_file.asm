;
; Splitting a binary koala file into its components is easy and
; just a matter using .binary a few times:
;
; bitmap     .binary "picture.kla", 2, 8000
; screen     .binary "picture.kla", 8002, 1000
; color      .binary "picture.kla", 9002, 1000
; background = binary("picture.kla", 10002, 1)
;
; Of course a constant could be used for the file name to not repeat
; it all the time.
;
; But once we have that why not use that constant to hold the various
; components instead of the file's name? That'd be lot more useful.
;
; So the following function does that. It loads the koala file and splits
; it into it's components. Later these are used where ever needed.

loadkla         .function _data : binary; use binary() on parameter
BITMAP          = _data[2:8002]         ; 8000 bytes bitmap
SCREEN          = _data[8002:9002]      ; 1000 bytes screen
COLOR           = _data[9002:10002]     ; 1000 bytes colour
BACKGROUND      = _data[10002]          ; 1 byte background
                .endf namespace(*)      ; return the namespace

; calculate constant for VIC-II font/bitmap and screen location in video RAM
vicmem          .sfunction _bmp, _scr, ((_bmp >> 10) & $0f) | ((_scr >> 6) & $f0)

; Simple koala displayer follows:

; PICTURE will hold the components of the koala picture
PICTURE         = loadkla("picture.kla"); load this file

*               = $0801                 ; C64 BASIC header
                .word (+), 2021
                .null $9e, format("%4d", start)
+               .word 0

start           lda #$3b
                sta $d011               ; select bitmap mode
                lda #$18
                sta $d016               ; select multi colour mode
                lda #vicmem(bitmap, screen)
                sta $d018               ; screen and bitmap locations
                lda #PICTURE.BACKGROUND
                sta $d021               ; background colour

                ldx #0                  ; copy 1K of colours
-               .for i in range(0, 1024, 256)
                lda color+i,x
                sta $d800+i,x
                .next
                inx
                bne -
                stx $d015               ; disable sprites (just in case)
                jmp *                   ; done

; dump screen to $0c00 to avoid copying it.
*               = $0c00
screen          .text PICTURE.SCREEN    ; dump screen data
color           .text PICTURE.COLOR     ; dump color data

; dump bitmap to $2000 to avoid copying it
*               = $2000
bitmap          .text PICTURE.BITMAP    ; dump bitmap data
