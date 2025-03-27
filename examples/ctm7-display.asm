;-------------------------------------
; Example displayer using the ctm7 parser
;
; For simplicity it does not handle tiled maps, only those with
; characters.
;
                .include "ctm7.asm"
MAPDATA         = ctm7("map.ctm")       ; load map file

                .with MAPDATA
                *= $2000
font            .text CHARS.DATA

                *= $0c00
screen          .text MAP.DATA[:2000:2] ; lower 8 bit index only

                *= $0801
                .word 2023, +
                .null $9e, format("%4d", start)
+               .word 0

                .cerror FLAGS.TILES, "can't draw tiles"
                .cerror MAP.WIDTH != 40, "must be 40 chars wide"
                .cerror CHARS.COUNT > 256, "too many characters"

char_colors     .text COLORING.CHAR ? CHARS.COLORS : []

start           lda #MODE.ECM ? $5B : $1B
                sta $d011
                lda #MODE.MULTI ? $18 : $08
                sta $d016
                lda #((screen >> 6) & $f0) | ((font >> 10) & $f)
                sta $d018
                lda #COLORS[0]
                sta $d021
                .if !MODE.HIRES
                lda #COLORS[1]
                sta $d022
                lda #COLORS[2]
                sta $d023
                .endif
                .if MODE.ECM
                lda #COLORS[3]
                sta $d024
                .endif

                .if COLORING.CHAR
                ldx #0
-               .for i in range(4)*$100
                ldy screen + i,x
                lda char_colors,y
                sta $d800 + i,x
                .next
                inx
                bne -
                .elsif COLORING.MAP
                ldx #0
                lda #COLORS[4]
-               sta $d800 + range(4)*$100,x
                inx
                bne -
                .else
                .error "Per tile colouring needs to be written"
                .endif
                jmp *
                .endwith
