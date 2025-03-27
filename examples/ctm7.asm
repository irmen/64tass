; Charpad CTM v7 file parser function
;
; The following attributes (depending on file type) may be present in the
; output:
;
;COLORS           - global colours
;COLORING.MAP     - using per map colours?
;COLORING.TILE    - using per tile colours?
;COLORING.CHAR    - using per character colours?
;MODE.HIRES       - using hires mode?
;MODE.MULTI       - using multi mode?
;MODE.ECM         - using ecm mode?
;FLAGS.TILES      - using tiles?
;CHARS.COUNT      - character count
;CHARS.DATA       - character bitmaps
;CHARS.ATTRIBUTES - packed attributes
;CHARS.COLORS     - character colours
;CHARS.MATERIALS  - character material
;TILES.COUNT      - tile count
;TILES.WIDTH      - tile width in characters
;TILES.HEIGHT     - tile height in characters
;TILES.DATA       - character index words
;TILES.COLOUR     - per tile colours
;TILES.TAGS       - tile tag bytes
;TILES.NAMES      - tile name strings
;MAP.WIDTH        - map width in tiles/characters
;MAP.HEIGHT       - map heigh in tiles/characters
;MAP.DATA         - map tile/character index (alternating low/high bytes)
;
; For detail check the original specs.
;
ctm7            .function data : binary
; helper function which reads count bytes
last            := ?                    ; last read
pos             := 0                    ; read position
read            .function count
last            ::= data[pos : pos + count]
pos             += count
                .endf last

; helper function to read count names
readnames       .function count
names           := []
                .rept count
name            := []
                .while read(1) != 0
name            ..= [last]
                .endwhile
names           ..= [format("%c", name) .. ...]; strings
                .endrept
                .endf names

; helper macro to verify block markers
marker          := x'DAB0'              ; first marker
blockstart      .segment name
                .cerror read(2) != marker, "Wrong ", (\name), " block marker: ", last
marker          += x'0001'
                .endsegment

; check if it's a CTM v7 file
                .cerror read(3) != x'43544D', "Wrong ID: ", last
                .cerror read(1) != 7, "Wrong version: ", int(last)
; interpret map data
mapdata         .namespace
COLORS          = read(5)               ; global colours
                .cerror read(1) > 2, "Unknown coloring method ", int(last)
COLORING        = {.MAP: last == 0, .TILE: last == 1, .CHAR: last == 2}
                .cerror read(1) > 2, "Unknown graphics mode ", int(last)
MODE            = {.HIRES: last == 0, .MULTI: last == 1, .ECM: last == 2}
                .cerror read(1) > %1, "Unknown flags ", bits(last)
FLAGS           = {.TILES: bool(last & %1)}
;----
CHARS           .namespace
                #blockstart "character"
COUNT           = read(2) + 1           ; character count
DATA            = read(COUNT * 8)       ; character bitmaps

                #blockstart "attribute"
ATTRIBUTES      = read(COUNT)           ; packed attributes
COLORS          = ATTRIBUTES & (x'0f' x len(ATTRIBUTES)); colours in low nibble
MATERIALS       = bytes((ATTRIBUTES ^ COLORS) >> 4); materials in high nibble
                .endnamespace
;----
                .if FLAGS.TILES
TILES           .namespace
                #blockstart "tile data"
COUNT           = read(2) + 1           ; tile count
WIDTH           = int(read(1))          ; tile width
HEIGHT          = int(read(1))          ; tile height
DATA            = read(WIDTH * HEIGHT * COUNT * 2); character index words

                .if COLORING.TILE
                #blockstart "tile colours"
COLORS          = read(COUNT)           ; per tile colours
                .fi

                #blockstart "tile tags"
TAGS            = read(COUNT)           ; tile tag bytes

                #blockstart "tile names"
NAMES           = readnames(COUNT)      ; tile names
                .endnamespace
                .fi
;----
MAP             .namespace
                #blockstart "map data"
WIDTH           = int(read(2))          ; map width
HEIGHT          = int(read(2))          ; map height
DATA            = read(WIDTH * HEIGHT * 2); map tile/character index low/high bytes
                .endnamespace

                .cerror len(data) != pos, "file size wrong"
                .endnamespace
                .endf mapdata
