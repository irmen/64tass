.cpu cpu64
brk
ora ($ff,x)
*=*-1
.byte $02
.byte $03
.byte $04
ora $ff
*=*-1
asl $ff
*=*-1
asl $ffffff
*=*-3
php
ora #$ff
*=*-1
asl a
.byte $0b
.byte $0c
ora $ffff
*=*-2
asl $ffff
*=*-2
ora $ffffff
*=*-3
bpl *+1
*=*-1
ora ($ff),y
*=*-1
.byte $12
ora $ffffff,y
*=*-3
.byte $14
ora $ff,x
*=*-1
asl $ff,x
*=*-1
asl $ffffff,x
*=*-3
clc
ora $ffff,y
*=*-2
inc a
.byte $1b
.byte $1c
ora $ffff,x
*=*-2
asl $ffff,x
*=*-2
ora $ffffff,x
*=*-3
jsr $ffff
*=*-2
and ($ff,x)
*=*-1
jsr $ffffff
*=*-3
.byte $33
bit $ff
*=*-1
and $ff
*=*-1
rol $ff
*=*-1
rol $ffffff
*=*-3
plp
and #$ff
*=*-1
rol a
.byte $2b
bit $ffff
*=*-2
and $ffff
*=*-2
rol $ffff
*=*-2
and $ffffff
*=*-3
bmi *+1
*=*-1
and ($ff),y
*=*-1
.byte $32
and $ffffff,y
*=*-3
bit $ff,x
*=*-1
and $ff,x
*=*-1
rol $ff,x
*=*-1
rol $ffffff,x
*=*-3
sec
and $ffff,y
*=*-2
dec a
.byte $3b
bit $ffff,x
*=*-2
and $ffff,x
*=*-2
rol $ffff,x
*=*-2
and $ffffff,x
*=*-3
rti
eor ($ff,x)
*=*-1
.byte $42
.byte $43
.byte $44
eor $ff
*=*-1
lsr $ff
*=*-1
lsr $ffffff
*=*-3
pha
eor #$ff
*=*-1
lsr a
.byte $4b
jmp $ffff
*=*-2
eor $ffff
*=*-2
lsr $ffff
*=*-2
eor $ffffff
*=*-3
bvc *+1
*=*-1
eor ($ff),y
*=*-1
.byte $52
eor $ffffff,y
*=*-3
.byte $54
eor $ff,x
*=*-1
lsr $ff,x
*=*-1
lsr $ffffff,x
*=*-3
cli
eor $ffff,y
*=*-2
phy
.byte $5b
jmp $ffffff
*=*-3
eor $ffff,x
*=*-2
lsr $ffff,x
*=*-2
eor $ffffff,x
*=*-3
rts
adc ($ff,x)
*=*-1
.byte $62
.byte $63
.byte $64
adc $ff
*=*-1
ror $ff
*=*-1
ror $ffffff
*=*-3
pla
adc #$ff
*=*-1
ror a
rtl
jmp ($ffff)
*=*-2
adc $ffff
*=*-2
ror $ffff
*=*-2
adc $ffffff
*=*-3
bvs *+1
*=*-1
adc ($ff),y
*=*-1
.byte $72
adc $ffffff,y
*=*-3
.byte $74
adc $ff,x
*=*-1
ror $ff,x
*=*-1
ror $ffffff,x
*=*-3
sei
adc $ffff,y
*=*-2
ply
.byte $7b
.byte $7c
adc $ffff,x
*=*-2
ror $ffff,x
*=*-2
adc $ffffff,x
*=*-3
bra *+1
*=*-1
sta ($ff,x)
*=*-1
.byte $82
sty $ffffff
*=*-3
sty $ff
*=*-1
sta $ff
*=*-1
stx $ff
*=*-1
stx $ffffff
*=*-3
dey
bit #$ff
*=*-1
txa
phb
sty $ffff
*=*-2
sta $ffff
*=*-2
stx $ffff
*=*-2
sta $ffffff
*=*-3
bcc *+1
*=*-1
sta ($ff),y
*=*-1
.byte $92
sta $ffffff,y
*=*-3
sty $ff,x
*=*-1
sta $ff,x
*=*-1
stx $ff,y
*=*-1
stx $ffffff,x
*=*-3
tya
sta $ffff,y
*=*-2
txs
.byte $9b
stz $ffff
*=*-2
sta $ffff,x
*=*-2
stz $ffff,x
*=*-2
sta $ffffff,x
*=*-3
ldy #$ff
*=*-1
lda ($ff,x)
*=*-1
ldx #$ff
*=*-1
ldy $ffffff
*=*-3
ldy $ff
*=*-1
lda $ff
*=*-1
ldx $ff
*=*-1
ldx $ffffff
*=*-3
tay
lda #$ff
*=*-1
tax
plb
ldy $ffff
*=*-2
lda $ffff
*=*-2
ldx $ffff
*=*-2
lda $ffffff
*=*-3
bcs *+1
*=*-1
lda ($ff),y
*=*-1
.byte $b2
lda $ffffff,y
*=*-3
ldy $ff,x
*=*-1
lda $ff,x
*=*-1
ldx $ff,y
*=*-1
ldx $ffffff,x
*=*-3
clv
lda $ffff,y
*=*-2
tsx
.byte $bb
ldy $ffff,x
*=*-2
lda $ffff,x
*=*-2
ldx $ffff,y
*=*-2
lda $ffffff,x
*=*-3
cpy #$ff
*=*-1
cmp ($ff,x)
*=*-1
.byte $c2
.byte $c3
cpy $ff
*=*-1
cmp $ff
*=*-1
dec $ff
*=*-1
dec $ffffff
*=*-3
iny
cmp #$ff
*=*-1
dex
.byte $cb
cpy $ffff
*=*-2
cmp $ffff
*=*-2
dec $ffff
*=*-2
cmp $ffffff
*=*-3
bne *+1
*=*-1
cmp ($ff),y
*=*-1
.byte $d2
cmp $ffffff,y
*=*-3
.byte $d4
cmp $ff,x
*=*-1
dec $ff,x
*=*-1
dec $ffffff,x
*=*-3
cld
cmp $ffff,y
*=*-2
phx
.byte $db
.byte $dc
cmp $ffff,x
*=*-2
dec $ffff,x
*=*-2
cmp $ffffff,x
*=*-3
cpx #$ff
*=*-1
sbc ($ff,x)
*=*-1
.byte $e2
.byte $e3
cpx $ff
*=*-1
sbc $ff
*=*-1
inc $ff
*=*-1
inc $ffffff
*=*-3
inx
sbc #$ff
*=*-1
nop
.byte $eb
cpx $ffff
*=*-2
sbc $ffff
*=*-2
inc $ffff
*=*-2
sbc $ffffff
*=*-3
beq *+1
*=*-1
sbc ($ff),y
*=*-1
.byte $f2
.byte $f3
.byte $f4
sbc $ff,x
*=*-1
inc $ff,x
*=*-1
inc $ffffff,x
*=*-3
sed
sbc $ffff,y
*=*-2
plx
.byte $fb
.byte $fc
sbc $ffff,x
*=*-2
inc $ffff,x
*=*-2
sbc $ffffff,x
