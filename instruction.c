/*
    $Id: instruction.c 3226 2025-04-25 05:34:13Z soci $

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/
#include "instruction.h"
#include <string.h>
#include "opcodes.h"
#include "64tass.h"
#include "section.h"
#include "file.h"
#include "listing.h"
#include "error.h"
#include "longjump.h"
#include "arguments.h"
#include "optimizer.h"

#include "addressobj.h"
#include "listobj.h"
#include "registerobj.h"
#include "codeobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "memblocksobj.h"
#include "eval.h"

static const uint32_t *mnemonic;    /* mnemonics */
static const uint16_t *opcode;      /* opcodes */
static unsigned int last_mnem;

bool longaccu, longindex, autosize; /* hack */
uint32_t dpage;
unsigned int databank;
bool longbranchasjmp;
bool allowslowbranch;

int lookup_opcode(const uint8_t *s) {
    int32_t s4;
    unsigned int also, felso, elozo, no;
    int32_t name;

    name = (s[0] << 16) | (s[1] << 8) | s[2];
    if (arguments.caseinsensitive != 0) name |= 0x202020;
    also = 0;
    no = (felso = last_mnem) / 2;
    for (;;) {  /* do binary search */
        if ((s4 = name - (int32_t)mnemonic[no]) == 0) return (int)no;
        elozo = no;
        if (elozo == (no = ((s4 > 0) ? (felso + (also = no)) : (also + (felso = no))) / 2)) break;
    }
    return -1;
}

void select_opcodes(const struct cpu_s *cpumode) {
    last_mnem = cpumode->opcodes;
    mnemonic = cpumode->mnemonic;
    opcode = cpumode->opcode;
}

MUST_CHECK bool touval(Obj *v1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Error *err;
    if (v1 == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = v1->obj->uval(v1, uv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

MUST_CHECK bool toival(Obj *v1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Error *err;
    if (v1 == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = v1->obj->ival(v1, iv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

MUST_CHECK bool touaddress(Obj *v1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Error *err;
    if (v1 == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = v1->obj->uaddress(v1, uv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

MUST_CHECK bool toiaddress(Obj *v1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Error *err;
    if (v1 == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = v1->obj->iaddress(v1, iv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

static MUST_CHECK bool tocode_uaddress(Obj *v1, uval_t *uv, uval_t *uv2, linepos_t epoint) {
    Error *err;
    if (v1 == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = code_uaddress(v1, uv, uv2, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

MUST_CHECK Error *err_addressing(atype_t am, linepos_t epoint, int prm) {
    Error *v;
    if (am > MAX_ADDRESS_MASK) return new_error(ERROR__ADDR_COMPLEX, epoint);
    v = new_error(ERROR_NO_ADDRESSING, epoint);
    v->u.addressing.am = am;
    v->u.addressing.cod = (prm >= 0) ? mnemonic[prm] : 0;
    return v;
}

static MUST_CHECK Error *err_addressize(Error_types no, linepos_t epoint, int prm) {
    Error *v;
    v = new_error(no, epoint);
    v->u.addresssize.cod = mnemonic[prm];
    return v;
}

static void dump_instr(unsigned int cod, uint32_t adr, int ln, linepos_t epoint)  {
    if (diagnostics.optimize) cpu_opt((uint8_t)cod, adr, ln, epoint);
    if (ln >= 0) {
        uint8_t *d;
        uint32_t temp;
        d = pokealloc((unsigned int)(ln + 1), epoint);
        temp = adr ^ outputeor;
        switch (ln) {
        case 4: d[4] = (uint8_t)(temp >> 24); FALL_THROUGH; /* fall through */
        case 3: d[3] = (uint8_t)(temp >> 16); FALL_THROUGH; /* fall through */
        case 2: d[2] = (uint8_t)(temp >> 8); FALL_THROUGH; /* fall through */
        case 1: d[1] = (uint8_t)temp; FALL_THROUGH; /* fall through */
        default: d[0] = (uint8_t)(cod ^ outputeor);
        }
    }
    listing_instr(cod, adr, ln);
}

typedef enum Adrgen {
    AG_ZP, AG_B0, AG_PB, AG_BYTE, AG_SBYTE, AG_CHAR, AG_DB3, AG_DB2, AG_WORD,
    AG_SWORD, AG_SINT, AG_RELPB, AG_RELL, AG_IMP, AG_NONE
} Adrgen;

#define is_amode3(a, b, c, d) (((a) & ((1u << (b)) | (1u << (c)) | (1u << (d)))) != 0)
#define is_amode2(a, b, c) (((a) & ((1u << (b)) | (1u << (c)))) != 0)
#define is_amode(a, b) (((a) & (1u << (b))) != 0)

static Adrgen adrmatch(const uint8_t *cnmemonic, uint32_t amode, atype_t am, unsigned int w, Opr_types *opr) {
    switch (am) {
    case A_IMMEDIATE_SIGNED:
    case A_IMMEDIATE:
        if (!is_amode(amode, ADR_IMMEDIATE)) return AG_NONE;
        switch (cnmemonic[(*opr = OPR_IMMEDIATE)]) {
        case 0xE0:
        case 0xC0:
        case 0xA2:
        case 0xA0:  /* cpx cpy ldx ldy */
            return ((longindex && w != 0) || w == 1) ? ((am == A_IMMEDIATE) ? AG_SWORD : AG_SINT) : ((am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR);
        case 0xF4: /* pea/phw #$ffff */
            return (am == A_IMMEDIATE) ? AG_SWORD : AG_SINT;
        case 0x32:
        case 0x42: /* sac sir/wdm */
            if (opcode == c65dtv02.opcode) {
                if (am != A_IMMEDIATE) return AG_NONE;
                return AG_BYTE;
            }
            return (am == A_IMMEDIATE) ? AG_SBYTE : AG_CHAR;
        case 0xC2:
        case 0xE2:
        case 0xEF:  /* sep rep mmu */
            if (opcode == w65816.opcode || opcode == c65el02.opcode) {
                if (am != A_IMMEDIATE) return AG_NONE;
                return AG_BYTE;
            }
            FALL_THROUGH; /* fall through */
        case 0x00:
        case 0x02: /* brk cop */
            return (am == A_IMMEDIATE) ? AG_SBYTE : AG_CHAR;
        default:
            if (is_amode2(amode, ADR_REL, ADR_REL_L)) {
                return (am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR;
            }
            return ((longaccu && w != 0) || w == 1) ? ((am == A_IMMEDIATE) ? AG_SWORD : AG_SINT) : ((am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR);
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_BR: /* lda #$ffff,b */
    case A_BR:
        if (is_amode(amode, ADR_ADDR) && cnmemonic[OPR_ADDR] != 0xF4) {/* pea */
            *opr = OPR_ADDR; return AG_WORD; /* lda $ffff,b */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_KR: /* jmp #$ffff,x */
    case A_KR:
        if (is_amode(amode, ADR_REL)) {
            *opr = OPR_REL; return AG_BYTE; /* bra */
        }
        if (is_amode(amode, ADR_REL_L)) {
            *opr = OPR_REL_L; return AG_RELPB; /* brl */
        }
        if (is_amode(amode, ADR_ADDR_K)) {     /* jmp $ffff, jsr $ffff */
            *opr = OPR_ADDR; return AG_WORD; /* jmp $ffff */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_DR:           /* lda #$ff,d */
    case A_DR:
        if (is_amode(amode, ADR_ZP)) {
            *opr = OPR_ZP; return AG_BYTE;  /* lda $ff,d */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_BR << 4) | A_XR: /* lda #$ffff,b,x */
    case (A_BR << 4) | A_XR:
        if (is_amode(amode, ADR_ADDR_X)) {
            *opr = OPR_ADDR_X; return AG_WORD; /* lda $ffff,b,x */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_XR: /* lda #$ff,d,x */
    case (A_DR << 4) | A_XR:
        if (is_amode(amode, ADR_ZP_X)) {
            *opr = OPR_ZP_X; return AG_BYTE; /* lda $ff,d,x */
        }
        return AG_NONE;
    case A_XR:
        if (is_amode3(amode, ADR_ZP_X, ADR_ADDR_X, ADR_LONG_X)) {
            *opr = OPR_ZP_X; return AG_DB3; /* lda $ff,x lda $ffff,x lda $ffffff,x */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_BR << 4) | A_YR:/* ldx #$ffff,b,y */
    case (A_BR << 4) | A_YR:
        if (is_amode(amode, ADR_ADDR_Y)) {
            *opr = OPR_ADDR_Y; return AG_WORD; /* ldx $ffff,b,y */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_YR:/* ldx #$ff,d,y */
    case (A_DR << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_Y)) {
            *opr = OPR_ZP_Y; return AG_BYTE; /* ldx $ff,d,y */
        }
        return AG_NONE;
    case A_YR:
        if (is_amode2(amode, ADR_ZP_Y, ADR_ADDR_Y)) {
            *opr = OPR_ZP_Y; return AG_DB2; /* lda $ff,y lda $ffff,y */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_SR:           /* lda #$ff,s */
    case A_SR:
        if (is_amode(amode, ADR_ZP_S)) {
            *opr = OPR_ZP_S; return AG_BYTE; /* lda $ff,s */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_RR:           /* lda #$ff,r */
    case A_RR:
        if (is_amode(amode, ADR_ZP_R)) {
            *opr = OPR_ZP_R; return AG_BYTE; /* lda $ff,r */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,d),y */
    case (A_DR << 8) | (A_I << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_I_Y)) {
            *opr = OPR_ZP_I_Y; return AG_BYTE; /* lda ($ff,d),y */
        }
        return AG_NONE;
    case (A_I << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_I_Y)) {
            *opr = OPR_ZP_I_Y; return AG_ZP; /* lda ($ff),y */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_I << 4) | A_ZR:/* lda (#$ff,d),z */
    case (A_DR << 8) | (A_I << 4) | A_ZR:
        if (is_amode(amode, ADR_ZP_I_Z)) {
            *opr = OPR_ZP_I_Z; return AG_BYTE; /* lda ($ff,d),z */
        }
        return AG_NONE;
    case (A_I << 4) | A_ZR:
        if (is_amode(amode, ADR_ZP_I_Z)) {
            *opr = OPR_ZP_I_Z; return AG_ZP; /* lda ($ff),z */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_SR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,s),y */
    case (A_SR << 8) | (A_I << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_S_I_Y)) {
            *opr = OPR_ZP_S_I_Y; return AG_BYTE; /* lda ($ff,s),y */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_RR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,r),y */
    case (A_RR << 8) | (A_I << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_R_I_Y)) {
            *opr = OPR_ZP_R_I_Y; return AG_BYTE; /* lda ($ff,r),y */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_LI << 4) | A_YR:/* lda [#$ff,d],y */
    case (A_DR << 8) | (A_LI << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_LI_Y)) {
            *opr = OPR_ZP_LI_Y; return AG_BYTE; /* lda [$ff,d],y */
        }
        return AG_NONE;
    case (A_LI << 4) | A_YR:
        if (is_amode(amode, ADR_ZP_LI_Y)) {
            *opr = OPR_ZP_LI_Y; return AG_ZP; /* lda [$ff],y */
        }
        return AG_NONE;
    case (A_XR << 4) | A_I:
        if (is_amode(amode, ADR_ADDR_K_X_I)) {/* jmp ($ffff,x) jsr ($ffff,x) */
            *opr = OPR_ADDR_K_X_I; return AG_PB; /* jmp ($ffff,x) */
        }
        if (is_amode(amode, ADR_ZP_X_I)) {
            *opr = OPR_ZP_X_I; return AG_ZP; /* lda ($ff,x) */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_KR << 8) | (A_XR << 4) | A_I: /* jmp (#$ffff,k,x) */
    case (A_KR << 8) | (A_XR << 4) | A_I:
        if (is_amode(amode, ADR_ADDR_K_X_I)) {/* jmp ($ffff,x) jsr ($ffff,x) */
            *opr = OPR_ADDR_K_X_I; return AG_WORD; /* jmp ($ffff,k,x) */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_XR << 4) | A_I:/* lda (#$ff,d,x) */
    case (A_DR << 8) | (A_XR << 4) | A_I:
        if (is_amode(amode, ADR_ZP_X_I)) {
            *opr = OPR_ZP_X_I; return AG_BYTE; /* lda ($ff,d,x) */
        }
        return AG_NONE;
    case A_I:
        if (is_amode(amode, ADR_ADDR_0_I)) {/* jmp ($ffff), jsr ($ffff) */
            *opr = OPR_ADDR_0_I; return AG_B0; /* jmp ($ffff) */
        }
        if (is_amode(amode, ADR_ZP_I)) {
            *opr = OPR_ZP_I; return AG_ZP; /* lda ($ff) */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_I: /* lda (#$ff,d) */
    case (A_DR << 4) | A_I:
        if (is_amode(amode, ADR_ZP_I)) {
            *opr = OPR_ZP_I; return AG_BYTE; /* lda ($ff,d) */
        }
        return AG_NONE;
    case A_LI:
        if (is_amode(amode, ADR_ADDR_0_LI)) { /* jmp [$ffff] */
            *opr = OPR_ADDR_0_LI; return AG_B0; /* jmp [$ffff] */
        }
        if (is_amode(amode, ADR_ZP_LI)) {
            *opr = OPR_ZP_LI; return AG_ZP; /* lda [$ff] */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_LI: /* lda [#$ff,d] */
    case (A_DR << 4) | A_LI:
        if (is_amode(amode, ADR_ZP_LI)) {
            *opr = OPR_ZP_LI; return AG_BYTE; /* lda [$ff,d] */
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_LI << 4) | A_ZR:/* lda [#$ff,d],z */
    case (A_DR << 8) | (A_LI << 4) | A_ZR:
        if (is_amode(amode, ADR_ZP_LI_Z)) {
            *opr = OPR_ZP_LI_Z; return AG_BYTE; /* lda [$ff,d],y */
        }
        return AG_NONE;
    case (A_LI << 4) | A_ZR:
        if (is_amode(amode, ADR_ZP_LI_Z)) {
            *opr = OPR_ZP_LI_Z; return AG_ZP; /* lda [$ff],z */
        }
        return AG_NONE;
    default:
        return AG_NONE;
    }
    return AG_NONE;
}

static int register_generic(int prm, int c) {
    switch (c) {
    case 'a':
        if (prm == current_cpu->ldr) return current_cpu->lda;
        if (prm == current_cpu->str) return current_cpu->sta;
        if (prm == current_cpu->cmp) return current_cpu->cpa;
        if (prm == current_cpu->adc) return current_cpu->adc;
        if (prm == current_cpu->sbc) return current_cpu->sbc;
        if (prm == current_cpu->and_) return current_cpu->and_;
        if (prm == current_cpu->orr) return current_cpu->ora;
        if (prm == current_cpu->eor) return current_cpu->eor;
        if (prm == current_cpu->bit) return current_cpu->bit;
        if (prm == current_cpu->tsb) return current_cpu->tsb;
        if (prm == current_cpu->trb) return current_cpu->trb;
        break;
    case 'q':
        if (prm == current_cpu->ldr) return current_cpu->ldq;
        if (prm == current_cpu->str) return current_cpu->stq;
        if (prm == current_cpu->cmp) return current_cpu->cpq;
        if (prm == current_cpu->adc) return current_cpu->adq;
        if (prm == current_cpu->sbc) return current_cpu->sbq;
        if (prm == current_cpu->and_) return current_cpu->anq;
        if (prm == current_cpu->orr) return current_cpu->orq;
        if (prm == current_cpu->eor) return current_cpu->eoq;
        if (prm == current_cpu->bit) return current_cpu->btq;
        break;
    case 'x':
        if (prm == current_cpu->ldr) return current_cpu->ldx;
        if (prm == current_cpu->str) return current_cpu->stx;
        if (prm == current_cpu->cmp) return current_cpu->cpx;
        break;
    case 'y':
        if (prm == current_cpu->ldr) return current_cpu->ldy;
        if (prm == current_cpu->str) return current_cpu->sty;
        if (prm == current_cpu->cmp) return current_cpu->cpy;
        break;
    case 'z':
        if (prm == current_cpu->ldr) return current_cpu->ldz;
        if (prm == current_cpu->str) return current_cpu->stz;
        if (prm == current_cpu->cmp) return current_cpu->cpz;
        break;
    default:
        break;
    }
    return -1;
}

static void qprefix(int prm, linepos_t epoint) {
    if (prm == current_cpu->adq
        || prm == current_cpu->anq
        || prm == current_cpu->ard
        || prm == current_cpu->btq
        || prm == current_cpu->cpq
        || prm == current_cpu->ded
        || prm == current_cpu->eoq
        || prm == current_cpu->ind
        || prm == current_cpu->ldq
        || prm == current_cpu->orq
        || prm == current_cpu->rld
        || prm == current_cpu->rrd
        || prm == current_cpu->sbq
        || prm == current_cpu->asd
        || prm == current_cpu->lsd
        || prm == current_cpu->stq) {
        dump_instr(0x42, 0, 0, epoint);
        dump_instr(0x42, 0, 0, epoint);
    }
}

MUST_CHECK Error *instruction(int prm, unsigned int w, struct values_s *vals, argcount_t argc, linepos_t epoint) {
    Adrgen adrgen;
    Opr_types opr;
    Reg_types reg;
    const uint8_t *cnmemonic; /* current nmemonic */
    uint32_t amode;
    unsigned int ln;
    uint8_t cod, longbranch;
    uint32_t adr;
    uval_t uval;
    Obj *val;
    struct linepos_s *epoint2;
    Error *err;
    atype_t am;
retry:
    cnmemonic = opcode_table[opcode[prm] & 0xff];
    amode = opcode_table_modes[opcode[prm] >> 8];
    longbranch = 0; reg = REG_A; adr = 0;

    switch (argc) {
    case 0:
    retry0:
        if (is_amode(amode, ADR_IMPLIED)) {
            if (diagnostics.implied_reg && is_amode(amode, ADR_REG)) err_msg_implied_reg(epoint, mnemonic[prm]);
            if (opcode == c45gs02.opcode && (prm == current_cpu->inq || prm == current_cpu->deq)) {
                dump_instr(0x42, 0, 0, epoint);
                dump_instr(0x42, 0, 0, epoint);
            }
            adrgen = AG_IMP; opr = OPR_IMPLIED;
            break;
        }
        return err_addressing(A_NONE, epoint, prm);
    case 1:
        epoint2 = &vals[0].epoint;
        val = vals[0].val;
        if (val->obj->iterable) {
            struct iter_s iter;
            struct star_s *s, *stree_old;
            if (is_amode(amode, ADR_REL)) {
                s = new_star(vline); stree_old = star_tree;
                if (s->pass != 0 && s->addr != star) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = false;
                }
                s->addr = star;
                star_tree->vline = vline; star_tree = s; vline = s->vline;
            } else { s = NULL; stree_old = NULL; }
            iter.data = vals[0].val; iter.data->obj->getiter(&iter);
            err = NULL;
            while ((vals[0].val = iter.next(&iter)) != NULL) {
                Error *err2 = instruction(prm, w, vals, argc, epoint);
                if (err != NULL) err_msg_output_and_destroy(err);
                err = err2;
                if (s != NULL) vline++;
            }
            vals[0].val = iter.data;
            iter_destroy(&iter);
            if (s != NULL) { s->vline = vline; star_tree = stree_old; vline = star_tree->vline; }
            return err;
        }
        am = val->obj->address(val);
    retry1:
        if (am != A_NONE) {
            adrgen = adrmatch(cnmemonic, amode, am, w, &opr);
            if (adrgen != AG_NONE) {
                switch (opr) {
                case OPR_REL:
                    ln = 1; longbranch = 0;
                    goto justrel2;
                case OPR_IMMEDIATE:
                    if (is_amode(amode, ADR_REL)) {
                        ln = 1; longbranch = 0; opr = OPR_REL;
                        goto immediaterel;
                    }
                    if (is_amode(amode, ADR_REL_L)) {
                        if (w == 0) err_msg2(ERROR__NO_BYTE_ADDR, &mnemonic[prm], epoint2);
                        ln = 2; longbranch = 0; opr = OPR_REL_L;
                        goto immediaterel;
                    }
                    if (pline[epoint2->pos] == '#') epoint2->pos++;
                    break;
                case OPR_ZP_I_Y:
                case OPR_ZP_S_I_Y:
                case OPR_ZP_X_I:
                case OPR_ZP_I:
                    if (pline[epoint2->pos] == '(') epoint2->pos++;
                    if (opcode == c45gs02.opcode) qprefix(prm, epoint);
                    break;
                case OPR_ZP_LI_Y:
                case OPR_ZP_LI:
                    if (pline[epoint2->pos] == '[') epoint2->pos++;
                    if (opcode == c45gs02.opcode) {
                        qprefix(prm, epoint);
                        dump_instr(0xea, 0, 0, epoint);
                    }
                    break;
                case OPR_ZP:
                case OPR_ZP_X:
                case OPR_ADDR:
                    if (opcode == c45gs02.opcode) qprefix(prm, epoint);
                    break;
                default:
                    break;
                }
                break;
            }
            if (am > MAX_ADDRESS_MASK) return new_error(ERROR__ADDR_COMPLEX, epoint2);
            return err_addressing(am, epoint2, prm);
        }
        if (val->obj == REGISTER_OBJ) {
            Register *cpureg = Register(val);
            if (cpureg->len == 1) {
                const char *ind = strchr(reg_names, cpureg->data[0]);
                if (ind != NULL) {
                    cod = cnmemonic[(opr = OPR_REG)];
                    if (cod != 0) {
                        reg = (Reg_types)(ind - reg_names);
                        if (regopcode_table[cod][reg] != ____) {
                            if (reg == REG_Q && opcode == c45gs02.opcode) {
                                dump_instr(0x42, 0, 0, epoint);
                                dump_instr(0x42, 0, 0, epoint);
                            }
                            adrgen = AG_IMP;
                            break;
                        }
                    }
                    if ((prm == current_cpu->sta && cpureg->data[0] == 'a')
                               || (prm == current_cpu->stx && cpureg->data[0] == 'x')
                               || (prm == current_cpu->sty && cpureg->data[0] == 'y')
                               || (prm == current_cpu->stz && cpureg->data[0] == 'z')) 
                    {
                        dump_instr(0, 0, -1, epoint);
                        return NULL;
                    }
                }
            }
            goto noregister;
        }
        if (is_amode(amode, ADR_REL)) {
            struct star_s *s;
            uint16_t xadr;
            uval_t oadr;
            bool crossbank, invalid;
            Obj *oval;
            ln = 1; opr = OPR_REL;
            longbranch = 0;
            if (false) {
        justrel2:
                invalid = touaddress(val, &uval, 16, epoint2);
                if (invalid) uval = current_address->l_address + 1 + ln;
                uval &= 0xffff;
                uval |= current_address->l_address & ~(uval_t)0xffff;
                crossbank = false;
            } else {
        justrel:
                invalid = touaddress(val, &uval, all_mem_bits, epoint2);
                if (invalid) {
                    uval = current_address->l_address + 1 + ln;
                    crossbank = false;
                } else {
                    uval &= all_mem;
                    crossbank = (current_address->l_address ^ uval) > 0xffff;
                }
            }
            xadr = (uint16_t)adr;

            oadr = uval;
            oval = val->obj == ADDRESS_OBJ ? Address(val)->val : val;
            if (oval->obj == CODE_OBJ && pass != Code(oval)->apass && !is_amode(amode, ADR_REL_L)) { /* not for 65CE02! */
                s = new_star(vline + 1);
                adr = s->pass != 0 ? (uint16_t)(uval - s->addr) : (uint16_t)(uval - current_address->l_address - 1 - ln);
            } else {
                s = invalid ? new_star(vline + 1) : NULL;
                adr = (uint16_t)(uval - current_address->l_address - 1 - ln);
            }
            if (false) {
                bool longpossible;
        immediaterel:
                longpossible = is_amode(amode, ADR_REL_L) && w != 0;
                if (adrgen == AG_SBYTE) {
                    invalid = touaddress(val, &uval, longpossible ? 16 : 8, epoint2);
                } else {
                    invalid = toiaddress(val, (ival_t *)&uval, longpossible ? 16 : 8, epoint2);
                }
                if (invalid) uval = 0;
                if (!longpossible && (uval & 0x80) != 0) uval |= ~(uval_t)0xff;
                crossbank = false;
                xadr = (uint16_t)adr;
                s = NULL;
                adr = (uint16_t)uval;
                uval = (uval + current_address->l_address + 1 + ln) & 0xffff;
                uval |= current_address->l_address & ~(uval_t)0xffff;
                oadr = uval;
                oval = val;
            }
            if ((adr<0xFF80 && adr>0x007F) || crossbank || w == 1 || w == 2) {
                if (is_amode(amode, ADR_REL_L) && !crossbank && (w == 3 || w == 1)) { /* 65CE02 long branches */
                    opr = OPR_REL_L;
                    ln = 2;
                } else if (arguments.longbranch && !is_amode(amode, ADR_ADDR) && w == 3) { /* fake long branches */
                    if ((cnmemonic[OPR_REL] & 0x1f) == 0x10) {/* bxx branch */
                        struct longjump_s *lj;
                        int opc;
                        if (oval->obj == CODE_OBJ) {
                            lj = new_longjump(&current_section->longjump, uval, Code(oval));
                            if (lj->defpass == pass) {
                                if ((current_address->l_address ^ lj->dest) <= 0xffff) {
                                    uint32_t adrk = (uint16_t)(lj->dest - current_address->l_address - 2);
                                    if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                        adr = adrk;
                                        if (((current_address->l_address + 2) & 0xff00) != (lj->dest & 0xff00)) {
                                            int diff = (int8_t)oadr;
                                            if (diff >= 0) diff++;
                                            if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, &diff, epoint2);
                                            else if (diagnostics.branch_page) err_msg_branch_page(diff, epoint2);
                                        }
                                        goto branchok;
                                    }
                                }
                            }
                            opc = code_opcode(Code(oval));
                            if (opc != 0x60 && opc != 0x40 && (opc != 0x6B || opcode != w65816.opcode)) opc = -1; /* rts, rti, rtl */
                        } else {
                            lj = NULL;
                            opc = -1;
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[OPR_REL]);
                        dump_instr(cnmemonic[OPR_REL] ^ 0x20, opc < 0 ? 3 : 1, 1, epoint);
                        if (lj != NULL) {
                            lj->dest = current_address->l_address;
                            lj->defpass = pass;
                        }
                        if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                        if (diagnostics.optimize) cpu_opt_long_branch(0xea);
                        if (opc < 0) {
                            err = instruction((current_cpu->brl >= 0 && !longbranchasjmp && !crossbank) ? current_cpu->brl : current_cpu->jmp, w, vals, argc, epoint);
                        } else {
                            err = NULL;
                            dump_instr((uint8_t)opc, 1, 0, epoint);
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(0);
                        goto branchend;
                    }
                    if (opr == OPR_BIT_ZP_REL) {
                        struct longjump_s *lj;
                        int opc;
                        if (crossbank) {
                            err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
                            goto branchok;
                        }
                        if (oval->obj == CODE_OBJ) {
                            lj = new_longjump(&current_section->longjump, uval, Code(oval));
                            if (lj->defpass == pass) {
                                if ((current_address->l_address ^ lj->dest) <= 0xffff) {
                                    uint32_t adrk = (uint16_t)(lj->dest - current_address->l_address - 3);
                                    if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                        adr = adrk;
                                        if (((current_address->l_address + 3) & 0xff00) != (lj->dest & 0xff00)) {
                                            int diff = (int8_t)oadr;
                                            if (diff >= 0) diff++;
                                            if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, &diff, epoint2);
                                            else if (diagnostics.branch_page) err_msg_branch_page(diff, epoint2);
                                        }
                                        goto branchok;
                                    }
                                }
                            }
                            opc = code_opcode(Code(oval));
                            if (opc != 0x60 && opc != 0x40) opc = -1; /* rts, rti */
                        } else {
                            lj = NULL;
                            opc = -1;
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[OPR_BIT_ZP_REL] ^ longbranch);
                        dump_instr(cnmemonic[OPR_BIT_ZP_REL] ^ 0x80 ^ longbranch, xadr | (opc < 0 ? 0x300u : 0x100u), 2, epoint);
                        if (lj != NULL) {
                            lj->dest = current_address->l_address;
                            lj->defpass = pass;
                        }
                        if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                        if (diagnostics.optimize) cpu_opt_long_branch(0xea);
                        if (opc < 0) {
                            err = instruction(current_cpu->jmp, w, &vals[2], argc - 2, epoint);
                        } else {
                            err = NULL;
                            dump_instr((uint8_t)opc, 1, 0, epoint);
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(0);
                        goto branchend;
                    } else {/* bra */
                        if (cnmemonic[OPR_REL] == 0x82 && opcode == c65el02.opcode) { /* not a branch ! */
                            int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                            if (crossbank) {
                                err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
                            } else err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint2); /* rer not a branch */
                        } else { /* bra -> jmp or brl */
                        asjmpbrl:
                            if (oval->obj == CODE_OBJ) {
                                int opc = code_opcode(Code(oval));
                                if (opc == 0x60 || opc == 0x40 || (opc == 0x6B && opcode == w65816.opcode)) { /* rts, rti, rtl */
                                    struct longjump_s *lj = new_longjump(&current_section->longjump, uval, Code(oval));
                                    lj->dest = current_address->l_address;
                                    lj->defpass = pass;
                                    dump_instr((uint8_t)opc, 1, 0, epoint);
                                    err = NULL;
                                    goto branchend;
                                }
                            }
                            if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                            err = instruction((current_cpu->brl >= 0 && !longbranchasjmp) ? current_cpu->brl : current_cpu->jmp, w, vals, argc, epoint);
                        branchend:
                            if (s != NULL) {
                                address_t st = current_address->l_address;
                                if (s->pass != 0 && s->addr != st) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = false;
                                }
                                s->addr = st;
                            }
                            return err;
                        }
                    }
                } else if (is_amode(amode, ADR_ADDR)) { /* gcc */
                    goto asjmpbrl; /* gcc -> jmp or brl */
                } else { /* too long */
                    if (w != 3 && w != 0) {
                        err_msg2((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, &mnemonic[prm], epoint2);
                    } else if (crossbank) {
                        err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
                    } else {
                        int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                        err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint2);
                    }
                }
            } else if (!invalid) { /* short */
                if (is_amode(amode, ADR_ADDR) && w == 3 && oval->obj == CODE_OBJ) { /* gcc */
                    int opc;
                    bool after = pass != Code(oval)->apass;
                    if (after && adr == 0) {
                        dump_instr(cnmemonic[OPR_REL], 0, -1, epoint);
                        err = NULL;
                        goto branchend;
                    }
                    opc = code_opcode(Code(oval));
                    if (opc == 0x60 || opc == 0x40 || (opc == 0x6B && opcode == w65816.opcode)) { /* rts, rti, rtl */
                        dump_instr((uint8_t)opc, 1, 0, epoint);
                        err = NULL;
                        goto branchend;
                    }
                    if (after) {
                        if ((cnmemonic[OPR_REL] & 0x1f) == 0x10) {
                            if (adr == 1) {
                                if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[OPR_REL] | 0x100U);
                                dump_instr(cnmemonic[OPR_REL] ^ 0x20, 1, 0, epoint);
                                if (diagnostics.optimize) cpu_opt_long_branch(0);
                                err = NULL;
                                goto branchend;
                            } else if (adr == 2 && (opcode == c65ce02.opcode || opcode == c4510.opcode || opcode == c45gs02.opcode)) {
                                if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[OPR_REL] | 0x100U);
                                dump_instr(cnmemonic[OPR_REL] ^ 0x23, 2, 0, epoint);
                                if (diagnostics.optimize) cpu_opt_long_branch(0);
                                err = NULL;
                                goto branchend;
                            }
                        } else if (cnmemonic[OPR_REL] == 0x80 && adr == 1 && (opcode == r65c02.opcode || opcode == w65c02.opcode)) {
                            dump_instr(0x82, 1, 0, epoint);
                            err = NULL;
                            goto branchend;
                        }
                    }
                }
                if (((current_address->l_address + 1 + ln) & 0xff00) != (oadr & 0xff00)) {
                    int diff = (int8_t)oadr;
                    if (diff >= 0) diff++;
                    if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, &diff, epoint2);
                    else if (diagnostics.branch_page) err_msg_branch_page(diff, epoint2);
                }
            }
        branchok:
            if (s != NULL) {
                address_t st = (current_address->l_address + 1 + ln) & all_mem;
                if (s != NULL && s->pass != 0 && s->addr != st) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = false;
                }
                s->addr = st;
            }
            if (opr == OPR_BIT_ZP_REL) adr = xadr | (adr << 8);
            adrgen = AG_NONE; break;
        }
        if (is_amode(amode, ADR_REL_L)) {
            adrgen = AG_RELL; opr = OPR_REL_L; /* brl */
            break;
        }
        if (is_amode(amode, ADR_ADDR_K)) {
            adrgen = AG_PB; opr = OPR_ADDR; /* jsr $ffff, jmp */
            break;
        }
        if (cnmemonic[OPR_ADDR] == 0xF4) {
            adrgen = AG_WORD; opr = OPR_ADDR; /* pea $ffff */
            break;
        }
        if (is_amode3(amode, ADR_ZP, ADR_ADDR, ADR_LONG)) {
            if (opcode == c45gs02.opcode) qprefix(prm, epoint);
            adrgen = AG_DB3; opr = OPR_ZP; /* lda $ff lda $ffff lda $ffffff */
            break;
        }
        goto unknown;
    case 2:
        epoint2 = &vals[0].epoint;
        if (vals[0].val->obj->iterable || vals[1].val->obj->iterable) {
            argcount_t j;
            size_t ln2;
            struct elements_s {
                Obj *oval;
                struct iter_s iter;
            } elements[4];
        broadcast:
            ln2 = 1;
            err = NULL;
            for (j = 0; j < argc; j++) {
                const Type *objt = vals[j].val->obj;
                if (objt->iterable) {
                    struct iter_s *iter = &elements[j].iter;
                    elements[j].oval = iter->data = vals[j].val; objt->getiter(iter);
                    if (iter->len == 1) {
                        vals[j].val = iter->next(iter);
                    } else if (iter->len != ln2) {
                        if (ln2 != 1) {
                            err = new_error(ERROR_CANT_BROADCAS, &vals[j].epoint);
                            err->u.broadcast.v1 = ln2;
                            err->u.broadcast.v2 = iter->len;
                            for (j++; j < argc; j++) {
                                elements[j].oval = NULL;
                            }
                            break;
                        }
                        ln2 = iter->len;
                    }
                } else {
                    elements[j].oval = NULL;
                }
            }
            if (err == NULL) {
                struct star_s *s, *stree_old;
                if (is_amode(amode, ADR_REL)) {
                    s = new_star(vline); stree_old = star_tree;
                    if (s->pass != 0 && s->addr != star) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = false;
                    }
                    s->addr = star;
                    star_tree->vline = vline; star_tree = s; vline = s->vline;
                } else { s = NULL; stree_old = NULL; }
                while (ln2 != 0) {
                    Error *err2;
                    for (j = 0; j < argc; j++) {
                        if (elements[j].oval == NULL) continue;
                        if (elements[j].iter.len != 1) vals[j].val = elements[j].iter.next(&elements[j].iter);
                    }
                    err2 = instruction(prm, w, vals, argc, epoint);
                    if (err != NULL) err_msg_output_and_destroy(err);
                    err = err2;
                    ln2--;
                    if (s != NULL) vline++;
                }
                if (s != NULL) { s->vline = vline; star_tree = stree_old; vline = star_tree->vline; }
            }
            for (j = 0; j < argc; j++) {
                if (elements[j].oval == NULL) continue;
                vals[j].val = elements[j].oval;
                iter_destroy(&elements[j].iter);
            }
            return err;
        }
        if (vals[0].val->obj == REGISTER_OBJ) {
            const Register *r1 = Register(vals[0].val);
            if (r1->len == 1) {
                const Register *r2;
                int r1name = r1->data[0];
                int nprm = register_generic(prm, r1name);
                if (nprm >= 0) {
                    prm = nprm; 
                    vals++; 
                    argc--;
                    goto retry;
                }
                if (vals[1].val->obj == REGISTER_OBJ) {
                    r2 = Register(vals[1].val);
                    if (r2->len == 1) {
                        int r2name = r2->data[0];
                        nprm = -1;
                        if (r1name == 'd' && r2name == 'a') nprm = current_cpu->tcd;
                        if (r1name == 'i' && r2name == 'x') nprm = current_cpu->txi;
                        if (r1name == 'r' && r2name == 'x') nprm = current_cpu->txr;
                        if (nprm >= 0) {
                            prm = nprm;
                            cnmemonic = opcode_table[opcode[prm] & 0xff];
                            amode = opcode_table_modes[opcode[prm] >> 8];
                            goto retry0;
                        }
                        if (prm == current_cpu->str && r1name == r2name && r1name >= 'a' && r1name <= 'z' && ((current_cpu->registers >> (r1name - 'a')) & 1) != 0) {
                            dump_instr(0, 0, -1, epoint);
                            return NULL;
                        }
                    }
                }
            }
            goto noregister;
        }
        if (vals[1].val->obj == REGISTER_OBJ) {
            am = register_to_indexing(Register(vals[1].val));
            if (am != A_NONE) {
                val = vals[0].val;
                am |= val->obj->address(val) << 4;
                goto retry1;
            }
        }
        if (is_amode(amode, ADR_MOVE)) {
            if (w != 3 && w != 1) return err_addressize((w == 0) ? ERROR__NO_BYTE_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
            val = vals[0].val;
            if (touaddress(val, &uval, 8, epoint2)) {}
            else {
                am = val->obj->address(val);
                if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                else adr = (uval & 0xff) << 8;
            }
            epoint2 = &vals[1].epoint;
            val = vals[1].val;
            if (touaddress(val, &uval, 8, epoint2)) {}
            else {
                am = val->obj->address(val);
                if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                else adr |= uval & 0xff;
            }
            ln = 2;
            adrgen = AG_NONE; opr = OPR_MOVE;
            break;
        }
        if (is_amode(amode, ADR_BIT_ZP)) {
            if (w != 3 && w != 0) return err_addressize((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
            if (touval(vals[0].val, &uval, 3, epoint2)) {}
            else longbranch = ((uval & 7) << 4) & 0x70;
            val = vals[1].val;
            epoint2 = &vals[1].epoint;
            am = val->obj->address(val);
            if (am == A_DR) {
                adrgen = AG_BYTE;
            } else {
                if (am != A_NONE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                adrgen = AG_ZP;
            }
            opr = OPR_BIT_ZP;
            break;
        }
        goto unknown;
    case 3:
        epoint2 = &vals[0].epoint;
        if (vals[0].val->obj->iterable || vals[1].val->obj->iterable || vals[2].val->obj->iterable) {
            goto broadcast;
        }
        if (vals[0].val->obj == REGISTER_OBJ) {
            if (Register(vals[0].val)->len == 1) {
                int nprm = register_generic(prm, Register(vals[0].val)->data[0]);
                if (nprm >= 0) {
                    prm = nprm; 
                    vals++; 
                    argc--;
                    goto retry;
                }
            }
            goto noregister;
        }
        if (vals[1].val->obj == REGISTER_OBJ && vals[2].val->obj == REGISTER_OBJ) {
            atype_t am2 = register_to_indexing(Register(vals[2].val));
            am = register_to_indexing(Register(vals[1].val));
            if (am != A_NONE && am2 != A_NONE) {
                val = vals[0].val;
                am |= val->obj->address(val) << 4;
                am = (am << 4) | am2;
                goto retry1;
            }
        }
        if (is_amode(amode, ADR_BIT_ZP_REL)) {
            if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
            if (touval(vals[0].val, &uval, 3, epoint2)) {}
            else longbranch = ((uval & 7) << 4) & 0x70;
            val = vals[1].val;
            epoint2 = &vals[1].epoint;
            am = val->obj->address(val);
            if (am == A_DR) {
                if (touaddress(val, &uval, 8, epoint2)) {}
                else adr = uval & 0xff;
            } else {
                if (am != A_NONE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                else if (touaddress(val, &uval, all_mem_bits, epoint2)) {}
                else {
                    uval &= all_mem;
                    if (uval <= 0xffff) {
                        adr = (uint16_t)(uval - dpage);
                        if (adr > 0xff || dpage > 0xffff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                    } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                }
            }
            val = vals[2].val;
            epoint2 = &vals[2].epoint;
            ln = 2; opr = OPR_BIT_ZP_REL;
            am = val->obj->address(val);
            if (am == A_KR) {
                goto justrel2;
            }
            if (am == A_IMMEDIATE) {
                adrgen = AG_SBYTE;
                goto immediaterel;
            }
            if (am == A_IMMEDIATE_SIGNED) {
                adrgen = AG_CHAR;
                goto immediaterel;
            }
            goto justrel;
        }
        goto unknown;
    case 4:
        epoint2 = &vals[0].epoint;
        if (vals[0].val->obj->iterable || vals[1].val->obj->iterable || vals[2].val->obj->iterable || vals[3].val->obj->iterable) {
            goto broadcast;
        }
        if (vals[0].val->obj == REGISTER_OBJ) {
            if (Register(vals[0].val)->len == 1) {
                int nprm = register_generic(prm, Register(vals[0].val)->data[0]);
                if (nprm >= 0) {
                    prm = nprm; 
                    vals++; 
                    argc--;
                    goto retry;
                }
            }
            goto noregister;
        }
        FALL_THROUGH; /* fall through */
    default:
    unknown:
        {
            argcount_t j;
            for (j = 0; j < argc; j++) {
                Obj *v = vals[j].val;
                if (v->obj == ERROR_OBJ) return Error(val_reference(v));
            }
            if (prm == current_cpu->ldr || prm == current_cpu->str || prm == current_cpu->orr) {
                struct values_s *v = &vals[0];
                am = v->val->obj->address(v->val);
                if (am != A_NONE) {
                    if (am > MAX_ADDRESS_MASK) return new_error(ERROR__ADDR_COMPLEX, &v->epoint);
                    return err_addressing(am, &v->epoint, prm);
                }
                err = new_error(ERROR____WRONG_TYPE, &v->epoint);
                err->u.otype.t1 = v->val->obj;
                err->u.otype.t2 = REGISTER_OBJ;
                return err;
            }
            err = new_error(ERROR___NO_LOT_OPER, epoint);
            err->u.opers.num = j;
            err->u.opers.cod = mnemonic[prm];
            return err;
        }
    noregister:
        err = new_error(ERROR___NO_REGISTER, epoint2);
        err->u.reg.reg = ref_register(Register(vals[0].val));
        err->u.reg.cod = mnemonic[prm];
        return err;
    }
    switch (adrgen) {
    case AG_ZP: /* zero page address only */
        {
            uval_t uval2;
            Obj *val2;
            if (w != 3 && w != 0) return err_addressize((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
            ln = 1;
            val2 = (val->obj == ADDRESS_OBJ) ? Address(val)->val : val;
            if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                if (tocode_uaddress(val2, &uval, &uval2, epoint2)) break;
            } else {
                if (touaddress(val, &uval, all_mem_bits, epoint2)) break;
                uval2 = uval;
            }
            if (opcode == c65el02.opcode || opcode == w65816.opcode) uval2 = uval;
            uval2 &= all_mem;
            if (uval2 <= 0xffff) {
                adr = uval - dpage;
                if (dpage > 0xffff || (uint16_t)(uval2 - dpage) > 0xff) err_msg2(ERROR____NOT_DIRECT, val2, epoint2);
                else if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                    if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                } else {
                    if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                }
                break;
            }
            err_msg2(ERROR_____NOT_BANK0, val2, epoint2);
            break;
        }
    case AG_B0: /* bank 0 address only */
        {
            uval_t uval2;
            Obj *val2;
            if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
            ln = 2;
            val2 = (val->obj == ADDRESS_OBJ) ? Address(val)->val : val;
            if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                if (tocode_uaddress(val2, &uval, &uval2, epoint2)) break;
            } else {
                if (touaddress(val, &uval, all_mem_bits, epoint2)) break;
                uval2 = uval;
            }
            uval2 &= all_mem;
            if (uval2 <= 0xffff) {
                adr = uval;
                if (uval > 0xffff) err_msg_bank0_wrap(epoint2);
                if (diagnostics.jmp_bug && cnmemonic[opr] == 0x6c && opcode != w65816.opcode && opcode != c65c02.opcode && opcode != r65c02.opcode && opcode != w65c02.opcode && opcode != c65ce02.opcode && opcode != c4510.opcode && opcode != c45gs02.opcode && opcode != c65el02.opcode && (~adr & 0xff) == 0) err_msg_jmp_bug(val2, epoint2);/* jmp ($xxff) */
                break;
            }
            err_msg2(ERROR_____NOT_BANK0, val2, epoint2);
            break;
        }
    case AG_PB: /* address in program bank */
        {
            uval_t uval2;
            Obj *val2;
            if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
            ln = 2;
            val2 = (val->obj == ADDRESS_OBJ) ? Address(val)->val : val;
            if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                if (tocode_uaddress(val2, &uval, &uval2, epoint2)) break;
            } else {
                if (touaddress(val, &uval, all_mem_bits, epoint2)) break;
                uval2 = uval;
            }
            if ((current_address->l_address ^ uval2) <= 0xffff) {
                adr = uval;
                if ((current_address->l_address ^ uval) > 0xffff) err_msg_pbank_wrap(epoint2);
                break;
            }
            err_msg2(ERROR_CANT_CROSS_BA, val2, epoint2);
            break;
        }
    case AG_CHAR:
    case AG_SBYTE: /* byte only */
    case AG_BYTE: /* byte only */
        if (w != 3 && w != 0) return err_addressize((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
        ln = 1;
        if (adrgen == AG_CHAR) {
            if (toiaddress(val, (ival_t *)&uval, 8, epoint2)) break;
        } else {
            if (touaddress(val, &uval, 8, epoint2)) {
                if (adrgen == AG_SBYTE && diagnostics.pitfalls && val != none_value) {
                    err = val->obj->iaddress(val, (ival_t *)&uval, 8, epoint2);
                    if (err != NULL) val_destroy(Obj(err));
                    else err_msg_immediate_note(epoint2);
                }
                break;
            }
        }
        adr = uval & 0xff;
        if (autosize && (opcode == c65el02.opcode || opcode == w65816.opcode)) {
            switch (cnmemonic[opr]) {
            case 0xC2:
                if ((adr & 0x10) != 0) longindex = true;
                if ((adr & 0x20) != 0) longaccu = true;
                break;
            case 0xE2:
                if ((adr & 0x10) != 0) longindex = false;
                if ((adr & 0x20) != 0) longaccu = false;
                break;
            }
        }
        break;
    case AG_DB2: /* 2 choice data bank */
    case AG_DB3: /* 3 choice data bank */
        {
            uval_t uval2;
            Obj *val2 = (val->obj == ADDRESS_OBJ) ? Address(val)->val : val;
            if (opr == OPR_ZP_X) amode >>= ADR_ZP_X - ADR_ZP;
            else if (opr == OPR_ZP_Y) amode >>= ADR_ZP_Y - ADR_ZP;

            if (w == 3) {/* auto length */
                if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                    if (tocode_uaddress(val2, &uval, &uval2, epoint2)) w = is_amode(amode, ADR_ADDR) ? 1 : 0;
                } else {
                    if (touaddress(val, &uval, all_mem_bits, epoint2)) w = is_amode(amode, ADR_ADDR) ? 1 : 0;
                    else uval2 = uval;
                }
                if (w == 3) {/* auto length */
                    uval_t uval3;
                    uval2 &= all_mem;
                    uval3 = (opr == OPR_ZP || opcode == c65el02.opcode || opcode == w65816.opcode) ? (uval & all_mem) : uval2;
                    if (diagnostics.altmode) {
                        if (uval3 <= 0xffff && dpage <= 0xffff && (uint16_t)(uval3 - dpage) <= 0xff) w = 0;
                        else if (databank == ((uval & all_mem) >> 16) || adrgen != AG_DB3) w = 1;
                        else w = 2;
                    }

                    if (is_amode(amode, ADR_ZP) && uval3 <= 0xffff && dpage <= 0xffff && (uint16_t)(uval3 - dpage) <= 0xff) {
                        if (diagnostics.immediate && opr == OPR_ZP && is_amode(amode, ADR_IMMEDIATE) && (val->obj != CODE_OBJ || Code(val)->memblocks->enumeration) && val->obj != ADDRESS_OBJ) err_msg2(ERROR_NONIMMEDCONST, NULL, epoint2);
                        else if (w != 3 && w != 0) err_msg_address_mismatch(opr-0, opr-w, epoint2);
                        adr = uval - dpage; w = 0;
                        if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                            if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                        } else {
                            if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                        }
                    } else if (is_amode(amode, ADR_ADDR) && databank == ((uval & all_mem) >> 16)) {
                        if (w != 3 && w != 1) err_msg_address_mismatch(opr-1, opr-w, epoint2);
                        adr = uval; w = 1;
                        if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    } else if (adrgen == AG_DB3 && is_amode(amode, ADR_LONG)) {
                        if (w != 3 && w != 2) err_msg_address_mismatch(opr-2, opr-w, epoint2);
                        adr = uval; w = 2;
                        if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    } else {
                        w = is_amode(amode, ADR_ADDR) ? 1 : 0;
                        err_msg2((w != 0) ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val2, epoint2);
                    }
                }
            } else {
                uval_t uval3;
                if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                    if (tocode_uaddress(val2, &uval, &uval2, epoint2)) goto err;
                } else {
                    if (touaddress(val, &uval, all_mem_bits, epoint2)) goto err;
                    uval2 = uval;
                }

                switch (w) {
                case 0:
                    if (!is_amode(amode, ADR_ZP)) return err_addressize(ERROR__NO_BYTE_ADDR, epoint2, prm);
                    uval2 &= all_mem;
                    uval3 = (opcode == c65el02.opcode || opcode == w65816.opcode) ? (uval & all_mem) : uval2;
                    if (uval3 <= 0xffff) {
                        adr = uval - dpage;
                        if (dpage > 0xffff || (uint16_t)(uval3 - dpage) > 0xff) err_msg2(ERROR____NOT_DIRECT, val2, epoint2);
                        else if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                            if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                        } else {
                            if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                        }
                        break;
                    }
                    err_msg2(ERROR_____NOT_BANK0, val2, epoint2);
                    break;
                case 1:
                    if (!is_amode(amode, ADR_ADDR)) return err_addressize(ERROR__NO_WORD_ADDR, epoint2, prm);
                    uval &= all_mem;
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val2, epoint2);
                    else if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    break;
                case 2:
                    if (adrgen == AG_DB3 && is_amode(amode, ADR_LONG)) {
                        adr = uval & all_mem;
                        if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                        break;
                    }
                    FALL_THROUGH; /* fall through */
                default:
                    return err_addressize(ERROR__NO_LONG_ADDR, epoint2, prm);
                }
            }
        err:
            opr = (Opr_types)(opr - w); ln = w + 1;
            break;
        }
    case AG_SINT:
    case AG_SWORD:
    case AG_WORD: /* word only */
        if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
        ln = 2;
        if (adrgen == AG_SINT) {
            if (toiaddress(val, (ival_t *)&uval, 16, epoint2)) break;
        } else {
            if (touaddress(val, &uval, 16, epoint2)) {
                if (adrgen == AG_SWORD && diagnostics.pitfalls && val != none_value) {
                    err = val->obj->iaddress(val, (ival_t *)&uval, 16, epoint2);
                    if (err != NULL) val_destroy(Obj(err));
                    else err_msg_immediate_note(epoint2);
                }
                break;
            }
        }
        adr = uval & 0xffff;
        break;
    case AG_RELPB:
        if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
        ln = 2;
        if (touaddress(val, &uval, 16, epoint2)) break;
        uval &= 0xffff;
        adr = uval - current_address->l_address - ((opcode != c65ce02.opcode && opcode != c4510.opcode && opcode != c45gs02.opcode) ? 3 : 2);
        break;
    case AG_RELL:
        if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
        ln = 2;
        if (touaddress(val, &uval, all_mem_bits, epoint2)) break;
        uval &= all_mem;
        if ((current_address->l_address ^ uval) <= 0xffff) {
            adr = uval - current_address->l_address - ((opcode != c65ce02.opcode && opcode != c4510.opcode && opcode != c45gs02.opcode) ? 3 : 2);
            break;
        }
        err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
        break;
    case AG_IMP:
        ln = 0;
        break;
    case AG_NONE:
        break;
    }

    cod = cnmemonic[opr];
    if (opr == OPR_REG) cod = regopcode_table[cod][reg];
    dump_instr(cod ^ longbranch, adr, (int)ln, epoint);
    return NULL;
}

