/*
    $Id: instruction.c 3126 2023-10-06 06:16:09Z soci $

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
static const uint8_t *opcode;       /* opcodes */
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

static Adrgen adrmatch(const uint8_t *cnmemonic, int prm, atype_t am, unsigned int w, Adr_types *opr) {
    Adrgen adrgen;
    uint8_t cod;
    switch (am) {
    case A_IMMEDIATE_SIGNED:
    case A_IMMEDIATE:
        if ((cod = cnmemonic[(*opr = ADR_IMMEDIATE)]) == ____ && prm != 0) { /* 0x69 hack */
            return AG_NONE;
        }
        switch (cod) {
        case 0xE0:
        case 0xC0:
        case 0xA2:
        case 0xA0:  /* cpx cpy ldx ldy */
            adrgen = ((longindex && w != 0) || w == 1) ? ((am == A_IMMEDIATE) ? AG_SWORD : AG_SINT) : ((am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR);
            break;
        case 0xF4: /* pea/phw #$ffff */
            adrgen = (am == A_IMMEDIATE) ? AG_SWORD : AG_SINT;
            break;
        case 0x32:
        case 0x42: /* sac sir/wdm */
            if (opcode == c65dtv02.opcode) {
                if (am != A_IMMEDIATE) return AG_NONE;
                adrgen = AG_BYTE;
                break;
            }
            adrgen = (am == A_IMMEDIATE) ? AG_SBYTE : AG_CHAR;
            break;
        case 0xC2:
        case 0xE2:
        case 0xEF:  /* sep rep mmu */
            if (opcode == w65816.opcode || opcode == c65el02.opcode) {
                if (am != A_IMMEDIATE) return AG_NONE;
                adrgen = AG_BYTE;
                break;
            }
            FALL_THROUGH; /* fall through */
        case 0x00:
        case 0x02: /* brk cop */
            adrgen = (am == A_IMMEDIATE) ? AG_SBYTE : AG_CHAR;
            break;
        default:
            if (cnmemonic[ADR_REL] != ____ || cnmemonic[ADR_REL_L] != ____) {
                adrgen = (am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR;
                break;
            }
            adrgen = ((longaccu && w != 0) || w == 1) ? ((am == A_IMMEDIATE) ? AG_SWORD : AG_SINT) : ((am == A_IMMEDIATE) ? AG_SBYTE: AG_CHAR);
        }
        break;
    case (A_IMMEDIATE << 4) | A_BR: /* lda #$ffff,b */
    case A_BR:
        if (cnmemonic[ADR_ADDR] != ____ && cnmemonic[ADR_ADDR] != 0x4C && cnmemonic[ADR_ADDR] != 0x20 && cnmemonic[ADR_ADDR] != 0xF4) {/* jmp $ffff, jsr $ffff, pea */
            adrgen = AG_WORD; *opr = ADR_ADDR; /* lda $ffff,b */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_KR:
    case A_KR:
        if (cnmemonic[ADR_REL] != ____) {
            adrgen = AG_BYTE; *opr = ADR_REL;
            break;
        }
        if (cnmemonic[ADR_REL_L] != ____) {
            adrgen = AG_RELPB; *opr = ADR_REL_L; /* brl */
            break;
        }
        if (cnmemonic[ADR_ADDR] == 0x4C || cnmemonic[ADR_ADDR] == 0x20) {/* jmp $ffff, jsr $ffff */
            adrgen = AG_WORD; *opr = ADR_ADDR; /* jmp $ffff */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_DR:           /* lda #$ff,d */
    case A_DR:
        if (cnmemonic[ADR_ZP] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP; /* lda $ff,d */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_BR << 4) | A_XR: /* lda #$ffff,b,x */
    case (A_BR << 4) | A_XR:
        if (cnmemonic[ADR_ADDR_X] != ____) {
            adrgen = AG_WORD; *opr = ADR_ADDR_X; /* lda $ffff,b,x */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_XR: /* lda #$ff,d,x */
    case (A_DR << 4) | A_XR:
        if (cnmemonic[ADR_ZP_X] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_X; /* lda $ff,d,x */
            break;
        }
        return AG_NONE;
    case A_XR:
        if (cnmemonic[ADR_ZP_X] != ____ || cnmemonic[ADR_ADDR_X] != ____ || cnmemonic[ADR_LONG_X] != ____) {
            adrgen = AG_DB3; *opr = ADR_ZP_X; /* lda $ff,x lda $ffff,x lda $ffffff,x */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_BR << 4) | A_YR:/* ldx #$ffff,b,y */
    case (A_BR << 4) | A_YR:
        if (cnmemonic[ADR_ADDR_Y] != ____) {
            adrgen = AG_WORD; *opr = ADR_ADDR_Y; /* ldx $ffff,b,y */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_YR:/* ldx #$ff,d,y */
    case (A_DR << 4) | A_YR:
        if (cnmemonic[ADR_ZP_Y] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_Y; /* ldx $ff,d,y */
            break;
        }
        return AG_NONE;
    case A_YR:
        if (cnmemonic[ADR_ZP_Y] != ____ || cnmemonic[ADR_ADDR_Y] != ____) {
            adrgen = AG_DB2; *opr = ADR_ZP_Y; /* lda $ff,y lda $ffff,y lda $ffffff,y */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_SR:           /* lda #$ff,s */
    case A_SR:
        if (cnmemonic[ADR_ZP_S] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_S; /* lda $ff,s */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 4) | A_RR:           /* lda #$ff,r */
    case A_RR:
        if (cnmemonic[ADR_ZP_R] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_R; /* lda $ff,r */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,d),y */
    case (A_DR << 8) | (A_I << 4) | A_YR:
        if (cnmemonic[ADR_ZP_I_Y] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_I_Y; /* lda ($ff,d),y */
            break;
        }
        return AG_NONE;
    case (A_I << 4) | A_YR:
        if (cnmemonic[ADR_ZP_I_Y] != ____) {
            adrgen = AG_ZP; *opr = ADR_ZP_I_Y; /* lda ($ff),y */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_I << 4) | A_ZR:/* lda (#$ff,d),z */
    case (A_DR << 8) | (A_I << 4) | A_ZR:
        if (cnmemonic[ADR_ZP_I] != ____ && (opcode == c65ce02.opcode || opcode == c4510.opcode)) {
            adrgen = AG_BYTE; *opr = ADR_ZP_I; /* lda ($ff,d),z */
            break;
        }
        return AG_NONE;
    case (A_I << 4) | A_ZR:
        if (cnmemonic[ADR_ZP_I] != ____ && (opcode == c65ce02.opcode || opcode == c4510.opcode)) {
            adrgen = AG_ZP; *opr = ADR_ZP_I; /* lda ($ff),z */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_SR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,s),y */
    case (A_SR << 8) | (A_I << 4) | A_YR:
        if (cnmemonic[ADR_ZP_S_I_Y] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_S_I_Y; /* lda ($ff,s),y */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_RR << 8) | (A_I << 4) | A_YR:/* lda (#$ff,r),y */
    case (A_RR << 8) | (A_I << 4) | A_YR:
        if (cnmemonic[ADR_ZP_R_I_Y] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_R_I_Y; /* lda ($ff,r),y */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_LI << 4) | A_YR:/* lda [#$ff,d],y */
    case (A_DR << 8) | (A_LI << 4) | A_YR:
        if (cnmemonic[ADR_ZP_LI_Y] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_LI_Y; /* lda [$ff,d],y */
            break;
        }
        return AG_NONE;
    case (A_LI << 4) | A_YR:
        if (cnmemonic[ADR_ZP_LI_Y] != ____) {
            adrgen = AG_ZP; *opr = ADR_ZP_LI_Y; /* lda [$ff],y */
            break;
        }
        return AG_NONE;
    case (A_XR << 4) | A_I:
        if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
            adrgen = AG_PB; *opr = ADR_ADDR_X_I; /* jmp ($ffff,x) */
            break;
        }
        if (cnmemonic[ADR_ZP_X_I] != ____) {
            adrgen = AG_ZP; *opr = ADR_ZP_X_I; /* lda ($ff,x) */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_KR << 8) | (A_XR << 4) | A_I: /* jmp (#$ffff,k,x) */
    case (A_KR << 8) | (A_XR << 4) | A_I:
        if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
            adrgen = AG_WORD; *opr = ADR_ADDR_X_I; /* jmp ($ffff,k,x) */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 12) | (A_DR << 8) | (A_XR << 4) | A_I:/* lda (#$ff,d,x) */
    case (A_DR << 8) | (A_XR << 4) | A_I:
        if (cnmemonic[ADR_ZP_X_I] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_X_I; /* lda ($ff,d,x) */
            break;
        }
        return AG_NONE;
    case A_I:
        if (cnmemonic[ADR_ADDR_I] == 0x6C || cnmemonic[ADR_ADDR_I] == 0x22) {/* jmp ($ffff), jsr ($ffff) */
            adrgen = AG_B0; *opr = ADR_ADDR_I; /* jmp ($ffff) */
            break;
        }
        if (cnmemonic[ADR_ZP_I] != ____ && opcode != c65ce02.opcode && opcode != c4510.opcode) {
            adrgen = AG_ZP; *opr = ADR_ZP_I; /* lda ($ff) */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_I: /* lda (#$ff,d) */
    case (A_DR << 4) | A_I:
        if (cnmemonic[ADR_ZP_I] != ____ && opcode != c65ce02.opcode && opcode != c4510.opcode) {
            adrgen = AG_BYTE; *opr = ADR_ZP_I; /* lda ($ff,d) */
            break;
        }
        return AG_NONE;
    case A_LI:
        if (cnmemonic[ADR_ADDR_LI] == 0xDC) { /* jmp [$ffff] */
            adrgen = AG_B0; *opr = ADR_ADDR_LI; /* jmp [$ffff] */
            break;
        }
        if (cnmemonic[ADR_ZP_LI] != ____) {
            adrgen = AG_ZP; *opr = ADR_ZP_LI; /* lda [$ff] */
            break;
        }
        return AG_NONE;
    case (A_IMMEDIATE << 8) | (A_DR << 4) | A_LI: /* lda [#$ff,d] */
    case (A_DR << 4) | A_LI:
        if (cnmemonic[ADR_ZP_LI] != ____) {
            adrgen = AG_BYTE; *opr = ADR_ZP_LI; /* lda [$ff,d] */
            break;
        }
        return AG_NONE;
    default:
        return AG_NONE;
    }
    return adrgen;
}

MUST_CHECK Error *instruction(int prm, unsigned int w, Funcargs *vals, linepos_t epoint) {
    Adrgen adrgen;
    Adr_types opr;
    Reg_types reg;
    const uint8_t *cnmemonic; /* current nmemonic */
    unsigned int ln;
    uint8_t cod, longbranch;
    uint32_t adr;
    uval_t uval;
    Obj *val;
    struct linepos_s *epoint2;
    Error *err;
    atype_t am;

    cnmemonic = opcode_table[opcode[prm]];
    longbranch = 0; reg = REG_A; adr = 0;

    switch (vals->len) {
    case 0:
        if (cnmemonic[ADR_IMPLIED] != ____) {
            if (diagnostics.implied_reg && cnmemonic[ADR_REG] != 0) err_msg_implied_reg(epoint, mnemonic[prm]);
            adrgen = AG_IMP; opr = ADR_IMPLIED;
            break;
        }
        return err_addressing(A_NONE, epoint, prm);
    case 1:
        epoint2 = &vals->val[0].epoint;
        val = vals->val[0].val;
        if (val->obj->iterable) {
            struct iter_s iter;
            iter.data = vals->val[0].val; iter.data->obj->getiter(&iter);
            err = NULL;
            while ((vals->val[0].val = iter.next(&iter)) != NULL) {
                Error *err2 = instruction(prm, w, vals, epoint);
                if (err != NULL) err_msg_output_and_destroy(err);
                err = err2;
            }
            vals->val[0].val = iter.data;
            iter_destroy(&iter);
            return err;
        }
        am = val->obj->address(val);
    retry1:
        if (am != A_NONE) {
            adrgen = adrmatch(cnmemonic, prm, am, w, &opr);
            if (adrgen != AG_NONE) {
                switch (opr) {
                case ADR_REL:
                    ln = 1; longbranch = 0;
                    goto justrel2;
                case ADR_IMMEDIATE:
                    if (cnmemonic[ADR_REL] != ____) {
                        ln = 1; longbranch = 0; opr = ADR_REL;
                        goto immediaterel;
                    }
                    if (cnmemonic[ADR_REL_L] != ____) {
                        if (w == 0) err_msg2(ERROR__NO_BYTE_ADDR, &mnemonic[prm], epoint2);
                        ln = 2; longbranch = 0; opr = ADR_REL_L;
                        goto immediaterel;
                    }
                    if (pline[epoint2->pos] == '#') epoint2->pos++;
                    break;
                case ADR_ZP_I_Y:
                case ADR_ZP_S_I_Y:
                case ADR_ZP_R_I_Y:
                case ADR_ADDR_X_I:
                case ADR_ZP_X_I:
                case ADR_ADDR_I:
                case ADR_ZP_I:
                    if (pline[epoint2->pos] == '(') epoint2->pos++;
                    break;
                case ADR_ZP_LI_Y:
                case ADR_ADDR_LI:
                case ADR_ZP_LI:
                    if (pline[epoint2->pos] == '[') epoint2->pos++;
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
            cod = cnmemonic[(opr = ADR_REG)];
            if (cod != 0 && cpureg->len == 1) {
                const char *ind = strchr(reg_names, cpureg->data[0]);
                if (ind != NULL) {
                    reg = (Reg_types)(ind - reg_names);
                    if (regopcode_table[cod][reg] != ____) {
                        adrgen = AG_IMP;
                        break;
                    }
                }
            }
            err = new_error(ERROR___NO_REGISTER, epoint2);
            err->u.reg.reg = ref_register(cpureg);
            err->u.reg.cod = mnemonic[prm];
            err_msg_output_and_destroy(err);
            val = none_value;
        }
        if (cnmemonic[ADR_REL] != ____) {
            struct star_s *s;
            uint16_t xadr;
            uval_t oadr;
            bool crossbank, invalid;
            Obj *oval;
            ln = 1; opr = ADR_REL;
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
            if (oval->obj == CODE_OBJ && pass != Code(oval)->apass && cnmemonic[ADR_REL_L] == ____) { /* not for 65CE02! */
                s = new_star(vline + 1);
                adr = s->pass != 0 ? (uint16_t)(uval - s->addr) : (uint16_t)(uval - current_address->l_address - 1 - ln);
            } else {
                s = invalid ? new_star(vline + 1) : NULL;
                adr = (uint16_t)(uval - current_address->l_address - 1 - ln);
            }
            if (false) {
                bool longpossible;
        immediaterel:
                longpossible = (cnmemonic[ADR_REL_L] != ____) && w != 0;
                if (adrgen == AG_SBYTE) {
                    invalid = touaddress(val, &uval, longpossible ? 16 : 8, epoint2);
                } else {
                    invalid = toiaddress(val, (ival_t *)&uval, longpossible ? 16 : 8, epoint2);
                }
                if (invalid) uval = 0;
                if (!longpossible && (uval & 0x80) != 0) uval |= ~(uval_t)0xff;
                uval &= 0xffff;
                uval |= current_address->l_address & ~(uval_t)0xffff;
                crossbank = false;
                xadr = (uint16_t)adr;
                s = NULL;
                oadr = uval;
                adr = (uint16_t)uval;
            }
            if ((adr<0xFF80 && adr>0x007F) || crossbank || w == 1 || w == 2) {
                if (cnmemonic[ADR_REL_L] != ____ && !crossbank && (w == 3 || w == 1)) { /* 65CE02 long branches */
                    opr = ADR_REL_L;
                    ln = 2;
                } else if (arguments.longbranch && (cnmemonic[ADR_ADDR] == ____) && w == 3) { /* fake long branches */
                    if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {/* bxx branch */
                        struct longjump_s *lj = new_longjump(&current_section->longjump, uval);
                        if (lj->defpass == pass) {
                            if ((current_address->l_address ^ lj->dest) <= 0xffff) {
                                uint32_t adrk = (uint16_t)(lj->dest - current_address->l_address - 2);
                                if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                    adr = adrk;
                                    goto branchok;
                                }
                            }
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL]);
                        if (s == NULL) s = new_star(vline + 1);
                        dump_instr(cnmemonic[ADR_REL] ^ 0x20, s->pass != 0 ? ((uint16_t)(s->addr - current_address->l_address - 2)) : 3, 1, epoint);
                        lj->dest = current_address->l_address;
                        lj->defpass = pass;
                        if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                        if (diagnostics.optimize) cpu_opt_long_branch(0xea);
                        err = instruction((current_cpu->brl >= 0 && !longbranchasjmp && !crossbank) ? current_cpu->brl : current_cpu->jmp, w, vals, epoint);
                        if (diagnostics.optimize) cpu_opt_long_branch(0);
                        goto branchend;
                    }
                    if (opr == ADR_BIT_ZP_REL) {
                        struct longjump_s *lj;
                        if (crossbank) {
                            err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
                            goto branchok;
                        }
                        lj = new_longjump(&current_section->longjump, uval);
                        if (lj->defpass == pass) {
                            if ((current_address->l_address ^ lj->dest) <= 0xffff) {
                                uint32_t adrk = (uint16_t)(lj->dest - current_address->l_address - 3);
                                if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                    adr = adrk;
                                    goto branchok;
                                }
                            }
                        }
                        if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_BIT_ZP_REL] ^ longbranch);
                        dump_instr(cnmemonic[ADR_BIT_ZP_REL] ^ 0x80 ^ longbranch, xadr | 0x300, 2, epoint);
                        lj->dest = current_address->l_address;
                        lj->defpass = pass;
                        if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                        if (diagnostics.optimize) cpu_opt_long_branch(0xea);
                        err = instruction(current_cpu->jmp, w, vals, epoint);
                        if (diagnostics.optimize) cpu_opt_long_branch(0);
                        goto branchend;
                    } else {/* bra */
                        if (current_cpu->brl >= 0 && !longbranchasjmp) { /* bra -> brl */
                        asbrl:
                            if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                            if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL] | 0x100U);
                            err = instruction(current_cpu->brl, w, vals, epoint);
                            if (diagnostics.optimize) cpu_opt_long_branch(0);
                            goto branchend;
                        } else if (cnmemonic[ADR_REL] == 0x82 && opcode == c65el02.opcode) { /* not a branch ! */
                            int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                            if (crossbank) {
                                err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
                            } else err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint2); /* rer not a branch */
                        } else { /* bra -> jmp */
                        asjmp:
                            if (diagnostics.long_branch) err_msg2(ERROR___LONG_BRANCH, NULL, epoint2);
                            if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL] | 0x100U);
                            err = instruction(current_cpu->jmp, w, vals, epoint);
                            if (diagnostics.optimize) cpu_opt_long_branch(0);
                        branchend:
                            if (s != NULL) {
                                address_t st = current_address->l_address;
                                if (s != NULL && s->pass != 0 && s->addr != st) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = false;
                                }
                                s->addr = st;
                            }
                            return err;
                        }
                    }
                } else if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                    if (current_cpu->brl >= 0 && !longbranchasjmp) goto asbrl; /* gcc -> brl */
                    goto asjmp; /* gcc -> jmp */
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
                if (((current_address->l_address + 1 + ln) & 0xff00) != (oadr & 0xff00)) {
                    int diff = (int8_t)oadr;
                    if (diff >= 0) diff++;
                    if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, &diff, epoint2);
                    else if (diagnostics.branch_page) err_msg_branch_page(diff, epoint2);
                }
                if (cnmemonic[ADR_ADDR] != ____ && w == 3) { /* gcc */
                    if (adr == 0) {
                        dump_instr(cnmemonic[ADR_REL], 0, -1, epoint);
                        err = NULL;
                        goto branchend;
                    }
                    if (adr == 1) {
                        if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                            if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL] | 0x100U);
                            dump_instr(cnmemonic[ADR_REL] ^ 0x20, 1, 0, epoint);
                            if (diagnostics.optimize) cpu_opt_long_branch(0);
                            err = NULL;
                            goto branchend;
                        }
                        if (cnmemonic[ADR_REL] == 0x80 && (opcode == r65c02.opcode || opcode == w65c02.opcode)) {
                            if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL] | 0x100U);
                            dump_instr(0x82, 1, 0, epoint);
                            if (diagnostics.optimize) cpu_opt_long_branch(0);
                            err = NULL;
                            goto branchend;
                        }
                    }
                    if (adr == 2 && (opcode == c65ce02.opcode || opcode == c4510.opcode)) {
                        if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                            if (diagnostics.optimize) cpu_opt_long_branch(cnmemonic[ADR_REL] | 0x100U);
                            dump_instr(cnmemonic[ADR_REL] ^ 0x23, 2, 0, epoint);
                            if (diagnostics.optimize) cpu_opt_long_branch(0);
                            err = NULL;
                            goto branchend;
                        }
                    }
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
            if (opr == ADR_BIT_ZP_REL) adr = xadr | (adr << 8);
            adrgen = AG_NONE; break;
        }
        if (cnmemonic[ADR_REL_L] != ____) {
            adrgen = AG_RELL; opr = ADR_REL_L; /* brl */
            break;
        }
        if (cnmemonic[ADR_ADDR] == 0x20 || cnmemonic[ADR_ADDR] == 0x4C) {
            adrgen = AG_PB; opr = ADR_ADDR; /* jsr $ffff, jmp */
            break;
        }
        if (cnmemonic[ADR_ADDR] == 0xF4) {
            adrgen = AG_WORD; opr = ADR_ADDR; /* pea $ffff */
            break;
        }
        if (cnmemonic[ADR_ZP] != ____ || cnmemonic[ADR_ADDR] != ____ || cnmemonic[ADR_LONG] != ____) {
            adrgen = AG_DB3; opr = ADR_ZP; /* lda $ff lda $ffff lda $ffffff */
            break;
        }
        goto unknown;
    case 2:
        epoint2 = &vals->val[0].epoint;
        if (vals->val[0].val->obj->iterable || vals->val[1].val->obj->iterable) {
            struct values_s *v;
            argcount_t args;
            argcount_t j;
            size_t ln2;
            struct elements_s {
                Obj *oval;
                struct iter_s iter;
            } elements[3];
        broadcast:
            v = vals->val;
            args = vals->len;
            ln2 = 1;
            err = NULL;
            for (j = 0; j < args; j++) {
                const Type *objt = v[j].val->obj;
                if (objt->iterable) {
                    struct iter_s *iter = &elements[j].iter;
                    elements[j].oval = iter->data = v[j].val; objt->getiter(iter);
                    if (iter->len == 1) {
                        v[j].val = iter->next(iter);
                    } else if (iter->len != ln2) {
                        if (ln2 != 1) {
                            err = new_error(ERROR_CANT_BROADCAS, &v[j].epoint);
                            err->u.broadcast.v1 = ln2;
                            err->u.broadcast.v2 = iter->len;
                            for (j++; j < args; j++) {
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
                while (ln2 != 0) {
                    Error *err2;
                    for (j = 0; j < args; j++) {
                        if (elements[j].oval == NULL) continue;
                        if (elements[j].iter.len != 1) v[j].val = elements[j].iter.next(&elements[j].iter);
                    }
                    err2 = instruction(prm, w, vals, epoint);
                    if (err != NULL) err_msg_output_and_destroy(err);
                    err = err2;
                    ln2--;
                }
            }
            for (j = 0; j < args; j++) {
                if (elements[j].oval == NULL) continue;
                v[j].val = elements[j].oval;
                iter_destroy(&elements[j].iter);
            }
            return err;
        }
        if (vals->val[1].val->obj == REGISTER_OBJ) {
            am = Register(vals->val[1].val)->len != 1 ? A_NONE : register_to_indexing(Register(vals->val[1].val)->data[0]);
            if (am != A_NONE) {
                val = vals->val[0].val;
                am |= val->obj->address(val) << 4;
                goto retry1;
            }
        }
        if (cnmemonic[ADR_MOVE] != ____) {
            if (w != 3 && w != 1) return err_addressize((w == 0) ? ERROR__NO_BYTE_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
            val = vals->val[0].val;
            if (touaddress(val, &uval, 8, epoint2)) {}
            else {
                am = val->obj->address(val);
                if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                else adr = (uval & 0xff) << 8;
            }
            epoint2 = &vals->val[1].epoint;
            val = vals->val[1].val;
            if (touaddress(val, &uval, 8, epoint2)) {}
            else {
                am = val->obj->address(val);
                if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                else adr |= uval & 0xff;
            }
            ln = 2;
            adrgen = AG_NONE; opr = ADR_MOVE;
            break;
        }
        if (cnmemonic[ADR_BIT_ZP] != ____) {
            if (w != 3 && w != 0) return err_addressize((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint2, prm);
            if (touval(vals->val[0].val, &uval, 3, epoint2)) {}
            else longbranch = ((uval & 7) << 4) & 0x70;
            val = vals->val[1].val;
            epoint2 = &vals->val[1].epoint;
            am = val->obj->address(val);
            if (am == A_DR) {
                adrgen = AG_BYTE;
            } else {
                if (am != A_NONE) err_msg_output_and_destroy(err_addressing(am, epoint2, prm));
                adrgen = AG_ZP;
            }
            opr = ADR_BIT_ZP;
            break;
        }
        goto unknown;
    case 3:
        epoint2 = &vals->val[0].epoint;
        if (vals->val[0].val->obj->iterable || vals->val[1].val->obj->iterable || vals->val[2].val->obj->iterable) {
            goto broadcast;
        }
        if (vals->val[1].val->obj == REGISTER_OBJ && vals->val[2].val->obj == REGISTER_OBJ) {
            atype_t am2 = Register(vals->val[2].val)->len != 1 ? A_NONE : register_to_indexing(Register(vals->val[2].val)->data[0]);
            am = Register(vals->val[1].val)->len != 1 ? A_NONE : register_to_indexing(Register(vals->val[1].val)->data[0]);
            if (am != A_NONE && am2 != A_NONE) {
                val = vals->val[0].val;
                am |= val->obj->address(val) << 4;
                am = (am << 4) | am2;
                goto retry1;
            }
        }
        if (cnmemonic[ADR_BIT_ZP_REL] != ____) {
            if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
            if (touval(vals->val[0].val, &uval, 3, epoint2)) {}
            else longbranch = ((uval & 7) << 4) & 0x70;
            val = vals->val[1].val;
            epoint2 = &vals->val[1].epoint;
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
            val = vals->val[2].val;
            epoint2 = &vals->val[2].epoint;
            ln = 2; opr = ADR_BIT_ZP_REL;
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
        FALL_THROUGH; /* fall through */
    default:
        epoint2 = &vals->val[0].epoint;
    unknown:
        {
            argcount_t j, args = vals->len;
            for (j = 0; j < args; j++) {
                Obj *v = vals->val[j].val;
                if (v->obj == ERROR_OBJ) return Error(v);
            }
            err = new_error(ERROR___NO_LOT_OPER, epoint);
            err->u.opers.num = j;
            err->u.opers.cod = mnemonic[prm];
            return err;
        }
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
                if (dpage > 0xffff || (uint16_t)(uval2 - dpage) > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                else if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                    if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                } else {
                    if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                }
                break;
            }
            err_msg2(ERROR_____NOT_BANK0, val, epoint2);
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
                if (diagnostics.jmp_bug && cnmemonic[opr] == 0x6c && opcode != w65816.opcode && opcode != c65c02.opcode && opcode != r65c02.opcode && opcode != w65c02.opcode && opcode != c65ce02.opcode && opcode != c4510.opcode && opcode != c65el02.opcode && (~adr & 0xff) == 0) err_msg_jmp_bug(epoint2);/* jmp ($xxff) */
                break;
            }
            err_msg2(ERROR_____NOT_BANK0, val, epoint2);
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
            err_msg2(ERROR_CANT_CROSS_BA, val, epoint2);
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

            if (w == 3) {/* auto length */
                if (val2->obj == CODE_OBJ && !Code(val2)->memblocks->enumeration) {
                    if (tocode_uaddress(val2, &uval, &uval2, epoint2)) w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
                } else {
                    if (touaddress(val, &uval, all_mem_bits, epoint2)) w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
                    else uval2 = uval;
                }
                if (w == 3) {/* auto length */
                    uval_t uval3;
                    uval2 &= all_mem;
                    uval3 = (opr == ADR_ZP || opcode == c65el02.opcode || opcode == w65816.opcode) ? (uval & all_mem) : uval2;
                    if (diagnostics.altmode) {
                        if (uval3 <= 0xffff && dpage <= 0xffff && (uint16_t)(uval3 - dpage) <= 0xff) w = 0;
                        else if (databank == ((uval & all_mem) >> 16) || adrgen != AG_DB3) w = 1;
                        else w = 2;
                    }

                    if (cnmemonic[opr] != ____ && uval3 <= 0xffff && dpage <= 0xffff && (uint16_t)(uval3 - dpage) <= 0xff) {
                        if (diagnostics.immediate && opr == ADR_ZP && (cnmemonic[ADR_IMMEDIATE] != ____ || prm == 0) && (val->obj != CODE_OBJ || Code(val)->memblocks->enumeration) && val->obj != ADDRESS_OBJ) err_msg2(ERROR_NONIMMEDCONST, NULL, epoint2);
                        else if (w != 3 && w != 0) err_msg_address_mismatch(opr-0, opr-w, epoint2);
                        adr = uval - dpage; w = 0;
                        if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                            if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                        } else {
                            if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                        }
                    } else if (cnmemonic[opr - 1] != ____ && databank == ((uval & all_mem) >> 16)) {
                        if (w != 3 && w != 1) err_msg_address_mismatch(opr-1, opr-w, epoint2);
                        adr = uval; w = 1;
                        if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    } else if (adrgen == AG_DB3 && cnmemonic[opr - 2] != ____) {
                        if (w != 3 && w != 2) err_msg_address_mismatch(opr-2, opr-w, epoint2);
                        adr = uval; w = 2;
                        if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    } else {
                        w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
                        err_msg2((w != 0) ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val, epoint2);
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
                    if (cnmemonic[opr] == ____) return err_addressize(ERROR__NO_BYTE_ADDR, epoint2, prm);
                    uval2 &= all_mem;
                    uval3 = (opcode == c65el02.opcode || opcode == w65816.opcode) ? (uval & all_mem) : uval2;
                    if (uval3 <= 0xffff) {
                        adr = uval - dpage;
                        if (dpage > 0xffff || (uint16_t)(uval3 - dpage) > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                        else if (opcode == c65el02.opcode || opcode == w65816.opcode) {
                            if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                        } else {
                            if (adr > 0xff) err_msg_dpage_wrap(epoint2);
                        }
                        break;
                    }
                    err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                    break;
                case 1:
                    if (cnmemonic[opr - 1] == ____) return err_addressize(ERROR__NO_WORD_ADDR, epoint2, prm);
                    uval &= all_mem;
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                    else if ((uval & all_mem) != uval) err_msg_addr_wrap(epoint2);
                    break;
                case 2:
                    if (adrgen == AG_DB3 && cnmemonic[opr - 2] != ____) {
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
            opr = (Adr_types)(opr - w); ln = w + 1;
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
        adr = uval - current_address->l_address - ((opcode != c65ce02.opcode && opcode != c4510.opcode) ? 3 : 2);
        break;
    case AG_RELL:
        if (w != 3 && w != 1) return err_addressize((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint2, prm);
        ln = 2;
        if (touaddress(val, &uval, all_mem_bits, epoint2)) break;
        uval &= all_mem;
        if ((current_address->l_address ^ uval) <= 0xffff) {
            adr = uval - current_address->l_address - ((opcode != c65ce02.opcode && opcode != c4510.opcode) ? 3 : 2);
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
    if (opr == ADR_REG) cod = regopcode_table[cod][reg];
    dump_instr(cod ^ longbranch, adr, (int)ln, epoint);
    return NULL;
}

