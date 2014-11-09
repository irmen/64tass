/*
    $Id$

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
#include <string.h>
#include "instruction.h"
#include "opcodes.h"
#include "values.h"
#include "64tass.h"
#include "misc.h"
#include "section.h"
#include "file.h"
#include "listing.h"

static const uint32_t *mnemonic;    /* mnemonics */
static const uint8_t *opcode;       /* opcodes */
static unsigned int last_mnem;
static const struct cpu_s *cpu;

int longaccu = 0, longindex = 0; /* hack */
uint16_t dpage = 0;
uint8_t databank = 0;
int longbranchasjmp = 0;
int allowslowbranch = 1;

int lookup_opcode(const char *s) {
    int32_t s4;
    unsigned int also, felso, elozo, no;
    uint32_t name;

    if (arguments.casesensitive) {
        name = (s[0] << 16) | (s[1] << 8) | s[2];
    } else {
        name = (lowcase(s[0]) << 16) | (lowcase(s[1]) << 8) | lowcase(s[2]);
    }
    also = 0;
    no = (felso = last_mnem) / 2;
    for (;;) {  /* do binary search */
        if (!(s4 = name - mnemonic[no]))
            return no;
            elozo = no;
        if (elozo == (no = ((s4 > 0) ? (felso + (also = no)) : (also + (felso = no))) / 2)) break;
    }
    return -1;
}

void select_opcodes(const struct cpu_s *cpumode) {
    last_mnem = cpumode->opcodes;
    mnemonic = cpumode->mnemonic; 
    opcode = cpumode->opcode;
    cpu = cpumode;
}

MUST_CHECK int touval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t err;
    if (v1->obj == NONE_OBJ) {
        err_msg_still_none(NULL, epoint);
        return 1;
    }
    err = v1->obj->uval(v1, uv, bits, epoint);
    if (err) {
        err_msg_output_and_destroy(err);
        return 1;
    }
    return 0;
}

static MUST_CHECK value_t err_addressing(atype_t am, linepos_t epoint) {
    value_t v = new_error_obj(ERROR_NO_ADDRESSING, epoint);
    v->u.error.u.addressing = am;
    return v;
}

MUST_CHECK value_t instruction(int prm, int w, value_t vals, linepos_t epoint, struct linepos_s *epoints) {
    enum { AG_ZP, AG_B0, AG_PB, AG_PB2, AG_BYTE, AG_DB3, AG_DB2, AG_WORD, AG_RELPB, AG_RELL, AG_IMP, AG_NONE } adrgen;
    enum opr_e opr;
    enum reg_e reg;
    const uint8_t *cnmemonic; /* current nmemonic */
    int_fast8_t ln;
    uint8_t cod, longbranch;
    uint32_t adr;
    uval_t uval;
    value_t val;
    linepos_t epoint2 = &epoints[0];

    cnmemonic = opcode_table[opcode[prm]];
    longbranch = 0; reg = REG_A; adr = 0;


    if (vals->obj != ADDRLIST_OBJ) {
        val = vals; goto single;
    } else {
        size_t opers = vals->u.list.len;
        switch (opers) {
        case 0:
            if (cnmemonic[ADR_IMPLIED] != ____) {
                adrgen = AG_IMP; opr = ADR_IMPLIED;
                break;
            }
            return err_addressing(A_NONE, epoint);
        case 1:
            val = vals->u.list.data[0];
        single:
            if (val->obj == ADDRESS_OBJ) {
                atype_t am = val->u.addr.type;
                val = val->u.addr.val;
                switch (am) {
                case A_IMMEDIATE:
                    if ((cod = cnmemonic[(opr = ADR_IMMEDIATE)]) == ____ && prm) { /* 0x69 hack */
                        return err_addressing(am, epoint);
                    }
                    switch (cod) {
                    case 0xE0:
                    case 0xC0:
                    case 0xA2:
                    case 0xA0:  /* cpx cpy ldx ldy */
                        adrgen = longindex ? AG_WORD : AG_BYTE;
                        break;
                    case 0xF4: /* pea/phw #$ffff */
                        adrgen = AG_WORD; 
                        break;
                    case 0xC2:
                    case 0xE2:
                    case 0x00:
                    case 0x02:
                    case 0xEF: /* sep rep brk cop mmu */
                        adrgen = AG_BYTE;
                        break;
                    default:
                        adrgen = longaccu ? AG_WORD : AG_BYTE;
                    }
                    break;
                case A_BR:
                    if (cnmemonic[ADR_ADDR] != 0x4C && cnmemonic[ADR_ADDR] != 0x20 && cnmemonic[ADR_ADDR] != 0xF4) {/* jmp $ffff, jsr $ffff, pea */
                        adrgen = AG_WORD; opr = ADR_ADDR; /* lda $ffff,b */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_KR:
                    if (cnmemonic[ADR_REL] != ____) {
                        ln = 1; opr = ADR_REL;
                        longbranch = 0;
                        goto justrel2;
                    }
                    if (cnmemonic[ADR_REL_L] != ____) {
                        adrgen = AG_RELPB; opr = ADR_REL_L; /* brl */
                        break;
                    }
                    if (cnmemonic[ADR_ADDR] == 0x4C || cnmemonic[ADR_ADDR] == 0x20) {/* jmp $ffff, jsr $ffff */
                        adrgen = AG_WORD; opr = ADR_ADDR; /* jmp $ffff */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_DR:
                    if (cnmemonic[ADR_ZP] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP; /* lda $ff,d */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_BR << 4) | A_XR:
                    if (cnmemonic[ADR_ADDR_X] != ____) {
                        adrgen = AG_WORD; opr = ADR_ADDR_X; /* lda $ffff,b,x */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 4) | A_XR:
                    if (cnmemonic[ADR_ZP_X] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_X; /* lda $ff,d,x */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_XR:
                    if (cnmemonic[ADR_ZP_X] != ____ || cnmemonic[ADR_ADDR_X] != ____ || cnmemonic[ADR_LONG_X] != ____) {
                        adrgen = AG_DB3; opr = ADR_ZP_X; /* lda $ff,x lda $ffff,x lda $ffffff,x */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_BR << 4) | A_YR:
                    if (cnmemonic[ADR_ADDR_Y] != ____) {
                        adrgen = AG_WORD; opr = ADR_ADDR_Y; /* ldx $ffff,b,y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_Y] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_Y; /* ldx $ff,d,y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_YR: 
                    if (cnmemonic[ADR_ZP_Y] != ____ || cnmemonic[ADR_ADDR_Y] != ____) {
                        adrgen = AG_DB2; opr = ADR_ZP_Y; /* lda $ff,y lda $ffff,y lda $ffffff,y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_SR:
                    if (cnmemonic[ADR_ZP_S] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_S; /* lda $ff,s */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_RR:
                    if (cnmemonic[ADR_ZP_R] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_R; /* lda $ff,r */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 8) | (A_I << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_I_Y] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_I_Y; /* lda ($ff,d),y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_I << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_I_Y] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_I_Y; /* lda ($ff),y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 8) | (A_I << 4) | A_ZR:
                    if (cnmemonic[ADR_ZP_I_Z] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_I_Z; /* lda ($ff,d),z */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_I << 4) | A_ZR:
                    if (cnmemonic[ADR_ZP_I_Z] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_I_Z; /* lda ($ff),z */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_SR << 8) | (A_I << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_S_I_Y] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_S_I_Y; /* lda ($ff,s),y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_RR << 8) | (A_I << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_R_I_Y] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_R_I_Y; /* lda ($ff,r),y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 8) | (A_LI << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_LI_Y] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_LI_Y; /* lda [$ff,d],y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_LI << 4) | A_YR:
                    if (cnmemonic[ADR_ZP_LI_Y] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_LI_Y; /* lda [$ff],y */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_XR << 4) | A_I:
                    if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
                        adrgen = AG_PB; opr = ADR_ADDR_X_I; /* jmp ($ffff,x) */
                        break;
                    } 
                    if (cnmemonic[ADR_ZP_X_I] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_X_I; /* lda ($ff,x) */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_KR << 8) | (A_XR << 4) | A_I:
                    if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
                        adrgen = AG_WORD; opr = ADR_ADDR_X_I; /* jmp ($ffff,k,x) */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 8) | (A_XR << 4) | A_I:
                    if (cnmemonic[ADR_ZP_X_I] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_X_I; /* lda ($ff,d,x) */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_I:
                    if (cnmemonic[ADR_ADDR_I] == 0x6C || cnmemonic[ADR_ADDR_I] == 0x22) {/* jmp ($ffff), jsr ($ffff) */
                        adrgen = AG_B0; opr = ADR_ADDR_I; /* jmp ($ffff) */
                        break;
                    } 
                    if (cnmemonic[ADR_ZP_I] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_I; /* lda ($ff) */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 4) | A_I:
                    if (cnmemonic[ADR_ZP_I] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_I; /* lda ($ff,d) */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_LI:
                    if (cnmemonic[ADR_ADDR_LI] == 0xDC) { /* jmp [$ffff] */
                        adrgen = AG_B0; opr = ADR_ADDR_LI; /* jmp [$ffff] */
                        break;
                    } 
                    if (cnmemonic[ADR_ZP_LI] != ____) {
                        adrgen = AG_ZP; opr = ADR_ZP_LI; /* lda [$ff] */
                        break;
                    }
                    return err_addressing(am, epoint);
                case (A_DR << 4) | A_LI:
                    if (cnmemonic[ADR_ZP_LI] != ____) {
                        adrgen = AG_BYTE; opr = ADR_ZP_LI; /* lda [$ff,d] */
                        break;
                    }
                    return err_addressing(am, epoint);
                case A_NONE:
                    goto noneaddr;
                default: 
                    return err_addressing(am, epoint); /* non-existing */
                }
                break;
            }
        noneaddr:
            if (val->obj == REGISTER_OBJ) {
                value_t err;
                cod = cnmemonic[(opr = ADR_REG)];
                if (cod && val->u.reg.len == 1) {
                    const char *ind = strchr(reg_names, val->u.reg.data[0]);
                    if (ind) {
                        reg = ind - reg_names;
                        if (regopcode_table[cod][reg] != ____) {
                            adrgen = AG_IMP;
                            break;
                        }
                    }
                }
                err = new_error_obj(ERROR___NO_REGISTER, epoint);
                err->u.error.u.reg = val_reference(val);
                return err;
            }
            if (cnmemonic[ADR_REL] != ____) {
                struct star_s *s;
                int labelexists;
                uint16_t oadr, xadr;
                int labelexists2;
                int crossbank;
                ln = 1; opr = ADR_REL;
                longbranch = 0;
                if (0) {
            justrel2:
                    if (touval(val, &uval, 16, epoint2)) uval = current_section->l_address + 1 + ln;
                    crossbank = 0;
                } else {
            justrel: 
                    if (touval(val, &uval, 24, epoint2)) uval = current_section->l_address + 1 + ln;
                    crossbank = ((uval_t)current_section->l_address ^ uval) > 0xffff;
                }
                xadr = adr;
                s = new_star(vline + 1, &labelexists);

                labelexists2 = labelexists;
                oadr = uval;
                if (labelexists2 && val->obj == CODE_OBJ && pass != val->u.code.apass) {
                    adr = (uint16_t)(uval - s->addr);
                } else {
                    adr = (uint16_t)(uval - current_section->l_address - 1 - ln); labelexists2 = 0;
                }
                if ((adr<0xFF80 && adr>0x007F) || crossbank) {
                    if (cnmemonic[ADR_REL_L] != ____ && !crossbank) { /* 65CE02 long branches */
                    asbrl:
                        if (!labelexists2) adr = (uint16_t)(adr - 1);
                        opr = ADR_REL_L;
                        ln = 2;
                    } else if (arguments.longbranch && (cnmemonic[ADR_ADDR] == ____)) { /* fake long branches */
                        if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {/* bxx branch */
                            value_t err;
                            cod = cnmemonic[ADR_REL] ^ 0x20;
                            ln = labelexists ? ((uint16_t)(s->addr - star - 2)) : 3;
                            pokeb(cod);
                            pokeb(ln);
                            listing_instr(cod, ln, 1);
                            err = instruction((cpu->brl >= 0 && !longbranchasjmp && !crossbank) ? cpu->brl : cpu->jmp, w, vals, epoint, epoints);
                            if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = 0;
                            }
                            s->addr = current_section->l_address & all_mem;
                            return err;
                        } else if (opr == ADR_BIT_ZP_REL) {
                            if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                            cod = cnmemonic[ADR_BIT_ZP_REL] ^ 0x80 ^ longbranch;
                            pokeb(cod);
                            pokeb(xadr);
                            pokeb(3);
                            listing_instr(cod, xadr | 0x300, 2);
                            adr = oadr; opr = ADR_ADDR; ln = 2;
                            prm = cpu->jmp; longbranch = 0;
                            cnmemonic = opcode_table[opcode[prm]];
                        } else {/* bra */
                            if (cpu->brl >= 0 && !longbranchasjmp) { /* bra -> brl */
                            asbrl2:
                                if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                                else {
                                    prm = cpu->brl;
                                    cnmemonic = opcode_table[opcode[prm]];
                                    goto asbrl;
                                }
                            } else if (cnmemonic[ADR_REL] == 0x82 && opcode == c65el02.opcode) { /* not a branch ! */
                                int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                                if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                                else err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint); /* rer not a branch */
                            } else { /* bra -> jmp */
                                value_t err;
                            asjmp:
                                err = instruction(cpu->jmp, w, vals, epoint, epoints);
                                if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = 0;
                                }
                                s->addr = current_section->l_address & all_mem;
                                return err;
                            }
                        }
                        /* erro = ERROR___LONG_BRANCH; */
                    } else if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                        if (cpu->brl >= 0 && !longbranchasjmp) goto asbrl2; /* gcc -> brl */
                        if (crossbank) goto asjmp; /* gcc -> jmp */
                        adr = oadr;
                        opr = ADR_ADDR;
                        ln = 2;
                    } else { /* too long */
                        if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                        else {
                            int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                            err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint);
                        }
                    }
                } else { /* short */
                    if (((uint16_t)(current_section->l_address + 1 + ln) & 0xff00) != (oadr & 0xff00)) {
                        if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, NULL, epoint);
                    }
                    if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                        if (adr == 0) {
                            listing_instr(0, 0, -1);
                            if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = 0;
                            }
                            s->addr = current_section->l_address & all_mem;
                            return NULL;
                        } else if (adr == 1 && (cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                            cod = cnmemonic[ADR_REL] ^ 0x20;
                            pokeb(cod);
                            listing_instr(cod, 0, 0);
                            if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = 0;
                            }
                            s->addr = current_section->l_address & all_mem;
                            return NULL;
                        }
                    }
                }
                if (labelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = 0;
                }
                s->addr = (star + 1 + ln) & all_mem;
                if (opr == ADR_BIT_ZP_REL) adr = xadr | (adr << 8);
                adrgen = AG_NONE; break;
            }
            if (cnmemonic[ADR_REL_L] != ____) {
                adrgen = AG_RELL; opr = ADR_REL_L; /* brl */
                break;
            }
            if (cnmemonic[ADR_LONG] == 0x5C) {
                adrgen = AG_PB2; opr = ADR_ZP; /* jml */
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
            val = new_error_obj(ERROR___NO_LOT_OPER, epoint);
            val->u.error.u.opers = 1;
            return val;
        case 2:
            if (cnmemonic[ADR_MOVE] != ____) {
                val = vals->u.list.data[0];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_IMMEDIATE) val = val->u.addr.val;
                if (touval(val, &uval, 8, epoint2)) {}
                else adr = (uint16_t)uval << 8;
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_IMMEDIATE) val = val->u.addr.val;
                if (touval(val, &uval, 8, epoint2)) {}
                else adr |= (uint8_t)uval;
                ln = 2; 
                adrgen = AG_NONE; opr = ADR_MOVE;
                break;
            } 
            if (cnmemonic[ADR_BIT_ZP] != ____) {
                if (w != 3 && w != 0) return new_error_obj((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
                if (touval(vals->u.list.data[0], &uval, 3, epoint2)) {}
                else longbranch = (uval << 4) & 0x70;
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    adrgen = AG_BYTE;
                } else {
                    adrgen = AG_ZP;
                }
                opr = ADR_BIT_ZP;
                break;
            }
            val = new_error_obj(ERROR___NO_LOT_OPER, epoint);
            val->u.error.u.opers = opers;
            return val;
        case 3:
            if (cnmemonic[ADR_BIT_ZP_REL] != ____) {
                if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
                if (touval(vals->u.list.data[0], &uval, 3, epoint2)) {}
                else longbranch = (uval << 4) & 0x70;
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    if (touval(val, &uval, 8, epoint2)) {}
                    else adr = (uint8_t)uval;
                } else {
                    if (touval(val, &uval, 24, epoint2)) {}
                    else {
                        if (uval <= 0xffff) {
                            adr = (uint16_t)(uval - dpage);
                            if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                    }
                }
                val = vals->u.list.data[2];
                epoint2 = &epoints[2];
                ln = 2; opr = ADR_BIT_ZP_REL;
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_KR) {
                    val = val->u.addr.val;
                    goto justrel2;
                }
                goto justrel;
            }
            /* fall through */
        default: 
            val = new_error_obj(ERROR___NO_LOT_OPER, epoint);
            val->u.error.u.opers = opers;
            return val;
        }
    } 
    switch (adrgen) {
    case AG_ZP: /* zero page address only */
        if (w != 3 && w != 0) return new_error_obj((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
        if (touval(val, &uval, 24, epoint2)) {}
        else if (uval <= 0xffff) {
            adr = (uint16_t)(uval - dpage);
            if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
        ln = 1;
        break;
    case AG_B0: /* bank 0 address only */
        if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 24, epoint2)) {}
        else if (uval <= 0xffff) {
            adr = uval;
            if (cnmemonic[opr] == 0x6c && opcode != w65816.opcode && opcode != c65c02.opcode && opcode != r65c02.opcode && opcode != w65c02.opcode && opcode != c65ce02.opcode && opcode != c65el02.opcode && !(~adr & 0xff)) err_msg2(ERROR______JUMP_BUG, NULL, epoint);/* jmp ($xxff) */
        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
        ln = 2;
        break;
    case AG_PB: /* address in program bank */
        if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 24, epoint2)) {}
        else if ((current_section->l_address ^ uval) <= 0xffff) adr = uval;
        else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
        ln = 2;
        break;
    case AG_BYTE: /* byte only */
        if (w != 3 && w != 0) return new_error_obj((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
        if (touval(val, &uval, 8, epoint2)) {}
        else adr = uval;
        ln = 1;
        break;
    case AG_DB3: /* 3 choice data bank */
        if (w == 3) {/* auto length */
            if (touval(val, &uval, 24, epoint2)) w = (cnmemonic[opr - 1] != ____);
            else if (cnmemonic[opr] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = uval - dpage; w = 0;}
            else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = uval; w = 1;}
            else if (cnmemonic[opr - 2] != ____) {adr = uval; w = 2;}
            else {
                w = (cnmemonic[opr - 1] != ____);
                err_msg2(w ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val, epoint2);
            }
        } else {
            switch (w) {
            case 0:
                if (cnmemonic[opr] == ____) return new_error_obj(ERROR__NO_BYTE_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                break;
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error_obj(ERROR__NO_WORD_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else {
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                }
                break;
            case 2:
                if (cnmemonic[opr - 2] == ____) return new_error_obj(ERROR__NO_LONG_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else adr = uval;
                break;
            default: return new_error_obj(ERROR__NO_LONG_ADDR, epoint); /* can't happen */
            }
        }
        opr = opr - w; ln = w + 1;
        break;
    case AG_DB2: /* 2 choice data bank */
        if (w == 3) {/* auto length */
            if (touval(val, &uval, 24, epoint2)) w = (cnmemonic[opr - 1] != ____);
            else if (cnmemonic[opr] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = uval - dpage; w = 0;}
            else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = uval; w = 1;}
            else {
                w = (cnmemonic[opr - 1] != ____);
                err_msg2(w ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val, epoint2);
            }
        } else {
            switch (w) {
            case 0:
                if (cnmemonic[opr] == ____) return new_error_obj(ERROR__NO_BYTE_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                break;
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error_obj(ERROR__NO_WORD_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else {
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                }
                break;
            default: return new_error_obj(ERROR__NO_LONG_ADDR, epoint);
            }
        }
        opr = opr - w; ln = w + 1;
        break;
    case AG_WORD: /* word only */
        if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 16, epoint2)) {}
        else adr = uval;
        ln = 2;
        break;
    case AG_RELPB:
        if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 16, epoint2)) {}
        else adr = uval - current_section->l_address - 3;
        ln = 2;
        break;
    case AG_RELL:
        if (w != 3 && w != 1) return new_error_obj(w ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 24, epoint2)) {}
        else if ((current_section->l_address ^ uval) <= 0xffff) adr = uval - current_section->l_address - 3;
        else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
        ln = 2;
        break;
    case AG_PB2:
        if (w == 3) {/* auto length */
            if (touval(val, &uval, 24, epoint2)) w = (cnmemonic[ADR_ADDR] == ____) + 1;
            else if (cnmemonic[ADR_ADDR] != ____ && (current_section->l_address ^ uval) <= 0xffff) {adr = uval; w = 1;}
            else {adr = uval; w = 2;}
        } else {
            switch (w) {
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error_obj(ERROR__NO_WORD_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else if ((current_section->l_address ^ uval) <= 0xffff) adr = uval;
                else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                break;
            case 2:
                if (cnmemonic[opr - 2] == ____) return new_error_obj(ERROR__NO_LONG_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else adr = uval;
                break;
            default: return new_error_obj(ERROR__NO_BYTE_ADDR, epoint);
            }
        }
        opr = opr - w; ln = w + 1;
        break;
    case AG_IMP:
        ln = 0;
        break;
    case AG_NONE:
        break;
    }

    cod = cnmemonic[opr];
    if (opr == ADR_REG) cod = regopcode_table[cod][reg];
    if (ln >= 0) {
        uint32_t temp = adr;
        pokeb(cod ^ longbranch);
        switch (ln)
        {
        case 4: pokeb((uint8_t)temp); temp >>= 8;
        case 3: pokeb((uint8_t)temp); temp >>= 8;
        case 2: pokeb((uint8_t)temp); temp >>= 8;
        case 1: pokeb((uint8_t)temp);
        }
    }
    listing_instr(cod ^ longbranch, adr, ln);
    return NULL;
}

