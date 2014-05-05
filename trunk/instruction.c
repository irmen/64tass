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

static const uint32_t *mnemonic;    /* mnemonics */
static const uint8_t *opcode;       /* opcodes */
static unsigned int last_mnem;
static const struct cpu_s *cpu;

int longaccu = 0, longindex = 0; // hack
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

static const uint32_t relopnames[8] = { 0x62706C, 0x626d69, 0x627663, 0x627673, 0x626363, 0x626373, 0x626E65, 0x626571 };

int instruction(int prm, int w, address_t all_mem, struct value_s *vals, linepos_t epoint, struct linepos_s *epoints) {
    enum { AG_ZP, AG_B0, AG_PB, AG_BYTE, AG_DB3, AG_WORD, AG_NONE } adrgen;
    enum opr_e opr;
    const uint8_t *cnmemonic; /* current nmemonic */
    int_fast8_t ln;
    uint8_t cod, longbranch;
    uint32_t adr, opname;
    int d;
    uval_t uval;
    struct value_s err, *val;
    linepos_t epoint2 = &epoints[0];

    opr = ADR_IMPLIED;
    cnmemonic = &opcode[prm * ADR_LEN];
    opname = mnemonic[prm];
    ln = 0; cod = 0; longbranch = 0; adr = 0; adrgen = AG_NONE;

    if (vals->obj == ADDRLIST_OBJ && vals->u.list.len < 1) {
        opr = (cnmemonic[ADR_ACCU] == cnmemonic[ADR_IMPLIED]) ? ADR_ACCU : ADR_IMPLIED; w = ln = 0; d = 1;
    }  /* clc */
    else {
        val = (vals->obj == ADDRLIST_OBJ) ? vals->u.list.data[0] : vals;
        if (val->obj == NONE_OBJ) {
            err_msg_still_none(NULL, epoint2);
            d = 0;
        } else d = 1;

        if (val->obj == ADDRESS_OBJ) {
            atype_t am = val->u.addr.type;
            val = val->u.addr.val;
            if (val->obj == NONE_OBJ) {
                err_msg_still_none(NULL, epoint2);
                d = 0;
            }
            switch (am) {
            case A_IMMEDIATE:
                if ((cod = cnmemonic[(opr = ADR_IMMEDIATE)]) == ____ && prm) { /* 0x69 hack */
                    ln = d = 1;
                    break;
                }
                if (cnmemonic[ADR_MOVE] != ____) goto noneaddr;
                ln = 1;
                if (cod == 0xE0 || cod == 0xC0 || cod == 0xA2 || cod == 0xA0) {/* cpx cpy ldx ldy */
                    if (longindex) ln++;
                }
                else if (cod == 0xF4) ln = 2; /* pea/phw #$ffff */
                else if (cod != 0xC2 && cod != 0xE2 && cod != 0x00 && cod != 0x02 && cod != 0xEF) {/* not sep rep brk cop mmu=all accu */
                    if (longaccu) ln++;
                }

                if (d) {
                    if (val->obj->uval(val, &err, &uval, ln * 8, epoint2)) err_msg_output_and_destroy(&err);
                    else adr = uval;
                }
                break;
            case A_BR:
                if (cnmemonic[ADR_ADDR] != 0x4C && cnmemonic[ADR_ADDR] != 0x20 && cnmemonic[ADR_ADDR] != 0xF4) {/* jmp $ffff, jsr $ffff, pea */
                    adrgen = AG_WORD; opr = ADR_ADDR; /* lda $ffff,b */
                }
                break;
            case A_KR:
                if (cnmemonic[ADR_ADDR] == 0x4C || cnmemonic[ADR_ADDR] == 0x20) {/* jmp $ffff, jsr $ffff */
                    adrgen = AG_WORD; opr = ADR_ADDR; /* jmp $ffff */
                }
                break;
            case A_DR:
                adrgen = AG_BYTE; opr = ADR_ZP; /* lda $ff,d */
                break;
            case (A_BR << 4) | A_XR:
                adrgen = AG_WORD; opr = ADR_ADDR_X; /* lda $ffff,b,x */
                break;
            case (A_DR << 4) | A_XR:
                adrgen = AG_BYTE; opr = ADR_ZP_X; /* lda $ff,d,x */
                break;
            case A_XR:
                adrgen = AG_DB3; opr = ADR_ZP_X; /* lda $ff,x lda $ffff,x lda $ffffff,x */
                break;
            case (A_BR << 4) | A_YR:
                adrgen = AG_WORD; opr = ADR_ADDR_Y; /* ldx $ffff,b,y */
                break;
            case (A_DR << 4) | A_YR:
                adrgen = AG_BYTE; opr = ADR_ZP_Y; /* ldx $ff,d,y */
                break;
            case A_YR: /* lda $ff,y lda $ffff,y lda $ffffff,y */
                if (w == 3) {/* auto length */
                    if (d) {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_output_and_destroy(&err); w = (cnmemonic[ADR_ADDR_Y] != ____);}
                        else if (cnmemonic[ADR_ZP_Y] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = (uint16_t)(uval - dpage); w = 0;}
                        else if (databank == (uval >> 16)) {adr = (uint16_t)uval; w = 1;}
                        else return 2;
                    } else w = (cnmemonic[ADR_ADDR_Y] != ____);
                } else if (d) {
                    if (!w && cnmemonic[ADR_ZP_Y] != ____) {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                        else if (uval <= 0xffff) {
                            adr = (uint16_t)(uval - dpage);
                            if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                    } else if (w == 1 && cnmemonic[ADR_ADDR_Y] != ____) {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                        else {
                            adr = (uint16_t)uval;
                            if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                        }
                    } else return 2;
                } else if (w > 1) return 2;
                opr = ADR_ZP_Y - w; ln = w + 1; /* ldx $ff,y lda $ffff,y */
                break;
            case A_SR:
                adrgen = AG_BYTE; opr = ADR_ZP_S; /* lda $ff,s */
                break;
            case A_RR:
                adrgen = AG_BYTE; opr = ADR_ZP_R; /* lda $ff,r */
                break;
            case (A_DR << 8) | (A_I << 4) | A_YR:
                adrgen = AG_BYTE; opr = ADR_ZP_I_Y; /* lda ($ff,d),y */
                break;
            case (A_I << 4) | A_YR:
                adrgen = AG_ZP; opr = ADR_ZP_I_Y; /* lda ($ff),y */
                break;
            case (A_DR << 8) | (A_I << 4) | A_ZR:
                adrgen = AG_BYTE; opr = ADR_ZP_I_Z; /* lda ($ff,d),z */
                break;
            case (A_I << 4) | A_ZR:
                adrgen = AG_ZP; opr = ADR_ZP_I_Z; /* lda ($ff),z */
                break;
            case (A_SR << 8) | (A_I << 4) | A_YR:
                adrgen = AG_BYTE; opr = ADR_ZP_S_I_Y; /* lda ($ff,s),y */
                break;
            case (A_RR << 8) | (A_I << 4) | A_YR:
                adrgen = AG_BYTE; opr = ADR_ZP_R_I_Y; /* lda ($ff,r),y */
                break;
            case (A_DR << 8) | (A_LI << 4) | A_YR:
                adrgen = AG_BYTE; opr = ADR_ZP_LI_Y; /* lda [$ff,d],y */
                break;
            case (A_LI << 4) | A_YR:
                adrgen = AG_ZP; opr = ADR_ZP_LI_Y; /* lda [$ff],y */
                break;
            case (A_XR << 4) | A_I:
                if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
                    adrgen = AG_PB; opr = ADR_ADDR_X_I; /* jmp ($ffff,x) */
                } else {
                    adrgen = AG_ZP; opr = ADR_ZP_X_I; /* lda ($ff,x) */
                }
                break;
            case (A_KR << 8) | (A_XR << 4) | A_I:
                if (cnmemonic[ADR_ADDR_X_I] == 0x7C || cnmemonic[ADR_ADDR_X_I] == 0xFC || cnmemonic[ADR_ADDR_X_I] == 0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
                    adrgen = AG_WORD; opr = ADR_ADDR_X_I; /* jmp ($ffff,k,x) */
                }
                break;
            case (A_DR << 8) | (A_XR << 4) | A_I:
                adrgen = AG_BYTE; opr = ADR_ZP_X_I; /* lda ($ff,d,x) */
                break;
            case A_I:
                if (cnmemonic[ADR_ADDR_I] == 0x6C || cnmemonic[ADR_ADDR_I] == 0x22) {/* jmp ($ffff), jsr ($ffff) */
                    adrgen = AG_B0; opr = ADR_ADDR_I; /* jmp ($ffff) */
                } else {
                    adrgen = AG_ZP; opr = ADR_ZP_I; /* lda ($ff) */
                }
                break;
            case (A_DR << 4) | A_I:
                adrgen = AG_BYTE; opr = ADR_ZP_I; /* lda ($ff,d) */
                break;
            case A_LI:
                if (cnmemonic[ADR_ADDR_LI] == 0xDC) { /* jmp [$ffff] */
                    adrgen = AG_B0; opr = ADR_ADDR_LI; /* jmp [$ffff] */
                } else {
                    adrgen = AG_ZP; opr = ADR_ZP_LI; /* lda [$ff] */
                }
                break;
            case (A_DR << 4) | A_LI:
                adrgen = AG_BYTE; opr = ADR_ZP_LI; /* lda [$ff,d] */
                break;
            case A_NONE:
                goto noneaddr;
            default: return 2; /* non-existing */
            }
            if (vals->obj == ADDRLIST_OBJ && vals->u.list.len != 1) return 2;
        } else if (val->obj == REGISTER_OBJ && val->u.str.len == 1 && val->u.str.data[0] == 'a') {
            if (vals->obj == ADDRLIST_OBJ && vals->u.list.len != 1) return 2;
            opr = ADR_ACCU;
            ln = 0;
        } else {
        noneaddr:
            if (cnmemonic[ADR_MOVE] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 2) return 2;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_output_and_destroy(&err);
                    else adr = (uint16_t)uval << 8;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_IMMEDIATE) val = val->u.addr.val;
                if (val->obj == NONE_OBJ) {
                    err_msg_still_none(NULL, epoint2);
                    d = 0;
                } else {
                    if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_output_and_destroy(&err);
                    else adr |= (uint8_t)uval;
                }
                ln = 2; opr = ADR_MOVE;
            } else if (cnmemonic[ADR_BIT_ZP] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 2) return 2;
                if (w != 3 && w != 0) return 2;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 3, epoint2)) err_msg_output_and_destroy(&err);
                    else longbranch = (uval << 4) & 0x70;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == NONE_OBJ) {
                    err_msg_still_none(NULL, epoint2);
                    d = 0;
                } else if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    if (val->obj == NONE_OBJ) {
                        err_msg_still_none(NULL, epoint2);
                        d = 0;
                    } else d = 1;
                    adrgen = AG_BYTE;
                } else {
                    d = 1;
                    adrgen = AG_ZP;
                }
                ln = 1; opr = ADR_BIT_ZP;
            } else if (cnmemonic[ADR_BIT_ZP_REL] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 3) return 2;
                if (w != 3 && w != 0) return 2;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 3, epoint2)) err_msg_output_and_destroy(&err);
                    else longbranch = (uval << 4) & 0x70;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == NONE_OBJ) {
                    err_msg_still_none(NULL, epoint2);
                    d = 0;
                } else if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    if (val->obj == NONE_OBJ) {
                        err_msg_still_none(NULL, epoint2);
                        d = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_output_and_destroy(&err);
                        else adr = (uint8_t)uval;
                    }
                } else {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
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
                if (val->obj == NONE_OBJ) {
                    err_msg_still_none(NULL, epoint2);
                    d = 0;
                }
                goto justrel;
            } else if (vals->obj == ADDRLIST_OBJ && vals->u.list.len != 1) return 2;
            else if (cnmemonic[ADR_REL] != ____) {
                struct star_s *s;
                int labelexists;
                uint16_t oadr, xadr;
                ln = 1; opr = ADR_REL;
                longbranch = 0;
            justrel: xadr = adr;
                s = new_star(vline + 1, &labelexists);

                if (d) {
                    int labelexists2 = labelexists;
                    int crossbank = 0;
                    if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_KR) {
                        val = val->u.addr.val;
                        if (val->obj == NONE_OBJ) {
                            err_msg_still_none(NULL, epoint2);
                            d = 0;
                            uval = current_section->l_address + 1 + ln;
                        } else {
                            if (val->obj->uval(val, &err, &uval, 16, epoint2)) {err_msg_output_and_destroy(&err); uval = current_section->l_address + 2;}
                        }
                    } else {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_output_and_destroy(&err); uval = current_section->l_address + 2;}
                        crossbank = ((uval_t)current_section->l_address ^ uval) > 0xffff;
                    }
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
                                int r;
                                cod = cnmemonic[ADR_REL] ^ 0x20;
                                ln = labelexists ? ((uint16_t)(s->addr - star - 2)) : 3;
                                pokeb(cod);
                                pokeb(ln);
                                list_instr(cod, ln, 1, ADR_REL, relopnames[cod >> 5]);
                                r = instruction((cpu->brl >= 0 && !longbranchasjmp && !crossbank) ? cpu->brl : cpu->jmp, w, all_mem, vals, epoint, epoints);
                                if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = 0;
                                }
                                s->addr = current_section->l_address & all_mem;
                                return r;
                            } else if (opr == ADR_BIT_ZP_REL) {
                                if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                                cod = cnmemonic[ADR_BIT_ZP_REL] ^ 0x80 ^ longbranch;
                                pokeb(cod);
                                pokeb(xadr);
                                pokeb(3);
                                list_instr(cod, xadr | 0x300, 2, ADR_BIT_ZP_REL, (cnmemonic[ADR_BIT_ZP_REL] & 0x80) ? 0x626272 : 0x626273);
                                adr = oadr; opr = ADR_ADDR; ln = 2;
                                prm = cpu->jmp; longbranch = 0;
                                cnmemonic = &opcode[prm * ADR_LEN];
                                opname = mnemonic[prm];
                            } else {/* bra */
                                if (cpu->brl >= 0 && !longbranchasjmp) { /* bra -> brl */
                                asbrl2:
                                    if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                                    else {
                                        prm = cpu->brl;
                                        cnmemonic = &opcode[prm * ADR_LEN];
                                        opname = mnemonic[prm];
                                        goto asbrl;
                                    }
                                } else if (cnmemonic[ADR_REL] == 0x82 && opcode == c65el02.opcode) { /* not a branch ! */
                                    int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                                    if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                                    else err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint); /* rer not a branch */
                                } else { /* bra -> jmp */
                                    int r;
                                asjmp:
                                    r = instruction(cpu->jmp, w, all_mem, vals, epoint, epoints);
                                    if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                        fixeddig = 0;
                                    }
                                    s->addr = current_section->l_address & all_mem;
                                    return r;
                                }
                            }
                            /* erro = ERROR___LONG_BRANCH; */
                        } else if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                            if (cpu->brl >= 0 && !longbranchasjmp) goto asbrl2; /* gcc -> brl */
                            if (crossbank) goto asjmp; /* gcc -> jmp */
                            if (cnmemonic[ADR_ADDR] == 0x4C) {
                                opname = 0x6A6D70;
                            }
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
                    } else if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                    else { /* short */
                        if (((uint16_t)(current_section->l_address + 1 + ln) & 0xff00) != (oadr & 0xff00)) {
                            if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, NULL, epoint);
                        }
                        if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                            if (adr == 0) {
                                list_instr(0, 0, -1, ADR_IMPLIED, 0x202020);
                                if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = 0;
                                }
                                s->addr = current_section->l_address & all_mem;
                                return 0;
                            } else if (adr == 1 && (cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                                ln = 0; adr = 0x10000;
                                cod = cnmemonic[ADR_REL] ^ 0x20;
                                pokeb(cod);
                                list_instr(cod, 0, 0, ADR_IMPLIED, relopnames[cod >> 5]);
                                if (labelexists && s->addr != (current_section->l_address & all_mem)) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = 0;
                                }
                                s->addr = current_section->l_address & all_mem;
                                return 0;
                            }
                            if ((opname & 0xff0000) == 0x670000) opname ^= 0x670000 ^ 0x620000;
                        }
                    }
                }
                if (labelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = 0;
                }
                s->addr = (star + 1 + ln) & all_mem;
                if (opr == ADR_BIT_ZP_REL) adr = xadr | (adr << 8);
            }
            else if (cnmemonic[ADR_REL_L] != ____) {
                if (w != 3 && w != 1) return 2;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                    else {
                        adr = (uint16_t)(uval - current_section->l_address - 3);
                        if ((current_section->l_address ^ uval) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                    }
                }
                opr = ADR_REL_L; ln = 2; /* brl */
            }
            else if (cnmemonic[ADR_LONG] == 0x5C) {
                if (w == 3) {/* auto length */
                    if (d) {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_output_and_destroy(&err); w = (cnmemonic[ADR_ADDR] == ____) + 1;}
                        else if (cnmemonic[ADR_ADDR] != ____ && (current_section->l_address ^ uval) <= 0xffff) {adr = uval; w = 1;}
                        else {adr = uval; w = 2;}
                    } else w = (cnmemonic[ADR_ADDR] == ____) + 1;
                } else if (d) {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                    else if (w == 1 && cnmemonic[ADR_ADDR] != ____) {
                        adr = uval;
                        if ((current_section->l_address ^ uval) > 0xffff) {err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint); w = 2;}
                    } else if (w == 2) adr = uval;
                    else return 2;
                }
                opr = ADR_ZP - w; ln = w + 1; /* jml */
            } else if (cnmemonic[ADR_ADDR] == 0x20 || cnmemonic[ADR_ADDR] == 0x4C) {
                adrgen = AG_PB; opr = ADR_ADDR; /* jsr $ffff, jmp */
            } else if (cnmemonic[ADR_ADDR] == 0xF4) {
                adrgen = AG_WORD; opr = ADR_ADDR; /* pea $ffff */
            } else {
                adrgen = AG_DB3; opr = ADR_ZP; /* lda $ff lda $ffff lda $ffffff */
            }
        }
    }
    switch (adrgen) {
    case AG_ZP: /* zero page address only */
        if (w != 3 && w != 0) return 2;
        if (d) {
            if (cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
            }
        }
        ln = 1;
        break;
    case AG_B0: /* bank 0 address only */
        if (w != 3 && w != 1) return 2;
        if (d) {
            if (cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else if (uval <= 0xffff) {
                    adr = uval;
                    if (cnmemonic[opr] == 0x6c && opcode != w65816.opcode && opcode != c65c02.opcode && opcode != r65c02.opcode && opcode != w65c02.opcode && opcode != c65ce02.opcode && opcode != c65el02.opcode && !(~adr & 0xff)) err_msg2(ERROR______JUMP_BUG, NULL, epoint);/* jmp ($xxff) */
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
            }
        }
        ln = 2;
        break;
    case AG_PB: /* address in program bank */
        if (w != 3 && w != 1) return 2;
        if (d) {
            if (cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else if ((current_section->l_address ^ uval) <= 0xffff) adr = (uint16_t)uval;
                else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
            }
        }
        ln = 2;
        break;
    case AG_BYTE: /* byte only */
        if (w != 3 && w != 0) return 2;
        if (d) {
            if (cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_output_and_destroy(&err);
                else adr = uval;
            }
        }
        ln = 1;
        break;
    case AG_DB3: /* 3 choice data bank */
        if (w == 3) {/* auto length */
            if (d) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_output_and_destroy(&err); w = (cnmemonic[opr - 1] != ____);}
                else if (cnmemonic[opr] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = (uint16_t)(uval - dpage); w = 0;}
                else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = (uint16_t)uval; w = 1;}
                else {adr = uval; w = 2;}
            } else w = (cnmemonic[opr - 1] != ____);
        } else if (d) {
            if (!w && cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
            } else if (w == 1 && cnmemonic[opr - 1] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else {
                    adr = (uint16_t)uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                }
            } else if (w == 2 && cnmemonic[opr - 2] != ____) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_output_and_destroy(&err);
                else adr = uval;
            }
            else return 2;
        }
        opr = opr - w; ln = w + 1;
        break;
    case AG_WORD: /* word only */
        if (w != 3 && w != 1) return 2;
        if (d) {
            if (cnmemonic[opr] != ____) {
                if (val->obj->uval(val, &err, &uval, 16, epoint2)) err_msg_output_and_destroy(&err);
                else adr = uval;
            }
        }
        ln = 2;
        break;
    case AG_NONE:
        break;
    }

    if (d) {
        if ((cod = cnmemonic[opr]) == ____ && (prm || opr != ADR_IMMEDIATE)) { /* 0x69 hack */
            return 2;
        }
    }

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
    list_instr(cod ^ longbranch, adr, ln, opr, opname);
    return 0;
}

