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

void select_opcodes(int cpumode) {
    last_mnem = cpumode;
    switch (cpumode) {
    case OPCODES_C65C02: mnemonic = mnemonic_c65c02; opcode = c65c02; break;
    case OPCODES_C65CE02: mnemonic = mnemonic_c65ce02; opcode = c65ce02; break;
    case OPCODES_C6502I: mnemonic = mnemonic_c6502i; opcode = c6502i; break;
    case OPCODES_C65816: mnemonic = mnemonic_c65816; opcode = c65816; break;
    case OPCODES_C65DTV02: mnemonic = mnemonic_c65dtv02; opcode = c65dtv02; break;
    case OPCODES_C65EL02: mnemonic = mnemonic_c65el02; opcode = c65el02; break;
    case OPCODES_CR65C02: mnemonic = mnemonic_cr65c02; opcode = cr65c02; break;
    case OPCODES_CW65C02: mnemonic = mnemonic_cw65c02; opcode = cw65c02; break;
    default: mnemonic = mnemonic_c6502; opcode = c6502; break;
    }
}

int instruction(int prm, int w, address_t all_mem, struct value_s *vals, linepos_t epoint, struct linepos_s *epoints) {
    enum { AG_ZP, AG_B0, AG_PB, AG_BYTE, AG_DB3, AG_WORD, AG_NONE } adrgen;
    enum opr_e opr;
    const uint8_t *cnmemonic; /* current nmemonic */
    int_fast8_t ln;
    uint8_t cod, longbranch;
    uint32_t adr;
    int d;
    uval_t uval;
    struct value_s err, *val;
    linepos_t epoint2 = &epoints[0];

    opr = ADR_IMPLIED;
    cnmemonic = &opcode[prm * ADR_LEN];
    ln = 0; cod = 0; longbranch = 0; adr = 0; adrgen = AG_NONE;

    if (vals->obj == ADDRLIST_OBJ && vals->u.list.len < 1) {
        opr = (cnmemonic[ADR_ACCU] == cnmemonic[ADR_IMPLIED]) ? ADR_ACCU : ADR_IMPLIED; w = ln = 0; d = 1;
    }  /* clc */
    else {
        val = (vals->obj == ADDRLIST_OBJ) ? vals->u.list.data[0] : vals;
        if (val->obj == NONE_OBJ) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
            d = fixeddig = 0;
        } else d = 1;

        if (val->obj == ADDRESS_OBJ) {
            atype_t am = val->u.addr.type;
            val = val->u.addr.val;
            if (val->obj == NONE_OBJ) {
                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                d = fixeddig = 0;
            }
            switch (am) {
            case A_IMMEDIATE:
                if ((cod = cnmemonic[(opr = ADR_IMMEDIATE)]) == ____ && prm) { /* 0x69 hack */
                    ln = w = d = 1;
                    break;
                } 
                if (cnmemonic[ADR_MOVE] != ____) goto noneaddr;
                ln = w = 1;
                if (cod == 0xE0 || cod == 0xC0 || cod == 0xA2 || cod == 0xA0) {/* cpx cpy ldx ldy */
                    if (longindex) ln++;
                }
                else if (cod == 0xF4) ln = 2; /* pea/phw #$ffff */
                else if (cod != 0xC2 && cod != 0xE2 && cod != 0x00 && cod != 0x02 && cod != 0xEF) {/* not sep rep brk cop mmu=all accu */
                    if (longaccu) ln++;
                }

                if (d) {
                    if (val->obj->uval(val, &err, &uval, ln * 8, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else adr = uval;
                }
                break;
            case A_BR:
                if (cnmemonic[ADR_ADDR] != 0x4C && cnmemonic[ADR_ADDR] != 0x20 && cnmemonic[ADR_ADDR] != 0xF4) {/* jmp $ffff, jsr $ffff, pea */
                    adrgen = AG_WORD; opr = ADR_ADDR; /* lda $ffff,b */
                } else w = 0;
                break;
            case A_KR:
                if (cnmemonic[ADR_ADDR] == 0x4C || cnmemonic[ADR_ADDR] == 0x20) {/* jmp $ffff, jsr $ffff */
                    adrgen = AG_WORD; opr = ADR_ADDR; /* jmp $ffff */
                } else w = 0;
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
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_wrong_type(&err, epoint2); w = (cnmemonic[ADR_ADDR_Y] != ____);}
                        else if (cnmemonic[ADR_ZP_Y] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = (uint16_t)(uval - dpage); w = 0;}
                        else if (databank == (uval >> 16)) {adr = (uint16_t)uval; w = 1;}
                        else {err_msg2(ERROR_NO_ADDRESSING, NULL, epoint); w = (cnmemonic[ADR_ADDR_Y] != ____);}
                    } else w = (cnmemonic[ADR_ADDR_Y] != ____);
                } else if (d) {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else if (!w && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) adr = (uint16_t)(uval - dpage);
                    else if (w == 1 && databank == (uval >> 16)) adr = (uint16_t)uval;
                    else {err_msg2(ERROR_NO_ADDRESSING, NULL, epoint); w = cnmemonic[ADR_ADDR_Y] != ____;}
                } else if (w > 1) {err_msg2(ERROR_NO_ADDRESSING, NULL, epoint); w = (cnmemonic[ADR_ADDR_Y] != ____);}
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
                    if (d && opcode != c65816 && opcode != c65c02 && opcode != cr65c02 && opcode != cw65c02 && opcode != c65ce02 && opcode != c65el02 && !(~adr & 0xff)) err_msg(ERROR______JUMP_BUG, NULL);/* jmp ($xxff) */
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
        } else if (val->obj == REGISTER_OBJ && val->u.reg.len == 1 && val->u.reg.data[0] == 'a') {
            if (vals->obj == ADDRLIST_OBJ && vals->u.list.len != 1) return 2;
            opr = ADR_ACCU;
            w = ln = 0; 
        } else {
        noneaddr:
            if (cnmemonic[ADR_MOVE] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 2) return 2;
                w = 0;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else adr = (uint16_t)uval << 8;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_IMMEDIATE) val = val->u.addr.val;
                if (val->obj == NONE_OBJ) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                    d = fixeddig = 0;
                } else {
                    if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else adr |= (uint8_t)uval;
                }
                ln = 2; opr = ADR_MOVE;
            } else if (cnmemonic[ADR_BIT_ZP] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 2) return 2;
                w = 0;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 3, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else longbranch = (uval << 4) & 0x70;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == NONE_OBJ) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                    d = fixeddig = 0;
                } else if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                        d = fixeddig = 0;
                    } else d = 1;
                    adrgen = AG_BYTE;
                } else {
                    d = 1;
                    adrgen = AG_ZP;
                }
                ln = 1; opr = ADR_BIT_ZP;
            } else if (cnmemonic[ADR_BIT_ZP_REL] != ____) {
                if (vals->obj != ADDRLIST_OBJ || vals->u.list.len != 3) return 2;
                w = 0;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 3, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else longbranch = (uval << 4) & 0x70;
                }
                val = vals->u.list.data[1];
                epoint2 = &epoints[1];
                if (val->obj == NONE_OBJ) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                    d = fixeddig = 0;
                } else if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_DR) {
                    val = val->u.addr.val;
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                        d = fixeddig = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_wrong_type(&err, epoint2);
                        else adr = (uint8_t)uval;
                    }
                } else {
                    if (val->obj->uval(val, &err, &uval, 16, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, NULL, epoint2); 
                }
                val = vals->u.list.data[2];
                epoint2 = &epoints[2];
                if (val->obj == NONE_OBJ) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                    d = fixeddig = 0;
                } else {
                    if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_KR) {
                        val = val->u.addr.val;
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                            d = fixeddig = 0;
                        } else {
                            if (val->obj->uval(val, &err, &uval, 16, epoint2)) {err_msg_wrong_type(&err, epoint2); uval = current_section->l_address + 3;}
                        }
                    } else {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_wrong_type(&err, epoint2); uval = current_section->l_address + 3;}
                        else if (((uval_t)current_section->l_address ^ uval) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                    }
                    uval = (uint16_t)(uval - current_section->l_address - 3);
                    if (uval >= 0xFF80 || uval <= 0x007F) {
                        adr |= ((uint8_t)uval) << 8;
                    } else {
                        int dist = (int16_t)uval;
                        dist += (dist < 0) ? 0x80 : -0x7f;
                        err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint);
                    }
                }
                ln = 2; opr = ADR_BIT_ZP_REL;
            } else if (vals->obj == ADDRLIST_OBJ && vals->u.list.len != 1) return 2;
            else if (cnmemonic[ADR_REL] != ____) {
                struct star_s *s;
                int labelexists;
                uint16_t oadr;
                w = 0;
                s = new_star(vline + 1, &labelexists);

                ln = 1; opr = ADR_REL;
                if (d) {
                    int labelexists2 = labelexists;
                    if (val->obj == ADDRESS_OBJ && val->u.addr.type == A_KR) {
                        val = val->u.addr.val;
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint2);
                            d = fixeddig = 0;
                        } else {
                            if (val->obj->uval(val, &err, &uval, 16, epoint2)) {err_msg_wrong_type(&err, epoint2); uval = current_section->l_address + 2;}
                        }
                    } else {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_wrong_type(&err, epoint2); uval = current_section->l_address + 2;}
                        else if (((uval_t)current_section->l_address ^ uval) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                    }
                    oadr = uval;
                    if (val->obj == CODE_OBJ) {
                        if (labelexists2 && pass != val->u.code.pass) {
                            adr = (uint16_t)(uval - s->addr);
                        } else {
                            adr = (uint16_t)(uval - current_section->l_address - 2); labelexists2 = 0;
                        }
                    } else {
                        if (labelexists2 && uval >= s->addr) {
                            adr = (uint16_t)(uval - s->addr);
                        } else {
                            adr = (uint16_t)(uval - current_section->l_address - 2); labelexists2 = 0;
                        }
                    }
                    longbranch = 0;
                    if (adr<0xFF80 && adr>0x007F) {
                        if (cnmemonic[ADR_REL_L] != ____) {
                            if (!labelexists2) adr = (uint16_t)(adr - 1);
                            opr = ADR_REL_L;
                            ln = 2;
                        } else if (arguments.longbranch && (cnmemonic[ADR_ADDR] == ____)) {
                            if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {/* branch */
                                longbranch = 0x20; ln = 4;
                                if (opcode == c65816 && !longbranchasjmp) {
                                    if (!labelexists2) adr = (uint16_t)(adr - 3);
                                    adr = 0x8203 + (adr << 16);
                                } else {
                                    adr = 0x4C03 + (oadr << 16);
                                }
                            } else {/* bra */
                                if (opcode == c65816 && !longbranchasjmp) {
                                    longbranch = cnmemonic[ADR_REL] ^ 0x82;
                                    if (!labelexists2) adr = (uint16_t)(adr - 1);
                                    ln = 2;
                                } else if (cnmemonic[ADR_REL] == 0x82 && opcode == c65el02) {
                                    int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                                    err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint); /* rer not a branch */
                                } else {
                                    longbranch = cnmemonic[ADR_REL] ^ 0x4C;
                                    adr = oadr; ln = 2;
                                }
                            }
                            /* erro = ERROR___LONG_BRANCH; */
                        } else {
                            if (cnmemonic[ADR_ADDR] != ____) {
                                if (opcode == c65816 && !longbranchasjmp) {
                                    longbranch = cnmemonic[ADR_REL]^0x82;
                                    if (!labelexists2) adr = (uint16_t)(adr - 1);
                                } else {
                                    adr = oadr;
                                    opr = ADR_ADDR;
                                }
                                ln = 2;
                            } else {
                                int dist = (int16_t)adr; dist += (dist < 0) ? 0x80 : -0x7f;
                                err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint);
                            }
                        }
                    } else {
                        if (!longbranch && ((uint16_t)(current_section->l_address + 2) & 0xff00) != (oadr & 0xff00)) {
                            if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, NULL, epoint);
                        }
                        if (cnmemonic[ADR_ADDR] != ____) {
                            if (adr == 0) ln = -1;
                            else if (adr == 1 && (cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                                ln = 0; longbranch = 0x20; adr = 0x10000;
                            }
                        }
                    }
                }
                if (labelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = 0;
                }
                s->addr = (star + 1 + ln) & all_mem;
            }
            else if (cnmemonic[ADR_REL_L] != ____) {
                w = 0;
                if (d) {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_wrong_type(&err, epoint2);
                    if ((current_section->l_address ^ uval) <= 0xffff) {
                        adr = (uint16_t)(uval - current_section->l_address - 3);
                    } else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                }
                opr = ADR_REL_L; ln = 2; /* brl */
            }
            else if (cnmemonic[ADR_LONG] == 0x5C) {
                if (w == 3) {/* auto length */
                    if (d) {
                        if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_wrong_type(&err, epoint2); w = (cnmemonic[ADR_ADDR] == ____) + 1;}
                        else if (cnmemonic[ADR_ADDR] != ____ && (current_section->l_address ^ uval) <= 0xffff) {adr = uval; w = 1;}
                        else {adr = uval; w = 2;}
                    } else w = (cnmemonic[ADR_ADDR] == ____) + 1;
                } else if (d) {
                    if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_wrong_type(&err, epoint2);
                    else if (w == 1 && (current_section->l_address ^ uval) <= 0xffff) adr = uval;
                    else if (w == 2) adr = uval;
                }
                opr = ADR_ZP - w; ln = w + 1; /* jml */
            } else if (cnmemonic[ADR_ADDR] == 0x20) {
                adrgen = AG_PB; opr = ADR_ADDR; /* jsr $ffff */
            } else if (cnmemonic[ADR_ADDR] == 0xF4) {
                adrgen = AG_WORD; opr = ADR_ADDR; /* pea $ffff */
            } else {
                adrgen = AG_DB3; opr = ADR_ZP; /* lda $ff lda $ffff lda $ffffff */
            }
        }
    }
    switch (adrgen) {
    case AG_ZP: /* zero page address only */
        if (w == 3) w = 0;/* auto length */
        else if (w != 0) err_msg2(ERROR_NO_ADDRESSING, NULL, epoint); 
        if (d) {
            if (val->obj->uval(val, &err, &uval, 16, epoint2)) err_msg_wrong_type(&err, epoint2);
            else adr = (uint16_t)(uval - dpage);
            if (adr > 0xff) err_msg2(ERROR____NOT_DIRECT, NULL, epoint2); 
        }
        ln = 1;
        break;
    case AG_B0: /* bank 0 address only */
        if (w == 3) w = 1;/* auto length */
        else if (w != 1) err_msg2(ERROR_NO_ADDRESSING, NULL, epoint);
        if (d) {
            if (val->obj->uval(val, &err, &uval, 16, epoint2)) err_msg_wrong_type(&err, epoint2);
            else adr = uval;
        }
        ln = 2;
        break;
    case AG_PB: /* address in program bank */
        if (w == 3) w = 1;/* auto length */
        else if (w != 1) err_msg2(ERROR_NO_ADDRESSING, NULL, epoint);
        if (d) {
            if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_wrong_type(&err, epoint2);
            else if ((current_section->l_address ^ uval) <= 0xffff) adr = (uint16_t)uval;
            else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
        }
        ln = 2;
        break;
    case AG_BYTE: /* byte only */
        if (w == 3) w = 0;/* auto length */
        else if (w != 0) err_msg2(ERROR_NO_ADDRESSING, NULL, epoint);
        if (d) {
            if (val->obj->uval(val, &err, &uval, 8, epoint2)) err_msg_wrong_type(&err, epoint2);
            else adr = uval;
        }
        ln = 1;
        break;
    case AG_DB3: /* 3 choice data bank */
        if (w == 3) {/* auto length */
            if (d) {
                if (val->obj->uval(val, &err, &uval, 24, epoint2)) {err_msg_wrong_type(&err, epoint2); w = (cnmemonic[opr - 1] != ____);}
                else if (cnmemonic[opr] != ____ && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = (uint16_t)(uval - dpage); w = 0;}
                else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = (uint16_t)uval; w = 1;}
                else {adr = uval; w = 2;}
            } else w = (cnmemonic[opr - 1] != ____);
        } else if (d) {
            if (val->obj->uval(val, &err, &uval, 24, epoint2)) err_msg_wrong_type(&err, epoint2);
            else if (!w && uval <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) adr = (uint16_t)(uval - dpage);
            else if (w == 1 && databank == (adr >> 16)) adr = (uint16_t)uval;
            else if (w == 2) adr = uval;
        }
        opr = opr - w; ln = w + 1;
        break;
    case AG_WORD: /* word only */
        if (w == 3) w = 1;/* auto length */
        else if (w != 1) err_msg2(ERROR_NO_ADDRESSING, NULL, epoint);
        if (d) {
            if (val->obj->uval(val, &err, &uval, 16, epoint2)) err_msg_wrong_type(&err, epoint2);
            else adr = uval;
        }
        ln = 2;
        break;
    case AG_NONE:
        break;
    }

    if (d) {
        if (w == 3) {err_msg(ERROR_CONSTNT_LARGE, NULL); return 1;}
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
    list_instr(cod ^ longbranch, adr, ln, opr, mnemonic[prm]);
    return 0;
}

