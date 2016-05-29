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
#include "obj.h"
#include "64tass.h"
#include "misc.h"
#include "section.h"
#include "file.h"
#include "listing.h"
#include "error.h"
#include "addressobj.h"
#include "listobj.h"
#include "registerobj.h"
#include "codeobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "longjump.h"

static const uint32_t *mnemonic;    /* mnemonics */
static const uint8_t *opcode;       /* opcodes */
static unsigned int last_mnem;
static const struct cpu_s *cpu;

bool longaccu = false, longindex = false, autosize = false; /* hack */
uint32_t dpage = 0;
unsigned int databank = 0;
bool longbranchasjmp = false;
bool allowslowbranch = true;

int lookup_opcode(const uint8_t *s) {
    int32_t s4;
    unsigned int also, felso, elozo, no;
    uint32_t name;

    name = ((uint32_t)s[0] << 16) | (s[1] << 8) | s[2];
    if (arguments.caseinsensitive != 0) name |= 0x202020;
    also = 0;
    no = (felso = last_mnem) / 2;
    for (;;) {  /* do binary search */
        if ((s4 = name - mnemonic[no]) == 0) return no;
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

MUST_CHECK bool touval(Obj *v1, uval_t *uv, int bits, linepos_t epoint) {
    Error *err = v1->obj->uval(v1, uv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

MUST_CHECK bool toaddress(Obj *v1, uval_t *uv, int bits, atype_t *am, linepos_t epoint) {
    Error *err;
    if (am != NULL) *am = A_NONE;
    err = v1->obj->address(v1, uv, bits, am, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

static atype_t get_address_mode(Obj *v1, linepos_t epoint) {
    atype_t am = A_NONE; 
    Error *err = v1->obj->address(v1, NULL, 0, &am, epoint);
    if (err != NULL) {
        err_msg_output_and_destroy(err);
    }
    return am;
}

MUST_CHECK Error *err_addressing(atype_t am, linepos_t epoint) {
    Error *v = new_error(ERROR_NO_ADDRESSING, epoint);
    v->u.addressing = am;
    return v;
}

MUST_CHECK Error *instruction(int prm, int w, Obj *vals, linepos_t epoint, struct linepos_s *epoints) {
    enum { AG_ZP, AG_B0, AG_PB, AG_PB2, AG_BYTE, AG_DB3, AG_DB2, AG_WORD, AG_RELPB, AG_RELL, AG_IMP, AG_NONE } adrgen;
    enum opr_e opr;
    enum reg_e reg;
    const uint8_t *cnmemonic; /* current nmemonic */
    int_fast8_t ln;
    uint8_t cod, longbranch;
    uint32_t adr;
    uval_t uval;
    Obj *val, *oval;
    linepos_t epoint2 = &epoints[0];
    Error *err;

    cnmemonic = opcode_table[opcode[prm]];
    longbranch = 0; reg = REG_A; adr = 0;


    if (vals->obj != ADDRLIST_OBJ) {
        oval = val = vals; goto single;
    } else {
        Addrlist *addrlist;
        atype_t am;
        switch (((Addrlist *)vals)->len) {
        case 0:
            if (cnmemonic[ADR_IMPLIED] != ____) {
                adrgen = AG_IMP; opr = ADR_IMPLIED;
                break;
            }
            return err_addressing(A_NONE, epoint);
        case 1:
            addrlist = (Addrlist *)vals;
            oval = val = addrlist->data[0];
        single:
            am = get_address_mode(val, epoint2);
            switch (am) {
            case A_IMMEDIATE:
                if ((cod = cnmemonic[(opr = ADR_IMMEDIATE)]) == ____ && prm != 0) { /* 0x69 hack */
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
        noneaddr:
            if (val->obj == REGISTER_OBJ) {
                Register *cpureg = (Register *)val;
                cod = cnmemonic[(opr = ADR_REG)];
                if (cod != 0 && cpureg->len == 1) {
                    const char *ind = strchr(reg_names, cpureg->data[0]);
                    if (ind != NULL) {
                        reg = (enum reg_e)(ind - reg_names);
                        if (regopcode_table[cod][reg] != ____) {
                            adrgen = AG_IMP;
                            break;
                        }
                    }
                }
                err = new_error(ERROR___NO_REGISTER, epoint);
                err->u.reg = ref_register(cpureg);
                return err;
            }
            if (cnmemonic[ADR_REL] != ____) {
                struct star_s *s;
                bool labelexists;
                uint16_t oadr, xadr;
                bool labelexists2;
                bool crossbank;
                ln = 1; opr = ADR_REL;
                longbranch = 0;
                if (false) {
            justrel2:
                    if (toaddress(val, &uval, 16, NULL, epoint2)) uval = current_section->l_address.address + 1 + ln;
                    crossbank = false;
                } else {
            justrel: 
                    if (touval(val, &uval, 24, epoint2)) {
                        uval = current_section->l_address.address + 1 + ln;
                        crossbank = false;
                    } else crossbank = ((uval_t)current_section->l_address.bank ^ uval) > 0xffff;
                }
                xadr = adr;
                s = new_star(vline + 1, &labelexists);

                labelexists2 = labelexists;
                oadr = uval;
                if (labelexists2 && oval->obj == ADDRESS_OBJ) {
                    oval = ((Address *)oval)->val;
                }
                if (labelexists2 && oval->obj == CODE_OBJ && pass != ((Code *)oval)->apass) {
                    adr = (uint16_t)(uval - s->addr);
                } else {
                    adr = (uint16_t)(uval - current_section->l_address.address - 1 - ln); labelexists2 = false;
                }
                if ((adr<0xFF80 && adr>0x007F) || crossbank) {
                    if (cnmemonic[ADR_REL_L] != ____ && !crossbank) { /* 65CE02 long branches */
                    asbrl:
                        if (!labelexists2) adr = (uint16_t)adr; /* same + 2 offset! */
                        opr = ADR_REL_L;
                        ln = 2;
                    } else if (arguments.longbranch && (cnmemonic[ADR_ADDR] == ____)) { /* fake long branches */
                        if ((cnmemonic[ADR_REL] & 0x1f) == 0x10) {/* bxx branch */
                            bool exists;
                            struct longjump_s *lj = new_longjump(uval, &exists);
                            if (exists && lj->defpass == pass) {
                                if (((uval_t)current_section->l_address.bank ^ (uval_t)lj->dest) <= 0xffff) {
                                    uint16_t adrk = lj->dest - current_section->l_address.address - 2;
                                    if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                        adr = adrk;
                                        goto branchok;
                                    }
                                }
                            }
                            cod = cnmemonic[ADR_REL] ^ 0x20;
                            ln = labelexists ? ((uint16_t)(s->addr - star - 2)) : 3;
                            pokeb(cod);
                            pokeb(ln);
                            listing_instr(cod, ln, 1);
                            lj->dest = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
                            lj->defpass = pass;
                            err = instruction((cpu->brl >= 0 && !longbranchasjmp && !crossbank) ? cpu->brl : cpu->jmp, w, vals, epoint, epoints);
                            if (labelexists && s->addr != ((current_section->l_address.address & 0xffff) | current_section->l_address.bank)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = false;
                            }
                            s->addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
                            return err;
                        }
                        if (opr == ADR_BIT_ZP_REL) {
                            bool exists;
                            struct longjump_s *lj = new_longjump(uval, &exists);
                            if (crossbank) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                            if (exists && lj->defpass == pass) {
                                if (((uval_t)current_section->l_address.bank ^ (uval_t)lj->dest) <= 0xffff) {
                                    uint16_t adrk = lj->dest - current_section->l_address.address - 3;
                                    if (adrk >= 0xFF80 || adrk <= 0x007F) {
                                        adr = adrk;
                                        goto branchok;
                                    }
                                }
                            }
                            cod = cnmemonic[ADR_BIT_ZP_REL] ^ 0x80 ^ longbranch;
                            pokeb(cod);
                            pokeb(xadr);
                            pokeb(3);
                            listing_instr(cod, xadr | 0x300, 2);
                            lj->dest = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
                            lj->defpass = pass;
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
                            asjmp:
                                err = instruction(cpu->jmp, w, vals, epoint, epoints);
                                if (labelexists && s->addr != ((current_section->l_address.address & 0xffff) | current_section->l_address.bank)) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig = false;
                                }
                                s->addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
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
                    if (((uint16_t)(current_section->l_address.address + 1 + ln) & 0xff00) != (oadr & 0xff00)) {
                        if (!allowslowbranch) err_msg2(ERROR__BRANCH_CROSS, NULL, epoint);
                    }
                    if (cnmemonic[ADR_ADDR] != ____) { /* gcc */
                        if (adr == 0) {
                            listing_instr(0, 0, -1);
                            if (labelexists && s->addr != ((current_section->l_address.address & 0xffff) | current_section->l_address.bank)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = false;
                            }
                            s->addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
                            return NULL;
                        } 
                        if (adr == 1 && (cnmemonic[ADR_REL] & 0x1f) == 0x10) {
                            cod = cnmemonic[ADR_REL] ^ 0x20;
                            pokeb(cod);
                            listing_instr(cod, 0, 0);
                            if (labelexists && s->addr != ((current_section->l_address.address & 0xffff) | current_section->l_address.bank)) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = false;
                            }
                            s->addr = (current_section->l_address.address & 0xffff) | current_section->l_address.bank;
                            return NULL;
                        }
                    }
                }
            branchok:
                if (labelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = false;
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
            if (val->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, epoint2);
            }
            err = new_error(ERROR___NO_LOT_OPER, epoint);
            err->u.opers = 1;
            return err;
        case 2:
            addrlist = (Addrlist *)vals;
            if (cnmemonic[ADR_MOVE] != ____) {
                if (toaddress(addrlist->data[0], &uval, 8, &am, epoint2)) {}
                else {
                    if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2));
                    else adr = (uint16_t)uval << 8;
                }
                epoint2 = &epoints[1];
                if (toaddress(addrlist->data[1], &uval, 8, &am, epoint2)) {}
                else {
                    if (am != A_NONE && am != A_IMMEDIATE) err_msg_output_and_destroy(err_addressing(am, epoint2));
                    else adr |= (uint8_t)uval;
                }
                ln = 2; 
                adrgen = AG_NONE; opr = ADR_MOVE;
                break;
            } 
            if (cnmemonic[ADR_BIT_ZP] != ____) {
                if (w != 3 && w != 0) return new_error((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
                if (touval(addrlist->data[0], &uval, 3, epoint2)) {}
                else longbranch = (uval << 4) & 0x70;
                val = addrlist->data[1];
                epoint2 = &epoints[1];
                am = get_address_mode(val, epoint2);
                if (am == A_DR) {
                    adrgen = AG_BYTE;
                } else {
                    if (am != A_NONE) err_msg_output_and_destroy(err_addressing(am, epoint2));
                    adrgen = AG_ZP;
                }
                opr = ADR_BIT_ZP;
                break;
            }
            if (addrlist->data[0]->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, epoint2);
            }
            if (addrlist->data[1]->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, &epoints[1]);
            }
            err = new_error(ERROR___NO_LOT_OPER, epoint);
            err->u.opers = 2;
            return err;
        case 3:
            addrlist = (Addrlist *)vals;
            if (cnmemonic[ADR_BIT_ZP_REL] != ____) {
                if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
                if (touval(addrlist->data[0], &uval, 3, epoint2)) {}
                else longbranch = (uval << 4) & 0x70;
                val = addrlist->data[1];
                epoint2 = &epoints[1];
                am = get_address_mode(val, epoint2);
                if (am == A_DR) {
                    if (toaddress(val, &uval, 8, NULL, epoint2)) {}
                    else adr = (uint8_t)uval;
                } else {
                    if (am != A_NONE) err_msg_output_and_destroy(err_addressing(am, epoint2));
                    else if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                    else {
                        if (uval <= 0xffff) {
                            adr = (uint16_t)(uval - dpage);
                            if (adr > 0xff || dpage > 0xffff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                    }
                }
                oval = val = addrlist->data[2];
                epoint2 = &epoints[2];
                ln = 2; opr = ADR_BIT_ZP_REL;
                am = get_address_mode(val, epoint2);
                if (am == A_KR) {
                    goto justrel2;
                }
                goto justrel;
            }
            /* fall through */
        default: 
            addrlist = (Addrlist *)vals;
            if (addrlist->data[0]->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, epoint2);
            }
            if (addrlist->data[1]->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, &epoints[1]);
            }
            if (addrlist->data[2]->obj == NONE_OBJ) {
                return new_error(ERROR____STILL_NONE, &epoints[2]);
            }
            {
                size_t i;
                for (i = 3; i < addrlist->len; i++) {
                    if (addrlist->data[i]->obj == NONE_OBJ) {
                        return new_error(ERROR____STILL_NONE, epoint);
                    }
                }
            }
            err = new_error(ERROR___NO_LOT_OPER, epoint);
            err->u.opers = addrlist->len;
            return err;
        }
    } 
    switch (adrgen) {
    case AG_ZP: /* zero page address only */
        if (w != 3 && w != 0) return new_error((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
        if (toaddress(val, &uval, 24, NULL, epoint2)) {}
        else if (uval <= 0xffff) {
            adr = (uint16_t)(uval - dpage);
            if (adr > 0xff || dpage > 0xffff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
        ln = 1;
        break;
    case AG_B0: /* bank 0 address only */
        if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (toaddress(val, &uval, 24, NULL, epoint2)) {}
        else if (uval <= 0xffff) {
            adr = uval;
            if (cnmemonic[opr] == 0x6c && opcode != w65816.opcode && opcode != c65c02.opcode && opcode != r65c02.opcode && opcode != w65c02.opcode && opcode != c65ce02.opcode && opcode != c4510.opcode && opcode != c65el02.opcode && (~adr & 0xff) == 0) err_msg2(ERROR______JUMP_BUG, NULL, epoint);/* jmp ($xxff) */
        } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
        ln = 2;
        break;
    case AG_PB: /* address in program bank */
        if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (toaddress(val, &uval, 24, NULL, epoint2)) {}
        else if ((current_section->l_address.bank ^ uval) <= 0xffff) adr = uval;
        else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
        ln = 2;
        break;
    case AG_BYTE: /* byte only */
        if (w != 3 && w != 0) return new_error((w == 1) ? ERROR__NO_WORD_ADDR : ERROR__NO_LONG_ADDR, epoint);
        if (toaddress(val, &uval, 8, NULL, epoint2)) {}
        else {
            adr = uval;
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
        }
        ln = 1;
        break;
    case AG_DB3: /* 3 choice data bank */
        if (w == 3) {/* auto length */
            if (toaddress(val, &uval, 24, NULL, epoint2)) w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
            else if (cnmemonic[opr] != ____ && uval <= 0xffff && dpage <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = uval - dpage; w = 0;}
            else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = uval; w = 1;}
            else if (cnmemonic[opr - 2] != ____) {adr = uval; w = 2;}
            else {
                w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
                err_msg2((w != 0) ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val, epoint2);
            }
        } else {
            switch (w) {
            case 0:
                if (cnmemonic[opr] == ____) return new_error(ERROR__NO_BYTE_ADDR, epoint);
                if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff || dpage > 0xffff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                break;
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error(ERROR__NO_WORD_ADDR, epoint);
                if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                else {
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                }
                break;
            case 2:
                if (cnmemonic[opr - 2] == ____) return new_error(ERROR__NO_LONG_ADDR, epoint);
                if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                else adr = uval;
                break;
            default: return new_error(ERROR__NO_LONG_ADDR, epoint); /* can't happen */
            }
        }
        opr = (enum opr_e)(opr - w); ln = w + 1;
        break;
    case AG_DB2: /* 2 choice data bank */
        if (w == 3) {/* auto length */
            if (toaddress(val, &uval, 24, NULL, epoint2)) w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
            else if (cnmemonic[opr] != ____ && uval <= 0xffff && dpage <= 0xffff && (uint16_t)(uval - dpage) <= 0xff) {adr = uval - dpage; w = 0;}
            else if (cnmemonic[opr - 1] != ____ && databank == (uval >> 16)) {adr = uval; w = 1;}
            else {
                w = (cnmemonic[opr - 1] != ____) ? 1 : 0;
                err_msg2((w != 0) ? ERROR__NOT_DATABANK : ERROR____NOT_DIRECT, val, epoint2);
            }
        } else {
            switch (w) {
            case 0:
                if (cnmemonic[opr] == ____) return new_error(ERROR__NO_BYTE_ADDR, epoint);
                if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                else if (uval <= 0xffff) {
                    adr = (uint16_t)(uval - dpage);
                    if (adr > 0xff || dpage > 0xffff) err_msg2(ERROR____NOT_DIRECT, val, epoint2);
                } else err_msg2(ERROR_____NOT_BANK0, val, epoint2);
                break;
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error(ERROR__NO_WORD_ADDR, epoint);
                if (toaddress(val, &uval, 24, NULL, epoint2)) {}
                else {
                    adr = uval;
                    if (databank != (uval >> 16)) err_msg2(ERROR__NOT_DATABANK, val, epoint2);
                }
                break;
            default: return new_error(ERROR__NO_LONG_ADDR, epoint);
            }
        }
        opr = (enum opr_e)(opr - w); ln = w + 1;
        break;
    case AG_WORD: /* word only */
        if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (toaddress(val, &uval, 16, NULL, epoint2)) {}
        else adr = uval;
        ln = 2;
        break;
    case AG_RELPB:
        if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (toaddress(val, &uval, 16, NULL, epoint2)) {}
        else adr = uval - current_section->l_address.address - ((opcode != c65ce02.opcode && opcode != c4510.opcode) ? 3 : 2);
        ln = 2;
        break;
    case AG_RELL:
        if (w != 3 && w != 1) return new_error((w != 0) ? ERROR__NO_LONG_ADDR : ERROR__NO_BYTE_ADDR, epoint);
        if (touval(val, &uval, 24, epoint2)) {}
        else if ((current_section->l_address.bank ^ uval) <= 0xffff) adr = uval - current_section->l_address.address - ((opcode != c65ce02.opcode && opcode != c4510.opcode) ? 3 : 2);
        else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
        ln = 2;
        break;
    case AG_PB2:
        if (w == 3) {/* auto length */
            if (touval(val, &uval, 24, epoint2)) w = (cnmemonic[ADR_ADDR] == ____) ? 2 : 1;
            else if (cnmemonic[ADR_ADDR] != ____ && (current_section->l_address.bank ^ uval) <= 0xffff) {adr = uval; w = 1;}
            else {adr = uval; w = 2;}
        } else {
            switch (w) {
            case 1:
                if (cnmemonic[opr - 1] == ____) return new_error(ERROR__NO_WORD_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else if ((current_section->l_address.bank ^ uval) <= 0xffff) adr = uval;
                else err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                break;
            case 2:
                if (cnmemonic[opr - 2] == ____) return new_error(ERROR__NO_LONG_ADDR, epoint);
                if (touval(val, &uval, 24, epoint2)) {}
                else adr = uval;
                break;
            default: return new_error(ERROR__NO_BYTE_ADDR, epoint);
            }
        }
        opr = (enum opr_e)(opr - w); ln = w + 1;
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

