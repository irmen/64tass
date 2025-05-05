/*
    $Id: opcodes.h 3226 2025-04-25 05:34:13Z soci $

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
#ifndef OPCODES_H
#define OPCODES_H
#include "inttypes.h"

#define ____ 0x69

typedef enum Adr_types {
    ADR_REG, ADR_IMPLIED, ADR_IMMEDIATE, ADR_LONG, ADR_ADDR, ADR_ZP, ADR_LONG_X,
    ADR_ADDR_X, ADR_ZP_X, ADR_ZP_X_I, ADR_ZP_S, ADR_ZP_S_I_Y, ADR_ADDR_Y,
    ADR_ZP_Y, ADR_ZP_I_Y, ADR_ZP_I, ADR_ZP_LI_Y, ADR_ZP_LI, ADR_REL_L, ADR_REL,
    ADR_BIT_ZP_REL, ADR_BIT_ZP, ADR_ZP_R_I_Y, ADR_ZP_R, ADR_ADDR_K_X_I,
    ADR_ADDR_0_LI, ADR_ADDR_0_I, ADR_ZP_I_Z, ADR_ZP_LI_Z, ADR_ADDR_K, ADR_MOVE,
    ADR_LEN
} Adr_types;

typedef enum Opr_types {
    OPR_REG, OPR_IMPLIED, OPR_IMMEDIATE, OPR_LONG, OPR_ADDR, OPR_ZP, OPR_LONG_X,
    OPR_ADDR_X, OPR_ZP_X, OPR_ZP_X_I, OPR_ZP_S, OPR_ZP_S_I_Y, OPR_ADDR_Y,
    OPR_ZP_Y, OPR_ZP_I_Y, OPR_ZP_I, OPR_ZP_LI_Y, OPR_ZP_LI, OPR_REL_L, OPR_REL,
    OPR_BIT_ZP_REL, OPR_BIT_ZP = 5, OPR_ZP_R_I_Y = 16, OPR_ZP_R = 1,
    OPR_ADDR_K_X_I = 9, OPR_ADDR_0_LI = 17, OPR_ADDR_0_I = 15, OPR_ZP_I_Z = 15,
    OPR_ZP_LI_Z, OPR_ADDR_K = 4, OPR_MOVE = 2, OPR_LEN = 21
} Opr_types;

typedef enum Reg_types {
    REG_A, REG_X, REG_Y, REG_S, REG_D, REG_R, REG_I, REG_Z, REG_B, REG_K, REG_P,
    REG_Q, REG_LEN
} Reg_types;

struct cpu_s {
    const char *name;
    const uint32_t *mnemonic;
    const uint16_t *opcode;
    const uint16_t *disasm;
    const uint8_t *alias;
    const uint32_t registers;
    unsigned int opcodes;
    address_t max_address;
    int jmp, brl, ldr, lda, ldx, ldy, ldz, str, sta, stx, sty, stz, cmp, cpa;
    int cpx, cpy, cpz, tcd, txi, txr, adc, sbc, and_, orr, eor, bit, ora, tsb, trb;
    int adq, anq, ard, btq, cpq, ded, eoq, ind, ldq, orq, rld, rrd, sbq, asd, lsd, stq, inq, deq;
};

extern const char *reg_names;
extern const uint8_t regopcode_table[][REG_LEN];

extern const char *const addr_modes[ADR_LEN];
extern const char *const addr_names[ADR_LEN];
extern const uint32_t opcode_table_modes[];
extern const uint8_t opcode_table[][OPR_LEN];

extern const struct cpu_s w65816;
extern const struct cpu_s c6502;
extern const struct cpu_s c65c02;
extern const struct cpu_s c6502i;
extern const struct cpu_s c65dtv02;
extern const struct cpu_s c65el02;
extern const struct cpu_s r65c02;
extern const struct cpu_s w65c02;
extern const struct cpu_s c65ce02;
extern const struct cpu_s c4510;
extern const struct cpu_s c45gs02;

#endif
