/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#ifndef _OPCODES_H_
#define _OPCODES_H_
#include <stdint.h>

#define ____ 0x69
enum opr_e {
    ADR_IMPLIED=0, ADR_ACCU, ADR_IMMEDIATE, ADR_LONG, ADR_ADDR, ADR_ZP,
    ADR_LONG_X, ADR_ADDR_X, ADR_ZP_X, ADR_ADDR_X_I, ADR_ZP_X_I, ADR_ZP_S,
    ADR_ZP_S_I_Y, ADR_ADDR_Y, ADR_ZP_Y, ADR_ZP_LI_Y, ADR_ZP_I_Y, ADR_ADDR_LI,
    ADR_ZP_LI, ADR_ADDR_I, ADR_ZP_I, ADR_REL_L, ADR_REL, ADR_MOVE,
    ADR_ZP_R, ADR_ZP_R_I_Y
};

extern const uint8_t c65816[];
extern const char mnemonic_c65816[];
#define OPCODES_C65816 111

extern const uint8_t c6502[];
extern const char mnemonic_c6502[];
#define OPCODES_C6502 68

extern const uint8_t c65c02[];
extern const char mnemonic_c65c02[];
#define OPCODES_C65C02 79

extern const uint8_t c6502i[];
extern const char mnemonic_c6502i[];
#define OPCODES_C6502I 98

extern const uint8_t c65dtv02[];
extern const char mnemonic_c65dtv02[];
#define OPCODES_C65DTV02 89

extern const uint8_t c65el02[];
extern const char mnemonic_c65el02[];
#define OPCODES_C65EL02 118

#endif
