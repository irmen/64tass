/*
    $Id: instruction.h 2737 2021-10-06 20:50:52Z soci $

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
#ifndef INSTRUCTION_H
#define INSTRUCTION_H
#include "attributes.h"
#include "stdbool.h"
#include "inttypes.h"
struct cpu_s;

struct Obj;
struct Funcargs;

extern MUST_CHECK struct Error *instruction(int, unsigned int, struct Funcargs *, linepos_t);
extern void select_opcodes(const struct cpu_s *);
extern int lookup_opcode(const uint8_t *);
extern MUST_CHECK bool touval(struct Obj *, uval_t *, unsigned int, linepos_t);
extern MUST_CHECK bool toival(struct Obj *, ival_t *, unsigned int, linepos_t);
extern MUST_CHECK bool touaddress(struct Obj *, uval_t *, unsigned int, linepos_t);
extern MUST_CHECK bool toiaddress(struct Obj *, ival_t *, unsigned int, linepos_t);
extern MUST_CHECK struct Error *err_addressing(uint32_t, linepos_t, int);

extern bool longaccu, longindex, autosize;
extern uint32_t dpage;
extern unsigned int databank;
extern bool longbranchasjmp;
extern bool allowslowbranch;
#endif
