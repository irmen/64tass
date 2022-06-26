/*
    $Id: 64tass.h 2786 2022-05-25 04:08:01Z soci $

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
#ifndef _64TASS_H
#define _64TASS_H
#include "attributes.h"
#include "stdbool.h"
#include "inttypes.h"
#include "wait_e.h"

#define MAX_PASS 20

#define ignore() while(pline[lpoint.pos]==0x20 || pline[lpoint.pos]==0x09) lpoint.pos++
#define here() pline[lpoint.pos]

struct Obj;

extern address_t all_mem, all_mem2;
extern unsigned int all_mem_bits;
extern uint32_t outputeor;
extern linenum_t vline;
extern struct linepos_s lpoint;
extern struct star_s *star_tree;
extern bool fixeddig, constcreated;
extern address_t star;
extern const uint8_t *pline;
extern uint8_t pass, max_pass;
extern bool referenceit;
extern const struct cpu_s *current_cpu;
extern void new_waitfor(Wait_types, linepos_t);
extern bool close_waitfor(Wait_types);
extern struct Obj *compile(void);
extern FAST_CALL uint8_t *pokealloc(address_t, linepos_t);
extern int main2(int *, char **[]);
#endif
