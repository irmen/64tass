/*
    $Id: 64tass.h 1900 2019-02-17 21:46:21Z soci $

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
#ifndef REVISION
#define REVISION "1900?"
#endif
#undef VERSION
#define VERSION "1.54." REVISION
#define MAX_PASS 20

#define ignore() while(pline[lpoint.pos]==0x20 || pline[lpoint.pos]==0x09) lpoint.pos++
#define here() pline[lpoint.pos]

struct Obj;
struct Listing;

extern struct Listing *listing;
extern linepos_t poke_pos;
extern address_t all_mem, all_mem2;
extern unsigned int all_mem_bits;
extern unsigned int outputeor;
extern int temporary_label_branch;
extern line_t vline;
extern struct linepos_s lpoint;
extern struct avltree *star_tree;
extern bool fixeddig, constcreated;
extern address_t star;
extern const uint8_t *pline;
extern uint8_t pass, max_pass;
extern bool referenceit;
extern const struct cpu_s *current_cpu;
extern void new_waitfor(Wait_types, linepos_t);
extern bool close_waitfor(Wait_types);
extern struct Obj *compile(void);
extern FAST_CALL uint8_t *pokealloc(address_t);
extern int main2(int *, char **[]);
#endif
