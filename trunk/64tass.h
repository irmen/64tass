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
#ifndef _64TASS_H_
#define _64TASS_H_
#include <stdio.h>
#include "inttypes.h"
#include "opcodes.h"
#ifndef REVISION
#define REVISION "883?"
#endif
#undef VERSION
#define VERSION "1.51." REVISION
#define MAX_PASS 20

struct file_list_s;
struct label_s;

enum wait_e {
    W_NONE, W_ENDM, W_ENDM2, W_BEND, W_BEND2, W_HERE, W_HERE2, W_ENDU, W_ENDU2,
    W_ENDS, W_ENDS2, W_ENDC, W_ENDP, W_ENDP2, W_NEXT, W_NEXT2, W_SEND, W_SEND2,
    W_PEND, W_FI, W_FI2, W_ENDF, W_ENDF2, W_SWITCH, W_SWITCH2, W_WEAK, W_WEAK2
};

extern address_t all_mem, all_mem2;
extern uint8_t outputeor;
extern int temporary_label_branch;
extern line_t vline;
extern struct linepos_s lpoint; 
extern struct avltree *star_tree;
extern int fixeddig, constcreated;
extern address_t star;
extern const uint8_t *pline;
extern void status(int);
extern uint16_t reffile;
extern uint32_t backr, forwr;
extern uint8_t pass, max_pass;
extern void new_waitfor(enum wait_e, linepos_t);
extern value_t compile(struct file_list_s *);
extern void var_assign(struct label_s *, value_t, int);
extern void pokeb(uint8_t);
#endif
