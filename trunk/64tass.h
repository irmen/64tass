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
#include "wait_e.h"
#ifndef REVISION
#define REVISION "992?"
#endif
#undef VERSION
#define VERSION "1.51." REVISION
#define MAX_PASS 20

struct file_list_s;
struct Label;
struct Obj;

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
extern struct Obj *compile(struct file_list_s *);
extern void var_assign(struct Label *, struct Obj *, int);
extern void pokeb(uint8_t);
#endif
