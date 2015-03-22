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
#ifndef _VARIABLES_H_
#define _VARIABLES_H_
#include "inttypes.h"
#include "obj.h"

extern Type *LABEL_OBJ;

typedef struct Namespace Namespace;

typedef struct Label {
    Obj v;
    str_t name;
    str_t cfname;

    Obj *value;
    struct file_list_s *file_list;
    struct linepos_s epoint;
    unsigned ref:1;
    unsigned shadowcheck:1;
    unsigned update_after:1;
    unsigned constant:1;
    uint8_t usepass;
    uint8_t defpass;
    uint8_t strength;
} Label;

extern void labelobj_init(void);

extern void push_context(Namespace *);
extern int pop_context(void);
extern void reset_context(void);

extern Namespace *current_context, *cheap_context, *root_namespace;
extern Label *find_label(const str_t *);
extern Label *find_label2(const str_t *, Namespace *);
extern Label *find_label3(const str_t *, Namespace *, uint8_t);
extern Label *find_anonlabel(int32_t);
extern Label *find_anonlabel2(int32_t, Namespace *);
extern Label *new_label(const str_t *, Namespace *, uint8_t, int *);
extern void labelprint(void);
extern void shadow_check(Namespace *);
extern void destroy_variables(void);
extern void init_variables(void);
extern void destroy_lastlb(void);
extern void new_builtin(const char *, Obj *);
#endif
