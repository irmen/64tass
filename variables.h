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
#include "libtree.h"
#include "inttypes.h"

extern obj_t LABEL_OBJ;

typedef struct {
    str_t name;
    str_t cfname;

    value_t value;
    struct file_list_s *file_list;
    struct linepos_s epoint;
    unsigned ref:1;
    unsigned shadowcheck:1;
    unsigned update_after:1;
    unsigned constant:1;
    uint8_t usepass;
    uint8_t defpass;
    uint8_t strength;
} label_t;

extern void push_context(value_t);
extern int pop_context(void);
extern void reset_context(void);

extern void labelobj_init(void);
extern value_t current_context, cheap_context, root_dict;
extern value_t find_label(const str_t *);
extern value_t find_label2(const str_t *, value_t);
extern value_t find_label3(const str_t *, value_t, uint8_t);
extern value_t find_anonlabel(int32_t);
extern value_t find_anonlabel2(int32_t, value_t);
extern value_t new_label(const str_t *, value_t, uint8_t, int *);
extern void labelprint(void);
extern void shadow_check(value_t);
extern void destroy_variables(void);
extern void init_variables(void);
extern void init_defaultlabels(void);
#endif
