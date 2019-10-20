/*
    $Id: macroobj.h 2016 2019-10-20 06:41:22Z soci $

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
#ifndef MACROOBJ_H
#define MACROOBJ_H
#include "obj.h"
#include "str.h"
#include "stdbool.h"

extern struct Type *const MACRO_OBJ;
extern struct Type *const SEGMENT_OBJ;
extern struct Type *const STRUCT_OBJ;
extern struct Type *const UNION_OBJ;

struct macro_param_s {
    str_t cfname;
    str_t init;
};

typedef struct Macro {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    const struct file_list_s *file_list;
    line_t line;
    bool retval;
    uint8_t recursion_pass;
} Macro;
typedef struct Macro Segment;

typedef struct Struct {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    const struct file_list_s *file_list;
    line_t line;
    bool retval;
    uint8_t recursion_pass;
    address_t size; /* first part same as macro! */
    struct Namespace *names;
} Struct;
typedef struct Struct Union;

extern void macroobj_init(void);

#endif
