/*
    $Id: macroobj.h 2651 2021-05-09 19:33:48Z soci $

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
    argcount_t argc;
    linenum_t line;
    struct macro_param_s *param;
    const struct file_list_s *file_list;
    bool retval;
    uint8_t recursion_pass;
} Macro;
typedef struct Macro Segment;

#define Macro(a) OBJ_CAST(Macro, a)
#define Segment(a) OBJ_CAST(Segment, a)

typedef struct Struct {
    Obj v;
    argcount_t argc;
    linenum_t line;
    struct macro_param_s *param;
    const struct file_list_s *file_list;
    bool retval;
    uint8_t recursion_pass;
    address_t size; /* first part same as macro! */
    struct Namespace *names;
} Struct;
typedef struct Struct Union;

#define Struct(a) OBJ_CAST(Struct, a)
#define Union(a) OBJ_CAST(Union, a)

extern void macroobj_init(void);

#endif
