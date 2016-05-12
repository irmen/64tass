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
#ifndef MACRO_H
#define MACRO_H
#include "obj.h"
#include "wait_e.h"

extern struct Type *MACRO_OBJ;
extern struct Type *SEGMENT_OBJ;

typedef struct Macro {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    struct file_list_s *file_list;
    line_t line;
    bool retval;
} Macro;
typedef struct Macro Segment;

extern void macroobj_init(void);

struct values_s;
struct file_s;

struct Namespace;
struct Mfunc;

extern bool mtranslate(struct file_s *);
extern Obj *macro_recurse(enum wait_e, Obj *, struct Namespace *, linepos_t);
extern Obj *mfunc_recurse(enum wait_e, struct Mfunc *, struct Namespace *, linepos_t, uint8_t);
extern Obj *mfunc2_recurse(struct Mfunc *, struct values_s *, unsigned int, linepos_t);
extern void init_macro(void);
extern void free_macro(void);
extern void get_macro_params(Obj *);
extern void get_func_params(struct Mfunc *, struct file_s *);
extern bool in_macro(void);
#endif
