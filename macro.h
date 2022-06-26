/*
    $Id: macro.h 2787 2022-05-25 04:26:09Z soci $

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
#include "inttypes.h"
#include "wait_e.h"
#include "stdbool.h"

struct Funcargs;
struct Namespace;
struct Mfunc;
struct Obj;

extern bool in_macro;
extern bool in_function;

extern bool mtranslate(void);
extern struct Obj *macro_recurse(Wait_types, struct Obj *, struct Namespace *, linepos_t);
extern struct Obj *mfunc_recurse(struct Mfunc *, struct Namespace *, uint8_t, linepos_t);
extern struct Obj *mfunc2_recurse(struct Mfunc *, struct Funcargs *, linepos_t);
extern void init_macro(void);
extern void free_macro(void);
extern void get_macro_params(struct Obj *);
extern bool get_func_params(struct Mfunc *, bool);
extern const struct file_list_s *macro_error_translate(struct linepos_s *, linecpos_t);
extern linecpos_t macro_error_translate2(linecpos_t);
#endif
