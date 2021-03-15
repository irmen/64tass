/*
    $Id: mfuncobj.h 2517 2021-03-14 18:44:48Z soci $

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
#ifndef MFUNCOBJ_H
#define MFUNCOBJ_H
#include "obj.h"
#include "str.h"
#include "stdbool.h"

extern struct Type *const MFUNC_OBJ;
extern struct Type *const SFUNC_OBJ;

struct mfunc_param_s {
    str_t name;
    str_t cfname;
    Obj *init;
    struct linepos_s epoint;
};

typedef struct Mfunc {
    Obj v;
    argcount_t argc;
    struct mfunc_param_s *param;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
    bool retval, single;
    uint8_t recursion_pass;
    size_t nslen;
    struct Namespace **namespaces, *names;
    size_t ipoint;
    struct List *inamespaces;
} Mfunc;
typedef struct Mfunc Sfunc;

#define Mfunc(a) ((Mfunc *)(1 ? (a) : (Obj *)(Mfunc *)(a)))
#define Sfunc(a) ((Sfunc *)(1 ? (a) : (Obj *)(Sfunc *)(a)))

extern void mfuncobj_init(void);
#endif
