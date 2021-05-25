/*
    $Id: operobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef OPEROBJ_H
#define OPEROBJ_H
#include "obj.h"
#include "oper_e.h"

extern struct Type *const OPER_OBJ;

typedef struct Oper {
    Obj v;
    const char *const name;
    const Oper_types op;
    const unsigned int prio;
    const unsigned int len;
} Oper;

#define Oper(a) OBJ_CAST(Oper, a)

extern void operobj_init(void);

extern Oper operators[];

#endif
