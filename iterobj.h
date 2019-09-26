/*
    $Id: iterobj.h 1928 2019-08-25 14:10:00Z soci $

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
#ifndef ITEROBJ_H
#define ITEROBJ_H
#include "obj.h"

extern struct Type *const ITER_OBJ;

struct Iter;
typedef FAST_CALL MUST_CHECK Obj *(*iter_next_t)(struct Iter *);
typedef size_t (*iter_len_t)(struct Iter *);

typedef struct Iter {
    Obj v;
    Obj *iter;
    size_t val;
    Obj *data;
    iter_next_t next;
    size_t len;
} Iter;

extern void iterobj_init(void);

#endif
