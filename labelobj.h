/*
    $Id: labelobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef LABELOBJ_H
#define LABELOBJ_H
#include "obj.h"
#include "str.h"
#include "stdbool.h"

extern struct Type *const LABEL_OBJ;

typedef struct Label {
    Obj v;
    str_t name;
    str_t cfname;
    int hash;

    Obj *value;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
    bool ref;
    bool update_after;
    bool constant;
    bool owner;
    uint8_t usepass;
    uint8_t defpass;
    uint8_t strength;
    uint8_t fwpass;
} Label;

#define Label(a) OBJ_CAST(Label, a)

extern void labelobj_init(void);

static inline Label *ref_label(Label *v1) {
    v1->v.refcount++; return v1;
}

#endif
