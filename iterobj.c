/*
    $Id: iterobj.c 1925 2019-08-25 11:09:57Z soci $

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
#include "iterobj.h"
#include "values.h"

#include "typeobj.h"

static Type obj;

Type *const ITER_OBJ = &obj;

static FAST_CALL void destroy(Obj *o1) {
    Iter *v1 = (Iter *)o1;
    if (v1->iter != NULL) val_destroy(v1->iter);
    val_destroy(v1->data);
}

static FAST_CALL void garbage(Obj *o1, int i) {
    Iter *v1 = (Iter *)o1;
    Obj *v;
    switch (i) {
    case -1:
        v1->data->refcount--;
        if (v1->iter != NULL) v1->iter->refcount--;
        return;
    case 0:
        return;
    case 1:
        v = v1->iter;
        if (v != NULL) {
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        v = v1->data;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static MUST_CHECK Iter *getiter(Obj *o1) {
    return (Iter *)val_reference(o1);
}

void iterobj_init(void) {
    new_type(&obj, T_ITER, "iter", sizeof(Iter));
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.getiter = getiter;
}

