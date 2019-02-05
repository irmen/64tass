/*
    $Id: foldobj.c 1861 2019-02-03 19:36:52Z soci $

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
#include "foldobj.h"
#include <string.h>
#include "values.h"
#include "eval.h"

#include "typeobj.h"
#include "operobj.h"
#include "strobj.h"
#include "errorobj.h"

static Type obj;

Type *FOLD_OBJ = &obj;
Fold *fold_value;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_FOLD: return val_reference(v1);
    default: break;
    }
    return (Obj *)new_error_conv(v1, FOLD_OBJ, epoint);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static MUST_CHECK Error *hash(Obj *UNUSED(v1), int *hs, linepos_t UNUSED(epoint)) {
    *hs = 0; /* whatever, there's only one */
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *UNUSED(v1), linepos_t UNUSED(epoint), size_t maxsize) {
    Str *v;
    if (3 > maxsize) return NULL;
    v = new_str2(3);
    if (v == NULL) return NULL;
    v->chars = 3;
    memset(v->data, '.', 3);
    return &v->v;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *v2 = op->v2;
    switch (v2->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(v2);
    default:
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *v1 = op->v1;
    switch (op->v1->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(v1);
    default: break;
    }
    return obj_oper_error(op);
}

void foldobj_init(void) {
    new_type(&obj, T_FOLD, "fold", sizeof(Fold));
    obj_init(&obj);
    obj.create = create;
    obj.same = same;
    obj.hash = hash;
    obj.repr = repr;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;

    fold_value = (Fold *)val_alloc(FOLD_OBJ);
}

void foldobj_destroy(void) {
#ifdef DEBUG
    if (fold_value->v.refcount != 1) fprintf(stderr, "fold %" PRIuSIZE "\n", fold_value->v.refcount - 1);
#endif

    val_destroy(&fold_value->v);
}
