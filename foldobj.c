/*
    $Id: foldobj.c 2593 2021-04-18 13:00:11Z soci $

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
#include "error.h"

#include "typeobj.h"
#include "strobj.h"
#include "errorobj.h"
#include "boolobj.h"

static Type obj;

Type *const FOLD_OBJ = &obj;

static Fold foldval = { { &obj, 1 }, NULL };

Obj *const fold_value = &foldval.v;

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static MUST_CHECK Obj *hash(Obj *UNUSED(v1), int *hs, linepos_t UNUSED(epoint)) {
    *hs = 0; /* whatever, there's only one */
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *UNUSED(v1), linepos_t UNUSED(epoint), size_t maxsize) {
    Str *v;
    if (3 > maxsize) return NULL;
    v = foldval.repr;
    if (v == NULL) {
        v = new_str2(3);
        if (v == NULL) return NULL;
        v->chars = 3;
        memset(v->data, '.', 3);
        foldval.repr = v;
    }
    return val_reference(Obj(v));
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *v2 = op->v2;
    if (v2->obj->iterable && op->op != O_MEMBER && op->op != O_X) {
        bool minmax = (op->op == O_MIN) || (op->op == O_MAX);
        struct iter_s iter;
        Obj *ret = NULL;
        iter.data = v2; v2->obj->getiter(&iter);

        while ((v2 = iter.next(&iter)) != NULL) {
            Obj *val;
            if (ret == NULL) {
                ret = val_reference(v2);
                continue;
            }
            op->v1 = ret;
            op->v2 = v2;
            op->inplace = (ret->refcount == 1 && !minmax) ? ret : NULL;
            val = ret->obj->calc2(op);
            if (minmax) {
                if (val == true_value) val_replace(&val, ret);
                else if (val == false_value) val_replace(&val, v2);
            }
            val_destroy(ret); ret = val;
        }
        iter_destroy(&iter);
        return ret != NULL ? ret : Obj(new_error(ERROR____EMPTY_LIST, op->epoint2));
    }
    switch (v2->obj->type) {
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
    if (v1->obj->iterable) {
        if (op->op != O_IN) {
            bool minmax = (op->op == O_MIN) || (op->op == O_MAX);
            struct iter_s iter;
            Obj *ret = NULL;
            iter.data = v1; v1->obj->getriter(&iter);

            while ((v1 = iter.next(&iter)) != NULL) {
                Obj *val;
                if (ret == NULL) {
                    ret = val_reference(v1);
                    continue;
                }
                op->v1 = v1;
                op->v2 = ret;
                op->inplace = (ret->refcount == 1 && !minmax) ? ret : NULL;
                val = v1->obj->calc2(op);
                if (minmax) {
                    if (val == true_value) val_replace(&val, v1);
                    else if (val == false_value) val_replace(&val, ret);
                }
                val_destroy(ret); ret = val;
            }
            iter_destroy(&iter);
            return ret != NULL ? ret : Obj(new_error(ERROR____EMPTY_LIST, op->epoint));
        }
    }
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
        return val_reference(v1);
    default:
        break;
    }
    return obj_oper_error(op);
}

void foldobj_init(void) {
    new_type(&obj, T_FOLD, "fold", sizeof(Fold));
    obj.same = same;
    obj.hash = hash;
    obj.repr = repr;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}

void foldobj_destroy(void) {
#ifdef DEBUG
    if (fold_value->refcount != 1) fprintf(stderr, "fold %" PRIuSIZE "\n", fold_value->refcount - 1);
#endif
    if (foldval.repr != NULL) val_destroy(Obj(foldval.repr));
}
