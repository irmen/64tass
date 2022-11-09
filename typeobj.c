/*
    $Id: typeobj.c 2896 2022-11-05 05:33:41Z soci $

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
#include "typeobj.h"
#include <string.h>
#include "variables.h"
#include "eval.h"

#include "strobj.h"
#include "functionobj.h"

static Type obj;

Type *const TYPE_OBJ = &obj;

static struct Slot *values_free[MAXIMUM_TYPE_LENGTH];

#define ALIGN sizeof(int *)

Type *new_type(Type *t, Type_types type, const char *name, size_t length) {
    t->v.obj = TYPE_OBJ;
    t->v.refcount = 1;
    t->type = type;
    t->length = (length + (ALIGN - 1)) / ALIGN;
    t->slot = &values_free[t->length];
    t->name = name;
    obj_init(t);
    return t;
}

static MUST_CHECK Obj *type_from_obj(Obj *v1, linepos_t UNUSED(epoint)) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR: return val_reference(v1);
    default: break;
    }
    return val_reference((Obj *)Obj(v1->obj));
}

static MUST_CHECK Obj *convert(oper_t op) {
    return type_from_obj(op->v2, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    *hs = (int)Type(o1)->type;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Type *v1 = Type(o1);
    Str *v;
    uint8_t *s;
    const char *name;
    size_t ln, ln2;
    if (epoint == NULL) return NULL;
    name = v1->name;
    ln = strlen(name);
    ln2 = ln + 9;
    if (ln2 > maxsize) return NULL;
    v = new_str2(ln2);
    if (v == NULL) return NULL;
    v->chars = ln2;
    s = v->data;
    memcpy(s, "<type '", 7);
    s += 7;
    memcpy(s, name, ln);
    s[ln] = '\'';
    s[ln + 1] = '>';
    return Obj(v);
}

static inline int icmp(oper_t op) {
    Type_types v1 = Type(op->v1)->type;
    Type_types v2 = Type(op->v2)->type;
    return (v1 < v2) ? -1 : (v1 > v2) ? 1 : 0;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;

    switch (o2->obj->type) {
    case T_TYPE:
        return obj_oper_compare(op, icmp(op));
    case T_FUNCARGS:
        if (op->op == O_FUNC) {
            const Type *v1 = Type(op->v1);
            Funcargs *v2 = Funcargs(o2);
            argcount_t args = v2->len;
            if (args != 1) {
                return apply_convert2(op);
            }
            op->v2 = v2->val->val;
            op->inplace = op->v2->refcount == 1 ? op->v2 : NULL;
            if (v1->iterable || v1 == TYPE_OBJ) {
                return v1->convert(op);
            }
            return apply_function(op, v1->convert);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != O_MEMBER && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

void init_type(void) {
    memset(values_free, 0, sizeof values_free);
}

void typeobj_init(void) {
    Type *type = new_type(&obj, T_TYPE, "type", sizeof(Type));
    type->convert = convert;
    type->same = same;
    type->hash = hash;
    type->repr = repr;
    type->calc2 = calc2;
}

void typeobj_names(void) {
    new_builtin("type", val_reference(Obj(TYPE_OBJ)));
}
