/*
    $Id: anonsymbolobj.c 2385 2021-02-20 12:25:17Z soci $

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
#include "anonsymbolobj.h"
#include <string.h>
#include "eval.h"
#include "values.h"

#include "typeobj.h"
#include "operobj.h"
#include "strobj.h"
#include "errorobj.h"

static Type obj;

Type *const ANONSYMBOL_OBJ = &obj;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_ANONSYMBOL: return val_reference(v1);
    default: break;
    }
    return (Obj *)new_error_conv(v1, ANONSYMBOL_OBJ, epoint);
}

Anonsymbol *new_anonsymbol(int32_t count) {
    Anonsymbol *anonsymbol = (Anonsymbol *)val_alloc(ANONSYMBOL_OBJ);
    anonsymbol->count = count;
    return anonsymbol;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Anonsymbol *v1 = (const Anonsymbol *)o1, *v2 = (const Anonsymbol *)o2;
    if (o1->obj != o2->obj) return false;
    return v1->count == v2->count;
}

static MUST_CHECK struct Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Anonsymbol *v1 = (Anonsymbol *)o1;
    unsigned int h = v1->count;
    h &= ((~0U) >> 1);
    *hs = h;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Anonsymbol *v1 = (Anonsymbol *)o1;
    Str *v;
    size_t len = v1->count < 0 ? (1 - v1->count) : (v1->count + 2);
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    v->data[0] = '.';
    memset(v->data + 1, v1->count >= 0 ? '+' : '-', len);
    return &v->v;
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Anonsymbol *v1 = (Anonsymbol *)o1;
    Str *v;
    size_t len = v1->count < 0 ? -v1->count : (v1->count + 1);
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    memset(v->data, v1->count >= 0 ? '+' : '-', len);
    return &v->v;
}

static inline int icmp(oper_t op) {
    const int32_t v1 = ((Anonsymbol *)op->v1)->count, v2 = ((Anonsymbol *)op->v2)->count;
    return v1 - v2;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;
    switch (o2->obj->type) {
    case T_ANONSYMBOL: 
        return obj_oper_compare(op, icmp(op));
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != &o_MEMBER && op->op != &o_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

void anonsymbolobj_init(void) {
    new_type(&obj, T_ANONSYMBOL, "anonsymbol", sizeof(Anonsymbol));
    obj.create = create;
    obj.same = same;
    obj.hash = hash;
    obj.repr = repr;
    obj.str = str;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
