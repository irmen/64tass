/*
    $Id: noneobj.c 2690 2021-09-08 09:56:34Z soci $

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
#ifdef DEBUG
#include <stdio.h>
#endif
#include "noneobj.h"
#include "eval.h"

#include "typeobj.h"
#include "errorobj.h"

static Type obj;

Type *const NONE_OBJ = &obj;

static None noneval = { { &obj, 1} };

Obj *const none_value = &noneval.v;

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static MUST_CHECK Obj *truth(Obj *UNUSED(v1), Truth_types UNUSED(type), linepos_t epoint) {
    return Obj(new_error(ERROR____STILL_NONE, epoint));
}

static MUST_CHECK Obj *hash(Obj *UNUSED(v1), int *UNUSED(v), linepos_t epoint) {
    return Obj(new_error(ERROR____STILL_NONE, epoint));
}

static MUST_CHECK Obj *repr(Obj *v1, linepos_t epoint, size_t UNUSED(maxsize)) {
    return (epoint == NULL) ? NULL : val_reference(v1);
}

static MUST_CHECK Obj *str(Obj *v1, linepos_t UNUSED(epoint), size_t UNUSED(maxsize)) {
    return val_reference(v1);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    return val_reference(op->v2->obj->type == T_ERROR ? op->v2 : op->v1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    return val_reference(op->v1->obj->type == T_ERROR ? op->v1 : op->v2);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t UNUSED(indx)) {
    return val_reference(op->v1);
}

static MUST_CHECK Error *ival(Obj *UNUSED(v1), ival_t *UNUSED(iv), unsigned int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *uval(Obj *UNUSED(v1), uval_t *UNUSED(uv), unsigned int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Obj *sign(Obj *v1, linepos_t UNUSED(epoint)) {
    return val_reference(v1);
}

static MUST_CHECK Obj *function(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK Obj *len(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK Obj *size(oper_t op) {
    return val_reference(op->v2);
}

void noneobj_init(void) {
    Type *type = new_type(&obj, T_NONE, "none", sizeof(None));
    type->same = same;
    type->truth = truth;
    type->repr = repr;
    type->str = str;
    type->hash = hash;
    type->calc1 = calc1;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    type->slice = slice;
    type->contains = rcalc2;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->iaddress = ival;
    type->uaddress = uval;
    type->sign = sign;
    type->function = function;
    type->len = len;
    type->size = size;
}

void noneobj_destroy(void) {
#ifdef DEBUG
    if (none_value->refcount != 1) fprintf(stderr, "none %" PRIuSIZE "\n", none_value->refcount - 1);
#endif
}
