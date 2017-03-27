/*
    $Id$

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
#include "noneobj.h"
#include "eval.h"
#include "values.h"

#include "typeobj.h"
#include "errorobj.h"

static Type obj;

Type *NONE_OBJ = &obj;
None *none_value;

static MUST_CHECK Obj *truth(Obj *UNUSED(v1), Truth_types UNUSED(type), linepos_t epoint) {
    return (Obj *)new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *hash(Obj *UNUSED(v1), int *UNUSED(v), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Obj *repr(Obj *UNUSED(v1), linepos_t epoint, size_t UNUSED(maxsize)) {
    if (epoint == NULL) return NULL;
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *calc1(oper_t UNUSED(op)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return ERROR_OBJ->rcalc2(op);
    }
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc2(op);
    }
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *slice(Obj *UNUSED(v1), oper_t UNUSED(op), size_t UNUSED(indx)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Error *ival(Obj *UNUSED(v1), ival_t *UNUSED(iv), unsigned int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *uval(Obj *UNUSED(v1), uval_t *UNUSED(uv), unsigned int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *address(Obj *UNUSED(v1), uval_t *UNUSED(uv), int UNUSED(bits), uint32_t *UNUSED(am), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Obj *sign(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *function(Obj *UNUSED(v1), Func_types UNUSED(f), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *len(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *size(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

void noneobj_init(void) {
    new_type(&obj, T_NONE, "none", sizeof(None));
    obj.truth = truth;
    obj.repr = repr;
    obj.hash = hash;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.slice = slice;
    obj.ival = ival;
    obj.uval = uval;
    obj.address = address;
    obj.sign = sign;
    obj.function = function;
    obj.len = len;
    obj.size = size;

    none_value = (None *)val_alloc(NONE_OBJ);
}

void noneobj_destroy(void) {
#ifdef DEBUG
    if (none_value->v.refcount != 1) fprintf(stderr, "none %" PRIuSIZE "\n", none_value->v.refcount - 1);
#endif

    val_destroy(&none_value->v);
}
