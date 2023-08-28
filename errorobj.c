/*
    $Id: errorobj.c 3068 2023-08-28 06:18:09Z soci $

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
#include <string.h>
#include "errorobj.h"
#include "eval.h"
#include "values.h"
#include "error.h"
#include "64tass.h"
#include "file.h"
#include "macro.h"

#include "typeobj.h"
#include "registerobj.h"
#include "namespaceobj.h"

static Type obj;

Type *const ERROR_OBJ = &obj;

static FAST_CALL void destroy(Obj *o1) {
    Error *v1 = Error(o1);
    if (v1->line != NULL) free((uint8_t *)v1->line);
    switch (v1->num) {
    case ERROR__INVALID_OPER:
        val_destroy(v1->u.invoper.v1);
        if (v1->u.invoper.v2 != NULL) val_destroy(v1->u.invoper.v2);
        return;
    case ERROR___NO_REGISTER:
        val_destroy(Obj(v1->u.reg.reg));
        return;
    case ERROR____CANT_UVAL2:
    case ERROR____CANT_IVAL2:
    case ERROR_____CANT_UVAL:
    case ERROR_____CANT_IVAL:
        val_destroy(v1->u.intconv.val);
        return;
    case ERROR___NOT_DEFINED:
        val_destroy(v1->u.notdef.symbol);
        val_destroy(Obj(v1->u.notdef.names));
        return;
    case ERROR__NOT_KEYVALUE:
    case ERROR__NOT_HASHABLE:
    case ERROR_____CANT_SIGN:
    case ERROR______CANT_ABS:
    case ERROR______CANT_INT:
    case ERROR______CANT_LEN:
    case ERROR_____CANT_SIZE:
    case ERROR_____CANT_BOOL:
    case ERROR______NOT_ITER:
    case ERROR___MATH_DOMAIN:
    case ERROR_LOG_NON_POSIT:
    case ERROR_SQUARE_ROOT_N:
    case ERROR___INDEX_RANGE:
    case ERROR_____KEY_ERROR:
    case ERROR_DIVISION_BY_Z:
    case ERROR_ZERO_NEGPOWER:
    case ERROR__NOT_ONE_CHAR:
    case ERROR______NOT_UVAL:
        val_destroy(v1->u.obj);
        return;
    case ERROR__INVALID_CONV:
        val_destroy(v1->u.conv.val);
        return;
    default: return;
    }
}

static FAST_CALL void garbage(Obj *o1, int i) {
    Error *v1 = Error(o1);
    Obj *v;
    if (i == 0) {
        if (v1->line != NULL) free((uint8_t *)v1->line);
        return;
    }
    switch (v1->num) {
    case ERROR__INVALID_OPER:
        v = v1->u.invoper.v1;
        if (v1->u.invoper.v2 != NULL) {
            if (i < 0) v->refcount--;
            else if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
            v = v1->u.invoper.v2;
        }
        break;
    case ERROR___NO_REGISTER:
        v = Obj(v1->u.reg.reg);
        break;
    case ERROR____CANT_UVAL2:
    case ERROR____CANT_IVAL2:
    case ERROR_____CANT_UVAL:
    case ERROR_____CANT_IVAL:
        v = v1->u.intconv.val;
        break;
    case ERROR___NOT_DEFINED:
        v = v1->u.notdef.symbol;
        if (i < 0) v->refcount--;
        else if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        v = Obj(v1->u.notdef.names);
        break;
    case ERROR__NOT_KEYVALUE:
    case ERROR__NOT_HASHABLE:
    case ERROR_____CANT_SIGN:
    case ERROR______CANT_ABS:
    case ERROR______CANT_INT:
    case ERROR______CANT_LEN:
    case ERROR_____CANT_SIZE:
    case ERROR_____CANT_BOOL:
    case ERROR______NOT_ITER:
    case ERROR___MATH_DOMAIN:
    case ERROR_LOG_NON_POSIT:
    case ERROR_SQUARE_ROOT_N:
    case ERROR___INDEX_RANGE:
    case ERROR_____KEY_ERROR:
    case ERROR_DIVISION_BY_Z:
    case ERROR_ZERO_NEGPOWER:
    case ERROR__NOT_ONE_CHAR:
    case ERROR______NOT_UVAL:
        v = v1->u.obj;
        break;
    case ERROR__INVALID_CONV:
        v = v1->u.conv.val;
        break;
    default:
        return;
    }
    if (i < 0) v->refcount--;
    else if ((v->refcount & SIZE_MSB) != 0) {
        v->refcount -= SIZE_MSB - 1;
        v->obj->garbage(v, 1);
    } else v->refcount++;
}

MALLOC Error *new_error(Error_types num, linepos_t epoint) {
    Error *v = Error(val_alloc(ERROR_OBJ));
    v->num = num;
    v->file_list = current_file_list;
    v->epoint.line = epoint->line;
    v->caret = epoint->pos;
    v->epoint.pos = macro_error_translate2(epoint->pos);
    if (not_in_file(pline, current_file_list->file)) {
        size_t ln = strlen((const char *)pline) + 1;
        uint8_t *l = allocate_array(uint8_t, ln);
        if (l != NULL) memcpy(l, pline, ln);
        v->line = l;
    } else v->line = NULL;
    return v;
}

MALLOC Obj *new_error_mem(linepos_t epoint) {
    return Obj(new_error(ERROR_OUT_OF_MEMORY, epoint));
}

MALLOC Obj *new_error_obj(Error_types num, Obj *v1, linepos_t epoint) {
    Error *v = new_error(num, epoint);
    v->u.obj = val_reference(v1);
    return Obj(v);
}

MALLOC Obj *new_error_conv(Obj *v1, Type *t, linepos_t epoint) {
    Error *v = new_error(ERROR__INVALID_CONV, epoint);
    v->u.conv.t = t;
    v->u.conv.val = val_reference(v1);
    return Obj(v);
}

MALLOC Obj *new_error_argnum(argcount_t num, argcount_t min, argcount_t max, linepos_t epoint) {
    Error *v = new_error(ERROR__WRONG_ARGNUM, epoint);
    v->u.argnum.num = num;
    v->u.argnum.min = min;
    v->u.argnum.max = max;
    return Obj(v);
}

static MUST_CHECK Obj *truth(Obj *v1, Truth_types UNUSED(type), linepos_t UNUSED(epoint)) {
    return val_reference(v1);
}

static MUST_CHECK Obj *hash(Obj *v1, int *UNUSED(hash), linepos_t UNUSED(epoint)) {
    return val_reference(v1);
}

static MUST_CHECK Obj *repr(Obj *v1, linepos_t epoint, size_t UNUSED(maxsize)) {
    return (epoint == NULL) ? NULL : val_reference(v1);
}

static MUST_CHECK Obj *str(Obj *v1, linepos_t UNUSED(epoint), size_t UNUSED(maxsize)) {
    return val_reference(v1);
}

static MUST_CHECK Error *ival(Obj *v1, ival_t *UNUSED(iv), unsigned int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return Error(val_reference(v1));
}

static MUST_CHECK Error *uval(Obj *v1, uval_t *UNUSED(uv), unsigned int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return Error(val_reference(v1));
}

static MUST_CHECK Obj *sign(Obj *v1, linepos_t UNUSED(epoint)) {
    return val_reference(v1);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t UNUSED(indx)) {
    return val_reference(op->v1);
}

void errorobj_init(void) {
    Type *type = new_type(&obj, T_ERROR, "error", sizeof(Error));
    type->destroy = destroy;
    type->garbage = garbage;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
    type->calc1 = calc1;
    type->calc2 = calc1;
    type->rcalc2 = rcalc2;
    type->slice = slice;
    type->contains = rcalc2;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->iaddress = ival;
    type->uaddress = uval;
    type->sign = sign;
    type->function = rcalc2;
    type->len = rcalc2;
    type->size = rcalc2;
}

void error_obj_update(Error *err, const Obj *v1, Obj *v2) {
    switch (err->num) {
    case ERROR__INVALID_OPER:
        if (err->u.invoper.v1 == v1) {
            val_replace(&err->u.invoper.v1, v2);
        }
        if (err->u.invoper.v2 == v1) {
            val_replace(&err->u.invoper.v2, v2);
        }
        return;
    case ERROR____CANT_UVAL2:
    case ERROR____CANT_IVAL2:
    case ERROR_____CANT_UVAL:
    case ERROR_____CANT_IVAL:
        if (err->u.intconv.val == v1) {
            val_replace(&err->u.intconv.val, v2);
        }
        return;
    case ERROR__NOT_KEYVALUE:
    case ERROR__NOT_HASHABLE:
    case ERROR_____CANT_SIGN:
    case ERROR______CANT_ABS:
    case ERROR______CANT_INT:
    case ERROR______CANT_LEN:
    case ERROR_____CANT_SIZE:
    case ERROR_____CANT_BOOL:
    case ERROR______NOT_ITER:
    case ERROR___MATH_DOMAIN:
    case ERROR_LOG_NON_POSIT:
    case ERROR_SQUARE_ROOT_N:
    case ERROR___INDEX_RANGE:
    case ERROR_____KEY_ERROR:
    case ERROR_DIVISION_BY_Z:
    case ERROR_ZERO_NEGPOWER:
    case ERROR__NOT_ONE_CHAR:
    case ERROR______NOT_UVAL:
        if (err->u.obj == v1) {
            val_replace(&err->u.obj, v2);
        }
        return;
    default:
        return;
    }
}
