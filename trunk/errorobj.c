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
#include "errorobj.h"
#include "eval.h"
#include "values.h"

#include "typeobj.h"
#include "registerobj.h"
#include "namespaceobj.h"

static Type obj;

Type *ERROR_OBJ = &obj;

static void destroy(Obj *o1) {
    Error *v1 = (Error *)o1;
    switch (v1->num) {
    case ERROR__INVALID_OPER:
        if (v1->u.invoper.v1 != NULL) val_destroy(v1->u.invoper.v1);
        if (v1->u.invoper.v2 != NULL) val_destroy(v1->u.invoper.v2);
        return;
    case ERROR___NO_REGISTER: 
        val_destroy(&v1->u.reg->v);
        return;
    case ERROR_____CANT_UVAL: 
    case ERROR_____CANT_IVAL: 
        val_destroy(v1->u.intconv.val);
        return;
    case ERROR___NOT_DEFINED: 
        val_destroy((Obj *)v1->u.notdef.names);
        return;
    case ERROR___MATH_DOMAIN:
    case ERROR_LOG_NON_POSIT:
    case ERROR_SQUARE_ROOT_N:
    case ERROR___INDEX_RANGE:
    case ERROR_____KEY_ERROR: 
        val_destroy(v1->u.key);
        return;
    default: return;
    }
}

static void garbage(Obj *o1, int i) {
    Error *v1 = (Error *)o1;
    Obj *v;
    switch (v1->num) {
    case ERROR__INVALID_OPER:
        v = v1->u.invoper.v1;
        if (v != NULL) {
            switch (i) {
            case -1:
                v->refcount--;
                break;
            case 0:
                break;
            case 1:
                if ((v->refcount & SIZE_MSB) != 0) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
                break;
            }
        }
        v = v1->u.invoper.v2;
        if (v == NULL) return;
        break;
    case ERROR___NO_REGISTER: 
        v = &v1->u.reg->v;
        break;
    case ERROR_____CANT_UVAL: 
    case ERROR_____CANT_IVAL: 
        v = v1->u.intconv.val;
        break;
    case ERROR___NOT_DEFINED: 
        v = &v1->u.notdef.names->v;
        break;
    case ERROR___MATH_DOMAIN:
    case ERROR_LOG_NON_POSIT:
    case ERROR_SQUARE_ROOT_N:
    case ERROR___INDEX_RANGE:
    case ERROR_____KEY_ERROR: 
        v = v1->u.key;
        break;
    default: return;
    }
    switch (i) {
    case -1:
        v->refcount--;
        return;
    case 0:
        return;
    case 1:
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

MUST_CHECK Error *new_error(Error_types num, linepos_t epoint) {
    Error *v = (Error *)val_alloc(ERROR_OBJ);
    v->num = num;
    v->epoint = *epoint;
    return v;
}

MUST_CHECK Error *new_error_key(Error_types num, Obj *v1, linepos_t epoint) {
    Error *v = (Error *)val_alloc(ERROR_OBJ);
    v->num = num;
    v->epoint = *epoint;
    v->u.key = val_reference(v1);
    return v;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK Obj *slice(Obj *v1, oper_t UNUSED(op), size_t UNUSED(indx)) {
    return val_reference(v1);
}

void errorobj_init(void) {
    new_type(&obj, T_ERROR, "error", sizeof(Error));
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.slice = slice;
}
