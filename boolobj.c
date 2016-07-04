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
#include "boolobj.h"
#include <string.h>
#include "eval.h"
#include "error.h"
#include "variables.h"
#include "arguments.h"

#include "floatobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "intobj.h"
#include "operobj.h"
#include "typeobj.h"

static Type obj;

Type *BOOL_OBJ = &obj;
Bool *true_value;
Bool *false_value;
Bool *bool_value[2];

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BOOL: return val_reference(v1);
    default: return v1->obj->truth(v1, TRUTH_BOOL, epoint);
    }
}

static bool same(const Obj *o1, const Obj *o2) {
    const Bool *v1 = (const Bool *)o1, *v2 = (const Bool *)o2;
    return o2->obj == BOOL_OBJ && v1->boolean == v2->boolean;
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return val_reference(o1);
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bool *v1 = (Bool *)o1;
    *hs = v1->boolean ? 1 : 0;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Bool *v1 = (Bool *)o1;
    Str *v;
    size_t len = v1->boolean ? 4 : 5;
    if (len > maxsize) return NULL;
    v = new_str(len);
    v->chars =len;
    memcpy(v->data, v1->boolean ? "true" : "false", len);
    return &v->v;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int UNUSED(bits), linepos_t UNUSED(epoint)) {
    Bool *v1 = (Bool *)o1;
    *iv = v1->boolean ? 1 : 0;
    return NULL;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int UNUSED(bits), linepos_t UNUSED(epoint)) {
    Bool *v1 = (Bool *)o1;
    *uv = v1->boolean ? 1 : 0;
    return NULL;
}

MUST_CHECK Float *float_from_bool(const Bool *v1) {
    return new_float(v1->boolean ? 1.0 : 0.0);
}

MUST_CHECK Int *int_from_bool(const Bool *v1) {
    return ref_int(int_value[v1->boolean ? 1 : 0]);
}

static inline MUST_CHECK Obj *int_from_bool2(bool i) {
    return (Obj *)ref_int(int_value[i ? 1 : 0]);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bool *v1 = (Bool *)o1;
    return (Obj *)int_from_bool(v1);
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t UNUSED(epoint)) {
    Bool *v1 = (Bool *)o1;
    return (Obj *)int_from_bool(v1);
}

static MUST_CHECK Obj *calc1_strict(oper_t op) {
    if (op->op == &o_LNOT) {
        Bool *v1 = (Bool *)op->v1;
        return truth_reference(!v1->boolean);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bool *v1 = (Bool *)op->v1;
    Str *v;
    switch (op->op->op) {
    case O_BANK:
    case O_HIGHER: return (Obj *)bytes_from_u8(0);
    case O_LOWER: return (Obj *)bytes_from_u8(v1->boolean ? 1 : 0);
    case O_HWORD: return (Obj *)bytes_from_u16(0);
    case O_WORD: return (Obj *)bytes_from_u16(v1->boolean ? 1 : 0);
    case O_BSWORD: return (Obj *)bytes_from_u16(v1->boolean ? 0x100 : 0);
    case O_INV: return (Obj *)ibits_from_bool(v1->boolean);
    case O_NEG: return (Obj *)ref_int(v1->boolean ? minus1_value : int_value[0]);
    case O_POS: return (Obj *)int_from_bool(v1);
    case O_STRING:
        v = new_str(1);
        v->chars = 1;
        v->data[0] = v1->boolean ? '1' : '0';
        return &v->v;
    case O_LNOT: return truth_reference(!v1->boolean);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2_bool(oper_t op, bool v1, bool v2) {
    switch (op->op->op) {
    case O_SUB:
    case O_CMP: 
        if (!v1 && v2) return (Obj *)ref_int(minus1_value);
        return (Obj *)ref_int(int_value[(v1 != v2) ? 1 : 0]);
    case O_EQ: return truth_reference(v1 == v2);
    case O_NE: return truth_reference(v1 != v2);
    case O_LT: return truth_reference(!v1 && v2);
    case O_LE: return truth_reference(!v1 || v2);
    case O_GT: return truth_reference(v1 && !v2);
    case O_GE: return truth_reference(v1 || !v2);
    case O_ADD: return (Obj *)int_from_int((v1 ? 1 : 0) + (v2 ? 1 : 0));
    case O_MUL: return int_from_bool2(v1 && v2);
    case O_DIV:
        if (!v2) { 
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3); 
        }
        return int_from_bool2(v1);
    case O_MOD:
        if (!v2) { 
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        return int_from_bool2(false);
    case O_EXP: return int_from_bool2(v1 || !v2);
    case O_AND: return truth_reference(v1 && v2);
    case O_OR: return truth_reference(v1 || v2);
    case O_XOR: return truth_reference(v1 != v2);
    case O_LSHIFT: return v2 ? (Obj *)bits_from_bools(v1, false) : (Obj *)ref_bits(bits_value[v1 ? 1 : 0]);
    case O_RSHIFT: return v2 ? (Obj *)ref_bits(null_bits) : (Obj *)ref_bits(bits_value[v1 ? 1 : 0]);
    case O_CONCAT: return (Obj *)bits_from_bools(v1, v2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2_strict(oper_t op) {
    if (op->v2->obj == BOOL_OBJ) {
        Bool *v1 = (Bool *)op->v1;

        if (op->op == &o_LAND) {
            return val_reference(v1->boolean ? op->v2 : &v1->v);
        }
        if (op->op == &o_LOR) {
            return val_reference(v1->boolean ? &v1->v : op->v2);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bool *v1 = (Bool *)op->v1;

    if (op->op == &o_LAND) {
        return val_reference(v1->boolean ? op->v2 : &v1->v);
    }
    if (op->op == &o_LOR) {
        return val_reference(v1->boolean ? &v1->v : op->v2);
    }
    switch (op->v2->obj->type) {
    case T_BOOL: return calc2_bool(op, v1->boolean, ((Bool *)op->v2)->boolean);
    default: 
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    const Bool *v2 = (Bool *)op->v2;
    switch (op->v1->obj->type) {
    case T_BOOL: return calc2_bool(op, ((Bool *)op->v1)->boolean, v2->boolean);
    default:
        if (op->op != &o_IN) {
            return op->v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
}

void boolobj_init(void) {
    new_type(&obj, T_BOOL, "bool", sizeof(Bool));
    obj_init(&obj);
    obj.create = create;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;

    bool_value[0] = false_value = new_boolean(false);
    bool_value[1] = true_value = new_boolean(true);
}

void boolobj_init2(void) {
    if (arguments.strict) {
        obj.calc1 = calc1_strict;
        obj.calc2 = calc2_strict;
    } else {
        obj.ival = ival;
        obj.uval = uval;
        obj.sign = sign;
        obj.absolute = absolute;
        obj.calc1 = calc1;
        obj.calc2 = calc2;
        obj.rcalc2 = rcalc2;
    }
}

void boolobj_names(void) {
    new_builtin("bool", val_reference(&BOOL_OBJ->v));

    new_builtin("true", (Obj *)ref_bool(true_value));
    new_builtin("false", (Obj *)ref_bool(false_value));
}

void boolobj_destroy(void) {
#ifdef DEBUG
    if (false_value->v.refcount != 1) fprintf(stderr, "false %" PRIuSIZE "\n", false_value->v.refcount - 1);
    if (true_value->v.refcount != 1) fprintf(stderr, "true %" PRIuSIZE "\n", true_value->v.refcount - 1);
#endif

    val_destroy(&false_value->v);
    val_destroy(&true_value->v);
}
