/*
    $Id: obj.c 2999 2023-08-12 21:46:25Z soci $

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
#include "obj.h"
#include <string.h>
#include "eval.h"
#include "values.h"

#include "boolobj.h"
#include "floatobj.h"
#include "strobj.h"
#include "macroobj.h"
#include "intobj.h"
#include "listobj.h"
#include "namespaceobj.h"
#include "addressobj.h"
#include "codeobj.h"
#include "registerobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "functionobj.h"
#include "dictobj.h"
#include "operobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "labelobj.h"
#include "errorobj.h"
#include "mfuncobj.h"
#include "symbolobj.h"
#include "anonsymbolobj.h"
#include "memblocksobj.h"
#include "foldobj.h"
#include "encobj.h"

static Type lbl_obj;
static Type default_obj;
static Type funcargs_obj;
static Type alignblk_obj;

Type *const LBL_OBJ = &lbl_obj;
Type *const DEFAULT_OBJ = &default_obj;
Type *const FUNCARGS_OBJ = &funcargs_obj;
Type *const ALIGNBLK_OBJ = &alignblk_obj;

static Default defaultval = { { &default_obj, 1} };

Obj *const default_value = &defaultval.v;

MUST_CHECK Obj *obj_oper_error(oper_t op) {
    Obj *v1, *v2;
    Error *err;
    switch (op->op) {
    case O_EQ: return truth_reference(op->v1 == op->v2 || op->v1->obj->same(op->v1, op->v2));
    case O_NE: return truth_reference(op->v1 != op->v2 && !op->v1->obj->same(op->v1, op->v2));
    case O_WORD:
    case O_HWORD:
    case O_BSWORD:
    case O_LOWER:
    case O_HIGHER:
    case O_BANK:
    case O_STRING:
    case O_INV:
    case O_NEG:
    case O_POS:
    case O_LNOT:
    case O_X:
    case O_FUNC:
    case O_INDEX: v2 = NULL; break;
    default: v2 = op->v2; break;
    }
    v1 = op->v1;
    err = new_error(ERROR__INVALID_OPER, op->epoint3);
    err->u.invoper.op = op->op;
    err->u.invoper.v1 = (v1->refcount != 0) ? val_reference(v1) : v1;
    err->u.invoper.v2 = (v2 != NULL) ? ((v2->refcount != 0) ? val_reference(v2) : v2) : NULL;
    return Obj(err);
}

MUST_CHECK Obj *obj_oper_compare(oper_t op, int val) {
    bool result;
    switch (op->op) {
    case O_CMP: return val_reference(val < 0 ? minus1_value : int_value[(val > 0) ? 1 : 0]);
    case O_EQ: result = (val == 0); break;
    case O_NE: result = (val != 0); break;
    case O_MIN:
    case O_LT: result = (val < 0); break;
    case O_LE: result = (val <= 0); break;
    case O_MAX:
    case O_GT: result = (val > 0); break;
    case O_GE: result = (val >= 0); break;
    default: return obj_oper_error(op);
    }
    return truth_reference(result);
}

static MUST_CHECK Obj *invalid_convert(oper_t op) {
    Obj *v1 = op->v2;
    if (v1->obj == Type(op->v1)) return val_reference(v1);
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR: return val_reference(v1);
    case T_ADDRESS:
        if (Address(v1)->val == none_value || Address(v1)->val->obj == ERROR_OBJ) return val_reference(Address(v1)->val);
        break;
    default: break;
    }
    return new_error_conv(v1, Type(op->v1), op->epoint2);
}

static MUST_CHECK Obj *invalid_convert2(oper_t op) {
    return new_error_argnum(Funcargs(op->v2)->len, 1, 1, op->epoint2);
}

static FAST_CALL bool invalid_same(const Obj *o1, const Obj *o2) {
    return o1->obj == o2->obj;
}

static MUST_CHECK Obj *invalid_truth(Obj *v1, Truth_types UNUSED(type), linepos_t epoint) {
    return new_error_obj(ERROR_____CANT_BOOL, v1, epoint);
}

static MUST_CHECK Obj *invalid_hash(Obj *v1, int *UNUSED(hash), linepos_t epoint) {
    return new_error_obj(ERROR__NOT_HASHABLE, v1, epoint);
}

static MUST_CHECK Obj *invalid_repr(Obj *v1, linepos_t epoint, size_t maxsize) {
    Str *v;
    uint8_t *s;
    const char *name;
    size_t len, len2;
    if (epoint == NULL) return NULL;
    name = v1->obj->name;
    len2 = strlen(name);
    len = len2 + 2;
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;
    *s++ = '<';
    memcpy(s, name, len2);
    s[len2] = '>';
    return Obj(v);
}

static MUST_CHECK Obj *invalid_str(Obj *v1, linepos_t epoint, size_t maxsize) {
    return v1->obj->repr(v1, epoint, maxsize);
}

static MUST_CHECK Obj *invalid_calc1(oper_t op) {
    return obj_oper_error(op);
}

static MUST_CHECK Obj *invalid_calc2(oper_t op) {
    Obj *o2 = op->v2;
    if (o2 == none_value || o2->obj == ERROR_OBJ) {
        return val_reference(o2);
    }
    if (o2->obj == ADDRESS_OBJ) {
        Obj *val = Address(o2)->val;
        if (val == none_value || val->obj == ERROR_OBJ) return val_reference(val);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *invalid_rcalc2(oper_t op) {
    Obj *o1 = op->v1;
    if (o1 == none_value || o1->obj == ERROR_OBJ) {
        return val_reference(o1);
    }
    if (o1->obj == ADDRESS_OBJ) {
        Obj *val = Address(o1)->val;
        if (val == none_value || val->obj == ERROR_OBJ) return val_reference(val);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *invalid_slice(oper_t op, argcount_t indx) {
    Funcargs *args = Funcargs(op->v2);
    if (indx == 0) {
        if (args->len > 0) {
            Obj *o2 = args->val[0].val;
            if (o2 == none_value || o2->obj == ERROR_OBJ) {
                return val_reference(o2);
            }
            if (o2->obj == ADDRESS_OBJ) {
                Obj *val = Address(o2)->val;
                if (val == none_value || val->obj == ERROR_OBJ) return val_reference(val);
            }
        }
    } else {
        return new_error_argnum(args->len, 1, indx, op->epoint2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Error *invalid_ival(Obj *v1, ival_t *UNUSED(iv), unsigned int UNUSED(bits), linepos_t epoint) {
    return Error(new_error_obj(ERROR______CANT_INT, v1, epoint));
}

static MUST_CHECK Error *invalid_uval(Obj *v1, uval_t *UNUSED(uv), unsigned int UNUSED(bits), linepos_t epoint) {
    return Error(new_error_obj(ERROR______CANT_INT, v1, epoint));
}

static MUST_CHECK Error *invalid_uval2(Obj *v1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    return v1->obj->uval(v1, uv, bits, epoint);
}

static FAST_CALL uint32_t invalid_address(const Obj *UNUSED(v1)) {
    return A_NONE;
}

static MUST_CHECK Error *invalid_iaddress(Obj *v1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    return v1->obj->ival(v1, iv, bits, epoint);
}

static MUST_CHECK Error *invalid_uaddress(Obj *v1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    return v1->obj->uval(v1, uv, bits, epoint);
}

static MUST_CHECK Obj *invalid_sign(Obj *v1, linepos_t epoint) {
    return new_error_obj(ERROR_____CANT_SIGN, v1, epoint);
}

static MUST_CHECK Obj *invalid_function(oper_t op) {
    return new_error_obj((Function(op->v1)->func == F_ABS) ? ERROR______CANT_ABS : ERROR______CANT_INT, op->v2, op->epoint2);
}

static MUST_CHECK Obj *invalid_len(oper_t op) {
    return new_error_obj(ERROR______CANT_LEN, op->v2, op->epoint2);
}

static MUST_CHECK Obj *invalid_size(oper_t op) {
    return new_error_obj(ERROR_____CANT_SIZE, op->v2, op->epoint2);
}

static FAST_CALL MUST_CHECK Obj *invalid_next(struct iter_s *v1) {
    if (v1->val != 0) return NULL;
    v1->val = 1;
    return v1->data;
}

static void invalid_getiter(struct iter_s *v) {
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = invalid_next;
    v->len = 1;
}

static FAST_CALL bool lbl_same(const Obj *o1, const Obj *o2) {
    const Lbl *v1 = Lbl(o1), *v2 = Lbl(o2);
    return o1->obj == o2->obj && v1->sline == v2->sline && v1->waitforp == v2->waitforp && v1->file_list == v2->file_list && v1->parent == v2->parent;
}

static FAST_CALL bool default_same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static FAST_CALL bool funcargs_same(const Obj *o1, const Obj *o2) {
    const Funcargs *v1 = Funcargs(o1), *v2 = Funcargs(o2);
    return o1->obj == o2->obj && v1->val == v2->val && v1->len == v2->len;
}

static FAST_CALL bool alignblk_same(const Obj *o1, const Obj *o2) {
    const Alignblk *v1 = Alignblk(o1), *v2 = Alignblk(o2);
    return o1->obj == o2->obj && v1->size == v2->size;
}

void obj_init(Type *obj) {
    obj->iterable = false;
    obj->convert = invalid_convert;
    obj->convert2 = invalid_convert2;
    obj->destroy = NULL;
    obj->garbage = NULL;
    obj->same = invalid_same;
    obj->truth = invalid_truth;
    obj->hash = invalid_hash;
    obj->repr = invalid_repr;
    obj->str = invalid_str;
    obj->calc1 = invalid_calc1;
    obj->calc2 = invalid_calc2;
    obj->rcalc2 = invalid_rcalc2;
    obj->slice = invalid_slice;
    obj->contains = invalid_rcalc2;
    obj->ival = invalid_ival;
    obj->uval = invalid_uval;
    obj->uval2 = invalid_uval2;
    obj->address = invalid_address;
    obj->iaddress = invalid_iaddress;
    obj->uaddress = invalid_uaddress;
    obj->sign = invalid_sign;
    obj->function = invalid_function;
    obj->len = invalid_len;
    obj->size = invalid_size;
    obj->getiter = invalid_getiter;
    obj->getriter = invalid_getiter;
}

void objects_init(void) {
    Type *type;
    boolobj_init();
    floatobj_init();
    addressobj_init();
    codeobj_init();
    strobj_init();
    registerobj_init();
    listobj_init();
    bytesobj_init();
    bitsobj_init();
    intobj_init();
    functionobj_init();
    dictobj_init();
    labelobj_init();
    namespaceobj_init();
    memblocksobj_init();
    macroobj_init();
    errorobj_init();
    operobj_init();
    gapobj_init();
    typeobj_init();
    noneobj_init();
    mfuncobj_init();
    symbolobj_init();
    anonsymbolobj_init();
    foldobj_init();
    encobj_init();

    type = new_type(&lbl_obj, T_LBL, "lbl", sizeof(Lbl));
    type->same = lbl_same;

    type = new_type(&default_obj, T_DEFAULT, "default", sizeof(Default));
    type->same = default_same;

    type = new_type(&funcargs_obj, T_FUNCARGS, "funcargs", sizeof(Funcargs));
    type->same = funcargs_same;

    type = new_type(&alignblk_obj, T_ALIGNBLK, "alignblk", sizeof(Alignblk));
    type->same = alignblk_same;
}

void objects_destroy(void) {
    dictobj_destroy();
    listobj_destroy();
    bitsobj_destroy();
    bytesobj_destroy();
    strobj_destroy();
    boolobj_destroy();
    intobj_destroy();
    gapobj_destroy();
    noneobj_destroy();
    foldobj_destroy();
    functionobj_destroy();
    floatobj_destroy();
    encobj_destroy();

#ifdef DEBUG
    if (default_value->refcount != 1) fprintf(stderr, "default %" PRIuSIZE "\n", default_value->refcount - 1);
#endif
}
