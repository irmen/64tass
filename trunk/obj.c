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
#include <string.h>
#include "variables.h"
#include "misc.h"
#include "section.h"
#include "64tass.h"
#include "eval.h"
#include "error.h"
#include "values.h"

#include "boolobj.h"
#include "floatobj.h"
#include "strobj.h"
#include "macro.h"
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

int referenceit = 1;

static struct obj_s lbl_obj;
static struct obj_s mfunc_obj;
static struct obj_s struct_obj;
static struct obj_s union_obj;
static struct obj_s none_obj;
static struct obj_s ident_obj;
static struct obj_s anonident_obj;
static struct obj_s default_obj;
static struct obj_s iter_obj;
static struct obj_s funcargs_obj;
static struct obj_s type_obj;

obj_t LBL_OBJ = &lbl_obj;
obj_t MFUNC_OBJ = &mfunc_obj;
obj_t STRUCT_OBJ = &struct_obj;
obj_t UNION_OBJ = &union_obj;
obj_t NONE_OBJ = &none_obj;
obj_t IDENT_OBJ = &ident_obj;
obj_t ANONIDENT_OBJ = &anonident_obj;
obj_t DEFAULT_OBJ = &default_obj;
obj_t ITER_OBJ = &iter_obj;
obj_t FUNCARGS_OBJ = &funcargs_obj;
obj_t TYPE_OBJ = &type_obj;
None *none_value;
Default *default_value;

MUST_CHECK Obj *obj_oper_error(oper_t op) {
    Obj *v1, *v2;
    Error *err;
    switch (op->op->op) {
    case O_EQ: return (Obj *)ref_bool(false_value);
    case O_NE: return (Obj *)ref_bool(true_value);
    case O_FUNC:
    case O_INDEX: v2 = NULL; break;
    default: v2 = op->v2; break;
    }
    v1 = op->v1;
    err = new_error(ERROR__INVALID_OPER, op->epoint3);
    err->u.invoper.op = op->op;
    err->u.invoper.v1 = v1 ? (v1->refcount ? val_reference(v1) : v1) : NULL;
    err->u.invoper.v2 = v2 ? (v2->refcount ? val_reference(v2) : v2) : NULL;
    return &err->v;
}

static void invalid_destroy(Obj *UNUSED(v1)) {
    return;
}

static MUST_CHECK Obj *invalid_create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static int invalid_same(const Obj *v1, const Obj *v2) {
    return v1->obj == v2->obj;
}

static MUST_CHECK Error *generic_invalid(Obj *v1, linepos_t epoint, enum errors_e num) {
    Error *err;
    if (v1->obj == ERROR_OBJ) {
        return (Error *)val_reference(v1);
    }
    err = new_error(num, epoint);
    err->u.objname = v1->obj->name;
    return err;
}

static MUST_CHECK Obj *invalid_truth(Obj *v1, enum truth_e UNUSED(type), linepos_t epoint) {
    return (Obj *)generic_invalid(v1, epoint, ERROR_____CANT_BOOL);
}

static MUST_CHECK Error *invalid_hash(Obj *v1, int *UNUSED(hash), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR__NOT_HASHABLE);
}

static MUST_CHECK Obj *invalid_repr(Obj *v1, linepos_t epoint) {
    Str *v;
    uint8_t *s;
    const char *name;
    if (!epoint) return NULL;
    if (v1->obj == ERROR_OBJ) {
        return val_reference(v1);
    }
    name = v1->obj->name;
    v = new_str();
    v->len = strlen(name) + 2;
    v->chars = v->len;
    s = str_create_elements(v, v->len);
    *s = '<';
    memcpy(s + 1, name, v->len);
    s[v->len - 1] = '>';
    v->data = s;
    return &v->v;
}

static MUST_CHECK Obj *invalid_calc1(oper_t op) {
    return obj_oper_error(op);
}

static MUST_CHECK Obj *invalid_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return ERROR_OBJ->rcalc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *invalid_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Error *invalid_ival(Obj *v1, ival_t *UNUSED(iv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK Error *invalid_uval(Obj *v1, uval_t *UNUSED(uv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK Error *invalid_address(Obj *v1, uval_t *uv, int bits, uint32_t *UNUSED(am), linepos_t epoint) {
    return uv ? v1->obj->uval(v1, uv, bits, epoint) : NULL;
}

static MUST_CHECK Obj *invalid_sign(Obj *v1, linepos_t epoint) {
    return (Obj *)generic_invalid(v1, epoint, ERROR_____CANT_SIGN);
}

static MUST_CHECK Obj *invalid_abs(Obj *v1, linepos_t epoint) {
    return (Obj *)generic_invalid(v1, epoint, ERROR______CANT_ABS);
}

static MUST_CHECK Obj *invalid_len(Obj *v1, linepos_t epoint) {
    return (Obj *)generic_invalid(v1, epoint, ERROR______CANT_LEN);
}

static MUST_CHECK Obj *invalid_size(Obj *v1, linepos_t epoint) {
    return (Obj *)generic_invalid(v1, epoint, ERROR_____CANT_SIZE);
}

MUST_CHECK Iter *invalid_getiter(Obj *v1) {
    Iter *v = (Iter *)val_alloc(ITER_OBJ);
    v->data = val_reference(v1);
    v->iter = NULL;
    v->val = 1;
    return v;
}

static MUST_CHECK Obj *invalid_next(Iter *v1) {
    if (v1->val) {
        v1->val = 0;
        return val_reference(v1->data);
    }
    return NULL;
}

static void iter_destroy(Obj *o1) {
    Iter *v1 = (Iter *)o1;
    if (v1->iter != &v1->val) free(v1->iter);
    val_destroy(v1->data);
}

static void iter_garbage(Obj *o1, int i) {
    Iter *v1 = (Iter *)o1;
    Obj *v;
    switch (i) {
    case -1:
        v1->data->refcount--;
        return;
    case 0:
        if (v1->iter != &v1->val) free(v1->iter);
        return;
    case 1:
        v = v1->data;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static MUST_CHECK Obj *iter_next(Iter *v1) {
    if (!v1->iter) return invalid_next(v1);
    return v1->data->obj->next(v1);
}

static void mfunc_destroy(Obj *o1) {
    Mfunc *v1 = (Mfunc *)o1;
    size_t i = v1->argc;
    while (i--) {
        free((char *)v1->param[i].name.data);
        if (v1->param[i].name.data != v1->param[i].cfname.data) free((char *)v1->param[i].cfname.data);
        if (v1->param[i].init) val_destroy(v1->param[i].init);
    }
    free(v1->param);
}

static void mfunc_garbage(Obj *o1, int j) {
    Mfunc *v1 = (Mfunc *)o1;
    size_t i = v1->argc;
    Obj *v ;
    switch (j) {
    case -1:
        while (i--) {
            v = v1->param[i].init;
            if (v) v->refcount--;
        }
        return;
    case 0:
        while (i--) {
            free((char *)v1->param[i].name.data);
            if (v1->param[i].name.data != v1->param[i].cfname.data) free((char *)v1->param[i].cfname.data);
        }
        free(v1->param);
        return;
    case 1:
        while (i--) {
            v = v1->param[i].init;
            if (v) {
                if (v->refcount & SIZE_MSB) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
            }
        }
        return;
    }
}

static int mfunc_same(const Obj *o1, const Obj *o2) {
    const Mfunc *v1 = (const Mfunc *)o1, *v2 = (const Mfunc *)o2;
    size_t i;
    if (o2->obj != MFUNC_OBJ || v1->file_list != v2->file_list || v1->line != v2->line) return 0;
    for (i = 0; i < v1->argc; i++) {
        if (str_cmp(&v1->param[i].name, &v2->param[i].name)) return 0;
        if ((v1->param[i].name.data != v1->param[i].cfname.data || v2->param[i].name.data != v2->param[i].cfname.data) && str_cmp(&v1->param[i].cfname, &v2->param[i].cfname)) return 0;
        if (v1->param[i].init != v2->param[i].init && (!v1->param[i].init || !v2->param[i].init || !obj_same(v1->param[i].init, v2->param[i].init))) return 0;
        if (v1->param[i].epoint.pos != v2->param[i].epoint.pos) return 0;
    }
    return 1;
}

static MUST_CHECK Obj *mfunc_calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_FUNCARGS: 
        switch (op->op->op) {
        case O_FUNC:
        {
            Mfunc *v1 = (Mfunc *)op->v1;
            Funcargs *v2 = (Funcargs *)op->v2;
            Obj *val;
            size_t i, max = 0, args = v2->len;
            for (i = 0; i < args; i++) {
                if (v2->val[i].val->obj == NONE_OBJ || v2->val[i].val->obj == ERROR_OBJ) {
                    return val_reference(v2->val[i].val);
                }
            }
            for (; i < v1->argc; i++) {
                if (!v1->param[i].init) {
                    max = i + 1;
                }
            }
            if (max) err_msg_argnum(args, max, v1->argc, op->epoint2);
            eval_enter();
            val = mfunc2_recurse(v1, v2->val, args, op->epoint);
            eval_leave();
            return val ? val : (Obj *)ref_tuple(null_tuple);
        }
        default: break;
        }
        break;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *ident_calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *ident_rcalc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *none_truth(Obj *UNUSED(v1), enum truth_e UNUSED(type), linepos_t epoint) {
    return (Obj *)new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *none_hash(Obj *UNUSED(v1), int *UNUSED(v), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Obj *none_repr(Obj *UNUSED(v1), linepos_t epoint) {
    if (!epoint) return NULL;
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_calc1(oper_t UNUSED(op)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return ERROR_OBJ->rcalc2(op);
    }
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc2(op);
    }
    return (Obj *)ref_none();
}

static MUST_CHECK Error *none_ival(Obj *UNUSED(v1), ival_t *UNUSED(iv), int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *none_uval(Obj *UNUSED(v1), uval_t *UNUSED(uv), int UNUSED(bits), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Error *none_address(Obj *UNUSED(v1), uval_t *UNUSED(uv), int UNUSED(bits), uint32_t *UNUSED(am), linepos_t epoint) {
    return new_error(ERROR____STILL_NONE, epoint);
}

static MUST_CHECK Obj *none_sign(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_abs(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_len(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *none_size(Obj *UNUSED(v1), linepos_t UNUSED(epoint)) {
    return (Obj *)ref_none();
}

static int lbl_same(const Obj *o1, const Obj *o2) {
    const Lbl *v1 = (const Lbl *)o1, *v2 = (const Lbl *)o2;
    return o2->obj == LBL_OBJ && v1->sline == v2->sline && v1->waitforp == v2->waitforp && v1->file_list == v2->file_list && v1->parent == v2->parent;
}

static int funcargs_same(const Obj *o1, const Obj *o2) {
    Funcargs *v1 = (Funcargs *)o1, *v2 = (Funcargs *)o2;
    return o2->obj == FUNCARGS_OBJ && v1->val == v2->val && v1->len == v2->len;
}

static MUST_CHECK Obj *type_create(Obj *v1, linepos_t UNUSED(epoint)) {
    Type *v;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TYPE: return val_reference(v1);
    default: break;
    }
    v = (Type *)val_alloc(TYPE_OBJ);
    v->type = v1->obj;
    return (Obj *)v;
}

static int type_same(const Obj *o1, const Obj *o2) {
    const Type *v1 = (const Type *)o1, *v2 = (const Type *)o2;
    return o2->obj == TYPE_OBJ && v1->type == v2->type;
}

static MUST_CHECK Error *type_hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Type *v1 = (Type *)o1;
    *hs = v1->type->type;
    return NULL;
}

static MUST_CHECK Obj *type_repr(Obj *o1, linepos_t epoint) {
    Type *v1 = (Type *)o1;
    Str *v;
    uint8_t *s;
    const char *name;
    size_t ln;
    if (!epoint) return NULL;
    name = v1->type->name;
    ln = strlen(name);
    v = new_str();
    v->len = ln + 9;
    v->chars = v->len;
    s = str_create_elements(v, v->len);
    memcpy(s, "<type '", 7);
    memcpy(s + 7, name, ln);
    memcpy(s + 7 + ln , "'>", 2);
    v->data = s;
    return (Obj *)v;
}

static inline int tcmp(const Type *vv1, const Type *vv2) {
    enum type_e v1 = vv1->type->type;
    enum type_e v2 = vv2->type->type;
    if (v1 < v2) return -1;
    return v1 > v2;
}

static MUST_CHECK Obj *type_calc2(oper_t op) {
    Type *v1 = (Type *)op->v1;
    Obj *o2 = op->v2;
    size_t args;

    switch (o2->obj->type) {
    case T_TYPE:
        {
            Type *v2 = (Type *)o2;
            int val;
            switch (op->op->op) {
            case O_CMP:
                val = tcmp(v1, v2);
                if (val < 0) return (Obj *)ref_int(minus1_value);
                return (Obj *)ref_int(int_value[val > 0]);
            case O_EQ: return truth_reference(tcmp(v1, v2) == 0);
            case O_NE: return truth_reference(tcmp(v1, v2) != 0);
            case O_LT: return truth_reference(tcmp(v1, v2) < 0);
            case O_LE: return truth_reference(tcmp(v1, v2) <= 0);
            case O_GT: return truth_reference(tcmp(v1, v2) > 0);
            case O_GE: return truth_reference(tcmp(v1, v2) >= 0);
            default: break;
            }
        }
    case T_FUNCARGS:
        if (op->op == &o_FUNC) {
            args = ((Funcargs *)o2)->len;
            if (args != 1) {
                err_msg_argnum(args, 1, 1, op->epoint2);
                return (Obj *)ref_none();
            }
            o2 = ((Funcargs *)o2)->val[0].val;
            switch (o2->obj->type) {
            case T_LIST:
            case T_TUPLE: 
                if (v1->type != LIST_OBJ && v1->type != TUPLE_OBJ && v1->type != TYPE_OBJ) {
                    List *v2 = (List *)o2;
                    Obj **vals;
                    size_t i;
                    int error;
                    List *v = (List *)val_alloc(o2->obj);
                    v->data = vals = list_create_elements(v, v2->len);
                    error = 1;
                    for (i = 0;i < v2->len; i++) {
                        Obj *val = v1->type->create(v2->data[i], op->epoint2);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                        vals[i] = val;
                    }
                    v->len = i;
                    return (Obj *)v;
                }
                /* fall through */
            default: return v1->type->create(o2, op->epoint2);
            }
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return o2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static void struct_destroy(Obj *o1) {
    Struct *v1 = (Struct *)o1;
    while (v1->argc) {
        --v1->argc;
        free((char *)v1->param[v1->argc].cfname.data);
        free((char *)v1->param[v1->argc].init.data);
    }
    free(v1->param);
    val_destroy((Obj *)v1->names);
}

static void struct_garbage(Obj *o1, int i) {
    Struct *v1 = (Struct *)o1;
    Obj *v;
    switch (i) {
    case -1:
        ((Obj *)v1->names)->refcount--;
        return;
    case 0:
        while (v1->argc) {
            --v1->argc;
            free((char *)v1->param[v1->argc].cfname.data);
            free((char *)v1->param[v1->argc].init.data);
        }
        free(v1->param);
        return;
    case 1:
        v = (Obj *)v1->names;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static int struct_same(const Obj *o1, const Obj *o2) {
    const Struct *v1 = (const Struct *)o1, *v2 = (const Struct *)o2;
    size_t i;
    if (o1->obj != o2->obj || v1->size != v2->size || v1->file_list != v2->file_list || v1->line != v2->line || v1->argc != v2->argc) return 0;
    if (v1->names != v2->names && !obj_same(&v1->names->v, &v2->names->v)) return 0;
    for (i = 0; i < v1->argc; i++) {
        if (str_cmp(&v1->param[i].cfname, &v2->param[i].cfname)) return 0;
        if (str_cmp(&v1->param[i].init, &v2->param[i].init)) return 0;
    }
    return 1;
}

static MUST_CHECK Obj *struct_size(Obj *o1, linepos_t UNUSED(epoint)) {
    Struct *v1 = (Struct *)o1;
    return (Obj *)int_from_size(v1->size);
}

static MUST_CHECK Obj *struct_calc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return namespace_member(op, ((Struct *)op->v1)->names);
    }
    return obj_oper_error(op);
}

void obj_init(struct obj_s *obj, enum type_e type, const char *name, size_t length) {
    obj->type = type;
    obj->length = length;
    obj->name = name;
    obj->create = invalid_create;
    obj->destroy = invalid_destroy;
    obj->garbage = NULL;
    obj->same = invalid_same;
    obj->truth = invalid_truth;
    obj->hash = invalid_hash;
    obj->repr = invalid_repr;
    obj->calc1 = invalid_calc1;
    obj->calc2 = invalid_calc2;
    obj->rcalc2 = invalid_rcalc2;
    obj->ival = invalid_ival;
    obj->uval = invalid_uval;
    obj->address = invalid_address;
    obj->sign = invalid_sign;
    obj->abs = invalid_abs;
    obj->len = invalid_len;
    obj->size = invalid_size;
    obj->getiter = invalid_getiter;
    obj->next = invalid_next;
}

void objects_init(void) {
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
    macroobj_init();
    errorobj_init();
    operobj_init();
    gapobj_init();

    obj_init(&lbl_obj, T_LBL, "lbl", sizeof(Lbl));
    lbl_obj.same = lbl_same;
    obj_init(&mfunc_obj, T_MFUNC, "function", sizeof(Mfunc));
    mfunc_obj.destroy = mfunc_destroy;
    mfunc_obj.garbage = mfunc_garbage;
    mfunc_obj.same = mfunc_same;
    mfunc_obj.calc2 = mfunc_calc2;
    obj_init(&struct_obj, T_STRUCT, "struct", sizeof(Struct));
    struct_obj.destroy = struct_destroy;
    struct_obj.garbage = struct_garbage;
    struct_obj.same = struct_same;
    struct_obj.size = struct_size;
    struct_obj.calc2 = struct_calc2;
    obj_init(&union_obj, T_UNION, "union", sizeof(Union));
    union_obj.destroy = struct_destroy;
    union_obj.garbage = struct_garbage;
    union_obj.same = struct_same;
    union_obj.size = struct_size;
    union_obj.calc2 = struct_calc2;
    obj_init(&none_obj, T_NONE, "none", sizeof(None));
    none_obj.truth = none_truth;
    none_obj.repr = none_repr;
    none_obj.hash = none_hash;
    none_obj.calc1 = none_calc1;
    none_obj.calc2 = none_calc2;
    none_obj.rcalc2 = none_rcalc2;
    none_obj.ival = none_ival;
    none_obj.uval = none_uval;
    none_obj.address = none_address;
    none_obj.sign = none_sign;
    none_obj.abs = none_abs;
    none_obj.len = none_len;
    none_obj.size = none_size;
    obj_init(&ident_obj, T_IDENT, "ident", sizeof(Ident));
    ident_obj.calc2 = ident_calc2;
    ident_obj.rcalc2 = ident_rcalc2;
    obj_init(&anonident_obj, T_ANONIDENT, "anonident", sizeof(Anonident));
    anonident_obj.calc2 = ident_calc2;
    anonident_obj.rcalc2 = ident_rcalc2;
    obj_init(&default_obj, T_DEFAULT, "default", sizeof(Default));
    obj_init(&iter_obj, T_ITER, "iter", sizeof(Iter));
    iter_obj.destroy = iter_destroy;
    iter_obj.garbage = iter_garbage;
    iter_obj.next = iter_next;
    obj_init(&funcargs_obj, T_FUNCARGS, "funcargs", sizeof(Funcargs));
    funcargs_obj.same = funcargs_same;
    obj_init(&type_obj, T_TYPE, "type", sizeof(Type));
    type_obj.create = type_create;
    type_obj.same = type_same;
    type_obj.hash = type_hash;
    type_obj.repr = type_repr;
    type_obj.calc2 = type_calc2;

    none_value = (None *)val_alloc(NONE_OBJ);
    default_value = (Default *)val_alloc(DEFAULT_OBJ);
}

void typeobj_names(void) {
    Type *v = (Type *)val_alloc(TYPE_OBJ); 
    v->type = TYPE_OBJ; 
    new_builtin("type", &v->v);
}

void objects_destroy(void) {
    listobj_destroy();
    bitsobj_destroy();
    bytesobj_destroy();
    strobj_destroy();
    boolobj_destroy();
    intobj_destroy();
    gapobj_destroy();

#ifdef DEBUG
    if (none_value->v.refcount != 1) fprintf(stderr, "none %d\n", none_value->v.refcount - 1);
    if (default_value->v.refcount != 1) fprintf(stderr, "default %d\n", default_value->v.refcount - 1);
#endif

    val_destroy(&none_value->v);
    val_destroy(&default_value->v);
}
