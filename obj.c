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
#include "obj.h"
#include <string.h>
#include "misc.h"
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
#include "typeobj.h"
#include "noneobj.h"
#include "labelobj.h"

bool referenceit = true;

static Type lbl_obj;
static Type mfunc_obj;
static Type struct_obj;
static Type union_obj;
static Type ident_obj;
static Type anonident_obj;
static Type default_obj;
static Type iter_obj;
static Type funcargs_obj;

Type *LBL_OBJ = &lbl_obj;
Type *MFUNC_OBJ = &mfunc_obj;
Type *STRUCT_OBJ = &struct_obj;
Type *UNION_OBJ = &union_obj;
Type *IDENT_OBJ = &ident_obj;
Type *ANONIDENT_OBJ = &anonident_obj;
Type *DEFAULT_OBJ = &default_obj;
Type *ITER_OBJ = &iter_obj;
Type *FUNCARGS_OBJ = &funcargs_obj;
Default *default_value;

MUST_CHECK Obj *obj_oper_error(oper_t op) {
    Obj *v1, *v2;
    Error *err;
    switch (op->op->op) {
    case O_EQ: return truth_reference(op->v1 == op->v2 || op->v1->obj->same(op->v1, op->v2));
    case O_NE: return truth_reference(op->v1 != op->v2 && !op->v1->obj->same(op->v1, op->v2));
    case O_FUNC:
    case O_INDEX: v2 = NULL; break;
    default: v2 = op->v2; break;
    }
    v1 = op->v1;
    err = new_error(ERROR__INVALID_OPER, op->epoint3);
    err->u.invoper.op = op->op;
    err->u.invoper.v1 = (v1 != NULL) ? ((v1->refcount != 0) ? val_reference(v1) : v1) : NULL;
    err->u.invoper.v2 = (v2 != NULL) ? ((v2->refcount != 0) ? val_reference(v2) : v2) : NULL;
    return &err->v;
}

static void invalid_destroy(Obj *UNUSED(v1)) {
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

static bool invalid_same(const Obj *v1, const Obj *v2) {
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

static MUST_CHECK Obj *invalid_repr(Obj *v1, linepos_t epoint, size_t maxsize) {
    Str *v;
    uint8_t *s;
    const char *name;
    size_t len, len2;
    if (epoint == NULL) return NULL;
    if (v1->obj == ERROR_OBJ) {
        return val_reference(v1);
    }
    name = v1->obj->name;
    len2 = strlen(name);
    len = len2 + 2;
    if (len > maxsize) return NULL;
    v = new_str(len);
    v->chars = len;
    s = v->data;
    *s++ = '<';
    memcpy(s, name, len2);
    s[len2] = '>';
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

static MUST_CHECK Obj *invalid_slice(Obj *UNUSED(v1), oper_t op, size_t indx) {
    if (op->v2->obj == ERROR_OBJ) {
        return val_reference(op->v2);
    }
    if (indx != 0) {
        Obj *o2 = op->v2;
        Funcargs *args = (Funcargs *)o2;
        err_msg_argnum(args->len, 1, indx, op->epoint2);
        return (Obj *)ref_none();
    }
    return obj_oper_error(op);
}

static MUST_CHECK Error *invalid_ival(Obj *v1, ival_t *UNUSED(iv), unsigned int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK Error *invalid_uval(Obj *v1, uval_t *UNUSED(uv), unsigned int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK Error *invalid_address(Obj *v1, uval_t *uv, int bits, uint32_t *UNUSED(am), linepos_t epoint) {
    if (uv == NULL) return NULL;
    return (bits >= 0) ? v1->obj->uval(v1, uv, bits, epoint) : v1->obj->ival(v1, (ival_t *)uv, -bits, epoint);
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
    if (v1->val == 0) return NULL;
    v1->val = 0;
    return val_reference(v1->data);
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
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static MUST_CHECK Obj *iter_next(Iter *v1) {
    if (v1->iter == NULL) return invalid_next(v1);
    return v1->data->obj->next(v1);
}

static void mfunc_destroy(Obj *o1) {
    Mfunc *v1 = (Mfunc *)o1;
    size_t i = v1->argc;
    while ((i--) != 0) {
        free((char *)v1->param[i].name.data);
        if (v1->param[i].name.data != v1->param[i].cfname.data) free((char *)v1->param[i].cfname.data);
        if (v1->param[i].init != NULL) val_destroy(v1->param[i].init);
    }
    i = v1->nslen;
    while ((i--) != 0) {
        val_destroy(&v1->namespaces[i]->v);
    }
    free(v1->namespaces);
    free(v1->param);
}

static void mfunc_garbage(Obj *o1, int j) {
    Mfunc *v1 = (Mfunc *)o1;
    size_t i = v1->argc;
    Obj *v ;
    switch (j) {
    case -1:
        while ((i--) != 0) {
            v = v1->param[i].init;
            if (v != NULL) v->refcount--;
        }
        i = v1->nslen;
        while ((i--) != 0) {
            v = &v1->namespaces[i]->v;
            v->refcount--;
        }
        return;
    case 0:
        while ((i--) != 0) {
            free((char *)v1->param[i].name.data);
            if (v1->param[i].name.data != v1->param[i].cfname.data) free((char *)v1->param[i].cfname.data);
        }
        free(v1->param);
        free(v1->namespaces);
        return;
    case 1:
        while ((i--) != 0) {
            v = v1->param[i].init;
            if (v != NULL) {
                if ((v->refcount & SIZE_MSB) != 0) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
            }
        }
        i = v1->nslen;
        while ((i--) != 0) {
            v = &v1->namespaces[i]->v;
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        return;
    }
}

static bool mfunc_same(const Obj *o1, const Obj *o2) {
    const Mfunc *v1 = (const Mfunc *)o1, *v2 = (const Mfunc *)o2;
    size_t i;
    if (o2->obj != MFUNC_OBJ || v1->file_list != v2->file_list || v1->line != v2->line || v1->argc != v2->argc || v1->nslen != v2->nslen) return false;
    for (i = 0; i < v1->argc; i++) {
        if (str_cmp(&v1->param[i].name, &v2->param[i].name) != 0) return false;
        if ((v1->param[i].name.data != v1->param[i].cfname.data || v2->param[i].name.data != v2->param[i].cfname.data) && str_cmp(&v1->param[i].cfname, &v2->param[i].cfname) != 0) return false;
        if (v1->param[i].init != v2->param[i].init && (v1->param[i].init == NULL || v2->param[i].init == NULL || !v1->param[i].init->obj->same(v1->param[i].init, v2->param[i].init))) return false;
        if (v1->param[i].epoint.pos != v2->param[i].epoint.pos) return false;
    }
    for (i = 0; i < v1->nslen; i++) {
        if (v1->namespaces[i] != v2->namespaces[i] && !v1->namespaces[i]->v.obj->same(&v1->namespaces[i]->v, &v2->namespaces[i]->v)) return false;
    }
    return true;
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
                if (v1->param[i].init == NULL) {
                    max = i + 1;
                }
            }
            if (max != 0) err_msg_argnum(args, max, v1->argc, op->epoint2);
            eval_enter();
            val = mfunc2_recurse(v1, v2->val, args, op->epoint);
            eval_leave();
            return (val != NULL) ? val : (Obj *)ref_tuple(null_tuple);
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
        if (op->op != &o_MEMBER && op->op != &o_X) {
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

static bool lbl_same(const Obj *o1, const Obj *o2) {
    const Lbl *v1 = (const Lbl *)o1, *v2 = (const Lbl *)o2;
    return o2->obj == LBL_OBJ && v1->sline == v2->sline && v1->waitforp == v2->waitforp && v1->file_list == v2->file_list && v1->parent == v2->parent;
}

static bool funcargs_same(const Obj *o1, const Obj *o2) {
    Funcargs *v1 = (Funcargs *)o1, *v2 = (Funcargs *)o2;
    return o2->obj == FUNCARGS_OBJ && v1->val == v2->val && v1->len == v2->len;
}

static void struct_destroy(Obj *o1) {
    Struct *v1 = (Struct *)o1;
    while (v1->argc != 0) {
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
        while (v1->argc != 0) {
            --v1->argc;
            free((char *)v1->param[v1->argc].cfname.data);
            free((char *)v1->param[v1->argc].init.data);
        }
        free(v1->param);
        return;
    case 1:
        v = (Obj *)v1->names;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static bool struct_same(const Obj *o1, const Obj *o2) {
    const Struct *v1 = (const Struct *)o1, *v2 = (const Struct *)o2;
    size_t i;
    if (o1->obj != o2->obj || v1->size != v2->size || v1->file_list != v2->file_list || v1->line != v2->line || v1->argc != v2->argc) return false;
    if (v1->names != v2->names && !v1->names->v.obj->same(&v1->names->v, &v2->names->v)) return false;
    for (i = 0; i < v1->argc; i++) {
        if (str_cmp(&v1->param[i].cfname, &v2->param[i].cfname) != 0) return false;
        if (str_cmp(&v1->param[i].init, &v2->param[i].init) != 0) return false;
    }
    return true;
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

void obj_init(Type *obj) {
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
    obj->slice = invalid_slice;
    obj->ival = invalid_ival;
    obj->uval = invalid_uval;
    obj->address = invalid_address;
    obj->sign = invalid_sign;
    obj->absolute = invalid_abs;
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
    typeobj_init();
    noneobj_init();

    new_type(&lbl_obj, T_LBL, "lbl", sizeof(Lbl));
    obj_init(&lbl_obj);
    lbl_obj.same = lbl_same;
    new_type(&mfunc_obj, T_MFUNC, "function", sizeof(Mfunc));
    obj_init(&mfunc_obj);
    mfunc_obj.destroy = mfunc_destroy;
    mfunc_obj.garbage = mfunc_garbage;
    mfunc_obj.same = mfunc_same;
    mfunc_obj.calc2 = mfunc_calc2;
    new_type(&struct_obj, T_STRUCT, "struct", sizeof(Struct));
    obj_init(&struct_obj);
    struct_obj.destroy = struct_destroy;
    struct_obj.garbage = struct_garbage;
    struct_obj.same = struct_same;
    struct_obj.size = struct_size;
    struct_obj.calc2 = struct_calc2;
    new_type(&union_obj, T_UNION, "union", sizeof(Union));
    obj_init(&union_obj);
    union_obj.destroy = struct_destroy;
    union_obj.garbage = struct_garbage;
    union_obj.same = struct_same;
    union_obj.size = struct_size;
    union_obj.calc2 = struct_calc2;
    new_type(&ident_obj, T_IDENT, "ident", sizeof(Ident));
    obj_init(&ident_obj);
    ident_obj.calc2 = ident_calc2;
    ident_obj.rcalc2 = ident_rcalc2;
    new_type(&anonident_obj, T_ANONIDENT, "anonident", sizeof(Anonident));
    obj_init(&anonident_obj);
    anonident_obj.calc2 = ident_calc2;
    anonident_obj.rcalc2 = ident_rcalc2;
    new_type(&default_obj, T_DEFAULT, "default", sizeof(Default));
    obj_init(&default_obj);
    new_type(&iter_obj, T_ITER, "iter", sizeof(Iter));
    obj_init(&iter_obj);
    iter_obj.destroy = iter_destroy;
    iter_obj.garbage = iter_garbage;
    iter_obj.next = iter_next;
    new_type(&funcargs_obj, T_FUNCARGS, "funcargs", sizeof(Funcargs));
    obj_init(&funcargs_obj);
    funcargs_obj.same = funcargs_same;

    default_value = (Default *)val_alloc(DEFAULT_OBJ);
}

void objects_destroy(void) {
    listobj_destroy();
    bitsobj_destroy();
    bytesobj_destroy();
    strobj_destroy();
    boolobj_destroy();
    intobj_destroy();
    gapobj_destroy();
    noneobj_destroy();

#ifdef DEBUG
    if (default_value->v.refcount != 1) fprintf(stderr, "default %" PRIuSIZE "\n", default_value->v.refcount - 1);
#endif

    val_destroy(&default_value->v);
}
