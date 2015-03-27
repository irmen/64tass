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
#include "listobj.h"
#include "eval.h"
#include "variables.h"
#include "boolobj.h"
#include "error.h"
#include "codeobj.h"
#include "strobj.h"
#include "intobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type list_obj;
static Type tuple_obj;
static Type addrlist_obj;
static Type colonlist_obj;

Type *LIST_OBJ = &list_obj;
Type *TUPLE_OBJ = &tuple_obj;
Type *ADDRLIST_OBJ = &addrlist_obj;
Type *COLONLIST_OBJ = &colonlist_obj;
Tuple *null_tuple;
List *null_list;
Addrlist *null_addrlist;

static void destroy(Obj *o1) {
    List *v1 = (List *)o1;
    size_t i;
    for (i = 0; i < v1->len; i++) {
        val_destroy(v1->data[i]);
    }
    if (v1->val != v1->data) free(v1->data);
}

static void garbage(Obj *o1, int j) {
    List *v1 = (List *)o1;
    size_t i;
    Obj *v;
    switch (j) {
    case -1:
        for (i = 0; i < v1->len; i++) {
            v1->data[i]->refcount--;
        }
        return;
    case 0:
        if (v1->val != v1->data) free(v1->data);
        return;
    case 1:
        for (i = 0; i < v1->len; i++) {
            v = v1->data[i];
            if (v->refcount & SIZE_MSB) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        return;
    }
}

static Obj **lnew(List *v, size_t len) {
    if (len > sizeof(v->val) / sizeof(Obj *)) {
        Obj **s = (Obj **)mallocx(len * sizeof(Obj *));
        if (len > SIZE_MAX / sizeof(Obj *)) err_msg_out_of_memory(); /* overflow */
        return s; 
    }
    return v->val;
}

static MUST_CHECK Obj *list_create(Obj *o1, linepos_t epoint) {
    Tuple *v1;
    List *v;
    Obj **vals;
    size_t i;
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_LIST: return val_reference(o1);
    case T_TUPLE:
        v1 = (Tuple *)o1;
        v = new_list();
        v->data = vals = lnew(v, v1->len);
        for (i = 0;i < v1->len; i++) {
            vals[i] = val_reference(v1->data[i]);
        }
        v->len = i;
        return &v->v;
    case T_CODE: return tuple_from_code((Code *)o1, LIST_OBJ, epoint);
    default: break;
    }
    err_msg_wrong_type(o1, NULL, epoint);
    return (Obj *)ref_none();
}

static MUST_CHECK Obj *tuple_create(Obj *o1, linepos_t epoint) {
    List *v1;
    Tuple *v;
    Obj **vals;
    size_t i;
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE: return val_reference(o1);
    case T_LIST:
        v1 = (List *)o1;
        v = new_tuple();
        v->data = vals = lnew(v, v1->len);
        for (i = 0;i < v1->len; i++) {
            vals[i] = val_reference(v1->data[i]);
        }
        v->len = i;
        return &v->v;
    case T_CODE: return tuple_from_code((Code *)o1, TUPLE_OBJ, epoint);
    default: break;
    }
    err_msg_wrong_type(o1, NULL, epoint);
    return (Obj *)ref_none();
}

static int same(const Obj *o1, const Obj *o2) {
    const List *v1 = (const List *)o1, *v2 = (const List *)o2;
    size_t i;
    if (o1->obj != o2->obj || v1->len != v2->len) return 0;
    for (i = 0; i < v2->len; i++) {
        Obj *val = v1->data[i];
        if (!val->obj->same(val, v2->data[i])) return 0;
    }
    return 1;
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t epoint) {
    List *v1 = (List *)o1;
    size_t i;
    Obj *val;
    Error *err;
    switch (type) {
    case TRUTH_ALL:
        for (i = 0; i < v1->len; i++) {
            val = v1->data[i]->obj->truth(v1->data[i], type, epoint);
            if (val->obj != BOOL_OBJ) return val;
            if (!((Bool *)val)->boolean) return val;
            val_destroy(val);
        }
        return (Obj *)ref_bool(true_value);
    case TRUTH_ANY:
        for (i = 0; i < v1->len; i++) {
            val = v1->data[i]->obj->truth(v1->data[i], type, epoint);
            if (val->obj != BOOL_OBJ) return val;
            if (((Bool *)val)->boolean) return val;
            val_destroy(val);
        }
        return (Obj *)ref_bool(false_value);
    default: 
        err = new_error(ERROR_____CANT_BOOL, epoint);
        err->u.objname = o1->obj->name;
        return &err->v;
    }
}

static MUST_CHECK Obj *repr_listtuple(Obj *o1, linepos_t epoint) {
    Tuple *v1 = (List *)o1;
    size_t i, len = (o1->obj == ADDRLIST_OBJ || o1->obj == COLONLIST_OBJ) ? 0 : 2, chars = 0;
    List *list = NULL;
    Obj **vals = NULL, *val;
    Str *v;
    uint8_t *s;
    size_t llen = v1->len;
    if (llen) {
        list = new_tuple();
        list->data = vals = lnew(list, llen);
        for (i = 0;i < llen; i++) {
            val = v1->data[i]->obj->repr(v1->data[i], epoint);
            if (!val || val->obj != STR_OBJ) {
                list->len = i;
                val_destroy(&list->v);
                return val;
            }
            len += ((Str *)val)->len;
            if (len < ((Str *)val)->len) err_msg_out_of_memory(); /* overflow */
            vals[i] = val;
        }
        list->len = i;
        if (i && (o1->obj != TUPLE_OBJ)) i--;
        if (i) {
            len += i;
            if (len < i) err_msg_out_of_memory(); /* overflow */
        }
    }
    v = new_str();
    s = str_create_elements(v, len);
    len = 0;
    if (o1->obj != ADDRLIST_OBJ && o1->obj != COLONLIST_OBJ) s[len++] = (o1->obj == LIST_OBJ) ? '[' : '(';
    for (i = 0;i < llen; i++) {
        Str *str = (Str *)vals[i];
        if (i) s[len++] = (o1->obj == COLONLIST_OBJ) ? ':' : ',';
        if (str->len) {
            memcpy(s + len, str->data, str->len);
            len += str->len;
            chars += str->len - str->chars;
        }
    }
    if (i == 1 && (o1->obj == TUPLE_OBJ)) s[len++] = ',';
    if (o1->obj != ADDRLIST_OBJ && o1->obj != COLONLIST_OBJ) s[len++] = (o1->obj == LIST_OBJ) ? ']' : ')';
    if (list) val_destroy(&list->v);
    v->data = s;
    v->len = len;
    v->chars = len - chars;
    return &v->v;
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    List *v1 = (List *)o1;
    return (Obj *)int_from_size(v1->len);
}

static MUST_CHECK Iter *getiter(Obj *v1) {
    Iter *v = (Iter *)val_alloc(ITER_OBJ);
    v->val = 0;
    v->iter = &v->val;
    v->data = val_reference(v1);
    return v;
}

static MUST_CHECK Obj *next(Iter *v1) {
    const List *vv1 = (List *)v1->data;
    if (v1->val >= vv1->len) return NULL;
    return val_reference(vv1->data[v1->val++]);
}

Obj **list_create_elements(List *v, size_t n) {
    return lnew(v, n);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *o1 = op->v1;
    List *v1 = (List *)o1, *v;
    if (v1->len) {
        Obj **vals;
        int error = 1;
        size_t i = 0;
        v = (List *)val_alloc(o1->obj);
        vals = lnew(v, v1->len);
        for (;i < v1->len; i++) {
            Obj *val;
            op->v1 = v1->data[i];
            val = op->v1->obj->calc1(op);
            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
            vals[i] = val;
        }
        op->v1 = &v1->v;
        v->len = i;
        v->data = vals;
        return &v->v;
    }
    return val_reference((o1->obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
}

static MUST_CHECK Obj *calc2_list(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = (List *)o1, *v2 = (List *)o2;
    size_t i;
    Error *err;

    switch (op->op->op) {
    case O_CMP:
    case O_EQ:
    case O_NE:
    case O_LT:
    case O_LE:
    case O_GT:
    case O_GE: 
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_ADD:
    case O_SUB:
    case O_AND:
    case O_OR:
    case O_XOR:
    case O_LSHIFT:
    case O_RSHIFT:
    case O_EXP:
    case O_MEMBER:
        {
            if (v1->len == v2->len) {
                if (v1->len) {
                    Obj **vals;
                    int error = 1;
                    List *v = (List *)val_alloc(o1->obj);
                    vals = lnew(v, v1->len);
                    for (i = 0; i < v1->len; i++) {
                        Obj *val;
                        op->v1 = v1->data[i];
                        op->v2 = v2->data[i];
                        val = op->v1->obj->calc2(op);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                        vals[i] = val;
                    }
                    op->v1 = &v1->v;
                    op->v2 = &v2->v;
                    v->len = i;
                    v->data = vals;
                    return &v->v;
                } 
                return val_reference(&v1->v);
            } 
            if (v1->len == 1) {
                Obj *v;
                op->v1 = v1->data[0];
                v = op->v2->obj->rcalc2(op);
                op->v1 = &v1->v;
                return v;
            } 
            if (v2->len == 1) {
                Obj *v;
                op->v2 = v2->data[0];
                v = op->v1->obj->calc2(op);
                op->v2 = &v2->v;
                return v;
            } 
            err = new_error(ERROR_CANT_BROADCAS, op->epoint3);
            err->u.broadcast.v1 = v1->len;
            err->u.broadcast.v2 = v2->len;
            return &err->v;
        }
    case O_CONCAT:
        {
            Obj **vals;
            size_t ln;
            List *v;

            if (!v1->len) {
                return val_reference(o2);
            }
            if (!v2->len) {
                return val_reference(o1);
            }
            ln = v1->len + v2->len;
            if (ln < v2->len) err_msg_out_of_memory(); /* overflow */
            v = (List *)val_alloc(o1->obj);
            vals = lnew(v, ln);
            for (i = 0; i < v1->len; i++) {
                vals[i] = val_reference(v1->data[i]);
            }
            for (; i < ln; i++) {
                vals[i] = val_reference(v2->data[i - v1->len]);
            }
            v->len = ln;
            v->data = vals;
            return &v->v;
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Obj **vals, *o1 = op->v1;
    List *v1 = (List *)o1, *v;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (err) return &err->v;

    if (v1->len && rep) {
        size_t i = 0, j, ln;
        if (rep == 1) {
            return val_reference(o1);
        }
        ln = v1->len * rep;
        if (v1->len > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = (List *)val_alloc(o1->obj);
        v->data = vals = lnew(v, ln);
        while (rep--) {
            for (j = 0;j < v1->len; j++, i++) {
                vals[i] = val_reference(v1->data[j]);
            }
        }
        v->len = i;
        return &v->v;
    }
    return val_reference((o1->obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
}

static inline MUST_CHECK Obj *slice(Colonlist *v2, oper_t op, size_t ln) {
    Obj **vals, *o1 = op->v1;
    List *v, *v1 = (List *)o1;
    size_t i;
    size_t length;
    ival_t offs, end, step;
    Obj *err;

    err = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (err) return err;

    if (!length) {
        return val_reference((o1->obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
    }

    if (step == 1 && length == v1->len) {
        return val_reference(o1); /* original tuple */
    }
    v = (List *)val_alloc(o1->obj);
    v->data = vals = lnew(v, length);
    i = 0;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        vals[i++] = val_reference(v1->data[offs]);
        offs += step;
    }
    v->len = i;
    return &v->v;
}

static inline MUST_CHECK Obj *iindex(oper_t op) {
    Obj **vals;
    size_t offs, i, ln;
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = (List *)o1;
    Error *err;
    Funcargs *args = (Funcargs *)o2;

    if (args->len != 1) {
        err_msg_argnum(args->len, 1, 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val->val;

    ln = v1->len;

    if (o2->obj == LIST_OBJ) {
        List *v2 = (List *)o2, *v;
        int error = 1;
        if (!v2->len) {
            return val_reference((o1->obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
        }
        v = (List *)val_alloc(o1->obj);
        v->data = vals = lnew(v, v2->len);
        for (i = 0; i < v2->len; i++) {
            err = indexoffs(v2->data[i], ln, &offs, op->epoint2);
            if (err) {
                if (error) {err_msg_output(err); error = 0;} 
                val_destroy(&err->v);
                vals[i] = (Obj *)ref_none();
                continue;
            }
            vals[i] = val_reference(v1->data[offs]);
        }
        v->len = i;
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        return slice((Colonlist *)o2, op, ln);
    }
    err = indexoffs(o2, ln, &offs, op->epoint2);
    if (err) return &err->v;
    return val_reference(v1->data[offs]);
}


static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = (List *)o1;
    size_t i = 0;
    Obj **vals;

    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op); 
    }
    if (op->op == &o_IN) {
        return o2->obj->rcalc2(op);
    }
    if (o1->obj == o2->obj && (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ)) {
        return calc2_list(op);
    }
    if (o2->obj == NONE_OBJ || o2->obj == ERROR_OBJ) {
        return o2->obj->rcalc2(op);
    }
    if (v1->len) {
        int error = 1;
        List *list = (List *)val_alloc(o1->obj);
        list->data = vals = lnew(list, v1->len);
        for (;i < v1->len; i++) {
            Obj *val;
            op->v1 = v1->data[i];
            val = op->v1->obj->calc2(op);
            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
            vals[i] = val;
        }
        op->v1 = o1;
        list->len = i;
        return &list->v;
    }
    return val_reference(o1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v2 = (List *)o2;
    size_t i = 0;
    Obj **vals;

    if (op->op == &o_IN) {
        op->op = &o_EQ;
        for (;i < v2->len; i++) {
            Obj *result;
            op->v2 = v2->data[i];
            result = o1->obj->calc2(op);
            if (result->obj == BOOL_OBJ && ((Bool *)result)->boolean) {
                op->op = &o_IN;
                op->v2 = o2;
                return result;
            }
            val_destroy(result);
        }
        op->op = &o_IN;
        op->v2 = o2;
        return (Obj *)ref_bool(false_value);
    }
    if (o1->obj == o2->obj && (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ)) {
        return calc2_list(op);
    }
    if (o1->obj == NONE_OBJ || o1->obj == ERROR_OBJ) {
        return o1->obj->calc2(op);
    }
    if (v2->len) {
        int error = 1;
        List *v = (List *)val_alloc(o2->obj);
        v->data = vals = lnew(v, v2->len);
        for (;i < v2->len; i++) {
            Obj *val;
            op->v2 = v2->data[i];
            val = op->v1->obj->calc2(op);
            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
            vals[i] = val;
        }
        op->v2 = o2;
        v->len = i;
        return &v->v;
    }
    return val_reference(o2);
}

static void init(Type *obj) {
    obj->destroy = destroy;
    obj->garbage = garbage;
    obj->same = same;
    obj->truth = truth;
    obj->len = len;
    obj->getiter = getiter;
    obj->next = next;
    obj->calc1 = calc1;
    obj->calc2 = calc2;
    obj->rcalc2 = rcalc2;
    obj->repr = repr_listtuple;
}

void listobj_init(void) {
    new_type(&list_obj, T_LIST, "list", sizeof(List));
    obj_init(&list_obj);
    init(&list_obj);
    list_obj.create = list_create;
    new_type(&tuple_obj, T_TUPLE, "tuple", sizeof(Tuple));
    obj_init(&tuple_obj);
    init(&tuple_obj);
    tuple_obj.create = tuple_create;
    new_type(&addrlist_obj, T_ADDRLIST, "addresslist", sizeof(Addrlist));
    obj_init(&addrlist_obj);
    addrlist_obj.destroy = destroy;
    addrlist_obj.garbage = garbage;
    addrlist_obj.same = same;
    addrlist_obj.repr = repr_listtuple;
    new_type(&colonlist_obj, T_COLONLIST, "colonlist", sizeof(Colonlist));
    obj_init(&colonlist_obj);
    colonlist_obj.destroy = destroy;
    colonlist_obj.garbage = garbage;
    colonlist_obj.same = same;
    colonlist_obj.repr = repr_listtuple;

    null_tuple = new_tuple();
    null_tuple->len = 0;
    null_tuple->data = NULL;
    null_list = new_list();
    null_list->len = 0;
    null_list->data = NULL;
    null_addrlist = new_addrlist();
    null_addrlist->len = 0;
    null_addrlist->data = NULL;
}

void listobj_names(void) {
    new_builtin("list", val_reference(&LIST_OBJ->v));
    new_builtin("tuple", val_reference(&TUPLE_OBJ->v));
}

void listobj_destroy(void) {
#ifdef DEBUG
    if (null_tuple->v.refcount != 1) fprintf(stderr, "tuple %d\n", null_tuple->v.refcount - 1);
    if (null_list->v.refcount != 1) fprintf(stderr, "list %d\n", null_list->v.refcount - 1);
    if (null_addrlist->v.refcount != 1) fprintf(stderr, "addrlist %d\n", null_addrlist->v.refcount - 1);
#endif

    val_destroy(&null_tuple->v);
    val_destroy(&null_list->v);
    val_destroy(&null_addrlist->v);
}
