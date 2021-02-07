/*
    $Id: listobj.c 2328 2021-02-06 04:39:54Z soci $

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
#include "listobj.h"
#include <string.h>
#include "eval.h"
#include "variables.h"
#include "error.h"
#include "arguments.h"

#include "boolobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "intobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "foldobj.h"

static Type list_obj;
static Type tuple_obj;
static Type addrlist_obj;
static Type colonlist_obj;

Type *const LIST_OBJ = &list_obj;
Type *const TUPLE_OBJ = &tuple_obj;
Type *const ADDRLIST_OBJ = &addrlist_obj;
Type *const COLONLIST_OBJ = &colonlist_obj;
Tuple *null_tuple;
List *null_list;
Addrlist *null_addrlist;

static FAST_CALL NO_INLINE void list_destroy(List *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    List *v1 = (List *)o1;
    size_t i;
    for (i = 0; i < v1->len; i++) {
        val_destroy(v1->data[i]);
    }
    if (v1->u.val != v1->data) list_destroy(v1);
}

static FAST_CALL void garbage(Obj *o1, int j) {
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
        if (v1->u.val != v1->data) free(v1->data);
        return;
    case 1:
        for (i = 0; i < v1->len; i++) {
            v = v1->data[i];
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        return;
    }
}

static Obj **lnew(List *v, size_t len) {
    v->len = len;
    if (len <= lenof(v->u.val)) {
        v->data = v->u.val;
        return v->u.val;
    }
    if (len <= SIZE_MAX / sizeof *v->data) { /* overflow */
        Obj **n = (Obj **)malloc(len * sizeof *v->data);
        if (n != NULL) {
            v->data = n;
            v->u.s.max = len;
            v->u.s.hash = -1;
            return n;
        }
    }
    v->len = 0;
    v->data = v->u.val;
    val_destroy(&v->v);
    return NULL;
}

static Obj **lextend(List *v, size_t len) {
    Obj **tmp;
    if (len <= lenof(v->u.val)) {
        return v->u.val;
    }
    if (len > SIZE_MAX / sizeof *v->data) return NULL; /* overflow */
    if (v->u.val != v->data) {
        size_t len2;
        if (len <= v->u.s.max) return v->data;
        len2 = len + (len < 1024 ? len : 1024);
        if (len2 > len) len = len2;
        tmp = (Obj **)realloc(v->data, len * sizeof *v->data);
        if (tmp != NULL) {
            v->data = tmp;
            v->u.s.max = len;
            v->u.s.hash = -1;
        }
        return tmp;
    }
    tmp = (Obj **)malloc(len * sizeof *v->data);
    if (tmp != NULL) {
        memcpy(tmp, v->u.val, v->len * sizeof *v->data);
        v->data = tmp;
        v->u.s.max = len;
        v->u.s.hash = -1;
    }
    return tmp;
}

static MUST_CHECK Obj *tuple_from_list(List *v1, Type *typ, linepos_t epoint) {
    size_t i, ln;
    Obj **vals, **data = v1->data;
    Tuple *v = (Tuple *)val_alloc(typ);

    ln = v1->len;
    vals = lnew(v, ln);
    if (vals == NULL) return (Obj *)new_error_mem(epoint);

    for (i = 0; i < ln; i++) {
        vals[i] = val_reference(data[i]);
    }
    return &v->v;
}

static MUST_CHECK Obj *list_create(Obj *o1, linepos_t epoint) {
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_LIST: return val_reference(o1);
    case T_TUPLE: return tuple_from_list((List *)o1, LIST_OBJ, epoint);
    case T_CODE: return tuple_from_code((Code *)o1, LIST_OBJ);
    default: break;
    }
    return (Obj *)new_error_conv(o1, LIST_OBJ, epoint);
}

static MUST_CHECK Obj *tuple_create(Obj *o1, linepos_t epoint) {
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE: return val_reference(o1);
    case T_LIST: return tuple_from_list((List *)o1, TUPLE_OBJ, epoint);
    case T_CODE: return tuple_from_code((Code *)o1, TUPLE_OBJ);
    default: break;
    }
    return (Obj *)new_error_conv(o1, TUPLE_OBJ, epoint);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const List *v1 = (const List *)o1, *v2 = (const List *)o2;
    size_t i;
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    for (i = 0; i < v2->len; i++) {
        Obj *val = v1->data[i];
        if (!val->obj->same(val, v2->data[i])) return false;
    }
    return true;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    List *v1 = (List *)o1;
    size_t i;
    Obj *val, *val2;
    switch (type) {
    case TRUTH_ALL:
        val2 = NULL;
        for (i = 0; i < v1->len; i++) {
            val = v1->data[i]->obj->truth(v1->data[i], type, epoint);
            if ((Bool *)val == true_value) {
                val_destroy(val);
                continue;
            }
            if (val2 != NULL) val_destroy(val2);
            if ((Bool *)val == false_value) {
                return val;
            }
            val2 = val;
        }
        return (val2 != NULL) ? val2 : (Obj *)ref_bool(true_value);
    case TRUTH_ANY:
        val2 = NULL;
        for (i = 0; i < v1->len; i++) {
            val = v1->data[i]->obj->truth(v1->data[i], type, epoint);
            if ((Bool *)val == false_value) {
                val_destroy(val);
                continue;
            }
            if (val2 != NULL) val_destroy(val2);
            if ((Bool *)val == true_value) return val;
            val2 = val;
        }
        return (val2 != NULL) ? val2 : (Obj *)ref_bool(false_value);
    default:
        return DEFAULT_OBJ->truth(o1, type, epoint);
    }
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t epoint) {
    List *v1 = (List *)o1;
    size_t i, l = v1->len;
    Obj **vals = v1->data;
    unsigned int h;
    if (vals != v1->u.val && v1->u.s.hash >= 0) {
        *hs = v1->u.s.hash;
        return NULL;
    }
    h = 0;
    for (i = 0; i < l; i++) {
        int h2;
        Obj *o2 = vals[i];
        Error *err = o2->obj->hash(o2, &h2, epoint);
        if (err != NULL) return err;
        h += h2;
    }
    h ^= i;
    h &= ((~0U) >> 1);
    if (vals != v1->u.val) v1->u.s.hash = h;
    *hs = h;
    return NULL;
}

static MUST_CHECK Obj *repr_listtuple(Obj *o1, linepos_t epoint, size_t maxsize) {
    Tuple *v1 = (List *)o1;
    bool tupleorlist = (o1->obj != ADDRLIST_OBJ && o1->obj != COLONLIST_OBJ);
    size_t i, len = tupleorlist ? 2 : 0, chars = len;
    List *list = NULL;
    Obj **vals = NULL, *val;
    Str *v;
    uint8_t *s;
    size_t llen = v1->len;
    if (llen != 0) {
        i = (llen != 1 || o1->obj != TUPLE_OBJ) ? (llen - 1) : llen;
        len += i;
        if (len < i) return NULL; /* overflow */
        chars = len;
        if (chars > maxsize) return NULL;
        list = (Tuple *)val_alloc(TUPLE_OBJ);
        vals = lnew(list, llen);
        if (vals == NULL) return (Obj *)new_error_mem(epoint);
        for (i = 0;i < llen; i++) {
            Obj *o2 = v1->data[i];
            if (o2 == &default_value->v && o1->obj == COLONLIST_OBJ) {
                val = (Obj *)ref_str(null_str);
            } else {
                val = o2->obj->repr(o2, epoint, maxsize - chars);
                if (val == NULL || val->obj != STR_OBJ) goto error;
            }
            v = (Str *)val;
            len += v->len;
            if (len < v->len) goto error2; /* overflow */
            chars += v->chars;
            if (chars > maxsize) {
            error2:
                val_destroy(val);
                val = NULL;
            error:
                list->len = i;
                val_destroy(&list->v);
                return val;
            }
            vals[i] = val;
        }
        list->len = i;
    } else if (chars > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) {
        if (list != NULL) val_destroy(&list->v);
        return NULL;
    }
    v->chars = chars;
    s = v->data;
    if (tupleorlist) *s++ = (o1->obj == LIST_OBJ) ? '[' : '(';
    for (i = 0; i < llen; i++) {
        Str *str = (Str *)vals[i];
        if (i != 0) *s++ = (o1->obj == COLONLIST_OBJ) ? ':' : ',';
        if (str->len != 0) {
            memcpy(s, str->data, str->len);
            s += str->len;
        }
    }
    if (i == 1 && o1->obj == TUPLE_OBJ) *s++ = ',';
    if (tupleorlist) *s = (o1->obj == LIST_OBJ) ? ']' : ')';
    if (list != NULL) val_destroy(&list->v);
    return &v->v;
}

static MUST_CHECK Obj *len(oper_t op) {
    List *v1 = (List *)op->v2;
    return (Obj *)int_from_size(v1->len);
}

static FAST_CALL MUST_CHECK Obj *iter_forward(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return ((List *)v1->data)->data[v1->val++];
}

static void getiter(struct iter_s *v) {
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_forward;
    v->len = ((List *)v->data)->len;
}

static FAST_CALL MUST_CHECK Obj *iter_reverse(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return ((List *)v1->data)->data[v1->len - ++v1->val];
}

static void getriter(struct iter_s *v) {
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_reverse;
    v->len = ((List *)v->data)->len;
}

Obj **list_create_elements(List *v, size_t n) {
    if (n <= lenof(v->u.val)) return v->u.val;
    if (n > SIZE_MAX / sizeof *v->data) err_msg_out_of_memory(); /* overflow */
    v->u.s.max = n;
    v->u.s.hash = -1;
    return (Obj **)mallocx(n * sizeof *v->data);
}

MUST_CHECK bool list_extend(List *lst) {
    Obj **vals;
    size_t o = lst->len, n;
    if (lst->data == lst->u.val) {
        n = 16;
        vals = (Obj **)malloc(n * sizeof *lst->data);
        if (vals == NULL) return true;
        memcpy(vals, lst->u.val, o * sizeof *lst->data);
    } else {
        if (o < 256) n = o * 2;
        else {
            n = o + 256;
            if (/*n < 256 ||*/ n > SIZE_MAX / sizeof *lst->data) return true; /* overflow */
        }
        vals = (Obj **)realloc(lst->data, n * sizeof *lst->data);
        if (vals == NULL) return true;
    }
    lst->data = vals;
    lst->u.s.max = n;
    lst->u.s.hash = -1;
    return false;
}

void list_shrink(List *lst, size_t i) {
    size_t j = i;
    while (j < lst->len) val_destroy(lst->data[j++]);
    lst->len = i;
    if (lst->data != lst->u.val) {
        if (lst->len <= lenof(lst->u.val)) {
            memcpy(lst->u.val, lst->data, lst->len * sizeof *lst->data);
            free(lst->data);
            lst->data = lst->u.val;
        } else {
            Obj **v = (Obj **)realloc(lst->data, lst->len * sizeof *lst->data);
            if (v != NULL) {
                lst->data = v;
                lst->u.s.max = lst->len;
                lst->u.s.hash = -1;
            }
        }
    }
}

MUST_CHECK Tuple *new_tuple(size_t n) {
     Tuple *v = (Tuple *)val_alloc(TUPLE_OBJ);
     v->len = n;
     if (n <= lenof(v->u.val)) {
         v->data = v->u.val;
         return v;
     }
     if (n > SIZE_MAX / sizeof *v->data) err_msg_out_of_memory(); /* overflow */
     v->u.s.max = n;
     v->u.s.hash = -1;
     v->data = (Obj **)mallocx(n * sizeof *v->data);
     return v;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *o1 = op->v1;
    List *v1 = (List *)o1, *v;
    if (v1->len != 0) {
        Obj **vals;
        bool inplace = (op->inplace == o1);
        size_t i;
        if (inplace) {
            v = ref_list(v1);
            vals = v1->data;
            if (vals != v->u.val) v->u.s.hash = -1;
        } else {
            v = (List *)val_alloc(o1->obj);
            vals = lnew(v, v1->len);
            if (vals == NULL) return (Obj *)new_error_mem(op->epoint3);
        }
        for (i = 0; i < v1->len; i++) {
            Obj *val = v1->data[i];
            op->v1 = val;
            op->inplace = (inplace && val->refcount == 1) ? val : NULL;
            val = op->v1->obj->calc1(op);
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return &v->v;
    }
    return val_reference(o1);
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
    case O_MIN:
    case O_LT:
    case O_LE:
    case O_MAX:
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
    case O_LAND:
    case O_LOR:
        {
            if (v1->len == v2->len) {
                if (v1->len != 0) {
                    Obj **vals;
                    bool minmax = (op->op == &o_MIN) || (op->op == &o_MAX);
                    List *v, *inplace;
                    if (op->inplace == &v1->v) {
                        v = ref_list(v1);
                        vals = v1->data;
                        inplace = v1;
                        if (vals != v->u.val) v->u.s.hash = -1;
                    } else if (o1->obj == o2->obj && op->inplace == &v2->v) {
                        v = ref_list(v2);
                        vals = v2->data;
                        inplace = v2;
                        if (vals != v->u.val) v->u.s.hash = -1;
                    } else {
                        v = (List *)val_alloc(o1->obj);
                        vals = lnew(v, v1->len);
                        if (vals == NULL) goto failed;
                        inplace = NULL;
                    }
                    for (i = 0; i < v1->len; i++) {
                        Obj *oo1 = op->v1 = v1->data[i];
                        Obj *oo2 = op->v2 = v2->data[i];
                        Obj *val;
                        op->inplace = (inplace == v1 && oo1->refcount == 1 && !minmax) ? oo1 : NULL;
                        val = op->v1->obj->calc2(op);
                        if (minmax) {
                            if (val == &true_value->v) val_replace(&val, oo1);
                            else if (val == &false_value->v) val_replace(&val, oo2);
                        }
                        if (inplace != NULL) val_destroy(vals[i]);
                        vals[i] = val;
                    }
                    return &v->v;
                }
                return val_reference(&v1->v);
            }
            if (v1->len == 1) {
                if (op->inplace == o1) op->inplace = NULL;
                op->v1 = v1->data[0];
                return op->v2->obj->rcalc2(op);
            }
            if (v2->len == 1) {
                if (op->inplace == o2) op->inplace = NULL;
                op->v2 = v2->data[0];
                return op->v1->obj->calc2(op);
            }
            err = new_error(ERROR_CANT_BROADCAS, op->epoint3);
            err->u.broadcast.v1 = v1->len;
            err->u.broadcast.v2 = v2->len;
            return &err->v;
        }
    case O_CONCAT:
        {
            Obj **vals;
            size_t ln, j;
            List *v;

            if (v1->len == 0) {
                return val_reference(o2);
            }
            if (v2->len == 0) {
                return val_reference(o1);
            }
            ln = v1->len + v2->len;
            if (ln < v2->len) goto failed; /* overflow */
            if (op->inplace == &v1->v) {
                vals = lextend(v1, ln);
                if (vals == NULL) goto failed;
                i = v1->len;
                v1->len = ln;
                v = (List *)val_reference(o1);
            } else if (o1->obj == o2->obj && op->inplace == &v2->v) {
                vals = lextend(v2, ln);
                if (vals == NULL) goto failed;
                memmove(vals + v1->len, v2->data, v2->len * sizeof *v2->data);
                v2->len = ln;
                for (i = 0; i < v1->len; i++) {
                    vals[i] = val_reference(v1->data[i]);
                }
                return val_reference(o2);
            } else {
                v = (List *)val_alloc(o1->obj);
                vals = lnew(v, ln);
                if (vals == NULL) goto failed;
                for (i = 0; i < v1->len; i++) {
                    vals[i] = val_reference(v1->data[i]);
                }
            }
            for (j = 0; i < ln; i++) {
                vals[i] = val_reference(v2->data[j++]);
            }
            return &v->v;
        }
    default: break;
    }
    return obj_oper_error(op);
failed:
    return (Obj *)new_error_mem(op->epoint3);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Obj **vals, *o1 = op->v1;
    List *v1 = (List *)o1, *v;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return &err->v;

    if (v1->len == 0 || rep == 0) return val_reference((o1->obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
    do {
        size_t i = 0, j, ln;
        if (rep == 1) {
            return val_reference(o1);
        }
        if (v1->len > SIZE_MAX / rep) break; /* overflow */
        v = (List *)val_alloc(o1->obj);
        ln = v1->len * rep;
        vals = lnew(v, ln);
        if (vals == NULL) break;
        while ((rep--) != 0) {
            for (j = 0;j < v1->len; j++, i++) {
                vals[i] = val_reference(v1->data[j]);
            }
        }
        return &v->v;
    } while (false);
    return (Obj *)new_error_mem(op->epoint3);
}

MUST_CHECK Error *indexoffs(Obj *v1, size_t len, size_t *offs, linepos_t epoint) {
    ival_t ival;
    Error *err = v1->obj->ival(v1, &ival, 8 * sizeof ival, epoint);
    if (err != NULL) return err;

    if (ival >= 0) {
        if ((uval_t)ival < len) {
            *offs = (uval_t)ival;
            return NULL;
        }
    } else {
        ival = -ival;
        if ((uval_t)ival <= len) {
            *offs = len - (uval_t)ival;
            return NULL;
        }
    }
    return new_error_obj(ERROR___INDEX_RANGE, v1, epoint);
}

MUST_CHECK Obj *sliceparams(const Colonlist *v2, size_t len2, struct sliceparam_s *s, linepos_t epoint) {
    Error *err;
    ival_t len, offs, end, step = 1;

    if (len2 >= (1U << (8 * sizeof(ival_t) - 1))) return (Obj *)new_error_mem(epoint); /* overflow */
    len = (ival_t)len2;
    if (v2->len > 3 || v2->len < 1) {
        return (Obj *)new_error_argnum(v2->len, 1, 3, epoint);
    }
    end = len;
    if (v2->len > 2) {
        if (v2->data[2] != &default_value->v) {
            err = v2->data[2]->obj->ival(v2->data[2], &step, 8 * sizeof step, epoint);
            if (err != NULL) return &err->v;
            if (step == 0) {
                return (Obj *)new_error(ERROR_NO_ZERO_VALUE, epoint);
            }
        }
    }
    if (v2->len > 1) {
        if (v2->data[1] == &default_value->v) end = (step > 0) ? len : -1;
        else {
            err = v2->data[1]->obj->ival(v2->data[1], &end, 8 * sizeof end, epoint);
            if (err != NULL) return &err->v;
            if (end >= 0) {
                if (end > len) end = len;
            } else {
                end += len;
                if (end < -1) end = -1;
            }
        }
    } else end = len;
    if (v2->data[0] == &default_value->v) offs = (step > 0) ? 0 : len - 1;
    else {
        ival_t minus;
        err = v2->data[0]->obj->ival(v2->data[0], &offs, 8 * sizeof offs, epoint);
        if (err != NULL) return &err->v;
        minus = (step < 0) ? -1 : 0;
        if (offs >= 0) {
            if (offs > len + minus) offs = len + minus;
        } else {
            offs += len;
        }
        if (offs < minus) offs = minus;
    }

    if (step > 0) {
        if (offs > end) offs = end;
        s->length = (uval_t)(end - offs + step - 1) / (uval_t)step;
    } else {
        if (end > offs) end = offs;
        s->length = (uval_t)(offs - end - step - 1) / (uval_t)-step;
    }

    s->offset = offs;
    s->end = end;
    s->step = step;
    return NULL;
}

static MUST_CHECK Obj *slice(oper_t op, size_t indx) {
    Obj **vals;
    Obj *o2 = op->v2;
    size_t offs2;
    List *v, *v1 = (List *)op->v1;
    Funcargs *args = (Funcargs *)o2;
    size_t i, ln;
    Error *err;
    bool more = args->len > indx + 1;
    linepos_t epoint2;

    if (args->len < 1) {
        return (Obj *)new_error_argnum(args->len, 1, 0, op->epoint2);
    }

    o2 = args->val[indx].val;
    epoint2 = &args->val[indx].epoint;

    ln = v1->len;

    if (o2->obj->iterable) {
        struct iter_s iter;
        iter.data = o2; o2->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference((v1->v.obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
        }
        v = (List *)val_alloc(v1->v.obj);
        vals = lnew(v, iter.len);
        if (vals == NULL) {
            iter_destroy(&iter);
            goto failed;
        }
        for (i = 0; i < iter.len && (o2 = iter.next(&iter)) != NULL; i++) {
            err = indexoffs(o2, ln, &offs2, epoint2);
            if (err != NULL) {
                vals[i] = &err->v;
                continue;
            }
            if (more) {
                op->v1 = v1->data[offs2];
                vals[i] = op->v1->obj->slice(op, indx + 1);
            } else {
                vals[i] = val_reference(v1->data[offs2]);
            }
        }
        v->len = i;
        iter_destroy(&iter);
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;

        err = (Error *)sliceparams((Colonlist *)o2, ln, &s, epoint2);
        if (err != NULL) return &err->v;

        if (s.length == 0) {
            return val_reference((v1->v.obj == TUPLE_OBJ) ? &null_tuple->v : &null_list->v);
        }

        if (s.step == 1 && s.length == v1->len && !more) {
            return val_reference(&v1->v); /* original tuple */
        }
        v = (List *)val_alloc(v1->v.obj);
        vals = lnew(v, s.length);
        if (vals == NULL) goto failed;
        for (i = 0; i < s.length; i++) {
            if (more) {
                op->v1 = v1->data[s.offset];
                vals[i] = op->v1->obj->slice(op, indx + 1);
            } else {
                vals[i] = val_reference(v1->data[s.offset]);
            }
            s.offset += s.step;
        }
        return &v->v;
    }
    err = indexoffs(o2, ln, &offs2, epoint2);
    if (err != NULL) return &err->v;
    if (more) {
        op->v1 = v1->data[offs2];
        return op->v1->obj->slice(op, indx + 1);
    }
    return val_reference(v1->data[offs2]);
failed:
    return (Obj *)new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = (List *)o1;
    size_t i;
    Obj **vals;

    if (op->op == &o_X) {
        if (o2 == &none_value->v || o2->obj == ERROR_OBJ) return val_reference(o2);
        return repeat(op);
    }
    if (op->op == &o_IN || o2->obj == FOLD_OBJ) {
        return o2->obj->rcalc2(op);
    }
    if (o2->obj == TUPLE_OBJ || o2->obj == LIST_OBJ) {
        return calc2_list(op);
    }
    if (o2 == &none_value->v || o2->obj == ERROR_OBJ) {
        return val_reference(o2);
    }
    if (v1->len != 0) {
        bool minmax = (op->op == &o_MIN) || (op->op == &o_MAX), inplace = (op->inplace == o1);
        List *list;
        if (inplace) {
            list = (List *)val_reference(o1);
            vals = list->data;
            if (vals != list->u.val) list->u.s.hash = -1;
        } else {
            list = (List *)val_alloc(o1->obj);
            vals = lnew(list, v1->len);
            if (vals == NULL) return (Obj *)new_error_mem(op->epoint3);
        }
        for (i = 0; i < v1->len; i++) {
            Obj *val;
            Obj *oo1 = v1->data[i];
            op->v1 = oo1;
            op->v2 = o2;
            op->inplace = (inplace && oo1->refcount == 1 && !minmax) ? oo1 : NULL;
            val = op->v1->obj->calc2(op);
            if (minmax) {
                if (val == &true_value->v) val_replace(&val, oo1);
                else if (val == &false_value->v) val_replace(&val, o2);
            }
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return &list->v;
    }
    return val_reference(o1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v2 = (List *)o2;
    size_t i;
    Obj **vals;

    if (op->op == &o_IN) {
        op->op = &o_EQ;
        for (i = 0; i < v2->len; i++) {
            Obj *result;
            op->v1 = o1;
            op->v2 = v2->data[i];
            op->inplace = NULL;
            result = o1->obj->calc2(op);
            if ((Bool *)result == true_value) {
                op->op = &o_IN;
                return result;
            }
            val_destroy(result);
        }
        op->op = &o_IN;
        return (Obj *)ref_bool(false_value);
    }
    if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ) {
        return calc2_list(op);
    }
    if (o1 == &none_value->v || o1->obj == ERROR_OBJ || o1->obj == FOLD_OBJ) {
        return o1->obj->calc2(op);
    }
    if (v2->len != 0) {
        bool minmax = (op->op == &o_MIN) || (op->op == &o_MAX), inplace = (op->inplace == o2);
        List *v;
        if (inplace) {
            v = (List *)val_reference(o2);
            vals = v->data;
            if (vals != v->u.val) v->u.s.hash = -1;
        } else {
            v = (List *)val_alloc(o2->obj);
            vals = lnew(v, v2->len);
            if (vals == NULL) return (Obj *)new_error_mem(op->epoint3);
        }
        for (i = 0; i < v2->len; i++) {
            Obj *val;
            Obj *oo2 = v2->data[i];
            op->v2 = oo2;
            op->v1 = o1;
            op->inplace = (inplace && oo2->refcount == 1 && !minmax) ? oo2 : NULL;
            val = o1->obj->calc2(op);
            if (minmax) {
                if (val == &true_value->v) val_replace(&val, o1);
                else if (val == &false_value->v) val_replace(&val, oo2);
            }
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return &v->v;
    }
    return val_reference(o2);
}

static void init(Type *obj) {
    obj->iterable = true;
    obj->destroy = destroy;
    obj->garbage = garbage;
    obj->same = same;
    obj->truth = truth;
    obj->hash = hash;
    obj->len = len;
    obj->getiter = getiter;
    obj->getriter = getriter;
    obj->calc1 = calc1;
    obj->calc2 = calc2;
    obj->rcalc2 = rcalc2;
    obj->slice = slice;
    obj->repr = repr_listtuple;
}

void listobj_init(void) {
    new_type(&list_obj, T_LIST, "list", sizeof(List));
    init(&list_obj);
    list_obj.create = list_create;
    new_type(&tuple_obj, T_TUPLE, "tuple", sizeof(Tuple));
    init(&tuple_obj);
    tuple_obj.create = tuple_create;
    new_type(&addrlist_obj, T_ADDRLIST, "addresslist", sizeof(Addrlist));
    addrlist_obj.destroy = destroy;
    addrlist_obj.garbage = garbage;
    addrlist_obj.same = same;
    addrlist_obj.repr = repr_listtuple;
    new_type(&colonlist_obj, T_COLONLIST, "colonlist", sizeof(Colonlist));
    colonlist_obj.destroy = destroy;
    colonlist_obj.garbage = garbage;
    colonlist_obj.same = same;
    colonlist_obj.repr = repr_listtuple;

    null_tuple = new_tuple(0);
    null_list = new_list();
    null_list->len = 0;
    null_list->data = null_list->u.val;
    null_addrlist = new_addrlist();
    null_addrlist->len = 0;
    null_addrlist->data = null_addrlist->u.val;
}

void listobj_names(void) {
    new_builtin("list", val_reference(&LIST_OBJ->v));
    new_builtin("tuple", val_reference(&TUPLE_OBJ->v));
}

void listobj_destroy(void) {
#ifdef DEBUG
    if (null_tuple->v.refcount != 1) fprintf(stderr, "tuple %" PRIuSIZE "\n", null_tuple->v.refcount - 1);
    if (null_list->v.refcount != 1) fprintf(stderr, "list %" PRIuSIZE "\n", null_list->v.refcount - 1);
    if (null_addrlist->v.refcount != 1) fprintf(stderr, "addrlist %" PRIuSIZE "\n", null_addrlist->v.refcount - 1);
#endif

    val_destroy(&null_tuple->v);
    val_destroy(&null_list->v);
    val_destroy(&null_addrlist->v);
}
