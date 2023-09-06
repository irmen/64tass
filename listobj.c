/*
    $Id: listobj.c 3086 2023-09-03 06:23:08Z soci $

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

#include "boolobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "intobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "foldobj.h"

static Type list_obj;
static Type tuple_obj;
static Type colonlist_obj;

Type *const LIST_OBJ = &list_obj;
Type *const TUPLE_OBJ = &tuple_obj;
Type *const COLONLIST_OBJ = &colonlist_obj;

static Tuple null_tupleval = { { &tuple_obj, 1 }, 0, null_tupleval.u.val, { { 0 } } };
static List null_listval = { { &list_obj, 1 }, 0, null_listval.u.val, { { 0 } } };

Obj *const null_tuple = &null_tupleval.v;
Obj *const null_list = &null_listval.v;

static inline List *ref_list(List *v1) {
    v1->v.refcount++; return v1;
}

static FAST_CALL NO_INLINE void list_destroy(List *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    List *v1 = List(o1);
    size_t i;
    for (i = 0; i < v1->len; i++) {
        val_destroy(v1->data[i]);
    }
    if (v1->u.val != v1->data) list_destroy(v1);
}

static FAST_CALL void garbage(Obj *o1, int j) {
    List *v1 = List(o1);
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
    Obj **n;
    v->len = len;
    if (len <= lenof(v->u.val)) {
        v->data = v->u.val;
        return v->u.val;
    }
    n = allocate_array(Obj *, len);
    if (n != NULL) {
        v->data = n;
        v->u.s.max = len;
        v->u.s.hash = -1;
        return n;
    }
    v->len = 0;
    v->data = v->u.val;
    val_destroy(Obj(v));
    return NULL;
}

static Obj **lextend(List *v, size_t len) {
    Obj **tmp;
    if (len <= lenof(v->u.val)) {
        return v->u.val;
    }
    if (v->u.val != v->data) {
        if (len <= v->u.s.max) {
            v->u.s.hash = -1;
            return v->data;
        }
        tmp = reallocate_array(v->data, len);
        if (tmp != NULL) {
            v->data = tmp;
            v->u.s.max = len;
            v->u.s.hash = -1;
        }
        return tmp;
    }
    tmp = allocate_array(Obj *, len);
    if (tmp != NULL) {
        memcpy(tmp, v->u.val, v->len * sizeof *v->data);
        v->data = tmp;
        v->u.s.max = len;
        v->u.s.hash = -1;
    }
    return tmp;
}

static MUST_CHECK Obj *tuple_from_iterable(Obj *v1, Type *typ, linepos_t epoint) {
    Obj *v;
    struct iter_s iter;
    iter.data = v1; v1->obj->getiter(&iter);

    if (iter.len == 0) {
        v = val_reference((typ == TUPLE_OBJ) ? null_tuple : null_list);
    } else {
        Obj **vals, *o2;
        v = val_alloc(typ);
        vals = lnew(List(v), iter.len);
        if (vals == NULL) {
            v = new_error_mem(epoint);
        } else {
            size_t i;
            for (i = 0; i < iter.len && (o2 = iter.next(&iter)) != NULL; i++) {
                vals[i] = val_reference(o2);
            }
            List(v)->len = i;
        }
    }
    iter_destroy(&iter);
    return v;
}

static MUST_CHECK Obj *tuple_from_list(List *v1, Type *typ, linepos_t epoint) {
    size_t i, ln;
    Obj **vals, **data = v1->data;
    Tuple *v = Tuple(val_alloc(typ));

    ln = v1->len;
    vals = lnew(v, ln);
    if (vals == NULL) return new_error_mem(epoint);

    for (i = 0; i < ln; i++) {
        vals[i] = val_reference(data[i]);
    }
    return Obj(v);
}

static MUST_CHECK Obj *list_from_obj(Obj *o1, Type *typ, linepos_t epoint) {
    if (o1->obj == typ) {
        return val_reference(o1);
    }
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
        return val_reference(o1);
    case T_LIST:
    case T_TUPLE:
        return tuple_from_list(List(o1), typ, epoint);
    case T_CODE:
        return tuple_from_code(Code(o1), typ);
    default:
        if (o1->obj->iterable) {
            return tuple_from_iterable(o1, typ, epoint);
        }
        break;
    }
    return new_error_conv(o1, typ, epoint);
}

static MUST_CHECK Obj *list_convert(oper_t op) {
    return list_from_obj(op->v2, LIST_OBJ, op->epoint2);
}

static MUST_CHECK Obj *tuple_convert(oper_t op) {
    return list_from_obj(op->v2, TUPLE_OBJ, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const List *v1 = List(o1), *v2 = List(o2);
    size_t i;
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    for (i = 0; i < v2->len; i++) {
        Obj *val = v1->data[i];
        if (!val->obj->same(val, v2->data[i])) return false;
    }
    return true;
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t epoint) {
    List *v1 = List(o1);
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
        Obj *err = o2->obj->hash(o2, &h2, epoint);
        if (err != NULL) return err;
        h += (unsigned int)h2;
    }
    h ^= (unsigned int)i;
    h &= ((~0U) >> 1);
    if (vals != v1->u.val) v1->u.s.hash = (int)h;
    *hs = (int)h;
    return NULL;
}

static MUST_CHECK Obj *repr_listtuple(Obj *o1, linepos_t epoint, size_t maxsize, bool tupleorlist) {
    Tuple *v1 = List(o1);
    size_t i, len = tupleorlist ? 2 : 0, chars = len;
    Tuple *list = NULL;
    Obj **vals = NULL, *val;
    Str *v;
    uint8_t *s;
    size_t llen = v1->len;
    if (llen != 0) {
        i = (llen != 1 || o1->obj != TUPLE_OBJ) ? (llen - 1) : llen;
        if (inc_overflow(&len, i)) return NULL;
        chars = len;
        if (chars > maxsize) return NULL;
        list = Tuple(val_alloc(TUPLE_OBJ));
        vals = lnew(list, llen);
        if (vals == NULL) return (epoint != NULL) ? new_error_mem(epoint) : NULL;
        for (i = 0;i < llen; i++) {
            Obj *o2 = v1->data[i];
            if (o2 == default_value && o1->obj == COLONLIST_OBJ) {
                val = val_reference(null_str);
            } else {
                val = o2->obj->repr(o2, epoint, maxsize - chars);
                if (val == NULL || val->obj != STR_OBJ) goto error;
            }
            v = Str(val);
            if (inc_overflow(&len, v->len)) goto error2;
            chars += v->chars;
            if (chars > maxsize) {
            error2:
                val_destroy(val);
                val = NULL;
            error:
                list->len = i;
                val_destroy(Obj(list));
                return val;
            }
            vals[i] = val;
        }
        list->len = i;
    } else if (chars > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) {
        if (list != NULL) val_destroy(Obj(list));
        return NULL;
    }
    v->chars = chars;
    s = v->data;
    if (tupleorlist) *s++ = (o1->obj == LIST_OBJ) ? '[' : '(';
    for (i = 0; i < llen; i++) {
        Str *str = Str(vals[i]);
        if (i != 0) *s++ = (o1->obj == COLONLIST_OBJ) ? ':' : ',';
        if (str->len != 0) {
            memcpy(s, str->data, str->len);
            s += str->len;
        }
    }
    if (i == 1 && o1->obj == TUPLE_OBJ) *s++ = ',';
    if (tupleorlist) *s = (o1->obj == LIST_OBJ) ? ']' : ')';
    if (list != NULL) val_destroy(Obj(list));
    return Obj(v);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    return repr_listtuple(o1, epoint, maxsize, true);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t epoint, size_t maxsize) {
    return repr_listtuple(o1, epoint, maxsize, o1->obj != COLONLIST_OBJ);
}

static MUST_CHECK Obj *len(oper_t op) {
    List *v1 = List(op->v2);
    return int_from_size(v1->len);
}

static FAST_CALL MUST_CHECK Obj *iter_forward(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return List(v1->data)->data[v1->val++];
}

static void getiter(struct iter_s *v) {
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_forward;
    v->len = List(v->data)->len;
}

static FAST_CALL MUST_CHECK Obj *iter_reverse(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return List(v1->data)->data[v1->len - ++v1->val];
}

static void getriter(struct iter_s *v) {
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_reverse;
    v->len = List(v->data)->len;
}

Obj **list_create_elements(List *v, size_t n) {
    Obj **d;
    if (n <= lenof(v->u.val)) return v->u.val;
    v->u.s.max = n;
    v->u.s.hash = -1;
    new_array(&d, n);
    return d;
}

MUST_CHECK bool list_extend(List *lst) {
    Obj **vals;
    size_t o = lst->len, n;
    if (lst->data == lst->u.val) {
        n = 16;
        vals = allocate_array(Obj *, 16);
        if (vals == NULL) return true;
        memcpy(vals, lst->u.val, o * sizeof *lst->data);
    } else {
        if (o < 256) n = o * 2;
        else {
            if (add_overflow(o, 256, &n)) return true;
        }
        vals = reallocate_array(lst->data, n);
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
            Obj **v = reallocate_array(lst->data, lst->len);
            if (v != NULL) {
                lst->data = v;
                lst->u.s.max = lst->len;
                lst->u.s.hash = -1;
            }
        }
    }
}

MUST_CHECK Tuple *new_tuple(size_t n) {
     Tuple *v = Tuple(val_alloc(TUPLE_OBJ));
     v->len = n;
     if (n <= lenof(v->u.val)) {
         v->data = v->u.val;
         return v;
     }
     v->u.s.max = n;
     v->u.s.hash = -1;
     new_array(&v->data, n);
     return v;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *o1 = op->v1;
    List *v1 = List(o1), *v;
    if (v1->len != 0) {
        Obj **vals;
        bool inplace = (op->inplace == o1);
        size_t i;
        if (inplace) {
            v = ref_list(v1);
            vals = v1->data;
            if (vals != v->u.val) v->u.s.hash = -1;
        } else {
            v = List(val_alloc(o1->obj));
            vals = lnew(v, v1->len);
            if (vals == NULL) return new_error_mem(op->epoint3);
        }
        for (i = 0; i < v1->len; i++) {
            Obj *val = v1->data[i];
            op->v1 = val;
            op->inplace = (inplace && val->refcount == 1) ? val : NULL;
            val = op->v1->obj->calc1(op);
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return Obj(v);
    }
    return val_reference(o1);
}

static MUST_CHECK Obj *calc2_list(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = List(o1), *v2 = List(o2);
    size_t i;
    Error *err;

    switch (op->op) {
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
    case O_LXOR:
        {
            if (v1->len == v2->len) {
                if (v1->len != 0) {
                    Obj **vals;
                    bool minmax = (op->op == O_MIN) || (op->op == O_MAX);
                    List *v, *inplace;
                    if (op->inplace == Obj(v1)) {
                        v = ref_list(v1);
                        vals = v1->data;
                        inplace = v1;
                        if (vals != v->u.val) v->u.s.hash = -1;
                    } else if (o1->obj == o2->obj && op->inplace == Obj(v2)) {
                        v = ref_list(v2);
                        vals = v2->data;
                        inplace = v2;
                        if (vals != v->u.val) v->u.s.hash = -1;
                    } else {
                        v = List(val_alloc(o1->obj));
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
                            if (val == true_value) val_replace(&val, oo1);
                            else if (val == false_value) val_replace(&val, oo2);
                        }
                        if (inplace != NULL) val_destroy(vals[i]);
                        vals[i] = val;
                    }
                    return Obj(v);
                }
                return val_reference(Obj(v1));
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
            return Obj(err);
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
            if (add_overflow(v1->len, v2->len, &ln)) goto failed;
            if (op->inplace == Obj(v1)) {
                size_t ln2;
                if (ln > sizeof v1->u.val && v1->u.val != v1->data && ln > v1->u.s.max) {
                    ln2 = ln + (ln < 1024 ? ln : 1024);
                    if (ln2 < ln) ln2 = ln;
                } else ln2 = ln;
                vals = lextend(v1, ln2);
                if (vals == NULL) goto failed;
                i = v1->len;
                v1->len = ln;
                v = ref_list(List(o1));
            } else if (o1->obj == o2->obj && op->inplace == Obj(v2)) {
                size_t ln2;
                if (ln > sizeof v2->u.val && v2->u.val != v2->data && ln > v2->u.s.max) {
                    ln2 = ln + (ln < 1024 ? ln : 1024);
                    if (ln2 < ln) ln2 = ln;
                } else ln2 = ln;
                vals = lextend(v2, ln2);
                if (vals == NULL) goto failed;
                memmove(vals + v1->len, v2->data, v2->len * sizeof *v2->data);
                v2->len = ln;
                for (i = 0; i < v1->len; i++) {
                    vals[i] = val_reference(v1->data[i]);
                }
                return val_reference(o2);
            } else {
                v = List(val_alloc(o1->obj));
                vals = lnew(v, ln);
                if (vals == NULL) goto failed;
                for (i = 0; i < v1->len; i++) {
                    vals[i] = val_reference(v1->data[i]);
                }
            }
            for (j = 0; i < ln; i++) {
                vals[i] = val_reference(v2->data[j++]);
            }
            return Obj(v);
        }
    default: break;
    }
    return obj_oper_error(op);
failed:
    return new_error_mem(op->epoint3);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Obj **vals, *o1 = op->v1;
    List *v1 = List(o1), *v;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return Obj(err);

    if (v1->len == 0 || rep == 0) return val_reference((o1->obj == TUPLE_OBJ) ? null_tuple : null_list);
    do {
        size_t i, sz, ln;
        if (rep == 1) {
            return val_reference(o1);
        }
        ln = v1->len;
        if (ln > SIZE_MAX / rep) break; /* overflow */
        sz = ln * rep;
        if (op->inplace == Obj(v1)) {
            vals = lextend(v1, sz);
            if (vals == NULL) break;
            v = ref_list(v1);
            v->len = sz;
            rep--;
        } else {
            v = List(val_alloc(o1->obj));
            vals = lnew(v, sz);
            if (vals == NULL) break;
        }
        if (ln == 1) {
            for (i = 0; i < sz; i++) {
                vals[i] = v1->data[0];
            }
            vals[0]->refcount += rep;
        } else {
            for (i = 0; i < ln; i++) {
                vals[i] = v1->data[i];
                vals[i]->refcount += rep;
            }
            while (sz > ln) {
                size_t oln = ln;
                if (ln > sz - ln) ln = sz - ln;
                memcpy(vals + oln, vals, ln * sizeof *vals);
                ln += oln;
            }
        }
        return Obj(v);
    } while (false);
    return new_error_mem(op->epoint3);
}

MUST_CHECK Obj *indexoffs(struct indexoffs_s *io) {
    ival_t ival;
    Obj *err = Obj(io->val->obj->ival(io->val, &ival, 8 * sizeof ival, io->epoint));
    if (err != NULL) return err;

    if (ival >= 0) {
        if ((uval_t)ival < io->len) {
            io->offs = (uval_t)ival;
            return NULL;
        }
    } else {
        ival = -ival;
        if ((uval_t)ival <= io->len) {
            io->offs = io->len - (uval_t)ival;
            return NULL;
        }
    }
    return new_error_obj(ERROR___INDEX_RANGE, io->val, io->epoint);
}

MUST_CHECK Obj *sliceparams(struct sliceparam_s *s, const struct indexoffs_s *io) {
    const Colonlist *v2 = Colonlist(io->val);
    Obj *err;
    ival_t length, offs, end, step = 1;

    if (io->len >= (1U << (8 * sizeof(ival_t) - 1))) return new_error_mem(io->epoint); /* overflow */
    length = (ival_t)io->len;
    if (v2->len > 3 || v2->len < 1) {
        return new_error_argnum(v2->len <= ~(argcount_t)0 ? (argcount_t)v2->len : ~(argcount_t)0, 1, 3, io->epoint);
    }
    end = length;
    if (v2->len > 2) {
        if (v2->data[2] != default_value) {
            err = Obj(v2->data[2]->obj->ival(v2->data[2], &step, 8 * sizeof step, io->epoint));
            if (err != NULL) return err;
            if (step == 0) {
                return Obj(new_error(ERROR_NO_ZERO_VALUE, io->epoint));
            }
        }
    }
    if (v2->len > 1) {
        if (v2->data[1] == default_value) end = (step > 0) ? length : -1;
        else {
            err = Obj(v2->data[1]->obj->ival(v2->data[1], &end, 8 * sizeof end, io->epoint));
            if (err != NULL) return err;
            if (end >= 0) {
                if (end > length) end = length;
            } else {
                end += length;
                if (end < -1) end = -1;
            }
        }
    } else end = length;
    if (v2->data[0] == default_value) offs = (step > 0) ? 0 : length - 1;
    else {
        ival_t minus;
        err = Obj(v2->data[0]->obj->ival(v2->data[0], &offs, 8 * sizeof offs, io->epoint));
        if (err != NULL) return err;
        minus = (step < 0) ? -1 : 0;
        if (offs >= 0) {
            if (offs > length + minus) offs = length + minus;
        } else {
            offs += length;
        }
        if (offs < minus) offs = minus;
    }

    if (step > 0) {
        if (offs > end) offs = end;
        s->length = (uval_t)(end - offs + step - 1) / (uval_t)step;
    } else {
        if (end > offs) end = offs;
        s->length = (uval_t)(offs - end - step - 1) / -(uval_t)step;
    }

    s->offset = offs;
    s->end = end;
    s->step = step;
    return NULL;
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    Obj **vals;
    List *v, *v1 = List(op->v1);
    Funcargs *args = Funcargs(op->v2);
    size_t i;
    Obj *err;
    bool more;
    struct indexoffs_s io;

    if (args->len < 1) {
        return new_error_argnum(args->len, 1, 0, op->epoint2);
    }
    more = args->len - 1 > indx;
    io.len = v1->len;
    io.epoint = &args->val[indx].epoint;
    io.val = args->val[indx].val;

    if (io.val->obj->iterable) {
        struct iter_s iter;
        iter.data = io.val; io.val->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference((v1->v.obj == TUPLE_OBJ) ? null_tuple : null_list);
        }
        v = List(val_alloc(v1->v.obj));
        vals = lnew(v, iter.len);
        if (vals == NULL) {
            iter_destroy(&iter);
            goto failed;
        }
        for (i = 0; i < iter.len && (io.val = iter.next(&iter)) != NULL; i++) {
            err = indexoffs(&io);
            if (err != NULL) {
                vals[i] = err;
                continue;
            }
            if (more) {
                op->v1 = v1->data[io.offs];
                vals[i] = op->v1->obj->slice(op, indx + 1);
            } else {
                vals[i] = val_reference(v1->data[io.offs]);
            }
        }
        v->len = i;
        iter_destroy(&iter);
        return Obj(v);
    }
    if (io.val->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;

        err = sliceparams(&s, &io);
        if (err != NULL) return err;

        if (s.length == 0) {
            return val_reference((v1->v.obj == TUPLE_OBJ) ? null_tuple : null_list);
        }

        if (s.step > 0 && op->inplace == Obj(v1)) {
            if (s.offset != 0 || s.step != 1 || more) {
                vals = v1->data;
                for (i = 0; i < s.length; i++) {
                    Obj *val;
                    if (more) {
                        op->v1 = vals[s.offset];
                        val = op->v1->obj->slice(op, indx + 1);
                        val_destroy(vals[i]);
                    } else {
                        val = vals[s.offset];
                        vals[s.offset] = vals[i];
                    }
                    vals[i] = val;
                    s.offset += s.step;
                }
            }
            if (v1->len != s.length) list_shrink(v1, s.length);
            v = ref_list(v1);
        } else {
            if (s.step == 1 && s.length == v1->len && !more) {
                return val_reference(Obj(v1)); /* original tuple */
            }
            v = List(val_alloc(v1->v.obj));
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
        }
        return Obj(v);
    }
    err = indexoffs(&io);
    if (err != NULL) return err;
    if (more) {
        op->v1 = v1->data[io.offs];
        return op->v1->obj->slice(op, indx + 1);
    }
    return val_reference(v1->data[io.offs]);
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v2 = List(o2);
    Oper_types oper = op->op;
    Obj *good = (oper == O_IN) ? false_value : true_value;
    Obj *bad;
    Obj *result2;
    size_t i;
    if (v2->len == 0) return val_reference(good);
    if (o1 == none_value || o1->obj == ERROR_OBJ) return val_reference(o1);
    bad = (oper == O_IN) ? true_value : false_value;
    op->op = (oper == O_IN) ? O_EQ : O_NE;
    result2 = val_reference(good);
    for (i = 0; i < v2->len; i++) {
        Obj *result;
        op->v1 = o1;
        op->v2 = v2->data[i];
        op->inplace = NULL;
        result = o1->obj->calc2(op);
        if (result == good) {
            val_destroy(result);
            continue;
        }
        val_destroy(result2);
        result2 = result;
        if (result == bad) {
            break;
        }
    }
    op->op = oper;
    return result2;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v1 = List(o1);
    size_t i;
    Obj **vals;

    if (op->op == O_X) {
        if (o2 == none_value) return val_reference(o2);
        return repeat(op);
    }
    if (o2 == fold_value) {
        return o2->obj->rcalc2(op);
    }
    if (o2->obj == TUPLE_OBJ || o2->obj == LIST_OBJ) {
        return calc2_list(op);
    }
    if (o2 == none_value || o2->obj == ERROR_OBJ) {
        return val_reference(o2);
    }
    if (v1->len != 0) {
        bool minmax = (op->op == O_MIN) || (op->op == O_MAX), inplace = (op->inplace == o1);
        List *list;
        if (inplace) {
            list = ref_list(List(o1));
            vals = list->data;
            if (vals != list->u.val) list->u.s.hash = -1;
        } else {
            list = List(val_alloc(o1->obj));
            vals = lnew(list, v1->len);
            if (vals == NULL) return new_error_mem(op->epoint3);
        }
        for (i = 0; i < v1->len; i++) {
            Obj *val;
            Obj *oo1 = v1->data[i];
            op->v1 = oo1;
            op->v2 = o2;
            op->inplace = (inplace && oo1->refcount == 1 && !minmax) ? oo1 : NULL;
            val = op->v1->obj->calc2(op);
            if (minmax) {
                if (val == true_value) val_replace(&val, oo1);
                else if (val == false_value) val_replace(&val, o2);
            }
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return Obj(list);
    }
    return val_reference(o1);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1, *o2 = op->v2;
    List *v2 = List(o2);
    size_t i;
    Obj **vals;

    if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ) {
        return calc2_list(op);
    }
    if (o1 == none_value || o1->obj == ERROR_OBJ || o1 == fold_value) {
        return o1->obj->calc2(op);
    }
    if (v2->len != 0) {
        bool minmax = (op->op == O_MIN) || (op->op == O_MAX), inplace = (op->inplace == o2);
        List *v;
        if (inplace) {
            v = ref_list(List(o2));
            vals = v->data;
            if (vals != v->u.val) v->u.s.hash = -1;
        } else {
            v = List(val_alloc(o2->obj));
            vals = lnew(v, v2->len);
            if (vals == NULL) return new_error_mem(op->epoint3);
        }
        for (i = 0; i < v2->len; i++) {
            Obj *val;
            Obj *oo2 = v2->data[i];
            op->v2 = oo2;
            op->v1 = o1;
            op->inplace = (inplace && oo2->refcount == 1 && !minmax) ? oo2 : NULL;
            val = o1->obj->calc2(op);
            if (minmax) {
                if (val == true_value) val_replace(&val, o1);
                else if (val == false_value) val_replace(&val, oo2);
            }
            if (inplace) val_destroy(vals[i]);
            vals[i] = val;
        }
        return Obj(v);
    }
    return val_reference(o2);
}

static void init(Type *obj) {
    obj->iterable = true;
    obj->destroy = destroy;
    obj->garbage = garbage;
    obj->same = same;
    obj->hash = hash;
    obj->len = len;
    obj->getiter = getiter;
    obj->getriter = getriter;
    obj->calc1 = calc1;
    obj->calc2 = calc2;
    obj->rcalc2 = rcalc2;
    obj->slice = slice;
    obj->contains = contains;
    obj->repr = repr;
}

void listobj_init(void) {
    Type *type = new_type(&list_obj, T_LIST, "list", sizeof(List));
    init(type);
    type->convert = list_convert;

    type = new_type(&tuple_obj, T_TUPLE, "tuple", sizeof(Tuple));
    init(type);
    type->convert = tuple_convert;

    type = new_type(&colonlist_obj, T_COLONLIST, "colonlist", sizeof(Colonlist));
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
}

void listobj_names(void) {
    new_builtin("list", val_reference(Obj(LIST_OBJ)));
    new_builtin("tuple", val_reference(Obj(TUPLE_OBJ)));
}

void listobj_destroy(void) {
#ifdef DEBUG
    if (null_tuple->refcount != 1) fprintf(stderr, "tuple %" PRIuSIZE "\n", null_tuple->refcount - 1);
    if (null_list->refcount != 1) fprintf(stderr, "list %" PRIuSIZE "\n", null_list->refcount - 1);
#endif
}
