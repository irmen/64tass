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
#include "values.h"
#include "listobj.h"
#include "eval.h"
#include "boolobj.h"

static struct obj_s list_obj;
static struct obj_s tuple_obj;
static struct obj_s addrlist_obj;
static struct obj_s colonlist_obj;

obj_t LIST_OBJ = &list_obj;
obj_t TUPLE_OBJ = &tuple_obj;
obj_t ADDRLIST_OBJ = &addrlist_obj;
obj_t COLONLIST_OBJ = &colonlist_obj;

static void destroy(value_t v1) {
    size_t i;
    for (i = 0; i < v1->u.list.len; i++) {
        val_destroy(v1->u.list.data[i]);
    }
    if (v1->u.list.val != v1->u.list.data) free(v1->u.list.data);
}

static value_t *lnew(value_t v, size_t len) {
    if (len > sizeof(v->u.list.val) / sizeof(value_t)) {
        value_t *s = (value_t *)malloc(len * sizeof(value_t));
        if (!s || len > SIZE_MAX / sizeof(value_t)) err_msg_out_of_memory(); /* overflow */
        return s; 
    }
    return v->u.list.val;
}

static int same(const value_t v1, const value_t v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.list.len != v2->u.list.len) return 0;
    for (i = 0; i < v2->u.list.len; i++) {
        if (!obj_same(v1->u.list.data[i], v2->u.list.data[i])) return 0;
    }
    return 1;
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e type, linepos_t epoint) {
    size_t i;
    value_t err;
    switch (type) {
    case TRUTH_ALL:
        for (i = 0; i < v1->u.list.len; i++) {
            err = v1->u.list.data[i]->obj->truth(v1->u.list.data[i], type, epoint);
            if (err->obj != BOOL_OBJ) return err;
            if (!err->u.boolean) return err;
            val_destroy(err);
        }
        return truth_reference(1);
    case TRUTH_ANY:
        for (i = 0; i < v1->u.list.len; i++) {
            err = v1->u.list.data[i]->obj->truth(v1->u.list.data[i], type, epoint);
            if (err->obj != BOOL_OBJ) return err;
            if (err->u.boolean) return err;
            val_destroy(err);
        }
        return truth_reference(0);
    default: 
        err = val_alloc();
        err->obj = ERROR_OBJ;
        err->u.error.num = ERROR_____CANT_BOOL;
        err->u.error.epoint = *epoint;
        err->u.error.u.objname = v1->obj->name;
        return err;
    }
}

static MUST_CHECK value_t repr_listtuple(const value_t v1, linepos_t epoint) {
    size_t i, len = (v1->obj == ADDRLIST_OBJ || v1->obj == COLONLIST_OBJ) ? 0 : 2, chars = 0;
    value_t *tmp = NULL, err, v;
    uint8_t *s;
    size_t llen = v1->u.list.len;
    if (llen) {
        tmp = (value_t *)malloc(llen * sizeof(value_t));
        if (!tmp || llen > SIZE_MAX / sizeof(value_t)) err_msg_out_of_memory(); /* overflow */
        for (i = 0;i < llen; i++) {
            err = v1->u.list.data[i]->obj->repr(v1->u.list.data[i], epoint);
            if (err->obj != STR_OBJ) {
                while (i--) val_destroy(tmp[i]);
                free(tmp);
                return err;
            }
            len += err->u.str.len;
            if (len < err->u.str.len) err_msg_out_of_memory(); /* overflow */
            tmp[i] = err;
        }
        if (i && (v1->obj != TUPLE_OBJ)) i--;
        if (i) {
            len += i;
            if (len < i) err_msg_out_of_memory(); /* overflow */
        }
    }
    v = val_alloc();
    s = str_create_elements(v, len);
    len = 0;
    if (v1->obj != ADDRLIST_OBJ && v1->obj != COLONLIST_OBJ) s[len++] = (v1->obj == LIST_OBJ) ? '[' : '(';
    for (i = 0;i < llen; i++) {
        err = tmp[i];
        if (i) s[len++] = (v1->obj == COLONLIST_OBJ) ? ':' : ',';
        if (err->u.str.len) {
            memcpy(s + len, err->u.str.data, err->u.str.len);
            len += err->u.str.len;
            chars += err->u.str.len - err->u.str.chars;
        }
        val_destroy(err);
    }
    if (i == 1 && (v1->obj == TUPLE_OBJ)) s[len++] = ',';
    if (v1->obj != ADDRLIST_OBJ && v1->obj != COLONLIST_OBJ) s[len++] = (v1->obj == LIST_OBJ) ? ']' : ')';
    free(tmp);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
    return v;
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.list.len);
}

static MUST_CHECK value_t getiter(value_t v1) {
    value_t v = val_alloc();
    v->obj = ITER_OBJ;
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
    return v;
}

static MUST_CHECK value_t next(value_t v1) {
    const value_t vv1 = v1->u.iter.data;
    if (v1->u.iter.val >= vv1->u.list.len) return NULL;
    return val_reference(vv1->u.list.data[v1->u.iter.val++]);
}

value_t *list_create_elements(value_t v, size_t n) {
    return lnew(v, n);
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    if (v1->u.list.len) {
        value_t *vals;
        int error = 1;
        size_t i = 0;
        v = val_alloc();
        v->obj = v1->obj;
        vals = lnew(v, v1->u.list.len);
        for (;i < v1->u.list.len; i++) {
            value_t val;
            op->v1 = v1->u.list.data[i];
            val = op->v1->obj->calc1(op);
            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
            vals[i] = val;
        }
        op->v1 = v1;
        v->u.list.len = i;
        v->u.list.data = vals;
        return v;
    }
    return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
}

static MUST_CHECK value_t calc2_list(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    size_t i;

    switch (op->op->u.oper.op) {
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
            value_t *vals;
            value_t vx;
            int d1 = 0, d2 = 0;
            i = 0;
            vx = v1;
            while (vx->obj == LIST_OBJ || vx->obj == TUPLE_OBJ) {
                d1++;
                if (!vx->u.list.len) break;
                vx = vx->u.list.data[0];
            }
            vx = v2;
            while (vx->obj == LIST_OBJ || vx->obj == TUPLE_OBJ) {
                d2++;
                if (!vx->u.list.len) break;
                vx = vx->u.list.data[0];
            }
            if (d1 == d2) {
                if (v1->u.list.len == 1) {
                    if (v2->u.list.len) {
                        int error = 1;
                        op->v1 = v1->u.list.data[0];
                        v = val_alloc();
                        vals = lnew(v, v2->u.list.len);
                        for (; i < v2->u.list.len; i++) {
                            value_t val;
                            op->v2 = v2->u.list.data[i];
                            val = op->v1->obj->calc2(op);
                            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                            vals[i] = val;
                        }
                        op->v1 = v1;
                        op->v2 = v2;
                    } else {
                         return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
                    }
                } else if (v2->u.list.len == 1) {
                    if (v1->u.list.len) {
                        int error = 1;
                        op->v2 = v2->u.list.data[0];
                        v = val_alloc();
                        vals = lnew(v, v1->u.list.len);
                        for (; i < v1->u.list.len; i++) {
                            value_t val;
                            op->v1 = v1->u.list.data[i];
                            val = op->v1->obj->calc2(op);
                            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                            vals[i] = val;
                        }
                        op->v1 = v1;
                        op->v2 = v2;
                    } else {
                         return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
                    }
                } else if (v1->u.list.len == v2->u.list.len) {
                    if (v1->u.list.len) {
                        int error = 1;
                        v = val_alloc();
                        vals = lnew(v, v1->u.list.len);
                        for (; i < v1->u.list.len; i++) {
                            value_t val;
                            op->v1 = v1->u.list.data[i];
                            op->v2 = v2->u.list.data[i];
                            val = op->v1->obj->calc2(op);
                            if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                            vals[i] = val;
                        }
                        op->v1 = v1;
                        op->v2 = v2;
                    } else {
                         return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
                    }
                } else {
                    return obj_oper_error(op);
                }
            } else if (d1 > d2) {
                if (v1->u.list.len) {
                    int error = 1;
                    v = val_alloc();
                    vals = lnew(v, v1->u.list.len);
                    for (; i < v1->u.list.len; i++) {
                        value_t val;
                        op->v1 = v1->u.list.data[i];
                        val = op->v1->obj->calc2(op);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                        vals[i] = val;
                    }
                    op->v1 = v1;
                } else {
                     return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
                }
            } else if (v2->u.list.len) {
                int error = 1;
                v = val_alloc();
                vals = lnew(v, v2->u.list.len);
                for (; i < v2->u.list.len; i++) {
                    value_t val;
                    op->v2 = v2->u.list.data[i];
                    val = v1->obj->calc2(op);
                    if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                    vals[i] = val;
                }
                op->v2 = v2;
            } else {
                return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
            }
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return v;
        }
    case O_CONCAT:
        {
            value_t *vals;
            size_t ln;

            if (!v1->u.list.len) {
                return val_reference(v2);
            }
            if (!v2->u.list.len) {
                return val_reference(v1);
            }
            ln = v1->u.list.len + v2->u.list.len;
            if (ln < v2->u.list.len) err_msg_out_of_memory(); /* overflow */
            v = val_alloc();
            v->obj = v1->obj;
            vals = lnew(v, ln);
            for (i = 0; i < v1->u.list.len; i++) {
                vals[i] = val_reference(v1->u.list.data[i]);
            }
            for (; i < ln; i++) {
                vals[i] = val_reference(v2->u.list.data[i - v1->u.list.len]);
            }
            v->u.list.len = ln;
            v->u.list.data = vals;
            return v;
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    size_t i = 0;
    value_t *vals;

    switch (v2->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (v1->obj == v2->obj) {
            return calc2_list(op);
        }
    default:
        if (v1->u.list.len) {
            int error = 1;
            v = val_alloc();
            v->obj = v1->obj;
            v->u.list.data = vals = lnew(v, v1->u.list.len);
            for (;i < v1->u.list.len; i++) {
                value_t val;
                op->v1 = v1->u.list.data[i];
                val = op->v1->obj->calc2(op);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                vals[i] = val;
            }
            op->v1 = v1;
            v->u.list.len = i;
            return v;
        }
    }
    return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    size_t i = 0;
    value_t *vals;
    switch (v1->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (v1->obj == v2->obj) {
            return calc2_list(op);
        }
    default:
        switch (op->op->u.oper.op) {
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
        case O_CONCAT:
            if (v2->u.list.len) {
                int error = 1;
                v = val_alloc();
                v->obj = v2->obj;
                v->u.list.data = vals = lnew(v, v2->u.list.len);
                for (;i < v2->u.list.len; i++) {
                    value_t val;
                    op->v2 = v2->u.list.data[i];
                    val = op->v2->obj->rcalc2(op);
                    if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                    vals[i] = val;
                }
                op->v2 = v2;
                v->u.list.len = i;
                return v;
            }
            break;
        case O_IN:
            op->op = &o_EQ;
            for (;i < v2->u.list.len; i++) {
                value_t result;
                op->v2 = v2->u.list.data[i];
                result = v1->obj->calc2(op);
                if (result->obj == BOOL_OBJ && result->u.boolean) {
                    op->op = &o_IN;
                    op->v2 = v2;
                    return result;
                }
                val_destroy(result);
            }
            op->op = &o_IN;
            op->v2 = v2;
            return truth_reference(0);
        default: 
            return v1->obj->calc2(op);
        }
        break;
    }
    return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
}

static MUST_CHECK value_t repeat(oper_t op, uval_t rep) {
    value_t *vals;
    value_t v1 = op->v1, v;

    if (v1->u.list.len && rep) {
        size_t i = 0, j, ln;
        if (rep == 1) {
            return val_reference(v1);
        }
        ln = v1->u.list.len * rep;
        if (v1->u.list.len > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = val_alloc();
        v->obj = v1->obj;
        v->u.list.data = vals = lnew(v, ln);
        while (rep--) {
            for (j = 0;j < v1->u.list.len; j++, i++) {
                vals[i] = val_reference(v1->u.list.data[j]);
            }
        }
        v->u.list.len = i;
        return v;
    }
    return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
}

static inline MUST_CHECK value_t slice(value_t v1, uval_t ln, ival_t offs, ival_t end, ival_t step) {
    value_t *vals, v;
    size_t i;

    if (!ln) {
        return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
    }

    if (step == 1 && ln == v1->u.list.len) {
        return val_reference(v1); /* original tuple */
    }
    v = val_alloc();
    v->obj = v1->obj;
    v->u.list.data = vals = lnew(v, ln);
    i = 0;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        vals[i++] = val_reference(v1->u.list.data[offs]);
        offs += step;
    }
    v->u.list.len = i;
    return v;
}

static MUST_CHECK value_t iindex(oper_t op) {
    value_t *vals;
    size_t i, ln;
    ival_t offs;
    value_t v1 = op->v1, v2 = op->v2, v, err;

    ln = v1->u.list.len;

    if (v2->obj == LIST_OBJ) {
        int error = 1;
        if (!v2->u.list.len) {
            return val_reference((v1->obj == TUPLE_OBJ) ? null_tuple : null_list);
        }
        v = val_alloc();
        v->obj = v1->obj;
        v->u.list.data = vals = lnew(v, v2->u.list.len);
        for (i = 0; i < v2->u.list.len; i++) {
            err = indexoffs(v2->u.list.data[i], &offs, ln, op->epoint2);
            if (err) {
                if (error) {err_msg_output(err); error = 0;} 
                val_destroy(err);
                vals[i] = val_reference(none_value);
                continue;
            }
            vals[i] = val_reference(v1->u.list.data[offs]);
        }
        v->u.list.len = i;
        return v;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t length, end, step;
        err = sliceparams(op, ln, &length, &offs, &end, &step);
        if (err) return err;
        return slice(v1, length, offs, end, step);
    }
    err = indexoffs(v2, &offs, ln, op->epoint2);
    if (err) return err;
    return val_reference(v1->u.list.data[offs]);
}

static void init(struct obj_s *obj) {
    obj->destroy = destroy;
    obj->same = same;
    obj->truth = truth;
    obj->len = len;
    obj->getiter = getiter;
    obj->next = next;
    obj->calc1 = calc1;
    obj->calc2 = calc2;
    obj->rcalc2 = rcalc2;
    obj->repeat = repeat;
    obj->iindex = iindex;
    obj->repr = repr_listtuple;
}

void listobj_init(void) {
    obj_init(&list_obj, T_LIST, "<list>");
    init(&list_obj);
    obj_init(&tuple_obj, T_TUPLE, "<tuple>");
    init(&tuple_obj);
    obj_init(&addrlist_obj, T_ADDRLIST, "<address list>");
    addrlist_obj.destroy = destroy;
    addrlist_obj.same = same;
    addrlist_obj.repr = repr_listtuple;
    obj_init(&colonlist_obj, T_COLONLIST, "<colon list>");
    colonlist_obj.destroy = destroy;
    colonlist_obj.same = same;
    colonlist_obj.repr = repr_listtuple;
}
