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
#include "isnprintf.h"
#include "boolobj.h"

static struct obj_s list_obj;
static struct obj_s tuple_obj;
static struct obj_s addrlist_obj;
static struct obj_s colonlist_obj;

obj_t LIST_OBJ = &list_obj;
obj_t TUPLE_OBJ = &tuple_obj;
obj_t ADDRLIST_OBJ = &addrlist_obj;
obj_t COLONLIST_OBJ = &colonlist_obj;

static void destroy(struct value_s *v1) {
    size_t i;
    for (i = 0; i < v1->u.list.len; i++) {
        val_destroy(v1->u.list.data[i]);
    }
    if (v1->u.list.val != v1->u.list.data) free(v1->u.list.data);
}

static struct value_s **lnew(struct value_s *v, size_t len) {
    if (len > sizeof(v->u.list.val) / sizeof(struct value_s *)) {
        struct value_s **s = (struct value_s **)malloc(len * sizeof(struct value_s *));
        if (!s || len > SIZE_MAX / sizeof(struct value_s *)) err_msg_out_of_memory(); /* overflow */
        return s; 
    }
    return v->u.list.val;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    struct value_s **vals;
    size_t len = v1->u.list.len;
    size_t i;
    vals = lnew(v, len);
    for (i = 0; i < len; i++) {
        vals[i] = val_reference(v1->u.list.data[i]);
    }
    v->obj = v1->obj;
    v->u.list.data = vals;
    v->u.list.len = len;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->u.list.len = v1->u.list.len;
    if (v1->u.list.data == v1->u.list.val) {
        v->u.list.data = v->u.list.val;
        memcpy(v->u.list.data, v1->u.list.data, v->u.list.len * sizeof(struct value_s *));
    } else v->u.list.data = v1->u.list.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.list.len != v2->u.list.len) return 0;
    for (i = 0; i < v2->u.list.len; i++) {
        if (!obj_same(v1->u.list.data[i], v2->u.list.data[i])) return 0;
    }
    return 1;
}

static int truth(const struct value_s *v1, struct value_s *v, enum truth_e type, linepos_t epoint) {
    size_t i;
    struct value_s err;
    switch (type) {
    case TRUTH_ALL:
        err.u.boolean = 1;
        for (i = 0; i < v1->u.list.len; i++) {
            if (v1->u.list.data[i]->obj->truth(v1->u.list.data[i], &err, type, epoint)) {
                if (v1 == v) destroy(v);
                err.obj->copy_temp(&err, v);
                return 1;
            }
            if (!err.u.boolean) break;
        }
        break;
    case TRUTH_ANY:
        err.u.boolean = 0;
        for (i = 0; i < v1->u.list.len; i++) {
            if (v1->u.list.data[i]->obj->truth(v1->u.list.data[i], &err, type, epoint)) {
                if (v1 == v) destroy(v);
                err.obj->copy_temp(&err, v);
                return 1;
            }
            if (err.u.boolean) break;
        }
        break;
    default: 
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_BOOL;
        v->u.error.epoint = *epoint;
        v->u.error.u.objname = v1->obj->name;
        return 1;
    }
    if (v1 == v) destroy(v);
    bool_from_int(v, err.u.boolean);
    return 0;
}

static void repr_listtuple(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    size_t i, len = (v1->obj == ADDRLIST_OBJ || v1->obj == COLONLIST_OBJ) ? 0 : 2, chars = 0;
    struct value_s *tmp = NULL;
    uint8_t *s;
    if (v1->u.list.len) {
        tmp = (struct value_s *)malloc(v1->u.list.len * sizeof(struct value_s));
        if (!tmp || v1->u.list.len > SIZE_MAX / sizeof(struct value_s)) err_msg_out_of_memory(); /* overflow */
        for (i = 0;i < v1->u.list.len; i++) {
            v1->u.list.data[i]->obj->repr(v1->u.list.data[i], &tmp[i], epoint);
            if (tmp[i].obj != STR_OBJ) {
                if (v1 == v) v->obj->destroy(v);
                tmp[i].obj->copy_temp(&tmp[i], v);
                while (i--) tmp[i].obj->destroy(&tmp[i]);
                free(tmp);
                return;
            }
            len += tmp[i].u.str.len;
            if (len < tmp[i].u.str.len) err_msg_out_of_memory(); /* overflow */
        }
        if (i && (v1->obj != TUPLE_OBJ)) i--;
        if (i) {
            len += i;
            if (len < i) err_msg_out_of_memory(); /* overflow */
        }
    }
    if (v1 == v) v->obj->destroy(v);
    s = str_create_elements(v, len);
    len = 0;
    if (v1->obj != ADDRLIST_OBJ && v1->obj != COLONLIST_OBJ) s[len++] = (v1->obj == LIST_OBJ) ? '[' : '(';
    for (i = 0;i < v1->u.list.len; i++) {
        if (i) s[len++] = (v1->obj == COLONLIST_OBJ) ? ':' : ',';
        if (tmp[i].u.str.len) {
            memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
            len += tmp[i].u.str.len;
            chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        }
        STR_OBJ->destroy(&tmp[i]);
    }
    if (i == 1 && (v1->obj == TUPLE_OBJ)) s[len++] = ',';
    if (v1->obj != ADDRLIST_OBJ && v1->obj != COLONLIST_OBJ) s[len++] = (v1->obj == LIST_OBJ) ? ']' : ')';
    free(tmp);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
}

static void len(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t uv = v1->u.list.len;
    if (v1 == v) destroy(v);
    int_from_uval(v, uv);
}

static void getiter(struct value_s *v1, struct value_s *v) {
    v->obj = ITER_OBJ;
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
}

static struct value_s *MUST_CHECK next(struct value_s *v1, struct value_s *UNUSED(v)) {
    const struct value_s *vv1 = v1->u.iter.data;
    if (v1->u.iter.val >= vv1->u.list.len) return NULL;
    return val_reference(vv1->u.list.data[v1->u.iter.val++]);
}

struct value_s **list_create_elements(struct value_s *v, size_t n) {
    return lnew(v, n);
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
    size_t i = 0;
    struct value_s **vals;
    if (v == v1) {
        for (;i < v1->u.list.len; i++) {
            op->v1 = v1->u.list.data[i];
            if (op->v1->refcount != 1) {
                op->v = val_alloc();
                op->v1->obj->calc1(op);
                val_destroy(v1->u.list.data[i]); v1->u.list.data[i] = op->v;
            } else {
                op->v = op->v1;
                op->v1->obj->calc1(op);
            }
        }
        op->v = v;
        op->v1 = v1;
        return;
    }
    if (v1->u.list.len) {
        vals = lnew(v, v1->u.list.len);
        for (;i < v1->u.list.len; i++) {
            op->v1 = v1->u.list.data[i];
            op->v = vals[i] = val_alloc();
            op->v1->obj->calc1(op);
        }
        op->v = v;
        op->v1 = v1;
    } else vals = v->u.list.val;
    v->obj = v1->obj;
    v->u.list.len = i;
    v->u.list.data = vals;
    return;
}

static int calc2_list(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    size_t i;
    int val;

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
            struct value_s **vals, tmp;
            const struct value_s *vx;
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
                        op->v1 = v1->u.list.data[0];
                        vals = lnew(&tmp, v2->u.list.len);
                        for (; i < v2->u.list.len; i++) {
                            op->v = vals[i] = val_alloc();
                            op->v2 = v2->u.list.data[i];
                            op->v1->obj->calc2(op);
                        }
                        op->v = v;
                        op->v1 = v1;
                        op->v2 = v2;
                    } else vals = NULL;
                } else if (v2->u.list.len == 1) {
                    if (v1->u.list.len) {
                        op->v2 = v2->u.list.data[0];
                        vals = lnew(&tmp, v1->u.list.len);
                        for (; i < v1->u.list.len; i++) {
                            op->v = vals[i] = val_alloc();
                            op->v1 = v1->u.list.data[i];
                            op->v1->obj->calc2(op);
                        }
                        op->v = v;
                        op->v1 = v1;
                        op->v2 = v2;
                    } else vals = NULL;
                } else if (v1->u.list.len == v2->u.list.len) {
                    if (v1->u.list.len) {
                        vals = lnew(&tmp, v1->u.list.len);
                        for (; i < v1->u.list.len; i++) {
                            op->v = vals[i] = val_alloc();
                            op->v1 = v1->u.list.data[i];
                            op->v2 = v2->u.list.data[i];
                            op->v1->obj->calc2(op);
                        }
                        op->v = v;
                        op->v1 = v1;
                        op->v2 = v2;
                    } else vals = NULL;
                } else {
                    obj_oper_error(op);
                    return 0;
                }
            } else if (d1 > d2) {
                if (v1->u.list.len) {
                    vals = lnew(&tmp, v1->u.list.len);
                    for (; i < v1->u.list.len; i++) {
                        op->v = vals[i] = val_alloc();
                        op->v1 = v1->u.list.data[i];
                        op->v1->obj->calc2(op);
                    }
                    op->v = v;
                    op->v1 = v1;
                } else vals = NULL;
            } else if (v2->u.list.len) {
                vals = lnew(&tmp, v2->u.list.len);
                for (; i < v2->u.list.len; i++) {
                    op->v = vals[i] = val_alloc();
                    op->v2 = v2->u.list.data[i];
                    v1->obj->calc2(op);
                }
                op->v = v;
                op->v2 = v2;
            } else vals = NULL;
            if (v == v1 || v == v2) destroy(v);
            if (vals == tmp.u.list.val) {
                if (i) memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
                vals = v->u.list.val;
            }
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return 0;
        }
    case O_CONCAT:
        {
            struct value_s **vals;
            struct value_s tmp;
            size_t ln;

            ln = v1->u.list.len + v2->u.list.len;
            if (ln < v2->u.list.len) err_msg_out_of_memory(); /* overflow */
            vals = lnew(&tmp, ln);
            for (i = 0; i < v1->u.list.len; i++) {
                vals[i] = val_reference(v1->u.list.data[i]);
            }
            for (; i < ln; i++) {
                vals[i] = val_reference(v2->u.list.data[i - v1->u.list.len]);
            }
            v->obj = v1->obj;
            if (v == v1 || v == v2) destroy(v);
            if (vals == tmp.u.list.val) {
                if (ln) memcpy(v->u.list.val, vals, ln * sizeof(v->u.list.data[0]));
                vals = v->u.list.val;
            }
            v->u.list.len = ln;
            v->u.list.data = vals;
            return 0;
        }
    default: return 1;
    }
    if (v == v1 || v == v2) destroy(v);
    bool_from_int(v, val);
    return 0;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    size_t i = 0;
    struct value_s **vals, tmp;

    switch (v2->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (v1->obj == v2->obj) {
            if (calc2_list(op)) break; return;
        }
    default:
        if (v == v1) {
            for (;i < v1->u.list.len; i++) {
                op->v1 = v1->u.list.data[i];
                if (op->v1->refcount != 1) {
                    op->v = val_alloc();
                    op->v1->obj->calc2(op);
                    val_destroy(v1->u.list.data[i]); v1->u.list.data[i] = op->v;
                } else {
                    op->v = op->v1;
                    op->v1->obj->calc2(op);
                }
            }
            op->v = v;
            op->v1 = v1;
            return;
        }
        vals = lnew(&tmp, v1->u.list.len);
        if (v1->u.list.len) {
            for (;i < v1->u.list.len; i++) {
                op->v1 = v1->u.list.data[i];
                op->v = vals[i] = val_alloc();
                op->v1->obj->calc2(op);
            }
            op->v = v;
            op->v1 = v1;
        }
        if (v == v2) v->obj->destroy(v);
        v->obj = v1->obj;
        if (vals == tmp.u.list.val) {
            memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
            vals = v->u.list.val;
        }
        v->u.list.len = i;
        v->u.list.data = vals;
        return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    size_t i = 0;
    struct value_s **vals, tmp;
    if (op->op == &o_IN) {
        op->op = &o_EQ;
        op->v = &tmp;
        for (;i < v2->u.list.len; i++) {
            op->v2 = v2->u.list.data[i];
            v1->obj->calc2(op);
            if (tmp.obj == BOOL_OBJ) {
                if (tmp.u.boolean) {
                    if (v == v1 || v == v2) v->obj->destroy(v);
                    bool_from_int(v, 1);
                    return;
                }
            } else tmp.obj->destroy(&tmp);
        }
        op->v = v;
        op->v2 = v2;
        if (v == v1 || v == v2) v->obj->destroy(v);
        bool_from_int(v, 0);
        return;
    }
    switch (v1->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (v1->obj == v2->obj) {
            if (calc2_list(op)) break; return;
        }
        goto def;
    case T_STR: 
        if (op->op == &o_MOD) {
            isnprintf(v1, v2, v, &op->epoint, &op->epoint2); return;
        }
        /* fall through */
    default:
    def:
        switch (op->op->u.oper.op) {
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
            if (v == v2) {
                for (;i < v2->u.list.len; i++) {
                    op->v2 = v2->u.list.data[i];
                    if (op->v2->refcount != 1) {
                        op->v = val_alloc();
                        op->v2->obj->rcalc2(op);
                        val_destroy(v2->u.list.data[i]); v2->u.list.data[i] = op->v;
                    } else {
                        op->v = op->v2;
                        op->v2->obj->rcalc2(op);
                    }
                }
                op->v = v;
                op->v2 = v2;
                return;
            }
            vals = lnew(&tmp, v2->u.list.len);
            if (v2->u.list.len) {
                for (;i < v2->u.list.len; i++) {
                    op->v = vals[i] = val_alloc();
                    op->v2 = v2->u.list.data[i];
                    op->v2->obj->rcalc2(op);
                }
                op->v = v;
                op->v2 = v2;
            }
            if (v == v1) v->obj->destroy(v);
            v->obj = v2->obj;
            if (vals == tmp.u.list.val) {
                memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
                vals = v->u.list.val;
            }
            v->u.list.len = i;
            v->u.list.data = vals;
            return;
        default: 
            v1->obj->calc2(op);return;
        }
        break;
    }
    obj_oper_error(op);
}

static void repeat(oper_t op, uval_t rep) {
    struct value_s **vals;
    struct value_s *v1 = op->v1, *v = op->v, tmp;

    v->obj = v1->obj;
    if (v1->u.list.len && rep) {
        size_t i = 0, j, ln;
        ln = v1->u.list.len * rep;
        if (v1->u.list.len > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        vals = lnew(&tmp, ln);
        while (rep--) {
            for (j = 0;j < v1->u.list.len; j++, i++) {
                vals[i] = val_reference(v1->u.list.data[j]);
            }
        }
        if (v == v1) destroy(v);
        if (vals == tmp.u.list.val) {
            memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
            vals = v->u.list.val;
        }
        v->u.list.len = i;
        v->u.list.data = vals;
        return;
    }
    if (v == v1) destroy(v);
    v->u.list.data = v->u.list.val;
    v->u.list.len = 0;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v) {
    struct value_s **vals, tmp;
    size_t i;
    size_t ln;

    if (step > 0) {
        if (end < offs) end = offs;
        ln = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        ln = (offs - end - step - 1) / -step;
    }

    if (!ln) {
        if (v1 == v) destroy(v);
        copy((v1->obj == TUPLE_OBJ) ? &null_tuple : &null_list, v);
        return;
    }

    if (step == 1 && ln == v1->u.list.len && v1->obj == TUPLE_OBJ) {
        if (v1 != v) copy(v1, v);
        return; /* original tuple */
    }
    vals = lnew(&tmp, ln);
    i = 0;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        vals[i++] = val_reference(v1->u.list.data[offs]);
        offs += step;
    }
    if (v == v1) destroy(v);
    if (vals == tmp.u.list.val) {
        if (i) memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
        vals = v->u.list.val;
    }
    v->obj = v1->obj;
    v->u.list.len = i;
    v->u.list.data = vals;
}

static void iindex(oper_t op) {
    struct value_s **vals, tmp;
    size_t i, ln;
    ival_t offs;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v, err;

    ln = v1->u.list.len;

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            if (v1 == v) destroy(v);
            copy((v1->obj == TUPLE_OBJ) ? &null_tuple : &null_list, v);
            return;
        }
        vals = lnew(&tmp, v2->u.list.len);
        for (i = 0; i < v2->u.list.len; i++) {
            offs = indexoffs(v2->u.list.data[i], &err, ln, &op->epoint2);
            if (offs < 0) {
                if (v1 == v) destroy(v);
                v->u.list.len = i;
                v->u.list.data = vals;
                destroy(v);
                err.obj->copy_temp(&err, v);
                return;
            }
            vals[i] = val_reference(v1->u.list.data[offs]);
        }
        if (v1 == v) destroy(v);
        if (vals == tmp.u.list.val) {
            if (i) memcpy(v->u.list.val, vals, i * sizeof(v->u.list.data[0]));
            vals = v->u.list.val;
        }
        v->obj = v1->obj;
        v->u.list.len = i;
        v->u.list.data = vals;
        return;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t len, end, step;
        len = sliceparams(op, ln, &offs, &end, &step);
        if (len < 0) return;
        return slice(v1, offs, end, step, v);
    }
    offs = indexoffs(v2, &err, ln, &op->epoint2);
    if (offs < 0) {
        if (v1 == v) destroy(v);
        err.obj->copy_temp(&err, v);
        return;
    }
    v2 = val_reference(v1->u.list.data[offs]);
    if (v1 == v) destroy(v);
    v2->obj->copy(v2, v);
    val_destroy(v2);
}

static void init(struct obj_s *obj) {
    obj->destroy = destroy;
    obj->copy = copy;
    obj->copy_temp = copy_temp;
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
    addrlist_obj.copy = copy;
    addrlist_obj.copy_temp = copy_temp;
    addrlist_obj.same = same;
    addrlist_obj.repr = repr_listtuple;
    obj_init(&colonlist_obj, T_COLONLIST, "<colon list>");
    colonlist_obj.destroy = destroy;
    colonlist_obj.copy = copy;
    colonlist_obj.copy_temp = copy_temp;
    colonlist_obj.same = same;
    colonlist_obj.repr = repr_listtuple;
}
