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
#include "strobj.h"

static struct obj_s list_obj;
static struct obj_s tuple_obj;

obj_t LIST_OBJ = &list_obj;
obj_t TUPLE_OBJ = &tuple_obj;

static void destroy(struct value_s *v1) {
    size_t i;
    for (i = 0; i < v1->u.list.len; i++) {
        val_destroy(v1->u.list.data[i]);
    }
    free(v1->u.list.data);
}

static void copy(const struct value_s *v1, struct value_s *v) {
    struct value_s **vals;
    size_t len = v1->u.list.len;
    if (len) {
        size_t i;
        vals = (struct value_s **)malloc(len * sizeof(v->u.list.data[0]));
        if (!vals) err_msg_out_of_memory();
        for (i = 0; i < len; i++) {
            vals[i] = val_reference(v1->u.list.data[i]);
        }
    } else vals = NULL;
    v->obj = v1->obj;
    v->refcount = 1;
    v->u.list.data = vals;
    v->u.list.len = len;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->refcount = 1;
    v->u.list.len = v1->u.list.len;
    v->u.list.data = v1->u.list.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.list.len != v2->u.list.len) return 0;
    for (i = 0; i < v2->u.list.len; i++) {
        if (!obj_same(v1->u.list.data[i], v2->u.list.data[i])) return 0;
    }
    return 1;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.list.len;
}

static void repr_list(const struct value_s *v1, struct value_s *v) {
    size_t i, len = 2, chars = 0;
    struct value_s *tmp = NULL;
    uint8_t *s;
    if (v1->u.list.len) {
        tmp = (struct value_s *)malloc(v1->u.list.len * sizeof(struct value_s));
        if (!tmp || v1->u.list.len > ((size_t)~0) / sizeof(struct value_s)) err_msg_out_of_memory(); /* overflow */
        for (i = 0;i < v1->u.list.len; i++) {
            v1->u.list.data[i]->obj->repr(v1->u.list.data[i], &tmp[i]);
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
        if (i > 1) {
            i--;
            len += i;
            if (len < i) err_msg_out_of_memory(); /* overflow */
        }
    }
    s = (uint8_t *)malloc(len);
    if (!s) err_msg_out_of_memory();
    len = 0;
    s[len++] = '[';
    for (i = 0;i < v1->u.list.len; i++) {
        if (i) s[len++] = ',';
        if (tmp[i].u.str.len) {
            memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
            len += tmp[i].u.str.len;
            chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        }
        tmp[i].obj->destroy(&tmp[i]);
    }
    s[len++] = ']';
    free(tmp);
    if (v1 == v) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
}

static void repr_tuple(const struct value_s *v1, struct value_s *v) {
    size_t i, len = 2, chars = 0;
    struct value_s *tmp = NULL;
    uint8_t *s;
    if (v1->u.list.len) {
        tmp = (struct value_s *)malloc(v1->u.list.len * sizeof(struct value_s));
        if (!tmp || v1->u.list.len > ((size_t)~0) / sizeof(struct value_s)) err_msg_out_of_memory(); /* overflow */
        for (i = 0;i < v1->u.list.len; i++) {
            v1->u.list.data[i]->obj->repr(v1->u.list.data[i], &tmp[i]);
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
        if (i) {
            len += i;
            if (len < i) err_msg_out_of_memory(); /* overflow */
        }
    }
    s = (uint8_t *)malloc(len);
    if (!s) err_msg_out_of_memory();
    len = 0;
    s[len++] = '(';
    for (i = 0;i < v1->u.list.len; i++) {
        if (i) s[len++] = ',';
        if (tmp[i].u.str.len) {
            memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
            len += tmp[i].u.str.len;
            chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        }
        tmp[i].obj->destroy(&tmp[i]);
    }
    if (i == 1) s[len++] = ',';
    s[len++] = ')';
    free(tmp);
    if (v1 == v) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
}

static int MUST_CHECK len(const struct value_s *v1, struct value_s *UNUSED(v), uval_t *uv, linepos_t UNUSED(epoint)) {
    *uv = v1->u.list.len;
    return 0;
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
    size_t i = 0;
    struct value_s **vals, new_value;
    if (op->op == &o_LNOT) {
        if (v1 == v) destroy(v);
        bool_from_int(v, !v1->u.list.len);
        return;
    }
    if (v == v1) {
        for (;i < v1->u.list.len; i++) {
            op->v1 = v1->u.list.data[i];
            if (op->v1->refcount != 1) {
                op->v = &new_value;
                op->v1->obj->calc1(op);
                val_replace_template(v->u.list.data + i, &new_value);
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
        vals = (struct value_s **)malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
        if (!vals) err_msg_out_of_memory();
        for (;i < v1->u.list.len; i++) {
            op->v1 = v1->u.list.data[i];
            op->v = vals[i] = val_alloc();
            op->v1->obj->calc1(op);
        }
        op->v = v;
        op->v1 = v1;
    } else vals = NULL;
    v->obj = v1->obj;
    v->u.list.len = i;
    v->u.list.data = vals;
    return;
}

static int calc2_list(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    size_t i;
    int val;

    if (v1->obj != v2->obj) return 1;
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
        {
            struct value_s **vals;
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
                        vals = (struct value_s **)malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
                        if (!vals) err_msg_out_of_memory();
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
                        vals = (struct value_s **)malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                        if (!vals) err_msg_out_of_memory();
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
                        vals = (struct value_s **)malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                        if (!vals) err_msg_out_of_memory();
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
                    vals = (struct value_s **)malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (; i < v1->u.list.len; i++) {
                        op->v = vals[i] = val_alloc();
                        op->v1 = v1->u.list.data[i];
                        op->v1->obj->calc2(op);
                    }
                    op->v = v;
                    op->v1 = v1;
                } else vals = NULL;
            } else if (v2->u.list.len) {
                vals = (struct value_s **)malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
                if (!vals) err_msg_out_of_memory();
                for (; i < v2->u.list.len; i++) {
                    op->v = vals[i] = val_alloc();
                    op->v2 = v2->u.list.data[i];
                    v1->obj->calc2(op);
                }
                op->v = v;
                op->v2 = v2;
            } else vals = NULL;
            if (v == v1 || v == v2) destroy(v);
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return 0;
        }
    case O_EQ:
        if (v1->u.list.len != v2->u.list.len) { val = 0; break; }
        val = 1;
        for (i = 0; i < v2->u.list.len; i++) {
            struct value_s tmp;
            op->v = &tmp;
            op->v1 = v1->u.list.data[i];
            op->v2 = v2->u.list.data[i];
            op->v1->obj->calc2(op);
            if (tmp.obj != BOOL_OBJ || !tmp.u.boolean) { val = 0; break; }
        }
        op->v = v;
        op->v1 = v1;
        op->v2 = v2;
        break;
    case O_NE:
        if (v1->u.list.len != v2->u.list.len) { val = 1; break; }
        val = 0;
        for (i = 0; i < v2->u.list.len; i++) {
            struct value_s tmp;
            op->v = &tmp;
            op->v1 = v1->u.list.data[i];
            op->v2 = v2->u.list.data[i];
            op->v1->obj->calc2(op);
            if (tmp.obj == BOOL_OBJ && tmp.u.boolean) { val = 1; break; }
        }
        op->v = v;
        op->v1 = v1;
        op->v2 = v2;
        break;
    case O_CMP:
    case O_LT:
    case O_LE:
    case O_GT:
    case O_GE:    /* TODO */
        obj_oper_error(op);
        return 0;
    case O_CONCAT:
        {
            struct value_s **vals;
            size_t len;

            len = v1->u.list.len + v2->u.list.len;
            if (len < v2->u.list.len) err_msg_out_of_memory(); /* overflow */
            if (v == v1) {
                if (!v2->u.list.len) return 0;
                vals = (struct value_s **)realloc(v1->u.list.data, len * sizeof(v->u.list.data[0]));
                if (!vals || len > ((size_t)~0) / sizeof(v->u.list.data[0])) err_msg_out_of_memory(); /* overflow */
                for (i = v1->u.list.len; i < len; i++) {
                    vals[i] = val_reference(v2->u.list.data[i - v1->u.list.len]);
                }
            } else if (len) {
                vals = (struct value_s **)malloc(len * sizeof(v->u.list.data[0]));
                if (!vals || len > ((size_t)~0) / sizeof(v->u.list.data[0])) err_msg_out_of_memory(); /* overflow */
                for (i = 0; i < v1->u.list.len; i++) {
                    vals[i] = val_reference(v1->u.list.data[i]);
                }
                for (; i < len; i++) {
                    vals[i] = val_reference(v2->u.list.data[i - v1->u.list.len]);
                }
            } else vals = NULL;
            v->obj = v1->obj;
            v->u.list.len = len;
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
    struct value_s **vals, new_value;

    switch (v2->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (calc2_list(op)) break; return;
    case T_IDENT:
    case T_ANONIDENT:
    case T_IDENTREF:
        if (op->op != &o_MEMBER) {
            v2->obj->rcalc2(op);return;
        }
    default:
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
            if (v == v1) {
                for (;i < v1->u.list.len; i++) {
                    op->v1 = v1->u.list.data[i];
                    if (op->v1->refcount != 1) {
                        op->v = &new_value;
                        op->v1->obj->calc2(op);
                        val_replace_template(v->u.list.data + i, &new_value);
                    } else {
                        op->v = op->v1;
                        op->v1->obj->calc2(op);
                    }
                }
                op->v = v;
                op->v1 = v1;
                return;
            }
            if (v1->u.list.len) {
                vals = (struct value_s **)malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                if (!vals) err_msg_out_of_memory();
                for (;i < v1->u.list.len; i++) {
                    op->v1 = v1->u.list.data[i];
                    op->v = vals[i] = val_alloc();
                    op->v1->obj->calc2(op);
                }
                op->v = v;
                op->v1 = v1;
            } else vals = NULL;
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return;
        default: 
            v2->obj->rcalc2(op);return;
        }
        break;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    size_t i = 0;
    struct value_s **vals, new_value;
    if (op->op == &o_IN) {
        struct value_s tmp;
        op->op = &o_EQ;
        op->v = &tmp;
        for (;i < v2->u.list.len; i++) {
            op->v2 = v2->u.list.data[i];
            v1->obj->calc2(op);
            if (tmp.obj == BOOL_OBJ && tmp.u.boolean) {
                if (v == v1) obj_destroy(v);
                bool_from_int(v, 1);
                return;
            }
        }
        op->v = v;
        op->v2 = v2;
        if (v == v1) obj_destroy(v);
        bool_from_int(v, 0);
        return;
    }
    switch (v1->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (calc2_list(op)) break; return;
    case T_IDENT:
    case T_ANONIDENT:
    case T_IDENTREF:
        if (op->op != &o_MEMBER) {
            v1->obj->calc2(op);return;
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
                        op->v = &new_value;
                        op->v2->obj->rcalc2(op);
                        val_replace_template(v2->u.list.data + i, &new_value);
                    } else {
                        op->v = op->v2;
                        op->v2->obj->rcalc2(op);
                    }
                }
                op->v = v;
                op->v2 = v2;
                return;
            }
            if (v2->u.list.len) {
                vals = (struct value_s **)malloc(v2->u.list.len * sizeof(new_value.u.list.data[0]));
                if (!vals) err_msg_out_of_memory();
                for (;i < v2->u.list.len; i++) {
                    op->v = vals[i] = val_alloc();
                    op->v2 = v2->u.list.data[i];
                    op->v2->obj->rcalc2(op);
                }
                op->v = v;
                op->v2 = v2;
            } else vals = NULL;
            if (v == v1) v->obj->destroy(v);
            v->obj = v2->obj;
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
    size_t i = 0, j, len;
    struct value_s **vals;
    struct value_s *v1 = op->v1, *v = op->v;

    if (v == v1) {
        if (!v1->u.list.len) return;
        if (!rep) {
            destroy(v1);
            v->u.list.len = 0;
            v->u.list.data = NULL;
            return;
        }
        if (rep == 1) return;
        i = len = v->u.list.len;
        v->u.list.len *= rep;
        if (len > ((size_t)~0) / rep) err_msg_out_of_memory(); /* overflow */
        v->u.list.data = (struct value_s **)realloc(v->u.list.data, v->u.list.len * sizeof(v->u.list.data[0]));
        if (!v->u.list.data || v->u.list.len > ((size_t)~0) / sizeof(v->u.list.data[0])) err_msg_out_of_memory(); /* overflow */
        while (--rep) {
            for (j = 0;j < len; j++, i++) {
                v->u.list.data[i] = val_reference(v->u.list.data[j]);
            }
        }
    } else {
        if (v1->u.list.len && rep) {
            len = v1->u.list.len * rep;
            if (v1->u.list.len > ((size_t)~0) / rep) err_msg_out_of_memory(); /* overflow */
            vals = (struct value_s **)malloc(len * sizeof(v->u.list.data[0]));
            if (!vals || len > ((size_t)~0) / sizeof(v->u.list.data[0])) err_msg_out_of_memory(); /* overflow */
            while (rep--) {
                for (j = 0;j < v1->u.list.len; j++, i++) {
                    vals[i] = val_reference(v1->u.list.data[j]);
                }
            }
        } else vals = NULL;
        v->obj = v1->obj;
        v->u.list.len = i;
        v->u.list.data = vals;
    }
}

static void iindex(oper_t op) {
    struct value_s **vals;
    size_t i, len;
    ival_t offs;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    len = v1->u.list.len;

    if (v2->obj == TUPLE_OBJ || v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            if (v1 == v) destroy(v);
            copy((v1->obj == TUPLE_OBJ) ? &null_tuple : &null_list, v);
            return;
        }
        vals = (struct value_s **)malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
        if (!vals) err_msg_out_of_memory();
        for (i = 0; i < v2->u.list.len; i++) {
            offs = indexoffs(v2->u.list.data[i], len);
            if (offs < 0) {
                if (v1 == v) destroy(v);
                v->u.list.len = i;
                v->u.list.data = vals;
                destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___INDEX_RANGE;
                v->u.error.epoint = op->epoint2;
                return;
            }
            vals[i] = val_reference(v1->u.list.data[offs]);
        }
        if (v1 == v) destroy(v);
        v->obj = v1->obj;
        v->u.list.len = i;
        v->u.list.data = vals;
        return;
    }
    offs = indexoffs(v2, len);
    if (offs < 0) {
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR___INDEX_RANGE;
        v->u.error.epoint = op->epoint2;
        return;
    }
    v2 = val_reference(v1->u.list.data[offs]);
    if (v1 == v) destroy(v);
    v2->obj->copy(v2, v);
    val_destroy(v2);
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t UNUSED(epoint)) {
    struct value_s **vals;
    size_t i;
    size_t len;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }

    if (!len) {
        if (v1 == v) destroy(v);
        copy((v1->obj == TUPLE_OBJ) ? &null_tuple : &null_list, v);
        return;
    }

    if (step == 1 && len == v1->u.list.len && v1->obj == TUPLE_OBJ) {
        if (v1 != v) copy(v1, v);
        return; /* original tuple */
    }
    vals = (struct value_s **)malloc(len * sizeof(v->u.list.data[0]));
    if (!vals) err_msg_out_of_memory();
    i = 0;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        vals[i++] = val_reference(v1->u.list.data[offs]);
        offs += step;
    }
    if (v == v1) destroy(v);
    v->obj = v1->obj;
    v->u.list.len = i;
    v->u.list.data = vals;
}

static void init(struct obj_s *obj) {
    obj->destroy = destroy;
    obj->copy = copy;
    obj->copy_temp = copy_temp;
    obj->same = same;
    obj->truth = truth;
    obj->len = len;
    obj->calc1 = calc1;
    obj->calc2 = calc2;
    obj->rcalc2 = rcalc2;
    obj->repeat = repeat;
    obj->iindex = iindex;
    obj->slice = slice;
}

void listobj_init(void) {
    obj_init(&list_obj, T_LIST, "<list>");
    init(&list_obj);
    list_obj.repr = repr_list;
    obj_init(&tuple_obj, T_TUPLE, "<tuple>");
    init(&tuple_obj);
    tuple_obj.repr = repr_tuple;
}
