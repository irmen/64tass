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
#include "codeobj.h"
#include "eval.h"
#include "mem.h"
#include "64tass.h"
#include "section.h"
#include "variables.h"

#include "boolobj.h"

static struct obj_s obj;

obj_t CODE_OBJ = &obj;

static void destroy(struct value_s *v1) {
    val_destroy(v1->u.code.addr);
}

static int access_check(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    str_t name;
    if (v1->u.code.parent->requires & ~current_section->provides) {
        name = v1->u.code.parent->name;
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.u.ident = name;
        v->u.error.epoint = *epoint;
        v->u.error.num = ERROR_REQUIREMENTS_;
        return 1;
    }
    if (v1->u.code.parent->conflicts & current_section->provides) {
        name = v1->u.code.parent->name;
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.u.ident = name;
        v->u.error.epoint = *epoint;
        v->u.error.num = ERROR______CONFLICT;
        return 1;
    }
    return 0;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = CODE_OBJ;
    v->refcount = 1;
    memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
    v->u.code.addr = val_reference(v1->u.code.addr);
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == CODE_OBJ && (v1->u.code.addr == v2->u.code.addr || v1->u.code.addr->obj->same(v1->u.code.addr, v2->u.code.addr)) && v1->u.code.size == v2->u.code.size && v1->u.code.dtype == v2->u.code.dtype && v1->u.code.parent == v2->u.code.parent;
}

static int truth(const struct value_s *v1, struct value_s *v, enum truth_e type, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    if (v1 == v) {
        int res;
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        res = tmp->obj->truth(tmp, v, type, epoint);
        val_destroy(tmp);
        return res;
    }
    v1 = v1->u.code.addr;
    return v1->obj->truth(v1, v, type, epoint);
}

static void repr(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (epoint && access_check(v1, v, epoint)) return;
    if (v1 == v) {
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        tmp->obj->repr(tmp, v, epoint);
        val_destroy(tmp);
        return;
    }
    v1 = v1->u.code.addr;
    return v1->obj->repr(v1, v, epoint);
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    if (v1 == v) {
        int res;
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        res = tmp->obj->ival(tmp, v, iv, bits, epoint);
        val_destroy(tmp);
        return res;
    }
    v1 = v1->u.code.addr;
    return v1->obj->ival(v1, v, iv, bits, epoint);
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    if (v1 == v) {
        int res;
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        res = tmp->obj->uval(tmp, v, uv, bits, epoint);
        val_destroy(tmp);
        return res;
    }
    v1 = v1->u.code.addr;
    return v1->obj->uval(v1, v, uv, bits, epoint);
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    if (v1 == v) {
        int res;
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        res = tmp->obj->real(tmp, v, r, epoint);
        val_destroy(tmp);
        return res;
    }
    v1 = v1->u.code.addr;
    return v1->obj->real(v1, v, r, epoint);
}

static void sign(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return;
    if (v1 == v) {
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        tmp->obj->sign(tmp, v, epoint);
        val_destroy(tmp);
        return;
    }
    v1 = v1->u.code.addr;
    return v1->obj->sign(v1, v, epoint);
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return;
    if (v1 == v) {
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        tmp->obj->abs(tmp, v, epoint);
        val_destroy(tmp);
        return;
    }
    v1 = v1->u.code.addr;
    return v1->obj->abs(v1, v, epoint);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return;
    if (v1 == v) {
        struct value_s *tmp = val_reference(v1->u.code.addr);
        destroy(v);
        tmp->obj->integer(tmp, v, epoint);
        val_destroy(tmp);
        return;
    }
    v1 = v1->u.code.addr;
    return v1->obj->integer(v1, v, epoint);
}

static void len(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    size_t uv;
    if (!v1->u.code.pass) {
        str_t name = v1->u.code.parent->name;
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident = name;
        return;
    }
    uv = v1->u.code.size / (abs(v1->u.code.dtype) + !v1->u.code.dtype);
    if (v1 == v) destroy(v);
    int_from_uval(v, uv);
}

static void size(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    size_t s;
    if (!v1->u.code.pass) {
        str_t name = v1->u.code.parent->name;
        if (v1 == v) destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident = name;
        return;
    }
    s = v1->u.code.size;
    if (v1 == v) destroy(v);
    int_from_uval(v, s);
}

static void calc1(oper_t op) {
    struct value_s *v = op->v, *v1 = op->v1;
    switch (op->op->u.oper.op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_STRING:
        op->v1 = val_reference(v1->u.code.addr);
        if (v == v1) destroy(v);
        op->v1->obj->calc1(op);
        val_destroy(op->v1);
        op->v1 = v1;
        return;
    case O_INV:
    case O_NEG:
    case O_POS:
        op->v1 = val_reference(v1->u.code.addr);
        op->v = val_alloc();
        if (v == v1) destroy(v);
        op->v1->obj->calc1(op);
        v->obj = CODE_OBJ; 
        if (v != v1) memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
        v->u.code.addr = op->v;
        val_destroy(op->v1);
        op->v1 = v1;
        op->v = v;
        return;
    default: break;
    }
    obj_oper_error(op);
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
        struct linepos_s epoint;
        str_t name;
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.code.parent;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                if (v == v1) v->obj->destroy(v);
                l->value->obj->copy(l->value, v);
                return;
            } 
            if (!referenceit) {
                if (v == v1) v->obj->destroy(v);
                v->obj = NONE_OBJ;
                return;
            }
            epoint = v2->u.ident.epoint;
            name = v2->u.ident.name;
            if (v == v1) v->obj->destroy(v);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint;
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.ident = name;
            v->u.error.u.notdef.down = 0;
            return;
        case T_ANONIDENT:
            {
                l2 = v1->u.code.parent;
                l = find_anonlabel2(v2->u.anonident.count, l2);
                if (l) {
                    touch_label(l);
                    if (v == v1) v->obj->destroy(v);
                    l->value->obj->copy(l->value, op->v);
                    return;
                }
                if (!referenceit) {
                    if (v == v1) v->obj->destroy(v);
                    v->obj = NONE_OBJ;
                    return;
                }
                epoint = v2->u.anonident.epoint;
                if (v == v1) v->obj->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___NOT_DEFINED;
                v->u.error.epoint = epoint;
                v->u.error.u.notdef.label = l2;
                v->u.error.u.notdef.ident.len = 1;
                v->u.error.u.notdef.ident.data = (const uint8_t *)((v2->u.anonident.count >= 0) ? "+" : "-");
                v->u.error.u.notdef.down = 0;
                return;
            }
        case T_TUPLE:
        case T_LIST: return v2->obj->rcalc2(op);
        default: return obj_oper_error(op);
        }
    }
    switch (v2->obj->type) {
    case T_CODE:
        if (access_check(op->v1, v, &op->epoint)) return;
        if (access_check(op->v2, v, &op->epoint2)) return;
        op->v1 = val_reference(v1->u.code.addr);
        op->v2 = val_reference(v2->u.code.addr);
        if (v1 == v || v2 == v) destroy(v);
        op->v1->obj->calc2(op);
        val_destroy(op->v1);
        val_destroy(op->v2);
        op->v1 = v1;
        op->v2 = v2;
        return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
        if (access_check(op->v1, v, &op->epoint)) return;
        op->v1 = val_reference(v1->u.code.addr);
        switch (op->op->u.oper.op) {
        case O_ADD:
        case O_SUB:
            op->v = val_alloc();
            op->v1->obj->calc2(op);
            if (v == v2) v->obj->destroy(v);
            v->obj = CODE_OBJ; 
            if (v != v1) memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
            else val_destroy(v->u.code.addr);
            v->u.code.addr = op->v;
            val_destroy(op->v1);
            op->v = v;
            op->v1 = v1;
            return;
        default: break;
        }
        if (v == v1) v->obj->destroy(v);
        op->v1->obj->calc2(op);
        val_destroy(op->v1);
        op->v1 = v1;
        return;
    default: v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    if (op->op == &o_IN) {
        struct oper_s oper;
        size_t i, ln, offs;
        struct value_s new_value;
        int16_t r;
        uval_t uv;

        if (v2->u.code.pass != pass) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR____NO_FORWARD;
            v->u.error.epoint = op->epoint2;
            v->u.error.u.ident = v2->u.code.parent->name;
            return;
        }
        ln = (v2->u.code.dtype < 0) ? -v2->u.code.dtype : v2->u.code.dtype;
        ln = ln + !ln;
        oper.op = &o_EQ;
        oper.v1 = &new_value;
        oper.v2 = v1;
        oper.v = &new_value;
        oper.epoint = op->epoint;
        oper.epoint2 = op->epoint2;
        oper.epoint3 = op->epoint3;
        for (offs = 0; offs < v2->u.code.size;) {
            uv = 0;
            r = -1;
            for (i = 0; i < ln; i++) {
                r = read_mem(v2->u.code.mem, v2->u.code.memp, v2->u.code.membp, offs++);
                if (r < 0) break;
                uv |= r << (i * 8);
            }
            if (v2->u.code.dtype < 0 && (r & 0x80)) {
                for (; i < sizeof(uv); i++) {
                    uv |= 0xff << (i * 8);
                }
            }
            if (r < 0) new_value.obj = GAP_OBJ;
            else if (v2->u.code.dtype < 0) int_from_ival(&new_value, (ival_t)uv);
            else int_from_uval(&new_value, uv);
            new_value.obj->calc2(&oper);
            if (new_value.obj == BOOL_OBJ) {
                if (new_value.u.boolean) {
                    if (v == v1) obj_destroy(v);
                    bool_from_int(v, 1);
                    return;
                }
            } else new_value.obj->destroy(&new_value);
        }
        if (v == v1 || v == v2) v->obj->destroy(v);
        bool_from_int(v, 0);
        return;
    }
    switch (v1->obj->type) {
    case T_CODE:
        if (access_check(op->v1, v, &op->epoint)) return;
        if (access_check(op->v2, v, &op->epoint2)) return;
        op->v1 = val_reference(v1->u.code.addr);
        op->v2 = val_reference(v2->u.code.addr);
        if (v1 == v || v2 == v) destroy(v);
        op->v2->obj->rcalc2(op);
        val_destroy(op->v1);
        val_destroy(op->v2);
        op->v1 = v1;
        op->v2 = v2;
        return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
        if (access_check(op->v2, v, &op->epoint2)) return;
        op->v2 = val_reference(v2->u.code.addr);
        switch (op->op->u.oper.op) {
        case O_ADD:
            op->v = val_alloc();
            op->v2->obj->rcalc2(op);
            if (v == v1) v->obj->destroy(v);
            v->obj = CODE_OBJ; 
            if (v2 != v) memcpy(&v->u.code, &v2->u.code, sizeof(v->u.code));
            else val_destroy(v->u.code.addr);
            v->u.code.addr = op->v;
            val_destroy(op->v2);
            op->v = v;
            op->v2 = v2;
            return;
        default: break;
        }
        if (v == v2) v->obj->destroy(v);
        op->v2->obj->rcalc2(op);
        val_destroy(op->v2);
        op->v2 = v2;
        return;
    default: v1->obj->calc2(op); return;
    }
    obj_oper_error(op);
}

static inline void slice(struct value_s *v1, uval_t ln, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t epoint) {
    struct value_s **vals;
    size_t i, i2;
    size_t ln2;
    size_t offs2;
    int16_t r;
    uval_t val;

    if (!ln) {
        TUPLE_OBJ->copy(&null_tuple, v); return;
    }
    if (v1->u.code.pass != pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident = v1->u.code.parent->name;
        return;
    }
    vals = (struct value_s **)malloc(ln * sizeof(v1->u.list.data[0]));
    if (!vals) err_msg_out_of_memory();
    i = 0;
    ln2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    ln2 = ln2 + !ln2;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        offs2 = offs * ln2;
        val = 0;
        r = -1;
        for (i2 = 0; i2 < ln2; i2++) {
            r = read_mem(v1->u.code.mem, v1->u.code.memp, v1->u.code.membp, offs2++);
            if (r < 0) break;
            val |= r << (i2 * 8);
        }
        if (v1->u.code.dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(val); i2++) {
                val |= 0xff << (i2 * 8);
            }
        }
        vals[i] = val_alloc();
        if (r < 0) vals[i]->obj = GAP_OBJ;
        else if (v1->u.code.dtype < 0) int_from_ival(vals[i], (ival_t)val);
        else int_from_uval(vals[i], val);
        i++; offs += step;
    }
    v->obj = TUPLE_OBJ;
    v->u.list.len = ln;
    v->u.list.data = vals;
}

static void iindex(oper_t op) {
    struct value_s **vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs2;
    int16_t r;
    ival_t offs;
    uval_t val;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v, err;

    if (v1->u.code.pass != pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = op->epoint;
        v->u.error.u.ident = v1->u.code.parent->name;
        return;
    }

    ln2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    ln2 = ln2 + !ln2;
    ln = v1->u.code.size / ln2;

    if (v2->obj == TUPLE_OBJ || v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            TUPLE_OBJ->copy(&null_tuple, v); return;
        }
        vals = (struct value_s **)malloc(v2->u.list.len * sizeof(v2->u.list.data[0]));
        if (!vals) err_msg_out_of_memory();
        for (i = 0; i < v2->u.list.len; i++) {
            offs = indexoffs(v2->u.list.data[i], &err, ln, &op->epoint2);
            if (offs < 0) {
                v->u.list.data = vals;
                v->u.list.len = i;
                TUPLE_OBJ->destroy(v);
                err.obj->copy_temp(&err, v);
                return;
            }
            offs2 = offs * ln2;
            val = 0;
            r = -1;
            for (i2 = 0; i2 < ln2; i2++) {
                r = read_mem(v1->u.code.mem, v1->u.code.memp, v1->u.code.membp, offs2++);
                if (r < 0) break;
                val |= r << (i2 * 8);
            }
            if (v1->u.code.dtype < 0 && (r & 0x80)) {
                for (; i2 < sizeof(val); i2++) {
                    val |= 0xff << (i2 * 8);
                }
            }
            vals[i] = val_alloc();
            if (r < 0) vals[i]->obj = GAP_OBJ;
            else if (v1->u.code.dtype < 0) int_from_ival(vals[i],  (ival_t)val);
            else int_from_uval(vals[i], val);
        }
        v->obj = TUPLE_OBJ;
        v->u.list.data = vals;
        v->u.list.len = i;
        return;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t len, end, step;
        len = sliceparams(op, ln, &offs, &end, &step);
        if (len < 0) return;
        return slice(v1, len, offs, end, step, v, &op->epoint);
    }
    offs = indexoffs(v2, &err, ln, &op->epoint2);
    if (offs < 0) {
        err.obj->copy_temp(&err, v);
        return;
    }

    offs2 = offs * ln2;
    val = 0;
    r = -1;
    for (i2 = 0; i2 < ln2; i2++) {
        r = read_mem(v1->u.code.mem, v1->u.code.memp, v1->u.code.membp, offs2++);
        if (r < 0) break;
        val |= r << (i2 * 8);
    }
    if (v1->u.code.dtype < 0 && (r & 0x80)) {
        for (; i2 < sizeof(val); i2++) {
            val |= 0xff << (i2 * 8);
        }
    }
    if (r < 0) v->obj = GAP_OBJ;
    else if (v1->u.code.dtype < 0) int_from_ival(v,  (ival_t)val);
    else int_from_uval(v, val);
}

void codeobj_init(void) {
    obj_init(&obj, T_CODE, "<code>");
    obj.destroy = destroy;
    obj.copy = copy;
    obj.same = same;
    obj.truth = truth;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.real = real;
    obj.sign = sign;
    obj.abs = absolute;
    obj.integer = integer;
    obj.len = len;
    obj.size = size;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.iindex = iindex;
}
