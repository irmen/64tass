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

static int access_check(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (pass != 1) {
        if (v1->u.code.parent->requires & ~current_section->provides) {
            v->u.error.u.ident = v1->u.code.parent->name;
            v->obj = ERROR_OBJ;
            v->u.error.epoint = *epoint;
            v->u.error.num = ERROR_REQUIREMENTS_;
            return 1;
        }
        if (v1->u.code.parent->conflicts & current_section->provides) {
            v->u.error.u.ident = v1->u.code.parent->name;
            v->obj = ERROR_OBJ;
            v->u.error.epoint = *epoint;
            v->u.error.num = ERROR______CONFLICT;
            return 1;
        }
    }
    return 0;
}

static void destroy(struct value_s *v1) {
    val_destroy(v1->u.code.addr);
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

static int MUST_CHECK truth(const struct value_s *v1, struct value_s *v, int *result, enum truth_e type, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    return v1->u.code.addr->obj->truth(v1->u.code.addr, v, result, type, epoint);
}

static void repr(const struct value_s *v1, struct value_s *v) {
    return v1->u.code.addr->obj->repr(v1->u.code.addr, v);
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    return v1->u.code.addr->obj->ival(v1->u.code.addr, v, iv, bits, epoint);
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    return v1->u.code.addr->obj->uval(v1->u.code.addr, v, uv, bits, epoint);
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    return v1->u.code.addr->obj->real(v1->u.code.addr, v, r, epoint);
}

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *v, int *s, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return 1;
    return v1->u.code.addr->obj->sign(v1->u.code.addr, v, s, epoint);
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return;
    return v1->u.code.addr->obj->abs(v1->u.code.addr, v, epoint);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (access_check(v1, v, epoint)) return;
    return v1->u.code.addr->obj->integer(v1->u.code.addr, v, epoint);
}

static int MUST_CHECK len(const struct value_s *v1, struct value_s *v, uval_t *uv, linepos_t epoint) {
    if (!v1->u.code.pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident = v1->u.code.parent->name;
        return 1;
    }
    *uv = v1->u.code.size / (abs(v1->u.code.dtype) + !v1->u.code.dtype);
    return 0;
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
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.code.parent;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                l->value->obj->copy(l->value, op->v);
                return;
            } 
            epoint = v2->u.ident.epoint;
            v->u.error.u.notdef.ident = v2->u.ident.name;
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.down = 0;
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint;
            return;
        case T_ANONIDENT:
            {
                char idents[100];
                str_t ident;
                sprintf(idents, (v2->u.anonident.count >= 0) ? "+%x+%x" : "-%x-%x" , reffile, ((v2->u.anonident.count >= 0) ? forwr : backr) + v2->u.anonident.count);
                ident.data = (const uint8_t *)idents;
                ident.len = strlen(idents);
                l2 = v1->u.code.parent;
                l = find_label2(&ident, l2);
                if (l) {
                    touch_label(l);
                    l->value->obj->copy(l->value, op->v);
                    return;
                }
                v->u.error.epoint = v2->u.anonident.epoint;
                v->u.error.u.notdef.label = l2;
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___NOT_DEFINED;
                v->u.error.u.notdef.ident.len = 1;
                v->u.error.u.notdef.ident.data = (const uint8_t *)((v2->u.anonident.count >= 0) ? "+" : "-");
                v->u.error.u.notdef.down = 0;
                return;
            }
        case T_TUPLE:
        case T_LIST: v2->obj->rcalc2(op); return;
        default: v2->obj->rcalc2(op); return;
        }
    }
    switch (v2->obj->type) {
    case T_CODE:
        if (access_check(op->v1, v, &op->epoint)) return;
        if (access_check(op->v2, v, &op->epoint2)) return;
        op->v1 = val_reference(v1->u.code.addr);
        op->v2 = val_reference(v2->u.code.addr);
        if (v1 == v || v2 == v) v->obj->destroy(v);
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
        if (v == v1 || v == v2) v->obj->destroy(v);
        switch (op->op->u.oper.op) {
        case O_ADD:
        case O_SUB:
            op->v = val_alloc();
            op->v1->obj->calc2(op);
            v->obj = CODE_OBJ; 
            if (v != v1) memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
            v->u.code.addr = op->v;
            val_destroy(op->v1);
            op->v = v;
            op->v1 = v1;
            return;
        default: break;
        }
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
            if (new_value.obj == BOOL_OBJ && new_value.u.boolean) {
                if (v == v1) obj_destroy(v);
                bool_from_int(v, 1);
                return;
            }
        }
        if (v == v1) obj_destroy(v);
        bool_from_int(v, 0);
        return;
    }
    switch (v1->obj->type) {
    case T_CODE:
        if (access_check(op->v1, v, &op->epoint)) return;
        if (access_check(op->v2, v, &op->epoint2)) return;
        op->v1 = val_reference(v1->u.code.addr);
        op->v2 = val_reference(v2->u.code.addr);
        if (v1 == v || v2 == v) v->obj->destroy(v);
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
        if (v == v1 || v == v2) v->obj->destroy(v);
        switch (op->op->u.oper.op) {
        case O_ADD:
            op->v = val_alloc();
            op->v2->obj->rcalc2(op);
            v->obj = CODE_OBJ; 
            if (v2 != v) memcpy(&v->u.code, &v2->u.code, sizeof(v->u.code));
            v->u.code.addr = op->v;
            val_destroy(op->v2);
            op->v = v;
            op->v2 = v2;
            return;
        default: break;
        }
        op->v2->obj->rcalc2(op);
        val_destroy(op->v2);
        op->v2 = v2;
        return;
    default: v1->obj->calc2(op); return;
    }
    obj_oper_error(op);
}

static void iindex(oper_t op) {
    struct value_s **vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs2;
    int16_t r;
    ival_t offs;
    uval_t val;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

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
            offs = indexoffs(v2->u.list.data[i], ln);
            if (offs < 0) {
                v->u.list.data = vals;
                v->u.list.len = i;
                TUPLE_OBJ->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___INDEX_RANGE;
                v->u.error.epoint = op->epoint2;
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
    offs = indexoffs(v2, ln);
    if (offs < 0) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR___INDEX_RANGE;
        v->u.error.epoint = op->epoint2;
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

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t epoint) {
    struct value_s **vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs2;
    int16_t r;
    uval_t val;

    if (step > 0) {
        if (end < offs) end = offs;
        ln = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        ln = (offs - end - step - 1) / -step;
    }

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
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.iindex = iindex;
    obj.slice = slice;
}
