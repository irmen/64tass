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
    if (v1->u.code.label->parent == NULL) label_destroy(v1->u.code.label);
}

static MUST_CHECK struct value_s *access_check(const struct value_s *v1, linepos_t epoint) {
    if (v1->u.code.label->requires & ~current_section->provides) {
        struct value_s *v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.u.ident = v1->u.code.label->name;
        v->u.error.epoint = *epoint;
        v->u.error.num = ERROR_REQUIREMENTS_;
        return v;
    }
    if (v1->u.code.label->conflicts & current_section->provides) {
        struct value_s *v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.u.ident = v1->u.code.label->name;
        v->u.error.epoint = *epoint;
        v->u.error.num = ERROR______CONFLICT;
        return v;
    }
    return NULL;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = CODE_OBJ;
    memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
    v->u.code.addr = val_reference(v1->u.code.addr);
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == CODE_OBJ && (v1->u.code.addr == v2->u.code.addr || obj_same(v1->u.code.addr, v2->u.code.addr)) && v1->u.code.size == v2->u.code.size && v1->u.code.dtype == v2->u.code.dtype && v1->u.code.label == v2->u.code.label;
}

static MUST_CHECK struct value_s *truth(const struct value_s *v1, enum truth_e type, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->truth(v1, type, epoint);
}

static MUST_CHECK struct value_s *repr(const struct value_s *v1, linepos_t epoint) {
    if (epoint) {
        struct value_s *err = access_check(v1, epoint);
        if (err) return err;
    }
    v1 = v1->u.code.addr;
    return v1->obj->repr(v1, epoint);
}

static MUST_CHECK struct value_s *MUST_CHECK ival(const struct value_s *v1, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->ival(v1, iv, bits, epoint);
}

static MUST_CHECK struct value_s *MUST_CHECK uval(const struct value_s *v1, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->uval(v1, uv, bits, epoint);
}

static MUST_CHECK struct value_s *MUST_CHECK real(const struct value_s *v1, double *r, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->real(v1, r, epoint);
}

static MUST_CHECK struct value_s *sign(const struct value_s *v1, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->sign(v1, epoint);
}

static MUST_CHECK struct value_s *absolute(const struct value_s *v1, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->abs(v1, epoint);
}

static MUST_CHECK struct value_s *integer(const struct value_s *v1, linepos_t epoint) {
    struct value_s *err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->integer(v1, epoint);
}

static MUST_CHECK struct value_s *len(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    if (!v1->u.code.pass) {
        return val_reference(none_value);
    }
    return int_from_uval(v1->u.code.size / (abs(v1->u.code.dtype) + !v1->u.code.dtype));
}

static MUST_CHECK struct value_s *size(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    if (!v1->u.code.pass) {
        return val_reference(none_value);
    }
    return int_from_uval(v1->u.code.size);
}

static MUST_CHECK struct value_s *calc1(oper_t op) {
    struct value_s *v, *v1 = op->v1;
    switch (op->op->u.oper.op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_STRING:
        op->v1 = v1->u.code.addr;
        v = op->v1->obj->calc1(op);
        op->v1 = v1;
        return v;
    case O_INV:
    case O_NEG:
    case O_POS:
        op->v1 = v1->u.code.addr;
        v = val_alloc();
        v->obj = CODE_OBJ; 
        memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
        v->u.code.addr = op->v1->obj->calc1(op);
        if (v->u.code.addr->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.code.addr); v->u.code.addr = val_reference(none_value); }
        op->v1 = v1;
        return v;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v, *err;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
        struct linepos_s epoint;
        str_t name;
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.code.label;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                return val_reference(l->value);
            } 
            if (!referenceit) {
                return val_reference(none_value);
            }
            epoint = v2->u.ident.epoint;
            name = v2->u.ident.name;
            v = val_alloc();
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint;
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.ident = name;
            v->u.error.u.notdef.down = 0;
            return v;
        case T_ANONIDENT:
            {
                ssize_t count;
                l2 = v1->u.code.label;
                l = find_anonlabel2(v2->u.anonident.count, l2);
                if (l) {
                    touch_label(l);
                    return val_reference(l->value);
                }
                if (!referenceit) {
                    return val_reference(none_value);
                }
                epoint = v2->u.anonident.epoint;
                count = v2->u.anonident.count;
                v = val_alloc();
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___NOT_DEFINED;
                v->u.error.epoint = epoint;
                v->u.error.u.notdef.label = l2;
                v->u.error.u.notdef.ident.len = count + (count >= 0);
                v->u.error.u.notdef.ident.data = NULL;
                v->u.error.u.notdef.down = 0;
                return v;
            }
        case T_TUPLE:
        case T_LIST: return v2->obj->rcalc2(op);
        default: return obj_oper_error(op);
        }
    }
    switch (v2->obj->type) {
    case T_CODE:
        err = access_check(op->v1, op->epoint);
        if (err) return err;
        err = access_check(op->v2, op->epoint2);
        if (err) return err;
        op->v1 = v1->u.code.addr;
        op->v2 = v2->u.code.addr;
        v = op->v1->obj->calc2(op);
        op->v1 = v1;
        op->v2 = v2;
        return v;
    case T_BOOL:
    case T_INT:
    case T_BITS:
        err = access_check(op->v1, op->epoint);
        if (err) return err;
        op->v1 = v1->u.code.addr;
        switch (op->op->u.oper.op) {
        case O_ADD:
        case O_SUB:
            v = val_alloc(); 
            v->obj = CODE_OBJ; 
            memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
            v->u.code.addr = op->v1->obj->calc2(op);
            if (v->u.code.addr->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.code.addr); v->u.code.addr = val_reference(none_value); }
            op->v1 = v1;
            return v;
        default: break;
        }
        v = op->v1->obj->calc2(op);
        op->v1 = v1;
        return v;
    default: return v2->obj->rcalc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v, *err;
    if (op->op == &o_IN) {
        struct oper_s oper;
        size_t i, ln, offs;
        int16_t r;
        uval_t uv;

        if (v2->u.code.pass != pass) {
            v = val_alloc();
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR____NO_FORWARD;
            v->u.error.epoint = *op->epoint2;
            v->u.error.u.ident = v2->u.code.label->name;
            return v;
        }
        ln = (v2->u.code.dtype < 0) ? -v2->u.code.dtype : v2->u.code.dtype;
        ln = ln + !ln;
        oper.op = &o_EQ;
        oper.v2 = v1;
        oper.epoint = op->epoint;
        oper.epoint2 = op->epoint2;
        oper.epoint3 = op->epoint3;
        for (offs = 0; offs < v2->u.code.size;) {
            struct value_s *tmp;
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
            if (r < 0) tmp = val_reference(gap_value);
            else if (v2->u.code.dtype < 0) tmp = int_from_ival((ival_t)uv);
            else tmp = int_from_uval(uv);
            oper.v1 = tmp;
            err = tmp->obj->calc2(&oper);
            val_destroy(tmp);
            if (err->obj == BOOL_OBJ && err->u.boolean) return err;
            val_destroy(err);
        }
        return truth_reference(0);
    }
    switch (v1->obj->type) {
    case T_CODE:
        err = access_check(op->v1, op->epoint);
        if (err) return err;
        err = access_check(op->v2, op->epoint2);
        if (err) return err;
        op->v1 = v1->u.code.addr;
        op->v2 = v2->u.code.addr;
        v = op->v2->obj->rcalc2(op);
        op->v1 = v1;
        op->v2 = v2;
        return v;
    case T_BOOL:
    case T_INT:
    case T_BITS:
        err = access_check(op->v2, op->epoint2);
        if (err) return err;
        op->v2 = v2->u.code.addr;
        switch (op->op->u.oper.op) {
        case O_ADD:
            v = val_alloc(); 
            v->obj = CODE_OBJ; 
            memcpy(&v->u.code, &v2->u.code, sizeof(v->u.code));
            v->u.code.addr = op->v2->obj->rcalc2(op);
            if (v->u.code.addr->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.code.addr); v->u.code.addr = val_reference(none_value); }
            op->v2 = v2;
            return v;
        default: break;
        }
        v = op->v2->obj->rcalc2(op);
        op->v2 = v2;
        return v;
    default: return v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK struct value_s *slice(struct value_s *v1, uval_t ln, ival_t offs, ival_t end, ival_t step, linepos_t epoint) {
    struct value_s **vals, *v;
    size_t i, i2;
    size_t ln2;
    size_t offs2;
    int16_t r;
    uval_t val;

    if (!ln) {
        return val_reference(null_tuple);
    }
    v = val_alloc();
    if (v1->u.code.pass != pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }
    vals = list_create_elements(v, ln);
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
        if (r < 0) vals[i] = val_reference(gap_value);
        else if (v1->u.code.dtype < 0) vals[i] = int_from_ival((ival_t)val);
        else vals[i] = int_from_uval(val);
        i++; offs += step;
    }
    v->obj = TUPLE_OBJ;
    v->u.list.len = ln;
    v->u.list.data = vals;
    return NULL;
}

static MUST_CHECK struct value_s *iindex(oper_t op) {
    struct value_s **vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs2;
    int16_t r;
    ival_t offs;
    uval_t val;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v, *err;

    if (v1->u.code.pass != pass) {
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR____NO_FORWARD;
        v->u.error.epoint = *op->epoint;
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }

    ln2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    ln2 = ln2 + !ln2;
    ln = v1->u.code.size / ln2;

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            return val_reference(null_tuple);
        }
        v = val_alloc();
        v->obj = TUPLE_OBJ;
        v->u.list.data = vals = list_create_elements(v, v2->u.list.len);
        for (i = 0; i < v2->u.list.len; i++) {
            err = indexoffs(v2->u.list.data[i], &offs, ln, op->epoint2);
            if (err) {
                v->u.list.len = i;
                val_destroy(v);
                return err;
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
            if (r < 0) vals[i] = val_reference(gap_value);
            else if (v1->u.code.dtype < 0) vals[i] = int_from_ival((ival_t)val);
            else vals[i] = int_from_uval(val);
        }
        v->u.list.len = i;
        return v;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t length, end, step;
        err = sliceparams(op, ln, &length, &offs, &end, &step);
        if (err) return err;
        return slice(v1, length, offs, end, step, op->epoint);
    }
    err = indexoffs(v2, &offs, ln, op->epoint2);
    if (err) return err;

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
    if (r < 0) return val_reference(gap_value);
    if (v1->u.code.dtype < 0) return int_from_ival((ival_t)val);
    return int_from_uval(val);
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
