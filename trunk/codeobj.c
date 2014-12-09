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
#include "floatobj.h"

static struct obj_s obj;

obj_t CODE_OBJ = &obj;

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_CODE: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static void destroy(value_t v1) {
    val_destroy(v1->u.code.addr);
    if (v1->u.code.label->parent == NULL) label_destroy(v1->u.code.label);
}

static MUST_CHECK value_t access_check(const value_t v1, linepos_t epoint) {
    if (v1->u.code.label->requires & ~current_section->provides) {
        value_t v = new_error_obj(ERROR_REQUIREMENTS_, epoint);
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }
    if (v1->u.code.label->conflicts & current_section->provides) {
        value_t v = new_error_obj(ERROR______CONFLICT, epoint);
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }
    return NULL;
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == CODE_OBJ && (v1->u.code.addr == v2->u.code.addr || obj_same(v1->u.code.addr, v2->u.code.addr)) && v1->u.code.size == v2->u.code.size && v1->u.code.dtype == v2->u.code.dtype && v1->u.code.label == v2->u.code.label;
}

static MUST_CHECK value_t truth(value_t v1, enum truth_e type, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->truth(v1, type, epoint);
}

static MUST_CHECK value_t repr(value_t v1, linepos_t epoint) {
    if (epoint) {
        value_t err = access_check(v1, epoint);
        if (err) return err;
    }
    v1 = v1->u.code.addr;
    return v1->obj->repr(v1, epoint);
}

static MUST_CHECK value_t ival(value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->ival(v1, iv, bits, epoint);
}

static MUST_CHECK value_t uval(value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->uval(v1, uv, bits, epoint);
}

MUST_CHECK value_t float_from_code(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    return FLOAT_OBJ->create(v1->u.code.addr, epoint);
}

static MUST_CHECK value_t sign(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->sign(v1, epoint);
}

static MUST_CHECK value_t absolute(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    v1 = v1->u.code.addr;
    return v1->obj->abs(v1, epoint);
}

MUST_CHECK value_t int_from_code(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    return INT_OBJ->create(v1->u.code.addr, epoint);
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    if (!v1->u.code.pass) {
        return val_reference(none_value);
    }
    return int_from_size(v1->u.code.size / (abs(v1->u.code.dtype) + !v1->u.code.dtype));
}

static MUST_CHECK value_t size(const value_t v1, linepos_t UNUSED(epoint)) {
    if (!v1->u.code.pass) {
        return val_reference(none_value);
    }
    return int_from_size(v1->u.code.size);
}

MUST_CHECK value_t bits_from_code(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    return BITS_OBJ->create(v1->u.code.addr, epoint);
}

MUST_CHECK value_t bytes_from_code(value_t v1, linepos_t epoint) {
    value_t err = access_check(v1, epoint);
    if (err) return err;
    return BYTES_OBJ->create(v1->u.code.addr, epoint);
}

MUST_CHECK value_t tuple_from_code(const value_t v1, obj_t typ, linepos_t epoint) {
    size_t ln, ln2, i, i2, offs2;
    value_t v, *vals;
    int16_t r;
    uval_t val;

    if (v1->u.code.pass != pass) {
        v = new_error_obj(ERROR____NO_FORWARD, epoint);
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }

    ln2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    ln2 = ln2 + !ln2;
    ln = v1->u.code.size / ln2;

    if (!ln) {
        return val_reference(typ == TUPLE_OBJ ? null_tuple : null_list);
    }

    v = val_alloc(typ);
    v->u.list.len = ln;
    v->u.list.data = vals = list_create_elements(v, ln);
    i = 0;
    while (ln > i) {
        offs2 = i * ln2;
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
        i++;
    }
    return v;
}

static inline MUST_CHECK value_t slice(value_t v2, oper_t op, size_t ln) {
    value_t *vals, v1 = op->v1, v;
    size_t i, i2;
    size_t ln2;
    size_t offs2;
    int16_t r;
    uval_t val;
    size_t length;
    ival_t offs, end, step;

    v = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (v) return v;

    if (!length) {
        return val_reference(null_tuple);
    }
    if (v1->u.code.pass != pass) {
        v = new_error_obj(ERROR____NO_FORWARD, op->epoint);
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }
    v = val_alloc(TUPLE_OBJ);
    vals = list_create_elements(v, length);
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
    v->u.list.len = length;
    v->u.list.data = vals;
    return v;
}

static inline MUST_CHECK value_t iindex(oper_t op) {
    value_t *vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs, offs2;
    int16_t r;
    uval_t val;
    value_t v1 = op->v1, v2 = op->v2, v, err;

    if (v2->u.funcargs.len != 1) {
        err_msg_argnum(v2->u.funcargs.len, 1, 1, op->epoint2);
        return val_reference(none_value);
    }
    v2 = v2->u.funcargs.val->val;

    if (v1->u.code.pass != pass) {
        v = new_error_obj(ERROR____NO_FORWARD, op->epoint);
        v->u.error.u.ident = v1->u.code.label->name;
        return v;
    }

    ln2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    ln2 = ln2 + !ln2;
    ln = v1->u.code.size / ln2;

    if (v2->obj == LIST_OBJ) {
        int error;
        if (!v2->u.list.len) {
            return val_reference(null_tuple);
        }
        v = val_alloc(TUPLE_OBJ);
        v->u.list.data = vals = list_create_elements(v, v2->u.list.len);
        error = 1;
        for (i = 0; i < v2->u.list.len; i++) {
            err = indexoffs(v2->u.list.data[i], ln, &offs, op->epoint2);
            if (err) {
                if (error) {err_msg_output(err); error = 0;} 
                val_destroy(err);
                vals[i] = val_reference(none_value);
                continue;
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
        return slice(v2, op, ln);
    }
    err = indexoffs(v2, ln, &offs, op->epoint2);
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

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v, v1 = op->v1;
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
        v = val_alloc(CODE_OBJ);
        memcpy(&v->u.code, &v1->u.code, sizeof(v->u.code));
        v->u.code.addr = op->v1->obj->calc1(op);
        if (v->u.code.addr->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.code.addr); v->u.code.addr = val_reference(none_value); }
        op->v1 = v1;
        return v;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v, err;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
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
            v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.ident.epoint);
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.ident = v2->u.ident.name;
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
                count = v2->u.anonident.count;
                v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.anonident.epoint);
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
    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return obj_oper_error(op);
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
    case T_FLOAT:
    case T_STR:
    case T_BYTES:
        err = access_check(op->v1, op->epoint);
        if (err) return err;
        op->v1 = v1->u.code.addr;
        switch (op->op->u.oper.op) {
        case O_ADD:
        case O_SUB:
            v = val_alloc(CODE_OBJ); 
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

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v, err;
    if (op->op == &o_IN) {
        struct oper_s oper;
        size_t i, ln, offs;
        int16_t r;
        uval_t uv;

        if (v2->u.code.pass != pass) {
            v = new_error_obj(ERROR____NO_FORWARD, op->epoint2);
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
            value_t tmp;
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
        return val_reference(false_value);
    }
    switch (v1->obj->type) {
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
    case T_FLOAT:
        err = access_check(op->v2, op->epoint2);
        if (err) return err;
        op->v2 = v2->u.code.addr;
        switch (op->op->u.oper.op) {
        case O_ADD:
            v = val_alloc(CODE_OBJ); 
            memcpy(&v->u.code, &v2->u.code, sizeof(v->u.code));
            v->u.code.addr = op->v1->obj->calc2(op);
            if (v->u.code.addr->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.code.addr); v->u.code.addr = val_reference(none_value); }
            op->v2 = v2;
            return v;
        default: break;
        }
        v = op->v1->obj->calc2(op);
        op->v2 = v2;
        return v;
    default: break;
    }
    return obj_oper_error(op);
}

void codeobj_init(void) {
    obj_init(&obj, T_CODE, "code");
    obj.create = create;
    obj.destroy = destroy;
    obj.same = same;
    obj.truth = truth;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.sign = sign;
    obj.abs = absolute;
    obj.len = len;
    obj.size = size;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
