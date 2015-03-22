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
#include "codeobj.h"
#include "eval.h"
#include "mem.h"
#include "64tass.h"
#include "section.h"
#include "variables.h"
#include "error.h"
#include "values.h"

#include "boolobj.h"
#include "floatobj.h"
#include "namespaceobj.h"
#include "listobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "bytesobj.h"
#include "operobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type obj;

Type *CODE_OBJ = &obj;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_CODE: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static void destroy(Obj *o1) {
    Code *v1 = (Code *)o1;
    val_destroy(v1->addr);
    val_destroy(&v1->names->v);
}

static void garbage(Obj *o1, int i) {
    Code *v1 = (Code *)o1;
    Obj *v;
    switch (i) {
    case -1:
        v1->addr->refcount--;
        v1->names->v.refcount--;
        return;
    case 0:
        return;
    case 1:
        v = v1->addr;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        v = &v1->names->v;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static MUST_CHECK Error *access_check(const Code *v1, linepos_t epoint) {
    if (v1->requires & ~current_section->provides) {
        return new_error(ERROR_REQUIREMENTS_, epoint);
    }
    if (v1->conflicts & current_section->provides) {
        return new_error(ERROR______CONFLICT, epoint);
    }
    return NULL;
}

static int same(const Obj *o1, const Obj *o2) {
    const Code *v1 = (const Code *)o1, *v2 = (const Code *)o2;
    return o2->obj == CODE_OBJ && (v1->addr == v2->addr || v1->addr->obj->same(v1->addr, v2->addr))
        && v1->size == v2->size && v1->dtype == v2->dtype
        && v1->requires == v2->requires && v1->conflicts == v2->conflicts
        && (v1->names == v2->names || v1->names->v.obj->same(&v1->names->v, &v2->names->v));
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    v = v1->addr;
    return v->obj->truth(v, type, epoint);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    if (epoint) {
        Error *err = access_check(v1, epoint);
        if (err) return &err->v;
    }
    v = v1->addr;
    return v->obj->repr(v, epoint);
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, int bits, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    Error *err = access_check(v1, epoint);
    if (err) return err;
    v = v1->addr;
    return v->obj->ival(v, iv, bits, epoint);
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, int bits, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    Error *err = access_check(v1, epoint);
    if (err) return err;
    v = v1->addr;
    return v->obj->uval(v, uv, bits, epoint);
}

static MUST_CHECK Error *address(Obj *o1, uval_t *uv, int bits, uint32_t *am, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    if (uv) {
        Error *err = access_check(v1, epoint);
        if (err) return err;
    }
    v = v1->addr;
    return v->obj->address(v, uv, bits, am, epoint);
}

MUST_CHECK Obj *float_from_code(Code *v1, linepos_t epoint) {
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    return FLOAT_OBJ->create(v1->addr, epoint);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    v = v1->addr;
    return v->obj->sign(v, epoint);
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t epoint) {
    Code *v1 = (Code *)o1;
    Obj *v;
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    v = v1->addr;
    return v->obj->abs(v, epoint);
}

MUST_CHECK Obj *int_from_code(Code *v1, linepos_t epoint) {
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    return INT_OBJ->create(v1->addr, epoint);
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    Code *v1 = (Code *)o1;
    if (!v1->pass) {
        return (Obj *)ref_none();
    }
    return (Obj *)int_from_size(v1->size / (abs(v1->dtype) + !v1->dtype));
}

static MUST_CHECK Obj *size(Obj *o1, linepos_t UNUSED(epoint)) {
    Code *v1 = (Code *)o1;
    if (!v1->pass) {
        return (Obj *)ref_none();
    }
    return (Obj *)int_from_size(v1->size);
}

MUST_CHECK Obj *bits_from_code(Code *v1, linepos_t epoint) {
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    return BITS_OBJ->create(v1->addr, epoint);
}

MUST_CHECK Obj *bytes_from_code(Code *v1, linepos_t epoint) {
    Error *err = access_check(v1, epoint);
    if (err) return &err->v;
    return BYTES_OBJ->create(v1->addr, epoint);
}

MUST_CHECK Obj *tuple_from_code(const Code *v1, Type *typ, linepos_t epoint) {
    size_t ln, ln2, i, i2, offs2;
    List *v;
    Obj **vals;
    int16_t r;
    uval_t val;

    if (v1->pass != pass) {
        return (Obj *)new_error(ERROR____NO_FORWARD, epoint);
    }

    ln2 = (v1->dtype < 0) ? -v1->dtype : v1->dtype;
    ln2 = ln2 + !ln2;
    ln = v1->size / ln2;

    if (!ln) {
        return val_reference(typ == TUPLE_OBJ ? &null_tuple->v : &null_list->v);
    }

    v = (List *)val_alloc(typ);
    v->len = ln;
    v->data = vals = list_create_elements(v, ln);
    i = 0;
    while (ln > i) {
        offs2 = i * ln2;
        val = 0;
        r = -1;
        for (i2 = 0; i2 < ln2; i2++) {
            r = read_mem(v1->mem, v1->memp, v1->membp, offs2++);
            if (r < 0) break;
            val |= r << (i2 * 8);
        }
        if (v1->dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(val); i2++) {
                val |= 0xff << (i2 * 8);
            }
        }
        if (r < 0) vals[i] = (Obj *)ref_gap();
        else if (v1->dtype < 0) vals[i] = (Obj *)int_from_ival((ival_t)val);
        else vals[i] = (Obj *)int_from_uval(val);
        i++;
    }
    return &v->v;
}

static inline MUST_CHECK Obj *slice(Colonlist *v2, oper_t op, size_t ln) {
    Obj **vals;
    Tuple *v;
    Code *v1 = (Code *)op->v1;
    size_t i, i2;
    size_t ln2;
    size_t offs2;
    int16_t r;
    uval_t val;
    size_t length;
    ival_t offs, end, step;
    Obj *err;

    err = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (err) return err;

    if (!length) {
        return (Obj *)ref_tuple(null_tuple);
    }
    if (v1->pass != pass) {
        return (Obj *)new_error(ERROR____NO_FORWARD, op->epoint);
    }
    v = new_tuple();
    vals = list_create_elements(v, length);
    i = 0;
    ln2 = (v1->dtype < 0) ? -v1->dtype : v1->dtype;
    ln2 = ln2 + !ln2;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        offs2 = offs * ln2;
        val = 0;
        r = -1;
        for (i2 = 0; i2 < ln2; i2++) {
            r = read_mem(v1->mem, v1->memp, v1->membp, offs2++);
            if (r < 0) break;
            val |= r << (i2 * 8);
        }
        if (v1->dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(val); i2++) {
                val |= 0xff << (i2 * 8);
            }
        }
        if (r < 0) vals[i] = (Obj *)ref_gap();
        else if (v1->dtype < 0) vals[i] = (Obj *)int_from_ival((ival_t)val);
        else vals[i] = (Obj *)int_from_uval(val);
        i++; offs += step;
    }
    v->len = length;
    v->data = vals;
    return &v->v;
}

static inline MUST_CHECK Obj *iindex(oper_t op) {
    Obj **vals;
    size_t i, i2;
    size_t ln, ln2;
    size_t offs, offs2;
    int16_t r;
    uval_t val;
    Code *v1 = (Code *)op->v1;
    Obj *o2 = op->v2;
    Error *err;
    Funcargs *args = (Funcargs *)o2;

    if (args->len != 1) {
        err_msg_argnum(args->len, 1, 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val->val;

    if (v1->pass != pass) {
        return (Obj *)new_error(ERROR____NO_FORWARD, op->epoint);
    }

    ln2 = (v1->dtype < 0) ? -v1->dtype : v1->dtype;
    ln2 = ln2 + !ln2;
    ln = v1->size / ln2;

    if (o2->obj == LIST_OBJ) {
        List *list = (List *)o2;
        Tuple *v;
        int error;
        if (!list->len) {
            return (Obj *)ref_tuple(null_tuple);
        }
        v = new_tuple();
        v->data = vals = list_create_elements(v, list->len);
        error = 1;
        for (i = 0; i < list->len; i++) {
            err = indexoffs(list->data[i], ln, &offs, op->epoint2);
            if (err) {
                if (error) {err_msg_output(err); error = 0;} 
                val_destroy(&err->v);
                vals[i] = (Obj *)ref_none();
                continue;
            }
            offs2 = offs * ln2;
            val = 0;
            r = -1;
            for (i2 = 0; i2 < ln2; i2++) {
                r = read_mem(v1->mem, v1->memp, v1->membp, offs2++);
                if (r < 0) break;
                val |= r << (i2 * 8);
            }
            if (v1->dtype < 0 && (r & 0x80)) {
                for (; i2 < sizeof(val); i2++) {
                    val |= 0xff << (i2 * 8);
                }
            }
            if (r < 0) vals[i] = (Obj *)ref_gap();
            else if (v1->dtype < 0) vals[i] = (Obj *)int_from_ival((ival_t)val);
            else vals[i] = (Obj *)int_from_uval(val);
        }
        v->len = i;
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        return slice((Colonlist *)o2, op, ln);
    }
    err = indexoffs(o2, ln, &offs, op->epoint2);
    if (err) return &err->v;

    offs2 = offs * ln2;
    val = 0;
    r = -1;
    for (i2 = 0; i2 < ln2; i2++) {
        r = read_mem(v1->mem, v1->memp, v1->membp, offs2++);
        if (r < 0) break;
        val |= r << (i2 * 8);
    }
    if (v1->dtype < 0 && (r & 0x80)) {
        for (; i2 < sizeof(val); i2++) {
            val |= 0xff << (i2 * 8);
        }
    }
    if (r < 0) return (Obj *)ref_gap();
    if (v1->dtype < 0) return (Obj *)int_from_ival((ival_t)val);
    return (Obj *)int_from_uval(val);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Code *v, *v1 = (Code *)op->v1;
    switch (op->op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_STRING:
        op->v1 = v1->addr;
        v = (Code *)op->v1->obj->calc1(op);
        op->v1 = &v1->v;
        return &v->v;
    case O_INV:
    case O_NEG:
    case O_POS:
        op->v1 = v1->addr;
        v = new_code();
        memcpy(((unsigned char *)v) + sizeof(Obj), ((unsigned char *)v1) + sizeof(Obj), sizeof(Code) - sizeof(Obj));
        v->names = ref_namespace(v1->names);
        v->addr = op->v1->obj->calc1(op);
        if (v->addr->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)v->addr); v->addr = (Obj *)ref_none(); }
        op->v1 = &v1->v;
        return &v->v;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Code *v1 = (Code *)op->v1, *v;
    Obj *o2 = op->v2;
    Error *err;
    if (op->op == &o_MEMBER) {
        return namespace_member(op, v1->names);
    }
    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return obj_oper_error(op);
    }
    switch (o2->obj->type) {
    case T_CODE:
        {
            Code *v2 = (Code *)o2;
            err = access_check(v1, op->epoint);
            if (err) return &err->v;
            err = access_check(v2, op->epoint2);
            if (err) return &err->v;
            op->v1 = v1->addr;
            op->v2 = v2->addr;
            v = (Code *)op->v1->obj->calc2(op);
            op->v1 = &v1->v;
            op->v2 = &v2->v;
            return &v->v;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_STR:
    case T_BYTES:
        err = access_check(v1, op->epoint);
        if (err) return &err->v;
        op->v1 = v1->addr;
        switch (op->op->op) {
        case O_ADD:
        case O_SUB:
            v = new_code();
            memcpy(((unsigned char *)v) + sizeof(Obj), ((unsigned char *)v1) + sizeof(Obj), sizeof(Code) - sizeof(Obj));
            v->names = ref_namespace(v1->names);
            v->addr = op->v1->obj->calc2(op);
            if (v->addr->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)v->addr); v->addr = (Obj *)ref_none(); }
            op->v1 = &v1->v;
            return &v->v;
        default: break;
        }
        v = (Code *)op->v1->obj->calc2(op);
        op->v1 = &v1->v;
        return &v->v;
    default: return o2->obj->rcalc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1;
    Code *v2 = (Code *)op->v2, *v;
    Error *err;
    if (op->op == &o_IN) {
        struct oper_s oper;
        size_t i, ln, offs;
        int16_t r;
        uval_t uv;

        if (v2->pass != pass) {
            return (Obj *)new_error(ERROR____NO_FORWARD, op->epoint2);
        }
        ln = (v2->dtype < 0) ? -v2->dtype : v2->dtype;
        ln = ln + !ln;
        oper.op = &o_EQ;
        oper.v2 = o1;
        oper.epoint = op->epoint;
        oper.epoint2 = op->epoint2;
        oper.epoint3 = op->epoint3;
        for (offs = 0; offs < v2->size;) {
            Obj *tmp;
            uv = 0;
            r = -1;
            for (i = 0; i < ln; i++) {
                r = read_mem(v2->mem, v2->memp, v2->membp, offs++);
                if (r < 0) break;
                uv |= r << (i * 8);
            }
            if (v2->dtype < 0 && (r & 0x80)) {
                for (; i < sizeof(uv); i++) {
                    uv |= 0xff << (i * 8);
                }
            }
            if (r < 0) tmp = (Obj *)ref_gap();
            else if (v2->dtype < 0) tmp = (Obj *)int_from_ival((ival_t)uv);
            else tmp = (Obj *)int_from_uval(uv);
            oper.v1 = tmp;
            o1 = tmp->obj->calc2(&oper);
            val_destroy(tmp);
            if (o1->obj == BOOL_OBJ && ((Bool *)o1)->boolean) return o1;
            val_destroy(o1);
        }
        return (Obj *)ref_bool(false_value);
    }
    switch (o1->obj->type) {
    case T_CODE:
        {
            Code *v1 = (Code *)o1;
            err = access_check(v1, op->epoint);
            if (err) return &err->v;
            err = access_check(v2, op->epoint2);
            if (err) return &err->v;
            op->v1 = v1->addr;
            op->v2 = v2->addr;
            v = (Code *)op->v1->obj->calc2(op);
            op->v1 = &v1->v;
            op->v2 = &v2->v;
            return &v->v;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
        err = access_check(v2, op->epoint2);
        if (err) return &err->v;
        op->v2 = v2->addr;
        switch (op->op->op) {
        case O_ADD:
            v = new_code();
            memcpy(((unsigned char *)v) + sizeof(Obj), ((unsigned char *)v2) + sizeof(Obj), sizeof(Code) - sizeof(Obj));
            v->names = ref_namespace(v2->names);
            v->addr = op->v1->obj->calc2(op);
            if (v->addr->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)v->addr); v->addr = (Obj *)ref_none(); }
            op->v2 = &v2->v;
            return &v->v;
        default: break;
        }
        v = (Code *)op->v1->obj->calc2(op);
        op->v2 = &v2->v;
        return &v->v;
    default: break;
    }
    return obj_oper_error(op);
}

void codeobj_init(void) {
    new_type(&obj, T_CODE, "code", sizeof(Code));
    obj_init(&obj);
    obj.create = create;
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.truth = truth;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.address = address;
    obj.sign = sign;
    obj.abs = absolute;
    obj.len = len;
    obj.size = size;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}

void codeobj_names(void) {
    new_builtin("code", val_reference(&CODE_OBJ->v));
}
