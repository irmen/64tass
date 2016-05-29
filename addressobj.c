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
#include "addressobj.h"
#include "error.h"
#include "eval.h"
#include "variables.h"

#include "boolobj.h"
#include "strobj.h"
#include "intobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type obj;

Type *ADDRESS_OBJ = &obj;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_ADDRESS: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static void destroy(Obj *o1) {
    Address *v1 = (Address *)o1;
    val_destroy(v1->val);
}

static void garbage(Obj *o1, int i) {
    Address *v1 = (Address *)o1;
    Obj *v;
    switch (i) {
    case -1:
        v1->val->refcount--;
        return;
    case 0:
        return;
    case 1:
        v = v1->val;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static bool same(const Obj *o1, const Obj *o2) {
    const Address *v1 = (const Address *)o1, *v2 = (const Address *)o2;
    return o2->obj == ADDRESS_OBJ && v1->type == v2->type && v1->val->obj->same(v1->val, v2->val);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Address *v1 = (Address *)o1;
    uint8_t *s;
    size_t len, chars;
    char buffer[100], buffer2[100], *b2;
    atype_t addrtype;
    int ind, ind2;
    Obj *tmp;
    Str *v, *str;

    ind2 = 0;
    addrtype = v1->type;
    ind = 99;
    buffer2[ind] = '\0';
    while ((addrtype & 0xfff) != 0) {
        switch ((enum atype_e)((addrtype & 0xf00) >> 8)) {
        case A_NONE:break;
        case A_XR: buffer[ind2++] = ','; buffer[ind2++] = 'x';break;
        case A_YR: buffer[ind2++] = ','; buffer[ind2++] = 'y';break;
        case A_ZR: buffer[ind2++] = ','; buffer[ind2++] = 'z';break;
        case A_SR: buffer[ind2++] = ','; buffer[ind2++] = 's';break;
        case A_RR: buffer[ind2++] = ','; buffer[ind2++] = 'r';break;
        case A_DR: buffer[ind2++] = ','; buffer[ind2++] = 'd';break;
        case A_BR: buffer[ind2++] = ','; buffer[ind2++] = 'b';break;
        case A_KR: buffer[ind2++] = ','; buffer[ind2++] = 'k';break;
        case A_I: buffer2[--ind] = '('; buffer[ind2++] = ')';break;
        case A_LI: buffer2[--ind] = '['; buffer[ind2++] = ']';break;
        case A_IMMEDIATE: buffer2[--ind] = '#';break;
        }
        addrtype <<= 4;
    }
    b2 = buffer2 + ind;
    ind = 99 - ind;

    chars = ind + ind2;
    if (chars > maxsize) return NULL;
    tmp = v1->val->obj->repr(v1->val, epoint, maxsize - chars);
    if (tmp == NULL || tmp->obj != STR_OBJ) return tmp;
    str = (Str *)tmp;
    len = chars + str->len;
    if (len < chars) err_msg_out_of_memory(); /* overflow */
    chars += str->chars;
    if (chars > maxsize) {
        val_destroy(tmp);
        return NULL;
    }

    v = new_str(len);
    v->chars = chars;
    s = v->data;
    if (ind != 0) {
        memcpy(s, b2, ind);
        s += ind;
    }
    if (str->len != 0) {
        memcpy(s, str->data, str->len);
        s += str->len;
    }
    if (ind2 != 0) memcpy(s, buffer, ind2);
    val_destroy(tmp);
    return &v->v;
}

bool check_addr(atype_t type) {
    while (type != A_NONE) {
        switch ((enum atype_e)(type & 0xf)) {
        case A_I:
        case A_LI: return true;
        case A_IMMEDIATE:
        case A_KR:
        case A_DR:
        case A_BR:
        case A_XR:
        case A_YR:
        case A_ZR:
        case A_RR:
        case A_SR:
        case A_NONE: break;
        }
        type >>= 4;
    }
    return false;
}

static inline bool check_addr2(atype_t type) {
    while (type != A_NONE) {
        switch ((enum atype_e)(type & 0xf)) {
        case A_KR:
        case A_DR:
        case A_BR:
        case A_XR:
        case A_YR:
        case A_ZR:
        case A_RR:
        case A_SR:
        case A_I:
        case A_LI: return true;
        case A_IMMEDIATE:
        case A_NONE: break;
        }
        type >>= 4;
    }
    return false;
}

static MUST_CHECK Error *address(Obj *o1, uval_t *uv, unsigned int bits, uint32_t *am, linepos_t epoint) {
    const Address *v1 = (Address *)o1;
    Obj *v = v1->val;
    Error *err;
    err = v->obj->address(v1->val, uv, bits, am, epoint);
    if (am != NULL) {
        atype_t type = v1->type;
        while (type != A_NONE) {
            *am <<= 4;
            type >>= 4;
        }
        *am |= v1->type;
    }
    return err;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *result;
    Address *v1 = (Address *)op->v1;
    atype_t am;
    switch (op->op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV:
    case O_NEG:
    case O_POS:
        am = v1->type;
        if (check_addr(am)) break;
        op->v1 = v1->val;
        result = op->v1->obj->calc1(op);
        op->v1 = &v1->v;
        if (result->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)result); result = (Obj *)ref_none(); }
        return (Obj *)new_address(result, am);
    case O_STRING: break;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2, *result;
    Address *v1 = (Address *)op->v1, *v;
    atype_t am;
    switch (o2->obj->type) {
    case T_ADDRESS:
        {
            Address *v2 = (Address *)o2;
            am = v1->type;
            switch (op->op->op) {
            case O_CMP:
            case O_EQ:
            case O_NE:
            case O_LT:
            case O_LE:
            case O_GT:
            case O_GE:
                if (am == v2->type) {
                    op->v1 = v1->val;
                    op->v2 = v2->val;
                    result = op->v1->obj->calc2(op);
                    op->v1 = &v1->v;
                    op->v2 = &v2->v;
                    return result;
                }
                switch (op->op->op) {
                default: /* can't happen */
                case O_CMP: return (Obj *)ref_int((am < v2->type) ? minus1_value : int_value[1]);
                case O_EQ: return (Obj *)ref_bool(false_value);
                case O_NE: return (Obj *)ref_bool(true_value);
                case O_LE:
                case O_LT: return truth_reference(am < v2->type);
                case O_GE:
                case O_GT: return truth_reference(am > v2->type);
                }
            case O_ADD:
                if (check_addr(am)) break;
                if (check_addr(v2->type)) break;
                op->v1 = v1->val;
                op->v2 = v2->val;
                result = op->v1->obj->calc2(op);
                op->v1 = &v1->v;
                op->v2 = &v2->v;
                if (result->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)result); result = (Obj *)ref_none(); }
                v = new_address(result, am);
                am = v2->type;
                while ((enum atype_e)(am & 0xf) != A_NONE) { v->type <<= 4; am >>= 4; }
                v->type |= v2->type;
                return &v->v;
            case O_SUB:
                if (check_addr(am)) break;
                if (check_addr(v2->type)) break;
                if (am != v2->type) break; /* TODO */
                op->v1 = v1->val;
                op->v2 = v2->val;
                result = op->v1->obj->calc2(op);
                op->v1 = &v1->v;
                op->v2 = &v2->v;
                return result;
            default:
                break;
            }
            break;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_BYTES:
    case T_STR:
        switch (op->op->op) {
        default:
            am = v1->type;
            if (check_addr2(am)) break;
            goto ok;
        case O_ADD:
        case O_SUB:
            am = v1->type;
            if (check_addr(am)) break;
        ok:
            op->v1 = v1->val;
            result = op->v1->obj->calc2(op);
            op->v1 = &v1->v;
            if (result->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)result); result = (Obj *)ref_none(); }
            return (Obj *)new_address(result, am);
        }
        break;
    default:
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return o2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1, *result;
    Address *v2 = (Address *)op->v2;
    atype_t am;
    switch (o1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_BYTES:
    case T_STR:
    case T_GAP:
        switch (op->op->op) {
        case O_ADD:
            am = v2->type;
            if (check_addr(am)) break;
            op->v2 = v2->val;
            result = o1->obj->calc2(op);
            op->v2 = &v2->v;
            if (result->obj == ERROR_OBJ) { err_msg_output_and_destroy((Error *)result); result = (Obj *)ref_none(); }
            return (Obj *)new_address(result, am);
        default: break;
        }
        break;
    default: break;
    }
    return obj_oper_error(op);
}

void addressobj_init(void) {
    new_type(&obj, T_ADDRESS, "address", sizeof(Address));
    obj_init(&obj);
    obj.create = create;
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.repr = repr;
    obj.address = address;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}

void addressobj_names(void) {
    new_builtin("address", val_reference(&ADDRESS_OBJ->v));
}
