/*
    $Id: registerobj.c 3122 2023-09-16 10:44:56Z soci $

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
#include "registerobj.h"
#include <string.h>
#include "eval.h"
#include "variables.h"
#include "values.h"

#include "strobj.h"
#include "typeobj.h"
#include "errorobj.h"
#include "addressobj.h"
#include "intobj.h"
#include "error.h"

static Type obj;

Type *const REGISTER_OBJ = &obj;

static FAST_CALL NO_INLINE void register_destroy(Register *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    Register *v1 = Register(o1);
    if unlikely(v1->val != v1->data) register_destroy(v1);
}

static inline MALLOC Register *new_register(void) {
    return Register(val_alloc(REGISTER_OBJ));
}

static MUST_CHECK Obj *register_from_obj(Obj *o1, linepos_t epoint) {
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_REGISTER:
        return val_reference(o1);
    case T_STR:
        {
            uint8_t *s;
            Str *v1 = Str(o1);
            Register *v = new_register();
            v->chars = v1->chars;
            v->len = v1->len;
            if (v->len != 0) {
                if (v->len > sizeof v->val) {
                    s = allocate_array(uint8_t, v->len);
                    if (s == NULL) return new_error_mem(epoint);
                } else s = v->val;
                memcpy(s, v1->data, v->len);
            } else s = NULL;
            v->data = s;
            return Obj(v);
        }
    default: break;
    }
    return new_error_conv(o1, REGISTER_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return register_from_obj(op->v2, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Register *v1 = Register(o1), *v2 = Register(o2);
    return o1->obj == o2->obj && v1->len == v2->len && (
            v1->data == v2->data ||
            memcmp(v1->data, v2->data, v2->len) == 0);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Register *v1 = Register(o1);
    size_t l = v1->len;
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (l == 0) {
        *hs = 0;
        return NULL;
    }
    h = (unsigned int)*s2 << 7;
    while ((l--) != 0) h = (1000003 * h) ^ *s2++;
    h ^= (unsigned int)v1->len;
    *hs = h & ((~0U) >> 1);
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Register *v1 = Register(o1);
    size_t i2, i;
    uint8_t *s, *s2;
    uint8_t q;
    size_t chars;
    Str *v;
    i = str_quoting(v1->data, v1->len, &q);

    if (add_overflow(i, 12, &i2)) return NULL;
    chars = i2 - (v1->len - v1->chars);
    if (chars > maxsize) return NULL;
    v = new_str2(i2);
    if (v == NULL) return NULL;
    v->chars = chars;
    s = v->data;

    memcpy(s, "register(", 9);
    s += 9;
    *s++ = q;
    s2 = v1->data;
    for (i = 0; i < v1->len; i++) {
        s[i] = s2[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    s[i + 1] = ')';
    return Obj(v);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Register *v1 = Register(o1);
    Str *v;

    if (v1->chars > maxsize) return NULL;
    v = new_str2(v1->len);
    if (v == NULL) return NULL;
    v->chars = v1->chars;
    memcpy(v->data, v1->data, v1->len);
    return Obj(v);
}

static inline int icmp(oper_t op) {
    const Register *v1 = Register(op->v1), *v2 = Register(op->v2);
    int h = memcmp(v1->data, v2->data, (v1->len < v2->len) ? v1->len : v2->len);
    if (h != 0) return h;
    if (v1->len < v2->len) return -1;
    return (v1->len > v2->len) ? 1 : 0;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    const Type *t2 = op->v2->obj;
    switch (t2->type) {
    case T_REGISTER:
        return obj_oper_compare(op, icmp(op));
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_BYTES:
    case T_STR:
    case T_CODE:
        if (Register(op->v1)->len == 1) {
            Address_types am = register_to_indexing(Register(op->v1)->data[0]);
            if (am == A_NONE) break;
            if (op->op == O_ADD) {
                return new_address(val_reference(op->v2), am);
            }
            if (op->op == O_SUB) {
                op->v1 = int_value[0];
                op->inplace = NULL;
                return new_address(INT_OBJ->calc2(op), am);
            }
        }
        break;
    case T_ADDRESS:
        return t2->rcalc2(op);
    case T_NONE:
    case T_ERROR:
        return val_reference(op->v2);
    default:
        if (t2->iterable && op->op != O_MEMBER && op->op != O_X) {
            return t2->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    const Type *t1 = op->v1->obj;
    switch (t1->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_BYTES:
        if (Register(op->v2)->len == 1) {
            if (op->op == O_ADD) {
                Address_types am = register_to_indexing(Register(op->v2)->data[0]);
                if (am != A_NONE) {
                    return new_address(val_reference(op->v1), am);
                }
            }
        }
        break;
    default:
        if (!t1->iterable) {
            break;
        }
        FALL_THROUGH; /* fall through */
    case T_NONE:
    case T_ERROR:
        return t1->calc2(op);
    }
    return obj_oper_error(op);
}

static uint32_t register_names;

void registerobj_init(void) {
    Type *type = new_type(&obj, T_REGISTER, "register", sizeof(Register));
    type->convert = convert;
    type->destroy = destroy;
    type->same = same;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    register_names = 0;
}

void registerobj_names(void) {
    new_builtin("register", val_reference(Obj(REGISTER_OBJ)));
}

bool registerobj_createnames(uint32_t registers) {
    uint32_t regs = registers & ~register_names;
    uint8_t name;

    if (regs == 0) return false;
    register_names |= regs;

    name = 'a';
    for (; regs != 0; regs >>= 1, name++) if ((regs & 1) != 0) {
        Register *reg = new_register();
        reg->val[0] = name;
        reg->val[1] = 0;
        reg->data = reg->val;
        reg->len = 1;
        reg->chars = 1;
        new_builtin((const char *)reg->val, Obj(reg));
    }
    return true;
}
