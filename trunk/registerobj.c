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
#include "registerobj.h"
#include <string.h>
#include "error.h"
#include "eval.h"
#include "variables.h"

#include "boolobj.h"
#include "strobj.h"
#include "intobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type register_obj;

Type *REGISTER_OBJ = &register_obj;

static void destroy(Obj *o1) {
    Register *v1 = (Register *)o1;
    if (v1->val != v1->data) free(v1->data);
}

static uint8_t *rnew(Register *v, size_t len) {
    if (len > sizeof v->val) {
        return (uint8_t *)mallocx(len);
    }
    return v->val;
}

static MUST_CHECK Obj *create(Obj *o1, linepos_t epoint) {
    uint8_t *s;
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_REGISTER: 
        return val_reference(o1);
    case T_STR:
        {
            Str *v1 = (Str *)o1;
            Register *v = new_register();
            v->chars = v1->chars;
            v->len = v1->len;
            if (v1->len != 0) {
                s = rnew(v, v->len);
                memcpy(s, v1->data, v->len);
            } else s = NULL;
            v->data = s;
            return &v->v;
        }
    default: break;
    }
    err_msg_wrong_type(o1, STR_OBJ, epoint);
    return (Obj *)ref_none();
}

static bool same(const Obj *o1, const Obj *o2) {
    const Register *v1 = (const Register *)o1, *v2 = (const Register *)o2;
    return o2->obj == REGISTER_OBJ && v1->len == v2->len && (
            v1->data == v2->data ||
            memcmp(v1->data, v2->data, v2->len) == 0);
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Register *v1 = (Register *)o1;
    size_t l = v1->len;
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (l == 0) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while ((l--) != 0) h = (1000003 * h) ^ *s2++;
    h ^= v1->len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Register *v1 = (Register *)o1;
    size_t i2, i;
    uint8_t *s, *s2;
    char q;
    size_t chars;
    Str *v;
    i = str_quoting(v1->data, v1->len, &q);

    i2 = i + 12;
    if (i2 < 12) err_msg_out_of_memory(); /* overflow */
    chars = i2 - (v1->len - v1->chars);
    if (chars > maxsize) return NULL;
    v = new_str(i2);
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
    return &v->v;
}

static int rcmp(Register *v1, Register *v2) {
    int h = memcmp(v1->data, v2->data, (v1->len < v2->len) ? v1->len : v2->len);
    if (h != 0) return h;
    if (v1->len < v2->len) return -1;
    return (v1->len > v2->len) ? 1 : 0;
}

static MUST_CHECK Obj *calc2_register(oper_t op) {
    Register *v1 = (Register *)op->v1, *v2 = (Register *)op->v2;
    int val;
    switch (op->op->op) {
    case O_CMP:
        val = rcmp(v1, v2);
        if (val < 0) return (Obj *)ref_int(minus1_value);
        return (Obj *)ref_int(int_value[val > 0]);
    case O_EQ: return truth_reference(rcmp(v1, v2) == 0);
    case O_NE: return truth_reference(rcmp(v1, v2) != 0);
    case O_LT: return truth_reference(rcmp(v1, v2) < 0);
    case O_LE: return truth_reference(rcmp(v1, v2) <= 0);
    case O_GT: return truth_reference(rcmp(v1, v2) > 0);
    case O_GE: return truth_reference(rcmp(v1, v2) >= 0);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *v2 = op->v2;
    switch (v2->obj->type) {
    case T_REGISTER: return calc2_register(op);
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *v1 = op->v1;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

void registerobj_init(void) {
    new_type(&register_obj, T_REGISTER, "register", sizeof(Register));
    obj_init(&register_obj);
    register_obj.create = create;
    register_obj.destroy = destroy;
    register_obj.same = same;
    register_obj.hash = hash;
    register_obj.repr = repr;
    register_obj.calc2 = calc2;
    register_obj.rcalc2 = rcalc2;
}

void registerobj_names(void) {
    new_builtin("register", val_reference(&REGISTER_OBJ->v));
}

static uint32_t register_names;

bool registerobj_createnames(uint32_t registers) {
    uint32_t regs = registers & ~register_names;
    char name[2];

    if (regs == 0) return false;
    register_names |= regs;

    name[0] = 'a';
    name[1] = 0;
    for (; regs != 0; regs >>= 1, name[0]++) if ((regs & 1) != 0) {
        Register *reg = new_register();
        reg->val[0] = name[0];
        reg->data = reg->val;
        reg->len = 1;
        reg->chars = 1;
        new_builtin(name, &reg->v);
    }
    return true;
}
