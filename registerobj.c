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
#include "registerobj.h"

#include "boolobj.h"

static struct obj_s register_obj;

obj_t REGISTER_OBJ = &register_obj;

static void destroy(value_t v1) {
    if (v1->u.reg.val != v1->u.reg.data) free(v1->u.reg.data);
}

static uint8_t *rnew(value_t v, size_t len) {
    if (len > sizeof(v->u.reg.val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->u.reg.val;
}

MUST_CHECK value_t register_from_str(const value_t v1) {
    uint8_t *s;
    value_t v = val_alloc(REGISTER_OBJ);
    v->u.reg.chars = v1->u.str.chars;
    v->u.reg.len = v1->u.str.len;
    if (v1->u.str.len) {
        s = rnew(v, v->u.reg.len);
        memcpy(s, v1->u.str.data, v->u.reg.len);
    } else s = NULL;
    v->u.reg.data = s;
    return v;
}

static int same(const value_t v1, const value_t v2) {
    return v1->obj == REGISTER_OBJ && v1->u.reg.len == v2->u.reg.len && (
            v1->u.reg.data == v2->u.reg.data ||
            !memcmp(v1->u.reg.data, v2->u.reg.data, v2->u.reg.len));
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    size_t l = v1->u.reg.len;
    const uint8_t *s2 = v1->u.reg.data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.reg.len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    size_t i2, i;
    uint8_t *s, *s2;
    char q;
    const char *prefix = "register(";
    size_t ln = strlen(prefix) + 3;
    value_t v = val_alloc(STR_OBJ);
    i = str_quoting(v1, &q);

    i2 = i + ln;
    if (i2 < ln) err_msg_out_of_memory(); /* overflow */
    s2 = s = str_create_elements(v, i2);

    while (*prefix) *s++ = *prefix++;
    *s++ = q;
    for (i = 0; i < v1->u.reg.len; i++) {
        s[i] = v1->u.reg.data[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    s[i+1] = ')';
    v->u.str.data = s2;
    v->u.str.len = i2;
    v->u.str.chars = i2 - (i - v1->u.reg.chars);
    return v;
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    int val;
    switch (v2->obj->type) {
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_CMP:
            {
                int h = memcmp(v1->u.reg.data, v2->u.reg.data, (v1->u.reg.len < v2->u.reg.len) ? v1->u.reg.len : v2->u.reg.len);
                if (h) h = (h > 0) - (h < 0);
                else h = (v1->u.reg.len > v2->u.reg.len) - (v1->u.reg.len < v2->u.reg.len);
                if (h < 0) return int_from_int(-1);
                return val_reference(int_value[h]);
            }
        case O_EQ: return truth_reference(v1->u.reg.len == v2->u.reg.len && (v1->u.reg.data == v2->u.reg.data || !memcmp(v1->u.reg.data, v2->u.reg.data, v1->u.reg.len)));
        case O_NE: return truth_reference(v1->u.reg.len != v2->u.reg.len || (v1->u.reg.data != v2->u.reg.data && memcmp(v1->u.reg.data, v2->u.reg.data, v1->u.reg.len)));
        case O_LT:
            val = memcmp(v1->u.reg.data, v2->u.reg.data, (v1->u.reg.len < v2->u.reg.len) ? v1->u.reg.len:v2->u.reg.len);
            return truth_reference(val ? (val < 0) : (v1->u.reg.len < v2->u.reg.len));
        case O_GT:
            val = memcmp(v1->u.reg.data, v2->u.reg.data, (v1->u.reg.len < v2->u.reg.len) ? v1->u.reg.len:v2->u.reg.len);
            return truth_reference(val ? (val > 0) : (v1->u.reg.len > v2->u.reg.len));
        case O_LE:
            val = memcmp(v1->u.reg.data, v2->u.reg.data, (v1->u.reg.len < v2->u.reg.len) ? v1->u.reg.len:v2->u.reg.len);
            return truth_reference(val ? (val <= 0) : (v1->u.reg.len <= v2->u.reg.len));
        case O_GE:
            val = memcmp(v1->u.reg.data, v2->u.reg.data, (v1->u.reg.len < v2->u.reg.len) ? v1->u.reg.len:v2->u.reg.len);
            return truth_reference(val ? (val >= 0) : (val = v1->u.reg.len >= v2->u.reg.len));
        default: break;
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        return v2->obj->calc2(op);
    default:
    case T_REGISTER:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
}

void registerobj_init(void) {
    obj_init(&register_obj, T_REGISTER, "<register>");
    register_obj.destroy = destroy;
    register_obj.same = same;
    register_obj.hash = hash;
    register_obj.repr = repr;
    register_obj.calc2 = calc2;
    register_obj.rcalc2 = rcalc2;
}
