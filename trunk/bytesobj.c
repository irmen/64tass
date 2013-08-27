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
#include "bytesobj.h"
#include "eval.h"
#include "isnprintf.h"

#include "intobj.h"
#include "boolobj.h"
#include "strobj.h"
#include "listobj.h"

static struct obj_s obj;

obj_t BYTES_OBJ = &obj;

static void destroy(struct value_s *v1) {
    free((uint8_t*)v1->u.bytes.data);
}

static void copy(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    v->obj = BYTES_OBJ;
    v->refcount = 1;
    v->u.bytes.len = v1->u.bytes.len;
    if (v1->u.bytes.len) {
        s = (uint8_t *)malloc(v1->u.bytes.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, v1->u.bytes.data, v1->u.bytes.len);
    } else s = NULL;
    v->u.bytes.data = s;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = BYTES_OBJ;
    v->refcount = 1;
    v->u.bytes.len = v1->u.bytes.len;
    v->u.bytes.data = v1->u.bytes.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BYTES_OBJ && v1->u.bytes.len == v2->u.bytes.len && (
            v1->u.bytes.data == v2->u.bytes.data ||
            !memcmp(v1->u.bytes.data, v2->u.bytes.data, v2->u.bytes.len));
}

static int truth(const struct value_s *v1) {
    return !!v1->u.bytes.len;
}

static void repr(const struct value_s *v1, struct value_s *v) {
    size_t i, len, len2;
    uint8_t *s;
    len2 = v1->u.bytes.len * 4;
    len = 9 - (v1->u.bytes.len > 0) + len2;
    s = (uint8_t *)malloc(len);
    if (!s || len < len2 || v1->u.bytes.len > ((size_t)~0) / 4) err_msg_out_of_memory();

    memcpy(s, "bytes([", 7);
    len = 7;
    for (i = 0;i < v1->u.bytes.len; i++) {
        len += sprintf((char *)s + len, i ? ",$%02x" : "$%02x", v1->u.bytes.data[i]);
    }
    s[len++] = ']';
    s[len++] = ')';
    if (v == v1) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.len = len;
    v->u.str.chars = len;
    v->u.str.data = s;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    size_t l = v1->u.bytes.len;
    const uint8_t *s2 = v1->u.bytes.data;
    unsigned int h;
    if (!l) return 0;
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.bytes.len;
    return h & ((~(unsigned int)0) >> 1);
}

int bytes_from_str(struct value_s *v, const struct value_s *v1) {
    size_t len = v1->u.str.len, len2 = 0;
    uint8_t *s;
    if (len) {
        s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();

        if (actual_encoding) {
            size_t i = 0;
            int16_t ch;
            while (len > i) {
                ch = petascii(&i, v1);
                if (ch > 255) {
                    free(s);
                    v->obj = NONE_OBJ;
                    return 1;
                }
                s[len2++] = ch;
            }
        } else {
            memcpy(s, v1->u.str.data, len);
            len2 = len;
        }
    } else s = NULL;
    if (v == v1) v->obj->destroy(v);
    v->obj = BYTES_OBJ;
    v->u.bytes.len = len2;
    v->u.bytes.data = s;
    return 0;
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    int_from_bytes(&tmp, v1);
    ret = tmp.obj->ival(&tmp, v, iv, bits, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    int_from_bytes(&tmp, v1);
    ret = tmp.obj->uval(&tmp, v, uv, bits, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    int_from_bytes(&tmp, v1);
    ret = tmp.obj->real(&tmp, v, r, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *UNUSED(v), int *s, linepos_t UNUSED(epoint)) {
    *s = !!v1->u.bytes.len;
    return 0;
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    if (v1 != v) copy(v1, v);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    struct value_s tmp;
    int_from_bytes(&tmp, v1);
    if (v == v1) v->obj->destroy(v);
    tmp.obj->copy_temp(&tmp, v);
}

static int MUST_CHECK len(const struct value_s *v1, struct value_s *UNUSED(v), uval_t *uv, linepos_t UNUSED(epoint)) {
    *uv = v1->u.bytes.len;
    return 0;
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
    struct value_s tmp;
    switch (op->op->u.oper.op) {
    case O_NEG:
    case O_POS:
    case O_STRING: int_from_bytes(&tmp, v1);
        break;
    case O_LNOT:
        if (v1 == v) destroy(v);
        return bool_from_int(v, !truth(v1)); 
    default: bits_from_bytes(&tmp, v1);
        break;
    }
    if (v == v1) destroy(v);
    op->v1 = &tmp;
    tmp.refcount = 0;
    tmp.obj->calc1(op);
    op->v1 = v1;
    tmp.obj->destroy(&tmp);
}

static int calc2_bytes(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    int val;
    switch (op->op->u.oper.op) {
    case O_CMP:
        {
            int h = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len : v2->u.bytes.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.bytes.len > v2->u.bytes.len) - (v1->u.bytes.len < v2->u.bytes.len);
            if (v == v1 || v == v2) destroy(v);
            int_from_int(v, h);
            return 0;
        }
    case O_EQ:
        val = (v1->u.bytes.len == v2->u.bytes.len) && (v1->u.bytes.data == v2->u.bytes.data || !memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len));
        break;
    case O_NE:
        val = (v1->u.bytes.len != v2->u.bytes.len) || (v1->u.bytes.data != v2->u.bytes.data && memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len));
        break;
    case O_LT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len < v2->u.bytes.len; else val = val < 0;
        break;
    case O_GT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len > v2->u.bytes.len; else val = val > 0;
        break;
    case O_LE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len <= v2->u.bytes.len; else val = val <= 0;
        break;
    case O_GE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len >= v2->u.bytes.len; else val = val >= 0;
        break;
    case O_CONCAT:
        if (v == v1) {
            uint8_t *s;
            size_t len;
            if (!v2->u.bytes.len) return 0;
            len = v1->u.bytes.len;
            v->u.bytes.len += v2->u.bytes.len;
            s = (uint8_t *)v1->u.bytes.data;
            s = (uint8_t *)realloc(s, v->u.bytes.len);
            if (!s || v->u.bytes.len < v2->u.bytes.len) err_msg_out_of_memory(); /* overflow */
            memcpy(s + len, v2->u.bytes.data, v2->u.bytes.len);
            v->u.bytes.data = s;
            return 0;
        } 
        v->obj = BYTES_OBJ;
        v->u.bytes.len = v1->u.bytes.len + v2->u.bytes.len;
        if (v->u.bytes.len < v2->u.bytes.len) err_msg_out_of_memory(); /* overflow */
        if (v->u.bytes.len) {
            uint8_t *s;
            s = (uint8_t *)malloc(v->u.bytes.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s, v1->u.bytes.data, v1->u.bytes.len);
            memcpy(s + v1->u.bytes.len, v2->u.bytes.data, v2->u.bytes.len);
            v->u.bytes.data = s;
        } else v->u.bytes.data = NULL;
        return 0;
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.bytes.len) { val = 1; break; }
            if (v1->u.bytes.len > v2->u.bytes.len) { val = 0; break; }
            c2 = v2->u.bytes.data;
            e = c2 + v2->u.bytes.len - v1->u.bytes.len;
            for (;;) {   
                c = (uint8_t *)memchr(c2, v1->u.bytes.data[0], e - c2 + 1);
                if (!c) { val = 0; break; }
                if (!memcmp(c, v1->u.bytes.data, v1->u.bytes.len)) { val = 1; break; }
                c2 = c + 1;
            }
            break;
        }
    default: return 1;
    }
    if (v == v1 || v == v2) destroy(v);
    bool_from_int(v, val);
    return 0;
}

static void repeat(oper_t op, uval_t rep) {
    struct value_s *v1 = op->v1, *v = op->v;
    if (v == v1) {
        uint8_t *s;
        size_t len = v1->u.bytes.len;
        
        if (!rep || !len) {
            if (v1 == v) destroy(v);
            copy(&null_bytes, v); return;
        }
        if (rep == 1) {
            if (v1 != v) copy(v1, v);
            return;
        }
        v->u.bytes.len *= rep;
        s = (uint8_t *)v1->u.bytes.data;
        s = (uint8_t *)realloc(s, v->u.bytes.len);
        if (!s) err_msg_out_of_memory();
        v->u.bytes.data = s;
        while (--rep) {
            memcpy(s + len, s, len);
            s += len;
        }
    } else {
        v->obj = BYTES_OBJ;
        v->u.bytes.len = 0;
        if (v1->u.bytes.len && rep) {
            uint8_t *s;
            s = (uint8_t *)malloc(v1->u.bytes.len * rep);
            if (!s || v1->u.bytes.len > ((size_t)~0) / rep) err_msg_out_of_memory();
            while (rep--) {
                memcpy(s + v->u.bytes.len, v1->u.bytes.data, v1->u.bytes.len);
                v->u.bytes.len += v1->u.bytes.len;
            }
            v->u.bytes.data = s;
        } else v->u.bytes.data = NULL;
    }
    return;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v2->obj->type) {
    case T_BYTES: if (calc2_bytes(op)) break; return;
    case T_BOOL:
    case T_INT:
    case T_BITS: 
    case T_FLOAT:
    case T_CODE: 
    case T_ADDRESS: 
        {
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: bits_from_bytes(&tmp, v1); break;
            default: int_from_bytes(&tmp, v1);
            }
            if (v1 == v) v->obj->destroy(v);
            op->v1 = &tmp;
            tmp.refcount = 0;
            tmp.obj->calc2(op);
            op->v1 = v1;
            tmp.obj->destroy(&tmp);
        }
        return;
    case T_TUPLE:
    case T_LIST: 
    case T_STR: 
        v2->obj->rcalc2(op); return;
    default: break;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v1->obj->type) {
    case T_BYTES: if (calc2_bytes(op)) break; return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS: 
        {
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: bits_from_bytes(&tmp, v2); break;
            default: int_from_bytes(&tmp, v2);
            }
            if (v2 == v) v->obj->destroy(v);
            op->v2 = &tmp;
            tmp.refcount = 0;
            tmp.obj->rcalc2(op); 
            op->v2 = v2;
            tmp.obj->destroy(&tmp);
        }
        return;
    case T_TUPLE:
    case T_LIST: 
    case T_STR: 
        if (op->op != &o_IN) {
            v1->obj->calc2(op); return;
        }
    default: break;
    }
    obj_oper_error(op); return;
}

static void iindex(oper_t op) {
    const uint8_t *p;
    uint8_t *p2;
    size_t len, len2;
    ival_t offs;
    size_t i;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    len = v1->u.bytes.len;

    if (v2->obj == TUPLE_OBJ || v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            if (v1 == v) destroy(v);
            copy(&null_bytes, v);return;
        }
        len2 = v2->u.list.len;
        p2 = (uint8_t *)malloc(len2);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        for (i = 0; i < v2->u.list.len; i++) {
            offs = indexoffs(v2->u.list.data[i], len);
            if (offs < 0) {
                free((uint8_t *)p);
                if (v1 == v) destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___INDEX_RANGE;
                v->u.error.epoint = op->epoint2;
                return;
            }
            *p2++ = v1->u.bytes.data[offs];
        }
        if (v == v1) destroy(v);
        v->obj = BYTES_OBJ;
        v->u.bytes.len = len2;
        v->u.bytes.data = p;
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
    p2 = (uint8_t *)malloc(1);
    if (!p2) err_msg_out_of_memory();
    p2[0] = v1->u.bytes.data[offs];
    if (v1 == v) destroy(v);
    v->obj = BYTES_OBJ;
    v->u.bytes.len = 1;
    v->u.bytes.data = p2;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t len;
    const uint8_t *p;
    uint8_t *p2;

    if (step > 0) {
        if (offs > end) offs = end;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }
    if (!len) {
        if (v1 == v) destroy(v);
        copy(&null_bytes, v);return;
    }
    if (step == 1) {
        if (len == v1->u.bytes.len) {
            if (v1 != v) copy(v1, v);
            return; /* original bytes */
        }
        p2 = (uint8_t *)malloc(len);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        memcpy(p2, v1->u.bytes.data + offs, len);
    } else {
        p2 = (uint8_t *)malloc(len);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            *p2++ = v1->u.bytes.data[offs];
            offs += step;
        }
    }
    if (v == v1) destroy(v);
    v->obj = BYTES_OBJ;
    v->u.bytes.len = len;
    v->u.bytes.data = p;
}

void bytesobj_init(void) {
    obj_init(&obj, T_BYTES, "<bytes>");
    obj.destroy = destroy;
    obj.copy = copy;
    obj.copy_temp = copy_temp;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
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
    obj.repeat = repeat;
    obj.iindex = iindex;
    obj.slice = slice;
}
