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
#include "unicode.h"
#include "encoding.h"

#include "boolobj.h"

static struct obj_s obj;

obj_t BYTES_OBJ = &obj;

static void destroy(struct value_s *v1) {
    if (v1->u.bytes.val != v1->u.bytes.data) free(v1->u.bytes.data);
}

static uint8_t *bnew(struct value_s *v, size_t len) {
    if (len > sizeof(v->u.bytes.val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->u.bytes.val;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    v->obj = BYTES_OBJ;
    v->u.bytes.len = v1->u.bytes.len;
    if (v1->u.bytes.len) {
        s = bnew(v, v->u.bytes.len);
        memcpy(s, v1->u.bytes.data, v->u.bytes.len);
    } else s = NULL;
    v->u.bytes.data = s;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = BYTES_OBJ;
    v->u.bytes.len = v1->u.bytes.len;
    if (v1->u.bytes.data == v1->u.bytes.val) {
        v->u.bytes.data = v->u.bytes.val;
        if (v->u.bytes.len) memcpy(v->u.bytes.data, v1->u.bytes.data, v->u.bytes.len);
    } else v->u.bytes.data = v1->u.bytes.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BYTES_OBJ && v1->u.bytes.len == v2->u.bytes.len && (
            v1->u.bytes.data == v2->u.bytes.data ||
            !memcmp(v1->u.bytes.data, v2->u.bytes.data, v2->u.bytes.len));
}

static MUST_CHECK struct value_s *truth(const struct value_s *v1, enum truth_e type, linepos_t epoint) {
    struct value_s *v;
    size_t i;
    switch (type) {
    case TRUTH_ALL:
        for (i = 0; i < v1->u.bytes.len; i++) {
            if (!v1->u.bytes.data[i]) return truth_reference(0);
        }
        return truth_reference(1);
    case TRUTH_ANY:
    case TRUTH_BOOL:
        for (i = 0; i < v1->u.bytes.len; i++) {
            if (v1->u.bytes.data[i]) return truth_reference(1);
        }
        return truth_reference(0);
    default: 
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_BOOL;
        v->u.error.epoint = *epoint;
        v->u.error.u.objname = v1->obj->name;
        return v;
    }
}

static MUST_CHECK struct value_s *repr(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    size_t i, len, len2;
    uint8_t *s;
    struct value_s *v = val_alloc();
    len2 = v1->u.bytes.len * 4;
    len = 9 - (v1->u.bytes.len > 0) + len2;
    if (len < len2 || v1->u.bytes.len > SIZE_MAX / 4) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, len);

    memcpy(s, "bytes([", 7);
    len = 7;
    for (i = 0;i < v1->u.bytes.len; i++) {
        len += sprintf((char *)s + len, i ? ",$%02x" : "$%02x", v1->u.bytes.data[i]);
    }
    s[len++] = ']';
    s[len++] = ')';
    v->obj = STR_OBJ;
    v->u.str.len = len;
    v->u.str.chars = len;
    v->u.str.data = s;
    return v;
}

static MUST_CHECK struct value_s *hash(const struct value_s *v1, int *hs, linepos_t UNUSED(epoint)) {
    size_t l = v1->u.bytes.len;
    const uint8_t *s2 = v1->u.bytes.data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.bytes.len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

MUST_CHECK struct value_s *bytes_from_str(const struct value_s *v1, linepos_t epoint) {
    size_t len = v1->u.str.len, len2 = 0;
    uint8_t *s;
    struct value_s *v;
    if (len) {
        v = val_alloc();
        if (actual_encoding) {
            int ch;
            if (len < sizeof(v->u.bytes.val)) len = sizeof(v->u.bytes.val);
            s = bnew(v, len);
            encode_string_init(v1, epoint);
            while ((ch = encode_string()) != EOF) {
                if (len2 >= len) {
                    if (v->u.bytes.val == s) {
                        len = 32;
                        s = (uint8_t *)malloc(len);
                        memcpy(s, v->u.bytes.val, len2);
                    } else {
                        len += 1024;
                        if (len < 1024) err_msg_out_of_memory(); /* overflow */
                        s = (uint8_t *)realloc(s, len);
                    }
                    if (!s) err_msg_out_of_memory();
                }
                s[len2++] = ch;
            }
        } else if (v1->u.str.chars == 1) {
            uint32_t ch2 = v1->u.str.data[0];
            if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
            s = bnew(v, 3);
            s[0] = ch2;
            s[1] = ch2 >> 8;
            s[2] = ch2 >> 16;
            len2 = 3;
        } else {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_BIG_STRING_CO;
            v->u.error.epoint = *epoint;
            return v;
        }
        if (v->u.bytes.val != s) {
            if (len2 <= sizeof(v->u.bytes.val)) {
                if (len2) memcpy(v->u.bytes.val, s, len2);
                free(s);
                s = v->u.bytes.val;
            } else if (len2 < len) {
                s = (uint8_t *)realloc(s, len2);
                if (!s) err_msg_out_of_memory();
            }
        }
        v->obj = BYTES_OBJ;
        v->u.bytes.len = len2;
        v->u.bytes.data = s;
        return v;
    }
    return val_reference(null_bytes);
}

static MUST_CHECK struct value_s *ival(const struct value_s *v1, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s *tmp, *ret;
    tmp = int_from_bytes(v1);
    ret = tmp->obj->ival(tmp, iv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK struct value_s *uval(const struct value_s *v1, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s *tmp, *ret;
    tmp = int_from_bytes(v1);
    ret = tmp->obj->uval(tmp, uv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK struct value_s *real(const struct value_s *v1, double *r, linepos_t epoint) {
    struct value_s *tmp, *ret;
    tmp = int_from_bytes(v1);
    ret = tmp->obj->real(tmp, r, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK struct value_s *sign(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    size_t i;
    for (i = 0; i < v1->u.bytes.len; i++) {
        if (v1->u.bytes.data[i]) return val_reference(int_value[1]);
    }
    return val_reference(int_value[0]);
}

static MUST_CHECK struct value_s *absolute(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    return int_from_bytes(v1);
}

static MUST_CHECK struct value_s *integer(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    return int_from_bytes(v1);
}

static MUST_CHECK struct value_s *len(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.bytes.len);
}

static MUST_CHECK struct value_s *getiter(struct value_s *v1) {
    struct value_s *v = val_alloc();
    v->obj = ITER_OBJ;
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
    return v;
}

static MUST_CHECK struct value_s *next(struct value_s *v1) {
    const struct value_s *vv1 = v1->u.iter.data;
    struct value_s *v;
    if (v1->u.iter.val >= vv1->u.bytes.len) return NULL;
    v = val_alloc();
    v->u.bytes.val[0] = vv1->u.bytes.data[v1->u.iter.val++];
    v->obj = BYTES_OBJ;
    v->u.bytes.len = 1;
    v->u.bytes.data = v->u.bytes.val;
    return v;
}

static MUST_CHECK struct value_s *calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v;
    struct value_s *tmp;
    switch (op->op->u.oper.op) {
    case O_NEG:
    case O_POS:
    case O_STRING: tmp = int_from_bytes(v1); break;
    case O_INV:
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD: tmp = bits_from_bytes(v1); break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    v = tmp->obj->calc1(op);
    op->v1 = v1;
    val_destroy(tmp);
    return v;
}

static MUST_CHECK struct value_s *calc2_bytes(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v;
    int val;
    switch (op->op->u.oper.op) {
    case O_CMP:
        {
            int h = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len : v2->u.bytes.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.bytes.len > v2->u.bytes.len) - (v1->u.bytes.len < v2->u.bytes.len);
            if (h < 0) return int_from_int(h);
            return val_reference(int_value[h]);
        }
    case O_EQ: return truth_reference(v1->u.bytes.len == v2->u.bytes.len && (v1->u.bytes.data == v2->u.bytes.data || !memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len)));
    case O_NE: return truth_reference(v1->u.bytes.len != v2->u.bytes.len || (v1->u.bytes.data != v2->u.bytes.data && memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len)));
    case O_LT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        return truth_reference(val ? (val < 0) : (v1->u.bytes.len < v2->u.bytes.len));
    case O_GT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        return truth_reference(val ? (val > 0) : (v1->u.bytes.len > v2->u.bytes.len));
    case O_LE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        return truth_reference(val ? (val <= 0) : (v1->u.bytes.len <= v2->u.bytes.len));
    case O_GE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        return truth_reference(val ? (val >= 0) : (v1->u.bytes.len >= v2->u.bytes.len));
    case O_CONCAT:
        {
            uint8_t *s;
            size_t ln;
            if (!v1->u.bytes.len) {
                return val_reference(v2);
            }
            if (!v2->u.bytes.len) {
                return val_reference(v1);
            }
            ln = v1->u.bytes.len + v2->u.bytes.len;
            if (ln < v2->u.bytes.len) err_msg_out_of_memory(); /* overflow */

            v = val_alloc();
            s = bnew(v, ln);
            memcpy(s, v1->u.bytes.data, v1->u.bytes.len);
            memcpy(s + v1->u.bytes.len, v2->u.bytes.data, v2->u.bytes.len);
            v->obj = BYTES_OBJ;
            v->u.bytes.len = ln;
            v->u.bytes.data = s;
            return v;
        }
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.bytes.len) return truth_reference(1);
            if (v1->u.bytes.len > v2->u.bytes.len) return truth_reference(0);
            c2 = v2->u.bytes.data;
            e = c2 + v2->u.bytes.len - v1->u.bytes.len;
            for (;;) {
                c = (uint8_t *)memchr(c2, v1->u.bytes.data[0], e - c2 + 1);
                if (!c) return truth_reference(0);
                if (!memcmp(c, v1->u.bytes.data, v1->u.bytes.len)) return truth_reference(1);
                c2 = c + 1;
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *repeat(oper_t op, uval_t rep) {
    struct value_s *v1 = op->v1, *v;

    if (v1->u.bytes.len && rep) {
        uint8_t *s, *s2;
        size_t ln;
        if (rep == 1) {
            return val_reference(v1);
        }
        ln = v1->u.bytes.len;
        if (ln > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = val_alloc();
        s2 = s = bnew(v, ln * rep);
        v->obj = BYTES_OBJ;
        v->u.bytes.len = 0;
        while (rep--) {
            memcpy(s + v->u.bytes.len, v1->u.bytes.data, ln);
            v->u.bytes.len += ln;
        }
        if (v->u.bytes.len <= sizeof(v->u.bytes.val) && v->u.bytes.val != s2) {
            memcpy(v->u.bytes.val, s2, v->u.bytes.len);
            free(s2);
            s2 = v->u.bytes.val;
        }
        v->u.bytes.data = s2;
        return v;
    }
    return val_reference(null_bytes);
}

static MUST_CHECK struct value_s *calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2;
    struct value_s *tmp;
    switch (v2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            struct value_s *result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: tmp = bits_from_bytes(v1); break;
            default: tmp = int_from_bytes(v1);
            }
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_GAP:
    case T_REGISTER:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2;
    struct value_s *tmp;
    switch (v1->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            struct value_s *result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = bits_from_bytes(v2); break;
            default: tmp = int_from_bytes(v2);
            }
            op->v2 = tmp;
            result = tmp->obj->rcalc2(op);
            op->v2 = v2;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    default: break;
    }
    return obj_oper_error(op); 
}

static inline MUST_CHECK struct value_s *slice(struct value_s *v1, uval_t len1, ival_t offs, ival_t end, ival_t step) {
    uint8_t *p;
    uint8_t *p2;
    struct value_s *v;

    if (!len1) {
        return val_reference(null_bytes);
    }
    if (step == 1) {
        if (len1 == v1->u.bytes.len) {
            return val_reference(v1); /* original bytes */
        }
        v = val_alloc();
        p = p2 = bnew(v, len1);
        memcpy(p2, v1->u.bytes.data + offs, len1);
    } else {
        v = val_alloc();
        p = p2 = bnew(v, len1);
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            *p2++ = v1->u.bytes.data[offs];
            offs += step;
        }
    }
    v->obj = BYTES_OBJ;
    v->u.bytes.len = len1;
    v->u.bytes.data = p;
    return v;
}

static MUST_CHECK struct value_s *iindex(oper_t op) {
    uint8_t *p;
    uint8_t *p2;
    size_t len1, len2;
    ival_t offs;
    size_t i;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v, *err;

    len1 = v1->u.bytes.len;

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            return val_reference(null_bytes);
        }
        len2 = v2->u.list.len;
        v = val_alloc();
        p = p2 = bnew(v, len2);
        for (i = 0; i < len2; i++) {
            err = indexoffs(v2->u.list.data[i], &offs, len1, op->epoint2);
            if (err) {
                if (p != v->u.bytes.val) free(p);
                return err;
            }
            *p2++ = v1->u.bytes.data[offs];
        }
        v->obj = BYTES_OBJ;
        v->u.bytes.len = len2;
        v->u.bytes.data = p;
        return v;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t length, end, step;
        err = sliceparams(op, len1, &length, &offs, &end, &step);
        if (err) return err;
        return slice(v1, length, offs, end, step);
    }
    err = indexoffs(v2, &offs, len1, op->epoint2);
    if (err) return err;
    v = val_alloc();
    v->obj = BYTES_OBJ;
    v->u.bytes.len = 1;
    v->u.bytes.val[0] = v1->u.bytes.data[offs];
    v->u.bytes.data = v->u.bytes.val;
    return v;
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
    obj.getiter = getiter;
    obj.next = next;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.repeat = repeat;
    obj.iindex = iindex;
}
