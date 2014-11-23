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
#include "strobj.h"
#include "eval.h"
#include "misc.h"
#include "unicode.h"

#include "boolobj.h"
#include "floatobj.h"

static struct obj_s obj;

obj_t STR_OBJ = &obj;

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_STR: return val_reference(v1);
    default: return v1->obj->repr(v1, epoint); 
    }
}

static void destroy(value_t v1) {
    if (v1->u.str.val != v1->u.str.data) free(v1->u.str.data);
}

static uint8_t *snew(value_t v, size_t len) {
    if (len > sizeof(v->u.str.val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->u.str.val;
}

static int same(const value_t v1, const value_t v2) {
    return v1->obj == STR_OBJ && v1->u.str.len == v2->u.str.len && (
            v1->u.str.data == v2->u.str.data ||
            !memcmp(v1->u.str.data, v2->u.str.data, v2->u.str.len));
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e type, linepos_t epoint) {
    value_t tmp, ret;
    tmp = bytes_from_str(v1, epoint);
    ret = tmp->obj->truth(tmp, type, epoint);
    val_destroy(tmp);
    return ret;
}

size_t str_quoting(const value_t v1, char *q) {
    size_t i, sq = 0, dq = 0;
    for (i = 0; i < v1->u.str.len; i++) {
        switch (v1->u.str.data[i]) {
        case '\'': sq++; continue;
        case '"': dq++; continue;
        }
    }
    if (sq < dq) {
        i += sq;
        if (i < sq) err_msg_out_of_memory(); /* overflow */
        *q = '\'';
    } else {
        i += dq;
        if (i < dq) err_msg_out_of_memory(); /* overflow */
        *q = '"';
    }
    return i;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    size_t i2, i;
    uint8_t *s, *s2;
    char q;
    value_t v = val_alloc(STR_OBJ);
    i = str_quoting(v1, &q);

    i2 = i + 2;
    if (i2 < 2) err_msg_out_of_memory(); /* overflow */
    s2 = s = snew(v, i2);

    *s++ = q;
    for (i = 0; i < v1->u.str.len; i++) {
        s[i] = v1->u.str.data[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    v->u.str.data = s2;
    v->u.str.len = i2;
    v->u.str.chars = i2 - (i - v1->u.str.chars);
    return v;
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    size_t l = v1->u.str.len;
    const uint8_t *s2 = v1->u.str.data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.str.len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t tmp, ret;
    tmp = bytes_from_str(v1, epoint);
    ret = tmp->obj->ival(tmp, iv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t tmp, ret;
    tmp = bytes_from_str(v1, epoint);
    ret = tmp->obj->uval(tmp, uv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

MUST_CHECK value_t float_from_str(const value_t v1, linepos_t epoint) {
    value_t tmp, ret;
    tmp = bytes_from_str(v1, epoint);
    if (tmp->obj != BYTES_OBJ) return tmp;
    ret = float_from_bytes(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t epoint) {
    value_t tmp, ret;
    tmp = bytes_from_str(v1, epoint);
    ret = tmp->obj->sign(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t epoint) {
    return int_from_str(v1, epoint);
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_size(v1->u.str.chars);
}

static MUST_CHECK value_t getiter(value_t v1) {
    value_t v = val_alloc(ITER_OBJ);
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
    return v;
}

static MUST_CHECK value_t next(value_t v1) {
    const value_t vv1 = v1->u.iter.data;
    int ln;
    uint8_t *s;
    value_t v;
    if (v1->u.iter.val >= vv1->u.str.len) return NULL;
    ln = utf8len(vv1->u.str.data[v1->u.iter.val]);
    v = val_alloc(STR_OBJ);
    s = snew(v, ln);
    memcpy(s, vv1->u.str.data + v1->u.iter.val, ln);
    v1->u.iter.val += ln;

    v->u.str.chars = 1;
    v->u.str.len = ln;
    v->u.str.data = s;
    return v;
}

MUST_CHECK value_t str_from_str(const uint8_t *s, size_t *ln) {
    value_t v;
    size_t i2 = 0;
    size_t i, j;
    size_t r = 0;
    uint8_t ch2, ch = s[0];

    i = 1;
    for (;;) {
        if (!(ch2 = s[i])) {err_msg(ERROR______EXPECTED,"end of string"); break;}
        if (ch2 & 0x80) i += utf8len(ch2); else i++;
        if (ch2 == ch) {
            if (s[i] == ch && !arguments.tasmcomp) {i++;r++;} /* handle 'it''s' */
            else break; /* end of string; */
        }
        i2++;
    }
    v = val_alloc(STR_OBJ);
    if (r) {
        const uint8_t *p = s + 1, *p2;
        uint8_t *d;
        j = i - 2;
        v->u.str.len = j - r;
        v->u.str.chars = i2;
        d = snew(v, v->u.str.len);
        v->u.str.data = d;
        while (j) {
            p2 = (const uint8_t *)memchr(p, ch, j);
            if (p2) {
                memcpy(d, p, p2 - p + 1);
                j -= p2 - p + 2;
                d += p2 - p + 1; p = p2 + 2;
            } else {
                memcpy(d, p, j);
                j = 0;
            }
        }
    } else {
        v->u.str.len = (i > 1) ? (i - 2) : 0;
        v->u.str.chars = i2;
        v->u.str.data = snew(v, v->u.str.len);
        memcpy(v->u.str.data, s + 1, v->u.str.len);
    }
    *ln = i;
    return v;
}

uint8_t *str_create_elements(value_t v, size_t ln) {
    return snew(v, ln);
}

static int scmp(value_t v1, value_t v2) {
    int h = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len : v2->u.str.len);
    if (h) return h;
    return (v1->u.str.len > v2->u.str.len) - (v1->u.str.len < v2->u.str.len);
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    value_t tmp;
    switch (op->op->u.oper.op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV: tmp = bytes_from_str(v1, op->epoint); break;
    case O_NEG:
    case O_POS:
    case O_STRING: tmp = int_from_str(v1, op->epoint); break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    v = tmp->obj->calc1(op);
    op->v1 = v1;
    val_destroy(tmp);
    return v;
}

static MUST_CHECK value_t calc2_str(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    int val;
    switch (op->op->u.oper.op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
        {
            value_t tmp, tmp2, result;
            tmp = int_from_str(v1, op->epoint);
            tmp2 = int_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_AND:
    case O_OR:
    case O_XOR:
        {
            value_t tmp, tmp2, result;
            tmp = bytes_from_str(v1, op->epoint);
            tmp2 = bytes_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_LSHIFT:
    case O_RSHIFT:
        {
            value_t tmp, tmp2, result;
            tmp = bits_from_str(v1, op->epoint);
            tmp2 = bits_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_CMP:
        val = scmp(v1, v2);
        if (val < 0) return int_from_int(-1);
        return val_reference(int_value[val > 0]);
    case O_EQ: return truth_reference(scmp(v1, v2) == 0);
    case O_NE: return truth_reference(scmp(v1, v2) != 0);
    case O_LT: return truth_reference(scmp(v1, v2) < 0);
    case O_LE: return truth_reference(scmp(v1, v2) <= 0);
    case O_GT: return truth_reference(scmp(v1, v2) > 0);
    case O_GE: return truth_reference(scmp(v1, v2) >= 0);
    case O_CONCAT:
        if (!v1->u.str.len) {
            return val_reference(v2);
        }
        if (!v2->u.str.len) {
            return val_reference(v1);
        }
        {
            uint8_t *s;
            size_t ln = v1->u.str.len + v2->u.str.len;
            size_t ch = v1->u.str.chars + v2->u.str.chars;
            if (ln < v2->u.str.len) err_msg_out_of_memory(); /* overflow */

            v = val_alloc(STR_OBJ);
            s = snew(v, v->u.str.len);
            memcpy(s, v1->u.str.data, v1->u.str.len);
            memcpy(s + v1->u.str.len, v2->u.str.data, v2->u.str.len);
            v->u.str.len = ln;
            v->u.str.chars = ch;
            v->u.str.data = s;
            return v;
        }
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.str.len) return val_reference(true_value);
            if (v1->u.str.len > v2->u.str.len) return val_reference(false_value);
            c2 = v2->u.str.data;
            e = c2 + v2->u.str.len - v1->u.str.len;
            for (;;) {
                c = (uint8_t *)memchr(c2, v1->u.str.data[0], e - c2 + 1);
                if (!c) return val_reference(false_value);
                if (!memcmp(c, v1->u.str.data, v1->u.str.len)) return val_reference(true_value);
                c2 = c + 1;
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK value_t repeat(oper_t op) {
    value_t v1 = op->v1, v;
    uval_t rep;

    v = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (v) return v;

    if (v1->u.str.len && rep) {
        uint8_t *s;
        size_t ln;
        size_t chars;
        if (rep == 1) {
            return val_reference(v1);
        }
        chars = v1->u.str.chars;
        ln = v1->u.str.len;
        if (ln > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = val_alloc(STR_OBJ);
        s = snew(v, ln * rep);
        v->u.str.len = 0;
        v->u.str.chars = chars * rep;
        while (rep--) {
            memcpy(s + v->u.str.len, v1->u.str.data, ln);
            v->u.str.len += ln;
        }
        v->u.str.data = s;
        return v;
    } 
    return val_reference(null_str);
}

static inline MUST_CHECK value_t slice(value_t v2, oper_t op, size_t ln) {
    size_t len2;
    uint8_t *p;
    uint8_t *p2;
    value_t v, v1 = op->v1;
    size_t length;
    ival_t offs, end, step;

    v = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (v) return v;

    if (!length) {
        return val_reference(null_str);
    }
    if (step == 1) {
        if (length == v1->u.str.chars) {
            return val_reference(v1); /* original string */
        }
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = length;
        } else {
            ival_t i;
            p = v1->u.str.data;
            for (i = 0; i < offs; i++) {
                p += utf8len(*p);
            }
            offs = p - v1->u.str.data;
            for (; i < end; i++) {
                p += utf8len(*p);
            }
            len2 = p - v1->u.str.data - offs;
        }
        v = val_alloc(STR_OBJ);
        p = p2 = snew(v, len2);
        memcpy(p2, v1->u.str.data + offs, len2);
    } else {
        v = val_alloc(STR_OBJ);
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = length;
            p = p2 = snew(v, len2);
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p2++ = v1->u.str.data[offs];
                offs += step;
            }
        }
        else {
            ival_t i, j, k;
            uint8_t *o;
            o = p2 = snew(v, v1->u.str.len);
            p = v1->u.str.data;
            for (i = 0; i < offs; i++) {
                p += utf8len(*p);
            }
            if (step > 0) {
                for (k = i; i < end; i++) {
                    j = utf8len(*p);
                    if (i == k) {memcpy(p2, p, j);p2 += j; k += step;}
                    p += j;
                }
            } else {
                p += utf8len(*p);
                for (k = i; i > end; i--) {
                    j = 0;
                    do {
                        p--;j++;
                    } while (*p >= 0x80 && *p < 0xc0);
                    if (i == k) {memcpy(p2, p, j);p2 += j; k += step;}
                }
            }
            len2 = p2 - o;
            if (len2 > sizeof(v->u.str.val) && o != v->u.str.val) {
                if (len2 <= sizeof(v->u.str.val)) {
                    memcpy(v->u.str.val, o, len2);
                    free(o);
                    p = v->u.str.val;
                } else {
                    p = (uint8_t *)realloc(o, len2);
                    if (!p) err_msg_out_of_memory();
                }
            } else p = o;
        }
    }
    v->u.str.chars = length;
    v->u.str.len = len2;
    v->u.str.data = p;
    return v;
}

static inline MUST_CHECK value_t iindex(oper_t op) {
    uint8_t *p;
    uint8_t *p2;
    size_t offs, len1, len2;
    size_t i;
    value_t v1 = op->v1, v2 = op->v2, v, err;

    if (v2->u.funcargs.len != 1) {
        err_msg_argnum(v2->u.funcargs.len, 1, 1, op->epoint2);
        return val_reference(none_value);
    }
    v2 = v2->u.funcargs.val->val;

    len1 = v1->u.str.chars;

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            return val_reference(null_str);
        }
        v = val_alloc(STR_OBJ);
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = v2->u.list.len;
            p = p2 = snew(v, len2);
            for (i = 0; i < len2; i++) {
                err = indexoffs(v2->u.list.data[i], len1, &offs, op->epoint2);
                if (err) {
                    v->u.str.data = p;
                    val_destroy(v);
                    return err;
                }
                *p2++ = v1->u.str.data[offs];
            }
            len1 = i;
        }
        else {
            size_t m = v1->u.str.len;
            uint8_t *o;
            size_t j = 0;
            o = p2 = snew(v, m);
            p = v1->u.str.data;

            for (i = 0; i < v2->u.list.len; i++) {
                int k;
                err = indexoffs(v2->u.list.data[i], len1, &offs, op->epoint2);
                if (err) {
                    v->u.str.data = o;
                    val_destroy(v);
                    return err;
                }
                while (offs != j) {
                    if (offs > j) {
                        p += utf8len(*p);
                        j++;
                    } else {
                        do { p--; } while (*p >= 0x80 && *p < 0xc0);
                        j--;
                    }
                }
                k = utf8len(*p);
                if ((size_t)(p2 + k - o) > m) {
                    const uint8_t *r = o;
                    m += 4096;
                    if (o != v->u.str.val) o = (uint8_t *)realloc(o, m);
                    else {
                        o = (uint8_t *)malloc(m);
                        if (o) memcpy(o, v->u.str.val, m - 4096);
                    }
                    if (!o || m < 4096) err_msg_out_of_memory(); /* overflow */
                    p2 += o - r;
                }
                memcpy(p2, p, k);p2 += k;
            }
            len1 = i;
            len2 = p2 - o;
            if (len2 > sizeof(v->u.str.val) && o != v->u.str.val) {
                if (len2 <= sizeof(v->u.str.val)) {
                    memcpy(v->u.str.val, o, len2);
                    free(o);
                    p = v->u.str.val;
                } else {
                    p = (uint8_t *)realloc(o, len2);
                    if (!p) err_msg_out_of_memory();
                }
            } else p = o;
        }
        v->u.str.chars = len1;
        v->u.str.len = len2;
        v->u.str.data = p;
        return v;
    }
    if (v2->obj == COLONLIST_OBJ) {
        return slice(v2, op, len1);
    }
    err = indexoffs(v2, len1, &offs, op->epoint2);
    if (err) return err;

    v = val_alloc(STR_OBJ);
    if (v1->u.str.len == v1->u.str.chars) {
        len1 = 1;
        p2 = v->u.str.val;
        p2[0] = v1->u.str.data[offs];
    }
    else {
        p = v1->u.str.data;
        while (offs--) p += utf8len(*p);
        len1 = utf8len(*p);
        p2 = snew(v, len1);
        memcpy(p2, p, len1);
    }
    v->u.str.data = p2;
    v->u.str.chars = 1;
    v->u.str.len = len1;
    return v;
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp;

    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op); 
    }
    switch (v2->obj->type) {
    case T_STR: return calc2_str(op);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            value_t result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: tmp = bits_from_str(v1, op->epoint); break;
            default: tmp = int_from_str(v1, op->epoint);
            }
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            value_t result;
            tmp = bytes_from_str(v1, op->epoint);
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_GAP:
    case T_REGISTER:
    case T_DICT:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp;
    switch (v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            value_t result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = bits_from_str(v2, op->epoint2); break;
            default: tmp = int_from_str(v2, op->epoint2);
            }
            op->v2 = tmp;
            result = v1->obj->calc2(op);
            op->v2 = v2;
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            value_t result;
            tmp = bytes_from_str(v2, op->epoint2);
            op->v2 = tmp;
            result = v1->obj->calc2(op);
            op->v2 = v2;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

void strobj_init(void) {
    obj_init(&obj, T_STR, "str");
    obj.create = create;
    obj.destroy = destroy;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.sign = sign;
    obj.abs = absolute;
    obj.len = len;
    obj.getiter = getiter;
    obj.next = next;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
