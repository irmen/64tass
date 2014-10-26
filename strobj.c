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

struct encoding_s;
static struct obj_s str_obj;
static struct obj_s register_obj;

obj_t STR_OBJ = &str_obj;
obj_t REGISTER_OBJ = &register_obj;

static void destroy(struct value_s *v1) {
    if (v1->u.str.val != v1->u.str.data) free(v1->u.str.data);
}

static uint8_t *snew(struct value_s *v, size_t len) {
    if (len > sizeof(v->u.str.val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->u.str.val;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    v->obj = v1->obj;
    v->u.str.chars = v1->u.str.chars;
    v->u.str.len = v1->u.str.len;
    if (v1->u.str.len) {
        s = snew(v, v->u.str.len);
        memcpy(s, v1->u.str.data, v->u.str.len);
    } else s = NULL;
    v->u.str.data = s;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->u.str.chars = v1->u.str.chars;
    v->u.str.len = v1->u.str.len;
    if (v1->u.str.data == v1->u.str.val) {
        v->u.str.data = v->u.str.val;
        if (v->u.str.len) memcpy(v->u.str.data, v1->u.str.data, v->u.str.len);
    } else v->u.str.data = v1->u.str.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v1->obj == v2->obj && v1->u.str.len == v2->u.str.len && (
            v1->u.str.data == v2->u.str.data ||
            !memcmp(v1->u.str.data, v2->u.str.data, v2->u.str.len));
}

static int truth(const struct value_s *v1, struct value_s *v, enum truth_e type, linepos_t epoint) {
    int ret;
    struct value_s tmp;
    bytes_from_str(&tmp, v1, epoint);
    if (v == v1) destroy(v);
    ret = tmp.obj->truth(&tmp, v, type, epoint);
    obj_destroy(&tmp);
    return ret;
}

static void repr(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t i2, i, sq = 0, dq = 0;
    uint8_t *s, *s2;
    char q;
    struct value_s tmp;
    for (i = 0; i < v1->u.str.len; i++) {
        switch (v1->u.str.data[i]) {
        case '\'': sq++; continue;
        case '"': dq++; continue;
        }
    }
    if (sq < dq) {
        i += sq;
        if (i < sq) err_msg_out_of_memory(); /* overflow */
        q = '\'';
    } else {
        i += dq;
        if (i < dq) err_msg_out_of_memory(); /* overflow */
        q = '"';
    }

    i2 = i + 2;
    if (i2 < 2) err_msg_out_of_memory(); /* overflow */
    s2 = s = snew(&tmp, i2);

    *s++ = q;
    for (i = 0; i < v1->u.str.len; i++) {
        s[i] = v1->u.str.data[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    if (v == v1) destroy(v);
    if (s2 == tmp.u.str.val) {
        memcpy(v->u.str.val, s2, i2);
        s2 = v->u.str.val;
    }
    v->obj = STR_OBJ;
    v->u.str.data = s2;
    v->u.str.len = i2;
    v->u.str.chars = i2;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    size_t l = v1->u.str.len;
    const uint8_t *s2 = v1->u.str.data;
    unsigned int h;
    if (!l) return 0;
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.str.len;
    return h & ((~(unsigned int)0) >> 1);
}

static void str(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    if (v != v1) copy(v1, v);
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s tmp, tmp2;
    int ret;
    bits_from_str(&tmp, v1, epoint);
    ret = tmp.obj->ival(&tmp, &tmp2, iv, bits, epoint);
    obj_destroy(&tmp);
    if (ret) {
        if (v1 == v) destroy(v);
        tmp2.obj->copy_temp(&tmp2, v);
    }
    return ret;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s tmp, tmp2;
    int ret;
    bits_from_str(&tmp, v1, epoint);
    ret = tmp.obj->uval(&tmp, &tmp2, uv, bits, epoint);
    obj_destroy(&tmp);
    if (ret) {
        if (v1 == v) destroy(v);
        tmp2.obj->copy_temp(&tmp2, v);
    }
    return ret;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    struct value_s tmp, tmp2;
    int ret;
    bits_from_str(&tmp, v1, epoint);
    ret = tmp.obj->real(&tmp, &tmp2, r, epoint);
    obj_destroy(&tmp);
    if (ret) {
        if (v1 == v) destroy(v);
        tmp2.obj->copy_temp(&tmp2, v);
    }
    return ret;
}

static void sign(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    bytes_from_str(&tmp, v1, epoint);
    if (v1 == v) destroy(v);
    tmp.obj->sign(&tmp, v, epoint);
    obj_destroy(&tmp);
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    int_from_str(&tmp, v1, epoint);
    if (v == v1) destroy(v);
    tmp.obj->abs(&tmp, v, epoint);
    obj_destroy(&tmp);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    int_from_str(&tmp, v1, epoint);
    if (v == v1) destroy(v);
    tmp.obj->copy_temp(&tmp, v);
}

static void len(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t uv = v1->u.str.chars;
    if (v1 == v) destroy(v);
    int_from_uval(v, uv);
}

static void getiter(struct value_s *v1, struct value_s *v) {
    v->obj = ITER_OBJ;
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
}

static MUST_CHECK struct value_s *next(struct value_s *v1, struct value_s *v) {
    const struct value_s *vv1 = v1->u.iter.data;
    int ln;
    uint8_t *s;
    if (v1->u.iter.val >= vv1->u.str.len) return NULL;
    ln = utf8len(vv1->u.str.data[v1->u.iter.val]);
    s = snew(v, ln);
    memcpy(s, vv1->u.str.data + v1->u.iter.val, ln);
    v1->u.iter.val += ln;

    v->obj = STR_OBJ;
    v->u.str.chars = 1;
    v->u.str.len = ln;
    v->u.str.data = s;
    return v;
}

size_t str_from_str(struct value_s *v, const uint8_t *s) {
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
    if (r) {
        const uint8_t *p = s + 1, *p2;
        uint8_t *d;
        j = i - 2;
        v->obj = STR_OBJ;
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
        v->obj = STR_OBJ;
        v->u.str.len = (i > 1) ? (i - 2) : 0;
        v->u.str.chars = i2;
        v->u.str.data = snew(v, v->u.str.len);
        memcpy(v->u.str.data, s + 1, v->u.str.len);
    }
    return i;
}

uint8_t *str_create_elements(struct value_s *v, size_t ln) {
    return snew(v, ln);
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
    struct value_s tmp;
    switch (op->op->u.oper.op) {
    case O_NEG:
    case O_POS:
    case O_STRING: int_from_str(&tmp, v1, op->epoint); break;
    case O_INV:
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD: bits_from_str(&tmp, v1, op->epoint); break;
    default: obj_oper_error(op); return;
    }
    if (v == v1) destroy(v);
    op->v1 = &tmp;
    tmp.refcount = 0;
    tmp.obj->calc1(op);
    op->v1 = v1;
    obj_destroy(&tmp);
}

static MUST_CHECK struct value_s *calc2_str(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    int val;
    switch (op->op->u.oper.op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
        {
            struct value_s tmp, tmp2, *result;
            int_from_str(&tmp, v1, op->epoint);
            int_from_str(&tmp2, v2, op->epoint2);
            if (v1 == v || v2 == v) {destroy(v); v->obj = NONE_OBJ;}
            op->v1 = &tmp;
            op->v2 = &tmp2;
            tmp.refcount = 0;
            tmp2.refcount = 0;
            result = tmp.obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            obj_destroy(&tmp2);
            obj_destroy(&tmp);
            return result;
        }
    case O_AND:
    case O_OR:
    case O_XOR:
    case O_LSHIFT:
    case O_RSHIFT:
        {
            struct value_s tmp, tmp2, *result;
            bits_from_str(&tmp, v1, op->epoint);
            bits_from_str(&tmp2, v2, op->epoint2);
            if (v1 == v || v2 == v) {destroy(v); v->obj = NONE_OBJ;}
            op->v1 = &tmp;
            op->v2 = &tmp2;
            tmp.refcount = 0;
            tmp2.refcount = 0;
            result = tmp.obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            obj_destroy(&tmp2);
            obj_destroy(&tmp);
            return result;
        }
    case O_CMP:
        {
            int h = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len : v2->u.str.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.str.len > v2->u.str.len) - (v1->u.str.len < v2->u.str.len);
            if (v == v1 || v == v2) destroy(v);
            int_from_int(v, h);
            return NULL;
        }
    case O_EQ: return truth_reference(v1->u.str.len == v2->u.str.len && (v1->u.str.data == v2->u.str.data || !memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len)));
    case O_NE: return truth_reference(v1->u.str.len != v2->u.str.len || (v1->u.str.data != v2->u.str.data && memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len)));
    case O_LT:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        return truth_reference(val ? (val < 0) : (v1->u.str.len < v2->u.str.len));
    case O_GT:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        return truth_reference(val ? (val > 0) : (v1->u.str.len > v2->u.str.len));
    case O_LE:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        return truth_reference(val ? (val <= 0) : (v1->u.str.len <= v2->u.str.len));
    case O_GE:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        return truth_reference(val ? (val >= 0) : (val = v1->u.str.len >= v2->u.str.len));
    case O_CONCAT:
        {
            uint8_t *s;
            size_t ln = v1->u.str.len + v2->u.str.len;
            size_t ch = v1->u.str.chars + v2->u.str.chars;
            if (ln < v2->u.str.len) err_msg_out_of_memory(); /* overflow */

            if (v == v1) {
                if (!v2->u.str.len) return NULL;
                s = (uint8_t *)v1->u.str.data;
                if (s != v1->u.str.val) {
                    s = (uint8_t *)realloc(s, v->u.str.len);
                    if (!s) err_msg_out_of_memory();
                } else {
                    s = snew(v, v->u.str.len);
                    if (s != v1->u.str.val) memcpy(s, v1->u.str.val, v1->u.str.len);
                }
                memcpy(s + v1->u.str.len, v2->u.str.data, v2->u.str.len);
            } else {
                s = snew(v, v->u.str.len);
                memcpy(s, v1->u.str.data, v1->u.str.len);
                memcpy(s + v1->u.str.len, v2->u.str.data, v2->u.str.len);
                if (v == v2) destroy(v);
                v->obj = STR_OBJ;
            }
            v->u.str.len = ln;
            v->u.str.chars = ch;
            v->u.str.data = s;
            return NULL;
        }
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.str.len) return truth_reference(1);
            if (v1->u.str.len > v2->u.str.len) return truth_reference(0);
            c2 = v2->u.str.data;
            e = c2 + v2->u.str.len - v1->u.str.len;
            for (;;) {
                c = (uint8_t *)memchr(c2, v1->u.str.data[0], e - c2 + 1);
                if (!c) return truth_reference(0);
                if (!memcmp(c, v1->u.str.data, v1->u.str.len)) return truth_reference(1);
                c2 = c + 1;
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *repeat(oper_t op, uval_t rep) {
    struct value_s *v1 = op->v1, *v = op->v, tmp;
    if (v1->u.str.len && rep) {
        uint8_t *s;
        size_t ln = v1->u.str.len;
        size_t chars = v1->u.str.chars;
        if (ln > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        s = snew(&tmp, ln * rep);
        v->obj = STR_OBJ;
        v->u.str.len = 0;
        while (rep--) {
            memcpy(s + v->u.str.len, v1->u.str.data, ln);
            v->u.str.len += ln;
        }
        if (v == v1) destroy(v);
        if (s == tmp.u.str.val) {
            memcpy(v->u.str.val, s, v->u.str.len);
            s = v->u.str.val;
        }
        v->u.str.data = s;
        v->u.str.chars = chars * rep;
        return NULL;
    } 
    null_str->refcount++;
    return null_str;
}

static MUST_CHECK struct value_s *calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v2->obj->type) {
    case T_STR: return calc2_str(op);
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
            case O_RSHIFT: bits_from_str(&tmp, v1, op->epoint); break;
            default: int_from_str(&tmp, v1, op->epoint);
            }
            if (v1 == v) {obj_destroy(v); v->obj = NONE_OBJ;}
            op->v1 = &tmp;
            tmp.refcount = 0;
            result = tmp.obj->calc2(op);
            op->v1 = v1;
            obj_destroy(&tmp);
            return result;
        }
    case T_BYTES:
        {
            struct value_s *result;
            bytes_from_str(&tmp, v1, op->epoint);
            if (v1 == v) {obj_destroy(v); v->obj = NONE_OBJ;}
            op->v1 = &tmp;
            tmp.refcount = 0;
            result = tmp.obj->calc2(op);
            op->v1 = v1;
            obj_destroy(&tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
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
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v1->obj->type) {
    case T_STR: return calc2_str(op);
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
            case O_XOR: bits_from_str(&tmp, v2, op->epoint2); break;
            default: int_from_str(&tmp, v2, op->epoint2);
            }
            if (v2 == v) {obj_destroy(v); v->obj = NONE_OBJ;}
            op->v2 = &tmp;
            tmp.refcount = 0;
            result = tmp.obj->rcalc2(op);
            op->v2 = v2;
            obj_destroy(&tmp);
            return result;
        }
    case T_BYTES:
        {
            struct value_s *result;
            bytes_from_str(&tmp, v2, op->epoint2);
            if (v2 == v) {obj_destroy(v); v->obj = NONE_OBJ;}
            op->v2 = &tmp;
            tmp.refcount = 0;
            result = tmp.obj->rcalc2(op);
            op->v2 = v2;
            obj_destroy(&tmp);
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

static inline MUST_CHECK struct value_s *slice(struct value_s *v1, uval_t len1, ival_t offs, ival_t end, ival_t step, struct value_s *v) {
    size_t len2;
    uint8_t *p;
    uint8_t *p2;
    struct value_s tmp;

    if (!len1) {
        null_str->refcount++;
        return null_str;
    }
    if (step == 1) {
        if (len1 == v1->u.str.chars) {
            if (v1 != v) copy(v1, v);
            return NULL; /* original string */
        }
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = len1;
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
        p = p2 = snew(&tmp, len2);
        memcpy(p2, v1->u.str.data + offs, len2);
    } else {
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = len1;
            p = p2 = snew(&tmp, len2);
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p2++ = v1->u.str.data[offs];
                offs += step;
            }
        }
        else {
            ival_t i, j, k;
            uint8_t *o;
            o = p2 = snew(&tmp, v1->u.str.len);
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
            if (len2 > sizeof(v->u.str.val) && o != tmp.u.str.val) {
                p = (uint8_t *)realloc(o, len2);
                if (!p) err_msg_out_of_memory();
            } else p = o;
        }
    }
    if (v == v1) destroy(v);
    if (len2 <= sizeof(v->u.str.val)) {
        memcpy(v->u.str.val, p, len2);
        if (tmp.u.str.val != p) free(p);
        p = v->u.str.val;
    }
    v->obj = STR_OBJ;
    v->u.str.chars = len1;
    v->u.str.len = len2;
    v->u.str.data = p;
    return NULL;
}

static MUST_CHECK struct value_s *iindex(oper_t op) {
    uint8_t *p;
    uint8_t *p2;
    size_t len1, len2;
    ival_t offs;
    size_t i;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v, tmp, err;

    len1 = v1->u.str.chars;

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            null_str->refcount++;
            return null_str;
        }
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = v2->u.list.len;
            p = p2 = snew(&tmp, len2);
            for (i = 0; i < v2->u.list.len; i++) {
                offs = indexoffs(v2->u.list.data[i], &err, len1, op->epoint2);
                if (offs < 0) {
                    if (p != tmp.u.str.val) free(p);
                    if (v1 == v) destroy(v);
                    err.obj->copy_temp(&err, v);
                    return NULL;
                }
                *p2++ = v1->u.str.data[offs];
            }
        }
        else {
            ival_t j, k;
            size_t m = v1->u.str.len;
            uint8_t *o;
            o = p2 = snew(&tmp, m);
            p = v1->u.str.data;
            j = 0;

            for (i = 0; i < v2->u.list.len; i++) {
                offs = indexoffs(v2->u.list.data[i], &err, len1, op->epoint2);
                if (offs < 0) {
                    if (o != tmp.u.str.val) free(o);
                    if (v1 == v) destroy(v);
                    err.obj->copy_temp(&err, v);
                    return NULL;
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
                    if (o != tmp.u.str.val) o = (uint8_t *)realloc(o, m);
                    else {
                        o = (uint8_t *)malloc(m);
                        if (o) memcpy(o, tmp.u.str.val, m - 4096);
                    }
                    if (!o || m < 4096) err_msg_out_of_memory(); /* overflow */
                    p2 += o - r;
                }
                memcpy(p2, p, k);p2 += k;
            }
            len2 = p2 - o;
            if (len2 > sizeof(v->u.str.val) && o != tmp.u.str.val) {
                p = (uint8_t *)realloc(o, len2);
                if (!p) err_msg_out_of_memory();
            } else p = o;
        }
        if (v == v1) destroy(v);
        if (len2 <= sizeof(v->u.str.val)) {
            memcpy(v->u.str.val, p, len2);
            if (tmp.u.str.val != p) free(p);
            p = v->u.str.val;
        }
        v->obj = STR_OBJ;
        v->u.str.chars = len1;
        v->u.str.len = len2;
        v->u.str.data = p;
        return NULL;
    }
    if (v2->obj == COLONLIST_OBJ) {
        ival_t length, end, step;
        length = sliceparams(op, len1, &offs, &end, &step);
        if (length < 0) return NULL;
        return slice(v1, length, offs, end, step, v);
    }
    offs = indexoffs(v2, &err, len1, op->epoint2);
    if (offs < 0) {
        if (v1 == v) destroy(v);
        err.obj->copy_temp(&err, v);
        return NULL;
    }

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
        memmove(p2, p, len1);
    }
    if (v1 == v) destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = p2;
    v->u.str.chars = 1;
    v->u.str.len = len1;
    return NULL;
}

static void register_repr(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    uint8_t *s;
    const char *prefix = "<register '";
    size_t ln = strlen(prefix), len2 = v1->u.str.len;
    v->obj = STR_OBJ;
    v->u.str.len = v1->u.str.len + 2 + ln;
    v->u.str.chars = v->u.str.chars + 2 + ln;
    if (v->u.str.len < (2 + ln)) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, v->u.str.len);
    memmove(s + ln, v1->u.str.data, len2);
    memcpy(s, prefix, ln);
    s[v->u.str.len - 2] = '\'';
    s[v->u.str.len - 1] = '>';
    v->u.str.data = s;
}

static MUST_CHECK struct value_s *register_calc2(oper_t op) {
    struct value_s *v2 = op->v2;
    switch (v2->obj->type) {
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_CMP:
        case O_EQ:
        case O_NE:
        case O_LT:
        case O_GT:
        case O_LE:
        case O_GE:
            return calc2_str(op);
        default: break;
        }
        break;
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_GAP:
        switch (op->op->u.oper.op) {
        case O_EQ: return truth_reference(0);
        case O_NE: return truth_reference(1);
        default: break;
        }
        break;
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *register_rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2;
    switch (v1->obj->type) {
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_GAP:
        switch (op->op->u.oper.op) {
        case O_EQ: return truth_reference(0);
        case O_NE: return truth_reference(1);
        default: break;
        }
        break;
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


void strobj_init(void) {
    obj_init(&str_obj, T_STR, "<str>");
    str_obj.destroy = destroy;
    str_obj.copy = copy;
    str_obj.copy_temp = copy_temp;
    str_obj.same = same;
    str_obj.truth = truth;
    str_obj.hash = hash;
    str_obj.repr = repr;
    str_obj.str = str;
    str_obj.ival = ival;
    str_obj.uval = uval;
    str_obj.real = real;
    str_obj.sign = sign;
    str_obj.abs = absolute;
    str_obj.integer = integer;
    str_obj.len = len;
    str_obj.getiter = getiter;
    str_obj.next = next;
    str_obj.calc1 = calc1;
    str_obj.calc2 = calc2;
    str_obj.rcalc2 = rcalc2;
    str_obj.repeat = repeat;
    str_obj.iindex = iindex;
    obj_init(&register_obj, T_REGISTER, "<register>");
    register_obj.destroy = destroy;
    register_obj.copy = copy;
    register_obj.copy_temp = copy_temp;
    register_obj.same = same;
    register_obj.hash = hash;
    register_obj.repr = register_repr;
    register_obj.calc2 = register_calc2;
    register_obj.rcalc2 = register_rcalc2;
}
