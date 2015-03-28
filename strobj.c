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
#include "strobj.h"
#include "eval.h"
#include "misc.h"
#include "unicode.h"
#include "error.h"
#include "variables.h"

#include "boolobj.h"
#include "floatobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "listobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type obj;

Type *STR_OBJ = &obj;
Str *null_str;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_STR: return val_reference(v1);
    default: return v1->obj->repr(v1, epoint); 
    }
}

static void destroy(Obj *o1) {
    Str *v1 = (Str *)o1;
    if (v1->val != v1->data) free(v1->data);
}

static uint8_t *snew(Str *v, size_t len) {
    if (len > sizeof(v->val)) {
        return (uint8_t *)mallocx(len);
    }
    return v->val;
}

static int same(const Obj *o1, const Obj *o2) {
    const Str *v1 = (const Str *)o1, *v2 = (const Str *)o2;
    return o2->obj == STR_OBJ && v1->len == v2->len && (
            v1->data == v2->data || !memcmp(v1->data, v2->data, v2->len));
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t epoint) {
    Str *v1 = (Str *)o1;
    Obj *tmp, *ret;
    tmp = (Obj *)bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    ret = tmp->obj->truth(tmp, type, epoint);
    val_destroy(tmp);
    return ret;
}

size_t str_quoting(const uint8_t *data, size_t ln, char *q) {
    size_t i, sq = 0, dq = 0;
    for (i = 0; i < ln; i++) {
        switch (data[i]) {
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

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint)) {
    Str *v1 = (Str *)o1;
    size_t i2, i;
    uint8_t *s, *s2;
    char q;
    Str *v = new_str();
    i = str_quoting(v1->data, v1->len, &q);

    i2 = i + 2;
    if (i2 < 2) err_msg_out_of_memory(); /* overflow */
    s2 = s = snew(v, i2);

    *s++ = q;
    for (i = 0; i < v1->len; i++) {
        s[i] = v1->data[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    v->data = s2;
    v->len = i2;
    v->chars = i2 - (i - v1->chars);
    return &v->v;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Str *v1 = (Str *)o1;
    size_t l = v1->len;
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, int bits, linepos_t epoint) {
    Str *v1 = (Str *)o1;
    Obj *tmp;
    Error *ret;
    tmp = (Obj *)bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    ret = tmp->obj->ival(tmp, iv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, int bits, linepos_t epoint) {
    Str *v1 = (Str *)o1;
    Obj *tmp;
    Error *ret;
    tmp = (Obj *)bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    ret = tmp->obj->uval(tmp, uv, bits, epoint);
    val_destroy(tmp);
    return ret;
}

MUST_CHECK Obj *float_from_str(const Str *v1, linepos_t epoint) {
    Obj *tmp, *ret;
    tmp = (Obj *)bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    if (tmp->obj != BYTES_OBJ) return tmp;
    ret = float_from_bytes((Bytes *)tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    Str *v1 = (Str *)o1;
    Obj *tmp, *ret;
    tmp = bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    ret = tmp->obj->sign(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t epoint) {
    Str *v1 = (Str *)o1;
    return int_from_str(v1, epoint);
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    Str *v1 = (Str *)o1;
    return (Obj *)int_from_size(v1->chars);
}

static MUST_CHECK Iter *getiter(Obj *v1) {
    Iter *v = (Iter *)val_alloc(ITER_OBJ);
    v->val = 0;
    v->iter = &v->val;
    v->data = val_reference(v1);
    return v;
}

static MUST_CHECK Obj *next(Iter *v1) {
    const Str *vv1 = (Str *)v1->data;
    int ln;
    uint8_t *s;
    Str *v;
    if (v1->val >= vv1->len) return NULL;
    ln = utf8len(vv1->data[v1->val]);
    v = new_str();
    s = snew(v, ln);
    memcpy(s, vv1->data + v1->val, ln);
    v1->val += ln;

    v->chars = 1;
    v->len = ln;
    v->data = s;
    return &v->v;
}

MUST_CHECK Obj *str_from_str(const uint8_t *s, size_t *ln) {
    Str *v;
    size_t i2 = 0;
    size_t i, j;
    size_t r = 0;
    uint8_t ch2, ch = s[0];

    i = 1;
    for (;;) {
        if (!(ch2 = s[i])) {
            err_msg(ERROR______EXPECTED, "end of string");
            *ln = i;
            return (Obj *)ref_none();
        }
        if (ch2 & 0x80) i += utf8len(ch2); else i++;
        if (ch2 == ch) {
            if (s[i] == ch && !arguments.tasmcomp) {i++;r++;} /* handle 'it''s' */
            else break; /* end of string; */
        }
        i2++;
    }
    v = new_str();
    if (r) {
        const uint8_t *p = s + 1, *p2;
        uint8_t *d;
        j = i - 2;
        v->len = j - r;
        v->chars = i2;
        d = snew(v, v->len);
        v->data = d;
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
        v->len = (i > 1) ? (i - 2) : 0;
        v->chars = i2;
        v->data = snew(v, v->len);
        memcpy(v->data, s + 1, v->len);
    }
    *ln = i;
    return &v->v;
}

uint8_t *str_create_elements(Str *v, size_t ln) {
    return snew(v, ln);
}

static int scmp(Str *v1, Str *v2) {
    int h = memcmp(v1->data, v2->data, (v1->len < v2->len) ? v1->len : v2->len);
    if (h) return h;
    return (v1->len > v2->len) - (v1->len < v2->len);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Str *v1 = (Str *)op->v1;
    Obj *v, *tmp;
    switch (op->op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV: tmp = bytes_from_str(v1, op->epoint, BYTES_MODE_TEXT); break;
    case O_NEG:
    case O_POS:
    case O_STRING: tmp = int_from_str(v1, op->epoint); break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    v = tmp->obj->calc1(op);
    op->v1 = &v1->v;
    val_destroy(tmp);
    return v;
}

static MUST_CHECK Obj *calc2_str(oper_t op) {
    Str *v1 = (Str *)op->v1, *v2 = (Str *)op->v2, *v;
    int val;
    switch (op->op->op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
        {
            Obj *tmp, *tmp2, *result;
            tmp = int_from_str(v1, op->epoint);
            tmp2 = int_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = &v1->v;
            op->v2 = &v2->v;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_AND:
    case O_OR:
    case O_XOR:
        {
            Obj *tmp, *tmp2, *result;
            tmp = bytes_from_str(v1, op->epoint, BYTES_MODE_TEXT);
            tmp2 = bytes_from_str(v2, op->epoint2, BYTES_MODE_TEXT);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = &v1->v;
            op->v2 = &v2->v;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_LSHIFT:
    case O_RSHIFT:
        {
            Obj *tmp, *tmp2, *result;
            tmp = bits_from_str(v1, op->epoint);
            tmp2 = bits_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = &v1->v;
            op->v2 = &v2->v;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_CMP:
        val = scmp(v1, v2);
        if (val < 0) return (Obj *)ref_int(minus1_value);
        return (Obj *)ref_int(int_value[val > 0]);
    case O_EQ: return truth_reference(scmp(v1, v2) == 0);
    case O_NE: return truth_reference(scmp(v1, v2) != 0);
    case O_LT: return truth_reference(scmp(v1, v2) < 0);
    case O_LE: return truth_reference(scmp(v1, v2) <= 0);
    case O_GT: return truth_reference(scmp(v1, v2) > 0);
    case O_GE: return truth_reference(scmp(v1, v2) >= 0);
    case O_CONCAT:
        if (!v1->len) {
            return (Obj *)ref_str(v2);
        }
        if (!v2->len) {
            return (Obj *)ref_str(v1);
        }
        {
            uint8_t *s;
            size_t ln = v1->len + v2->len;
            size_t ch = v1->chars + v2->chars;
            if (ln < v2->len) err_msg_out_of_memory(); /* overflow */

            v = new_str();
            s = snew(v, ln);
            memcpy(s, v1->data, v1->len);
            memcpy(s + v1->len, v2->data, v2->len);
            v->len = ln;
            v->chars = ch;
            v->data = s;
            return &v->v;
        }
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->len) return (Obj *)ref_bool(true_value);
            if (v1->len > v2->len) return (Obj *)ref_bool(false_value);
            c2 = v2->data;
            e = c2 + v2->len - v1->len;
            for (;;) {
                c = (uint8_t *)memchr(c2, v1->data[0], e - c2 + 1);
                if (!c) return (Obj *)ref_bool(false_value);
                if (!memcmp(c, v1->data, v1->len)) return (Obj *)ref_bool(true_value);
                c2 = c + 1;
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Str *v1 = (Str *)op->v1, *v;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (err) return &err->v;

    if (v1->len && rep) {
        uint8_t *s;
        size_t ln;
        size_t chars;
        if (rep == 1) {
            return (Obj *)ref_str(v1);
        }
        chars = v1->chars;
        ln = v1->len;
        if (ln > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = new_str();
        s = snew(v, ln * rep);
        v->len = 0;
        v->chars = chars * rep;
        while (rep--) {
            memcpy(s + v->len, v1->data, ln);
            v->len += ln;
        }
        v->data = s;
        return &v->v;
    } 
    return (Obj *)ref_str(null_str);
}

static inline MUST_CHECK Obj *slice(Colonlist *v2, oper_t op, size_t ln) {
    size_t len2;
    uint8_t *p;
    uint8_t *p2;
    Str *v, *v1 = (Str *)op->v1;
    size_t length;
    ival_t offs, end, step;
    Obj *err;

    err = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (err) return err;

    if (!length) {
        return (Obj *)ref_str(null_str);
    }
    if (step == 1) {
        if (length == v1->chars) {
            return (Obj *)ref_str(v1); /* original string */
        }
        if (v1->len == v1->chars) {
            len2 = length;
        } else {
            ival_t i;
            p = v1->data;
            for (i = 0; i < offs; i++) {
                p += utf8len(*p);
            }
            offs = p - v1->data;
            for (; i < end; i++) {
                p += utf8len(*p);
            }
            len2 = p - v1->data - offs;
        }
        v = new_str();
        p = p2 = snew(v, len2);
        memcpy(p2, v1->data + offs, len2);
    } else {
        v = new_str();
        if (v1->len == v1->chars) {
            len2 = length;
            p = p2 = snew(v, len2);
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p2++ = v1->data[offs];
                offs += step;
            }
        }
        else {
            ival_t i, j, k;
            uint8_t *o;
            o = p2 = snew(v, v1->len);
            p = v1->data;
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
            if (o != v->val) {
                if (len2 <= sizeof(v->val)) {
                    memcpy(v->val, o, len2);
                    free(o);
                    p = v->val;
                } else {
                    p = (uint8_t *)reallocx(o, len2);
                }
            } else p = o;
        }
    }
    v->chars = length;
    v->len = len2;
    v->data = p;
    return &v->v;
}

static inline MUST_CHECK Obj *iindex(oper_t op) {
    uint8_t *p;
    uint8_t *p2;
    size_t offs, len1, len2;
    size_t i;
    Str *v1 = (Str *)op->v1, *v;
    Obj *o2 = op->v2;
    Error *err;
    Funcargs *args = (Funcargs *)o2;

    if (args->len != 1) {
        err_msg_argnum(args->len, 1, 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val->val;

    len1 = v1->chars;

    if (o2->obj == LIST_OBJ) {
        List *v2 = (List *)o2;
        if (!v2->len) {
            return (Obj *)ref_str(null_str);
        }
        v = new_str();
        if (v1->len == v1->chars) {
            len2 = v2->len;
            p = p2 = snew(v, len2);
            for (i = 0; i < len2; i++) {
                err = indexoffs(v2->data[i], len1, &offs, op->epoint2);
                if (err) {
                    v->data = p;
                    val_destroy(&v->v);
                    return &err->v;
                }
                *p2++ = v1->data[offs];
            }
            len1 = i;
        }
        else {
            size_t m = v1->len;
            uint8_t *o;
            size_t j = 0;
            o = p2 = snew(v, m);
            p = v1->data;

            for (i = 0; i < v2->len; i++) {
                int k;
                err = indexoffs(v2->data[i], len1, &offs, op->epoint2);
                if (err) {
                    v->data = o;
                    val_destroy(&v->v);
                    return &err->v;
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
                    if (m < 4096) err_msg_out_of_memory(); /* overflow */
                    if (o != v->val) o = (uint8_t *)reallocx(o, m);
                    else {
                        o = (uint8_t *)mallocx(m);
                        memcpy(o, v->val, m - 4096);
                    }
                    p2 += o - r;
                }
                memcpy(p2, p, k);p2 += k;
            }
            len1 = i;
            len2 = p2 - o;
            if (o != v->val) {
                if (len2 <= sizeof(v->val)) {
                    memcpy(v->val, o, len2);
                    free(o);
                    p = v->val;
                } else {
                    p = (uint8_t *)reallocx(o, len2);
                }
            } else p = o;
        }
        v->chars = len1;
        v->len = len2;
        v->data = p;
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        return slice((Colonlist *)o2, op, len1);
    }
    err = indexoffs(o2, len1, &offs, op->epoint2);
    if (err) return &err->v;

    v = new_str();
    if (v1->len == v1->chars) {
        len1 = 1;
        p2 = v->val;
        p2[0] = v1->data[offs];
    }
    else {
        p = v1->data;
        while (offs--) p += utf8len(*p);
        len1 = utf8len(*p);
        p2 = snew(v, len1);
        memcpy(p2, p, len1);
    }
    v->data = p2;
    v->chars = 1;
    v->len = len1;
    return &v->v;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Str *v1 = (Str *)op->v1;
    Obj *v2 = op->v2;
    Obj *tmp;

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
            Obj *result;
            switch (op->op->op) {
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
            op->v1 = &v1->v;
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            Obj *result;
            tmp = bytes_from_str(v1, op->epoint, BYTES_MODE_TEXT);
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = &v1->v;
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

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Str *v2 = (Str *)op->v2;
    Obj *v1 = op->v1;
    Obj *tmp;
    switch (v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            Obj *result;
            switch (op->op->op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = bits_from_str(v2, op->epoint2); break;
            default: tmp = int_from_str(v2, op->epoint2);
            }
            op->v2 = tmp;
            result = v1->obj->calc2(op);
            op->v2 = &v2->v;
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            Obj *result;
            tmp = bytes_from_str(v2, op->epoint2, BYTES_MODE_TEXT);
            op->v2 = tmp;
            result = v1->obj->calc2(op);
            op->v2 = &v2->v;
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
    new_type(&obj, T_STR, "str", sizeof(Str));
    obj_init(&obj);
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

    null_str = new_str();
    null_str->len = 0;
    null_str->chars = 0;
    null_str->data = NULL;
}

void strobj_names(void) {
    new_builtin("str", val_reference(&STR_OBJ->v));
}

void strobj_destroy(void) {
#ifdef DEBUG
    if (null_str->v.refcount != 1) fprintf(stderr, "str %d\n", null_str->v.refcount - 1);
#endif

    val_destroy(&null_str->v);
}
