/*
    $Id: strobj.c 3063 2023-08-27 09:19:30Z soci $

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
#include "strobj.h"
#include <string.h>
#include "eval.h"
#include "unicode.h"
#include "error.h"
#include "variables.h"
#include "arguments.h"
#include "str.h"

#include "boolobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "listobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"

static Type obj;

Type *const STR_OBJ = &obj;

static Str null_strval = { { &obj, 1 }, 0, 0, null_strval.u.val, { { 0 } } };

Obj *const null_str = &null_strval.v;

static inline Str *ref_str(Str *v1) {
    v1->v.refcount++; return v1;
}

MUST_CHECK Obj *str_from_obj(Obj *v1, linepos_t epoint) {
    Obj *v;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_STR:
        return val_reference(v1);
    default:
        v = v1->obj->str(v1, epoint, SIZE_MAX);
        return v != NULL ? v : new_error_mem(epoint);
    }
}

static MUST_CHECK Obj *convert(oper_t op) {
    return str_from_obj(op->v2, op->epoint2);
}

static FAST_CALL NO_INLINE void str_destroy(Str *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    Str *v1 = Str(o1);
    if unlikely(v1->u.val != v1->data) str_destroy(v1);
}

static FAST_CALL NO_INLINE bool str_same(const Str *v1, const Str *v2) {
    return memcmp(v1->data, v2->data, v2->len) == 0;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Str *v1 = Str(o1), *v2 = Str(o2);
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    switch (v1->len) {
    case 0: return true;
    case 1: return v1->data[0] == v2->data[0];
    default: return v1->data == v2->data || str_same(v1, v2);
    }
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    Obj *tmp = bytes_from_str(Str(o1), epoint, BYTES_MODE_TEXT);
    Obj *ret = tmp->obj->truth(tmp, type, epoint);
    val_destroy(tmp);
    return ret;
}

size_t str_quoting(const uint8_t *data, size_t ln, uint8_t *q) {
    size_t i, sq = 0, dq = 0;
    for (i = 0; i < ln; i++) {
        switch (data[i]) {
        case '\'': sq++; continue;
        case '"': dq++; continue;
        }
    }
    if (sq < dq) {
        *q = '\'';
    } else {
        sq = dq;
        *q = '"';
    }
    if (inc_overflow(&i, sq)) err_msg_out_of_memory();
    return i;
}

bool tostr(const struct values_s *v1, str_t *out) {
    Obj *val = v1->val;
    if (val->obj == STR_OBJ) {
        out->len = Str(val)->len;
        out->data = Str(val)->data;
        return false;
    }
    err_msg_wrong_type2(val, STR_OBJ, &v1->epoint);
    return true;
}

MUST_CHECK Obj *tostr2(const struct values_s *v1, str_t *out) {
    Error *err;
    Obj *val = v1->val;

    switch (val->obj->type) {
    case T_ERROR:
    case T_NONE:
        return val_reference(val);
    case T_STR:
        out->len = Str(val)->len;
        out->data = Str(val)->data;
        return NULL;
    default:
        err = new_error(ERROR____WRONG_TYPE, &v1->epoint);
        err->u.otype.t1 = val->obj;
        err->u.otype.t2 = STR_OBJ;
        return Obj(err);
    }
}

MALLOC Str *new_str2(size_t ln) {
    Str *v = Str(val_alloc(STR_OBJ));
    v->len = ln;
    if (ln <= sizeof v->u.val) {
        v->data = v->u.val;
        return v;
    }
    v->u.s.max = ln;
    v->u.s.hash = -1;
    v->data = allocate_array(uint8_t, ln);
    if (v->data == NULL) {
        val_destroy(Obj(v));
        v = NULL;
    }
    return v;
}

uint8_t *extend_str(Str *v, size_t ln) {
    uint8_t *tmp;
    if (ln <= sizeof v->u.val) {
        return v->u.val;
    }
    if (v->u.val != v->data) {
        if (ln <= v->u.s.max) {
            v->u.s.hash = -1;
            return v->data;
        }
        tmp = reallocate_array(v->data, ln);
        if (tmp != NULL) {
            v->data = tmp;
            v->u.s.max = ln;
            v->u.s.hash = -1;
        }
        return tmp;
    }
    tmp = allocate_array(uint8_t, ln);
    if (tmp != NULL) {
        memcpy(tmp, v->u.val, v->len);
        v->data = tmp;
        v->u.s.max = ln;
        v->u.s.hash = -1;
    }
    return tmp;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Str *v1 = Str(o1);
    size_t i2, i, chars;
    uint8_t *s, *s2;
    uint8_t q;
    Str *v;
    i = str_quoting(v1->data, v1->len, &q);

    if (add_overflow(i, 2, &i2)) return NULL;
    chars = i2 - (v1->len - v1->chars);
    if (chars > maxsize) return NULL;
    v = new_str2(i2);
    if (v == NULL) return NULL;
    v->chars = chars;
    s = v->data;

    *s++ = q;
    s2 = v1->data;
    for (i = 0; i < v1->len; i++) {
        s[i] = s2[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    return Obj(v);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Str *v1 = Str(o1);
    if (v1->chars > maxsize) return NULL;
    return val_reference(o1);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Str *v1 = Str(o1);
    size_t l = v1->len;
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (s2 != v1->u.val && v1->u.s.hash >= 0) {
        *hs = v1->u.s.hash;
        return NULL;
    }
    if (l == 0) {
        *hs = 0;
        return NULL;
    }
    h = (unsigned int)*s2 << 7;
    while ((l--) != 0) h = (1000003 * h) ^ *s2++;
    h ^= (unsigned int)v1->len;
    h &= ((~0U) >> 1);
    if (v1->data != v1->u.val) v1->u.s.hash = (int)h;
    *hs = (int)h;
    return NULL;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Str *v1 = Str(o1);
    Obj *tmp = bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    Error *ret = tmp->obj->ival(tmp, iv, bits, epoint);
    if (ret != NULL) error_obj_update(ret, tmp, o1);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Str *v1 = Str(o1);
    Obj *tmp = bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    Error *ret = tmp->obj->uval(tmp, uv, bits, epoint);
    if (ret != NULL) error_obj_update(ret, tmp, o1);
    val_destroy(tmp);
    return ret;
}

MUST_CHECK Obj *float_from_str(Str *v1, linepos_t epoint) {
    Obj *tmp = bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    Obj *ret;
    if (tmp->obj != BYTES_OBJ) return tmp;
    ret = float_from_bytes(Bytes(tmp), epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    Str *v1 = Str(o1);
    Obj *tmp = bytes_from_str(v1, epoint, BYTES_MODE_TEXT);
    Obj *ret = tmp->obj->sign(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *function(oper_t op) {
    Str *v1 = Str(op->v2);
    return int_from_str(v1, op->epoint2);
}

static MUST_CHECK Obj *len(oper_t op) {
    Str *v1 = Str(op->v2);
    return int_from_size(v1->chars);
}

static FAST_CALL MUST_CHECK Obj *iter_forward(struct iter_s *v1) {
    Str *iter;
    const Str *string = Str(v1->data);
    unsigned int ln;
    const uint8_t *s;
    if (v1->val >= string->len) return NULL;
    s = string->data + v1->val;
    ln = utf8len(*s);
    v1->val += ln;
    iter = Str(v1->iter);
    if (iter->v.refcount != 1) {
        iter->v.refcount--;
        iter = new_str(6);
        iter->chars = 1;
        v1->iter = Obj(iter);
    }
    iter->len = ln;
    memcpy(iter->data, s, ln);
    return Obj(iter);
}

static void getiter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_forward;
    v->len = Str(v->data)->chars;
}

static FAST_CALL MUST_CHECK Obj *iter_reverse(struct iter_s *v1) {
    Str *iter;
    const Str *string = Str(v1->data);
    unsigned int ln;
    const uint8_t *s;
    if (v1->val >= string->len) return NULL;
    s = string->data + string->len - v1->val;
    ln = 0;
    do { s--; ln++; } while (*s >= 0x80 && *s < 0xc0);
    v1->val += ln;
    iter = Str(v1->iter);
    if (iter->v.refcount != 1) {
        iter->v.refcount--;
        iter = new_str(6);
        iter->chars = 1;
        v1->iter = Obj(iter);
    }
    iter->len = ln;
    memcpy(iter->data, s, ln);
    return Obj(iter);
}

static void getriter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_reverse;
    v->len = Str(v->data)->chars;
}

MUST_CHECK Obj *str_from_str(const uint8_t *s, linecpos_t *ln, linepos_t epoint) {
    Str *v;
    linecpos_t i2 = 0;
    linecpos_t i, j;
    linecpos_t r = 0;
    uint8_t ch2, ch = s[0];

    i = 1;
    for (;;) {
        if ((ch2 = s[i]) == 0) {
            *ln = i;
            return ref_none();
        }
        if ((ch2 & 0x80) != 0) i += utf8len(ch2); else i++;
        if (ch2 == ch) {
            if (s[i] == ch && !arguments.tasmcomp) {i++;r++;} /* handle 'it''s' */
            else break; /* end of string; */
        }
        i2++;
    }
    *ln = i;
    j = (i > 1) ? (i - 2) : 0;
    if (j == r) return val_reference(null_str);
    if (j - r == 1) {
        v = new_str(1);
        v->chars = 1;
        v->data[0] = s[1];
        return Obj(v);
    }
    v = new_str2(j - r);
    if (v == NULL) return new_error_mem(epoint);
    v->chars = i2;
    if (r != 0) {
        const uint8_t *p = s + 1, *p2;
        uint8_t *d = v->data;
        while (j != 0) {
            p2 = (const uint8_t *)memchr(p, ch, j);
            if (p2 != NULL) {
                linecpos_t l = (linecpos_t)(p2 - p);
                memcpy(d, p, l + 1);
                j -= l + 2;
                d += l + 1; p = p2 + 2;
            } else {
                memcpy(d, p, j);
                j = 0;
            }
        }
    } else {
        memcpy(v->data, s + 1, j);
    }
    return Obj(v);
}

MALLOC Str *new_str(size_t ln) {
    Str *v = Str(val_alloc(STR_OBJ));
    v->len = ln;
    if (ln > sizeof v->u.val) {
        v->u.s.max = ln;
        v->u.s.hash = -1;
        new_array(&v->data, ln);
        return v;
    }
    v->data = v->u.val;
    return v;
}

static int icmp(oper_t op) {
    const Str *v1 = Str(op->v1), *v2 = Str(op->v2);
    int h = memcmp(v1->data, v2->data, (v1->len < v2->len) ? v1->len : v2->len);
    if (h != 0) return h;
    if (v1->len < v2->len) return -1;
    return (v1->len > v2->len) ? 1 : 0;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Str *v1 = Str(op->v1);
    Obj *v, *tmp;
    switch (op->op) {
    case O_LNOT:
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_NEG:
    case O_POS:
    case O_INV: tmp = bytes_from_str(v1, op->epoint, BYTES_MODE_TEXT); break;
    case O_STRING: tmp = int_from_str(v1, op->epoint); break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    op->inplace = (tmp->refcount == 1) ? tmp : NULL;
    v = tmp->obj->calc1(op);
    val_destroy(tmp);
    return v;
}

static MUST_CHECK Obj *calc2_str(oper_t op) {
    Str *v1 = Str(op->v1), *v2 = Str(op->v2), *v;
    int val;
    switch (op->op) {
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
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) {
                error_obj_update(Error(result), tmp, Obj(v1));
                error_obj_update(Error(result), tmp2, Obj(v2));
            }
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
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) {
                error_obj_update(Error(result), tmp, Obj(v1));
                error_obj_update(Error(result), tmp2, Obj(v2));
            }
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_LSHIFT:
    case O_RSHIFT:
        {
            Obj *tmp, *tmp2, *result;
            tmp = bits_from_str(v1, op->epoint);
            tmp2 = int_from_str(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) {
                error_obj_update(Error(result), tmp, Obj(v1));
                error_obj_update(Error(result), tmp2, Obj(v2));
            }
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_CMP:
        val = icmp(op);
        return val_reference(val < 0 ? minus1_value : int_value[(val > 0) ? 1 : 0]);
    case O_EQ: return truth_reference(icmp(op) == 0);
    case O_NE: return truth_reference(icmp(op) != 0);
    case O_MIN:
    case O_LT: return truth_reference(icmp(op) < 0);
    case O_LE: return truth_reference(icmp(op) <= 0);
    case O_MAX:
    case O_GT: return truth_reference(icmp(op) > 0);
    case O_GE: return truth_reference(icmp(op) >= 0);
    case O_CONCAT:
        if (v1->len == 0) {
            return Obj(ref_str(v2));
        }
        if (v2->len == 0) {
            return Obj(ref_str(v1));
        }
        do {
            uint8_t *s;
            size_t ln;
            if (add_overflow(v1->len, v2->len, &ln)) break;

            if (op->inplace == Obj(v1)) {
                size_t ln2;
                if (ln > sizeof v1->u.val && v1->u.val != v1->data && ln > v1->u.s.max) {
                    ln2 = ln + (ln < 1024 ? ln : 1024);
                    if (ln2 < ln) ln2 = ln;
                } else ln2 = ln;
                s = extend_str(v1, ln2);
                if (s == NULL) break;
                s += v1->len;
                v1->len = ln;
                v1->chars += v2->chars;
                v = ref_str(v1);
            } else if (op->inplace == Obj(v2)) {
                size_t ln2;
                if (ln > sizeof v2->u.val && v2->u.val != v2->data && ln > v2->u.s.max) {
                    ln2 = ln + (ln < 1024 ? ln : 1024);
                    if (ln2 < ln) ln2 = ln;
                } else ln2 = ln;
                s = extend_str(v2, ln2);
                if (s == NULL) break;
                memmove(s + v1->len, v2->data, v2->len);
                memcpy(s, v1->data, v1->len);
                v2->len = ln;
                v2->chars += v1->chars;
                return val_reference(Obj(v2));
            } else {
                v = new_str2(ln);
                if (v == NULL) break;
                v->chars = v1->chars + v2->chars;
                s = v->data;
                memcpy(s, v1->data, v1->len);
                s += v1->len;
            }
            if (v2->len == 1) {
                s[0] = v2->data[0];
            } else {
                memcpy(s, v2->data, v2->len);
            }
            return Obj(v);
        } while (false);
        return new_error_mem(op->epoint3);
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Str *v1 = Str(op->v1), *v;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return Obj(err);

    if (v1->len == 0 || rep == 0) return val_reference(null_str);
    do {
        size_t ln, sz;
        if (rep == 1) {
            return Obj(ref_str(v1));
        }
        ln = v1->len;
        if (ln > SIZE_MAX / rep) break; /* overflow */
        sz = ln * rep;
        if (op->inplace == Obj(v1)) {
            if (extend_str(v1, sz) == NULL) break;
            v = ref_str(v1);
            v->len = sz;
        } else {
            v = new_str2(sz);
            if (v == NULL) break;
        }
        v->chars = v1->chars * rep;
        if (ln == 1) {
            memset(v->data, v1->data[0], sz);
        } else {
            if (v->data != v1->data) memcpy(v->data, v1->data, ln);
            while (sz > ln) {
                size_t oln = ln;
                if (ln > sz - ln) ln = sz - ln;
                memcpy(v->data + oln, v->data, ln);
                ln += oln;
            }
        }
        return Obj(v);
    } while (false);
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    uint8_t *p, *p2;
    size_t len1, len2;
    Obj *o2 = op->v2;
    Str *v, *v1 = Str(op->v1);
    Funcargs *args = Funcargs(o2);
    Obj *err;
    struct indexoffs_s io;

    if (args->len < 1 || args->len - 1 > indx) {
        return new_error_argnum(args->len, 1, indx + 1, op->epoint2);
    }
    io.len = v1->chars;
    io.epoint = &args->val[indx].epoint;
    io.val = args->val[indx].val;

    if (io.val->obj->iterable) {
        struct iter_s iter;
        size_t i;
        iter.data = io.val; io.val->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(null_str);
        }

        if (v1->len == v1->chars) {
            v = new_str2(iter.len);
            if (v == NULL) {
                iter_destroy(&iter);
                goto failed;
            }
            p2 = v->data;
            for (i = 0; i < iter.len && (io.val = iter.next(&iter)) != NULL; i++) {
                err = indexoffs(&io);
                if (err != NULL) {
                    val_destroy(Obj(v));
                    iter_destroy(&iter);
                    return err;
                }
                *p2++ = v1->data[io.offs];
            }
            iter_destroy(&iter);
            len1 = i;
        } else {
            size_t m = v1->len;
            uint8_t *o;
            size_t j = 0;

            v = new_str2(m);
            if (v == NULL) {
                iter_destroy(&iter);
                goto failed;
            }
            o = p2 = v->data;
            p = v1->data;

            for (i = 0; i < iter.len && (io.val = iter.next(&iter)) != NULL; i++) {
                unsigned int k;
                err = indexoffs(&io);
                if (err != NULL) {
                    val_destroy(Obj(v));
                    iter_destroy(&iter);
                    return err;
                }
                while (io.offs != j) {
                    if (io.offs > j) {
                        p += utf8len(*p);
                        j++;
                    } else {
                        do { p--; } while (*p >= 0x80 && *p < 0xc0);
                        j--;
                    }
                }
                k = utf8len(*p);
                if ((size_t)(p2 - o) + k > m) {
                    const uint8_t *r = o;
                    if (inc_overflow(&m, 4096)) {
                        iter_destroy(&iter);
                        goto failed2; /* overflow */
                    }
                    if (o == v->u.val) {
                        o = allocate_array(uint8_t, m);
                        if (o == NULL) {
                            iter_destroy(&iter);
                            goto failed2;
                        }
                        v->data = o;
                        memcpy(o, v->u.val, m - 4096);
                        v->u.s.hash = -1;
                    } else {
                        o = reallocate_array(o, m);
                        if (o == NULL) {
                            iter_destroy(&iter);
                            goto failed2;
                        }
                        v->data = o;
                    }
                    v->u.s.max = m;
                    p2 += o - r;
                }
                memcpy(p2, p, k);p2 += k;
            }
            iter_destroy(&iter);
            len1 = i;
            len2 = (size_t)(p2 - o);
            if (o != v->u.val) {
                if (len2 <= sizeof v->u.val) {
                    memcpy(v->u.val, o, len2);
                    free(o);
                    v->data = v->u.val;
                } else {
                    uint8_t *oo = reallocate_array(o, len2);
                    v->data = (oo != NULL) ? oo : o;
                    v->u.s.max = len2;
                }
            }
            v->len = len2;
        }
        v->chars = len1;
        return Obj(v);
    }
    if (io.val->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;

        err = sliceparams(&s, &io);
        if (err != NULL) return err;

        if (s.length == 0) {
            return val_reference(null_str);
        }
        if (s.step == 1) {
            if (s.length == v1->chars) {
                return Obj(ref_str(v1)); /* original string */
            }
            if (v1->len == v1->chars) {
                len2 = s.length;
            } else {
                ival_t i;
                p = v1->data;
                for (i = 0; i < s.offset; i++) {
                    p += utf8len(*p);
                }
                len2 = (size_t)(p - v1->data);
                s.offset = (ival_t)len2;
                for (; i < s.end; i++) {
                    p += utf8len(*p);
                }
                len2 = (size_t)(p - v1->data) - len2;
            }
            if (op->inplace == Obj(v1)) {
                v = ref_str(v1);
                v->len = len2;
                if (v->data != v->u.val && len2 <= sizeof v->u.val) {
                    memcpy(v->u.val, v1->data + s.offset, len2);
                    free(v->data);
                    v->data = v->u.val;
                } else {
                    if (s.offset != 0) memmove(v->data, v1->data + s.offset, len2);
                    if (v->data != v->u.val) v->u.s.hash = -1;
                }
            } else {
                v = new_str2(len2);
                if (v == NULL) goto failed;
                memcpy(v->data, v1->data + s.offset, len2);
            }
            v->chars = s.length;
            return Obj(v);
        }
        if (v1->len == v1->chars) {
            size_t i;
            if (s.step > 0 && op->inplace == Obj(v1)) {
                v = ref_str(v1);
                if (v->data != v->u.val && s.length <= sizeof v->u.val) {
                    p2 = v->u.val;
                } else {
                    p2 = v->data;
                    if (v->data != v->u.val) v->u.s.hash = -1;
                }
                v->len = s.length;
            } else {
                v = new_str2(s.length);
                if (v == NULL) goto failed;
                p2 = v->data;
            }
            for (i = 0; i < s.length; i++) {
                p2[i] = v1->data[s.offset];
                s.offset += s.step;
            }
            if (p2 != v->data) {
                free(v->data);
                v->data = p2;
            }
        } else {
            ival_t i, k;
            size_t j, offs2;
            uint8_t *o;
            if (s.step > 0 && op->inplace == Obj(v1)) {
                v = ref_str(v1);
            } else {
                v = new_str2(v1->len);
                if (v == NULL) goto failed;
            }
            o = p2 = v->data;
            p = v1->data;
            for (i = 0; i < s.offset; i++) {
                p += utf8len(*p);
            }
            if (s.step > 0) {
                for (k = i; i < s.end; i++) {
                    j = utf8len(*p);
                    if (i == k) {
                        for (offs2 = 0; offs2 < j; offs2++) p2[offs2] = p[offs2];
                        p2 += j; k += s.step;
                    }
                    p += j;
                }
            } else {
                p += utf8len(*p);
                for (k = i; i > s.end; i--) {
                    j = 0;
                    do {
                        p--;j++;
                    } while (*p >= 0x80 && *p < 0xc0);
                    if (i == k) {
                        for (offs2 = 0; offs2 < j; offs2++) p2[offs2] = p[offs2];
                        p2 += j; k += s.step;
                    }
                }
            }
            len2 = (size_t)(p2 - o);
            if (o != v->u.val) {
                if (len2 <= sizeof v->u.val) {
                    memcpy(v->u.val, o, len2);
                    free(o);
                    v->data = v->u.val;
                } else {
                    uint8_t *oo = reallocate_array(o, len2);
                    v->data = (oo != NULL) ? oo : o;
                    v->u.s.max = len2;
                    v->u.s.hash = -1;
                }
            }
            v->len = len2;
        }
        v->chars = s.length;
        return Obj(v);
    }
    err = indexoffs(&io);
    if (err != NULL) return err;

    if (v1->len == v1->chars) {
        p = v1->data + io.offs;
        len1 = 1;
    } else {
        p = v1->data;
        for (; io.offs != 0; io.offs--) p += utf8len(*p);
        len1 = utf8len(*p);
    }

    if (op->inplace == Obj(v1)) {
        v = ref_str(v1);
        if (v->data != v->u.val) {
            p2 = v->u.val;
        } else {
            p2 = v->data;
        }
        v->len = len1;
    } else {
        v = new_str2(len1);
        if (v == NULL) goto failed;
        p2 = v->data;
    }
    v->chars = 1;
    p2[0] = p[0];
    if (len1 > 1) {
        for (io.offs = 1; io.offs < len1; io.offs++) p2[io.offs] = p[io.offs];
    }
    if (p2 != v->data) {
        free(v->data);
        v->data = p2;
    }
    return Obj(v);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(op->epoint3);
}

static bool str_contains(oper_t op) {
    const Str *v1 = Str(op->v1);
    const Str *v2 = Str(op->v2);
    const uint8_t *c, *c2, *e;
    if (v1->len == 0) return true;
    if (v1->len > v2->len) return false;
    c2 = v2->data;
    e = c2 + v2->len - v1->len;
    for (;;) {
        c = (const uint8_t *)memchr(c2, v1->data[0], (size_t)(e - c2) + 1);
        if (c == NULL) return false;
        if (memcmp(c, v1->data, v1->len) == 0) return true;
        c2 = c + 1;
    }
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1;
    if (o1->obj == STR_OBJ) {
        bool result = str_contains(op);
        return truth_reference(op->op == O_IN ? result : !result);
    }
    if (o1 == none_value || o1->obj == ERROR_OBJ) return val_reference(o1);
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Str *v1 = Str(op->v1);
    Obj *v2 = op->v2;
    Obj *tmp;

    if (op->op == O_X) {
        if (v2 == none_value) return val_reference(v2);
        return repeat(op);
    }
    if (op->op == O_LAND || op->op == O_LOR || op->op == O_LXOR) {
        Obj *result = truth(Obj(v1), TRUTH_BOOL, op->epoint);
        bool i;
        if (result->obj != BOOL_OBJ) return result;
        i = Bool(result)->value;
        val_destroy(result);
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        if (op->op == O_LXOR) return calc2_lxor(op, i);
        return val_reference(i != (op->op == O_LOR) ? v2 : Obj(v1));
    }
    if (v2->obj->iterable) {
        if (op->op != O_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    }
    switch (v2->obj->type) {
    case T_STR: return calc2_str(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        FALL_THROUGH; /* fall through */
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_REGISTER:
        {
            Obj *result;
            switch (op->op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: tmp = bits_from_str(v1, op->epoint); break;
            default: tmp = int_from_str(v1, op->epoint);
            }
            op->v1 = tmp;
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v1));
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            Obj *result;
            tmp = bytes_from_str(v1, op->epoint, BYTES_MODE_TEXT);
            op->v1 = tmp;
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v1));
            val_destroy(tmp);
            return result;
        }
    case T_GAP:
        if (op->op != O_MEMBER) {
            return v2->obj->rcalc2(op);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(v2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Str *v2 = Str(op->v2);
    const Type *t1 = op->v1->obj;
    Obj *tmp;
    switch (t1->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        FALL_THROUGH; /* fall through */
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            Obj *result;
            switch (op->op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = bits_from_str(v2, op->epoint2); break;
            default: tmp = int_from_str(v2, op->epoint2);
            }
            op->v2 = tmp;
            op->inplace = NULL;
            result = t1->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v2));
            val_destroy(tmp);
            return result;
        }
    case T_BYTES:
        {
            Obj *result;
            tmp = bytes_from_str(v2, op->epoint2, BYTES_MODE_TEXT);
            op->v2 = tmp;
            op->inplace = NULL;
            result = t1->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v2));
            val_destroy(tmp);
            return result;
        }
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

void strobj_init(void) {
    Type *type = new_type(&obj, T_STR, "str", sizeof(Str));
    type->convert = convert;
    type->destroy = destroy;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->iaddress = ival;
    type->uaddress = uval;
    type->sign = sign;
    type->function = function;
    type->len = len;
    type->getiter = getiter;
    type->getriter = getriter;
    type->calc1 = calc1;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    type->slice = slice;
    type->contains = contains;
}

void strobj_names(void) {
    new_builtin("str", val_reference(Obj(STR_OBJ)));
}

void strobj_destroy(void) {
#ifdef DEBUG
    if (null_str->refcount != 1) fprintf(stderr, "str %" PRIuSIZE "\n", null_str->refcount - 1);
#endif
}
