/*
    $Id: bitsobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "bitsobj.h"
#include <string.h>
#include "math.h"
#include "eval.h"
#include "variables.h"
#include "unicode.h"
#include "error.h"
#include "arguments.h"

#include "codeobj.h"
#include "boolobj.h"
#include "floatobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "listobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"
#include "encobj.h"

#define SHIFT (8 * (unsigned int)sizeof(bdigit_t))

static Type obj;

Type *const BITS_OBJ = &obj;

static Bits null_bitsval = { { &obj, 1 }, 0, 0, null_bitsval.u.val, { {0, 0} } };
static Bits inv_bitsval = { { &obj, 1 }, ~0, 0, inv_bitsval.u.val, { {0, 0} } };
static Bits zero_bitsval = { { &obj, 1 }, 0, 1, zero_bitsval.u.val, { {0, 0} } };
static Bits one_bitsval = { { &obj, 1 }, 1, 1, one_bitsval.u.val, { {1, 0} } };
static Bits zero_ibitsval = { { &obj, 1 }, ~0, 1, zero_ibitsval.u.val, { {0, 0} } };
static Bits one_ibitsval = { { &obj, 1 }, ~1, 1, one_ibitsval.u.val, { {1, 0} } };

Obj *const null_bits = &null_bitsval.v;
Obj *const inv_bits = &inv_bitsval.v;
Obj *const bits_value[2] = { &zero_bitsval.v, &one_bitsval.v };
Obj *const ibits_value[2] = { &zero_ibitsval.v, &one_ibitsval.v };

static inline Bits *ref_bits(Bits *v1) {
    v1->v.refcount++; return v1;
}

static MUST_CHECK Obj *bits_from_int(const Int *, linepos_t);

MUST_CHECK Obj *bits_from_obj(Obj *v1, linepos_t epoint) {
    Obj *err, *ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BITS: return val_reference(v1);
    case T_BOOL: return val_reference(bits_value[Bool(v1)->value ? 1 : 0]);
    case T_STR: return bits_from_str(Str(v1), epoint);
    case T_BYTES: return bits_from_bytes(Bytes(v1), epoint);
    case T_INT: return bits_from_int(Int(v1), epoint);
    case T_CODE: return bits_from_code(Code(v1), epoint);
    case T_ADDRESS: return bits_from_address(Address(v1), epoint);
    case T_FLOAT:
         err = int_from_float(Float(v1), epoint);
         if (err->obj != INT_OBJ) return err;
         ret = bits_from_int(Int(err), epoint);
         val_destroy(err);
         return ret;
    default: break;
    }
    return new_error_conv(v1, BITS_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return bits_from_obj(op->v2, op->epoint2);
}

static inline size_t bitslen(const Bits *v1) {
    ssize_t len = v1->len;
    return (size_t)(len < 0 ? ~len : len);
}

static FAST_CALL NO_INLINE void bits_destroy(Bits *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    Bits *v1 = Bits(o1);
    if unlikely(v1->u.val != v1->data) bits_destroy(v1);
}

static MALLOC Bits *new_bits2(size_t len) {
    Bits *v = Bits(val_alloc(BITS_OBJ));
    if (len > lenof(v->u.val)) {
        v->u.hash = -1;
        v->data = allocate_array(bdigit_t, len);
        if (v->data == NULL) {
            val_destroy(Obj(v));
            v = NULL;
        }
    } else {
        v->data = v->u.val;
    }
    return v;
}

static MUST_CHECK Obj *convert2(oper_t op) {
    Funcargs *v2 = Funcargs(op->v2);
    argcount_t args = v2->len;
    Error *err;
    ival_t ival;
    uval_t len2, blen2;
    size_t blen;
    Obj *v;
    Bits *bits, *bits2;
    unsigned int m;
    bool inplace, bytes;
    if (args != 2) {
        return new_error_argnum(args, 1, 2, op->epoint2);
    }
    v = v2->val[1].val;
    err = v->obj->ival(v, &ival, 8 * sizeof ival, &v2->val[1].epoint);
    if (err != 0) return Obj(err);
    v = v2->val[0].val;
    inplace = (v->obj == BITS_OBJ);
    bytes = (v->obj == BYTES_OBJ || v->obj == STR_OBJ);
    if (!inplace) {
        v = bits_from_obj(v, op->epoint2);
        if (v->obj != BITS_OBJ) return v;
    }
    bits = Bits(v);
    if (ival >= 0) {
        len2 = (uval_t)ival;
        if (!inplace && !bytes && bits->len < 0) {
            val_destroy(Obj(bits));
            return new_error_obj(ERROR______NOT_UVAL, v2->val[0].val, &v2->val[0].epoint);
        }
    } else {
        len2 = (uval_t)-(1 + ival);
    }
    blen2 = len2 / SHIFT + 1;
    m = len2 % SHIFT;
    if (ival < 0) len2++;
    blen = bitslen(bits);
    if (((inplace || bytes) && bits->bits > len2) || blen > blen2 || (blen == blen2 && (bits->data[blen2 - 1] >> m) != 0)) {
        if (!inplace) val_destroy(Obj(bits));
        err = new_error(ival < 0 ? ERROR_____CANT_IVAL : ERROR_____CANT_UVAL, &v2->val[0].epoint);
        err->u.intconv.bits = len2;
        err->u.intconv.val = val_reference(v2->val[0].val);
        return Obj(err);
    }
    if (bits->v.refcount == 1) {
        bits->bits = len2;
    }
    if (bits->bits == len2) {
        return inplace ? val_reference(Obj(bits)) : Obj(bits);
    }
    bits2 = new_bits2(blen);
    bits2->len = bits->len;
    bits2->bits = len2;
    if (blen != 0) memcpy(bits2->data, bits->data, blen * sizeof *bits2->data); else bits2->u.val[0] = 0;
    if (!inplace) val_destroy(Obj(bits));
    return Obj(bits2);
}

static MUST_CHECK Obj *invert(const Bits *v1, linepos_t epoint) {
    size_t sz = bitslen(v1);
    Bits *v = new_bits2(sz);
    if (v == NULL) return new_error_mem(epoint);
    v->bits = v1->bits;
    v->len = ~v1->len;
    if (sz != 0) {
        memcpy(v->data, v1->data, sz * sizeof *v->data);
    } else {
        v->data[0] = 0;
    }
    return Obj(v);
}

static FAST_CALL NO_INLINE Obj *normalize2(Bits *v, size_t sz) {
    if (sz != 0) {
        do {
            sz--;
            v->u.val[sz] = v->data[sz];
        } while (sz != 0);
    } else {
        v->u.val[0] = 0;
    }
    free(v->data);
    v->data = v->u.val;
    return Obj(v);
}

static FAST_CALL MUST_CHECK Obj *normalize(Bits *v, size_t sz, bool neg) {
    bdigit_t *d = v->data;
    while (sz != 0 && d[sz - 1] == 0) sz--;
    /*if (sz >= SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    v->len = (ssize_t)(neg ? ~sz : sz);
    if (v->u.val != d && sz <= lenof(v->u.val)) {
        return normalize2(v, sz);
    }
    return Obj(v);
}

static MUST_CHECK Obj *negate(Bits *v1, linepos_t epoint) {
    size_t i, sz = bitslen(v1);
    Bits *v;
    bool ext = false;
    if (v1->len == 0) return val_reference(Obj(v1));
    if (v1->len < 0) {
        size_t bs = (v1->bits % SHIFT);
        ext = true;
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != (bdigit_t)~0) {
                ext = false;
                break;
            }
        }
        if (i == v1->bits / SHIFT) {
            if (bs == 0) return NULL;
            if (i < sz && v1->data[i] == ~((~(v1->data[i] >> bs)) << bs)) return NULL;
        }
        v = new_bits2(ext ? sz + 1 : sz);
        if (v == NULL) goto failed;
        if (ext) {
            for (i = 0; i < sz; i++) v->data[i] = 0;
            v->data[i] = 1;
        } else {
            for (i = 0; i < sz; i++) {
                if (v1->data[i] != (bdigit_t)~0) {
                    v->data[i] = v1->data[i] + 1;
                    i++;
                    break;
                }
                v->data[i] = 0;
            }
        }
    } else {
        v = new_bits2(sz);
        if (v == NULL) goto failed;
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != 0) {
                v->data[i] = v1->data[i] - 1;
                i++;
                break;
            }
            v->data[i] = ~(bdigit_t)0;
        }
    }
    for (; i < sz; i++) v->data[i] = v1->data[i];
    v->bits = v1->bits;
    return normalize(v, ext ? sz + 1 : sz, v1->len > 0);
failed:
    return new_error_mem(epoint);
}

static MALLOC Obj *return_bits(bdigit_t c, unsigned int blen) {
    Bits *vv = Bits(val_alloc(BITS_OBJ));
    vv->data = vv->u.val;
    vv->u.val[0] = c;
    vv->len = (c != 0) ? 1 : 0;
    vv->bits = blen;
    return Obj(vv);
}

static FAST_CALL NO_INLINE bool bits_same(const Bits *v1, const Bits *v2) {
    return memcmp(v1->data, v2->data, bitslen(v1) * sizeof *v1->data) == 0;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Bits *v1 = Bits(o1), *v2 = Bits(o2);
    if (o1->obj != o2->obj || v1->len != v2->len || v1->bits != v2->bits) return false;
    switch (v1->len) {
    case 0: return true;
    case -1:
    case 1: return v1->data[0] == v2->data[0];
    default: return bits_same(v1, v2);
    }
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t UNUSED(epoint)) {
    const Bits *v1 = Bits(o1);
    size_t i, sz, sz2;
    bdigit_t b, inv;
    switch (type) {
    case TRUTH_ALL:
        if (v1->bits == 0) return ref_true();
        sz = bitslen(v1);
        sz2 = v1->bits / SHIFT;
        if (sz2 > sz) sz2 = sz;
        inv = (v1->len >= 0) ? ~(bdigit_t)0 : 0;
        for (i = 0; i < sz2; i++) {
            if (v1->data[i] != inv) return ref_false();
        }
        if ((v1->bits % SHIFT) == 0) return ref_true();
        b = (i >= sz) ? 0 : v1->data[i];
        b ^= inv;
        b <<= SHIFT - v1->bits % SHIFT;
        return truth_reference(b == 0);
    case TRUTH_ANY:
        if (v1->bits == 0) return ref_false();
        FALL_THROUGH; /* fall through */
    default:
        return truth_reference(v1->len != 0);
    }
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    const Bits *v1 = Bits(o1);
    size_t len, i, len2, sz;
    uint8_t *s;
    bool inv;
    Str *v;

    len2 = v1->bits;
    sz = bitslen(v1);
    inv = (v1->len < 0);
    len = inv ? 2 : 1;
    if ((len2 & 3) != 0) {
        if (inc_overflow(&len, len2)) return NULL;
        if (len > maxsize) return NULL;
        v = new_str2(len);
        if (v == NULL) return NULL;
        v->chars = len;
        s = v->data;

        if (inv) *s++ = '~';
        *s++ = '%';
        for (i = len2; (i--) != 0;) {
            size_t j = i / SHIFT;
            *s++ = (j < sz && (v1->data[j] & (1U << (i % SHIFT))) != 0) ? '1' : '0';
        }
        return Obj(v);
    }
    len2 /= 4;
    if (inc_overflow(&len, len2)) return NULL;
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;

    if (inv) *s++ = '~';
    *s++ = '$';
    for (i = len2; (i--) != 0;) {
        size_t j = i / (2 * sizeof *v1->data);
        *s++ = (j >= sz) ? 0x30 : (uint8_t)"0123456789abcdef"[(v1->data[j] >> ((i & (2 * (sizeof *v1->data) - 1)) * 4)) & 15];
    }
    return Obj(v);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bits *v1 = Bits(o1);
    size_t l;
    unsigned int h;

    switch (v1->len) {
    case ~1: *hs = (v1->bits ^ ~v1->data[0]) & ((~0U) >> 1); return NULL;
    case ~0: *hs = (v1->bits ^ ~0U) & ((~0U) >> 1); return NULL;
    case 0: *hs = v1->bits & ((~0U) >> 1); return NULL;
    case 1: *hs = (v1->bits ^ v1->data[0]) & ((~0U) >> 1); return NULL;
    default: break;
    }
    if (v1->data != v1->u.val && v1->u.hash >= 0) {
        *hs = v1->u.hash;
        return NULL;
    }
    if (v1->len < 0) {
        l = (size_t)~v1->len;
        h = ~0U;
        while ((l--) != 0) {
            h -= v1->data[l];
        }
    } else {
        l = (size_t)v1->len;
        h = 0;
        while ((l--) != 0) {
            h += v1->data[l];
        }
    }
    h ^= (unsigned int)v1->bits;
    h &= ((~0U) >> 1);
    if (v1->data != v1->u.val) v1->u.hash = (int)h;
    *hs = (int)h;
    return NULL;
}

static bool uvalx(Obj *o1, uval_t *uv, unsigned int bits) {
    Bits *v1 = Bits(o1);
    switch (v1->len) {
    case ~1:
        *uv = ~v1->data[0];
        if (v1->bits > bits) break;
        return true;
    case ~0:
        *uv = ~(uval_t)0;
        if (v1->bits > bits) break;
        return true;
    case 0:
        *uv = 0;
        return true;
    case 1:
        *uv = v1->data[0];
        if (bits < SHIFT && (*uv >> bits) != 0) break;
        return true;
    default:
        break;
    }
    return false;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Error *v;
    if (uvalx(o1, (uval_t *)iv, bits)) return NULL;
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Error *v;
    if (uvalx(o1, uv, bits)) return NULL;
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

static MUST_CHECK Error *uval2(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    if (Bits(o1)->len < 0) return Error(new_error_obj(ERROR______NOT_UVAL, o1, epoint));
    return uval(o1, uv, bits, epoint);
}

MUST_CHECK Obj *float_from_bits(const Bits *v1, linepos_t epoint) {
    double d;
    size_t i, len1;
    switch (v1->len) {
    case ~1: d = -1.0 - v1->data[0]; break;
    case ~0: d = -1.0; break;
    case 0: d = 0.0; break;
    case 1: d = v1->data[0]; break;
    default:
        d = ((v1->len < 0) ? 1.0 : 0.0) + v1->data[0];
        len1 = bitslen(v1);
        for (i = 1; i < len1; i++) {
            d += ldexp(v1->data[i], (int)(i * SHIFT));
        }
        if (v1->len < 0) d = -d;
        return float_from_double(d, epoint);
    }
    return new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bits *v1 = Bits(o1);
    ssize_t len = v1->len;
    return val_reference(len < 0 ? minus1_value : int_value[(len > 0) ? 1 : 0]);
}

static MUST_CHECK Obj *function(oper_t op) {
    Bits *v1 = Bits(op->v2);
    Obj *tmp, *ret;
    op->v2 = tmp = int_from_bits(v1, op->epoint2);
    op->inplace = tmp->refcount == 1 ? tmp : NULL;
    ret = tmp->obj->function(op);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *len(oper_t op) {
    Bits *v1 = Bits(op->v2);
    return int_from_size(v1->bits);
}

static FAST_CALL MUST_CHECK Obj *iter_element(struct iter_s *v1, size_t i) {
    const Bits *vv1 = Bits(v1->data);
    size_t o = i / SHIFT;
    bdigit_t v;
    if (vv1->len < 0) {
        v = o < (size_t)~vv1->len ? ~vv1->data[o] : ~(bdigit_t)0;
    } else {
        v = o < (size_t)vv1->len ? vv1->data[o] : 0;
    }
    return bits_value[(v >> (i % SHIFT)) & 1];
}

static FAST_CALL MUST_CHECK Obj *iter_forward(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return iter_element(v1, v1->val++);
}

static void getiter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_forward;
    v->len = Bits(v->data)->bits;
}

static FAST_CALL MUST_CHECK Obj *iter_reverse(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return iter_element(v1, v1->len - ++v1->val);
}

static void getriter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_reverse;
    v->len = Bits(v->data)->bits;
}

MUST_CHECK Obj *bits_from_uval(uval_t i, unsigned int bits) {
    return return_bits(i, bits);
}

MUST_CHECK Obj *bits_from_hexstr(const uint8_t *s, linecpos_t *ln) {
    linecpos_t i, j, k;
    int bits;
    bdigit_t *d, uv;
    Bits *v;

    i = k = 0; uv = 0;
    if (s[0] != '_') {
        for (;;k++) {
            uint8_t c2, c = s[k] ^ 0x30;
            if (c < 10) {
                uv = (uv << 4) + c;
                continue;
            }
            c2 = (uint8_t)((c | 0x20) - 0x71);
            if (c2 < 6) {
                uv = (uv << 4) + c2 + 10;
                continue;
            }
            if (c != ('_' ^ 0x30)) break;
            i++;
        }
        while (k != 0 && s[k - 1] == '_') {
            k--;
            i--;
        }
    }
    *ln = k;
    i = k - i;

    if (i <= 2 * sizeof *d) {
        return (i == 0) ? val_reference(null_bits) : return_bits(uv, (unsigned int)i * 4);
    }

    if ((size_t)i + 0 > SIZE_MAX / 4) return NULL; /* overflow */

    v = new_bits2(i / (2 * sizeof *d) + ((i % (2 * sizeof *d)) != 0 ? 1 :0));
    if (v == NULL) return NULL;
    v->bits = i * 4;
    d = v->data;

    uv = 0; j = 0; bits = 0;
    while ((k--) != 0) {
        bdigit_t c = s[k] ^ 0x30;
        if (c < 10) uv += c << bits;
        else if (c == ('_' ^ 0x30)) continue;
        else uv += ((c & 7) + 9) << bits;
        if (bits == SHIFT - 4) {
            d[j++] = uv;
            bits = 0; uv = 0;
        } else bits += 4;
    }
    if (uv != 0) d[j++] = uv;

    return normalize(v, j, false);
}

MUST_CHECK Obj *bits_from_binstr(const uint8_t *s, linecpos_t *ln) {
    linecpos_t i, j, k;
    bdigit_t bits;
    bdigit_t *d, uv;
    Bits *v;

    i = k = 0; uv = 0;
    if (s[0] != '_') {
        for (;;k++) {
            uint8_t c = s[k] ^ 0x30;
            if (c < 2) {
                uv = (uv << 1) + c;
                continue;
            }
            if (c != ('_' ^ 0x30)) break;
            i++;
        }
        while (k != 0 && s[k - 1] == '_') {
            k--;
            i--;
        }
    }
    *ln = k;
    i = k - i;

    if (i <= 8 * sizeof *d) {
        return (i == 0) ? val_reference(null_bits) : return_bits(uv, (unsigned int)i);
    }

    v = new_bits2(i / SHIFT + ((i % SHIFT) != 0 ? 1 : 0));
    if (v == NULL) return NULL;
    v->bits = i;
    d = v->data;

    uv = 0; j = 0; bits = 1;
    while ((k--) != 0) {
        if (s[k] == 0x31) uv += bits;
        else if (s[k] == '_') continue;
        bits <<= 1;
        if (bits == 0) {
            d[j++] = uv;
            uv = 0;
            bits = 1;
        }
    }
    if (uv != 0) d[j++] = uv;

    return normalize(v, j, false);
}

MUST_CHECK Obj *bits_from_str(Str *v1, linepos_t epoint) {
    struct encoder_s *encoder;
    int ch;
    Bits *v;
    unsigned int bits;
    size_t j, sz, osz;
    bdigit_t *d, uv;

    if (actual_encoding->updating) {
        if (v1->chars == 1) {
            unichar_t ch2 = v1->data[0];
            if ((ch2 & 0x80) != 0) utf8in(v1->data, &ch2);
            return return_bits(ch2 & 0xffffff, ch2 < 256 ? 8 : ch2 < 65536 ? 16 : 24);
        }
        if (v1->chars != 0) return new_error_obj(ERROR__NOT_ONE_CHAR, Obj(v1), epoint);
        return Obj(new_error(ERROR__EMPTY_STRING, epoint));
    }

    if (v1->len == 0) {
        return val_reference(null_bits);
    }

    if (v1->len <= sizeof v->u.val) sz = lenof(v->u.val);
    else {
        sz = v1->len / sizeof *d;
        if ((v1->len % sizeof *d) != 0) sz++;
    }
    v = new_bits2(sz);
    if (v == NULL) goto failed;
    d = v->data;

    uv = 0; bits = 0; j = 0;
    encoder = enc_string_init(actual_encoding, v1, epoint);
    while ((ch = enc_string(encoder)) != EOF) {
        uv |= (bdigit_t)(ch & 0xff) << bits;
        if (bits == SHIFT - 8) {
            if (j >= sz) {
                if (v->u.val == d) {
                    sz = 16 / sizeof *d;
                    d = allocate_array(bdigit_t, 16 / sizeof *d);
                    if (d == NULL) goto failed2;
                    v->data = d;
                    memcpy(d, v->u.val, j * sizeof *d);
                    v->u.hash = -1;
                } else {
                    if (inc_overflow(&sz, 1024 / sizeof *d)) goto failed2;
                    d = reallocate_array(d, sz);
                    if (d == NULL) goto failed2;
                    v->data = d;
                }
            }
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 8;
    }
    if (bits != 0) {
        if (j >= sz) {
            sz++;
            if (v->u.val == d) {
                d = allocate_array(bdigit_t, sz);
                if (d == NULL) goto failed2;
                v->data = d;
                memcpy(d, v->u.val, j * sizeof *d);
                v->u.hash = -1;
            } else {
                d = reallocate_array(d, sz);
                if (d == NULL) goto failed2;
                v->data = d;
            }
        }
        d[j] = uv;
        osz = j + 1;
    } else osz = j;

    while (osz != 0 && d[osz - 1] == 0) osz--;
    if (v->u.val != d) {
        if (osz <= lenof(v->u.val)) {
            if (osz != 0) {
                memcpy(v->u.val, d, osz * sizeof *d);
            } else {
                v->u.val[0] = 0;
            }
            free(d);
            v->data = v->u.val;
        } else if (osz < sz) {
            bdigit_t *d2 = reallocate_array(d, osz);
            v->data = (d2 != NULL) ? d2 : d;
        }
    }
    v->len = (ssize_t)osz;
    v->bits = j * SHIFT + bits;
    if (/*v->bits < bits ||*/ j > SIZE_MAX / SHIFT /*|| osz > SSIZE_MAX*/) goto failed2; /* overflow */
    return Obj(v);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(epoint);
}

MUST_CHECK Obj *bits_from_bytes(const Bytes *v1, linepos_t epoint) {
    int bits;
    size_t i, j, sz, len1;
    bdigit_t *d, uv;
    Bits *v;
    bool inv = (v1->len < 0);

    len1 = (size_t)(inv ? ~v1->len : v1->len);
    if (len1 == 0) {
        return val_reference(inv ? inv_bits : null_bits);
    }

    if (len1 > SIZE_MAX / 8) goto failed; /* overflow */

    sz = len1 / sizeof *d;
    if ((len1 % sizeof *d) != 0) sz++;
    v = new_bits2(sz);
    if (v == NULL) goto failed;
    v->bits = len1 * 8;
    d = v->data;

    uv = 0; j = 0; i = 0; bits = 0;
    while (len1 > i) {
        uv |= (bdigit_t)v1->data[i++] << bits;
        if (bits == SHIFT - 8) {
            d[j++] = uv;
            bits = 0; uv = 0;
        } else bits += 8;
    }
    if (bits != 0) d[j] = uv;

    return normalize(v, sz, inv);
failed:
    return new_error_mem(epoint);
}

static MUST_CHECK Obj *bits_from_int(const Int *v1, linepos_t epoint) {
    bool inv;
    size_t i, sz, bits;
    bdigit_t *d, d2;
    const digit_t *b;
    Bits *v;

    if (v1->len == 0) return val_reference(null_bits);
    if (v1->len == -1 && v1->data[0] == 1) return val_reference(inv_bits);

    inv = (v1->len < 0);
    sz = (size_t)(inv ? -v1->len : v1->len);
    v = new_bits2(sz);
    if (v == NULL) return new_error_mem(epoint);
    d = v->data;

    b = v1->data;
    if (inv) {
        digit_t c = 0;
        for (i = 0; c == 0 && i < sz; i++) {
            d[i] = (c = b[i]) - 1;
        }
        for (; i < sz; i++) {
            d[i] = b[i];
        }
    } else memcpy(d, b, sz * sizeof *d);

    d2 = d[sz - 1];
    for (bits = 0; d2 != 0; bits++) d2 >>= 1;

    v->bits = (sz - 1) * SHIFT + bits;

    return normalize(v, sz, inv);
}

MUST_CHECK Obj *bits_calc1(Oper_types op, unsigned int val) {
    switch (op) {
    case O_BANK: val >>= 8; FALL_THROUGH; /* fall through */
    case O_HIGHER: val >>= 8; FALL_THROUGH; /* fall through */
    case O_LOWER:
    default: return return_bits((uint8_t)val, 8);
    case O_BSWORD: val = (uint16_t)val | (val << 16); FALL_THROUGH; /* fall through */
    case O_HWORD: val >>= 8; FALL_THROUGH; /* fall through */
    case O_WORD: return return_bits((uint16_t)val, 16);
    }
}

static ssize_t icmp(oper_t op) {
    const Bits *vv1 = Bits(op->v1), *vv2 = Bits(op->v2);
    ssize_t i;
    size_t j;
    bdigit_t a, b;
    i = vv1->len - vv2->len;
    if (i != 0) return i;
    j = bitslen(vv1);
    while (j != 0) {
        j--;
        a = vv1->data[j]; b = vv2->data[j];
        if (a != b) return (a > b) ? vv1->len : -vv1->len;
    }
    return 0;
}

static inline unsigned int ldigit(const Bits *v1) {
    ssize_t ln = v1->len;
    if (ln > 0) return v1->data[0];
    if (ln == 0) return 0;
    ln = ~ln;
    if (ln == 0) return ~(bdigit_t)0;
    return ~v1->data[0];
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bits *v1 = Bits(op->v1);
    Obj *tmp, *v;
    switch (op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        return bits_calc1(op->op, ldigit(v1));
    case O_INV:
        if (op->inplace != Obj(v1)) return invert(v1, op->epoint3);
        v1->len = ~v1->len;
        if (v1->data != v1->u.val) v1->u.hash = -1;
        return val_reference(Obj(v1));
    case O_POS: return val_reference(Obj(v1));
    case O_NEG:
        v = negate(v1, op->epoint3);
        if (v != NULL) return v;
        FALL_THROUGH; /* fall through */
    case O_STRING:
        tmp = int_from_bits(v1, op->epoint);
        op->v1 = tmp;
        op->inplace = (tmp->refcount == 1) ? tmp : NULL;
        v = tmp->obj->calc1(op);
        val_destroy(tmp);
        return v;
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return truth_reference(v1->len == 0);
    default:
        break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *binary(oper_t op) {
    Bits *vv1 = Bits(op->v1), *vv2 = Bits(op->v2);
    size_t i, len1, len2, sz;
    bool neg;
    const bdigit_t *v1, *v2;
    bdigit_t *v;
    Bits *vv;
    len1 = bitslen(vv1);
    len2 = bitslen(vv2);

    if (len1 < len2) {
        Bits *tmp = vv1; vv1 = vv2; vv2 = tmp;
        sz = len1; len1 = len2; len2 = sz;
    }

    switch (op->op) {
    case O_AND:
        sz = vv2->len < 0 ? len1 : len2;
        break;
    case O_OR:
        sz = vv2->len < 0 ? len2 : len1;
        break;
    default:
        sz = len1;
        break;
    }
    if (op->inplace == Obj(vv1)) {
        vv = ref_bits(vv1);
        if (vv->data != vv->u.val) vv->u.hash = -1;
    } else if (op->inplace == Obj(vv2) && sz <= len2) {
        vv = ref_bits(vv2);
        if (vv->data != vv->u.val) vv->u.hash = -1;
    } else {
        vv = new_bits2(sz);
        if (vv == NULL) return new_error_mem(op->epoint3);
        if (sz == 0) vv->data[0] = 0;
    }
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    i = 0;
    switch (op->op) {
    case O_AND:
        if (vv1->bits < vv2->bits) {
            vv->bits = (vv1->len < 0) ? vv2->bits : vv1->bits;
        } else {
            vv->bits = (vv2->len < 0) ? vv1->bits : vv2->bits;
        }
        if (vv1->len < 0) {
            if (vv2->len < 0) {
                neg = true;
                for (; i < len2; i++) v[i] = v1[i] | v2[i];
            } else {
                neg = false;
                for (; i < len2; i++) v[i] = ~v1[i] & v2[i];
            }
        } else {
            neg = false;
            if (vv2->len < 0) {
                for (; i < len2; i++) v[i] = v1[i] & ~v2[i];
            } else {
                for (; i < len2; i++) v[i] = v1[i] & v2[i];
            }
        }
        break;
    case O_OR:
        if (vv1->bits < vv2->bits) {
            vv->bits = (vv1->len < 0) ? vv1->bits : vv2->bits;
        } else {
            vv->bits = (vv2->len < 0) ? vv2->bits : vv1->bits;
        }
        if (vv1->len < 0) {
            neg = true;
            if (vv2->len < 0) {
                for (; i < len2; i++) v[i] = v1[i] & v2[i];
            } else {
                for (; i < len2; i++) v[i] = v1[i] & ~v2[i];
            }
        } else {
            if (vv2->len < 0) {
                neg = true;
                for (; i < len2; i++) v[i] = ~v1[i] & v2[i];
            } else {
                neg = false;
                for (; i < len2; i++) v[i] = v1[i] | v2[i];
            }
        }
        break;
    default:
        vv->bits = vv2->bits > vv1->bits ? vv2->bits : vv1->bits;
        neg = (vv1->len < 0) != (vv2->len < 0);
        for (; i < len2; i++) v[i] = v1[i] ^ v2[i];
        break;
    }
    for (; i < sz; i++) v[i] = v1[i];
    return normalize(vv, i, neg);
}

static inline MUST_CHECK Obj *concat(oper_t op) {
    Bits *vv1 = Bits(op->v1), *vv2 = Bits(op->v2);
    size_t blen;
    size_t sz, bits, i, j, rbits, l;
    bdigit_t *v1, *v, uv;
    bdigit_t inv;
    Bits *vv;

    if (vv2->bits == 0) {
        return Obj(ref_bits(vv1));
    }
    inv = ((vv1->len ^ vv2->len) < 0) ? ~(bdigit_t)0 : 0;
    if (vv1->bits == 0 && inv == 0) {
        return Obj(ref_bits(vv2));
    }
    if (add_overflow(vv1->bits, vv2->bits, &blen)) goto failed;
    sz = blen / SHIFT;
    if ((blen % SHIFT) != 0) sz++;
    i = vv2->bits / SHIFT + bitslen(vv1);
    if ((vv2->bits % SHIFT) != 0) i++;
    if (i < sz) sz = i;
    vv = new_bits2(sz);
    if (vv == NULL) goto failed;
    v = vv->data;

    v1 = vv2->data;
    rbits = vv2->bits / SHIFT;
    bits = vv2->bits & (SHIFT - 1);
    l = bitslen(vv2);
    for (i = 0; i < rbits && i < l; i++) v[i] = v1[i] ^ inv;
    for (; i < rbits; i++) v[i] = inv;
    if (i < l) uv = v1[i] ^ inv; else uv = inv;
    if (inv != 0) uv &= (1U << bits) - 1;

    v1 = vv1->data;

    l = bitslen(vv1);
    if (bits != 0) {
        for (j = 0; j < l; j++) {
            v[i++] = uv | (v1[j] << bits);
            uv = v1[j] >> (SHIFT - bits);
        }
        if (i < sz) v[i++] = uv;
    } else {
        for (j = 0; j < l; j++) v[i++] = v1[j];
    }

    vv->bits = blen;
    return normalize(vv, i, (vv1->len < 0));
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *lshift(oper_t op, uval_t s) {
    Bits *vv1 = Bits(op->v1);
    size_t i, sz, bits, len1;
    unsigned int bit, word;
    bdigit_t *v1, *v, *v2;
    Bits *vv;

    sz = word = s / SHIFT;
    bit = s % SHIFT;
    if (bit != 0) sz++;
    if (add_overflow(vv1->bits, s, &bits)) goto failed;
    len1 = bitslen(vv1);
    if (inc_overflow(&sz, len1)) goto failed;
    if (op->inplace == Obj(vv1) && sz <= lenof(vv->u.val)) {
        vv = ref_bits(vv1);
    } else {
        vv = new_bits2(sz);
        if (vv == NULL) goto failed;
    }
    vv->bits = bits;
    v = vv->data;
    v1 = vv1->data;
    v2 = v + word;
    if (bit != 0) {
        bdigit_t d = 0;
        for (i = len1; i != 0;) {
            bdigit_t d2 = v1[--i];
            v2[i + 1] = d | (d2 >> (SHIFT - bit));
            d = d2 << bit;
        }
        if (vv1->len < 0) d |= ((bdigit_t)1 << bit) - 1;
        v2[0] = d;
    } else if (len1 != 0) memmove(v2, v1, len1 * sizeof *v2);
    if (word != 0) memset(v, (vv1->len < 0) ? ~0 : 0, word * sizeof *v);

    return normalize(vv, sz, (vv1->len < 0));
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *rshift(oper_t op, uval_t s) {
    Bits *vv1 = Bits(op->v1);
    size_t i, sz, bits, len1;
    unsigned int bit, word;
    Bits *vv;

    word = s / SHIFT;
    bit = s % SHIFT;
    bits = vv1->bits;
    if (bits <= s) {
        return val_reference((vv1->len < 0) ? inv_bits : null_bits);
    }
    bits -= s;
    len1 = bitslen(vv1);
    sz = (len1 > word) ? (len1 - word) : 0;
    if (op->inplace == Obj(vv1)) {
        vv = ref_bits(vv1);
        if (vv->data != vv->u.val) vv->u.hash = -1;
    } else {
        vv = new_bits2(sz);
        if (vv == NULL) return new_error_mem(op->epoint3);
    }
    vv->bits = bits;
    if (sz == 0) {
        vv->data[0] = 0;
    } else {
        bdigit_t *v = vv->data;
        bdigit_t *v1 = vv1->data + word;
        if (bit != 0) {
            bdigit_t d = v1[0] >> bit;
            for (i = 0; i < sz - 1; i++) {
                bdigit_t d2 = v1[i + 1];
                v[i] = d | (d2 << (SHIFT - bit));
                d = d2 >> bit;
            }
            v[i] = d;
        } else memmove(v, v1, sz * sizeof *v);
    }
    return normalize(vv, sz, (vv1->len < 0));
}

static MUST_CHECK Obj *calc2_bits(oper_t op) {
    ssize_t val;
    switch (op->op) {
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
    case O_AND:
    case O_OR:
    case O_XOR: return binary(op);
    case O_CONCAT: return concat(op);
    default: return NULL;
    }
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Bits *vv1 = Bits(op->v1), *vv;
    bdigit_t *v, *v1;
    bdigit_t uv;
    size_t sz, i, j, rbits, abits, bits, l;
    size_t blen = vv1->bits;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return Obj(err);

    if (rep == 0 || blen == 0) {
        return val_reference((vv1->len < 0) ? inv_bits : null_bits);
    }
    if (rep == 1) {
        return Obj(ref_bits(vv1));
    }

    if (blen > SIZE_MAX / rep) goto failed; /* overflow */
    blen *= rep;
    sz = blen / SHIFT;
    if ((blen % SHIFT) != 0) sz++;

    vv = new_bits2(sz);
    if (vv == NULL) goto failed;
    v = vv->data;
    v1 = vv1->data;

    i = 0;
    bits = 0;
    uv = 0;
    rbits = vv1->bits / SHIFT;
    abits = vv1->bits % SHIFT;
    l = bitslen(vv1);
    for (; rep != 0; rep--) {
        if (bits != 0) {
            for (j = 0; j < rbits && j < l; j++) {
                v[i++] = uv | (v1[j] << bits);
                uv = (v1[j] >> (SHIFT - bits));
            }
            if (j < l) uv |= v1[j] << bits;
            if (j < rbits) {
                v[i++] = uv; uv = 0; j++;
                for (; j < rbits; j++) v[i++] = 0;
            }
            bits += abits;
            if (bits >= SHIFT) {
                bits -= SHIFT;
                v[i++] = uv;
                if (bits != 0 && j < l) uv = v1[j] >> (abits - bits);
                else uv = 0;
            }
        } else {
            for (j = 0; j < rbits && j < l; j++) v[i++] = v1[j];
            for (; j < rbits; j++) v[i++] = 0;
            uv = (j < l) ? v1[j] : 0;
            bits = abits;
        }
    }
    if (i < sz) v[i] = uv;

    vv->bits = blen;
    return normalize(vv, sz, (vv1->len < 0));
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    size_t sz;
    size_t i, o;
    Bits *vv, *vv1 = Bits(op->v1);
    bdigit_t *v;
    bdigit_t uv;
    bdigit_t inv = (vv1->len < 0) ? ~(bdigit_t)0 : 0;
    int bits;
    Obj *err;
    Funcargs *args = Funcargs(op->v2);
    struct indexoffs_s io;

    if (args->len < 1 || args->len - 1 > indx) {
        return new_error_argnum(args->len, 1, indx + 1, op->epoint2);
    }
    io.len = vv1->bits;
    io.epoint = &args->val[indx].epoint;
    io.val = args->val[indx].val;

    if (io.val->obj->iterable) {
        struct iter_s iter;
        iter.data = io.val; io.val->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(null_bits);
        }
        sz = (iter.len + SHIFT - 1) / SHIFT;

        vv = new_bits2(sz);
        if (vv == NULL) {
            iter_destroy(&iter);
            goto failed;
        }
        v = vv->data;

        uv = inv;
        bits = 0; sz = 0;
        for (i = 0; i < iter.len && (io.val = iter.next(&iter)) != NULL; i++) {
            err = indexoffs(&io);
            if (err != NULL) {
                val_destroy(Obj(vv));
                iter_destroy(&iter);
                return err;
            }
            o = io.offs / SHIFT;
            if (o < bitslen(vv1) && ((vv1->data[o] >> (io.offs % SHIFT)) & 1) != 0) {
                uv ^= 1U << bits;
            }
            bits++;
            if (bits == SHIFT) {
                v[sz++] = uv;
                uv = inv;
                bits = 0;
            }
        }
        iter_destroy(&iter);
        if (bits != 0) v[sz++] = uv & ((1U << bits) - 1);

        vv->bits = i;
        return normalize(vv, sz, false);
    }
    if (io.val->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;
        size_t bo, wo, bl, wl, wl2, l;
        bdigit_t *v1;

        err = sliceparams(&s, &io);
        if (err != NULL) return err;

        if (s.length == 0) {
            return val_reference(null_bits);
        }
        if (s.step == 1) {
            if (s.length == vv1->bits && inv == 0) {
                return Obj(ref_bits(vv1)); /* original bits */
            }

            bo = (uval_t)s.offset % SHIFT;
            wo = (uval_t)s.offset / SHIFT;
            bl = s.length % SHIFT;
            wl = s.length / SHIFT;

            sz = (bl > 0) ? (wl + 1) : wl;
            vv = new_bits2(sz);
            if (vv == NULL) goto failed;
            v = vv->data;

            l = bitslen(vv1);
            v1 = vv1->data + wo;
            wl2 = (wo > l) ? 0 : (l - wo);
            if (bo != 0) {
                for (sz = 0; sz < wl; sz++) {
                    v[sz] = inv ^ (sz < wl2 ? (v1[sz] >> bo) : 0) ^ ((sz + 1 < wl2) ? (v1[sz + 1] << (SHIFT - bo)) : 0);
                }
                if (bl != 0) {
                    v[sz] = sz < wl2 ? (v1[sz] >> bo) : 0;
                    if (bl > (SHIFT - bo) && (sz + 1 < wl2)) v[sz] |= v1[sz + 1] << (SHIFT - bo);
                    v[sz] ^= inv;
                }
            } else {
                for (sz = 0; sz < wl2 && sz < wl; sz++) v[sz] = v1[sz] ^ inv;
                for (; sz < wl; sz++) v[sz] = inv;
                if (bl != 0) v[sz] = inv ^ ((sz < wl2) ? v1[sz] : 0);
            }
            if (bl != 0) v[sz++] &= ((1U << bl) - 1);
        } else {
            sz = s.length / SHIFT;
            if ((s.length % SHIFT) != 0) sz++;
            vv = new_bits2(sz);
            if (vv == NULL) goto failed;
            v = vv->data;

            uv = inv;
            sz = 0; bits = 0;
            l = bitslen(vv1);
            for (i = 0; i < s.length; i++) {
                wo = (uval_t)s.offset / SHIFT;
                if (wo < l && ((vv1->data[wo] >> ((uval_t)s.offset % SHIFT)) & 1) != 0) {
                    uv ^= 1U << bits;
                }
                bits++;
                if (bits == SHIFT) {
                    v[sz++] = uv;
                    uv = inv;
                    bits = 0;
                }
                s.offset += s.step;
            }
            if (bits != 0) v[sz++] = uv & ((1U << bits) - 1);
        }

        vv->bits = s.length;
        return normalize(vv, sz, false);
    }
    err = indexoffs(&io);
    if (err != NULL) return err;

    uv = inv;
    o = io.offs / SHIFT;
    if (o < bitslen(vv1) && ((vv1->data[o] >> (io.offs % SHIFT)) & 1) != 0) {
        uv ^= 1;
    }
    return val_reference(bits_value[uv & 1]);
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bits *v1 = Bits(op->v1);
    Obj *o2 = op->v2;
    Obj *tmp, *result;
    Error *err;
    ival_t shift;

    if (op->op == O_X) {
        if (o2 == none_value) return val_reference(o2);
        return repeat(op);
    }
    if (op->op == O_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((v1->len != 0) ? o2 : Obj(v1));
    }
    if (op->op == O_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((v1->len != 0) ? Obj(v1) : o2);
    }
    if (op->op == O_LXOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return calc2_lxor(op, v1->len != 0);
    }
    switch (o2->obj->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        op->v2 = tmp = val_reference(bits_value[Bool(o2)->value ? 1 : 0]);
        if (op->inplace != NULL && op->inplace->refcount != 1) op->inplace = NULL;
        result = calc2(op);
        val_destroy(tmp);
        return result;
    case T_BITS:
        result = calc2_bits(op);
        if (result != NULL) return result;
        FALL_THROUGH; /* fall through */
    case T_INT:
        switch (op->op) {
        case O_LSHIFT:
            err = o2->obj->ival(o2, &shift, 8 * sizeof shift, op->epoint2);
            if (err != NULL) return Obj(err);
            if (shift == 0) return val_reference(Obj(v1));
            return (shift < 0) ? rshift(op, -(uval_t)shift) : lshift(op, (uval_t)shift);
        case O_RSHIFT:
            err = o2->obj->ival(o2, &shift, 8 * sizeof shift, op->epoint2);
            if (err != NULL) return Obj(err);
            if (shift == 0) return val_reference(Obj(v1));
            return (shift < 0) ? lshift(op, -(uval_t)shift) : rshift(op, (uval_t)shift);
        default: break;
        }
        tmp = int_from_bits(v1, op->epoint);
        op->v1 = tmp;
        op->inplace = (tmp->refcount == 1) ? tmp : NULL;
        result = tmp->obj->calc2(op);
        if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v1));
        val_destroy(tmp);
        return result;
    default:
        if (op->op != O_MEMBER) {
            return o2->obj->rcalc2(op);
        }
        if (o2 == none_value || o2->obj == ERROR_OBJ) return val_reference(o2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Bits *v2 = Bits(op->v2);
    Obj *o1 = op->v1;
    Obj *tmp, *result;

    switch (o1->obj->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        op->v1 = tmp = val_reference(bits_value[Bool(o1)->value ? 1 : 0]);
        op->inplace = NULL;
        result = calc2(op);
        val_destroy(tmp);
        return result;
    case T_BITS:
        result = calc2_bits(op);
        if (result != NULL) return result;
        FALL_THROUGH; /* fall through */
    case T_INT:
        tmp = int_from_bits(v2, op->epoint2);
        op->v2 = tmp;
        op->inplace = NULL;
        result = o1->obj->calc2(op);
        if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v2));
        val_destroy(tmp);
        return result;
    default: break;
    }
    return obj_oper_error(op);
}

void bitsobj_init(void) {
    Type *type = new_type(&obj, T_BITS, "bits", sizeof(Bits));
    type->convert = convert;
    type->convert2 = convert2;
    type->destroy = destroy;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval2;
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
}

void bitsobj_names(void) {
    new_builtin("bits", val_reference(Obj(BITS_OBJ)));
}

void bitsobj_destroy(void) {
#ifdef DEBUG
    if (null_bits->refcount != 1) fprintf(stderr, "bits %" PRIuSIZE "\n", null_bits->refcount - 1);
    if (inv_bits->refcount != 1) fprintf(stderr, "invbits %" PRIuSIZE "\n", inv_bits->refcount - 1);
    if (bits_value[0]->refcount != 1) fprintf(stderr, "bit0 %" PRIuSIZE "\n", bits_value[0]->refcount - 1);
    if (bits_value[1]->refcount != 1) fprintf(stderr, "bit1 %" PRIuSIZE "\n", bits_value[1]->refcount - 1);
    if (ibits_value[0]->refcount != 1) fprintf(stderr, "bit0 %" PRIuSIZE "\n", ibits_value[0]->refcount - 1);
    if (ibits_value[1]->refcount != 1) fprintf(stderr, "bit1 %" PRIuSIZE "\n", ibits_value[1]->refcount - 1);
#endif
}
