/*
    $Id: bytesobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "bytesobj.h"
#include <string.h>
#include "math.h"
#include "eval.h"
#include "unicode.h"
#include "variables.h"
#include "arguments.h"
#include "error.h"

#include "boolobj.h"
#include "floatobj.h"
#include "codeobj.h"
#include "intobj.h"
#include "strobj.h"
#include "bitsobj.h"
#include "listobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"
#include "encobj.h"

static Type obj;

Type *const BYTES_OBJ = &obj;

static Bytes null_bytesval = { { &obj, 1 }, 0, null_bytesval.u.val, { { 0 } } };
static Bytes inv_bytesval = { { &obj, 1 }, ~0, inv_bytesval.u.val, { { 0 } } };

Obj *const null_bytes = &null_bytesval.v;
Obj *const inv_bytes = &inv_bytesval.v;
static Bytes *bytes_value[256];

static inline Bytes *ref_bytes(Bytes *v1) {
    v1->v.refcount++; return v1;
}

static inline size_t byteslen(const Bytes *v1) {
    ssize_t len = v1->len;
    return (size_t)(len < 0 ? ~len : len);
}

static MUST_CHECK Obj *bytes_from_int(const Int *, linepos_t);
static MUST_CHECK Obj *bytes_from_u8(unsigned int i);

MUST_CHECK Obj *bytes_from_obj(Obj *v1, linepos_t epoint) {
    Obj *err, *ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BYTES: return val_reference(v1);
    case T_BOOL: return bytes_from_u8(Bool(v1)->value ? 1 : 0);
    case T_BITS: return bytes_from_bits(Bits(v1), epoint);
    case T_STR: return bytes_from_str(Str(v1), epoint, BYTES_MODE_TEXT);
    case T_INT: return bytes_from_int(Int(v1), epoint);
    case T_CODE: return bytes_from_code(Code(v1), epoint);
    case T_ADDRESS: return bytes_from_address(Address(v1), epoint);
    case T_FLOAT:
         err = int_from_float(Float(v1), epoint);
         if (err->obj != INT_OBJ) return err;
         ret = bytes_from_int(Int(err), epoint);
         val_destroy(err);
         return ret;
    default: break;
    }
    return new_error_conv(v1, BYTES_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return bytes_from_obj(op->v2, op->epoint2);
}

static FAST_CALL NO_INLINE void bytes_destroy(Bytes *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    Bytes *v1 = Bytes(o1);
    if unlikely(v1->u.val != v1->data) bytes_destroy(v1);
}

MALLOC Bytes *new_bytes(size_t ln) {
    Bytes *v = Bytes(val_alloc(BYTES_OBJ));
    if (ln > sizeof v->u.val) {
        v->u.s.max = ln;
        v->u.s.hash = -1;
        new_array(&v->data, ln);
    } else {
        v->data = v->u.val;
    }
    return v;
}

static MALLOC Bytes *new_bytes2(size_t ln) {
    Bytes *v = Bytes(val_alloc(BYTES_OBJ));
    if (ln > sizeof v->u.val) {
        v->u.s.max = ln;
        v->u.s.hash = -1;
        v->data = allocate_array(uint8_t, ln);
        if (v->data == NULL) {
            val_destroy(Obj(v));
            v = NULL;
        }
    } else {
        v->data = v->u.val;
    }
    return v;
}

static uint8_t *extend_bytes(Bytes *v, size_t ln) {
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
        if (tmp == NULL) return tmp;
    } else {
        tmp = allocate_array(uint8_t, ln);
        if (tmp == NULL) return tmp;
        memcpy(tmp, v->u.val, byteslen(v));
    }
    v->data = tmp;
    v->u.s.max = ln;
    v->u.s.hash = -1;
    return tmp;
}

static MUST_CHECK Obj *convert2(oper_t op) {
    Funcargs *v2 = Funcargs(op->v2);
    argcount_t args = v2->len;
    Error *err;
    ival_t ival;
    uval_t len2;
    size_t blen;
    Obj *v;
    Bytes *bytes, *bytes2;
    bool inplace, bits;
    if (args != 2) {
        return new_error_argnum(args, 1, 2, op->epoint2);
    }
    v = v2->val[1].val;
    err = v->obj->ival(v, &ival, 8 * sizeof ival - 3, &v2->val[1].epoint);
    if (err != 0) return Obj(err);
    v = v2->val[0].val;
    inplace = (v->obj == BYTES_OBJ);
    bits = (v->obj == BITS_OBJ);
    if (!inplace) {
        v = bytes_from_obj(v, op->epoint2);
        if (v->obj != BYTES_OBJ) return v;
    }
    bytes = Bytes(v);
    if (ival >= 0) {
        len2 = (uval_t)ival;
        if (!inplace && !bits && bytes->len < 0) {
            val_destroy(Obj(bytes));
            return new_error_obj(ERROR______NOT_UVAL, v2->val[0].val, &v2->val[0].epoint);
        }
    } else {
        len2 = -(uval_t)ival;
    }
    blen = byteslen(bytes);
    if (blen > len2 || (ival < 0 && blen == len2 && bytes->data[len2 - 1] >= 0x80)) {
        if (!inplace) val_destroy(Obj(bytes));
        err = new_error(ival < 0 ? ERROR____CANT_IVAL2 : ERROR____CANT_UVAL2, &v2->val[0].epoint);
        err->u.intconv.bits = len2;
        err->u.intconv.val = val_reference(v2->val[0].val);
        return Obj(err);
    }
    if (blen == len2) {
        return Obj(inplace ? ref_bytes(bytes) : bytes);
    }
    if (bytes->v.refcount == 1) {
        if (extend_bytes(bytes, len2) == NULL) {
            if (!inplace) val_destroy(Obj(bytes));
            return new_error_mem(op->epoint3);
        }
        bytes2 = inplace ? ref_bytes(bytes) : bytes;
        inplace = true;
    } else {
        bytes2 = new_bytes(len2);
        if (blen != 0) memcpy(bytes2->data, bytes->data, blen);
    }
    memset(bytes2->data + blen, 0, len2 - blen);
    bytes2->len = (ssize_t)(bytes->len < 0 ? ~(size_t)len2 : len2);
    if (!inplace) val_destroy(Obj(bytes));
    return Obj(bytes2);
}

static MUST_CHECK Obj *invert(const Bytes *v1, linepos_t epoint) {
    size_t sz;
    sz = byteslen(v1);
    if (sz != 0) {
        Bytes *v = new_bytes2(sz);
        if (v == NULL) return new_error_mem(epoint);
        v->len = ~v1->len;
        memcpy(v->data, v1->data, sz);
        return Obj(v);
    }
    return val_reference((v1->len < 0) ? null_bytes : inv_bytes);
}

static MUST_CHECK Obj *negate(Bytes *v1, linepos_t epoint) {
    size_t i, sz = byteslen(v1);
    Bytes *v;
    if (v1->len == 0) return val_reference(Obj(v1));
    if (v1->len < 0) {
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != (uint8_t)~0) break;
        }
        if (i == sz) return NULL;
        v = new_bytes2(sz);
        if (v == NULL) goto failed;
        v->data[i] = (uint8_t)(v1->data[i] + 1U);
    } else {
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != 0) break;
        }
        if (i == sz) return val_reference(Obj(v1));
        v = new_bytes2(sz);
        if (v == NULL) goto failed;
        v->data[i] = (uint8_t)(v1->data[i] - 1U);
    }
    if (i != 0) memset(v->data, (v1->len < 0) ? 0 : ~0, i);
    i++;
    if (i < sz) memcpy(v->data + i, v1->data + i, sz - i);
    v->len = ~v1->len;
    return Obj(v);
failed:
    return new_error_mem(epoint);
}

static FAST_CALL NO_INLINE bool bytes_same(const Bytes *v1, const Bytes *v2) {
    return memcmp(v1->data, v2->data, byteslen(v2)) == 0;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Bytes *v1 = Bytes(o1), *v2 = Bytes(o2);
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    switch (v1->len) {
    case ~0:
    case 0: return true;
    case ~1:
    case 1: return v1->data[0] == v2->data[0];
    default: return bytes_same(v1, v2);
    }
}

static bool to_bool(const Bytes *v1) {
    size_t i, sz;
    if (v1->len < 0) return true;
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i] != 0) return true;
    }
    return false;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t UNUSED(epoint)) {
    const Bytes *v1 = Bytes(o1);
    size_t i, sz;
    uint8_t inv;
    switch (type) {
    case TRUTH_ALL:
        sz = byteslen(v1);
        inv = (v1->len < 0) ? (uint8_t)~0 : 0;
        for (i = 0; i < sz; i++) {
            if (v1->data[i] == inv) return ref_false();
        }
        return ref_true();
    case TRUTH_ANY:
        if (v1->len == 0 || v1->len == ~0) return ref_false();
        FALL_THROUGH; /* fall through */
    default:
        return truth_reference(to_bool(v1));
    }
}

static uint8_t *z85_encode(uint8_t *dest, const uint8_t *src, size_t len) {
    static const char *z85 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#";
    size_t i;

    for (i = 0; i < len; i += 4) {
        const uint8_t *src2 = src + i;
        uint32_t tmp;
        unsigned int j;

        tmp = (uint32_t)src2[3] | ((uint32_t)src2[2] << 8) | ((uint32_t)src2[1] << 16) | ((uint32_t)src2[0] << 24);

        for (j = 4; j > 0; j--) {
            uint32_t divided = tmp / 85;
            dest[j] = (uint8_t)z85[tmp - divided * 85];
            tmp = divided;
        }
        dest[j] = (uint8_t)z85[tmp];
        dest += 5;
    }
    return dest;
}

static const uint8_t z85_dec[93] = {
        68, 85, 84, 83, 82, 72, 85, 75, 76, 70, 65, 85, 63, 62, 69,
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  64, 85, 73, 66, 74, 71,
    81, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 77, 85, 78, 67, 85,
    85, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 79, 85, 80
};

static const uint8_t *z85_decode(uint8_t *dest, const uint8_t *src, size_t len) {
    size_t i;

    for (i = 0; i < len; i += 4) {
        uint32_t tmp;
        unsigned int j;

        tmp = z85_dec[src[0] - 33];
        for (j = 1; j < 5; j++) {
            tmp = tmp * 85 + z85_dec[src[j] - 33];
        }
        dest[i] = (uint8_t)(tmp >> 24);
        dest[i+1] = (uint8_t)(tmp >> 16);
        dest[i+2] = (uint8_t)(tmp >> 8);
        dest[i+3] = (uint8_t)tmp;
        src += 5;
    }
    return src;
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Bytes *v1 = Bytes(o1);
    static const char *hex = "0123456789abcdef";
    size_t i, len, len2, sz;
    uint8_t *s, b;
    Str *v;
    sz = byteslen(v1);
    if (add_overflow(sz, sz, &len2)) return NULL;
    len = (v1->len < 0) ? 4 : 3;
    if (inc_overflow(&len, len2)) return NULL;
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;

    if (v1->len < 0) *s++ = '~';
    *s++ = 'x';
    *s++ = '\'';
    for (i = 0;i < sz; i++) {
        b = v1->data[i];
        *s++ = (uint8_t)hex[b >> 4];
        *s++ = (uint8_t)hex[b & 0xf];
    }
    *s = '\'';
    return Obj(v);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Bytes *v1 = Bytes(o1);
    size_t len, len2, sz;
    uint8_t *s;
    Str *v;
    sz = byteslen(v1);
    if (sz <= 1) return str(o1, epoint, maxsize);
    len2 = sz / 4 * 5;
    if ((sz & 3) != 0) len2 += (sz & 3) + 1;
    len = (v1->len < 0) ? 4 : 3;
    if (inc_overflow(&len, len2) || sz > SIZE_MAX / 2) return NULL; /* overflow */
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;

    if (v1->len < 0) *s++ = '~';
    *s++ = 'z';
    *s++ = '\'';
    s = z85_encode(s, v1->data, sz & ~3U);
    if ((sz & 3) != 0) {
        uint8_t tmp2[5], tmp[4] = {0, 0, 0, 0};
        memcpy(tmp + 4 - (sz & 3), v1->data + (sz & ~3U), sz & 3);
        sz &= 3;
        z85_encode(tmp2, tmp, sz);
        sz++;
        memcpy(s, tmp2 + 5 - sz, sz);
        s += sz;
    }
    *s = '\'';
    return Obj(v);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bytes *v1 = Bytes(o1);
    size_t l = byteslen(v1);
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

MUST_CHECK Obj *bytes_from_hexstr(const uint8_t *s, linecpos_t *ln, linepos_t epoint) {
    Bytes *v;
    linecpos_t i, j, spc;
    uint8_t ch2, ch = s[0];

    i = 1; j = (s[1] == 0x20) ? 2 : 0; spc = 0;
    for (;;) {
        if ((ch2 = s[i]) == 0) {
            *ln = i;
            return ref_none();
        }
        i++;
        if (ch2 == ch) {
            break; /* end of string; */
        }
        ch2 ^= 0x30;
        if (ch2 < 10) continue;
        ch2 = (uint8_t)((ch2 | 0x20) - 0x71);
        if (ch2 >= 6 && j == 0) {
            if (ch2 == 0xbf && ((i - spc) & 1) == 0) {
                spc++;
                continue;
            }
            j = i;
        }
    }
    if (j == 0 && s[i - 2] == 0x20) j = i - 1;
    *ln = i;
    if (j != 0) {
        struct linepos_s epoint2;
        epoint2 = *epoint;
        epoint2.pos += j;
        err_msg2(ERROR______EXPECTED, "hex digit", &epoint2);
        return ref_none();
    }
    i -= spc;
    if ((i & 1) != 0) err_msg2(ERROR______EXPECTED, "even number of hex digits", epoint);
    j = (i - 2) / 2;
    v = new_bytes2(j);
    if (v == NULL) return new_error_mem(epoint);
    v->len = (ssize_t)j;
    for (i = 0; i < j; i++) {
        unsigned int c1, c2;
        s++;
        c1 = s[i] ^ 0x30;
        if (c1 >= 10) {
            if (c1 == 0x10) {i--; continue;}
            c1 = (c1 | 0x20) - 0x67;
        }
        c2 = s[i+1] ^ 0x30;
        if (c2 >= 10) c2 = (c2 | 0x20) - 0x67;
        v->data[i] = (uint8_t)((c1 << 4) | c2);
    }
    return Obj(v);
}

MUST_CHECK Obj *bytes_from_z85str(const uint8_t *s, linecpos_t *ln, linepos_t epoint) {
    Bytes *v;
    linecpos_t i, j, sz;
    uint8_t ch2, ch = s[0];

    i = 1; j = 0;
    for (;;) {
        if ((ch2 = s[i]) == 0) {
            *ln = i;
            return ref_none();
        }
        i++;
        if (ch2 == ch) {
            break; /* end of string; */
        }
        if (j == 0 && (ch2 <= ' ' || ch2 >= '~' || z85_dec[ch2 - 33] == 85)) j = i;
    }
    *ln = i;
    if (j != 0) {
        struct linepos_s epoint2;
        epoint2 = *epoint;
        epoint2.pos += j;
        err_msg2(ERROR______EXPECTED, "z85 character", &epoint2);
        return ref_none();
    }
    i = (i > 1) ? (i - 2) : 0;
    j = i / 5;
    i -= j * 5;
    j *= 4;
    if (i == 1) {
        err_msg2(ERROR______EXPECTED, "valid z85 string", epoint);
        return ref_none();
    }
    sz = (i > 1) ? (j + i - 1) : j;
    v = new_bytes2(sz);
    if (v == NULL) return new_error_mem(epoint);
    v->len = (ssize_t)sz;
    s = z85_decode(v->data, s + 1, j);
    if (i > 1) {
        uint8_t tmp2[4], tmp[5] = {'0', '0', '0', '0', '0'};
        memcpy(tmp + 5 - i, s, i);
        i--;
        z85_decode(tmp2, tmp, i);
        memcpy(v->data + j, tmp2 + 4 - i, i);
        if (memcmp(tmp2, "\x0\x0\x0\x0", 4 - i) != 0) {
            err_msg2(ERROR______EXPECTED, "valid z85 string", epoint);
        }
    }
    return Obj(v);
}

MUST_CHECK Obj *bytes_from_str(Str *v1, linepos_t epoint, Textconv_types mode) {
    size_t len = v1->len, len2 = (mode == BYTES_MODE_PTEXT || mode == BYTES_MODE_NULL) ? 1 : 0;
    uint8_t *s;
    Bytes *v;
    if (len != 0 || len2 != 0) {
        struct encoder_s *encoder;
        int ch;
        if (actual_encoding->updating) {
            if (v1->chars == 1) {
                unichar_t ch2 = v1->data[0];
                if ((ch2 & 0x80) != 0) utf8in(v1->data, &ch2);
                return bytes_from_uval(ch2, ch2 < 256 ? 1 : ch2 < 65536 ? 2 : 3);
            }
            if (v1->chars != 0) return new_error_obj(ERROR__NOT_ONE_CHAR, Obj(v1), epoint);
            return Obj(new_error(ERROR__EMPTY_STRING, epoint));
        }
        if (len < sizeof v->u.val) len = sizeof v->u.val;
        if (len == 0) {
            return val_reference(null_bytes);
        }
        v = new_bytes2(len);
        if (v == NULL) goto failed;
        s = v->data;
        encoder = enc_string_init(actual_encoding, v1, epoint);
        while ((ch = enc_string(encoder)) != EOF) {
            if (len2 >= len) {
                if (v->u.val == s) {
                    len = 32;
                    s = allocate_array(uint8_t, 32);
                    if (s == NULL) goto failed2;
                    v->data = s;
                    memcpy(s, v->u.val, len2);
                    v->u.s.hash = -1;
                } else {
                    if (inc_overflow(&len, 1024)) goto failed2;
                    s = reallocate_array(s, len);
                    if (s == NULL) goto failed2;
                    v->data = s;
                }
                v->u.s.max = len;
            }
            switch (mode) {
            case BYTES_MODE_SHIFT_CHECK:
            case BYTES_MODE_SHIFT: if ((ch & 0x80) != 0) enc_error(encoder, ERROR___NO_HIGH_BIT); s[len2] = ch & 0x7f; break;
            case BYTES_MODE_SHIFTL: if ((ch & 0x80) != 0) enc_error(encoder, ERROR___NO_HIGH_BIT); s[len2] = (uint8_t)(ch << 1); break;
            case BYTES_MODE_NULL_CHECK:if (ch == 0) {enc_error(encoder, ERROR_NO_ZERO_VALUE); ch = 0xff;} s[len2] = (uint8_t)ch; break;
            case BYTES_MODE_NULL: if (ch == 0) enc_error(encoder, ERROR_NO_ZERO_VALUE); s[len2 - 1] = (uint8_t)ch; break;
            case BYTES_MODE_PTEXT:
            case BYTES_MODE_TEXT: s[len2] = (uint8_t)ch; break;
            }
            len2++;
        }
        switch (mode) {
        case BYTES_MODE_SHIFT: if (len2 != 0) s[len2 - 1] |= 0x80; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint); break;
        case BYTES_MODE_SHIFTL: if (len2 != 0) s[len2 - 1] |= 0x01; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint);break;
        case BYTES_MODE_NULL: s[len2 - 1] = 0x00; break;
        case BYTES_MODE_PTEXT: s[0] = (uint8_t)(len2 - 1); if (len2 > 256) err_msg2(ERROR____PTEXT_LONG, &len2, epoint); break;
        case BYTES_MODE_SHIFT_CHECK:
        case BYTES_MODE_NULL_CHECK:
        case BYTES_MODE_TEXT: break;
        }
        if (v->u.val != s) {
            if (len2 <= sizeof v->u.val) {
                if (len2 != 0) memcpy(v->u.val, s, len2);
                free(s);
                v->data = v->u.val;
            } else if (len2 < len) {
                uint8_t *s2 = reallocate_array(s, len2);
                v->data = (s2 != NULL) ? s2 : s;
                v->u.s.max = len2;
            }
        }
        if (len2 > SSIZE_MAX) goto failed2; /* overflow */
        v->len = (ssize_t)len2;
        return Obj(v);
    }
    if (mode == BYTES_MODE_SHIFT || mode == BYTES_MODE_SHIFTL) err_msg2(ERROR__EMPTY_STRING, NULL, epoint);
    return val_reference(null_bytes);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(epoint);
}

static MUST_CHECK Obj *bytes_from_u8(unsigned int i) {
    Bytes *v;
    i &= 0xff;
    v = bytes_value[i];
    if (v == NULL) {
        v = new_bytes(1);
        v->len = 1;
        v->data[0] = (uint8_t)i;
        bytes_value[i] = v;
    }
    return Obj(ref_bytes(v));
}

MUST_CHECK Obj *bytes_from_uval(uval_t i, unsigned int bytes) {
    Bytes *v = new_bytes(bytes);
    v->len = (ssize_t)bytes;
    switch (bytes) {
    default: v->data[3] = (uint8_t)(i >> 24); FALL_THROUGH; /* fall through */
    case 3: v->data[2] = (uint8_t)(i >> 16); FALL_THROUGH; /* fall through */
    case 2: v->data[1] = (uint8_t)(i >> 8); FALL_THROUGH; /* fall through */
    case 1: v->data[0] = (uint8_t)i; FALL_THROUGH; /* fall through */
    case 0: break;
    }
    return Obj(v);
}

MUST_CHECK Obj *bytes_from_bits(const Bits *v1, linepos_t epoint) {
    size_t i, sz, len1;
    uint8_t *d;
    Bytes *v;
    bool inv = (v1->len < 0);

    len1 = v1->bits;
    if (len1 == 0) {
        return val_reference(inv ? inv_bytes : null_bytes);
    }
    if (len1 <= 8 && !inv) {
        return bytes_from_u8(v1->data[0]);
    }

    sz = len1 / 8;
    if ((len1 % 8) != 0) sz++;
    if (sz > SSIZE_MAX) goto failed; /* overflow */
    v = new_bytes2(sz);
    if (v == NULL) goto failed;
    v->len = (ssize_t)(inv ? ~sz : sz);
    d = v->data;

    len1 = (size_t)(inv ? ~v1->len : v1->len);
    i = 0;
    if (len1 != 0) {
        bdigit_t b = v1->data[0];
        int bits = 0;
        size_t j = 0;
        while (sz > i) {
            d[i++] = (uint8_t)(b >> bits);
            if (bits == (8 * sizeof b) - 8) {
                bits = 0;
                j++;
                if (j >= len1) break;
                b = v1->data[j];
            } else bits += 8;
        }
    }
    if (sz > i) memset(d + i , 0, sz - i);
    return Obj(v);
failed:
    return new_error_mem(epoint);
}

static MUST_CHECK Obj *bytes_from_int(const Int *v1, linepos_t epoint) {
    bool inv, c;
    size_t i, sz;
    uint8_t *d;
    digit_t b2;
    const digit_t *b;
    Bytes *v;

    switch (v1->len) {
    case -1:
        if (v1->data[0] == 1) return val_reference(inv_bytes);
        break;
    case 0:
        return val_reference(null_bytes);
    case 1:
        if (v1->data[0] < 256) return bytes_from_u8(v1->data[0]);
        break;
    default:
        break;
    }

    inv = (v1->len < 0);
    sz = (size_t)(inv ? -v1->len : v1->len);
    if (sz > SSIZE_MAX / sizeof *b) goto failed; /* overflow */
    sz *= sizeof *b;
    v = new_bytes2(sz);
    if (v == NULL) goto failed;
    d = v->data;

    b = v1->data;
    c = inv;
    b2 = 0;
    for (i = 0; i < sz; i++) {
        if (i % sizeof *b == 0) {
            b2 = b[i / sizeof *b];
            if (c) {
                c = (b2 == 0);
                b2--;
            }
        } else b2 >>= 8;
        d[i] = (uint8_t)b2;
    }

    while (sz != 0 && d[sz - 1] == 0) sz--;
    if (sz > SSIZE_MAX) goto failed2; /* overflow */
    v->len = (ssize_t)(inv ? ~sz : sz);
    if (v->u.val != d) {
        if (sz <= sizeof v->u.val) {
            if (sz != 0) memcpy(v->u.val, d, sz);
            free(d);
            v->data = v->u.val;
        } else if (sz < i) {
            uint8_t *d2 = reallocate_array(d, sz);
            v->data = (d2 != NULL) ? d2 : d;
            v->u.s.max = sz;
        }
    }
    return Obj(v);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(epoint);
}

static bool uvalx(Obj *o1, uval_t *uv, unsigned int bits) {
    Bytes *v1 = Bytes(o1);
    size_t ln = byteslen(v1);
    uval_t u;
    if (ln > bits / 8) return false;
    u = 0;
    while (ln != 0) u = (u << 8) | v1->data[--ln];
    *uv = (v1->len < 0) ? ~u : u;
    return true;
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
    if (Bytes(o1)->len < 0) return Error(new_error_obj(ERROR______NOT_UVAL, o1, epoint));
    return uval(o1, uv, bits, epoint);
}

MUST_CHECK Obj *float_from_bytes(const Bytes *v1, linepos_t epoint) {
    double d;
    size_t i, len1;
    switch (v1->len) {
    case ~3: d = -1.0 - (double)(v1->data[0] + v1->data[1] * 256 + v1->data[2] * 65536); break;
    case ~2: d = -1.0 - (double)(v1->data[0] + v1->data[1] * 256); break;
    case ~1: d = -1.0 - v1->data[0]; break;
    case ~0: d = -1.0; break;
    case 0: d = 0.0; break;
    case 1: d = v1->data[0]; break;
    case 2: d = (v1->data[0] + v1->data[1] * 256); break;
    case 3: d = (v1->data[0] + v1->data[1] * 256 + v1->data[2] * 65536); break;
    default:
        d = ((v1->len < 0) ? 1 : 0) + v1->data[0];
        len1 = byteslen(v1);
        for (i = 1; i < len1; i++) {
            d += ldexp(v1->data[i], (int)(i * 8));
        }
        if (v1->len < 0) d = -d;
        return float_from_double(d, epoint);
    }
    return new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = Bytes(o1);
    size_t i, sz;
    if (v1->len < 0) return val_reference(minus1_value);
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i] != 0) return val_reference(int_value[1]);
    }
    return val_reference(int_value[0]);
}

static MUST_CHECK Obj *function(oper_t op) {
    Bytes *v1 = Bytes(op->v2);
    Obj *tmp, *ret;
    op->v2 = tmp = int_from_bytes(v1, op->epoint2);
    op->inplace = tmp->refcount == 1 ? tmp : NULL;
    ret = tmp->obj->function(op);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *len(oper_t op) {
    Bytes *v1 = Bytes(op->v2);
    return int_from_size(byteslen(v1));
}

static FAST_CALL MUST_CHECK Obj *iter_element(struct iter_s *v1, size_t i) {
    Bytes *iter = Bytes(v1->iter);
    const Bytes *vv1 = Bytes(v1->data);
    if (iter->v.refcount != 1) {
        iter->v.refcount--;
        iter = new_bytes(1);
        v1->iter = Obj(iter);
        iter->len = 1;
    }
    iter->data[0] = (vv1->len < 0) ? (uint8_t)~vv1->data[i] : vv1->data[i];
    return Obj(iter);
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
    v->len = byteslen(Bytes(v->data));
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
    v->len = byteslen(Bytes(v->data));
}

static inline MUST_CHECK Obj *binary(oper_t op) {
    Bytes *vv1 = Bytes(op->v1), *vv2 = Bytes(op->v2);
    size_t i, len1, len2, sz;
    bool neg1, neg2, neg;
    const uint8_t *v1, *v2;
    uint8_t *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        sz = len1; len1 = len2; len2 = sz;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    switch (op->op) {
    case O_AND:
        neg = neg1 && neg2;
        sz = neg2 ? len1 : len2;
        break;
    case O_OR:
        neg = neg1 || neg2;
        sz = neg2 ? len2 : len1;
        break;
    default:
        neg = neg1 != neg2;
        sz = len1;
        break;
    }
    if (sz == 0) return val_reference(neg ? inv_bytes : null_bytes);
    if (op->inplace == Obj(vv1)) {
        vv = ref_bytes(vv1);
        if (vv->data != vv->u.val) vv->u.s.hash = -1;
    } else if (op->inplace == Obj(vv2) && sz <= len2) {
        vv = ref_bytes(vv2);
        if (vv->data != vv->u.val) vv->u.s.hash = -1;
    } else {
        vv = new_bytes2(sz);
        if (vv == NULL) return new_error_mem(op->epoint3);
    }
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    i = 0;
    switch (op->op) {
    case O_AND:
        if (neg1) {
            if (neg2) {
                for (; i < len2; i++) v[i] = v1[i] | v2[i];
            } else {
                for (; i < len2; i++) v[i] = (uint8_t)(~v1[i] & v2[i]);
            }
        } else {
            if (neg2) {
                for (; i < len2; i++) v[i] = (uint8_t)(v1[i] & ~v2[i]);
            } else {
                for (; i < len2; i++) v[i] = v1[i] & v2[i];
            }
        }
        break;
    case O_OR:
        if (neg1) {
            if (neg2) {
                for (; i < len2; i++) v[i] = v1[i] & v2[i];
            } else {
                for (; i < len2; i++) v[i] = (uint8_t)(v1[i] & ~v2[i]);
            }
        } else {
            if (neg2) {
                for (; i < len2; i++) v[i] = (uint8_t)(~v1[i] & v2[i]);
            } else {
                for (; i < len2; i++) v[i] = v1[i] | v2[i];
            }
        }
        break;
    default:
        for (; i < len2; i++) v[i] = v1[i] ^ v2[i];
        break;
    }
    for (; i < sz; i++) v[i] = v1[i];
    /*if (i > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (ssize_t)(neg ? ~i : i);
    return Obj(vv);
}

static MUST_CHECK Obj *concat(oper_t op) {
    Bytes *v1 = Bytes(op->v1), *v2 = Bytes(op->v2), *v;
    uint8_t *s;
    bool inv;
    size_t ln, i, len1, len2;

    if (v2->len == 0 || v2->len == ~(ssize_t)0) {
        return Obj(ref_bytes(v1));
    }
    inv = (v1->len ^ v2->len) < 0;
    if ((v1->len == 0 || v1->len == ~(ssize_t)0) && !inv) {
        return Obj(ref_bytes(v2));
    }
    len1 = byteslen(v1);
    len2 = byteslen(v2);
    if (add_overflow(len1, len2, &ln) || ln > SSIZE_MAX) goto failed; /* overflow */
    if (op->inplace == Obj(v1)) {
        size_t ln2;
        if (ln > sizeof v1->u.val && v1->u.val != v1->data && ln > v1->u.s.max) {
            ln2 = ln + (ln < 1024 ? ln : 1024);
            if (ln2 < ln) ln2 = ln;
        } else ln2 = ln;
        s = extend_bytes(v1, ln2);
        if (s == NULL) goto failed;
        v = ref_bytes(v1);
    } else if (op->inplace == Obj(v2)) {
        size_t ln2;
        if (ln > sizeof v2->u.val && v2->u.val != v2->data && ln > v2->u.s.max) {
            ln2 = ln + (ln < 1024 ? ln : 1024);
            if (ln2 < ln) ln2 = ln;
        } else ln2 = ln;
        s = extend_bytes(v2, ln2);
        if (s == NULL) goto failed;
        if (inv) {
            for (i = len2; i != 0;) {
                i--;
                s[i + len1] = (uint8_t)~v2->data[i];
            }
        } else memmove(s + len1, v2->data, len2);
        memcpy(s, v1->data, len1);
        v2->len = (ssize_t)(v1->len < 0 ? ~ln : ln);
        return val_reference(Obj(v2));
    } else {
        v = new_bytes2(ln);
        if (v == NULL) goto failed;
        s = v->data;
        memcpy(s, v1->data, len1);
    }
    if (inv) {
        for (i = 0; i < len2; i++) s[i + len1] = (uint8_t)~v2->data[i];
    } else if (len2 == 1) {
        s[len1] = v2->data[0];
    } else {
        memcpy(s + len1, v2->data, len2);
    }
    v->len = (ssize_t)(v1->len < 0 ? ~ln : ln);
    return Obj(v);
failed:
    return new_error_mem(op->epoint3);
}

static int icmp(oper_t op) {
    const Bytes *v1 = Bytes(op->v1), *v2 = Bytes(op->v2);
    size_t len1 = byteslen(v1), len2 = byteslen(v2);
    size_t i, ln = (len1 < len2) ? len1 : len2;
    int h;
    if ((v1->len >= 0) == (v2->len >= 0)) {
        h = memcmp(v1->data, v2->data, ln);
    } else {
        h = 0;
        for (i = 0; i < ln; i++) {
            h = v1->data[i] - (uint8_t)~v2->data[i];
            if (h != 0) break;
        }
    }
    if (h != 0) return (v1->len >= 0) ? h : -h;
    if (len1 < len2) return -1;
    return (len1 > len2) ? 1 : 0;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bytes *v1 = Bytes(op->v1);
    Obj *v;
    Obj *tmp;
    unsigned int u;
    switch (op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        u = (v1->len > 0 || v1->len < ~0) ? v1->data[0] : 0;
        if (v1->len > 1 || v1->len < ~1) u |= (unsigned int)v1->data[1] << 8;
        if (v1->len > 2 || v1->len < ~2) u |= (unsigned int)v1->data[2] << 8;
        return bits_calc1(op->op, (v1->len < 0) ? ~u : u);
    case O_POS: return val_reference(Obj(v1));
    case O_INV:
        if (op->inplace != Obj(v1)) return invert(v1, op->epoint3);
        v1->len = ~v1->len;
        if (v1->data != v1->u.val) v1->u.s.hash = -1;
        return val_reference(Obj(v1));
    case O_NEG:
        v = negate(v1, op->epoint3);
        if (v != NULL) return v;
        FALL_THROUGH; /* fall through */
    case O_STRING:
        tmp = int_from_bytes(v1, op->epoint);
        op->v1 = tmp;
        op->inplace = (tmp->refcount == 1) ? tmp : NULL;
        v = tmp->obj->calc1(op);
        val_destroy(tmp);
        return v;
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return truth_reference(!to_bool(v1));
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2_bytes(oper_t op) {
    Bytes *v1 = Bytes(op->v1), *v2 = Bytes(op->v2);
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
            tmp = int_from_bytes(v1, op->epoint);
            tmp2 = int_from_bytes(v2, op->epoint2);
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
    case O_XOR: return binary(op);
    case O_LSHIFT:
    case O_RSHIFT:
        {
            Obj *tmp, *result;
            tmp = bits_from_bytes(v1, op->epoint);
            op->v1 = tmp;
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v1));
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
    case O_CONCAT: return concat(op);
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Bytes *v1 = Bytes(op->v1), *v;
    uval_t rep;
    size_t len1 = byteslen(v1);
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return Obj(err);

    if (len1 == 0 || rep == 0) return val_reference((v1->len < 0) ? inv_bytes : null_bytes);
    do {
        size_t sz;
        if (rep == 1) {
            return Obj(ref_bytes(v1));
        }
        if (len1 > SSIZE_MAX / rep) break; /* overflow */
        sz = len1 * rep;
        if (op->inplace == Obj(v1)) {
            if (extend_bytes(v1, sz) == NULL) break;
            v = ref_bytes(v1);
        } else {
            v = new_bytes2(sz);
            if (v == NULL) break;
        }
        v->len = (ssize_t)(v1->len < 0 ? ~sz : sz);
        if (len1 == 1) {
            memset(v->data, v1->data[0], sz);
        } else {
            if (v->data != v1->data) memcpy(v->data, v1->data, len1);
            while (sz > len1) {
                size_t oln = len1;
                if (len1 > sz - len1) len1 = sz - len1;
                memcpy(v->data + oln, v->data, len1);
                len1 += oln;
            }
        }
        return Obj(v);
    } while (false);
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    uint8_t *p2;
    size_t i;
    Bytes *v, *v1 = Bytes(op->v1);
    Obj *err;
    uint8_t inv = (v1->len < 0) ? (uint8_t)~0 : 0;
    Funcargs *args = Funcargs(op->v2);
    struct indexoffs_s io;

    if (args->len < 1 || args->len - 1 > indx) {
        return new_error_argnum(args->len, 1, indx + 1, op->epoint2);
    }
    io.len = byteslen(v1);
    io.epoint = &args->val[indx].epoint;
    io.val = args->val[indx].val;

    if (io.val->obj->iterable) {
        struct iter_s iter;
        iter.data = io.val; io.val->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(null_bytes);
        }
        v = new_bytes2(iter.len);
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
            p2[i] = v1->data[io.offs] ^ inv;
        }
        iter_destroy(&iter);
        if (i > SSIZE_MAX) goto failed2; /* overflow */
        v->len = (ssize_t)i;
        return Obj(v);
    }
    if (io.val->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;

        err = sliceparams(&s, &io);
        if (err != NULL) return err;

        switch (s.length) {
        case 0:
            return val_reference(null_bytes);
        case 1:
            return bytes_from_u8(v1->data[s.offset] ^ inv);
        }
        if (s.step == 1 && inv == 0) {
            if (s.length == byteslen(v1)) {
                return Obj(ref_bytes(v1)); /* original bytes */
            }
            if (op->inplace == Obj(v1)) {
                v = ref_bytes(v1);
                if (v->data != v->u.val && s.length <= sizeof v->u.val) {
                    p2 = v->u.val;
                    memcpy(p2, v1->data + s.offset, s.length);
                } else {
                    p2 = v->data;
                    if (s.offset != 0) memmove(p2, v1->data + s.offset, s.length);
                    if (v->data != v->u.val) v->u.s.hash = -1;
                }
            } else {
                v = new_bytes2(s.length);
                if (v == NULL) goto failed;
                p2 = v->data;
                memcpy(p2, v1->data + s.offset, s.length);
            }
        } else {
            if (s.step > 0 && op->inplace == Obj(v1)) {
                v = ref_bytes(v1);
                if (v->data != v->u.val && s.length <= sizeof v->u.val) {
                    p2 = v->u.val;
                } else {
                    p2 = v->data;
                    if (v->data != v->u.val) v->u.s.hash = -1;
                }
            } else {
                v = new_bytes2(s.length);
                if (v == NULL) goto failed;
                p2 = v->data;
            }
            for (i = 0; i < s.length; i++) {
                p2[i] = v1->data[s.offset] ^ inv;
                s.offset += s.step;
            }
        }
        if (p2 != v->data) {
            free(v->data);
            v->data = p2;
        }
        v->len = (ssize_t)s.length;
        return Obj(v);
    }
    err = indexoffs(&io);
    if (err != NULL) return err;
    return bytes_from_u8(v1->data[io.offs] ^ inv);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(op->epoint3);
}

static bool bytes_contains(oper_t op) {
    const Bytes *v1 = Bytes(op->v1);
    const Bytes *v2 = Bytes(op->v2);
    const uint8_t *c, *c2, *e;
    size_t len1 = byteslen(v1), len2 = byteslen(v2), i;
    if (len1 == 0) return true;
    if (len1 > len2) return false;
    c2 = v2->data;
    e = c2 + len2 - len1;
    if ((v1->len ^ v2->len) < 0) {
        for (;;) {
            c = (const uint8_t *)memchr(c2, ~v1->data[0], (size_t)(e - c2) + 1);
            if (c == NULL) return false;
            for (i = 1; i < len1; i++) {
                if (c[i] != (0xff - v1->data[i])) break;
            }
            if (i == len1) return true;
            c2 = c + 1;
        }
    } else {
        for (;;) {
            c = (const uint8_t *)memchr(c2, v1->data[0], (size_t)(e - c2) + 1);
            if (c == NULL) return false;
            if (memcmp(c, v1->data, len1) == 0) return true;
            c2 = c + 1;
        }
    }
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1;
    if (o1->obj == BYTES_OBJ) {
        bool result = bytes_contains(op);
        return truth_reference(op->op == O_IN ? result : !result);
    }
    if (o1 == none_value || o1->obj == ERROR_OBJ) return val_reference(o1);
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bytes *v1 = Bytes(op->v1);
    Obj *o2 = op->v2;
    Obj *tmp;

    if (op->op == O_X) {
        if (o2 == none_value) return val_reference(o2);
        return repeat(op);
    }
    if (op->op == O_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? o2 : Obj(v1));
    }
    if (op->op == O_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? Obj(v1) : o2);
    }
    if (op->op == O_LXOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return calc2_lxor(op, to_bool(v1));
    }
    if (o2->obj->iterable) {
        if (op->op != O_MEMBER) {
            return o2->obj->rcalc2(op);
        }
    }
    switch (o2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
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
            case O_RSHIFT: tmp = bits_from_bytes(v1, op->epoint); break;
            default: tmp = int_from_bytes(v1, op->epoint);
            }
            op->v1 = tmp;
            op->inplace = (tmp->refcount == 1) ? tmp : NULL;
            result = tmp->obj->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v1));
            val_destroy(tmp);
            return result;
        }
    case T_STR:
    case T_GAP:
        if (op->op != O_MEMBER) {
            return o2->obj->rcalc2(op);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Bytes *v2 = Bytes(op->v2);
    Obj *o1 = op->v1;
    Obj *tmp;
    switch (o1->obj->type) {
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
            case O_XOR: tmp = bits_from_bytes(v2, op->epoint2); break;
            default: tmp = int_from_bytes(v2, op->epoint2);
            }
            op->v2 = tmp;
            op->inplace = NULL;
            result = o1->obj->calc2(op);
            if (result->obj == ERROR_OBJ) error_obj_update(Error(result), tmp, Obj(v2));
            val_destroy(tmp);
            return result;
        }
    default:
        if (!o1->obj->iterable) {
            break;
        }
        FALL_THROUGH; /* fall through */
    case T_NONE:
    case T_ERROR:
        return o1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

void bytesobj_init(void) {
    Type *type = new_type(&obj, T_BYTES, "bytes", sizeof(Bytes));
    type->convert = convert;
    type->convert2 = convert2;
    type->destroy = destroy;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
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
    type->contains = contains;
}

void bytesobj_names(void) {
    new_builtin("bytes", val_reference(Obj(BYTES_OBJ)));
}

void bytesobj_destroy(void) {
    int i;
#ifdef DEBUG
    if (null_bytes->refcount != 1) fprintf(stderr, "bytes %" PRIuSIZE "\n", null_bytes->refcount - 1);
    if (inv_bytes->refcount != 1) fprintf(stderr, "invbytes %" PRIuSIZE "\n", inv_bytes->refcount - 1);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] && bytes_value[i]->v.refcount != 1) {
            fprintf(stderr, "bytes[%d] %" PRIuSIZE "\n", i, bytes_value[i]->v.refcount - 1);
        }
    }
#endif

    for (i = 0; i < 256; i++) {
        if (bytes_value[i] == NULL) continue;
        val_destroy(Obj(bytes_value[i]));
        bytes_value[i] = NULL;
    }
}
