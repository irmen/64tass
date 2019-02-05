/*
    $Id: bytesobj.c 1861 2019-02-03 19:36:52Z soci $

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
#include "encoding.h"
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
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"

static Type obj;

Type *const BYTES_OBJ = &obj;
Bytes *null_bytes;
Bytes *inv_bytes;
static Bytes *bytes_value[256];

static inline size_t byteslen(const Bytes *v1) {
    ssize_t len = v1->len;
    return (len < 0) ? (size_t)~len : (size_t)len;
}

static MUST_CHECK Obj *bytes_from_bits(const Bits *, linepos_t);
static MUST_CHECK Obj *bytes_from_int(const Int *, linepos_t);

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    Obj *err, *ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BYTES: return val_reference(v1);
    case T_BOOL: return (Obj *)bytes_from_u8((Bool *)v1 == true_value ? 1 : 0);
    case T_BITS: return bytes_from_bits((Bits *)v1, epoint);
    case T_STR: return bytes_from_str((Str *)v1, epoint, BYTES_MODE_TEXT);
    case T_INT: return bytes_from_int((Int *)v1, epoint);
    case T_CODE: return bytes_from_code((Code *)v1, epoint);
    case T_ADDRESS: return bytes_from_address((Address *)v1, epoint);
    case T_FLOAT:
         err = int_from_float((Float *)v1, epoint);
         if (err->obj != INT_OBJ) return err;
         ret = bytes_from_int((Int *)err, epoint);
         val_destroy(err);
         return ret;
    default: break;
    }
    return (Obj *)new_error_conv(v1, BYTES_OBJ, epoint);
}

static FAST_CALL void destroy(Obj *o1) {
    Bytes *v1 = (Bytes *)o1;
    if (v1->u.val != v1->data) free(v1->data);
}

MALLOC Bytes *new_bytes(size_t ln) {
    Bytes *v = (Bytes *)val_alloc(BYTES_OBJ);
    if (ln > sizeof v->u.val) {
        v->u.s.max = ln;
        v->u.s.hash = -1;
        v->data = (uint8_t *)mallocx(ln);
    } else {
        v->data = v->u.val;
    }
    return v;
}

static MALLOC Bytes *new_bytes2(size_t ln) {
    Bytes *v = (Bytes *)val_alloc(BYTES_OBJ);
    if (ln > sizeof v->u.val) {
        v->u.s.max = ln;
        v->u.s.hash = -1;
        v->data = (uint8_t *)malloc(ln);
        if (v->data == NULL) {
            val_destroy(&v->v);
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
        size_t ln2;
        if (ln <= v->u.s.max) return v->data;
        ln2 = ln + (ln < 1024 ? ln : 1024);
        if (ln2 > ln) ln = ln2;
        tmp = (uint8_t *)realloc(v->data, ln);
        if (tmp != NULL) {
            v->data = tmp;
            v->u.s.max = ln;
            v->u.s.hash = -1;
        }
        return tmp;
    }
    tmp = (uint8_t *)malloc(ln);
    if (tmp != NULL) {
        memcpy(tmp, v->u.val, v->len);
        v->data = tmp;
        v->u.s.max = ln;
        v->u.s.hash = -1;
    }
    return tmp;
}

static MUST_CHECK Obj *invert(const Bytes *v1, linepos_t epoint) {
    size_t sz;
    sz = byteslen(v1);
    if (sz != 0) {
        Bytes *v = new_bytes2(sz);
        if (v == NULL) return (Obj *)new_error_mem(epoint);
        v->len = ~v1->len;
        memcpy(v->data, v1->data, sz);
        return &v->v;
    }
    return (Obj *)ref_bytes((v1->len < 0) ? null_bytes : inv_bytes);
}

static MUST_CHECK Obj *negate(Bytes *v1, linepos_t epoint) {
    size_t i, sz = byteslen(v1);
    Bytes *v;
    if (v1->len == 0) return val_reference(&v1->v);
    if (v1->len < 0) {
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != (uint8_t)~0) break;
        }
        if (i == sz) return NULL;
        v = new_bytes2(sz);
        if (v == NULL) goto failed;
        v->data[i] = v1->data[i] + 1;
    } else {
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != 0) break;
        }
        if (i == sz) return val_reference(&v1->v);
        v = new_bytes2(sz);
        if (v == NULL) goto failed;
        v->data[i] = v1->data[i] - 1;
    }
    if (i != 0) memset(v->data, (v1->len < 0) ? 0 : ~0, i);
    i++;
    if (i < sz) memcpy(v->data + i, v1->data + i, sz - i);
    v->len = ~v1->len;
    return &v->v;
failed:
    return (Obj *)new_error_mem(epoint);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Bytes *v1 = (const Bytes *)o1, *v2 = (const Bytes *)o2;
    return o2->obj == BYTES_OBJ && v1->len == v2->len && (
            v1->data == v2->data ||
            memcmp(v1->data, v2->data, byteslen(v2)) == 0);
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

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    const Bytes *v1 = (const Bytes *)o1;
    size_t i, sz;
    uint8_t inv;
    switch (type) {
    case TRUTH_ALL:
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, o1, epoint);
        sz = byteslen(v1);
        inv = (v1->len < 0) ? (uint8_t)~0 : 0;
        for (i = 0; i < sz; i++) {
            if (v1->data[i] == inv) return (Obj *)ref_bool(false_value);
        }
        return (Obj *)ref_bool(true_value);
    case TRUTH_ANY:
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, o1, epoint);
        /* fall through */
    default:
        return truth_reference(to_bool(v1));
    }
    return DEFAULT_OBJ->truth(o1, type, epoint);
}

static uint8_t *z85_encode(uint8_t *dest, const uint8_t *src, size_t len) {
    static const char *z85 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#";
    size_t i;

    for (i = 0; i < len; i += 4) {
        uint8_t src2[4];
        uint32_t tmp;
        unsigned int j;

        memcpy(src2, src + i, 4);
        tmp = src2[3] | (src2[2] << 8) | (src2[1] << 16) | (src2[0] << 24);

        for (j = 4; j > 0; j--) {
            uint32_t div = tmp / 85;
            dest[j] = z85[tmp - div * 85];
            tmp = div;
        }
        dest[j] = z85[tmp];
        dest += 5;
    }
    return dest;
}

const uint8_t z85_dec[93] = {
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
        dest[i] = tmp >> 24;
        dest[i+1] = tmp >> 16;
        dest[i+2] = tmp >> 8;
        dest[i+3] = tmp;
        src += 5;
    }
    return src;
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Bytes *v1 = (Bytes *)o1;
    static const char *hex = "0123456789abcdef";
    size_t i, len, len2, sz;
    uint8_t *s, b;
    Str *v;
    sz = byteslen(v1);
    len2 = sz * 2;
    len = (v1->len < 0) ? 4 : 3;
    len += len2;
    if (len < len2 || sz > SIZE_MAX / 2) return NULL; /* overflow */
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
    return &v->v;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Bytes *v1 = (Bytes *)o1;
    size_t len, len2, sz;
    uint8_t *s;
    Str *v;
    sz = byteslen(v1);
    if (sz <= 1) return str(o1, epoint, maxsize);
    len2 = sz / 4 * 5;
    if ((sz & 3) != 0) len2 += (sz & 3) + 1;
    len = (v1->len < 0) ? 4 : 3;
    len += len2;
    if (len < len2 || sz > SIZE_MAX / 2) return NULL; /* overflow */
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;

    if (v1->len < 0) *s++ = '~';
    *s++ = 'z';
    *s++ = '\'';
    s = z85_encode(s, v1->data, sz & ~3);
    if ((sz & 3) != 0) {
        uint8_t tmp2[5], tmp[4] = {0, 0, 0, 0};
        memcpy(tmp + 4 - (sz & 3), v1->data + (sz & ~3), sz & 3);
        sz &= 3;
        z85_encode(tmp2, tmp, sz);
        sz++;
        memcpy(s, tmp2 + 5 - sz, sz);
        s += sz;
    }
    *s = '\'';
    return &v->v;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
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
    if (v1->data != v1->u.val) v1->u.s.hash = h;
    *hs = h;
    return NULL;
}

MUST_CHECK Obj *bytes_from_hexstr(const uint8_t *s, size_t *ln, linepos_t epoint) {
    Bytes *v;
    size_t i, j;
    uint8_t ch2, ch = s[0];

    i = 1; j = 0;
    for (;;) {
        if ((ch2 = s[i]) == 0) {
            *ln = i;
            return (Obj *)ref_none();
        }
        i++;
        if (ch2 == ch) {
            break; /* end of string; */
        }
        ch2 ^= 0x30;
        if (ch2 < 10) continue;
        ch2 = (ch2 | 0x20) - 0x71;
        if (ch2 >= 6 && j == 0) j = i;
    }
    *ln = i;
    if (j != 0) {
        struct linepos_s epoint2;
        epoint2 = *epoint;
        epoint2.pos += j;
        err_msg2(ERROR______EXPECTED, "hex digit", &epoint2);
        return (Obj *)ref_none();
    }
    j = (i > 1) ? (i - 2) : 0;
    j /= 2;
    v = new_bytes2(j);
    if (v == NULL) return (Obj *)new_error_mem(epoint);
    v->len = j;
    for (i = 0; i < (size_t)v->len; i++) {
        uint8_t c1, c2;
        s++;
        c1 = s[i] ^ 0x30;
        if (c1 >= 10) c1 = (c1 | 0x20) - 0x67;
        c2 = s[i+1] ^ 0x30;
        if (c2 >= 10) c2 = (c2 | 0x20) - 0x67;
        v->data[i] = (c1 << 4) | c2;
    }
    if ((*ln & 1) != 0) err_msg2(ERROR______EXPECTED, "even number of hex digits", epoint);
    return &v->v;
}

MUST_CHECK Obj *bytes_from_z85str(const uint8_t *s, size_t *ln, linepos_t epoint) {
    Bytes *v;
    size_t i, j, sz;
    uint8_t ch2, ch = s[0];

    i = 1; j = 0;
    for (;;) {
        if ((ch2 = s[i]) == 0) {
            *ln = i;
            return (Obj *)ref_none();
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
        return (Obj *)ref_none();
    }
    i = (i > 1) ? (i - 2) : 0;
    j = i / 5;
    i -= j * 5;
    j *= 4;
    if (i == 1) {
        err_msg2(ERROR______EXPECTED, "valid z85 string", epoint);
        return (Obj *)ref_none();
    }
    sz = (i > 1) ? (j + i - 1) : j;
    v = new_bytes2(sz);
    if (v == NULL) return (Obj *)new_error_mem(epoint);
    v->len = sz;
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
    return &v->v;
}

MUST_CHECK Obj *bytes_from_str(const Str *v1, linepos_t epoint, Textconv_types mode) {
    size_t len = v1->len, len2 = (mode == BYTES_MODE_PTEXT || mode == BYTES_MODE_NULL) ? 1 : 0;
    uint8_t *s;
    Bytes *v;
    if (len != 0 || len2 != 0) {
        int ch;
        if (actual_encoding == NULL) {
            if (v1->chars == 1) {
                uchar_t ch2 = v1->data[0];
                if ((ch2 & 0x80) != 0) utf8in(v1->data, &ch2);
                return (Obj *)bytes_from_uval(ch2, 3);
            }
            return (Obj *)new_error((v1->chars == 0) ? ERROR__EMPTY_STRING : ERROR__NOT_ONE_CHAR, epoint);
        }
        if (len < sizeof v->u.val) len = sizeof v->u.val;
        if (len == 0) {
            return (Obj *)ref_bytes(null_bytes);
        }
        v = new_bytes2(len);
        if (v == NULL) goto failed;
        s = v->data;
        encode_string_init(v1, epoint);
        while ((ch = encode_string()) != EOF) {
            if (len2 >= len) {
                if (v->u.val == s) {
                    len = 32;
                    s = (uint8_t *)malloc(len);
                    if (s == NULL) goto failed2;
                    v->data = s;
                    memcpy(s, v->u.val, len2);
                    v->u.s.hash = -1;
                } else {
                    len += 1024;
                    if (len < 1024) goto failed2; /* overflow */
                    s = (uint8_t *)realloc(s, len);
                    if (s == NULL) goto failed2;
                    v->data = s;
                }
                v->u.s.max = len;
            }
            switch (mode) {
            case BYTES_MODE_SHIFT_CHECK:
            case BYTES_MODE_SHIFT: if ((ch & 0x80) != 0) encode_error(ERROR___NO_HIGH_BIT); s[len2] = ch & 0x7f; break;
            case BYTES_MODE_SHIFTL: if ((ch & 0x80) != 0) encode_error(ERROR___NO_HIGH_BIT); s[len2] = (uint8_t)(ch << 1); break;
            case BYTES_MODE_NULL_CHECK:if (ch == 0) {encode_error(ERROR_NO_ZERO_VALUE); ch = 0xff;} s[len2] = (uint8_t)ch; break;
            case BYTES_MODE_NULL: if (ch == 0) encode_error(ERROR_NO_ZERO_VALUE); s[len2 - 1] = (uint8_t)ch; break;
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
                uint8_t *s2 = (uint8_t *)realloc(s, len2);
                v->data = (s2 != NULL) ? s2 : s;
                v->u.s.max = len2;
            }
        }
        if (len2 > SSIZE_MAX) goto failed2; /* overflow */
        v->len = (ssize_t)len2;
        return &v->v;
    }
    if (actual_encoding != NULL) {
        if (mode == BYTES_MODE_SHIFT || mode == BYTES_MODE_SHIFTL) err_msg2(ERROR__EMPTY_STRING, NULL, epoint);
    }
    return (Obj *)ref_bytes(null_bytes);
failed2:
    val_destroy(&v->v);
failed:
    return (Obj *)new_error_mem(epoint);
}

MUST_CHECK Bytes *bytes_from_u8(unsigned int i) {
    Bytes *v;
    i &= 0xff;
    v = bytes_value[i];
    if (v == NULL) {
        v = new_bytes(1);
        v->len = 1;
        v->data[0] = (uint8_t)i;
        bytes_value[i] = v;
    }
    return ref_bytes(v);
}

MUST_CHECK Bytes *bytes_from_u16(unsigned int i) {
    Bytes *v = new_bytes(2);
    v->len = 2;
    v->data[0] = (uint8_t)i;
    v->data[1] = (uint8_t)(i >> 8);
    return v;
}

MUST_CHECK Bytes *bytes_from_uval(uval_t i, unsigned int bytes) {
    Bytes *v = new_bytes(bytes);
    v->len = (ssize_t)bytes;
    switch (bytes) {
    default: v->data[3] = (uint8_t)(i >> 24); /* fall through */
    case 3: v->data[2] = (uint8_t)(i >> 16); /* fall through */
    case 2: v->data[1] = (uint8_t)(i >> 8); /* fall through */
    case 1: v->data[0] = (uint8_t)i; /* fall through */
    case 0: break;
    }
    return v;
}

static MUST_CHECK Obj *bytes_from_bits(const Bits *v1, linepos_t epoint) {
    size_t i, sz, len1;
    uint8_t *d;
    Bytes *v;
    bool inv = (v1->len < 0);

    len1 = v1->bits;
    if (len1 == 0) {
        return val_reference(inv ? &inv_bytes->v : &null_bytes->v);
    }
    if (len1 <= 8 && !inv) {
        return (Obj *)bytes_from_u8(v1->data[0]);
    }

    sz = len1 / 8;
    if ((len1 % 8) != 0) sz++;
    if (sz > SSIZE_MAX) goto failed; /* overflow */
    v = new_bytes2(sz);
    if (v == NULL) goto failed;
    v->len = inv ? (ssize_t)~sz : (ssize_t)sz;
    d = v->data;

    len1 = inv ? (size_t)~v1->len : (size_t)v1->len;
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
    return &v->v;
failed:
    return (Obj *)new_error_mem(epoint);
}

static MUST_CHECK Obj *bytes_from_int(const Int *v1, linepos_t epoint) {
    bool inv;
    size_t i, j, sz, bits;
    uint8_t *d;
    const digit_t *b;
    Bytes *v;

    switch (v1->len) {
    case -1:
        if (v1->data[0] == 1) return val_reference(&inv_bytes->v);
        break;
    case 0:
        return val_reference(&null_bytes->v);
    case 1:
        if (v1->data[0] < 256) return (Obj *)bytes_from_u8(v1->data[0]);
        break;
    default:
        break;
    }

    inv = (v1->len < 0);
    sz = inv ? (size_t)-v1->len : (size_t)v1->len;
    if (sz > SSIZE_MAX / sizeof *b) goto failed; /* overflow */
    sz *= sizeof *b;
    v = new_bytes2(sz);
    if (v == NULL) goto failed;
    d = v->data;

    b = v1->data;
    if (inv) {
        bool c = (b[0] == 0);
        digit_t b2 = b[0] - 1;
        bits = j = 0;
        for (i = 0; i < sz; i++) {
            d[i] = (uint8_t)(b2 >> bits);
            if (bits == (8 * sizeof b2) - 8) {
                j++;
                if (c) {
                    c = (b[j] == 0);
                    b2 = b[j] - 1;
                } else b2 = b[j];
                bits = 0;
            } else bits += 8;
        }
    } else {
        digit_t b2 = b[0];
        bits = j = 0;
        for (i = 0; i < sz; i++) {
            d[i] = (uint8_t)(b2 >> bits);
            if (bits == (8 * sizeof b2) - 8) {
                j++;
                b2 = b[j];
                bits = 0;
            } else bits += 8;
        }
    }

    while (sz != 0 && d[sz - 1] == 0) sz--;
    if (v->u.val != d) {
        if (sz <= sizeof v->u.val) {
            if (sz != 0) memcpy(v->u.val, d, sz);
            free(d);
            v->data = v->u.val;
        } else if (sz < i) {
            uint8_t *d2 = (uint8_t *)realloc(d, sz);
            v->data = (d2 != NULL) ? d2 : d;
            v->u.s.max = sz;
        }
    }
    if (sz > SSIZE_MAX) goto failed2; /* overflow */
    v->len = inv ? (ssize_t)~sz : (ssize_t)sz;

    return &v->v;
failed2:
    val_destroy(&v->v);
failed:
    return (Obj *)new_error_mem(epoint);
}

static bool uvalx(Obj *o1, uval_t *uv, unsigned int bits) {
    Bytes *v1 = (Bytes *)o1;
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
    if (((Bytes *)o1)->len < 0) {
        Error *v = new_error(ERROR______NOT_UVAL, epoint);
        v->u.intconv.val = val_reference(o1);
        return v;
    }
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
    return (Obj *)new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    size_t i, sz;
    if (v1->len < 0) return (Obj *)ref_int(minus1_value);
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i] != 0) return (Obj *)ref_int(int_value[1]);
    }
    return (Obj *)ref_int(int_value[0]);
}

static MUST_CHECK Obj *function(Obj *o1, Func_types f, linepos_t epoint) {
    Bytes *v1 = (Bytes *)o1;
    Obj *tmp = int_from_bytes(v1, epoint);
    Obj *ret = tmp->obj->function(tmp, f, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    return (Obj *)int_from_size(byteslen(v1));
}

static size_t iter_len(Iter *v1) {
    return ((Bytes *)v1->data)->len - v1->val;
}

static FAST_CALL MUST_CHECK Obj *next(Iter *v1) {
    Bytes *iter;
    const Bytes *vv1 = (Bytes *)v1->data;
    if (v1->val >= byteslen(vv1)) return NULL;
    iter = (Bytes *)v1->iter;
    if (iter == NULL) {
    renew:
        iter = new_bytes(1);
        v1->iter = &iter->v;
        iter->len = (vv1->len < 0) ? ~1 : 1;
    } else if (iter->v.refcount != 1) {
        iter->v.refcount--;
        goto renew;
    }
    iter->data[0] = vv1->data[v1->val++];
    return &iter->v;
}

static MUST_CHECK Iter *getiter(Obj *v1) {
    Iter *v = (Iter *)val_alloc(ITER_OBJ);
    v->iter = NULL;
    v->val = 0;
    v->data = val_reference(v1);
    v->next = next;
    v->len = iter_len;
    return v;
}

static MUST_CHECK Obj *and_(const Bytes *vv1, const Bytes *vv2, linepos_t epoint) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len1 : len2;
    if (sz == 0) return val_reference((neg1 && neg2) ? &inv_bytes->v : &null_bytes->v);
    vv = new_bytes2(sz);
    if (vv == NULL) return (Obj *)new_error_mem(epoint);
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    if (neg1) {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = ~v1[i] & v2[i];
        }
    } else {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] & ~v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        }
    }
    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 && neg2) ? (ssize_t)~sz : (ssize_t)sz;
    vv->data = v;
    return &vv->v;
}

static MUST_CHECK Obj *or_(const Bytes *vv1, const Bytes *vv2, linepos_t epoint) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len2 : len1;
    if (sz == 0) return val_reference((neg1 || neg2) ? &inv_bytes->v : &null_bytes->v);
    vv = new_bytes2(sz);
    if (vv == NULL) return (Obj *)new_error_mem(epoint);
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    if (neg1) {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & ~v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    } else {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = ~v1[i] & v2[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }

    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 || neg2) ? (ssize_t)~sz : (ssize_t)sz;
    vv->data = v;
    return &vv->v;
}

static MUST_CHECK Obj *xor_(const Bytes *vv1, const Bytes *vv2, linepos_t epoint) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = len1;
    if (sz == 0) return val_reference((neg1 != neg2) ? &inv_bytes->v : &null_bytes->v);
    vv = new_bytes2(sz);
    if (vv == NULL) return (Obj *)new_error_mem(epoint);
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 != neg2) ? (ssize_t)~sz : (ssize_t)sz;
    vv->data = v;
    return &vv->v;
}

static MUST_CHECK Obj *concat(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1, *v2 = (Bytes *)op->v2, *v;
    uint8_t *s;
    size_t ln, i, len1, len2;

    if (v1->len == 0) {
        return (Obj *)ref_bytes(v2);
    }
    if (v2->len == 0 || v2->len == ~(ssize_t)0) {
        return (Obj *)ref_bytes(v1);
    }
    len1 = byteslen(v1);
    len2 = byteslen(v2);
    ln = len1 + len2;
    if (ln < len2 || ln > SSIZE_MAX) goto failed; /* overflow */

    if (op->inplace == &v1->v) {
        s = extend_bytes(v1, ln);
        if (s == NULL) goto failed;
        v = ref_bytes(v1);
    } else {
        v = new_bytes2(ln);
        if (v == NULL) goto failed;
        s = v->data;
        memcpy(s, v1->data, len1);
    }
    if ((v2->len ^ v1->len) < 0) {
        for (i = 0; i < len2; i++) s[i + len1] = ~v2->data[i];
    } else memcpy(s + len1, v2->data, len2);
    v->len = (v1->len < 0) ? (ssize_t)~ln : (ssize_t)ln;
    return &v->v;
failed:
    return (Obj *)new_error_mem(op->epoint3);
}

static int icmp(Bytes *v1, Bytes *v2) {
    size_t len1 = byteslen(v1), len2 = byteslen(v2);
    int h = memcmp(v1->data, v2->data, (len1 < len2) ? len1 : len2);
    if (h != 0) return h;
    if (len1 < len2) return -1;
    return (len1 > len2) ? 1 : 0;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *v;
    Obj *tmp;
    switch (op->op->op) {
    case O_BANK:
        if (v1->len > 2) return (Obj *)bytes_from_u8(v1->data[2]);
        if (v1->len < ~2) return (Obj *)bytes_from_u8(~v1->data[2]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? ~0U : 0);
    case O_HIGHER:
        if (v1->len > 1) return (Obj *)bytes_from_u8(v1->data[1]);
        if (v1->len < ~1) return (Obj *)bytes_from_u8(~v1->data[1]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? ~0U : 0);
    case O_LOWER:
        if (v1->len > 0) return (Obj *)bytes_from_u8(v1->data[0]);
        if (v1->len < ~0) return (Obj *)bytes_from_u8(~v1->data[0]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? ~0U : 0);
    case O_HWORD:
        if (v1->len > 2) return (Obj *)bytes_from_u16(v1->data[1] + ((unsigned int)v1->data[2] << 8));
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1]);
        if (v1->len < ~2) return (Obj *)bytes_from_u16(~(v1->data[1] + ((unsigned int)v1->data[2] << 8)));
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~v1->data[1]);
        return (Obj *)bytes_from_u16((v1->len < 0) ? ~0U : 0);
    case O_WORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[0] + ((unsigned int)v1->data[1] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16(v1->data[0]);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[0] + ((unsigned int)v1->data[1] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~v1->data[0]);
        return (Obj *)bytes_from_u16((v1->len < 0) ? ~0U : 0);
    case O_BSWORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1] + ((unsigned int)v1->data[0] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16((unsigned int)v1->data[0] << 8);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[1] + ((unsigned int)v1->data[0] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~((unsigned int)v1->data[0] << 8));
        return (Obj *)bytes_from_u16((v1->len < 0) ? ~0U : 0);
    case O_POS: return val_reference(&v1->v);
    case O_INV:
        if (op->inplace != &v1->v) return invert(v1, op->epoint3);
        v1->len = ~v1->len;
        if (v1->data != v1->u.val) v1->u.s.hash = -1;
        return val_reference(&v1->v);
    case O_NEG:
        v = negate(v1, op->epoint3);
        if (v != NULL) return v;
        /* fall through */
    case O_STRING:
        tmp = int_from_bytes(v1, op->epoint);
        op->v1 = tmp;
        op->inplace = NULL;
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
    Bytes *v1 = (Bytes *)op->v1, *v2 = (Bytes *)op->v2;
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
            tmp = int_from_bytes(v1, op->epoint);
            tmp2 = int_from_bytes(v2, op->epoint2);
            op->v1 = tmp;
            op->v2 = tmp2;
            op->inplace = NULL;
            result = tmp->obj->calc2(op);
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_AND: return and_(v1, v2, op->epoint3);
    case O_OR: return or_(v1, v2, op->epoint3);
    case O_XOR: return xor_(v1, v2, op->epoint3);
    case O_LSHIFT:
    case O_RSHIFT:
        {
            Obj *tmp, *result;
            tmp = bits_from_bytes(v1, op->epoint);
            op->v1 = tmp;
            op->inplace = NULL;
            result = tmp->obj->calc2(op);
            val_destroy(tmp);
            return result;
        }
    case O_CMP:
        val = icmp(v1, v2);
        if (val < 0) return (Obj *)ref_int(minus1_value);
        return (Obj *)ref_int(int_value[(val > 0) ? 1 : 0]);
    case O_EQ: return truth_reference(icmp(v1, v2) == 0);
    case O_NE: return truth_reference(icmp(v1, v2) != 0);
    case O_MIN:
    case O_LT: return truth_reference(icmp(v1, v2) < 0);
    case O_LE: return truth_reference(icmp(v1, v2) <= 0);
    case O_MAX:
    case O_GT: return truth_reference(icmp(v1, v2) > 0);
    case O_GE: return truth_reference(icmp(v1, v2) >= 0);
    case O_CONCAT: return concat(op);
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            size_t len1 = byteslen(v1), len2 = byteslen(v2), i;
            if (len1 == 0) return (Obj *)ref_bool(true_value);
            if (len1 > len2) return (Obj *)ref_bool(false_value);
            c2 = v2->data;
            e = c2 + len2 - len1;
            if ((v1->len ^ v2->len) < 0) {
                for (;;) {
                    c = (uint8_t *)memchr(c2, ~v1->data[0], (size_t)(e - c2) + 1);
                    if (c == NULL) return (Obj *)ref_bool(false_value);
                    for (i = 1; i < len1; i++) {
                        if (c[i] != (0xff - v1->data[i])) break;
                    }
                    if (i == len1) return (Obj *)ref_bool(true_value);
                    c2 = c + 1;
                }
            } else {
                for (;;) {
                    c = (uint8_t *)memchr(c2, v1->data[0], (size_t)(e - c2) + 1);
                    if (c == NULL) return (Obj *)ref_bool(false_value);
                    if (memcmp(c, v1->data, len1) == 0) return (Obj *)ref_bool(true_value);
                    c2 = c + 1;
                }
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1, *v;
    uval_t rep;
    size_t len1 = byteslen(v1);
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return &err->v;

    if (len1 == 0 || rep == 0) return (Obj *)ref_bytes((v1->len < 0) ? inv_bytes : null_bytes);
    do {
        uint8_t *s;
        if (rep == 1) {
            return (Obj *)ref_bytes(v1);
        }
        if (len1 > SSIZE_MAX / rep) break; /* overflow */
        v = new_bytes2(len1 * rep);
        if (v == NULL) break;
        s = v->data;
        v->len = 0;
        while ((rep--) != 0) {
            memcpy(s + v->len, v1->data, len1);
            v->len += len1;
        }
        if (v1->len < 0) v->len = ~v->len;
        return &v->v;
    } while (false);
    return (Obj *)new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *slice(Obj *o1, oper_t op, size_t indx) {
    uint8_t *p2;
    size_t offs2, len1;
    size_t i;
    Bytes *v, *v1 = (Bytes *)o1;
    Obj *o2 = op->v2;
    Error *err;
    uint8_t inv = (v1->len < 0) ? (uint8_t)~0 : 0;
    Funcargs *args = (Funcargs *)o2;
    linepos_t epoint2;

    if (args->len < 1 || args->len > indx + 1) {
        err_msg_argnum(args->len, 1, indx + 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val[indx].val;
    epoint2 = &args->val[indx].epoint;

    len1 = byteslen(v1);

    if (o2->obj == LIST_OBJ) {
        iter_next_t iter_next;
        Iter *iter = o2->obj->getiter(o2);
        size_t len2 = iter->len(iter);

        if (len2 == 0) {
            val_destroy(&iter->v);
            return (Obj *)ref_bytes(null_bytes);
        }
        v = new_bytes2(len2);
        if (v == NULL) {
            val_destroy(&iter->v);
            goto failed;
        }
        p2 = v->data;
        iter_next = iter->next;
        for (i = 0; i < len2 && (o2 = iter_next(iter)) != NULL; i++) {
            err = indexoffs(o2, len1, &offs2, epoint2);
            if (err != NULL) {
                val_destroy(&v->v);
                val_destroy(&iter->v);
                return &err->v;
            }
            p2[i] = v1->data[offs2] ^ inv;
        }
        val_destroy(&iter->v);
        if (i > SSIZE_MAX) goto failed2; /* overflow */
        v->len = (ssize_t)i;
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        uval_t length;
        ival_t offs, end, step;

        err = (Error *)sliceparams((Colonlist *)o2, len1, &length, &offs, &end, &step, epoint2);
        if (err != NULL) return &err->v;

        if (length == 0) {
            return (Obj *)ref_bytes(null_bytes);
        }
        if (step == 1 && inv == 0) {
            if (length == byteslen(v1)) {
                return (Obj *)ref_bytes(v1); /* original bytes */
            }
            if (length == 1) return (Obj *)bytes_from_u8(v1->data[offs]);
            v = new_bytes2(length);
            if (v == NULL) goto failed;
            memcpy(v->data, v1->data + offs, length);
        } else {
            v = new_bytes2(length);
            if (v == NULL) goto failed;
            p2 = v->data;
            for (i = 0; i < length; i++) {
                p2[i] = v1->data[offs] ^ inv;
                offs += step;
            }
        }
        v->len = (ssize_t)length;
        return &v->v;
    }
    err = indexoffs(o2, len1, &offs2, epoint2);
    if (err != NULL) return &err->v;
    return (Obj *)bytes_from_u8(v1->data[offs2] ^ inv);
failed2:
    val_destroy(&v->v);
failed:
    return (Obj *)new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *o2 = op->v2;
    Obj *tmp;

    if (op->op == &o_X) {
        if (o2 == &none_value->v || o2->obj == ERROR_OBJ) return val_reference(o2);
        return repeat(op);
    }
    if (op->op == &o_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? o2 : &v1->v);
    }
    if (op->op == &o_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? &v1->v : o2);
    }
    switch (o2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
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
            case O_RSHIFT: tmp = bits_from_bytes(v1, op->epoint); break;
            default: tmp = int_from_bytes(v1, op->epoint);
            }
            op->v1 = tmp;
            op->inplace = NULL;
            result = tmp->obj->calc2(op);
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_GAP:
    case T_REGISTER:
    case T_DICT:
        if (op->op != &o_MEMBER) {
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
    Bytes *v2 = (Bytes *)op->v2;
    Obj *o1 = op->v1;
    Obj *tmp;
    switch (o1->obj->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
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
            case O_XOR: tmp = bits_from_bytes(v2, op->epoint2); break;
            default: tmp = int_from_bytes(v2, op->epoint2);
            }
            op->v2 = tmp;
            op->inplace = NULL;
            result = o1->obj->calc2(op);
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_IN) {
            return o1->obj->calc2(op);
        }
        break;
    default: break;
    }
    return obj_oper_error(op);
}

void bytesobj_init(void) {
    new_type(&obj, T_BYTES, "bytes", sizeof(Bytes));
    obj.create = create;
    obj.destroy = destroy;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.str = str;
    obj.ival = ival;
    obj.uval = uval;
    obj.uval2 = uval2;
    obj.sign = sign;
    obj.function = function;
    obj.len = len;
    obj.getiter = getiter;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.slice = slice;

    null_bytes = new_bytes(0);
    null_bytes->len = 0;
    null_bytes->data[0] = 0;
    inv_bytes = new_bytes(0);
    inv_bytes->len = ~0;
    inv_bytes->data[0] = 0;
}

void bytesobj_names(void) {
    new_builtin("bytes", val_reference(&BYTES_OBJ->v));
}

void bytesobj_destroy(void) {
    int i;
#ifdef DEBUG
    if (null_bytes->v.refcount != 1) fprintf(stderr, "bytes %" PRIuSIZE "\n", null_bytes->v.refcount - 1);
    if (inv_bytes->v.refcount != 1) fprintf(stderr, "invbytes %" PRIuSIZE "\n", inv_bytes->v.refcount - 1);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] && bytes_value[i]->v.refcount != 1) {
            fprintf(stderr, "bytes[%d] %" PRIuSIZE "\n", i, bytes_value[i]->v.refcount - 1);
        }
    }
#endif

    val_destroy(&null_bytes->v);
    val_destroy(&inv_bytes->v);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] != NULL) val_destroy(&bytes_value[i]->v);
    }
}
