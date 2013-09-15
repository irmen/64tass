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
#include <math.h>
#include "values.h"
#include "bitsobj.h"
#include "eval.h"
#include "isnprintf.h"
#include "misc.h"

#include "intobj.h"
#include "boolobj.h"
#include "strobj.h"
#include "addressobj.h"
#include "listobj.h"

static struct obj_s obj;

obj_t BITS_OBJ = &obj;

static void destroy(struct value_s *v1) {
    if (v1->u.bits.val != v1->u.bits.data) free(v1->u.bits.data);
}

static bdigit_t *bnew(struct value_s *v, size_t len) {
    if (len > 2) {
        bdigit_t *s = (bdigit_t *)malloc(len * sizeof(bdigit_t));
        if (!s || len > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        return s; 
    }
    return v->u.bits.val;
}

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->refcount = 1;
    v->u.bits.bits = v1->u.bits.bits;
    v->u.bits.inv = v1->u.bits.inv;
    v->u.bits.len = v1->u.bits.len;
    if (v1->u.bits.len) {
        v->u.bits.data = bnew(v, v->u.bits.len);
        memcpy(v->u.bits.data, v1->u.bits.data, v->u.bits.len * sizeof(bdigit_t));
    } else {
        v->u.bits.val[0] = 0;
        v->u.bits.data = v->u.bits.val;
    }
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = BITS_OBJ;
    v->refcount = 1;
    v->u.bits.bits = v1->u.bits.bits;
    v->u.bits.inv = v1->u.bits.inv;
    v->u.bits.len = v1->u.bits.len;
    if (v1->u.bits.len) {
        if (v1->u.bits.data == v1->u.bits.val) {
            v->u.bits.data = v->u.bits.val;
            memcpy(v->u.bits.data, v1->u.bits.data, v->u.bits.len * sizeof(bdigit_t));
        } else v->u.bits.data = v1->u.bits.data;
    } else {
        v->u.bits.val[0] = 0;
        v->u.bits.data = v->u.bits.val;
    }
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    if (v2->obj != BITS_OBJ || v1->u.bits.len != v2->u.bits.len || v1->u.bits.bits != v2->u.bits.bits || v1->u.bits.inv != v2->u.bits.inv) return 0;
    return !memcmp(v1->u.bits.data, v2->u.bits.data, v1->u.bits.len * sizeof(bdigit_t));
}

static int truth(const struct value_s *v1) {
    if (v1->u.bits.len) return 1;
    return v1->u.bits.inv;
}

static void repr(const struct value_s *v1, struct value_s *v) {
    size_t len, i, len2;
    uint8_t *s;

    len2 = v1->u.bits.bits;
    if (len2 & 3) {
        len = 1 + v1->u.bits.inv;
        len += len2;
        s = (uint8_t *)malloc(len);
        if (!s || len < len2) err_msg_out_of_memory(); /* overflow */

        len = 0;
        if (v1->u.bits.inv) s[len++] = '~';
        s[len++] = '%';
        for (i = len2; i--;) {
            size_t j = i / 8 / sizeof(bdigit_t);
            s[len++] = (j >= v1->u.bits.len) ? 0x30 : 0x30 | ((v1->u.bits.data[j] >> (i & (8 * sizeof(bdigit_t) - 1))) & 1);
        }
    } else {
        static const char *hex = "0123456789abcdef";
        len2 /= 4;
        len = 1 + v1->u.bits.inv;
        len += len2;
        s = (uint8_t *)malloc(len);
        if (!s || len < len2) err_msg_out_of_memory(); /* overflow */

        len = 0;
        if (v1->u.bits.inv) s[len++] = '~';
        s[len++] = '$';
        for (i = len2; i--;) {
            size_t j = i / 2 / sizeof(bdigit_t);
            s[len++] = (j >= v1->u.bits.len) ? 0x30 : (uint8_t)hex[(v1->u.bits.data[j] >> ((i & (2 * sizeof(bdigit_t) - 1)) * 4)) & 15];
        }
    }
    if (v == v1) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.len = len;
    v->u.str.chars = len;
    v->u.str.data = s;
}

static int hash(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    int ret;

    switch (v1->u.bits.len) {
    case 0: return (-v1->u.bits.inv) & ((~(unsigned int)0) >> 1);
    case 1: return (v1->u.bits.inv ? ~v1->u.bits.data[0] : v1->u.bits.data[0]) & ((~(unsigned int)0) >> 1);
    case 2: return (v1->u.bits.inv ? ~(v1->u.bits.data[0] | (v1->u.bits.data[1] << 16)) : (v1->u.bits.data[0] | (v1->u.bits.data[1] << 16))) & ((~(unsigned int)0) >> 1);
    }
    int_from_bits(&tmp, v1);
    ret = tmp.obj->hash(&tmp, v, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    int_from_bits(&tmp, v1);
    ret = tmp.obj->ival(&tmp, v, iv, bits, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    switch (v1->u.bits.len) {
    case 2: *uv = v1->u.bits.data[0] | (v1->u.bits.data[1] << (8 * sizeof(bdigit_t)));
            if (bits < 2 * 8 * (int)sizeof(bdigit_t) && *uv >> bits) break;
            if (v1->u.bits.inv) *uv = ~*uv;
            return 0;
    case 1: *uv = v1->u.bits.data[0];
            if (bits < 8 * (int)sizeof(bdigit_t) && *uv >> bits) break;
            if (v1->u.bits.inv) *uv = ~*uv;
            return 0;
    case 0: *uv = v1->u.bits.inv ? ~(uval_t)0 : 0; return 0;
    default: break;
    }
    *uv = 0;
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR_____CANT_UVAL;
    v->u.error.u.bits = bits;
    v->u.error.epoint = *epoint;
    return 1;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    size_t i, len1 = v1->u.bits.len;
    double d = -v1->u.bits.inv;
    for (i = 0; i < len1; i++) {
        if (v1->u.bits.inv) d -= ldexp((double)v1->u.bits.data[i], i * 8 * sizeof(bdigit_t));
        else d += ldexp((double)v1->u.bits.data[i], i * 8 * sizeof(bdigit_t));
        if (d == HUGE_VAL) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_____CANT_REAL;
            v->u.error.epoint = *epoint;
            return 1;
        }
    }
    *r = d;
    return 0;
}

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *UNUSED(v), int *s, linepos_t UNUSED(epoint)) {
    if (!v1->u.bits.len) *s = 0;
    else if (v1->u.bits.inv) *s = -1;
    else *s = 1;
    return 0;
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    int_from_bits(&tmp, v1);
    tmp.obj->abs(&tmp, v, epoint);
    tmp.obj->destroy(&tmp);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    struct value_s tmp;
    int_from_bits(&tmp, v1);
    if (v == v1) v->obj->destroy(v);
    tmp.obj->copy_temp(&tmp, v);
}

static int MUST_CHECK len(const struct value_s *v1, struct value_s *UNUSED(v), uval_t *uv, linepos_t UNUSED(epoint)) {
    *uv = v1->u.bits.bits;
    return 0;
}

static void bits_from_bool(struct value_s *v, int i) {
    v->obj = BITS_OBJ;
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 1;
}

void bits_from_bools(struct value_s *v, int i, int j) {
    v->obj = BITS_OBJ;
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = (i << 1) | j;
    v->u.bits.len = (v->u.bits.val[0] != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 2;
}

void bits_from_u8(struct value_s *v, uint8_t i) {
    v->obj = BITS_OBJ;
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 8;
}

void bits_from_u16(struct value_s *v, uint16_t i) {
    v->obj = BITS_OBJ;
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 16;
}

static void bits_from_u24(struct value_s *v, int i) {
    v->obj = BITS_OBJ;
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.val[1] = i >> 16;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 24;
}

size_t bits_from_hexstr(struct value_s *v, const uint8_t *s) {
    size_t i = 0, j = 0, sz, l;
    bdigit_t uv = 0;
    int bits = 0;
    bdigit_t *d;
    v->obj = BITS_OBJ;

    while ((s[i] ^ 0x30) < 10 || (uint8_t)((s[i] | 0x20) - 0x61) < 6) i++;
    if (!i) {
        v->u.bits.data = v->u.bits.val;
        v->u.bits.val[0] = 0;
        v->u.bits.len = 0;
        v->u.bits.inv = 0;
        v->u.bits.bits = 0;
        return 0;
    }
    l = i;
    sz = i / 2 / sizeof(bdigit_t);
    if (i % (2 * sizeof(bdigit_t))) sz++;

    d = bnew(v, sz);

    while (i) {
        i--;
        if (s[i] < 0x40) uv |= (s[i] & 15) << bits;
        else uv |= ((s[i] & 7) + 9) << bits;
        if (bits == (8 * sizeof(bdigit_t) - 4)) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 4;
    }
    if (bits) d[j] = uv;

    while (sz && !d[sz - 1]) sz--;
    if (sz <= 2 && v->u.bits.val != d) {
        memcpy(v->u.bits.val, d, sz);
        free(d);
        d = v->u.bits.val;
    }
    v->u.bits.data = d;
    v->u.bits.len = sz;
    v->u.bits.inv = 0;
    v->u.bits.bits = j * sizeof(bdigit_t) * 8 + bits;
    return l;
}

size_t bits_from_binstr(struct value_s *v, const uint8_t *s) {
    size_t i = 0, j = 0, sz, l;
    uval_t uv = 0;
    int bits = 0;
    bdigit_t *d;
    v->obj = BITS_OBJ;

    while ((s[i] & 0xfe) == 0x30) i++;
    if (!i) {
        v->u.bits.data = v->u.bits.val;
        v->u.bits.val[0] = 0;
        v->u.bits.len = 0;
        v->u.bits.inv = 0;
        v->u.bits.bits = 0;
        return 0;
    }
    l = i;
    sz = i / 8 / sizeof(bdigit_t);
    if (i % (8 * sizeof(bdigit_t))) sz++;

    d = bnew(v, sz);

    while (i) {
        i--;
        if (s[i] == 0x31) uv |= 1 << bits;
        if (bits == (8 * sizeof(bdigit_t) - 1)) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits++;
    }
    if (bits) d[j] = uv;

    while (sz && !d[sz - 1]) sz--;
    if (sz <= 2 && v->u.bits.val != d) {
        memcpy(v->u.bits.val, d, sz);
        free(d);
        d = v->u.bits.val;
    }
    v->u.bits.data = d;
    v->u.bits.len = sz;
    v->u.bits.inv = 0;
    v->u.bits.bits = j * sizeof(bdigit_t) * 8 + bits;
    return l;
}

int bits_from_str(struct value_s *v, const struct value_s *v1) {
    uint16_t ch;

    if (actual_encoding) {
        uval_t uv = 0;
        unsigned int bits = 0;
        size_t i = 0, j = 0, sz;
        bdigit_t *d, tmp[2];

        if (!v1->u.str.len) {
            v->obj = BITS_OBJ;
            v->u.bits.data = v->u.bits.val;
            v->u.bits.val[0] = 0;
            v->u.bits.len = 0;
            v->u.bits.inv = 0;
            v->u.bits.bits = 0;
            return 0;
        }

        sz = v1->u.str.len / sizeof(bdigit_t);
        if (v1->u.str.len % sizeof(bdigit_t)) sz++;
        if (sz > 2) {
            d = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
            if (!d || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        } else d = tmp;

        while (v1->u.str.len > i) {
            ch = petascii(&i, v1);
            if (ch > 255) return 1;

            uv |= (uint8_t)ch << bits;
            bits += 8;
            if (bits >= 8 * sizeof(bdigit_t)) {
                if (j >= sz) err_msg_out_of_memory();
                d[j++] = uv;
                bits = uv = 0;
            }
        }
        if (bits) {
            if (j >= sz) err_msg_out_of_memory();
            d[j] = uv;
            sz = j + 1;
        } else sz = j;

        while (sz && !d[sz - 1]) sz--;
        if (v == v1) v->obj->destroy(v);
        if (sz <= 2) {
            memcpy(v->u.bits.val, d, sz * sizeof(bdigit_t));
            if (tmp != d) free(d);
            d = v->u.bits.val;
        }
        v->obj = BITS_OBJ;
        v->u.bits.data = d;
        v->u.bits.len = sz;
        v->u.bits.inv = 0;
        v->u.bits.bits = j * sizeof(bdigit_t) * 8 + bits;
        return 0;
    } 
    if (v1->u.str.len == 1) {
        uint32_t ch2 = v1->u.str.data[0];
        if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
        if (v == v1) v->obj->destroy(v);
        bits_from_u24(v, ch2);
        return 0;
    } 
    return 1;
}

void bits_from_bytes(struct value_s *v, const struct value_s *v1) {
    unsigned int bits = 0;
    size_t i = 0, j = 0, sz;
    bdigit_t *d, tmp[2], uv = 0;

    if (!v1->u.bytes.len) {
        v->obj = BITS_OBJ;
        v->u.bits.data = v->u.bits.val;
        v->u.bits.val[0] = 0;
        v->u.bits.len = 0;
        v->u.bits.inv = 0;
        v->u.bits.bits = 0;
        return;
    }

    sz = v1->u.bytes.len / sizeof(bdigit_t);
    if (v1->u.bytes.len % sizeof(bdigit_t)) sz++;
    if (sz > 2) {
        d = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
        if (!d || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
    } else d = tmp;

    while (v1->u.bytes.len > i) {
        uv |= v1->u.bytes.data[i++] << bits;
        bits += 8;
        if (bits >= 8 * sizeof(bdigit_t)) {
            d[j++] = uv;
            bits = uv = 0;
        }
    }
    if (bits) {
        d[j] = uv;
        sz = j + 1;
    } else sz = j;

    while (sz && !d[sz - 1]) sz--;
    if (v == v1) v->obj->destroy(v);
    if (sz <= 2) {
        memcpy(v->u.bits.val, d, sz * sizeof(bdigit_t));
        if (tmp != d) free(d);
        d = v->u.bits.val;
    }
    v->obj = BITS_OBJ;
    v->u.bits.data = d;
    v->u.bits.len = sz;
    v->u.bits.inv = 0;
    v->u.bits.bits = j * sizeof(bdigit_t) * 8 + bits;
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v, *v2;
    struct value_s tmp;
    uval_t uv;
    enum atype_e am;
    switch (op->op->u.oper.op) {
    case O_BANK:
        uv = v1->u.bits.len > 1 ? v1->u.bits.data[1] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        if (v == v1) destroy(v);
        bits_from_u8(v, uv);
        return;
    case O_HIGHER:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        if (v == v1) destroy(v);
        bits_from_u8(v, uv >> 8);
        return;
    case O_LOWER:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        if (v == v1) destroy(v);
        bits_from_u8(v, uv);
        return;
    case O_HWORD:
        uv = v1->u.bits.len > 1 ? (v1->u.bits.data[1] << 16) : 0;
        uv |= v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        bits_from_u16(v, uv >> 8);
        return;
    case O_WORD:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        if (v == v1) destroy(v);
        bits_from_u16(v, uv);
        return;
    case O_BSWORD:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        if (v == v1) destroy(v);
        bits_from_u16(v, (uint8_t)(uv >> 8) | (uint16_t)(uv << 8));
        return;
    case O_COMMAS: am =  A_SR; goto addr;
    case O_COMMAR: am =  A_RR; goto addr;
    case O_COMMAZ: am =  A_ZR; goto addr;
    case O_COMMAY: am =  A_YR; goto addr;
    case O_COMMAX: am =  A_XR; goto addr;
    case O_COMMAD: am = A_DR; goto addr;
    case O_COMMAB: am = A_BR; goto addr;
    case O_COMMAK: am = A_KR; goto addr;
    case O_HASH: am = A_IMMEDIATE;
    addr:
        if (v == v1) {
            v2 = val_alloc();
            copy_temp(v1, v2);
        } else v2 = val_reference(v1);
        v->obj = ADDRESS_OBJ;
        v->u.addr.val = v2;
        v->u.addr.type = am;
        return;
    case O_INV:
        if (v != v1) copy(v1, v);
        v->u.bits.inv = !v->u.bits.inv;
        return;
    case O_NEG:
    case O_POS:
    case O_STRING:
        int_from_bits(&tmp, v1);
        if (v == v1) destroy(v);
        op->v1 = &tmp;
        tmp.refcount = 0;
        tmp.obj->calc1(op);
        op->v1 = v1;
        tmp.obj->destroy(&tmp);
        return;
    case O_LNOT:
        if (v1 == v) destroy(v);
        bool_from_int(v, !truth(v1)); 
        return;
    default:
        break;
    }
    obj_oper_error(op);
}

static void and_(const struct value_s *vv1, const struct value_s *vv2, struct value_s *vv) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    bdigit_t c;
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        c = neg1 ? ~vv1->u.bits.val[0] : vv1->u.bits.val[0];
        c &= neg2 ? ~vv2->u.bits.val[0] : vv2->u.bits.val[0];
        neg1 &= neg2;
        if (neg1) c = ~c;
        v = vv->u.bits.val;
        vv->obj = BITS_OBJ; 
        vv->u.bits.data = v;
        vv->u.bits.bits = blen1 > blen2 ? blen1 : blen2;
        vv->u.bits.inv = neg1;
        v[0] = c;
        vv->u.bits.len = (c != 0);
        return;
    }
    if (len1 < len2) {
        const struct value_s *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
    if (blen1 < blen2) blen1 = blen2;

    sz = neg2 ? len1 : len2;
    v = bnew(vv, sz);

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
    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && v != vv->u.bits.val) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->obj = BITS_OBJ; 
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.inv = neg1 & neg2;
    vv->u.bits.bits = blen1;
}

static void or_(const struct value_s *vv1, const struct value_s *vv2, struct value_s *vv) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    bdigit_t c;
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        c = neg1 ? ~vv1->u.bits.val[0] : vv1->u.bits.val[0];
        c |= neg2 ? ~vv2->u.bits.val[0] : vv2->u.bits.val[0];
        neg1 |= neg2;
        if (neg1) c = ~c;
        v = vv->u.bits.val;
        vv->obj = BITS_OBJ; 
        vv->u.bits.data = v;
        vv->u.bits.bits = blen1 > blen2 ? blen1 : blen2;
        vv->u.bits.inv = neg1;
        v[0] = c;
        vv->u.bits.len = (c != 0);
        return;
    }
    if (len1 < len2) {
        const struct value_s *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
    if (blen1 < blen2) blen1 = blen2;

    sz = neg2 ? len2 : len1;
    v = bnew(vv, sz);

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
    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && vv->u.bits.val != v) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->obj = BITS_OBJ; 
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.inv = neg1 | neg2;
    vv->u.bits.bits = blen1;
}

static void xor_(const struct value_s *vv1, const struct value_s *vv2, struct value_s *vv) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    bdigit_t c;
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        c = vv1->u.bits.val[0] ^ vv2->u.bits.val[0];
        neg1 ^= neg2;
        v = vv->u.bits.val;
        vv->obj = BITS_OBJ; 
        vv->u.bits.data = v;
        vv->u.bits.bits = blen1 > blen2 ? blen1 : blen2;
        vv->u.bits.inv = neg1;
        v[0] = c;
        vv->u.bits.len = (c != 0);
        return;
    }
    if (len1 < len2) {
        const struct value_s *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    blen1 = vv1->u.bits.bits; blen2 = vv2->u.bits.bits;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
    if (blen1 < blen2) blen1 = blen2;

    sz = len1;
    v = bnew(vv, sz);

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];
    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && vv->u.bits.val != v) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->obj = BITS_OBJ; 
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.inv = neg1 ^ neg2;
    vv->u.bits.bits = blen1;
}

static void concat(const struct value_s *vv1, const struct value_s *vv2, struct value_s *vv) {
    size_t blen = vv1->u.bits.bits + vv2->u.bits.bits;
    size_t sz, bits, i, j, rbits;
    bdigit_t *v1, *v, tmp[2], uv;
    bdigit_t inv = -vv2->u.bits.inv;

    if (blen < vv2->u.bits.bits) err_msg_out_of_memory(); /* overflow */
    sz = blen / 8 / sizeof(bdigit_t);
    if (blen % (8 * sizeof(bdigit_t))) sz++;
    if (sz > 2) {
        v = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
        if (!v || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
    } else v = tmp;

    v1 = vv2->u.bits.data;
    rbits = vv2->u.bits.bits / 8 / sizeof(bdigit_t);
    bits = vv2->u.bits.bits & (8 * sizeof(bdigit_t) - 1);
    for (i = 0; i < rbits && i < vv2->u.bits.len; i++) v[i] = v1[i] ^ inv;
    for (; i < rbits; i++) v[i] = inv;
    if (i < vv2->u.bits.len) uv = v1[i] ^ inv; else uv = inv;
    if (inv) uv &= (1 << bits) - 1;
    inv = vv1->u.bits.inv;

    rbits = vv1->u.bits.bits / 8 / sizeof(bdigit_t);
    v1 = vv1->u.bits.data;

    if (bits) {
        for (j = 0; j < vv1->u.bits.len; j++) {
            v[i++] = uv | (v1[j] << bits);
            uv = v1[j] >> (8 * sizeof(bdigit_t) - bits);
        }
        if (j < rbits) { v[i++] = uv; uv = 0; j++;}
        for (; j < rbits; j++) v[i++] = 0;
        if (i < sz) v[i] = uv;
    } else {
        for (j = 0; j < vv1->u.bits.len; j++) v[i++] = v1[j];
        for (; j < rbits; j++) v[i++] = 0;
    }

    while (sz && !v[sz - 1]) sz--;
    if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
    if (sz <= 2) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        if (tmp != v) free(v);
        v = vv->u.bits.val;
    }
    vv->obj = BITS_OBJ; 
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.bits = blen;
    vv->u.bits.inv = inv;
}

static void lshift(const struct value_s *vv1, const struct value_s *vv2, size_t s, struct value_s *vv) {
    size_t i, sz, bits, len1, word;
    int bit, inv;
    bdigit_t *v1, *v;

    word = s / 8 / sizeof(bdigit_t);
    bit = s % (8 * sizeof(bdigit_t));
    v1 = vv1->u.bits.data;
    len1 = vv1->u.bits.len;
    bits = vv1->u.bits.bits + s;
    if (bits < s) err_msg_out_of_memory(); /* overflow */
    inv = vv1->u.bits.inv;
    sz = word + (bit > 0);
    sz += len1;
    if (sz < len1) err_msg_out_of_memory(); /* overflow */
    v = bnew(vv, sz);
    v += word;
    if (bit) {
        v[len1] = 0;
        for (i = len1; i--;) {
            v[i + 1] |= v1[i] >> (8 * sizeof(bdigit_t) - bit);
            v[i] = v1[i] << bit;
        }
    } else if (len1) memmove(v, v1, len1 * sizeof(bdigit_t));
    if (word) {
        v -= word;
        memset(v, 0, word * sizeof(bdigit_t));
    }

    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && v != vv->u.bits.val) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.bits = bits;
    vv->u.bits.inv = inv;
}

static void rshift(const struct value_s *vv1, const struct value_s *vv2, uval_t s, struct value_s *vv) {
    size_t i, sz, bits, word;
    int bit, inv;
    bdigit_t *v1, *v;

    word = s / 8 / sizeof(bdigit_t);
    bit = s - word * 8 * sizeof(bdigit_t);
    bits = vv1->u.bits.bits;
    sz = vv1->u.bits.len - word;
    if (sz <= 0 || bits <= s) {
        if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
        vv->u.bits.data = vv->u.bits.val;
        vv->u.bits.val[0] = 0;
        vv->u.bits.len = 0;
        vv->u.bits.inv = 0;
        vv->u.bits.bits = 0;
        return;
    }
    bits -= s;
    v = bnew(vv, sz);
    v1 = vv1->u.bits.data + word;
    if (bit) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= v1[i + 1] << (8 * sizeof(bdigit_t) - bit);
        }
        v[i] = v1[i] >> bit;
    } else if (sz) memmove(v, v1, sz * sizeof(bdigit_t));

    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && v != vv->u.bits.val) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    inv = vv1->u.bits.inv;
    if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.bits = bits;
    vv->u.bits.inv = inv;
}

static void repeat(oper_t op, uval_t rep) {
    struct value_s *vv1 = op->v1, *vv = op->v;
    bdigit_t *v, *v1;
    bdigit_t uv;
    size_t sz, i, j, rbits, abits, bits;
    size_t blen = vv1->u.bits.bits;

    if (!rep || !blen) {
        if (vv1 == vv) destroy(vv);
        copy(&null_bits, vv); return;
    }
    if (rep == 1) {
        if (vv1 != vv) copy(vv1, vv);
        return;
    }

    if ((size_t)~0 / rep < blen) err_msg_out_of_memory(); /* overflow */
    blen *= rep;
    sz = blen / 8 / sizeof(bdigit_t);
    if (blen % (8 * sizeof(bdigit_t))) sz++;

    v = bnew(vv, sz);
    v1 = vv1->u.bits.data;

    i = 0;
    bits = 0;
    uv = 0;
    rbits = vv1->u.bits.bits / 8 / sizeof(bdigit_t);
    abits = vv1->u.bits.bits % (8 * sizeof(bdigit_t));
    while (rep--) {
        if (bits) {
            for (j = 0; j < rbits && j < vv1->u.bits.len; j++) {
                v[i++] = uv | (v1[j] << bits);
                uv = (v1[j] >> (8 * sizeof(bdigit_t) - bits));
            }
            uv |= v1[j] << bits;
            if (j < rbits) { v[i++] = uv; uv = 0; j++;}
            for (; j < rbits; j++) v[i++] = 0;
            bits += abits;
            if (bits >= 8 * sizeof(bdigit_t)) {
                v[i++] = uv;
                bits -= 8 * sizeof(bdigit_t);
                if (bits) uv = v1[j] >> (abits - bits);
                else uv = 0;
            }
        } else {
            for (j = 0; j < rbits && j < vv1->u.bits.len; j++) v[i++] = v1[j];
            for (; j < rbits; j++) v[i++] = 0;
            uv = v1[j];
            bits = abits;
        }
    }
    if (i < sz) v[i] = uv;

    while (sz && !v[sz - 1]) sz--;
    if (sz <= 2 && vv->u.bits.val != v) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        free(v);
        v = vv->u.bits.val;
    }
    if (vv == vv1 || vv == op->v2) destroy(vv);
    vv->obj = BITS_OBJ; 
    vv->u.bits.data = v;
    vv->u.bits.len = sz;
    vv->u.bits.bits = blen;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    ival_t shift;
    switch (v2->obj->type) {
    case T_BOOL:
        bits_from_bool(&tmp, op->v2->u.boolean);
        op->v2 = &tmp;
        tmp.refcount = 0;
        calc2(op);
        op->v2 = v2;
        return;
    case T_BITS: 
        switch (op->op->u.oper.op) {
        case O_AND: return and_(v1, v2, v);
        case O_OR: return or_(v1, v2, v);
        case O_XOR: return xor_(v1, v2, v);
        case O_CONCAT: return concat(v1, v2, v);
        default: break;
        }
        /* fall through */
    case T_INT:
        switch (op->op->u.oper.op) {
        case O_LSHIFT:
            if (v2->obj->ival(v2, &tmp, &shift, 8*sizeof(ival_t), &op->epoint2)) {
                if (v1 == v || v2 == v) v->obj->destroy(v);
                tmp.obj->copy_temp(&tmp, v);
                return;
            }
            v->obj = BITS_OBJ;
            if (shift < 0) rshift(v1, v2, -shift, v);
            else lshift(v1, v2, shift, v);
            return;
        case O_RSHIFT:
            if (v2->obj->ival(v2, &tmp, &shift, 8*sizeof(ival_t), &op->epoint2)) {
                if (v1 == v || v2 == v) v->obj->destroy(v);
                tmp.obj->copy_temp(&tmp, v);
                return;
            }
            v->obj = BITS_OBJ;
            if (shift < 0) lshift(v1, v2, -shift, v);
            else rshift(v1, v2, shift, v);
            return;
        default: break;
        }
        int_from_bits(&tmp, v1);
        if (v1 == v) v->obj->destroy(v);
        op->v1 = &tmp;
        tmp.refcount = 0;
        tmp.obj->calc2(op);
        op->v1 = v1;
        tmp.obj->destroy(&tmp);
        return;
    case T_TUPLE:
    case T_LIST: 
        if (op->op == &o_MOD) {
            isnprintf(v1, v2, v, &op->epoint, &op->epoint2); return;
        }
    default: 
        if (op->op != &o_MEMBER) {
            op->v2->obj->rcalc2(op); return;
        }
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v1->obj->type) {
    case T_BOOL:
        bits_from_bool(&tmp, op->v1->u.boolean);
        op->v1 = &tmp;
        tmp.refcount = 0;
        rcalc2(op);
        op->v1 = v1;
        return;
    case T_BITS: 
        switch (op->op->u.oper.op) {
        case O_AND: return and_(v1, v2, v);
        case O_OR: return or_(v1, v2, v);
        case O_XOR: return xor_(v1, v2, v);
        case O_CONCAT: return concat(v1, v2, v);
        default: break;
        }
        /* fall through */
    case T_INT:
        int_from_bits(&tmp, v2);
        if (v2 == v) v->obj->destroy(v);
        op->v2 = &tmp;
        tmp.refcount = 0;
        tmp.obj->rcalc2(op);
        op->v2 = v2;
        tmp.obj->destroy(&tmp);
        return;
    default:
        if (op->op != &o_IN) {
            v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op); return;
}

static void iindex(oper_t op) {
    size_t ln, sz;
    ival_t offs;
    size_t i, j, o;
    struct value_s *vv1 = op->v1, *vv2 = op->v2, *vv = op->v;
    bdigit_t *v;
    bdigit_t uv, tmp[2];
    bdigit_t inv = -vv1->u.bits.inv;
    int bits;

    ln = vv1->u.bits.bits;

    if (vv2->obj == TUPLE_OBJ || vv2->obj == LIST_OBJ) {
        if (!vv2->u.list.len) {
            if (vv1 == vv) destroy(vv);
            copy(&null_bits, vv);return;
        }
        sz = vv2->u.list.len / 8 / sizeof(bdigit_t);
        if (vv2->u.list.len % (8 * sizeof(bdigit_t))) sz++;

        if (sz > 2) {
            v = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
            if (!v || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        } else v = tmp;

        uv = inv;
        bits = j = 0;
        for (i = 0; i < vv2->u.list.len; i++) {
            offs = indexoffs(vv2->u.list.data[i], ln);
            if (offs < 0) {
                if (tmp != v) free(v);
                if (vv1 == vv) destroy(vv);
                vv->obj = ERROR_OBJ;
                vv->u.error.num = ERROR___INDEX_RANGE;
                vv->u.error.epoint = op->epoint2;
                return;
            }
            o = offs / 8 / sizeof(bdigit_t);
            if (o < vv1->u.bits.len && ((vv1->u.bits.data[o] >> (offs & (8 * sizeof(bdigit_t) - 1))) & 1)) {
                uv ^= 1 << bits;
            }
            bits++;
            if (bits == 8 * sizeof(bdigit_t)) {
                v[j++] = uv;
                uv = inv;
                bits = 0;
            }
        }
        if (bits) v[j] = uv & ((1 << bits) - 1);

        while (sz && !v[sz - 1]) sz--;
        if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
        if (sz <= 2) {
            memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
            if (tmp != v) free(v);
            v = vv->u.bits.val;
        }
        vv->obj = BITS_OBJ;
        vv->u.bits.bits = vv2->u.list.len;
        vv->u.bits.len = sz;
        vv->u.bits.inv = 0;
        vv->u.bits.data = v;
        return;
    }
    offs = indexoffs(vv2, ln);
    if (offs < 0) {
        if (vv1 == vv) destroy(vv);
        vv->obj = ERROR_OBJ;
        vv->u.error.num = ERROR___INDEX_RANGE;
        vv->u.error.epoint = op->epoint2;
        return;
    }

    uv = inv;
    o = offs / 8 / sizeof(bdigit_t);
    if (o < vv1->u.bits.len && ((vv1->u.bits.data[o] >> (offs & (8 * sizeof(bdigit_t) - 1))) & 1)) {
        uv ^= 1;
    }
    if (vv == vv1 || vv == vv2) vv->obj->destroy(vv);
    bits_from_bool(vv, uv & 1);
}

static void slice(struct value_s *vv1, ival_t offs, ival_t end, ival_t step, struct value_s *vv, linepos_t UNUSED(epoint)) {
    size_t ln, bo, wo, bl, wl, i, sz;
    bdigit_t uv, tmp[2];
    bdigit_t *v, *v1;
    bdigit_t inv = -vv1->u.bits.inv;
    int bits;

    if (step > 0) {
        if (offs > end) offs = end;
        ln = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        ln = (offs - end - step - 1) / -step;
    }
    if (!ln) {
        if (vv1 == vv) destroy(vv);
        copy(&null_bits, vv);return;
    }
    if (step == 1) {
        if (ln == vv1->u.bits.len) {
            if (vv1 != vv) copy(vv1, vv);
            return; /* original bits */
        }

        bo = offs % (8 * sizeof(bdigit_t));
        wo = offs / 8 / sizeof(bdigit_t);
        bl = ln % (8 * sizeof(bdigit_t));
        wl = ln / 8 / sizeof(bdigit_t);

        sz = wl + (bl > 0);
        if (sz > 2) {
            v = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
            if (!v || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        } else v = tmp;

        v1 = vv1->u.bits.data + wo;
        if (bo) {
            for (i = 0; i < wl; i++) {
                v[i] = inv ^ (v1[i] >> bo) ^ (v1[i + 1] << (8 * sizeof(bdigit_t) - bo));
            }
            if (bl) {
                v[i] = v1[i] >> bo;
                if (bl > (8 * sizeof(bdigit_t) - bo)) v[i] |= v1[i + 1] << (8 * sizeof(bdigit_t) - bo);
                v[i] ^= inv;
            }
        } else {
            for (i = 0; i < wl; i++) v[i] = v1[i] ^ inv;
            if (bl) v[i] = v1[i] ^ inv;
        }
        if (bl) v[i] &= ((1 << bl) - 1);
    } else {
        sz = ln / 8 / sizeof(bdigit_t);
        if (ln % (8 * sizeof(bdigit_t))) sz++;
        if (sz > 2) {
            v = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
            if (!v || sz > ((size_t)~0) / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        } else v = tmp;

        uv = inv;
        i = bits = 0;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            wo = offs / 8 / sizeof(bdigit_t);
            if (wo < vv1->u.bits.len && ((vv1->u.bits.data[wo] >> (offs & (8 * sizeof(bdigit_t) - 1))) & 1)) {
                uv ^= 1 << bits;
            }
            bits++;
            if (bits == 8 * sizeof(bdigit_t)) {
                v[i++] = uv;
                uv = inv;
                bits = 0;
            }
            offs += step;
        }
        if (bits) v[i] = uv & ((1 << bits) - 1);
    }
    while (sz && !v[sz - 1]) sz--;
    if (vv == vv1) vv->obj->destroy(vv);
    if (sz <= 2) {
        memcpy(vv->u.bits.val, v, sz * sizeof(bdigit_t));
        if (tmp != v) free(v);
        v = vv->u.bits.val;
    }
    vv->obj = BITS_OBJ;
    vv->u.bits.bits = ln;
    vv->u.bits.len = sz;
    vv->u.bits.inv = 0;
    vv->u.bits.data = v;
}

void bitsobj_init(void) {
    obj_init(&obj, T_BITS, "<bits>");
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
