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
#include "isnprintf.h"

#include "boolobj.h"
#include "intobj.h"
#include "bytesobj.h"
#include "listobj.h"

struct encoding_s;
static struct obj_s obj;

obj_t STR_OBJ = &obj;

static inline int utf8len(uint8_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}


static void destroy(struct value_s *v1) {
    free((uint8_t*)v1->u.str.data);
}

static void copy(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    v->obj = STR_OBJ;
    v->refcount = 1;
    v->u.str.chars = v1->u.str.chars;
    v->u.str.len = v1->u.str.len;
    if (v1->u.str.len) {
        s = (uint8_t *)malloc(v1->u.str.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, v1->u.str.data, v1->u.str.len);
    } else s = NULL;
    v->u.str.data = s;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = STR_OBJ;
    v->refcount = 1;
    v->u.str.chars = v1->u.str.chars;
    v->u.str.len = v1->u.str.len;
    v->u.str.data = v1->u.str.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == STR_OBJ && v1->u.str.len == v2->u.str.len && (
            v1->u.str.data == v2->u.str.data ||
            !memcmp(v1->u.str.data, v2->u.str.data, v2->u.str.len));
}

static int truth(const struct value_s *v1) {
    return !!v1->u.str.len;
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

static void repr(const struct value_s *v1, struct value_s *v) {
    size_t i2, i, sq = 0, dq = 0;
    uint8_t *s, *s2;
    char q;
    for (i = 0; i < v1->u.str.len; i++) {
        switch (v1->u.str.data[i]) {
        case '\'': sq++; continue;
        case '"': dq++; continue;
        }
    }
    if (sq < dq) {
        i += sq;
        q = '\'';
    } else {
        i += dq;
        q = '"';
    }

    i2 = i + 2;
    s2 = s = (uint8_t *)malloc(i2);
    if (!s || i2 < 2) err_msg_out_of_memory(); /* overflow */

    *s++ = q;
    for (i = 0; i < v1->u.str.len; i++) {
        s[i] = v1->u.str.data[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    if (v == v1) destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = s2;
    v->u.str.len = i2;
    v->u.str.chars = i2;
}

static void str(const struct value_s *v1, struct value_s *v) {
    if (v != v1) copy(v1, v);
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    if (bits_from_str(&tmp, v1)) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return 1;
    }
    ret = tmp.obj->ival(&tmp, v, iv, bits, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    if (bits_from_str(&tmp, v1)) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return 1;
    }
    ret = tmp.obj->uval(&tmp, v, uv, bits, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    if (bits_from_str(&tmp, v1)) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return 1;
    }
    ret = tmp.obj->real(&tmp, v, r, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *v, int *s, linepos_t epoint) {
    struct value_s tmp;
    int ret;
    if (bits_from_str(&tmp, v1)) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return 1;
    }
    ret = tmp.obj->sign(&tmp, v, s, epoint);
    tmp.obj->destroy(&tmp);
    return ret;
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    if (int_from_str(&tmp, v1)) {
        if (v == v1) v->obj->destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return;
    }
    if (v == v1) v->obj->destroy(v);
    tmp.obj->abs(&tmp, v, epoint);
    tmp.obj->destroy(&tmp);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    struct value_s tmp;
    if (int_from_str(&tmp, v1)) {
        if (v == v1) v->obj->destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_BIG_STRING_CO;
        v->u.error.epoint = *epoint;
        return;
    }
    if (v == v1) v->obj->destroy(v);
    tmp.obj->copy_temp(&tmp, v);
}

static int MUST_CHECK len(const struct value_s *v1, struct value_s *UNUSED(v), uval_t *uv, linepos_t UNUSED(epoint)) {
    *uv = v1->u.str.chars;
    return 0;
}

static void getiter(struct value_s *v1, struct value_s *v) {
    v->obj = ITER_OBJ;
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
}

static struct value_s *MUST_CHECK next(struct value_s *v1, struct value_s *v) {
    const struct value_s *vv1 = v1->u.iter.data;
    int len;
    uint8_t *s;
    if (v1->u.iter.val >= vv1->u.str.len) return NULL;
    len = utf8len(vv1->u.str.data[v1->u.iter.val]);
    s = (uint8_t *)malloc(len);
    if (!s) err_msg_out_of_memory();
    memcpy(s, vv1->u.str.data + v1->u.iter.val, len);
    v1->u.iter.val += len;

    v->obj = STR_OBJ;
    v->u.str.chars = 1;
    v->u.str.len = len;
    v->u.str.data = s;
    return v;
}

static void calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
    struct value_s tmp;
    switch (op->op->u.oper.op) {
    case O_NEG:
    case O_POS:
    case O_STRING: 
        if (int_from_str(&tmp, v1)) {
            if (v == v1) destroy(v);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_BIG_STRING_CO;
            v->u.error.epoint = op->epoint;
            return;
        }
        break;
    case O_LNOT:
        if (v1 == v) destroy(v);
        return bool_from_int(v, !truth(v1)); 
    default:
        if (bits_from_str(&tmp, v1)) {
            if (v == v1) destroy(v);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_BIG_STRING_CO;
            v->u.error.epoint = op->epoint;
        }
        break;
    }
    if (v == v1) destroy(v);
    op->v1 = &tmp;
    tmp.refcount = 0;
    tmp.obj->calc1(op);
    op->v1 = v1;
    tmp.obj->destroy(&tmp);
}

static int calc2_str(oper_t op) {
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
            struct value_s tmp, tmp2;
            if (int_from_str(&tmp, v1)) {
                if (v == v1) destroy(v1);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return 0;
            }
            if (int_from_str(&tmp2, v2)) {
                if (v == v1) destroy(v1);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return 0;
            }
            if (v1 == v || v2 == v) destroy(v);
            op->v1 = &tmp;
            op->v2 = &tmp2;
            tmp.refcount = 0;
            tmp2.refcount = 0;
            tmp.obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            tmp2.obj->destroy(&tmp2);
            tmp.obj->destroy(&tmp);
        }
        return 0;
    case O_AND:
    case O_OR:
    case O_XOR:
    case O_LSHIFT:
    case O_RSHIFT:
        {
            struct value_s tmp, tmp2;
            if (bits_from_str(&tmp, v1)) {
                if (v == v1) destroy(v1);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return 0;
            }
            if (bits_from_str(&tmp2, v2)) {
                if (v == v1) destroy(v1);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return 0;
            }
            if (v1 == v || v2 == v) destroy(v);
            op->v1 = &tmp;
            op->v2 = &tmp2;
            tmp.refcount = 0;
            tmp2.refcount = 0;
            tmp.obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            tmp2.obj->destroy(&tmp2);
            tmp.obj->destroy(&tmp);
        }
        return 0;
    case O_CMP:
        {
            int h = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len : v2->u.str.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.str.len > v2->u.str.len) - (v1->u.str.len < v2->u.str.len);
            if (v == v1 || v == v2) destroy(v);
            int_from_int(v, h);
            return 0;
        }
    case O_EQ:
        val = (v1->u.str.len == v2->u.str.len) && (v1->u.str.data == v2->u.str.data || !memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len));
        break;
    case O_NE:
        val = (v1->u.str.len != v2->u.str.len) || (v1->u.str.data != v2->u.str.data && memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len));
        break;
    case O_LT:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        if (!val) val = v1->u.str.len < v2->u.str.len; else val = val < 0;
        break;
    case O_GT:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        if (!val) val = v1->u.str.len > v2->u.str.len; else val = val > 0;
        break;
    case O_LE:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        if (!val) val = v1->u.str.len <= v2->u.str.len; else val = val <= 0;
        break;
    case O_GE:
        val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
        if (!val) val = v1->u.str.len >= v2->u.str.len; else val = val >= 0;
        break;
    case O_CONCAT:
        if (v == v1) {
            uint8_t *s;
            size_t len;
            if (!v2->u.str.len) return 0;
            len = v1->u.str.len;
            v->u.str.len += v2->u.str.len;
            v->u.str.chars += v2->u.str.chars;
            s = (uint8_t *)v1->u.str.data;
            s = (uint8_t *)realloc(s, v->u.str.len);
            if (!s || v->u.str.len < v2->u.str.len) err_msg_out_of_memory(); /* overflow */
            memcpy(s + len, v2->u.str.data, v2->u.str.len);
            v->u.str.data = s;
            return 0;
        } 
        v->obj = STR_OBJ;
        v->u.str.len = v1->u.str.len + v2->u.str.len;
        if (v->u.str.len < v2->u.str.len) err_msg_out_of_memory(); /* overflow */
        v->u.str.chars = v1->u.str.chars + v2->u.str.chars;
        if (v->u.str.len) {
            uint8_t *s;
            s = (uint8_t *)malloc(v->u.str.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s, v1->u.str.data, v1->u.str.len);
            memcpy(s + v1->u.str.len, v2->u.str.data, v2->u.str.len);
            v->u.str.data = s;
        } else v->u.str.data = NULL;
        return 0;
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.str.len) { val = 1; break; }
            if (v1->u.str.len > v2->u.str.len) { val = 0; break; }
            c2 = v2->u.str.data;
            e = c2 + v2->u.str.len - v1->u.str.len;
            for (;;) {   
                c = (uint8_t *)memchr(c2, v1->u.str.data[0], e - c2 + 1);
                if (!c) { val = 0; break; }
                if (!memcmp(c, v1->u.str.data, v1->u.str.len)) { val = 1; break; }
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
        uint8_t *s, *m;
        size_t len = v1->u.str.len;
        
        if (!rep || !len) {
            if (v1 == v) destroy(v);
            copy(&null_str, v); return;
        }
        if (rep == 1) {
            if (v1 != v) copy(v1, v);
            return;
        }
        v->u.str.len *= rep;
        v->u.str.chars *= rep;
        s = (uint8_t *)v1->u.str.data;
        s = (uint8_t *)realloc(s, v->u.str.len);
        if (!s || v->u.str.len < len) err_msg_out_of_memory(); /* overflow */
        v->u.str.data = s;
        m = s + v->u.str.len - len;
        s += len;
        while (--rep) {
            if (s > m) err_msg_out_of_memory(); /* overflow */
            memcpy(s, s - len, len);
            s += len;
        }
    } else {
        v->obj = STR_OBJ;
        v->u.str.len = 0;
        v->u.str.chars = 0;
        if (v1->u.str.len && rep) {
            uint8_t *s, *m;
            size_t len = v1->u.str.len * rep;
            s = (uint8_t *)malloc(len);
            if (!s || len < v1->u.str.len) err_msg_out_of_memory(); /* overflow */
            v->u.str.data = s;
            m = s + len - v1->u.str.len;
            while (rep--) {
                if (s > m) err_msg_out_of_memory(); /* overflow */
                memcpy(s, v1->u.str.data, v1->u.str.len);
                s += v1->u.str.len;
                v->u.str.chars += v1->u.str.chars;
            }
            v->u.str.len = s - v->u.str.data;
        } else v->u.str.data = NULL;
    }
    return;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v2->obj->type) {
    case T_STR: if (calc2_str(op)) break; return;
    case T_BOOL:
    case T_INT:
    case T_BITS: 
    case T_FLOAT:
    case T_CODE: 
    case T_ADDRESS: 
        {
            int ret;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: ret = bits_from_str(&tmp, v1); break;
            default: ret = int_from_str(&tmp, v1);
            }
            if (ret) {
                if (v == v1 || v == v2) v->obj->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return;
            }
            if (v1 == v) v->obj->destroy(v);
            op->v1 = &tmp;
            tmp.refcount = 0;
            tmp.obj->calc2(op);
            op->v1 = v1;
            tmp.obj->destroy(&tmp);
        }
        return;
    case T_BYTES: 
        {
            if (bytes_from_str(&tmp, v1)) {
                if (v == v1 || v == v2) v->obj->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return;
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
        if (op->op == &o_MOD) {
            isnprintf(v1, v2, v, &op->epoint, &op->epoint2); return;
        }
        v2->obj->rcalc2(op); return;
    default: break;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    switch (v1->obj->type) {
    case T_STR: if (calc2_str(op)) break; return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS: 
        {
            int ret;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: ret = bits_from_str(&tmp, v2); break;
            default: ret = int_from_str(&tmp, v2);
            }
            if (ret) {
                if (v == v1 || v == v2) v->obj->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return;
            }
            if (v2 == v) v->obj->destroy(v);
            op->v2 = &tmp;
            tmp.refcount = 0;
            tmp.obj->rcalc2(op); 
            op->v2 = v2;
            tmp.obj->destroy(&tmp);
        }
        return;
    case T_BYTES: 
        {
            if (bytes_from_str(&tmp, v2)) {
                if (v == v1 || v == v2) v->obj->destroy(v);
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = op->epoint;
                return;
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

    len = v1->u.str.chars;

    if (v2->obj == TUPLE_OBJ || v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            if (v1 == v) destroy(v);
            copy(&null_str, v);return;
        }
        if (v1->u.str.len == v1->u.str.chars) {
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
                *p2++ = v1->u.str.data[offs];
            }
        }
        else {
            ival_t j, k;
            size_t m = v1->u.str.len;
            uint8_t *o;
            p2 = (uint8_t*)malloc(m);
            if (!p2) err_msg_out_of_memory();
            o = p2;
            p = v1->u.str.data;
            j = 0;

            for (i = 0; i < v2->u.list.len; i++) {
                offs = indexoffs(v2->u.list.data[i], len);
                if (offs < 0) {
                    free(o);
                    v->obj = ERROR_OBJ;
                    v->u.error.num = ERROR___INDEX_RANGE;
                    v->u.error.epoint = op->epoint2;
                    return;
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
                    o = (uint8_t *)realloc(o, m);
                    if (!o || m < 4096) err_msg_out_of_memory(); /* overflow */
                    p2 += o - r;
                }
                memcpy(p2, p, k);p2 += k;
            }
            len2 = p2 - o;
            p = (uint8_t *)realloc(o, len2);
            if (!p) err_msg_out_of_memory();
        }
        if (v == v1) destroy(v);
        v->obj = STR_OBJ;
        v->u.str.chars = len;
        v->u.str.len = len2;
        v->u.str.data = p;
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

    if (v1->u.str.len == v1->u.str.chars) {
        len = 1;
        p2 = (uint8_t *)malloc(len);
        if (!p2) err_msg_out_of_memory();
        p2[0] = v1->u.str.data[offs];
    }
    else {
        p = v1->u.str.data;
        while (offs--) p += utf8len(*p);
        len = utf8len(*p);
        p2 = (uint8_t *)malloc(len);
        if (!p2) err_msg_out_of_memory();
        memcpy(p2, p, len);
    }
    if (v1 == v) destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = p2;
    v->u.str.chars = 1;
    v->u.str.len = len;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t len, len2;
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
        copy(&null_str, v);return;
    }
    if (step == 1) {
        if (len == v1->u.str.chars) {
            if (v1 != v) copy(v1, v);
            return; /* original string */
        }
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = len;
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
        p2 = (uint8_t *)malloc(len2);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        memcpy(p2, v1->u.str.data + offs, len2);
    } else {
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = len;
            p2 = (uint8_t *)malloc(len2);
            if (!p2) err_msg_out_of_memory();
            p = p2;
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p2++ = v1->u.str.data[offs];
                offs += step;
            }
        }
        else {
            ival_t i, j, k;
            uint8_t *o;
            p2 = (uint8_t *)malloc(v1->u.str.len);
            if (!p2) err_msg_out_of_memory();
            o = p2;
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
            p = (uint8_t *)realloc(o, len2);
            if (!p) err_msg_out_of_memory();
        }
    }
    if (v == v1) destroy(v);
    v->obj = STR_OBJ;
    v->u.str.chars = len;
    v->u.str.len = len2;
    v->u.str.data = p;
}

void strobj_init(void) {
    obj_init(&obj, T_STR, "<str>");
    obj.destroy = destroy;
    obj.copy = copy;
    obj.copy_temp = copy_temp;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.str = str;
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
    obj.slice = slice;
}
