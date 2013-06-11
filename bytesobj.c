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
#include "isnprintf.h"

#include "numobj.h"
#include "uintobj.h"
#include "boolobj.h"
#include "sintobj.h"

static struct obj_s obj;

obj_t BYTES_OBJ = &obj;

static void destroy(struct value_s *v1) {
    free((uint8_t*)v1->u.bytes.data);
}

static void copy(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    v->obj = BYTES_OBJ;
    v->refcount = 1;
    v->u.bytes.len = v1->u.bytes.len;
    if (v1->u.bytes.len) {
        s = malloc(v1->u.bytes.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, v1->u.bytes.data, v1->u.bytes.len);
    } else s = NULL;
    v->u.bytes.data = s;
}

static void copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = BYTES_OBJ;
    v->refcount = 1;
    v->u.bytes.len = v1->u.bytes.len;
    v->u.bytes.data = v1->u.bytes.data;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BYTES_OBJ && v1->u.bytes.len == v2->u.bytes.len && (
            v1->u.bytes.data == v2->u.bytes.data ||
            !memcmp(v1->u.bytes.data, v2->u.bytes.data, v2->u.bytes.len));
}

static int truth(const struct value_s *v1) {
    return !!v1->u.bytes.len;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    size_t l = v1->u.bytes.len;
    const uint8_t *s2 = v1->u.bytes.data;
    unsigned int h;
    if (!l) return 0;
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.bytes.len;
    return h & ((~(unsigned int)0) >> 1);
}

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t UNUSED(epoint), linepos_t epoint2) {
    if (t == NUM_OBJ) {
        uval_t val;

        if (v1->u.bytes.len == 1) {
            val = v1->u.bytes.data[0];
        } else {
            if (v == v1) destroy(v1);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_BIG_STRING_CO;
            v->u.error.epoint = *epoint2;
            return;
        }
        if (v == v1) destroy(v1);
        v->obj = t;
        v->u.num.val = val;
        v->u.num.len = 8;
        return;
    }
    if (t == BYTES_OBJ) {
        if (v != v1) copy(v1, v);
        return;
    }
    return;
}

static void calc1(oper_t op) {
    switch (op->op->u.oper.op) {
    case O_LNOT:
        if (op->v1 == op->v) destroy(op->v);
        op->v->obj = BOOL_OBJ; 
        op->v->u.num.val = !op->v1->u.bytes.len;
        return;
    default:
        break;
    }
    obj_oper_error(op);
}

static int calc2_bytes(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    ival_t val;
    switch (op->op->u.oper.op) {
    case O_CMP:
        {
            int h = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len : v2->u.bytes.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.bytes.len > v2->u.bytes.len) - (v1->u.bytes.len < v2->u.bytes.len);
            if (v == v1 || v == v2) destroy(v);
            v->obj = (h < 0) ? SINT_OBJ : UINT_OBJ; v->u.num.val = h;
            return 0;
        }
    case O_EQ:
        val = (v1->u.bytes.len == v2->u.bytes.len) && (v1->u.bytes.data == v2->u.bytes.data || !memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len));
        break;
    case O_NE:
        val = (v1->u.bytes.len != v2->u.bytes.len) || (v1->u.bytes.data != v2->u.bytes.data && memcmp(v1->u.bytes.data, v2->u.bytes.data, v1->u.bytes.len));
        break;
    case O_LT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len < v2->u.bytes.len; else val = val < 0;
        break;
    case O_GT:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len > v2->u.bytes.len; else val = val > 0;
        break;
    case O_LE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len <= v2->u.bytes.len; else val = val <= 0;
        break;
    case O_GE:
        val = memcmp(v1->u.bytes.data, v2->u.bytes.data, (v1->u.bytes.len < v2->u.bytes.len) ? v1->u.bytes.len:v2->u.bytes.len);
        if (!val) val = v1->u.bytes.len >= v2->u.bytes.len; else val = val >= 0;
        break;
    case O_CONCAT:
        if (v == v1) {
            uint8_t *s;
            size_t len;
            if (!v2->u.bytes.len) return 0;
            len = v1->u.bytes.len;
            v->u.bytes.len += v2->u.bytes.len;
            s = (uint8_t *)v1->u.bytes.data;
            s = realloc(s, v->u.bytes.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s + len, v2->u.bytes.data, v2->u.bytes.len);
            v->u.bytes.data = s;
            return 0;
        } 
        v->obj = BYTES_OBJ;
        v->u.bytes.len = v1->u.bytes.len + v2->u.bytes.len;
        if (v->u.bytes.len) {
            uint8_t *s;
            s = malloc(v->u.bytes.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s, v1->u.bytes.data, v1->u.bytes.len);
            memcpy(s + v1->u.bytes.len, v2->u.bytes.data, v2->u.bytes.len);
            v->u.bytes.data = s;
        } else v->u.bytes.data = NULL;
        return 0;
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            if (!v1->u.bytes.len) { val = 1; break; }
            if (v1->u.bytes.len > v2->u.bytes.len) { val = 0; break; }
            c2 = v2->u.bytes.data;
            e = c2 + v2->u.bytes.len - v1->u.bytes.len;
            for (;;) {   
                c = memchr(c2, v1->u.bytes.data[0], e - c2 + 1);
                if (!c) { val = 0; break; }
                if (!memcmp(c, v1->u.bytes.data, v1->u.bytes.len)) { val = 1; break; }
                c2 = c + 1;
            }
            break;
        }
    default: return 1;
    }
    if (v == v1 || v == v2) destroy(v);
    v->obj = BOOL_OBJ; v->u.num.val = val;
    return 0;
}

static void repeat(oper_t op, uval_t rep) {
    struct value_s *v1 = op->v1, *v = op->v;
    if (v == v1) {
        uint8_t *s;
        size_t len = v1->u.bytes.len;
        
        if (!rep || !len) {
            if (v1 == v) destroy(v);
            copy(&null_bytes, v); return;
        }
        if (rep == 1) {
            if (v1 != v) copy(v1, v);
            return;
        }
        v->u.bytes.len *= rep;
        s = (uint8_t *)v1->u.bytes.data;
        s = realloc(s, v->u.bytes.len);
        if (!s) err_msg_out_of_memory();
        v->u.bytes.data = s;
        while (--rep) {
            memcpy(s + len, s, len);
            s += len;
        }
    } else {
        v->obj = BYTES_OBJ;
        v->u.bytes.len = 0;
        if (v1->u.bytes.len && rep) {
            uint8_t *s;
            s = malloc(v1->u.bytes.len * rep);
            if (!s) err_msg_out_of_memory();
            while (rep--) {
                memcpy(s + v->u.bytes.len, v1->u.bytes.data, v1->u.bytes.len);
                v->u.bytes.len += v1->u.bytes.len;
            }
            v->u.bytes.data = s;
        } else v->u.bytes.data = NULL;
    }
    return;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    switch (v2->obj->type) {
    case T_BYTES: if (calc2_bytes(op)) break; return;
    case T_TUPLE:
    case T_LIST: 
        if (op->op == &o_MOD) {
            isnprintf(v1, v2, v, &op->epoint, &op->epoint2); return;
        }
    case T_STR: 
        v2->obj->rcalc2(op); return;
    default: break;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1;
    switch (v1->obj->type) {
    case T_BYTES: if (calc2_bytes(op)) break; return;
    case T_TUPLE:
    case T_LIST: 
    case T_STR: 
        if (op->op != &o_IN) {
            v1->obj->calc2(op); return;
        }
    default: break;
    }
    obj_oper_error(op); return;
}

static int print(const struct value_s *v1, FILE *f) {
    size_t val;
    uint32_t ch;
    int l;
    l = printf("bytes([");
    for (val = 0;val < v1->u.bytes.len; val++) {
        ch = v1->u.bytes.data[val];
        if (val) {fputc(',', f);l++;}
        l += fprintf(f,"$%02x", ch);
    }
    l += printf("])");
    return l;
}

static void iindex(oper_t op) {
    const uint8_t *p;
    uint8_t *p2;
    size_t len, len2;
    ival_t offs;
    size_t i;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    if (!v2->u.list.len) {
        if (v1 == v) destroy(v);
        copy(&null_bytes, v);return;
    }
    len = v1->u.bytes.len;
    len2 = v2->u.list.len;
    p2 = malloc(len2);
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
        *p2++ = v1->u.bytes.data[offs];
    }
    if (v == v1) destroy(v);
    v->obj = BYTES_OBJ;
    v->u.bytes.len = len2;
    v->u.bytes.data = p;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t len;
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
        copy(&null_bytes, v);return;
    }
    if (step == 1) {
        if (len == v1->u.bytes.len) {
            if (v1 != v) copy(v1, v);
            return; /* original bytes */
        }
        p2 = malloc(len);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        memcpy(p2, v1->u.bytes.data + offs, len);
    } else {
        p2 = malloc(len);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            *p2++ = v1->u.bytes.data[offs];
            offs += step;
        }
    }
    if (v == v1) destroy(v);
    v->obj = BYTES_OBJ;
    v->u.bytes.len = len;
    v->u.bytes.data = p;
}

void bytesobj_init(void) {
    obj_init(&obj, T_BYTES, "<bytes>");
    obj.destroy = destroy;
    obj.copy = copy;
    obj.copy_temp = copy_temp;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.convert = convert;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.repeat = repeat;
    obj.print = print;
    obj.iindex = iindex;
    obj.slice = slice;
}
