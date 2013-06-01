/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include <string.h>
#include "values.h"
#include "strobj.h"
#include "eval.h"
#include "misc.h"
#include "isnprintf.h"

#include "numobj.h"
#include "uintobj.h"
#include "boolobj.h"
#include "sintobj.h"
#include "bytesobj.h"

struct encoding_s;
static struct obj_s obj;

obj_t STR_OBJ = &obj;

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
        s = malloc(v1->u.str.len);
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

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t UNUSED(epoint), linepos_t epoint2) {
    if (t == NUM_OBJ) {
        uint16_t ch;
        unsigned int large = 0;
        size_t i = 0;
        uval_t val = 0;

        if (actual_encoding) {
            while (v1->u.str.len > i) {
                if (large >= 8 * sizeof(val)) {
                    if (v == v1) destroy(v1);
                    v->obj = ERROR_OBJ;
                    v->u.error.num = ERROR_BIG_STRING_CO;
                    v->u.error.epoint = *epoint2;
                    return;
                }

                ch = petascii(&i, v1);
                if (ch > 255) {
                    if (v == v1) destroy(v1);
                    v->obj = NONE_OBJ;return;   /*          TODO         */
                }

                val |= (uint8_t)ch << large;
                large += 8;
            }
        } else if (v1->u.str.len == 1) {
            uint32_t ch2 = v1->u.str.data[0];
            if (ch2 & 0x80) i = utf8in(v1->u.str.data, &ch2); else i=1;
            val = ch2;
            large = 32;
        } else if (v1->u.str.len) {
            if (v == v1) destroy(v1);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_BIG_STRING_CO;
            v->u.error.epoint = *epoint2;
            return;
        }
        if (v == v1) destroy(v1);
        v->obj = t;
        v->u.num.val = val;
        v->u.num.len = large;
        return;
    }
    if (t == STR_OBJ) {
        if (v != v1) copy(v1, v);
        return;
    }
    if (t == BYTES_OBJ) {
        size_t len = v1->u.str.len, len2 = 0;
        uint8_t *s;
        if (len) {
            s = malloc(len);
            if (!s) err_msg_out_of_memory();

            if (actual_encoding) {
                size_t i = 0;
                int16_t ch;
                while (v1->u.str.len > i) {
                    ch = petascii(&i, v1);
                    if (ch > 255) {
                        free(s);
                        v->obj = NONE_OBJ;
                        return;
                    }
                    s[len2++] = ch;
                }
            } else {
                memcpy(s, v1->u.str.data, len);
                len2 = len;
            }
        } else s = NULL;
        if (v == v1) destroy(v1);
        v->obj = t;
        v->u.bytes.len = len2;
        v->u.bytes.data = s;
        return;
    }
    return;
}

static void calc1(oper_t op) {
    switch (op->op->u.oper.op) {
    case O_LNOT:
        if (op->v1 == op->v) destroy(op->v);
        op->v->obj = BOOL_OBJ; 
        op->v->u.num.val = !op->v1->u.str.len;
        return;
    case O_STRING:
        convert(op->v1, op->v, STR_OBJ, &op->epoint, &op->epoint3);
        return;
    default:
        convert(op->v1, op->v, NUM_OBJ, &op->epoint, &op->epoint);
        if (op->v->obj == NUM_OBJ) {
            struct value_s *old = op->v1;
            op->v1 = op->v;
            NUM_OBJ->calc1(op);
            op->v1 = old;
        }
    }
}

static int calc2_str(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    ival_t val;
    switch (op->op->u.oper.op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
    case O_AND:
    case O_OR:
    case O_XOR:
    case O_LSHIFT:
    case O_RSHIFT:
        {
            struct value_s tmp;
            convert(v1, &tmp, NUM_OBJ, &op->epoint, &op->epoint);
            if (tmp.obj == NUM_OBJ) {
                convert(v2, v, NUM_OBJ, &op->epoint2, &op->epoint2);
                if (v->obj == NUM_OBJ) {
                    if (v1 == v || v2 == v) destroy(v);
                    return calc2_num_num(op, tmp.u.num.val, tmp.u.num.len, v->u.num.val, v->u.num.len);
                }
            } else tmp.obj->copy_temp(&tmp, v);
        }
        return 0;
    case O_CMP:
        {
            int h = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len : v2->u.str.len);
            if (h) h = (h > 0) - (h < 0);
            else h = (v1->u.str.len > v2->u.str.len) - (v1->u.str.len < v2->u.str.len);
            if (v == v1 || v == v2) destroy(v);
            v->obj = (h < 0) ? SINT_OBJ : UINT_OBJ; v->u.num.val = h;
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
            s = realloc(s, v->u.str.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s + len, v2->u.str.data, v2->u.str.len);
            v->u.str.data = s;
            return 0;
        } 
        v->obj = STR_OBJ;
        v->u.str.len = v1->u.str.len + v2->u.str.len;
        v->u.str.chars = v1->u.str.chars + v2->u.str.chars;
        if (v->u.str.len) {
            uint8_t *s;
            s = malloc(v->u.str.len);
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
                c = memchr(c2, v1->u.str.data[0], e - c2 + 1);
                if (!c) { val = 0; break; }
                if (!memcmp(c, v1->u.str.data, v1->u.str.len)) { val = 1; break; }
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
        s = realloc(s, v->u.str.len);
        if (!s) err_msg_out_of_memory();
        v->u.str.data = s;
        while (--rep) {
            memcpy(s + len, s, len);
            s += len;
        }
    } else {
        v->obj = STR_OBJ;
        v->u.str.len = 0;
        v->u.str.chars = 0;
        if (v1->u.str.len && rep) {
            uint8_t *s;
            s = malloc(v1->u.str.len * rep);
            if (!s) err_msg_out_of_memory();
            while (rep--) {
                memcpy(s + v->u.str.len, v1->u.str.data, v1->u.str.len);
                v->u.str.len += v1->u.str.len;
                v->u.str.chars += v1->u.str.chars;
            }
            v->u.str.data = s;
        } else v->u.str.data = NULL;
    }
    return;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    switch (v2->obj->type) {
    case T_STR: if (calc2_str(op)) break; return;
    case T_BOOL:
    case T_UINT:
    case T_NUM: 
    case T_SINT: 
    case T_FLOAT:
    case T_CODE: 
    case T_ADDRESS: 
        {
            struct value_s tmp;
            convert(v1, &tmp, NUM_OBJ, &op->epoint, &op->epoint);
            if (tmp.obj == NUM_OBJ) {
                if (v1 == v) destroy(v);
                op->v1 = &tmp;
                NUM_OBJ->calc2(op);
                op->v1 = v1;
            } else tmp.obj->copy_temp(&tmp, v);
        }
        return;
    case T_BYTES: 
        {
            struct value_s tmp;
            convert(v1, &tmp, BYTES_OBJ, &op->epoint, &op->epoint);
            if (tmp.obj == BYTES_OBJ) {
                if (v1 == v) destroy(v);
                op->v1 = &tmp;
                BYTES_OBJ->calc2(op);
                op->v1 = v1;
            } else tmp.obj->copy_temp(&tmp, v);
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
    switch (v1->obj->type) {
    case T_STR: if (calc2_str(op)) break; return;
    case T_BOOL:
    case T_UINT:
    case T_NUM: 
    case T_SINT:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS: 
        {
            struct value_s tmp;
            convert(v2, &tmp, NUM_OBJ, &op->epoint, &op->epoint);
            if (tmp.obj == NUM_OBJ) {
                if (v2 == v) destroy(v);
                op->v2 = &tmp;
                NUM_OBJ->rcalc2(op); 
                op->v2 = v2;
            } else tmp.obj->copy_temp(&tmp, v);
        }
        return;
    case T_BYTES: 
        {
            struct value_s tmp;
            convert(v2, &tmp, BYTES_OBJ, &op->epoint, &op->epoint);
            if (tmp.obj == BYTES_OBJ) {
                if (v2 == v) destroy(v);
                op->v2 = &tmp;
                BYTES_OBJ->rcalc2(op); 
                op->v2 = v2;
            } else tmp.obj->copy_temp(&tmp, v);
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

static int print(const struct value_s *v1, FILE *f) {
    size_t val;
    uint32_t ch;
    uint8_t c;
    int l = 2;
    c = memchr(v1->u.str.data, '"', v1->u.str.len) ? '\'' : '"';
    fputc(c, f);
    for (val = 0;val < v1->u.str.len;) {
        ch = v1->u.str.data[val];
        if (ch & 0x80) val += utf8in(v1->u.str.data + val, &ch); else val++;
        if (ch == c) {fputc(c, f);l++;}
        if (ch < 32 || ch > 127) l += fprintf(f,"{$%02x}", ch);
        else {fputc(ch, f);l++;}
    }
    fputc(c, f);
    return l;
}

static inline int utf8len(uint8_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}

static void iindex(oper_t op) {
    const uint8_t *p;
    uint8_t *p2;
    size_t len, len2;
    ival_t offs;
    size_t i;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    len = v1->u.str.chars;
    if (!v2->u.list.len) {
        if (v1 == v) destroy(v);
        copy(&null_str, v);return;
    }
    if (v1->u.str.len == v1->u.str.chars) {
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
            *p2++ = v1->u.str.data[offs];
        }
    }
    else {
        ival_t j, k;
        size_t m = v1->u.str.len;
        uint8_t *o;
        p2 = malloc(m);
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
                o = realloc(o, m);
                if (!o) err_msg_out_of_memory();
                p2 += o - r;
            }
            memcpy(p2, p, k);p2 += k;
        }
        len2 = p2 - o;
        p = realloc(o, len2);
        if (!p) err_msg_out_of_memory();
    }
    if (v == v1) destroy(v);
    v->obj = STR_OBJ;
    v->u.str.chars = len;
    v->u.str.len = len2;
    v->u.str.data = p;
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
        p2 = malloc(len2);
        if (!p2) err_msg_out_of_memory();
        p = p2;
        memcpy(p2, v1->u.str.data + offs, len2);
    } else {
        if (v1->u.str.len == v1->u.str.chars) {
            len2 = len;
            p2 = malloc(len2);
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
            p2 = malloc(v1->u.str.len);
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
            p = realloc(o, len2);
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
    obj.convert = convert;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.repeat = repeat;
    obj.print = print;
    obj.iindex = iindex;
    obj.slice = slice;
}
