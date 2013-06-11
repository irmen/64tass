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
#include "codeobj.h"
#include "eval.h"
#include "mem.h"
#include "64tass.h"

#include "strobj.h"
#include "numobj.h"
#include "sintobj.h"
#include "uintobj.h"
#include "listobj.h"
#include "boolobj.h"

static struct obj_s obj;

obj_t CODE_OBJ = &obj;

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == CODE_OBJ && v1->u.code.addr == v2->u.code.addr && v1->u.code.size == v2->u.code.size && v1->u.code.dtype == v2->u.code.dtype;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.code.addr;
}

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t UNUSED(epoint), linepos_t UNUSED(epoint2)) {
    if (t == STR_OBJ) {
        char line[100]; 
        uint8_t *s;
        v->obj = t;
        v->u.str.len = sprintf(line, "%" PRIuval, (uval_t)v1->u.code.addr);
        v->u.str.chars = v->u.str.len;
        s = malloc(v->u.str.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, line, v->u.str.len);
        v->u.str.data = s;
        return;
    }
    if (t == NUM_OBJ) {
        v->obj = NUM_OBJ;
        v->u.num.val = v1->u.code.addr;
        v->u.num.len = 8*sizeof(address_t);
        return;
    }
    ////////////////////////obj_oper_error(O_STRING, v1, NULL, v, epoint);
}

static void calc1(oper_t op) {
    if (!calc1_uint(op, op->v1->u.code.addr)) return;
    obj_oper_error(op);
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    switch (v2->obj->type) {
    case T_NUM: 
        switch (op->op->u.oper.op) {
        case O_AND:
        case O_OR:
        case O_XOR: if (calc2_num_num(op, v1->u.code.addr, 0, v2->u.num.val, v2->u.num.len)) break; return;
        default: break;
        }
        /* fall through */
    case T_BOOL:
    case T_UINT:
        switch (op->op->u.oper.op) {
        case O_ADD:
            v->obj = CODE_OBJ;
            v->u.code.addr = v1->u.code.addr + (uval_t)v2->u.num.val;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE; return;
        case O_SUB:
            v->obj = CODE_OBJ;
            v->u.code.addr = v1->u.code.addr - (uval_t)v2->u.num.val;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE; return;
        default: break;
        }
        if (calc2_uint_uint(op, v1->u.code.addr, v2->u.num.val)) break; return;
    case T_SINT:
        switch (op->op->u.oper.op) {
        case O_ADD:
            v->obj = CODE_OBJ;
            v->u.code.addr = v1->u.code.addr + v2->u.num.val;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE; return;
        case O_SUB:
            v->obj = CODE_OBJ;
            v->u.code.addr = v1->u.code.addr - v2->u.num.val;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE; return;
        default: break;
        }
        if (calc2_uint_sint(op, v1->u.code.addr, v2->u.num.val)) break; return;
    case T_CODE:
        if (calc2_uint_uint(op, v1->u.code.addr, v2->u.code.addr)) break; return;
    default: v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    if (op->op == &o_X) {
        v1->obj->repeat(op, v2->u.code.addr); return;
    }
    if (op->op == &o_IN) {
        struct oper_s oper;
        size_t i, len, offs;
        struct value_s new_value;
        int16_t r;

        if (v2->u.code.pass != pass) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = op->epoint2;
            v->u.error.u.ident.len = 6;
            v->u.error.u.ident.data = (const uint8_t *)"<code>";
            return;
        }
        len = (v2->u.code.dtype < 0) ? -v2->u.code.dtype : v2->u.code.dtype;
        len = len + !len;
        oper.op = &o_EQ;
        oper.v1 = &new_value;
        oper.v2 = v1;
        oper.v = &new_value;
        oper.epoint = op->epoint;
        oper.epoint2 = op->epoint2;
        oper.epoint3 = op->epoint3;
        for (offs = 0; offs < v2->u.code.size;) {
            new_value.u.num.val = 0;
            r = -1;
            for (i = 0; i < len; i++) {
                r = read_mem(v2->u.code.mem, v2->u.code.memp, v2->u.code.membp, offs++);
                if (r < 0) break;
                new_value.u.num.val |= r << (i * 8);
            }
            if (v2->u.code.dtype < 0 && (r & 0x80)) {
                for (; i < sizeof(new_value.u.num.val); i++) {
                    new_value.u.num.val |= 0xff << (i * 8);
                }
            }
            if (r < 0) {
                new_value.obj = GAP_OBJ;
            } else if (v2->u.code.dtype < 0) {
                new_value.obj = SINT_OBJ;
            } else {
                new_value.obj = NUM_OBJ;
                new_value.u.num.len = len * 8;
            }
            new_value.obj->calc2(&oper);
            if (new_value.obj == BOOL_OBJ && new_value.u.num.val) {
                if (v == v1) obj_destroy(v);
                v->obj = BOOL_OBJ; v->u.num.val = 1;
                return;
            }
        }
        if (v == v1) obj_destroy(v);
        v->obj = BOOL_OBJ; v->u.num.val = 0;
        return;
    }
    switch (v1->obj->type) {
    case T_NUM: 
        switch (op->op->u.oper.op) {
        case O_LSHIFT:
        case O_RSHIFT:
        case O_AND:
        case O_OR:
        case O_XOR: if (calc2_num_num(op, v1->u.num.val, v1->u.num.len, v2->u.code.addr, 0)) break; return;
        default: break;
        }
        /* fall through */
    case T_BOOL:
    case T_UINT:
        if (op->op == &o_ADD) {
            v->obj = CODE_OBJ;
            v->u.code.addr = (uval_t)v1->u.num.val + v2->u.code.addr;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE; return;
        }
        if (calc2_uint_uint(op, v1->u.num.val, v2->u.code.addr)) break; return;
    case T_SINT:
        if (op->op == &o_ADD) {
            v->obj = CODE_OBJ;
            v->u.code.addr = v1->u.num.val + v2->u.code.addr;
            v->u.code.size = 0;
            v->u.code.dtype = D_NONE;
            return;
        }
        if (calc2_sint_uint(op, v1->u.num.val, v2->u.code.addr)) break; return;
    case T_CODE:
        if (calc2_uint_uint(op, v1->u.code.addr, v2->u.code.addr)) break; return;
    default: v1->obj->calc2(op); return;
    }
    obj_oper_error(op);
}

static int print(const struct value_s *v1, FILE *f) {
    return fprintf(f, "$%" PRIxval, (uval_t)v1->u.code.addr);
}

static void iindex(oper_t op) {
    struct value_s **vals;
    size_t i, i2;
    size_t len, len2;
    size_t offs2;
    int16_t r;
    ival_t offs;
    uval_t val;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    if (!v2->u.list.len) {
        TUPLE_OBJ->copy(&null_tuple, v); return;
    }
    if (v1->u.code.pass != pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR___NOT_DEFINED;
        v->u.error.epoint = op->epoint;
        v->u.error.u.ident.len = 6;
        v->u.error.u.ident.data = (const uint8_t *)"<code>";
        return;
    }
    vals = malloc(v2->u.list.len * sizeof(v2->u.list.data[0]));
    if (!vals) err_msg_out_of_memory();
    len2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    len2 = len2 + !len2;
    len = v1->u.code.size / len2;
    for (i = 0; i < v2->u.list.len; i++) {
        offs = indexoffs(v2->u.list.data[i], len);
        if (offs < 0) {
            v->u.list.data = vals;
            v->u.list.len = i;
            TUPLE_OBJ->destroy(v);
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___INDEX_RANGE;
            v->u.error.epoint = op->epoint2;
            return;
        }
        offs2 = offs * len2;
        val = 0;
        r = -1;
        for (i2 = 0; i2 < len2; i2++) {
            r = read_mem(v1->u.code.mem, v1->u.code.memp, v1->u.code.membp, offs2++);
            if (r < 0) break;
            val |= r << (i2 * 8);
        }
        if (v1->u.code.dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(val); i2++) {
                val |= 0xff << (i2 * 8);
            }
        }
        vals[i] = val_alloc();
        if (r < 0) {
            vals[i]->obj = GAP_OBJ;
        } else if (v1->u.code.dtype < 0) {
            vals[i]->obj = SINT_OBJ;
            vals[i]->u.num.val = val;
        } else {
            vals[i]->obj = NUM_OBJ;
            vals[i]->u.num.len = len2 * 8;
            vals[i]->u.num.val = val;
        }
    }
    v->obj = TUPLE_OBJ;
    v->u.list.data = vals;
    v->u.list.len = i;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t epoint) {
    struct value_s **vals;
    size_t i, i2;
    size_t len, len2;
    size_t offs2;
    int16_t r;
    uval_t val;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }

    if (!len) {
        TUPLE_OBJ->copy(&null_tuple, v); return;
    }
    if (v1->u.code.pass != pass) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR___NOT_DEFINED;
        v->u.error.epoint = *epoint;
        v->u.error.u.ident.len = 6;
        v->u.error.u.ident.data = (const uint8_t *)"<code>";
        return;
    }
    vals = malloc(len * sizeof(v1->u.list.data[0]));
    if (!vals) err_msg_out_of_memory();
    i = 0;
    len2 = (v1->u.code.dtype < 0) ? -v1->u.code.dtype : v1->u.code.dtype;
    len2 = len2 + !len2;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        offs2 = offs * len2;
        val = 0;
        r = -1;
        for (i2 = 0; i2 < len2; i2++) {
            r = read_mem(v1->u.code.mem, v1->u.code.memp, v1->u.code.membp, offs2++);
            if (r < 0) break;
            val |= r << (i2 * 8);
        }
        if (v1->u.code.dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(val); i2++) {
                val |= 0xff << (i2 * 8);
            }
        }
        if (r < 0) {
            vals[i]->obj = GAP_OBJ;
        } else if (v1->u.code.dtype < 0) {
            vals[i]->obj = SINT_OBJ;
            vals[i]->u.num.val = val;
        } else {
            vals[i]->obj = NUM_OBJ;
            vals[i]->u.num.len = len2 * 8;
            vals[i]->u.num.val = val;
        }
        i++; offs += step;
    }
    v->obj = TUPLE_OBJ;
    v->u.list.len = len;
    v->u.list.data = vals;
}

void codeobj_init(void) {
    obj_init(&obj, T_CODE, "<code>");
    obj.same = same;
    obj.truth = truth;
    obj.convert = convert;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.print = print;
    obj.iindex = iindex;
    obj.slice = slice;
}
