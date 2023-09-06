/*
    $Id: codeobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "codeobj.h"
#include <string.h>
#include "eval.h"
#include "mem.h"
#include "64tass.h"
#include "section.h"
#include "variables.h"
#include "error.h"
#include "values.h"
#include "arguments.h"

#include "boolobj.h"
#include "floatobj.h"
#include "namespaceobj.h"
#include "listobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "bytesobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "memblocksobj.h"
#include "symbolobj.h"
#include "addressobj.h"
#include "strobj.h"

static Type obj;

Type *const CODE_OBJ = &obj;

static FAST_CALL void destroy(Obj *o1) {
    Code *v1 = Code(o1);
    val_destroy(v1->typ);
    val_destroy(Obj(v1->memblocks));
    val_destroy(Obj(v1->names));
}

static FAST_CALL void garbage(Obj *o1, int i) {
    Code *v1 = Code(o1);
    Obj *v;
    switch (i) {
    case -1:
        v1->typ->refcount--;
        v1->memblocks->v.refcount--;
        v1->names->v.refcount--;
        return;
    case 0:
        return;
    case 1:
        v = v1->typ;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        v = Obj(v1->memblocks);
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        v = Obj(v1->names);
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static MUST_CHECK Obj *access_check(const Code *v1, linepos_t epoint) {
    if ((v1->requires & ~current_section->provides) != 0) {
        return Obj(new_error(ERROR_REQUIREMENTS_, epoint));
    }
    if ((v1->conflicts & current_section->provides) != 0) {
        return Obj(new_error(ERROR______CONFLICT, epoint));
    }
    return NULL;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Code *v1 = Code(o1), *v2 = Code(o2);
    return o1->obj == o2->obj && v1->addr == v2->addr && (v1->typ == v2->typ || v1->typ->obj->same(v1->typ, v2->typ))
        && v1->size == v2->size && v1->offs == v2->offs && v1->dtype == v2->dtype
        && v1->requires == v2->requires && v1->conflicts == v2->conflicts
/*        && (v1->memblocks == v2->memblocks || v1->memblocks->v.obj->same(Obj(v1->memblocks), Obj(v2->memblocks))) */
        && (v1->names == v2->names || v1->names->v.obj->same(Obj(v1->names), Obj(v2->names)));
}

static inline address_t code_address(const Code *v1) {
    return v1->offs < 0 ? v1->addr - -(uval_t)v1->offs : v1->addr + (uval_t)v1->offs;
}

static inline address_t code_memaddress(const Code *v1) {
    return v1->offs < 0 ? v1->memaddr - -(uval_t)v1->offs : v1->memaddr + (uval_t)v1->offs;
}

static MUST_CHECK Obj *get_code_address(const Code *v1, linepos_t epoint) {
    address_t addr2 = code_address(v1);
    address_t addr = addr2 & all_mem;
    if (addr2 != addr) err_msg_addr_wrap(epoint);
    return get_star_value(addr, v1->typ);
}

MUST_CHECK Obj *get_code_value(const Code *v1, linepos_t epoint) {
    Obj *err = access_check(v1, epoint);
    if (err != NULL) return err;
    return get_code_address(v1, epoint);
}

MUST_CHECK Error *code_uaddress(Obj *o1, uval_t *uv, uval_t *uv2, linepos_t epoint) {
    unsigned int bits = all_mem_bits;
    Code *v1 = Code(o1);
    Error *v = Error(access_check(v1, epoint));
    if (v != NULL) return v;
    *uv = code_address(v1);
    *uv2 = v1->addr;
    if (bits >= sizeof(*uv2)*8 || (*uv2 >> bits) == 0) {
        return NULL;
    }
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = get_star_value(*uv & all_mem, v1->typ);
    return v;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    Code *v1 = Code(o1);
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = v->obj->truth(v, type, epoint);
    val_destroy(v);
    return result;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Code *v1 = Code(o1);
    Obj *v, *result;
    if (epoint != NULL) {
        v = access_check(v1, epoint);
        if (v != NULL) return v;
        v = get_code_address(v1, epoint);
    } else v = get_star_value(code_address(v1) & all_mem, v1->typ);
    result = v->obj->repr(v, epoint, maxsize);
    val_destroy(v);
    return result;
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t epoint, size_t maxsize) {
    Code *v1 = Code(o1);
    Obj *v, *result;
    if (epoint != NULL) {
        v = access_check(v1, epoint);
        if (v != NULL) return v;
        v = get_code_address(v1, epoint);
    } else v = get_star_value(code_address(v1) & all_mem, v1->typ);
    result = v->obj->str(v, epoint, maxsize);
    val_destroy(v);
    return result;
}

static MUST_CHECK Error *iaddress(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Code *v1 = Code(o1);
    address_t addr, addr2;
    Error *v = Error(access_check(v1, epoint));
    if (v != NULL) return v;
    addr2 = code_address(v1);
    addr = addr2 & all_mem;
    *iv = (ival_t)addr;
    if ((addr >> (bits - 1)) == 0) {
        if (addr2 != addr) err_msg_addr_wrap(epoint);
        return NULL;
    }
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = get_star_value(addr, v1->typ);
    return v;
}

static MUST_CHECK Error *uaddress(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Code *v1 = Code(o1);
    address_t addr, addr2;
    Error *v = Error(access_check(v1, epoint));
    if (v != NULL) return v;
    addr2 = code_address(v1);
    addr = addr2 & all_mem;
    *uv = addr;
    if (bits >= sizeof(addr)*8 || (addr >> bits) == 0) {
        if (addr2 != addr) err_msg_addr_wrap(epoint);
        return NULL;
    }
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = get_star_value(addr, v1->typ);
    return v;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Code *v1 = Code(o1);
    if (v1->typ->obj->address(v1->typ) != A_NONE) {
        Error *v = new_error(ERROR______CANT_INT, epoint);
        v->u.obj = get_star_value(code_address(v1) & all_mem, v1->typ);
        return v;
    }
    return iaddress(o1, iv, bits, epoint);
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Code *v1 = Code(o1);
    if (v1->typ->obj->address(v1->typ) != A_NONE) {
        Error *v = new_error(ERROR______CANT_INT, epoint);
        v->u.obj = get_star_value(code_address(v1) & all_mem, v1->typ);
        return v;
    }
    return uaddress(o1, uv, bits, epoint);
}

static FAST_CALL uint32_t address(const Obj *o1) {
    const Code *v1 = Code(o1);
    Obj *v = v1->typ;
    return v->obj->address(v);
}

MUST_CHECK Obj *float_from_code(const Code *v1, linepos_t epoint) {
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = float_from_obj(v, epoint);
    val_destroy(v);
    return result;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    Code *v1 = Code(o1);
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = v->obj->sign(v, epoint);
    val_destroy(v);
    return result;
}

static MUST_CHECK Obj *function(oper_t op) {
    Code *v1 = Code(op->v2);
    Obj *v, *result;
    v = access_check(v1, op->epoint2);
    if (v != NULL) return v;
    op->v2 = v = get_code_address(v1, op->epoint2);
    op->inplace = v->refcount == 1 ? v : NULL;
    result = v->obj->function(op);
    val_destroy(v);
    return result;
}

MUST_CHECK Obj *int_from_code(const Code *v1, linepos_t epoint) {
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = int_from_obj(v, epoint);
    val_destroy(v);
    return result;
}

static MUST_CHECK Obj *len(oper_t op) {
    address_t ln, s;
    Code *v1 = Code(op->v2);
    if (v1->pass == 0) {
        return ref_none();
    }
    if (v1->offs == 0) {
        s = v1->size;
    } else if (v1->offs > 0) {
        s = v1->size - (uval_t)v1->offs;
        if (s > v1->size) return Obj(new_error(ERROR_NEGATIVE_SIZE, op->epoint2));
    } else {
        if (add_overflow(-(uval_t)v1->offs, v1->size, &s)) err_msg_out_of_memory();
        if (diagnostics.size_larger) err_msg_size_larger(op->epoint2);
    }
    ln = (v1->dtype < 0) ? (address_t)-v1->dtype : (address_t)v1->dtype;
    return int_from_size((ln != 0) ? (s / ln) : s);
}

static MUST_CHECK Obj *size(oper_t op) {
    address_t s;
    Code *v1 = Code(op->v2);
    if (v1->pass == 0) {
        return ref_none();
    }
    if (v1->offs == 0) {
        s = v1->size;
    } else if (v1->offs > 0) {
        s = v1->size - (uval_t)v1->offs;
        if (s > v1->size) return Obj(new_error(ERROR_NEGATIVE_SIZE, op->epoint2));
    } else {
        if (add_overflow(-(uval_t)v1->offs, v1->size, &s)) err_msg_out_of_memory();
        if (diagnostics.size_larger) err_msg_size_larger(op->epoint2);
    }
    return int_from_size(s);
}

MUST_CHECK Obj *bits_from_code(const Code *v1, linepos_t epoint) {
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = bits_from_obj(v, epoint);
    val_destroy(v);
    return result;
}

MUST_CHECK Obj *bytes_from_code(const Code *v1, linepos_t epoint) {
    Obj *v, *result;
    v = access_check(v1, epoint);
    if (v != NULL) return v;
    v = get_code_address(v1, epoint);
    result = bytes_from_obj(v, epoint);
    val_destroy(v);
    return result;
}

struct code_item_s {
    const Code *v1;
    address_t offs2;
    ival_t offs0;
    address_t ln2;
};

static MUST_CHECK Obj *code_item(const struct code_item_s *ci) {
    int r;
    address_t i2;
    address_t offs;
    uval_t val;

    if (ci->offs0 < 0) {
        if (ci->offs2 < -(uval_t)ci->offs0) return ref_gap();
        offs = ci->offs2 - -(uval_t)ci->offs0;
    } else {
        offs = ci->offs2 + (uval_t)ci->offs0;
    }
    offs *= ci->ln2;
    r = -1;
    val = 0;
    for (i2 = 0; i2 < ci->ln2; i2++, offs++) {
        r = read_mem(ci->v1->memblocks, ci->v1->memaddr, ci->v1->membp, offs);
        if (r < 0) return ref_gap();
        val |= (uval_t)r << (i2 * 8);
    }
    if (ci->v1->dtype < 0 && (r & 0x80) != 0) {
        for (; i2 < sizeof val; i2++) val |= (uval_t)0xff << (i2 * 8);
    }
    return (ci->v1->dtype < 0) ? int_from_ival((ival_t)val) : int_from_uval(val);
}

static address_t code_item_prepare(struct code_item_s *ci, const Code *v1) {
    address_t ln, ln2 = (v1->dtype < 0) ? (address_t)-v1->dtype : (address_t)v1->dtype;
    if (ln2 == 0) ln2 = 1;
    ci->ln2 = ln2;
    ci->v1 = v1;

    if (v1->offs >= 0) {
        ci->offs0 = (ival_t)(((uval_t)v1->offs + ln2 - 1) / ln2);
        if (v1->size < (uval_t)v1->offs) return 0;
        return (v1->size - (uval_t)v1->offs) / ln2;
    }
    ci->offs0 = -(ival_t)((-(uval_t)v1->offs + ln2 - 1) / ln2);
    if (add_overflow(-(uval_t)v1->offs, v1->size, &ln)) err_msg_out_of_memory();
    return ln / ln2;
}

MUST_CHECK Obj *tuple_from_code(const Code *v1, const Type *typ) {
    address_t ln;
    List *v;
    Obj **vals;
    struct code_item_s ci;

    ln = code_item_prepare(&ci, v1);

    if (ln == 0) {
        return val_reference(typ == TUPLE_OBJ ? null_tuple : null_list);
    }

    v = List(val_alloc(typ));
    v->len = ln;
    v->data = vals = list_create_elements(v, ln);
    for (ci.offs2 = 0; ci.offs2 < ln; ci.offs2++) {
        vals[ci.offs2] = code_item(&ci);
    }
    return Obj(v);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    Obj **vals;
    Code *v1 = Code(op->v1);
    Obj *err;
    Funcargs *args = Funcargs(op->v2);
    struct code_item_s ci;
    struct indexoffs_s io;

    if (args->len < 1 || args->len - 1 > indx) {
        return new_error_argnum(args->len, 1, indx + 1, op->epoint2);
    }
    io.len = code_item_prepare(&ci, v1);
    io.epoint = &args->val[indx].epoint;
    io.val = args->val[indx].val;

    if (io.val->obj->iterable) {
        struct iter_s iter;
        Tuple *v;
        size_t i;
        iter.data = io.val; io.val->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(null_tuple);
        }
        v = new_tuple(iter.len);
        vals = v->data;
        for (i = 0; i < iter.len && (io.val = iter.next(&iter)) != NULL; i++) {
            err = indexoffs(&io);
            if (err != NULL) {
                vals[i] = err;
                continue;
            }
            ci.offs2 = (address_t)io.offs;
            vals[i] = code_item(&ci);
        }
        v->len = i;
        iter_destroy(&iter);
        return Obj(v);
    }
    if (io.val->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;
        Tuple *v;
        size_t i;

        err = sliceparams(&s, &io);
        if (err != NULL) return err;

        if (s.length == 0) {
            return val_reference(null_tuple);
        }
        v = new_tuple(s.length);
        vals = v->data;
        ci.offs2 = 0;
        ci.offs0 += s.offset;
        for (i = 0; i < s.length; i++) {
            vals[i] = code_item(&ci);
            ci.offs0 += s.step;
        }
        return Obj(v);
    }
    err = indexoffs(&io);
    if (err != NULL) return err;

    ci.offs2 = (address_t)io.offs;
    return code_item(&ci);
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1;
    Code *v2 = Code(op->v2);
    address_t ln;
    Obj *result2;
    Obj *good;
    struct code_item_s ci;
    Oper_types oper;

    switch (o1->obj->type) {
    case T_SYMBOL:
    case T_ANONSYMBOL:
        op->v2 = Obj(v2->names);
        return v2->names->v.obj->contains(op);
    case T_NONE:
    case T_ERROR:
        return val_reference(o1);
    case T_GAP:
        o1 = val_reference(o1);
        break;
    default:
        o1 = int_from_obj(o1, op->epoint);
        if (o1->obj != INT_OBJ) return o1;
        break;
    }
    ln = code_item_prepare(&ci, v2);

    oper = op->op;
    good = (oper == O_IN) ? false_value : true_value;
    result2 = val_reference(good);
    if (ln != 0) {
        Obj *result, *bad = (oper == O_IN) ? true_value : false_value;
        op->op = (oper == O_IN) ? O_EQ : O_NE;
        for (ci.offs2 = 0; ci.offs2 < ln; ci.offs2++) {
            Obj *tmp = code_item(&ci);
            op->v1 = tmp;
            op->v2 = o1;
            op->inplace = NULL;
            result = tmp->obj->calc2(op);
            val_destroy(tmp);
            if (result == good) {
                val_destroy(result);
                continue;
            }
            val_destroy(result2);
            result2 = result;
            if (result == bad) {
                break;
            }
        }
        op->op = oper;
    }
    val_destroy(o1);
    return result2;
}

static inline address_t ldigit(Code *v1, linepos_t epoint) {
    address_t addr2 = code_address(v1);
    address_t addr = addr2 & all_mem;
    if (addr2 != addr) err_msg_addr_wrap(epoint);
    return addr;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *v, *result;
    Code *v1 = Code(op->v1);
    switch (op->op) {
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        v = get_code_address(v1, op->epoint);
        op->v1 = v;
        op->inplace = (op->inplace == Obj(v1) && v->refcount == 1) ? v : NULL;
        result = op->v1->obj->calc1(op);
        val_destroy(v);
        return result;
    case O_BANK:
        if (all_mem < 0xffffff) return bits_calc1(op->op, ldigit(v1, op->epoint));
        FALL_THROUGH; /* fall through */
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        return bits_calc1(op->op, code_address(v1) & all_mem);
    case O_STRING:
    case O_INV:
    case O_NEG:
    case O_POS:
        v = access_check(v1, op->epoint);
        if (v != NULL) return v;
        v = get_code_address(v1, op->epoint);
        op->v1 = v;
        op->inplace = (op->inplace == Obj(v1) && v->refcount == 1) ? v : NULL;
        result = op->v1->obj->calc1(op);
        val_destroy(v);
        return result;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Code *v1 = Code(op->v1), *v;
    Obj *o2 = op->v2;
    if (op->op == O_MEMBER) {
        if (o2->obj == SYMBOL_OBJ) {
            Symbol *v2 = Symbol(o2);
            if (v2->name.len == 10 && v2->name.data[0] == '_' && v2->name.data[1] == '_') {
                static const str_t of = {(const uint8_t *)"__offset__", 10};
                str_t cf;
                str_cfcpy(&cf, &v2->name);
                if (str_cmp(&cf, &of) == 0) {
                    if (diagnostics.case_symbol && str_cmp(&v2->name, &cf) != 0) err_msg_symbol_case(&v2->name, NULL, op->epoint2);
                    return int_from_uval(code_memaddress(v1) & all_mem2);
                }
            }
        }
        return namespace_member(op, v1->names);
    }
    if (op->op == O_X) {
        if (o2 == none_value || o2->obj == ERROR_OBJ) return val_reference(o2);
        return obj_oper_error(op);
    }
    if (op->op == O_LAND || op->op == O_LOR || op->op == O_LXOR) {
        Obj *result = truth(Obj(v1), TRUTH_BOOL, op->epoint);
        bool i;
        if (result->obj != BOOL_OBJ) return result;
        i = Bool(result)->value;
        val_destroy(result);
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        if (op->op == O_LXOR) return calc2_lxor(op, i);
        return val_reference(i != (op->op == O_LOR) ? o2 : Obj(v1));
    }
    switch (o2->obj->type) {
    case T_CODE:
        {
            Obj *tmp1, *tmp2, *result;
            Code *v2 = Code(o2);
            result = access_check(v1, op->epoint);
            if (result != NULL) return result;
            result = access_check(v2, op->epoint2);
            if (result != NULL) return result;
            if (op->op == O_SUB) {
                address_t addr1 = ldigit(v1, op->epoint);
                address_t addr2 = ldigit(v2, op->epoint2);
                return int_from_ival((ival_t)addr1 - (ival_t)addr2);
            }
            tmp1 = get_code_address(v1, op->epoint);
            tmp2 = get_code_address(v2, op->epoint2);
            op->v1 = tmp1;
            op->v2 = tmp2;
            op->inplace = (op->inplace == Obj(v1) && tmp1->refcount == 1) ? tmp1 : NULL;
            result = op->v1->obj->calc2(op);
            val_destroy(tmp2);
            val_destroy(tmp1);
            return result;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_STR:
    case T_BYTES:
    case T_ADDRESS:
    case T_REGISTER:
        {
            Obj *tmp, *result;
            switch (op->op) {
            case O_ADD:
            case O_SUB:
                {
                    bool inplace;
                    ival_t iv;
                    Error *err = o2->obj->ival(o2, &iv, 30, op->epoint2);
                    if (err != NULL) { val_destroy(Obj(err)); break; }
                    if (iv == 0) return val_reference(Obj(v1));
                    inplace = (op->inplace == Obj(v1));
                    if (inplace) {
                        v = Code(val_reference(Obj(v1)));
                    } else {
                        v = new_code();
                        memcpy(((unsigned char *)v) + sizeof(Obj), ((unsigned char *)v1) + sizeof(Obj), sizeof(Code) - sizeof(Obj));
                        v->memblocks = ref_memblocks(v1->memblocks);
                        v->names = ref_namespace(v1->names);
                        v->typ = val_reference(v1->typ);
                    }
                    if (op->op == O_ADD) { v->offs += iv; } else { v->offs -= iv; }
                    if (v->offs >= 1073741824) { err_msg2(ERROR__OFFSET_RANGE, o2, op->epoint2); v->offs = 1073741823; }
                    if (v->offs < -1073741824) { err_msg2(ERROR__OFFSET_RANGE, o2, op->epoint2); v->offs = -1073741824; }
                    return Obj(v);
                }
            default: break;
            }
            result = access_check(v1, op->epoint);
            if (result != NULL) return result;
            tmp = get_code_address(v1, op->epoint);
            op->v1 = tmp;
            op->inplace = (op->inplace == Obj(v1) && tmp->refcount == 1) ? tmp : NULL;
            result = op->v1->obj->calc2(op);
            val_destroy(tmp);
            return result;
        }
    default:
        return o2->obj->rcalc2(op);
    }
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Code *v2 = Code(op->v2), *v;
    Obj *o1 = op->v1;
    switch (o1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_ADDRESS:
        {
            Obj *tmp, *result;
            if (op->op == O_ADD) {
                ival_t iv;
                Error *err = o1->obj->ival(o1, &iv, 30, op->epoint);
                if (err != NULL) { val_destroy(Obj(err)); break; }
                v = new_code();
                memcpy(((unsigned char *)v) + sizeof(Obj), ((unsigned char *)v2) + sizeof(Obj), sizeof(Code) - sizeof(Obj));
                v->memblocks = ref_memblocks(v2->memblocks);
                v->names = ref_namespace(v2->names);
                v->typ = val_reference(v2->typ);
                v->offs += iv;
                if (v->offs >= 1073741824) { err_msg2(ERROR__OFFSET_RANGE, o1, op->epoint2); v->offs = 1073741823; }
                if (v->offs < -1073741824) { err_msg2(ERROR__OFFSET_RANGE, o1, op->epoint2); v->offs = -1073741824; }
                return Obj(v);
            }
            result = access_check(v2, op->epoint2);
            if (result != NULL) return result;
            tmp = get_code_address(v2, op->epoint2);
            op->v2 = tmp;
            op->inplace = NULL;
            result = o1->obj->calc2(op);
            val_destroy(tmp);
            return result;
        }
    default: break;
    }
    return obj_oper_error(op);
}

void codeobj_init(void) {
    Type *type = new_type(&obj, T_CODE, "code", sizeof(Code));
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->truth = truth;
    type->repr = repr;
    type->str = str;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->address = address;
    type->iaddress = iaddress;
    type->uaddress = uaddress;
    type->sign = sign;
    type->function = function;
    type->len = len;
    type->size = size;
    type->calc1 = calc1;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    type->slice = slice;
    type->contains = contains;
}

void codeobj_names(void) {
    new_builtin("code", val_reference(Obj(CODE_OBJ)));
}
