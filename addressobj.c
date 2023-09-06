/*
    $Id: addressobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "addressobj.h"
#include <string.h>
#include "error.h"
#include "eval.h"
#include "variables.h"
#include "arguments.h"
#include "instruction.h"

#include "boolobj.h"
#include "strobj.h"
#include "intobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "floatobj.h"
#include "bitsobj.h"
#include "bytesobj.h"
#include "registerobj.h"

static Type obj;

Type *const ADDRESS_OBJ = &obj;

static MUST_CHECK Obj *address_from_obj(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_BYTES:
    case T_STR:
        return new_address(val_reference(v1), A_NONE);
    case T_NONE:
    case T_ERROR:
    case T_ADDRESS: return val_reference(v1);
    default: break;
    }
    return new_error_conv(v1, ADDRESS_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return address_from_obj(op->v2, op->epoint2);
}

static FAST_CALL void destroy(Obj *o1) {
    Address *v1 = Address(o1);
    val_destroy(v1->val);
}

static FAST_CALL void garbage(Obj *o1, int i) {
    Address *v1 = Address(o1);
    Obj *v;
    switch (i) {
    case -1:
        v1->val->refcount--;
        return;
    case 0:
        return;
    case 1:
        v = v1->val;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Address *v1 = Address(o1), *v2 = Address(o2);
    return o1->obj == o2->obj && v1->type == v2->type && v1->val->obj->same(v1->val, v2->val);
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->truth(o1, type, epoint);
    }
    v = v1->val;
    return v->obj->truth(v, type, epoint);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v = v1->val;
    Obj *err = v->obj->hash(v, hs, epoint);
    if (err == NULL) {
        *hs = ((unsigned int)*hs + v1->type) & ((~0U) >> 1);
    }
    return err;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Address *v1 = Address(o1);
    uint8_t *s;
    size_t len, chars;
    char buffer[100], buffer2[100], *b2;
    atype_t addrtype;
    unsigned int ind, ind2;
    Obj *tmp;
    Str *v, *str;

    ind2 = 0;
    addrtype = v1->type;
    ind = 99;
    buffer2[ind] = '\0';
    if (addrtype == A_NONE) {
        ind -= 8;
        memcpy(&buffer2[ind], "address(", 8);
        buffer[ind2++] = ')';
    } else {
        while ((addrtype & MAX_ADDRESS_MASK) != 0) {
            uint32_t mode;
            switch ((Address_types)((addrtype & 0xf000) >> 12)) {
            case A_XR: mode = (',' << 16) + ('x' << 24);break;
            case A_YR: mode = (',' << 16) + ('y' << 24);break;
            case A_ZR: mode = (',' << 16) + ('z' << 24);break;
            case A_SR: mode = (',' << 16) + ('s' << 24);break;
            case A_RR: mode = (',' << 16) + ('r' << 24);break;
            case A_DR: mode = (',' << 16) + ('d' << 24);break;
            case A_BR: mode = (',' << 16) + ('b' << 24);break;
            case A_KR: mode = (',' << 16) + ('k' << 24);break;
            case A_I:  mode = ('(' << 8) + (')' << 16);break;
            case A_LI: mode = ('[' << 8) + (']' << 16);break;
            case A_IMMEDIATE_SIGNED: mode = ('#' << 8) + '+';break;
            case A_IMMEDIATE: mode = '#' << 8;break;
            default: mode = 0;
            }
            if ((char)mode != '\0') buffer2[--ind] = (char)mode;
            mode >>= 8;
            if ((char)mode != '\0') buffer2[--ind] = (char)mode;
            mode >>= 8;
            if ((char)mode != '\0') buffer[ind2++] = (char)mode;
            mode >>= 8;
            if ((char)mode != '\0') buffer[ind2++] = (char)mode;
            addrtype <<= 4;
        }
    }
    b2 = buffer2 + ind;
    ind = 99 - ind;

    chars = ind + ind2;
    if (chars > maxsize) return NULL;
    tmp = v1->val->obj->repr(v1->val, epoint, maxsize - chars);
    if (tmp == NULL || tmp->obj != STR_OBJ) return tmp;
    str = Str(tmp);
    if (add_overflow(str->len, chars, &len)) goto error;
    chars += str->chars;
    if (chars > maxsize) {
    error:
        val_destroy(tmp);
        return NULL;
    }

    if (ind != 0 && str->len != 0 && b2[ind - 1] == '+' && str->data[0] == '-') {
        ind--; len--; chars--;
    }
    v = new_str2(len);
    if (v == NULL) goto error;
    v->chars = chars;
    s = v->data;
    if (ind != 0) {
        memcpy(s, b2, ind);
        s += ind;
    }
    if (str->len != 0) {
        memcpy(s, str->data, str->len);
        s += str->len;
    }
    if (ind2 != 0) memcpy(s, buffer, ind2);
    val_destroy(tmp);
    return Obj(v);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t epoint, size_t maxsize) {
    Address *v1 = Address(o1);
    if (v1->type == A_NONE) {
        o1 = v1->val;
        return o1->obj->repr(o1, epoint, maxsize);
    }
    return repr(o1, epoint, maxsize);
}

bool check_addr(atype_t type) {
    while (type != A_NONE) {
        switch ((Address_types)(type & 0xf)) {
        case A_I:
        case A_LI: return true;
        case A_IMMEDIATE:
        case A_IMMEDIATE_SIGNED:
        case A_KR:
        case A_DR:
        case A_BR:
        case A_XR:
        case A_YR:
        case A_ZR:
        case A_RR:
        case A_SR:
        case A_NONE: break;
        }
        type >>= 4;
    }
    return false;
}

static inline bool check_addr2(atype_t type) {
    while (type != A_NONE) {
        switch ((Address_types)(type & 0xf)) {
        case A_KR:
        case A_DR:
        case A_BR:
        case A_XR:
        case A_YR:
        case A_ZR:
        case A_RR:
        case A_SR:
        case A_I:
        case A_LI: return true;
        case A_IMMEDIATE:
        case A_IMMEDIATE_SIGNED:
        case A_NONE: break;
        }
        type >>= 4;
    }
    return false;
}

Address_types register_to_indexing(unsigned int c) {
    switch (c) {
    case 's': return A_SR;
    case 'r': return A_RR;
    case 'z': return A_ZR;
    case 'y': return A_YR;
    case 'x': return A_XR;
    case 'd': return A_DR;
    case 'b': return A_BR;
    case 'k': return A_KR;
    default: return A_NONE;
    }
}

static FAST_CALL uint32_t address(const Obj *o1) {
    const Address *v1 = Address(o1);
    Obj *v = v1->val;
    uint32_t am = v->obj->address(v);
    atype_t type = v1->type;
    while (type != A_NONE) {
        am <<= 4;
        type >>= 4;
    }
    return am | v1->type;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->ival(o1, iv, bits, epoint);
    }
    v = v1->val;
    return v->obj->ival(v, iv, bits, epoint);
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->uval(o1, uv, bits, epoint);
    }
    v = v1->val;
    return v->obj->uval(v, uv, bits, epoint);
}

static MUST_CHECK Error *uval2(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->uval2(o1, uv, bits, epoint);
    }
    v = v1->val;
    return v->obj->uval2(v, uv, bits, epoint);
}

static MUST_CHECK Error *iaddress(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v = v1->val;
    return v->obj->iaddress(v, iv, bits, epoint);
}

static MUST_CHECK Error *uaddress(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v = v1->val;
    return v->obj->uaddress(v, uv, bits, epoint);
}

MUST_CHECK Obj *float_from_address(Address *v1, linepos_t epoint) {
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return new_error_conv(Obj(v1), FLOAT_OBJ, epoint);
    }
    return float_from_obj(v1->val, epoint);
}

MUST_CHECK Obj *int_from_address(Address *v1, linepos_t epoint) {
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return new_error_conv(Obj(v1), INT_OBJ, epoint);
    }
    return int_from_obj(v1->val, epoint);
}

MUST_CHECK Obj *bits_from_address(Address *v1, linepos_t epoint) {
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return new_error_conv(Obj(v1), BITS_OBJ, epoint);
    }
    return bits_from_obj(v1->val, epoint);
}

MUST_CHECK Obj *bytes_from_address(Address *v1, linepos_t epoint) {
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return new_error_conv(Obj(v1), BYTES_OBJ, epoint);
    }
    return bytes_from_obj(v1->val, epoint);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    Address *v1 = Address(o1);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->sign(o1, epoint);
    }
    v = v1->val;
    return v->obj->sign(v, epoint);
}

static MUST_CHECK Obj *function(oper_t op) {
    Address *v1 = Address(op->v2);
    Obj *v;
    if (v1->type != A_NONE && v1->val != none_value && v1->val->obj != ERROR_OBJ) {
        return DEFAULT_OBJ->function(op);
    }
    op->v2 = v = v1->val;
    op->inplace = op->inplace == Obj(v1) && v->refcount == 1 ? v : NULL;
    return v->obj->function(op);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Obj *result;
    Address *v1 = Address(op->v1);
    atype_t am = v1->type;
    switch (op->op) {
    case O_LNOT:
    case O_STRING:
        if (am != A_NONE) break;
        FALL_THROUGH; /* fall through */
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV:
    case O_NEG:
    case O_POS:
        if (check_addr2(am)) break;
        op->v1 = v1->val;
        op->inplace = NULL;
        result = op->v1->obj->calc1(op);
        if (am == A_NONE) return result;
        return new_address(result, am);
    default: break;
    }
    if (v1->val == none_value || v1->val->obj == ERROR_OBJ) return val_reference(v1->val);
    return obj_oper_error(op);
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    Obj *val = Address(op->v1)->val;
    if (val == none_value || val->obj == ERROR_OBJ) {
        return val_reference(val);
    }
    return DEFAULT_OBJ->slice(op, indx);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2, *result;
    Address *v1 = Address(op->v1);
    atype_t am;
    if (op->op == O_LAND || op->op == O_LOR || op->op == O_LXOR) {
        bool i;
        result = truth(Obj(v1), TRUTH_BOOL, op->epoint);
        if (result->obj != BOOL_OBJ) return result;
        i = Bool(result)->value;
        val_destroy(result);
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        if (op->op == O_LXOR) return calc2_lxor(op, i);
        return val_reference(i != (op->op == O_LOR) ? o2 : Obj(v1));
    }
    switch (o2->obj->type) {
    case T_ADDRESS:
        {
            Address *v2 = Address(o2);
            atype_t am2 = v2->type;
            am = v1->type;
            switch (op->op) {
            case O_CMP:
            case O_EQ:
            case O_NE:
            case O_MIN:
            case O_LT:
            case O_LE:
            case O_MAX:
            case O_GT:
            case O_GE:
                if (am == am2) {
                    op->v1 = v1->val;
                    op->v2 = v2->val;
                    op->inplace = NULL;
                    return op->v1->obj->calc2(op);
                }
                return obj_oper_compare(op, (am < am2) ? -1 : 1);
            case O_ADD:
                if (check_addr(am)) break;
                if (check_addr(am2)) break;
                op->v1 = v1->val;
                op->v2 = v2->val;
                op->inplace = NULL;
                result = op->v1->obj->calc2(op);
                if (am == A_NONE && v2->type == A_NONE) return result;
                while ((Address_types)(am2 & 0xf) != A_NONE) { am <<= 4; am2 >>= 4; }
                return new_address(result, am | v2->type);
            case O_SUB:
                if (check_addr(am)) break;
                if (check_addr(am2)) break;
                {
                    atype_t am1 = A_NONE;
                    for (; (am & MAX_ADDRESS_MASK) != 0; am <<= 4) {
                        atype_t amc = (am >> 12) & 0xf;
                        atype_t am3, am4;
                        if (amc == A_NONE) continue;
                        am3 = A_NONE; am4 = am2;
                        while (am4 != A_NONE) {
                            atype_t am5 = (am4 & 0xf);
                            if (amc == am5) amc = A_NONE;
                            else am3 = (am3 << 4) | am5;
                            am4 >>= 4;
                        }
                        am2 = am3;
                        if (amc == A_NONE) continue;
                        am1 = (am1 << 4) | amc;
                    }
                    if (am2 != A_NONE) break;
                    op->v1 = v1->val;
                    op->v2 = v2->val;
                    op->inplace = NULL;
                    result = op->v1->obj->calc2(op);
                    if (am1 == A_NONE) return result;
                    return new_address(result, am1);
                }
            case O_XOR:
                if (check_addr(am)) break;
                if (check_addr(am2)) break;
                if (am == am2) {
                    op->v1 = v1->val;
                    op->v2 = v2->val;
                    op->inplace = NULL;
                    return op->v1->obj->calc2(op);
                }
                break;
            default:
                break;
            }
            break;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_BYTES:
    case T_STR:
        am = v1->type;
        switch (op->op) {
        case O_CMP:
        case O_EQ:
        case O_NE:
        case O_MIN:
        case O_LT:
        case O_LE:
        case O_MAX:
        case O_GT:
        case O_GE:
            if (am == A_NONE || (am == A_DR && dpage == 0) || (am == A_BR && databank == 0)) {
                op->v1 = v1->val;
                op->inplace = NULL;
                return op->v1->obj->calc2(op);
            }
            break;
        default:
            if (am == A_NONE) {
                op->v1 = v1->val;
                op->inplace = NULL;
                return op->v1->obj->calc2(op);
            }
            if (check_addr2(am)) break;
            goto ok;
        case O_ADD:
        case O_SUB:
            if (check_addr(am)) break;
        ok:
            op->v1 = v1->val;
            op->inplace = NULL;
            return new_address(op->v1->obj->calc2(op), am);
        }
        break;
    case T_REGISTER:
        if (Register(op->v2)->len == 1) {
            Address_types am2 = register_to_indexing(Register(op->v2)->data[0]);
            if (am2 == A_NONE) break;
            if (op->op == O_ADD) {
                return new_address(val_reference(v1->val), v1->type << 4 | am2);
            }
            if (op->op == O_SUB) {
                atype_t am1 = A_NONE;
                for (am = v1->type; ; am >>= 4) {
                    atype_t amc = am & 0xf;
                    switch (amc) {
                    case A_I:
                    case A_LI:
                    case A_IMMEDIATE:
                    case A_IMMEDIATE_SIGNED:
                        if (am2 == A_NONE) {
                            am1 = (am1 >> 4) | (amc << 12);
                            continue;
                        }
                        break;
                    case A_KR:
                    case A_DR:
                    case A_BR:
                    case A_XR:
                    case A_YR:
                    case A_ZR:
                    case A_RR:
                    case A_SR:
                        if ((Address_types)amc == am2) {
                            am2 = A_NONE;
                            continue;
                        }
                        am1 = (am1 >> 4) | (amc << 12);
                        continue;
                    default: break;
                    }
                    break;
                }
                if (am2 != A_NONE) break;
                if (am1 != A_NONE) while ((am1 & 0xf) == A_NONE) am1 >>= 4;
                return new_address(val_reference(v1->val), am1);
            }
        }
        break;
    default:
        if (op->op != O_MEMBER && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        if (o2 == none_value || o2->obj == ERROR_OBJ) return val_reference(o2);
    }
    if (v1->val == none_value || v1->val->obj == ERROR_OBJ) return val_reference(v1->val);
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    const Type *t1 = op->v1->obj;
    Address *v2 = Address(op->v2);
    atype_t am;
    switch (t1->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_BYTES:
    case T_GAP:
        am = v2->type;
        switch (op->op) {
        default:
            if (am == A_NONE) {
                op->v2 = v2->val;
                op->inplace = NULL;
                return t1->calc2(op);
            }
            break;
        case O_MUL:
        case O_OR:
        case O_XOR:
        case O_AND:
            if (check_addr2(am)) break;
            goto ok;
        case O_ADD:
            if (check_addr(am)) break;
        ok:
            op->v2 = v2->val;
            op->inplace = NULL;
            return new_address(t1->calc2(op), am);
        }
        break;
    case T_CODE:
        return t1->calc2(op);
    case T_REGISTER:
        if (Register(op->v1)->len == 1) {
            am = register_to_indexing(Register(op->v1)->data[0]);
            if (am == A_NONE) break;
            if (op->op == O_ADD) {
                return new_address(val_reference(v2->val), v2->type << 4 | am);
            }
            if (op->op == O_SUB) {
                if (am == v2->type) {
                    op->v1 = int_value[0];
                    op->v2 = v2->val;
                    op->inplace = NULL;
                    return INT_OBJ->calc2(op);
                }
            }
        }
        break;
    default: break;
    }
    if (v2->val == none_value || v2->val->obj == ERROR_OBJ) return val_reference(v2->val);
    return obj_oper_error(op);
}

void addressobj_init(void) {
    Type *type = new_type(&obj, T_ADDRESS, "address", sizeof(Address));
    type->convert = convert;
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
    type->address = address;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval2;
    type->iaddress = iaddress;
    type->uaddress = uaddress;
    type->sign = sign;
    type->function = function;
    type->calc1 = calc1;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    type->slice = slice;
}

void addressobj_names(void) {
    new_builtin("address", val_reference(Obj(ADDRESS_OBJ)));
}
