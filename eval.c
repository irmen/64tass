/*
    $Id: eval.c 3122 2023-09-16 10:44:56Z soci $

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

#include "eval.h"
#include <string.h>
#include "math.h"
#include "section.h"
#include "variables.h"
#include "64tass.h"
#include "unicode.h"
#include "error.h"
#include "values.h"
#include "arguments.h"
#include "optimizer.h"
#include "unicodedata.h"

#include "floatobj.h"
#include "boolobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "strobj.h"
#include "codeobj.h"
#include "bytesobj.h"
#include "addressobj.h"
#include "listobj.h"
#include "dictobj.h"
#include "registerobj.h"
#include "namespaceobj.h"
#include "operobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "labelobj.h"
#include "errorobj.h"
#include "symbolobj.h"
#include "anonsymbolobj.h"
#include "foldobj.h"
#include "memblocksobj.h"
#include "functionobj.h"

static FAST_CALL NO_INLINE unsigned int get_label_start(const uint8_t *s) {
    unsigned int l;
    unichar_t ch;
    if (!arguments.to_ascii) return 0;
    l = utf8in(s, &ch);
    return ((uget_property(ch)->property & id_Start) != 0) ? l : 0;
}

static FAST_CALL NO_INLINE unsigned int get_label_continue(const uint8_t *s) {
    unsigned int l;
    unichar_t ch;
    if (!arguments.to_ascii) return 0;
    l = utf8in(s, &ch);
    return ((uget_property(ch)->property & (id_Continue | id_Start)) != 0) ? l : 0;
}

FAST_CALL size_t get_label(const uint8_t *s) {
    const uint8_t *i;
    if likely(((uint8_t)((*s | 0x20) - 'a')) <= 'z' - 'a' || *s == '_') {
        i = s + 1;
    } else {
        unsigned int l;
        if (*s < 0x80) return 0;
        l = get_label_start(s);
        if (l == 0) return 0;
        i = s + l;
    }
    for (;;) {
        unsigned int l;
        if likely(((uint8_t)((*i | 0x20) - 'a')) <= 'z' - 'a' || (*i ^ 0x30) < 10 || *i == '_') {
            i++;
            continue;
        }
        if (*i < 0x80) return (size_t)(i - s);
        l = get_label_continue(i);
        if (l == 0) return (size_t)(i - s);
        i += l;
    }
}

FAST_CALL size_t get_label2(const uint8_t *s, const uint8_t *e) {
    const uint8_t *i;
    if (s >= e) return 0;
    if likely(((uint8_t)((*s | 0x20) - 'a')) <= 'z' - 'a' || *s == '_') {
        i = s + 1;
    } else {
        unsigned int l;
        if (*s < 0x80) return 0;
        l = get_label_start(s);
        if (l == 0) return 0;
        i = s + l;
    }
    while (i < e) {
        unsigned int l;
        if likely(((uint8_t)((*i | 0x20) - 'a')) <= 'z' - 'a' || (*i ^ 0x30) < 10 || *i == '_') {
            i++;
            continue;
        }
        if (*i < 0x80) return (size_t)(i - s);
        l = get_label_continue(i);
        if (l == 0) return (size_t)(i - s);
        i += l;
    }
    return (size_t)(i - s);
}

static MUST_CHECK Obj *get_dec(linepos_t epoint) {
    linecpos_t len, len2;
    Obj *v = int_from_decstr(pline + lpoint.pos, &len, &len2);
    lpoint.pos += len;
    return (v != NULL) ? v : new_error_mem(epoint);
}

static double ldexp10(double d, unsigned int expo, bool neg) {
    static const double nums[10] = {1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9};
    double scal;
    if (expo < lenof(nums)) {
        scal = nums[expo];
    } else {
#ifdef __APPLE__
        scal = pow(10.0+d*0.0, (double)expo);
#else
        scal = pow(10.0, (double)expo);
#endif
    }
    return neg ? d / scal : d * scal;
}

static MUST_CHECK Obj *get_exponent(Obj *v1, Obj *v2, size_t len, linepos_t epoint) {
    Obj *v;
    double real;
    uval_t expo = 0;
    bool neg = false;
    uint8_t base = here() | 0x20;

    if (base == 'p' || base == 'e') {
        neg = (pline[lpoint.pos + 1] == '-');
        if (neg || pline[lpoint.pos + 1] == '+') {
            if ((pline[lpoint.pos + 2] ^ 0x30) < 10) lpoint.pos++;
        }
        if ((pline[lpoint.pos + 1] ^ 0x30) < 10) {
            Error *err;
            linecpos_t len1, len2;
            lpoint.pos++;

            v = int_from_decstr(pline + lpoint.pos, &len1, &len2);
            if (v == NULL) {
                err = Error(new_error_mem(epoint));
            } else {
                err = v->obj->uval(v, &expo, 8 * (sizeof expo < sizeof(int) ? sizeof expo : sizeof(int)) - 1, &lpoint);
                val_destroy(v);
            }
            lpoint.pos += len1;
            if (err != NULL) {
                if (v2 != NULL) val_destroy(v2);
                val_destroy(v1);
                return Obj(err);
            }
        }
    }
    if (v2 == NULL) {
        real = 0.0;
    } else {
        bool bits = v2->obj == BITS_OBJ;
        if (bits) {
            len = Bits(v2)->bits;
        }
        v = float_from_obj(v2, epoint);
        val_destroy(v2);
        if (v->obj != FLOAT_OBJ) {
            val_destroy(v1);
            return v;
        }
        real = Float(v)->real;
        if (len != 0 && real != 0.0) real = bits ? ldexp(real, -(int)len) : ldexp10(real, len <= ~(unsigned int)0 ? (unsigned int)len : ~(unsigned int)0, true);
        val_destroy(v);
    }
    v = float_from_obj(v1, epoint);
    val_destroy(v1);
    if (v->obj != FLOAT_OBJ) {
        return v;
    }
    real += Float(v)->real;
    if (expo != 0) {
        real = (base == 'p') ? ldexp(real, neg ? -(ival_t)expo : (ival_t)expo) : ldexp10(real, expo, neg);
    }
    if (real == HUGE_VAL || real == -HUGE_VAL || real != real) {
        val_destroy(v);
        return Obj(new_error(ERROR_NUMERIC_OVERF, epoint));
    }
    if (v->refcount == 1) {
        Float(v)->real = real;
        return v;
    }
    val_destroy(v);
    return new_float(real);
}

static MUST_CHECK Obj *get_exponent2(Obj *v, linepos_t epoint) {
    if (pline[lpoint.pos + 1] == '-' || pline[lpoint.pos + 1] == '+') {
        if ((pline[lpoint.pos + 2] ^ 0x30) < 10) {
            return get_exponent(v, NULL, 0, epoint);
        }
    } else if ((pline[lpoint.pos + 1] ^ 0x30) < 10) {
        return get_exponent(v, NULL, 0, epoint);
    }
    return v;
}

static MUST_CHECK Obj *get_hex(linepos_t epoint) {
    linecpos_t len;
    Obj *v = bits_from_hexstr(pline + lpoint.pos + 1, &len);
    if (v == NULL) v = new_error_mem(epoint);
    lpoint.pos += len + 1;
    if (here() == '.' && pline[lpoint.pos + 1] != '.') {
        Obj *v2 = bits_from_hexstr(pline + lpoint.pos + 1, &len);
        lpoint.pos += len + 1;
        return get_exponent(v, (v2 != NULL) ? v2 : new_error_mem(epoint), 0, epoint);
    }
    return (here() | 0x20) == 'p' ? get_exponent2(v, epoint) : v;
}

static MUST_CHECK Obj *get_hex_compat(linepos_t epoint) {
    linecpos_t len;
    Obj *v = bits_from_hexstr(pline + lpoint.pos + 1, &len);
    lpoint.pos += len + 1;
    return (v != NULL) ? v : new_error_mem(epoint);
}

static MUST_CHECK Obj *get_bin(linepos_t epoint) {
    linecpos_t len;
    Obj *v = bits_from_binstr(pline + lpoint.pos + 1, &len);
    if (v == NULL) v = new_error_mem(epoint);
    lpoint.pos += len + 1;
    if (here() == '.' && pline[lpoint.pos + 1] != '.') {
        Obj *v2 = bits_from_binstr(pline + lpoint.pos + 1, &len);
        lpoint.pos += len + 1;
        return get_exponent(v, (v2 != NULL) ? v2 : new_error_mem(epoint), 0, epoint);
    }
    switch (here() | 0x20) {
    case 'e':
    case 'p':
        return get_exponent2(v, epoint);
    default:
        return v;
    }
}

static MUST_CHECK Obj *get_bin_compat(linepos_t epoint) {
    linecpos_t len;
    Obj *v = bits_from_binstr(pline + lpoint.pos + 1, &len);
    lpoint.pos += len + 1;
    return (v != NULL) ? v : new_error_mem(epoint);
}

static MUST_CHECK Obj *get_float(linepos_t epoint) {
    linecpos_t len, len2;
    Obj *v = int_from_decstr(pline + lpoint.pos, &len, &len2);
    if (v == NULL) v = new_error_mem(epoint);
    lpoint.pos += len;
    if (here() == '.' && pline[lpoint.pos + 1] != '.') {
        Obj *v2 = int_from_decstr(pline + lpoint.pos + 1, &len, &len2);
        lpoint.pos += len + 1;
        return get_exponent(v, (v2 != NULL) ? v2 : new_error_mem(epoint), len2, epoint);
    }
    switch (here() | 0x20) {
    case 'e':
    case 'p':
        return get_exponent2(v, epoint);
    default:
        return v;
    }
}

static MUST_CHECK Obj *get_bytes(linepos_t epoint, bool z85) {
    char txt[4];
    linecpos_t len;
    Obj *v;
    if (z85) {
        v = bytes_from_z85str(pline + lpoint.pos, &len, epoint);
    } else {
        v = bytes_from_hexstr(pline + lpoint.pos, &len, epoint);
    }
    if (v->obj == BYTES_OBJ) {
        lpoint.pos += len;
        return v;
    }
    txt[1] = (char)here();
    txt[2] = txt[0] = txt[1] ^ ('\'' ^ '"');
    txt[3] = 0;
    lpoint.pos += len;
    if (pline[lpoint.pos - 1] != txt[1] || len == 1) err_msg2(ERROR______EXPECTED, txt, &lpoint);
    return v;
}

static MUST_CHECK Obj *get_string(linepos_t epoint) {
    char txt[4];
    linecpos_t len;
    Obj *v = str_from_str(pline + lpoint.pos, &len, epoint);
    if (v->obj == STR_OBJ) {
        lpoint.pos += len;
        return v;
    }
    txt[1] = (char)here();
    txt[2] = txt[0] = txt[1] ^ ('\'' ^ '"');
    txt[3] = 0;
    lpoint.pos += len;
    err_msg2(ERROR______EXPECTED, txt, &lpoint);
    return v;
}

void touch_label(Label *tmp) {
    if (referenceit) tmp->ref = true;
    tmp->usepass = pass;
}

static uval_t bitscalc(address_t addr, Bits *val) {
    size_t b = val->bits;
    if (b >= 8 * sizeof(addr)) return (uval_t)b;
    if ((addr >> b) == 0) return (uval_t)b;
    if (addr <= 0xff) return 8;
    if (addr <= 0xffff) return 16;
    return all_mem_bits;
}

static uval_t bytescalc(address_t addr, Bytes *val) {
    size_t b = val->len < 0 ? (size_t)~val->len : (size_t)val->len;
    if (b >= 8 * sizeof(addr)) return (uval_t)b;
    if ((addr >> (b << 3)) == 0) return (uval_t)b;
    if (addr <= 0xff) return 1;
    if (addr <= 0xffff) return 2;
    return all_mem_bits >> 3;
}

MUST_CHECK Obj *get_star_value(address_t addr, Obj *val) {
    switch (val->obj->type) {
    case T_BITS: return bits_from_uval(addr, bitscalc(addr, Bits(val)));
    case T_CODE: return get_star_value(addr, Code(val)->typ);
    default:
    case T_BOOL:
    case T_INT: return int_from_uval(addr);
    case T_FLOAT: return new_float(addr + (Float(val)->real - trunc(Float(val)->real)));
    case T_STR: return bytes_from_uval(addr, all_mem_bits >> 3);
    case T_BYTES: return bytes_from_uval(addr, bytescalc(addr, Bytes(val)));
    case T_ADDRESS: return new_address(get_star_value(addr, Address(val)->val), Address(val)->type);
    }
}

MUST_CHECK Obj *get_star(void) {
    Code *code;
    if (diagnostics.optimize) cpu_opt_invalidate();
    code = new_code();
    code->addr = star;
    code->typ = val_reference(current_address->l_address_val);
    code->size = 0;
    code->offs = 0;
    code->dtype = D_NONE;
    code->pass = pass;
    code->apass = pass;
    code->memblocks = ref_memblocks(current_address->mem);
    code->names = ref_namespace(current_context);
    code->requires = current_section->requires;
    code->conflicts = current_section->conflicts;
    code->memaddr = current_address->address;
    code->membp = 0;
    return Obj(code);
}

static MUST_CHECK Obj *new_starsymbol(linecpos_t pos) {
    static const str_t symbol = { (const uint8_t *)"*", 1 };
    struct linepos_s epoint;
    epoint.line = lpoint.line;
    epoint.pos = pos;
    return new_symbol(&symbol, &epoint);
}

static MUST_CHECK Obj *resolv_anonlabel(ssize_t as, linecpos_t pos) {
    Error *err;
    struct linepos_s epoint;
    Label *l = find_anonlabel(as);
    if (l != NULL) {
        touch_label(l);
        return val_reference(l->value);
    }
    if (constcreated && pass < max_pass) {
        return ref_none();
    }
    epoint.line = lpoint.line;
    epoint.pos = pos;
    err = new_error(ERROR___NOT_DEFINED, &epoint);
    err->u.notdef.symbol = new_anonsymbol(as);
    err->u.notdef.names = ref_namespace(current_context);
    err->u.notdef.down = true;
    return Obj(err);
}

struct opr_s {
    Oper_types op;
    linecpos_t pos;
};

struct opr_list_s {
    struct opr_s *data, *p, *max;
};

struct out_s {
    Obj *val;
    linecpos_t pos;
};

struct out_list_s {
    struct out_s *data, *p, *max;
};

static size_t evxnum, evx_p;
static struct eval_context_s {
    struct values_s *values;
    argcount_t values_len;
    argcount_t values_p;
    argcount_t values_size;
    int gstop;
    struct {
        struct out_s *data, *max;
        const struct out_s *end, *p;
    } out;
    struct {
        struct opr_s *data, *max;
    } opr;
} **evx;

static struct eval_context_s *eval;

static NO_INLINE void extend_out(struct out_list_s *out) {
    argcount_t size = (argcount_t)(out->max - out->data);
    argcount_t p = (argcount_t)(out->p - out->data);
    extend_array(&out->data, &size, 64);
    out->max = out->data + size;
    out->p = out->data + p;
}

static NO_INLINE void extend_opr(struct opr_list_s *opr) {
    argcount_t size = (argcount_t)(opr->max - opr->data);
    argcount_t p = (argcount_t)(opr->p - opr->data);
    extend_array(&opr->data, &size, 64);
    opr->max = opr->data + size;
    opr->p = opr->data + p;
}

static inline void clean_out(struct eval_context_s *ev) {
    const struct out_s *i;
    for (i = ev->out.p; i != ev->out.end; i++) val_destroy(i->val);
}

static bool get_exp_compat(int stop) {/* length in bytes, defined */
    uint8_t ch;

    Oper_types conv;
    struct linepos_s epoint;
    linecpos_t cpoint = 0, cpoint2 = 0;
    linecpos_t llen;
    bool first;
    str_t symbol;
    Label *l;
    struct out_list_s out;
    struct opr_list_s opr;

    out.p = out.data = eval->out.data;
    out.max = eval->out.max;
    opr.p = opr.data = eval->opr.data;
    opr.max = eval->opr.max;
rest:
    ignore();
    conv = O_NONE;
    first = (here() == '(') && (stop == 3 || stop == 4);
    if (out.p == out.data && here() == '#') {
        cpoint2 = lpoint.pos; lpoint.pos++;
    }
    switch (here()) {
    case 0:
    case ';':
        eval->opr.data = opr.data;
        eval->opr.max = opr.max;
        eval->out.data = out.data;
        eval->out.p = out.data;
        eval->out.max = out.max;
        eval->out.end = out.p;
        return true;
    case '<': conv = O_LOWER; cpoint = lpoint.pos; lpoint.pos++;break;
    case '>': conv = O_HIGHER; cpoint = lpoint.pos; lpoint.pos++;break;
    }
    epoint.line = lpoint.line;
    for (;;) {
        Oper_types op;
        Obj *val;
        ignore();ch = here(); epoint.pos = lpoint.pos;

        switch (ch) {
        case '(': op = O_PARENT;goto add;
        case '$': val = get_hex_compat(&epoint); goto push_other;
        case '%': val = get_bin_compat(&epoint); goto push_other;
        case '"': val = get_string(&epoint); goto push_other;
        case '*': lpoint.pos++;val = get_star(); goto push_other;
        case '0':
            if (diagnostics.leading_zeros && pline[lpoint.pos + 1] >= '0' && pline[lpoint.pos + 1] <= '9') err_msg2(ERROR_LEADING_ZEROS, NULL, &lpoint);
            FALL_THROUGH; /* fall through */
        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            val = get_dec(&epoint); goto push_other;
        default:
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            if (llen == 0) {
                if (opr.p != opr.data) epoint.pos = opr.p[-1].pos;
                if ((ch < ' ' && ch != 0) || ch > '~') err_msg_wrong_character(&lpoint);
                else err_msg2(ERROR______EXPECTED, "an expression is", &lpoint);
                goto error;
            }
            lpoint.pos += llen;
            break;
        }
    as_symbol:
        symbol.data = pline + epoint.pos;
        symbol.len = lpoint.pos - epoint.pos;
        l = find_label(&symbol, NULL);
        if (l != NULL) {
            if (diagnostics.case_symbol && str_cmp(&symbol, &l->name) != 0) err_msg_symbol_case(&symbol, l, &epoint);
            touch_label(l);
            val = val_reference(l->value);
        } else if (constcreated && pass < max_pass) {
            val = ref_none();
        } else {
            Error *err = new_error(ERROR___NOT_DEFINED, &epoint);
            err->u.notdef.symbol = new_symbol(&symbol, &epoint);
            err->u.notdef.names = ref_namespace(current_context);
            err->u.notdef.down = true;
            val = Obj(err);
        }
    push_other:
        out.p->val = val;
        out.p->pos = epoint.pos;
        if (++out.p == out.max) extend_out(&out);
    other:
        if (stop != 2) ignore();
        ch = here(); epoint.pos = lpoint.pos;

        while (opr.p != opr.data) {
            if (opr.p[-1].op == O_PARENT) break;
            opr.p--;
            out.p->val = &operators[opr.p->op].v;
            out.p->pos = opr.p->pos;
            if (++out.p == out.max) extend_out(&out);
        }
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            lpoint.pos += llen;
            if (llen == 1) {
                switch (pline[epoint.pos + 1] | arguments.caseinsensitive) {
                case 'x':
                    opr.p->op = O_COMMAX;
                    opr.p->pos = epoint.pos;
                    if (++opr.p == opr.max) extend_opr(&opr);
                    goto other;
                case 'y':
                    opr.p->op = O_COMMAY;
                    opr.p->pos = epoint.pos;
                    if (++opr.p == opr.max) extend_opr(&opr);
                    goto other;
                default: break;
                }
            }
            if (conv != O_NONE) {
                out.p->val = &operators[conv].v;
                out.p->pos = cpoint;
                if (++out.p == out.max) extend_out(&out);
            }
            if (cpoint2 != 0) {
                out.p->val = &operators[O_HASH].v;
                out.p->pos = cpoint2;
                if (++out.p == out.max) extend_out(&out);
            }
            if (stop == 1) {lpoint.pos = epoint.pos;break;}
            if (llen != 0) {
                epoint.pos++;
                goto as_symbol;
            }
            goto rest;
        case '&': op = O_AND; goto add;
        case '.': op = O_OR; goto add;
        case '(': /* yes, this is accepted by the original */
        case ':': op = O_XOR; goto add;
        case '*': op = O_MUL; goto add;
        case '/': op = O_DIV; goto add;
        case '+': op = O_ADD; goto add;
        case '-': op = O_SUB;
        add:
            lpoint.pos++;
            opr.p->op = op;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            continue;
        case ')':
            if (opr.p == opr.data) {err_msg2(ERROR______EXPECTED, "')' not", &lpoint); goto error;}
            lpoint.pos++;
            opr.p--;
            if (opr.p == opr.data && first) {
                opr.p->op = O_TUPLE;
                opr.p->pos = epoint.pos;
                opr.p++;
                first = false;
            }
            goto other;
        case 0:
        case ';':
        case '\t':
        case ' ':
            if (conv != O_NONE) {
                out.p->val = &operators[conv].v;
                out.p->pos = cpoint;
                if (++out.p == out.max) extend_out(&out);
            }
            if (cpoint2 != 0) {
                out.p->val = &operators[O_HASH].v;
                out.p->pos = cpoint2;
                if (++out.p == out.max) extend_out(&out);
            }
            break;
        default:
            if (ch < ' ' || ch > '~') err_msg_wrong_character(&epoint);
            else err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        }
        if (opr.p == opr.data) {
            eval->opr.data = opr.data;
            eval->opr.max = opr.max;
            eval->out.data = out.data;
            eval->out.p = out.data;
            eval->out.max = out.max;
            eval->out.end = out.p;
            return true;
        }
        epoint.pos = opr.p[-1].pos;
        err_msg2(ERROR______EXPECTED, "')'", &epoint);
    error:
        break;
    }
    eval->opr.data = opr.data;
    eval->opr.max = opr.max;
    eval->out.data = out.data;
    eval->out.p = out.data;
    eval->out.max = out.max;
    eval->out.end = out.p;
    return false;
}

static struct values_s *extend_values(struct eval_context_s *ev, size_t by) {
    argcount_t j = ev->values_size;
    ev->values_size += (argcount_t)by;
    if (ev->values_size < by) err_msg_out_of_memory(); /* overflow */
    resize_array(&ev->values, ev->values_size);
    for (; j < ev->values_size; j++) ev->values[j].val = NULL;
    return ev->values;
}

static bool get_val2_compat(struct eval_context_s *ev) {/* length in bytes, defined */
    argcount_t vsp = 0;
    Oper_types op;
    const struct out_s *out;
    Error *err;
    struct values_s *values;
    struct linepos_s epoint;

    ev->values_p = 0;
    values = ev->values;

    epoint.line = lpoint.line;
    for (out = ev->out.p; out != ev->out.end; out++) {
        struct values_s *v;
        Obj *val = out->val;
        if (val->obj != OPER_OBJ) {
            if (vsp >= ev->values_size) values = extend_values(ev, 16);
            val = values[vsp].val;
            if (val != NULL) val_destroy(val);
            values[vsp].val = out->val;
            values[vsp].epoint.pos = out->pos;
            values[vsp++].epoint.line = epoint.line;
            continue;
        }

        op = Oper(val)->op;

        if (vsp < 1) goto syntaxe;
        v = &values[vsp - 1];
        switch (op) {
        case O_LOWER:
        case O_HIGHER:
        case O_HASH:
        case O_COMMAX:
        case O_COMMAY:
        case O_TUPLE:
            switch (v[0].val->obj->type) {
            case T_ADDRESS:
                switch (op) {
                case O_COMMAX:
                case O_COMMAY:
                case O_TUPLE:
                    {
                        const Address *old = Address(v[0].val);
                        Obj *val2 = new_address(val_reference(old->val), (old->type << 4) | ((op == O_TUPLE) ? A_I : (op == O_COMMAX) ? A_XR : A_YR));
                        val_destroy(v[0].val); v[0].val = val2;
                        v[0].epoint.pos = out->pos;
                        continue;
                    }
                default:break;
                }
                epoint.pos = out->pos;
                err = new_error(ERROR__INVALID_OPER, &epoint);
                err->u.invoper.op = op;
                err->u.invoper.v1 = v[0].val;
                err->u.invoper.v2 = NULL;
                v[0].val = Obj(err);
                break;
            default:
                {
                    uint16_t val1;
                    uval_t uval;
                    err = v[0].val->obj->uval(v[0].val, &uval, 8 * sizeof uval, &v[0].epoint);
                    if (err != NULL) {
                        val_destroy(v[0].val);
                        v[0].val = Obj(err);
                        break;
                    }
                    val1 = (uint16_t)uval;

                    switch (op) {
                    case O_HASH:
                    case O_COMMAX:
                    case O_COMMAY:
                        v[0].val = new_address(v[0].val, (op == O_HASH) ? A_IMMEDIATE : (op == O_COMMAX) ? A_XR : A_YR);
                        v[0].epoint.pos = out->pos;
                        continue;
                    case O_HIGHER:
                        val1 = (uint8_t)(val1 >> 8);
                        break;
                    case O_LOWER:
                        val1 = (uint8_t)val1;
                        break;
                    case O_TUPLE:
                        v[0].val = new_address(v[0].val, A_I);
                        v[0].epoint.pos = out->pos;
                        continue;
                    default: break;
                    }
                    val_destroy(v[0].val);
                    v[0].val = int_from_uval(val1);
                    break;
                }
            case T_ERROR:
            case T_NONE:break;
            }
            v[0].epoint.pos = out->pos;
            continue;
        default:break;
        }
        if (vsp < 2) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            ev->out.p = out + 1;
            ev->values_len = 0;
            return false;
        }
        vsp--;
        v--;
        switch (v[0].val->obj->type) {
        case T_INT:
        case T_BITS:
        case T_CODE:
        case T_STR:
        case T_ADDRESS:
            switch (v[1].val->obj->type) {
            case T_INT:
            case T_BITS:
            case T_CODE:
            case T_STR:
            case T_ADDRESS:
                {
                    uint16_t val1, val2;
                    uval_t uval;
                    err = v[0].val->obj->uval(v[0].val, &uval, 8 * sizeof uval, &v[0].epoint);
                    if (err != NULL) {
                        val_destroy(v[0].val);
                        v[0].val = Obj(err);
                        continue;
                    }
                    val1 = (uint16_t)uval;
                    err = v[1].val->obj->uval(v[1].val, &uval, 8 * sizeof uval, &v[1].epoint);
                    if (err != NULL) {
                        val_destroy(v[0].val);
                        v[0].val = Obj(err);
                        continue;
                    }
                    val2 = (uint16_t)uval;

                    switch (op) {
                    case O_MUL: val1 = (uint16_t)(val1 * val2); break;
                    case O_DIV:
                        if (val2 == 0) {
                            epoint.pos = out->pos;
                            val_destroy(v[0].val); v[0].val = new_error_obj(ERROR_DIVISION_BY_Z, v[1].val, &epoint);
                            continue;
                        }
                        val1 /= val2; break;
                    case O_ADD: val1 = (uint16_t)(val1 + val2); break;
                    case O_SUB: val1 = (uint16_t)(val1 - val2); break;
                    case O_AND: val1 &= val2; break;
                    case O_OR:  val1 |= val2; break;
                    case O_XOR: val1 ^= val2; break;
                    default: break;
                    }
                    val_destroy(v[0].val);
                    v[0].val = int_from_uval(val1);
                    continue;
                }
            default:
                epoint.pos = out->pos;
                break;
            case T_ERROR:
            case T_NONE:
                val_replace(&v[0].val, v[1].val);
                continue;
            }
            break;
        default:
            epoint.pos = out->pos;
            break;
        case T_ERROR:
        case T_NONE: continue;
        }
        err = new_error(ERROR__INVALID_OPER, &epoint);
        err->u.invoper.op = op;
        err->u.invoper.v1 = v[0].val;
        err->u.invoper.v2 = val_reference(v[1].val);
        v[0].val = Obj(err);
    }
    ev->out.p = out;
    ev->values_len = vsp;
    return true;
}

static MUST_CHECK Obj *apply_addressing(Obj *o1, atype_t addrtype, bool inplace) {
    if (o1->obj->iterable) {
        struct iter_s iter;
        size_t i;
        List *v;
        Obj **vals;

        if (o1->refcount != 1) inplace = false;

        iter.data = o1; o1->obj->getiter(&iter);
        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(o1->obj == TUPLE_OBJ ? null_tuple : null_list);
        }

        v = List(val_alloc(o1->obj == TUPLE_OBJ ? TUPLE_OBJ : LIST_OBJ));
        vals = list_create_elements(v, iter.len);
        for (i = 0; i < iter.len && (o1 = iter.next(&iter)) != NULL; i++) {
            vals[i] = apply_addressing(o1, addrtype, inplace);
        }
        iter_destroy(&iter);
        v->len = i;
        v->data = vals;
        return Obj(v);
    }
    if (o1->obj == ADDRESS_OBJ) {
        Address *v1 = Address(o1);
        if (inplace && o1->refcount == 1) {
            v1->type = addrtype | (v1->type << 4);
            return val_reference(o1);
        }
        return new_address(val_reference(v1->val), addrtype | (v1->type << 4));
    }
    return new_address(val_reference(o1), addrtype);
}

static bool get_val2(struct eval_context_s *ev) {
    argcount_t vsp = 0;
    const struct out_s *out;
    bool stop = (ev->gstop == 3 || ev->gstop == 4);
    struct values_s *values;
    struct oper_s oper;
    Address_types am;
    struct linepos_s epoint;

    ev->values_p = 0;
    values = ev->values;

    epoint.line = lpoint.line;
    oper.epoint3 = &epoint;
    for (out = ev->out.p; out != ev->out.end; out++) {
        struct values_s *v;
        Obj *val = out->val;
        if (val->obj != OPER_OBJ || Oper(val)->op == O_PARENT || Oper(val)->op == O_BRACKET || Oper(val)->op == O_BRACE) {
            if (vsp >= ev->values_size) values = extend_values(ev, 16);
            val = values[vsp].val;
            if (val != NULL) val_destroy(val);
            values[vsp].val = out->val;
            values[vsp].epoint.pos = out->pos;
            values[vsp++].epoint.line = epoint.line;
            continue;
        }

        if (Oper(val)->op == O_COMMA || Oper(val)->op == O_COLON2) continue;
        oper.op = Oper(val)->op;
        switch (oper.op) {
        case O_FUNC:
        case O_INDEX:
            {
                argcount_t args = vsp;
                Funcargs tmp;
                Oper_types op = (oper.op == O_FUNC) ? O_PARENT : O_BRACKET;
                if (vsp == 0) goto syntaxe;
                v = &values[vsp - 1];
                while (v->val->obj != OPER_OBJ || Oper(v->val)->op != op) {
                    vsp--;
                    if (vsp == 0) goto syntaxe;
                    v--;
                }
                args -= vsp;
                vsp--;
                if (vsp == 0) goto syntaxe;
                tmp.val = v + 1;
                tmp.len = args; /* assumes no referencing */
                tmp.v.obj = FUNCARGS_OBJ;

                epoint.pos = out->pos;
                v--;
                oper.v1 = v[1].val = v[0].val;
                oper.v2 = &tmp.v;
                oper.epoint = &v[0].epoint;
                oper.epoint2 = (args != 0) ? &tmp.val->epoint : &epoint;
                if (op == O_BRACKET) {
                    oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
                    v[0].val = oper.v1->obj->slice(&oper, 0);
                } else {
                    oper.inplace = NULL;
                    v[0].val = oper.v1->obj->calc2(&oper);
                }
                while ((args--) != 0) {
                    val_destroy(tmp.val[args].val);
                    tmp.val[args].val = NULL;
                }
                continue;
            }
        case O_RBRACKET:
        case O_RPARENT:
        case O_TUPLE:
        case O_LIST:
            {
                List *list;
                bool tup = (oper.op == O_RPARENT), expc = (oper.op == O_TUPLE || oper.op == O_LIST);
                argcount_t args = vsp;
                Oper_types op = (oper.op == O_RBRACKET || oper.op == O_LIST) ? O_BRACKET : O_PARENT;
                if (vsp == 0) goto syntaxe;
                v = &values[vsp - 1];
                while (v->val->obj != OPER_OBJ || Oper(v->val)->op != op) {
                    vsp--;
                    if (vsp == 0) goto syntaxe;
                    v--;
                }
                args -= vsp;
                if (stop && !expc) {
                    Address_types am2;
                    if (out + 1 != ev->out.end) {
                        Obj *obj = out[1].val;
                        if (obj->obj != OPER_OBJ ||
                            (Oper(obj)->op != O_RPARENT &&   /* ((3)) */
                             Oper(obj)->op != O_RBRACKET &&  /* [(3)] */
                             Oper(obj)->op != O_RBRACE &&    /* {(3)} */
                             Oper(obj)->op != O_FUNC &&      /* f((3)) */
                             Oper(obj)->op != O_INDEX &&     /* m[(3)] */
                             Oper(obj)->op != O_COMMA &&     /* [(3),(3)] */
                             !(Oper(obj)->op >= O_COMMAX && Oper(obj)->op <= O_COMMAK) /* (3),y */
                            )) {
                            goto nind;
                        }
                    }
                    switch (args) {
                    case 1:
                    ind1:
                        am = (op == O_BRACKET) ? A_LI : A_I;
                        if (v[1].val->obj != ADDRESS_OBJ && !v[1].val->obj->iterable) {
                            v[0].val = new_address(v[1].val, am);
                        } else {
                            v[0].val = apply_addressing(v[1].val, am, true);
                            val_destroy(v[1].val);
                        }
                        v[1].val = NULL;
                        continue;
                    case 2:
                        if (v[2].val->obj != REGISTER_OBJ || Register(v[2].val)->len != 1) goto nind2;
                        am = register_to_indexing(Register(v[2].val)->data[0]);
                        if (am == A_NONE) goto nind2;
                    ind2:
                        val_destroy(v[2].val);
                        v[2].val = NULL;
                        if (v[1].val->obj != ADDRESS_OBJ && !v[1].val->obj->iterable) {
                            v[1].val = new_address(v[1].val, am);
                        } else {
                            Obj *tmp = apply_addressing(v[1].val, am, true);
                            val_destroy(v[1].val);
                            v[1].val = tmp;
                        }
                        goto ind1;
                    case 3:
                        if (v[3].val->obj != REGISTER_OBJ || Register(v[3].val)->len != 1) goto nind2;
                        am2 = register_to_indexing(Register(v[3].val)->data[0]);
                        if (am2 == A_NONE) goto nind2;
                        if (v[2].val->obj != REGISTER_OBJ || Register(v[2].val)->len != 1) goto nind2;
                        am = register_to_indexing(Register(v[2].val)->data[0]);
                        if (am == A_NONE) goto nind2;
                        val_destroy(v[2].val);
                        v[2].val = v[3].val;
                        v[3].val = NULL;
                        if (v[1].val->obj != ADDRESS_OBJ && !v[1].val->obj->iterable) {
                            v[1].val = new_address(v[1].val, am);
                        } else {
                            Obj *tmp = apply_addressing(v[1].val, am, true);
                            val_destroy(v[1].val);
                            v[1].val = tmp;
                        }
                        am = am2;
                        goto ind2;
                    default: break;
                    }
                }
            nind:
                if (args == 1) {
                    if (tup) {
                        v[0].val = v[1].val; v[1].val = NULL;
                        v[0].epoint.pos = v[1].epoint.pos;
                        continue;
                    }
                }
                if (args != 0) {
            nind2:
                    list = List(val_alloc((op == O_BRACKET) ? LIST_OBJ : TUPLE_OBJ));
                    list->len = args;
                    list->data = list_create_elements(list, args);
                    do  {
                        list->data[args - 1] = v[args].val;
                        v[args].val = NULL;
                        args--;
                    } while (args != 0);
                } else list = List(val_reference((op == O_BRACKET) ? null_list : null_tuple));
                v[0].val = Obj(list);
                continue;
            }
        case O_RBRACE:
        case O_DICT:
            {
                argcount_t args = vsp;
                if (vsp == 0) goto syntaxe;
                v = &values[vsp - 1];
                while (v->val != &operators[O_BRACE].v) {
                    vsp--;
                    if (vsp == 0) goto syntaxe;
                    v--;
                }
                args -= vsp;
                v[0].val = (args == 0) ? val_reference(null_dict) : dictobj_parse(v + 1, args);
                continue;
            }
        case O_COND:
            if (vsp < 3) goto syntaxe;
            vsp -= 2;
            v = &values[vsp - 1];
            val = v[0].val;
            if (val == true_value) {
            cond_true:
                v[0].val = v[1].val;
                v[0].epoint.pos = v[1].epoint.pos;
                v[1].val = val;
                continue;
            }
            if (val == false_value) {
            cond_false:
                v[0].val = v[2].val;
                v[0].epoint.pos = v[2].epoint.pos;
                v[2].val = val;
                continue;
            }
            {
                Obj *tmp = val->obj->truth(val, TRUTH_BOOL, &v[0].epoint);
                if (tmp->obj != BOOL_OBJ) {
                    val_destroy(val);
                    v[0].val = tmp;
                    continue;
                }
                val_destroy(tmp);
                if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, val, &v[0].epoint);
                if (tmp == true_value) goto cond_true;
                goto cond_false;
            }
        case O_DCOND:
            {
                Funcargs tmp;
                if (vsp < 3) goto syntaxe;
                vsp -= 2;
                v = &values[vsp - 1];
                tmp.val = v;
                tmp.len = 3; /* assumes no referencing */
                tmp.v.obj = FUNCARGS_OBJ;

                epoint.pos = out->pos;
                oper.v1 = none_value;
                oper.v2 = &tmp.v;
                oper.epoint = &epoint;
                oper.epoint2 = &v[0].epoint;
                oper.inplace = NULL;
                val = apply_condition(&oper);
                val_destroy(v[0].val); v[0].val = val;
                val_destroy(v[1].val);
                val_destroy(v[2].val);
                v[1].val = NULL;
                v[2].val = NULL;
                continue;
            }
        case O_QUEST:
        case O_DQUEST:
            if (vsp < 2) goto syntaxe;
            vsp--;
            v = &values[vsp - 1];
            epoint.pos = out->pos;
            err_msg2(ERROR______EXPECTED, "':'", &epoint);
            val_destroy(v[0].val); v[0].val = ref_none();
            continue;
        case O_COLON:
            {
                Colonlist *list;
                const struct out_s *out2;
                argcount_t args, j;
                for (out2 = out + 1; out2 != ev->out.end; out2++) {
                    Obj *vv = out2->val;
                    if (vv->obj != OPER_OBJ) break;
                    if (Oper(vv)->op != O_COLON) break;
                }
                args = (argcount_t)(out2 - out) + 1;
                if (vsp < args) goto syntaxe;
                list = new_colonlist();
                list->len = args;
                list->data = list_create_elements(list, args);
                for (j = 0; j < args; j++) {
                    struct values_s *vv = &values[vsp - args + j];
                    list->data[j] = vv->val;
                    vv->val = NULL;
                }
                vsp -= args - 1;
                out += args - 2;
                values[vsp - 1].val = Obj(list);
                continue;
            }
        case O_WORD:    /* <> */
        case O_HWORD:   /* >` */
        case O_BSWORD:  /* >< */
        case O_LOWER:   /* <  */
        case O_HIGHER:  /* >  */
        case O_BANK:    /* `  */
        case O_STRING:  /* ^  */
        case O_INV:     /* ~  */
        case O_NEG:     /* -  */
        case O_POS:     /* +  */
        case O_LNOT:    /* !  */
            epoint.pos = out->pos;
            if (vsp == 0) goto syntaxe;
            v = &values[vsp - 1];
            oper.v1 = v[0].val;
            oper.v2 = none_value;
            oper.epoint = &v[0].epoint;
            oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
            val = oper.v1->obj->calc1(&oper);
            val_destroy(v[0].val); v[0].val = val;
            v[0].epoint.pos = out->pos;
            continue;
        case O_COMMAS: am = A_SR; goto addr;                    /* ,s */
        case O_COMMAR: am = A_RR; goto addr;                    /* ,r */
        case O_COMMAZ: am = A_ZR; goto addr;                    /* ,z */
        case O_COMMAY: am = A_YR; goto addr;                    /* ,y */
        case O_COMMAX: am = A_XR; goto addr;                    /* ,x */
        case O_COMMAD: am = A_DR; goto addr;                    /* ,d */
        case O_COMMAB: am = A_BR; goto addr;                    /* ,b */
        case O_COMMAK: am = A_KR; goto addr;                    /* ,k */
        case O_HASH_SIGNED: am = A_IMMEDIATE_SIGNED; goto addr; /* #+ */
        case O_HASH: am = A_IMMEDIATE;                          /* #  */
        addr:
            if (vsp == 0) goto syntaxe;
            v = &values[vsp - 1];
            if (v[0].val->obj != ADDRESS_OBJ && !v[0].val->obj->iterable) {
                v[0].val = new_address(v[0].val, am);
            } else {
                val = apply_addressing(v[0].val, am, true);
                val_destroy(v[0].val); v[0].val = val;
            }
            if (oper.op == O_HASH || oper.op == O_HASH_SIGNED) v[0].epoint.pos = out->pos;
            continue;
        case O_SPLAT:   /* *  */
            if (vsp == 0) goto syntaxe;
            v = &values[vsp - 1];
            if (out + 1 != ev->out.end) {
                Obj *o = out[1].val;
                if (o->obj != OPER_OBJ || (Oper(o)->op != O_RPARENT && Oper(o)->op != O_RBRACKET && Oper(o)->op != O_RBRACE && Oper(o)->op != O_FUNC && Oper(o)->op != O_INDEX && Oper(o)->op != O_COMMA)) {
                    if (o == &operators[O_MEMBER].v) {
                        v[0].epoint.pos = out->pos;
                        continue;
                    }
                    epoint.pos = out->pos;
                    err_msg2(ERROR_EXPRES_SYNTAX, NULL, &epoint);
                    val_destroy(v[0].val); v[0].val = ref_none();
                    continue;
                }
            }
            if (v[0].val->obj->iterable) {
                struct iter_s iter;
                size_t k, len;
                argcount_t len2;
                Obj *tmp, *def;
                iter.data = v[0].val; v[0].val->obj->getiter(&iter);
                len = iter.len;

                if (v[0].val->obj == DICT_OBJ && Dict(v[0].val)->def != NULL) {
                    Colonlist *list = new_colonlist();
                    list->len = 2;
                    list->data = list->u.val;
                    list->data[0] = ref_default();
                    list->data[1] = val_reference(Dict(v[0].val)->def);
                    def = Obj(list);
                    if (inc_overflow(&len, 1)) err_msg_out_of_memory();
                } else def = NULL;

                len2 = vsp + (argcount_t)len;
                if (len2 < len) err_msg_out_of_memory(); /* overflow */
                vsp--;
                if (len2 >= ev->values_size) values = extend_values(ev, len);
                for (k = 0; k < len && (tmp = iter.next(&iter)) != NULL; k++) {
                    if (values[vsp].val != NULL) val_destroy(values[vsp].val);
                    values[vsp].val = val_reference(tmp);
                    values[vsp].epoint.pos = out->pos;
                    values[vsp++].epoint.line = epoint.line;
                }
                iter_destroy(&iter);
                if (def != NULL) {
                    if (values[vsp].val != NULL) val_destroy(values[vsp].val);
                    values[vsp].val = def;
                    values[vsp].epoint.pos = out->pos;
                    values[vsp++].epoint.line = epoint.line;
                }
                continue;
            }
            continue;
        case O_IDENTITY:
        case O_NIDENTITY:
            if (vsp < 2) goto syntaxe;
            vsp--;
            v = &values[vsp - 1];
            val = v[0].val;
            if (val == none_value || val->obj == ERROR_OBJ) continue;
            val = v[1].val;
            if (val == none_value || val->obj == ERROR_OBJ) {
                v[1].val = v[0].val;
                v[0].val = val;
                continue;
            }
            {
                bool result = v[0].val == v[1].val || v[0].val->obj->same(v[0].val, v[1].val);
                val = truth_reference(oper.op == O_IDENTITY ? result : !result);
            }
            val_destroy(v[0].val); v[0].val = val;
            continue;
        case O_MIN: /* <? */
        case O_MAX: /* >? */
            epoint.pos = out->pos;
            if (vsp < 2) goto syntaxe;
            vsp--;
            v = &values[vsp - 1];
            oper.v1 = v[0].val;
            oper.v2 = v[1].val;
            oper.epoint = &v[0].epoint;
            oper.epoint2 = &v[1].epoint;
            oper.inplace = NULL;
            val = oper.v1->obj->calc2(&oper);
            if (val->obj != BOOL_OBJ) {
                val_destroy(v[0].val); v[0].val = val;
                continue;
            }
            if (val != true_value) {
                val_destroy(v[0].val); v[0].val = v[1].val; v[1].val = NULL;
                v[0].epoint.pos = v[1].epoint.pos;
            }
            val_destroy(val);
            continue;
        case O_IN:
        case O_NOTIN:
            epoint.pos = out->pos;
            if (vsp < 2) goto syntaxe;
            vsp--;
            v = &values[vsp - 1];
            oper.v1 = v[0].val;
            oper.v2 = v[1].val;
            oper.epoint = &v[0].epoint;
            oper.epoint2 = &v[1].epoint;
            oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
            val = oper.v2->obj->contains(&oper);
            val_destroy(v[0].val); v[0].val = val;
            continue;
        default: break;
        }
        epoint.pos = out->pos;
        if (vsp < 2) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX, NULL);
            ev->out.p = out + 1;
            ev->values_len = 0;
            return false;
        }
        vsp--;
        v = &values[vsp - 1];
        oper.v1 = v[0].val;
        oper.v2 = v[1].val;
        oper.epoint = &v[0].epoint;
        oper.epoint2 = &v[1].epoint;
        oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
        val = oper.v1->obj->calc2(&oper);
        val_destroy(v[0].val); v[0].val = val;
    }
    ev->out.p = out;
    ev->values_len = vsp;
    return true;
}

struct values_s *get_val(void) {
    if (eval->values_p >= eval->values_len) return NULL;
    return &eval->values[eval->values_p++];
}

Obj *pull_val(void) {
    struct values_s *value = &eval->values[eval->values_p++];
    Obj *val = value->val;
    value->val = NULL;
    return val;
}

argcount_t get_val_remaining(void) {
    return eval->values_len - eval->values_p;
}

/* 0 - normal */
/* 1 - 1 only, till comma */
/* 2 - 1 only, till space  */
/* 3 - opcode */
/* 4 - opcode, with defaults */
/* 5 - 1 only, till comma/equal  */

static bool get_exp2(int stop) {
    uint8_t ch;

    Oper_types op;
    argcount_t db;
    unsigned int prec;
    struct linepos_s epoint;
    linecpos_t llen;
    size_t symbollist;
    Obj *val;
    struct out_list_s out;
    struct opr_list_s opr;

    clean_out(eval);
    eval->gstop = stop;
    eval->values_p = eval->values_len = 0;

    if (arguments.tasmcomp) {
        if (get_exp_compat(stop)) return get_val2_compat(eval);
        return false;
    }

    ignore();
    ch = here();
    if (ch == 0 || ch == ';') {
        eval->out.end = eval->out.data;
        eval->out.p = eval->out.data;
        return true;
    }
    symbollist = 0;
    epoint.line = lpoint.line;
    out.p = out.data = eval->out.data;
    out.max = eval->out.max;
    opr.p = opr.data = eval->opr.data;
    opr.max = eval->opr.max;
    for (;;) {
        ignore(); ch = here(); epoint.pos = lpoint.pos;
        switch (ch) {
        case ',':
            if (stop != 4 || opr.p != opr.data) goto tryanon;
            lpoint.pos++;
            val = ref_default();
        push_continue:
            out.p->val = val;
            out.p->pos = epoint.pos;
            if (++out.p == out.max) extend_out(&out);
            continue;
        case ')':
            if (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_TUPLE].v;goto tphack;}
                else if (o == O_PARENT || o == O_FUNC) goto other;
            }
            goto tryanon;
        case ']':
            if (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_LIST].v;goto lshack;}
                else if (o == O_BRACKET || o == O_INDEX) goto other;
            }
            goto tryanon;
        case '}':
            if (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_DICT].v;goto brhack;}
                else if (o == O_BRACE) goto other;
            }
            goto tryanon;
        case ':':
            if (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (o != O_PARENT && o != O_BRACKET && o != O_BRACE && o != O_FUNC && o != O_INDEX && o != O_COMMA) goto tryanon;
            }
            val = ref_default();
            goto push_other;
        case '(':
            if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) symbollist++;
        tphack2:
            lpoint.pos++;
            opr.p->op = O_PARENT;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            val = &operators[O_PARENT].v;
            goto push_continue;
        case '[':
            if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) symbollist++;
        lshack2:
            lpoint.pos++;
            opr.p->op = O_BRACKET;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            val = &operators[O_BRACKET].v;
            goto push_continue;
        case '{':
            lpoint.pos++;
            opr.p->op = O_BRACE;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            val = &operators[O_BRACE].v;
            goto push_continue;
        case '+': op = O_POS; break;
        case '-': op = O_NEG; break;
        case '*': op = O_SPLAT; break;
        case '!': op = O_LNOT;break;
        case '~': op = O_INV; break;
        case '<': if (pline[lpoint.pos + 1] == '>') {lpoint.pos++;op = O_WORD;} else op = O_LOWER; goto priocheck;
        case '>': if (pline[lpoint.pos + 1] == '`') {lpoint.pos++;op = O_HWORD;} else if (pline[lpoint.pos + 1] == '<') {lpoint.pos++;op = O_BSWORD;} else op = O_HIGHER; goto priocheck;
        case '#': op = (pline[lpoint.pos + 1] == '+' || pline[lpoint.pos + 1] == '-') ? O_HASH_SIGNED : O_HASH; goto priocheck;
        case '`': op = O_BANK; goto priocheck;
        case '^':
            op = O_STRING;
            if (diagnostics.deprecated) err_msg2(ERROR____OLD_STRING, NULL, &lpoint);
        priocheck:
            if (diagnostics.priority && opr.p != opr.data && opr.p - 1 != opr.data && op == opr.p[-2].op) {
                unsigned int prio = operators[opr.p[-1].op].prio;
                if (prio > operators[op].prio && prio != operators[O_NEG].prio && prio != operators[O_SPLAT].prio) {
                    struct linepos_s epoint2;
                    epoint2.line = lpoint.line;
                    epoint2.pos = opr.p[-2].pos;
                    err_msg_priority(&operators[op], &epoint2);
                }
            }
            break;
        case '$': val = get_hex(&epoint); goto push_other;
        case '%':
            if ((pline[lpoint.pos + 1] & 0xfe) == 0x30 || (pline[lpoint.pos + 1] == '.' && (pline[lpoint.pos + 2] & 0xfe) == 0x30)) {
                val = get_bin(&epoint); goto push_other;
            }
            goto tryanon;
        case '"':
        case '\'': val = get_string(&epoint); goto push_other;
        case '?':
            if (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (o == O_SPLAT || o == O_POS || o == O_NEG) goto tryanon;
            }
            lpoint.pos++; val = ref_gap(); goto push_other;
        case '.':
            if ((pline[lpoint.pos + 1] ^ 0x30) >= 10) {
                str_t symbol;
                symbol.data = pline + lpoint.pos + 1;
                symbol.len = get_label(symbol.data);
                if (symbol.len != 0) {
                    lpoint.pos += (linecpos_t)symbol.len + 1;
                    if (symbol.len > 1 && symbol.data[0] == '_' && symbol.data[1] == '_') err_msg2(ERROR_RESERVED_LABL, &symbol, &epoint);
                    val = new_symbol(&symbol, &epoint);
                    goto push_other;
                }
                switch (symbol.data[0]) {
                case '+':
                case '-':
                    while (symbol.data[0] == symbol.data[++symbol.len]);
                    lpoint.pos += (linecpos_t)symbol.len + 1;
                    val = new_anonsymbol((symbol.data[0] == '+') ? ((ssize_t)symbol.len - 1) : -(ssize_t)symbol.len);
                    goto push_other;
                case '*':
                    lpoint.pos += 2;
                    symbol.len = 1;
                    val = new_symbol(&symbol, &epoint);
                    goto push_other;
                case '(':
                    lpoint.pos++;
                    symbollist++;
                    goto tphack2;
                case '[':
                    lpoint.pos++;
                    symbollist++;
                    goto lshack2;
                case '.':
                    if (symbol.data[1] == '.') {
                        lpoint.pos += 3;
                        val = ref_fold();
                        goto push_other;
                    }
                    FALL_THROUGH; /* fall through */
                default:
                    goto tryanon;
                }
            }
            val = get_float(&epoint);
            goto push_other;
        case '0':
            if (diagnostics.leading_zeros && (pline[lpoint.pos + 1] ^ 0x30) < 10) err_msg2(ERROR_LEADING_ZEROS, NULL, &lpoint);
            FALL_THROUGH; /* fall through */
        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            val = get_float(&epoint);
            goto push_other;
        case 0:
        case ';':
            goto tryanon;
        default:
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            if (llen != 0) {
                bool down;
                Label *l;
                str_t symbol;
                lpoint.pos += llen;
            as_symbol:
                if (pline[epoint.pos + 1] == '"' || pline[epoint.pos + 1] == '\'') {
                    Textconv_types mode;
                    switch (pline[epoint.pos] | arguments.caseinsensitive) {
                    case 'n': mode = BYTES_MODE_NULL; break;
                    case 's': mode = BYTES_MODE_SHIFT; break;
                    case 'p': mode = BYTES_MODE_PTEXT; break;
                    case 'l': mode = BYTES_MODE_SHIFTL; break;
                    case 'b': mode = BYTES_MODE_TEXT; break;
                    case 'x': val = get_bytes(&epoint, false); goto push_other;
                    case 'z': val = get_bytes(&epoint, true); goto push_other;
                    default: mode = BYTES_MODE_NULL_CHECK; break;
                    }
                    if (mode != BYTES_MODE_NULL_CHECK) {
                        Obj *str = get_string(&epoint);
                        if (str->obj == STR_OBJ) {
                            epoint.pos++;
                            val = bytes_from_str(Str(str), &epoint, mode);
                            epoint.pos--;
                            val_destroy(str);
                        } else {
                            val = str;
                        }
                        goto push_other;
                    }
                }
                symbol.data = pline + epoint.pos;
                symbol.len = lpoint.pos - epoint.pos;
                if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) {
                    val = new_symbol(&symbol, &epoint);
                } else {
                    down = (symbol.data[0] != '_');
                    l = down ? find_label(&symbol, NULL) : find_label2(&symbol, cheap_context);
                    if (l != NULL) {
                        if (diagnostics.case_symbol && str_cmp(&symbol, &l->name) != 0) err_msg_symbol_case(&symbol, l, &epoint);
                        touch_label(l);
                        val = val_reference(l->value);
                    } else if (constcreated && pass < max_pass) {
                        val = ref_none();
                    } else {
                        Error *err = new_error(ERROR___NOT_DEFINED, &epoint);
                        err->u.notdef.symbol = new_symbol(&symbol, &epoint);
                        err->u.notdef.names = ref_namespace(down ? current_context : cheap_context);
                        err->u.notdef.down = down;
                        val = Obj(err);
                    }
                }
                goto push_other;
            }
        tryanon:
            db = 0;
            while (opr.p != opr.data && opr.p[-1].op == O_POS) {opr.p--; db++;}
            if (db != 0) {
                ssize_t as = (ssize_t)db - 1;
                if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) {
                    val = new_anonsymbol(as);
                } else {
                    val = resolv_anonlabel(as, opr.p->pos);
                }
                epoint.pos = opr.p->pos;
                goto push_other;
            }
            while (opr.p != opr.data && opr.p[-1].op == O_NEG) {opr.p--; db++;}
            if (db != 0) {
                ssize_t as = -(ssize_t)db;
                if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) {
                    val = new_anonsymbol(as);
                } else {
                    val = resolv_anonlabel(as, opr.p->pos);
                }
                epoint.pos = opr.p->pos;
                goto push_other;
            }
            if (opr.p != opr.data) {
                if (opr.p[-1].op == O_COLON) {
                    val = ref_default();
                    goto push_other;
                }
                if (opr.p[-1].op == O_SPLAT) {
                    opr.p--;
                    if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) {
                        val = new_starsymbol(opr.p->pos);
                    } else {
                        val = get_star();
                    }
                    epoint.pos = opr.p->pos;
                    goto push_other;
                }
                epoint.pos = opr.p[-1].pos;
            }
            if ((ch < ' ' && ch != 0) || ch > '~') err_msg_wrong_character(&lpoint);
            else err_msg2(ERROR______EXPECTED, "an expression is", &lpoint);
            goto error;
        }
        if (opr.p != opr.data && opr.p[-1].op == O_SPLAT) {
            opr.p--;
            lpoint.pos = epoint.pos;
            if ((opr.p != opr.data && opr.p[-1].op == O_MEMBER) || symbollist != 0) {
                val = new_starsymbol(opr.p->pos);
            } else {
                val = get_star();
            }
            epoint.pos = opr.p->pos;
            goto push_other;
        }
        lpoint.pos++;
    rtl:
        opr.p->op = op;
        opr.p->pos = epoint.pos;
        if (++opr.p == opr.max) extend_opr(&opr);
        continue;
    push_other:
        out.p->val = val;
        out.p->pos = epoint.pos;
        if (++out.p == out.max) extend_out(&out);
    other:
        if (stop != 2) ignore();
        ch = here();epoint.pos = lpoint.pos;
        switch (ch) {
        case ',':
            lpoint.pos++;
            if (pline[lpoint.pos] >= 'A') {
                llen = (linecpos_t)get_label(pline + lpoint.pos);
                lpoint.pos += llen;
                if (llen == 1 && pline[epoint.pos + 2] != '"' && pline[epoint.pos + 2] != '\'') {
                    switch (pline[epoint.pos + 1] | arguments.caseinsensitive) {
                    case 'x': op = O_COMMAX; break;
                    case 'y': op = O_COMMAY; break;
                    case 'z': op = O_COMMAZ; break;
                    case 'r': op = O_COMMAR; break;
                    case 's': op = O_COMMAS; break;
                    case 'd': op = O_COMMAD; break;
                    case 'b': op = O_COMMAB; break;
                    case 'k': op = O_COMMAK; break;
                    default: op = O_NONE; break;
                    }
                    if (op != O_NONE) {
                        prec = operators[O_HASH].prio;
                        while (opr.p != opr.data) {
                            Oper_types o = opr.p[-1].op;
                            if (prec > operators[o].prio) break;
                            if (o == O_COLON2 || o == O_COND || o == O_DCOND) break;
                            out.p->val = &operators[o].v;
                            opr.p--;
                            out.p->pos = opr.p->pos;
                            if (++out.p == out.max) extend_out(&out);
                        }
                        opr.p->op = op;
                        opr.p->pos = epoint.pos;
                        if (++opr.p == opr.max) extend_opr(&opr);
                        goto other;
                    }
                }
            } else llen = 0;
            prec = operators[O_COMMA].prio;
            while (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (prec > operators[o].prio) break;
                out.p->val = &operators[o].v;
                opr.p--;
                out.p->pos = opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
            }
            if (opr.p == opr.data) {
                if (stop == 1 || stop == 5) {lpoint.pos = epoint.pos;break;}
            }
            out.p->val = &operators[O_COMMA].v;
            out.p->pos = epoint.pos;
            if (++out.p == out.max) extend_out(&out);
            opr.p->op = O_COMMA;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            if (llen != 0) {
                epoint.pos++;
                goto as_symbol;
            }
            continue;
        case '(':
            lpoint.pos++;
            prec = operators[O_MEMBER].prio;
            while (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (prec > operators[o].prio) break;
                out.p->val = &operators[o].v;
                opr.p--;
                out.p->pos =  opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
            }
            opr.p->op = O_FUNC;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            if (symbollist != 0) symbollist++;
            val = &operators[O_PARENT].v;
            goto push_continue;
        case '[':
            lpoint.pos++;
            prec = operators[O_MEMBER].prio;
            while (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (prec > operators[o].prio) break;
                out.p->val = &operators[o].v;
                opr.p--;
                out.p->pos = opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
            }
            opr.p->op = O_INDEX;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            if (symbollist != 0) symbollist++;
            val = &operators[O_BRACKET].v;
            goto push_continue;
        case '&': op = pline[lpoint.pos + 1] == '&' ? (pline[lpoint.pos + 2] == '=' ? O_LAND_ASSIGN : O_LAND) : (pline[lpoint.pos + 1] == '=' ? O_AND_ASSIGN : O_AND); goto push2;
        case '|': op = pline[lpoint.pos + 1] == '|' ? (pline[lpoint.pos + 2] == '=' ? O_LOR_ASSIGN : O_LOR) : (pline[lpoint.pos + 1] == '=' ? O_OR_ASSIGN : O_OR); goto push2;
        case '^': op = pline[lpoint.pos + 1] == '^' ? O_LXOR : (pline[lpoint.pos + 1] == '=' ? O_XOR_ASSIGN : O_XOR); goto push2;
        case '*': op = pline[lpoint.pos + 1] == '*' ? (pline[lpoint.pos + 2] == '=' ? O_EXP_ASSIGN : O_EXP) : (pline[lpoint.pos + 1] == '=' ? O_MUL_ASSIGN : O_MUL); if (op == O_EXP) {lpoint.pos+=2; goto rtl;} goto push2;
        case '%': op = pline[lpoint.pos + 1] == '=' ? O_MOD_ASSIGN : O_MOD; goto push2;
        case '/': if (pline[lpoint.pos + 1] == '/') {if (diagnostics.deprecated) err_msg2(ERROR____OLD_MODULO, NULL, &lpoint);lpoint.pos++;op = O_MOD;} else op = pline[lpoint.pos + 1] == '=' ? O_DIV_ASSIGN : O_DIV; goto push2;
        case '+': op = pline[lpoint.pos + 1] == '=' ? O_ADD_ASSIGN : O_ADD; goto push2;
        case '-': op = pline[lpoint.pos + 1] == '=' ? O_SUB_ASSIGN : O_SUB; goto push2;
        case '.': op = pline[lpoint.pos + 1] == '.' ? (pline[lpoint.pos + 2] == '=' ? O_CONCAT_ASSIGN : O_CONCAT) : (pline[lpoint.pos + 1] == '=' ? O_MEMBER_ASSIGN : O_MEMBER); goto push2;
        case '?': op = (pline[lpoint.pos + 1] == '?') ? O_DQUEST : O_QUEST; lpoint.pos += operators[op].len; prec = operators[O_COND].prio + 1; goto push3;
        case ':': if (pline[lpoint.pos + 1] == '=') {op = O_COLON_ASSIGN;goto push2;}
            if (pline[lpoint.pos + 1] == '?' && pline[lpoint.pos + 2] == '=') {op = O_COND_ASSIGN;goto push2;}
            if (pline[lpoint.pos + 1] == ':' && pline[lpoint.pos + 2] == '=') {op = O_REASSIGN;goto push2;}
            lpoint.pos++;
            op = O_COLON;
            prec = operators[O_COLON].prio + 1;
            while (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (prec > operators[o].prio) break;
                out.p->val = &operators[o].v;
                opr.p--;
                out.p->pos = opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
            }
            if (opr.p != opr.data) {
                if (opr.p[-1].op == O_QUEST) { opr.p[-1].op = O_COND; op = O_COLON2;}
                else if (opr.p[-1].op == O_DQUEST) { opr.p[-1].op = O_DCOND; op = O_COLON2;}
            }
            opr.p->op = op;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            continue;
        case '=':
            if (pline[lpoint.pos + 1] != '=') {
                op = O_EQ;
                prec = operators[O_EQ].prio;
                while (opr.p != opr.data) {
                    Oper_types o = opr.p[-1].op;
                    if (prec > operators[o].prio) break;
                    out.p->val = &operators[o].v;
                    opr.p--;
                    out.p->pos = opr.p->pos;
                    if (++out.p == out.max) extend_out(&out);
                }
                if (opr.p == opr.data) {
                    if (stop == 5) break;
                }
                if (diagnostics.old_equal) err_msg2(ERROR_____OLD_EQUAL, NULL, &lpoint);
                lpoint.pos++;
                opr.p->op = op;
                opr.p->pos = epoint.pos;
                if (++opr.p == opr.max) extend_opr(&opr);
                continue;
            }
            op = pline[lpoint.pos + 2] != '=' ? O_EQ : O_IDENTITY;
        push2:
            lpoint.pos += operators[op].len;
        push2a:
            prec = operators[op].prio;
        push3:
            while (opr.p != opr.data) {
                Oper_types o = opr.p[-1].op;
                if (prec > operators[o].prio) break;
                out.p->val = &operators[o].v;
                opr.p--;
                out.p->pos = opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
            }
            opr.p->op = op;
            opr.p->pos = epoint.pos;
            if (++opr.p == opr.max) extend_opr(&opr);
            continue;
        case '<':
            switch (pline[lpoint.pos + 1]) {
            case '>': if (diagnostics.deprecated) err_msg2(ERROR_______OLD_NEQ, NULL, &lpoint); op = O_NE; break;
            case '<': op = pline[lpoint.pos + 2] == '=' ? O_BLS_ASSIGN : O_LSHIFT; break;
            case '?': op = pline[lpoint.pos + 2] == '=' ? O_MIN_ASSIGN : O_MIN; break;
            case '=': op = pline[lpoint.pos + 2] == '>' ? O_CMP : O_LE; break;
            default: op = O_LT; break;
            }
            goto push2;
        case '>':
            switch (pline[lpoint.pos + 1]) {
            case '<': if (diagnostics.deprecated) err_msg2(ERROR_______OLD_NEQ, NULL, &lpoint); op = O_NE; break;
            case '>': op = pline[lpoint.pos + 2] == '=' ? O_BRS_ASSIGN : O_RSHIFT; break;
            case '?': op = pline[lpoint.pos + 2] == '=' ? O_MAX_ASSIGN : O_MAX; break;
            case '=': op = O_GE; break;
            default: op = O_GT; break;
            }
            goto push2;
        case '!':
            if (pline[lpoint.pos + 1] == '=') {op = pline[lpoint.pos + 2] != '=' ? O_NE : O_NIDENTITY;goto push2;}
            if (get_label(pline + lpoint.pos + 1) == 2 &&
                (pline[epoint.pos + 1] | arguments.caseinsensitive) == 'i' &&
                (pline[epoint.pos + 2] | arguments.caseinsensitive) == 'n') {op = O_NOTIN;goto push2;}
            err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        case ')':
            val = &operators[O_RPARENT].v;
        tphack:
            if (symbollist != 0) symbollist--;
            do {
                const char *mis;
                if (opr.p != opr.data) {
                    switch (opr.p[-1].op) {
                    case O_FUNC:
                        val = &operators[O_FUNC].v;
                        FALL_THROUGH; /* fall through */
                    case O_PARENT:
                        lpoint.pos++;
                        opr.p--;
                        epoint.pos = opr.p->pos;
                        goto push_other;
                    case O_BRACKET:
                    case O_INDEX: mis = "']'"; break;
                    case O_BRACE: mis = "'}'"; break;
                    default:
                        opr.p--;
                        out.p->val = &operators[opr.p->op].v;
                        out.p->pos = opr.p->pos;
                        if (++out.p == out.max) extend_out(&out);
                        continue;
                    }
                } else mis = "')' not";
                err_msg2(ERROR______EXPECTED, mis, &lpoint); goto error;
            } while (true);
        case ']':
            val = &operators[O_RBRACKET].v;
        lshack:
            if (symbollist != 0) symbollist--;
            do {
                const char *mis;
                if (opr.p != opr.data) {
                    switch (opr.p[-1].op) {
                    case O_INDEX:
                        val = &operators[O_INDEX].v;
                        FALL_THROUGH; /* fall through */
                    case O_BRACKET:
                        lpoint.pos++;
                        opr.p--;
                        epoint.pos = opr.p->pos;
                        goto push_other;
                    case O_PARENT:
                    case O_FUNC: mis = "')'"; break;
                    case O_BRACE: mis = "'}'"; break;
                    default:
                        opr.p--;
                        out.p->val = &operators[opr.p->op].v;
                        out.p->pos = opr.p->pos;
                        if (++out.p == out.max) extend_out(&out);
                        continue;
                    }
                } else mis = "']' not";
                err_msg2(ERROR______EXPECTED, mis, &lpoint); goto error;
            } while (true);
        case '}':
            val = &operators[O_RBRACE].v;
        brhack:
            do {
                const char *mis;
                if (opr.p != opr.data) {
                    switch (opr.p[-1].op) {
                    case O_BRACE:
                        lpoint.pos++;
                        opr.p--;
                        epoint.pos = opr.p->pos;
                        goto push_other;
                    case O_PARENT:
                    case O_FUNC: mis = "')'"; break;
                    case O_BRACKET:
                    case O_INDEX: mis = "']'"; break;
                    default:
                        opr.p--;
                        out.p->val = &operators[opr.p->op].v;
                        out.p->pos = opr.p->pos;
                        if (++out.p == out.max) extend_out(&out);
                        continue;
                    }
                } else mis = "'}' not";
                err_msg2(ERROR______EXPECTED, mis, &lpoint); goto error;
            } while (true);
        case 0:
        case ';':
        case '\t':
        case ' ': break;
        default:
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            lpoint.pos += llen;
            switch (llen) {
            case 1: if ((pline[epoint.pos] | arguments.caseinsensitive) == 'x') {if (pline[lpoint.pos] == '=') {lpoint.pos++; op = O_X_ASSIGN;} else op = O_X;goto push2a;} break;
            case 2: if ((pline[epoint.pos] | arguments.caseinsensitive) == 'i' &&
                        (pline[epoint.pos + 1] | arguments.caseinsensitive) == 'n') {op = O_IN;goto push2a;} break;
            }
            if (ch < ' ' || ch > '~') err_msg_wrong_character(&epoint);
            else err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        }
        while (opr.p != opr.data) {
            const char *mis;
            switch (opr.p[-1].op) {
            case O_PARENT:
            case O_FUNC: mis = "')'"; break;
            case O_BRACKET:
            case O_INDEX: mis = "']'"; break;
            case O_BRACE: mis = "'}'"; break;
            default:
                opr.p--;
                out.p->val = &operators[opr.p->op].v;
                out.p->pos = opr.p->pos;
                if (++out.p == out.max) extend_out(&out);
                continue;
            }
            err_msg2(ERROR______EXPECTED, mis, &epoint); goto error;
        }
        eval->opr.data = opr.data;
        eval->opr.max = opr.max;
        eval->out.data = out.data;
        eval->out.p = out.data;
        eval->out.max = out.max;
        eval->out.end = out.p;
        return get_val2(eval);
    error:
        break;
    }
    eval->opr.data = opr.data;
    eval->opr.max = opr.max;
    eval->out.data = out.data;
    eval->out.p = out.data;
    eval->out.max = out.max;
    eval->out.end = out.p;
    return false;
}

bool get_exp(int stop, argcount_t min, argcount_t max, linepos_t epoint) {/* length in bytes, defined */
    if (!get_exp2(stop)) {
        return false;
    }
    if (eval->values_len < min || (max != 0 && eval->values_len > max)) {
        err_msg_argnum(eval->values_len, min, max, epoint);
        return false;
    }
    return true;
}

MUST_CHECK Obj *get_vals_tuple(void) {
    argcount_t i, len = get_val_remaining();
    Tuple *list;

    switch (len) {
    case 0:
        return val_reference(null_tuple);
    case 1:
        return pull_val();
    default:
        break;
    }
    list = new_tuple(len);
    for (i = 0; i < len; i++) {
        list->data[i] = pull_val();
    }
    return Obj(list);
}

void get_vals_funcargs(Funcargs *f) {
    f->val = eval->values;
    f->len = eval->values_len;
}

MUST_CHECK Obj *calc2_lxor(oper_t op, bool i) {
    Obj *o2 = op->v2;
    Obj *result = o2->obj->truth(o2, TRUTH_BOOL, op->epoint2);
    if (result->obj != BOOL_OBJ) return result;
    if (i == Bool(result)->value) {
        if (i) {
            val_destroy(result);
            result = ref_false();
        }
        return result;
    }
    val_destroy(result);
    return val_reference(i ? op->v1 : o2);
}

void eval_enter(void) {
    evx_p++;
    if (evx_p >= evxnum) {
        extend_array(&evx, &evxnum, 1);
        new_instance(&eval);
        eval->values = NULL;
        eval->values_size = 0;
        new_array(&eval->out.data, 16);
        eval->out.max = eval->out.data + 16;
        eval->out.p = NULL;
        eval->out.end = NULL;
        new_array(&eval->opr.data, 16);
        eval->opr.max = eval->opr.data + 16;
        evx[evx_p] = eval;
        return;
    }
    eval = evx[evx_p];
}

void eval_leave(void) {
    if (evx_p != 0) evx_p--;
    eval = evx[evx_p];
}

void init_eval(void) {
    evx = NULL;
    evxnum = 0;
    evx_p = ~(size_t)0;
    eval_enter();
}

void destroy_eval(void) {
    while ((evxnum--) != 0) {
        struct values_s *v;
        eval = evx[evxnum];
        clean_out(eval);
        free(eval->out.data);
        free(eval->opr.data);
        v = eval->values;
        while ((eval->values_size--) != 0) {
            if (v->val != NULL) val_destroy(v->val);
            v++;
        }
        free(eval->values);
        free(eval);
    }
    free(evx);
}
