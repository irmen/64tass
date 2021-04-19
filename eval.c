/*
    $Id: eval.c 2596 2021-04-18 18:52:11Z soci $

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
#include "macro.h"
#include "variables.h"
#include "64tass.h"
#include "unicode.h"
#include "listing.h"
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
    uchar_t ch;
    if (!arguments.to_ascii) return 0;
    l = utf8in(s, &ch);
    return ((uget_property(ch)->property & id_Start) != 0) ? l : 0;
}

static FAST_CALL NO_INLINE unsigned int get_label_continue(const uint8_t *s) {
    unsigned int l;
    uchar_t ch;
    if (!arguments.to_ascii) return 0;
    l = utf8in(s, &ch);
    return ((uget_property(ch)->property & (id_Continue | id_Start)) != 0) ? l : 0;
}

FAST_CALL size_t get_label(const uint8_t *s) {
    size_t i;
    if (((uint8_t)((*s | 0x20) - 'a')) > 'z' - 'a' && *s != '_') {
        if (*s < 0x80) return 0;
        i = get_label_start(s);
        if (i == 0) return 0;
    } else i = 1;
    for (;;) {
        unsigned int l;
        if (((uint8_t)((s[i] | 0x20) - 'a')) <= 'z' - 'a' || (s[i] ^ 0x30) < 10 || s[i] == '_') {
            i++;
            continue;
        }
        if (s[i] < 0x80) return i;
        l = get_label_continue(s + i);
        if (l == 0) return i;
        i += l;
    }
}

static MUST_CHECK Obj *get_dec(linepos_t epoint) {
    linecpos_t len, len2;
    Obj *v = int_from_decstr(pline + lpoint.pos, &len, &len2);
    lpoint.pos += len;
    return (v != NULL) ? v : new_error_mem(epoint);
}

static double ldexp10(double d, unsigned int expo, bool neg) {
    static const double nums[10] = {1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9};
    double scal = expo < 10 ? nums[expo] : pow(10.0, (double)expo);
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
    struct opr_s *data;
    argcount_t p, size;
};

struct out_s {
    Obj *val;
    linecpos_t pos;
};

struct out_list_s {
    struct out_s *data;
    argcount_t p, size;
};

static size_t evxnum, evx_p;
static struct eval_context_s {
    struct values_s *values;
    argcount_t values_len;
    argcount_t values_p;
    argcount_t values_size;
    argcount_t outp2;
    int gstop;
    struct out_list_s out;
    struct opr_list_s opr;
} **evx;

static struct eval_context_s *eval;

static NO_INLINE void extend_out(struct out_list_s *out) {
    out->size += 64;
    if (out->size < 64 || out->size > ARGCOUNT_MAX / sizeof *out->data) err_msg_out_of_memory(); /* overflow */
    out->data = (struct out_s *)reallocx(out->data, out->size * sizeof *out->data);
}

static NO_INLINE void extend_opr(struct opr_list_s *opr) {
    opr->size += 64;
    if (opr->size < 64 || opr->size > ARGCOUNT_MAX / sizeof *opr->data) err_msg_out_of_memory(); /* overflow */
    opr->data = (struct opr_s *)reallocx(opr->data, opr->size * sizeof *opr->data);
}

static inline void clean_out(struct eval_context_s *ev) {
    struct out_s *o, *o2 = &ev->out.data[ev->out.p];
    for (o = &ev->out.data[ev->outp2]; o < o2; o++) val_destroy(o->val);
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
    struct out_list_s out = eval->out;
    struct opr_list_s opr;

    opr.data = eval->opr.data;
    opr.size = eval->opr.size;
    opr.p = 0;
    opr.data[0].op = O_COMMA;
rest:
    ignore();
    conv = O_NONE;
    first = (here() == '(') && (stop == 3 || stop == 4);
    if (out.p == 0 && here() == '#') {
        cpoint2 = lpoint.pos; lpoint.pos++;
    }
    switch (here()) {
    case 0:
    case ';':
        eval->opr = opr;
        eval->out = out;
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
            /* fall through */
        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            val = get_dec(&epoint); goto push_other;
        default:
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            if (llen == 0) {
                if (opr.p != 0) epoint.pos = opr.data[opr.p - 1].pos;
                err_msg2(ERROR______EXPECTED, "an expression is", &lpoint);
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
        out.data[out.p].val = val;
        out.data[out.p++].pos = epoint.pos;
        if (out.p >= out.size) extend_out(&out);
    other:
        if (stop != 2) ignore();
        ch = here(); epoint.pos = lpoint.pos;

        while (opr.p != 0) {
            const struct opr_s *o = &opr.data[opr.p - 1];
            if (o->op == O_PARENT) break;
            out.data[out.p].val = &operators[o->op].v;
            out.data[out.p++].pos = o->pos;
            if (out.p >= out.size) extend_out(&out);
            opr.p--;
        }
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = (linecpos_t)get_label(pline + lpoint.pos);
            lpoint.pos += llen;
            if (llen == 1) {
                switch (pline[epoint.pos + 1] | arguments.caseinsensitive) {
                case 'x':
                    opr.data[opr.p].op = O_COMMAX;
                    opr.data[opr.p++].pos = epoint.pos; 
                    if (opr.p >= opr.size) extend_opr(&opr);
                    goto other;
                case 'y':
                    opr.data[opr.p].op = O_COMMAY;
                    opr.data[opr.p++].pos = epoint.pos; 
                    if (opr.p >= opr.size) extend_opr(&opr);
                    goto other;
                default: break;
                }
            }
            if (conv != O_NONE) {
                out.data[out.p].val = &operators[conv].v;
                out.data[out.p++].pos = cpoint;
                if (out.p >= out.size) extend_out(&out);
            }
            if (cpoint2 != 0) {
                out.data[out.p].val = &operators[O_HASH].v;
                out.data[out.p++].pos = cpoint2;
                if (out.p >= out.size) extend_out(&out);
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
            opr.data[opr.p].op = op;
            opr.data[opr.p++].pos = epoint.pos; 
            if (opr.p >= opr.size) extend_opr(&opr);
            continue;
        case ')':
            if (opr.p == 0) {err_msg2(ERROR______EXPECTED, "')' not", &lpoint); goto error;}
            lpoint.pos++;
            opr.p--;
            if (opr.p == 0 && first) {
                opr.data[opr.p].op = O_TUPLE;
                opr.data[opr.p++].pos = epoint.pos;
                if (opr.p >= opr.size) extend_opr(&opr);
                first = false;
            }
            goto other;
        case 0:
        case ';':
        case '\t':
        case ' ':
            if (conv != O_NONE) {
                out.data[out.p].val = &operators[conv].v;
                out.data[out.p++].pos = cpoint;
                if (out.p >= out.size) extend_out(&out);
            }
            if (cpoint2 != 0) {
                out.data[out.p].val = &operators[O_HASH].v;
                out.data[out.p++].pos = cpoint2;
                if (out.p >= out.size) extend_out(&out);
            }
            break;
        default:
            err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        }
        if (opr.p == 0) {
            eval->opr = opr;
            eval->out = out;
            return true;
        }
        epoint.pos = opr.data[opr.p - 1].pos;
        err_msg2(ERROR______EXPECTED, "')'", &epoint);
    error:
        break;
    }
    eval->opr = opr;
    eval->out = out;
    return false;
}

static struct values_s *extend_values(struct eval_context_s *ev, size_t by) {
    argcount_t j = ev->values_size;
    struct values_s *values;
    ev->values_size += (argcount_t)by;
    if (ev->values_size < by || ev->values_size > ARGCOUNT_MAX / sizeof *values) err_msg_out_of_memory(); /* overflow */
    ev->values = values = (struct values_s *)reallocx(ev->values, ev->values_size * sizeof *values);
    for (; j < ev->values_size; j++) values[j].val = NULL;
    return values;
}

static bool get_val2_compat(struct eval_context_s *ev) {/* length in bytes, defined */
    argcount_t vsp = 0;
    Oper_types op;
    argcount_t i;
    Obj *val;
    Error *err;
    struct values_s *v1, *v2;
    struct out_s *out;
    struct values_s *values;
    struct linepos_s epoint;

    ev->values_p = 0;
    values = ev->values;

    epoint.line = lpoint.line;
    for (i = ev->outp2; i < ev->out.p; i++) {
        out = &ev->out.data[i];
        val = out->val;
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
        v1 = &values[vsp - 1];
        switch (op) {
        case O_LOWER:
        case O_HIGHER:
        case O_HASH:
        case O_COMMAX:
        case O_COMMAY:
        case O_TUPLE:
            switch (v1->val->obj->type) {
            case T_ADDRESS:
                switch (op) {
                case O_COMMAX:
                case O_COMMAY:
                case O_TUPLE:
                    {
                        const Address *old = Address(v1->val);
                        Obj *val2 = new_address(val_reference(old->val), (old->type << 4) | ((op == O_TUPLE) ? A_I : (op == O_COMMAX) ? A_XR : A_YR));
                        val_destroy(v1->val); v1->val = val2;
                        v1->epoint.pos = out->pos;
                        continue;
                    }
                default:break;
                }
                epoint.pos = out->pos;
                err = new_error(ERROR__INVALID_OPER, &epoint);
                err->u.invoper.op = op;
                err->u.invoper.v1 = v1->val;
                err->u.invoper.v2 = NULL;
                v1->val = Obj(err);
                break;
            default:
                {
                    uint16_t val1;
                    uval_t uval;
                    err = v1->val->obj->uval(v1->val, &uval, 8 * sizeof uval, &v1->epoint);
                    if (err != NULL) {
                        val_destroy(v1->val);
                        v1->val = Obj(err);
                        break;
                    }
                    val1 = (uint16_t)uval;

                    switch (op) {
                    case O_HASH:
                    case O_COMMAX:
                    case O_COMMAY:
                        v1->val = new_address(v1->val, (op == O_HASH) ? A_IMMEDIATE : (op == O_COMMAX) ? A_XR : A_YR);
                        v1->epoint.pos = out->pos;
                        continue;
                    case O_HIGHER:
                        val1 = (uint8_t)(val1 >> 8);
                        break;
                    case O_LOWER:
                        val1 = (uint8_t)val1;
                        break;
                    case O_TUPLE:
                        v1->val = new_address(v1->val, A_I);
                        v1->epoint.pos = out->pos;
                        continue;
                    default: break;
                    }
                    val_destroy(v1->val);
                    v1->val = int_from_uval(val1);
                    break;
                }
            case T_ERROR:
            case T_NONE:break;
            }
            v1->epoint.pos = out->pos;
            continue;
        default:break;
        }
        if (vsp < 2) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            ev->outp2 = i + 1;
            ev->values_len = 0;
            return false;
        }
        v2 = v1; v1 = &values[--vsp - 1];
        switch (v1->val->obj->type) {
        case T_INT:
        case T_BITS:
        case T_CODE:
        case T_STR:
        case T_ADDRESS:
            switch (v2->val->obj->type) {
            case T_INT:
            case T_BITS:
            case T_CODE:
            case T_STR:
            case T_ADDRESS:
                {
                    uint16_t val1, val2;
                    uval_t uval;
                    err = v1->val->obj->uval(v1->val, &uval, 8 * sizeof uval, &v1->epoint);
                    if (err != NULL) {
                        val_destroy(v1->val);
                        v1->val = Obj(err);
                        continue;
                    }
                    val1 = (uint16_t)uval;
                    err = v2->val->obj->uval(v2->val, &uval, 8 * sizeof uval, &v2->epoint);
                    if (err != NULL) {
                        val_destroy(v1->val);
                        v1->val = Obj(err);
                        continue;
                    }
                    val2 = (uint16_t)uval;

                    switch (op) {
                    case O_MUL: val1 = (uint16_t)(val1 * val2); break;
                    case O_DIV:
                        if (val2 == 0) {
                            epoint.pos = out->pos;
                            val_destroy(v1->val); v1->val = new_error_obj(ERROR_DIVISION_BY_Z, v2->val, &epoint);
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
                    val_destroy(v1->val);
                    v1->val = int_from_uval(val1);
                    continue;
                }
            default: 
                epoint.pos = out->pos;
                break;
            case T_ERROR:
            case T_NONE:
                val_replace(&v1->val, v2->val);
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
        err->u.invoper.v1 = v1->val;
        err->u.invoper.v2 = val_reference(v2->val);
        v1->val = Obj(err);
    }
    ev->outp2 = i;
    ev->values_len = vsp;
    return true;
}

static MUST_CHECK Obj *apply_addressing(Obj *o1, Address_types am, bool inplace) {
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
            vals[i] = apply_addressing(o1, am, inplace);
        }
        iter_destroy(&iter);
        v->len = i;
        v->data = vals;
        return Obj(v);
    }
    if (o1->obj == ADDRESS_OBJ) {
        Address *v1 = Address(o1);
        if (inplace && o1->refcount == 1) {
            v1->type = am | (v1->type << 4);
            return val_reference(o1);
        }
        return new_address(val_reference(v1->val), am | (v1->type << 4));
    }
    return new_address(val_reference(o1), am);
}

static bool get_val2(struct eval_context_s *ev) {
    argcount_t vsp = 0;
    argcount_t i;
    struct values_s *v1, *v2;
    bool stop = (ev->gstop == 3 || ev->gstop == 4);
    struct out_s *out;
    Obj *val;
    struct values_s *values;
    struct oper_s oper;
    Address_types am;
    struct linepos_s epoint;

    ev->values_p = 0;
    values = ev->values;

    epoint.line = lpoint.line;
    for (i = ev->outp2; i < ev->out.p; i++) {
        out = &ev->out.data[i];
        val = out->val;
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
        if (vsp == 0) goto syntaxe;
        v1 = &values[vsp - 1];
        switch (oper.op) {
        case O_FUNC:
        case O_INDEX:
            {
                argcount_t args = 0;
                Funcargs tmp;
                Oper_types op = (oper.op == O_FUNC) ? O_PARENT : O_BRACKET;
                while (v1->val->obj != OPER_OBJ || Oper(v1->val)->op != op) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp - 1 - args];
                }
                if (v1 == values) goto syntaxe;
                tmp.val = &values[vsp - args];
                tmp.len = args; /* assumes no referencing */
                vsp -= args + 1;
                tmp.v.obj = FUNCARGS_OBJ;
                v1--;

                epoint.pos = out->pos;
                oper.v1 = v1[1].val = v1->val;
                oper.v2 = &tmp.v;
                oper.epoint = &v1->epoint;
                oper.epoint2 = (args != 0) ? &tmp.val->epoint : &epoint;
                oper.epoint3 = &epoint;
                if (op == O_BRACKET) {
                    oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
                    v1->val = oper.v1->obj->slice(&oper, 0);
                } else {
                    oper.inplace = NULL;
                    v1->val = oper.v1->obj->calc2(&oper);
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
                argcount_t args = 0;
                Oper_types op = (oper.op == O_RBRACKET || oper.op == O_LIST) ? O_BRACKET : O_PARENT;
                while (v1->val->obj != OPER_OBJ || Oper(v1->val)->op != op) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp - 1 - args];
                }
                if (args == 1) {
                    if (stop && !expc) {
                        argcount_t j = i + 1;
                        vsp--;
                        if (tup && j < ev->out.p) {
                            Obj *obj = ev->out.data[j].val;
                            if (obj->obj != OPER_OBJ ||
                                    (Oper(obj)->op != O_RPARENT &&   /* ((3)) */
                                     Oper(obj)->op != O_RBRACKET &&  /* [(3)] */
                                     Oper(obj)->op != O_FUNC &&      /* f((3)) */
                                     Oper(obj)->op != O_LIST &&      /* [(3),] */
                                     Oper(obj)->op != O_COMMA &&     /* [(3),(3)] */
                                     !(Oper(obj)->op >= O_COMMAX && Oper(obj)->op <= O_COMMAK) /* (3),y */
                                    )) {
                                v1->val = values[vsp].val;
                                values[vsp].val = NULL;
                                continue;
                            }
                        }
                        am = (op == O_BRACKET) ? A_LI : A_I;
                        v1->val = apply_addressing(values[vsp].val, am, true);
                        val_destroy(values[vsp].val);
                        values[vsp].val = NULL;
                        continue;
                    }
                    if (tup) {
                        vsp--;
                        v1->val = values[vsp].val;
                        v1->epoint = values[vsp].epoint;
                        values[vsp].val = NULL;
                        continue;
                    }
                }
                if (args != 0) {
                    list = List(val_alloc((op == O_BRACKET) ? LIST_OBJ : TUPLE_OBJ));
                    list->len = args;
                    list->data = list_create_elements(list, args);
                    while ((args--) != 0) {
                        v2 = &values[vsp - 1];
                        list->data[args] = v2->val;
                        v2->val = NULL;
                        vsp--;
                    }
                } else list = List(val_reference((op == O_BRACKET) ? null_list : null_tuple));
                v1->val = Obj(list);
                continue;
            }
        case O_RBRACE:
        case O_DICT:
            {
                argcount_t args = 0;
                while (v1->val != &operators[O_BRACE].v) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp - 1 - args];
                }
                vsp -= args;
                v1->val = dictobj_parse(&values[vsp], args);
                continue;
            }
        case O_COND:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp - 1]; vsp--;
            if (vsp == 0) goto syntaxe;
            val = values[vsp - 1].val;
            if (val == true_value) {
            cond_true:
                values[vsp - 1].val = v1->val;
                values[vsp - 1].epoint = v1->epoint;
                v1->val = val;
                continue;
            }
            if (val == false_value) {
            cond_false:
                values[vsp - 1].val = v2->val;
                values[vsp - 1].epoint = v2->epoint;
                v2->val = val;
                continue;
            }
            {
                Obj *tmp = val->obj->truth(val, TRUTH_BOOL, &values[vsp - 1].epoint);
                if (tmp->obj != BOOL_OBJ) {
                    val_destroy(val);
                    values[vsp - 1].val = tmp;
                    continue;
                }
                val_destroy(tmp);
                if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, val, &values[vsp - 1].epoint);
                if (tmp == true_value) goto cond_true;
                goto cond_false;
            }
        case O_DCOND:
            {
                Funcargs tmp;
                vsp--;
                if (vsp == 0) goto syntaxe;
                vsp--;
                if (vsp == 0) goto syntaxe;
                v1 = &values[vsp - 1];
                tmp.val = v1;
                tmp.len = 3; /* assumes no referencing */
                tmp.v.obj = FUNCARGS_OBJ;

                epoint.pos = out->pos;
                oper.v1 = none_value; 
                oper.v2 = &tmp.v;
                oper.epoint = &epoint;
                oper.epoint2 = &v1->epoint;
                oper.epoint3 = &epoint;
                val = apply_condition(&oper);
                val_destroy(v1->val); v1->val = val;
                val_destroy(v1[1].val);
                val_destroy(v1[2].val);
                v1[1].val = NULL;
                v1[2].val = NULL;
                continue;
            }
        case O_QUEST:
        case O_DQUEST:
            vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp - 1];
            epoint.pos = out->pos;
            err_msg2(ERROR______EXPECTED,"':'", &epoint);
            val_replace(&v1->val, none_value);
            continue;
        case O_COLON:
            v2 = v1; v1 = &values[--vsp - 1];
            if (vsp == 0) goto syntaxe;
            if (v1->val->obj == COLONLIST_OBJ && v1->val->refcount == 1) {
                Colonlist *l1 = Colonlist(v1->val);
                Colonlist *list = new_colonlist();
                if (v2->val->obj == COLONLIST_OBJ && v2->val->refcount == 1) {
                    Colonlist *l2 = Colonlist(v2->val);
                    list->len = l1->len + l2->len;
                    if (list->len < l2->len) err_msg_out_of_memory(); /* overflow */
                    list->data = list_create_elements(list, list->len);
                    memcpy(list->data, l1->data, l1->len * sizeof *list->data);
                    memcpy(list->data + l1->len, l2->data, l2->len * sizeof *list->data);
                    l1->len = 0;
                    l2->len = 0;
                    val_destroy(v1->val); v1->val = Obj(list);
                    continue;
                }
                list->len = l1->len + 1;
                if (list->len < 1) err_msg_out_of_memory(); /* overflow */
                list->data = list_create_elements(list, list->len);
                memcpy(list->data, l1->data, l1->len * sizeof *list->data);
                list->data[l1->len] = v2->val;
                l1->len = 0;
                v2->val = v1->val;
                v1->val = Obj(list);
                continue;
            }
            if (v2->val->obj == COLONLIST_OBJ && v2->val->refcount == 1) {
                Colonlist *l2 = Colonlist(v2->val);
                Colonlist *list = new_colonlist();
                list->len = l2->len + 1;
                if (list->len < 1) err_msg_out_of_memory(); /* overflow */
                list->data = list_create_elements(list, list->len);
                list->data[0] = v1->val;
                memcpy(&list->data[1], l2->data, l2->len * sizeof *list->data);
                v1->val = Obj(list);
                l2->len = 0;
                continue;
            }
            {
                Colonlist *list = new_colonlist();
                list->len = 2;
                list->data = list_create_elements(list, 2);
                list->data[0] = v1->val;
                list->data[1] = v2->val;
                v1->val = Obj(list);
                v2->val = NULL;
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
            oper.v1 = v1->val;
            oper.v2 = none_value;
            oper.epoint = &v1->epoint;
            oper.epoint3 = &epoint;
            oper.inplace = (oper.v1->refcount == 1) ? oper.v1 : NULL;
            val = oper.v1->obj->calc1(&oper);
            val_destroy(v1->val); v1->val = val;
            v1->epoint.pos = out->pos;
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
            if (v1->val->obj != ADDRESS_OBJ && !v1->val->obj->iterable) {
                v1->val = new_address(v1->val, am);
            } else {
                val = apply_addressing(v1->val, am, true);
                val_destroy(v1->val); v1->val = val;
            }
            if (oper.op == O_HASH || oper.op == O_HASH_SIGNED) v1->epoint.pos = out->pos;
            continue;
        case O_SPLAT:   /* *  */
            if (i + 1 < ev->out.p) {
                Obj *o = ev->out.data[i + 1].val;
                if (o->obj != OPER_OBJ || (Oper(o)->op != O_RPARENT && Oper(o)->op != O_RBRACKET && Oper(o)->op != O_RBRACE && Oper(o)->op != O_FUNC && Oper(o)->op != O_INDEX && Oper(o)->op != O_COMMA)) {
                    if (o == &operators[O_MEMBER].v) {
                        v1->epoint.pos = out->pos;
                        continue;
                    }
                    epoint.pos = out->pos;
                    err_msg2(ERROR_EXPRES_SYNTAX, NULL, &epoint);
                    val_replace(&v1->val, none_value);
                    continue;
                }
            }
            if (v1->val->obj->iterable || v1->val->obj == ADDRLIST_OBJ) {
                struct iter_s iter;
                size_t k, len;
                argcount_t len2;
                Obj *tmp, *def;
                iter.data = v1->val; v1->val->obj->getiter(&iter);
                len = iter.len;

                if (v1->val->obj == DICT_OBJ && Dict(v1->val)->def != NULL) {
                    Colonlist *list = new_colonlist();
                    list->len = 2;
                    list->data = list->u.val;
                    list->data[0] = ref_default();
                    list->data[1] = val_reference(Dict(v1->val)->def);
                    def = Obj(list);
                    len++;
                    if (len < 1) err_msg_out_of_memory(); /* overflow */
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
        case O_LXOR: /* ^^ */
            v2 = v1; v1 = &values[--vsp - 1];
            if (vsp == 0) goto syntaxe;
            val = v1->val->obj->truth(v1->val, TRUTH_BOOL, &v1->epoint);
            if (val->obj != BOOL_OBJ) {
                val_destroy(v1->val); v1->val = val;
                continue;
            }
            if (diagnostics.strict_bool && v1->val->obj != BOOL_OBJ) err_msg_bool(ERROR_____CANT_BOOL, v1->val, &v1->epoint); /* TODO */
            {
                Obj *val2 = v2->val->obj->truth(v2->val, TRUTH_BOOL, &v2->epoint);
                if (val2->obj != BOOL_OBJ) {
                    val_destroy(v1->val); v1->val = val2;
                    val_destroy(val);
                    continue;
                }
                if (diagnostics.strict_bool && v2->val->obj != BOOL_OBJ) err_msg_bool(ERROR_____CANT_BOOL, v2->val, &v2->epoint); /* TODO */
                if (val == true_value) {
                    if (val2 == true_value) val_replace(&v1->val, false_value);
                } else {
                    val_replace(&v1->val, val2 == true_value ? v2->val : false_value);
                }
                val_destroy(val2);
            }
            val_destroy(val);
            continue;
        case O_IDENTITY: 
            v2 = v1; v1 = &values[--vsp - 1];
            if (vsp == 0) goto syntaxe;
            val = v1->val;
            if (val == none_value || val->obj == ERROR_OBJ) continue;
            val = v2->val;
            if (val == none_value || val->obj == ERROR_OBJ) {
                v2->val = v1->val;
                v1->val = val;
                continue;
            }
            val = truth_reference(v1->val == v2->val || v1->val->obj->same(v1->val, v2->val));
            val_destroy(v1->val); v1->val = val;
            continue;
        case O_MIN: /* <? */
        case O_MAX: /* >? */
            v2 = v1; v1 = &values[--vsp - 1];
            if (vsp == 0) goto syntaxe;
            epoint.pos = out->pos;
            oper.v1 = v1->val;
            oper.v2 = v2->val;
            oper.epoint = &v1->epoint;
            oper.epoint2 = &v2->epoint;
            oper.epoint3 = &epoint;
            oper.inplace = NULL;
            val = oper.v1->obj->calc2(&oper);
            if (val->obj != BOOL_OBJ) {
                val_destroy(v1->val); v1->val = val;
                continue;
            }
            if (val != true_value) {
                v1->epoint = v2->epoint;
                val_replace(&v1->val, v2->val);
            }
            val_destroy(val);
            continue;
        default: break;
        }
        v2 = v1; v1 = &values[--vsp - 1];
        if (vsp == 0) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX, NULL);
            ev->outp2 = i + 1;
            ev->values_len = 0;
            return false;
        }

        epoint.pos = out->pos;
        oper.v1 = v1->val;
        oper.v2 = v2->val;
        oper.epoint = &v1->epoint;
        oper.epoint2 = &v2->epoint;
        oper.epoint3 = &epoint;
        oper.inplace = (oper.v1->refcount == 1) ? v1->val : NULL;
        val = oper.v1->obj->calc2(&oper);
        val_destroy(v1->val); v1->val = val;
    }
    ev->outp2 = i;
    ev->values_len = vsp;
    return true;
}

struct values_s *get_val(void) {
    if (eval->values_p >= eval->values_len) return NULL;
    return &eval->values[eval->values_p++];
}

Obj *pull_val(struct linepos_s *epoint) {
    Obj *val;
    struct values_s *value = &eval->values[eval->values_p++];
    if (epoint != NULL) *epoint = value->epoint;
    val = value->val;
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
    eval->out.p = 0;
    eval->outp2 = 0;
    eval->values_p = eval->values_len = 0;

    if (arguments.tasmcomp) {
        if (get_exp_compat(stop)) return get_val2_compat(eval);
        return false;
    }

    ignore();
    ch = here();
    if (ch == 0 || ch == ';') return true;
    symbollist = 0;
    epoint.line = lpoint.line;
    out = eval->out;
    opr.data = eval->opr.data;
    opr.size = eval->opr.size;
    opr.p = 0;
    opr.data[0].op = O_COMMA;
    for (;;) {
        ignore(); ch = here(); epoint.pos = lpoint.pos;
        switch (ch) {
        case ',':
            if (stop != 4 || opr.p != 0) goto tryanon;
            lpoint.pos++;
            val = ref_default();
        push_continue:
            out.data[out.p].val = val;
            out.data[out.p++].pos = epoint.pos;
            if (out.p >= out.size) extend_out(&out);
            continue;
        case ')':
            if (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_TUPLE].v;goto tphack;}
                else if (o == O_PARENT || o == O_FUNC) goto other;
            }
            goto tryanon;
        case ']':
            if (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_LIST].v;goto lshack;}
                else if (o == O_BRACKET || o == O_INDEX) goto other;
            }
            goto tryanon;
        case '}':
            if (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (o == O_COMMA) {opr.p--;val = &operators[O_DICT].v;goto brhack;}
                else if (o == O_BRACE) goto other;
            }
            goto tryanon;
        case ':':
            if (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (o != O_PARENT && o != O_BRACKET && o != O_BRACE && o != O_FUNC && o != O_INDEX && o != O_COMMA) goto tryanon;
            }
            val = ref_default();
            goto push_other;
        case '(':
            if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) symbollist++;
        tphack2:
            lpoint.pos++;
            opr.data[opr.p].op = O_PARENT;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            val = &operators[O_PARENT].v;
            goto push_continue;
        case '[':
            if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) symbollist++;
        lshack2:
            lpoint.pos++;
            opr.data[opr.p].op = O_BRACKET;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            val = &operators[O_BRACKET].v;
            goto push_continue;
        case '{':
            lpoint.pos++;
            opr.data[opr.p].op = O_BRACE;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            val = &operators[O_BRACE].v;
            goto push_continue;
        case '+': op = O_POS; break;
        case '-': op = O_NEG; break;
        case '*': op = O_SPLAT; break;
        case '!': op = O_LNOT;break;
        case '~': op = O_INV; break;
        case '<': if (pline[lpoint.pos + 1] == '>') {lpoint.pos++;op = O_WORD;} else op = O_LOWER; break;
        case '>': if (pline[lpoint.pos + 1] == '`') {lpoint.pos++;op = O_HWORD;} else if (pline[lpoint.pos + 1] == '<') {lpoint.pos++;op = O_BSWORD;} else op = O_HIGHER; break;
        case '#': op = (pline[lpoint.pos + 1] == '+' || pline[lpoint.pos + 1] == '-') ? O_HASH_SIGNED : O_HASH; break;
        case '`': op = O_BANK; break;
        case '^': op = O_STRING; if (diagnostics.deprecated) err_msg2(ERROR____OLD_STRING, NULL, &lpoint); break;
        case '$': val = get_hex(&epoint); goto push_other;
        case '%': 
            if ((pline[lpoint.pos + 1] & 0xfe) == 0x30 || (pline[lpoint.pos + 1] == '.' && (pline[lpoint.pos + 2] & 0xfe) == 0x30)) { 
                val = get_bin(&epoint); goto push_other; 
            }
            goto tryanon;
        case '"':
        case '\'': val = get_string(&epoint); goto push_other;
        case '?':
            if (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
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
                    /* fall through */
                default:
                    goto tryanon;
                }
            }
            val = get_float(&epoint);
            goto push_other;
        case '0':
            if (diagnostics.leading_zeros && (pline[lpoint.pos + 1] ^ 0x30) < 10) err_msg2(ERROR_LEADING_ZEROS, NULL, &lpoint);
            /* fall through */
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
                if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) {
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
            db = opr.p;
            while (opr.p != 0 && opr.data[opr.p - 1].op == O_POS) opr.p--;
            if (db != opr.p) {
                ssize_t as = (ssize_t)(db - opr.p) - 1;
                if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) {
                    val = new_anonsymbol(as);
                } else {
                    val = resolv_anonlabel(as, opr.data[opr.p].pos);
                }
                epoint.pos = opr.data[opr.p].pos;
                goto push_other;
            }
            while (opr.p != 0 && opr.data[opr.p - 1].op == O_NEG) opr.p--;
            if (db != opr.p) {
                ssize_t as = -(ssize_t)(db - opr.p);
                if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) {
                    val = new_anonsymbol(as);
                } else {
                    val = resolv_anonlabel(as, opr.data[opr.p].pos);
                }
                epoint.pos = opr.data[opr.p].pos;
                goto push_other;
            }
            if (opr.p != 0) {
                if (opr.data[opr.p - 1].op == O_COLON) {
                    val = ref_default();
                    goto push_other;
                }
                if (opr.data[opr.p - 1].op == O_SPLAT) {
                    opr.p--;
                    if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) {
                        val = new_starsymbol(opr.data[opr.p].pos);
                    } else {
                        val = get_star();
                    }
                    epoint.pos = opr.data[opr.p].pos;
                    goto push_other;
                }
                epoint.pos = opr.data[opr.p - 1].pos;
            }
            err_msg2(ERROR______EXPECTED, "an expression is", &lpoint);
            goto error;
        }
        if (opr.p != 0 && opr.data[opr.p - 1].op == O_SPLAT) {
            opr.p--;
            lpoint.pos = epoint.pos;
            if ((opr.p != 0 && opr.data[opr.p - 1].op == O_MEMBER) || symbollist != 0) {
                val = new_starsymbol(opr.data[opr.p].pos);
            } else {
                val = get_star();
            }
            epoint.pos = opr.data[opr.p].pos;
            goto push_other;
        }
        lpoint.pos++;
    rtl:
        opr.data[opr.p].op = op;
        opr.data[opr.p++].pos = epoint.pos;
        if (opr.p >= opr.size) extend_opr(&opr);
        continue;
    push_other:
        out.data[out.p].val = val;
        out.data[out.p++].pos = epoint.pos;
        if (out.p >= out.size) extend_out(&out);
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
                        while (opr.p != 0) {
                            Oper_types o = opr.data[opr.p - 1].op;
                            if (prec > operators[o].prio) break;
                            if (o == O_COLON2 || o == O_COND || o == O_DCOND) break;
                            out.data[out.p].val = &operators[o].v;
                            out.data[out.p++].pos = opr.data[--opr.p].pos;
                            if (out.p >= out.size) extend_out(&out);
                        }
                        opr.data[opr.p].op = op;
                        opr.data[opr.p++].pos = epoint.pos;
                        if (opr.p >= opr.size) extend_opr(&opr);
                        goto other;
                    } 
                }
            } else llen = 0;
            prec = operators[O_COMMA].prio;
            while (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (prec > operators[o].prio) break;
                out.data[out.p].val = &operators[o].v;
                out.data[out.p++].pos = opr.data[--opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
            }
            if (opr.p == 0) {
                if (stop == 1) {lpoint.pos = epoint.pos;break;}
            }
            out.data[out.p].val = &operators[O_COMMA].v;
            out.data[out.p++].pos = epoint.pos;
            if (out.p >= out.size) extend_out(&out);
            opr.data[opr.p].op = O_COMMA;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            if (llen != 0) {
                epoint.pos++;
                goto as_symbol;
            }
            continue;
        case '(':
            lpoint.pos++;
            prec = operators[O_MEMBER].prio;
            while (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (prec > operators[o].prio) break;
                out.data[out.p].val = &operators[o].v;
                out.data[out.p++].pos =  opr.data[--opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
            }
            opr.data[opr.p].op = O_FUNC;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            if (symbollist != 0) symbollist++;
            val = &operators[O_PARENT].v;
            goto push_continue;
        case '[':
            lpoint.pos++;
            prec = operators[O_MEMBER].prio;
            while (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (prec > operators[o].prio) break;
                out.data[out.p].val = &operators[o].v;
                out.data[out.p++].pos = opr.data[--opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
            }
            opr.data[opr.p].op = O_INDEX;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
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
            lpoint.pos++;
            op = O_COLON;
            prec = operators[O_COLON].prio + 1;
            while (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (prec > operators[o].prio) break;
                out.data[out.p].val = &operators[o].v;
                out.data[out.p++].pos = opr.data[--opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
            }
            if (opr.p != 0) {
                if (opr.data[opr.p - 1].op == O_QUEST) { opr.data[opr.p - 1].op = O_COND; op = O_COLON2;}
                else if (opr.data[opr.p - 1].op == O_DQUEST) { opr.data[opr.p - 1].op = O_DCOND; op = O_COLON2;}
            }
            opr.data[opr.p].op = op;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
            continue;
        case '=':
            if (pline[lpoint.pos + 1] != '=') {
                if (diagnostics.old_equal) err_msg2(ERROR_____OLD_EQUAL, NULL, &lpoint);
                lpoint.pos--;
                op = O_EQ;
            } else {
                op = pline[lpoint.pos + 2] != '=' ? O_EQ : O_IDENTITY;
            }
        push2:
            lpoint.pos += operators[op].len;
        push2a:
            prec = operators[op].prio;
        push3:
            while (opr.p != 0) {
                Oper_types o = opr.data[opr.p - 1].op;
                if (prec > operators[o].prio) break;
                out.data[out.p].val = &operators[o].v;
                out.data[out.p++].pos = opr.data[--opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
            }
            opr.data[opr.p].op = op;
            opr.data[opr.p++].pos = epoint.pos;
            if (opr.p >= opr.size) extend_opr(&opr);
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
            if (pline[lpoint.pos + 1] == '=') {op = O_NE;goto push2;}
            err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        case ')':
            val = &operators[O_RPARENT].v;
        tphack:
            if (symbollist != 0) symbollist--;
            do {
                const char *mis;
                if (opr.p != 0) {
                    switch (opr.data[opr.p - 1].op) {
                    case O_FUNC:
                        val = &operators[O_FUNC].v;
                        /* fall through */
                    case O_PARENT:
                        lpoint.pos++;
                        epoint.pos = opr.data[--opr.p].pos;
                        goto push_other;
                    case O_BRACKET:
                    case O_INDEX: mis = "']'"; break;
                    case O_BRACE: mis = "'}'"; break;
                    default:
                        out.data[out.p].val = &operators[opr.data[--opr.p].op].v;
                        out.data[out.p++].pos = opr.data[opr.p].pos;
                        if (out.p >= out.size) extend_out(&out);
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
                if (opr.p != 0) {
                    switch (opr.data[opr.p - 1].op) {
                    case O_INDEX:
                        val = &operators[O_INDEX].v;
                        /* fall through */
                    case O_BRACKET:
                        lpoint.pos++;
                        epoint.pos = opr.data[--opr.p].pos;
                        goto push_other;
                    case O_PARENT:
                    case O_FUNC: mis = "')'"; break;
                    case O_BRACE: mis = "'}'"; break;
                    default:
                        out.data[out.p].val = &operators[opr.data[--opr.p].op].v;
                        out.data[out.p++].pos = opr.data[opr.p].pos;
                        if (out.p >= out.size) extend_out(&out);
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
                if (opr.p != 0) {
                    switch (opr.data[opr.p - 1].op) {
                    case O_BRACE:
                        lpoint.pos++;
                        epoint.pos = opr.data[--opr.p].pos;
                        goto push_other;
                    case O_PARENT:
                    case O_FUNC: mis = "')'"; break;
                    case O_BRACKET:
                    case O_INDEX: mis = "']'"; break;
                    default:
                        out.data[out.p].val = &operators[opr.data[--opr.p].op].v;
                        out.data[out.p++].pos = opr.data[opr.p].pos;
                        if (out.p >= out.size) extend_out(&out);
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
            err_msg2(ERROR______EXPECTED, "an operator is", &epoint);
            goto error;
        }
        while (opr.p != 0) {
            const char *mis;
            switch (opr.data[opr.p - 1].op) {
            case O_PARENT:
            case O_FUNC: mis = "')'"; break;
            case O_BRACKET:
            case O_INDEX: mis = "']'"; break;
            case O_BRACE: mis = "'}'"; break;
            default:
                out.data[out.p].val = &operators[opr.data[--opr.p].op].v;
                out.data[out.p++].pos = opr.data[opr.p].pos;
                if (out.p >= out.size) extend_out(&out);
                continue;
            }
            err_msg2(ERROR______EXPECTED, mis, &epoint); goto error;
        }
        eval->opr = opr;
        eval->out = out;
        return get_val2(eval);
    error:
        break;
    }
    eval->opr = opr;
    eval->out = out;
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


Obj *get_vals_tuple(void) {
    argcount_t i, len = get_val_remaining();
    Tuple *list;

    switch (len) {
    case 0:
        return val_reference(null_tuple);
    case 1:
        return pull_val(NULL);
    default:
        break;
    }
    list = new_tuple(len);
    for (i = 0; i < len; i++) {
        list->data[i] = pull_val(NULL);
    }
    return Obj(list);
}

Obj *get_vals_addrlist(struct linepos_s *epoints) {
    argcount_t i, j, len = get_val_remaining();
    Addrlist *list;
    Obj *val;

    switch (len) {
    case 0:
        return val_reference(null_addrlist);
    case 1:
        val = pull_val(&epoints[0]);
        if (val->obj == ADDRLIST_OBJ) {
            list = Addrlist(val);
            i = list->len < 3 ? (argcount_t)list->len : 3;
            for (j = 1; j < i; j++) epoints[j] = epoints[0];
        }
        return val;
    default:
        break;
    }
    list = new_addrlist();
    list->data = list_create_elements(list, len);
    for (i = j = 0; j < len; j++) {
        Obj *val2 = pull_val((i < 3) ? &epoints[i] : NULL);
        if (val2->obj == REGISTER_OBJ && Register(val2)->len == 1 && i != 0) {
            Address_types am = register_to_indexing(Register(val2)->data[0]);
            if (am != A_NONE) {
                val_destroy(val2);
                val2 = apply_addressing(list->data[i - 1], am, true);
                val_destroy(list->data[i - 1]);
                list->data[i - 1] = val2;
                continue;
            }
        }
        list->data[i++] = val2;
    }
    if (i == 1) {
        Obj *val2 = list->data[0];
        list->len = 0;
        val_destroy(Obj(list));
        return val2;
    }
    list->len = i;
    return Obj(list);
}

void eval_enter(void) {
    evx_p++;
    if (evx_p >= evxnum) {
        evxnum++;
        if (/*evxnum < 1 ||*/ evxnum > SIZE_MAX / sizeof *evx) err_msg_out_of_memory(); /* overflow */
        evx = (struct eval_context_s **)reallocx(evx, evxnum * sizeof *evx);
        eval = (struct eval_context_s *)mallocx(sizeof *eval);
        eval->values = NULL;
        eval->values_size = 0;
        eval->out.size = 16;
        eval->out.data = (struct out_s *)mallocx(16 * sizeof *eval->out.data);
        eval->out.p = 0;
        eval->opr.size = 16;
        eval->opr.data = (struct opr_s *)mallocx(16 * sizeof *eval->opr.data);
        eval->opr.p = 0;
        eval->outp2 = 0;
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
