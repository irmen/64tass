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

#include "eval.h"
#include <string.h>
#include <math.h>
#include "file.h"
#include "section.h"
#include "encoding.h"
#include "macro.h"
#include "variables.h"
#include "64tass.h"
#include "misc.h"
#include "unicode.h"
#include "listing.h"

#include "floatobj.h"
#include "boolobj.h"

#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE >= 500 || _XOPEN_SOURCE && _XOPEN_SOURCE_EXTENDED || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#define cbrt(a) pow((a), 1.0/3.0)
#endif 
#if _XOPEN_SOURCE >= 600 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
double round(double);
double trunc(double);
inline double round(double a) {return (a < 0.0) ? ceil(a-0.5) : floor(a+0.5);}
inline double trunc(double a) {return (a > 0.0) ? floor(a) : ceil(a);}
#endif 
#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE >= 600 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
double sinh(double);
double cosh(double);
double tanh(double);
inline double sinh(double a) {return (exp(a) - exp(-a)) / 2.0;}
inline double cosh(double a) {return (exp(a) + exp(-a)) / 2.0;}
inline double tanh(double a) {return sinh(a) / cosh(a);}
#endif
#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
double hypot(double, double);
inline double hypot(double a, double b) {return sqrt(a*a+b*b);}
#endif

size_t get_label(void) {
    uint32_t ch;
    unsigned int l;
    const struct properties_s *prop;
    const uint8_t *s = pline + lpoint.pos;
    const uint8_t *e;
    static const uint8_t typ[256] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 0 */
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 1 */
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 2 */
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, /* 3 */
        0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 4 */
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 2, /* 5 */
        0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 6 */
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, /* 7 */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* 8 */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* 9 */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* a */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* b */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* c */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* d */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /* e */
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3  /* f */
    };

    switch (typ[*s]) {
    default:
    case 1:
    case 0: return 0;
    case 2:
        e = s;
        s++;
        break;
    case 3:
        if (!arguments.toascii) return 0;
        l = utf8in(s, &ch);
        prop = uget_property(ch);
        if (!(prop->property & id_Start)) return 0;
        e = s;
        s += l;
    }
    for (;;) {
        switch (typ[*s]) {
        default:
        case 0: break;
        case 1:
        case 2: s++; continue;
        case 3:
            if (!arguments.toascii) break;
            l = utf8in(s, &ch);
            prop = uget_property(ch);
            if (!(prop->property & (id_Continue | id_Start))) break;
            s += l;
            continue;
        }
        break;
    }
    lpoint.pos = s - pline;
    return s - e;
}

static MUST_CHECK value_t get_dec(void) {
    value_t v;
    size_t len;
    while (here() == 0x30) lpoint.pos++;
    v = int_from_decstr(pline + lpoint.pos, &len);
    lpoint.pos += len;
    return v;
}

static MUST_CHECK value_t get_exponent(double real, linepos_t epoint) {
    int base;
    value_t v;

    switch (here() | 0x20) {
    case 'p': base = 2; break;
    case 'e': base = 10; break;
    default: base = 0;
    }
    if (base) {
        int neg = 0;
        neg = (pline[lpoint.pos + 1] == '-');
        if (neg || pline[lpoint.pos + 1] == '+') {
            if ((pline[lpoint.pos + 2] ^ 0x30) < 10) lpoint.pos++;
        }
        if ((pline[lpoint.pos + 1] ^ 0x30) < 10) {
            ival_t expo;
            value_t err;
            size_t len;
            lpoint.pos++;

            v = int_from_decstr(pline + lpoint.pos, &len);
            err = v->obj->ival(v, &expo, 8*sizeof(uval_t), &lpoint);
            val_destroy(v);
            if (err) return err;
            lpoint.pos += len;

            if (neg) expo = -expo;
            if (expo) real *= pow(base, expo);
        }
    }
    if (real == HUGE_VAL) {
        return new_error_obj(ERROR_CONSTNT_LARGE, epoint);
    }
    return float_from_double(real);
}

static double toreal_destroy(value_t v, linepos_t epoint) {
    value_t err = FLOAT_OBJ->create(v, epoint);
    double real;
    if (err->obj != FLOAT_OBJ) {
        err_msg_output(err);
        real = 0;
    } else {
        real = err->u.real;
    }
    val_destroy(err);
    val_destroy(v);
    return real;
}

static MUST_CHECK value_t get_exponent2(value_t v, linepos_t epoint) {
    double real;
    switch (here() | 0x20) {
    case 'e':
    case 'p':
        if (pline[lpoint.pos + 1] == '-' || pline[lpoint.pos + 1] == '+') {
            if ((pline[lpoint.pos + 2] ^ 0x30) < 10) {
                real = toreal_destroy(v, &lpoint);
                return get_exponent(real, epoint);
            }
        } else if ((pline[lpoint.pos + 1] ^ 0x30) < 10) {
            real = toreal_destroy(v, &lpoint);
            return get_exponent(real, epoint);
        }
    default: break;
    }
    return v;
}

static MUST_CHECK value_t get_hex(linepos_t epoint) {
    size_t len;
    value_t v = bits_from_hexstr(pline + lpoint.pos, &len);

    if (pline[lpoint.pos + len] == '.' && pline[lpoint.pos + len + 1] != '.') {
        double real, real2;
        real = toreal_destroy(v, &lpoint);
        lpoint.pos += len + 1;

        v = bits_from_hexstr(pline + lpoint.pos, &len);
        real2 = toreal_destroy(v, &lpoint);
        lpoint.pos += len;

        if (real2) real += real2 * pow(16.0, -(double)len);
        return get_exponent(real, epoint);
    }
    lpoint.pos += len;
    return get_exponent2(v, epoint);
}

static MUST_CHECK value_t get_bin(linepos_t epoint) {
    size_t len;
    value_t v = bits_from_binstr(pline + lpoint.pos, &len);

    if (pline[lpoint.pos + len] == '.' && pline[lpoint.pos + len + 1] != '.') {
        double real, real2;
        real = toreal_destroy(v, &lpoint);
        lpoint.pos += len + 1;

        v = bits_from_binstr(pline + lpoint.pos, &len);
        real2 = toreal_destroy(v, &lpoint);
        lpoint.pos += len;

        if (real2) real += real2 * pow(2.0, -(double)len);
        return get_exponent(real, epoint);
    }
    lpoint.pos += len;
    return get_exponent2(v, epoint);
}

static MUST_CHECK value_t get_float(linepos_t epoint) {
    size_t len;
    value_t v;

    while (here() == 0x30) lpoint.pos++;
    v = int_from_decstr(pline + lpoint.pos, &len);
    if (pline[lpoint.pos + len] == '.' && pline[lpoint.pos + len + 1] != '.') {
        double real, real2;
        real = toreal_destroy(v, &lpoint);
        lpoint.pos += len + 1;

        v = int_from_decstr(pline + lpoint.pos, &len);
        real2 = toreal_destroy(v, &lpoint);
        lpoint.pos += len;

        if (real2) real += real2 * pow(10.0, -(double)len);
        return get_exponent(real, epoint);
    }
    lpoint.pos += len;
    return get_exponent2(v, epoint);
}

static MUST_CHECK value_t get_string(void) {
    size_t len;
    value_t v = str_from_str(pline + lpoint.pos, &len);
    lpoint.pos += len;
    return v;
}

void touch_label(struct label_s *tmp) {
    if (referenceit) tmp->ref = 1;
    tmp->usepass = pass;
}

static MUST_CHECK value_t get_star(linepos_t epoint) {
    struct star_s *tmp;
    int labelexists;

    tmp=new_star(vline, &labelexists);
    if (labelexists && tmp->addr != star) {
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = 0;
    }
    tmp->addr=star;
    return int_from_uval(star);
}

static size_t evxnum, evx_p;
static struct eval_context_s {
    struct values_s *values;
    size_t values_len;
    size_t values_p;
    size_t values_size;
    size_t outp, outp2;
    int gstop;
    struct values_s *o_out;
    size_t out_size;
} **evx;

static struct eval_context_s *eval;

static inline void push_oper(value_t val, linepos_t epoint) {
    if (eval->outp >= eval->out_size) {
        size_t i;
        eval->out_size += 64;
        eval->o_out = (struct values_s *)realloc(eval->o_out, eval->out_size * sizeof(eval->o_out[0]));
        if (!eval->o_out || eval->out_size < 64 || eval->out_size > SIZE_MAX / sizeof(eval->o_out[0])) err_msg_out_of_memory(); /* overflow */
        for (i = eval->outp + 1; i < eval->out_size; i++) eval->o_out[i].val = NULL;
    } else if (eval->o_out[eval->outp].val) val_destroy(eval->o_out[eval->outp].val);
    eval->o_out[eval->outp].val = val;
    eval->o_out[eval->outp++].epoint = *epoint;
}

static int get_exp_compat(int *wd, int stop) {/* length in bytes, defined */
    char ch;

    value_t conv, conv2;
    struct values_s o_oper[256];
    uint8_t operp = 0;
    struct linepos_s epoint, cpoint = {0, 0};
    value_t val;
    size_t llen;
    int first;
    str_t ident;
    struct label_s *l;

    *wd=3;    /* 0=byte 1=word 2=long 3=negative/too big */

    eval->outp = 0;
    o_oper[0].val = &o_COMMA;
rest:
    ignore();
    conv = conv2 = NULL;
    first = (here() == '(') && (stop == 3 || stop == 4);
    if (!eval->outp && here() == '#') {
        conv2 = &o_HASH; lpoint.pos++;
    }
    switch (here()) {
    case 0:
    case ';': return 1;
    case '!':*wd=1;lpoint.pos++;break;
    case '<': conv = &o_LOWER; cpoint = lpoint; lpoint.pos++;break; 
    case '>': conv = &o_HIGHER;cpoint = lpoint; lpoint.pos++;break; 
    }
    for (;;) {
        ignore();ch = here(); epoint=lpoint;

        switch (ch) {
        case '(': lpoint.pos++;o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_PARENT;continue;
        case '$': lpoint.pos++;push_oper(get_hex(&epoint), &epoint);goto other;
        case '%': lpoint.pos++;push_oper(get_bin(&epoint), &epoint);goto other;
        case '"': push_oper(get_string(), &epoint);goto other;
        case '*': lpoint.pos++;push_oper(get_star(&epoint), &epoint);goto other;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            push_oper(get_dec(), &epoint);goto other;
        default: 
            if (!get_label()) {
                if (operp) epoint = o_oper[operp-1].epoint;
                goto syntaxe;
            }
            break;
        }
    as_ident:
        ident.data = pline + epoint.pos;
        ident.len = lpoint.pos - epoint.pos;
        l = find_label(&ident);
        if (l) {
            touch_label(l);
            l->shadowcheck = 1;
            push_oper(val_reference(l->value), &epoint);
        } else {
            val = new_error_obj(ERROR___NOT_DEFINED, &epoint);
            val->u.error.u.notdef.ident = ident;
            val->u.error.u.notdef.label = current_context;
            val->u.error.u.notdef.down = 1;
            push_oper(val, &epoint);
        }
    other:
        if (stop != 2) ignore();
        ch = here(); epoint=lpoint;

        while (operp && o_oper[operp-1].val != &o_PARENT) {
            operp--;
            push_oper(o_oper[operp].val, &o_oper[operp].epoint);
        }
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = get_label();
            if (llen == 1) {
                uint8_t c = pline[epoint.pos + 1];
                if (!arguments.casesensitive) c |= 0x20;
                switch (c) {
                case 'x': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_COMMAX;goto other;
                case 'y': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_COMMAY;goto other;
                default: break;
                }
            }
            if (conv) push_oper(conv, &cpoint);
            if (conv2) push_oper(conv2, &cpoint);
            if (stop == 1) {lpoint = epoint;break;}
            if (llen) {
                epoint.pos++;
                goto as_ident;
            }
            goto rest;
        case '&': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_AND; lpoint.pos++;continue;
        case '.': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_OR; lpoint.pos++;continue;
        case ':': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_XOR; lpoint.pos++;continue;
        case '*': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_MUL; lpoint.pos++;continue;
        case '/': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_DIV; lpoint.pos++;continue;
        case '+': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_ADD; lpoint.pos++;continue;
        case '-': o_oper[operp].epoint = epoint; o_oper[operp++].val = &o_SUB; lpoint.pos++;continue;
        case ')':
            if (!operp) {err_msg2(ERROR______EXPECTED, "(", &lpoint); goto error;}
            lpoint.pos++;
            operp--;
            if (!operp && first) {
                o_oper[operp].epoint = o_oper[operp].epoint; o_oper[operp++].val = &o_TUPLE;
                first = 0;
            }
            goto other;
        case 0:
        case ';':
        case '\t':
        case ' ':
            if (conv) push_oper(conv, &cpoint);
            if (conv2) push_oper(conv2, &cpoint);
            break;
        default: goto syntaxe;
        }
        if (!operp) return 1;
        epoint = o_oper[operp - 1].epoint;
        err_msg2(ERROR______EXPECTED, ")", &epoint); goto error;
    syntaxe:
        err_msg2(ERROR_EXPRES_SYNTAX, NULL, &epoint);
    error:
        break;
    }
    return 0;
}

static int get_val2_compat(struct eval_context_s *ev) {/* length in bytes, defined */
    size_t vsp = 0;
    enum oper_e op;
    value_t op2;
    size_t i;
    value_t val;
    struct values_s *v1, *v2;
    struct values_s *o_out;
    struct values_s *values;

    ev->values_p = 0;
    values = ev->values;

    for (i = ev->outp2; i < ev->outp; i++) {
        o_out = &ev->o_out[i];
        val = o_out->val;
        if (val->obj != OPER_OBJ) {
            if (vsp >= ev->values_size) {
                size_t j = ev->values_size;
                ev->values_size += 16;
                ev->values = values = (struct values_s *)realloc(ev->values, ev->values_size * sizeof(struct values_s));
                if (!values || ev->values_size < 16 || ev->values_size > SIZE_MAX / sizeof(struct values_s)) err_msg_out_of_memory(); /* overflow */
                for (; j < ev->values_size; j++) ev->values[j].val = NULL;
            }
            o_out->val = values[vsp].val;
            values[vsp].val = val;
            values[vsp++].epoint = o_out->epoint;
            continue;
        }
        op2 = val;
        op = op2->u.oper.op;

        if (vsp < 1) goto syntaxe;
        v1 = &values[vsp-1];
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
                    val = val_alloc(ADDRESS_OBJ);
                    val->u.addr.type = v1->val->u.addr.type << 4;
                    val->u.addr.type |= (op == O_TUPLE) ? A_I : (op == O_COMMAX) ? A_XR : A_YR;
                    val->u.addr.val = val_reference(v1->val->u.addr.val);
                    val_destroy(v1->val); v1->val = val;
                    v1->epoint = o_out->epoint;
                    continue;
                default:break;
                }
                err_msg_invalid_oper(op2, v1->val, NULL, &o_out->epoint);
                val_replace(&v1->val, none_value); 
                break;
            case T_CODE:
            case T_INT:
            case T_BITS:
            case T_STR:
            case T_BOOL:
                {
                    uint16_t val1;
                    uval_t uval;
                    value_t err;
                    err = v1->val->obj->uval(v1->val, &uval, 8*sizeof(uval_t), &v1->epoint);
                    if (err) {
                        val_destroy(v1->val);
                        v1->val = err;
                        break;
                    }
                    val1 = uval;

                    switch (op) {
                    case O_HASH:
                    case O_COMMAX:
                    case O_COMMAY:
                        val = val_alloc(ADDRESS_OBJ);
                        val->u.addr.type = (op == O_HASH) ? A_IMMEDIATE : (op == O_COMMAX) ? A_XR : A_YR;
                        val->u.addr.val = val_reference(v1->val);
                        val_destroy(v1->val); v1->val = val;
                        v1->epoint = o_out->epoint;
                        continue;
                    case O_HIGHER: val1 >>= 8;
                    case O_LOWER: val1 = (uint8_t)val1;break;
                    case O_TUPLE:
                        val = val_alloc(ADDRESS_OBJ);
                        val->u.addr.type = A_I;
                        val->u.addr.val = val_reference(v1->val);
                        val_destroy(v1->val); v1->val = val;
                        v1->epoint = o_out->epoint;
                        continue;
                    default: break;
                    }
                    val_destroy(v1->val);
                    v1->val = int_from_uval(val1);
                    break;
                }
            default:
                err_msg_invalid_oper(op2, v1->val, NULL, &o_out->epoint);
                val_replace(&v1->val, none_value); 
            case T_ERROR:
            case T_NONE:break;
            }
            v1->epoint = o_out->epoint;
            continue;
        default:break;
        }
        if (vsp < 2) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            ev->outp2 = ev->outp;
            ev->values_len = 0;
            return 0;
        }
        v2 = v1; v1 = &values[--vsp-1];
        switch (v1->val->obj->type) {
        case T_INT:
        case T_BITS:
        case T_CODE:
        case T_STR:
        case T_BOOL:
        case T_ADDRESS:
            switch (v2->val->obj->type) {
            case T_INT:
            case T_BITS:
            case T_CODE:
            case T_STR:
            case T_BOOL:
            case T_ADDRESS:
                {
                    uint16_t val1, val2;
                    uval_t uval;
                    value_t err;
                    err = v1->val->obj->uval(v1->val, &uval, 8*sizeof(uval_t), &v1->epoint);
                    if (err) {
                        val_destroy(v1->val);
                        v1->val = err;
                        continue;
                    }
                    val1 = uval;
                    err = v2->val->obj->uval(v2->val, &uval, 8*sizeof(uval_t), &v2->epoint);
                    if (err) {
                        val_destroy(v1->val);
                        v1->val = err;
                        continue;
                    }
                    val2 = uval;

                    switch (op) {
                    case O_MUL: val1 *= val2; break;
                    case O_DIV: if (!val2) {
                        val = new_error_obj(ERROR_DIVISION_BY_Z, &o_out->epoint);
                        val_destroy(v1->val); v1->val = val;
                        continue;
                    } else val1 /= val2; break;
                    case O_ADD: val1 += val2; break;
                    case O_SUB: val1 -= val2; break;
                    case O_AND: val1 &= val2; break;
                    case O_OR:  val1 |= val2; break;
                    case O_XOR: val1 ^= val2; break;
                    default: break;
                    }
                    val_destroy(v1->val);
                    v1->val = int_from_uval(val1);
                    continue;
                }
            default: err_msg_invalid_oper(op2, v1->val, v2->val, &o_out->epoint); break;
            case T_ERROR:
            case T_NONE:
                val_replace(&v1->val, v2->val);
                continue;
            }
            break;
        default:
            err_msg_invalid_oper(op2, v1->val, v2->val, &o_out->epoint); break;
        case T_ERROR:
        case T_NONE: continue;
        }
        val_replace(&v1->val, none_value); continue;
    }
    ev->outp2 = i;
    ev->values_len = vsp;
    return 1;
}

MUST_CHECK value_t indexoffs(const value_t v1, size_t len, size_t *offs, linepos_t epoint) {
    ival_t ival;
    value_t err;
    err = v1->obj->ival(v1, &ival, 8*sizeof(ival_t), epoint);
    if (err) return err;

    if (ival >= 0) {
        if ((size_t)ival < len) {
            *offs = ival;
            return NULL;
        }
    } else {
        if ((size_t)-ival <= len) {
            *offs = len + ival;
            return NULL;
        }
    }
    return new_error_obj(ERROR___INDEX_RANGE, epoint);
}

MUST_CHECK value_t sliceparams(const value_t v2, size_t len, size_t *olen, ival_t *offs2, ival_t *end2, ival_t *step2, linepos_t epoint) {
    value_t err;
    ival_t offs, end, step = 1;
    if (v2->u.list.len > 3 || v2->u.list.len < 1) {
        err_msg_argnum(v2->u.list.len, 1, 3, epoint);
        return val_reference(none_value);
    }
    end = (ival_t)len;
    if (v2->u.list.len > 2) {
        if (v2->u.list.data[2]->obj != DEFAULT_OBJ) {
            err = v2->u.list.data[2]->obj->ival(v2->u.list.data[2], &step, 8*sizeof(ival_t), epoint);
            if (err) return err;
            if (step == 0) {
                return new_error_obj(ERROR_NO_ZERO_VALUE, epoint);
            }
        }
    }
    if (v2->u.list.len > 1) {
        if (v2->u.list.data[1]->obj == DEFAULT_OBJ) end = (step > 0) ? (ival_t)len : -1;
        else {
            err = v2->u.list.data[1]->obj->ival(v2->u.list.data[1], &end, 8*sizeof(ival_t), epoint);
            if (err) return err;
            if (end >= 0) {
                if (end > (ival_t)len) end = len;
            } else {
                if (end < 0) end += len;
            }
            if (end < -1) end = -1;
        }
    } else end = len;
    if (v2->u.list.data[0]->obj == DEFAULT_OBJ) offs = (step > 0) ? 0 : len - 1;
    else {
        err = v2->u.list.data[0]->obj->ival(v2->u.list.data[0], &offs, 8*sizeof(ival_t), epoint);
        if (err) return err;
        if (offs >= 0) {
            if (offs > (ival_t)len - (step < 0)) offs = len - (step < 0);
        } else {
            if (offs < 0) offs += len;
        }
        if (offs < - (step < 0)) offs = - (step < 0);
    }

    if (step > 0) {
        if (offs > end) offs = end;
        *olen = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        *olen = (offs - end - step - 1) / -step;
    }

    *offs2 = offs;
    *end2 = end;
    *step2 = step;
    return NULL;
}

static MUST_CHECK value_t apply_addressing(value_t v1, enum atype_e am) {
    size_t i;
    value_t *vals, v;

    switch (v1->obj->type) {
    case T_ADDRESS:
        v = val_alloc(ADDRESS_OBJ);
        v->u.addr.val = val_reference(v1->u.addr.val);
        v->u.addr.type = am | (v1->u.addr.type << 4);
        return v;
    case T_LIST:
    case T_TUPLE:
        v = val_alloc(v1->obj);
        vals = list_create_elements(v, v1->u.list.len);
        for (i = 0; i < v1->u.list.len; i++) {
            vals[i] = apply_addressing(v1->u.list.data[i], am);
        }
        v->u.list.len = v1->u.list.len;
        v->u.list.data = vals;
        return v;
    case T_ERROR:
        err_msg_output(v1);
        v1 = none_value;
        /* fall through */
    default:
        v = val_alloc(ADDRESS_OBJ);
        v->u.addr.val = val_reference(v1);
        v->u.addr.type = am;
        return v;
    }
}

static int get_val2(struct eval_context_s *ev) {
    size_t vsp = 0;
    size_t i;
    enum oper_e op;
    value_t op2;
    struct values_s *v1, *v2;
    int stop = ev->gstop == 3 || ev->gstop == 4;
    struct values_s *o_out;
    value_t val, err;
    struct values_s *values;
    struct oper_s oper;
    enum atype_e am;

    ev->values_p = 0;
    values = ev->values;

    for (i = ev->outp2; i < ev->outp; i++) {
        o_out = &ev->o_out[i];
        val = o_out->val;
        if (val->obj != OPER_OBJ || val == &o_PARENT || val == &o_BRACKET || val == &o_BRACE) {
            if (vsp >= ev->values_size) {
                size_t j = ev->values_size;
                ev->values_size += 16;
                ev->values = values = (struct values_s *)realloc(values, ev->values_size * sizeof(struct values_s));
                if (!values || ev->values_size < 16 || ev->values_size > SIZE_MAX / sizeof(struct values_s)) err_msg_out_of_memory(); /* overflow */
                for (; j < ev->values_size; j++) ev->values[j].val = NULL;
            }
            o_out->val = values[vsp].val;
            values[vsp].val = val;
            values[vsp++].epoint = o_out->epoint;
            continue;
        }

        if (val == &o_COMMA || val == &o_COLON2) continue;
        op2 = val;
        op = op2->u.oper.op;
        if (vsp == 0) goto syntaxe;
        v1 = &values[vsp-1];
        switch (op) {
        case O_FUNC:
        case O_INDEX:
            {
                unsigned int args = 0;
                value_t tmp = val_alloc(FUNCARGS_OBJ);
                op = (op == O_FUNC) ? O_PARENT : O_BRACKET;
                while (v1->val->obj != OPER_OBJ || v1->val->u.oper.op != op) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp-1-args];
                }
                tmp->u.funcargs.val = &values[vsp-args];
                tmp->u.funcargs.len = args; /* assumes no referencing */
                v1--;

                oper.op = op2;
                oper.v1 = v1->val;
                oper.v2 = tmp;
                oper.epoint = &v1->epoint;
                oper.epoint2 = args ? &tmp->u.funcargs.val->epoint : &o_out->epoint;
                oper.epoint3 = &o_out->epoint;
                val = oper.v1->obj->calc2(&oper);
                val_destroy(tmp);
                val_destroy(v1->val); v1->val = val;

                vsp -= args + 1;
                continue;
            }
        case O_RBRACKET:
        case O_RPARENT:
        case O_TUPLE:
        case O_LIST:
            {
                unsigned int tup = (op == O_RPARENT), expc = (op == O_TUPLE || op == O_LIST);
                size_t args = 0;
                op = (op == O_RBRACKET || op == O_LIST) ? O_BRACKET : O_PARENT;
                while (v1->val->obj != OPER_OBJ || v1->val->u.oper.op != op) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp-1-args];
                }
                if (args == 1) {
                    if (stop && !expc) {
                        size_t j = i + 1;
                        vsp--;
                        if (tup && j < ev->outp && (ev->o_out[j].val->obj != OPER_OBJ || (
                                        ev->o_out[j].val != &o_RPARENT &&   /* ((3)) */
                                        ev->o_out[j].val != &o_RBRACKET &&  /* [(3)] */
                                        ev->o_out[j].val != &o_FUNC &&      /* f((3)) */
                                        ev->o_out[j].val != &o_LIST &&      /* [(3),] */
                                        ev->o_out[j].val != &o_COMMA &&     /* [(3),(3)] */
                                        ev->o_out[j].val != &o_COMMAY &&    /* (3),y */
                                        ev->o_out[j].val != &o_COMMAZ       /* (3),z */
                                        ))) {
                            v1->val = values[vsp].val; 
                            values[vsp].val = NULL;
                            continue;
                        }
                        am = (op == O_BRACKET) ? A_LI : A_I;
                        v1->val = apply_addressing(values[vsp].val, am);
                        continue;
                    } else if (tup) {
                        vsp--;
                        v1->val = values[vsp].val; 
                        values[vsp].val = NULL;
                        continue;
                    }
                }
                if (args) {
                    val = val_alloc((op == O_BRACKET) ? LIST_OBJ : TUPLE_OBJ);
                    val->u.list.len = args;
                    val->u.list.data = list_create_elements(val, args);
                    while (args--) {
                        v2 = &values[vsp-1];
                        if (v2->val->obj == ERROR_OBJ) { err_msg_output_and_destroy(v2->val); v2->val = val_reference(none_value); }
                        val->u.list.data[args] = v2->val;
                        v2->val = NULL;
                        vsp--;
                    }
                } else val = val_reference((op == O_BRACKET) ? null_list : null_tuple);
                v1->val = val;
                continue;
            }
        case O_RBRACE:
        case O_DICT:
            {
                unsigned int args = 0;
                while (v1->val->obj != OPER_OBJ || v1->val->u.oper.op != O_BRACE) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp-1-args];
                }
                v1->val = val = val_alloc(DICT_OBJ);
                val->u.dict.len = 0;
                val->u.dict.def = NULL;
                avltree_init(&val->u.dict.members);
                if (args) {
                    unsigned int j;
                    vsp -= args;
                    for (j = 0; j < args; j++) {
                        v2 = &values[vsp+j];
                        if (v2->val->obj == NONE_OBJ || v2->val->obj == ERROR_OBJ) {
                            obj_destroy(val);
                            v1->val = val_reference(v2->val);
                            break;
                        }
                        if (v2->val->obj == COLONLIST_OBJ) {
                            struct pair_s *p, *p2;
                            struct avltree_node *b;
                            if (v2->val->u.list.data[0]->obj == DEFAULT_OBJ) {
                                if (val->u.dict.def) val_destroy(val->u.dict.def);
                                val->u.dict.def = val_reference(v2->val->u.list.data[1]);
                            } else {
                                p = (struct pair_s *)malloc(sizeof(struct pair_s));
                                if (!p) err_msg_out_of_memory();
                                err = obj_hash(v2->val->u.list.data[0], &p->hash, &v2->epoint);
                                if (err) {
                                    free(p);
                                    obj_destroy(val);
                                    v1->val = err;
                                    break;
                                }
                                p->key = v2->val->u.list.data[0];
                                b = avltree_insert(&p->node, &val->u.dict.members, pair_compare);
                                if (b) {
                                    p2 = avltree_container_of(b, struct pair_s, node);
                                    val_replace(&p2->data, v2->val->u.list.data[1]);
                                    free(p);
                                } else {
                                    p->key = val_reference(p->key);
                                    p->data = val_reference(v2->val->u.list.data[1]);
                                    val->u.dict.len++;
                                }
                            }
                            continue;
                        }
                        val_destroy(val);
                        val = new_error_obj(ERROR__NOT_KEYVALUE, &v1->epoint);
                        val->u.error.u.objname = v1->val->obj->name;
                        break;
                    }
                }
                continue;
            }
        case O_COND:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1]; vsp--;
            if (vsp == 0) goto syntaxe;
            err = values[vsp-1].val->obj->truth(values[vsp-1].val, TRUTH_BOOL, &values[vsp-1].epoint);
            if (err->obj != BOOL_OBJ) {
                val_destroy(values[vsp-1].val);
                values[vsp-1].val = err;
                continue;
            }
            if (err->u.boolean) {
                value_t tmp = values[vsp-1].val;
                values[vsp-1].val = v1->val;
                v1->val = tmp;
                values[vsp-1].epoint = v1->epoint;
            } else {
                value_t tmp = values[vsp-1].val;
                values[vsp-1].val = v2->val;
                v2->val = tmp;
                values[vsp-1].epoint = v2->epoint;
            }
            val_destroy(err);
            continue;
        case O_QUEST:
            vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1];
            err_msg2(ERROR______EXPECTED,"':'", &o_out->epoint);
            val_replace(&v1->val, none_value);
            continue;
        case O_COLON:
            v2 = v1; v1 = &values[--vsp-1];
            if (vsp == 0) goto syntaxe;
            switch (v1->val->obj->type) {
            case T_COLONLIST:
                if (v1->val->refcount == 1) {
                    val = val_alloc(COLONLIST_OBJ);
                    if (v2->val->obj == COLONLIST_OBJ && v2->val->refcount == 1) {
                        val->u.list.len = v1->val->u.list.len + v2->val->u.list.len;
                        if (val->u.list.len < v2->val->u.list.len) err_msg_out_of_memory(); /* overflow */
                        val->u.list.data = list_create_elements(val, val->u.list.len);
                        memcpy(val->u.list.data, v1->val->u.list.data, v1->val->u.list.len * sizeof(val->u.list.data[0]));
                        memcpy(val->u.list.data + v1->val->u.list.len, v2->val->u.list.data, v2->val->u.list.len * sizeof(val->u.list.data[0]));
                        v1->val->u.list.len = 0;
                        v2->val->u.list.len = 0;
                        val_destroy(v1->val); v1->val = val;
                        continue;
                    }
                    val->u.list.len = v1->val->u.list.len + 1;
                    if (val->u.list.len < 1) err_msg_out_of_memory(); /* overflow */
                    val->u.list.data = list_create_elements(val, val->u.list.len);
                    memcpy(val->u.list.data, v1->val->u.list.data, v1->val->u.list.len * sizeof(val->u.list.data[0]));
                    val->u.list.data[v1->val->u.list.len] = v2->val;
                    v1->val->u.list.len = 0;
                    v2->val = v1->val;
                    v1->val = val;
                    continue;
                }
                /* fall through */
            default:
                switch (v2->val->obj->type) {
                case T_COLONLIST:
                    if (v2->val->refcount == 1) {
                        val = val_alloc(COLONLIST_OBJ);
                        val->u.list.len = v2->val->u.list.len + 1;
                        if (val->u.list.len < 1) err_msg_out_of_memory(); /* overflow */
                        val->u.list.data = list_create_elements(val, val->u.list.len);
                        val->u.list.data[0] = v1->val;
                        memcpy(&val->u.list.data[1], v2->val->u.list.data, v2->val->u.list.len * sizeof(val->u.list.data[0]));
                        v1->val = val;
                        v2->val->u.list.len = 0;
                        continue;
                    }
                    /* fall through */
                default: 
                    val = val_alloc(COLONLIST_OBJ);
                    val->u.list.len = 2;
                    val->u.list.data = list_create_elements(val, 2);
                    val->u.list.data[0] = v1->val;
                    val->u.list.data[1] = v2->val;
                    v1->val = val;
                    v2->val = NULL;
                    continue;
                case T_ERROR:
                    val_replace(&v1->val, v2->val);
                    continue;
                }
            case T_ERROR:continue;
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
            oper.op = op2;
            oper.v1 = v1->val;
            oper.v2 = NULL;
            oper.epoint = &v1->epoint;
            oper.epoint3 = &o_out->epoint;
            val = oper.v1->obj->calc1(&oper);
            val_destroy(v1->val); v1->val = val;
            v1->epoint = o_out->epoint;
            continue;
        case O_COMMAS: am = A_SR; goto addr;  /* ,s */
        case O_COMMAR: am = A_RR; goto addr;  /* ,r */
        case O_COMMAZ: am = A_ZR; goto addr;  /* ,z */
        case O_COMMAY: am = A_YR; goto addr;  /* ,y */
        case O_COMMAX: am = A_XR; goto addr;  /* ,x */
        case O_COMMAD: am = A_DR; goto addr;  /* ,d */
        case O_COMMAB: am = A_BR; goto addr;  /* ,b */
        case O_COMMAK: am = A_KR; goto addr;  /* ,k */
        case O_HASH: am = A_IMMEDIATE;        /* #  */
        addr:
            val = apply_addressing(v1->val, am);
            val_destroy(v1->val); v1->val = val;
            if (op == O_HASH) v1->epoint = o_out->epoint;
            continue;
        case O_SPLAT:   /* *  */
            if (v1->val->obj == TUPLE_OBJ || v1->val->obj == LIST_OBJ) {
                value_t tmp = v1->val;
                size_t k, len = tmp->u.list.len;
                v1->val = NULL;
                vsp--;
                if (vsp + len >= ev->values_size) {
                    size_t j = ev->values_size;
                    ev->values_size = vsp + len;
                    ev->values = values = (struct values_s *)realloc(values, ev->values_size * sizeof(struct values_s));
                    if (!values || ev->values_size < len || ev->values_size > SIZE_MAX / sizeof(struct values_s)) err_msg_out_of_memory(); /* overflow */
                    for (; j < ev->values_size; j++) ev->values[j].val = NULL;
                }
                for (k = 0; k < len; k++) {
                    if (values[vsp].val) val_destroy(values[vsp].val);
                    values[vsp].val = (tmp->refcount == 1) ? tmp->u.list.data[k] : val_reference(tmp->u.list.data[k]);
                    values[vsp++].epoint = o_out->epoint;
                }
                if (tmp->refcount == 1) tmp->u.list.len = 0;
                val_destroy(tmp);
                continue;
            }
            if (v1->val->obj == DICT_OBJ) {
                value_t tmp = v1->val;
                const struct avltree_node *n = avltree_first(&tmp->u.dict.members);
                v1->val = NULL;
                vsp--;
                if (vsp + tmp->u.dict.len >= ev->values_size) {
                    size_t j = ev->values_size;
                    ev->values_size = vsp + tmp->u.dict.len;
                    ev->values = values = (struct values_s *)realloc(values, ev->values_size * sizeof(struct values_s));
                    if (!values || ev->values_size < vsp || ev->values_size > SIZE_MAX / sizeof(struct values_s)) err_msg_out_of_memory(); /* overflow */
                    for (; j < ev->values_size; j++) ev->values[j].val = NULL;
                }
                while (n) {
                    const struct pair_s *p = cavltree_container_of(n, struct pair_s, node);
                    val = val_alloc(COLONLIST_OBJ);
                    val->u.list.len = 2;
                    val->u.list.data = list_create_elements(val, 2);
                    val->u.list.data[0] = val_reference(p->key);
                    val->u.list.data[1] = val_reference(p->data);

                    if (values[vsp].val) val_destroy(values[vsp].val);
                    values[vsp].val = val;
                    values[vsp++].epoint = o_out->epoint;
                    n = avltree_next(n);
                }
                if (tmp->u.dict.def) {
                    if (vsp >= ev->values_size) {
                        size_t j = ev->values_size;
                        ev->values_size += 16;
                        ev->values = values = (struct values_s *)realloc(values, ev->values_size * sizeof(struct values_s));
                        if (!values || ev->values_size < 16 || ev->values_size > SIZE_MAX / sizeof(struct values_s)) err_msg_out_of_memory(); /* overflow */
                        for (; j < ev->values_size; j++) ev->values[j].val = NULL;
                    }
                    val = val_alloc(COLONLIST_OBJ);
                    val->u.list.len = 2;
                    val->u.list.data = list_create_elements(val, 2);
                    val->u.list.data[0] = val_reference(default_value);
                    val->u.list.data[1] = val_reference(tmp->u.dict.def);

                    if (values[vsp].val) val_destroy(values[vsp].val);
                    values[vsp].val = val;
                    values[vsp++].epoint = o_out->epoint;
                }
                val_destroy(tmp);
                continue;
            }
            v1->epoint = o_out->epoint;
            continue;
        case O_LNOT: /* ! */
            err = v1->val->obj->truth(v1->val, TRUTH_BOOL, &v1->epoint);
            if (err->obj != BOOL_OBJ) {
                val_destroy(v1->val); v1->val = err;
                continue;
            }
            val_replace(&v1->val, err->u.boolean ? false_value : true_value);
            val_destroy(err);
            continue;
        case O_LAND: /* && */
        case O_LOR:  /* || */
        case O_LXOR: /* ^^ */
            v2 = v1; v1 = &values[--vsp-1];
            if (vsp == 0) goto syntaxe;
            err = v1->val->obj->truth(v1->val, TRUTH_BOOL, &v1->epoint);
            if (err->obj != BOOL_OBJ) {
                val_destroy(v1->val); v1->val = err;
                continue;
            }
            if (op != O_LXOR) { 
                if (err->u.boolean != (op == O_LOR)) {
                    value_t tmp = v1->val;
                    v1->val = v2->val;
                    v2->val = tmp;
                    v1->epoint = v2->epoint;
                }
            } else {
                value_t err2 = v2->val->obj->truth(v2->val, TRUTH_BOOL, &v2->epoint);
                if (err2) {
                    val_destroy(v1->val);
                    v1->val = err2;
                    val_destroy(err);
                    continue;
                }
                if (err->u.boolean) {
                    if (err2->u.boolean) val_replace(&v1->val, false_value);
                } else {
                    val_replace(&v1->val, err2->u.boolean ? v2->val : false_value);
                }
                val_destroy(err2);
            }
            val_destroy(err);
            continue;
        default: break;
        }
        v2 = v1; v1 = &values[--vsp-1];
        if (vsp == 0) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX, NULL);
            ev->outp2 = ev->outp;
            ev->values_len = 0;
            return 0;
        }

        oper.op = op2;
        oper.v1 = v1->val;
        oper.v2 = v2->val;
        oper.epoint = &v1->epoint;
        oper.epoint2 = &v2->epoint;
        oper.epoint3 = &o_out->epoint;
        val = oper.v1->obj->calc2(&oper);
        val_destroy(v1->val); v1->val = val;
    }
    ev->outp2 = i;
    ev->values_len = vsp;
    return 1;
}

struct values_s *get_val(void) {
    if (eval->values_p >= eval->values_len) return NULL;
    return &eval->values[eval->values_p++];
}

value_t pull_val(struct linepos_s *epoint) {
    value_t val;
    struct values_s *value;

    if (eval->values_p >= eval->values_len) return NULL;

    value = &eval->values[eval->values_p];
    if (epoint) *epoint = value->epoint;
    val = value->val;
    eval->values[eval->values_p++].val = NULL;
    return val;
}

size_t get_val_remaining(void) {
    return eval->values_len - eval->values_p;
}


/* 0 - normal */
/* 1 - 1 only, till comma */
/* 2 - 1 only, till space  */
/* 3 - opcode */
/* 4 - opcode, with defaults */

static int get_exp2(int *wd, int stop, struct file_s *cfile) {
    char ch;

    value_t op;
    struct values_s o_oper[256];
    uint8_t operp = 0, prec, db;
    struct linepos_s epoint;
    value_t val;
    size_t llen;
    size_t openclose, identlist;

    eval->gstop = stop;
    eval->outp2 = 0;
    eval->values_p = eval->values_len = 0;

    if (arguments.tasmcomp) {
        if (get_exp_compat(wd, stop)) return get_val2_compat(eval);
        return 0;
    }
    eval->outp = 0;
    o_oper[0].val = &o_COMMA;

    *wd=3;    /* 0=byte 1=word 2=long 3=negative/too big */
    openclose=identlist=0;

    ignore();
    switch (here()) {
    case 0:
    case ';': return 1;
    case '@':
        ch = pline[++lpoint.pos];
        if (!arguments.casesensitive) ch |= 0x20;
        switch (ch) {
        case 'b':*wd=0;break;
        case 'w':*wd=1;break;
        case 'l':*wd=2;break;
        default:err_msg2(ERROR______EXPECTED, "@B or @W or @L", &lpoint); return 0;
        }
        lpoint.pos++;
        break;
    }
    for (;;) {
        ignore(); ch = here(); epoint = lpoint;
        switch (ch) {
        case ',':
            if (stop != 4 || operp) goto tryanon;
            lpoint.pos++;push_oper(val_reference(default_value), &epoint);
            continue;
        case ')':
            if (operp) {
                const value_t o = o_oper[operp-1].val;
                if (o == &o_COMMA) {operp--;op = &o_TUPLE;goto tphack;}
                else if (o == &o_PARENT || o == &o_FUNC) goto other;
            }
            goto tryanon;
        case ']':
            if (operp) { 
                const value_t o = o_oper[operp-1].val;
                if (o == &o_COMMA) {operp--;op = &o_LIST;goto lshack;}
                else if (o == &o_BRACKET || o == &o_INDEX) goto other;
            }
            goto tryanon;
        case '}':
            if (operp) { 
                const value_t o = o_oper[operp-1].val;
                if (o == &o_COMMA) {operp--;op = &o_DICT;goto brhack;}
                else if (o == &o_BRACE) goto other;
            }
            goto tryanon;
        case ':':
            if (operp) { 
                const value_t o = o_oper[operp-1].val;
                if (o != &o_PARENT && o != &o_BRACKET && o != &o_BRACE && o != &o_FUNC && o != &o_INDEX && o != &o_COMMA) goto tryanon;
            }
            push_oper(val_reference(default_value), &epoint);
            goto other;
        case '(': 
            if ((operp && o_oper[operp-1].val == &o_MEMBER) || identlist) identlist++;
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = &o_PARENT; lpoint.pos++;
            push_oper(&o_PARENT, &epoint);
            openclose++;
            continue;
        case '[':
            if ((operp && o_oper[operp-1].val == &o_MEMBER) || identlist) identlist++;
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = &o_BRACKET; lpoint.pos++;
            push_oper(&o_BRACKET, &epoint);
            openclose++;
            continue;
        case '{':
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = &o_BRACE; lpoint.pos++;
            push_oper(&o_BRACE, &epoint);
            openclose++;
            continue;
        case '+': op = &o_POS; break;
        case '-': op = &o_NEG; break;
        case '*': op = &o_SPLAT; break;
        case '!': op = &o_LNOT;break;
        case '~': op = &o_INV; break;
        case '<': if (pline[lpoint.pos+1] == '>') {lpoint.pos++;op = &o_WORD;} else op = &o_LOWER; break;
        case '>': if (pline[lpoint.pos+1] == '`') {lpoint.pos++;op = &o_HWORD;} else if (pline[lpoint.pos+1] == '<') {lpoint.pos++;op = &o_BSWORD;} else op = &o_HIGHER; break;
        case '#': op = &o_HASH; break;
        case '`': op = &o_BANK; break;
        case '^': op = &o_STRING; break;
        case '$': lpoint.pos++;push_oper(get_hex(&epoint), &epoint);goto other;
        case '%': if ((pline[lpoint.pos+1] & 0xfe) == 0x30 || (pline[lpoint.pos+1] == '.' && (pline[lpoint.pos+2] & 0xfe) == 0x30)) { lpoint.pos++;push_oper(get_bin(&epoint), &epoint);goto other; }
                  goto tryanon;
        case '"':
        case '\'': push_oper(get_string(), &epoint);goto other;
        case '?': 
            if (operp) { 
                if (o_oper[operp - 1].val == &o_SPLAT || o_oper[operp - 1].val == &o_POS || o_oper[operp - 1].val == &o_NEG) goto tryanon;
            }
            lpoint.pos++;push_oper(val_reference(gap_value), &epoint);goto other;
        case '.': if ((pline[lpoint.pos+1] ^ 0x30) >= 10) goto tryanon; /* fall through */;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            push_oper(get_float(&epoint), &epoint);
            goto other;
        case 0:
        case ';': 
            if (openclose) {
                listing_line(0);
                if (!mtranslate(cfile)) { /* expand macro parameters, if any */
                    continue;
                }
            }
            goto tryanon;
        default: 
            if (get_label()) {
                int down;
                struct label_s *l;
                str_t ident;
            as_ident:
                if ((pline[epoint.pos] | (arguments.casesensitive ? 0 : 0x20)) == 'b' && (pline[epoint.pos + 1] == '"' || pline[epoint.pos + 1] == '\'')) {
                    val = get_string();
                    push_oper(bytes_from_str(val, &epoint), &epoint);
                    val_destroy(val);
                    goto other;
                }
                if ((operp && o_oper[operp-1].val == &o_MEMBER) || identlist) {
                    val = val_alloc(IDENT_OBJ);
                    val->u.ident.name.data = pline + epoint.pos;
                    val->u.ident.name.len = lpoint.pos - epoint.pos;
                    val->u.ident.epoint = epoint;
                    push_oper(val, &epoint);
                    goto other;
                } 
                ident.data = pline + epoint.pos;
                ident.len = lpoint.pos - epoint.pos;
                down = (ident.data[0] != '_');
                l = down ? find_label(&ident) : find_label2(&ident, cheap_context);
                if (l) {
                    touch_label(l);
                    l->shadowcheck = 1;
                    push_oper(val_reference(l->value), &epoint);
                    goto other;
                }
                val = new_error_obj(ERROR___NOT_DEFINED, &epoint);
                val->u.error.u.notdef.ident = ident;
                val->u.error.u.notdef.label = down ? current_context : cheap_context;
                val->u.error.u.notdef.down = down;
                push_oper(val, &epoint);
                goto other;
            }
        tryanon:
            db = operp;
            while (operp && o_oper[operp-1].val == &o_POS) operp--;
            if (db != operp) {
                struct label_s *l;
                if ((operp && o_oper[operp-1].val == &o_MEMBER) || identlist) {
                    val = val_alloc(ANONIDENT_OBJ);
                    val->u.anonident.count = db - operp - 1;
                    val->u.anonident.epoint = o_oper[operp].epoint;
                    push_oper(val, &o_oper[operp].epoint);
                    goto other;
                }
                l = find_anonlabel(db - operp -1);
                if (l) {
                    touch_label(l);
                    push_oper(val_reference(l->value), &o_oper[operp].epoint);
                    goto other;
                }
                val = new_error_obj(ERROR___NOT_DEFINED, &o_oper[operp].epoint);
                val->u.error.u.notdef.ident.len = (size_t)((ssize_t)(db - operp));
                val->u.error.u.notdef.ident.data = NULL;
                val->u.error.u.notdef.label = current_context;
                val->u.error.u.notdef.down = 1;
                push_oper(val, &o_oper[operp].epoint);
                goto other;
            }
            while (operp && o_oper[operp-1].val == &o_NEG) operp--;
            if (db != operp) {
                struct label_s *l;
                if ((operp && o_oper[operp-1].val == &o_MEMBER) || identlist) {
                    val = val_alloc(ANONIDENT_OBJ);
                    val->u.anonident.count = operp - db;
                    val->u.anonident.epoint = o_oper[operp].epoint;
                    push_oper(val, &o_oper[operp].epoint);
                    goto other;
                }
                l = find_anonlabel(operp - db);
                if (l) {
                    touch_label(l);
                    push_oper(val_reference(l->value), &o_oper[operp].epoint);
                    goto other;
                }
                val = new_error_obj(ERROR___NOT_DEFINED, &o_oper[operp].epoint);
                val->u.error.u.notdef.ident.len = (size_t)((ssize_t)(operp - db));
                val->u.error.u.notdef.ident.data = NULL;
                val->u.error.u.notdef.label = current_context;
                val->u.error.u.notdef.down = 1;
                push_oper(val, &o_oper[operp].epoint);
                goto other;
            }
            if (operp) {
                if (o_oper[operp-1].val == &o_COLON) {
                    push_oper(val_reference(default_value), &epoint);
                    goto other;
                }
                if (o_oper[operp-1].val == &o_SPLAT) {
                    operp--;
                    push_oper(get_star(&o_oper[operp].epoint), &o_oper[operp].epoint);
                    goto other;
                }
                epoint = o_oper[operp-1].epoint;
            }
            goto syntaxe;
        }
        if (operp && o_oper[operp - 1].val == &o_SPLAT) {
            operp--;
            lpoint.pos = epoint.pos;
            push_oper(get_star(&o_oper[operp].epoint), &o_oper[operp].epoint);
            goto other;
        }
        lpoint.pos++;
    rtl:
        o_oper[operp].epoint = epoint;
        o_oper[operp++].val = op;
        continue;
    other:
        if (stop != 2 || openclose) ignore();
        ch = here();epoint = lpoint;
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = get_label();
            if (llen == 1) {
                uint8_t c = pline[epoint.pos + 1];
                if (!arguments.casesensitive) c |= 0x20;
                switch (c) {
                case 'x': op = &o_COMMAX;llen=0;break;
                case 'y': op = &o_COMMAY;llen=0;break;
                case 'z': op = &o_COMMAZ;llen=0;break;
                case 'r': op = &o_COMMAR;llen=0;break;
                case 's': op = &o_COMMAS;llen=0;break;
                case 'd': op = &o_COMMAD;llen=0;break;
                case 'b': op = &o_COMMAB;llen=0;break;
                case 'k': op = &o_COMMAK;llen=0;break;
                default: op = &o_COMMA;break;
                }
            } else op = &o_COMMA;
            prec = op->u.oper.prio;
            while (operp && prec <= o_oper[operp-1].val->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            if (op != &o_COMMA) {
                o_oper[operp].epoint = epoint;
                o_oper[operp++].val = op;
                goto other;
            }
            if (!operp) {
                if (stop == 1) {lpoint = epoint;break;}
            }
            push_oper(&o_COMMA, &epoint);
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = op;
            if (llen) {
                epoint.pos++;
                goto as_ident;
            }
            continue;
        case '(':
            prec = o_MEMBER.u.oper.prio;
            while (operp && prec <= o_oper[operp-1].val->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            push_oper(&o_PARENT, &epoint);
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = &o_FUNC; lpoint.pos++;
            if (identlist) identlist++;
            openclose++;
            continue;
        case '[':
            prec = o_MEMBER.u.oper.prio;
            while (operp && prec <= o_oper[operp-1].val->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            push_oper(&o_BRACKET, &epoint);
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = &o_INDEX; lpoint.pos++;
            if (identlist) identlist++;
            openclose++;
            continue;
        case '&': if (pline[lpoint.pos+1] == '&') {lpoint.pos+=2;op = &o_LAND;} else {lpoint.pos++;op = &o_AND;} goto push2;
        case '|': if (pline[lpoint.pos+1] == '|') {lpoint.pos+=2;op = &o_LOR;} else {lpoint.pos++;op = &o_OR;} goto push2;
        case '^': if (pline[lpoint.pos+1] == '^') {lpoint.pos+=2;op = &o_LXOR;} else {lpoint.pos++;op = &o_XOR;} goto push2;
        case '*': if (pline[lpoint.pos+1] == '*') {lpoint.pos+=2;op = &o_EXP; goto rtl;} lpoint.pos++;op = &o_MUL; goto push2;
        case '%': lpoint.pos++;op = &o_MOD; goto push2;
        case '/': if (pline[lpoint.pos+1] == '/') {lpoint.pos+=2;op = &o_MOD;} else {lpoint.pos++;op = &o_DIV;} goto push2;
        case '+': lpoint.pos++;op = &o_ADD; goto push2;
        case '-': lpoint.pos++;op = &o_SUB; goto push2;
        case '.': if (pline[lpoint.pos+1] == '.') {lpoint.pos+=2;op = &o_CONCAT;} else {lpoint.pos++;op = &o_MEMBER;} goto push2;
        case '?': lpoint.pos++;op = &o_QUEST; prec = o_COND.u.oper.prio + 1; goto push3;
        case ':': op = &o_COLON;
            prec = op->u.oper.prio + 1;
            while (operp && prec <= o_oper[operp-1].val->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            if (operp && o_oper[operp-1].val == &o_QUEST) { o_oper[operp-1].val = &o_COND; op = &o_COLON2;}
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = op;
            lpoint.pos++;
            continue;
        case '=': op = &o_EQ; if (pline[lpoint.pos+1] == '=') lpoint.pos+=2; else lpoint.pos++;
        push2:
            prec = op->u.oper.prio;
        push3:
            while (operp && prec <= o_oper[operp-1].val->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            o_oper[operp].epoint = epoint;
            o_oper[operp++].val = op;
            continue;
        case '<': 
            switch (pline[lpoint.pos + 1]) {
            case '>': lpoint.pos+=2;op = &o_NE; break;
            case '<': lpoint.pos+=2;op = &o_LSHIFT; break;
            case '=': if (pline[lpoint.pos + 2] == '>') {lpoint.pos += 3; op = &o_CMP;} else {lpoint.pos += 2; op = &o_LE;} break;
            default: lpoint.pos++;op = &o_LT; break;
            }
            goto push2;
        case '>':
            switch (pline[lpoint.pos+1]) {
            case '<': lpoint.pos+=2;op = &o_NE; break;
            case '>': lpoint.pos+=2;op = &o_RSHIFT; break;
            case '=': lpoint.pos+=2;op = &o_GE; break;
            default: lpoint.pos++;op = &o_GT; break;
            }
            goto push2;
        case '!':
            if (pline[lpoint.pos+1]=='=') {lpoint.pos+=2;op = &o_NE;goto push2;}
            goto syntaxe;
        case ')':
            op = &o_RPARENT;
        tphack:
            openclose--;
            if (identlist) identlist--;
            while (operp) {
                const value_t o = o_oper[operp-1].val;
                if (o == &o_PARENT || o == &o_FUNC) break;
                if (o == &o_BRACKET || o == &o_INDEX || o == &o_BRACE) {operp = 0; break;}
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            if (!operp) {err_msg2(ERROR______EXPECTED, "(", &lpoint); goto error;}
            lpoint.pos++;
            operp--;
            push_oper((o_oper[operp].val == &o_PARENT)? op : o_oper[operp].val, &o_oper[operp].epoint);
            goto other;
        case ']':
            op = &o_RBRACKET;
        lshack:
            openclose--;
            if (identlist) identlist--;
            while (operp) {
                const value_t o = o_oper[operp-1].val;
                if (o == &o_BRACKET || o == &o_INDEX) break;
                if (o == &o_PARENT || o == &o_FUNC || o == &o_BRACE) {operp = 0; break;}
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            if (!operp) {err_msg2(ERROR______EXPECTED, "[", &lpoint); goto error;}
            lpoint.pos++;
            operp--;
            push_oper((o_oper[operp].val == &o_BRACKET) ? op : o_oper[operp].val, &o_oper[operp].epoint);
            goto other;
        case '}':
            op = &o_RBRACE;
        brhack:
            openclose--;
            while (operp) {
                const value_t o = o_oper[operp-1].val;
                if (o == &o_BRACE) break;
                if (o == &o_BRACKET || o == &o_INDEX || o == &o_PARENT || o == &o_FUNC) {operp = 0; break;}
                operp--;
                push_oper(o_oper[operp].val, &o_oper[operp].epoint);
            }
            if (!operp) {err_msg2(ERROR______EXPECTED, "{", &lpoint); goto error;}
            lpoint.pos++;
            operp--;
            push_oper((o_oper[operp].val == &o_BRACE) ? op : o_oper[operp].val, &o_oper[operp].epoint);
            goto other;
        case 0:
        case ';': 
            if (openclose) {
                listing_line(0);
                if (!mtranslate(cfile)) { /* expand macro parameters, if any */
                    goto other;
                }
            }
            break;
        case '\t':
        case ' ': break;
        default: 
            switch (get_label()) {
            case 1: if (pline[epoint.pos] == 'x') {op = &o_X;goto push2;} break;
            case 2: if (pline[epoint.pos] == 'i' && pline[epoint.pos + 1] == 'n') {op = &o_IN;goto push2;} break;
            }
            goto syntaxe;
        }
        while (operp) {
            const value_t o = o_oper[operp-1].val;
            if (o == &o_PARENT || o == &o_FUNC) {err_msg2(ERROR______EXPECTED, ")", &o_oper[operp - 1].epoint); goto error;}
            if (o == &o_BRACKET || o == &o_INDEX) {err_msg2(ERROR______EXPECTED,"]", &o_oper[operp - 1].epoint); goto error;}
            if (o == &o_BRACE) {err_msg2(ERROR______EXPECTED, "}", &o_oper[operp - 1].epoint); goto error;}
            operp--;
            push_oper(o_oper[operp].val, &o_oper[operp].epoint);
        }
        if (!operp) return get_val2(eval);
    syntaxe:
        err_msg2(ERROR_EXPRES_SYNTAX, NULL, &epoint);
    error:
        break;
    }
    return 0;
}

int get_exp(int *wd, int stop, struct file_s *cfile, unsigned int min, unsigned int max, linepos_t epoint) {/* length in bytes, defined */
    if (!get_exp2(wd, stop, cfile)) {
        return 0;
    }
    if (eval->values_len < min || (max && eval->values_len > max)) {
        err_msg_argnum(eval->values_len, min, max, epoint);
        return 0;
    }
    return 1;
}


int get_exp_var(struct file_s *cfile, linepos_t epoint) {
    int w;
    return get_exp(&w, 2, cfile, 1, 1, epoint);
}

value_t get_vals_tuple(void) {
    size_t i, len = get_val_remaining();
    value_t val;

    if (len == 1) {
        val = pull_val(NULL);
        if (val->obj == ERROR_OBJ) { err_msg_output_and_destroy(val); val = val_reference(none_value); }
        return val;
    }
    val = val_alloc(TUPLE_OBJ);
    val->u.list.len = len;
    val->u.list.data = list_create_elements(val, len);
    for (i = 0; i < len; i++) {
        value_t val2 = pull_val(NULL);
        if (val2->obj == ERROR_OBJ) { err_msg_output_and_destroy(val2); val2 = val_reference(none_value); }
        val->u.list.data[i] = val2;
    }
    return val;
}

value_t get_vals_addrlist(struct linepos_s *epoints) {
    size_t i, j, len = get_val_remaining();
    value_t val;
    struct linepos_s epoint;

    if (len == 1) {
        val = pull_val(&epoints[0]);
        if (val->obj == ERROR_OBJ) { err_msg_output_and_destroy(val); val = val_reference(none_value); }
        return val;
    }
    val = val_alloc(ADDRLIST_OBJ);
    val->u.list.data = list_create_elements(val, len);
    for (i = j = 0; j < len; j++) {
        value_t val2 = pull_val((i < 3) ? &epoints[i] : &epoint);
        if (val2->obj == ERROR_OBJ) { err_msg_output_and_destroy(val2); val2 = val_reference(none_value); }
        else if (val2->obj == REGISTER_OBJ && val2->u.reg.len == 1 && i) {
            enum atype_e am;
            switch (val2->u.reg.data[0]) {
            case 's': am = A_SR; break;
            case 'r': am = A_RR; break;
            case 'z': am = A_ZR; break;
            case 'y': am = A_YR; break;
            case 'x': am = A_XR; break;
            case 'd': am = A_DR; break;
            case 'b': am = A_BR; break;
            case 'k': am = A_KR; break;
            default: am = A_NONE; break;
            }
            if (am != A_NONE) {
                val_destroy(val2);
                val2 = apply_addressing(val->u.list.data[i - 1], am);
                val_destroy(val->u.list.data[i - 1]);
                val->u.list.data[i - 1] = val2;
                continue;
            }
        }
        val->u.list.data[i++] = val2;
    }
    if (i == 1) {
        value_t val2 = val->u.list.data[0];
        val->u.list.len = 0;
        val_destroy(val);
        return val2;
    }
    val->u.list.len = i;
    return val;
}

void eval_enter(void) {
    evx_p++;
    if (evx_p >= evxnum) {
        evxnum++;
        evx = (struct eval_context_s **)realloc(evx, evxnum * sizeof(struct eval_context_s *));
        if (!evx || evxnum < 1 || evxnum > SIZE_MAX / sizeof(struct eval_context_s *)) err_msg_out_of_memory(); /* overflow */
        eval = (struct eval_context_s *)malloc(sizeof(struct eval_context_s));
        if (!eval) err_msg_out_of_memory();
        eval->values = NULL;
        eval->values_size = 0;
        eval->o_out = NULL;
        eval->out_size = 0;
        evx[evx_p] = eval;
        return;
    }
    eval = evx[evx_p];
}

void eval_leave(void) {
    if (evx_p) evx_p--;
    eval = evx[evx_p];
}

void init_eval(void) {
    evxnum = 0;
    evx_p = (size_t)-1;
    eval_enter();
}

void destroy_eval(void) {
    while (evxnum--) {
        struct values_s *v;
        eval = evx[evxnum];
        v = eval->o_out;
        while (eval->out_size--) {
            if (v->val) val_destroy(v->val);
            v++;
        }
        free(eval->o_out);
        v = eval->values;
        while (eval->values_size--) {
            if (v->val) val_destroy(v->val);
            v++;
        }
        free(eval->values);
        free(eval);
    }
    free(evx);
}
