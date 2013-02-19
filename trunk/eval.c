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

#include "eval.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "file.h"
#include "error.h"
#include "section.h"
#include "encoding.h"
#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE >= 500 || _XOPEN_SOURCE && _XOPEN_SOURCE_EXTENDED || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#define cbrt(a) pow((a), 1.0/3.0)
#endif 
#if _XOPEN_SOURCE >= 600 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
inline double round(double a) {return (a < 0.0) ? ceil(a-0.5) : floor(a+0.5);}
inline double trunc(double a) {return (a > 0.0) ? floor(a) : ceil(a);}
#endif 
#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE >= 600 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
inline double sinh(double a) {return (exp(a) - exp(-a)) / 2.0;}
inline double cosh(double a) {return (exp(a) + exp(-a)) / 2.0;}
inline double tanh(double a) {return sinh(a) / cosh(a);}
#endif
#if _BSD_SOURCE || _SVID_SOURCE || _XOPEN_SOURCE || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
inline double hypot(double a, double b) {return sqrt(a*a+b*b);}
#endif

static struct value_s new_value = {T_NONE, 0, {}};
static struct value_s none_value = {T_NONE, 0, {}};
static struct value_s true_value = {T_BOOL, 0, {{1,1}}};
static struct value_s false_value = {T_BOOL, 0, {{1,0}}};
static struct value_s null_str = {T_STR, 0, {}};
static struct value_s null_tuple = {T_TUPLE, 0, {}};
static struct value_s null_list = {T_LIST, 0, {}};
struct value_s error_value = {T_NONE, 0, {}};

struct encoding_s *actual_encoding;

void set_uint(struct value_s *v, uval_t val) {
    v->u.num.val = val;
    v->u.num.len = 1;
    while (val & ~(uval_t)0xff) {val>>=8;v->u.num.len++;}
    v->type = T_UINT;
}

void set_int(struct value_s *v, ival_t val) {
    v->u.num.val = val;
    v->u.num.len = 1;
    if (val < 0) val = ~val;
    while ((uval_t)val & ~(uval_t)0x7f) {val >>= 8;v->u.num.len++;}
    v->type = T_SINT;
}

static int get_hex(struct value_s *v) {
    uval_t val = 0;
    unsigned int start;
    ignore();
    while (here() == 0x30) lpoint++;
    start = lpoint;
    while ((here() ^ 0x30) < 10 || (uint8_t)((here() | 0x20) - 0x61) < 6 ) {
        val = (val << 4) + (here() & 15);
        if (here() & 0x40) val += 9;
        lpoint++;
    }
    v->u.num.val = val;
    v->u.num.len = (lpoint - start + 1) / 2;
    v->u.num.len |= !v->u.num.len;
    v->type = T_NUM;
    return v->u.num.len > sizeof(val);
}

static int get_bin(struct value_s *v) {
    uval_t val = 0;
    unsigned int start;
    ignore();
    while (here() == 0x30) lpoint++;
    start = lpoint;
    while ((here() & 0xfe) == '0') {
        val = (val << 1) | (here() & 1);
        lpoint++;
    }
    v->u.num.val = val;
    v->u.num.len = (lpoint - start + 7) / 8;
    v->u.num.len |= !v->u.num.len;
    v->type = T_NUM;
    return v->u.num.len > sizeof(val);
}

static int get_dec(struct value_s *v) {
    uval_t val = 0;
    int large = 0;
    while (here() == '0') lpoint++;
    while ((uint8_t)(here() ^ '0') < 10) {
        if (val >= ((uval_t)1 << (8 * sizeof(val) - 1)) / 5) {
            if (val == ((uval_t)1 << (8 * sizeof(val) - 1)) / 5) {
               if ((uval_t)(here() & 15) > (((uval_t)1 << (8 * sizeof(val) - 1)) % 5) * 2) large = 1;
            } else large = 1;
        }
        val=(val * 10) + (here() & 15);
        lpoint++;
    }
    set_uint(v, val);
    return large;
}

static int get_float(struct value_s *v) {
    unsigned int i = lpoint;

    while ((uint8_t)(pline[i] ^ '0') < 10) i++;
    if (pline[i]=='.') {
        do {
            i++;
        } while ((uint8_t)(pline[i] ^ '0') < 10);
    }
    if ((pline[i] | 0x20)=='e') {
        if ((pline[i+1]=='-' || pline[i+1]=='+') && (uint8_t)(pline[i+2] ^ '0') < 10) i++;
        if ((uint8_t)(pline[i+1] ^ '0') < 10) {
            do {
                i++;
            } while ((uint8_t)(pline[i] ^ '0') < 10);
        }
    }
    v->u.real = strtod((char *)pline + lpoint, NULL);
    v->type = T_FLOAT;
    lpoint = i;
    return 0;
}

uint_fast16_t petascii(size_t *i, const struct value_s *v) {
    uint32_t ch, rc2;
    uint8_t *text = v->u.str.data + *i;
    uint16_t rc;

    rc2 = find_escape((char *)text, (char *)v->u.str.data + v->u.str.len, actual_encoding);
    if (rc2) {
        *i = (rc2 >> 8) + text - v->u.str.data;
        return rc2 & 0xff;
    }
    ch = text[0];
    if (ch & 0x80) (*i) += utf8in(text, &ch); else (*i)++;
    rc = find_trans(ch, actual_encoding);
    if (rc < 256) return rc;
    err_msg(ERROR___UNKNOWN_CHR, (char *)ch);
    ch = 0;
    return ch;
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b < a * 0.0000000005;
    return b - a < b * 0.0000000005;
}

int str_to_num(const struct value_s *v, enum type_e type, struct value_s *v2) {
    uint16_t ch;
    unsigned int large = 0;
    size_t i = 0;
    uval_t val = 0;

    if (actual_encoding) {
        while (v->u.str.len > i) {
            if (large >= (arguments.tasmcomp ? 1 : sizeof(val))) {
                v2->type = T_NONE;return 1;
            }

            ch = petascii(&i, v);
            if (ch > 255) {
                v2->type = T_NONE;return 1;
            }

            val |= (uint8_t)ch << (8 * large);
            large++;
        }
    } else if (v->u.str.len) {
        uint32_t ch;

        ch = v->u.str.data[0];
        if (ch & 0x80) i = utf8in(v->u.str.data, &ch); else i=1;

        if (v->u.str.len > i) {
            v2->type = T_NONE;return 1;
        }
        val = ch;
    } else {
        v2->type = T_NONE;return 1;
    }
    v2->type = type;
    v2->u.num.val = val;
    v2->u.num.len = large | (!large);
    return 0;
}

static int str_to_num2(struct value_s **v2, enum type_e type) {
    int r = str_to_num(*v2, type, &new_value);
    val_replace(v2, &new_value);
    return r;
}

static void get_string(struct value_s *v, uint8_t ch) {
    unsigned int i, r = 0, i2 = 0;
    uint32_t ch2;

    i = lpoint;
    for (;;) {
        if (!(ch2 = here())) {err_msg(ERROR______EXPECTED,"End of string"); v->type = T_NONE; return;}
        if (ch2 & 0x80) lpoint += utf8in(pline + lpoint, &ch2); else lpoint++;
        if (ch2 == ch) {
            if (here() == ch && !arguments.tasmcomp) {lpoint++;r++;} // handle 'it''s'
            else break; // end of string;
        }
        i2++;
    }
    if (r) {
        const uint8_t *p = (uint8_t *)pline + i, *e, *p2;
        uint8_t *d;
        v->type = T_NONE;
        v->u.str.len = lpoint - i - 1 - r;
        v->u.str.chars = i2;
        d = v->u.str.data = malloc(v->u.str.len);
        e = pline + lpoint - 1;
        while (e > p) {
            p2 = memchr(p, ch, e - p);
            if (p2) {
                memcpy(d, p, p2 - p + 1);
                d += p2 - p + 1; p = p2 + 2;
            } else {
                memcpy(d, p, e - p);
                p = e;
            }
        }
    } else {
        v->type = T_STR;
        v->u.str.len = lpoint - i - 1;
        v->u.str.chars = i2;
        v->u.str.data = (uint8_t *)pline + i;
    }
    return;
}

static int touch_label(struct label_s *tmp) {
    tmp->ref=1;tmp->pass=pass;
    if (tmp->type != L_VAR || tmp->upass==pass) return 0;
    return 1;
}

static void copy_name(struct value_s *val, char *ident) {
    unsigned int len = val->u.ident.len;
    if (len > linelength - 1) len = linelength - 1;
    if (arguments.casesensitive) memcpy(ident, val->u.ident.name, len);
    else {
        unsigned int i;
        for (i=0;i < len;i++) ident[i]=lowcase(val->u.ident.name[i]);
    }
    ident[len] = 0;
}

static void try_resolv_ident(struct value_s **val2) {
    char ident[linelength];
    struct value_s *val = val2[0];

    switch (val->type) {
    case T_FORWR: 
        new_value.u.ident.len = 1;
        new_value.u.ident.name = (uint8_t *)"+";
        sprintf(ident,"+%x+%x", reffile, forwr + val->u.ref - 1); break;
    case T_BACKR: 
        new_value.u.ident.len = 1;
        new_value.u.ident.name = (uint8_t *)"-";
        sprintf(ident,"-%x-%x", reffile, backr - val->u.ref); break;
    case T_IDENT: 
        new_value.u.ident.len = val->u.ident.len;
        new_value.u.ident.name = val->u.ident.name;
        copy_name(val, ident); break;
    default: return;
    }
    new_value.u.ident.label = find_label(ident);
    new_value.type = (new_value.u.ident.label) ? T_IDENTREF : T_UNDEF;
    val_replace(val2, &new_value);
}

static const struct value_s *unwind_identrefs(const struct value_s *v1, unsigned int epoint) {
    const struct value_s *vold = v1;
    int rec = 100;
    while (v1->type == T_IDENTREF) {
        if (pass != 1 && v1->u.ident.label->value->type != T_IDENTREF) {
            if (v1->u.ident.label->requires & ~current_section->provides) err_msg2(ERROR_REQUIREMENTS_, v1->u.ident.label->origname, epoint);
            if (v1->u.ident.label->conflicts & current_section->provides) err_msg2(ERROR______CONFLICT, v1->u.ident.label->origname, epoint);
        }
        if (touch_label(v1->u.ident.label)) {
            new_value.type = T_UNDEF;
            new_value.u.ident.len = vold->u.ident.len;
            new_value.u.ident.name = vold->u.ident.name;
            return &new_value;
        }
        v1 = v1->u.ident.label->value;
        if (!rec--) {
            err_msg2(ERROR__REFRECURSION, NULL, epoint);
            return &none_value;
        }
    } 
    if (v1->type == T_UNDEF && pass == 1) return &none_value;
    return v1;
}

static void try_resolv_identref(struct value_s **val2) {
    struct value_s *val = val2[0];
    val = (struct value_s *)unwind_identrefs(val, 0);
    val_replace(val2, val);
}

static enum type_e try_resolv(struct value_s **val) {
    try_resolv_ident(val);
    try_resolv_identref(val);
    return val[0]->type;
}

static void get_star(struct value_s *v) {
    struct star_s *tmp;

    tmp=new_star(vline);
    if (labelexists && tmp->addr != star) {
        fixeddig=0;
    }
    tmp->addr=star;
    set_uint(v, star);
    v->type = T_NUM;
}

/*
 * get priority for operator in an expression
 */
static int priority(enum oper_e ch)
{
    switch (ch) {
    default:
    case O_FUNC:               // a(
    case O_INDEX:              // a[
    case O_SLICE:              // a[x:
    case O_SLICE2:             // a[x::
    case O_BRACKET:            // [a]
    case O_PARENT:return 0;    // (a)
    case O_COND:               // ?
    case O_COLON:return 1;     // :
    case O_COMMA:return 2;     // ,
    case O_WORD:               // <>
    case O_HWORD:              // >`
    case O_BSWORD:             // ><
    case O_LOWER:              // <
    case O_HIGHER:             // >
    case O_BANK:               // `
    case O_STRING:return 3;    // ^
    case O_LOR:return 4;       // ||
    case O_LXOR:return 5;      // ^^
    case O_LAND:return 6;      // &&
    case O_EQ:                 // ==
    case O_NEQ:                // !=
    case O_LT:                 // <
    case O_GT:                 // >
    case O_GE:                 // >=
    case O_LE:return 7;        // <=
    case O_OR:return 8;        // |
    case O_XOR:return 9;       // ^
    case O_AND:return 10;      // &
    case O_LSHIFT:             // <<
    case O_ASHIFT:             // >>
    case O_RSHIFT:return 11;   // >>>
    case O_ADD:                // +
    case O_SUB:return 12;      // -
    case O_MUL:                // *
    case O_DIV:                // /
    case O_MOD:return 13;      // %
    case O_EXP:return 14;      // **
    case O_NEG:                // -
    case O_POS:                // +
    case O_INV:                // ~
    case O_LNOT:return 15;     // !
    case O_MEMBER:return 16;   // .
    }
}

static struct values_s *values = NULL;
static size_t values_size=0;
static uint8_t outp, outp2;
static int gstop;
static struct {
    struct value_s val;
    unsigned int epoint;
} o_out[256];

static int get_exp_compat(int *wd, int stop) {// length in bytes, defined
    int cd;
    char ch;

    enum oper_e o_oper[256] = {0}, conv;
    unsigned int epoints[256];
    uint8_t operp = 0;
    int large=0;
    unsigned int epoint, cpoint = 0;

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    cd=0;     // 0=error, 1=ok, 2=(a, 3=()
rest:
    ignore();
    conv = O_POS;
    switch (here()) {
    case '!':*wd=1;lpoint++;break;
    case '<': conv = O_LOWER; cpoint = lpoint; lpoint++;break; 
    case '>': conv = O_HIGHER;cpoint = lpoint; lpoint++;break; 
    }
    for (;;) {
        ignore();ch = here(); epoint=lpoint;

        switch (ch) {
        case '(': epoints[operp] = epoint; o_oper[operp++] = O_PARENT; lpoint++;continue;
        case '$': lpoint++;if (get_hex(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '%': lpoint++;if (get_bin(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '"': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
        case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
        }
        if (ch>='0' && ch<='9') { if (get_dec(&o_out[outp].val)) goto pushlarge;
        pushval:
            if (type_is_int(o_out[outp].val.type) && (o_out[outp].val.u.num.val & ~0xffff)) {
            pushlarge:
                err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);large=1;
                o_out[outp].val.u.num.val = 0xffff;
            }
        } else {
            while ((((ch=here()) | 0x20) >= 'a' && (ch | 0x20) <= 'z') || (ch>='0' && ch<='9') || ch=='_') lpoint++;
            if (epoint == lpoint) goto syntaxe;
            o_out[outp].val.u.ident.name = pline + epoint;
            o_out[outp].val.u.ident.len = lpoint - epoint;
            o_out[outp].val.type = T_IDENT;
        }
        o_out[outp++].epoint=epoint;
    other:
        ignore();ch = here(); epoint=lpoint;

        while (operp && o_oper[operp-1] != O_PARENT) {o_out[outp].val.type = T_OPER; o_out[outp].epoint = epoints[--operp]; o_out[outp++].val.u.oper=o_oper[operp];}
        switch (ch) {
        case ',':
            if (conv != O_POS) {
                o_out[outp].epoint = cpoint;
                o_out[outp].val.type = T_OPER;
                o_out[outp++].val.u.oper = conv;
            }
            if (stop) break;
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper=O_SEPARATOR;
            o_out[outp++].epoint=epoint;
            lpoint++;
            goto rest;
        case '&': epoints[operp] = epoint; o_oper[operp++] = O_AND; lpoint++;continue;
        case '.': epoints[operp] = epoint; o_oper[operp++] = O_OR; lpoint++;continue;
        case ':': epoints[operp] = epoint; o_oper[operp++] = O_XOR; lpoint++;continue;
        case '*': epoints[operp] = epoint; o_oper[operp++] = O_MUL; lpoint++;continue;
        case '/': epoints[operp] = epoint; o_oper[operp++] = O_DIV; lpoint++;continue;
        case '+': epoints[operp] = epoint; o_oper[operp++] = O_ADD; lpoint++;continue;
        case '-': epoints[operp] = epoint; o_oper[operp++] = O_SUB; lpoint++;continue;
        case ')':
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            lpoint++;
            operp--;
            goto other;
        case 0:
        case ';':
            if (conv != O_POS) {
                o_out[outp].epoint = cpoint;
                o_out[outp].val.type = T_OPER;
                o_out[outp++].val.u.oper = conv;
            }
            break;
        default: goto syntaxe;
        }
        if (stop && o_oper[0] == O_PARENT) {
            if (!operp) {cd=3;break;}
            if (operp==1 && ch == ',') {cd=2; break;}
        }
        if (!operp) {cd=1;break;}
        err_msg(ERROR______EXPECTED,")"); goto error;
    syntaxe:
        err_msg(ERROR_EXPRES_SYNTAX,NULL);
    error:
        return 0;
    }
    if (large) cd=0;
    return cd;
}

static int get_val2_compat(void) {// length in bytes, defined
    uint8_t vsp = 0;
    enum type_e t1, t2;
    enum oper_e op;
    unsigned int i;
    struct value_s tmp;
    struct values_s *v1, *v2;
    int large = 0;

    if (outp2 >= outp) return 1;

    for (i=outp2;i<outp;i++) {
        if (o_out[i].val.type != T_OPER) {
            if (vsp >= values_size) {
                values_size += 16;
                values = realloc(values, sizeof(struct values_s)*values_size);
                if (!values) err_msg_out_of_memory();
                memset(&values[values_size-16], 0, 16 * sizeof(struct values_s));
            }
            if (!values[vsp].val) values[vsp].val = &none_value;
            if (o_out[i].val.type == T_NONE) {
                o_out[i].val.type = T_STR;
                val_replace(&values[vsp].val, &o_out[i].val);
                free(o_out[i].val.u.str.data);
            } else val_replace(&values[vsp].val, &o_out[i].val);
            values[vsp++].epoint = o_out[i].epoint;
            continue;
        }
        op = o_out[i].val.u.oper;

        if (op == O_SEPARATOR) {
            outp2 = i + 1;
            return 0;
        }
        if (vsp < 1) goto syntaxe;
        v1 = &values[vsp-1];
        t1 = try_resolv(&v1->val);
        if (t1 == T_STR) {
            if (str_to_num(v1->val, T_NUM, &tmp)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
            }
            val_replace(&v1->val, &tmp);
            t1 = v1->val->type;
        }
        if (op == O_LOWER || op == O_HIGHER) {
            switch (t1) {
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
                {
                    uint16_t val1 = v1->val->u.num.val;

                    switch (op) {
                    case O_HIGHER: val1 >>= 8;
                    case O_LOWER: val1 = (uint8_t)val1;
                    default: break;
                    }
                    set_uint(&new_value, val1);
                    val_replace(&v1->val, &new_value);
                    break;
                }
            default:
                err_msg_invalid_oper(op, v1->val, NULL, o_out[i].epoint);
                val_replace(&v1->val, &none_value); 
            case T_NONE:break;
            }
            v1->epoint = o_out[i].epoint;
            continue;
        }
        if (vsp < 2) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            outp2 = outp;
            return -1;
        }
        v2 = &values[vsp-2];
        t2 = try_resolv(&v2->val);
        if (t2 == T_STR) {
            if (str_to_num(v2->val, T_NUM, &tmp)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, v2->epoint); large=1;
            }
            val_replace(&v2->val, &tmp); 
            t2 = v2->val->type;
        }
        switch (t1) {
        case T_SINT:
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            switch (t2) {
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
                {
                    uint16_t val1 = v1->val->u.num.val;
                    uint16_t val2 = v2->val->u.num.val;

                    switch (op) {
                    case O_MUL: val1 *= val2; break;
                    case O_DIV: if (!val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v1->epoint); val1 = 0xffff;large=1;} else val1=val2 / val1; break;
                    case O_ADD: val1 += val2; break;
                    case O_SUB: val1 = val2 - val1; break;
                    case O_AND: val1 &= val2; break;
                    case O_OR:  val1 |= val2; break;
                    case O_XOR: val1 ^= val2; break;
                    default: break;
                    }
                    vsp--;
                    set_uint(&new_value, val1);
                    val_replace(&v2->val, &new_value);
                    continue;
                }
            default: err_msg_invalid_oper(op, v2->val, v1->val, o_out[i].epoint);
            case T_NONE:break;
            }
            break;
        default:
            err_msg_invalid_oper(op, v2->val, v1->val, o_out[i].epoint);
        case T_NONE:break;
        }
        vsp--; val_replace(&v2->val, &none_value); continue;
    }
    outp2 = i;
    if (large) return -1;
    return 0;
}

int eval_finish(void) {
    if (outp2 < outp) {
        lpoint = o_out[outp2].epoint;
        outp2 = outp;
        return 1;
    }
    return 0;
}

static int get_val2(int);

struct value_s *get_val(enum type_e type, unsigned int *epoint) {// length in bytes, defined
    int res;

    if (arguments.tasmcomp) {
        res = get_val2_compat();
    } else {
        res = get_val2(gstop);
    }
    if (res) return (res > 0) ? NULL : &error_value;

    if (epoint) *epoint = values[0].epoint;
    try_resolv_ident(&values[0].val);

    switch (values[0].val->type) {
    case T_STR:
    case T_SINT:
    case T_UINT:
    case T_NUM:
    case T_BOOL:
    case T_FLOAT:
    case T_GAP:
    case T_LIST:
    case T_TUPLE:
    case T_IDENTREF:
        if (type == T_IDENTREF) return values[0].val;
        try_resolv_identref(&values[0].val);
        if (type == T_NONE) return values[0].val;
        if (type_is_int(type) || type == T_GAP) {
            switch (values[0].val->type) {
            case T_STR:
                if (str_to_num2(&values[0].val, (type == T_GAP) ? T_NUM : type)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, values[0].epoint);
                    return &error_value;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
            case T_NONE:
                return values[0].val;
            case T_FLOAT:
                new_value.type = (type == T_GAP) ? T_NUM : type;
                if (type == T_SINT) set_int(&new_value, (ival_t)values[0].val->u.real);
                else set_uint(&new_value, (uval_t)values[0].val->u.real);
                val_replace(&values[0].val, &new_value);
                return values[0].val;
            case T_GAP: if (type == T_GAP) return values[0].val;
            default:
                break;
            }
        }
    default:
        err_msg_wrong_type(values[0].val, values[0].epoint);
        return &error_value;
    case T_UNDEF:
        if (pass == 1) break;
        err_msg_wrong_type(values[0].val, values[0].epoint);
        return &error_value;
    case T_NONE: break;
    }
    return &none_value;
}

static double to_float(const struct value_s *val) {
    switch (val->type) {
    case T_FLOAT: return val->u.real;
    case T_SINT: return (ival_t)val->u.num.val;
    case T_NUM:
    case T_BOOL:
    case T_UINT: return (uval_t)val->u.num.val;
    default:
         return 0.0;
    }
}

enum func_e {
    F_NONE, F_FLOOR, F_CEIL, F_ROUND, F_TRUNC, F_FRAC, F_SQRT, F_CBRT, F_LOG,
    F_LOG10, F_EXP, F_SIN, F_COS, F_TAN, F_ACOS, F_ASIN, F_ATAN, F_RAD, F_DEG,
    F_COSH, F_SINH, F_TANH, F_HYPOT, F_ATAN2, F_POW, F_SIGN, F_ABS
};

static struct value_s *apply_func(enum func_e func, struct value_s *v1, unsigned int epoint) {
    switch (func) {
    case F_SIGN:
        switch (v1->type) {
        case T_SINT:
            set_int(&new_value, ((ival_t)v1->u.num.val > 0) - ((ival_t)v1->u.num.val < 0));
            return &new_value;
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            set_int(&new_value, ((uval_t)v1->u.num.val) > 0);
            return &new_value;
        case T_FLOAT:
            set_int(&new_value, (v1->u.real > 0.0) - (v1->u.real < 0.0));
            return &new_value;
        case T_LIST:
        case T_TUPLE: break;
        default: err_msg_wrong_type(v1, epoint);
        case T_NONE: return &none_value;
        }
        break;
    case F_ABS:
        switch (v1->type) {
        case T_SINT:
            if (v1->u.num.val < 0) {
                set_int(&new_value, -v1->u.num.val);
                return &new_value;
            }
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            return v1;
        case T_FLOAT:
            if (v1->u.real < 0.0) {
                new_value.type = T_FLOAT;
                new_value.u.real = -v1->u.real;
                return &new_value;
            }
            return v1;
        case T_LIST:
        case T_TUPLE: break;
        default: err_msg_wrong_type(v1, epoint);
        case T_NONE: return &none_value;
        }
    default: break;
    }
    switch (v1->type) {
    case T_FLOAT:
    case T_SINT:
    case T_UINT:
    case T_NUM:
    case T_BOOL:
        new_value.type = T_FLOAT;
        new_value.u.real = to_float(v1);
        switch (func) {
        case F_FLOOR: new_value.u.real = floor(new_value.u.real);break;
        case F_CEIL: new_value.u.real = ceil(new_value.u.real);break;
        case F_SQRT: 
                     if (new_value.u.real < 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                     else new_value.u.real = sqrt(new_value.u.real);break;
        case F_LOG10:
                     if (new_value.u.real <= 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                     else new_value.u.real = log10(new_value.u.real);break;
        case F_LOG:
                     if (new_value.u.real <= 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                     else new_value.u.real = log(new_value.u.real);break;
        case F_EXP: new_value.u.real = exp(new_value.u.real);break;
        case F_SIN: new_value.u.real = sin(new_value.u.real);break;
        case F_COS: new_value.u.real = cos(new_value.u.real);break;
        case F_TAN: new_value.u.real = tan(new_value.u.real);break;
        case F_ACOS: 
                    if (new_value.u.real < -1.0 || new_value.u.real > 1.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                    else new_value.u.real = acos(new_value.u.real);break;
        case F_ASIN: 
                    if (new_value.u.real < -1.0 || new_value.u.real > 1.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                    else new_value.u.real = asin(new_value.u.real);break;
        case F_ATAN: new_value.u.real = atan(new_value.u.real);break;
        case F_CBRT: new_value.u.real = cbrt(new_value.u.real);break;
        case F_ROUND: new_value.u.real = round(new_value.u.real);break;
        case F_TRUNC: new_value.u.real = trunc(new_value.u.real);break;
        case F_FRAC: new_value.u.real -= trunc(new_value.u.real);break;
        case F_RAD: new_value.u.real = new_value.u.real * M_PI / 180.0;break;
        case F_DEG: new_value.u.real = new_value.u.real * 180.0 / M_PI;break;
        case F_COSH: new_value.u.real = cosh(new_value.u.real);break;
        case F_SINH: new_value.u.real = sinh(new_value.u.real);break;
        case F_TANH: new_value.u.real = tanh(new_value.u.real);break;
        default:break;
        }
        break;
    case T_LIST:
    case T_TUPLE:
            {
                size_t i = 0;
                struct value_s **vals, *val;
                if (v1->u.list.len) {
                    vals = malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (;i < v1->u.list.len; i++) {
                        val = apply_func(func, v1->u.list.data[i], epoint);
                        vals[i] = val_reference(val);
                        val_destroy(val);
                    }
                } else vals = NULL;
                new_value.type = v1->type;
                new_value.u.list.len = i;
                new_value.u.list.data = vals;
                break;
            }
    default: err_msg_wrong_type(v1, epoint);
    case T_NONE: return &none_value;
    }
    return &new_value;
}

static void functions(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    const struct value_s *v0;
    size_t len;
    const uint8_t *name;
    enum func_e func = F_NONE;

    if (vals->val->type != T_IDENT) {
        err_msg_invalid_oper(O_FUNC, vals->val, NULL, vals->epoint);
        val_replace(&vals->val, &none_value);
        return;
    }
    len = vals->val->u.ident.len;
    name = vals->val->u.ident.name;

    // len(a) - length of string in characters
    if (len == 3 && !memcmp(name, "len", len)) {
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else
        switch (try_resolv(&v[0].val)) {
        case T_STR:
            set_uint(&new_value, v[0].val->u.str.chars);
            val_replace(&vals->val, &new_value);
            return;
        case T_LIST:
        case T_TUPLE:
            set_uint(&new_value, v[0].val->u.list.len);
            val_replace(&vals->val, &new_value);
            return;
        default: err_msg_wrong_type(v[0].val, v[0].epoint);
        case T_NONE: break;
        }
        val_replace(&vals->val, &none_value);
        return;
    } // range([start],end,[step])
    if (len == 5 && !memcmp(name, "range", len)) {
        ival_t start = 0, end, step = 1;
        size_t i = 0, len;
        struct value_s **val;
        if (args < 1 || args > 3) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else {
            for (i = 0; i < args; i++) {
                switch (try_resolv(&v[i].val)) {
                case T_SINT:
                case T_UINT:
                case T_NUM:
                case T_BOOL:
                    break;
                default: err_msg_wrong_type(v[i].val, v[i].epoint);
                case T_NONE: 
                    val_replace(&vals->val, &none_value);
                    return;
                }
            }
            switch (args) {
            case 1: end = v[0].val->u.num.val;break;
            case 3: step = v[2].val->u.num.val;
            case 2: start = v[0].val->u.num.val;
                    end = v[1].val->u.num.val;break;
            }
            if (step == 0) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v[2].epoint); val_replace(&vals->val, &none_value); return;}
            if (step > 0) {
                if (end < start) end = start;
                len = (end - start + step - 1) / step;
            } else {
                if (end > start) end = start;
                len = (start - end - step - 1) / -step;
            }
            val = malloc(len * sizeof(new_value.u.list.data[0]));
            if (!val) err_msg_out_of_memory();
            i = 0;
            while ((end > start && step > 0) || (end < start && step < 0)) {
                if (start >= 0) set_uint(&new_value, (uval_t)start);
                else set_int(&new_value, start);
                val[i++] = val_reference(&new_value);
                start += step;
            }
            new_value.type = T_LIST;
            new_value.u.list.len = len;
            new_value.u.list.data = val;
            val_replace(&vals->val, &new_value); val_destroy(&new_value);
            return;
        }
        val_replace(&vals->val, &none_value);
        return;
    } // min(a, b, ...) - minimum value
    if (len == 3 && !memcmp(name, "min", len)) {
        ival_t min = 0;
        if (args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
        else {
            int volt = 1, t = 1;
            while (args--) {
                switch (try_resolv(&v[args].val)) {
                case T_SINT:
                    if (volt || (!t && min < 0) || v[args].val->u.num.val < min) {min = v[args].val->u.num.val;t = 1;}
                    break;
                case T_UINT:
                case T_NUM:
                case T_BOOL:
                    if (volt || ((!t || min > 0) && (uval_t)v[args].val->u.num.val < (uval_t)min)) {min = v[args].val->u.num.val; t = 0;}
                    break;
                default: err_msg_wrong_type(v[args].val, v[args].epoint);
                case T_NONE:
                    val_replace(&vals->val, &none_value);
                    return;
                }
                volt = 0;
            }
            if (t) set_int(&new_value, min); else set_uint(&new_value, min);
            val_replace(&vals->val, &new_value);
            return;
        }
        val_replace(&vals->val, &none_value);
        return;
    } // max(a, b, ...) - maximum value
    else if (len == 3 && !memcmp(name, "max", len)) {
        ival_t max = 0;
        if (args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
        else {
            int volt = 1, t = 1;
            while (args--) {
                switch (try_resolv(&v[args].val)) {
                case T_SINT:
                    if (volt || ((t || max < 0) && v[args].val->u.num.val > max)) {max = v[args].val->u.num.val;t = 1;}
                    break;
                case T_UINT:
                case T_NUM:
                case T_BOOL:
                    if (volt || (t && max < 0) || (uval_t)v[args].val->u.num.val > (uval_t)max) {max = v[args].val->u.num.val;t = 0;}
                    break;
                default: err_msg_wrong_type(v[args].val, v[args].epoint);
                case T_NONE:
                    val_replace(&vals->val, &none_value);
                    return;
                }
                volt = 0;
            }
            if (t) set_int(&new_value, max); else set_uint(&new_value, max);
            val_replace(&vals->val, &new_value);
            return;
        }
        val_replace(&vals->val, &none_value);
        return;
    } // size(a) - size of data structure at location
    else if (len == 4 && !memcmp(name, "size", len)) {
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
        else {
            int rec = 100;
            try_resolv_ident(&v[0].val);
            v0 = v[0].val;
        sizeretr:
            switch (v0->type) {
            case T_IDENTREF:
                switch (v0->u.ident.label->type) {
                case L_LABEL:
                case L_STRUCT:
                case L_UNION:
                    touch_label(v0->u.ident.label);
                    set_uint(&new_value, v0->u.ident.label->size);
                    val_replace(&vals->val, &new_value);
                    return;
                case L_VAR:
                case L_CONST:
                    if (touch_label(v0->u.ident.label)) {
                        v[0].val->type = T_UNDEF;
                        val_replace(&vals->val, v[0].val);
                        vals->epoint = v[0].epoint;
                        return;
                    }
                    v0 = v0->u.ident.label->value;
                    if (rec--) goto sizeretr;
                    err_msg2(ERROR__REFRECURSION, NULL, v[0].epoint);
                    break;
                }
            case T_NONE:
                break;
            case T_UNDEF:
                val_replace(&vals->val, v[0].val);
                vals->epoint = v[0].epoint;
                return;
            default: err_msg_wrong_type(v0, v[0].epoint);
            }
        }
        val_replace(&vals->val, &none_value);
        return;
    } else
    if (len == 5 && !memcmp(name, "floor", len)) func = F_FLOOR; else
    if (len == 4 && !memcmp(name, "ceil", len)) func = F_CEIL; else
    if (len == 5 && !memcmp(name, "round", len)) func = F_ROUND; else
    if (len == 5 && !memcmp(name, "trunc", len)) func = F_TRUNC; else
    if (len == 4 && !memcmp(name, "frac", len)) func = F_FRAC; else
    if (len == 4 && !memcmp(name, "sqrt", len)) func = F_SQRT; else
    if (len == 5 && !memcmp(name, "log10", len)) func = F_LOG10; else
    if (len == 3 && !memcmp(name, "log", len)) func = F_LOG; else
    if (len == 3 && !memcmp(name, "exp", len)) func = F_EXP; else
    if (len == 3 && !memcmp(name, "sin", len)) func = F_SIN; else
    if (len == 3 && !memcmp(name, "cos", len)) func = F_COS; else
    if (len == 3 && !memcmp(name, "tan", len)) func = F_TAN; else
    if (len == 4 && !memcmp(name, "acos", len)) func = F_ACOS; else
    if (len == 4 && !memcmp(name, "asin", len)) func = F_ASIN; else
    if (len == 4 && !memcmp(name, "atan", len)) func = F_ATAN; else
    if (len == 4 && !memcmp(name, "cbrt", len)) func = F_CBRT; else
    if (len == 3 && !memcmp(name, "rad", len)) func = F_RAD; else
    if (len == 3 && !memcmp(name, "deg", len)) func = F_DEG; else
    if (len == 4 && !memcmp(name, "cosh", len)) func = F_COSH; else
    if (len == 4 && !memcmp(name, "sinh", len)) func = F_SINH; else
    if (len == 4 && !memcmp(name, "tanh", len)) func = F_TANH; else
    if (len == 4 && !memcmp(name, "sign", len)) func = F_SIGN; else
    if (len == 3 && !memcmp(name, "abs", len)) func = F_ABS; 

    if (func != F_NONE) {
        try_resolv(&v[0].val);
        val_replace(&vals->val, apply_func(func, v[0].val, v[0].epoint));
        return;
    }
    func = F_NONE;
    if (len == 5 && !memcmp(name, "hypot", len)) func = F_HYPOT; else
    if (len == 5 && !memcmp(name, "atan2", len)) func = F_ATAN2; else
    if (len == 3 && !memcmp(name, "pow", len)) func = F_POW;
    if (func != F_NONE) {
        double val1, val2;
        if (args != 2) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else
        switch (try_resolv(&v[0].val)) {
        case T_SINT:
        case T_UINT:
        case T_NUM:
        case T_BOOL:
        case T_FLOAT:
            switch (try_resolv(&v[1].val)) {
            case T_SINT:
            case T_UINT:
            case T_NUM:
            case T_BOOL:
            case T_FLOAT:
                new_value.type = T_FLOAT;
                val1 = to_float(v[0].val);
                val2 = to_float(v[1].val);
                switch (func) {
                case F_HYPOT: new_value.u.real = hypot(val1, val2);break;
                case F_ATAN2: new_value.u.real = atan2(val1, val2);break;
                case F_POW:
                    if (val2 < 0.0 && !val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v[1].epoint); new_value.u.real = 0.0;}
                    else if (val1 < 0.0 && (double)((int)val2) != val2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, v[0].epoint); new_value.u.real = 0.0;}
                    else new_value.u.real = pow(val1, val2);
                    break;
                default: break;
                }
                val_replace(&vals->val, &new_value);
                return;
            default: err_msg_wrong_type(v[1].val, v[1].epoint);
            case T_NONE:break;
            }
            val_replace(&vals->val, &none_value);
            return;
        default: err_msg_wrong_type(v[0].val, v[0].epoint);
        case T_NONE: break;
        }
        val_replace(&vals->val, &none_value);
        return;
    }
    try_resolv(&vals->val);
    if (vals->val->type != T_NONE) {
        err_msg_wrong_type(vals->val, vals->epoint); 
        val_replace(&vals->val, &none_value);
    }
}

static inline int utf8len(uint8_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}

static void str_slice(struct values_s *vals, ival_t offs, ival_t end, ival_t step) {
    struct value_s *val;
    uint8_t *p, *p2;
    size_t len;

    if (step > 0) {
        if (offs > end) offs = end;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }
    if (step == 1) {
        new_value.u.str.chars = len;
        if (new_value.u.str.chars == vals->val->u.str.chars) {
            return; /* original string */
        }
        val = val_reference(vals->val);
        p = val->u.str.data;
        if (val->u.str.len == val->u.str.chars) {
            new_value.u.str.len = new_value.u.str.chars;
            new_value.u.str.data = p + offs;
        }
        else {
            ival_t i;
            for (i = 0; i < offs; i++) {
                p += utf8len(*p);
            }
            new_value.u.str.data = p;
            for (; i < end; i++) {
                p += utf8len(*p);
            }
            new_value.u.str.len = p - new_value.u.str.data;
        }
        new_value.type = T_STR;
        val_replace(&vals->val, &new_value); val_destroy(val);
    } else {
        val = vals->val;
        new_value.u.str.chars = len;
        if (!new_value.u.str.chars) {
            val_replace(&vals->val, &null_str);return;
        }
        if (val->u.str.len == val->u.str.chars) {
            new_value.u.str.len = new_value.u.str.chars;
            p = new_value.u.str.data = malloc(new_value.u.str.len);
            if (!new_value.u.str.data) err_msg_out_of_memory();
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p++ = val->u.str.data[offs];
                offs += step;
            }
        }
        else {
            ival_t i = offs, j, k;
            p = val->u.str.data;
            new_value.u.str.len = 0;
            for (i = 0; i < offs; i++) {
                p += utf8len(*p);
            }
            p2 = p;
            if (step > 0) {
                for (k = offs; i < end; i++) {
                    j = utf8len(*p2);
                    if (i == k) {new_value.u.str.len += j; k += step;}
                    p2 += j;
                }
            } else {
                p2 += utf8len(*p2);
                for (k = i = offs; i > end; i--) {
                    j = 0;
                    do {
                        p2--;j++;
                    } while (*p2 >= 0x80 && *p2 < 0xc0);
                    if (i == k) {new_value.u.str.len += j; k += step;}
                }
            }
            p2 = p;
            p = malloc(new_value.u.str.len);
            if (!p) err_msg_out_of_memory();
            new_value.u.str.data = p;
            if (step > 0) {
                for (k = i = offs; i < end; i++) {
                    j = utf8len(*p2);
                    if (i == k) {memcpy(p, p2, j);p += j; k += step;}
                    p2 += j;
                }
            } else {
                p2 += utf8len(*p2);
                for (k = i = offs; i > end; i--) {
                    j = 0;
                    do {
                        p2--;j++;
                    } while (*p2 >= 0x80 && *p2 < 0xc0);
                    if (i == k) {memcpy(p, p2, j);p += j; k += step;}
                }
            }
        }
        new_value.type = T_STR;
        val_replace(&vals->val, &new_value); val_destroy(&new_value);
    }
}

static void list_slice(struct values_s *vals, ival_t offs, ival_t end, ival_t step) {
    struct value_s *val;
    size_t i;
    size_t len;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }
    if (step == 1) {
        new_value.u.list.len = len;
        if (new_value.u.list.len == vals->val->u.list.len && new_value.type == T_TUPLE) {
            return; /* original tuple */
        }
        val = val_reference(vals->val);
        new_value.u.list.data = &val->u.list.data[offs];
        new_value.type = vals->val->type;
        val_replace(&vals->val, &new_value); val_destroy(val);
    } else {
        val = vals->val;
        new_value.type = vals->val->type;
        new_value.u.list.len = len;
        if (!new_value.u.list.len) {
            val_replace(&vals->val, (new_value.type == T_TUPLE) ? &null_tuple : &null_list);return;
        }
        new_value.u.list.data = malloc(new_value.u.list.len * sizeof(new_value.u.list.data[0]));
        if (!new_value.u.list.data) err_msg_out_of_memory();
        i = 0;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            new_value.u.list.data[i++] = val_reference(val->u.list.data[offs]);
            offs += step;
        }
        val_replace(&vals->val, &new_value); val_destroy(&new_value);
    }
}

static void indexes(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];

    switch (try_resolv(&vals->val)) {
    case T_LIST:
    case T_TUPLE:
    case T_STR:
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals[0].epoint); else
            switch (try_resolv(&v[0].val)) {
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
                {
                    struct value_s *val;
                    size_t len = (vals->val->type == T_STR) ? vals->val->u.str.chars : vals->val->u.list.len, offs;

                    if (v[0].val->type != T_SINT || v[0].val->u.num.val >= 0) {
                        if ((uval_t)v[0].val->u.num.val < len) offs = (uval_t)v[0].val->u.num.val;
                        else {err_msg2(ERROR_CONSTNT_LARGE, NULL, v[0].epoint); val_replace(&vals->val, &none_value); return;}
                    } else {
                        if ((uval_t)-v[0].val->u.num.val <= len) offs = len + v[0].val->u.num.val;
                        else {err_msg2(ERROR_CONSTNT_LARGE, NULL, v[0].epoint); val_replace(&vals->val, &none_value); return;}
                    }
                    if (vals->val->type == T_STR) { 
                        str_slice(vals, offs, offs + 1, 1);
                    } else {
                        val = val_reference(vals->val->u.list.data[offs]);
                        val_replace(&vals->val, val);
                        val_destroy(val);
                    }
                }
                return;
            default: err_msg_invalid_oper(O_INDEX, v[0].val, NULL, v[0].epoint);
            case T_NONE: 
                val_replace(&vals->val, &none_value);
                return;
            }
        break;
    default: err_msg_invalid_oper(O_INDEX, vals->val, NULL, vals->epoint);
             val_replace(&vals->val, &none_value);
    case T_NONE: return;
    }
    return;
}

static void slices(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    int i;

    switch (try_resolv(&vals->val)) {
    case T_LIST:
    case T_TUPLE:
    case T_STR:
        if (args > 3 || args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals[0].epoint); else {
            uval_t len = (vals->val->type == T_STR) ? vals->val->u.str.chars : vals->val->u.list.len;
            ival_t offs = 0, end = (ival_t)len, step = 1;
            for (i = args - 1; i >= 0; i--) {
                switch (try_resolv(&v[i].val)) {
                case T_SINT:
                case T_UINT:
                case T_NUM:
                case T_BOOL:
                case T_DEFAULT:
                    break;
                default: err_msg_wrong_type(v[i].val, v[i].epoint);
                case T_NONE: 
                    val_replace(&vals->val, &none_value);
                    return;
                }
            }
            if (args > 2) {
                if (v[2].val->type != T_DEFAULT) {
                    if (v[2].val->u.num.val == 0) {
                        err_msg2(ERROR_DIVISION_BY_Z, NULL, v[0].epoint); val_replace(&vals->val, &none_value);return;
                    }
                    step = v[2].val->u.num.val;
                }
            }
            if (args > 1) {
                if (v[1].val->type == T_DEFAULT) end = (step > 0) ? (ival_t)len : -1;
                else {
                    if (v[1].val->type != T_SINT || v[1].val->u.num.val >= 0) {
                        end = v[1].val->u.num.val;
                        if (end > (ival_t)len) end = len;
                    } else {
                        end = v[1].val->u.num.val;
                        if (end < 0) end += len;
                    }
                    if (end < -1) end = -1;
                }
            } else end = len;
            if (v[0].val->type == T_DEFAULT) offs = (step > 0) ? 0 : len - 1;
            else {
                if (v[0].val->type != T_SINT || v[0].val->u.num.val >= 0) {
                    offs = v[0].val->u.num.val;
                    if (offs > (ival_t)len - (step < 0)) offs = len - (step < 0);
                } else {
                    offs = v[0].val->u.num.val;
                    if (offs < 0) offs += len;
                }
                if (offs < - (step < 0)) offs = - (step < 0);
            }
            if (vals->val->type == T_STR) {
                str_slice(vals, offs, end, step);
            } else {
                list_slice(vals, offs, end, step);
            }
            return;
        }
        val_replace(&vals->val, &none_value);
        return;
    default: err_msg_invalid_oper(O_SLICE, vals->val, NULL, vals->epoint);
             val_replace(&vals->val, &none_value);
    case T_NONE: return;
    }
    return;
}

static struct value_s *apply_op(enum oper_e op, const struct value_s *v1, unsigned int epoint, unsigned int epoint2, int *large) {
    enum type_e t1;
    struct value_s tmp1;
    char line[linelength]; 

    v1 = unwind_identrefs(v1, epoint);
    t1 = v1->type;
strretr:
    if (t1 == T_NONE) {
        return &none_value;
    }

    if (type_is_int(t1)) {
        switch (op) {
        case O_LOWER:
            new_value.type = T_NUM;
            new_value.u.num.len = 1;
            new_value.u.num.val = (uint8_t)v1->u.num.val;
            return &new_value;
        case O_HIGHER: 
            new_value.type = T_NUM;
            new_value.u.num.len = 1;
            new_value.u.num.val = (uint8_t)(v1->u.num.val >> 8);
            return &new_value;
        case O_BANK: 
            new_value.type = T_NUM;
            new_value.u.num.len = 1;
            new_value.u.num.val = (uint8_t)(v1->u.num.val >> 16);
            return &new_value;
        case O_BSWORD:
            new_value.type = T_NUM;
            new_value.u.num.len = 2;
            new_value.u.num.val = (uint8_t)(v1->u.num.val >> 8) | (uint16_t)(v1->u.num.val << 8);
            return &new_value;
        case O_HWORD: 
            new_value.type = T_NUM;
            new_value.u.num.len = 2;
            new_value.u.num.val = (uint16_t)(v1->u.num.val >> 8);
            return &new_value;
        case O_WORD: 
            new_value.type = T_NUM;
            new_value.u.num.len = 2;
            new_value.u.num.val = (uint16_t)v1->u.num.val;
            return &new_value;
        case O_INV:
            new_value.type = T_NUM;
            new_value.u.num.val = ~v1->u.num.val;
            new_value.u.num.len = v1->u.num.len;
            return &new_value;
        case O_NEG:
            new_value.type = T_SINT;
            new_value.u.num.val = -v1->u.num.val;
            new_value.u.num.len = v1->u.num.len;
            return &new_value;
        case O_POS:
            new_value.type = T_SINT;
            new_value.u.num.val = v1->u.num.val;
            new_value.u.num.len = v1->u.num.len;
            return &new_value;
        case O_STRING:
            sprintf(line, (v1->type == T_SINT) ? "%" PRIdval : "%" PRIuval, v1->u.num.val);
            new_value.type = T_STR;
            new_value.u.str.len = strlen(line);
            new_value.u.str.chars = new_value.u.str.len;
            new_value.u.str.data = malloc(new_value.u.str.len);
            if (!new_value.u.str.data) err_msg_out_of_memory();
            memcpy(new_value.u.str.data, line, new_value.u.str.len);
            return &new_value;
        default: err_msg_invalid_oper(op, v1, NULL, epoint2); 
                 return &none_value;
        }
    }
    if (t1 == T_STR) {
        switch (op) {
        case O_LOWER:
        case O_HIGHER: 
        case O_BANK: 
        case O_BSWORD:
        case O_HWORD: 
        case O_WORD: 
        case O_INV:
        case O_NEG:
        case O_POS:
            if (str_to_num(v1, T_NUM, &tmp1)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint); *large=1;
            }
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        case O_STRING:
            new_value.type = T_STR;
            new_value.u.str.len = v1->u.str.len;
            new_value.u.str.chars = v1->u.str.chars;
            if (new_value.u.str.len) {
                new_value.u.str.data = malloc(new_value.u.str.len);
                if (!new_value.u.str.data) err_msg_out_of_memory();
                memcpy(new_value.u.str.data, v1->u.str.data, new_value.u.str.len);
            } else new_value.u.str.data = NULL;
            return &new_value;
        default: err_msg_invalid_oper(op, v1, NULL, epoint2);
                 return &none_value;
        }
    }
    if (t1 == T_FLOAT) {
        switch (op) {
        case O_LOWER:
        case O_HIGHER: 
        case O_BANK: 
        case O_BSWORD:
        case O_HWORD: 
        case O_WORD: 
        case O_INV:
            tmp1.type = T_NUM;
            tmp1.u.num.val = (ival_t)v1->u.real;
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        case O_NEG:
            new_value.type = T_FLOAT;
            new_value.u.real = -v1->u.real;
            return &new_value;
        case O_POS:
            new_value.type = T_FLOAT;
            new_value.u.real = v1->u.real;
            return &new_value;
        case O_STRING:
            {
                int i = 0;
                sprintf(line, "%.10g", v1->u.real);
                while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
                if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
                new_value.type = T_STR;
                new_value.u.str.len = strlen(line);
                new_value.u.str.chars = new_value.u.str.len;
                new_value.u.str.data = malloc(new_value.u.str.len);
                if (!new_value.u.str.data) err_msg_out_of_memory();
                memcpy(new_value.u.str.data, line, new_value.u.str.len);
                return &new_value;
            }
        default: err_msg_invalid_oper(op, v1, NULL, epoint2);
                 return &none_value;
        }
    }
    if (t1 == T_LIST || t1 == T_TUPLE) {
        switch (op) {
        case O_LOWER:
        case O_HIGHER: 
        case O_BANK: 
        case O_BSWORD:
        case O_HWORD: 
        case O_WORD: 
        case O_INV:
        case O_NEG:
        case O_STRING:
            {
                size_t i = 0;
                struct value_s **vals, *val;
                if (v1->u.list.len) {
                    vals = malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (;i < v1->u.list.len; i++) {
                        val = apply_op(op, v1->u.list.data[i], epoint, epoint2, large);
                        vals[i] = val_reference(val);
                        val_destroy(val);
                    }
                } else vals = NULL;
                new_value.type = t1;
                new_value.u.list.len = i;
                new_value.u.list.data = vals;
                return &new_value;
            }
        case O_POS:
            {
                size_t i, j;
                if (v1->u.list.len) {
                    switch (v1->u.list.data[0]->type) {
                    case T_STR:
                        new_value.type = T_STR;
                        new_value.u.str.len = 0;
                        new_value.u.str.chars = 0;
                        for (i = 0;i < v1->u.list.len; i++) {
                            switch (v1->u.list.data[i]->type) {
                            case T_STR:
                                new_value.u.str.len += v1->u.list.data[i]->u.str.len;
                                new_value.u.str.chars += v1->u.list.data[i]->u.str.chars;
                                break;
                            default:err_msg_invalid_oper(op, v1, NULL, epoint2); 
                            case T_NONE: return &none_value;
                            }
                        }
                        new_value.u.str.data = malloc(new_value.u.str.len);
                        if (!new_value.u.str.data) err_msg_out_of_memory();
                        for (i = 0;i < v1->u.list.len; i++) {
                            memcpy(new_value.u.str.data, v1->u.list.data[i]->u.str.data, v1->u.list.data[i]->u.str.len);
                            new_value.u.str.data += v1->u.list.data[i]->u.str.len;
                        }
                        new_value.u.str.data -= new_value.u.str.len;
                        return &new_value;
                    case T_LIST:
                    case T_TUPLE:
                        new_value.type = v1->u.list.data[0]->type;
                        new_value.u.list.len = 0;
                        for (i = 0;i < v1->u.list.len; i++) {
                            switch (v1->u.list.data[i]->type) {
                            case T_LIST:
                            case T_TUPLE:
                                if (v1->u.list.data[i]->type == new_value.type) {
                                    new_value.u.list.len += v1->u.list.data[i]->u.list.len;
                                    break;
                                }
                            default:err_msg_invalid_oper(op, v1, NULL, epoint2); 
                            case T_NONE: return &none_value;
                            }
                        }
                        new_value.u.list.data = malloc(new_value.u.list.len * sizeof(new_value.u.list.data[0]));
                        if (!new_value.u.list.data) err_msg_out_of_memory();
                        for (i = 0;i < v1->u.list.len; i++) {
                            for (j = 0;j < v1->u.list.data[i]->u.list.len; j++) {
                                *new_value.u.list.data = val_reference(v1->u.list.data[i]->u.list.data[j]);
                                new_value.u.list.data++;
                            }
                        }
                        new_value.u.list.data -= new_value.u.list.len;
                        return &new_value;
                    default:err_msg_invalid_oper(op, v1, NULL, epoint2); 
                    case T_NONE: return &none_value;
                    }
                }
                new_value.type = t1;
                new_value.u.list.len = 0;
                new_value.u.list.data = NULL;
                return &new_value;
            }
        default: err_msg_invalid_oper(op, v1, NULL, epoint2); 
                 return &none_value;
        }
    }
    if (t1 == T_UNDEF) err_msg_wrong_type(v1, epoint); 
    else err_msg_invalid_oper(op, v1, NULL, epoint2);
    return &none_value;
}

static struct value_s *apply_op2(enum oper_e op, const struct value_s *v1, const struct value_s *v2, unsigned int epoint, unsigned int epoint2, unsigned int epoint3, int *large) {
    enum type_e t1;
    enum type_e t2;
    struct value_s tmp1, tmp2;

    v1 = unwind_identrefs(v1, epoint);
    t1 = v1->type;
    v2 = unwind_identrefs(v2, epoint2);
    t2 = v2->type;
strretr:
    if (t1 == T_NONE) return &none_value;
    if (t2 == T_NONE) {
    errtype:
        return &none_value;
    }

    if (type_is_int(t1) && type_is_int(t2)) {
        ival_t val1 = v1->u.num.val;
        ival_t val2 = v2->u.num.val;

        if (t2 < t1) t2 = t1;

        switch (op) {
        case O_EQ:   return ( val1 == val2) ? &true_value : &false_value;
        case O_NEQ:  return ( val1 != val2) ? &true_value : &false_value;
        case O_LT:   return ( val1 <  val2) ? &true_value : &false_value;
        case O_GT:   return ( val1 >  val2) ? &true_value : &false_value;
        case O_LE:   return ( val1 <= val2) ? &true_value : &false_value;
        case O_GE:   return ( val1 >= val2) ? &true_value : &false_value;
        case O_MUL:  val1 = ( val1 *  val2);break;
        case O_DIV: if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); val1 = (~(uval_t)0) >> 1; *large=1;}
                        else if (t2==T_SINT) val1 = ( val1 / val2); else val1 = ( (uval_t)val1 / (uval_t)val2);  break;
        case O_MOD: if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); val1 = (~(uval_t)0) >> 1; *large=1;}
                        else if (t2==T_SINT) val1 = ( val1 % val2); else val1 = ( (uval_t)val1 % (uval_t)val2); break;
        case O_ADD:  val1 = ( val1 +  val2);break;
        case O_SUB:  if (t1 == T_UINT && (uval_t)val2 > (uval_t)val1) t1 = T_SINT;
                     val1 = ( val1 -  val2);break;
        case O_AND:  val1 = ( val1 &  val2);break;
        case O_OR:   val1 = ( val1 |  val2);break;
        case O_XOR:  val1 = ( val1 ^  val2);break;
        case O_LSHIFT:
                     if (val2 >= (ival_t)sizeof(val1)*8 || val2 <= -(ival_t)sizeof(val1)*8) val1=0;
                     else val1 = (val2 > 0) ? (val1 << val2) : (ival_t)((uval_t)val1 >> (-val2));
                     break;
        case O_ASHIFT: 
                     if (t1 == T_SINT) {
                         if (val2 >= (ival_t)sizeof(val1)*8) val1 = (val1 > 0) ? 0 : -1;
                         if (val2 <= -(ival_t)sizeof(val1)*8) val1 = 0;
                         else if (val1 >= 0) val1 = (val2 > 0) ? (val1 >> val2) : (val1 << (-val2));
                         else val1 = ~((val2 > 0) ? ((~val1) >> val2) : ((~val1) << (-val2)));
                         break;
                     }
        case O_RSHIFT: 
                     if (val2 >= (ival_t)sizeof(val1)*8 || val2 <= -(ival_t)sizeof(val1)*8) val1=0;
                     else val1 = (val2 > 0) ? (ival_t)((uval_t)val1 >> val2) : (val1 << (-val2));
                     break;
        case O_EXP: 
                     {
                         ival_t res = 1;

                         if (val2 < 0) {
                             if (!val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); res = (~(uval_t)0) >> 1; *large=1;}
                             else res = 0;
                         } else {
                             while (val2) {
                                 if (val2 & 1) res *= val1;
                                 val1 *= val1;
                                 val2 >>= 1;
                             }
                         }
                         val1 = res;
                     }
                     break;
        default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                 goto errtype;
        }
        if (t1 == T_SINT) set_int(&new_value, val1); else {set_uint(&new_value, val1); new_value.type = t1;}
        return &new_value;
    }

    if (t1 == T_STR && type_is_num(t2)) {
        if (str_to_num(v1, T_NUM, &tmp1)) {
            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint); *large=1;
        }
        v1 = &tmp1;
        t1 = v1->type;
        goto strretr;
    }
    if (t2 == T_STR && type_is_num(t1)) {
        if (str_to_num(v2, T_NUM, &tmp2)) {
            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint2); *large=1;
        }
        v2 = &tmp2;
        t2 = v2->type;
        goto strretr;
    }
    if (t1 == T_STR && t2 == T_STR) {
        int val;
        switch (op) {
        case O_EQ:
            val = (v1->u.str.len == v2->u.str.len) && (v1->u.str.data == v2->u.str.data || !memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len));
        strcomp:
            return val ? &true_value : &false_value;
        case O_NEQ:
            val = (v1->u.str.len != v2->u.str.len) || (v1->u.str.data != v2->u.str.data && memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len));
            goto strcomp;
        case O_LT:
            val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
            if (!val) val = v1->u.str.len < v2->u.str.len; else val = val < 0;
            goto strcomp;
        case O_GT:
            val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
            if (!val) val = v1->u.str.len > v2->u.str.len; else val = val > 0;
            goto strcomp;
        case O_LE:
            val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
            if (!val) val = v1->u.str.len <= v2->u.str.len; else val = val <= 0;
            goto strcomp;
        case O_GE:
            val = memcmp(v1->u.str.data, v2->u.str.data, (v1->u.str.len < v2->u.str.len) ? v1->u.str.len:v2->u.str.len);
            if (!val) val = v1->u.str.len >= v2->u.str.len; else val = val >= 0;
            goto strcomp;
        case O_MUL: 
        case O_DIV:
        case O_MOD:
        case O_ADD:
        case O_SUB:
        case O_AND:
        case O_OR:
        case O_XOR:
        case O_LSHIFT:
        case O_ASHIFT: 
        case O_RSHIFT: 
        case O_EXP: 
            if (str_to_num(v1, T_NUM, &tmp1)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint); *large=1;
            }
            if (str_to_num(v2, T_NUM, &tmp2)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint2); *large=1;
            }
            v1 = &tmp1; v2 = &tmp2;
            t1 = v1->type;
            t2 = v2->type;
            goto strretr;
        default: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
        }
    }

    if (t1 == T_FLOAT && type_is_int(t2)) {
        tmp2.type = T_FLOAT;
        tmp2.u.real = (t2 == T_SINT) ? (double)((ival_t)v2->u.num.val) : (double)((uval_t)v2->u.num.val);
        v2 = &tmp2;
        t2 = v2->type;
    }
    if (t2 == T_FLOAT && type_is_int(t1)) {
        tmp1.type = T_FLOAT;
        tmp1.u.real = (t1 == T_SINT) ? (double)((ival_t)v1->u.num.val) : (double)((uval_t)v1->u.num.val);
        v1 = &tmp1;
        t1 = v1->type;
    }
    if (t1 == T_FLOAT && t2 == T_FLOAT) {
        double val1 = v1->u.real;
        double val2 = v2->u.real;

        switch (op) {
        case O_EQ:   return almost_equal(val1, val2) ? &true_value : &false_value;
        case O_NEQ:  return almost_equal(val1, val2) ? &false_value : &true_value;
        case O_LT:   return ( val1 <  val2) ? &true_value : &false_value;
        case O_GT:   return ( val1 >  val2) ? &true_value : &false_value;
        case O_LE:   return ( val1 < val2 || almost_equal(val1, val2)) ? &true_value : &false_value;
        case O_GE:   return ( val1 > val2 || almost_equal(val1, val2)) ? &true_value : &false_value;
        case O_MUL:  val1 = ( val1 *  val2);break;
        case O_DIV: if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); val1 = 0.0; *large=1;}
                        else val1 = ( val1 /  val2); break;
        case O_MOD: if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); val1 = 0.0; *large=1;}
                        else val1 = fmod(val1, val2); break;
        case O_ADD:  val1 = ( val1 +  val2);break;
        case O_SUB:  val1 = ( val1 -  val2);break;
        case O_EXP: 
                     {
                         if (val2 < 0.0 && !val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, epoint2); val1 = 0.0; *large=1;}
                         else if (val1 < 0.0 && (double)((int)val2) != val2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint2); new_value.u.real = 0.0;}
                         else val1 = pow(val1, val2);
                     }
                     break;
        default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                 goto errtype;
        }
        new_value.type = t1;
        new_value.u.real = val1;
        return &new_value;
    }

    if ((t1 == T_LIST && t2 == T_LIST) || (t1 == T_TUPLE && t2 == T_TUPLE)) {
        int val;
        switch (op) {
        case O_EQ:
            val = val_equal(v1, v2);
        listcomp:
            return val ? &true_value : &false_value;
        case O_NEQ:
            val = !val_equal(v1, v2);
            goto listcomp;
        default: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
        }
        return &new_value;
    }
    if ((t1 == T_LIST || t1 == T_TUPLE) && type_is_num(t2)) {
        size_t i = 0;
        struct value_s **vals, *val;
        if (v1->u.list.len) {
            vals = malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
            for (;i < v1->u.list.len; i++) {
                val = apply_op2(op, v1->u.list.data[i], v2, epoint, epoint2, epoint3, large);
                vals[i] = val_reference(val);
                val_destroy(val);
            }
        } else vals = NULL;
        new_value.type = t1;
        new_value.u.list.len = i;
        new_value.u.list.data = vals;
        return &new_value;
    }
    if ((t2 == T_LIST || t2 == T_TUPLE) && type_is_num(t1)) {
        size_t i = 0;
        struct value_s **vals, *val;
        if (v2->u.list.len) {
            vals = malloc(v2->u.list.len * sizeof(new_value.u.list.data[0]));
            for (;i < v2->u.list.len; i++) {
                val = apply_op2(op, v1, v2->u.list.data[i], epoint, epoint2, epoint3, large);
                vals[i] = val_reference(val);
                val_destroy(val);
            }
        } else vals = NULL;
        new_value.type = t2;
        new_value.u.list.len = i;
        new_value.u.list.data = vals;
        return &new_value;
    }
    if ((type_is_num(t1) || t1 == T_STR || t1 == T_LIST || t1 == T_TUPLE) && (type_is_num(t2) || t2 == T_STR || t2 == T_LIST || t2 == T_TUPLE) && t1 != t2) {
        switch (op) {
        case O_EQ: return &false_value;
        case O_NEQ: return &true_value;
        case O_LT: return (t1 < t2) ? &true_value : &false_value;
        case O_GT: return (t1 > t2) ? &true_value : &false_value;
        case O_LE: return (t1 <= t2) ? &true_value : &false_value;
        case O_GE: return (t1 >= t2) ? &true_value : &false_value;
        default: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
        }
    }

    if (t1 == T_UNDEF) err_msg_wrong_type(v1, epoint); 
    else if (t2 == T_UNDEF) err_msg_wrong_type(v2, epoint2); 
    else err_msg_invalid_oper(op, v1, v2, epoint3);
    goto errtype;
}

static int get_val2(int stop) {
    uint8_t vsp = 0;
    unsigned int i;
    enum oper_e op;
    struct values_s *v1, *v2;
    enum type_e t1, t2;
    int large=0;

    if (outp2 >= outp) return 1;

    for (i=outp2;i<outp;i++) {
        op = o_out[i].val.u.oper;
        if (o_out[i].val.type != T_OPER || op == O_PARENT || op == O_BRACKET) {
            if (vsp >= values_size) {
                values_size += 16;
                values = realloc(values, sizeof(struct values_s)*values_size);
                if (!values) err_msg_out_of_memory();
                memset(&values[values_size-16], 0, 16 * sizeof(struct values_s));
            }
            if (!values[vsp].val) values[vsp].val = &none_value;
            val_replace(&values[vsp].val, &o_out[i].val);
            values[vsp++].epoint = o_out[i].epoint;
            continue;
        }

        if (op == O_SEPARATOR) {
            outp2 = i + 1;
            return 0;
        }
        if (op == O_COMMA) continue;
        if (vsp == 0) goto syntaxe;
        v1 = &values[vsp-1];
        switch (op) {
        case O_MEMBER:
            {
                char ident[linelength];
                struct value_s *vv1;
                int rec = 100;
                v2 = v1; vsp--;
                if (vsp == 0) goto syntaxe;
                v1 = &values[vsp-1];
                if (v1->val->type == T_NONE) continue;
                try_resolv_ident(&v1->val);
                vv1 = v1->val;
            membretr:
                if (vv1->type == T_IDENTREF) {
                    if (vv1->u.ident.label->type == L_CONST || vv1->u.ident.label->type == L_VAR) {
                        if (touch_label(vv1->u.ident.label)) {
                            vv1->type = T_UNDEF;
                            continue;
                        }
                        vv1 = vv1->u.ident.label->value;
                        if (vv1->type == T_NONE) goto errtype;
                        if (rec--) goto membretr;
                        err_msg2(ERROR__REFRECURSION, NULL, v1->epoint);
                        goto errtype;
                    }
                    if (v2->val->type == T_IDENT) {
                        copy_name(v2->val, ident);
                        new_value.u.ident.label = find_label2(ident, &vv1->u.ident.label->members);
                        new_value.type = (new_value.u.ident.label) ? T_IDENTREF : T_UNDEF;
                        if (new_value.u.ident.label) touch_label(vv1->u.ident.label);
                        new_value.u.ident.len = v2->val->u.ident.len;
                        new_value.u.ident.name = v2->val->u.ident.name;
                        val_replace(&v1->val, &new_value);
                        v1->epoint=v2->epoint;
                        continue;
                    } else err_msg_invalid_oper(op, vv1, v2->val, v2->epoint);
                } else if (vv1->type == T_UNDEF) continue;
                else err_msg_invalid_oper(op, vv1, v2->val, v1->epoint);
            errtype:
                val_replace(&v1->val, &none_value);
                continue;
            }
        case O_FUNC:
        case O_INDEX:
        case O_SLICE:
        case O_SLICE2:
            {
                unsigned int args = 0, slice = (op == O_SLICE || op == O_SLICE2);
                op = (op == O_FUNC) ? O_PARENT : O_BRACKET;
                while (v1->val->type != T_OPER || v1->val->u.oper != op) {
                    args++;
                    v1 = &values[vsp-1-args];
                }
                if (op == O_PARENT) functions(&values[vsp-2-args], args);
                else if (slice) slices(&values[vsp-2-args], args);
                else indexes(&values[vsp-2-args], args);
                vsp -= args + 1;
                continue;
            }
        case O_RBRACKET:
        case O_RPARENT:
        case O_TUPLE:
            {
                unsigned int args = 0, args2, tup = (op == O_RPARENT);
                op = (op == O_RBRACKET) ? O_BRACKET : O_PARENT;
                while (v1->val->type != T_OPER || v1->val->u.oper != op) {
                    args++;
                    v1 = &values[vsp-1-args];
                }
                if ((tup || stop) && args == 1) {val_replace(&v1->val, values[vsp-1].val); vsp--;continue;}
                v1->val->type = (op == O_BRACKET) ? T_LIST : T_TUPLE; // safe, replacing of T_OPER
                v1->val->u.list.len = args;
                if (args) {
                    args2 = args;
                    v1->val->u.list.data = malloc(args * sizeof(v1->val->u.list.data[0]));
                    if (!v1->val->u.list.data) err_msg_out_of_memory();
                    while (args--) {
                        try_resolv(&values[vsp-1].val);
                        if (values[vsp-1].val->type == T_UNDEF) {
                            while (args2 > args + 1) {
                                args2--;
                                val_destroy(v1->val->u.list.data[args2]);
                            }
                            free(v1->val->u.list.data);
                            v1->val->type = T_UNDEF;
                            v1->val->u.ident.len = values[vsp-1].val->u.ident.len;
                            v1->val->u.ident.name = values[vsp-1].val->u.ident.name;
                            v1->epoint = values[vsp-1].epoint;
                            vsp -= args + 1;
                            break;
                        } 
                        v1->val->u.list.data[args] = val_reference(values[vsp-1].val);
                        vsp--;
                    }
                } else v1->val->u.list.data = NULL;
                continue;
            }
        case O_COLON:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1]; vsp--;
            if (vsp == 0) goto syntaxe;
            switch (try_resolv(&values[vsp-1].val)) {
            case T_STR:
            case T_UINT:
            case T_SINT: 
            case T_BOOL:
            case T_LIST:
            case T_TUPLE:
                if (val_truth(values[vsp-1].val)) {
                    val_replace(&values[vsp-1].val, v1->val);
                    values[vsp-1].epoint = v1->epoint;
                } else {
                    val_replace(&values[vsp-1].val, v2->val);
                    values[vsp-1].epoint = v2->epoint;
                }
                continue;
            default: err_msg_invalid_oper(O_COND, values[vsp-1].val, NULL, values[vsp-1].epoint); 
                     goto errtype;
            case T_NONE: continue;
            }
        case O_COND:
            if (vsp == 1) goto syntaxe;
            continue;
        case O_WORD: // <>
        case O_HWORD: // >`
        case O_BSWORD: // ><
        case O_LOWER: // <
        case O_HIGHER: // >
        case O_BANK: // `
        case O_INV: // ~
        case O_NEG: // -
        case O_POS: // +
        case O_STRING: // ^
            {
                struct value_s *tmp;
                try_resolv_ident(&v1->val);
                tmp = apply_op(op, v1->val, v1->epoint, o_out[i].epoint, &large);
                val_replace(&v1->val, tmp);
                v1->epoint = o_out[i].epoint;
                val_destroy(tmp);
                continue;
            }
        case O_LNOT:
            switch (try_resolv(&v1->val)) {
            case T_SINT:
            case T_NUM: 
            case T_BOOL:
            case T_UINT:
            case T_STR:
            case T_FLOAT:
            case T_LIST:
            case T_TUPLE:
                val_replace(&v1->val, val_truth(v1->val) ? &false_value : &true_value);
                v1->epoint = o_out[i].epoint;
                continue;
            default: err_msg_invalid_oper(op, v1->val, NULL, v1->epoint);
                     goto errtype;
            case T_NONE: continue;
            }
        case O_LAND:
        case O_LOR:
        case O_LXOR:
            v2 = v1; v1 = &values[--vsp-1];
            if (vsp == 0) goto syntaxe;
            t1 = try_resolv(&v1->val);
            switch (t1) {
            case T_SINT:
            case T_NUM: 
            case T_BOOL:
            case T_UINT:
            case T_STR:
            case T_FLOAT:
            case T_LIST:
            case T_TUPLE:
                if (op != O_LXOR) { 
                    if (val_truth(v1->val) != (op == O_LOR)) {
                        val_replace(&v1->val, v2->val);
                        v1->epoint = v2->epoint;
                    }
                    continue;
                }
                t2 = try_resolv(&v2->val);
                switch (t2) {
                case T_SINT:
                case T_NUM: 
                case T_BOOL:
                case T_UINT:
                case T_STR:
                case T_FLOAT:
                case T_LIST:
                case T_TUPLE:
                case T_NONE:
                    if (t2 == T_NONE) { val_replace(&v1->val, &none_value); continue;}
                    if (val_truth(v1->val)) {
                        if (val_truth(v2->val)) val_replace(&v1->val, &false_value);
                    } else {
                        val_replace(&v1->val, val_truth(v2->val) ? v2->val : &false_value);
                    }
                    continue;
                default: err_msg_invalid_oper(op, v1->val, v2->val, v2->epoint); 
                         goto errtype;
                }
            default: err_msg_invalid_oper(op, v1->val, v2->val, v1->epoint); 
                     goto errtype;
            case T_NONE: continue;
            }
        default: break;
        }
        v2 = v1; v1 = &values[--vsp-1];
        if (vsp == 0) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            outp2 = outp;
            return -1;
        }
        try_resolv_ident(&v2->val);
        try_resolv_ident(&v1->val);

        {
            struct value_s *tmp = apply_op2(op, v1->val, v2->val, v1->epoint, v2->epoint, o_out[i].epoint, &large);
            val_replace(&v1->val, tmp);
            val_destroy(tmp);
        }
    }
    outp2 = i;
    if (large) return -1;
    return 0;
}

int get_exp(int *wd, int stop) {// length in bytes, defined
    int cd;
    char ch;

    enum oper_e o_oper[256] = {0}, op;
    unsigned int epoints[256];
    uint8_t operp = 0, prec, db;
    int large=0;
    int cond=0;
    unsigned int epoint;

    gstop = stop;
    outp2 = outp = 0;

    if (arguments.tasmcomp) {
        return get_exp_compat(wd, stop);
    }

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    cd=0;    // 0=error, 1=ok, 2=(a, 3=(), 4=[]

    ignore();
    switch (here()) {
    case '@':
        switch (pline[++lpoint] | 0x20) {
        case 'b':*wd=0;break;
        case 'w':*wd=1;break;
        case 'l':*wd=2;break;
        default:err_msg(ERROR______EXPECTED,"@B or @W or @L"); return 0;
        }
        lpoint++;
        break;
    }
    for (;;) {
        ignore();ch = here(); epoint = lpoint;
        switch (ch) {
        case ')':
            if (operp) {
                if (o_oper[operp-1] == O_COMMA) {operp--;op = O_TUPLE;goto tphack;}
                else if (o_oper[operp-1] == O_PARENT || o_oper[operp-1] == O_FUNC) goto other;
            }
            goto tryanon;
        case ']':
            if (operp) { 
                if (o_oper[operp-1] == O_COMMA) {operp--;goto other;}
                else if (o_oper[operp-1] == O_BRACKET) goto other;
            }
            goto tryanon;
        case ':':
            if (operp && o_oper[operp-1] == O_INDEX) {
                o_out[outp].val.type = T_DEFAULT;
                goto pushval;
            } else if (operp > 1 && o_oper[operp-1] == O_COMMA && o_oper[operp-2] == O_SLICE) {
                o_out[outp].val.type = T_DEFAULT;
                goto pushval;
            }
            goto syntaxe;
        case '(': 
            epoints[operp]=epoint;
            o_oper[operp++] = O_PARENT; lpoint++;
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = O_PARENT;
            o_out[outp++].epoint = epoint;
            continue;
        case '[':
            epoints[operp]=epoint;
            o_oper[operp++] = O_BRACKET; lpoint++;
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = O_BRACKET;
            o_out[outp++].epoint = epoint;
            continue;
        case '+': op = O_POS; break;
        case '-': op = O_NEG; break;
        case '!': op = O_LNOT;break;
        case '~': op = O_INV; break;
        case '<': if (pline[lpoint+1] == '>') {lpoint++;op = O_WORD;} else op = O_LOWER; break;
        case '>': if (pline[lpoint+1] == '`') {lpoint++;op = O_HWORD;} else if (pline[lpoint+1] == '<') {lpoint++;op = O_BSWORD;} else op = O_HIGHER; break;
        case '`': op = O_BANK; break;
        case '^': op = O_STRING; break;
        case '$': lpoint++;if (get_hex(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '%': lpoint++;if (get_bin(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '"':
        case '\'': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
        case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
        case '?': lpoint++;o_out[outp].val.type = T_GAP;goto pushval;
        case '.': if ((uint8_t)(pline[lpoint+1] ^ 0x30) < 10) goto pushfloat;
        default: 
            if (ch>='0' && ch<='9') {
                if (get_dec(&o_out[outp].val)) {
                pushfloat:
                    lpoint = epoint;
                    if (get_float(&o_out[outp].val)) {
                    pushlarge:
                        err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);large=1;
                    }
                } else if (here() == '.' || (here() | 0x20) == 'e') goto pushfloat;
            pushval: 
                o_out[outp++].epoint=epoint;
                goto other;
            }
            while ((((ch=here()) | 0x20) >= 'a' && (ch | 0x20) <= 'z') || (ch>='0' && ch<='9') || ch=='_') lpoint++;
            if (epoint != lpoint) {
                o_out[outp].val.u.ident.name = pline + epoint;
                o_out[outp].val.u.ident.len = lpoint - epoint;
                o_out[outp].val.type = T_IDENT;
                o_out[outp++].epoint=epoint;
                goto other;
            }
        tryanon:
            db = operp;
            while (operp && o_oper[operp-1] == O_POS) operp--;
            if (db != operp) {
                o_out[outp].val.u.ref = db - operp;
                o_out[outp].val.type = T_FORWR;
                epoint = epoints[operp];
                goto pushval;
            }
            while (operp && o_oper[operp-1] == O_NEG) operp--;
            if (db != operp) {
                o_out[outp].val.u.ref = db - operp;
                o_out[outp].val.type = T_BACKR;
                epoint = epoints[operp];
                goto pushval;
            }
            goto syntaxe;
        }
        lpoint++;
        prec = priority(op);
        while (operp && prec < priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
        epoints[operp] = epoint;
        o_oper[operp++] = op;
        continue;
    other:
        ignore();ch = here(); epoint = lpoint;
        switch (ch) {
        case ',':
            if (stop) {
                while (operp && o_oper[operp-1] != O_PARENT) {
                    if (o_oper[operp-1]==O_BRACKET || o_oper[operp-1]==O_INDEX || o_oper[operp-1]==O_SLICE || o_oper[operp-1]==O_SLICE2) {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
                }
                if (!operp) break;
                if (operp==1 && o_oper[0]==O_PARENT && ((pline[lpoint+1] | 0x20)=='x' || (pline[lpoint+1] | 0x20)=='s' || (pline[lpoint+1] | 0x20)=='r') && pline[lpoint+2]==')') {
                    outp--;
                    memmove(&o_out[0], &o_out[1], outp * sizeof(o_out[0]));
                    break;
                }
            }
            op = O_COMMA; goto push2; 
        case '(': 
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = O_PARENT;
            o_out[outp++].epoint=epoint;
            epoints[operp]=epoint;
            o_oper[operp++] = O_FUNC; lpoint++;
            continue;
        case '[':
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = O_BRACKET;
            o_out[outp++].epoint=epoint;
            epoints[operp]=epoint;
            o_oper[operp++] = O_INDEX; lpoint++;
            continue;
        case '&': if (pline[lpoint+1] == '&') {lpoint++;op = O_LAND;} else op = O_AND; goto push2;
        case '|': if (pline[lpoint+1] == '|') {lpoint++;op = O_LOR;} else op = O_OR;goto push2;
        case '^': if (pline[lpoint+1] == '^') {lpoint++;op = O_LXOR;} else op = O_XOR; goto push2;
        case '*': if (pline[lpoint+1] == '*') {lpoint++;op = O_EXP;} else op = O_MUL; goto push2;
        case '%': op = O_MOD; goto push2;
        case '/': if (pline[lpoint+1] == '/') {lpoint++;op = O_MOD;} else op = O_DIV; goto push2;
        case '+': op = O_ADD; goto push2;
        case '-': op = O_SUB; goto push2;
        case '.': op = O_MEMBER; goto push2;
        case '?': op = O_COND; cond++; goto push2;
        case ':': op = O_COLON;
            prec = priority(op);
            while (operp && prec <= priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
            if (operp && o_oper[operp-1] == O_INDEX) { o_oper[operp-1] = O_SLICE; op = O_COMMA;}
            else if (operp && o_oper[operp-1] == O_SLICE) { o_oper[operp-1] = O_SLICE2; op = O_COMMA;}
            else {
                if (o_oper[operp] == O_COND) cond--;
                else {err_msg(ERROR______EXPECTED,"?");goto error;}
            }
            epoints[operp] = epoint;
            o_oper[operp++] = op;
            lpoint++;
            continue;
        case '=': op = O_EQ; if (pline[lpoint+1] == '=') lpoint++;
        push2:
            prec = priority(op);
            while (operp && prec <= priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
            if (ch == ',' && !operp) {
                o_out[outp].val.type = T_OPER;
                o_out[outp].val.u.oper=O_SEPARATOR;
                o_out[outp++].epoint=epoint;
            } else {
                epoints[operp] = epoint;
                o_oper[operp++] = op;
            }
            lpoint++;
            continue;
        case '<': 
            switch (pline[lpoint+1]) {
            case '>': lpoint++;op = O_NEQ; break;
            case '<': lpoint++;op = O_LSHIFT; break;
            case '=': lpoint++;op = O_LE; break;
            default: op = O_LT; break;
            }
            goto push2;
        case '>':
            switch (pline[lpoint+1]) {
            case '<': lpoint++;op = O_NEQ; break;
            case '>': lpoint++;if (pline[lpoint+1] == '>') {lpoint++;op = O_RSHIFT;} else op = O_ASHIFT; break;
            case '=': lpoint++;op = O_GE; break;
            default: op = O_GT; break;
            }
            goto push2;
        case '!':
            if (pline[lpoint+1]=='=') {lpoint++;op = O_NEQ;goto push2;}
            goto syntaxe;
        case ')':
            op = O_RPARENT;
        tphack:
            while (operp && o_oper[operp-1] != O_PARENT && o_oper[operp-1] != O_FUNC) {
                if (o_oper[operp-1]==O_BRACKET || o_oper[operp-1]==O_INDEX || o_oper[operp-1]==O_SLICE || o_oper[operp-1]==O_SLICE2) {err_msg(ERROR______EXPECTED,"("); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            lpoint++;
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            operp--;
            o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[operp];o_out[outp++].val.u.oper = (o_oper[operp] == O_PARENT)? op : o_oper[operp];
            goto other;
        case ']':
            while (operp && o_oper[operp-1] != O_BRACKET && o_oper[operp-1] != O_INDEX && o_oper[operp-1] != O_SLICE && o_oper[operp-1] != O_SLICE2) {
                if (o_oper[operp-1]==O_PARENT || o_oper[operp-1]==O_FUNC) {err_msg(ERROR______EXPECTED,"["); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            lpoint++;
            if (!operp) {err_msg(ERROR______EXPECTED,"["); goto error;}
            operp--;
            o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[operp];o_out[outp++].val.u.oper=(o_oper[operp] == O_BRACKET)? O_RBRACKET : o_oper[operp];
            goto other;
        case 0:
        case ';': break;
        default: goto syntaxe;
        }
        if (cond) {err_msg(ERROR______EXPECTED,":"); goto error;}
        if (stop && o_oper[0]==O_PARENT) {
            if (!operp) {cd=3;break;}
            if (ch == ',') {
                while (operp && o_oper[operp-1] != O_PARENT) {
                    if (o_oper[operp-1]==O_BRACKET || o_oper[operp-1]==O_INDEX || o_oper[operp-1]==O_SLICE || o_oper[operp-1]==O_SLICE2) {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
                }
                if (operp==1) {cd=2; break;}
            }
            err_msg(ERROR______EXPECTED,")"); goto error;
        } else if (stop && o_oper[0]==O_BRACKET) {
            if (!operp) {cd=4;break;}
            err_msg(ERROR______EXPECTED,"]"); goto error;
        } else {
            while (operp) {
                if (o_oper[operp-1] == O_PARENT || o_oper[operp-1] == O_FUNC) {err_msg(ERROR______EXPECTED,")"); goto error;}
                if (o_oper[operp-1] == O_BRACKET || o_oper[operp-1] == O_INDEX || o_oper[operp-1]==O_SLICE || o_oper[operp-1]==O_SLICE2) {err_msg(ERROR______EXPECTED,"]"); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            if (!operp) {cd=1;break;}
        }
    syntaxe:
        err_msg(ERROR_EXPRES_SYNTAX,NULL);
    error:
        return 0;
    }
    if (large) cd=0;
    return cd;
}

void free_values(void) {
    while (values_size--) if (values[values_size].val) val_destroy(values[values_size].val);
    free(values);
}
