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
#include "mem.h"
#include "isnprintf.h"
#include "macro.h"
#include "values.h"
#include "variables.h"
#include "misc.h"

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

static struct value_s new_value = {T_NONE, 0, {{0, 0}}};
static struct value_s none_value = {T_NONE, 0, {{0, 0}}};
static struct value_s true_value = {T_BOOL, 0, {{1, 1}}};
static struct value_s false_value = {T_BOOL, 0, {{1, 0}}};
static struct value_s null_str = {T_STR, 0, {{0, 0}}};
static struct value_s null_tuple = {T_TUPLE, 0, {{0, 0}}};
static struct value_s null_list = {T_LIST, 0, {{0, 0}}};
struct value_s error_value = {T_NONE, 0, {{0, 0}}};

struct encoding_s *actual_encoding;
int referenceit = 1;

static uint8_t get_val_len(uval_t val, enum type_e type) {
    int span, bits;

    switch (type) {
    case T_SINT:
        span = 4 * sizeof(uval_t); bits = 0;
        if ((ival_t)val < 0) val = ~val;
        while (span) {
            if (val >> (bits + span)) {
                bits |= span;
            }
            span >>= 1;
        }
        return bits + 2;
    case T_UINT:
        span = 4 * sizeof(uval_t); bits = 0;
        while (span) {
            if (val >> (bits + span)) {
                bits |= span;
            }
            span >>= 1;
        }
        return bits + 1;
    case T_BOOL:
        return 1;
    default:
        return 8*sizeof(uval_t);
    }
}

inline uint8_t get_val_len2(const struct value_s *v) {
    return (v->type == T_NUM) ? v->u.num.len : get_val_len((uval_t)v->u.num.val, v->type);
}

size_t get_label(void) {
    uint8_t ch;
    linepos_t e;
    if (here()>='0' && here()<='9') return 0;
    e.pos = lpoint.pos;
    while ((((ch=here()) | 0x20) >= 'a' && (ch | 0x20) <= 'z') || (ch>='0' && ch<='9') || ch=='_') lpoint.pos++;
    return lpoint.pos - e.pos;
}

static void get_hex(struct value_s *v) {
    uval_t val = 0;
    linepos_t start;
    ignore();
    start = lpoint;
    while (here() == 0x30) lpoint.pos++;
    while ((here() ^ 0x30) < 10 || (uint8_t)((here() | 0x20) - 0x61) < 6 ) {
        val = (val << 4) + (here() & 15);
        if (here() & 0x40) val += 9;
        lpoint.pos++;
    }
    v->type = T_NUM;
    v->u.num.val = val;
    v->u.num.len = (lpoint.pos - start.pos) * 4;
    if (v->u.num.len > 8*sizeof(val)) {
        v->type = T_ERROR;
        v->u.error.num = ERROR_CONSTNT_LARGE;
        v->u.error.epoint = start;
    }
    return;
}

static void get_bin(struct value_s *v) {
    uval_t val = 0;
    linepos_t start;
    ignore();
    start = lpoint;
    while (here() == 0x30) lpoint.pos++;
    while ((here() & 0xfe) == '0') {
        val = (val << 1) | (here() & 1);
        lpoint.pos++;
    }
    v->type = T_NUM;
    v->u.num.val = val;
    v->u.num.len = lpoint.pos - start.pos;
    if (v->u.num.len > 8*sizeof(val)) {
        v->type = T_ERROR;
        v->u.error.num = ERROR_CONSTNT_LARGE;
        v->u.error.epoint = start;
    }
    return;
}

static int get_dec(struct value_s *v) {
    uval_t val = 0;
    int large = 0;
    while (here() == '0') lpoint.pos++;
    while ((uint8_t)(here() ^ '0') < 10) {
        if (val >= ((uval_t)1 << (8 * sizeof(val) - 1)) / 5) {
            if (val == ((uval_t)1 << (8 * sizeof(val) - 1)) / 5) {
               if ((uval_t)(here() & 15) > (((uval_t)1 << (8 * sizeof(val) - 1)) % 5) * 2) large = 1;
            } else large = 1;
        }
        val=(val * 10) + (here() & 15);
        lpoint.pos++;
    }
    v->type = T_UINT;
    v->u.num.val = val;
    return large;
}

static void get_float(struct value_s *v) {
    linepos_t start = lpoint;

    while ((uint8_t)(here() ^ '0') < 10) lpoint.pos++;
    if (here()=='.') {
        do {
            lpoint.pos++;
        } while ((uint8_t)(here() ^ '0') < 10);
    }
    if ((here() | 0x20)=='e') {
        if ((pline[lpoint.pos + 1]=='-' || pline[lpoint.pos + 1]=='+') && (uint8_t)(pline[lpoint.pos + 2] ^ '0') < 10) lpoint.pos++;
        if ((uint8_t)(pline[lpoint.pos + 1] ^ '0') < 10) {
            do {
                lpoint.pos++;
            } while ((uint8_t)(here() ^ '0') < 10);
        }
    }
    v->u.real = strtod((const char *)pline + start.pos, NULL);
    v->type = T_FLOAT;
    if (v->u.real == HUGE_VAL) {
        v->type = T_ERROR;
        v->u.error.num = ERROR_CONSTNT_LARGE;
        v->u.error.epoint = start;
    }
    return;
}

uint_fast16_t petascii(size_t *i, const struct value_s *v) {
    uint32_t ch, rc2;
    const uint8_t *text = v->u.str.data + *i;
    uint16_t rc;

    rc2 = find_escape(text, v->u.str.data + v->u.str.len, actual_encoding);
    if (rc2) {
        *i = (rc2 >> 8) + text - v->u.str.data;
        return rc2 & 0xff;
    }
    ch = text[0];
    if (ch & 0x80) (*i) += utf8in(text, &ch); else (*i)++;
    rc = find_trans(ch, actual_encoding);
    if (rc < 256) return rc;
    err_msg(ERROR___UNKNOWN_CHR, &ch);
    ch = 0;
    return ch;
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b < a * 0.0000000005;
    return b - a < b * 0.0000000005;
}

int str_to_num(const struct value_s *v1, enum type_e type, struct value_s *v, linepos_t epoint) {
    uint16_t ch;
    unsigned int large = 0;
    size_t i = 0;
    uval_t val = 0;

    if (actual_encoding) {
        while (v1->u.str.len > i) {
            if (large >= (arguments.tasmcomp ? 8 : (8 * sizeof(val)))) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_BIG_STRING_CO;
                v->u.error.epoint = epoint;
                return 1;
            }

            ch = petascii(&i, v1);
            if (ch > 255) {
                v->type = T_NONE;return 1;
            }

            val |= (uint8_t)ch << large;
            large += 8;
        }
    } else if (v1->u.str.len) {
        uint32_t ch2;

        ch2 = v1->u.str.data[0];
        if (ch2 & 0x80) i = utf8in(v1->u.str.data, &ch2); else i=1;
        val = ch2;
        large = 32;
    }
    v->type = type;
    v->u.num.val = val;
    v->u.num.len = large;
    return 0;
}

static inline int utf8len(uint8_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}

static void get_string(struct value_s *v, uint8_t ch) {
    size_t i2 = 0;
    size_t i;
    unsigned int u;
    size_t r = 0;
    uint8_t ch2;

    i = lpoint.pos;
    for (;;) {
        if (!(ch2 = here())) {err_msg(ERROR______EXPECTED,"End of string"); lpoint.pos++; break;}
        if (ch2 & 0x80) {
            u = utf8len(ch2); 
            lpoint.pos += u; lpoint.upos += u - 1;
        } else lpoint.pos++;
        if (ch2 == ch) {
            if (here() == ch && !arguments.tasmcomp) {lpoint.pos++;r++;} /* handle 'it''s' */
            else break; /* end of string; */
        }
        i2++;
    }
    if (r) {
        const uint8_t *p = (const uint8_t *)pline + i, *e, *p2;
        uint8_t *d;
        v->type = T_STR;
        v->u.str.len = lpoint.pos - i - 1 - r;
        v->u.str.chars = i2;
        d = malloc(v->u.str.len);
        if (!d) err_msg_out_of_memory();
        v->u.str.data = d;
        e = pline + lpoint.pos - 1;
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
        uint8_t *d;
        v->type = T_STR;
        v->u.str.len = lpoint.pos - i - 1;
        v->u.str.chars = i2;
        d = malloc(v->u.str.len);
        if (!d) err_msg_out_of_memory();
        v->u.str.data = d;
        memcpy(d, pline + i, v->u.str.len);
    }
    return;
}

static int touch_label(struct label_s *tmp) {
    if (referenceit) tmp->ref = 1;tmp->pass=pass;
    if (tmp->type != L_VAR || tmp->upass==pass) return 0;
    return 1;
}

static void try_resolv_ident(struct values_s *vals) {
    char idents[100];
    str_t ident;
    struct value_s *val = vals->val, *v;
    struct label_s *l;

    switch (val->type) {
    case T_FORWR: 
        sprintf(idents,"+%x+%x", reffile, forwr + val->u.ref - 1);
        ident.data = (const uint8_t *)idents;
        ident.len = strlen(idents);
        l = find_label(&ident);
        v = val_realloc(&vals->val);
        if (l) {
            v->type = T_IDENTREF;
            v->u.identref = l;
        } else {
            v->type = T_ERROR;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = vals->epoint;
            v->u.error.u.ident.len = 1;
            v->u.error.u.ident.data = (const uint8_t *)"+";
        }
        break;
    case T_BACKR: 
        sprintf(idents,"-%x-%x", reffile, backr - val->u.ref);
        ident.data = (const uint8_t *)idents;
        ident.len = strlen(idents);
        l = find_label(&ident);
        v = val_realloc(&vals->val);
        if (l) {
            v->type = T_IDENTREF;
            v->u.identref = l;
        } else {
            v->type = T_ERROR;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = vals->epoint;
            v->u.error.u.ident.len = 1;
            v->u.error.u.ident.data = (const uint8_t *)"-";
        }
        break;
    case T_IDENT: 
        l = find_label(&val->u.ident);
        v = val_realloc(&vals->val);
        if (l) {
            v->type = T_IDENTREF;
            v->u.identref = l;
        } else {
            v->u.error.u.ident = val->u.ident;
            v->type = T_ERROR;
            v->u.error.epoint = vals->epoint;
            v->u.error.num = ERROR___NOT_DEFINED;
        }
        break;
    default: return;
    }
}

static struct value_s *unwind_identrefs(struct values_s *vals, struct value_s *v) {
    struct value_s *vold = vals->val;
    struct value_s *v1 = vold;
    int rec = 100;
    while (v1->type == T_IDENTREF) {
        if (pass != 1 && v1->u.identref->value->type != T_IDENTREF) {
            if (v1->u.identref->requires & ~current_section->provides) {
                v->u.error.u.ident = vold->u.identref->name;
                v->type = T_ERROR;
                v->u.error.epoint = vals->epoint;
                v->u.error.num = ERROR_REQUIREMENTS_;
                return v;
            }
            if (v1->u.identref->conflicts & current_section->provides) {
                v->u.error.u.ident = vold->u.identref->name;
                v->type = T_ERROR;
                v->u.error.epoint = vals->epoint;
                v->u.error.num = ERROR______CONFLICT;
                return v;
            }
        }
        if (touch_label(v1->u.identref)) {
            v->u.error.u.ident = vold->u.identref->name;
            v->type = T_ERROR;
            v->u.error.epoint = vals->epoint;
            v->u.error.num = ERROR___NOT_DEFINED;
            return v;
        }
        v1 = v1->u.identref->value;
        if (!rec--) {
            err_msg2(ERROR__REFRECURSION, NULL, vals->epoint);
            v->type = T_NONE;
            return v1;
        }
    } 
    return v1;
}

static void try_resolv_identref(struct values_s *vals) {
    struct value_s *val = vals->val, tmp;
    val = unwind_identrefs(vals, &tmp);
    if (val == &tmp) val_replace_template(&vals->val, val);
    else val_replace(&vals->val, val);
}

static enum type_e try_resolv(struct values_s *vals) {
    try_resolv_ident(vals);
    try_resolv_identref(vals);
    return vals->val->type;
}

static void get_star(struct value_s *v) {
    struct star_s *tmp;
    int labelexists;

    tmp=new_star(vline, &labelexists);
    if (labelexists && tmp->addr != star) {
        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, lpoint);
        fixeddig=0;
    }
    tmp->addr=star;
    v->type = T_CODE;
    v->u.code.addr = star;
    v->u.code.size = 0;
    v->u.code.dtype = D_NONE;
    v->u.code.memp = ~(size_t)0;
    v->u.code.membp = ~(size_t)0;
}

static struct value_s o_TUPLE = { T_OPER, 0, .u.oper = {O_TUPLE, 0}};
static struct value_s o_LIST = { T_OPER, 0, .u.oper = {O_LIST, 0}};
static struct value_s o_RPARENT = { T_OPER, 0, .u.oper = {O_RPARENT, 0}};
static struct value_s o_RBRACKET = { T_OPER, 0, .u.oper = {O_RBRACKET, 0}};
static struct value_s o_FUNC = { T_OPER, 0, .u.oper = {O_FUNC, 0}};
static struct value_s o_INDEX = { T_OPER, 0, .u.oper = {O_INDEX, 0}};
static struct value_s o_SLICE = { T_OPER, 0, .u.oper = {O_SLICE, 0}};
static struct value_s o_SLICE2 = { T_OPER, 0, .u.oper = {O_SLICE2, 0}};
static struct value_s o_BRACKET = { T_OPER, 0, .u.oper = {O_BRACKET, 0}};
static struct value_s o_PARENT = { T_OPER, 0, .u.oper = {O_PARENT, 0}};
static struct value_s o_SEPARATOR = { T_OPER, 0, .u.oper = {O_SEPARATOR, 1}};
static struct value_s o_COMMA = { T_OPER, 0, .u.oper = {O_COMMA, 1}};
static struct value_s o_QUEST = { T_OPER, 0, .u.oper = {O_QUEST, 2}};
static struct value_s o_COLON = { T_OPER, 0, .u.oper = {O_COLON, 2}};
static struct value_s o_COND = { T_OPER, 0, .u.oper = {O_COND, 3}};
static struct value_s o_COLON2 = { T_OPER, 0, .u.oper = {O_COLON2, 3}};
static struct value_s o_COLON3 = { T_OPER, 0, .u.oper = {O_COLON3, 3}};
static struct value_s o_HASH = { T_OPER, 0, .u.oper = {O_HASH, 3}};
static struct value_s o_COMMAX = { T_OPER, 0, .u.oper = {O_COMMAX, 4}};
static struct value_s o_COMMAY = { T_OPER, 0, .u.oper = {O_COMMAY, 4}};
static struct value_s o_COMMAZ = { T_OPER, 0, .u.oper = {O_COMMAZ, 4}};
static struct value_s o_COMMAS = { T_OPER, 0, .u.oper = {O_COMMAS, 4}};
static struct value_s o_COMMAR = { T_OPER, 0, .u.oper = {O_COMMAR, 4}};
static struct value_s o_WORD = { T_OPER, 0, .u.oper = {O_WORD, 4}};
static struct value_s o_HWORD = { T_OPER, 0, .u.oper = {O_HWORD, 4}};
static struct value_s o_BSWORD = { T_OPER, 0, .u.oper = {O_BSWORD, 4}};
static struct value_s o_LOWER = { T_OPER, 0, .u.oper = {O_LOWER, 4}};
static struct value_s o_HIGHER = { T_OPER, 0, .u.oper = {O_HIGHER, 4}};
static struct value_s o_BANK = { T_OPER, 0, .u.oper = {O_BANK, 4}};
static struct value_s o_STRING = { T_OPER, 0, .u.oper = {O_STRING, 4}};
static struct value_s o_LOR = { T_OPER, 0, .u.oper = {O_LOR, 5}};
static struct value_s o_LXOR = { T_OPER, 0, .u.oper = {O_LXOR, 6}};
static struct value_s o_LAND = { T_OPER, 0, .u.oper = {O_LAND, 7}};
static struct value_s o_IN = { T_OPER, 0, .u.oper = {O_IN, 8}};
static struct value_s o_EQ = { T_OPER, 0, .u.oper = {O_EQ, 8}};
static struct value_s o_NEQ = { T_OPER, 0, .u.oper = {O_NEQ, 8}};
static struct value_s o_LT = { T_OPER, 0, .u.oper = {O_LT, 8}};
static struct value_s o_GT = { T_OPER, 0, .u.oper = {O_GT, 8}};
static struct value_s o_GE = { T_OPER, 0, .u.oper = {O_GE, 8}};
static struct value_s o_LE = { T_OPER, 0, .u.oper = {O_LE, 8}};
static struct value_s o_OR = { T_OPER, 0, .u.oper = {O_OR, 9}};
static struct value_s o_XOR = { T_OPER, 0, .u.oper = {O_XOR, 10}};
static struct value_s o_AND = { T_OPER, 0, .u.oper = {O_AND, 11}};
static struct value_s o_LSHIFT = { T_OPER, 0, .u.oper = {O_LSHIFT, 12}};
static struct value_s o_ASHIFT = { T_OPER, 0, .u.oper = {O_ASHIFT, 12}};
static struct value_s o_RSHIFT = { T_OPER, 0, .u.oper = {O_RSHIFT, 12}};
static struct value_s o_ADD = { T_OPER, 0, .u.oper = {O_ADD, 13}};
static struct value_s o_SUB = { T_OPER, 0, .u.oper = {O_SUB, 13}};
static struct value_s o_MUL = { T_OPER, 0, .u.oper = {O_MUL, 14}};
static struct value_s o_DIV = { T_OPER, 0, .u.oper = {O_DIV, 14}};
static struct value_s o_MOD = { T_OPER, 0, .u.oper = {O_MOD, 14}};
static struct value_s o_EXP = { T_OPER, 0, .u.oper = {O_EXP, 15}};
static struct value_s o_NEG = { T_OPER, 0, .u.oper = {O_NEG, 16}};
static struct value_s o_POS = { T_OPER, 0, .u.oper = {O_POS, 16}};
static struct value_s o_INV = { T_OPER, 0, .u.oper = {O_INV, 16}};
static struct value_s o_LNOT = { T_OPER, 0, .u.oper = {O_LNOT, 16}};
static struct value_s o_CONCAT = { T_OPER, 0, .u.oper = {O_CONCAT, 17}};
static struct value_s o_X = { T_OPER, 0, .u.oper = {O_X, 18}};
static struct value_s o_MEMBER = { T_OPER, 0, .u.oper = {O_MEMBER, 19}};

static int evxnum, evx_p;
static struct eval_context_s {
    struct values_s *values;
    size_t values_size;
    size_t outp, outp2;
    int gstop;
    struct values_s *o_out;
    size_t out_size;
} **evx;

static struct eval_context_s *eval;


static inline struct value_s *push(linepos_t epoint) {
    struct values_s *o_out;
    if (eval->outp >= eval->out_size) {
        size_t i;
        eval->out_size += 64;
        eval->o_out = realloc(eval->o_out, eval->out_size * sizeof(eval->o_out[0]));
        for (i = eval->outp; i < eval->out_size; i++) eval->o_out[i].val = &none_value;
    }
    o_out = &eval->o_out[eval->outp++];
    o_out->epoint = epoint;
    return val_realloc(&o_out->val);
}

static inline void push_oper(struct value_s *val, linepos_t epoint) {
    if (eval->outp >= eval->out_size) {
        size_t i;
        eval->out_size += 64;
        eval->o_out = realloc(eval->o_out, eval->out_size * sizeof(eval->o_out[0]));
        for (i = eval->outp; i < eval->out_size; i++) eval->o_out[i].val = &none_value;
    } else val_destroy(eval->o_out[eval->outp].val);
    eval->o_out[eval->outp].val = val;
    eval->o_out[eval->outp++].epoint = epoint;
}

static int get_exp_compat(int *wd, int stop) {/* length in bytes, defined */
    int cd;
    char ch;

    struct value_s *o_oper[256], *conv, *conv2;
    linepos_t epoints[256];
    uint8_t operp = 0;
    int large=0;
    linepos_t epoint, cpoint = {0, 0};
    struct value_s *val;
    size_t llen;
    int first;

    *wd=3;    /* 0=byte 1=word 2=long 3=negative/too big */
    cd=0;     /* 0=error, 1=ok, 2=(a, 3=() */

    eval->outp = 0;
    o_oper[0] = &o_SEPARATOR;
rest:
    ignore();
    conv = conv2 = NULL;
    first = (here() == '(') && (stop == 3 || stop == 4);
    if (!eval->outp && here() == '#') {
        conv2 = &o_HASH; lpoint.pos++;
    }
    switch (here()) {
    case 0:
    case ';': err_msg(ERROR_MISSING_ARGUM,NULL); return 0;
    case '!':*wd=1;lpoint.pos++;break;
    case '<': conv = &o_LOWER; cpoint = lpoint; lpoint.pos++;break; 
    case '>': conv = &o_HIGHER;cpoint = lpoint; lpoint.pos++;break; 
    }
    for (;;) {
        ignore();ch = here(); epoint=lpoint;

        switch (ch) {
        case '(': lpoint.pos++;epoints[operp] = epoint; o_oper[operp++] = &o_PARENT;continue;
        case '$': lpoint.pos++;val = push(epoint);get_hex(val);goto pushval;
        case '%': lpoint.pos++;val = push(epoint);get_bin(val);goto pushval;
        case '"': lpoint.pos++;val = push(epoint);get_string(val, ch);goto pushval;
        case '*': lpoint.pos++;val = push(epoint);get_star(val);goto pushval;
        }
        if (ch>='0' && ch<='9') {val = push(epoint); if (get_dec(val)) goto pushlarge;
        pushval:
            if (type_is_int(val->type) && (val->u.num.val & ~0xffff)) {
            pushlarge:
                err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);large=1;
                val->u.num.val = 0xffff;
            }
        } else {
            if (!get_label()) goto syntaxe;
            val = push(epoint);
            val->type = T_IDENT;
            val->u.ident.data = pline + epoint.pos;
            val->u.ident.len = lpoint.pos - epoint.pos;
        }
    other:
        if (stop != 2) ignore();
        ch = here(); epoint=lpoint;

        while (operp && o_oper[operp-1] != &o_PARENT) {
            operp--;
            push_oper(o_oper[operp], epoints[operp]);
        }
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = get_label();
            if (llen == 1) {
                switch (pline[epoint.pos + 1] | 0x20) {
                case 'x': epoints[operp] = epoint; o_oper[operp++] = &o_COMMAX;goto other;
                case 'y': epoints[operp] = epoint; o_oper[operp++] = &o_COMMAY;goto other;
                default: break;
                }
            }
            if (conv) push_oper(conv, cpoint);
            if (conv2) push_oper(conv2, cpoint);
            if (stop == 1 || stop == 4) {lpoint = epoint;break;}
            push_oper(&o_SEPARATOR, epoint);
            if (llen) {
                epoint.pos++;
                val = push(epoint);
                val->type = T_IDENT;
                val->u.ident.data = pline + epoint.pos;
                val->u.ident.len = lpoint.pos - epoint.pos;
                goto other;
            }
            goto rest;
        case '&': epoints[operp] = epoint; o_oper[operp++] = &o_AND; lpoint.pos++;continue;
        case '.': epoints[operp] = epoint; o_oper[operp++] = &o_OR; lpoint.pos++;continue;
        case ':': epoints[operp] = epoint; o_oper[operp++] = &o_XOR; lpoint.pos++;continue;
        case '*': epoints[operp] = epoint; o_oper[operp++] = &o_MUL; lpoint.pos++;continue;
        case '/': epoints[operp] = epoint; o_oper[operp++] = &o_DIV; lpoint.pos++;continue;
        case '+': epoints[operp] = epoint; o_oper[operp++] = &o_ADD; lpoint.pos++;continue;
        case '-': epoints[operp] = epoint; o_oper[operp++] = &o_SUB; lpoint.pos++;continue;
        case ')':
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            lpoint.pos++;
            operp--;
            if (first) {
                epoints[operp] = epoint; o_oper[operp++] = &o_TUPLE;
                first = 0;
            }
            goto other;
        case 0:
        case ';':
        case '\t':
        case ' ':
            if (conv) push_oper(conv, cpoint);
            if (conv2) push_oper(conv2, cpoint);
            break;
        default: goto syntaxe;
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

static ival_t to_ival(const struct value_s *val) {
    switch (val->type) {
    case T_FLOAT: return (ival_t)val->u.real;
    case T_SINT: return val->u.num.val;
    case T_CODE: return (ival_t)val->u.code.addr;
    case T_NUM:
    case T_BOOL:
    case T_UINT: return (ival_t)val->u.num.val;
    default:
         return 0;
    }
}

static int get_val2_compat(struct eval_context_s *ev) {/* length in bytes, defined */
    size_t vsp = 0;
    enum type_e t1, t2;
    enum oper_e op;
    size_t i;
    struct value_s tmp, *val;
    struct values_s *v1, *v2;
    struct values_s *o_out;
    struct values_s *values;

    if (ev->outp2 >= ev->outp) return 1;
    values = ev->values;

    for (i = ev->outp2; i < ev->outp; i++) {
        o_out = &ev->o_out[i];
        val = o_out->val;
        if (val->type != T_OPER) {
            if (vsp >= ev->values_size) {
                size_t j = ev->values_size;
                ev->values_size += 16;
                ev->values = values = realloc(ev->values, sizeof(struct values_s)*ev->values_size);
                if (!values) err_msg_out_of_memory();
                for (; j < ev->values_size; j++) ev->values[j].val = &none_value;
            }
            val_destroy(values[vsp].val);
            values[vsp].val = val;
            o_out->val = &none_value;
            values[vsp++].epoint = o_out->epoint;
            continue;
        }
        op = val->u.oper.op;

        if (op == O_SEPARATOR) {
            ev->outp2 = i + 1;
            return 0;
        }
        if (vsp < 1) goto syntaxe;
        v1 = &values[vsp-1];
        t1 = try_resolv(v1);
        if (t1 == T_STR) {
            str_to_num(v1->val, T_NUM, &tmp, v1->epoint);
            val_replace_template(&v1->val, &tmp);
            t1 = v1->val->type;
        }
        switch (op) {
        case O_LOWER:
        case O_HIGHER:
        case O_HASH:
        case O_COMMAX:
        case O_COMMAY:
        case O_TUPLE:
            switch (t1) {
            case T_ADDRESS:
                switch (op) {
                case O_COMMAX:
                case O_COMMAY:
                case O_TUPLE:
                    new_value.type = T_ADDRESS;
                    new_value.u.addr.type = v1->val->u.addr.type << 4;
                    new_value.u.addr.type |= (op == O_TUPLE) ? A_I : (op == O_COMMAX) ? A_XR : A_YR;
                    new_value.u.addr.val = v1->val->u.addr.val;
                    new_value.u.addr.len = v1->val->u.addr.len;
                    val_replace(&v1->val, &new_value);
                    v1->epoint = o_out->epoint;
                    continue;
                default:break;
                }
                err_msg_invalid_oper(op, v1->val, NULL, o_out->epoint);
                val_replace(&v1->val, &none_value); 
                break;
            case T_CODE:
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
                {
                    uint16_t val1 = to_ival(v1->val);

                    switch (op) {
                    case O_HASH:
                    case O_COMMAX:
                    case O_COMMAY:
                        new_value.type = T_ADDRESS;
                        new_value.u.addr.type = (op == O_HASH) ? A_IMMEDIATE : (op == O_COMMAX) ? A_XR : A_YR;
                        new_value.u.addr.val = val1;
                        new_value.u.addr.len = 16;
                        val_replace(&v1->val, &new_value);
                        v1->epoint = o_out->epoint;
                        continue;
                    case O_HIGHER: val1 >>= 8;
                    case O_LOWER: val1 = (uint8_t)val1;break;
                    case O_TUPLE:
                        new_value.type = T_ADDRESS;
                        new_value.u.addr.type = A_I;
                        new_value.u.addr.val = val1;
                        new_value.u.addr.len = 16;
                        val_replace(&v1->val, &new_value);
                        v1->epoint = o_out->epoint;
                        continue;
                    default: break;
                    }
                    new_value.type = T_NUM;
                    new_value.u.num.val = val1;
                    new_value.u.num.len = 8;
                    val_replace(&v1->val, &new_value);
                    break;
                }
            default:
                err_msg_invalid_oper(op, v1->val, NULL, o_out->epoint);
                val_replace(&v1->val, &none_value); 
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
            return -1;
        }
        v2 = &values[vsp-2];
        t2 = try_resolv(v2);
        if (t2 == T_STR) {
            str_to_num(v2->val, T_NUM, &tmp, v2->epoint);
            val_replace_template(&v2->val, &tmp); 
            t2 = v2->val->type;
        }
        switch (t1) {
        case T_SINT:
        case T_UINT:
        case T_CODE:
        case T_NUM:
        case T_BOOL:
            switch (t2) {
            case T_UINT:
            case T_SINT:
            case T_CODE:
            case T_NUM:
            case T_BOOL:
                {
                    uint16_t val1 = to_ival(v1->val);
                    uint16_t val2 = to_ival(v2->val);

                    switch (op) {
                    case O_MUL: val1 *= val2; break;
                    case O_DIV: if (!val1) {
                        new_value.type = T_ERROR;
                        new_value.u.error.num = ERROR_DIVISION_BY_Z;
                        new_value.u.error.epoint = v1->epoint;
                        val_replace_template(&v2->val, &new_value);
                        continue;
                    } else val1=val2 / val1; break;
                    case O_ADD: val1 += val2; break;
                    case O_SUB: val1 = val2 - val1; break;
                    case O_AND: val1 &= val2; break;
                    case O_OR:  val1 |= val2; break;
                    case O_XOR: val1 ^= val2; break;
                    default: break;
                    }
                    vsp--;
                    new_value.type = T_NUM;
                    new_value.u.num.val = val1;
                    new_value.u.num.len = 16;
                    val_replace_template(&v2->val, &new_value);
                    continue;
                }
            default: err_msg_invalid_oper(op, v2->val, v1->val, o_out->epoint);
            case T_NONE:break;
            }
            break;
        default:
            err_msg_invalid_oper(op, v2->val, v1->val, o_out->epoint);
        case T_NONE:break;
        }
        vsp--; val_replace(&v2->val, &none_value); continue;
    }
    ev->outp2 = i;
    return 0;
}

int eval_finish(void) {
    if (eval->outp2 < eval->outp) {
        lpoint = eval->o_out[eval->outp2].epoint;
        eval->outp2 = eval->outp;
        return 1;
    }
    return 0;
}

static int get_val2(struct eval_context_s *);

struct value_s *get_val(enum type_e type, linepos_t *epoint) {/* length in bytes, defined */
    int res;
    struct values_s *value;

    if (arguments.tasmcomp) {
        res = get_val2_compat(eval);
    } else {
        res = get_val2(eval);
    }
    if (res) return (res > 0) ? NULL : &error_value;

    value = eval->values;

    if (epoint) *epoint = value->epoint;
    try_resolv_ident(value);

    switch (value->val->type) {
    case T_STR:
    case T_SINT:
    case T_UINT:
    case T_CODE:
    case T_NUM:
    case T_BOOL:
    case T_FLOAT:
    case T_GAP:
    case T_LIST:
    case T_TUPLE:
    case T_MACRO:
    case T_SEGMENT:
    case T_UNION:
    case T_STRUCT:
    case T_FUNCTION:
    case T_ADDRESS:
    case T_IDENTREF:
        if (type == T_IDENTREF) return value->val;
        try_resolv_identref(value);
        if (type == T_NONE) return value->val;
        if (type == T_ADDRESS) {
            if (value->val->type == T_ADDRESS) return value->val;
            type = T_UINT;
        }
        if (type_is_int(type) || type == T_GAP) {
            switch (value->val->type) {
            case T_STR:
                str_to_num(value->val, (type == T_GAP) ? T_NUM : type, &new_value, value->epoint);
                val_replace_template(&value->val, &new_value);
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
            case T_NONE:
                return value->val;
            case T_CODE:
                new_value.type = T_UINT;
                new_value.u.num.val = value->val->u.code.addr;
                val_replace(&value->val, &new_value);
                return value->val;
            case T_FLOAT:
                new_value.type = (type == T_SINT) ? T_SINT : T_UINT;
                new_value.u.num.val = (ival_t)value->val->u.real;
                val_replace(&value->val, &new_value);
                return value->val;
            case T_GAP: if (type == T_GAP) return value->val;
            default:
                break;
            }
        }
    default:
        err_msg_wrong_type(value->val, value->epoint);
        return &error_value;
    case T_NONE: break;
    }
    return &none_value;
}

static double to_float(const struct value_s *val) {
    switch (val->type) {
    case T_FLOAT: return val->u.real;
    case T_SINT: return (ival_t)val->u.num.val;
    case T_CODE: return val->u.code.addr;
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
    F_COSH, F_SINH, F_TANH, F_HYPOT, F_ATAN2, F_POW, F_SIGN, F_ABS, F_FLOAT,
    F_INT
};

/* return templates only! */
static const struct value_s *apply_func(enum func_e func, struct value_s *v1, linepos_t epoint) {
    switch (func) {
    case F_SIGN:
        switch (v1->type) {
        case T_SINT:
            new_value.type = T_SINT;
            new_value.u.num.val = ((ival_t)v1->u.num.val > 0) - ((ival_t)v1->u.num.val < 0);
            return &new_value;
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            new_value.type = T_SINT;
            new_value.u.num.val = ((uval_t)v1->u.num.val) > 0;
            return &new_value;
        case T_CODE:
            new_value.type = T_SINT;
            new_value.u.num.val = v1->u.code.addr > 0;
            return &new_value;
        case T_FLOAT:
            new_value.type = T_SINT;
            new_value.u.num.val = (v1->u.real > 0.0) - (v1->u.real < 0.0);
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
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            new_value.type = v1->type;
            new_value.u.num.val = (v1->type == T_SINT && v1->u.num.val < 0) ? -v1->u.num.val : v1->u.num.val;
            new_value.u.num.len = v1->u.num.len;
            return &new_value;
        case T_CODE:
            new_value.type = T_UINT;
            new_value.u.num.val = v1->u.code.addr;
            return &new_value;
        case T_FLOAT:
            new_value.type = v1->type;
            new_value.u.real = (v1->u.real < 0.0) ? -v1->u.real : v1->u.real;
            return &new_value;
        case T_LIST:
        case T_TUPLE: break;
        default: err_msg_wrong_type(v1, epoint);
        case T_NONE: return &none_value;
        }
        break;
    case F_INT:
        switch (v1->type) {
        case T_UINT:
        case T_NUM:
        case T_BOOL:
            new_value.type = T_UINT;
            new_value.u.num.val = v1->u.num.val;
            return &new_value;
        case T_CODE:
            new_value.type = T_UINT;
            new_value.u.num.val = v1->u.code.addr;
            return &new_value;
        case T_SINT:
            new_value.type = T_SINT;
            new_value.u.num.val = v1->u.num.val;
            return &new_value;
        case T_FLOAT:
            new_value.type = (v1->u.real < 0.0) ? T_SINT : T_UINT;
            new_value.u.num.val = (ival_t)v1->u.real;
            return &new_value;
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
    case T_CODE:
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
        case F_FLOAT: break; /* nothing to do */
        default:break;
        }
        break;
    case T_LIST:
    case T_TUPLE:
            {
                size_t i = 0;
                struct value_s **vals;
                const struct value_s *val;
                if (v1->u.list.len) {
                    vals = malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (;i < v1->u.list.len; i++) {
                        val = apply_func(func, v1->u.list.data[i], epoint);
                        val_set_template(vals + i, val);
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
    size_t len;
    const uint8_t *name;
    enum func_e func = F_NONE;

    if (vals->val->type == T_IDENT) {
        len = vals->val->u.ident.len;
        name = vals->val->u.ident.data;

        /* len(a) - length of string in characters */
        if (len == 3 && !memcmp(name, "len", len)) {
            if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else
                switch (try_resolv(&v[0])) {
                case T_STR:
                    new_value.type = T_UINT;
                    new_value.u.num.val = v[0].val->u.str.chars;
                    val_replace(&vals->val, &new_value);
                    return;
                case T_LIST:
                case T_TUPLE:
                    new_value.type = T_UINT;
                    new_value.u.num.val = v[0].val->u.list.len;
                    val_replace(&vals->val, &new_value);
                    return;
                case T_CODE:
                    if (!v[0].val->u.code.pass) {
                        new_value.type = T_ERROR;
                        new_value.u.error.num = ERROR___NOT_DEFINED;
                        new_value.u.error.epoint = v[0].epoint;
                        new_value.u.error.u.ident.len = 6;
                        new_value.u.error.u.ident.data = (const uint8_t *)"<code>";
                        val_replace(&vals->val, &new_value);
                        return;
                    }
                    new_value.type = T_UINT;
                    new_value.u.num.val = (v[0].val->u.code.dtype < 0) ? -v[0].val->u.code.dtype : v[0].val->u.code.dtype;
                    new_value.u.num.val = v[0].val->u.code.size / (new_value.u.num.val + !new_value.u.num.val);
                    val_replace(&vals->val, &new_value);
                    return;
                case T_UINT:
                case T_SINT:
                case T_NUM:
                case T_BOOL:
                    new_value.type = T_UINT;
                    new_value.u.num.val = get_val_len2(v[0].val);
                    val_replace(&vals->val, &new_value);
                    return;
                default: err_msg_wrong_type(v[0].val, v[0].epoint);
                case T_NONE: break;
                }
            val_replace(&vals->val, &none_value);
            return;
        } /* range([start],end,[step]) */
        if (len == 5 && !memcmp(name, "range", len)) {
            ival_t start = 0, end, step = 1;
            size_t i = 0, len2;
            struct value_s **val;
            if (args < 1 || args > 3) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else {
                for (i = 0; i < args; i++) {
                    switch (try_resolv(&v[i])) {
                    case T_CODE:
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
                case 1: end = to_ival(v[0].val);break;
                case 3: step = to_ival(v[2].val);
                case 2: start = to_ival(v[0].val);
                        end = to_ival(v[1].val);break;
                }
                if (step == 0) {
                    new_value.type = T_ERROR;
                    new_value.u.error.num = ERROR_DIVISION_BY_Z;
                    new_value.u.error.epoint = v[2].epoint;
                    val_replace_template(&vals->val, &new_value); return;
                }
                if (step > 0) {
                    if (end < start) end = start;
                    len2 = (end - start + step - 1) / step;
                } else {
                    if (end > start) end = start;
                    len2 = (start - end - step - 1) / -step;
                }
                val = malloc(len2 * sizeof(new_value.u.list.data[0]));
                if (!val) err_msg_out_of_memory();
                i = 0;
                while ((end > start && step > 0) || (end < start && step < 0)) {
                    new_value.type = (start >= 0) ? T_UINT : T_SINT;
                    new_value.u.num.val = start;
                    val[i++] = val_reference(&new_value);
                    start += step;
                }
                new_value.type = T_LIST;
                new_value.u.list.len = len2;
                new_value.u.list.data = val;
                val_replace_template(&vals->val, &new_value);
                return;
            }
            val_replace(&vals->val, &none_value);
            return;
        } /* min(a, b, ...) - minimum value */
        if (len == 3 && !memcmp(name, "min", len)) {
            ival_t min = 0;
            if (args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
            else {
                int volt = 1, t = 1;
                while (args--) {
                    switch (try_resolv(&v[args])) {
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
                new_value.type = t ? T_SINT : T_UINT;
                new_value.u.num.val = min;
                val_replace(&vals->val, &new_value);
                return;
            }
            val_replace(&vals->val, &none_value);
            return;
        } /* max(a, b, ...) - maximum value */
        else if (len == 3 && !memcmp(name, "max", len)) {
            ival_t max = 0;
            if (args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
            else {
                int volt = 1, t = 1;
                while (args--) {
                    switch (try_resolv(&v[args])) {
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
                new_value.type = t ? T_SINT : T_UINT;
                new_value.u.num.val = max;
                val_replace(&vals->val, &new_value);
                return;
            }
            val_replace(&vals->val, &none_value);
            return;
        } /* size(a) - size of data structure at location */
        else if (len == 4 && !memcmp(name, "size", len)) {
            if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
            else {
                switch (try_resolv(&v[0])) {
                case T_CODE:
                    if (!v[0].val->u.code.pass) {
                        new_value.type = T_ERROR;
                        new_value.u.error.num = ERROR___NOT_DEFINED;
                        new_value.u.error.epoint = v[0].epoint;
                        new_value.u.error.u.ident.len = 6;
                        new_value.u.error.u.ident.data = (const uint8_t *)"<code>";
                        val_replace(&vals->val, &new_value);
                        return;
                    }
                    new_value.type = T_UINT;
                    new_value.u.num.val = v[0].val->u.code.size;
                    val_replace(&vals->val, &new_value);
                    return;
                case T_STRUCT:
                case T_UNION:
                    new_value.type = T_UINT;
                    new_value.u.num.val = v[0].val->u.macro.size;
                    val_replace(&vals->val, &new_value);
                    return;
                default: err_msg_wrong_type(v[0].val, v[0].epoint);
                case T_NONE: break;
                }
            }
            val_replace(&vals->val, &none_value);
            return;
        } else if (len == 4 && !memcmp(name, "repr", len)) {
            if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
            else {
                struct error_s user_error;
                switch (try_resolv(&v[0])) {
                case T_ADDRESS:
                case T_UINT:
                case T_SINT:
                case T_NUM:
                case T_BOOL:
                case T_FLOAT:
                case T_STR:
                case T_CODE:
                case T_TUPLE:
                case T_LIST:
                case T_GAP:
                    error_init(&user_error);
                    err_msg_variable(&user_error, v[0].val, 1);

                    new_value.type = T_STR;
                    new_value.u.str.len = user_error.len;
                    new_value.u.str.chars = user_error.chars;
                    new_value.u.str.data = realloc(user_error.data, user_error.len);
                    val_replace_template(&vals->val, &new_value);
                    return;
                default: err_msg_wrong_type(v[0].val, v[0].epoint);
                case T_NONE: break;
                }
            }
            val_replace(&vals->val, &none_value);
            return;
        } else
            switch (len) {
            case 3:
                if (!memcmp(name, "log", 3)) func = F_LOG; 
                else if (!memcmp(name, "exp", 3)) func = F_EXP; 
                else if (!memcmp(name, "sin", 3)) func = F_SIN; 
                else if (!memcmp(name, "cos", 3)) func = F_COS; 
                else if (!memcmp(name, "tan", 3)) func = F_TAN; 
                else if (!memcmp(name, "rad", 3)) func = F_RAD; 
                else if (!memcmp(name, "deg", 3)) func = F_DEG; 
                else if (!memcmp(name, "abs", 3)) func = F_ABS; 
                else if (!memcmp(name, "int", 3)) func = F_INT;
                break;
            case 4:
                if (!memcmp(name, "ceil", 4)) func = F_CEIL;
                else if (!memcmp(name, "frac", 4)) func = F_FRAC; 
                else if (!memcmp(name, "sqrt", 4)) func = F_SQRT; 
                else if (!memcmp(name, "acos", 4)) func = F_ACOS; 
                else if (!memcmp(name, "asin", 4)) func = F_ASIN; 
                else if (!memcmp(name, "atan", 4)) func = F_ATAN; 
                else if (!memcmp(name, "cbrt", 4)) func = F_CBRT; 
                else if (!memcmp(name, "cosh", 4)) func = F_COSH; 
                else if (!memcmp(name, "sinh", 4)) func = F_SINH; 
                else if (!memcmp(name, "tanh", 4)) func = F_TANH; 
                else if (!memcmp(name, "sign", 4)) func = F_SIGN; 
                break;
            case 5:
                if (!memcmp(name, "floor", 5)) func = F_FLOOR;
                else if (!memcmp(name, "round", 5)) func = F_ROUND;
                else if (!memcmp(name, "trunc", 5)) func = F_TRUNC; 
                else if (!memcmp(name, "log10", 5)) func = F_LOG10; 
                else if (!memcmp(name, "float", 5)) func = F_FLOAT; 
                break;
            }

        if (func != F_NONE) {
            const struct value_s *val;
            if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
            try_resolv(&v[0]);
            val = apply_func(func, v[0].val, v[0].epoint);
            val_replace_template(&vals->val, val);
            return;
        }
        func = F_NONE;
        if (len == 5 && !memcmp(name, "hypot", len)) func = F_HYPOT; else
            if (len == 5 && !memcmp(name, "atan2", len)) func = F_ATAN2; else
                if (len == 3 && !memcmp(name, "pow", len)) func = F_POW;
        if (func != F_NONE) {
            double val1, val2;
            if (args != 2) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else
                switch (try_resolv(&v[0])) {
                case T_SINT:
                case T_UINT:
                case T_CODE:
                case T_NUM:
                case T_BOOL:
                case T_FLOAT:
                    switch (try_resolv(&v[1])) {
                    case T_SINT:
                    case T_UINT:
                    case T_CODE:
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
                                      if (val2 < 0.0 && !val1) {
                                          new_value.type = T_ERROR;
                                          new_value.u.error.num = ERROR_DIVISION_BY_Z;
                                          new_value.u.error.epoint = v[1].epoint;
                                          val_replace_template(&vals->val, &new_value);
                                          return;
                                      }
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
    }
    switch (try_resolv(vals)) {
    case T_FUNCTION:
        {
            struct value_s *val;
            unsigned int i;
            if (args > vals->val->u.func.argc) {
                err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
                val_replace(&vals->val, &none_value);
                return;
            }
            for (i = 0; i < args; i++) try_resolv_ident(&v[i]);
            for (; i < vals->val->u.func.argc; i++) {
                if (!vals->val->u.func.param[i].init) {
                    err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
                    break;
                }
            }
            eval_enter();
            val = function_recurse(vals->val, v, args);
            eval_leave();
            val_replace(&vals->val, val);
            val_destroy(val);
        }
        break;
    default:
        err_msg_invalid_oper(O_FUNC, vals->val, NULL, vals->epoint);
        val_replace(&vals->val, &none_value);
    case T_NONE: break;
    }
}

static ival_t indexoffs(const struct value_s *v, size_t len) {
    switch (v->type) {
    case T_CODE:
        if ((uval_t)v->u.code.addr < len) return (ival_t)v->u.code.addr;
        return -1;
    case T_UINT:
    case T_SINT:
    case T_NUM:
    case T_BOOL: break;
    default: return -1;
    }

    if (v->type != T_SINT || v->u.num.val >= 0) {
        if ((uval_t)v->u.num.val < len) return v->u.num.val;
    } else {
        if ((uval_t)-v->u.num.val <= len) return len + v->u.num.val;
    }
    return -1;
}

static void str_iindex(struct values_s *vals, const struct value_s *list, linepos_t epoint) {
    struct value_s *val;
    uint8_t *p, *o;
    size_t len;
    ival_t offs;
    size_t i;

    val = vals->val;
    len = val->u.str.chars;
    new_value.u.str.chars = list->u.list.len;
    if (!new_value.u.str.chars) {
        val_replace(&vals->val, &null_str);return;
    }
    if (val->u.str.len == val->u.str.chars) {
        new_value.u.str.len = new_value.u.str.chars;
        o = p = malloc(new_value.u.str.len);
        if (!p) err_msg_out_of_memory();
        for (i = 0; i < list->u.list.len; i++) {
            offs = indexoffs(list->u.list.data[i], len);
            if (offs < 0) {
                free(o);
                new_value.type = T_ERROR;
                new_value.u.error.num = ERROR___INDEX_RANGE;
                new_value.u.error.epoint = epoint;
                val_replace_template(&vals->val, &new_value);
                return;
            }
            *p++ = val->u.str.data[offs];
        }
    }
    else {
        ival_t j, k;
        size_t m = val->u.str.len;
        const uint8_t *p2;
        o = p = malloc(m);
        if (!p) err_msg_out_of_memory();
        p2 = val->u.str.data;
        j = 0;

        for (i = 0; i < list->u.list.len; i++) {
            offs = indexoffs(list->u.list.data[i], len);
            if (offs < 0) {
                free(o);
                new_value.type = T_ERROR;
                new_value.u.error.num = ERROR___INDEX_RANGE;
                new_value.u.error.epoint = epoint;
                val_replace_template(&vals->val, &new_value);
                return;
            }
            while (offs != j) {
                if (offs > j) {
                    p2 += utf8len(*p2);
                    j++;
                } else {
                    do { p2--; } while (*p2 >= 0x80 && *p2 < 0xc0);
                    j--;
                }
            }
            k = utf8len(*p2);
            if ((size_t)(p + k - o) > m) {
                const uint8_t *r = o;
                m += 4096;
                o = realloc(o, m);
                if (!o) err_msg_out_of_memory();
                p += o - r;
            }
            memcpy(p, p2, k);p += k;
        }
        new_value.u.str.len = p - o;
        o = realloc(o, new_value.u.str.len);
        if (!o) err_msg_out_of_memory();
    }
    new_value.u.str.data = o;
    new_value.type = T_STR;
    val_replace_template(&vals->val, &new_value);
}

static void list_iindex(struct values_s *vals, const struct value_s *list, linepos_t epoint) {
    struct value_s *val;
    size_t i;
    size_t len;
    ival_t offs;

    val = vals->val;
    len = val->u.list.len;
    new_value.type = vals->val->type;
    new_value.u.list.len = list->u.list.len;
    if (!new_value.u.list.len) {
        val_replace(&vals->val, (new_value.type == T_TUPLE) ? &null_tuple : &null_list);return;
    }
    new_value.u.list.data = malloc(new_value.u.list.len * sizeof(new_value.u.list.data[0]));
    if (!new_value.u.list.data) err_msg_out_of_memory();
    for (i = 0; i < list->u.list.len; i++) {
        offs = indexoffs(list->u.list.data[i], len);
        if (offs < 0) {
            new_value.u.list.len = i;
            val_destroy2(&new_value);
            new_value.type = T_ERROR;
            new_value.u.error.num = ERROR___INDEX_RANGE;
            new_value.u.error.epoint = epoint;
            val_replace_template(&vals->val, &new_value);
            return;
        }
        new_value.u.list.data[i] = val_reference(val->u.list.data[offs]);
    }
    val_replace_template(&vals->val, &new_value);
}

static void code_iindex(struct values_s *vals, const struct value_s *list, linepos_t epoint) {
    struct value_s **val;
    const struct value_s *v;
    size_t i, i2;
    size_t len, len2;
    size_t offs2;
    int16_t r;
    ival_t offs;

    v = vals->val;
    if (!list->u.list.len) {
        val_replace(&vals->val, &null_tuple);return;
    }
    if (v->u.code.pass != pass) {
        new_value.type = T_ERROR;
        new_value.u.error.num = ERROR___NOT_DEFINED;
        new_value.u.error.epoint = vals->epoint;
        new_value.u.error.u.ident.len = 6;
        new_value.u.error.u.ident.data = (const uint8_t *)"<code>";
        val_replace(&vals->val, &new_value);
        return;
    }
    val = malloc(list->u.list.len * sizeof(new_value.u.list.data[0]));
    if (!val) err_msg_out_of_memory();
    len2 = (v->u.code.dtype < 0) ? -v->u.code.dtype : v->u.code.dtype;
    len2 = len2 + !len2;
    len = v->u.code.size / len2;
    new_value.u.num.len = len2 * 8;
    for (i = 0; i < list->u.list.len; i++) {
        offs = indexoffs(list->u.list.data[i], len);
        if (offs < 0) {
            new_value.type = T_TUPLE;
            new_value.u.list.data = val;
            new_value.u.list.len = i;
            val_destroy2(&new_value);
            new_value.type = T_ERROR;
            new_value.u.error.num = ERROR___INDEX_RANGE;
            new_value.u.error.epoint = epoint;
            val_replace_template(&vals->val, &new_value);
            return;
        }
        offs2 = offs * len2;
        new_value.u.num.val = 0;
        r = -1;
        for (i2 = 0; i2 < len2; i2++) {
            r = read_mem(v->u.code.memp, v->u.code.membp, offs2++);
            if (r < 0) break;
            new_value.u.num.val |= r << (i2 * 8);
        }
        if (v->u.code.dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(new_value.u.num.val); i2++) {
                new_value.u.num.val |= 0xff << (i2 * 8);
            }
        }
        new_value.type = (r < 0) ? T_GAP : ((v->u.code.dtype < 0) ? T_SINT : T_NUM);
        val[i] = val_reference(&new_value);
    }
    new_value.type = T_TUPLE;
    new_value.u.list.data = val;
    new_value.u.list.len = list->u.list.len;
    val_replace_template(&vals->val, &new_value);
}

static void bits_iindex(struct values_s *vals, const struct value_s *list, linepos_t epoint) {
    size_t i;
    size_t len;
    ival_t offs;

    len = get_val_len2(vals->val);
    new_value.type = T_NUM;
    new_value.u.num.len = (list->u.list.len < 8*sizeof(uval_t)) ? list->u.list.len : 8*sizeof(uval_t);
    new_value.u.num.val = 0;
    for (i = 0; i < list->u.list.len; i++) {
        offs = indexoffs(list->u.list.data[i], len);
        if (offs < 0) {
            new_value.type = T_ERROR;
            new_value.u.error.num = ERROR___INDEX_RANGE;
            new_value.u.error.epoint = epoint;
            val_replace_template(&vals->val, &new_value);
            return;
        }
        new_value.u.num.val |= ((vals->val->u.num.val >> offs) & 1) << i;
    }
    val_replace(&vals->val, &new_value);
}

static void str_slice(struct values_s *vals, ival_t offs, ival_t end, ival_t step) {
    struct value_s *val;
    size_t len;

    if (step > 0) {
        if (offs > end) offs = end;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }
    if (step == 1) {
        const uint8_t *p;
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
        val_replace(&vals->val, &new_value);
        val_destroy(val);
    } else {
        uint8_t *p, *o;
        const uint8_t *p2;
        val = vals->val;
        new_value.u.str.chars = len;
        if (!new_value.u.str.chars) {
            val_replace(&vals->val, &null_str);return;
        }
        if (val->u.str.len == val->u.str.chars) {
            new_value.u.str.len = new_value.u.str.chars;
            p = malloc(new_value.u.str.len);
            if (!p) err_msg_out_of_memory();
            new_value.u.str.data = p;
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p++ = val->u.str.data[offs];
                offs += step;
            }
        }
        else {
            ival_t i, j, k;
            o = p = malloc(val->u.str.len);
            if (!p) err_msg_out_of_memory();
            p2 = val->u.str.data;
            new_value.u.str.len = 0;
            for (i = 0; i < offs; i++) {
                p2 += utf8len(*p2);
            }
            if (step > 0) {
                for (k = i; i < end; i++) {
                    j = utf8len(*p2);
                    if (i == k) {memcpy(p, p2, j);p += j; k += step;}
                    p2 += j;
                }
            } else {
                p2 += utf8len(*p2);
                for (k = i; i > end; i--) {
                    j = 0;
                    do {
                        p2--;j++;
                    } while (*p2 >= 0x80 && *p2 < 0xc0);
                    if (i == k) {memcpy(p, p2, j);p += j; k += step;}
                }
            }
            new_value.u.str.len = p - o;
            o = realloc(o, new_value.u.str.len);
            if (!o) err_msg_out_of_memory();
            new_value.u.str.data = o;
        }
        new_value.type = T_STR;
        val_replace_template(&vals->val, &new_value);
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
        val_replace_template(&vals->val, &new_value);
    }
}

static void code_slice(struct values_s *vals, ival_t offs, ival_t end, ival_t step) {
    struct value_s **val;
    struct value_s *v;
    size_t i, i2;
    size_t len, len2;
    size_t offs2;
    int16_t r;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }

    v = vals->val;
    if (!len) {
        val_replace(&vals->val, &null_tuple);return;
    }
    if (v->u.code.pass != pass) {
        new_value.type = T_ERROR;
        new_value.u.error.num = ERROR___NOT_DEFINED;
        new_value.u.error.epoint = vals->epoint;
        new_value.u.error.u.ident.len = 6;
        new_value.u.error.u.ident.data = (const uint8_t *)"<code>";
        val_replace(&vals->val, &new_value);
        return;
    }
    val = malloc(len * sizeof(new_value.u.list.data[0]));
    if (!val) err_msg_out_of_memory();
    i = 0;
    len2 = (v->u.code.dtype < 0) ? -v->u.code.dtype : v->u.code.dtype;
    len2 = len2 + !len2;
    new_value.u.num.len = len2 * 8;
    while ((end > offs && step > 0) || (end < offs && step < 0)) {
        offs2 = offs * len2;
        new_value.u.num.val = 0;
        r = -1;
        for (i2 = 0; i2 < len2; i2++) {
            r = read_mem(v->u.code.memp, v->u.code.membp, offs2++);
            if (r < 0) break;
            new_value.u.num.val |= r << (i2 * 8);
        }
        if (v->u.code.dtype < 0 && (r & 0x80)) {
            for (; i2 < sizeof(new_value.u.num.val); i2++) {
                new_value.u.num.val |= 0xff << (i2 * 8);
            }
        }
        new_value.type = (r < 0) ? T_GAP : ((v->u.code.dtype < 0) ? T_SINT : T_NUM);
        val[i++] = val_reference(&new_value);
        offs += step;
    }
    new_value.type = T_TUPLE;
    new_value.u.list.len = len;
    new_value.u.list.data = val;
    val_replace_template(&vals->val, &new_value);
}

static void bits_slice(struct values_s *vals, ival_t offs, ival_t end, ival_t step) {
    size_t i;
    size_t len;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }

    new_value.type = T_NUM;
    new_value.u.num.len = len;
    if (step == 1) {
        new_value.u.num.val = (vals->val->u.num.val >> offs) & (((uval_t)1 << len)-1);
    } else {
        i = 0; new_value.u.num.val = 0;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            new_value.u.num.val |= ((vals->val->u.num.val >> offs) & 1) << (i++);
            offs += step;
        }
    }
    val_replace(&vals->val, &new_value);
}

static void indexes(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s *v2;
    int16_t r;
    size_t i;

    switch (try_resolv(vals)) {
    case T_LIST:
    case T_TUPLE:
    case T_STR:
    case T_CODE:
    case T_UINT:
    case T_SINT:
    case T_NUM:
    case T_BOOL:
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals[1].epoint); else
            switch (try_resolv(&v[0])) {
            case T_CODE:
                    err_msg_strange_oper(O_INDEX, v[0].val, NULL, v[0].epoint);
            case T_UINT:
            case T_SINT:
            case T_NUM:
            case T_BOOL:
                {
                    struct value_s *val;
                    size_t len;
                    ival_t offs;
                    switch (vals->val->type) {
                    case T_STR: len = vals->val->u.str.chars; break;
                    case T_CODE:
                        len = (vals->val->u.code.dtype < 0) ? -vals->val->u.code.dtype : vals->val->u.code.dtype;
                        len = vals->val->u.code.size / (len + !len);
                        break;
                    case T_LIST:
                    case T_TUPLE: len = vals->val->u.list.len; break;
                    default: len = get_val_len2(vals->val); break;
                    }

                    offs = indexoffs(v[0].val, len);
                    if (offs < 0) {
                        new_value.type = T_ERROR;
                        new_value.u.error.num = ERROR___INDEX_RANGE;
                        new_value.u.error.epoint = v[0].epoint;
                        val_replace_template(&vals->val, &new_value);
                        return;
                    }
                    switch (vals->val->type) {
                    case T_STR: str_slice(vals, offs, offs + 1, 1); break;
                    case T_CODE: 
                        v2 = vals->val;
                        if (v2->u.code.pass != pass) {
                            new_value.type = T_ERROR;
                            new_value.u.error.num = ERROR___NOT_DEFINED;
                            new_value.u.error.epoint = vals->epoint;
                            new_value.u.error.u.ident.len = 6;
                            new_value.u.error.u.ident.data = (const uint8_t *)"<code>";
                            val_replace(&vals->val, &new_value);
                            return;
                        }
                        new_value.u.num.len = (v2->u.code.dtype < 0) ? -v2->u.code.dtype : v2->u.code.dtype;
                        new_value.u.num.len = new_value.u.num.len + !new_value.u.num.len;
                        offs *= new_value.u.num.len;
                        new_value.u.num.val = 0;
                        r = -1;
                        for (i = 0; i < new_value.u.num.len; i++) {
                            r = read_mem(v2->u.code.memp, v2->u.code.membp, offs++);
                            if (r < 0) break;
                            new_value.u.num.val |= r << (i * 8);
                        }
                        if (v2->u.code.dtype < 0 && (r & 0x80)) {
                            for (; i < sizeof(new_value.u.num.val); i++) {
                                new_value.u.num.val |= 0xff << (i * 8);
                            }
                        }
                        new_value.u.num.len *= 8;
                        new_value.type = (r < 0) ? T_GAP : ((v2->u.code.dtype < 0) ? T_SINT : T_NUM);
                        val_replace(&vals->val, &new_value);
                        break;
                    case T_LIST:
                    case T_TUPLE:
                        val = val_reference(vals->val->u.list.data[offs]);
                        val_replace(&vals->val, val);
                        val_destroy(val);
                        break;
                    default:
                        new_value.type = T_NUM;
                        new_value.u.num.len = 1;
                        new_value.u.num.val = (vals->val->u.num.val >> offs) & 1;
                        val_replace(&vals->val, &new_value);
                        break;
                    }
                }
                return;
            case T_LIST:
            case T_TUPLE:
                switch (vals->val->type) {
                case T_STR: str_iindex(vals, v[0].val, v[0].epoint); return;
                case T_CODE: code_iindex(vals, v[0].val, v[0].epoint); return;
                case T_LIST:
                case T_TUPLE: list_iindex(vals, v[0].val, v[0].epoint); return;
                default: bits_iindex(vals, v[0].val, v[0].epoint); return;
                }
            default: err_msg_invalid_oper(O_INDEX, v[0].val, NULL, v[0].epoint);
            case T_NONE: 
                val_replace(&vals->val, &none_value);
                return;
            }
        break;
    default: err_msg_invalid_oper(O_INDEX, vals->val, NULL, vals->epoint);
             break;
    case T_NONE: return;
    }
    val_replace(&vals->val, &none_value);
    return;
}

static void slices(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    int i;

    switch (try_resolv(vals)) {
    case T_LIST:
    case T_TUPLE:
    case T_STR:
    case T_CODE:
    case T_UINT:
    case T_SINT:
    case T_NUM:
    case T_BOOL:
        if (args > 3 || args < 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals[0].epoint); else {
            uval_t len;
            ival_t offs = 0, end, step = 1;
            switch (vals->val->type) {
            case T_STR: len = vals->val->u.str.chars; break;
            case T_CODE: 
                len = (vals->val->u.code.dtype < 0) ? -vals->val->u.code.dtype : vals->val->u.code.dtype;
                len = vals->val->u.code.size / (len + !len);
                break;
            case T_LIST:
            case T_TUPLE: len = vals->val->u.list.len; break;
            default: len = get_val_len2(vals->val); break;
            }
            end = (ival_t)len;
            for (i = args - 1; i >= 0; i--) {
                switch (try_resolv(&v[i])) {
                case T_CODE:
                    err_msg_strange_oper(O_SLICE, v[i].val, NULL, v[i].epoint);
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
                    step = to_ival(v[2].val);
                    if (step == 0) {
                        new_value.type = T_ERROR;
                        new_value.u.error.num = ERROR_DIVISION_BY_Z;
                        new_value.u.error.epoint = v[0].epoint;
                        val_replace_template(&vals->val, &new_value);
                        return;
                    }
                }
            }
            if (args > 1) {
                if (v[1].val->type == T_DEFAULT) end = (step > 0) ? (ival_t)len : -1;
                else {
                    end = to_ival(v[1].val);
                    if (end >= 0) {
                        if (end > (ival_t)len) end = len;
                    } else {
                        if (end < 0) end += len;
                    }
                    if (end < -1) end = -1;
                }
            } else end = len;
            if (v[0].val->type == T_DEFAULT) offs = (step > 0) ? 0 : len - 1;
            else {
                offs = to_ival(v[0].val);
                if (offs >= 0) {
                    if (offs > (ival_t)len - (step < 0)) offs = len - (step < 0);
                } else {
                    if (offs < 0) offs += len;
                }
                if (offs < - (step < 0)) offs = - (step < 0);
            }
            switch (vals->val->type) {
            case T_STR: str_slice(vals, offs, end, step); break;
            case T_CODE: code_slice(vals, offs, end, step); break;
            case T_LIST:
            case T_TUPLE: list_slice(vals, offs, end, step); break;
            default: bits_slice(vals, offs, end, step); break;
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

static void apply_op(enum oper_e op, struct value_s *v1, struct value_s *v, linepos_t epoint, linepos_t epoint2) {
    enum type_e t1;
    struct value_s tmp1;
    char line[linelength]; 
    struct values_s vals1 = {v1, epoint};

    v1 = unwind_identrefs(&vals1, &tmp1);
    t1 = v1->type;
strretr:
    if (t1 == T_NONE) {
        v->type = T_NONE;
        return;
    }

    if (type_is_int(t1)) {
        switch (op) {
        case O_LOWER:
            v->type = T_NUM;
            v->u.num.len = 8;
            v->u.num.val = (uint8_t)v1->u.num.val;
            return;
        case O_HIGHER: 
            v->type = T_NUM;
            v->u.num.len = 8;
            v->u.num.val = (uint8_t)(v1->u.num.val >> 8);
            return;
        case O_BANK: 
            v->type = T_NUM;
            v->u.num.len = 8;
            v->u.num.val = (uint8_t)(v1->u.num.val >> 16);
            return;
        case O_BSWORD:
            v->type = T_NUM;
            v->u.num.len = 16;
            v->u.num.val = (uint8_t)(v1->u.num.val >> 8) | (uint16_t)(v1->u.num.val << 8);
            return;
        case O_HWORD: 
            v->type = T_NUM;
            v->u.num.len = 16;
            v->u.num.val = (uint16_t)(v1->u.num.val >> 8);
            return;
        case O_WORD: 
            v->type = T_NUM;
            v->u.num.len = 16;
            v->u.num.val = (uint16_t)v1->u.num.val;
            return;
        case O_HASH: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_IMMEDIATE; return;
            }
        case O_COMMAX: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_XR; return;
            }
        case O_COMMAY: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_YR; return;
            }
        case O_COMMAZ: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_ZR; return;
            }
        case O_COMMAR: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_RR; return;
            }
        case O_COMMAS: 
            {
                uint8_t len = (t1 == T_NUM) ? v1->u.num.len : 8*sizeof(address_t);
                v->type = T_ADDRESS; v->u.addr.val = v1->u.num.val; v->u.addr.len = len;
                v->u.addr.type = A_SR; return;
            }
        case O_INV:
            if (t1 == T_NUM) {
                v->type = T_NUM;
                v->u.num.len = v1->u.num.len;
            } else v->type = T_SINT;
            v->u.num.val = ~v1->u.num.val;
            return;
        case O_NEG: 
            if (t1 == T_NUM) {
                v->type = T_NUM;
                v->u.num.len = v1->u.num.len;
            } else v->type = T_SINT;
            v->u.num.val = -v1->u.num.val;
            return;
        case O_POS:
            if (t1 == T_NUM) {
                v->type = T_NUM;
                v->u.num.len = v1->u.num.len;
            } else v->type = T_SINT;
            v->u.num.val = v1->u.num.val;
            return;
        case O_STRING:
            {
                uint8_t *s;
                v->type = T_STR;
                v->u.str.len = sprintf(line, (v1->type == T_SINT) ? "%" PRIdval : "%" PRIuval, v1->u.num.val);
                v->u.str.chars = v->u.str.len;
                s = malloc(v->u.str.len);
                if (!s) err_msg_out_of_memory();
                memcpy(s, line, v->u.str.len);
                v->u.str.data = s;
                return;
            }
        default: err_msg_invalid_oper(op, v1, NULL, epoint2); 
                 v->type = T_NONE;
                 return;
        }
    }
    if (t1 == T_ADDRESS) {
        switch (op) {
        case O_LOWER:
            if (v1->u.addr.type == A_IMMEDIATE) {
                v->type = v1->type;
                v->u.addr.val = (uint8_t)v1->u.addr.val;
                v->u.addr.len = 8;
                v->u.addr.type = v1->u.addr.type;
                return;
            }
            break;
        case O_HIGHER:
            if (v1->u.addr.type == A_IMMEDIATE) {
                v->type = v1->type;
                v->u.addr.val = (uint8_t)(v1->u.addr.val >> 8);
                v->u.addr.len = 8;
                v->u.addr.type = v1->u.addr.type;
                return;
            }
            break;
        case O_COMMAX:
        case O_COMMAY:
        case O_COMMAZ:
        case O_COMMAR:
        case O_COMMAS:
            v->type = t1;
            v->u.addr.val = v1->u.addr.val;
            v->u.addr.len = v1->u.addr.len;
            v->u.addr.type = (v1->u.addr.type << 4);
            switch (op) {
            case O_COMMAX: v->u.addr.type |= A_XR;break;
            case O_COMMAY: v->u.addr.type |= A_YR;break;
            case O_COMMAZ: v->u.addr.type |= A_ZR;break;
            case O_COMMAR: v->u.addr.type |= A_RR;break;
            case O_COMMAS: v->u.addr.type |= A_SR;break;
            default: break;
            }
            return;
        default: break;
        }
    }
    if (t1 == T_CODE) {
        switch (op) {
        case O_INV:
        case O_NEG:
        case O_POS:
        case O_STRING:
            err_msg_strange_oper(op, v1, NULL, epoint2); 
        case O_HASH: 
        case O_COMMAX: 
        case O_COMMAY: 
        case O_COMMAZ: 
        case O_COMMAR: 
        case O_COMMAS: 
        case O_LOWER:
        case O_HIGHER: 
        case O_BANK: 
        case O_BSWORD:
        case O_HWORD: 
        case O_WORD: 
            tmp1.type = T_UINT;
            tmp1.u.num.val = v1->u.code.addr;
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        default: err_msg_invalid_oper(op, v1, NULL, epoint2); 
                 v->type = T_NONE;
                 return;
        }
    }
    if (t1 == T_STR) {
        switch (op) {
        case O_HASH: 
        case O_COMMAX: 
        case O_COMMAY: 
        case O_COMMAZ: 
        case O_COMMAR: 
        case O_COMMAS: 
        case O_LOWER:
        case O_HIGHER: 
        case O_BANK: 
        case O_BSWORD:
        case O_HWORD: 
        case O_WORD: 
        case O_INV:
        case O_NEG:
        case O_POS:
            str_to_num(v1, T_NUM, &tmp1, epoint);
            if (v == v1) free((uint8_t*)v1->u.str.data);
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        case O_STRING:
            if (v == v1) return;
            v->type = T_STR;
            v->u.str.len = v1->u.str.len;
            v->u.str.chars = v1->u.str.chars;
            if (v->u.str.len) {
                uint8_t *s;
                s = malloc(v->u.str.len);
                if (!s) err_msg_out_of_memory();
                memcpy(s, v1->u.str.data, v->u.str.len);
                v->u.str.data = s;
            } else v->u.str.data = NULL;
            return;
        default: err_msg_invalid_oper(op, v1, NULL, epoint2);
                 if (v == v1) free((uint8_t*)v1->u.str.data);
                 v->type = T_NONE;
                 return;
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
            tmp1.type = T_NUM;
            tmp1.u.num.val = (ival_t)v1->u.real;
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        case O_INV:
            v->type = T_FLOAT;
            v->u.real = -v1->u.real-1.0;
            return;
        case O_NEG:
            v->type = T_FLOAT;
            v->u.real = -v1->u.real;
            return;
        case O_POS:
            v->type = T_FLOAT;
            v->u.real = v1->u.real;
            return;
        case O_STRING:
            {
                int i = 0;
                uint8_t *s;
                sprintf(line, "%.10g", v1->u.real);
                while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
                if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
                v->type = T_STR;
                v->u.str.len = i + strlen(line + i);
                v->u.str.chars = v->u.str.len;
                s = malloc(v->u.str.len);
                if (!s) err_msg_out_of_memory();
                memcpy(s, line, v->u.str.len);
                v->u.str.data = s;
                return;
            }
        default: err_msg_invalid_oper(op, v1, NULL, epoint2);
                 v->type = T_NONE;
                 return;
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
        case O_POS:
        case O_STRING:
            {
                size_t i = 0;
                struct value_s **vals;
                if (v == v1) {
                    for (;i < v1->u.list.len; i++) {
                        if (v1->u.list.data[i]->refcount != 1) {
                            apply_op(op, v1->u.list.data[i], &new_value, epoint, epoint2);
                            val_replace_template(v1->u.list.data + i, &new_value);
                        } else apply_op(op, v1->u.list.data[i], v1->u.list.data[i], epoint, epoint2);
                    }
                    return;
                }
                if (v1->u.list.len) {
                    vals = malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (;i < v1->u.list.len; i++) {
                        vals[i] = val_alloc();
                        vals[i]->refcount = 1;
                        apply_op(op, v1->u.list.data[i], vals[i], epoint, epoint2);
                    }
                } else vals = NULL;
                v->type = t1;
                v->u.list.len = i;
                v->u.list.data = vals;
                return;
            }
        default: err_msg_invalid_oper(op, v1, NULL, epoint2); 
                 if (v == v1) val_destroy2(v1);
                 v->type = T_NONE;
                 return;
        }
    }
    if (t1 == T_ERROR) { 
        if (v != v1) {
            v->type = T_ERROR; 
            v->u.error = v1->u.error;
        }
        return; 
    }
    else err_msg_invalid_oper(op, v1, NULL, epoint2);
    if (v == v1) val_destroy2(v1);
    v->type = T_NONE;
    return;
}

static inline uint8_t addlen(int l1, int l2) {
    int l = l1 + l2;
    if (l < 0) return 0;
    if (l < 8*(int)sizeof(uval_t)) return l;
    return 8*sizeof(uval_t);
}

static void apply_op2(enum oper_e, struct value_s *, struct value_s *, struct value_s *, linepos_t, linepos_t, linepos_t);

int val_equals(struct value_s *v1, struct value_s *v2, linepos_t epoint, linepos_t epoint2, linepos_t epoint3) {
    struct value_s tmp;
    apply_op2(O_EQ, v1, v2, &tmp, epoint, epoint2, epoint3);
    if (tmp.type == T_BOOL) return tmp.u.num.val;
    return -1;
}

static void inlist(struct value_s *v1, struct value_s *v2, struct value_s *v, linepos_t epoint, linepos_t epoint2, linepos_t epoint3) {
    size_t i, len, offs;
    int16_t r;

    switch (v2->type) {
    case T_CODE:
        if (v2->u.code.pass != pass) {
            v->type = T_ERROR;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint2;
            v->u.error.u.ident.len = 6;
            v->u.error.u.ident.data = (const uint8_t *)"<code>";
            return;
        }
        len = (v2->u.code.dtype < 0) ? -v2->u.code.dtype : v2->u.code.dtype;
        len = len + !len;
        new_value.u.num.len = len * 8;
        for (offs = 0; offs < v2->u.code.size;) {
            new_value.u.num.val = 0;
            r = -1;
            for (i = 0; i < len; i++) {
                r = read_mem(v2->u.code.memp, v2->u.code.membp, offs++);
                if (r < 0) break;
                new_value.u.num.val |= r << (i * 8);
            }
            if (v2->u.code.dtype < 0 && (r & 0x80)) {
                for (; i < sizeof(new_value.u.num.val); i++) {
                    new_value.u.num.val |= 0xff << (i * 8);
                }
            }
            new_value.type = (r < 0) ? T_GAP : ((v2->u.code.dtype < 0) ? T_SINT : T_NUM);
            if (val_equals(v1, &new_value, epoint, epoint2, epoint3)) {
                if (v == v1) val_destroy2(v1);
                v->type = T_BOOL; v->u.num.val = 1;
                return;
            }
        }
        if (v == v1) val_destroy2(v1);
        v->type = T_BOOL; v->u.num.val = 0;
        return;
    case T_LIST:
    case T_TUPLE:
        for (i = 0;i < v2->u.list.len; i++) {
            if (val_equals(v1, v2->u.list.data[i], epoint, epoint2, epoint3)) {
                if (v == v1) val_destroy2(v1);
                v->type = T_BOOL; v->u.num.val = 1;
                return;
            }
        }
        if (v == v1) val_destroy2(v1);
        v->type = T_BOOL; v->u.num.val = 0;
        return;
    default: err_msg_wrong_type(v2, epoint2); 
    case T_NONE: 
             if (v == v1) val_destroy2(v1);
             v->type = T_NONE;
    }
}

int val_inlist(struct value_s *v1, struct value_s *v2, linepos_t epoint, linepos_t epoint2, linepos_t epoint3) {
    struct value_s tmp;
    inlist(v1, v2, &tmp, epoint, epoint2, epoint3);
    if (tmp.type == T_BOOL) return tmp.u.num.val;
    return -1;
}

static void onlist(enum oper_e op, struct value_s *v1, struct value_s *v2, struct value_s *v, linepos_t epoint, linepos_t epoint2, linepos_t epoint3) {
    size_t i = 0;
    struct value_s **vals;
    if (v2->u.list.len) {
        vals = malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
        if (!vals) err_msg_out_of_memory();
        for (;i < v2->u.list.len; i++) {
            vals[i] = val_alloc();
            vals[i]->refcount = 1;
            apply_op2(op, v1, v2->u.list.data[i], vals[i], epoint, epoint2, epoint3);
        }
    } else vals = NULL;
    if (v == v1) val_destroy2(v1);
    v->type = v2->type;
    v->u.list.len = i;
    v->u.list.data = vals;
    return;
}

static void apply_op2(enum oper_e op, struct value_s *v1, struct value_s *v2, struct value_s *v, linepos_t epoint, linepos_t epoint2, linepos_t epoint3) {
    enum type_e t1;
    enum type_e t2;
    struct value_s tmp1, tmp2;
    struct values_s vals1 = {v1, epoint}, vals2 = {v2, epoint2};

    v1 = unwind_identrefs(&vals1, &tmp1);
    t1 = v1->type;
    v2 = unwind_identrefs(&vals2, &tmp2);
    t2 = v2->type;
strretr:
    if (t1 == T_NONE) {v->type = T_NONE;return;}
    if (t2 == T_NONE) {
    errtype:
        if (v == v1) val_destroy2(v1);
        v->type = T_NONE;
        return;
    }

    if (type_is_int(t1)) {
        if (type_is_int(t2)) {
            ival_t val1 = v1->u.num.val;
            ival_t val2 = v2->u.num.val;

            if (t2 < t1) t2 = t1;

            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = (val1 == val2);return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = (val1 != val2);return;
            case O_LT: v->type = T_BOOL; v->u.num.val = (val1 < val2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = (val1 > val2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = (val1 <= val2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = (val1 >= val2);return;
            case O_MUL: val1 = ( val1 *  val2);break;
            case O_DIV: if (!val2) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = epoint2;
                return;
            } else if (t2==T_SINT) val1 = ( val1 / val2); else val1 = ( (uval_t)val1 / (uval_t)val2);  break;
            case O_MOD: if (!val2) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = epoint2;
                return;
            } else if (t2==T_SINT) val1 = ( val1 % val2); else val1 = ( (uval_t)val1 % (uval_t)val2); break;
            case O_ADD: val1 = ( val1 +  val2);break;
            case O_SUB: if (t2 == T_UINT && (uval_t)val2 > (uval_t)val1) t2 = T_SINT;
                             val1 = ( val1 -  val2);break;
            case O_AND: val1 &= val2; goto binset;
            case O_OR: val1 |= val2; goto binset;
            case O_XOR: val1 ^= val2;
                    binset:
                        if (t2 == T_NUM) {
                            v->u.num.len = (v1->u.num.len > v2->u.num.len) ? v1->u.num.len : v2->u.num.len;
                        } else {
                            v->u.num.len = get_val_len(val1, t2);
                            if (v1->type == T_NUM && v->u.num.len < v1->u.num.len) v->u.num.len = v1->u.num.len;
                            else if (v2->type == T_NUM && v->u.num.len < v2->u.num.len) v->u.num.len = v2->u.num.len;
                        }
                        v->type = T_NUM;
                        v->u.num.val = val1;
                        return;
            case O_LSHIFT:
                        if (val2 < 0) {val2 = -val2; goto rshift;}
                    lshift: 
                        if (val2 >= (ival_t)sizeof(val1)*8) val1=0;
                        else val1 <<= val2;
                        if (t1 == T_NUM) {
                            v->type = T_NUM;
                            v->u.num.len = addlen(v1->u.num.len, val2);
                        } else if (t1 == T_BOOL) {
                            v->type = T_NUM;
                            v->u.num.len = addlen(1, val2);
                        } else v->type = t1;
                        v->u.num.val = val1;
                        return;
            case O_ASHIFT: 
                    rshift: 
                        if (t1 == T_SINT) {
                            if (val2 < 0) {val2 = -val2; goto lshift;}
                            if (val2 >= (ival_t)sizeof(val1)*8) val1 = (val1 > 0) ? 0 : -1;
                            else if (val1 >= 0) val1 >>= val2;
                            else val1 = ~((~val1) >> val2);
                            v->type = T_SINT;
                            v->u.num.val = val1;
                            return;
                        }
            case O_RSHIFT: 
                        if (val2 < 0) {val2 = -val2; goto lshift;}
                        if (val2 >= (ival_t)sizeof(val1)*8) val1=0;
                        else val1 = (ival_t)((uval_t)val1 >> val2);
                        if (t1 == T_NUM) {
                            v->type = T_NUM;
                            v->u.num.len = addlen(v1->u.num.len, -val2);
                        } else if (t1 == T_BOOL) {
                            v->type = T_NUM;
                            v->u.num.len = addlen(1, -val2);
                        } else v->type = t1;
                        v->u.num.val = val1;
                        return;
            case O_EXP: 
                        {
                            ival_t res = 1;

                            if (val2 < 0) {
                                if (!val1) {
                                    v->type = T_ERROR;
                                    v->u.error.num = ERROR_DIVISION_BY_Z;
                                    v->u.error.epoint = epoint2;
                                    return;
                                }
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
            case O_CONCAT: 
                        {
                            uint8_t l1 = get_val_len2(v1), l2 = get_val_len2(v2);
                            v->type = T_NUM;
                            v->u.num.len = addlen(l1 ,l2);
                            if (l1 < (8*sizeof(uval_t))) val1 &= (((uval_t)1 << l1)-1);
                            v->u.num.val = (val1 << l2) | (val2 & (((uval_t)1 << l2)-1));
                            return;
                        }
            default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                     v->type = T_NONE;
                     return;
            }
            v->type = (t2 == T_SINT) ? T_SINT : T_UINT;
            v->u.num.val = val1;
            return;
        }
        if (t2 == T_ADDRESS) {
            ival_t val1 = v1->u.num.val;
            switch (op) {
            case O_ADD: 
                v->u.addr.val = (t1 == T_SINT) ? (val1 + v2->u.addr.val) : ((uval_t)val1 + v2->u.addr.val);
                v->u.addr.len = v2->u.addr.len;
                v->type = t2;
                v->u.addr.type = v2->u.addr.type;
                return;
            default:break;
            }
        }
        if (t2 == T_CODE && op != O_CONCAT && op != O_X) {
            switch (op) {
            case O_IN: inlist(v1, v2, v, epoint, epoint2, epoint3);return;
            case O_ADD:
                v->u.code.addr = v2->u.code.addr + to_ival(v1);
                v->type = t2;
                v->u.code.size = 0;
                v->u.code.dtype = D_NONE;
                return;
            default:
                err_msg_strange_oper(op, v1, v2, epoint3); 
            }
            tmp2.type = T_UINT;
            tmp2.u.num.val = v2->u.code.addr;
            v2 = &tmp2;
            t2 = v2->type;
            goto strretr;
        }
        if (t2 == T_STR && op != O_IN && op != O_CONCAT && op != O_X) {
            str_to_num(v2, T_NUM, &tmp2, epoint2);
            v2 = &tmp2;
            t2 = v2->type;
            goto strretr;
        }
        if (t2 == T_LIST || t2 == T_TUPLE) {
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = 0;return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = 1;return;
            case O_LT: v->type = T_BOOL; v->u.num.val = (t1 < t2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = (t1 > t2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = (t1 <= t2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = (t1 >= t2);return;
            case O_IN: inlist(v1, v2, v, epoint, epoint2, epoint3);return;
            case O_X:
            case O_CONCAT:err_msg_invalid_oper(op, v1, v2, epoint3);
                          v->type = T_NONE;
                          return;
            default: onlist(op, v1, v2, v, epoint, epoint2, epoint3);return;
            }
        }
        if (t2 == T_FLOAT && op != O_IN && op != O_CONCAT && op != O_X) {
            if (v1 != v) {
                tmp1.type = T_FLOAT;
                tmp1.u.real = (t1 == T_SINT) ? (double)((ival_t)v1->u.num.val) : (double)((uval_t)v1->u.num.val);
                v1 = &tmp1;
            } else {
                v1->type = T_FLOAT;
                v1->u.real = (t1 == T_SINT) ? (double)((ival_t)v1->u.num.val) : (double)((uval_t)v1->u.num.val);
            }
            t1 = v1->type;
        }
    }
    if (t1 == T_CODE) {
        if (t1 == t2) {
            address_t val1 = v1->u.code.addr;
            address_t val2 = v2->u.code.addr;
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = ( val1 == val2);return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = ( val1 != val2);return;
            case O_LT: v->type = T_BOOL; v->u.num.val = ( val1 < val2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = ( val1 > val2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = ( val1 <= val2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = ( val1 >= val2);return;
            case O_SUB:
                v->type = (val1 >= val2) ? T_UINT : T_SINT;
                v->u.num.val = val1 - val2;
                return;
            case O_ADD: 
            case O_MUL:
            case O_DIV:
            case O_MOD:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_ASHIFT: 
            case O_RSHIFT: 
            case O_EXP: 
                err_msg_strange_oper(op, v1, v2, epoint3);
                break;
            default:err_msg_invalid_oper(op, v1, v2, epoint3);
                    v->type = T_NONE;
                    return;
            }
            if (v1 != v) {
                tmp1.type = T_UINT;
                tmp1.u.num.val = v1->u.code.addr;
                v1 = &tmp1;
            } else {
                v1->type = T_UINT;
                v1->u.num.val = v1->u.code.addr;
            }
            t1 = v1->type;
            goto strretr;
        }
        if (type_is_num(t2)) {
            switch (op) {
            case O_ADD: 
                v->u.code.addr = v1->u.code.addr + to_ival(v2);
                v->type = t1;
                v->u.code.size = 0;
                v->u.code.dtype = D_NONE;
                return;
            case O_SUB: 
                v->u.code.addr = v1->u.code.addr - to_ival(v2);
                v->type = t1;
                v->u.code.size = 0;
                v->u.code.dtype = D_NONE;
                return;
            case O_EQ:  
            case O_NEQ:
            case O_LT:
            case O_GT:
            case O_LE:
            case O_GE:
            case O_MUL:
            case O_DIV:
            case O_MOD:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_ASHIFT: 
            case O_RSHIFT: 
            case O_EXP: 
                err_msg_strange_oper(op, v1, v2, epoint3);
                break;
            default:err_msg_invalid_oper(op, v1, v2, epoint3);
                    v->type = T_NONE;
                    return;
            }
            if (v1 != v) {
                tmp1.type = T_UINT;
                tmp1.u.num.val = v1->u.code.addr;
                v1 = &tmp1;
            } else {
                v1->type = T_UINT;
                v1->u.num.val = v1->u.code.addr;
            }
            t1 = v1->type;
            goto strretr;
        }
        if (t2 == T_STR && op != O_IN && op != O_CONCAT && op != O_X) {
            err_msg_strange_oper(op, v1, v2, epoint3);
            str_to_num(v2, T_NUM, &tmp2, epoint2);
            v2 = &tmp2;
            t2 = v2->type;
            goto strretr;
        }
        if (t2 == T_LIST || t2 == T_TUPLE) {
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = 0;return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = 1;return;
            case O_LT: v->type = T_BOOL; v->u.num.val = (t1 < t2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = (t1 > t2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = (t1 <= t2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = (t1 >= t2);return;
            case O_IN: inlist(v1, v2, v, epoint, epoint2, epoint3);return;
            case O_X:
            case O_CONCAT:err_msg_invalid_oper(op, v1, v2, epoint3);
                          v->type = T_NONE;
                          return;
            default: onlist(op, v1, v2, v, epoint, epoint2, epoint3);return;
            }
        }
        if (t2 == T_FLOAT && op != O_IN && op != O_CONCAT && op != O_X) {
            err_msg_strange_oper(op, v1, v2, epoint3);
            if (v1 != v) {
                tmp1.type = T_FLOAT;
                tmp1.u.real = v1->u.code.addr;
                v1 = &tmp1;
            } else {
                v1->type = T_FLOAT;
                v1->u.real = v1->u.code.addr;
            }
            t1 = v1->type;
        }
    }

    if (t1 == T_ADDRESS) {
        if (t1 == t2) {
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = (v1->u.addr.type == v2->u.addr.type) && (v1->u.addr.val == v2->u.addr.val); return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = (v1->u.addr.type != v2->u.addr.type) || (v1->u.addr.val != v2->u.addr.val); return;
            default:break;
            }
        }
        if (type_is_num(t2)) {
            switch (op) {
            case O_ADD: 
                v->u.addr.val = v1->u.addr.val + to_ival(v2);
                v->u.addr.len = v1->u.addr.len;
                v->type = t1;
                v->u.addr.type = v1->u.addr.type;
                return;
            case O_SUB: 
                v->u.addr.val = v1->u.addr.val - to_ival(v2);
                v->u.addr.len = v1->u.addr.len;
                v->type = t1;
                v->u.addr.type = v1->u.addr.type;
                return;
            default: break;
            }
        }
    }

    if (t1 == T_STR) {
        if (t1 == t2) {
            int val;
            switch (op) {
            case O_EQ:
                val = (v1->u.str.len == v2->u.str.len) && (v1->u.str.data == v2->u.str.data || !memcmp(v1->u.str.data, v2->u.str.data, v1->u.str.len));
            strcomp:
                if (v == v1) free((uint8_t *)v1->u.str.data);
                v->type = T_BOOL; v->u.num.val = val;
                return;
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
                str_to_num(v1, T_NUM, &tmp1, epoint);
                if (v == v1) free((uint8_t *)v1->u.str.data);
                v1 = &tmp1; t1 = v1->type;
                str_to_num(v2, T_NUM, &tmp2, epoint2);
                v2 = &tmp2; t2 = v2->type;
                goto strretr;
            case O_CONCAT:
                if (v == v1) {
                    uint8_t *s;
                    size_t len;
                    if (!v2->u.str.len) return;
                    len = v1->u.str.len;
                    v->u.str.len += v2->u.str.len;
                    v->u.str.chars += v2->u.str.chars;
                    s = (uint8_t *)v1->u.str.data;
                    s = realloc(s, v->u.str.len);
                    if (!s) err_msg_out_of_memory();
                    memcpy(s + len, v2->u.str.data, v2->u.str.len);
                    v->u.str.data = s;
                } else {
                    v->type = T_STR;
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
                }
                return;
            case O_IN:
                {
                    const uint8_t *c, *c2, *e;
                    if (!v1->u.str.len) {
                        if (v == v1) free((uint8_t *)v1->u.str.data);
                        v->type = T_BOOL; v->u.num.val = 1;return;
                    }
                    if (v1->u.str.len > v2->u.str.len) {
                        if (v == v1) free((uint8_t *)v1->u.str.data);
                        v->type = T_BOOL; v->u.num.val = 0;return;
                    }
                    c2 = v2->u.str.data;
                    e = c2 + v2->u.str.len - v1->u.str.len;
                    for (;;) {   
                        c = memchr(c2, v1->u.str.data[0], e - c2 + 1);
                        if (!c) {
                            if (v == v1) free((uint8_t *)v1->u.str.data);
                            v->type = T_BOOL; v->u.num.val = 0;return;
                        }
                        if (!memcmp(c, v1->u.str.data, v1->u.str.len)) {
                            if (v == v1) free((uint8_t *)v1->u.str.data);
                            v->type = T_BOOL; v->u.num.val = 1;return;
                        }
                        c2 = c + 1;
                    }
                }
            default: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
            }
        }
        if (type_is_num(t2)) {
            switch (op) {
            case O_X:
                {
                    uval_t rep;

                    if (!type_is_int(t2)) {err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;}

                    rep = (t2 == T_SINT && v2->u.num.val < 0) ? 0 : (uval_t)v2->u.num.val;
                    if (v == v1) {
                        uint8_t *s;
                        size_t len = v1->u.str.len;
                        if (!len) return;
                        if (!rep) {
                            v->u.str.len = 0;
                            v->u.str.chars = 0;
                            free((uint8_t *)v->u.str.data);
                            v->u.str.data = NULL;
                            return;
                        }
                        if (rep == 1) return;
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
                        v->type = T_STR;
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
            case O_IN:
            case O_CONCAT: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
            default: break;
            }
            str_to_num(v1, T_NUM, &tmp1, epoint);
            if (v == v1) free((uint8_t *)v1->u.str.data);
            v1 = &tmp1;
            t1 = v1->type;
            goto strretr;
        }
        if (t2 == T_CODE && op != O_IN && op != O_CONCAT && op != O_X) {
            err_msg_strange_oper(op, v1, v2, epoint3); 
            tmp2.type = T_UINT;
            tmp2.u.num.val = v2->u.code.addr;
            v2 = &tmp2;
            t2 = v2->type;
            goto strretr;
        }
        if (t2 == T_LIST || t2 == T_TUPLE) {
            if (op == O_MOD) {
                isnprintf(v1, v2, v, epoint, epoint2);
                return;
            }
        }
    }

    if (t1 == T_FLOAT) {
        if (type_is_int(t2)) {
            switch (op) {
            case O_EQ:
            case O_NEQ: 
            case O_LT: 
            case O_GT:
            case O_LE:
            case O_GE:
            case O_MUL:
            case O_DIV:
            case O_MOD: 
            case O_ADD:
            case O_SUB:
            case O_EXP: break; 
            default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                     v->type = T_NONE;
                     return;
            }
            tmp2.type = T_FLOAT;
            tmp2.u.real = (t2 == T_SINT) ? (double)((ival_t)v2->u.num.val) : (double)((uval_t)v2->u.num.val);
            v2 = &tmp2;
            t2 = v2->type;
        }
        if (t2 == T_CODE) {
            switch (op) {
            case O_LT: 
            case O_GT:
            case O_LE:
            case O_GE:
            case O_MUL:
            case O_DIV:
            case O_MOD: 
            case O_ADD:
            case O_SUB:
            case O_EXP: 
                err_msg_strange_oper(op, v1, v2, epoint3); 
            case O_EQ:
            case O_NEQ: 
                break; 
            default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                     v->type = T_NONE;
                     return;
            }
            tmp2.type = T_FLOAT;
            tmp2.u.real = v2->u.code.addr;
            v2 = &tmp2;
            t2 = v2->type;
        }
        if (t1 == t2) {
            double val1 = v1->u.real;
            double val2 = v2->u.real;

            switch (op) {
            case O_EQ:   v->type = T_BOOL; v->u.num.val = almost_equal(val1, val2); return;
            case O_NEQ:  v->type = T_BOOL; v->u.num.val = !almost_equal(val1, val2);
            case O_LT:   v->type = T_BOOL; v->u.num.val = ( val1 < val2);return;
            case O_GT:   v->type = T_BOOL; v->u.num.val = ( val1 > val2);return;
            case O_LE:   v->type = T_BOOL; v->u.num.val = ( val1 < val2 || almost_equal(val1, val2));return;
            case O_GE:   v->type = T_BOOL; v->u.num.val = ( val1 > val2 || almost_equal(val1, val2));return;
            case O_MUL:  val1 = ( val1 *  val2);break;
            case O_DIV: if (!val2) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = epoint2;
                return;
            } else val1 = ( val1 /  val2); break;
            case O_MOD: if (!val2) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = epoint2;
                return;
            } else val1 = fmod(val1, val2); break;
            case O_ADD:  val1 = ( val1 +  val2);break;
            case O_SUB:  val1 = ( val1 -  val2);break;
            case O_EXP: if (val2 < 0.0 && !val1) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = epoint2;
                return;
            } else if (val1 < 0.0 && (double)((int)val2) != val2) {
                v->type = T_ERROR;
                v->u.error.num = ERROR_NEGFRAC_POWER;
                v->u.error.epoint = epoint2;
                return;
            } else val1 = pow(val1, val2); break;
            default: err_msg_invalid_oper(op, v1, v2, epoint3); 
                     v->type = T_NONE;
                     return;
            }
            v->type = t1;
            v->u.real = val1;
            return;
        }
        if (t2 == T_LIST || t2 == T_TUPLE) {
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = 0;return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = 1;return;
            case O_LT: v->type = T_BOOL; v->u.num.val = (t1 < t2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = (t1 > t2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = (t1 <= t2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = (t1 >= t2);return;
            case O_IN: inlist(v1, v2, v, epoint, epoint2, epoint3);return;
            case O_X:
            case O_CONCAT:err_msg_invalid_oper(op, v1, v2, epoint3);
                          v->type = T_NONE;
                          return;
            default: onlist(op, v1, v2, v, epoint, epoint2, epoint3);return;
            }
        }
        if (t2 == T_STR) {
            switch (op) {
            case O_EQ:
            case O_NEQ: 
            case O_LT: 
            case O_GT:
            case O_LE:
            case O_GE:
            case O_MUL:
            case O_DIV:
            case O_MOD: 
            case O_ADD:
            case O_SUB:
            case O_EXP: break; 
            default: err_msg_invalid_oper(op, v1, v2, epoint3);
                     v->type = T_NONE;
                     return;
            }
            str_to_num(v2, T_NUM, &tmp2, epoint2);
            v2 = &tmp2;
            t2 = v2->type;
            goto strretr;
        }
    }

    if (t1 == T_LIST || t1 == T_TUPLE) {
        if (t1 == t2) {
            size_t i;
            switch (op) {
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
                {
                    struct value_s **vals;
                    const struct value_s *vx;
                    int d1 = 0, d2 = 0;
                    i = 0;
                    vx = v1;
                    while (vx->type == T_LIST || vx->type == T_TUPLE) {
                        d1++;
                        if (!vx->u.list.len) break;
                        vx = vx->u.list.data[0];
                    }
                    vx = v2;
                    while (vx->type == T_LIST || vx->type == T_TUPLE) {
                        d2++;
                        if (!vx->u.list.len) break;
                        vx = vx->u.list.data[0];
                    }
                    if (d1 == d2) {
                        if (v1->u.list.len != v2->u.list.len && v1->u.list.len != 1 && v2->u.list.len != 1) {err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;}
                        if (v1->u.list.len == 1) {
                            if (v2->u.list.len) {
                                vals = malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
                                if (!vals) err_msg_out_of_memory();
                                for (; i < v2->u.list.len; i++) {
                                    vals[i] = val_alloc();
                                    vals[i]->refcount = 1;
                                    apply_op2(op, v1->u.list.data[0], v2->u.list.data[i], vals[i], epoint, epoint2, epoint3);
                                }
                            } else vals = NULL;
                        } else if (v2->u.list.len == 1) {
                            if (v1->u.list.len) {
                                vals = malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                                if (!vals) err_msg_out_of_memory();
                                for (; i < v1->u.list.len; i++) {
                                    vals[i] = val_alloc();
                                    vals[i]->refcount = 1;
                                    apply_op2(op, v1->u.list.data[i], v2->u.list.data[0], vals[i], epoint, epoint2, epoint3);
                                }
                            } else vals = NULL;
                        } else if (v1->u.list.len) {
                            vals = malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                            if (!vals) err_msg_out_of_memory();
                            for (; i < v1->u.list.len; i++) {
                                vals[i] = val_alloc();
                                vals[i]->refcount = 1;
                                apply_op2(op, v1->u.list.data[i], v2->u.list.data[i], vals[i], epoint, epoint2, epoint3);
                            }
                        } else vals = NULL;
                    } else if (d1 > d2) {
                        if (v1->u.list.len) {
                            vals = malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                            if (!vals) err_msg_out_of_memory();
                            for (; i < v1->u.list.len; i++) {
                                vals[i] = val_alloc();
                                vals[i]->refcount = 1;
                                apply_op2(op, v1->u.list.data[i], v2, vals[i], epoint, epoint2, epoint3);
                            }
                        } else vals = NULL;
                    } else if (v2->u.list.len) {
                        vals = malloc(v2->u.list.len * sizeof(v->u.list.data[0]));
                        if (!vals) err_msg_out_of_memory();
                        for (; i < v2->u.list.len; i++) {
                            vals[i] = val_alloc();
                            vals[i]->refcount = 1;
                            apply_op2(op, v1, v2->u.list.data[i], vals[i], epoint, epoint2, epoint3);
                        }
                    } else vals = NULL;
                    if (v == v1) val_destroy2(v1);
                    v->type = t1;
                    v->u.list.len = i;
                    v->u.list.data = vals;
                    return;
                }
            case O_EQ:
                {
                    if (v1->u.list.len != v2->u.list.len) {
                        if (v == v1) val_destroy2(v1);
                        v->type = T_BOOL; v->u.num.val = 0;return;
                    }
                    for (i = 0; i < v2->u.list.len; i++) {
                        struct value_s tmp;
                        apply_op2(op, v1->u.list.data[i], v2->u.list.data[i], &tmp, epoint, epoint2, epoint3);
                        if (tmp.type == T_BOOL && !tmp.u.num.val) {
                            if (v == v1) val_destroy2(v1);
                            v->type = T_BOOL; v->u.num.val = 0;return;
                        }
                    }
                    if (v == v1) val_destroy2(v1);
                    v->type = T_BOOL; v->u.num.val = 1;return;
                }
            case O_NEQ:
                {
                    if (v1->u.list.len != v2->u.list.len) {
                        if (v == v1) val_destroy2(v1);
                        v->type = T_BOOL; v->u.num.val = 1;return;
                    }
                    for (i = 0; i < v2->u.list.len; i++) {
                        struct value_s tmp;
                        apply_op2(op, v1->u.list.data[i], v2->u.list.data[i], &tmp, epoint, epoint2, epoint3);
                        if (tmp.type == T_BOOL && tmp.u.num.val) {
                            if (v == v1) val_destroy2(v1);
                            v->type = T_BOOL; v->u.num.val = 1;return;
                        }
                    }
                    if (v == v1) val_destroy2(v1);
                    v->type = T_BOOL; v->u.num.val = 0;return;
                }
            case O_CONCAT:
                {
                    if (v == v1) {
                        size_t len;
                        if (!v2->u.list.len) return;
                        i = len = v->u.list.len;
                        v->u.list.len += v2->u.list.len;
                        v->u.list.data = realloc(v->u.list.data, v->u.list.len * sizeof(v->u.list.data[0]));
                        if (!v->u.list.data) err_msg_out_of_memory();
                        for (; i < v->u.list.len; i++) {
                            v->u.list.data[i] = val_reference(v2->u.list.data[i - len]);
                        }
                    } else {
                        v->type = t1;
                        v->u.list.len = v1->u.list.len + v2->u.list.len;
                        if (v->u.list.len) {
                            v->u.list.data = malloc(v->u.list.len * sizeof(v->u.list.data[0]));
                            if (!v->u.list.data) err_msg_out_of_memory();
                            for (i = 0; i < v1->u.list.len; i++) {
                                v->u.list.data[i] = val_reference(v1->u.list.data[i]);
                            }
                            for (; i < v->u.list.len; i++) {
                                v->u.list.data[i] = val_reference(v2->u.list.data[i - v1->u.list.len]);
                            }
                        } else v->u.list.data = NULL;
                    }
                    return;
                }
            case O_IN: inlist(v1, v2, v, epoint, epoint2, epoint3);return;
            default: err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
            }
            return;
        }
        if (type_is_num(t2) || t2 == T_CODE) {
            switch (op) {
            case O_EQ: v->type = T_BOOL; v->u.num.val = 0;return;
            case O_NEQ: v->type = T_BOOL; v->u.num.val = 1;return;
            case O_LT: v->type = T_BOOL; v->u.num.val = (t1 < t2);return;
            case O_GT: v->type = T_BOOL; v->u.num.val = (t1 > t2);return;
            case O_LE: v->type = T_BOOL; v->u.num.val = (t1 <= t2);return;
            case O_GE: v->type = T_BOOL; v->u.num.val = (t1 >= t2);return;
            case O_X:
                {
                    size_t i = 0, j, len;
                    struct value_s **vals;
                    uval_t rep;

                    if (!type_is_int(t2)) {err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;}

                    rep = (t2 == T_SINT && v2->u.num.val < 0) ? 0 : (uval_t)v2->u.num.val;
                    if (v == v1) {
                        if (!v1->u.list.len) return;
                        if (!rep) {
                            val_destroy2(v1);
                            v->u.list.len = 0;
                            v->u.list.data = NULL;
                            return;
                        }
                        if (rep == 1) return;
                        i = len = v->u.list.len;
                        v->u.list.len *= rep;
                        v->u.list.data = realloc(v->u.list.data, v->u.list.len * sizeof(v->u.list.data[0]));
                        if (!v->u.list.data) err_msg_out_of_memory();
                        while (--rep) {
                            for (j = 0;j < len; j++, i++) {
                                v->u.list.data[i] = val_reference(v->u.list.data[j]);
                            }
                        }
                    } else {
                        if (v1->u.list.len && rep) {
                            vals = malloc(v1->u.list.len * rep * sizeof(v->u.list.data[0]));
                            if (!vals) err_msg_out_of_memory();
                            while (rep--) {
                                for (j = 0;j < v1->u.list.len; j++, i++) {
                                    vals[i] = val_reference(v1->u.list.data[j]);
                                }
                            }
                        } else vals = NULL;
                        v->type = t1;
                        v->u.list.len = i;
                        v->u.list.data = vals;
                    }
                    return;
                }
            case O_IN:
            case O_CONCAT:err_msg_invalid_oper(op, v1, v2, epoint3); goto errtype;
            default:
                {
                    size_t i = 0;
                    struct value_s **vals;
                    if (v == v1) {
                        if (!v1->u.list.len) return;
                        for (;i < v1->u.list.len; i++) {
                            if (v->u.list.data[i]->refcount != 1) {
                                apply_op2(op, v1->u.list.data[i], v2, &new_value, epoint, epoint2, epoint3);
                                val_replace_template(&v->u.list.data[i], &new_value);
                            } else apply_op2(op, v1->u.list.data[i], v2, v->u.list.data[i], epoint, epoint2, epoint3);
                        }
                    } else {
                        if (v1->u.list.len) {
                            vals = malloc(v1->u.list.len * sizeof(v->u.list.data[0]));
                            if (!vals) err_msg_out_of_memory();
                            for (;i < v1->u.list.len; i++) {
                                vals[i] = val_alloc();
                                vals[i]->refcount = 1;
                                apply_op2(op, v1->u.list.data[i], v2, vals[i], epoint, epoint2, epoint3);
                            }
                        } else vals = NULL;
                        v->type = t1;
                        v->u.list.len = i;
                        v->u.list.data = vals;
                    }
                    return;
                }
            }
        }
    }
    if (t1 == T_GAP) {
        if (t1 == t2) {
            switch (op) {
            case O_GE:
            case O_LE:
            case O_EQ: v->type = T_BOOL; v->u.num.val = 1;return;
            case O_LT:
            case O_GT:
            case O_NEQ: v->type = T_BOOL; v->u.num.val = 0;return;
            default:break;
            }
        }
    }

    if (t1 != t2) {
        if (type_is_num(t1) || t1 == T_STR || t1 == T_LIST || t1 == T_TUPLE || t1 == T_CODE || t1 == T_ADDRESS || t1 == T_GAP) {
            if (type_is_num(t2) || t2 == T_STR || t2 == T_LIST || t2 == T_TUPLE || t2 == T_CODE || t2 == T_ADDRESS || t2 == T_GAP) {
                switch (op) {
                case O_EQ: v->type = T_BOOL; v->u.num.val = 0;return;
                case O_NEQ: v->type = T_BOOL; v->u.num.val = 1;return;
                case O_LT: v->type = T_BOOL; v->u.num.val = (t1 < t2);return;
                case O_GT: v->type = T_BOOL; v->u.num.val = (t1 > t2);return;
                case O_LE: v->type = T_BOOL; v->u.num.val = (t1 <= t2);return;
                case O_GE: v->type = T_BOOL; v->u.num.val = (t1 >= t2);return;
                case O_IN:
                           if (t2 == T_LIST || t2 == T_TUPLE || t2 == T_CODE) {
                               inlist(v1, v2, v, epoint, epoint2, epoint3);return;
                           }
                default:err_msg_invalid_oper(op, v1, v2, epoint3);
                        v->type = T_NONE;
                        return;
                }
            }
        }
    }

    if (t1 == T_ERROR) {
        if (v != v1) {
            v->type = T_ERROR; 
            v->u.error = v1->u.error;
        }
        return;
    }
    else if (t2 == T_ERROR) {
        if (v == v1) val_destroy2(v1);
        v->type = T_ERROR; 
        v->u.error = v2->u.error;
        return;
    } 
    else err_msg_invalid_oper(op, v1, v2, epoint3);
    goto errtype;
}

static int get_val2(struct eval_context_s *ev) {
    size_t vsp = 0;
    size_t i;
    enum oper_e op;
    struct values_s *v1, *v2;
    enum type_e t1, t2;
    int stop = ev->gstop == 3 || ev->gstop == 4;
    struct values_s *o_out;
    struct value_s *val;
    struct values_s *values;

    if (ev->outp2 >= ev->outp) return 1;
    values = ev->values;

    for (i = ev->outp2; i < ev->outp; i++) {
        o_out = &ev->o_out[i];
        val = o_out->val;
        if (val->type != T_OPER || val == &o_PARENT || val == &o_BRACKET) {
            if (vsp >= ev->values_size) {
                size_t j = ev->values_size;
                ev->values_size += 16;
                ev->values = values = realloc(values, sizeof(struct values_s)*ev->values_size);
                if (!values) err_msg_out_of_memory();
                for (; j < ev->values_size; j++) ev->values[j].val = &none_value;
            }
            val_destroy(values[vsp].val);
            values[vsp].val = val;
            o_out->val = &none_value;
            values[vsp++].epoint = o_out->epoint;
            continue;
        }

        if (val == &o_SEPARATOR) {
            ev->outp2 = i + 1;
            return 0;
        }
        if (val == &o_COMMA || val == &o_COLON2 || val == &o_COLON3) continue;
        op = val->u.oper.op;
        if (vsp == 0) goto syntaxe;
        v1 = &values[vsp-1];
        switch (op) {
        case O_MEMBER:
            {
                struct value_s *vv1;
                int rec = 100;
                v2 = v1; vsp--;
                if (vsp == 0) goto syntaxe;
                v1 = &values[vsp-1];
                if (v1->val->type == T_NONE) continue;
                try_resolv_ident(v1);
                vv1 = v1->val;
            membretr:
                if (vv1->type == T_IDENTREF) {
                    if (!vv1->u.identref->nested) {
                        if (touch_label(vv1->u.identref)) {
                            vv1->u.error.u.ident = vv1->u.identref->name;
                            vv1->type = T_ERROR;
                            vv1->u.error.num = ERROR___NOT_DEFINED;
                            vv1->u.error.epoint = v1->epoint;
                            continue;
                        }
                        vv1 = vv1->u.identref->value;
                        if (vv1->type == T_NONE) goto errtype;
                        if (rec--) goto membretr;
                        err_msg2(ERROR__REFRECURSION, NULL, v1->epoint);
                        goto errtype;
                    }
                    if (v2->val->type == T_IDENT) {
                        new_value.u.identref = find_label2(&v2->val->u.ident, vv1->u.identref);
                        if (new_value.u.identref) {
                            new_value.type = T_IDENTREF;
                            touch_label(vv1->u.identref);
                        } else {
                            new_value.type = T_ERROR;
                            new_value.u.error.num = ERROR___NOT_DEFINED;
                            new_value.u.error.epoint = v2->epoint;
                            new_value.u.error.u.ident = v2->val->u.ident;
                        }
                        val_replace(&v1->val, &new_value);
                        v1->epoint=v2->epoint;
                        continue;
                    } else err_msg_invalid_oper(op, vv1, v2->val, v2->epoint);
                } else if (vv1->type == T_ERROR) continue;
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
                while (v1->val->type != T_OPER || v1->val->u.oper.op != op) {
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
        case O_LIST:
            {
                unsigned int args = 0, args2, tup = (op == O_RPARENT), expl = (op == O_TUPLE || op == O_LIST);
                op = (op == O_RBRACKET || op == O_LIST) ? O_BRACKET : O_PARENT;
                while (v1->val->type != T_OPER || v1->val->u.oper.op != op) {
                    args++;
                    if (vsp <= args) goto syntaxe;
                    v1 = &values[vsp-1-args];
                }
                if (args == 1) {
                    if (stop && !expl) {
                        size_t j = i + 1;
                        if (tup && j < ev->outp && (ev->o_out[j].val->type != T_OPER || (
                                        ev->o_out[j].val != &o_SEPARATOR && /* (3),2 */
                                        ev->o_out[j].val != &o_RPARENT &&   /* ((3)) */
                                        ev->o_out[j].val != &o_RBRACKET &&  /* [(3)] */
                                        ev->o_out[j].val != &o_FUNC &&      /* f((3)) */
                                        ev->o_out[j].val != &o_LIST &&      /* [(3),] */
                                        ev->o_out[j].val != &o_COMMA &&     /* [(3),(3)] */
                                        ev->o_out[j].val != &o_COMMAY &&    /* (3),y */
                                        ev->o_out[j].val != &o_COMMAZ       /* (3),z */
                                        ))) {
                            val_replace(&v1->val, values[vsp-1].val); vsp--;continue;
                        }
                        try_resolv(&values[vsp-1]);
                        if (values[vsp-1].val->type == T_STR) {
                            str_to_num(values[vsp-1].val, T_NUM, &new_value, values[vsp-1].epoint);
                            val_replace_template(&values[vsp-1].val, &new_value);
                        }
                        if (type_is_num(values[vsp-1].val->type) || values[vsp-1].val->type == T_CODE) {
                            uint8_t len = (values[vsp-1].val->type == T_NUM) ? values[vsp-1].val->u.num.len : 8*sizeof(address_t);
                            val = val_realloc(&v1->val);
                            val->type = T_ADDRESS;
                            val->u.addr.val = to_ival(values[vsp-1].val);
                            val->u.addr.len = len;
                            val->u.addr.type = (op == O_BRACKET) ? A_LI : A_I;
                            vsp--; continue;
                        }
                        if (values[vsp-1].val->type == T_ADDRESS) {
                            val = val_realloc(&v1->val);
                            val->type = T_ADDRESS;
                            val->u.addr.val = values[vsp-1].val->u.addr.val;
                            val->u.addr.len = values[vsp-1].val->u.addr.len;
                            val->u.addr.type = (values[vsp-1].val->u.addr.type << 4) | ((op == O_BRACKET) ? A_LI : A_I);
                            vsp--; continue;
                        }
                    } else if (tup) {
                        val_replace(&v1->val, values[vsp-1].val); vsp--;continue;
                    }
                }
                val = val_realloc(&v1->val);
                val->type = (op == O_BRACKET) ? T_LIST : T_TUPLE;
                val->u.list.len = args;
                if (args) {
                    args2 = args;
                    val->u.list.data = malloc(args * sizeof(val->u.list.data[0]));
                    if (!val->u.list.data) err_msg_out_of_memory();
                    while (args--) {
                        try_resolv(&values[vsp-1]);
                        if (values[vsp-1].val->type == T_ERROR) {
                            while (args2 > args + 1) {
                                args2--;
                                val_destroy(val->u.list.data[args2]);
                            }
                            free(val->u.list.data);
                            val->type = T_ERROR;
                            val->u.error = values[vsp-1].val->u.error;
                            vsp -= args + 1;
                            break;
                        } 
                        val->u.list.data[args] = values[vsp-1].val;
                        values[vsp-1].val = &none_value;
                        vsp--;
                    }
                } else val->u.list.data = NULL;
                continue;
            }
        case O_COND:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1]; vsp--;
            if (vsp == 0) goto syntaxe;
            switch (try_resolv(&values[vsp-1])) {
            case T_CODE: err_msg_strange_oper(op, v1->val, NULL, v1->epoint);
            case T_STR:
            case T_UINT:
            case T_SINT: 
            case T_NUM:
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
        case O_QUEST:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1];
            err_msg2(ERROR______EXPECTED,"':'", o_out->epoint);
            goto errtype;
        case O_COLON:
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1];
            err_msg2(ERROR______EXPECTED,"'?'", o_out->epoint);
            goto errtype;
        case O_WORD:    /* <> */
        case O_HWORD:   /* >` */
        case O_BSWORD:  /* >< */
        case O_LOWER:   /* <  */
        case O_HIGHER:  /* >  */
        case O_BANK:    /* `  */
        case O_STRING:  /* ^  */
        case O_HASH:    /* #  */
        case O_COMMAX:  /* ,x */
        case O_COMMAY:  /* ,y */
        case O_COMMAZ:  /* ,z */
        case O_COMMAR:  /* ,r */
        case O_COMMAS:  /* ,s */
        case O_INV:     /* ~  */
        case O_NEG:     /* -  */
        case O_POS:     /* +  */
            {
                try_resolv_ident(v1);
                if (v1->val->refcount != 1) {
                    apply_op(op, v1->val, &new_value, v1->epoint, o_out->epoint);
                    val_replace_template(&v1->val, &new_value);
                } else apply_op(op, v1->val, v1->val, v1->epoint, o_out->epoint);
                v1->epoint = o_out->epoint;
                continue;
            }
        case O_LNOT:
            switch (try_resolv(v1)) {
            case T_CODE: err_msg_strange_oper(op, v1->val, NULL, v1->epoint);
            case T_SINT:
            case T_NUM: 
            case T_BOOL:
            case T_UINT:
            case T_STR:
            case T_FLOAT:
            case T_LIST:
            case T_TUPLE:
                val_replace(&v1->val, val_truth(v1->val) ? &false_value : &true_value);
                v1->epoint = o_out->epoint;
                continue;
            default: err_msg_invalid_oper(op, v1->val, NULL, v1->epoint);
                     goto errtype;
            case T_ERROR:
            case T_NONE: continue;
            }
        case O_LAND:
        case O_LOR:
        case O_LXOR:
            v2 = v1; v1 = &values[--vsp-1];
            if (vsp == 0) goto syntaxe;
            t1 = try_resolv(v1);
            switch (t1) {
            case T_SINT:
            case T_CODE:
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
                t2 = try_resolv(v2);
                switch (t2) {
                case T_SINT:
                case T_CODE:
                case T_NUM: 
                case T_BOOL:
                case T_UINT:
                case T_STR:
                case T_FLOAT:
                case T_LIST:
                case T_TUPLE:
                    if (val_truth(v1->val)) {
                        if (val_truth(v2->val)) val_replace(&v1->val, &false_value);
                    } else {
                        val_replace(&v1->val, val_truth(v2->val) ? v2->val : &false_value);
                    }
                    continue;
                case T_NONE:
                case T_ERROR:
                    val_replace(&v1->val, v2->val);
                    continue;
                default: err_msg_invalid_oper(op, v1->val, v2->val, v2->epoint); 
                         goto errtype;
                }
            default: err_msg_invalid_oper(op, v1->val, v2->val, v1->epoint); 
                     goto errtype;
            case T_ERROR:
            case T_NONE: continue;
            }
        default: break;
        }
        v2 = v1; v1 = &values[--vsp-1];
        if (vsp == 0) {
        syntaxe:
            err_msg(ERROR_EXPRES_SYNTAX,NULL);
            ev->outp2 = ev->outp;
            return -1;
        }
        try_resolv_ident(v2);
        try_resolv_ident(v1);

        if (v1->val->refcount != 1) {
            apply_op2(op, v1->val, v2->val, &new_value, v1->epoint, v2->epoint, o_out->epoint);
            val_replace_template(&v1->val, &new_value);
        } else apply_op2(op, v1->val, v2->val, v1->val, v1->epoint, v2->epoint, o_out->epoint);
    }
    ev->outp2 = i;
    return 0;
}

/* 0 - normal */
/* 1 - 1 only, till comma */
/* 2 - 1 only, till space  */
/* 3 - opcode */
/* 4 - opcode, with defaults */

int get_exp(int *wd, int stop) {/* length in bytes, defined */
    int cd;
    char ch;

    struct value_s *o_oper[256], *op;
    linepos_t epoints[256];
    uint8_t operp = 0, prec, db;
    linepos_t epoint;
    struct value_s *val;
    size_t llen;

    eval->gstop = stop;
    eval->outp2 = 0;

    if (arguments.tasmcomp) {
        return get_exp_compat(wd, stop);
    }
    eval->outp = 0;
    o_oper[0] = &o_SEPARATOR;

    *wd=3;    /* 0=byte 1=word 2=long 3=negative/too big */
    cd=0;     /* 0=error, 1=ok, 2=(a, 3=(), 4=[] */

    ignore();
    switch (here()) {
    case 0:
    case ';': err_msg(ERROR_MISSING_ARGUM,NULL); return 0;
    case '@':
        switch (pline[++lpoint.pos] | 0x20) {
        case 'b':*wd=0;break;
        case 'w':*wd=1;break;
        case 'l':*wd=2;break;
        default:err_msg(ERROR______EXPECTED,"@B or @W or @L"); return 0;
        }
        lpoint.pos++;
        break;
    }
    for (;;) {
        ignore(); ch = here(); epoint = lpoint;
        switch (ch) {
        case ',':
            if (stop != 4 || operp) goto tryanon;
            lpoint.pos++;val = push(epoint);val->type = T_DEFAULT;
            continue;
        case ')':
            if (operp) {
                if (o_oper[operp-1] == &o_COMMA) {operp--;op = &o_TUPLE;goto tphack;}
                else if (o_oper[operp-1] == &o_PARENT || o_oper[operp-1] == &o_FUNC) goto other;
            }
            goto tryanon;
        case ']':
            if (operp) { 
                if (o_oper[operp-1] == &o_COMMA) {operp--;op = &o_LIST;goto lshack;}
                else if (o_oper[operp-1] == &o_COLON3) {operp--;goto other;}
                else if (o_oper[operp-1] == &o_BRACKET) goto other;
            }
            goto tryanon;
        case ':':
            if (operp && o_oper[operp-1] == &o_INDEX) {
                val = push(epoint);
                val->type = T_DEFAULT;
                goto other;
            } else if (operp > 1 && o_oper[operp-1] == &o_COLON3 && o_oper[operp-2] == &o_SLICE) {
                val = push(epoint);
                val->type = T_DEFAULT;
                goto other;
            }
            goto syntaxe;
        case '(': 
            epoints[operp]=epoint;
            o_oper[operp++] = &o_PARENT; lpoint.pos++;
            push_oper(&o_PARENT, epoint);
            continue;
        case '[':
            epoints[operp]=epoint;
            o_oper[operp++] = &o_BRACKET; lpoint.pos++;
            push_oper(&o_BRACKET, epoint);
            continue;
        case '+': op = &o_POS; break;
        case '-': op = &o_NEG; break;
        case '!': op = &o_LNOT;break;
        case '~': op = &o_INV; break;
        case '<': if (pline[lpoint.pos+1] == '>') {lpoint.pos++;op = &o_WORD;} else op = &o_LOWER; break;
        case '>': if (pline[lpoint.pos+1] == '`') {lpoint.pos++;op = &o_HWORD;} else if (pline[lpoint.pos+1] == '<') {lpoint.pos++;op = &o_BSWORD;} else op = &o_HIGHER; break;
        case '#': op = &o_HASH; break;
        case '`': op = &o_BANK; break;
        case '^': op = &o_STRING; break;
        case '$': lpoint.pos++;val = push(epoint);get_hex(val);goto other;
        case '%': lpoint.pos++;val = push(epoint);get_bin(val);goto other;
        case '"':
        case '\'': lpoint.pos++;val = push(epoint);get_string(val, ch);goto other;
        case '*': lpoint.pos++;val = push(epoint);get_star(val);goto other;
        case '?': lpoint.pos++;val = push(epoint);val->type = T_GAP;goto other;
        case '.': if ((uint8_t)(pline[lpoint.pos+1] ^ 0x30) < 10) {val = push(epoint);goto pushfloat;}
        default: 
            if (ch>='0' && ch<='9') {
                val = push(epoint);
                if (get_dec(val)) {
                pushfloat:
                    lpoint = epoint;
                    get_float(val);
                } else if ((here() == '.' && pline[lpoint.pos + 1] != '.') || (here() | 0x20) == 'e') goto pushfloat;
                goto other;
            }
            if (get_label()) {
                val = push(epoint);
                val->type = T_IDENT;
                val->u.ident.data = pline + epoint.pos;
                val->u.ident.len = lpoint.pos - epoint.pos;
                goto other;
            }
        tryanon:
            db = operp;
            while (operp && o_oper[operp-1] == &o_POS) operp--;
            if (db != operp) {
                val = push(epoints[operp]);
                val->type = T_FORWR;
                val->u.ref = db - operp;
                goto other;
            }
            while (operp && o_oper[operp-1] == &o_NEG) operp--;
            if (db != operp) {
                val = push(epoints[operp]);
                val->type = T_BACKR;
                val->u.ref = db - operp;
                goto other;
            }
            goto syntaxe;
        }
        lpoint.pos++;
        prec = op->u.oper.prio;
        while (operp && prec < o_oper[operp-1]->u.oper.prio) {
            operp--;
            push_oper(o_oper[operp], epoints[operp]);
        }
        epoints[operp] = epoint;
        o_oper[operp++] = op;
        continue;
    other:
        if (stop != 2 || (operp && (o_oper[operp-1] == &o_PARENT || o_oper[operp-1] == &o_FUNC))) ignore();
        ch = here();epoint = lpoint;
        switch (ch) {
        case ',':
            lpoint.pos++;
            llen = get_label();
            if (llen == 1) {
                switch (pline[epoint.pos + 1] | 0x20) {
                case 'x': op = &o_COMMAX;llen=0;break;
                case 'y': op = &o_COMMAY;llen=0;break;
                case 'z': op = &o_COMMAZ;llen=0;break;
                case 'r': op = &o_COMMAR;llen=0;break;
                case 's': op = &o_COMMAS;llen=0;break;
                default: op = &o_COMMA;break;
                }
            } else op = &o_COMMA;
            prec = op->u.oper.prio;
            while (operp && prec <= o_oper[operp-1]->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            if (op != &o_COMMA) {
                epoints[operp] = epoint;
                o_oper[operp++] = op;
                goto other;
            }
            if (!operp) {
                push_oper(&o_SEPARATOR, epoint);
                if (stop == 1 || stop == 4) {lpoint = epoint;break;}
            } else {
                push_oper(&o_COMMA, epoint);
                epoints[operp] = epoint;
                o_oper[operp++] = op;
            }
            if (llen) {
                epoint.pos++;
                val = push(epoint);
                val->type = T_IDENT;
                val->u.ident.data = pline + epoint.pos;
                val->u.ident.len = llen;
                goto other;
            }
            continue;
        case '(':
            prec = o_MEMBER.u.oper.prio;
            while (operp && prec <= o_oper[operp-1]->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            push_oper(&o_PARENT, epoint);
            epoints[operp]=epoint;
            o_oper[operp++] = &o_FUNC; lpoint.pos++;
            continue;
        case '[':
            prec = o_MEMBER.u.oper.prio;
            while (operp && prec <= o_oper[operp-1]->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            push_oper(&o_BRACKET, epoint);
            epoints[operp] = epoint;
            o_oper[operp++] = &o_INDEX; lpoint.pos++;
            continue;
        case '&': if (pline[lpoint.pos+1] == '&') {lpoint.pos+=2;op = &o_LAND;} else {lpoint.pos++;op = &o_AND;} goto push2;
        case '|': if (pline[lpoint.pos+1] == '|') {lpoint.pos+=2;op = &o_LOR;} else {lpoint.pos++;op = &o_OR;} goto push2;
        case '^': if (pline[lpoint.pos+1] == '^') {lpoint.pos+=2;op = &o_LXOR;} else {lpoint.pos++;op = &o_XOR;} goto push2;
        case '*': if (pline[lpoint.pos+1] == '*') {lpoint.pos+=2;op = &o_EXP;} else {lpoint.pos++;op = &o_MUL;} goto push2;
        case '%': lpoint.pos++;op = &o_MOD; goto push2;
        case '/': if (pline[lpoint.pos+1] == '/') {lpoint.pos+=2;op = &o_MOD;} else {lpoint.pos++;op = &o_DIV;} goto push2;
        case '+': lpoint.pos++;op = &o_ADD; goto push2;
        case '-': lpoint.pos++;op = &o_SUB; goto push2;
        case '.': if (pline[lpoint.pos+1] == '.') {lpoint.pos+=2;op = &o_CONCAT;} else {lpoint.pos++;op = &o_MEMBER;} goto push2;
        case '?': lpoint.pos++;op = &o_QUEST; prec = o_COND.u.oper.prio + 1; goto push3;
        case ':': op = &o_COLON;
            prec = op->u.oper.prio + 1;
            while (operp && prec <= o_oper[operp-1]->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            if (operp && o_oper[operp-1] == &o_INDEX) { o_oper[operp-1] = &o_SLICE; op = &o_COLON3;}
            else if (operp && o_oper[operp-1] == &o_SLICE) { o_oper[operp-1] = &o_SLICE2; op = &o_COLON3;}
            else if (operp && o_oper[operp-1] == &o_QUEST) { o_oper[operp-1] = &o_COND; op = &o_COLON2;}
            epoints[operp] = epoint;
            o_oper[operp++] = op;
            lpoint.pos++;
            continue;
        case '=': op = &o_EQ; if (pline[lpoint.pos+1] == '=') lpoint.pos+=2; else lpoint.pos++;
        push2:
            prec = op->u.oper.prio;
        push3:
            while (operp && prec <= o_oper[operp-1]->u.oper.prio) {
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            epoints[operp] = epoint;
            o_oper[operp++] = op;
            continue;
        case '<': 
            switch (pline[lpoint.pos+1]) {
            case '>': lpoint.pos+=2;op = &o_NEQ; break;
            case '<': lpoint.pos+=2;op = &o_LSHIFT; break;
            case '=': lpoint.pos+=2;op = &o_LE; break;
            default: lpoint.pos++;op = &o_LT; break;
            }
            goto push2;
        case '>':
            switch (pline[lpoint.pos+1]) {
            case '<': lpoint.pos+=2;op = &o_NEQ; break;
            case '>': lpoint.pos+=2;if (pline[lpoint.pos] == '>') {lpoint.pos++;op = &o_RSHIFT;} else op = &o_ASHIFT; break;
            case '=': lpoint.pos+=2;op = &o_GE; break;
            default: lpoint.pos++;op = &o_GT; break;
            }
            goto push2;
        case '!':
            if (pline[lpoint.pos+1]=='=') {lpoint.pos+=2;op = &o_NEQ;goto push2;}
            goto syntaxe;
        case ')':
            op = &o_RPARENT;
        tphack:
            while (operp && o_oper[operp-1] != &o_PARENT && o_oper[operp-1] != &o_FUNC) {
                if (o_oper[operp-1] == &o_BRACKET || o_oper[operp-1] == &o_INDEX || o_oper[operp-1] == &o_SLICE || o_oper[operp-1] == &o_SLICE2) {err_msg(ERROR______EXPECTED,"("); goto error;}
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            lpoint.pos++;
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            operp--;
            push_oper((o_oper[operp] == &o_PARENT)? op : o_oper[operp], epoints[operp]);
            goto other;
        case ']':
            op = &o_RBRACKET;
        lshack:
            while (operp && o_oper[operp-1] != &o_BRACKET && o_oper[operp-1] != &o_INDEX && o_oper[operp-1] != &o_SLICE && o_oper[operp-1] != &o_SLICE2) {
                if (o_oper[operp-1] == &o_PARENT || o_oper[operp-1] == &o_FUNC) {err_msg(ERROR______EXPECTED,"["); goto error;}
                operp--;
                push_oper(o_oper[operp], epoints[operp]);
            }
            lpoint.pos++;
            if (!operp) {err_msg(ERROR______EXPECTED,"["); goto error;}
            operp--;
            push_oper((o_oper[operp] == &o_BRACKET) ? op : o_oper[operp], epoints[operp]);
            goto other;
        case 0:
        case ';': break;
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
            if (o_oper[operp-1] == &o_PARENT || o_oper[operp-1] == &o_FUNC) {err_msg(ERROR______EXPECTED,")"); goto error;}
            if (o_oper[operp-1] == &o_BRACKET || o_oper[operp-1] == &o_INDEX || o_oper[operp-1] == &o_SLICE || o_oper[operp-1] == &o_SLICE2) {err_msg(ERROR______EXPECTED,"]"); goto error;}
            operp--;
            push_oper(o_oper[operp], epoints[operp]);
        }
        if (!operp) {cd=1;break;}
    syntaxe:
        err_msg(ERROR_EXPRES_SYNTAX,NULL);
    error:
        return 0;
    }
    return cd;
}

int get_exp_var(void) {
    int w;
    return get_exp(&w, 2);
}

struct value_s *get_vals_tuple(enum type_e type) {
    size_t ln = 0, i = 0;
    struct value_s **vals = NULL, *retval = NULL, *val;
    linepos_t epoint;
    while ((val = get_val(type, &epoint))) {
        if (i) {
            if (i >= ln) {
                ln += 16;
                vals = realloc(vals, ln * sizeof(retval->u.list.data[0]));
                if (!vals) err_msg_out_of_memory();
            }
            if (i == 1) {
                struct values_s vals2 = {retval, epoint};
                try_resolv_identref(&vals2);
                if (vals2.val->type == T_NONE) {
                    free(vals);
                    return vals2.val;
                }
                vals[0] = vals2.val;
                type = T_NONE;
            }
            if (val->type == T_NONE) {
                val_destroy(val);
                while (--i) val_destroy(vals[i]);
                free(vals);
                return &none_value;
            }
            vals[i] = val;
            eval->values->val = &none_value;
        } else {
            retval = val;
            eval->values->val = &none_value;
        }
        i++;
    }
    eval_finish();
    if (i > 1) {
        retval = val_alloc();
        retval->refcount = 1;
        retval->type = T_TUPLE;
        retval->u.list.len = i;
        if (i != ln) {
            vals = realloc(vals, i * sizeof(val->u.list.data[0]));
            if (!vals) err_msg_out_of_memory();
        }
        retval->u.list.data = vals;
    }
    return retval;
}

void eval_enter(void) {
    evx_p++;
    if (evx_p >= evxnum) {
        evxnum++;
        evx = (struct eval_context_s **)realloc(evx, evxnum * sizeof(struct eval_context_s *));
        if (!evx) err_msg_out_of_memory();
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
    evx_p = -1;
    eval_enter();
}

void destroy_eval(void) {
    while (evxnum--) {
        eval = evx[evxnum];
        while (eval->out_size--) val_destroy(eval->o_out[eval->out_size].val);
        while (eval->values_size--) val_destroy(eval->values[eval->values_size].val);
        free(eval->values);
        free(eval->o_out);
        free(eval);
    }
    free(evx);
}
