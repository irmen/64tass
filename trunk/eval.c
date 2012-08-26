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
#include "file.h"
#include "error.h"
#include "section.h"
#include "encoding.h"

static struct value_s new_value = {T_NONE, 0, {}};
static struct value_s none_value = {T_NONE, 0, {}};
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

static int str_to_num(struct value_s **v2, enum type_e type) {
    uint16_t ch;
    unsigned int large = 0;
    size_t i = 0;
    uval_t val = 0;
    struct value_s *v = *v2;

    if (actual_encoding) {
        while (v->u.str.len > i) {
            if (large >= (arguments.tasmcomp ? 1 : sizeof(val))) {
                val_replace(v2, &none_value);return 1;
            }

            ch = petascii(&i, v);
            if (ch > 255) {
                val_replace(v2, &none_value);return 1;
            }

            val |= (uint8_t)ch << (8 * large);
            large++;
        }
    } else if (v->u.str.len) {
        uint32_t ch;

        ch = v->u.str.data[0];
        if (ch & 0x80) i = utf8in(v->u.str.data, &ch); else i=1;

        if (v->u.str.len > i) {
            val_replace(v2, &none_value);return 1;
        }
        val = ch;
    } else {
        val_replace(v2, &none_value);return 1;
    }
    new_value.type = type;
    new_value.u.num.val = val;
    new_value.u.num.len = large | (!large);
    val_replace(v2, &new_value);
    return 0;
}

static void get_string(struct value_s *v, uint8_t ch) {
    unsigned int i;
    uint32_t ch2;

    i = lpoint;
    for (;;) {
        if (!(ch2 = here())) {err_msg(ERROR______EXPECTED,"End of string"); v->type = T_NONE; return;}
        if (ch2 & 0x80) lpoint += utf8in(pline + lpoint, &ch2); else lpoint++;
        if (ch2 == ch) {
            if (here() == ch && !arguments.tasmcomp) lpoint++; // handle 'it''s'
            else break; // end of string;
        }
    }
    v->type = T_STR;
    v->u.str.len = lpoint - i - 1;
    v->u.str.data = (uint8_t *)pline + i;
    return;
}

static enum type_e touch_label(struct label_s *tmp) {

    if (tmp) {
        tmp->ref=1;tmp->pass=pass;
        if (tmp->type != L_VAR || tmp->upass==pass) return T_IDENTREF;
    }
    return T_UNDEF;
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
    case T_FORWR: sprintf(ident,"+%x+%x", reffile, forwr + val->u.ref - 1); break;
    case T_BACKR: sprintf(ident,"-%x-%x", reffile, backr - val->u.ref); break;
    case T_IDENT: copy_name(val, ident); break;
    default: return;
    }
    new_value.u.label = find_label(ident);
    new_value.type = touch_label(new_value.u.label);
    val_replace(val2, &new_value);
}

static void try_resolv_identref(struct value_s **val2) {
    struct value_s *val = val2[0];
    if (val->type == T_IDENTREF) {
        if (pass != 1) {
            if ((val->u.label->requires & current_section->provides)!=val->u.label->requires) err_msg(ERROR_REQUIREMENTS_,val->u.label->name);
            if (val->u.label->conflicts & current_section->provides) err_msg(ERROR______CONFLICT,val->u.label->name);
        }
        val_replace(val2, val->u.label->value);
    } else if (val->type == T_UNDEF && pass == 1) val_replace(val2, &none_value);
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
static int priority(char ch)
{
    switch (ch) {
    default:
    case 'F':          // a(
    case 'I':          // a[
    case '[':          // [a]
    case '(':return 0; // (a)
    case '?':          // ?
    case ':':return 1; // :
    case ',':return 2;
    case 'l':          // <
    case 'h':          // >
    case 'H':          // `
    case 'S':return 3; // ^
    case 'O':return 4; // ||
    case 'X':return 5; // ^^
    case 'A':return 6; // &&
    case '=':
    case 'o':          // !=
    case '<':
    case '>':
    case 'g':          // >=
    case 's':return 7; // <=
    case '|':return 8;
    case '^':return 9;
    case '&':return 10;
    case 'm':          // <<
    case 'D':          // >>
    case 'd':return 11; // >>>
    case '+':
    case '-':return 12;
    case '*':
    case '/':
    case '%':return 13;// %
    case 'E':return 14;// **
    case '.':return 15;// .
    case 'n':          // -
    case 'p':return 16;// +
    case '~':return 17;// ~
    case '!':return 18;// !
    }
}

static struct values_s *values = NULL;
static size_t values_size=0;
static size_t values_len=0;
static size_t values_p=0;

static int get_exp_compat(int *wd, int stop) {// length in bytes, defined
    int cd;
    unsigned int i;
    char ch, conv;

    static struct {
        struct value_s val;
        unsigned int epoint;
    } o_out[256];
    char o_oper[256] = {0};
    uint8_t outp = 0, operp = 0, vsp;
    int large=0;
    enum type_e t1, t2;
    unsigned int epoint, cpoint = 0;
    values_len = values_p = 0;

    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    cd=0;     // 0=error, 1=ok, 2=(a, 3=()
rest:
    ignore();
    conv = 0;
    switch (here()) {
    case '!':*wd=1;lpoint++;break;
    case '<':
    case '>': conv = here();cpoint = lpoint; lpoint++;break; 
    }
    for (;;) {
        ignore();ch = here(); epoint=lpoint;

        switch (ch) {
        case '(': o_oper[operp++] = ch; lpoint++;continue;
        case '$': lpoint++;if (get_hex(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '%': lpoint++;if (get_bin(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '"': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
        case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
        }
        if (ch>='0' && ch<='9') { if (get_dec(&o_out[outp].val)) goto pushlarge;
        pushval:
            if ((o_out[outp].val.type == T_SINT || o_out[outp].val.type == T_UINT || o_out[outp].val.type == T_NUM) && (o_out[outp].val.u.num.val & ~0xffff)) {
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
	
        while (operp && o_oper[operp-1] != '(') {o_out[outp].val.type = T_OPER; o_out[outp++].val.u.oper=o_oper[--operp];}
        switch (ch) {
        case ',':
            if (conv) {
                o_out[outp].epoint = cpoint;
                o_out[outp].val.type = T_OPER;
                o_out[outp++].val.u.oper = conv;
            }
            if (stop) break;
            o_oper[operp++] = ch;
            lpoint++;
            goto rest;
        case '&':
        case '.':
        case ':':
        case '*':
        case '/':
        case '+':
        case '-': 
            o_oper[operp++] = ch;
            lpoint++;
            continue;
        case ')':
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            lpoint++;
            operp--;
            goto other;
        case 0:
        case ';':
            if (conv) {
                o_out[outp].epoint = cpoint;
                o_out[outp].val.type = T_OPER;
                o_out[outp++].val.u.oper = conv;
            }
            break;
        default: goto syntaxe;
        }
        if (stop && o_oper[0]=='(') {
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
    vsp = 0;
    for (i=0;i<outp;i++) {
	if (o_out[i].val.type != T_OPER) {
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
        ch = o_out[i].val.u.oper;
        if (ch==',') continue;
        if (vsp < 1) goto syntaxe;
        t1 = try_resolv(&values[vsp-1].val);
        if (ch == '<' || ch == '>') {
            switch (t1) {
            case T_STR:
                if (str_to_num(&values[vsp-1].val, T_NUM)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, values[vsp-1].epoint); large=1;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM:
                {
                    uint16_t val1 = values[vsp-1].val->u.num.val;

                    switch (ch) {
                    case '>': val1 >>= 8;
                    case '<': val1 = (uint8_t)val1; break;
                    }
                    set_uint(&new_value, val1);
                    val_replace(&values[vsp-1].val, &new_value);
                    break;
                }
            default:
                err_msg_wrong_type(t1, values[vsp-1].epoint);
                val_replace(&values[vsp-1].val, &none_value); 
            case T_NONE:break;
            }
            values[vsp-1].epoint = o_out[i].epoint;
            continue;
        }
        if (vsp < 2) goto syntaxe;
        t2 = try_resolv(&values[vsp-2].val);

        if (t1 <= T_SINT && t2 == T_STR) {
            if (str_to_num(&values[vsp-2].val, T_NUM)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, values[vsp-2].epoint); large=1;
            }
            t2 = values[vsp-2].val->type;
        }
        if (t2 <= T_SINT && t1 == T_STR) {
            if (str_to_num(&values[vsp-1].val, T_NUM)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, values[vsp-1].epoint); large=1;
            }
            t1 = values[vsp-1].val->type;
        }
        switch (t1) {
        case T_SINT:
        case T_UINT:
        case T_NUM:
            switch (t2) {
            case T_UINT:
            case T_SINT:
            case T_NUM:
                {
                    uint16_t val1 = values[vsp-1].val->u.num.val;
                    uint16_t val2 = values[vsp-2].val->u.num.val;

                    switch (ch) {
                    case '*': val1 *= val2; break;
                    case '/': if (!val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, values[vsp-1].epoint); val1 = 0xffff;large=1;} else val1=val2 / val1; break;
                    case '+': val1 += val2; break;
                    case '-': val1 = val2 - val1; break;
                    case '&': val1 &= val2; break;
                    case '.': val1 |= val2; break;
                    case ':': val1 ^= val2; break;
                    }
                    vsp--;
                    set_uint(&new_value, val1);
                    val_replace(&values[vsp-1].val, &new_value);
                    continue;
                }
            default: err_msg_wrong_type(t2, values[vsp-2].epoint);
            case T_NONE:break;
            }
            break;
        default:
            err_msg_wrong_type(t1, values[vsp-1].epoint);
        case T_NONE:break;
        }
        vsp--; val_replace(&values[vsp-1].val, &none_value); continue;
    }
    if (large) cd=0;
    values_len = vsp;
    return cd;
}

void eval_finish(void) {
    if (values_p >= values_len) return;
    lpoint = values[values_p].epoint;
}

struct value_s *get_val(enum type_e type, unsigned int *epoint) {// length in bytes, defined
    enum type_e type2;

    if (values_p >= values_len) return NULL;

    if (epoint) *epoint = values[values_p].epoint;
    try_resolv(&values[values_p].val);
    type2 = values[values_p].val->type;

    switch (type2) {
    case T_STR:
    case T_SINT:
    case T_UINT:
    case T_NUM:
    case T_GAP:
    case T_LIST:
        if (type == T_NONE) return values[values_p++].val;
        if (type == T_SINT || type == T_UINT || type == T_NUM || type == T_GAP) {
            switch (type2) {
            case T_STR:
                if (str_to_num(&values[values_p].val, (type == T_GAP) ? T_NUM : type)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, values[values_p++].epoint);
                    return &error_value;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM:
                return values[values_p++].val;
            case T_GAP: if (type == T_GAP) return values[values_p++].val;
            default:
                break;
            }
        }
    default:
        err_msg_wrong_type(type2, values[values_p++].epoint);
        return &error_value;
    case T_NONE: break;
    }
    values_p++;
    return &none_value;
}

static void functions(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    size_t len;
    const uint8_t *name;

    if (vals->val->type != T_IDENT) {
        err_msg_wrong_type(vals->val->type, vals->epoint);
        val_replace(&vals->val, &none_value);
        return;
    }
    len = vals->val->u.ident.len;
    name = vals->val->u.ident.name;

    // len(a) - length of string in characters
    if (len == 3 && !memcmp(name, "len", 3)) {
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint); else
        switch (try_resolv(&v[0].val)) {
#if 0
        case T_TSTR:
        case T_STR:
            set_uint(&new_value, v[0].val->u.str.len);
            val_replace(&vals->val, &new_value);
            break;
#endif
        case T_LIST:
            set_uint(&new_value, v[0].val->u.list.len);
            val_replace(&vals->val, &new_value);
            return;
        default: err_msg_wrong_type(v[0].val->type, v[0].epoint);
        case T_NONE: break;
        }
        val_replace(&vals->val, &none_value);
        return;
    }
    // min(a, b, ...) - minimum value
    if (len == 3 && !memcmp(name, "min", 3)) {
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
                    if (volt || ((!t || min > 0) && (uval_t)v[args].val->u.num.val < (uval_t)min)) {min = v[args].val->u.num.val; t = 0;}
                    break;
                default: err_msg_wrong_type(v[args].val->type, v[args].epoint);
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
    else if (len == 3 && !memcmp(name, "max", 3)) {
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
                    if (volt || (t && max < 0) || (uval_t)v[args].val->u.num.val > (uval_t)max) {max = v[args].val->u.num.val;t = 0;}
                    break;
                default: err_msg_wrong_type(v[args].val->type, v[args].epoint);
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
    else if (len == 4 && !memcmp(name, "size", 4)) {
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals->epoint);
        else {
	    try_resolv_ident(&v[0].val);
	    switch (v[0].val->type) {
	    case T_IDENTREF:
                switch (v[0].val->u.label->type) {
                case L_LABEL:
                case L_STRUCT:
                case L_UNION:
                    set_uint(&new_value, v[0].val->u.label->size);
                    val_replace(&vals->val, &new_value);
                    return;
                default: try_resolv_identref(&v[0].val);err_msg_wrong_type(v[0].val->type, v[0].epoint);
                }
                break;
	    case T_NONE:
		return;
            case T_UNDEF:
                if (pass == 1) val_replace(&vals->val, &none_value); else val_replace(&vals->val, v[0].val);
                return;
	    default: err_msg_wrong_type(v[0].val->type, v[0].epoint);
	    }
        }
        val_replace(&vals->val, &none_value);
        return;
    }
    err_msg2(ERROR___NOT_DEFINED,"function", vals->epoint);
    val_replace(&vals->val, &none_value);
}

static void indexes(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];

    switch (try_resolv(&vals->val)) {
    case T_LIST:
        if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, vals[0].epoint); else
            switch (try_resolv(&v[0].val)) {
            case T_UINT:
            case T_SINT:
            case T_NUM:
                {
                    struct value_s *val;
                    if (v[0].val->type != T_SINT || v[0].val->u.num.val >= 0) {
                        if ((uval_t)v[0].val->u.num.val < vals->val->u.list.len) val = val_reference(vals->val->u.list.data[(uval_t)v[0].val->u.num.val]);
                        else {err_msg2(ERROR_CONSTNT_LARGE, NULL, v[0].epoint); val = &none_value;}
                    } else {
                        if ((uval_t)-v[0].val->u.num.val <= vals->val->u.list.len) val = val_reference(vals->val->u.list.data[vals->val->u.list.len + v[0].val->u.num.val]);
                        else {err_msg2(ERROR_CONSTNT_LARGE, NULL, v[0].epoint); val = &none_value;}
                    }
                    val_replace(&vals->val, val); val_destroy(val);
                }
                return;
            default: err_msg_wrong_type(v[0].val->type, v[0].epoint);
            case T_NONE: 
                     val_replace(&vals->val, &none_value);
                     return;
            }
        break;
#if 0
    case T_TSTR:
    case T_STR:
        if (t1 == T_STR && (t2 == T_SINT || t2 == T_UINT || t2 == T_NUM)) {
            if (ch=='I') {
                val=0;
                if (v2->val.u.num.val >= 0) {
                    if ((uval_t)v2->val.u.num.val < v1->val.u.str.len) val = v1->val.u.str.data[v2->val.u.num.val];
                    else err_msg2(ERROR_CONSTNT_LARGE, NULL, v2->epoint);
                } else {
                    if ((uval_t)-v2->val.u.num.val <= v1->val.u.str.len) val = v1->val.u.str.data[v1->val.u.str.len + v2->val.u.num.val];
                    else err_msg2(ERROR_CONSTNT_LARGE, NULL, v2->epoint);
                }
                if (v1->val.type == T_TSTR) free(v1->val.u.str.data);
                v1->val.type = T_CHR; v1->val.u.num.val = val;v1->val.u.num.len = 1;continue;
            }
        }
#endif
    default: err_msg_wrong_type(vals->val->type, vals->epoint);
             val_replace(&vals->val, &none_value);
    case T_NONE: return;
    }
    return;
}

int get_exp(int *wd, int stop) {// length in bytes, defined
    int cd;
    unsigned int i;
    char ch;
    static uint8_t line[linelength];  //current line data

    static struct {
        struct value_s val;
        unsigned int epoint;
    } o_out[256];
    struct values_s *v1, *v2;
    char o_oper[256] = {0};
    unsigned int epoints[256];
    uint8_t outp = 0, operp = 0, vsp, prec, db;
    int large=0;
    int cond=0;
    ival_t val;
    enum type_e t1, t2;
    unsigned int epoint;

    if (arguments.tasmcomp) {
        return get_exp_compat(wd, stop);
    }
    values_len = values_p = 0;

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
        case '(': 
        case '[':
            o_oper[operp++] = ch; lpoint++;
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = ch;
            o_out[outp++].epoint = epoint;
            continue;
        case '+': ch = 'p'; break;
        case '-': ch = 'n'; break;
        case '!': break;
        case '~': break;
        case '<': ch = 'l'; break;
        case '>': ch = 'h'; break;
        case '`': ch = 'H'; break;
        case '^': ch = 'S'; break;
        case '$': lpoint++;if (get_hex(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '%': lpoint++;if (get_bin(&o_out[outp].val)) goto pushlarge;goto pushval;
        case '"':
        case '\'': lpoint++;get_string(&o_out[outp].val, ch);goto pushval;
        case '*': lpoint++;get_star(&o_out[outp].val);goto pushval;
        case '?': lpoint++;o_out[outp].val.type = T_GAP;goto pushval;
        default: 
            if (ch>='0' && ch<='9') {
                if (get_dec(&o_out[outp].val)) {
                pushlarge:
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);large=1;
                }
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
            db = operp;
            while (operp && o_oper[operp-1] == 'p') operp--;
            if (db != operp) {
                o_out[outp].val.u.ref = db - operp;
                o_out[outp].val.type = T_FORWR;
                epoint = epoints[operp];
                goto pushval;
            }
            while (operp && o_oper[operp-1] == 'n') operp--;
            if (db != operp) {
                o_out[outp].val.u.ref = db - operp;
                o_out[outp].val.type = T_BACKR;
                epoint = epoints[operp];
                goto pushval;
            }
            goto syntaxe;
        }
        lpoint++;
        prec = priority(ch);
        while (operp && prec < priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
        epoints[operp] = epoint;
        o_oper[operp++] = ch;
        continue;
    other:
        ignore();ch = here(); epoint = lpoint;
        switch (ch) {
        case ',':
            if (stop) {
                while (operp && o_oper[operp-1] != '(') {
                    if (o_oper[operp-1]=='[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
                }
                if (!operp) break;
                if (operp==1 && o_oper[0]=='(' && (pline[lpoint+1]=='x' || pline[lpoint+1]=='r') && pline[lpoint+2]==')') {
                    outp--;
                    memmove(&o_out[0], &o_out[1], outp * sizeof(o_out[0]));
                    break;
                }
                break;
            }
            goto push2;
        case '(': 
        case '[':
            o_out[outp].val.type = T_OPER;
            o_out[outp].val.u.oper = ch;
            epoint = epoints[operp];
            o_out[outp++].epoint=epoint;
            ch = (ch == '(') ? 'F' : 'I';
            o_oper[operp++] = ch; lpoint++;
            continue;
        case '&': if (pline[lpoint+1] == '&') {lpoint++;ch = 'A';} goto push2;
        case '|': if (pline[lpoint+1] == '|') {lpoint++;ch = 'O';} goto push2;
        case '^': if (pline[lpoint+1] == '^') {lpoint++;ch = 'X';} goto push2;
        case '*': if (pline[lpoint+1] == '*') {lpoint++;ch = 'E';} goto push2;
        case '%': goto push2;
        case '/': if (pline[lpoint+1] == '/') {lpoint++;ch = '%';} goto push2;
        case '+': goto push2;
        case '-': goto push2;
        case '.': goto push2;
        case '?': cond++;goto push2;
        case ':': 
            if (!cond) {err_msg(ERROR______EXPECTED,"?"); goto error;}
            cond--;
            prec = priority(ch);
            while (operp && prec <= priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
            if (o_oper[operp] != '?') {err_msg(ERROR______EXPECTED,"?");goto error;}
            o_oper[operp++] = ch;
            lpoint++;
            continue;
        case '=': if (pline[lpoint+1] == '=') lpoint++;
        push2:
            prec = priority(ch);
            while (operp && prec <= priority(o_oper[operp-1])) {o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];}
            o_oper[operp++] = ch;
            lpoint++;
            continue;
        case '<': 
            switch (pline[lpoint+1]) {
            case '>': lpoint++;ch = 'o'; break;
            case '<': lpoint++;ch = 'm'; break;
            case '=': lpoint++;ch = 's'; break;
            }
            goto push2;
        case '>':
            switch (pline[lpoint+1]) {
            case '<': lpoint++;ch = 'o'; break;
            case '>': lpoint++;if (pline[lpoint+1] == '>') {lpoint++;ch = 'd';} else ch = 'D'; break;
            case '=': lpoint++;ch = 'g'; break;
            }
            goto push2;
        case '!':
            if (pline[lpoint+1]=='=') {lpoint++;ch = 'o';goto push2;}
            goto syntaxe;
        case ')':
            while (operp && o_oper[operp-1] != '(' && o_oper[operp-1] != 'F') {
                if (o_oper[operp-1]=='[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"("); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            lpoint++;
            if (!operp) {err_msg(ERROR______EXPECTED,"("); goto error;}
            operp--;
            o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[operp];o_out[outp++].val.u.oper=(o_oper[operp] == 'F')? 'F' : ')';
            goto other;
        case ']':
            while (operp && o_oper[operp-1] != '[' && o_oper[operp-1] != 'I') {
                if (o_oper[operp-1]=='(' || o_oper[operp-1]=='F') {err_msg(ERROR______EXPECTED,"["); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            lpoint++;
            if (!operp) {err_msg(ERROR______EXPECTED,"["); goto error;}
            operp--;
            o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[operp];o_out[outp++].val.u.oper=(o_oper[operp] == 'I')? 'I' : ']';
            goto other;
        case 0:
        case ';': break;
        default: goto syntaxe;
        }
        if (cond) {err_msg(ERROR______EXPECTED,":"); goto error;}
        if (stop && o_oper[0]=='(') {
            if (!operp) {cd=3;break;}
            if (ch == ',') {
                while (operp && o_oper[operp-1] != '(') {
                    if (o_oper[operp-1]=='[' || o_oper[operp-1]=='I') {err_msg(ERROR______EXPECTED,"("); goto error;}
                    o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
                }
                if (operp==1) {cd=2; break;}
            }
            err_msg(ERROR______EXPECTED,")"); goto error;
        } else if (stop && o_oper[0]=='[') {
            if (!operp) {cd=4;break;}
            err_msg(ERROR______EXPECTED,"]"); goto error;
        } else {
            while (operp) {
                if (o_oper[operp-1] == '(' || o_oper[operp-1] == 'F') {err_msg(ERROR______EXPECTED,")"); goto error;}
                if (o_oper[operp-1] == '[' || o_oper[operp-1] == 'I') {err_msg(ERROR______EXPECTED,"]"); goto error;}
                o_out[outp].val.type = T_OPER;o_out[outp].epoint=epoints[--operp];o_out[outp++].val.u.oper=o_oper[operp];
            }
            if (!operp) {cd=1;break;}
        }
    syntaxe:
        err_msg(ERROR_EXPRES_SYNTAX,NULL);
    error:
        return 0;
    }
    vsp = 0;
    for (i=0;i<outp;i++) {
        ch = o_out[i].val.u.oper;
	if (o_out[i].val.type != T_OPER || ch == '(' || ch == '[') {
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

        if (ch==',') continue;
        if (vsp == 0) goto syntaxe;
        v1 = &values[vsp-1];
        switch (ch) {
        case '.':
            {
                char ident[linelength];
                v2 = v1; vsp--;
                if (vsp == 0) goto syntaxe;
                v1 = &values[vsp-1];
                if (v1->val->type == T_NONE) goto errtype;
                try_resolv_ident(&v1->val);
                if (v1->val->type == T_IDENTREF) {
                    if (v2->val->type == T_IDENT) {
                        copy_name(v2->val, ident);
                        new_value.u.label = find_label2(ident, &v1->val->u.label->members);
                        new_value.type = touch_label(new_value.u.label);
                        val_replace(&v1->val, &new_value);
                        v1->epoint=v2->epoint;
                        continue;
                    } else err_msg_wrong_type(v2->val->type, v2->epoint);
                } else if (v1->val->type != T_UNDEF || pass != 1) err_msg_wrong_type(v1->val->type, v1->epoint);
                goto errtype;
            }
        case 'F':
        case 'I':
            {
                unsigned int args = 0;
                ch = (ch == 'F') ? '(' : '[';
                while (v1->val->type != T_OPER || v1->val->u.oper != ch) {
                    args++;
                    v1 = &values[vsp-1-args];
                }
                if (ch == '(') functions(&values[vsp-2-args], args);
                else indexes(&values[vsp-2-args], args);
                vsp -= args + 1;
                continue;
            }
        case ']':
        case ')':
            {
                unsigned int args = 0;
                ch = (ch == ')') ? '(' : '[';
                while (v1->val->type != T_OPER || v1->val->u.oper != ch) {
                    args++;
                    v1 = &values[vsp-1-args];
                }
                if ((ch == '(' || stop) && args == 1) {val_replace(&v1->val, values[vsp-1].val); vsp--;continue;}
                v1->val->type = T_LIST; // safe, replacing of T_OPER
                v1->val->u.list.len = args;
                v1->val->u.list.data = malloc(args * sizeof(struct value_s));
                if (!v1->val->u.list.data) err_msg_out_of_memory();
                while (args--) {
                    try_resolv(&values[vsp-1].val);
                    v1->val->u.list.data[args] = val_reference(values[vsp-1].val);
                    vsp--;
                }
                continue;
            }
        case ':':
            v2 = v1; vsp--;
            if (vsp == 0) goto syntaxe;
            v1 = &values[vsp-1]; vsp--;
            if (vsp == 0) goto syntaxe;
            switch (try_resolv(&values[vsp-1].val)) {
            case T_STR: val_replace(&values[vsp-1].val, values[vsp-1].val->u.str.len ? v1->val : v2->val);
                break;
            case T_UINT:
            case T_SINT: 
            case T_NUM: val_replace(&values[vsp-1].val, values[vsp-1].val->u.num.val ? v1->val : v2->val);
                break;
            default: err_msg_wrong_type(values[vsp-1].val->type, values[vsp-1].epoint); 
                     goto errtype;
            case T_NONE: 
                break;
            }
            continue;
        case '?':
            if (vsp == 1) goto syntaxe;
            continue;
        }

        switch (ch) {
        case 'l': // <
        case 'h': // >
        case 'H': // `
            switch (try_resolv(&v1->val)) {
            case T_STR:
                if (str_to_num(&v1->val, T_NUM)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM: new_value.type = T_NUM;
                        new_value.u.num.val = (uint8_t)(v1->val->u.num.val >> ((ch == 'l') ? 0 : ((ch == 'h') ? 8 : 16)));
                        new_value.u.num.len = 1;val_replace(&v1->val, &new_value);
                        v1->epoint = o_out[i].epoint;
                        continue;
            default: err_msg_wrong_type(v1->val->type, v1->epoint); 
                     goto errtype;
            case T_NONE: continue;
            }
        case 'S': // ^
            switch (try_resolv(&v1->val)) {
            case T_STR:
                if (str_to_num(&v1->val, T_NUM)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM:
                {
                    sprintf((char *)line, (v1->val->type == T_SINT) ? "%" PRIdval : "%" PRIuval, v1->val->u.num.val);
                    new_value.type = T_STR;
                    new_value.u.str.len = strlen((char *)line);
                    new_value.u.str.data = line;
                    val_replace(&v1->val, &new_value);
                    v1->epoint = o_out[i].epoint;
                }
                continue;
            default: err_msg_wrong_type(v1->val->type, v1->epoint); 
                     goto errtype;
            case T_NONE: continue;
            }
        case 'p': // +
        case 'n': // -
            new_value.type = T_NUM;
            switch (try_resolv(&v1->val)) {
            case T_STR:
                if (str_to_num(&v1->val, T_NUM)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
                }
            case T_UINT:
            case T_SINT:
            case T_NUM: new_value.type = T_SINT;
                        new_value.u.num.val = (ch == 'n') ? (-v1->val->u.num.val) : v1->val->u.num.val;
                        new_value.u.num.len = v1->val->u.num.len;
                        val_replace(&v1->val, &new_value);
                        v1->epoint = o_out[i].epoint;
                        continue;
            default: err_msg_wrong_type(v1->val->type, v1->epoint); 
                     goto errtype;
            case T_NONE: continue;
            }
        case '~':
            switch (try_resolv(&v1->val)) {
            case T_STR:
                if (str_to_num(&v1->val, T_NUM)) {
                    err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
                }
            case T_SINT:
            case T_UINT: 
            case T_NUM: new_value.type = T_NUM;
                        new_value.u.num.val = ~v1->val->u.num.val;
                        new_value.u.num.len = v1->val->u.num.len;
                        val_replace(&v1->val, &new_value);
                        v1->epoint = o_out[i].epoint;
                        continue;
            default: err_msg_wrong_type(v1->val->type, v1->epoint);
                     goto errtype;
            case T_NONE: continue;
            }
        case '!':
            switch (try_resolv(&v1->val)) {
            case T_SINT:
            case T_NUM: 
            case T_UINT: new_value.type = T_UINT;
                         new_value.u.num.val = !v1->val->u.num.val;
                         new_value.u.num.len = 1;
                         val_replace(&v1->val, &new_value);
                         v1->epoint = o_out[i].epoint;
                         continue;
            case T_STR: new_value.type = T_UINT;
                        new_value.u.num.val = !v1->val->u.str.len;
                        new_value.u.num.len = 1;
                        val_replace(&v1->val, &new_value);
                        v1->epoint = o_out[i].epoint;
                        continue;
                continue;
            default: err_msg_wrong_type(v1->val->type, v1->epoint);
                     goto errtype;
            case T_NONE: continue;
            }
        }
        v2 = v1; v1 = &values[--vsp-1];
        if (vsp == 0) goto syntaxe;
        t2 = try_resolv(&v2->val);
        t1 = try_resolv(&v1->val);

        if (t1 == T_NONE || t2 == T_NONE) {
        errtype:
            val_replace(&v1->val, &none_value);
            continue;
        }

        if (t1 <= T_SINT && t2 == T_STR) {
            if (str_to_num(&v2->val, T_NUM)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, v2->epoint); large=1;
            }
            t2 = v2->val->type;
        }
        if (t2 <= T_SINT && t1 == T_STR) {
            if (str_to_num(&v1->val, T_NUM)) {
                err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
            }
            t1 = v1->val->type;
        }
    strretr:

        if ((t1 <= T_SINT && t2 <= T_SINT)) {
            ival_t val1 = v1->val->u.num.val;
            ival_t val2 = v2->val->u.num.val;

            if (t2 < t1) t2 = t1;

            switch (ch) {
            case '=': val1 = ( val1 == val2);break;
            case 'o': val1 = ( val1 != val2);break;
            case '<': val1 = ( val1 <  val2);break;
            case '>': val1 = ( val1 >  val2);break;
            case 's': val1 = ( val1 <= val2);break;
            case 'g': val1 = ( val1 >= val2);break;
            case 'A': val1 = ( val1 && val2);break;
            case 'O': val1 = ( val1 || val2);break;
            case 'X': val1 = (!val1 ^ !val2);break;
            case '*': val1 = ( val1 *  val2);break;
            case '/': if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v2->epoint); val1 = (~(uval_t)0) >> 1; large=1;}
                else if (t2==T_SINT) val1 = ( val1 / val2); else val1 = ( (uval_t)val1 / (uval_t)val2);  break;
            case '%': if (!val2) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v2->epoint); val1 = (~(uval_t)0) >> 1; large=1;}
                else if (t2==T_SINT) val1 = ( val1 % val2); else val1 = ( (uval_t)val1 % (uval_t)val2); break;
            case '+': val1 = ( val1 +  val2);break;
            case '-': val1 = ( val1 -  val2);break;
            case '&': val1 = ( val1 &  val2);break;
            case '|': val1 = ( val1 |  val2);break;
            case '^': val1 = ( val1 ^  val2);break;
            case 'm':
                if (val2 >= (ival_t)sizeof(val1)*8 || val2 <= -(ival_t)sizeof(val1)*8) val1=0;
                else val1 = (val2 > 0) ? (val1 << val2) : (ival_t)((uval_t)val1 >> (-val2));
                break;
            case 'D': 
                if (t1 == T_SINT) {
                    if (val2 >= (ival_t)sizeof(val1)*8) val1 = (val1 > 0) ? 0 : -1;
                    if (val2 <= -(ival_t)sizeof(val1)*8) val1 = 0;
                    else if (val1 >= 0) val1 = (val2 > 0) ? (val1 >> val2) : (val1 << (-val2));
                    else val1 = ~((val2 > 0) ? ((~val1) >> val2) : ((~val1) << (-val2)));
                    break;
                }
            case 'd': 
                if (val2 >= (ival_t)sizeof(val1)*8 || val2 <= -(ival_t)sizeof(val1)*8) val1=0;
                else val1 = (val2 > 0) ? (ival_t)((uval_t)val1 >> val2) : (val1 << (-val2));
                break;
            case 'E': 
                {
                    ival_t res = 1;

                    if (val2 < 0) {
                        if (!val1) {err_msg2(ERROR_DIVISION_BY_Z, NULL, v2->epoint); res = (~(uval_t)0) >> 1; large=1;}
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
            default: err_msg_wrong_type(v1->val->type, v1->epoint); 
                     goto errtype;
            }
            if (t1 == T_SINT) set_int(&new_value, val1); else {set_uint(&new_value, val1); new_value.type = t1;}
            val_replace(&v1->val, &new_value);
            continue;
        }

        if (t1 == T_STR && t2 == T_STR) {
            switch (ch) {
                case '=':
                    val = (v1->val->u.str.len == v2->val->u.str.len) && (v1->val->u.str.data == v2->val->u.str.data || !memcmp(v1->val->u.str.data, v2->val->u.str.data, v1->val->u.str.len));
                strcomp:
                    new_value.type = T_UINT;
                    new_value.u.num.len = 1;
                    new_value.u.num.val = val; 
                    val_replace(&v1->val, &new_value);
                    continue;
                case 'o':
                    val = (v1->val->u.str.len != v2->val->u.str.len) || (v1->val->u.str.data != v2->val->u.str.data && memcmp(v1->val->u.str.data, v2->val->u.str.data, v1->val->u.str.len));
                    goto strcomp;
                case '<':
                    val = memcmp(v1->val->u.str.data, v2->val->u.str.data, (v1->val->u.str.len < v2->val->u.str.len) ? v1->val->u.str.len:v2->val->u.str.len);
                    if (!val) val = v1->val->u.str.len < v2->val->u.str.len; else val = val < 0;
                    goto strcomp;
                case '>':
                    val = memcmp(v1->val->u.str.data, v2->val->u.str.data, (v1->val->u.str.len < v2->val->u.str.len) ? v1->val->u.str.len:v2->val->u.str.len);
                    if (!val) val = v1->val->u.str.len > v2->val->u.str.len; else val = val > 0;
                    goto strcomp;
                case 's':
                    val = memcmp(v1->val->u.str.data, v2->val->u.str.data, (v1->val->u.str.len < v2->val->u.str.len) ? v1->val->u.str.len:v2->val->u.str.len);
                    if (!val) val = v1->val->u.str.len <= v2->val->u.str.len; else val = val <= 0;
                    goto strcomp;
                case 'g':
                    val = memcmp(v1->val->u.str.data, v2->val->u.str.data, (v1->val->u.str.len < v2->val->u.str.len) ? v1->val->u.str.len:v2->val->u.str.len);
                    if (!val) val = v1->val->u.str.len >= v2->val->u.str.len; else val = val >= 0;
                    goto strcomp;
                case 'A': val = (v1->val->u.str.len && v2->val->u.str.len); goto strcomp;
                case 'O': val = (v1->val->u.str.len || v2->val->u.str.len); goto strcomp;
                case 'X': val = (!v1->val->u.str.len ^ !v2->val->u.str.len); goto strcomp;
                case '*': 
                case '/':
                case '%':
                case '+':
                case '-':
                case '&':
                case '|':
                case '^':
                case 'm':
                case 'D': 
                case 'd': 
                case 'E': 
                    if (str_to_num(&v1->val, T_NUM)) {
                        err_msg2(ERROR_CONSTNT_LARGE, NULL, v1->epoint); large=1;
                    }
                    if (str_to_num(&v2->val, T_NUM)) {
                        err_msg2(ERROR_CONSTNT_LARGE, NULL, v2->epoint); large=1;
                    }
                    t1 = v1->val->type;
                    t2 = v2->val->type;
                    goto strretr;
                default: err_msg_wrong_type(v1->val->type, v1->epoint); goto errtype;
            }
        }
        if (t2 == T_UNDEF) err_msg_wrong_type(v2->val->type, v2->epoint); 
        else err_msg_wrong_type(v1->val->type, v1->epoint);
        goto errtype;
    }
    if (large) cd=0;
    values_len = vsp;
    return cd;
}

void free_values(void) {
    while (values_size--) if (values[values_size].val) val_destroy(values[values_size].val);
    free(values);
}
