/*
   Unix snprintf implementation.
   Version 1.3

   Adapted for use in 64tass by Soci/Singular
   $Id: isnprintf.c 3102 2023-09-03 19:40:45Z soci $

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

   Revision History:

   1.3:
      *  add #include <config.h> ifdef HAVE_CONFIG_H
      *  cosmetic change, when exponent is 0 print xxxE+00
         instead of xxxE-00
   1.2:
      *  put the program under LGPL.
   1.1:
      *  added changes from Miles Bader
      *  corrected a bug with %f
      *  added support for %#g
      *  added more comments :-)
   1.0:
      *  supporting must ANSI syntaxic_sugars
   0.0:
      *  suppot %s %c %d

 THANKS(for the patches and ideas):
     Miles Bader
     Cyrille Rustom
     Jacek Slabocewiz
     Mike Parker(mouse)

 Alain Magloire: alainm@rcsm.ee.mcgill.ca
*/

#include "isnprintf.h"
#include <string.h>
#include "unicode.h"
#include "eval.h"
#include "error.h"
#include "str.h"

#include "floatobj.h"
#include "strobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"

#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#ifndef snprintf
#define snprintf(str, size, format, var, var2) sprintf(str, format, var, var2)
#endif
#endif

/* this struct holds everything we need */
typedef struct Data {
    const uint8_t *pf;
    const uint8_t *pfend;
    /* FLAGS */
    int width, precision;
    bool left, square, space, plus, tilde, star_w, star_p, dot, zero;
    argcount_t listp;
    const struct values_s *list;
    argcount_t largs;
    argcount_t none;
    Obj *failure;
    Str *return_value;
    size_t returnsize;
    unichar_t c;
} Data;

/* those are defines specific to snprintf to hopefully
 * make the code clearer :-)
 */
enum { NOT_FOUND = -1 };

static const struct values_s *next_arg(Data *p) {
    const struct values_s *ret;
    if (p->largs > p->listp) {
        Obj *val;
        ret = &p->list[p->listp];
        val = ret->val;
        if (val == none_value) {
            if (p->none == 0) p->none = p->listp + 1;
            ret = NULL;
        } else if (val->obj == ERROR_OBJ) {
            if (p->failure == NULL) p->failure = val_reference(val);
            ret = NULL;
        }
    } else {
        ret = NULL;
    }
    p->listp++;
    return ret;
}

static void put_char(Data *p) {
    uint8_t *d;
    Str *s = p->return_value;
    if (s == NULL) return;
    s->chars++;
    d = s->data;
    if (s->len + 6 >= p->returnsize) {
        p->returnsize = s->len + 256;
        d = p->returnsize < 256 ? NULL : extend_str(s, p->returnsize);
        if (d == NULL) {
            val_destroy(Obj(p->return_value));
            p->return_value = NULL;
            return;
        }
    }
    if (p->c != 0 && p->c < 0x80) {
        d[s->len++] = (uint8_t)p->c;
        return;
    }
    s->len += utf8out(p->c, d + s->len);
}

static inline void pad(Data *p)
{
    for (; p->width > 0; p->width--) put_char(p);
}

static void pad_right2(Data *p, uint8_t c, char minus, size_t ln)
{
    size_t n = 0;
    p->width = (p->width < 0 || ln > (unsigned int)p->width) ? -1 : p->width - (int)ln;
    if (p->precision != NOT_FOUND) p->zero = false;
    if (p->precision > 0 && (unsigned int)p->precision > ln) {
        n = (unsigned int)p->precision - ln;
        p->width = (p->width < 0 || n > (unsigned int)p->width) ? -1 : p->width - (int)n;
    }
    if (minus != '\0' || p->plus || p->space) p->width--;
    if (c != 0 && p->square) p->width--;
    if (!p->zero && !p->left) {
        p->c = ' ';
        pad(p);
    }
    do {
        if (minus != '\0') p->c = (unsigned char)minus;
        else if (p->plus) p->c = '+';
        else if (p->space) p->c = ' ';
        else break;
        put_char(p);
    } while (false);
    if (c != 0 && p->square) {p->c = c; put_char(p);}
    p->c = '0';
    if (p->zero && !p->left) pad(p);
    for (;n > 0; n--) put_char(p);
}

static void note_failure(Data *p, Obj *err) {
    if (err->obj == ERROR_OBJ) {
        if (p->failure == NULL) {
            p->failure = err;
            return;
        }
    } else {
        if (p->none == 0) p->none = p->listp;
    }
    val_destroy(err);
}

static ival_t get_ival(Data *p) {
    const struct values_s *v = next_arg(p);

    if (v != NULL) {
        ival_t ival;
        Obj *val = v->val;
        Error *err = val->obj->ival(val, &ival, 8 * (sizeof ival), &v->epoint);
        if (err == NULL) return ival;
        note_failure(p, Obj(err));
    }
    return 0;
}

/* if width and prec. in the args */
static const struct values_s *star_args(Data *p)
{
    if (p->star_w) {
        ival_t ival = get_ival(p);
        p->width = ival < 0 ? -ival : ival;
        if (ival < 0) p->left = true;
    }
    if (p->star_p) {
        ival_t ival = get_ival(p);
        p->precision = ival > 0 ? ival : 0;
    }
    return next_arg(p);
}

/* for %d and friends, it puts in holder
 * the representation with the right padding
 */
static inline void decimal(Data *p)
{
    const struct values_s *v = star_args(p);
    bool minus;
    size_t i;
    Str *str;
    Obj *err2 = NULL;

    if (v == NULL) {
        minus = false;
        if (p->precision < 1) p->precision = 1;
    } else {
        Obj *val = v->val;
        Obj *err = int_from_obj(val, &v->epoint);
        if (err->obj != INT_OBJ) {
            note_failure(p, err);
            minus = false;
        } else {
            err2 = INT_OBJ->repr(err, &v->epoint, SIZE_MAX);
            if (err2 == NULL) err2 = new_error_mem(&v->epoint);
            if (err2->obj != STR_OBJ) {
                note_failure(p, err2);
                err2 = NULL;
                minus = false;
            } else {
                minus = Int(err)->len < 0;
            }
            val_destroy(err);
        }
    }

    str = Str(err2 != NULL ? err2 : val_reference(null_str));
    i = minus ? 1 : 0;
    pad_right2(p, 0, minus ? '-' : '\0', str->len - i);
    for (; i < str->len; i++) {p->c = str->data[i]; put_char(p);}
    val_destroy(Obj(str));
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

static MUST_CHECK Obj *get_intbits(Data *p) {
    const struct values_s *v = star_args(p);

    if (v != NULL) {
        Obj *err;
        if (p->tilde) {
            err = bits_from_obj(v->val, &v->epoint);
            if (err->obj == BITS_OBJ) return err;
        } else {
            err = int_from_obj(v->val, &v->epoint);
            if (err->obj == INT_OBJ) return err;
        }
        note_failure(p, err);
    }
    return val_reference(p->tilde ? bits_value[0] : int_value[0]);
}

/* for %x %X hexadecimal representation */
static inline void hexa(Data *p)
{
    bool minus;
    const char *hex = (*p->pf == 'x') ? "0123456789abcdef" : "0123456789ABCDEF";
    unsigned int bp, b;
    size_t bp2;
    Obj *val = get_intbits(p);

    if (val->obj == BITS_OBJ) {
        Bits *bits = Bits(val);
        minus = (bits->len < 0);
        bp2 = (size_t)(minus ? ~bits->len : bits->len);
        bp = b = 0;
        do {
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(bdigit_t) - 4;
            } else bp -= 4;
            b = (bits->data[bp2] >> bp) & 0xf;
        } while (b == 0);

        pad_right2(p, '$', minus ? '~' : '\0', bp / 4 + bp2 * (sizeof(bdigit_t) * 2) + 1);
        for (;;) {
            p->c = (unsigned char)hex[b];
            put_char(p);
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(bdigit_t) - 4;
            } else bp -= 4;
            b = (bits->data[bp2] >> bp) & 0xf;
        }
        val_destroy(Obj(bits));
    } else {
        Int *integer = Int(val);
        minus = (integer->len < 0);
        bp2 = minus ? -(size_t)integer->len : (size_t)integer->len;
        bp = b = 0;
        do {
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(digit_t) - 4;
            } else bp -= 4;
            b = (integer->data[bp2] >> bp) & 0xf;
        } while (b == 0);

        pad_right2(p, '$', minus ? '-' : '\0', bp / 4 + bp2 * (sizeof(digit_t) * 2) + 1);
        for (;;) {
            p->c = (unsigned char)hex[b];
            put_char(p);
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(digit_t) - 4;
            } else bp -= 4;
            b = (integer->data[bp2] >> bp) & 0xf;
        }
        val_destroy(Obj(integer));
    }
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

/* for %b binary representation */
static inline void bin(Data *p)
{
    bool minus;
    unsigned int bp, b;
    size_t bp2;
    Obj *val = get_intbits(p);

    if (val->obj == BITS_OBJ) {
        Bits *bits = Bits(val);
        minus = (bits->len < 0);
        bp2 = (size_t)(minus ? ~bits->len : bits->len);
        bp = b = 0;
        do {
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(bdigit_t) - 1;
            } else bp--;
            b = (bits->data[bp2] >> bp) & 1;
        } while (b == 0);

        pad_right2(p, '%', minus ? '~' : '\0', bp + bp2 * (sizeof(bdigit_t) * 8) + 1);
        for (;;) {
            p->c = '0' + b;
            put_char(p);
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(bdigit_t) - 1;
            } else bp--;
            b = (bits->data[bp2] >> bp) & 1;
        }
        val_destroy(Obj(bits));
    } else {
        Int *integer = Int(val);
        minus = (integer->len < 0);
        bp2 = minus ? -(size_t)integer->len : (size_t)integer->len;
        bp = b = 0;
        do {
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(digit_t) - 1;
            } else bp--;
            b = (integer->data[bp2] >> bp) & 1;
        } while (b == 0);

        pad_right2(p, '%', minus ? '-' : '\0', bp + bp2 * (sizeof(digit_t) * 8) + 1);
        for (;;) {
            p->c = '0' + b;
            put_char(p);
            if (bp == 0) {
                if (bp2 == 0) break;
                bp2--;
                bp = 8 * sizeof(digit_t) - 1;
            } else bp--;
            b = (integer->data[bp2] >> bp) & 1;
        }
        val_destroy(Obj(integer));
    }
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

/* %c chars */
static inline void chars(Data *p)
{
    const struct values_s *v = star_args(p);
    uval_t uval;
    int i;

    if (v == NULL) {
        uval = 63;
    } else {
        Obj *val = v->val;
        Error *err = val->obj->uval(val, &uval, 24, &v->epoint);
        if (err != NULL) {
            note_failure(p, Obj(err));
            uval = 63;
        } else {
            uval &= 0xffffff;
        }
    }

    i = (p->dot && p->precision <= 0) ? 0 : 1;
    p->width = (p->width < 0 || i > p->width) ? -1 : p->width - i;
    if (!p->left) {
        p->c = ' ';
        pad(p);
    }
    if (i != 0) {p->c = uval; put_char(p);}
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

/* %s strings */
static inline void strings(Data *p)
{
    const struct values_s *v = star_args(p);
    size_t i;
    const uint8_t *tmp;
    Str *str;
    Obj *err = NULL;

    if (v != NULL) {
        if (*p->pf == 'r') {
            Obj *val = v->val;
            err = val->obj->repr(val, &v->epoint, SIZE_MAX);
            if (err == NULL) err = new_error_mem(&v->epoint);
        } else {
            err = str_from_obj(v->val, &v->epoint);
        }
        if (err->obj != STR_OBJ) {
            note_failure(p, err);
            err = NULL;
        }
    }

    str = Str(err != NULL ? err : val_reference(null_str));
    tmp = str->data;
    i = str->chars;
    if (p->dot) { /* the smaller number */
        i = (p->precision < 0) ? 0 : (i < (unsigned int)p->precision) ? i : (unsigned int)p->precision;
    }
    p->width = (p->width < 0 || i > (unsigned int)p->width) ? -1 : p->width - (int)i;
    if (!p->left) {
        p->c = ' ';
        pad(p);
    }
    for (; i != 0; i--) { /* put the string */
        p->c = *tmp;
        if ((p->c & 0x80) != 0) tmp += utf8in(tmp, &p->c); else tmp++;
        put_char(p);
    }
    val_destroy(Obj(str));
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

/* %f or %g  floating point representation */
static inline void floating(Data *p)
{
    const struct values_s *v = star_args(p);
    char tmp[400], *t, form[10];
    bool minus;
    double d;
    int l;

    if (v == NULL) {
        d = 0.0;
    } else {
        Obj *err = float_from_obj(v->val, &v->epoint);
        if (err->obj != FLOAT_OBJ) {
            note_failure(p, err);
            d = 0.0;
        } else {
            d = Float(err)->real;
            val_destroy(err);
        }
    }
    if (d < 0.0) { d = -d; minus = true;} else minus = false;
    if (!p->dot) p->precision = 6;
    t = form;
    *t++ = '%';
    if (p->square) *t++ = '#';
    *t++ = '.';
    *t++ = '*';
    *t++ = (char)*p->pf;
    *t++ = 0;
    l = snprintf(tmp, sizeof tmp, form, (p->precision < 80) ? (p->precision > 0 ? p->precision : 0) : 80, d);
    t = tmp;

    p->precision = 0;
    pad_right2(p, 0, minus ? '-' : '\0', (l > 0) ? (size_t)l : 0);
    while (*t != 0) { /* the integral */
        p->c = (uint8_t)*t;
        put_char(p);
        t++;
    }
    if (p->left) {
        p->c = ' ';
        pad(p);
    }
}

MUST_CHECK Obj *isnprintf(oper_t op)
{
    Funcargs *vals = Funcargs(op->v2);
    struct values_s *v = vals->val;
    argcount_t args = vals->len;
    const uint8_t *invalid_format_char = NULL;
    Obj *err;
    Data data;
    str_t fmt;

    err = tostr2(&v[0], &fmt);
    if (err != NULL) return err;
    data.pf = fmt.data;
    data.pfend = data.pf + fmt.len;

    data.listp = 0;
    data.list = &v[1];
    data.largs = args - 1;

    data.returnsize = sizeof data.return_value->u.val;
    data.return_value = new_str2(data.returnsize);
    if (data.return_value != NULL) {
        data.return_value->len = 0;
        data.return_value->chars = 0;
    }
    data.failure = NULL;
    data.none = 0;

    for (; data.pf < data.pfend; data.pf++) {
        const uint8_t *pf = data.pf;
        unichar_t c = *data.pf;
        if (c != '%') {
            if ((c & 0x80) != 0) data.pf += utf8in(data.pf, &c) - 1;
            data.c = c;
            put_char(&data);  /* add the char the string */
            continue;
        }
        /* reset the flags. */
        data.precision = data.width = NOT_FOUND;
        data.star_w = data.star_p = false;
        data.square = data.plus = data.space = false;
        data.left = false; data.dot = false;
        data.zero = false; data.tilde = false;
        while (data.pf < data.pfend) {
            data.pf++;
            if (data.pf >= data.pfend) goto error;
            c = *data.pf;
            switch (c) {
            case 'e':
            case 'E':  /* Exponent double */
            case 'f':  /* float, double */
            case 'F':
            case 'g':
            case 'G':
                floating(&data);
                break;
            case 'd':  /* decimal */
                decimal(&data);
                break;
            case 'x':
            case 'X':  /* hexadecimal */
                hexa(&data);
                break;
            case 'b':  /* binary */
                bin(&data);
                break;
            case 'c': /* character */
                chars(&data);
                break;
            case 'r':  /* repr */
            case 's':  /* string */
                strings(&data);
                break;
            case '%':  /* nothing just % */
                if (pf + 1 != data.pf) goto error;
                data.c = '%';
                put_char(&data);
                break;
            case ' ':
                if (data.dot) goto error;
                data.space = true;
                continue;
            case '#':
                if (data.dot) goto error;
                data.square = true;
                continue;
            case '*':
                if (data.dot) {
                    if (data.precision == NOT_FOUND) {
                        data.precision = 0;
                        data.star_p = true;
                        continue;
                    }
                } else if (data.width == NOT_FOUND) {
                    data.width = 0;
                    data.star_w = true;
                    continue;
                }
                goto error;
            case '+':
                if (data.dot) goto error;
                data.plus = true;
                continue;
            case '-':
                if (data.dot) goto error;
                data.left = true;
                continue;
            case '~':
                if (data.dot) goto error;
                data.tilde = true;
                continue;
            case '.':
                if (data.dot) goto error;
                data.dot = true;
                if (data.width == NOT_FOUND) data.width = 0;
                continue;
            case '0':
                if (data.width == NOT_FOUND) {
                    data.zero = true;
                    continue;
                }
                FALL_THROUGH; /* fall through */
            case '1':
            case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
                c -= '0';
                if (data.dot) {
                    if (!data.star_p) {
                        data.precision = ((data.precision == NOT_FOUND) ? 0 : data.precision * 10) + (int)c;
                        if (data.precision < 100000) continue;
                    }
                } else if (!data.star_w) {
                    data.width = ((data.width == NOT_FOUND) ? 0 : data.width * 10) + (int)c;
                    if (data.width < 100000) continue;
                }
                FALL_THROUGH; /* fall through */
            default:
            error:
                if (invalid_format_char == NULL) invalid_format_char = data.pf;
                data.pf += utf8len(c);
                star_args(&data);
                while (pf < data.pf) {
                    data.c = *pf;
                    if ((data.c & 0x80) != 0) pf += utf8in(pf, &data.c); else pf++;
                    put_char(&data);
                }
                data.pf--;
                break;
            }
            break;
        }
    }
    if (invalid_format_char != NULL) {
        err_msg_unknown_formatchar(Str(v[0].val), (size_t)(invalid_format_char - fmt.data), &v[0].epoint);
    } else if (data.listp != data.largs) {
        err_msg_argnum(args, data.listp + 1, data.listp + 1, op->epoint3);
    } else if (data.failure != NULL) {
        err_msg_output(Error(data.failure));
    } else if (data.none != 0) {
        err_msg_still_none(NULL, (data.largs >= data.none) ? &v[data.none].epoint : op->epoint);
    }
    if (data.failure != NULL) val_destroy(data.failure);
    if (data.return_value == NULL) return new_error_mem(op->epoint);
    if (data.return_value->u.val != data.return_value->data) {
        if (data.return_value->len < data.returnsize) {
            uint8_t *s2 = reallocate_array(data.return_value->data, data.return_value->len);
            if (s2 != NULL) {
                data.return_value->data = s2;
                data.return_value->u.s.max = data.return_value->len;
            }
        }
    }
    return Obj(data.return_value);
}

