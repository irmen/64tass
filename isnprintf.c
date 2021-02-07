/*
   Unix snprintf implementation.
   Version 1.3

   Adapted for use in 64tass by Soci/Singular
   $Id: isnprintf.c 2349 2021-02-07 12:46:22Z soci $

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
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"

#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#ifndef snprintf
#define snprintf(str, size, format, var, var2) sprintf(str, format, var, var2)
#endif
#endif

static Str return_value;
static size_t returnsize = 0;
static size_t none;
static Obj *failure;

/* this struct holds everything we need */
typedef struct Data {
  const uint8_t *pf;
  const uint8_t *pfend;
/* FLAGS */
  int width, precision;
  uchar_t pad;
  bool left, square, space, plus, star_w, star_p, dot;
} Data;

/* those are defines specific to snprintf to hopefully
 * make the code clearer :-)
 */
enum { NOT_FOUND = -1 };

static size_t listp;
static const struct values_s *list;
static size_t largs;

static const struct values_s *next_arg(void) {
    const struct values_s *ret;
    if (largs > listp) {
        Obj *val;
        ret = &list[listp];
        val = ret->val;
        if (val == &none_value->v) {
            if (none != 0) none = listp;
            ret = NULL;
        } else if (val->obj == ERROR_OBJ) {
            if (failure == NULL) failure = val_reference(val);
            ret = NULL;
        }
    } else {
        ret = NULL;
    }
    listp++;
    return ret;
}

static void put_char(uchar_t c) {
    uint8_t *p;
    return_value.chars++;
    p = return_value.data;
    if (return_value.len + 6 >= returnsize) {
        returnsize += 256;
        if (returnsize < 256) err_msg_out_of_memory(); /* overflow */
        p = (uint8_t *)reallocx(p, returnsize);
        return_value.data = p;
    }
    if (c != 0 && c < 0x80) {
        p[return_value.len++] = (uint8_t)c;
        return;
    }
    p = utf8out(c, p + return_value.len);
    return_value.len = (size_t)(p - return_value.data);
}

/* pad right */
static inline void pad_right(Data *p)
{
    if (p->width > 0 && !p->left) {
        for (; p->width > 0; p->width--) put_char(p->pad);
    }
}

static void pad_right2(Data *p, uint8_t c, bool minus, size_t ln)
{
    size_t n = 0;
    p->width -= ln;
    if (p->precision > 0 && (size_t)p->precision > ln) {
        n = (size_t)p->precision - ln;
        p->width -= n;
    }
    if (minus || p->plus || p->space) p->width--;
    if (c != 0 && p->square) p->width--;
    if (p->pad != '0') pad_right(p);
    if (minus) put_char('-');
    else if (p->plus) put_char('+');
    else if (p->space) put_char(' ');
    if (c != 0 && p->square) put_char(c);
    if (p->pad == '0') pad_right(p);
    for (;n > 0; n--) put_char('0');
}

/* pad left */
static void pad_left(Data *p)
{
    if (p->width > 0 && p->left) {
        for (; p->width > 0; p->width--) put_char(p->pad);
    }
}

static void note_failure(Obj *err) {
    if (err->obj == ERROR_OBJ) {
        if (failure == NULL) {
            failure = err;
            return;
        }
    } else {
        if (none != 0) none = listp;
    }
    val_destroy(err);
}

static ival_t get_ival(void) {
    const struct values_s *v = next_arg();

    if (v != NULL) {
        ival_t ival;
        Obj *val = v->val;
        Error *err = val->obj->ival(val, &ival, 8 * (sizeof ival), &v->epoint);
        if (err == NULL) return ival;
        note_failure(&err->v);
    }
    return 0;
}

/* if width and prec. in the args */
static void star_args(Data *p)
{
    if (p->star_w) {
        ival_t ival = get_ival();
        p->width = ival < 0 ? -ival : ival;
        if (ival < 0) p->left = true;
    }
    if (p->star_p) {
        ival_t ival = get_ival();
        p->precision = ival > 0 ? ival : 0;
    }
}

/* for %d and friends, it puts in holder
 * the representation with the right padding
 */
static inline void decimal(Data *p)
{
    const struct values_s *v = next_arg();
    bool minus;
    Str *str;
    size_t i;

    star_args(p);

    if (v == NULL) {
        str = ref_str(null_str);
        minus = false;
    } else {
        Obj *val = v->val;
        Obj *err = INT_OBJ->create(val, &v->epoint);
        if (err->obj != INT_OBJ) {
            note_failure(err);
            str = ref_str(null_str);
            minus = false;
        } else {
            Obj *err2 = INT_OBJ->repr(err, &v->epoint, SIZE_MAX);
            if (err2 == NULL) err2 = (Obj *)new_error_mem(&v->epoint);
            if (err2->obj != STR_OBJ) {
                note_failure(err2);
                str = ref_str(null_str);
                minus = false;
            } else {
                str = (Str *)err2;
                minus = ((Int *)err)->len < 0;
            }
            val_destroy(err);
        }
    }

    i = minus ? 1 : 0;
    pad_right2(p, 0, minus, str->len - i);
    for (; i < str->len; i++) put_char(str->data[i]);
    val_destroy(&str->v);
    pad_left(p);
}

static MUST_CHECK Int *get_int(Data *p) {
    const struct values_s *v = next_arg();

    star_args(p);

    if (v != NULL) {
        Obj *val = v->val;
        Obj *err = INT_OBJ->create(val, &v->epoint);
        if (err->obj == INT_OBJ) return (Int *)err;
        note_failure(err);
    }
    return ref_int(int_value[0]);
}

/* for %x %X hexadecimal representation */
static inline void hexa(Data *p)
{
    bool minus;
    Int *integer;
    const char *hex = (*p->pf == 'x') ? "0123456789abcdef" : "0123456789ABCDEF";
    unsigned int bp, b;
    size_t bp2;

    integer = get_int(p);
    minus = (integer->len < 0);
    bp2 = (size_t)(minus ? -integer->len : integer->len);
    bp = b = 0;
    do {
        if (bp == 0) {
            if (bp2 == 0) break;
            bp2--;
            bp = 8 * sizeof(digit_t) - 4;
        } else bp -= 4;
        b = (integer->data[bp2] >> bp) & 0xf;
    } while (b == 0);

    pad_right2(p, '$', minus, bp / 4 + bp2 * (sizeof(digit_t) * 2) + 1);
    for (;;) {
        put_char((uint8_t)hex[b]);
        if (bp == 0) {
            if (bp2 == 0) break;
            bp2--;
            bp = 8 * sizeof(digit_t) - 4;
        } else bp -= 4;
        b = (integer->data[bp2] >> bp) & 0xf;
    }
    val_destroy(&integer->v);
    pad_left(p);
}

/* for %b binary representation */
static inline void bin(Data *p)
{
    bool minus;
    Int *integer;
    unsigned int bp, b;
    size_t bp2;

    integer = get_int(p);
    minus = (integer->len < 0);
    bp2 = (size_t)(minus ? -integer->len : integer->len);
    bp = b = 0;
    do {
        if (bp == 0) {
            if (bp2 == 0) break;
            bp2--;
            bp = 8 * sizeof(digit_t) - 1;
        } else bp--;
        b = (integer->data[bp2] >> bp) & 1;
    } while (b == 0);

    pad_right2(p, '%', minus, bp + bp2 * (sizeof(digit_t) * 8) + 1);
    for (;;) {
        put_char('0' + b);
        if (bp == 0) {
            if (bp2 == 0) break;
            bp2--;
            bp = 8 * sizeof(digit_t) - 1;
        } else bp--;
        b = (integer->data[bp2] >> bp) & 1;
    }
    val_destroy(&integer->v);
    pad_left(p);
}

/* %c chars */
static inline void chars(void)
{
    const struct values_s *v = next_arg();
    uval_t uval;

    if (v == NULL) {
        uval = 63;
    } else {
        Obj *val = v->val;
        Error *err = val->obj->uval(val, &uval, 24, &v->epoint);
        if (err != NULL) {
            note_failure(&err->v);
            uval = 63;
        } else {
            uval &= 0xffffff;
        }
    }

    put_char(uval);
}

/* %s strings */
static inline void strings(Data *p)
{
    const struct values_s *v = next_arg();
    int i;
    const uint8_t *tmp;
    uchar_t ch;
    Str *str;

    star_args(p);

    if (v == NULL) {
        str = ref_str(null_str);
    } else {
        Obj *err;
        if (*p->pf == 'r') {
            Obj *val = v->val;
            err = val->obj->repr(val, &v->epoint, SIZE_MAX);
            if (err == NULL) err = (Obj *)new_error_mem(&v->epoint);
        } else {
            err = STR_OBJ->create(v->val, &v->epoint);
        }
        if (err->obj != STR_OBJ) {
            note_failure(err);
            str = ref_str(null_str);
        } else {
            str = (Str *)err;
        }
    }

    tmp = str->data;
    i = (int)str->chars;
    if (p->dot) { /* the smallest number */
        i = (i < p->precision ? i : p->precision);
    }
    if (i < 0) i = 0;
    p->width -= i;
    pad_right(p);
    while (i-- > 0) { /* put the string */
        ch = *tmp;
        if ((ch & 0x80) != 0) tmp += utf8in(tmp, &ch); else tmp++;
        put_char(ch);
    }
    val_destroy(&str->v);
    pad_left(p);
}

/* %f or %g  floating point representation */
static inline void floating(Data *p)
{
    const struct values_s *v = next_arg();
    char tmp[400], *t, form[10];
    bool minus;
    double d;
    int l;

    star_args(p);

    if (v == NULL) {
        d = 0.0;
    } else {
        Obj *err = FLOAT_OBJ->create(v->val, &v->epoint);
        if (err->obj != FLOAT_OBJ) {
            note_failure(err);
            d = 0.0;
        } else {
            d = ((Float *)err)->real;
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
    pad_right2(p, 0, minus, (l > 0) ? (size_t)l : 0);
    while (*t != 0) { /* the integral */
        put_char((uint8_t)*t);
        t++;
    }
    pad_left(p);
}

MUST_CHECK Obj *isnprintf(Funcargs *vals, linepos_t epoint)
{
    struct values_s *v = vals->val;
    size_t args = vals->len;
    Obj *val;
    Str *str;
    Data data;

    val = v[0].val;
    switch (val->obj->type) {
    case T_ERROR:
    case T_NONE:
        return val_reference(val);
    case T_STR: break;
    case T_ADDRESS:
        if (((Address *)val)->val == &none_value->v || ((Address *)val)->val->obj == ERROR_OBJ) return val_reference(((Address *)val)->val);
        /* fall through */
    default:
        err_msg_wrong_type(val, STR_OBJ, &v[0].epoint);
        return (Obj *)ref_none();
    }
    data.pf = ((Str *)val)->data;
    data.pfend = data.pf +((Str *)val)->len;

    listp = 0;
    list = &v[1];
    largs = args - 1;

    return_value.data = NULL;
    failure = NULL;
    return_value.len = 0;
    return_value.chars = 0;
    none = returnsize = 0;

    for (; data.pf < data.pfend; data.pf++) {
        uchar_t c = *data.pf;
        if (c != '%') {
            if ((c & 0x80) != 0) data.pf += utf8in(data.pf, &c) - 1;
            put_char(c);  /* add the char the string */
            continue;
        }
        /* reset the flags. */
        data.precision = data.width = NOT_FOUND;
        data.star_w = data.star_p = false;
        data.square = data.plus = data.space = false;
        data.left = false; data.dot = false;
        data.pad = ' ';
        while (data.pf < data.pfend - 1) {
            struct linepos_s epoint2;
            str_t msg;

            c = *(++data.pf);
            switch (c) {
            case 'e':
            case 'E':  /* Exponent double */
            case 'f':  /* float, double */
            case 'F':
            case 'a':
            case 'A':
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
                chars();
                break;
            case 'r':  /* repr */
            case 's':  /* string */
                strings(&data);
                break;
            case '%':  /* nothing just % */
                put_char('%');
                break;
            case ' ':
                data.space = true;
                continue;
            case '#':
                data.square = true;
                continue;
            case '*':
                if (data.width == NOT_FOUND) {
                    data.width = 0;
                    data.star_w = true;
                } else if (data.dot && data.precision == NOT_FOUND) {
                    data.precision = 0;
                    data.star_p = true;
                }
                continue;
            case '+':
                data.plus = true;
                continue;
            case '-':
                data.left = true;
                continue;
            case '.':
                data.dot = true;
                if (data.width == NOT_FOUND) data.width = 0;
                continue;
            case '0':
                if (data.width == NOT_FOUND) {
                    data.pad = '0';
                    continue;
                }
                /* fall through */
            case '1':
            case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
                c -= '0';
                if (data.dot && !data.star_p) {
                    data.precision = ((data.precision == NOT_FOUND) ? 0 : data.precision * 10) + (int)c;
                    if (data.precision < 100000) continue;
                } else if (!data.star_w) {
                    data.width = ((data.width == NOT_FOUND) ? 0 : data.width * 10) + (int)c;
                    if (data.width < 100000) continue;
                }
                /* fall through */
            default:
                epoint2 = v[0].epoint;
                epoint2.pos = interstring_position(&epoint2, ((Str *)val)->data, (size_t)(data.pf - ((Str *)val)->data));
                msg.data = data.pf;
                if ((c & 0x80) != 0) msg.len = utf8in(data.pf, &c); else msg.len = 1;
                err_msg_not_defined(&msg, &epoint2);
                next_arg();
                star_args(&data);
                put_char('%');
                put_char(c);
                data.pf += msg.len - 1;
                break;
            }
            break;
        }
    }
    if (listp != largs) {
        err_msg_argnum(args, listp + 1, listp + 1, epoint);
    } else if (failure != NULL) {
        err_msg_output((const Error *)failure);
    } else if (none != 0) {
        err_msg_still_none(NULL, (largs >= none) ? &v[none].epoint : epoint);
    }
    if (failure != NULL) val_destroy(failure);
    str = new_str(0);
    str->len = return_value.len;
    str->chars = return_value.chars;
    if (return_value.len > sizeof str->u.val) {
        str->u.s.max = return_value.len;
        str->u.s.hash = -1;
        if (returnsize > return_value.len) {
            uint8_t *d = (uint8_t *)realloc(return_value.data, return_value.len);
            if (d != NULL) {
                str->data = d;
                return &str->v;
            }
        }
        str->data = return_value.data;
        return &str->v;
    }
    memcpy(str->u.val, return_value.data, return_value.len);
    str->data = str->u.val;
    free(return_value.data);
    return &str->v;
}

