/*
   Unix snprintf implementation.
   Version 1.3

   Adapted for use in 64tass by Soci/Singular
   $Id$

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

#include <string.h>
#include "isnprintf.h"
#include "unicode.h"
#include "eval.h"

#include "strobj.h"

#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#ifndef snprintf
#define snprintf(str, size, format, var, var2) sprintf(str, format, var, var2)
#endif
#endif

static ustr_t return_value;
static size_t returnsize = 0;
static int none;

/* this struct holds everything we need */
struct DATA {
  const char *pf;
  const char *pfend;
/* FLAGS */
  int width, precision;
  int justify; char pad;
  int square, space, star_w, star_p;
};

/* those are defines specific to snprintf to hopefully
 * make the code clearer :-)
 */
#define RIGHT 1
#define LEFT  0
#define NOT_FOUND -1
#define FOUND 1
#define MAX_FIELD 15

/* the conversion flags */
#define isflag(c) ((c) == '#' || (c) == ' ' || \
                   (c) == '*' || (c) == '+' || \
                   (c) == '-' || (c) == '.' || \
                   ((c) >= '0' && (c) <= '9'))

static size_t listp;
static const struct values_s *list;
static size_t largs;
static struct values_s dummy = {NULL, {0, 0}};

static inline const struct values_s *next_arg(void) {
    if (!none && largs > listp) return &list[listp++];
    listp++;
    dummy.val = none_value;
    return &dummy;
}

static inline void PUT_CHAR(uint32_t c) {
    uint8_t *p = (uint8_t *)return_value.data;
    if (return_value.len + 6 >= returnsize) {
        returnsize += 256;
        p = (uint8_t *)realloc(p, returnsize);
        if (!p || returnsize < 256) err_msg_out_of_memory(); /* overflow */
        return_value.data = p;
    }
    if (c && c < 0x80) p[return_value.len++] = c; else {
        p = utf8out(c, p + return_value.len);
        return_value.len = p - return_value.data;
    }
    return_value.chars++;
}

/* pad right */
static inline void PAD_RIGHT(struct DATA *p)
{
    if (p->width > 0 && p->justify != LEFT) {
        for (; p->width > 0; p->width--) PUT_CHAR(p->pad);
    }
}

static inline void PAD_RIGHT2(struct DATA *p, char c, int minus)
{
    if (minus || p->justify == RIGHT || p->space == FOUND) p->width--;
    if (c && p->square == FOUND) p->width--;
    if (p->pad != '0') PAD_RIGHT(p);
    if (minus) PUT_CHAR('-');
    else if (p->justify == RIGHT) PUT_CHAR('+');
    else if (p->space == FOUND) PUT_CHAR(' ');
    if (c && p->square == FOUND) PUT_CHAR(c);
    if (p->pad == '0') PAD_RIGHT(p);
}

/* pad left */
static inline void PAD_LEFT(struct DATA *p)
{
    if (p->width > 0 && p->justify == LEFT) {
        for (; p->width > 0; p->width--) PUT_CHAR((p)->pad);
    }
}

/* if width and prec. in the args */
static MUST_CHECK value_t star_args(struct DATA *p)
{
    uval_t uval;
    value_t err;

    if (p->star_w == FOUND) {
        const struct values_s *v = next_arg();
        const value_t val = v->val;
        if (val->obj == NONE_OBJ) none = 1;
        else {
            err = val->obj->uval(val, &uval, 8*sizeof(uval_t)-1, &v->epoint);
            if (err) return err;
            p->width = uval;
        }
    }
    if (p->star_p == FOUND) {
        const struct values_s *v = next_arg();
        const value_t val = v->val;
        if (val->obj == NONE_OBJ) none = 1;
        else {
            err = val->obj->uval(val, &uval, 8*sizeof(uval_t)-1, &v->epoint);
            if (err) return err;
            p->precision = uval;
        }
    }
    return NULL;
}

/* for %d and friends, it puts in holder
 * the representation with the right padding
 */
static inline MUST_CHECK value_t decimal(struct DATA *p, const struct values_s *v)
{
    int minus;
    value_t val = v->val, err, err2;
    size_t i;

    if (val->obj == NONE_OBJ) {
        none = 1;
        err = val_reference(int_value[0]);
    } else {
        err = val->obj->integer(val, &v->epoint);
        if (err->obj != INT_OBJ) return err;
    }

    minus = (err->u.integer.len < 0);
    err2 = err->obj->str(err, &v->epoint);
    val_destroy(err);
    if (err2->obj != STR_OBJ) return err2;

    p->width -= err2->u.str.len - minus;
    PAD_RIGHT2(p, 0, minus);
    for (i = minus ;i < err2->u.str.len;i++) PUT_CHAR(err2->u.str.data[i]);
    PAD_LEFT(p);
    val_destroy(err2);
    return NULL;
}

/* for %x %X hexadecimal representation */
static inline MUST_CHECK value_t hexa(struct DATA *p, const struct values_s *v)
{
    int minus;
    value_t val = v->val, err;
    const char *hex = (*p->pf == 'x') ? "0123456789abcdef" : "0123456789ABCDEF";
    size_t bits;
    int bp, bp2, b;

    if (val->obj == NONE_OBJ) {
        none = 1;
        err = val_reference(int_value[0]);
    } else {
        err = val->obj->integer(val, &v->epoint);
        if (err->obj != INT_OBJ) return err;
    }

    minus = (err->u.integer.len < 0);
    bits = 30 * abs(err->u.integer.len);
    if (bits & 3) {
        bits &= ~3;
        bp = bits % 30;
        bp2 = bits / 30;
        if (bp <= (30 - 4)) b = err->u.integer.data[bp2] >> bp;
        else {
            b = err->u.integer.data[bp2] >> bp;
            if (bp2 + 1 < err->u.integer.len) b |= err->u.integer.data[bp2 + 1] << (30 - bp);
        }
    } else {
        b = 0;
        bp = bits % 30;
        bp2 = bits / 30;
    }
    while (!b && bp | bp2) {
        if (bp >= 4) bp -=4; else { bp2--; bp = 30 - 4 + bp; }
        if (bp <= (30 - 4)) b = err->u.integer.data[bp2] >> bp;
        else b = (err->u.integer.data[bp2] >> bp) | (err->u.integer.data[bp2 + 1] << (30 - bp));
        b &= 15;
    }
    bits = bp2 * 30 + bp + 4;

    p->width -= (bits + 3) / 4;
    PAD_RIGHT2(p, '$', minus);
    
    b = err->u.integer.data[bp2] >> bp;
    if (bp > 30 - 4 && bp2 + 1 < err->u.integer.len) b |= err->u.integer.data[bp2 + 1] << (30 - bp);
    PUT_CHAR(hex[b & 15]);
  
    while (bp | bp2) {
        if (bp >= 4) bp -=4; else { bp2--; bp = 30 - 4 + bp; }
        b = err->u.integer.data[bp2] >> bp;
        if (bp > 30 - 4) b |= (err->u.integer.data[bp2 + 1] << (30 - bp));
        PUT_CHAR(hex[b & 15]);
    }
    PAD_LEFT(p);
    val_destroy(err);
    return NULL;
}

/* for %b binary representation */
static inline MUST_CHECK value_t bin(struct DATA *p, const struct values_s *v)
{
    int i;
    value_t val = v->val, err;

    if (val->obj == NONE_OBJ) {
        none = 1; 
        err = val_reference(int_value[0]);
    } else {
        err = val->obj->integer(val, &v->epoint);
        if (err->obj != INT_OBJ) return err;
    }
    i = abs(err->u.integer.len);
    if (i) {
        int minus;
        int span, bits;
        uval_t uval = err->u.integer.data[i - 1];
        minus = (err->u.integer.len < 0);
        span = 4 * sizeof(digit_t); bits = 0;
        while (span) {
            if (uval >> (bits + span)) {
                bits |= span;
            }
            span >>= 1;
        }
        i = (i - 1) * 30 + bits + 1;

        p->width -= i;
        PAD_RIGHT2(p, '%', minus);
        for (i = i - 1; i >= 0; i--) {
            PUT_CHAR((err->u.integer.data[i / 30] & (1 << (i % 30))) ? '1' : '0');
        }
    } else {
        p->width -= 1;
        PAD_RIGHT2(p, '%', 0);
        PUT_CHAR('0');
    }

    PAD_LEFT(p);
    val_destroy(err);
    return NULL;
}

/* %c chars */
static inline MUST_CHECK value_t chars(const struct values_s *v)
{
    uval_t uval;
    value_t val = v->val, err;

    if (val->obj == NONE_OBJ) {
        none = 1; 
        uval = 0;
    } else {
        err = val->obj->uval(val, &uval, 24, &v->epoint);
        if (err) return err;
    }

    PUT_CHAR(uval);
    return NULL;
}

/* %s strings */
static inline MUST_CHECK value_t strings(struct DATA *p, const struct values_s *v)
{
    int i;
    const uint8_t *tmp;
    uint32_t ch;
    value_t val = v->val, err;

    if (val->obj == NONE_OBJ) {
        none = 1;
        return NULL;
    }
    if (*p->pf == 'r') err = val->obj->repr(val, &v->epoint);
    else err = val->obj->str(val, &v->epoint);
    if (err->obj != STR_OBJ) {
        if (err->obj == NONE_OBJ) {
            none = 1;
            val_destroy(err);
            return NULL;
        }
        return err;
    }
    tmp = err->u.str.data;
    i = err->u.str.chars;
    if (p->precision != NOT_FOUND) /* the smallest number */
        i = (i < p->precision ? i : p->precision);
    p->width -= i;
    PAD_RIGHT(p);
    while (i-- > 0) { /* put the string */
        if (*tmp & 0x80) tmp += utf8in(tmp, &ch); else ch = *tmp++;
        PUT_CHAR(ch);
    }
    PAD_LEFT(p);
    val_destroy(err);
    return NULL;
}

/* %f or %g  floating point representation */
static inline MUST_CHECK value_t floating(struct DATA *p, const struct values_s *v)
{
    char tmp[400], *t, form[10];
    int minus;
    double d;
    value_t val = v->val, err;

    if (val->obj == NONE_OBJ) {
        none = 1;
        d = 0;
    } else {
        err = val->obj->real(val, &d, &v->epoint);
        if (err) return err;
    }
    if (d < 0.0) { d = -d; minus = 1;} else minus = 0;
    if (p->precision == NOT_FOUND) p->precision = 6;
    t = form;
    *t++ = '%';
    if (p->square == FOUND) *t++ = '#';
    *t++ = '.';
    *t++ = '*';
    *t++ = *p->pf;
    *t++ = 0;
    snprintf(tmp, sizeof(tmp), form, (p->precision < 80) ? p->precision : 80, d);
    t = tmp;

    p->width -= strlen(tmp);
    PAD_RIGHT2(p, 0, minus);
    while (*t) { /* the integral */
        PUT_CHAR(*t);
        t++;
    }
    PAD_LEFT(p);
    return NULL;
}

/* initialize the conversion specifiers */
static void conv_flag(char * s, struct DATA * p)
{
    int number;

    /* reset the flags.  */
    p->precision = p->width = NOT_FOUND;
    p->star_w = p->star_p = NOT_FOUND;
    p->square = p->space = NOT_FOUND;
    p->justify = NOT_FOUND;
    p->pad = ' ';

    for(;s && *s ;s++) {
        switch(*s) {
        case ' ': p->space = FOUND; break;
        case '#': p->square = FOUND; break;
        case '*': if (p->width == NOT_FOUND)
                      p->width = p->star_w = FOUND;
                  else
                      p->precision = p->star_p = FOUND;
                  break;
        case '+': p->justify = RIGHT; break;
        case '-': p->justify = LEFT; break;
        case '.': if (p->width == NOT_FOUND)
                      p->width = 0;
                  break;
        case '0': p->pad = '0'; break;
        case '1': case '2': case '3':
        case '4': case '5': case '6':
        case '7': case '8': case '9':     /* gob all the digits */
                  for (number = 0; *s >= '0' && *s <= '9'; s++) {
                      number = number*10 + *s - '0';
                  }
                  if (p->width == NOT_FOUND) p->width = number;
                  else p->precision = number;
                  s--;   /* went to far go back */
                  break;
        }
    }
}

/* return templates only! */
MUST_CHECK value_t isnprintf(value_t vals, linepos_t epoint)
{
    struct values_s *v = vals->u.funcargs.val;
    size_t args = vals->u.funcargs.len;
    value_t err;
    struct DATA data;
    char conv_field[MAX_FIELD];
    int state;
    int i;

    if (args < 1) {
        err_msg_argnum(args, 1, 0, epoint);
        return val_reference(none_value);
    }
    switch (v[0].val->obj->type) {
    case T_ERROR:
    case T_NONE:
        return val_reference(v[0].val);
    case T_STR: break;
    default:
        err_msg_wrong_type(v[0].val, &v[0].epoint);
        return val_reference(none_value);
    }
    data.pf = (char *)v[0].val->u.str.data;
    data.pfend = data.pf + v[0].val->u.str.len;

    listp = 0;
    list = &v[1];
    largs = args - 1;

    return_value.data = NULL;
    return_value.len = 0;
    return_value.chars = 0;
    none = returnsize = 0;

    for (; data.pf < data.pfend; data.pf++) {
        if ( *data.pf == '%' ) { /* we got a magic % cookie */
            conv_flag((char *)0, &data); /* initialise format flags */
            for (state = 1; data.pf < data.pfend - 1 && state;) {
                switch (*(++data.pf)) {
                case 'e':
                case 'E':  /* Exponent double */
                case 'f':  /* float, double */
                case 'F':
                case 'a':
                case 'A':
                case 'g':
                case 'G':
                    err = star_args(&data);
                    if (err) goto error;
                    err = floating(&data, next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case 'd':  /* decimal */
                    err = star_args(&data);
                    if (err) goto error;
                    err = decimal(&data, next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case 'x':
                case 'X':  /* hexadecimal */
                    err = star_args(&data);
                    if (err) goto error;
                    err = hexa(&data, next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case 'b':  /* binary */
                    err = star_args(&data);
                    if (err) goto error;
                    err = bin(&data, next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case 'c': /* character */
                    err = chars(next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case 'r':  /* repr */
                case 's':  /* string */
                    err = star_args(&data);
                    if (err) goto error;
                    err = strings(&data, next_arg());
                    if (err) goto error;
                    state = 0;
                    break;
                case '%':  /* nothing just % */
                    PUT_CHAR('%');
                    state = 0;
                    break;
                case '#': case ' ': case '+': case '*':
                case '-': case '.': case '0': case '1':
                case '2': case '3': case '4': case '5':
                case '6': case '7': case '8': case '9':
                    /* initialize width and precision */
                    for (i = 0; data.pf < data.pfend && isflag(*data.pf); i++, data.pf++) {
                        if (i < MAX_FIELD - 1) conv_field[i] = *data.pf;
                    }
                    conv_field[i] = '\0';
                    conv_flag(conv_field, &data);
                    data.pf--;   /* went to far go back */
                    break;
                default:
                    {
                        uint8_t str[2] = {'%', (uint8_t)*data.pf};
                        str_t msg = {2, str};
                        err_msg_not_defined(&msg, &v[0].epoint);
                        state = 0;
                    }
                } /* end switch */
            } /* end of for state */
        } else { /* not % */
            PUT_CHAR(*data.pf);  /* add the char the string */
        }
    }
    if (listp != largs) {
        err_msg_argnum(args, listp + 1, listp + 1, epoint);
    } else if (none) {
        err_msg_still_none(NULL, epoint);
    }
    err = val_alloc(STR_OBJ);
    err->u.str.len = return_value.len;
    err->u.str.chars = return_value.chars;
    if (return_value.len > sizeof(err->u.str.val)) {
        if (returnsize > return_value.len) {
            err->u.str.data = (uint8_t *)realloc(return_value.data, return_value.len);
            if (!err->u.str.data) err_msg_out_of_memory(); /* overflow */
        } else err->u.str.data = return_value.data;
        return err;
    }
    memcpy(err->u.str.val, return_value.data, return_value.len);
    err->u.str.data = err->u.str.val;
    free(return_value.data);
    return err;
error:
    free(return_value.data);
    return err;
}

