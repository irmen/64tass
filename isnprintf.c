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
#include "misc.h"
#include "eval.h"

#include "strobj.h"

#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#define snprintf(str, size, format, var, var2) sprintf(str, format, var, var2)
#endif

static struct value_s return_value;
static size_t returnsize = 0;
static linepos_t epoint;

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
static const struct value_s *list;
static inline struct value_s *next_arg(void) {
    if (list->u.list.len > listp) return list->u.list.data[listp++];
    err_msg2(ERROR_MISSING_ARGUM, NULL, epoint);
    return &none_value;
}

static inline void PUT_CHAR(uint32_t c) {
    uint8_t *p = (uint8_t *)return_value.u.str.data;
    if (return_value.u.str.len + 6 >= returnsize) {
        returnsize += 256;
        p = (uint8_t *)realloc(p, returnsize);
        if (!p || returnsize < 256) err_msg_out_of_memory(); /* overflow */
        return_value.u.str.data = p;
    }
    p = utf8out(c, p + return_value.u.str.len);
    return_value.u.str.len = p - return_value.u.str.data;
    return_value.u.str.chars++;
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
static void STAR_ARGS(struct DATA *p)
{
    struct value_s err;
    uval_t uval;
    if (p->star_w == FOUND) {
        const struct value_s *v = next_arg();
        if (v->obj->uval(v, &err, &uval, 8*sizeof(uval_t)-1, epoint)) {
            err_msg_wrong_type(&err, epoint);
            uval = 0;
        }
        p->width = uval;
    }
    if (p->star_p == FOUND) {
        const struct value_s *v = next_arg();
        if (v->obj->uval(v, &err, &uval, 8*sizeof(uval_t)-1, epoint)) {
            err_msg_wrong_type(&err, epoint);
            uval = 0;
        }
        p->precision = uval;
    }
}

/* for %d and friends, it puts in holder
 * the representation with the right padding
 */
static void decimal(struct DATA *p, const struct value_s *v)
{
    int minus;
    struct value_s tmp2, tmp;
    size_t i;

    if (v->obj == NONE_OBJ) int_from_int(&tmp2, 0);
    else {
        v->obj->integer(v, &tmp2, epoint);
        if (tmp2.obj != INT_OBJ) {
            err_msg_wrong_type(&tmp2, epoint);
            tmp2.obj->destroy(&tmp2);
            int_from_int(&tmp2, 0);
        }
    }

    minus = (tmp2.u.integer.len < 0);
    tmp2.obj->str(&tmp2, &tmp);

    p->width -= tmp.u.str.len - minus;
    PAD_RIGHT2(p, 0, minus);
    for (i = minus ;i < tmp.u.str.len;i++) PUT_CHAR(tmp.u.str.data[i]);
    PAD_LEFT(p);
    tmp.obj->destroy(&tmp);
    tmp2.obj->destroy(&tmp2);
}

/* for %x %X hexadecimal representation */
static void hexa(struct DATA *p, const struct value_s *v)
{
    int minus;
    struct value_s tmp;
    const char *hex = (*p->pf == 'x') ? "0123456789abcdef" : "0123456789ABCDEF";
    size_t bits;
    int bp, bp2, b;

    if (v->obj == NONE_OBJ) int_from_int(&tmp, 0);
    else {
        v->obj->integer(v, &tmp, epoint);
        if (tmp.obj != INT_OBJ) {
            err_msg_wrong_type(&tmp, epoint);
            tmp.obj->destroy(&tmp);
            int_from_int(&tmp, 0);
        }
    }

    minus = (tmp.u.integer.len < 0);
    bits = 30 * abs(tmp.u.integer.len);
    if (bits & 3) {
        bits &= ~3;
        bp = bits % 30;
        bp2 = bits / 30;
        if (bp <= (30 - 4)) b = tmp.u.integer.data[bp2] >> bp;
        else {
            b = tmp.u.integer.data[bp2] >> bp;
            if (bp2 + 1 < tmp.u.integer.len) b |= tmp.u.integer.data[bp2 + 1] << (30 - bp);
        }
    } else {
        b = 0;
        bp = bits % 30;
        bp2 = bits / 30;
    }
    while (!b && bp | bp2) {
        if (bp >= 4) bp -=4; else { bp2--; bp = 30 - 4 + bp; }
        if (bp <= (30 - 4)) b = tmp.u.integer.data[bp2] >> bp;
        else b = (tmp.u.integer.data[bp2] >> bp) | (tmp.u.integer.data[bp2 + 1] << (30 - bp));
        b &= 15;
    }
    bits = bp2 * 30 + bp + 4;

    p->width -= (bits + 3) / 4;
    PAD_RIGHT2(p, '$', minus);
    
    b = tmp.u.integer.data[bp2] >> bp;
    if (bp > 30 - 4 && bp2 + 1 < tmp.u.integer.len) b |= tmp.u.integer.data[bp2 + 1] << (30 - bp);
    PUT_CHAR(hex[b & 15]);
  
    while (bp | bp2) {
        if (bp >= 4) bp -=4; else { bp2--; bp = 30 - 4 + bp; }
        b = tmp.u.integer.data[bp2] >> bp;
        if (bp > 30 - 4) b |= (tmp.u.integer.data[bp2 + 1] << (30 - bp));
        PUT_CHAR(hex[b & 15]);
    }
    PAD_LEFT(p);
    tmp.obj->destroy(&tmp);
}

/* for %b binary representation */
static void bin(struct DATA *p, const struct value_s *v)
{
    int i;
    struct value_s err;

    if (v->obj == NONE_OBJ) int_from_int(&err, 0);
    else {
        v->obj->integer(v, &err, epoint);
        if (err.obj != INT_OBJ) {
            err_msg_wrong_type(&err, epoint);
            err.obj->destroy(&err);
            int_from_int(&err, 0);
        }
    }
    i = abs(err.u.integer.len);
    if (i) {
        int minus;
        int span, bits;
        uval_t val = err.u.integer.data[i - 1];
        minus = (err.u.integer.len < 0);
        span = 4 * sizeof(digit_t); bits = 0;
        while (span) {
            if (val >> (bits + span)) {
                bits |= span;
            }
            span >>= 1;
        }
        i = (i - 1) * 30 + bits + 1;

        p->width -= i;
        PAD_RIGHT2(p, '%', minus);
        for (i = i - 1; i >= 0; i--) {
            PUT_CHAR((err.u.integer.data[i / 30] & (1 << (i % 30))) ? '1' : '0');
        }
    } else {
        p->width -= 1;
        PAD_RIGHT2(p, '%', 0);
        PUT_CHAR('0');
    }

    PAD_LEFT(p);
    err.obj->destroy(&err);
}

/* %c chars */
static inline void chars(const struct value_s *v)
{
    uval_t uval;
    struct value_s err;

    if (v->obj == NONE_OBJ) uval = 0;
    if (v->obj->uval(v, &err, &uval, 24, epoint)) {
        err_msg_wrong_type(&err, epoint);
        uval = 0;
    }

    PUT_CHAR(uval);
}

/* %s strings */
static void strings(struct DATA *p, const struct value_s *v)
{
    int i;
    const uint8_t *tmp;
    uint32_t ch;
    struct value_s err;

    if (*p->pf == 'r') v->obj->repr(v, &err);
    else v->obj->str(v, &err);
    tmp = err.u.str.data;
    i = err.u.str.chars;
    if (p->precision != NOT_FOUND) /* the smallest number */
        i = (i < p->precision ? i : p->precision);
    p->width -= i;
    PAD_RIGHT(p);
    while (i-- > 0) { /* put the sting */
        if (*tmp & 0x80) tmp += utf8in(tmp, &ch); else ch = *tmp++;
        PUT_CHAR(ch);
    }
    PAD_LEFT(p);
    err.obj->destroy(&err);
}

/* %f or %g  floating point representation */
static void floating(struct DATA *p, const struct value_s *v)
{
    char tmp[400], *t, form[10];
    int minus;
    double d;
    struct value_s err;

    if (v->obj == NONE_OBJ) d = 0.0;
    else if (v->obj->real(v, &err, &d, epoint)) {
        err_msg_wrong_type(&err, epoint);
        d = 0.0;
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
void isnprintf(const struct value_s *v1, const struct value_s *v2, struct value_s *v, linepos_t se, linepos_t me)
{
    struct DATA data;
    char conv_field[MAX_FIELD];
    int state;
    int i;

    data.pf = (char *)v1->u.str.data;
    data.pfend = data.pf + v1->u.str.len;

    listp = 0;
    list = v2;

    epoint = me;
    return_value.u.str.data = NULL;
    return_value.u.str.len = 0;
    return_value.u.str.chars = 0;
    returnsize = 0;

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
                    STAR_ARGS(&data);
                    floating(&data, next_arg());
                    state = 0;
                    break;
                case 'd':  /* decimal */
                    STAR_ARGS(&data);
                    decimal(&data, next_arg());
                    state = 0;
                    break;
                case 'x':
                case 'X':  /* hexadecimal */
                    STAR_ARGS(&data);
                    hexa(&data, next_arg());
                    state = 0;
                    break;
                case 'b':  /* binary */
                    STAR_ARGS(&data);
                    bin(&data, next_arg());
                    state = 0;
                    break;
                case 'c': /* character */
                    chars(next_arg());
                    state = 0;
                    break;
                case 'r':  /* repr */
                case 's':  /* string */
                    STAR_ARGS(&data);
                    strings(&data, next_arg());
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
                        err_msg_not_defined(&msg, se);
                    }
                    /* is this an error ? maybe bail out */
                    state = 0;
                    break;
                } /* end switch */
            } /* end of for state */
        } else { /* not % */
            PUT_CHAR(*data.pf);  /* add the char the string */
        }
    }
    if (v == v1) free((uint8_t *)v1->u.str.data);
    v->obj = STR_OBJ;
    v->u.str.data = return_value.u.str.data;
    v->u.str.len = return_value.u.str.len;
    v->u.str.chars = return_value.u.str.chars;
    return;
}

