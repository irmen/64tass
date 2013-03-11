/*
   Unix snprintf implementation.
   Version 1.3

   Adapted for use in 64tass by Soci/Singular

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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
#include <stdlib.h> 
#include "isnprintf.h"
#include "misc.h"
#include "error.h"
#include "eval.h"

static struct value_s none_value = {T_NONE, 0, {}};
static struct value_s return_value = {T_STR, 0, {}};
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
    uint8_t *p;
    if (return_value.u.str.len + 6 >= returnsize) {
        returnsize += 256;
        return_value.u.str.data = realloc(return_value.u.str.data, returnsize);
        if (!return_value.u.str.data) err_msg_out_of_memory();
    }
    p = utf8out(c, return_value.u.str.data + return_value.u.str.len);
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

static uval_t asint(const struct value_s *v, int *minus)
{
    uval_t d;
    switch (v->type) {
    case T_SINT: 
        if ((ival_t)v->u.num.val < 0) {
            d = (uval_t)-v->u.num.val; *minus = 1;
        } else {
            d = (uval_t)v->u.num.val; *minus = 0;
        }
        break;
    case T_LABEL:
    case T_NUM:
    case T_UINT:
    case T_BOOL: d = (uval_t)v->u.num.val; *minus = 0;break;
    case T_FLOAT: 
        if ((ival_t)v->u.num.val < 0) {
            d = (uval_t)-v->u.real; *minus = 1;
        } else {
            d = (uval_t)v->u.real; *minus = 0;
        }
        break;
    default: err_msg_wrong_type(v, epoint);
    case T_NONE: *minus = -1; d = 0; break;
    }
    return d;
}

/* if width and prec. in the args */
static void STAR_ARGS(struct DATA *p)
{
    uval_t v;
    int minus;
    if (p->star_w == FOUND) {
        v = asint(next_arg(), &minus);
        if (minus < 0) return;
        p->width = v;
    }
    if (p->star_p == FOUND) {
        v = asint(next_arg(), &minus);
        if (minus < 0) return;
        p->precision = v;
    }
}

/* for %d and friends, it puts in holder
 * the representation with the right padding
 */
static void decimal(struct DATA *p, const struct value_s *v)
{
    char tmp[100], *t;
    int minus;
    uval_t d;

    d = asint(v, &minus);
    if (minus < 0) return;
    sprintf(tmp, "%" PRIuval, d);

    p->width -= strlen(tmp);
    PAD_RIGHT2(p, 0, minus);
    t = tmp;
    while (*t) { /* the integral */
        PUT_CHAR(*t);
        t++;
    }
    PAD_LEFT(p);
}

/* for %x %X hexadecimal representation */
static void hexa(struct DATA *p, const struct value_s *v)
{
    char tmp[100], *t;
    int minus;
    uval_t d;

    d = asint(v, &minus);
    if (minus < 0) return;
    sprintf(tmp, (*p->pf == 'x') ? "%" PRIxval : "%" PRIXval, d);

    p->width -= strlen(tmp);
    PAD_RIGHT2(p, '$', minus);
    t = tmp;
    while (*t) { /* hexa */
        PUT_CHAR(*t);
        t++;
    }
    PAD_LEFT(p);
}

/* for %b binary representation */
static void bin(struct DATA *p, const struct value_s *v)
{
    char tmp[100], *t;
    int minus;
    int i, j;
    uval_t d;

    d = asint(v, &minus);
    if (minus < 0) return;
    for (i = get_val_len2(v) - 1, j = 0; i >= 0; i--, j++) {
        tmp[j] = (d & (1 << i)) ? '1' : '0';
        if (!j && tmp[j] == '0' && i) j--;
    }
    tmp[j] = 0;

    p->width -= j;
    PAD_RIGHT2(p, '%', minus);
    t = tmp;
    while (*t) { /* bin */
        PUT_CHAR(*t);
        t++;
    }
    PAD_LEFT(p);
}

/* %c chars */
static inline void chars(const struct value_s *v)
{
    uint32_t ch;
    switch (v->type) {
    case T_SINT: if ((ival_t)v->u.num.val < 0) {err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);return;}
    case T_LABEL:
    case T_NUM:
    case T_UINT:
    case T_BOOL: 
        if ((uval_t)v->u.num.val & ~(uval_t)0xffffff) {err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);return;}
        PUT_CHAR(v->u.num.val); return;
    case T_FLOAT:
        if (v->u.real < 0.0 || ((uval_t)v->u.real & ~(uval_t)0xffffff)) {err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);return;}
        PUT_CHAR(v->u.real); return;
    case T_STR:
        if (v->u.str.chars != 1) {err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);return;}
        if (v->u.str.data[0] & 0x80) utf8in(v->u.str.data, &ch); else ch = v->u.str.data[0];
        PUT_CHAR(ch); return;
    default: err_msg_wrong_type(v, epoint);
    case T_NONE: return;
    }
}

/* %s strings */
static void strings(struct DATA *p, const struct value_s *v)
{
    int i;
    uint8_t *tmp;
    uint32_t ch;

    switch (v->type) {
    case T_STR: break;
    default: err_msg_wrong_type(v, epoint);
    case T_NONE: return;
    }

    tmp = v->u.str.data;
    i = v->u.str.chars;
    if (p->precision != NOT_FOUND) /* the smallest number */
        i = (i < p->precision ? i : p->precision);
    p->width -= i;
    PAD_RIGHT(p);
    while (i-- > 0) { /* put the sting */
        if (*tmp & 0x80) tmp += utf8in(tmp, &ch); else ch = *tmp++;
        PUT_CHAR(ch);
    }
    PAD_LEFT(p);
}

/* %f or %g  floating point representation */
static void floating(struct DATA *p, const struct value_s *v)
{
    char tmp[100], *t, form[10];
    int minus;
    double d;

    switch (v->type) {
    case T_SINT: if ((ival_t)v->u.num.val < 0) {d = (uval_t)-v->u.num.val;minus = 1;} else {d = (uval_t)v->u.num.val; minus = 0;} break;
    case T_LABEL:
    case T_NUM:
    case T_UINT:
    case T_BOOL: d = (uval_t)v->u.num.val; minus = 0; break;
    case T_FLOAT: if (v->u.real < 0.0) { d = -v->u.real; minus = 1;} else { d = v->u.real; minus = 0; } break;
    default: err_msg_wrong_type(v, epoint);
    case T_NONE: return;
    }
    if (p->precision == NOT_FOUND) p->precision = 6;
    t = form;
    *t++ = '%';
    if (p->square == FOUND) *t++ = '#';
    *t++ = '.';
    *t++ = '*';
    *t++ = *p->pf;
    *t++ = 0;
    sprintf(tmp, form, (p->precision < 80) ? p->precision : 80, d);

    p->width -= strlen(tmp);
    PAD_RIGHT2(p, 0, minus);
    t = tmp;
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

struct value_s *isnprintf(const struct value_s *fstr, const struct value_s *l, linepos_t me)
{
    struct DATA data;
    char conv_field[MAX_FIELD];
    int state;
    int i;

    data.pf = (char *)fstr->u.str.data;
    data.pfend = data.pf + fstr->u.str.len;

    listp = 0;
    list = l;

    epoint = me;
    return_value.type = T_STR;
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
                    err_msg(ERROR___NOT_DEFINED,"formatting character used");
                    /* is this an error ? maybe bail out */
                    state = 0;
                    break;
                } /* end switch */
            } /* end of for state */
        } else { /* not % */
            PUT_CHAR(*data.pf);  /* add the char the string */
        }
    }
    return &return_value;
}

