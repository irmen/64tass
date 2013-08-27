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

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#ifndef _INTOBJ_H
#define _INTOBJ_H

extern obj_t INT_OBJ;

typedef uint32_t digit_t;
typedef uint64_t twodigits_t;
typedef struct {
    ssize_t len;
    digit_t val[2];
    digit_t *data;
} integer_t;

extern void intobj_init(void);
extern void int_from_int(struct value_s *, int);
extern void int_from_uval(struct value_s *, uval_t);
extern void int_from_ival(struct value_s *, ival_t);
extern void int_from_double(struct value_s *, double, linepos_t);
extern int int_from_str(struct value_s *, const struct value_s *);
extern void int_from_bytes(struct value_s *, const struct value_s *);
extern void int_from_bits(struct value_s *, const struct value_s *);
extern size_t int_from_decstr(struct value_s *, const uint8_t *);

#endif
