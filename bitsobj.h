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
#ifndef _BITSOBJ_H
#define _BITSOBJ_H

extern obj_t BITS_OBJ;

typedef uint16_t bdigit_t;
typedef struct {
    size_t len;
    size_t bits;
    int inv;
    bdigit_t val[6];
    bdigit_t *data;
} bits_t;

extern size_t bits_from_hexstr(struct value_s *, const uint8_t *);
extern size_t bits_from_binstr(struct value_s *, const uint8_t *);
extern int bits_from_str(struct value_s *, const struct value_s *);
extern void bits_from_bytes(struct value_s *, const struct value_s *);
extern void bits_from_u8(struct value_s *, uint8_t);
extern void bits_from_u16(struct value_s *, uint16_t);
extern void bits_from_bools(struct value_s *, int, int);

extern void bitsobj_init(void);
#endif
