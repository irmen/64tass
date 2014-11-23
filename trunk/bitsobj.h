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

typedef digit_t bdigit_t;
typedef struct {
    ssize_t len;
    size_t bits;
    bdigit_t val[4];
    bdigit_t *data;
} bits_t;

extern MUST_CHECK value_t bits_from_hexstr(const uint8_t *, size_t *);
extern MUST_CHECK value_t bits_from_binstr(const uint8_t *, size_t *);
extern MUST_CHECK value_t bits_from_str(const value_t, linepos_t);
extern MUST_CHECK value_t bits_from_bytes(const value_t);
extern MUST_CHECK value_t ibits_from_bool(int);
extern MUST_CHECK value_t bits_from_bool(int);
extern MUST_CHECK value_t bits_from_bools(int, int);
extern MUST_CHECK value_t float_from_bits(const value_t, linepos_t);

extern void bitsobj_init(void);
#endif
