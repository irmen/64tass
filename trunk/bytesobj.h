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
#ifndef _BYTESOBJ_H
#define _BYTESOBJ_H

extern obj_t BYTES_OBJ;

typedef struct {
    ssize_t len;
    uint8_t *data;
    uint8_t val[20];
} bytes_t;

extern void bytesobj_init(void);

extern MUST_CHECK value_t bytes_from_u8(uint8_t);
extern MUST_CHECK value_t bytes_from_u16(uint16_t);
extern MUST_CHECK value_t bytes_from_str(const value_t, linepos_t);
extern MUST_CHECK value_t float_from_bytes(const value_t, linepos_t);
#endif
