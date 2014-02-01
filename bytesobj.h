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
    size_t len;
    uint8_t val[24];
    uint8_t *data;
} bytes_t;

extern void bytesobj_init(void);

extern int bytes_from_str(struct value_s *, const struct value_s *);
#endif
