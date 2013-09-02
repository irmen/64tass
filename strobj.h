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
#ifndef _STROBJ_H
#define _STROBJ_H

extern obj_t STR_OBJ;

typedef struct {
    size_t len;
    size_t chars;
    uint8_t val[4];
    uint8_t *data;
} ustr_t;

extern void strobj_init(void);
extern size_t str_from_str(struct value_s *, size_t *, const uint8_t *);
#endif
