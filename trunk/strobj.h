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
    uint8_t *data;
    uint8_t val[16];
} ustr_t;

extern void strobj_init(void);
extern MUST_CHECK value_t str_from_str(const uint8_t *, size_t *);
extern MUST_CHECK value_t float_from_str(const value_t, linepos_t);
extern uint8_t *str_create_elements(value_t, size_t);
extern size_t str_quoting(const value_t, char *);
#endif
