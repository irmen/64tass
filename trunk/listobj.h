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
#ifndef _LISTOBJ_H
#define _LISTOBJ_H

extern obj_t LIST_OBJ;
extern obj_t TUPLE_OBJ;
extern obj_t ADDRLIST_OBJ;
extern obj_t COLONLIST_OBJ;

typedef struct {
    size_t len;
    value_t *data;
    value_t val[5];
} list_t;

extern void listobj_init(void);

extern value_t *list_create_elements(value_t, size_t);
#endif
