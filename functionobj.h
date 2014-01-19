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
#ifndef _FUNCTIONOBJ_H
#define _FUNCTIONOBJ_H
struct values_s;

extern obj_t FUNCTION_OBJ;

typedef void (*function_fn_t)(struct values_s *, unsigned int);

typedef struct {
    str_t name;
    function_fn_t call;
} function_t;

struct builtin_functions_s {
    const char *name;
    function_fn_t call;
};

extern struct builtin_functions_s builtin_functions[];

extern void functionobj_init(void);

#endif
