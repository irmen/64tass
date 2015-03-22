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
#ifndef _VALUES_H_
#define _VALUES_H_
#include <stdio.h>
#include "inttypes.h"

extern Obj *val_alloc(obj_t);
extern void val_destroy(Obj *);
extern void val_replace(Obj **, Obj *);
extern int val_print(Obj *, FILE *);

extern void destroy_values(void);
extern void init_values(void);
extern void garbage_collect(void);
#endif
