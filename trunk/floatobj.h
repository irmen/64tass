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
#ifndef _FLOATOBJ_H
#define _FLOATOBJ_H
extern obj_t FLOAT_OBJ;

extern void floatobj_init(void);

extern MUST_CHECK value_t calc2_double(oper_t, double, double);
extern MUST_CHECK value_t float_from_double(double);
extern MUST_CHECK value_t float_from_double2(double, linepos_t);

#endif
