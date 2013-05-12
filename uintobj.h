/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#ifndef _UINTOBJ_H
#define _UINTOBJ_H
enum oper_e;

extern obj_t UINT_OBJ;

extern void uintobj_init(void);
extern int calc2_uint_uint(oper_t, uval_t, uval_t);
extern int calc2_uint_sint(oper_t, uval_t, ival_t);
extern int calc1_uint(oper_t, uval_t);
#endif
