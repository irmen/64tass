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
#ifndef _ADDRESSOBJ_H
#define _ADDRESSOBJ_H

extern obj_t ADDRESS_OBJ;

enum atype_e {
    A_NONE,          /*       */
    A_IMMEDIATE,     /* #     */
    A_XR,            /* ,x    */
    A_YR,            /* ,y    */
    A_ZR,            /* ,z    */
    A_RR,            /* ,r    */
    A_SR,            /* ,s    */
    A_DR,            /* ,d    */
    A_BR,            /* ,b    */
    A_KR,            /* ,k    */
    A_I,             /* )     */
    A_LI             /* ]     */
};

typedef uint32_t atype_t;
typedef struct {
    atype_t type;
    value_t val;
} addrs_t;

extern void addressobj_init(void);
#endif
