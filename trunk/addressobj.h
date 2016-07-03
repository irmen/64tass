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
#ifndef ADDRESSOBJ_H
#define ADDRESSOBJ_H
#include "obj.h"
#include "values.h"

extern struct Type *ADDRESS_OBJ;

enum atype_e {
    A_NONE,             /*       */
    A_IMMEDIATE,        /* #     */
    A_IMMEDIATE_SIGNED, /* #+    */
    A_XR,               /* ,x    */
    A_YR,               /* ,y    */
    A_ZR,               /* ,z    */
    A_RR,               /* ,r    */
    A_SR,               /* ,s    */
    A_DR,               /* ,d    */
    A_BR,               /* ,b    */
    A_KR,               /* ,k    */
    A_I,                /* )     */
    A_LI                /* ]     */
};

typedef uint32_t atype_t;
typedef struct Address {
    Obj v;
    atype_t type;
    Obj *val;
} Address; 

extern void addressobj_init(void);
extern void addressobj_names(void);

static inline MUST_CHECK Address *new_address(Obj *val, atype_t type) {
    Address *v = (Address *)val_alloc(ADDRESS_OBJ);
    v->val = val;
    v->type = type;
    return v;
}

extern bool check_addr(atype_t);
#endif
