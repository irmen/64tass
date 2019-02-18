/*
    $Id: errorobj.h 1885 2019-02-10 15:05:45Z soci $

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
#ifndef ERROROBJ_H
#define ERROROBJ_H
#include "obj.h"
#include "errors_e.h"
#include "str.h"
#include "stdbool.h"

extern struct Type *const ERROR_OBJ;

typedef struct Error {
    Obj v;
    Error_types num;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
    const uint8_t *line;
    union {
        struct {
            const struct Oper *op;
            Obj *v1;
            Obj *v2;
        } invoper;
        struct {
            Obj *ident;
            struct Namespace *names;
            bool down;
        } notdef;
        struct {
            unsigned int bits;
            Obj *val;
        } intconv;
        struct {
            const struct Type *t;
            Obj *val;
        } conv;
        Obj *obj;
        uint32_t addressing;
        struct Register *reg;
        size_t opers;
        struct {
            size_t v1;
            size_t v2;
        } broadcast;
    } u;
} Error;

extern void errorobj_init(void);

extern MALLOC Error *new_error(Error_types, linepos_t);
extern MALLOC Error *new_error_mem(linepos_t);
extern MALLOC Error *new_error_obj(Error_types, Obj *, linepos_t);
extern MALLOC Error *new_error_conv(Obj *, struct Type *, linepos_t);

#endif
