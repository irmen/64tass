/*
    $Id: errorobj.h 2808 2022-10-17 04:49:11Z soci $

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
#include "oper_e.h"
#include "stdbool.h"

extern struct Type *const ERROR_OBJ;

typedef struct Error {
    Obj v;
    Error_types num;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
    linecpos_t caret;
    const uint8_t *line;
    union {
        struct {
            Oper_types op;
            Obj *v1;
            Obj *v2;
        } invoper;
        struct {
            Obj *symbol;
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
        struct {
            uint32_t am, cod;
        } addressing;
        struct {
            struct Register *reg;
            uint32_t cod;
        } reg;
        struct {
            size_t num;
            uint32_t cod;
        } opers;
        struct {
            uint32_t cod;
        } addresssize;
        struct {
            size_t v1;
            size_t v2;
        } broadcast;
        struct {
            argcount_t num, min, max;
        } argnum;
        struct {
            const struct Type *t1;
            const struct Type *t2;
        } otype;
    } u;
} Error;

#define Error(a) OBJ_CAST(Error, a)

extern void errorobj_init(void);

extern MALLOC Error *new_error(Error_types, linepos_t);
extern MALLOC Obj *new_error_mem(linepos_t);
extern MALLOC Obj *new_error_obj(Error_types, Obj *, linepos_t);
extern MALLOC Obj *new_error_conv(Obj *, struct Type *, linepos_t);
extern MALLOC Obj *new_error_argnum(argcount_t, argcount_t, argcount_t, linepos_t);
extern void error_obj_update(Error *, const Obj *, Obj *);

#endif
