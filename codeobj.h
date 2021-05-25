/*
    $Id: codeobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef CODEOBJ_H
#define CODEOBJ_H
#include "obj.h"
#include "values.h"

extern struct Type *const CODE_OBJ;

typedef enum Code_types {
    D_DINT = -4,
    D_LINT = -3,
    D_SINT = -2,
    D_CHAR = -1,
    D_NONE = 0,
    D_BYTE = 1,
    D_WORD = 2,
    D_LONG = 3,
    D_DWORD = 4
} Code_types;

typedef struct Code {
    Obj v;
    address_t size;
    address_t addr;
    ival_t offs;
    uint8_t pass;
    uint8_t apass;
    signed char dtype;
    Obj *typ;
    struct Memblocks *memblocks;
    address_t memaddr;
    size_t membp;
    struct Namespace *names;
    uval_t requires;
    uval_t conflicts;
} Code;

#define Code(a) OBJ_CAST(Code, a)

extern void codeobj_init(void);
extern void codeobj_names(void);

static inline MUST_CHECK Code *new_code(void) {
    return Code(val_alloc(CODE_OBJ));
}

struct Error;

extern MUST_CHECK Obj *get_code_value(const Code *, linepos_t);
extern MUST_CHECK struct Error *code_uaddress(Obj *, uval_t *, uval_t *, linepos_t);
extern MUST_CHECK Obj *int_from_code(const Code *, linepos_t);
extern MUST_CHECK Obj *float_from_code(const Code *, linepos_t);
extern MUST_CHECK Obj *bits_from_code(const Code *, linepos_t);
extern MUST_CHECK Obj *bytes_from_code(const Code *, linepos_t);
extern MUST_CHECK Obj *tuple_from_code(const Code *, const struct Type *);
#endif
