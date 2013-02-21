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
#ifndef _VALUES_H_
#define _VALUES_H_
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include "inttypes.h"

typedef int32_t ival_t;
#define PRIdval PRId32
typedef uint32_t uval_t;
#define PRIxval PRIx32
#define PRIuval PRIu32

enum type_e {
    T_NONE, T_BOOL, T_NUM, T_UINT, T_SINT, T_FLOAT, T_STR, T_GAP, T_IDENT,
    T_IDENTREF, T_FORWR, T_BACKR, T_UNDEF, T_OPER, T_TUPLE, T_LIST, T_DEFAULT,
};

#define type_is_int(a) ((a) >= T_BOOL && (a) <= T_SINT) 
#define type_is_num(a) ((a) >= T_BOOL && (a) <= T_FLOAT) 

enum oper_e {
    O_SEPARATOR,     // ,
    O_FUNC,          // a(
    O_INDEX,         // a[
    O_SLICE,         // a[x:
    O_SLICE2,        // a[x::
    O_BRACKET,       // [a]
    O_PARENT,        // (a)
    O_COMMA,         // ,
    O_COND,          // ?
    O_COLON2,        // :
    O_COLON3,        // :
    O_WORD,          // <>
    O_HWORD,         // >`
    O_BSWORD,        // ><
    O_LOWER,         // <
    O_HIGHER,        // >
    O_BANK,          // `
    O_STRING,        // ^
    O_LOR,           // ||
    O_LXOR,          // ^^
    O_LAND,          // &&
    O_EQ,            // =
    O_NEQ,           // !=
    O_LT,            // <
    O_GT,            // >
    O_GE,            // >=
    O_LE,            // <=
    O_OR,            // |
    O_XOR,           // ^
    O_AND,           // &
    O_LSHIFT,        // <<
    O_ASHIFT,        // >>
    O_RSHIFT,        // >>>
    O_ADD,           // +
    O_SUB,           // -
    O_MUL,           // *
    O_DIV,           // /
    O_MOD,           // %
    O_EXP,           // **
    O_MEMBER,        // .
    O_NEG,           // -
    O_POS,           // +
    O_INV,           // ~
    O_LNOT,          // !

    O_TUPLE,         // )
    O_LIST,          // ]
    O_RPARENT,       // )
    O_RBRACKET,      // ]
    O_QUEST,         // ?
    O_COLON,         // :
};

struct value_s {
    enum type_e type;
    size_t refcount;
    union {
        struct {
            uint8_t len;
            ival_t val;
        } num;
        struct {
            size_t len;
            size_t chars;
            uint8_t *data;
        } str;
        struct {
            size_t len;
            const uint8_t *name;
            struct label_s *label;
        } ident;
        struct {
            size_t len;
            struct value_s **data;
        } list;
        enum oper_e oper;
        uint8_t ref;
        double real;
    } u;
};

extern void val_destroy(struct value_s *);
extern void val_replace(struct value_s **, struct value_s *);
extern int val_equal(const struct value_s *, const struct value_s *);
extern int val_truth(const struct value_s *);
extern struct value_s *val_reference(struct value_s *);
extern void val_print(const struct value_s *, FILE *);

extern void destroy_values(void);
extern void init_values(void);
#endif
