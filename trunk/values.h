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
#include "error.h"
#include "obj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "bytesobj.h"
#include "strobj.h"
#include "registerobj.h"
#include "listobj.h"
#include "codeobj.h"
#include "addressobj.h"
#include "functionobj.h"
#include "dictobj.h"

struct memblocks_s;

enum oper_e {
    O_FUNC,          /* a(    */
    O_INDEX,         /* a[    */
    O_BRACE,         /* {a}   */
    O_BRACKET,       /* [a]   */
    O_PARENT,        /* (a)   */
    O_COMMA,         /* ,     */
    O_COND,          /* ?     */
    O_COLON2,        /* :     */
    O_LOR,           /* ||    */
    O_LXOR,          /* ^^    */
    O_LAND,          /* &&    */

    O_HASH,          /* #     */
    O_COMMAX,        /* ,x    */
    O_COMMAY,        /* ,y    */
    O_COMMAZ,        /* ,z    */
    O_COMMAR,        /* ,r    */
    O_COMMAS,        /* ,s    */
    O_COMMAD,        /* ,d    */
    O_COMMAB,        /* ,b    */
    O_COMMAK,        /* ,k    */
    O_WORD,          /* <>    */
    O_HWORD,         /* >`    */
    O_BSWORD,        /* ><    */
    O_LOWER,         /* <     */
    O_HIGHER,        /* >     */
    O_BANK,          /* `     */
    O_STRING,        /* ^     */
    O_SPLAT,         /* *     */
    O_NEG,           /* -     */
    O_POS,           /* +     */
    O_INV,           /* ~     */
    O_LNOT,          /* !     */

    O_CMP,           /* <=>   */
    O_EQ,            /* =     */
    O_NE,            /* !=    */
    O_LT,            /* <     */
    O_GT,            /* >     */
    O_GE,            /* >=    */
    O_LE,            /* <=    */
    O_OR,            /* |     */
    O_XOR,           /* ^     */
    O_AND,           /* &     */
    O_LSHIFT,        /* <<    */
    O_RSHIFT,        /* >>    */
    O_ADD,           /* +     */
    O_SUB,           /* -     */
    O_MUL,           /* *     */
    O_DIV,           /* /     */
    O_MOD,           /* %     */
    O_EXP,           /* **    */
    O_MEMBER,        /* .     */
    O_CONCAT,        /* ..    */

    O_X,             /* x     */
    O_IN,            /* in    */

    O_TUPLE,         /* )     */
    O_LIST,          /* ]     */
    O_DICT,          /* }     */
    O_RPARENT,       /* )     */
    O_RBRACKET,      /* ]     */
    O_RBRACE,        /* }     */
    O_QUEST,         /* ?     */
    O_COLON          /* :     */
};

struct oper_s {
    value_t op;
    value_t v1;
    value_t v2;
    linepos_t epoint;
    linepos_t epoint2;
    linepos_t epoint3;
};
typedef struct oper_s *oper_t;

struct value_s {
    obj_t obj;
    size_t refcount;
    union {
        integer_t integer;
        addrs_t addr;
        ustr_t str;
        ureg_t reg;
        bytes_t bytes;
        bits_t bits;
        code_t code;
        list_t list;
        dict_t dict;
        mfunc_t mfunc;
        macro_t macro;
        struct {
            line_t sline;
            size_t waitforp;
            const struct file_list_s *file_list;
            const struct label_s *parent;
        } lbl;
        struct {
            const char *name;
            enum oper_e op;
            int prio;
        } oper;
        struct {
            enum errors_e num;
            struct linepos_s epoint;
            union {
                str_t ident;
                struct {
                    value_t op;
                    value_t v1;
                    value_t v2;
                } invoper;
                struct {
                    str_t ident;
                    const struct label_s *label;
                    int down;
                } notdef;
                int bits;
                const char *objname;
                atype_t addressing;
                value_t reg;
                size_t opers;
                struct {
                    size_t v1;
                    size_t v2;
                } broadcast;
            } u;
        } error;
        struct {
            str_t name;
            struct linepos_s epoint;
        } ident;
        struct {
            int32_t count;
            struct linepos_s epoint;
        } anonident;
        struct {
            void *iter;
            size_t val;
            value_t data;
        } iter;
        struct {
            size_t len;
            struct values_s *val;
        } funcargs;
        double real;
        int boolean;
        function_t function;
        obj_t type;
    } u;
};

extern value_t val_alloc(obj_t);
extern void val_destroy(value_t);
extern void val_replace(value_t *, value_t);
extern int val_print(const value_t, FILE *);

extern value_t int_value[2];
extern value_t none_value;
extern value_t true_value;
extern value_t false_value;
extern value_t gap_value;
extern value_t default_value;
extern value_t null_str;
extern value_t null_bytes;
extern value_t inv_bytes;
extern value_t null_bits;
extern value_t inv_bits;
extern value_t null_tuple;
extern value_t null_list;
extern value_t null_addrlist;

extern struct value_s o_TUPLE;
extern struct value_s o_LIST;
extern struct value_s o_DICT;
extern struct value_s o_RPARENT;
extern struct value_s o_RBRACKET;
extern struct value_s o_RBRACE;
extern struct value_s o_FUNC;
extern struct value_s o_INDEX;
extern struct value_s o_BRACE;
extern struct value_s o_BRACKET;
extern struct value_s o_PARENT;
extern struct value_s o_COMMA;
extern struct value_s o_QUEST;
extern struct value_s o_COLON;
extern struct value_s o_COND;
extern struct value_s o_COLON2;
extern struct value_s o_HASH;
extern struct value_s o_COMMAX;
extern struct value_s o_COMMAY;
extern struct value_s o_COMMAZ;
extern struct value_s o_COMMAS;
extern struct value_s o_COMMAR;
extern struct value_s o_COMMAD;
extern struct value_s o_COMMAB;
extern struct value_s o_COMMAK;
extern struct value_s o_WORD;
extern struct value_s o_HWORD;
extern struct value_s o_BSWORD;
extern struct value_s o_LOWER;
extern struct value_s o_HIGHER;
extern struct value_s o_BANK;
extern struct value_s o_STRING;
extern struct value_s o_LOR;
extern struct value_s o_LXOR;
extern struct value_s o_LAND;
extern struct value_s o_IN;
extern struct value_s o_CMP;
extern struct value_s o_EQ;
extern struct value_s o_NE;
extern struct value_s o_LT;
extern struct value_s o_GT;
extern struct value_s o_GE;
extern struct value_s o_LE;
extern struct value_s o_OR;
extern struct value_s o_XOR;
extern struct value_s o_AND;
extern struct value_s o_LSHIFT;
extern struct value_s o_RSHIFT;
extern struct value_s o_ADD;
extern struct value_s o_SUB;
extern struct value_s o_MUL;
extern struct value_s o_DIV;
extern struct value_s o_MOD;
extern struct value_s o_EXP;
extern struct value_s o_SPLAT;
extern struct value_s o_NEG;
extern struct value_s o_POS;
extern struct value_s o_INV;
extern struct value_s o_LNOT;
extern struct value_s o_CONCAT;
extern struct value_s o_X;
extern struct value_s o_MEMBER;

extern void destroy_values(void);
extern void init_values(void);

static inline void obj_destroy(value_t v1) {
    v1->obj->destroy(v1);
}

static inline int obj_same(const value_t v1, const value_t v2) {
    return v1->obj->same(v1, v2);
}

static inline MUST_CHECK value_t obj_hash(const value_t v1, int *hs, linepos_t epoint) {
    return v1->obj->hash(v1, hs, epoint);
}

static inline value_t val_reference(value_t v1) {
    v1->refcount++; return v1;
}

static inline MUST_CHECK value_t truth_reference(int i) {
    return val_reference(i ? true_value : false_value);
}

static inline value_t obj_next(value_t v1) {
    return v1->obj->next(v1);
}
#endif
