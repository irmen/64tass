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
#include <stdio.h>
#include "inttypes.h"
#include "error.h"
#include "obj.h"
struct memblocks_s;

#define type_is_int(a) ((a) >= T_BOOL && (a) <= T_SINT) 
#define type_is_num(a) ((a) >= T_BOOL && (a) <= T_FLOAT) 

enum oper_e {
    O_SEPARATOR,     /* ,     */
    O_FUNC,          /* a(    */
    O_INDEX,         /* a[    */
    O_SLICE,         /* a[x:  */
    O_SLICE2,        /* a[x:: */
    O_BRACE,         /* {a}   */
    O_BRACKET,       /* [a]   */
    O_PARENT,        /* (a)   */
    O_COMMA,         /* ,     */
    O_COND,          /* ?     */
    O_COLON2,        /* :     */
    O_COLON3,        /* :     */
    O_LOR,           /* ||    */
    O_LXOR,          /* ^^    */
    O_LAND,          /* &&    */

    O_HASH,          /* #     */
    O_COMMAX,        /* ,x    */
    O_COMMAY,        /* ,y    */
    O_COMMAZ,        /* ,z    */
    O_COMMAR,        /* ,r    */
    O_COMMAS,        /* ,s    */
    O_WORD,          /* <>    */
    O_HWORD,         /* >`    */
    O_BSWORD,        /* ><    */
    O_LOWER,         /* <     */
    O_HIGHER,        /* >     */
    O_BANK,          /* `     */
    O_STRING,        /* ^     */
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

enum dtype_e {
    D_DINT = -4,
    D_LINT = -3,
    D_INT = -2,
    D_CHAR = -1,
    D_NONE = 0,
    D_BYTE = 1,
    D_WORD = 2,
    D_LONG = 3,
    D_DWORD = 4
};

enum atype_e {
    A_NONE,          /*       */
    A_IMMEDIATE,     /* #     */
    A_XR,            /* ,x    */
    A_YR,            /* ,y    */
    A_ZR,            /* ,z    */
    A_RR,            /* ,r    */
    A_SR,            /* ,s    */
    A_I,             /* )     */
    A_LI,            /* ]     */
};

struct pair_s {
    int hash;
    struct value_s *key;
    struct value_s *data;
    struct avltree_node node;
};

struct oper_s {
    const struct value_s *op;
    struct value_s *v1;
    struct value_s *v2;
    struct value_s *v;
    struct linepos_s epoint;
    struct linepos_s epoint2;
    struct linepos_s epoint3;
};
typedef struct oper_s *oper_t;

struct value_s {
    obj_t obj;
    size_t refcount;
    union {
        struct {
            uint8_t len;
            ival_t val;
        } num;
        struct {
            uint32_t type;
            address_t val;
            uint8_t len;
        } addr;
        struct {
            size_t len;
            size_t chars;
            const uint8_t *data;
        } str;
        struct {
            size_t len;
            const uint8_t *data;
        } bytes;
        struct {
            size_t size;
            uint8_t pass;
            signed char dtype;
            address_t addr;
            const struct memblocks_s *mem;
            size_t memp;
            size_t membp;
        } code;
        struct {
            size_t len;
            struct value_s **data;
        } list;
        struct {
            struct value_s *key;
            struct value_s *data;
        } pair;
        struct {
            size_t len;
            struct avltree members;
        } dict;
        struct {
            size_t p;
            struct file_list_s *file_list;
            struct label_s *context;
            line_t sline;
            size_t argc;
            struct {
                str_t name;
                struct value_s *init;
                struct linepos_s epoint;
            } *param; 
        } func;
        struct {
            size_t size;
            size_t p;
            struct file_list_s *file_list;
            line_t sline;
            size_t argc;
            struct {
                str_t name;
                str_t init;
            } *param; 
        } macro;
        struct {
            size_t p;
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
            int num;
            struct linepos_s epoint;
            union {
                str_t ident;
                struct {
                    const struct value_s *op;
                    struct value_s *v1;
                    struct value_s *v2;
                } invoper;
            } u;
        } error;
        struct {
            str_t name;
            struct linepos_s epoint;
        } ident;
        struct {
            struct label_s *label;
            struct linepos_s epoint;
        } identref;
        struct {
            int32_t count;
            struct linepos_s epoint;
        } anonident;
        double real;
    } u;
};

extern struct value_s *val_alloc(void);
extern void val_destroy(struct value_s *);
extern void val_replace(struct value_s **, struct value_s *);
extern void val_replace_template(struct value_s **, const struct value_s *);
extern struct value_s *val_realloc(struct value_s **);
extern void val_set_template(struct value_s **, const struct value_s *);
extern struct value_s *val_reference(struct value_s *);
extern void val_print(const struct value_s *, FILE *);
extern int val_hash(const struct value_s *);
extern int pair_compare(const struct avltree_node *, const struct avltree_node *);

extern struct value_s none_value;
extern struct value_s true_value;
extern struct value_s false_value;
extern struct value_s null_str;
extern struct value_s null_bytes;
extern struct value_s null_tuple;
extern struct value_s null_list;

extern struct value_s o_TUPLE;
extern struct value_s o_LIST;
extern struct value_s o_DICT;
extern struct value_s o_RPARENT;
extern struct value_s o_RBRACKET;
extern struct value_s o_RBRACE;
extern struct value_s o_FUNC;
extern struct value_s o_INDEX;
extern struct value_s o_SLICE;
extern struct value_s o_SLICE2;
extern struct value_s o_BRACE;
extern struct value_s o_BRACKET;
extern struct value_s o_PARENT;
extern struct value_s o_SEPARATOR;
extern struct value_s o_COMMA;
extern struct value_s o_QUEST;
extern struct value_s o_COLON;
extern struct value_s o_COND;
extern struct value_s o_COLON2;
extern struct value_s o_COLON3;
extern struct value_s o_HASH;
extern struct value_s o_COMMAX;
extern struct value_s o_COMMAY;
extern struct value_s o_COMMAZ;
extern struct value_s o_COMMAS;
extern struct value_s o_COMMAR;
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
extern struct value_s o_NEG;
extern struct value_s o_POS;
extern struct value_s o_INV;
extern struct value_s o_LNOT;
extern struct value_s o_CONCAT;
extern struct value_s o_X;
extern struct value_s o_MEMBER;

extern void destroy_values(void);
extern void init_values(void);
#endif
