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

enum type_e {
    T_NONE, T_BOOL, T_NUM, T_UINT, T_SINT, T_FLOAT, T_STR, T_GAP, T_ADDRESS,
    T_IDENT, T_ANONIDENT, T_IDENTREF, T_ERROR, T_OPER, T_TUPLE, T_LIST,
    T_MACRO, T_SEGMENT, T_UNION, T_STRUCT, T_FUNCTION, T_CODE, T_LBL, T_DEFAULT
};

#define type_is_int(a) ((a) >= T_BOOL && (a) <= T_SINT) 
#define type_is_num(a) ((a) >= T_BOOL && (a) <= T_FLOAT) 

enum oper_e {
    O_SEPARATOR,     /* ,     */
    O_FUNC,          /* a(    */
    O_INDEX,         /* a[    */
    O_SLICE,         /* a[x:  */
    O_SLICE2,        /* a[x:: */
    O_BRACKET,       /* [a]   */
    O_PARENT,        /* (a)   */
    O_COMMA,         /* ,     */
    O_COND,          /* ?     */
    O_COLON2,        /* :     */
    O_COLON3,        /* :     */
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
    O_LOR,           /* ||    */
    O_LXOR,          /* ^^    */
    O_LAND,          /* &&    */
    O_IN,            /* in    */
    O_EQ,            /* =     */
    O_NEQ,           /* !=    */
    O_LT,            /* <     */
    O_GT,            /* >     */
    O_GE,            /* >=    */
    O_LE,            /* <=    */
    O_OR,            /* |     */
    O_XOR,           /* ^     */
    O_AND,           /* &     */
    O_LSHIFT,        /* <<    */
    O_ASHIFT,        /* >>    */
    O_RSHIFT,        /* >>>   */
    O_ADD,           /* +     */
    O_SUB,           /* -     */
    O_MUL,           /* *     */
    O_DIV,           /* /     */
    O_MOD,           /* %     */
    O_EXP,           /* **    */
    O_X,             /* x     */
    O_CONCAT,        /* ..    */
    O_MEMBER,        /* .     */
    O_NEG,           /* -     */
    O_POS,           /* +     */
    O_INV,           /* ~     */
    O_LNOT,          /* !     */

    O_TUPLE,         /* )     */
    O_LIST,          /* ]     */
    O_RPARENT,       /* )     */
    O_RBRACKET,      /* ]     */
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

struct value_s {
    enum type_e type;
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
            size_t size;
            uint8_t pass;
            signed char dtype;
            address_t addr;
            size_t memp;
            size_t membp;
        } code;
        struct {
            size_t len;
            struct value_s **data;
        } list;
        struct {
            size_t p;
            struct file_list_s *file_list;
            struct label_s *context;
            line_t sline;
            size_t argc;
            struct {
                str_t name;
                struct value_s *init;
                linepos_t epoint;
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
            enum oper_e op;
            int prio;
        } oper;
        struct {
            int num;
            linepos_t epoint;
            union {
                str_t ident;
            } u;
        } error;
        struct {
            str_t name;
            linepos_t epoint;
        } ident;
        struct {
            struct label_s *label;
            linepos_t epoint;
        } identref;
        struct {
            int32_t count;
            linepos_t epoint;
        } anonident;
        double real;
    } u;
};

extern struct value_s *val_alloc(void);
extern void val_destroy(struct value_s *);
extern void val_destroy2(struct value_s *);
extern void val_replace(struct value_s **, struct value_s *);
extern void val_replace_template(struct value_s **, const struct value_s *);
extern struct value_s *val_realloc(struct value_s **);
extern void val_set_template(struct value_s **, const struct value_s *);
extern int val_same(const struct value_s *, const struct value_s *);
extern int val_truth(const struct value_s *);
extern struct value_s *val_reference(struct value_s *);
extern void val_print(const struct value_s *, FILE *);

extern void destroy_values(void);
extern void init_values(void);
#endif