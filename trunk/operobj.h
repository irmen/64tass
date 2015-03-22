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
#ifndef _OPEROBJ_H
#define _OPEROBJ_H
#include "obj.h"

extern struct Type *OPER_OBJ;

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

typedef struct Oper {
    Obj v;
    const char *name;
    enum oper_e op;
    int prio;
} Oper;

extern void operobj_init(void);

extern Oper o_TUPLE;
extern Oper o_LIST;
extern Oper o_DICT;
extern Oper o_RPARENT;
extern Oper o_RBRACKET;
extern Oper o_RBRACE;
extern Oper o_FUNC;
extern Oper o_INDEX;
extern Oper o_BRACE;
extern Oper o_BRACKET;
extern Oper o_PARENT;
extern Oper o_COMMA;
extern Oper o_QUEST;
extern Oper o_COLON;
extern Oper o_COND;
extern Oper o_COLON2;
extern Oper o_HASH;
extern Oper o_COMMAX;
extern Oper o_COMMAY;
extern Oper o_COMMAZ;
extern Oper o_COMMAS;
extern Oper o_COMMAR;
extern Oper o_COMMAD;
extern Oper o_COMMAB;
extern Oper o_COMMAK;
extern Oper o_WORD;
extern Oper o_HWORD;
extern Oper o_BSWORD;
extern Oper o_LOWER;
extern Oper o_HIGHER;
extern Oper o_BANK;
extern Oper o_STRING;
extern Oper o_LOR;
extern Oper o_LXOR;
extern Oper o_LAND;
extern Oper o_IN;
extern Oper o_CMP;
extern Oper o_EQ;
extern Oper o_NE;
extern Oper o_LT;
extern Oper o_GT;
extern Oper o_GE;
extern Oper o_LE;
extern Oper o_OR;
extern Oper o_XOR;
extern Oper o_AND;
extern Oper o_LSHIFT;
extern Oper o_RSHIFT;
extern Oper o_ADD;
extern Oper o_SUB;
extern Oper o_MUL;
extern Oper o_DIV;
extern Oper o_MOD;
extern Oper o_EXP;
extern Oper o_SPLAT;
extern Oper o_NEG;
extern Oper o_POS;
extern Oper o_INV;
extern Oper o_LNOT;
extern Oper o_CONCAT;
extern Oper o_X;
extern Oper o_MEMBER;

#endif
