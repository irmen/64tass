/*
    $Id: operobj.h 1943 2019-08-31 09:05:47Z soci $

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
#ifndef OPEROBJ_H
#define OPEROBJ_H
#include "obj.h"
#include "oper_e.h"

extern struct Type *const OPER_OBJ;

typedef struct Oper {
    Obj v;
    const char *name;
    Oper_types op;
    unsigned int prio;
    unsigned int len;
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
extern Oper o_ASSIGN;
extern Oper o_COLON_ASSIGN;
extern Oper o_MIN_ASSIGN;
extern Oper o_MAX_ASSIGN;
extern Oper o_OR_ASSIGN;
extern Oper o_XOR_ASSIGN;
extern Oper o_AND_ASSIGN;
extern Oper o_BLS_ASSIGN;
extern Oper o_BRS_ASSIGN;
extern Oper o_ADD_ASSIGN;
extern Oper o_SUB_ASSIGN;
extern Oper o_MUL_ASSIGN;
extern Oper o_DIV_ASSIGN;
extern Oper o_MOD_ASSIGN;
extern Oper o_EXP_ASSIGN;
extern Oper o_CONCAT_ASSIGN;
extern Oper o_X_ASSIGN;
extern Oper o_MEMBER_ASSIGN;
extern Oper o_LOR_ASSIGN;
extern Oper o_LAND_ASSIGN;
extern Oper o_COND_ASSIGN;
extern Oper o_QUEST;
extern Oper o_COLON;
extern Oper o_COND;
extern Oper o_COLON2;
extern Oper o_HASH;
extern Oper o_HASH_SIGNED;
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
extern Oper o_MIN;
extern Oper o_MAX;
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
