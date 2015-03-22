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
#include <string.h>
#include "operobj.h"

#include "strobj.h"

static struct obj_s obj;

obj_t OPER_OBJ = &obj;

Oper o_TUPLE;
Oper o_LIST;
Oper o_DICT;
Oper o_RPARENT;
Oper o_RBRACKET;
Oper o_RBRACE;
Oper o_FUNC;
Oper o_INDEX;
Oper o_BRACE;
Oper o_BRACKET;
Oper o_PARENT;
Oper o_COMMA;
Oper o_QUEST;
Oper o_COLON;
Oper o_COND;
Oper o_COLON2;
Oper o_HASH;
Oper o_COMMAX;
Oper o_COMMAY;
Oper o_COMMAZ;
Oper o_COMMAS;
Oper o_COMMAR;
Oper o_COMMAD;
Oper o_COMMAB;
Oper o_COMMAK;
Oper o_WORD;
Oper o_HWORD;
Oper o_BSWORD;
Oper o_LOWER;
Oper o_HIGHER;
Oper o_BANK;
Oper o_STRING;
Oper o_LOR;
Oper o_LXOR;
Oper o_LAND;
Oper o_IN;
Oper o_CMP;
Oper o_EQ;
Oper o_NE;
Oper o_LT;
Oper o_GT;
Oper o_GE;
Oper o_LE;
Oper o_OR;
Oper o_XOR;
Oper o_AND;
Oper o_LSHIFT;
Oper o_RSHIFT;
Oper o_ADD;
Oper o_SUB;
Oper o_MUL;
Oper o_DIV;
Oper o_MOD;
Oper o_EXP;
Oper o_SPLAT;
Oper o_NEG;
Oper o_POS;
Oper o_INV;
Oper o_LNOT;
Oper o_CONCAT;
Oper o_X;
Oper o_MEMBER;

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint) {
    Oper *v1 = (Oper *)o1;
    const char *txt;
    size_t len;
    uint8_t *s;
    Str *v;
    if (!epoint) return NULL;
    v = new_str();
    txt = v1->name;
    len = strlen(txt);
    s = str_create_elements(v, len + 8);
    memcpy(s, "<oper ", 6);
    memcpy(s + 6, txt, len);
    len += 6;
    s[len++] = '\'';
    s[len++] = '>';
    v->data = s;
    v->len = len;
    v->chars = len;
    return (Obj *)v;
}

static inline void oper_init(Oper *o, const char *name, enum oper_e op, int prio) {
    o->v.obj = OPER_OBJ;
    o->v.refcount = 0;
    o->name = name;
    o->op = op;
    o->prio = prio;
}

void operobj_init(void) {
    obj_init(&obj, T_OPER, "oper", sizeof(Oper));
    obj.repr = repr;

    oper_init(&o_TUPLE, "')", O_TUPLE, 0);
    oper_init(&o_LIST, "']", O_LIST, 0);
    oper_init(&o_DICT, "'}", O_DICT, 0);
    oper_init(&o_RPARENT, "')", O_RPARENT, 0);
    oper_init(&o_RBRACKET, "']", O_RBRACKET, 0);
    oper_init(&o_RBRACE, "'}", O_RBRACE, 0);
    oper_init(&o_FUNC, "function call '()", O_FUNC, 0);
    oper_init(&o_INDEX, "indexing '[]", O_INDEX, 0);
    oper_init(&o_BRACE, "'{", O_BRACE, 0);
    oper_init(&o_BRACKET, "'[", O_BRACKET, 0);
    oper_init(&o_PARENT, "'(", O_PARENT, 0);
    oper_init(&o_COMMA, "',", O_COMMA, 1);
    oper_init(&o_QUEST, "'?", O_QUEST, 2);
    oper_init(&o_COLON, "':", O_COLON, 2);
    oper_init(&o_COND, "condition '?", O_COND, 3);
    oper_init(&o_COLON2, "':", O_COLON2, 3);
    oper_init(&o_HASH, "immediate '#", O_HASH, 3);
    oper_init(&o_WORD, "word '<>", O_WORD, 4);
    oper_init(&o_HWORD, "high word '>`", O_HWORD, 4);
    oper_init(&o_BSWORD, "swapped word '><", O_BSWORD, 4);
    oper_init(&o_LOWER, "low byte '<", O_LOWER, 4);
    oper_init(&o_HIGHER, "high byte '>", O_HIGHER, 4);
    oper_init(&o_BANK, "bank byte '`", O_BANK, 4);
    oper_init(&o_STRING, "decimal string '^", O_STRING, 4);
    oper_init(&o_LOR, "logical or '||", O_LOR, 5);
    oper_init(&o_LXOR, "logical xor '^^", O_LXOR, 6);
    oper_init(&o_LAND, "logical and '&&", O_LAND, 7);
    oper_init(&o_IN, "contains 'in", O_IN, 8);
    oper_init(&o_CMP, "compare '<=>", O_CMP, 8);
    oper_init(&o_EQ, "equal '==", O_EQ, 8);
    oper_init(&o_NE, "not equal '!=", O_NE, 8);
    oper_init(&o_LT, "less than '<", O_LT, 8);
    oper_init(&o_GT, "greater than '>", O_GT, 8);
    oper_init(&o_GE, "greater than or equal '>=", O_GE, 8);
    oper_init(&o_LE, "less than or equal '<=", O_LE, 8);
    oper_init(&o_OR, "binary or '|", O_OR, 9);
    oper_init(&o_XOR, "binary exclusive or '^", O_XOR, 10);
    oper_init(&o_AND, "binary and '&", O_AND, 11);
    oper_init(&o_LSHIFT, "binary left shift '<<", O_LSHIFT, 12);
    oper_init(&o_RSHIFT, "binary right shift '>>", O_RSHIFT, 12);
    oper_init(&o_ADD, "add '+", O_ADD, 13);
    oper_init(&o_SUB, "subtract '-", O_SUB, 13);
    oper_init(&o_MUL, "multiply '*", O_MUL, 14);
    oper_init(&o_DIV, "division '/", O_DIV, 14);
    oper_init(&o_MOD, "modulo '%", O_MOD, 14);
    oper_init(&o_EXP, "exponent '**", O_EXP, 15);
    oper_init(&o_NEG, "unary negative '-", O_NEG, 16);
    oper_init(&o_POS, "unary positive '+", O_POS, 16);
    oper_init(&o_INV, "binary invert '~", O_INV, 16);
    oper_init(&o_LNOT, "logical not '!", O_LNOT, 16);
    oper_init(&o_COMMAX, "register indexing ',x", O_COMMAX, 16);
    oper_init(&o_COMMAY, "register indexing ',y", O_COMMAY, 16);
    oper_init(&o_COMMAZ, "register indexing ',z", O_COMMAZ, 16);
    oper_init(&o_COMMAS, "register indexing ',s", O_COMMAS, 16);
    oper_init(&o_COMMAR, "register indexing ',r", O_COMMAR, 16);
    oper_init(&o_COMMAD, "register indexing ',d", O_COMMAD, 16);
    oper_init(&o_COMMAB, "register indexing ',b", O_COMMAB, 16);
    oper_init(&o_COMMAK, "register indexing ',k", O_COMMAK, 16);
    oper_init(&o_SPLAT, "unary splat '*", O_SPLAT, 17);
    oper_init(&o_CONCAT, "concatenate '..", O_CONCAT, 18);
    oper_init(&o_X, "repeat 'x", O_X, 19);
    oper_init(&o_MEMBER, "member '.", O_MEMBER, 20);
}

