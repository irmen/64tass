/*
    $Id: operobj.c 2799 2022-08-12 22:57:23Z soci $

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
#include "operobj.h"
#include <string.h>

#include "strobj.h"
#include "typeobj.h"

static Type obj;

Type *const OPER_OBJ = &obj;

Oper operators[] = {
    { {NULL, 0}, "')", O_TUPLE, 0, 1},
    { {NULL, 0}, "']", O_LIST, 0, 1},
    { {NULL, 0}, "'}", O_DICT, 0, 1},
    { {NULL, 0}, "')", O_RPARENT, 0, 1},
    { {NULL, 0}, "']", O_RBRACKET, 0, 1},
    { {NULL, 0}, "'}", O_RBRACE, 0, 1},
    { {NULL, 0}, "function call '()", O_FUNC, 0, 1},
    { {NULL, 0}, "indexing '[]", O_INDEX, 0, 1},
    { {NULL, 0}, "'{", O_BRACE, 0, 1},
    { {NULL, 0}, "'[", O_BRACKET, 0, 1},
    { {NULL, 0}, "'(", O_PARENT, 0, 1},
    { {NULL, 0}, "',", O_COMMA, 1, 1},
    { {NULL, 0}, "assign '=", O_ASSIGN, 2, 1},
    { {NULL, 0}, "variable assign ':=", O_COLON_ASSIGN, 2, 2},
    { {NULL, 0}, "variable reassign '::=", O_REASSIGN, 2, 3},
    { {NULL, 0}, "smaller of assign '<?=", O_MIN_ASSIGN, 2, 3},
    { {NULL, 0}, "greater of assign '>?=", O_MAX_ASSIGN, 2, 3},
    { {NULL, 0}, "binary or assign '|=", O_OR_ASSIGN, 2, 2},
    { {NULL, 0}, "binary exclusive or assign '^=", O_XOR_ASSIGN, 2, 2},
    { {NULL, 0}, "binary and assign '&=", O_AND_ASSIGN, 2, 2},
    { {NULL, 0}, "binary left shift assign '<<=", O_BLS_ASSIGN, 2, 3},
    { {NULL, 0}, "binary right shift assign '>>=", O_BRS_ASSIGN, 2, 3},
    { {NULL, 0}, "add assign '+=", O_ADD_ASSIGN, 2, 2},
    { {NULL, 0}, "subtract assign '-=", O_SUB_ASSIGN, 2, 2},
    { {NULL, 0}, "multiply assign '*=", O_MUL_ASSIGN, 2, 2},
    { {NULL, 0}, "division assign '/=", O_DIV_ASSIGN, 2, 2},
    { {NULL, 0}, "modulo assign '%=", O_MOD_ASSIGN, 2, 2},
    { {NULL, 0}, "exponent assign '**=", O_EXP_ASSIGN, 2, 3},
    { {NULL, 0}, "concatenate assign '..=", O_CONCAT_ASSIGN, 2, 3},
    { {NULL, 0}, "repeat assign 'x=", O_X_ASSIGN, 2, 2},
    { {NULL, 0}, "member assign '.=", O_MEMBER_ASSIGN, 2, 2},
    { {NULL, 0}, "logical or assign '||=", O_LOR_ASSIGN, 2, 3},
    { {NULL, 0}, "logical and assign '&&=", O_LAND_ASSIGN, 2, 3},
    { {NULL, 0}, "conditional assign ':?=", O_COND_ASSIGN, 2, 3},
    { {NULL, 0}, "'?", O_QUEST, 2, 1},
    { {NULL, 0}, "'??", O_DQUEST, 2, 2},
    { {NULL, 0}, "':", O_COLON, 2, 1},
    { {NULL, 0}, "condition '?", O_COND, 3, 1},
    { {NULL, 0}, "condition '??", O_DCOND, 3, 2},
    { {NULL, 0}, "':", O_COLON2, 3, 1},
    { {NULL, 0}, "immediate '#", O_HASH, 3, 1},
    { {NULL, 0}, "signed immediate '#+", O_HASH_SIGNED, 3, 2},
    { {NULL, 0}, "word '<>", O_WORD, 4, 2},
    { {NULL, 0}, "high word '>`", O_HWORD, 4, 2},
    { {NULL, 0}, "swapped word '><", O_BSWORD, 4, 2},
    { {NULL, 0}, "low byte '<", O_LOWER, 4, 1},
    { {NULL, 0}, "high byte '>", O_HIGHER, 4, 1},
    { {NULL, 0}, "bank byte '`", O_BANK, 4, 1},
    { {NULL, 0}, "decimal string '^", O_STRING, 4, 1},
    { {NULL, 0}, "logical or '||", O_LOR, 5, 2},
    { {NULL, 0}, "logical xor '^^", O_LXOR, 6, 2},
    { {NULL, 0}, "logical and '&&", O_LAND, 7, 2},
    { {NULL, 0}, "identical '===", O_IDENTITY, 8, 3},
    { {NULL, 0}, "not identical '!==", O_NIDENTITY, 8, 3},
    { {NULL, 0}, "contains 'in", O_IN, 8, 2},
    { {NULL, 0}, "excludes '!in", O_NOTIN, 8, 3},
    { {NULL, 0}, "compare '<=>", O_CMP, 8, 3},
    { {NULL, 0}, "equal '==", O_EQ, 8, 2},
    { {NULL, 0}, "not equal '!=", O_NE, 8, 2},
    { {NULL, 0}, "less than '<", O_LT, 8, 1},
    { {NULL, 0}, "greater than '>", O_GT, 8, 1},
    { {NULL, 0}, "greater than or equal '>=", O_GE, 8, 2},
    { {NULL, 0}, "less than or equal '<=", O_LE, 8, 2},
    { {NULL, 0}, "smaller of '<?", O_MIN, 9, 2},
    { {NULL, 0}, "greater of '>?", O_MAX, 9, 2},
    { {NULL, 0}, "binary or '|", O_OR, 10, 1},
    { {NULL, 0}, "binary exclusive or '^", O_XOR, 11, 1},
    { {NULL, 0}, "binary and '&", O_AND, 12, 1},
    { {NULL, 0}, "binary left shift '<<", O_LSHIFT, 13, 2},
    { {NULL, 0}, "binary right shift '>>", O_RSHIFT, 13, 2},
    { {NULL, 0}, "add '+", O_ADD, 14, 1},
    { {NULL, 0}, "subtract '-", O_SUB, 14, 1},
    { {NULL, 0}, "multiply '*", O_MUL, 15, 1},
    { {NULL, 0}, "division '/", O_DIV, 15, 1},
    { {NULL, 0}, "modulo '%", O_MOD, 15, 1},
    { {NULL, 0}, "exponent '**", O_EXP, 16, 2},
    { {NULL, 0}, "unary negative '-", O_NEG, 17, 1},
    { {NULL, 0}, "unary positive '+", O_POS, 17, 1},
    { {NULL, 0}, "binary invert '~", O_INV, 17, 1},
    { {NULL, 0}, "logical not '!", O_LNOT, 17, 1},
    { {NULL, 0}, "unary splat '*", O_SPLAT, 18, 1},
    { {NULL, 0}, "concatenate '..", O_CONCAT, 19, 2},
    { {NULL, 0}, "repeat 'x", O_X, 20, 1},
    { {NULL, 0}, "register indexing ',x", O_COMMAX, 21, 2},
    { {NULL, 0}, "register indexing ',y", O_COMMAY, 21, 2},
    { {NULL, 0}, "register indexing ',z", O_COMMAZ, 21, 2},
    { {NULL, 0}, "register indexing ',s", O_COMMAS, 21, 2},
    { {NULL, 0}, "register indexing ',r", O_COMMAR, 21, 2},
    { {NULL, 0}, "register indexing ',d", O_COMMAD, 21, 2},
    { {NULL, 0}, "register indexing ',b", O_COMMAB, 21, 2},
    { {NULL, 0}, "register indexing ',k", O_COMMAK, 21, 2},
    { {NULL, 0}, "member '.", O_MEMBER, 22, 1},
    { {NULL, 0}, "", O_NONE, 0, 0}
};

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Oper *v1 = Oper(o1);
    const char *txt;
    size_t len, len2;
    uint8_t *s;
    Str *v;
    if (epoint == NULL) return NULL;
    txt = v1->name;
    len = strlen(txt);
    len2 = len + 8;
    if (len2 > maxsize) return NULL;
    v = new_str2(len2);
    if (v == NULL) return NULL;
    v->chars = len2;
    s = v->data;
    memcpy(s, "<oper ", 6);
    s += 6;
    memcpy(s, txt, len);
    s[len] = '\'';
    s[len + 1] = '>';
    return Obj(v);
}

void operobj_init(void) {
    unsigned int i;
    Type *type = new_type(&obj, T_OPER, "oper", sizeof(Oper));
    type->repr = repr;
    for (i = 0; i < lenof(operators); i++) {
        operators[i].v.obj = &obj;
    }
}

