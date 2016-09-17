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
#include "operobj.h"
#include <string.h>

#include "strobj.h"
#include "typeobj.h"

static Type obj;

Type *OPER_OBJ = &obj;

Oper o_TUPLE       = { {&obj, 0}, "')", O_TUPLE, 0};
Oper o_LIST        = { {&obj, 0}, "']", O_LIST, 0};
Oper o_DICT        = { {&obj, 0}, "'}", O_DICT, 0};
Oper o_RPARENT     = { {&obj, 0}, "')", O_RPARENT, 0};
Oper o_RBRACKET    = { {&obj, 0}, "']", O_RBRACKET, 0};
Oper o_RBRACE      = { {&obj, 0}, "'}", O_RBRACE, 0};
Oper o_FUNC        = { {&obj, 0}, "function call '()", O_FUNC, 0};
Oper o_INDEX       = { {&obj, 0}, "indexing '[]", O_INDEX, 0};
Oper o_BRACE       = { {&obj, 0}, "'{", O_BRACE, 0};
Oper o_BRACKET     = { {&obj, 0}, "'[", O_BRACKET, 0};
Oper o_PARENT      = { {&obj, 0}, "'(", O_PARENT, 0};
Oper o_COMMA       = { {&obj, 0}, "',", O_COMMA, 1};
Oper o_QUEST       = { {&obj, 0}, "'?", O_QUEST, 2};
Oper o_COLON       = { {&obj, 0}, "':", O_COLON, 2};
Oper o_COND        = { {&obj, 0}, "condition '?", O_COND, 3};
Oper o_COLON2      = { {&obj, 0}, "':", O_COLON2, 3};
Oper o_HASH        = { {&obj, 0}, "immediate '#", O_HASH, 3};
Oper o_HASH_SIGNED = { {&obj, 0}, "signed immediate '#+", O_HASH_SIGNED, 3};
Oper o_WORD        = { {&obj, 0}, "word '<>", O_WORD, 4};
Oper o_HWORD       = { {&obj, 0}, "high word '>`", O_HWORD, 4};
Oper o_BSWORD      = { {&obj, 0}, "swapped word '><", O_BSWORD, 4};
Oper o_LOWER       = { {&obj, 0}, "low byte '<", O_LOWER, 4};
Oper o_HIGHER      = { {&obj, 0}, "high byte '>", O_HIGHER, 4};
Oper o_BANK        = { {&obj, 0}, "bank byte '`", O_BANK, 4};
Oper o_STRING      = { {&obj, 0}, "decimal string '^", O_STRING, 4};
Oper o_LOR         = { {&obj, 0}, "logical or '||", O_LOR, 5};
Oper o_LXOR        = { {&obj, 0}, "logical xor '^^", O_LXOR, 6};
Oper o_LAND        = { {&obj, 0}, "logical and '&&", O_LAND, 7};
Oper o_IN          = { {&obj, 0}, "contains 'in", O_IN, 8};
Oper o_CMP         = { {&obj, 0}, "compare '<=>", O_CMP, 8};
Oper o_EQ          = { {&obj, 0}, "equal '==", O_EQ, 8};
Oper o_NE          = { {&obj, 0}, "not equal '!=", O_NE, 8};
Oper o_LT          = { {&obj, 0}, "less than '<", O_LT, 8};
Oper o_GT          = { {&obj, 0}, "greater than '>", O_GT, 8};
Oper o_GE          = { {&obj, 0}, "greater than or equal '>=", O_GE, 8};
Oper o_LE          = { {&obj, 0}, "less than or equal '<=", O_LE, 8};
Oper o_OR          = { {&obj, 0}, "binary or '|", O_OR, 9};
Oper o_XOR         = { {&obj, 0}, "binary exclusive or '^", O_XOR, 10};
Oper o_AND         = { {&obj, 0}, "binary and '&", O_AND, 11};
Oper o_LSHIFT      = { {&obj, 0}, "binary left shift '<<", O_LSHIFT, 12};
Oper o_RSHIFT      = { {&obj, 0}, "binary right shift '>>", O_RSHIFT, 12};
Oper o_ADD         = { {&obj, 0}, "add '+", O_ADD, 13};
Oper o_SUB         = { {&obj, 0}, "subtract '-", O_SUB, 13};
Oper o_MUL         = { {&obj, 0}, "multiply '*", O_MUL, 14};
Oper o_DIV         = { {&obj, 0}, "division '/", O_DIV, 14};
Oper o_MOD         = { {&obj, 0}, "modulo '%", O_MOD, 14};
Oper o_EXP         = { {&obj, 0}, "exponent '**", O_EXP, 15};
Oper o_NEG         = { {&obj, 0}, "unary negative '-", O_NEG, 16};
Oper o_POS         = { {&obj, 0}, "unary positive '+", O_POS, 16};
Oper o_INV         = { {&obj, 0}, "binary invert '~", O_INV, 16};
Oper o_LNOT        = { {&obj, 0}, "logical not '!", O_LNOT, 16};
Oper o_COMMAX      = { {&obj, 0}, "register indexing ',x", O_COMMAX, 16};
Oper o_COMMAY      = { {&obj, 0}, "register indexing ',y", O_COMMAY, 16};
Oper o_COMMAZ      = { {&obj, 0}, "register indexing ',z", O_COMMAZ, 16};
Oper o_COMMAS      = { {&obj, 0}, "register indexing ',s", O_COMMAS, 16};
Oper o_COMMAR      = { {&obj, 0}, "register indexing ',r", O_COMMAR, 16};
Oper o_COMMAD      = { {&obj, 0}, "register indexing ',d", O_COMMAD, 16};
Oper o_COMMAB      = { {&obj, 0}, "register indexing ',b", O_COMMAB, 16};
Oper o_COMMAK      = { {&obj, 0}, "register indexing ',k", O_COMMAK, 16};
Oper o_SPLAT       = { {&obj, 0}, "unary splat '*", O_SPLAT, 17};
Oper o_CONCAT      = { {&obj, 0}, "concatenate '..", O_CONCAT, 18};
Oper o_X           = { {&obj, 0}, "repeat 'x", O_X, 19};
Oper o_MEMBER      = { {&obj, 0}, "member '.", O_MEMBER, 20};

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Oper *v1 = (Oper *)o1;
    const char *txt;
    size_t len, len2;
    uint8_t *s;
    Str *v;
    if (epoint == NULL) return NULL;
    txt = v1->name;
    len = strlen(txt);
    len2 = len + 8;
    if (len2 > maxsize) return NULL;
    v = new_str(len2);
    v->chars = len2;
    s = v->data;
    memcpy(s, "<oper ", 6);
    s += 6;
    memcpy(s, txt, len);
    s[len] = '\'';
    s[len + 1] = '>';
    return &v->v;
}

void operobj_init(void) {
    new_type(&obj, T_OPER, "oper", sizeof(Oper));
    obj_init(&obj);
    obj.repr = repr;
}

