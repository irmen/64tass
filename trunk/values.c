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
#include "values.h"

#include "unicode.h"
#include "error.h"
#include "strobj.h"
#include "variables.h"

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

#define SLOTS 128
#define ALIGN sizeof(int *)

typedef struct Slot {
    Obj v;
    struct Slot *next;
} Slot;

static Slot *values_free[32];

typedef struct Slotcoll {
    struct Slotcoll *next;
} Slotcoll;

static Slotcoll *slotcoll[32];

static inline void value_free(Obj *val) {
    size_t p = (val->obj->length + (ALIGN - 1)) / ALIGN;
    Slot *slot = (Slot *)val, **c = &values_free[p];
    slot->next = *c;
    val->obj = NONE_OBJ;
    *c = slot;
}

Obj *val_alloc(obj_t obj) {
    size_t p = (obj->length + (ALIGN - 1)) / ALIGN;
    Slot **c = &values_free[p];
    Obj *val = (Obj *)*c;
    if (!val) {
        size_t i, size = p * ALIGN;
        Slot *slot;
        Slotcoll **s = &slotcoll[p];
        Slotcoll *n = (Slotcoll *)malloc(sizeof(Slotcoll) + size * SLOTS);
        if (!n) err_msg_out_of_memory();
        n->next = *s; *s = n;
        slot = ((void *)n) + sizeof(Slotcoll);
        val = (Obj *)slot;
        for (i = 0; i < (SLOTS - 1); i++, slot = ((void *)slot) + size) {
            slot->v.obj = NONE_OBJ;
            slot->v.refcount = 1;
            slot->next = (Slot *)(((void *)slot) + size);
        }
        slot->v.obj = NONE_OBJ;
        slot->v.refcount = 1;
        slot->next = NULL;
    }
    *c = ((Slot *)val)->next;
    val->obj = obj;
    return val;
}

static inline void obj_destroy(Obj *v1) {
    v1->obj->destroy(v1);
}

void garbage_collect(void) {
    Slotcoll *vals;
    size_t i, j;
    destroy_lastlb();

    for (j = 0; j < sizeof(slotcoll) / sizeof(slotcoll[0]); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals; vals = vals->next) {
            Obj *val = ((void *)vals) + sizeof(Slotcoll);
            for (i = 0; i < SLOTS; i++, val = ((void *)val) + size) {
                if (val->obj->garbage) {
                    val->obj->garbage(val, -1);
                    val->refcount |= SIZE_MSB;
                }
            }
        }
    }

    for (j = 0; j < sizeof(slotcoll) / sizeof(slotcoll[0]); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals; vals = vals->next) {
            Obj *val = ((void *)vals) + sizeof(Slotcoll);
            for (i = 0; i < SLOTS; i++, val = ((void *)val) + size) {
                if (val->obj->garbage) {
                    if (val->refcount > SIZE_MSB) {
                        val->refcount -= SIZE_MSB;
                        val->obj->garbage(val, 1);
                    }
                }
            }
        }
    }

    for (j = 0; j < sizeof(slotcoll) / sizeof(slotcoll[0]); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals; vals = vals->next) {
            Obj *val = ((void *)vals) + sizeof(Slotcoll);
            for (i = 0; i < SLOTS; i++, val = ((void *)val) + size) {
                if (!(val->refcount & ~SIZE_MSB)) {
                    val->refcount = 1;
                    if (val->obj->garbage) val->obj->garbage(val, 0);
                    else obj_destroy(val);
                    value_free(val);
                }
            }
        }
    }
}

void val_destroy(Obj *val) {
    if (!val->refcount) {
        obj_destroy(val);
        return;
    }
    if (val->refcount == 1) {
        obj_destroy(val);
        value_free(val);
    } else val->refcount--;
}

void val_replace(Obj **val, Obj *val2) {
    if (*val == val2) return;
    val_destroy(*val);
    *val = val_reference(val2);
}

int val_print(Obj *v1, FILE *f) {
    int oldreferenceit = referenceit;
    Obj *err;
    struct linepos_s nopoint = {0, 0};
    int len;
    referenceit = 0;
    err = v1->obj->repr(v1, &nopoint);
    if (err->obj == STR_OBJ) len = printable_print2(((Str *)err)->data, f, ((Str *)err)->len);
    else len = printable_print2((uint8_t *)err->obj->name, f, strlen(err->obj->name));
    val_destroy(err);
    referenceit = oldreferenceit;
    return len;
}

void init_values(void)
{
    o_TUPLE.v.obj = OPER_OBJ;
    o_TUPLE.v.refcount = 0;
    o_TUPLE.name = "')";
    o_TUPLE.op = O_TUPLE;
    o_TUPLE.prio = 0;
    o_LIST.v.obj = OPER_OBJ;
    o_LIST.v.refcount = 0;
    o_LIST.name = "']";
    o_LIST.op = O_LIST;
    o_LIST.prio = 0;
    o_DICT.v.obj = OPER_OBJ;
    o_DICT.v.refcount = 0;
    o_DICT.name = "'}";
    o_DICT.op = O_DICT;
    o_DICT.prio = 0;
    o_RPARENT.v.obj = OPER_OBJ;
    o_RPARENT.v.refcount = 0;
    o_RPARENT.name = "')";
    o_RPARENT.op = O_RPARENT;
    o_RPARENT.prio = 0;
    o_RBRACKET.v.obj = OPER_OBJ;
    o_RBRACKET.v.refcount = 0;
    o_RBRACKET.name = "']";
    o_RBRACKET.op = O_RBRACKET;
    o_RBRACKET.prio = 0;
    o_RBRACE.v.obj = OPER_OBJ;
    o_RBRACE.v.refcount = 0;
    o_RBRACE.name = "'}";
    o_RBRACE.op = O_RBRACE;
    o_RBRACE.prio = 0;
    o_FUNC.v.obj = OPER_OBJ;
    o_FUNC.v.refcount = 0;
    o_FUNC.name = "function call '()";
    o_FUNC.op = O_FUNC;
    o_FUNC.prio = 0;
    o_INDEX.v.obj = OPER_OBJ;
    o_INDEX.v.refcount = 0;
    o_INDEX.name = "indexing '[]";
    o_INDEX.op = O_INDEX;
    o_INDEX.prio = 0;
    o_BRACE.v.obj = OPER_OBJ;
    o_BRACE.v.refcount = 0;
    o_BRACE.name = "'{";
    o_BRACE.op = O_BRACE;
    o_BRACE.prio = 0;
    o_BRACKET.v.obj = OPER_OBJ;
    o_BRACKET.v.refcount = 0;
    o_BRACKET.name = "'[";
    o_BRACKET.op = O_BRACKET;
    o_BRACKET.prio = 0;
    o_PARENT.v.obj = OPER_OBJ;
    o_PARENT.v.refcount = 0;
    o_PARENT.name = "'(";
    o_PARENT.op = O_PARENT;
    o_PARENT.prio = 0;
    o_COMMA.v.obj = OPER_OBJ;
    o_COMMA.v.refcount = 0;
    o_COMMA.name = "',";
    o_COMMA.op = O_COMMA;
    o_COMMA.prio = 1;
    o_QUEST.v.obj = OPER_OBJ;
    o_QUEST.v.refcount = 0;
    o_QUEST.name = "'?";
    o_QUEST.op = O_QUEST;
    o_QUEST.prio = 2;
    o_COLON.v.obj = OPER_OBJ;
    o_COLON.v.refcount = 0;
    o_COLON.name = "':";
    o_COLON.op = O_COLON;
    o_COLON.prio = 2;
    o_COND.v.obj = OPER_OBJ;
    o_COND.v.refcount = 0;
    o_COND.name = "condition '?";
    o_COND.op = O_COND;
    o_COND.prio = 3;
    o_COLON2.v.obj = OPER_OBJ;
    o_COLON2.v.refcount = 0;
    o_COLON2.name = "':";
    o_COLON2.op = O_COLON2;
    o_COLON2.prio = 3;
    o_HASH.v.obj = OPER_OBJ;
    o_HASH.v.refcount = 0;
    o_HASH.name = "immediate '#";
    o_HASH.op = O_HASH;
    o_HASH.prio = 3;
    o_WORD.v.obj = OPER_OBJ;
    o_WORD.v.refcount = 0;
    o_WORD.name = "word '<>";
    o_WORD.op = O_WORD;
    o_WORD.prio = 4;
    o_HWORD.v.obj = OPER_OBJ;
    o_HWORD.v.refcount = 0;
    o_HWORD.name = "high word '>`";
    o_HWORD.op = O_HWORD;
    o_HWORD.prio = 4;
    o_BSWORD.v.obj = OPER_OBJ;
    o_BSWORD.v.refcount = 0;
    o_BSWORD.name = "swapped word '><";
    o_BSWORD.op = O_BSWORD;
    o_BSWORD.prio = 4;
    o_LOWER.v.obj = OPER_OBJ;
    o_LOWER.v.refcount = 0;
    o_LOWER.name = "low byte '<";
    o_LOWER.op = O_LOWER;
    o_LOWER.prio = 4;
    o_HIGHER.v.obj = OPER_OBJ;
    o_HIGHER.v.refcount = 0;
    o_HIGHER.name = "high byte '>";
    o_HIGHER.op = O_HIGHER;
    o_HIGHER.prio = 4;
    o_BANK.v.obj = OPER_OBJ;
    o_BANK.v.refcount = 0;
    o_BANK.name = "bank byte '`";
    o_BANK.op = O_BANK;
    o_BANK.prio = 4;
    o_STRING.v.obj = OPER_OBJ;
    o_STRING.v.refcount = 0;
    o_STRING.name = "decimal string '^";
    o_STRING.op = O_STRING;
    o_STRING.prio = 4;
    o_LOR.v.obj = OPER_OBJ;
    o_LOR.v.refcount = 0;
    o_LOR.name = "logical or '||";
    o_LOR.op = O_LOR;
    o_LOR.prio = 5;
    o_LXOR.v.obj = OPER_OBJ;
    o_LXOR.v.refcount = 0;
    o_LXOR.name = "logical xor '^^";
    o_LXOR.op = O_LXOR;
    o_LXOR.prio = 6;
    o_LAND.v.obj = OPER_OBJ;
    o_LAND.v.refcount = 0;
    o_LAND.name = "logical and '&&";
    o_LAND.op = O_LAND;
    o_LAND.prio = 7;
    o_IN.v.obj = OPER_OBJ;
    o_IN.v.refcount = 0;
    o_IN.name = "contains 'in";
    o_IN.op = O_IN;
    o_IN.prio = 8;
    o_CMP.v.obj = OPER_OBJ;
    o_CMP.v.refcount = 0;
    o_CMP.name = "compare '<=>";
    o_CMP.op = O_CMP;
    o_CMP.prio = 8;
    o_EQ.v.obj = OPER_OBJ;
    o_EQ.v.refcount = 0;
    o_EQ.name = "equal '==";
    o_EQ.op = O_EQ;
    o_EQ.prio = 8;
    o_NE.v.obj = OPER_OBJ;
    o_NE.v.refcount = 0;
    o_NE.name = "not equal '!=";
    o_NE.op = O_NE;
    o_NE.prio = 8;
    o_LT.v.obj = OPER_OBJ;
    o_LT.v.refcount = 0;
    o_LT.name = "less than '<";
    o_LT.op = O_LT;
    o_LT.prio = 8;
    o_GT.v.obj = OPER_OBJ;
    o_GT.v.refcount = 0;
    o_GT.name = "greater than '>";
    o_GT.op = O_GT;
    o_GT.prio = 8;
    o_GE.v.obj = OPER_OBJ;
    o_GE.v.refcount = 0;
    o_GE.name = "greater than or equal '>=";
    o_GE.op = O_GE;
    o_GE.prio = 8;
    o_LE.v.obj = OPER_OBJ;
    o_LE.v.refcount = 0;
    o_LE.name = "less than or equal '<=";
    o_LE.op = O_LE;
    o_LE.prio = 8;
    o_OR.v.obj = OPER_OBJ;
    o_OR.v.refcount = 0;
    o_OR.name = "binary or '|";
    o_OR.op = O_OR;
    o_OR.prio = 9;
    o_XOR.v.obj = OPER_OBJ;
    o_XOR.v.refcount = 0;
    o_XOR.name = "binary exclusive or '^";
    o_XOR.op = O_XOR;
    o_XOR.prio = 10;
    o_AND.v.obj = OPER_OBJ;
    o_AND.v.refcount = 0;
    o_AND.name = "binary and '&";
    o_AND.op = O_AND;
    o_AND.prio = 11;
    o_LSHIFT.v.obj = OPER_OBJ;
    o_LSHIFT.v.refcount = 0;
    o_LSHIFT.name = "binary left shift '<<";
    o_LSHIFT.op = O_LSHIFT;
    o_LSHIFT.prio = 12;
    o_RSHIFT.v.obj = OPER_OBJ;
    o_RSHIFT.v.refcount = 0;
    o_RSHIFT.name = "binary right shift '>>";
    o_RSHIFT.op = O_RSHIFT;
    o_RSHIFT.prio = 12;
    o_ADD.v.obj = OPER_OBJ;
    o_ADD.v.refcount = 0;
    o_ADD.name = "add '+";
    o_ADD.op = O_ADD;
    o_ADD.prio = 13;
    o_SUB.v.obj = OPER_OBJ;
    o_SUB.v.refcount = 0;
    o_SUB.name = "subtract '-";
    o_SUB.op = O_SUB;
    o_SUB.prio = 13;
    o_MUL.v.obj = OPER_OBJ;
    o_MUL.v.refcount = 0;
    o_MUL.name = "multiply '*";
    o_MUL.op = O_MUL;
    o_MUL.prio = 14;
    o_DIV.v.obj = OPER_OBJ;
    o_DIV.v.refcount = 0;
    o_DIV.name = "division '/";
    o_DIV.op = O_DIV;
    o_DIV.prio = 14;
    o_MOD.v.obj = OPER_OBJ;
    o_MOD.v.refcount = 0;
    o_MOD.name = "modulo '%";
    o_MOD.op = O_MOD;
    o_MOD.prio = 14;
    o_EXP.v.obj = OPER_OBJ;
    o_EXP.v.refcount = 0;
    o_EXP.name = "exponent '**";
    o_EXP.op = O_EXP;
    o_EXP.prio = 15;
    o_NEG.v.obj = OPER_OBJ;
    o_NEG.v.refcount = 0;
    o_NEG.name = "unary negative '-";
    o_NEG.op = O_NEG;
    o_NEG.prio = 16;
    o_POS.v.obj = OPER_OBJ;
    o_POS.v.refcount = 0;
    o_POS.name = "unary positive '+";
    o_POS.op = O_POS;
    o_POS.prio = 16;
    o_INV.v.obj = OPER_OBJ;
    o_INV.v.refcount = 0;
    o_INV.name = "binary invert '~";
    o_INV.op = O_INV;
    o_INV.prio = 16;
    o_LNOT.v.obj = OPER_OBJ;
    o_LNOT.v.refcount = 0;
    o_LNOT.name = "logical not '!";
    o_LNOT.op = O_LNOT;
    o_LNOT.prio = 16;
    o_COMMAX.v.obj = OPER_OBJ;
    o_COMMAX.v.refcount = 0;
    o_COMMAX.name = "register indexing ',x";
    o_COMMAX.op = O_COMMAX;
    o_COMMAX.prio = 16;
    o_COMMAY.v.obj = OPER_OBJ;
    o_COMMAY.v.refcount = 0;
    o_COMMAY.name = "register indexing ',y";
    o_COMMAY.op = O_COMMAY;
    o_COMMAY.prio = 16;
    o_COMMAZ.v.obj = OPER_OBJ;
    o_COMMAZ.v.refcount = 0;
    o_COMMAZ.name = "register indexing ',z";
    o_COMMAZ.op = O_COMMAZ;
    o_COMMAZ.prio = 16;
    o_COMMAS.v.obj = OPER_OBJ;
    o_COMMAS.v.refcount = 0;
    o_COMMAS.name = "register indexing ',s";
    o_COMMAS.op = O_COMMAS;
    o_COMMAS.prio = 16;
    o_COMMAR.v.obj = OPER_OBJ;
    o_COMMAR.v.refcount = 0;
    o_COMMAR.name = "register indexing ',r";
    o_COMMAR.op = O_COMMAR;
    o_COMMAR.prio = 16;
    o_COMMAD.v.obj = OPER_OBJ;
    o_COMMAD.v.refcount = 0;
    o_COMMAD.name = "register indexing ',d";
    o_COMMAD.op = O_COMMAD;
    o_COMMAD.prio = 16;
    o_COMMAB.v.obj = OPER_OBJ;
    o_COMMAB.v.refcount = 0;
    o_COMMAB.name = "register indexing ',b";
    o_COMMAB.op = O_COMMAB;
    o_COMMAB.prio = 16;
    o_COMMAK.v.obj = OPER_OBJ;
    o_COMMAK.v.refcount = 0;
    o_COMMAK.name = "register indexing ',k";
    o_COMMAK.op = O_COMMAK;
    o_COMMAK.prio = 16;
    o_SPLAT.v.obj = OPER_OBJ;
    o_SPLAT.v.refcount = 0;
    o_SPLAT.name = "unary splat '*";
    o_SPLAT.op = O_SPLAT;
    o_SPLAT.prio = 17;
    o_CONCAT.v.obj = OPER_OBJ;
    o_CONCAT.v.refcount = 0;
    o_CONCAT.name = "concatenate '..";
    o_CONCAT.op = O_CONCAT;
    o_CONCAT.prio = 18;
    o_X.v.obj = OPER_OBJ;
    o_X.v.refcount = 0;
    o_X.name = "repeat 'x";
    o_X.op = O_X;
    o_X.prio = 19;
    o_MEMBER.v.obj = OPER_OBJ;
    o_MEMBER.v.refcount = 0;
    o_MEMBER.name = "member '.";
    o_MEMBER.op = O_MEMBER;
    o_MEMBER.prio = 20;
}

void destroy_values(void)
{
    size_t j;
    garbage_collect();

#ifdef DEBUG
    {
    Slotcoll *vals;
    size_t i;
    for (j = 0; j < sizeof(slotcoll) / sizeof(slotcoll[0]); j++) {
        size_t size = j * ALIGN;
        for (vals = slotcoll[j]; vals; vals = vals->next) {
            Obj *val = ((void *)vals) + sizeof(Slotcoll);
            for (i = 0; i < SLOTS; i++, val = ((void *)val) + size) {
                if (val->obj != NONE_OBJ) {
                    val_print(val, stderr);
                    fprintf(stderr, " %s %d %x\n", val->obj->name, val->refcount, (int)val); 
                }
            }
        }
    }
    }
#endif

    for (j = 0; j < sizeof(slotcoll) / sizeof(slotcoll[0]); j++) {
        Slotcoll *s = slotcoll[j];
        while (s) {
            Slotcoll *old = s;
            s = s->next;
            free(old);
        }
    }
}
