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

#include "boolobj.h"
#include "listobj.h"
#include "strobj.h"
#include "intobj.h"
#include "bytesobj.h"
#include "bitsobj.h"

struct value_s none_value;
struct value_s *true_value;
struct value_s *false_value;
struct value_s *gap_value;
struct value_s null_str;
struct value_s null_bytes;
struct value_s null_bits;
struct value_s null_tuple;
struct value_s null_list;
struct value_s null_addrlist;

struct value_s o_TUPLE;
struct value_s o_LIST;
struct value_s o_DICT;
struct value_s o_RPARENT;
struct value_s o_RBRACKET;
struct value_s o_RBRACE;
struct value_s o_FUNC;
struct value_s o_INDEX;
struct value_s o_BRACE;
struct value_s o_BRACKET;
struct value_s o_PARENT;
struct value_s o_COMMA;
struct value_s o_QUEST;
struct value_s o_COLON;
struct value_s o_COND;
struct value_s o_COLON2;
struct value_s o_HASH;
struct value_s o_COMMAX;
struct value_s o_COMMAY;
struct value_s o_COMMAZ;
struct value_s o_COMMAS;
struct value_s o_COMMAR;
struct value_s o_COMMAD;
struct value_s o_COMMAB;
struct value_s o_COMMAK;
struct value_s o_WORD;
struct value_s o_HWORD;
struct value_s o_BSWORD;
struct value_s o_LOWER;
struct value_s o_HIGHER;
struct value_s o_BANK;
struct value_s o_STRING;
struct value_s o_LOR;
struct value_s o_LXOR;
struct value_s o_LAND;
struct value_s o_IN;
struct value_s o_CMP;
struct value_s o_EQ;
struct value_s o_NE;
struct value_s o_LT;
struct value_s o_GT;
struct value_s o_GE;
struct value_s o_LE;
struct value_s o_OR;
struct value_s o_XOR;
struct value_s o_AND;
struct value_s o_LSHIFT;
struct value_s o_RSHIFT;
struct value_s o_ADD;
struct value_s o_SUB;
struct value_s o_MUL;
struct value_s o_DIV;
struct value_s o_MOD;
struct value_s o_EXP;
struct value_s o_SPLAT;
struct value_s o_NEG;
struct value_s o_POS;
struct value_s o_INV;
struct value_s o_LNOT;
struct value_s o_CONCAT;
struct value_s o_X;
struct value_s o_MEMBER;

static union values_u {
    struct value_s val;
    union values_u *next;
} *values_free = NULL;

static struct values_s {
    union values_u vals[255];
    struct values_s *next;
} *values = NULL;

static inline void value_free(union values_u *val) {
//    return free(val);
    val->next = values_free;
    values_free = val;
}

struct value_s *val_alloc(void) {
    struct value_s *val;
    size_t i;
//    val = (struct value_s *)malloc(sizeof(struct value_s)); val->refcount = 1; return val;
    val = (struct value_s *)values_free;
    values_free = values_free->next;
    if (!values_free) {
        struct values_s *old = values;
        values = (struct values_s *)malloc(sizeof(struct values_s));
        if (!values) err_msg_out_of_memory();
        for (i = 0; i < 254; i++) {
            values->vals[i].next = &values->vals[i+1];
        }
        values->vals[i].next = NULL;
        values->next = old;
        values_free = &values->vals[0];
    }
    val->refcount = 1;
    return val;
}

void val_destroy(struct value_s *val) {
    if (!val->refcount) {
        obj_destroy(val);
        return;
    }
    if (val->refcount == 1) {
        obj_destroy(val);
        value_free((union values_u *)val);
    } else val->refcount--;
}

struct value_s *val_reference(struct value_s *val2) {
    struct value_s *val;
    if (val2->refcount) {val2->refcount++;return val2;}
    val = val_alloc();
    val2->obj->copy(val2, val);
    return val;
}

struct value_s *val_realloc(struct value_s **val) {
    if ((*val)->refcount == 1) {
        obj_destroy(*val);
        return *val;
    }
    if ((*val)->refcount) (*val)->refcount--;
    *val = val_alloc();
    return *val;
}

void val_replace(struct value_s **val, struct value_s *val2) {
    if (*val == val2) return;
    if ((*val)->refcount == 1 && val2->refcount == 0) {
        obj_destroy(*val);
        val2->obj->copy(val2, *val);
        return;
    }
    val_destroy(*val);
    *val = val_reference(val2);
}

void val_replace_template(struct value_s **val, const struct value_s *val2) {
    if ((*val)->refcount == 1) {
        obj_destroy(*val);
    } else { 
        val_destroy(*val);
        *val = val_alloc();
    }
    val2->obj->copy_temp(val2, *val);
}

void val_set_template(struct value_s **val, const struct value_s *val2) {
    *val = val_alloc();
    val2->obj->copy_temp(val2, *val);
}

int pair_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct pair_s *a = cavltree_container_of(aa, struct pair_s, node);
    const struct pair_s *b = cavltree_container_of(bb, struct pair_s, node);
    struct value_s tmp, *result;
    struct oper_s oper;
    int h = a->hash - b->hash;

    if (h) return h;
    tmp.refcount = 0;
    oper.op = &o_CMP;
    oper.v1 = a->key;
    oper.v2 = b->key;
    oper.v = &tmp;
    result = oper.v1->obj->calc2(&oper);
    if (result) {
        if (result->obj == INT_OBJ) h = result->u.integer.len;
        else h = oper.v1->obj->type - oper.v2->obj->type;
        val_destroy(result);
        return h;
    }
    if (tmp.obj == INT_OBJ) return tmp.u.integer.len;
    return oper.v1->obj->type - oper.v2->obj->type;
}

int val_print(const struct value_s *v1, FILE *f) {
    struct value_s tmp;
    struct linepos_s nopoint = {0, 0, 0};
    int len;
    v1->obj->repr(v1, &tmp, &nopoint);
    if (tmp.obj == STR_OBJ) {
        len = fwrite(tmp.u.str.data, 1, tmp.u.str.len, f);
    } else len = fwrite(tmp.obj->name, 1, strlen(tmp.obj->name), f);
    tmp.obj->destroy(&tmp);
    return len;
}

void init_values(void)
{
    size_t i;
    values = (struct values_s *)malloc(sizeof(struct values_s));
    if (!values) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        values->vals[i].next = &values->vals[i+1];
    }
    values->vals[i].next = NULL;
    values->next = NULL;

    values_free = &values->vals[0];

    none_value.obj = NONE_OBJ;
    none_value.refcount = 0;
    true_value = val_alloc();
    true_value->obj = BOOL_OBJ;
    true_value->u.boolean = 1;
    false_value = val_alloc();
    false_value->obj = BOOL_OBJ;
    false_value->u.boolean = 0;
    gap_value = val_alloc();
    gap_value->obj = GAP_OBJ;
    null_str.obj = STR_OBJ;
    null_str.refcount = 0;
    null_str.u.str.len = 0;
    null_str.u.str.chars = 0;
    null_str.u.str.data = NULL;
    null_bytes.obj = BYTES_OBJ;
    null_bytes.refcount = 0;
    null_bytes.u.bytes.len = 0;
    null_bytes.u.bytes.data = null_bytes.u.bytes.val;
    null_bits.obj = BITS_OBJ;
    null_bits.refcount = 0;
    null_bits.u.bits.len = 0;
    null_bits.u.bits.bits = 0;
    null_bits.u.bits.inv = 0;
    null_bits.u.bits.val[0] = 0;
    null_bits.u.bits.data = null_bits.u.bits.val;
    null_tuple.obj = TUPLE_OBJ;
    null_tuple.refcount = 0;
    null_tuple.u.list.len = 0;
    null_tuple.u.list.data = NULL;
    null_list.obj = LIST_OBJ;
    null_list.refcount = 0;
    null_list.u.list.len = 0;
    null_list.u.list.data = NULL;
    null_addrlist.obj = ADDRLIST_OBJ;
    null_addrlist.refcount = 0;
    null_addrlist.u.list.len = 0;
    null_addrlist.u.list.data = NULL;

    o_TUPLE.obj = OPER_OBJ;
    o_TUPLE.refcount = 0;
    o_TUPLE.u.oper.name = "')";
    o_TUPLE.u.oper.op = O_TUPLE;
    o_TUPLE.u.oper.prio = 0;
    o_LIST.obj = OPER_OBJ;
    o_LIST.refcount = 0;
    o_LIST.u.oper.name = "']";
    o_LIST.u.oper.op = O_LIST;
    o_LIST.u.oper.prio = 0;
    o_DICT.obj = OPER_OBJ;
    o_DICT.refcount = 0;
    o_DICT.u.oper.name = "'}";
    o_DICT.u.oper.op = O_DICT;
    o_DICT.u.oper.prio = 0;
    o_RPARENT.obj = OPER_OBJ;
    o_RPARENT.refcount = 0;
    o_RPARENT.u.oper.name = "')";
    o_RPARENT.u.oper.op = O_RPARENT;
    o_RPARENT.u.oper.prio = 0;
    o_RBRACKET.obj = OPER_OBJ;
    o_RBRACKET.refcount = 0;
    o_RBRACKET.u.oper.name = "']";
    o_RBRACKET.u.oper.op = O_RBRACKET;
    o_RBRACKET.u.oper.prio = 0;
    o_RBRACE.obj = OPER_OBJ;
    o_RBRACE.refcount = 0;
    o_RBRACE.u.oper.name = "'}";
    o_RBRACE.u.oper.op = O_RBRACE;
    o_RBRACE.u.oper.prio = 0;
    o_FUNC.obj = OPER_OBJ;
    o_FUNC.refcount = 0;
    o_FUNC.u.oper.name = "function call '()";
    o_FUNC.u.oper.op = O_FUNC;
    o_FUNC.u.oper.prio = 0;
    o_INDEX.obj = OPER_OBJ;
    o_INDEX.refcount = 0;
    o_INDEX.u.oper.name = "indexing '[]";
    o_INDEX.u.oper.op = O_INDEX;
    o_INDEX.u.oper.prio = 0;
    o_BRACE.obj = OPER_OBJ;
    o_BRACE.refcount = 0;
    o_BRACE.u.oper.name = "'{";
    o_BRACE.u.oper.op = O_BRACE;
    o_BRACE.u.oper.prio = 0;
    o_BRACKET.obj = OPER_OBJ;
    o_BRACKET.refcount = 0;
    o_BRACKET.u.oper.name = "'[";
    o_BRACKET.u.oper.op = O_BRACKET;
    o_BRACKET.u.oper.prio = 0;
    o_PARENT.obj = OPER_OBJ;
    o_PARENT.refcount = 0;
    o_PARENT.u.oper.name = "'(";
    o_PARENT.u.oper.op = O_PARENT;
    o_PARENT.u.oper.prio = 0;
    o_COMMA.obj = OPER_OBJ;
    o_COMMA.refcount = 0;
    o_COMMA.u.oper.name = "',";
    o_COMMA.u.oper.op = O_COMMA;
    o_COMMA.u.oper.prio = 1;
    o_QUEST.obj = OPER_OBJ;
    o_QUEST.refcount = 0;
    o_QUEST.u.oper.name = "'?";
    o_QUEST.u.oper.op = O_QUEST;
    o_QUEST.u.oper.prio = 2;
    o_COLON.obj = OPER_OBJ;
    o_COLON.refcount = 0;
    o_COLON.u.oper.name = "':";
    o_COLON.u.oper.op = O_COLON;
    o_COLON.u.oper.prio = 2;
    o_COND.obj = OPER_OBJ;
    o_COND.refcount = 0;
    o_COND.u.oper.name = "condition '?";
    o_COND.u.oper.op = O_COND;
    o_COND.u.oper.prio = 3;
    o_COLON2.obj = OPER_OBJ;
    o_COLON2.refcount = 0;
    o_COLON2.u.oper.name = "':";
    o_COLON2.u.oper.op = O_COLON2;
    o_COLON2.u.oper.prio = 3;
    o_HASH.obj = OPER_OBJ;
    o_HASH.refcount = 0;
    o_HASH.u.oper.name = "immediate '#";
    o_HASH.u.oper.op = O_HASH;
    o_HASH.u.oper.prio = 3;
    o_COMMAX.obj = OPER_OBJ;
    o_COMMAX.refcount = 0;
    o_COMMAX.u.oper.name = "register indexing ',x";
    o_COMMAX.u.oper.op = O_COMMAX;
    o_COMMAX.u.oper.prio = 4;
    o_COMMAY.obj = OPER_OBJ;
    o_COMMAY.refcount = 0;
    o_COMMAY.u.oper.name = "register indexing ',y";
    o_COMMAY.u.oper.op = O_COMMAY;
    o_COMMAY.u.oper.prio = 4;
    o_COMMAZ.obj = OPER_OBJ;
    o_COMMAZ.refcount = 0;
    o_COMMAZ.u.oper.name = "register indexing ',z";
    o_COMMAZ.u.oper.op = O_COMMAZ;
    o_COMMAZ.u.oper.prio = 4;
    o_COMMAS.obj = OPER_OBJ;
    o_COMMAS.refcount = 0;
    o_COMMAS.u.oper.name = "register indexing ',s";
    o_COMMAS.u.oper.op = O_COMMAS;
    o_COMMAS.u.oper.prio = 4;
    o_COMMAR.obj = OPER_OBJ;
    o_COMMAR.refcount = 0;
    o_COMMAR.u.oper.name = "register indexing ',r";
    o_COMMAR.u.oper.op = O_COMMAR;
    o_COMMAR.u.oper.prio = 4;
    o_COMMAD.obj = OPER_OBJ;
    o_COMMAD.refcount = 0;
    o_COMMAD.u.oper.name = "register indexing ',d";
    o_COMMAD.u.oper.op = O_COMMAD;
    o_COMMAD.u.oper.prio = 4;
    o_COMMAB.obj = OPER_OBJ;
    o_COMMAB.refcount = 0;
    o_COMMAB.u.oper.name = "register indexing ',b";
    o_COMMAB.u.oper.op = O_COMMAB;
    o_COMMAB.u.oper.prio = 4;
    o_COMMAK.obj = OPER_OBJ;
    o_COMMAK.refcount = 0;
    o_COMMAK.u.oper.name = "register indexing ',k";
    o_COMMAK.u.oper.op = O_COMMAK;
    o_COMMAK.u.oper.prio = 4;
    o_WORD.obj = OPER_OBJ;
    o_WORD.refcount = 0;
    o_WORD.u.oper.name = "word '<>";
    o_WORD.u.oper.op = O_WORD;
    o_WORD.u.oper.prio = 4;
    o_HWORD.obj = OPER_OBJ;
    o_HWORD.refcount = 0;
    o_HWORD.u.oper.name = "high word '>`";
    o_HWORD.u.oper.op = O_HWORD;
    o_HWORD.u.oper.prio = 4;
    o_BSWORD.obj = OPER_OBJ;
    o_BSWORD.refcount = 0;
    o_BSWORD.u.oper.name = "swapped word '><";
    o_BSWORD.u.oper.op = O_BSWORD;
    o_BSWORD.u.oper.prio = 4;
    o_LOWER.obj = OPER_OBJ;
    o_LOWER.refcount = 0;
    o_LOWER.u.oper.name = "low byte '<";
    o_LOWER.u.oper.op = O_LOWER;
    o_LOWER.u.oper.prio = 4;
    o_HIGHER.obj = OPER_OBJ;
    o_HIGHER.refcount = 0;
    o_HIGHER.u.oper.name = "high byte '>";
    o_HIGHER.u.oper.op = O_HIGHER;
    o_HIGHER.u.oper.prio = 4;
    o_BANK.obj = OPER_OBJ;
    o_BANK.refcount = 0;
    o_BANK.u.oper.name = "bank byte '`";
    o_BANK.u.oper.op = O_BANK;
    o_BANK.u.oper.prio = 4;
    o_STRING.obj = OPER_OBJ;
    o_STRING.refcount = 0;
    o_STRING.u.oper.name = "decimal string '^";
    o_STRING.u.oper.op = O_STRING;
    o_STRING.u.oper.prio = 4;
    o_LOR.obj = OPER_OBJ;
    o_LOR.refcount = 0;
    o_LOR.u.oper.name = "logical or '||";
    o_LOR.u.oper.op = O_LOR;
    o_LOR.u.oper.prio = 5;
    o_LXOR.obj = OPER_OBJ;
    o_LXOR.refcount = 0;
    o_LXOR.u.oper.name = "logical xor '^^";
    o_LXOR.u.oper.op = O_LXOR;
    o_LXOR.u.oper.prio = 6;
    o_LAND.obj = OPER_OBJ;
    o_LAND.refcount = 0;
    o_LAND.u.oper.name = "logical and '&&";
    o_LAND.u.oper.op = O_LAND;
    o_LAND.u.oper.prio = 7;
    o_IN.obj = OPER_OBJ;
    o_IN.refcount = 0;
    o_IN.u.oper.name = "membership 'in";
    o_IN.u.oper.op = O_IN;
    o_IN.u.oper.prio = 8;
    o_CMP.obj = OPER_OBJ;
    o_CMP.refcount = 0;
    o_CMP.u.oper.name = "compare '<=>";
    o_CMP.u.oper.op = O_CMP;
    o_CMP.u.oper.prio = 8;
    o_EQ.obj = OPER_OBJ;
    o_EQ.refcount = 0;
    o_EQ.u.oper.name = "equal '==";
    o_EQ.u.oper.op = O_EQ;
    o_EQ.u.oper.prio = 8;
    o_NE.obj = OPER_OBJ;
    o_NE.refcount = 0;
    o_NE.u.oper.name = "not equal '!=";
    o_NE.u.oper.op = O_NE;
    o_NE.u.oper.prio = 8;
    o_LT.obj = OPER_OBJ;
    o_LT.refcount = 0;
    o_LT.u.oper.name = "less than '<";
    o_LT.u.oper.op = O_LT;
    o_LT.u.oper.prio = 8;
    o_GT.obj = OPER_OBJ;
    o_GT.refcount = 0;
    o_GT.u.oper.name = "greater than '>";
    o_GT.u.oper.op = O_GT;
    o_GT.u.oper.prio = 8;
    o_GE.obj = OPER_OBJ;
    o_GE.refcount = 0;
    o_GE.u.oper.name = "greater than or equal '>=";
    o_GE.u.oper.op = O_GE;
    o_GE.u.oper.prio = 8;
    o_LE.obj = OPER_OBJ;
    o_LE.refcount = 0;
    o_LE.u.oper.name = "less than or equal '<=";
    o_LE.u.oper.op = O_LE;
    o_LE.u.oper.prio = 8;
    o_OR.obj = OPER_OBJ;
    o_OR.refcount = 0;
    o_OR.u.oper.name = "binary or '|";
    o_OR.u.oper.op = O_OR;
    o_OR.u.oper.prio = 9;
    o_XOR.obj = OPER_OBJ;
    o_XOR.refcount = 0;
    o_XOR.u.oper.name = "binary exclusive or '^";
    o_XOR.u.oper.op = O_XOR;
    o_XOR.u.oper.prio = 10;
    o_AND.obj = OPER_OBJ;
    o_AND.refcount = 0;
    o_AND.u.oper.name = "binary and '&";
    o_AND.u.oper.op = O_AND;
    o_AND.u.oper.prio = 11;
    o_LSHIFT.obj = OPER_OBJ;
    o_LSHIFT.refcount = 0;
    o_LSHIFT.u.oper.name = "binary left shift '<<";
    o_LSHIFT.u.oper.op = O_LSHIFT;
    o_LSHIFT.u.oper.prio = 12;
    o_RSHIFT.obj = OPER_OBJ;
    o_RSHIFT.refcount = 0;
    o_RSHIFT.u.oper.name = "binary right shift '>>";
    o_RSHIFT.u.oper.op = O_RSHIFT;
    o_RSHIFT.u.oper.prio = 12;
    o_ADD.obj = OPER_OBJ;
    o_ADD.refcount = 0;
    o_ADD.u.oper.name = "add '+";
    o_ADD.u.oper.op = O_ADD;
    o_ADD.u.oper.prio = 13;
    o_SUB.obj = OPER_OBJ;
    o_SUB.refcount = 0;
    o_SUB.u.oper.name = "subtract '-";
    o_SUB.u.oper.op = O_SUB;
    o_SUB.u.oper.prio = 13;
    o_MUL.obj = OPER_OBJ;
    o_MUL.refcount = 0;
    o_MUL.u.oper.name = "multiply '*";
    o_MUL.u.oper.op = O_MUL;
    o_MUL.u.oper.prio = 14;
    o_DIV.obj = OPER_OBJ;
    o_DIV.refcount = 0;
    o_DIV.u.oper.name = "division '/";
    o_DIV.u.oper.op = O_DIV;
    o_DIV.u.oper.prio = 14;
    o_MOD.obj = OPER_OBJ;
    o_MOD.refcount = 0;
    o_MOD.u.oper.name = "modulo '%";
    o_MOD.u.oper.op = O_MOD;
    o_MOD.u.oper.prio = 14;
    o_EXP.obj = OPER_OBJ;
    o_EXP.refcount = 0;
    o_EXP.u.oper.name = "exponent '**";
    o_EXP.u.oper.op = O_EXP;
    o_EXP.u.oper.prio = 15;
    o_NEG.obj = OPER_OBJ;
    o_NEG.refcount = 0;
    o_NEG.u.oper.name = "unary negative '-";
    o_NEG.u.oper.op = O_NEG;
    o_NEG.u.oper.prio = 16;
    o_POS.obj = OPER_OBJ;
    o_POS.refcount = 0;
    o_POS.u.oper.name = "unary positive '+";
    o_POS.u.oper.op = O_POS;
    o_POS.u.oper.prio = 16;
    o_INV.obj = OPER_OBJ;
    o_INV.refcount = 0;
    o_INV.u.oper.name = "binary invert '~";
    o_INV.u.oper.op = O_INV;
    o_INV.u.oper.prio = 16;
    o_LNOT.obj = OPER_OBJ;
    o_LNOT.refcount = 0;
    o_LNOT.u.oper.name = "logical not '!";
    o_LNOT.u.oper.op = O_LNOT;
    o_LNOT.u.oper.prio = 16;
    o_SPLAT.obj = OPER_OBJ;
    o_SPLAT.refcount = 0;
    o_SPLAT.u.oper.name = "unary splat '*";
    o_SPLAT.u.oper.op = O_SPLAT;
    o_SPLAT.u.oper.prio = 17;
    o_CONCAT.obj = OPER_OBJ;
    o_CONCAT.refcount = 0;
    o_CONCAT.u.oper.name = "concatenate '..";
    o_CONCAT.u.oper.op = O_CONCAT;
    o_CONCAT.u.oper.prio = 18;
    o_X.obj = OPER_OBJ;
    o_X.refcount = 0;
    o_X.u.oper.name = "repeat 'x";
    o_X.u.oper.op = O_X;
    o_X.u.oper.prio = 19;
    o_MEMBER.obj = OPER_OBJ;
    o_MEMBER.refcount = 0;
    o_MEMBER.u.oper.name = "member '.";
    o_MEMBER.u.oper.op = O_MEMBER;
    o_MEMBER.u.oper.prio = 20;
}

void destroy_values(void)
{
    struct values_s *old;

    val_destroy(true_value);
    val_destroy(false_value);
    val_destroy(gap_value);

    while (values) {
        old = values;
        values = values->next;
        free(old);
    }
}
