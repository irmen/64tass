/*
    $Id: symbolobj.c 3122 2023-09-16 10:44:56Z soci $

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
#include "symbolobj.h"
#include <string.h>
#include "eval.h"
#include "unicode.h"
#include "error.h"
#include "file.h"
#include "values.h"
#include "arguments.h"

#include "typeobj.h"
#include "strobj.h"
#include "errorobj.h"

static Type obj;

Type *const SYMBOL_OBJ = &obj;

Obj *new_symbol(const str_t *name, linepos_t epoint) {
    Symbol *symbol = Symbol(val_alloc(SYMBOL_OBJ));
    if (not_in_file(name->data, current_file_list->file)) str_cpy(&symbol->name, name);
    else symbol->name = *name;
    symbol->cfname.data = NULL;
    symbol->cfname.len = 0;
    symbol->hash = -1;
    symbol->file_list = current_file_list;
    symbol->epoint = *epoint;
    return Obj(symbol);
}

static MUST_CHECK Obj *symbol_from_obj(Obj *o1, linepos_t epoint) {
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_SYMBOL:
        return val_reference(o1);
    case T_STR:
        if (Str(o1)->len == 0) return Obj(new_error(ERROR__EMPTY_STRING, epoint));
        else {
            Str *v1 = Str(o1);
            uint8_t *s;
            Symbol *symbol;
            size_t ln = get_label2(v1->data, v1->data + v1->len);
            if (ln != v1->len) {
                struct linepos_s epoint2;
                epoint2.line = epoint->line;
                epoint2.pos = interstring_position(epoint, v1->data, ln);
                return new_error_conv(o1, SYMBOL_OBJ, &epoint2);
            }
            s = allocate_array(uint8_t, ln);
            if (s == NULL) return new_error_mem(epoint);
            memcpy(s, v1->data, ln);
            symbol = Symbol(val_alloc(SYMBOL_OBJ));
            symbol->name.len = ln;
            symbol->name.data = s;
            symbol->cfname.data = NULL;
            symbol->cfname.len = 0;
            symbol->hash = -1;
            symbol->file_list = current_file_list;
            symbol->epoint = *epoint;
            return Obj(symbol);
        }
    default: break;
    }
    return new_error_conv(o1, SYMBOL_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return symbol_from_obj(op->v2, op->epoint2);
}

static FAST_CALL NO_INLINE void symbol_destroy(Symbol *v1) {
    free((char *)v1->name.data);
}

static FAST_CALL NO_INLINE bool symbol_same(const Symbol *v1, const Symbol *v2) {
    return memcmp(v1->name.data, v2->name.data, v2->name.len) == 0;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Symbol *v1 = Symbol(o1), *v2 = Symbol(o2);
    if (o1->obj != o2->obj || v1->name.len != v2->name.len) return false;
    switch (v1->name.len) {
    case 0: return true;
    case 1: return v1->name.data[0] == v2->name.data[0];
    default: return v1->name.data == v2->name.data || symbol_same(v1, v2);
    }
}

static FAST_CALL void destroy(Obj *o1) {
    Symbol *v1 = Symbol(o1);
    const struct file_s *cfile = v1->file_list->file;
    if (not_in_file(v1->name.data, cfile)) symbol_destroy(v1);
    if (v1->cfname.data != NULL && v1->name.data != v1->cfname.data) free((uint8_t *)v1->cfname.data);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Symbol *v1 = Symbol(o1);
    size_t chars;
    Str *v;
    size_t len;

    chars = calcpos(v1->name.data, v1->name.len) + 1;
    if (chars < 1 || chars > maxsize) return NULL;/* overflow */
    if (add_overflow(v1->name.len, 1, &len)) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = chars;
    v->data[0] = '.';
    memcpy(v->data + 1, v1->name.data, len - 1);
    return Obj(v);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Symbol *v1 = Symbol(o1);
    Str *v;
    size_t chars = calcpos(v1->name.data, v1->name.len);
    if (chars > maxsize) return NULL;
    v = new_str2(v1->name.len);
    if (v == NULL) return NULL;
    v->chars = chars;
    memcpy(v->data, v1->name.data, v1->name.len);
    return Obj(v);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Symbol *v1 = Symbol(o1);
    str_t s;
    size_t l;
    unsigned int h;
    if (v1->name.len == 0) {
        *hs = 0;
        return NULL;
    }
    if (v1->hash >= 0) {
        *hs = v1->hash;
        return NULL;
    }
    if (v1->cfname.data == NULL) {
        str_cfcpy(&v1->cfname, &v1->name);
        if (v1->cfname.data != v1->name.data) str_cfcpy(&v1->cfname, NULL);
    }
    s = v1->cfname;
    h = (unsigned int)*s.data << 7;
    l = s.len;
    while ((l--) != 0) h = (1000003 * h) ^ *s.data++;
    h ^= (unsigned int)s.len;
    *hs = v1->hash = h & ((~0U) >> 1);
    return NULL;
}

bool symbol_cfsame(Symbol *v1, Symbol *v2) {
    str_t *n1 = &v1->cfname, *n2 = &v2->cfname;
    if (n1->data == NULL) {
        str_cfcpy(n1, &v1->name);
        if (n1->data != v1->name.data) str_cfcpy(n1, NULL);
    }
    if (n2->data == NULL) {
        str_cfcpy(n2, &v2->name);
        if (n2->data != v2->name.data) str_cfcpy(n2, NULL);
    }
    if (str_cmp(n1, n2) != 0) return false;
    if (diagnostics.case_symbol && str_cmp(&v1->name, &v2->name) != 0) err_msg_symbol_case2(v1, v2);
    return true;
}

static inline int icmp(oper_t op) {
    Symbol *v1 = Symbol(op->v1), *v2 = Symbol(op->v2);
    str_t *n1 = &v1->cfname, *n2 = &v2->cfname;
    int h;
    if (n1->data == NULL) {
        str_cfcpy(n1, &v1->name);
        if (n1->data != v1->name.data) str_cfcpy(n1, NULL);
    }
    if (n2->data == NULL) {
        str_cfcpy(n2, &v2->name);
        if (n2->data != v2->name.data) str_cfcpy(n2, NULL);
    }
    h = memcmp(n1->data, n2->data, (n1->len < n2->len) ? n1->len : n2->len);
    if (h != 0) return h;
    if (n1->len < n2->len) return -1;
    return (n1->len > n2->len) ? 1 : 0;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;
    int i;
    switch (o2->obj->type) {
    case T_SYMBOL:
        i = icmp(op);
        if (i == 0 && diagnostics.case_symbol && str_cmp(&Symbol(op->v1)->name, &Symbol(o2)->name) != 0) err_msg_symbol_case2(Symbol(op->v1), Symbol(o2));
        return obj_oper_compare(op, i);
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != O_MEMBER && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    if (op->op == O_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

void symbolobj_init(void) {
    Type *type = new_type(&obj, T_SYMBOL, "symbol", sizeof(Symbol));
    type->convert = convert;
    type->destroy = destroy;
    type->same = same;
    type->hash = hash;
    type->repr = repr;
    type->str = str;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
}
