/*
    $Id: identobj.c 1621 2018-08-30 20:34:53Z soci $

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
#include "identobj.h"
#include <string.h>
#include "eval.h"
#include "unicode.h"

#include "typeobj.h"
#include "errorobj.h"
#include "operobj.h"
#include "strobj.h"

static Type ident_obj;
static Type anonident_obj;

Type *const IDENT_OBJ = &ident_obj;
Type *const ANONIDENT_OBJ = &anonident_obj;


static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Ident *v1 = (Ident *)o1;
    size_t i2, i;
    uint8_t *s;
    const uint8_t *s2;
    uint8_t q;
    size_t chars;
    Str *v;
    size_t len;

    if (epoint == NULL) return NULL;
    s2 = v1->name.data;
    len = v1->name.len;
    i2 = str_quoting(s2, len, &q) + 9;
    if (i2 < 9) return NULL; /* overflow */
    chars = i2 - (len - calcpos(s2, len));
    if (chars > maxsize) return NULL;
    v = new_str2(i2);
    if (v == NULL) return NULL;
    v->chars = chars;
    s = v->data;

    memcpy(s, "ident(", 6);
    s += 6;
    *s++ = q;
    for (i = 0; i < len; i++) {
        s[i] = s2[i];
        if (s[i] == q) {
            s++; s[i] = q;
        }
    }
    s[i] = q;
    s[i + 1] = ')';
    return &v->v;
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Ident *v1 = (Ident *)o1;
    Str *v;
    size_t chars = calcpos(v1->name.data, v1->name.len);
    if (chars > maxsize) return NULL;
    v = new_str2(v1->name.len);
    if (v == NULL) return NULL;
    v->chars = chars;
    memcpy(v->data, v1->name.data, v1->name.len);
    return &v->v;
}

static MUST_CHECK Obj *anon_repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Anonident *v1 = (Anonident *)o1;
    Str *v;
    size_t len;

    if (epoint == NULL) return NULL;
    len = v1->count < 0 ? -v1->count : (v1->count + 1);
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    memset(v->data, v1->count >= 0 ? '+' : '-', len);
    return &v->v;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;
    switch (o2->obj->type) {
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

void identobj_init(void) {
    new_type(&ident_obj, T_IDENT, "ident", sizeof(Ident));
    ident_obj.repr = repr;
    ident_obj.str = str;
    ident_obj.calc2 = calc2;
    ident_obj.rcalc2 = rcalc2;
    new_type(&anonident_obj, T_ANONIDENT, "anonident", sizeof(Anonident));
    anonident_obj.repr = anon_repr;
    anonident_obj.calc2 = calc2;
    anonident_obj.rcalc2 = rcalc2;
}
