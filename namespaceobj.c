/*
    $Id: namespaceobj.c 2727 2021-10-03 20:21:13Z soci $

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
#include "namespaceobj.h"
#include <string.h>
#include "variables.h"
#include "eval.h"
#include "error.h"
#include "arguments.h"
#include "64tass.h"

#include "listobj.h"
#include "strobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "labelobj.h"
#include "errorobj.h"
#include "symbolobj.h"
#include "anonsymbolobj.h"
#include "codeobj.h"
#include "macroobj.h"
#include "mfuncobj.h"
#include "boolobj.h"

static Type obj;

Type *const NAMESPACE_OBJ = &obj;

static MUST_CHECK Obj *namespace_from_obj(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_NAMESPACE: return val_reference(v1);
    case T_CODE:
        return val_reference(Obj(Code(v1)->names));
    case T_UNION:
    case T_STRUCT:
        return val_reference(Obj(Struct(v1)->names));
    case T_MFUNC:
        return val_reference(Obj(Mfunc(v1)->names));
    default: break;
    }
    return new_error_conv(v1, NAMESPACE_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return namespace_from_obj(op->v2, op->epoint2);
}

static FAST_CALL void destroy(Obj *o1) {
    Namespace *v1 = Namespace(o1);
    size_t i;
    if (v1->data == NULL) return;
    for (i = 0; i <= v1->mask; i++) {
        if (v1->data[i] != NULL) val_destroy(Obj(v1->data[i]));
    }
    free(v1->data);
}

static FAST_CALL void garbage(Obj *o1, int j) {
    Namespace *v1 = Namespace(o1);
    size_t i;
    if (v1->data == NULL) return;
    switch (j) {
    case -1:
        for (i = 0; i <= v1->mask; i++) {
            if (v1->data[i] != NULL) v1->data[i]->v.refcount--;
        }
        return;
    case 0:
        free(v1->data);
        return;
    case 1:
        for (i = 0; i <= v1->mask; i++) {
            Obj *v = Obj(v1->data[i]);
            if (v == NULL) continue;
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        return;
    }
}

static Label *namespace_lookup(const Namespace *ns, const Label *p) {
    size_t mask = ns->mask;
    size_t hash = (size_t)p->hash;
    size_t offs = hash & mask;
    if (ns->data == NULL) return NULL;
    while (ns->data[offs] != NULL) {
        Label *d = ns->data[offs];
        if (p->hash == d->hash && p->strength == d->strength) {
            if (d->defpass == pass || (d->constant && (!fixeddig || d->defpass == pass - 1))) {
                const str_t *s1 = &p->cfname;
                const str_t *s2 = &d->cfname;
                if (s1->len == s2->len && (s1->data == s2->data || memcmp(s1->data, s2->data, s1->len) == 0)) {
                    return d;
                }
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    }
    return NULL;
}

static bool namespace_issubset(Namespace *v1, const Namespace *v2) {
    size_t n, ln;
    bool ret = true;
    if (v1->len == 0) return ret;
    ln = v1->len; v1->len = 0;
    for (n = 0; n <= v1->mask; n++) {
        const Label *p2, *p = v1->data[n];
        if (p == NULL) continue;
        if (p->defpass == pass || (p->constant && (!fixeddig || p->defpass == pass - 1))) {
            p2 = namespace_lookup(v2, p);
            if (p2 == NULL) {
                ret = false;
                break;
            }
            if (!p->v.obj->same(Obj(p), Obj(p2))) {
                ret = false;
                break;
            }
        }
    }
    v1->len = ln;
    return ret;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    Namespace *v1 = Namespace(o1), *v2 = Namespace(o2);
    if (o1->obj != o2->obj) return false;
    return namespace_issubset(v1, v2) && namespace_issubset(v2, v1);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    const Namespace *v1 = Namespace(o1);
    size_t i = 0, j, ln = 13, chars = 13;
    Obj **vals;
    Str *str;
    Tuple *tuple = NULL;
    uint8_t *s;

    if (epoint == NULL) return NULL;
    if (v1->len != 0) {
        size_t n;
        ln = v1->len;
        if (add_overflow(ln, 12, &chars)) return NULL;
        if (chars > maxsize) return NULL;
        tuple = new_tuple(ln);
        vals = tuple->data;
        ln = chars;
        for (n = 0; n <= v1->mask; n++) {
            Label *p = v1->data[n];
            Obj *v;
            if (p == NULL) continue;
            if (p->defpass != pass && !(p->constant && (!fixeddig || p->defpass == pass - 1))) {
                ln--;
                chars--;
                continue;
            }
            v = p->v.obj->repr(Obj(p), epoint, maxsize - chars);
            if (v == NULL || v->obj != STR_OBJ) goto error;
            str = Str(v);
            if (inc_overflow(&ln, str->len)) goto error2;
            chars += str->chars;
            if (chars > maxsize) {
            error2:
                val_destroy(v);
                v = NULL;
            error:
                tuple->len = i;
                val_destroy(Obj(tuple));
                return v;
            }
            vals[i++] = v;
        }
        tuple->len = i;
        if (i == 0) { ln++; chars++; }
    }
    str = new_str2(ln);
    if (str == NULL) {
        if (tuple != NULL) val_destroy(Obj(tuple));
        return NULL;
    }
    str->chars = chars;
    s = str->data;
    memcpy(s, "namespace({", 11);
    s += 11;
    for (j = 0; j < i; j++) {
        Str *str2 = Str(vals[j]);
        if (j != 0) *s++ = ',';
        if (str2->len != 0) {
            memcpy(s, str2->data, str2->len);
            s += str2->len;
        }
    }
    *s++ = '}';
    *s = ')';
    if (tuple != NULL) val_destroy(Obj(tuple));
    return Obj(str);
}

MUST_CHECK Obj *namespace_member(oper_t op, Namespace *v1) {
    Obj *o2 = op->v2;
    Error *err;
    Label *l;
    switch (o2->obj->type) {
    case T_SYMBOL:
        {
            Symbol *v2 = Symbol(o2);
            l = find_label2(&v2->name, v1);
            if (l != NULL) {
                if (diagnostics.case_symbol && str_cmp(&v2->name, &l->name) != 0) err_msg_symbol_case(&v2->name, l, op->epoint2);
                touch_label(l);
                return val_reference(l->value);
            }
            if (!referenceit || (constcreated && pass < max_pass)) {
                return ref_none();
            }
            err = new_error(ERROR___NOT_DEFINED, op->epoint2);
            err->u.notdef.names = ref_namespace(v1);
            err->u.notdef.symbol = Obj(ref_symbol(v2));
            err->u.notdef.down = false;
            return Obj(err);
        }
    case T_ANONSYMBOL:
        {
            Anonsymbol *v2 = Anonsymbol(o2);
            l = find_anonlabel2(v2->count, v1);
            if (l != NULL) {
                touch_label(l);
                return val_reference(l->value);
            }
            if (!referenceit || (constcreated && pass < max_pass)) {
                return ref_none();
            }
            err = new_error(ERROR___NOT_DEFINED, op->epoint2);
            err->u.notdef.names = ref_namespace(v1);
            err->u.notdef.symbol = Obj(ref_anonsymbol(v2));
            err->u.notdef.down = false;
            return Obj(err);
        }
    case T_ERROR:
    case T_NONE: return val_reference(o2);
    default:
        if (o2->obj->iterable) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

MALLOC Namespace *new_namespace(const struct file_list_s *file_list, linepos_t epoint) {
    Namespace *val = Namespace(val_alloc(NAMESPACE_OBJ));
    val->data = NULL;
    val->mask = 0;
    val->file_list = file_list;
    val->epoint = *epoint;
    val->len = 0;
    val->backr = 0;
    val->forwr = 0;
    return val;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    if (op->op == O_MEMBER) {
        return namespace_member(op, Namespace(op->v1));
    }
    if (op->v2 == none_value || op->v2->obj == ERROR_OBJ) return val_reference(op->v2);
    return obj_oper_error(op);
}

static MUST_CHECK Obj *contains(oper_t op) {
    Namespace *v2 = Namespace(op->v2);
    Obj *o1 = op->v1;
    Label *l;
    switch (o1->obj->type) {
    case T_SYMBOL:
        {
            Symbol *v1 = Symbol(o1);
            l = find_label2(&v1->name, v2);
            if (l != NULL) {
                if (diagnostics.case_symbol && str_cmp(&v1->name, &l->name) != 0) err_msg_symbol_case(&v1->name, l, op->epoint);
                if (l->constant) {
                    err_msg_not_variable(l, &v1->name, op->epoint);
                    l = NULL;
                }
            }
        }
        break;
    case T_ANONSYMBOL:
        l = find_anonlabel2(Anonsymbol(o1)->count, v2);
        if (l != NULL && l->constant) {
            err_msg_not_variable(l, &l->name, op->epoint);
            l = NULL;
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o1);
    default:
        return obj_oper_error(op);
    }
    if (l != NULL) touch_label(l);
    return truth_reference(op->op == O_IN ? (l != NULL) : (l == NULL));
}

void namespaceobj_init(void) {
    Type *type = new_type(&obj, T_NAMESPACE, "namespace", sizeof(Namespace));
    type->convert = convert;
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->repr = repr;
    type->calc2 = calc2;
    type->contains = contains;
}

void namespaceobj_names(void) {
    new_builtin("namespace", val_reference(Obj(NAMESPACE_OBJ)));
}

Namespace *get_namespace(const Obj *o) {
    switch (o->obj->type) {
    case T_CODE:
        return Code(o)->names;
    case T_UNION:
    case T_STRUCT:
        return Struct(o)->names;
    case T_MFUNC:
        return Mfunc(o)->names;
    case T_NAMESPACE:
        return Namespace(o);
    default:
        return NULL;
    }
}
