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
#include "namespaceobj.h"
#include "variables.h"
#include "eval.h"
#include "intobj.h"
#include "listobj.h"
#include "error.h"
#include "strobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type obj;

Type *NAMESPACE_OBJ = &obj;

static void namespace_free(struct avltree_node *aa)
{
    struct namespacekey_s *a = avltree_container_of(aa, struct namespacekey_s, node);
    val_destroy(&a->key->v);
    free(a);
}

static void namespace_free2(struct avltree_node *aa)
{
    struct namespacekey_s *a = avltree_container_of(aa, struct namespacekey_s, node);
    free(a);
}

static void garbage1(struct avltree_node *aa)
{
    struct namespacekey_s *a = avltree_container_of(aa, struct namespacekey_s, node);
    a->key->v.refcount--;
}

static void garbage2(struct avltree_node *aa)
{
    struct namespacekey_s *a = avltree_container_of(aa, struct namespacekey_s, node);
    Obj *v = &a->key->v;
    if (v->refcount & SIZE_MSB) {
        v->refcount -= SIZE_MSB - 1;
        v->obj->garbage(v, 1);
    } else v->refcount++;
}

static void destroy(Obj *o1) {
    Namespace *v1 = (Namespace *)o1;
    avltree_destroy(&v1->members, namespace_free);
}

static void garbage(Obj *o1, int i) {
    Namespace *v1 = (Namespace *)o1;
    switch (i) {
    case -1:
        avltree_destroy(&v1->members, garbage1);
        return;
    case 0:
        avltree_destroy(&v1->members, namespace_free2);
        return;
    case 1:
        avltree_destroy(&v1->members, garbage2);
        return;
    }
}

static struct oper_s pair_oper;
static int namespacekey_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct namespacekey_s *a = cavltree_container_of(aa, struct namespacekey_s, node);
    const struct namespacekey_s *b = cavltree_container_of(bb, struct namespacekey_s, node);
    Obj *result;
    int h = a->hash - b->hash;

    if (h) return h;
    pair_oper.v1 = (Obj *)a->key;
    pair_oper.v2 = (Obj *)b->key;
    result = pair_oper.v1->obj->calc2(&pair_oper);
    if (result->obj == INT_OBJ) h = ((Int *)result)->len;
    else h = pair_oper.v1->obj->type - pair_oper.v2->obj->type;
    val_destroy(result);
    return h;
}

static int same(const Obj *o1, const Obj *o2) {
    const Namespace *v1 = (const Namespace *)o1, *v2 = (const Namespace *)o2;
    const struct avltree_node *n;
    const struct avltree_node *n2;
    if (o2->obj != NAMESPACE_OBJ) return 0;
    n = avltree_first(&v1->members);
    n2 = avltree_first(&v2->members);
    while (n && n2) {
        if (namespacekey_compare(n, n2)) return 0;
        n = avltree_next(n);
        n2 = avltree_next(n2);
    }
    return n == n2;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint) {
    const Namespace *v1 = (const Namespace *)o1;
    const struct namespacekey_s *p;
    size_t i = 0, j, ln = 2, chars = 0;
    Obj **vals;
    Str *str;
    Tuple *tuple = NULL;
    uint8_t *s;

    if (v1->len) {
        ln = v1->len;
        tuple = new_tuple();
        tuple->data = vals = list_create_elements(tuple, ln);
        ln += 1;
        if (ln < 1) err_msg_out_of_memory(); /* overflow */
        if (v1->len) {
            const struct avltree_node *n = avltree_first(&v1->members);
            while (n) {
                Obj *v;
                p = cavltree_container_of(n, struct namespacekey_s, node);
                v = ((Obj *)p->key)->obj->repr((Obj *)p->key, epoint);
                if (!v || v->obj != STR_OBJ) {
                    tuple->len = i;
                    val_destroy(&tuple->v);
                    return v;
                }
                str = (Str *)v;
                ln += str->len;
                if (ln < str->len) err_msg_out_of_memory(); /* overflow */
                vals[i++] = &str->v;
                n = avltree_next(n);
            }
        }
        tuple->len = i;
    }
    str = new_str();
    s = str_create_elements(str, ln);
    ln = 0;
    s[ln++] = '{';
    for (j = 0; j < i; j++) {
        Str *str2 = (Str *)vals[j];
        if (j) s[ln++] = ',';
        memcpy(s + ln, str2->data, str2->len);
        ln += str2->len;
        chars += str2->len - str2->chars;
    }
    s[ln++] = '}';
    if (tuple) val_destroy(&tuple->v);
    str->data = s;
    str->len = ln;
    str->chars = ln - chars;
    return &str->v;
}

MUST_CHECK Obj *namespace_member(oper_t op, Namespace *v1) {
    Obj *o2 = op->v2;
    Error *err;
    if (op->op == &o_MEMBER) {
        Label *l;
        switch (o2->obj->type) {
        case T_IDENT:
            {
                Ident *v2 = (Ident *)o2;
                l = find_label2(&v2->name, v1);
                if (l) {
                    touch_label(l);
                    return val_reference(l->value);
                }
                if (!referenceit) {
                    return (Obj *)ref_none();
                }
                err = new_error(ERROR___NOT_DEFINED, &v2->epoint);
                err->u.notdef.names = ref_namespace(v1);
                err->u.notdef.ident = v2->name;
                err->u.notdef.down = 0;
                return &err->v;
            }
        case T_ANONIDENT:
            {
                Anonident *v2 = (Anonident *)o2;
                ssize_t count;
                l = find_anonlabel2(v2->count, v1);
                if (l) {
                    touch_label(l);
                    return val_reference(l->value);
                }
                if (!referenceit) {
                    return (Obj *)ref_none();
                }
                count = v2->count;
                err = new_error(ERROR___NOT_DEFINED, &v2->epoint);
                err->u.notdef.names = ref_namespace(v1);
                err->u.notdef.ident.len = count + (count >= 0);
                err->u.notdef.ident.data = NULL;
                err->u.notdef.down = 0;
                return &err->v;
            }
        case T_TUPLE:
        case T_LIST: return o2->obj->rcalc2(op);
        default: return o2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

MUST_CHECK Namespace *new_namespace(const struct file_list_s *file_list, linepos_t epoint) {
    Namespace *val = (Namespace *)val_alloc(NAMESPACE_OBJ);
    avltree_init(&val->members);
    val->file_list = file_list;
    val->epoint = *epoint;
    val->len = 0;
    return val;
}

void namespaceobj_init(void) {
    static struct linepos_s nopoint;

    new_type(&obj, T_NAMESPACE, "namespace", sizeof(Namespace));
    obj_init(&obj);
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.repr = repr;

    pair_oper.op = &o_CMP;
    pair_oper.epoint = &nopoint;
    pair_oper.epoint2 = &nopoint;
    pair_oper.epoint3 = &nopoint;
}
