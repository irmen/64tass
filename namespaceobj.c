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
#include "namespaceobj.h"
#include "eval.h"

static struct obj_s obj;

obj_t NAMESPACE_OBJ = &obj;

static void namespace_free(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    val_destroy(a->key);
    free(a);
}

static void namespace_free2(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    free(a);
}

static void garbage1(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    a->key->refcount--;
}

static void garbage2(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    value_t v = a->key;
    if (v->refcount & SIZE_MSB) {
        v->refcount -= SIZE_MSB - 1;
        v->obj->garbage(v, 1);
    } else v->refcount++;
}

static void destroy(value_t v1) {
    avltree_destroy(&v1->u.names.members, namespace_free);
}

static void garbage(value_t v1, int i) {
    switch (i) {
    case -1:
        avltree_destroy(&v1->u.names.members, garbage1);
        return;
    case 0:
        avltree_destroy(&v1->u.names.members, namespace_free2);
        return;
    case 1:
        avltree_destroy(&v1->u.names.members, garbage2);
        return;
    }
}

static int same(const value_t v1, const value_t v2) {
    const struct avltree_node *n;
    const struct avltree_node *n2;
    if (v2->obj != NAMESPACE_OBJ) return 0;
    n = avltree_first(&v1->u.names.members);
    n2 = avltree_first(&v2->u.names.members);
    while (n && n2) {
        if (pair_compare(n, n2)) return 0;
        n = avltree_next(n);
        n2 = avltree_next(n2);
    }
    return n == n2;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t epoint) {
    const struct pair_s *p;
    size_t i = 0, j, ln = 2, chars = 0;
    value_t tmp = NULL, *vals;
    value_t v;
    uint8_t *s;

    if (v1->u.names.len) {
        ln = v1->u.names.len;
        tmp = val_alloc(TUPLE_OBJ);
        tmp->u.list.data = vals = list_create_elements(tmp, ln);
        ln += 1;
        if (ln < 1) err_msg_out_of_memory(); /* overflow */
        if (v1->u.names.len) {
            const struct avltree_node *n = avltree_first(&v1->u.names.members);
            while (n) {
                p = cavltree_container_of(n, struct pair_s, node);
                v = p->key->obj->repr(p->key, epoint);
                if (!v || v->obj != STR_OBJ) {
                    tmp->u.list.len = i;
                    val_destroy(tmp);
                    return v;
                }
                ln += v->u.str.len;
                if (ln < v->u.str.len) err_msg_out_of_memory(); /* overflow */
                vals[i++] = v;
                n = avltree_next(n);
            }
        }
        tmp->u.list.len = i;
    }
    v = val_alloc(STR_OBJ);
    s = str_create_elements(v, ln);
    ln = 0;
    s[ln++] = '{';
    for (j = 0; j < i; j++) {
        if (j) s[ln++] = ',';
        memcpy(s + ln, vals[j]->u.str.data, vals[j]->u.str.len);
        ln += vals[j]->u.str.len;
        chars += vals[j]->u.str.len - vals[j]->u.str.chars;
    }
    s[ln++] = '}';
    if (tmp) val_destroy(tmp);
    v->u.str.data = s;
    v->u.str.len = ln;
    v->u.str.chars = ln - chars;
    return v;
}

MUST_CHECK value_t namespace_member(oper_t op, value_t v1) {
    value_t v2 = op->v2, v;
    if (op->op == &o_MEMBER) {
        value_t l;
        switch (v2->obj->type) {
        case T_IDENT:
            l = find_label2(&v2->u.ident.name, v1);
            if (l) {
                touch_label(l);
                return val_reference(l->u.label.value);
            }
            if (!referenceit) {
                return val_reference(none_value);
            }
            v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.ident.epoint);
            v->u.error.u.notdef.names = val_reference(v1);
            v->u.error.u.notdef.ident = v2->u.ident.name;
            v->u.error.u.notdef.down = 0;
            return v;
        case T_ANONIDENT:
            {
                ssize_t count;
                l = find_anonlabel2(v2->u.anonident.count, v1);
                if (l) {
                    touch_label(l);
                    return val_reference(l->u.label.value);
                }
                if (!referenceit) {
                    return val_reference(none_value);
                }
                count = v2->u.anonident.count;
                v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.anonident.epoint);
                v->u.error.u.notdef.names = val_reference(v1);
                v->u.error.u.notdef.ident.len = count + (count >= 0);
                v->u.error.u.notdef.ident.data = NULL;
                v->u.error.u.notdef.down = 0;
                return v;
            }
        case T_TUPLE:
        case T_LIST: return v2->obj->rcalc2(op);
        default: return v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

MUST_CHECK value_t new_namespace(const struct file_list_s *file_list, linepos_t epoint) {
    value_t val = val_alloc(NAMESPACE_OBJ);
    avltree_init(&val->u.names.members);
    val->u.names.file_list = file_list;
    val->u.names.epoint = *epoint;
    val->u.names.len = 0;
    return val;
}

void namespaceobj_init(void) {
    obj_init(&obj, T_NAMESPACE, "namespace");
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.repr = repr;
}
