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
#include "dictobj.h"
#include "eval.h"

static struct obj_s dict_obj;

obj_t DICT_OBJ = &dict_obj;

static void dict_free(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    val_destroy(a->key);
    val_destroy(a->data);
    free(a);
}

static void destroy(value_t v1) {
    avltree_destroy(&v1->u.dict.members, dict_free);
    if (v1->u.dict.def) val_destroy(v1->u.dict.def);
}

static int same(const value_t v1, const value_t v2) {
    const struct avltree_node *n;
    const struct avltree_node *n2;
    if (v2->obj != DICT_OBJ || v1->u.dict.len != v2->u.dict.len) return 0;
    if (!v1->u.dict.def ^ !v2->u.dict.def) return 0;
    if (v1->u.dict.def && v2->u.dict.def && !obj_same(v1->u.dict.def, v2->u.dict.def)) return 0;
    n = avltree_first(&v1->u.dict.members);
    n2 = avltree_first(&v2->u.dict.members);
    while (n && n2) {
        const struct pair_s *p, *p2;
        if (pair_compare(n, n2)) return 0;
        p = cavltree_container_of(n, struct pair_s, node);
        p2 = cavltree_container_of(n2, struct pair_s, node);
        if (!obj_same(p->data, p2->data)) return 0;
        n = avltree_next(n);
        n2 = avltree_next(n2);
    }
    return n == n2;
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.dict.len);
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t epoint) {
    const struct pair_s *p;
    size_t i = 0, j, len = 2, chars = 0;
    value_t *tmp = NULL;
    value_t v;
    uint8_t *s;
    unsigned int def = (v1->u.dict.def != NULL);
    if (v1->u.dict.len || def) {
        len = v1->u.dict.len * 2;
        if (len < v1->u.dict.len) err_msg_out_of_memory(); /* overflow */
        len += def;
        tmp = (value_t *)malloc(len * sizeof(value_t));
        if (!tmp || len < def || len > SIZE_MAX / sizeof(value_t)) err_msg_out_of_memory(); /* overflow */
        len += 1 + def;
        if (len < 1 + def) err_msg_out_of_memory(); /* overflow */
        if (v1->u.dict.len) {
            const struct avltree_node *n = avltree_first(&v1->u.dict.members);
            while (n) {
                p = cavltree_container_of(n, struct pair_s, node);
                v = p->key->obj->repr(p->key, epoint);
                if (!v || v->obj != STR_OBJ) goto error;
                len += v->u.str.len;
                if (len < v->u.str.len) err_msg_out_of_memory(); /* overflow */
                tmp[i++] = v;
                v = p->data->obj->repr(p->data, epoint);
                if (!v || v->obj != STR_OBJ) goto error;
                len += v->u.str.len;
                if (len < v->u.str.len) err_msg_out_of_memory(); /* overflow */
                tmp[i++] = v;
                n = avltree_next(n);
            }
        }
        if (def) {
            v = v1->u.dict.def->obj->repr(v1->u.dict.def, epoint);
            if (!v || v->obj != STR_OBJ) {
            error:
                while (i--) val_destroy(tmp[i]);
                free(tmp);
                return v;
            }
            len += v->u.str.len;
            if (len < v->u.str.len) err_msg_out_of_memory(); /* overflow */
            tmp[i] = v;
        }
    }
    v = val_alloc(STR_OBJ);
    s = str_create_elements(v, len);
    len = 0;
    s[len++] = '{';
    for (j = 0; j < i; j++) {
        if (j) s[len++] = (j & 1) ? ':' : ',';
        memcpy(s + len, tmp[j]->u.str.data, tmp[j]->u.str.len);
        len += tmp[j]->u.str.len;
        chars += tmp[j]->u.str.len - tmp[j]->u.str.chars;
        val_destroy(tmp[j]);
    }
    if (def) {
        if (j) s[len++] = ',';
        s[len++] = ':';
        memcpy(s + len, tmp[j]->u.str.data, tmp[j]->u.str.len);
        len += tmp[j]->u.str.len;
        chars += tmp[j]->u.str.len - tmp[j]->u.str.chars;
        val_destroy(tmp[j]);
        j++;
    }
    s[len++] = '}';
    free(tmp);
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
    return v;
}

static MUST_CHECK value_t calc2(oper_t op) {
    if (op->op == &o_INDEX) {
        value_t v1 = op->v1, v2 = op->v2;
        struct pair_s pair;
        const struct avltree_node *b;
        value_t err;

        if (v2->u.funcargs.len != 1) {
            err_msg_argnum(v2->u.funcargs.len, 1, 1, op->epoint2);
            return val_reference(none_value);
        }
        v2 = v2->u.funcargs.val->val;

        pair.key = v2;
        err = obj_hash(pair.key, &pair.hash, op->epoint2);
        if (err) return err;
        b = avltree_lookup(&pair.node, &v1->u.dict.members, pair_compare);
        if (b) {
            const struct pair_s *p = cavltree_container_of(b, struct pair_s, node);
            return val_reference(p->data);
        }
        if (v1->u.dict.def) {
            return val_reference(v1->u.dict.def);
        }
        return new_error_obj(ERROR_____KEY_ERROR, op->epoint2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    if (op->op == &o_IN) {
        struct pair_s p;
        struct avltree_node *b;
        value_t err;

        p.key = op->v1;
        err = obj_hash(p.key, &p.hash, op->epoint);
        if (err) return err;
        b = avltree_lookup(&p.node, &op->v2->u.dict.members, pair_compare);
        return truth_reference(b != NULL);
    }
    return obj_oper_error(op);
}


void dictobj_init(void) {
    obj_init(&dict_obj, T_DICT, "<dict>");
    dict_obj.destroy = destroy;
    dict_obj.same = same;
    dict_obj.len = len;
    dict_obj.repr = repr;
    dict_obj.calc2 = calc2;
    dict_obj.rcalc2 = rcalc2;
}
