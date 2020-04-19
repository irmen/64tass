/*
    $Id: dictobj.c 2179 2020-03-28 21:58:25Z soci $

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
#include "dictobj.h"
#include <string.h>
#include "eval.h"
#include "error.h"
#include "variables.h"

#include "intobj.h"
#include "listobj.h"
#include "strobj.h"
#include "boolobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"

static Type obj;

Type *const DICT_OBJ = &obj;

static Dict *new_dict(size_t ln) {
    size_t ln1, ln2, ln3;
    Dict *v;
    struct pair_s *p;
    if (ln > lenof(v->u.val)) {
        if (ln > SIZE_MAX / (sizeof(struct pair_s) + sizeof(size_t) * 2)) return NULL; /* overflow */
        ln1 = ln * 3 / 2;
        ln2 = 8; while (ln1 > ln2) ln2 <<= 1;
        ln3 = ln2 * ((ln2 <= (1 << (sizeof(uint8_t)*8))) ? sizeof(uint8_t) : sizeof(size_t));
        p = (struct pair_s *)malloc(ln * sizeof(struct pair_s) + ln3);
        if (p == NULL) return NULL; /* out of memory */
        memset(&p[ln], 255, ln3);
    } else {
        p = NULL;
        ln2 = 1;
    }
    v = (Dict *)val_alloc(DICT_OBJ);
    if (p == NULL) {
        v->data = v->u.val;
    } else {
        v->data = p;
        v->u.s.max = ln;
        v->u.s.mask = ln2 - 1;
    }
    v->len = 0;
    return v;
}

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_DICT: return val_reference(v1);
    default: break;
    }
    return (Obj *)new_error_conv(v1, DICT_OBJ, epoint);
}

static FAST_CALL void destroy(Obj *o1) {
    Dict *v1 = (Dict *)o1;
    size_t i;
    for (i = 0; i < v1->len; i++) {
        struct pair_s *a = &v1->data[i];
        val_destroy(a->key);
        if (a->data != NULL) val_destroy(a->data);
    }
    if (v1->u.val != v1->data) free(v1->data);
    if (v1->def != NULL) val_destroy(v1->def);
}

static FAST_CALL void garbage(Obj *o1, int j) {
    Dict *v1 = (Dict *)o1;
    Obj *v;
    size_t i;
    switch (j) {
    case -1:
        for (i = 0; i < v1->len; i++) {
            struct pair_s *a = &v1->data[i];
            a->key->refcount--;
            if (a->data != NULL) a->data->refcount--;
        }
        v = v1->def;
        if (v != NULL) v->refcount--;
        return;
    case 0:
        if (v1->u.val != v1->data) free(v1->data);
        return;
    case 1:
        for (i = 0; i < v1->len; i++) {
            struct pair_s *a = &v1->data[i];
            v = a->data;
            if (v != NULL) {
                if ((v->refcount & SIZE_MSB) != 0) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
            }
            v = a->key;
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        v = v1->def;
        if (v == NULL) return;
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static struct oper_s pair_oper;

static bool rpair_equal(Obj *o1, Obj *o2) {
    bool h;
    Obj *result;
    struct iter_s iter1;
    struct iter_s iter2;
    iter1.data = o1; o1->obj->getiter(&iter1);
    iter2.data = o2; o2->obj->getiter(&iter2);
    do {
        o1 = iter1.next(&iter1);
        o2 = iter2.next(&iter2);
        if (o1 == NULL) {
            h = (o2 == NULL);
            break;
        }
        if (o2 == NULL) {
            h = false;
            break;
        }
        if (o1->obj->iterable || o2->obj->iterable) {
            h = o1->obj->iterable && o2->obj->iterable && rpair_equal(o1, o2);
        } else {
            pair_oper.v1 = o1;
            pair_oper.v2 = o2;
            pair_oper.inplace = NULL;
            result = o1->obj->calc2(&pair_oper);
            h = (result == &true_value->v);
            val_destroy(result);
        }
    } while (h);
    iter_destroy(&iter2);
    iter_destroy(&iter1);
    return h;
}

static bool pair_equal(const struct pair_s *a, const struct pair_s *b)
{
    Obj *result;
    bool h;
    if (a->hash != b->hash) return false;
    if (a->key->obj->iterable || b->key->obj->iterable) {
        return a->key->obj->iterable && b->key->obj->iterable && rpair_equal(a->key, b->key);
    }
    pair_oper.v1 = a->key;
    pair_oper.v2 = b->key;
    pair_oper.inplace = NULL;
    result = pair_oper.v1->obj->calc2(&pair_oper);
    h = (result == &true_value->v);
    val_destroy(result);
    return h;
}

static void dict_update(Dict *dict, const struct pair_s *p) {
    struct pair_s *d;
    if (dict->u.val == dict->data) {
        /* nothing */
    } else if (dict->u.s.mask < (1 << (sizeof(uint8_t)*8))) {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        uint8_t *indexes = (uint8_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != (uint8_t)~0) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_equal(p, d)) {
                if (d->data != NULL) val_destroy(d->data);
                d->data = (p->data == NULL) ? NULL : val_reference(p->data);
                return;
            }
            hash >>= 5;
            offs = (5 * offs + hash + 1) & mask;
        } 
        indexes[offs] = dict->len;
    } else {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        size_t *indexes = (size_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != SIZE_MAX) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_equal(p, d)) {
                if (d->data != NULL) val_destroy(d->data);
                d->data = (p->data == NULL) ? NULL : val_reference(p->data);
                return;
            }
            hash >>= 5;
            offs = (5 * offs + hash + 1) & mask;
        } 
        indexes[offs] = dict->len;
    }
    d = &dict->data[dict->len];
    d->hash = p->hash;
    d->key = val_reference(p->key);
    d->data = (p->data == NULL) ? NULL : val_reference(p->data);
    dict->len++;
}

static const struct pair_s *dict_lookup(const Dict *dict, const struct pair_s *p) {
    struct pair_s *d;
    if (dict->u.val == dict->data) {
        d = &dict->data[0];
        if (p->key == d->key || pair_equal(p, d)) return d;
    } else if (dict->u.s.mask < (1 << (sizeof(uint8_t)*8))) {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        uint8_t *indexes = (uint8_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != (uint8_t)~0) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_equal(p, d)) return d;
            hash >>= 5;
            offs = (5 * offs + hash + 1) & mask;
        } 
    } else {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        size_t *indexes = (size_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != SIZE_MAX) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_equal(p, d)) return d;
            hash >>= 5;
            offs = (5 * offs + hash + 1) & mask;
        } 
    }
    return NULL;
}

static void reindex(Dict *dict) {
    if (dict->u.val == dict->data) {
        return;
    } else if (dict->u.s.mask < (1 << (sizeof(uint8_t)*8))) {
        unsigned int i;
        size_t mask = dict->u.s.mask;
        uint8_t *indexes = (uint8_t *)&dict->data[dict->u.s.max];
        for (i = 0; i < dict->len; i++) {
            size_t hash = (size_t)dict->data[i].hash;
            size_t offs = hash & mask;
            while (indexes[offs] != (uint8_t)~0) {
                hash >>= 5;
                offs = (5 * offs + hash + 1) & mask;
            }
            indexes[offs] = i;
        }
    } else {
        size_t i;
        size_t mask = dict->u.s.mask;
        size_t *indexes = (size_t *)&dict->data[dict->u.s.max];
        for (i = 0; i < dict->len; i++) {
            size_t hash = (size_t)dict->data[i].hash;
            size_t offs = hash & mask;
            while (indexes[offs] != SIZE_MAX) {
                hash >>= 5;
                offs = (5 * offs + hash + 1) & mask;
            }
            indexes[offs] = i;
        }
    }
}

static bool resize(Dict *dict, size_t ln) {
    struct pair_s *p;
    size_t ln2 = 8, ln3;
    size_t ln1 = ln * 3 / 2;
    while (ln1 > ln2) ln2 <<= 1;
    ln3 = ln2 * ((ln2 <= (1 << (sizeof(uint8_t)*8))) ? sizeof(uint8_t) : sizeof(size_t));
    if (dict->u.val == dict->data) {
        p = (struct pair_s *)malloc(ln * sizeof *dict->data + ln3);
        if (p == NULL) return true;
        if (dict->len != 0) p[0] = dict->u.val[0];
        dict->u.s.mask = 0;
    } else {
        bool same = dict->u.s.mask == ln2 - 1;
        if (same && dict->u.s.max > ln) {
            memmove(&dict->data[ln], &dict->data[dict->u.s.max], ln3);
            dict->u.s.max = ln;
        }
        p = (struct pair_s *)realloc(dict->data, ln * sizeof *dict->data + ln3);
        if (p == NULL) return true;
        if (same) {
            if (dict->u.s.max < ln) {
                memmove(&p[ln], &p[dict->u.s.max], ln3);
                dict->u.s.max = ln;
            }
            dict->data = p;
            return false;
        }
    }
    dict->data = p;
    dict->u.s.max = ln;
    dict->u.s.mask = ln2 - 1;
    memset(&p[ln], 255, ln3);
    reindex(dict);
    return false;
}

static MUST_CHECK Obj *normalize(Dict *dict) {
    if (dict->u.val == dict->data || dict->u.s.max - dict->len < 2) return &dict->v;
    resize(dict, dict->len);
    return &dict->v;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Dict *v1 = (const Dict *)o1, *v2 = (const Dict *)o2;
    size_t n;
    if (o2->obj != DICT_OBJ || v1->len != v2->len) return false;
    if (v1->def != v2->def) {
        if (v1->def == NULL || v2->def == NULL) return false;
        if (!v1->def->obj->same(v1->def, v2->def)) return false;
    }
    for (n = 0; n < v1->len; n++) {
        const struct pair_s *p = &v1->data[n];
        const struct pair_s *p2 = dict_lookup(v2, p);
        if (p2 == NULL) return false;
        if (p->key != p2->key && !p->key->obj->same(p->key, p2->key)) return false;
        if (p->data == p2->data) continue;
        if (p->data == NULL || p2->data == NULL) return false;
        if (!p->data->obj->same(p->data, p2->data)) return false;
    }
    return true;
}

static MUST_CHECK Obj *len(oper_t op) {
    Dict *v1 = (Dict *)op->v2;
    return (Obj *)int_from_size(v1->len);
}

static FAST_CALL MUST_CHECK Obj *next(struct iter_s *v1) {
    Colonlist *iter;
    const struct pair_s *p;
    if (v1->val >= v1->len) return NULL;
    p = &((Dict *)v1->data)->data[v1->val++];
    if (p->data == NULL) {
        return p->key;
    }
    iter = (Colonlist *)v1->iter;
    if (iter->v.refcount != 1) {
        iter->v.refcount--;
        iter = new_colonlist();
        v1->iter = &iter->v;
        iter->data = iter->u.val;
        iter->len = 2;
    } else {
        val_destroy(iter->data[0]);
        val_destroy(iter->data[1]);
    }
    iter->data[0] = val_reference(p->key);
    iter->data[1] = val_reference(p->data);
    return &iter->v;
}

static void getiter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = next;
    v->len = ((Dict *)v->data)->len;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Dict *v1 = (Dict *)o1;
    const struct pair_s *p;
    size_t i = 0, j, ln = 2, chars = 2;
    Tuple *list = NULL;
    Obj **vals;
    Obj *v;
    Str *str;
    uint8_t *s;
    size_t def = (v1->def != NULL) ? 1 : 0;
    if (v1->len != 0 || def != 0) {
        ln = v1->len * 2;
        if (ln < v1->len) return NULL; /* overflow */
        ln += def;
        if (ln < def) return NULL; /* overflow */
        chars = ln + 1 + def;
        if (chars < ln) return NULL; /* overflow */
        if (chars > maxsize) return NULL;
        list = new_tuple(ln);
        vals = list->data;
        ln = chars;
        if (v1->len != 0) {
            size_t n;
            for (n = 0; n < v1->len; n++) {
                p = &v1->data[n];
                v = p->key->obj->repr(p->key, epoint, maxsize - chars);
                if (v == NULL || v->obj != STR_OBJ) goto error;
                str = (Str *)v;
                ln += str->len;
                if (ln < str->len) goto error2; /* overflow */
                chars += str->chars;
                if (chars > maxsize) goto error2;
                vals[i++] = v;
                if (p->data != NULL) {
                    v = p->data->obj->repr(p->data, epoint, maxsize - chars);
                    if (v == NULL || v->obj != STR_OBJ) goto error;
                    str = (Str *)v;
                    ln += str->len;
                    if (ln < str->len) goto error2; /* overflow */
                    chars += str->chars;
                    if (chars > maxsize) goto error2;
                } else {
                    v = (Obj *)ref_none();
                    ln--;
                    chars--;
                }
                vals[i++] = v;
            }
        }
        if (def != 0) {
            v = v1->def->obj->repr(v1->def, epoint, maxsize - chars);
            if (v == NULL || v->obj != STR_OBJ) goto error;
            str = (Str *)v;
            ln += str->len;
            if (ln < str->len) goto error2; /* overflow */
            chars += str->chars;
            if (chars > maxsize) {
            error2:
                val_destroy(v);
                v = NULL;
            error:
                list->len = i;
                val_destroy(&list->v);
                return v;
            }
            vals[i] = v;
        }
        list->len = i + def;
    }
    str = new_str2(ln);
    if (str == NULL) {
        if (list != NULL) val_destroy(&list->v);
        return NULL;
    }
    str->chars = chars;
    s = str->data;
    *s++ = '{';
    for (j = 0; j < i; j++) {
        Str *str2 = (Str *)vals[j];
        if (str2->v.obj != STR_OBJ) continue;
        if (j != 0) *s++ = ((j & 1) != 0) ? ':' : ',';
        if (str2->len != 0) {
            memcpy(s, str2->data, str2->len);
            s += str2->len;
        }
    }
    if (def != 0) {
        Str *str2 = (Str *)vals[j];
        if (j != 0) *s++ = ',';
        *s++ = ':';
        if (str2->len != 0) {
            memcpy(s, str2->data, str2->len);
            s += str2->len;
        }
    }
    *s = '}';
    if (list != NULL) val_destroy(&list->v);
    return &str->v;
}

static MUST_CHECK Obj *findit(Dict *v1, Obj *o2, linepos_t epoint) {
    if (v1->len != 0) {
        Error *err;
        const struct pair_s *p;
        struct pair_s pair;
        pair.key = o2;
        err = o2->obj->hash(o2, &pair.hash, epoint);
        if (err != NULL) return &err->v;
        p = dict_lookup(v1, &pair);
        if (p != NULL && p->data != NULL) return val_reference(p->data);
    }
    if (v1->def != NULL) {
        return val_reference(v1->def);
    }
    return (Obj *)new_error_obj(ERROR_____KEY_ERROR, o2, epoint);
}

static MUST_CHECK Obj *slice(oper_t op, size_t indx) {
    Obj *o2 = op->v2, *vv;
    Dict *v1 = (Dict *)op->v1;
    Funcargs *args = (Funcargs *)o2;
    bool more = args->len > indx + 1;
    linepos_t epoint2;

    if (args->len < 1) {
        return (Obj *)new_error_argnum(args->len, 1, 0, op->epoint2);
    }

    o2 = args->val[indx].val;
    epoint2 = &args->val[indx].epoint;

    if (o2 == &none_value->v) return val_reference(o2);
    if (o2->obj->iterable) {
        struct iter_s iter;
        size_t i;
        List *v;
        Obj **vals;
        iter.data = o2; o2->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(&null_list->v);
        }
        v = new_list();
        v->data = vals = list_create_elements(v, iter.len);
        pair_oper.epoint3 = epoint2;
        for (i = 0; i < iter.len && (o2 = iter.next(&iter)) != NULL; i++) {
            vv = findit(v1, o2, epoint2);
            if (vv->obj != ERROR_OBJ && more) {
                op->v1 = vv;
                vv = vv->obj->slice(op, indx + 1);
            }
            vals[i] = vv;
        }
        iter_destroy(&iter);
        v->len = i;
        return &v->v;
    }

    pair_oper.epoint3 = epoint2;
    vv = findit(v1, o2, epoint2);
    if (vv->obj != ERROR_OBJ && more) {
        op->v1 = vv;
        vv = vv->obj->slice(op, indx + 1);
    }
    return vv;
}

MUST_CHECK Obj *dict_sort(Dict *v1, const size_t *sort_index) {
    size_t i;
    Dict *v = new_dict(v1->len);
    if (v == NULL) err_msg_out_of_memory(); /* overflow */
    for (i = 0; i < v1->len; i++) {
        struct pair_s *d = &v->data[i];
        const struct pair_s *s = &v1->data[sort_index[i]];
        d->hash = s->hash;
        d->key = val_reference(s->key);
        d->data = s->data == NULL ? NULL : val_reference(s->data);
    }
    v->len = i;
    reindex(v);
    v->def = (v1->def == NULL) ? NULL : val_reference(v1->def);
    return &v->v;
}

static MUST_CHECK Obj *concat(oper_t op) {
    Dict *v1 = (Dict *)op->v1;
    Dict *v2 = (Dict *)op->v2;
    size_t j;
    size_t ln;
    Dict *dict;

    if (v2->len == 0 && v2->def == NULL) return val_reference(&v1->v);
    if (v1->len == 0 && (v1->def == NULL || v2->def != NULL)) return val_reference(&v2->v);

    ln = v1->len + v2->len;
    if (ln < v1->len) goto failed; /* overflow */
    if (op->inplace == &v1->v) {
        if (ln > ((v1->u.val == v1->data) ? lenof(v1->u.val) : v1->u.s.max)) {
            size_t ln2 = ln + (ln < 1024 ? ln : 1024);
            if (ln2 > ln) ln = ln2;
            if (ln > SIZE_MAX / (sizeof(struct pair_s) + sizeof(size_t) * 2)) goto failed; /* overflow */
            if (resize(v1, ln)) goto failed; /* overflow */
        }
        dict = (Dict *)val_reference(&v1->v);
    } else {
        dict = new_dict(ln);
        if (dict == NULL) goto failed; /* overflow */

        for (j = 0; j < v1->len; j++) {
            struct pair_s *d = &dict->data[j];
            const struct pair_s *s = &v1->data[j];
            d->hash = s->hash;
            d->key = val_reference(s->key);
            d->data = s->data == NULL ? NULL : val_reference(s->data);
        }
        dict->len = j;
        reindex(dict);
    }
    for (j = 0; j < v2->len; j++) {
        dict_update(dict, &v2->data[j]);
    }
    dict->def = v2->def != NULL ? val_reference(v2->def) : v1->def != NULL ? val_reference(v1->def) : NULL;
    if (dict == v1) return &dict->v;
    return normalize(dict);
failed:
    return (Obj *)new_error_mem(op->epoint3); /* overflow */
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;

    switch (o2->obj->type) {
    case T_DICT:
        if (op->op->op == O_CONCAT) {
            return concat(op);
        }
        break;
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
    Dict *v2 = (Dict *)op->v2;
    Obj *o1 = op->v1;
    if (op->op == &o_IN) {
        struct pair_s p;
        Error *err;

        if (v2->len == 0) return val_reference(&false_value->v);
        p.key = o1;
        err = o1->obj->hash(o1, &p.hash, op->epoint);
        if (err != NULL) return &err->v;
        return truth_reference(dict_lookup(v2, &p) != NULL);
    }
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        return o1->obj->calc2(op);
    default: break;
    }
    return obj_oper_error(op);
}

Obj *dictobj_parse(struct values_s *values, size_t args) {
    size_t j;
    Dict *dict = new_dict(args);
    if (dict == NULL) return (Obj *)new_error_mem(&values->epoint);
    dict->def = NULL;

    for (j = 0; j < args; j++) {
        Error *err;
        struct pair_s p;
        struct values_s *v2 = &values[j];

        p.key = v2->val;
        if (p.key == &none_value->v || p.key->obj == ERROR_OBJ) {
            val_destroy(&dict->v);
            return val_reference(p.key);
        }
        if (p.key->obj != COLONLIST_OBJ) p.data = NULL;
        else {
            Colonlist *list = (Colonlist *)p.key;
            if (list->len != 2 || list->data[1] == &default_value->v) {
                err = new_error(ERROR__NOT_KEYVALUE, &v2->epoint);
                err->u.obj = val_reference(p.key);
                val_destroy(&dict->v);
                return &err->v;
            }
            p.key = list->data[0];
            p.data = list->data[1];
        }
        if (p.key == &default_value->v) {
            if (dict->def != NULL) val_destroy(dict->def);
            dict->def = (p.data == NULL) ? NULL : val_reference(p.data);
            continue;
        }
        err = p.key->obj->hash(p.key, &p.hash, &v2->epoint);
        if (err != NULL) {
            val_destroy(&dict->v);
            return &err->v;
        }
        dict_update(dict, &p);
    }
    return normalize(dict);
}

void dictobj_init(void) {
    static struct linepos_s nopoint;

    new_type(&obj, T_DICT, "dict", sizeof(Dict));
    obj.iterable = true;
    obj.create = create;
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.len = len;
    obj.getiter = getiter;
    obj.repr = repr;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.slice = slice;

    pair_oper.op = &o_EQ;
    pair_oper.epoint = &nopoint;
    pair_oper.epoint2 = &nopoint;
    pair_oper.epoint3 = &nopoint;
}

void dictobj_names(void) {
    new_builtin("dict", val_reference(&DICT_OBJ->v));
}

