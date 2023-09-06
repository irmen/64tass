/*
    $Id: dictobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "symbolobj.h"

static Type obj;

Type *const DICT_OBJ = &obj;

static Dict null_dictval = { { &obj, 1 }, 0, null_dictval.u.val, { { { 0, NULL, NULL } } }, NULL };

Obj *const null_dict = &null_dictval.v;

static Dict *new_dict(size_t ln) {
    size_t ln1, ln2, ln3;
    Dict *v;
    struct pair_s *p;
    if (ln > lenof(v->u.val)) {
        if (ln > SIZE_MAX / (sizeof(struct pair_s) + sizeof(size_t) * 2)) return NULL; /* overflow */
        ln1 = ln * 3 / 2;
        ln2 = 8; while (ln1 > ln2) ln2 <<= 1;
        ln3 = ln2 * ((ln2 <= (1 << (sizeof(uint8_t)*8))) ? sizeof(uint8_t) : sizeof(size_t));
        ln1 = ln * sizeof(struct pair_s) + ln3;
        p = (struct pair_s *)allocate_array(uint8_t, ln1);
        if (p == NULL) return NULL; /* out of memory */
        memset(&p[ln], 255, ln3);
    } else {
        p = NULL;
        ln2 = 1;
    }
    v = Dict(val_alloc(DICT_OBJ));
    if (p == NULL) {
        v->data = v->u.val;
    } else {
        v->data = p;
        v->u.s.hash = -1;
        v->u.s.max = ln;
        v->u.s.mask = ln2 - 1;
    }
    v->len = 0;
    return v;
}

static FAST_CALL void destroy(Obj *o1) {
    Dict *v1 = Dict(o1);
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
    Dict *v1 = Dict(o1);
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

static bool pair_same(const struct pair_s *a, const struct pair_s *b)
{
    if (a->hash != b->hash || a->key->obj != b->key->obj) return false;
    if (a->key->obj->type == T_SYMBOL) return symbol_cfsame(Symbol(a->key), Symbol(b->key));
    return a->key->obj->same(a->key, b->key);
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
            if (p->key == d->key || pair_same(p, d)) {
                if (d->data != NULL) val_destroy(d->data);
                d->data = (p->data == NULL) ? NULL : val_reference(p->data);
                return;
            }
            hash >>= 5;
            offs = (5 * offs + hash + 1) & mask;
        }
        indexes[offs] = (uint8_t)dict->len;
    } else {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        size_t *indexes = (size_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != SIZE_MAX) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_same(p, d)) {
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
        if (p->key == d->key || pair_same(p, d)) return d;
    } else if (dict->u.s.mask < (1 << (sizeof(uint8_t)*8))) {
        size_t mask = dict->u.s.mask;
        size_t hash = (size_t)p->hash;
        size_t offs = hash & mask;
        uint8_t *indexes = (uint8_t *)&dict->data[dict->u.s.max];
        while (indexes[offs] != (uint8_t)~0) {
            d = &dict->data[indexes[offs]];
            if (p->key == d->key || pair_same(p, d)) return d;
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
            if (p->key == d->key || pair_same(p, d)) return d;
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
            indexes[offs] = (uint8_t)i;
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
        ln1 = ln * sizeof *dict->data + ln3;
        p = (struct pair_s *)allocate_array(uint8_t, ln1);
        if (p == NULL) return true;
        if (dict->len != 0) p[0] = dict->u.val[0];
        dict->u.s.hash = -1;
        dict->u.s.mask = 0;
    } else {
        bool same = dict->u.s.mask == ln2 - 1;
        if (same && dict->u.s.max > ln) {
            memmove(&dict->data[ln], &dict->data[dict->u.s.max], ln3);
            dict->u.s.max = ln;
        }
        ln1 = ln * sizeof *dict->data + ln3;
        p = (struct pair_s *)reallocate_array((uint8_t *)dict->data, ln1);
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
    if (dict->u.val == dict->data || dict->u.s.max - dict->len < 2) return Obj(dict);
    resize(dict, dict->len);
    return Obj(dict);
}

static MUST_CHECK Obj *dict_from_iterable(Obj *v1, linepos_t epoint) {
    Obj *v;
    struct iter_s iter;
    iter.data = v1; v1->obj->getiter(&iter);

    if (iter.len == 0) {
        v = val_reference(null_dict);
    } else {
        Dict *dict = new_dict(iter.len);
        if (dict == NULL) {
            v = new_error_mem(epoint);
        } else {
            struct pair_s p;
            size_t i;
            dict->def = NULL;
            for (i = 0; i < iter.len && (p.key = iter.next(&iter)) != NULL; i++) {
                Obj *err;

                if (p.key == none_value || p.key->obj == ERROR_OBJ) {
                    v = val_reference(p.key);
                error:
                    val_destroy(Obj(dict));
                    iter_destroy(&iter);
                    return v;
                }
                if (p.key->obj != COLONLIST_OBJ) p.data = NULL;
                else {
                    Colonlist *list = Colonlist(p.key);
                    if (list->len != 2 || (list->data[0] != default_value && list->data[1] == default_value)) {
                        v = new_error_obj(ERROR__NOT_KEYVALUE, p.key, epoint);
                        goto error;
                    }
                    p.key = list->data[0];
                    p.data = list->data[1];
                }
                if (p.key == default_value) {
                    if (dict->def != NULL) val_destroy(dict->def);
                    dict->def = (p.data == NULL || p.data == default_value) ? NULL : val_reference(p.data);
                    continue;
                }
                err = p.key->obj->hash(p.key, &p.hash, epoint);
                if (err != NULL) {
                    v = err;
                    goto error;
                }
                dict_update(dict, &p);
            }
            v = normalize(dict);
        }
    }
    iter_destroy(&iter);
    return v;
}

static MUST_CHECK Obj *dict_from_obj(Obj *o1, linepos_t epoint) {
    switch (o1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_DICT:
        return val_reference(o1);
    default:
        if (o1->obj->iterable) {
            return dict_from_iterable(o1, epoint);
        }
        break;
    }
    return new_error_conv(o1, DICT_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return dict_from_obj(op->v2, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Dict *v1 = Dict(o1), *v2 = Dict(o2);
    size_t n;
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    if (v1->def != v2->def) {
        if (v1->def == NULL || v2->def == NULL) return false;
        if (!v1->def->obj->same(v1->def, v2->def)) return false;
    }
    for (n = 0; n < v1->len; n++) {
        const struct pair_s *p = &v1->data[n];
        const struct pair_s *p2 = &v2->data[n];
        if (p->key != p2->key && !p->key->obj->same(p->key, p2->key)) return false;
        if (p->data == p2->data) continue;
        if (p->data == NULL || p2->data == NULL) return false;
        if (!p->data->obj->same(p->data, p2->data)) return false;
    }
    return true;
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t epoint) {
    Dict *v1 = Dict(o1);
    size_t i, l = v1->len;
    struct pair_s *vals = v1->data;
    unsigned int h;
    if (vals != v1->u.val && v1->u.s.hash >= 0) {
        *hs = v1->u.s.hash;
        return NULL;
    }
    h = 0;
    for (i = 0; i < l; i++) {
        Obj *o2 = vals[i].data;
        if (o2 != NULL) {
            int h2;
            Obj *err = o2->obj->hash(o2, &h2, epoint);
            if (err != NULL) return err;
            h += (unsigned int)h2;
        }
        h += (unsigned int)vals[i].hash;
    }
    if (v1->def != NULL) {
        Obj *o2 = v1->def;
        int h2;
        Obj *err = o2->obj->hash(o2, &h2, epoint);
        if (err != NULL) return err;
        h += (unsigned int)h2;
    }
    h ^= (unsigned int)i;
    h &= ((~0U) >> 1);
    if (vals != v1->u.val) v1->u.s.hash = (int)h;
    *hs = (int)h;
    return NULL;
}

static MUST_CHECK Obj *len(oper_t op) {
    Dict *v1 = Dict(op->v2);
    return int_from_size(v1->len);
}

static FAST_CALL MUST_CHECK Obj *iter_element(struct iter_s *v1, size_t i) {
    Colonlist *iter;
    const struct pair_s *p = &Dict(v1->data)->data[i];
    if (p->data == NULL) {
        return p->key;
    }
    iter = Colonlist(v1->iter);
    if (iter->v.refcount != 1) {
        iter->v.refcount--;
        iter = new_colonlist();
        v1->iter = Obj(iter);
        iter->data = iter->u.val;
        iter->len = 2;
    } else {
        val_destroy(iter->data[0]);
        val_destroy(iter->data[1]);
    }
    iter->data[0] = val_reference(p->key);
    iter->data[1] = val_reference(p->data);
    return Obj(iter);
}

static FAST_CALL MUST_CHECK Obj *iter_forward(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return iter_element(v1, v1->val++);
}

static void getiter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_forward;
    v->len = Dict(v->data)->len;
}

static FAST_CALL MUST_CHECK Obj *iter_reverse(struct iter_s *v1) {
    if (v1->val >= v1->len) return NULL;
    return iter_element(v1, v1->len - ++v1->val);
}

static void getriter(struct iter_s *v) {
    v->iter = val_reference(v->data);
    v->val = 0;
    v->data = val_reference(v->data);
    v->next = iter_reverse;
    v->len = Dict(v->data)->len;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    Dict *v1 = Dict(o1);
    const struct pair_s *p;
    size_t i = 0, j, ln = 2, chars = 2;
    Tuple *list = NULL;
    Obj **vals;
    Obj *v;
    Str *str;
    uint8_t *s;
    size_t def = (v1->def != NULL) ? 1 : 0;
    if (v1->len != 0 || def != 0) {
        if (add_overflow(v1->len, v1->len, &ln)) return NULL;
        if (inc_overflow(&ln, def)) return NULL;
        if (add_overflow(1 + def, ln, &chars)) return NULL;
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
                str = Str(v);
                if (inc_overflow(&ln, str->len)) goto error2;
                chars += str->chars;
                if (chars > maxsize) goto error2;
                vals[i++] = v;
                if (p->data != NULL) {
                    v = p->data->obj->repr(p->data, epoint, maxsize - chars);
                    if (v == NULL || v->obj != STR_OBJ) goto error;
                    str = Str(v);
                    if (inc_overflow(&ln, str->len)) goto error2;
                    chars += str->chars;
                    if (chars > maxsize) goto error2;
                } else {
                    v = ref_none();
                    ln--;
                    chars--;
                }
                vals[i++] = v;
            }
        }
        if (def != 0) {
            v = v1->def->obj->repr(v1->def, epoint, maxsize - chars);
            if (v == NULL || v->obj != STR_OBJ) goto error;
            str = Str(v);
            if (inc_overflow(&ln, str->len)) goto error2;
            chars += str->chars;
            if (chars > maxsize) {
            error2:
                val_destroy(v);
                v = NULL;
            error:
                list->len = i;
                val_destroy(Obj(list));
                return v;
            }
            vals[i] = v;
        }
        list->len = i + def;
    }
    str = new_str2(ln);
    if (str == NULL) {
        if (list != NULL) val_destroy(Obj(list));
        return NULL;
    }
    str->chars = chars;
    s = str->data;
    *s++ = '{';
    for (j = 0; j < i; j++) {
        Str *str2 = Str(vals[j]);
        if (str2->v.obj != STR_OBJ) continue;
        if (j != 0) *s++ = ((j & 1) != 0) ? ':' : ',';
        if (str2->len != 0) {
            memcpy(s, str2->data, str2->len);
            s += str2->len;
        }
    }
    if (def != 0) {
        Str *str2 = Str(vals[j]);
        if (j != 0) *s++ = ',';
        *s++ = ':';
        if (str2->len != 0) {
            memcpy(s, str2->data, str2->len);
            s += str2->len;
        }
    }
    *s = '}';
    if (list != NULL) val_destroy(Obj(list));
    return Obj(str);
}

static MUST_CHECK Obj *findit(const Dict *v1, Obj *o2, linepos_t epoint) {
    if (v1->len != 0) {
        Obj *err;
        const struct pair_s *p;
        struct pair_s pair;
        pair.key = o2;
        err = o2->obj->hash(o2, &pair.hash, epoint);
        if (err != NULL) return err;
        p = dict_lookup(v1, &pair);
        if (p != NULL) return val_reference(p->data != NULL ? p->data : p->key);
    }
    if (v1->def != NULL) {
        return val_reference(v1->def);
    }
    return new_error_obj(ERROR_____KEY_ERROR, o2, epoint);
}

static MUST_CHECK Obj *indexof(const Dict *v1, Obj *o2, ival_t *r, linepos_t epoint) {
    if (v1->len != 0) {
        Obj *err;
        const struct pair_s *p;
        struct pair_s pair;
        pair.key = o2;
        err = o2->obj->hash(o2, &pair.hash, epoint);
        if (err != NULL) return err;
        p = dict_lookup(v1, &pair);
        if (p != NULL) {
            size_t i = (size_t)(p - v1->data);
            if (i <= ~(uval_t)0 >> 1) {
                *r = (ival_t)i;
                return NULL;
            }
        }
    }
    return new_error_obj(ERROR_____KEY_ERROR, o2, epoint);
}

static MUST_CHECK Obj *dictsliceparams(const Dict *v1, const Colonlist *v2, struct sliceparam_s *s, linepos_t epoint) {
    Obj *err;
    ival_t length, offs, end, step = 1;

    s->length = 0;
    if (v1->len >= (1U << (8 * sizeof(ival_t) - 1))) return new_error_mem(epoint); /* overflow */
    length = (ival_t)v1->len;
    if (v2->len > 3 || v2->len < 1) {
        return new_error_argnum(v2->len <= ~(argcount_t)0 ? (argcount_t)v2->len : ~(argcount_t)0, 1, 3, epoint);
    }
    end = length;
    if (v2->len > 2) {
        if (v2->data[2] != default_value) {
            err = Obj(v2->data[2]->obj->ival(v2->data[2], &step, 8 * sizeof step, epoint));
            if (err != NULL) return err;
            if (step == 0) {
                return Obj(new_error(ERROR_NO_ZERO_VALUE, epoint));
            }
        }
    }
    if (v2->len > 1) {
        if (v2->data[1] == default_value) end = (step > 0) ? length : -1;
        else {
            err = indexof(v1, v2->data[1], &end, epoint);
            if (err != NULL) return err;
        }
    } else end = length;
    if (v2->data[0] == default_value) offs = (step > 0) ? 0 : length - 1;
    else {
        err = indexof(v1, v2->data[0], &offs, epoint);
        if (err != NULL) return err;
    }

    if (step > 0) {
        if (offs > end) offs = end;
        s->length = (uval_t)(end - offs + step - 1) / (uval_t)step;
    } else {
        if (end > offs) end = offs;
        s->length = (uval_t)(offs - end - step - 1) / -(uval_t)step;
    }

    s->offset = offs;
    s->end = end;
    s->step = step;
    return NULL;
}

static MUST_CHECK Obj *slice(oper_t op, argcount_t indx) {
    Obj *o2 = op->v2, *vv;
    Dict *v1 = Dict(op->v1);
    Funcargs *args = Funcargs(o2);
    bool more;
    linepos_t epoint2;

    if (args->len < 1) {
        return new_error_argnum(args->len, 1, 0, op->epoint2);
    }
    more =  args->len - 1> indx;
    o2 = args->val[indx].val;
    epoint2 = &args->val[indx].epoint;

    if (o2 == none_value) return val_reference(o2);
    if (o2->obj->iterable) {
        struct iter_s iter;
        size_t i;
        List *v;
        Obj **vals;
        iter.data = o2; o2->obj->getiter(&iter);

        if (iter.len == 0) {
            iter_destroy(&iter);
            return val_reference(null_list);
        }
        v = new_list();
        v->data = vals = list_create_elements(v, iter.len);
        for (i = 0; i < iter.len && (o2 = iter.next(&iter)) != NULL; i++) {
            vv = findit(v1, o2, epoint2);
            if (vv->obj != ERROR_OBJ && more) {
                Obj *result;
                op->v1 = vv;
                result = vv->obj->slice(op, indx + 1);
                val_destroy(vv);
                vv = result;
            }
            vals[i] = vv;
        }
        v->len = i;
        iter_destroy(&iter);
        return Obj(v);
    }
    if (o2->obj == COLONLIST_OBJ) {
        struct sliceparam_s s;
        uval_t i;
        Dict *v;
        Obj *err = dictsliceparams(v1, Colonlist(o2), &s, epoint2);
        if (err != NULL) return err;

        if (s.length == 0) {
            return val_reference(null_dict);
        }

        if (s.step == 1 && s.length == v1->len && v1->def == NULL && !more) {
            return val_reference(Obj(v1)); /* original dict */
        }
        v = new_dict(s.length);
        if (v == NULL) return new_error_mem(epoint2); /* overflow */
        v->def = NULL;
        for (i = 0; i < s.length; i++) {
            struct pair_s *p = &v->data[i];
            if (v1->data[s.offset].data == NULL) {
                if (more) {
                    v->len = i;
                    val_destroy(Obj(v));
                    return new_error_obj(ERROR_____KEY_ERROR, v1->data[s.offset].key, epoint2);
                }
                p->data = NULL;
            } else {
                if (more) {
                    op->v1 = v1->data[s.offset].data;
                    p->data = op->v1->obj->slice(op, indx + 1);
                } else {
                    p->data = val_reference(v1->data[s.offset].data);
                }
            }
            p->hash = v1->data[s.offset].hash;
            p->key = val_reference(v1->data[s.offset].key);
            s.offset += s.step;
        }
        v->len = i;
        reindex(v);
        return Obj(v);
    }

    vv = findit(v1, o2, epoint2);
    if (vv->obj != ERROR_OBJ && more) {
        Obj *result;
        op->v1 = vv;
        result = vv->obj->slice(op, indx + 1);
        val_destroy(vv);
        return result;
    }
    return vv;
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1;
    Dict *v2 = Dict(op->v2);
    struct pair_s p;
    Obj *err;
    bool result;

    if (v2->len == 0) return truth_reference(op->op != O_IN);
    if (o1 == none_value || o1->obj == ERROR_OBJ) return val_reference(o1);
    p.key = o1;
    err = o1->obj->hash(o1, &p.hash, op->epoint);
    if (err != NULL) return err;
    result = dict_lookup(v2, &p) != NULL;
    return truth_reference(op->op == O_IN ? result : !result);
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
    return Obj(v);
}

static MUST_CHECK Obj *concat(oper_t op) {
    Dict *v1 = Dict(op->v1);
    Dict *v2 = Dict(op->v2);
    size_t j;
    size_t ln;
    Dict *dict;

    if (v2->len == 0 && v2->def == NULL) return val_reference(Obj(v1));
    if (v1->len == 0 && (v1->def == NULL || v2->def != NULL)) return val_reference(Obj(v2));

    if (add_overflow(v1->len, v2->len, &ln)) goto failed;
    if (op->inplace == Obj(v1)) {
        if (ln > ((v1->u.val == v1->data) ? lenof(v1->u.val) : v1->u.s.max)) {
            size_t ln2 = ln + (ln < 1024 ? ln : 1024);
            if (ln2 > ln) ln = ln2;
            if (ln > SIZE_MAX / (sizeof(struct pair_s) + sizeof(size_t) * 2)) goto failed; /* overflow */
            if (resize(v1, ln)) goto failed; /* overflow */
        }
        dict = Dict(val_reference(Obj(v1)));
        if (dict->data != dict->u.val) dict->u.s.hash = -1;
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
    if (op->inplace == Obj(v1)) {
        if (v2->def != NULL) {
            if (dict->def != NULL) val_destroy(dict->def);
            dict->def = val_reference(v2->def);
        }
    } else {
        dict->def = v2->def != NULL ? val_reference(v2->def) : v1->def != NULL ? val_reference(v1->def) : NULL;
    }
    if (dict == v1) return Obj(dict);
    return normalize(dict);
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;

    switch (o2->obj->type) {
    case T_DICT:
        if (op->op == O_CONCAT) {
            return concat(op);
        }
        break;
    case T_ANONSYMBOL:
    case T_SYMBOL:
        if (op->op == O_MEMBER) {
            return findit(Dict(op->v1), o2, op->epoint2);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *o1 = op->v1;
    switch (o1->obj->type) {
    default:
        if (!o1->obj->iterable) {
            break;
        }
        FALL_THROUGH; /* fall through */
    case T_NONE:
    case T_ERROR:
        return o1->obj->calc2(op);
    case T_DICT:
        break;
    }
    return obj_oper_error(op);
}

Obj *dictobj_parse(struct values_s *values, size_t args) {
    size_t j;
    Dict *dict = new_dict(args);
    if (dict == NULL) return new_error_mem(&values->epoint);
    dict->def = NULL;

    for (j = 0; j < args; j++) {
        Obj *err;
        struct pair_s p;
        struct values_s *v2 = &values[j];

        p.key = v2->val;
        if (p.key == none_value || p.key->obj == ERROR_OBJ) {
            val_destroy(Obj(dict));
            return val_reference(p.key);
        }
        if (p.key->obj != COLONLIST_OBJ) p.data = NULL;
        else {
            Colonlist *list = Colonlist(p.key);
            if (list->len != 2 || (list->data[0] != default_value && list->data[1] == default_value)) {
                err = new_error_obj(ERROR__NOT_KEYVALUE, p.key, &v2->epoint);
                val_destroy(Obj(dict));
                return err;
            }
            p.key = list->data[0];
            p.data = list->data[1];
        }
        if (p.key == default_value) {
            if (dict->def != NULL) val_destroy(dict->def);
            dict->def = (p.data == NULL || p.data == default_value) ? NULL : val_reference(p.data);
            continue;
        }
        err = p.key->obj->hash(p.key, &p.hash, &v2->epoint);
        if (err != NULL) {
            val_destroy(Obj(dict));
            return err;
        }
        dict_update(dict, &p);
    }
    return normalize(dict);
}

void dictobj_init(void) {
    Type *type = new_type(&obj, T_DICT, "dict", sizeof(Dict));
    type->iterable = true;
    type->destroy = destroy;
    type->garbage = garbage;
    type->convert = convert;
    type->same = same;
    type->hash = hash;
    type->len = len;
    type->getiter = getiter;
    type->getriter = getriter;
    type->repr = repr;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    type->slice = slice;
    type->contains = contains;
}

void dictobj_names(void) {
    new_builtin("dict", val_reference(Obj(DICT_OBJ)));
}

void dictobj_destroy(void) {
#ifdef DEBUG
    if (null_dict->refcount != 1) fprintf(stderr, "dict %" PRIuSIZE "\n", null_dict->refcount - 1);
#endif
}
