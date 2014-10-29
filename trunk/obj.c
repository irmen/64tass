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
#include "variables.h"
#include "misc.h"
#include "section.h"
#include "64tass.h"
#include "eval.h"

#include "listobj.h"
#include "boolobj.h"
#include "addressobj.h"
#include "codeobj.h"
#include "floatobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "intobj.h"
#include "functionobj.h"

int referenceit = 1;

static struct obj_s macro_obj;
static struct obj_s segment_obj;
static struct obj_s lbl_obj;
static struct obj_s mfunc_obj;
static struct obj_s struct_obj;
static struct obj_s union_obj;
static struct obj_s none_obj;
static struct obj_s error_obj;
static struct obj_s gap_obj;
static struct obj_s ident_obj;
static struct obj_s anonident_obj;
static struct obj_s oper_obj;
static struct obj_s default_obj;
static struct obj_s dict_obj;
static struct obj_s iter_obj;

obj_t MACRO_OBJ = &macro_obj;
obj_t SEGMENT_OBJ = &segment_obj;
obj_t LBL_OBJ = &lbl_obj;
obj_t MFUNC_OBJ = &mfunc_obj;
obj_t STRUCT_OBJ = &struct_obj;
obj_t UNION_OBJ = &union_obj;
obj_t NONE_OBJ = &none_obj;
obj_t ERROR_OBJ = &error_obj;
obj_t GAP_OBJ = &gap_obj;
obj_t IDENT_OBJ = &ident_obj;
obj_t ANONIDENT_OBJ = &anonident_obj;
obj_t OPER_OBJ = &oper_obj;
obj_t DEFAULT_OBJ = &default_obj;
obj_t DICT_OBJ = &dict_obj;
obj_t ITER_OBJ = &iter_obj;

static void error_copy(const value_t, value_t);

MUST_CHECK value_t obj_oper_error(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v = val_alloc();
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR__INVALID_OPER;
    v->u.error.u.invoper.op = op->op;
    v->u.error.u.invoper.v1 = v1 ? val_reference(v1) : NULL;
    v->u.error.u.invoper.v2 = v2 ? val_reference(v2) : NULL;
    v->u.error.epoint = *op->epoint3;
    return v;
}

static void invalid_destroy(value_t UNUSED(v1)) {
    return;
}

static void invalid_copy(const value_t v1, value_t v) {
    v->obj = v1->obj;
    memcpy(&v->u, &v1->u, sizeof(v->u));
}

static int invalid_same(const value_t v1, const value_t v2) {
    return v1->obj == v2->obj;
}

static MUST_CHECK value_t generic_invalid(const value_t v1, linepos_t epoint, enum errors_e num) {
    value_t v = val_alloc();
    if (v1->obj == ERROR_OBJ) {
        error_copy(v1, v);
        return v;
    }
    v->obj = ERROR_OBJ;
    v->u.error.num = num;
    v->u.error.epoint = *epoint;
    v->u.error.u.objname = v1->obj->name;
    return v;
}

static MUST_CHECK value_t invalid_truth(const value_t v1, enum truth_e UNUSED(type), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_BOOL);
}

static MUST_CHECK value_t invalid_hash(const value_t v1, int *UNUSED(hash), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR__NOT_HASHABLE);
}

static MUST_CHECK value_t invalid_repr(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v;
    uint8_t *s;
    const char *name;
    if (v1->obj == ERROR_OBJ) {
        v = val_alloc();
        error_copy(v1, v);
        return v;
    }
    name = v1->obj->name;
    v = val_alloc();
    v->obj = STR_OBJ;
    v->u.str.len = strlen(name);
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, name, v->u.str.len);
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t invalid_str(const value_t v1, linepos_t epoint) {
    return v1->obj->repr(v1, epoint);
}

static MUST_CHECK value_t gap_hash(const value_t UNUSED(v1), int *hs, linepos_t UNUSED(epoint)) {
    *hs = 0; /* whatever, there's only one */
    return NULL;
}

static MUST_CHECK value_t gap_repr(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    uint8_t *s;
    value_t v = val_alloc();
    v->obj = STR_OBJ;
    v->u.str.len = 1;
    v->u.str.chars = 1;
    s = str_create_elements(v, 1);
    *s = '?';
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t gap_calc1(oper_t op) {
    value_t v1 = op->v1;
    switch (op->op->u.oper.op) {
    case O_BANK: 
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV:
    case O_NEG:
    case O_POS:
        return val_reference(gap_value);
    case O_STRING: return gap_repr(v1, op->epoint);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t gap_calc2(oper_t op) {
    value_t v2 = op->v2;
    switch (v2->obj->type) {
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_GAP:
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_EQ: return truth_reference(v2->obj == GAP_OBJ);
        case O_NE: return truth_reference(v2->obj != GAP_OBJ);
        default: break;
        }
        break;
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t gap_rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    switch (v1->obj->type) {
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_GAP:
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_EQ: return truth_reference(v1->obj == GAP_OBJ);
        case O_NE: return truth_reference(v1->obj != GAP_OBJ);
        default: break;
        }
        break;
    case T_TUPLE:
    case T_LIST:
        return v2->obj->calc2(op);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_calc1(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc1(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return ERROR_OBJ->rcalc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_repeat(oper_t op, uval_t rep) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->repeat(op, rep);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_iindex(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->iindex(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_ival(const value_t v1, ival_t *UNUSED(iv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK value_t invalid_uval(const value_t v1, uval_t *UNUSED(uv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK value_t invalid_real(const value_t v1, double *UNUSED(r), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_REAL);
}

static MUST_CHECK value_t invalid_sign(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_SIGN);
}

static MUST_CHECK value_t invalid_abs(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_ABS);
}

static MUST_CHECK value_t invalid_integer(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK value_t invalid_len(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_LEN);
}

static MUST_CHECK value_t invalid_size(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_SIZE);
}

MUST_CHECK value_t invalid_getiter(value_t v1) {
    value_t v = val_alloc();
    v->obj = ITER_OBJ;
    v->u.iter.data = val_reference(v1);
    v->u.iter.iter = NULL;
    v->u.iter.val = 1;
    return v;
}

static MUST_CHECK value_t invalid_next(value_t v1) {
    if (v1->u.iter.val) {
        v1->u.iter.val = 0;
        return val_reference(v1->u.iter.data);
    }
    return NULL;
}

static void iter_destroy(value_t v1) {
    if (v1->u.iter.iter != &v1->u.iter.val) free(v1->u.iter.iter);
    val_destroy((value_t)v1->u.iter.data);
}

static MUST_CHECK value_t iter_next(value_t v1) {
    if (!v1->u.iter.iter) return invalid_next(v1);
    return v1->u.iter.data->obj->next(v1);
}

static void macro_destroy(value_t v1) {
    while (v1->u.macro.argc) {
        --v1->u.macro.argc;
        free((char *)v1->u.macro.param[v1->u.macro.argc].cfname.data);
        free((char *)v1->u.macro.param[v1->u.macro.argc].init.data);
    }
    free(v1->u.macro.param);
    if (v1->u.macro.label->parent == NULL) label_destroy(v1->u.macro.label);
}

static void macro_copy(const value_t v1, value_t v) {
    v->obj = v1->obj;
    memcpy(&v->u.macro, &v1->u.macro, sizeof(v->u.macro));
    if (v1->u.macro.argc) {
        size_t i;
        v->u.macro.param = (struct macro_param_s *)malloc(v1->u.macro.argc * sizeof(v->u.macro.param[0]));
        if (!v->u.macro.param) err_msg_out_of_memory();
        for (i = 0; i < v1->u.macro.argc; i++) {
            str_cpy(&v->u.macro.param[i].cfname, &v1->u.macro.param[i].cfname);
            str_cpy(&v->u.macro.param[i].init, &v1->u.macro.param[i].init);
        }
        v->u.macro.argc = i;
    } else v->u.macro.param = NULL;
}

static int macro_same(const value_t v1, const value_t v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.macro.size != v2->u.macro.size || v1->u.macro.label != v2->u.macro.label) return 0;
    for (i = 0; i < v1->u.macro.argc; i++) {
        if (str_cmp(&v1->u.macro.param[i].cfname, &v2->u.macro.param[i].cfname)) return 0;
        if (str_cmp(&v1->u.macro.param[i].init, &v2->u.macro.param[i].init)) return 0;
    }
    return 1;
}

static void mfunc_destroy(value_t v1) {
    size_t i = v1->u.mfunc.argc;
    while (i--) {
        free((char *)v1->u.mfunc.param[i].name.data);
        if (v1->u.mfunc.param[i].name.data != v1->u.mfunc.param[i].cfname.data) free((char *)v1->u.mfunc.param[i].cfname.data);
        if (v1->u.mfunc.param[i].init) val_destroy(v1->u.mfunc.param[i].init);
    }
    free(v1->u.mfunc.param);
}

static void mfunc_copy(const value_t v1, value_t v) {
    v->obj = MFUNC_OBJ;
    memcpy(&v->u.mfunc, &v1->u.mfunc, sizeof(v->u.mfunc));
    if (v1->u.mfunc.argc) {
        size_t i;
        v->u.mfunc.param = (struct mfunc_param_s *)malloc(v1->u.mfunc.argc * sizeof(v->u.mfunc.param[0]));
        if (!v->u.mfunc.param) err_msg_out_of_memory();
        for (i = 0; i < v1->u.mfunc.argc; i++) {
            str_cpy(&v->u.mfunc.param[i].name, &v1->u.mfunc.param[i].name);
            if (v1->u.mfunc.param[i].name.data != v1->u.mfunc.param[i].cfname.data) str_cpy(&v->u.mfunc.param[i].cfname, &v1->u.mfunc.param[i].cfname);
            else v->u.mfunc.param[i].cfname = v->u.mfunc.param[i].name;
            if (v1->u.mfunc.param[i].init) {
                v->u.mfunc.param[i].init = val_reference(v1->u.mfunc.param[i].init);
            } else v->u.mfunc.param[i].init = NULL;
            v->u.mfunc.param[i].epoint = v1->u.mfunc.param[i].epoint;
        }
        v->u.mfunc.argc = i;
    } else v->u.mfunc.param = NULL;
}

static int mfunc_same(const value_t v1, const value_t v2) {
    size_t i;
    if (v2->obj != MFUNC_OBJ || v1->u.mfunc.label != v2->u.mfunc.label) return 0;
    for (i = 0; i < v1->u.mfunc.argc; i++) {
        if (str_cmp(&v1->u.mfunc.param[i].name, &v2->u.mfunc.param[i].name)) return 0;
        if ((v1->u.mfunc.param[i].name.data != v1->u.mfunc.param[i].cfname.data || v2->u.mfunc.param[i].name.data != v2->u.mfunc.param[i].cfname.data) && str_cmp(&v1->u.mfunc.param[i].cfname, &v2->u.mfunc.param[i].cfname)) return 0;
        if (v1->u.mfunc.param[i].init != v2->u.mfunc.param[i].init && (!v1->u.mfunc.param[i].init || !v2->u.mfunc.param[i].init || !obj_same(v1->u.mfunc.param[i].init, v2->u.mfunc.param[i].init))) return 0;
        if (v1->u.mfunc.param[i].epoint.pos != v2->u.mfunc.param[i].epoint.pos) return 0;
    }
    return 1;
}

static void dict_free(struct avltree_node *aa)
{
    struct pair_s *a = avltree_container_of(aa, struct pair_s, node);
    val_destroy(a->key);
    val_destroy(a->data);
    free(a);
}

static void dict_destroy(value_t v1) {
    avltree_destroy(&v1->u.dict.members, dict_free);
    if (v1->u.dict.def) val_destroy(v1->u.dict.def);
}

static void dict_copy(const value_t v1, value_t v) {
    v->obj = DICT_OBJ;
    v->u.dict.len = v1->u.dict.len;
    v->u.dict.def = v1->u.dict.def ? val_reference(v1->u.dict.def) : NULL;
    avltree_init(&v->u.dict.members);
    if (v1->u.dict.len) {
        const struct avltree_node *n = avltree_first(&v1->u.dict.members);
        const struct pair_s *p;
        struct pair_s *p2;
        while (n) {
            p = cavltree_container_of(n, struct pair_s, node);
            p2 = (struct pair_s *)malloc(sizeof(struct pair_s));
            if (!p2) err_msg_out_of_memory();
            p2->hash = p->hash;
            p2->key = val_reference(p->key);
            p2->data = val_reference(p->data);
            avltree_insert(&p2->node, &v->u.dict.members, pair_compare);
            n = avltree_next(n);
        }
    }
}

static int dict_same(const value_t v1, const value_t v2) {
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

static MUST_CHECK value_t dict_len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.dict.len);
}

static MUST_CHECK value_t dict_repr(const value_t v1, linepos_t epoint) {
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
                if (v->obj != STR_OBJ) goto error;
                len += v->u.str.len;
                if (len < v->u.str.len) err_msg_out_of_memory(); /* overflow */
                tmp[i++] = v;
                v = p->data->obj->repr(p->data, epoint);
                if (v->obj != STR_OBJ) goto error;
                len += v->u.str.len;
                if (len < v->u.str.len) err_msg_out_of_memory(); /* overflow */
                tmp[i++] = v;
                n = avltree_next(n);
            }
        }
        if (def) {
            v = v1->u.dict.def->obj->repr(v1->u.dict.def, epoint);
            if (v->obj != STR_OBJ) {
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
    v = val_alloc();
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
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
    return v;
}

static MUST_CHECK value_t dict_rcalc2(oper_t op) {
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
    return op->v1->obj->calc2(op);
}

static MUST_CHECK value_t dict_iindex(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    struct pair_s pair;
    const struct avltree_node *b;
    value_t err;
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
    err = val_alloc();
    err->obj = ERROR_OBJ;
    err->u.error.num = ERROR_____KEY_ERROR;
    err->u.error.epoint = *op->epoint2;
    return err;
}

static void error_destroy(value_t v1) {
    if (v1->u.error.num == ERROR__INVALID_OPER) {
        if (v1->u.error.u.invoper.v1) val_destroy(v1->u.error.u.invoper.v1);
        if (v1->u.error.u.invoper.v2) val_destroy(v1->u.error.u.invoper.v2);
    }
}

static MUST_CHECK value_t oper_repr(const value_t v1, linepos_t UNUSED(epoint)) {
    const char *txt = v1->u.oper.name;
    size_t len = strlen(txt);
    uint8_t *s;
    value_t v = val_alloc();
    v->obj = STR_OBJ;
    s = str_create_elements(v, len + 8);
    memcpy(s, "<oper ", 6);
    memcpy(s + 6, txt, len);
    len += 6;
    s[len++] = '\'';
    s[len++] = '>';
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len;
    return v;
}

static void error_copy(const value_t v1, value_t v) {
    v->obj = ERROR_OBJ;
    memcpy(&v->u.error, &v1->u.error, sizeof(v->u.error));
    if (v1->u.error.num == ERROR__INVALID_OPER) {
        if (v1->u.error.u.invoper.v1) v->u.error.u.invoper.v1 = val_reference(v1->u.error.u.invoper.v1);
        if (v1->u.error.u.invoper.v2) v->u.error.u.invoper.v2 = val_reference(v1->u.error.u.invoper.v2);
    }
}

static MUST_CHECK value_t error_calc1(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t error_calc2(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t error_rcalc2(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK value_t error_repeat(oper_t op, uval_t UNUSED(rep)) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t error_iindex(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t ident_rcalc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t none_truth(const value_t UNUSED(v1), enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_hash(const value_t UNUSED(v1), int *UNUSED(v), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_repr(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_calc1(oper_t UNUSED(op)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return ERROR_OBJ->rcalc2(op);
    }
    return val_reference(none_value);
}

static MUST_CHECK value_t none_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return ERROR_OBJ->calc2(op);
    }
    return val_reference(none_value);
}

static MUST_CHECK value_t none_ival(const value_t UNUSED(v1), ival_t *UNUSED(iv), int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_uval(const value_t UNUSED(v1), uval_t *UNUSED(uv), int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_real(const value_t UNUSED(v1), double *UNUSED(r), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_sign(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_abs(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_integer(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_len(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_size(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static int lbl_same(const value_t v1, const value_t v2) {
    return v2->obj == LBL_OBJ && v1->u.lbl.sline == v2->u.lbl.sline && v1->u.lbl.waitforp == v2->u.lbl.waitforp && v1->u.lbl.file_list == v2->u.lbl.file_list && v1->u.lbl.parent == v2->u.lbl.parent;
}

static MUST_CHECK value_t struct_size(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.macro.size);
}

static MUST_CHECK value_t struct_calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
        struct linepos_s epoint;
        str_t name;
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.macro.label;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                return val_reference(l->value);
            }
            if (!referenceit) {
                return val_reference(none_value);
            }
            epoint = v2->u.ident.epoint;
            name = v2->u.ident.name;
            v = val_alloc();
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint;
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.ident = name;
            v->u.error.u.notdef.down = 0;
            return v;
        case T_ANONIDENT:
            {
                ssize_t count;
                l2 = v1->u.macro.label;
                l = find_anonlabel2(v2->u.anonident.count, l2);
                if (l) {
                    touch_label(l);
                    return val_reference(l->value);
                }
                if (!referenceit) {
                    return val_reference(none_value);
                }
                epoint = v2->u.anonident.epoint;
                count = v2->u.anonident.count;
                v = val_alloc();
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___NOT_DEFINED;
                v->u.error.epoint = epoint;
                v->u.error.u.notdef.label = l2;
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

void obj_init(struct obj_s *obj, enum type_e type, const char *name) {
    obj->type = type;
    obj->name = name;
    obj->destroy = invalid_destroy;
    obj->copy = invalid_copy;
    obj->same = invalid_same;
    obj->truth = invalid_truth;
    obj->hash = invalid_hash;
    obj->repr = invalid_repr;
    obj->str = invalid_str;
    obj->calc1 = invalid_calc1;
    obj->calc2 = invalid_calc2;
    obj->rcalc2 = invalid_rcalc2;
    obj->repeat = invalid_repeat;
    obj->iindex = invalid_iindex;
    obj->ival = invalid_ival;
    obj->uval = invalid_uval;
    obj->real = invalid_real;
    obj->sign = invalid_sign;
    obj->abs = invalid_abs;
    obj->integer = invalid_integer;
    obj->len = invalid_len;
    obj->size = invalid_size;
    obj->getiter = invalid_getiter;
    obj->next = invalid_next;
};

void objects_init(void) {
    boolobj_init();
    floatobj_init();
    addressobj_init();
    codeobj_init();
    strobj_init();
    listobj_init();
    bytesobj_init();
    bitsobj_init();
    intobj_init();
    functionobj_init();

    obj_init(&macro_obj, T_MACRO, "<macro>");
    macro_obj.destroy = macro_destroy;
    macro_obj.copy = macro_copy;
    macro_obj.same = macro_same;
    obj_init(&segment_obj, T_SEGMENT, "<segment>");
    segment_obj.destroy = macro_destroy;
    segment_obj.copy = macro_copy;
    segment_obj.same = macro_same;
    obj_init(&lbl_obj, T_LBL, "<lbl>");
    lbl_obj.same = lbl_same;
    obj_init(&mfunc_obj, T_MFUNC, "<function>");
    mfunc_obj.destroy = mfunc_destroy;
    mfunc_obj.copy = mfunc_copy;
    mfunc_obj.same = mfunc_same;
    obj_init(&struct_obj, T_STRUCT, "<struct>");
    struct_obj.destroy = macro_destroy;
    struct_obj.copy = macro_copy;
    struct_obj.same = macro_same;
    struct_obj.size = struct_size;
    struct_obj.calc2 = struct_calc2;
    obj_init(&union_obj, T_UNION, "<union>");
    union_obj.destroy = macro_destroy;
    union_obj.copy = macro_copy;
    union_obj.same = macro_same;
    union_obj.size = struct_size;
    union_obj.calc2 = struct_calc2;
    obj_init(&none_obj, T_NONE, "<none>");
    none_obj.truth = none_truth;
    none_obj.repr = none_repr;
    none_obj.hash = none_hash;
    none_obj.calc1 = none_calc1;
    none_obj.calc2 = none_calc2;
    none_obj.rcalc2 = none_rcalc2;
    none_obj.ival = none_ival;
    none_obj.uval = none_uval;
    none_obj.real = none_real;
    none_obj.sign = none_sign;
    none_obj.abs = none_abs;
    none_obj.integer = none_integer;
    none_obj.len = none_len;
    none_obj.size = none_size;
    obj_init(&error_obj, T_ERROR, "<error>");
    error_obj.destroy = error_destroy;
    error_obj.copy = error_copy;
    error_obj.calc1 = error_calc1;
    error_obj.calc2 = error_calc2;
    error_obj.rcalc2 = error_rcalc2;
    error_obj.repeat = error_repeat;
    error_obj.iindex = error_iindex;
    obj_init(&gap_obj, T_GAP, "<gap>");
    gap_obj.hash = gap_hash;
    gap_obj.repr = gap_repr;
    gap_obj.calc1 = gap_calc1;
    gap_obj.calc2 = gap_calc2;
    gap_obj.rcalc2 = gap_rcalc2;
    obj_init(&ident_obj, T_IDENT, "<ident>");
    ident_obj.rcalc2 = ident_rcalc2;
    obj_init(&anonident_obj, T_ANONIDENT, "<anonident>");
    anonident_obj.rcalc2 = ident_rcalc2;
    obj_init(&oper_obj, T_OPER, "<oper>");
    oper_obj.repr = oper_repr;
    obj_init(&default_obj, T_DEFAULT, "<default>");
    obj_init(&dict_obj, T_DICT, "<dict>");
    dict_obj.destroy = dict_destroy;
    dict_obj.copy = dict_copy;
    dict_obj.same = dict_same;
    dict_obj.len = dict_len;
    dict_obj.repr = dict_repr;
    dict_obj.rcalc2 = dict_rcalc2;
    dict_obj.iindex = dict_iindex;
    obj_init(&iter_obj, T_ITER, "<iter>");
    iter_obj.destroy = iter_destroy;
    iter_obj.next = iter_next;
};

