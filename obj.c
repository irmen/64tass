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

static void error_copy(const struct value_s *, struct value_s *);

void obj_oper_error(oper_t op) {
    struct value_s *e1, *e2;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    if (v == v1) {
        e1 = val_alloc();
        v->obj->copy_temp(v1, e1);
    } else e1 = v1 ? val_reference(v1) : NULL;
    if (v == v2) {
        e2 = val_alloc();
        v->obj->copy_temp(v2, e2);
    } else e2 = v2 ? val_reference(v2) : NULL;
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR__INVALID_OPER;
    v->u.error.u.invoper.op = op->op;
    v->u.error.u.invoper.v1 = e1;
    v->u.error.u.invoper.v2 = e2;
    v->u.error.epoint = op->epoint3;
}

static void invalid_destroy(struct value_s *UNUSED(v1)) {
    return;
}

static void invalid_copy(const struct value_s *v1, struct value_s *v) {
    *v = *v1;
    v->refcount = 1;
}

static int invalid_same(const struct value_s *v1, const struct value_s *v2) {
    return v1->obj == v2->obj;
}

static void generic_invalid(const struct value_s *v1, struct value_s *v, linepos_t epoint, enum errors_e num) {
    if (v1->obj == ERROR_OBJ) {
        if (v != v1) error_copy(v1, v);
        return;
    }
    if (v1 == v) v->obj->destroy(v);
    v->obj = ERROR_OBJ;
    v->u.error.num = num;
    v->u.error.epoint = *epoint;
    v->u.error.u.objname = v1->obj->name;
}

static int invalid_truth(const struct value_s *v1, struct value_s *v, enum truth_e UNUSED(type), linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR_____CANT_BOOL);
    return 1;
}

static int invalid_hash(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR__NOT_HASHABLE);
    return -1;
}

static void invalid_repr(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    uint8_t *s;
    const char *name;
    if (v1->obj == ERROR_OBJ) {
        if (v != v1) error_copy(v1, v);
        return;
    }
    name = v1->obj->name;
    if (v == v1) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.len = strlen(name);
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, name, v->u.str.len);
    v->u.str.data = s;
}

static void invalid_str(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    return v1->obj->repr(v1, v, epoint);
}

static int gap_hash(const struct value_s *UNUSED(v1), struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return 0; /* whatever, there's only one */
}

static void gap_repr(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    uint8_t *s;
    v->obj = STR_OBJ;
    v->u.str.len = 1;
    v->u.str.chars = 1;
    s = str_create_elements(v, v->u.str.len);
    *s = '?';
    v->u.str.data = s;
}

static void gap_calc1(oper_t op) {
    struct value_s *v1 = op->v1, *v = op->v;
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
        if (v != v1) v->obj = GAP_OBJ;
        return;
    case O_STRING: gap_repr(v1, v, &op->epoint); return;
    default: break;
    }
    obj_oper_error(op);
}

static void gap_calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
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
        case O_EQ: 
            if (v1 == v || v2 == v) v->obj->destroy(v);
            bool_from_int(v, v2->obj == GAP_OBJ); return;
        case O_NE: 
            if (v1 == v || v2 == v) v->obj->destroy(v);
            bool_from_int(v, v2->obj != GAP_OBJ); return;
        default: break;
        }
        break;
    case T_TUPLE:
    case T_LIST:
    case T_IDENT:
    case T_ANONIDENT:
        if (op->op != &o_MEMBER) {
            v2->obj->rcalc2(op); return;
        }
    default: break;
    }
    obj_oper_error(op);
}

static void gap_rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
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
        case O_EQ: 
            if (v1 == v || v2 == v) v->obj->destroy(v);
            bool_from_int(v, v1->obj == GAP_OBJ); return;
        case O_NE: 
            if (v1 == v || v2 == v) v->obj->destroy(v);
            bool_from_int(v, v1->obj != GAP_OBJ); return;
        default: break;
        }
        break;
    case T_TUPLE:
    case T_LIST:
        v2->obj->calc2(op); return;
    default: break;
    }
    obj_oper_error(op);
}

static void invalid_calc1(oper_t op) {
    obj_oper_error(op);
}

static void invalid_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        ERROR_OBJ->rcalc2(op);
        return;
    }
    obj_oper_error(op);
}

static void invalid_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        ERROR_OBJ->calc2(op);
        return;
    }
    obj_oper_error(op);
}

static void invalid_repeat(oper_t op, uval_t UNUSED(rep)) {
    if (op->v1->obj == ERROR_OBJ) {
        ERROR_OBJ->calc2(op);
        return;
    }
    obj_oper_error(op);
}

static void invalid_iindex(oper_t op) {
    obj_oper_error(op);
}

static int MUST_CHECK invalid_ival(const struct value_s *v1, struct value_s *v, ival_t *UNUSED(iv), int UNUSED(bits), linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR______CANT_INT);
    return 1;
}

static int MUST_CHECK invalid_uval(const struct value_s *v1, struct value_s *v, uval_t *UNUSED(uv), int UNUSED(bits), linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR______CANT_INT);
    return 1;
}

static int MUST_CHECK invalid_real(const struct value_s *v1, struct value_s *v, double *UNUSED(r), linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR_____CANT_REAL);
    return 1;
}

static void invalid_sign(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR_____CANT_SIGN);
}

static void invalid_abs(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR______CANT_ABS);
}

static void invalid_integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR______CANT_INT);
}

static void invalid_len(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR______CANT_LEN);
}

static void invalid_size(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    generic_invalid(v1, v, epoint, ERROR_____CANT_SIZE);
}

void invalid_getiter(struct value_s *v1, struct value_s *v) {
    v->obj = ITER_OBJ;
    v->u.iter.data = val_reference(v1);
    v->u.iter.iter = NULL;
    v->u.iter.val = 1;
}

static struct value_s *MUST_CHECK invalid_next(struct value_s *v1, struct value_s *UNUSED(v)) {
    if (v1->u.iter.val) {
        v1->u.iter.val = 0;
        return val_reference(v1->u.iter.data);
    }
    return NULL;
}

static void iter_destroy(struct value_s *v1) {
    if (v1->u.iter.iter != &v1->u.iter.val) free(v1->u.iter.iter);
    val_destroy((struct value_s *)v1->u.iter.data);
}

static struct value_s *MUST_CHECK iter_next(struct value_s *v1, struct value_s *v) {
    if (!v1->u.iter.iter) return invalid_next(v1, v);
    return v1->u.iter.data->obj->next(v1, v);
}

static void macro_destroy(struct value_s *v1) {
    while (v1->u.macro.argc) {
        --v1->u.macro.argc;
        free((char *)v1->u.macro.param[v1->u.macro.argc].name.data);
        free((char *)v1->u.macro.param[v1->u.macro.argc].init.data);
    }
    free(v1->u.macro.param);
}

static void macro_copy(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->refcount = 1;
    memcpy(&v->u.macro, &v1->u.macro, sizeof(v->u.macro));
    if (v1->u.macro.argc) {
        size_t i;
        v->u.macro.param = malloc(v1->u.macro.argc * sizeof(v->u.macro.param[0]));
        if (!v->u.macro.param) err_msg_out_of_memory();
        for (i = 0; i < v1->u.macro.argc; i++) {
            str_cpy(&v->u.macro.param[i].name, &v1->u.macro.param[i].name);
            str_cpy(&v->u.macro.param[i].init, &v1->u.macro.param[i].init);
        }
        v->u.macro.argc = i;
    } else v->u.macro.param = NULL;
}

static void macro_copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->refcount = 1;
    memcpy(&v->u.macro, &v1->u.macro, sizeof(v->u.macro));
}

static int macro_same(const struct value_s *v1, const struct value_s *v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.macro.p != v2->u.macro.p || v1->u.macro.size != v2->u.macro.size || v1->u.macro.parent != v2->u.macro.parent) return 0;
    for (i = 0; i < v1->u.macro.argc; i++) {
        if (str_cmp(&v1->u.macro.param[i].name, &v2->u.macro.param[i].name)) return 0;
        if (str_cmp(&v1->u.macro.param[i].init, &v2->u.macro.param[i].init)) return 0;
    }
    return 1;
}

static void mfunc_destroy(struct value_s *v1) {
    size_t i = v1->u.mfunc.argc;
    while (i--) {
        free((char *)v1->u.mfunc.param[i].name.data);
        if (v1->u.mfunc.param[i].init) val_destroy(v1->u.mfunc.param[i].init);
    }
    free(v1->u.mfunc.param);
}

static void mfunc_copy(const struct value_s *v1, struct value_s *v) {
    v->obj = MFUNC_OBJ;
    v->refcount = 1;
    memcpy(&v->u.mfunc, &v1->u.mfunc, sizeof(v->u.mfunc));
    if (v1->u.mfunc.argc) {
        size_t i;
        v->u.mfunc.param = malloc(v1->u.mfunc.argc * sizeof(v->u.mfunc.param[0]));
        if (!v->u.mfunc.param) err_msg_out_of_memory();
        for (i = 0; i < v1->u.mfunc.argc; i++) {
            str_cpy(&v->u.mfunc.param[i].name, &v1->u.mfunc.param[i].name);
            if (v1->u.mfunc.param[i].init) {
                v->u.mfunc.param[i].init = val_reference(v1->u.mfunc.param[i].init);
            } else v->u.mfunc.param[i].init = NULL;
            v->u.mfunc.param[i].epoint = v1->u.mfunc.param[i].epoint;
        }
        v->u.mfunc.argc = i;
    } else v->u.mfunc.param = NULL;
}

static void mfunc_copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = MFUNC_OBJ;
    v->refcount = 1;
    memcpy(&v->u.mfunc, &v1->u.mfunc, sizeof(v->u.mfunc));
}

static int mfunc_same(const struct value_s *v1, const struct value_s *v2) {
    size_t i;
    if (v2->obj != MFUNC_OBJ || v1->u.mfunc.p != v2->u.mfunc.p || v1->u.mfunc.label != v2->u.mfunc.label) return 0;
    for (i = 0; i < v1->u.mfunc.argc; i++) {
        if (str_cmp(&v1->u.mfunc.param[i].name, &v2->u.mfunc.param[i].name)) return 0;
        if (v1->u.mfunc.param[i].init != v2->u.mfunc.param[i].init && (!v1->u.mfunc.param[i].init || !v2->u.mfunc.param[i].init || !obj_same(v1->u.mfunc.param[i].init, v2->u.mfunc.param[i].init))) return 0;
        if (v1->u.mfunc.param[i].epoint.pos != v2->u.mfunc.param[i].epoint.pos) return 0;
        if (v1->u.mfunc.param[i].epoint.upos != v2->u.mfunc.param[i].epoint.upos) return 0;
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

static void dict_destroy(struct value_s *v1) {
    avltree_destroy(&v1->u.dict.members, dict_free);
    if (v1->u.dict.def) val_destroy(v1->u.dict.def);
}

static void dict_copy(const struct value_s *v1, struct value_s *v) {
    v->obj = DICT_OBJ;
    v->refcount = 1;
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

static void dict_copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = DICT_OBJ;
    v->refcount = 1;
    v->u.dict.len = v1->u.dict.len;
    v->u.dict.members = v1->u.dict.members;
    v->u.dict.def = v1->u.dict.def;
}

static int dict_same(const struct value_s *v1, const struct value_s *v2) {
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

static void dict_len(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t uv = v1->u.dict.len;
    if (v1 == v) dict_destroy(v);
    int_from_uval(v, uv);
}

static void dict_repr(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    const struct pair_s *p;
    size_t i, len = 2, chars = 0;
    struct value_s *tmp = NULL;
    uint8_t *s;
    unsigned int def = (v1->u.dict.def != NULL);
    if (v1->u.dict.len || def) {
        len = v1->u.dict.len * 2;
        if (len < v1->u.dict.len) err_msg_out_of_memory(); /* overflow */
        len += def;
        tmp = (struct value_s *)malloc(len * sizeof(struct value_s));
        if (!tmp || len < def || len > SIZE_MAX / sizeof(struct value_s)) err_msg_out_of_memory(); /* overflow */
        len += 1 + def;
        if (len < 1 + def) err_msg_out_of_memory(); /* overflow */
        i = 0;
        if (v1->u.dict.len) {
            const struct avltree_node *n = avltree_first(&v1->u.dict.members);
            while (n) {
                p = cavltree_container_of(n, struct pair_s, node);
                p->key->obj->repr(p->key, &tmp[i], epoint);
                if (tmp[i].obj != STR_OBJ) goto error;
                len += tmp[i].u.str.len;
                if (len < tmp[i].u.str.len) err_msg_out_of_memory(); /* overflow */
                i++;
                p->data->obj->repr(p->data, &tmp[i], epoint);
                if (tmp[i].obj != STR_OBJ) goto error;
                len += tmp[i].u.str.len;
                if (len < tmp[i].u.str.len) err_msg_out_of_memory(); /* overflow */
                i++;
                n = avltree_next(n);
            }
        }
        if (v1->u.dict.def) {
            v1->u.dict.def->obj->repr(v1->u.dict.def, &tmp[i], epoint);
            if (tmp[i].obj != STR_OBJ) {
            error:
                if (v1 == v) v->obj->destroy(v);
                tmp[i].obj->copy_temp(&tmp[i], v);
                while (i--) tmp[i].obj->destroy(&tmp[i]);
                free(tmp);
                return;
            }
            len += tmp[i].u.str.len;
            if (len < tmp[i].u.str.len) err_msg_out_of_memory(); /* overflow */
        }
    }
    s = str_create_elements(v, len);
    len = 0;
    s[len++] = '{';
    for (i = 0;i < v1->u.dict.len * 2;) {
        if (i) s[len++] = ',';
        memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
        len += tmp[i].u.str.len;
        chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        tmp[i].obj->destroy(&tmp[i]);
        i++;
        s[len++] = ':';
        memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
        len += tmp[i].u.str.len;
        chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        tmp[i].obj->destroy(&tmp[i]);
        i++;
    }
    if (def) {
        if (i) s[len++] = ',';
        s[len++] = ':';
        memcpy(s + len, tmp[i].u.str.data, tmp[i].u.str.len);
        len += tmp[i].u.str.len;
        chars += tmp[i].u.str.len - tmp[i].u.str.chars;
        tmp[i].obj->destroy(&tmp[i]);
        i++;
    }
    s[len++] = '}';
    free(tmp);
    if (v1 == v) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len - chars;
}

static void dict_rcalc2(oper_t op) {
    if (op->op == &o_IN) {
        struct pair_s p;
        struct avltree_node *b;

        p.key = op->v1;
        p.hash = obj_hash(p.key, op->v, &op->epoint);
        if (p.hash >= 0) {
            b = avltree_lookup(&p.node, &op->v2->u.dict.members, pair_compare);
            if (op->v == op->v1) obj_destroy(op->v);
            bool_from_int(op->v, b != NULL); return;
        }
        return;
    } else {
        op->v1->obj->calc2(op); return;
    }
    obj_oper_error(op);
}

static void dict_iindex(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct pair_s pair;
    const struct avltree_node *b;
    pair.key = v2;
    pair.hash = obj_hash(pair.key, v, &op->epoint2);
    if (pair.hash >= 0) {
        b = avltree_lookup(&pair.node, &v1->u.dict.members, pair_compare);
        if (b) {
            const struct pair_s *p;
            p = cavltree_container_of(b, struct pair_s, node);
            v2 = val_reference(p->data);
            if (v1 == v) dict_destroy(v);
            v2->obj->copy(v2, v);
            val_destroy(v2);
            return;
        }
        if (v1->u.dict.def) {
            v2 = val_reference(v1->u.dict.def);
            if (v1 == v) dict_destroy(v);
            v2->obj->copy(v2, v);
            val_destroy(v2);
            return;
        }
        if (v1 == v) dict_destroy(v);
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____KEY_ERROR;
        v->u.error.epoint = op->epoint2;
    }
}

static void error_destroy(struct value_s *v1) {
    if (v1->u.error.num == ERROR__INVALID_OPER) {
        if (v1->u.error.u.invoper.v1) val_destroy(v1->u.error.u.invoper.v1);
        if (v1->u.error.u.invoper.v2) val_destroy(v1->u.error.u.invoper.v2);
    }
}

static void error_copy(const struct value_s *v1, struct value_s *v) {
    v->obj = ERROR_OBJ;
    v->refcount = 1;
    memcpy(&v->u.error, &v1->u.error, sizeof(v->u.error));
    if (v1->u.error.num == ERROR__INVALID_OPER) {
        if (v1->u.error.u.invoper.v1) v->u.error.u.invoper.v1 = val_reference(v1->u.error.u.invoper.v1);
        if (v1->u.error.u.invoper.v2) v->u.error.u.invoper.v2 = val_reference(v1->u.error.u.invoper.v2);
    }
}

static void error_copy_temp(const struct value_s *v1, struct value_s *v) {
    v->obj = ERROR_OBJ;
    v->refcount = 1;
    memcpy(&v->u.error, &v1->u.error, sizeof(v->u.error));
}

static void error_calc1(oper_t op) {
    if (op->v == op->v1) return;
    error_copy(op->v1, op->v);
}

static void error_calc2(oper_t op) {
    struct value_s *v = op->v;
    if (v == op->v1) return;
    if (v == op->v2) v->obj->destroy(v);
    error_copy(op->v1, v);
}

static void error_rcalc2(oper_t op) {
    struct value_s *v = op->v;
    if (v == op->v2) return;
    if (v == op->v1) v->obj->destroy(v);
    error_copy(op->v2, v);
}

static void error_repeat(oper_t op, uval_t UNUSED(rep)) {
    struct value_s *v = op->v;
    if (v == op->v1) return;
    if (v == op->v2) v->obj->destroy(v);
    error_copy(op->v1, v);
}

static struct value_s *ident_resolv(const struct value_s *v1, struct value_s *v) {
    if (v1->obj == ANONIDENT_OBJ) {
        char idents[100];
        str_t ident;
        struct label_s *l;

        sprintf(idents, (v1->u.anonident.count >= 0) ? "+%x+%x" : "-%x-%x" , reffile, ((v1->u.anonident.count >= 0) ? forwr : backr) + v1->u.anonident.count);
        ident.data = (const uint8_t *)idents;
        ident.len = strlen(idents);
        l = find_label(&ident);
        if (l) {
            touch_label(l);
            return l->value;
        }
        v->u.error.epoint = v1->u.anonident.epoint;
        v->u.error.u.notdef.ident.len = 1;
        v->u.error.u.notdef.ident.data = (const uint8_t *)((v1->u.anonident.count >= 0) ? "+" : "-");
        v->u.error.u.notdef.down = 1;
    } else {
        int down = (v1->u.ident.name.data[0] != '_');
        struct label_s *l = down ? find_label(&v1->u.ident.name) : find_label2(&v1->u.ident.name, cheap_context);
        struct linepos_s epoint;
        if (l) {
            touch_label(l);
            l->shadowcheck = 1;
            return l->value;
        }
        epoint = v1->u.ident.epoint;
        v->u.error.u.notdef.ident = v1->u.ident.name;
        v->u.error.u.notdef.down = down;
        v->u.error.epoint = epoint;
    }
    v->u.error.u.notdef.label = v->u.error.u.notdef.down ? current_context : cheap_context;
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR___NOT_DEFINED;
    return v;
}

static void ident_calc1(oper_t op) {
    struct value_s *v, *v1 = op->v1;
    v = ident_resolv(v1, op->v);
    op->v1 = v;
    v->obj->calc1(op);
    op->v1 = v1;
}

static void ident_calc2(oper_t op) {
    struct value_s *v, *v1 = op->v1, tmp;
    v = ident_resolv(v1, &tmp);
    if (v == &tmp) {
        v = op->v;
        if (v == v1 || v == op->v2) v->obj->destroy(v);
        tmp.obj->copy_temp(&tmp, v);
        return;
    }
    op->v1 = v;
    v->obj->calc2(op);
    op->v1 = v1;
}

static void ident_rcalc2(oper_t op) {
    struct value_s *v, *v2, tmp;
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    v2 = op->v2;
    v = ident_resolv(v2, &tmp);
    if (v == &tmp) {
        v = op->v;
        if (v == op->v1 || v == v2) v->obj->destroy(v);
        tmp.obj->copy_temp(&tmp, v);
        return;
    }
    op->v2 = v;
    v->obj->rcalc2(op);
    op->v2 = v2;
}

static int ident_truth(const struct value_s *v1, struct value_s *v, enum truth_e type, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->truth(v1, v, type, epoint);
}

static int ident_hash(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->hash(v1, v, epoint);
}

static void ident_repr(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->repr(v1, v, epoint);
}

static int MUST_CHECK ident_ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->ival(v1, v, iv, bits, epoint);
}

static int MUST_CHECK ident_uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->uval(v1, v, uv, bits, epoint);
}

static void ident_str(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->str(v1, v, epoint);
}

static int MUST_CHECK ident_real(const struct value_s *v1, struct value_s *v, double *r, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->real(v1, v, r, epoint);
}

static void ident_integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->integer(v1, v, epoint);
}

static void ident_len(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->len(v1, v, epoint);
}

static void ident_size(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    v1 = ident_resolv(v1, v);
    return v1->obj->size(v1, v, epoint);
}

static void ident_repeat(oper_t op, uval_t rep) {
    struct value_s *v, *v1 = op->v1;
    v = ident_resolv(v1, op->v);
    op->v1 = v;
    v->obj->repeat(op, rep);
    op->v1 = v1;
}

static int none_truth(const struct value_s *UNUSED(v1), struct value_s *v, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
    return 1;
}

static int none_hash(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
    return -1;
}

static void none_repr(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static void none_calc1(oper_t op) {
    if (op->v != op->v1) op->v->obj = NONE_OBJ;
}

static void none_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        ERROR_OBJ->rcalc2(op);
        return;
    }
    if (op->v == op->v2) op->v->obj->destroy(op->v);
    op->v->obj = NONE_OBJ;
}

static void none_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        ERROR_OBJ->calc2(op);
        return;
    }
    if (op->v == op->v1) op->v->obj->destroy(op->v);
    op->v->obj = NONE_OBJ;
}

static int MUST_CHECK none_ival(const struct value_s *UNUSED(v1), struct value_s *v, ival_t *iv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
    *iv = 0;
    return 1;
}

static int MUST_CHECK none_uval(const struct value_s *UNUSED(v1), struct value_s *v, uval_t *uv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
    *uv = 0;
    return 1;
}

static int MUST_CHECK none_real(const struct value_s *UNUSED(v1), struct value_s *v, double *r, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
    *r = 0.0;
    return 1;
}

static void none_sign(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static void none_abs(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static void none_integer(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static void none_len(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static void none_size(const struct value_s *UNUSED(v1), struct value_s *v, linepos_t UNUSED(epoint)) {
    v->obj = NONE_OBJ;
}

static int lbl_same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == LBL_OBJ && v1->u.lbl.p == v2->u.lbl.p && v1->u.lbl.sline == v2->u.lbl.sline && v1->u.lbl.waitforp == v2->u.lbl.waitforp && v1->u.lbl.file_list == v2->u.lbl.file_list && v1->u.lbl.parent == v2->u.lbl.parent;
}

static void struct_size(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t s = v1->u.macro.size;
    if (v == v1) macro_destroy(v);
    int_from_uval(v, s);
}

static void struct_calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
        struct linepos_s epoint;
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.macro.parent;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                l->value->obj->copy(l->value, op->v);
                return;
            }
            epoint = v2->u.ident.epoint;
            v->u.error.u.notdef.ident = v2->u.ident.name;
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.down = 0;
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___NOT_DEFINED;
            v->u.error.epoint = epoint;
            return;
        case T_ANONIDENT:
            {
                char idents[100];
                str_t ident;
                sprintf(idents, (v2->u.anonident.count >= 0) ? "+%x+%x" : "-%x-%x" , reffile, ((v2->u.anonident.count >= 0) ? forwr : backr) + v2->u.anonident.count);
                ident.data = (const uint8_t *)idents;
                ident.len = strlen(idents);
                l2 = v1->u.macro.parent;
                l = find_label2(&ident, l2);
                if (l) {
                    touch_label(l);
                    l->value->obj->copy(l->value, op->v);
                    return;
                }
                v->u.error.epoint = v2->u.anonident.epoint;
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR___NOT_DEFINED;
                v->u.error.u.notdef.ident.len = 1;
                v->u.error.u.notdef.ident.data = (const uint8_t *)((v2->u.anonident.count >= 0) ? "+" : "-");
                v->u.error.u.notdef.label = l2;
                v->u.error.u.notdef.down = 0;
                return;
            }
        case T_TUPLE:
        case T_LIST: v2->obj->rcalc2(op); return;
        default: v2->obj->rcalc2(op); return;
        }
    }
    obj_oper_error(op);
}

void obj_init(struct obj_s *obj, enum type_e type, const char *name) {
    obj->type = type;
    obj->name = name;
    obj->destroy = invalid_destroy;
    obj->copy = invalid_copy;
    obj->copy_temp = invalid_copy;
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
    macro_obj.copy_temp = macro_copy_temp;
    macro_obj.same = macro_same;
    obj_init(&segment_obj, T_SEGMENT, "<segment>");
    segment_obj.destroy = macro_destroy;
    segment_obj.copy = macro_copy;
    segment_obj.copy_temp = macro_copy_temp;
    segment_obj.same = macro_same;
    obj_init(&lbl_obj, T_LBL, "<lbl>");
    lbl_obj.same = lbl_same;
    obj_init(&mfunc_obj, T_MFUNC, "<function>");
    mfunc_obj.destroy = mfunc_destroy;
    mfunc_obj.copy = mfunc_copy;
    mfunc_obj.copy_temp = mfunc_copy_temp;
    mfunc_obj.same = mfunc_same;
    obj_init(&struct_obj, T_STRUCT, "<struct>");
    struct_obj.destroy = macro_destroy;
    struct_obj.copy = macro_copy;
    struct_obj.copy_temp = macro_copy_temp;
    struct_obj.same = macro_same;
    struct_obj.size = struct_size;
    struct_obj.calc2 = struct_calc2;
    obj_init(&union_obj, T_UNION, "<union>");
    union_obj.destroy = macro_destroy;
    union_obj.copy = macro_copy;
    union_obj.copy_temp = macro_copy_temp;
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
    error_obj.copy_temp = error_copy_temp;
    error_obj.calc1 = error_calc1;
    error_obj.calc2 = error_calc2;
    error_obj.rcalc2 = error_rcalc2;
    error_obj.repeat = error_repeat;
    obj_init(&gap_obj, T_GAP, "<gap>");
    gap_obj.hash = gap_hash;
    gap_obj.repr = gap_repr;
    gap_obj.calc1 = gap_calc1;
    gap_obj.calc2 = gap_calc2;
    gap_obj.rcalc2 = gap_rcalc2;
    obj_init(&ident_obj, T_IDENT, "<ident>");
    ident_obj.truth = ident_truth;
    ident_obj.hash = ident_hash;
    ident_obj.repr = ident_repr;
    ident_obj.str = ident_str;
    ident_obj.ival = ident_ival;
    ident_obj.uval = ident_uval;
    ident_obj.real = ident_real;
    ident_obj.integer = ident_integer;
    ident_obj.len = ident_len;
    ident_obj.size = ident_size;
    ident_obj.calc1 = ident_calc1;
    ident_obj.calc2 = ident_calc2;
    ident_obj.rcalc2 = ident_rcalc2;
    ident_obj.repeat = ident_repeat;
    obj_init(&anonident_obj, T_ANONIDENT, "<anonident>");
    anonident_obj.truth = ident_truth;
    anonident_obj.hash = ident_hash;
    anonident_obj.repr = ident_repr;
    anonident_obj.str = ident_str;
    anonident_obj.ival = ident_ival;
    anonident_obj.uval = ident_uval;
    anonident_obj.real = ident_real;
    anonident_obj.integer = ident_integer;
    anonident_obj.len = ident_len;
    anonident_obj.size = ident_size;
    anonident_obj.calc1 = ident_calc1;
    anonident_obj.calc2 = ident_calc2;
    anonident_obj.rcalc2 = ident_rcalc2;
    anonident_obj.repeat = ident_repeat;
    obj_init(&oper_obj, T_OPER, "<oper>");
    obj_init(&default_obj, T_DEFAULT, "<default>");
    obj_init(&dict_obj, T_DICT, "<dict>");
    dict_obj.destroy = dict_destroy;
    dict_obj.copy = dict_copy;
    dict_obj.copy_temp = dict_copy_temp;
    dict_obj.same = dict_same;
    dict_obj.len = dict_len;
    dict_obj.repr = dict_repr;
    dict_obj.rcalc2 = dict_rcalc2;
    dict_obj.iindex = dict_iindex;
    obj_init(&iter_obj, T_ITER, "<iter>");
    iter_obj.destroy = iter_destroy;
    iter_obj.next = iter_next;
};

