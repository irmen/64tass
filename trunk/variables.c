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
#include <errno.h>
#include "unicode.h"
#include "variables.h"
#include "misc.h"
#include "values.h"
#include "64tass.h"
#include "file.h"
#include "obj.h"
#include "boolobj.h"
#include "floatobj.h"

#define EQUAL_COLUMN 16

struct label_s *root_label;
struct label_s builtin_label;
struct label_s *current_context;
struct label_s *cheap_context;

static union label_u {
    struct label_s val;
    union label_u *next;
} *labels_free = NULL;

static struct labels_s {
    union label_u vals[255];
    struct labels_s *next;
} *labels = NULL;

static inline void var_free(union label_u *val) {
#ifdef DEBUG
    return free(val);
#else
    val->next = labels_free;
    labels_free = val;
#endif
}

static struct label_s *var_alloc(void) {
#ifdef DEBUG
    return (struct label_s *)malloc(sizeof(struct label_s));
#else
    struct label_s *val;
    size_t i;
    val = (struct label_s *)labels_free;
    labels_free = labels_free->next;
    if (!labels_free) {
        struct labels_s *old = labels;
        labels = (struct labels_s *)malloc(sizeof(struct labels_s));
        if (!labels) err_msg_out_of_memory();
        for (i = 0; i < 254; i++) {
            labels->vals[i].next = &labels->vals[i+1];
        }
        labels->vals[i].next = NULL;
        labels->next = old;
        labels_free = &labels->vals[0];
    }
    return val;
#endif
}

/* --------------------------------------------------------------------------- */
static void label_free(struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    free((char *)a->name.data);
    if (a->name.data != a->cfname.data) free((uint8_t *)a->cfname.data);
    avltree_destroy(&a->members, label_free);
    val_destroy(a->value);
    var_free((union label_u *)a);
}

static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct label_s *a = cavltree_container_of(aa, struct label_s, node);
    const struct label_s *b = cavltree_container_of(bb, struct label_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    return str_cmp(&a->cfname, &b->cfname);
}

static int label_compare2(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct label_s *a = cavltree_container_of(aa, struct label_s, node);
    const struct label_s *b = cavltree_container_of(bb, struct label_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    h = str_cmp(&a->cfname, &b->cfname);
    if (h) return h;
    return b->strength - a->strength;
}

static struct label_s *strongest_label(struct avltree_node *b) {
    struct label_s *a = NULL, *c; 
    struct avltree_node *n = b;

    do {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass || (c->constant && (!fixeddig || c->defpass == pass - 1))) a = c;
        n = avltree_next(n);
    } while (n && !label_compare(n, b));
    if (a) return a;
    n = avltree_prev(b);
    while (n && !label_compare(n, b)) {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass || (c->constant && (!fixeddig || c->defpass == pass - 1))) a = c;
        n = avltree_prev(n);
    }
    return a;
}

struct label_s *find_label(const str_t *name) {
    struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp, *c;

    if (name->len > 1 && !name->data[1]) tmp.cfname = *name;
    else str_cfcpy(&tmp.cfname, name);
    tmp.name_hash = str_hash(&tmp.cfname);

    while (context->parent) {
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) {
                return c;
            }
        }
        context = context->parent;
    }
    b = avltree_lookup(&tmp.node, &builtin_label.members, label_compare);
    if (b) return avltree_container_of(b, struct label_s, node);
    return NULL;
}

struct label_s *find_label2(const str_t *name, const struct label_s *context) {
    struct avltree_node *b;
    struct label_s tmp;

    if (name->len > 1 && !name->data[1]) tmp.cfname = *name;
    else str_cfcpy(&tmp.cfname, name);
    tmp.name_hash = str_hash(&tmp.cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
    if (!b) return NULL;
    return strongest_label(b);
}

static struct {
    uint8_t dir;
    uint8_t padding;
    uint16_t reffile;
    int32_t count;
} anon_idents;

struct label_s *find_label3(const str_t *name, const struct label_s *context, uint8_t strength) {
    struct avltree_node *b;
    struct label_s tmp;

    if (name->len > 1 && !name->data[1]) tmp.cfname = *name;
    else str_cfcpy(&tmp.cfname, name);
    tmp.name_hash = str_hash(&tmp.cfname);
    tmp.strength = strength;

    b = avltree_lookup(&tmp.node, &context->members, label_compare2);
    if (!b) return NULL;
    return avltree_container_of(b, struct label_s, node);
}

struct label_s *find_anonlabel(int32_t count) {
    struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp, *c;

    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    tmp.cfname.data = (const uint8_t *)&anon_idents;
    tmp.cfname.len = sizeof(anon_idents);
    tmp.name_hash = str_hash(&tmp.cfname);

    while (context->parent) {
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) return c;
        }
        context = context->parent;
    }
    b = avltree_lookup(&tmp.node, &builtin_label.members, label_compare);
    if (b) return avltree_container_of(b, struct label_s, node);
    return NULL;
}

struct label_s *find_anonlabel2(int32_t count, const struct label_s *context) {
    struct avltree_node *b;
    struct label_s tmp;
    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    tmp.cfname.data = (const uint8_t *)&anon_idents;
    tmp.cfname.len = sizeof(anon_idents);
    tmp.name_hash = str_hash(&tmp.cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
    if (!b) return NULL;
    return strongest_label(b);
}

/* --------------------------------------------------------------------------- */
static struct label_s *lastlb = NULL;
struct label_s *new_label(const str_t *name, struct label_s *context, uint8_t strength, int *exists) {
    struct avltree_node *b;
    struct label_s *tmp;
    if (!lastlb) lastlb = var_alloc();

    if (name->len > 1 && !name->data[1]) lastlb->cfname = *name;
    else str_cfcpy(&lastlb->cfname, name);
    lastlb->name_hash = str_hash(&lastlb->cfname);
    lastlb->strength = strength;

    b = avltree_insert(&lastlb->node, &context->members, label_compare2);
    if (!b) { /* new label */
        str_cpy(&lastlb->name, name);
        if (lastlb->cfname.data == name->data) lastlb->cfname = lastlb->name;
        else str_cfcpy(&lastlb->cfname, NULL);
        lastlb->parent = context;
        lastlb->ref = 0;
        lastlb->shadowcheck = 0;
        lastlb->update_after = 0;
        lastlb->usepass = 0;
        lastlb->defpass = pass;
        avltree_init(&lastlb->members);
	*exists = 0;
	tmp = lastlb;
	lastlb = NULL;
	return tmp;
    }
    *exists = 1;
    return avltree_container_of(b, struct label_s, node);            /* already exists */
}

void shadow_check(const struct avltree *members) {
    const struct avltree_node *n, *b;
    const struct label_s *l, *c;

    return; /* this works, but needs an option to enable */

    n = avltree_first(members);
    while (n) {
        l = cavltree_container_of(n, struct label_s, node);            /* already exists */
        shadow_check(&l->members);
        n = avltree_next(n);
        if (l->shadowcheck) {
            c = l->parent->parent;
            while (c) {
                b = avltree_lookup(&l->node, &c->members, label_compare);
                if (b) {
                    const struct label_s *l2, *v1, *v2;
                    v1 = l2 = cavltree_container_of(b, struct label_s, node);
                    v2 = l;
                    if (v1 != v2 && !obj_same(v1->value, v2->value)) {
                        err_msg_shadow_defined(l2, l);
                        break;
                    }
                }
                c = c->parent;
            }
        }
    }
}

static struct label_s *find_strongest_label(struct avltree_node **x, avltree_cmp_fn_t cmp) {
    struct label_s *a = NULL, *c; 
    struct avltree_node *b = *x, *n = b;
    do {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass) a = c;
        n = avltree_next(n);
    } while (n && !cmp(n, b));
    *x = n;
    if (a) return a;
    n = avltree_prev(b);
    while (n && !cmp(n, b)) {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass) return c;
        n = avltree_prev(n);
    }
    return NULL;
}

static void labelname_print(const struct label_s *l, FILE *flab) {
    if (l->parent->parent->parent) labelname_print(l->parent, flab);
    putc('.', flab);
    printable_print2(l->name.data, flab, l->name.len);
}

static inline void padding(int l, int t, FILE *f) {
    if (arguments.tab_size > 1) {
        int l2 = l - l % arguments.tab_size;
        while (l2 + arguments.tab_size <= t) { l2 += arguments.tab_size; l = l2; putc('\t', f);} 
    }
    while (l < t) { l++; putc(' ', f);} 
}

static void labelprint2(const struct avltree *members, FILE *flab) {
    struct avltree_node *n;
    struct label_s *l;

    n = avltree_first(members);
    while (n) {
        l = find_strongest_label(&n, label_compare);            /* already exists */
        if (!l) continue;
        if (l->name.data && l->name.len > 1 && !l->name.data[1]) continue;
        switch (l->value->obj->type) {
        case T_LBL:
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT: continue;
        default:break;
        }
        if (0) { /* for future use with VICE */
            if (l->value->obj == CODE_OBJ) {
                value_t err;
                uval_t uv;
                struct linepos_s epoint;
                err = l->value->obj->uval(l->value, &uv, 24, &epoint);
                if (err) val_destroy(err);
                else {
                    fprintf(flab, "al %x ", uv);
                    labelname_print(l, flab);
                    switch ((enum dtype_e)l->value->u.code.dtype) {
                    case D_CHAR:
                    case D_BYTE: 
                        fputs(" byte", flab);
                        if (l->value->u.code.size > 1) {
                            fprintf(flab, " %" PRIxSIZE, l->value->u.code.size);
                        }
                        break;
                    case D_INT:
                    case D_WORD: 
                        fputs(" word", flab);
                        if (l->value->u.code.size > 2) {
                            fprintf(flab, " %" PRIxSIZE, l->value->u.code.size);
                        }
                        break;
                    case D_LINT:
                    case D_LONG:
                        fputs(" long", flab);
                        if (l->value->u.code.size > 3) {
                            fprintf(flab, " %" PRIxSIZE, l->value->u.code.size);
                        }
                        break;
                    case D_DINT:
                    case D_DWORD:
                        fputs(" dword", flab);
                        if (l->value->u.code.size > 4) {
                            fprintf(flab, " %" PRIxSIZE, l->value->u.code.size);
                        }
                        break;
                    case D_NONE:
                        break;
                    }
                    putc('\n', flab);
                }
            }
            labelprint2(&l->members, flab);
        } else {
            value_t val = l->value->obj->repr(l->value, NULL);
            size_t len;
            if (!val || val->obj != STR_OBJ) continue;
            len = printable_print2(l->name.data, flab, l->name.len);
            padding(len, EQUAL_COLUMN, flab);
            if (l->constant) fputs("= ", flab);
            else fputs(&" .var "[len < EQUAL_COLUMN], flab);
            printable_print2(val->u.str.data, flab, val->u.str.len);
            putc('\n', flab);
        }
    }
}

void labelprint(void) {
    int oldreferenceit = referenceit;
    FILE *flab;
    struct linepos_s nopoint = {0, 0};

    if (arguments.label[0] == '-' && !arguments.label[1]) {
        flab = stdout;
    } else {
        if (!(flab = file_open(arguments.label, "wt"))) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label, &nopoint);
    }
    clearerr(flab);
    referenceit = 0;
    labelprint2(&root_label->members, flab);
    referenceit = oldreferenceit;
    if (flab == stdout) fflush(flab);
    if (ferror(flab) && errno) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label, &nopoint);
    if (flab != stdout) fclose(flab);
}


void init_variables2(struct label_s *label) {
    avltree_init(&label->members);
}

static const struct label_s *new_builtin(const char *ident, value_t val) {
    struct linepos_s nopoint = {0, 0};
    str_t name;
    struct label_s *label;
    int label_exists;
    name.len = strlen(ident);
    name.data = (const uint8_t *)ident;
    label = new_label(&name, &builtin_label, 0, &label_exists);
    label->constant = 1;
    label->requires = 0;
    label->conflicts = 0;
    label->value = val;
    label->file_list = NULL;
    label->epoint = nopoint;
    return label;
}

void init_variables(void)
{
    size_t i;
    str_t name;
    int label_exists;
    struct linepos_s nopoint = {0, 0};

    labels = (struct labels_s *)malloc(sizeof(struct labels_s));
    if (!labels) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        labels->vals[i].next = &labels->vals[i+1];
    }
    labels->vals[i].next = NULL;
    labels->next = NULL;

    labels_free = &labels->vals[0];

    avltree_init(&builtin_label.members);
    builtin_label.cfname.len = builtin_label.name.len = 9;
    builtin_label.cfname.data = builtin_label.name.data = (const uint8_t *)"<builtin>";
    builtin_label.constant = 1;
    builtin_label.defpass = 1;
    builtin_label.usepass = 1;
    builtin_label.parent = NULL;

    name.len = 8;
    name.data = (const uint8_t *)"<global>";
    root_label = new_label(&name, &builtin_label, 0, &label_exists);
    root_label->constant = 1;
    root_label->requires = 0;
    root_label->conflicts = 0;
    root_label->value = val_reference(none_value);
    root_label->file_list = NULL;
    root_label->epoint = nopoint;
}

void init_defaultlabels(void) {
    value_t v;
    int i;

    new_builtin("true", val_reference(true_value));
    new_builtin("false", val_reference(false_value));

    for (i = 0; reg_names[i]; i++) {
        char name[2];
        name[0] = reg_names[i];
        name[1] = 0;
        v = val_alloc(REGISTER_OBJ);
        v->u.reg.val[0] = name[0];
        v->u.reg.data = v->u.reg.val;
        v->u.reg.len = 1;
        v->u.reg.chars = 1;
        new_builtin(name, v);
    }

    for (i = 0; builtin_functions[i].name; i++) {
        const struct label_s *label;
        v = val_alloc(FUNCTION_OBJ);
        v->u.function.name.data = (const uint8_t *)builtin_functions[i].name;
        v->u.function.name.len = strlen(builtin_functions[i].name);
        v->u.function.func = builtin_functions[i].func;
        label = new_builtin(builtin_functions[i].name, v);
        v->u.function.name_hash = label->name_hash;
    }

    v = val_alloc(TYPE_OBJ); v->u.type = TYPE_OBJ; new_builtin("type", v);
    v = val_alloc(TYPE_OBJ); v->u.type = STR_OBJ; new_builtin("str", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BOOL_OBJ; new_builtin("bool", v);
    v = val_alloc(TYPE_OBJ); v->u.type = INT_OBJ; new_builtin("int", v);
    v = val_alloc(TYPE_OBJ); v->u.type = FLOAT_OBJ; new_builtin("float", v);
    v = val_alloc(TYPE_OBJ); v->u.type = REGISTER_OBJ; new_builtin("register", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BYTES_OBJ; new_builtin("bytes", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BITS_OBJ; new_builtin("bits", v);
    v = val_alloc(TYPE_OBJ); v->u.type = DICT_OBJ; new_builtin("dict", v);
    v = val_alloc(TYPE_OBJ); v->u.type = LIST_OBJ; new_builtin("list", v);
    v = val_alloc(TYPE_OBJ); v->u.type = TUPLE_OBJ; new_builtin("tuple", v);
    v = val_alloc(TYPE_OBJ); v->u.type = ADDRESS_OBJ; new_builtin("address", v);
    v = val_alloc(TYPE_OBJ); v->u.type = CODE_OBJ; new_builtin("code", v);
    v = val_alloc(TYPE_OBJ); v->u.type = GAP_OBJ; new_builtin("gap", v);
}

static void label_free2(struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    value_t val = a->value;
    if (val->refcount > 1) {
        switch (val->obj->type) {
        case T_CODE:
            if (val->u.code.label == a) {
                val->refcount--;
                a->parent = NULL;
                return;
            }
            break;
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT:
            if (val->u.macro.label == a) {
                val->refcount--;
                a->parent = NULL;
                return;
            }
            break;
        default: break;
        }
    }
    free((char *)a->name.data);
    if (a->name.data != a->cfname.data) free((uint8_t *)a->cfname.data);
    avltree_destroy(&a->members, label_free2);
    val_destroy(a->value);
    var_free((union label_u *)a);
}

void label_destroy(struct label_s *a) {
    free((char *)a->name.data);
    if (a->name.data != a->cfname.data) free((uint8_t *)a->cfname.data);
    avltree_destroy(&a->members, label_free2);
    var_free((union label_u *)a);
}

void destroy_variables2(struct label_s *label) {
    avltree_destroy(&label->members, label_free2);
}

void destroy_variables(void)
{
    struct labels_s *old;

    avltree_destroy(&builtin_label.members, label_free);
    if (lastlb) var_free((union label_u *)lastlb);

    while (labels) {
        old = labels;
        labels = labels->next;
        free(old);
    }
}
