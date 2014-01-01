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
#include "variables.h"
#include "misc.h"
#include "values.h"
#include "64tass.h"
#include "file.h"

struct label_s root_label;
struct label_s *current_context = &root_label;

static union label_u {
    struct label_s val;
    union label_u *next;
} *labels_free = NULL;

static struct labels_s {
    union label_u vals[255];
    struct labels_s *next;
} *labels = NULL;

static void var_free(union label_u *val) {
    //free(val); return;
    val->next = labels_free;
    labels_free = val;
}

static struct label_s *var_alloc(void) {
    struct label_s *val;
    size_t i;
    //return malloc(sizeof(struct label_s));
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
}

// ---------------------------------------------------------------------------
static void label_free(struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    free((char *)a->name.data);
    avltree_destroy(&a->members, label_free);
    val_destroy(a->value);
}

static void label_free2(struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    label_free(aa);
    var_free((union label_u *)a);
}

static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct label_s *a = cavltree_container_of(aa, struct label_s, node);
    const struct label_s *b = cavltree_container_of(bb, struct label_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    return arguments.casesensitive ? str_cmp(&a->name, &b->name) : str_casecmp(&a->name, &b->name);
}

static int label_compare2(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct label_s *a = cavltree_container_of(aa, struct label_s, node);
    const struct label_s *b = cavltree_container_of(bb, struct label_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    h = arguments.casesensitive ? str_cmp(&a->name, &b->name) : str_casecmp(&a->name, &b->name);
    if (h) return h;
    return a->strength - b->strength;
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
    return NULL;
}

struct label_s *find_label(const str_t *name) {
    struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp, *c;
    tmp.name = *name;
    tmp.name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);

    while (context) {
        b=avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) return c;
        }
        context = context->parent;
    }
    return NULL;
}

struct label_s *find_label2(const str_t *name, const struct label_s *context) {
    struct avltree_node *b;
    struct label_s tmp;
    tmp.name = *name;
    tmp.name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);

    b=avltree_lookup(&tmp.node, &context->members, label_compare);
    if (!b) return NULL;
    return strongest_label(b);
}

struct label_s *find_label3(const str_t *name, const struct label_s *context, int8_t strength) {
    struct avltree_node *b;
    struct label_s tmp;
    tmp.name = *name;
    tmp.name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);
    tmp.strength = strength;

    b=avltree_lookup(&tmp.node, &context->members, label_compare2);
    if (!b) return NULL;
    return avltree_container_of(b, struct label_s, node);
}

// ---------------------------------------------------------------------------
static struct label_s *lastlb=NULL;
struct label_s *new_label(const str_t *name, struct label_s *context, int8_t strength, int *exists) {
    struct avltree_node *b;
    struct label_s *tmp;
    if (!lastlb) lastlb=var_alloc();
    lastlb->name = *name;
    lastlb->name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);
    lastlb->strength = strength;

    b = avltree_insert(&lastlb->node, &context->members, label_compare2);
    if (!b) { //new label
        str_cpy(&lastlb->name, name);
        lastlb->parent = context;
        lastlb->ref = 0;
        lastlb->shadowcheck = 0;
        lastlb->update_after = 0;
        lastlb->usepass = pass;
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

static struct label_s *find_strongest_label(struct avltree_node **x) {
    struct label_s *a = NULL, *c; 
    struct avltree_node *b = *x, *n = b;
    do {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass) a = c;
        n = avltree_next(n);
    } while (n && !label_compare(n, b));
    *x = n;
    if (a) return a;
    n = avltree_prev(b);
    while (n && !label_compare(n, b)) {
        c = avltree_container_of(n, struct label_s, node);
        if (c->defpass == pass) return c;
        n = avltree_prev(n);
    }
    return NULL;
}

static void labelname_print(const struct label_s *l, FILE *flab) {
    if (l->parent->parent) labelname_print(l->parent, flab);
    fputc('.', flab);
    fwrite(l->name.data, l->name.len, 1, flab);
}

static void labelprint2(const struct avltree *members, FILE *flab) {
    struct avltree_node *n;
    struct label_s *l;
    n = avltree_first(members);
    while (n) {
        l = find_strongest_label(&n);            /* already exists */
        if (!l) continue;
        if (l->name.data && l->name.len) {
            if (l->name.data[0]=='-' || l->name.data[0]=='+') continue;
            if (l->name.data[0]=='.' || l->name.data[0]=='#') continue;
        }
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
                struct value_s tmp;
                uval_t uv;
                struct linepos_s epoint;
                if (!l->value->obj->uval(l->value, &tmp, &uv, 24, &epoint)) {
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
            if (l->constant) {
                fwrite(l->name.data, l->name.len, 1, flab);
                if (l->name.len < 16) fputs(&"                "[l->name.len], flab);
                fputs("= ", flab);
            } else {
                fwrite(l->name.data, l->name.len, 1, flab);
                if (l->name.len < 15) fputs(&"               "[l->name.len], flab);
                fputs(" .var ", flab);
            }
            obj_print(l->value, flab);
            putc('\n', flab);
        }
    }
}

void labelprint(void) {
    FILE *flab;

    if (arguments.label[0] == '-' && !arguments.label[1]) {
        flab = stdout;
    } else {
        if (!(flab=file_open(arguments.label,"wt"))) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label);
    }
    clearerr(flab);
    labelprint2(&root_label.members, flab);
    if (ferror(flab)) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label);
    if (flab != stdout) fclose(flab);
}


void init_variables2(struct label_s *label) {
    avltree_init(&label->members);
}

void init_variables(void)
{
    size_t i;
    labels = (struct labels_s *)malloc(sizeof(struct labels_s));
    if (!labels) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        labels->vals[i].next = &labels->vals[i+1];
    }
    labels->vals[i].next = NULL;
    labels->next = NULL;

    labels_free = &labels->vals[0];

    avltree_init(&root_label.members);
    root_label.name.len = 8;
    root_label.name.data = (const uint8_t *)"<global>";
}

void destroy_variables2(struct label_s *label) {
    avltree_destroy(&label->members, label_free2);
}

void destroy_variables(void)
{
    struct labels_s *old;

    avltree_destroy(&root_label.members, label_free);

    while (labels) {
        old = labels;
        labels = labels->next;
        free(old);
    }
}
