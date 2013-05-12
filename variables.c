/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include "variables.h"
#include <stdlib.h>
#include <string.h>
#include "error.h"
#include "misc.h"
#include "values.h"
#include "file.h"
#include "listobj.h"

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
        labels = malloc(sizeof(struct labels_s));
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

struct label_s *find_label(const str_t *name) {
    struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp;
    tmp.name = *name;
    tmp.name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);

    while (context) {
        b=avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) return avltree_container_of(b, struct label_s, node);
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
    return avltree_container_of(b, struct label_s, node);
}

// ---------------------------------------------------------------------------
static struct label_s *lastlb=NULL;
struct label_s *new_label(const str_t *name, struct label_s *context, enum label_e type, int *exists) {
    struct avltree_node *b;
    struct label_s *tmp;
    if (!lastlb) lastlb=var_alloc();
    lastlb->name = *name;
    lastlb->name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);

    b = avltree_insert(&lastlb->node, &context->members, label_compare);
    if (!b) { //new label
        str_cpy(&lastlb->name, name);
        lastlb->type = type;
        lastlb->parent = context;
        lastlb->ref = 0;
        lastlb->nested = 0;
        lastlb->shadowcheck = 0;
        lastlb->update_after = 0;
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
                    int rec = 100;
                    v1 = l2 = cavltree_container_of(b, struct label_s, node);
                    while (v1->value->obj == IDENTREF_OBJ) {
                        v1 = v1->value->u.identref.label;
                        if (!rec--) break;
                    }
                    rec = 100; v2 = l;
                    while (v2->value->obj == IDENTREF_OBJ) {
                        v2 = v2->value->u.identref.label;
                        if (!rec--) break;
                    }
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

void init_variables2(struct label_s *label) {
    avltree_init(&label->members);
    label->parent = NULL;
}

void init_variables(void)
{
    size_t i;
    labels = malloc(sizeof(struct labels_s));
    if (!labels) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        labels->vals[i].next = &labels->vals[i+1];
    }
    labels->vals[i].next = NULL;
    labels->next = NULL;

    labels_free = &labels->vals[0];

    avltree_init(&root_label.members);
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
