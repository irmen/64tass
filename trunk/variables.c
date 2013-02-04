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

/*static void var_free(struct label_s *val) {
    //free(val); return;
    ((union label_u *)val)->next = labels_free;
    labels_free = (union label_u *)val;
}*/

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
static void label_free(const struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    free((char *)a->name);
    free((char *)a->origname);
    avltree_destroy(&a->members);
    val_destroy(a->value);
//    var_free(a);
}

static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    struct label_s *b = avltree_container_of(bb, struct label_s, node);

    return strcmp(a->name, b->name);
}

struct label_s *find_label(const char* name) {
    const struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp;
    tmp.name=name;
    
    while (context) {
        b=avltree_lookup(&tmp.node, &context->members);
        if (b) return avltree_container_of(b, struct label_s, node);
        context = context->parent;
    }
    return NULL;
}

struct label_s *find_label2(const char* name, const struct avltree *tree) {
    const struct avltree_node *b;
    struct label_s tmp;
    tmp.name=name;
    b=avltree_lookup(&tmp.node, tree);
    if (!b) return NULL;
    return avltree_container_of(b, struct label_s, node);
}

// ---------------------------------------------------------------------------
static struct label_s *lastlb=NULL;
struct label_s *new_label(const char* name, const char* origname, enum label_e type) {
    const struct avltree_node *b;
    struct label_s *tmp;
    if (!lastlb) lastlb=var_alloc();
    lastlb->name=name;
    b=avltree_insert(&lastlb->node, &current_context->members);
    if (!b) { //new label
	if (!(lastlb->name=malloc(strlen(name)+1))) err_msg_out_of_memory();
        strcpy((char *)lastlb->name,name);
	if (!(lastlb->origname=malloc(strlen(origname)+1))) err_msg_out_of_memory();
        strcpy((char *)lastlb->origname,origname);
        lastlb->type = type;
        lastlb->parent=current_context;
        lastlb->ref=lastlb->size=lastlb->esize=0;
        avltree_init(&lastlb->members, label_compare, label_free);
	labelexists=0;
	tmp=lastlb;
	lastlb=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct label_s, node);            //already exists
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

    avltree_init(&root_label.members, label_compare, label_free);
}

void destroy_variables(void)
{
    struct labels_s *old;

    avltree_destroy(&root_label.members);

    while (labels) {
        old = labels;
        labels = labels->next;
        free(old);
    }
}
