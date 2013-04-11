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
#include "section.h"
#include <stdlib.h>
#include <string.h>
#include "error.h"
#include "misc.h"

struct section_s root_section;
struct section_s *current_section = &root_section;
static struct section_s *prev_section = &root_section;

static int section_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct section_s *a = cavltree_container_of(aa, struct section_s, node);
    const struct section_s *b = cavltree_container_of(bb, struct section_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    return arguments.casesensitive ? str_cmp(&a->name, &b->name) : str_casecmp(&a->name, &b->name);
}

static void section_free(struct avltree_node *aa)
{
    struct section_s *a = avltree_container_of(aa, struct section_s, node);
    free((char *)a->name.data);
    avltree_destroy(&a->members, section_free);
    free(a);
}

struct section_s *find_new_section(const str_t *name) {
    struct avltree_node *b;
    struct section_s *context = current_section;
    struct section_s tmp, *tmp2 = NULL;
    tmp.name = *name;
    tmp.name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);

    while (context) {
        b=avltree_lookup(&tmp.node, &context->members, section_compare);
        if (b) {
            tmp2 = avltree_container_of(b, struct section_s, node);
            if (tmp2->defpass >= pass - 1) {
                return tmp2;
            }
        }
        context = context->parent;
    }
    if (tmp2) return tmp2;
    return new_section(name);
}

static struct section_s *lastsc=NULL;
struct section_s *new_section(const str_t *name) {
    struct avltree_node *b;
    struct section_s *tmp;

    if (!lastsc)
	if (!(lastsc=malloc(sizeof(struct section_s)))) err_msg_out_of_memory();
    lastsc->name = *name;
    lastsc->name_hash = arguments.casesensitive ? str_hash(name) : str_casehash(name);
    b=avltree_insert(&lastsc->node, &current_section->members, section_compare);
    if (!b) { //new section
        str_cpy(&lastsc->name, name);
        lastsc->parent=current_section;
        lastsc->provides=~(uval_t)0;lastsc->requires=lastsc->conflicts=0;
        lastsc->end=lastsc->address=lastsc->l_address=lastsc->size=0;
        lastsc->dooutput=1;
        lastsc->defpass=0;
        lastsc->usepass=0;
        lastsc->unionmode=0;
        lastsc->structrecursion=0;
        lastsc->logicalrecursion=0;
        lastsc->moved=0;
        lastsc->wrapwarn=0;
        lastsc->wrapwarn2=0;
        lastsc->next=NULL;
        prev_section->next = lastsc;
        prev_section = lastsc;
        avltree_init(&lastsc->members);
	tmp=lastsc;
	lastsc=NULL;
	return tmp;
    }
    return avltree_container_of(b, struct section_s, node);            //already exists
}

void reset_section(void) {
    current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
    current_section->end=current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
    current_section->dooutput=1;
    current_section->structrecursion=0;
    current_section->logicalrecursion=0;
    current_section->moved=0;
    current_section->wrapwarn=0;
    current_section->wrapwarn2=0;
    current_section->unionmode=0;
}

void init_section2(struct section_s *section) {
    section->parent = NULL;
    section->name.data = NULL;
    section->name.len = 0;
    section->next = NULL;
    avltree_init(&section->members);
}

void init_section(void) {
    init_section2(&root_section);
    prev_section = &root_section;
}

void destroy_section2(struct section_s *section) {
    avltree_destroy(&section->members, section_free);
}

void destroy_section(void) {
    free(lastsc);
    destroy_section2(&root_section);
}
// ---------------------------------------------------------------------------
