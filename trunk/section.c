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
#include "unicode.h"
#include "section.h"
#include "error.h"
#include "misc.h"
#include "64tass.h"

struct section_s root_section;
struct section_s *current_section = &root_section;
static struct section_s *prev_section = &root_section;

static int section_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct section_s *a = cavltree_container_of(aa, struct section_s, node);
    const struct section_s *b = cavltree_container_of(bb, struct section_s, node);
    int h = a->name_hash - b->name_hash;
    if (h) return h; 
    return str_cmp(&a->cfname, &b->cfname);
}

static void section_free(struct avltree_node *aa)
{
    struct section_s *a = avltree_container_of(aa, struct section_s, node);
    free((uint8_t *)a->name.data);
    if (a->name.data != a->cfname.data) free((uint8_t *)a->cfname.data);
    avltree_destroy(&a->members, section_free);
    destroy_memblocks(&a->mem);
    free(a);
}

struct section_s *find_new_section(const str_t *name) {
    struct avltree_node *b;
    struct section_s *context = current_section;
    struct section_s tmp, *tmp2 = NULL;

    str_cfcpy(&tmp.cfname, name);
    tmp.name_hash = str_hash(&tmp.cfname);

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

    if (!lastsc) {
	lastsc = (struct section_s *)malloc(sizeof(struct section_s));
	if (!lastsc) err_msg_out_of_memory();
    }
    str_cfcpy(&lastsc->cfname, name);
    lastsc->name_hash = str_hash(&lastsc->cfname);
    b=avltree_insert(&lastsc->node, &current_section->members, section_compare);
    if (!b) { /* new section */
        str_cpy(&lastsc->name, name);
        if (lastsc->cfname.data == name->data) lastsc->cfname = lastsc->name;
        else str_cfcpy(&lastsc->cfname, NULL);
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
        init_memblocks(&lastsc->mem);
        avltree_init(&lastsc->members);
	tmp=lastsc;
	lastsc=NULL;
	return tmp;
    }
    return avltree_container_of(b, struct section_s, node);            /* already exists */
}

void reset_section(struct section_s *section) {
    section->provides = ~(uval_t)0; section->requires = section->conflicts = 0;
    section->end = section->start = section->restart = section->l_restart = section->address = section->l_address = 0;
    section->dooutput = 1;
    section->structrecursion = 0;
    section->logicalrecursion = 0;
    section->moved = 0;
    section->wrapwarn = 0;
    section->wrapwarn2 = 0;
    section->unionmode = 0;
}

void init_section2(struct section_s *section) {
    section->parent = NULL;
    section->name.data = NULL;
    section->name.len = 0;
    section->cfname.data = NULL;
    section->cfname.len = 0;
    section->next = NULL;
    init_memblocks(&section->mem);
    avltree_init(&section->members);
}

void init_section(void) {
    init_section2(&root_section);
    prev_section = &root_section;
}

void destroy_section2(struct section_s *section) {
    avltree_destroy(&section->members, section_free);
    destroy_memblocks(&section->mem);
}

void destroy_section(void) {
    free(lastsc);
    destroy_section2(&root_section);
}

static void sectionprint2(const struct section_s *l) {
    if (l->name.data) {
        sectionprint2(l->parent);
        printable_print2(l->name.data, stdout, l->name.len);
        putchar('.');
    }
}

void sectionprint(void) {
    struct section_s *l;
    char temp[10], temp2[10];

    l = &root_section;
    if (l->size) {
        sprintf(temp, "$%04" PRIaddress, l->start);
        sprintf(temp2, "$%04" PRIaddress, (address_t)(l->start + l->size - 1));
        printf("Section:         %7s-%-7s\n", temp, temp2);
    }
    memprint(&l->mem);
    l = root_section.next;
    while (l) {
        if (l->defpass == pass) {
            if (l->size) {
                sprintf(temp, "$%04" PRIaddress, l->start);
                sprintf(temp2, "$%04" PRIaddress, (address_t)(l->start + l->size - 1));
                printf("Section:         %7s-%-7s ", temp, temp2);
            } else {
                printf("Section:                         ");
            }
            sectionprint2(l->parent);
            printable_print2(l->name.data, stdout, l->name.len);
            putchar('\n');
            memprint(&l->mem);
        }
        l = l->next;
    }
}
