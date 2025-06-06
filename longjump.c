/*
    $Id: longjump.c 3209 2025-04-12 14:22:00Z soci $

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
#include "longjump.h"
#include "error.h"
#include "obj.h"
#include "values.h"

#include "codeobj.h"

static FAST_CALL int longjump_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    Code *a2, *b2;
    address_t a = cavltree_container_of(aa, struct longjump_s, node)->address;
    address_t b = cavltree_container_of(bb, struct longjump_s, node)->address;
    if (a != b) return (a < b) ? -1 : 1;
    a2 = cavltree_container_of(aa, struct longjump_s, node)->code;
    b2 = cavltree_container_of(bb, struct longjump_s, node)->code;
    if (a2 == b2) return 0;
    if (a2->memblocks != b2->memblocks) return (a2->memblocks < b2->memblocks) ? -1 : 1;
    if (a2->membp != b2->membp) return (a2->membp < b2->membp) ? -1 : 1;
    if (a2->memaddr != b2->memaddr) return (a2->memaddr < b2->memaddr) ? -1 : 1;
    return 0;
}

static void longjump_free(struct avltree_node *aa)
{
    struct longjump_s *a = avltree_container_of(aa, struct longjump_s, node);
    val_destroy(Obj(a->code));
    free(a);
}

static struct longjump_s *lastlj;
struct longjump_s *new_longjump(struct avltree *t, address_t address, Code *code) {
    struct avltree_node *b;
    struct longjump_s *tmp;

    if (lastlj == NULL) new_instance(&lastlj);
    lastlj->code = code;
    lastlj->address = address;
    b = avltree_insert(&lastlj->node, t, longjump_compare);
    if (b == NULL) { /* new longjump */
        lastlj->defpass = 0;
        val_reference(Obj(lastlj->code));
        tmp = lastlj;
        lastlj = NULL;
        return tmp;
    }
    return avltree_container_of(b, struct longjump_s, node);            /* already exists */
}

void destroy_longjump(void) {
    free(lastlj);
    lastlj = NULL;
}

void longjump_destroy(struct avltree *t) {
    avltree_destroy(t, longjump_free);
}
