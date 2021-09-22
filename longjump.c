/*
    $Id: longjump.c 2708 2021-09-18 18:12:25Z soci $

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

static FAST_CALL int longjump_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    address_t a = cavltree_container_of(aa, struct longjump_s, node)->address;
    address_t b = cavltree_container_of(bb, struct longjump_s, node)->address;
    return (a > b) - (a < b);
}

static void longjump_free(struct avltree_node *aa)
{
    struct longjump_s *a = avltree_container_of(aa, struct longjump_s, node);
    free(a);
}

static struct longjump_s *lastlj;
struct longjump_s *new_longjump(struct avltree *t, address_t address) {
    struct avltree_node *b;
    struct longjump_s *tmp;

    if (lastlj == NULL) new_instance(&lastlj);
    lastlj->address = address;
    b = avltree_insert(&lastlj->node, t, longjump_compare);
    if (b == NULL) { /* new longjump */
        lastlj->defpass = 0;
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
