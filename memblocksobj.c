/*
    $Id: memblocksobj.c 1851 2019-01-29 20:04:20Z soci $

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
#include "memblocksobj.h"
#include <string.h>
#include "values.h"
#include "error.h"

#include "typeobj.h"

static Type obj;

Type *const MEMBLOCKS_OBJ = &obj;

static FAST_CALL void destroy(Obj *o1) {
    size_t i;
    Memblocks *v1 = (Memblocks *)o1;
    free(v1->mem.data);
    for (i = 0; i < v1->p; i++) {
        struct memblock_s *b = &v1->data[i];
        if (b->ref != NULL) val_destroy(&b->ref->v);
    }
    free(v1->data);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    size_t i;
    const Memblocks *v1 = (const Memblocks *)o1, *v2 = (const Memblocks *)o2;
    if (v1->p != v2->p) return false;
    for (i = 0; i < v1->p; i++) {
        struct memblock_s *b1 = &v1->data[i];
        struct memblock_s *b2 = &v2->data[i];
        if (b1->p != b2->p || b1->addr != b2->addr || b1->len != b2->len) return false;
        if (b1->ref == b2->ref) continue;
        if (b1->ref == NULL || b2->ref == NULL) return false;
        if (!same(&b1->ref->v, &b2->ref->v)) return false;
    }
    return v1->mem.p == v2->mem.p && !memcmp(v1->mem.data, v2->mem.data, v1->mem.p);
}

MALLOC Memblocks *new_memblocks(size_t ln, size_t ln2) {
    Memblocks *val = (Memblocks *)val_alloc(MEMBLOCKS_OBJ);
    val->mem.p = 0;
    val->mem.len = ln;
    val->mem.data = (ln == 0) ? NULL : (uint8_t*)mallocx(ln);
    val->p = 0;
    val->len = ln2;
    val->lastp = 0;
    val->lastaddr = 0;
    val->data = (ln2 == 0) ? NULL : (struct memblock_s *)mallocx(ln2 * sizeof *val->data);
    val->compressed = false;
    return val;
}

void memblocksobj_init(void) {
    new_type(&obj, T_MEMBLOCKS, "memblocks", sizeof(Memblocks));
    obj.destroy = destroy;
    obj.same = same;
}
