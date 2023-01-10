/*
    $Id: memblocksobj.h 2954 2023-01-07 10:22:44Z soci $

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
#ifndef MEMBLOCKSOBJ_H
#define MEMBLOCKSOBJ_H
#include "obj.h"
#include "stdbool.h"
#include "inttypes.h"

struct section_s;
struct output_s;

extern struct Type *const MEMBLOCKS_OBJ;

struct memblock_s { /* starts and sizes */
    address_t p;
    address_t addr, len;
    struct Memblocks *ref;
};

typedef struct Memblocks {
    Obj v;
    struct {       /* Linear memory dump */
        address_t p, len;
        uint8_t *data;
    } mem;
    size_t p, len;
    address_t lastp;
    address_t lastaddr;
    bool flattened, merged, enumeration;
    struct memblock_s *data;
    const struct section_s *section;
} Memblocks;

#define Memblocks(a) OBJ_CAST(Memblocks, a)

extern void memblocksobj_init(void);

static inline Memblocks *ref_memblocks(Memblocks *v1) {
    v1->v.refcount++; return v1;
}

extern MALLOC Memblocks *new_memblocks(address_t, size_t);
extern MALLOC Memblocks *copy_memblocks(Memblocks *);
extern void printmemorymap(const Memblocks *);
extern void memorymapfile(const Memblocks *, const struct output_s *);
#endif
