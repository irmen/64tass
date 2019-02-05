/*
    $Id: mem.h 1845 2019-01-28 07:55:58Z soci $

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
#ifndef MEM_H
#define MEM_H
#include "attributes.h"
#include "stdbool.h"
#include "inttypes.h"

struct Memblocks;
struct output_s;

extern void mark_mem(const struct Memblocks *, address_t, address_t);
extern void write_mark_mem(struct Memblocks *, unsigned int);
extern void list_mem(const struct Memblocks *);
extern void memjmp(struct Memblocks *, address_t);
extern void memref(struct Memblocks *, struct Memblocks *);
extern void memprint(struct Memblocks *);
extern void output_mem(struct Memblocks *, const struct output_s *);
extern FAST_CALL uint8_t *alloc_mem(struct Memblocks *, size_t);
extern int read_mem(const struct Memblocks *, address_t, size_t, size_t);
extern size_t get_mem(const struct Memblocks *);
#endif
