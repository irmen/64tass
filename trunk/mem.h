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
#ifndef _MEM_H_
#define _MEM_H_
#include "inttypes.h"

struct memblock_s;

struct memblocks_s {
    struct {       /* Linear memory dump */
        size_t p, len;
        uint8_t *data;
    } mem;
    unsigned int compressed:1;
    size_t p, len;
    size_t lastp;
    address_t lastaddr;
    struct memblock_s *data;
};

extern void mark_mem(const struct memblocks_s *, address_t);
extern void write_mark_mem(struct memblocks_s *, uint8_t);
extern void list_mem(const struct memblocks_s *, int);
extern void memjmp(struct memblocks_s *, address_t);
extern void memref(struct memblocks_s *, struct memblocks_s *);
extern void memprint(struct memblocks_s *);
extern void output_mem(struct memblocks_s *);
extern void write_mem(struct memblocks_s *, uint8_t);
extern int16_t read_mem(const struct memblocks_s *, size_t, size_t, size_t);
extern void get_mem(const struct memblocks_s *, size_t *, size_t *);
extern void restart_memblocks(struct memblocks_s *, address_t);
extern void init_memblocks(struct memblocks_s *);
extern void destroy_memblocks(struct memblocks_s *);
#endif
