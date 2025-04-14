/*
    $Id: longjump.h 3209 2025-04-12 14:22:00Z soci $

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
#ifndef LONGJUMP_H
#define LONGJUMP_H
#include "avl.h"
#include "inttypes.h"

struct Code;

struct longjump_s {
    struct Code *code;
    address_t address;
    address_t dest;
    struct avltree_node node;
    uint8_t defpass;
};

extern struct longjump_s *new_longjump(struct avltree *, address_t, struct Code *);
extern void destroy_longjump(void);
extern void longjump_destroy(struct avltree *);
#endif
