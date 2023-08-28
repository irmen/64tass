/*
    $Id: section.h 3013 2023-08-15 06:36:01Z soci $

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
#ifndef SECTION_H
#define SECTION_H
#include "avl.h"
#include "stdbool.h"
#include "str.h"

struct Obj;
struct Memblocks;
struct optimizer_s;
struct output_s;

struct section_address_s {
    address_t address;
    address_t l_address;
    address_t start;
    address_t end;
    address_t l_start;
    address_t l_union;
    bool moved;
    bool wrapwarn;
    bool bankwarn;
    bool unionmode;
    struct Obj *l_address_val;
    struct Memblocks *mem;
};

struct section_s {
    int name_hash;
    str_t name;
    str_t cfname;
    struct avltree_node node;

    uval_t requires;
    uval_t conflicts;
    uval_t provides;
    address_t restart;
    address_t l_restart;
    address_t size;
    struct section_address_s address;
    uint8_t usepass;
    uint8_t defpass;
    uint8_t logicalrecursion;
    bool declared;
    struct section_s *parent;
    const struct file_list_s *file_list;
    struct optimizer_s *optimizer;
    struct linepos_s epoint;
    struct avltree members;
    struct avltree longjump;
};

extern struct section_s *new_section(const str_t *);
extern struct section_s *find_new_section(const str_t *);
extern struct section_s *find_this_section(const char *);
extern void init_section(void);
extern void destroy_section(void);
extern void reset_section(struct section_s *);
extern void section_sizecheck(const struct avltree_node *);
extern void section_memclose(const struct avltree_node *);
extern struct section_s *current_section, root_section;
extern struct section_address_s *current_address;
#endif
