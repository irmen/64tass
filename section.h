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
#ifndef _SECTION_H_
#define _SECTION_H_
#include "libtree.h"
#include <stdint.h>
#include "misc.h"

struct section_s {
    const char *name;
    struct avltree_node node;

    uval_t requires;
    uval_t conflicts;
    uval_t provides;
    address_t start;
    address_t l_start;
    address_t address;
    address_t l_address;
    unsigned int dooutput:1;
    struct section_s *parent;
    struct avltree members;
};

extern struct section_s *new_section(const char*);
extern struct section_s *find_section(const char* name);
extern void init_section(void);
extern void destroy_section(void);
extern struct section_s *current_section, root_section;
#endif
