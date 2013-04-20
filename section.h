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
#include "inttypes.h"

struct section_s {
    int name_hash;
    str_t name;
    struct avltree_node node;

    uval_t requires;
    uval_t conflicts;
    uval_t provides;
    address_t start;
    address_t l_start;
    address_t address;
    address_t l_address;
    address_t end;
    address_t unionstart;
    address_t unionend;
    address_t l_unionstart;
    address_t l_unionend;
    size_t size;
    uint8_t usepass;
    uint8_t defpass;
    uint_fast8_t structrecursion;
    uint_fast8_t logicalrecursion;
    unsigned int dooutput:1;
    unsigned int declared:1;
    unsigned int unionmode:1;
    unsigned int moved:1;
    unsigned int wrapwarn:1;
    unsigned int wrapwarn2:1;
    struct section_s *parent;
    struct section_s *next;
    struct file_list_s *file_list;
    line_t sline;
    linepos_t epoint;
    struct avltree members;
};

extern struct section_s *new_section(const str_t *);
extern struct section_s *find_new_section(const str_t *);
extern void init_section(void);
extern void init_section2(struct section_s *);
extern void destroy_section(void);
extern void destroy_section2(struct section_s *);
extern void reset_section(void);
extern struct section_s *current_section, root_section;
#endif
