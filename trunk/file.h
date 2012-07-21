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
#ifndef _FILE_H_
#define _FILE_H_
#include "libtree.h"
#include "misc.h"

struct file_s {
    const char *name;
    uint8_t *data;    /* data */
    size_t len;       /* length */
    size_t p;         /* current point */
    uint16_t open;    /* open/not open */
    uint16_t uid;     /* uid */
    struct avltree star;
    struct avltree_node node;
};

struct star_s {
    line_t line;
    address_t addr;
    struct avltree tree;
    struct avltree_node node;
};

extern struct file_s *openfile(const char*);
extern void closefile(struct file_s*);
extern struct star_s *new_star(line_t);
extern void destroy_file(void);
extern void init_file(void);

#endif
