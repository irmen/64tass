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
#ifndef _FILE_H_
#define _FILE_H_
#include <stdio.h>
#include "inttypes.h"
#include "libtree.h"

enum filecoding_e {
    E_UNKNOWN, E_UTF8, E_UTF16LE, E_UTF16BE, E_ISO
};

struct file_s {
    const char *name;
    const char *realname;
    const char *base;
    size_t *line;
    size_t lines;
    uint8_t *data;    /* data */
    size_t len;       /* length */
    uint16_t open;    /* open/not open */
    uint16_t uid;     /* uid */
    int type;
    enum filecoding_e coding;
    struct avltree star;
    struct avltree_node node;
};

struct star_s {
    line_t line;
    address_t addr;
    struct avltree tree;
    struct avltree_node node;
};

extern struct file_s *openfile(const char *, const char *, int, const value_t, linepos_t);
extern void closefile(struct file_s*);
extern struct star_s *new_star(line_t, int *);
extern void destroy_file(void);
extern void init_file(void);
extern FILE *file_open(const char *, const char *);
extern void include_list_add(const char *);
extern const char *get_path(const value_t, const char *);
extern uint32_t fromiso(uint8_t);

#endif
