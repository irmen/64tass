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
#ifndef _ENCODING_H_
#define _ENCODING_H_
#include "libtree.h"
#include "inttypes.h"

struct encoding_s;

struct trans_s {
    uint32_t start;
    unsigned end : 24;
    unsigned offset : 8;
    struct avltree_node node;
};

extern struct encoding_s *actual_encoding;

extern struct encoding_s *new_encoding(const str_t *);
extern struct trans_s *new_trans(struct trans_s *, struct encoding_s *);
extern int new_escape(const value_t, value_t, struct encoding_s *, linepos_t);
extern void encode_string_init(const value_t, linepos_t);
extern int encode_string(void);
extern void init_encoding(int);
extern void destroy_encoding(void);
#endif
