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
#ifndef _VARIABLES_H_
#define _VARIABLES_H_
#include "libtree.h"
#include "inttypes.h"

struct label_s {
    int name_hash;
    str_t name;
    str_t cfname;
    struct avltree_node node;

    value_t value;
    uval_t requires;
    uval_t conflicts;
    struct file_list_s *file_list;
    struct linepos_s epoint;
    unsigned ref:1;
    unsigned shadowcheck:1;
    unsigned update_after:1;
    unsigned constant:1;
    uint8_t usepass;
    uint8_t defpass;
    uint8_t strength;
    struct label_s *parent;
    struct avltree members;
};

extern struct label_s *current_context, *cheap_context, *root_label;
extern struct label_s *find_label(const str_t *);
extern struct label_s *find_label2(const str_t *, const struct label_s *);
extern struct label_s *find_label3(const str_t *, const struct label_s *, uint8_t);
extern struct label_s *find_anonlabel(int32_t);
extern struct label_s *find_anonlabel2(int32_t, const struct label_s *);
extern struct label_s *new_label(const str_t *, struct label_s *, uint8_t, int *);
extern void labelprint(void);
extern void shadow_check(const struct avltree *);
extern void destroy_variables(void);
extern void destroy_variables2(struct label_s *);
extern void init_variables(void);
extern void init_defaultlabels(void);
extern void init_variables2(struct label_s *);
extern void label_destroy(struct label_s *);
#endif
