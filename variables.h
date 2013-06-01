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
#ifndef _VARIABLES_H_
#define _VARIABLES_H_
#include "libtree.h"
#include "inttypes.h"

enum label_e {
    L_LABEL, L_CONST, L_VAR
};

struct label_s {
    int name_hash;
    str_t name;
    enum label_e type;
    struct avltree_node node;

    struct value_s *value;
    uval_t requires;
    uval_t conflicts;
    struct file_list_s *file_list;
    line_t sline;
    struct linepos_s epoint;
    unsigned ref:1;
    unsigned nested:1;
    unsigned shadowcheck:1;
    unsigned update_after:1;
    uint8_t usepass;
    uint8_t defpass;
    struct label_s *parent;
    struct avltree members;
};

extern struct label_s *current_context, root_label;
extern struct label_s *find_label(const str_t *);
extern struct label_s *find_label2(const str_t *, const struct label_s *);
extern struct label_s *new_label(const str_t *, struct label_s *, enum label_e, int *);
extern void shadow_check(const struct avltree *);
extern void destroy_variables(void);
extern void destroy_variables2(struct label_s *);
extern void init_variables(void);
extern void init_variables2(struct label_s *);
#endif