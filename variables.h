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
#include <stdint.h>
#include "values.h"
#include "libtree.h"
#include "inttypes.h"

enum label_e {
    L_LABEL, L_CONST, L_VAR, L_STRUCT, L_UNION
};

struct label_s {
    const char *name;
    const char *origname;
    enum label_e type;
    struct avltree_node node;

    struct value_s *value;
    size_t size;
    size_t memp;
    size_t membp;
    uval_t requires;
    uval_t conflicts;
    const char *file;
    line_t sline;
    unsigned int epoint;
    unsigned ref:1;
    unsigned sign:1;
    uint8_t esize;
    uint8_t pass;
    uint8_t upass;
    struct label_s *parent;
    struct avltree members;
};

extern struct label_s *current_context, root_label;
extern struct label_s *find_label(const char*);
extern struct label_s *find_label2(const char*, const struct avltree *);
extern struct label_s *new_label(const char*, const char*, enum label_e);
extern void destroy_variables(void);
extern void init_variables(void);
#endif
