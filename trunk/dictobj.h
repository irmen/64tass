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
#ifndef _DICTOBJ_H
#define _DICTOBJ_H

typedef struct oper_s *oper_t;

extern obj_t DICT_OBJ;
extern obj_t LABELDICT_OBJ;

typedef struct {
    size_t len;
    struct avltree members;
    value_t def;
} dict_t;

typedef struct {
    size_t len;
    struct avltree members;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
} labeldict_t;

struct pair_s {
    int hash;
    value_t key;
    value_t data;
    struct avltree_node node;
};

extern void dictobj_init(void);
extern int pair_compare(const struct avltree_node *, const struct avltree_node *);
extern MUST_CHECK value_t new_labeldict(const struct file_list_s *, linepos_t);
extern MUST_CHECK value_t labeldict_member(oper_t, value_t);
#endif
