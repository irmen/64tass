/*
    $Id: variables.h 2813 2022-10-18 18:28:21Z soci $

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
#ifndef VARIABLES_H
#define VARIABLES_H
#include "stdbool.h"
#include "inttypes.h"

struct Namespace;
struct Label;
struct Obj;
struct Mfunc;
struct symbol_output_s;
struct file_list_s;
struct str_t;

extern void push_dummy_context(void);
extern void push_context(struct Namespace *);
extern bool pop_context(void);
extern void reset_context(void);
extern void push_context2(struct Namespace *);
extern bool pop_context2(void);
extern void get_namespaces(struct Mfunc *);
extern size_t context_get_bottom(void);
extern void context_set_bottom(size_t);

extern struct Namespace *current_context, *cheap_context, *root_namespace;
extern size_t fwcount;
extern struct Label *find_label(const struct str_t *, struct Namespace **);
extern struct Label *find_label2(const struct str_t *, struct Namespace *);
extern struct Label *find_label3(const struct str_t *, struct Namespace *, uint8_t);
extern struct Label *find_anonlabel(ssize_t);
extern struct Label *find_anonlabel2(ssize_t, struct Namespace *);
extern struct Label *new_label(const struct str_t *, struct Namespace *, uint8_t, const struct file_list_s *);
extern void label_move(struct Label *, const struct str_t *, const struct file_list_s *);
extern void labelprint(const struct symbol_output_s *);
extern void unused_check(struct Namespace *);
extern void ref_labels(void);
extern void destroy_variables(void);
extern void init_variables(void);
extern void destroy_lastlb(void);
extern void new_builtin(const char *, struct Obj *);
#endif
