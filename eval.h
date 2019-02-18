/*
    $Id: eval.h 1866 2019-02-09 09:26:10Z soci $

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
#ifndef EVAL_H
#define EVAL_H
#include "attributes.h"
#include "inttypes.h"
#include "stdbool.h"

struct List;
struct Label;
struct Oper;
struct Obj;

struct oper_s {
    struct Oper *op;
    struct Obj *v1;
    struct Obj *v2;
    linepos_t epoint;
    linepos_t epoint2;
    linepos_t epoint3;
    struct Obj *inplace;
};
typedef struct oper_s *oper_t;

extern bool get_exp(int, unsigned int, unsigned int, linepos_t);
extern struct values_s *get_val(void);
extern struct Obj *pull_val(struct linepos_s *);
extern size_t get_val_remaining(void);
extern void destroy_eval(void);
extern void init_eval(void);
extern void eval_enter(void);
extern void eval_leave(void);
extern size_t get_label(void);
extern MUST_CHECK struct Obj *get_star_value(struct Obj *);
extern struct Obj *get_vals_tuple(void);
extern struct Obj *get_vals_addrlist(struct linepos_s *);
extern MUST_CHECK struct Error *indexoffs(struct Obj *, size_t, size_t *, linepos_t);
extern MUST_CHECK struct Obj *sliceparams(const struct List *, size_t, uval_t *, ival_t *, ival_t *, ival_t *, linepos_t);
extern void touch_label(struct Label *);

struct values_s {
    struct Obj *val;
    struct linepos_s epoint;
};
#endif
