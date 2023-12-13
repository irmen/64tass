/*
    $Id: eval.h 3122 2023-09-16 10:44:56Z soci $

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
#include "oper_e.h"

struct Label;
struct Oper;
struct Obj;
struct Funcargs;

struct oper_s {
    Oper_types op;
    struct Obj *v1;
    struct Obj *v2;
    linepos_t epoint;
    linepos_t epoint2;
    linepos_t epoint3;
    struct Obj *inplace;
};
typedef struct oper_s *oper_t;

struct values_s {
    struct Obj *val;
    struct linepos_s epoint;
};

struct argpos_s;

extern bool get_exp(int, argcount_t, argcount_t, linepos_t);
extern struct values_s *get_val(void);
extern struct Obj *pull_val(void);
extern argcount_t get_val_remaining(void);
extern void destroy_eval(void);
extern void init_eval(void);
extern void eval_enter(void);
extern void eval_leave(void);
extern FAST_CALL size_t get_label(const uint8_t *);
extern FAST_CALL size_t get_label2(const uint8_t *, const uint8_t *);
extern MUST_CHECK struct Obj *get_star_value(address_t, struct Obj *);
extern MUST_CHECK struct Obj *get_star(void);
extern MUST_CHECK struct Obj *get_vals_tuple(void);
extern void get_vals_funcargs(struct Funcargs *f);
extern void touch_label(struct Label *);
extern MUST_CHECK struct Obj *calc2_lxor(oper_t, bool);
#endif
