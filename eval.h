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
#ifndef EVAL_H
#define EVAL_H
#include "values.h"

extern int get_exp(int *, int);
extern int get_exp_var(void);
extern struct value_s *get_val(enum type_e, linepos_t *);
extern void destroy_eval(void);
extern void init_eval(void);
extern void eval_enter(void);
extern void eval_leave(void);
extern int eval_finish(void);
extern uint_fast16_t petascii(size_t *, const struct value_s *);
extern int str_to_num(const struct value_s *, enum type_e, struct value_s *, linepos_t);
extern struct value_s error_value;
extern inline uint8_t get_val_len2(const struct value_s *);
extern size_t get_label(void);
extern int val_equals(struct value_s *, struct value_s *, linepos_t, linepos_t, linepos_t);
extern int val_inlist(struct value_s *, struct value_s *, linepos_t, linepos_t, linepos_t);
extern struct value_s *get_vals_tuple(enum type_e);

struct values_s {
    struct value_s *val;
    linepos_t epoint;
};
#endif
