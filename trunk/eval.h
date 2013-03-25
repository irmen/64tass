#ifndef EVAL_H
#define EVAL_H
#include "misc.h"
#include "values.h"

extern int get_exp(int *wd, int);
extern struct value_s *get_val(enum type_e, linepos_t *);
extern void destroy_eval(void);
extern void init_eval(void);
extern void eval_enter(void);
extern void eval_leave(void);
extern int eval_finish(void);
extern uint_fast16_t petascii(size_t *, const struct value_s *);
extern int str_to_num(const struct value_s *, enum type_e, struct value_s *);
extern struct value_s error_value;
extern inline uint8_t get_val_len2(const struct value_s *);
extern size_t get_label(void);

struct values_s {
    struct value_s *val;
    linepos_t epoint;
};
#endif
