#ifndef EVAL_H
#define EVAL_H
#include "misc.h"
extern int get_exp(int *wd, int);
extern int get_val(struct value_s *, enum type_e, unsigned int *);
extern void free_values(void);
extern void eval_finish(void);
extern void set_uint(struct value_s *v, uval_t val);
extern void set_int(struct value_s *v, ival_t val);

struct values_s {
    struct value_s val;
    unsigned int epoint;
};
#endif
