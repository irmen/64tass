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
#ifndef OPT_BIT_H
#define OPT_BIT_H
#include "inttypes.h"
#include "stdbool.h"

struct Bit;

#define BU 0
#define B0 1
#define B1 2

extern struct Bit *new_bit0(void);
extern struct Bit *new_bit1(void);
extern struct Bit *new_bitu(void);
extern struct Bit *new_bit(uint8_t);
extern void del_bit(struct Bit *);
extern struct Bit *ref_bit(struct Bit *);
extern struct Bit *inv_bit(struct Bit *);
extern void mod_bit(struct Bit *, uint8_t);
extern uint8_t get_bit(const struct Bit *);
extern void reset_bit(struct Bit **);
extern void reset_reg8(struct Bit **);
extern bool eq_bit(const struct Bit *, const struct Bit *);
extern bool neq_bit(const struct Bit *, const struct Bit *);
extern struct Bit *add_bit(struct Bit *, struct Bit *, struct Bit *, struct Bit **);
extern struct Bit *ld_bit(struct Bit *, struct Bit *);
extern struct Bit *and_bit(struct Bit *, struct Bit *);
extern struct Bit *or_bit(struct Bit *, struct Bit *);
extern struct Bit *xor_bit(struct Bit *, struct Bit *);
extern void init_opt_bit(void);
extern void destroy_opt_bit(void);

#endif
