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
#ifndef _MACRO_H_
#define _MACRO_H_
#include "inttypes.h"
struct value_s;
struct values_s;
struct label_s;
struct file_s;
enum wait_e;

extern int mtranslate(struct file_s *);
extern struct value_s *macro_recurse(enum wait_e, struct value_s *, struct label_s *, linepos_t);
extern struct value_s *func_recurse(enum wait_e, struct value_s *, struct label_s *, linepos_t, struct file_s *);
extern struct value_s *function_recurse(struct value_s *, struct values_s *, unsigned int, linepos_t);
extern void init_macro(void);
extern void free_macro(void);
extern void get_macro_params(struct value_s *);
extern void get_func_params(struct value_s *, struct file_s *);
#endif