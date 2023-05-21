/*
    $Id: encoding.h 2981 2023-05-16 19:30:41Z soci $

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
#ifndef ENCODING_H
#define ENCODING_H
#include "stdbool.h"

struct Enc;
struct str_t;
struct linepos_s;

extern const char *identmap;

extern struct Enc *new_encoding(const struct str_t *, const struct linepos_s *);
extern void init_encoding(bool);
extern void destroy_encoding(void);
#endif
