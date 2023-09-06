/*
    $Id: listing.h 3112 2023-09-06 06:34:22Z soci $

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
#ifndef LISTING_H
#define LISTING_H
#include "attributes.h"
#include "inttypes.h"
#include "stdbool.h"
struct cpu_s;
struct Obj;
struct file_s;
struct list_output_s;

extern bool listing_pccolumn;
extern unsigned int nolisting;
extern const uint8_t *llist;
extern void listing_open(const struct list_output_s *, int, char *[]);
extern void listing_close(const struct list_output_s *);
extern FAST_CALL void listing_equal(struct Obj *);
extern FAST_CALL void listing_equal2(struct Obj *, linecpos_t);
extern FAST_CALL void listing_line(linecpos_t);
extern FAST_CALL void listing_line_cut(linecpos_t);
extern FAST_CALL void listing_line_cut2(linecpos_t);
extern void listing_instr(unsigned int, uint32_t, int);
extern void listing_mem(const uint8_t *, size_t, address_t, address_t);
extern void listing_file(const char *, const struct file_s *);
#endif
