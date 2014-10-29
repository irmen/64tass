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
#ifndef _LISTING_H_
#define _LISTING_H_
#include "inttypes.h"
struct cpu_s;

extern unsigned int nolisting;
extern const uint8_t *llist;
extern void listing_open(const char *, int, char *[]);
extern void listing_close(void);
extern void listing_equal(const value_t);
extern void listing_line(linecpos_t);
extern void listing_line_cut(linecpos_t);
extern void listing_line_cut2(linecpos_t);
extern void listing_set_cpumode(const struct cpu_s *);
extern void listing_instr(uint8_t, uint32_t, int);
extern void listing_mem(const uint8_t *, size_t, address_t);
extern void listing_file(const char *, const char *);
#endif
