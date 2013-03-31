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
#ifndef _MEM_H_
#define _MEM_H_
#include <stdio.h>
#include "inttypes.h"
enum lastl_e;

extern void mark_mem(address_t);
extern void write_mark_mem(uint8_t c);
extern void list_mem(FILE *, address_t, int, enum lastl_e *);
extern void memcomp(void);
extern void memjmp(address_t adr);
extern void memprint(void);
extern void output_mem(int);
extern void write_mem(uint8_t);
int16_t read_mem(size_t, size_t, size_t);
extern void get_mem(size_t *, size_t *);
extern void restart_mem(void);
extern void init_mem(void);
extern void destroy_mem(void);
#endif
