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
#ifndef _MISC_H_
#define _MISC_H_
#include "inttypes.h"

enum output_mode_e {
    OUTPUT_CBM, OUTPUT_RAW, OUTPUT_NONLINEAR, OUTPUT_FLAT, OUTPUT_XEX, OUTPUT_APPLE
};

struct file_s;

static inline char lowcase(char cch) {return (cch<'A' || cch>'Z')?cch:(cch|0x20);}

struct arguments_s {
    unsigned warning:1;
    unsigned quiet:1;
    unsigned toascii:1;
    unsigned monitor:1;
    unsigned source:1;
    unsigned casesensitive:1;
    unsigned longbranch:1;
    unsigned longaddr:1;
    unsigned tasmcomp:1;
    const char *output;
    const struct cpu_s *cpumode;
    const char *label;
    const char *list;
    enum output_mode_e output_mode;
    uint8_t tab_size;
};

#define ignore() while(pline[lpoint.pos]==0x20 || pline[lpoint.pos]==0x09) lpoint.pos++
#define here() pline[lpoint.pos]

extern void tfree(void);
extern void tinit(void);
extern int testarg(int,char **,struct file_s *);
extern struct arguments_s arguments;
extern int str_hash(const str_t *);
extern int str_cmp(const str_t *, const str_t *);
extern void str_cfcpy(str_t *, const str_t *);
extern void str_cpy(str_t *, const str_t *);

#endif
