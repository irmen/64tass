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
#ifndef _MISC_H_
#define _MISC_H_
#include "inttypes.h"
#ifndef REVISION
#define REVISION "?"
#endif
#undef VERSION
#define VERSION "1.5x" REVISION
#define MAX_PASS 20

struct file_s;
struct file_list_s;
struct label_s;

enum what_e { WHAT_EXPRESSION, WHAT_HASHMARK, WHAT_X, WHAT_Y, WHAT_Z, WHAT_XZ, WHAT_R, WHAT_RZ,
    WHAT_COMMAND, WHAT_EQUAL, WHAT_EOL, WHAT_STAR, WHAT_COMA, WHAT_COLON,
    WHAT_S, WHAT_SZ, WHAT_COMMENT, WHAT_CHAR, WHAT_LBL
};

enum lastl_e { 
    LIST_NONE, LIST_CODE, LIST_DATA, LIST_EQU
};

enum wait_e {
    W_NONE, W_ENDM, W_ENDM2, W_BEND, W_BEND2, W_HERE, W_HERE2, W_ENDU, W_ENDU2,
    W_ENDS, W_ENDS2, W_ENDC, W_ENDP, W_ENDP2, W_NEXT, W_NEXT2, W_SEND, W_SEND2,
    W_PEND, W_FI, W_FI2, W_ENDF, W_ENDF2, W_ENDF3, W_SWITCH, W_SWITCH2
};

static inline char lowcase(char cch) {return (cch<'A' || cch>'Z')?cch:(cch|0x20);}

struct arguments_s {
    unsigned warning:1;
    unsigned quiet:1;
    unsigned nonlinear:1;
    unsigned stripstart:1;
    unsigned toascii:1;
    unsigned monitor:1;
    unsigned source:1;
    unsigned casesensitive:1;
    unsigned longbranch:1;
    unsigned wordstart:1;
    unsigned tasmcomp:1;
    unsigned flat:1;
    const char *output;
    uint8_t cpumode;
    const char *label;
    const char *list;
};

extern line_t sline, vline;
extern linepos_t lpoint; 
extern struct avltree *star_tree;
extern int fixeddig;
extern unsigned int errors,conderrors,warnings;
extern address_t star;
extern const uint8_t *pline, *llist;
extern void status(void);
extern uint16_t reffile;
extern uint32_t backr, forwr;
extern uint8_t pass, max_pass;
extern void new_waitfor(enum wait_e, linepos_t);
extern struct value_s *compile(struct file_list_s *);
extern void var_assign(struct label_s *, struct value_s *, int);

#define ignore() while(pline[lpoint.pos]==0x20 || pline[lpoint.pos]==0x09) lpoint.pos++
#define get() pline[lpoint.pos++]
#define here() pline[lpoint.pos]
#define linelength 4096

extern const uint8_t whatis[256];
extern void tfree(void);
extern void tinit(void);
extern void labelprint(void);
extern void sectionprint(void);
extern int testarg(int,char **,struct file_s *);
extern struct arguments_s arguments;
extern unsigned int utf8in(const uint8_t *, uint32_t *);
extern uint8_t *utf8out(uint32_t, uint8_t *);
extern struct encoding_s *actual_encoding;
extern int str_hash(const str_t *);
extern int str_casehash(const str_t *);
extern int str_cmp(const str_t *, const str_t *);
extern int str_casecmp(const str_t *, const str_t *);
extern void str_cpy(str_t *, const str_t *);

#endif