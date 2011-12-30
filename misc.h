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
#include "libtree.h"
#include "stdint.h"
#define VERSION "1.46"
// ---------------------------------------------------------------------------
// $00-$3f warning
// $40-$7f error
// $80-$bf fatal
enum errors_e {
    ERROR_TOP_OF_MEMORY=0x00,
    ERROR_A_USED_AS_LBL,
    ERROR___BANK_BORDER,
    ERROR______JUMP_BUG,
    ERROR___LONG_BRANCH,
    ERROR_DIRECTIVE_IGN,
    ERROR_WUSER_DEFINED,

    ERROR_DOUBLE_DEFINE=0x40,
    ERROR___NOT_DEFINED,
    ERROR_EXTRA_CHAR_OL,
    ERROR_CONSTNT_LARGE,
    ERROR_GENERL_SYNTAX,
    ERROR______EXPECTED,
    ERROR_EXPRES_SYNTAX,
    ERROR_BRANCH_TOOFAR,
    ERROR_MISSING_ARGUM,
    ERROR_ILLEGAL_OPERA,
    ERROR_UNKNOWN_ENCOD,
    ERROR_REQUIREMENTS_,
    ERROR______CONFLICT,
    ERROR_DIVISION_BY_Z,
    ERROR____WRONG_TYPE,
    ERROR___UNKNOWN_CHR,
    ERROR____PAGE_ERROR,
    ERROR__BRANCH_CROSS,

    ERROR_CANT_FINDFILE=0x80,
    ERROR_OUT_OF_MEMORY,
    ERROR_CANT_WRTE_OBJ,
    ERROR_LINE_TOO_LONG,
    ERROR_CANT_DUMP_LST,
    ERROR_CANT_DUMP_LBL,
    ERROR__USER_DEFINED,
    ERROR_FILERECURSION,
    ERROR__MACRECURSION,
    ERROR___UNKNOWN_CPU,
    ERROR_UNKNOWN_OPTIO,
    ERROR_TOO_MANY_PASS,
    ERROR__TOO_MANY_ERR
};

#define WHAT_EXPRESSION 1
#define WHAT_HASHMARK   3
#define WHAT_X          4
#define WHAT_Y          5
#define WHAT_XZ         6
#define WHAT_COMMAND    7
#define WHAT_EQUAL      8
#define WHAT_EOL        9
#define WHAT_STAR       10
#define WHAT_COMA       11
#define WHAT_S          13
#define WHAT_SZ         14
#define WHAT_CHAR       16
#define WHAT_LBL        17

static inline char lowcase(char cch) {return (cch<'A' || cch>'Z')?cch:(cch|0x20);}

struct context_s {
    char* name;
    struct context_s *parent;
    uint32_t backr, forwr;
    struct avltree contexts;
    struct avltree label_tree;
    struct avltree jump_tree;
    struct avltree_node node;
};

enum type_e {
    T_NONE=0, T_INT, T_CHR, T_STR, T_TSTR
};

struct value_s {
    enum type_e type;
    union {
        int32_t num;
        struct {
            size_t len;
            uint8_t *data;
        } str;
    } u;
};

struct label_s {
    char* name;
    struct value_s value;
    uint32_t requires;
    uint32_t conflicts;
    unsigned proclabel:1;
    unsigned varlabel:1;
    uint8_t pass;
    uint8_t upass;
    struct avltree_node node;
};

struct file_s {
    char *name;
    uint8_t *data;    /* data */
    size_t len;       /* length */
    size_t p;         /* current point */
    uint16_t open;    /* open/not open */
    uint16_t uid;     /* uid */
    struct avltree_node node;
};

struct macro_s {
    char *name;
    size_t p;
    uint32_t sline;
    struct file_s *file;
    int type;
    struct avltree_node node;
};

struct jump_s {
    char *name;
    size_t p;
    uint32_t sline;
    struct file_s *file;
    struct avltree_node node;
};

struct errorlist_s {
    struct errorlist_s *next;
    char name[1];
};

struct filenamelist_s {
    struct filenamelist_s *next;
    uint32_t line;
    char *name;
};

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
    unsigned noprecedence:1;
    unsigned oldops:1;
    char *output;
    uint8_t cpumode;
    char *label;
    char *list;
};

struct encoding_s {
    uint32_t start;
    uint32_t end;
    int16_t offset;
};

extern uint32_t sline;
extern unsigned int errors,conderrors,warnings;
extern uint32_t l_address;
extern uint8_t pline[];
extern int labelexists;
extern void status();
extern uint16_t reffile;
extern uint8_t pass;

#ifdef _MAIN_C_
#define ignore() while(pline[lpoint]==0x20) lpoint++
#define get() pline[lpoint++]
#define here() pline[lpoint]
#endif
extern const uint8_t whatis[256];
extern uint_fast8_t encode(uint_fast8_t);
extern uint_fast16_t petsymbolic(char*);
extern void err_msg(enum errors_e, char*);
extern struct label_s *find_label(char*);
extern struct label_s *new_label(char*);
extern struct macro_s *find_macro(char*);
extern struct macro_s *new_macro(char*);
extern struct jump_s *find_jump(char*);
extern struct jump_s *new_jump(char*);
extern struct context_s *new_context(char*, struct context_s *);
extern struct file_s *openfile(char*);
extern void closefile(struct file_s*);
extern void tfree();
extern void tinit();
extern void labelprint();
extern int testarg(int,char **,struct file_s *);
extern struct arguments_s arguments;
extern void freeerrorlist(int);
extern void enterfile(char *,uint32_t);
extern void exitfile();
extern struct filenamelist_s *filenamelist;
extern unsigned int encoding;
extern struct context_s *current_context;
extern struct context_s root_context;
extern unsigned int utf8in(uint8_t *c, uint32_t *out);
extern struct encoding_s no_encoding[];
extern struct encoding_s screen_encoding[];
extern struct encoding_s ascii_encoding[];

#endif
