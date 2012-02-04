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
#define WHAT_COLON      12
#define WHAT_S          13
#define WHAT_SZ         14
#define WHAT_COMMENT    15
#define WHAT_CHAR       16
#define WHAT_LBL        17

typedef uint_fast32_t line_t;
typedef uint_fast32_t address_t;
typedef int32_t ival_t;
typedef uint32_t uval_t;

static inline char lowcase(char cch) {return (cch<'A' || cch>'Z')?cch:(cch|0x20);}

enum type_e {
    T_NONE, T_CHR, T_NUM, T_UINT, T_SINT, T_STR, T_TSTR, T_GAP, T_IDENT, T_FUNC, T_IDENTREF, T_FORWR, T_BACKR, T_UNDEF, T_OPER
};

struct value_s {
    enum type_e type;
    union {
        struct {
            uint8_t len;
            ival_t val;
        } num;
        struct {
            size_t len;
            uint8_t *data;
        } str;
        struct {
            size_t len;
            const uint8_t *name;
        } ident;
        struct label_s *label;
        char oper;
        uint8_t ref;
    } u;
};

enum label_e {
    L_LABEL, L_CONST, L_VAR, L_STRUCT, L_UNION
};

struct label_s {
    const char *name;
    enum label_e type;
    struct avltree_node node;

    struct value_s value;
    size_t size;
    uval_t requires;
    uval_t conflicts;
    unsigned ref:1;
    uint8_t esize;
    uint8_t pass;
    uint8_t upass;
    struct label_s *parent;
    struct avltree members;
};

struct star_s {
    line_t line;
    address_t addr;
    struct avltree tree;
    struct avltree_node node;
};

struct file_s {
    const char *name;
    uint8_t *data;    /* data */
    size_t len;       /* length */
    size_t p;         /* current point */
    uint16_t open;    /* open/not open */
    uint16_t uid;     /* uid */
    struct avltree star;
    struct avltree_node node;
};

struct macro_s {
    const char *name;
    size_t p;
    line_t sline;
    struct file_s *file;
    int type;
    struct avltree_node node;
};

struct jump_s {
    const char *name;
    size_t p;
    line_t sline;
    uint8_t waitforp;
    const struct file_s *file;
    const struct label_s *parent;
    struct avltree_node node;
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
    unsigned tasmcomp:1;
    const char *output;
    uint8_t cpumode;
    const char *label;
    const char *list;
};

struct encoding_s {
    uint32_t start;
    uint32_t end;
    int16_t offset;
};

extern line_t sline, vline;
extern unsigned int lpoint; 
extern struct file_s *cfile; 
extern struct avltree *star_tree;
extern int fixeddig;
extern unsigned int errors,conderrors,warnings;
extern address_t star;
extern const uint8_t *pline;
extern int labelexists;
extern void status(void);
extern uint16_t reffile;
extern uint32_t backr, forwr;
extern uint8_t pass;

#define ignore() while(pline[lpoint]==0x20 || pline[lpoint]==0x09) lpoint++
#define get() pline[lpoint++]
#define here() pline[lpoint]
#define linelength 4096

extern const uint8_t whatis[256];
extern uint_fast8_t encode(uint_fast8_t);
extern uint_fast16_t petsymbolic(const char*);
extern struct label_s *find_label(const char*);
extern struct label_s *find_label2(const char*, const struct avltree *);
extern struct label_s *new_label(const char*, enum label_e);
extern struct macro_s *find_macro(const char*);
extern struct macro_s *new_macro(const char*);
extern struct jump_s *find_jump(const char*);
extern struct jump_s *new_jump(const char*);
extern struct star_s *new_star(line_t);
extern struct file_s *openfile(const char*);
extern void closefile(struct file_s*);
extern void tfree(void);
extern void tinit(void);
extern void labelprint(void);
extern int testarg(int,char **,struct file_s *);
extern struct arguments_s arguments;
extern unsigned int encoding;
extern struct label_s *current_context, root_label;
extern unsigned int utf8in(const uint8_t *c, uint32_t *out);
extern struct encoding_s no_encoding[];
extern struct encoding_s screen_encoding[];
extern struct encoding_s ascii_encoding[];
extern uval_t current_requires, current_conflicts, current_provides;
extern int get_ident(void);

#endif
