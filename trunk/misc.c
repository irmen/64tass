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

#define _MISC_C_
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libtree.h"
#include "misc.h"
#include "opcodes.h"
#include "getopt.h"
#include "error.h"

struct arguments_s arguments={1,1,0,0,0,1,1,0,0,1,0,"a.out",OPCODES_6502,NULL,NULL};

static struct avltree macro_tree;
static struct avltree jump_tree;
static struct avltree file_tree;
struct label_s root_label;
struct label_s *current_context = &root_label;
struct section_s root_section;
struct section_s *current_section = &root_section;
unsigned int encoding;

const uint8_t whatis[256]={
    WHAT_EOL,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_HASHMARK,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_STAR,WHAT_EXPRESSION,WHAT_COMA,WHAT_EXPRESSION,WHAT_COMMAND,0,
    WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_COMMENT,WHAT_EXPRESSION,WHAT_EQUAL,WHAT_EXPRESSION,0,
    WHAT_EXPRESSION,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_EXPRESSION,0,0,0,WHAT_LBL,
    0,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

//------------------------------------------------------------------------------

uint_fast8_t encode(uint_fast8_t ch) {
    switch (encoding) {
    case 1:
        if (ch<=0x1F) return ch+0x80;
        if (ch>=0x20 && ch<=0x3F) return ch;
        if (ch>=0x40 && ch<=0x5F) return ch-0x40;
        if (ch>=0x60 && ch<=0x7F) return ch-0x20;
        if (ch>=0x80 && ch<=0x9F) return ch;
        if (ch>=0xA0 && ch<=0xBF) return ch-0x40;
        if (ch>=0xC0 && ch<=0xFE) return ch-0x80;
        if (ch==0xFF) return 0x5E;
    }
    return ch;
}

struct encoding_s no_encoding[] = {
    {-1, -1, 1},
    {0x00, 0xff, 0x00},
};

struct encoding_s screen_encoding[] = {
    {-1, -1, 8},
    {0x00, 0x1F, 0x80},
    {0x20, 0x3F, 0x20},
    {0x40, 0x5F, 0x00},
    {0x60, 0x7F, 0x40},
    {0x80, 0x9F, 0x80},
    {0xA0, 0xBF, 0x60},
    {0xC0, 0xFE, 0x40},
    {0xFF, 0xFF, 0x5E},
};

struct encoding_s ascii_encoding[] = {
    {-1, -1, 53},
    {0x20, 0x40, 0x20},// -@
    {0x41, 0x5a, 0xc1},//A-Z
    {0x5b, 0x5b, 0x5b},//[
    {0x5d, 0x5d, 0x5d},//]
    {0x61, 0x7a, 0x41},//a-z
    {0x7b, 0x7b, -0x7d},// {code}
    {0xa3, 0xa3, 0x5c},// £
    {0x03c0, 0x03c0, 0xff},// π
    {0x2190, 0x2190, 0x5f},// ←
    {0x2191, 0x2191, 0x5e},// ↑
    {0x2500, 0x2500, 0xc0},// ─
    {0x2502, 0x2502, 0xdd},// │
    {0x250c, 0x250c, 0xb0},// ┌
    {0x2510, 0x2510, 0xae},// ┐
    {0x2514, 0x2514, 0xad},// └
    {0x2518, 0x2518, 0xbd},// ┘
    {0x251c, 0x251c, 0xab},// ├
    {0x2524, 0x2524, 0xb3},// ┤
    {0x252c, 0x252c, 0xb2},// ┬
    {0x2534, 0x2534, 0xb1},// ┴
    {0x253c, 0x253c, 0xdb},// ┼
    {0x256d, 0x256d, 0xd5},// ╭
    {0x256e, 0x256e, 0xc9},// ╮
    {0x256f, 0x256f, 0xcb},// ╯
    {0x2570, 0x2570, 0xca},// ╰
    {0x2571, 0x2571, 0xce},// ╱
    {0x2572, 0x2572, 0xcd},// ╲
    {0x2573, 0x2573, 0xd6},// ╳
    {0x2581, 0x2581, 0xa4},// ▁
    {0x2582, 0x2582, 0xaf},// ▂
    {0x2583, 0x2583, 0xb9},// ▃
    {0x2584, 0x2584, 0xa2},// ▄
    {0x258c, 0x258c, 0xa1},// ▌
    {0x258d, 0x258d, 0xb5},// ▍
    {0x258e, 0x258e, 0xb4},// ▎
    {0x258f, 0x258f, 0xa5},// ▏
    {0x2592, 0x2592, 0xa6},// ▒
    {0x2594, 0x2594, 0xa3},// ▔
    {0x2595, 0x2595, 0xa7},// ▕
    {0x2596, 0x2596, 0xbb},// ▖
    {0x2597, 0x2597, 0xac},// ▗
    {0x2598, 0x2598, 0xbe},// ▘
    {0x259a, 0x259a, 0xbf},// ▚
    {0x259d, 0x259d, 0xbc},// ▝
    {0x25cb, 0x25cb, 0xd7},// ○
    {0x25cf, 0x25cf, 0xd1},// ●
    {0x25e4, 0x25e4, 0xa9},// ◤
    {0x25e5, 0x25e5, 0xdf},// ◥
    {0x2660, 0x2660, 0xc1},// ♠
    {0x2663, 0x2663, 0xd8},// ♣
    {0x2665, 0x2665, 0xd3},// ♥
    {0x2666, 0x2666, 0xda},// ♦
    {0x2713, 0x2713, 0xba},// ✓
};

/* PETSCII codes, must be sorted */
static const char *petsym[] = {
    "\x07" "bell", /* c128 */
    "\x90" "black",
    "\x90" "blk",
    "\x1f" "blu",
    "\x1f" "blue",
    "\x95" "brn",
    "\x95" "brown",
    "\xa8" "cbm-pound",
    "\xde" "cbm-up arrow",
    "\x93" "clear",
    "\x93" "clr",
    "\x06" "control-left arrow",
    "\x1c" "control-pound",
    "\x1e" "control-up arrow",
    "\x0d" "cr",
    "\x9f" "cyan",
    "\x9f" "cyn",
    "\x14" "del",
    "\x14" "delete",
    "\x08" "dish",
    "\x11" "down",
    "\x09" "ensh",
    "\x1b" "esc",
    "\x85" "f1",
    "\x82" "f10", /* dolphin dos */
    "\x84" "f11", /* dolphin dos */
    "\x8f" "f12", /* dolphin dos */
    "\x89" "f2",
    "\x86" "f3",
    "\x8a" "f4",
    "\x87" "f5",
    "\x8b" "f6",
    "\x88" "f7",
    "\x8c" "f8",
    "\x80" "f9", /* dolphin dos */
    "\x1e" "green",
    "\x97" "gray1",
    "\x98" "gray2",
    "\x9b" "gray3",
    "\x97" "grey1",
    "\x98" "grey2",
    "\x9b" "grey3",
    "\x1e" "grn",
    "\x97" "gry1",
    "\x98" "gry2",
    "\x9b" "gry3",
    "\x84" "help", /* c128 help */
    "\x13" "home",
    "\x94" "insert",
    "\x94" "inst",
    "\x9a" "lblu",
    "\x9d" "left",
    "\x5f" "left arrow",
    "\x0a" "lf", /* c128 */
    "\x99" "lgrn",
    "\x0e" "lower case",
    "\x96" "lred",
    "\x9a" "lt blue",
    "\x99" "lt green",
    "\x96" "lt red",
    "\x81" "orange",
    "\x81" "orng",
    "\xff" "pi",
    "\x5c" "pound",
    "\x9c" "pur",
    "\x9c" "purple",
    "\x1c" "red",
    "\x0d" "return",
    "\x92" "reverse off",
    "\x12" "reverse on",
    "\x1d" "rght",
    "\x1d" "right",
    "\x83" "run",
    "\x92" "rvof",
    "\x12" "rvon",
    "\x92" "rvs off",
    "\x12" "rvs on",
    "\x8d" "shift return",
    "\xa9" "shift-pound",
    "\xa0" "shift-space",
    "\xde" "shift-up arrow",
    "\x20" "space",
    "\x8d" "sret",
    "\x03" "stop",
    "\x0e" "swlc",
    "\x8e" "swuc",
    "\x09" "tab", /* c128 */
    "\x91" "up",
    "\x5e" "up arrow",
    "\x09" "up/lo lock off",
    "\x08" "up/lo lock on",
    "\x8e" "upper case",
    "\x05" "white",
    "\x05" "wht",
    "\x9e" "yel",
    "\x9e" "yellow",
};

static const unsigned char petsymcbm[26] = {
    0xb0, 0xbf, 0xbc, 0xac, 0xb1, 0xbb, 0xa5, 0xb4,
    0xa2, 0xb5, 0xa1, 0xb6, 0xa7, 0xaa, 0xb9, 0xaf,
    0xab, 0xb2, 0xae, 0xa3, 0xb8, 0xbe, 0xb3, 0xbd,
    0xb7, 0xad 
};

uint_fast16_t petsymbolic(const char *str) {
    int n, n2;
    int also=0,felso,s4,elozo;

    if (str[0] == '$') { /* convert {$ab} or {$a} */
        n = lowcase(str[1]);
        if ((n >= '0' && n <= '9') ||
            (n >= 'a' && n <= 'f')) {
            n -= (n <= '9') ? '0' : 'a' - 10;
            n2 = lowcase(str[2]);
            if (n2 == 0) return n;
            if ((n2 >= '0' && n2 <= '9') ||
                (n2 >= 'a' && n2 <= 'f')) {
                n2 -= (n2 <= '9') ? '0' : 'a' - 10;
                if (str[3] == 0) {
                    return (n << 4) | n2;
                }
            }
        }
    }
    if (str[0] >= '0' && str[0] <= '9') { /* convert {255} */
        n = str[0] - '0';
        if (!str[1]) return (uint8_t)n;
        if (str[1] >= '0' && str[1] <= '9') {
            n = (n * 10) + str[1] - '0';
            if (!str[2]) return (uint8_t)n;
            if (str[2] >= '0' && str[2] <= '9') {
                n = (n * 10) + str[2] - '0';
                if (n < 256) return (uint8_t)n;
            }
        }
    }
    if (!strncasecmp(str, "cbm-", 4) && str[4] && str[5] == 0) {
        n=lowcase(str[4]);/* {cbm-x} */
        if (n >='a' && n <='z') return petsymcbm[n - 'a']; 
        if (n >='2' && n <='8') return (uint8_t)(n - '2' + 0x95); 
        switch (n) {
        case '0': return '0';
        case '1': return 0x81;
        case '9': return ')';
        case '@': return 0xa4;
        case '+': return 0xa6;
        case '-': return 0xdc;
        case '^': return 0xde;
        case '*': return 0xdf;
        }
    }
    if (!strncasecmp(str, "shift-", 6) && str[6] && str[7] == 0) {
        n = lowcase(str[6]); /* {shift-x} */
        if (n >='a' && n <='z') return (uint8_t)(n - 'a' + 0xc1);
        if (n >='1' && n <='9') return (uint8_t)(n - '0' + 0x20);
        switch (n) {
        case '0': return '0';
        case '*': return 0xc0;
        case '+': return 0xdb;
        case '^': return 0xde;
        case '-': return 0xdd;
        case '@': return 0xba;
        case ',': return '<';
        case '.': return '>';
        case '/': return '?';
        case ':': return '[';
        case ';': return ']';
        }
    }
    if (!strncasecmp(str, "control-", 8) && str[8] && str[9] == 0) {
        n = lowcase(str[8]);
        if (n >='a' && n <= 'z') return (uint8_t)(n - 'a' + 1); /* {control-x} */
        switch (n) {
        case ':': return 0x1b;
        case ';': return 0x1d;
        case '=': return 0x1f;
        case '@': return 0x00;
        case '0': return 0x92;
        case '1': return 0x90;
        case '2': return 0x05;
        case '3': return 0x1c;
        case '4': return 0x9f;
        case '5': return 0x9c;
        case '6': return 0x1e;
        case '7': return 0x1f;
        case '8': return 0x9e;
        case '9': return 0x12;
        }
    }

    felso=sizeof(petsym)/sizeof(petsym[0]);
    n=felso/2;
    for (;;) {  // do binary search
        if (!(s4=strcasecmp(str, petsym[n] + 1))) {
            return (uint8_t)petsym[n][0];
        }

        elozo = n;
        n = ((s4>0) ? (felso+(also=n)) : (also+(felso=n)))/2;
        if (elozo == n) break;
    }
    return 256;
}

//----------------------------------------------------------------------
static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    struct label_s *b = avltree_container_of(bb, struct label_s, node);

    return strcmp(a->name, b->name);
}

static int section_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct section_s *a = avltree_container_of(aa, struct section_s, node);
    struct section_s *b = avltree_container_of(bb, struct section_s, node);

    return strcmp(a->name, b->name);
}

static int macro_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct macro_s *a = avltree_container_of(aa, struct macro_s, node);
    struct macro_s *b = avltree_container_of(bb, struct macro_s, node);

    return strcmp(a->name, b->name);
}

static int file_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct file_s *a = avltree_container_of(aa, struct file_s, node);
    struct file_s *b = avltree_container_of(bb, struct file_s, node);

    return strcmp(a->name, b->name);
}

static int jump_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct jump_s *a = avltree_container_of(aa, struct jump_s, node);
    struct jump_s *b = avltree_container_of(bb, struct jump_s, node);

    return strcmp(a->name, b->name);
}

static int star_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct star_s *a = avltree_container_of(aa, struct star_s, node);
    struct star_s *b = avltree_container_of(bb, struct star_s, node);

    return a->line - b->line;
}

static void label_free(const struct avltree_node *aa)
{
    struct label_s *a = avltree_container_of(aa, struct label_s, node);
    free((char *)a->name);
    avltree_destroy(&a->members);
    if (a->value.type == T_STR) free(a->value.u.str.data);
    free(a);
}

static void section_free(const struct avltree_node *aa)
{
    struct section_s *a = avltree_container_of(aa, struct section_s, node);
    free((char *)a->name);
    avltree_destroy(&a->members);
    free(a);
}

static void macro_free(const struct avltree_node *aa)
{
    struct macro_s *a = avltree_container_of(aa, struct macro_s, node);
    free((char *)a->name);
    free(a);
}

static void file_free(const struct avltree_node *aa)
{
    struct file_s *a = avltree_container_of(aa, struct file_s, node);

    avltree_destroy(&a->star);
    free((char *)a->data);
    free((char *)a->name);
    free(a);
}

static void jump_free(const struct avltree_node *aa)
{
    struct jump_s *a = avltree_container_of(aa, struct jump_s, node);

    free((char *)a->name);
    free(a);
}

static void star_free(const struct avltree_node *aa)
{
    struct star_s *a = avltree_container_of(aa, struct star_s, node);

    avltree_destroy(&a->tree);
    free(a);
}

// ---------------------------------------------------------------------------
struct label_s *find_label(const char* name) {
    const struct avltree_node *b;
    struct label_s *context = current_context;
    struct label_s tmp;
    tmp.name=name;
    
    while (context) {
        b=avltree_lookup(&tmp.node, &context->members);
        if (b) return avltree_container_of(b, struct label_s, node);
        context = context->parent;
    }
    return NULL;
}

struct label_s *find_label2(const char* name, const struct avltree *tree) {
    const struct avltree_node *b;
    struct label_s tmp;
    tmp.name=name;
    b=avltree_lookup(&tmp.node, tree);
    if (!b) return NULL;
    return avltree_container_of(b, struct label_s, node);
}

// ---------------------------------------------------------------------------
static struct label_s *lastlb=NULL;
struct label_s *new_label(const char* name, enum label_e type) {
    const struct avltree_node *b;
    struct label_s *tmp;
    if (!lastlb)
	if (!(lastlb=malloc(sizeof(struct label_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastlb->name=name;
    b=avltree_insert(&lastlb->node, &current_context->members);
    if (!b) { //new label
	if (!(lastlb->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy((char *)lastlb->name,name);
        lastlb->type = type;
        lastlb->parent=current_context;
        lastlb->ref=lastlb->size=lastlb->esize=0;
        avltree_init(&lastlb->members, label_compare, label_free);
	labelexists=0;
	tmp=lastlb;
	lastlb=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct label_s, node);            //already exists
}

// ---------------------------------------------------------------------------
static struct section_s *lastsc=NULL;
struct section_s *new_section(const char* name) {
    const struct avltree_node *b;
    struct section_s *tmp;
    if (!lastsc)
	if (!(lastsc=malloc(sizeof(struct section_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastsc->name=name;
    b=avltree_insert(&lastsc->node, &current_section->members);
    if (!b) { //new label
	if (!(lastsc->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy((char *)lastsc->name,name);
        lastsc->parent=current_section;
        lastsc->provides=~(uval_t)0;lastsc->requires=lastsc->conflicts=0;
        lastsc->address=lastsc->l_address=0;
        avltree_init(&lastsc->members, section_compare, section_free);
	labelexists=0;
	tmp=lastsc;
	lastsc=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct section_s, node);            //already exists
}
// ---------------------------------------------------------------------------

struct jump_s *find_jump(const char* name) {
    struct jump_s a;
    const struct avltree_node *c;
    a.name=name;
    if (!(c=avltree_lookup(&a.node, &jump_tree))) return NULL;
    return avltree_container_of(c, struct jump_s, node);
}

static struct jump_s *lastjp=NULL;
struct jump_s *new_jump(const char* name) {
    const struct avltree_node *b;
    struct jump_s *tmp;
    if (!lastjp)
	if (!(lastjp=malloc(sizeof(struct jump_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastjp->name=name;
    b=avltree_insert(&lastjp->node, &jump_tree);
    if (!b) { //new label
	if (!(lastjp->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy((char *)lastjp->name,name);
	labelexists=0;
	tmp=lastjp;
	lastjp=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct jump_s, node);            //already exists
}

static struct star_s *lastst=NULL;
struct star_s *new_star(line_t line) {
    const struct avltree_node *b;
    struct star_s *tmp;
    if (!lastst)
	if (!(lastst=malloc(sizeof(struct star_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastst->line=line;
    b=avltree_insert(&lastst->node, star_tree);
    if (!b) { //new label
	labelexists=0;
        avltree_init(&lastst->tree, star_compare, star_free);
	tmp=lastst;
	lastst=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct star_s, node);            //already exists
}
// ---------------------------------------------------------------------------

struct macro_s *find_macro(const char* name) {
    struct macro_s a;
    const struct avltree_node *c;
    a.name=name;
    if (!(c=avltree_lookup(&a.node, &macro_tree))) return NULL;
    return avltree_container_of(c, struct macro_s, node);
}

struct section_s *find_section(const char* name) {
    const struct avltree_node *b;
    struct section_s *context = current_section;
    struct section_s tmp;
    tmp.name=name;
    
    while (context) {
        b=avltree_lookup(&tmp.node, &context->members);
        if (b) return avltree_container_of(b, struct section_s, node);
        context = context->parent;
    }
    return NULL;
}

// ---------------------------------------------------------------------------
static struct macro_s *lastma=NULL;
struct macro_s *new_macro(const char* name) {
    const struct avltree_node *b;
    struct macro_s *tmp;
    if (!lastma)
	if (!(lastma=malloc(sizeof(struct macro_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastma->name=name;
    b=avltree_insert(&lastma->node, &macro_tree);
    if (!b) { //new macro
	if (!(lastma->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy((char *)lastma->name,name);
	labelexists=0;
	tmp=lastma;
	lastma=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct macro_s, node);            //already exists
}
// ---------------------------------------------------------------------------
unsigned int utf8in(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    int i, j;
    ch = c[0];

    if (ch < 0xe0) {
        ch ^= 0xc0;i = 2;
    } else if (ch < 0xf0) {
        ch ^= 0xe0;i = 3;
    } else if (ch < 0xf8) {
        ch ^= 0xf0;i = 4;
    } else if (ch < 0xfc) {
        ch ^= 0xf8;i = 5;
    } else {
        ch ^= 0xfc;i = 6;
    }

    for (j = 1;j < i; j++) {
        ch = (ch << 6) ^ c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

static uint8_t *utf8out(uint32_t i, uint8_t *c) {
    if (!i) {
        *c++=0xc0;
        *c++=0x80;
	return c;
    }
    if (i < 0x80) {
        *c++=i;
	return c;
    }
    if (i < 0x800) {
        *c++=0xc0 | (i >> 6);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x10000) {
        *c++=0xe0 | (i >> 12);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x200000) {
        *c++=0xf0 | (i >> 18);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x4000000) {
        *c++=0xf0 | (i >> 24);
        *c++=0x80 | ((i >> 18) & 0x3f);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i & ~0x7fffffff) return c;
    *c++=0xf0 | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

static struct file_s *lastfi=NULL;
static uint16_t curfnum=1;
struct file_s *openfile(const char* name) {
    const struct avltree_node *b;
    struct file_s *tmp;
    if (!lastfi)
	if (!(lastfi=malloc(sizeof(struct file_s)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastfi->name=name;
    b=avltree_insert(&lastfi->node, &file_tree);
    if (!b) { //new file
	enum {UNKNOWN, UTF8, UTF16LE, UTF16BE, ISO1} type = UNKNOWN;
        int ch;
        FILE *f;

	if (!(lastfi->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy((char *)lastfi->name, name);
	lastfi->data=NULL;
	lastfi->len=0;
	lastfi->p=0;
        lastfi->open=0;
        avltree_init(&lastfi->star, star_compare, star_free);
        tmp = lastfi;
        lastfi=NULL;
        if (name[0]) {
            if (name[0]=='-' && !name[1]) f=stdin;
            else f=fopen(name,"rb");
            if (!f) {
                lastfi=tmp;
                err_msg(ERROR_CANT_FINDFILE,name);
                return NULL;
            }
            if (arguments.quiet) printf("Assembling file:   %s\n",name);
            ch=getc(f);
            ungetc(ch, f); 
            if (!ch) type=UTF16BE; /* most likely */

            do {
                int i, j, ch2;
                uint8_t *pline;
                uint32_t c = 0, lastchar;

                if (tmp->p + linelength > tmp->len) {
                    tmp->len += linelength * 2;
                    tmp->data=realloc(tmp->data, tmp->len);
                }
                pline=&tmp->data[tmp->p];
                for (;;) {
                    lastchar = c;
                    if (arguments.toascii) {
                        switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            c = ch = getc(f);
                            if (ch == EOF) break;

                            if (ch < 0x80) goto done;
                            if (ch < 0xc0) {
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                c = 0xfffd; break;
                            } else if (ch < 0xe0) {
                                c ^= 0xc0;i = 1;
                            } else if (ch < 0xf0) {
                                c ^= 0xe0;i = 2;
                            } else if (ch < 0xf8) {
                                c ^= 0xf0;i = 3;
                            } else if (ch < 0xfc) {
                                c ^= 0xf8;i = 4;
                            } else if (ch < 0xfe) {
                                c ^= 0xfc;i = 5;
                            } else {
                                if (type == UNKNOWN) {
                                    ch2=getc(f);
                                    if (ch == 0xff && ch2 == 0xfe) {
                                        type = UTF16LE;continue;
                                    }
                                    if (ch == 0xfe && ch2 == 0xff) {
                                        type = UTF16BE;continue;
                                    }
                                    ungetc(ch2, f); 
                                    type = ISO1; break;
                                }
                                c = 0xfffd; break;
                            }

                            for (j = i; i; i--) {
                                ch2 = getc(f);
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == UNKNOWN) {
                                        type = ISO1;
                                        i = (j - i) * 6;
                                        pline = utf8out(((~0x7f >> j) & 0xff) | (c >> i), pline);
                                        for (;i; i-= 6) {
                                            pline = utf8out(((c >> (i-6)) & 0x3f) | 0x80, pline);
                                        }
                                        c = ch2; j = 0;
                                        break;
                                    }
                                    ungetc(ch2, f);
                                    c = 0xfffd;break;
                                }
                                c = (c << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            break;
                        case UTF16LE:
                            c = getc(f);
                            ch = getc(f);
                            if (ch == EOF) break;
                            c |= ch << 8;
                            if (c == 0xfffe) {
                                type = UTF16BE;
                                continue;
                            }
                            break;
                        case UTF16BE:
                            c = getc(f) << 8;
                            ch = getc(f);
                            if (ch == EOF) break;
                            c |= ch;
                            if (c == 0xfffe) {
                                type = UTF16LE;
                                continue;
                            }
                            break;
                        case ISO1:
                            c = ch = getc(f);
                            goto done;
                        }
                        if (c == 0xfeff) continue;
                        if (type != UTF8) {
                            if (c >= 0xd800 && c < 0xdc00) {
                                if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                                c = 0xfffd;
                            } else if (c >= 0xdc00 && c < 0xe000) {
                                if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                    c ^= 0x361dc00 ^ (lastchar << 10);
                                } else
                                    c = 0xfffd;
                            } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                c = 0xfffd;
                            }
                        }
                    } else c = ch = getc(f);

                    if (ch == EOF) break;
                done:
                    if (c == 10) {
                        if (lastchar == 13) continue;
                        break;
                    } else if (c == 13) {
                        break;
                    }
                    if (c && c < 0x80) *pline++ = c; else pline = utf8out(c, pline);
                    if (pline > &tmp->data[tmp->p + linelength - 6*6]) {
                        err_msg(ERROR_LINE_TOO_LONG,NULL);ch=EOF;break;
                    }
                }
                i = pline - &tmp->data[tmp->p];
                pline=&tmp->data[tmp->p];
                if (i)
                    while (i && pline[i-1]==' ') i--;
                pline[i++] = 0;
                tmp->p += i;

                if (ch == EOF) break;
            } while (1);
            if (ferror(f)) err_msg(ERROR__READING_FILE,name);
            if (f!=stdin) fclose(f);
            tmp->len = tmp->p;
            tmp->data=realloc(tmp->data, tmp->len);
        }
	tmp->p = 0;

        tmp->uid=curfnum++;
    } else {
        tmp = avltree_container_of(b, struct file_s, node);
    }
    tmp->open++;
    return tmp;
}

void closefile(struct file_s *f) {
    if (f->open) f->open--;
}

void tfree(void) {
    avltree_destroy(&root_label.members);
    avltree_destroy(&root_section.members);
    avltree_destroy(&macro_tree);
    avltree_destroy(&jump_tree);
    avltree_destroy(&file_tree);
    free(lastfi);
    free(lastma);
    free(lastlb);
    free(lastjp);
    free(lastst);
    free(lastsc);
    err_destroy();
}

void tinit(void) {
    root_label.type = T_NONE;
    root_label.parent = NULL;
    root_label.name = NULL;
    root_section.parent = NULL;
    root_section.name = NULL;
    avltree_init(&root_label.members, label_compare, label_free);
    avltree_init(&root_section.members, section_compare, section_free);
    avltree_init(&macro_tree, macro_compare, macro_free);
    avltree_init(&jump_tree, jump_compare, jump_free);
    avltree_init(&file_tree, file_compare, file_free);
}

void labelprint(void) {
    const struct avltree_node *n;
    const struct label_s *l;
    FILE *flab;

    if (arguments.label) {
        if (arguments.label[0] == '-' && !arguments.label[1]) {
            flab = stdout;
        } else {
            if (!(flab=fopen(arguments.label,"wt"))) err_msg(ERROR_CANT_DUMP_LBL,arguments.label);
        }
        n = avltree_first(&root_label.members);
        while (n) {
            l = avltree_container_of(n, struct label_s, node);            //already exists
            n = avltree_next(n);
            if (strchr(l->name,'-') || strchr(l->name,'+')) continue;
            switch (l->type) {
            case L_VAR: fprintf(flab,"%-15s .var ",l->name);break;
            case L_UNION:
            case L_STRUCT: continue;
            default: fprintf(flab,"%-16s= ",l->name);break;
            }
            switch (l->value.type) {
            case T_CHR:
            case T_UINT:
            case T_SINT:
                {
                    int32_t val;
                    val=l->value.u.num.val;
                    if (val<0) {putc('-', flab);val=-val;}
                    if (val<0x100) fprintf(flab,"$%02x",val);
                    else if (val<0x10000) fprintf(flab,"$%04x",val);
                    else if (val<0x1000000) fprintf(flab,"$%06x",val);
                    else fprintf(flab,"$%08x",val);
                    if (l->pass<pass) {
                        if (val<0x100) fputs("  ", flab);
                        if (val<0x10000) fputs("  ", flab);
                        if (val<0x1000000) fputs("  ", flab);
                        fputs("; *** unused", flab);
                    }
                    break;
                }
            case T_STR:
                {
                    size_t val;
                    for (val=0;val<l->value.u.str.len;val++) {
                        if (val) fputs(", ", flab);
                        fprintf(flab,"$%02x", l->value.u.str.data[val]);
                    }
                    if (l->pass<pass) {
                        fputs("; *** unused", flab);
                    }
                    break;
                }
            default:
                putc('?', flab);
                break;
            }
            putc('\n', flab);
        }
	if (flab != stdout) fclose(flab);
    }
}

// ------------------------------------------------------------------
static const char *short_options= "wqnbWaTCBicxtl:L:msV?o:D:";

static const struct option long_options[]={
    {"no-warn"          , no_argument      , 0, 'w'},
    {"quiet"            , no_argument      , 0, 'q'},
    {"nonlinear"        , no_argument      , 0, 'n'},
    {"nostart"          , no_argument      , 0, 'b'},
    {"wordstart"        , no_argument      , 0, 'W'},
    {"ascii"            , no_argument      , 0, 'a'},
    {"tasm-compatible"  , no_argument      , 0, 'T'},
    {"case-sensitive"   , no_argument      , 0, 'C'},
    {"long-branch"      , no_argument      , 0, 'B'},
    {"m65xx"            , no_argument      , 0,   1},
    {"m6502"            , no_argument      , 0, 'i'},
    {"m65c02"           , no_argument      , 0, 'c'},
    {"m65816"           , no_argument      , 0, 'x'},
    {"m65dtv02"         , no_argument      , 0, 't'},
    {"labels"           , required_argument, 0, 'l'},
    {"list"             , required_argument, 0, 'L'},
    {"no-monitor"       , no_argument      , 0, 'm'},
    {"no-source"        , no_argument      , 0, 's'},
    {"version"          , no_argument      , 0, 'V'},
    {"usage"            , no_argument      , 0,   2},
    {"help"             , no_argument      , 0,   3},
    { 0, 0, 0, 0}
};

int testarg(int argc,char *argv[],struct file_s *fin) {
    int opt, longind;
    enum {UNKNOWN, UTF8, ISO1} type = UNKNOWN;
    
    while ((opt = getopt_long_only(argc, argv, short_options, long_options, &longind)) != -1)
        switch (opt)
        {
            case 'w':arguments.warning=0;break;
            case 'q':arguments.quiet=0;break;
            case 'W':arguments.wordstart=0;break;
            case 'n':arguments.nonlinear=1;break;
            case 'b':arguments.stripstart=1;break;
            case 'a':arguments.toascii=1;break;
            case 'T':arguments.tasmcomp=1;break;
            case 'o':arguments.output=optarg;break;
            case 'D':
                {
                    const uint8_t *c = (uint8_t *)optarg;
                    uint8_t *pline=&fin->data[fin->p], ch2;
                    int i, j;
                    uint32_t ch = 0;

                    if (fin->p + linelength > fin->len) {
                        fin->len += linelength;
                        if (!(fin->data=realloc(fin->data, fin->len))) exit(1);
                    }
                    pline=&fin->data[fin->p];
                    while ((ch=*c++)) {
                        switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            if (ch < 0x80) {
                                i = 0;
                            } else if (ch < 0xc0) {
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                ch = 0xfffd; i = 0;
                            } else if (ch < 0xe0) {
                                ch ^= 0xc0;i = 1;
                            } else if (ch < 0xf0) {
                                ch ^= 0xe0;i = 2;
                            } else if (ch < 0xf8) {
                                ch ^= 0xf0;i = 3;
                            } else if (ch < 0xfc) {
                                ch ^= 0xf8;i = 4;
                            } else if (ch < 0xfe) {
                                ch ^= 0xfc;i = 5;
                            } else {
                                type = ISO1; break;
                            }

                            for (j = i; i; i--) {
                                ch2 = *c++;
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == UNKNOWN) {
                                        type = ISO1;
                                        i = (j - i) * 6;
                                        pline = utf8out(((~0x7f >> j) & 0xff) | (ch >> i), pline);
                                        for (;i; i-= 6) {
                                            pline = utf8out(((ch >> (i-6)) & 0x3f) | 0x80, pline);
                                        }
                                        ch = ch2; j = 0;
                                        break;
                                    }
                                    c--;
                                    ch = 0xfffd;break;
                                }
                                ch = (ch << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            if (ch == 0xfeff) continue;
                        case ISO1:
                            break;
                        }
                        if (ch && ch < 0x80) *pline++ = ch; else pline = utf8out(ch, pline);
                        if (pline > &fin->data[fin->p + linelength - 6*6]) break;
                    }
                    *pline++ = 0;
                    fin->p = pline - fin->data;
                }
            case 'B':arguments.longbranch=1;break;
            case 1:arguments.cpumode=OPCODES_6502;break;
            case 'i':arguments.cpumode=OPCODES_6502i;break;
            case 'c':arguments.cpumode=OPCODES_65C02;break;
            case 'x':arguments.cpumode=OPCODES_65816;break;
            case 't':arguments.cpumode=OPCODES_65DTV02;break;
            case 'l':arguments.label=optarg;break;
            case 'L':arguments.list=optarg;break;
            case 'm':arguments.monitor=0;break;
            case 's':arguments.source=0;break;
            case 'C':arguments.casesensitive=1;break;
            case 2:puts(
	       "Usage: 64tass [-abBCnTqwWcitxms?V] [-D <label>=<value>] [-o <file>]\n"
	       "	[-l <file>] [-L <file>] [--ascii] [--nostart] [--long-branch]\n"
	       "	[--case-sensitive] [--nonlinear] [--tasm-compatible]\n"
	       "	[--quiet] [--no-warn] [--wordstart] [--m65c02] [--m6502]\n"
	       "	[--m65xx] [--m65dtv02] [--m65816] [--labels=<file>]\n"
	       "	[--list=<file>] [--no-monitor] [--no-source] [--help] [--usage]\n"
	       "	[--version] SOURCES");exit(0);

            case 'V':puts("64tass Turbo Assembler Macro V" VERSION);exit(0);
            case 3:
            case '?':if (optopt=='?' || opt==3) { puts(
	       "Usage: 64tass [OPTIONS...] SOURCES\n"
	       "64tass Turbo Assembler Macro V" VERSION "\n"
	       "\n"			
	       "  -a, --ascii		Source is not in PETASCII\n"
	       "  -b, --nostart		Strip starting address\n"
	       "  -B, --long-branch	Automatic bxx *+3 jmp $xxxx\n"
	       "  -C, --case-sensitive	Case sensitive labels\n"
	       "  -D <label>=<value>	Define <label> to <value>\n"
	       "  -n, --nonlinear	Generate nonlinear output file\n"
	       "  -o <file>		Place output into <file>\n"
	       "  -q, --quiet		Display errors/warnings\n"
	       "  -T, --tasm-compatible Enable TASM compatible mode\n"
	       "  -w, --no-warn		Suppress warnings\n"
	       "  -W, --wordstart	Force 2 byte start address\n"
	       "\n"
	       " Target selection:\n"
	       "  -c, --m65c02		CMOS 65C02\n"
	       "  -i, --m6502		NMOS 65xx\n"
	       "      --m65xx		Standard 65xx (default)\n"
	       "  -t, --m65dtv02	65DTV02\n"
	       "  -x, --m65816		W65C816\n"
	       "\n"
	       " Source listing:\n"
	       "  -l, --labels=<file>	List labels into <file>\n"
	       "  -L, --list=<file>	List into <file>\n"
	       "  -m, --no-monitor	Don't put monitor code into listing\n"
	       "  -s, --no-source	Don't put source code into listing\n"
	       "\n"
	       " Misc:\n"
	       "  -?, --help		Give this help list\n"
	       "      --usage		Give a short usage message\n"
	       "  -V, --version		Print program version\n"
	       "\n"
	       "Mandatory or optional arguments to long options are also mandatory or optional\n"
	       "for any corresponding short options.\n"
	       "\n"
	       "Report bugs to <soci" "\x40" "c64.rulez.org>.");exit(0);}
            default:fputs("Try `64tass --help' or `64tass --usage' for more information.\n", stderr);exit(1);
        }
    closefile(fin); fin->len = fin->p; fin->p = 0;
    if (fin->data && !(fin->data=realloc(fin->data, fin->len))) exit(1);
    if (argc <= optind) {
        fputs("Usage: 64tass [OPTIONS...] SOURCES\n"
              "Try `64tass --help' or `64tass --usage' for more information.\n", stderr);exit(1);
    }
    return optind;
}
