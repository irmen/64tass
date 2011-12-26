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
#ifndef WIN32
#include <argp.h>
#endif
#include "libtree.h"
#include "misc.h"
#include "opcodes.h"
#include <string.h>

void err_msg(unsigned char no, char* prm);

struct arguments_t arguments={1,1,0,0,0,NULL,"a.out",OPCODES_6502,NULL,NULL,1,1,0,0,1,0,0};

static struct avltree macro_tree;
static struct avltree file_tree;
struct scontext root_context;
struct scontext *current_context = &root_context;
struct serrorlist *errorlist=NULL,*errorlistlast=NULL;
struct sfilenamelist *filenamelist=NULL;
int encoding;

unsigned char tolower_tab[256];

const unsigned char whatis[256]={
    WHAT_EOL,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_HASHMARK,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_EXPRESSION,WHAT_EXPRESSION,0,WHAT_STAR,WHAT_EXPRESSION,WHAT_COMA,WHAT_EXPRESSION,WHAT_COMMAND,0,
    WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,WHAT_EXPRESSION,0,0,WHAT_EXPRESSION,WHAT_EQUAL,WHAT_EXPRESSION,0,
    WHAT_EXPRESSION,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,
    WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,WHAT_CHAR,0,0,0,0,WHAT_LBL,
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

unsigned char encode(unsigned char ch) {
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

struct sencoding no_encoding[] = {
    {-1, -1, 1},
    {0x00, 0xff, 0x00},
};

struct sencoding screen_encoding[] = {
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

struct sencoding ascii_encoding[] = {
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

int petsymbolic(char *str) {
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
        if (!str[1]) return (unsigned char)n;
        if (str[1] >= '0' && str[1] <= '9') {
            n = (n * 10) + str[1] - '0';
            if (!str[2]) return (unsigned char)n;
            if (str[2] >= '0' && str[2] <= '9') {
                n = (n * 10) + str[1] - '0';
                if (n < 256) return (unsigned char)n;
            }
        }
    }
    if (!strncasecmp(str, "cbm-", 4) && str[4] && str[5] == 0) {
        n=lowcase(str[4]);/* {cbm-x} */
        if (n >='a' && n <='z') return petsymcbm[n - 'a']; 
        if (n >='2' && n <='8') return (unsigned char)(n - '2' + 0x95); 
        switch (n) {
        case '1': return 0x81;
        case '@': return 0xa4;
        case '+': return 0xa6;
        case '-': return 0xdc;
        case '^': return 0xde;
        case '*': return 0xdf;
        }
    }
    if (!strncasecmp(str, "shift-", 6) && str[6] && str[7] == 0) {
        n = lowcase(str[6]); /* {shift-x} */
        if (n >='a' && n <='z') return (unsigned char)(n - 'a' + 0xc1);
        if (n >='1' && n <='9') return (unsigned char)(n - '0' + 0x20);
        switch (n) {
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
        if (n >='a' && n <= 'z') return (unsigned char)(n - 'a' + 1); /* {control-x} */
        switch (n) {
        case ':': return 0x1b;
        case ';': return 0x1d;
        case '=': return 0x1f;
        case '@': return 0x00;
        case '0': return 0x92;
        case '1': return 0x90;
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
            return (unsigned char)petsym[n][0];
        }

        elozo = n;
        n = ((s4>0) ? (felso+(also=n)) : (also+(felso=n)))/2;
        if (elozo == n) break;
    }
    return -1;
}

//------------------------------------------------------------------------------

void adderror(char *s) {
    struct serrorlist *b;

    b=malloc(sizeof(struct serrorlist)+strlen(s));

    if (!b) {fprintf(stderr,"Out of memory\n");exit(1);}

    b->next=NULL;
    strcpy(b->name,s);

    if (!errorlist)
        errorlist=b;
    else
        errorlistlast->next=b;

    errorlistlast=b;
}

void freeerrorlist(int print) {
    struct serrorlist *b;

    while (errorlist) {
        b=errorlist->next;
        if (print) fprintf(stderr,"%s",errorlist->name);
        free(errorlist);
        errorlist=b;
    }
}

void enterfile(char *s, long l) {
    struct sfilenamelist *b;

    b=malloc(sizeof(struct sfilenamelist)+strlen(s));

    if (!b) {fprintf(stderr,"Out of memory\n");exit(1);}
    b->next=filenamelist;
    strcpy(b->name,s);
    b->line=l;

    filenamelist=b;
}

void exitfile() {
    struct sfilenamelist *b;

    b=filenamelist;
    filenamelist=b->next;
    free(b);
}

#define linelength 4096

const char *terr_warning[]={
	"Top of memory excedeed",
	"Possibly incorrectly used A",
	"Memory bank excedeed",
	"Possible jmp ($xxff) bug",
        "Long branch used",
        "Directive ignored"
//	"%s\n",
};
const char *terr_error[]={
	"Double defined %s",
	"Not defined %s",
	"Extra characters on line",
	"Constant too large",
	"General syntax",
	"%s expected",
	"Expression syntax",
	"Branch too far",
        "Missing argument",
        "Illegal operand",
        "Unknown encoding: %s",
        "Requirements not met: %s",
        "Conflict: %s",
        "Division by zero",
        "Wrong type",
        "Unknown character $%02x",
};
const char *terr_fatal[]={
	"Can't locate file: %s\n",
	"Out of memory\n",
	"Can't write object file: %s\n",
	"Line too long\n",
	"Can't write listing file: %s\n",
	"Can't write label file: %s\n",
	"%s\n",
	"File recursion\n",
	"Macro recursion too deep\n",
        "Unknown CPU: %s\n",
        "Unknown option: %s\n"
};

void err_msg(unsigned char no, char* prm) {
    char line[linelength];
    struct sfilenamelist *b=NULL, *b2=filenamelist;
    char *p;

    if (!arguments.warning && no<0x40) return;

    if (filenamelist) {
	b=filenamelist->next;
	snprintf(line,linelength,"%s:%ld: ",filenamelist->name,sline);
    } else line[0]=0;

    adderror(line);

    while (b) {
        snprintf(line,linelength,"(%s:%ld) ",b->name,b2->line);
        adderror(line);
        b2=b;
        b=b->next;
    }

    if (no<0x40) {
        snprintf(line,linelength,"warning: %s",(no==ERROR_WUSER_DEFINED)?prm:terr_warning[no]);
        warnings++;
    }
    else if (no<0x80) {
        if (no==ERROR____PAGE_ERROR) {
            snprintf(line,linelength,"Page error at $%06lx",l_address);
            conderrors++;
        }
        else if (no==ERROR__BRANCH_CROSS) {
            snprintf(line,linelength,"Branch crosses page");
            conderrors++;
        }
        else {
            snprintf(line,linelength,terr_error[no & 63],prm);
            if (no==ERROR_BRANCH_TOOFAR || no==ERROR_CONSTNT_LARGE) conderrors++;
            else errors++;
        }
    }
    else {
        adderror("[**Fatal**] ");
        snprintf(line,linelength,terr_fatal[no & 63],prm);
        if (no==ERROR__USER_DEFINED) conderrors++; else
        {
            adderror(line);
            errors++;
            status();exit(1);
        }
    }

    adderror(line);
    if (no==ERROR_WUSER_DEFINED && *prm) {
        snprintf(line,linelength,"\n");
    }
    else {
        p=pline;
        while (*p==0x20) p++;

        snprintf(line,linelength," \"%s\"\n",p);
    }
    if (no!=ERROR__USER_DEFINED) adderror(line);

    if (errors==100) {adderror("Too many errors\n"); status(); exit(1);}
}

//----------------------------------------------------------------------
int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct slabel *a = avltree_container_of(aa, struct slabel, node);
    struct slabel *b = avltree_container_of(bb, struct slabel, node);

    return strcmp(a->name, b->name);
}

int context_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct scontext *a = avltree_container_of(aa, struct scontext, node);
    struct scontext *b = avltree_container_of(bb, struct scontext, node);

    return strcmp(a->name, b->name);
}

int macro_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct smacro *a = avltree_container_of(aa, struct smacro, node);
    struct smacro *b = avltree_container_of(bb, struct smacro, node);

    return strcmp(a->name, b->name);
}

int file_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    struct sfile *a = avltree_container_of(aa, struct sfile, node);
    struct sfile *b = avltree_container_of(bb, struct sfile, node);

    return strcmp(a->name, b->name);
}

void label_free(const struct avltree_node *aa)
{
    struct slabel *a = avltree_container_of(aa, struct slabel, node);
    free(a->name);
    if (a->value.type == T_STR) free(a->value.str.data);
    free(a);
}

void context_free(const struct avltree_node *aa)
{
    struct scontext *a = avltree_container_of(aa, struct scontext, node);
    free(a->name);
    avltree_destroy(&a->tree);
    avltree_destroy(&a->contexts);
    free(a);
}

void macro_free(const struct avltree_node *aa)
{
    struct smacro *a = avltree_container_of(aa, struct smacro, node);
    free(a->name);
    free(a);
}

void file_free(const struct avltree_node *aa)
{
    struct sfile *a = avltree_container_of(aa, struct sfile, node);

    free(a->linebuf);
    free(a->name);
    free(a);
}

// ---------------------------------------------------------------------------
struct slabel* find_label(char* name) {
    struct slabel a, *a2;
    struct scontext b;
    const struct avltree_node *c, *d;
    struct scontext *context = current_context, *context2;
    char *n;
    
    while (context) {
        b.name = name;
        context2 = context;
        while ((n=strchr(b.name,'.'))) {
            *n=0;
            c=avltree_lookup(&b.node, &context2->contexts);
            if (c) {
                a.name = b.name;
                d=avltree_lookup(&a.node, &context2->tree);
            }
            *n='.';
            if (!c) break;
            if (d) {
                a2 = avltree_container_of(d, struct slabel, node);
                a2->proclabel = 0; a2->used = 1;
            }
            context2 = avltree_container_of(c, struct scontext, node);
            b.name = n + 1;
        }
        if (context2) {
            a.name = b.name;
            d=avltree_lookup(&a.node, &context2->tree);
            if (d) {
                a2 = avltree_container_of(d, struct slabel, node);
                return a2;
            }
        }
        context = context->parent;
    }
    return NULL;
}


// ---------------------------------------------------------------------------
static struct slabel *lastlb=NULL;
struct slabel* new_label(char* name) {
    struct avltree_node *b;
    struct slabel *tmp;
    if (!lastlb)
	if (!(lastlb=malloc(sizeof(struct slabel)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastlb->name=name;
    b=avltree_insert(&lastlb->node, &current_context->tree);
    if (!b) { //new label
	if (!(lastlb->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy(lastlb->name,name);
	labelexists=0;
	tmp=lastlb;
	lastlb=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct slabel, node);            //already exists
}

// ---------------------------------------------------------------------------
static struct scontext *lastco=NULL;
struct scontext* new_context(char* name, struct scontext *parent) {
    struct avltree_node *b;
    struct scontext *tmp;
    if (!lastco)
	if (!(lastco=malloc(sizeof(struct scontext)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastco->name=name;
    b=avltree_insert(&lastco->node, &current_context->contexts);
    if (!b) { //new context
	if (!(lastco->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy(lastco->name,name);
        avltree_init(&lastco->tree, label_compare, label_free);
        avltree_init(&lastco->contexts, context_compare, context_free);
        lastco->parent=parent;
	labelexists=0;
	tmp=lastco;
	lastco=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct scontext, node);            //already exists
}
// ---------------------------------------------------------------------------

struct smacro* find_macro(char* name) {
    struct smacro a;
    const struct avltree_node *c;
    a.name=name;
    if (!(c=avltree_lookup(&a.node, &macro_tree))) return NULL;
    return avltree_container_of(c, struct smacro, node);
}

// ---------------------------------------------------------------------------
static struct smacro* lastma=NULL;
struct smacro* new_macro(char* name) {
    struct avltree_node *b;
    struct smacro *tmp;
    if (!lastma)
	if (!(lastma=malloc(sizeof(struct smacro)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastma->name=name;
    b=avltree_insert(&lastma->node, &macro_tree);
    if (!b) { //new macro
	if (!(lastma->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy(lastma->name,name);
	labelexists=0;
	tmp=lastma;
	lastma=NULL;
	return tmp;
    }
    labelexists=1;
    return avltree_container_of(b, struct smacro, node);            //already exists
}
// ---------------------------------------------------------------------------
int utf8in(char *c, int *out) { /* only for internal use with validated utf-8! */
    int ch = (unsigned char)c[0], i, j;

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
        ch = (ch << 6) ^ (unsigned char)c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

static unsigned char *utf8out(int i, unsigned char *c) {
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
    if (i & 0x7fffffff) return c;
    *c++=0xf0 | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

static struct sfile *lastfi=NULL;
static unsigned long curfnum=1;
struct sfile* openfile(char* name,char* volt) {
    struct avltree_node *b;
    struct sfile *tmp;
    if (!lastfi)
	if (!(lastfi=malloc(sizeof(struct sfile)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    lastfi->name=name;
    b=avltree_insert(&lastfi->node, &file_tree);
    if (!b) { //new file
	int type = 0, lastchar;
        FILE *f;

	if (!(lastfi->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy(lastfi->name,name);
	lastfi->linebuf=NULL;
	lastfi->linebuflen=0;
	lastfi->currentp=0;
	f=fopen(name,"rb");
        if (!f) {
            lastfi=NULL;
            return NULL;
        }
        lastchar=fgetc(f);
        ungetc(lastchar, f); 
        if (!lastchar) type=2; /* most likely */

	do {
	    int i=0,ch = 0, ch2;
	    unsigned char *pline;
	    if (lastfi->currentp + linelength > lastfi->linebuflen) {
		lastfi->linebuflen += 0x1000;
		lastfi->linebuf=realloc(lastfi->linebuf, lastfi->linebuflen);
	    }
	    pline=&lastfi->linebuf[lastfi->currentp];
	    for (;;) {
		lastchar = ch;
		switch (type) {
		case 0: // utf-8
		    ch=fgetc(f);
		    if (ch < 0) break;

		    if (ch < 0x80) {
			i = 0;
		    } else if (ch < 0xc0) {
			ch = 0xfffd;i = 0;
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
			ch2=fgetc(f);
			if (ch == 0xff && ch2 == 0xfe) {
			    type = 1;continue;
			}
			if (ch == 0xfe && ch2 == 0xff) {
			    type = 2;continue;
			}
                        ungetc(ch2, f); 
			ch = 0xfffd;i = 0;
		    }

		    for (;i;i--) {
			ch2 = fgetc(f);
			if (ch2 < 0x80 || ch2 >= 0xc0) {
			    ungetc(ch2, f); 
			    ch = 0xfffd;
			    break;
			}
			ch = (ch << 6) ^ ch2 ^ 0x80;
		    }
		    break;
		case 1: // utf-16 le
		    ch=fgetc(f);
		    ch2=fgetc(f);
		    if (ch2 == EOF) break;
                    ch |= ch2 << 8;
		    if (ch == 0xfffe) {
			type = 2;
			continue;
		    }
		    break;
		case 2: // utf-16 be
		    ch2=fgetc(f);
		    ch=fgetc(f);
		    if (ch == EOF) break;
                    ch |= ch2 << 8;
		    if (ch == 0xfffe) {
			type = 1;
			continue;
		    }
		    break;
		}
		if (ch == 0xfeff) continue;
		if (type) {
		    if (ch >= 0xd800 && ch < 0xdc00) {
			if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                        ch = 0xfffd;
                    } else if (ch >= 0xdc00 && ch < 0xe000) {
			if (lastchar >= 0xd800 && lastchar < 0xdc00) {
			    ch = 0x361dc00 ^ ch ^ (lastchar << 10);
			} else
			    ch = 0xfffd;
                    } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
			ch = 0xfffd;
		    }
		}

		if (ch == EOF) break;
		if (ch == 10) {
		    if (lastchar == 13) continue;
		    break;
		}
		if (ch == 13) {
		    break;
		}
		pline = utf8out(ch, pline);
		if (pline > &lastfi->linebuf[lastfi->currentp + linelength - 6]) {pline[i]=0;err_msg(ERROR_LINE_TOO_LONG,NULL);break;}
	    }
	    i = pline - &lastfi->linebuf[lastfi->currentp];
	    pline=&lastfi->linebuf[lastfi->currentp];
	    if (i)
		while (i && pline[i-1]==' ') i--;
	    pline[i++] = 0;
	    lastfi->currentp += i;
	
	    if (ch == EOF) break;
	} while (1);
        fclose(f);
	lastfi->linebuflen = lastfi->currentp;
	lastfi->linebuf=realloc(lastfi->linebuf, lastfi->linebuflen);
	lastfi->currentp = 0;

        tmp = lastfi;
	lastfi=NULL;
        *volt=0;
        tmp->num=curfnum++;
    } else {
        tmp = avltree_container_of(b, struct sfile, node);
        *volt=tmp->open;
    }
    tmp->open=1;
    reffile=tmp->num;
    return tmp;
}

void closefile(struct sfile* f) {
    f->open=0;
}

void tfree() {
    avltree_destroy(&root_context.tree);
    avltree_destroy(&root_context.contexts);
    avltree_destroy(&macro_tree);
    avltree_destroy(&file_tree);
    free(lastco);
    free(lastfi);
    free(lastma);
    free(lastlb);
#ifdef WIN32
    free(arguments.input);
    //free(arguments.output);
    free(arguments.list);
    free(arguments.label);
#endif
    while (filenamelist) exitfile(); 
}

void tinit() {
    avltree_init(&root_context.tree, label_compare, label_free);
    avltree_init(&root_context.contexts, context_compare, context_free);
    root_context.parent = NULL;
    avltree_init(&macro_tree, macro_compare, macro_free);
    avltree_init(&file_tree, file_compare, file_free);
}

void labelprint() {
    struct avltree_node *n;
    long val;
    struct slabel *l;
    FILE *flab;

    if (arguments.label) {
        if (arguments.label[0] == '-' && !arguments.label[1]) {
            flab = stdout;
        } else {
            if (!(flab=fopen(arguments.label,"wt"))) err_msg(ERROR_CANT_DUMP_LBL,arguments.label);
        }
        n = avltree_first(&root_context.tree);
        while (n) {
            l = avltree_container_of(n, struct slabel, node);            //already exists
            n = avltree_next(n);
            if (strchr(l->name,'-') || strchr(l->name,'+')) continue;
            fprintf(flab,"%-16s= ",l->name);
            switch (l->value.type) {
            case T_INT:
                val=l->value.num;
                if (val<0) fprintf(flab,"-");
                val=(val>=0?val:-val);
                if (val<0x100) fprintf(flab,"$%02lx",val);
                else if (val<0x10000l) fprintf(flab,"$%04lx",val);
                else if (val<0x1000000l) fprintf(flab,"$%06lx",val);
                else fprintf(flab,"$%08lx",val);
                if (!l->used) {
                    if (val<0x100) fprintf(flab,"  ");
                    if (val<0x10000l) fprintf(flab,"  ");
                    if (val<0x1000000l) fprintf(flab,"  ");
                    fprintf(flab,"; *** unused");
                }
                break;
            case T_STR:
                for (val=0;val<l->value.str.len;val++) {
                    if (val) fprintf(flab, ", ");
                    fprintf(flab,"$%02x", l->value.str.data[val]);
                }
                if (!l->used) {
                    fprintf(flab,"; *** unused");
                }
                break;
            default:
                fputc('?', flab);
                break;
            }
            fprintf(flab,"\n");
        }
	if (flab != stdout) fclose(flab);
    }
}

// ------------------------------------------------------------------
#ifndef WIN32
const char *argp_program_version="6502/65C02/65816/DTV TASM 1.46";
const char *argp_program_bug_address="<soci@c64.rulez.org>";
const char doc[]="64tass Turbo Assembler Macro";
const char args_doc[]="SOURCE";
const struct argp_option options[]={
    {"no-warn"	, 	'w',		0,     	0,  "Suppress warnings", 0 },
    {"quiet"	,	'q',		0,     	0,  "Display errors/warnings", 0 },
    {"nonlinear",	'n',		0,     	0,  "Generate nonlinear output file", 0 },
    {"nostart" 	,	'b',		0,     	0,  "Strip starting address", 0 },
    {"wordstart",	'W',		0,     	0,  "Force 2 byte start address", 0 },
    {"ascii" 	,	'a',		0,     	0,  "Convert ASCII to PETASCII", 0 },
    {"no-precedence",   'P',            0,      0,  "No operator precedence in expressions", 0 },
    {"compatible-ops",  'O',            0,      0,  "Enable TASS compatible operators", 0 },
    {"case-sensitive",	'C',		0,     	0,  "Case sensitive labels", 0 },
    {		0,	'o',"<file>"	,      	0,  "Place output into <file>", 0 },
    {		0,	'D',"<label>=<value>",     	0,  "Define <label> to <value>", 0 },
    {"long-branch",	'B',		0,     	0,  "Automatic bxx *+3 jmp $xxxx", 0 },
    {		0,  	0,		0,     	0,  "Target selection:", 0 },
    {"m65xx"  	,     	1,		0,     	0,  "Standard 65xx (default)", 0 },
    {"m6502"  	,     	'i',		0,     	0,  "NMOS 65xx", 0 },
    {"m65c02"  	,     	'c',		0,     	0,  "CMOS 65C02", 0 },
    {"m65816"  	,     	'x',		0,     	0,  "W65C816", 0 },
    {"m65dtv02"	,     	't',		0,     	0,  "65DTV02", 0 },
    {		0,  	0,		0,     	0,  "Source listing:", 0 },
    {"labels"	,	'l',"<file>"	,      	0,  "List labels into <file>", 0 },
    {"list"	,	'L',"<file>"	,      	0,  "List into <file>", 0 },
    {"no-monitor",	'm',		0,      0,  "Don't put monitor code into listing", 0 },
    {"no-source",	's',		0,      0,  "Don't put source code into listing", 0 },
    {		0,  	0,		0,     	0,  "Misc:", 0 },
    { 0, 0, 0, 0, NULL, 0 }
};

static error_t parse_opt (int key,char *arg,struct argp_state *state)
{
    switch (key)
    {
    case 'w':arguments.warning=0;break;
    case 'q':arguments.quiet=0;break;
    case 'W':arguments.wordstart=0;break;
    case 'n':arguments.nonlinear=1;break;
    case 'b':arguments.stripstart=1;break;
    case 'a':arguments.toascii=1;break;
    case 'P':arguments.noprecedence=1;break;
    case 'O':arguments.oldops=1;break;
    case 'o':arguments.output=arg;break;
    case 'D':
    {
	struct slabel* tmp;
	int i=0;
	while (arg[i] && arg[i]!='=') {
            if (!arguments.casesensitive) arg[i]=lowcase(arg[i]);
	    i++;
	}
	if (arg[i]=='=') {
            arg[i]=0;
            tmp=new_label(arg);tmp->proclabel=0;
            tmp->requires=0;
            tmp->conflicts=0;
	    tmp->value.type=T_INT;tmp->value.num=atoi(&arg[i+1]);
	}
	break;
    }
    case 'B':arguments.longbranch=1;break;
    case 1:arguments.cpumode=OPCODES_6502;break;
    case 'i':arguments.cpumode=OPCODES_6502i;break;
    case 'c':arguments.cpumode=OPCODES_65C02;break;
    case 'x':arguments.cpumode=OPCODES_65816;break;
    case 't':arguments.cpumode=OPCODES_65DTV02;break;
    case 'l':arguments.label=arg;break;
    case 'L':arguments.list=arg;break;
    case 'm':arguments.monitor=0;break;
    case 's':arguments.source=0;break;
    case 'C':arguments.casesensitive=1;break;
    case ARGP_KEY_ARG:if (state->arg_num) argp_usage(state);arguments.input=arg;break;
    case ARGP_KEY_END:if (!state->arg_num) argp_usage(state);break;
    default:return ARGP_ERR_UNKNOWN;
    }
    return 0;
}

const struct argp argp={options,parse_opt,args_doc,doc,NULL,NULL,NULL};

void testarg(int argc,char *argv[]) {
    argp_parse(&argp,argc,argv,0,0,&arguments);
}
#else
void testarg(int argc,char *argv[]) {
    int j,out=0;
    for (j=1;j<argc;j++)
    {
	if (!strcmp(argv[j],"-?") || !strcmp(argv[j],"--help")) {
	    printf(
		"Usage: 64tass [OPTION...] SOURCE\n"
		"64tass Turbo Assembler Macro\n"
		"\n"
                "  -a, --ascii\t\t     Convert ASCII to PETASCII\n"
		"  -b, --nostart\t\t     Strip starting address\n"
		"  -B, --long-branch\t     Automatic bxx *+3 jmp $xxxx\n"
		"  -C, --case-sensitive\t     Case sensitive labels\n"
		"  -D <label>=<value>\t     Define <label> to <value>\n"
		"  -n, --nonlinear\t     Generate nonlinear output file\n"
		"  -o <file>\t\t     Place output into <file>\n"
                "  -O, --compatible-ops\t     Enable TASS compatible operators\n"
                "  -P, --no-precedence\t     No operator precedence in expressions\n"
		"  -q, --quiet\t\t     Display errors/warnings\n"
		"  -w, --no-warn\t\t     Suppress warnings\n"
		"  -W, --wordstart\t     Force 2 byte start address\n"
		"\n"
		" Target selection:\n"
		"  -c, --m65c02\t\t     CMOS 65C02\n"
		"  -i, --m6502\t\t     NMOS 65xx\n"
		"      --m65xx\t\t     Standard 65xx (default)\n"
		"  -t, --m65dtv02\t     65DTV02\n"
		"  -x, --m65816\t\t     W65C816\n"
		"\n"
		" Source listing:\n"
		"  -l <file>\t\t     List labels into <file>\n"
		"  -L <file>\t\t     List into <file>\n"
		"  -m, --no-monitor\t     Don't put monitor code into listing\n"
		"  -s, --no-source\t     Don't put source code into listing\n"
		"\n"	
		" Misc:\n"
		"\n"
		"  -?, --help\t\t     Give this help list\n"
		"      --usage\t\t     Give a short usage message\n"
		"  -V, --version\t\t     Print program version\n"
		"\n"
		"Mandatory or optional arguments to long options are also mandatory or optional\n"
		"for any corresponding short options.\n"
		"\n"
		"Report bugs to <soci@c64.rulez.org>.\n");
	    exit(1);
	}
	if (!strcmp(argv[j],"--usage")) {
	    printf(
		"Usage: 64tass [-abBCnwWcitxXms?V] [-D <label>=<value>] [-o <file>] [-l <file>]\n"
		"\t    [-L <file>] [--ascii] [--nostart] [--long-branch]\n"
		"\t    [--case-sensitive] [--nonlinear] [--no-warn] [--wordstart]\n"
		"\t    [--m65c02] [--m6502] [--m65xx] [--m65dtv02] [--m65816]\n"
		"\t    [--no-monitor] [--no-source] [--help] [--usage] [--version] SOURCE\n");
	    exit(1);
	}
	if (!strcmp(argv[j],"-V") || !strcmp(argv[j],"--version")) {
	    printf("6502/65C02/65816/DTV TASM 1.46\n");
	    exit(1);
	}
	if (!strcmp(argv[j],"-w") || !strcmp(argv[j],"--no-warn")) {arguments.warning=0;continue;}
	if (!strcmp(argv[j],"-W") || !strcmp(argv[j],"--wordstart")) {arguments.wordstart=0;continue;}
	if (!strcmp(argv[j],"-n") || !strcmp(argv[j],"--nonlinear")) {arguments.nonlinear=1;continue;}
	if (!strcmp(argv[j],"-b") || !strcmp(argv[j],"--nostart")) {arguments.stripstart=1;continue;}
        if (!strcmp(argv[j],"-a") || !strcmp(argv[j],"--ascii")) {arguments.toascii=1;continue;}
        if (!strcmp(argv[j],"-P") || !strcmp(argv[j],"--no-precedence")) {arguments.noprecedence=1;continue;}
        if (!strcmp(argv[j],"-O") || !strcmp(argv[j],"--compatible-ops")) {arguments.oldops=1;continue;}
        if (!strcmp(argv[j],"-q") || !strcmp(argv[j],"--quiet")) {arguments.quiet=0;continue;}
	if (!strcmp(argv[j],"-B") || !strcmp(argv[j],"--long-branch")) {arguments.longbranch=1;continue;}
        if (!strcmp(argv[j],"--m65xx")) {arguments.cpumode=OPCODES_6502;continue;}
        if (!strcmp(argv[j],"-i") || !strcmp(argv[j],"--m6502")) {arguments.cpumode=OPCODES_6502i;continue;}
        if (!strcmp(argv[j],"-c") || !strcmp(argv[j],"--m65c02")) {arguments.cpumode=OPCODES_65C02;continue;}
        if (!strcmp(argv[j],"-x") || !strcmp(argv[j],"--m65816")) {arguments.cpumode=OPCODES_65816;continue;}
        if (!strcmp(argv[j],"-t") || !strcmp(argv[j],"--m65dtv02")) {arguments.cpumode=OPCODES_65DTV02;continue;}
	if (!strcmp(argv[j],"-l")) {
	    j++;if (j>=argc) goto ide2;
	    if (arguments.label) goto ide3;
	    arguments.label=malloc(strlen(argv[j])+1);
	    strcpy(arguments.label,argv[j]);
	    continue;
	}
	if (!strcmp(argv[j],"-L")) {
	    j++;if (j>=argc) goto ide2;
	    if (arguments.list) goto ide3;
	    arguments.list=malloc(strlen(argv[j])+1);
	    strcpy(arguments.list,argv[j]);
	    continue;
	}
	if (!strcmp(argv[j],"-m") || !strcmp(argv[j],"--no-monitor")) {arguments.monitor=0;continue;}
	if (!strcmp(argv[j],"-s") || !strcmp(argv[j],"--no-source")) {arguments.source=0;continue;}
	if (!strcmp(argv[j],"-C") || !strcmp(argv[j],"--case-sensitive")) {arguments.casesensitive=1;continue;}
        if (!strcmp(argv[j],"-o")) {
	    j++;if (j>=argc) goto ide2;
	    if (out) goto ide3;
	    arguments.output=malloc(strlen(argv[j])+1);
	    strcpy(arguments.output,argv[j]);
	    out=1;
	    continue;
	}
        if (!strcmp(argv[j],"-D")) {
	    struct slabel* tmp;
	    int i=0;
	    j++;if (j>=argc) {
		ide2:
		printf("64tass: option requires an argument -- %c\n",argv[j-1][1]);
		goto ide;
	    }

	    while (argv[j][i] && argv[j][i]!='=') {
        	if (!arguments.casesensitive) argv[j][i]=lowcase(argv[j][i]);
		i++;
	    }
	    if (argv[j][i]=='=') {
        	argv[j][i]=0;
                tmp=new_label(argv[j]);tmp->proclabel=0;
                tmp->requires=0;
                tmp->conflicts=0;
		tmp->value.type=T_INT;tmp->value.num=atoi(&argv[j][i+1]);
	    }
	    continue;
	    }
	if (arguments.input) goto ide3;
	arguments.input=malloc(strlen(argv[j])+1);
	strcpy(arguments.input,argv[j]);
    }
    if (!arguments.input) {
	ide3:
	printf("Usage: 64tass [OPTION...] SOURCE\n");
	ide:
	printf("Try `64tass --help' or `64tass --usage' for more information.\n");
        free(arguments.list);
        free(arguments.label);
	free(arguments.input);
	if (out) free(arguments.output);
	exit(1);
    }
}
#endif
