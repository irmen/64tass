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
#include "libtree.h"
#include "misc.h"
#include "opcodes.h"
#include <string.h>
#include "getopt.h"

void err_msg(unsigned char no, char* prm);

struct arguments_t arguments={1,1,0,0,0,"a.out",OPCODES_6502,NULL,NULL,1,1,0,0,1,0,0};

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
                n = (n * 10) + str[2] - '0';
                if (n < 256) return (unsigned char)n;
            }
        }
    }
    if (!strncasecmp(str, "cbm-", 4) && str[4] && str[5] == 0) {
        n=lowcase(str[4]);/* {cbm-x} */
        if (n >='a' && n <='z') return petsymcbm[n - 'a']; 
        if (n >='2' && n <='8') return (unsigned char)(n - '2' + 0x95); 
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
        if (n >='a' && n <='z') return (unsigned char)(n - 'a' + 0xc1);
        if (n >='1' && n <='9') return (unsigned char)(n - '0' + 0x20);
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
        if (n >='a' && n <= 'z') return (unsigned char)(n - 'a' + 1); /* {control-x} */
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
        "Unknown option: %s\n",
        "Too many passes\n",
        "Too many errors\n"
};

void err_msg(unsigned char no, char* prm) {
    char line[linelength];
    struct sfilenamelist *b=NULL, *b2=filenamelist;
    char *p;

    if (errors+conderrors==99 && no>=0x40) no=ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        warnings++;
        return;
    }

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
                a2->proclabel = 0; a2->used = pass;
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
	enum {UNKNOWN, UTF8, UTF16LE, UTF16BE, ISO1} type = UNKNOWN, lastchar;
        FILE *f;

	if (!(lastfi->name=malloc(strlen(name)+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        strcpy(lastfi->name,name);
	lastfi->linebuf=NULL;
	lastfi->linebuflen=0;
	lastfi->currentp=0;
        if (name[0]=='-' && !name[1]) f=stdin;
        else f=fopen(name,"rb");
        if (!f) {
            lastfi=NULL;
            return NULL;
        }
        if (arguments.quiet) fprintf(stdout, "Assembling file:   %s\n",name);
        lastchar=fgetc(f);
        ungetc(lastchar, f); 
        if (!lastchar) type=UTF16BE; /* most likely */

	do {
	    int i=0, j, ch = 0, ch2;
	    unsigned char *pline;
	    if (lastfi->currentp + linelength > lastfi->linebuflen) {
		lastfi->linebuflen += 0x1000;
		lastfi->linebuf=realloc(lastfi->linebuf, lastfi->linebuflen);
	    }
	    pline=&lastfi->linebuf[lastfi->currentp];
	    for (;;) {
                if (arguments.toascii) {
                    lastchar = ch;
                    switch (type) {
                        case UNKNOWN:
                        case UTF8:
                            ch=fgetc(f);
                            if (ch < 0) break;

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
                                ch2=fgetc(f);
                                if (ch == 0xff && ch2 == 0xfe) {
                                    type = UTF16LE;continue;
                                }
                                if (ch == 0xfe && ch2 == 0xff) {
                                    type = UTF16BE;continue;
                                }
                                ungetc(ch2, f); 
                                if (type == UNKNOWN) {
                                    type = ISO1; break;
                                }
                                ch = 0xfffd; i = 0;
                            }

                            for (j = i; i; i--) {
                                ch2 = fgetc(f);
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
                                    ungetc(ch2, f);
                                    ch = 0xfffd;break;
                                }
                                ch = (ch << 6) ^ ch2 ^ 0x80;
                            }
                            if (j) type = UTF8;
                            break;
                        case UTF16LE:
                            ch=fgetc(f);
                            ch2=fgetc(f);
                            if (ch2 == EOF) break;
                            ch |= ch2 << 8;
                            if (ch == 0xfffe) {
                                type = UTF16BE;
                                continue;
                            }
                            break;
                        case UTF16BE:
                            ch2=fgetc(f);
                            ch=fgetc(f);
                            if (ch == EOF) break;
                            ch |= ch2 << 8;
                            if (ch == 0xfffe) {
                                type = UTF16LE;
                                continue;
                            }
                            break;
                        case ISO1:
                            ch=fgetc(f);
                            break;
                    }
                    if (ch == 0xfeff) continue;
                    if (type != UTF8) {
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
                } else {
                    ch = fgetc(f);
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
        if (f!=stdin) fclose(f);
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
            case T_CHR:
            case T_INT:
                val=l->value.num;
                if (val<0) fprintf(flab,"-");
                val=(val>=0?val:-val);
                if (val<0x100) fprintf(flab,"$%02lx",val);
                else if (val<0x10000l) fprintf(flab,"$%04lx",val);
                else if (val<0x1000000l) fprintf(flab,"$%06lx",val);
                else fprintf(flab,"$%08lx",val);
                if (l->used<pass) {
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
                if (l->used<pass) {
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
const char *short_options= "wqnbWaPOCBicxtl:L:msVo:D:";

const struct option long_options[]={
    {"no-warn"	        , no_argument      , 0,	'w'},
    {"quiet"	        , no_argument      , 0,	'q'},
    {"nonlinear"        , no_argument      , 0,	'n'},
    {"nostart" 	        , no_argument      , 0,	'b'},
    {"wordstart"        , no_argument      , 0,	'W'},
    {"ascii" 	        , no_argument      , 0,	'a'},
    {"no-precedence"    , no_argument      , 0, 'P'},
    {"compatible-ops"   , no_argument      , 0, 'O'},
    {"case-sensitive"   , no_argument      , 0,	'C'},
    {"long-branch"      , no_argument      , 0,	'B'},
    {"m65xx"  	        , no_argument      , 0,   1},
    {"m6502"  	        , no_argument      , 0,	'i'},
    {"m65c02"  	        , no_argument      , 0,	'c'},
    {"m65816"  	        , no_argument      , 0,	'x'},
    {"m65dtv02"	        , no_argument      , 0, 't'},
    {"labels"	        , required_argument, 0,	'l'},
    {"list"	        , required_argument, 0,	'L'},
    {"no-monitor"       , no_argument      , 0,	'm'},
    {"no-source"        , no_argument      , 0, 's'},
    {"version"          , no_argument      , 0, 'V'},
    {"usage"            , no_argument      , 0,  2},
    {"help"             , no_argument      , 0,  3},
    { 0, 0, 0, 0}
};

int testarg(int argc,char *argv[]) {
    int opt, longind;
    
    while ((opt = getopt_long_only(argc, argv, short_options, long_options, &longind)) != -1)
        switch (opt)
        {
            case 'w':arguments.warning=0;break;
            case 'q':arguments.quiet=0;break;
            case 'W':arguments.wordstart=0;break;
            case 'n':arguments.nonlinear=1;break;
            case 'b':arguments.stripstart=1;break;
            case 'a':arguments.toascii=1;break;
            case 'P':arguments.noprecedence=1;break;
            case 'O':arguments.oldops=1;break;
            case 'o':arguments.output=optarg;break;
            case 'D':
                {
                    struct slabel* tmp;
                    int i=0;
                    while (optarg[i] && optarg[i]!='=') {
                        if (!arguments.casesensitive) optarg[i]=lowcase(optarg[i]);
                        i++;
                    }
                    if (optarg[i]=='=') {
                        optarg[i]=0;
                        tmp=new_label(optarg);tmp->proclabel=0;
                        tmp->requires=0;
                        tmp->conflicts=0;
                        tmp->value.type=T_INT;tmp->value.num=atoi(&optarg[i+1]);
                        optarg[i]='=';
                    }
                    break;
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
            case 2:printf(
	       "Usage: 64tass [-abBCnOPqwWcitxms?V] [-D <label>=<value>] [-o <file>]\n"
	       "	[-l <file>] [-L <file>] [--ascii] [--nostart] [--long-branch]\n"
	       "	[--case-sensitive] [--nonlinear] [--compatible-ops]\n"
	       "	[--no-precedence] [--quiet] [--no-warn] [--wordstart] [--m65c02]\n"
	       "	[--m6502] [--m65xx] [--m65dtv02] [--m65816] [--labels=<file>]\n"
	       "	[--list=<file>] [--no-monitor] [--no-source] [--help] [--usage]\n"
	       "	[--version] SOURCES\n");exit(0);

            case 'V':printf("64tass Turbo Assembler Macro V" VERSION "\n");exit(0);
            case 3:
            case '?':if (optopt=='?' || opt==3) { printf(
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
	       "  -O, --compatible-ops	Enable TASS compatible operators\n"
	       "  -P, --no-precedence	No operator precedence in expressions\n"
	       "  -q, --quiet		Display errors/warnings\n"
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
	       "Report bugs to <soci" "\x40" "c64.rulez.org>.\n");exit(0);}
            default:fprintf(stderr,
                "Try `64tass --help' or `64tass --usage' for more information.\n");exit(1);
        }
    if (argc <= optind) {
        fprintf(stderr,
            "Usage: 64tass [OPTIONS...] SOURCES\n"
            "Try `64tass --help' or `64tass --usage' for more information.\n");exit(1);
    }
    return optind;
}
