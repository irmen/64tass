/*
    Turbo Assembler 6502/65C02/65816/DTV
    Copyright (C) <year>  <name of author>

   6502/65C02 Turbo Assembler  Version 1.3
   (c)1996 Taboo Productions, Marek Matula

   6502/65C02 Turbo Assembler  Version 1.35  ANSI C port
   (c)2000 [BiGFooT/BReeZe^2000]

   6502/65C02/65816/DTV Turbo Assembler  Version 1.4x
   (c)2001-2011 Soci/Singular (soci@c64.rulez.org)

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

#define _GNU_SOURCE
#define _MAIN_C_
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <time.h>

#include "opcodes.h"
#include "misc.h"
#include "eval.h"

static const char *mnemonic;    //mnemonics
static const uint8_t *opcode;    //opcodes

struct memblock_s {size_t p, len;uint32_t start;}; //starts and sizes

#define nestinglevel 256
unsigned int errors=0,conderrors=0,warnings=0;
static int wrapwarn=0, wrapwarn2=0;
uint32_t sline, vline;      //current line
static uint32_t all_mem;
uint8_t pass=0;      //pass
static int listing=0;   //listing
static struct {size_t p, len;uint8_t *data;} mem = {0, 0, NULL};//Linear memory dump
static size_t memblocklastp = 0;
static uint32_t memblocklaststart = 0;
static struct {unsigned int p, len;struct memblock_s *data;} memblocks = {0, 0, NULL};
uint32_t address=0, l_address=0, star=0; //address, logical address
const uint8_t *pline, *llist;  //current line data
unsigned int lpoint;              //position in current line
char ident[linelength], ident2[linelength];  //identifier (label, etc)
static char varname[linelength];//variable (same as identifier?)
static char path[linelength];   //path
static int32_t pagelo=-1;           //still in same page?
static FILE* flist = NULL;      //listfile
static enum { LIST_NONE, LIST_CODE, LIST_DATA, LIST_EQU } lastl = LIST_CODE;
static struct {uint16_t p, len; int32_t *data;} logitab = {0,0,NULL};  //.logical .here
static int longaccu=0,longindex=0,scpumode=0,dtvmode=0;
static uint8_t databank=0;
static uint16_t dpage=0;
int fixeddig, dooutput;
uint32_t current_requires, current_conflicts, current_provides;
static int allowslowbranch=1;
static int longbranchasjmp=0;
static uint8_t outputeor = 0; // EOR value for final output (usually 0, except changed by .eor)

static struct {
    char what;
    uint32_t line;
} waitfor[nestinglevel];
static uint8_t skipit[nestinglevel];
static uint8_t waitforp=0;

static unsigned int last_mnem;

int labelexists;
uint16_t reffile;
uint32_t backr, forwr;
struct file_s *cfile;
struct avltree *star_tree = NULL;
static uint_fast8_t macrecursion, structrecursion;

static const char* command[]={ /* must be sorted, first char is the ID */
    "\x1e" "al",
    "\x30" "align",
    "\x1d" "as",
    "\x31" "assert",
    "\x36" "bend",
    "\x16" "binary",
    "\x35" "block",
    "\x00" "byte",
    "\x2e" "cerror",
    "\x03" "char",
    "\x32" "check",
    "\x17" "comment",
    "\x33" "cpu",
    "\x2f" "cwarn",
    "\x24" "databank",
    "\x25" "dpage",
    "\x43" "dstruct",
    "\x11" "else",
    "\x13" "elsif",
    "\x28" "enc",
    "\x3b" "end",
    "\x18" "endc",
    "\x29" "endif",
    "\x0d" "endm",
    "\x1a" "endp",
    "\x42" "ends",
    "\x3c" "eor",
    "\x21" "error",
    "\x12" "fi",
    "\x26" "fill",
    "\x0e" "for",
    "\x40" "goto",
    "\x1c" "here",
    "\x3a" "hidemac",
    "\x10" "if",
    "\x2b" "ifeq",
    "\x2d" "ifmi",
    "\x2a" "ifne",
    "\x2c" "ifpl",
    "\x15" "include",
    "\x08" "int",
    "\x3f" "lbl",
    "\x1b" "logical",
    "\x0a" "long",
    "\x0c" "macro",
    "\x0f" "next",
    "\x06" "null",
    "\x0b" "offs",
    "\x34" "option",
    "\x19" "page",
    "\x23" "pend",
    "\x22" "proc",
    "\x38" "proff",
    "\x37" "pron",
    "\x02" "ptext",
    "\x14" "rept",
    "\x07" "rta",
    "\x3d" "segment",
    "\x04" "shift",
    "\x05" "shiftl",
    "\x39" "showmac",
    "\x41" "struct",
    "\x01" "text",
    "\x3e" "var",
    "\x27" "warn",
    "\x09" "word",
    "\x20" "xl",
    "\x1f" "xs",
};

enum command_e {
    CMD_BYTE=0, CMD_TEXT, CMD_PTEXT, CMD_CHAR, CMD_SHIFT, CMD_SHIFTL, CMD_NULL,
    CMD_RTA, CMD_INT, CMD_WORD, CMD_LONG, CMD_OFFS, CMD_MACRO, CMD_ENDM,
    CMD_FOR, CMD_NEXT, CMD_IF, CMD_ELSE, CMD_FI, CMD_ELSIF, CMD_REPT,
    CMD_INCLUDE, CMD_BINARY, CMD_COMMENT, CMD_ENDC, CMD_PAGE, CMD_ENDP,
    CMD_LOGICAL, CMD_HERE, CMD_AS, CMD_AL, CMD_XS, CMD_XL, CMD_ERROR, CMD_PROC,
    CMD_PEND, CMD_DATABANK, CMD_DPAGE, CMD_FILL, CMD_WARN, CMD_ENC, CMD_ENDIF,
    CMD_IFNE, CMD_IFEQ, CMD_IFPL, CMD_IFMI, CMD_CERROR, CMD_CWARN, CMD_ALIGN,
    CMD_ASSERT, CMD_CHECK, CMD_CPU, CMD_OPTION, CMD_BLOCK, CMD_BEND, CMD_PRON,
    CMD_PROFF, CMD_SHOWMAC, CMD_HIDEMAC, CMD_END, CMD_EOR, CMD_SEGMENT,
    CMD_VAR, CMD_LBL, CMD_GOTO, CMD_STRUCT, CMD_ENDS, CMD_DSTRUCT
};

// ---------------------------------------------------------------------------

void status(void) {
    freeerrorlist(1);
    errors+=conderrors;
    if (arguments.quiet) {
        uint32_t start, end;
        unsigned int i;
        char temp[10];
        printf("Error messages:    ");
        if (errors) printf("%u\n", errors); else puts("None");
        printf("Warning messages:  ");
        if (warnings) printf("%u\n", warnings); else puts("None");
        printf("Passes:            %u\n",pass);
        if (memblocks.p) {
            start = memblocks.data[0].start;
            end = memblocks.data[0].start + memblocks.data[0].len;
            for (i=1;i<memblocks.p;i++) {
                if (memblocks.data[i].start != end) {
                    sprintf(temp, "$%04x", start);
                    printf("Memory range:    %7s-$%04x\n",temp,end-1);
                    start = memblocks.data[i].start;
                }
                end = memblocks.data[i].start + memblocks.data[i].len;
            }
            sprintf(temp, "$%04x", start);
            printf("Memory range:    %7s-$%04x\n",temp,end-1);
        } else puts("Memory range:      None");
    }
    free(mem.data);		        	// free codemem
    free(memblocks.data);				// free memorymap
    free(logitab.data);

    tfree();
}

static void printllist(FILE *f) {
    const uint8_t *c = llist, *last, *n;
    uint32_t ch;
    if (c) {
        last = c;
        while ((ch = *c)) {
            if (ch & 0x80) n=c+utf8in(c, &ch); else n=c+1;
            if ((ch < 0x20 || ch > 0x7e) && ch!=9) {
                fwrite(last, c - last, 1, f);
                fprintf(f, "{$%x}", ch);
                last=n;
            }
            c = n;
        }
        while (c > last && (c[-1] == 0x20 || c[-1] == 0x09)) c--;
        fwrite(last, c - last, 1, f);
        llist=NULL;
    }
    fputc('\n', f);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
static void memjmp(uint32_t adr) {
    if (mem.p == memblocklastp) {
        memblocklaststart = adr;
        return;
    }
    if (memblocks.p>=memblocks.len) {
        memblocks.len+=64;
        memblocks.data=realloc(memblocks.data, memblocks.len*sizeof(*memblocks.data));
        if (!memblocks.data) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    }
    memblocks.data[memblocks.p].len = mem.p-memblocklastp;
    memblocks.data[memblocks.p].p = memblocklastp;
    memblocks.data[memblocks.p++].start = memblocklaststart;
    memblocklastp = mem.p;
    memblocklaststart = adr;
}

static int memblockcomp(const void *a, const void *b) {
    const struct memblock_s *aa=(struct memblock_s *)a;
    const struct memblock_s *bb=(struct memblock_s *)b;
    return aa->start-bb->start;
}

static void memcomp(void) {
    unsigned int i, j, k;
    memjmp(0);
    if (memblocks.p<2) return;

    for (k = j = 0; j < memblocks.p; j++) {
        const struct memblock_s *bj = &memblocks.data[j];
        if (bj->len) {
            for (i = j + 1; i < memblocks.p; i++) if (memblocks.data[i].len) {
                struct memblock_s *bi = &memblocks.data[i];
                if (bj->start <= bi->start && bj->start + bj->len > bi->start) {
                    uint32_t overlap = bj->start + bj->len - bi->start;
                    if (overlap > bi->len) overlap = bi->len;
                    memcpy(mem.data + bj->p + bi->start - bj->start, mem.data + bi->p, overlap);
                    bi->len-=overlap;
                    bi->p+=overlap;
                    bi->start+=overlap;
                }
            }
            if (j!=k) memblocks.data[k]=*bj;
            k++;
        }
    }
    memblocks.p = k;
    qsort(memblocks.data, memblocks.p, sizeof(*memblocks.data), memblockcomp);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
static void pokeb(uint8_t byte)
{

    if (fixeddig && dooutput)
    {
        if (mem.p>=mem.len) {
            mem.len+=0x1000;
            mem.data=realloc(mem.data, mem.len);
            if (!mem.data) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        }
	mem.data[mem.p++] = byte ^ outputeor;
    }
    if (wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);wrapwarn=0;}
    if (wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);wrapwarn2=0;}
    address++;l_address++;l_address&=all_mem;
    if (address & ~all_mem) {
	if (fixeddig) wrapwarn=1;
	address=0;
        memjmp(address);
    }
    if (fixeddig && scpumode) if (!(address & 0xffff) || !(l_address & 0xffff)) wrapwarn2=1;
}

static int lookup_opcode(const char *s) {
    char s2,s3, ch;
    const char *p;
    int s4;
    unsigned int also=0,felso,elozo, no;

    ch=lowcase(s[0]);
    no=(felso=last_mnem)/2;
    if (ch && (s2=lowcase(s[1])) && (s3=lowcase(s[2])) && !s[3])
        for (;;) {  // do binary search
            if (!(s4=ch-*(p=mnemonic+no*3)))
                if (!(s4=s2-*(++p)))
                    if (!(s4=s3-*(++p)))
                    {
                        return no;
                    }

            elozo=no;
            if (elozo==(no=((s4>0) ? (felso+(also=no)) : (also+(felso=no)) )/2)) break;
        }
    return -1;
}

// ---------------------------------------------------------------------------
static int what(int *tempno) {
    char ch;

    ignore();
    switch (ch=whatis[(int)here()]) {
    case WHAT_COMMAND:
	{
            char cmd[20];
            unsigned int no, l, also, felso, elozo;
            int s4;
            lpoint++;
            for (also = l = 0; l < sizeof(cmd)-1; l++) {
                cmd[l]=pline[lpoint+l] | 0x20;
                if (!pline[lpoint+l] || cmd[l] < 'a' || cmd[l] > 'z') {
                    cmd[l]=(cmd[l] >= '0' && cmd[l] <= '9') || cmd[l]=='_';
                    l++;break;
                }
            }
            l--;
            if (!cmd[l]) {
                felso=sizeof(command)/sizeof(command[0]);
                no=felso/2;
                for (;;) {  // do binary search
                    if (!(s4=strcmp(cmd, command[no] + 1))) {
                        lpoint+=l;
                        no = (uint8_t)command[no][0];
                        if (no==CMD_ENDIF) no=CMD_FI; else
                        if (no==CMD_IFNE) no=CMD_IF;
                        *tempno=no;
                        return WHAT_COMMAND;
                    }

                    elozo = no;
                    no = ((s4>0) ? (felso+(also=no)) : (also+(felso=no)))/2;
                    if (elozo == no) break;
                }
            }
	    *tempno=sizeof(command)/sizeof(command[0]);
	    return 0;
	}
    case WHAT_COMA:
	lpoint++;
        ignore();
	switch (get() | 0x20) {
	case 'y': ignore();return WHAT_Y;
	case 'x': ignore();if (here()==')') {lpoint++;ignore();return WHAT_XZ;} else return WHAT_X;
	case 's': ignore();if (here()==')') {lpoint++;ignore();return WHAT_SZ;} else return WHAT_S;
	default: lpoint--;return WHAT_COMA;
	}
    case WHAT_CHAR:
    case WHAT_LBL:
            *tempno=1;return WHAT_EXPRESSION;
    case WHAT_EXPRESSION://tempno=1 if label, 0 if expression
	    *tempno=0;return WHAT_EXPRESSION;
    case WHAT_COMMENT:
    case WHAT_EOL:return ch;
    default:lpoint++;return ch;
    }
}

static int get_ident2(void) {
    unsigned int i=0;
    uint8_t ch;
    if (arguments.casesensitive)
	while ((whatis[ch=here()]==WHAT_CHAR) || (ch>='0' && ch<='9') || ch=='_') {ident[i++]=ch; lpoint++; }
    else
	while (((ch=lowcase(pline[lpoint]))>='a' && ch<='z') || (ch>='0' && ch<='9') || ch=='_') { ident[i++]=ch; lpoint++; }
    ident[i]=0;
    return i == 0;
}

int get_ident(void) {
    int code;

    if (what(&code)!=WHAT_EXPRESSION || !code) {
	err_msg(ERROR_EXPRES_SYNTAX,NULL);
	return 1;
    }
    return get_ident2();
}

static uint_fast8_t val_length(int32_t val)
{
        if (val<0) return 3;
    	if (val<0x100) return 0;
        if (val<0x10000) return 1;
	if (val<0x1000000) return 2;
        return 3;
}

static int get_path(const char *base) {
    int q=1;
    unsigned int i=0, i2;
    if (base) {
        char *c=strrchr(base,'/');
        if (c) {
            i=c-base+1;
            if (i>=sizeof(path)) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
            memcpy(path,base,i);
        }
    }
    i2 = i;
    ignore();
    if (here()=='\"') {lpoint++;q=0;}
    while (here() && (here()!=';' || !q) && (here()!='\"' || q) && i<sizeof(path)) path[i++]=get();
    if (i>=sizeof(path) || (!q && here()!='\"')) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    if (!q) lpoint++; else while (i && (path[i-1]==0x20 || path[i-1]==0x09)) i--;
    path[i]=0;
    ignore();
    if (i <= i2) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    return 0;
}

//------------------------------------------------------------------------------

/*
 * macro parameter expansion
 *
 * in:
 *   mpr:  parameters, separated by zeros
 *   nprm: number of parameters
 *   cucc: one line of the macro (unexpanded)
 * out:
 *   cucc: one line of the macro (expanded)
*/
static inline void mtranslate(const char* mpr, uint_fast8_t nprm, const uint8_t *tmp)
{
    uint_fast8_t q;
    uint_fast16_t p, pp, i, j;
    uint8_t ch;
    static uint8_t cucc[linelength];

    q=p=0;
    for (i = 0; (ch = tmp[i]); i++) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        else if ((ch == ';') && (!q)) { q=4; }
        else if ((ch=='\\') && (!q)) {
            /* normal parameter reference */
            if (((ch=lowcase(tmp[i+1]))>='1' && ch<='9') || (ch>='a' && ch<='z')) {
                /* \1..\9, \a..\z */
                if ((j=(ch<='9' ? ch-'1' : ch-'a'+9))>=nprm) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                for (pp=0; j; j--) while (mpr[pp++]); //skip parameters
                while (mpr[pp]==0x20 || mpr[pp]==0x09) pp++; //skip space
                while (mpr[pp] && p<linelength) cucc[p++]=mpr[pp++];//copy
                if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                i++;continue;
            } else if (ch=='@') {
                /* \@ gives complete parameter list */
                for (pp=j=0;j<nprm;j++) {
                    while (mpr[pp] && p<linelength) cucc[p++]=mpr[pp++];//copy
                    if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                    if ((j+1)<nprm) {
                        cucc[p++]=',';pp++;
                    }
                }
                if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                i++;continue;
            } else ch='\\';
        } else if (ch=='@') {
            /* text parameter reference */
            if (((ch=lowcase(tmp[i+1]))>='1' && ch<='9')) {
                /* @1..@9 */
                if ((j=ch-'1')>=nprm) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                for (pp=0; j; j--) while (mpr[pp++]); //skip parameters
                while (mpr[pp]==0x20 || mpr[pp]==0x09) pp++; //skip space
                while (mpr[pp] && p<linelength) cucc[p++]=mpr[pp++];//copy
                if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                i++;continue;
            } else ch='@';
        }
        cucc[p++]=ch;
        if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
    }
    cucc[p]=0;
    pline = cucc;
}

//------------------------------------------------------------------------------

static void set_cpumode(uint_fast8_t cpumode) {
    all_mem=0xffff;scpumode=0;dtvmode=0;
    switch (last_mnem=cpumode) {
    case OPCODES_65C02:mnemonic=MNEMONIC65C02;opcode=c65c02;break;
    case OPCODES_6502i:mnemonic=MNEMONIC6502i;opcode=c6502i;break;
    case OPCODES_65816:mnemonic=MNEMONIC65816;opcode=c65816;all_mem=0xffffff;scpumode=1;break;
    case OPCODES_65DTV02:mnemonic=MNEMONIC65DTV02;opcode=c65dtv02;dtvmode=1;break;
    default: mnemonic=MNEMONIC6502;opcode=c6502;break;
    }
}

void var_assign(struct label_s *tmp, const struct value_s *val, int fix) {
    switch (tmp->value.type) {
    case T_CHR:
    case T_INT:
        if ((val->type != T_INT && val->type != T_CHR) || tmp->value.u.num!=val->u.num) {
            tmp->value=*val;
            if (val->type == T_STR) {
                tmp->value.u.str.data=malloc(val->u.str.len);
                memcpy(tmp->value.u.str.data,val->u.str.data,val->u.str.len);
            }
            fixeddig=fix;
        } else tmp->value.type = val->type;
        break;
    case T_STR:
        if (val->type != T_STR || tmp->value.u.str.len!=val->u.str.len || memcmp(tmp->value.u.str.data, val->u.str.data, val->u.str.len)) {
            if (val->type == T_STR) {
                tmp->value.type = val->type;
                tmp->value.u.str.len=val->u.str.len;
                tmp->value.u.str.data=realloc(tmp->value.u.str.data, val->u.str.len);
                memcpy(tmp->value.u.str.data,val->u.str.data,val->u.str.len);
            } else {
                free(tmp->value.u.str.data);
                tmp->value=*val;
            }
            fixeddig=fix;
        }
        break;
    case T_NONE:
        if (val->type != T_NONE) fixeddig=fix;
        tmp->value=*val;
        if (val->type == T_STR) {
            tmp->value.u.str.data=malloc(val->u.str.len);
            memcpy(tmp->value.u.str.data,val->u.str.data,val->u.str.len);
        }
        break;
    default: /* not possible here */
        exit(1);
        break;
    }
    tmp->upass=pass;
}

static void compile(const char*,int8_t);

static void macro_recurse(const char* mprm,int8_t nprm, char t, struct macro_s *tmp2) {
    macrecursion++;
    if (macrecursion<100) {
        size_t oldpos = tmp2->file->p;
        uint32_t lin = sline;
        struct file_s *f;
        struct star_s *s = new_star(vline);
        struct avltree *stree_old = star_tree;
        uint32_t ovline = vline;

        if (labelexists && s->addr != star) fixeddig=0;
        s->addr = star;
        star_tree = &s->tree;vline=0;
        enterfile(tmp2->file->name, sline);
        tmp2->file->p = tmp2->p; sline = tmp2->sline;
        waitfor[++waitforp].what=t;waitfor[waitforp].line=sline;skipit[waitforp]=1;
        f = cfile; cfile = tmp2->file;
        compile(mprm,nprm);
        exitfile(); cfile = f;
        star_tree = stree_old; vline = ovline;
        sline = lin; tmp2->file->p = oldpos;
    } else err_msg(ERROR__MACRECURSION,"!!!!");
    macrecursion--;
}

static void compile(const char* mprm,int8_t nprm) // "",0
{
    int wht,w,d,c;
    int prm = 0;
    struct value_s val;

    char ch;

    struct label_s *tmp = NULL;
    struct macro_s *tmp2 = NULL;

    uint8_t oldwaitforp = waitforp;
    unsigned wasref;
    int nobreak = 1;

    while (cfile->len != cfile->p && nobreak) {
        pline = cfile->data + cfile->p; lpoint = 0; sline++;vline++; cfile->p += strlen((char *)pline) + 1;
        if (nprm>=0) mtranslate(mprm,nprm,pline); //expand macro parameters, if any
        llist = pline;
        star=l_address;
        ident2[0]=wasref=0;
        if ((wht=what(&prm))==WHAT_EXPRESSION) {
            if (!prm) {
                if (here()=='-' || here()=='+') {
                    ident2[0]=here();ident2[1]=0;
                    lpoint++;if (here()!=0x20 && here()!=0x09 && here()!=';' && here()) goto baj;
                    prm=1;
                    if (ident2[0]=='-') {
                        sprintf(ident,"-%x-%x", reffile, backr++);
                    } else {
                        sprintf(ident,"+%x+%x", reffile, forwr++);
                    }
                    goto hh;
                }
            baj:
                if (skipit[waitforp] & 1) err_msg(ERROR_GENERL_SYNTAX,NULL);
                goto breakerr;
            } //not label
            get_ident();                                           //get label
            if (here()==':') lpoint++;
            else if ((prm=lookup_opcode(ident))>=0) {
                if (skipit[waitforp] & 1) goto as_opcode; else continue;
            }
            if (listing) strcpy(ident2,ident);
        hh:
            if (!(skipit[waitforp] & 1)) {wht=what(&prm);goto jn;} //skip things if needed
            if ((wht=what(&prm))==WHAT_EQUAL) { //variable
                strcpy(varname,ident);
                get_exp(&w,&d,&c,&val, T_NONE); //ellenorizve.
                if (!c) goto breakerr;
                if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL);goto breakerr;}
                tmp=new_label(varname, L_LABEL);
                if (listing && flist && arguments.source && tmp->ref) {
                    if (lastl!=LIST_EQU) {fputc('\n',flist);lastl=LIST_EQU;}
                    if (val.type == T_INT || val.type == T_CHR) {
                        fprintf(flist,"=%x\t\t\t\t\t",val.u.num);
                    } else {
                        fputs("=\t\t\t\t\t", flist);
                    }
                    printllist(flist);
                }
                tmp->ref=0;
                if (pass==1) {
                    if (labelexists) err_msg(ERROR_DOUBLE_DEFINE,varname);
                    else {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        tmp->upass=tmp->pass=pass;
                        tmp->value=val;
                        if (val.type == T_STR) {
                            tmp->value.u.str.data=malloc(val.u.str.len);
                            memcpy(tmp->value.u.str.data,val.u.str.data,val.u.str.len);
                        }
                    }
                } else {
                    if (labelexists) {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        var_assign(tmp, &val, 0);
                    }
                }
                goto finish;
            }
            if (wht==WHAT_COMMAND) {
                switch (prm) {
                case CMD_VAR: //variable
                    strcpy(varname,ident);
                    get_exp(&w,&d,&c,&val, T_NONE); //ellenorizve.
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL);goto breakerr;}
                    tmp=new_label(varname, L_VAR);
                    if (listing && flist && arguments.source && tmp->ref) {
                        if (lastl!=LIST_EQU) {fputc('\n',flist);lastl=LIST_EQU;}
                        if (val.type == T_INT || val.type == T_CHR) {
                            fprintf(flist,"=%x\t\t\t\t\t",val.u.num);
                        } else {
                            fputs("=\t\t\t\t\t", flist);
                        }
                        printllist(flist);
                    }
                    tmp->ref=0;
                    if (labelexists) {
                        if (tmp->type != L_VAR) err_msg(ERROR_DOUBLE_DEFINE,varname);
                        else {
                            tmp->requires=current_requires;
                            tmp->conflicts=current_conflicts;
                            var_assign(tmp, &val, fixeddig);
                        }
                    } else {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        tmp->upass=tmp->pass=pass;
                        tmp->value=val;
                        if (val.type == T_STR) {
                            tmp->value.u.str.data=malloc(val.u.str.len);
                            memcpy(tmp->value.u.str.data,val.u.str.data,val.u.str.len);
                        }
                        if (!d) err_msg(ERROR___NOT_DEFINED,"argument used");
                    }
                    goto finish;
                case CMD_LBL:
                    { //variable
                        struct jump_s *tmp2;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_EQU) {fputc('\n',flist);lastl=LIST_EQU;}
                            fputs("=\t\t\t\t\t", flist);
                            printllist(flist);
                        }
                        tmp2 = new_jump(ident);
                        if (labelexists) {
                            if (tmp2->sline != sline
                                    || tmp2->waitforp != waitforp
                                    || tmp2->file != cfile
                                    || tmp2->p != cfile->p
                                    || tmp2->parent != current_context) {
                                err_msg(ERROR_DOUBLE_DEFINE,ident);
                            }
                        } else {
                            tmp2->sline = sline;
                            tmp2->waitforp = waitforp;
                            tmp2->file = cfile;
                            tmp2->p = cfile->p;
                            tmp2->parent = current_context;
                        }
                        goto finish;
                    }
                case CMD_MACRO:// .macro
                case CMD_SEGMENT:
                    waitfor[++waitforp].what='m';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    tmp2=new_macro(ident);
                    if (labelexists) {
                        if (tmp2->p!=cfile->p
                         || tmp2->sline!=sline
                         || tmp2->type!=prm
                         || tmp2->file!=cfile) {
                            err_msg(ERROR_DOUBLE_DEFINE,ident);
                        }
                    } else {
                        tmp2->p=cfile->p;
                        tmp2->sline=sline;
                        tmp2->type=prm;
                        tmp2->file=cfile;
                    }
                    goto finish;
                case CMD_STRUCT:
                    {
                        struct label_s *old_context=current_context;
                        uint32_t old_address = address, old_laddress = l_address;
                        int old_dooutput = dooutput;
                        waitfor[++waitforp].what='s';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                        tmp2=new_macro(ident);
                        if (labelexists) {
                            if (tmp2->p!=cfile->p
                             || tmp2->sline!=sline
                             || tmp2->type!=prm
                             || tmp2->file!=cfile) {
                                err_msg(ERROR_DOUBLE_DEFINE,ident);
                            }
                        } else {
                            tmp2->p=cfile->p;
                            tmp2->sline=sline;
                            tmp2->type=prm;
                            tmp2->file=cfile;
                        }
                        tmp=new_label(ident, L_STRUCT);
                        if (pass==1) {
                            if (labelexists) err_msg(ERROR_DOUBLE_DEFINE,ident);
                            else {
                                tmp->requires=0;
                                tmp->conflicts=0;
                                tmp->upass=tmp->pass=pass;
                                tmp->value.type=T_INT;tmp->value.u.num=0;
                            }
                        } else {
                            if (labelexists) {
                                if (tmp->value.type != T_INT || tmp->type != L_STRUCT) { /* should not happen */
                                    err_msg(ERROR_DOUBLE_DEFINE,ident);
                                } else {
                                    if ((uint32_t)tmp->value.u.num != 0) {
                                        tmp->value.u.num=0;
                                        fixeddig=0;
                                    }
                                    tmp->requires=0;
                                    tmp->conflicts=0;
                                    tmp->value.type=T_INT;
                                }
                            }
                        }
                        current_context=tmp;
                        tmp->ref=0;
                        if (!structrecursion) {address = l_address = 0;dooutput = 0;memjmp(0);}
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                            printllist(flist);
                        }
                        structrecursion++;
                        if (structrecursion<100) {
                            waitforp--;
                            waitfor[++waitforp].what='S';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                            compile(mprm,nprm);
                            current_context = old_context; 
                        } else err_msg(ERROR__MACRECURSION,"!!!!");
                        structrecursion--;
                        if (!structrecursion) {address = old_address; l_address = old_laddress; dooutput = old_dooutput; memjmp(address);}
                        goto finish;
                    }
                }
            }
            tmp=find_label2(ident, &current_context->members);
            if (tmp) labelexists=1;
            else {
                if ((tmp2=find_macro(ident))) {lpoint--;ident2[0]=0;goto as_macro;}
                tmp=new_label(ident, L_LABEL);
            }
            if (pass==1) {
                if (labelexists) err_msg(ERROR_DOUBLE_DEFINE,ident);
                else {
                    tmp->requires=current_requires;
                    tmp->conflicts=current_conflicts;
                    tmp->upass=tmp->pass=pass;
                    tmp->value.type=T_INT;tmp->value.u.num=l_address;
                }
            } else {
                if (labelexists) {
                    if (tmp->value.type != T_INT || tmp->type != L_LABEL) { /* should not happen */
                        err_msg(ERROR_DOUBLE_DEFINE,ident);
                    } else {
                        if ((uint32_t)tmp->value.u.num != l_address) {
                            tmp->value.u.num=l_address;
                            fixeddig=0;
                        }
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        tmp->value.type=T_INT;
                    }
                }
            }
            if (pline[lpoint-1]!=':' && (pline[0]==0x20 || pline[0]==0x09)) err_msg(ERROR_LABEL_NOT_LEF,NULL);
            if (wht==WHAT_COMMAND) { // .proc
                switch (prm) {
                case CMD_PROC:
                    waitfor[++waitforp].what='p';waitfor[waitforp].line=sline;
                    if (!tmp->ref && pass != 1) skipit[waitforp]=0;
                    else {
                        skipit[waitforp]=1;
                        current_context=tmp;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                            fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t%s\n":".%06x\t\t\t\t\t%s\n",address,ident2);
                        }
                        tmp->ref=0;
                    }
                    goto finish;
                case CMD_BLOCK: // .block
                    waitfor[++waitforp].what='b';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                    current_context=tmp;
                    if (listing && flist && arguments.source) {
                        if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t%s\n":".%06x\t\t\t\t\t%s\n",address,ident2);
                    }
                    tmp->ref=0;
                    goto finish;
                case CMD_DSTRUCT: // .dstruct
                    {
                        struct label_s *oldcontext = current_context;
                        current_context=tmp;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                            printllist(flist);
                        }
                        tmp->ref=0;
                        ignore();
                        if (get_ident2()) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (!(tmp2=find_macro(ident)) || tmp2->type!=CMD_STRUCT) {err_msg(ERROR___NOT_DEFINED,ident); goto breakerr;}
                        structrecursion++;
                        macro_recurse(mprm,nprm,'S',tmp2);
                        structrecursion--;
                        current_context=oldcontext;
                        goto finish;
                    }
                }
            }
            wasref=tmp->ref;tmp->ref=0;
        }
        jn:
        switch (wht) {
        case WHAT_STAR:if (skipit[waitforp] & 1) //skip things if needed
            {
                ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                lpoint++;
                wrapwarn=0;wrapwarn2=0;
                get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                if (!c) goto breakerr;
                if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                if (listing && flist && arguments.source) {
                    lastl=LIST_NONE;
                    if (wasref)
                        fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                    else
                        fputs("\n\t\t\t\t\t", flist);
                    printllist(flist);
                }
                if (structrecursion) err_msg(ERROR___NOT_ALLOWED, "*=");
                else if (val.type != T_NONE) {
                    if (val.u.num & ~all_mem) {
                        err_msg(ERROR_CONSTNT_LARGE,NULL);
                    } else {
                        uint32_t ch2=(uint32_t)val.u.num;
                        if (address!=ch2 || l_address!=ch2) {
                            address=l_address=ch2;
                            memjmp(address);
                        }
                    }
                }
            }
            break;
        case WHAT_COMMENT:
        case WHAT_EOL:
            if (listing && flist && arguments.source && (skipit[waitforp] & 1) && wasref) {
                if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                printllist(flist);
            }
            break;
        case WHAT_COMMAND:
            {
                ignore();
                if (listing && flist && arguments.source && (skipit[waitforp] & 1) && prm>=CMD_LONG) {
                    switch (prm) {
                        case CMD_FILL:
                        case CMD_ALIGN:
                        case CMD_OFFS:
                        case CMD_ENDS:
                        case CMD_STRUCT:
                            if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                            printllist(flist);
                        case CMD_BINARY:
                            break;
                        case CMD_PROC:break;
                        case CMD_AS:
                        case CMD_AL:
                        case CMD_XS:
                        case CMD_XL:
                        case CMD_DATABANK:
                        case CMD_DPAGE:
                        case CMD_LOGICAL:
                        case CMD_HERE:
                        case CMD_ENC:
                        case CMD_EOR:
                        case CMD_CPU:
                        case CMD_INCLUDE:
                            if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                            if (wasref)
                                fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t":".%06x\t\t\t\t\t",address);
                            else
                                fputs("\t\t\t\t\t", flist);
                            printllist(flist);
                            break;
                        default:
                            if (wasref) {
                                if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                                fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t%s\n":".%06x\t\t\t\t\t%s\n",address,ident2);
                            }
                    }
                }
                if (prm==CMD_ENDC) { // .endc
                    if (waitfor[waitforp].what!='c') err_msg(ERROR______EXPECTED,".COMMENT");
                    else waitforp--;
                    break;
                } else if (waitfor[waitforp].what=='c') break;
                if (prm==CMD_FI) // .fi
                {
                    if (waitfor[waitforp].what!='e' && waitfor[waitforp].what!='f') err_msg(ERROR______EXPECTED,".IF");
                    else waitforp--;
                    break;
                }
                if (prm==CMD_ELSE) { // .else
                    if (waitfor[waitforp].what=='f') {err_msg(ERROR______EXPECTED,".FI"); break;}
                    if (waitfor[waitforp].what!='e') {err_msg(ERROR______EXPECTED,".IF"); break;}
                    skipit[waitforp]=skipit[waitforp] >> 1;
                    waitfor[waitforp].what='f';waitfor[waitforp].line=sline;
                    break;
                }
                if (prm==CMD_IF || prm==CMD_IFEQ || prm==CMD_IFPL || prm==CMD_IFMI || prm==CMD_ELSIF) { // .if
                    uint8_t skwait = skipit[waitforp];
                    if (prm==CMD_ELSIF) {
                        if (waitfor[waitforp].what!='e') {err_msg(ERROR______EXPECTED,".IF"); break;}
                    } else waitfor[++waitforp].what='e';
                    waitfor[waitforp].line=sline;
                    if (((skwait==1) && prm!=CMD_ELSIF) || ((skwait==2) && prm==CMD_ELSIF)) {
                        get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                        if (!c) goto breakerr;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                        if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for condition");val.type=T_NONE;}
                    } else val.type=T_NONE;
                    switch (prm) {
                    case CMD_ELSIF:
                        if (((val.type == T_INT || val.type == T_CHR) && val.u.num) || (val.type == T_STR && val.u.str.len)) skipit[waitforp]=skipit[waitforp] >> 1; else
                            skipit[waitforp]=skipit[waitforp] & 2;
                        break;
                    case CMD_IF:
                        if (((val.type == T_INT || val.type == T_CHR) && val.u.num) || (val.type == T_STR && val.u.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFEQ:
                        if (((val.type == T_INT || val.type == T_CHR) && !val.u.num) || (val.type == T_STR && !val.u.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFPL:
                        if ((val.type == T_INT && (arguments.tasmcomp ? (~val.u.num & 0x8000) : (val.u.num>=0))) || val.type == T_CHR || (val.type == T_STR && val.u.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFMI:
                        if (val.type == T_INT && (arguments.tasmcomp ? (val.u.num & 0x8000) : (val.u.num < 0))) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    }
                    break;
                }
                if (prm==CMD_ENDM) { // .endm
                    if (waitfor[waitforp].what=='m') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='M') {
                        waitforp--; nobreak=0;
                    } else err_msg(ERROR______EXPECTED,".MACRO or .SEGMENT");
                    break;
                }
                if (prm==CMD_NEXT) { // .next
                    if (waitfor[waitforp].what=='n') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='N') {
                        waitforp--; nobreak=0;
                    } else err_msg(ERROR______EXPECTED,".FOR or .REPT");
                    break;
                }
                if (prm==CMD_PEND) { //.pend
                    if (waitfor[waitforp].what!='p') {err_msg(ERROR______EXPECTED,".PROC"); break;}
                    if (skipit[waitforp] & 1) {
                        if (current_context->parent) {
                            current_context = current_context->parent;
                        } else err_msg(ERROR______EXPECTED,".proc");
                    }
                    waitforp--;
                    break;
                }
                if (prm==CMD_ENDS) { // .ends
                    if (waitfor[waitforp].what=='s') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='S') {
                        waitforp--; nobreak=0;
                    } else err_msg(ERROR______EXPECTED,".STRUCT"); break;
                    break;
                }
                if (prm==CMD_ENDP) { // .endp
                    if (waitfor[waitforp].what!='P') {err_msg(ERROR______EXPECTED,".ENDP"); break;}
                    waitforp--;
                    if (pagelo==-1) {err_msg(ERROR______EXPECTED,".PAGE"); break;}
                    if ((l_address>>8) != (uint32_t)pagelo && fixeddig) {
                        err_msg(ERROR____PAGE_ERROR,(const char *)l_address);
                    }
                    pagelo=-1;
                    break;
                }
                if (!(skipit[waitforp] & 1)) {
                    switch (prm) {
                    case CMD_PAGE: waitfor[++waitforp].what='P';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    case CMD_STRUCT: waitfor[++waitforp].what='s';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    case CMD_MACRO:
                    case CMD_SEGMENT: waitfor[++waitforp].what='m';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    case CMD_FOR:
                    case CMD_REPT: waitfor[++waitforp].what='n';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    case CMD_COMMENT: waitfor[++waitforp].what='c';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    case CMD_PROC: waitfor[++waitforp].what='p';waitfor[waitforp].line=sline;skipit[waitforp]=0; break;
                    }
                    break;//skip things if needed
                }
                if (prm<=CMD_LONG || prm==CMD_BINARY) { // .byte .text .rta .char .int .word .long
                    size_t ptextaddr=mem.p;
                    uint32_t myaddr = address;

                    if (prm<CMD_RTA) {    // .byte .text .ptext .char .shift .shift2 .null
                        int16_t ch2=-1;
                        int large=0;
                        if (prm==CMD_PTEXT) ch2=0;
                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); if (!d) fixeddig=0; //ellenorizve.
                            if (!c) goto breakerr;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                            if (val.type != T_STR || val.u.str.len)
                                do {
                                    if (ch2>=0) pokeb(ch2);

                                    if (val.type == T_STR) {
                                        ch2 = *val.u.str.data++;
                                        val.u.str.len--;
                                    } else if (val.type == T_INT) {
                                        if (prm==CMD_CHAR) {
                                            if (val.u.num>0x7f || val.u.num<-0x80) large=1;
                                        } else {
                                            if (val.u.num & ~0xff) large=1;
                                        }
                                        ch2 = (uint8_t)val.u.num;
                                    } else if (val.type == T_CHR) {
                                        ch2 = (uint8_t)val.u.num;
                                    } else if (val.type == T_NONE) {
                                        ch2 = 0;
                                    } else {
                                        ch2 = 0; err_msg(ERROR____WRONG_TYPE,NULL);
                                    }

                                    if (prm==CMD_SHIFT || prm==CMD_SHIFTL) {
                                        if (ch2>=0x80) large=1;
                                        if (prm==CMD_SHIFTL) ch2<<=1;
                                    } else if (prm==CMD_NULL && !ch2 && d) large=1;
                                } while (val.type == T_STR && val.u.str.len);

                            ignore();if (here()==',') {lpoint++;continue;}
                            if (ch2>=0) {
                                if (prm==CMD_SHIFT) ch2|=0x80;
                                if (prm==CMD_SHIFTL) ch2|=0x01;
                                pokeb(ch2);
                            }
                            if (prm==CMD_NULL) {
                                pokeb(0);
                            }
                            if (prm==CMD_PTEXT) {
                                if (mem.p-ptextaddr>0x100) large=1;

                                if (fixeddig && dooutput) mem.data[ptextaddr]=mem.p-ptextaddr-1;
                            }
                            if (large) err_msg(ERROR_CONSTNT_LARGE,NULL);
                            break;
                        }
                    } else if (prm==CMD_WORD || prm==CMD_INT || prm==CMD_RTA) { // .word .int .rta
                        uint16_t ch2;
                        int large=0;

                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                            if (!c) goto breakerr;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                            if (val.type == T_STR && val.u.str.len < 5) {
                                ch2 = 0;
                                if (val.u.str.len>0) ch2 = val.u.str.data[0];
                                if (val.u.str.len>1) ch2 |= val.u.str.data[1] << 8;
                                if (val.u.str.len>2) large=1;
                            } else if (val.type == T_INT) {
                                if (prm==CMD_INT) {
                                    if (val.u.num>0x7fff || val.u.num<-0x8000) large=1;
                                    ch2 = (uint16_t)val.u.num;
                                } else {
                                    if (val.u.num & ~0xffff) large=1;
                                    ch2 = (uint16_t)val.u.num;
                                    if (prm==CMD_RTA) ch2--;
                                }
                            } else if (val.type == T_CHR) {
                                ch2 = (uint8_t)val.u.num;
                            } else if (val.type == T_NONE) {
                                ch2 = 0;
                            } else {
                                ch2 = 0; err_msg(ERROR____WRONG_TYPE,NULL);
                            }

                            pokeb((uint8_t)ch2);
                            pokeb((uint8_t)(ch2>>8));
                            ignore();if (here()==',') {lpoint++;continue;}
                            if (large) err_msg(ERROR_CONSTNT_LARGE,NULL);
                            break;
                        }
                    } else if (prm==CMD_LONG) { // .long
                        uint32_t ch2;
                        int large=0;

                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                            if (!c) goto breakerr;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                            if (val.type == T_STR && val.u.str.len < 5) {
                                ch2 = 0;
                                if (val.u.str.len>0) ch2 = val.u.str.data[0];
                                if (val.u.str.len>1) ch2 |= val.u.str.data[1] << 8;
                                if (val.u.str.len>2) ch2 |= val.u.str.data[2] << 16;
                                if (val.u.str.len>3) large=1;
                            } else if (val.type == T_INT) {
                                if (val.u.num & ~0xffffff) large=1;
                                ch2 = (uint32_t)val.u.num;
                            } else if (val.type == T_CHR) {
                                ch2 = (uint8_t)val.u.num;
                            } else if (val.type == T_NONE) {
                                ch2 = 0;
                            } else { 
                                ch2 = 0; err_msg(ERROR____WRONG_TYPE,NULL);
                            }

                            pokeb((uint8_t)ch2);
                            pokeb((uint8_t)(ch2>>8));
                            pokeb((uint8_t)(ch2>>16));
                            ignore();if (here()==',') {lpoint++;continue;}
                            if (large) err_msg(ERROR_CONSTNT_LARGE,NULL);
                            break;
                        }
                    } else if (prm==CMD_BINARY) { // .binary
                        uint32_t foffset=0,fsize=all_mem+1;
                        FILE* fil;
                        if (get_path(cfile->name)) goto breakerr;
                        if (here()==',') {
                            lpoint++;
                            get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                            if (!c) goto breakerr;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                            ignore();
                            if (val.type != T_NONE) {
                                if (val.u.num<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                                foffset = val.u.num;
                            }
                            ignore();
                            if (here()==',') {
                                lpoint++;
                                get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                                if (!c) goto breakerr;
                                if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                                if (val.type != T_NONE) {
                                    if (val.u.num<0 || (uint32_t)val.u.num>(all_mem+1)) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}
                                    fsize = val.u.num;
                                }
                            }
                        }

                        if ((fil=fopen(path,"rb"))==NULL) {err_msg(ERROR_CANT_FINDFILE,path);goto breakerr;}
                        fseek(fil,foffset,SEEK_SET);
                        for (;fsize;fsize--) {
                            int st=fgetc(fil);
                            if (st == EOF) break;
                            if (st < 0) err_msg(ERROR_CANT_FINDFILE,path);
                            pokeb((uint8_t)st);
                        }
                        fclose(fil);
                    }

                    if (listing && flist) {
                        unsigned int i, lcol;
                        if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                        lcol=arguments.source?25:49;
                        if (dooutput) {
                            fprintf(flist,(all_mem==0xffff)?">%04x\t":">%06x ",(address-mem.p+ptextaddr) & all_mem);
                            while (ptextaddr!=mem.p) {
                                if (lcol==1) {
                                    if (arguments.source && llist) {
                                        fputc('\t', flist);printllist(flist);
                                    } else fputc('\n',flist);
                                    fprintf(flist,(all_mem==0xffff)?">%04x\t":">%06x ",(address-mem.p+ptextaddr) & all_mem);lcol=49;
                                }
                                fprintf(flist," %02x", mem.data[ptextaddr++]);

                                lcol-=3;
                            }
                        } else fprintf(flist,(all_mem==0xffff)?">%04x\t":">%06x ", myaddr & all_mem);
                
                        if (arguments.source && llist) {
                            for (i=0; i<lcol-1; i+=8) fputc('\t',flist);
                            fputc('\t', flist);printllist(flist);
                        } else fputc('\n',flist);
                    }
                    break;
                }
                if (prm==CMD_OFFS) {   // .offs
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0; //ellenorizve.
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (structrecursion) err_msg(ERROR___NOT_ALLOWED, ".OFFS");
                    if (val.type != T_NONE) {
                        if (val.u.num) {
                            if (fixeddig && scpumode) {
                                if (((address + val.u.num)^address) & ~0xffff) wrapwarn2=1;
                            }
                            address+=val.u.num;
                            if (address>all_mem) {
                                if (fixeddig) wrapwarn=1;
                                address&=all_mem;
                            }
                            memjmp(address);
                        }
                    }
                    break;
                }
                if (prm==CMD_LOGICAL) { // .logical
                    if (logitab.p >= logitab.len) {
                        logitab.len += 16;
                        if (!(logitab.data=realloc(logitab.data,logitab.len*sizeof(*logitab.data)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
                    }
                    logitab.data[logitab.p++]=l_address-address;
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (structrecursion) err_msg(ERROR___NOT_ALLOWED, ".LOGICAL");
                    if (val.type != T_NONE) {
                        if (val.u.num & ~all_mem) err_msg(ERROR_CONSTNT_LARGE,NULL);
                        else l_address=val.u.num;
                    }
                    break;
                }
                if (prm==CMD_HERE) { // .here
                    if (!logitab.p) err_msg(ERROR______EXPECTED,".LOGICAL");
                    else if (structrecursion) err_msg(ERROR___NOT_ALLOWED, ".HERE");
                    else l_address=address+logitab.data[--logitab.p];
                    break;
                }
                if (prm==CMD_AS) { // .as
                    longaccu=0;
                    break;
                }
                if (prm==CMD_AL) { // .al
                    longaccu=1;
                    break;
                }
                if (prm==CMD_XS) { // .xs
                    longindex=0;
                    break;
                }
                if (prm==CMD_XL) { // .xl
                    longindex=1;
                    break;
                }
                if (prm==CMD_ERROR) { // .error
                    err_msg(ERROR__USER_DEFINED,(char *)&pline[lpoint]);
                    goto breakerr;
                }
                if (prm==CMD_BLOCK) { // .block
                    sprintf(varname, ".%x.%x", (unsigned)star_tree, vline);
                    current_context=new_label(varname, L_LABEL);
                    current_context->value.type = T_NONE;
                    break;
                }
                if (prm==CMD_BEND) { //.bend
                    if (waitfor[waitforp].what=='b') waitforp--;
                    if (current_context->parent) {
                        current_context = current_context->parent;
                    } else err_msg(ERROR______EXPECTED,".block");
                    break;
                }
                if (prm==CMD_DATABANK) { // .databank
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        if (val.u.num & ~0xff) err_msg(ERROR_CONSTNT_LARGE,NULL);
                        else databank=val.u.num;
                    }
                    break;
                }
                if (prm==CMD_DPAGE) { // .dpage
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        if (val.u.num & ~0xffff) err_msg(ERROR_CONSTNT_LARGE,NULL);
                        else {
                            if (dtvmode) dpage=val.u.num & 0xff00;
                            else dpage=val.u.num;
                        }
                    }
                    break;
                }
                if (prm==CMD_FILL) { // .fill
                    uint32_t db = 0;
                    uint8_t ch;
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        db=val.u.num;
                        if (db>(all_mem+1)) {err_msg(ERROR_CONSTNT_LARGE,NULL);goto breakerr;}
                    }
                    ignore();
                    if (here()==',') {
                        lpoint++;
                        get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                        if (!c) goto breakerr;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                        if (val.type != T_NONE) {
                            if (val.u.num & ~0xff) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                            ch = (uint8_t)val.u.num;
                        } else ch = 0;
                        while (db-->0) pokeb(ch);
                    } else {
                        if (fixeddig && scpumode) {
                            if (((address + db)^address) & ~0xffff) wrapwarn2=1;
                            if (((l_address + db)^l_address) & ~0xffff) wrapwarn2=1;
                        }
                        l_address+=db;
                        if (l_address>all_mem) {
                            if (fixeddig) wrapwarn=1;
                            l_address&=all_mem;
                        }
                        address+=db;
                        if (address>all_mem) {
                            if (fixeddig) wrapwarn=1;
                            address&=all_mem;
                        }
                        memjmp(address);
                    }
                    break;
                }
                if (prm==CMD_ASSERT) { // .assert
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,",");goto breakerr;}
                    lpoint++;
                    if (val.type != T_NONE) {
                        current_provides=val.u.num;
                    } else current_provides=~0;
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,",");goto breakerr;}
                    lpoint++;
                    if (val.type != T_NONE) {
                        current_requires=val.u.num;
                    } else current_requires=0;
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        current_conflicts=val.u.num;
                    } else current_conflicts=0;
                    break;
                }
                if (prm==CMD_CHECK) { // .check
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,",");goto breakerr;}
                    lpoint++;
                    if (val.type != T_NONE) {
                        if ((val.u.num & current_provides) ^ val.u.num) err_msg(ERROR_REQUIREMENTS_,".CHECK");
                    }
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        if (val.u.num & current_provides) err_msg(ERROR______CONFLICT,".CHECK");
                    }
                    break;
                }
                if (prm==CMD_WARN) { // .warn
                    err_msg(ERROR_WUSER_DEFINED,(char *)&pline[lpoint]);
                    goto breakerr;
                }
                if (prm==CMD_ENC) { // .enc
                    if (get_path(NULL)) goto breakerr;
                    if (!strcasecmp(path,"none")) encoding=0;
                    else
                        if (!strcasecmp(path,"screen")) encoding=1;
                        else
                            err_msg(ERROR_UNKNOWN_ENCOD,ident);
                    break;
                }
                if (prm==CMD_CPU) { // .cpu
                    int def;
                    if (get_path(NULL)) goto breakerr;
                    def=arguments.cpumode;
                    if (!strcmp(path,"6502")) def=OPCODES_6502;
                    else if (!strcasecmp(path,"65c02")) def=OPCODES_65C02;
                    else if (!strcasecmp(path,"6502i")) def=OPCODES_6502i;
                    else if (!strcmp(path,"65816")) def=OPCODES_65816;
                    else if (!strcasecmp(path,"65dtv02")) def=OPCODES_65DTV02;
                    else if (strcasecmp(path,"default")) err_msg(ERROR___UNKNOWN_CPU,ident);
                    set_cpumode(def);
                    break;
                }
                if (prm==CMD_CERROR || prm==CMD_CWARN) { // .cerror
                    get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    ignore();
                    if (here()==',') {
                        lpoint++;ignore();
                    }
                    if (((val.type == T_INT || val.type == T_CHR) && val.u.num) || (val.type == T_STR && val.u.str.len)) err_msg((prm==CMD_CERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED,(char *)&pline[lpoint]);
                    goto breakerr;
                }
                if (prm==CMD_REPT) { // .rept
                    int32_t cnt;
                    waitfor[++waitforp].what='n';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    get_exp(&w,&d,&c,&val,T_INT);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for count");val.type=T_NONE;}
                    cnt = 0;
                    if (val.type != T_NONE) {
                        if (cnt<val.u.num) {
                            size_t pos = cfile->p;
                            uint32_t lin = sline;
                            struct star_s *s = new_star(vline);
                            struct avltree *stree_old = star_tree;
                            uint32_t ovline = vline;

                            waitforp--;
                            if (labelexists && s->addr != star) fixeddig=0;
                            s->addr = star;
                            star_tree = &s->tree;vline=0;
                            for (; cnt<val.u.num; cnt++) {
                                sline=lin;cfile->p=pos;
                                waitfor[++waitforp].what='N';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                                compile(mprm,nprm);
                            }
                            star_tree = stree_old; vline = ovline;
                        }
                    }
                    break;
                }
                if (prm==CMD_ALIGN) { // .align
                    int align, fill=-1;
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (structrecursion) err_msg(ERROR___NOT_ALLOWED, ".ALIGN");
                    if (val.type != T_NONE) {
                        if (val.u.num<1 || val.u.num>(int32_t)all_mem) {
                            err_msg(ERROR_CONSTNT_LARGE,NULL);
                            goto breakerr;
                        }
                        align = val.u.num;
                    } else align = 1;
                    ignore();
                    if (here()==',') {
                        int d2;
                        lpoint++;
                        get_exp(&w,&d2,&c,&val,T_INT);if (!d2) fixeddig=0;
                        if (!c) goto breakerr;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                        if (val.type != T_NONE) {
                            if (val.u.num & ~0xff) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                            fill = val.u.num;
                        } else fill = 0;
                    }
                    if (align>1 && (l_address % align)) {
                        if (fill>0)
                            while (l_address % align) pokeb((unsigned char)fill);
                        else {
                            align-=l_address % align;
                            if (align) {
                                if (fixeddig && scpumode) {
                                    if (((address + align)^address) & ~0xffff) wrapwarn2=1;
                                    if (((l_address + align)^l_address) & ~0xffff) wrapwarn2=1;
                                }
                                l_address+=align;
                                if (l_address>all_mem) {
                                    if (fixeddig) wrapwarn=1;
                                    l_address&=all_mem;
                                }
                                address+=align;
                                if (address>all_mem) {
                                    if (fixeddig) wrapwarn=1;
                                    address&=all_mem;
                                }
                                memjmp(address);
                            }
                        }
                    }
                    break;
                }
                if (prm==CMD_EOR) {   // .eor
                    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (val.type != T_NONE) {
                        if (val.u.num & ~0xff) err_msg(ERROR_CONSTNT_LARGE,NULL);
                        else outputeor = val.u.num;
                    } else outputeor = 0;
                    break;
                }
                if (prm==CMD_END) {
                    nobreak=0;
                    break;
                }
                if (prm==CMD_PRON) {
                    listing = (flist != NULL);
                    break;
                }
                if (prm==CMD_PROFF) {
                    listing = 0;
                    break;
                }
                if (prm==CMD_SHOWMAC || prm==CMD_HIDEMAC) {
                    err_msg(ERROR_DIRECTIVE_IGN,NULL);
                    break;
                }
                if (prm==CMD_COMMENT) { // .comment
                    waitfor[++waitforp].what='c';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    break;
                }
                if (prm==CMD_INCLUDE) { // .include
                    struct file_s *f;
                    if (get_path(cfile->name)) goto breakerr;
                    if (listing && flist) {
                        fprintf(flist,"\n;******  Processing file \"%s\"\n",path);
                        lastl=LIST_NONE;
                    }
                    f = cfile;
                    cfile = openfile(path);
                    if (cfile->open>1) {
                        err_msg(ERROR_FILERECURSION,NULL);
                    } else {
                        uint32_t lin = sline;
                        uint32_t vlin = vline;
                        struct avltree *stree_old = star_tree;
                        uint32_t old_backr = backr, old_forwr = forwr;

                        enterfile(cfile->name,sline);
                        sline = vline = 0; cfile->p=0;
                        star_tree = &cfile->star;
                        backr = forwr = 0;
                        reffile=cfile->uid;
                        compile(mprm,nprm);
                        sline = lin; vline = vlin;
                        star_tree = stree_old;
                        backr = old_backr; forwr = old_forwr;
                        exitfile();
                    }
                    closefile(cfile);cfile = f;
                    reffile=cfile->uid;
                    if (listing && flist) {
                        fprintf(flist,"\n;******  Return to file \"%s\"\n",cfile->name);
                        lastl=LIST_NONE;
                    }
                    break;
                }
                if (prm==CMD_FOR) { // .for
                    size_t pos, xpos;
                    uint32_t lin, xlin;
                    int apoint, bpoint = -1;
                    uint8_t expr[linelength];
                    struct label_s *var;
                    struct star_s *s;
                    struct avltree *stree_old;
                    uint32_t ovline;

                    waitfor[++waitforp].what='n';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    if (strlen((char *)pline)>=linelength) {err_msg(ERROR_LINE_TOO_LONG,NULL);goto breakerr;}
                    if ((wht=what(&prm))==WHAT_EXPRESSION && prm==1) { //label
                        if (get_ident()) goto breakerr;
                        ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                        lpoint++;
                        strcpy(varname,ident);
                        get_exp(&w,&d,&c,&val,T_NONE);
                        if (!c) goto breakerr;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                        var=new_label(varname, L_VAR);
                        if (labelexists) {
                            if (var->type != L_VAR) err_msg(ERROR_DOUBLE_DEFINE,varname);
                            else {
                                var->requires=current_requires;
                                var->conflicts=current_conflicts;
                                var_assign(var, &val, fixeddig);
                            }
                        } else {
                            var->requires=current_requires;
                            var->conflicts=current_conflicts;
                            var->upass=var->pass=pass;
                            var->value=val;
                            if (val.type == T_STR) {
                                var->value.u.str.data=malloc(val.u.str.len);
                                memcpy(var->value.u.str.data,val.u.str.data,val.u.str.len);
                            }
                            if (!d) err_msg(ERROR___NOT_DEFINED,"argument used");
                        }
                        wht=what(&prm);
                    }
                    if (wht==WHAT_S || wht==WHAT_Y || wht==WHAT_X) lpoint--; else
                        if (wht!=WHAT_COMA) {err_msg(ERROR______EXPECTED,","); goto breakerr;}

                    s = new_star(vline); stree_old = star_tree; ovline = vline;
                    if (labelexists && s->addr != star) fixeddig=0;
                    s->addr = star;
                    star_tree = &s->tree;vline=0;
                    xlin=lin=sline; xpos=pos=cfile->p; apoint=lpoint;
                    strcpy((char *)expr, (char *)pline);var = NULL;
                    for (;;) {
                        lpoint=apoint;
                        get_exp(&w,&d,&c,&val,T_NONE);
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used in condition");break;}
                        if (((val.type == T_INT || val.type == T_CHR) && !val.u.num) || (val.type == T_STR && !val.u.str.len)) break;
                        if (bpoint < 0) {
                            ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                            lpoint++;
                            if (get_ident()) break;
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                            lpoint++;
                            ignore();
                            if (!here() || here()==';') bpoint = 0;
                            else {
                                var=new_label(ident, L_VAR);
                                if (labelexists) {
                                    if (var->type != L_VAR) {
                                        err_msg(ERROR_DOUBLE_DEFINE,varname);
                                        break;
                                    }
                                    var->requires=current_requires;
                                    var->conflicts=current_conflicts;
                                } else {
                                    var->requires=current_requires;
                                    var->conflicts=current_conflicts;
                                    var->upass=var->pass=pass;
                                    var->value.type=T_NONE;
                                }
                                bpoint=lpoint;
                            }
                        }
                        waitfor[++waitforp].what='N';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                        compile(mprm,nprm);
                        xpos = cfile->p; xlin= sline;
                        pline = expr;
                        sline=lin;cfile->p=pos;
                        if (bpoint) {
                            lpoint=bpoint;
                            get_exp(&w,&d,&c,&val,T_NONE);
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            var_assign(var, &val, fixeddig);
                        }
                    }
                    if (pos!=xpos || lin!=xlin) waitforp--;
                    sline=xlin;cfile->p=xpos;
                    star_tree = stree_old; vline = ovline;
                    goto breakerr;
                }
                if (prm==CMD_PAGE) { // .page
                    waitfor[++waitforp].what='P';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                    if (pagelo!=-1) err_msg(ERROR______EXPECTED,".ENDP");
                    else pagelo=(l_address>>8);
                    break;
                }
                if (prm==CMD_OPTION) { // .option
                    get_ident();
                    ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); goto breakerr;}
                    lpoint++;
                    get_exp(&w,&d,&c,&val,T_NONE);
                    if (!c) goto breakerr;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); goto breakerr;}
                    if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for option");goto breakerr;}
                    if (!strcasecmp(ident,"allow_branch_across_page")) allowslowbranch=(((val.type == T_INT || val.type == T_CHR) && val.u.num) || (val.type == T_STR && val.u.str.len));
                    else if (!strcasecmp(ident,"auto_longbranch_as_jmp")) longbranchasjmp=(((val.type == T_INT || val.type == T_CHR) && val.u.num) || (val.type == T_STR && val.u.str.len));
                    else err_msg(ERROR_UNKNOWN_OPTIO,ident);
                    break;
                }
                if (prm==CMD_GOTO) { // .goto
                    struct jump_s *tmp2;
                    int noerr = 1;
                    get_ident();
                    tmp2 = find_jump(ident);
                    if (tmp2 && tmp2->file == cfile && tmp2->parent == current_context) {
                        uint8_t oldwaitforp = waitforp;
                        while (tmp2->waitforp < waitforp) {
                            uint32_t os = sline;
                            sline = waitfor[waitforp].line;
                            switch (waitfor[waitforp--].what) {
                            case 'M':
                            case 'm': err_msg(ERROR______EXPECTED,".ENDM"); noerr = 0; break;
                            case 'N':
                            case 'n': err_msg(ERROR______EXPECTED,".NEXT"); noerr = 0; break;
                            case 'p': err_msg(ERROR______EXPECTED,".PEND"); noerr = 0; break;
                            case 'b': err_msg(ERROR______EXPECTED,".BEND"); noerr = 0; break;
                            case 'S':
                            case 's': err_msg(ERROR______EXPECTED,".ENDS"); noerr = 0; break;
                            case 'P': err_msg(ERROR______EXPECTED,".ENDP"); noerr = 0; break;
                            }
                            sline = os;
                        }
                        if (noerr) {
                            sline = tmp2->sline;
                            cfile->p = tmp2->p;
                        } else waitforp = oldwaitforp;
                    } else err_msg(ERROR___NOT_DEFINED,ident);
                    break;
                }
                if (prm==CMD_MACRO || prm==CMD_SEGMENT) {
                    waitfor[++waitforp].what='m';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    err_msg(ERROR___NOT_DEFINED,ident);
                    break;
                }
                if (prm==CMD_PROC) {
                    waitfor[++waitforp].what='p';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    err_msg(ERROR___NOT_DEFINED,ident);
                    break;
                }
                if (prm==CMD_STRUCT) {
                    waitfor[++waitforp].what='s';waitfor[waitforp].line=sline;skipit[waitforp]=0;
                    structrecursion++;
                    if (structrecursion<100) {
                        waitforp--;
                        waitfor[++waitforp].what='S';waitfor[waitforp].line=sline;skipit[waitforp]=1;
                        compile(mprm,nprm);
                    } else err_msg(ERROR__MACRECURSION,"!!!!");
                    structrecursion--;
                    break;
                }
                if (prm==CMD_DSTRUCT) { 
                    if (get_ident2()) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (!(tmp2=find_macro(ident)) || tmp2->type!=CMD_STRUCT) {err_msg(ERROR___NOT_DEFINED,ident); goto breakerr;}
                    structrecursion++;
                    macro_recurse(mprm,nprm,'S',tmp2);
                    structrecursion--;
                    break;
                }
            }
        case WHAT_HASHMARK:if (skipit[waitforp] & 1) //skip things if needed
            {                   //macro stuff
                int ppoint, nprm;
                char mparams[256];
                struct label_s *old_context;

                if (get_ident2()) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (!(tmp2=find_macro(ident))) {err_msg(ERROR___NOT_DEFINED,ident); goto breakerr;}
            as_macro:
                if (listing && flist && arguments.source && wasref) {
                    if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                    fprintf(flist,(all_mem==0xffff)?".%04x\t\t\t\t\t%s\n":".%06x\t\t\t\t\t%s\n",address,ident2);
                }
                ppoint=nprm=0;
                ignore();
                while ((ch=here()) && ch!=';') {
                    lpoint++;
                    if (ch=='"' || ch=='\'') {
                        char quo=ch;
                        for (;;) {
                            if (!(ch=here())) {err_msg(ERROR______EXPECTED,"End of string"); goto breakerr;}
                            lpoint++;
                            if (ch==quo) {
                                if (here()!=quo) break;
                                lpoint++;
                            }
                            mparams[ppoint++]=ch;
                        }
                    }
                    else {
                        do mparams[ppoint++]=ch; while ((ch=get())!=',' && ch && ch != ';');
                        lpoint--;
                    }
                    nprm++;
                    mparams[ppoint++]=0;
                    ignore();
                    if (!here() || here()==';') break;
                    if (here()!=',') {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    lpoint++;
                }
                if (tmp2->type==CMD_MACRO) {
                    sprintf(varname, "#%x#%x", (unsigned)star_tree, vline);
                    old_context = current_context;
                    current_context=new_label(varname, L_LABEL);
                    current_context->value.type = T_NONE;
                } else old_context = NULL;
                macro_recurse(mparams,nprm,'M',tmp2);
                if (tmp2->type==CMD_MACRO) current_context = old_context;
                break;
            }
        case WHAT_EXPRESSION:
            if (skipit[waitforp] & 1) {
                get_ident2();
                if ((prm=lookup_opcode(ident))>=0) {
                    enum opr_e opr;
                    int mnem, oldlpoint;
                    const uint8_t *cnmemonic; //current nmemonic
                    int_fast8_t ln;
                    uint8_t cod, longbranch;
                    uint32_t adr;
                as_opcode:

                    opr = 0;mnem = prm;
                    oldlpoint = lpoint;
                    cnmemonic = &opcode[prm*24];
                    ln = 0; cod = 0; longbranch = 0; adr = 0;

                    ignore();
                    if (!(wht=here()) || wht==';') {
                        opr=(cnmemonic[ADR_ACCU]==cnmemonic[ADR_IMPLIED])?ADR_ACCU:ADR_IMPLIED;w=ln=0;d=1;
                    }  //clc
                    // 1 Db
                    else if (lowcase(wht)=='a' && cnmemonic[ADR_ACCU]!=____ && (!pline[lpoint+1] || pline[lpoint+1]==';' || pline[lpoint+1]==0x20 || pline[lpoint+1]==0x09))
                    {
                        unsigned int opoint=lpoint;
                        lpoint++;ignore();
                        if (here() && here()!=';') {lpoint=opoint;goto nota;}
                        if (find_label("a")) err_msg(ERROR_A_USED_AS_LBL,NULL);
                        opr=ADR_ACCU;w=ln=0;d=1;// asl a
                    }
                    // 2 Db
                    else if (wht=='#') {
                        if ((cod=cnmemonic[(opr=ADR_IMMEDIATE)])==____) {
                            lpoint += strlen((char *)pline + lpoint);ln=w=d=1;
                        } else {
                            lpoint++;
                            get_exp(&w,&d,&c,&val,T_INT); //ellenorizve.
                            if (!c) goto breakerr;
                            if (c==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}

                            ln=1;
                            if (cod==0xE0 || cod==0xC0 || cod==0xA2 || cod==0xA0) {// cpx cpy ldx ldy
                                if (longindex && scpumode) ln++;
                            }
                            else if (cod==0xF4) ln=2; //pea #$ffff
                            else if (cod!=0xC2 && cod!=0xE2) {//not sep rep=all accu
                                if (longaccu && scpumode) ln++;
                            }
                            if (dtvmode && cod==0x02) longbranch=0x40;//hack

                            if (val.type != T_NONE) {
                                adr=val.u.num;
                                if (w==3) w=val_length(adr);//auto length
                                if (w>=ln) w=3; //const too large
                            } else fixeddig=0;
                        }
                    }
                    // 3 Db
                    else {
                        if (whatis[wht]!=WHAT_EXPRESSION && whatis[wht]!=WHAT_CHAR && wht!='_' && wht!='*') {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    nota:
                        get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0; //ellenorizve.
                        if (!c) goto breakerr;
                        if (val.type != T_NONE) {
                            adr = val.u.num;
                        }
                    meg:
                        if (c==1) {
                            if ((wht=what(&prm))==WHAT_X) {// lda $ff,x lda $ffff,x lda $ffffff,x
                                if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                                if (w==3) {//auto length
                                    if (val.type != T_NONE) {
                                        if (cnmemonic[ADR_ZP_X]!=____ && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) {adr=(uint16_t)(adr - dpage);w=0;}
                                        else if (cnmemonic[ADR_ADDR_X]!=____ && databank==(adr >> 16)) w=1;
                                        else {
                                            w=val_length(adr);
                                            if (w<2) w=2;
                                        }
                                    } else w=(cnmemonic[ADR_ADDR_X]!=____);
                                } else {
                                    if (!w && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) adr=(uint16_t)(adr - dpage);
                                    if (databank==(adr >> 16) && w<2) adr&=0xffff;
                                    if (w<val_length(adr)) w=3;
                                }
                                opr=ADR_ZP_X-w;ln=w+1;
                            }// 6 Db
                            else if (wht==WHAT_Y) {// lda $ff,y lda $ffff,y lda $ffffff,y
                                if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                                if (w==3) {//auto length
                                    if (val.type != T_NONE) {
                                        if (cnmemonic[ADR_ZP_Y]!=____ && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) {adr=(uint16_t)(adr - dpage);w=0;}
                                        else if (databank==(adr >> 16)) w=1;
                                    } else w=(cnmemonic[ADR_ADDR_Y]!=____);
                                } else {
                                    if (!w && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) adr=(uint16_t)(adr - dpage);
                                    if (databank==(adr >> 16) && w<2) adr&=0xffff;
                                    if (w<val_length(adr)) w=3;
                                }
                                if (w==2) w=3; // there's no lda $ffffff,y!
                                opr=ADR_ZP_Y-w;ln=w+1; // ldx $ff,y lda $ffff,y
                            }// 8 Db
                            else if (wht==WHAT_S) {
                                if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                                if (val.type != T_NONE) {
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w) w=3; // there's no lda $ffffff,s or lda $ffff,s!
                                    opr=ADR_ZP_S;
                                }
                                ln=1; // lda $ff,s
                            }// 9 Db
                            else if (wht==WHAT_COMA) { // mvp $10,$20
                                int w2,c2,d2;
                                struct value_s val2;
                                megint:
                                d2=d;
                                get_exp(&w2,&d,&c2,&val2,T_INT);if (!d) fixeddig=0;
                                if (!c2) goto breakerr;
                                if (c2==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                if (cnmemonic[ADR_REL]!=____) {
                                    int32_t valx,valx2;
                                    if (val.type != T_NONE && val2.type != T_NONE) {
                                        valx=val.u.num-l_address-2;
                                        valx2=val2.u.num-l_address-2;
                                        if ((valx<-128 || valx>127) && valx2>=-128 && valx2<=127) {
                                            val=val2;
                                            adr = val.u.num;
                                            c=c2;
                                            w=w2;
                                        } else d=d2;
                                    } else {
                                        if (d2) d=d2;
                                        else {
                                            val=val2;
                                            if (val2.type != T_NONE) adr = val.u.num;
                                            c=c2;w=w2;
                                        }
                                    }
                                    goto meg;
                                }
                                adr <<= 8;
                                if (val.type != T_NONE && val2.type != T_NONE) {
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w2==3) w2=val_length(val2.u.num);//auto length
                                    if (w || w2) w=3; // only byte operands...
                                    opr=ADR_MOVE;
                                    adr|=val2.u.num;
                                }
                                ln=2;
                            }// 10 Db
                            else if (wht==WHAT_EOL || wht==WHAT_COMMENT) {
                                if (cnmemonic[ADR_REL]!=____) {
                                    struct star_s *s;
                                    int olabelexists;
                                    s=new_star(vline+1);olabelexists=labelexists;
                                    ln=1;opr=ADR_REL;
                                    if (val.type != T_NONE) {
                                        uint16_t oadr = adr;
                                        if (fixeddig && (l_address >> 16)!=(adr >> 16)) err_msg(ERROR_BRANCH_TOOFAR,NULL);

                                        if (labelexists && adr >= s->addr) {
                                            adr=(uint16_t)(adr - s->addr);
                                        } else {
                                            adr=(uint16_t)(adr - l_address - 2);labelexists=0;
                                        }
                                        if (adr<0xFF80 && adr>0x007F) {
                                            if (arguments.longbranch && (cnmemonic[ADR_ADDR]==____)) {
                                                if ((cnmemonic[ADR_REL] & 0x1f)==0x10) {//branch
                                                    longbranch=0x20;ln=4;
                                                    if (scpumode && !longbranchasjmp) {
                                                        if (!labelexists) adr=(uint16_t)(adr-3);
                                                        adr=0x8203+(adr << 16);
                                                    } else {
                                                        adr=0x4C03+(oadr << 16);
                                                    }
                                                } else {//bra
                                                    if (scpumode && !longbranchasjmp) {
                                                        longbranch=cnmemonic[ADR_REL]^0x82;
                                                        if (!labelexists) adr=(uint16_t)(adr-1);
                                                        ln=2;
                                                    } else {
                                                        longbranch=cnmemonic[ADR_REL]^0x4C;
                                                        adr=oadr;ln=2;
                                                    }
                                                }
                                                if (fixeddig) err_msg(ERROR___LONG_BRANCH,NULL);
                                            } else {
                                                if (cnmemonic[ADR_ADDR]!=____) {
                                                    if (scpumode && !longbranchasjmp) {
                                                        longbranch=cnmemonic[ADR_REL]^0x82;
                                                        if (!labelexists) adr=(uint16_t)(adr-1);
                                                    } else {
                                                        adr=oadr;
                                                        opr=ADR_ADDR;
                                                    }
                                                    ln=2;
                                                } else if (fixeddig) err_msg(ERROR_BRANCH_TOOFAR,NULL);
                                            }
                                        } else {
                                            if (fixeddig) {
                                                if (!longbranch && ((l_address+2) & 0xff00)!=(oadr & 0xff00)) {
                                                    if (!allowslowbranch) err_msg(ERROR__BRANCH_CROSS,NULL);
                                                }
                                            }
                                            if (cnmemonic[ADR_ADDR]!=____) {
                                                if (adr==0) ln=-1;
                                                else if (adr==1 && (cnmemonic[ADR_REL] & 0x1f)==0x10) {
                                                ln=0;longbranch=0x20;adr=0x10000;
                                                }
                                            }
                                        }
                                    }
                                    w=0;// bne
                                    if (olabelexists && s->addr != ((star + 1 + ln) & all_mem)) fixeddig=0;
                                    s->addr = (star + 1 + ln) & all_mem;
                                }
                                else if (cnmemonic[ADR_REL_L]!=____) {
                                    if (val.type != T_NONE) {
                                        if (fixeddig && (l_address >> 16)!=(adr >> 16)) err_msg(ERROR_BRANCH_TOOFAR,NULL);
                                        adr=(uint16_t)(adr-l_address-3);
                                    }
                                    ln=2;opr=ADR_REL_L;w=1;//brl
                                }
                                else if (cnmemonic[ADR_LONG]==0x5C) {
                                    if (w==3) {
                                        if (cnmemonic[ADR_ADDR]==____) w=2; // jml
                                        else {
                                            if (val.type != T_NONE) {
                                                if ((l_address >> 16)==(adr >> 16)) w=1;
                                                else {
                                                    w=val_length(adr);
                                                    if (w<2) w=2; // in another bank
                                                }
                                            } else w=1;
                                        }
                                    }
                                    opr=ADR_ZP-w;ln=w+1;
                                }
                                else if (cnmemonic[ADR_ADDR]==0x20) {
                                    if (val.type != T_NONE) {
                                        if (fixeddig && (l_address >> 16)!=(adr >> 16)) err_msg(ERROR_BRANCH_TOOFAR,NULL);
                                        adr^=(l_address & ~0xffff);
                                        if (w==3) {
                                            w=val_length(adr);//auto length
                                            if (!w) w=1; // there's no jsr $ff
                                        }
                                        if (w!=1) w=3; // there's no jsr $ffffff!
                                    }
                                    opr=ADR_ADDR;ln=2;
                                }
                                else {
                                    if (w==3) {//auto length
                                        if (val.type != T_NONE) {
                                            if (cnmemonic[ADR_ZP]!=____ && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) {adr=(uint16_t)(adr - dpage);w=0;}
                                            else if (cnmemonic[ADR_ADDR]!=____ && databank==(adr >> 16)) w=1;
                                            else {
                                                w=val_length(adr);
                                                if (w<2) w=2;
                                            }
                                        } else w=1;
                                    } else {
                                        if (!w && adr < 0x10000 && (uint16_t)(adr - dpage) < 0x100) adr=(uint16_t)(adr - dpage);
                                        if (databank==(adr >> 16) && w<2) adr&=0xffff;
                                        if (w<val_length(adr)) w=3;
                                    }
                                    opr=ADR_ZP-w;ln=w+1; // lda $ff lda $ffff lda $ffffff
                                }
                            } else w=0; // 13+2 Db
                        }
                        else if (c==2) {
                            if ((wht=what(&prm))==WHAT_SZ) {
                                if ((wht=what(&prm))!=WHAT_Y) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                if (val.type != T_NONE) {
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w) w=3; // there's no lda ($ffffff,s),y or lda ($ffff,s),y!
                                    opr=ADR_ZP_S_I_Y;
                                }
                                ln=1; // lda ($ff,s),y
                            } // 16 Db
                            else {
                                if (wht!=WHAT_XZ) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                if (cnmemonic[ADR_ADDR_X_I]==0x7C || cnmemonic[ADR_ADDR_X_I]==0xFC) {// jmp ($ffff,x) jsr ($ffff,x)
                                    if (val.type != T_NONE) {
                                        if ((l_address >> 16)==(adr >> 16)) w=1; else w=3; // only same program bank!
                                        opr=ADR_ADDR_X_I;
                                    }
                                    ln=2; // jmp ($ffff,x)
                                }
                                else {
                                    if (val.type != T_NONE) {
                                        adr=(uint16_t)(adr - dpage) | (adr & ~0xffff);
                                        if (w==3) w=val_length(adr);//auto length
                                        if (w) w=3; // there's no lda ($ffff,x) lda ($ffffff,x)!
                                        opr=ADR_ZP_X_I;
                                    }
                                    ln=1; // lda ($ff,x)
                                }
                            } // 18 Db
                        }
                        else if (c==3) {
                            if ((wht=what(&prm))==WHAT_Y) {
                                if (val.type != T_NONE) {
                                    adr=(uint16_t)(adr - dpage) | (adr & ~0xffff);
                                    if (w==3) w=val_length(adr);
                                    if (w) w=3;
                                    opr=ADR_ZP_I_Y;
                                }
                                ln=1; // lda ($ff),y
                            } // 19 Db
                            else if (wht==WHAT_EOL || wht==WHAT_COMMENT) {
                                if (cnmemonic[ADR_ADDR_I]==0x6C) {// jmp ($ffff)
                                    if (val.type != T_NONE) {
                                        if (fixeddig) {
                                            if (w==3) {
                                                w=val_length(adr);//auto length
                                                if (!w) w=1;
                                            }
                                            if (w!=1) w=3; // there's no jmp ($ffffff)!
                                            if ((opcode!=c65816 && opcode!=c65c02) && (adr & 0xff)==0xff) err_msg(ERROR______JUMP_BUG,NULL);//jmp ($xxff)
                                        } else w=1;
                                        opr=ADR_ADDR_I;
                                    }
                                    ln=2; // jmp ($ffff)
                                }
                                else {
                                    if (val.type != T_NONE) {
                                        adr=(uint16_t)(adr - dpage) | (adr & ~0xffff);
                                        if (w==3) w=val_length(adr);//auto length
                                        if (w) w=3; // there's no lda ($ffff) lda ($ffffff)!
                                        opr=ADR_ZP_I;
                                    }
                                    ln=1; // lda ($ff)
                                }
                            } // 21 Db
                        } else {
                            if ((wht=what(&prm))==WHAT_Y) {
                                if (val.type != T_NONE) {
                                    adr = (uint16_t)(val.u.num - dpage) | (adr && ~0xffff);
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w) w=3;// there's no lda [$ffff],y lda [$ffffff],y!
                                    opr=ADR_ZP_LI_Y;
                                } else fixeddig=0;
                                ln=1; // lda [$ff],y
                            }
                            else if (wht==WHAT_EOL || wht==WHAT_COMMENT) {
                                if (cnmemonic[ADR_ADDR_LI]==0xDC) { // jmp [$ffff]
                                    if (val.type != T_NONE) {
                                        adr = val.u.num;
                                        if (w==3) {
                                            w=val_length(adr);//auto length
                                            if (!w) w=1;
                                        }
                                        if (w!=1) w=3; // there's no jmp [$ffffff]!
                                        opr=ADR_ADDR_LI;
                                    } else fixeddig=0;
                                    ln=2;// jmp [$ffff]
                                }
                                else {
                                    if (val.type != T_NONE) {
                                        adr = (uint16_t)(val.u.num - dpage) | (adr && ~0xffff);
                                        if (w==3) w=val_length(adr);//auto length
                                        if (w) w=3; // there's no lda [$ffff] lda [$ffffff]!
                                        opr=ADR_ZP_LI;
                                    } else fixeddig=0;
                                    ln=1;// lda [$ff]
                                }
                            }
                        }
                    }

                    if (d) {
                        if (w==3) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                        if ((cod=cnmemonic[opr])==____) {
                            memcpy(ident,&mnemonic[mnem*3],3);
                            ident[3]=0;
                            if ((tmp2=find_macro(ident))) {
                                lpoint=oldlpoint;
                                goto as_macro;
                            }
                            err_msg(ERROR_ILLEGAL_OPERA,NULL);
                            goto breakerr;
                        }
                    }
                    if (ln>=0) {
                        uint32_t temp=adr;
                        pokeb(cod ^ longbranch);
                        switch (ln)
                        {
                        case 4:pokeb((uint8_t)temp);temp>>=8;
                        case 3:pokeb((uint8_t)temp);temp>>=8;
                        case 2:pokeb((uint8_t)temp);temp>>=8;
                        case 1:pokeb((uint8_t)temp);
                        }
                    }

                    if (listing && flist) {
                        uint32_t temp=adr;
                        unsigned int i;

                        if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04x\t":".%06x ",(address-ln-1) & all_mem);
                        if (dooutput) {
                            if (ln>=0) {
                                fprintf(flist," %02x", cod ^ longbranch ^ outputeor);
                                for (i=0;i<(unsigned)ln;i++) {fprintf(flist," %02x",(uint8_t)temp ^ outputeor);temp>>=8;}
                            }
                            if (ln<2) fputc('\t',flist);
                            fputc('\t',flist);
                            if (arguments.monitor) {
                                for (i=0;i<3;i++) fputc(mnemonic[mnem*3+i],flist);

                                switch (opr) {
                                    case ADR_IMPLIED: fputc('\t', flist); break;
                                    case ADR_ACCU: fputs(" a\t", flist); break;
                                    case ADR_IMMEDIATE:
                                                   {
                                                       if (ln==1) fprintf(flist," #$%02x",(uint8_t)adr);
                                                       else fprintf(flist," #$%04x",(uint16_t)adr);
                                                       break;
                                                   }
                                    case ADR_LONG: fprintf(flist," $%06x",(uint32_t)(adr&0xffffff)); break;
                                    case ADR_ADDR:
                                                   if (cnmemonic[ADR_ADDR]==0x20 || cnmemonic[ADR_ADDR]==0x4c)
                                                       fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",((uint16_t)adr) | (l_address & 0xff0000));
                                                   else
                                                       fprintf(flist,databank?" $%06x":" $%04x",(uint16_t)adr | (databank << 16));
                                                   break;
                                    case ADR_ZP: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x\t":" $%04x",(uint16_t)(adr+dpage)); break;
                                    case ADR_LONG_X: fprintf(flist," $%06x,x",(uint32_t)(adr&0xffffff)); break;
                                    case ADR_ADDR_X: fprintf(flist,databank?" $%06x,x":" $%04x,x",(uint16_t)adr | (databank << 16)); break;
                                    case ADR_ZP_X: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x,x":" $%04x,x",(uint16_t)(adr+dpage)); break;
                                    case ADR_ADDR_X_I: fprintf(flist,(l_address&0xff0000)?" ($%06x,x)":" ($%04x,x)",((uint16_t)adr) | (l_address&0xff0000)); break;
                                    case ADR_ZP_X_I: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x,x)":" ($%04x,x)",(uint16_t)(adr+dpage)); break;
                                    case ADR_ZP_S: fprintf(flist," $%02x,s",(uint8_t)adr); break;
                                    case ADR_ZP_S_I_Y: fprintf(flist," ($%02x,s),y",(uint8_t)adr); break;
                                    case ADR_ADDR_Y: fprintf(flist,databank?" $%06x,y":" $%04x,y",(uint16_t)adr | (databank << 16)); break;
                                    case ADR_ZP_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x,y":" $%04x,y",(uint16_t)(adr+dpage)); break;
                                    case ADR_ZP_LI_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" [$%02x],y":" [$%04x],y",(uint16_t)(adr+dpage)); break;
                                    case ADR_ZP_I_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x),y":" ($%04x),y",(uint16_t)(adr+dpage)); break;
                                    case ADR_ADDR_LI: fprintf(flist," [$%04x]",(uint16_t)adr); break;
                                    case ADR_ZP_LI: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" [$%02x]":" [$%04x]",(uint16_t)(adr+dpage)); break;
                                    case ADR_ADDR_I: fprintf(flist," ($%04x)",(uint16_t)adr); break;
                                    case ADR_ZP_I: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x)":" ($%04x)",(uint16_t)(adr+dpage)); break;
                                    case ADR_REL:
                                                   if (ln==1) fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",(uint16_t)(((int8_t)adr)+l_address) | (l_address & 0xff0000));
                                                   else if (ln==2) {
                                                       if ((cod ^ longbranch)==0x4C)
                                                           fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",((uint16_t)adr) | (l_address & 0xff0000));
                                                       else
                                                           fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",(uint16_t)(adr+l_address) | (l_address & 0xff0000));
                                                   }
                                                   else {
                                                       if ((uint16_t)adr==0x4C03)
                                                           fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",((uint16_t)(adr >> 16)) | (l_address & 0xff0000));
                                                       else
                                                           fprintf(flist,(l_address&0xff0000)?" $%06x":" $%04x",(uint16_t)((adr >> 16)+l_address) | (l_address & 0xff0000));
                                                   }
                                                   break;
                                    case ADR_REL_L: fprintf(flist,(l_address & 0xff0000)?" $%06x":" $%04x",(uint16_t)(adr+l_address) | (l_address & 0xff0000)); break;
                                    case ADR_MOVE: fprintf(flist," $%02x,$%02x",(uint8_t)adr,(uint8_t)(adr>>8));
                                }
                            } else if (arguments.source) fputc('\t',flist);
                        } else if (arguments.source) fputs("\t\t\t", flist);
                        if (arguments.source) {
                            fputc('\t', flist);printllist(flist);
                        } else fputc('\n',flist);
                    }
                    break;
                }
                if ((tmp2=find_macro(ident))) goto as_macro;
            }            // fall through
        default: if (skipit[waitforp] & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); //skip things if needed
        }
    finish:
        ignore();if (!here() || here()==';') continue;
        if (skipit[waitforp] & 1) err_msg(ERROR_EXTRA_CHAR_OL,NULL); 
    breakerr:
        continue;
    }

    while (oldwaitforp < waitforp) {
        uint32_t os = sline;
        sline = waitfor[waitforp].line;
        switch (waitfor[waitforp--].what) {
        case 'e':
        case 'f': err_msg(ERROR______EXPECTED,".FI"); break;
        case 'P': err_msg(ERROR______EXPECTED,".ENDP"); break;
        case 'M':
        case 'm': err_msg(ERROR______EXPECTED,".ENDM"); break;
        case 'N':
        case 'n': err_msg(ERROR______EXPECTED,".NEXT"); break;
        case 'p': err_msg(ERROR______EXPECTED,".PEND"); break;
        case 'b': err_msg(ERROR______EXPECTED,".BEND"); break;
        case 'c': err_msg(ERROR______EXPECTED,".ENDC"); break;
        case 'S':
        case 's': err_msg(ERROR______EXPECTED,".ENDS"); break;
        }
        sline = os;
    }
    return;
}

int main(int argc,char *argv[]) {
    time_t t;
    FILE* fout;
    int optind, i;
    struct file_s *fin;

    tinit();

    fin = openfile("");
    optind = testarg(argc,argv, fin);

    if (arguments.quiet)
        puts("6502/65C02 Turbo Assembler Version 1.3  Copyright (c) 1997 Taboo Productions\n"
             "6502/65C02 Turbo Assembler Version 1.35 ANSI C port by [BiGFooT/BReeZe^2000]\n"
             "6502/65C02/65816/DTV TASM Version " VERSION " Fixing by Soci/Singular 2001-2011\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");

    /* assemble the input file(s) */
    do {
        if (pass++>20) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        mem.p=0;memblocklastp=0;memblocks.p=0;memblocklaststart=0;
        for (i = optind - 1; i<argc; i++) {
            set_cpumode(arguments.cpumode);
            address=l_address=star=databank=dpage=longaccu=longindex=0;encoding=0;wrapwarn=0;wrapwarn2=0;
            current_provides=~0;current_requires=0;current_conflicts=0;macrecursion=structrecursion=0;allowslowbranch=1;
            waitfor[waitforp=0].what=0;skipit[0]=1;sline=vline=0;outputeor=0;forwr=backr=0;dooutput=1;
            current_context=&root_label;logitab.p=0;
            /*	listing=1;flist=stderr;*/
            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0; cfile = fin;
                star_tree=&fin->star;
                reffile=cfile->uid;
                compile("",-1);
                exitfile();
                mem.p=0;memblocklastp=0;memblocks.p=0;memblocklaststart=0;
                continue;
            }
            memjmp(address);
            enterfile(argv[i],0);
            cfile = openfile(argv[i]);
            if (cfile) {
                cfile->p = 0;
                star_tree=&cfile->star;
                reffile=cfile->uid;
                compile("",-1);
                closefile(cfile);
            }
            exitfile();
        }
        if (errors) {memcomp();status();return 1;}
        if (conderrors && !arguments.list && pass==1) fixeddig=0;
    } while (!fixeddig || pass==1);

    /* assemble again to create listing */
    if (arguments.list) {
        char **argv2 = argv;
        listing=1;
        if (arguments.list[0] == '-' && !arguments.list[1]) {
            flist = stdout;
        } else {
            if (!(flist=fopen(arguments.list,"wt"))) err_msg(ERROR_CANT_DUMP_LST,arguments.list);
        }
	fputs("\n; 64tass Turbo Assembler Macro V" VERSION " listing file\n;", flist);
        while (*argv2) fprintf(flist," %s", *argv2++);
	time(&t); fprintf(flist,"\n; %s",ctime(&t));

        pass++;
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        mem.p=0;memblocklastp=0;memblocks.p=0;memblocklaststart=0;
        for (i = optind - 1; i<argc; i++) {
            if (i >= optind) {fprintf(flist,"\n;******  Processing input file: %s\n", argv[i]);}
            lastl=LIST_NONE;
            set_cpumode(arguments.cpumode);
            address=l_address=star=databank=dpage=longaccu=longindex=0;encoding=0;wrapwarn=0;wrapwarn2=0;
            current_provides=~0;current_requires=0;current_conflicts=0;macrecursion=structrecursion=0;allowslowbranch=1;
            waitfor[waitforp=0].what=0;skipit[0]=1;sline=vline=0;outputeor=0;forwr=backr=0;dooutput=1;
            current_context=&root_label;logitab.p=0;

            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0; cfile = fin;
                star_tree=&fin->star;
                reffile=cfile->uid;
                compile("",-1);
                exitfile();
                mem.p=0;memblocklastp=0;memblocks.p=0;memblocklaststart=0;
                continue;
            }
            memjmp(address);

            enterfile(argv[i],0);
            cfile = openfile(argv[i]);
            if (cfile) {
                cfile->p = 0;
                star_tree=&cfile->star;
                reffile=cfile->uid;
                compile("",-1);
                closefile(cfile);
            }
            exitfile();
        }
	fputs("\n;******  End of listing\n", flist);
	if (flist != stdout) fclose(flist);
    }
    if (!fixeddig) err_msg(ERROR_TOO_MANY_PASS, NULL);
    memcomp();

    set_cpumode(arguments.cpumode);

    if (arguments.label) labelprint();

    if (errors || conderrors) {status();return 1;}

    /* output file */
    if (mem.p) {
        uint32_t start, last, size;
        unsigned int i;
        if (arguments.output[0] == '-' && !arguments.output[1]) {
            fout = stdout;
        } else {
            if ((fout=fopen(arguments.output,"wb"))==NULL) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
        }
        clearerr(fout);
        if (memblocks.p) {
            start = memblocks.data[0].start;
            size = memblocks.data[0].len;
            last = 0;
            for (i=1;i<memblocks.p;i++) {
                if (memblocks.data[i].start != start + size) {
                    if (arguments.nonlinear) {
                        fputc(size,fout);
                        fputc(size >> 8,fout);
                        if (scpumode) fputc(size >> 16,fout);
                    }
                    if ((!arguments.stripstart && !last) || arguments.nonlinear) {
                        fputc(start,fout);
                        fputc(start >> 8,fout);
                        if (scpumode) fputc(start >> 16,fout);
                    }
                    while (last<i) {
                        fwrite(mem.data+memblocks.data[last].p,memblocks.data[last].len,1,fout);
                        last++;
                    }
                    if (!arguments.nonlinear) {
                        size = memblocks.data[i].start - start - size;
                        while (size--) fputc(0, fout);
                    }
                    start = memblocks.data[i].start;
                    size = 0;
                }
                size += memblocks.data[i].len;
            }
            if (arguments.nonlinear) {
                fputc(size,fout);
                fputc(size >> 8,fout);
                if (scpumode) fputc(size >> 16,fout);
            }
            if ((!arguments.stripstart && !last) || arguments.nonlinear) {
                fputc(start,fout);
                fputc(start >> 8,fout);
                if (scpumode) fputc(start >> 16,fout);
            }
            while (last<i) {
                fwrite(mem.data+memblocks.data[last].p,memblocks.data[last].len,1,fout);
                last++;
            }
        }
        if (arguments.nonlinear) {
            fputc(0,fout);
            fputc(0,fout);
            if (scpumode) fputc(0 ,fout);
        }
        if (ferror(fout)) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
	if (fout != stdout) fclose(fout);
    }
    status();
    return 0;
}
