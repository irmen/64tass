/*
    Turbo Assembler 6502/65C02/65816/DTV
    Copyright (C) <year>  <name of author>

   6502/65C02 Turbo Assembler  Version 1.3
   (c)1996 Taboo Productions, Marek Matula
  
   6502/65C02 Turbo Assembler  Version 1.35  ANSI C port
   (c)2000 [BiGFooT/BReeZe^2000]
  
   6502/65C02/65816/DTV Turbo Assembler  Version 1.4x
   (c)2001-2005 Soci/Singular (soci@c64.rulez.org)

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

#include <sys/stat.h>
#include <time.h>

#include "opcodes.h"
#include "misc.h"

static char *mnemonic;    //mnemonics
const unsigned char *opcode;    //opcodes

static struct sencoding *actual_encoding = ascii_encoding;

#define linelength 4096
#define nestinglevel 256
int errors=0,conderrors=0,warnings=0, wrapwarn=0, wrapwarn2=0;
long sline;      //current line
static unsigned long all_mem;
int pass=0;      //pass
static int listing=0;   //listing
static unsigned char* memdata=NULL;//Linear memory dump
static unsigned long memdatap = 0, memdatasize = 0, memblocklastp = 0, memblocklaststart = 0;
static struct smemblock {unsigned long size;unsigned long memp;unsigned long start;} *memblocks; //starts and sizes
static unsigned long memblockp = 0, memblocksize = 0;
unsigned long address=0,l_address=0; //address, logical address
char pline[linelength];  //current line data
static char llist[linelength];  //current line for listing
static int lpoint;              //position in current line
static char ident[linelength], ident2[linelength];  //identifier (label, etc)
static char varname[linelength];//variable (same as identifier?)
static char path[80];           //path
static int pagelo=-1;           //still in same page?
static FILE* flist = NULL;      //listfile
enum {
    LIST_NONE,
    LIST_CODE,
    LIST_DATA,
    LIST_EQU,
} lastl = LIST_CODE;
static int logisave=0;          // number of nested .logical
static unsigned long* logitab = NULL;  //.logical .here
static int logisize=0;
static char longaccu=0,longindex=0,scpumode=0,dtvmode=0;
static unsigned char databank=0;
static unsigned int dpage=0;
static char fixeddig;
static unsigned long current_requires;
static unsigned long current_conflicts;
static unsigned long current_provides;
static int allowslowbranch=1;
static int longbranchasjmp=0;
static int outputeor = 0; // EOR value for final output (usually 0, except changed by .eor)

static char s_stack[256];
static struct {struct svalue val; char sgn;} e_stack[256];
static struct svalue v_stack[256];
static int ssp,esp,vsp;

static char waitfor[nestinglevel];
static char skipit[nestinglevel];
static int waitforp=0;

static int last_mnem;

int labelexists;
unsigned long reffile;
unsigned int macrecursion;

                       // 0       1       2        3        4         5        6         7      8         9
const char* command[]={"byte"   ,"text", "ptext", "char" ,"shift","shiftl" ,"null"  ,"rta" , "int"  , "word" , "long" ,"offs"  ,"macro"  ,"endm"   ,"for" ,
                        "next"   ,"if"   ,"else"  ,"fi"    ,"elsif","rept"   ,"include","binary","comment","endc",
                        "page"   ,"endp" ,"logical","here" ,"as"   ,"al"     ,"xs"    ,"xl"     ,"error"  ,"proc",
                        "pend"   ,"databank","dpage","fill","warn"  ,"enc"   ,"endif"  , "ifne"  , "ifeq",
                        "ifpl"   , "ifmi","cerror","cwarn", "align","assert", "check", "cpu", "option",
                        "block"  , "bend", "pron", "proff", "showmac", "hidemac", "end", "eor", "segment"
};
enum {
    CMD_BYTE, CMD_TEXT, CMD_PTEXT, CMD_CHAR, CMD_SHIFT, CMD_SHIFT2, CMD_NULL, CMD_RTA, CMD_INT, CMD_WORD, CMD_LONG, CMD_OFFS, CMD_MACRO, CMD_ENDM, CMD_FOR, CMD_NEXT, CMD_IF,
    CMD_ELSE, CMD_FI, CMD_ELSIF, CMD_REPT, CMD_INCLUDE, CMD_BINARY, CMD_COMMENT, CMD_ENDC, CMD_PAGE, CMD_ENDP, CMD_LOGICAL,
    CMD_HERE, CMD_AS, CMD_AL, CMD_XS, CMD_XL, CMD_ERROR, CMD_PROC, CMD_PEND, CMD_DATABANK, CMD_DPAGE,
    CMD_FILL, CMD_WARN, CMD_ENC, CMD_ENDIF, CMD_IFNE, CMD_IFEQ, CMD_IFPL, CMD_IFMI, CMD_CERROR, CMD_CWARN, CMD_ALIGN, CMD_ASSERT, CMD_CHECK, CMD_CPU, CMD_OPTION,
    CMD_BLOCK, CMD_BEND, CMD_PRON, CMD_PROFF, CMD_SHOWMAC, CMD_HIDEMAC, CMD_END, CMD_EOR, CMD_SEGMENT
};
#define COMMANDS 62

// ---------------------------------------------------------------------------

void status() {
    freeerrorlist(1);
    errors+=conderrors;
    if (arguments.quiet) {
        unsigned long i, start, end;
        char temp[10];
        fprintf(stdout,"Error messages:    ");
        if (errors) fprintf(stdout,"%i\n",errors); else fprintf(stdout,"None\n");
        fprintf(stdout,"Warning messages:  ");
        if (warnings) fprintf(stdout,"%i\n",warnings); else fprintf(stdout,"None\n");
        fprintf(stdout,"Passes:            %i\n",pass);
        if (memblockp) {
            start = memblocks[0].start;
            end = memblocks[0].start + memblocks[0].size;
            for (i=1;i<memblockp;i++) {
                if (memblocks[i].start != end) {
                    fprintf(stdout,"Memory range:    ");
                    sprintf(temp, "$%04lx", start);
                    fprintf(stdout,"%7s-$%04lx\n",temp,end-1);
                    start = memblocks[i].start;
                }
                end = memblocks[i].start + memblocks[i].size;
            }
            fprintf(stdout,"Memory range:    ");
            sprintf(temp, "$%04lx", start);
            fprintf(stdout,"%7s-$%04lx\n",temp,end-1);
        } else fprintf(stdout,"Memory range:      None\n\n");
    }
    free(memdata);		        	// free codemem
    free(memblocks);				// free memorymap
    free(logitab);

    tfree();
}

// ---------------------------------------------------------------------------
/*
 * read one input line, filters comments (';') and breaks up statements
 * seperated by colons (':') into seperate lines.
 * 
 * in:
 *      fle - file
 * out:
 *      llist -
 *      pline -
 */
void readln(struct sfile* fle) {
    int i=0,i2=0,q=0,ch;

    for (;;) {
        if (fle->linebuflen==fle->currentp || !(ch=fle->linebuf[fle->currentp++])) {sline++;break;}// eof?
        llist[i2++]=ch;
        if (ch=='\'' && !(q & 5)) q^=2;
        if (ch=='"' && !(q & 6)) q^=1;
        if (!q) {
            if (ch==9) ch=32;
            if (ch==';') q=4;
            else if (ch== ':' && !arguments.oldops) {
                i2--;
                break;
            }
        }
        if (!(q & 4)) {
            pline[i]=ch;
            i++;
        }
        if (i2==sizeof(pline)-1) {sline++;pline[i]=0;err_msg(ERROR_LINE_TOO_LONG,NULL);}
    }
    llist[i2]=0;
    if (i)
        while (pline[i-1]==0x20) i--;
    pline[i]=lpoint=0;
}

// ---------------------------------------------------------------------------
// Read a character in the current string encoding
int petascii(char quo) {
    int ch;

    if (!(ch=here())) {err_msg(ERROR______EXPECTED,"End of string"); return 256;}
    if (ch & 0x80) lpoint+=utf8in((unsigned char *)pline + lpoint, &ch); else lpoint++;
    if (ch==quo) {
        if (here()==quo) lpoint++; // handle 'it''s'
        else return 257; // end of string;
    }
    if (arguments.toascii) {
        int n;
        int also=0,felso,s4,elozo;

        felso=actual_encoding[0].offset + 1;
        n=felso/2;
        for (;;) {  // do binary search
            s4=ch-actual_encoding[n].start;
            if (s4 >= 0 && ch <= actual_encoding[n].end) {
                if (actual_encoding[n].offset < 0) {
                    char sym[0x10];
                    int end = -actual_encoding[n].offset;
                    int n;
                    
                    for (n=0;;) {
                        if (!(ch=here())) {err_msg(ERROR______EXPECTED,"End of symbol");return 256;}
                        if (ch & 0x80) lpoint+=utf8in((unsigned char *)pline + lpoint, &ch); else lpoint++;
                        if (ch == end) break;
                        if (ch == quo) {err_msg(ERROR______EXPECTED,"End of symbol");return 256;}
                        sym[n]=ch;
                        n++;
                        if (n == 0x10) {err_msg(ERROR_CONSTNT_LARGE,NULL);return 256;}
                    }
                    sym[n] = 0;
                    ch = petsymbolic(sym);
                    if (ch < 0) {err_msg(ERROR______EXPECTED, "PETASCII symbol");return 256;}
                    return encode(ch);
                }
                return encode((unsigned char)(s4 + actual_encoding[n].offset));
            }

            elozo = n;
            n = ((s4>0) ? (felso+(also=n)) : (also+(felso=n)))/2;
            if (elozo == n) break;
        }
        err_msg(ERROR___UNKNOWN_CHR, (char *)ch);
        ch = 0;
    }
    return encode(ch);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
void memjmp(unsigned long adr) {
    if (memdatap == memblocklastp) {
        memblocklaststart = adr;
        return;
    }
    if (memblockp>=memblocksize) {
        memblocksize+=64;
        memblocks=realloc(memblocks, memblocksize*sizeof(*memblocks));
        if (!memblocks) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    }
    memblocks[memblockp].size = memdatap-memblocklastp;
    memblocks[memblockp].memp = memblocklastp;
    memblocks[memblockp++].start = memblocklaststart;
    memblocklastp = memdatap;
    memblocklaststart = adr;
}

int memblockcomp(const void *a, const void *b) {
    struct smemblock *aa=(struct smemblock *)a;
    struct smemblock *bb=(struct smemblock *)b;
    return aa->start-bb->start;
}

void memcomp(void) {
    unsigned long i, j, k;
    memjmp(0);
    if (memblockp<2) return;
    
    for (k = j = 0; j < memblockp; j++) {
        if (memblocks[j].size) {
            for (i = j + 1; i < memblockp; i++) if (memblocks[i].size) {
                if (memblocks[j].start <= memblocks[i].start
                        && memblocks[j].start + memblocks[j].size > memblocks[i].start) {
                    unsigned long overlap = memblocks[j].start + memblocks[j].size - memblocks[i].start;
                    if (overlap > memblocks[i].size) overlap = memblocks[i].size;
                    memcpy(memdata + memblocks[j].memp + memblocks[i].start - memblocks[j].start, memdata + memblocks[i].memp, overlap);
                    memblocks[i].size-=overlap;
                    memblocks[i].memp+=overlap;
                    memblocks[i].start+=overlap;
                }
            }
            if (j!=k) memblocks[k]=memblocks[j];
            k++;
        }
    }
    memblockp = k;
    qsort(memblocks, memblockp, sizeof(*memblocks), memblockcomp);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
void pokeb(unsigned char byte) 
{

    if (fixeddig)
    {
        if (memdatap>=memdatasize) {
            memdatasize+=0x1000;
            memdata=realloc(memdata, memdatasize);
            if (!memdata) err_msg(ERROR_OUT_OF_MEMORY,NULL);
        }
	memdata[memdatap++] = byte ^ outputeor;
    }
    if (wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);wrapwarn=0;}
    if (wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);wrapwarn2=0;}
    address++;l_address++;l_address&=all_mem;
    if (address>all_mem) {
	if (fixeddig) wrapwarn=1;
	address=0;
        memjmp(address);
    }
    if (fixeddig && scpumode) if (!(address & 0xffff) || !(l_address & 0xffff)) wrapwarn2=1;
}

int lookup_opcode(char *pline) {
    char s2,s3,*p, ch;
    int also=0,felso,s4,elozo, no;

    ch=lowcase(pline[0]);
    no=(felso=last_mnem)/2;
    if (ch && (s2=lowcase(pline[1])) && (s3=lowcase(pline[2])) && !pline[3])
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
int what(int *tempno) {
    char ch;
    int no, l;

    ignore();
    switch (ch=whatis[(int)here()]) {
    case WHAT_COMMAND:
	{
            lpoint++;
	    for (no=0; no<COMMANDS; no++)
	    {
		for (l = 0; command[no][l]; l++) {
		    if (command[no][l] != (pline[lpoint+l] | 0x20)) {
                        break;
                    }
                }
		if (!command[no][l])
		{
		    if (((ch=lowcase(pline[lpoint+l]))<'a' ||
			 ch>'z') && ch!='_') {
                        lpoint+=l;
                        if (no==CMD_ENDIF) no=CMD_FI; else
                        if (no==CMD_IFNE) no=CMD_IF;
                        *tempno=no;
			return WHAT_COMMAND;
		    }
		}
	    }
	    *tempno=no;
	    return 0;
	}
    case WHAT_COMA:
	lpoint++;
        ignore();
	switch (get() | 0x20) {
	case 'y': return WHAT_Y;
	case 'x': ignore();if (get()==')') return WHAT_XZ; else {lpoint--; return WHAT_X;}
	case 's': ignore();if (get()==')') return WHAT_SZ; else {lpoint--; return WHAT_S;}
	default: lpoint--;return WHAT_COMA;
	}
    case WHAT_CHAR:
    case WHAT_LBL:
            *tempno=1;return WHAT_EXPRESSION;
    case WHAT_EXPRESSION://tempno=1 if label, 0 if expression
	    *tempno=0;return WHAT_EXPRESSION;
    default:lpoint++;return ch;
    }
}

int get_ident2(unsigned char allowed) {
    int i=0;
    unsigned char ch;
    if (arguments.casesensitive)
	while ((whatis[ch=here()]==WHAT_CHAR) || (ch>='0' && ch<='9') || ch==allowed || ch=='_') {ident[i++]=ch; lpoint++; }
    else
	while (((ch=lowcase(pline[lpoint]))>='a' && ch<='z') || (ch>='0' && ch<='9') || ch==allowed || ch=='_') { ident[i++]=ch; lpoint++; }
    ident[i]=0;
    return 0;
}

int get_ident(unsigned char allowed) {
    int v,code;
 
    if ((v=what(&code))!=WHAT_EXPRESSION || !code) {
	err_msg(ERROR_EXPRES_SYNTAX,NULL);
	return 1;
    }
    return get_ident2(allowed);
}

int get_num(int mode, struct svalue *v) {// 0=unknown stuff, 1=ok
    long val=0;            //md=0, define it, md=1 error if not exist
    struct slabel* tmp;

    unsigned char ch;
    v->type=T_NONE;

    if (mode) {
        if (mode & 1) {
            sprintf(ident,"+%lu+%lu",reffile,current_context->forwr+(mode >> 1));
            goto in;
        } else {
            sprintf(ident,"-%lu-%lu",reffile,current_context->backr-(mode >> 1));
            goto in;
        }
    }
    ignore();
    switch (ch=get()) {
    case '$': // hex
	{
	    ignore();
	    while (((ch=lowcase(get()))>='0' && ch<='9') ||
		   (ch>='a' && ch<='f')) {
		if (val>0x7ffffffl) {err_msg(ERROR_CONSTNT_LARGE,NULL); return 0;}
		val=(val<<4)+(ch=ch<='9' ? ch-'0' : ch-'a'+10);
	    }
	    lpoint--;
            v->type=T_INT;v->num=val;
	    return 1;
	}
    case '%': // binary
	{
	    ignore();
	    while (((ch=get()) & 0xfe)=='0') {
		if (val>0x3fffffffl) {err_msg(ERROR_CONSTNT_LARGE,NULL); return 0;}
		val=(val<<1)+ch-'0';
	    }
	    lpoint--;
            v->type=T_INT;v->num=val;
	    return 1;
	}
    case '"': // string
    case '\'':
	{
            unsigned char line[linelength];  //current line data
            unsigned int i;

            val = petascii(ch);
            if (val < 256 && here()==ch) {lpoint++;v->type=T_CHR;v->num=val;return 1;}
	    if (val == 256) return 0;
            i=0;
            for (;val < 256 && i < sizeof(line)-1;val = petascii(ch)) {
                line[i++]=(char)val;
            }
            if (val == 257) {
                v->type=T_TSTR;
                v->str.len=i;
                v->str.data=malloc(i);
                memcpy(v->str.data, line, i);
                return 1;
            }
	}
    case '*': // program counter
        v->type=T_INT;v->num=l_address;return 1;
    default:
	{
	    lpoint--;
	    if ((ch>='0') && (ch<='9')) { //decimal number...
		while (((ch=get())>='0') && (ch<='9')) {
		    if (val>(0x7fffffffl-ch+'0')/10) {err_msg(ERROR_CONSTNT_LARGE,NULL);v->type=T_NONE;return 0;}
		    val=(val*10)+ch-'0';
		}
		lpoint--;
                v->type=T_INT;v->num=val;
		return 1;
	    }
            if (get_ident('.')) return 0; //label?
        in:
            tmp=find_label(ident);
	    if (pass==1) {
                if (tmp) {tmp->proclabel=0;tmp->used=pass;*v=tmp->value;}
		return 1;
	    }
	    else {
                if (tmp) {
                    if ((tmp->requires & current_provides)!=tmp->requires) err_msg(ERROR_REQUIREMENTS_,ident);
                    if (tmp->conflicts & current_provides) err_msg(ERROR______CONFLICT,ident);
                    tmp->proclabel=0;tmp->used=pass;*v=tmp->value;return 1;
                }
                if (mode) err_msg(ERROR___NOT_DEFINED,(mode & 1)?"+":"-");
                else
                    err_msg(ERROR___NOT_DEFINED,ident); //never reached
	    }
	}
    }
    return 0;
}

/*
 * get priority for operator in an expression
 */
static int priority(char ch) 
{
    if (arguments.noprecedence) {
        switch (ch) {
        case '(':return 0;
        case 'l':          // <
        case 'h':          // >
        case 'H':          // `
        case 'S':return 5; // ^
        case '=':
        case '<':
        case '>':
        case 'o':          // !=
        case 'g':          // >=
        case 's':          // <=
        case '+':
        case '-':
        case '*':
        case '/':
        case 'u':          // mod
        case '|':
        case '^':
        case '&':
        case 'm':          // <<
        case 'd':          // >>
        case 'n':          // -
        case 'i':          // ~
        case 't':return 40;// !
        default:return 0;
        }
    } else {
        switch (ch) {
        case '(':return 0;
        case 'l':          // <
        case 'h':          // >
        case 'H':          // `
        case 'S':return 5; // ^
        case '=':
        case '<':
        case '>':
        case 'o':          // !=
        case 'g':          // >=
        case 's':return 10;// <=
        case '+':
        case '-':return 15;
        case '*':
        case '/':
        case 'u':return 20;// mod
        case '|':
        case '^':return 25;
        case '&':return 30;
        case 'm':          // <<
        case 'd':return 35;// >>
        case 'n':          // -
        case 'i':          // ~
        case 't':return 40;// !
        default:return 0;
        }
    }
}

void pushs(char ch) {
    if ((ch=='n' || ch=='t' || ch=='i' || ch=='l' || ch=='h' || ch=='H' || ch=='S') && ssp &&
	priority(s_stack[ssp-1])==priority(ch)) { s_stack[ssp++]=ch; return; }
    if (!ssp || priority(s_stack[ssp-1])<priority(ch)) {
	s_stack[ssp++]=ch;
	return;
    }
    while (ssp && priority(s_stack[ssp-1])>=priority(ch)) e_stack[esp++].sgn=s_stack[--ssp];
    s_stack[ssp++]=ch;
}

char val_length(long val)
{
        if (val<0) return 3;
    	if (val<0x100) return 0;
        if (val<0x10000) return 1;
	if (val<0x1000000) return 2;
        return 3;
}

void get_exp(int *wd, int *df,int *cd, struct svalue *v, enum etype type) {// length in bytes, defined
    struct svalue val;
    int i,nd=0,tp=0;
    char ch;
    static unsigned char line[linelength];  //current line data

    ssp=esp=0;
    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    *df=1;    // 1=result is ok, result is not yet known
    *cd=0;    // 0=error
    v->type = T_NONE;

    ignore();
    switch (here()) {
    case '@':
	switch (lowcase(pline[++lpoint])) {
	case 'b':*wd=0;break;
	case 'w':*wd=1;break;
	case 'l':*wd=2;break;
	default:err_msg(ERROR______EXPECTED,"@B or @W or @L"); return;
	}
        lpoint++;
	ignore();
        break;
    case '!':
        if (arguments.oldops) {
            *wd=1;
            lpoint++;
            ignore();
        }
        break;
    case '(': tp=1; break;
    }
    for (;;) {
        if (!nd) {
            int db=0;
            ignore();
            ch=get();
            switch (ch) {
            case '(': s_stack[ssp++]='('; continue;
            case '+':
            ba: ch=here();
                db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    if (ch=='+') {lpoint++;goto ba;}
                    if (!get_num(db*2-1, &val)) {
                        for (i=0; i<esp; i++) if (e_stack[i].sgn==' ' && e_stack[i].val.type == T_TSTR) free(e_stack[i].val.str.data);
                        return;
                    }
                    goto ide;
                }
                continue;
            case '-':
            ba2:ch=here();
                db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    if (ch=='-') {lpoint++;goto ba2;}
                    if (!get_num(db*2, &val)) {
                        for (i=0; i<esp; i++) if (e_stack[i].sgn==' ' && e_stack[i].val.type == T_TSTR) free(e_stack[i].val.str.data);
                        return;
                    }
                    goto ide;
                }
                pushs('n');
                continue;
            case '!': pushs('t'); continue;
            case '~': pushs('i'); continue;
            case '<': pushs('l'); continue;
            case '>': pushs('h'); continue;
            case '`': pushs('H'); continue;
            case '^': pushs('S'); continue;
            }
	    lpoint--;
            if (!get_num(0, &val)) {
                for (i=0; i<esp; i++) if (e_stack[i].sgn==' ' && e_stack[i].val.type == T_TSTR) free(e_stack[i].val.str.data);
                return;
            }
        ide:
	    e_stack[esp].val=val;
	    e_stack[esp++].sgn=' ';
	    nd=1;
	}
	else {
	    ignore();
	    if ((ch=pline[lpoint])=='&' || ch=='|' || ch=='^' ||
		ch=='*' || ch=='/' || ch=='+' || ch=='-' || 
		ch=='=' || ch=='<' || ch=='>' || 
                (ch=='.' && arguments.oldops) ||
                (ch==':' && arguments.oldops) ||
		(ch=='!' && pline[lpoint+1]=='=')) {
		if (tp) tp=1;
		if ((ch=='<') && (pline[lpoint+1]=='<')) {pushs('m'); lpoint++;}
		else if ((ch=='>') && (pline[lpoint+1]=='>')) {pushs('d'); lpoint++;}
		else if ((ch=='>') && (pline[lpoint+1]=='=')) {pushs('g'); lpoint++;}
		else if ((ch=='<') && (pline[lpoint+1]=='=')) {pushs('s'); lpoint++;}
		else if ((ch=='/') && (pline[lpoint+1]=='/')) {pushs('u'); lpoint++;}
                else if (ch=='!') {pushs('o'); lpoint++;}
                else if (ch=='.') {pushs('|');} // bitor (tass)
                else if (ch==':') {pushs('^');} // bitxor (tass)
		else pushs(ch);
		nd=0;
		lpoint++;
		continue;
	    }
	    if (ch==')') {
		while ((ssp) && (s_stack[ssp-1]!='('))
		    e_stack[esp++].sgn=s_stack[--ssp];
		lpoint++;
		if (ssp==1 && tp) tp=2;
		if (!ssp) goto syntaxe;
		ssp--;
		continue;
	    }
	    while ((ssp) && (s_stack[ssp-1]!='('))
		e_stack[esp++].sgn=s_stack[--ssp];
	    if (!ssp) {
		if (tp==2) *cd=3; else *cd=1;
		break;
	    }
	    if (ssp>1) goto syntaxe;
	    if (tp) *cd=2;
	    else {
            syntaxe:
                err_msg(ERROR_EXPRES_SYNTAX,NULL);
                for (i=0; i<esp; i++) if (e_stack[i].sgn==' ' && e_stack[i].val.type == T_TSTR) free(e_stack[i].val.str.data);
                return;
            }
	    break;
	}
    }
    vsp=0;
    for (i=0; i<esp; i++) {
	if ((ch=e_stack[i].sgn)==' ')
	    v_stack[vsp++]=e_stack[i].val;
        else if (v_stack[vsp-1].type == T_INT) {
            long val1;
            long val2;
        reint:
            val1 = v_stack[vsp-1].num;
            switch (ch) {
            case 'l': val1 &= 255; break;
            case 'h': val1 = (val1 >> 8) & 255; break;
            case 'H': val1 = (val1 >> 16) & 255; break;
            case 'S': 
                if (v_stack[vsp-1].type == T_CHR) {
                    line[0]=v_stack[vsp-1].num;
                    line[1]=0;
                } else sprintf((char *)line,"%ld",val1);
                v_stack[vsp-1].type = T_TSTR;
                v_stack[vsp-1].str.len=strlen((char *)line);
                v_stack[vsp-1].str.data=malloc(v_stack[vsp-1].str.len);
                memcpy(v_stack[vsp-1].str.data, line, v_stack[vsp-1].str.len);
                continue;
            case 'n': val1 = -val1; break;
            case 'i': val1 = ~val1; break;
            case 't': val1 = !val1; break;
            default:
                if (v_stack[vsp-2].type != T_INT && v_stack[vsp-2].type != T_CHR) {
                    if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].str.data);
                    if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
                    vsp--;v_stack[vsp-1].type = T_NONE;
                    continue;
                }
                v_stack[vsp-2].type = T_INT;
                val2 = v_stack[vsp-2].num;
                switch (ch) {
                case '=': val1 = (val2 == val1); break;
                case 'o': val1 = (val2 != val1); break;
                case '<': val1 = (val2 < val1); break;
                case '>': val1 = (val2 > val1); break;
                case 'g': val1 = (val2 >= val1); break;
                case 's': val1 = (val2 <= val1); break;
                case '*': val1 *= val2; break;
                case '/': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); vsp--;v_stack[vsp-1].type = T_NONE;continue;} else val1=val2 / val1; break;
                case 'u': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); vsp--;v_stack[vsp-1].type = T_NONE;continue;} else val1=val2 % val1; break;
                case '+': val1 += val2; break;
                case '-': val1 = val2 - val1; break;
                case '&': val1 &= val2; break;
                case '|': val1 |= val2; break;
                case '^': val1 ^= val2; break;
                case 'm': val1 = val2 << val1; break;
                case 'd': val1 = val2 >> val1; break;
                }
                vsp--; 
            }
            v_stack[vsp-1].type = T_INT;
            v_stack[vsp-1].num = val1;
	} else if (v_stack[vsp-1].type == T_CHR) {
            switch (ch) {
            case 'l':
            case 'h':
            case 'H':
            case 'S': 
            case 'n':
            case 'i':
            case 't': goto reint;
            }
            if (v_stack[vsp-2].type == T_INT || v_stack[vsp-2].type == T_CHR || v_stack[vsp-2].type == T_NONE) goto reint;
            if (v_stack[vsp-2].type == T_STR || v_stack[vsp-2].type == T_TSTR) {
                line[0]=v_stack[vsp-1].num;
                v_stack[vsp-1].type = T_STR;
                v_stack[vsp-1].str.len = 1;
                v_stack[vsp-1].str.data = line;
                goto restr;
            }
            err_msg(ERROR____WRONG_TYPE,NULL);
            continue;
	} else if (v_stack[vsp-1].type == T_STR || v_stack[vsp-1].type == T_TSTR) {
            long val1;
            switch (ch) {
            case 'l':
            case 'h':
            case 'H':
            case 'S': 
            case 'n':
            case 'i':
                v_stack[vsp-1].type = T_NONE;
                err_msg(ERROR____WRONG_TYPE,NULL);
                continue;
            case 't': 
                v_stack[vsp-1].type = T_INT;
                v_stack[vsp-1].num = !v_stack[vsp-1].str.len;
                continue;
            }
            if (v_stack[vsp-2].type == T_CHR) {
                line[0]=v_stack[vsp-2].num;
                v_stack[vsp-2].type = T_STR;
                v_stack[vsp-2].str.len = 1;
                v_stack[vsp-2].str.data = line;
            }
        restr:
            if (v_stack[vsp-2].type != T_STR && v_stack[vsp-2].type != T_TSTR) {
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].str.data);
                if (v_stack[vsp-2].type != T_NONE) err_msg(ERROR____WRONG_TYPE,NULL);
                vsp--;v_stack[vsp-1].type = T_NONE;
                continue;
            }
            switch (ch) {
            case '=': val1=(v_stack[vsp-2].str.len == v_stack[vsp-1].str.len) && !memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, v_stack[vsp-1].str.len); break;
            case 'o': val1=(v_stack[vsp-2].str.len != v_stack[vsp-1].str.len) || memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, v_stack[vsp-1].str.len); break;
            case '<': 
                val1=memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, (v_stack[vsp-1].str.len < v_stack[vsp-2].str.len)?v_stack[vsp-1].str.len:v_stack[vsp-2].str.len);
                if (val1==0) val1 = (v_stack[vsp-2].str.len < v_stack[vsp-1].str.len);
                else val1 = val1 < 0;
                break;
            case '>': 
                val1=memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, (v_stack[vsp-1].str.len < v_stack[vsp-2].str.len)?v_stack[vsp-1].str.len:v_stack[vsp-2].str.len);
                if (val1==0) val1 = (v_stack[vsp-2].str.len > v_stack[vsp-1].str.len);
                else val1 = val1 > 0;
                break;
            case 's': 
                val1=memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, (v_stack[vsp-1].str.len < v_stack[vsp-2].str.len)?v_stack[vsp-1].str.len:v_stack[vsp-2].str.len);
                if (val1==0) val1 = (v_stack[vsp-2].str.len <= v_stack[vsp-1].str.len);
                else val1 = val1 <= 0;
                break;
            case 'g': 
                val1=memcmp(v_stack[vsp-2].str.data, v_stack[vsp-1].str.data, (v_stack[vsp-1].str.len < v_stack[vsp-2].str.len)?v_stack[vsp-1].str.len:v_stack[vsp-2].str.len);
                if (val1==0) val1 = (v_stack[vsp-2].str.len >= v_stack[vsp-1].str.len);
                else val1 = val1 >= 0;
                break;
            default:
                err_msg(ERROR____WRONG_TYPE,NULL);
                if (v_stack[vsp-1].type == T_TSTR) free(v_stack[vsp-1].str.data);
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].str.data);
                vsp--;v_stack[vsp-1].type = T_NONE;
                continue;
            }
            if (v_stack[vsp-1].type == T_TSTR) free(v_stack[vsp-1].str.data);
            if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].str.data);
            vsp--;
            v_stack[vsp-1].type = T_INT;
            v_stack[vsp-1].num = val1;
	} else if (v_stack[vsp-1].type == T_NONE) {
            switch (ch) {
            case '=':
            case 'o':
            case '<':
            case '>':
            case 'g':
            case 's':
            case '*':
            case '/':
            case 'u':
            case '+':
            case '-':
            case '&':
            case '|':
            case '^':
            case 'm':
            case 'd': 
                if (v_stack[vsp-2].type == T_TSTR) free(v_stack[vsp-2].str.data);
                vsp--; break;
            }
            v_stack[vsp-1].type = T_NONE;
        } else err_msg(ERROR____WRONG_TYPE,NULL);
    }
    if (v_stack[0].type == T_TSTR) {
        if (v_stack[0].str.len<=linelength) memcpy(line, v_stack[0].str.data, v_stack[0].str.len);
        free(v_stack[0].str.data);
        v_stack[0].str.data = line;
        v_stack[0].type = T_STR;
    }
    if (v_stack[0].type == T_INT || v_stack[0].type == T_CHR || v_stack[0].type == T_STR) {
        if (type == T_NONE) {
            *v=v_stack[0];
            return;
        }
        if (type == T_INT)
            switch (v_stack[0].type) {
            case T_STR:
                if (v_stack[0].str.len < 5) {
                    v->type = T_INT;
                    v->num = 0;
                    for (i=v_stack[0].str.len;i;i--) v->num = (v->num << 8) | v_stack[0].str.data[i-1];
                    return;
                } else {
                    *cd=0;
                    err_msg(ERROR_CONSTNT_LARGE,NULL);
                    return;
                }
                break;
            case T_CHR:
                v->type = T_INT;
            case T_INT:
                *v=v_stack[0];
                return;
            default:
                break;
        }
        *cd=0;
        err_msg(ERROR____WRONG_TYPE,NULL);
        return;
    }
    *df = 0;
    return;
}

void wait_cmd(struct sfile *fil, int no)
{
    int wrap=waitforp;
    int pr,wh;
    long lin = 1,pos = 0;

    for (;;) {
	if (fil->linebuflen==fil->currentp) { // eof?
	    char nc[20] = {'.',0};
	    strcat(nc,command[no]);
	    err_msg(ERROR______EXPECTED,nc);
	    return;
	}
	if (no==CMD_PEND) { //.pend
	    lin=sline;
	    pos=fil->currentp;
	}
	readln(fil);
	if ((wh=what(&pr))==WHAT_EXPRESSION) {
            if (!pr) {
                if (here()=='-') {
                    lpoint++;if (here()!=0x20 && here()) goto baj;
                    current_context->backr++;
                    goto hh;
                } else if (here()=='+') {
                    lpoint++;if (here()!=0x20 && here()) goto baj;
                    current_context->forwr++;
                    goto hh;
                }
            baj: lpoint--;
            }
            get_ident('_');   //skip label
            hh:
	    wh=what(&pr);
	}
	if (wh==WHAT_COMMAND) {
	    if (pr==no && wrap==waitforp) return;
	    switch (pr) {
	    case CMD_FOR:waitfor[++waitforp]='n';break;//.for
	    case CMD_NEXT:if (waitfor[waitforp]=='n') waitforp--;break;//.next
	    case CMD_IFEQ:
	    case CMD_IFPL:
	    case CMD_IFMI:
            case CMD_IF:waitfor[++waitforp]='e';break;//.if
            case CMD_ELSE:if (waitfor[waitforp]=='e') waitfor[waitforp]='f';break;//.else
            case CMD_FI:if (waitfor[waitforp]=='e' || waitfor[waitforp]=='f') waitforp--;break;//.fi
            case CMD_ELSIF:break;//.elsif
	    case CMD_REPT:waitfor[++waitforp]='n';break;//.rept
	    case CMD_PROC:if (no==CMD_PEND && wrap==waitforp) {sline=lin;fil->currentp=pos;return;}break;// .proc
	    case CMD_BLOCK:waitfor[++waitforp]='b';break;//.block
	    case CMD_BEND:if (waitfor[waitforp]=='b') waitforp--;break;//.bend
	    case CMD_SEGMENT: //.segment
	    case CMD_MACRO:waitfor[++waitforp]='m';break;//.macro
	    case CMD_ENDM:if (waitfor[waitforp]=='m') waitforp--;break;//.endm
	    }
	}
    }
}

int get_path() {
    int i=0,q=1;
    ignore();
    if (here()=='\"') {lpoint++;q=0;}
    while (here() && (here()!='\"' || q) && i<80) path[i++]=get();
    if (i==80 || (!q && here()!='\"')) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    if (!q) lpoint++;
    path[i]=0;
    ignore();
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
void mtranslate(char* mpr, int nprm, char *cucc)
{
    int q,p,pp,i,j;
    char ch;
    char tmp[linelength];

    strcpy(tmp,cucc);
    q=p=0;
    for (i = 0; (ch = tmp[i]); i++) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        else if ((ch == ';') && (!q)) { q=4; }
        else if ((ch=='\\') && (!q)) {
            /* normal parameter reference */
            if (((ch=lowcase(tmp[i+1]))>='1' && ch<='9') || (ch>='a' && ch<='z')) {
                /* \1..\9, \a..\z */
                if ((ch=(ch<='9' ? ch-'1' : ch-'a'+9))>=nprm) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                for (pp=j=0; j<ch; j++) while (mpr[pp++]); //skip parameters
                while (mpr[pp]==0x20) pp++; //skip space
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
                if ((ch=ch-'1')>=nprm) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                for (pp=j=0; j<ch; j++) while (mpr[pp++]); //skip parameters
                while (mpr[pp]==0x20) pp++; //skip space
                while (mpr[pp] && p<linelength) cucc[p++]=mpr[pp++];//copy
                if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                i++;continue;
            } else ch='@';
        }
        cucc[p++]=ch;
        if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
    }
    cucc[p]=0;
}

//------------------------------------------------------------------------------

void set_cpumode(int cpumode) {
    all_mem=0xffff;scpumode=0;dtvmode=0;
    switch (last_mnem=cpumode) {
    case OPCODES_6502:mnemonic=MNEMONIC6502;opcode=c6502;break;
    case OPCODES_65C02:mnemonic=MNEMONIC65C02;opcode=c65c02;break;
    case OPCODES_6502i:mnemonic=MNEMONIC6502i;opcode=c6502i;break;
    case OPCODES_65816:mnemonic=MNEMONIC65816;opcode=c65816;all_mem=0xffffff;scpumode=1;break;
    case OPCODES_65DTV02:mnemonic=MNEMONIC65DTV02;opcode=c65dtv02;dtvmode=1;break;
    }
}

void compile(char* nam,long fpos,char tpe,char* mprm,int nprm,struct sfile* fin) // "",0
{
    char mparams[256];
    FILE* fil;
  
    int wht,w,d,c,i;
    int prm = 0;
    struct svalue val;

    char ch;

    long pos,lin = 1,cnt,oldpos=-1;

    struct slabel* tmp = NULL;
    struct smacro* tmp2 = NULL;
    struct scontext *old_context = NULL;

    unsigned long backr_old = 0, forwr_old = 0, reffile_old = 0;

    if (tpe==0) {
        backr_old=current_context->backr;
        forwr_old=current_context->forwr;
        current_context->backr=current_context->forwr=1;
    }
    if (tpe==0 || tpe==1) reffile_old=reffile;
    if (fin) oldpos=fin->currentp;
    else {
        char volt;
	if ((fin=openfile(nam,&volt))==NULL) err_msg(ERROR_CANT_FINDFILE,nam);
	if (volt) {
	    if (tpe==1 || tpe==3) oldpos=fin->currentp;
	    else err_msg(ERROR_FILERECURSION,NULL);
        }
     }
    fin->currentp=fpos;
    for (;;) {
	if (fin->linebuflen==fin->currentp) // eof?
	{
            switch (tpe) {
            case 3:
	    case 1:err_msg(ERROR______EXPECTED,".ENDM"); break;
	    case 2:err_msg(ERROR______EXPECTED,".NEXT");
	    }
	    break;
	}

	readln(fin);
	if (nprm>=0) mtranslate(mprm,nprm,pline); //expand macro parameters, if any

        ident2[0]=0;
	if ((wht=what(&prm))==WHAT_EXPRESSION) {
            if (!prm) {
                if (here()=='-') {
                    lpoint++;if (here()!=0x20 && here()) goto baj;
                    prm=1;
                    sprintf(ident,"-%lu-%lu",reffile,current_context->backr++);
                    ident2[0]='-';ident2[1]=0;
                    goto hh;
                } else if (here()=='+') {
                    lpoint++;if (here()!=0x20 && here()) goto baj;
                    prm=1;
                    sprintf(ident,"+%lu+%lu",reffile,current_context->forwr++);
                    ident2[0]='+';ident2[1]=0;
                    goto hh;
                }
            baj:
                err_msg(ERROR_GENERL_SYNTAX,NULL);
                continue;
            } //not label
            get_ident('_');                                           //get label
            if ((prm=lookup_opcode(ident))>=0) goto as_opcode;
            if (listing) strcpy(ident2,ident);
        hh: 
            if (!(skipit[waitforp] & 1)) {wht=what(&prm);goto jn;} //skip things if needed
            if ((wht=what(&prm))==WHAT_EQUAL) { //variable
                strcpy(varname,ident);
                get_exp(&w,&d,&c,&val, T_NONE); //ellenorizve.
		if (!c) continue;
		if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); continue;}
		ignore();if (here()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL); continue;}
		tmp=new_label(varname);
                if (listing && flist && arguments.source && tmp->used>=pass-1) {
                    if (nprm>=0) mtranslate(mprm,nprm,llist);
                    if (lastl!=LIST_EQU) {fputc('\n',flist);lastl=LIST_EQU;}
                    if (val.type == T_INT || val.type == T_CHR) {
                        fprintf(flist,(all_mem==0xffff)?"=%04lx\t\t\t\t\t%s\n":"=%06lx\t\t\t\t\t%s\n",val.num,llist);
                    } else {
                        fprintf(flist,"=\t\t\t\t\t%s\n",llist);
                    }
                }
		if (pass==1) {
		    if (labelexists) {
			err_msg(ERROR_DOUBLE_DEFINE,varname);
			continue;
		    }
                    else {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
			tmp->proclabel=0;tmp->used=pass;
			tmp->value=val;
                        if (val.type == T_STR) {
                            tmp->value.str.data=malloc(val.str.len);
                            memcpy(tmp->value.str.data,val.str.data,val.str.len);
                        }
		    }
		}
		else {
                    if (labelexists) {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        switch (tmp->value.type) {
                        case T_CHR:
                        case T_INT:
                            if ((val.type != T_INT && val.type != T_CHR) || tmp->value.num!=val.num) {
                                tmp->value=val;
                                if (val.type == T_STR) {
                                    tmp->value.str.data=malloc(val.str.len);
                                    memcpy(tmp->value.str.data,val.str.data,val.str.len);
                                }
                                fixeddig=0;
                            } else tmp->value.type = val.type;
                            break;
                        case T_STR:
                            if (val.type != T_STR || tmp->value.str.len!=val.str.len || memcmp(tmp->value.str.data, val.str.data, val.str.len)) {
                                if (val.type == T_STR) {
                                    tmp->value.type = val.type;
                                    tmp->value.str.len=val.str.len;
                                    tmp->value.str.data=realloc(tmp->value.str.data, val.str.len);
                                    memcpy(tmp->value.str.data,val.str.data,val.str.len);
                                } else {
                                    free(tmp->value.str.data);
                                    tmp->value=val;
                                }
                                fixeddig=0;
                            }
                            break;
                        case T_NONE:
                            if (val.type != T_NONE) fixeddig=0;
                            tmp->value=val;
                            if (val.type == T_STR) {
                                tmp->value.str.data=malloc(val.str.len);
                                memcpy(tmp->value.str.data,val.str.data,val.str.len);
                            }
                            break;
                        case T_TSTR: /* not possible here */
                            exit(1);
                            break;
                        }
                    }
		}
                continue;
            }
            if (wht==WHAT_COMMAND && (prm==CMD_MACRO || prm==CMD_SEGMENT)) { // .macro
            do_macro:
		ignore();if (here()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL); continue;}
		tmp2=new_macro(ident);
		if (labelexists) {
		    if (pass==1) {err_msg(ERROR_DOUBLE_DEFINE,ident); continue;}
		}
		else {
		    tmp2->point=fin->currentp;
		    tmp2->lin=sline;
                    tmp2->type=prm;
                    tmp2->file=fin;
		}
		wait_cmd(fin,CMD_ENDM); //.endm
		continue;
	    }
            if ((tmp2=find_macro(ident))) {lpoint--;ident2[0]=0;goto as_macro;}
	    if (wht==WHAT_COMMAND && prm==CMD_PROC) { //.proc
                old_context = current_context;
                if (current_context->parent) {
                    current_context = current_context->parent;
                }
            }
            tmp=new_label(ident);
	    if (pass==1) {
		if (labelexists) {
		    err_msg(ERROR_DOUBLE_DEFINE,ident);
		    continue;
		}
                else {
                    tmp->requires=current_requires;
                    tmp->conflicts=current_conflicts;
                    tmp->used=pass;
		    tmp->value.type=T_INT;tmp->value.num=l_address;
		    if (wht==WHAT_COMMAND && prm==CMD_PROC) tmp->proclabel=1; else tmp->proclabel=0;
		}
	    }
	    else {
                if (labelexists) {
                    tmp->requires=current_requires;
                    tmp->conflicts=current_conflicts;
                    if ((tmp->value.type != T_INT && tmp->value.type != T_CHR) || (unsigned long)tmp->value.num != l_address) {
                        if (tmp->value.type == T_STR) free(tmp->value.str.data);
                        tmp->value.num=l_address;
                        fixeddig=0;
                    }
                    tmp->value.type=T_INT;
		}
	    }
	}
	jn:
	switch (wht) {
	case WHAT_STAR:if (skipit[waitforp] & 1) //skip things if needed
	    {
                unsigned long ch2;
		ignore();if (get()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
		wrapwarn=0;wrapwarn2=0;
		get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		if (!c) break;
		if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                ignore();if (here()) goto extrachar;
                if (listing && flist && arguments.source) {
                    lastl=LIST_NONE;
                    if (ident2[0] && tmp->used>=pass-1)
                        fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,llist);
                    else
                        fprintf(flist,"\n\t\t\t\t\t%s\n",llist);
                }
                if (val.type != T_NONE) {
                    ch2=val.num;
                    if (ch2>all_mem) {
                        err_msg(ERROR_CONSTNT_LARGE,NULL); 
                        break;
                    }
                    if (address!=ch2 || l_address!=ch2) {
                        address=l_address=ch2;
                        memjmp(address);
                    }
                }
	    }
            break;
	case WHAT_EOL:
            if (listing && flist && arguments.source && (skipit[waitforp] & 1) && ident2[0] && tmp->used>=pass-1) {
                if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,llist);
            }
            break;
	case WHAT_COMMAND:
	    {
		char lcol = 0,kiirva = 0; //for listing
                ignore();
                if (listing && flist && arguments.source && (skipit[waitforp] & 1) && prm>=CMD_LONG) {
                    switch (prm) {
                        case CMD_FILL:
                        case CMD_ALIGN:
                        case CMD_OFFS:
                            if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,llist);
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
                            if (ident2[0] && tmp->used>=pass-1)
                                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,llist);
                            else
                                fprintf(flist,"\t\t\t\t\t%s\n",llist);
                            break;
                        default:
                            if (ident2[0] && tmp->used>=pass-1) {
                                if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident2);
                            }
                    }
                }
		if (prm==CMD_FI) // .fi
		{
                    if (waitfor[waitforp]!='e' && waitfor[waitforp]!='f') {err_msg(ERROR______EXPECTED,".IF"); break;}
		    if (here()) goto extrachar;
		    waitforp--;
                    break;
		}
		if (prm==CMD_ELSE) { // .else
		    if (waitfor[waitforp]=='f') {err_msg(ERROR______EXPECTED,".FI"); break;}
		    if (waitfor[waitforp]!='e') {err_msg(ERROR______EXPECTED,".IF"); break;}
		    if (here()) goto extrachar;
		    skipit[waitforp]=skipit[waitforp] >> 1;
		    waitfor[waitforp]='f';
                    break;
		}
		if (prm==CMD_IF || prm==CMD_IFEQ || prm==CMD_IFPL || prm==CMD_IFMI || prm==CMD_ELSIF) { // .if
		    if (prm==CMD_ELSIF && waitfor[waitforp]!='e') {err_msg(ERROR______EXPECTED,".IF"); break;}
		    if (((skipit[waitforp]==1) && prm!=CMD_ELSIF) || ((skipit[waitforp]==2) && prm==CMD_ELSIF)) {
			get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			ignore();if (here()) goto extrachar;
                	if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for condition");val.type=T_NONE;}
		    } else val.type=T_NONE;
                    waitfor[++waitforp]='e';
                    switch (prm) {
                    case CMD_ELSIF:
                        waitforp--;
                        if (((val.type == T_INT || val.type == T_CHR) && val.num) || (val.type == T_STR && val.str.len)) skipit[waitforp]=skipit[waitforp] >> 1; else
                            skipit[waitforp]=skipit[waitforp] & 2;
                        break;
                    case CMD_IF:
                        if (((val.type == T_INT || val.type == T_CHR) && val.num) || (val.type == T_STR && val.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFEQ:
                        if (((val.type == T_INT || val.type == T_CHR) && !val.num) || (val.type == T_STR && !val.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFPL:
                        if (((val.type == T_INT || val.type == T_CHR) && val.num>=0) || (val.type == T_STR && val.str.len)) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFMI:
                        if ((val.type == T_INT || val.type == T_CHR) && val.num<0) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    }
		    break;
		}
                if (!(skipit[waitforp] & 1)) break; //skip things if needed
                if (prm<=CMD_LONG || prm==CMD_BINARY) { // .byte .text .rta .char .int .word .long
                    unsigned long ptextaddr=memdatap;

                    if (prm<CMD_RTA) {    // .byte .text .ptext .char .shift .shift2 .null
                        int ch2=-1;
                        if (prm==CMD_PTEXT) ch2=0;
                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); if (!d) fixeddig=0; //ellenorizve.
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            if (d && w!=3 && w!=0) {err_msg(ERROR_ILLEGAL_OPERA,NULL);break;}
                            if (val.type != T_STR || val.str.len)
                                do {
                                    if (ch2>=0) {
                                        pokeb(ch2);
                                    }

                                    if (val.type == T_STR) {
                                        ch2 = *val.str.data++;
                                        val.str.len--;
                                    } else if (val.type == T_INT) {
                                        if (prm==CMD_CHAR) {
                                            if (val.num>0x7f || val.num<-0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                        } else {
                                            if (val.num & ~0xff) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                        }
                                        ch2 = (unsigned char)val.num;
                                    } else if (val.type == T_CHR) {
                                        ch2 = (unsigned char)val.num;
                                    } else if (val.type == T_NONE) {
                                        ch2 = 0;
                                    } else {err_msg(ERROR____WRONG_TYPE,NULL); break;}

                                    if (prm==CMD_SHIFT || prm==CMD_SHIFT2) {
                                        if (encoding==1 && ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                        if (ch2>=0xc0 && ch2<0xe0) ch2-=0x60; else
                                            if (ch2==0xff) ch2=0x7e; else
                                                if (ch2>=0x80 && d) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                        if (prm==CMD_SHIFT2) ch2<<=1;
                                    } else if (prm==CMD_NULL && !ch2 && d) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                } while (val.type == T_STR && val.str.len);

                            ignore();if ((ch=get())==',') continue;
                            if (ch) err_msg(ERROR______EXPECTED,",");
                            if (ch2>=0) {
                                if (prm==CMD_SHIFT) ch2|=0x80;
                                if (prm==CMD_SHIFT2) ch2|=0x01;
                                pokeb(ch2);
                            }
                            if (prm==CMD_NULL) {
                                pokeb(0);
                            }
                            if (prm==CMD_PTEXT) {
                                if (memdatap-ptextaddr>0x100) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}

                                memdata[ptextaddr]=memdatap-ptextaddr-1;
                            }
                            break;
                        }
                    } else if (prm==CMD_WORD || prm==CMD_INT || prm==CMD_RTA) { // .word .int .rta
                        long ch2;

                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            if (d && w!=3 && w!=1) {err_msg(ERROR_ILLEGAL_OPERA,NULL);break;}
                            if (val.type == T_STR && val.str.len < 5) {
                                ch2 = 0;
                                if (val.str.len>0) ch2 = val.str.data[0];
                                if (val.str.len>1) ch2 |= val.str.data[1] << 8;
                                if (val.str.len>2) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            } else if (val.type == T_INT) {
                                ch2 = val.num;
                                if (prm==CMD_INT) {
                                    if (ch2>0x7fff || ch2<-0x8000) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                } else {
                                    if (ch2 & ~0xffff) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                    if (prm==CMD_RTA)
                                        ch2=(ch2-1) & 0xffff;
                                }
                            } else if (val.type == T_CHR) {
                                ch2 = (unsigned char)val.num;
                            } else if (val.type == T_NONE) {
                                ch2 = 0;
                            } else {err_msg(ERROR____WRONG_TYPE,NULL); break;}

                            pokeb((unsigned char)ch2);
                            pokeb((unsigned char)(ch2>>8));
                            ignore();if ((ch=get())==',') continue;
                            if (ch) err_msg(ERROR______EXPECTED,",");
                            break;
                        }
                    } else if (prm==CMD_LONG) { // .long
                        long ch2;

                        for (;;) {
                            get_exp(&w,&d,&c,&val,T_NONE); //ellenorizve.
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            if (d && w!=3 && w!=2) {err_msg(ERROR_ILLEGAL_OPERA,NULL);break;}
                            if (val.type == T_STR && val.str.len < 5) {
                                ch2 = 0;
                                if (val.str.len>0) ch2 = val.str.data[0];
                                if (val.str.len>1) ch2 |= val.str.data[1] << 8;
                                if (val.str.len>2) ch2 |= val.str.data[2] << 16;
                                if (val.str.len>3) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            } else if (val.type == T_INT) {
                                ch2 = val.num;
                                if (ch2 & ~0xffffff) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            } else if (val.type == T_CHR) {
                                ch2 = (unsigned char)val.num;
                            } else if (val.type == T_NONE) {
                                ch2 = 0;
                            } else { err_msg(ERROR____WRONG_TYPE,NULL); break; }

                            pokeb((unsigned char)ch2);
                            pokeb((unsigned char)(ch2>>8));
                            pokeb((unsigned char)(ch2>>16));
                            ignore();if ((ch=get())==',') continue;
                            if (ch) err_msg(ERROR______EXPECTED,",");
                            break;
                        }
                    } else if (prm==CMD_BINARY) { // .binary
                        long foffset=0,fsize=all_mem+1;
                        if (get_path()) break;
                        if ((ch=get())) {
                            if (ch!=',') goto extrachar;
                            get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            ignore();
                            if (val.type != T_NONE) {
                                foffset = val.num;
                                if (foffset<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            }
                            if ((ch=get())) {
                                if (ch!=',') goto extrachar;
                                get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                                if (!c) break;
                                if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                                ignore();if (here()) goto extrachar;
                                if (val.type != T_NONE) {
                                    fsize = val.num;
                                    if (fsize<0 || fsize>(long)all_mem) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}
                                }
                            }
                        }

                        if ((fil=fopen(path,"rb"))==NULL) {err_msg(ERROR_CANT_FINDFILE,path);break;}
                        fseek(fil,foffset,SEEK_SET);
                        for (;fsize;fsize--) {
                            int st=fgetc(fil);
                            if (st == EOF) break;
                            if (st < 0) err_msg(ERROR_CANT_FINDFILE,path);
                            pokeb(st);
                        }
                        fclose(fil);
                    }

                    if (listing && flist) {
                        if (lastl!=LIST_DATA) {fputc('\n',flist);lastl=LIST_DATA;}
                        fprintf(flist,(all_mem==0xffff)?">%04lx\t":">%06lx ",(address-memdatap+ptextaddr) & all_mem);
                        lcol=arguments.source?25:49;
                        kiirva=1;
                        while (ptextaddr!=memdatap) {
                            if (lcol==1) {
                                if (arguments.source && kiirva) {
                                    if (nprm>=0) mtranslate(mprm,nprm,llist);
                                    fprintf(flist,"\t%s\n",llist);kiirva=0;
                                } else fputc('\n',flist);
                                fprintf(flist,(all_mem==0xffff)?">%04lx\t":">%06lx ",(address-memdatap+ptextaddr) & all_mem);lcol=49;
                            }
                            fprintf(flist," %02x",(unsigned char)memdata[ptextaddr++]);

                            lcol-=3;
                        }
		    
			if (arguments.source && kiirva) {
                            for (i=0; i<lcol-1; i+=8) fputc('\t',flist);
                            if (nprm>=0) mtranslate(mprm,nprm,llist);
			    fprintf(flist,"\t%s\n",llist);
			} else fputc('\n',flist);
		    }
		    break;
		}
		if (prm==CMD_OFFS) {   // .offs
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0; //ellenorizve.
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.num) {
                            if (fixeddig && scpumode) {
                                if (((address + val.num)^address) & ~0xffff) wrapwarn2=1;
                            }
                            address+=val.num;
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
                    unsigned long ch2;
                    if (logisave+1 >= logisize) {
                        logisize += 16;
                        if (!(logitab=realloc(logitab,logisize*sizeof(*logitab)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
                    }
		    logitab[logisave++]=0;
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        ch2=val.num;
                        if (ch2>all_mem) {
                            err_msg(ERROR_CONSTNT_LARGE,NULL); 
                            break;
                        }
                    } else ch2=l_address;
		    logitab[logisave-1]=l_address-address;
                    l_address=ch2;
		    break;
		}
		if (prm==CMD_HERE) { // .here
		    if (here()) goto extrachar;
		    if (!logisave) {err_msg(ERROR______EXPECTED,".LOGICAL"); break;}
		    l_address=address+logitab[--logisave];
		    break;
		}
		if (prm==CMD_AS) { // .as
		    if (here()) goto extrachar;
                    longaccu=0;
		    break;
		}
		if (prm==CMD_AL) { // .al
		    if (here()) goto extrachar;
                    longaccu=1;
		    break;
		}
		if (prm==CMD_XS) { // .xs
		    if (here()) goto extrachar;
		    longindex=0;
		    break;
		}
		if (prm==CMD_XL) { // .xl
		    if (here()) goto extrachar;
		    longindex=1;
		    break;
		}
		if (prm==CMD_ERROR) { // .error
		    err_msg(ERROR__USER_DEFINED,&pline[lpoint]);
		    break;
		}
		if (prm==CMD_PROC) { // .proc
		    if (here()) goto extrachar;
		    if (tmp) {
			if (tmp->proclabel && pass!=1 && old_context == &root_context) wait_cmd(fin,CMD_PEND);//.pend
                        else {
		            tmp->proclabel=1;
                            current_context=new_context(ident, current_context);
                            current_context->backr=current_context->forwr=1;
                            if (listing && flist && arguments.source) {
                                if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident2);
                            }
                        }
		    }
		    break;
		}
		if (prm==CMD_PEND) { //.pend
		    if (here()) goto extrachar;
                    if (current_context->parent && current_context->name[0]!='.') {
                        current_context->parent->backr += current_context->backr - 1;
                        current_context->parent->forwr += current_context->forwr - 1;
                        current_context = current_context->parent;
                    } else err_msg(ERROR______EXPECTED,".proc");
		    break;
		}
                if (prm==CMD_BLOCK) { // .block
                    if (here()) goto extrachar;
                    sprintf(varname, ".%lu.%ld", reffile, sline);
                    current_context=new_context(varname, current_context);
                    current_context->backr=current_context->forwr=1;
                    break;
                }
                if (prm==CMD_BEND) { //.bend
                    if (here()) goto extrachar;
                    if (current_context->parent && current_context->name[0]=='.') {
                        current_context->parent->backr += current_context->backr - 1;
                        current_context->parent->forwr += current_context->forwr - 1;
                        current_context = current_context->parent;
                    } else err_msg(ERROR______EXPECTED,".block");
                    break;
                }
		if (prm==CMD_DATABANK) { // .databank
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        databank=val.num;
                    }
		    break;
		}
		if (prm==CMD_DPAGE) { // .dpage
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val_length(val.num)>1) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        if (dtvmode) dpage=val.num & 0xff00;
                        else dpage=val.num;
                    }
		    break;
		}
		if (prm==CMD_FILL) { // .fill
                    unsigned long db = 0;
                    long ch;
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (val.type != T_NONE) {
                        db=val.num;
                        if (db>all_mem) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}
                    }
                    if ((ch=get())) {
                        if (ch!=',') goto extrachar;
                        get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        ignore();if (here()) goto extrachar;
                        if (val.type != T_NONE) {
                            if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            ch = val.num;
                        } else ch = 0;
                        while (db-->0) pokeb((unsigned char)ch);
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
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        current_provides=val.num;
                    } else current_provides=~0;
		    get_exp(&w,&d,&c,&val,T_INT);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        current_requires=val.num;
                    } else current_requires=0;
		    get_exp(&w,&d,&c,&val,T_INT);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        current_conflicts=val.num;
                    } else current_conflicts=0;
		    break;
		}
		if (prm==CMD_CHECK) { // .check
		    get_exp(&w,&d,&c,&val,T_INT);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        if ((val.num & current_provides)!=(unsigned long)val.num) {err_msg(ERROR_REQUIREMENTS_,".CHECK");break;}
                    }
		    get_exp(&w,&d,&c,&val,T_INT);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.num & current_provides) err_msg(ERROR______CONFLICT,".CHECK");
                    }
		    break;
		}
		if (prm==CMD_WARN) { // .warn
		    err_msg(ERROR_WUSER_DEFINED,&pline[lpoint]);
		    break;
		}
                if (prm==CMD_ENC) { // .enc
                    get_path();
                    if (!strcasecmp(path,"none")) encoding=0;
                    else
                        if (!strcasecmp(path,"screen")) encoding=1;
                        else
                            err_msg(ERROR_UNKNOWN_ENCOD,ident);
		    break;
		}
                if (prm==CMD_CPU) { // .cpu
                    int def;
                    get_path();
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
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (here()==',') {
                        lpoint++;ignore();
                    } else if (here()) goto extrachar;
                    if (((val.type == T_INT || val.type == T_CHR) && val.num) || (val.type == T_STR && val.str.len)) err_msg((prm==CMD_CERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED,&pline[lpoint]);
                    break;
                }
		if (prm==CMD_ENDM) { // .endm
		    if (tpe==1 || tpe==3) { // .macro
			if (here()) goto extrachar;
                        goto end;
		    } else {err_msg(ERROR______EXPECTED,".MACRO"); break;}
		}
		if (prm==CMD_NEXT) { // .next
		    if (tpe==2) { //.rept .for
			if (here()) goto extrachar;
                        goto end;
		    } else {err_msg(ERROR______EXPECTED,".FOR or .REPT"); break;}
		}
		if (prm==CMD_REPT) { // .rept
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for count");wait_cmd(fin,CMD_NEXT);break;}
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        lin=sline;
                        pos=fin->currentp;
                        enterfile(nam,lin);
                        for (cnt=0; cnt<val.num; cnt++) {
                            compile(nam,pos,2,mprm,nprm,fin);
                            sline=lin;
                        }
                        exitfile();
                    }
	            wait_cmd(fin,CMD_NEXT);
		    break;
		}
                if (prm==CMD_ALIGN) { // .align
                    int align, fill=-1;
		    get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (val.type != T_NONE) {
                        if (val.num<1 || val.num>(long)all_mem) {
                            err_msg(ERROR_CONSTNT_LARGE,NULL); 
                            break;
                        }
                        align = val.num;
                    } else align = 1;
                    if ((ch=get())) {
                        int d2;
                        if (ch!=',') goto extrachar;
                        get_exp(&w,&d2,&c,&val,T_INT);if (!d2) fixeddig=0;
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        ignore();if (here()) goto extrachar;
                        if (val.type != T_NONE) {
                            if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            fill = val.num;
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
                    if (!c) break;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        outputeor = val.num;
                    } else outputeor = 0;
                    break;
                }
                if (prm==CMD_END) {
                    goto end;
                }
                if (prm==CMD_PRON) {
                    if (here()) goto extrachar;
                    listing = (flist != NULL);
                    break;
                }
                if (prm==CMD_PROFF) {
                    if (here()) goto extrachar;
                    listing = 0;
                    break;
                }
                if (prm==CMD_SHOWMAC || prm==CMD_HIDEMAC) {
                    err_msg(ERROR_DIRECTIVE_IGN,NULL); 
                    break;
                }
		if (prm==CMD_ENDC) {err_msg(ERROR______EXPECTED,".COMMENT"); break;} // .endc
		if (prm==CMD_COMMENT) { // .comment
                    if (here()) goto extrachar;
		    wait_cmd(fin,CMD_ENDC);
		    break;
		}
		if (prm==CMD_INCLUDE) { // .include
                    if (get_path()) break;
                    if (here()) goto extrachar;
                    lin=sline;
                    enterfile(path,lin);
                    sline=0;
                    if (listing && flist) {
                        fprintf(flist,"\n;******  Processing file \"%s\"\n",path);
                        lastl=LIST_NONE;
                    }
                    compile(path,0,0,mprm,nprm,NULL);
                    exitfile();
                    if (listing && flist) {
                        fprintf(flist,"\n;******  Return to file \"%s\"\n",filenamelist->name);
                        lastl=LIST_NONE;
                    }
		    sline=lin;
		    break;
		}
		if (prm==CMD_FOR) { // .for
		    int apoint=0;
		    char expr[linelength];

		    if ((wht=what(&prm))==WHAT_EXPRESSION && prm==1) { //label
			if (get_ident('_')) break;
			ignore();if (get()!='=') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
			strcpy(varname,ident);
			get_exp(&w,&d,&c,&val,T_NONE);
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for start");break;}
                        tmp=new_label(varname);
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
			if (!labelexists) tmp->proclabel=0;
			tmp->value=val;
                        if (pass==1) tmp->used=pass;
			wht=what(&prm);
		    }
		    if (wht==WHAT_S || wht==WHAT_Y || wht==WHAT_X) lpoint--; else
			if (wht!=WHAT_COMA) {err_msg(ERROR______EXPECTED,","); break;}

		    strcpy(expr,&pline[lpoint]);
		    strcpy(pline,expr);
		    lin=sline;
                    pos=fin->currentp;
                    enterfile(nam,lin);
		    for (;;) {
			lpoint=0;
			get_exp(&w,&d,&c,&val,T_NONE);
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used in condition");break;}
                        if (((val.type == T_INT || val.type == T_CHR) && !val.num) || (val.type == T_STR && !val.str.len)) break;
			if (!apoint) {
                            ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
			    ignore();if (!get()) continue;
			    lpoint--;
			    if (get_ident('_')) break;
			    ignore();if (get()!='=') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
			    apoint=lpoint;
                            tmp=new_label(ident);
                            tmp->requires=current_requires;
                            tmp->conflicts=current_conflicts;
			    if (!labelexists) tmp->proclabel=0;
			    if (pass==1) tmp->used=pass;
                        }
			compile(nam,pos,2,mprm,nprm,fin);
			sline=lin;
			strcpy(pline,expr);
			lpoint=apoint;
			get_exp(&w,&d,&c,&val,T_NONE);
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			ignore();if (here()) goto extrachar;
			tmp->value=val;
                    }
                    exitfile();
                    wait_cmd(fin,CMD_NEXT);
		    break;
		}
		if (prm==CMD_ENDP) { // .endp
		    if (here()) goto extrachar;
		    if (pagelo==-1) {err_msg(ERROR______EXPECTED,".PAGE"); break;}
		    if ((l_address>>8) != (unsigned long)pagelo) {
                        err_msg(ERROR____PAGE_ERROR,NULL);
                    }
		    pagelo=-1;
		    break;
		}
		if (prm==CMD_PAGE) { // .page
		    if (here()) goto extrachar;
		    if (pagelo!=-1) {err_msg(ERROR______EXPECTED,".ENDP"); break;}
		    pagelo=(l_address>>8);
		    break;
		}
		if (prm==CMD_OPTION) { // .option
                    get_ident('_');
                    ignore();if (get()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                    get_exp(&w,&d,&c,&val,T_NONE);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for option");break;}
                    if (!strcasecmp(ident,"allow_branch_across_page")) allowslowbranch=(((val.type == T_INT || val.type == T_CHR) && val.num) || (val.type == T_STR && val.str.len));
                    else if (!strcasecmp(ident,"auto_longbranch_as_jmp")) longbranchasjmp=(((val.type == T_INT || val.type == T_CHR) && val.num) || (val.type == T_STR && val.str.len));
                    else err_msg(ERROR_UNKNOWN_OPTIO,ident);
		    break;
		}
	    }
	case WHAT_HASHMARK:if (skipit[waitforp] & 1) //skip things if needed
	    {                   //macro stuff
		int ppoint, nprm;
                if (get_ident2('_')) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                if (!(tmp2=find_macro(ident))) {err_msg(ERROR___NOT_DEFINED,ident); break;}
            as_macro:
                if (listing && flist && arguments.source && ident2[0] && tmp->used>=pass-1) {
                    if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                    fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident2);
                }
		ppoint=nprm=0;
                ignore();
		while ((ch=get())) {
                    if (ch=='"' || ch=='\'') {
                        char quo=ch;
			for (;;) {
			    if (!(ch=get())) {err_msg(ERROR______EXPECTED,"End of string"); break;}
			    if (ch==quo) {
				if (here()!=quo) break;
                                lpoint++;
			    }
			    mparams[ppoint++]=ch;
			}
		    }
		    else {
			do mparams[ppoint++]=ch; while ((ch=get())!=',' && ch);
			lpoint--;
		    }
		    nprm++;
                    mparams[ppoint++]=0;
                    ignore();
                    if (!(ch=get())) break;
		    if (ch!=',') {err_msg(ERROR______EXPECTED,","); break;}
		}
                lin=sline;
                sline=tmp2->lin;
                macrecursion++;
                if (tmp2->type==CMD_MACRO) {
                    sprintf(varname, "#%lu#%ld#%d", reffile, lin, macrecursion);
                    old_context = current_context;
                    current_context=new_context(varname, current_context);
                    current_context->backr=current_context->forwr=1;
                }
                if (macrecursion<100) {
                    enterfile(tmp2->file->name,lin);
                    compile(tmp2->file->name,tmp2->point,(tmp2->file!=fin)?1:3,mparams,nprm,tmp2->file);
                    exitfile();
                } else err_msg(ERROR__MACRECURSION,"!!!!");
                if (tmp2->type==CMD_MACRO) current_context = old_context;
                macrecursion--;
                sline=lin;
		break;
	    }
        case WHAT_EXPRESSION:
            if (skipit[waitforp] & 1) {
                get_ident2('_');
                if ((prm=lookup_opcode(ident))>=0) {
                    int opr, mnem;
                    int oldlpoint;
                    const unsigned char* cnmemonic; //current nmemonic
                    char ln;
                    unsigned char cod, longbranch;
                    unsigned long adr;
                as_opcode:
                    if (!(skipit[waitforp] & 1)) break;//skip things if needed

                    opr = 0;mnem = prm;
                    oldlpoint = lpoint;
                    cnmemonic = &opcode[prm*24];
                    ln = 0; cod = 0; longbranch = 0; adr = 0;

                    ignore();
                    if (!(wht=here())) {
                        cod=cnmemonic[ADR_IMPLIED];
                        opr=(cnmemonic[ADR_ACCU]==cnmemonic[ADR_IMPLIED])?ADR_ACCU:ADR_IMPLIED;w=ln=0;d=1;
                    }  //clc
                    // 1 Db
                    else if (lowcase(wht)=='a' && pline[lpoint+1]==0 && cnmemonic[ADR_ACCU]!=____)
                    {
                        cod=cnmemonic[ADR_ACCU];
                        if (find_label("a")) err_msg(ERROR_A_USED_AS_LBL,NULL);
                        opr=ADR_ACCU;w=ln=0;d=1;// asl a
                        lpoint++;
                    }
                    // 2 Db
                    else if (wht=='#') {
                        lpoint++;
                        get_exp(&w,&d,&c,&val,T_INT); //ellenorizve.
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}

                        ln=1;
                        if ((cod=cnmemonic[ADR_IMMEDIATE])==0xE0 || cod==0xC0 || cod==0xA2 || cod==0xA0) {// cpx cpy ldx ldy
                            if (longindex && scpumode) ln++;
                        }
                        else if (cod==0xF4) ln=2; //pea #$ffff
                        else if (cod!=0xC2 && cod!=0xE2) {//not sep rep=all accu
                            if (longaccu && scpumode) ln++;
                        }
                        if (dtvmode && cod==0x02) longbranch=0x40;//hack

                        if (val.type != T_NONE) {
                            adr=val.num;
                            if (w==3) w=val_length(adr);//auto length
                            if (w>=ln) w=3; //const too large
                            opr=ADR_IMMEDIATE;// lda #
                        } else fixeddig=0;
                    }
                    // 3 Db
                    else if (wht=='[') {
                        lpoint++;
                        get_exp(&w,&d,&c,&val,T_INT); //ellenorizve.
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        ignore();if (get()!=']') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        if ((wht=what(&prm))==WHAT_Y) {
                            if (val.type != T_NONE) {
                                adr = val.num - dpage;
                                if (w==3) w=val_length(adr);//auto length
                                if (w) w=3;// there's no lda [$ffff],y lda [$ffffff],y!
                                opr=ADR_ZP_LI_Y;
                            } else fixeddig=0;
                            ln=1; // lda [$ff],y
                        }
                        else if (wht==WHAT_EOL) {
                            if (cnmemonic[ADR_ADDR_LI]==0xDC) { // jmp [$ffff]
                                if (val.type != T_NONE) {
                                    adr = val.num;
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
                                    adr = val.num - dpage;
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w) w=3; // there's no lda [$ffff] lda [$ffffff]!
                                    opr=ADR_ZP_LI;
                                } else fixeddig=0;
                                ln=1;// lda [$ff]
                            }             
                            lpoint--;
                        }
                    }
                    else if (wht=='.') {
                        wht=what(&prm);
                        if (wht==WHAT_COMMAND && (prm==CMD_MACRO || prm==CMD_SEGMENT)) {
                            memcpy(ident,&mnemonic[mnem*3],3);
                            ident[3]=0;goto do_macro;
                        }
                        err_msg(ERROR_GENERL_SYNTAX,NULL);break;
                    }
                    else {
                        if (whatis[wht]!=WHAT_EXPRESSION && whatis[wht]!=WHAT_CHAR && wht!='_' && wht!='*') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        get_exp(&w,&d,&c,&val,T_INT);if (!d) fixeddig=0; //ellenorizve.
                        if (!c) break;
                        if (val.type != T_NONE) {
                            adr = val.num;
                        }
                    meg:
                        if (c==1) {
                            if ((wht=what(&prm))==WHAT_X) {// lda $ff,x lda $ffff,x lda $ffffff,x
                                if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                                if (w==3) {//auto length
                                    if (val.type != T_NONE) {
                                        if (cnmemonic[ADR_ZP_X]!=____ && adr>=dpage && adr<(dpage+0x100)) {adr-=dpage;w=0;}
                                        else if (cnmemonic[ADR_ADDR_X]!=____ && databank==(adr >> 16)) w=1;
                                        else {
                                            w=val_length(adr);
                                            if (w<2) w=2;
                                        }
                                    } else w=(cnmemonic[ADR_ADDR_X]!=____);
                                } else {
                                    if (!w && adr>=dpage && adr<(dpage+0x100)) adr-=dpage;
                                    if (databank==(adr >> 16) && w<2) adr&=0xffff;
                                    if (w<val_length(adr)) w=3;
                                }
                                opr=ADR_ZP_X-w;ln=w+1;
                            }// 6 Db
                            else if (wht==WHAT_Y) {// lda $ff,y lda $ffff,y lda $ffffff,y
                                if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                                if (w==3) {//auto length
                                    if (val.type != T_NONE) {
                                        if (cnmemonic[ADR_ZP_Y]!=____ && adr>=dpage && adr<(dpage+0x100)) {adr-=dpage;w=0;}
                                        else if (databank==(adr >> 16)) w=1;
                                    } else w=(cnmemonic[ADR_ADDR_Y]!=____);
                                } else {
                                    if (!w && adr>=dpage && adr<(dpage+0x100)) adr-=dpage;
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
                                struct svalue val2;
                                megint:
                                d2=d;
                                get_exp(&w2,&d,&c2,&val2,T_INT);if (!d) fixeddig=0;
                                if (!c2) break;
                                if (c2==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                                if (cnmemonic[ADR_REL]!=____) {
                                    long valx,valx2;
                                    if (val.type != T_NONE && val2.type != T_NONE) {
                                        valx=val.num-l_address-2;
                                        valx2=val2.num-l_address-2;
                                        if ((valx<-128 || valx>127) && valx2>=-128 && valx2<=127) {
                                            val=val2;
                                            adr = val.num;
                                            c=c2;
                                            w=w2;
                                        } else d=d2;
                                    } else {
                                        if (d2) d=d2;
                                        else {
                                            val=val2;
                                            if (val2.type != T_NONE) adr = val.num;
                                            c=c2;w=w2;
                                        }
                                    }
                                    goto meg;
                                }
                                adr <<= 8;
                                if (val.type != T_NONE && val2.type != T_NONE) {
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w2==3) w2=val_length(val2.num);//auto length
                                    if (w || w2) w=3; // only byte operands...
                                    opr=ADR_MOVE;
                                    adr|=val2.num;
                                }
                                ln=2;
                            }// 10 Db
                            else if (wht==WHAT_EOL) {
                                if (cnmemonic[ADR_REL]!=____) {
                                    ln=1;
                                    if (val.type != T_NONE) {
                                        if (fixeddig && (l_address >> 16)!=(adr >> 16)) {err_msg(ERROR_BRANCH_TOOFAR,NULL); break;}
                                        adr=(adr-l_address-2) & 0xffff;
                                        if (adr<0xFF80 && adr>0x007F) {
                                            if (arguments.longbranch && (cnmemonic[ADR_ADDR]==____)) {
                                                if ((cnmemonic[ADR_REL] & 0x1f)==0x10) {//branch
                                                    longbranch=0x20;ln=4;
                                                    if (scpumode && !longbranchasjmp) {
                                                        adr=0x8203+(((adr-3) & 0xffff) << 16);
                                                    } else {
                                                        adr=0x4C03+(((adr+l_address+2) & 0xffff) << 16);
                                                    }
                                                } else {//bra
                                                    if (scpumode && !longbranchasjmp) {
                                                        longbranch=cnmemonic[ADR_REL]^0x82;
                                                        adr=(adr-1) & 0xffff;
                                                        ln=2;
                                                    } else {
                                                        longbranch=cnmemonic[ADR_REL]^0x4C;
                                                        adr=(adr+l_address+2) & 0xffff;ln=2;
                                                    }
                                                }    
                                                if (fixeddig) err_msg(ERROR___LONG_BRANCH,NULL);
                                            } else {
                                                if (cnmemonic[ADR_ADDR]!=____) {
                                                    adr=(adr+l_address+2) & 0xffff;
                                                    opr=ADR_ADDR;w=1;ln=2;goto brancb;}
                                                else if (cnmemonic[ADR_REL_L]!=____) {//gra
                                                    adr=(adr-1) & 0xffff;
                                                    opr=ADR_REL_L;w=1;ln=2;goto brancb;}
                                                else if (fixeddig) err_msg(ERROR_BRANCH_TOOFAR,NULL);
                                            }
                                        }
                                        if (fixeddig) {
                                            if (!longbranch && ((l_address+2) & 0xff00)!=((l_address+2+adr) & 0xff00)) {
                                                if (!allowslowbranch) err_msg(ERROR__BRANCH_CROSS,NULL);
                                            }
                                        }
                                        opr=ADR_REL;w=0;// bne
                                    }
                                }
                                else if (cnmemonic[ADR_REL_L]!=____) {
                                    if (val.type != T_NONE) {
                                        if (fixeddig && (l_address >> 16)!=(adr >> 16)) {err_msg(ERROR_BRANCH_TOOFAR,NULL); break;}
                                        adr=(adr-l_address-3) & 0xffff;
                                        opr=ADR_REL_L;w=1;//brl
                                    }
                                    ln=2;
                                }
                                else if (cnmemonic[ADR_LONG]==0x5C || cnmemonic[ADR_LONG]==0x22) {
                                    if (w==3) {
                                        if (cnmemonic[ADR_ADDR]==____) w=2; // jml jsl
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
                                else {
                                    if (w==3) {//auto length
                                        if (val.type != T_NONE) {
                                            if (cnmemonic[ADR_ZP]!=____ && adr>=dpage && adr<(dpage+0x100)) {adr-=dpage;w=0;}
                                            else if (cnmemonic[ADR_ADDR]!=____ && databank==(adr >> 16)) w=1;
                                            else {
                                                w=val_length(adr);
                                                if (w<2) w=2;
                                            }
                                        } else w=1;
                                    } else {
                                        if (!w && adr>=dpage && adr<(dpage+0x100)) adr-=dpage;
                                        if (databank==(adr >> 16) && w<2) adr&=0xffff;
                                        if (w<val_length(adr)) w=3;
                                    }
                                    opr=ADR_ZP-w;ln=w+1; // lda $ff lda $ffff lda $ffffff
                                }
                                brancb: lpoint--;
                            }// 13+2 Db
                        }
                        else if (c==2) {
                            if ((wht=what(&prm))==WHAT_SZ) {
                                if ((wht=what(&prm))!=WHAT_Y) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                                if (val.type != T_NONE) {
                                    if (w==3) w=val_length(adr);//auto length
                                    if (w) w=3; // there's no lda ($ffffff,s),y or lda ($ffff,s),y!
                                    opr=ADR_ZP_S_I_Y;
                                }
                                ln=1; // lda ($ff,s),y
                            } // 16 Db
                            else {
                                if (wht!=WHAT_XZ) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                                if (cnmemonic[ADR_ADDR_X_I]==0x7C || cnmemonic[ADR_ADDR_X_I]==0xFC) {// jmp ($ffff,x) jsr ($ffff,x)
                                    if (val.type != T_NONE) {
                                        if ((l_address >> 16)==(adr >> 16)) w=1; else w=3; // only same program bank!
                                        opr=ADR_ADDR_X_I;
                                    }
                                    ln=2; // jmp ($ffff,x)
                                }
                                else {
                                    if (val.type != T_NONE) {
                                        adr-=dpage;
                                        if (w==3) w=val_length(adr);//auto length
                                        if (w) w=3; // there's no lda ($ffff,x) lda ($ffffff,x)!
                                        opr=ADR_ZP_X_I;
                                    }
                                    ln=1; // lda ($ff,x)
                                }
                            } // 18 Db
                        }
                        else {
                            if ((wht=what(&prm))==WHAT_Y) {
                                if (val.type != T_NONE) {
                                    adr-=dpage;
                                    if (w==3) w=val_length(adr);
                                    if (w) w=3;
                                    opr=ADR_ZP_I_Y;
                                }
                                ln=1; // lda ($ff),y
                            } // 19 Db
                            else if (wht==WHAT_EOL) {
                                if (cnmemonic[ADR_ADDR_I]==0x6C) {// jmp ($ffff)
                                    if (val.type != T_NONE) {
                                        if (fixeddig) {
                                            if (w==3) {
                                                w=val_length(adr);//auto length
                                                if (!w) w=1;
                                            }
                                            if (w!=1) w=3; // there's no jmp ($ffffff)!
                                            if (!scpumode && (adr & 0xff)==0xff) err_msg(ERROR______JUMP_BUG,NULL);//jmp ($xxff)
                                        } else w=1;
                                        opr=ADR_ADDR_I;
                                    }
                                    ln=2; // jmp ($ffff)
                                }
                                else {
                                    if (val.type != T_NONE) {
                                        adr-=dpage;
                                        if (w==3) w=val_length(adr);//auto length
                                        if (w) w=3; // there's no lda ($ffff) lda ($ffffff)!
                                        opr=ADR_ZP_I;
                                    }
                                    ln=1; // lda ($ff)
                                }
                                lpoint--;
                            } // 21 Db
                        }
                    } 
                    if (here()) {extrachar:err_msg(ERROR_EXTRA_CHAR_OL,NULL); break;}

                    if (d) {
                        if (w==3) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        if ((cod=cnmemonic[opr])==____) {
                            memcpy(ident,&mnemonic[mnem*3],3);
                            ident[3]=0;
                            if ((tmp2=find_macro(ident))) {
                                lpoint=oldlpoint;
                                goto as_macro;
                            }
                            err_msg(ERROR_ILLEGAL_OPERA,NULL);
                            break;
                        }
                    }
                    {
                        unsigned long temp=adr;
                        pokeb(cod ^ longbranch);
                        switch (ln)
                        {
                        case 4:pokeb((unsigned char)temp);temp>>=8;
                        case 3:pokeb((unsigned char)temp);temp>>=8;
                        case 2:pokeb((unsigned char)temp);temp>>=8;
                        case 1:pokeb((unsigned char)temp);
                        }
                    }

                    if (listing && flist) {
                        unsigned long temp=adr;
                        int i;

                        if (lastl!=LIST_CODE) {fputc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04lx\t %02x":".%06lx  %02x",(address-ln-1) & all_mem,(unsigned char)(cod ^ longbranch ^ outputeor));

                        for (i=0;i<ln;i++) {fprintf(flist," %02x",(unsigned char)temp ^ outputeor);temp>>=8;}
                        if (ln<2) fputc('\t',flist);
                        fputc('\t',flist);

                        if (arguments.monitor) {
                            for (i=0;i<3;i++) fputc(mnemonic[mnem*3+i],flist);

                            switch (opr) {
                            case ADR_IMPLIED: fprintf(flist,"\t"); break;
                            case ADR_ACCU: fprintf(flist," a\t"); break;
                            case ADR_IMMEDIATE:
                                {
                                    if (ln==1) fprintf(flist," #$%02x",(unsigned char)adr);
                                    else fprintf(flist," #$%04x",(unsigned)(adr&0xffff));
                                    break;
                                }
                            case ADR_LONG: fprintf(flist," $%06x",(unsigned)(adr&0xffffff)); break;
                            case ADR_ADDR: 
                                if (cnmemonic[ADR_LONG]==0x5C || cnmemonic[ADR_LONG]==0x22)
                                    fprintf(flist," $%06x",(unsigned)(adr&0xffff)+(unsigned)(l_address&0xff0000));
                                else
                                    fprintf(flist," $%04x",(unsigned)(adr&0xffff));
                                break;
                            case ADR_ZP: fprintf(flist," $%02x\t",(unsigned char)adr); break;
                            case ADR_LONG_X: fprintf(flist," $%06x,x",(unsigned)(adr&0xffffff)); break;
                            case ADR_ADDR_X: fprintf(flist," $%04x,x",(unsigned)(adr&0xffff)); break;
                            case ADR_ZP_X: fprintf(flist," $%02x,x",(unsigned char)adr); break;
                            case ADR_ADDR_X_I: fprintf(flist,(all_mem==0xffff)?" ($%04x,x)":" ($%06x,x)",(unsigned)(adr&0xffff)+(unsigned)(l_address&0xff0000)); break;
                            case ADR_ZP_X_I: fprintf(flist," ($%02x,x)",(unsigned char)adr); break;
                            case ADR_ZP_S: fprintf(flist," $%02x,s",(unsigned char)adr); break;
                            case ADR_ZP_S_I_Y: fprintf(flist," ($%02x,s),y",(unsigned char)adr); break;
                            case ADR_ADDR_Y: fprintf(flist," $%04x,y",(unsigned)(adr&0xffff)); break;
                            case ADR_ZP_Y: fprintf(flist," $%02x,y",(unsigned char)adr); break;
                            case ADR_ZP_LI_Y: fprintf(flist," [$%02x],y",(unsigned char)adr); break;
                            case ADR_ZP_I_Y: fprintf(flist," ($%02x),y",(unsigned char)adr); break;
                            case ADR_ADDR_LI: fprintf(flist," [$%04x]",(unsigned)(adr&0xffff)); break;
                            case ADR_ZP_LI: fprintf(flist," [$%02x]",(unsigned char)adr); break;
                            case ADR_ADDR_I: fprintf(flist," ($%04x)",(unsigned)(adr&0xffff)); break;
                            case ADR_ZP_I: fprintf(flist," ($%02x)",(unsigned char)adr); break;
                            case ADR_REL:
                                if (ln==1) fprintf(flist,(all_mem==0xffff)?" $%04x":" $%06x",(unsigned)((((signed char)adr)+l_address)&all_mem));
                                else if (ln==2) {
                                    if ((cod ^ longbranch)==0x4C)
                                        fprintf(flist,(all_mem==0xffff)?" $%04x":" $%06x",(unsigned)((adr&0xffff)+(l_address&0xff0000)));
                                    else
                                        fprintf(flist,(all_mem==0xffff)?" $%04x":" $%06x",(unsigned)((((signed short int)adr)+l_address)&all_mem));
                                }
                                else fprintf(flist,(all_mem==0xffff)?" $%04x":" $%06x",(unsigned)(((adr >> 16)&0xffff)+(l_address&0xff0000)));
                                break;
                            case ADR_REL_L: fprintf(flist," $%06x",(unsigned)((((signed short)adr)+l_address)&0xffffff)); break;
                            case ADR_MOVE: fprintf(flist," $%02x,$%02x",(unsigned char)adr,(unsigned char)(adr>>8));
                            }
                        } else if (arguments.source) fputc('\t',flist);
                        if (arguments.source) {
                            if (nprm>=0) mtranslate(mprm,nprm,llist);
                            fprintf(flist,"\t%s\n",llist);
                        } else fputc('\n',flist);
                    }
                    break;
                }
                if ((tmp2=find_macro(ident))) goto as_macro;
            }
            // fall through
	default: if (skipit[waitforp] & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); //skip things if needed
	}
    }
end:
    if (oldpos==-1) closefile(fin); else fin->currentp=oldpos;
    if (tpe==0) {
        current_context->backr=backr_old;
        current_context->forwr=forwr_old;
        reffile=reffile_old;
    }
    return;
}

int main(int argc,char *argv[]) {
    time_t t;
    FILE* fout;

    tinit();
    testarg(argc,argv);

    if (arguments.quiet)
    fprintf(stdout,"6502/65C02 Turbo Assembler Version 1.3  Copyright (c) 1997 Taboo Productions\n"
                   "6502/65C02 Turbo Assembler Version 1.35 ANSI C port by [BiGFooT/BReeZe^2000]\n"
                   "6502/65C02/65816/DTV TASM Version " VERSION " Fixing by Soci/Singular 2001-2011\n"
                   "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
                   "are welcome to redistribute it under certain conditions; See LICENSE!\n\n");

    /* assemble the input file(s) */
    do {
        if (pass++>20) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        set_cpumode(arguments.cpumode);
	address=l_address=databank=dpage=longaccu=longindex=0;encoding=0;wrapwarn=0;wrapwarn2=0;
        current_provides=~0;current_requires=0;current_conflicts=0;macrecursion=0;allowslowbranch=1;
        fixeddig=1;waitfor[waitforp=0]=0;skipit[0]=1;sline=0;conderrors=warnings=0;freeerrorlist(0);outputeor=0;
        current_context=&root_context;memdatap=0;memblocklastp=0;memblockp=0;memblocklaststart=0;logisave=0;
        /*	listing=1;flist=stderr;*/
        enterfile(arguments.input,0);
        sline=0;
        compile(arguments.input,0,0,"",-1,NULL);
        exitfile();
        if (errors) {memcomp();status();return 1;}
        if (conderrors && !arguments.list && pass==1) fixeddig=0;
    } while (!fixeddig || (pass==1 && !arguments.list));

    /* assemble again to create listing */
    if (arguments.list) {
        listing=1;
        if (arguments.list[0] == '-' && !arguments.list[1]) {
            flist = stdout;
        } else {
            if (!(flist=fopen(arguments.list,"wt"))) err_msg(ERROR_CANT_DUMP_LST,arguments.list);
        }
	fprintf(flist,"\n;6502/65C02/65816/DTV Turbo Assembler V" VERSION " listing file of \"%s\"\n",arguments.input);
	time(&t);
        fprintf(flist,";done on %s",ctime(&t));
        lastl=LIST_NONE;

        pass++;
        set_cpumode(arguments.cpumode);
	address=l_address=databank=dpage=longaccu=longindex=0;encoding=0;wrapwarn=0;wrapwarn2=0;
        current_provides=~0;current_requires=0;current_conflicts=0;macrecursion=0;allowslowbranch=1;
        fixeddig=1;waitfor[waitforp=0]=0;skipit[0]=1;sline=0;conderrors=warnings=0;freeerrorlist(0);outputeor=0;
        current_context=&root_context;memdatap=0;memblocklastp=0;memblockp=0;memblocklaststart=0;logisave=0;
        enterfile(arguments.input,0);
        sline=0;
        compile(arguments.input,0,0,"",-1,NULL);
        exitfile();
	fprintf(flist,"\n;******  end of code\n");
	if (flist != stdout) fclose(flist);
    }
    memcomp();

    set_cpumode(arguments.cpumode);

    if (arguments.label) labelprint();

    if (errors || conderrors) {status();return 1;}

    /* output file */
    if (memdatap) {
        unsigned long i, start, last, size;
        if (arguments.output[0] == '-' && !arguments.output[1]) {
            fout = stdout;
        } else {
            if ((fout=fopen(arguments.output,"wb"))==NULL) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
        }
        clearerr(fout);
        if (memblockp) {
            start = memblocks[0].start;
            size = memblocks[0].size;
            last = 0;
            for (i=1;i<memblockp;i++) {
                if (memblocks[i].start != start + size) {
                    if (arguments.nonlinear) {
                        fputc(size,fout);
                        fputc(size >> 8,fout);
                        if (scpumode) fputc(size >> 16,fout);
                    }
                    if (!arguments.stripstart || arguments.nonlinear) {
                        fputc(start,fout);
                        fputc(start >> 8,fout);
                        if (scpumode) fputc(start >> 16,fout);
                    }
                    while (last<i) {
                        fwrite(memdata+memblocks[last].memp,memblocks[last].size,1,fout);
                        last++;
                    }
                    if (!arguments.nonlinear) {
                        size = memblocks[i].start - start - size;
                        while (size--) fputc(0, fout);
                    }
                    start = memblocks[i].start;
                    size = 0;
                }
                size += memblocks[i].size;
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
                fwrite(memdata+memblocks[last].memp,memblocks[last].size,1,fout);
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
