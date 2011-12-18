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

#define linelength 4096
#define nestinglevel 256
int errors=0,conderrors=0,warnings=0, wrapwarn=0, wrapwarn2=0;
long sline;      //current line
static unsigned long low_mem,top_mem;
static unsigned long all_mem, full_mem;
static int pass=0;      //pass
static int listing=0;   //listing
static int foundopcode;   //listing
static unsigned char* mem64=NULL;//c64mem
static unsigned char* mmap=NULL; //c64 memory map
unsigned long address=0,l_address=0; //address, logical address
char pline[linelength];  //current line data
static char llist[linelength];  //current line for listing
static int lpoint;              //position in current line
static char ident[linelength], ident2[linelength];  //identifier (label, etc)
static char varname[linelength];//variable (same as identifier?)
static char path[80];           //path
static int pagelo=-1;           //still in same page?
static FILE* flist = NULL;      //listfile
static char lastl=1;            //for listing (skip one line
static int logisave=0;          // number of nested .logical
static unsigned long* logitab;  //.logical .here
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
unsigned long curfnum=1;
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
        fprintf(stdout,"Error messages:    ");
        if (errors) fprintf(stdout,"%i\n",errors); else fprintf(stdout,"None\n");
        fprintf(stdout,"Warning messages:  ");
        if (warnings) fprintf(stdout,"%i\n",warnings); else fprintf(stdout,"None\n");
        fprintf(stdout,"Passes:            %i\n",pass);
        fprintf(stdout,"Range:             ");
        if (low_mem<=top_mem) fprintf(stdout,(full_mem==0xffff)?"$%04lx-$%04lx\n\n":"$%06lx-$%06lx\n\n",low_mem,top_mem);else fprintf(stdout,"None\n");
    }
    if (mem64) free(mem64);			// free codemem
    if (mmap) free(mmap);				// free memorymap

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
void readln(FILE* fle) {
    int i=0,i2=0,q=0,ch;

    for (;;) {
        ch=fgetc(fle);
        if (ch == EOF) {sline++;break;}
        if (!ch) continue;   //null?
        if (ch==13) {
            ch=fgetc(fle);
            if (ch == EOF) ch=10;
            if (ch!=10) {
                if (ch == ':' && (!arguments.oldops)) ch=32;
                ungetc(ch,fle);
                sline++;break;
            }
        }
        if (ch==10) {
            ch=fgetc(fle);
            if (ch != EOF) {
                if (ch == ':' && (!arguments.oldops)) ch=32;
                ungetc(ch,fle);
            }
            sline++;break;
        }
        llist[i2++]=ch;
        if (ch=='\'' && !(q & 5)) q^=2;
        if (ch=='"' && !(q & 6)) q^=1;
        if (!q) {
            if (ch==9) ch=32;
            if (ch==';') {ch=32;q=4;}
            if ((ch == ':') && (!arguments.oldops)) {
                i2--;
                if (i2) {
                    ungetc(ch,fle);
                    sline++;
                    break;
                }
                sline--;
                goto at;
            }
        }
        if (!(q & 4)) {
            pline[i]=ch;
            i++;
        }
    at:
        if (i2==sizeof(pline)-1) {sline++;pline[i]=0;err_msg(ERROR_LINE_TOO_LONG,NULL);}
    }
    llist[i2]=0;
    if (i)
        while (pline[i-1]==' ') i--;
    pline[i]=lpoint=foundopcode=0;
}

// ---------------------------------------------------------------------------
// Read a character in the current string encoding
int petascii(char quo) {
    char ch;

    if (!here()) {err_msg(ERROR______EXPECTED,"End of string"); return 256;}
    ch=get();
    if (ch==quo) {
        if (here()==quo) lpoint++; // handle 'it''s'
        else return 257; // end of string;
    }
    if (arguments.toascii) {
        if (ch>='A' && ch<='Z') ch+=0x80;
        else if (ch>='a' && ch<='z') ch-=0x20;
        else if (ch=='{') {
            char sym[0x10];
            int n = 0;
            while ((ch=get())!='}') {
                if (ch == 0 || ch == quo) {err_msg(ERROR______EXPECTED,"End of symbol");return 256;}
                sym[n]=ch;
                n++;
                if (n == 0x10) {err_msg(ERROR_CONSTNT_LARGE,NULL);return 256;}
            }
            sym[n] = 0;
            ch = petsymbolic(sym);
            if (!ch) {err_msg(ERROR______EXPECTED, "PETASCII symbol");return 256;}
        }
    }
    return encode(ch);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
void pokeb(unsigned char byte) 
{

    if (fixeddig)
    {
	if (arguments.nonlinear) mmap[address>>3]|=(1<<(address & 7));
	if (address<low_mem) low_mem=address;
	if (address>top_mem) top_mem=address;
	if (address<=full_mem) mem64[address] = byte ^ outputeor;
    }
    if (wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);wrapwarn=0;}
    if (wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);wrapwarn2=0;}
    address++;l_address++;l_address&=all_mem;
    if (address>all_mem) {
	if (fixeddig) wrapwarn=1;
	address=0;
    }
    if (fixeddig && scpumode) if (!(address & 0xffff) || !(l_address & 0xffff)) wrapwarn2=1;
}

// ---------------------------------------------------------------------------
int what(int *tempno) {
    char ch;
    int no;

    ignore();
    switch (ch=whatis[(int)here()]) {
    case WHAT_COMMAND:
	{
            lpoint++;
	    for (no=0; no<COMMANDS; no++)
	    {
		for (ch = 0; ch < (char)strlen(command[(int)no]); ch++) {
		    if (command[(int)no][(int)ch] != lowcase(pline[lpoint+ch])) {
                        break;
                    }
                }
		if (ch == (char)strlen(command[(int)no]))
		{
		    if (((ch=lowcase(pline[lpoint+strlen(command[(int)no])]))<'a' ||
			 ch>'z') && ch!='_') {
                        lpoint+=strlen(command[(int)no]);
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
	switch (lowcase(get()))
	{
	case 'y': return WHAT_Y;
	case 'x': ignore();if (get()==')') return WHAT_XZ; else {lpoint--; return WHAT_X;}
	case 's': ignore();if (get()==')') return WHAT_SZ; else {lpoint--; return WHAT_S;}
	default: lpoint--;return WHAT_COMA;
	}
    case WHAT_CHAR:
	if (!foundopcode) {
	    char s1,s2,s3,*p;
	    int also=0,felso,s4,elozo;

	    ch=lowcase(here());
	    no=(felso=last_mnem)/2;
	    if (((s1=lowcase(pline[lpoint+3]))==0x20 || !s1) &&
		(s2=lowcase(pline[lpoint+1])) && (s3=lowcase(pline[lpoint+2])))
		for (;;) {  // do binary search
		    if (!(s4=ch-*(p=mnemonic+no*3)))
			if (!(s4=s2-*(++p)))
			    if (!(s4=s3-*(++p)))
			    {
				lpoint+=3;
				*tempno=no;foundopcode=1;
				return WHAT_MNEMONIC;
			    }

		    elozo=no;
		    if (elozo==(no=((s4>0) ? (felso+(also=no)) : (also+(felso=no)) )/2)) break;
		}
	}
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
            val = petascii(ch);
            if (val < 256 && get()==ch) {v->type=T_INT;v->num=val;return 1;}
	    if (val != 256) err_msg(ERROR_EXPRES_SYNTAX,NULL);
	    return 0;
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
                if (tmp) {tmp->proclabel=0;tmp->used=1;*v=tmp->value;}
		return 1;
	    }
	    else {
                if (tmp) {
                    if ((tmp->requires & current_provides)!=tmp->requires) err_msg(ERROR_REQUIREMENTS_,ident);
                    if (tmp->conflicts & current_provides) err_msg(ERROR______CONFLICT,ident);
                    tmp->proclabel=0;tmp->used=1;*v=tmp->value;return 1;
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
        switch (ch)
        {
            case '(':return 0;
            case 'l':          // <
            case 'h':          // >
            case 'H':return 5; // `
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
        switch (ch)
        {
            case '(':return 0;
            case 'l':          // <
            case 'h':          // >
            case 'H':return 5; // `
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
    if ((ch=='n' || ch=='t' || ch=='i' || ch=='l' || ch=='h' || ch=='H') && ssp &&
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

void get_exp(int *wd, int *df,int *cd, struct svalue *v) {// length in bytes, defined
    struct svalue val;
    int i,nd=0,tp=0;
    char ch;

    ssp=esp=0;
    *wd=3;    // 0=byte 1=word 2=long 3=negative/too big
    *df=1;    // 1=result is ok, result is not yet known
    *cd=0;    // 0=error
    v->type = T_NONE;

    ignore();
    switch (pline[lpoint]) {
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
                    if (!get_num(db*2-1, &val)) return;
                    goto ide;
                }
                continue;
            case '-':
            ba2:ch=here();
                db++;
                if (!(ch>='0' && ch<='9') && ch!='$' && ch!='"' && ch!='\'' && ch!='%' && ch!='(' && ch!='_' && !(ch>='a' && ch<='z') && !(ch>='A' && ch<='Z')) {
                    if (ch=='-') {lpoint++;goto ba2;}
                    if (!get_num(db*2, &val)) return;
                    goto ide;
                }
                pushs('n');
                continue;
            case '!': pushs('t'); continue;
            case '~': pushs('i'); continue;
            case '<': pushs('l'); continue;
            case '>': pushs('h'); continue;
            case '`': pushs('H'); continue;
            }
	    lpoint--;
            if (!get_num(0, &val)) return;
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
		if (!ssp) {err_msg(ERROR_EXPRES_SYNTAX,NULL); return;}
		ssp--;
		continue;
	    }
	    while ((ssp) && (s_stack[ssp-1]!='('))
		e_stack[esp++].sgn=s_stack[--ssp];
	    if (!ssp) {
		if (tp==2) *cd=3; else *cd=1;
		break;
	    }
	    if (ssp>1) {err_msg(ERROR_EXPRES_SYNTAX,NULL); return;}
	    if (tp) *cd=2;
	    else {err_msg(ERROR_EXPRES_SYNTAX,NULL); return;}
	    break;
	}
    }
    vsp=0;
    for (i=0; i<esp; i++) {
	if ((ch=e_stack[i].sgn)==' ')
	    v_stack[vsp++]=e_stack[i].val;
        else if (v_stack[vsp-1].type == T_INT) {
            long val1 = v_stack[vsp-1].num;
            long val2;
            switch (ch) {
            case 'l': val1&=255; break;
            case 'h': val1=(val1 >> 8) & 255; break;
            case 'H': val1=(val1 >> 16) & 255; break;
            case 'n': val1=-val1; break;
            case 'i': val1=~val1; break;
            case 't': val1=!val1; break;
            default:
                if (v_stack[vsp-2].type != T_INT) {
                    if (v_stack[vsp-2].type == T_NONE) goto nothing;
                    err_msg(ERROR____WRONG_TYPE,NULL); return;
                }
                val2 = v_stack[vsp-2].num;
                switch (ch) {
                case '=': val1=(val2 == val1); vsp--; break;
                case 'o': val1=(val2 != val1); vsp--; break;
                case '<': val1=(val2 < val1); vsp--; break;
                case '>': val1=(val2 > val1); vsp--; break;
                case 'g': val1=(val2 >= val1); vsp--; break;
                case 's': val1=(val2 <= val1); vsp--; break;
                case '*': val1*=val2; vsp--; break;
                case '/': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); return;} else val1=val2 / val1; vsp--; break;
                case 'u': if (!val1) {err_msg(ERROR_DIVISION_BY_Z,NULL); return;} else val1=val2 % val1; vsp--; break;
                case '+': val1+=val2; vsp--; break;
                case '-': val1=val2 - val1; vsp--; break;
                case '&': val1&=val2; vsp--; break;
                case '|': val1|=val2; vsp--; break;
                case '^': val1^=val2; vsp--; break;
                case 'm': val1=val2 << val1; vsp--; break;
                case 'd': val1=val2 >> val1; vsp--; break;
                }
            }
            v_stack[vsp-1].num = val1;
	} else if (v_stack[vsp-1].type == T_NONE) {
        nothing:
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
            case 'd': vsp--; break;
            }
            v_stack[vsp-1].type = T_NONE;
        } else {err_msg(ERROR____WRONG_TYPE,NULL); return;}
    }
    if (v_stack[0].type == T_INT) {
        long val1 = v_stack[0].num;
	switch (*wd)
	{
	case 0:if (val1>0xff) val1=-1;break;
	case 1:if (val1>0xffff) val1=-1;break;
	case 2:if (val1>0xffffff) val1=-1;break;
	default:*v=v_stack[0];return;
	}
        if (val1>=0) {*v=v_stack[0];return;}
	err_msg(ERROR_CONSTNT_LARGE,NULL);
	*cd=0;
    } else *df = 0;
    return;
}

void wait_cmd(FILE* fil,int no)
{
    int wrap=waitforp;
    int pr,wh;
    long lin = 1,pos = 0;

    for (;;) {
	if (feof(fil)) {
	    char nc[20];
	    strcpy(nc,".");
	    strcat(nc,command[no]);
	    err_msg(ERROR______EXPECTED,nc);
	    return;
	}
	if (no==CMD_PEND) { //.pend
	    lin=sline;
	    pos=ftell(fil);
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
	    switch (pr)
	    {
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
	    case CMD_PROC:if (no==CMD_PEND && wrap==waitforp) {sline=lin;fseek(fil,pos,SEEK_SET);return;}break;// .proc
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
    if (pline[lpoint]=='\"') {lpoint++;q=0;}
    while (pline[lpoint] && (pline[lpoint]!='\"' || q) && i<80) path[i++]=get();
    if (i==80 || (!q && pline[lpoint]!='\"')) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
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
                while (mpr[pp]==32) pp++; //skip space
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
                while (mpr[pp]==32) pp++; //skip space
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
    all_mem=0x00ffff;scpumode=0;dtvmode=0;
    switch (last_mnem=cpumode) {
    case OPCODES_6502: mnemonic=MNEMONIC6502; opcode=c6502;break;
    case OPCODES_65C02:mnemonic=MNEMONIC65C02;opcode=c65c02;break;
    case OPCODES_6502i:mnemonic=MNEMONIC6502i;opcode=c6502i;break;
    case OPCODES_65816:mnemonic=MNEMONIC65816;opcode=c65816;all_mem=0xffffff;scpumode=1;break;
    case OPCODES_65DTV02:mnemonic=MNEMONIC65DTV02;opcode=c65dtv02;dtvmode=1;break;
    }
}

void compile(char* nam,long fpos,char tpe,char* mprm,int nprm,FILE* fin) // "",0
{
    char mparams[256];
    FILE* fil;
  
    int wht,w,d,c,i;
    int prm = 0;
    struct svalue val;

    char ch;

    long pos,lin = 1,cnt,oldpos=-1;

    char snum[12];

    struct slabel* tmp = NULL;
    struct smacro* tmp2 = NULL;
    struct scontext *old_context = NULL;

#ifndef WIN32
    int fflen;
    struct stat filestat;
#endif
    unsigned long backr_old = 0, forwr_old = 0, reffile_old = 0;

    if (tpe==0) {
        backr_old=current_context->backr;
        forwr_old=current_context->forwr;
        current_context->backr=current_context->forwr=1;
    }
    if (tpe==0 || tpe==1) reffile_old=reffile;
    if (fin) oldpos=ftell(fin);
    else {
        char volt;
	if ((fin=openfile(nam,&volt))==NULL) err_msg(ERROR_CANT_FINDFILE,nam);
	if (volt) {
	    if (tpe==1 || tpe==3) oldpos=ftell(fin);
	    else err_msg(ERROR_FILERECURSION,NULL);
        }
        if (tpe==1) reffile=reffile_old;
     }
    if (fseek(fin,fpos,SEEK_SET)) err_msg(ERROR_CANT_FINDFILE,"[Compile - fseek()]");
    for (;;) {
	if (feof(fin))
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
                    goto hh;
                } else if (here()=='+') {
                    lpoint++;if (here()!=0x20 && here()) goto baj;
                    prm=1;
                    sprintf(ident,"+%lu+%lu",reffile,current_context->forwr++);
                    goto hh;
                }
            baj:
                err_msg(ERROR_GENERL_SYNTAX,NULL);
                continue;
            } //not label
            get_ident('_');                                           //get label
        hh: strcpy(ident2,ident);
            if (!(skipit[waitforp] & 1)) {wht=what(&prm);goto jn;} //skip things if needed
            if ((wht=what(&prm))==WHAT_EQUAL) { //variable
                strcpy(varname,ident);
                get_exp(&w,&d,&c,&val); //ellenorizve.
                if (listing && flist && arguments.source) {
                    if (nprm>=0) mtranslate(mprm,nprm,llist);
                    if (val.type == T_INT) {
                        fprintf(flist,(all_mem==0xffff)?"=%04lx\t\t\t\t\t%s\n":"=%06lx\t\t\t\t\t%s\n",val.num,llist);
                    }
                }
		if (!c) continue;
		if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); continue;}
		ignore();if (here()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL); continue;}
		tmp=new_label(varname);
		if (pass==1) {
		    if (labelexists) {
			err_msg(ERROR_DOUBLE_DEFINE,varname);
			continue;
		    }
                    else {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
			tmp->proclabel=0;tmp->used=0;
			tmp->value=val;
		    }
		}
		else {
                    if (labelexists) {
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
                        switch (tmp->value.type) {
                        case T_INT:
                            if (val.type != T_INT || tmp->value.num!=val.num) {
                                tmp->value=val;
                                fixeddig=0;
                            }
                            break;
                        case T_STR:
                            if (val.type != T_STR || strcmp(tmp->value.str, val.str)) {
                                free(tmp->value.str);
                                tmp->value=val;
                                fixeddig=0;
                            }
                            break;
                        case T_NONE:
                            if (val.type != T_NONE) fixeddig=0;
                            tmp->value=val;
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
		    tmp2->point=ftell(fin);
		    tmp2->lin=sline;
                    tmp2->type=prm;
		}
		wait_cmd(fin,CMD_ENDM); //.endm
		continue;
	    }
            if ((tmp2=find_macro(ident))) {lpoint--;goto as_macro;}
	    if (listing && flist && arguments.source &&
                ((wht==WHAT_COMMAND && prm>CMD_LONG && prm!=CMD_PROC) || wht==WHAT_STAR || wht==WHAT_EOL || wht==WHAT_HASHMARK || wht==WHAT_EXPRESSION)) {
                if (lastl==2) {fputc('\n',flist);lastl=1;}
                if (ident[0]!='-' && ident[0]!='+')
                    fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident);
                else
                    fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%c\n":".%06lx\t\t\t\t\t%c\n",address,ident[0]);
	    }
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
                    tmp->used=0;
		    tmp->value.type=T_INT;tmp->value.num=l_address;
		    if (wht==WHAT_COMMAND && prm==CMD_PROC) tmp->proclabel=1; else tmp->proclabel=0;
		}
	    }
	    else {
                if (labelexists) {
                    tmp->requires=current_requires;
                    tmp->conflicts=current_conflicts;
                    if (tmp->value.type != T_INT || (unsigned long)tmp->value.num != l_address) {
                        if (tmp->value.type == T_STR) free(tmp->value.str);
                        tmp->value.type=T_INT;tmp->value.num=l_address;
                        fixeddig=0;
                    }
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
		get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		if (!c) break;
		if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                ignore();if (here()) goto extrachar;
                if (val.type != T_NONE) {
                    if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                    ch2=val.num;
                    if (ch2>all_mem) {
                        err_msg(ERROR_CONSTNT_LARGE,NULL); 
                        break;
                    }
                    address=l_address=ch2;
                }
		lastl=0;
		break;
	    }
	case WHAT_EOL: break;
	case WHAT_COMMAND:
	    {
		char lcol = 0,kiirva = 0; //for listing
                ignore();
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
			get_exp(&w,&d,&c,&val); //ellenorizve.
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			ignore();if (here()) goto extrachar;
                	if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for condition");wait_cmd(fin,CMD_FI);break;}
		    } else val.type=T_NONE;
                    waitfor[++waitforp]='e';
                    switch (prm) {
                    case CMD_ELSIF:
                        waitforp--;
                        if ((val.type == T_INT && val.num) || (val.type == T_STR && val.str[0])) skipit[waitforp]=skipit[waitforp] >> 1; else
                            skipit[waitforp]=skipit[waitforp] & 2;
                        break;
                    case CMD_IF:
                        if ((val.type == T_INT && val.num) || (val.type == T_STR && val.str[0])) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFEQ:
                        if ((val.type == T_INT && !val.num) || (val.type == T_STR && !val.str[0])) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFPL:
                        if ((val.type == T_INT && val.num>=0) || (val.type == T_STR && val.str[0])) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    case CMD_IFMI:
                        if (val.type == T_INT && val.num<0) skipit[waitforp]=skipit[waitforp-1] & 1; else
                            skipit[waitforp]=(skipit[waitforp-1] & 1) << 1;
                        break;
                    }
		    break;
		}
                if (!(skipit[waitforp] & 1)) break; //skip things if needed
                if (prm<CMD_RTA) {    // .byte .text .ptext .char .shift .shift2 .null
                    int ch2=-1, ch3, oldlpoint;
                    char quo;
                    unsigned long ptextaddr=address;
                    if (prm==CMD_PTEXT) pokeb(0);
                    for (;;) {

                        ignore();
                        ch=here();

                        if (ch=='"' || ch=='\'') {
                            quo=ch;
                            oldlpoint=lpoint;
                            lpoint++;
                            ch3 = petascii(quo);
			    if (ch3 < 256 && here()==quo) {lpoint = oldlpoint;goto textconst;}
                            /* handle the string in quotes */
			    for (;;ch3 = petascii(quo)) {
                                if (ch3 >= 256) break;

                                if (ch2>=0) {
                                    pokeb(ch2);
                                }
                                ch2 = ch3;

                                if (prm==CMD_CHAR) {if (ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}}
                                else if (prm==CMD_SHIFT || prm==CMD_SHIFT2) {
                                    if (encoding==1 && ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
                                    if (ch2>=0xc0 && ch2<0xe0) ch2-=0x60; else
                                        if (ch2==0xff) ch2=0x7e; else
                                            if (ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
            			    if (prm==CMD_SHIFT2) ch2<<=1;
                                } else
                                    if (prm==CMD_NULL && !ch2) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
                            }
                            goto cvege;
			}
                        /* if ^ infront of number, convert decimal value to string */
			if (ch=='^') {
                            lpoint++;
			    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
			    if (!c) break;
			    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}

			    if (val.type == T_INT) sprintf(snum,"%ld",val.num);
                            else if (val.type == T_NONE) snum[0]=0;
                            else {err_msg(ERROR____WRONG_TYPE,NULL); break;}

                            for(i=0; snum[i]; i++) {
                                if (ch2>=0) {
                                    pokeb(ch2);
                                }
                                ch2=encode(snum[i]);
                                if (prm==CMD_CHAR) {if (ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}}
                                else if (prm==CMD_SHIFT || prm==CMD_SHIFT2) {
                                    if (encoding==1 && ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
                                    if (ch2>=0xc0 && ch2<0xe0) ch2-=0x60; else
                                        if (ch2==0xff) ch2=0x7e; else
                                            if (ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
				    if (prm==CMD_SHIFT2) ch2<<=1;
                                }
                            }
                            goto cvege;
			}
		    textconst:
			get_exp(&w,&d,&c,&val); //ellenorizve.
			if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            if (prm==CMD_CHAR) {
                                if (val.num>0x7f || val.num<-0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            } else {
                                if (val.num>0xff || val.num<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            }
                        }
                        if (ch2>=0) {
                            pokeb(ch2);
                        }
                        ch2 = (val.type == T_NONE) ? 0 : (unsigned char)val.num;
                        if (prm==CMD_SHIFT || prm==CMD_SHIFT2) {
                            if (encoding==1 && ch2>=0x80) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto cvege2;}
                            if (ch2>=0xc0 && ch2<0xe0) ch2-=0x60; else
                                if (ch2==0xff) ch2=0x7e; else
                                    if (ch2>=0x80 && d) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
			    if (prm==CMD_SHIFT2) ch2<<=1;
                        } else
                            if (prm==CMD_NULL && !ch2 && d) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                    cvege:
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
                            if ((address>ptextaddr && address-ptextaddr>0x100) ||
                                (address<ptextaddr && address+all_mem-ptextaddr>0xff)) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}

                            mem64[ptextaddr]=(address-ptextaddr-1) & 0xff;
                        }
                    cvege2:
                        if (listing && flist) {
                            if (lastl!=2) {fputc('\n',flist);lastl=2;}
                            fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",ptextaddr);
                            lcol=25;
                            kiirva=1;
                            while (ptextaddr!=address) {
                                ch2=mem64[ptextaddr];
                                ptextaddr=(ptextaddr+1) & all_mem;
                                if (lcol==1) {
                                    if (arguments.source && kiirva) {
                                        if (nprm>=0) mtranslate(mprm,nprm,llist);
                                        fprintf(flist,"\t%s\n",llist);kiirva=0;
                                    } else fputc('\n',flist);
                                    fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",ptextaddr);lcol=25;
                                }
                                fprintf(flist,"%02x ",(unsigned char)ch2);
    
                                lcol-=3;
                            }
                        }
			break;
		    }
		}
		if (prm==CMD_WORD || prm==CMD_INT || prm==CMD_RTA) { // .word .int .rta
                    long ch2;
		    if (listing && flist) {
			if (lastl!=2) {fputc('\n',flist);lastl=2;}
                        fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",address);
			lcol=25;
                        kiirva=1;
		    }
		    for (;;) {
			get_exp(&w,&d,&c,&val); //ellenorizve.
			if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            ch2 = val.num;
                            if (prm==CMD_INT) {
                                if (ch2>0x7fff || ch2<-0x8000) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            } else {
                                if (ch2>0xffff || ch2<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                                if (prm==CMD_RTA)
                                    ch2=(ch2-1) & 0xffff;
                            }
                        } else ch2 = 0;
			if (listing && flist) {
			    if (lcol==1) {
                                if (arguments.source && kiirva) {
                                    if (nprm>=0) mtranslate(mprm,nprm,llist);
                                    fprintf(flist,"\t%s\n",llist);kiirva=0;
                                } else fputc('\n',flist);
                                fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",address);lcol=25;
			    }
			    fprintf(flist,"%02x %02x ",(unsigned char)ch2,(unsigned char)(ch2>>8));
			    lcol-=6;
			}
			pokeb((unsigned char)ch2);
			pokeb((unsigned char)(ch2>>8));
			ignore();if ((ch=get())==',') continue;
			if (ch) err_msg(ERROR______EXPECTED,",");
			break;
		    }
		}
		if (prm==CMD_LONG) { // .long
                    long ch2;
		    if (listing && flist) {
			if (lastl!=2) {fputc('\n',flist);lastl=2;}
                        fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",address);
			lcol=25;
                        kiirva=1;
		    }
		    for (;;) {
			get_exp(&w,&d,&c,&val); //ellenorizve.
			if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            ch2=val.num;
                            if (ch2>0xffffff || ch2<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        } else ch2 = 0;
			if (listing && flist) {
			    if (lcol==1) {
                                if (arguments.source && kiirva) {
                                    if (nprm>=0) mtranslate(mprm,nprm,llist);
                                    fprintf(flist,"\t%s\n",llist);kiirva=0;
                                } else fputc('\n',flist);
                                fprintf(flist,(all_mem==0xffff)?">%04lx\t ":">%06lx  ",address);lcol=25;
			    }
			    fprintf(flist,"%02x %02x %02x ",(unsigned char)ch2,(unsigned char)(ch2>>8),(unsigned char)(ch2>>16));
			    lcol-=9;
			    if (lcol==7) {fprintf(flist,"\t");lcol=1;}
			}
			pokeb((unsigned char)ch2);
			pokeb((unsigned char)(ch2>>8));
			pokeb((unsigned char)(ch2>>16));
			ignore();if ((ch=get())==',') continue;
			if (ch) err_msg(ERROR______EXPECTED,",");
			break;
		    }
		}
                if (prm<=CMD_LONG) { // .byte .text .rta .char .int .word .long
		    if (listing && flist)
		    {
			if (arguments.source && kiirva) {
                            for (i=0; i<lcol-2; i+=8) fputc('\t',flist);
                            if (nprm>=0) mtranslate(mprm,nprm,llist);
			    fprintf(flist,"\t%s\n",llist);
			} else fputc('\n',flist);
		    }
		    break;
		}
		if (prm==CMD_OFFS) {   // .offs
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0; //ellenorizve.
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        address+=val.num;
                        if (address>all_mem) {
                            if (fixeddig) err_msg(ERROR_TOP_OF_MEMORY,NULL);
                            address&=all_mem;
                        }
                    }
		    break;
		}
		if (prm==CMD_LOGICAL) { // .logical
                    unsigned long ch2;
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        ch2=val.num;
                        if (ch2>all_mem) {
                            err_msg(ERROR_CONSTNT_LARGE,NULL); 
                            break;
                        }
                    } else ch2=l_address;
		    if (!(logitab=realloc(logitab,(logisave+1)*sizeof(*logitab)))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
		    logitab[logisave++]=l_address-address;
                    l_address=ch2;
		    break;
		}
		if (prm==CMD_HERE) { // .here
		    if (here()) goto extrachar;
		    if (!logisave) {err_msg(ERROR______EXPECTED,".LOGICAL"); break;}
		    l_address=address+logitab[--logisave];
		    logitab=realloc(logitab,logisave*sizeof(*logitab));
		    break;
		}
		if (prm==CMD_AS) { // .as
		    if (here()) goto extrachar;
                    if (scpumode) longaccu=0;
		    break;
		}
		if (prm==CMD_AL) { // .al
		    if (here()) goto extrachar;
                    if (scpumode) longaccu=1;
		    break;
		}
		if (prm==CMD_XS) { // .xs
		    if (here()) goto extrachar;
		    if (scpumode) longindex=0;
		    break;
		}
		if (prm==CMD_XL) { // .xl
		    if (here()) goto extrachar;
		    if (scpumode) longindex=1;
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
		            tmp->used=!tmp->proclabel;tmp->proclabel=1;
                            current_context=new_context(ident, current_context);
                            current_context->backr=current_context->forwr=1;
                            if (listing && flist && arguments.source) {
                                if (lastl==2) {fputc('\n',flist);lastl=1;}
                                if (ident[0]!='-' && ident[0]!='+')
                                    fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident);
                                else
                                    fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%c\n":".%06lx\t\t\t\t\t%c\n",address,ident[0]);
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
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        if (scpumode) databank=val.num;
                    }
		    break;
		}
		if (prm==CMD_DPAGE) { // .dpage
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        if (val_length(val.num)>1) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        if (dtvmode) dpage=val.num & 0xff00;
                        else dpage=val.num;
                    }
		    break;
		}
		if (prm==CMD_FILL) { // .fill
                    unsigned long db = 0;
                    long ch;
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        db=val.num;
                        if (db>all_mem) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}
                    }
                    if ((ch=get())) {
                        if (ch!=',') goto extrachar;
                        get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        ignore();if (here()) goto extrachar;
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            ch = val.num;
                        } else ch = 0;
                        while (db-->0) pokeb((unsigned char)ch);
                    } else {
                        l_address+=db;l_address&=all_mem;
                        address+=db;
                        if (address>all_mem) {
                            if (fixeddig) err_msg(ERROR_TOP_OF_MEMORY,NULL);
                            address&=all_mem;
                        }
                        if (fixeddig && scpumode) if (!(address & 0xffff) || !(l_address & 0xffff)) err_msg(ERROR___BANK_BORDER,NULL);
                    }
		    break;
		}
		if (prm==CMD_ASSERT) { // .assert
		    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        current_provides=val.num;
                    } else current_provides=~0;
		    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        current_requires=val.num;
                    } else current_requires=0;
		    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        current_conflicts=val.num;
                    } else current_conflicts=0;
		    break;
		}
		if (prm==CMD_CHECK) { // .check
		    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (get()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        if ((val.num & current_provides)!=(unsigned long)val.num) {err_msg(ERROR_REQUIREMENTS_,".CHECK");break;}
                    }
		    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                    if (full_mem<all_mem) {
                        if (!(mem64=realloc(mem64,all_mem+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
                        if (arguments.nonlinear) {
                            if (!(mmap=realloc(mmap,(all_mem+1) >> 3))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
                            memset(mmap+full_mem,0,(all_mem-full_mem) >> 3);
                        }
                        full_mem=all_mem;
                    }
		    break;
		}
                if (prm==CMD_CERROR || prm==CMD_CWARN) { // .cerror
		    get_exp(&w,&d,&c,&val); //ellenorizve.
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (here()==',') {
                        lpoint++;ignore();
                    } else if (here()) goto extrachar;
                    if ((val.type == T_INT && val.num) || (val.type == T_STR && val.str[0])) err_msg((prm==CMD_CERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED,&pline[lpoint]);
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
		    get_exp(&w,&d,&c,&val);if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for count");wait_cmd(fin,CMD_NEXT);break;}
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        lin=sline;
                        pos=ftell(fin);
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
		    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        if (val.num<1 || val.num>(long)all_mem) {
                            err_msg(ERROR_CONSTNT_LARGE,NULL); 
                            break;
                        }
                        align = val.num;
                    } else align = 1;
                    if ((ch=get())) {
                        int d2;
                        if (ch!=',') goto extrachar;
                        get_exp(&w,&d2,&c,&val);if (!d2) fixeddig=0;
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        ignore();if (here()) goto extrachar;
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            if (val_length(val.num)) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                            fill = val.num;
                        } else fill = 0;
                    }
                    if (align>1 && (l_address % align)) {
                        if (fill>0)
                            while (l_address % align) pokeb((unsigned char)fill);
                        else {
                            align-=l_address % align;
                            l_address+=align;l_address&=all_mem;
                            address+=align;
                            if (address>all_mem) {
                                if (fixeddig) err_msg(ERROR_TOP_OF_MEMORY,NULL);
                                address&=all_mem;
                            }
                            if (fixeddig && scpumode) if (!(address & 0xffff) || !(l_address & 0xffff)) err_msg(ERROR___BANK_BORDER,NULL);
                        }
                    }
		    break;
		}
                if (prm==CMD_EOR) {   // .eor
                    get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
                    if (!c) break;
                    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                    ignore();if (here()) goto extrachar;
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                        if (arguments.source) {
                            if (nprm>=0) mtranslate(mprm,nprm,llist);
                            fprintf(flist,"\t\t\t\t\t%s\n",llist);
                        }
                        fprintf(flist,"\n;******  Processing file \"%s\"\n\n",path);
                    }
                    compile(path,0,0,mprm,nprm,NULL);
                    exitfile();
                    if (listing && flist) fprintf(flist,"\n;******  Return to file \"%s\"\n\n",filenamelist->name);
		    sline=lin;
		    break;
		}
                if (prm==CMD_BINARY) { // .binary
                    long foffset=0,fsize=all_mem+1;
                    if (get_path()) break;
                    if ((ch=get())) {
                        if (ch!=',') goto extrachar;
                        get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
                        if (!c) break;
                        if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                        ignore();
                        if (val.type != T_NONE) {
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                            foffset = val.num;
                            if (foffset<0) {err_msg(ERROR_CONSTNT_LARGE,NULL); break;}
                        }
                        if ((ch=get())) {
                            if (ch!=',') goto extrachar;
                            get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
                            if (!c) break;
                            if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
                            ignore();if (here()) goto extrachar;
                            if (val.type != T_NONE) {
                                if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                                fsize = val.num;
                                if (fsize<0 || fsize>(long)all_mem) {err_msg(ERROR_CONSTNT_LARGE,NULL);break;}
                            }
                        }
                    }
#ifndef WIN32
		    if (!fixeddig) {
			if (stat(path,&filestat)) {err_msg(ERROR_CANT_FINDFILE,path);break;}
			if (!filestat.st_ino) {err_msg(ERROR_CANT_FINDFILE,path);break;}
                        fflen=filestat.st_size;
                        if (fflen>foffset) fflen-=foffset; else fflen=0;
			address+=(unsigned long)fflen;
			l_address+=(unsigned long)fflen;
			break;
                    }
#endif
                    if ((fil=fopen(path,"rb"))==NULL) {err_msg(ERROR_CANT_FINDFILE,path);break;}
                    fseek(fil,foffset,SEEK_SET);
		    lcol=0;
                    if (listing && flist) {
                        if (arguments.source) {
                            if (nprm>=0) mtranslate(mprm,nprm,llist);
                            fprintf(flist,"\t\t\t\t\t%s\n",llist);
                        }
                        fprintf(flist,"\n;******  Binary include \"%s\"\n",path);
                    }
		    for (;fsize;fsize--) {
			int st=fgetc(fil);
			if (st == EOF) break;
			if (st < 0) err_msg(ERROR_CANT_FINDFILE,path);
			if (listing && flist) {
                            if (!lcol) {fprintf(flist,(all_mem==0xffff)?"\n>%04lx\t ":"\n>%06lx  ",address);lcol=16;}
			    fprintf(flist,"%02x ",(unsigned char)st);
			    lcol--;
			}
			pokeb(st);
		    }
		    if (listing && flist) fputc('\n',flist);
		    fclose(fil);
		    break;
		}
		if (prm==CMD_FOR) { // .for
		    int apoint=0;
		    char expr[linelength];

		    if ((wht=what(&prm))==WHAT_EXPRESSION && prm==1) { //label
			if (get_ident('_')) break;
			ignore();if (get()!='=') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
			strcpy(varname,ident);
			get_exp(&w,&d,&c,&val);
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for start");break;}
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                        tmp=new_label(varname);
                        tmp->requires=current_requires;
                        tmp->conflicts=current_conflicts;
			if (!labelexists) tmp->proclabel=0;
			tmp->value=val;
                        tmp->used=0;
			wht=what(&prm);
		    }
		    if (wht==WHAT_S || wht==WHAT_Y || wht==WHAT_X) lpoint--; else
			if (wht!=WHAT_COMA) {err_msg(ERROR______EXPECTED,","); break;}

		    strcpy(expr,&pline[lpoint]);
		    strcpy(pline,expr);
		    lin=sline;
                    pos=ftell(fin);
                    enterfile(nam,lin);
		    for (;;) {
			lpoint=0;
			get_exp(&w,&d,&c,&val);
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
			if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used in condition");break;}
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
			if (!val.num) break;
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
			    tmp->used=0;
                        }
			compile(nam,pos,2,mprm,nprm,fin);
			sline=lin;
			strcpy(pline,expr);
			lpoint=apoint;
			get_exp(&w,&d,&c,&val);
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
                    get_exp(&w,&d,&c,&val);
		    if (!c) break;
		    if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}
		    ignore();if (here()) goto extrachar;
                    if (!d) {err_msg(ERROR___NOT_DEFINED,"argument used for option");break;}
		    if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                    if (!strcasecmp(ident,"allow_branch_across_page")) allowslowbranch=!!val.num;
                    else if (!strcasecmp(ident,"auto_longbranch_as_jmp")) longbranchasjmp=!!val.num;
                    else err_msg(ERROR_UNKNOWN_OPTIO,ident);
		    break;
		}
	    }
	case WHAT_HASHMARK:if (skipit[waitforp] & 1) //skip things if needed
	    {                   //macro stuff
		int ppoint;
                if (get_ident2('_')) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                if (!(tmp2=find_macro(ident))) {err_msg(ERROR___NOT_DEFINED,ident); break;}
            as_macro:
		ppoint=nprm=0;
                ignore();
		while ((ch=get())) {
                    /* if ^ infront of number, convert decimal value to string */
		    if (ch=='^') {
			get_exp(&w,&d,&c,&val);if (!d) fixeddig=0;
			if (!c) break;
			if (c==2) {err_msg(ERROR_EXPRES_SYNTAX,NULL); break;}

			if (val.type == T_INT) sprintf(snum,"%ld",val.num);
                        else if (val.type == T_NONE) snum[0]=0;
                        else {err_msg(ERROR____WRONG_TYPE,NULL); break;}

			i=0;while (snum[i]) mparams[ppoint++]=snum[i++];
		    }
                    else if (ch=='"' || ch=='\'') {
                        char quo=ch;
			for (;;) {
			    if (!(ch=get())) {err_msg(ERROR______EXPECTED,"End of string"); break;}
			    if (ch==quo) {
				if (pline[lpoint]!=quo) break;
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
                    enterfile(tmp2->file,lin);
                    if (strcmp(tmp2->file,nam)) compile(tmp2->file,tmp2->point,1,mparams,nprm,NULL);
                    else compile(tmp2->file,tmp2->point,3,mparams,nprm,fin);
                    exitfile();
                } else err_msg(ERROR__MACRECURSION,"!!!!");
                if (tmp2->type==CMD_MACRO) current_context = old_context;
                macrecursion--;
                sline=lin;
		break;
	    }
	case WHAT_MNEMONIC:if (skipit[waitforp] & 1) {//skip things if needed
            int opr = 0, mnem = prm;
            int oldlpoint = lpoint;
	    const unsigned char* cnmemonic = &opcode[prm*24]; //current nmemonic
	    char ln = 0;
	    unsigned char cod = 0, longbranch = 0;
            unsigned long adr = 0;

            ignore();
	    if (!(wht=here())) {
		cod=cnmemonic[ADR_IMPLIED];
		opr=ADR_IMPLIED;w=ln=0;d=1;
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
		get_exp(&w,&d,&c,&val); //ellenorizve.
		if (!c) break;
		if (c==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}

		ln=1;
		if ((cod=cnmemonic[ADR_IMMEDIATE])==0xE0 || cod==0xC0 || cod==0xA2 || cod==0xA0) {// cpx cpy ldx ldy
		    if (longindex) ln++;
		}
		else if (cod==0xF4) ln=2; //pea #$ffff
		else if (cod!=0xC2 && cod!=0xE2) {//not sep rep=all accu
		    if (longaccu) ln++;
                }
                if (dtvmode && cod==0x02) longbranch=0x40;//hack

		if (val.type != T_NONE) {
		    if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
                    adr=val.num;
		    if (w==3) w=val_length(adr);//auto length
		    if (w>=ln) w=3; //const too large
		    opr=ADR_IMMEDIATE;// lda #
		} else fixeddig=0;
	    }
	    // 3 Db
	    else if (wht=='[') {
                lpoint++;
		get_exp(&w,&d,&c,&val); //ellenorizve.
		if (!c) break;
		if (c==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
		ignore();if (get()!=']') {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
		if ((wht=what(&prm))==WHAT_Y) {
                    if (val.type != T_NONE) {
                        if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                            if (val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
		get_exp(&w,&d,&c,&val);if (!d) fixeddig=0; //ellenorizve.
		if (!c) break;
                if (val.type != T_NONE) {
                    if(val.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                        } else if (!w && adr>=dpage && adr<(dpage+0x100)) adr-=dpage;
			opr=ADR_ZP_X-w;ln=w+1;
		    }// 6 Db
		    else if (wht==WHAT_Y) {// lda $ff,y lda $ffff,y lda $ffffff,y
                        if (cnmemonic[ADR_REL]!=____) {lpoint--;goto megint;}
                        if (w==3) {//auto length
                            if (val.type != T_NONE) {
				if (cnmemonic[ADR_ZP_Y]!=____ && adr>=dpage && adr<(dpage+0x100)) {adr-=dpage;w=0;}
				else if (databank==(adr >> 16)) w=1;
                            } else w=(cnmemonic[ADR_ADDR_Y]!=____);
                        } else if (!w && adr>=dpage && adr<(dpage+0x100)) adr-=dpage;
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
                        get_exp(&w2,&d,&c2,&val2);if (!d) fixeddig=0;
                        if (!c2) break;
                        if (c2==2) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        if (val2.type != T_NONE && val2.type != T_INT) {err_msg(ERROR____WRONG_TYPE,NULL); break;}
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
                        if (listing && flist && arguments.source && ident2[0]) {
                            if (lastl==2) {fputc('\n',flist);lastl=1;}
                            if (ident2[0]!='-' && ident2[0]!='+')
                                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%s\n":".%06lx\t\t\t\t\t%s\n",address,ident2);
                            else
                                fprintf(flist,(all_mem==0xffff)?".%04lx\t\t\t\t\t%c\n":".%06lx\t\t\t\t\t%c\n",address,ident2[0]);
                        }
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

		if (lastl!=1) {fputc('\n',flist);lastl=1;}
                fprintf(flist,(all_mem==0xffff)?".%04lx\t %02x":".%06lx  %02x",address-ln-1,(unsigned char)(cod ^ longbranch));

                for (i=0;i<ln;i++) {fprintf(flist," %02x",(unsigned char)temp);temp>>=8;}
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
        case WHAT_EXPRESSION:
            if (skipit[waitforp] & 1) {
                get_ident2('_');
                if ((tmp2=find_macro(ident))) goto as_macro;
            }
            // fall through
	default: if (skipit[waitforp] & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); //skip things if needed
	}
    }
end:
    if (oldpos==-1) closefile(fin); else fseek(fin,oldpos,SEEK_SET);
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

    {
        unsigned char i=0;
        do {
            tolower_tab[i]=(i>='A' && i<='Z')?i+0x20:i;
            i++;
        } while (i);
    }

    tinit();
    testarg(argc,argv);

    if (arguments.quiet)
    fprintf(stdout,"6502/65C02 Turbo Assembler Version 1.3  Copyright (c) 1997 Taboo Productions\n"
                   "6502/65C02 Turbo Assembler Version 1.35 ANSI C port by [BiGFooT/BReeZe^2000]\n"
                   "6502/65C02/65816/DTV TASM Version 1.46 Fixing by Soci/Singular 2001-2011\n"
                   "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
                   "are welcome to redistribute it under certain conditions; See LICENSE!\n");
    
    if (arguments.cpumode==OPCODES_65816) all_mem=0xffffff;
    else all_mem=0x00ffff;
    full_mem=all_mem;

    if (arguments.quiet) fprintf(stdout,"\nAssembling file:   %s\n",arguments.input);

    if (!(mem64=malloc(all_mem+1))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
    if (arguments.nonlinear) if (!(mmap=malloc((all_mem+1) >> 3))) err_msg(ERROR_OUT_OF_MEMORY,NULL);
//   memset(mem64,0,all_mem+1);
    if (arguments.nonlinear) memset(mmap,0,(all_mem+1) >> 3);

    /* assemble the input file(s) */
    do {
        if (pass++>20) {fprintf(stderr,"Ooops! Too many passes...\n");exit(1);}
        set_cpumode(arguments.cpumode);
	address=l_address=databank=dpage=longaccu=longindex=0;low_mem=full_mem;top_mem=0;encoding=0;wrapwarn=0;wrapwarn2=0;
        current_provides=0xffffffff;current_requires=0;current_conflicts=0;macrecursion=0;allowslowbranch=1;
        fixeddig=1;waitfor[waitforp=0]=0;skipit[0]=1;sline=0;conderrors=warnings=0;freeerrorlist(0);outputeor=0;
        current_context=&root_context;
        /*	listing=1;flist=stderr;*/
        enterfile(arguments.input,0);
        sline=0;
        compile(arguments.input,0,0,"",-1,NULL);
        exitfile();
        if (errors) {status();return 1;}
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
	fprintf(flist,"\n;6502/65C02/65816/DTV Turbo Assembler V1.46 listing file of \"%s\"\n",arguments.input);
	time(&t);
        fprintf(flist,";done on %s\n",ctime(&t));

        pass++;
        set_cpumode(arguments.cpumode);
	address=l_address=databank=dpage=longaccu=longindex=0;low_mem=full_mem;top_mem=0;encoding=0;wrapwarn=0;wrapwarn2=0;
        current_provides=0xffffffff;current_requires=0;current_conflicts=0;macrecursion=0;allowslowbranch=1;
        fixeddig=1;waitfor[waitforp=0]=0;skipit[0]=1;sline=0;conderrors=warnings=0;freeerrorlist(0);outputeor=0;
        current_context=&root_context;
        enterfile(arguments.input,0);
        sline=0;
        compile(arguments.input,0,0,"",-1,NULL);
        exitfile();

	fprintf(flist,"\n;******  end of code\n");
	if (flist != stdout) fclose(flist);
    }

    set_cpumode(arguments.cpumode);

    if (arguments.label) labelprint();

    if (errors || conderrors) {status();return 1;}

    /* output file */
    if (low_mem<=top_mem) {
        if (arguments.output[0] == '-' && !arguments.output[1]) {
            fout = stdout;
        } else {
            if ((fout=fopen(arguments.output,"wb"))==NULL) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
        }
	if (arguments.nonlinear) {
	    unsigned long bl_adr=low_mem, bl_len;
	    while (bl_adr<=top_mem) {
		while (bl_adr<full_mem && !(mmap[(bl_adr)>>3] & (1<<((bl_adr) & 7)))) bl_adr++;
		bl_len=bl_adr;
		while (bl_len<full_mem && (mmap[(bl_len)>>3] & (1<<((bl_len) & 7)))) bl_len++;
		bl_len-=bl_adr;
		fputc(bl_len,fout);
		fputc(bl_len >> 8,fout);
		if (scpumode) fputc(bl_len >> 16,fout);
		fputc(bl_adr,fout);
		fputc(bl_adr >> 8,fout);
		if (scpumode) fputc(bl_adr >> 16,fout);
		if (fwrite(mem64+bl_adr,bl_len,1,fout)==0) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
		bl_adr+=bl_len;
	    }
	    bl_len=0;
	    if ((fwrite(&bl_len,2+scpumode,1,fout) == 0)) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
	}
	else {
	    if (!arguments.stripstart) {
		fputc(low_mem,fout);
		fputc(low_mem >> 8,fout);
		if (scpumode && arguments.wordstart) fputc(low_mem >> 16,fout);
	    }
	    if (fwrite(mem64+low_mem,top_mem-low_mem+1,1,fout)==0) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
	}
	if (fout != stdout && fclose(fout)) err_msg(ERROR_CANT_WRTE_OBJ,arguments.output);
	status();
	return 0;
    }
    return 0;
}
