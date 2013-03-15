/*
    Turbo Assembler 6502/65C02/65816/DTV
    Copyright (C) <year>  <name of author>

   6502/65C02 Turbo Assembler  Version 1.3
   (c)1996 Taboo Productions, Marek Matula

   6502/65C02 Turbo Assembler  Version 1.35  ANSI C port
   (c)2000 BiGFooT/BReeZe^2000

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
#ifdef _WIN32
#include <windows.h>
#include <wincon.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <time.h>
#include <errno.h>

#include "opcodes.h"
#include "misc.h"
#include "eval.h"
#include "error.h"
#include "section.h"
#include "encoding.h"
#include "file.h"
#include "values.h"
#include "variables.h"
#include "mem.h"

static const uint32_t *mnemonic;    //mnemonics
static const uint8_t *opcode;    //opcodes
static struct value_s none_value = {T_NONE, 0, {}};
static struct value_s new_value = {T_NONE, 0, {}};

#define nestinglevel 256
static int wrapwarn=0, wrapwarn2=0;
line_t sline, vline;      //current line
static address_t all_mem, all_mem2;
uint8_t pass=0;      //pass
static int listing=0;   //listing
address_t star=0;
const uint8_t *pline, *llist;  //current line data
linepos_t lpoint;              //position in current line
static char path[linelength];   //path
static FILE* flist = NULL;      //listfile
static enum lastl_e lastl;
static int longaccu=0,longindex=0,scpumode=0,dtvmode=0;
static uint8_t databank=0;
static uint16_t dpage=0;
int fixeddig;
static int allowslowbranch=1;
static int longbranchasjmp=0;
static uint8_t outputeor = 0; // EOR value for final output (usually 0, except changed by .eor)

static struct {
    char what;
    line_t line;
    linepos_t epoint;
    address_t addr;
    address_t laddr;
    struct label_s *label;
    struct section_s *section;
    uint8_t skip;
} waitfor[nestinglevel];

static uint8_t waitforp=0;

static unsigned int last_mnem;

int labelexists;
uint16_t reffile;
uint32_t backr, forwr;
struct file_s *cfile;
struct avltree *star_tree = NULL;

struct {
    uint8_t p, len;
    struct {
        size_t len, size;
        struct {
            size_t len;
            const uint8_t *data;
        } *param, all;
        uint8_t *pline;
    } *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

static const char* command[]={ /* must be sorted, first char is the ID */
    "\x21" "al",
    "\x33" "align",
    "\x20" "as",
    "\x34" "assert",
    "\x39" "bend",
    "\x19" "binary",
    "\x4f" "binclude",
    "\x38" "block",
    "\x05" "byte",
    "\x4d" "cdef",
    "\x31" "cerror",
    "\x06" "char",
    "\x35" "check",
    "\x1a" "comment",
    "\x36" "cpu",
    "\x32" "cwarn",
    "\x27" "databank",
    "\x0c" "dint",
    "\x28" "dpage",
    "\x4b" "dsection",
    "\x46" "dstruct",
    "\x49" "dunion",
    "\x0d" "dword",
    "\x4e" "edef",
    "\x14" "else",
    "\x16" "elsif",
    "\x2b" "enc",
    "\x3e" "end",
    "\x1b" "endc",
    "\x2c" "endif",
    "\x10" "endm",
    "\x1d" "endp",
    "\x45" "ends",
    "\x48" "endu",
    "\x3f" "eor",
    "\x24" "error",
    "\x15" "fi",
    "\x29" "fill",
    "\x11" "for",
    "\x43" "goto",
    "\x1f" "here",
    "\x3d" "hidemac",
    "\x13" "if",
    "\x2e" "ifeq",
    "\x30" "ifmi",
    "\x2d" "ifne",
    "\x2f" "ifpl",
    "\x18" "include",
    "\x08" "int",
    "\x42" "lbl",
    "\x0a" "lint",
    "\x1e" "logical",
    "\x0b" "long",
    "\x0f" "macro",
    "\x12" "next",
    "\x04" "null",
    "\x0e" "offs",
    "\x37" "option",
    "\x1c" "page",
    "\x26" "pend",
    "\x25" "proc",
    "\x3b" "proff",
    "\x3a" "pron",
    "\x01" "ptext",
    "\x17" "rept",
    "\x07" "rta",
    "\x4a" "section",
    "\x40" "segment",
    "\x4c" "send",
    "\x02" "shift",
    "\x03" "shiftl",
    "\x3c" "showmac",
    "\x44" "struct",
    "\x00" "text",
    "\x47" "union",
    "\x41" "var",
    "\x2a" "warn",
    "\x09" "word",
    "\x23" "xl",
    "\x22" "xs",
};

enum command_e {
    CMD_TEXT=0, CMD_PTEXT, CMD_SHIFT, CMD_SHIFTL, CMD_NULL, CMD_BYTE, CMD_CHAR,
    CMD_RTA, CMD_INT, CMD_WORD, CMD_LINT, CMD_LONG, CMD_DINT, CMD_DWORD,
    CMD_OFFS, CMD_MACRO, CMD_ENDM, CMD_FOR, CMD_NEXT, CMD_IF, CMD_ELSE, CMD_FI,
    CMD_ELSIF, CMD_REPT, CMD_INCLUDE, CMD_BINARY, CMD_COMMENT, CMD_ENDC,
    CMD_PAGE, CMD_ENDP, CMD_LOGICAL, CMD_HERE, CMD_AS, CMD_AL, CMD_XS, CMD_XL,
    CMD_ERROR, CMD_PROC, CMD_PEND, CMD_DATABANK, CMD_DPAGE, CMD_FILL, CMD_WARN,
    CMD_ENC, CMD_ENDIF, CMD_IFNE, CMD_IFEQ, CMD_IFPL, CMD_IFMI, CMD_CERROR,
    CMD_CWARN, CMD_ALIGN, CMD_ASSERT, CMD_CHECK, CMD_CPU, CMD_OPTION,
    CMD_BLOCK, CMD_BEND, CMD_PRON, CMD_PROFF, CMD_SHOWMAC, CMD_HIDEMAC,
    CMD_END, CMD_EOR, CMD_SEGMENT, CMD_VAR, CMD_LBL, CMD_GOTO, CMD_STRUCT,
    CMD_ENDS, CMD_DSTRUCT, CMD_UNION, CMD_ENDU, CMD_DUNION, CMD_SECTION,
    CMD_DSECTION, CMD_SEND, CMD_CDEF, CMD_EDEF, CMD_BINCLUDE,
};

// ---------------------------------------------------------------------------

void status(void) {
    freeerrorlist(1);
    errors+=conderrors;
    if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1])) {
        printf("Error messages:    ");
        if (errors) printf("%u\n", errors); else puts("None");
        printf("Warning messages:  ");
        if (warnings) printf("%u\n", warnings); else puts("None");
        printf("Passes:            %u\n",pass);
        memprint();
        sectionprint();
    }
    destroy_mem();
    free_values();
    tfree();
    {
        int i;
        for (i = 0; i < macro_parameters.len; i++) {
            free(macro_parameters.params[i].pline);
            free(macro_parameters.params[i].param);
        }
        free(macro_parameters.params);
    }
}

void printllist(FILE *f) {
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
    putc('\n', f);
}

static void new_waitfor(char what, linepos_t epoint) {
    waitfor[++waitforp].what = what;
    waitfor[waitforp].line = sline;
    waitfor[waitforp].epoint = epoint;
    waitfor[waitforp].label = NULL;
    waitfor[waitforp].skip=waitfor[waitforp-1].skip & 1;
}

static void set_size(struct label_s *var, size_t size) {
    size &= all_mem2;
    if (var->size != size) {
        var->size = size;
        if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, var->origname);
        fixeddig = 0;
    }
}

// ---------------------------------------------------------------------------
/*
 * Skip memory
 */
static void memskip(address_t db) {
    if (fixeddig && scpumode) {
        if (((current_section->address + db)^current_section->address) & ~(address_t)0xffff) wrapwarn2=1;
        if (((current_section->l_address + db)^current_section->l_address) & ~(address_t)0xffff) wrapwarn2=1;
    }
    current_section->l_address += db;
    if (current_section->l_address > all_mem) {
        if (fixeddig) wrapwarn2=1;
        current_section->l_address &= all_mem;
    }
    if (db > (~current_section->address & all_mem2)) {
        wrapwarn = 1;
        if (current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
    }
    current_section->address = (current_section->address + db) & all_mem2;
    memjmp(current_section->address);
}

// ---------------------------------------------------------------------------
/*
 * output one byte
 */
static void pokeb(uint8_t byte)
{

    if (current_section->dooutput) write_mem(byte ^ outputeor);
    if (wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);wrapwarn=0;}
    if (wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);wrapwarn2=0;}
    current_section->address++;current_section->l_address++;
    if (current_section->address & ~all_mem2) {
	if (fixeddig) wrapwarn=1;
	current_section->address=0;
        if (current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
        memjmp(current_section->address);
    }
    if (current_section->l_address & ~all_mem) {
	if (fixeddig) wrapwarn2=1;
	current_section->l_address=0;
    }
    if (fixeddig && !(current_section->l_address & 0xffff)) wrapwarn2=1;
}

static int lookup_opcode(const char *s) {
    uint8_t s2,s3, ch;
    int32_t s4;
    unsigned int also,felso,elozo, no;

    ch=lowcase(s[0]);
    s2=lowcase(s[1]);
    s3=lowcase(s[2]);
    uint32_t name = (ch << 16) | (s2 << 8) | s3;
    also = 0;
    no = (felso=last_mnem)/2;
    for (;;) {  // do binary search
        if (!(s4=name-mnemonic[no]))
            return no;
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
            unsigned int no, also, felso, elozo;
            linepos_t l;
            int s4;
            lpoint++;
            for (also = l = 0; l + 1 < (linepos_t)sizeof(cmd); l++) {
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
	case 'z': ignore();return WHAT_Z;
	case 'x': ignore();if (here()==')') {lpoint++;ignore();return WHAT_XZ;} else return WHAT_X;
	case 's': ignore();if (here()==')') {lpoint++;ignore();return WHAT_SZ;} else return WHAT_S;
	case 'r': ignore();if (here()==')') {lpoint++;ignore();return WHAT_RZ;} else return WHAT_R;
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

static int get_ident2(char *ident, char *ident2) {
    linepos_t i = 0;
    uint8_t ch;
    if (arguments.casesensitive) {
	while ((whatis[ch=here()]==WHAT_CHAR) || (ch>='0' && ch<='9') || ch=='_') {
            ident[i++]=ch;
            lpoint++;
        }
        memcpy(ident2, ident, i);
    } else {
	while (((ch=lowcase(here()))>='a' && ch<='z') || (ch>='0' && ch<='9') || ch=='_') {
            ident2[i]=here();
            ident[i++]=ch;
            lpoint++;
        }
    }
    ident[i]=0;
    ident2[i]=0;
    return i == 0;
}

static int get_hack(void) {
    int q=1;
    unsigned int i=0, i2;
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
static inline void mtranslate()
{
    uint_fast8_t q;
    uint_fast16_t j;
    linepos_t p;
    uint8_t ch, *cucc = macro_parameters.current->pline;

    q=p=0;
    for (; (ch = here()); lpoint++) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        else if ((ch == ';') && (!q)) { q=4; }
        else if ((ch=='\\') && (!q)) {
            /* normal parameter reference */
            if (((ch=lowcase(pline[lpoint+1])) >= '1' && ch <= '9') || (ch >= 'a' && ch <= 'z')) {
                /* \1..\9, \a..\z */
                if ((j=(ch<='9' ? ch-'1' : ch-'a'+9)) >= macro_parameters.current->len) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                if (p + macro_parameters.current->param[j].len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    memcpy(cucc + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                    p += macro_parameters.current->param[j].len;
                }
                lpoint++;continue;
            } else if (ch=='@') {
                /* \@ gives complete parameter list */
                if (p + macro_parameters.current->all.len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    memcpy(cucc + p, macro_parameters.current->all.data, macro_parameters.current->all.len);
                    p += macro_parameters.current->all.len;
                }
                lpoint++;continue;
            } else ch='\\';
        } else if (ch=='@' && arguments.tasmcomp) {
            /* text parameter reference */
            if (((ch=lowcase(pline[lpoint+1]))>='1' && ch<='9')) {
                /* @1..@9 */
                if ((j=ch-'1') >= macro_parameters.current->len) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                if (p + macro_parameters.current->param[j].len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    if (macro_parameters.current->param[j].len > 1 && macro_parameters.current->param[j].data[0] == '"' && macro_parameters.current->param[j].data[macro_parameters.current->param[j].len-1]=='"') {
                        memcpy(cucc + p, macro_parameters.current->param[j].data + 1, macro_parameters.current->param[j].len - 2);
                        p += macro_parameters.current->param[j].len - 2;
                    } else {
                        memcpy(cucc + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                        p += macro_parameters.current->param[j].len;
                    }
                }
                lpoint++;continue;
            } else ch='@';
        }
        cucc[p++]=ch;
        if (p>=linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
    }
    cucc[p]=0;
    pline = cucc; lpoint = 0;
}

//------------------------------------------------------------------------------

static void set_cpumode(uint_fast8_t cpumode) {
    all_mem=0xffff;scpumode=0;dtvmode=0;
    switch (last_mnem=cpumode) {
    case OPCODES_C65C02:mnemonic=mnemonic_c65c02;opcode=c65c02;break;
    case OPCODES_C65CE02:mnemonic=mnemonic_c65ce02;opcode=c65ce02;break;
    case OPCODES_C6502I:mnemonic=mnemonic_c6502i;opcode=c6502i;break;
    case OPCODES_C65816:mnemonic=mnemonic_c65816;opcode=c65816;all_mem=0xffffff;scpumode=1;break;
    case OPCODES_C65DTV02:mnemonic=mnemonic_c65dtv02;opcode=c65dtv02;dtvmode=1;break;
    case OPCODES_C65EL02:mnemonic=mnemonic_c65el02;opcode=c65el02;break;
    case OPCODES_CR65C02:mnemonic=mnemonic_cr65c02;opcode=cr65c02;break;
    case OPCODES_CW65C02:mnemonic=mnemonic_cw65c02;opcode=cw65c02;break;
    default: mnemonic=mnemonic_c6502;opcode=c6502;break;
    }
    all_mem2 = arguments.flat ? ~(address_t)0 : all_mem;
}

void var_assign(struct label_s *tmp, struct value_s *val, int fix) {
    tmp->upass = pass;
    if (val_equal(tmp->value, val)) return;
    val_replace(&tmp->value, val);
    if (fixeddig && !fix && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, tmp->origname);
    fixeddig=fix;
}

static void compile(void);

static void macro_recurse(char t, struct label_s *tmp2) {
    if (macro_parameters.p>100) {
        err_msg(ERROR__MACRECURSION,"!!!!");
        return;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        macro_parameters.len += 1;
        macro_parameters.params = realloc(macro_parameters.params, sizeof(*macro_parameters.params) * macro_parameters.len);
        if (!macro_parameters.params) err_msg_out_of_memory();
        macro_parameters.params[macro_parameters.p].param = NULL;
        macro_parameters.params[macro_parameters.p].size = 0;
        macro_parameters.params[macro_parameters.p].pline = malloc(linelength);
    }
    macro_parameters.current = &macro_parameters.params[macro_parameters.p];
    macro_parameters.p++;
    {
        uint_fast8_t q = 0, ch;
        linepos_t opoint, npoint;
        size_t p = 0;

        ignore(); opoint = lpoint;
        if (here() && here()!=';') {
            char par[256];
            uint8_t pp = 0;
            do {
                linepos_t opoint2, npoint2;
                ignore(); opoint2 = lpoint;
                while ((ch=here()) && (q || (ch!=';' && (ch!=',' || pp)))) {
                    if (ch == '"'  && !(q & 2)) { q^=1; }
                    else if (ch == '\'' && !(q & 1)) { q^=2; }
                    if (!q) {
                        if (ch == '(' || ch =='[') par[pp++]=ch;
                        else if (pp && ((ch == ')' && par[pp-1]=='(') || (ch == ']' && par[pp-1]=='['))) pp--;
                    }
                    lpoint++;
                }
                if (p >= macro_parameters.current->size) {
                    macro_parameters.current->size += 4;
                    macro_parameters.current->param = realloc(macro_parameters.current->param, sizeof(*macro_parameters.current->param) * macro_parameters.current->size);
                    if (!macro_parameters.current->param) err_msg_out_of_memory();
                }
                macro_parameters.current->param[p].data = pline + opoint2;
                npoint2 = lpoint;
                while (npoint2 > opoint2 && (pline[npoint2-1] == 0x20 || pline[npoint2-1] == 0x09)) npoint2--;
                macro_parameters.current->param[p].len = npoint2 - opoint2;
                p++;
                if (ch == ',') lpoint++;
            } while (ch == ',');
        }
        macro_parameters.current->len = p;
        macro_parameters.current->all.data = pline + opoint;
        npoint = lpoint;
        while (npoint > opoint && (pline[npoint-1] == 0x20 || pline[npoint-1] == 0x09)) npoint--;
        macro_parameters.current->all.len = npoint - opoint;
    }
    {
        size_t oldpos = tmp2->file->p;
        line_t lin = sline;
        struct file_s *f;
        struct star_s *s = new_star(vline);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, tmp2->origname);
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        enterfile(tmp2->file->realname, sline);
        sline = tmp2->sline;
        new_waitfor(t, 0);
        f = cfile; cfile = tmp2->file;
        cfile->p = tmp2->p;
        compile();
        exitfile(); cfile = f;
        star_tree = stree_old; vline = ovline;
        sline = lin; tmp2->file->p = oldpos;
    }
    macro_parameters.p--;
    if (macro_parameters.p) macro_parameters.current = &macro_parameters.params[macro_parameters.p - 1];
}

static void compile(void)
{
    int wht,w;
    int prm = 0;
    struct value_s *val;

    struct label_s *newlabel = NULL;
    struct label_s *tmp2 = NULL;
    address_t oaddr = 0;

    uint8_t oldwaitforp = waitforp;
    unsigned wasref;
    int nobreak = 1;
    char labelname[linelength], labelname2[linelength];
    linepos_t epoint;

    while (cfile->len != cfile->p && nobreak) {
        pline = cfile->data + cfile->p; lpoint = 0; sline++;vline++; cfile->p += strlen((const char *)pline) + 1;
        if (macro_parameters.p) mtranslate(); //expand macro parameters, if any
        llist = pline;
        star=current_section->l_address;newlabel = NULL;
        labelname2[0]=wasref=0;ignore();epoint=lpoint;
        if (current_section->unionmode) {
            if (current_section->address > current_section->unionend) current_section->unionend = current_section->address;
            if (current_section->l_address > current_section->l_unionend) current_section->l_unionend = current_section->l_address;
            current_section->l_address = current_section->l_unionstart;
            if (current_section->address != current_section->unionstart) {
                current_section->address = current_section->unionstart;
                memjmp(current_section->address);
            }
        }
        if ((wht=what(&prm))==WHAT_EXPRESSION) {
            int islabel;
            if (!prm) {
                if (here()=='-' || here()=='+') {
                    labelname2[0]=here();labelname2[1]=0;
                    lpoint++;if (here()!=0x20 && here()!=0x09 && here()!=';' && here()) goto baj;
                    if (labelname2[0]=='-') {
                        sprintf(labelname,"-%x-%x", reffile, backr++);
                    } else {
                        sprintf(labelname,"+%x+%x", reffile, forwr++);
                    }
                    islabel = 1;goto hh;
                }
            baj:
                if (waitfor[waitforp].skip & 1) err_msg2(ERROR_GENERL_SYNTAX,NULL, epoint);
                goto breakerr;
            } //not label
            get_ident2(labelname, labelname2);islabel = (here()==':');
            if (islabel) lpoint++;
            else if (labelname[0] && labelname[1] && labelname[2] && !labelname[3] && (prm=lookup_opcode(labelname))>=0) {
                if (waitfor[waitforp].skip & 1) goto as_opcode; else continue;
            }
        hh:
            if (!(waitfor[waitforp].skip & 1)) {wht=what(&prm);goto jn;} //skip things if needed
            if ((wht=what(&prm))==WHAT_EQUAL) { //variable
                newlabel=find_label2(labelname, &current_context->members);
                if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                if (newlabel && !newlabel->ref && pass != 1) goto finish;
                if (!(val = get_val(T_IDENTREF, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                eval_finish();
                if (newlabel) labelexists = 1;
                else newlabel = new_label(labelname, labelname2, L_CONST);
                oaddr=current_section->address;
                if (listing && flist && arguments.source && newlabel->ref) {
                    if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                    if (type_is_int(val->type)) {
                        fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.num.val);
                    } else {
                        fputs("=\t\t\t\t\t", flist);
                    }
                    printllist(flist);
                }
                newlabel->ref=0;
                if (labelexists) {
                    if (pass==1) err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                    else {
                        newlabel->requires=current_section->requires;
                        newlabel->conflicts=current_section->conflicts;
                        var_assign(newlabel, val, 0);
                    }
                } else {
                    newlabel->requires=current_section->requires;
                    newlabel->conflicts=current_section->conflicts;
                    newlabel->pass=pass;
                    newlabel->value=&none_value;
                    var_assign(newlabel, val, fixeddig);
                    newlabel->file = cfile;
                    newlabel->sline = sline;
                    newlabel->epoint = epoint;
                }
                goto finish;
            }
            if (wht==WHAT_COMMAND) {
                switch (prm) {
                case CMD_VAR: //variable
                    {
                        newlabel=find_label2(labelname, &current_context->members);
                        if (!get_exp(&w, 0)) goto breakerr; //ellenorizve.
                        if (newlabel && !newlabel->ref && pass != 1 && newlabel->upass != pass) goto finish;
                        if (!(val = get_val(T_IDENTREF, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (newlabel) labelexists = 1;
                        else newlabel = new_label(labelname, labelname2, L_VAR);
                        oaddr=current_section->address;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                            if (type_is_int(val->type)) {
                                fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.num.val);
                            } else {
                                fputs("=\t\t\t\t\t", flist);
                            }
                            printllist(flist);
                        }
                        if (labelexists) {
                            if (newlabel->upass != pass) newlabel->ref=0;
                            if (newlabel->type != L_VAR) err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                            else {
                                newlabel->requires=current_section->requires;
                                newlabel->conflicts=current_section->conflicts;
                                var_assign(newlabel, val, fixeddig);
                            }
                        } else {
                            newlabel->requires=current_section->requires;
                            newlabel->conflicts=current_section->conflicts;
                            newlabel->pass=pass;
                            newlabel->value=&none_value;
                            var_assign(newlabel, val, fixeddig);
                            newlabel->file = cfile;
                            newlabel->sline = sline;
                            newlabel->epoint = epoint;
                            if (val->type == T_NONE) err_msg(ERROR___NOT_DEFINED,"argument used");
                        }
                        goto finish;
                    }
                case CMD_LBL:
                    { //variable
                        struct jump_s *tmp3;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                            fputs("=\t\t\t\t\t", flist);
                            printllist(flist);
                        }
                        tmp3 = new_jump(labelname, labelname2);
                        if (labelexists) {
                            if (tmp3->sline != sline
                                    || tmp3->waitforp != waitforp
                                    || tmp3->file != cfile
                                    || tmp3->p != cfile->p
                                    || tmp3->parent != current_context) {
                                err_msg_double_defined(tmp3->origname, tmp3->file->realname, tmp3->sline, tmp3->epoint, labelname2, epoint);
                            }
                        } else {
                            tmp3->sline = sline;
                            tmp3->waitforp = waitforp;
                            tmp3->file = cfile;
                            tmp3->p = cfile->p;
                            tmp3->parent = current_context;
                            tmp3->epoint = epoint;
                        }
                        goto finish;
                    }
                case CMD_MACRO:// .macro
                case CMD_SEGMENT:
                    new_waitfor('m', epoint);waitfor[waitforp].skip=0;
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    newlabel=new_label(labelname, labelname2, (prm==CMD_MACRO)?L_MACRO:L_SEGMENT);
                    if (labelexists) {
                        if (newlabel->p != cfile->p
                         || newlabel->sline != sline
                         || newlabel->type != ((prm == CMD_MACRO) ? L_MACRO : L_SEGMENT)
                         || newlabel->file != cfile) {
                            err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                        }
                    } else {
                        newlabel->requires=0;
                        newlabel->conflicts=0;
                        newlabel->pass=pass;
                        newlabel->value = &none_value;
                        new_value.type = T_LABEL;
                        new_value.u.num.val = 0;
                        new_value.u.num.label = newlabel;
                        var_assign(newlabel, &new_value, fixeddig);
                        newlabel->memp = ~(size_t)0; newlabel->membp = ~(size_t)0;
                        newlabel->p = cfile->p;
                        newlabel->sline = sline;
                        newlabel->file = cfile;
                        newlabel->epoint = epoint;
                    }
                    newlabel->ref=0;
                    newlabel = NULL;
                    goto finish;
                case CMD_STRUCT:
                case CMD_UNION:
                    {
                        struct label_s *old_context=current_context;
                        struct section_s olds = *current_section;
                        int declaration = !current_section->structrecursion;

                        new_waitfor((prm==CMD_STRUCT)?'s':'u', epoint);waitfor[waitforp].skip=0;
                        ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                        newlabel=new_label(labelname, labelname2, declaration ? ((prm == CMD_STRUCT) ? L_STRUCT : L_UNION) : L_LABEL);oaddr = current_section->address;
                        if (declaration) {
                            current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
                            current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
                            current_section->r_start=current_section->r_l_start=current_section->r_address=current_section->r_l_address=0;
                            current_section->dooutput=0;memjmp(0); oaddr = 0;

                            if (labelexists) {
                                if (newlabel->p != cfile->p
                                        || newlabel->sline != sline
                                        || newlabel->type != ((prm==CMD_STRUCT)?L_STRUCT:L_UNION)
                                        || newlabel->file != cfile) {
                                    err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                                }
                            } else {
                                newlabel->requires = 0;
                                newlabel->conflicts = 0;
                                newlabel->pass = pass;
                                newlabel->value = &none_value;
                                new_value.type = T_LABEL;
                                new_value.u.num.val = 0;
                                new_value.u.num.label = newlabel;
                                var_assign(newlabel, &new_value, fixeddig);
                                newlabel->memp = ~(size_t)0; newlabel->membp = ~(size_t)0;
                                newlabel->p = cfile->p;
                                newlabel->sline = sline;
                                newlabel->file = cfile;
                                newlabel->epoint = epoint;
                            }
                        } else {
                            if (labelexists) {
                                if (pass==1) err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                                else {
                                    if (newlabel->type != L_LABEL) { /* should not happen */
                                        err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                                    } else {
                                        if ((uval_t)newlabel->value->u.num.val != current_section->l_address) {
                                            new_value.type=T_LABEL;
                                            new_value.u.num.val = current_section->l_address;
                                            new_value.u.num.label = newlabel;
                                            var_assign(newlabel, &new_value, 0);
                                        } else newlabel->upass = pass;
                                        get_mem(&newlabel->memp, &newlabel->membp);
                                        newlabel->requires=current_section->requires;
                                        newlabel->conflicts=current_section->conflicts;
                                    }
                                }
                            } else {
                                newlabel->requires=current_section->requires;
                                newlabel->conflicts=current_section->conflicts;
                                newlabel->pass=pass;
                                newlabel->value = &none_value;
                                new_value.type = T_LABEL;
                                new_value.u.num.val = current_section->l_address;
                                new_value.u.num.label = newlabel;
                                var_assign(newlabel, &new_value, fixeddig);
                                get_mem(&newlabel->memp, &newlabel->membp);
                                newlabel->file = cfile;
                                newlabel->sline = sline;
                                newlabel->epoint = epoint;
                            }
                        }
                        current_context=newlabel;
                        newlabel->ref=0;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_DATA) {putc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            printllist(flist);
                        }
                        current_section->structrecursion++;
                        if (current_section->structrecursion<100) {
                            int old_unionmode = current_section->unionmode;
                            address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                            address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                            current_section->unionmode = (prm==CMD_UNION);
                            current_section->unionstart = current_section->unionend = current_section->address;
                            current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                            waitforp--;
                            new_waitfor((prm==CMD_STRUCT)?'S':'U', epoint);waitfor[waitforp].skip=1;
                            compile();
                            current_context = old_context;
                            current_section->unionmode = old_unionmode;
                            current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                            current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                        } else err_msg(ERROR__MACRECURSION,"!!!!");
                        current_section->structrecursion--;
                        set_size(newlabel, current_section->address - oaddr);
                        if (declaration) {
                            current_section->provides=olds.provides;current_section->requires=olds.requires;current_section->conflicts=olds.conflicts;
                            current_section->start=olds.start;current_section->l_start=olds.l_start;current_section->address=olds.address;current_section->l_address=olds.l_address;
                            current_section->r_start=olds.r_start;current_section->r_l_start=olds.r_l_start;current_section->r_address=olds.r_address;current_section->r_l_address=olds.r_l_address;
                            current_section->dooutput=olds.dooutput;memjmp(current_section->address);
                        }
			newlabel = NULL;
                        goto finish;
                    }
                case CMD_SECTION:
                    {
                        struct section_s *tmp;
                        char sectionname[linelength], sectionname2[linelength];
                        linepos_t opoint;
                        new_waitfor('t', epoint);waitfor[waitforp].section=current_section;
                        ignore();opoint=lpoint;
                        if (get_ident2(sectionname, sectionname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        tmp=find_new_section(sectionname, sectionname2);
                        if (!tmp->declared) {
                            if (!labelexists) {
                                tmp->start = tmp->address = tmp->r_address = 0;
                                tmp->l_start = tmp->l_address = tmp->r_l_address = 0;
                                if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, sectionname2);
                                fixeddig=0;
                            } else if (pass > 1) {
                                err_msg2(ERROR___NOT_DEFINED,sectionname2,opoint); goto breakerr;
                            }
                        } else if (tmp->pass != pass) {
                            tmp->r_address = tmp->address;
                            tmp->address = tmp->start;
                            tmp->r_l_address = tmp->l_address;
                            tmp->l_address = tmp->l_start;
                        }
                        tmp->pass = pass;
                        waitfor[waitforp].what = 'T';
                        current_section = tmp;
                        memjmp(current_section->address);
                        break;
                    }
                }
            }
            if (!islabel && (tmp2=find_label(labelname)) && (tmp2->type == L_MACRO || tmp2->type == L_SEGMENT)) {lpoint--;labelname2[0]=0;goto as_macro;}
            if (!islabel && tmp2 && tmp2->parent == current_context) {newlabel = tmp2;labelexists = 1;}
            else newlabel=new_label(labelname, labelname2, L_LABEL);
            oaddr=current_section->address;
            if (labelexists) {
                if (pass==1) err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                else {
                    if (newlabel->type != L_LABEL) { /* should not happen */
                        err_msg_double_defined(newlabel->origname, newlabel->file->realname, newlabel->sline, newlabel->epoint, labelname2, epoint);
                    } else {
                        if ((uval_t)newlabel->value->u.num.val != current_section->l_address) {
                            new_value.type=T_LABEL;
                            new_value.u.num.val = current_section->l_address;
                            new_value.u.num.label = newlabel;
                            var_assign(newlabel, &new_value, 0);
                        } else newlabel->upass = pass;
                        get_mem(&newlabel->memp, &newlabel->membp);
                        newlabel->requires=current_section->requires;
                        newlabel->conflicts=current_section->conflicts;
                    }
                }
            } else {
                newlabel->requires=current_section->requires;
                newlabel->conflicts=current_section->conflicts;
                newlabel->pass=pass;
                newlabel->value = &none_value;
                new_value.type = T_LABEL;
                new_value.u.num.val = current_section->l_address;
                new_value.u.num.label = newlabel;
                var_assign(newlabel, &new_value, fixeddig);
                get_mem(&newlabel->memp, &newlabel->membp);
                newlabel->file = cfile;
                newlabel->sline = sline;
                newlabel->epoint = epoint;
            }
            if (epoint && !islabel) err_msg2(ERROR_LABEL_NOT_LEF,NULL,epoint);
            if (wht==WHAT_COMMAND) { // .proc
                switch (prm) {
                case CMD_PROC:
                    new_waitfor('r', epoint);waitfor[waitforp].label=newlabel;waitfor[waitforp].addr = current_section->address;
                    if (!newlabel->ref && pass != 1) waitfor[waitforp].skip=0;
                    else {
                        current_context=newlabel;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t%s\n":".%06" PRIaddress "\t\t\t\t\t%s\n",current_section->address,labelname2);
                        }
                        newlabel->ref=0;
                    }
                    newlabel = NULL;
                    goto finish;
                case CMD_DSTRUCT: // .dstruct
                case CMD_DUNION:
                    {
                        struct label_s *oldcontext = current_context;
                        int old_unionmode = current_section->unionmode;
                        address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                        address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                        current_section->unionmode = (prm==CMD_DUNION);
                        current_section->unionstart = current_section->unionend = current_section->address;
                        current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                        current_context=newlabel;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_DATA) {putc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            printllist(flist);
                        }
                        newlabel->ref=0;
                        ignore();epoint=lpoint;
                        if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                        if (!(tmp2=find_label(labelname)) || tmp2->type!=((prm==CMD_DSTRUCT) ? L_STRUCT : L_UNION)) {err_msg2(ERROR___NOT_DEFINED,labelname2,epoint); goto breakerr;}
                        current_section->structrecursion++;
                        macro_recurse((prm==CMD_DSTRUCT)?'S':'U',tmp2);
                        current_section->structrecursion--;
                        current_context=oldcontext;
                        current_section->unionmode = old_unionmode;
                        current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                        current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                        goto finish;
                    }
                case CMD_SECTION:
                    waitfor[waitforp].label=newlabel;waitfor[waitforp].addr = current_section->address;
                    if (newlabel->ref && listing && flist && arguments.source) {
                        if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t%s\n":".%06" PRIaddress "\t\t\t\t\t%s\n",current_section->address,labelname2);
                    }
                    newlabel->ref=0;
                    newlabel = NULL;
                    goto finish;
                }
            }
            wasref=newlabel->ref;newlabel->ref=0;
        }
        jn:
        switch (wht) {
        case WHAT_STAR:if (waitfor[waitforp].skip & 1) //skip things if needed
            {
                ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                lpoint++;
                wrapwarn=0;wrapwarn2=0;
                if (!get_exp(&w,0)) goto breakerr;
                if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (val == &error_value) goto breakerr;
                eval_finish();
                if (listing && flist && arguments.source) {
                    lastl=LIST_NONE;
                    if (wasref)
                        fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                    else
                        fputs("\n\t\t\t\t\t", flist);
                    printllist(flist);
                }
                if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, "*=");
                else if (val->type == T_NONE) {
                    if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "*", epoint);
                    fixeddig = 0;
                } else {
                    if (arguments.flat && !current_section->logicalrecursion) {
                        if ((uval_t)val->u.num.val & ~(uval_t)all_mem2) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        } else {
                            current_section->l_address = (uval_t)val->u.num.val & all_mem;
                            if (current_section->address != (uval_t)val->u.num.val) {
                                current_section->address = (uval_t)val->u.num.val;
                                if (current_section->address < current_section->start) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
                                memjmp(current_section->address);
                            }
                        }
                    } else {
                        if ((uval_t)val->u.num.val & ~(uval_t)all_mem) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        } else {
                            address_t addr;
                            if (arguments.tasmcomp) addr = (uint16_t)val->u.num.val;
                            else if ((uval_t)val->u.num.val > current_section->l_address) {
                                addr = (current_section->address + (((uval_t)val->u.num.val - current_section->l_address) & all_mem)) & all_mem2;
                            } else {
                                addr = (current_section->address - ((current_section->l_address - (uval_t)val->u.num.val) & all_mem)) & all_mem2;
                            }
                            if (current_section->address != addr) {
                                current_section->address = addr;
                                if (current_section->address < current_section->start) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
                                memjmp(current_section->address);
                            }
                            current_section->l_address = (uval_t)val->u.num.val;
                        }
                    }
                }
            }
            break;
        case WHAT_COMMENT:
        case WHAT_EOL:
            if (listing && flist && arguments.source && (waitfor[waitforp].skip & 1) && wasref) {
                if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                printllist(flist);
            }
            break;
        case WHAT_COMMAND:
            {
                ignore();
                if (listing && flist && arguments.source && (waitfor[waitforp].skip & 1) && prm>CMD_DWORD) {
                    switch (prm) {
                        case CMD_OFFS:
                        case CMD_ENDS:
                        case CMD_STRUCT:
                        case CMD_ENDU:
                        case CMD_UNION:
                            if (lastl!=LIST_DATA) {putc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            printllist(flist);
                        case CMD_ALIGN:
                        case CMD_FILL:
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
                        case CMD_BINCLUDE:
                            if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                            if (wasref)
                                fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            else
                                fputs("\t\t\t\t\t", flist);
                            printllist(flist);
                            break;
                        default:
                            if (wasref) {
                                if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                                fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t%s\n":".%06" PRIaddress "\t\t\t\t\t%s\n",current_section->address,labelname2);
                            }
                    }
                }
                if (prm==CMD_ENDC) { // .endc
                    if (waitfor[waitforp].what!='c') err_msg2(ERROR______EXPECTED,".COMMENT", epoint);
                    else waitforp--;
                    break;
                } else if (waitfor[waitforp].what=='c') break;
                if (prm==CMD_FI) // .fi
                {
                    if (waitfor[waitforp].what!='e' && waitfor[waitforp].what!='f') err_msg2(ERROR______EXPECTED,".IF", epoint);
                    else waitforp--;
                    break;
                }
                if (prm==CMD_ELSE) { // .else
                    if (waitfor[waitforp].what=='f') {err_msg2(ERROR______EXPECTED,".FI", epoint); break;}
                    if (waitfor[waitforp].what!='e') {err_msg2(ERROR______EXPECTED,".IF", epoint); break;}
                    waitfor[waitforp].skip=waitfor[waitforp].skip >> 1;
                    waitfor[waitforp].what='f';waitfor[waitforp].line=sline;
                    break;
                }
                if (prm==CMD_IF || prm==CMD_IFEQ || prm==CMD_IFPL || prm==CMD_IFMI || prm==CMD_ELSIF) { // .if
                    uint8_t skwait = waitfor[waitforp].skip;
                    if (prm==CMD_ELSIF) {
                        if (waitfor[waitforp].what!='e') {err_msg2(ERROR______EXPECTED,".IF", epoint); break;}
                    } else new_waitfor('e', epoint);
                    waitfor[waitforp].line=sline;
                    if (((skwait==1) && prm!=CMD_ELSIF) || ((skwait==2) && prm==CMD_ELSIF)) {
                        if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (val->type == T_NONE) err_msg2(ERROR___NOT_DEFINED,"argument used for condition", epoint);
                    } else val = &none_value;
                    switch (prm) {
                    case CMD_ELSIF:
                        waitfor[waitforp].skip = val_truth(val) ? (waitfor[waitforp].skip >> 1) : (waitfor[waitforp].skip & 2);
                        break;
                    case CMD_IF:
                        waitfor[waitforp].skip = val_truth(val) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);
                        break;
                    case CMD_IFEQ:
                        switch (val->type) {
                        case T_SINT:
                        case T_UINT:
                        case T_BOOL:
                        case T_LABEL:
                        case T_NUM: waitfor[waitforp].skip = (!val->u.num.val) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        case T_FLOAT: waitfor[waitforp].skip = (!val->u.real) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        case T_STR: waitfor[waitforp].skip = (!val->u.str.len) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        default: waitfor[waitforp].skip = (waitfor[waitforp-1].skip & 1) << 1;break;
                        }
                        break;
                    case CMD_IFPL:
                        switch (val->type) {
                        case T_SINT:
                        case T_UINT:
                        case T_BOOL:
                        case T_LABEL:
                        case T_NUM: waitfor[waitforp].skip = (arguments.tasmcomp ? (~val->u.num.val & 0x8000) : (val->u.num.val>=0)) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        case T_FLOAT: waitfor[waitforp].skip = (val->u.real >= 0.0) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        case T_STR: waitfor[waitforp].skip = val->u.str.len ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        default: waitfor[waitforp].skip = (waitfor[waitforp-1].skip & 1) << 1;break;
                        }
                        break;
                    case CMD_IFMI:
                        switch (val->type) {
                        case T_SINT:
                        case T_UINT:
                        case T_BOOL:
                        case T_LABEL:
                        case T_NUM: waitfor[waitforp].skip = (arguments.tasmcomp ? (val->u.num.val & 0x8000) : (val->u.num.val < 0)) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        case T_FLOAT: waitfor[waitforp].skip = (val->u.real < 0.0) ? (waitfor[waitforp-1].skip & 1) : ((waitfor[waitforp-1].skip & 1) << 1);break;
                        default: waitfor[waitforp].skip = (waitfor[waitforp-1].skip & 1) << 1;break;
                        }
                        break;
                    }
                    break;
                }
                if (prm==CMD_ENDM) { // .endm
                    if (waitfor[waitforp].what=='m') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='M') {
                        waitforp--; nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".MACRO or .SEGMENT", epoint);
                    break;
                }
                if (prm==CMD_NEXT) { // .next
                    if (waitfor[waitforp].what=='n') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='N') {
                        waitforp--; nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".FOR or .REPT", epoint);
                    break;
                }
                if (prm==CMD_PEND) { //.pend
                    if (waitfor[waitforp].what!='r') {err_msg2(ERROR______EXPECTED,".PROC", epoint); break;}
                    if (waitfor[waitforp].skip & 1) {
                        if (current_context->parent) {
                            current_context = current_context->parent;
                        } else err_msg2(ERROR______EXPECTED,".proc", epoint);
			lastl=LIST_NONE;
			if (waitfor[waitforp].label) set_size(waitfor[waitforp].label, current_section->address - waitfor[waitforp].addr);
                    }
                    waitforp--;
                    break;
                }
                if (prm==CMD_ENDS) { // .ends
                    if (waitfor[waitforp].what=='s') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='S') {
                        waitforp--; nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".STRUCT", epoint); break;
                    break;
                }
                if (prm==CMD_SEND) { // .send
                    if (waitfor[waitforp].what=='t') {
                        waitforp--;get_ident2(labelname, labelname2);
                    } else if (waitfor[waitforp].what=='T') {
                        ignore();epoint=lpoint;
                        if (!get_ident2(labelname, labelname2)) {
                            if (strcmp(labelname, current_section->name)) err_msg2(ERROR______EXPECTED,current_section->name,epoint);
                        }
			if (waitfor[waitforp].label) set_size(waitfor[waitforp].label, current_section->address - waitfor[waitforp].addr);
                        current_section = waitfor[waitforp].section;
                        memjmp(current_section->address);
                        waitforp--;
                    } else err_msg2(ERROR______EXPECTED,".SECTION or .DSECTION", epoint);
                    break;
                }
                if (prm==CMD_ENDU) { // .endu
                    if (waitfor[waitforp].what=='u') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='U') {
                        waitforp--; nobreak=0; current_section->l_address = current_section->l_unionend;
                        if (current_section->address != current_section->unionend) {
                            current_section->address = current_section->unionend;
                            memjmp(current_section->address);
                        }
                    } else err_msg2(ERROR______EXPECTED,".UNION", epoint); break;
                    break;
                }
                if (prm==CMD_ENDP) { // .endp
                    if (waitfor[waitforp].what=='p') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='P') {
			if ((current_section->l_address & ~0xff) != (waitfor[waitforp].laddr & ~0xff) && fixeddig) {
				err_msg2(ERROR____PAGE_ERROR, &current_section->l_address, epoint);
			}
			if (waitfor[waitforp].label) set_size(waitfor[waitforp].label, current_section->address - waitfor[waitforp].addr);
                        waitforp--;
                    } else err_msg2(ERROR______EXPECTED,".PAGE", epoint); break;
                    break;
                }
                if (prm==CMD_HERE) { // .here
                    if (waitfor[waitforp].what=='l') {
                        waitforp--;
                        current_section->logicalrecursion--;
                    } else if (waitfor[waitforp].what=='L') {
			current_section->l_address = current_section->address + waitfor[waitforp].laddr;
			if (waitfor[waitforp].label) set_size(waitfor[waitforp].label, current_section->address - waitfor[waitforp].addr);
                        waitforp--;
                        current_section->logicalrecursion--;
                    } else err_msg2(ERROR______EXPECTED,".LOGICAL", epoint); break;
                    break;
                }
                if (prm==CMD_BEND) { //.bend
                    if (waitfor[waitforp].what=='b') {
                        waitforp--;
                    } else if (waitfor[waitforp].what=='B') {
			if (waitfor[waitforp].label) set_size(waitfor[waitforp].label, current_section->address - waitfor[waitforp].addr);
			if (current_context->parent) current_context = current_context->parent;
			else err_msg2(ERROR______EXPECTED,".block", epoint);
			waitforp--;
                    } else err_msg2(ERROR______EXPECTED,".BLOCK", epoint); break;
                    break;
                }
                if (!(waitfor[waitforp].skip & 1)) {
                    char what2;
                    switch (prm) {
                    case CMD_BLOCK: what2 = 'b'; break;
                    case CMD_LOGICAL: what2 = 'l'; break;
                    case CMD_PAGE: what2 = 'p'; break;
                    case CMD_UNION: what2 = 'u'; break;
                    case CMD_STRUCT: what2 = 's'; break;
                    case CMD_DSECTION:
                    case CMD_SECTION: what2 = 't'; break;
                    case CMD_MACRO:
                    case CMD_SEGMENT: what2 = 'm'; break;
                    case CMD_FOR:
                    case CMD_REPT: what2 = 'n'; break;
                    case CMD_COMMENT: what2 = 'c'; break;
                    case CMD_PROC: what2 = 'r'; break;
                    default: what2 = 0;
                    }
                    if (what2) new_waitfor(what2, epoint);
                    break;//skip things if needed
                }
                if (prm<=CMD_DWORD || prm==CMD_BINARY) { // .byte .text .rta .char .int .word .long
                    size_t uninit = 0;
                    size_t sum = 0;

                    mark_mem(current_section->address);
                    if (prm<CMD_BYTE) {    // .text .ptext .shift .shift2 .null
                        int16_t ch2=-1;
                        int large=0;
                        if (newlabel) newlabel->esize = 1;
                        if (prm==CMD_PTEXT) ch2=0;
                        if (!get_exp(&w,0)) goto breakerr;
                        while ((val = get_val(T_NONE, &epoint))) {
                            if (val->type != T_STR || val->u.str.len) {
                                size_t i = 0;
                                do {
                                    if (ch2 >= 0) {
                                        if (uninit) { memskip(uninit); sum += uninit; uninit = 0; }
                                        pokeb(ch2);
                                        sum++;
                                    }

                                    switch (val->type) {
                                    case T_GAP:ch2 = -1; uninit++; continue;
                                    case T_STR:
                                        ch2 = petascii(&i, val);
                                        if (ch2 > 255) i = val->u.str.len;
                                        break;
                                    case T_BOOL:
                                    case T_LABEL:
                                    case T_NUM:
                                    case T_UINT:
                                    case T_SINT:
                                        if (val->type != T_NUM || val->u.num.len > 8) {
                                            if ((uval_t)val->u.num.val & ~(uval_t)0xff) large=epoint;
                                        }
                                        ch2 = (uint8_t)val->u.num.val;
                                        break;
                                    case T_FLOAT:
                                        if ((uval_t)val->u.real & ~(uval_t)0xff) large=epoint;
                                        ch2 = (uint8_t)val->u.real;
                                        break;
                                    case T_LIST:
                                    case T_TUPLE:
                                        {
                                            size_t li;
                                            ch2 = -1;
                                            for (li=0; li < val->u.list.len; li++) {
                                                struct value_s *val2 = val->u.list.data[li];
                                                i = 0;
                                                do {
                                                    if (ch2 >= 0) {
                                                        if (uninit) { memskip(uninit); sum += uninit; uninit = 0; }
                                                        pokeb(ch2); sum++;
                                                    }
                                                    switch (val2->type) {
                                                    case T_GAP:ch2 = -1; uninit++; continue;
                                                    case T_STR:
                                                        ch2 = petascii(&i, val2);
                                                        if (ch2 > 255) i = val2->u.str.len;
                                                        break;
                                                    case T_BOOL:
                                                    case T_LABEL:
                                                    case T_NUM:
                                                    case T_UINT:
                                                    case T_SINT:
                                                        if (val2->type != T_NUM || val2->u.num.len > 8) {
                                                            if ((uval_t)val2->u.num.val & ~(uval_t)0xff) large=epoint;
                                                        }
                                                        ch2 = (uint8_t)val2->u.num.val;
                                                        break;
                                                    case T_FLOAT:
                                                        if ((uval_t)val2->u.real & ~(uval_t)0xff) large=epoint;
                                                        ch2 = (uint8_t)val2->u.real;
                                                        break;
                                                    default: err_msg_wrong_type(val2, epoint);
                                                    case T_NONE:
                                                             if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                                             ch2 = fixeddig = 0;
                                                    }
                                                    if (prm==CMD_SHIFT || prm==CMD_SHIFTL) {
                                                        if (ch2>=0x80) large=epoint;
                                                        if (prm==CMD_SHIFTL) ch2<<=1;
                                                    } else if (prm==CMD_NULL && !ch2 && val->type != T_NONE) large=epoint;
                                                } while (val2->type == T_STR && val2->u.str.len > i);
                                            }
                                            continue;
                                        }
                                    default: err_msg_wrong_type(val, epoint);
                                    case T_NONE:
                                             if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                             ch2 = fixeddig = 0;
                                    }
                                    if (prm==CMD_SHIFT || prm==CMD_SHIFTL) {
                                        if (ch2>=0x80) large=epoint;
                                        if (prm==CMD_SHIFTL) ch2<<=1;
                                    } else if (prm==CMD_NULL && !ch2 && val->type != T_NONE) large=epoint;
                                } while (val->type == T_STR && val->u.str.len > i);
                            }
                        }
                        if (uninit) {memskip(uninit);sum += uninit;}
                        if (ch2>=0) {
                            if (prm==CMD_SHIFT) ch2|=0x80;
                            if (prm==CMD_SHIFTL) ch2|=0x01;
                            pokeb(ch2); sum++;
                        } else if (prm==CMD_SHIFT || prm==CMD_SHIFTL) large = epoint;
                        if (prm==CMD_NULL) pokeb(0);
                        if (prm==CMD_PTEXT) {
                            if (sum>0x100) large=epoint;

                            if (fixeddig && current_section->dooutput) write_mark_mem(sum-1);
                        }
                        if (large) err_msg2(ERROR_CONSTNT_LARGE, NULL, large);
                    } else if (prm<=CMD_DWORD) { // .word .int .rta .long
                        uint32_t ch2;
                        uval_t uv;
                        int large=0;
                        if (newlabel) {
                            newlabel->esize = 1 + (prm>=CMD_RTA) + (prm>=CMD_LINT) + (prm >= CMD_DINT);
                            newlabel->sign = (prm == CMD_CHAR) || (prm == CMD_INT) || (prm == CMD_DINT);
                        }
                        if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                        while ((val = get_val(T_NONE, &epoint))) {
                            if (val == &error_value) ch2 = 0; else
                            switch (val->type) {
                            case T_GAP:uninit += 1 + (prm>=CMD_RTA) + (prm>=CMD_LINT) + (prm >= CMD_DINT);continue;
                            case T_STR: if (str_to_num(val, T_NUM, &new_value)) {large = epoint; ch2 = 0; break;} val = &new_value;
                            case T_FLOAT:
                            case T_LABEL:
                            case T_NUM:
                            case T_BOOL:
                            case T_SINT:
                            case T_UINT:
                                uv = (val->type == T_FLOAT) ? (uval_t)val->u.real : (uval_t)val->u.num.val;
                                switch (prm) {
                                case CMD_CHAR: if ((val->u.num.len > 8 || val->type != T_NUM) && (uv & ~(uval_t)0x7f) && (~uv & ~(uval_t)0x7f)) large=epoint;break;
                                case CMD_BYTE: if ((val->u.num.len > 8 || val->type != T_NUM) && (uv & ~(uval_t)0xff)) large=epoint; break;
                                case CMD_INT: if ((val->u.num.len > 16 || val->type != T_NUM) && (uv & ~(uval_t)0x7fff) && (~uv & ~(uval_t)0x7fff)) large=epoint;break;
                                case CMD_LINT: if ((val->u.num.len > 24 || val->type != T_NUM) && (uv & ~(uval_t)0x7fffff) && (~uv & ~(uval_t)0x7fffff)) large=epoint;break;
                                case CMD_LONG: if ((val->u.num.len > 24 || val->type != T_NUM) && (uv & ~(uval_t)0xffffff)) large=epoint; break;
                                case CMD_DINT: if ((val->u.num.len > 32 || val->type != T_NUM) && (uv & ~(uval_t)0x7fffffff) && (~uv & ~(uval_t)0x7fffffff)) large=epoint;break;
                                case CMD_DWORD: if ((val->u.num.len > 32 || val->type != T_NUM) && (uv & ~(uval_t)0xffffffff)) large=epoint; break;
                                default: if ((val->u.num.len > 16 || val->type != T_NUM) && (uv & ~(uval_t)0xffff)) large=epoint;
                                }
                                ch2 = uv;
                                break;
                            case T_LIST:
                            case T_TUPLE:
                                {
                                    size_t li;
                                    for (li=0; li < val->u.list.len; li++) {
                                        struct value_s *val2 = val->u.list.data[li];
                                        switch (val2->type) {
                                        case T_GAP:uninit += 1 + (prm>=CMD_RTA) + (prm>=CMD_LINT) + (prm >= CMD_DINT);continue;
                                        case T_STR: if (str_to_num(val2, T_NUM, &new_value)) {large = epoint; ch2 = 0; break;} val2 = &new_value;
                                        case T_FLOAT:
                                        case T_LABEL:
                                        case T_NUM:
                                        case T_BOOL:
                                        case T_SINT:
                                        case T_UINT:
                                            uv = (val2->type == T_FLOAT) ? (uval_t)val2->u.real : (uval_t)val2->u.num.val;
                                            switch (prm) {
                                            case CMD_CHAR: if ((val2->u.num.len > 8 || val2->type != T_NUM) && (uv & ~(uval_t)0x7f) && (~uv & ~(uval_t)0x7f)) large=epoint;break;
                                            case CMD_BYTE: if ((val2->u.num.len > 8 || val2->type != T_NUM) && (uv & ~(uval_t)0xff)) large=epoint; break;
                                            case CMD_INT: if ((val2->u.num.len > 16 || val2->type != T_NUM) && (uv & ~(uval_t)0x7fff) && (~uv & ~(uval_t)0x7fff)) large=epoint;break;
                                            case CMD_LINT: if ((val2->u.num.len > 24 || val2->type != T_NUM) && (uv & ~(uval_t)0x7fffff) && (~uv & ~(uval_t)0x7fffff)) large=epoint;break;
                                            case CMD_LONG: if ((val2->u.num.len > 24 || val2->type != T_NUM) && (uv & ~(uval_t)0xffffff)) large=epoint; break;
                                            case CMD_DINT: if ((val2->u.num.len > 32 || val2->type != T_NUM) && (uv & ~(uval_t)0x7fffffff) && (~uv & ~(uval_t)0x7fffffff)) large=epoint;break;
                                            case CMD_DWORD: if ((val2->u.num.len > 32 || val2->type != T_NUM) && (uv & ~(uval_t)0xffffffff)) large=epoint; break;
                                            default: if ((val2->u.num.len > 16 || val2->type != T_NUM) && (uv & ~(uval_t)0xffff)) large=epoint;
                                            }
                                            ch2 = uv;
                                            break;
                                        default: err_msg_wrong_type(val2, epoint);
                                        case T_NONE:
                                                 if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                                 ch2 = fixeddig = 0;
                                        }
                                        if (prm==CMD_RTA) ch2--;

                                        if (uninit) {memskip(uninit);uninit = 0;}
                                        pokeb((uint8_t)ch2);
                                        if (prm>=CMD_RTA) pokeb((uint8_t)(ch2>>8));
                                        if (prm>=CMD_LINT) pokeb((uint8_t)(ch2>>16));
                                        if (prm>=CMD_DINT) pokeb((uint8_t)(ch2>>24));
                                    }
                                    continue;
                                }
                            default: err_msg_wrong_type(val, epoint);
                            case T_NONE:
                                     if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                     ch2 = fixeddig = 0;
                            }
                            if (prm==CMD_RTA) ch2--;

                            if (uninit) {memskip(uninit);uninit = 0;}
                            pokeb((uint8_t)ch2);
                            if (prm>=CMD_RTA) pokeb((uint8_t)(ch2>>8));
                            if (prm>=CMD_LINT) pokeb((uint8_t)(ch2>>16));
                            if (prm>=CMD_DINT) pokeb((uint8_t)(ch2>>24));
                        }
                        if (uninit) memskip(uninit);
                        if (large) err_msg2(ERROR_CONSTNT_LARGE, NULL, large);
                    } else if (prm==CMD_BINARY) { // .binary
                        size_t foffset = 0;
                        struct value_s *val2 = NULL;
                        address_t fsize = all_mem+1;
                        if (newlabel) newlabel->esize = 1;
                        if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                            fixeddig = 0;
                        } else {
                            if (val->type != T_STR) {err_msg_wrong_type(val, epoint);goto breakerr;}
                            if (get_path(val, cfile->realname, path, sizeof(path))) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);goto breakerr;}
                            val2 = val;
                        }
                        if ((val = get_val(T_UINT, &epoint))) {
                            if (val == &error_value) goto breakerr;
                            if (val->type == T_NONE) {
                                if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                fixeddig = 0;
                            } else {
                                if (val->u.num.val<0) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint); goto breakerr;}
                                foffset = val->u.num.val;
                            }
                            if ((val = get_val(T_UINT, &epoint))) {
                                if (val == &error_value) goto breakerr;
                                if (val->type == T_NONE) {
                                    if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                                    fixeddig = 0;
                                } else {
                                    if (val->u.num.val<0 || (address_t)val->u.num.val > fsize) err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);
                                    else fsize = val->u.num.val;
                                }
                            }
                        }
                        eval_finish();

                        if (val2) {
                            struct file_s *cfile2 = openfile(path, cfile->realname, 1, val2);
                            if (cfile2) {
                                for (;fsize && foffset < cfile2->len;fsize--) {
                                    pokeb(cfile2->data[foffset]);foffset++;
                                }
                            }
                        }
                    }

                    if (listing && flist) {
                        list_mem(flist, all_mem, &llist, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_OFFS) {   // .offs
                    linepos_t opoint = epoint;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_SINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else if (val->u.num.val) {
                        if (fixeddig && scpumode) {
                            if (((current_section->address + val->u.num.val)^current_section->address) & ~(address_t)0xffff) wrapwarn2=1;
                        }
                        if (current_section->structrecursion) {
                            if (val->u.num.val < 0) err_msg2(ERROR___NOT_ALLOWED, ".OFFS", opoint);
                            else {
                                current_section->l_address+=val->u.num.val;
                                current_section->address+=val->u.num.val;
                            }
                        } else current_section->address+=val->u.num.val;
                        if (current_section->address & ~all_mem) {
                            if (fixeddig) wrapwarn=1;
                            current_section->address&=all_mem;
                        }
                        if (current_section->address < current_section->start) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
                        memjmp(current_section->address);
                    }
                    break;
                }
                if (prm==CMD_LOGICAL) { // .logical
                    linepos_t opoint = epoint;
                    new_waitfor('L', epoint);waitfor[waitforp].laddr = current_section->l_address - current_section->address;waitfor[waitforp].label=newlabel;waitfor[waitforp].addr = current_section->address;
                    current_section->logicalrecursion++;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val == &error_value) goto breakerr;
                    else if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".LOGICAL", opoint);
                    else if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else {
                        if ((uval_t)val->u.num.val & ~(uval_t)all_mem) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else current_section->l_address=(uval_t)val->u.num.val;
                    }
                    newlabel = NULL;
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
                if (prm==CMD_BLOCK) { // .block
		    new_waitfor('B', epoint);
                    if (newlabel) {
                        current_context=newlabel;
                        waitfor[waitforp].label=newlabel;waitfor[waitforp].addr = current_section->address;
                        if (newlabel->ref && listing && flist && arguments.source) {
                            if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t%s\n":".%06" PRIaddress "\t\t\t\t\t%s\n",current_section->address,labelname2);
                        }
                        newlabel = NULL;
                    } else {
                        sprintf(labelname, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                        current_context=new_label(labelname, labelname, L_LABEL);
                        current_context->value = &none_value;
                    }
                    break;
                }
                if (prm==CMD_DATABANK) { // .databank
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else {
                        if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else databank=val->u.num.val;
                    }
                    break;
                }
                if (prm==CMD_DPAGE) { // .dpage
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else {
                        if ((val->type != T_NUM || val->u.num.len > 16) && ((uval_t)val->u.num.val & ~(uval_t)0xffff)) err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);
                        else {
                            if (dtvmode) dpage=val->u.num.val & 0xff00;
                            else dpage=val->u.num.val;
                        }
                    }
                    break;
                }
                if (prm==CMD_FILL) { // .fill
                    address_t db = 0;
                    int ch = -1;
                    if (newlabel) newlabel->esize = 1;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else {
                        db=val->u.num.val;
                        if (db && db - 1 > all_mem2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);goto breakerr;}
                    }
                    if ((val = get_val(T_GAP, &epoint))) {
                        if (val == &error_value) goto breakerr;
                        else if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                            fixeddig = 0;
                        } else if (val->type != T_GAP) {
                            if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                            ch = (uint8_t)val->u.num.val;
                        }
                    }
                    eval_finish();
                    mark_mem(current_section->address);
                    if (ch >= 0) while (db-- > 0) pokeb(ch);
                    else memskip(db);
                    if (listing && flist) {
                        list_mem(flist, all_mem, &llist, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_ASSERT) { // .assert
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;current_section->provides=~(uval_t)0;
                    } else current_section->provides=val->u.num.val;
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = current_section->requires = 0;
                    } else current_section->requires=val->u.num.val;
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = current_section->conflicts = 0;
                    } else current_section->conflicts=val->u.num.val;
                    eval_finish();
                    break;
                }
                if (prm==CMD_CHECK) { // .check
                    linepos_t opoint = epoint;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else if ((val->u.num.val & current_section->provides) ^ val->u.num.val) err_msg(ERROR_REQUIREMENTS_,".CHECK");
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg2(ERROR_CANT_CALCULAT, "", epoint);
                        fixeddig = 0;
                    } else if (val->u.num.val & current_section->provides) err_msg2(ERROR______CONFLICT,".CHECK", opoint);
                    eval_finish();
                    break;
                }
                if (prm==CMD_WARN || prm==CMD_CWARN || prm==CMD_ERROR || prm==CMD_CERROR) { // .warn .cwarn .error .cerror
                    int rc;
                    int first = 1;
                    int write = 1;
                    struct encoding_s *old = actual_encoding;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; //ellenorizve.
                    err_msg_variable(NULL, 0);
                    for (;;) {
                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;
                        if (first) {
                            first = 0;
                            if (prm == CMD_CWARN || prm == CMD_CERROR) {
                                if (val->type == T_UNDEF) err_msg_wrong_type(val, epoint);
                                write = val_truth(val);
                                continue;
                            }
                            write = 1;
                        }
                        if (write) {
                            if (val->type == T_UNDEF) err_msg_wrong_type(val, epoint);
                            else if (val->type != T_NONE) err_msg_variable(val, 0);
                        }
                    }
                    if (write) err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED,NULL,epoint);
                    eval_finish();
                    break;
                }
                if (prm==CMD_ENC) { // .enc
                    ignore();epoint=lpoint;
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    actual_encoding = new_encoding(labelname);
                    break;
                }
                if (prm==CMD_CDEF) { // .cdef
                    struct trans_s tmp, *t;
                    struct encoding_s *old = actual_encoding;
                    uint32_t ch;
                    int rc;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; //ellenorizve.
                    for (;;) {
                        int endok = 0;
                        size_t i = 0;

                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->type) {
                        case T_NONE: err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);goto breakerr;
                        case T_NUM: if (val->u.num.len <= 24) { tmp.start = val->u.num.val; break; }
                        case T_LABEL:
                        case T_BOOL:
                        case T_UINT:
                        case T_SINT:
                             if ((uval_t)val->u.num.val & ~(uval_t)0xffffff) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             tmp.start = val->u.num.val & (uval_t)0xffffff;
                             break;
                        case T_FLOAT:
                             if ((uval_t)val->u.real & ~(uval_t)0xffffff) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             tmp.start = (uval_t)val->u.real & (uval_t)0xffffff;
                             break;
                        case T_STR:
                             if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             else {
                                 ch = val->u.str.data[0];
                                 if (ch & 0x80) i = utf8in(val->u.str.data, &ch); else i = 1;
                                 tmp.start = ch;
                             }
                             if (val->u.str.len > i) {
                                 ch = val->u.str.data[i];
                                 if (ch & 0x80) i += utf8in(val->u.str.data + i, &ch); else i++;
                                 if (tmp.start > ch) {
                                     tmp.end = tmp.start;
                                     tmp.start = ch;
                                 } else tmp.end = ch;
                                 endok = 1;
                             }
                             if (val->u.str.len > i) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             break;
                        default:
                            err_msg_wrong_type(val, epoint);
                            goto breakerr;
                        }
                        if (!endok) {
                            actual_encoding = NULL;
                            val = get_val(T_UINT, &epoint);
                            actual_encoding = old;
                            if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                            if (val == &error_value) goto breakerr;
                            if (val->type == T_NONE) {err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);goto breakerr;}
                            if ((val->type != T_NUM || val->u.num.len > 24) && ((uval_t)val->u.num.val & ~(uval_t)0xffffff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                            if (tmp.start > (uint32_t)val->u.num.val) {
                                tmp.end = tmp.start;
                                tmp.start = val->u.num.val;
                            } else tmp.end = val->u.num.val;
                        }
                        actual_encoding = NULL;
                        val = get_val(T_UINT, &epoint);
                        actual_encoding = old;
                        if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                        if (val == &error_value) goto breakerr;
                        if (val->type == T_NONE) {err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);goto breakerr;}
                        if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        tmp.offset = val->u.num.val;
                        t = new_trans(&tmp, actual_encoding);
                        if (t->start != tmp.start || t->end != tmp.end || t->offset != tmp.offset) {
                            err_msg2(ERROR_DOUBLE_DEFINE, "range", epoint); goto breakerr;
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_EDEF) { // .edef
                    struct escape_s *t;
                    struct encoding_s *old = actual_encoding;
                    int rc;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; //ellenorizve.
                    for (;;) {
                        linepos_t opoint;
                        char expr[linelength];

                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->type) {
                        case T_NONE: err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);goto breakerr;
                        case T_STR:
                             if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             memcpy(expr, val->u.str.data, val->u.str.len);
                             expr[val->u.str.len]=0;
                             break;
                        default:
                            err_msg_wrong_type(val, epoint);
                            goto breakerr;
                        }
                        actual_encoding = NULL;
                        opoint = epoint;
                        val = get_val(T_UINT, &epoint);
                        actual_encoding = old;
                        if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                        if (val == &error_value) goto breakerr;
                        if (val->type == T_NONE) {err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);goto breakerr;}
                        if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        t = new_escape(expr, (uint8_t)val->u.num.val, actual_encoding);
                        if (t->code != (uint8_t)val->u.num.val) {
                            err_msg2(ERROR_DOUBLE_DEFINE,"escape", opoint); goto breakerr;
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_CPU) { // .cpu
                    int def;
                    if (get_hack()) goto breakerr;
                    def=arguments.cpumode;
                    if (!strcmp(path,"6502")) def=OPCODES_C6502;
                    else if (!strcasecmp(path,"65c02")) def=OPCODES_C65C02;
                    else if (!strcasecmp(path,"65ce02")) def=OPCODES_C65CE02;
                    else if (!strcasecmp(path,"6502i")) def=OPCODES_C6502I;
                    else if (!strcmp(path,"65816")) def=OPCODES_C65816;
                    else if (!strcasecmp(path,"65dtv02")) def=OPCODES_C65DTV02;
                    else if (!strcasecmp(path,"65el02")) def=OPCODES_C65EL02;
                    else if (!strcmp(path,"r65c02")) def=OPCODES_CR65C02;
                    else if (!strcmp(path,"w65c02")) def=OPCODES_CW65C02;
                    else if (strcasecmp(path,"default")) err_msg(ERROR___UNKNOWN_CPU,path);
                    set_cpumode(def);
                    break;
                }
                if (prm==CMD_REPT) { // .rept
                    new_waitfor('n', epoint);waitfor[waitforp].skip=0;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (eval_finish()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL);goto breakerr;}
                    if (val->type == T_NONE) err_msg2(ERROR___NOT_DEFINED, "repeat count", epoint);
                    else {
                        ival_t cnt = val->u.num.val;
                        if (cnt > 0) {
                            size_t pos = cfile->p;
                            line_t lin = sline;
                            struct star_s *s = new_star(vline);
                            struct avltree *stree_old = star_tree;
                            line_t ovline = vline;

                            waitforp--;
                            if (labelexists && s->addr != star) {
                                if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                fixeddig=0;
                            }
                            s->addr = star;
                            star_tree = &s->tree;vline=0;
                            while (cnt--) {
                                sline=lin;cfile->p=pos;
                                new_waitfor('N', epoint);waitfor[waitforp].skip=1;
                                compile();
                            }
                            star_tree = stree_old; vline = ovline;
                        }
                    }
                    break;
                }
                if (prm==CMD_ALIGN) { // .align
                    int align = 1, fill=-1;
                    if (newlabel) newlabel->esize = 1;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".ALIGN");
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                        fixeddig = 0;
                    } else {
                        if (!val->u.num.val || ((uval_t)val->u.num.val & ~(uval_t)all_mem)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else align = val->u.num.val;
                    }
                    if ((val = get_val(T_GAP, &epoint))) {
                        if (val == &error_value) goto breakerr;
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                            fixeddig = 0;
                        } else if (val->type != T_GAP) {
                            if ((val->u.num.len > 8 || val->type != T_NUM) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                            fill = (uint8_t)val->u.num.val;
                        }
                    }
                    eval_finish();
                    mark_mem(current_section->address);
                    if (align>1 && (current_section->l_address % align)) {
                        if (fill >= 0)
                            while (current_section->l_address % align) pokeb((uint8_t)fill);
                        else {
                            align-=current_section->l_address % align;
                            if (align) memskip(align);
                        }
                    }
                    if (listing && flist) {
                        list_mem(flist, all_mem, &llist, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_EOR) {   // .eor
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_NUM, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                        fixeddig = outputeor = 0;
                    } else {
                        if ((val->u.num.len > 8 || val->type != T_NUM) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else outputeor = val->u.num.val;
                    }
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
                    new_waitfor('c', epoint);waitfor[waitforp].skip=0;
                    break;
                }
                if (prm==CMD_INCLUDE || prm == CMD_BINCLUDE) { // .include, .binclude
                    struct file_s *f;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->type == T_NONE) err_msg2(ERROR___NOT_DEFINED,"argument used", epoint);
                    else {
                        if (val->type != T_STR) {err_msg_wrong_type(val, epoint);goto breakerr;}
                        if (get_path(val, cfile->realname, path, sizeof(path))) {err_msg(ERROR_CONSTNT_LARGE,NULL);goto breakerr;}

                        f = cfile;
                        cfile = openfile(path, cfile->realname, 0, val);
                        if (cfile->open>1) {
                            err_msg(ERROR_FILERECURSION,NULL);
                        } else {
                            if (listing && flist) {
                                fprintf(flist,"\n;******  Processing file \"%s\"\n",cfile->realname);
                                lastl=LIST_NONE;
                            }
                            line_t lin = sline;
                            line_t vlin = vline;
                            struct star_s *s = new_star(vline);
                            struct avltree *stree_old = star_tree;
                            uint32_t old_backr = backr, old_forwr = forwr;

                            if (labelexists && s->addr != star) {
                                if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                fixeddig=0;
                            }
                            s->addr = star;
                            enterfile(cfile->realname,sline);
                            sline = vline = 0; cfile->p=0;
                            star_tree = &s->tree;
                            backr = forwr = 0;
                            reffile=cfile->uid;
                            if (prm == CMD_BINCLUDE) {
                                if (newlabel) current_context = newlabel;
                                else {
                                    sprintf(labelname, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                                    current_context=new_label(labelname, labelname, L_LABEL);
                                    current_context->value = &none_value;
                                }
                                compile();
                                current_context = current_context->parent;
                            } else compile();
                            sline = lin; vline = vlin;
                            star_tree = stree_old;
                            backr = old_backr; forwr = old_forwr;
                            exitfile();
                        }
                        closefile(cfile);cfile = f;
                        reffile=cfile->uid;
                        if (listing && flist) {
                            fprintf(flist,"\n;******  Return to file \"%s\"\n",cfile->realname);
                            lastl=LIST_NONE;
                        }
                    }
                    break;
                }
                if (prm==CMD_FOR) { // .for
                    size_t pos, xpos;
                    line_t lin, xlin;
                    int apoint, bpoint = -1;
                    uint8_t expr[linelength];
                    struct label_s *var;
                    struct star_s *s;
                    struct avltree *stree_old;
                    line_t ovline;

                    new_waitfor('n', epoint);waitfor[waitforp].skip=0;
                    if (strlen((const char *)pline)>=linelength) {err_msg(ERROR_LINE_TOO_LONG,NULL);goto breakerr;}
                    if ((wht=what(&prm))==WHAT_EXPRESSION && prm==1) { //label
                        epoint = lpoint;
                        if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL);goto breakerr;}
                        ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                        lpoint++;
                        if (!get_exp(&w,1)) goto breakerr; //ellenorizve.
                        if (!(val = get_val(T_IDENTREF, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        var=new_label(labelname, labelname2, L_VAR);
                        if (labelexists) {
                            if (var->type != L_VAR) err_msg_double_defined(var->origname, var->file->realname, var->sline, var->epoint, labelname2, epoint);
                            else {
                                var->requires=current_section->requires;
                                var->conflicts=current_section->conflicts;
                                var_assign(var, val, fixeddig);
                            }
                        } else {
                            var->requires=current_section->requires;
                            var->conflicts=current_section->conflicts;
                            var->pass=pass;
                            var->value=&none_value;
                            var_assign(var, val, fixeddig);
                            var->file = cfile;
                            var->sline = sline;
                            var->epoint = epoint;
                            if (val->type == T_NONE) err_msg(ERROR___NOT_DEFINED,"argument used");
                        }
                        wht=what(&prm);
                    }
                    if (wht==WHAT_S || wht==WHAT_Y || wht==WHAT_X || wht==WHAT_R || wht==WHAT_Z) lpoint--; else
                        if (wht!=WHAT_COMA) {err_msg(ERROR______EXPECTED,","); goto breakerr;}

                    s = new_star(vline); stree_old = star_tree; ovline = vline;
                    if (labelexists && s->addr != star) {
                        if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                        fixeddig=0;
                    }
                    s->addr = star;
                    star_tree = &s->tree;vline=0;
                    xlin=lin=sline; xpos=pos=cfile->p; apoint=lpoint;
                    strcpy((char *)expr, (const char *)pline);var = NULL;
                    for (;;) {
                        lpoint=apoint;
                        if (!get_exp(&w,1)) break; //ellenorizve.
                        if (!(val = get_val(T_NONE, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        if (val->type == T_NONE) {err_msg(ERROR___NOT_DEFINED,"argument used in condition");break;}
                        if (!val_truth(val)) break;
                        if (bpoint < 0) {
                            ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                            lpoint++;ignore();
                            epoint = lpoint;
                            if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL);break;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                            lpoint++;ignore();
                            if (!here() || here()==';') bpoint = 0;
                            else {
                                var=new_label(labelname, labelname2, L_VAR);
                                if (labelexists) {
                                    if (var->type != L_VAR) {
                                        err_msg_double_defined(var->origname, var->file->realname, var->sline, var->epoint, labelname2, epoint);
                                        break;
                                    }
                                    var->requires=current_section->requires;
                                    var->conflicts=current_section->conflicts;
                                } else {
                                    var->requires=current_section->requires;
                                    var->conflicts=current_section->conflicts;
                                    var->pass=pass;
                                    var_assign(var, &none_value, fixeddig);
                                    var->file = cfile;
                                    var->sline = sline;
                                    var->epoint = epoint;
                                }
                                bpoint=lpoint;
                            }
                        }
                        new_waitfor('N', epoint);waitfor[waitforp].skip=1;
                        compile();
                        xpos = cfile->p; xlin= sline;
                        pline = expr;
                        sline=lin;cfile->p=pos;
                        if (bpoint) {
                            lpoint=bpoint;
                            if (!get_exp(&w,1)) break; //ellenorizve.
                            if (!(val = get_val(T_IDENTREF, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                            var_assign(var, val, fixeddig);
                            ignore();if (here() && here()!=';') {err_msg(ERROR_EXTRA_CHAR_OL,NULL);break;}
                        }
                    }
                    if (pos!=xpos || lin!=xlin) waitforp--;
                    sline=xlin;cfile->p=xpos;
                    star_tree = stree_old; vline = ovline;
                    goto breakerr;
                }
                if (prm==CMD_PAGE) { // .page
                    new_waitfor('P', epoint);waitfor[waitforp].addr = current_section->address;waitfor[waitforp].laddr = current_section->l_address;waitfor[waitforp].label=newlabel;
                    newlabel=NULL;
                    break;
                }
                if (prm==CMD_OPTION) { // .option
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); goto breakerr;}
                    lpoint++;
                    if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->type == T_NONE) {err_msg2(ERROR___NOT_DEFINED,"argument used for option", epoint);goto breakerr;}
                    if (!strcasecmp(labelname,"allow_branch_across_page")) allowslowbranch=val_truth(val);
                    else if (!strcasecmp(labelname,"auto_longbranch_as_jmp")) longbranchasjmp=val_truth(val);
                    else err_msg(ERROR_UNKNOWN_OPTIO,labelname2);
                    break;
                }
                if (prm==CMD_GOTO) { // .goto
                    const struct jump_s *tmp3;
                    int noerr = 1;
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp3 = find_jump(labelname);
                    if (tmp3 && tmp3->file == cfile && tmp3->parent == current_context) {
                        uint8_t oldwaitforp2 = waitforp;
                        while (tmp3->waitforp < waitforp) {
                            const char *msg = NULL;
                            line_t os = sline;
                            sline = waitfor[waitforp].line;
                            switch (waitfor[waitforp].what) {
                            case 'M':
                            case 'm': msg = ".ENDM"; break;
                            case 'N':
                            case 'n': msg = ".NEXT"; break;
                            case 'r': msg = ".PEND"; break;
			    case 'B':
                            case 'b': msg = ".BEND"; break;
                            case 'S':
                            case 's': msg = ".ENDS"; break;
                            case 'T':
                            case 't': msg = ".SEND"; break;
                            case 'U':
                            case 'u': msg = ".ENDU"; break;
			    case 'P':
                            case 'p': msg = ".ENDP"; break;
                            case 'L':
                            case 'l': msg = ".HERE"; break;
                            }
                            if (msg) {
                                err_msg2(ERROR______EXPECTED, msg, waitfor[waitforp].epoint);
                                noerr = 0;
                            }
                            waitforp--; sline = os;
                        }
                        if (noerr) {
                            sline = tmp3->sline;
                            cfile->p = tmp3->p;
                        } else waitforp = oldwaitforp2;
                    } else err_msg(ERROR___NOT_DEFINED,labelname2);
                    break;
                }
                if (prm==CMD_MACRO || prm==CMD_SEGMENT) {
                    new_waitfor('m', epoint);waitfor[waitforp].skip=0;
                    err_msg2(ERROR___NOT_DEFINED,"",epoint);
                    break;
                }
                if (prm==CMD_LBL) {
                    err_msg2(ERROR___NOT_DEFINED,"",epoint);
                    break;
                }
                if (prm==CMD_PROC) {
                    new_waitfor('r', epoint);waitfor[waitforp].skip=0;waitfor[waitforp].addr=current_section->address;
                    err_msg2(ERROR___NOT_DEFINED,"",epoint);
                    break;
                }
                if (prm==CMD_STRUCT) {
                    int old_unionmode = current_section->unionmode;
                    current_section->unionmode = 0;
                    new_waitfor('s', epoint);waitfor[waitforp].skip=0;
                    current_section->structrecursion++;
                    if (current_section->structrecursion<100) {
                        waitforp--;
                        new_waitfor('S', epoint);waitfor[waitforp].skip=1;
                        compile();
                    } else err_msg(ERROR__MACRECURSION,"!!!!");
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    break;
                }
                if (prm==CMD_UNION) {
                    int old_unionmode = current_section->unionmode;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                    new_waitfor('u', epoint);waitfor[waitforp].skip=0;
                    current_section->structrecursion++;
                    if (current_section->structrecursion<100) {
                        waitforp--;
                        new_waitfor('U', epoint);waitfor[waitforp].skip=1;
                        compile();
                    } else err_msg(ERROR__MACRECURSION,"!!!!");
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                    break;
                }
                if (prm==CMD_DSTRUCT) {
                    int old_unionmode = current_section->unionmode;
                    current_section->unionmode = 0;
                    ignore();epoint=lpoint;
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (!(tmp2=find_label(labelname)) || tmp2->type != L_STRUCT) {err_msg2(ERROR___NOT_DEFINED,labelname2,epoint); goto breakerr;}
                    current_section->structrecursion++;
                    macro_recurse('S',tmp2);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    break;
                }
                if (prm==CMD_DUNION) {
                    int old_unionmode = current_section->unionmode;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    ignore();epoint=lpoint;
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (!(tmp2=find_label(labelname)) || tmp2->type != L_UNION) {err_msg2(ERROR___NOT_DEFINED,labelname2,epoint); goto breakerr;}
                    current_section->structrecursion++;
                    macro_recurse('U',tmp2);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    break;
                }
                if (prm==CMD_DSECTION) {
                    struct section_s *tmp3;
                    new_waitfor('t', epoint);
                    if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".DSECTION");
                    ignore();epoint=lpoint;
                    if (get_ident2(labelname, labelname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp3=new_section(labelname, labelname2);
                    if (tmp3->declared && pass == 1) err_msg_double_defined(tmp3->origname, tmp3->file, tmp3->sline, tmp3->epoint, labelname2, epoint);
                    else {
                        address_t t, t2;
                        waitfor[waitforp].what='T';waitfor[waitforp].section=current_section;
                        if (!tmp3->declared) {
                            tmp3->r_start = tmp3->r_address = current_section->address;
                            tmp3->r_l_start = tmp3->r_l_address = current_section->l_address;
                            if (!labelexists) {
                                tmp3->start = tmp3->address = current_section->address;
                                tmp3->l_start = tmp3->l_address = current_section->l_address;
                            } else {
                                tmp3->address += current_section->address;
                                tmp3->start += current_section->address;
                                tmp3->l_address += current_section->l_address;
                                tmp3->l_start += current_section->l_address;
                            }
                            tmp3->pass = pass;
                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                            fixeddig = 0;
                            tmp3->declared = 1;
                        }
                        tmp3->provides=~(uval_t)0;tmp3->requires=tmp3->conflicts=0;
                        tmp3->dooutput = current_section->dooutput;
                        tmp3->unionmode = current_section->unionmode;
                        tmp3->unionstart = current_section->unionstart;
                        tmp3->unionend = current_section->unionend;
                        tmp3->l_unionstart = current_section->l_unionstart;
                        tmp3->l_unionend = current_section->l_unionend;
                        tmp3->structrecursion = current_section->structrecursion;
                        tmp3->logicalrecursion = current_section->logicalrecursion;
                        tmp3->file = cfile->realname;
                        tmp3->sline = sline;
                        tmp3->epoint = epoint;
                        if (tmp3->pass == pass) {
                            t = tmp3->r_address - tmp3->r_start;
                            t2 = tmp3->address - tmp3->start;
                            if (newlabel) set_size(newlabel, t2 + t);
                            tmp3->start = current_section->address;
                            current_section->address += t2;
                            tmp3->r_start = tmp3->r_address = current_section->address;
                        } else {
                            t = tmp3->address - tmp3->start;
                            if (newlabel) set_size(newlabel, t);
                            tmp3->start = tmp3->r_start = tmp3->address = tmp3->r_address = current_section->address;
                        }
                        current_section->address += t;
                        if (tmp3->pass == pass) {
                            t = tmp3->r_l_address - tmp3->r_l_start;
                            t2 = tmp3->l_address - tmp3->l_start;
                            tmp3->l_start = current_section->l_address;
                            current_section->l_address += t2;
                            tmp3->r_l_start = tmp3->r_l_address = current_section->l_address;
                        } else {
                            t = tmp3->l_address - tmp3->l_start;
                            tmp3->l_start = tmp3->r_l_start = tmp3->l_address = tmp3->r_l_address = current_section->l_address;
                        }
                        current_section->l_address += t;
                        current_section = tmp3;
                        tmp3->pass=pass;
                        memjmp(current_section->address);
                        newlabel=NULL;
                    }
                    break;
                }
                if (prm==CMD_SECTION) {
                    struct section_s *tmp;
                    char sectionname[linelength], sectionname2[linelength];
                    new_waitfor('t', epoint);waitfor[waitforp].section=current_section;
                    ignore();epoint=lpoint;
                    if (get_ident2(sectionname, sectionname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp=find_new_section(sectionname, sectionname2);
                    if (!tmp->declared) {
                        if (!labelexists) {
                            tmp->start = tmp->address = tmp->r_address = 0;
                            tmp->l_start = tmp->l_address = tmp->r_l_address = 0;
                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                            fixeddig=0;
                        } else if (pass > 1) {
                            err_msg2(ERROR___NOT_DEFINED,sectionname2,epoint); goto breakerr;
                        }
                    } else if (tmp->pass != pass) {
                        tmp->r_address = tmp->address;
                        tmp->address = tmp->start;
                        tmp->r_l_address = tmp->l_address;
                        tmp->l_address = tmp->l_start;
                    }
                    tmp->pass = pass;
                    waitfor[waitforp].what = 'T';
                    current_section = tmp;
                    memjmp(current_section->address);
                    newlabel = NULL;
                    break;
                }
            }
        case WHAT_HASHMARK:if (waitfor[waitforp].skip & 1) //skip things if needed
            {                   //macro stuff
                struct label_s *old_context;
                char macroname[linelength], macroname2[linelength];

                if (get_ident2(macroname, macroname2)) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (!(tmp2=find_label(macroname)) || (tmp2->type != L_MACRO && tmp2->type != L_SEGMENT)) {err_msg(ERROR___NOT_DEFINED,macroname2); goto breakerr;}
            as_macro:
                if (listing && flist && arguments.source && wasref) {
                    if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                    fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t%s\n":".%06" PRIaddress "\t\t\t\t\t%s\n",current_section->address,labelname2);
                }
                if (tmp2->type == L_MACRO) {
                    old_context = current_context;
                    if (newlabel) current_context=newlabel;
                    else {
                        sprintf(labelname, "#%" PRIxPTR "#%" PRIxline, (uintptr_t)star_tree, vline);
                        current_context=new_label(labelname, labelname, L_LABEL);
                        current_context->value = &none_value;
                    }
                } else old_context = NULL;
                macro_recurse('M',tmp2);
                if (tmp2->type == L_MACRO) current_context = old_context;
                break;
            }
        case WHAT_EXPRESSION:
            if (waitfor[waitforp].skip & 1) {
                enum { AG_ZP, AG_B0, AG_PB, AG_BYTE, AG_DB3, AG_NONE } adrgen;

                get_ident2(labelname, labelname2);
                if (labelname[0] && labelname[1] && labelname[2] && !labelname[3] && (prm=lookup_opcode(labelname))>=0) {
                    enum opr_e opr;
                    int mnem, oldlpoint;
                    const uint8_t *cnmemonic; //current nmemonic
                    int_fast8_t ln;
                    uint8_t cod, longbranch;
                    uint32_t adr;
                    int d;
                as_opcode:

                    opr = 0;mnem = prm;
                    oldlpoint = lpoint;
                    cnmemonic = &opcode[prm*ADR_LEN];
                    ln = 0; cod = 0; longbranch = 0; adr = 0; adrgen = AG_NONE;

                    ignore();
                    if (!(wht=here()) || wht==';') {
                        opr=(cnmemonic[ADR_ACCU]==cnmemonic[ADR_IMPLIED])?ADR_ACCU:ADR_IMPLIED;w=ln=0;d=1;
                    }  //clc
                    // 1 Db
                    else if (lowcase(wht)=='a' && cnmemonic[ADR_ACCU]!=____ && (!pline[lpoint+1] || pline[lpoint+1]==';' || pline[lpoint+1]==0x20 || pline[lpoint+1]==0x09))
                    {
                        linepos_t opoint=lpoint;
                        lpoint++;ignore();
                        if (here() && here()!=';') {lpoint=opoint;goto nota;}
                        if (find_label("a")) err_msg(ERROR_A_USED_AS_LBL,NULL);
                        opr=ADR_ACCU;w=ln=0;d=1;// asl a
                    }
                    // 2 Db
                    else if (wht=='#') {
                        if ((cod=cnmemonic[(opr=ADR_IMMEDIATE)])==____ && prm) { // 0x69 hack
                            lpoint += strlen((const char *)pline + lpoint);ln=w=d=1;
                        } else {
                            lpoint++;
                            if (!get_exp(&w,0)) goto breakerr; //ellenorizve.
                            if (!(val = get_val(T_NUM, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                            eval_finish();
                            d = (val->type != T_NONE);

                            ln=1;
                            if (cod==0xE0 || cod==0xC0 || cod==0xA2 || cod==0xA0) {// cpx cpy ldx ldy
                                if (longindex && scpumode) ln++;
                            }
                            else if (cod==0xF4) ln=2; //pea #$ffff
                            else if (cod!=0xC2 && cod!=0xE2) {//not sep rep=all accu
                                if (longaccu && scpumode) ln++;
                            }

                            if (w==3) w = ln - 1;
                            else if (w != ln - 1) w = 3;
                            if (val->type != T_NONE) {
                                if (!w && ln == 1 && ((val->u.num.len <= 8 && val->type == T_NUM) || !((uval_t)val->u.num.val & ~(uval_t)0xff))) adr = (uval_t)val->u.num.val;
                                else if (w == 1 && ln == 2 && ((val->u.num.len <= 16 && val->type == T_NUM) || !((uval_t)val->u.num.val & ~(uval_t)0xffff))) adr = (uval_t)val->u.num.val;
                                else w = 3;
                            }
                        }
                    }
                    // 3 Db
                    else {
                        int c;
                        if (whatis[wht]!=WHAT_EXPRESSION && whatis[wht]!=WHAT_CHAR && wht!='_' && wht!='*') {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    nota:
                        if (!(c=get_exp(&w, cnmemonic[ADR_REL]==____ && cnmemonic[ADR_MOVE]==____ && cnmemonic[ADR_BIT_ZP]==____ && cnmemonic[ADR_BIT_ZP_REL]==____))) goto breakerr; //ellenorizve.
                        if (!(val = get_val(T_UINT, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val == &error_value) d = 0;
                        else if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                            d = fixeddig = 0;
                        } else {adr = val->u.num.val;d = 1;}

                        switch (c) {
                        case 1:
                            switch (what(&prm)) {
                            case WHAT_X:
                                adrgen = AG_DB3; opr=ADR_ZP_X; // lda $ff,x lda $ffff,x lda $ffffff,x
                                break;
                            case WHAT_Y: // lda $ff,y lda $ffff,y lda $ffffff,y
                                if (w==3) {//auto length
                                    if (val->type != T_NONE) {
                                        if (cnmemonic[ADR_ZP_Y]!=____ && !((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) {adr = (uint16_t)(val->u.num.val - dpage);w = 0;}
                                        else if (databank==((uval_t)val->u.num.val >> 16)) {adr = (uval_t)val->u.num.val;w = 1;}
                                    } else w=(cnmemonic[ADR_ADDR_Y]!=____);
                                } else if (val->type != T_NONE) {
                                        if (!w && !((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) adr = (uint16_t)(val->u.num.val - dpage);
                                        else if (w == 1 && databank == ((uval_t)val->u.num.val >> 16)) adr = (uval_t)val->u.num.val;
                                        else w=3;
                                } else if (w > 1) w = 3;
                                opr=ADR_ZP_Y-w;ln=w+1; // ldx $ff,y lda $ffff,y
                                break;
                            case WHAT_S:
                                adrgen = AG_BYTE; opr=ADR_ZP_S; // lda $ff,s
                                break;
                            case WHAT_R:
                                adrgen = AG_BYTE; opr=ADR_ZP_R; // lda $ff,r
                                break;
                            case WHAT_EOL:
                            case WHAT_COMMENT:
                                if (cnmemonic[ADR_MOVE]!=____) {
                                    struct value_s *val2;
                                    if (w==3) {//auto length
                                        if (val->type != T_NONE) {
                                            if (!((uval_t)val->u.num.val & ~(uval_t)0xff)) {adr = (uval_t)val->u.num.val << 8; w = 0;}
                                        } else w = 0;
                                    } else if (val->type != T_NONE) {
                                        if (!w && (!((uval_t)val->u.num.val & ~(uval_t)0xff))) adr = (uval_t)val->u.num.val << 8;
                                        else w = 3;
                                    } else if (w) w = 3; // there's no mvp $ffff or mvp $ffffff
                                    if ((val2 = get_val(T_UINT, NULL))) {
                                        if (val2 == &error_value) d = 0;
                                        if (!((uval_t)val2->u.num.val & ~(uval_t)0xff)) {adr |= (uint8_t)val2->u.num.val;}
                                        else w = 3;
                                    } else err_msg(ERROR_ILLEGAL_OPERA,NULL);
                                    ln = 2; opr=ADR_MOVE;
                                } else if (cnmemonic[ADR_BIT_ZP]!=____) {
                                    if (w==3) {//auto length
                                        if (val->type != T_NONE) {
                                            if (!((uval_t)val->u.num.val & ~(uval_t)7)) {longbranch = (uval_t)val->u.num.val << 4; w = 0;}
                                        } else w = 0;
                                    } else if (val->type != T_NONE) {
                                        if (!w && (!((uval_t)val->u.num.val & ~(uval_t)7))) longbranch = (uval_t)val->u.num.val << 4;
                                        else w = 3;
                                    } else if (w) w = 3; // there's no rmb $ffff,xx or smb $ffffff,xx
                                    if (w != 3) {
                                        if (!(val = get_val(T_UINT, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                        if (val == &error_value) d = 0;
                                        else if (val->type == T_NONE) {
                                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                            d = fixeddig = 0;
                                        } else {adr = val->u.num.val;d = 1;}
                                        adrgen = AG_ZP; opr=ADR_BIT_ZP; w = 3;
                                    }
                                } else if (cnmemonic[ADR_BIT_ZP_REL]!=____) {
                                    if (w==3) {//auto length
                                        if (val->type != T_NONE) {
                                            if (!((uval_t)val->u.num.val & ~(uval_t)7)) {longbranch = (uval_t)val->u.num.val << 4; w = 0;}
                                        } else w = 0;
                                    } else if (val->type != T_NONE) {
                                        if (!w && (!((uval_t)val->u.num.val & ~(uval_t)7))) longbranch = (uval_t)val->u.num.val << 4;
                                        else w = 3;
                                    } else if (w) w = 3; // there's no rmb $ffff,xx or smb $ffffff,xx
                                    if (w != 3) {
                                        if (!(val = get_val(T_UINT, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                        if (val == &error_value) d = 0;
                                        else if (val->type == T_NONE) {
                                            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                            d = fixeddig = 0;
                                        } else {adr = val->u.num.val;d = 1;}
                                        w = 3;
                                        if (val->type == T_NONE) w = 1;
                                        else if (!((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) {adr = (uint16_t)(val->u.num.val - dpage);w = 0;}
                                        if (w != 3) {
                                            uint32_t adr2=0;
                                            if (!(val = get_val(T_UINT, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                            if (val == &error_value) d = 0;
                                            else if (val->type == T_NONE) {
                                                if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                                d = fixeddig = 0;
                                            } else {adr2 = val->u.num.val;d = 1;}
                                            w=3;
                                            if (val->type != T_NONE) {
                                                if (((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff) {
                                                    if (fixeddig) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;
                                                } else {
                                                    adr2=(uint16_t)(adr2 - current_section->l_address - 3);
                                                    if (adr2>=0xFF80 || adr2<=0x007F) {
                                                        adr |= ((uint8_t)adr2) << 8; w = 1;
                                                    } else if (fixeddig) {
                                                        int dist = (int16_t)adr2;
                                                        dist += (dist < 0) ? 0x80 : -0x7f;
                                                        err_msg2(ERROR_BRANCH_TOOFAR, &dist, epoint);w = 1;
                                                    }
                                                }
                                            } else w = 1;
                                        }
                                        ln = 2;opr=ADR_BIT_ZP_REL;
                                    }
                                } else if (cnmemonic[ADR_REL]!=____) {
                                    struct star_s *s;
                                    int olabelexists;
                                    int_fast8_t min = 10;
                                    uint32_t joadr = adr;
                                    int_fast8_t joln = 1;
                                    uint8_t jolongbranch = longbranch;
                                    enum opr_e joopr = ADR_REL;
                                    enum errors_e err = ERROR_WUSER_DEFINED;
                                    s=new_star(vline+1);olabelexists=labelexists;
                                    int dist = 0;

                                    for (; c; c = !!(val = get_val(T_UINT, NULL))) {
                                        if (val != &error_value && val->type != T_NONE) {
                                            uint16_t oadr = val->u.num.val;
                                            adr = (uval_t)val->u.num.val;
                                            labelexists = olabelexists;

                                            if (labelexists && adr >= s->addr) {
                                                adr=(uint16_t)(adr - s->addr);
                                            } else {
                                                adr=(uint16_t)(adr - current_section->l_address - 2);labelexists=0;
                                            }
                                            ln=1;opr=ADR_REL;longbranch=0;
                                            if (((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff) {
                                                err = ERROR_CANT_CROSS_BA; continue;
                                            }
                                            if (adr<0xFF80 && adr>0x007F) {
                                                if (cnmemonic[ADR_REL_L] != ____) {
                                                    if (!labelexists) adr=(uint16_t)(adr-1);
                                                    opr=ADR_REL_L;
                                                    ln=2;
                                                } else if (arguments.longbranch && (cnmemonic[ADR_ADDR]==____)) {
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
                                                        } else if (cnmemonic[ADR_REL] == 0x82 && opcode==c65el02) {
                                                            int dist2 = (int16_t)adr; dist2 += (dist < 0) ? 0x80 : -0x7f;
                                                            if (!dist || ((dist2 > 0) ? dist2 : -dist2) < ((dist > 0) ? dist : -dist)) dist = dist2;
                                                            err = ERROR_BRANCH_TOOFAR;continue; //rer not a branch
                                                        } else {
                                                            longbranch=cnmemonic[ADR_REL]^0x4C;
                                                            adr=oadr;ln=2;
                                                        }
                                                    }
                                                    //err = ERROR___LONG_BRANCH;
                                                } else {
                                                    if (cnmemonic[ADR_ADDR] != ____) {
                                                        if (scpumode && !longbranchasjmp) {
                                                            longbranch = cnmemonic[ADR_REL]^0x82;
                                                            if (!labelexists) adr=(uint16_t)(adr-1);
                                                        } else {
                                                            adr=oadr;
                                                            opr=ADR_ADDR;
                                                        }
                                                        ln=2;
                                                    } else {
                                                        int dist2 = (int16_t)adr; dist2 += (dist < 0) ? 0x80 : -0x7f;
                                                        if (!dist || ((dist2 > 0) ? dist2 : -dist2) < ((dist > 0) ? dist : -dist)) dist = dist2;
                                                        err = ERROR_BRANCH_TOOFAR;continue;
                                                    }
                                                }
                                            } else {
                                                if (fixeddig) {
                                                    if (!longbranch && ((uint16_t)(current_section->l_address+2) & 0xff00)!=(oadr & 0xff00)) {
                                                        if (!allowslowbranch) {err=ERROR__BRANCH_CROSS;continue;}
                                                    }
                                                }
                                                if (cnmemonic[ADR_ADDR]!=____) {
                                                    if (adr==0) ln=-1;
                                                    else if (adr==1 && (cnmemonic[ADR_REL] & 0x1f)==0x10) {
                                                        ln=0;longbranch=0x20;adr=0x10000;
                                                    }
                                                }
                                            }
                                            if (ln < min) {
                                                min = ln;
                                                joopr = opr; joadr = adr; joln = ln; jolongbranch = longbranch;
                                            }
                                        }
                                    }
                                    opr = joopr; adr = joadr; ln = joln; longbranch = jolongbranch;
                                    if (fixeddig && min == 10) err_msg2(err, &dist, epoint);
                                    w=0;// bne
                                    if (olabelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                                        if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
                                        fixeddig=0;
                                    }
                                    s->addr = (star + 1 + ln) & all_mem;
                                }
                                else if (cnmemonic[ADR_REL_L]!=____) {
                                    if (w==3) {
                                        if (val->type != T_NONE) {
                                            if (!(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {
                                                adr = (uint16_t)(val->u.num.val-current_section->l_address-3); w = 1;
                                            } else {if (fixeddig) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;}
                                        } else w = 1;
                                    } else if (val->type != T_NONE) {
                                        if (w == 1 && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) adr = (uint16_t)(val->u.num.val-current_section->l_address-3);
                                        else w = 3; // there's no brl $ffffff!
                                    } else if (w != 1) w = 3;
                                    opr=ADR_REL_L; ln = 2; //brl
                                }
                                else if (cnmemonic[ADR_LONG]==0x5C) {
                                    if (w==3) {//auto length
                                        if (val->type != T_NONE) {
                                            if (cnmemonic[ADR_ADDR]!=____ && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {adr = (uval_t)val->u.num.val;w = 1;}
                                            else if (!((uval_t)val->u.num.val & ~(uval_t)0xffffff)) {adr = (uval_t)val->u.num.val; w = 2;}
                                        } else w = (cnmemonic[ADR_ADDR]==____) + 1;
                                    } else if (val->type != T_NONE) {
                                        if (w == 1 && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) adr = (uval_t)val->u.num.val;
                                        else if (w == 2 && !((uval_t)val->u.num.val & ~(uval_t)0xffffff)) adr = (uval_t)val->u.num.val;
                                        else w = 3;
                                    }
                                    opr=ADR_ZP-w;ln=w+1; // jml
                                }
                                else if (cnmemonic[ADR_ADDR]==0x20) {
                                    if ((((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {
                                        if (fixeddig) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;
                                    } else adrgen = AG_PB;
                                    opr=ADR_ADDR; ln = 2; // jsr $ffff
                                } else {
                                    adrgen = AG_DB3; opr=ADR_ZP; // lda $ff lda $ffff lda $ffffff
                                }
                                break;
                            default: err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                            }
                            break;
                        case 2:
                            switch (what(&prm)) {
                            case WHAT_SZ:
                                if ((wht=what(&prm))!=WHAT_Y) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                adrgen = AG_BYTE; opr=ADR_ZP_S_I_Y; // lda ($ff,s),y
                                break;
                            case WHAT_RZ:
                                if ((wht=what(&prm))!=WHAT_Y) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                adrgen = AG_BYTE; opr=ADR_ZP_R_I_Y; // lda ($ff,r),y
                                break;
                            case WHAT_XZ:
                                if (cnmemonic[ADR_ADDR_X_I]==0x7C || cnmemonic[ADR_ADDR_X_I]==0xFC || cnmemonic[ADR_ADDR_X_I]==0x23) {// jmp ($ffff,x) jsr ($ffff,x)
                                    adrgen = AG_PB; opr=ADR_ADDR_X_I; // jmp ($ffff,x)
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_X_I; // lda ($ff,x)
                                }
                                break;
                            default: err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                            }
                            break;
                        case 3:
                            switch (what(&prm)) {
                            case WHAT_Y:
                                adrgen = AG_ZP; opr=ADR_ZP_I_Y; // lda ($ff),y
                                break;
                            case WHAT_Z:
                                adrgen = AG_ZP; opr=ADR_ZP_I_Z; // lda ($ff),z
                                break;
                            case WHAT_EOL:
                            case WHAT_COMMENT:
                                if (cnmemonic[ADR_ADDR_I]==0x6C || cnmemonic[ADR_ADDR_I]==0x22) {// jmp ($ffff), jsr ($ffff)
                                    if (fixeddig && opcode!=c65816 && opcode!=c65c02 && opcode!=cr65c02 && opcode!=cw65c02 && opcode!=c65ce02 && opcode!=c65el02 && !(~adr & 0xff)) err_msg(ERROR______JUMP_BUG,NULL);//jmp ($xxff)
                                    adrgen = AG_B0; opr=ADR_ADDR_I; // jmp ($ffff)
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_I; // lda ($ff)
                                }
                                break;
                            default: err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                            }
                            break;
                        case 4:
                            switch (what(&prm)) {
                            case WHAT_Y:
                                adrgen = AG_ZP; opr=ADR_ZP_LI_Y; // lda [$ff],y
                                break;
                            case WHAT_EOL:
                            case WHAT_COMMENT:
                                if (cnmemonic[ADR_ADDR_LI]==0xDC) { // jmp [$ffff]
                                    adrgen = AG_B0; opr=ADR_ADDR_LI; // jmp [$ffff]
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_LI; // lda [$ff]
                                }
                                break;
                            default: err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                            }
                            break;
                        default: err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                        }
                        eval_finish();
                    }
                    switch (adrgen) {
                    case AG_ZP: // zero page address only
                        if (w==3) {//auto length
                            if (val->type == T_NONE) w = 0;
                            else if (!((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) {adr = (uint16_t)(val->u.num.val - dpage);w = 0;}
                        } else if (val->type != T_NONE) {
                            if (!w && !((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) adr = (uint16_t)(val->u.num.val - dpage);
                            else w=3; // there's no $ffff] or $ffffff!
                        } else if (w) w = 3;
                        ln = 1;
                        break;
                    case AG_B0: // bank 0 address only
                        if (w==3) {
                            if (val->type == T_NONE) w = 1;
                            else if (!((uval_t)val->u.num.val & ~(uval_t)0xffff)) {adr = (uint16_t)val->u.num.val; w = 1;}
                        } else if (val->type != T_NONE) {
                            if (w == 1 && !((uval_t)val->u.num.val & ~(uval_t)0xffff)) adr = (uint16_t)val->u.num.val;
                            else w=3; // there's no jmp $ffffff!
                        } else if (w != 1) w = 3;
                        ln = 2;
                        break;
                    case AG_PB: // address in program bank
                        if (w==3) {
                            if (val->type != T_NONE) {
                                if (!(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {adr = (uint16_t)val->u.num.val; w = 1;}
                            } else w = 1;
                        } else if (val->type != T_NONE) {
                            if (w == 1 && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) adr = (uint16_t)val->u.num.val;
                            else w = 3; // there's no jsr ($ffff,x)!
                        } else if (w != 1) w = 3;
                        ln = 2;
                        break;
                    case AG_BYTE: // byte only
                        if (w==3) {//auto length
                            if (val->type != T_NONE) {
                                if (!((uval_t)val->u.num.val & ~(uval_t)0xff)) {adr = (uval_t)val->u.num.val; w = 0;}
                            } else w = 0;
                        } else if (val->type != T_NONE) {
                            if (!w && !((uval_t)val->u.num.val & ~(uval_t)0xff)) adr = (uval_t)val->u.num.val;
                            else w = 3;
                        } else if (w) w = 3; // there's no lda ($ffffff,s),y or lda ($ffff,s),y!
                        ln = 1;
                        break;
                    case AG_DB3: // 3 choice data bank
                        if (w==3) {//auto length
                            if (val->type != T_NONE) {
                                if (cnmemonic[opr]!=____ && !((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) {adr = (uint16_t)(val->u.num.val - dpage);w = 0;}
                                else if (cnmemonic[opr - 1]!=____ && databank==((uval_t)val->u.num.val >> 16)) {adr = (uval_t)val->u.num.val;w = 1;}
                                else if (!((uval_t)val->u.num.val & ~(uval_t)0xffffff)) {adr = (uval_t)val->u.num.val; w = 2;}
                            } else w=(cnmemonic[opr - 1]!=____);
                        } else if (val->type != T_NONE) {
                            if (!w && !((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) adr = (uint16_t)(val->u.num.val - dpage);
                            else if (w == 1 && databank == ((uval_t)val->u.num.val >> 16)) adr = (uval_t)val->u.num.val;
                            else if (w == 2 && !((uval_t)val->u.num.val & ~(uval_t)0xffffff)) adr = (uval_t)val->u.num.val;
                            else w = 3;
                        }
                        opr -= w;ln = w + 1;
                        break;
                    case AG_NONE:
                        break;
                    }

                    if (d) {
                        if (w==3) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                        if ((cod=cnmemonic[opr])==____ && (prm || opr!=ADR_IMMEDIATE)) { // 0x69 hack
                            labelname[0]=mnemonic[mnem] >> 16;
                            labelname[1]=mnemonic[mnem] >> 8;
                            labelname[2]=mnemonic[mnem];
                            labelname[3]=0;
                            if ((tmp2=find_label(labelname)) && (tmp2->type == L_MACRO || tmp2->type == L_SEGMENT)) {
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

                        if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t":".%06" PRIaddress " ",(current_section->address-ln-1) & all_mem);
                        if (current_section->dooutput) {
                            if (ln>=0) {
                                fprintf(flist," %02x", cod ^ longbranch ^ outputeor);
                                for (i=0;i<(unsigned)ln;i++) {fprintf(flist," %02x",(uint8_t)temp ^ outputeor);temp>>=8;}
                            }
                            if (ln<2) putc('\t',flist);
                            putc('\t',flist);
                            if (arguments.monitor) {
                                for (i=0;i<3;i++) putc(mnemonic[mnem] >> (16-8*i),flist);

                                switch (opr) {
                                case ADR_IMPLIED: putc('\t', flist); break;
                                case ADR_ACCU: fputs(" a\t", flist); break;
                                case ADR_IMMEDIATE: {
                                    if (ln==1) fprintf(flist," #$%02x",(uint8_t)adr);
                                    else fprintf(flist," #$%04x",(uint16_t)adr);
                                    break;
                                }
                                case ADR_LONG: fprintf(flist," $%06x",(uint32_t)(adr&0xffffff)); break;
                                case ADR_ADDR: {
                                    if (cnmemonic[ADR_ADDR]==0x20 || cnmemonic[ADR_ADDR]==0x4c)
                                        fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,((uint16_t)adr) | (current_section->l_address & 0xff0000));
                                    else fprintf(flist,databank?" $%06x":" $%04x",(uint16_t)adr | (databank << 16));
                                    break;
                                }
                                case ADR_ZP: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x\t":" $%04x",(uint16_t)(adr+dpage)); break;
                                case ADR_BIT_ZP: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" %d,$%02x":" %d,$%04x",((cod ^ longbranch) >> 4) & 7,(uint16_t)(adr+dpage)); break;
                                case ADR_LONG_X: fprintf(flist," $%06x,x",(uint32_t)(adr&0xffffff)); break;
                                case ADR_ADDR_X: fprintf(flist,databank?" $%06x,x":" $%04x,x",(uint16_t)adr | (databank << 16)); break;
                                case ADR_ZP_X: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x,x":" $%04x,x",(uint16_t)(adr+dpage)); break;
                                case ADR_ADDR_X_I: fprintf(flist,(current_section->l_address&0xff0000)?" ($%06" PRIaddress ",x)":" ($%04" PRIaddress ",x)",((uint16_t)adr) | (current_section->l_address&0xff0000)); break;
                                case ADR_ZP_X_I: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x,x)":" ($%04x,x)",(uint16_t)(adr+dpage)); break;
                                case ADR_ZP_S: fprintf(flist," $%02x,s",(uint8_t)adr); break;
                                case ADR_ZP_S_I_Y: fprintf(flist," ($%02x,s),y",(uint8_t)adr); break;
                                case ADR_ZP_R: fprintf(flist," $%02x,r",(uint8_t)adr); break;
                                case ADR_ZP_R_I_Y: fprintf(flist," ($%02x,r),y",(uint8_t)adr); break;
                                case ADR_ADDR_Y: fprintf(flist,databank?" $%06x,y":" $%04x,y",(uint16_t)adr | (databank << 16)); break;
                                case ADR_ZP_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" $%02x,y":" $%04x,y",(uint16_t)(adr+dpage)); break;
                                case ADR_ZP_LI_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" [$%02x],y":" [$%04x],y",(uint16_t)(adr+dpage)); break;
                                case ADR_ZP_I_Y: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x),y":" ($%04x),y",(uint16_t)(adr+dpage)); break;
                                case ADR_ZP_I_Z: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x),z":" ($%04x),z",(uint16_t)(adr+dpage)); break;
                                case ADR_ADDR_LI: fprintf(flist," [$%04x]",(uint16_t)adr); break;
                                case ADR_ZP_LI: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" [$%02x]":" [$%04x]",(uint16_t)(adr+dpage)); break;
                                case ADR_ADDR_I: fprintf(flist," ($%04x)",(uint16_t)adr); break;
                                case ADR_ZP_I: fprintf(flist,((uint16_t)(adr+dpage)<0x100)?" ($%02x)":" ($%04x)",(uint16_t)(adr+dpage)); break;
                                case ADR_REL: {
                                    if (ln==1) fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,(uint16_t)(((int8_t)adr)+current_section->l_address) | (current_section->l_address & 0xff0000));
                                    else if (ln==2) {
                                        if ((cod ^ longbranch)==0x4C)
                                            fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,((uint16_t)adr) | (current_section->l_address & 0xff0000));
                                        else
                                            fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,(uint16_t)(adr+current_section->l_address) | (current_section->l_address & 0xff0000));
                                    }
                                    else {
                                        if ((uint16_t)adr==0x4C03)
                                            fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,((uint16_t)(adr >> 16)) | (current_section->l_address & 0xff0000));
                                        else
                                            fprintf(flist,(current_section->l_address&0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,(uint16_t)((adr >> 16)+current_section->l_address) | (current_section->l_address & 0xff0000));
                                    }
                                    break;
                                }
                                case ADR_BIT_ZP_REL:
                                    fprintf(flist,((uint16_t)(((uint8_t)adr)+dpage)<0x100)?" %d,$%02x":" %d,$%04x",((cod ^ longbranch) >> 4) & 7,(uint16_t)((uint8_t)adr)+dpage);
                                    fprintf(flist,(current_section->l_address&0xff0000)?",$%06" PRIaddress:",$%04" PRIaddress,(uint16_t)(((int8_t)(adr >> 8))+current_section->l_address) | (current_section->l_address & 0xff0000));
                                    break;
                                case ADR_REL_L: fprintf(flist,(current_section->l_address & 0xff0000)?" $%06" PRIaddress:" $%04" PRIaddress,(uint16_t)(adr+current_section->l_address) | (current_section->l_address & 0xff0000)); break;
                                case ADR_MOVE: fprintf(flist," $%02x,$%02x",(uint8_t)(adr >> 8),(uint8_t)adr);
                                case ADR_LEN: break;// not an addressing mode
                                }
                            } else if (arguments.source) putc('\t',flist);
                        } else if (arguments.source) fputs("\t\t\t", flist);
                        if (arguments.source) {
                            putc('\t', flist);printllist(flist);
                        } else putc('\n',flist);
                    }
                    break;
                }
                if ((tmp2=find_label(labelname)) && (tmp2->type == L_MACRO || tmp2->type == L_SEGMENT)) goto as_macro;
            }            // fall through
        default: if (waitfor[waitforp].skip & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr; //skip things if needed
        }
    finish:
        ignore();if (here() && here()!=';' && (waitfor[waitforp].skip & 1)) err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    breakerr:
        if (newlabel) set_size(newlabel, current_section->address - oaddr);
        continue;
    }

    while (oldwaitforp < waitforp) {
        const char *msg = NULL;
        line_t os = sline;
        sline = waitfor[waitforp].line;
        switch (waitfor[waitforp].what) {
        case 'e':
        case 'f': msg = ".FI"; break;
	case 'P':
        case 'p': msg = ".ENDP"; break;
        case 'M':
        case 'm': msg = ".ENDM"; break;
        case 'N':
        case 'n': msg = ".NEXT"; break;
        case 'r': msg = ".PEND"; break;
	case 'B':
        case 'b': msg = ".BEND"; break;
        case 'c': msg = ".ENDC"; break;
        case 'S':
        case 's': msg = ".ENDS"; break;
        case 'T':
        case 't': msg = ".SEND"; break;
        case 'U':
        case 'u': msg = ".ENDU"; break;
        case 'L':
        case 'l': msg = ".HERE"; break;
        }
        if (msg) err_msg2(ERROR______EXPECTED, msg, waitfor[waitforp].epoint);
        waitforp--; sline = os;
    }
    return;
}

#ifdef _WIN32
int main2(int argc, char *argv[]);

int wmain(int argc, wchar_t *argv2[]) {
    int i, r;
    UINT oldcodepage = GetConsoleOutputCP();
    UINT oldcodepage2 = GetConsoleCP();

    SetConsoleCP(65001);
    SetConsoleOutputCP(65001);

    char **argv = malloc(sizeof(char *)*argc);
    for (i = 0; i < argc; i++) {
	uint32_t c = 0, lastchar;
	wchar_t *p = argv2[i];
	uint8_t *c2;

	while (*p) p++;
	c2 = malloc((p - argv2[i])*4/sizeof(wchar_t)+1);
	if (!c2) exit(1);
	p = argv2[i];
	argv[i] = (char *)c2;

	while (*p) {
	    lastchar = c;
	    c = *p++;
	    if (c >= 0xd800 && c < 0xdc00) {
		if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
		c = 0xfffd;
	    } else if (c >= 0xdc00 && c < 0xe000) {
		if (lastchar >= 0xd800 && lastchar < 0xdc00) {
		    c ^= 0x360dc00 ^ (lastchar << 10);
		    c += 0x10000;
		} else
		    c = 0xfffd;
	    } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
		c = 0xfffd;
	    }
	    if (c && c < 0x80) *c2++ = c; else c2 = utf8out(c, c2);
	}
	*c2++ = 0;
	argv[i] = realloc(argv[i], (char *)c2 - argv[i]);
	if (!argv[i]) exit(1);
    }
    r = main2(argc, argv);

    for (i = 0; i < argc; i++) free(argv[i]);
    free(argv);

    SetConsoleCP(oldcodepage2);
    SetConsoleOutputCP(oldcodepage);
    return r;
}

int main2(int argc, char *argv[]) {
#else
int main(int argc, char *argv[]) {
#endif
    time_t t;
    int optind, i;
    struct file_s *fin;

    tinit();

    fin = openfile("", "", 0, NULL);
    optind = testarg(argc,argv, fin);
    init_encoding(arguments.toascii);

    if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1]))
        puts("64tass Turbo Assembler Macro V" VERSION "\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");

    /* assemble the input file(s) */
    do {
        if (pass++>MAX_PASS) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        restart_mem();
        for (i = optind - 1; i<argc; i++) {
            set_cpumode(arguments.cpumode);
            star=databank=dpage=longaccu=longindex=0;wrapwarn=wrapwarn2=0;actual_encoding=new_encoding("none");wrapwarn2=0;
            allowslowbranch=1;
            waitfor[waitforp=0].what=0;waitfor[0].skip=1;sline=vline=0;outputeor=0;forwr=backr=0;
            current_context=&root_label;
            current_section=&root_section;
            current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
            current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
            current_section->dooutput=1;
            current_section->structrecursion=0;
            current_section->logicalrecursion=0;
            current_section->unionmode=0;
            macro_parameters.p = 0;
            /*	listing=1;flist=stderr;*/
            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0; cfile = fin;
                star_tree=&fin->star;
                reffile=cfile->uid;
                compile();
                exitfile();
                restart_mem();
                continue;
            }
            memjmp(current_section->address);
            cfile = openfile(argv[i], "", 0, NULL);
            enterfile(argv[i],0);
            if (cfile) {
                cfile->p = 0;
                star_tree=&cfile->star;
                reffile=cfile->uid;
                compile();
                closefile(cfile);
            }
            exitfile();
        }
        if (errors) {memcomp();status();return 1;}
    } while (!fixeddig || pass==1);

    /* assemble again to create listing */
    if (arguments.list) {
        char **argv2 = argv;
        int argc2 = argc;
        listing=1;
        if (arguments.list[0] == '-' && !arguments.list[1]) {
            flist = stdout;
        } else {
            if (!(flist=file_open(arguments.list,"wt"))) err_msg_file(ERROR_CANT_DUMP_LST, arguments.list);
        }
	fputs("\n; 64tass Turbo Assembler Macro V" VERSION " listing file\n;", flist);
        if (*argv2) {
            char *new = strrchr(*argv2, '/');
            if (new) *argv2 = new + 1;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
            new = strrchr(*argv2, '\\');
            if (new) *argv2 = new + 1;
#endif
        }
        while (argc2--) fprintf(flist," %s", *argv2++);
	time(&t); fprintf(flist,"\n; %s",ctime(&t));

        pass++;
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        restart_mem();
        for (i = optind - 1; i<argc; i++) {
            if (i >= optind) {fprintf(flist,"\n;******  Processing input file: %s\n", argv[i]);}
            lastl=LIST_NONE;
            set_cpumode(arguments.cpumode);
            star=databank=dpage=longaccu=longindex=0;wrapwarn=wrapwarn2=0;actual_encoding=new_encoding("none");wrapwarn2=0;
            allowslowbranch=1;
            waitfor[waitforp=0].what=0;waitfor[0].skip=1;sline=vline=0;outputeor=0;forwr=backr=0;
            current_context=&root_label;
            current_section=&root_section;
            current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
            current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
            current_section->dooutput=1;
            current_section->structrecursion=0;
            current_section->logicalrecursion=0;
            current_section->unionmode=0;
            macro_parameters.p = 0;

            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0; cfile = fin;
                star_tree=&fin->star;
                reffile=cfile->uid;
                compile();
                exitfile();
                restart_mem();
                continue;
            }
            memjmp(current_section->address);

            cfile = openfile(argv[i], "", 0, NULL);
            enterfile(argv[i],0);
            if (cfile) {
                cfile->p = 0;
                star_tree=&cfile->star;
                reffile=cfile->uid;
                compile();
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

    output_mem(scpumode);
    status();
    return 0;
}

#ifdef __MINGW32__

#include <shellapi.h>

int main(void)
{
  LPWSTR commandLine = GetCommandLineW();
  int argcw = 0;
  LPWSTR *argvw = CommandLineToArgvW(commandLine, &argcw);
  if (!argvw)
    return 127;

  int result = wmain(argcw, argvw);
  LocalFree(argvw);
  return result;
}
#endif /* __MINGW32__ */
