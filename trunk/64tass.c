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
#include "macro.h"

static const uint32_t *mnemonic;    /* mnemonics */
static const uint8_t *opcode;       /* opcodes */
static struct value_s none_value = {T_NONE, 0, {{0, 0}}};
static struct value_s new_value = {T_NONE, 0, {{0, 0}}};
static struct value_s null_tuple = {T_TUPLE, 0, {{0, 0}}};

line_t sline, vline;      /* current line */
static address_t all_mem, all_mem2;
uint8_t pass=0;         /* pass */
static int listing=0;   /* listing */
address_t star=0;
const uint8_t *pline, *llist;   /* current line data */
linepos_t lpoint;               /* position in current line */
static char path[linelength];   /* path */
static FILE* flist = NULL;      /* listfile */
static enum lastl_e lastl;
static int longaccu=0,longindex=0,scpumode=0,dtvmode=0;
static uint8_t databank=0;
static uint16_t dpage=0;
int fixeddig;
static int allowslowbranch=1;
static int longbranchasjmp=0;
static uint8_t outputeor = 0; /* EOR value for final output (usually 0, except changed by .eor) */

static size_t waitfor_p, waitfor_len;
static struct waitfor_s {
    enum wait_e what;
    line_t line;
    linepos_t epoint;
    address_t addr;
    address_t laddr;
    struct label_s *label;
    size_t memp, membp;
    struct section_s *section;
    struct value_s *val;
    uint8_t skip;
} *waitfors, *waitfor, *prevwaitfor;

static unsigned int last_mnem;

uint16_t reffile;
uint32_t backr, forwr;
struct avltree *star_tree = NULL;

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
    "\x53" "case",
    "\x4d" "cdef",
    "\x31" "cerror",
    "\x06" "char",
    "\x35" "check",
    "\x1a" "comment",
    "\x36" "cpu",
    "\x32" "cwarn",
    "\x27" "databank",
    "\x54" "default",
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
    "\x51" "endf",
    "\x2c" "endif",
    "\x10" "endm",
    "\x1d" "endp",
    "\x45" "ends",
    "\x55" "endswitch",
    "\x48" "endu",
    "\x3f" "eor",
    "\x24" "error",
    "\x15" "fi",
    "\x29" "fill",
    "\x11" "for",
    "\x50" "function",
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
    "\x52" "switch",
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
    CMD_DSECTION, CMD_SEND, CMD_CDEF, CMD_EDEF, CMD_BINCLUDE, CMD_FUNCTION,
    CMD_ENDF, CMD_SWITCH, CMD_CASE, CMD_DEFAULT, CMD_ENDSWITCH
};

/* --------------------------------------------------------------------------- */

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
    destroy_eval();
    tfree();
    free_macro();
    free(waitfors);
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

void new_waitfor(enum wait_e what, linepos_t epoint) {
    waitfor_p++;
    if (waitfor_p >= waitfor_len) {
        waitfor_len += 8;
        waitfors = realloc(waitfors, waitfor_len * sizeof(struct waitfor_s));
        if (!waitfors) err_msg_out_of_memory();
    }
    waitfor = &waitfors[waitfor_p];
    prevwaitfor = waitfor_p ? &waitfors[waitfor_p - 1] : waitfor;
    waitfor->what = what;
    waitfor->line = sline;
    waitfor->epoint = epoint;
    waitfor->label = NULL;
    waitfor->val = NULL;
    waitfor->skip = prevwaitfor->skip;
}

void reset_waitfor(void) {
    linepos_t lpos = {0,0};
    waitfor_p = -1;
    new_waitfor(W_NONE, lpos);
    waitfor->skip=1;
    prevwaitfor = waitfor;
}

int close_waitfor(enum wait_e what) {
    if (waitfor->what == what) {
        if (waitfor->val) val_destroy(waitfor->val);
        waitfor_p--;
        waitfor = &waitfors[waitfor_p];
        prevwaitfor = waitfor_p ? &waitfors[waitfor_p - 1] : waitfor;
        return 1;
    }
    return 0;
}

static void set_size(struct label_s *var, size_t size, size_t memp, size_t membp) {
    size &= all_mem2;
    if (var->value->u.code.size != size) {
        var->value->u.code.size = size;
        if (var->value->u.code.pass) {
            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&var->name, var->epoint);
            fixeddig = 0;
        }
    }
    var->value->u.code.pass = pass;
    var->value->u.code.memp = memp;
    var->value->u.code.membp = membp;
}

/* --------------------------------------------------------------------------- */
/*
 * Skip memory
 */
static void memskip(address_t db) {
    if (current_section->moved) {
        if (current_section->address < current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
        if (current_section->wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);current_section->wrapwarn=0;}
        current_section->moved = 0;
    }
    if (current_section->wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);current_section->wrapwarn2=0;}
    if (db > (~current_section->l_address & all_mem)) {
        if (db - 1 + current_section->l_address == all_mem) {
            current_section->wrapwarn2 = 1;
            current_section->l_address = 0;
        } else {
            current_section->l_address = (current_section->l_address + db) & all_mem;
            err_msg(ERROR___BANK_BORDER,NULL);current_section->wrapwarn2=0;
        }
    } else current_section->l_address += db;
    if (db > (~current_section->address & all_mem2)) {
        if (db - 1 + current_section->address == all_mem2) {
            current_section->wrapwarn = current_section->moved = 1;
            if (current_section->end <= all_mem2) current_section->end = all_mem2 + 1;
            current_section->address = 0;
        } else {
            if (current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
            if (current_section->end <= all_mem2) current_section->end = all_mem2 + 1;
            current_section->moved = 0;
            current_section->address = (current_section->address + db) & all_mem2;
            err_msg(ERROR_TOP_OF_MEMORY,NULL);current_section->wrapwarn=0;
        }
    } else current_section->address += db;
    memjmp(current_section->address);
}

/* --------------------------------------------------------------------------- */
/*
 * output one byte
 */
static void pokeb(uint8_t byte)
{
    if (current_section->moved) {
        if (current_section->address < current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
        if (current_section->wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);current_section->wrapwarn=0;}
        current_section->moved = 0;
    }
    if (current_section->wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);current_section->wrapwarn2=0;}
    if (current_section->dooutput) write_mem(byte ^ outputeor);
    current_section->address++;current_section->l_address++;
    if (current_section->address & ~all_mem2) {
        current_section->wrapwarn = current_section->moved = 1;
        if (current_section->end <= all_mem2) current_section->end = all_mem2 + 1;
        current_section->address = 0;
        memjmp(current_section->address);
    }
    if (current_section->l_address & ~all_mem) {
        current_section->wrapwarn2 = 1;
        current_section->l_address = 0;
    }
}

static int lookup_opcode(const char *s) {
    uint8_t s2,s3, ch;
    int32_t s4;
    unsigned int also,felso,elozo, no;
    uint32_t name;

    ch=lowcase(s[0]);
    s2=lowcase(s[1]);
    s3=lowcase(s[2]);
    name = (ch << 16) | (s2 << 8) | s3;
    also = 0;
    no = (felso=last_mnem)/2;
    for (;;) {  /* do binary search */
        if (!(s4=name-mnemonic[no]))
            return no;
            elozo=no;
        if (elozo==(no=((s4>0) ? (felso+(also=no)) : (also+(felso=no)) )/2)) break;
    }
    return -1;
}

/* --------------------------------------------------------------------------- */
static int what(int *tempno) {
    char ch;

    ignore();
    switch (ch=whatis[(int)here()]) {
    case WHAT_COMMAND:
	{
            char cmd[20];
            unsigned int no, also, felso, elozo;
            size_t l;
            int s4;
            lpoint.pos++;
            for (also = l = 0; l + 1 < sizeof(cmd); l++) {
                cmd[l]=pline[lpoint.pos + l] | 0x20;
                if (!pline[lpoint.pos + l] || cmd[l] < 'a' || cmd[l] > 'z') {
                    cmd[l]=(cmd[l] >= '0' && cmd[l] <= '9') || cmd[l]=='_';
                    l++;break;
                }
            }
            l--;
            if (!cmd[l]) {
                felso=sizeof(command)/sizeof(command[0]);
                no=felso/2;
                for (;;) {  /* do binary search */
                    if (!(s4=strcmp(cmd, command[no] + 1))) {
                        lpoint.pos += l;
                        no = (uint8_t)command[no][0];
                        if (no==CMD_ENDIF) no=CMD_FI;
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
	lpoint.pos++;
        ignore();
	switch (get() | 0x20) {
	case 'y': ignore();return WHAT_Y;
	case 'z': ignore();return WHAT_Z;
	case 'x': ignore();if (here()==')') {lpoint.pos++;ignore();return WHAT_XZ;} else return WHAT_X;
	case 's': ignore();if (here()==')') {lpoint.pos++;ignore();return WHAT_SZ;} else return WHAT_S;
	case 'r': ignore();if (here()==')') {lpoint.pos++;ignore();return WHAT_RZ;} else return WHAT_R;
	default: lpoint.pos--;return WHAT_COMA;
	}
    case WHAT_CHAR:
    case WHAT_LBL:
            *tempno=1;return WHAT_EXPRESSION;
    case WHAT_EXPRESSION:/* tempno=1 if label, 0 if expression */
	    *tempno=0;return WHAT_EXPRESSION;
    case WHAT_COMMENT:
    case WHAT_EOL:return ch;
    default:lpoint.pos++;return ch;
    }
}

static int get_hack(void) {
    int q=1;
    unsigned int i=0, i2;
    i2 = i;
    ignore();
    if (here()=='\"') {lpoint.pos++;q=0;}
    while (here() && (here()!=';' || !q) && (here()!='\"' || q) && i<sizeof(path)) path[i++]=get();
    if (i>=sizeof(path) || (!q && here()!='\"')) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    if (!q) lpoint.pos++; else while (i && (path[i-1]==0x20 || path[i-1]==0x09)) i--;
    path[i]=0;
    ignore();
    if (i <= i2) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    return 0;
}

/* ------------------------------------------------------------------------------ */

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
    tmp->defpass = pass;
    if (val_same(tmp->value, val)) return;
    val_replace(&tmp->value, val);
    if (tmp->usepass < pass) return;
    if (fixeddig && !fix && pass > MAX_PASS) err_msg_cant_calculate(&tmp->name, tmp->epoint);
    fixeddig=fix;
}

struct value_s *compile(struct file_s *cfile)
{
    int wht,w;
    int prm = 0;
    struct value_s *val;

    struct label_s *newlabel = NULL;
    size_t newmemp = 0, newmembp = 0;
    struct label_s *tmp2 = NULL, *mycontext;
    address_t oaddr = 0;
    struct value_s *retval = NULL;

    size_t oldwaitforp = waitfor_p;
    unsigned wasref;
    int nobreak = 1;
    str_t labelname;
    char reflabel[100];
    linepos_t epoint;

    while (cfile->len != cfile->p && nobreak) {
        pline = cfile->data + cfile->p; lpoint.pos = 0; lpoint.upos = 0; sline++;vline++; cfile->p += strlen((const char *)pline) + 1;
        mtranslate(); /* expand macro parameters, if any */
        llist = pline;
        star=current_section->l_address;newlabel = NULL;
        labelname.len=wasref=0;ignore();epoint=lpoint; mycontext = current_context;
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
            int islabel = 0;
            if (!prm) {
                if (here()=='-' || here()=='+') {
                    char c = here();
                    lpoint.pos++;if (here()!=0x20 && here()!=0x09 && here()!=';' && here()) goto baj;
                    if (c == '-') {
                        sprintf(reflabel,"-%x-%x", reffile, backr++);
                    } else {
                        sprintf(reflabel,"+%x+%x", reffile, forwr++);
                    }
                    labelname.data = (const uint8_t *)reflabel;labelname.len = strlen(reflabel);
                    islabel = 1;goto hh;
                }
            baj:
                if (waitfor->skip & 1) err_msg2(ERROR_GENERL_SYNTAX,NULL, epoint);
                goto breakerr;
            } /* not label */
            for (;;) {
                labelname.data = pline + lpoint.pos; labelname.len = get_label();
                if (here() != '.') {
                    break;
                }
                if (mycontext == current_context) tmp2 = find_label(&labelname);
                else tmp2 = find_label2(&labelname, mycontext);
                if (!tmp2) {err_msg_not_defined(&labelname, epoint); goto breakerr;}
                if (!tmp2->nested) {
                    err_msg_not_defined(&labelname, epoint); goto breakerr;
                }
                mycontext = tmp2;
                lpoint.pos++; islabel = 1; epoint = lpoint;
            }
            if (here()==':') {islabel = 1; lpoint.pos++;}
            if (!islabel && labelname.len == 3 && (prm=lookup_opcode((const char *)labelname.data))>=0) {
                if (waitfor->skip & 1) goto as_opcode; else continue;
            }
        hh:
            if (!(waitfor->skip & 1)) {wht=what(&prm);goto jn;} /* skip things if needed */
            if ((wht=what(&prm))==WHAT_EQUAL) { /* variable */
                struct label_s *label;
                int labelexists;
                int oldreferenceit = referenceit;
                label = find_label2(&labelname, mycontext);
                if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                referenceit &= label ? label->ref : 1;
                val = get_vals_tuple(T_IDENTREF);
                referenceit = oldreferenceit;
                if (!val) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (label) labelexists = 1;
                else label = new_label(&labelname, mycontext, L_CONST, &labelexists);
                oaddr=current_section->address;
                if (listing && flist && arguments.source && label->ref) {
                    if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                    if (type_is_int(val->type)) {
                        fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.num.val);
                    } else if (val->type == T_CODE) {
                        fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.code.addr);
                    } else {
                        fputs("=\t\t\t\t\t", flist);
                    }
                    printllist(flist);
                }
                label->ref = 0;
                if (labelexists) {
                    if (label->type != L_CONST || pass==1) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                    else {
                        label->requires = current_section->requires;
                        label->conflicts = current_section->conflicts;
                        var_assign(label, val, 0);
                        val_destroy(val);
                    }
                } else {
                    label->requires = current_section->requires;
                    label->conflicts = current_section->conflicts;
                    label->usepass = pass;
                    label->defpass = pass;
                    label->value = val;
                    label->file = cfile;
                    label->sline = sline;
                    label->epoint = epoint;
                }
                goto finish;
            }
            if (wht==WHAT_COMMAND) {
                switch (prm) {
                case CMD_VAR: /* variable */
                    {
                        struct label_s *label;
                        int labelexists;
                        int oldreferenceit = referenceit;
                        label=find_label2(&labelname, mycontext);
                        if (!get_exp(&w, 0)) goto breakerr; /* ellenorizve. */
                        referenceit &= label ? label->ref : 1;
                        val = get_vals_tuple(T_IDENTREF);
                        referenceit = oldreferenceit;
                        if (!val) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (label) labelexists = 1;
                        else label = new_label(&labelname, mycontext, L_VAR, &labelexists);
                        oaddr=current_section->address;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                            if (type_is_int(val->type)) {
                                fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.num.val);
                            } else if (val->type == T_CODE) {
                                fprintf(flist,"=%" PRIxval "\t\t\t\t\t",(uval_t)val->u.code.addr);
                            } else {
                                fputs("=\t\t\t\t\t", flist);
                            }
                            printllist(flist);
                        }
                        if (labelexists) {
                            if (label->defpass != pass) label->ref=0;
                            if (label->type != L_VAR) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                            else {
                                label->requires=current_section->requires;
                                label->conflicts=current_section->conflicts;
                                var_assign(label, val, fixeddig);
                                val_destroy(val);
                            }
                        } else {
                            label->requires = current_section->requires;
                            label->conflicts = current_section->conflicts;
                            label->usepass = pass;
                            label->defpass = pass;
                            label->value = val;
                            label->file = cfile;
                            label->sline = sline;
                            label->epoint = epoint;
                        }
                        goto finish;
                    }
                case CMD_LBL:
                    { /* label */
                        struct label_s *label;
                        int labelexists;
                        label=new_label(&labelname, mycontext, L_CONST, &labelexists);
                        if (labelexists) {
                            if (label->type != L_CONST || label->value->type != T_LBL || pass == 1) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                            new_value.type = T_LBL;
                            new_value.u.lbl.p = cfile->p;
                            new_value.u.lbl.sline = sline;
                            new_value.u.lbl.waitforp = waitfor_p;
                            new_value.u.lbl.file = cfile;
                            new_value.u.lbl.parent = current_context;
                            var_assign(label, &new_value, 0);
                        } else {
                            val = val_alloc();
                            val->refcount = 1;
                            label->usepass = pass;
                            label->defpass = pass;
                            label->value = val;
                            label->sline = sline;
                            label->file = cfile;
                            label->epoint = epoint;
                            label->requires = 0;
                            label->conflicts = 0;
                            val->type = T_LBL;
                            val->u.lbl.p = cfile->p;
                            val->u.lbl.sline = sline;
                            val->u.lbl.waitforp = waitfor_p;
                            val->u.lbl.file = cfile;
                            val->u.lbl.parent = current_context;
                        }
                        label->ref=0;
                        goto finish;
                    }
                case CMD_MACRO:/* .macro */
                case CMD_SEGMENT:
                    {
                        struct label_s *label;
                        enum type_e type = (prm == CMD_MACRO) ? T_MACRO : T_SEGMENT;
                        int labelexists;
                        new_waitfor(W_ENDM, epoint);waitfor->skip=0;
                        ignore();
                        label=new_label(&labelname, mycontext, L_LABEL, &labelexists);
                        if (labelexists) {
                            if (label->type != L_LABEL || label->value->type != type || pass == 1) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                            new_value.type = type;
                            new_value.u.macro.p = cfile->p;
                            new_value.u.macro.size = 0;
                            new_value.u.macro.sline = sline;
                            new_value.u.macro.file = cfile;
                            get_macro_params(&new_value);
                            var_assign(label, &new_value, 0);
                            val_destroy(&new_value);
                        } else {
                            val = val_alloc();
                            val->refcount = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->usepass = pass;
                            label->defpass = pass;
                            label->value = val;
                            label->sline = sline;
                            label->file = cfile;
                            label->epoint = epoint;
                            val->type = type;
                            val->u.macro.p = cfile->p;
                            val->u.macro.size = 0;
                            val->u.macro.sline = sline;
                            val->u.macro.file = cfile;
                            get_macro_params(val);
                        }
                        label->ref=0;
                        goto finish;
                    }
                case CMD_FUNCTION:
                    {
                        struct label_s *label;
                        int labelexists;
                        new_waitfor(W_ENDF, epoint);waitfor->skip=0;
                        ignore();
                        label=new_label(&labelname, mycontext, L_LABEL, &labelexists);
                        if (labelexists) {
                            if (label->type != L_LABEL || label->value->type != T_FUNCTION || pass == 1) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                            new_value.type = T_FUNCTION;
                            new_value.u.func.p = cfile->p;
                            new_value.u.func.sline = sline;
                            new_value.u.func.file = cfile;
                            new_value.u.func.context = current_context;
                            get_func_params(&new_value);
                            var_assign(label, &new_value, 0);
                            val_destroy(&new_value);
                        } else {
                            val = val_alloc();
                            val->refcount = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->usepass = pass;
                            label->defpass = pass;
                            label->value = val;
                            label->sline = sline;
                            label->file = cfile;
                            label->epoint = epoint;
                            val->type = T_FUNCTION;
                            val->u.func.p = cfile->p;
                            val->u.func.sline = sline;
                            val->u.func.file = cfile;
                            val->u.func.context = current_context;
                            get_func_params(val);
                        }
                        label->ref=0;
                        goto finish;
                    }
                case CMD_STRUCT:
                case CMD_UNION:
                    {
                        struct label_s *label;
                        size_t memp, membp;
                        struct section_s olds = *current_section;
                        int declaration = !current_section->structrecursion;
                        int labelexists;

                        new_waitfor((prm==CMD_STRUCT)?W_ENDS:W_ENDU, epoint);waitfor->skip=0;
                        ignore();
                        label=new_label(&labelname, mycontext, L_LABEL, &labelexists);oaddr = current_section->address;
                        if (declaration) {
                            enum type_e type = (prm == CMD_STRUCT) ? T_STRUCT : T_UNION;
                            current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
                            current_section->end=current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
                            current_section->dooutput=0;memjmp(0); oaddr = 0;

                            if (labelexists) {
                                if (label->type != L_LABEL || label->value->type != type || pass == 1) err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                                new_value.type = type;
                                new_value.u.macro.size = (label->value->type == type) ? label->value->u.macro.size : 0;
                                new_value.u.macro.p = cfile->p;
                                new_value.u.macro.sline = sline;
                                new_value.u.macro.file = cfile;
                                get_macro_params(&new_value);
                                var_assign(label, &new_value, 0);
                                val_destroy(&new_value);
                            } else {
                                val = val_alloc();
                                val->refcount = 1;
                                label->usepass = pass;
                                label->defpass = pass;
                                label->value = val;
                                label->sline = sline;
                                label->file = cfile;
                                label->epoint = epoint;
                                label->requires = 0;
                                label->conflicts = 0;
                                label->nested = 1;
                                val->type = type;
                                val->u.macro.size = 0;
                                val->u.macro.p = cfile->p;
                                val->u.macro.sline = sline;
                                val->u.macro.file = cfile;
                                get_macro_params(val);
                            }
                        } else {
                            if (labelexists) {
                                if (label->type != L_LABEL || label->value->type != T_CODE || pass==1) {
                                    err_msg_double_defined(&label->name, label->file->realname, label->sline, label->epoint, &labelname, epoint);
                                    label = NULL;
                                } else {
                                    label->requires = current_section->requires;
                                    label->conflicts = current_section->conflicts;
                                    if (label->value->u.code.addr != current_section->l_address) {
                                        size_t size = label->value->u.code.size;
                                        signed char dtype = label->value->u.code.dtype;
                                        val = val_realloc(&label->value);
                                        val->type=T_CODE;
                                        val->u.code.addr = current_section->l_address;
                                        val->u.code.pass = pass - 1;
                                        val->u.code.size = size;
                                        val->u.code.dtype = dtype;
                                        if (label->usepass >= pass) {
                                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&label->name, label->epoint);
                                            fixeddig = 0;
                                        }
                                    }
                                    label->defpass = pass;
                                    get_mem(&memp, &membp);
                                }
                            } else {
                                val = val_alloc();
                                val->refcount = 1;
                                label->usepass=pass;
                                label->defpass=pass;
                                label->value = val;
                                label->file = cfile;
                                label->sline = sline;
                                label->epoint = epoint;
                                label->requires=current_section->requires;
                                label->conflicts=current_section->conflicts;
                                val->type = T_CODE;
                                val->u.code.addr = current_section->l_address;
                                val->u.code.size = 0;
                                val->u.code.dtype = D_NONE;
                                val->u.code.pass = 0;
                                get_mem(&memp, &membp);
                            }
                        }
                        if (label) {
                            label->ref = 0;
                        }
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
                            waitfor->what = (prm == CMD_STRUCT) ? W_ENDS2 : W_ENDU2;
                            waitfor->skip=1;
                            if (label && (label->value->type == T_STRUCT || label->value->type == T_UNION)) macro_recurse(W_ENDS, label->value, label);
                            else compile(cfile);
                            current_section->unionmode = old_unionmode;
                            current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                            current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                        } else err_msg(ERROR__MACRECURSION,"!!!!");
                        current_section->structrecursion--;
                        if (!label) goto finish;
                        if (declaration) {
                            if (label->value->u.macro.size != ((current_section->address - oaddr) & all_mem2)) {
                                label->value->u.macro.size = (current_section->address - oaddr) & all_mem2;
                                if (label->usepass >= pass) {
                                    if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&label->name, label->epoint);
                                    fixeddig = 0;
                                }
                            }
                            current_section->provides=olds.provides;current_section->requires=olds.requires;current_section->conflicts=olds.conflicts;
                            current_section->end=olds.end;current_section->start=olds.start;current_section->l_start=olds.l_start;current_section->address=olds.address;current_section->l_address=olds.l_address;
                            current_section->dooutput=olds.dooutput;memjmp(current_section->address);
                        } else {
                            set_size(label, current_section->address - oaddr, memp, membp);
                        }
                        goto finish;
                    }
                case CMD_SECTION:
                    {
                        struct section_s *tmp;
                        str_t sectionname;
                        int labelexists;
                        linepos_t opoint;
                        new_waitfor(W_SEND, epoint);waitfor->section=current_section;
                        ignore();opoint=lpoint;
                        sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                        if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        tmp=find_new_section(&sectionname, &labelexists);
                        if (!tmp->declared) {
                            if (!labelexists) {
                                tmp->end = tmp->start = tmp->address = 0;
                                tmp->l_start = tmp->l_address = 0;
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&sectionname, opoint);
                                fixeddig=0;
                            } else if (pass > 1) {
                                err_msg_not_defined(&sectionname, opoint); goto breakerr;
                            }
                        } else if (tmp->pass != pass) {
                            if (!tmp->moved) {
                                if (tmp->end < tmp->address) tmp->end = tmp->address;
                                tmp->moved = 1;
                            }
                            tmp->wrapwarn = tmp->wrapwarn2 = 0;
                            tmp->address = tmp->start;
                            tmp->l_address = tmp->l_start;
                        }
                        tmp->pass = pass;
                        waitfor->what = W_SEND2;
                        current_section = tmp;
                        memjmp(current_section->address);
                        break;
                    }
                }
            }
            if (!islabel) {
                tmp2=find_label(&labelname);
                if (tmp2) {
                    int rec = 100;
                    while (tmp2->value->type == T_IDENTREF) {
                        tmp2 = tmp2->value->u.identref;
                        if (!rec--) {
                            err_msg2(ERROR__REFRECURSION, NULL, epoint);
                            break;
                        }
                    }
                    if (tmp2->type == L_LABEL && (tmp2->value->type == T_MACRO || tmp2->value->type == T_SEGMENT || tmp2->value->type == T_FUNCTION)) {if (wht == WHAT_HASHMARK) lpoint.pos--;labelname.len=0;val = tmp2->value; goto as_macro;}
                }
            }
            {
                int labelexists;
                if (!islabel && tmp2 && tmp2->parent == current_context) {newlabel = tmp2;labelexists = 1;}
                else newlabel=new_label(&labelname, mycontext, L_LABEL, &labelexists);
                oaddr=current_section->address;
                if (labelexists) {
                    if (newlabel->type != L_LABEL || newlabel->value->type != T_CODE || pass==1) {
                        err_msg_double_defined(&newlabel->name, newlabel->file->realname, newlabel->sline, newlabel->epoint, &labelname, epoint);
                        newlabel = NULL; goto jn;
                    } else {
                        newlabel->requires = current_section->requires;
                        newlabel->conflicts = current_section->conflicts;
                        if (newlabel->value->u.code.addr != current_section->l_address) {
                            size_t size = newlabel->value->u.code.size;
                            signed char dtype = newlabel->value->u.code.dtype;
                            val = val_realloc(&newlabel->value);
                            val->type = T_CODE;
                            val->u.code.addr = current_section->l_address;
                            val->u.code.pass = pass - 1;
                            val->u.code.size = size;
                            val->u.code.dtype = dtype;
                            if (newlabel->usepass >= pass) {
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&newlabel->name, newlabel->epoint);
                                fixeddig = 0;
                            }
                        }
                        get_mem(&newmemp, &newmembp);
                    }
                } else {
                    val = val_alloc();
                    val->refcount = 1;
                    newlabel->usepass = pass;
                    newlabel->value = val;
                    newlabel->file = cfile;
                    newlabel->sline = sline;
                    newlabel->epoint = epoint;
                    newlabel->requires=current_section->requires;
                    newlabel->conflicts=current_section->conflicts;
                    val->type = T_CODE;
                    val->u.code.addr = current_section->l_address;
                    val->u.code.size = 0;
                    val->u.code.dtype = D_NONE;
                    val->u.code.pass = 0;
                    get_mem(&newmemp, &newmembp);
                }
                newlabel->defpass = pass;
            }
            if (epoint.pos && !islabel) err_msg2(ERROR_LABEL_NOT_LEF,NULL,epoint);
            if (wht==WHAT_COMMAND) { /* .proc */
                switch (prm) {
                case CMD_PROC:
                    new_waitfor(W_PEND, epoint);waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    if (!newlabel->ref && pass != 1) waitfor->skip=0;
                    else {
                        newlabel->nested = 1;
                        current_context=newlabel;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                            fprintf(flist, (all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t", current_section->address);
                            if (labelname.len) {
                                if (labelname.data[0] == '-' || labelname.data[0] == '+') fputc(labelname.data[0], flist);
                                else fwrite(labelname.data, labelname.len, 1, flist);
                            }
                            fputc('\n', flist);
                        }
                        newlabel->ref=0;
                    }
                    newlabel = NULL;
                    goto finish;
                case CMD_DSTRUCT: /* .dstruct */
                case CMD_DUNION:
                    {
                        int old_unionmode = current_section->unionmode;
                        address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                        address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                        current_section->unionmode = (prm==CMD_DUNION);
                        current_section->unionstart = current_section->unionend = current_section->address;
                        current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                        if (listing && flist && arguments.source) {
                            if (lastl!=LIST_DATA) {putc('\n',flist);lastl=LIST_DATA;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            printllist(flist);
                        }
                        newlabel->nested = 1;
                        newlabel->ref=0;
                        if (!get_exp(&w,1)) goto breakerr;
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val == &error_value) goto breakerr;
                        if (val->type != ((prm==CMD_DSTRUCT) ? T_STRUCT : T_UNION)) {err_msg_wrong_type(val, epoint); goto breakerr;}
                        ignore();if (here() == ',') lpoint.pos++;
                        current_section->structrecursion++;
                        macro_recurse((prm==CMD_DSTRUCT)?W_ENDS2:W_ENDU2, val, newlabel);
                        current_section->structrecursion--;
                        current_section->unionmode = old_unionmode;
                        current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                        current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                        goto finish;
                    }
                case CMD_SECTION:
                    waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    if (newlabel->ref && listing && flist && arguments.source) {
                        if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                        fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t", current_section->address);
                        if (labelname.len) {
                            if (labelname.data[0] == '-' || labelname.data[0] == '+') fputc(labelname.data[0], flist);
                            else fwrite(labelname.data, labelname.len, 1, flist);
                        }
                        fputc('\n', flist);
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
        case WHAT_STAR:if (waitfor->skip & 1) /* skip things if needed */
            {
                ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                lpoint.pos++;
                current_section->wrapwarn = current_section->wrapwarn2 = 0;
                if (!current_section->moved) {
                    if (current_section->end < current_section->address) current_section->end = current_section->address;
                    current_section->moved = 1;
                }
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
                    static const str_t starname = {1, (const uint8_t*)"*"};
                    if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(&starname, epoint);
                    fixeddig = 0;
                } else {
                    if (arguments.flat && !current_section->logicalrecursion) {
                        if ((address_t)val->u.num.val & ~all_mem2) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        } else {
                            current_section->l_address = (address_t)val->u.num.val & all_mem;
                            if (current_section->address != (address_t)val->u.num.val) {
                                current_section->address = (address_t)val->u.num.val;
                                memjmp(current_section->address);
                            }
                        }
                    } else {
                        if ((uval_t)val->u.num.val & ~(uval_t)all_mem) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        } else {
                            address_t addr;
                            if (arguments.tasmcomp) addr = (uint16_t)val->u.num.val;
                            else if ((address_t)val->u.num.val > current_section->l_address) {
                                addr = (current_section->address + (((address_t)val->u.num.val - current_section->l_address) & all_mem)) & all_mem2;
                            } else {
                                addr = (current_section->address - ((current_section->l_address - (address_t)val->u.num.val) & all_mem)) & all_mem2;
                            }
                            if (current_section->address != addr) {
                                current_section->address = addr;
                                memjmp(current_section->address);
                            }
                            current_section->l_address = (uval_t)val->u.num.val & all_mem;
                        }
                    }
                }
            }
            break;
        case WHAT_COMMENT:
        case WHAT_EOL:
            if (listing && flist && arguments.source && (waitfor->skip & 1) && wasref) {
                if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                printllist(flist);
            }
            break;
        case WHAT_COMMAND:
            {
                ignore();
                if (listing && flist && arguments.source && (waitfor->skip & 1) && prm>CMD_DWORD) {
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
                        case CMD_DUNION:
                        case CMD_DSTRUCT:
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
                                fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                                if (labelname.len) {
                                    if (labelname.data[0] == '-' || labelname.data[0] == '+') fputc(labelname.data[0], flist);
                                    else fwrite(labelname.data, labelname.len, 1, flist);
                                }
                                fputc('\n', flist);
                            }
                    }
                }
                if (prm==CMD_ENDC) { /* .endc */
                    if (!close_waitfor(W_ENDC)) err_msg2(ERROR______EXPECTED,".COMMENT", epoint);
                    break;
                } else if (waitfor->what == W_ENDC) break;
                if (prm==CMD_FI) /* .fi */
                {
                    if (!close_waitfor(W_FI2) && !close_waitfor(W_FI)) err_msg2(ERROR______EXPECTED,".IF", epoint);
                    break;
                }
                if (prm==CMD_ENDSWITCH) /* .endswitch */
                {
                    if (!close_waitfor(W_SWITCH2) && !close_waitfor(W_SWITCH)) err_msg2(ERROR______EXPECTED,".SWITCH", epoint);
                    break;
                }
                if (prm==CMD_DEFAULT) { /* .default */
                    if (waitfor->what==W_SWITCH) {err_msg2(ERROR______EXPECTED,".ENDSWITCH", epoint); break;}
                    if (waitfor->what!=W_SWITCH2) {err_msg2(ERROR______EXPECTED,".SWITCH", epoint); break;}
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_SWITCH;waitfor->line=sline;
                    break;
                }
                if (prm==CMD_ELSE) { /* .else */
                    if (waitfor->what==W_FI) {err_msg2(ERROR______EXPECTED,".FI", epoint); break;}
                    if (waitfor->what!=W_FI2) {err_msg2(ERROR______EXPECTED,".IF", epoint); break;}
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_FI;waitfor->line=sline;
                    break;
                }
                if (prm==CMD_IF || prm==CMD_IFEQ || prm==CMD_IFNE || prm==CMD_IFPL || prm==CMD_IFMI || prm==CMD_ELSIF) { /* .if */
                    uint8_t skwait = waitfor->skip;
                    if (prm==CMD_ELSIF) {
                        if (waitfor->what!=W_FI2) {err_msg2(ERROR______EXPECTED,".IF", epoint); break;}
                    } else new_waitfor(W_FI2, epoint);
                    waitfor->line=sline;
                    if (((skwait==1) && prm!=CMD_ELSIF) || ((skwait==2) && prm==CMD_ELSIF)) {
                        if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        }
                    } else val = &none_value;
                    switch (prm) {
                    case CMD_ELSIF:
                        if (val->type == T_ERROR) err_msg_wrong_type(val, epoint);
                        waitfor->skip = val_truth(val) ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                        break;
                    case CMD_IF:
                        if (val->type == T_ERROR) err_msg_wrong_type(val, epoint);
                        waitfor->skip = val_truth(val) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                        break;
                    case CMD_IFNE:
                        switch (val->type) {
                        case T_SINT:
                        case T_UINT:
                        case T_BOOL:
                        case T_CODE:
                        case T_FLOAT:
                        case T_STR:
                        case T_NUM: waitfor->skip = val_truth(val) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        default: err_msg_wrong_type(val, epoint);
                        case T_NONE: waitfor->skip = (prevwaitfor->skip & 1) << 1;break;
                        }
                        break;
                    case CMD_IFEQ:
                        switch (val->type) {
                        case T_SINT:
                        case T_UINT:
                        case T_BOOL:
                        case T_CODE:
                        case T_FLOAT:
                        case T_STR:
                        case T_NUM: waitfor->skip = (!val_truth(val)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        default: err_msg_wrong_type(val, epoint);
                        case T_NONE: waitfor->skip = (prevwaitfor->skip & 1) << 1;break;
                        }
                        break;
                    case CMD_IFPL:
                        switch (val->type) {
                        case T_SINT: waitfor->skip = (arguments.tasmcomp ? (~val->u.num.val & 0x8000) : (val->u.num.val>=0)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_UINT:
                        case T_BOOL:
                        case T_NUM: waitfor->skip = (arguments.tasmcomp ? (~val->u.num.val & 0x8000) : 1) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_CODE: waitfor->skip = (arguments.tasmcomp ? (~val->u.code.addr & 0x8000) : 1) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_FLOAT: waitfor->skip = (val->u.real >= 0.0) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_STR: waitfor->skip = prevwaitfor->skip & 1;break;
                        default: err_msg_wrong_type(val, epoint);
                        case T_NONE: waitfor->skip = (prevwaitfor->skip & 1) << 1;break;
                        }
                        break;
                    case CMD_IFMI:
                        switch (val->type) {
                        case T_SINT: waitfor->skip = (arguments.tasmcomp ? (val->u.num.val & 0x8000) : (val->u.num.val < 0)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_UINT:
                        case T_BOOL:
                        case T_NUM: waitfor->skip = (arguments.tasmcomp ? (val->u.num.val & 0x8000) : 0) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_CODE: waitfor->skip = (arguments.tasmcomp ? (val->u.code.addr & 0x8000) : 0) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_FLOAT: waitfor->skip = (val->u.real < 0.0) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        case T_STR: waitfor->skip = (prevwaitfor->skip & 1) << 1;break;
                        default: err_msg_wrong_type(val, epoint);
                        case T_NONE: waitfor->skip = (prevwaitfor->skip & 1) << 1;break;
                        }
                        break;
                    }
                    break;
                }
                if (prm==CMD_SWITCH) { /* .switch */
                    uint8_t skwait = waitfor->skip;
                    new_waitfor(W_SWITCH2, epoint);
                    waitfor->line=sline;
                    if (skwait==1) {
                        if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        }
                    } else val = &none_value;
                    waitfor->val = val_reference(val);
                    waitfor->skip = (prevwaitfor->skip & 1) << 1;
                    break;
                }
                if (prm==CMD_CASE) { /* .case */
                    uint8_t skwait = waitfor->skip;
                    int truth = 0;
                    if (waitfor->what == W_SWITCH) {err_msg2(ERROR______EXPECTED,".ENDSWITCH", epoint); goto breakerr;}
                    if (waitfor->what != W_SWITCH2) {err_msg2(ERROR______EXPECTED,".SWITCH", epoint); goto breakerr;}
                    waitfor->line=sline;
                    if (skwait==2) {
                        if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_vals_tuple(T_NONE))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        }
                        if (val->type == T_TUPLE || val->type == T_LIST) {
                            truth = val_inlist(waitfor->val, val, epoint, epoint, epoint);
                        } else {
                            truth = val_equals(waitfor->val, val, epoint, epoint, epoint);
                        }
                        val_destroy(val);
                    }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    break;
                }
                if (prm==CMD_ENDM) { /* .endm */
                    if (close_waitfor(W_ENDM)) {
                    } else if (close_waitfor(W_ENDM2)) {
                        nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".MACRO or .SEGMENT", epoint);
                    break;
                }
                if (prm==CMD_ENDF) { /* .endf */
                    if (close_waitfor(W_ENDF)) {
                        lpoint.pos += strlen((const char *)pline + lpoint.pos);
                    } else if (close_waitfor(W_ENDF2)) {
                        nobreak = 0;
                        if (here() && here() != ';' && get_exp(&w,0)) {
                            retval = get_vals_tuple(T_NONE);
                        } 
                        if (!retval) retval = &null_tuple;
                    } else {err_msg2(ERROR______EXPECTED,".FUNCTION", epoint);goto breakerr;}
                    break;
                }
                if (prm==CMD_NEXT) { /* .next */
                    if (close_waitfor(W_NEXT)) {
                    } else if (close_waitfor(W_NEXT2)) {
                        nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".FOR or .REPT", epoint);
                    break;
                }
                if (prm==CMD_PEND) { /* .pend */
                    if (waitfor->what==W_PEND) {
                        if (waitfor->skip & 1) {
                            if (current_context->parent) {
                                current_context = current_context->parent;
                            } else err_msg2(ERROR______EXPECTED,".proc", epoint);
                            lastl=LIST_NONE;
                            if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, waitfor->memp, waitfor->membp);
                        }
                        close_waitfor(W_PEND);
                    } else err_msg2(ERROR______EXPECTED,".PROC", epoint);
                    break;
                }
                if (prm==CMD_ENDS) { /* .ends */
                    if (close_waitfor(W_ENDS)) {
                    } else if (close_waitfor(W_ENDS2)) {
                        nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".STRUCT", epoint); break;
                    break;
                }
                if (prm==CMD_SEND) { /* .send */
                    if (close_waitfor(W_SEND)) {
                        get_label();
                    } else if (waitfor->what==W_SEND2) {
                        str_t sectionname;
                        ignore();epoint=lpoint;
                        sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                        if (sectionname.len) {
                            int notsame = arguments.casesensitive ? str_cmp(&sectionname, &current_section->name) : str_casecmp(&sectionname, &current_section->name);
                            if (notsame) {
                                char *s = malloc(current_section->name.len + 1);
                                if (!s) err_msg_out_of_memory();
                                memcpy(s, current_section->name.data, current_section->name.len);
                                s[current_section->name.len] = '\0';
                                err_msg2(ERROR______EXPECTED, s, epoint);
                                free(s);
                            }
                        }
                        if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, waitfor->memp, waitfor->membp);
                        current_section = waitfor->section;
                        memjmp(current_section->address);
                        close_waitfor(W_SEND2);
                    } else {err_msg2(ERROR______EXPECTED,".SECTION", epoint);goto breakerr;}
                    break;
                }
                if (prm==CMD_ENDU) { /* .endu */
                    if (close_waitfor(W_ENDU)) {
                    } else if (close_waitfor(W_ENDU2)) {
                        nobreak=0; current_section->l_address = current_section->l_unionend;
                        if (current_section->address != current_section->unionend) {
                            current_section->address = current_section->unionend;
                            memjmp(current_section->address);
                        }
                    } else err_msg2(ERROR______EXPECTED,".UNION", epoint); break;
                    break;
                }
                if (prm==CMD_ENDP) { /* .endp */
                    if (close_waitfor(W_ENDP)) {
                    } else if (waitfor->what==W_ENDP2) {
			if ((current_section->l_address & ~0xff) != (waitfor->laddr & ~0xff)) {
                            err_msg2(ERROR____PAGE_ERROR, &current_section->l_address, epoint);
			}
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, waitfor->memp, waitfor->membp);
                        close_waitfor(W_ENDP2);
                    } else err_msg2(ERROR______EXPECTED,".PAGE", epoint); break;
                    break;
                }
                if (prm==CMD_HERE) { /* .here */
                    if (close_waitfor(W_HERE)) {
                        current_section->logicalrecursion--;
                    } else if (waitfor->what==W_HERE2) {
			current_section->l_address = current_section->address + waitfor->laddr;
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, waitfor->memp, waitfor->membp);
                        close_waitfor(W_HERE2);
                        current_section->logicalrecursion--;
                    } else err_msg2(ERROR______EXPECTED,".LOGICAL", epoint); break;
                    break;
                }
                if (prm==CMD_BEND) { /* .bend */
                    if (close_waitfor(W_BEND)) {
                    } else if (waitfor->what==W_BEND2) {
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, waitfor->memp, waitfor->membp);
			if (current_context->parent) current_context = current_context->parent;
			else err_msg2(ERROR______EXPECTED,".block", epoint);
			close_waitfor(W_BEND2);
                    } else err_msg2(ERROR______EXPECTED,".BLOCK", epoint); break;
                    break;
                }
                if (!(waitfor->skip & 1)) {
                    enum wait_e what2;
                    switch (prm) {
                    case CMD_BLOCK: what2 = W_BEND; break;
                    case CMD_LOGICAL: what2 = W_HERE; break;
                    case CMD_PAGE: what2 = W_ENDP; break;
                    case CMD_UNION: what2 = W_ENDU; break;
                    case CMD_STRUCT: what2 = W_ENDS; break;
                    case CMD_SECTION: what2 = W_SEND; break;
                    case CMD_FUNCTION: what2 = W_ENDF; break;
                    case CMD_MACRO:
                    case CMD_SEGMENT: what2 = W_ENDM; break;
                    case CMD_FOR:
                    case CMD_REPT: what2 = W_NEXT; break;
                    case CMD_COMMENT: what2 = W_ENDC; break;
                    case CMD_PROC: what2 = W_PEND; break;
                    default: what2 = W_NONE;
                    }
                    if (what2 != W_NONE) new_waitfor(what2, epoint);
                    break;/* skip things if needed */
                }
                if (prm<=CMD_DWORD || prm==CMD_BINARY) { /* .byte .text .rta .char .int .word .long */
                    size_t uninit = 0;
                    size_t sum = 0;

                    mark_mem(current_section->address);
                    if (prm<CMD_BYTE) {    /* .text .ptext .shift .shift2 .null */
                        int16_t ch2=-1;
                        linepos_t large = {0,0};
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (prm==CMD_PTEXT) ch2=0;
                        if (!get_exp(&w,0)) goto breakerr;
                        while ((val = get_val(T_NONE, &epoint))) {
                            if (val == &error_value) continue;
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
                                    case T_NUM:
                                    case T_UINT:
                                    case T_SINT:
                                        if (val->type != T_NUM || val->u.num.len > 8) {
                                            if ((uval_t)val->u.num.val & ~(uval_t)0xff) large=epoint;
                                        }
                                        ch2 = (uint8_t)val->u.num.val;
                                        break;
                                    case T_CODE:
                                        if (val->u.code.addr > (address_t)0xff) large=epoint;
                                        ch2 = (uint8_t)val->u.code.addr;
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
                                                    case T_NUM:
                                                    case T_UINT:
                                                    case T_SINT:
                                                        if (val2->type != T_NUM || val2->u.num.len > 8) {
                                                            if ((uval_t)val2->u.num.val & ~(uval_t)0xff) large=epoint;
                                                        }
                                                        ch2 = (uint8_t)val2->u.num.val;
                                                        break;
                                                    case T_CODE:
                                                        if (val2->u.code.addr > (address_t)0xff) large=epoint;
                                                        ch2 = (uint8_t)val2->u.code.addr;
                                                        break;
                                                    case T_FLOAT:
                                                        if ((uval_t)val2->u.real & ~(uval_t)0xff) large=epoint;
                                                        ch2 = (uint8_t)val2->u.real;
                                                        break;
                                                    default: err_msg_wrong_type(val2, epoint);
                                                    case T_NONE:
                                                             if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                                             if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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

                            if (current_section->dooutput) write_mark_mem(sum-1);
                        }
                        if (large.pos) err_msg2(ERROR_CONSTNT_LARGE, NULL, large);
                    } else if (prm<=CMD_DWORD) { /* .word .int .rta .long */
                        uint32_t ch2;
                        uval_t uv;
                        linepos_t large = {0,0};
                        if (newlabel) {
                            signed char dtype;
                            switch (prm) {
                            case CMD_DINT: dtype = D_DINT;break;
                            case CMD_LINT: dtype = D_LINT;break;
                            case CMD_INT: dtype = D_INT;break;
                            case CMD_CHAR: dtype = D_CHAR;break;
                            default:
                            case CMD_BYTE: dtype = D_BYTE;break;
                            case CMD_RTA:
                            case CMD_WORD: dtype = D_WORD;break;
                            case CMD_LONG: dtype = D_LONG;break;
                            case CMD_DWORD: dtype = D_DWORD;break;
                            }
                            newlabel->value->u.code.dtype = dtype;
                        }
                        if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                        while ((val = get_val(T_NONE, &epoint))) {
                            if (val == &error_value) ch2 = 0; else
                            switch (val->type) {
                            case T_GAP:uninit += 1 + (prm>=CMD_RTA) + (prm>=CMD_LINT) + (prm >= CMD_DINT);continue;
                            case T_STR: if (str_to_num(val, T_NUM, &new_value, epoint)) {large = epoint; ch2 = 0; break;} val = &new_value;
                            case T_FLOAT:
                            case T_CODE:
                            case T_NUM:
                            case T_BOOL:
                            case T_SINT:
                            case T_UINT:
                                uv = (val->type == T_FLOAT) ? (uval_t)val->u.real : ((val->type == T_CODE) ? (uval_t)val->u.code.addr : (uval_t)val->u.num.val);
                                switch (prm) {
                                case CMD_CHAR: if ((val->type != T_NUM || val->u.num.len > 8) && (uv & ~(uval_t)0x7f) && (~uv & ~(uval_t)0x7f)) large=epoint;break;
                                case CMD_BYTE: if ((val->type != T_NUM || val->u.num.len > 8) && (uv & ~(uval_t)0xff)) large=epoint; break;
                                case CMD_INT: if ((val->type != T_NUM || val->u.num.len > 16) && (uv & ~(uval_t)0x7fff) && (~uv & ~(uval_t)0x7fff)) large=epoint;break;
                                case CMD_LINT: if ((val->type != T_NUM || val->u.num.len > 24) && (uv & ~(uval_t)0x7fffff) && (~uv & ~(uval_t)0x7fffff)) large=epoint;break;
                                case CMD_LONG: if ((val->type != T_NUM || val->u.num.len > 24) && (uv & ~(uval_t)0xffffff)) large=epoint; break;
                                case CMD_DINT: if ((val->type != T_NUM || val->u.num.len > 32) && (uv & ~(uval_t)0x7fffffff) && (~uv & ~(uval_t)0x7fffffff)) large=epoint;break;
                                case CMD_DWORD: if ((val->type != T_NUM || val->u.num.len > 32) && (uv & ~(uval_t)0xffffffff)) large=epoint; break;
                                default: if ((val->type != T_NUM || val->u.num.len > 16) && (uv & ~(uval_t)0xffff)) large=epoint;
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
                                        case T_STR: if (str_to_num(val2, T_NUM, &new_value, epoint)) {large = epoint; ch2 = 0; break;} val2 = &new_value;
                                        case T_FLOAT:
                                        case T_CODE:
                                        case T_NUM:
                                        case T_BOOL:
                                        case T_SINT:
                                        case T_UINT:
                                            uv = (val2->type == T_FLOAT) ? (uval_t)val2->u.real : ((val2->type == T_CODE) ? (uval_t)val2->u.code.addr : (uval_t)val2->u.num.val);
                                            switch (prm) {
                                            case CMD_CHAR: if ((val2->type != T_NUM || val2->u.num.len > 8) && (uv & ~(uval_t)0x7f) && (~uv & ~(uval_t)0x7f)) large=epoint;break;
                                            case CMD_BYTE: if ((val2->type != T_NUM || val2->u.num.len > 8) && (uv & ~(uval_t)0xff)) large=epoint; break;
                                            case CMD_INT: if ((val2->type != T_NUM || val2->u.num.len > 16) && (uv & ~(uval_t)0x7fff) && (~uv & ~(uval_t)0x7fff)) large=epoint;break;
                                            case CMD_LINT: if ((val2->type != T_NUM || val2->u.num.len > 24) && (uv & ~(uval_t)0x7fffff) && (~uv & ~(uval_t)0x7fffff)) large=epoint;break;
                                            case CMD_LONG: if ((val2->type != T_NUM || val2->u.num.len > 24) && (uv & ~(uval_t)0xffffff)) large=epoint; break;
                                            case CMD_DINT: if ((val2->type != T_NUM || val2->u.num.len > 32) && (uv & ~(uval_t)0x7fffffff) && (~uv & ~(uval_t)0x7fffffff)) large=epoint;break;
                                            case CMD_DWORD: if ((val2->type != T_NUM || val2->u.num.len > 32) && (uv & ~(uval_t)0xffffffff)) large=epoint; break;
                                            default: if ((val2->type != T_NUM || val2->u.num.len > 16) && (uv & ~(uval_t)0xffff)) large=epoint;
                                            }
                                            ch2 = uv;
                                            break;
                                        default: err_msg_wrong_type(val2, epoint);
                                        case T_NONE:
                                                 if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                                     if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                        if (large.pos) err_msg2(ERROR_CONSTNT_LARGE, NULL, large);
                    } else if (prm==CMD_BINARY) { /* .binary */
                        size_t foffset = 0;
                        struct value_s *val2 = NULL;
                        address_t fsize = all_mem+1;
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        } else {
                            if (val->type != T_STR) {err_msg_wrong_type(val, epoint);goto breakerr;}
                            if (get_path(val, cfile->realname, path, sizeof(path))) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);goto breakerr;}
                            val2 = val;
                        }
                        if ((val = get_val(T_UINT, &epoint))) {
                            if (val == &error_value) goto breakerr;
                            if (val->type == T_NONE) {
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                fixeddig = 0;
                            } else {
                                if (val->u.num.val<0) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint); goto breakerr;}
                                foffset = val->u.num.val;
                            }
                            if ((val = get_val(T_UINT, &epoint))) {
                                if (val == &error_value) goto breakerr;
                                if (val->type == T_NONE) {
                                    if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                        list_mem(flist, all_mem, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_OFFS) {   /* .offs */
                    linepos_t opoint = epoint;
                    if (!current_section->moved) {
                        if (current_section->end < current_section->address) current_section->end = current_section->address;
                        current_section->moved = 1;
                    }
                    current_section->wrapwarn = current_section->wrapwarn2 = 0;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_SINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else if (val->u.num.val) {
                        if (current_section->structrecursion) {
                            if (val->u.num.val < 0) err_msg2(ERROR___NOT_ALLOWED, ".OFFS", opoint);
                            else {
                                current_section->l_address += val->u.num.val;
                                current_section->l_address &= all_mem;
                                current_section->address += val->u.num.val;
                            }
                        } else current_section->address += val->u.num.val;
                        current_section->address &= all_mem2;
                        memjmp(current_section->address);
                    }
                    break;
                }
                if (prm==CMD_LOGICAL) { /* .logical */
                    linepos_t opoint = epoint;
                    new_waitfor(W_HERE2, epoint);waitfor->laddr = current_section->l_address - current_section->address;waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    current_section->logicalrecursion++;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val == &error_value) goto breakerr;
                    else if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".LOGICAL", opoint);
                    else if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        if ((uval_t)val->u.num.val & ~(uval_t)all_mem) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else current_section->l_address=(uval_t)val->u.num.val;
                    }
                    newlabel = NULL;
                    break;
                }
                if (prm==CMD_AS) { /* .as */
                    longaccu=0;
                    break;
                }
                if (prm==CMD_AL) { /* .al */
                    longaccu=1;
                    break;
                }
                if (prm==CMD_XS) { /* .xs */
                    longindex=0;
                    break;
                }
                if (prm==CMD_XL) { /* .xl */
                    longindex=1;
                    break;
                }
                if (prm==CMD_BLOCK) { /* .block */
		    new_waitfor(W_BEND2, epoint);
                    if (newlabel) {
                        newlabel->nested = 1;
                        current_context=newlabel;
                        waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                        if (newlabel->ref && listing && flist && arguments.source) {
                            if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                            fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                            if (labelname.len) {
                                if (labelname.data[0] == '-' || labelname.data[0] == '+') fputc(labelname.data[0], flist);
                                else fwrite(labelname.data, labelname.len, 1, flist);
                            }
                            fputc('\n', flist);
                        }
                        newlabel = NULL;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        sprintf(reflabel, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                        tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                        current_context=new_label(&tmpname, mycontext, L_LABEL, &labelexists);
                        current_context->value = &none_value;
                    }
                    break;
                }
                if (prm==CMD_DATABANK) { /* .databank */
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else databank=val->u.num.val;
                    }
                    break;
                }
                if (prm==CMD_DPAGE) { /* .dpage */
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        if ((val->type != T_NUM || val->u.num.len > 16) && ((uval_t)val->u.num.val & ~(uval_t)0xffff)) err_msg2(ERROR_CONSTNT_LARGE,NULL, epoint);
                        else dpage=val->u.num.val;
                    }
                    break;
                }
                if (prm==CMD_FILL) { /* .fill */
                    address_t db = 0;
                    int ch = -1;
                    if (newlabel) {
                        newlabel->value->u.code.dtype = D_BYTE;
                    }
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        db=val->u.num.val;
                        if (db && db - 1 > all_mem2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);goto breakerr;}
                    }
                    if ((val = get_val(T_GAP, &epoint))) {
                        if (val == &error_value) goto breakerr;
                        else if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                        list_mem(flist, all_mem, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_ASSERT) { /* .assert */
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;current_section->provides=~(uval_t)0;
                    } else current_section->provides=val->u.num.val;
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = current_section->requires = 0;
                    } else current_section->requires=val->u.num.val;
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = current_section->conflicts = 0;
                    } else current_section->conflicts=val->u.num.val;
                    eval_finish();
                    break;
                }
                if (prm==CMD_CHECK) { /* .check */
                    linepos_t opoint = epoint;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else if ((val->u.num.val & current_section->provides) ^ val->u.num.val) err_msg_requires(NULL, opoint);
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else if (val->u.num.val & current_section->provides) err_msg_conflicts(NULL, opoint);
                    eval_finish();
                    break;
                }
                if (prm==CMD_WARN || prm==CMD_CWARN || prm==CMD_ERROR || prm==CMD_CERROR) { /* .warn .cwarn .error .cerror */
                    int rc;
                    int first = 1;
                    int write = 1;
                    struct encoding_s *old = actual_encoding;
                    struct error_s user_error;
                    error_init(&user_error);
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; /* ellenorizve. */
                    err_msg_variable(&user_error, NULL, 0);
                    for (;;) {
                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;
                        if (first) {
                            first = 0;
                            if (prm == CMD_CWARN || prm == CMD_CERROR) {
                                if (val->type == T_ERROR) err_msg_wrong_type(val, epoint);
                                write = val_truth(val);
                                continue;
                            }
                            write = 1;
                        }
                        if (write) {
                            if (val->type == T_ERROR) err_msg_wrong_type(val, epoint);
                            else if (val->type != T_NONE) err_msg_variable(&user_error, val, 0);
                        }
                    }
                    if (write) err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED,&user_error,epoint);
                    error_destroy(&user_error);
                    eval_finish();
                    break;
                }
                if (prm==CMD_ENC) { /* .enc */
                    str_t encname;
                    ignore();epoint=lpoint;
                    encname.data = pline + lpoint.pos; encname.len = get_label();
                    if (!encname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    actual_encoding = new_encoding(&encname);
                    break;
                }
                if (prm==CMD_CDEF) { /* .cdef */
                    struct trans_s tmp, *t;
                    struct encoding_s *old = actual_encoding;
                    uint32_t ch;
                    int rc;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; /* ellenorizve. */
                    for (;;) {
                        int endok = 0;
                        size_t i = 0;
                        int try = 1;

                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->type) {
                        case T_NUM: if (val->u.num.len <= 24) { tmp.start = val->u.num.val; break; }
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
                        case T_NONE: 
                             if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                             try = fixeddig = 0;
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
                            if (val->type == T_NONE) {
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                try = fixeddig = 0;
                            } else {
                                if ((val->type != T_NUM || val->u.num.len > 24) && ((uval_t)val->u.num.val & ~(uval_t)0xffffff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                                if (tmp.start > (uint32_t)val->u.num.val) {
                                    tmp.end = tmp.start;
                                    tmp.start = val->u.num.val;
                                } else tmp.end = val->u.num.val;
                            }
                        }
                        actual_encoding = NULL;
                        val = get_val(T_UINT, &epoint);
                        actual_encoding = old;
                        if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                        if (val == &error_value) goto breakerr;
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        } else if (try) {
                            if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                            tmp.offset = val->u.num.val;
                            t = new_trans(&tmp, actual_encoding);
                            if (t->start != tmp.start || t->end != tmp.end || t->offset != tmp.offset) {
                                err_msg2(ERROR_DOUBLE_DEFINE, "range", epoint); goto breakerr;
                            }
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_EDEF) { /* .edef */
                    struct escape_s *t;
                    struct encoding_s *old = actual_encoding;
                    int rc;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0);
                    actual_encoding = old;
                    if (!rc) goto breakerr; /* ellenorizve. */
                    for (;;) {
                        linepos_t opoint;
                        struct value_s *v;
                        int try = 1;

                        actual_encoding = NULL;
                        val = get_val(T_NONE, &epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->type) {
                        case T_STR:
                             if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                             break;
                        case T_NONE: 
                             if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                             try = fixeddig = 0;
                             break;
                        default:
                            err_msg_wrong_type(val, epoint);
                            goto breakerr;
                        }
                        v = val_reference(val);
                        actual_encoding = NULL;
                        opoint = epoint;
                        val = get_val(T_UINT, &epoint);
                        actual_encoding = old;
                        if (!val) {err_msg(ERROR______EXPECTED,","); val_destroy(v); goto breakerr;}
                        if (val == &error_value) {val_destroy(v); goto breakerr;}
                        if (val->type == T_NONE) {
                             if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                             fixeddig = 0;
                             val_destroy(v);
                        } else if (try) {
                            if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                            t = new_escape(v->u.str.data, v->u.str.data + v->u.str.len, (uint8_t)val->u.num.val, actual_encoding);
                            val_destroy(v);
                            if (t->code != (uint8_t)val->u.num.val) {
                                err_msg2(ERROR_DOUBLE_DEFINE,"escape", opoint); goto breakerr;
                            }
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_CPU) { /* .cpu */
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
                if (prm==CMD_REPT) { /* .rept */
                    new_waitfor(W_NEXT, epoint);waitfor->skip=0;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (eval_finish()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL);goto breakerr;}
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        ival_t cnt = val->u.num.val;
                        if (cnt > 0) {
                            size_t pos = cfile->p;
                            line_t lin = sline;
                            int labelexists;
                            struct star_s *s = new_star(vline, &labelexists);
                            struct avltree *stree_old = star_tree;
                            line_t ovline = vline;

                            close_waitfor(W_NEXT);
                            if (labelexists && s->addr != star) {
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                fixeddig=0;
                            }
                            s->addr = star;
                            star_tree = &s->tree;vline=0;
                            while (cnt--) {
                                sline=lin;cfile->p=pos;
                                new_waitfor(W_NEXT2, epoint);waitfor->skip=1;
                                compile(cfile);
                            }
                            star_tree = stree_old; vline = ovline;
                        }
                    }
                    break;
                }
                if (prm==CMD_ALIGN) { /* .align */
                    int align = 1, fill=-1;
                    if (newlabel) {
                        newlabel->value->u.code.dtype = D_BYTE;
                    }
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".ALIGN");
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        if (!val->u.num.val || ((uval_t)val->u.num.val & ~(uval_t)all_mem)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else align = val->u.num.val;
                    }
                    if ((val = get_val(T_GAP, &epoint))) {
                        if (val == &error_value) goto breakerr;
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        } else if (val->type != T_GAP) {
                            if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
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
                        list_mem(flist, all_mem, current_section->dooutput, &lastl);
                    }
                    break;
                }
                if (prm==CMD_EOR) {   /* .eor */
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_NUM, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = outputeor = 0;
                    } else {
                        if ((val->type != T_NUM || val->u.num.len > 8) && ((uval_t)val->u.num.val & ~(uval_t)0xff)) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
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
                if (prm==CMD_COMMENT) { /* .comment */
                    new_waitfor(W_ENDC, epoint);waitfor->skip=0;
                    break;
                }
                if (prm==CMD_INCLUDE || prm == CMD_BINCLUDE) { /* .include, .binclude */
                    struct file_s *f;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else {
                        if (val->type != T_STR) {err_msg_wrong_type(val, epoint);goto breakerr;}
                        if (get_path(val, cfile->realname, path, sizeof(path))) {err_msg(ERROR_CONSTNT_LARGE,NULL);goto breakerr;}

                        f = openfile(path, cfile->realname, 0, val);
                        if (f->open>1) {
                            err_msg(ERROR_FILERECURSION,NULL);
                        } else {
                            int starexists;
                            struct star_s *s = new_star(vline, &starexists);
                            struct avltree *stree_old = star_tree;
                            uint32_t old_backr = backr, old_forwr = forwr;
                            line_t lin = sline;
                            line_t vlin = vline;

                            if (listing && flist) {
                                fprintf(flist,"\n;******  Processing file \"%s\"\n",f->realname);
                                lastl=LIST_NONE;
                            }

                            if (starexists && s->addr != star) {
                                linepos_t nopoint = {0,0};
                                if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, nopoint);
                                fixeddig=0;
                            }
                            s->addr = star;
                            enterfile(f->realname,sline);
                            sline = vline = 0; f->p=0;
                            star_tree = &s->tree;
                            backr = forwr = 0;
                            reffile=f->uid;
                            if (prm == CMD_BINCLUDE) {
                                if (newlabel) {
                                    newlabel->nested = 1;
                                    current_context = newlabel;
                                } else {
                                    int labelexists;
                                    str_t tmpname;
                                    sprintf(reflabel, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                                    tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                                    current_context=new_label(&tmpname, mycontext, L_LABEL, &labelexists);
                                    current_context->value = &none_value;
                                }
                                compile(f);
                                current_context = current_context->parent;
                            } else compile(f);
                            sline = lin; vline = vlin;
                            star_tree = stree_old;
                            backr = old_backr; forwr = old_forwr;
                            exitfile();
                        }
                        closefile(f);
                        reffile=cfile->uid;
                        if (listing && flist) {
                            fprintf(flist,"\n;******  Return to file \"%s\"\n",cfile->realname);
                            lastl=LIST_NONE;
                        }
                    }
                    break;
                }
                if (prm==CMD_FOR) { /* .for */
                    size_t pos, xpos;
                    line_t lin, xlin;
                    linepos_t apoint, bpoint;
                    int nopos = -1;
                    uint8_t expr[linelength];
                    struct label_s *var;
                    struct star_s *s;
                    struct avltree *stree_old;
                    line_t ovline;
                    int starexists;

                    new_waitfor(W_NEXT, epoint);waitfor->skip=0;
                    if (strlen((const char *)pline)>=linelength) {err_msg(ERROR_LINE_TOO_LONG,NULL);goto breakerr;}
                    if ((wht=what(&prm))==WHAT_EXPRESSION && prm==1) { /* label */
                        int labelexists;
                        str_t varname;
                        epoint = lpoint;
                        varname.data = pline + lpoint.pos; varname.len = get_label();
                        if (!varname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL);goto breakerr;}
                        ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                        lpoint.pos++;
                        if (!get_exp(&w,1)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_vals_tuple(T_IDENTREF))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        var=new_label(&varname, mycontext, L_VAR, &labelexists);
                        if (labelexists) {
                            if (var->type != L_VAR) err_msg_double_defined(&var->name, var->file->realname, var->sline, var->epoint, &varname, epoint);
                            else {
                                var->requires=current_section->requires;
                                var->conflicts=current_section->conflicts;
                                var_assign(var, val, fixeddig);
                                val_destroy(val);
                            }
                        } else {
                            var->requires = current_section->requires;
                            var->conflicts = current_section->conflicts;
                            var->usepass = pass;
                            var->defpass = pass;
                            var->value = val;
                            var->file = cfile;
                            var->sline = sline;
                            var->epoint = epoint;
                        }
                    }
                    ignore();if (here() != ',') {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    lpoint.pos++;

                    s = new_star(vline, &starexists); stree_old = star_tree; ovline = vline;
                    if (starexists && s->addr != star) {
                        linepos_t nopoint = {0,0};
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, nopoint);
                        fixeddig=0;
                    }
                    s->addr = star;
                    star_tree = &s->tree;vline=0;
                    xlin=lin=sline; xpos=pos=cfile->p; apoint = lpoint;
                    strcpy((char *)expr, (const char *)pline);var = NULL;
                    for (;;) {
                        lpoint=apoint;
                        if (!get_exp(&w,1)) break; /* ellenorizve. */
                        if (!(val = get_val(T_NONE, NULL))) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        }
                        if (!val_truth(val)) break;
                        if (nopos < 0) {
                            str_t varname;
                            ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                            lpoint.pos++;ignore();
                            epoint = lpoint;
                            varname.data = pline + lpoint.pos; varname.len = get_label();
                            if (!varname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL);break;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                            lpoint.pos++;ignore();
                            if (!here() || here()==';') {bpoint.pos = bpoint.upos = 0; nopos = 0;}
                            else {
                                int labelexists;
                                var=new_label(&varname, mycontext, L_VAR, &labelexists);
                                if (labelexists) {
                                    if (var->type != L_VAR) {
                                        err_msg_double_defined(&var->name, var->file->realname, var->sline, var->epoint, &varname, epoint);
                                        break;
                                    }
                                    var->requires = current_section->requires;
                                    var->conflicts = current_section->conflicts;
                                } else {
                                    var->requires = current_section->requires;
                                    var->conflicts = current_section->conflicts;
                                    var->usepass = pass;
                                    var->defpass = pass;
                                    var->value = &none_value;
                                    var->file = cfile;
                                    var->sline = sline;
                                    var->epoint = epoint;
                                }
                                bpoint=lpoint; nopos = 1;
                            }
                        }
                        new_waitfor(W_NEXT2, epoint);waitfor->skip=1;
                        compile(cfile);
                        xpos = cfile->p; xlin= sline;
                        pline = expr;
                        sline=lin;cfile->p=pos;
                        if (nopos > 0) {
                            lpoint = bpoint;
                            if (!get_exp(&w,1)) break; /* ellenorizve. */
                            if (!(val = get_vals_tuple(T_IDENTREF))) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                            var_assign(var, val, fixeddig);
                            val_destroy(val);
                            ignore();if (here() && here()!=';') {err_msg(ERROR_EXTRA_CHAR_OL,NULL);break;}
                        }
                    }
                    if (pos!=xpos || lin!=xlin) close_waitfor(W_NEXT);
                    sline=xlin;cfile->p=xpos;
                    star_tree = stree_old; vline = ovline;
                    goto breakerr;
                }
                if (prm==CMD_PAGE) { /* .page */
                    new_waitfor(W_ENDP2, epoint);waitfor->addr = current_section->address;waitfor->laddr = current_section->l_address;waitfor->label=newlabel;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    newlabel=NULL;
                    break;
                }
                if (prm==CMD_OPTION) { /* .option */
                    static const str_t branch_across = {24, (const uint8_t *)"allow_branch_across_page"};
                    static const str_t longjmp = {22, (const uint8_t *)"auto_longbranch_as_jmp"};
                    str_t optname;
                    optname.data = pline + lpoint.pos; optname.len = get_label();
                    if (!optname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); goto breakerr;}
                    lpoint.pos++;
                    if (!get_exp(&w,0)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->type == T_NONE) {
                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                        fixeddig = 0;
                    } else if (!str_casecmp(&optname, &branch_across)) allowslowbranch=val_truth(val);
                    else if (!str_casecmp(&optname, &longjmp)) longbranchasjmp=val_truth(val);
                    else {
                        char *s = malloc(optname.len + 1);
                        if (!s) err_msg_out_of_memory();
                        memcpy(s, optname.data, optname.len);
                        s[optname.len] = '\0';
                        err_msg(ERROR_UNKNOWN_OPTIO, s);
                        free(s);
                    }
                    break;
                }
                if (prm==CMD_GOTO) { /* .goto */
                    int noerr = 1;
                    if (!get_exp(&w,0)) goto breakerr;
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    eval_finish();
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (val->type != T_LBL) {err_msg_wrong_type(val, epoint); goto breakerr;}
                    if (val->u.lbl.file == cfile && val->u.lbl.parent == current_context) {
                        while (val->u.lbl.waitforp < waitfor_p) {
                            const char *msg = NULL;
                            line_t os = sline;
                            sline = waitfor->line;
                            switch (waitfor->what) {
                            case W_SWITCH2:
                            case W_SWITCH: msg = ".ENDSWITCH"; break;
                            case W_ENDM2:
                            case W_ENDM: msg = ".ENDM"; break;
                            case W_ENDF2:
                            case W_ENDF: msg = ".ENDF"; break;
                            case W_NEXT2:
                            case W_NEXT: msg = ".NEXT"; break;
                            case W_PEND: msg = ".PEND"; break;
                            case W_BEND2:
                            case W_BEND: msg = ".BEND"; break;
                            case W_ENDS2:
                            case W_ENDS: msg = ".ENDS"; break;
                            case W_SEND2:
                            case W_SEND: msg = ".SEND"; break;
                            case W_ENDU2:
                            case W_ENDU: msg = ".ENDU"; break;
                            case W_ENDP2:
                            case W_ENDP: msg = ".ENDP"; break;
                            case W_HERE2:
                            case W_HERE: msg = ".HERE"; break;
                            case W_ENDC: msg = ".ENDC"; break;
                            case W_NONE:
                            case W_FI:
                            case W_FI2: break;
                            }
                            if (msg) {
                                err_msg2(ERROR______EXPECTED, msg, waitfor->epoint);
                                noerr = 0;
                            }
                            close_waitfor(waitfor->what); sline = os;
                        }
                        if (noerr) {
                            sline = val->u.lbl.sline;
                            cfile->p = val->u.lbl.p;
                        }
                    } else err_msg_not_defined(NULL, epoint);
                    break;
                }
                if (prm==CMD_MACRO || prm==CMD_SEGMENT) {
                    new_waitfor(W_ENDM, epoint);waitfor->skip=0;
                    err_msg_not_defined(NULL, epoint);
                    break;
                }
                if (prm==CMD_FUNCTION) {
                    new_waitfor(W_ENDF, epoint);waitfor->skip=0;
                    err_msg_not_defined(NULL, epoint);
                    break;
                }
                if (prm==CMD_LBL) {
                    err_msg_not_defined(NULL, epoint);
                    break;
                }
                if (prm==CMD_PROC) {
                    new_waitfor(W_PEND, epoint);waitfor->skip=0;waitfor->label = NULL;
                    err_msg_not_defined(NULL, epoint);
                    break;
                }
                if (prm==CMD_STRUCT) {
                    int old_unionmode = current_section->unionmode;
                    current_section->unionmode = 0;
                    new_waitfor(W_ENDS, epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDS2;waitfor->skip=1;
                        compile(cfile);
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
                    new_waitfor(W_ENDU, epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDU2;waitfor->skip=1;
                        compile(cfile);
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
                    if (!get_exp(&w,1)) goto breakerr;
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->type != T_STRUCT) {err_msg_wrong_type(val, epoint); goto breakerr;}
                    current_section->structrecursion++;
                    macro_recurse(W_ENDS2, val, current_context);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    break;
                }
                if (prm==CMD_DUNION) {
                    int old_unionmode = current_section->unionmode;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    if (!get_exp(&w,1)) goto breakerr;
                    if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val == &error_value) goto breakerr;
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->type != T_UNION) {err_msg_wrong_type(val, epoint); goto breakerr;}
                    current_section->structrecursion++;
                    macro_recurse(W_ENDU2, val, current_context);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    break;
                }
                if (prm==CMD_DSECTION) {
                    struct section_s *tmp3;
                    int labelexists;
                    str_t sectionname;
                    if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".DSECTION");
                    ignore();epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp3=new_section(&sectionname, &labelexists);
                    if (tmp3->declared && pass == 1) err_msg_double_defined(&tmp3->name, tmp3->file, tmp3->sline, tmp3->epoint, &sectionname, epoint);
                    else {
                        address_t t;
                        if (!tmp3->declared) {
                            if (!labelexists) {
                                tmp3->wrapwarn = tmp3->wrapwarn2 = tmp3->moved = 0;
                                tmp3->end = tmp3->start = tmp3->address = current_section->address;
                                tmp3->l_start = tmp3->l_address = current_section->l_address;
                            } else {
                                tmp3->address += current_section->address;
                                tmp3->start += current_section->address;
                                tmp3->end += current_section->address;
                                tmp3->l_address += current_section->l_address;
                                tmp3->l_start += current_section->l_address;
                            }
                            tmp3->pass = pass;
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
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
                            t = tmp3->end - tmp3->start;
                            tmp3->end = tmp3->start = current_section->address;
                            tmp3->l_start = current_section->l_address;
                        } else {
                            if (!tmp3->moved) {
                                if (tmp3->end < tmp3->address) tmp3->end = tmp3->address;
                                tmp3->moved=1;
                            }
                            tmp3->wrapwarn = tmp3->wrapwarn2 = 0;
                            t = tmp3->end - tmp3->start;
                            tmp3->end = tmp3->start = tmp3->address = current_section->address;
                            tmp3->l_start = tmp3->l_address = current_section->l_address;
                        }
                        tmp3->size = t;
                        tmp3->pass=pass;
                        memskip(t);
                    }
                    break;
                }
                if (prm==CMD_SECTION) {
                    struct section_s *tmp;
                    str_t sectionname;
                    int labelexists;
                    new_waitfor(W_SEND, epoint);waitfor->section=current_section;
                    ignore();epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp=find_new_section(&sectionname, &labelexists);
                    if (!tmp->declared) {
                        if (!labelexists) {
                            tmp->end = tmp->start = tmp->address = 0;
                            tmp->l_start = tmp->l_address = 0;
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            fixeddig = 0;
                        } else if (pass > 1) {
                            err_msg_not_defined(&sectionname, epoint); goto breakerr;
                        }
                    } else if (tmp->pass != pass) {
                        if (!tmp->moved) {
                            if (tmp->end < tmp->address) tmp->end = tmp->address;
                            tmp->moved=1;
                        }
                        tmp->wrapwarn = tmp->wrapwarn2 = 0;
                        tmp->address = tmp->start;
                        tmp->l_address = tmp->l_start;
                    }
                    tmp->pass = pass;
                    waitfor->what = W_SEND2;
                    current_section = tmp;
                    memjmp(current_section->address);
                    newlabel = NULL;
                    break;
                }
            }
        case WHAT_HASHMARK:if (waitfor->skip & 1) /* skip things if needed */
            {                   /* macro stuff */
                if (!get_exp_var()) goto breakerr;
                if (!(val = get_val(T_NONE, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (val == &error_value) goto breakerr;
                if (val->type == T_NONE) {
                    if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                    fixeddig = 0;
                    goto breakerr;
                }
                if (val->type != T_MACRO && val->type != T_SEGMENT && val->type != T_FUNCTION) {err_msg_wrong_type(val, epoint); goto breakerr;}
            as_macro:
                if (listing && flist && arguments.source && wasref) {
                    if (lastl!=LIST_CODE) {putc('\n',flist);lastl=LIST_CODE;}
                    fprintf(flist,(all_mem==0xffff)?".%04" PRIaddress "\t\t\t\t\t":".%06" PRIaddress "\t\t\t\t\t",current_section->address);
                    if (labelname.len) {
                        if (labelname.data[0] == '-' || labelname.data[0] == '+') fputc(labelname.data[0], flist);
                        else fwrite(labelname.data, labelname.len, 1, flist);
                    }
                    fputc('\n', flist);
                }
                if (val->type == T_MACRO) {
                    struct label_s *context;
                    if (newlabel) {
                        newlabel->nested = 1;
                        context=newlabel;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        sprintf(reflabel, "#%" PRIxPTR "#%" PRIxline, (uintptr_t)star_tree, vline);
                        tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                        context=new_label(&tmpname, mycontext, L_LABEL, &labelexists);
                        context->value = &none_value;
                    }
                    macro_recurse(W_ENDM2, val, context);
                } else if (val->type == T_FUNCTION) {
                    struct label_s *context;
                    int labelexists;
                    str_t tmpname;
                    sprintf(reflabel, "#%" PRIxPTR "#%" PRIxline, (uintptr_t)star_tree, vline);
                    tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                    context=new_label(&tmpname, val->u.func.context, L_LABEL, &labelexists);
                    context->value = &none_value;
                    func_recurse(W_ENDF2, val, context);
                } else macro_recurse(W_ENDM2, val, current_context);
                break;
            }
        case WHAT_EXPRESSION:
            if (waitfor->skip & 1) {
                enum { AG_ZP, AG_B0, AG_PB, AG_BYTE, AG_DB3, AG_NONE } adrgen;
                str_t opname;

                opname.data = pline + lpoint.pos; opname.len = get_label();
                if (opname.len == 3 && (prm=lookup_opcode((const char *)opname.data))>=0) {
                    enum opr_e opr;
                    int mnem;
                    linepos_t oldlpoint;
                    const uint8_t *cnmemonic; /* current nmemonic */
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
                    }  /* clc */
                    /* 1 Db */
                    else if (lowcase(wht)=='a' && cnmemonic[ADR_ACCU]!=____ && (!pline[lpoint.pos+1] || pline[lpoint.pos+1]==';' || pline[lpoint.pos+1]==0x20 || pline[lpoint.pos+1]==0x09))
                    {
                        static const str_t alabel = {1, (const uint8_t *)"a"};
                        linepos_t opoint=lpoint;
                        lpoint.pos++;ignore();
                        if (here() && here()!=';') {lpoint=opoint;goto nota;}
                        if (find_label(&alabel)) err_msg(ERROR_A_USED_AS_LBL,NULL);
                        opr=ADR_ACCU;w=ln=0;d=1;/* asl a */
                    }
                    /* 2 Db */
                    else {
                        int c;
                    nota:
                        if (!(c=get_exp(&w, 3))) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(T_ADDRESS, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val == &error_value) d = 0;
                        else if (val->type == T_NONE) {
                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                            d = fixeddig = 0;
                        } else d = 1;

                        if (val->type == T_ADDRESS) {
                            adr = val->u.addr.val;
                            switch (val->u.addr.type) {
                            case A_IMMEDIATE:
                                if ((cod=cnmemonic[(opr=ADR_IMMEDIATE)])==____ && prm) { /* 0x69 hack */
                                    lpoint.pos += strlen((const char *)pline + lpoint.pos);ln=w=d=1;
                                } else {
                                    ln=1;
                                    if (cod==0xE0 || cod==0xC0 || cod==0xA2 || cod==0xA0) {/* cpx cpy ldx ldy */
                                        if (longindex && scpumode) ln++;
                                    }
                                    else if (cod==0xF4) ln=2; /* pea #$ffff */
                                    else if (cod!=0xC2 && cod!=0xE2) {/* not sep rep=all accu */
                                        if (longaccu && scpumode) ln++;
                                    }

                                    if (w==3) w = ln - 1;
                                    else if (w != ln - 1) w = 3;
                                    if (val->type != T_NONE) {
                                        if (!w && ln == 1 && (val->u.addr.len <= 8 || !(adr & ~(address_t)0xff))) {}
                                        else if (w == 1 && ln == 2 && (val->u.addr.len <= 16 || !(adr & ~(address_t)0xffff))) {}
                                        else w = 3;
                                    }
                                }
                                break;
                            case A_XR:
                                adrgen = AG_DB3; opr=ADR_ZP_X; /* lda $ff,x lda $ffff,x lda $ffffff,x */
                                break;
                            case A_YR: /* lda $ff,y lda $ffff,y lda $ffffff,y */
                                if (w==3) {/* auto length */
                                    if (d) {
                                        if (cnmemonic[ADR_ZP_Y]!=____ && !(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) {adr = (uint16_t)(adr - dpage);w = 0;}
                                        else if (databank==(adr >> 16)) {w = 1;}
                                    } else w=(cnmemonic[ADR_ADDR_Y]!=____);
                                } else if (d) {
                                        if (!w && !(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) adr = (uint16_t)(adr - dpage);
                                        else if (w == 1 && databank == (adr >> 16)) {}
                                        else w=3;
                                } else if (w > 1) w = 3;
                                opr=ADR_ZP_Y-w;ln=w+1; /* ldx $ff,y lda $ffff,y */
                                break;
                            case A_SR:
                                adrgen = AG_BYTE; opr=ADR_ZP_S; /* lda $ff,s */
                                break;
                            case A_RR:
                                adrgen = AG_BYTE; opr=ADR_ZP_R; /* lda $ff,r */
                                break;
                            case (A_I << 4) | A_YR:
                                adrgen = AG_ZP; opr=ADR_ZP_I_Y; /* lda ($ff),y */
                                break;
                            case (A_I << 4) | A_ZR:
                                adrgen = AG_ZP; opr=ADR_ZP_I_Z; /* lda ($ff),z */
                                break;
                            case (A_SR << 8) | (A_I << 4) | A_YR:
                                adrgen = AG_BYTE; opr=ADR_ZP_S_I_Y; /* lda ($ff,s),y */
                                break;
                            case (A_RR << 8) | (A_I << 4) | A_YR:
                                adrgen = AG_BYTE; opr=ADR_ZP_R_I_Y; /* lda ($ff,r),y */
                                break;
                            case (A_LI << 4) | A_YR:
                                adrgen = AG_ZP; opr=ADR_ZP_LI_Y; /* lda [$ff],y */
                                break;
                            case (A_XR << 4) | A_I:
                                if (cnmemonic[ADR_ADDR_X_I]==0x7C || cnmemonic[ADR_ADDR_X_I]==0xFC || cnmemonic[ADR_ADDR_X_I]==0x23) {/* jmp ($ffff,x) jsr ($ffff,x) */
                                    adrgen = AG_PB; opr=ADR_ADDR_X_I; /* jmp ($ffff,x) */
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_X_I; /* lda ($ff,x) */
                                }
                                break;
                            case A_I:
                                if (cnmemonic[ADR_ADDR_I]==0x6C || cnmemonic[ADR_ADDR_I]==0x22) {/* jmp ($ffff), jsr ($ffff) */
                                    if (d && opcode!=c65816 && opcode!=c65c02 && opcode!=cr65c02 && opcode!=cw65c02 && opcode!=c65ce02 && opcode!=c65el02 && !(~adr & 0xff)) err_msg(ERROR______JUMP_BUG,NULL);/* jmp ($xxff) */
                                    adrgen = AG_B0; opr=ADR_ADDR_I; /* jmp ($ffff) */
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_I; /* lda ($ff) */
                                }
                                break;
                            case A_LI:
                                if (cnmemonic[ADR_ADDR_LI]==0xDC) { /* jmp [$ffff] */
                                    adrgen = AG_B0; opr=ADR_ADDR_LI; /* jmp [$ffff] */
                                } else {
                                    adrgen = AG_ZP; opr=ADR_ZP_LI; /* lda [$ff] */
                                }
                                break;
                            default:w = 0;break; /* non-existing */
                            }
                        } else {
                            adr = val->u.num.val;
                            if (cnmemonic[ADR_MOVE]!=____) {
                                struct value_s *val2;
                                if (w==3) {/* auto length */
                                    if (d) {
                                        if (!((uval_t)val->u.num.val & ~(uval_t)0xff)) {adr = (uval_t)val->u.num.val << 8; w = 0;}
                                    } else w = 0;
                                } else if (d) {
                                    if (!w && (!((uval_t)val->u.num.val & ~(uval_t)0xff))) adr = (uval_t)val->u.num.val << 8;
                                    else w = 3;
                                } else if (w) w = 3; /* there's no mvp $ffff or mvp $ffffff */
                                if ((val2 = get_val(T_UINT, NULL))) {
                                    if (val2 == &error_value) d = 0;
                                    else if (val->type == T_NONE) {
                                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                        d = fixeddig = 0;
                                    }
                                    if (!((uval_t)val2->u.num.val & ~(uval_t)0xff)) {adr |= (uint8_t)val2->u.num.val;}
                                    else w = 3;
                                } else err_msg(ERROR_ILLEGAL_OPERA,NULL);
                                ln = 2; opr=ADR_MOVE;
                            } else if (cnmemonic[ADR_BIT_ZP]!=____) {
                                if (w==3) {/* auto length */
                                    if (d) {
                                        if (!((uval_t)val->u.num.val & ~(uval_t)7)) {longbranch = (uval_t)val->u.num.val << 4; w = 0;}
                                    } else w = 0;
                                } else if (d) {
                                    if (!w && (!((uval_t)val->u.num.val & ~(uval_t)7))) longbranch = (uval_t)val->u.num.val << 4;
                                    else w = 3;
                                } else if (w) w = 3; /* there's no rmb $ffff,xx or smb $ffffff,xx */
                                if (w != 3) {
                                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                    if (val == &error_value) d = 0;
                                    else if (val->type == T_NONE) {
                                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                        d = fixeddig = 0;
                                    }
                                    adr = val->u.num.val;
                                    adrgen = AG_ZP; opr=ADR_BIT_ZP; w = 3;
                                }
                            } else if (cnmemonic[ADR_BIT_ZP_REL]!=____) {
                                if (w==3) {/* auto length */
                                    if (d) {
                                        if (!((uval_t)val->u.num.val & ~(uval_t)7)) {longbranch = (uval_t)val->u.num.val << 4; w = 0;}
                                    } else w = 0;
                                } else if (d) {
                                    if (!w && (!((uval_t)val->u.num.val & ~(uval_t)7))) longbranch = (uval_t)val->u.num.val << 4;
                                    else w = 3;
                                } else if (w) w = 3; /* there's no rmb $ffff,xx or smb $ffffff,xx */
                                if (w != 3) {
                                    if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                    if (val == &error_value) d = 0;
                                    else if (val->type == T_NONE) {
                                        if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                        d = fixeddig = 0;
                                    }
                                    adr = val->u.num.val;
                                    w = 3;
                                    if (d) {
                                        if (!((uval_t)val->u.num.val & ~(uval_t)0xffff) && (uint16_t)(val->u.num.val - dpage) < 0x100) {adr = (uint16_t)(val->u.num.val - dpage);w = 0;}
                                    } else w = 1;
                                    if (w != 3) {
                                        uint32_t adr2=0;
                                        if (!(val = get_val(T_UINT, &epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                                        if (val == &error_value) d = 0;
                                        else if (val->type == T_NONE) {
                                            if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                            d = fixeddig = 0;
                                        }
                                        adr2 = val->u.num.val;
                                        w=3;
                                        if (d) {
                                            if (((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff) {
                                                err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;
                                            } else {
                                                adr2=(uint16_t)(adr2 - current_section->l_address - 3);
                                                if (adr2>=0xFF80 || adr2<=0x007F) {
                                                    adr |= ((uint8_t)adr2) << 8; w = 1;
                                                } else {
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
                                int labelexists;
                                int dist = 0;
                                s = new_star(vline+1, &labelexists);olabelexists=labelexists;

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
                                                if ((cnmemonic[ADR_REL] & 0x1f)==0x10) {/* branch */
                                                    longbranch=0x20;ln=4;
                                                    if (scpumode && !longbranchasjmp) {
                                                        if (!labelexists) adr=(uint16_t)(adr-3);
                                                        adr=0x8203+(adr << 16);
                                                    } else {
                                                        adr=0x4C03+(oadr << 16);
                                                    }
                                                } else {/* bra */
                                                    if (scpumode && !longbranchasjmp) {
                                                        longbranch=cnmemonic[ADR_REL]^0x82;
                                                        if (!labelexists) adr=(uint16_t)(adr-1);
                                                        ln=2;
                                                    } else if (cnmemonic[ADR_REL] == 0x82 && opcode==c65el02) {
                                                        int dist2 = (int16_t)adr; dist2 += (dist < 0) ? 0x80 : -0x7f;
                                                        if (!dist || ((dist2 > 0) ? dist2 : -dist2) < ((dist > 0) ? dist : -dist)) dist = dist2;
                                                        err = ERROR_BRANCH_TOOFAR;continue; /* rer not a branch */
                                                    } else {
                                                        longbranch=cnmemonic[ADR_REL]^0x4C;
                                                        adr=oadr;ln=2;
                                                    }
                                                }
                                                /* err = ERROR___LONG_BRANCH; */
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
                                            if (!longbranch && ((uint16_t)(current_section->l_address+2) & 0xff00)!=(oadr & 0xff00)) {
                                                if (!allowslowbranch) {err=ERROR__BRANCH_CROSS;continue;}
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
                                w=0;/* bne */
                                if (olabelexists && s->addr != ((star + 1 + ln) & all_mem)) {
                                    if (fixeddig && pass > MAX_PASS) err_msg_cant_calculate(NULL, epoint);
                                    fixeddig=0;
                                } else if (min == 10) err_msg2(err, &dist, epoint);
                                s->addr = (star + 1 + ln) & all_mem;
                            }
                            else if (cnmemonic[ADR_REL_L]!=____) {
                                if (w==3) {
                                    if (d) {
                                        if (!(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {
                                            adr = (uint16_t)(val->u.num.val-current_section->l_address-3); w = 1;
                                        } else {if (d) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;}
                                    } else w = 1;
                                } else if (d) {
                                    if (w == 1 && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) adr = (uint16_t)(val->u.num.val-current_section->l_address-3);
                                    else w = 3; /* there's no brl $ffffff! */
                                } else if (w != 1) w = 3;
                                opr=ADR_REL_L; ln = 2; /* brl */
                            }
                            else if (cnmemonic[ADR_LONG]==0x5C) {
                                if (w==3) {/* auto length */
                                    if (d) {
                                        if (cnmemonic[ADR_ADDR]!=____ && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {adr = (uval_t)val->u.num.val;w = 1;}
                                        else if (!((uval_t)val->u.num.val & ~(uval_t)0xffffff)) {adr = (uval_t)val->u.num.val; w = 2;}
                                    } else w = (cnmemonic[ADR_ADDR]==____) + 1;
                                } else if (d) {
                                    if (w == 1 && !(((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) adr = (uval_t)val->u.num.val;
                                    else if (w == 2 && !((uval_t)val->u.num.val & ~(uval_t)0xffffff)) adr = (uval_t)val->u.num.val;
                                    else w = 3;
                                }
                                opr=ADR_ZP-w;ln=w+1; /* jml */
                            }
                            else if (cnmemonic[ADR_ADDR]==0x20) {
                                if ((((uval_t)current_section->l_address ^ (uval_t)val->u.num.val) & ~(uval_t)0xffff)) {
                                    if (d) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);w = 1;
                                } else adrgen = AG_PB;
                                opr=ADR_ADDR; ln = 2; /* jsr $ffff */
                            } else {
                                adrgen = AG_DB3; opr=ADR_ZP; /* lda $ff lda $ffff lda $ffffff */
                            }
                        }
                        eval_finish();
                    }
                    switch (adrgen) {
                    case AG_ZP: /* zero page address only */
                        if (w==3) {/* auto length */
                            if (d) {
                                if (!(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) {adr = (uint16_t)(adr - dpage);w = 0;}
                            } else w = 0;
                        } else if (d) {
                            if (!w && !(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) adr = (uint16_t)(adr - dpage);
                            else w=3; /* there's no $ffff] or $ffffff! */
                        } else if (w) w = 3;
                        ln = 1;
                        break;
                    case AG_B0: /* bank 0 address only */
                        if (w==3) {
                            if (d) {
                                if (!(adr & ~(address_t)0xffff)) {adr = (uint16_t)adr; w = 1;}
                            } else w = 1;
                        } else if (d) {
                            if (w == 1 && !(adr & ~(address_t)0xffff)) adr = (uint16_t)adr;
                            else w=3; /* there's no jmp $ffffff! */
                        } else if (w != 1) w = 3;
                        ln = 2;
                        break;
                    case AG_PB: /* address in program bank */
                        if (w==3) {
                            if (d) {
                                if (!((current_section->l_address ^ adr) & ~(address_t)0xffff)) {adr = (uint16_t)adr; w = 1;}
                            } else w = 1;
                        } else if (d) {
                            if (w == 1 && !((current_section->l_address ^ adr) & ~(address_t)0xffff)) adr = (uint16_t)adr;
                            else w = 3; /* there's no jsr ($ffff,x)! */
                        } else if (w != 1) w = 3;
                        ln = 2;
                        break;
                    case AG_BYTE: /* byte only */
                        if (w==3) {/* auto length */
                            if (d) {
                                if (!(adr & ~(address_t)0xff)) {w = 0;}
                            } else w = 0;
                        } else if (d) {
                            if (!w && !(adr & ~(address_t)0xff)) {}
                            else w = 3;
                        } else if (w) w = 3; /* there's no lda ($ffffff,s),y or lda ($ffff,s),y! */
                        ln = 1;
                        break;
                    case AG_DB3: /* 3 choice data bank */
                        if (w==3) {/* auto length */
                            if (d) {
                                if (cnmemonic[opr]!=____ && !(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) {adr = (uint16_t)(adr - dpage);w = 0;}
                                else if (cnmemonic[opr - 1] != ____ && databank == (adr >> 16)) {w = 1;}
                                else if (!(adr & ~(address_t)0xffffff)) {w = 2;}
                            } else w=(cnmemonic[opr - 1]!=____);
                        } else if (d) {
                            if (!w && !(adr & ~(address_t)0xffff) && (uint16_t)(adr - dpage) < 0x100) adr = (uint16_t)(adr - dpage);
                            else if (w == 1 && databank == (adr >> 16)) {}
                            else if (w == 2 && !(adr & ~(address_t)0xffffff)) {}
                            else w = 3;
                        }
                        opr -= w;ln = w + 1;
                        break;
                    case AG_NONE:
                        break;
                    }

                    if (d) {
                        if (w==3) {err_msg(ERROR_CONSTNT_LARGE,NULL); goto breakerr;}
                        if ((cod=cnmemonic[opr])==____ && (prm || opr!=ADR_IMMEDIATE)) { /* 0x69 hack */
                            uint8_t nm[3];
                            str_t nmname = {3, nm};
                            nm[0]=mnemonic[mnem] >> 16;
                            nm[1]=mnemonic[mnem] >> 8;
                            nm[2]=mnemonic[mnem];

                            tmp2=find_label(&nmname);
                            if (tmp2) {
                                int rec = 100;
                                while (tmp2->value->type == T_IDENTREF) {
                                    tmp2 = tmp2->value->u.identref;
                                    if (!rec--) {
                                        err_msg2(ERROR__REFRECURSION, NULL, epoint);
                                        break;
                                    }
                                }
                                if (tmp2->type == L_LABEL && (tmp2->value->type == T_MACRO || tmp2->value->type == T_SEGMENT || tmp2->value->type == T_FUNCTION)) {
                                    lpoint=oldlpoint;
                                    val = tmp2->value;
                                    goto as_macro;
                                }
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
                                case ADR_LEN: break;/* not an addressing mode */
                                }
                            } else if (arguments.source) putc('\t',flist);
                        } else if (arguments.source) fputs("\t\t\t", flist);
                        if (arguments.source) {
                            putc('\t', flist);printllist(flist);
                        } else putc('\n',flist);
                    }
                    break;
                }
                tmp2=find_label(&opname);
                if (tmp2) {
                    int rec = 100;
                    while (tmp2->value->type == T_IDENTREF) {
                        tmp2 = tmp2->value->u.identref;
                        if (!rec--) {
                            err_msg2(ERROR__REFRECURSION, NULL, epoint);
                            break;
                        }
                    }
                    if (tmp2->type == L_LABEL && (tmp2->value->type == T_MACRO || tmp2->value->type == T_SEGMENT || tmp2->value->type == T_FUNCTION)) {val = tmp2->value;goto as_macro;}
                }
            }            /* fall through */
        default: if (waitfor->skip & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr; /* skip things if needed */
        }
    finish:
        ignore();if (here() && here()!=';' && (waitfor->skip & 1)) err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    breakerr:
        if (newlabel) set_size(newlabel, current_section->address - oaddr, newmemp, newmembp);
        continue;
    }

    while (oldwaitforp < waitfor_p) {
        const char *msg = NULL;
        line_t os = sline;
        sline = waitfor->line;
        switch (waitfor->what) {
        case W_FI2:
        case W_FI: msg = ".FI"; break;
        case W_SWITCH2:
        case W_SWITCH: msg = ".ENDSWITCH"; break;
        case W_ENDP2:
        case W_ENDP: msg = ".ENDP"; break;
        case W_ENDM2:
        case W_ENDM: msg = ".ENDM"; break;
        case W_ENDF2:
        case W_ENDF: msg = ".ENDF"; break;
        case W_NEXT2:
        case W_NEXT: msg = ".NEXT"; break;
        case W_PEND: msg = ".PEND"; break;
        case W_BEND2:
        case W_BEND: msg = ".BEND"; break;
        case W_ENDC: msg = ".ENDC"; break;
        case W_ENDS2:
        case W_ENDS: msg = ".ENDS"; break;
        case W_SEND2:
        case W_SEND: msg = ".SEND"; break;
        case W_ENDU2:
        case W_ENDU: msg = ".ENDU"; break;
        case W_HERE2:
        case W_HERE: msg = ".HERE"; break;
        case W_NONE: break;
        }
        if (msg) err_msg2(ERROR______EXPECTED, msg, waitfor->epoint);
        close_waitfor(waitfor->what); sline = os;
    }
    return retval;
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
    struct file_s *fin, *cfile;
    static const str_t none_enc = {4, (const uint8_t *)"none"};

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
            star=databank=dpage=longaccu=longindex=0;actual_encoding=new_encoding(&none_enc);
            allowslowbranch=1;
            reset_waitfor();sline=vline=0;outputeor=0;forwr=backr=0;
            current_context=&root_label;
            current_section=&root_section;
            reset_section();
            init_macro();
            /*	listing=1;flist=stderr;*/
            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0;
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(fin);
                exitfile();
                restart_mem();
                continue;
            }
            memjmp(current_section->address);
            cfile = openfile(argv[i], "", 0, NULL);
            enterfile(argv[i],0);
            if (cfile) {
                cfile->p = 0;
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cfile);
                closefile(cfile);
            }
            exitfile();
        }
        if (errors) {memcomp();status();return 1;}
    } while (!fixeddig || pass==1);

    /* assemble again to create listing */
    if (arguments.list) {
        uint8_t oldpass = pass;
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

        pass = MAX_PASS + 1;
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        restart_mem();
        for (i = optind - 1; i<argc; i++) {
            if (i >= optind) {fprintf(flist,"\n;******  Processing input file: %s\n", argv[i]);}
            lastl=LIST_NONE;
            set_cpumode(arguments.cpumode);
            star=databank=dpage=longaccu=longindex=0;actual_encoding=new_encoding(&none_enc);
            allowslowbranch=1;
            reset_waitfor();sline=vline=0;outputeor=0;forwr=backr=0;
            current_context=&root_label;
            current_section=&root_section;
            reset_section();
            init_macro();

            if (i == optind - 1) {
                enterfile("<command line>",0);
                fin->p = 0; 
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(fin);
                exitfile();
                restart_mem();
                continue;
            }
            memjmp(current_section->address);

            cfile = openfile(argv[i], "", 0, NULL);
            enterfile(argv[i],0);
            if (cfile) {
                cfile->p = 0;
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cfile);
                closefile(cfile);
            }
            exitfile();
        }
	fputs("\n;******  End of listing\n", flist);
	if (flist != stdout) fclose(flist);
        pass = oldpass + 1;
    }
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
