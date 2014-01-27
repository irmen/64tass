/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id$

    6502/65C02 Turbo Assembler  Version 1.3
    (c) 1996 Taboo Productions, Marek Matula

    6502/65C02 Turbo Assembler  Version 1.35  ANSI C port
    (c) 2000 BiGFooT/BReeZe^2000

    6502/65C02/65816/DTV Turbo Assembler  Version 1.4x
    (c) 2001-2014 Soci/Singular (soci@c64.rulez.org)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#define _MAIN_C_
#ifdef _WIN32
#include <windows.h>
#include <wincon.h>
#endif
#include <string.h>

#include <time.h>

#include "64tass.h"
#include "opcodes.h"
#include "misc.h"
#include "eval.h"
#include "section.h"
#include "encoding.h"
#include "file.h"
#include "variables.h"
#include "macro.h"
#include "instruction.h"

#include "listobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "floatobj.h"
#include "addressobj.h"
#include "boolobj.h"
#include "bytesobj.h"
#include "intobj.h"

static struct value_s new_value;

int temporary_label_branch; /* function declaration in function context, not good */
line_t vline;      /* current line */
static address_t all_mem, all_mem2;
uint8_t pass=0, max_pass=MAX_PASS;         /* pass */
static int listing=0;   /* listing */
address_t star=0;
const uint8_t *pline, *llist;   /* current line data */
struct linepos_s lpoint;        /* position in current line */
static FILE* flist = NULL;      /* listfile */
static enum lastl_e lastl;
static int8_t strength=0;
int fixeddig;
static uint8_t outputeor = 0; /* EOR value for final output (usually 0, except changed by .eor) */

static size_t waitfor_p, waitfor_len;
static struct waitfor_s {
    enum wait_e what;
    struct linepos_s epoint;
    address_t addr;
    address_t laddr;
    struct label_s *label, *cheap_label;
    size_t memp, membp;
    struct section_s *section;
    struct value_s *val;
    uint8_t skip;
} *waitfors, *waitfor, *prevwaitfor;

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
    "\x56" "strength",
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
    CMD_ENDF, CMD_SWITCH, CMD_CASE, CMD_DEFAULT, CMD_ENDSWITCH, CMD_STRENGTH
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
        sectionprint();
    }
    destroy_eval();
    tfree();
    free_macro();
    free(waitfors);
}

int printaddr(char c, address_t addr, enum lastl_e mode) {
    if (lastl != mode) {putc('\n', flist);lastl = mode;}
    return fprintf(flist, (all_mem == 0xffff) ? "%c%04" PRIaddress : "%c%06" PRIaddress, c, addr);
}

void printllist(int l) {
    if (listing && flist && arguments.source) {
        const uint8_t *c = llist, *last, *n;
        uint32_t ch;

        if (c) {
            if (l >= 40) {putc('\n', flist); l = 0;}
            while (l < 40) { l += 8; putc('\t', flist);} 
            last = c;
            while ((ch = *c)) {
                if (ch & 0x80) n=c+utf8in(c, &ch); else n=c+1;
                if ((ch < 0x20 || ch > 0x7e) && ch!=9) {
                    fwrite(last, c - last, 1, flist);
                    fprintf(flist, "{$%x}", ch);
                    last=n;
                }
                c = n;
            }
            while (c > last && (c[-1] == 0x20 || c[-1] == 0x09)) c--;
            fwrite(last, c - last, 1, flist);
            llist=NULL;
        }
        putc('\n', flist);
    }
}

void list_instr(uint8_t cod, uint32_t adr, int ln, enum opr_e opr, uint32_t mnem) {
    if (listing && flist) {
        unsigned int i;

        if (lastl != LIST_CODE) {putc('\n', flist); lastl = LIST_CODE;}
        fprintf(flist, (all_mem == 0xffff) ? ".%04" PRIaddress "\t" : ".%06" PRIaddress " ", (current_section->address - ln - 1) & all_mem);
        if (current_section->dooutput) {
            if (ln >= 0) {
                uint32_t temp = adr;
                fprintf(flist, " %02x", cod ^ outputeor);
                for (i = 0; i < (unsigned)ln; i++) {fprintf(flist, " %02x", (uint8_t)temp ^ outputeor); temp >>= 8;}
            }
            if (ln<2) putc('\t', flist);
            putc('\t', flist);
            if (arguments.monitor) {
                for (i = 0; i < 3; i++) putc(mnem >> (16 - 8 * i), flist);

                switch (opr) {
                case ADR_IMPLIED: putc('\t', flist); break;
                case ADR_ACCU: fputs(" a\t", flist); break;
                case ADR_IMMEDIATE: {
                    if (ln == 1) fprintf(flist, " #$%02x", (uint8_t)adr);
                    else fprintf(flist, " #$%04x", (uint16_t)adr);
                    break;
                }
                case ADR_LONG: fprintf(flist, " $%06x", (uint32_t)(adr & 0xffffff)); break;
                case ADR_ADDR: {
                    if (cod == 0x20 || cod == 0x4c)
                        fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, ((uint16_t)adr) | (current_section->l_address & 0xff0000));
                    else fprintf(flist, databank ? " $%06x" : " $%04x", (uint16_t)adr | (databank << 16));
                    break;
                }
                case ADR_ZP: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " $%02x\t" : " $%04x", (uint16_t)(adr + dpage)); break;
                case ADR_BIT_ZP: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " %x,$%02x" : " %d,$%04x", (cod >> 4) & 7, (uint16_t)(adr + dpage)); break;
                case ADR_LONG_X: fprintf(flist, " $%06x,x", (uint32_t)(adr & 0xffffff)); break;
                case ADR_ADDR_X: fprintf(flist, databank ? " $%06x,x" : " $%04x,x", (uint16_t)adr | (databank << 16)); break;
                case ADR_ZP_X: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " $%02x,x" : " $%04x,x", (uint16_t)(adr + dpage)); break;
                case ADR_ADDR_X_I: fprintf(flist, (current_section->l_address & 0xff0000) ? " ($%06" PRIaddress ",x)" : " ($%04" PRIaddress ",x)", ((uint16_t)adr) | (current_section->l_address & 0xff0000)); break;
                case ADR_ZP_X_I: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " ($%02x,x)" : " ($%04x,x)", (uint16_t)(adr + dpage)); break;
                case ADR_ZP_S: fprintf(flist, " $%02x,s", (uint8_t)adr); break;
                case ADR_ZP_S_I_Y: fprintf(flist, " ($%02x,s),y", (uint8_t)adr); break;
                case ADR_ZP_R: fprintf(flist, " $%02x,r", (uint8_t)adr); break;
                case ADR_ZP_R_I_Y: fprintf(flist, " ($%02x,r),y", (uint8_t)adr); break;
                case ADR_ADDR_Y: fprintf(flist, databank ? " $%06x,y" : " $%04x,y", (uint16_t)adr | (databank << 16)); break;
                case ADR_ZP_Y: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " $%02x,y" : " $%04x,y", (uint16_t)(adr + dpage)); break;
                case ADR_ZP_LI_Y: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " [$%02x],y" : " [$%04x],y", (uint16_t)(adr + dpage)); break;
                case ADR_ZP_I_Y: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " ($%02x),y" : " ($%04x),y", (uint16_t)(adr + dpage)); break;
                case ADR_ZP_I_Z: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " ($%02x),z" : " ($%04x),z", (uint16_t)(adr + dpage)); break;
                case ADR_ADDR_LI: fprintf(flist, " [$%04x]", (uint16_t)adr); break;
                case ADR_ZP_LI: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " [$%02x]" : " [$%04x]", (uint16_t)(adr + dpage)); break;
                case ADR_ADDR_I: fprintf(flist, " ($%04x)", (uint16_t)adr); break;
                case ADR_ZP_I: fprintf(flist, ((uint16_t)(adr + dpage)<0x100) ? " ($%02x)" : " ($%04x)", (uint16_t)(adr + dpage)); break;
                case ADR_REL: {
                    if (ln == 1) fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, (uint16_t)(((int8_t)adr) + current_section->l_address) | (current_section->l_address & 0xff0000));
                    else if (ln == 2) {
                        if (cod == 0x4C)
                            fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, ((uint16_t)adr) | (current_section->l_address & 0xff0000));
                        else
                            fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, (uint16_t)(adr + current_section->l_address) | (current_section->l_address & 0xff0000));
                    }
                    else {
                        if ((uint16_t)adr == 0x4C03)
                            fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, ((uint16_t)(adr >> 16)) | (current_section->l_address & 0xff0000));
                        else
                            fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, (uint16_t)((adr >> 16) + current_section->l_address) | (current_section->l_address & 0xff0000));
                    }
                    break;
                }
                case ADR_BIT_ZP_REL:
                               fprintf(flist, ((uint16_t)(((uint8_t)adr) + dpage)<0x100) ? " %x,$%02x" : " %d,$%04x", (cod >> 4) & 7, (uint16_t)((uint8_t)adr) + dpage);
                               fprintf(flist, (current_section->l_address & 0xff0000) ? ",$%06" PRIaddress : ",$%04" PRIaddress, (uint16_t)(((int8_t)(adr >> 8)) + current_section->l_address) | (current_section->l_address & 0xff0000));
                               break;
                case ADR_REL_L: fprintf(flist, (current_section->l_address & 0xff0000) ? " $%06" PRIaddress : " $%04" PRIaddress, (uint16_t)(adr + current_section->l_address) | (current_section->l_address & 0xff0000)); break;
                case ADR_MOVE: fprintf(flist, " $%02x,$%02x", (uint8_t)(adr >> 8), (uint8_t)adr);
                case ADR_LEN: break;/* not an addressing mode */
                }
            } else if (arguments.source) putc('\t', flist);
        } else if (arguments.source) fputs("\t\t\t", flist);
        if (arguments.source) {
            printllist(32);
        } else putc('\n', flist);
    }
}

void new_waitfor(enum wait_e what, linepos_t epoint) {
    waitfor_p++;
    if (waitfor_p >= waitfor_len) {
        waitfor_len += 8;
        waitfors = (struct waitfor_s *)realloc(waitfors, waitfor_len * sizeof(struct waitfor_s));
        if (!waitfors || waitfor_len < 8 || waitfor_len > SIZE_MAX / sizeof(struct waitfor_s)) err_msg_out_of_memory(); /* overflow */
    }
    waitfor = &waitfors[waitfor_p];
    prevwaitfor = waitfor_p ? &waitfors[waitfor_p - 1] : waitfor;
    waitfor->what = what;
    waitfor->epoint = *epoint;
    waitfor->label = NULL;
    waitfor->val = NULL;
    waitfor->skip = prevwaitfor->skip;
}

static void reset_waitfor(void) {
    struct linepos_s lpos = {0,0,0};
    waitfor_p = (size_t)-1;
    new_waitfor(W_NONE, &lpos);
    waitfor->skip=1;
    prevwaitfor = waitfor;
}

static int close_waitfor(enum wait_e what) {
    if (waitfor->what == what) {
        if (waitfor->val) val_destroy(waitfor->val);
        waitfor_p--;
        waitfor = &waitfors[waitfor_p];
        prevwaitfor = waitfor_p ? &waitfors[waitfor_p - 1] : waitfor;
        return 1;
    }
    return 0;
}

static void set_size(struct label_s *var, size_t size, struct memblocks_s *mem, size_t memp, size_t membp) {
    size &= all_mem2;
    if (var->value->u.code.size != size) {
        var->value->u.code.size = size;
        if (var->value->u.code.pass) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&var->name, &var->epoint);
            fixeddig = 0;
        }
    }
    var->value->u.code.pass = pass;
    var->value->u.code.mem = mem;
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
    memjmp(&current_section->mem, current_section->address);
}

/* --------------------------------------------------------------------------- */
/*
 * output one byte
 */
void pokeb(uint8_t byte)
{
    if (current_section->moved) {
        if (current_section->address < current_section->start) err_msg(ERROR_OUTOF_SECTION,NULL);
        if (current_section->wrapwarn) {err_msg(ERROR_TOP_OF_MEMORY,NULL);current_section->wrapwarn=0;}
        current_section->moved = 0;
    }
    if (current_section->wrapwarn2) {err_msg(ERROR___BANK_BORDER,NULL);current_section->wrapwarn2=0;}
    if (current_section->dooutput) write_mem(&current_section->mem, byte ^ outputeor);
    current_section->address++;current_section->l_address++;
    if (current_section->address & ~all_mem2) {
        current_section->wrapwarn = current_section->moved = 1;
        if (current_section->end <= all_mem2) current_section->end = all_mem2 + 1;
        current_section->address = 0;
        memjmp(&current_section->mem, current_section->address);
    }
    if (current_section->l_address & ~all_mem) {
        current_section->wrapwarn2 = 1;
        current_section->l_address = 0;
    }
}

/* --------------------------------------------------------------------------- */
static int what(int *tempno) {
    char ch;

    ignore();
    switch (ch=whatis[(int)here()]) {
    case WHAT_COMMAND:
	{
            unsigned int no, also = 0, felso, elozo;
            const uint8_t *label;
            size_t l;
            int s4;
            lpoint.pos++;
            label = pline + lpoint.pos;
            l = get_label();
            if (l && l < 19) {
                char cmd[20];
                if (!arguments.casesensitive) {
                    size_t i;
                    for (i = 0; i < l; i++) cmd[i] = lowcase(label[i]);
                } else memcpy(cmd, label, l);
                cmd[l] = 0;
                if (l) {
                    felso=sizeof(command)/sizeof(command[0]);
                    no=felso/2;
                    for (;;) {  /* do binary search */
                        if (!(s4=strcmp(cmd, command[no] + 1))) {
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
            }
            lpoint.pos -= l;
	    *tempno=sizeof(command)/sizeof(command[0]);
	    return WHAT_COMMAND;
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

static int get_cpu(char *cpu) {
    int q=1;
    unsigned int i=0, i2;
    i2 = i;
    ignore();
    if (here()=='\"') {lpoint.pos++;q=0;}
    while (here() && (here()!=';' || !q) && (here()!='\"' || q) && i < 20) cpu[i++]=get();
    if (i >= 20 || (!q && here()!='\"')) {err_msg(ERROR_GENERL_SYNTAX, NULL); return 1;}
    if (!q) lpoint.pos++; else while (i && (cpu[i-1]==0x20 || cpu[i-1]==0x09)) i--;
    cpu[i]=0;
    ignore();
    if (i <= i2) {err_msg(ERROR_GENERL_SYNTAX,NULL); return 1;}
    return 0;
}

/* ------------------------------------------------------------------------------ */

static void set_cpumode(uint_fast8_t cpumode) {
    all_mem = (cpumode == OPCODES_C65816) ? 0xffffff : 0xffff;
    all_mem2 = (arguments.output_mode == OUTPUT_FLAT) ? ~(address_t)0 : all_mem;
    return select_opcodes(cpumode);
}

void var_assign(struct label_s *tmp, struct value_s *val, int fix) {
    tmp->defpass = pass;
    if (obj_same(val, tmp->value)) return;
    val_replace(&tmp->value, val);
    if (tmp->usepass < pass) return;
    if (fixeddig && !fix && pass > max_pass) err_msg_cant_calculate(&tmp->name, &tmp->epoint);
    fixeddig = fix;
}

struct value_s *compile(struct file_list_s *cflist)
{
    int wht,w;
    int prm = 0;
    struct value_s *val;

    struct label_s *newlabel = NULL, *oldcheap = NULL;
    size_t newmemp = 0, newmembp = 0;
    struct label_s *tmp2 = NULL, *mycontext;
    address_t oaddr = 0;
    struct value_s *retval = NULL;

    size_t oldwaitforp = waitfor_p;
    unsigned wasref;
    int nobreak = 1;
    str_t labelname;
    char reflabel[100];
    struct linepos_s epoint;
    struct file_s *cfile = cflist->file;

    while (nobreak) {
        if (mtranslate(cfile)) break; /* expand macro parameters, if any */
        llist = pline;
        star=current_section->l_address;newlabel = NULL;
        labelname.len = wasref = 0;ignore();epoint = lpoint; mycontext = current_context;
        if (current_section->unionmode) {
            if (current_section->address > current_section->unionend) current_section->unionend = current_section->address;
            if (current_section->l_address > current_section->l_unionend) current_section->l_unionend = current_section->l_address;
            current_section->l_address = current_section->l_unionstart;
            if (current_section->address != current_section->unionstart) {
                current_section->address = current_section->unionstart;
                memjmp(&current_section->mem, current_section->address);
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
                if (waitfor->skip & 1) err_msg2(ERROR_GENERL_SYNTAX, NULL, &lpoint);
                goto breakerr;
            } /* not label */
            for (;;) {
                labelname.data = pline + lpoint.pos; labelname.len = get_label();
                if (here() != '.') {
                    break;
                }
                if (mycontext == current_context) {
                    tmp2 = (labelname.len && labelname.data[0] == '_') ? find_label2(&labelname, cheap_context) : find_label(&labelname);
                    if (tmp2) tmp2->shadowcheck = (labelname.data[0] != '_');
                }
                else tmp2 = find_label2(&labelname, mycontext);
                if (!tmp2) {err_msg_not_defined(&labelname, &epoint); goto breakerr;}
                if (tmp2->value->obj != CODE_OBJ) {
                    err_msg_not_defined(&labelname, &epoint); goto breakerr;
                }
                mycontext = tmp2;
                lpoint.pos++; islabel = 1; epoint = lpoint;
            }
            if (!islabel && labelname.len && labelname.data[0] == '_') {
                mycontext = cheap_context;
            }
            if (here()==':') {islabel = 1; lpoint.pos++;}
            if (!islabel && labelname.len == 3 && (prm=lookup_opcode((const char *)labelname.data))>=0) {
                if (waitfor->skip & 1) goto as_opcode; else continue;
            }
        hh:
            if (!(waitfor->skip & 1)) {wht=what(&prm);goto jn;} /* skip things if needed */
            if (labelname.len > 1 && labelname.data[0] == '_' && labelname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint); goto breakerr;}
            if ((wht=what(&prm))==WHAT_EQUAL) { /* variable */
                struct label_s *label;
                int labelexists;
                int oldreferenceit = referenceit;
                label = find_label3(&labelname, mycontext, strength);
                ignore();
                if (!here() || here() == ';') val = &null_addrlist;
                else {
                    struct linepos_s epoints[3];
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    referenceit &= label ? label->ref : 1;
                    val = get_vals_addrlist(epoints);
                    referenceit = oldreferenceit;
                }
                if (label) labelexists = 1;
                else label = new_label(&labelname, mycontext, strength, &labelexists);
                oaddr=current_section->address;
                if (listing && flist && arguments.source && label->ref) {
                    int l;
                    if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                    putc('=', flist);
                    referenceit = 0;
                    l = val_print(val, flist) + 1;
                    referenceit = oldreferenceit;
                    printllist(l);
                }
                label->ref = 0;
                if (labelexists) {
                    if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                    else {
                        label->constant = 1;
                        label->requires = current_section->requires;
                        label->conflicts = current_section->conflicts;
                        var_assign(label, val, 0);
                    }
                    val_destroy(val);
                } else {
                    label->constant = 1;
                    label->requires = current_section->requires;
                    label->conflicts = current_section->conflicts;
                    label->value = val;
                    label->file_list = cflist;
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
                        label=find_label3(&labelname, mycontext, strength);
                        ignore();
                        if (!here() || here() == ';') val = &null_addrlist;
                        else {
                            struct linepos_s epoints[3];
                            if (!get_exp(&w, 0,cfile)) goto breakerr; /* ellenorizve. */
                            referenceit &= label ? label->ref : 1;
                            val = get_vals_addrlist(epoints);
                            referenceit = oldreferenceit;
                        }
                        if (!val) {err_msg(ERROR_GENERL_SYNTAX, NULL); goto breakerr;}
                        if (label) labelexists = 1;
                        else label = new_label(&labelname, mycontext, strength, &labelexists);
                        oaddr=current_section->address;
                        if (listing && flist && arguments.source) {
                            int l;
                            if (lastl!=LIST_EQU) {putc('\n',flist);lastl=LIST_EQU;}
                            putc('=', flist);
                            referenceit = 0;
                            l = val_print(val, flist) + 1;
                            referenceit = oldreferenceit;
                            printllist(l);
                        }
                        if (labelexists) {
                            if (label->constant) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (label->defpass != pass) label->ref = 0;
                                label->requires = current_section->requires;
                                label->conflicts = current_section->conflicts;
                                var_assign(label, val, fixeddig);
                            }
                            val_destroy(val);
                        } else {
                            label->constant = 0;
                            label->requires = current_section->requires;
                            label->conflicts = current_section->conflicts;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                        }
                        goto finish;
                    }
                case CMD_LBL:
                    { /* label */
                        struct label_s *label;
                        int labelexists;
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                new_value.obj = LBL_OBJ;
                                new_value.u.lbl.p = cfile->p;
                                new_value.u.lbl.sline = lpoint.line;
                                new_value.u.lbl.waitforp = waitfor_p;
                                new_value.u.lbl.file_list = cflist;
                                new_value.u.lbl.parent = current_context;
                                var_assign(label, &new_value, 0);
                            }
                        } else {
                            val = val_alloc();
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                            val->obj = LBL_OBJ;
                            val->u.lbl.p = cfile->p;
                            val->u.lbl.sline = lpoint.line;
                            val->u.lbl.waitforp = waitfor_p;
                            val->u.lbl.file_list = cflist;
                            val->u.lbl.parent = current_context;
                        }
                        label->ref = 0;
                        goto finish;
                    }
                case CMD_MACRO:/* .macro */
                case CMD_SEGMENT:
                    {
                        struct label_s *label;
                        obj_t obj = (prm == CMD_MACRO) ? MACRO_OBJ : SEGMENT_OBJ;
                        int labelexists;
                        new_waitfor(W_ENDM, &lpoint);waitfor->skip=0;
                        ignore();
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                new_value.obj = obj;
                                new_value.u.macro.p = cfile->p;
                                new_value.u.macro.size = 0;
                                new_value.u.macro.parent = label;
                                get_macro_params(&new_value);
                                var_assign(label, &new_value, 0);
                                val_destroy(&new_value);
                            }
                        } else {
                            val = val_alloc();
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                            val->obj = obj;
                            val->u.macro.p = cfile->p;
                            val->u.macro.size = 0;
                            val->u.macro.parent = label;
                            get_macro_params(val);
                        }
                        label->ref = 0;
                        goto finish;
                    }
                case CMD_FUNCTION:
                    {
                        struct label_s *label;
                        int labelexists;
                        new_waitfor(W_ENDF, &lpoint);waitfor->skip=0;
                        ignore();
                        if (temporary_label_branch) {err_msg(ERROR___NOT_ALLOWED, ".FUNCTION");goto breakerr;}
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                new_value.obj = MFUNC_OBJ;
                                new_value.u.mfunc.p = cfile->p;
                                new_value.u.mfunc.label = label;
                                get_func_params(&new_value, cfile);
                                var_assign(label, &new_value, 0);
                                val_destroy(&new_value);
                            }
                        } else {
                            val = val_alloc();
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                            val->obj = MFUNC_OBJ;
                            val->u.mfunc.p = cfile->p;
                            val->u.mfunc.label = label;
                            get_func_params(val, cfile);
                        }
                        label->ref = 0;
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

                        new_waitfor((prm==CMD_STRUCT)?W_ENDS:W_ENDU, &lpoint);waitfor->skip=0;
                        ignore();
                        label=new_label(&labelname, mycontext, strength, &labelexists);oaddr = current_section->address;
                        if (declaration) {
                            obj_t obj = (prm == CMD_STRUCT) ? STRUCT_OBJ : UNION_OBJ;
                            current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
                            current_section->end=current_section->start=current_section->l_start=current_section->address=current_section->l_address=0;
                            current_section->dooutput=0;memjmp(&current_section->mem, 0); oaddr = 0;

                            if (labelexists) {
                                if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                                else {
                                    label->constant = 1;
                                    label->requires = 0;
                                    label->conflicts = 0;
                                    new_value.obj = obj;
                                    new_value.u.macro.size = (label->value->obj == obj) ? label->value->u.macro.size : 0;
                                    new_value.u.macro.p = cfile->p;
                                    new_value.u.macro.parent = label;
                                    get_macro_params(&new_value);
                                    var_assign(label, &new_value, 0);
                                    val_destroy(&new_value);
                                }
                            } else {
                                val = val_alloc();
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                label->value = val;
                                label->file_list = cflist;
                                label->epoint = epoint;
                                val->obj = obj;
                                val->u.macro.size = 0;
                                val->u.macro.p = cfile->p;
                                val->u.macro.parent = label;
                                get_macro_params(val);
                            }
                        } else {
                            if (labelexists) {
                                if (label->defpass == pass) {
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    label = NULL;
                                } else {
                                    struct value_s tmp;
                                    label->requires = current_section->requires;
                                    label->conflicts = current_section->conflicts;
                                    int_from_uval(&tmp, current_section->l_address);
                                    if (!tmp.obj->same(&tmp, label->value->u.code.addr)) {
                                        size_t size = label->value->u.code.size;
                                        signed char dtype = label->value->u.code.dtype;
                                        val = val_realloc(&label->value);
                                        val->obj = CODE_OBJ;
                                        val_set_template(&val->u.code.addr, &tmp);
                                        val->u.code.pass = pass - 1;
                                        val->u.code.size = size;
                                        val->u.code.dtype = dtype;
                                        val->u.code.parent = label;
                                        if (label->usepass >= pass) {
                                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
                                            fixeddig = 0;
                                        }
                                    } else tmp.obj->destroy(&tmp);
                                    label->defpass = pass;
                                    get_mem(&current_section->mem, &memp, &membp);
                                }
                            } else {
                                val = val_alloc();
                                label->requires=current_section->requires;
                                label->conflicts=current_section->conflicts;
                                label->value = val;
                                label->file_list = cflist;
                                label->epoint = epoint;
                                val->obj = CODE_OBJ;
                                val->u.code.addr = val_alloc();
                                int_from_uval(val->u.code.addr, current_section->l_address);
                                val->u.code.size = 0;
                                val->u.code.dtype = D_NONE;
                                val->u.code.pass = 0;
                                val->u.code.parent = label;
                                get_mem(&current_section->mem, &memp, &membp);
                            }
                        }
                        if (label) {
                            label->ref = 0;
                        }
                        if (listing && flist && arguments.source) {
                            int l;
                            l = printaddr('.', current_section->address, LIST_DATA);
                            printllist(l);
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
                            if (label && (label->value->obj == STRUCT_OBJ || label->value->obj == UNION_OBJ)) val = macro_recurse(W_ENDS, label->value, label, &lpoint);
                            else val = compile(cflist);
                            if (val) val_destroy(val);
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
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
                                    fixeddig = 0;
                                }
                            }
                            current_section->provides=olds.provides;current_section->requires=olds.requires;current_section->conflicts=olds.conflicts;
                            current_section->end=olds.end;current_section->start=olds.start;current_section->l_start=olds.l_start;current_section->address=olds.address;current_section->l_address=olds.l_address;
                            current_section->dooutput=olds.dooutput;memjmp(&current_section->mem, current_section->address);
                        } else {
                            set_size(label, current_section->address - oaddr, &current_section->mem, memp, membp);
                        }
                        goto finish;
                    }
                case CMD_SECTION:
                    {
                        struct section_s *tmp;
                        str_t sectionname;
                        struct linepos_s opoint;
                        new_waitfor(W_SEND, &lpoint);waitfor->section=current_section;
                        ignore();opoint=lpoint;
                        sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                        if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        tmp=find_new_section(&sectionname);
                        if (!tmp->usepass || tmp->defpass < pass - 1) {
                            if (tmp->usepass && tmp->usepass >= pass - 1) {err_msg_not_defined(&sectionname, &opoint); goto breakerr;}
                            tmp->end = tmp->start = tmp->address = 0;
                            tmp->l_start = tmp->l_address = 0;
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&sectionname, &opoint);
                            fixeddig=0;
                            tmp->defpass = pass - 1;
                            restart_memblocks(&tmp->mem, tmp->address);
                        } else if (tmp->usepass != pass) {
                            if (!tmp->moved) {
                                if (tmp->end < tmp->address) tmp->end = tmp->address;
                                tmp->moved = 1;
                            }
                            tmp->wrapwarn = tmp->wrapwarn2 = 0;
                            tmp->address = tmp->start;
                            tmp->l_address = tmp->l_start;
                            restart_memblocks(&tmp->mem, tmp->address);
                        }
                        tmp->usepass = pass;
                        waitfor->what = W_SEND2;
                        current_section = tmp;
                        break;
                    }
                }
            }
            if (!islabel) {
                tmp2 = (labelname.len && labelname.data[0] == '_') ? find_label2(&labelname, cheap_context) : find_label(&labelname);
                if (tmp2) {
                    if (tmp2->value->obj == MACRO_OBJ || tmp2->value->obj == SEGMENT_OBJ || tmp2->value->obj == MFUNC_OBJ) {
                        tmp2->shadowcheck = 1;
                        if (wht == WHAT_HASHMARK) lpoint.pos--;labelname.len=0;val = tmp2->value; goto as_macro;
                    }
                }
            }
            {
                int labelexists;
                if (!islabel && tmp2 && tmp2->parent == current_context && tmp2->strength == strength) {newlabel = tmp2;labelexists = 1;}
                else newlabel=new_label(&labelname, mycontext, strength, &labelexists);
                if (labelname.data[0] != '_') {oldcheap = cheap_context;cheap_context = newlabel;}
                oaddr=current_section->address;
                if (labelexists) {
                    if (newlabel->defpass == pass) {
                        err_msg_double_defined(newlabel, &labelname, &epoint);
                        newlabel = NULL; goto jn;
                    }
                    newlabel->constant = 1;
                    newlabel->requires = current_section->requires;
                    newlabel->conflicts = current_section->conflicts;
                    if (!newlabel->update_after) {
                        struct value_s tmp;
                        int_from_uval(&tmp, current_section->l_address);
                        if (!tmp.obj->same(&tmp, newlabel->value->u.code.addr)) {
                            size_t size = newlabel->value->u.code.size;
                            signed char dtype = newlabel->value->u.code.dtype;
                            val = val_realloc(&newlabel->value);
                            val->obj = CODE_OBJ;
                            val_set_template(&val->u.code.addr, &tmp);
                            val->u.code.pass = pass - 1;
                            val->u.code.size = size;
                            val->u.code.dtype = dtype;
                            val->u.code.parent = newlabel;
                            if (newlabel->usepass >= pass) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
                                fixeddig = 0;
                            }
                        } else tmp.obj->destroy(&tmp);
                        get_mem(&current_section->mem, &newmemp, &newmembp);
                        newlabel->defpass = pass;
                    }
                } else {
                    val = val_alloc();
                    newlabel->constant = 1;
                    newlabel->requires=current_section->requires;
                    newlabel->conflicts=current_section->conflicts;
                    newlabel->value = val;
                    newlabel->file_list = cflist;
                    newlabel->epoint = epoint;
                    val->obj = CODE_OBJ;
                    val->u.code.addr = val_alloc();
                    int_from_uval(val->u.code.addr, current_section->l_address);
                    val->u.code.size = 0;
                    val->u.code.dtype = D_NONE;
                    val->u.code.pass = 0;
                    val->u.code.parent = newlabel;
                    get_mem(&current_section->mem, &newmemp, &newmembp);
                }
            }
            if (epoint.pos && !islabel) err_msg2(ERROR_LABEL_NOT_LEF, NULL, &epoint);
            epoint = lpoint;
            if (wht==WHAT_COMMAND) { /* .proc */
                switch (prm) {
                case CMD_PROC:
                    new_waitfor(W_PEND, &epoint);waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;waitfor->cheap_label = oldcheap;
                    if (!newlabel->ref && newlabel->value->u.code.pass) waitfor->skip=0;
                    else {
                        current_context=newlabel;
                        if (listing && flist && arguments.source) {
                            int l;
                            l = printaddr('.', current_section->address, LIST_CODE);
                            if (labelname.len) {
                                while (l < 40) { l += 8; putc('\t', flist); }
                                if (labelname.data[0] == '-' || labelname.data[0] == '+') putc(labelname.data[0], flist);
                                else fwrite(labelname.data, labelname.len, 1, flist);
                            }
                            putc('\n', flist);
                        }
                        newlabel->ref = 0;
                    }
                    newlabel = NULL;
                    goto finish;
                case CMD_DSTRUCT: /* .dstruct */
                case CMD_DUNION:
                    {
                        int old_unionmode = current_section->unionmode;
                        struct linepos_s epoint2;
                        address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                        address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                        if (listing && flist && arguments.source) {
                            int l;
                            l = printaddr('.', current_section->address, LIST_DATA);
                            printllist(l);
                        }
                        newlabel->ref = 0;
                        if (!get_exp(&w,1,cfile)) goto breakerr;
                        if (!(val = get_val(&epoint2))) {err_msg(ERROR_GENERL_SYNTAX, NULL); goto breakerr;}
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                            goto finish;
                        }
                        if (val->obj != ((prm==CMD_DSTRUCT) ? STRUCT_OBJ : UNION_OBJ)) {err_msg_wrong_type(val, &epoint2); goto breakerr;}
                        ignore();if (here() == ',') lpoint.pos++;
                        current_section->structrecursion++;
                        current_section->unionmode = (prm==CMD_DUNION);
                        current_section->unionstart = current_section->unionend = current_section->address;
                        current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                        val = macro_recurse((prm==CMD_DSTRUCT)?W_ENDS2:W_ENDU2, val, newlabel, &epoint);
                        if (val) {
                            if (newlabel) {
                                newlabel->update_after = 1;
                                var_assign(newlabel, val, 0);
                            }
                            val_destroy(val);
                        }
                        current_section->structrecursion--;
                        current_section->unionmode = old_unionmode;
                        current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                        current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                        goto finish;
                    }
                case CMD_SECTION:
                    waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    if (newlabel->ref && listing && flist && arguments.source) {
                        int l;
                        l = printaddr('.', current_section->address, LIST_CODE);
                        if (labelname.len) {
                            while (l < 40) { l += 8; putc('\t', flist); }
                            if (labelname.data[0] == '-' || labelname.data[0] == '+') putc(labelname.data[0], flist);
                            else fwrite(labelname.data, labelname.len, 1, flist);
                        }
                        putc('\n', flist);
                    }
                    newlabel->ref = 0;
                    newlabel = NULL;
                    goto finish;
                }
            }
            wasref = newlabel->ref;newlabel->ref = 0;
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
                if (!get_exp(&w,0,cfile)) goto breakerr;
                if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                eval_finish();
                if (listing && flist && arguments.source) {
                    int l;
                    if (wasref) l = printaddr('.', current_section->address, LIST_NONE);
                    else l = 0;
                    printllist(l);
                }
                if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, "*=");
                else if (val->obj == NONE_OBJ) {
                    static const str_t starname = {1, (const uint8_t*)"*"};
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(&starname, &epoint);
                    fixeddig = 0;
                } else {
                    struct value_s err;
                    uval_t uval;
                    if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) { err_msg_output_and_destroy(&err); uval = 0; }
                    if ((arguments.output_mode == OUTPUT_FLAT) && !current_section->logicalrecursion) {
                        if ((address_t)uval & ~all_mem2) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                        } else {
                            current_section->l_address = (address_t)uval & all_mem;
                            if (current_section->address != (address_t)uval) {
                                current_section->address = (address_t)uval;
                                memjmp(&current_section->mem, current_section->address);
                            }
                        }
                    } else {
                        if ((uval_t)uval & ~(uval_t)all_mem) {
                            err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                        } else {
                            address_t addr;
                            if (arguments.tasmcomp) addr = (uint16_t)uval;
                            else if ((address_t)uval > current_section->l_address) {
                                addr = (current_section->address + (((address_t)uval - current_section->l_address) & all_mem)) & all_mem2;
                            } else {
                                addr = (current_section->address - ((current_section->l_address - (address_t)uval) & all_mem)) & all_mem2;
                            }
                            if (current_section->address != addr) {
                                current_section->address = addr;
                                memjmp(&current_section->mem, current_section->address);
                            }
                            current_section->l_address = (uval_t)uval & all_mem;
                        }
                    }
                }
            }
            break;
        case WHAT_COMMENT:
        case WHAT_EOL:
            if (listing && flist && arguments.source && (waitfor->skip & 1) && wasref) {
                int l;
                l = printaddr('.', current_section->address, LIST_CODE);
                printllist(l);
            }
            break;
        case WHAT_COMMAND:
            {
                ignore();
                if (listing && flist && arguments.source && (waitfor->skip & 1) && prm>CMD_DWORD) {
                    int l;
                    switch (prm) {
                        case CMD_OFFS:
                        case CMD_ENDS:
                        case CMD_STRUCT:
                        case CMD_ENDU:
                        case CMD_UNION:
                            l = printaddr('.', current_section->address, LIST_DATA);
                            printllist(l);
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
                            if (wasref) l = printaddr('.', current_section->address, LIST_CODE);
                            else l = 0;
                            printllist(l);
                            break;
                        default:
                            if (wasref) {
                                l = printaddr('.',current_section->address, LIST_CODE);
                                if (labelname.len) {
                                    while (l < 40) { l += 8; putc('\t', flist); }
                                    if (labelname.data[0] == '-' || labelname.data[0] == '+') putc(labelname.data[0], flist);
                                    else fwrite(labelname.data, labelname.len, 1, flist);
                                }
                                putc('\n', flist);
                            }
                    }
                }
                if (prm==CMD_ENDC) { /* .endc */
                    if (!close_waitfor(W_ENDC)) err_msg2(ERROR______EXPECTED,".COMMENT", &epoint);
                    break;
                } else if (waitfor->what == W_ENDC) break;
                if (prm==CMD_FI) /* .fi */
                {
                    if (!close_waitfor(W_FI2) && !close_waitfor(W_FI)) err_msg2(ERROR______EXPECTED,".IF", &epoint);
                    break;
                }
                if (prm==CMD_ENDSWITCH) /* .endswitch */
                {
                    if (!close_waitfor(W_SWITCH2) && !close_waitfor(W_SWITCH)) err_msg2(ERROR______EXPECTED,".SWITCH", &epoint);
                    break;
                }
                if (prm==CMD_DEFAULT) { /* .default */
                    if (waitfor->what==W_SWITCH) {err_msg2(ERROR______EXPECTED,".ENDSWITCH", &epoint); break;}
                    if (waitfor->what!=W_SWITCH2) {err_msg2(ERROR______EXPECTED,".SWITCH", &epoint); break;}
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_SWITCH;waitfor->epoint = epoint;
                    break;
                }
                if (prm==CMD_ELSE) { /* .else */
                    if (waitfor->what==W_FI) {err_msg2(ERROR______EXPECTED,".FI", &epoint); break;}
                    if (waitfor->what!=W_FI2) {err_msg2(ERROR______EXPECTED,".IF", &epoint); break;}
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_FI;waitfor->epoint = epoint;
                    break;
                }
                if (prm==CMD_IF || prm==CMD_IFEQ || prm==CMD_IFNE || prm==CMD_IFPL || prm==CMD_IFMI || prm==CMD_ELSIF) { /* .if */
                    uint8_t skwait = waitfor->skip;
                    ival_t ival;
                    struct value_s err;
                    int truth;
                    if (prm==CMD_ELSIF) {
                        if (waitfor->what!=W_FI2) {err_msg2(ERROR______EXPECTED,".IF", &epoint); break;}
                        waitfor->epoint = epoint;
                    } else new_waitfor(W_FI2, &epoint);
                    if (((skwait==1) && prm!=CMD_ELSIF) || ((skwait==2) && prm==CMD_ELSIF)) {
                        if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                    } else val = &none_value;
                    switch (prm) {
                    case CMD_ELSIF:
                        if (val->obj == NONE_OBJ) truth = 0;
                        else if (val->obj->truth(val, &err, &truth, TRUTH_BOOL, &epoint)) {err_msg_output_and_destroy(&err); truth = 0; }
                        waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                        break;
                    case CMD_IF:
                        if (val->obj == NONE_OBJ) truth = 0;
                        else if (val->obj->truth(val, &err, &truth, TRUTH_BOOL, &epoint)) {err_msg_output_and_destroy(&err); truth = 0; }
                        waitfor->skip = truth ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                        break;
                    case CMD_IFNE:
                    case CMD_IFEQ:
                        if (val->obj == NONE_OBJ) truth = 0;
                        else {
                            int sign;
                            if (val->obj->sign(val, &err, &sign, &epoint)) { err_msg_output_and_destroy(&err); truth = 0; } 
                            else truth = (sign == 0) ^ (prm == CMD_IFNE);
                        }
                        waitfor->skip = truth ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        break;
                    case CMD_IFPL:
                    case CMD_IFMI:
                        if (val->obj == NONE_OBJ) truth = 0;
                        else if (arguments.tasmcomp) {
                            if (val->obj->ival(val, &err, &ival, 8*sizeof(uval_t), &epoint)) { err_msg_output_and_destroy(&err); truth = 0; } 
                            else truth = !(ival & 0x8000) ^ (prm == CMD_IFMI);
                        } else {
                            int sign;
                            if (val->obj->sign(val, &err, &sign, &epoint)) { err_msg_output_and_destroy(&err); truth = 0; }
                            else truth = (sign >= 0) ^ (prm == CMD_IFMI);
                        }
                        waitfor->skip = truth ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);break;
                        break;
                    }
                    break;
                }
                if (prm==CMD_SWITCH) { /* .switch */
                    uint8_t skwait = waitfor->skip;
                    new_waitfor(W_SWITCH2, &epoint);
                    if (skwait==1) {
                        if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        eval_finish();
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
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
                    if (waitfor->what == W_SWITCH) {err_msg2(ERROR______EXPECTED,".ENDSWITCH", &epoint); goto breakerr;}
                    if (waitfor->what != W_SWITCH2) {err_msg2(ERROR______EXPECTED,".SWITCH", &epoint); goto breakerr;}
                    waitfor->epoint = epoint;
                    if (skwait==2) {
                        struct value_s result;
                        struct oper_s tmp;
                        if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_vals_tuple())) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                        tmp.op = (val->obj == TUPLE_OBJ || val->obj == LIST_OBJ) ? &o_IN : &o_EQ;
                        tmp.v1 = waitfor->val;
                        tmp.v2 = val;
                        tmp.v = &result;
                        tmp.epoint = tmp.epoint2 = tmp.epoint3 = epoint;
                        val->obj->rcalc2(&tmp);
                        truth = result.obj == BOOL_OBJ && result.u.boolean;
                        val_destroy(val);
                    }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    break;
                }
                if (prm==CMD_ENDM) { /* .endm */
                    if (close_waitfor(W_ENDM)) {
                        lpoint.pos += strlen((const char *)pline + lpoint.pos);
                    } else if (close_waitfor(W_ENDM2)) {
                        nobreak=0;
                        if (here() && here() != ';' && get_exp(&w,0,cfile)) {
                            retval = get_vals_tuple();
                        } 
                    } else err_msg2(ERROR______EXPECTED,".MACRO or .SEGMENT", &epoint);
                    break;
                }
                if (prm==CMD_ENDF) { /* .endf */
                    if (close_waitfor(W_ENDF)) {
                        lpoint.pos += strlen((const char *)pline + lpoint.pos);
                    } else if (close_waitfor(W_ENDF2)) {
                        nobreak = 0;
                        if (here() && here() != ';' && get_exp(&w,0,cfile)) {
                            retval = get_vals_tuple();
                        } 
                    } else {err_msg2(ERROR______EXPECTED,".FUNCTION", &epoint);goto breakerr;}
                    break;
                }
                if (prm==CMD_NEXT) { /* .next */
                    if (close_waitfor(W_NEXT)) {
                    } else if (close_waitfor(W_NEXT2)) {
                        nobreak=0;
                    } else err_msg2(ERROR______EXPECTED,".FOR or .REPT", &epoint);
                    break;
                }
                if (prm==CMD_PEND) { /* .pend */
                    if (waitfor->what==W_PEND) {
                        if (waitfor->skip & 1) {
                            if (waitfor->cheap_label) cheap_context = waitfor->cheap_label;
                            if (current_context->parent) {
                                current_context = current_context->parent;
                            } else err_msg2(ERROR______EXPECTED,".proc", &epoint);
                            lastl=LIST_NONE;
                            if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                        }
                        close_waitfor(W_PEND);
                    } else err_msg2(ERROR______EXPECTED,".PROC", &epoint);
                    break;
                }
                if (prm==CMD_ENDS) { /* .ends */
                    if (close_waitfor(W_ENDS)) {
                        lpoint.pos += strlen((const char *)pline + lpoint.pos);
                    } else if (close_waitfor(W_ENDS2)) {
                        nobreak=0;
                        if (here() && here() != ';' && get_exp(&w,0,cfile)) {
                            retval = get_vals_tuple();
                        } 
                    } else err_msg2(ERROR______EXPECTED,".STRUCT", &epoint); break;
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
                                char *s = (char *)malloc(current_section->name.len + 1);
                                if (!s) err_msg_out_of_memory();
                                memcpy(s, current_section->name.data, current_section->name.len);
                                s[current_section->name.len] = '\0';
                                err_msg2(ERROR______EXPECTED, s, &epoint);
                                free(s);
                            }
                        }
                        if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                        current_section = waitfor->section;
                        close_waitfor(W_SEND2);
                    } else {err_msg2(ERROR______EXPECTED,".SECTION", &epoint);goto breakerr;}
                    break;
                }
                if (prm==CMD_ENDU) { /* .endu */
                    if (close_waitfor(W_ENDU)) {
                    } else if (close_waitfor(W_ENDU2)) {
                        nobreak=0; current_section->l_address = current_section->l_unionend;
                        if (current_section->address != current_section->unionend) {
                            current_section->address = current_section->unionend;
                            memjmp(&current_section->mem, current_section->address);
                        }
                    } else err_msg2(ERROR______EXPECTED,".UNION", &epoint); break;
                    break;
                }
                if (prm==CMD_ENDP) { /* .endp */
                    if (close_waitfor(W_ENDP)) {
                    } else if (waitfor->what==W_ENDP2) {
			if ((current_section->l_address ^ waitfor->laddr) > 0xff) {
                            err_msg2(ERROR____PAGE_ERROR, &current_section->l_address, &epoint);
			}
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                        close_waitfor(W_ENDP2);
                    } else err_msg2(ERROR______EXPECTED,".PAGE", &epoint); break;
                    break;
                }
                if (prm==CMD_HERE) { /* .here */
                    if (close_waitfor(W_HERE)) {
                        current_section->logicalrecursion--;
                    } else if (waitfor->what==W_HERE2) {
			current_section->l_address = current_section->address + waitfor->laddr;
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                        close_waitfor(W_HERE2);
                        current_section->logicalrecursion--;
                    } else err_msg2(ERROR______EXPECTED,".LOGICAL", &epoint); break;
                    break;
                }
                if (prm==CMD_BEND) { /* .bend */
                    if (close_waitfor(W_BEND)) {
                    } else if (waitfor->what==W_BEND2) {
			if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                        if (waitfor->cheap_label) cheap_context = waitfor->cheap_label;
			if (current_context->parent) current_context = current_context->parent;
			else err_msg2(ERROR______EXPECTED,".block", &epoint);
			close_waitfor(W_BEND2);
                    } else err_msg2(ERROR______EXPECTED,".BLOCK", &epoint); break;
                    break;
                }
                if (prm==CMD_END) {
                    nobreak=0;
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
                    if (what2 != W_NONE) new_waitfor(what2, &epoint);
                    break;/* skip things if needed */
                }
                if (prm<=CMD_DWORD || prm==CMD_BINARY) { /* .text .rta .char .int .word .long */
                    size_t uninit = 0;
                    size_t sum = 0;

                    mark_mem(&current_section->mem, current_section->address);
                    if (prm<CMD_BYTE) {    /* .text .ptext .shift .shift2 .null */
                        int16_t ch2=-1;
                        struct value_s err;
                        struct linepos_s large = {0,0,0};
                        int bits = 8 - (prm==CMD_SHIFT || prm==CMD_SHIFTL);
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (prm==CMD_PTEXT) ch2=0;
                        if (!get_exp(&w,0,cfile)) goto breakerr;
                        err.obj = NONE_OBJ;
                        while ((val = get_val(&epoint))) {
                            struct value_s iter, item, *val2;
                            uval_t uval;
                            item.refcount = 0;
                            if (val->obj == STR_OBJ) {
                                struct value_s *tmp = val_alloc();
                                bytes_from_str(tmp, val);
                                tmp->obj->getiter(tmp, &iter);
                                val_destroy(tmp);
                            } else val->obj->getiter(val, &iter);

                            while ((val2 = iter.obj->next(&iter, &item))) {
                                if (ch2 >= 0) {
                                    if (uninit) { memskip(uninit); sum += uninit; uninit = 0; }
                                    pokeb(ch2); sum++;
                                }
                                switch (val2->obj->type) {
                                case T_GAP:ch2 = -1; uninit++; val_destroy(val2); continue;
                                default:
                                    if (val2->obj->uval(val2, &err, &uval, bits, &epoint)) uval = 0;
                                    ch2 = (uint8_t)uval;
                                    break;
                                case T_NONE:
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                    ch2 = fixeddig = 0;
                                }
                                if (prm==CMD_SHIFTL) ch2<<=1;
                                else if (prm==CMD_NULL && !ch2 && val2->obj != NONE_OBJ) large=epoint;
                                val_destroy(val2);
                            }
                            iter.obj->destroy(&iter);
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

                            if (current_section->dooutput) write_mark_mem(&current_section->mem, sum-1);
                        }
                        if (large.pos) err_msg2(ERROR_CONSTNT_LARGE, NULL, &large);
                        if (err.obj != NONE_OBJ) {
                            err_msg_output_and_destroy(&err);
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                    } else if (prm<=CMD_DWORD) { /* .byte .word .int .rta .long */
                        int bits;
                        uint32_t ch2;
                        uval_t uv;
                        ival_t iv;
                        struct value_s err;
                        err.obj = NONE_OBJ;
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
                        switch (prm) {
                        case CMD_CHAR: bits = -8; break;
                        case CMD_INT: bits = -16; break;
                        case CMD_LINT: bits = -24; break;
                        case CMD_DINT: bits = -32; break;
                        case CMD_BYTE: bits = 8; break;
                        default: bits = 16; break;
                        case CMD_LONG: bits = 24; break;
                        case CMD_DWORD: bits = 32; break;
                        }
                        if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                        while ((val = get_val(&epoint))) {
                            struct value_s iter, item, *val2;
                            item.refcount = 0;
                            if (val->obj == LIST_OBJ || val->obj == TUPLE_OBJ) val->obj->getiter(val, &iter);
                            else invalid_getiter(val, &iter);
                            while ((val2 = iter.obj->next(&iter, &item))) {
                                switch (val2->obj->type) {
                                case T_GAP: uninit += abs(bits) / 8; val_destroy(val2); continue;
                                default:
                                    if (bits >= 0) {
                                        if (val2->obj->uval(val2, &err, &uv, bits, &epoint)) uv = 0;
                                        ch2 = uv;
                                    } else {
                                        if (val2->obj->ival(val2, &err, &iv, -bits, &epoint)) iv = 0;
                                        ch2 = iv;
                                    }
                                    break;
                                case T_NONE:
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                    ch2 = fixeddig = 0;
                                }
                                if (prm==CMD_RTA) ch2--;

                                if (uninit) {memskip(uninit);uninit = 0;}
                                pokeb((uint8_t)ch2);
                                if (prm>=CMD_RTA) pokeb((uint8_t)(ch2>>8));
                                if (prm>=CMD_LINT) pokeb((uint8_t)(ch2>>16));
                                if (prm>=CMD_DINT) pokeb((uint8_t)(ch2>>24));
                                val_destroy(val2);
                            }
                            iter.obj->destroy(&iter);
                        }
                        if (uninit) memskip(uninit);
                        if (err.obj != NONE_OBJ) {
                            err_msg_output_and_destroy(&err);
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                    } else if (prm==CMD_BINARY) { /* .binary */
                        const char *path = NULL;
                        size_t foffset = 0;
                        struct value_s *val2 = NULL;
                        address_t fsize = all_mem+1;
                        uval_t uval;
                        struct value_s err;
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                        if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        } else {
                            if (val->obj != STR_OBJ) {err_msg_wrong_type(val, &epoint);goto breakerr;}
                            path = get_path(val, cfile->realname);
                            val2 = val;
                        }
                        if ((val = get_val(&epoint))) {
                            if (val->obj == NONE_OBJ) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = 0;
                            } else {
                                if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err);
                                else foffset = uval;
                            }
                            if ((val = get_val(&epoint))) {
                                if (val->obj == NONE_OBJ) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                    fixeddig = 0;
                                } else {
                                    if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err);
                                    else if ((address_t)uval > fsize) err_msg2(ERROR_CONSTNT_LARGE,NULL, &epoint);
                                    else fsize = uval;
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
                        free((char *)path);
                    }

                    if (listing && flist) {
                        list_mem(&current_section->mem, flist, all_mem, current_section->dooutput);
                    }
                    break;
                }
                if (prm==CMD_OFFS) {   /* .offs */
                    struct linepos_s opoint = epoint;
                    ival_t ival;
                    struct value_s err;
                    if (!current_section->moved) {
                        if (current_section->end < current_section->address) current_section->end = current_section->address;
                        current_section->moved = 1;
                    }
                    current_section->wrapwarn = current_section->wrapwarn2 = 0;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        if (val->obj->ival(val, &err, &ival, 8*sizeof(ival_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else if (ival) {
                            if (current_section->structrecursion) {
                                if (ival < 0) err_msg2(ERROR___NOT_ALLOWED, ".OFFS", &opoint);
                                else {
                                    current_section->l_address += ival;
                                    current_section->l_address &= all_mem;
                                    current_section->address += ival;
                                }
                            } else current_section->address += ival;
                            current_section->address &= all_mem2;
                            memjmp(&current_section->mem, current_section->address);
                        }
                    }
                    break;
                }
                if (prm==CMD_LOGICAL) { /* .logical */
                    struct linepos_s opoint = epoint;
                    uval_t uval;
                    struct value_s err;
                    new_waitfor(W_HERE2, &epoint);waitfor->laddr = current_section->l_address - current_section->address;waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    current_section->logicalrecursion++;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".LOGICAL", &opoint);
                    else if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 24, &epoint)) err_msg_output_and_destroy(&err); 
                        else if (uval & ~(uval_t)all_mem) err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                        else current_section->l_address = (address_t)uval;
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
		    new_waitfor(W_BEND2, &epoint);
                    if (newlabel) {
                        current_context=newlabel;
                        waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;waitfor->cheap_label = oldcheap;
                        if (newlabel->ref && listing && flist && arguments.source) {
                            int l;
                            l = printaddr('.', current_section->address, LIST_CODE);
                            if (labelname.len) {
                                while (l < 40) { l += 8; putc('\t', flist); }
                                if (labelname.data[0] == '-' || labelname.data[0] == '+') putc(labelname.data[0], flist);
                                else fwrite(labelname.data, labelname.len, 1, flist);
                            }
                            putc('\n', flist);
                        }
                        newlabel = NULL;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        sprintf(reflabel, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                        tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                        current_context=new_label(&tmpname, mycontext, strength, &labelexists);
                        waitfor->cheap_label = cheap_context;
                        cheap_context = current_context;
                        if (!labelexists) {
                            current_context->constant = 1;
                            current_context->requires = 0;
                            current_context->conflicts = 0;
                            current_context->value = &none_value;
                            current_context->file_list = cflist;
                            current_context->epoint = epoint;
                        }
                    }
                    break;
                }
                if (prm==CMD_DATABANK || prm==CMD_DPAGE || prm==CMD_STRENGTH || prm==CMD_EOR) { /* .databank, .dpage, .strength, .eor */
                    struct value_s err;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        uval_t uval;
                        ival_t ival;
                        switch (prm) {
                        case CMD_DATABANK:
                            if (val->obj->uval(val, &err, &uval, 8, &epoint)) err_msg_output_and_destroy(&err); 
                            else databank = uval;
                            break;
                        case CMD_DPAGE:
                            if (val->obj->uval(val, &err, &uval, 16, &epoint)) err_msg_output_and_destroy(&err); 
                            else dpage = uval;
                            break;
                        case CMD_STRENGTH:
                            if (val->obj->ival(val, &err, &ival, 8, &epoint)) err_msg_output_and_destroy(&err); 
                            else strength = ival;
                            break;
                        case CMD_EOR:
                            if (val->obj->uval(val, &err, &uval, 8, &epoint)) err_msg_output_and_destroy(&err);
                            else outputeor = uval;
                            break;
                        default:
                            break;
                        }
                    }
                    break;
                }
                if (prm==CMD_FILL || prm==CMD_ALIGN) { /* .fill, .align */
                    address_t db = 0;
                    uval_t uval;
                    struct value_s err;
                    if (newlabel) {
                        newlabel->value->u.code.dtype = D_BYTE;
                    }
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (prm == CMD_ALIGN && current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".ALIGN");
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) {
                            err_msg_output_and_destroy(&err); 
                            uval = (prm == CMD_ALIGN) ? 1 : 0;
                        }
                        if (prm == CMD_ALIGN) {
                            if (!uval || (uval & ~(uval_t)all_mem)) err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                            else if (uval > 1 && current_section->l_address % uval) db = uval - (current_section->l_address % uval);
                        } else db = uval;
                        if (db && db - 1 > all_mem2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);goto breakerr;}
                    }
                    mark_mem(&current_section->mem, current_section->address);
                    if ((val = get_val(&epoint))) {
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                            memskip(db);
                        } else {
                            struct value_s iter, item, *val2;
                            size_t uninit = 0, sum = 0;
                            size_t memp, membp;
                            item.refcount = 0;
                            err.obj = NONE_OBJ;
                            get_mem(&current_section->mem, &memp, &membp);

                            if (val->obj == STR_OBJ) {
                                struct value_s *tmp = val_alloc();
                                bytes_from_str(tmp, val);
                                tmp->obj->getiter(tmp, &iter);
                                val_destroy(tmp);
                            } else val->obj->getiter(val, &iter);

                            while (db && ((val2 = iter.obj->next(&iter, &item)))) {
                                db--;
                                switch (val2->obj->type) {
                                case T_GAP:uninit++; break;
                                default:
                                    if (val2->obj->uval(val2, &err, &uval, 8, &epoint)) uval = 0;
                                    if (uninit) { memskip(uninit); sum += uninit; uninit = 0; }
                                    pokeb(uval); sum++;
                                    break;
                                case T_NONE:
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                    fixeddig = 0; uninit++;
                                }
                                val_destroy(val2);
                            }
                            iter.obj->destroy(&iter);
                            sum += uninit;
                            if (db) {
                                if (sum == 1 && uninit == 0) {
                                    while (db--) pokeb(uval); /* single byte shortcut */
                                } else if (sum == uninit) {
                                    uninit += db; /* gap shortcut */
                                } else {
                                    size_t offs = 0;
                                    while (db) { /* pattern repeat */
                                        int16_t ch;
                                        db--;
                                        ch = read_mem(&current_section->mem, memp, membp, offs);
                                        if (ch < 0) uninit++;
                                        else {
                                            if (uninit) {memskip(uninit); uninit = 0;}
                                            pokeb(ch);
                                        }
                                        offs++;
                                        if (offs >= sum) offs = 0;
                                    }
                                }
                            }
                            if (uninit) memskip(uninit);
                            if (err.obj != NONE_OBJ) {
                                err_msg_output_and_destroy(&err);
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = 0;
                            }
                        }
                    } else memskip(db);
                    if (listing && flist) {
                        list_mem(&current_section->mem, flist, all_mem, current_section->dooutput);
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_ASSERT) { /* .assert */
                    uval_t uval;
                    struct value_s err;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;current_section->provides=~(uval_t)0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else current_section->provides = uval;
                    }
                    if (!(val = get_val(&epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = current_section->requires = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else current_section->requires = uval;
                    }
                    if (!(val = get_val(&epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = current_section->conflicts = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else current_section->conflicts = uval;
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_CHECK) { /* .check */
                    struct linepos_s opoint = epoint;
                    uval_t uval;
                    struct value_s err;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else if ((uval & current_section->provides) ^ uval) err_msg_requires(NULL, &opoint);
                    }
                    if (!(val = get_val(&epoint))) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        if (val->obj->uval(val, &err, &uval, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err); 
                        else if (uval & current_section->provides) err_msg_conflicts(NULL, &opoint);
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_WARN || prm==CMD_CWARN || prm==CMD_ERROR || prm==CMD_CERROR) { /* .warn .cwarn .error .cerror */
                    int rc;
                    int first = 1;
                    int writeit = 1;
                    struct error_s user_error;
                    struct linepos_s epoint2;
                    error_init(&user_error);
                    rc = get_exp(&w,0,cfile);
                    if (!rc) goto breakerr; /* ellenorizve. */
                    err_msg_variable(&user_error, NULL);
                    for (;;) {
                        val = get_val(&epoint2);
                        if (!val) break;
                        if (first) {
                            first = 0;
                            if (prm == CMD_CWARN || prm == CMD_CERROR) {
                                struct value_s err;
                                if (val->obj == NONE_OBJ) writeit = 0;
                                else if (val->obj->truth(val, &err, &writeit, TRUTH_BOOL, &epoint2)) {err_msg_output_and_destroy(&err); writeit = 0; }
                                continue;
                            }
                            writeit = 1;
                        }
                        if (writeit) {
                            if (val->obj != NONE_OBJ) err_msg_variable(&user_error, val);
                        }
                    }
                    if (writeit) err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED, &user_error, &epoint);
                    errors_destroy(&user_error);
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
                    rc = get_exp(&w,0,cfile);
                    actual_encoding = old;
                    if (!rc) goto breakerr; /* ellenorizve. */
                    for (;;) {
                        int endok = 0;
                        size_t i = 0;
                        int tryit = 1;
                        uval_t uval;
                        struct value_s err;

                        actual_encoding = NULL;
                        val = get_val(&epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->obj->type) {
                        case T_FLOAT:
                        case T_BOOL:
                        case T_INT:
                        case T_BITS:
                             if (val->obj->uval(val, &err, &uval, 24, &epoint)) {
                                 err_msg_output_and_destroy(&err);
                                 uval = 0;
                             }
                             tmp.start = uval;
                             break;
                        case T_STR:
                             if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
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
                             if (val->u.str.len > i) err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                             break;
                        case T_NONE: 
                             if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                             tryit = fixeddig = 0;
                             break;
                        default:
                            err_msg_wrong_type(val, &epoint);
                            goto breakerr;
                        }
                        if (!endok) {
                            actual_encoding = NULL;
                            val = get_val(&epoint);
                            actual_encoding = old;
                            if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                            if (val->obj == NONE_OBJ) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                tryit = fixeddig = 0;
                            } else {
                                if (val->obj->uval(val, &err, &uval, 24, &epoint)) {
                                    err_msg_output_and_destroy(&err);
                                    uval = 0;
                                }
                                if (tmp.start > (uint32_t)uval) {
                                    tmp.end = tmp.start;
                                    tmp.start = uval;
                                } else tmp.end = uval;
                            }
                        }
                        actual_encoding = NULL;
                        val = get_val(&epoint);
                        actual_encoding = old;
                        if (!val) {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        } else if (tryit) {
                            if (val->obj->uval(val, &err, &uval, 8, &epoint)) {
                                err_msg_output_and_destroy(&err);
                                uval = 0;
                            }
                            tmp.offset = uval;
                            t = new_trans(&tmp, actual_encoding);
                            if (t->start != tmp.start || t->end != tmp.end || t->offset != tmp.offset) {
                                err_msg2(ERROR__DOUBLE_RANGE, NULL, &epoint); goto breakerr;
                            }
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_EDEF) { /* .edef */
                    struct encoding_s *old = actual_encoding;
                    int rc;
                    actual_encoding = NULL;
                    rc = get_exp(&w,0,cfile);
                    actual_encoding = old;
                    if (!rc) goto breakerr; /* ellenorizve. */
                    for (;;) {
                        struct linepos_s opoint;
                        struct value_s *v;
                        int tryit = 1;

                        actual_encoding = NULL;
                        val = get_val(&epoint);
                        actual_encoding = old;
                        if (!val) break;

                        switch (val->obj->type) {
                        case T_STR:
                             if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, &epoint);
                             break;
                        case T_NONE: 
                             if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                             tryit = fixeddig = 0;
                             break;
                        default:
                            err_msg_wrong_type(val, &epoint);
                            goto breakerr;
                        }
                        v = val_reference(val);
                        opoint = epoint;
                        val = get_val(&epoint);
                        if (!val) {err_msg(ERROR______EXPECTED,","); val_destroy(v); goto breakerr;}
                        if (val->obj == NONE_OBJ) {
                             if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                             fixeddig = 0;
                             val_destroy(v);
                        } else if (tryit) {
                            if (new_escape(v, val, actual_encoding, &epoint)) {
                                val_destroy(v);
                                err_msg2(ERROR_DOUBLE_ESCAPE, NULL, &opoint); goto breakerr;
                            }
                            val_destroy(v);
                        }
                    }
                    eval_finish();
                    break;
                }
                if (prm==CMD_CPU) { /* .cpu */
                    int def;
                    char cpu[20];
                    if (get_cpu(cpu)) goto breakerr;
                    def=arguments.cpumode;
                    if (!strcmp(cpu,"6502")) def=OPCODES_C6502;
                    else if (!strcasecmp(cpu,"65c02")) def=OPCODES_C65C02;
                    else if (!strcasecmp(cpu,"65ce02")) def=OPCODES_C65CE02;
                    else if (!strcasecmp(cpu,"6502i")) def=OPCODES_C6502I;
                    else if (!strcmp(cpu,"65816")) def=OPCODES_C65816;
                    else if (!strcasecmp(cpu,"65dtv02")) def=OPCODES_C65DTV02;
                    else if (!strcasecmp(cpu,"65el02")) def=OPCODES_C65EL02;
                    else if (!strcmp(cpu,"r65c02")) def=OPCODES_CR65C02;
                    else if (!strcmp(cpu,"w65c02")) def=OPCODES_CW65C02;
                    else if (strcasecmp(cpu,"default")) err_msg(ERROR___UNKNOWN_CPU,cpu);
                    set_cpumode(def);
                    break;
                }
                if (prm==CMD_REPT) { /* .rept */
                    new_waitfor(W_NEXT, &epoint);waitfor->skip=0;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (eval_finish()) {err_msg(ERROR_EXTRA_CHAR_OL,NULL);goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else {
                        uval_t cnt;
                        struct value_s err;
                        if (val->obj->uval(val, &err, &cnt, 8*sizeof(uval_t), &epoint)) err_msg_output_and_destroy(&err);
                        else if (cnt > 0) {
                            size_t pos = cfile->p;
                            line_t lin = lpoint.line;
                            int labelexists;
                            struct star_s *s = new_star(vline, &labelexists);
                            struct avltree *stree_old = star_tree;
                            line_t ovline = vline;

                            close_waitfor(W_NEXT);
                            if (labelexists && s->addr != star) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig=0;
                            }
                            s->addr = star;
                            star_tree = &s->tree;vline=0;
                            while (cnt--) {
                                lpoint.line=lin;cfile->p=pos;
                                new_waitfor(W_NEXT2, &epoint);waitfor->skip=1;
                                compile(cflist);
                            }
                            star_tree = stree_old; vline = ovline;
                        }
                    }
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
                    new_waitfor(W_ENDC, &epoint);waitfor->skip=0;
                    break;
                }
                if (prm==CMD_INCLUDE || prm == CMD_BINCLUDE) { /* .include, .binclude */
                    struct file_s *f;
                    struct linepos_s epoint2;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint2))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint2);
                        fixeddig = 0;
                    } else {
                        const char *path;
                        if (val->obj != STR_OBJ) {err_msg_wrong_type(val, &epoint2);goto breakerr;}
                        path = get_path(val, cfile->realname);

                        f = openfile(path, cfile->realname, 0, val);
                        free((char *)path);
                        if (f->open>1) {
                            err_msg(ERROR_FILERECURSION,NULL);
                        } else {
                            int starexists;
                            struct star_s *s = new_star(vline, &starexists);
                            struct avltree *stree_old = star_tree;
                            uint32_t old_backr = backr, old_forwr = forwr;
                            line_t lin = lpoint.line;
                            line_t vlin = vline;
                            struct file_list_s *cflist2;

                            if (listing && flist) {
                                fprintf(flist,"\n;******  Processing file \"%s\"\n",f->realname);
                                lastl=LIST_NONE;
                            }

                            if (starexists && s->addr != star) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig=0;
                            }
                            s->addr = star;
                            cflist2 = enterfile(f, &epoint);
                            lpoint.line = vline = 0; f->p=0;
                            star_tree = &s->tree;
                            backr = forwr = 0;
                            reffile=f->uid;
                            if (prm == CMD_BINCLUDE) {
                                if (newlabel) {
                                    current_context = newlabel;
                                } else {
                                    int labelexists;
                                    str_t tmpname;
                                    sprintf(reflabel, ".%" PRIxPTR ".%" PRIxline, (uintptr_t)star_tree, vline);
                                    tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                                    current_context=new_label(&tmpname, mycontext, strength, &labelexists);
                                    if (!labelexists) {
                                        current_context->constant = 1;
                                        current_context->requires = 0;
                                        current_context->conflicts = 0;
                                        current_context->value = &none_value;
                                        current_context->file_list = cflist;
                                        current_context->epoint = epoint;
                                    }
                                    oldcheap = cheap_context;
                                }
                                cheap_context = current_context;
                                compile(cflist2);
                                current_context = current_context->parent;
                                cheap_context = oldcheap;
                            } else compile(cflist2);
                            lpoint.line = lin; vline = vlin;
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
                    struct linepos_s apoint, bpoint = {0,0,0};
                    int nopos = -1;
                    uint8_t *expr;
                    struct label_s *var;
                    struct star_s *s;
                    struct avltree *stree_old;
                    struct value_s err;
                    line_t ovline;
                    int starexists, truth;

                    new_waitfor(W_NEXT, &epoint);waitfor->skip=0;
                    { /* label */
                        int labelexists;
                        str_t varname;
                        epoint = lpoint;
                        varname.data = pline + lpoint.pos; varname.len = get_label();
                        if (varname.len) {
                            if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint); goto breakerr;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                            lpoint.pos++;
                            if (!get_exp(&w,1,cfile)) goto breakerr; /* ellenorizve. */
                            if (!(val = get_vals_tuple())) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                            var=new_label(&varname, (varname.data[0] == '_') ? cheap_context : current_context, strength, &labelexists);
                            if (labelexists) {
                                if (var->constant) err_msg_double_defined(var, &varname, &epoint);
                                else {
                                    if (var->defpass != pass) var->ref = 0;
                                    var->requires=current_section->requires;
                                    var->conflicts=current_section->conflicts;
                                    var_assign(var, val, fixeddig);
                                }
                                val_destroy(val);
                            } else {
                                var->constant = 0;
                                var->requires = current_section->requires;
                                var->conflicts = current_section->conflicts;
                                var->value = val;
                                var->file_list = cflist;
                                var->epoint = epoint;
                            }
                            ignore();
                        }
                    }
                    if (here() != ',') {err_msg(ERROR______EXPECTED,","); goto breakerr;}
                    lpoint.pos++;

                    s = new_star(vline, &starexists); stree_old = star_tree; ovline = vline;
                    if (starexists && s->addr != star) {
                        struct linepos_s nopoint = {0,0,0};
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &nopoint);
                        fixeddig=0;
                    }
                    s->addr = star;
                    star_tree = &s->tree;vline=0;
                    xlin=lin=lpoint.line; xpos=pos=cfile->p; apoint = lpoint;
                    expr = (uint8_t *)malloc(strlen((char *)pline) + 1);
                    if (!expr) err_msg_out_of_memory();
                    strcpy((char *)expr, (char *)pline); var = NULL;
                    for (;;) {
                        lpoint=apoint;
                        if (!get_exp(&w,1,cfile)) break; /* ellenorizve. */
                        if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                        if (val->obj == NONE_OBJ) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                            break;
                        }
                        if (val->obj->truth(val, &err, &truth, TRUTH_BOOL, &epoint)) {err_msg_output_and_destroy(&err); break; }
                        if (!truth) break;
                        if (nopos < 0) {
                            str_t varname;
                            ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                            lpoint.pos++;ignore();
                            epoint = lpoint;
                            varname.data = pline + lpoint.pos; varname.len = get_label();
                            if (!varname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL);break;}
                            if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint); goto breakerr;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                            lpoint.pos++;ignore();
                            if (!here() || here()==';') {bpoint.pos = bpoint.upos = 0; nopos = 0;}
                            else {
                                int labelexists;
                                var=new_label(&varname, (varname.data[0] == '_') ? cheap_context : current_context, strength, &labelexists);
                                if (labelexists) {
                                    if (var->constant) {
                                        err_msg_double_defined(var, &varname, &epoint);
                                        break;
                                    }
                                    if (var->defpass != pass) var->ref = 0;
                                    var->requires = current_section->requires;
                                    var->conflicts = current_section->conflicts;
                                } else {
                                    var->constant = 0;
                                    var->requires = current_section->requires;
                                    var->conflicts = current_section->conflicts;
                                    var->value = &none_value;
                                    var->file_list = cflist;
                                    var->epoint = epoint;
                                }
                                bpoint=lpoint; nopos = 1;
                            }
                        }
                        new_waitfor(W_NEXT2, &epoint);waitfor->skip=1;
                        compile(cflist);
                        xpos = cfile->p; xlin= lpoint.line;
                        pline = expr;
                        lpoint.line=lin;cfile->p=pos;
                        if (nopos > 0) {
                            lpoint = bpoint;
                            if (!get_exp(&w,1,cfile)) break; /* ellenorizve. */
                            if (!(val = get_vals_tuple())) {err_msg(ERROR_GENERL_SYNTAX,NULL); break;}
                            var_assign(var, val, fixeddig);
                            val_destroy(val);
                            ignore();if (here() && here()!=';') {err_msg(ERROR_EXTRA_CHAR_OL,NULL);break;}
                        }
                    }
                    free(expr);
                    if (pos!=xpos || lin!=xlin) close_waitfor(W_NEXT);
                    lpoint.line=xlin;cfile->p=xpos;
                    star_tree = stree_old; vline = ovline;
                    goto breakerr;
                }
                if (prm==CMD_PAGE) { /* .page */
                    new_waitfor(W_ENDP2, &epoint);waitfor->addr = current_section->address;waitfor->laddr = current_section->l_address;waitfor->label=newlabel;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    newlabel=NULL;
                    break;
                }
                if (prm==CMD_OPTION) { /* .option */
                    static const str_t branch_across = {24, (const uint8_t *)"allow_branch_across_page"};
                    static const str_t longjmp = {22, (const uint8_t *)"auto_longbranch_as_jmp"};
                    int truth;
                    struct value_s err;
                    str_t optname;
                    optname.data = pline + lpoint.pos; optname.len = get_label();
                    if (!optname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); goto breakerr;}
                    lpoint.pos++;
                    if (!get_exp(&w,0,cfile)) goto breakerr; /* ellenorizve. */
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                    } else if ((!arguments.casesensitive && !str_casecmp(&optname, &branch_across)) || (arguments.casesensitive && !str_cmp(&optname, &branch_across))) {
                        if (val->obj->truth(val, &err, &truth, TRUTH_BOOL, &epoint)) {err_msg_output_and_destroy(&err); break; }
                        else allowslowbranch = truth;
                    }
                    else if ((!arguments.casesensitive && !str_casecmp(&optname, &longjmp)) || (arguments.casesensitive && !str_cmp(&optname, &longjmp))) {
                        if (val->obj->truth(val, &err, &truth, TRUTH_BOOL, &epoint)) {err_msg_output_and_destroy(&err); break; }
                        else longbranchasjmp = truth;
                    }
                    else {
                        char *s = (char *)malloc(optname.len + 1);
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
                    if (!get_exp(&w,0,cfile)) goto breakerr;
                    if (!(val = get_val(&epoint))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    eval_finish();
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                        goto breakerr;
                    }
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (val->obj != LBL_OBJ) {err_msg_wrong_type(val, &epoint); goto breakerr;}
                    if (val->u.lbl.file_list == cflist && val->u.lbl.parent == current_context) {
                        while (val->u.lbl.waitforp < waitfor_p) {
                            const char *msg = NULL;
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
                                err_msg2(ERROR______EXPECTED, msg, &waitfor->epoint);
                                noerr = 0;
                            }
                            close_waitfor(waitfor->what);
                        }
                        if (noerr) {
                            lpoint.line = val->u.lbl.sline;
                            cfile->p = val->u.lbl.p;
                        }
                    } else err_msg_not_defined(NULL, &epoint);
                    break;
                }
                if (prm==CMD_MACRO || prm==CMD_SEGMENT) {
                    new_waitfor(W_ENDM, &epoint);waitfor->skip=0;
                    err_msg_not_defined(NULL, &epoint);
                    break;
                }
                if (prm==CMD_FUNCTION) {
                    new_waitfor(W_ENDF, &epoint);waitfor->skip=0;
                    err_msg_not_defined(NULL, &epoint);
                    break;
                }
                if (prm==CMD_LBL) {
                    err_msg_not_defined(NULL, &epoint);
                    break;
                }
                if (prm==CMD_PROC) {
                    new_waitfor(W_PEND, &epoint);waitfor->skip=0;waitfor->label = NULL;waitfor->cheap_label = NULL;
                    err_msg_not_defined(NULL, &epoint);
                    break;
                }
                if (prm==CMD_STRUCT) {
                    int old_unionmode = current_section->unionmode;
                    current_section->unionmode = 0;
                    new_waitfor(W_ENDS, &epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDS2;waitfor->skip=1;
                        compile(cflist);
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
                    new_waitfor(W_ENDU, &epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    ignore();if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDU2;waitfor->skip=1;
                        compile(cflist);
                    } else err_msg(ERROR__MACRECURSION,"!!!!");
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                    break;
                }
                if (prm==CMD_DSTRUCT) {
                    int old_unionmode = current_section->unionmode;
                    struct linepos_s epoint2;
                    current_section->unionmode = 0;
                    if (!get_exp(&w,1,cfile)) goto breakerr;
                    if (!(val = get_val(&epoint2))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                        goto breakerr;
                    }
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->obj != STRUCT_OBJ) {err_msg_wrong_type(val, &epoint2); goto breakerr;}
                    current_section->structrecursion++;
                    val = macro_recurse(W_ENDS2, val, current_context, &epoint);
                    if (val) val_destroy(val);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    break;
                }
                if (prm==CMD_DUNION) {
                    int old_unionmode = current_section->unionmode;
                    struct linepos_s epoint2;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    if (!get_exp(&w,1,cfile)) goto breakerr;
                    if (!(val = get_val(&epoint2))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    if (val->obj == NONE_OBJ) {
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                        goto breakerr;
                    }
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->obj != UNION_OBJ) {err_msg_wrong_type(val, &epoint2); goto breakerr;}
                    current_section->structrecursion++;
                    val = macro_recurse(W_ENDU2, val, current_context, &epoint);
                    if (val) val_destroy(val);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    break;
                }
                if (prm==CMD_DSECTION) {
                    struct section_s *tmp3;
                    str_t sectionname;
                    if (current_section->structrecursion && !current_section->dooutput) err_msg(ERROR___NOT_ALLOWED, ".DSECTION");
                    ignore();epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp3=new_section(&sectionname);
                    if (tmp3->defpass == pass) {
                        struct label_s tmp;
                        tmp.name = tmp3->name;
                        tmp.file_list = tmp3->file_list;
                        tmp.epoint = tmp3->epoint;
                        err_msg_double_defined(&tmp, &sectionname, &epoint);
                    } else {
                        address_t t;
                        if (!tmp3->usepass || tmp3->defpass < pass - 1) {
                            if (tmp3->usepass == pass) {
                                tmp3->address += current_section->address;
                                tmp3->start += current_section->address;
                                tmp3->end += current_section->address;
                                tmp3->l_address += current_section->l_address;
                                tmp3->l_start += current_section->l_address;
                                memjmp(&tmp3->mem, tmp3->address);
                            } else {
                                tmp3->wrapwarn = tmp3->wrapwarn2 = tmp3->moved = 0;
                                tmp3->end = tmp3->start = tmp3->address = current_section->address;
                                tmp3->l_start = tmp3->l_address = current_section->l_address;
                                tmp3->usepass = pass;
                                restart_memblocks(&tmp3->mem, tmp3->address);
                            }  
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
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
                        tmp3->file_list = cflist;
                        tmp3->epoint = epoint;
                        if (tmp3->usepass == pass) {
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
                            restart_memblocks(&tmp3->mem, tmp3->address);
                        }
                        tmp3->size = t;
                        tmp3->usepass = pass;
                        tmp3->defpass = pass;
                        memskip(t);
                        memref(&current_section->mem, &tmp3->mem);
                    }
                    break;
                }
                if (prm==CMD_SECTION) {
                    struct section_s *tmp;
                    str_t sectionname;
                    new_waitfor(W_SEND, &epoint);waitfor->section=current_section;
                    ignore();epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                    tmp=find_new_section(&sectionname);
                    if (!tmp->usepass || tmp->defpass < pass - 1) {
                        if (tmp->usepass && tmp->usepass >= pass - 1) {err_msg_not_defined(&sectionname, &epoint); goto breakerr;}
                        tmp->end = tmp->start = tmp->address = 0;
                        tmp->l_start = tmp->l_address = 0;
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                        fixeddig = 0;
                        tmp->defpass = pass - 1;
                        restart_memblocks(&tmp->mem, tmp->address);
                    } else if (tmp->usepass != pass) {
                        if (!tmp->moved) {
                            if (tmp->end < tmp->address) tmp->end = tmp->address;
                            tmp->moved=1;
                        }
                        tmp->wrapwarn = tmp->wrapwarn2 = 0;
                        tmp->address = tmp->start;
                        tmp->l_address = tmp->l_start;
                        restart_memblocks(&tmp->mem, tmp->address);
                    }
                    tmp->usepass = pass;
                    waitfor->what = W_SEND2;
                    current_section = tmp;
                    newlabel = NULL;
                    break;
                }
                if (prm != sizeof(command)/sizeof(command[0])) {
                    err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;
                }
                /* fall through */
            }
        case WHAT_HASHMARK:if (waitfor->skip & 1) /* skip things if needed */
            {                   /* macro stuff */
                struct linepos_s epoint2;
                if (!get_exp_var(cfile)) goto breakerr;
                if (!(val = get_val(&epoint2))) {err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr;}
                if (val->obj == NONE_OBJ) {
                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint2);
                    fixeddig = 0;
                    goto breakerr;
                }
                if (val->obj != MACRO_OBJ && val->obj != SEGMENT_OBJ && val->obj != MFUNC_OBJ) {err_msg_wrong_type(val, &epoint2); goto breakerr;}
            as_macro:
                if (listing && flist && arguments.source && wasref) {
                    int l;
                    l = printaddr('.', current_section->address, LIST_CODE);
                    if (labelname.len) {
                        while (l < 40) { l += 8; putc('\t', flist); }
                        if (labelname.data[0] == '-' || labelname.data[0] == '+') putc(labelname.data[0], flist);
                        else fwrite(labelname.data, labelname.len, 1, flist);
                    }
                    putc('\n', flist);
                }
                if (val->obj == MACRO_OBJ) {
                    struct label_s *context;
                    if (newlabel) {
                        context=newlabel;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        sprintf(reflabel, "#%" PRIxPTR "#%" PRIxline, (uintptr_t)star_tree, vline);
                        tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                        context=new_label(&tmpname, mycontext, strength, &labelexists);
                        if (!labelexists) {
                            context->constant = 1;
                            context->requires = 0;
                            context->conflicts = 0;
                            context->value = &none_value;
                            context->file_list = cflist;
                            context->epoint = epoint;
                        }
                        oldcheap = cheap_context;
                    }
                    cheap_context = context;
                    val = macro_recurse(W_ENDM2, val, context, &epoint);
                    cheap_context = oldcheap;
                } else if (val->obj == MFUNC_OBJ) {
                    struct label_s *context;
                    struct value_s *mfunc;
                    int labelexists;
                    str_t tmpname;
                    sprintf(reflabel, "#%" PRIxPTR "#%" PRIxline, (uintptr_t)star_tree, vline);
                    tmpname.data = (const uint8_t *)reflabel; tmpname.len = strlen(reflabel);
                    context=new_label(&tmpname, val->u.mfunc.label->parent, strength, &labelexists);
                    if (!labelexists) {
                        context->constant = 1;
                        context->requires = 0;
                        context->conflicts = 0;
                        context->value = &none_value;
                        context->file_list = cflist;
                        context->epoint = epoint;
                    }
                    mfunc = val_reference(val);
                    val = mfunc_recurse(W_ENDF2, mfunc, context, &epoint, cfile, strength);
                    val_destroy(mfunc);
                } else val = macro_recurse(W_ENDM2, val, current_context, &epoint);
                if (val) {
                    if (newlabel) {
                        cheap_context = oldcheap;
                        newlabel->update_after = 1;
                        var_assign(newlabel, val, 0);
                    }
                    val_destroy(val);
                }
                break;
            }
        case WHAT_EXPRESSION:
            if (waitfor->skip & 1) {
                str_t opname;

                opname.data = pline + lpoint.pos; opname.len = get_label();
                if (opname.len == 3 && (prm=lookup_opcode((const char *)opname.data))>=0) {
                    int ret;
                    struct linepos_s oldlpoint;
                    struct linepos_s epoints[3];
                    if (0) {
                as_opcode: 
                        opname = labelname;
                    }
                    ignore();
                    oldlpoint = lpoint;
                    if (!here() || here() == ';') val = &null_addrlist;
                    else {
                        if (!get_exp(&w, 3, cfile)) goto breakerr;
                        val = get_vals_addrlist(epoints);
                    }
                    if (val->obj == TUPLE_OBJ || val->obj == LIST_OBJ) {
                        size_t i;
                        epoints[1] = epoints[0];
                        epoints[2] = epoints[0];
                        for (i = 0; i < val->u.list.len; i++) {
                            ret = instruction(prm, w, all_mem, val->u.list.data[i], &epoint, epoints);
                            if (ret == 0) continue;
                            if (ret == 2) {
                                err_msg2(ERROR_NO_ADDRESSING, NULL, &epoint);
                            }
                            val_destroy(val);
                            goto breakerr; /* ellenorizve. */
                        }
                        val_destroy(val);
                        break;
                    }
                    ret = instruction(prm, w, all_mem, val, &epoint, epoints);
                    val_destroy(val);
                    if (ret == 0) break;
                    if (ret == 2) {
                        tmp2 = find_label(&opname);
                        if (tmp2) {
                            if (tmp2->value->obj == MACRO_OBJ || tmp2->value->obj == SEGMENT_OBJ || tmp2->value->obj == MFUNC_OBJ) {
                                tmp2->shadowcheck = 1;
                                lpoint=oldlpoint;
                                val = tmp2->value;
                                goto as_macro;
                            }
                        }
                        err_msg2(ERROR_NO_ADDRESSING, NULL, &epoint); 
                    }
                    goto breakerr; /* ellenorizve. */
                }
                tmp2 = find_label(&opname);
                if (tmp2) {
                    if (tmp2->value->obj == MACRO_OBJ || tmp2->value->obj == SEGMENT_OBJ || tmp2->value->obj == MFUNC_OBJ) {
                        tmp2->shadowcheck = 1;
                        val = tmp2->value;goto as_macro;
                    }
                }
            }            /* fall through */
        default: if (waitfor->skip & 1) err_msg(ERROR_GENERL_SYNTAX,NULL); goto breakerr; /* skip things if needed */
        }
    finish:
        ignore();if (here() && here()!=';' && (waitfor->skip & 1)) err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    breakerr:
        if (newlabel && !newlabel->update_after) set_size(newlabel, current_section->address - oaddr, &current_section->mem, newmemp, newmembp);
        continue;
    }

    while (oldwaitforp < waitfor_p) {
        const char *msg = NULL;
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
        if (msg) err_msg2(ERROR______EXPECTED, msg, &waitfor->epoint);
        close_waitfor(waitfor->what);
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
    int opts, i;
    struct file_s *fin, *cfile;
    struct file_list_s *cflist;
    static const str_t none_enc = {4, (const uint8_t *)"none"};

    tinit();

    fin = openfile("", "", 0, NULL);
    opts = testarg(argc,argv, fin);
    init_encoding(arguments.toascii);
    init_defaultlabels();

    if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1]))
        puts("64tass Turbo Assembler Macro V" VERSION "\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");

    /* assemble the input file(s) */
    do {
        struct linepos_s nopoint = {0,0,0};
        if (pass++>max_pass) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        restart_memblocks(&root_section.mem, 0);
        for (i = opts - 1; i<argc; i++) {
            set_cpumode(arguments.cpumode);
            star=databank=dpage=strength=longaccu=longindex=0;actual_encoding=new_encoding(&none_enc);
            allowslowbranch=1;temporary_label_branch=0;
            reset_waitfor();lpoint.line=vline=0;outputeor=0;forwr=backr=0;
            cheap_context=root_label;
            current_context=root_label;
            current_section=&root_section;
            reset_section(current_section);
            init_macro();
            /*	listing=1;flist=stderr;*/
            if (i == opts - 1) {
                cflist = enterfile(fin, &nopoint);
                fin->p = 0;
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(cflist);
                exitfile();
                restart_memblocks(&root_section.mem, 0);
                continue;
            }
            cfile = openfile(argv[i], "", 0, NULL);
            cflist = enterfile(cfile, &nopoint);
            if (cfile) {
                cfile->p = 0;
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cflist);
                closefile(cfile);
            }
            exitfile();
        }
        if (fixeddig && pass != 1) shadow_check(&root_label->members);
        if (errors) {status();return 1;}
    } while (!fixeddig || pass==1);

    /* assemble again to create listing */
    if (arguments.list) {
        struct linepos_s nopoint = {0,0,0};
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
            char *newp = strrchr(*argv2, '/');
            if (newp) *argv2 = newp + 1;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
            newp = strrchr(*argv2, '\\');
            if (newp) *argv2 = newp + 1;
#endif
        }
        while (argc2--) fprintf(flist," %s", *argv2++);
	time(&t); fprintf(flist,"\n; %s",ctime(&t));

        max_pass = pass; pass++;
        fixeddig=1;conderrors=warnings=0;freeerrorlist(0);
        restart_memblocks(&root_section.mem, 0);
        for (i = opts - 1; i<argc; i++) {
            if (i >= opts) {fprintf(flist,"\n;******  Processing input file: %s\n", argv[i]);}
            lastl=LIST_NONE;
            set_cpumode(arguments.cpumode);
            star=databank=dpage=strength=longaccu=longindex=0;actual_encoding=new_encoding(&none_enc);
            allowslowbranch=1;temporary_label_branch=0;
            reset_waitfor();lpoint.line=vline=0;outputeor=0;forwr=backr=0;
            cheap_context=root_label;
            current_context=root_label;
            current_section=&root_section;
            reset_section(current_section);
            init_macro();

            if (i == opts - 1) {
                cflist = enterfile(fin, &nopoint);
                fin->p = 0; 
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(cflist);
                exitfile();
                restart_memblocks(&root_section.mem, 0);
                continue;
            }

            cfile = openfile(argv[i], "", 0, NULL);
            cflist = enterfile(cfile, &nopoint);
            if (cfile) {
                cfile->p = 0;
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cflist);
                closefile(cfile);
            }
            exitfile();
        }
	fputs("\n;******  End of listing\n", flist);
	if (flist != stdout) fclose(flist);
    }

    set_cpumode(arguments.cpumode);

    if (arguments.label) labelprint();

    if (errors || conderrors) {status();return 1;}

    output_mem(&root_section.mem);
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
