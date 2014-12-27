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
#include <locale.h>
#include <wchar.h>
#include <string.h>

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
#include "unicode.h"
#include "listing.h"

#include "listobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "floatobj.h"
#include "addressobj.h"
#include "boolobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"

int temporary_label_branch; /* function declaration in function context, not good */
line_t vline;      /* current line */
address_t all_mem, all_mem2;
uint8_t pass=0, max_pass=MAX_PASS;         /* pass */
address_t star=0;
const uint8_t *pline;           /* current line data */
struct linepos_s lpoint;        /* position in current line */
static uint8_t strength=0;
int fixeddig, constcreated;
uint8_t outputeor = 0; /* EOR value for final output (usually 0, except changed by .eor) */

static size_t waitfor_p, waitfor_len;
static struct waitfor_s {
    enum wait_e what;
    struct linepos_s epoint;
    address_t addr;
    address_t laddr;
    struct label_s *label, *cheap_label;
    size_t memp, membp;
    struct section_s *section;
    value_t val;
    uint8_t skip;
    uint8_t breakout;
} *waitfors, *waitfor, *prevwaitfor;

uint16_t reffile;
uint32_t backr, forwr;
struct avltree *star_tree = NULL;

static const char* command[]={ /* must be sorted, first char is the ID */
    "\x08" "addr",
    "\x22" "al",
    "\x34" "align",
    "\x21" "as",
    "\x35" "assert",
    "\x3a" "bend",
    "\x1a" "binary",
    "\x50" "binclude",
    "\x39" "block",
    "\x5a" "break",
    "\x05" "byte",
    "\x54" "case",
    "\x4e" "cdef",
    "\x32" "cerror",
    "\x06" "char",
    "\x36" "check",
    "\x1b" "comment",
    "\x59" "continue",
    "\x37" "cpu",
    "\x33" "cwarn",
    "\x28" "databank",
    "\x55" "default",
    "\x0d" "dint",
    "\x29" "dpage",
    "\x4c" "dsection",
    "\x47" "dstruct",
    "\x4a" "dunion",
    "\x0e" "dword",
    "\x4f" "edef",
    "\x15" "else",
    "\x17" "elsif",
    "\x2c" "enc",
    "\x3f" "end",
    "\x1c" "endc",
    "\x52" "endf",
    "\x2d" "endif",
    "\x11" "endm",
    "\x1e" "endp",
    "\x46" "ends",
    "\x56" "endswitch",
    "\x49" "endu",
    "\x58" "endweak",
    "\x40" "eor",
    "\x25" "error",
    "\x16" "fi",
    "\x2a" "fill",
    "\x12" "for",
    "\x51" "function",
    "\x44" "goto",
    "\x20" "here",
    "\x3e" "hidemac",
    "\x14" "if",
    "\x2f" "ifeq",
    "\x31" "ifmi",
    "\x2e" "ifne",
    "\x30" "ifpl",
    "\x19" "include",
    "\x09" "int",
    "\x43" "lbl",
    "\x0b" "lint",
    "\x1f" "logical",
    "\x0c" "long",
    "\x10" "macro",
    "\x13" "next",
    "\x04" "null",
    "\x0f" "offs",
    "\x38" "option",
    "\x1d" "page",
    "\x27" "pend",
    "\x26" "proc",
    "\x3c" "proff",
    "\x3b" "pron",
    "\x01" "ptext",
    "\x18" "rept",
    "\x07" "rta",
    "\x4b" "section",
    "\x41" "segment",
    "\x4d" "send",
    "\x02" "shift",
    "\x03" "shiftl",
    "\x3d" "showmac",
    "\x45" "struct",
    "\x53" "switch",
    "\x00" "text",
    "\x48" "union",
    "\x42" "var",
    "\x2b" "warn",
    "\x57" "weak",
    "\x0a" "word",
    "\x24" "xl",
    "\x23" "xs",
};

enum command_e {
    CMD_TEXT=0, CMD_PTEXT, CMD_SHIFT, CMD_SHIFTL, CMD_NULL, CMD_BYTE, CMD_CHAR,
    CMD_RTA, CMD_ADDR, CMD_INT, CMD_WORD, CMD_LINT, CMD_LONG, CMD_DINT,
    CMD_DWORD, CMD_OFFS, CMD_MACRO, CMD_ENDM, CMD_FOR, CMD_NEXT, CMD_IF,
    CMD_ELSE, CMD_FI, CMD_ELSIF, CMD_REPT, CMD_INCLUDE, CMD_BINARY,
    CMD_COMMENT, CMD_ENDC, CMD_PAGE, CMD_ENDP, CMD_LOGICAL, CMD_HERE, CMD_AS,
    CMD_AL, CMD_XS, CMD_XL, CMD_ERROR, CMD_PROC, CMD_PEND, CMD_DATABANK,
    CMD_DPAGE, CMD_FILL, CMD_WARN, CMD_ENC, CMD_ENDIF, CMD_IFNE, CMD_IFEQ,
    CMD_IFPL, CMD_IFMI, CMD_CERROR, CMD_CWARN, CMD_ALIGN, CMD_ASSERT,
    CMD_CHECK, CMD_CPU, CMD_OPTION, CMD_BLOCK, CMD_BEND, CMD_PRON, CMD_PROFF,
    CMD_SHOWMAC, CMD_HIDEMAC, CMD_END, CMD_EOR, CMD_SEGMENT, CMD_VAR, CMD_LBL,
    CMD_GOTO, CMD_STRUCT, CMD_ENDS, CMD_DSTRUCT, CMD_UNION, CMD_ENDU,
    CMD_DUNION, CMD_SECTION, CMD_DSECTION, CMD_SEND, CMD_CDEF, CMD_EDEF,
    CMD_BINCLUDE, CMD_FUNCTION, CMD_ENDF, CMD_SWITCH, CMD_CASE, CMD_DEFAULT,
    CMD_ENDSWITCH, CMD_WEAK, CMD_ENDWEAK, CMD_CONTINUE, CMD_BREAK
};

/* --------------------------------------------------------------------------- */

void status(int anyerr) {
    int errors = error_print(fixeddig, constcreated, anyerr);
    if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1])) {
        error_status();
        printf("Passes:            %u\n",pass);
        if (!errors) sectionprint();
    }
    tfree();
    free_macro();
    free(waitfors);
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
    struct linepos_s lpos = {0, 0};
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
    value_t val = var->value;
    size &= all_mem2;
    if (val->u.code.size != size) {
        val->u.code.size = size;
        if (val->u.code.pass) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&var->name, &var->epoint);
            fixeddig = 0;
        }
    }
    val->u.code.pass = pass;
    val->u.code.mem = mem;
    val->u.code.memp = memp;
    val->u.code.membp = membp;
}

static int toival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t err;
    obj_t obj = v1->obj;

    if (obj == NONE_OBJ) {
        err_msg_still_none(NULL, epoint);
        return 1;
    }
    err = obj->ival(v1, iv, bits, epoint);
    if (err) {
        err_msg_output_and_destroy(err);
        return 1;
    }
    return 0;
}

static int tobool(const struct values_s *v1, int *truth) {
    value_t err;
    value_t val = v1->val;
    obj_t obj = val->obj;

    if (obj == ERROR_OBJ) {
        err_msg_output(val);
        return 1;
    }
    if (obj == NONE_OBJ) {
        err_msg_still_none(NULL, &v1->epoint);
        return 1;
    }
    err = obj->truth(val, TRUTH_BOOL, &v1->epoint);
    if (err->obj != BOOL_OBJ) {
        err_msg_output_and_destroy(err);
        return 1;
    }
    *truth = err->u.boolean;
    val_destroy(err);
    return 0;
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
static int get_command(void) {
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
                    return (uint8_t)command[no][0];
                }

                elozo = no;
                no = ((s4>0) ? (felso+(also=no)) : (also+(felso=no)))/2;
                if (elozo == no) break;
            }
        }
    }
    lpoint.pos -= l;
    return sizeof(command)/sizeof(command[0]);
}

/* ------------------------------------------------------------------------------ */

static void set_cpumode(const struct cpu_s *cpumode) {
    all_mem = cpumode->max_address;
    all_mem2 = (arguments.output_mode == OUTPUT_FLAT) ? ~(address_t)0 : all_mem;
    select_opcodes(cpumode);
    listing_set_cpumode(cpumode);
}

void var_assign(struct label_s *tmp, value_t val, int fix) {
    tmp->defpass = pass;
    if (obj_same(val, tmp->value)) return;
    val_replace(&tmp->value, val);
    if (tmp->usepass < pass) return;
    if (fixeddig && !fix && pass > max_pass) err_msg_cant_calculate(&tmp->name, &tmp->epoint);
    fixeddig = fix;
}

static int textrecursion(value_t val, int prm, int *ch2, size_t *uninit, size_t *sum, linepos_t epoint2) {
    value_t iter, val2;
    uval_t uval;
    int warn = 0;
    if (val->obj == STR_OBJ || val->obj == BITS_OBJ) {
        value_t tmp = BYTES_OBJ->create(val, epoint2);
        iter = tmp->obj->getiter(tmp);
        val_destroy(tmp);
    } else iter = val->obj->getiter(val);

    while ((val2 = obj_next(iter))) {
        switch (val2->obj->type) {
        case T_LIST:
        case T_TUPLE:
        case T_STR: warn |= textrecursion(val2, prm, ch2, uninit, sum, epoint2); break;
        case T_GAP:
            if (*ch2 >= 0) {
                if (*uninit) { memskip(*uninit); (*sum) += *uninit; *uninit = 0; }
                pokeb(*ch2); (*sum)++;
            }
            *ch2 = -1; (*uninit)++; break;
        default:
            if (*ch2 >= 0) {
                if (*uninit) { memskip(*uninit); (*sum) += *uninit; *uninit = 0; }
                pokeb(*ch2); (*sum)++;
            }
            if (touval(val2, &uval, 8, epoint2)) uval = 256;
            switch (prm) {
            case CMD_SHIFT:
                if (uval & 0x80) err_msg2(ERROR___NO_HIGH_BIT, NULL, epoint2);
                *ch2 = uval & 0x7f;
                break;
            case CMD_SHIFTL:
                if (uval & 0x80) err_msg2(ERROR___NO_HIGH_BIT, NULL, epoint2);
                *ch2 = (uval << 1) & 0xfe;
                break;
            case CMD_NULL:
                if (!uval) err_msg2(ERROR_NO_ZERO_VALUE, NULL, epoint2);
                /* fall through */
            default:
                *ch2 = uval & 0xff;
                break;
            }
            break;
        case T_NONE:
            warn = 1;
        }
        val_destroy(val2);
    }
    val_destroy(iter);
    return warn;
}

static int byterecursion(value_t val, int prm, size_t *uninit, int bits, linepos_t epoint) {
    value_t iter, val2;
    uint32_t ch2;
    uval_t uv;
    ival_t iv;
    int warn = 0;
    if (val->obj == LIST_OBJ || val->obj == TUPLE_OBJ) iter = val->obj->getiter(val);
    else iter = invalid_getiter(val);
    while ((val2 = obj_next(iter))) {
        switch (val2->obj->type) {
        case T_LIST:
        case T_TUPLE: warn |= byterecursion(val2, prm, uninit, bits, epoint); continue;
        case T_GAP: *uninit += abs(bits) / 8; val_destroy(val2); continue;
        default:
            if (prm == CMD_RTA || prm == CMD_ADDR) {
                if (touval(val2, &uv, 24, epoint)) uv = 1;
                else if ((current_section->l_address ^ uv) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, NULL, epoint);
                ch2 = uv - (prm == CMD_RTA);
                break;
            }
            if (bits >= 0) {
                if (touval(val2, &uv, bits, epoint)) uv = 0;
                ch2 = uv;
            } else {
                if (toival(val2, &iv, -bits, epoint)) iv = 0;
                ch2 = iv;
            }
            break;
        case T_NONE:
            warn = 1;
            ch2 = 0;
        }
        if (*uninit) {memskip(*uninit);*uninit = 0;}
        pokeb((uint8_t)ch2);
        if (prm>=CMD_RTA) pokeb((uint8_t)(ch2>>8));
        if (prm>=CMD_LINT) pokeb((uint8_t)(ch2>>16));
        if (prm>=CMD_DINT) pokeb((uint8_t)(ch2>>24));
        val_destroy(val2);
    }
    val_destroy(iter);
    return warn;
}

static void instrecursion(value_t val, int prm, int w, linepos_t epoint, struct linepos_s *epoints) {
    size_t i;
    value_t err;
    for (i = 0; i < val->u.list.len; i++) {
        if (val->u.list.data[i]->obj == TUPLE_OBJ || val->u.list.data[i]->obj == LIST_OBJ) {
            instrecursion(val->u.list.data[i], prm, w, epoint, epoints);
            continue;
        }
        err = instruction(prm, w, val->u.list.data[i], epoint, epoints);
        if (err) err_msg_output_and_destroy(err);
    }
}

value_t compile(struct file_list_s *cflist)
{
    int wht,w;
    int prm = 0;
    value_t val;

    struct label_s *newlabel = NULL, *oldcheap = NULL;
    size_t newmemp = 0, newmembp = 0;
    struct label_s *tmp2 = NULL, *mycontext;
    address_t oaddr = 0;
    value_t retval = NULL;

    size_t oldwaitforp = waitfor_p;
    int nobreak = 1;
    str_t labelname;
    struct {
        uint8_t dir;
        uint8_t padding;
        uint16_t reffile;
        int32_t count;
    } anonident;
    struct {
        uint8_t type;
        uint8_t padding[3];
        line_t vline;
        struct avltree *star_tree;
    } anonident2;
    struct linepos_s epoint;
    struct file_s *cfile = cflist->file;

    while (nobreak) {
        if (mtranslate(cfile)) break; /* expand macro parameters, if any */
        star=current_section->l_address;newlabel = NULL;
        labelname.len = 0;ignore();epoint = lpoint; mycontext = current_context;
        if (current_section->unionmode) {
            if (current_section->address > current_section->unionend) current_section->unionend = current_section->address;
            if (current_section->l_address > current_section->l_unionend) current_section->l_unionend = current_section->l_address;
            current_section->l_address = current_section->l_unionstart;
            if (current_section->address != current_section->unionstart) {
                current_section->address = current_section->unionstart;
                memjmp(&current_section->mem, current_section->address);
            }
        }
        wht = here();
        if (wht =='-' || wht =='+') {
            lpoint.pos++;if (here()!=0x20 && here()!=0x09 && here()!=';' && here()) {
                lpoint.pos--;
            } else {
                if (sizeof(anonident) != sizeof(anonident.dir) + sizeof(anonident.padding) + sizeof(anonident.reffile) + sizeof(anonident.count)) memset(&anonident, 0, sizeof(anonident));
                else anonident.padding = 0;
                anonident.dir = wht;
                anonident.reffile = reffile;
                anonident.count = (wht == '-') ? backr++ : forwr++;

                labelname.data = (const uint8_t *)&anonident;labelname.len = sizeof(anonident);
                goto hh;
            }
        }
        labelname.data = pline + lpoint.pos; labelname.len = get_label();
        if (labelname.len) {
            struct linepos_s cmdpoint;
            int islabel;
            islabel = 0;
            while (here() == '.') {
                if (waitfor->skip & 1) {
                    if (mycontext == current_context) {
                        tmp2 = (labelname.len && labelname.data[0] == '_') ? find_label2(&labelname, cheap_context) : find_label(&labelname);
                        if (tmp2) tmp2->shadowcheck = (labelname.data[0] != '_');
                    }
                    else tmp2 = find_label2(&labelname, mycontext);
                    if (!tmp2) {err_msg_not_definedx(&labelname, &epoint); goto breakerr;}
                    if (tmp2->value->obj != CODE_OBJ) {
                        err_msg_wrong_type(tmp2->value, CODE_OBJ, &epoint); goto breakerr;
                    }
                    mycontext = tmp2;
                }
                lpoint.pos++; islabel = 1; epoint = lpoint;
                labelname.data = pline + lpoint.pos; labelname.len = get_label();
                if (!labelname.len) {
                    if (waitfor->skip & 1) err_msg2(ERROR_GENERL_SYNTAX, NULL, &lpoint);
                    goto breakerr;
                }
            }
            if (!islabel && labelname.len && labelname.data[0] == '_') {
                mycontext = cheap_context;
            }
            if (here()==':') {islabel = 1; lpoint.pos++;}
            if (!islabel && labelname.len == 3 && (prm=lookup_opcode((const char *)labelname.data))>=0) {
                if (waitfor->skip & 1) goto as_opcode; else continue;
            }
            if (0) {
            hh: islabel = 1;
            }
            ignore();wht = here();
            if (!(waitfor->skip & 1)) {epoint = lpoint; goto jn;} /* skip things if needed */
            if (labelname.len > 1 && labelname.data[0] == '_' && labelname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint); goto breakerr;}
            if (wht == '=') { /* variable */
                struct label_s *label;
                int labelexists;
                int oldreferenceit = referenceit;
                label = find_label3(&labelname, mycontext, strength);
                lpoint.pos++; ignore();
                if (!here() || here() == ';') val = val_reference(null_addrlist);
                else {
                    struct linepos_s epoints[3];
                    referenceit &= label ? label->ref : 1;
                    if (!get_exp(&w, 0, cfile, 0, 0, NULL)) goto breakerr;
                    val = get_vals_addrlist(epoints);
                    referenceit = oldreferenceit;
                }
                if (label) labelexists = 1;
                else label = new_label(&labelname, mycontext, strength, &labelexists);
                oaddr=current_section->address;
                listing_equal(val);
                label->ref = 0;
                if (labelexists) {
                    if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                    else {
                        if (label->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                        label->constant = 1;
                        label->requires = current_section->requires;
                        label->conflicts = current_section->conflicts;
                        var_assign(label, val, 0);
                    }
                    val_destroy(val);
                } else {
                    constcreated |= !temporary_label_branch;
                    label->constant = 1;
                    label->requires = current_section->requires;
                    label->conflicts = current_section->conflicts;
                    label->value = val;
                    label->file_list = cflist;
                    label->epoint = epoint;
                }
                goto finish;
            }
            if (wht == '.') {
                cmdpoint = lpoint;
                prm = get_command();
                ignore();
                switch (prm) {
                case CMD_VAR: /* variable */
                    {
                        struct label_s *label;
                        int labelexists;
                        int oldreferenceit = referenceit;
                        label=find_label3(&labelname, mycontext, strength);
                        if (!here() || here() == ';') val = val_reference(null_addrlist);
                        else {
                            struct linepos_s epoints[3];
                            referenceit &= 1; /* not good... */
                            if (!get_exp(&w, 0, cfile, 0, 0, NULL)) goto breakerr;
                            val = get_vals_addrlist(epoints);
                            referenceit = oldreferenceit;
                        }
                        if (label) labelexists = 1;
                        else label = new_label(&labelname, mycontext, strength, &labelexists);
                        oaddr=current_section->address;
                        listing_equal(val);
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
                        listing_line(0);
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (label->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                val = val_alloc(LBL_OBJ);
                                val->u.lbl.sline = lpoint.line;
                                val->u.lbl.waitforp = waitfor_p;
                                val->u.lbl.file_list = cflist;
                                val->u.lbl.parent = current_context;
                                var_assign(label, val, 0);
                                val_destroy(val);
                            }
                        } else {
                            constcreated |= !temporary_label_branch;
                            val = val_alloc(LBL_OBJ);
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
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
                        listing_line(0);
                        new_waitfor(W_ENDM, &lpoint);waitfor->skip=0;
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (label->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                val = val_alloc(obj);
                                val->u.macro.size = 0;
                                val->u.macro.label = label;
                                get_macro_params(val);
                                var_assign(label, val, 0);
                                val_destroy(val);
                            }
                        } else {
                            constcreated |= !temporary_label_branch;
                            val = val_alloc(obj);
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                            val->u.macro.size = 0;
                            val->u.macro.label = label;
                            get_macro_params(val);
                        }
                        label->ref = 0;
                        goto finish;
                    }
                case CMD_FUNCTION:
                    {
                        struct label_s *label;
                        int labelexists;
                        listing_line(0);
                        new_waitfor(W_ENDF, &lpoint);waitfor->skip=0;
                        if (temporary_label_branch) {err_msg2(ERROR___NOT_ALLOWED, ".FUNCTION", &cmdpoint);goto breakerr;}
                        label=new_label(&labelname, mycontext, strength, &labelexists);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (label->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                val = val_alloc(MFUNC_OBJ);
                                val->u.mfunc.label = label;
                                get_func_params(val, cfile);
                                var_assign(label, val, 0);
                                val_destroy(val);
                            }
                        } else {
                            constcreated |= !temporary_label_branch;
                            val = val_alloc(MFUNC_OBJ);
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
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
                        struct section_s olds = *current_section;
                        int labelexists;
                        obj_t obj = (prm == CMD_STRUCT) ? STRUCT_OBJ : UNION_OBJ;

                        new_waitfor((prm==CMD_STRUCT)?W_ENDS:W_ENDU, &lpoint);waitfor->skip=0;
                        label=new_label(&labelname, mycontext, strength, &labelexists);

                        current_section->provides=~(uval_t)0;current_section->requires=current_section->conflicts=0;
                        current_section->end=current_section->start=current_section->restart=current_section->l_restart=current_section->address=current_section->l_address=0;
                        current_section->dooutput=0;memjmp(&current_section->mem, 0);

                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (label->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                                label->constant = 1;
                                label->requires = 0;
                                label->conflicts = 0;
                                val = val_alloc(obj);
                                val->u.macro.size = (label->value->obj == obj) ? label->value->u.macro.size : 0;
                                val->u.macro.label = label;
                                get_macro_params(val);
                                var_assign(label, val, 0);
                                val_destroy(val);
                            }
                        } else {
                            constcreated |= !temporary_label_branch;
                            val = val_alloc(obj);
                            label->constant = 1;
                            label->requires = 0;
                            label->conflicts = 0;
                            label->value = val;
                            label->file_list = cflist;
                            label->epoint = epoint;
                            val->u.macro.size = 0;
                            val->u.macro.label = label;
                            get_macro_params(val);
                        }

                        if (label) {
                            label->ref = 0;
                        }
                        listing_line(cmdpoint.pos);
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
                        } else err_msg2(ERROR__MACRECURSION, NULL, &cmdpoint);
                        current_section->structrecursion--;
                        if (!label) goto finish;

                        if (label->value->u.macro.size != (current_section->address & all_mem2)) {
                            label->value->u.macro.size = current_section->address & all_mem2;
                            if (label->usepass >= pass) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
                                fixeddig = 0;
                            }
                        }
                        current_section->provides=olds.provides;current_section->requires=olds.requires;current_section->conflicts=olds.conflicts;
                        current_section->end=olds.end;current_section->start=olds.start;current_section->restart=olds.restart;current_section->l_restart=olds.l_restart;current_section->address=olds.address;current_section->l_address=olds.l_address;
                        current_section->dooutput=olds.dooutput;memjmp(&current_section->mem, current_section->address);
                        goto finish;
                    }
                case CMD_SECTION:
                    {
                        struct section_s *tmp;
                        str_t sectionname;
                        struct linepos_s opoint;
                        new_waitfor(W_SEND, &lpoint);waitfor->section=current_section;
                        opoint=lpoint;
                        sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                        if (!sectionname.len) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &opoint); goto breakerr;}
                        tmp=find_new_section(&sectionname);
                        if (!tmp->usepass || tmp->defpass < pass - 1) {
                            if (tmp->usepass && tmp->usepass >= pass - 1) {err_msg_not_defined(&sectionname, &opoint); goto breakerr;}
                            tmp->end = tmp->start = tmp->restart = tmp->address = 0;
                            tmp->size = tmp->l_restart = tmp->l_address = 0;
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&sectionname, &opoint);
                            fixeddig=0;
                            tmp->defpass = pass - 1;
                            restart_memblocks(&tmp->mem, tmp->address);
                        } else if (tmp->usepass != pass) {
                            if (!tmp->moved) {
                                if (tmp->end < tmp->address) tmp->end = tmp->address;
                                tmp->moved = 1;
                            }
                            tmp->size = tmp->end - tmp->start;
                            tmp->end = tmp->start = tmp->restart;
                            tmp->wrapwarn = tmp->wrapwarn2 = 0;
                            tmp->address = tmp->restart;
                            tmp->l_address = tmp->l_restart;
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
                    obj_t obj = tmp2->value->obj;
                    if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                        tmp2->shadowcheck = 1;
                        labelname.len = 0;val = tmp2->value; goto as_macro;
                    }
                }
                if (epoint.pos) err_msg2(ERROR_LABEL_NOT_LEF, NULL, &epoint);
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
                        newlabel = NULL; epoint = lpoint; if (wht == '.') goto as_command; else goto jn;
                    }
                    if (newlabel->defpass != pass - 1 && !temporary_label_branch) constcreated = 1;
                    newlabel->constant = 1;
                    newlabel->requires = current_section->requires;
                    newlabel->conflicts = current_section->conflicts;
                    if (!newlabel->update_after) {
                        value_t tmp;
                        tmp = int_from_uval(current_section->l_address);
                        if (!obj_same(tmp, newlabel->value->u.code.addr)) {
                            val_destroy(newlabel->value->u.code.addr);
                            newlabel->value->u.code.addr = tmp;
                            if (newlabel->usepass >= pass) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
                                fixeddig = 0;
                            }
                        } else val_destroy(tmp);
                        get_mem(&current_section->mem, &newmemp, &newmembp);
                        newlabel->value->u.code.apass = pass;
                        newlabel->defpass = pass;
                    }
                } else {
                    constcreated |= !temporary_label_branch;
                    val = val_alloc(CODE_OBJ);
                    newlabel->constant = 1;
                    newlabel->requires=current_section->requires;
                    newlabel->conflicts=current_section->conflicts;
                    newlabel->value = val;
                    newlabel->file_list = cflist;
                    newlabel->epoint = epoint;
                    val->u.code.addr = int_from_uval(current_section->l_address);
                    val->u.code.size = 0;
                    val->u.code.dtype = D_NONE;
                    val->u.code.pass = 0;
                    val->u.code.apass = pass;
                    val->u.code.label = newlabel;
                    get_mem(&current_section->mem, &newmemp, &newmembp);
                }
            }
            if (wht == '.') { /* .proc */
                epoint = cmdpoint;
                switch (prm) {
                case CMD_PROC:
                    listing_line(epoint.pos);
                    new_waitfor(W_PEND, &epoint);waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;waitfor->cheap_label = oldcheap;
                    if (!newlabel->ref && newlabel->value->u.code.pass) {waitfor->skip=0; set_size(newlabel, 0, &current_section->mem, newmemp, newmembp);}
                    else {         /* TODO: first time it should not compile */
                        current_context=newlabel;
                        newlabel->ref = 0;
                    }
                    newlabel = NULL;
                    goto finish;
                case CMD_DSTRUCT: /* .dstruct */
                case CMD_DUNION:
                    {
                        int old_unionmode = current_section->unionmode;
                        struct values_s *vs;
                        obj_t obj;
                        address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                        address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                        listing_line(epoint.pos);
                        newlabel->ref = 0;
                        if (!get_exp(&w, 1, cfile, 1, 0, &epoint)) goto breakerr;
                        vs = get_val(); val = vs->val;
                        if (val->obj == ERROR_OBJ) { err_msg_output(val); goto finish; }
                        if (val->obj == NONE_OBJ) { err_msg_still_none(NULL, &vs->epoint); goto finish;}
                        obj = (prm == CMD_DSTRUCT) ? STRUCT_OBJ : UNION_OBJ;
                        if (val->obj != obj) {err_msg_wrong_type(val, obj, &vs->epoint); goto breakerr;}
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
                    listing_line(epoint.pos);
                    newlabel->ref = 0;
                    newlabel = NULL;
                    goto finish;
                }
                newlabel->ref = 0;
                goto as_command;
            }
            epoint = lpoint;
            newlabel->ref = 0;
        }
        jn:
        switch (wht) {
        case '=':
            if (waitfor->skip & 1) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
            break;
        case '*':if (waitfor->skip & 1) /* skip things if needed */
            {
                struct values_s *vs;
                lpoint.pos++;ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                lpoint.pos++;
                current_section->wrapwarn = current_section->wrapwarn2 = 0;
                if (!current_section->moved) {
                    if (current_section->end < current_section->address) current_section->end = current_section->address;
                    current_section->moved = 1;
                }
                if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                vs = get_val();
                listing_line(epoint.pos);
                if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, "*=", &epoint);
                else {
                    uval_t uval;
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) break;
                    if ((arguments.output_mode == OUTPUT_FLAT) && !current_section->logicalrecursion) {
                        if ((address_t)uval & ~all_mem2) err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);
                        else {
                            current_section->l_address = (address_t)uval & all_mem;
                            if (current_section->address != (address_t)uval) {
                                current_section->address = (address_t)uval;
                                memjmp(&current_section->mem, current_section->address);
                            }
                        }
                    } else {
                        if ((uval_t)uval & ~(uval_t)all_mem) err_msg2(ERROR_ADDRESS_LARGE, NULL, &vs->epoint);
                        else {
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
        case ';':
        case '\0':
            if (waitfor->skip & 1) listing_line(epoint.pos);
            break;
        case '.':
            prm = get_command();
            ignore();
        as_command:
            switch (prm) {
            case CMD_ENDC: /* .endc */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (!close_waitfor(W_ENDC)) err_msg2(ERROR______EXPECTED,".COMMENT", &epoint);
                if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                break;
            case CMD_ENDIF: /* .endif */
            case CMD_FI: /* .fi */
                {
                    if (waitfor->skip & 1) listing_line(epoint.pos);
                    if (!close_waitfor(W_FI2) && !close_waitfor(W_FI)) err_msg2(ERROR______EXPECTED,".IF", &epoint);
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_ENDSWITCH: /* .endswitch */
                {
                    if (waitfor->skip & 1) listing_line(epoint.pos);
                    if (!close_waitfor(W_SWITCH2) && !close_waitfor(W_SWITCH)) err_msg2(ERROR______EXPECTED,".SWITCH", &epoint);
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_DEFAULT: /* .default */
                {
                    if (waitfor->skip & 1) listing_line_cut(epoint.pos);
                    if (waitfor->what==W_SWITCH) {err_msg2(ERROR______EXPECTED,".ENDSWITCH", &epoint); break;}
                    if (waitfor->what!=W_SWITCH2) {err_msg2(ERROR______EXPECTED,".SWITCH", &epoint); break;}
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_SWITCH;waitfor->epoint = epoint;
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_ELSE: /* .else */
                {
                    if (waitfor->skip & 1) listing_line_cut(epoint.pos);
                    if (waitfor->what==W_FI) { err_msg2(ERROR______EXPECTED,".FI", &epoint); break; }
                    if (waitfor->what!=W_FI2) { err_msg2(ERROR______EXPECTED,".IF", &epoint); break; }
                    waitfor->skip=waitfor->skip >> 1;
                    waitfor->what=W_FI;waitfor->epoint = epoint;
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_IF: /* .if */
            case CMD_IFEQ: /* .ifeq */
            case CMD_IFNE: /* .ifne */
            case CMD_IFPL: /* .ifpl */
            case CMD_IFMI: /* .ifmi */
                {
                    uint8_t skwait = waitfor->skip;
                    ival_t ival;
                    int truth;
                    value_t err;
                    struct values_s *vs;
                    if (waitfor->skip & 1) listing_line(epoint.pos);
                    new_waitfor(W_FI2, &epoint);
                    if (skwait != 1) { waitfor->skip = 0; break; }
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) { waitfor->skip = 0; goto breakerr;}
                    vs = get_val(); val = vs->val;
                    if (val->obj == ERROR_OBJ) { err_msg_output(val); waitfor->skip = 0; break;}
                    if (val->obj == NONE_OBJ) { err_msg_still_none(NULL, &vs->epoint); waitfor->skip = 0; break;}
                    switch (prm) {
                    case CMD_IF:
                        if (tobool(vs, &truth)) { waitfor->skip = 0; break; }
                        waitfor->skip = truth ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                        break;
                    case CMD_IFNE:
                    case CMD_IFEQ:
                        err = val->obj->sign(val, &vs->epoint);
                        if (err->obj != INT_OBJ) {err_msg_output_and_destroy(err); waitfor->skip = 0; break; }
                        waitfor->skip = ((err->u.integer.len == 0) ^ (prm == CMD_IFNE)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                        val_destroy(err);
                        break;
                    case CMD_IFPL:
                    case CMD_IFMI:
                        if (arguments.tasmcomp) {
                            if (toival(val, &ival, 8*sizeof(uval_t), &vs->epoint)) { waitfor->skip = 0; break; }
                            waitfor->skip = ((!(ival & 0x8000)) ^ (prm == CMD_IFMI)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                        } else {
                            err = val->obj->sign(val, &vs->epoint);
                            if (err->obj != INT_OBJ) { err_msg_output_and_destroy(err); waitfor->skip = 0; break; }
                            waitfor->skip = ((err->u.integer.len >= 0) ^ (prm == CMD_IFMI)) ? (prevwaitfor->skip & 1) : ((prevwaitfor->skip & 1) << 1);
                            val_destroy(err);
                        }
                        break;
                    }
                }
                break;
            case CMD_ELSIF: /* .elsif */
                {
                    uint8_t skwait = waitfor->skip;
                    int truth;
                    struct values_s *vs;
                    if (waitfor->skip & 1) listing_line_cut(epoint.pos);
                    if (waitfor->what != W_FI2) {err_msg2(ERROR______EXPECTED, ".IF", &epoint); break;}
                    waitfor->epoint = epoint;
                    if (skwait == 2) {
                        if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) { waitfor->skip = 0; goto breakerr;}
                        vs = get_val();
                    } else { waitfor->skip = 0; break; }
                    if (tobool(vs, &truth)) { waitfor->skip = 0; break; }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_SWITCH: /* .switch */
                {
                    uint8_t skwait = waitfor->skip;
                    if (waitfor->skip & 1) listing_line(epoint.pos);
                    new_waitfor(W_SWITCH2, &epoint);
                    if (skwait==1) {
                        struct values_s *vs;
                        if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) {waitfor->skip = 0; goto breakerr;}
                        vs = get_val(); val = vs->val;
                        if (val->obj == ERROR_OBJ) { err_msg_output(val); val = none_value; }
                        else if (val->obj == NONE_OBJ) err_msg_still_none(NULL, &vs->epoint);
                    } else val = none_value;
                    waitfor->val = val_reference(val);
                    waitfor->skip = (val->obj == NONE_OBJ) ? 0 : ((prevwaitfor->skip & 1) << 1);
                }
                break;
            case CMD_CASE: /* .case */
                {
                    uint8_t skwait = waitfor->skip;
                    int truth = 0;
                    if (waitfor->skip & 1) listing_line_cut(epoint.pos);
                    if (waitfor->what == W_SWITCH) { err_msg2(ERROR______EXPECTED,".ENDSWITCH", &epoint); goto breakerr; }
                    if (waitfor->what != W_SWITCH2) { err_msg2(ERROR______EXPECTED,".SWITCH", &epoint); goto breakerr; }
                    waitfor->epoint = epoint;
                    if (skwait==2) {
                        struct values_s *vs;
                        value_t result2;
                        struct oper_s tmp;
                        if (!get_exp(&w, 0, cfile, 1, 0, &epoint)) { waitfor->skip = 0; goto breakerr; }
                        tmp.op = &o_EQ;
                        tmp.v1 = waitfor->val;
                        tmp.epoint = tmp.epoint3 = &epoint;
                        while (!truth && (vs = get_val())) {
                            val = vs->val;
                            if (val->obj == ERROR_OBJ) { err_msg_output(val); continue; }
                            if (val->obj == NONE_OBJ) { err_msg_still_none(NULL, &vs->epoint);continue; }
                            tmp.v2 = val;
                            tmp.epoint2 = &vs->epoint;
                            result2 = tmp.v1->obj->calc2(&tmp);
                            truth = result2->obj == BOOL_OBJ && result2->u.boolean;
                            val_destroy(result2);
                        }
                    }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_ENDM: /* .endm */
                if (close_waitfor(W_ENDM)) {
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                    goto breakerr;
                } else if (close_waitfor(W_ENDM2)) {
                    nobreak=0;
                    if (here() && here() != ';' && get_exp(&w, 0, cfile, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                    }
                } else {
                    err_msg2(ERROR______EXPECTED,".MACRO or .SEGMENT", &epoint);
                    goto breakerr;
                }
                break;
            case CMD_ENDF: /* .endf */
                if (close_waitfor(W_ENDF)) {
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                    goto breakerr;
                } else if (close_waitfor(W_ENDF2)) {
                    nobreak = 0;
                    if (here() && here() != ';' && get_exp(&w, 0, cfile, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                    }
                } else {
                    err_msg2(ERROR______EXPECTED,".FUNCTION", &epoint);
                    goto breakerr;
                }
                break;
            case CMD_NEXT: /* .next */
                waitfor->epoint = epoint;
                if (close_waitfor(W_NEXT)) {
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                } else if (close_waitfor(W_NEXT2)) {
                    nobreak=0;
                } else err_msg2(ERROR______EXPECTED,".FOR or .REPT", &epoint);
                break;
            case CMD_PEND: /* .pend */
                if (waitfor->what==W_PEND) {
                    if (waitfor->skip & 1) {
                        listing_line(epoint.pos);
                        if (waitfor->cheap_label) cheap_context = waitfor->cheap_label;
                        if (current_context->parent) {
                            current_context = current_context->parent;
                        } else err_msg2(ERROR______EXPECTED,".proc", &epoint);
                        if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                    }
                    close_waitfor(W_PEND);
                    if (waitfor->skip & 1) listing_line_cut2(epoint.pos);
                } else err_msg2(ERROR______EXPECTED,".PROC", &epoint);
                break;
            case CMD_ENDS: /* .ends */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_ENDS)) {
                    goto breakerr;
                } else if (close_waitfor(W_ENDS2)) {
                    nobreak=0;
                    if (here() && here() != ';' && get_exp(&w, 0, cfile, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                    }
                } else err_msg2(ERROR______EXPECTED,".STRUCT", &epoint);
                break;
            case CMD_SEND: /* .send */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_SEND)) {
                    get_label();
                } else if (waitfor->what==W_SEND2) {
                    str_t sectionname;
                    epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (sectionname.len) {
                        str_t cf;
                        str_cfcpy(&cf, &sectionname);
                        if (str_cmp(&cf, &current_section->cfname)) {
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
            case CMD_ENDU: /* .endu */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_ENDU)) {
                } else if (close_waitfor(W_ENDU2)) {
                    nobreak=0; current_section->l_address = current_section->l_unionend;
                    if (current_section->address != current_section->unionend) {
                        current_section->address = current_section->unionend;
                        memjmp(&current_section->mem, current_section->address);
                    }
                } else err_msg2(ERROR______EXPECTED,".UNION", &epoint);
                break;
            case CMD_ENDP: /* .endp */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_ENDP)) {
                } else if (waitfor->what==W_ENDP2) {
                    if ((current_section->l_address ^ waitfor->laddr) > 0xff) {
                        err_msg2(ERROR____PAGE_ERROR, &current_section->l_address, &epoint);
                    }
                    if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                    close_waitfor(W_ENDP2);
                } else err_msg2(ERROR______EXPECTED,".PAGE", &epoint);
                break;
            case CMD_HERE: /* .here */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_HERE)) {
                    current_section->logicalrecursion--;
                } else if (waitfor->what==W_HERE2) {
                    current_section->l_address = current_section->address + waitfor->laddr;
                    if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                    close_waitfor(W_HERE2);
                    current_section->logicalrecursion--;
                } else err_msg2(ERROR______EXPECTED,".LOGICAL", &epoint);
                break;
            case CMD_BEND: /* .bend */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_BEND)) {
                } else if (waitfor->what==W_BEND2) {
                    if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                    if (waitfor->cheap_label) cheap_context = waitfor->cheap_label;
                    if (current_context->parent) current_context = current_context->parent;
                    else err_msg2(ERROR______EXPECTED,".block", &epoint);
                    close_waitfor(W_BEND2);
                } else err_msg2(ERROR______EXPECTED,".BLOCK", &epoint);
                break;
            case CMD_ENDWEAK: /* .endweak */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                if (close_waitfor(W_WEAK)) {
                    strength--;
                } else if (waitfor->what==W_WEAK2) {
                    if (waitfor->label) set_size(waitfor->label, current_section->address - waitfor->addr, &current_section->mem, waitfor->memp, waitfor->membp);
                    close_waitfor(W_WEAK2);
                    strength--;
                } else err_msg2(ERROR______EXPECTED,".WEAK", &epoint);
                break;
            case CMD_END: /* .end */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                nobreak=0;
                break;
            case CMD_TEXT: /* .text */
            case CMD_PTEXT: /* .ptext */
            case CMD_SHIFT: /* .shift */
            case CMD_SHIFTL: /* .shiftl */
            case CMD_NULL: /* .null */
            case CMD_BYTE: /* .byte */
            case CMD_CHAR: /* .char */
            case CMD_RTA: /* .rta */
            case CMD_ADDR: /* .addr */
            case CMD_INT: /* .int */
            case CMD_WORD: /* .word */
            case CMD_LINT: /* .lint */
            case CMD_LONG: /* .long */
            case CMD_DINT: /* .dint */
            case CMD_DWORD: /* .dword */
            case CMD_BINARY: if (waitfor->skip & 1)
                { /* .binary */
                    size_t uninit = 0;
                    size_t sum = 0;

                    mark_mem(&current_section->mem, current_section->address);
                    if (prm<CMD_BYTE) {    /* .text .ptext .shift .shiftl .null */
                        int ch2=-1;
                        struct values_s *vs;
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (prm==CMD_PTEXT) ch2=0;
                        if (!get_exp(&w, 0, cfile, 0, 0, NULL)) goto breakerr;
                        while ((vs = get_val())) {
                            if (textrecursion(vs->val, prm, &ch2, &uninit, &sum, &vs->epoint)) err_msg_still_none(NULL, &vs->epoint);
                        }
                        if (uninit) {memskip(uninit);sum += uninit;}
                        if (ch2 >= 0) {
                            if (prm==CMD_SHIFT) ch2|=0x80;
                            if (prm==CMD_SHIFTL) ch2|=0x01;
                            pokeb(ch2); sum++;
                        } else if (prm==CMD_SHIFT || prm==CMD_SHIFTL) err_msg_wrong_type(gap_value, NULL, &vs->epoint);
                        if (prm==CMD_NULL) pokeb(0);
                        if (prm==CMD_PTEXT) {
                            if (sum > 0x100) err_msg2(ERROR____PTEXT_LONG, &sum, &epoint);

                            if (current_section->dooutput) write_mark_mem(&current_section->mem, sum-1);
                        }
                    } else if (prm<=CMD_DWORD) { /* .byte .word .int .rta .long */
                        int bits;
                        struct values_s *vs;
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
                            case CMD_ADDR:
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
                        if (!get_exp(&w, 0, cfile, 0, 0, NULL)) goto breakerr;
                        while ((vs = get_val())) {
                            if (byterecursion(vs->val, prm, &uninit, bits, &vs->epoint)) err_msg_still_none(NULL, &vs->epoint);
                        }
                        if (uninit) memskip(uninit);
                    } else if (prm==CMD_BINARY) { /* .binary */
                        const char *path = NULL;
                        size_t foffset = 0;
                        value_t val2 = NULL;
                        size_t fsize = all_mem2 + 1;
                        uval_t uval;
                        struct values_s *vs;

                        if (fsize == 0) fsize = all_mem2; /* overflow */
                        if (newlabel) {
                            newlabel->value->u.code.dtype = D_BYTE;
                        }
                        if (!get_exp(&w, 0, cfile, 1, 3, &epoint)) goto breakerr;
                        vs = get_val(); val = vs->val;
                        if (val->obj == ERROR_OBJ) err_msg_output(val);
                        else if (val->obj == NONE_OBJ) err_msg_still_none(NULL, &vs->epoint);
                        else if (val->obj != STR_OBJ) err_msg_wrong_type(val, STR_OBJ, &vs->epoint);
                        else {
                            path = get_path(val, cfile->realname);
                            val2 = val;
                        }
                        if ((vs = get_val())) {
                            if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) {}
                            else foffset = uval;
                            if ((vs = get_val())) {
                                if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) {}
                                else if (uval && (address_t)uval - 1 > all_mem2) err_msg2(ERROR_CONSTNT_LARGE,NULL, &vs->epoint);
                                else fsize = uval;
                            }
                        }

                        if (val2) {
                            struct file_s *cfile2 = openfile(path, cfile->realname, 1, val2, &epoint);
                            if (cfile2) {
                                for (;fsize && foffset < cfile2->len;fsize--) {
                                    pokeb(cfile2->data[foffset]);foffset++;
                                }
                            }
                        }
                        free((char *)path);
                    }

                    if (!nolisting) {
                        list_mem(&current_section->mem, current_section->dooutput);
                    }
                }
                break;
            case CMD_OFFS: if (waitfor->skip & 1)
                {   /* .offs */
                    struct values_s *vs;
                    ival_t ival;
                    listing_line(epoint.pos);
                    if (!current_section->moved) {
                        if (current_section->end < current_section->address) current_section->end = current_section->address;
                        current_section->moved = 1;
                    }
                    current_section->wrapwarn = current_section->wrapwarn2 = 0;
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (toival(vs->val, &ival, 8*sizeof(ival_t), &vs->epoint)) break;
                    if (ival) {
                        if (current_section->structrecursion) {
                            if (ival < 0) err_msg2(ERROR___NOT_ALLOWED, ".OFFS", &epoint);
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
            case CMD_LOGICAL: if (waitfor->skip & 1)
                { /* .logical */
                    struct values_s *vs;
                    uval_t uval;
                    listing_line(epoint.pos);
                    new_waitfor(W_HERE2, &epoint);waitfor->laddr = current_section->l_address - current_section->address;waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    current_section->logicalrecursion++;
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".LOGICAL", &epoint);
                    else {
                        if (touval(vs->val, &uval, 24, &vs->epoint)) {}
                        else if ((uval_t)uval & ~(uval_t)all_mem) err_msg2(ERROR_ADDRESS_LARGE, NULL, &vs->epoint);
                        else current_section->l_address = (address_t)uval;
                    }
                    newlabel = NULL;
                } else new_waitfor(W_HERE, &epoint);
                break;
            case CMD_AS: /* .as */
            case CMD_AL: /* .al */
                if (waitfor->skip & 1) {
                    listing_line(epoint.pos);
                    longaccu = (prm == CMD_AL);
                }
                break;
            case CMD_XS: /* .xs */
            case CMD_XL: /* .xl */
                if (waitfor->skip & 1) {
                    listing_line(epoint.pos);
                    longindex = (prm == CMD_XL);
                }
                break;
            case CMD_BLOCK: if (waitfor->skip & 1)
                { /* .block */
                    listing_line(epoint.pos);
                    new_waitfor(W_BEND2, &epoint);
                    if (newlabel) {
                        current_context=newlabel;
                        waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;waitfor->cheap_label = oldcheap;
                        newlabel = NULL;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof(anonident2));
                        else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                        anonident2.type = '.';
                        anonident2.star_tree = star_tree;
                        anonident2.vline = vline;
                        tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof(anonident2);
                        current_context=new_label(&tmpname, mycontext, strength, &labelexists);
                        waitfor->cheap_label = cheap_context;
                        cheap_context = current_context;
                        if (!labelexists) {
                            current_context->constant = 1;
                            current_context->requires = 0;
                            current_context->conflicts = 0;
                            current_context->value = val_reference(none_value);
                            current_context->file_list = cflist;
                            current_context->epoint = epoint;
                        }
                    }
                } else new_waitfor(W_BEND, &epoint);
                break;
            case CMD_WEAK: if (waitfor->skip & 1)
                { /* .weak */
                    listing_line(epoint.pos);
                    new_waitfor(W_WEAK2, &epoint);waitfor->label=newlabel;waitfor->addr = current_section->address;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    strength++;
                    newlabel = NULL;
                } else new_waitfor(W_WEAK, &epoint);
                break;
            case CMD_DATABANK:
            case CMD_DPAGE:
            case CMD_EOR: if (waitfor->skip & 1)
                { /* .databank, .dpage, .eor */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    switch (prm) {
                    case CMD_DATABANK:
                        if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else databank = uval;
                        break;
                    case CMD_DPAGE:
                        if (touval(vs->val, &uval, 16, &vs->epoint)) {}
                        else dpage = uval;
                        break;
                    case CMD_EOR:
                        if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else outputeor = uval;
                        break;
                    default:
                        break;
                    }
                }
                break;
            case CMD_FILL:
            case CMD_ALIGN: if (waitfor->skip & 1)
                { /* .fill, .align */
                    address_t db = 0;
                    uval_t uval;
                    struct values_s *vs;
                    if (newlabel) {
                        newlabel->value->u.code.dtype = D_BYTE;
                    }
                    if (!get_exp(&w, 0, cfile, 1, 2, &epoint)) goto breakerr;
                    if (prm == CMD_ALIGN && current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".ALIGN", &epoint);
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) uval = (prm == CMD_ALIGN) ? 1 : 0;
                    if (prm == CMD_ALIGN) {
                        if (!uval) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs->epoint);
                        else if ((uval & ~(uval_t)all_mem)) err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);
                        else if (uval > 1 && current_section->l_address % uval) db = uval - (current_section->l_address % uval);
                    } else db = uval;
                    if (db && db - 1 > all_mem2) {err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);goto breakerr;}
                    mark_mem(&current_section->mem, current_section->address);
                    if ((vs = get_val())) {
                        val = vs->val;
                        if (val->obj == ERROR_OBJ) {err_msg_output(val); memskip(db);}
                        else if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); memskip(db);}
                        else {
                            value_t iter, val2;
                            size_t uninit = 0, sum = 0;
                            size_t memp, membp;
                            get_mem(&current_section->mem, &memp, &membp);

                            if (val->obj == STR_OBJ) {
                                value_t tmp = bytes_from_str(val, &vs->epoint);
                                iter = tmp->obj->getiter(tmp);
                                val_destroy(tmp);
                            } else iter = val->obj->getiter(val);

                            while (db && ((val2 = obj_next(iter)))) {
                                db--;
                                switch (val2->obj->type) {
                                case T_GAP:uninit++; break;
                                default:
                                           if (touval(val2, &uval, 8, &vs->epoint)) uval = 0;
                                           if (uninit) { memskip(uninit); sum += uninit; uninit = 0; }
                                           pokeb(uval); sum++;
                                           break;
                                case T_NONE:
                                           err_msg_still_none(NULL, &vs->epoint);
                                           uninit++;
                                }
                                val_destroy(val2);
                            }
                            val_destroy(iter);
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
                        }
                    } else memskip(db);
                    if (!nolisting) {
                        list_mem(&current_section->mem, current_section->dooutput);
                    }
                }
                break;
            case CMD_ASSERT: if (waitfor->skip & 1)
                { /* .assert */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 3, 3, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) current_section->provides=~(uval_t)0;
                    else current_section->provides = uval;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) current_section->requires = 0;
                    else current_section->requires = uval;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) current_section->conflicts = 0;
                    else current_section->conflicts = uval;
                }
                break;
            case CMD_CHECK: if (waitfor->skip & 1)
                { /* .check */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 2, 2, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) {}
                    else if ((uval & current_section->provides) ^ uval) err_msg_requires(NULL, &epoint);
                    vs = get_val();
                    if (touval(vs->val, &uval, 8*sizeof(uval_t), &vs->epoint)) {}
                    else if (uval & current_section->provides) err_msg_conflicts(NULL, &epoint);
                }
                break;
            case CMD_WARN:
            case CMD_CWARN:
            case CMD_ERROR:
            case CMD_CERROR: if (waitfor->skip & 1)
                { /* .warn .cwarn .error .cerror */
                    int writeit = 1;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, (prm == CMD_CWARN || prm == CMD_CERROR), 0, &epoint)) goto breakerr;
                    if (prm == CMD_CWARN || prm == CMD_CERROR) {
                        vs = get_val();
                        if (tobool(vs, &writeit)) writeit = 0;
                    }
                    if (writeit) {
                        size_t i, len = get_val_remaining(), len2 = 0;
                        value_t tmp, *vals, v;
                        uint8_t *s;
                        tmp = val_alloc(TUPLE_OBJ);
                        tmp->u.list.data = vals = list_create_elements(tmp, len);
                        for (i = 0; i < len; i++) {
                            vs = get_val(); val = vs->val;
                            if (val->obj == NONE_OBJ) val = val_reference(null_str);
                            else {
                                val = STR_OBJ->create(val, &vs->epoint);
                                if (val->obj != STR_OBJ) { err_msg_output_and_destroy(val); val = val_reference(null_str); }
                                else {
                                    len2 += val->u.str.len;
                                    if (len2 < val->u.str.len) err_msg_out_of_memory(); /* overflow */
                                }
                            }
                            vals[i] = val;
                        }
                        tmp->u.list.len = i;
                        v = val_alloc(STR_OBJ);
                        v->u.str.data = s = str_create_elements(v, len2);
                        len2 = 0;
                        for (i = 0; i < len; i++) {
                            val = vals[i];
                            memcpy(s + len2, val->u.str.data, val->u.str.len);
                            len2 += val->u.str.len;
                        }
                        v->u.str.len = len2;
                        v->u.str.chars = len2;
                        err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED, v, &epoint);
                        val_destroy(v);
                        val_destroy(tmp);
                    } else while (get_val());
                }
                break;
            case CMD_ENC: if (waitfor->skip & 1)
                { /* .enc */
                    str_t encname;
                    listing_line(epoint.pos);
                    epoint = lpoint;
                    encname.data = pline + lpoint.pos; encname.len = get_label();
                    if (!encname.len) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    actual_encoding = new_encoding(&encname);
                }
                break;
            case CMD_CDEF: if (waitfor->skip & 1)
                { /* .cdef */
                    struct trans_s tmp, *t;
                    struct encoding_s *old = actual_encoding;
                    uint32_t ch;
                    int rc;
                    size_t len;
                    listing_line(epoint.pos);
                    actual_encoding = NULL;
                    rc = get_exp(&w, 0, cfile, 2, 0, &epoint);
                    actual_encoding = old;
                    len = get_val_remaining();
                    if (!rc) goto breakerr;
                    for (;;) {
                        int endok = 0;
                        size_t i = 0;
                        int tryit = 1;
                        uval_t uval;
                        struct values_s *vs;

                        vs = get_val();
                        if (!vs) break;

                        val = vs->val;
                        switch (val->obj->type) {
                        case T_FLOAT:
                        case T_BOOL:
                        case T_INT:
                        case T_BITS:
                            if (touval(val, &uval, 24, &vs->epoint)) uval = 0;
                            tmp.start = uval;
                            break;
                        case T_STR:
                            if (!val->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);
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
                            if (val->u.str.len > i) err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);
                            break;
                        case T_NONE:
                            err_msg_still_none(NULL, &vs->epoint);
                            tryit = 0;
                            break;
                        case T_ERROR:
                            err_msg_output(val);
                            tryit = 0;
                            break;
                        default:
                            err_msg_wrong_type(val, NULL, &vs->epoint);
                            goto breakerr;
                        }
                        if (!endok) {
                            vs = get_val();
                            if (!vs) { err_msg_argnum(len, len + 2, 0, &epoint); goto breakerr; }
                            if (touval(vs->val, &uval, 24, &vs->epoint)) tryit = 0;
                            else {
                                if (tmp.start > (uint32_t)uval) {
                                    tmp.end = tmp.start;
                                    tmp.start = uval;
                                } else tmp.end = uval;
                            }
                        }
                        vs = get_val();
                        if (!vs) { err_msg_argnum(len, len + 1, 0, &epoint); goto breakerr;}
                        if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else if (tryit) {
                            tmp.offset = uval;
                            t = new_trans(&tmp, actual_encoding);
                            if (t->start != tmp.start || t->end != tmp.end || t->offset != tmp.offset) {
                                err_msg2(ERROR__DOUBLE_RANGE, NULL, &vs->epoint); goto breakerr;
                            }
                        }
                    }
                }
                break;
            case CMD_EDEF: if (waitfor->skip & 1)
                { /* .edef */
                    struct encoding_s *old = actual_encoding;
                    int rc;
                    size_t len;
                    listing_line(epoint.pos);
                    actual_encoding = NULL;
                    rc = get_exp(&w, 0, cfile, 2, 0, &epoint);
                    actual_encoding = old;
                    if (!rc) goto breakerr;
                    len = get_val_remaining();
                    for (;;) {
                        struct values_s *vs, *vs2;
                        value_t v;
                        int tryit = 1;

                        vs = get_val();
                        if (!vs) break;

                        v = vs->val;
                        switch (v->obj->type) {
                        case T_STR:
                            if (!v->u.str.len) err_msg2(ERROR_CONSTNT_LARGE, NULL, &vs->epoint);
                            break;
                        case T_NONE:
                            err_msg_still_none(NULL, &vs->epoint);
                            tryit = 0;
                            break;
                        case T_ERROR:
                            err_msg_output(v);
                            tryit = 0;
                            break;
                        default:
                            err_msg_wrong_type(v, STR_OBJ, &vs->epoint);
                            goto breakerr;
                        }
                        vs2 = get_val();
                        if (!vs2) { err_msg_argnum(len, len + 1, 0, &epoint); goto breakerr; }
                        val = vs2->val;
                        if (val->obj == ERROR_OBJ) err_msg_output(val);
                        else if (val->obj == NONE_OBJ) err_msg_still_none(NULL, &vs2->epoint);
                        else if (tryit && new_escape(v, val, actual_encoding, &vs2->epoint)) {
                            err_msg2(ERROR_DOUBLE_ESCAPE, NULL, &vs->epoint); goto breakerr;
                        }
                    }
                }
                break;
            case CMD_CPU: if (waitfor->skip & 1)
                { /* .cpu */
                    struct values_s *vs;
                    const struct cpu_list_s {
                        const char *name;
                        const struct cpu_s *def;
                    } *cpui;
                    static const struct cpu_list_s cpus[] = {
                        {"6502", &c6502}, {"65c02", &c65c02},
                        {"65ce02", &c65ce02}, {"6502i", &c6502i},
                        {"65816", &w65816}, {"65dtv02", &c65dtv02},
                        {"65el02", &c65el02}, {"r65c02", &r65c02},
                        {"w65c02", &w65c02}, {"default", NULL},
                        {NULL, NULL},
                    };
                    size_t len;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    switch (val->obj->type) {
                    case T_STR:
                        cpui = cpus;
                        len = val->u.str.len;
                        while (cpui->name) {
                            if (len == strlen(cpui->name) && !memcmp(cpui->name, val->u.str.data, len)) {
                                const struct cpu_s *cpumode = cpui->def ? cpui->def : arguments.cpumode;
                                if (current_section->l_address & ~(address_t)cpumode->max_address) {
                                    err_msg2(ERROR_ADDRESS_LARGE, NULL, &epoint);
                                    current_section->l_address &= cpumode->max_address;
                                }
                                if (arguments.output_mode != OUTPUT_FLAT) {
                                    if (current_section->address & ~cpumode->max_address) {
                                        current_section->address &= cpumode->max_address;
                                        memjmp(&current_section->mem, current_section->address);
                                    }
                                }
                                set_cpumode(cpumode);
                                break;
                            }
                            cpui++;
                        }
                        if (!cpui->name) err_msg2(ERROR___UNKNOWN_CPU, val, &vs->epoint);
                        break;
                    case T_NONE:
                        err_msg_still_none(NULL, &vs->epoint);
                        break;
                    case T_ERROR:
                        err_msg_output(val);
                        break;
                    default:
                        err_msg_wrong_type(val, STR_OBJ, &vs->epoint);
                        break;
                    }
                }
                break;
            case CMD_REPT: if (waitfor->skip & 1)
                { /* .rept */
                    uval_t cnt;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    new_waitfor(W_NEXT, &epoint);waitfor->skip=0;
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval(vs->val, &cnt, 8*sizeof(uval_t), &vs->epoint)) break;
                    if (cnt > 0) {
                        line_t lin = lpoint.line;
                        int labelexists;
                        struct star_s *s = new_star(vline, &labelexists);
                        struct avltree *stree_old = star_tree;
                        line_t ovline = vline, lvline;
                        const struct waitfor_s *my_waitfor;

                        close_waitfor(W_NEXT);
                        if (labelexists && s->addr != star) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                        s->addr = star;
                        star_tree = &s->tree;vline = 0;
                        waitfor->breakout = 0;
                        for (;;) {
                            lpoint.line=lin;
                            new_waitfor(W_NEXT2, &epoint);waitfor->skip = 1;lvline = vline;
                            my_waitfor = waitfor;
                            compile(cflist);
                            if (waitfor->breakout || !--cnt) {
                                break;
                            }
                            if (my_waitfor->skip & 1) listing_line_cut(my_waitfor->epoint.pos);
                        }
                        if (my_waitfor->skip & 1) listing_line(my_waitfor->epoint.pos);
                        else listing_line_cut2(my_waitfor->epoint.pos);
                        star_tree = stree_old; vline = ovline + vline - lvline;
                    }
                } else new_waitfor(W_NEXT, &epoint);
                break;
            case CMD_PRON: /* .pron */
                if (waitfor->skip & 1) {
                    listing_line(epoint.pos);
                    if (nolisting) nolisting--;
                }
                break;
            case CMD_PROFF: /* .proff */
                if (waitfor->skip & 1) {
                    nolisting++;
                    listing_line(epoint.pos);
                }
                break;
            case CMD_SHOWMAC: /* .showmac */
            case CMD_HIDEMAC: /* .hidemac */
                if (waitfor->skip & 1) {
                    listing_line(epoint.pos);
                    err_msg2(ERROR_DIRECTIVE_IGN, NULL, &epoint);
                }
                break;
            case CMD_COMMENT: /* .comment */
                if (waitfor->skip & 1) listing_line(epoint.pos);
                new_waitfor(W_ENDC, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_INCLUDE:
            case CMD_BINCLUDE: if (waitfor->skip & 1)
                { /* .include, .binclude */
                    struct file_s *f;
                    struct values_s *vs;
                    const char *path;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (val->obj == ERROR_OBJ) {err_msg_output(val); goto breakerr;}
                    if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); goto breakerr;}
                    if (val->obj != STR_OBJ) {err_msg_wrong_type(val, STR_OBJ, &vs->epoint);goto breakerr;}
                    path = get_path(val, cfile->realname);
                    if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);

                    f = openfile(path, cfile->realname, 0, val, &epoint);
                    free((char *)path);
                    if (f->open>1) {
                        err_msg2(ERROR_FILERECURSION, NULL, &epoint);
                    } else {
                        int starexists;
                        struct star_s *s = new_star(vline, &starexists);
                        struct avltree *stree_old = star_tree;
                        uint32_t old_backr = backr, old_forwr = forwr;
                        line_t lin = lpoint.line;
                        line_t vlin = vline;
                        struct file_list_s *cflist2;

                        listing_file("\n;******  Processing file: ", f->realname);

                        if (starexists && s->addr != star) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = 0;
                        }
                        s->addr = star;
                        cflist2 = enterfile(f, &epoint);
                        lpoint.line = vline = 0;
                        star_tree = &s->tree;
                        backr = forwr = 0;
                        reffile=f->uid;
                        if (prm == CMD_BINCLUDE) {
                            if (newlabel) {
                                current_context = newlabel;
                            } else {
                                int labelexists;
                                str_t tmpname;
                                if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof(anonident2));
                                else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                                anonident2.type = '.';
                                anonident2.star_tree = star_tree;
                                anonident2.vline = vline;
                                tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof(anonident2);
                                current_context=new_label(&tmpname, mycontext, strength, &labelexists);
                                if (!labelexists) {
                                    current_context->constant = 1;
                                    current_context->requires = 0;
                                    current_context->conflicts = 0;
                                    current_context->value = val_reference(none_value);
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

                    listing_file("\n;******  Return to file: ", cfile->realname);
                    goto breakerr;
                }
                break;
            case CMD_FOR: if (waitfor->skip & 1)
                { /* .for */
                    line_t lin, xlin;
                    struct linepos_s apoint, bpoint = {0, 0};
                    int nopos = -1;
                    uint8_t *expr;
                    struct label_s *var;
                    struct star_s *s;
                    struct avltree *stree_old;
                    int truth;
                    line_t ovline, lvline;
                    int starexists;
                    const struct waitfor_s *my_waitfor;

                    listing_line(epoint.pos);
                    new_waitfor(W_NEXT, &epoint);waitfor->skip=0;
                    { /* label */
                        int labelexists;
                        str_t varname;
                        epoint=lpoint;
                        varname.data = pline + lpoint.pos; varname.len = get_label();
                        if (varname.len) {
                            struct values_s *vs;
                            if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint); goto breakerr;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"=");goto breakerr;}
                            lpoint.pos++;
                            if (!get_exp(&w, 1, cfile, 1, 1, &lpoint)) goto breakerr;
                            vs = get_val(); val = vs->val;
                            if (val->obj == ERROR_OBJ) {err_msg_output(val); val = none_value;}
                            var = new_label(&varname, (varname.data[0] == '_') ? cheap_context : current_context, strength, &labelexists);
                            if (labelexists) {
                                if (var->constant) err_msg_double_defined(var, &varname, &epoint);
                                else {
                                    if (var->defpass != pass) var->ref = 0;
                                    var->requires=current_section->requires;
                                    var->conflicts=current_section->conflicts;
                                    var_assign(var, val, fixeddig);
                                }
                            } else {
                                var->constant = 0;
                                var->requires = current_section->requires;
                                var->conflicts = current_section->conflicts;
                                var->value = val_reference(val);
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
                        struct linepos_s nopoint = {0, 0};
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &nopoint);
                        fixeddig = 0;
                    }
                    s->addr = star;
                    star_tree = &s->tree;lvline = vline = 0;
                    xlin=lin=lpoint.line; apoint = lpoint;
                    expr = (uint8_t *)malloc(strlen((char *)pline) + 1);
                    if (!expr) err_msg_out_of_memory();
                    strcpy((char *)expr, (char *)pline); var = NULL;
                    waitfor->breakout = 0;
                    my_waitfor = NULL;
                    for (;;) {
                        struct values_s *vs;
                        lpoint=apoint;
                        if (!get_exp(&w, 1, cfile, 1, 1, &apoint)) break;
                        vs = get_val();
                        if (tobool(vs, &truth)) break;
                        if (!truth) break;
                        if (nopos < 0) {
                            str_t varname;
                            ignore();if (here()!=',') {err_msg(ERROR______EXPECTED,","); break;}
                            lpoint.pos++;ignore();
                            epoint = lpoint;
                            varname.data = pline + lpoint.pos; varname.len = get_label();
                            if (!varname.len) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);break;}
                            if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint); goto breakerr;}
                            ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); break;}
                            lpoint.pos++;ignore();
                            if (!here() || here()==';') {bpoint.pos = 0; nopos = 0;}
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
                                    var->value = val_reference(none_value);
                                    var->file_list = cflist;
                                    var->epoint = epoint;
                                }
                                bpoint=lpoint; nopos = 1;
                            }
                        }
                        if (my_waitfor && (my_waitfor->skip & 1)) listing_line_cut(my_waitfor->epoint.pos);
                        new_waitfor(W_NEXT2, &epoint);waitfor->skip=1;lvline = vline;
                        my_waitfor = waitfor;
                        compile(cflist);
                        xlin= lpoint.line;
                        pline = expr;
                        lpoint.line=lin;
                        if (waitfor->breakout) break;
                        if (nopos > 0) {
                            struct linepos_s epoints[3];
                            lpoint = bpoint;
                            if (!get_exp(&w, 0, cfile, 0, 0, &bpoint)) break;
                            val = get_vals_addrlist(epoints);
                            var_assign(var, val, fixeddig);
                            val_destroy(val);
                        }
                    }
                    if (my_waitfor) {
                        if (my_waitfor->skip & 1) listing_line(my_waitfor->epoint.pos);
                        else listing_line_cut2(my_waitfor->epoint.pos);
                    }
                    free(expr);
                    if (lin!=xlin) close_waitfor(W_NEXT);
                    lpoint.line=xlin;
                    star_tree = stree_old; vline = ovline + vline - lvline;
                    goto breakerr;
                } else new_waitfor(W_NEXT, &epoint);
                break;
            case CMD_CONTINUE:
            case CMD_BREAK: if (waitfor->skip & 1)
                { /* .continue, .break */
                    size_t wp = waitfor_p + 1;
                    int nok = 1;
                    listing_line(epoint.pos);
                    while (wp--) {
                        if (waitfors[wp].what == W_NEXT2) {
                            if (wp && prm == CMD_BREAK) waitfors[wp - 1].breakout = 1;
                            for (;wp <= waitfor_p; wp++) waitfors[wp].skip = 0;
                            nok = 0;
                            break;
                        }
                    }
                    if (nok) err_msg2(ERROR______EXPECTED,".FOR or .REPT", &epoint);
                }
                break;
            case CMD_PAGE: if (waitfor->skip & 1)
                { /* .page */
                    listing_line(epoint.pos);
                    new_waitfor(W_ENDP2, &epoint);waitfor->addr = current_section->address;waitfor->laddr = current_section->l_address;waitfor->label=newlabel;waitfor->memp = newmemp;waitfor->membp = newmembp;
                    newlabel=NULL;
                } else new_waitfor(W_ENDP, &epoint);
                break;
            case CMD_OPTION: if (waitfor->skip & 1)
                { /* .option */
                    static const str_t branch_across = {24, (const uint8_t *)"allow_branch_across_page"};
                    static const str_t longjmp = {22, (const uint8_t *)"auto_longbranch_as_jmp"};
                    struct values_s *vs;
                    str_t optname, cf;
                    listing_line(epoint.pos);
                    optname.data = pline + lpoint.pos; optname.len = get_label();
                    if (!optname.len) { err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    ignore();if (here()!='=') {err_msg(ERROR______EXPECTED,"="); goto breakerr;}
                    epoint = lpoint;
                    lpoint.pos++;
                    if (!get_exp(&w, 0, cfile, 1, 0, &epoint)) goto breakerr;
                    vs = get_val();
                    str_cfcpy(&cf, &optname);
                    if (!str_cmp(&cf, &branch_across)) {
                        if (tobool(vs, &allowslowbranch)) break;
                    } else if (!str_cmp(&cf, &longjmp)) {
                        if (tobool(vs, &longbranchasjmp)) break;
                    } else err_msg2(ERROR_UNKNOWN_OPTIO, &optname, &epoint);
                }
                break;
            case CMD_GOTO: if (waitfor->skip & 1)
                { /* .goto */
                    int noerr = 1;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(&w, 0, cfile, 1, 1, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (val->obj == ERROR_OBJ) {err_msg_output(val); break; }
                    if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); break; }
                    if (val->obj != LBL_OBJ) {err_msg_wrong_type(val, LBL_OBJ, &vs->epoint); break;}
                    if (val->u.lbl.file_list == cflist && val->u.lbl.parent == current_context) {
                        while (val->u.lbl.waitforp < waitfor_p) {
                            const char *msg = NULL;
                            switch (waitfor->what) {
                            case W_SWITCH2:
                            case W_SWITCH: msg = ".ENDSWITCH"; break;
                            case W_WEAK2:
                            case W_WEAK: msg = ".ENDWEAK"; break;
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
                        }
                    } else err_msg_not_defined(NULL, &vs->epoint);
                }
                break;
            case CMD_MACRO:
            case CMD_SEGMENT: /* .macro, .segment */
                if (waitfor->skip & 1) {
                    listing_line(0);
                    err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_ENDM, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_FUNCTION: /* .function */
                if (waitfor->skip & 1) {
                    listing_line(0);
                    err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_ENDF, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_VAR: /* .var */
                if (waitfor->skip & 1) {
                    listing_line(0);
                    err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    goto breakerr;
                }
                break;
            case CMD_LBL: /* .lbl */
                if (waitfor->skip & 1) {
                    listing_line(0);
                    err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                break;
            case CMD_PROC: /* .proc */
                if (waitfor->skip & 1) {
                    listing_line(0);
                    err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_PEND, &epoint);
                waitfor->skip = 0;waitfor->label = NULL;waitfor->cheap_label = NULL;
                break;
            case CMD_STRUCT: if (waitfor->skip & 1)
                { /* .struct */
                    int old_unionmode = current_section->unionmode;
                    listing_line(0);
                    current_section->unionmode = 0;
                    new_waitfor(W_ENDS, &epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDS2;waitfor->skip=1;
                        compile(cflist);
                    } else err_msg2(ERROR__MACRECURSION, NULL, &epoint);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                } else new_waitfor(W_ENDS, &epoint);
                break;
            case CMD_UNION: if (waitfor->skip & 1)
                { /* .union */
                    int old_unionmode = current_section->unionmode;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    address_t old_l_unionstart = current_section->l_unionstart, old_l_unionend = current_section->l_unionend;
                    listing_line(0);
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    current_section->l_unionstart = current_section->l_unionend = current_section->l_address;
                    new_waitfor(W_ENDU, &epoint);waitfor->skip=0;
                    current_section->structrecursion++;
                    if (here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                    if (current_section->structrecursion<100) {
                        waitfor->what = W_ENDU2;waitfor->skip=1;
                        compile(cflist);
                    } else err_msg2(ERROR__MACRECURSION, NULL, &epoint);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                    current_section->l_unionstart = old_l_unionstart; current_section->l_unionend = old_l_unionend;
                } else new_waitfor(W_ENDU, &epoint);
                break;
            case CMD_DSTRUCT: if (waitfor->skip & 1)
                { /* .dstruct */
                    int old_unionmode = current_section->unionmode;
                    struct values_s *vs;
                    listing_line(0);
                    current_section->unionmode = 0;
                    if (!get_exp(&w, 1, cfile, 1, 0, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (val->obj == ERROR_OBJ) {err_msg_output(val); goto breakerr; }
                    if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); goto breakerr; }
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->obj != STRUCT_OBJ) {err_msg_wrong_type(val, STRUCT_OBJ, &vs->epoint); goto breakerr;}
                    current_section->structrecursion++;
                    val = macro_recurse(W_ENDS2, val, current_context, &epoint);
                    if (val) val_destroy(val);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                }
                break;
            case CMD_DUNION: if (waitfor->skip & 1)
                { /* .dunion */
                    int old_unionmode = current_section->unionmode;
                    struct values_s *vs;
                    address_t old_unionstart = current_section->unionstart, old_unionend = current_section->unionend;
                    listing_line(0);
                    current_section->unionmode = 1;
                    current_section->unionstart = current_section->unionend = current_section->address;
                    if (!get_exp(&w, 1, cfile, 1, 0, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (val->obj == ERROR_OBJ) {err_msg_output(val); goto breakerr; }
                    if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); goto breakerr; }
                    ignore();if (here() == ',') lpoint.pos++;
                    if (val->obj != UNION_OBJ) {err_msg_wrong_type(val, UNION_OBJ, &vs->epoint); goto breakerr;}
                    current_section->structrecursion++;
                    val = macro_recurse(W_ENDU2, val, current_context, &epoint);
                    if (val) val_destroy(val);
                    current_section->structrecursion--;
                    current_section->unionmode = old_unionmode;
                    current_section->unionstart = old_unionstart; current_section->unionend = old_unionend;
                }
                break;
            case CMD_DSECTION: if (waitfor->skip & 1)
                { /* .dsection */
                    struct section_s *tmp3;
                    str_t sectionname;
                    listing_line(epoint.pos);
                    if (current_section->structrecursion && !current_section->dooutput) err_msg2(ERROR___NOT_ALLOWED, ".DSECTION", &epoint);
                    epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
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
                            tmp3->wrapwarn = tmp3->wrapwarn2 = tmp3->moved = 0;
                            tmp3->end = tmp3->start = tmp3->restart = tmp3->address = current_section->address;
                            tmp3->l_restart = tmp3->l_address = current_section->l_address;
                            tmp3->usepass = pass;
                            restart_memblocks(&tmp3->mem, tmp3->address);
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
                            t = tmp3->size;
                            if (t < tmp3->end - tmp3->start) t = tmp3->end - tmp3->start;
                            if (!tmp3->moved) {
                                if (t < tmp3->address - tmp3->start) t = tmp3->address - tmp3->start;
                            }
                            tmp3->restart = current_section->address;
                            if (tmp3->l_restart != current_section->l_address) {
                                tmp3->l_restart = current_section->l_address;
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = 0;
                            }
                        } else {
                            if (!tmp3->moved) {
                                if (tmp3->end < tmp3->address) tmp3->end = tmp3->address;
                                tmp3->moved=1;
                            }
                            tmp3->wrapwarn = tmp3->wrapwarn2 = 0;
                            t = tmp3->end - tmp3->start;
                            tmp3->end = tmp3->start = tmp3->restart = tmp3->address = current_section->address;
                            tmp3->l_restart = tmp3->l_address = current_section->l_address;
                            tmp3->size = t;
                            restart_memblocks(&tmp3->mem, tmp3->address);
                        }
                        tmp3->usepass = pass;
                        tmp3->defpass = pass;
                        memskip(t);
                        memref(&current_section->mem, &tmp3->mem);
                    }
                }
                break;
            case CMD_SECTION: if (waitfor->skip & 1)
                { /* .section */
                    struct section_s *tmp;
                    str_t sectionname;
                    listing_line(0);
                    new_waitfor(W_SEND, &epoint);waitfor->section=current_section;
                    epoint=lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (!sectionname.len) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    tmp=find_new_section(&sectionname);
                    if (!tmp->usepass || tmp->defpass < pass - 1) {
                        if (tmp->usepass && tmp->usepass >= pass - 1) {err_msg_not_defined(&sectionname, &epoint); goto breakerr;}
                        tmp->end = tmp->start = tmp->restart = tmp->address = 0;
                        tmp->size = tmp->l_restart = tmp->l_address = 0;
                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(&sectionname, &epoint);
                        fixeddig = 0;
                        tmp->defpass = pass - 1;
                        restart_memblocks(&tmp->mem, tmp->address);
                    } else if (tmp->usepass != pass) {
                        if (!tmp->moved) {
                            if (tmp->end < tmp->address) tmp->end = tmp->address;
                            tmp->moved=1;
                        }
                        tmp->size = tmp->end - tmp->start;
                        tmp->end = tmp->start = tmp->restart;
                        tmp->wrapwarn = tmp->wrapwarn2 = 0;
                        tmp->address = tmp->restart;
                        tmp->l_address = tmp->l_restart;
                        restart_memblocks(&tmp->mem, tmp->address);
                    }
                    tmp->usepass = pass;
                    waitfor->what = W_SEND2;
                    current_section = tmp;
                    newlabel = NULL;
                } else new_waitfor(W_SEND, &epoint);
                break;
            case sizeof(command)/sizeof(command[0]):
                if (waitfor->skip & 1) goto as_macro2;
                break;
            default:
                if (waitfor->skip & 1) {
                    listing_line(epoint.pos);
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    goto breakerr;
                }
                break;
            }
            break;
        case '#':if (waitfor->skip & 1) /* skip things if needed */
            {                   /* macro stuff */
                struct values_s *vs;
                lpoint.pos++;
            as_macro2:
                if (!get_exp_var(cfile, &epoint)) goto breakerr;
                vs = get_val(); val = vs->val;
                if (val->obj == ERROR_OBJ) {err_msg_output(val); goto breakerr; }
                if (val->obj == NONE_OBJ) {err_msg_still_none(NULL, &vs->epoint); goto breakerr; }
                if (val->obj != MACRO_OBJ && val->obj != SEGMENT_OBJ && val->obj != MFUNC_OBJ) {err_msg_wrong_type(val, NULL, &vs->epoint); goto breakerr;}
            as_macro:
                listing_line_cut(epoint.pos);
                if (val->obj == MACRO_OBJ) {
                    struct label_s *context;
                    if (newlabel) {
                        context=newlabel;
                    } else {
                        int labelexists;
                        str_t tmpname;
                        if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof(anonident2));
                        else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                        anonident2.type = '#';
                        anonident2.star_tree = star_tree;
                        anonident2.vline = vline;
                        tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof(anonident2);
                        context=new_label(&tmpname, mycontext, strength, &labelexists);
                        if (!labelexists) {
                            context->constant = 1;
                            context->requires = 0;
                            context->conflicts = 0;
                            context->value = val_reference(none_value);
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
                    value_t mfunc;
                    int labelexists;
                    str_t tmpname;
                    if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof(anonident2));
                    else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                    anonident2.type = '#';
                    anonident2.star_tree = star_tree;
                    anonident2.vline = vline;
                    tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof(anonident2);
                    context=new_label(&tmpname, val->u.mfunc.label->parent, strength, &labelexists);
                    if (!labelexists) {
                        context->constant = 1;
                        context->requires = 0;
                        context->conflicts = 0;
                        context->value = val_reference(none_value);
                        context->file_list = cflist;
                        context->epoint = epoint;
                    }
                    mfunc = val_reference(val);
                    if (!get_exp(&w, 4, cfile, 0, 0, NULL)) {
                        val = NULL;
                        val_destroy(mfunc);
                        goto breakerr;
                    }
                    val = mfunc_recurse(W_ENDF2, mfunc, context, &epoint, strength);
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
        default:
            if (waitfor->skip & 1) {
                str_t opname;

                opname.data = pline + lpoint.pos; opname.len = get_label();
                if (opname.len == 3 && (prm=lookup_opcode((const char *)opname.data))>=0) {
                    value_t err;
                    struct linepos_s oldlpoint;
                    struct linepos_s epoints[3];
                    if (0) {
                as_opcode:
                        opname = labelname;
                    }
                    ignore();
                    oldlpoint = lpoint;
                    if (!here() || here() == ';') {val = val_reference(null_addrlist); w = 3;}
                    else {
                        if (!get_exp(&w, 3, cfile, 0, 0, NULL)) goto breakerr;
                        val = get_vals_addrlist(epoints);
                    }
                    if (val->obj == TUPLE_OBJ || val->obj == LIST_OBJ) {
                        epoints[1] = epoints[0];
                        epoints[2] = epoints[0];
                        instrecursion(val, prm, w, &epoint, epoints);
                        val_destroy(val);
                        break;
                    }
                    err = instruction(prm, w, val, &epoint, epoints);
                    val_destroy(val);
                    if (err == NULL) break;
                    tmp2 = find_label(&opname);
                    if (tmp2) {
                        obj_t obj = tmp2->value->obj;
                        if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                            val_destroy(err);
                            tmp2->shadowcheck = 1;
                            lpoint=oldlpoint;
                            val = tmp2->value;
                            goto as_macro;
                        }
                    }
                    err_msg_output_and_destroy(err);
                    break;
                }
                tmp2 = find_label(&opname);
                if (tmp2) {
                    obj_t obj = tmp2->value->obj;
                    if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                        tmp2->shadowcheck = 1;
                        val = tmp2->value;goto as_macro;
                    }
                }
                err_msg2(ERROR_GENERL_SYNTAX, NULL, &epoint);
                goto breakerr;
            }
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
        case W_WEAK2:
        case W_WEAK: msg = ".ENDWEAK"; break;
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

static int main2(int argc, char *argv[]) {
    int opts, i;
    struct file_s *fin, *cfile;
    struct file_list_s *cflist;
    static const str_t none_enc = {4, (const uint8_t *)"none"};
    struct linepos_s nopoint = {0, 0};

    tinit();

    fin = openfile(NULL, "", 0, NULL, &nopoint);
    opts = testarg(argc,argv, fin);
    if (opts <= 0) {
        tfree();
        free_macro();
        free(waitfors);
        return -opts;
    }
    init_encoding(arguments.toascii);
    init_defaultlabels();

    if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1]))
        puts("64tass Turbo Assembler Macro V" VERSION "\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");

    /* assemble the input file(s) */
    do {
        if (pass++>max_pass) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        fixeddig=1;constcreated=0;error_reset();
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
            /*	nolisting=0;flist=stderr;*/
            if (i == opts - 1) {
                cflist = enterfile(fin, &nopoint);
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(cflist);
                exitfile();
                restart_memblocks(&root_section.mem, 0);
                continue;
            }
            cfile = openfile(argv[i], "", 0, NULL, &nopoint);
            cflist = enterfile(cfile, &nopoint);
            if (cfile) {
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cflist);
                closefile(cfile);
            }
            exitfile();
        }
        if (fixeddig && !constcreated) shadow_check(&root_label->members);
        if (error_serious(fixeddig, constcreated)) {status(1);return 1;}
    } while (!fixeddig || constcreated);

    /* assemble again to create listing */
    if (arguments.list) {
        nolisting = 0;
        listing_open(arguments.list, argc, argv);

        max_pass = pass; pass++;
        fixeddig=1;constcreated=0;error_reset();
        restart_memblocks(&root_section.mem, 0);
        for (i = opts - 1; i<argc; i++) {
            if (i >= opts) {
                listing_file("\n;******  Processing input file: ", argv[i]);
            }
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
                star_tree = &fin->star;
                reffile=fin->uid;
                compile(cflist);
                exitfile();
                restart_memblocks(&root_section.mem, 0);
                continue;
            }

            cfile = openfile(argv[i], "", 0, NULL, &nopoint);
            cflist = enterfile(cfile, &nopoint);
            if (cfile) {
                star_tree = &cfile->star;
                reffile=cfile->uid;
                compile(cflist);
                closefile(cfile);
            }
            exitfile();
        }
        listing_close();
    }

    set_cpumode(arguments.cpumode);

    if (arguments.label) labelprint();

    fixeddig = 1; constcreated = 0;
    if (error_serious(fixeddig, constcreated)) {status(1);return 1;}

    output_mem(&root_section.mem);
    status(0);
    return 0;
}

#ifdef _WIN32
static UINT oldcodepage;
static UINT oldcodepage2;

void myexit(void) {
    SetConsoleCP(oldcodepage2);
    SetConsoleOutputCP(oldcodepage);
}

int wmain(int argc, wchar_t *argv2[]) {
    int i, r;

    if (IsValidCodePage(CP_UTF8)) {
        oldcodepage = GetConsoleOutputCP();
        oldcodepage2 = GetConsoleCP();
        SetConsoleCP(CP_UTF8);
        SetConsoleOutputCP(CP_UTF8);
        atexit(myexit);
    }

    char **argv = (char **)malloc(sizeof(char *)*argc);
    for (i = 0; i < argc; i++) {
	uint32_t c = 0, lastchar;
	wchar_t *p = argv2[i];
	uint8_t *c2;

	while (*p) p++;
	c2 = (uint8_t *)malloc((p - argv2[i])*4/sizeof(wchar_t)+1);
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
	argv[i] = (char *)realloc(argv[i], (char *)c2 - argv[i]);
	if (!argv[i]) exit(1);
    }
    r = main2(argc, argv);

    for (i = 0; i < argc; i++) free(argv[i]);
    free(argv);
    return r;
}
#else
int main(int argc, char *argv[]) {
    int i, r;
    char **uargv;

    setlocale(LC_ALL, "");
    setlocale(LC_NUMERIC, "C");

    uargv = (char **)malloc(sizeof(char *)*argc);
    for (i = 0; i < argc; i++) {
        const char *s = argv[i];
        mbstate_t ps;
        uint8_t *p;
        size_t n = strlen(s), j = 0;
        size_t len = n + 64;
        uint8_t *data = (uint8_t *)malloc(len);
        if (!data || len < 64) err_msg_out_of_memory();

        memset(&ps, 0, sizeof(ps));
        p = data;
        for (;;) {
            ssize_t l;
            wchar_t w;
            uint32_t ch;
            if (p + 6*6 + 1 > data + len) {
                size_t o = p - data;
                len += 1024;
                data=(uint8_t*)realloc(data, len);
                if (!data) err_msg_out_of_memory();
                p = data + o;
            }
            l = mbrtowc(&w, s + j, n - j,  &ps);
            if (l < 1) break;
            j += l;
            ch = w;
            if (ch && ch < 0x80) *p++ = ch; else p = utf8out(ch, p);
        }
        *p++ = 0;
        uargv[i] = (char *)data;
    }
    r = main2(argc, uargv);

    for (i = 0; i < argc; i++) free(uargv[i]);
    free(uargv);
    return r;
}
#endif


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
