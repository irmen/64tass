/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: 64tass.c 3124 2023-09-17 09:29:06Z soci $

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

#include "64tass.h"
#include <string.h>

#include "error.h"
#include "opcodes.h"
#include "eval.h"
#include "values.h"
#include "section.h"
#include "encoding.h"
#include "file.h"
#include "variables.h"
#include "macro.h"
#include "instruction.h"
#include "unicode.h"
#include "listing.h"
#include "optimizer.h"
#include "arguments.h"
#include "ternary.h"
#include "opt_bit.h"
#include "longjump.h"
#include "mem.h"
#include "unicodedata.h"
#include "main.h"
#include "argvalues.h"
#include "version.h"

#include "listobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "addressobj.h"
#include "boolobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "functionobj.h"
#include "namespaceobj.h"
#include "operobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "registerobj.h"
#include "labelobj.h"
#include "errorobj.h"
#include "macroobj.h"
#include "mfuncobj.h"
#include "memblocksobj.h"
#include "symbolobj.h"
#include "anonsymbolobj.h"
#include "dictobj.h"
#include "encobj.h"

linenum_t vline;      /* current line */
address_t all_mem, all_mem2;
unsigned int all_mem_bits;
uint8_t pass = 0, max_pass = MAX_PASS;         /* pass */
address_t star = 0;
const uint8_t *pline;           /* current line data */
struct linepos_s lpoint;        /* position in current line */
static uint8_t strength = 0;
bool fixeddig, constcreated;
uint32_t outputeor = 0; /* EOR value for final output (usually 0, unless changed by .eor) */
bool referenceit = true;
const struct cpu_s *current_cpu;
static unsigned int err_msg_char_note_once;

static size_t waitfor_p, waitfor_len;
static struct waitfor_s {
    Wait_types what;
    uint8_t skip;
    struct linepos_s epoint;
    union {
        struct {
            address_t laddr;
            Obj *val;
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_logical;
        struct {
            struct section_address_s *section_address;
            Label *label;
            size_t membp;
        } cmd_virtual;
        struct {
            struct section_s *section;
            struct section_address_s *section_address;
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_section;
        struct {
            bool unionmode;
            address_t laddr;
            address_t addr, addr2;
        } cmd_union;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_with;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_weak;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
            address_t laddr;
            ival_t size;
            uval_t offset;
        } cmd_page;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_proc;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_block;
        struct {
            bool unionmode;
        } cmd_struct;
        struct {
            bool breakout;
        } cmd_rept;
        struct {
            Obj *val;
        } cmd_macro;
        struct {
            Obj *val;
        } cmd_function;
        struct {
            Obj *val;
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_switch;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_if;
        struct {
            Enc *enc;
        } cmd_encode;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
            Label *label2;
            address_t laddr;
            ival_t size;
            uval_t offset;
        } cmd_alignblk;
    } u;
} *waitfors, *waitfor;

struct star_s *star_tree = NULL;

static const char *const command[] = { /* must be sorted, first char is the ID */
    "\x08" "addr",
    "\x22" "al",
    "\x34" "align",
    "\x72" "alignblk",
    "\x75" "alignind",
    "\x74" "alignpageind",
    "\x21" "as",
    "\x35" "assert",
    "\x5b" "autsiz",
    "\x3a" "bend",
    "\x63" "bfor",
    "\x1a" "binary",
    "\x50" "binclude",
    "\x39" "block",
    "\x5a" "break",
    "\x66" "breakif",
    "\x62" "brept",
    "\x65" "bwhile",
    "\x05" "byte",
    "\x54" "case",
    "\x4e" "cdef",
    "\x32" "cerror",
    "\x06" "char",
    "\x36" "check",
    "\x1b" "comment",
    "\x59" "continue",
    "\x67" "continueif",
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
    "\x17" "elif",
    "\x15" "else",
    "\x17" "elsif",
    "\x2c" "enc",
    "\x6f" "encode",
    "\x3f" "end",
    "\x73" "endalignblk",
    "\x3a" "endblock",
    "\x1c" "endc",
    "\x1c" "endcomment",
    "\x70" "endencode",
    "\x52" "endf",
    "\x6c" "endfor",
    "\x52" "endfunction",
    "\x16" "endif",
    "\x20" "endlogical",
    "\x11" "endm",
    "\x6a" "endmacro",
    "\x5f" "endn",
    "\x5f" "endnamespace",
    "\x1e" "endp",
    "\x1e" "endpage",
    "\x27" "endproc",
    "\x6d" "endrept",
    "\x46" "ends",
    "\x4d" "endsection",
    "\x6b" "endsegment",
    "\x46" "endstruct",
    "\x56" "endswitch",
    "\x49" "endu",
    "\x49" "endunion",
    "\x61" "endv",
    "\x61" "endvirtual",
    "\x58" "endweak",
    "\x6e" "endwhile",
    "\x69" "endwith",
    "\x40" "eor",
    "\x25" "error",
    "\x16" "fi",
    "\x2a" "fill",
    "\x12" "for",
    "\x76" "from",
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
    "\x43" "lbl",
    "\x0b" "lint",
    "\x1f" "logical",
    "\x0c" "long",
    "\x10" "macro",
    "\x5c" "mansiz",
    "\x5e" "namespace",
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
    "\x5d" "seed",
    "\x41" "segment",
    "\x4d" "send",
    "\x2d" "sfunction",
    "\x02" "shift",
    "\x03" "shiftl",
    "\x3d" "showmac",
    "\x09" "sint",
    "\x45" "struct",
    "\x53" "switch",
    "\x71" "tdef",
    "\x00" "text",
    "\x48" "union",
    "\x42" "var",
    "\x60" "virtual",
    "\x2b" "warn",
    "\x57" "weak",
    "\x64" "while",
    "\x68" "with",
    "\x0a" "word",
    "\x24" "xl",
    "\x23" "xs",
};

typedef enum Command_types {
    CMD_TEXT = 0, CMD_PTEXT, CMD_SHIFT, CMD_SHIFTL, CMD_NULL, CMD_BYTE, CMD_CHAR,
    CMD_RTA, CMD_ADDR, CMD_SINT, CMD_WORD, CMD_LINT, CMD_LONG, CMD_DINT,
    CMD_DWORD, CMD_OFFS, CMD_MACRO, CMD_ENDM, CMD_FOR, CMD_NEXT, CMD_IF,
    CMD_ELSE, CMD_FI, CMD_ELSIF, CMD_REPT, CMD_INCLUDE, CMD_BINARY,
    CMD_COMMENT, CMD_ENDC, CMD_PAGE, CMD_ENDP, CMD_LOGICAL, CMD_HERE, CMD_AS,
    CMD_AL, CMD_XS, CMD_XL, CMD_ERROR, CMD_PROC, CMD_PEND, CMD_DATABANK,
    CMD_DPAGE, CMD_FILL, CMD_WARN, CMD_ENC, CMD_SFUNCTION, CMD_IFNE, CMD_IFEQ,
    CMD_IFPL, CMD_IFMI, CMD_CERROR, CMD_CWARN, CMD_ALIGN, CMD_ASSERT,
    CMD_CHECK, CMD_CPU, CMD_OPTION, CMD_BLOCK, CMD_BEND, CMD_PRON, CMD_PROFF,
    CMD_SHOWMAC, CMD_HIDEMAC, CMD_END, CMD_EOR, CMD_SEGMENT, CMD_VAR, CMD_LBL,
    CMD_GOTO, CMD_STRUCT, CMD_ENDS, CMD_DSTRUCT, CMD_UNION, CMD_ENDU,
    CMD_DUNION, CMD_SECTION, CMD_DSECTION, CMD_SEND, CMD_CDEF, CMD_EDEF,
    CMD_BINCLUDE, CMD_FUNCTION, CMD_ENDF, CMD_SWITCH, CMD_CASE, CMD_DEFAULT,
    CMD_ENDSWITCH, CMD_WEAK, CMD_ENDWEAK, CMD_CONTINUE, CMD_BREAK, CMD_AUTSIZ,
    CMD_MANSIZ, CMD_SEED, CMD_NAMESPACE, CMD_ENDN, CMD_VIRTUAL, CMD_ENDV,
    CMD_BREPT, CMD_BFOR, CMD_WHILE, CMD_BWHILE, CMD_BREAKIF, CMD_CONTINUEIF,
    CMD_WITH, CMD_ENDWITH, CMD_ENDMACRO, CMD_ENDSEGMENT, CMD_ENDFOR,
    CMD_ENDREPT, CMD_ENDWHILE, CMD_ENCODE, CMD_ENDENCODE, CMD_TDEF,
    CMD_ALIGNBLK, CMD_ENDALIGNBLK, CMD_ALIGNPAGEIND, CMD_ALIGNIND, CMD_FROM
} Command_types;

/* --------------------------------------------------------------------------- */
static void compile_init(const char *name) {
    err_init(name);
    init_type();
    objects_init();
    init_section();
    init_file();
    init_variables();
    init_eval();
    init_ternary();
    init_opt_bit();
    waitfors = NULL;
    waitfor_p = 0;
    waitfor_len = 0;
    pass = 0;
    err_msg_char_note_once = 0;
    max_pass = MAX_PASS;
}

static void compile_destroy(void) {
    destroy_lastlb();
    destroy_eval();
    destroy_argvalues();
    destroy_variables();
    destroy_section();
    destroy_longjump();
    destroy_encoding();
    destroy_values();
    destroy_transs();
    err_destroy();
    destroy_file();
    destroy_ternary();
    destroy_opt_bit();
    destroy_arguments();
    if (unfc(NULL)) {}
    if (unfkc(NULL, NULL, 0)) {}
    str_cfcpy(NULL, NULL);
    free_macro();
    free(waitfors);
}

void new_waitfor(Wait_types what, linepos_t epoint) {
    uint8_t skwait = waitfor->skip;
    waitfor_p++;
    if (waitfor_p >= waitfor_len) extend_array(&waitfors, &waitfor_len, 8);
    waitfor = &waitfors[waitfor_p];
    waitfor->what = what;
    waitfor->skip = skwait;
    waitfor->epoint = *epoint;
}

static void reset_waitfor(void) {
    struct waitfor_s dummy;
    struct linepos_s lpos = {0, 0};
    dummy.skip = 1;
    waitfor_p = (size_t)0 - 1U;
    waitfor = &dummy;
    new_waitfor(W_NONE, &lpos);
}

bool close_waitfor(Wait_types what) {
    if (waitfor->what == what) {
        waitfor_p--;
        waitfor = &waitfors[waitfor_p];
        return true;
    }
    return false;
}

static void set_size(const Label *label, address_t size, Memblocks *mem, address_t oaddr, size_t membp) {
    Code *code = Code(label->value);
    if (code->v.obj != CODE_OBJ) return;
    size &= all_mem2;
    if (code->size != size) {
        code->size = size;
        if (code->pass != 0) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
            fixeddig = false;
        }
    }
    code->pass = pass;
    if (code->memblocks != mem) {
        val_destroy(Obj(code->memblocks));
        code->memblocks = ref_memblocks(mem);
    }
    code->memaddr = oaddr;
    code->membp = membp;
}

static address_t alignblk_set_size(Label *label) {
    Alignblk *alignblk = Alignblk(label->value);
    address_t size;
    if (alignblk->v.obj != ALIGNBLK_OBJ) return 0;
    size = (current_address->address - alignblk->addr) & all_mem2;
    if (alignblk->size != size) {
        alignblk->size = size;
        if (alignblk->pass != 0) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
            fixeddig = false;
        }
    }
    alignblk->pass = pass;
    val_destroy(Obj(label));
    return size;
}

static bool tobool(const struct values_s *v1, bool *truth) {
    Obj *val = v1->val, *err;
    const Type *obj = val->obj;
    bool error;
    if (obj == BOOL_OBJ) {
        *truth = Bool(val)->value;
        return false;
    }
    err = obj->truth(val, TRUTH_BOOL, &v1->epoint);
    error = err->obj != BOOL_OBJ;
    if (error) {
        if (err->obj == ERROR_OBJ) {
            err_msg_output(Error(err));
        }
    } else {
        *truth = Bool(err)->value;
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, val, &v1->epoint);
    }
    val_destroy(err);
    return error;
}

static MUST_CHECK bool touval2(const struct values_s *vals, uval_t *uv, unsigned int bits) {
    Error *err;
    Obj *val = vals->val;
    if (val == none_value && (constcreated || !fixeddig) && pass < max_pass) return true;
    err = val->obj->uval2(val, uv, bits, &vals->epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

/* --------------------------------------------------------------------------- */
/*
 * Skip memory
 */
static void memskip(address_t db, linepos_t epoint) {
    if (current_address->moved) {
        if (current_address->address < current_address->start) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
        if (current_address->wrapwarn) {err_msg_mem_wrap(epoint);current_address->wrapwarn = false;}
        current_address->moved = false;
    }
    if (current_address->bankwarn) {err_msg_pc_bank(epoint);current_address->bankwarn = false;}
    if (db > (~current_address->l_address & 0xffff)) {
        current_address->bankwarn = (0x10000 - (current_address->l_address & 0xffff) == db);
        if (!current_address->bankwarn) err_msg_pc_bank(epoint);
        current_address->l_address = (current_address->l_address + db) & all_mem;
    } else current_address->l_address += db;
    if (db > (~current_address->address & all_mem2)) {
        if (db - 1 + current_address->address == all_mem2) {
            current_address->wrapwarn = current_address->moved = true;
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->address = 0;
        } else {
            if (current_address->start != 0) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->moved = false;
            current_address->address = (current_address->address + db) & all_mem2;
            err_msg_mem_wrap(epoint);current_address->wrapwarn = false;
        }
    } else current_address->address += db;
    memjmp(current_address->mem, current_address->address);
}

/* --------------------------------------------------------------------------- */
/*
 * output one byte
 */
FAST_CALL uint8_t *pokealloc(address_t db, linepos_t epoint) {
    if (current_address->moved) {
        if (current_address->address < current_address->start) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
        if (current_address->wrapwarn) {err_msg_mem_wrap(epoint);current_address->wrapwarn = false;}
        current_address->moved = false;
    }
    if (current_address->bankwarn) {err_msg_pc_bank(epoint);current_address->bankwarn = false;}
    if (db > (~current_address->l_address & 0xffff)) {
        current_address->bankwarn = (0x10000 - (current_address->l_address & 0xffff) == db);
        if (!current_address->bankwarn) err_msg_pc_bank(epoint);
        current_address->l_address = (current_address->l_address + db) & all_mem;
    } else current_address->l_address += db;
    if (db > (~current_address->address & all_mem2)) {
        if (db - 1 + current_address->address == all_mem2) {
            current_address->wrapwarn = current_address->moved = true;
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->address = 0;
        } else {
            if (current_address->start != 0) err_msg2(ERROR_OUTOF_SECTION, NULL, epoint);
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->moved = false;
            current_address->address = (current_address->address + db) & all_mem2;
            err_msg_mem_wrap(epoint);current_address->wrapwarn = false;
        }
    } else current_address->address += db;
    return alloc_mem(current_address->mem, db);
}

/* --------------------------------------------------------------------------- */
static int get_command(void) {
    enum { MASK = 255 };
    static uint8_t hash[MASK + 1];
    unsigned int no, also, felso, elozo;
    const uint8_t *label;
    uint8_t tmp[13];
    unsigned int ln, h;
    lpoint.pos++;
    label = pline + lpoint.pos;
    for (ln = h = 0; ln < sizeof tmp; ln++) {
        if ((uint8_t)(label[ln] - 'a') <= ('z' - 'a')) {
            h *= 7;
            h ^= tmp[ln] = label[ln];
            continue;
        }
        if ((uint8_t)(label[ln] - 'A') > ('Z' - 'A')) break;
        if (!arguments.caseinsensitive) break;
        h *= 7;
        h ^= tmp[ln] = label[ln] | 0x20;
    }
    if ((unsigned int)(ln - 2u) >= (sizeof tmp - 2)) return lenof(command);
    if (label[ln] >= '0') {
        if ((label[ln] & 0x80) != 0) {
            if (arguments.to_ascii) {
                unichar_t ch;
                utf8in(label + ln, &ch);
                if ((uget_property(ch)->property & (id_Continue | id_Start)) != 0) return lenof(command);
            }
        } else if (label[ln] <= '9' || (uint8_t)(label[ln] - 'A') <= ('Z' - 'A') || label[ln] == '_') return lenof(command);
    }
    tmp[ln] = 0;
    for (;;) {
        const uint8_t *cmd2;
        uint8_t h2 = hash[h & MASK];
        if (h2 == 0) break;
        cmd2 = (const uint8_t *)command[h2 - 1] + 1;
        if (tmp[0] == cmd2[0]) {
            unsigned int i;
            for (i = 1; tmp[i] == cmd2[i]; i++) {
                if (tmp[i] != 0) continue;
                lpoint.pos += ln;
                return cmd2[-1];
            }
        }
        h += 5;
    }

    also = 0;
    felso = lenof(command);
    no = lenof(command)/2;
    do {
        const uint8_t *cmd2 = (const uint8_t *)command[no] + 1;
        unsigned int i;
        for (i = 0; tmp[i] == cmd2[i]; i++) {
            if (tmp[i] != 0) continue;
            lpoint.pos += i;
            hash[h & MASK] = (uint8_t)(no + 1);
            return cmd2[-1];
        }
        elozo = no;
        no = ((tmp[i] >= cmd2[i]) ? (felso + (also = no)) : (also + (felso = no)))/2;
    } while (elozo != no);
    return lenof(command);
}

/* ------------------------------------------------------------------------------ */

static void set_cpumode(const struct cpu_s *cpumode) {
    current_cpu = cpumode;
    all_mem = cpumode->max_address;
    all_mem_bits = (all_mem == 0xffff) ? 16 : 24;
    select_opcodes(cpumode);
    cpu_opt_set_cpumode(cpumode);
    if (registerobj_createnames(cpumode->registers)) constcreated = true;
}

static void const_assign(Label *label, Obj *val) {
    label->defpass = pass;
    if (fixeddig && label->usepass >= pass) {
        if (val->obj->same(val, label->value)) {
            val_destroy(val);
            return;
        }
        if (pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
        fixeddig = false;
    }
    val_destroy(label->value);
    label->value = val;
}

struct textrecursion_s {
    address_t gaps, p;
    address_t sum, max;
    int prm;
    Textconv_types tconv;
    Error_types error;
    uint8_t buff[16];
    linepos_t epoint;
};

static void textrecursion_flush(struct textrecursion_s *trec) {
    memcpy(pokealloc(trec->p, trec->epoint), trec->buff, trec->p);
    trec->p = 0;
}

static void textrecursion_gaps(struct textrecursion_s *trec) {
    memskip(trec->gaps, trec->epoint);
    trec->gaps = 0;
}

static void textdump(struct textrecursion_s *trec, unsigned int uval) {
    switch (trec->prm) {
    case CMD_SHIFT:
        if ((uval & 0x80) != 0) {
            uval ^= 0x80;
            trec->error = ERROR___NO_HIGH_BIT;
        }
        break;
    case CMD_SHIFTL:
        if ((uval & 0x80) != 0) trec->error = ERROR___NO_HIGH_BIT;
        uval <<= 1;
        break;
    case CMD_NULL:
        if (uval == 0) trec->error = ERROR_NO_ZERO_VALUE;
        break;
    default:
        break;
    }
    if (trec->p >= sizeof trec->buff) textrecursion_flush(trec);
    trec->buff[trec->p++] = (uint8_t)(uval ^ outputeor);
}

static void textdump_bytes(struct textrecursion_s *trec, const Bytes *bytes) {
    size_t len2;
    address_t i, ln;
    unsigned int inv;
    if (bytes->len > 0) {
        len2 = (size_t)bytes->len;
        inv = 0;
    } else if (bytes->len == 0) {
        return;
    } else {
        inv = 0xff;
        len2 = (size_t)~bytes->len;
    }
    ln = trec->max - trec->sum;
    if (len2 < ln) ln = (address_t)len2;
    trec->sum += ln;
    if (trec->gaps > 0) textrecursion_gaps(trec);
    if (ln > sizeof trec->buff) {
        uint8_t *d;
        if (trec->p > 0) textrecursion_flush(trec);
        if (trec->prm == CMD_SHIFT || trec->prm == CMD_SHIFTL) ln--;
        d = pokealloc(ln, trec->epoint);
        switch (trec->prm) {
        case CMD_SHIFT:
            for (i = 0; i < ln; i++) {
                unsigned int uval = bytes->data[i] ^ inv;
                if ((uval & 0x80) != 0) {
                    uval ^= 0x80;
                    trec->error = ERROR___NO_HIGH_BIT;
                }
                d[i] = (uint8_t)(uval ^ outputeor);
            }
            textdump(trec, bytes->data[i] ^ inv);
            return;
        case CMD_SHIFTL:
            for (i = 0; i < ln; i++) {
                unsigned int uval = bytes->data[i] ^ inv;
                if ((uval & 0x80) != 0) trec->error = ERROR___NO_HIGH_BIT;
                uval <<= 1;
                d[i] = (uint8_t)(uval ^ outputeor);
            }
            textdump(trec, bytes->data[i] ^ inv);
            return;
        case CMD_NULL:
            for (i = 0; i < ln; i++) {
                unsigned int uval = bytes->data[i] ^ inv;
                if (uval == 0) trec->error = ERROR_NO_ZERO_VALUE;
                d[i] = (uint8_t)(uval ^ outputeor);
            }
            return;
        default:
            inv = (uint8_t)(inv ^ outputeor);
            if (inv == 0) {
                memcpy(d, bytes->data, ln);
                return;
            }
            for (i = 0; i < ln; i++) {
                d[i] = (uint8_t)(bytes->data[i] ^ inv);
            }
            return;
        }
    }
    for (i = 0; i < ln; i++) {
        textdump(trec, bytes->data[i] ^ inv);
    }
}

static void textrecursion(struct textrecursion_s *trec, Obj *val) {
    struct iter_s iter;
    uval_t uval;

    if (trec->sum >= trec->max) return;

retry:
    switch (val->obj->type) {
    case T_STR:
        val = bytes_from_str(Str(val), trec->epoint, trec->tconv);
    asbytes:
        if (val->obj != BYTES_OBJ) goto nbytes;
        textdump_bytes(trec, Bytes(val));
        val_destroy(val);
        return;
    case T_ERROR:
    case T_FLOAT:
    case T_INT:
    case T_BOOL:
    case T_NONE:
        iter.data = NULL;
        goto doit;
    case T_CODE:
        val = get_code_value(Code(val), trec->epoint);
    nbytes:
        textrecursion(trec, val);
        val_destroy(val);
        return;
    case T_ADDRESS:
        if (Address(val)->type != A_NONE) {
            iter.data = NULL;
            goto doit;
        }
        val = Address(val)->val;
        goto retry;
    case T_GAP:
        iter.data = NULL;
        goto dogap;
    case T_BITS:
        if (Bits(val)->bits <= 8) {
            if (Bits(val)->bits == 0) return;
            iter.data = NULL;
            goto doit;
        }
        val = bytes_from_bits(Bits(val), trec->epoint);
        goto asbytes;
    case T_BYTES:
        iter.data = NULL;
        goto dobytes;
    default:
        iter.data = val; val->obj->getiter(&iter);
    }

    while ((val = iter.next(&iter)) != NULL) {
        if (val->obj->iterable) goto rec;
        switch (val->obj->type) {
        case T_BITS:
            if (Bits(val)->bits <= 8) {
                if (Bits(val)->bits == 0) break;
                goto doit;
            }
            FALL_THROUGH; /* fall through */
        case T_STR:
        case T_CODE:
        case T_ADDRESS:
        rec:
            textrecursion(trec, val);
            break;
        case T_GAP:
        dogap:
            if (trec->p > 0) textrecursion_flush(trec);
            trec->gaps++;
            trec->sum++;
            if (iter.data == NULL) return;
            break;
        case T_BYTES:
        dobytes:
            textdump_bytes(trec, Bytes(val));
            if (iter.data == NULL) return;
            break;
        default:
        doit:
            if (touval(val, &uval, 8, trec->epoint)) uval = 256 + '?'; else uval &= 0xff;
            trec->sum++;
            if (trec->gaps > 0) textrecursion_gaps(trec);
            textdump(trec, uval);
            if (iter.data == NULL) return;
            break;
        }
        if (trec->sum >= trec->max) break;
    }
    iter_destroy(&iter);
}

struct byterecursion_s {
    address_t gaps, p;
    uint8_t buff[16];
    bool warn;
    linepos_t epoint;
    linepos_t epoint2;
    int bits;
    int prm;
};

static void byterecursion_flush(struct byterecursion_s *brec) {
    memcpy(pokealloc(brec->p, brec->epoint), brec->buff, brec->p);
    brec->p = 0;
}

static void byterecursion_gaps(struct byterecursion_s *brec) {
    memskip(brec->gaps, brec->epoint);
    brec->gaps = 0;
}

static void byterecursion(struct byterecursion_s *brec, Obj *val) {
    struct iter_s iter;
    uint32_t ch2;
    uval_t uv;
    ival_t iv;
    const Type *type = val->obj;

    if (!type->iterable) {
        if (type == GAP_OBJ) {
            if (brec->p > 0) byterecursion_flush(brec);
            brec->gaps += (unsigned int)abs(brec->bits) / 8;
            return;
        }
        iter.data = NULL;
        if (type == NONE_OBJ) goto donone;
        goto doit;
    }
    iter.data = val; type->getiter(&iter);
    while ((val = iter.next(&iter)) != NULL) {
        if (val->obj->iterable) {
            byterecursion(brec, val);
            continue;
        }
        switch (val->obj->type) {
        case T_GAP:
            if (brec->p > 0) byterecursion_flush(brec);
            brec->gaps += (unsigned int)abs(brec->bits) / 8;
            continue;
        default:
        doit:
            if (brec->prm == CMD_RTA || brec->prm == CMD_ADDR) {
                atype_t am = val->obj->address(val);
                if (touaddress(val, &uv, (am == A_KR) ? 16 : all_mem_bits, brec->epoint)) {
                    ch2 = 0;
                    break;
                }
                uv &= all_mem;
                switch (am) {
                case A_NONE:
                    if ((current_address->l_address ^ uv) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, val, brec->epoint);
                    break;
                case A_KR:
                    break;
                default:
                    err_msg_output_and_destroy(err_addressing(am, brec->epoint, -1));
                }
                ch2 = (brec->prm == CMD_RTA) ? (uv - 1) : uv;
                break;
            }
            if (brec->bits >= 0) {
                if (touval(val, &uv, (unsigned int)brec->bits, brec->epoint)) {
                    if (diagnostics.pitfalls) {
                        if (brec->prm == CMD_BYTE && val->obj == STR_OBJ) err_msg_byte_note(brec->epoint2);
                        else if (brec->prm != CMD_RTA && brec->prm != CMD_ADDR && err_msg_char_note_once != pass) {
                            Error *err = val->obj->ival(val, &iv, (unsigned int)brec->bits, brec->epoint2);
                            if (err != NULL) val_destroy(Obj(err));
                            else {
                                const char *txt;
                                switch (brec->prm) {
                                case CMD_BYTE:  txt = ".char"; break;
                                case CMD_LONG:  txt = ".lint"; break;
                                case CMD_DWORD: txt = ".dint"; break;
                                default:
                                case CMD_WORD:  txt = ".sint"; break;
                                }
                                err_msg_char_note(txt, brec->epoint2);
                                err_msg_char_note_once = pass;
                            }
                        }
                    }
                    uv = 0;
                }
                ch2 = uv;
            } else {
                if (toival(val, &iv, (unsigned int)-brec->bits, brec->epoint)) iv = 0;
                ch2 = (uint32_t)iv;
            }
            break;
        case T_NONE:
        donone:
            brec->warn = true;
            ch2 = 0;
        }
        if (brec->gaps > 0) byterecursion_gaps(brec);
        else if (brec->p >= (sizeof brec->buff) - 4) byterecursion_flush(brec);
        ch2 ^= outputeor;
        brec->buff[brec->p++] = (uint8_t)ch2;
        if (brec->prm >= CMD_RTA) {
            brec->buff[brec->p++] = (uint8_t)(ch2 >> 8);
            if (brec->prm >= CMD_LINT) {
                brec->buff[brec->p++] = (uint8_t)(ch2 >> 16);
                if (brec->prm >= CMD_DINT) brec->buff[brec->p++] = (uint8_t)(ch2 >> 24);
            }
        }
        if (iter.data == NULL) return;
    }
    iter_destroy(&iter);
}

static void memfill(address_t db, const struct values_s *vs) {
    struct textrecursion_s trec;
    size_t membp = get_mem(current_address->mem);
    address_t oaddr = current_address->address;

    trec.p = 0;
    trec.gaps = 0;
    trec.sum = 0;
    trec.max = db;
    trec.prm = CMD_TEXT;
    trec.tconv = BYTES_MODE_TEXT;
    trec.error = ERROR__USER_DEFINED;
    trec.epoint = &vs->epoint;
    textrecursion(&trec, vs->val);
    if (trec.error != ERROR__USER_DEFINED) err_msg2(trec.error, NULL, trec.epoint);

    db -= trec.sum;
    if (db != 0) {
        if (trec.sum == 1 && trec.p == 1) {
            memset(pokealloc(db + 1, trec.epoint), trec.buff[0], db + 1); /* single byte shortcut */
            trec.p = 0;
        } else if (trec.sum == trec.gaps) {
            if (trec.sum == 0) err_msg2(ERROR__BYTES_NEEDED, NULL, trec.epoint);
            trec.gaps += db; /* gap shortcut */
        } else {
            address_t offs = 0;
            if (trec.p > 0) textrecursion_flush(&trec);
            while (db != 0) { /* pattern repeat */
                int ch;
                db--;
                ch = read_mem(current_address->mem, oaddr, membp, offs);
                if (ch < 0) {
                    if (trec.p > 0) textrecursion_flush(&trec);
                    trec.gaps++;
                } else {
                    if (trec.gaps > 0) textrecursion_gaps(&trec);
                    else if (trec.p >= sizeof trec.buff) textrecursion_flush(&trec);
                    trec.buff[trec.p++] = (uint8_t)ch;
                }
                offs++;
                if (offs >= trec.sum) offs = 0;
            }
        }
    }
    if (trec.p > 0) textrecursion_flush(&trec);
    else if (trec.gaps > 0) textrecursion_gaps(&trec);
}

static void memskipfill(address_t db, const struct values_s *vs, linepos_t epoint) {
    struct mem_mark_s mm;
    mark_mem(&mm, current_address->mem, current_address->address, current_address->l_address);
    if (vs != NULL && vs->val != gap_value) {
        memfill(db, vs);
    } else {
        memskip(db, epoint);
    }
    if (nolisting == 0) {
        list_mem(&mm, current_address->mem);
    }
}

static uval_t memalign_offset(const struct values_s *vs, uval_t size) {
    ival_t ival;
    if (vs != NULL && !toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) {
        if (ival < 0) {
            if (-(uval_t)ival <= size) return size - -(uval_t)ival;
        } else {
            if ((uval_t)ival < size) return (uval_t)ival;
        }
        err_msg2(ERROR__OFFSET_RANGE, vs->val, &vs->epoint);
    }
    return 0;
}

static address_t rmemalign(uval_t offset, uval_t size, address_t itt) {
    address_t max, rem;
    if (size < 2) return 0;
    max = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? all_mem2 : all_mem;
    if (size > max) size = max + 1;
    rem = itt % size;
    if (rem > offset) offset += size;
    return offset - rem;
}

static address_t dmemalign(uval_t offset, uval_t size) {
    return rmemalign(offset, size, (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address - current_address->l_start) & all_mem));
}

static void logical_close(linepos_t epoint) {
    address_t diff;
    if (current_address->unionmode) {
        current_address->l_union = waitfor->u.cmd_logical.laddr;
        diff = 0;
    } else {
        diff = (current_address->address - waitfor->u.cmd_logical.addr) & all_mem2;
        if (diff > (~waitfor->u.cmd_logical.laddr & 0xffff)) {
            current_address->bankwarn = (0x10000 - (waitfor->u.cmd_logical.laddr & 0xffff) == diff);
            if (epoint != NULL && !current_address->bankwarn) err_msg_pc_bank(epoint);
            current_address->l_address = (waitfor->u.cmd_logical.laddr + diff) & all_mem;
        } else {
            current_address->bankwarn = false;
            current_address->l_address = waitfor->u.cmd_logical.laddr + diff;
        }
    }
    val_destroy(current_address->l_address_val);
    current_address->l_address_val = waitfor->u.cmd_logical.val;
    if (waitfor->u.cmd_logical.label != NULL) {
        set_size(waitfor->u.cmd_logical.label, diff, current_address->mem, waitfor->u.cmd_logical.addr, waitfor->u.cmd_logical.membp);
        val_destroy(Obj(waitfor->u.cmd_logical.label));
    }
    current_section->logicalrecursion--;
}

static void virtual_close(linepos_t epoint) {
    if (waitfor->u.cmd_virtual.label != NULL) {
        address_t end = (current_address->end < current_address->address) ? current_address->address : current_address->end;
        set_size(waitfor->u.cmd_virtual.label, end - current_address->start, current_address->mem, current_address->start, waitfor->u.cmd_virtual.membp);
        val_destroy(Obj(waitfor->u.cmd_virtual.label));
    }
    val_destroy(current_address->l_address_val);
    val_destroy(Obj(current_address->mem));
    free(current_address);
    current_address = waitfor->u.cmd_virtual.section_address;
    if (current_address->l_address > all_mem) {
        if (epoint != NULL) err_msg_big_address(epoint);
        current_address->l_address &= all_mem;
    }
}

static void section_close(linepos_t epoint) {
    if (waitfor->u.cmd_section.label != NULL) {
        set_size(waitfor->u.cmd_section.label, current_address->address - waitfor->u.cmd_section.addr, current_address->mem, waitfor->u.cmd_section.addr, waitfor->u.cmd_section.membp);
        val_destroy(Obj(waitfor->u.cmd_section.label));
    }
    current_section = waitfor->u.cmd_section.section;
    current_address = waitfor->u.cmd_section.section_address;
    if (current_address->l_address > all_mem) {
        if (epoint != NULL) err_msg_big_address(epoint);
        current_address->l_address &= all_mem;
    }
}

static void union_close(linepos_t epoint) {
    address_t end;
    current_address->unionmode = waitfor->u.cmd_union.unionmode;
    current_address->l_union = waitfor->u.cmd_union.laddr;
    current_address->start = waitfor->u.cmd_union.addr;
    end = (current_address->address < current_address->end) ? current_address->end : current_address->address;
    current_address->end = (waitfor->u.cmd_union.addr2 > end) ? waitfor->u.cmd_union.addr2 : end;
    if (end > current_address->address) {
        current_address->wrapwarn = false;
        current_address->bankwarn = false;
        memskip(end - current_address->address, epoint);
    }
}

static void encode_close(void) {
    val_destroy(Obj(actual_encoding));
    actual_encoding = waitfor->u.cmd_encode.enc;
}

static const char *check_waitfor(void) {
    switch (waitfor->what) {
    case W_FI2:
    case W_FI:
        if (waitfor->u.cmd_if.label != NULL) {set_size(waitfor->u.cmd_if.label, current_address->address - waitfor->u.cmd_if.addr, current_address->mem, waitfor->u.cmd_if.addr, waitfor->u.cmd_if.membp);val_destroy(Obj(waitfor->u.cmd_if.label));}
        return ".endif";
    case W_SWITCH2:
    case W_SWITCH:
        if (waitfor->u.cmd_switch.label != NULL) {set_size(waitfor->u.cmd_switch.label, current_address->address - waitfor->u.cmd_switch.addr, current_address->mem, waitfor->u.cmd_switch.addr, waitfor->u.cmd_switch.membp);val_destroy(Obj(waitfor->u.cmd_switch.label));}
        if (waitfor->u.cmd_switch.val != NULL) val_destroy(waitfor->u.cmd_switch.val);
        return ".endswitch";
    case W_WEAK2:
        if (waitfor->u.cmd_weak.label != NULL) {set_size(waitfor->u.cmd_weak.label, current_address->address - waitfor->u.cmd_weak.addr, current_address->mem, waitfor->u.cmd_weak.addr, waitfor->u.cmd_weak.membp);val_destroy(Obj(waitfor->u.cmd_weak.label));}
        strength--;
        FALL_THROUGH; /* fall through */
    case W_WEAK: return ".endweak";
    case W_ENDP2:
        if (waitfor->u.cmd_page.label != NULL) {set_size(waitfor->u.cmd_page.label, current_address->address - waitfor->u.cmd_page.addr, current_address->mem, waitfor->u.cmd_page.addr, waitfor->u.cmd_page.membp);val_destroy(Obj(waitfor->u.cmd_page.label));}
        FALL_THROUGH; /* fall through */
    case W_ENDP: return ".endpage";
    case W_ENDALIGNBLK2:
        alignblk_set_size(waitfor->u.cmd_alignblk.label2);
        if (waitfor->u.cmd_alignblk.label != NULL) {set_size(waitfor->u.cmd_alignblk.label, current_address->address - waitfor->u.cmd_alignblk.addr, current_address->mem, waitfor->u.cmd_alignblk.addr, waitfor->u.cmd_alignblk.membp);val_destroy(Obj(waitfor->u.cmd_alignblk.label));}
        FALL_THROUGH; /* fall through */
    case W_ENDALIGNBLK: return ".endalignblk";
    case W_ENDSEGMENT:
        if (waitfor->u.cmd_macro.val != NULL) val_destroy(waitfor->u.cmd_macro.val);
        return ".endsegment";
    case W_ENDMACRO:
        if (waitfor->u.cmd_macro.val != NULL) val_destroy(waitfor->u.cmd_macro.val);
        return ".endmacro";
    case W_ENDF:
        if (waitfor->u.cmd_function.val != NULL) val_destroy(waitfor->u.cmd_function.val);
        return ".endfunction";
    case W_ENDFOR3:
        pop_context();
        FALL_THROUGH; /* fall through */
    case W_ENDFOR: return ".endfor";
    case W_ENDREPT3:
        pop_context();
        FALL_THROUGH; /* fall through */
    case W_ENDREPT: return ".endrept";
    case W_ENDWHILE3:
        pop_context();
        FALL_THROUGH; /* fall through */
    case W_ENDWHILE: return ".endwhile";
    case W_PEND:
        pop_context();
        if (waitfor->u.cmd_proc.label != NULL) {set_size(waitfor->u.cmd_proc.label, current_address->address - waitfor->u.cmd_proc.addr, current_address->mem, waitfor->u.cmd_proc.addr, waitfor->u.cmd_proc.membp);val_destroy(Obj(waitfor->u.cmd_proc.label));}
        return ".endproc";
    case W_BEND2:
        if (waitfor->u.cmd_block.label != NULL) {set_size(waitfor->u.cmd_block.label, current_address->address - waitfor->u.cmd_block.addr, current_address->mem, waitfor->u.cmd_block.addr, waitfor->u.cmd_block.membp);val_destroy(Obj(waitfor->u.cmd_block.label));}
        FALL_THROUGH; /* fall through */
    case W_BEND:
        pop_context();
        return ".endblock";
    case W_ENDN2:
    case W_ENDN:
        pop_context();
        return ".endnamespace";
    case W_ENDWITH2:
        pop_context2();
        FALL_THROUGH; /* fall through */
    case W_ENDWITH:
        if (waitfor->u.cmd_with.label != NULL) {set_size(waitfor->u.cmd_with.label, current_address->address - waitfor->u.cmd_with.addr, current_address->mem, waitfor->u.cmd_with.addr, waitfor->u.cmd_with.membp);val_destroy(Obj(waitfor->u.cmd_with.label));}
        return ".endwith";
    case W_ENDENCODE2:
        encode_close();
        FALL_THROUGH; /* fall through */
    case W_ENDENCODE: return ".endencode";
    case W_ENDC: return ".endcomment";
    case W_ENDS:
        if ((waitfor->skip & 1) != 0) current_address->unionmode = waitfor->u.cmd_struct.unionmode;
        FALL_THROUGH; /* fall through */
    case W_ENDS2: return ".endstruct";
    case W_SEND2:
        section_close(NULL);
        FALL_THROUGH; /* fall through */
    case W_SEND: return ".endsection";
    case W_ENDU:
        if ((waitfor->skip & 1) != 0) union_close(&lpoint);
        FALL_THROUGH; /* fall through */
    case W_ENDU2: return ".endunion";
    case W_HERE2:
        logical_close(NULL);
        FALL_THROUGH; /* fall through */
    case W_HERE: return ".endlogical";
    case W_ENDV2:
        virtual_close(NULL);
        FALL_THROUGH; /* fall through */
    case W_ENDV: return ".endvirtual";
    case W_ENDU3:
    case W_ENDS3:
    case W_ENDSEGMENT2:
    case W_ENDMACRO2:
    case W_ENDF2:
    case W_ENDF3:
    case W_ENDFOR2:
    case W_ENDREPT2:
    case W_ENDWHILE2:
    case W_NONE:
        break;
    }
    return NULL;
}

static bool section_start(linepos_t epoint) {
    struct section_s *tmp;
    str_t sectionname;
    struct linepos_s opoint;

    new_waitfor(W_SEND, epoint);waitfor->u.cmd_section.section = current_section;waitfor->u.cmd_section.section_address = current_address;
    opoint = lpoint;
    sectionname.data = pline + lpoint.pos; sectionname.len = get_label(sectionname.data);
    if (sectionname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &opoint); return true;}
    lpoint.pos += (linecpos_t)sectionname.len;
    tmp = find_new_section(&sectionname);
    if (tmp->usepass == 0 || tmp->defpass < pass - 1) {
        address_t ln = tmp->address.mem->mem.p;
        size_t ln2 = tmp->address.mem->p;
        tmp->address.end = tmp->address.start = tmp->restart = tmp->address.address = current_address->address;
        tmp->size = 0; tmp->l_restart = tmp->address.l_address = current_address->l_address;
        if (tmp->usepass != 0 && tmp->usepass >= pass - 1) err_msg_not_defined(&sectionname, &opoint);
        else {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&sectionname, epoint);
            fixeddig = false;
        }
        tmp->defpass = (uint8_t)(pass - 1);
        val_destroy(Obj(tmp->address.mem));
        tmp->address.mem = new_memblocks(ln, ln2);
        tmp->address.mem->lastaddr = tmp->address.address;
        tmp->address.mem->section = tmp;
        if (diagnostics.optimize) cpu_opt_invalidate();
    } else if (tmp->usepass != pass) {
        address_t ln = tmp->address.mem->mem.p;
        size_t ln2 = tmp->address.mem->p;
        if (!tmp->address.moved) {
            if (tmp->address.end < tmp->address.address) tmp->address.end = tmp->address.address;
            tmp->address.moved = true;
        }
        tmp->size = tmp->address.end - tmp->address.start;
        tmp->address.end = tmp->address.start = tmp->restart;
        tmp->address.wrapwarn = false;
        tmp->address.bankwarn = false;
        tmp->address.address = tmp->restart;
        tmp->address.l_address = tmp->l_restart;
        val_destroy(Obj(tmp->address.mem));
        tmp->address.mem = new_memblocks(ln, ln2);
        tmp->address.mem->lastaddr = tmp->address.address;
        tmp->address.mem->section = tmp;
        if (diagnostics.optimize) cpu_opt_invalidate();
    }
    tmp->usepass = pass;
    waitfor->what = W_SEND2;
    waitfor->u.cmd_section.label = NULL;
    current_section = tmp;
    current_address = &tmp->address;
    return false;
}

static void address_update(struct section_address_s *section_address, Obj *val) {
    Obj *tmp = get_star_value(0, val);
    if (tmp->obj->same(tmp, section_address->l_address_val)) {
        val_destroy(tmp);
        return;
    }
    val_destroy(section_address->l_address_val);
    section_address->l_address_val = tmp;
}

static bool virtual_start(linepos_t epoint) {
    struct section_address_s *section_address;
    Obj *tmp = NULL;
    bool retval = false;

    if (diagnostics.optimize) cpu_opt_invalidate();
    new_waitfor(W_ENDV2, epoint); waitfor->u.cmd_virtual.section_address = current_address; waitfor->u.cmd_virtual.label = NULL;
    new_instance(&section_address);
    section_address->wrapwarn = section_address->moved = false;
    section_address->bankwarn = false;

    do {
        struct values_s *vs;
        atype_t am;
        uval_t uval;

        if (!get_exp(0, 0, 1, epoint)) {retval = true;break;}
        vs = get_val();
        if (vs == NULL) break;
        if (touaddress(vs->val, &uval, all_mem_bits, &vs->epoint)) {retval = true; break;}
        am = vs->val->obj->address(vs->val);
        if (am != A_NONE && check_addr(am)) {
            err_msg_output_and_destroy(err_addressing(am, &vs->epoint, -1));
            retval = true;
            break;
        }
        tmp = vs->val;
        section_address->address = uval;
        section_address->l_address = uval & all_mem;
        section_address->l_address_val = get_star_value(0, tmp);
    } while (false);

    if (tmp == NULL) {
        section_address->address = current_address->address;
        section_address->l_address = current_address->l_address;
        section_address->l_address_val = val_reference(current_address->l_address_val);
    }
    section_address->unionmode = current_address->unionmode;
    section_address->l_start = current_address->l_start;
    section_address->l_union = current_address->l_union;
    section_address->start = section_address->end = section_address->address;
    section_address->mem = new_memblocks(0, 0);
    section_address->mem->lastaddr = section_address->address;
    current_address = section_address;
    return retval;
}

static void starhandle(Obj *val, linepos_t epoint, linepos_t epoint2) {
    uval_t uval;
    atype_t am;
    address_t addr, laddr;

    current_address->wrapwarn = false;
    current_address->bankwarn = false;
    if (!current_address->moved) {
        if (current_address->end < current_address->address) current_address->end = current_address->address;
        current_address->moved = true;
    }
    listing_line(epoint->pos);
    do {
        {
            address_t max = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? all_mem2 : all_mem;
            if (touaddress(val, &uval, (max == 0xffff) ? 16 : (max == 0xffffff) ? 24 : 32, epoint2)) {
                break;
            }
        }
        am = val->obj->address(val);
        if (am != A_NONE && check_addr(am)) {
            err_msg_output_and_destroy(err_addressing(am, epoint2, -1));
            break;
        }
        if (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) {
            current_address->l_address = uval & all_mem;
            address_update(current_address, val);
            val_destroy(val);
            addr = uval & all_mem2;
            if (current_address->address != addr) {
                current_address->address = addr;
                memjmp(current_address->mem, current_address->address);
            }
            return;
        }
        laddr = current_address->l_address;
        addr = uval & all_mem;
        if (arguments.tasmcomp) addr = (uint16_t)addr;
        else if (addr >= laddr) {
            addr = (current_address->address + (addr - laddr)) & all_mem2;
        } else {
            addr = (current_address->address - (laddr - addr)) & all_mem2;
        }
        if (current_address->address != addr) {
            current_address->address = addr;
            memjmp(current_address->mem, current_address->address);
        }
        current_address->l_address = uval & all_mem;
        address_update(current_address, val);
        val_destroy(val);
        return;
    } while (false);
    val_destroy(val);
}

static Oper_types oper_from_token(int wht) {
    switch (wht) {
    case 'X':
        if (arguments.caseinsensitive == 0) {
            return O_NONE;
        }
        FALL_THROUGH; /* fall through */
    case 'x': return O_X;
    case '*': return O_MUL;
    case '+': return O_ADD;
    case '-': return O_SUB;
    case '/': return O_DIV;
    case '%': return O_MOD;
    case '|': return O_OR;
    case '&': return O_AND;
    case '^': return O_XOR;
    case '.': return O_MEMBER;
    default: return O_NONE;
    }
}

static Oper_types oper_from_token2(int wht, int wht2) {
    if (wht == wht2) {
        switch (wht) {
        case '&': return O_LAND;
        case '|': return O_LOR;
        case '>': return O_RSHIFT;
        case '<': return O_LSHIFT;
        case '.': return O_CONCAT;
        case '*': return O_EXP;
        case ':': return O_REASSIGN;
        default: return O_NONE;
        }
    }
    if (wht2 == '?') {
        switch (wht) {
        case '<': return O_MIN;
        case '>': return O_MAX;
        case ':': return O_COND;
        default: return O_NONE;
        }
    }
    return O_NONE;
}

static MUST_CHECK Obj *tuple_scope_light(Obj **o, linepos_t epoint) {
    Obj *nf, *val = *o;
    if (val->obj != NAMESPACE_OBJ) {
        val_destroy(val);
        *o = val = Obj(new_namespace(current_file_list, epoint));
    } else {
        Namespace *names = Namespace(val);
        names->backr = names->forwr = 0;
        names->file_list = current_file_list;
        names->epoint = *epoint;
    }
    push_context(Namespace(val));
    nf = compile();
    pop_context();
    return nf;
}

static void update_code(const Label *newlabel, Code *code) {
    Obj *tmp;
    if (current_address->bankwarn) {err_msg_pc_bank(&newlabel->epoint);current_address->bankwarn = false;}
    tmp = current_address->l_address_val;
    if (!tmp->obj->same(tmp, code->typ)) {
        val_destroy(code->typ); code->typ = val_reference(tmp);
        if (newlabel->usepass >= pass) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
            fixeddig = false;
        }
    }
    if (code->addr != star || code->requires != current_section->requires || code->conflicts != current_section->conflicts || code->offs != 0) {
        code->addr = star;
        code->requires = current_section->requires;
        code->conflicts = current_section->conflicts;
        code->offs = 0;
        if (newlabel->usepass >= pass) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
            fixeddig = false;
        }
    }
    code->names->backr = code->names->forwr = 0;
    code->names->file_list = current_file_list;
    code->names->epoint = newlabel->epoint;
    code->apass = pass;
}

static MUST_CHECK Code *create_code(linepos_t epoint) {
    Code *code;
    if (current_address->bankwarn) {err_msg_pc_bank(epoint);current_address->bankwarn = false;}
    code = new_code();
    code->addr = star;
    code->typ = val_reference(current_address->l_address_val);
    code->size = 0;
    code->offs = 0;
    code->dtype = D_NONE;
    code->pass = 0;
    code->apass = pass;
    code->memblocks = ref_memblocks(current_address->mem);
    code->names = new_namespace(current_file_list, epoint);
    code->requires = current_section->requires;
    code->conflicts = current_section->conflicts;
    return code;
}

static MUST_CHECK Obj *tuple_scope(const Label *newlabel, Obj **o) {
    Obj *nf;
    address_t size;
    Code *code;
    address_t oaddr;
    size_t newmembp;
    Obj *val = *o;

    if (diagnostics.optimize && newlabel->ref) cpu_opt_invalidate();
    oaddr = current_address->address;
    if (val->obj == CODE_OBJ) {
        code = Code(val);
        update_code(newlabel, code);
    } else {
        val_destroy(val);
        code = create_code(&newlabel->epoint);
        *o = Obj(code);
    }
    newmembp = get_mem(current_address->mem);
    push_context(code->names);
    nf = compile();
    pop_context();
    size = (current_address->address - oaddr) & all_mem2;
    if (code->size != size) {
        code->size = size;
        if (code->pass != 0) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
            fixeddig = false;
        }
    }
    code->pass = pass;
    if (code->memblocks != current_address->mem) {
        val_destroy(Obj(code->memblocks));
        code->memblocks = ref_memblocks(current_address->mem);
    }
    code->memaddr = oaddr;
    code->membp = newmembp;
    return nf;
}

static MUST_CHECK bool list_extend2(List *lst) {
    size_t o;
    Obj **vals;
    if (list_extend(lst)) return true;
    o = lst->len;
    vals = lst->data;
    while (o < lst->u.s.max) vals[o++] = ref_none();
    lst->len = o;
    return false;
}

static size_t for_command(const Label *newlabel, List *lst, linepos_t epoint) {
    int wht;
    linenum_t lin;
    int nopos = -1;
    struct linepos_s epoint2, epoint3;
    uint8_t *expr, *expr2;
    struct {
        size_t p, len;
        Label **data;
        Label *val[4];
    } labels;
    Label *label;
    Obj *val, *nf = NULL;
    struct star_s *s, *stree_old;
    bool foreach = false;
    struct iter_s iter;
    size_t i = 0;
    labels.p = 0;
    iter.data = NULL;

    if (diagnostics.optimize) cpu_opt_invalidate();
    if (lst != NULL && newlabel != NULL) listing_equal2(Obj(lst), epoint->pos);
    else listing_line(epoint->pos);

    do { /* label */
        str_t varname;
        epoint2 = lpoint;

        varname.data = pline + lpoint.pos; varname.len = get_label(varname.data);
        if (varname.len == 0) break;
        lpoint.pos += (linecpos_t)varname.len;
        if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint2); goto error;}
        ignore(); wht = here();
        if (wht == ',') {
            lpoint.pos++;
            foreach = true;
            val = ref_none();
        } else {
            if (wht != '=' || foreach) {
                if (wht == ':' && pline[lpoint.pos + 1] == '=' && !arguments.tasmcomp && !foreach) lpoint.pos += 2;
                else {
                    if ((pline[lpoint.pos] | arguments.caseinsensitive) != 'i' || (pline[lpoint.pos+1] | arguments.caseinsensitive) != 'n' || get_label(pline + lpoint.pos) != 2) {
                        err_msg(ERROR______EXPECTED, foreach ? "',' or 'in'" : "':=' or ',' or 'in'"); goto error;
                    }
                    lpoint.pos += 2;
                    foreach = true;
                }
            } else lpoint.pos++;
            if (foreach) {
                ignore();
                epoint3 = lpoint;
                if (!get_exp(0, 1, 0, &epoint3)) goto error;
                val = get_vals_tuple();
                if (val->obj == ERROR_OBJ) {
                    err_msg_output(Error(val));
                } else if (val->obj->getiter == DEFAULT_OBJ->getiter) {
                    if (val != none_value) {
                        Obj *err = new_error_obj(ERROR______NOT_ITER, val, &epoint3);
                        val_destroy(val); val = err;
                        err_msg_output(Error(val));
                    }
                } else {
                    iter.data = val; val->obj->getiter(&iter);
                }
                val_destroy(val); val = ref_none();
            } else {
                if (!get_exp(1, 1, 1, &lpoint)) goto error;
                val = pull_val();
            }
        }
        label = new_label(&varname, (varname.data[0] == '_') ? cheap_context : current_context, strength, current_file_list);
        if (label->value != NULL) {
            if (label->constant) {
                err_msg_double_defined(label, &varname, &epoint2);
                val_destroy(val);
                label = NULL;
            } else {
                if (label->defpass != pass) {
                    label->ref = false;
                    label->defpass = pass;
                } else {
                    if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                }
                label->owner = false;
                if (label->file_list != current_file_list) {
                    label_move(label, &varname, current_file_list);
                }
                label->epoint = epoint2;
                val_destroy(label->value);
                label->value = val;
                label->usepass = 0;
            }
        } else {
            label->constant = false;
            label->owner = false;
            label->value = val;
            label->epoint = epoint2;
        }
        if (foreach) {
            if (labels.p == 0) {
                labels.len = lenof(labels.val);
                labels.data = labels.val;
            } else if (labels.p >= labels.len) {
                if (inc_overflow(&labels.len, 16)) err_msg_out_of_memory();
                if (labels.data == labels.val) {
                    new_array(&labels.data, labels.len);
                    memcpy(labels.data, labels.val, sizeof labels.val);
                } else {
                    resize_array(&labels.data, labels.len);
                }
            }
            labels.data[labels.p++] = label;
        }
        ignore();
    } while (wht == ',');

    if (foreach) {
        if (here() != 0 && here() != ';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    } else {
        if (here() != ',') {err_msg(ERROR______EXPECTED, "','");
        error:
            if (labels.p != 0 && labels.data != labels.val) free(labels.data);
            new_waitfor(W_ENDFOR, epoint); waitfor->skip = 0;
            return i;
        }
        lpoint.pos++;ignore();
    }

    s = new_star(vline); stree_old = star_tree;
    if (s->pass != 0 && s->addr != star) {
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    }
    s->addr = star;
    star_tree->vline = vline; star_tree = s; vline = s->vline;
    lin = lpoint.line;

    new_waitfor(W_ENDFOR2, epoint);
    if (foreach) {
        waitfor->u.cmd_rept.breakout = false;
        if (iter.data != NULL) {
            Obj *val2;

            while ((val2 = iter.next(&iter)) != NULL) {
                if (nopos < 0) nopos = 0;
                else if ((waitfor->skip & 1) != 0) listing_line_cut(waitfor->epoint.pos);
                if (labels.p == 1) {
                    if (labels.data[0] != NULL) {
                        val_destroy(labels.data[0]->value);
                        labels.data[0]->value = val_reference(val2);
                    }
                } else {
                    struct iter_s iter2;
                    size_t j;
                    iter2.data = val2; val2->obj->getiter(&iter2);
                    for (j = 0; j < labels.p && (val2 = iter2.next(&iter2)) != NULL; j++) {
                        if (labels.data[j] != NULL) {
                            val_destroy(labels.data[j]->value);
                            labels.data[j]->value = val_reference(val2);
                        }
                    }
                    if (iter2.len != labels.p) err_msg_cant_unpack(labels.p, iter2.len, epoint);
                    iter_destroy(&iter2);
                }
                lpoint.line = lin;
                waitfor->skip = 1;
                if (lst != NULL) {
                    if (i >= lst->len && list_extend2(lst)) {i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL;}
                    else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                    else nf = tuple_scope(newlabel, &lst->data[i]);
                    i++;
                } else nf = compile();
                if (nf == NULL || waitfor->u.cmd_rept.breakout) {
                    break;
                }
            }
            iter_destroy(&iter);
        }
        if (labels.p != 0 && labels.data != labels.val) free(labels.data);
        expr = expr2 = NULL;
    } else {
        struct linepos_s apoint = lpoint, bpoint = {0, 0};
        linenum_t xlin = lpoint.line;
        struct oper_s tmp;
        const uint8_t *oldpline = pline, *oldpline2 = pline;
        uint8_t skip = 0;
        expr2 = (uint8_t *)oldpline2;
        if (not_in_file(pline, current_file_list->file)) {
            size_t lentmp = strlen((const char *)pline) + 1;
            new_array(&expr, lentmp);
            memcpy(expr, pline, lentmp);
            pline = expr;
        } else expr = (uint8_t *)oldpline;
        label = NULL;
        waitfor->u.cmd_rept.breakout = false;
        tmp.op = O_NONE;
        for (;;) {
            if (here() != ',' && here() != 0) {
                bool truth;
                if (!get_exp(1, 1, 1, &apoint) || tobool(get_val(), &truth) || !truth) {
                    waitfor->skip = skip;
                    break;
                }
            }
            if (nopos < 0) {
                ignore();if (here() != ',') {err_msg(ERROR______EXPECTED, "','"); waitfor->skip = skip; break;}
                lpoint.pos++;ignore();
                oldpline2 = pline;
                if (pline != expr && not_in_file(pline, current_file_list->file)) {
                    size_t lentmp = strlen((const char *)pline) + 1;
                    new_array(&expr2, lentmp);
                    memcpy(expr2, pline, lentmp);
                    pline = expr2;
                } else expr2 = (uint8_t *)oldpline2;
                if (here() == 0 || here() == ';') {bpoint.pos = 0; nopos = 0;}
                else bpoint = lpoint;
            } else {
                if ((skip & 1) != 0) listing_line_cut(waitfor->epoint.pos);
            }
            if (lst != NULL) {
                if (i >= lst->len && list_extend2(lst)) { i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL; }
                else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                else nf = tuple_scope(newlabel, &lst->data[i]);
                i++;
            } else nf = compile();
            xlin = lpoint.line;
            if (nf == NULL || waitfor->u.cmd_rept.breakout) break;
            pline = expr2;
            if (nopos < 0) {
                str_t varname;
                Namespace *context;
                lpoint = bpoint;
                varname.data = pline + lpoint.pos; varname.len = get_label(varname.data);
                if (varname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &bpoint);break;}
                lpoint.pos += (linecpos_t)varname.len;
                if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &bpoint); break;}
                ignore(); wht = here();
                while (wht != 0 && !arguments.tasmcomp) {
                    int wht2 = pline[lpoint.pos + 1];
                    if (wht2 == '=') {
                        Oper_types op;
                        if (wht == ':') {
                            wht = '=';
                            lpoint.pos++;
                            break;
                        }
                        op = oper_from_token(wht);
                        if (op == O_NONE) break;
                        tmp.op = op;
                        epoint3 = lpoint;
                        lpoint.pos += 2;
                    } else if (wht2 != 0 && pline[lpoint.pos + 2] == '=') {
                        Oper_types op;
                        op = oper_from_token2(wht, wht2);
                        if (op == O_COND) op = O_NONE;
                        if (op == O_NONE) break;
                        tmp.op = op;
                        epoint3 = lpoint;
                        lpoint.pos += 3;
                    } else break;

                    ignore();
                    epoint2 = lpoint;
                    tmp.epoint = &bpoint;
                    tmp.epoint2 = &epoint2;
                    tmp.epoint3 = &epoint3;
                    break;
                }
                context = (varname.data[0] == '_') ? cheap_context : current_context;
                if (tmp.op == O_NONE) {
                    if (wht != '=') {err_msg(ERROR______EXPECTED, "':='"); break;}
                    lpoint.pos++;ignore();
                    label = new_label(&varname, context, strength, current_file_list);
                    if (label->value != NULL) {
                        if (label->constant) { err_msg_double_defined(label, &varname, &bpoint); break; }
                        if (label->defpass != pass) {
                            label->ref = false;
                            label->defpass = pass;
                        } else {
                            if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                        }
                        label->owner = false;
                        if (label->file_list != current_file_list) {
                            label_move(label, &varname, current_file_list);
                        }
                        label->epoint = bpoint;
                    } else {
                        label->constant = false;
                        label->owner = false;
                        label->value = ref_none();
                        label->epoint = bpoint;
                    }
                } else {
                    label = (varname.data[0] == '_') ? find_label2(&varname, context) : find_label(&varname, NULL);
                    if (label == NULL) {err_msg_not_defined2(&varname, context, false, &bpoint); break;}
                    if (label->constant) {err_msg_not_variable(label, &varname, &bpoint); break;}
                    if (diagnostics.case_symbol && str_cmp(&varname, &label->name) != 0) err_msg_symbol_case(&varname, label, &bpoint);
                }
                bpoint = lpoint; nopos = 1;
            }
            skip = waitfor->skip;
            waitfor->skip = 1;
            if (nopos > 0) {
                lpoint = bpoint;
                if (here() == 0 || here() == ';') {
                    err_msg(ERROR______EXPECTED, "an expression is");
                    break;
                }
                if (!get_exp(0, 1, 0, &bpoint)) break;
                val = get_vals_tuple();
                if (tmp.op != O_NONE && tmp.op != O_REASSIGN) {
                    bool minmax = (tmp.op == O_MIN) || (tmp.op == O_MAX);
                    Obj *result2, *val1 = label->value;
                    tmp.v1 = val1;
                    tmp.v2 = val;
                    tmp.inplace = (tmp.v1->refcount == 1 && !minmax) ? tmp.v1 : NULL;
                    result2 = tmp.v1->obj->calc2(&tmp);
                    if (minmax) {
                        if (result2 == true_value) val_replace(&result2, val1);
                        else if (result2 == false_value) val_replace(&result2, val);
                    }
                    val_destroy(val);
                    val = result2;
                }
                val_destroy(label->value);
                label->value = val;
                label->usepass = 0;
            }
            pline = expr;
            lpoint = apoint;
        }
        pline = oldpline;
        if (expr == oldpline) expr = NULL;
        if (expr2 == oldpline2) expr2 = NULL;
        lpoint.line = xlin;
    }
    if (nf != NULL) {
        if ((waitfor->skip & 1) != 0) listing_line(waitfor->epoint.pos);
        else listing_line_cut2(waitfor->epoint.pos);
        close_waitfor(W_ENDFOR2);
    } else {
        waitfor->what = W_ENDFOR; waitfor->skip = 0;
    }
    free(expr);
    free(expr2);
    s->vline = vline; star_tree = stree_old; vline = star_tree->vline + lpoint.line - lin;
    return i;
}

static size_t rept_command(const Label *newlabel, List *lst, linepos_t epoint) {
    uval_t cnt;
    Obj *nf;
    size_t i = 0;

    if (diagnostics.optimize) cpu_opt_invalidate();
    if (lst != NULL && newlabel != NULL) listing_equal2(Obj(lst), epoint->pos);
    else listing_line(epoint->pos);
    if (!get_exp(0, 1, 1, epoint)) cnt = 0;
    else {
        if (touval2(get_val(), &cnt, 8 * sizeof cnt)) cnt = 0;
    }
    if (cnt == 0) {
        new_waitfor(W_ENDREPT, epoint); waitfor->skip = 0;
    } else {
        linenum_t lin = lpoint.line;
        struct star_s *s = new_star(vline);
        struct star_s *stree_old = star_tree;

        if (s->pass != 0 && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree->vline = vline; star_tree = s; vline = s->vline;
        new_waitfor(W_ENDREPT2, epoint);
        waitfor->u.cmd_rept.breakout = false;
        for (;;) {
            lpoint.line = lin;
            if (lst != NULL) {
                if (i >= lst->len && list_extend2(lst)) { i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL; }
                else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                else nf = tuple_scope(newlabel, &lst->data[i]);
                i++;
            } else nf = compile();
            if (nf == NULL || waitfor->u.cmd_rept.breakout || (--cnt) == 0) {
                break;
            }
            if ((waitfor->skip & 1) != 0) listing_line_cut(waitfor->epoint.pos);
            waitfor->skip = 1;
        }
        if (nf != NULL) {
            if ((waitfor->skip & 1) != 0) listing_line(waitfor->epoint.pos);
            else listing_line_cut2(waitfor->epoint.pos);
            close_waitfor(W_ENDREPT2);
        } else {
            waitfor->what = W_ENDREPT; waitfor->skip = 0;
        }
        s->vline = vline; star_tree = stree_old; vline = star_tree->vline + lpoint.line - lin;
    }
    return i;
}

static size_t while_command(const Label *newlabel, List *lst, linepos_t epoint) {
    uint8_t *expr;
    Obj *nf = NULL;
    struct star_s *s, *stree_old;
    size_t i = 0;
    struct linepos_s apoint;
    linenum_t xlin;
    const uint8_t *oldpline;
    uint8_t skip = 0;

    if (diagnostics.optimize) cpu_opt_invalidate();
    if (lst != NULL && newlabel != NULL) listing_equal2(Obj(lst), epoint->pos);
    else listing_line(epoint->pos);

    s = new_star(vline); stree_old = star_tree;
    if (s->pass != 0 && s->addr != star) {
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    }
    s->addr = star;
    star_tree->vline = vline; star_tree = s; vline = s->vline;

    apoint = lpoint;
    xlin = lpoint.line;
    oldpline = pline;
    if (not_in_file(pline, current_file_list->file)) {
        size_t lentmp = strlen((const char *)pline) + 1;
        new_array(&expr, lentmp);
        memcpy(expr, pline, lentmp);
        pline = expr;
    } else expr = (uint8_t *)oldpline;
    new_waitfor(W_ENDWHILE2, epoint);
    waitfor->u.cmd_rept.breakout = false;
    for (;;) {
        bool truth;
        if (!get_exp(0, 1, 1, &apoint) || tobool(get_val(), &truth) || !truth) {
            waitfor->skip = skip;
            break;
        }
        if ((skip & 1) != 0) listing_line_cut(waitfor->epoint.pos);
        if (lst != NULL) {
            if (i >= lst->len && list_extend2(lst)) { i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL; }
            else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
            else nf = tuple_scope(newlabel, &lst->data[i]);
            i++;
        } else nf = compile();
        xlin = lpoint.line;
        if (nf == NULL || waitfor->u.cmd_rept.breakout) break;
        skip = waitfor->skip;
        waitfor->skip = 1;
        pline = expr;
        lpoint = apoint;
    }
    pline = oldpline;
    lpoint.line = xlin;

    if (nf != NULL) {
        if ((waitfor->skip & 1) != 0) listing_line(waitfor->epoint.pos);
        else listing_line_cut2(waitfor->epoint.pos);
        close_waitfor(W_ENDWHILE2);
    } else {
        waitfor->what = W_ENDWHILE; waitfor->skip = 0;
    }
    if (expr != oldpline) free(expr);
    s->vline = vline; star_tree = stree_old; vline = star_tree->vline + lpoint.line - apoint.line;
    return i;
}


static bool cdef_command(linepos_t epoint) {
    Obj *val;
    struct character_range_s tmp;
    Enc *oldenc = ref_enc(actual_encoding);
    bool old = oldenc->updating;
    bool rc;
    argcount_t len;
    listing_line(epoint->pos);
    oldenc->updating = true;
    rc = get_exp(0, 2, 0, epoint);
    oldenc->updating = old;
    val_destroy(Obj(actual_encoding));
    actual_encoding = oldenc;
    len = get_val_remaining();
    if (!rc) return true;
    for (;;) {
        bool endok = false;
        size_t i;
        bool tryit = true;
        uval_t uval;
        struct values_s *vs;
        linepos_t opoint;

        vs = get_val();
        if (vs == NULL) break;

        opoint = &vs->epoint;
        val = vs->val;
        if (val->obj == STR_OBJ) {
            Str *str = Str(val);
            if (str->len == 0) {err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint); return true;}
            else {
                unichar_t ch = str->data[0];
                if ((ch & 0x80) != 0) i = utf8in(str->data, &ch); else i = 1;
                tmp.start = ch;
                if (str->len > i) {
                    ch = str->data[i];
                    if ((ch & 0x80) != 0) i += utf8in(str->data + i, &ch); else i++;
                    tmp.end = ch & 0xffffff;
                    endok = true;
                    if (str->len > i) {err_msg2(ERROR_NOT_TWO_CHARS, str, &vs->epoint); return true;}
                }
            }
        } else {
            if (touval2(vs, &uval, 24)) tryit = false;
            else tmp.start = uval & 0xffffff;
        }
        if (!endok) {
            vs = get_val();
            if (vs == NULL) { err_msg_argnum(len, len + 2, 0, epoint); return true;}

            if (tryit) {
                old = actual_encoding->updating;
                actual_encoding->updating = true;
                if (touval2(vs, &uval, 24)) tryit = false;
                else tmp.end = uval & 0xffffff;
                actual_encoding->updating = old;
            }
        }
        vs = get_val();
        if (vs == NULL) { err_msg_argnum(len, len + 1, 0, epoint); return true;}
        if (tryit) {
            old = actual_encoding->updating;
            actual_encoding->updating = true;
            if (touval(vs->val, &uval, 8, &vs->epoint)) tryit = false;
            else uval &= 0xff;
            actual_encoding->updating = old;
        }
        if (tryit) {
            tmp.offset = uval & 0xff;
            if (tmp.start > tmp.end) {
                unichar_t tmpe = tmp.start;
                tmp.start = tmp.end;
                tmp.end = tmpe & 0xffffff;
            }
            if (enc_trans_add(actual_encoding, &tmp, epoint)) {
                err_msg2(ERROR__DOUBLE_RANGE, NULL, opoint); return true;
            } else {
                uval += tmp.end - tmp.start;
                if (uval > 256) {
                    err_msg_enc_large(uval - 1, &vs->epoint);
                }
            }
        }
    }
    return false;
}

static bool edef_command(linepos_t epoint) {
    Obj *val;
    Enc *oldenc = ref_enc(actual_encoding);
    bool old = oldenc->updating;
    bool rc;
    argcount_t len;
    listing_line(epoint->pos);
    oldenc->updating = true;
    rc = get_exp(0, 2, 0, epoint);
    oldenc->updating = old;
    val_destroy(Obj(actual_encoding));
    actual_encoding = oldenc;
    if (!rc) return true;
    len = get_val_remaining();
    for (;;) {
        struct values_s *vs, *vs2;
        bool tryit;
        str_t escape;

        vs = get_val();
        if (vs == NULL) break;
        tryit = !tostr(vs, &escape);

        if (tryit && (escape.len == 0 || escape.len > 1024)) {
            err_msg2(escape.len == 0 ? ERROR__EMPTY_STRING : ERROR_OUT_OF_MEMORY, NULL, &vs->epoint);
            tryit = false;
        }
        vs2 = get_val();
        if (vs2 == NULL) { err_msg_argnum(len, len + 1, 0, epoint); return true; }
        val = vs2->val;
        if (val == none_value) err_msg_still_none(NULL, &vs2->epoint);
        else if (tryit && enc_escape_add(actual_encoding, &escape, val, &vs2->epoint)) {
            err_msg2(ERROR_DOUBLE_ESCAPE, NULL, &vs->epoint); return true;
        }
    }
    return false;
}

static bool tdef_command(linepos_t epoint) {
    Obj *val;
    Enc *oldenc = ref_enc(actual_encoding);
    bool old = oldenc->updating;
    bool rc;
    argcount_t len;
    listing_line(epoint->pos);
    oldenc->updating = true;
    rc = get_exp(0, 2, 0, epoint);
    oldenc->updating = old;
    val_destroy(Obj(actual_encoding));
    actual_encoding = oldenc;
    if (!rc) return true;
    len = get_val_remaining();
    for (;;) {
        struct character_range_s tmp;
        struct iter_s iter, iter2;
        struct values_s *vs, *vs2, vs1;
        bool doublerange;
        uval_t uval = 0;

        vs = get_val();
        if (vs == NULL) break;
        val = vs->val;

        vs2 = get_val();
        if (vs2 == NULL) { err_msg_argnum(len, len + 1, 0, epoint); return true; }
        if (vs2->val->obj->iterable) {
            iter2.data = vs2->val; iter2.data->obj->getiter(&iter2);
        } else {
            bool err;
            old = actual_encoding->updating;
            actual_encoding->updating = true;
            err = touval(vs2->val, &uval, 8, &vs2->epoint);
            actual_encoding->updating = old;
            if (err) continue;
            uval &= 0xff;
            iter2.data = NULL;
        }
        doublerange = false;
        iter.data = val;
        if (val->obj->iterable || val->obj == STR_OBJ) {
            val->obj->getiter(&iter);
        } else {
            DEFAULT_OBJ->getiter(&iter);
        }
        if (iter2.data != NULL && iter.len != iter2.len) {
            Error *err = new_error(ERROR_CANT_BROADCAS, &vs->epoint);
            err->u.broadcast.v1 = iter.len;
            err->u.broadcast.v2 = iter2.len;
            err_msg_output_and_destroy(err);
        }
        vs1.epoint = vs->epoint;
        while ((vs1.val = iter.next(&iter)) != NULL) {
            uval_t uval2;
            bool ret;
            old = actual_encoding->updating;
            actual_encoding->updating = true;
            ret = touval2(&vs1, &uval2, 24);
            if (iter2.data != NULL) {
                val = iter2.next(&iter2);
                if (!ret && val != NULL) {
                    if (touval(val, &uval, 8, &vs2->epoint)) ret = true;
                    else uval &= 0xff;
                }
            }
            actual_encoding->updating = old;
            if (val == NULL) break;
            if (!ret) {
                tmp.offset = uval & 0xff;
                tmp.end = uval2 & 0xffffff;
                tmp.start = tmp.end;
                if (enc_trans_add(actual_encoding, &tmp, &vs2->epoint)) doublerange = true;
            }
            uval++;
        }
        iter_destroy(&iter);
        if (iter2.data != NULL) iter_destroy(&iter2);
        if (doublerange) {
            err_msg2(ERROR__DOUBLE_RANGE, NULL, &vs->epoint);
        } else if (uval > 256) {
            err_msg_enc_large(uval - 1, &vs2->epoint);
        }
    }
    return false;
}

static Namespace *anonlabel(Namespace *mycontext, uint8_t type, linepos_t epoint) {
    struct {
        uint8_t type;
        uint8_t padding[3];
        linenum_t vline;
        struct star_s *star_tree;
    } anonsymbol;
    Label *label;
    str_t tmpname;
    if (sizeof(anonsymbol) != sizeof(anonsymbol.type) + sizeof(anonsymbol.padding) + sizeof(anonsymbol.star_tree) + sizeof(anonsymbol.vline)) memset(&anonsymbol, 0, sizeof anonsymbol);
    else anonsymbol.padding[0] = anonsymbol.padding[1] = anonsymbol.padding[2] = 0;
    anonsymbol.type = type;
    anonsymbol.star_tree = star_tree;
    anonsymbol.vline = vline;
    tmpname.data = (const uint8_t *)&anonsymbol; tmpname.len = sizeof anonsymbol;
    label = new_label(&tmpname, mycontext, strength, current_file_list);
    if (label->value != NULL) {
        if (label->defpass == pass) err_msg_double_defined(label, &tmpname, epoint);
        else if (label->fwpass == pass) fwcount--;
        label->constant = true;
        label->owner = true;
        label->defpass = pass;
        if (label->value->obj != NAMESPACE_OBJ) {
            val_destroy(label->value);
            label->value = Obj(new_namespace(current_file_list, epoint));
        } else {
            Namespace *names = Namespace(label->value);
            names->backr = names->forwr = 0;
            names->file_list = current_file_list;
            names->epoint = *epoint;
        }
    } else {
        label->constant = true;
        label->owner = true;
        label->value = Obj(new_namespace(current_file_list, epoint));
        label->epoint = *epoint;
    }
    return Namespace(label->value);
}

static MUST_CHECK Label *new_anonlabel(Namespace *context) {
    struct {
        uint8_t type;
        uint8_t padding[3];
        linenum_t vline;
        struct star_s *star_tree;
    } anonsymbol;
    str_t tmpname;
    if (sizeof(anonsymbol) != sizeof(anonsymbol.type) + sizeof(anonsymbol.padding) + sizeof(anonsymbol.star_tree) + sizeof(anonsymbol.vline)) memset(&anonsymbol, 0, sizeof anonsymbol);
    else anonsymbol.padding[0] = anonsymbol.padding[1] = anonsymbol.padding[2] = 0;
    anonsymbol.type = '.';
    anonsymbol.star_tree = star_tree;
    anonsymbol.vline = vline;
    tmpname.data = (const uint8_t *)&anonsymbol; tmpname.len = sizeof anonsymbol;
    return new_label(&tmpname, context, strength, current_file_list);
}

MUST_CHECK Obj *compile(void)
{
    int wht;
    int prm = 0;
    Obj *val;

    Label *newlabel = NULL;
    size_t newmembp = 0;
    Label *tmp2 = NULL;
    Namespace *mycontext;
    address_t oaddr = 0;
    Obj *retval = NULL;

    size_t oldwaitforp = waitfor_p;
    bool nobreak = true;
    str_t labelname;
    struct anonsymbol_s {
        uint8_t dir, pad;
        uint8_t count[sizeof(uint32_t)];
    } anonsymbol;
    struct linepos_s epoint;

    while (nobreak) {
        if (mtranslate()) {
            if (signal_received) err_msg_signal();
            break;
        }
        newlabel = NULL;
        labelname.len = 0;ignore();epoint = lpoint; mycontext = current_context;
        if (current_address->unionmode) {
            if (current_address->l_address != current_address->l_union) {
                current_address->l_address = current_address->l_union;
                if (current_address->l_address > all_mem) {
                    err_msg_big_address(&epoint);
                    current_address->l_address &= all_mem;
                }
                current_address->bankwarn = false;
            }
            if (current_address->address != current_address->start) {
                if (!current_address->moved) {
                    if (current_address->address > current_address->end) current_address->end = current_address->address;
                    current_address->moved = true;
                }
                current_address->wrapwarn = false;
                current_address->address = current_address->start;
                memjmp(current_address->mem, current_address->address);
            }
        }
        star = current_address->l_address;
        wht = here();
        if (wht >= 'A') {
            labelname.data = pline + lpoint.pos;
            labelname.len = get_label(labelname.data);
            if (labelname.len != 0) {
                struct linepos_s cmdpoint;
                bool islabel, tcontext, error;
                lpoint.pos += (linecpos_t)labelname.len;
                islabel = false; error = (waitfor->skip & 1) == 0;
                while (here() == '.' && pline[lpoint.pos+1] != '.') {
                    if (!error) {
                        if (!islabel) {
                            if (labelname.data[0] != '_') {
                                tmp2 = find_label(&labelname, NULL);
                                if (tmp2 == NULL) {err_msg_not_defined2(&labelname, mycontext, true, &epoint); error = true;}
                            } else {
                                tmp2 = find_label2(&labelname, cheap_context);
                                if (tmp2 == NULL) {err_msg_not_defined2(&labelname, cheap_context, false, &epoint); error = true;}
                            }
                        } else {
                            tmp2 = find_label2(&labelname, mycontext);
                            if (tmp2 == NULL) {err_msg_not_defined2(&labelname, mycontext, false, &epoint); error = true;}
                        }
                        if (tmp2 != NULL) {
                            if (diagnostics.case_symbol && str_cmp(&labelname, &tmp2->name) != 0) err_msg_symbol_case(&labelname, tmp2, &epoint);
                            tmp2->usepass = pass; /* touch_label(tmp2) */
                        }
                    }
                    lpoint.pos++; islabel = true; epoint = lpoint;
                    labelname.data = pline + lpoint.pos; labelname.len = get_label(labelname.data);
                    if (labelname.len == 0) {
                        if (!error) err_msg2(ERROR______EXPECTED, "a symbol is", &lpoint);
                        goto breakerr;
                    }
                    lpoint.pos += (linecpos_t)labelname.len;
                    if (!error) {
                        Namespace *context = get_namespace(tmp2->value);
                        if (context == NULL) {
                            epoint.pos--;
                            if (tmp2->value == none_value) err_msg_still_none(NULL, &epoint);
                            else if (tmp2->value->obj == ERROR_OBJ) err_msg_output(Error(tmp2->value));
                            else {
                                Error *err = new_error(ERROR__INVALID_OPER, &epoint);
                                err->u.invoper.op = O_MEMBER;
                                err->u.invoper.v1 = val_reference(tmp2->value);
                                err->u.invoper.v2 = new_symbol(&labelname, &epoint);
                                err_msg_output_and_destroy(err);
                            }
                            error = true;
                        } else mycontext = context;
                    }
                }
                if (!islabel && labelname.data[0] == '_') {
                    tcontext = true;
                    mycontext = cheap_context;
                } else tcontext = islabel;
                if (here() == ':' && pline[lpoint.pos + 1] != '=') {islabel = true; lpoint.pos++;}
                if (!islabel && labelname.len == 3 && (prm = lookup_opcode(labelname.data)) >=0) {
                    if (!error) goto as_opcode; else continue;
                }
                if (false) {
                hh: islabel = true; tcontext = false; error = (waitfor->skip & 1) == 0;
                }
                ignore();wht = here();
                if (error) {epoint = lpoint; goto jn;} /* skip things if needed */
                if (labelname.len > 1 && labelname.data[0] == '_' && labelname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint); goto breakerr;}
                while (wht != 0 && !arguments.tasmcomp) {
                    bool error2;
                    bool minmax;
                    Label *label;
                    struct oper_s tmp;
                    Obj *result2, *val2;
                    struct linepos_s epoint2, epoint3;
                    int wht2 = pline[lpoint.pos + 1];

                    if (wht2 == '=') {
                        Oper_types op;
                        if (wht == ':') {
                            if (labelname.data[0] == '*') {
                                lpoint.pos++;
                                goto starassign;
                            }
                            lpoint.pos += 2;
                            ignore();
                            goto itsvar;
                        }
                        op = oper_from_token(wht);
                        if (op == O_NONE) break;
                        tmp.op = op;
                        epoint3 = lpoint;
                        lpoint.pos += 2;
                    } else if (wht2 != 0 && pline[lpoint.pos + 2] == '=') {
                        Oper_types op = oper_from_token2(wht, wht2);
                        if (op == O_NONE) break;
                        tmp.op = op;
                        epoint3 = lpoint;
                        lpoint.pos += 3;
                    } else break;

                    ignore();
                    epoint2 = lpoint;
                    if (labelname.data[0] == '*') {
                        label = NULL;
                        if (diagnostics.optimize) cpu_opt_invalidate();
                        val = get_star();
                    } else if (tmp.op == O_COND) {
                        label = NULL; val = NULL;
                    } else {
                        if (labelname.data == (const uint8_t *)&anonsymbol) {
                            ssize_t cnt;
                            if (anonsymbol.dir == '-') {
                                cnt = -1; current_context->backr--;
                            } else {
                                cnt = 0;  current_context->forwr--;
                            }
                            label = tcontext ? find_anonlabel2(cnt, mycontext) : find_anonlabel(cnt);
                        } else {
                            label = tcontext ? find_label2(&labelname, mycontext) : find_label(&labelname, NULL);
                        }
                        if (tmp.op == O_MUL && !islabel && (label == NULL || label->constant)) {
                            if (diagnostics.star_assign) {
                                err_msg_star_assign(&epoint3);
                                if (pline[lpoint.pos] == '*') err_msg_compound_note(&epoint3);
                            }
                            lpoint.pos = epoint3.pos;
                            wht = '*';
                            break;
                        }
                        if (label == NULL) {
                            if (labelname.data == (const uint8_t *)&anonsymbol) {
                                err_msg_not_defined2a((anonsymbol.dir == '-') ? -1 : 0, mycontext, !tcontext, &epoint);
                            } else {
                                err_msg_not_defined2(&labelname, mycontext, !tcontext, &epoint);
                            }
                            goto breakerr;
                        }
                        if (label->constant) {
                            err_msg_not_variable(label, &labelname, &epoint);
                            goto breakerr;
                        }
                        if (diagnostics.case_symbol && str_cmp(&labelname, &label->name) != 0) err_msg_symbol_case(&labelname, label, &epoint);
                        val = label->value;
                        label->usepass = 0;
                    }
                    if (here() == 0 || here() == ';') {
                        err_msg(ERROR______EXPECTED, "an expression is");
                        val2 = ref_none();
                        error2 = true;
                    } else {
                        bool oldreferenceit = referenceit;
                        referenceit &= 1; /* not good... */
                        error2 = !get_exp(0, 1, 0, &epoint2);
                        val2 = error2 ? ref_none() : get_vals_tuple();
                        referenceit = oldreferenceit;
                    }
                    if (val == NULL) {
                        bool labelexists = false;
                        if (labelname.data == (const uint8_t *)&anonsymbol) {
                            str_t labelname2;
                            struct anonsymbol_s anonsymbol3;
                            uint32_t count = (anonsymbol.dir == '-') ? current_context->backr :  current_context->forwr;
                            count -= 2;
                            anonsymbol3.dir = anonsymbol.dir;
                            anonsymbol3.pad = 0;
                            labelname2.len = 2;
                            while (count != 0) {
                                anonsymbol3.count[labelname2.len - 2] = (uint8_t)count;
                                labelname2.len++;
                                count >>= 8;
                            }
                            labelname2.data = (const uint8_t *)&anonsymbol3;
                            label = find_label3(&labelname2, mycontext, strength);
                            if (label != NULL) {
                                if (anonsymbol.dir == '-') {
                                    current_context->backr--;
                                } else {
                                    current_context->forwr--;
                                }
                                labelexists = true;
                            }
                        }
                        if (!labelexists) {
                            label = new_label(&labelname, mycontext, strength, current_file_list);
                            labelexists = label->value != NULL;
                        }
                        if (labelexists) {
                            if (label->constant) {
                                err_msg_not_variable(label, &labelname, &epoint);
                                val_destroy(val2);
                            } else if (label->defpass == pass) {
                                val_destroy(val2);
                            } else {
                                label->ref = false;
                                label->defpass = pass;
                                label->owner = false;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                val_destroy(label->value);
                                label->value = val2;
                                label->usepass = 0;
                            }
                        } else {
                            label->constant = false;
                            label->owner = false;
                            label->value = val2;
                            label->epoint = epoint;
                        }
                        if (error2) goto breakerr;
                        goto finish;
                    }
                    if (tmp.op == O_REASSIGN) {
                        result2 = val2;
                    } else {
                        minmax = (tmp.op == O_MIN) || (tmp.op == O_MAX);
                        tmp.v1 = val;
                        tmp.v2 = val2;
                        tmp.epoint = &epoint;
                        tmp.epoint2 = &epoint2;
                        tmp.epoint3 = &epoint3;
                        tmp.inplace = (tmp.v1->refcount == 1 && !minmax) ? tmp.v1 : NULL;
                        result2 = tmp.v1->obj->calc2(&tmp);
                        if (minmax) {
                            if (result2 == true_value) val_replace(&result2, val);
                            else if (result2 == false_value) val_replace(&result2, val2);
                        }
                        val_destroy(val2);
                    }
                    if (label != NULL) {
                        listing_equal(result2);
                        if (label->file_list != current_file_list) {
                            label_move(label, &labelname, current_file_list);
                        }
                        label->epoint = epoint;
                        val_destroy(label->value);
                        label->value = result2;
                    } else {
                        val_destroy(val);
                        if (error2) {
                            val_destroy(result2);
                            goto breakerr;
                        }
                        starhandle(result2, &epoint, &epoint2);
                    }
                    if (error2) goto breakerr;
                    goto finish;
                }
                switch (wht) {
                case '=':
                    { /* variable */
                        bool error2;
                        struct linepos_s opoint;
                        Label *label;
                    starassign:
                        if (labelname.data[0] == '*') {
                            label = NULL;
                            if (diagnostics.optimize) cpu_opt_invalidate();
                        } else label = find_label3(&labelname, mycontext, strength);
                        lpoint.pos++; ignore();
                        opoint = lpoint; /* for no elements! */
                        if (here() == 0 || here() == ';') {
                            err_msg(ERROR______EXPECTED, "an expression is");
                            val = ref_none();
                            error2 = true;
                        } else {
                            bool oldreferenceit = referenceit;
                            if (label != NULL && !label->ref) {
                                referenceit = false;
                            }
                            error2 = !get_exp(0, 1, 0, &opoint);
                            val = error2 ? ref_none() : get_vals_tuple();
                            referenceit = oldreferenceit;
                        }
                        if (labelname.data[0] == '*') {
                            if (error2) {
                                val_destroy(val);
                                goto breakerr;
                            }
                            starhandle(val, &epoint, &opoint);
                            goto finish;
                        }
                        listing_equal(val);
                        if (label == NULL) label = new_label(&labelname, mycontext, strength, current_file_list);
                        if (label->value != NULL) {
                            if (label->defpass == pass) {
                                val_destroy(val);
                                err_msg_double_defined(label, &labelname, &epoint);
                            } else {
                                if (label->fwpass == pass) fwcount--;
                                if (!constcreated && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = false;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                label->ref = false;
                                const_assign(label, val);
                            }
                        } else {
                            if (!constcreated) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = false;
                            label->value = val;
                            label->epoint = epoint;
                            label->ref = false;
                        }
                        if (error2) goto breakerr;
                        goto finish;
                    }
                case '.':
                    cmdpoint = lpoint;
                    prm = get_command();
                    ignore();
                    if (labelname.data[0] == '*') {
                        err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint);
                        newlabel = NULL; epoint = cmdpoint; goto as_command;
                    }
                    switch (prm) {
                    case CMD_VAR: /* variable */
                        {
                            bool error2;
                            Label *label;
                        itsvar:
                            label = find_label3(&labelname, mycontext, strength);
                            if (here() == 0 || here() == ';') {
                                err_msg(ERROR______EXPECTED, "an expression is");
                                val = ref_none();
                                error2 = true;
                            } else {
                                bool oldreferenceit = referenceit;
                                referenceit &= 1; /* not good... */
                                cmdpoint = lpoint;
                                error2 = !get_exp(0, 1, 0, &cmdpoint);
                                val = error2 ? ref_none() : get_vals_tuple();
                                referenceit = oldreferenceit;
                            }
                            listing_equal(val);
                            if (label == NULL) label = new_label(&labelname, mycontext, strength, current_file_list);
                            else if (diagnostics.case_symbol && str_cmp(&labelname, &label->name) != 0) err_msg_symbol_case(&labelname, label, &epoint);
                            if (label->value != NULL) {
                                if (label->constant) {
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    val_destroy(val);
                                } else {
                                    if (label->defpass != pass) {
                                        label->ref = false;
                                        label->defpass = pass;
                                    } else {
                                        if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                                    }
                                    label->owner = false;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    val_destroy(label->value);
                                    label->value = val;
                                    label->usepass = 0;
                                }
                            } else {
                                label->constant = false;
                                label->owner = false;
                                label->value = val;
                                label->epoint = epoint;
                            }
                            if (error2) goto breakerr;
                            goto finish;
                        }
                    case CMD_LBL:
                        { /* label */
                            Lbl *lbl;
                            Label *label = new_label(&labelname, mycontext, strength, current_file_list);
                            listing_line(0);
                            lbl = Lbl(val_alloc(LBL_OBJ));
                            lbl->sline = epoint.line;
                            lbl->pass = pass;
                            lbl->waitforp = waitfor_p;
                            lbl->file_list = current_file_list;
                            lbl->parent = current_context;
                            if (label->value != NULL) {
                                if (label->defpass == pass) {
                                    val_destroy(Obj(lbl));
                                    err_msg_double_defined(label, &labelname, &epoint);
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->constant = true;
                                    label->owner = true;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    label->ref = false;
                                    const_assign(label, Obj(lbl));
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                label->value = Obj(lbl);
                                label->epoint = epoint;
                                label->ref = false;
                            }
                            if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &cmdpoint);
                            goto finish;
                        }
                    case CMD_NAMESPACE:
                        { /* namespace */
                            Label *label = new_label(&labelname, mycontext, strength, current_file_list);
                            bool labelexists = label->value != NULL;
                            if (labelexists) {
                                if (label->defpass == pass) {
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    epoint = cmdpoint;
                                    goto as_command;
                                }
                                if (label->fwpass == pass) fwcount--;
                                if (!constcreated && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->owner = true;
                                label->value = none_value;
                            }
                            label->constant = true;
                            label->epoint = epoint;
                            label->ref = false;
                            listing_line(0);
                            new_waitfor(W_ENDN, &cmdpoint);
                            if (get_exp(0, 0, 1, &cmdpoint)) {
                                struct values_s *vs = get_val();
                                if (vs != NULL) {
                                    val = vs->val;
                                    val = Obj(get_namespace(val));
                                    if (val == NULL) err_msg_invalid_namespace_conv(vs);
                                } else val = NULL;
                            } else val = NULL;
                            label->owner = (val == NULL);
                            if (labelexists) {
                                if (val != NULL) const_assign(label, val_reference(val));
                                else {
                                    label->defpass = pass;
                                    if (label->value->obj != NAMESPACE_OBJ) {
                                        val_destroy(label->value);
                                        label->value = Obj(new_namespace(current_file_list, &epoint));
                                    } else {
                                        Namespace *names = Namespace(label->value);
                                        names->backr = names->forwr = 0;
                                        names->file_list = current_file_list;
                                        names->epoint = epoint;
                                    }
                                }
                            } else {
                                label->value = (val != NULL) ? val_reference(val) : Obj(new_namespace(current_file_list, &epoint));
                            }
                            if (label->value->obj == NAMESPACE_OBJ) {
                                push_context(Namespace(label->value));
                                waitfor->what = W_ENDN2;
                            } else push_context(current_context);
                            goto finish;
                        }
                    case CMD_ENCODE:
                        { /* encode */
                            Label *label = new_label(&labelname, mycontext, strength, current_file_list);
                            bool labelexists = label->value != NULL;
                            if (labelexists) {
                                if (label->defpass == pass) {
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    epoint = cmdpoint;
                                    goto as_command;
                                }
                                if (label->fwpass == pass) fwcount--;
                                if (!constcreated && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->owner = true;
                                label->value = none_value;
                            }
                            label->constant = true;
                            label->epoint = epoint;
                            label->ref = false;
                            listing_line(0);
                            new_waitfor(W_ENDENCODE, &cmdpoint);
                            if (get_exp(0, 0, 1, &cmdpoint)) {
                                struct values_s *vs = get_val();
                                if (vs != NULL) {
                                    val = vs->val;
                                    if (val->obj != ENC_OBJ) {
                                        val = NULL;
                                        err_msg_wrong_type2(vs->val, ENC_OBJ, &vs->epoint);
                                    }
                                } else val = NULL;
                            } else val = NULL;
                            label->owner = (val == NULL);
                            if (labelexists) {
                                if (val != NULL) const_assign(label, val_reference(val));
                                else {
                                    label->defpass = pass;
                                    if (label->value->obj != ENC_OBJ) {
                                        val_destroy(label->value);
                                        label->value = new_enc(current_file_list, &epoint);
                                    } else {
                                        Enc *enc = Enc(label->value);
                                        enc->file_list = current_file_list;
                                        enc->epoint = epoint;
                                    }
                                }
                            } else {
                                label->value = (val != NULL) ? val_reference(val) : new_enc(current_file_list, &epoint);
                            }
                            if (label->value->obj == ENC_OBJ) {
                                waitfor->u.cmd_encode.enc = actual_encoding;
                                actual_encoding = Enc(val_reference(label->value));
                                waitfor->what = W_ENDENCODE2;
                            } else {
                                waitfor->u.cmd_encode.enc = NULL;
                            }
                            goto finish;
                        }
                    case CMD_MACRO:/* .macro */
                    case CMD_SEGMENT:
                        {
                            Label *label;
                            Macro *macro;
                            Type *obj = (prm == CMD_MACRO) ? MACRO_OBJ : SEGMENT_OBJ;
                            listing_line(0);
                            new_waitfor(prm == CMD_MACRO ? W_ENDMACRO : W_ENDSEGMENT, &cmdpoint);waitfor->skip = 0;
                            label = new_label(&labelname, mycontext, strength, current_file_list);
                            macro = Macro(val_alloc(obj));
                            macro->file_list = current_file_list;
                            macro->line = epoint.line;
                            macro->recursion_pass = 0;
                            if (label->value != NULL && label->value->obj == obj) {
                                macro->retval = Macro(label->value)->retval;
                                macro->argc = Macro(label->value)->argc;
                            } else {
                                macro->retval = false;
                                macro->argc = 0;
                            }
                            get_macro_params(Obj(macro));
                            if (label->value != NULL) {
                                if (label->defpass == pass) {
                                    waitfor->u.cmd_macro.val = Obj(macro);
                                    err_msg_double_defined(label, &labelname, &epoint);
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->constant = true;
                                    label->owner = true;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    label->ref = false;
                                    const_assign(label, Obj(macro));
                                    waitfor->u.cmd_macro.val = val_reference(label->value);
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                label->value = Obj(macro);
                                label->epoint = epoint;
                                label->ref = false;
                                waitfor->u.cmd_macro.val = val_reference(Obj(macro));
                            }
                            goto finish;
                        }
                    case CMD_SFUNCTION:
                    case CMD_FUNCTION:
                        {
                            Label *label;
                            Mfunc *mfunc;
                            bool failed;
                            Type *obj = (prm == CMD_FUNCTION) ? MFUNC_OBJ : SFUNC_OBJ;
                            listing_line(0);
                            if (prm == CMD_FUNCTION) new_waitfor(W_ENDF, &cmdpoint);
                            label = new_label(&labelname, mycontext, strength, current_file_list);
                            mfunc = Mfunc(val_alloc(obj));
                            mfunc->file_list = current_file_list;
                            mfunc->epoint.line = epoint.line;
                            mfunc->epoint.pos = 0;
                            mfunc->recursion_pass = 0;
                            mfunc->argc = 0;
                            mfunc->param = NULL; /* might be recursive through init */
                            mfunc->nslen = 0;
                            mfunc->namespaces = NULL;
                            mfunc->ipoint = 0;
                            if (prm == CMD_SFUNCTION && not_in_file(pline, current_file_list->file)) {
                                size_t ln = strlen((const char *)pline) + 1;
                                uint8_t *l = allocate_array(uint8_t, ln);
                                if (l != NULL) memcpy(l, pline, ln);
                                mfunc->line = l;
                            } else mfunc->line = NULL;
                            if (label->value != NULL) {
                                mfunc->retval = (label->value->obj == obj) && Mfunc(label->value)->retval;
                                if (label->defpass == pass) {
                                    mfunc->names = new_namespace(current_file_list, &epoint);
                                    mfunc->inamespaces = Tuple(val_reference(null_tuple));
                                    if (prm == CMD_FUNCTION) waitfor->u.cmd_function.val = Obj(mfunc); else val_destroy(Obj(mfunc));
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    failed = true;
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->constant = true;
                                    label->owner = true;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    if (label->value->obj == obj) {
                                        Mfunc *prev = Mfunc(label->value);
                                        mfunc->names = ref_namespace(prev->names);
                                        mfunc->names->backr = mfunc->names->forwr = 0;
                                        mfunc->names->file_list = current_file_list;
                                        mfunc->names->epoint = epoint;
                                        mfunc->inamespaces = Tuple(val_reference(Obj(prev->inamespaces)));
                                        mfunc->argc = prev->argc;
                                    } else {
                                        mfunc->names = new_namespace(current_file_list, &epoint);
                                        mfunc->inamespaces = Tuple(val_reference(null_tuple));
                                    }
                                    label->epoint = epoint;
                                    label->ref = false;
                                    get_namespaces(mfunc);
                                    failed = get_func_params(mfunc, prm == CMD_SFUNCTION);
                                    const_assign(label, Obj(mfunc));
                                    if (label->value != Obj(mfunc)) Mfunc(label->value)->ipoint = 0;
                                    if (prm == CMD_FUNCTION) waitfor->u.cmd_function.val = val_reference(label->value);
                                }
                            } else {
                                mfunc->retval = false;
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                label->value = Obj(mfunc);
                                mfunc->inamespaces = Tuple(val_reference(null_tuple));
                                label->epoint = epoint;
                                label->ref = false;
                                get_namespaces(mfunc);
                                mfunc->names = new_namespace(current_file_list, &epoint);
                                if (prm == CMD_FUNCTION) waitfor->u.cmd_function.val = val_reference(Obj(mfunc));
                                failed = get_func_params(mfunc, prm == CMD_SFUNCTION);
                            }
                            if (prm == CMD_FUNCTION) {
                                if (!failed && here() != 0 && here() != ';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
                                waitfor->skip = 0;
                            }
                            goto breakerr;
                        }
                    case CMD_STRUCT:
                    case CMD_UNION:
                        {
                            Label *label;
                            Struct *structure;
                            struct section_address_s section_address, *oldsection_address = current_address;
                            uval_t provides = current_section->provides, requires = current_section->requires, conflicts = current_section->conflicts;
                            bool doubledef = false;
                            Type *obj = (prm == CMD_STRUCT) ? STRUCT_OBJ : UNION_OBJ;
                            listing_line(0);
                            if (diagnostics.optimize) cpu_opt_invalidate();
                            new_waitfor((prm==CMD_STRUCT) ? W_ENDS : W_ENDU, &cmdpoint);waitfor->skip = 0;
                            label = new_label(&labelname, mycontext, strength, current_file_list);

                            current_section->provides = ~(uval_t)0;current_section->requires = current_section->conflicts = 0;
                            section_address.wrapwarn = section_address.moved = false;
                            section_address.bankwarn = false;
                            section_address.unionmode = (prm == CMD_UNION);
                            section_address.address = section_address.start = section_address.end = 0;
                            section_address.l_start = 0;
                            section_address.l_union = 0;
                            section_address.l_address = 0;
                            section_address.l_address_val = val_reference(int_value[0]);
                            section_address.mem = new_memblocks(0, 0);
                            section_address.mem->lastaddr = 0;
                            section_address.mem->enumeration = true;
                            current_address = &section_address;

                            structure = Struct(val_alloc(obj));
                            structure->file_list = current_file_list;
                            structure->line = epoint.line;
                            structure->recursion_pass = 0;
                            if (label->value != NULL && label->value->obj == obj) {
                                structure->retval = Struct(label->value)->retval;
                                structure->argc = Struct(label->value)->argc;
                            } else {
                                structure->retval = false;
                                structure->argc = 0;
                            }
                            get_macro_params(Obj(structure));
                            if (label->value != NULL) {
                                if (label->defpass == pass) {
                                    doubledef = true;
                                    structure->size = 0;
                                    structure->names = new_namespace(current_file_list, &epoint);
                                    err_msg_double_defined(label, &labelname, &epoint);
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->constant = true;
                                    label->owner = true;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    label->ref = false;
                                    if (label->value->obj == obj) {
                                        Struct *prev = Struct(label->value);
                                        structure->size = prev->size;
                                        structure->names = ref_namespace(prev->names);
                                        structure->names->backr = structure->names->forwr = 0;
                                        structure->names->file_list = current_file_list;
                                        structure->names->epoint = epoint;
                                    } else {
                                        structure->size = 0;
                                        structure->names = new_namespace(current_file_list, &epoint);
                                    }
                                    const_assign(label, Obj(structure));
                                    structure = Struct(label->value);
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                label->value = Obj(structure);
                                label->epoint = epoint;
                                label->ref = false;
                                structure->size = 0;
                                structure->names = new_namespace(current_file_list, &epoint);
                            }
                            waitfor->what = (prm == CMD_STRUCT) ? W_ENDS2 : W_ENDU2;
                            waitfor->skip = 1;
                            val = macro_recurse(W_ENDS, Obj(structure), structure->names, &cmdpoint);
                            structure->retval = (val != NULL);
                            if (val != NULL) val_destroy(val);
                            waitfor->what = (prm == CMD_STRUCT) ? W_ENDS : W_ENDU;
                            waitfor->skip = 0;
                            lpoint.line--; vline--;

                            current_section->provides = provides; current_section->requires = requires; current_section->conflicts = conflicts;
                            current_address = oldsection_address;
                            if (current_address->l_address > all_mem) {
                                err_msg_big_address(&cmdpoint);
                                current_address->l_address &= all_mem;
                            }

                            if (doubledef) val_destroy(Obj(structure));
                            else {
                                address_t end = (section_address.end < section_address.address) ? section_address.address : section_address.end;
                                if (structure->size != (end & all_mem2)) {
                                    structure->size = end & all_mem2;
                                    if (label->usepass >= pass) {
                                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &label->epoint);
                                        fixeddig = false;
                                    }
                                }
                            }
                            val_destroy(section_address.l_address_val);
                            val_destroy(Obj(section_address.mem));
                            goto breakerr;
                        }
                    case CMD_SECTION:
                        if (section_start(&cmdpoint)) goto breakerr;
                        star = current_address->l_address;
                        break;
                    case CMD_VIRTUAL:
                        if (virtual_start(&cmdpoint)) goto breakerr;
                        star = current_address->l_address;
                        break;
                    case CMD_BREPT:
                    case CMD_BFOR:
                    case CMD_BWHILE:
                        { /* .bfor */
                            List *lst;
                            size_t i;
                            Label *label = new_label(&labelname, mycontext, strength, current_file_list);
                            if (label->value != NULL) {
                                if (label->defpass == pass) {
                                    err_msg_double_defined(label, &labelname, &epoint);
                                    epoint = cmdpoint;
                                    goto as_command;
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->defpass = pass;
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->value = ref_none();
                            }
                            label->constant = true;
                            label->owner = true;
                            label->epoint = epoint;
                            label->ref = false;
                            if (label->value->obj == TUPLE_OBJ) {
                                Tuple *old = Tuple(label->value);
                                lst = new_tuple(old->len);
                                for (i = 0; i < old->len; i++) lst->data[i] = val_reference(old->data[i]);
                            } else {
                                lst = new_tuple(lenof(lst->u.val));
                                for (i = 0; i < lst->len; i++) lst->data[i] = ref_none();
                            }
                            label = ref_label(label);
                            i = (prm == CMD_BFOR) ? for_command(label, lst, &cmdpoint) : (prm == CMD_BREPT) ? rept_command(label, lst, &cmdpoint) : while_command(label, lst, &cmdpoint);
                            if (lst->len > i) list_shrink(lst, i);
                            const_assign(label, Obj(lst));
                            val_destroy(Obj(label));
                            goto breakerr;
                        }
                    case CMD_DSTRUCT: /* .dstruct */
                    case CMD_DUNION:
                        {
                            address_t oldstart, oldend;
                            address_t oldl_start, oldl_union;
                            bool oldunionmode;
                            bool ret, doubledef = false;
                            Type *obj;
                            Namespace *context;
                            Label *label = new_label(&labelname, mycontext, strength, current_file_list);
                            bool labelexists = label->value != NULL;
                            if (labelexists) {
                                if (label->defpass == pass) {
                                    doubledef = true;
                                    err_msg_double_defined(label, &labelname, &epoint);
                                } else {
                                    if (label->fwpass == pass) fwcount--;
                                    if (!constcreated && label->defpass != pass - 1) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->constant = true;
                                    label->owner = true;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    label->ref = false;
                                }
                            } else {
                                if (!constcreated) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->value = ref_none();
                                label->constant = true;
                                label->owner = true;
                                label->epoint = epoint;
                                label->ref = false;
                            }

                            if (diagnostics.optimize) cpu_opt_invalidate();
                            listing_line(cmdpoint.pos);
                            if (get_exp(1, 1, 0, &cmdpoint)) {
                                struct values_s *vs = get_val();
                                val = vs->val;
                                obj = (prm == CMD_DSTRUCT) ? STRUCT_OBJ : UNION_OBJ;
                                if (val->obj != obj) {
                                    err_msg_wrong_type2(val, obj, &vs->epoint);
                                    val = NULL;
                                }
                            } else val = NULL;
                            ret = (val != NULL) && Struct(val)->retval;
                            if (here() == ',') lpoint.pos++;

                            oaddr = current_address->address;
                            newmembp = get_mem(current_address->mem);
                            oldstart = current_address->start;
                            oldend = current_address->end;
                            oldl_start = current_address->l_start;
                            oldl_union = current_address->l_union;
                            oldunionmode = current_address->unionmode;
                            current_address->start = current_address->end = current_address->address;
                            current_address->l_start = current_address->l_address;
                            current_address->l_union = current_address->l_address;
                            current_address->unionmode = (prm == CMD_DUNION);

                            if (!ret && !doubledef) {
                                Code *code = Code(label->value);
                                if (labelexists && code->v.obj == CODE_OBJ) {
                                    update_code(label, code);
                                    label->defpass = pass;
                                } else {
                                    val_destroy(Obj(code));
                                    code = create_code(&epoint);
                                    label->value = Obj(code);
                                }
                                context = code->names;
                            } else {
                                context = anonlabel(mycontext, '#', &epoint);
                            }
                            label = ref_label(label);
                            if (val != NULL) {
                                val = macro_recurse((prm == CMD_DSTRUCT) ? W_ENDS3 : W_ENDU3, val, context, &cmdpoint);
                                if (val != NULL) {
                                    if (ret && !doubledef) const_assign(label, val);
                                    else val_destroy(val);
                                }
                            }

                            if (!ret && !doubledef) set_size(label, ((current_address->end < current_address->address) ? current_address->address : current_address->end) - oaddr, current_address->mem, oaddr, newmembp);
                            current_address->start = oldstart;
                            oldstart = current_address->unionmode ? current_address->end : current_address->address;
                            if (oldend > current_address->end) current_address->end = oldend;
                            current_address->l_start = oldl_start;
                            current_address->l_union = oldl_union;
                            current_address->unionmode = oldunionmode;
                            if (oldstart > current_address->address) {
                                memskip(oldstart - current_address->address, &epoint);
                            }
                            val_destroy(Obj(label));
                            goto breakerr;
                        }
                    case CMD_FROM: /* .from */
                        {
                            bool error2;
                            bool constant = true;
                            bool oldreferenceit = referenceit;
                            Label *label = find_label3(&labelname, mycontext, strength);
                            if (label != NULL && !label->ref) {
                                referenceit = false;
                            }
                            error2 = !get_exp(0, 1, 1, &cmdpoint);
                            if (error2) val = ref_none();
                            else {
                                const struct values_s *vs = get_val();
                                Namespace *context = get_namespace(vs->val);
                                if (context == NULL) {
                                    if (vs->val == none_value || vs->val->obj == ERROR_OBJ) {
                                        val = val_reference(vs->val);
                                    } else {
                                        val = new_error_conv(vs->val, NAMESPACE_OBJ, &vs->epoint);
                                    }
                                } else {
                                    Label *l;
                                    if (labelname.data == (const uint8_t *)&anonsymbol) {
                                        l = find_anonlabel2((anonsymbol.dir == '-') ? -1 : 0, context);
                                    } else {
                                        l = find_label2(&labelname, context);
                                    }
                                    if (l != NULL) {
                                        if (labelname.data != (const uint8_t *)&anonsymbol) {
                                            if (diagnostics.case_symbol && str_cmp(&labelname, &l->name) != 0) err_msg_symbol_case(&labelname, l, &epoint);
                                        }
                                        touch_label(l);
                                        val = val_reference(l->value);
                                        constant = l->constant;
                                    } else if (constcreated && pass < max_pass) {
                                        val = ref_none();
                                    } else {
                                        Error *err = new_error(ERROR___NOT_DEFINED, &epoint);
                                        if (labelname.data == (const uint8_t *)&anonsymbol) {
                                            err->u.notdef.symbol = new_anonsymbol((anonsymbol.dir == '-') ? -1 : 0);
                                        } else {
                                            err->u.notdef.symbol = new_symbol(&labelname, &epoint);
                                        }
                                        err->u.notdef.names = ref_namespace(context);
                                        err->u.notdef.down = false;
                                        val = Obj(err);
                                    }
                                }
                            }
                            referenceit = oldreferenceit;
                            listing_equal(val);
                            if (label == NULL) label = new_label(&labelname, mycontext, strength, current_file_list);
                            if (label->value != NULL) {
                                if (constant ? (label->defpass == pass) : label->constant) {
                                    val_destroy(val);
                                    err_msg_double_defined(label, &labelname, &epoint);
                                } else {
                                    if (constant) {
                                        if (label->fwpass == pass) fwcount--;
                                        if (!constcreated && label->defpass != pass - 1) {
                                            if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                            constcreated = true;
                                        }
                                        label->constant = true;
                                    } else {
                                        if (label->defpass != pass) {
                                            label->ref = false;
                                            label->defpass = pass;
                                        } else {
                                            if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                                        }
                                    }
                                    label->owner = false;
                                    if (label->file_list != current_file_list) {
                                        label_move(label, &labelname, current_file_list);
                                    }
                                    label->epoint = epoint;
                                    if (constant) {
                                        label->ref = false;
                                        const_assign(label, val);
                                    } else {
                                        val_destroy(label->value);
                                        label->value = val;
                                        label->usepass = 0;
                                    }
                                }
                            } else {
                                label->constant = constant;
                                label->owner = false;
                                label->value = val;
                                label->epoint = epoint;
                                if (constant) {
                                    if (!constcreated) {
                                        if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        constcreated = true;
                                    }
                                    label->ref = false;
                                }
                            }
                            if (error2) goto breakerr;
                            goto finish;
                        }
                    }
                    islabel = true;
                    break;
                }
                {
                    bool labelexists = false;
                    if (labelname.data[0] == '*') {
                        err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint);
                        newlabel = NULL;
                        epoint = lpoint;
                        goto jn;
                    }
                    if (!islabel) {
                        Namespace *parent;
                        bool down = (labelname.data[0] != '_');
                        if (down) {
                            parent = mycontext;
                            tmp2 = find_label(&labelname, &parent);
                        } else {
                            parent = cheap_context;
                            tmp2 = find_label2(&labelname, cheap_context);
                        }
                        if (tmp2 != NULL) {
                            const Type *obj = tmp2->value->obj;
                            if (diagnostics.case_symbol && str_cmp(&labelname, &tmp2->name) != 0) err_msg_symbol_case(&labelname, tmp2, &epoint);
                            if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                                if (diagnostics.macro_prefix && (here() == 0 || here() == ';')) err_msg_macro_prefix(&epoint);
                                touch_label(tmp2);
                                labelname.len = 0;val = tmp2->value; goto as_macro;
                            }
                            if (parent == mycontext && tmp2->strength == strength) {
                                newlabel = tmp2;
                                labelexists = true;
                            }
                        }
                    }
                    if (!labelexists) {
                        newlabel = new_label(&labelname, mycontext, strength, current_file_list);
                        labelexists = newlabel->value != NULL;
                    }
                    oaddr = current_address->address;
                    ref_label(newlabel);
                    if (labelexists) {
                        if (newlabel->defpass == pass) {
                            err_msg_double_defined(newlabel, &labelname, &epoint);
                            val_destroy(Obj(newlabel));
                            newlabel = NULL;
                            if (wht == '.') {
                                epoint = cmdpoint;
                                goto as_command;
                            }
                            epoint = lpoint;
                            goto jn;
                        }
                        if (!constcreated && newlabel->defpass != pass - 1) {
                            if (pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                            constcreated = true;
                        }
                        if (newlabel->fwpass == pass) fwcount--;
                        if (newlabel->file_list != current_file_list) {
                            label_move(newlabel, &labelname, current_file_list);
                        }
                        if (!newlabel->update_after && newlabel->value->obj != CODE_OBJ) {
                            val_destroy(newlabel->value);
                            labelexists = false;
                            newlabel->defpass = pass;
                        }
                    } else if (!constcreated) {
                        if (pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                        constcreated = true;
                    }
                    if (labelexists) {
                        newlabel->constant = true;
                        newlabel->owner = true;
                        newlabel->epoint = epoint;
                        if (newlabel->update_after) {
                            newlabel->update_after = false;
                        } else {
                            if (diagnostics.optimize && newlabel->ref) cpu_opt_invalidate();
                            update_code(newlabel, Code(newlabel->value));
                            newmembp = get_mem(current_address->mem);
                            newlabel->defpass = pass;
                        }
                    } else {
                        if (diagnostics.optimize) cpu_opt_invalidate();
                        newlabel->value = Obj(create_code(&epoint));
                        newlabel->constant = true;
                        newlabel->owner = true;
                        newlabel->epoint = epoint;
                        newmembp = get_mem(current_address->mem);
                    }
                }
                if (wht == '.') { /* .proc */
                    epoint = cmdpoint;
                    switch (prm) {
                    case CMD_PROC:
                        new_waitfor(W_PEND, &epoint);
                        if (newlabel->value->obj != CODE_OBJ) {
                            listing_line(0);
                            waitfor->skip = 0; push_dummy_context();
                            waitfor->u.cmd_proc.label = NULL;
                            val_destroy(Obj(newlabel));
                        } else if (!newlabel->ref && Code(newlabel->value)->pass != 0) {
                            listing_line(0);
                            waitfor->skip = 0;
                            set_size(newlabel, 0, current_address->mem, oaddr, newmembp);
                            Code(newlabel->value)->pass = 1;
                            push_dummy_context();
                            waitfor->u.cmd_proc.label = NULL;
                            val_destroy(Obj(newlabel));
                        } else {         /* TODO: first time it should not compile */
                            listing_line(epoint.pos);
                            push_context(Code(newlabel->value)->names);
                            newlabel->ref = false;
                            waitfor->u.cmd_proc.addr = current_address->address;waitfor->u.cmd_proc.membp = newmembp;waitfor->u.cmd_proc.label = newlabel;
                        }
                        newlabel = NULL;
                        goto finish;
                    case CMD_SECTION:
                        waitfor->u.cmd_section.addr = current_address->address;waitfor->u.cmd_section.membp = newmembp;waitfor->u.cmd_section.label = newlabel;
                        listing_line(epoint.pos);
                        newlabel->ref = false;
                        newlabel = NULL;
                        goto finish;
                    case CMD_VIRTUAL:
                        waitfor->u.cmd_virtual.membp = newmembp;waitfor->u.cmd_virtual.label = newlabel;
                        listing_line(epoint.pos);
                        newlabel->ref = false;
                        newlabel = NULL;
                        goto finish;
                    }
                    newlabel->ref = false;
                    goto as_command;
                }
                if (labelname.len == 3 && !newlabel->ref && epoint.pos != 0 && !islabel && diagnostics.label_left) {
                    unsigned int i;
                    for (i = 0; i < 3; i++) {
                        uint8_t c = labelname.data[i] | arguments.caseinsensitive;
                        if ((c < 'a') || (c > 'z')) break;
                    }
                    if (i == 3) err_msg_label_left(&epoint);
                }
                epoint = lpoint;
                newlabel->ref = false;
            }
        } else {
            switch (wht) {
            case '*':
                lpoint.pos++;
                switch (here()) {
                case ' ':
                case '\t':
                case ';':
                case '=':
                case '\0':
                    labelname.data = (const uint8_t *)"*";labelname.len = 1;
                    goto hh;
                default:
                    lpoint.pos--;
                }
                break;
            case '-':
            case '+':
                lpoint.pos++;
                switch (here()) {
                case ' ':
                case '\t':
                case ';':
                case '\0':
                    {
                        uint32_t count = (wht == '-') ? current_context->backr++ : current_context->forwr++;
                        anonsymbol.dir = (uint8_t)wht;
                        anonsymbol.pad = 0;
                        labelname.len = 2;
                        while (count != 0) {
                            anonsymbol.count[labelname.len - 2] = (uint8_t)count;
                            labelname.len++;
                            count >>= 8;
                        }
                        labelname.data = (const uint8_t *)&anonsymbol;
                    }
                    goto hh;
                default:
                    lpoint.pos--;
                }
                break;
            default:
                break;
            }
        }
        jn:
        switch (wht) {
        case '>':
        case '<':
        case '*':
            if ((waitfor->skip & 1) != 0) {
                if (pline[lpoint.pos + 1] != wht || pline[lpoint.pos + 2] != '=' || arguments.tasmcomp) {
                    if (wht == '*') {
                        lpoint.pos++;ignore();
                        if (here() == '=') {
                            labelname.data = (const uint8_t *)"*";labelname.len = 1;
                            goto starassign;
                        }
                    }
                    err_msg2(ERROR_GENERL_SYNTAX, NULL, &epoint);
                } else if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                goto breakerr;
            }
            break;
        case '|':
        case '&':
            if ((waitfor->skip & 1) != 0) {
                if (pline[lpoint.pos + 1] == wht && pline[lpoint.pos + 2] == '=' && !arguments.tasmcomp) {
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    goto breakerr;
                }
            }
            FALL_THROUGH; /* fall through */
        case '+':
        case '-':
        case '/':
        case '%':
        case '^':
        case ':':
            if ((waitfor->skip & 1) != 0) {
                if (pline[lpoint.pos + 1] != '=' || arguments.tasmcomp) {
                    err_msg2(ERROR_GENERL_SYNTAX, NULL, &epoint);
                    goto breakerr;
                }
            }
            FALL_THROUGH; /* fall through */
        case '=':
            if ((waitfor->skip & 1) != 0) {if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
            break;
        case ';':
        case '\0':
            if ((waitfor->skip & 1) != 0) {
                if (newlabel != NULL && newlabel->value->obj == CODE_OBJ && labelname.len != 0 && labelname.data[0] != '_' && labelname.data[0] != '+' && labelname.data[0] != '-') {val_destroy(Obj(cheap_context));cheap_context = ref_namespace(Code(newlabel->value)->names);}
                listing_line(epoint.pos);
            }
            break;
        case '.':
            prm = get_command();
            ignore();
            if ((waitfor->skip & 1) == 0 && waitfor->what == W_ENDC && prm != CMD_ENDC && prm != CMD_COMMENT) {
                break;
            }
        as_command:
            switch (prm) {
            case CMD_ENDC: /* .endc */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (!close_waitfor(W_ENDC)) {err_msg2(ERROR__MISSING_OPEN, ".comment", &epoint); goto breakerr;}
                if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                break;
            case CMD_FI: /* .fi */
                {
                    if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                    if (waitfor->what==W_FI || waitfor->what==W_FI2) {
                        if (waitfor->u.cmd_if.label != NULL) {set_size(waitfor->u.cmd_if.label, current_address->address - waitfor->u.cmd_if.addr, current_address->mem, waitfor->u.cmd_if.addr, waitfor->u.cmd_if.membp);val_destroy(Obj(waitfor->u.cmd_if.label));}
                        close_waitfor(waitfor->what);
                    } else {err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr;}
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_ENDSWITCH: /* .endswitch */
                {
                    if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                    if (waitfor->what==W_SWITCH || waitfor->what==W_SWITCH2) {
                        if (waitfor->u.cmd_switch.label != NULL) {set_size(waitfor->u.cmd_switch.label, current_address->address - waitfor->u.cmd_switch.addr, current_address->mem, waitfor->u.cmd_switch.addr, waitfor->u.cmd_switch.membp);val_destroy(Obj(waitfor->u.cmd_switch.label));}
                        if (waitfor->u.cmd_switch.val != NULL) val_destroy(waitfor->u.cmd_switch.val);
                        close_waitfor(waitfor->what);
                    } else {err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr;}
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_DEFAULT: /* .default */
                {
                    if ((waitfor->skip & 1) != 0) listing_line_cut(epoint.pos);
                    if (waitfor->what==W_SWITCH) {err_msg2(ERROR______EXPECTED, "'.endswitch'", &epoint); goto breakerr;}
                    if (waitfor->what!=W_SWITCH2) {err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr;}
                    waitfor->skip = (uint8_t)(waitfor->skip >> 1);
                    waitfor->what = W_SWITCH;waitfor->epoint = epoint;
                    if ((waitfor->skip & 1) != 0) listing_line_cut2( epoint.pos);
                }
                break;
            case CMD_ELSE: /* .else */
                {
                    if ((waitfor->skip & 1) != 0) listing_line_cut(epoint.pos);
                    if (waitfor->what==W_FI) { err_msg2(ERROR______EXPECTED, "'.endif'", &epoint); goto breakerr; }
                    if (waitfor->what!=W_FI2) { err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr; }
                    waitfor->skip = (uint8_t)(waitfor->skip >> 1);
                    waitfor->what = W_FI;waitfor->epoint = epoint;
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
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
                    bool truth;
                    Obj *err;
                    struct values_s *vs;
                    if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                    new_waitfor(W_FI2, &epoint);
                    waitfor->u.cmd_if.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_if.addr = current_address->address;waitfor->u.cmd_if.membp = newmembp;
                        newlabel = NULL;
                    }
                    if (skwait != 1) { waitfor->skip = 0; break; }
                    if (!get_exp(0, 1, 1, &epoint)) { waitfor->skip = 0; goto breakerr;}
                    vs = get_val(); val = vs->val;
                    switch (prm) {
                    case CMD_IF:
                        if (tobool(vs, &truth)) { waitfor->skip = 0; break; }
                        waitfor->skip = truth ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
                        break;
                    case CMD_IFNE:
                    case CMD_IFEQ:
                        err = val->obj->sign(val, &vs->epoint);
                        if (err->obj != INT_OBJ) {
                            if (err == none_value) err_msg_still_none(NULL, &vs->epoint);
                            else if (err->obj == ERROR_OBJ) err_msg_output(Error(err));
                            val_destroy(err);
                            waitfor->skip = 0; break;
                        }
                        waitfor->skip = ((Int(err)->len == 0) != (prm == CMD_IFNE)) ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
                        val_destroy(err);
                        break;
                    case CMD_IFPL:
                    case CMD_IFMI:
                        if (arguments.tasmcomp) {
                            if (toival(val, &ival, 8 * sizeof ival, &vs->epoint)) { waitfor->skip = 0; break; }
                            waitfor->skip = (((ival & 0x8000) == 0) != (prm == CMD_IFMI)) ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
                        } else {
                            err = val->obj->sign(val, &vs->epoint);
                            if (err->obj != INT_OBJ) {
                                if (err == none_value) err_msg_still_none(NULL, &vs->epoint);
                                else if (err->obj == ERROR_OBJ) err_msg_output(Error(err));
                                val_destroy(err);
                                waitfor->skip = 0; break;
                            }
                            waitfor->skip = ((Int(err)->len >= 0) != (prm == CMD_IFMI)) ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
                            val_destroy(err);
                        }
                        break;
                    }
                }
                break;
            case CMD_ELSIF: /* .elsif, .elif */
                {
                    bool truth;
                    if ((waitfor->skip & 1) != 0) listing_line_cut(epoint.pos);
                    if (waitfor->what == W_FI) {err_msg2(ERROR______EXPECTED, "'.endif'", &epoint); goto breakerr; }
                    if (waitfor->what != W_FI2) {err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr;}
                    waitfor->epoint = epoint;
                    if (waitfor->skip != 2) { waitfor->skip = 0; break; }
                    waitfor->skip = 1;
                    if (!get_exp(0, 1, 1, &epoint)) { waitfor->skip = 0; goto breakerr;}
                    if (tobool(get_val(), &truth)) waitfor->skip = 0;
                    else if (truth) listing_line_cut2(epoint.pos);
                    else waitfor->skip = 2;
                }
                break;
            case CMD_SWITCH: /* .switch */
                {
                    uint8_t skwait = waitfor->skip;
                    if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                    new_waitfor(W_SWITCH2, &epoint);
                    waitfor->u.cmd_switch.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_switch.addr = current_address->address;waitfor->u.cmd_switch.membp = newmembp;
                        newlabel = NULL;
                    }
                    if (skwait == 1) {
                        struct values_s *vs;
                        if (!get_exp(0, 1, 1, &epoint)) {waitfor->skip = 0; waitfor->u.cmd_switch.val = NULL; goto breakerr;}
                        vs = get_val(); val = vs->val;
                        if (val->obj == ERROR_OBJ) { err_msg_output(Error(val)); val = none_value; }
                        else if (val == none_value) err_msg_still_none(NULL, &vs->epoint);
                    } else val = none_value;
                    waitfor->u.cmd_switch.val = val_reference(val);
                    waitfor->skip = (val == none_value) ? 0 : (uint8_t)((skwait & 1) << 1);
                }
                break;
            case CMD_CASE: /* .case */
                {
                    bool truth = false;
                    uint8_t skwait = waitfor->skip;
                    if ((skwait & 1) != 0) listing_line_cut(epoint.pos);
                    if (waitfor->what == W_SWITCH) { err_msg2(ERROR______EXPECTED, "'.endswitch'", &epoint); goto breakerr; }
                    if (waitfor->what != W_SWITCH2) { err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr; }
                    waitfor->epoint = epoint;
                    if (skwait == 2) {
                        struct linepos_s epoint2;
                        struct values_s *vs;
                        Obj *result2;
                        struct oper_s tmp;
                        waitfor->skip = 1;
                        if (!get_exp(0, 1, 0, &epoint)) { waitfor->skip = 0; goto breakerr; }
                        result2 = ref_false();
                        tmp.op = O_EQ;
                        tmp.epoint = tmp.epoint3 = &epoint;
                        while ((vs = get_val()) != NULL) {
                            Obj *result;
                            tmp.v1 = waitfor->u.cmd_switch.val;
                            tmp.v2 = vs->val;
                            tmp.epoint2 = &vs->epoint;
                            tmp.inplace = NULL;
                            result = tmp.v1->obj->calc2(&tmp);
                            if (result == false_value) {
                                val_destroy(result);
                                continue;
                            }
                            val_destroy(result2);
                            result2 = result;
                            if (result == true_value) {
                                break;
                            } else if (result == none_value) {
                                epoint2 = vs->epoint;
                            }
                        }
                        if (result2 == none_value) err_msg_still_none(NULL, &epoint2);
                        else if (result2->obj == ERROR_OBJ) err_msg_output(Error(result2));
                        else truth = (result2 == true_value);
                        val_destroy(result2);
                    }
                    waitfor->skip = truth ? (uint8_t)(skwait >> 1) : (skwait & 2);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                }
                break;
            case CMD_ENDMACRO: /* .endmacro */
            case CMD_ENDSEGMENT: /* .endsegment */
            case CMD_ENDM: /* .endm */
                if ((prm != CMD_ENDSEGMENT && waitfor->what == W_ENDMACRO) || (prm != CMD_ENDMACRO && waitfor->what==W_ENDSEGMENT)) {
                    if (waitfor->u.cmd_macro.val != NULL) {
                        Macro(waitfor->u.cmd_macro.val)->retval = (here() != 0 && here() != ';');
                        val_destroy(waitfor->u.cmd_macro.val);
                    }
                    close_waitfor(waitfor->what);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else if ((prm != CMD_ENDSEGMENT && waitfor->what == W_ENDMACRO2) || (prm != CMD_ENDMACRO && waitfor->what==W_ENDSEGMENT2)) { /* not closed here */
                    if ((waitfor->skip & 1) != 0) listing_line_cut(epoint.pos);
                    nobreak = false;
                    if (here() != 0 && here() != ';' && get_exp(0, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                        break;
                    }
                } else err_msg2(ERROR__MISSING_OPEN, prm == CMD_ENDMACRO ? ".macro" : prm == CMD_ENDSEGMENT ? ".segment" : ".macro' or '.segment", &epoint);
                goto breakerr;
            case CMD_ENDF: /* .endf */
                if (waitfor->what==W_ENDF) {
                    if (waitfor->u.cmd_function.val != NULL) {
                        Mfunc(waitfor->u.cmd_function.val)->retval = (here() != 0 && here() != ';');
                        val_destroy(waitfor->u.cmd_function.val);
                    }
                    close_waitfor(W_ENDF);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else if (waitfor->what==W_ENDF2 || waitfor->what==W_ENDF3) { /* not closed here */
                    if ((waitfor->skip & 1) != 0) listing_line_cut(epoint.pos);
                    nobreak = false;
                    if (here() != 0 && here() != ';' && get_exp(0, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                        break;
                    }
                } else err_msg2(ERROR__MISSING_OPEN, ".function", &epoint);
                goto breakerr;
            case CMD_NEXT: /* .next */
                switch (waitfor->what) {
                case W_ENDFOR:
                case W_ENDFOR2:
                case W_ENDFOR3:
                    goto cmd_endfor;
                case W_ENDREPT:
                case W_ENDREPT2:
                case W_ENDREPT3:
                    goto cmd_endrept;
                case W_ENDWHILE:
                case W_ENDWHILE2:
                case W_ENDWHILE3:
                    goto cmd_endwhile;
                default:
                    err_msg2(ERROR__MISSING_OPEN, ".for', '.rept' or '.while", &epoint);
                    goto breakerr;
                }
            case CMD_ENDFOR: /* .endfor */
            cmd_endfor:
                waitfor->epoint = epoint;
                if (close_waitfor(W_ENDFOR)) {
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else if (waitfor->what == W_ENDFOR2) {
                    retval = true_value; /* anything non-null */
                    nobreak = false;
                } else if (close_waitfor(W_ENDFOR3)) {
                    pop_context();
                } else {err_msg2(ERROR__MISSING_OPEN, ".for", &epoint); goto breakerr;}
                break;
            case CMD_ENDREPT: /* .endrept */
            cmd_endrept:
                waitfor->epoint = epoint;
                if (close_waitfor(W_ENDREPT)) {
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else if (waitfor->what == W_ENDREPT2) {
                    retval = true_value; /* anything non-null */
                    nobreak = false;
                } else if (close_waitfor(W_ENDREPT3)) {
                    pop_context();
                } else {err_msg2(ERROR__MISSING_OPEN, ".rept", &epoint); goto breakerr;}
                break;
            case CMD_ENDWHILE: /* .endwhile */
            cmd_endwhile:
                waitfor->epoint = epoint;
                if (close_waitfor(W_ENDWHILE)) {
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else if (waitfor->what == W_ENDWHILE2) {
                    retval = true_value; /* anything non-null */
                    nobreak = false;
                } else if (close_waitfor(W_ENDWHILE3)) {
                    pop_context();
                } else {err_msg2(ERROR__MISSING_OPEN, ".while", &epoint); goto breakerr;}
                break;
            case CMD_PEND: /* .pend */
                if (waitfor->what==W_PEND) {
                    if ((waitfor->skip & 1) != 0) {
                        listing_line(epoint.pos);
                        if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".proc", &epoint);
                        if (waitfor->u.cmd_proc.label != NULL) {set_size(waitfor->u.cmd_proc.label, current_address->address - waitfor->u.cmd_proc.addr, current_address->mem, waitfor->u.cmd_proc.addr, waitfor->u.cmd_proc.membp);val_destroy(Obj(waitfor->u.cmd_proc.label));}
                    } else pop_context();
                    close_waitfor(W_PEND);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(epoint.pos);
                } else {err_msg2(ERROR__MISSING_OPEN, ".proc", &epoint); goto breakerr;}
                break;
            case CMD_ENDS: /* .ends */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (waitfor->what==W_ENDS) {
                    if ((waitfor->skip & 1) != 0) {
                        current_address->unionmode = waitfor->u.cmd_struct.unionmode;
                        close_waitfor(W_ENDS);
                        break;
                    }
                    close_waitfor(W_ENDS);
                } else if (waitfor->what==W_ENDS3 || waitfor->what==W_ENDS2) { /* not closed here */
                    nobreak = false;
                    if (here() != 0 && here() != ';' && get_exp(0, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                        break;
                    }
                } else err_msg2(ERROR__MISSING_OPEN, ".struct", &epoint);
                goto breakerr;
            case CMD_SEND: /* .send */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_SEND)) {
                    lpoint.pos += (linecpos_t)get_label(pline + lpoint.pos);
                } else if (waitfor->what==W_SEND2) {
                    str_t sectionname;
                    epoint = lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label(sectionname.data);
                    if (sectionname.len != 0) {
                        str_t cf;
                        lpoint.pos += (linecpos_t)sectionname.len;
                        str_cfcpy(&cf, &sectionname);
                        if (str_cmp(&cf, &current_section->cfname) != 0) {
                            char *s;
                            new_array(&s, current_section->name.len + 1);
                            memcpy(s, current_section->name.data, current_section->name.len);
                            s[current_section->name.len] = '\0';
                            err_msg2(ERROR______EXPECTED, s, &epoint);
                            free(s);
                        }
                    }
                    section_close(&epoint);
                    close_waitfor(W_SEND2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".section", &epoint); goto breakerr;}
                break;
            case CMD_ENDU: /* .endu */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (waitfor->what==W_ENDU) {
                    if ((waitfor->skip & 1) != 0) union_close(&epoint);
                    close_waitfor(W_ENDU);
                    oaddr = current_address->address;
                } else if (waitfor->what==W_ENDU3) { /* not closed here */
                    nobreak = false;
                    goto breakerr;
                } else if (waitfor->what==W_ENDU2) { /* not closed here */
                    nobreak = false;
                } else {err_msg2(ERROR__MISSING_OPEN, ".union", &epoint); goto breakerr;}
                break;
            case CMD_ENDP: /* .endp */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_ENDP)) {
                } else if (waitfor->what==W_ENDP2) {
                    address_t size = current_address->address - waitfor->u.cmd_page.addr;
                    if (waitfor->u.cmd_page.size != 0) {
                        uval_t size2 = waitfor->u.cmd_page.size >= 0 ? (uval_t)waitfor->u.cmd_page.size : -(uval_t)waitfor->u.cmd_page.size;
                        if ((waitfor->u.cmd_page.size > 0) ? (size > size2) : (size >= size2)) {
                            address_t ln2 = size - size2;
                            if (waitfor->u.cmd_page.size < 0) ln2++;
                            ln2 &= all_mem2;
                            err_msg2(ERROR____ALIGN_LONG, &ln2, &epoint);
                        } else if (diagnostics.page) {
                            uval_t offset = waitfor->u.cmd_page.offset;
                            address_t rem = waitfor->u.cmd_page.laddr % size2;
                            if (rem >= offset) offset += size2;
                            offset -= rem;
                            if (waitfor->u.cmd_page.size >= 0) {
                                if (offset < size) err_msg_page_cross(waitfor->u.cmd_page.laddr + offset, size - offset, (uval_t)waitfor->u.cmd_page.size, &epoint);
                            } else if (offset <= size) {
                                err_msg_page(waitfor->u.cmd_page.laddr, (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address - current_address->l_start) & all_mem), -(uval_t)waitfor->u.cmd_page.size, &epoint);
                            }
                        }
                    }
                    if (waitfor->u.cmd_page.label != NULL) {set_size(waitfor->u.cmd_page.label, size, current_address->mem, waitfor->u.cmd_page.addr, waitfor->u.cmd_page.membp);val_destroy(Obj(waitfor->u.cmd_page.label));}
                    close_waitfor(W_ENDP2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".page", &epoint); goto breakerr;}
                break;
            case CMD_ENDALIGNBLK: /* .endalignblk */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_ENDALIGNBLK)) {
                } else if (waitfor->what==W_ENDALIGNBLK2) {
                    address_t db = 0;
                    address_t size = alignblk_set_size(waitfor->u.cmd_alignblk.label2);
                    if (waitfor->u.cmd_alignblk.size != 0) {
                        uval_t size2 = waitfor->u.cmd_alignblk.size >= 0 ? (uval_t)waitfor->u.cmd_alignblk.size : -(uval_t)waitfor->u.cmd_alignblk.size;
                        if ((waitfor->u.cmd_alignblk.size > 0) ? (size > size2) : (size >= size2)) {
                        } else {
                            db = rmemalign(waitfor->u.cmd_alignblk.offset, size2, waitfor->u.cmd_alignblk.laddr);
                            if ((waitfor->u.cmd_alignblk.size > 0) ? (size <= db) : (size < db)) db = 0;
                        }
                    }
                    if (db != 0) memskip(db, &epoint);
                    if (waitfor->u.cmd_alignblk.label != NULL) {set_size(waitfor->u.cmd_alignblk.label, current_address->address - waitfor->u.cmd_alignblk.addr, current_address->mem, waitfor->u.cmd_alignblk.addr, waitfor->u.cmd_alignblk.membp); val_destroy(Obj(waitfor->u.cmd_alignblk.label));}
                    close_waitfor(W_ENDALIGNBLK2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".alignblk", &epoint); goto breakerr;}
                break;
            case CMD_HERE: /* .here */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_HERE)) {
                } else if (waitfor->what==W_HERE2) {
                    logical_close(&epoint);
                    close_waitfor(W_HERE2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".logical", &epoint); goto breakerr;}
                break;
            case CMD_ENDV: /* .endv */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_ENDV)) {
                } else if (waitfor->what==W_ENDV2) {
                    virtual_close(&epoint);
                    close_waitfor(W_ENDV2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".virtual", &epoint); goto breakerr;}
                break;
            case CMD_BEND: /* .bend */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_BEND)) {
                    pop_context();
                } else if (waitfor->what==W_BEND2) {
                    if (waitfor->u.cmd_block.label != NULL) {set_size(waitfor->u.cmd_block.label, current_address->address - waitfor->u.cmd_block.addr, current_address->mem, waitfor->u.cmd_block.addr, waitfor->u.cmd_block.membp);val_destroy(Obj(waitfor->u.cmd_block.label));}
                    if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".block", &epoint);
                    close_waitfor(W_BEND2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".block", &epoint); goto breakerr;}
                break;
            case CMD_ENDN: /* .endn */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_ENDN)) {
                    pop_context();
                } else if (waitfor->what==W_ENDN2) {
                    if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".namespace", &epoint);
                    close_waitfor(W_ENDN2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".namespace", &epoint); goto breakerr;}
                break;
            case CMD_ENDENCODE: /* .endencode */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_ENDENCODE)) {
                } else if (waitfor->what==W_ENDENCODE2) {
                    encode_close();
                    close_waitfor(W_ENDENCODE2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".encode", &epoint); goto breakerr;}
                break;
            case CMD_ENDWITH: /* .endwith */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if ((waitfor->what==W_ENDWITH || waitfor->what==W_ENDWITH2) && waitfor->u.cmd_with.label != NULL) {
                    set_size(waitfor->u.cmd_with.label, current_address->address - waitfor->u.cmd_with.addr, current_address->mem, waitfor->u.cmd_with.addr, waitfor->u.cmd_with.membp);val_destroy(Obj(waitfor->u.cmd_with.label));
                }
                if (close_waitfor(W_ENDWITH)) {
                } else if (waitfor->what==W_ENDWITH2) {
                    if (pop_context2()) err_msg2(ERROR__MISSING_OPEN, ".with", &epoint);
                    close_waitfor(W_ENDWITH2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".with", &epoint); goto breakerr;}
                break;
            case CMD_ENDWEAK: /* .endweak */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                if (close_waitfor(W_WEAK)) {
                } else if (waitfor->what==W_WEAK2) {
                    if (waitfor->u.cmd_weak.label != NULL) {set_size(waitfor->u.cmd_weak.label, current_address->address - waitfor->u.cmd_weak.addr, current_address->mem, waitfor->u.cmd_weak.addr, waitfor->u.cmd_weak.membp);val_destroy(Obj(waitfor->u.cmd_weak.label));}
                    close_waitfor(W_WEAK2);
                    strength--;
                } else {err_msg2(ERROR__MISSING_OPEN, ".weak", &epoint); goto breakerr;}
                break;
            case CMD_END: /* .end */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                nobreak = false;
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
            case CMD_SINT: /* .sint */
            case CMD_WORD: /* .word */
            case CMD_LINT: /* .lint */
            case CMD_LONG: /* .long */
            case CMD_DINT: /* .dint */
            case CMD_DWORD: /* .dword */
            case CMD_BINARY: if ((waitfor->skip & 1) != 0)
                { /* .binary */
                    struct mem_mark_s mm;
                    if (diagnostics.optimize) cpu_opt_invalidate();

                    if (prm<CMD_BYTE) {    /* .text .ptext .shift .shiftl .null */
                        argcount_t ln;
                        struct values_s *vs;
                        struct textrecursion_s trec;
                        if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                            Code(newlabel->value)->dtype = D_BYTE;
                        }
                        if (here() == 0 || here() == ';') { err_msg_argnum(0, 1, 0, &epoint); goto breakerr; }
                        if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                        if (prm == CMD_PTEXT) {
                            trec.buff[0] = (uint8_t)outputeor;
                            trec.sum = 1;
                            trec.p = 1;
                        } else {
                            trec.sum = 0;
                            trec.p = 0;
                        }
                        trec.gaps = 0;
                        trec.max = ~(address_t)0;
                        trec.prm = prm;
                        switch (prm) {
                        case CMD_SHIFTL:
                        case CMD_SHIFT: trec.tconv = BYTES_MODE_SHIFT_CHECK; break;
                        case CMD_NULL: trec.tconv = BYTES_MODE_NULL_CHECK; break;
                        default: trec.tconv = BYTES_MODE_TEXT; break;
                        }
                        trec.error = ERROR__USER_DEFINED;
                        trec.epoint = &epoint;
                        mark_mem(&mm, current_address->mem, current_address->address, current_address->l_address);
                        for (ln = get_val_remaining(), vs = get_val(); ln != 0; ln--, vs++) {
                            if (trec.p > 0) textrecursion_flush(&trec);
                            else if (trec.gaps > 0) textrecursion_gaps(&trec);
                            trec.epoint = &vs->epoint;
                            textrecursion(&trec, vs->val);
                            if (trec.error != ERROR__USER_DEFINED) { err_msg2(trec.error, NULL, trec.epoint); trec.error = ERROR__USER_DEFINED;}
                        }
                        if (trec.gaps > 0) textrecursion_gaps(&trec);
                        switch (prm) {
                        case CMD_SHIFTL:
                        case CMD_SHIFT:
                            if (trec.p > 0) {
                                uint8_t inv = (prm == CMD_SHIFT) ? 0x80 : 0x01;
                                trec.buff[trec.p - 1] ^= inv;
                            } else if (trec.sum != 0) err_msg2(ERROR___NO_LAST_GAP, NULL, trec.epoint);
                            else err_msg2(ERROR__BYTES_NEEDED, NULL, &epoint);
                            break;
                        case CMD_NULL:
                            if (trec.p >= sizeof trec.buff) textrecursion_flush(&trec);
                            trec.buff[trec.p++] = (uint8_t)outputeor; break;
                        default: break;
                        }
                        if (trec.p > 0) textrecursion_flush(&trec);
                        if (prm == CMD_PTEXT) {
                            if (trec.sum > 0x100) {
                                size_t ln2 = trec.sum;
                                err_msg2(ERROR____PTEXT_LONG, &ln2, &epoint);
                            }
                            write_mark_mem(&mm, current_address->mem, (trec.sum-1) ^ outputeor);
                        }
                        if (nolisting == 0) list_mem(&mm, current_address->mem);
                    } else if (prm<=CMD_DWORD) { /* .byte .word .int .rta .long */
                        argcount_t ln;
                        struct values_s *vs;
                        struct byterecursion_s brec;
                        if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                            signed char dtype;
                            switch (prm) {
                            case CMD_DINT: dtype = D_DINT;break;
                            case CMD_LINT: dtype = D_LINT;break;
                            case CMD_SINT: dtype = D_SINT;break;
                            case CMD_CHAR: dtype = D_CHAR;break;
                            default:
                            case CMD_BYTE: dtype = D_BYTE;break;
                            case CMD_RTA:
                            case CMD_ADDR:
                            case CMD_WORD: dtype = D_WORD;break;
                            case CMD_LONG: dtype = D_LONG;break;
                            case CMD_DWORD: dtype = D_DWORD;break;
                            }
                            Code(newlabel->value)->dtype = dtype;
                        }
                        if (here() == 0 || here() == ';') { err_msg_argnum(0, 1, 0, &epoint); goto breakerr; }
                        switch (prm) {
                        case CMD_CHAR: brec.bits = -8; break;
                        case CMD_SINT: brec.bits = -16; break;
                        case CMD_LINT: brec.bits = -24; break;
                        case CMD_DINT: brec.bits = -32; break;
                        case CMD_BYTE: brec.bits = 8; break;
                        default: brec.bits = 16; break;
                        case CMD_LONG: brec.bits = 24; break;
                        case CMD_DWORD: brec.bits = 32; break;
                        }
                        if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                        brec.p = 0;
                        brec.gaps = 0;
                        brec.warn = false;
                        brec.epoint2 = &epoint;
                        brec.prm = prm;
                        mark_mem(&mm, current_address->mem, current_address->address, current_address->l_address);
                        for (ln = get_val_remaining(), vs = get_val(); ln != 0; ln--, vs++) {
                            brec.epoint = &vs->epoint;
                            byterecursion(&brec, vs->val);
                            if (brec.warn) { err_msg_still_none(NULL, brec.epoint); brec.warn = false; }
                            if (brec.p > 0) byterecursion_flush(&brec);
                            else if (brec.gaps > 0) byterecursion_gaps(&brec);
                        }
                        if (nolisting == 0) list_mem(&mm, current_address->mem);
                    } else if (prm==CMD_BINARY) { /* .binary */
                        struct file_s *cfile2 = NULL;
                        ival_t foffs = 0;
                        filesize_t fsize = ~(filesize_t)0;
                        struct values_s *vs;
                        str_t filename;

                        if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                            Code(newlabel->value)->dtype = D_BYTE;
                        }
                        if (!get_exp(0, 1, 3, &epoint)) goto breakerr;
                        vs = get_val();
                        if (!tostr(vs, &filename)) {
                            if (filename.len == 0) err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint);
                            else cfile2 = file_open(&filename, current_file_list, FILE_OPEN_BINARY, &vs->epoint);
                        }
                        if ((vs = get_val()) != NULL) {
                            ival_t ival;
                            if (toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) {}
                            else foffs = ival;
                            if ((vs = get_val()) != NULL) {
                                uval_t uval;
                                if (touval2(vs, &uval, 8 * sizeof uval)) {}
                                else fsize = uval;
                            }
                        }

                        if (cfile2 != NULL) {
                            filesize_t foffset;
                            mark_mem(&mm, current_address->mem, current_address->address, current_address->l_address);
                            if (foffs < 0) foffset = -(uval_t)foffs < cfile2->binary.len ? (cfile2->binary.len - -(uval_t)foffs) : 0;
                            else foffset = (uval_t)foffs;
                            for (; fsize != 0 && foffset < cfile2->binary.len;) {
                                filesize_t i, ln = cfile2->binary.len - foffset;
                                uint8_t *d, *s = cfile2->binary.data + foffset;
                                if (ln > fsize) ln = fsize;
                                d = pokealloc(ln, &epoint);
                                if (outputeor != 0) {
                                    for (i = 0; i < ln; i++) d[i] = s[i] ^ (uint8_t)outputeor;
                                } else memcpy(d, s, ln);
                                foffset += ln;
                                fsize -= ln;
                            }
                            if (nolisting == 0) list_mem(&mm, current_address->mem);
                        }
                    }
                }
                break;
            case CMD_OFFS: if ((waitfor->skip & 1) != 0)
                {   /* .offs */
                    struct values_s *vs;
                    ival_t ival;
                    address_t addr;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) break;
                    if (!current_address->moved) {
                        if (current_address->end < current_address->address) current_address->end = current_address->address;
                        current_address->moved = true;
                    }
                    current_address->wrapwarn = false;
                    addr = (ival < 0 ? current_address->l_address - -(uval_t)ival : current_address->l_address + (uval_t)ival) & all_mem2;
                    if (current_address->address != addr) {
                        current_address->address = addr;
                        memjmp(current_address->mem, current_address->address);
                    }
                }
                break;
            case CMD_LOGICAL: if ((waitfor->skip & 1) != 0)
                { /* .logical */
                    struct values_s *vs;
                    uval_t uval;
                    atype_t am;
                    Obj *tmp;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    new_waitfor(W_HERE2, &epoint);
                    waitfor->u.cmd_logical.laddr = current_address->unionmode ? current_address->l_union : current_address->l_address;
                    waitfor->u.cmd_logical.addr = current_address->address;
                    waitfor->u.cmd_logical.val = val_reference(current_address->l_address_val);
                    waitfor->u.cmd_logical.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_logical.membp = newmembp;
                        newlabel = NULL;
                    }
                    current_section->logicalrecursion++;
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    tmp = vs->val;
                    if (touaddress(tmp, &uval, all_mem_bits, &vs->epoint)) break;
                    am = tmp->obj->address(tmp);
                    if (am != A_NONE && check_addr(am)) {
                        err_msg_output_and_destroy(err_addressing(am, &vs->epoint, -1));
                        break;
                    }
                    uval &= all_mem;
                    if (current_address->unionmode) {
                        current_address->l_union = uval;
                    } else {
                        current_address->l_address = uval;
                    }
                    current_address->bankwarn = false;
                    address_update(current_address, tmp);
                } else new_waitfor(W_HERE, &epoint);
                break;
            case CMD_VIRTUAL: if ((waitfor->skip & 1) != 0)
                { /* .virtual */
                    listing_line(0);
                    if (virtual_start(&epoint)) goto breakerr;
                } else new_waitfor(W_ENDV, &epoint);
                break;
            case CMD_AS: /* .as */
            case CMD_AL: /* .al */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    longaccu = (prm == CMD_AL);
                }
                break;
            case CMD_XS: /* .xs */
            case CMD_XL: /* .xl */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    longindex = (prm == CMD_XL);
                }
                break;
            case CMD_AUTSIZ: /* .autsiz */
            case CMD_MANSIZ: /* .mansiz */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    autosize = (prm == CMD_AUTSIZ);
                }
                break;
            case CMD_BLOCK: if ((waitfor->skip & 1) != 0)
                { /* .block */
                    listing_line(epoint.pos);
                    new_waitfor(W_BEND2, &epoint);
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        push_context(Code(newlabel->value)->names);
                        waitfor->u.cmd_block.addr = current_address->address;waitfor->u.cmd_block.membp = newmembp;waitfor->u.cmd_block.label = newlabel;
                        newlabel = NULL;
                    } else {
                        waitfor->u.cmd_block.label = NULL;
                        push_context(anonlabel(mycontext, '.', &epoint));
                    }
                } else {push_dummy_context(); new_waitfor(W_BEND, &epoint);}
                break;
            case CMD_NAMESPACE: if ((waitfor->skip & 1) != 0)
                { /* .namespace */
                    listing_line(epoint.pos);
                    new_waitfor(W_ENDN, &epoint);
                    if (get_exp(0, 0, 1, &epoint)) {
                        struct values_s *vs = get_val();
                        if (vs != NULL) {
                            val = Obj(get_namespace(vs->val));
                            if (val == NULL) err_msg_invalid_namespace_conv(vs);
                        } else val = NULL;
                    } else val = NULL;
                    if (val == NULL) {
                        val = Obj(anonlabel(mycontext, '.', &epoint));
                    }
                    if (val->obj == NAMESPACE_OBJ) {
                        push_context(Namespace(val));
                        waitfor->what = W_ENDN2;
                    } else push_context(current_context);
                } else {push_dummy_context(); new_waitfor(W_ENDN, &epoint);}
                break;
            case CMD_ENCODE: if ((waitfor->skip & 1) != 0)
                { /* .encode */
                    listing_line(epoint.pos);
                    new_waitfor(W_ENDENCODE, &epoint);
                    if (get_exp(0, 0, 1, &epoint)) {
                        struct values_s *vs = get_val();
                        if (vs != NULL) {
                            val = vs->val;
                            if (val->obj != ENC_OBJ) {
                                val = NULL;
                                err_msg_wrong_type2(vs->val, ENC_OBJ, &vs->epoint);
                            }
                        } else val = NULL;
                    } else val = NULL;
                    if (val == NULL) {
                        Label *label = new_anonlabel(mycontext);
                        if (label->value != NULL) {
                            if (label->defpass == pass) err_msg_double_defined(label, &label->name, &epoint);
                            else if (label->fwpass == pass) fwcount--;
                            label->constant = true;
                            label->owner = true;
                            label->defpass = pass;
                            if (label->value->obj != ENC_OBJ) {
                                val_destroy(label->value);
                                label->value = new_enc(current_file_list, &epoint);
                            } else {
                                Enc *enc = Enc(label->value);
                                enc->file_list = current_file_list;
                                enc->epoint = epoint;
                            }
                        } else {
                            label->constant = true;
                            label->owner = true;
                            label->value = new_enc(current_file_list, &epoint);
                            label->epoint = epoint;
                        }
                        val = label->value;
                    }
                    if (val->obj == ENC_OBJ) {
                        waitfor->u.cmd_encode.enc = actual_encoding;
                        actual_encoding = Enc(val_reference(val));
                        waitfor->what = W_ENDENCODE2;
                    } else {
                        waitfor->u.cmd_encode.enc = NULL;
                    }
                } else new_waitfor(W_ENDENCODE, &epoint);
                break;
            case CMD_WITH: if ((waitfor->skip & 1) != 0)
                { /* .with */
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    new_waitfor(W_ENDWITH, &epoint);
                    waitfor->u.cmd_with.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_with.addr = current_address->address;waitfor->u.cmd_with.membp = newmembp;
                        newlabel = NULL;
                    }
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    val = Obj(get_namespace(vs->val));
                    if (val == NULL) err_msg_invalid_namespace_conv(vs);
                    else {
                        push_context2(Namespace(val));
                        waitfor->what = W_ENDWITH2;
                    }
                } else { new_waitfor(W_ENDWITH, &epoint); waitfor->u.cmd_with.label = NULL; }
                break;
            case CMD_WEAK: if ((waitfor->skip & 1) != 0)
                { /* .weak */
                    listing_line(epoint.pos);
                    new_waitfor(W_WEAK2, &epoint);
                    waitfor->u.cmd_weak.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_weak.addr = current_address->address;waitfor->u.cmd_weak.membp = newmembp;
                        newlabel = NULL;
                    }
                    strength++;
                    if (strength == 0) err_msg2(ERROR_WEAKRECURSION, NULL, &epoint);
                } else new_waitfor(W_WEAK, &epoint);
                break;
            case CMD_SEED:
            case CMD_DATABANK:
            case CMD_DPAGE:
            case CMD_EOR: if ((waitfor->skip & 1) != 0)
                { /* .seed .databank, .dpage, .eor */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    switch (prm) {
                    case CMD_DATABANK:
                        if (vs->val == gap_value) databank = 256;
                        else if (touval2(vs, &uval, 8)) {}
                        else databank = uval & 0xff;
                        break;
                    case CMD_DPAGE:
                        if (vs->val == gap_value) dpage = 65536;
                        else if (touval2(vs, &uval, 16)) {}
                        else dpage = uval & 0xffff;
                        break;
                    case CMD_EOR:
                        if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else outputeor = (uval & 0xff) * 0x01010101;
                        break;
                    case CMD_SEED:
                        random_reseed(vs->val, &vs->epoint);
                        break;
                    default:
                        break;
                    }
                }
                break;
            case CMD_ALIGNIND: if ((waitfor->skip & 1) != 0)
                { /* .alignind */
                    struct star_s *s = NULL;
                    uval_t itt;
                    const struct values_s *vs;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        Code(newlabel->value)->dtype = D_BYTE;
                    }
                    if (!get_exp(0, 1, 4, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval2(vs, &itt, 8 * sizeof itt)) {}
                    else {
                        uval_t uval = 256;
                        const struct values_s *vs2 = get_val();
                        if (vs2 != NULL && touval2(vs2, &uval, 8 * sizeof uval)) {}
                        else if (uval == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs2->epoint);
                        else {
                            address_t db;
                            address_t oldstar, itt2;
                            uval_t offset;
                            s = new_star(vline);
                            if (s->pass != 0) {
                                oldstar = s->addr;
                                if (s->addr != star) {
                                    if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                    fixeddig = false;
                                }
                            } else oldstar = star;
                            s->addr = star;
                            s = new_star(vline + 1);
                            itt2 = (s->pass != 0) ? itt - (s->addr - oldstar) : itt;
                            vs2 = get_val();
                            offset = (vs2 == NULL) ? 0 : memalign_offset(get_val(), uval);
                            db = rmemalign(offset, uval, itt2);
                            if (db != 0) {
                                if (diagnostics.align) err_msg_align(db, &epoint);
                                memskipfill(db, vs2, &epoint);
                            }
                            if (fixeddig) {
                                if (itt % uval != offset) {
                                    if (pass > max_pass) err_msg_still_align(&vs->epoint);
                                    fixeddig = false;
                                }
                            }
                        }
                    }
                    if (s != NULL) {
                        if (s->pass != 0 && s->addr != current_address->l_address) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        s->addr = current_address->l_address;
                    }
                }
                break;
            case CMD_ALIGNPAGEIND: if ((waitfor->skip & 1) != 0)
                { /* .alignpageind */
                    struct star_s *s = NULL;
                    address_t size = 0;
                    const struct values_s *vs, *vs2;
                    Code *code;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        Code(newlabel->value)->dtype = D_BYTE;
                    }
                    if (!get_exp(0, 1, 4, &epoint)) goto breakerr;
                    vs = get_val();
                    code = Code(vs->val);
                    if (code->v.obj != CODE_OBJ) {err_msg_wrong_type2(Obj(code), CODE_OBJ, &vs->epoint); break;}
                    if (code->pass == pass) {err_msg2(ERROR__DEFINE_LATER, NULL, &vs->epoint); break;}
                    if (code->pass != 0) {
                        if (code->offs == 0) {
                            size = code->size;
                        } else if (code->offs > 0) {
                            size = code->size - (uval_t)code->offs;
                            if (size > code->size) return Obj(new_error(ERROR_NEGATIVE_SIZE, &vs->epoint));
                        } else {
                            if (add_overflow(-(uval_t)code->offs, code->size, &size)) err_msg_out_of_memory();
                            if (diagnostics.size_larger) err_msg_size_larger(&vs->epoint);
                        }
                    }
                    vs2 = get_val();
                    if (size != 0) {
                        ival_t ival = -256;
                        if (vs2 != NULL && toival(vs2->val, &ival, 8 * sizeof ival, &vs2->epoint)) {}
                        else if (ival == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs2->epoint);
                        else {
                            uval_t uval = (ival >= 0) ? (uval_t)ival : -(uval_t)ival;
                            if ((ival > 0) ? (size > uval) : (size >= uval)) {
                                address_t ln2 = size - uval;
                                if (ival < 0) ln2++;
                                ln2 &= all_mem2;
                                err_msg2(ERROR____ALIGN_LONG, &ln2, &epoint);
                            } else {
                                address_t db;
                                address_t itt, oldstar, itt2;
                                uval_t offset;
                                s = new_star(vline);
                                if (s->pass != 0) {
                                    oldstar = s->addr;
                                    if (s->addr != star) {
                                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                        fixeddig = false;
                                    }
                                } else oldstar = star;
                                s->addr = star;
                                s = new_star(vline + 1);
                                if (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) {
                                    itt = code->offs < 0 ? code->memaddr - -(uval_t)code->offs : code->memaddr + (uval_t)code->offs;
                                } else {
                                    itt = code->offs < 0 ? code->addr - -(uval_t)code->offs : code->addr + (uval_t)code->offs;
                                    itt = (itt - current_address->l_start) & all_mem;
                                }
                                itt2 = (s->pass != 0) ? itt - (s->addr - oldstar) : itt;
                                vs2 = get_val();
                                offset = (vs2 == NULL) ? 0 : memalign_offset(get_val(), uval);
                                db = rmemalign(offset, uval, itt2);
                                if ((ival > 0) ? (size <= db) : (size < db)) {}
                                else if (db != 0) {
                                    if (diagnostics.align) err_msg_alignblk(size - db, db, &epoint);
                                    memskipfill(db, vs2, &epoint);
                                }
                                if (fixeddig) {
                                    itt %= uval;
                                    if (itt >= offset) offset += uval;
                                    offset -= itt;
                                    if ((ival >= 0) ? (offset < size) : (offset <= size)) {
                                        if (pass > max_pass) err_msg_still_align(&vs->epoint);
                                        fixeddig = false;
                                    }
                                }
                            }
                        }
                    }
                    if (s != NULL) {
                        if (s->pass != 0 && s->addr != current_address->l_address) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        s->addr = current_address->l_address;
                    }
                }
                break;
            case CMD_ALIGNBLK: if ((waitfor->skip & 1) != 0)
                { /* .alignblk */
                    Label *label;
                    address_t size = 0;
                    const struct values_s *vs;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        Code(newlabel->value)->dtype = D_BYTE;
                    }
                    new_waitfor(W_ENDALIGNBLK2, &epoint);
                    waitfor->u.cmd_alignblk.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_alignblk.addr = current_address->address;waitfor->u.cmd_alignblk.membp = newmembp;
                        newlabel = NULL;
                    }
                    waitfor->u.cmd_alignblk.size = 0;
                    waitfor->u.cmd_alignblk.offset = 0;
                    label = new_anonlabel(mycontext);
                    if (label->value != NULL) {
                        if (label->defpass == pass) err_msg_double_defined(label, &label->name, &epoint);
                        else if (label->fwpass == pass) fwcount--;
                        label->constant = true;
                        label->owner = true;
                        label->defpass = pass;
                        if (label->value->obj == ALIGNBLK_OBJ) {
                            size = Alignblk(label->value)->size;
                        } else {
                            Alignblk *alignblk = Alignblk(val_alloc(ALIGNBLK_OBJ));
                            alignblk->size = 0;
                            alignblk->pass = pass;
                            val_destroy(label->value);
                            label->value = Obj(alignblk);
                        }
                    } else {
                        Alignblk *alignblk = Alignblk(val_alloc(ALIGNBLK_OBJ));
                        alignblk->size = 0;
                        alignblk->pass = pass;
                        label->constant = true;
                        label->owner = true;
                        label->value = Obj(alignblk);
                        label->epoint = epoint;
                    }
                    waitfor->u.cmd_alignblk.label2 = ref_label(label);
                    if (!get_exp(0, 0, 3, &epoint)) {
                        waitfor->u.cmd_alignblk.laddr = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address - current_address->l_start) & all_mem);
                        Alignblk(label->value)->addr = current_address->address;
                        goto breakerr;
                    } else {
                        ival_t ival = 256;
                        vs = get_val();
                        if (vs != NULL && toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) {}
                        else if (ival == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs->epoint);
                        else {
                            uval_t uval = (ival >= 0) ? (uval_t)ival : -(uval_t)ival;
                            if ((ival > 0) ? (size > uval) : (size >= uval)) {
                                address_t ln2 = size - uval;
                                if (ival < 0) ln2++;
                                ln2 &= all_mem2;
                                err_msg2(ERROR____ALIGN_LONG, &ln2, &epoint);
                            } else {
                                address_t db;
                                uval_t offset;
                                vs = get_val();
                                waitfor->u.cmd_alignblk.size = ival;
                                offset = (vs == NULL) ? 0 : memalign_offset(get_val(), uval);
                                waitfor->u.cmd_alignblk.offset = offset;
                                db = dmemalign(offset, uval);
                                if ((ival > 0) ? (size <= db) : (size < db)) {}
                                else if (db != 0) {
                                    if (diagnostics.align) err_msg_alignblk(size - db, db, &epoint);
                                    memskipfill(db, vs, &epoint);
                                }
                            }
                        }
                    }
                    waitfor->u.cmd_alignblk.laddr = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address - current_address->l_start) & all_mem);
                    Alignblk(label->value)->addr = current_address->address;
                } else new_waitfor(W_ENDALIGNBLK, &epoint);
                break;
            case CMD_FILL: if ((waitfor->skip & 1) != 0)
                { /* .fill */
                    address_t db = 0;
                    uval_t uval;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        Code(newlabel->value)->dtype = D_BYTE;
                    }
                    if (!get_exp(0, 1, 2, &epoint)) goto breakerr;
                    if (touval2(get_val(), &uval, 8 * sizeof uval)) {}
                    else db = uval;

                    if (db != 0) memskipfill(db, get_val(), &epoint);
                    else if (nolisting == 0) {
                        struct mem_mark_s mm;
                        mark_mem(&mm, current_address->mem, current_address->address, current_address->l_address);
                        list_mem(&mm, current_address->mem);
                    }
                }
                break;
            case CMD_ALIGN: if ((waitfor->skip & 1) != 0)
                { /* .align */
                    uval_t uval = 256;
                    const struct values_s *vs;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        Code(newlabel->value)->dtype = D_BYTE;
                    }
                    if (!get_exp(0, 0, 3, &epoint)) goto breakerr;
                    vs = get_val();
                    if (vs != NULL && touval2(vs, &uval, 8 * sizeof uval)) {}
                    else if (uval == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs->epoint);
                    else {
                        address_t db;
                        vs = get_val();
                        db = dmemalign((vs == NULL ? 0 : memalign_offset(get_val(), uval)), uval);
                        if (db != 0) {
                            if (diagnostics.align) err_msg_align(db, &epoint);
                            memskipfill(db, vs, &epoint);
                        }
                    }
                }
                break;
            case CMD_ASSERT: if ((waitfor->skip & 1) != 0)
                { /* .assert */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(0, 3, 3, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval2(vs, &uval, 8 * sizeof uval)) current_section->provides = ~(uval_t)0;
                    else current_section->provides = uval;
                    if (touval2(vs + 1, &uval, 8 * sizeof uval)) current_section->requires = 0;
                    else current_section->requires = uval;
                    if (touval2(vs + 2, &uval, 8 * sizeof uval)) current_section->conflicts = 0;
                    else current_section->conflicts = uval;
                }
                break;
            case CMD_CHECK: if ((waitfor->skip & 1) != 0)
                { /* .check */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (!get_exp(0, 2, 2, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval2(vs, &uval, 8 * sizeof uval)) {}
                    else if ((uval & current_section->provides) != uval) err_msg2(ERROR_REQUIREMENTS_, NULL, &epoint);
                    if (touval2(vs + 1, &uval, 8 * sizeof uval)) {}
                    else if ((uval & current_section->provides) != 0) err_msg2(ERROR______CONFLICT, NULL, &epoint);
                }
                break;
            case CMD_WARN:
            case CMD_CWARN:
            case CMD_ERROR:
            case CMD_CERROR: if ((waitfor->skip & 1) != 0)
                { /* .warn .cwarn .error .cerror */
                    argcount_t i, len;
                    size_t len2, chars;
                    Obj **vals;
                    uint8_t *s;
                    Tuple *tmp;
                    struct values_s *vs;
                    listing_line(epoint.pos);
                    if (prm == CMD_CWARN || prm == CMD_CERROR) {
                        bool writeit;
                        if (!get_exp(1, 1, 0, &epoint)) goto breakerr;
                        if (here() == ',') {
                            lpoint.pos++; ignore();
                            if (here() == 0 || here() == ';') {
                                err_msg2(ERROR______EXPECTED, "an expression is", &lpoint);
                            }
                        }
                        if (tobool(get_val(), &writeit) || !writeit) goto breakerr;
                    }
                    if (!get_exp(0, 0, 0, &epoint)) goto breakerr;
                    len = get_val_remaining();
                    tmp = new_tuple(len);
                    vals = tmp->data;
                    vs = get_val();
                    len2 = 0; chars = 0;
                    for (i = 0; i < len; i++, vs++) {
                        val = str_from_obj(vs->val, &vs->epoint);
                        if (val->obj != STR_OBJ) {
                            if (val == none_value) err_msg_still_none(NULL, &vs->epoint);
                            else if (val->obj == ERROR_OBJ) { err_msg_output_and_destroy(Error(val)); break; }
                            val_destroy(val);
                            val = val_reference(null_str);
                        } else {
                            Str *str = Str(val);
                            if (inc_overflow(&len2, str->len)) err_msg_out_of_memory();
                            chars += str->chars;
                        }
                        vals[i] = val;
                    }
                    tmp->len = i;
                    if (i == len) {
                        Str *v = new_str(len2);
                        v->chars = chars;
                        s = v->data;
                        for (i = 0; i < len; i++) {
                            Str *str = Str(vals[i]);
                            if (str->len == 0) continue;
                            memcpy(s, str->data, str->len);
                            s += str->len;
                        }
                        err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED, v, &epoint);
                        val_destroy(Obj(v));
                    }
                    val_destroy(Obj(tmp));
                }
                break;
            case CMD_ENC: if ((waitfor->skip & 1) != 0)
                { /* .enc */
                    struct values_s *vs;
                    Enc *newenc;
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (vs->val->obj == ENC_OBJ) {
                        newenc = Enc(vs->val);
                    } else {
                        str_t encname;
                        if (tostr(vs, &encname)) break;
                        if (encname.len == 0) {err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint); break;}
                        newenc = new_encoding(&encname, &epoint);
                    }
                    val_destroy(Obj(actual_encoding));
                    actual_encoding = ref_enc(newenc);
                }
                break;
            case CMD_CDEF: if ((waitfor->skip & 1) != 0)
                { /* .cdef */
                    if (cdef_command(&epoint)) goto breakerr;
                }
                break;
            case CMD_EDEF: if ((waitfor->skip & 1) != 0)
                { /* .edef */
                    if (edef_command(&epoint)) goto breakerr;
                }
                break;
            case CMD_TDEF: if ((waitfor->skip & 1) != 0)
                { /* .tdef */
                    if (tdef_command(&epoint)) goto breakerr;
                }
                break;
            case CMD_CPU: if ((waitfor->skip & 1) != 0)
                { /* .cpu */
                    struct values_s *vs;
                    const struct cpu_s **cpui;
                    static const struct cpu_s default_cpu = {"default", NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0};
                    static const struct cpu_s *cpus[] = {
                        &c6502, &c65c02, &c65ce02, &c6502i, &w65816, &c65dtv02,
                        &c65el02, &r65c02, &w65c02, &c4510, &default_cpu, NULL
                    };
                    str_t cpuname;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (tostr(vs, &cpuname)) break;
                    for (cpui = cpus; *cpui != NULL; cpui++) {
                        if (cpuname.len == strlen((*cpui)->name) && memcmp((*cpui)->name, cpuname.data, cpuname.len) == 0) {
                            const struct cpu_s *cpumode = (*cpui != &default_cpu) ? *cpui : arguments.cpumode;
                            if (current_address->l_address > cpumode->max_address) {
                                err_msg_big_address(&epoint);
                                current_address->l_address &= cpumode->max_address;
                            }
                            set_cpumode(cpumode);
                            break;
                        }
                    }
                    if (*cpui == NULL) err_msg2(ERROR___UNKNOWN_CPU, &cpuname, &vs->epoint);
                }
                break;
            case CMD_PRON: /* .pron */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    if (nolisting != 0) nolisting--;
                }
                break;
            case CMD_PROFF: /* .proff */
                if ((waitfor->skip & 1) != 0) {
                    nolisting++;
                    listing_line(epoint.pos);
                }
                break;
            case CMD_SHOWMAC: /* .showmac */
            case CMD_HIDEMAC: /* .hidemac */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    if (diagnostics.ignored) err_msg2(ERROR_DIRECTIVE_IGN, NULL, &epoint);
                }
                break;
            case CMD_COMMENT: /* .comment */
                if ((waitfor->skip & 1) != 0) listing_line(epoint.pos);
                new_waitfor(W_ENDC, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_INCLUDE:
            case CMD_BINCLUDE: if ((waitfor->skip & 1) != 0)
                { /* .include, .binclude */
                    struct file_s *f = NULL;
                    struct values_s *vs;
                    str_t filename;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (!tostr(vs, &filename)) {
                        if (filename.len == 0) err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint);
                        else f = file_open(&filename, current_file_list, FILE_OPEN_SOURCE, &vs->epoint);
                    }
                    if (here() != 0 && here() != ';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);

                    if (newlabel != NULL && prm == CMD_BINCLUDE && (f == NULL || f->open) && (newlabel->value->obj == CODE_OBJ || newlabel->value->obj == NONE_OBJ)) {
                        newlabel->update_after = true;
                        const_assign(newlabel, ref_none());
                    }
                    if (f == NULL) goto breakerr;
                    if (f->open) {
                        err_msg2(ERROR_FILERECURSION, NULL, &epoint);
                    } else {
                        Wait_types what;
                        struct star_s *s = new_star(vline);
                        struct star_s *stree_old = star_tree;
                        linenum_t lin = lpoint.line;

                        if (s->pass != 0 && s->addr != star) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        s->addr = star;
                        listing_file(";******  Processing file: ", f);
                        if (prm == CMD_BINCLUDE) {
                            if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                                push_context(Code(newlabel->value)->names);
                            } else {
                                push_context(anonlabel(mycontext, '.', &epoint));
                            }
                        }
                        f->open = true;
                        enterfile(f, &epoint);
                        lpoint.line = 0;
                        star_tree->vline = vline; star_tree = s; vline = s->vline;
                        what = waitfor->what; waitfor->what = W_NONE;
                        val = compile();
                        waitfor->what = what;
                        if (prm == CMD_BINCLUDE) pop_context();
                        if (val != NULL) val_destroy(val);
                        lpoint.line = lin;
                        s->vline = vline; star_tree = stree_old; vline = star_tree->vline;
                        exitfile();
                        f->open = false;
                        listing_file(";******  Return to file: ", current_file_list->file);
                    }
                    goto breakerr;
                }
                break;
            case CMD_BREPT:
            case CMD_BWHILE:
            case CMD_BFOR: if ((waitfor->skip & 1) != 0)
                { /* .bfor */
                    List *lst;
                    size_t i;
                    Label *label = new_anonlabel(mycontext);
                    if (label->value != NULL) {
                        if (label->defpass == pass) err_msg_double_defined(label, &label->name, &epoint);
                        else if (label->fwpass == pass) fwcount--;
                        label->defpass = pass;
                    } else {
                        label->value = ref_none();
                        label->epoint = epoint;
                    }
                    label->constant = true;
                    label->owner = true;
                    if (label->value->obj == TUPLE_OBJ) {
                        Tuple *old = Tuple(label->value);
                        lst = new_tuple(old->len);
                        for (i = 0; i < old->len; i++) lst->data[i] = val_reference(old->data[i]);
                    } else {
                        lst = new_tuple(lenof(lst->u.val));
                        for (i = 0; i < lst->len; i++) lst->data[i] = ref_none();
                    }
                    i = (prm == CMD_BFOR) ? for_command(NULL, lst, &epoint) : (prm == CMD_BREPT) ? rept_command(NULL, lst, &epoint) : while_command(NULL, lst, &epoint);
                    if (lst->len > i) list_shrink(lst, i);
                    const_assign(label, Obj(lst));
                    goto breakerr;
                } else {push_dummy_context(); new_waitfor(prm == CMD_BFOR ? W_ENDFOR3 : prm == CMD_BREPT ? W_ENDREPT3 : W_ENDWHILE3, &epoint);}
                break;
            case CMD_FOR: if ((waitfor->skip & 1) != 0)
                { /* .for */
                    for_command(NULL, NULL, &epoint);
                    goto breakerr;
                } else new_waitfor(W_ENDFOR, &epoint);
                break;
            case CMD_REPT: if ((waitfor->skip & 1) != 0)
                { /* .rept */
                    rept_command(NULL, NULL, &epoint);
                    goto breakerr;
                } else new_waitfor(W_ENDREPT, &epoint);
                break;
            case CMD_WHILE: if ((waitfor->skip & 1) != 0)
                { /* .while */
                    while_command(NULL, NULL, &epoint);
                    goto breakerr;
                } else new_waitfor(W_ENDWHILE, &epoint);
                break;
            case CMD_CONTINUEIF:
            case CMD_BREAKIF:
            case CMD_CONTINUE:
            case CMD_BREAK: if ((waitfor->skip & 1) != 0)
                { /* .continue, .break, .continueif, .breakif */
                    size_t wp = waitfor_p + 1;
                    bool nok = true, doit = true;
                    listing_line(epoint.pos);
                    if (prm == CMD_CONTINUEIF || prm == CMD_BREAKIF) {
                        if (get_exp(0, 1, 1, &epoint)) {
                            struct values_s *vs = get_val();
                            bool truth, result = tobool(vs, &truth);
                            if (prm == CMD_BREAKIF) {
                                if (!result && !truth) doit = false;
                            } else {
                                if (result || !truth) doit = false;
                            }
                        }
                    }
                    while (wp != 0) {
                        switch (waitfors[--wp].what) {
                        case W_ENDFOR2:
                        case W_ENDREPT2:
                        case W_ENDWHILE2:
                            break;
                        case W_ENDF3:
                            wp = 0;
                            continue;
                        default:
                            continue;
                        }
                        if (doit) {
                            if (wp != 0 && (prm == CMD_BREAK || prm == CMD_BREAKIF)) waitfors[wp].u.cmd_rept.breakout = true;
                            for (;wp <= waitfor_p; wp++) waitfors[wp].skip = 0;
                        }
                        nok = false;
                        break;
                    }
                    if (nok) err_msg2(ERROR__MISSING_LOOP, NULL, &epoint);
                }
                break;
            case CMD_PAGE: if ((waitfor->skip & 1) != 0)
                { /* .page */
                    struct values_s *vs;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    new_waitfor(W_ENDP2, &epoint);
                    waitfor->u.cmd_page.addr = current_address->address;
                    waitfor->u.cmd_page.label = newlabel;
                    if (newlabel != NULL) {
                        waitfor->u.cmd_page.membp = newmembp;
                        newlabel = NULL;
                    }
                    waitfor->u.cmd_page.laddr = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address - current_address->l_start) & all_mem);
                    waitfor->u.cmd_page.size = 0;
                    waitfor->u.cmd_page.offset = 0;
                    if (!get_exp(0, 0, 2, &epoint)) goto breakerr;
                    vs = get_val();
                    if (vs != NULL) {
                        ival_t ival;
                        if (toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) {}
                        else if (ival == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs->epoint);
                        else {
                            waitfor->u.cmd_page.size = ival;
                            waitfor->u.cmd_page.offset = memalign_offset(get_val(), (ival >= 0) ? (uval_t)ival : -(uval_t)ival);
                        }
                    } else waitfor->u.cmd_page.size = -256;
                } else new_waitfor(W_ENDP, &epoint);
                break;
            case CMD_OPTION: if ((waitfor->skip & 1) != 0)
                { /* .option */
                    static const str_t branch_across = {(const uint8_t *)"allow_branch_across_page", 24};
                    static const str_t longjmp = {(const uint8_t *)"auto_longbranch_as_jmp", 22};
                    struct values_s *vs;
                    str_t optname, cf;
                    listing_line(epoint.pos);
                    optname.data = pline + lpoint.pos; optname.len = get_label(optname.data);
                    if (optname.len == 0) { err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    lpoint.pos += (linecpos_t)optname.len;
                    ignore();if (here() != '=') {err_msg(ERROR______EXPECTED, "'='"); goto breakerr;}
                    epoint = lpoint;
                    lpoint.pos++;
                    if (!get_exp(0, 1, 0, &epoint)) goto breakerr;
                    vs = get_val();
                    str_cfcpy(&cf, &optname);
                    if (str_cmp(&cf, &branch_across) == 0) {
                        if (tobool(vs, &allowslowbranch)) break;
                    } else if (str_cmp(&cf, &longjmp) == 0) {
                        if (tobool(vs, &longbranchasjmp)) break;
                    } else err_msg2(ERROR_UNKNOWN_OPTIO, &optname, &epoint);
                }
                break;
            case CMD_GOTO: if ((waitfor->skip & 1) != 0)
                { /* .goto */
                    bool noerr = true;
                    struct values_s *vs;
                    Lbl *lbl;
                    listing_line(epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &epoint);
                    vs = get_val(); val = vs->val;
                    if (val->obj != LBL_OBJ) {err_msg_wrong_type2(val, LBL_OBJ, &vs->epoint); break;}
                    lbl = Lbl(val);
                    if (lbl->pass == pass && lbl->file_list == current_file_list && lbl->parent == current_context && oldwaitforp <= lbl->waitforp) {
                        while (lbl->waitforp < waitfor_p) {
                            const char *msg;
                            switch (waitfor->what) {
                            case W_FI:
                            case W_FI2:
                                if (waitfor->u.cmd_if.label != NULL) {set_size(waitfor->u.cmd_if.label, current_address->address - waitfor->u.cmd_if.addr, current_address->mem, waitfor->u.cmd_if.addr, waitfor->u.cmd_if.membp);val_destroy(Obj(waitfor->u.cmd_if.label));}
                                break;
                            case W_SWITCH2:
                            case W_SWITCH:
                                if (waitfor->u.cmd_switch.label != NULL) {set_size(waitfor->u.cmd_switch.label, current_address->address - waitfor->u.cmd_switch.addr, current_address->mem, waitfor->u.cmd_switch.addr, waitfor->u.cmd_switch.membp);val_destroy(Obj(waitfor->u.cmd_switch.label));}
                                if (waitfor->u.cmd_switch.val != NULL) val_destroy(waitfor->u.cmd_switch.val);
                                break;
                            default:
                                msg = check_waitfor();
                                if (msg != NULL) {
                                    err_msg2(ERROR_MISSING_CLOSE, msg, &waitfor->epoint);
                                    noerr = false;
                                }
                            }
                            close_waitfor(waitfor->what);
                        }
                        if (noerr) {
                            lpoint.line = lbl->sline;
                        }
                    } else err_msg_not_defined(NULL, &vs->epoint);
                }
                break;
            case CMD_MACRO:
            case CMD_SEGMENT: /* .macro, .segment */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(prm == CMD_MACRO ? W_ENDMACRO : W_ENDSEGMENT, &epoint);
                waitfor->u.cmd_macro.val = NULL;
                waitfor->skip = 0;
                break;
            case CMD_FUNCTION: /* .function */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_ENDF, &epoint);
                waitfor->u.cmd_function.val = NULL;
                waitfor->skip = 0;
                break;
            case CMD_FROM: /* .from */
            case CMD_SFUNCTION: /* .sfunction */
            case CMD_VAR: /* .var */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    goto breakerr;
                }
                break;
            case CMD_LBL: /* .lbl */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    else if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &epoint);
                }
                break;
            case CMD_PROC: /* .proc */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                push_dummy_context();
                new_waitfor(W_PEND, &epoint);
                waitfor->skip = 0;waitfor->u.cmd_proc.label = NULL;
                break;
            case CMD_STRUCT: /* .struct */
                new_waitfor(W_ENDS, &epoint);
                if ((waitfor->skip & 1) != 0) {
                    listing_line(0);
                    waitfor->u.cmd_struct.unionmode = current_address->unionmode;
                    current_address->unionmode = false;
                }
                break;
            case CMD_UNION: /* .union */
                new_waitfor(W_ENDU, &epoint);
                if ((waitfor->skip & 1) != 0) {
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(0);
                    waitfor->u.cmd_union.unionmode = current_address->unionmode;
                    current_address->unionmode = true;
                    waitfor->u.cmd_union.addr = current_address->start;
                    waitfor->u.cmd_union.addr2 = current_address->end;
                    waitfor->u.cmd_union.laddr = current_address->l_union;
                    current_address->start = current_address->end = current_address->address;
                    current_address->l_union = current_address->l_address;
                }
                break;
            case CMD_DUNION:
            case CMD_DSTRUCT: if ((waitfor->skip & 1) != 0)
                { /* .dstruct */
                    address_t oldstart, oldend;
                    address_t oldl_start, oldl_union;
                    bool oldunionmode;
                    struct values_s *vs;
                    Type *obj;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(0);
                    if (!get_exp(1, 1, 0, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (here() == ',') lpoint.pos++;
                    obj = (prm == CMD_DUNION) ? UNION_OBJ : STRUCT_OBJ;
                    if (val->obj != obj) {err_msg_wrong_type2(val, obj, &vs->epoint); goto breakerr;}
                    oldstart = current_address->start;
                    oldend = current_address->end;
                    oldl_start = current_address->l_start;
                    oldl_union = current_address->l_union;
                    oldunionmode = current_address->unionmode;
                    current_address->start = current_address->end = current_address->address;
                    current_address->l_start = current_address->l_address;
                    current_address->l_union = current_address->l_address;
                    current_address->unionmode = (prm == CMD_DUNION);
                    val = macro_recurse(prm == CMD_DUNION ? W_ENDU3 : W_ENDS3, val, NULL, &epoint);
                    if (val != NULL) val_destroy(val);
                    current_address->start = oldstart;
                    oldstart = current_address->unionmode ? current_address->end : current_address->address;
                    if (oldend > current_address->end) current_address->end = oldend;
                    current_address->l_start = oldl_start;
                    current_address->l_union = oldl_union;
                    current_address->unionmode = oldunionmode;
                    if (oldstart > current_address->address) {
                        memskip(oldstart - current_address->address, &epoint);
                    }
                }
                goto breakerr;
            case CMD_DSECTION: if ((waitfor->skip & 1) != 0)
                { /* .dsection */
                    struct section_s *tmp3;
                    str_t sectionname;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(epoint.pos);
                    epoint = lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label(sectionname.data);
                    if (sectionname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    lpoint.pos += (linecpos_t)sectionname.len;
                    tmp3=new_section(&sectionname);
                    if (tmp3->defpass == pass) {
                        err_msg_double_definedo(tmp3->file_list, &tmp3->epoint, &sectionname, &epoint);
                    } else {
                        address_t t;
                        if (tmp3->usepass == 0 || tmp3->defpass < pass - 1) {
                            address_t ln = tmp3->address.mem->mem.p;
                            size_t ln2 = tmp3->address.mem->p;
                            tmp3->address.wrapwarn = tmp3->address.moved = false;
                            tmp3->address.bankwarn = false;
                            tmp3->address.end = tmp3->address.start = tmp3->restart = tmp3->address.address = current_address->address;
                            tmp3->l_restart = tmp3->address.l_address = current_address->l_address;
                            tmp3->usepass = pass;
                            val_destroy(Obj(tmp3->address.mem));
                            tmp3->address.mem = new_memblocks(ln, ln2);
                            tmp3->address.mem->lastaddr = tmp3->address.address;
                            tmp3->address.mem->section = tmp3;
                            if (diagnostics.optimize) cpu_opt_invalidate();
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        tmp3->provides = ~(uval_t)0;tmp3->requires = tmp3->conflicts = 0;
                        tmp3->address.unionmode = current_address->unionmode;
                        tmp3->address.l_start = current_address->l_start;
                        tmp3->address.l_union = current_address->l_union;
                        tmp3->logicalrecursion = current_section->logicalrecursion;
                        val_destroy(tmp3->address.l_address_val); /* TODO: restart as well */
                        tmp3->address.l_address_val = val_reference(current_address->l_address_val);
                        tmp3->file_list = current_file_list;
                        tmp3->epoint = epoint;
                        if (tmp3->usepass == pass) {
                            t = tmp3->size;
                            if (t < tmp3->address.end - tmp3->address.start) t = tmp3->address.end - tmp3->address.start;
                            if (!tmp3->address.moved) {
                                if (t < tmp3->address.address - tmp3->address.start) t = tmp3->address.address - tmp3->address.start;
                            }
                            if (tmp3->restart != current_address->address) {
                                address_t change = current_address->address - tmp3->restart;
                                tmp3->address.end = (tmp3->address.end + change) & all_mem2;
                                tmp3->address.address = (tmp3->address.address + change) & all_mem2;
                                tmp3->address.start = tmp3->restart = current_address->address;
                                if (tmp3->address.end < tmp3->address.start) tmp3->address.end = all_mem2 + 1;
                                tmp3->address.mem->lastaddr = (tmp3->address.mem->lastaddr + change) & all_mem2;
                            }
                            if (tmp3->l_restart != current_address->l_address) {
                                tmp3->address.l_address = (tmp3->address.l_address + current_address->l_address - tmp3->l_restart) & all_mem;
                                tmp3->l_restart = current_address->l_address;
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = false;
                            }
                        } else {
                            address_t ln = tmp3->address.mem->mem.p;
                            size_t ln2 = tmp3->address.mem->p;
                            if (!tmp3->address.moved) {
                                if (tmp3->address.end < tmp3->address.address) tmp3->address.end = tmp3->address.address;
                                tmp3->address.moved = true;
                            }
                            tmp3->address.wrapwarn = false;
                            tmp3->address.bankwarn = false;
                            t = tmp3->address.end - tmp3->address.start;
                            tmp3->address.end = tmp3->address.start = tmp3->restart = tmp3->address.address = current_address->address;
                            tmp3->address.l_address = current_address->l_address;
                            if (tmp3->l_restart != current_address->l_address) {
                                tmp3->l_restart = current_address->l_address;
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = false;
                            }
                            tmp3->size = t;
                            val_destroy(Obj(tmp3->address.mem));
                            tmp3->address.mem = new_memblocks(ln, ln2);
                            tmp3->address.mem->lastaddr = tmp3->address.address;
                            tmp3->address.mem->section = tmp3;
                            if (diagnostics.optimize) cpu_opt_invalidate();
                        }
                        tmp3->usepass = pass;
                        tmp3->defpass = pass;
                        memref(current_address->mem, tmp3->address.mem, current_address->address, t);
                        if (t != 0) {
                            memskip(t, &epoint);
                        }
                    }
                }
                break;
            case CMD_SECTION: if ((waitfor->skip & 1) != 0)
                { /* .section */
                    listing_line(0);
                    if (section_start(&epoint)) goto breakerr;
                } else new_waitfor(W_SEND, &epoint);
                break;
            case lenof(command):
                if ((waitfor->skip & 1) != 0) goto as_macro2;
                break;
            default:
                if ((waitfor->skip & 1) != 0) {
                    listing_line(epoint.pos);
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    goto breakerr;
                }
                break;
            }
            break;
        case '#':if ((waitfor->skip & 1) != 0) /* skip things if needed */
            {                   /* macro stuff */
                struct values_s *vs;
                lpoint.pos++;
            as_macro2:
                if (!get_exp(2, 1, 1, &epoint)) goto breakerr;
                vs = get_val(); val = vs->val;
                switch (val->obj->type) {
                case T_MACRO:
                case T_SEGMENT:
                case T_STRUCT:
                case T_UNION:
                case T_MFUNC: break;
                default : err_msg_wrong_type2(val, NULL, &vs->epoint); goto breakerr;
                }
            as_macro:
                if (val->obj == MACRO_OBJ || val->obj == STRUCT_OBJ || val->obj == UNION_OBJ) {
                    Namespace *context;
                    if (newlabel != NULL && !Macro(val)->retval && newlabel->value->obj == CODE_OBJ) {
                        context = Code(newlabel->value)->names;
                    } else {
                        context = anonlabel(mycontext, '#', &epoint);
                    }
                    if (newlabel != NULL && Macro(val)->retval) {
                        listing_equal(newlabel->value);
                    } else {
                        listing_line_cut(epoint.pos);
                    }
                    val = macro_recurse(val->obj == MACRO_OBJ ? W_ENDMACRO2 : val->obj == STRUCT_OBJ ? W_ENDS3 : W_ENDU3, val, context, &epoint);
                } else if (val->obj == MFUNC_OBJ) {
                    Mfunc *mfunc = Mfunc(val_reference(val));
                    if (!get_exp(4, 0, 0, NULL)) {
                        val = NULL;
                        val_destroy(Obj(mfunc));
                        goto breakerr;
                    }
                    if (newlabel != NULL && mfunc->retval) {
                        listing_equal(newlabel->value);
                    } else {
                        listing_line_cut(epoint.pos);
                    }
                    val = mfunc_recurse(mfunc, anonlabel(mfunc->namespaces[mfunc->nslen - 1], '#', &epoint), strength, &epoint);
                    val_destroy(Obj(mfunc));
                } else { /* segment */
                    if (newlabel != NULL && Macro(val)->retval) {
                        listing_equal(newlabel->value);
                    } else {
                        listing_line_cut(epoint.pos);
                    }
                    val = macro_recurse(W_ENDSEGMENT2, val, NULL, &epoint);
                }
                if (val != NULL) {
                    if (newlabel != NULL) {
                        newlabel->update_after = true;
                        const_assign(newlabel, val);
                    } else val_destroy(val);
                }
                goto breakerr;
            }
            break;
        default:
            if ((waitfor->skip & 1) != 0) {
                str_t opname;
                bool down;

                if (newlabel != NULL && newlabel->value->obj == CODE_OBJ && labelname.len != 0 && labelname.data[0] != '_' && labelname.data[0] != '+' && labelname.data[0] != '-') {val_destroy(Obj(cheap_context));cheap_context = ref_namespace(Code(newlabel->value)->names);}
                opname.data = pline + lpoint.pos; opname.len = get_label(opname.data);
                lpoint.pos += (linecpos_t)opname.len;
                if (opname.len == 3 && (prm = lookup_opcode(opname.data)) >= 0) {
                    Error *err;
                    struct linepos_s oldlpoint;
                    Funcargs tmp;
                    unsigned int w;
                    if (false) {
                as_opcode:
                        opname = labelname;
                    }
                    ignore();
                    oldlpoint = lpoint;
                    w = 3; /* 0=byte 1=word 2=long 3=negative/too big */
                    if (here() == 0 || here() == ';') {
                        tmp.len = 0;
                        err = instruction(prm, w, &tmp, &epoint);
                    } else {
                        if (arguments.tasmcomp) {
                            if (here() == '!') {w = 1; lpoint.pos++;}
                        } else {
                            if (here()=='@') {
                                switch (pline[lpoint.pos + 1] | arguments.caseinsensitive) {
                                case 'b': w = 0;break;
                                case 'w': w = 1;break;
                                case 'l': w = 2;break;
                                default:err_msg2(ERROR______EXPECTED, "'@b' or '@w' or '@l'", &lpoint);goto breakerr;
                                }
                                if (diagnostics.deprecated) {
                                    bool warn;
                                    unichar_t ch = pline[lpoint.pos + 2];
                                    if ((ch & 0x80) != 0) {
                                        if (arguments.to_ascii) {
                                            utf8in(pline + lpoint.pos + 2, &ch);
                                            warn = (uget_property(ch)->property & (id_Continue | id_Start)) != 0;
                                        } else warn = false;
                                    } else warn = (uint8_t)((ch | 0x20) - 'a') <= ('z' - 'a') || (uint8_t)(ch - '0') < 10 || ch == '_';
                                    if (warn) err_msg2(ERROR________OLD_AT, NULL, &lpoint);
                                }
                                lpoint.pos += 2;
                            }
                        }
                        if (!get_exp(3, 0, 0, NULL)) goto breakerr;
                        get_vals_funcargs(&tmp);
                        err = instruction(prm, w, &tmp, &epoint);
                    }
                    if (err == NULL) {
                        if (llist != NULL) listing_instr(0, 0, -1);
                        if (diagnostics.alias && prm != current_cpu->alias[prm]) err_msg_alias(current_cpu->mnemonic[prm], current_cpu->mnemonic[current_cpu->alias[prm]], &epoint);
                        break;
                    }
                    tmp2 = find_label(&opname, NULL);
                    if (tmp2 != NULL) {
                        const Type *obj = tmp2->value->obj;
                        if (diagnostics.case_symbol && str_cmp(&opname, &tmp2->name) != 0) err_msg_symbol_case(&opname, tmp2, &epoint);
                        if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                            val_destroy(Obj(err));
                            touch_label(tmp2);
                            lpoint = oldlpoint;
                            val = tmp2->value;
                            goto as_macro;
                        }
                    }
                    err_msg_output_and_destroy(err);
                    break;
                }
                down = (opname.data[0] != '_');
                tmp2 = down ? find_label(&opname, NULL) : find_label2(&opname, cheap_context);
                if (tmp2 != NULL) {
                    const Type *obj = tmp2->value->obj;
                    if (diagnostics.case_symbol && str_cmp(&opname, &tmp2->name) != 0) err_msg_symbol_case(&opname, tmp2, &epoint);
                    if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                        if (diagnostics.macro_prefix) {
                            ignore();
                            if (here() == 0 || here() == ';') err_msg_macro_prefix(&epoint);
                        }
                        touch_label(tmp2);
                        val = tmp2->value;goto as_macro;
                    }
                }
                err_msg2(ERROR_GENERL_SYNTAX, NULL, &epoint);
                goto breakerr;
            }
        }
    finish:
        ignore();if (here() != 0 && here() != ';' && (waitfor->skip & 1) != 0) err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    breakerr:
        if (newlabel != NULL) {
            if (!newlabel->update_after) set_size(newlabel, current_address->address - oaddr, current_address->mem, oaddr, newmembp);
            val_destroy(Obj(newlabel));
        }
    }

    while (oldwaitforp < waitfor_p) {
        const char *msg = check_waitfor();
        if (msg != NULL) err_msg2(ERROR_MISSING_CLOSE, msg, &waitfor->epoint);
        close_waitfor(waitfor->what);
    }
    return retval;
}

static void one_pass(int argc, char **argv, int opts) {
    static const str_t none_enc = {(const uint8_t *)"none", 4};
    static const str_t cmdline_name = { (const uint8_t *)"<command line>", 14 };
    static const struct linepos_s nopoint = {0, 0};
    struct file_s *cfile;
    Obj *val;
    int i;
    address_t ln = root_section.address.mem->mem.p;
    size_t ln2 = root_section.address.mem->p;
    str_t filename;

    fixeddig = true;constcreated = false; fwcount = 0; efwcount = 0; error_reset();random_reseed(int_value[0], NULL);
    val_destroy(Obj(root_section.address.mem));
    root_section.address.mem = new_memblocks(0, 0);
    if (diagnostics.optimize) cpu_opt_invalidate();
    for (i = opts - 1; i <= argc; i++) {
        set_cpumode(arguments.cpumode); if (pass == 1 && i == opts - 1) constcreated = false;
        star = databank = dpage = strength = 0;longaccu = longindex = autosize = false;
        val_destroy(Obj(actual_encoding));
        actual_encoding = ref_enc(new_encoding(&none_enc, &nopoint));
        allowslowbranch = true; longbranchasjmp = false;
        reset_waitfor();lpoint.line = vline = 0;outputeor = 0; pline = (const uint8_t *)"";
        reset_context();
        current_section = &root_section;
        current_address = &root_section.address;
        reset_section(current_section);
        init_macro();
        star_tree = init_star((linenum_t)i);

        if (i == opts - 1 || i == argc) {
            cfile = file_open(&cmdline_name, NULL, i == argc ? FILE_OPEN_COMMANDLINE : FILE_OPEN_DEFINES, &nopoint);
            if (cfile != NULL) {
                cfile->open = true;
                enterfile(cfile, &nopoint);
                if (i == argc) {
                    commandline_file_list = current_file_list;
                    update_argvalues();
                    ref_labels();
                } else {
                    listing_file(";******  Command line definitions", NULL);
                    val = compile();
                    if (val != NULL) val_destroy(val);
                }
                exitfile();
                cfile->open = false;
                if (i != argc) {
                    val_destroy(Obj(root_section.address.mem));
                    root_section.address.mem = new_memblocks(ln, ln2);
                    if (diagnostics.optimize) cpu_opt_invalidate();
                }
                continue;
            }
            i++;
            if (i >= argc) break;
        }

        filename.data = (const uint8_t *)argv[i];
        filename.len = strlen(argv[i]);
        cfile = file_open(&filename, NULL, dash_name(argv[i]) ? FILE_OPEN_STDIN : FILE_OPEN_SOURCE, &nopoint);
        if (cfile != NULL) {
            cfile->cmdline = true;
            cfile->open = true;
            enterfile(cfile, &nopoint);
            listing_file(";******  Processing input file: ", cfile);
            val = compile();
            if (val != NULL) val_destroy(val);
            exitfile();
            cfile->open = false;
        }
    }
    if (fwcount != 0 || efwcount != 0) fixeddig = false;
    if (fixeddig && root_section.members.root != NULL) section_sizecheck(root_section.members.root);
    /*garbage_collect();*/
}

int main2(int *argc2, char **argv2[]) {
    size_t j;
    int opts;
    static const struct linepos_s nopoint = {0, 0};
    char **argv;
    int argc;
    bool failed;

    compile_init(*argv2[0]);

    opts = init_arguments(argc2, argv2); argc = *argc2; argv = *argv2;
    if (opts <= 0) {
        compile_destroy();
        return (opts < 0) ? EXIT_FAILURE : EXIT_SUCCESS;
    }
    init_encoding(arguments.to_ascii);

    if (arguments.quiet) {
        puts("64tass Turbo Assembler Macro V" VERSION "\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");
        fflush(stdout);
    }

    /* assemble the input file(s) */
    do {
        if (pass++>max_pass) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        listing_pccolumn = false;
        one_pass(argc, argv, opts);
        if (signal_received) { err_msg_signal(); break; }
    } while (!fixeddig || constcreated);

    if (arguments.list.name == NULL) {
        if (diagnostics.unused.macro || diagnostics.unused.consts || diagnostics.unused.label || diagnostics.unused.variable) unused_check(root_namespace);
    }
    failed = error_serious();
    if (!failed) {
        /* assemble again to create listing */
        if (arguments.list.name != NULL) {
            nolisting = 0;

            max_pass = pass; pass++;
            listing_open(&arguments.list, argc, argv);
            one_pass(argc, argv, opts);
            listing_close(&arguments.list);

            if (diagnostics.unused.macro || diagnostics.unused.consts || diagnostics.unused.label || diagnostics.unused.variable) unused_check(root_namespace);
        }

        for (j = 0; j < arguments.symbol_output_len; j++) {
            labelprint(&arguments.symbol_output[j]);
        }
        if (arguments.make.name != NULL) makefile(argc - opts, argv + opts);

        failed = error_serious();
    }
    if (!failed) {
        memclose(root_section.address.mem);
        if (root_section.members.root != NULL) section_memclose(root_section.members.root);
        for (j = 0; j < arguments.output_len; j++) {
            const struct output_s *output = &arguments.output[j];
            struct section_s *section, *parent;
            section = find_this_section(output->section);
            if (section == NULL) {
                str_t sectionname;
                sectionname.data = pline;
                sectionname.len = lpoint.pos;
                err_msg2(ERROR__SECTION_ROOT, &sectionname, &nopoint);
                continue;
            }
            parent = section->parent;
            section->parent = NULL;
            if (arguments.quiet) {
                if (output->name != NULL) {
                    fputs("Output file:       ", stdout);
                    argv_print(output->name, stdout);
                    putc('\n', stdout);
                }
                if (!output->mapfile) printmemorymap(section->address.mem);
                if (output->name != NULL || !output->mapfile) {
                    if (fflush(stdout) != 0) setvbuf(stdout, NULL, _IOLBF, 1024);
                }
            }
            if (output->mapname != NULL) memorymapfile(section->address.mem, output);
            if (output->name != NULL) {
                if (j == arguments.output_len - 1) {
                    output_mem(section->address.mem, output);
                } else {
                    Memblocks *tmp = section->address.mem;
                    section->address.mem = copy_memblocks(tmp);
                    output_mem(tmp, output);
                    val_destroy(Obj(section->address.mem));
                    section->address.mem = tmp;
                }
            }
            section->parent = parent;
        }
        failed = error_serious();
    }

    error_print(&arguments.error);
    if (arguments.quiet) {
        error_status();
        printf("Passes:            %u\n", pass);
        fflush(stdout);
    }
    compile_destroy();
    return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
