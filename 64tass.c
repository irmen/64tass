/*
    Turbo Assembler 6502/65C02/65816/DTV
    $Id: 64tass.c 1896 2019-02-16 16:48:56Z soci $

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
#include "identobj.h"
#include "dictobj.h"

struct Listing *listing = NULL;
int temporary_label_branch; /* function declaration in function context, not good */
linepos_t poke_pos;
line_t vline;      /* current line */
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

static size_t waitfor_p, waitfor_len;
static struct waitfor_s {
    Wait_types what;
    uint8_t skip;
    struct linepos_s epoint;
    union {
        struct {
            address2_t laddr;
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
            address2_t laddr;
            address_t addr, addr2;
        } cmd_union;
        struct {
            address_t addr;
            Label *label;
            size_t membp;
        } cmd_weak;
        struct {
            address2_t laddr;
            address_t addr;
            Label *label;
            size_t membp;
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
        } cmd_switch;
    } u;
} *waitfors, *waitfor;

static struct avltree star_root;
struct avltree *star_tree = NULL;

static const char * const command[] = { /* must be sorted, first char is the ID */
    "\x08" "addr",
    "\x22" "al",
    "\x34" "align",
    "\x21" "as",
    "\x35" "assert",
    "\x5b" "autsiz",
    "\x3a" "bend",
    "\x63" "bfor",
    "\x1a" "binary",
    "\x50" "binclude",
    "\x39" "block",
    "\x5a" "break",
    "\x62" "brept",
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
    "\x5f" "endn",
    "\x1e" "endp",
    "\x46" "ends",
    "\x56" "endswitch",
    "\x49" "endu",
    "\x61" "endv",
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
    "\x02" "shift",
    "\x03" "shiftl",
    "\x3d" "showmac",
    "\x09" "sint",
    "\x45" "struct",
    "\x53" "switch",
    "\x00" "text",
    "\x48" "union",
    "\x42" "var",
    "\x60" "virtual",
    "\x2b" "warn",
    "\x57" "weak",
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
    CMD_DPAGE, CMD_FILL, CMD_WARN, CMD_ENC, CMD_ENDIF, CMD_IFNE, CMD_IFEQ,
    CMD_IFPL, CMD_IFMI, CMD_CERROR, CMD_CWARN, CMD_ALIGN, CMD_ASSERT,
    CMD_CHECK, CMD_CPU, CMD_OPTION, CMD_BLOCK, CMD_BEND, CMD_PRON, CMD_PROFF,
    CMD_SHOWMAC, CMD_HIDEMAC, CMD_END, CMD_EOR, CMD_SEGMENT, CMD_VAR, CMD_LBL,
    CMD_GOTO, CMD_STRUCT, CMD_ENDS, CMD_DSTRUCT, CMD_UNION, CMD_ENDU,
    CMD_DUNION, CMD_SECTION, CMD_DSECTION, CMD_SEND, CMD_CDEF, CMD_EDEF,
    CMD_BINCLUDE, CMD_FUNCTION, CMD_ENDF, CMD_SWITCH, CMD_CASE, CMD_DEFAULT,
    CMD_ENDSWITCH, CMD_WEAK, CMD_ENDWEAK, CMD_CONTINUE, CMD_BREAK, CMD_AUTSIZ,
    CMD_MANSIZ, CMD_SEED, CMD_NAMESPACE, CMD_ENDN, CMD_VIRTUAL, CMD_ENDV,
    CMD_BREPT, CMD_BFOR
} Command_types;

/* --------------------------------------------------------------------------- */
static void tfree(void) {
    destroy_lastlb();
    destroy_eval();
    destroy_variables();
    destroy_section();
    destroy_longjump();
    destroy_encoding();
    destroy_values();
    destroy_namespacekeys();
    err_destroy();
    destroy_file();
    destroy_ternary();
    destroy_pairs();
    destroy_opt_bit();
    free(arguments.symbol_output);
    if (unfc(NULL)) {}
    if (unfkc(NULL, NULL, 0)) {}
    str_cfcpy(NULL, NULL);
    free_macro();
    free(waitfors);
}

static void status(void) {
    bool errors = error_print();
    if (arguments.quiet) {
        error_status();
        printf("Passes: %12u\n",pass);
        if (!errors) sectionprint();
    }
    tfree();
}

void new_waitfor(Wait_types what, linepos_t epoint) {
    uint8_t skwait = waitfor->skip;
    waitfor_p++;
    if (waitfor_p >= waitfor_len) {
        waitfor_len += 8;
        if (/*waitfor_len < 8 ||*/ waitfor_len > SIZE_MAX / sizeof *waitfors) err_msg_out_of_memory(); /* overflow */
        waitfors = (struct waitfor_s *)reallocx(waitfors, waitfor_len * sizeof *waitfors);
    }
    waitfor = &waitfors[waitfor_p];
    waitfor->what = what;
    waitfor->skip = skwait;
    waitfor->epoint = *epoint;
}

static void reset_waitfor(void) {
    struct waitfor_s dummy;
    struct linepos_s lpos = {0, 0};
    dummy.skip = 1;
    waitfor_p = (size_t)-1;
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
    Code *code = (Code *)label->value;
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
        val_destroy(&code->memblocks->v);
        code->memblocks = ref_memblocks(mem);
    }
    code->memaddr = oaddr;
    code->membp = membp;
}

static bool tobool(const struct values_s *v1, bool *truth) {
    Obj *val = v1->val, *err;
    const Type *obj = val->obj;
    bool error;
    if (obj == BOOL_OBJ) {
        *truth = val == &true_value->v;
        return false;
    }
    err = obj->truth(val, TRUTH_BOOL, &v1->epoint);
    error = err->obj != BOOL_OBJ;
    if (error) {
        if (err->obj == ERROR_OBJ) {
            err_msg_output((Error *)err);
        }
    } else {
        *truth = (Bool *)err == true_value;
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, val, &v1->epoint);
    }
    val_destroy(err);
    return error;
}

static MUST_CHECK bool touval2(Obj *v1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Error *err = v1->obj->uval2(v1, uv, bits, epoint);
    if (err == NULL) return false;
    err_msg_output_and_destroy(err);
    return true;
}

/* --------------------------------------------------------------------------- */
/*
 * Skip memory
 */
static void memskip(address_t db) { /* poke_pos! */
    if (current_address->moved) {
        if (current_address->address < current_address->start) err_msg2(ERROR_OUTOF_SECTION, NULL, poke_pos);
        if (current_address->wrapwarn) {err_msg_mem_wrap(poke_pos);current_address->wrapwarn = false;}
        current_address->moved = false;
    }
    if (current_address->l_address.address > 0xffff || db > 0x10000 - current_address->l_address.address) {
        current_address->l_address.address = ((current_address->l_address.address + db - 1) & 0xffff) + 1;
        err_msg_pc_wrap(poke_pos);
    } else current_address->l_address.address += db;
    if (db > (~current_address->address & all_mem2)) {
        if (db - 1 + current_address->address == all_mem2) {
            current_address->wrapwarn = current_address->moved = true;
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->address = 0;
        } else {
            if (current_address->start != 0) err_msg2(ERROR_OUTOF_SECTION, NULL, poke_pos);
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->moved = false;
            current_address->address = (current_address->address + db) & all_mem2;
            err_msg_mem_wrap(poke_pos);current_address->wrapwarn = false;
        }
    } else current_address->address += db;
    memjmp(current_address->mem, current_address->address);
}

/* --------------------------------------------------------------------------- */
/*
 * output one byte
 */
FAST_CALL uint8_t *pokealloc(address_t db) { /* poke_pos! */
    if (current_address->moved) {
        if (current_address->address < current_address->start) err_msg2(ERROR_OUTOF_SECTION, NULL, poke_pos);
        if (current_address->wrapwarn) {err_msg_mem_wrap(poke_pos);current_address->wrapwarn = false;}
        current_address->moved = false;
    }
    if (current_address->l_address.address > 0xffff || db > 0x10000 - current_address->l_address.address) {
        current_address->l_address.address = ((current_address->l_address.address + db - 1) & 0xffff) + 1;
        err_msg_pc_wrap(poke_pos);
    } else current_address->l_address.address += db;
    if (db > (~current_address->address & all_mem2)) {
        if (db - 1 + current_address->address == all_mem2) {
            current_address->wrapwarn = current_address->moved = true;
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->address = 0;
        } else {
            if (current_address->start != 0) err_msg2(ERROR_OUTOF_SECTION, NULL, poke_pos);
            if (current_address->end <= all_mem2) current_address->end = all_mem2 + 1;
            current_address->moved = false;
            current_address->address = (current_address->address + db) & all_mem2;
            err_msg_mem_wrap(poke_pos);current_address->wrapwarn = false;
        }
    } else current_address->address += db;
    return alloc_mem(current_address->mem, db);
}

/* --------------------------------------------------------------------------- */
static int get_command(void) {
    unsigned int no, also, felso, elozo;
    const uint8_t *label;
    uint8_t tmp[10];
    lpoint.pos++;
    label = pline + lpoint.pos;
    if (arguments.caseinsensitive) {
        int i, j;
        for (i = j = 0; i < 10; i++) {
            if ((uint8_t)(label[i] - 'a') <= ('z' - 'a')) continue;
            if ((uint8_t)(label[i] - 'A') > ('Z' - 'A')) break;
            while (j < i) { tmp[j] = label[j]; j++; }
            tmp[j++] = label[i] | 0x20;
        }
        if ((unsigned int)(i - 2) >= (10 - 2)) return lenof(command);
        if (j != 0) {
            while (j <= i) { tmp[j] = label[j]; j++; }
            label = tmp;
        }
    }

    also = 0;
    felso = lenof(command);
    no = lenof(command)/2;
    do {  /* do binary search */
        const uint8_t *cmd2 = (const uint8_t *)command[no];
        int s4 = label[0] - cmd2[1];
        if (s4 == 0) {
            int l = 1;
            for (;;) {
                s4 = label[l] - cmd2[l + 1];
                if (s4 != 0) break;
                l++;
                if (cmd2[l + 1] != 0) continue;
                if (label[l] >= '0') {
                    if ((uint8_t)(label[l] - 'a') <= ('z' - 'a')) break;
                    if ((label[l] & 0x80) != 0) {
                        if (arguments.to_ascii) {
                            uint32_t ch;
                            utf8in(label + l, &ch);
                            if ((uget_property(ch)->property & (id_Continue | id_Start)) != 0) return lenof(command);
                        }
                    } else if (label[l] <= '9' || (uint8_t)(label[l] - 'A') <= ('Z' - 'A') || label[l] == '_') return lenof(command);
                }
                lpoint.pos += l;
                return cmd2[0];
            }
        }
        elozo = no;
        no = ((s4 >= 0) ? (felso + (also = no)) : (also + (felso = no)))/2;
    } while (elozo != no);
    return lenof(command);
}

/* ------------------------------------------------------------------------------ */

static void set_cpumode(const struct cpu_s *cpumode) {
    current_cpu = cpumode;
    all_mem = cpumode->max_address;
    all_mem_bits = (all_mem == 0xffff) ? 16 : 24;
    select_opcodes(cpumode);
    listing_set_cpumode(listing, cpumode);
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
    ssize_t len;
    size_t sum, max;
    int prm;
    bool warn;
    uint8_t buff[16];
};

static void textrecursion(struct textrecursion_s *trec, Obj *val) {
    Iter *iter;
    iter_next_t iter_next;
    Obj *val2 = NULL;
    uval_t uval;

    if (trec->sum >= trec->max) return;
retry:
    switch (val->obj->type) {
    case T_STR:
        {
            Obj *tmp;
            Textconv_types m;
            switch (trec->prm) {
            case CMD_SHIFTL:
            case CMD_SHIFT: m = BYTES_MODE_SHIFT_CHECK; break;
            case CMD_NULL: m = BYTES_MODE_NULL_CHECK; break;
            default: m = BYTES_MODE_TEXT; break;
            }
            tmp = bytes_from_str((Str *)val, poke_pos, m);
            iter = tmp->obj->getiter(tmp);
            val_destroy(tmp);
            break;
        }
    case T_ERROR:
    case T_FLOAT:
    case T_INT:
    case T_BOOL:
        iter = NULL;
        val2 = val;
        goto doit;
    case T_CODE:
        val = ((Code *)val)->addr;
        goto retry;
    case T_GAP:
        iter = NULL;
        goto dogap;
    case T_BITS:
        {
            Obj *tmp;
            size_t bits = ((Bits *)val)->bits;
            if (bits == 0) return;
            if (bits <= 8) {
                iter = NULL;
                val2 = val;
                goto doit;
            }
            tmp = BYTES_OBJ->create(val, poke_pos);
            iter = tmp->obj->getiter(tmp);
            val_destroy(tmp);
            break;
        }
    case T_NONE:
        trec->warn = true;
        return;
    case T_BYTES:
        {
            ssize_t len = ((Bytes *)val)->len;
            if (len < 0) len = ~len;
            if (len == 0) return;
            if (len == 1) {
                iter = NULL;
                val2 = val;
                goto doit;
            }
        }
        /* fall through */
    default:
        iter = val->obj->getiter(val);
    }

    iter_next = iter->next;
    while ((val2 = iter_next(iter)) != NULL) {
        switch (val2->obj->type) {
        case T_BITS:
            {
                size_t bits = ((Bits *)val2)->bits;
                if (bits == 0) break;
                if (bits <= 8) goto doit;
            }
            /* fall through */
        case T_LIST:
        case T_TUPLE:
        case T_STR:
        rec:
            textrecursion(trec, val2);
            break;
        case T_GAP:
        dogap:
            if (trec->len > 0) {
                memcpy(pokealloc(trec->len), trec->buff, trec->len);
                trec->len = 0;
            }
            trec->len--;
            trec->sum++;
            if (iter == NULL) return;
            break;
        case T_BYTES:
            {
                ssize_t len = ((Bytes *)val2)->len;
                if (len < 0) len = ~len;
                if (len == 0) break;
                if (len > 1) goto rec;
            }
            /* fall through */
        default:
        doit:
            if (touval(val2, &uval, 8, poke_pos)) uval = 256 + '?';
            switch (trec->prm) {
            case CMD_SHIFT:
                if ((uval & 0x80) != 0) err_msg2(ERROR___NO_HIGH_BIT, NULL, poke_pos);
                uval &= 0x7f;
                break;
            case CMD_SHIFTL:
                if ((uval & 0x80) != 0) err_msg2(ERROR___NO_HIGH_BIT, NULL, poke_pos);
                uval <<= 1;
                break;
            case CMD_NULL:
                if (uval == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, poke_pos);
                /* fall through */
            default:
                break;
            }
            if (trec->len < 0) { memskip(-trec->len); trec->len = 0; }
            else if (trec->len >= (ssize_t)sizeof trec->buff) {
                memcpy(pokealloc(trec->len), trec->buff, trec->len);
                trec->len = 0;
            }
            trec->buff[trec->len++] = uval ^ outputeor;
            trec->sum++;
            if (iter == NULL) return;
            break;
        case T_NONE:
            trec->warn = true;
        }
        if (trec->sum >= trec->max) break;
    }
    val_destroy(&iter->v);
}

struct byterecursion_s {
    ssize_t len;
    uint8_t buff[16];
    bool warn;
};

static void byterecursion(Obj *val, int prm, struct byterecursion_s *brec, int bits) {
    Iter *iter;
    iter_next_t iter_next;
    Obj *val2;
    uint32_t ch2;
    uval_t uv;
    ival_t iv;
    const Type *type = val->obj;

    if (type != LIST_OBJ && type != TUPLE_OBJ) {
        if (type == GAP_OBJ) {
            if (brec->len > 0) {
                memcpy(pokealloc(brec->len), brec->buff, brec->len);
                brec->len = 0;
            }
            brec->len -= (unsigned int)abs(bits) / 8;
            return;
        }
        iter = NULL;
        if (type == NONE_OBJ) goto donone;
        val2 = val;
        goto doit;
    }
    iter = type->getiter(val);
    iter_next = iter->next;
    while ((val2 = iter_next(iter)) != NULL) {
        switch (val2->obj->type) {
        case T_LIST:
        case T_TUPLE:
            byterecursion(val2, prm, brec, bits);
            continue;
        case T_GAP:
            if (brec->len > 0) {
                memcpy(pokealloc(brec->len), brec->buff, brec->len);
                brec->len = 0;
            }
            brec->len -= (unsigned int)abs(bits) / 8;
            continue;
        default:
        doit:
            if (prm == CMD_RTA || prm == CMD_ADDR) {
                atype_t am;
                Obj *tmp = val2->obj->address(val2, &am);
                if (touval(tmp, &uv, (am == A_KR) ? 16 : all_mem_bits, poke_pos)) {
                    ch2 = 0;
                    break;
                }
                uv &= all_mem;
                switch (am) {
                case A_NONE:
                    if ((current_address->l_address.bank ^ uv) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, tmp, poke_pos);
                    break;
                case A_KR:
                    break;
                default:
                    err_msg_output_and_destroy(err_addressing(am, poke_pos));
                }
                ch2 = (prm == CMD_RTA) ? (uv - 1) : uv;
                break;
            }
            if (bits >= 0) {
                if (touval(val2, &uv, (unsigned int)bits, poke_pos)) {
                    if (diagnostics.pitfalls) {
                        static unsigned int once;
                        if (prm == CMD_BYTE && val2->obj == STR_OBJ) err_msg_byte_note(poke_pos);
                        else if (prm != CMD_RTA && prm != CMD_ADDR && once != pass) {
                            Error *err = val2->obj->ival(val2, &iv, (unsigned int)bits, poke_pos);
                            if (err != NULL) val_destroy(&err->v);
                            else {
                                const char *txt;
                                switch (prm) {
                                case CMD_BYTE:  txt = ".char"; break;
                                case CMD_LONG:  txt = ".lint"; break;
                                case CMD_DWORD: txt = ".dint"; break;
                                default:
                                case CMD_WORD:  txt = ".sint"; break;
                                }
                                err_msg_char_note(txt, poke_pos);
                                once = pass;
                            }
                        }
                    }
                    uv = 0;
                }
                ch2 = uv;
            } else {
                if (toival(val2, &iv, (unsigned int)-bits, poke_pos)) iv = 0;
                ch2 = (uint32_t)iv;
            }
            break;
        case T_NONE:
        donone:
            brec->warn = true;
            ch2 = 0;
        }
        if (brec->len < 0) {memskip(-brec->len);brec->len = 0;}
        else if (brec->len >= (ssize_t)(sizeof brec->buff) - 4) {memcpy(pokealloc(brec->len), brec->buff, brec->len); brec->len = 0;}
        ch2 ^= outputeor;
        brec->buff[brec->len++] = ch2;
        if (prm >= CMD_RTA) {
            brec->buff[brec->len++] = ch2 >> 8;
            if (prm >= CMD_LINT) {
                brec->buff[brec->len++] = ch2 >> 16;
                if (prm >= CMD_DINT) brec->buff[brec->len++] = ch2 >> 24;
            }
        }
        if (iter == NULL) return;
    }
    val_destroy(&iter->v);
}

static bool instrecursion(Obj *o1, int prm, unsigned int w, linepos_t epoint, struct linepos_s *epoints) {
    iter_next_t iter_next;
    Iter *iter = o1->obj->getiter(o1);
    Error *err;
    bool was = false;
    iter_next = iter->next;
    while ((o1 = iter_next(iter)) != NULL) {
        if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ) {
            if (instrecursion(o1, prm, w, epoint, epoints)) was = true;
        } else {
            err = instruction(prm, w, o1, epoint, epoints);
            if (err != NULL) err_msg_output_and_destroy(err); else was = true;
        }
    }
    val_destroy(&iter->v);
    return was;
}

static void logical_close(linepos_t epoint) {
    address_t diff;
    if (current_address->unionmode) {
        current_address->l_union = waitfor->u.cmd_logical.laddr;
        diff = 0;
    } else {
        diff = (current_address->address - waitfor->u.cmd_logical.addr) & all_mem2;
        if (diff != 0) {
            if (waitfor->u.cmd_logical.laddr.address > 0xffff || diff > 0x10000 - waitfor->u.cmd_logical.laddr.address) {
                current_address->l_address.address = ((waitfor->u.cmd_logical.laddr.address + diff - 1) & 0xffff) + 1;
                if (epoint != NULL) err_msg_pc_wrap(epoint);
            } else current_address->l_address.address = waitfor->u.cmd_logical.laddr.address + diff;
        } else current_address->l_address.address = waitfor->u.cmd_logical.laddr.address;
        current_address->l_address.bank = waitfor->u.cmd_logical.laddr.bank;
        if (current_address->l_address.bank > all_mem) {
            if (epoint != NULL) err_msg_big_address(epoint);
            current_address->l_address.bank &= all_mem;
        }
    }
    val_destroy(current_address->l_address_val);
    current_address->l_address_val = waitfor->u.cmd_logical.val;
    if (waitfor->u.cmd_logical.label != NULL) {
        set_size(waitfor->u.cmd_logical.label, diff, current_address->mem, waitfor->u.cmd_logical.addr, waitfor->u.cmd_logical.membp);
        val_destroy(&waitfor->u.cmd_logical.label->v);
    }
    current_section->logicalrecursion--;
}

static void virtual_close(linepos_t epoint) {
    if (waitfor->u.cmd_virtual.label != NULL) {
        address_t end = (current_address->end < current_address->address) ? current_address->address : current_address->end;
        set_size(waitfor->u.cmd_virtual.label, end - current_address->start, current_address->mem, current_address->start, waitfor->u.cmd_virtual.membp);
        val_destroy(&waitfor->u.cmd_virtual.label->v);
    }
    val_destroy(current_address->l_address_val);
    val_destroy(&current_address->mem->v);
    free(current_address);
    current_address = waitfor->u.cmd_virtual.section_address;
    if (current_address->l_address.bank > all_mem) {
        if (epoint != NULL) err_msg_big_address(epoint);
        current_address->l_address.bank &= all_mem;
    }
}

static void section_close(linepos_t epoint) {
    if (waitfor->u.cmd_section.label != NULL) {
        set_size(waitfor->u.cmd_section.label, current_address->address - waitfor->u.cmd_section.addr, current_address->mem, waitfor->u.cmd_section.addr, waitfor->u.cmd_section.membp);
        val_destroy(&waitfor->u.cmd_section.label->v);
    }
    current_section = waitfor->u.cmd_section.section;
    current_address = waitfor->u.cmd_section.section_address;
    if (current_address->l_address.bank > all_mem) {
        if (epoint != NULL) err_msg_big_address(epoint);
        current_address->l_address.bank &= all_mem;
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
        poke_pos = epoint;
        memskip(end - current_address->address);
    }
}

static const char *check_waitfor(void) {
    switch (waitfor->what) {
    case W_FI2:
    case W_FI: return ".fi";
    case W_SWITCH2:
    case W_SWITCH:
        if (waitfor->u.cmd_switch.val != NULL) val_destroy(waitfor->u.cmd_switch.val);
        return ".endswitch";
    case W_WEAK2:
        if (waitfor->u.cmd_weak.label != NULL) {set_size(waitfor->u.cmd_weak.label, current_address->address - waitfor->u.cmd_weak.addr, current_address->mem, waitfor->u.cmd_weak.addr, waitfor->u.cmd_weak.membp);val_destroy(&waitfor->u.cmd_weak.label->v);}
        strength--;
        /* fall through */
    case W_WEAK: return ".endweak";
    case W_ENDP2:
        if (waitfor->u.cmd_page.label != NULL) {set_size(waitfor->u.cmd_page.label, current_address->address - waitfor->u.cmd_page.addr, current_address->mem, waitfor->u.cmd_page.addr, waitfor->u.cmd_page.membp);val_destroy(&waitfor->u.cmd_page.label->v);}
        /* fall through */
    case W_ENDP: return ".endp";
    case W_ENDM:
        if (waitfor->u.cmd_macro.val != NULL) val_destroy(waitfor->u.cmd_macro.val);
        return ".endm";
    case W_ENDF: return ".endf";
    case W_NEXT3:
        pop_context();
        /* fall through */
    case W_NEXT: return ".next";
    case W_PEND:
        pop_context();
        if (waitfor->u.cmd_proc.label != NULL) {set_size(waitfor->u.cmd_proc.label, current_address->address - waitfor->u.cmd_proc.addr, current_address->mem, waitfor->u.cmd_proc.addr, waitfor->u.cmd_proc.membp);val_destroy(&waitfor->u.cmd_proc.label->v);}
        return ".pend";
    case W_BEND2:
        if (waitfor->u.cmd_block.label != NULL) {set_size(waitfor->u.cmd_block.label, current_address->address - waitfor->u.cmd_block.addr, current_address->mem, waitfor->u.cmd_block.addr, waitfor->u.cmd_block.membp);val_destroy(&waitfor->u.cmd_block.label->v);}
        /* fall through */
    case W_BEND:
        pop_context();
        return ".bend";
    case W_ENDN2:
    case W_ENDN:
        pop_context();
        return ".endn";
    case W_ENDC: return ".endc";
    case W_ENDS:
        if ((waitfor->skip & 1) != 0) current_address->unionmode = waitfor->u.cmd_struct.unionmode;
        /* fall through */
    case W_ENDS2: return ".ends";
    case W_SEND2:
        section_close(NULL);
        /* fall through */
    case W_SEND: return ".send";
    case W_ENDU:
        if ((waitfor->skip & 1) != 0) union_close(&lpoint);
        /* fall through */
    case W_ENDU2: return ".endu";
    case W_HERE2:
        logical_close(NULL);
        /* fall through */
    case W_HERE: return ".here";
    case W_ENDV2:
        virtual_close(NULL);
        /* fall through */
    case W_ENDV: return ".endv";
    case W_ENDU3:
    case W_ENDS3:
    case W_ENDM3:
    case W_ENDF3:
    case W_NEXT2:
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
    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
    if (sectionname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &opoint); return true;}
    tmp = find_new_section(&sectionname);
    if (tmp->usepass == 0 || tmp->defpass < pass - 1) {
        size_t ln = tmp->address.mem->mem.p, ln2 = tmp->address.mem->p;
        tmp->address.end = tmp->address.start = tmp->restart = tmp->address.address = 0;
        tmp->size = tmp->l_restart.address = tmp->l_restart.bank = tmp->address.l_address.address = tmp->address.l_address.bank = 0;
        if (tmp->usepass != 0 && tmp->usepass >= pass - 1) err_msg_not_defined(&sectionname, &opoint);
        else {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(&sectionname, epoint);
            fixeddig = false;
        }
        tmp->defpass = pass - 1;
        val_destroy(&tmp->address.mem->v);
        tmp->address.mem = new_memblocks(ln, ln2);
        tmp->address.mem->lastaddr = tmp->address.address;
        if (diagnostics.optimize) cpu_opt_invalidate();
    } else if (tmp->usepass != pass) {
        size_t ln = tmp->address.mem->mem.p, ln2 = tmp->address.mem->p;
        if (!tmp->address.moved) {
            if (tmp->address.end < tmp->address.address) tmp->address.end = tmp->address.address;
            tmp->address.moved = true;
        }
        tmp->size = tmp->address.end - tmp->address.start;
        tmp->address.end = tmp->address.start = tmp->restart;
        tmp->address.wrapwarn = false;
        tmp->address.address = tmp->restart;
        tmp->address.l_address = tmp->l_restart;
        val_destroy(&tmp->address.mem->v);
        tmp->address.mem = new_memblocks(ln, ln2);
        tmp->address.mem->lastaddr = tmp->address.address;
        if (diagnostics.optimize) cpu_opt_invalidate();
    }
    tmp->usepass = pass;
    waitfor->what = W_SEND2;
    waitfor->u.cmd_section.label = NULL;
    current_section = tmp;
    current_address = &tmp->address;
    return false;
}

static bool virtual_start(linepos_t epoint) {
    struct section_address_s *section_address;
    Obj *tmp = NULL;
    bool retval = false;

    if (diagnostics.optimize) cpu_opt_invalidate();
    listing_line(listing, epoint->pos);
    new_waitfor(W_ENDV2, epoint); waitfor->u.cmd_virtual.section_address = current_address; waitfor->u.cmd_virtual.label = NULL;
    section_address = (struct section_address_s *)mallocx(sizeof *section_address);
    section_address->wrapwarn = section_address->moved = false;

    do {
        struct values_s *vs;
        atype_t am;
        uval_t uval;

        if (!get_exp(0, 0, 1, epoint)) {retval = true;break;}
        vs = get_val();
        if (vs == NULL) break;
        if (touval(vs->val->obj->address(vs->val, &am), &uval, all_mem_bits, &vs->epoint)) {retval = true; break;}
        if (am != A_NONE && check_addr(am)) {
            err_msg_output_and_destroy(err_addressing(am, &vs->epoint));
            retval = true;
            break;
        }
        tmp = vs->val;
        section_address->address = uval;
        section_address->l_address.address = uval & 0xffff;
        section_address->l_address.bank = uval & all_mem & ~(address_t)0xffff;
        section_address->l_address_val = val_reference(tmp == NULL ? &int_value[0]->v : (tmp->obj == CODE_OBJ) ? ((Code *)tmp)->addr : tmp);
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
    if (!current_address->moved) {
        if (current_address->end < current_address->address) current_address->end = current_address->address;
        current_address->moved = true;
    }
    listing_line(listing, epoint->pos);
    do {
        {
            address_t max = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? all_mem2 : all_mem;
            if (touval(val->obj->address(val, &am), &uval, (max == 0xffff) ? 16 : (max == 0xffffff) ? 24 : 32, epoint2)) {
                break;
            }
        }
        if (am != A_NONE && check_addr(am)) {
            err_msg_output_and_destroy(err_addressing(am, epoint2));
            break;
        }
        if (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) {
            current_address->l_address.address = uval & 0xffff;
            current_address->l_address.bank = uval & all_mem & ~(address_t)0xffff;
            val_destroy(current_address->l_address_val);
            if (val->obj == CODE_OBJ) {
                current_address->l_address_val = val_reference(((Code *)val)->addr);
                val_destroy(val);
            } else current_address->l_address_val = val;
            addr = (address_t)uval & all_mem2;
            if (current_address->address != addr) {
                current_address->address = addr;
                memjmp(current_address->mem, current_address->address);
            }
            return;
        }
        laddr = current_address->l_address.address + (current_address->l_address.bank & all_mem); /* overflow included! */
        addr = (address_t)uval & all_mem;
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
        current_address->l_address.address = uval & 0xffff;
        current_address->l_address.bank = uval & all_mem & ~(address_t)0xffff;
        val_destroy(current_address->l_address_val);
        if (val->obj == CODE_OBJ) {
            current_address->l_address_val = val_reference(((Code *)val)->addr);
            val_destroy(val);
        } else current_address->l_address_val = val;
        return;
    } while (false);
    val_destroy(val);
}

static MUST_CHECK Oper *oper_from_token(int wht) {
    switch (wht) {
    case 'X':
        if (arguments.caseinsensitive == 0) {
            return NULL;
        }
        /* fall through */
    case 'x': return &o_X;
    case '*': return &o_MUL;
    case '+': return &o_ADD;
    case '-': return &o_SUB;
    case '/': return &o_DIV;
    case '%': return &o_MOD;
    case '|': return &o_OR;
    case '&': return &o_AND;
    case '^': return &o_XOR;
    case '.': return &o_MEMBER;
    default: return NULL;
    }
}

static MUST_CHECK Oper *oper_from_token2(int wht, int wht2) {
    if (wht == wht2) {
        switch (wht) {
        case '&': return &o_LAND;
        case '|': return &o_LOR;
        case '>': return &o_RSHIFT;
        case '<': return &o_LSHIFT;
        case '.': return &o_CONCAT;
        case '*': return &o_EXP;
        default: return NULL;
        }
    }
    if (wht2 == '?') {
        switch (wht) {
        case '<': return &o_MIN;
        case '>': return &o_MAX;
        case ':': return &o_COND;
        default: return NULL;
        }
    }
    return NULL;
}

static MUST_CHECK Obj *tuple_scope_light(Obj **o, linepos_t epoint) {
    Obj *nf, *val = *o;
    if (val->obj != NAMESPACE_OBJ) {
        val_destroy(val);
        *o = val = (Obj *)new_namespace(current_file_list, epoint);
    } else ((Namespace *)val)->backr = ((Namespace *)val)->forwr = 0;
    push_context((Namespace *)val);
    nf = compile();
    pop_context();
    return nf;
}

static MUST_CHECK Obj *tuple_scope(Label *newlabel, Obj **o) {
    Obj *nf;
    size_t size;
    Code *code;
    address_t oaddr;
    size_t newmembp;
    Obj *val = *o;

    if (diagnostics.optimize && newlabel->ref) cpu_opt_invalidate();
    oaddr = current_address->address;
    if (val->obj == CODE_OBJ) {
        Obj *tmp = get_star_value(current_address->l_address_val);
        code = (Code *)val;
        if (!tmp->obj->same(tmp, code->addr)) {
            val_destroy(code->addr); code->addr = tmp;
            if (newlabel->usepass >= pass) {
                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
                fixeddig = false;
            }
        } else val_destroy(tmp);
        if (code->requires != current_section->requires || code->conflicts != current_section->conflicts || code->offs != 0) {
            code->requires = current_section->requires;
            code->conflicts = current_section->conflicts;
            code->offs = 0;
            if (newlabel->usepass >= pass) {
                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &newlabel->epoint);
                fixeddig = false;
            }
        }
        code->names->backr = code->names->forwr = 0;
    } else {
        code = new_code();
        code->addr = get_star_value(current_address->l_address_val);
        code->size = 0;
        code->offs = 0;
        code->dtype = D_NONE;
        code->pass = 0;
        code->memblocks = ref_memblocks(current_address->mem);
        code->names = new_namespace(current_file_list, &newlabel->epoint);
        code->requires = current_section->requires;
        code->conflicts = current_section->conflicts;
        val_destroy(val);
        *o = val = &code->v;
    }
    newmembp = get_mem(current_address->mem);
    code->apass = pass;
    push_context(((Code *)val)->names);
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
        val_destroy(&code->memblocks->v);
        code->memblocks = ref_memblocks(current_address->mem);
    }
    code->memaddr = oaddr;
    code->membp = newmembp;
    return nf;
}

static MUST_CHECK bool list_extend(List *lst) {
    Obj **vals;
    size_t o = lst->len, n;
    if (lst->data == lst->u.val) {
        n = 16;
        vals = (Obj **)malloc(n * sizeof *lst->data);
        if (vals == NULL) return true;
        memcpy(vals, lst->u.val, o * sizeof *lst->data);
    } else {
        if (o < 256) n = o * 2;
        else {
            n = o + 256;
            if (/*n < 256 ||*/ n > SIZE_MAX / sizeof *lst->data) return true; /* overflow */
        }
        vals = (Obj **)realloc(lst->data, n * sizeof *lst->data);
        if (vals == NULL) return true;
    }
    while (o < n) vals[o++] = (Obj *)ref_none();
    lst->data = vals;
    lst->len = n;
    lst->u.max = n;
    return false;
}

static void list_shrink(List *lst, size_t i) {
    size_t j = i;
    while (j < lst->len) val_destroy(lst->data[j++]);
    lst->len = i;
    if (lst->data != lst->u.val) {
        if (lst->len <= lenof(lst->u.val)) {
            memcpy(lst->u.val, lst->data, lst->len * sizeof *lst->data);
            free(lst->data);
            lst->data = lst->u.val;
        } else {
            Obj **v = (Obj **)realloc(lst->data, lst->len * sizeof *lst->data);
            if (v != NULL) {
                lst->data = v;
                lst->u.max = lst->len;
            }
        }
    }
}

static size_t for_command(Label *newlabel, List *lst, linepos_t epoint) {
    int wht;
    line_t lin;
    int nopos = -1;
    struct linepos_s epoint2, epoint3;
    uint8_t *expr;
    struct {
        size_t p, len;
        Label **data;
        Label *val[4];
    } labels;
    Label *label;
    Obj *val, *nf = NULL;
    struct star_s *s;
    struct avltree *stree_old;
    line_t ovline, lvline;
    bool starexists, foreach = false;
    Iter *iter = NULL;
    size_t i = 0;
    labels.p = 0;

    if (diagnostics.optimize) cpu_opt_invalidate();
    listing_line(listing, epoint->pos);
    new_waitfor(W_NEXT, epoint);waitfor->skip = 0;

    do { /* label */
        bool labelexists;
        str_t varname;
        epoint2 = lpoint;

        varname.data = pline + lpoint.pos; varname.len = get_label();
        if (varname.len == 0) break;

        if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &epoint2); goto error;}
        ignore(); wht = here();
        if (wht == ',') {
            lpoint.pos++;
            foreach = true;
            val = (Obj *)ref_none();
        } else {
            if (wht != '=' || foreach) {
                if (wht == ':' && pline[lpoint.pos + 1] == '=' && !arguments.tasmcomp && !foreach) lpoint.pos += 2;
                else {
                    size_t l = get_label();
                    if (l != 2 || (pline[lpoint.pos-2] | arguments.caseinsensitive) != 'i' || (pline[lpoint.pos-1] | arguments.caseinsensitive) != 'n') {
                        lpoint.pos -= l;
                        err_msg(ERROR______EXPECTED, foreach ? "',' or 'in'" : "':=' or ',' or 'in'"); goto error;
                    } else foreach = true;
                }
            } else lpoint.pos++;
            if (foreach) {
                ignore();
                epoint3 = lpoint;
                if (!get_exp(0, 1, 0, &epoint3)) goto error;
                val = get_vals_tuple();
                if (val->obj == ERROR_OBJ) {
                    err_msg_output((Error *)val);
                } else if (val->obj->getiter == DEFAULT_OBJ->getiter) {
                    if (val != &none_value->v) {
                        Error *err = new_error_obj(ERROR______NOT_ITER, val, &epoint3);
                        val_destroy(val); val = &err->v;
                        err_msg_output((Error *)val);
                    }
                } else {
                    iter = val->obj->getiter(val);
                }
                val_destroy(val); val = (Obj *)ref_none();
            } else {
                if (!get_exp(1, 1, 1, &lpoint)) goto error;
                val = pull_val(NULL);
            }
        }
        label = new_label(&varname, (varname.data[0] == '_') ? cheap_context : current_context, strength, &labelexists, current_file_list);
        if (foreach) {
            if (labels.p == 0) {
                labels.len = lenof(labels.val);
                labels.data = labels.val;
            } else if (labels.p >= labels.len) {
                labels.len += 16;
                if (/*labels.len < 16 ||*/ labels.len > SIZE_MAX / sizeof *labels.data) err_msg_out_of_memory(); /* overflow */
                if (labels.data == labels.val) {
                    labels.data = (Label **)mallocx(labels.len * sizeof *labels.data);
                    memcpy(labels.data, labels.val, sizeof labels.val);
                } else labels.data = (Label **)reallocx(labels.data, labels.len * sizeof *labels.data);
            }
            labels.data[labels.p++] = label;
        }
        if (labelexists) {
            if (label->constant) {
                err_msg_double_defined(label, &varname, &epoint2);
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
        ignore();
    } while (wht == ',');
    if (foreach) {
        if (here() != 0 && here() != ';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    } else {
        if (here() != ',') {err_msg(ERROR______EXPECTED, "','");
        error:
            if (labels.p != 0 && labels.data != labels.val) free(labels.data);
            return i;
        }
        lpoint.pos++;ignore();
    }

    s = new_star(vline, &starexists); stree_old = star_tree; ovline = vline;
    if (starexists && s->addr != star) {
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    }
    s->addr = star;
    star_tree = &s->tree; lvline = vline = 0;
    lin = lpoint.line;

    if (foreach) {
        new_waitfor(W_NEXT2, epoint);
        waitfor->u.cmd_rept.breakout = false;
        if (iter != NULL) {
            iter_next_t iter_next = iter->next;
            Obj *val2;

            while ((val2 = iter_next(iter)) != NULL) {
                if (nopos < 0) nopos = 0;
                else if ((waitfor->skip & 1) != 0) listing_line_cut(listing, waitfor->epoint.pos);
                if (labels.p == 1) {
                    val_destroy(label->value);
                    label->value = val_reference(val2);
                } else {
                    Iter *iter2 = val2->obj->getiter(val2);
                    iter_next_t iter2_next = iter2->next;
                    size_t j;
                    for (j = 0; j < labels.p && (val2 = iter2_next(iter2)) != NULL; j++) {
                        val_destroy(labels.data[j]->value);
                        labels.data[j]->value = val_reference(val2);
                    }
                    j += iter2->len(iter2);
                    if (j != labels.p) err_msg_cant_unpack(labels.p, j, epoint);
                }
                lpoint.line = lin;
                waitfor->skip = 1; lvline = vline;
                if (lst != NULL) {
                    if (i >= lst->len && list_extend(lst)) {i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL;}
                    else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                    else nf = tuple_scope(newlabel, &lst->data[i]);
                    i++;
                } else nf = compile();
                if (nf == NULL || waitfor->u.cmd_rept.breakout) {
                    break;
                }
            }
            val_destroy(&iter->v);
        }
        if (labels.p != 0 && labels.data != labels.val) free(labels.data);
        expr = NULL;
    } else {
        struct linepos_s apoint = lpoint, bpoint = {0, 0};
        line_t xlin = lpoint.line;
        struct oper_s tmp;
        const uint8_t *oldpline = pline;
        size_t lentmp = strlen((const char *)pline) + 1;
        expr = (uint8_t *)mallocx(lentmp);
        memcpy(expr, pline, lentmp); label = NULL;
        new_waitfor(W_NEXT2, epoint);
        waitfor->u.cmd_rept.breakout = false;
        tmp.op = NULL;
        for (;;) {
            lpoint = apoint;
            if (here() != ',' && here() != 0) {
                struct values_s *vs;
                bool truth;
                if (!get_exp(1, 1, 1, &apoint)) break;
                vs = get_val();
                if (tobool(vs, &truth)) break;
                if (!truth) break;
            }
            if (nopos < 0) {
                ignore();if (here() != ',') {err_msg(ERROR______EXPECTED, "','"); break;}
                lpoint.pos++;ignore();
                if (here() == 0 || here() == ';') {bpoint.pos = 0; nopos = 0;}
                else bpoint = lpoint;
            } else {
                if ((waitfor->skip & 1) != 0) listing_line_cut(listing, waitfor->epoint.pos);
            }
            waitfor->skip = 1;lvline = vline;
            if (lst != NULL) {
                if (i >= lst->len && list_extend(lst)) { i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL; }
                else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                else nf = tuple_scope(newlabel, &lst->data[i]);
                i++;
            } else nf = compile();
            xlin = lpoint.line;
            pline = expr;
            lpoint.line = lin;
            if (nf == NULL || waitfor->u.cmd_rept.breakout) break;
            if (nopos < 0) {
                str_t varname;
                Namespace *context;
                bool labelexists;
                lpoint = bpoint;
                varname.data = pline + lpoint.pos; varname.len = get_label();
                if (varname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &bpoint);break;}
                if (varname.len > 1 && varname.data[0] == '_' && varname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &varname, &bpoint); break;}
                ignore(); wht = here();
                while (wht != 0 && !arguments.tasmcomp) {
                    int wht2 = pline[lpoint.pos + 1];
                    if (wht2 == '=') {
                        if (wht == ':') {
                            wht = '=';
                            lpoint.pos++;
                            break;
                        }
                        tmp.op = oper_from_token(wht);
                        if (tmp.op == NULL) break;
                        epoint3 = lpoint;
                        lpoint.pos += 2;
                    } else if (wht2 != 0 && pline[lpoint.pos + 2] == '=') {
                        if (wht == '?') break;
                        tmp.op = oper_from_token2(wht, wht2);
                        if (tmp.op == NULL) break;
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
                if (tmp.op == NULL) {
                    if (wht != '=') {err_msg(ERROR______EXPECTED, "':='"); break;}
                    lpoint.pos++;ignore();
                    label = new_label(&varname, context, strength, &labelexists, current_file_list);
                    if (labelexists) {
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
                        label->value = (Obj *)ref_none();
                        label->epoint = bpoint;
                    }
                } else {
                    label = find_label3(&varname, context, strength);
                    if (label == NULL) {err_msg_not_defined2(&varname, context, false, &bpoint); break;}
                    if (label->constant) {err_msg_not_variable(label, &varname, &bpoint); break;}
                    if (diagnostics.case_symbol && str_cmp(&varname, &label->name) != 0) err_msg_symbol_case(&varname, label, &bpoint);
                }
                bpoint = lpoint; nopos = 1;
            }
            if (nopos > 0) {
                struct linepos_s epoints[3];
                lpoint = bpoint;
                if (!get_exp(0, 0, 0, &bpoint)) break;
                val = get_vals_addrlist(epoints);
                if (tmp.op != NULL) {
                    bool minmax = (tmp.op == &o_MIN) || (tmp.op == &o_MAX);
                    Obj *result2, *val1 = label->value;
                    tmp.v1 = val1;
                    tmp.v2 = val;
                    tmp.inplace = (tmp.v1->refcount == 1 && !minmax) ? tmp.v1 : NULL;
                    result2 = tmp.v1->obj->calc2(&tmp);
                    if (minmax) {
                        if (result2 == &true_value->v) val_replace(&result2, val1);
                        else if (result2 == &false_value->v) val_replace(&result2, val);
                    }
                    val_destroy(val);
                    val = result2;
                }
                val_destroy(label->value);
                label->value = val;
                label->usepass = 0;
            }
        }
        pline = oldpline;
        lpoint.line = xlin;
    }
    if (nf != NULL) {
        if ((waitfor->skip & 1) != 0) listing_line(listing, waitfor->epoint.pos);
        else listing_line_cut2(listing, waitfor->epoint.pos);
    }
    close_waitfor(W_NEXT2);
    free(expr);
    if (nf != NULL) close_waitfor(W_NEXT);
    star_tree = stree_old; vline = ovline + vline - lvline;
    return i;
}

static size_t rept_command(Label *newlabel, List *lst, linepos_t epoint) {
    uval_t cnt;
    Obj *nf;
    size_t i = 0;

    if (diagnostics.optimize) cpu_opt_invalidate();
    listing_line(listing, epoint->pos);
    new_waitfor(W_NEXT, epoint);waitfor->skip = 0;
    if (!get_exp(0, 1, 1, epoint)) cnt = 0;
    else {
        struct values_s *vs = get_val();
        if (touval2(vs->val, &cnt, 8 * sizeof cnt, &vs->epoint)) cnt = 0;
    }
    if (cnt > 0) {
        line_t lin = lpoint.line;
        bool starexists;
        struct star_s *s = new_star(vline, &starexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline, lvline;

        if (starexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree = &s->tree;vline = 0;
        new_waitfor(W_NEXT2, epoint);
        waitfor->u.cmd_rept.breakout = false;
        for (;;) {
            lpoint.line = lin;
            waitfor->skip = 1; lvline = vline;
            if (lst != NULL) {
                if (i >= lst->len && list_extend(lst)) { i = lst->len - 1; err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint); nf = NULL; }
                else if (newlabel == NULL) nf = tuple_scope_light(&lst->data[i], epoint);
                else nf = tuple_scope(newlabel, &lst->data[i]);
                i++;
            } else nf = compile();
            if (nf == NULL || waitfor->u.cmd_rept.breakout || (--cnt) == 0) {
                break;
            }
            if ((waitfor->skip & 1) != 0) listing_line_cut(listing, waitfor->epoint.pos);
        }
        if (nf != NULL) {
            if ((waitfor->skip & 1) != 0) listing_line(listing, waitfor->epoint.pos);
            else listing_line_cut2(listing, waitfor->epoint.pos);
        }
        close_waitfor(W_NEXT2);
        if (nf != NULL) close_waitfor(W_NEXT);
        star_tree = stree_old; vline = ovline + vline - lvline;
    }
    return i;
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
    struct anonident_s {
        uint8_t dir;
        uint8_t padding[3];
        int32_t count;
    } anonident;
    struct {
        uint8_t type;
        uint8_t padding[3];
        line_t vline;
        struct avltree *star_tree;
    } anonident2;
    struct linepos_s epoint;

    while (nobreak) {
        if (mtranslate()) break; /* expand macro parameters, if any */
        newlabel = NULL;
        labelname.len = 0;ignore();epoint = lpoint; mycontext = current_context;
        if (current_address->unionmode) {
            if (current_address->l_address.address != current_address->l_union.address || current_address->l_address.bank != current_address->l_union.bank) {
                current_address->l_address = current_address->l_union;
                if (current_address->l_address.bank > all_mem) {
                    err_msg_big_address(&epoint);
                    current_address->l_address.bank &= all_mem;
                }
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
        star = (current_address->l_address.address & 0xffff) | current_address->l_address.bank;
        wht = here();
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
                if (sizeof(anonident) != sizeof(anonident.dir) + sizeof(anonident.padding) + sizeof(anonident.count)) memset(&anonident, 0, sizeof anonident);
                else memset(anonident.padding, 0, sizeof anonident.padding);
                anonident.dir = (uint8_t)wht;
                anonident.count = (int32_t)((wht == '-') ? current_context->backr++ : current_context->forwr++);
                labelname.data = (const uint8_t *)&anonident;labelname.len = sizeof anonident;
                goto hh;
            default:
                lpoint.pos--;
            }
            break;
        default:
            break;
        }
        labelname.data = pline + lpoint.pos; labelname.len = get_label();
        if (labelname.len != 0) {
            struct linepos_s cmdpoint;
            bool islabel, error;
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
                labelname.data = pline + lpoint.pos; labelname.len = get_label();
                if (labelname.len == 0) {
                    if (!error) err_msg2(ERROR______EXPECTED, "a symbol is", &lpoint);
                    goto breakerr;
                }
                if (!error) {
                    Namespace *context = get_namespace(tmp2->value);
                    if (context == NULL) {
                        Ident *idn = new_ident(&labelname);
                        epoint.pos--;
                        err_msg_invalid_oper(&o_MEMBER, tmp2->value, &idn->v, &epoint);
                        val_destroy(&idn->v);
                        error = true;
                    } else mycontext = context;
                }
            }
            if (!islabel && labelname.data[0] == '_') {
                mycontext = cheap_context;
            }
            if (here() == ':' && pline[lpoint.pos + 1] != '=') {islabel = true; lpoint.pos++;}
            if (!islabel && labelname.len == 3 && (prm = lookup_opcode(labelname.data)) >=0) {
                if (!error) goto as_opcode; else continue;
            }
            if (false) {
            hh: islabel = true; error = (waitfor->skip & 1) == 0;
            }
            ignore();wht = here();
            if (error) {epoint = lpoint; goto jn;} /* skip things if needed */
            if (labelname.len > 1 && labelname.data[0] == '_' && labelname.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &labelname, &epoint); goto breakerr;}
            while (wht != 0 && !arguments.tasmcomp) {
                bool minmax;
                Label *label;
                struct oper_s tmp;
                Obj *result2, *val2;
                struct linepos_s epoint2, epoint3;
                int wht2 = pline[lpoint.pos + 1];

                if (wht2 == '=') {
                    if (wht == ':') {
                        if (labelname.data[0] == '*') {
                            lpoint.pos++;
                            goto starassign;
                        }
                        lpoint.pos += 2;
                        ignore();
                        goto itsvar;
                    }
                    tmp.op = oper_from_token(wht);
                    if (tmp.op == NULL) break;
                    epoint3 = lpoint;
                    lpoint.pos += 2;
                } else if (wht2 != 0 && pline[lpoint.pos + 2] == '=') {
                    tmp.op = oper_from_token2(wht, wht2);
                    if (tmp.op == NULL) break;
                    epoint3 = lpoint;
                    lpoint.pos += 3;
                } else break;

                if (labelname.data[0] == '-') {current_context->backr--;((struct anonident_s *)labelname.data)->count--;}
                else if (labelname.data[0] == '+') {current_context->forwr--;((struct anonident_s *)labelname.data)->count--;}
                ignore();
                epoint2 = lpoint;
                if (labelname.data[0] == '*') {
                    label = NULL;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    val = get_star_value(current_address->l_address_val);
                } else if (tmp.op == &o_COND) {
                    label = NULL; val = NULL;
                } else {
                    label = find_label3(&labelname, mycontext, strength);
                    if (label == NULL) {
                        if (tmp.op == &o_MUL) {
                            if (diagnostics.star_assign) {
                                err_msg_star_assign(&epoint3);
                                if (pline[lpoint.pos] == '*') err_msg_compound_note(&epoint3);
                            }
                            lpoint.pos = epoint3.pos;
                            wht = '*';
                            break;
                        }
                        err_msg_not_defined2(&labelname, mycontext, false, &epoint);
                        goto breakerr;
                    }
                    if (label->constant) {
                        if (tmp.op == &o_MUL) {
                            if (diagnostics.star_assign) {
                                err_msg_star_assign(&epoint3);
                                if (pline[lpoint.pos] == '*') err_msg_compound_note(&epoint3);
                            }
                            lpoint.pos = epoint3.pos;
                            wht = '*';
                            break;
                        }
                        err_msg_not_variable(label, &labelname, &epoint); goto breakerr;
                        goto breakerr;
                    }
                    if (diagnostics.case_symbol && str_cmp(&labelname, &label->name) != 0) err_msg_symbol_case(&labelname, label, &epoint);
                    val = label->value;
                    label->usepass = 0;
                }
                if (here() == 0 || here() == ';') val2 = (Obj *)ref_addrlist(null_addrlist);
                else {
                    struct linepos_s epoints[3];
                    bool oldreferenceit = referenceit;
                    referenceit &= 1; /* not good... */
                    if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                    val2 = get_vals_addrlist(epoints);
                    referenceit = oldreferenceit;
                }
                oaddr = current_address->address;
                if (val == NULL) {
                    bool labelexists;
                    label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                    if (labelexists) {
                        if (label->constant) {
                            err_msg_not_variable(label, &labelname, &epoint); goto breakerr;
                            val_destroy(val2);
                            goto breakerr;
                        }
                        if (label->defpass != pass) {
                            label->ref = false;
                            label->defpass = pass;
                        } else {
                            val_destroy(val2);
                            goto finish;
                        }
                        label->owner = false;
                        if (label->file_list != current_file_list) {
                            label_move(label, &labelname, current_file_list);
                        }
                        label->epoint = epoint;
                        val_destroy(label->value);
                        label->value = val2;
                        label->usepass = 0;
                    } else {
                        label->constant = false;
                        label->owner = false;
                        label->value = val2;
                        label->epoint = epoint;
                    }
                    goto finish;
                }
                minmax = (tmp.op == &o_MIN) || (tmp.op == &o_MAX);
                tmp.v1 = val;
                tmp.v2 = val2;
                tmp.epoint = &epoint;
                tmp.epoint2 = &epoint2;
                tmp.epoint3 = &epoint3;
                tmp.inplace = (tmp.v1->refcount == 1 && !minmax) ? tmp.v1 : NULL;
                result2 = tmp.v1->obj->calc2(&tmp);
                if (minmax) {
                    if (result2 == &true_value->v) val_replace(&result2, val);
                    else if (result2 == &false_value->v) val_replace(&result2, val2);
                }
                val_destroy(val2);
                if (label != NULL) {
                    listing_equal(listing, result2);
                    if (label->file_list != current_file_list) {
                        label_move(label, &labelname, current_file_list);
                    }
                    label->epoint = epoint;
                    val_destroy(label->value);
                    label->value = result2;
                } else {
                    val_destroy(val);
                    starhandle(result2, &epoint, &epoint2);
                }
                goto finish;
            }
            switch (wht) {
            case '=':
                { /* variable */
                    struct linepos_s epoints[3];
                    Label *label;
                    bool labelexists;
                starassign:
                    if (labelname.data[0] == '*') {
                        label = NULL;
                        if (diagnostics.optimize) cpu_opt_invalidate();
                    } else label = find_label3(&labelname, mycontext, strength);
                    lpoint.pos++; ignore();
                    epoints[0] = lpoint; /* for no elements! */
                    if (here() == 0 || here() == ';') {
                        if (labelname.data[0] == '*') {
                            err_msg(ERROR______EXPECTED, "an expression is");
                            goto breakerr;
                        }
                        val = (Obj *)ref_addrlist(null_addrlist);
                    } else {
                        bool oldreferenceit = referenceit;
                        if (label != NULL && !label->ref) {
                            referenceit = false;
                        }
                        if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                        val = get_vals_addrlist(epoints);
                        referenceit = oldreferenceit;
                    }
                    if (labelname.data[0] == '*') {
                        starhandle(val, &epoint, &epoints[0]);
                        goto finish;
                    }
                    if (label != NULL) {
                        labelexists = true;
                    } else label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                    oaddr = current_address->address;
                    listing_equal(listing, val);
                    label->ref = false;
                    if (labelexists) {
                        if (label->defpass == pass) {
                            val_destroy(val);
                            err_msg_double_defined(label, &labelname, &epoint);
                        } else {
                            if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = false;
                            if (label->file_list != current_file_list) {
                                label_move(label, &labelname, current_file_list);
                            }
                            label->epoint = epoint;
                            const_assign(label, val);
                        }
                    } else {
                        if (!constcreated && temporary_label_branch == 0) {
                            if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                            constcreated = true;
                        }
                        label->constant = true;
                        label->owner = false;
                        label->value = val;
                        label->epoint = epoint;
                    }
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
                        Label *label;
                        bool labelexists;
                    itsvar:
                        label = find_label3(&labelname, mycontext, strength);
                        if (here() == 0 || here() == ';') val = (Obj *)ref_addrlist(null_addrlist);
                        else {
                            struct linepos_s epoints[3];
                            bool oldreferenceit = referenceit;
                            referenceit &= 1; /* not good... */
                            if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                            val = get_vals_addrlist(epoints);
                            referenceit = oldreferenceit;
                        }
                        if (label != NULL) {
                            labelexists = true;
                            if (diagnostics.case_symbol && str_cmp(&labelname, &label->name) != 0) err_msg_symbol_case(&labelname, label, &epoint);
                        } else label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        oaddr = current_address->address;
                        listing_equal(listing, val);
                        if (labelexists) {
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
                        goto finish;
                    }
                case CMD_LBL:
                    { /* label */
                        Label *label;
                        Lbl *lbl;
                        bool labelexists;
                        listing_line(listing, 0);
                        label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        lbl = (Lbl *)val_alloc(LBL_OBJ);
                        lbl->sline = epoint.line;
                        lbl->waitforp = waitfor_p;
                        lbl->file_list = current_file_list;
                        lbl->parent = current_context;
                        if (labelexists) {
                            if (label->defpass == pass) {
                                val_destroy(&lbl->v);
                                err_msg_double_defined(label, &labelname, &epoint);
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                const_assign(label, &lbl->v);
                            }
                        } else {
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = true;
                            label->value = &lbl->v;
                            label->epoint = epoint;
                        }
                        if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &cmdpoint);
                        label->ref = false;
                        goto finish;
                    }
                case CMD_NAMESPACE:
                    { /* namespace */
                        Label *label;
                        struct values_s *vs;
                        bool labelexists;
                        listing_line(listing, 0);
                        new_waitfor(W_ENDN, &cmdpoint);
                        if (get_exp(0, 0, 1, &cmdpoint)) {
                            vs = get_val();
                            if (vs != NULL) {
                                val = vs->val;
                                val = (Obj *)get_namespace(val);
                                if (val == NULL) err_msg_wrong_type2(vs->val, NULL, &vs->epoint);
                            } else val = NULL;
                        } else val = NULL;
                        label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &labelname, &epoint);
                            else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = (val == NULL);
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                if (val != NULL) const_assign(label, val_reference(val));
                                else {
                                    label->defpass = pass;
                                    if (label->value->obj != NAMESPACE_OBJ) {
                                        val_destroy(label->value);
                                        label->value = (Obj *)new_namespace(current_file_list, &epoint);
                                    } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                                }
                            }
                        } else {
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = (val == NULL);
                            label->value = (val != NULL) ? val_reference(val) : (Obj *)new_namespace(current_file_list, &epoint);
                            label->epoint = epoint;
                        }
                        if (label->value->obj == NAMESPACE_OBJ) {
                            push_context((Namespace *)label->value);
                            waitfor->what = W_ENDN2;
                        } else push_context(current_context);
                        label->ref = false;
                        goto finish;
                    }
                case CMD_MACRO:/* .macro */
                case CMD_SEGMENT:
                    {
                        Label *label;
                        Macro *macro;
                        Type *obj = (prm == CMD_MACRO) ? MACRO_OBJ : SEGMENT_OBJ;
                        bool labelexists;
                        listing_line(listing, 0);
                        new_waitfor(W_ENDM, &cmdpoint);waitfor->skip = 0;
                        label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        macro = (Macro *)val_alloc(obj);
                        macro->file_list = current_file_list;
                        macro->line = epoint.line;
                        macro->recursion_pass = 0;
                        get_macro_params(&macro->v);
                        if (labelexists) {
                            macro->retval = (label->value->obj == obj) && ((Macro *)label->value)->retval;
                            if (label->defpass == pass) {
                                waitfor->u.cmd_macro.val = &macro->v;
                                err_msg_double_defined(label, &labelname, &epoint);
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                const_assign(label, &macro->v);
                                waitfor->u.cmd_macro.val = val_reference(label->value);
                            }
                        } else {
                            macro->retval = false;
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = true;
                            label->value = &macro->v;
                            label->epoint = epoint;
                            waitfor->u.cmd_macro.val = val_reference(&macro->v);
                        }
                        label->ref = false;
                        goto finish;
                    }
                case CMD_FUNCTION:
                    {
                        Label *label;
                        Mfunc *mfunc;
                        bool labelexists;
                        listing_line(listing, 0);
                        new_waitfor(W_ENDF, &cmdpoint);waitfor->skip = 0;
                        if (temporary_label_branch != 0) {err_msg2(ERROR___NOT_ALLOWED, ".function", &cmdpoint);goto breakerr;}
                        label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        mfunc = (Mfunc *)val_alloc(MFUNC_OBJ);
                        mfunc->file_list = current_file_list;
                        mfunc->line = epoint.line;
                        mfunc->recursion_pass = 0;
                        mfunc->argc = 0;
                        mfunc->param = NULL; /* might be recursive through init */
                        mfunc->nslen = 0;
                        mfunc->namespaces = NULL;
                        if (labelexists) {
                            if (label->defpass == pass) {
                                val_destroy(&mfunc->v);
                                err_msg_double_defined(label, &labelname, &epoint);
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                get_func_params(mfunc);
                                get_namespaces(mfunc);
                                const_assign(label, &mfunc->v);
                            }
                        } else {
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = true;
                            label->value = &mfunc->v;
                            label->epoint = epoint;
                            get_func_params(mfunc);
                            get_namespaces(mfunc);
                        }
                        label->ref = false;
                        goto finish;
                    }
                case CMD_STRUCT:
                case CMD_UNION:
                    {
                        Label *label;
                        Struct *structure;
                        struct section_address_s section_address, *oldsection_address = current_address;
                        uval_t provides = current_section->provides, requires = current_section->requires, conflicts = current_section->conflicts;
                        bool labelexists, doubledef = false;
                        Type *obj = (prm == CMD_STRUCT) ? STRUCT_OBJ : UNION_OBJ;

                        if (diagnostics.optimize) cpu_opt_invalidate();
                        new_waitfor((prm==CMD_STRUCT) ? W_ENDS : W_ENDU, &cmdpoint);waitfor->skip = 0;
                        label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);

                        current_section->provides = ~(uval_t)0;current_section->requires = current_section->conflicts = 0;
                        section_address.wrapwarn = section_address.moved = false;
                        section_address.unionmode = (prm == CMD_UNION);
                        section_address.address = section_address.start = section_address.end = 0;
                        section_address.l_start.address = section_address.l_start.bank = 0;
                        section_address.l_union.address = section_address.l_union.bank = 0;
                        section_address.l_address.address = section_address.l_address.bank = 0;
                        section_address.l_address_val = (Obj *)ref_int(int_value[0]);
                        section_address.mem = new_memblocks(0, 0);
                        section_address.mem->lastaddr = 0;
                        current_address = &section_address;

                        structure = (Struct *)val_alloc(obj);
                        structure->file_list = current_file_list;
                        structure->line = epoint.line;
                        structure->recursion_pass = 0;
                        get_macro_params(&structure->v);
                        if (labelexists) {
                            structure->retval = (label->value->obj == obj) && ((Struct *)label->value)->retval;
                            if (label->defpass == pass) {
                                doubledef = true;
                                structure->size = 0;
                                structure->names = new_namespace(current_file_list, &epoint);
                                err_msg_double_defined(label, &labelname, &epoint);
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                label->constant = true;
                                label->owner = true;
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->epoint = epoint;
                                if (label->value->obj == obj) {
                                    Struct *prev = (Struct *)label->value;
                                    structure->size = prev->size;
                                    structure->names = ref_namespace(prev->names);
                                    structure->names->backr = structure->names->forwr = 0;
                                } else {
                                    structure->size = 0;
                                    structure->names = new_namespace(current_file_list, &epoint);
                                }
                                const_assign(label, &structure->v);
                                structure = (Struct *)label->value;
                            }
                        } else {
                            structure->retval = false;
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->constant = true;
                            label->owner = true;
                            label->value = &structure->v;
                            label->epoint = epoint;
                            structure->size = 0;
                            structure->names = new_namespace(current_file_list, &epoint);
                        }
                        label->ref = false;
                        listing_line(listing, cmdpoint.pos);
                        current_section->structrecursion++;
                        waitfor->what = (prm == CMD_STRUCT) ? W_ENDS2 : W_ENDU2;
                        waitfor->skip = 1;
                        val = macro_recurse(W_ENDS, &structure->v, structure->names, &cmdpoint);
                        structure->retval = (val != NULL);
                        if (val != NULL) val_destroy(val);
                        close_waitfor((prm == CMD_STRUCT) ? W_ENDS2 : W_ENDU2);
                        current_section->structrecursion--;

                        current_section->provides = provides; current_section->requires = requires; current_section->conflicts = conflicts;
                        current_address = oldsection_address;
                        if (current_address->l_address.bank > all_mem) {
                            err_msg_big_address(&cmdpoint);
                            current_address->l_address.bank &= all_mem;
                        }

                        if (doubledef) val_destroy(&structure->v);
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
                        val_destroy(&section_address.mem->v);
                        goto breakerr;
                    }
                case CMD_SECTION:
                    if (section_start(&cmdpoint)) goto breakerr;
                    star = (current_address->l_address.address & 0xffff) | current_address->l_address.bank;
                    break;
                case CMD_VIRTUAL:
                    if (virtual_start(&cmdpoint)) goto breakerr;
                    star = (current_address->l_address.address & 0xffff) | current_address->l_address.bank;
                    break;
                case CMD_BREPT:
                case CMD_BFOR:
                    { /* .bfor */
                        List *lst;
                        bool labelexists;
                        size_t i;
                        Label *label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        if (labelexists) {
                            if (label->defpass == pass) {
                                err_msg_double_defined(label, &labelname, &epoint);
                                epoint = cmdpoint;
                                goto as_command;
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                                label->defpass = pass;
                            }
                        } else {
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->value = (Obj *)ref_none();
                        }
                        label->constant = true;
                        label->owner = true;
                        label->epoint = epoint;
                        label->ref = false;
                        if (label->value->obj == TUPLE_OBJ) {
                            List *old = (List *)label->value;
                            lst = new_tuple(old->len);
                            for (i = 0; i < old->len; i++) lst->data[i] = val_reference(old->data[i]);
                        } else {
                            lst = new_tuple(lenof(lst->u.val));
                            for (i = 0; i < lst->len; i++) lst->data[i] = (Obj *)ref_none();
                        }
                        label = ref_label(label);
                        i = (prm == CMD_BFOR) ? for_command(label, lst, &cmdpoint) : rept_command(label, lst, &cmdpoint);
                        if (lst->len > i) list_shrink(lst, i);
                        const_assign(label, &lst->v);
                        val_destroy(&label->v);
                        goto breakerr;
                    }
                case CMD_DSTRUCT: /* .dstruct */
                case CMD_DUNION:
                    {
                        address_t oldstart, oldend;
                        address2_t oldl_start, oldl_union;
                        bool oldunionmode;
                        bool labelexists, ret;
                        Type *obj;
                        Namespace *context;
                        Label *label = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                        if (labelexists) {
                            if (label->defpass == pass) {
                                err_msg_double_defined(label, &labelname, &epoint);
                                epoint = cmdpoint;
                                goto as_command;
                            } else {
                                if (!constcreated && temporary_label_branch == 0 && label->defpass != pass - 1) {
                                    if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                    constcreated = true;
                                }
                                if (label->file_list != current_file_list) {
                                    label_move(label, &labelname, current_file_list);
                                }
                            }
                        } else {
                            if (!constcreated && temporary_label_branch == 0) {
                                if (pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                constcreated = true;
                            }
                            label->value = (Obj *)ref_none();
                        }
                        label->constant = true;
                        label->owner = true;
                        label->epoint = epoint;
                        label->ref = false;

                        if (diagnostics.optimize) cpu_opt_invalidate();
                        listing_line(listing, cmdpoint.pos);
                        if (get_exp(1, 1, 0, &cmdpoint)) {
                            struct values_s *vs = get_val();
                            val = vs->val;
                            obj = (prm == CMD_DSTRUCT) ? STRUCT_OBJ : UNION_OBJ;
                            if (val->obj != obj) {
                                err_msg_wrong_type2(val, obj, &vs->epoint);
                                val = NULL;
                            }
                        } else val = NULL;
                        ret = (val != NULL) && ((Struct *)val)->retval;
                        if (here() == ',') lpoint.pos++;
                        current_section->structrecursion++;

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

                        if (!ret) {
                            Code *code = (Code *)label->value;
                            if (labelexists && code->v.obj == CODE_OBJ) {
                                Obj *tmp = get_star_value(current_address->l_address_val);
                                if (!tmp->obj->same(tmp, code->addr)) {
                                    val_destroy(code->addr); code->addr = tmp;
                                    if (label->usepass >= pass) {
                                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        fixeddig = false;
                                    }
                                } else val_destroy(tmp);
                                if (code->requires != current_section->requires || code->conflicts != current_section->conflicts || code->offs != 0) {
                                    code->requires = current_section->requires;
                                    code->conflicts = current_section->conflicts;
                                    code->offs = 0;
                                    if (label->usepass >= pass) {
                                        if (fixeddig && pass > max_pass) err_msg_cant_calculate(&label->name, &epoint);
                                        fixeddig = false;
                                    }
                                }
                                code->apass = pass;
                                label->defpass = pass;
                                code->names->backr = code->names->forwr = 0;
                            } else {
                                val_destroy(&code->v);
                                code = new_code();
                                label->value = (Obj *)code;
                                code->addr = get_star_value(current_address->l_address_val);
                                code->size = 0;
                                code->offs = 0;
                                code->dtype = D_NONE;
                                code->pass = 0;
                                code->apass = pass;
                                code->memblocks = ref_memblocks(current_address->mem);
                                code->names = new_namespace(current_file_list, &epoint);
                                code->requires = current_section->requires;
                                code->conflicts = current_section->conflicts;
                            }
                            context = code->names;
                        } else {
                            Label *label2;
                            bool labelexists2;
                            str_t tmpname;
                            if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                            else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                            anonident2.type = '#';
                            anonident2.star_tree = star_tree;
                            anonident2.vline = vline;
                            tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                            label2 = new_label(&tmpname, mycontext, strength, &labelexists2, current_file_list);
                            if (labelexists2) {
                                if (label2->defpass == pass) err_msg_double_defined(label2, &tmpname, &epoint);
                                label2->constant = true;
                                label2->owner = true;
                                label2->defpass = pass;
                                if (label2->value->obj != NAMESPACE_OBJ) {
                                    val_destroy(label2->value);
                                    label2->value = (Obj *)new_namespace(current_file_list, &epoint);
                                } else ((Namespace *)label2->value)->backr = ((Namespace *)label2->value)->forwr = 0;
                            } else {
                                label2->constant = true;
                                label2->owner = true;
                                label2->value = (Obj *)new_namespace(current_file_list, &epoint);
                                label2->epoint = epoint;
                            }
                            context = (Namespace *)label2->value;
                        }
                        label = ref_label(label);
                        if (val != NULL) {
                            val = macro_recurse((prm == CMD_DSTRUCT) ? W_ENDS3 : W_ENDU3, val, context, &cmdpoint);
                            if (val != NULL) {
                                if (ret) const_assign(label, val);
                                else val_destroy(val);
                            }
                        }
                        current_section->structrecursion--;

                        if (!ret) set_size(label, ((current_address->end < current_address->address) ? current_address->address : current_address->end) - oaddr, current_address->mem, oaddr, newmembp);
                        current_address->start = oldstart;
                        oldstart = current_address->unionmode ? current_address->end : current_address->address;
                        if (oldend > current_address->end) current_address->end = oldend;
                        current_address->l_start = oldl_start;
                        current_address->l_union = oldl_union;
                        current_address->unionmode = oldunionmode;
                        if (oldstart > current_address->address) {
                            poke_pos = &epoint;
                            memskip(oldstart - current_address->address);
                        }
                        val_destroy(&label->v);
                        goto breakerr;
                    }
                }
                break;
            }
            {
                bool labelexists = false;
                Code *code;
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
                            touch_label(tmp2);
                            labelname.len = 0;val = tmp2->value; goto as_macro;
                        }
                        if (parent == mycontext && tmp2->strength == strength) {
                            newlabel = tmp2;
                            labelexists = true;
                        }
                    }
                }
                if (!labelexists) newlabel = new_label(&labelname, mycontext, strength, &labelexists, current_file_list);
                oaddr = current_address->address;
                if (labelexists) {
                    if (newlabel->defpass == pass) {
                        err_msg_double_defined(newlabel, &labelname, &epoint);
                        newlabel = NULL;
                        if (wht == '.') {
                            epoint = cmdpoint;
                            goto as_command;
                        }
                        epoint = lpoint;
                        goto jn;
                    }
                    if (!newlabel->update_after && newlabel->value->obj != CODE_OBJ) {
                        val_destroy(newlabel->value);
                        labelexists = false;
                        newlabel->defpass = pass;
                    }
                }
                if (labelexists) {
                    if (!constcreated && temporary_label_branch == 0 && newlabel->defpass != pass - 1) {
                        if (pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                        constcreated = true;
                    }
                    newlabel->constant = true;
                    newlabel->owner = true;
                    if (newlabel->file_list != current_file_list) {
                        label_move(newlabel, &labelname, current_file_list);
                    }
                    newlabel->epoint = epoint;
                    if (!newlabel->update_after) {
                        Obj *tmp;
                        if (diagnostics.optimize && newlabel->ref) cpu_opt_invalidate();
                        tmp = get_star_value(current_address->l_address_val);
                        code = (Code *)newlabel->value;
                        if (!tmp->obj->same(tmp, code->addr)) {
                            val_destroy(code->addr); code->addr = tmp;
                            if (newlabel->usepass >= pass) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                                fixeddig = false;
                            }
                        } else val_destroy(tmp);
                        if (code->requires != current_section->requires || code->conflicts != current_section->conflicts || code->offs != 0) {
                            code->requires = current_section->requires;
                            code->conflicts = current_section->conflicts;
                            code->offs = 0;
                            if (newlabel->usepass >= pass) {
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                                fixeddig = false;
                            }
                        }
                        newmembp = get_mem(current_address->mem);
                        code->apass = pass;
                        newlabel->defpass = pass;
                        code->names->backr = code->names->forwr = 0;
                    }
                } else {
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    code = new_code();
                    if (!constcreated && temporary_label_branch == 0) {
                        if (pass > max_pass) err_msg_cant_calculate(&newlabel->name, &epoint);
                        constcreated = true;
                    }
                    newlabel->constant = true;
                    newlabel->owner = true;
                    newlabel->value = (Obj *)code;
                    newlabel->epoint = epoint;
                    code->addr = get_star_value(current_address->l_address_val);
                    code->size = 0;
                    code->offs = 0;
                    code->dtype = D_NONE;
                    code->pass = 0;
                    code->apass = pass;
                    code->memblocks = ref_memblocks(current_address->mem);
                    code->names = new_namespace(current_file_list, &epoint);
                    code->requires = current_section->requires;
                    code->conflicts = current_section->conflicts;
                    newmembp = get_mem(current_address->mem);
                }
            }
            if (wht == '.') { /* .proc */
                epoint = cmdpoint;
                switch (prm) {
                case CMD_PROC:
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_PEND, &epoint);
                    if (!newlabel->ref && ((Code *)newlabel->value)->pass != 0) {
                        waitfor->skip = 0; set_size(newlabel, 0, current_address->mem, oaddr, newmembp);
                        push_dummy_context();
                        waitfor->u.cmd_proc.label = NULL;
                    } else {         /* TODO: first time it should not compile */
                        push_context(((Code *)newlabel->value)->names);
                        newlabel->ref = false;
                        waitfor->u.cmd_proc.addr = current_address->address;waitfor->u.cmd_proc.membp = newmembp;waitfor->u.cmd_proc.label = ref_label(newlabel);
                    }
                    newlabel = NULL;
                    goto finish;
                case CMD_SECTION:
                    waitfor->u.cmd_section.addr = current_address->address;waitfor->u.cmd_section.membp = newmembp;waitfor->u.cmd_section.label = ref_label(newlabel);
                    listing_line(listing, epoint.pos);
                    newlabel->ref = false;
                    newlabel = NULL;
                    goto finish;
                case CMD_VIRTUAL:
                    waitfor->u.cmd_virtual.membp = newmembp;waitfor->u.cmd_virtual.label = ref_label(newlabel);
                    listing_line(listing, epoint.pos);
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
            /* fall through */
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
            /* fall through */
        case '=':
            if ((waitfor->skip & 1) != 0) {if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
            break;
        case ';':
        case '\0':
            if ((waitfor->skip & 1) != 0) {
                if (newlabel != NULL && newlabel->value->obj == CODE_OBJ && labelname.len != 0 && labelname.data[0] != '_' && labelname.data[0] != '+' && labelname.data[0] != '-' && mycontext == current_context) {val_destroy(&cheap_context->v);cheap_context = ref_namespace(((Code *)newlabel->value)->names);}
                listing_line(listing, epoint.pos);
            }
            break;
        case '.':
            prm = get_command();
            ignore();
        as_command:
            switch (prm) {
            case CMD_ENDC: /* .endc */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (!close_waitfor(W_ENDC)) {err_msg2(ERROR__MISSING_OPEN, ".comment", &epoint); goto breakerr;}
                if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                break;
            case CMD_ENDIF: /* .endif */
            case CMD_FI: /* .fi */
                {
                    if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                    if (!close_waitfor(W_FI2) && !close_waitfor(W_FI)) {err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr;}
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                }
                break;
            case CMD_ENDSWITCH: /* .endswitch */
                {
                    if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                    if (waitfor->what==W_SWITCH || waitfor->what==W_SWITCH2) {
                        if (waitfor->u.cmd_switch.val != NULL) val_destroy(waitfor->u.cmd_switch.val);
                        close_waitfor(waitfor->what);
                    } else {err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr;}
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                }
                break;
            case CMD_DEFAULT: /* .default */
                {
                    if ((waitfor->skip & 1) != 0) listing_line_cut(listing, epoint.pos);
                    if (waitfor->what==W_SWITCH) {err_msg2(ERROR______EXPECTED, "'.endswitch'", &epoint); goto breakerr;}
                    if (waitfor->what!=W_SWITCH2) {err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr;}
                    waitfor->skip = waitfor->skip >> 1;
                    waitfor->what = W_SWITCH;waitfor->epoint = epoint;
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                }
                break;
            case CMD_ELSE: /* .else */
                {
                    if ((waitfor->skip & 1) != 0) listing_line_cut(listing, epoint.pos);
                    if (waitfor->what==W_FI) { err_msg2(ERROR______EXPECTED, "'.fi'", &epoint); goto breakerr; }
                    if (waitfor->what!=W_FI2) { err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr; }
                    waitfor->skip = waitfor->skip >> 1;
                    waitfor->what = W_FI;waitfor->epoint = epoint;
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
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
                    if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                    new_waitfor(W_FI2, &epoint);
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
                            if (err == &none_value->v) err_msg_still_none(NULL, &vs->epoint);
                            else if (err->obj == ERROR_OBJ) err_msg_output((Error *)err);
                            val_destroy(err);
                            waitfor->skip = 0; break;
                        }
                        waitfor->skip = ((((Int *)err)->len == 0) != (prm == CMD_IFNE)) ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
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
                                if (err == &none_value->v) err_msg_still_none(NULL, &vs->epoint);
                                else if (err->obj == ERROR_OBJ) err_msg_output((Error *)err);
                                val_destroy(err);
                                waitfor->skip = 0; break;
                            }
                            waitfor->skip = ((((Int *)err)->len >= 0) != (prm == CMD_IFMI)) ? (skwait & 1) : (uint8_t)((skwait & 1) << 1);
                            val_destroy(err);
                        }
                        break;
                    }
                }
                break;
            case CMD_ELSIF: /* .elsif */
                {
                    uint8_t skwait = waitfor->skip;
                    bool truth;
                    struct values_s *vs;
                    if ((waitfor->skip & 1) != 0) listing_line_cut(listing, epoint.pos);
                    if (waitfor->what == W_FI) {err_msg2(ERROR______EXPECTED, "'.fi'", &epoint); goto breakerr; }
                    if (waitfor->what != W_FI2) {err_msg2(ERROR__MISSING_OPEN, ".if", &epoint); goto breakerr;}
                    waitfor->epoint = epoint;
                    if (skwait == 2) {
                        if (!get_exp(0, 1, 1, &epoint)) { waitfor->skip = 0; goto breakerr;}
                        vs = get_val();
                    } else { waitfor->skip = 0; break; }
                    if (tobool(vs, &truth)) { waitfor->skip = 0; break; }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                }
                break;
            case CMD_SWITCH: /* .switch */
                {
                    uint8_t skwait = waitfor->skip;
                    if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                    new_waitfor(W_SWITCH2, &epoint);
                    if (skwait == 1) {
                        struct values_s *vs;
                        if (!get_exp(0, 1, 1, &epoint)) {waitfor->skip = 0; waitfor->u.cmd_switch.val = NULL; goto breakerr;}
                        vs = get_val(); val = vs->val;
                        if (val->obj == ERROR_OBJ) { err_msg_output((Error *)val); val = (Obj *)none_value; }
                        else if (val == &none_value->v) err_msg_still_none(NULL, &vs->epoint);
                    } else val = (Obj *)none_value;
                    waitfor->u.cmd_switch.val = val_reference(val);
                    waitfor->skip = (val == &none_value->v) ? 0 : (uint8_t)((skwait & 1) << 1);
                }
                break;
            case CMD_CASE: /* .case */
                {
                    uint8_t skwait = waitfor->skip;
                    bool truth = false;
                    if ((waitfor->skip & 1) != 0) listing_line_cut(listing, epoint.pos);
                    if (waitfor->what == W_SWITCH) { err_msg2(ERROR______EXPECTED, "'.endswitch'", &epoint); goto breakerr; }
                    if (waitfor->what != W_SWITCH2) { err_msg2(ERROR__MISSING_OPEN, ".switch", &epoint); goto breakerr; }
                    waitfor->epoint = epoint;
                    if (skwait==2 || diagnostics.switch_case) {
                        struct values_s *vs;
                        Obj *result2;
                        struct oper_s tmp;
                        if (!get_exp(0, 1, 0, &epoint)) { waitfor->skip = 0; goto breakerr; }
                        tmp.op = &o_EQ;
                        tmp.epoint = tmp.epoint3 = &epoint;
                        while (!truth && (vs = get_val()) != NULL) {
                            val = vs->val;
                            if (val->obj == ERROR_OBJ) { err_msg_output((Error *)val); continue; }
                            if (val == &none_value->v) { err_msg_still_none(NULL, &vs->epoint);continue; }
                            tmp.v1 = waitfor->u.cmd_switch.val;
                            tmp.v2 = val;
                            tmp.epoint2 = &vs->epoint;
                            tmp.inplace = NULL;
                            result2 = tmp.v1->obj->calc2(&tmp);
                            truth = (Bool *)result2 == true_value;
                            val_destroy(result2);
                            if (truth && diagnostics.switch_case && skwait != 2) {
                                err_msg2(ERROR_DUPLICATECASE, NULL, &vs->epoint);
                                truth = false;
                                break;
                            }
                        }
                    }
                    waitfor->skip = truth ? (waitfor->skip >> 1) : (waitfor->skip & 2);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                }
                break;
            case CMD_ENDM: /* .endm */
                if (waitfor->what==W_ENDM) {
                    if (waitfor->u.cmd_macro.val != NULL) {
                        ((Macro *)waitfor->u.cmd_macro.val)->retval = (here() != 0 && here() != ';');
                        val_destroy(waitfor->u.cmd_macro.val);
                    }
                    close_waitfor(W_ENDM);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                } else if (waitfor->what==W_ENDM3) { /* not closed here */
                    nobreak = false;
                    if (here() != 0 && here() != ';' && get_exp(0, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                        break;
                    }
                } else err_msg2(ERROR__MISSING_OPEN, ".macro' or '.segment", &epoint);
                goto breakerr;
            case CMD_ENDF: /* .endf */
                if (close_waitfor(W_ENDF)) {
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                } else if (waitfor->what==W_ENDF3) { /* not closed here */
                    nobreak = false;
                    if (here() != 0 && here() != ';' && get_exp(0, 0, 0, NULL)) {
                        retval = get_vals_tuple();
                        break;
                    }
                } else err_msg2(ERROR__MISSING_OPEN, ".function", &epoint);
                goto breakerr;
            case CMD_NEXT: /* .next */
                waitfor->epoint = epoint;
                if (close_waitfor(W_NEXT)) {
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                } else if (waitfor->what == W_NEXT2) {
                    retval = (Obj *)true_value; /* anything non-null */
                    nobreak = false;
                } else if (close_waitfor(W_NEXT3)) {
                    pop_context();
                } else {err_msg2(ERROR__MISSING_OPEN, ".for' or '.rept", &epoint); goto breakerr;}
                break;
            case CMD_PEND: /* .pend */
                if (waitfor->what==W_PEND) {
                    if ((waitfor->skip & 1) != 0) {
                        listing_line(listing, epoint.pos);
                        if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".proc", &epoint);
                        if (waitfor->u.cmd_proc.label != NULL) {set_size(waitfor->u.cmd_proc.label, current_address->address - waitfor->u.cmd_proc.addr, current_address->mem, waitfor->u.cmd_proc.addr, waitfor->u.cmd_proc.membp);val_destroy(&waitfor->u.cmd_proc.label->v);}
                    } else pop_context();
                    close_waitfor(W_PEND);
                    if ((waitfor->skip & 1) != 0) listing_line_cut2(listing, epoint.pos);
                } else {err_msg2(ERROR__MISSING_OPEN, ".proc", &epoint); goto breakerr;}
                break;
            case CMD_ENDS: /* .ends */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
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
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_SEND)) {
                    get_label();
                } else if (waitfor->what==W_SEND2) {
                    str_t sectionname;
                    epoint = lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (sectionname.len != 0) {
                        str_t cf;
                        str_cfcpy(&cf, &sectionname);
                        if (str_cmp(&cf, &current_section->cfname) != 0) {
                            char *s = (char *)mallocx(current_section->name.len + 1);
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
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
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
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_ENDP)) {
                } else if (waitfor->what==W_ENDP2) {
                    if (diagnostics.page) {
                        if (((current_address->l_address.address ^ waitfor->u.cmd_page.laddr.address) & 0xff00) != 0 ||
                                current_address->l_address.bank != waitfor->u.cmd_page.laddr.bank) {
                            err_msg_page((waitfor->u.cmd_page.laddr.address & 0xffff) | waitfor->u.cmd_page.laddr.bank, (current_address->l_address.address & 0xffff) | current_address->l_address.bank, &epoint);
                        }
                    }
                    if (waitfor->u.cmd_page.label != NULL) {set_size(waitfor->u.cmd_page.label, current_address->address - waitfor->u.cmd_page.addr, current_address->mem, waitfor->u.cmd_page.addr, waitfor->u.cmd_page.membp);val_destroy(&waitfor->u.cmd_page.label->v);}
                    close_waitfor(W_ENDP2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".page", &epoint); goto breakerr;}
                break;
            case CMD_HERE: /* .here */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_HERE)) {
                } else if (waitfor->what==W_HERE2) {
                    logical_close(&epoint);
                    close_waitfor(W_HERE2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".logical", &epoint); goto breakerr;}
                break;
            case CMD_ENDV: /* .endv */
                if (diagnostics.optimize) cpu_opt_invalidate();
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_ENDV)) {
                } else if (waitfor->what==W_ENDV2) {
                    virtual_close(&epoint);
                    close_waitfor(W_ENDV2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".virtual", &epoint); goto breakerr;}
                break;
            case CMD_BEND: /* .bend */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_BEND)) {
                    pop_context();
                } else if (waitfor->what==W_BEND2) {
                    if (waitfor->u.cmd_block.label != NULL) {set_size(waitfor->u.cmd_block.label, current_address->address - waitfor->u.cmd_block.addr, current_address->mem, waitfor->u.cmd_block.addr, waitfor->u.cmd_block.membp);val_destroy(&waitfor->u.cmd_block.label->v);}
                    if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".block", &epoint);
                    close_waitfor(W_BEND2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".block", &epoint); goto breakerr;}
                break;
            case CMD_ENDN: /* .endn */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_ENDN)) {
                    pop_context();
                } else if (waitfor->what==W_ENDN2) {
                    if (pop_context()) err_msg2(ERROR__MISSING_OPEN, ".namespace", &epoint);
                    close_waitfor(W_ENDN2);
                } else {err_msg2(ERROR__MISSING_OPEN, ".namespace", &epoint); goto breakerr;}
                break;
            case CMD_ENDWEAK: /* .endweak */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                if (close_waitfor(W_WEAK)) {
                } else if (waitfor->what==W_WEAK2) {
                    if (waitfor->u.cmd_weak.label != NULL) {set_size(waitfor->u.cmd_weak.label, current_address->address - waitfor->u.cmd_weak.addr, current_address->mem, waitfor->u.cmd_weak.addr, waitfor->u.cmd_weak.membp);val_destroy(&waitfor->u.cmd_weak.label->v);}
                    close_waitfor(W_WEAK2);
                    strength--;
                } else {err_msg2(ERROR__MISSING_OPEN, ".weak", &epoint); goto breakerr;}
                break;
            case CMD_END: /* .end */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
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
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    mark_mem(current_address->mem, current_address->address, star);
                    poke_pos = &epoint;

                    if (prm<CMD_BYTE) {    /* .text .ptext .shift .shiftl .null */
                        size_t ln;
                        struct values_s *vs;
                        struct textrecursion_s trec;
                        if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                            ((Code *)newlabel->value)->dtype = D_BYTE;
                        }
                        if (here() == 0 || here() == ';') { err_msg_argnum(0, 1, 0, &epoint); goto breakerr; }
                        if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                        if (prm == CMD_PTEXT) {
                            trec.buff[0] = outputeor;
                            trec.len = 1;
                        } else trec.len = 0;
                        trec.sum = trec.len;
                        trec.max = SIZE_MAX;
                        trec.prm = prm;
                        trec.warn = false;
                        for (ln = get_val_remaining(), vs = get_val(); ln != 0; ln--, vs++) {
                            if (trec.len != 0) {
                                if (trec.len > 0) memcpy(pokealloc(trec.len), trec.buff, trec.len);
                                else if (trec.len < 0) memskip(-trec.len);
                                trec.len = 0;
                            }
                            poke_pos = &vs->epoint;
                            textrecursion(&trec, vs->val);
                            if (trec.warn) { err_msg_still_none(NULL, poke_pos); trec.warn = false; }
                        }
                        if (trec.len < 0) { memskip(-trec.len); trec.len = 0; }
                        switch (prm) {
                        case CMD_SHIFTL:
                        case CMD_SHIFT:
                            if (trec.len > 0) trec.buff[trec.len - 1] ^= (prm == CMD_SHIFT) ? 0x80 : 0x01;
                            else if (trec.sum != 0) err_msg2(ERROR___NO_LAST_GAP, NULL, poke_pos);
                            else err_msg2(ERROR__BYTES_NEEDED, NULL, &epoint);
                            break;
                        case CMD_NULL:
                            if (trec.len >= (ssize_t)sizeof trec.buff) {
                                memcpy(pokealloc(trec.len), trec.buff, trec.len);
                                trec.len = 0;
                            }
                            trec.buff[trec.len++] = outputeor; break;
                        default: break;
                        }
                        if (trec.len > 0) memcpy(pokealloc(trec.len), trec.buff, trec.len);
                        if (prm == CMD_PTEXT) {
                            if (trec.sum > 0x100) err_msg2(ERROR____PTEXT_LONG, &trec.sum, &epoint);
                            write_mark_mem(current_address->mem, (trec.sum-1) ^ outputeor);
                        }
                    } else if (prm<=CMD_DWORD) { /* .byte .word .int .rta .long */
                        int bits;
                        size_t ln;
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
                            ((Code *)newlabel->value)->dtype = dtype;
                        }
                        if (here() == 0 || here() == ';') { err_msg_argnum(0, 1, 0, &epoint); goto breakerr; }
                        switch (prm) {
                        case CMD_CHAR: bits = -8; break;
                        case CMD_SINT: bits = -16; break;
                        case CMD_LINT: bits = -24; break;
                        case CMD_DINT: bits = -32; break;
                        case CMD_BYTE: bits = 8; break;
                        default: bits = 16; break;
                        case CMD_LONG: bits = 24; break;
                        case CMD_DWORD: bits = 32; break;
                        }
                        if (!get_exp(0, 0, 0, NULL)) goto breakerr;
                        brec.len = 0;
                        brec.warn = false;
                        for (ln = get_val_remaining(), vs = get_val(); ln != 0; ln--, vs++) {
                            poke_pos = &vs->epoint;
                            byterecursion(vs->val, prm, &brec, bits);
                            if (brec.warn) { err_msg_still_none(NULL, poke_pos); brec.warn = false; }
                            if (brec.len == 0) continue;
                            if (brec.len > 0) memcpy(pokealloc(brec.len), brec.buff, brec.len);
                            else memskip(-brec.len);
                            brec.len = 0;
                        }
                    } else if (prm==CMD_BINARY) { /* .binary */
                        char *path = NULL;
                        ival_t foffs = 0;
                        size_t fsize = SIZE_MAX;
                        struct values_s *vs;
                        str_t filename;

                        if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                            ((Code *)newlabel->value)->dtype = D_BYTE;
                        }
                        if (!get_exp(0, 1, 3, &epoint)) goto breakerr;
                        vs = get_val();
                        if (!tostr(vs, &filename)) {
                            path = get_path(&filename, current_file_list->file->realname);
                        }
                        if ((vs = get_val()) != NULL) {
                            ival_t ival;
                            if (toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) {}
                            else foffs = ival;
                            if ((vs = get_val()) != NULL) {
                                uval_t uval;
                                if (touval2(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) {}
                                else fsize = uval;
                            }
                        }

                        if (path != NULL) {
                            struct file_s *cfile2 = openfile(path, current_file_list->file->realname, 1, &filename, &epoint);
                            if (cfile2 != NULL) {
                                size_t foffset;
                                if (foffs < 0) foffset = (uval_t)-foffs < cfile2->len ? (cfile2->len - (uval_t)-foffs) : 0;
                                else foffset = (uval_t)foffs;
                                for (; fsize != 0 && foffset < cfile2->len;) {
                                    size_t i, ln = cfile2->len - foffset;
                                    uint8_t *d, *s = cfile2->data + foffset;
                                    if (ln > fsize) ln = fsize;
                                    d = pokealloc((address_t)ln);
                                    if (outputeor != 0) {
                                        for (i = 0; i < ln; i++) d[i] = s[i] ^ outputeor;
                                    } else memcpy(d, s, ln);
                                    foffset += ln;
                                    fsize -= ln;
                                }
                            }
                            free(path);
                        }
                    }

                    if (nolisting == 0) {
                        list_mem(current_address->mem);
                    }
                }
                break;
            case CMD_OFFS: if ((waitfor->skip & 1) != 0)
                {   /* .offs */
                    struct values_s *vs;
                    ival_t ival;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, epoint.pos);
                    if (!current_address->moved) {
                        if (current_address->end < current_address->address) current_address->end = current_address->address;
                        current_address->moved = true;
                    }
                    current_address->wrapwarn = false;
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (toival(vs->val, &ival, 8 * sizeof ival, &vs->epoint)) break;
                    if (current_address->address != ((address_t)(star + ival) & all_mem2)) {
                        current_address->address = (address_t)(star + ival) & all_mem2;
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
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_HERE2, &epoint);
                    waitfor->u.cmd_logical.laddr = current_address->unionmode ? current_address->l_union : current_address->l_address;waitfor->u.cmd_logical.addr = current_address->address;waitfor->u.cmd_logical.val = val_reference(current_address->l_address_val);
                    if (newlabel == NULL) waitfor->u.cmd_logical.label = NULL;
                    else {
                        waitfor->u.cmd_logical.membp = newmembp;waitfor->u.cmd_logical.label = ref_label(newlabel);
                        newlabel = NULL;
                    }
                    current_section->logicalrecursion++;
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    tmp = vs->val;
                    if (touval(tmp->obj->address(tmp, &am), &uval, all_mem_bits, &vs->epoint)) break;
                    if (am != A_NONE && check_addr(am)) {
                        err_msg_output_and_destroy(err_addressing(am, &vs->epoint));
                        break;
                    }
                    if (current_address->unionmode) {
                        current_address->l_union.address = uval & 0xffff;
                        current_address->l_union.bank = uval & all_mem & ~(address_t)0xffff;
                    } else {
                        current_address->l_address.address = uval & 0xffff;
                        current_address->l_address.bank = uval & all_mem & ~(address_t)0xffff;
                    }
                    val_destroy(current_address->l_address_val);
                    tmp = vs->val;
                    current_address->l_address_val = val_reference(tmp->obj == CODE_OBJ ? ((Code *)tmp)->addr : tmp);
                } else new_waitfor(W_HERE, &epoint);
                break;
            case CMD_VIRTUAL: if ((waitfor->skip & 1) != 0)
                { /* .virtual */
                    listing_line(listing, 0);
                    if (virtual_start(&epoint)) goto breakerr;
                } else new_waitfor(W_ENDV, &epoint);
                break;
            case CMD_AS: /* .as */
            case CMD_AL: /* .al */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
                    longaccu = (prm == CMD_AL);
                }
                break;
            case CMD_XS: /* .xs */
            case CMD_XL: /* .xl */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
                    longindex = (prm == CMD_XL);
                }
                break;
            case CMD_AUTSIZ: /* .autsiz */
            case CMD_MANSIZ: /* .mansiz */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
                    autosize = (prm == CMD_AUTSIZ);
                }
                break;
            case CMD_BLOCK: if ((waitfor->skip & 1) != 0)
                { /* .block */
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_BEND2, &epoint);
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        push_context(((Code *)newlabel->value)->names);
                        waitfor->u.cmd_block.addr = current_address->address;waitfor->u.cmd_block.membp = newmembp;waitfor->u.cmd_block.label = ref_label(newlabel);
                        newlabel = NULL;
                    } else {
                        Label *label;
                        bool labelexists;
                        str_t tmpname;
                        waitfor->u.cmd_block.label = NULL;
                        if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                        else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                        anonident2.type = '.';
                        anonident2.star_tree = star_tree;
                        anonident2.vline = vline;
                        tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                        label = new_label(&tmpname, mycontext, strength, &labelexists, current_file_list);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                            label->constant = true;
                            label->owner = true;
                            label->defpass = pass;
                            if (label->value->obj != NAMESPACE_OBJ) {
                                val_destroy(label->value);
                                label->value = (Obj *)new_namespace(current_file_list, &epoint);
                            } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                        } else {
                            label->constant = true;
                            label->owner = true;
                            label->value = (Obj *)new_namespace(current_file_list, &epoint);
                            label->epoint = epoint;
                        }
                        push_context((Namespace *)label->value);
                    }
                } else {push_dummy_context(); new_waitfor(W_BEND, &epoint);}
                break;
            case CMD_NAMESPACE: if ((waitfor->skip & 1) != 0)
                { /* .namespace */
                    struct values_s *vs;
                    Label *label;
                    bool labelexists;
                    str_t tmpname;
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_ENDN, &epoint);
                    if (get_exp(0, 0, 1, &epoint)) {
                        vs = get_val();
                        if (vs != NULL) {
                            val = vs->val;
                            val = (Obj *)get_namespace(val);
                            if (val == NULL) err_msg_wrong_type2(vs->val, NULL, &vs->epoint);
                        } else val = NULL;
                    } else val = NULL;
                    if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                    else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                    anonident2.type = '.';
                    anonident2.star_tree = star_tree;
                    anonident2.vline = vline;
                    tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                    label = new_label(&tmpname, mycontext, strength, &labelexists, current_file_list);
                    if (labelexists) {
                        if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                        label->constant = true;
                        label->owner = (val == NULL);
                        if (val != NULL) const_assign(label, val_reference(val));
                        else {
                            label->defpass = pass;
                            if (label->value->obj != NAMESPACE_OBJ) {
                                val_destroy(label->value);
                                label->value = (Obj *)new_namespace(current_file_list, &epoint);
                            } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                        }
                    } else {
                        label->constant = true;
                        label->owner = (val == NULL);
                        label->value = (val != NULL) ? val_reference(val) : (Obj *)new_namespace(current_file_list, &epoint);
                        label->epoint = epoint;
                    }
                    if (label->value->obj == NAMESPACE_OBJ) {
                        push_context((Namespace *)label->value);
                        waitfor->what = W_ENDN2;
                    } else push_context(current_context);
                } else {push_dummy_context(); new_waitfor(W_ENDN, &epoint);}
                break;
            case CMD_WEAK: if ((waitfor->skip & 1) != 0)
                { /* .weak */
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_WEAK2, &epoint);
                    if (newlabel == NULL) waitfor->u.cmd_weak.label = NULL;
                    else {
                        waitfor->u.cmd_weak.addr = current_address->address;waitfor->u.cmd_weak.membp = newmembp;waitfor->u.cmd_weak.label = ref_label(newlabel);
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
                { /* .databank, .dpage, .eor */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    switch (prm) {
                    case CMD_DATABANK:
                        if (vs->val == &gap_value->v) databank = 256;
                        else if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else databank = uval & 0xff;
                        break;
                    case CMD_DPAGE:
                        if (vs->val == &gap_value->v) dpage = 65536;
                        else if (touval(vs->val, &uval, 16, &vs->epoint)) {}
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
            case CMD_FILL:
            case CMD_ALIGN: if ((waitfor->skip & 1) != 0)
                { /* .fill, .align */
                    address_t db = 0;
                    uval_t uval;
                    struct values_s *vs;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                        ((Code *)newlabel->value)->dtype = D_BYTE;
                    }
                    if (!get_exp(0, 1, 2, &epoint)) goto breakerr;
                    vs = get_val();
                    if (prm == CMD_ALIGN) {
                        address_t max = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? all_mem2 : all_mem;
                        if (touval2(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) {}
                        else if (uval == 0) err_msg2(ERROR_NO_ZERO_VALUE, NULL, &vs->epoint);
                        else if (uval > 1) {
                            address_t itt = (all_mem2 == 0xffffffff && current_section->logicalrecursion == 0) ? current_address->address : ((current_address->l_address.address + current_address->l_address.bank - current_address->l_start.address - current_address->l_start.bank) & all_mem);
                            if (uval > max) {
                                if (itt != 0) db = max - itt + 1;
                            } else {
                                address_t rem = itt % (address_t)uval;
                                if (rem != 0) db = (address_t)uval - rem;
                            }
                        }
                    } else {
                        if (touval2(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) {}
                        else db = uval;
                    }
                    mark_mem(current_address->mem, current_address->address, star);
                    if ((vs = get_val()) != NULL) {
                        struct textrecursion_s trec;
                        size_t membp = get_mem(current_address->mem);

                        poke_pos = &vs->epoint;
                        trec.len = 0;
                        trec.sum = 0;
                        trec.max = db;
                        trec.prm = CMD_TEXT;
                        trec.warn = false;
                        textrecursion(&trec, vs->val);
                        if (trec.warn) {
                            err_msg_still_none(NULL, poke_pos);
                        }

                        db -= trec.sum;
                        if (db != 0) {
                            if (trec.sum == 1 && trec.len == 1) {
                                memset(pokealloc(db + 1), trec.buff[0], db + 1); /* single byte shortcut */
                                trec.len = 0;
                            } else if (trec.sum == (size_t)-trec.len) {
                                if (trec.sum == 0) err_msg2(ERROR__BYTES_NEEDED, NULL, poke_pos);
                                trec.len -= db; /* gap shortcut */
                            } else {
                                size_t offs = 0;
                                if (trec.len > 0) {
                                    memcpy(pokealloc(trec.len), trec.buff, trec.len);
                                    trec.len = 0;
                                }
                                while (db != 0) { /* pattern repeat */
                                    int ch;
                                    db--;
                                    ch = read_mem(current_address->mem, oaddr, membp, offs);
                                    if (ch < 0) {
                                        if (trec.len > 0) {
                                            memcpy(pokealloc(trec.len), trec.buff, trec.len);
                                            trec.len = 0;
                                        }
                                        trec.len--;
                                    } else {
                                        if (trec.len < 0) {memskip(-trec.len); trec.len = 0;}
                                        else if (trec.len >= (ssize_t)sizeof trec.buff) {
                                            memcpy(pokealloc(trec.len), trec.buff, trec.len);
                                            trec.len = 0;
                                        }
                                        trec.buff[trec.len++] = ch;
                                    }
                                    offs++;
                                    if (offs >= trec.sum) offs = 0;
                                }
                            }
                        }
                        if (trec.len > 0) memcpy(pokealloc(trec.len), trec.buff, trec.len);
                        else if (trec.len < 0) memskip(-trec.len);
                    } else if (db != 0) {
                        poke_pos = &epoint;
                        memskip(db);
                    }
                    if (nolisting == 0) {
                        list_mem(current_address->mem);
                    }
                }
                break;
            case CMD_ASSERT: if ((waitfor->skip & 1) != 0)
                { /* .assert */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 3, 3, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) current_section->provides = ~(uval_t)0;
                    else current_section->provides = uval;
                    vs++;
                    if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) current_section->requires = 0;
                    else current_section->requires = uval;
                    vs++;
                    if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) current_section->conflicts = 0;
                    else current_section->conflicts = uval;
                }
                break;
            case CMD_CHECK: if ((waitfor->skip & 1) != 0)
                { /* .check */
                    uval_t uval;
                    struct values_s *vs;
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 2, 2, &epoint)) goto breakerr;
                    vs = get_val();
                    if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) {}
                    else if ((uval & current_section->provides) != uval) err_msg2(ERROR_REQUIREMENTS_, NULL, &epoint);
                    vs++;
                    if (touval(vs->val, &uval, 8 * sizeof uval, &vs->epoint)) {}
                    else if ((uval & current_section->provides) != 0) err_msg2(ERROR______CONFLICT, NULL, &epoint);
                }
                break;
            case CMD_WARN:
            case CMD_CWARN:
            case CMD_ERROR:
            case CMD_CERROR: if ((waitfor->skip & 1) != 0)
                { /* .warn .cwarn .error .cerror */
                    size_t i, len, len2, chars;
                    Str *v;
                    Obj **vals;
                    uint8_t *s;
                    Tuple *tmp;
                    struct values_s *vs;
                    listing_line(listing, epoint.pos);
                    if (prm == CMD_CWARN || prm == CMD_CERROR) {
                        bool writeit;
                        if (!get_exp(1, 1, 0, &epoint)) goto breakerr;
                        if (tobool(get_val(), &writeit) || !writeit) goto breakerr;
                        if (here() == ',') lpoint.pos++;
                    }
                    if (!get_exp(0, 0, 0, &epoint)) goto breakerr;
                    len = get_val_remaining();
                    tmp = new_tuple(len);
                    vals = tmp->data;
                    vs = get_val();
                    len2 = 0; chars = 0;
                    for (i = 0; i < len; i++, vs++) {
                        val = vs->val;
                        if (val == &none_value->v) val = (Obj *)ref_str(null_str);
                        else {
                            val = STR_OBJ->create(val, &vs->epoint);
                            if (val->obj != STR_OBJ) {
                                if (val == &none_value->v) err_msg_still_none(NULL, &vs->epoint);
                                else if (val->obj == ERROR_OBJ) err_msg_output((Error *)val);
                                val_destroy(val);
                                val = (Obj *)ref_str(null_str);
                            } else {
                                Str *str = (Str *)val;
                                len2 += str->len;
                                if (len2 < str->len) err_msg_out_of_memory(); /* overflow */
                                chars += str->chars;
                            }
                        }
                        vals[i] = val;
                    }
                    v = new_str(len2);
                    v->chars = chars;
                    s = v->data;
                    for (i = 0; i < len; i++) {
                        Str *str = (Str *)vals[i];
                        if (str->len != 0) {
                            memcpy(s, str->data, str->len);
                            s += str->len;
                        }
                    }
                    err_msg2((prm==CMD_CERROR || prm==CMD_ERROR)?ERROR__USER_DEFINED:ERROR_WUSER_DEFINED, v, &epoint);
                    val_destroy(&v->v);
                    val_destroy(&tmp->v);
                }
                break;
            case CMD_ENC: if ((waitfor->skip & 1) != 0)
                { /* .enc */
                    str_t encname;
                    listing_line(listing, epoint.pos);
                    encname.len = 0;
                    if (pline[lpoint.pos] != '"' && pline[lpoint.pos] != '\'') { /* will be removed to allow variables */
                        if (diagnostics.deprecated) err_msg2(ERROR_______OLD_ENC, NULL, &lpoint);
                        encname.data = pline + lpoint.pos; encname.len = get_label();
                    }
                    if (encname.len == 0) {
                        struct values_s *vs;
                        if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                        vs = get_val();
                        if (tostr(vs, &encname)) break;
                        if (encname.len == 0) {err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint); break;}
                    }
                    actual_encoding = new_encoding(&encname, &epoint);
                }
                break;
            case CMD_CDEF: if ((waitfor->skip & 1) != 0)
                { /* .cdef */
                    struct trans_s tmp, *t;
                    struct encoding_s *old = actual_encoding;
                    bool rc;
                    size_t len;
                    listing_line(listing, epoint.pos);
                    actual_encoding = NULL;
                    rc = get_exp(0, 2, 0, &epoint);
                    actual_encoding = old;
                    len = get_val_remaining();
                    if (!rc) goto breakerr;
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
                            Str *str = (Str *)val;
                            if (str->len == 0) {err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint); tryit = false;}
                            else {
                                uchar_t ch = str->data[0];
                                if ((ch & 0x80) != 0) i = utf8in(str->data, &ch); else i = 1;
                                tmp.start = ch;
                                if (str->len > i) {
                                    ch = str->data[i];
                                    if ((ch & 0x80) != 0) i += utf8in(str->data + i, &ch); else i++;
                                    tmp.end = ch;
                                    endok = true;
                                    if (str->len > i) {err_msg2(ERROR_NOT_TWO_CHARS, NULL, &vs->epoint); tryit = false;}
                                }
                            }
                        } else {
                            if (touval2(val, &uval, 24, &vs->epoint)) tryit = false;
                            tmp.start = uval;
                        }
                        if (!endok) {
                            vs = get_val();
                            if (vs == NULL) { err_msg_argnum(len, len + 2, 0, &epoint); goto breakerr; }

                            val = vs->val;
                            if (val->obj == STR_OBJ) {
                                Str *str = (Str *)val;
                                if (str->len == 0) {err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint); tryit = false;}
                                else {
                                    uchar_t ch = str->data[0];
                                    if ((ch & 0x80) != 0) i = utf8in(str->data, &ch); else i = 1;
                                    tmp.end = ch;
                                    if (str->len > i) {err_msg2(ERROR__NOT_ONE_CHAR, NULL, &vs->epoint); tryit = false;}
                                }
                            } else {
                                if (touval2(val, &uval, 24, &vs->epoint)) tryit = false;
                                tmp.end = uval;
                            }
                        }
                        vs = get_val();
                        if (vs == NULL) { err_msg_argnum(len, len + 1, 0, &epoint); goto breakerr;}
                        if (touval(vs->val, &uval, 8, &vs->epoint)) {}
                        else if (tryit) {
                            tmp.offset = uval & 0xff;
                            if (tmp.start > tmp.end) {
                                uchar_t tmpe = tmp.start;
                                tmp.start = tmp.end;
                                tmp.end = tmpe;
                            }
                            t = new_trans(&tmp, actual_encoding, &epoint);
                            if (t->start != tmp.start || t->end != tmp.end || t->offset != tmp.offset) {
                                err_msg2(ERROR__DOUBLE_RANGE, NULL, opoint); goto breakerr;
                            }
                        }
                    }
                }
                break;
            case CMD_EDEF: if ((waitfor->skip & 1) != 0)
                { /* .edef */
                    struct encoding_s *old = actual_encoding;
                    bool rc;
                    size_t len;
                    listing_line(listing, epoint.pos);
                    actual_encoding = NULL;
                    rc = get_exp(0, 2, 0, &epoint);
                    actual_encoding = old;
                    if (!rc) goto breakerr;
                    len = get_val_remaining();
                    for (;;) {
                        struct values_s *vs, *vs2;
                        bool tryit;
                        str_t escape;

                        vs = get_val();
                        if (vs == NULL) break;
                        tryit = !tostr(vs, &escape);

                        if (tryit && escape.len == 0) {
                            err_msg2(ERROR__EMPTY_STRING, NULL, &vs->epoint);
                            tryit = false;
                        }
                        vs2 = get_val();
                        if (vs2 == NULL) { err_msg_argnum(len, len + 1, 0, &epoint); goto breakerr; }
                        val = vs2->val;
                        if (val == &none_value->v) err_msg_still_none(NULL, &vs2->epoint);
                        else if (tryit && new_escape(&escape, val, actual_encoding, &vs2->epoint)) {
                            err_msg2(ERROR_DOUBLE_ESCAPE, NULL, &vs->epoint); goto breakerr;
                        }
                    }
                }
                break;
            case CMD_CPU: if ((waitfor->skip & 1) != 0)
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
                        {"w65c02", &w65c02}, {"4510", &c4510},
                        {"default", NULL}, {NULL, NULL},
                    };
                    str_t cpuname;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (tostr(vs, &cpuname)) break;
                    for (cpui = cpus; cpui->name != NULL; cpui++) {
                        if (cpuname.len == strlen(cpui->name) && memcmp(cpui->name, cpuname.data, cpuname.len) == 0) {
                            const struct cpu_s *cpumode = (cpui->def != NULL) ? cpui->def : arguments.cpumode;
                            if (current_address->l_address.bank > cpumode->max_address) {
                                err_msg_big_address(&epoint);
                                current_address->l_address.bank &= cpumode->max_address;
                            }
                            set_cpumode(cpumode);
                            break;
                        }
                    }
                    if (cpui->name == NULL) err_msg2(ERROR___UNKNOWN_CPU, &cpuname, &vs->epoint);
                }
                break;
            case CMD_PRON: /* .pron */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
                    if (nolisting != 0) nolisting--;
                }
                break;
            case CMD_PROFF: /* .proff */
                if ((waitfor->skip & 1) != 0) {
                    nolisting++;
                    listing_line(listing, epoint.pos);
                }
                break;
            case CMD_SHOWMAC: /* .showmac */
            case CMD_HIDEMAC: /* .hidemac */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
                    if (diagnostics.ignored) err_msg2(ERROR_DIRECTIVE_IGN, NULL, &epoint);
                }
                break;
            case CMD_COMMENT: /* .comment */
                if ((waitfor->skip & 1) != 0) listing_line(listing, epoint.pos);
                new_waitfor(W_ENDC, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_INCLUDE:
            case CMD_BINCLUDE: if ((waitfor->skip & 1) != 0)
                { /* .include, .binclude */
                    struct file_s *f = NULL;
                    struct values_s *vs;
                    char *path;
                    str_t filename;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    vs = get_val();
                    if (!tostr(vs, &filename)) {
                        path = get_path(&filename, current_file_list->file->realname);
                        f = openfile(path, current_file_list->file->realname, 2, &filename, &epoint);
                        free(path);
                    }
                    if (here() != 0 && here() != ';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);

                    if (f == NULL) goto breakerr;
                    if (f->open>1) {
                        err_msg2(ERROR_FILERECURSION, NULL, &epoint);
                    } else {
                        Wait_types what;
                        bool starexists;
                        struct star_s *s = new_star(vline, &starexists);
                        struct avltree *stree_old = star_tree;
                        line_t lin = lpoint.line;
                        line_t vlin = vline;

                        if (starexists && s->addr != star) {
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        s->addr = star;
                        listing_file(listing, ";******  Processing file: ", f);
                        if (prm == CMD_BINCLUDE) {
                            if (newlabel != NULL && newlabel->value->obj == CODE_OBJ) {
                                push_context(((Code *)newlabel->value)->names);
                            } else {
                                Label *label;
                                bool labelexists;
                                str_t tmpname;
                                if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                                else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                                anonident2.type = '.';
                                anonident2.star_tree = star_tree;
                                anonident2.vline = vline;
                                tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                                label = new_label(&tmpname, mycontext, strength, &labelexists, current_file_list);
                                if (labelexists) {
                                    if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                                    label->constant = true;
                                    label->owner = true;
                                    label->defpass = pass;
                                    if (label->value->obj != NAMESPACE_OBJ) {
                                        val_destroy(label->value);
                                        label->value = (Obj *)new_namespace(current_file_list, &epoint);
                                    } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                                } else {
                                    label->constant = true;
                                    label->owner = true;
                                    label->value = (Obj *)new_namespace(current_file_list, &epoint);
                                    label->epoint = epoint;
                                }
                                push_context((Namespace *)label->value);
                            }
                        }
                        enterfile(f, &epoint);
                        lpoint.line = vline = 0;
                        star_tree = &s->tree;
                        what = waitfor->what; waitfor->what = W_NONE;
                        val = compile();
                        waitfor->what = what;
                        if (prm == CMD_BINCLUDE) pop_context();
                        if (val != NULL) val_destroy(val);
                        lpoint.line = lin; vline = vlin;
                        star_tree = stree_old;
                        exitfile();
                        listing_file(listing, ";******  Return to file: ", current_file_list->file);
                    }
                    closefile(f);
                    goto breakerr;
                }
                break;
            case CMD_BREPT:
            case CMD_BFOR: if ((waitfor->skip & 1) != 0)
                { /* .bfor */
                    List *lst;
                    size_t i;
                    Label *label;
                    bool labelexists;
                    str_t tmpname;
                    if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                    else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                    anonident2.type = '.';
                    anonident2.star_tree = star_tree;
                    anonident2.vline = vline;
                    tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                    label = new_label(&tmpname, mycontext, strength, &labelexists, current_file_list);
                    if (labelexists) {
                        if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                        label->defpass = pass;
                    } else {
                        label->value = (Obj *)ref_none();
                        label->epoint = epoint;
                    }
                    label->constant = true;
                    label->owner = true;
                    if (label->value->obj == TUPLE_OBJ) {
                        List *old = (List *)label->value;
                        lst = new_tuple(old->len);
                        for (i = 0; i < old->len; i++) lst->data[i] = val_reference(old->data[i]);
                    } else {
                        lst = new_tuple(lenof(lst->u.val));
                        for (i = 0; i < lst->len; i++) lst->data[i] = (Obj *)ref_none();
                    }
                    i = (prm == CMD_BFOR) ? for_command(NULL, lst, &epoint) : rept_command(NULL, lst, &epoint);
                    if (lst->len > i) list_shrink(lst, i);
                    const_assign(label, &lst->v);
                    goto breakerr;
                } else {push_dummy_context(); new_waitfor(W_NEXT3, &epoint);}
                break;
            case CMD_FOR: if ((waitfor->skip & 1) != 0)
                { /* .for */
                    for_command(NULL, NULL, &epoint);
                    goto breakerr;
                } else new_waitfor(W_NEXT, &epoint);
                break;
            case CMD_REPT: if ((waitfor->skip & 1) != 0)
                { /* .for */
                    rept_command(NULL, NULL, &epoint);
                    goto breakerr;
                } else new_waitfor(W_NEXT, &epoint);
                break;
            case CMD_CONTINUE:
            case CMD_BREAK: if ((waitfor->skip & 1) != 0)
                { /* .continue, .break */
                    size_t wp = waitfor_p + 1;
                    bool nok = true;
                    listing_line(listing, epoint.pos);
                    while ((wp--) != 0) {
                        if (waitfors[wp].what == W_NEXT2) {
                            if (wp != 0 && prm == CMD_BREAK) waitfors[wp].u.cmd_rept.breakout = true;
                            for (;wp <= waitfor_p; wp++) waitfors[wp].skip = 0;
                            nok = false;
                            break;
                        }
                    }
                    if (nok) err_msg2(ERROR__MISSING_LOOP, NULL, &epoint);
                }
                break;
            case CMD_PAGE: if ((waitfor->skip & 1) != 0)
                { /* .page */
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, epoint.pos);
                    new_waitfor(W_ENDP2, &epoint);
                    waitfor->u.cmd_page.laddr = current_address->l_address;
                    if (newlabel == NULL) waitfor->u.cmd_page.label = NULL;
                    else {
                        waitfor->u.cmd_page.addr = current_address->address;waitfor->u.cmd_page.membp = newmembp;waitfor->u.cmd_page.label = ref_label(newlabel);
                        newlabel = NULL;
                    }
                } else new_waitfor(W_ENDP, &epoint);
                break;
            case CMD_OPTION: if ((waitfor->skip & 1) != 0)
                { /* .option */
                    static const str_t branch_across = {24, (const uint8_t *)"allow_branch_across_page"};
                    static const str_t longjmp = {22, (const uint8_t *)"auto_longbranch_as_jmp"};
                    struct values_s *vs;
                    str_t optname, cf;
                    listing_line(listing, epoint.pos);
                    optname.data = pline + lpoint.pos; optname.len = get_label();
                    if (optname.len == 0) { err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
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
                    listing_line(listing, epoint.pos);
                    if (!get_exp(0, 1, 1, &epoint)) goto breakerr;
                    if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &epoint);
                    vs = get_val(); val = vs->val;
                    if (val->obj != LBL_OBJ) {err_msg_wrong_type2(val, LBL_OBJ, &vs->epoint); break;}
                    lbl = (Lbl *)val;
                    if (lbl->file_list == current_file_list && lbl->parent == current_context && oldwaitforp <= lbl->waitforp) {
                        while (lbl->waitforp < waitfor_p) {
                            const char *msg;
                            switch (waitfor->what) {
                            case W_FI:
                            case W_FI2:
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
                    listing_line(listing, 0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_ENDM, &epoint);
                waitfor->u.cmd_macro.val = NULL;
                waitfor->skip = 0;
                break;
            case CMD_FUNCTION: /* .function */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, 0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                new_waitfor(W_ENDF, &epoint);
                waitfor->skip = 0;
                break;
            case CMD_VAR: /* .var */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, 0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    goto breakerr;
                }
                break;
            case CMD_LBL: /* .lbl */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, 0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                    else if (!arguments.tasmcomp && diagnostics.deprecated) err_msg2(ERROR______OLD_GOTO, NULL, &epoint);
                }
                break;
            case CMD_PROC: /* .proc */
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, 0);
                    if (labelname.len == 0) err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint);
                }
                push_dummy_context();
                new_waitfor(W_PEND, &epoint);
                waitfor->skip = 0;waitfor->u.cmd_proc.label = NULL;
                break;
            case CMD_STRUCT: /* .struct */
                new_waitfor(W_ENDS, &epoint);
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, 0);
                    waitfor->u.cmd_struct.unionmode = current_address->unionmode;
                    current_address->unionmode = false;
                }
                break;
            case CMD_UNION: /* .union */
                new_waitfor(W_ENDU, &epoint);
                if ((waitfor->skip & 1) != 0) {
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, 0);
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
                    address2_t oldl_start, oldl_union;
                    bool oldunionmode;
                    struct values_s *vs;
                    Type *obj;
                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, 0);
                    if (!get_exp(1, 1, 0, &epoint)) goto breakerr;
                    vs = get_val(); val = vs->val;
                    if (here() == ',') lpoint.pos++;
                    obj = (prm == CMD_DUNION) ? UNION_OBJ : STRUCT_OBJ;
                    if (val->obj != obj) {err_msg_wrong_type2(val, obj, &vs->epoint); goto breakerr;}
                    current_section->structrecursion++;
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
                    current_section->structrecursion--;
                    current_address->start = oldstart;
                    oldstart = current_address->unionmode ? current_address->end : current_address->address;
                    if (oldend > current_address->end) current_address->end = oldend;
                    current_address->l_start = oldl_start;
                    current_address->l_union = oldl_union;
                    current_address->unionmode = oldunionmode;
                    if (oldstart > current_address->address) {
                        poke_pos = &epoint;
                        memskip(oldstart - current_address->address);
                    }
                }
                goto breakerr;
            case CMD_DSECTION: if ((waitfor->skip & 1) != 0)
                { /* .dsection */
                    struct section_s *tmp3;
                    str_t sectionname;

                    if (diagnostics.optimize) cpu_opt_invalidate();
                    listing_line(listing, epoint.pos);
                    if (current_section->structrecursion != 0) { err_msg2(ERROR___NOT_ALLOWED, ".dsection", &epoint); goto breakerr; }
                    epoint = lpoint;
                    sectionname.data = pline + lpoint.pos; sectionname.len = get_label();
                    if (sectionname.len == 0) {err_msg2(ERROR_LABEL_REQUIRE, NULL, &epoint); goto breakerr;}
                    tmp3=new_section(&sectionname);
                    if (tmp3->defpass == pass) {
                        err_msg_double_definedo(tmp3->file_list, &tmp3->epoint, &sectionname, &epoint);
                    } else {
                        address_t t;
                        if (tmp3->usepass == 0 || tmp3->defpass < pass - 1) {
                            size_t ln = tmp3->address.mem->mem.p, ln2 = tmp3->address.mem->p;
                            tmp3->address.wrapwarn = tmp3->address.moved = false;
                            tmp3->address.end = tmp3->address.start = tmp3->restart = tmp3->address.address = current_address->address;
                            tmp3->l_restart = tmp3->address.l_address = current_address->l_address;
                            tmp3->usepass = pass;
                            val_destroy(&tmp3->address.mem->v);
                            tmp3->address.mem = new_memblocks(ln, ln2);
                            tmp3->address.mem->lastaddr = tmp3->address.address;
                            if (diagnostics.optimize) cpu_opt_invalidate();
                            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                            fixeddig = false;
                        }
                        tmp3->provides = ~(uval_t)0;tmp3->requires = tmp3->conflicts = 0;
                        tmp3->address.unionmode = current_address->unionmode;
                        tmp3->address.l_start = current_address->l_start;
                        tmp3->address.l_union = current_address->l_union;
                        tmp3->structrecursion = current_section->structrecursion;
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
                                memjmp(tmp3->address.mem, current_address->address);
                            }
                            if (tmp3->l_restart.address != current_address->l_address.address ||
                                    tmp3->l_restart.bank != current_address->l_address.bank) {
                                tmp3->address.l_address.address = (tmp3->address.l_address.address + current_address->l_address.address - tmp3->l_restart.address) & 0xffff;
                                tmp3->address.l_address.bank = (tmp3->address.l_address.bank + current_address->l_address.bank - tmp3->l_restart.bank) & all_mem;
                                tmp3->l_restart = current_address->l_address;
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = false;
                            }
                        } else {
                            size_t ln = tmp3->address.mem->mem.p, ln2 = tmp3->address.mem->p;
                            if (!tmp3->address.moved) {
                                if (tmp3->address.end < tmp3->address.address) tmp3->address.end = tmp3->address.address;
                                tmp3->address.moved = true;
                            }
                            tmp3->address.wrapwarn = false;
                            t = tmp3->address.end - tmp3->address.start;
                            tmp3->address.end = tmp3->address.start = tmp3->restart = tmp3->address.address = current_address->address;
                            tmp3->address.l_address = current_address->l_address;
                            if (tmp3->l_restart.address != current_address->l_address.address ||
                                    tmp3->l_restart.bank != current_address->l_address.bank) {
                                tmp3->l_restart = current_address->l_address;
                                if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &epoint);
                                fixeddig = false;
                            }
                            tmp3->size = t;
                            val_destroy(&tmp3->address.mem->v);
                            tmp3->address.mem = new_memblocks(ln, ln2);
                            tmp3->address.mem->lastaddr = tmp3->address.address;
                            if (diagnostics.optimize) cpu_opt_invalidate();
                        }
                        tmp3->usepass = pass;
                        tmp3->defpass = pass;
                        if (t != 0) {
                            poke_pos = &epoint;
                            memskip(t);
                        }
                        memref(current_address->mem, tmp3->address.mem);
                    }
                }
                break;
            case CMD_SECTION: if ((waitfor->skip & 1) != 0)
                { /* .section */
                    listing_line(listing, 0);
                    if (section_start(&epoint)) goto breakerr;
                } else new_waitfor(W_SEND, &epoint);
                break;
            case lenof(command):
                if ((waitfor->skip & 1) != 0) goto as_macro2;
                break;
            default:
                if ((waitfor->skip & 1) != 0) {
                    listing_line(listing, epoint.pos);
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
                listing_line_cut(listing, epoint.pos);
                if (val->obj == MACRO_OBJ || val->obj == STRUCT_OBJ || val->obj == UNION_OBJ) {
                    Namespace *context;
                    if (newlabel != NULL && !((Macro *)val)->retval && newlabel->value->obj == CODE_OBJ) {
                        context = ((Code *)newlabel->value)->names;
                    } else {
                        Label *label;
                        bool labelexists;
                        str_t tmpname;
                        if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                        else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                        anonident2.type = '#';
                        anonident2.star_tree = star_tree;
                        anonident2.vline = vline;
                        tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                        label = new_label(&tmpname, mycontext, strength, &labelexists, current_file_list);
                        if (labelexists) {
                            if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                            label->constant = true;
                            label->owner = true;
                            label->defpass = pass;
                            if (label->value->obj != NAMESPACE_OBJ) {
                                val_destroy(label->value);
                                label->value = (Obj *)new_namespace(current_file_list, &epoint);
                            } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                        } else {
                            label->constant = true;
                            label->owner = true;
                            label->value = (Obj *)new_namespace(current_file_list, &epoint);
                            label->epoint = epoint;
                        }
                        context = (Namespace *)label->value;
                    }
                    val = macro_recurse(val->obj == MACRO_OBJ ? W_ENDM3 : val->obj == STRUCT_OBJ ? W_ENDS3 : W_ENDU3, val, context, &epoint);
                } else if (val->obj == MFUNC_OBJ) {
                    Label *label;
                    Mfunc *mfunc;
                    bool labelexists;
                    str_t tmpname;
                    if (sizeof(anonident2) != sizeof(anonident2.type) + sizeof(anonident2.padding) + sizeof(anonident2.star_tree) + sizeof(anonident2.vline)) memset(&anonident2, 0, sizeof anonident2);
                    else anonident2.padding[0] = anonident2.padding[1] = anonident2.padding[2] = 0;
                    anonident2.type = '#';
                    anonident2.star_tree = star_tree;
                    anonident2.vline = vline;
                    tmpname.data = (const uint8_t *)&anonident2; tmpname.len = sizeof anonident2;
                    label = new_label(&tmpname, ((Mfunc *)val)->namespaces[((Mfunc *)val)->nslen - 1], strength, &labelexists, current_file_list);
                    if (labelexists) {
                        if (label->defpass == pass) err_msg_double_defined(label, &tmpname, &epoint);
                        label->constant = true;
                        label->owner = true;
                        label->defpass = pass;
                        if (label->value->obj != NAMESPACE_OBJ) {
                            val_destroy(label->value);
                            label->value = (Obj *)new_namespace(current_file_list, &epoint);
                        } else ((Namespace *)label->value)->backr = ((Namespace *)label->value)->forwr = 0;
                    } else {
                        label->constant = true;
                        label->owner = true;
                        label->value = (Obj *)new_namespace(current_file_list, &epoint);
                        label->epoint = epoint;
                    }
                    mfunc = (Mfunc *)val_reference(val);
                    if (!get_exp(4, 0, 0, NULL)) {
                        val = NULL;
                        val_destroy(&mfunc->v);
                        goto breakerr;
                    }
                    val = mfunc_recurse(mfunc, (Namespace *)label->value, strength, &epoint);
                    val_destroy(&mfunc->v);
                } else val = macro_recurse(W_ENDM3, val, NULL, &epoint);
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

                if (newlabel != NULL && newlabel->value->obj == CODE_OBJ && labelname.len != 0 && labelname.data[0] != '_' && labelname.data[0] != '+' && labelname.data[0] != '-' && mycontext == current_context) {val_destroy(&cheap_context->v);cheap_context = ref_namespace(((Code *)newlabel->value)->names);}
                opname.data = pline + lpoint.pos; opname.len = get_label();
                if (opname.len == 3 && (prm = lookup_opcode(opname.data)) >= 0) {
                    Error *err;
                    struct linepos_s oldlpoint;
                    struct linepos_s epoints[3];
                    unsigned int w;
                    if (false) {
                as_opcode:
                        opname = labelname;
                    }
                    ignore();
                    oldlpoint = lpoint;
                    w = 3; /* 0=byte 1=word 2=long 3=negative/too big */
                    if (here() == 0 || here() == ';') {val = (Obj *)ref_addrlist(null_addrlist);}
                    else {
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
                                lpoint.pos += 2;
                            }
                        }
                        if (!get_exp(3, 0, 0, NULL)) goto breakerr;
                        val = get_vals_addrlist(epoints);
                    }
                    if (val->obj == TUPLE_OBJ || val->obj == LIST_OBJ) {
                        epoints[1] = epoints[0];
                        epoints[2] = epoints[0];
                        if (!instrecursion(val, prm, w, &epoint, epoints)) {
                            listing_instr(listing, 0, 0, -1);
                        }
                        err = NULL;
                    } else err = instruction(prm, w, val, &epoint, epoints);
                    val_destroy(val);
                    if (err == NULL) {
                        if (diagnostics.alias && prm != current_cpu->alias[prm]) err_msg_alias(current_cpu->mnemonic[prm], current_cpu->mnemonic[current_cpu->alias[prm]], &epoint);
                        break;
                    }
                    tmp2 = find_label(&opname, NULL);
                    if (tmp2 != NULL) {
                        const Type *obj = tmp2->value->obj;
                        if (diagnostics.case_symbol && str_cmp(&opname, &tmp2->name) != 0) err_msg_symbol_case(&opname, tmp2, &epoint);
                        if (obj == MACRO_OBJ || obj == SEGMENT_OBJ || obj == MFUNC_OBJ) {
                            val_destroy(&err->v);
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
        if (newlabel != NULL && !newlabel->update_after) set_size(newlabel, current_address->address - oaddr, current_address->mem, oaddr, newmembp);
    }

    while (oldwaitforp < waitfor_p) {
        const char *msg = check_waitfor();
        if (msg != NULL) err_msg2(ERROR_MISSING_CLOSE, msg, &waitfor->epoint);
        close_waitfor(waitfor->what);
    }
    return retval;
}

static void one_pass(int argc, char **argv, int opts, struct file_s *fin) {
    static const str_t none_enc = {4, (const uint8_t *)"none"};
    static struct linepos_s nopoint = {0, 0};
    struct file_s *cfile;
    Obj *val;
    int i;
    size_t ln = root_section.address.mem->mem.p, ln2 = root_section.address.mem->p;
    bool starexists;
    struct star_s *s;

    fixeddig = true;constcreated = false;error_reset();random_reseed(&int_value[0]->v, NULL);
    val_destroy(&root_section.address.mem->v);
    root_section.address.mem = new_memblocks(0, 0);
    if (diagnostics.optimize) cpu_opt_invalidate();
    for (i = opts - 1; i < argc; i++) {
        set_cpumode(arguments.cpumode); if (pass == 1 && i == opts - 1) constcreated = false;
        star = databank = dpage = strength = 0;longaccu = longindex = autosize = false;actual_encoding = new_encoding(&none_enc, &nopoint);
        allowslowbranch = true; longbranchasjmp = false; temporary_label_branch = 0;
        reset_waitfor();lpoint.line = vline = 0;outputeor = 0;
        reset_context();
        current_section = &root_section;
        current_address = &root_section.address;
        reset_section(current_section);
        init_macro();
        star_tree = &star_root;
        s = new_star(i, &starexists);
        s->addr = 0;
        star_tree = &s->tree;

        if (i == opts - 1) {
            if (fin->lines != 0) {
                enterfile(fin, &nopoint);
                listing_file(listing, ";******  Command line definitions", NULL);
                val = compile();
                if (val != NULL) val_destroy(val);
                exitfile();
            }
            val_destroy(&root_section.address.mem->v);
            root_section.address.mem = new_memblocks(ln, ln2);
            if (diagnostics.optimize) cpu_opt_invalidate();
            continue;
        }

        cfile = openfile(argv[i], "", 0, NULL, &nopoint);
        if (cfile != NULL) {
            enterfile(cfile, &nopoint);
            listing_file(listing, ";******  Processing input file: ", cfile);
            val = compile();
            if (val != NULL) val_destroy(val);
            closefile(cfile);
            exitfile();
        }
    }
    ref_labels();
    if (fixeddig) section_sizecheck();
    /*garbage_collect();*/
}

int main2(int *argc2, char **argv2[]) {
    size_t j;
    int opts;
    struct file_s *fin;
    static struct linepos_s nopoint = {0, 0};
    char **argv;
    int argc;

    err_init(*argv2[0]);
    avltree_init(&star_root);
    objects_init();
    init_section();
    init_file();
    init_variables();
    init_eval();

    fin = openfile(NULL, "", 0, NULL, &nopoint);
    opts = testarg(argc2, argv2, fin); argc = *argc2; argv = *argv2;
    if (opts <= 0) {
        tfree();
        return (opts < 0) ? EXIT_FAILURE : EXIT_SUCCESS;
    }
    init_encoding(arguments.to_ascii);

    if (arguments.quiet) {
        puts("64tass Turbo Assembler Macro V" VERSION "\n"
             "64TASS comes with ABSOLUTELY NO WARRANTY; This is free software, and you\n"
             "are welcome to redistribute it under certain conditions; See LICENSE!\n");
    }

    /* assemble the input file(s) */
    do {
        if (pass++>max_pass) {err_msg(ERROR_TOO_MANY_PASS, NULL);break;}
        listing_pccolumn = false;
        one_pass(argc, argv, opts, fin);
    } while (!fixeddig || constcreated);

    if (arguments.list == NULL) {
        if (diagnostics.unused.macro || diagnostics.unused.consts || diagnostics.unused.label || diagnostics.unused.variable) unused_check(root_namespace);
    }
    if (error_serious()) {status();return EXIT_FAILURE;}

    /* assemble again to create listing */
    if (arguments.list != NULL) {
        nolisting = 0;

        max_pass = pass; pass++;
        listing = listing_open(arguments.list, argc, argv);
        one_pass(argc, argv, opts, fin);
        listing_close(listing);
        listing = NULL;

        if (diagnostics.unused.macro || diagnostics.unused.consts || diagnostics.unused.label || diagnostics.unused.variable) unused_check(root_namespace);
    }

    for (j = 0; j < arguments.symbol_output_len; j++) {
        const struct symbol_output_s *s = &arguments.symbol_output[j];
        size_t k;
        for (k = 0; k < j; k++) {
            const struct symbol_output_s *s2 = &arguments.symbol_output[k];
            if (strcmp(s->name, s2->name) == 0) break;
        }
        if (labelprint(s, k != j)) break;
    }
    if (arguments.make != NULL) makefile(argc - opts, argv + opts);

    if (error_serious()) {status();return EXIT_FAILURE;}

    {
        struct section_s *section = find_this_section(arguments.output.section);
        if (section == NULL) {
            str_t sectionname;
            sectionname.data = pline;
            sectionname.len = lpoint.pos;
            err_msg2(ERROR__SECTION_ROOT, &sectionname, &nopoint);
        } else {
            output_mem(section->address.mem, &arguments.output);
        }
    }

    {
        bool e = error_serious();
        status();
        return e ? EXIT_FAILURE : EXIT_SUCCESS;
    }
}
