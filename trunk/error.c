/*
    $Id$

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
#include <string.h>
#include <errno.h>
#include <wchar.h>
#include <wctype.h>
#include <locale.h>
#include "error.h"
#include "misc.h"
#include "values.h"
#include "file.h"
#include "variables.h"
#include "64tass.h"
#include "macro.h"
#include "strobj.h"
#include "unicode.h"

static unsigned int errors = 0, warnings = 0;

static struct file_list_s file_list;
static const struct file_list_s *included_from = &file_list;
static struct file_list_s *current_file_list = &file_list;

struct errorbuffer_s {
    size_t max;
    size_t len;
    size_t header_pos;
    uint8_t *data;
};

static struct errorbuffer_s error_list = {0, 0, 0, NULL};
static struct avltree notdefines;

enum severity_e {
    SV_DOUBLENOTE, SV_NOTDEFGNOTE, SV_NOTDEFLNOTE, SV_WARNING, SV_CONDERROR, SV_DOUBLEERROR, SV_NOTDEFERROR, SV_NONEERROR, SV_ERROR, SV_FATAL
};

struct error_s {
    enum severity_e severity;
    size_t error_len;
    size_t line_len;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
};

struct notdefines_s {
    str_t cfname;
    const struct label_s *parent;
    uint8_t pass;
    struct avltree_node node;
};

static int check_duplicate(const struct error_s *nerr) {
    size_t pos;
    const struct error_s *err;
    for (pos = 0; pos < error_list.header_pos; pos = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7) {
        err = (const struct error_s *)&error_list.data[pos];
        if (err->severity != nerr->severity) continue;
        if (err->file_list != nerr->file_list) continue;
        if (err->line_len != nerr->line_len) continue;
        if (err->error_len != nerr->error_len) continue;
        if (err->epoint.line != nerr->epoint.line) continue;
        if (err->epoint.pos != nerr->epoint.pos) continue;
        if (memcmp(((uint8_t *)err) + sizeof(struct error_s), ((uint8_t *)nerr) + sizeof(struct error_s), err->line_len + err->error_len)) continue;
        return 1;
    }
    return 0;
}

static int close_error(void) {
    if (error_list.header_pos < error_list.len) {
        struct error_s *err = (struct error_s *)&error_list.data[error_list.header_pos];
        err->error_len = error_list.len - error_list.header_pos - sizeof(struct error_s) - err->line_len;
        switch (err->severity) {
        case SV_NOTDEFGNOTE:
        case SV_NOTDEFLNOTE:
        case SV_DOUBLENOTE: return 0;
        default: break;
        }
        if (check_duplicate(err)) {
            error_list.len = error_list.header_pos;
            return 1;
        }
    }
    return 0;
}

static int new_error(enum severity_e severity, const struct file_list_s *flist, linepos_t epoint) {
    struct error_s *err;
    size_t line_len;
    int dupl;
    dupl = close_error();
    switch (severity) {
    case SV_NOTDEFGNOTE:
    case SV_NOTDEFLNOTE:
    case SV_DOUBLENOTE:
        if (dupl) return 1;
        line_len = 0;
        break;
    default: line_len = ((epoint->line == lpoint.line) && in_macro()) ? (strlen((char *)pline) + 1) : 0; break;
    }
    error_list.header_pos = (error_list.len + 7) & ~7;
    if (error_list.header_pos + sizeof(struct error_s) + line_len > error_list.max) {
        error_list.max += (sizeof(struct error_s) > 0x200) ? sizeof(struct error_s) : 0x200;
        error_list.data = (uint8_t *)realloc(error_list.data, error_list.max);
        if (!error_list.data) {fputs("Out of memory error\n", stderr);exit(1);}
    }
    error_list.len = error_list.header_pos + sizeof(struct error_s) + line_len;
    err = (struct error_s *)&error_list.data[error_list.header_pos];
    err->severity = severity;
    err->error_len = 0;
    err->line_len = line_len;
    err->file_list = flist;
    err->epoint = *epoint;
    if (line_len) memcpy(&error_list.data[error_list.header_pos + sizeof(struct error_s)], pline, line_len);
    return 0;
}

static int file_list_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct file_list_s *a = cavltree_container_of(aa, struct file_list_s, node);
    const struct file_list_s *b = cavltree_container_of(bb, struct file_list_s, node);
    if (a->file->uid != b->file->uid) return a->file->uid - b->file->uid;
    if (a->epoint.line != b->epoint.line) return a->epoint.line - b->epoint.line;
    return a->epoint.pos - b->epoint.pos;
}

static void file_list_free(struct avltree_node *aa)
{
    struct file_list_s *a = avltree_container_of(aa, struct file_list_s, node);
    avltree_destroy(&a->members, file_list_free);
    free(a);
}

static struct file_list_s *lastfl = NULL;
struct file_list_s *enterfile(struct file_s *file, linepos_t epoint) {
    struct avltree_node *b;
    if (!lastfl) {
        lastfl = (struct file_list_s *)malloc(sizeof(struct file_list_s));
        if (!lastfl) err_msg_out_of_memory();
    }
    lastfl->file = file;
    lastfl->epoint = *epoint;

    b = avltree_insert(&lastfl->node, &current_file_list->members, file_list_compare);
    if (!b) {
        lastfl->parent = current_file_list;
        avltree_init(&lastfl->members);
        current_file_list = lastfl;
        lastfl = NULL;
        return current_file_list;
    }
    current_file_list = avltree_container_of(b, struct file_list_s, node);
    return current_file_list;
}

void exitfile(void) {
    if (current_file_list->parent) current_file_list = current_file_list->parent;
}

static void adderror2(const uint8_t *s, size_t len) {
    if (len + error_list.len > error_list.max) {
        error_list.max += (len > 0x200) ? len : 0x200;
        error_list.data = (uint8_t *)realloc(error_list.data, error_list.max);
        if (!error_list.data) {fputs("Out of memory error\n", stderr);exit(1);}
    }
    memcpy(error_list.data + error_list.len, s, len);
    error_list.len += len;
}

static void adderror(const char *s) {
    adderror2((const uint8_t *)s, strlen(s));
}

static const char *terr_warning[]={
    "top of memory exceeded",
    "memory bank exceeded",
    "possible jmp ($xxff) bug",
    "long branch used",
    "directive ignored",
    "label not on left side",
};

static const char *terr_error[]={
    "double defined range",
    "double defined escape",
    "extra characters on line",
    "constant too large",
    "floating point overflow",
    "address not in processor address space",
    "general syntax",
    "expression syntax",
    "label required",
    "missing argument",
    "division by zero",
    "zero value not allowed",
    "most significiant bit must be clear in byte",
    "instruction can't cross banks",
    "address out of section",
    "negative number raised on fractional power",
    "square root of negative number",
    "logarithm of non-positive number",
    "not in range -1.0 to 1.0",
    "not a one character string",
    "index out of range",
    "key error",
    "not hashable",
    "not a key and value pair",
    "can't convert to a %d bit signed integer",
    "can't convert to a %d bit unsigned integer",
    "operands could not be broadcast together with shapes %u and %u",
    "can't get sign",
    "can't get absolute value",
    "can't convert to integer",
    "can't get length",
    "can't get size",
    "can't convert to boolean",
    "not iterable",
    "no byte sized addressing mode for opcode",
    "no word sized addressing mode for opcode",
    "no long sized addressing mode for opcode",
    "not a direct page address",
    "not a data bank address",
    "not a bank 0 address"
};

static const char *terr_fatal[]={
    "can't open file ",
    "error reading file ",
    "can't write object file ",
    "can't write listing file ",
    "can't write label file ",
    "file recursion",
    "macro recursion too deep",
    "too many passes"
};

static void err_msg_variable(value_t val, linepos_t epoint) {
    value_t err;
    err = val->obj->repr(val, epoint);
    if (err->obj == STR_OBJ) adderror2(err->u.str.data, err->u.str.len);
    else err_msg_output(err);
    val_destroy(err);
}

void err_msg2(enum errors_e no, const void *prm, linepos_t epoint) {

    if (no < 0x40) {
        new_error(SV_WARNING, current_file_list, epoint);
        if (!arguments.warning) return;
        if (no == ERROR_WUSER_DEFINED) adderror2(((value_t)prm)->u.str.data, ((value_t)prm)->u.str.len);
        else adderror(terr_warning[no]);
        return;
    }

    if (no < 0x80) {
        char line[1024];
        switch (no) {
        case ERROR____PAGE_ERROR:
        case ERROR_BRANCH_TOOFAR:
        case ERROR____PTEXT_LONG:
        case ERROR__BRANCH_CROSS:
        case ERROR__USER_DEFINED:
        case ERROR___UNKNOWN_CHR:
        case ERROR_CANT_CROSS_BA:
        case ERROR_OUTOF_SECTION:
        case ERROR_DIVISION_BY_Z:
        case ERROR_NO_ZERO_VALUE:
        case ERROR_CONSTNT_LARGE: 
        case ERROR_NUMERIC_OVERF: 
        case ERROR_NEGFRAC_POWER:
        case ERROR_SQUARE_ROOT_N:
        case ERROR_LOG_NON_POSIT:
        case ERROR___MATH_DOMAIN: new_error(SV_CONDERROR, current_file_list, epoint); break;
        default: new_error(SV_ERROR, current_file_list, epoint);
        }
        switch (no) {
        case ERROR____PAGE_ERROR:
            adderror("page error at $");
            sprintf(line,"%06" PRIaddress, *(const address_t *)prm); adderror(line);
            break;
        case ERROR_BRANCH_TOOFAR:
            sprintf(line,"branch too far by %+d bytes", *(const int *)prm); adderror(line);
            break;
        case ERROR____PTEXT_LONG:
            sprintf(line,"ptext too long by %lu bytes", (unsigned long)*(const size_t *)prm - 0x100); adderror(line);
            break;
        case ERROR__BRANCH_CROSS:
            adderror("branch crosses page");
            break;
        case ERROR__USER_DEFINED:
            adderror2(((value_t)prm)->u.str.data, ((value_t)prm)->u.str.len);
            break;
        case ERROR___UNKNOWN_CHR:
            sprintf(line,"can't encode character $%02x", *(const uint32_t *)prm); adderror(line);
            break;
        case ERROR______EXPECTED:
            adderror((char *)prm);
            adderror(" expected");
            break;
        case ERROR___NOT_ALLOWED:
            adderror("not allowed here: ");
            adderror((char *)prm);
            break;
        case ERROR_RESERVED_LABL:
            adderror("reserved symbol name '");
            adderror2(((str_t *)prm)->data, ((str_t *)prm)->len);
            adderror("'");
            break;
        case ERROR_____NOT_BANK0:
        case ERROR____NOT_DIRECT:
        case ERROR__NOT_DATABANK:
            adderror(terr_error[no & 63]);
            adderror(" '");
            err_msg_variable((value_t)prm, epoint);
            adderror("'");
            break;
        case ERROR___UNKNOWN_CPU:
            adderror("unknown processor ");
            err_msg_variable((value_t)prm, epoint);
            break;
        default:
            adderror(terr_error[no & 63]);
        }
        return;
    }

    new_error(SV_FATAL, current_file_list, epoint);
    switch (no) {
    case ERROR_UNKNOWN_OPTIO:
        adderror("unknown option '");
        adderror2(((str_t *)prm)->data, ((str_t *)prm)->len);
        adderror("'");
        break;
    default:
        adderror(terr_fatal[no & 63]);
    }
    status(1);exit(1);
}

void err_msg(enum errors_e no, const void* prm) {
    err_msg2(no, prm, &lpoint);
}

static void str_name(const uint8_t *data, size_t len) {
    adderror(" '");
    if (len) {
        if (data[0] == '-') {
            adderror("-");
        } else if (data[0] == '+') {
            adderror("+");
        } else if (data[0] == '.' || data[0] == '#') {
            adderror("<anonymous>");
        } else adderror2(data, len);
    }
    adderror("'");
}

static void err_msg_str_name(const char *msg, const str_t *name, linepos_t epoint) {
    new_error(SV_ERROR, current_file_list, epoint);
    adderror(msg);
    if (name) str_name(name->data, name->len);
}

static void err_msg_char_name(const char *msg, const char *name, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror(msg);
    str_name((const uint8_t *)name, strlen(name));
}

static void err_msg_big_integer(const char *msg, int bits, linepos_t epoint) {
    char msg2[256];
    new_error(SV_CONDERROR, current_file_list, epoint);
    sprintf(msg2, msg, bits);
    adderror(msg2);
}

static void err_msg_no_forward(const str_t *name, linepos_t epoint) {
    err_msg_str_name("too early to reference", name, epoint);
}

static int notdefines_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct notdefines_s *a = cavltree_container_of(aa, struct notdefines_s, node);
    const struct notdefines_s *b = cavltree_container_of(bb, struct notdefines_s, node);
    int h = a->parent - b->parent;
    if (h) return h;
    return str_cmp(&a->cfname, &b->cfname);
}

static void notdefines_free(struct avltree_node *aa) {
    struct notdefines_s *a = avltree_container_of(aa, struct notdefines_s, node);
    free((uint8_t *)a->cfname.data);
    free(a);
}

static struct notdefines_s *lastnd = NULL;
static inline void err_msg_not_defined2(const str_t *name, const struct label_s *l, int down, linepos_t epoint) {
    struct notdefines_s *tmp2;
    struct avltree_node *b;

    if (constcreated && pass < max_pass) return;

    if (!lastnd) {
        lastnd = (struct notdefines_s *)malloc(sizeof(struct notdefines_s));
        if (!lastnd) err_msg_out_of_memory();
    }

    if (name->data) {
        str_cfcpy(&lastnd->cfname, name);
        lastnd->parent = l;
        lastnd->pass = pass;
        b=avltree_insert(&lastnd->node, &notdefines, notdefines_compare);
        if (b) {
            tmp2 = avltree_container_of(b, struct notdefines_s, node);
            if (tmp2->pass == pass) {
                return;
            }
            tmp2->pass = pass;
        } else {
            if (lastnd->cfname.data == name->data) str_cpy(&lastnd->cfname, name);
            else str_cfcpy(&lastnd->cfname, NULL);
            lastnd = NULL;
        }
    }

    new_error(SV_NOTDEFERROR, current_file_list, epoint);
    adderror("not defined");
    if (name->data) {
        str_name(name->data, name->len);
    } else {
        ssize_t count = name->len;
        adderror(" '");
        while (count) {
            adderror((count > 0) ? "+" : "-");
            count += (count > 0) ? -1 : 1;
        }
        adderror("'");
    }

    if (!l->file_list) {
        if (new_error(SV_NOTDEFGNOTE, current_file_list, epoint)) return;
        adderror("searched in the global scope");
    } else {
        if (new_error(SV_NOTDEFLNOTE, l->file_list, &l->epoint)) return;
        adderror("searched in");
        str_name(l->name.data, l->name.len);
        adderror(down ? " defined here, and in all it's parents" : " defined here");
    }
}

static void err_msg_no_addressing(atype_t addrtype, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror("no");
    if (addrtype == A_NONE) adderror(" implied");
    for (;addrtype & 0xfff; addrtype <<= 4) {
        const char *txt = "?";
        switch ((enum atype_e)((addrtype & 0xf00) >> 8)) {
        case A_NONE: continue;
        case A_IMMEDIATE: txt = " immediate"; break;
        case A_XR: txt = " x indexed"; break;
        case A_YR: txt = " y indexed"; break;
        case A_ZR: txt = " z indexed"; break;
        case A_SR: txt = " stack"; break;
        case A_RR: txt = " data stack"; break;
        case A_DR: txt = " direct page"; break;
        case A_BR: txt = " data bank"; break;
        case A_KR: txt = " program bank"; break;
        case A_I: txt = " indirect"; break;
        case A_LI: txt = " long indirect"; break;
        }
        adderror(txt);
    }
    adderror(" addressing mode for opcode");
}

static void err_msg_no_register(value_t val, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror("no register '");
    adderror2(val->u.reg.data, val->u.reg.len);
    adderror("' addressing mode for opcode");
}

static void err_msg_no_lot_operand(size_t opers, linepos_t epoint) {
    char msg2[256];
    new_error(SV_CONDERROR, current_file_list, epoint);
    sprintf(msg2, "no %u operand addressing mode for opcode", (unsigned int)opers);
    adderror(msg2);
}

static void err_msg_cant_broadcast(const char *msg, size_t v1, size_t v2, linepos_t epoint) {
    char msg2[256];
    new_error(SV_CONDERROR, current_file_list, epoint);
    sprintf(msg2, msg, (unsigned int)v1, (unsigned int)v2);
    adderror(msg2);
}

void err_msg_output(const value_t val) {
    if (val->obj == ERROR_OBJ) {
        switch (val->u.error.num) {
        case ERROR___NOT_DEFINED: err_msg_not_defined2(&val->u.error.u.notdef.ident, val->u.error.u.notdef.label, val->u.error.u.notdef.down, &val->u.error.epoint);break;
        case ERROR____NO_FORWARD: err_msg_no_forward(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR_REQUIREMENTS_: err_msg_requires(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR______CONFLICT: err_msg_conflicts(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR__INVALID_OPER: err_msg_invalid_oper(val->u.error.u.invoper.op, val->u.error.u.invoper.v1, val->u.error.u.invoper.v2, &val->u.error.epoint);break;
        case ERROR____STILL_NONE: err_msg_still_none(NULL, &val->u.error.epoint); break;
        case ERROR_____CANT_IVAL:
        case ERROR_____CANT_UVAL: err_msg_big_integer(terr_error[val->u.error.num & 63], val->u.error.u.bits, &val->u.error.epoint);break;
        case ERROR___INDEX_RANGE:
        case ERROR_CONSTNT_LARGE:
        case ERROR_NUMERIC_OVERF:
        case ERROR_NEGFRAC_POWER:
        case ERROR_SQUARE_ROOT_N:
        case ERROR_LOG_NON_POSIT:
        case ERROR___MATH_DOMAIN:
        case ERROR_BIG_STRING_CO:
        case ERROR_____KEY_ERROR:
        case ERROR__NO_BYTE_ADDR:
        case ERROR__NO_WORD_ADDR:
        case ERROR__NO_LONG_ADDR:
        case ERROR_DIVISION_BY_Z: err_msg_str_name(terr_error[val->u.error.num & 63], NULL, &val->u.error.epoint);break;
        case ERROR__NOT_KEYVALUE:
        case ERROR__NOT_HASHABLE:
        case ERROR_____CANT_SIGN:
        case ERROR______CANT_ABS:
        case ERROR______CANT_INT:
        case ERROR______CANT_LEN:
        case ERROR_____CANT_SIZE:
        case ERROR_____CANT_BOOL: err_msg_char_name(terr_error[val->u.error.num & 63], val->u.error.u.objname, &val->u.error.epoint);break;
        case ERROR_NO_ADDRESSING: err_msg_no_addressing(val->u.error.u.addressing, &val->u.error.epoint);break;
        case ERROR___NO_REGISTER: err_msg_no_register(val->u.error.u.reg, &val->u.error.epoint);break;
        case ERROR___NO_LOT_OPER: err_msg_no_lot_operand(val->u.error.u.opers, &val->u.error.epoint);break;
        case ERROR_CANT_BROADCAS: err_msg_cant_broadcast(terr_error[val->u.error.num & 63], val->u.error.u.broadcast.v1, val->u.error.u.broadcast.v2, &val->u.error.epoint);break;
        default: break;
        }
    }
}

void err_msg_output_and_destroy(value_t val) {
    err_msg_output(val);
    val_destroy(val);
}

void err_msg_wrong_type(const value_t val, obj_t expected, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror("wrong type '");
    adderror(val->obj->name);
    if (expected) {
        adderror("', expected '");
        adderror(expected->name);
    }
    adderror("'");
}

void err_msg_cant_calculate(const str_t *name, linepos_t epoint) {
    err_msg_str_name("can't calculate stable value", name, epoint);
}

void err_msg_still_none(const str_t *name, linepos_t epoint) {
    if ((constcreated || !fixeddig) && pass < max_pass) return;
    new_error(SV_NONEERROR, current_file_list, epoint);
    adderror("can't calculate this");
    if (name) str_name(name->data, name->len);
}

void err_msg_not_defined(const str_t *name, linepos_t epoint) {
    err_msg_str_name("not defined", name, epoint);
}

void err_msg_not_definedx(const str_t *name, linepos_t epoint) {
    new_error(SV_NOTDEFERROR, current_file_list, epoint);
    adderror("not defined");
    if (name) str_name(name->data, name->len);
}

void err_msg_requires(const str_t *name, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror("requirements not met");
    if (name) str_name(name->data, name->len);
}

void err_msg_conflicts(const str_t *name, linepos_t epoint) {
    new_error(SV_CONDERROR, current_file_list, epoint);
    adderror("conflict");
    if (name) str_name(name->data, name->len);
}

static void err_msg_double_defined2(const char *msg, const struct label_s *l, struct file_list_s *cflist, const str_t *labelname2, linepos_t epoint2) {
    new_error(SV_DOUBLEERROR, cflist, epoint2);
    adderror(msg);
    str_name(labelname2->data, labelname2->len);
    if (new_error(SV_DOUBLENOTE, l->file_list, &l->epoint)) return;
    adderror("previous definition of");
    str_name(labelname2->data, labelname2->len);
    adderror(" was here");
}

void err_msg_double_defined(const struct label_s *l, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("duplicate definition", l, current_file_list, labelname2, epoint2);
}

void err_msg_shadow_defined(const struct label_s *l, const struct label_s *l2) {
    err_msg_double_defined2("shadow definition", l, l2->file_list, &l2->name, &l2->epoint);
}

void err_msg_invalid_oper(const value_t op, const value_t v1, const value_t v2, linepos_t epoint) {
    if (v1->obj == ERROR_OBJ) {
        err_msg_output(v1);
        return;
    }
    if (v2 && v2->obj == ERROR_OBJ) {
        err_msg_output(v2);
        return;
    }

    new_error(SV_CONDERROR, current_file_list, epoint);

    adderror(v2 ? "invalid operands to " : "invalid type argument to ");
    adderror(op->u.oper.name);

    if (v2) {
        adderror("' '");
        adderror(v1->obj->name);
        adderror("' and '");
        adderror(v2->obj->name);
    } else {
        adderror("' '");
        adderror(v1->obj->name);
    }
    adderror("'");
}

void err_msg_argnum(unsigned int num, unsigned int min, unsigned int max, linepos_t epoint) {
    unsigned int n;
    char line[1024];

    new_error(SV_ERROR, current_file_list, epoint);
    adderror("expected ");
    n = min;
    if (min == max) adderror("exactly ");
    else if (num < min) adderror("at least ");
    else {n = max; adderror("at most "); }
    switch (n) {
    case 0: adderror("no arguments"); break;
    case 1: adderror("one argument"); break;
    default: sprintf(line, "%d arguments", n); adderror(line); break;
    }
    if (num) {
        sprintf(line, ", got %d", num);
        adderror(line);
    }
    return;
}

static linecpos_t calcpos(const uint8_t *line, size_t pos, int utf8) {
    size_t s, l;
    if (utf8) return pos + 1;
    s = l = 0;
    while (s < pos) {
        if (!line[s]) break;
        s += utf8len(line[s]);
        l++;
    }
    return l + 1;
}

static inline const uint8_t *get_line(const struct file_s *file, size_t line) {
    return &file->data[file->line[line - 1]];
}

static inline void print_error(FILE *f, const struct error_s *err) {
    const struct file_list_s *cflist = err->file_list;
    linepos_t epoint = &err->epoint;
    const uint8_t *line;
    int text = (cflist != &file_list);

    if (text) {
        if (cflist != included_from) {
            included_from = cflist;
            while (included_from->parent != &file_list) {
                line = get_line(included_from->parent->file, included_from->epoint.line);
                fputs((included_from == cflist) ? "In file included from " : "                      ", f);
                printable_print((uint8_t *)included_from->parent->file->realname, f);
                fprintf(f, ":%" PRIuline ":%" PRIlinepos, included_from->epoint.line, calcpos(line, included_from->epoint.pos, included_from->parent->file->coding == E_UTF8));
                included_from = included_from->parent;
                fputs((included_from->parent != &file_list) ? ",\n" : ":\n", f);
            }
            included_from = cflist;
        }
        line = err->line_len ? (((uint8_t *)err) + sizeof(struct error_s)) : get_line(cflist->file, epoint->line);
        printable_print((uint8_t *)cflist->file->realname, f);
        fprintf(f, ":%" PRIuline ":%" PRIlinepos ": ", epoint->line, calcpos(line, epoint->pos, cflist->file->coding == E_UTF8));
    } else {
        fputs("<command line>:0:0: ", f);
    }
    switch (err->severity) {
    case SV_NOTDEFGNOTE:
    case SV_NOTDEFLNOTE:
    case SV_DOUBLENOTE: fputs("note: ", f);break;
    case SV_WARNING: fputs("warning: ", f);break;
    case SV_CONDERROR:
    case SV_DOUBLEERROR:
    case SV_NOTDEFERROR:
    case SV_NONEERROR:
    case SV_ERROR: fputs("error: ", f);break;
    case SV_FATAL: fputs("fatal error: ", f);break;
    }
    printable_print2(((uint8_t *)err) + sizeof(struct error_s) + err->line_len, f, err->error_len);
    putc('\n', f);
    if (text) {
        if (err->severity != SV_NOTDEFGNOTE) {
            putc(' ', f);
            printable_print(line, f);
            fputs("\n ", f);
            caret_print(line, f, epoint->pos);
            fputs("^\n", f);
        }
    }
}

int error_print(int fix, int newvar, int anyerr) {
    const struct error_s *err, *err2;
    size_t pos, pos2;
    int noneerr = 0;
    warnings = errors = 0;
    close_error();

    for (pos = 0; !noneerr && pos < error_list.len; pos = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7) {
        err = (const struct error_s *)&error_list.data[pos];
        switch (err->severity) {
        case SV_NOTDEFGNOTE:
        case SV_NOTDEFLNOTE:
        case SV_DOUBLENOTE:
        case SV_WARNING:
        case SV_NONEERROR:
            break;
        case SV_CONDERROR: if (!fix) continue; noneerr = 1; break;
        case SV_NOTDEFERROR: if (newvar) continue; noneerr = 1; break;
        default: noneerr = 1; break;
        }
    }

    for (pos = 0; pos < error_list.len; pos = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7) {
        err = (const struct error_s *)&error_list.data[pos];
        switch (err->severity) {
        case SV_NOTDEFGNOTE:
        case SV_NOTDEFLNOTE:
            if (newvar) continue;
            pos2 = pos; err2 = err;
            do {
                pos2 = (pos2 + sizeof(struct error_s) + err2->line_len + err2->error_len + 7) & ~7;
                err2 = (const struct error_s *)&error_list.data[pos2];
                if (pos2 >= error_list.len) break;
            } while (noneerr && err2->severity == SV_NONEERROR);
            if (err2->severity != SV_NOTDEFERROR) break;
            do {
                pos2 = (pos2 + sizeof(struct error_s) + err2->line_len + err2->error_len + 7) & ~7;
                err2 = (const struct error_s *)&error_list.data[pos2];
                if (pos2 >= error_list.len) break;
            } while (noneerr && err2->severity == SV_NONEERROR);
            if (err2->severity != SV_NOTDEFGNOTE && err2->severity != SV_NOTDEFLNOTE) break;
            if (err->severity != err2->severity) break;
            if (err->severity == SV_NOTDEFGNOTE) continue;
            if (err->file_list == err2->file_list && err->error_len == err2->error_len && err->epoint.line == err2->epoint.line &&
                err->epoint.pos == err2->epoint.pos) continue;
            break;
        case SV_DOUBLENOTE:
            pos2 = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7;
            err2 = (const struct error_s *)&error_list.data[pos2];
            if (pos2 >= error_list.len || err2->severity != SV_DOUBLEERROR) break;
            pos2 = (pos2 + sizeof(struct error_s) + err2->line_len + err2->error_len + 7) & ~7;
            err2 = (const struct error_s *)&error_list.data[pos2];
            if (pos2 >= error_list.len || err2->severity != SV_DOUBLENOTE) break;
            if (err->file_list == err2->file_list && err->error_len == err2->error_len && err->epoint.line == err2->epoint.line &&
                err->epoint.pos == err2->epoint.pos) continue;
            break;
        case SV_WARNING: warnings++; if (!arguments.warning || anyerr) continue; break;
        case SV_CONDERROR: if (!fix) continue; errors++; break;
        case SV_NOTDEFERROR: if (newvar) continue; errors++; break;
        case SV_NONEERROR: if (noneerr) continue; errors++; break;
        default: errors++; break;
        }
        print_error(stderr, err);
    }
    return errors;
}

void error_reset(void) {
    error_list.len = error_list.header_pos = 0;
    current_file_list = &file_list;
    included_from = &file_list;
}

void err_init(void) {
    avltree_init(&file_list.members);
    error_list.len = error_list.max = error_list.header_pos = 0;
    error_list.data = NULL;
    current_file_list = &file_list;
    included_from = &file_list;
    avltree_init(&notdefines);
}

void err_destroy(void) {
    avltree_destroy(&file_list.members, file_list_free);
    free(lastfl);
    free(error_list.data);
    avltree_destroy(&notdefines, notdefines_free);
    free(lastnd);
}

void err_msg_out_of_memory(void)
{
    error_print(0, 1, 1);
    fputs("Out of memory error\n", stderr);
    exit(1);
}

void err_msg_file(enum errors_e no, const char *prm, linepos_t epoint) {
    mbstate_t ps;
    const char *s;
    wchar_t w;
    uint8_t s2[10];
    size_t n, i = 0;
    ssize_t l;

#ifdef _WIN32
    setlocale(LC_ALL, "");
#endif
    s = strerror(errno);
    n = strlen(s);

    new_error(SV_FATAL, current_file_list, epoint);
    adderror(terr_fatal[no & 63]);
    adderror(prm);
    adderror(": ");
    memset(&ps, 0, sizeof(ps));
    while (i < n) {
        if (s[i] && !(s[i] & 0x80)) {
            adderror2((uint8_t *)s + i, 1);
            i++;
            continue;
        }
        l = mbrtowc(&w, s + i, n - i,  &ps);
        if (l <= 0) break;
        s2[utf8out(w, s2) - s2] = 0;
        adderror((char *)s2);
        i += l;
    }
#ifdef _WIN32
    setlocale(LC_ALL, "C");
#endif
    status(1);exit(1);
}

void error_status(void) {
    printf("Error messages:    ");
    if (errors) printf("%u\n", errors); else puts("None");
    printf("Warning messages:  ");
    if (warnings) printf("%u\n", warnings); else puts("None");
}

linecpos_t interstring_position(linepos_t epoint, const uint8_t *data, size_t i, uint32_t ch) {
    if (epoint->line == lpoint.line && strlen((char *)pline) > epoint->pos) {
        uint8_t q = pline[epoint->pos];
        if (q == '"' || q == '\'') {
            linecpos_t pos = epoint->pos + 1;
            size_t pp = 0;
            uint32_t ch2;
            while (pp < i && pline[pos]) {
                unsigned int ln2;
                if (pline[pos] == q) {
                    if (pline[pos + 1] != q) break;
                    pos++;
                }
                ln2 = utf8len(pline[pos]);
                if (memcmp(pline + pos, data + pp, ln2)) break;
                pos += ln2; pp += ln2;
            }
            if (pp == i) {
                ch2 = pline[pos];
                if (ch2 & 0x80) utf8in(data + i, &ch2);
                if (ch2 == ch) return pos;
            }
        }
    }
    return epoint->pos;
}

int error_serious(int fix, int newvar) {
    const struct error_s *err;
    size_t pos;
    close_error();
    for (pos = 0; pos < error_list.len; pos = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7) {
        err = (const struct error_s *)&error_list.data[pos];
        switch (err->severity) {
        case SV_NOTDEFGNOTE:
        case SV_NOTDEFLNOTE:
        case SV_DOUBLENOTE:
        case SV_WARNING: break;
        case SV_NONEERROR:
        case SV_CONDERROR: if (fix && !newvar) return 1; break;
        case SV_NOTDEFERROR: if (!newvar) return 1; break;
        default: return 1;
        }
    }
    return 0;
}

MUST_CHECK value_t new_error_obj(enum errors_e num, linepos_t epoint) {
    value_t v = val_alloc(ERROR_OBJ);
    v->u.error.num = num;
    v->u.error.epoint = *epoint;
    return v;
}
