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
#ifndef _WIN32
#include <wchar.h>
#endif
#include <wctype.h>
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
    str_t name;
    const struct label_s *parent;
    uint8_t pass;
    struct avltree_node node;
};

static void close_error(struct errorbuffer_s *elist) {
    if (elist->header_pos < elist->len) {
        struct error_s *err = (struct error_s *)&elist->data[elist->header_pos];
        err->error_len = elist->len - elist->header_pos - sizeof(struct error_s) - err->line_len;
    }
}

static void new_error(enum severity_e severity, const struct file_list_s *flist, linepos_t epoint) {
    struct error_s *err;
    size_t line_len;
    switch (severity) {
    case SV_NOTDEFGNOTE:
    case SV_NOTDEFLNOTE:
    case SV_DOUBLENOTE: line_len = 0;break;
    default: line_len = in_macro() ? (strlen((char *)pline) + 1) : 0; break;
    }
    close_error(&error_list);
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
    "general syntax",
    "expression syntax",
    "label required",
    "missing argument",
    "division by zero",
    "zero value not allowed",
    "instruction can't cross banks",
    "address out of section",
    "negative number raised on fractional power",
    "string constant too long for a number",
    "index out of range",
    "key error",
    "not hashable",
    "not a key and value pair",
    "can't convert to a %d bit signed integer",
    "can't convert to a %d bit unsigned integer",
    "can't convert to float",
    "can't get sign",
    "can't get absolute value",
    "can't convert to integer",
    "can't get length",
    "can't get size",
    "can't convert to boolean",
    "not iterable",
    "no fitting addressing mode for opcode",
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
    "reference recursion too deep",
    "too many passes"
};

void err_msg2(enum errors_e no, const void* prm, linepos_t epoint) {

    if (no < 0x40) {
        new_error(SV_WARNING, current_file_list, epoint);
        if (!arguments.warning) return;
        if (no == ERROR_WUSER_DEFINED) adderror2(((struct errorbuffer_s *)prm)->data, ((struct errorbuffer_s *)prm)->len);
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
        case ERROR____WRONG_TYPE:
        case ERROR_CONSTNT_LARGE: new_error(SV_CONDERROR, current_file_list, epoint); break;
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
            adderror2(((struct errorbuffer_s *)prm)->data, ((struct errorbuffer_s *)prm)->len);
            break;
        case ERROR___UNKNOWN_CHR:
            sprintf(line,"can't encode character $%02x", *(const uint32_t *)prm); adderror(line);
            break;
        case ERROR______EXPECTED:
            adderror((char *)prm);
            adderror(" expected");
            break;
        case ERROR____WRONG_TYPE:
            adderror("wrong type ");
            adderror((char *)prm);
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
            err_msg_variable(&error_list, (struct value_s *)prm, epoint);
            adderror("'");
            break;
        default:
            adderror(terr_error[no & 63]);
        }
        return;
    }
    
    new_error(SV_FATAL, current_file_list, epoint);
    switch (no) {
    case ERROR___UNKNOWN_CPU:
        adderror("unknown cpu: ");
        adderror((char *)prm);
        break;
    case ERROR_UNKNOWN_OPTIO:
        adderror("unknown option: ");
        adderror((char *)prm);
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
    return arguments.casesensitive ? str_cmp(&a->name, &b->name) : str_casecmp(&a->name, &b->name);
}

static void notdefines_free(struct avltree_node *aa) {
    struct notdefines_s *a = avltree_container_of(aa, struct notdefines_s, node);
    free((uint8_t *)a->name.data);
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
    lastnd->name = *name;
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
        str_cpy(&lastnd->name, name);
        lastnd = NULL;
    }

    new_error(SV_NOTDEFERROR, current_file_list, epoint);
    adderror("not defined");
    if (name) str_name(name->data, name->len);

    if (!l->file_list) {
        new_error(SV_NOTDEFGNOTE, current_file_list, epoint);
        adderror("searched in the global scope");
    } else {
        new_error(SV_NOTDEFLNOTE, l->file_list, &l->epoint);
        adderror("searched in");
        str_name(l->name.data, l->name.len);
        adderror(down ? " defined here, and in all it's parents" : " defined here");
    }
}

void err_msg_output(const struct value_s *val) {
    if (val->obj == ERROR_OBJ) {
        switch (val->u.error.num) {
        case ERROR___NOT_DEFINED: err_msg_not_defined2(&val->u.error.u.notdef.ident, val->u.error.u.notdef.label, val->u.error.u.notdef.down, &val->u.error.epoint);break;
        case ERROR____NO_FORWARD: err_msg_no_forward(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR_REQUIREMENTS_: err_msg_requires(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR______CONFLICT: err_msg_conflicts(&val->u.error.u.ident, &val->u.error.epoint);break;
        case ERROR__INVALID_OPER: err_msg_invalid_oper(val->u.error.u.invoper.op, val->u.error.u.invoper.v1, val->u.error.u.invoper.v2, &val->u.error.epoint);break;
        case ERROR_____CANT_IVAL:
        case ERROR_____CANT_UVAL: err_msg_big_integer(terr_error[val->u.error.num & 63], val->u.error.u.bits, &val->u.error.epoint);break;
        case ERROR___INDEX_RANGE:
        case ERROR_CONSTNT_LARGE:
        case ERROR_NEGFRAC_POWER:
        case ERROR_BIG_STRING_CO:
        case ERROR_____KEY_ERROR:
        case ERROR_DIVISION_BY_Z: err_msg_str_name(terr_error[val->u.error.num & 63], NULL, &val->u.error.epoint);break;
        case ERROR__NOT_KEYVALUE:
        case ERROR__NOT_HASHABLE:
        case ERROR_____CANT_REAL:
        case ERROR_____CANT_SIGN:
        case ERROR______CANT_ABS:
        case ERROR______CANT_INT:
        case ERROR______CANT_LEN:
        case ERROR_____CANT_SIZE:
        case ERROR_____CANT_BOOL: err_msg_char_name(terr_error[val->u.error.num & 63], val->u.error.u.objname, &val->u.error.epoint);break;
        default: break;
        }
    }
}

void err_msg_output_and_destroy(struct value_s *val) {
    err_msg_output(val);
    return val->obj->destroy(val);
}

void err_msg_wrong_type(const struct value_s *val, linepos_t epoint) {
    err_msg2(ERROR____WRONG_TYPE, val->obj->name, epoint);
}

void err_msg_cant_calculate(const str_t *name, linepos_t epoint) {
    err_msg_str_name("can't calculate stable value", name, epoint);
    return;
}

void err_msg_still_none(const str_t *name, linepos_t epoint) {
    if ((constcreated || !fixeddig) && pass < max_pass) return;
    new_error(SV_NONEERROR, current_file_list, epoint);
    adderror("can't calculate this");
    if (name) str_name(name->data, name->len);
    return;
}

void err_msg_not_defined(const str_t *name, linepos_t epoint) {
    err_msg_str_name("not defined", name, epoint);
    return;
}

void err_msg_not_definedx(const str_t *name, linepos_t epoint) {
    new_error(SV_NOTDEFERROR, current_file_list, epoint);
    adderror("not defined");
    if (name) str_name(name->data, name->len);
    return;
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

static void add_user_error2(struct errorbuffer_s *user_error, const uint8_t *s, size_t len) {
    if (len + user_error->len > user_error->max) {
        user_error->max += (len > 0x100) ? len : 0x100;
        user_error->data = (uint8_t *)realloc(user_error->data, user_error->max);
        if (!user_error->data) {fputs("Out of memory error\n", stderr);exit(1);}
    }
    memcpy(user_error->data + user_error->len, s, len);
    user_error->len += len;
}

void err_msg_variable(struct errorbuffer_s *user_error, struct value_s *val, linepos_t epoint) {
    struct value_s tmp;
    if (!val) {user_error->len = 0;return;}
    val->obj->str(val, &tmp, epoint);
    if (tmp.obj == STR_OBJ) add_user_error2(user_error, tmp.u.str.data, tmp.u.str.len);
    else err_msg_output(&tmp);
    tmp.obj->destroy(&tmp);
}

static void err_msg_double_defined2(const char *msg, const struct label_s *l, struct file_list_s *cflist, const str_t *labelname2, linepos_t epoint2) {
    new_error(SV_DOUBLEERROR, cflist, epoint2);
    adderror(msg);
    str_name(labelname2->data, labelname2->len);
    new_error(SV_DOUBLENOTE, l->file_list, &l->epoint);
    adderror("previous definition of");
    str_name(l->name.data, l->name.len);
    adderror(" was here");
}

void err_msg_double_defined(const struct label_s *l, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("duplicate definition", l, current_file_list, labelname2, epoint2);
}

void err_msg_shadow_defined(const struct label_s *l, const struct label_s *l2) {
    err_msg_double_defined2("shadow definition", l, l2->file_list, &l2->name, &l2->epoint);
}

void err_msg_invalid_oper(const struct value_s *op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
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

static inline int utf8len(uint8_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}

static linecpos_t calcpos(const uint8_t *line, size_t pos, int utf8) {
    if (utf8) return pos + 1;
    size_t s = 0, l = 0;
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

static inline void caret_print(const uint8_t *line, FILE *f, size_t max) {
    size_t i, l = 0;
    for (i = 0; i < max;) {
        char temp[64];
        uint32_t ch = line[i];
        if (ch & 0x80) {
            i += utf8in(line + i, &ch);
#ifdef _WIN32
            if (!iswprint(ch)) l += sprintf(temp, "{$%x}", ch); else l++;
#else
            if (!iswprint(ch) || sprintf(temp, "%lc", ch) < 0) l += sprintf(temp, "{$%x}", ch); else l++;
#endif
            continue;
        }
        if (ch == 0) break;
        if (ch == '\t') {
            while (l) { 
                putc(' ', f); 
                l--; 
            }
            putc('\t', f);
            i++;
            continue;
        }
        if (ch < 0x20 || ch > 0x7e) l += sprintf(temp, "{$%x}", ch); else l++;
        i++;
    }
    while (l) { 
        putc(' ', f); 
        l--; 
    }
}

static inline void printable_print2(const uint8_t *line, FILE *f, size_t max) {
    size_t i, l = 0;
    for (i = 0; i < max;) {
        uint32_t ch = line[i];
        if (ch & 0x80) {
#ifdef _WIN32
            i += utf8in(line + i, &ch);
            if (iswprint(ch)) continue;
            if (l != i) fwrite(line + l, i - l, 1, f);
            fprintf(f, "{$%x}", ch);
#else
            if (l != i) fwrite(line + l, i - l, 1, f);
            i += utf8in(line + i, &ch);
            if (!iswprint(ch) || fprintf(f, "%lc", ch) < 0) fprintf(f, "{$%x}", ch);
#endif
            l = i;
            continue;
        }
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) fwrite(line + l, i - l, 1, f);
            i++;
            fprintf(f, "{$%x}", ch);
            l = i;
            continue;
        }
        i++;
    }
    if (i != l) fwrite(line + l, i - l, 1, f);
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
    close_error(&error_list);

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
            pos2 = (pos + sizeof(struct error_s) + err->line_len + err->error_len + 7) & ~7;
            err2 = (const struct error_s *)&error_list.data[pos2];
            if (pos2 >= error_list.len || err2->severity != SV_NOTDEFERROR) break;
            pos2 = (pos2 + sizeof(struct error_s) + err2->line_len + err2->error_len + 7) & ~7;
            err2 = (const struct error_s *)&error_list.data[pos2];
            if (pos2 >= error_list.len || (err2->severity != SV_NOTDEFGNOTE && err2->severity != SV_NOTDEFLNOTE)) break;
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
    error_init(&error_list);
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
    new_error(SV_FATAL, current_file_list, epoint);
    adderror(terr_fatal[no & 63]);
    adderror(prm);
    adderror(": ");
    adderror(strerror(errno));
    status(1);exit(1);
}

void error_init(struct errorbuffer_s *error) {
    error->len = error->max = error->header_pos = 0;
    error->data = NULL;
}

void errors_destroy(struct errorbuffer_s *error) {
    free(error->data);
}

void error_status(void) {
    printf("Error messages:    ");
    if (errors) printf("%u\n", errors); else puts("None");
    printf("Warning messages:  ");
    if (warnings) printf("%u\n", warnings); else puts("None");
}

int error_serious(int fix, int newvar) {
    const struct error_s *err;
    size_t pos;
    close_error(&error_list);
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

