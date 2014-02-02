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
#include "error.h"
#include "misc.h"
#include "values.h"
#include "file.h"
#include "variables.h"
#include "64tass.h"
#include "strobj.h"

#define MAX_ERRORS 99

unsigned int errors=0,conderrors=0,warnings=0;

static struct file_list_s file_list;
static struct file_list_s *included_from;
static struct file_list_s *current_file_list;

static struct error_s error_list = {0,0,0,NULL};
static struct avltree notdefines;
static int notdefinespass;

struct notdefines_s {
    str_t name;
    const struct label_s *parent;
    uint8_t pass;
    struct avltree_node node;
};

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
        if (!error_list.data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(error_list.data + error_list.len, s, len);
    error_list.len += len;
}

static void adderror(const char *s) {
    adderror2((const uint8_t *)s, strlen(s));
}

static void addorigin(struct file_list_s *cflist, linepos_t lpoint2) {
    char line[256];
    if (cflist && cflist->parent) {
        if (cflist != included_from) {
            included_from = cflist;
            while (included_from->parent->parent) {
                adderror( (included_from == cflist) ? "In file included from " : "                      ");
                adderror(included_from->parent->file->realname);
                sprintf(line,":%" PRIuline ":%" PRIlinepos, included_from->epoint.line, included_from->epoint.pos - included_from->epoint.upos + 1);
                adderror(line);
                included_from = included_from->parent;
                adderror(included_from->parent->parent ? ",\n" : ":\n");
            }
            included_from = cflist;
        }
        if (cflist->file->realname[0]) {
            adderror(cflist->file->realname);
        } else {
            adderror("<command line>");
        }
    } else {
        adderror("<command line>");
    }
    sprintf(line,":%" PRIuline ":%" PRIlinepos ": ", lpoint2->line, lpoint2->pos - lpoint2->upos + 1); 
    adderror(line);
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
    "missing argument",
    "illegal operand",
    "division by zero",
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
    "not a direct page address"
};

static const char *terr_fatal[]={
    "can't open file ",
    "error reading file ",
    "can't write object file ",
    "can't write listing file ",
    "can't write label file ",
    "file recursion\n",
    "macro recursion too deep\n",
    "reference recursion too deep\n",
    "too many passes\n",
    "too many errors\n"
};

static void inc_errors(void) {
    if (!errors) conderrors = warnings = error_list.len = 0;
    errors++;
}

void err_msg2(enum errors_e no, const void* prm, linepos_t lpoint2) {
    if (pass == 1 && no < 0x80) return;
    if (errors + conderrors == MAX_ERRORS && no >= 0x40) no = ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        if (errors) return;
        warnings++;
        return;
    }

    if (no<0x40) {
        if (errors) return;
        addorigin(current_file_list, lpoint2);
        adderror("warning: ");
        warnings++;
        if (no == ERROR_WUSER_DEFINED) adderror2(((struct error_s *)prm)->data, ((struct error_s *)prm)->len);
        else adderror(terr_warning[no]);
    }
    else if (no<0x80) {
        char line[1024];
        switch (no) {
        case ERROR____PAGE_ERROR:
        case ERROR_BRANCH_TOOFAR:
        case ERROR__BRANCH_CROSS:
        case ERROR__USER_DEFINED:
        case ERROR___UNKNOWN_CHR:
        case ERROR_CANT_CROSS_BA:
        case ERROR_OUTOF_SECTION:
        case ERROR_CONSTNT_LARGE:
            if (errors) return; 
            conderrors++;break;
        default: inc_errors();
        }
        addorigin(current_file_list, lpoint2);
        adderror("error: ");
        switch (no) {
        case ERROR____PAGE_ERROR:
            adderror("page error at $");
            sprintf(line,"%06" PRIaddress, *(const address_t *)prm); adderror(line);
            break;
        case ERROR_BRANCH_TOOFAR:
            sprintf(line,"branch too far by %+d bytes", *(const int *)prm); adderror(line);
            break;
        case ERROR__BRANCH_CROSS:
            adderror("branch crosses page");
            break;
        case ERROR__USER_DEFINED:
            adderror2(((struct error_s *)prm)->data, ((struct error_s *)prm)->len);
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
        default:
            adderror(terr_error[no & 63]);
        }
    }
    else {
        if (no == ERROR__TOO_MANY_ERR) errors++; else inc_errors();
        addorigin(current_file_list, lpoint2);
        adderror("fatal error: ");
        switch (no) {
        case ERROR___UNKNOWN_CPU:
            adderror("unknown cpu: ");
            adderror((char *)prm);
            adderror("\n");
            break;
        case ERROR_UNKNOWN_OPTIO:
            adderror("unknown option: ");
            adderror((char *)prm);
            adderror("\n");
            break;
        default:
            adderror(terr_fatal[no & 63]);
        }
        status();exit(1);
    }
    adderror("\n");
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
    if (pass == 1) return;
    if (errors + conderrors == MAX_ERRORS) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }
    inc_errors();
    addorigin(current_file_list, epoint);
    adderror(msg);
    if (name) str_name(name->data, name->len);
    adderror("\n");
    return;
}

static void err_msg_char_name(const char *msg, const char *name, linepos_t epoint) {
    str_t tmp;
    tmp.data = (const unsigned char *)name;
    tmp.len = strlen(name);
    err_msg_str_name(msg, &tmp, epoint);
}

static void err_msg_big_integer(const char *msg, int bits, linepos_t epoint) {
    char msg2[256];
    if (pass == 1) return;
    if (errors + conderrors == MAX_ERRORS) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }
    if (fixeddig) inc_errors(); else {
        if (errors) return; 
        conderrors++;
    }
    addorigin(current_file_list, epoint);
    sprintf(msg2, msg, bits);
    adderror(msg2);
    adderror("\n");
    return;
}

static void err_msg_no_forward(const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    err_msg_str_name("error: too early to reference", name, epoint);
    return;
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
static void err_msg_not_defined2(const str_t *name, const struct label_s *l, int down, linepos_t epoint) {
    struct notdefines_s *tmp2;
    struct avltree_node *b;
    if (pass == 1) return;
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
    err_msg_str_name("error: not defined", name, epoint);
    if (!l->file_list) {
        addorigin(current_file_list, epoint);
        adderror("note: searched in the global scope\n");
    } else {
        addorigin(l->file_list, &l->epoint);
        adderror("note: searched in");
        str_name(l->name.data, l->name.len);
        if (down) adderror(" defined here, and in all it's parents\n");
        else adderror(" defined here\n");
    }
    if (notdefinespass != pass) {
        notdefinespass = pass;
        addorigin(l->file_list ? l->file_list : current_file_list, epoint);
        adderror("note: each undefined identifier is reported only once for each scope\n");
    }
}

static void err_msg_output(const struct value_s *val) {
    if (pass != 1 && val->obj == ERROR_OBJ) {
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
    if (pass == 1) return;
    err_msg2(ERROR____WRONG_TYPE, val->obj->name, epoint);
}

void err_msg_cant_calculate(const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    err_msg_str_name("error: can't calculate stable value", name, epoint);
    return;
}

void err_msg_not_defined(const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    err_msg_str_name("error: not defined", name, epoint);
    return;
}

void err_msg_requires(const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    err_msg_str_name("error: requirements not met", name, epoint);
    return;
}

void err_msg_conflicts(const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    err_msg_str_name("error: conflict", name, epoint);
    return;
}

static void add_user_error2(struct error_s *user_error, const uint8_t *s, size_t len) {
    if (len + user_error->len > user_error->max) {
        user_error->max += (len > 0x100) ? len : 0x100;
        user_error->data = (uint8_t *)realloc(user_error->data, user_error->max);
        if (!user_error->data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(user_error->data + user_error->len, s, len);
    user_error->len += len;
    user_error->chars += len;
}

void err_msg_variable(struct error_s *user_error, struct value_s *val) {
    struct value_s tmp;
    if (!val) {user_error->chars = user_error->len = 0;return;}
    val->obj->str(val, &tmp, NULL);
    if (tmp.obj == STR_OBJ) {
        add_user_error2(user_error, tmp.u.str.data, tmp.u.str.len);
        user_error->chars -= tmp.u.str.len - tmp.u.str.chars;
    } else err_msg_output(&tmp);
    tmp.obj->destroy(&tmp);
}

static void err_msg_double_defined2(const char *msg, const struct label_s *l, struct file_list_s *cflist, const str_t *labelname2, linepos_t epoint2) {

    if (errors + conderrors == MAX_ERRORS) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }

    inc_errors();
    addorigin(cflist, epoint2);
    adderror(msg);
    str_name(labelname2->data, labelname2->len);
    adderror("\n");
    addorigin(l->file_list, &l->epoint);
    adderror("note: previous definition of");
    str_name(l->name.data, l->name.len);
    adderror(" was here\n");
}

void err_msg_double_defined(const struct label_s *l, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("error: duplicate definition", l, current_file_list, labelname2, epoint2);
}

void err_msg_shadow_defined(const struct label_s *l, const struct label_s *l2) {
    err_msg_double_defined2("error: shadow definition", l, l2->file_list, &l2->name, &l2->epoint);
}

static int err_oper(const char *msg, const struct value_s *op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    if (pass == 1) return 0;
    if (v1->obj == ERROR_OBJ) {
        err_msg_output(v1);
        return 0;
    }
    if (v2 && v2->obj == ERROR_OBJ) {
        err_msg_output(v2);
        return 0;
    }

    if (errors + conderrors == MAX_ERRORS) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return 0;
    }

    addorigin(current_file_list, epoint);

    adderror(msg);
    if (v2) {
        adderror(" operands to ");
    } else {
        adderror(" type argument to ");
    }
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
    adderror("'\n");
    return 1;
}

void err_msg_invalid_oper(const struct value_s *op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    if (err_oper("error: invalid", op, v1, v2, epoint)) errors++;
}

void err_msg_argnum(unsigned int num, unsigned int min, unsigned int max, linepos_t epoint) {
    unsigned int n;
    char line[1024];
    if (pass == 1) return;
    if (errors + conderrors == MAX_ERRORS) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }
    inc_errors();
    addorigin(current_file_list, epoint);
    adderror("error: expected ");
    n = min;
    if (min == max) adderror("exactly ");
    else if (num < min) adderror("at least ");
    else {n = max; adderror("at most "); }
    switch (n) {
    case 0: adderror("no arguments"); break;
    case 1: adderror("one argument"); break;
    default: sprintf(line, "%d arguments", n); adderror(line); break;
    }
    sprintf(line, ", got %d\n", num);
    adderror(line);
    return;
}

void freeerrorlist(int print) {
    if (print) {
        fwrite(error_list.data, error_list.len, 1, stderr);
    }
    error_list.len = 0;
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
    freeerrorlist(1);
    fputs("Out of memory error\n", stderr);
    exit(1);
}

void err_msg_file(enum errors_e no, const char* prm) {
    char *error;
    inc_errors();
    addorigin(current_file_list, &lpoint);
    adderror("fatal error: ");
    adderror(terr_fatal[no & 63]);
    adderror(prm);
    adderror(": ");
    error = strerror(errno);
    adderror(error);
    adderror("\n");
    status();exit(1);
}

void error_init(struct error_s *error) {
    error->len = error->chars = error->max = 0;
    error->data = NULL;
}

void errors_destroy(struct error_s *error) {
    free(error->data);
}
