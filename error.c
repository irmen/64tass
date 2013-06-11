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

unsigned int errors=0,conderrors=0,warnings=0;

static struct file_list_s file_list;
static struct file_list_s *included_from;
static struct file_list_s *current_file_list;

static struct error_s error_list = {0,0,0,NULL};

static int file_list_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct file_list_s *a = cavltree_container_of(aa, struct file_list_s, node);
    const struct file_list_s *b = cavltree_container_of(bb, struct file_list_s, node);
    if (a->file->uid != b->file->uid) return a->file->uid - b->file->uid;
    if (a->sline != b->sline) return a->sline - b->sline;
    return a->epoint.pos - b->epoint.pos;
}

static void file_list_free(struct avltree_node *aa)
{
    struct file_list_s *a = avltree_container_of(aa, struct file_list_s, node);
    avltree_destroy(&a->members, file_list_free);
    free(a);
}

static struct file_list_s *lastfl = NULL;
struct file_list_s *enterfile(struct file_s *file, line_t line, linepos_t epoint) {
    struct avltree_node *b;
    if (!lastfl)
        if (!(lastfl=malloc(sizeof(struct file_list_s)))) err_msg_out_of_memory();
    lastfl->file = file;
    lastfl->sline = line;
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
        error_list.data = realloc(error_list.data, error_list.max);
        if (!error_list.data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(error_list.data + error_list.len, s, len);
    error_list.len += len;
}

static void adderror(const char *s) {
    adderror2((const uint8_t *)s, strlen(s));
}

static void addorigin(struct file_list_s *cflist, line_t lnum, linepos_t lpoint2) {
    char line[256];
    if (cflist->parent) {
        if (cflist != included_from) {
            included_from = cflist;
            while (included_from->parent->parent) {
                adderror( (included_from == cflist) ? "In file included from " : "                      ");
                adderror(included_from->parent->file->realname);
                sprintf(line,":%" PRIuline ":%" PRIlinepos, included_from->sline, included_from->epoint.pos - included_from->epoint.upos + 1);
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
    sprintf(line,":%" PRIuline ":%" PRIlinepos ": ", lnum, lpoint2->pos - lpoint2->upos + 1); 
    adderror(line);
}

static const char *terr_warning[]={
    "top of memory excedeed",
    "possibly incorrectly used a",
    "memory bank excedeed",
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
    "not hashable"
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
    if (errors+conderrors==99 && no>=0x40) no=ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        if (errors) return;
        warnings++;
        return;
    }

    if (no<0x40) {
        if (errors) return;
        addorigin(current_file_list, sline, lpoint2);
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
        addorigin(current_file_list, sline, lpoint2);
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
        default:
            adderror(terr_error[no & 63]);
        }
    }
    else {
        if (no == ERROR__TOO_MANY_ERR) errors++; else inc_errors();
        addorigin(current_file_list, sline, lpoint2);
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
        } else adderror2(data, len);
    }
    adderror("'");
}

static void err_msg_str_name(const char *msg, const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }
    inc_errors();
    addorigin(current_file_list, sline, epoint);
    adderror(msg);
    if (name) str_name(name->data, name->len);
    adderror("\n");
    return;
}

void err_msg_wrong_type(const struct value_s *val, linepos_t epoint) {
    if (pass == 1) return;
    if (val->obj == ERROR_OBJ) {
        switch (val->u.error.num) {
        case ERROR___NOT_DEFINED: err_msg_not_defined(&val->u.error.u.ident, &val->u.error.epoint);return;
        case ERROR_REQUIREMENTS_: err_msg_requires(&val->u.error.u.ident, &val->u.error.epoint);return;
        case ERROR______CONFLICT: err_msg_conflicts(&val->u.error.u.ident, &val->u.error.epoint);return;
        case ERROR__INVALID_OPER: err_msg_invalid_oper(val->u.error.u.invoper.op, val->u.error.u.invoper.v1, val->u.error.u.invoper.v2, &val->u.error.epoint);return;
        case ERROR___INDEX_RANGE:
        case ERROR_CONSTNT_LARGE:
        case ERROR_NEGFRAC_POWER:
        case ERROR_BIG_STRING_CO:
        case ERROR_____KEY_ERROR:
        case ERROR__NOT_HASHABLE:
        case ERROR_DIVISION_BY_Z: err_msg_str_name(terr_error[val->u.error.num & 63], NULL, &val->u.error.epoint);return;
        }
    }
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
        user_error->data = realloc(user_error->data, user_error->max);
        if (!user_error->data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(user_error->data + user_error->len, s, len);
    user_error->len += len;
    user_error->chars += len;
}

static void add_user_error(struct error_s *user_error, const char *s) {
    add_user_error2(user_error, (const uint8_t *)s, strlen(s));
}

void err_msg_variable(struct error_s *user_error, struct value_s *val, int repr) {
    char buffer[100], buffer2[100];
    uint32_t addrtype;
    int ind;

    if (!val) {user_error->chars = user_error->len = 0;return;}
    switch (val->obj->type) {
    case T_ADDRESS: 
        sprintf(buffer,"$%" PRIxval, val->u.addr.val);
        addrtype = val->u.addr.type;
        ind = 99;
        buffer2[ind] = '\0';
        while (addrtype & 0xfff) {
            switch ((enum atype_e)((addrtype & 0xf00) >> 8)) {
            case A_NONE:break;
            case A_XR: strcat(buffer, ",x");break;
            case A_YR: strcat(buffer, ",y");break;
            case A_ZR: strcat(buffer, ",z");break;
            case A_SR: strcat(buffer, ",s");break;
            case A_RR: strcat(buffer, ",r");break;
            case A_I: buffer2[--ind] = '(';strcat(buffer, ")");break;
            case A_LI: buffer2[--ind] = '[';strcat(buffer, "]");break;
            case A_IMMEDIATE: buffer2[--ind] = '#';break;
            }
            addrtype <<= 4;
        }
        add_user_error(user_error, buffer2 + ind);
        add_user_error(user_error, buffer);
        break;
    case T_SINT: sprintf(buffer,"%+" PRIdval, val->u.num.val); add_user_error(user_error, buffer); break;
    case T_UINT: sprintf(buffer,"%" PRIuval, val->u.num.val); add_user_error(user_error, buffer); break;
    case T_NUM: {
        sprintf(buffer2,"$%%0%d%s", (val->u.num.len + 3) / 4 + !val->u.num.len, PRIxval);
        sprintf(buffer, buffer2, val->u.num.val);
        add_user_error(user_error, buffer); break;
    }
    case T_CODE: sprintf(buffer,"$%" PRIxval, val->u.code.addr); add_user_error(user_error, buffer); break;
    case T_FLOAT:
       {
           int i = 0;
           sprintf(buffer, "%.10g", val->u.real);
           while (buffer[i] && buffer[i]!='.' && buffer[i]!='e' && buffer[i]!='n' && buffer[i]!='i') i++;
           if (!buffer[i]) {buffer[i++]='.';buffer[i++]='0';buffer[i]=0;}
           add_user_error(user_error, buffer);
           break;
       }
    case T_STR:
       {
           if (repr) {
               const char *c;
               const uint8_t *p, *c2;
               c = memchr(val->u.str.data, '"', val->u.str.len) ? "'" : "\"";
               add_user_error(user_error, c);
               p = val->u.str.data;
               while (p < val->u.str.data + val->u.str.len) {
                   c2 = memchr(p, c[0], val->u.str.len + (val->u.str.data - p));
                   if (c2) {
                       add_user_error2(user_error, p, c2 - p + 1);
                       add_user_error(user_error, c);
                       p = c2 + 1;
                   } else {
                       add_user_error2(user_error, p, val->u.str.len + (val->u.str.data - p));
                       p = val->u.str.data + val->u.str.len;
                   }
               }
               add_user_error(user_error, c);
           }
           else add_user_error2(user_error, val->u.str.data, val->u.str.len);
           user_error->chars -= val->u.str.len - val->u.str.chars;
           break;
       }
    case T_BYTES:
        {
            size_t i;
            add_user_error(user_error, "bytes([");
            for (i = 0;i < val->u.bytes.len; i++) {
                char tmp[10];
                sprintf(tmp, i ? ",$%02x" : "$%02x", val->u.bytes.data[i]);
                add_user_error(user_error, tmp);
            }
            add_user_error(user_error, "])");
            break;
        }
    case T_GAP: add_user_error(user_error, "?");break;
    case T_LIST:
        {
            size_t i;
            add_user_error(user_error, "[");
            for (i = 0;i < val->u.list.len; i++) {
                if (i) add_user_error(user_error, ",");
                err_msg_variable(user_error, val->u.list.data[i], 1);
            }
            add_user_error(user_error, "]");
            break;
        }
    case T_TUPLE:
        {
            size_t i;
            add_user_error(user_error, "(");
            for (i = 0;i < val->u.list.len; i++) {
                if (i) add_user_error(user_error, ",");
                err_msg_variable(user_error, val->u.list.data[i], 1);
            }
            if (val->u.list.len == 1) add_user_error(user_error, ",");
            add_user_error(user_error, ")");
            break;
        }
    case T_DICT:
        {
            const struct avltree_node *n;
            const struct pair_s *p;
            int first = 0;
            add_user_error(user_error, "{");
            n = avltree_first(&val->u.dict.members);
            while (n) {
                p = cavltree_container_of(n, struct pair_s, node);
                if (first) add_user_error(user_error, ",");
                err_msg_variable(user_error, p->key, 1);
                add_user_error(user_error, ":");
                err_msg_variable(user_error, p->data, 1);
                first = 1;
                n = avltree_next(n);
            }
            add_user_error(user_error, "}");
            break;
        }
    case T_PAIR:
        err_msg_variable(user_error, val->u.pair.key, 1);
        add_user_error(user_error, ":");
        err_msg_variable(user_error, val->u.pair.data, 1);
        break;
    case T_BOOL: add_user_error(user_error, val->u.num.val ? "1" : "0");break;
    default: add_user_error(user_error, val->obj->name);break;
    }
}

static void err_msg_double_defined2(const char *msg, const struct label_s *l, struct file_list_s *cflist, const str_t *labelname2, line_t line, linepos_t epoint2) {

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }

    inc_errors();
    addorigin(cflist, line, epoint2);
    adderror(msg);
    str_name(labelname2->data, labelname2->len);
    adderror("\n");
    addorigin(l->file_list, l->sline, &l->epoint);
    adderror("note: previous definition of");
    str_name(l->name.data, l->name.len);
    adderror(" was here\n");
}

void err_msg_double_defined(const struct label_s *l, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("error: duplicate definition", l, current_file_list, labelname2, sline, epoint2);
}

void err_msg_shadow_defined(const struct label_s *l, const struct label_s *l2) {
    err_msg_double_defined2("error: shadow definition", l, l2->file_list, &l2->name, l2->sline, &l2->epoint);
}

static int err_oper(const char *msg, const struct value_s *op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    if (pass == 1) return 0;
    if (v1->obj == ERROR_OBJ) {
        err_msg_wrong_type(v1, &v1->u.error.epoint);
        return 0;
    }
    if (v2 && v2->obj == ERROR_OBJ) {
        err_msg_wrong_type(v2, &v2->u.error.epoint);
        return 0;
    }

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return 0;
    }

    addorigin(current_file_list, sline, epoint);

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

void err_msg_strange_oper(const struct value_s *op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    return;
    if (!arguments.warning) {
        warnings++;
        return;
    }
    if (err_oper("warning: possibly wrong", op, v1, v2, epoint)) warnings++;
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
}

void err_destroy(void) {
    avltree_destroy(&file_list.members, file_list_free);
    free(lastfl);
    free(error_list.data);
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
    addorigin(current_file_list, sline, &lpoint);
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
