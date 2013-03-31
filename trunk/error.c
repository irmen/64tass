/*

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
#include "error.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "misc.h"
#include "values.h"

#if _BSD_SOURCE || _XOPEN_SOURCE >= 500 || _ISOC99_SOURCE || _POSIX_C_SOURCE >= 200112L
#else
#define snprintf(str, size, format, var) sprintf(str, format, var)
#endif

unsigned int errors=0,conderrors=0,warnings=0;

static struct {
    size_t p;
    size_t len;
    struct {
        line_t line;
        const char *name;
    } *data;
} file_list = {0,0,NULL};

static struct error_s error_list = {0,0,NULL};

void enterfile(const char *name, line_t line) {

    if (file_list.p >= file_list.len) {
        file_list.len += 16;
        file_list.data = realloc(file_list.data, file_list.len * sizeof(*file_list.data));
        if (!file_list.data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    file_list.data[file_list.p].name=name;
    file_list.data[file_list.p].line=line;
    file_list.p++;
}

void exitfile(void) {
    if (file_list.p) file_list.p--;
}

static void adderror2(const uint8_t *s, size_t len) {
    if (len + error_list.p > error_list.len) {
        error_list.len += (len > 0x200) ? len : 0x200;
        error_list.data = realloc(error_list.data, error_list.len);
        if (!error_list.data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(error_list.data + error_list.p, s, len);
    error_list.p += len;
}

static void adderror(const char *s) {
    adderror2((const uint8_t *)s, strlen(s));
}

static void addorigin(linepos_t lpoint2) {
    char line[linelength];
    size_t i;

    if (file_list.p) {
        adderror(file_list.data[file_list.p - 1].name);
	sprintf(line,":%" PRIuline ":%" PRIlinepos ": ", sline, lpoint2.pos - lpoint2.upos + 1); adderror(line);
    } else {
        adderror("<command line>:0:0: ");
    }

    for (i = file_list.p; i > 1; i--) {
        adderror("(");
        adderror(file_list.data[i - 2].name);
        sprintf(line,":%" PRIuline ") ", file_list.data[i - 1].line);
        adderror(line);
    }
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
    "double defined %s",
    "extra characters on line",
    "constant too large",
    "general syntax",
    "%s expected",
    "expression syntax",
    "missing argument",
    "illegal operand",
    "division by zero",
    "wrong type %s",
    "not allowed here: %s",
    "instruction can't cross banks",
    "address out of section",
    "negative number raised on fractional power",
    "string constant too long for a number",
    "index out of range",
    "%s\n",
};
static const char *terr_fatal[]={
    "can't open file ",
    "error reading file ",
    "can't write object file ",
    "line too long\n",
    "can't write listing file ",
    "can't write label file ",
    "file recursion\n",
    "macro recursion too deep\n",
    "reference recursion too deep\n",
    "unknown cpu: %s\n",
    "unknown option: %s\n",
    "too many passes\n",
    "too many errors\n"
};

void err_msg2(enum errors_e no, const void* prm, linepos_t lpoint2) {
    char line[linelength];

    if (pass == 1 && no < 0x80) return;
    if (errors+conderrors==99 && no>=0x40) no=ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        warnings++;
        return;
    }

    addorigin(lpoint2);

    if (no<0x40) {
        adderror("warning: ");
        warnings++;
        if (no == ERROR_WUSER_DEFINED) adderror2(((struct error_s *)prm)->data, ((struct error_s *)prm)->p);
        else adderror(terr_warning[no]);
    }
    else if (no<0x80) {
        adderror("error: ");
        switch (no) {
        case ERROR____PAGE_ERROR:
            adderror("page error at $");
            sprintf(line,"%06" PRIaddress, *(const address_t *)prm); adderror(line);
            conderrors++; break;
        case ERROR_BRANCH_TOOFAR:
            sprintf(line,"branch too far by %+d bytes", *(const int *)prm); adderror(line);
            conderrors++; break;
        case ERROR__BRANCH_CROSS:
            adderror("Branch crosses page");
            conderrors++; break;
        case ERROR__USER_DEFINED:
            adderror2(((struct error_s *)prm)->data, ((struct error_s *)prm)->p);
            conderrors++; break;
        case ERROR___UNKNOWN_CHR:
            sprintf(line,"can't encode character $%02x", *(const uint32_t *)prm); adderror(line);
            conderrors++; break;
        default:
                snprintf(line,linelength,terr_error[no & 63], (const char *)prm);
                switch (no) {
                case ERROR_CANT_CROSS_BA:
                case ERROR_OUTOF_SECTION:
                case ERROR_CONSTNT_LARGE: conderrors++;break;
                default: errors++;
                }
                adderror(line);
        }
    }
    else {
        adderror("fatal error: ");
        snprintf(line, linelength, terr_fatal[no & 63], (const char *)prm);
        adderror(line);
        errors++;
        status();exit(1);
    }
    adderror("\n");
}

void err_msg(enum errors_e no, const void* prm) {
    err_msg2(no, prm, lpoint);
}

static const char *type_name(enum type_e t) {
    switch (t) {
    case T_SINT: return "<sint>";
    case T_UINT: return "<uint>";
    case T_NUM: return "<num>";
    case T_STR: return "<string>";
    case T_ERROR: return "<error>";
    case T_IDENT: return "<ident>";
    case T_IDENTREF: return "<identref>";
    case T_NONE: return "<none>";
    case T_BACKR: return "<backr>";
    case T_FORWR: return "<forwr>";
    case T_OPER: return "<operator>";
    case T_GAP: return "<uninit>";
    case T_LIST: return "<list>";
    case T_TUPLE: return "<tuple>";
    case T_FLOAT: return "<float>";
    case T_BOOL: return "<bool>";
    case T_MACRO: return "<macro>";
    case T_SEGMENT: return "<segment>";
    case T_STRUCT: return "<struct>";
    case T_UNION: return "<union>";
    case T_FUNCTION: return "<function>";
    case T_CODE: return "<code>";
    case T_LBL: return "<lbl>";
    case T_DEFAULT: return "<default>";
    }
    return NULL;
}

static void err_msg_str_name(const char *msg, const str_t *name, linepos_t epoint) {
    if (pass == 1) return;
    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }
    addorigin(epoint);
    adderror(msg);
    if (name) {
        adderror(" '");
        adderror2(name->data, name->len);
        adderror("'\n");
    } else adderror("\n");
    errors++;
    return;
}

void err_msg_wrong_type(const struct value_s *val, linepos_t epoint) {
    const char *name;
    if (pass == 1) return;
    if (val->type == T_ERROR) {
        switch (val->u.error.num) {
        case ERROR___NOT_DEFINED: err_msg_not_defined(&val->u.error.u.ident, val->u.error.epoint);return;
        case ERROR_REQUIREMENTS_: err_msg_requires(&val->u.error.u.ident, val->u.error.epoint);return;
        case ERROR______CONFLICT: err_msg_conflicts(&val->u.error.u.ident, val->u.error.epoint);return;
        case ERROR___INDEX_RANGE:
        case ERROR_CONSTNT_LARGE:
        case ERROR_NEGFRAC_POWER:
        case ERROR_BIG_STRING_CO:
        case ERROR_DIVISION_BY_Z: err_msg_str_name(terr_error[val->u.error.num & 63], NULL, val->u.error.epoint);return;
        }
    }
    name = type_name(val->type);
    err_msg2(ERROR____WRONG_TYPE, name, epoint);
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
    if (len + user_error->p > user_error->len) {
        user_error->len += (len > 0x100) ? len : 0x100;
        user_error->data = realloc(user_error->data, user_error->len);
        if (!user_error->data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(user_error->data + user_error->p, s, len);
    user_error->p += len;
}

static void add_user_error(struct error_s *user_error, const char *s) {
    add_user_error2(user_error, (const uint8_t *)s, strlen(s));
}

void err_msg_variable(struct error_s *user_error, struct value_s *val, int repr) {
    char buffer[100], buffer2[100];

    if (!val) {user_error->p=0;return;}
    switch (val->type) {
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
           break;
       }
    case T_ERROR: add_user_error(user_error, "<error>");break;
    case T_IDENT: add_user_error(user_error, "<ident>");break;
    case T_IDENTREF: add_user_error(user_error, "<identref>");break;
    case T_NONE: add_user_error(user_error, "<none>");break;
    case T_BACKR: add_user_error(user_error, "<backr>");break;
    case T_FORWR: add_user_error(user_error, "<forwr>");break;
    case T_OPER: add_user_error(user_error, "<operator>");break;
    case T_MACRO: add_user_error(user_error, "<macro>");break;
    case T_SEGMENT: add_user_error(user_error, "<segment>");break;
    case T_STRUCT: add_user_error(user_error, "<struct>");break;
    case T_UNION: add_user_error(user_error, "<union>");break;
    case T_FUNCTION: add_user_error(user_error, "<function>");break;
    case T_LBL: add_user_error(user_error, "<lbl>");break;
    case T_DEFAULT: add_user_error(user_error, "<default>");break;
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
    case T_BOOL: add_user_error(user_error, val->u.num.val ? "<true>" : "<false>");break;
    }
}

static void err_msg_double_defined2(const char *msg, const str_t *name, const char *file, line_t sline2, linepos_t epoint, const str_t *labelname2, linepos_t epoint2) {
    char line[linelength];

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }

    addorigin(epoint2);
    adderror(msg);
    adderror2(labelname2->data, labelname2->len);
    adderror("'\n");
    if (file[0]) {
        adderror(file);
	sprintf(line,":%" PRIuline ":%" PRIlinepos ": ", sline2, epoint.pos - epoint.upos + 1); adderror(line);
    } else {
        adderror("<command line>:0:0: ");
    }
    adderror("note: previous definition of '");
    adderror2(name->data, name->len);
    adderror("' was here\n");
    errors++;
}

void err_msg_double_defined(const str_t *name, const char *file, line_t sline2, linepos_t epoint, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("error: duplicate definition '", name, file, sline2, epoint, labelname2, epoint2);
}

void err_msg_shadow_defined(const str_t *name, const char *file, line_t sline2, linepos_t epoint, const str_t *labelname2, linepos_t epoint2) {
    err_msg_double_defined2("error: shadowing another definition '", name, file, sline2, epoint, labelname2, epoint2);
}

static int err_oper(const char *msg, enum oper_e op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    const char *name;

    if (pass == 1) return 0;
    if (v1->type == T_ERROR) {
        err_msg_wrong_type(v1, v1->u.error.epoint);
        return 0;
    }
    if (v2 && v2->type == T_ERROR) {
        err_msg_wrong_type(v2, v2->u.error.epoint);
        return 0;
    }

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return 0;
    }

    addorigin(epoint);

    adderror(msg);
    if (v2) {
        adderror(" operands to ");
    } else {
        adderror(" type argument to ");
    }
    switch (op) {
    case O_SEPARATOR: name = "',";break;
    case O_FUNC:    name = "function call '()";break;
    case O_INDEX:   name = "indexing '[]";break;
    case O_SLICE: 
    case O_SLICE2:  name = "slicing '[:]";break;
    case O_BRACKET: name = "'[";break;
    case O_PARENT:  name = "'(";break;
    case O_COND:    name = "condition '?";break;
    case O_COLON:
    case O_COLON2:
    case O_COLON3:  name = "':";break;
    case O_COMMA:   name = "',";break;
    case O_WORD:    name = "word '<>";break;
    case O_HWORD:   name = "high word '>`";break;
    case O_BSWORD:  name = "swapped word '><";break;
    case O_LOWER:   name = "low byte '<";break;
    case O_HIGHER:  name = "high byte '>";break;
    case O_BANK:    name = "bank byte '`";break;
    case O_STRING:  name = "string '^";break;
    case O_LOR:     name = "logical or '||";break;
    case O_LXOR:    name = "logical xor '^^";break;
    case O_LAND:    name = "logical and '&&";break;
    case O_IN:      name = "membership 'in";break;
    case O_EQ:      name = "equal '==";break;
    case O_NEQ:     name = "not equal '!=";break;
    case O_LT:      name = "less than '<";break;
    case O_GT:      name = "greater than '>";break;
    case O_GE:      name = "greater than or equal '>=";break;
    case O_LE:      name = "less than or equal '<=";break;
    case O_OR:      name = "binary or '|";break;
    case O_XOR:     name = "binary exclusive or '^";break;
    case O_AND:     name = "binary and '&";break;
    case O_LSHIFT:  name = "binary left shift '<<";break;
    case O_ASHIFT:  name = "arithmetic right shift '>>";break;
    case O_RSHIFT:  name = "binary right shift '>>>";break;
    case O_ADD:     name = "add '+";break;
    case O_SUB:     name = "substract '-";break;
    case O_MUL:     name = "multiply '*";break;
    case O_DIV:     name = "division '/";break;
    case O_MOD:     name = "modulo '%";break;
    case O_EXP:     name = "exponent '**";break;
    case O_X:       name = "repeat 'x";break;
    case O_CONCAT:  name = "concat '..";break;
    case O_MEMBER:  name = "member '.";break;
    case O_NEG:     name = "unary negative '-";break;
    case O_POS:     name = "unary positive '+";break;
    case O_INV:     name = "binary invert '~";break;
    case O_LNOT:    name = "logical not '!";break;
    case O_TUPLE: 
    case O_RPARENT: name = "')";break;
    case O_LIST: 
    case O_RBRACKET:name = "']";break;
    case O_QUEST:   name = "'?";break;
    default: name = "";break;
    }
    adderror(name);

    if (v2) {
        adderror("' '");
        adderror(type_name(v1->type));
        adderror("' and '");
        adderror(type_name(v2->type));
    } else {
        adderror("' '");
        adderror(type_name(v1->type));
    }
    adderror("'\n");
    return 1;
}

void err_msg_invalid_oper(enum oper_e op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    if (err_oper("error: invalid", op, v1, v2, epoint)) errors++;
}

void err_msg_strange_oper(enum oper_e op, const struct value_s *v1, const struct value_s *v2, linepos_t epoint) {
    return;
    if (!arguments.warning) {
        warnings++;
        return;
    }
    if (err_oper("warning: possibly wrong", op, v1, v2, epoint)) warnings++;
}

void freeerrorlist(int print) {
    if (print) {
        fwrite(error_list.data, error_list.p, 1, stderr);
    }
    error_list.p = 0;
}

void err_destroy(void) {
    free(file_list.data);
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
    addorigin(lpoint);
    adderror("fatal error: ");
    adderror(terr_fatal[no & 63]);
    adderror(prm);
    adderror(": ");
    error = strerror(errno);
    adderror(error);
    adderror("\n");
    errors++;
    status();exit(1);
}

void error_init(struct error_s *error) {
    error->p = error->len = 0;
    error->data = NULL;
}

void error_destroy(struct error_s *error) {
    free(error->data);
}
