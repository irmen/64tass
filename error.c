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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "misc.h"

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

struct error_s {
    size_t p;
    size_t len;
    uint8_t *data;
};

static struct error_s error_list = {0,0,NULL};
static struct error_s user_error = {0,0,NULL};

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
    adderror2((uint8_t *)s, strlen(s));
}

static void addorigin(unsigned int lpoint) {
    char line[linelength];
    size_t i;

    if (file_list.p) {
        adderror(file_list.data[file_list.p - 1].name);
	sprintf(line,":%" PRIuline ":%u: ", sline, lpoint + 1); adderror(line);
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
    "not defined %s",
    "extra characters on line",
    "constant too large",
    "general syntax",
    "%s expected",
    "expression syntax",
    "branch too far",
    "missing argument",
    "illegal operand",
    "requirements not met: %s",
    "conflict: %s",
    "division by zero",
    "wrong type %s",
    "unknown character $%02x",
    "not allowed here: %s",
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
    "unknown cpu: %s\n",
    "unknown option: %s\n",
    "too many passes\n",
    "too many errors\n"
};

void err_msg2(enum errors_e no, const char* prm, unsigned int lpoint) {
    char line[linelength];

    if (errors+conderrors==99 && no>=0x40) no=ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        warnings++;
        return;
    }

    addorigin(lpoint);

    if (no<0x40) {
        adderror("warning: ");
        warnings++;
        if (no == ERROR_WUSER_DEFINED) adderror2(user_error.data, user_error.p);
        else adderror(terr_warning[no]);
    }
    else if (no<0x80) {
        adderror("error: ");
        if (no==ERROR____PAGE_ERROR) {
            adderror("Page error at $");
            sprintf(line,"%06" PRIaddress,(address_t)prm); adderror(line);
            conderrors++;
        }
        else if (no==ERROR__BRANCH_CROSS) {
            adderror("Branch crosses page");
            conderrors++;
        }
        else {
            if (no==ERROR__USER_DEFINED) {
                adderror2(user_error.data, user_error.p);
                conderrors++;
            } else {
                snprintf(line,linelength,terr_error[no & 63],prm);
                if (no==ERROR_BRANCH_TOOFAR || no==ERROR_CONSTNT_LARGE) conderrors++;
                else errors++;
                adderror(line);
            }
        }
    }
    else {
        adderror("fatal error: ");
        snprintf(line,linelength,terr_fatal[no & 63],prm);
        adderror(line);
        errors++;
        status();exit(1);
    }
    adderror("\n");
}

void err_msg(enum errors_e no, const char* prm) {
    err_msg2(no,prm, lpoint);
}

static char *type_name(enum type_e t) {
    switch (t) {
    case T_SINT: return "<sint>";
    case T_UINT: return "<uint>";
    case T_NUM: return "<num>";
    case T_STR: return "<string>";
    case T_UNDEF: return "<undefined>";
    case T_IDENT: return "<ident>";
    case T_IDENTREF: return "<identref>";
    case T_NONE: return "<none>";
    case T_BACKR: return "<backr>";
    case T_FORWR: return "<forwr>";
    case T_OPER: return "<operator>";
    case T_GAP: return "<uninit>";
    case T_LIST: return "<list>";
    case T_TUPPLE: return "<tupple>";
    case T_FLOAT: return "<float>";
    case T_BOOL: return "<bool>";
    }
    return NULL;
}

void err_msg_wrong_type(const struct value_s *val, unsigned int epoint) {
    const char *name = NULL;
    if (val->type == T_UNDEF) {
        err_msg2(ERROR___NOT_DEFINED, "", epoint);return;
    }
    name = type_name(val->type);
    err_msg2(ERROR____WRONG_TYPE, name, epoint);
}

static void add_user_error2(const uint8_t *s, size_t len) {
    if (len + user_error.p > user_error.len) {
        user_error.len += (len > 0x100) ? len : 0x100;
        user_error.data = realloc(user_error.data, user_error.len);
        if (!user_error.data) {fputs("Out of memory\n", stderr);exit(1);}
    }
    memcpy(user_error.data + user_error.p, s, len);
    user_error.p += len;
}

static void add_user_error(const char *s) {
    add_user_error2((uint8_t *)s, strlen(s));
}

void err_msg_variable(struct value_s *val, int repr) {
    char buffer[100];

    if (!val) {user_error.p=0;return;}
    switch (val->type) {
    case T_SINT: sprintf(buffer,"%+" PRIdval, val->u.num.val); add_user_error(buffer); break;
    case T_UINT: sprintf(buffer,"%" PRIuval, val->u.num.val); add_user_error(buffer); break;
    case T_NUM: {
        if (val->u.num.val<0x100) sprintf(buffer,"$%02" PRIxval, val->u.num.val);
        else if (val->u.num.val<0x10000) sprintf(buffer,"$%04" PRIxval, val->u.num.val);
        else if (val->u.num.val<0x1000000) sprintf(buffer,"$%06" PRIxval, val->u.num.val);
        else sprintf(buffer,"$%08" PRIxval, val->u.num.val);
        add_user_error(buffer); break;
    }
    case T_FLOAT:
       {
           int i = 0;
           sprintf(buffer, "%.10g", val->u.real);
           while (buffer[i] && buffer[i]!='.' && buffer[i]!='e' && buffer[i]!='n' && buffer[i]!='i') i++;
           if (!buffer[i]) {buffer[i++]='.';buffer[i++]='0';buffer[i]=0;}
           add_user_error(buffer);
           break;
       }
    case T_STR:
       {
           if (repr) {
               char *c;
               uint8_t *p, *c2;
               c = memchr(val->u.str.data, '"', val->u.str.len) ? "'" : "\"";
               add_user_error(c);
               p = val->u.str.data;
               while (p < val->u.str.data + val->u.str.len) {
                   c2 = memchr(p, c[0], val->u.str.len + (val->u.str.data - p));
                   if (c2) {
                       add_user_error2(p, c2 - p + 1);
                       add_user_error(c);
                       p = c2 + 1;
                   } else {
                       add_user_error2(p, val->u.str.len + (val->u.str.data - p));
                       p = val->u.str.data + val->u.str.len;
                   }
               }
               add_user_error(c);
           }
           else add_user_error2(val->u.str.data, val->u.str.len);
           break;
       }
    case T_UNDEF: add_user_error("<undefined>");break;
    case T_IDENT: add_user_error("<ident>");break;
    case T_IDENTREF: add_user_error("<identref>");break;
    case T_NONE: add_user_error("<none>");break;
    case T_BACKR: add_user_error("<backr>");break;
    case T_FORWR: add_user_error("<forwr>");break;
    case T_OPER: add_user_error("<operator>");break;
    case T_GAP: add_user_error("?");break;
    case T_LIST:
        {
            size_t i;
            add_user_error("[");
            for (i = 0;i < val->u.list.len; i++) {
                if (i) add_user_error(",");
                err_msg_variable(val->u.list.data[i], 1);
            }
            add_user_error("]");
            break;
        }
    case T_TUPPLE:
        {
            size_t i;
            add_user_error("(");
            for (i = 0;i < val->u.list.len; i++) {
                if (i) add_user_error(",");
                err_msg_variable(val->u.list.data[i], 1);
            }
            if (val->u.list.len == 1) add_user_error(",");
            add_user_error(")");
            break;
        }
    case T_BOOL: add_user_error(val->u.num.val ? "<true>" : "<false>");break;
    }
}

void err_msg_double_defined(const char *origname, const char *file, line_t sline, unsigned int epoint, const char *labelname2, unsigned int epoint2) {
    char line[linelength];

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }

    addorigin(epoint2);
    adderror("error: duplicate definition '");
    adderror(labelname2);
    adderror("'\n");
    if (file[0]) {
        adderror(file);
	sprintf(line,":%" PRIuline ":%u: ", sline, epoint + 1); adderror(line);
    } else {
        adderror("<command line>:0:0: ");
    }
    adderror("note: previous definition of '");
    adderror(origname);
    adderror("' was here\n");
    errors++;
}

void err_msg_invalid_oper(enum oper_e op, const struct value_s *v1, const struct value_s *v2, unsigned int epoint) {
    char *name;

    if (v1->type == T_UNDEF || (v2 && v2->type == T_UNDEF)) {
        err_msg2(ERROR___NOT_DEFINED, "", epoint);return;
    }

    if (errors+conderrors==99) {
        err_msg(ERROR__TOO_MANY_ERR, NULL);
        return;
    }

    addorigin(epoint);
    if (v2) {
        adderror("error: invalid operands to ");
    } else {
        adderror("error: invalid type argument to ");
    }
    switch (op) {
    case O_FUNC:    name = "function call '(";break;
    case O_INDEX:   name = "indexing '[";break;
    case O_SLICE:   name = "slicing '[";break;
    case O_BRACKET: name = "'[";break;
    case O_PARENT:  name = "'(";break;
    case O_COND:    name = "condition '?";break;
    case O_COLON:   name = "':";break;
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
    case O_EQ:      name = "equal '=";break;
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
    case O_MEMBER:  name = "member '.";break;
    case O_NEG:     name = "unary negative '-";break;
    case O_POS:     name = "unary positive '+";break;
    case O_INV:     name = "binary invert '~";break;
    case O_LNOT:    name = "logical not '!";break;
    case O_TUPPLE:  name = "')";break;
    case O_RPARENT: name = "')";break;
    case O_RBRACKET:name = "']";break;
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
    errors++;
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
    free(user_error.data);
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
