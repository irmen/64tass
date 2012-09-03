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

static const char *terr_warning[]={
    "Top of memory excedeed",
    "Possibly incorrectly used A",
    "Memory bank excedeed",
    "Possible jmp ($xxff) bug",
    "Long branch used",
    "Directive ignored",
    "Label not on left side",
};

static const char *terr_error[]={
    "Double defined %s",
    "Not defined %s",
    "Extra characters on line",
    "Constant too large",
    "General syntax",
    "%s expected",
    "Expression syntax",
    "Branch too far",
    "Missing argument",
    "Illegal operand",
    "Requirements not met: %s",
    "Conflict: %s",
    "Division by zero",
    "Wrong type %s",
    "Unknown character $%02x",
    "Not allowed here: %s",
    "%s\n",
};
static const char *terr_fatal[]={
    "Can't locate file: %s\n",
    "Error reading file: %s\n",
    "Can't write object file: %s\n",
    "Line too long\n",
    "Can't write listing file: %s\n",
    "Can't write label file: %s\n",
    "File recursion\n",
    "Macro recursion too deep\n",
    "Unknown CPU: %s\n",
    "Unknown option: %s\n",
    "Too many passes\n",
    "Too many errors\n"
};

void err_msg2(enum errors_e no, const char* prm, unsigned int lpoint) {
    char line[linelength];
    unsigned int i;

    if (errors+conderrors==99 && no>=0x40) no=ERROR__TOO_MANY_ERR;

    if (!arguments.warning && no<0x40) {
        warnings++;
        return;
    }

    if (file_list.p) {
        adderror(file_list.data[file_list.p - 1].name);
	sprintf(line,":%" PRIuline ":%u: ", sline, lpoint + 1); adderror(line);
    }

    for (i = file_list.p; i > 1; i--) {
        adderror("(");
        adderror(file_list.data[i - 2].name);
        sprintf(line,":%" PRIuline ") ", file_list.data[i - 1].line);
        adderror(line);
    }

    if (no<0x40) {
        adderror("warning: ");
        warnings++;
        if (no == ERROR_WUSER_DEFINED) adderror2(user_error.data, user_error.p);
        else adderror(terr_warning[no]);
    }
    else if (no<0x80) {
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
        adderror("[**Fatal**] ");
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

void err_msg_wrong_type(enum type_e type, unsigned int epoint) {
    const char *name = NULL;
    switch (type) {
    case T_SINT: name = "<sint>";break;
    case T_UINT: name = "<uint>";break;
    case T_NUM: name = "<num>";break;
    case T_STR: name = "<string>";break;
    case T_UNDEF: err_msg2(ERROR___NOT_DEFINED, "", epoint);return;
    case T_IDENT: name = "<ident>";break;
    case T_IDENTREF: name = "<identref>";break;
    case T_NONE: name = "<none>";break;
    case T_BACKR: name = "<backr>";break;
    case T_FORWR: name = "<forwr>";break;
    case T_OPER: name = "<operator>";break;
    case T_GAP: name = "<uninit>";break;
    case T_LIST: name = "<list>";break;
    case T_FLOAT: name = "<float>";break;
    }
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

void err_msg_variable(struct value_s *val) {
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
    case T_STR: add_user_error2(val->u.str.data, val->u.str.len);break;
    case T_UNDEF: add_user_error("<undefined>");break;
    case T_IDENT: add_user_error("<ident>");break;
    case T_IDENTREF: add_user_error("<identref>");break;
    case T_NONE: add_user_error("<none>");break;
    case T_BACKR: add_user_error("<backr>");break;
    case T_FORWR: add_user_error("<forwr>");break;
    case T_OPER: add_user_error("<operator>");break;
    case T_GAP: add_user_error("<uninit>");break;
    case T_LIST: add_user_error("<list>");break;
    }
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
