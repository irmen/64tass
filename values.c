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
#include "values.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "error.h"

static union values_u {
    struct value_s val;
    union values_u *next;
} *values_free = NULL;

static struct values_s {
    union values_u vals[255];
    struct values_s *next;
} *values = NULL;

static void value_free(union values_u *val) {
    //free(val); return;
    val->next = values_free;
    values_free = val;
}

struct value_s *val_alloc(void) {
    struct value_s *val;
    size_t i;
    //return malloc(sizeof(struct value_s));
    val = (struct value_s *)values_free;
    values_free = values_free->next;
    if (!values_free) {
        struct values_s *old = values;
        values = malloc(sizeof(struct values_s));
        if (!values) err_msg_out_of_memory();
        for (i = 0; i < 254; i++) {
            values->vals[i].next = &values->vals[i+1];
        }
        values->vals[i].next = NULL;
        values->next = old;
        values_free = &values->vals[0];
    }
    return val;
}

void val_destroy2(struct value_s *val) {
    switch (val->type) {
    case T_STR: free((uint8_t *)val->u.str.data); break;
    case T_LIST: 
    case T_TUPLE: 
        while (val->u.list.len) val_destroy(val->u.list.data[--val->u.list.len]);
        free(val->u.list.data); break;
    case T_MACRO:
    case T_SEGMENT:
    case T_STRUCT:
    case T_UNION:
        while (val->u.macro.argc) {
            --val->u.macro.argc;
            free((char *)val->u.macro.param[val->u.macro.argc].name.data);
            free((char *)val->u.macro.param[val->u.macro.argc].init.data);
        }
        free(val->u.macro.param);break;
    case T_FUNCTION:
        while (val->u.func.argc) {
            --val->u.func.argc;
            free((char *)val->u.func.param[val->u.func.argc].name.data);
            if (val->u.func.param[val->u.func.argc].init) val_destroy(val->u.func.param[val->u.func.argc].init);
        }
        free(val->u.func.param); break;
    default:
        break;
    }
}

void val_destroy(struct value_s *val) {
    if (!val->refcount) {
        val_destroy2(val);
        return;
    }
    if (val->refcount == 1) {
        val_destroy2(val);
        value_free((union values_u *)val);
    } else val->refcount--;
}

struct value_s *val_realloc(struct value_s **val) {
    if (val[0]->refcount == 1) {
        val_destroy2(*val);
        return *val;
    }
    if (val[0]->refcount) val[0]->refcount--;
    *val = val_alloc();
    val[0]->refcount = 1;
    return *val;
}

static void val_copy2(struct value_s *val, const struct value_s *val2) {
    size_t i;

    *val = *val2;
    val->refcount = 1;
    switch (val2->type) {
    case T_STR: 
        if (val2->u.str.len) {
            uint8_t *s;
            s = malloc(val2->u.str.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s, val2->u.str.data, val2->u.str.len);
            val->u.str.data = s;
        } else val->u.str.data = NULL;
        break;
    case T_LIST:
    case T_TUPLE:
        if (val2->u.list.len) {
            val->u.list.data = malloc(val2->u.list.len * sizeof(val->u.list.data[0]));
            if (!val->u.list.data) err_msg_out_of_memory();
            for (i = 0; i < val2->u.list.len; i++)
                val->u.list.data[i] = val_reference(val2->u.list.data[i]);
            val->u.list.len = i;
        } else val->u.list.data = NULL;
        break;
    case T_MACRO:
    case T_SEGMENT:
    case T_STRUCT:
    case T_UNION:
        if (val2->u.macro.argc) {
            val->u.macro.param = malloc(val2->u.macro.argc * sizeof(val->u.macro.param[0]));
            if (!val->u.macro.param) err_msg_out_of_memory();
            for (i = 0; i < val2->u.macro.argc; i++) {
                str_cpy(&val->u.macro.param[i].name, &val2->u.macro.param[i].name);
                str_cpy(&val->u.macro.param[i].init, &val2->u.macro.param[i].init);
            }
            val->u.macro.argc = i;
        } else val->u.macro.param = NULL;
        break;
    case T_FUNCTION:
        if (val2->u.func.argc) {
            val->u.func.param = malloc(val2->u.func.argc * sizeof(val->u.func.param[0]));
            if (!val->u.func.param) err_msg_out_of_memory();
            for (i = 0; i < val2->u.func.argc; i++) {
                str_cpy(&val->u.func.param[i].name, &val2->u.func.param[i].name);
                if (val2->u.func.param[i].init) {
                    val->u.func.param[i].init = val_reference(val2->u.func.param[i].init);
                } else val->u.func.param[i].init = NULL;
                val->u.func.param[i].epoint = val2->u.func.param[i].epoint;
            }
            val->u.func.argc = i;
        } else val->u.func.param = NULL;
        break;
    default:
        break;
    }
}

static struct value_s *val_copy(const struct value_s *val2) {
    struct value_s *val = val_alloc();
    if (!val) err_msg_out_of_memory();
    val_copy2(val, val2);
    return val;
}

void val_replace(struct value_s **val, struct value_s *val2) {
    if (*val == val2) return;
    if (val[0]->refcount == 1 && val2->refcount == 0) {
        val_destroy2(*val);
        val_copy2(*val, val2);
        return;
    }
    val_destroy(*val);
    *val = val_reference(val2);
}

void val_replace_template(struct value_s **val, const struct value_s *val2) {
    if (val[0]->refcount == 1) {
        val_destroy2(*val);
    } else { 
        val_destroy(*val);
        *val = val_alloc();
        if (!*val) err_msg_out_of_memory();
    }
    **val = *val2;
    val[0]->refcount = 1;
}

void val_set_template(struct value_s **val, const struct value_s *val2) {
    *val = val_alloc();
    if (!*val) err_msg_out_of_memory();
    **val = *val2;
    val[0]->refcount = 1;
}

struct value_s *val_reference(struct value_s *val2) {
    if (val2->refcount) {val2->refcount++;return val2;}
    return val_copy(val2);
}

int val_same(const struct value_s *val, const struct value_s *val2) {
    size_t i;

    switch (val->type) {
    case T_SINT:
    case T_UINT:
    case T_BOOL:
        return val2->type == val->type && val->u.num.val == val2->u.num.val;
    case T_CODE:
        return val2->type == val->type && val->u.code.addr == val2->u.code.addr && val->u.code.size == val2->u.code.size && val->u.code.dtype == val2->u.code.dtype;
    case T_LBL:
        return val2->type == val->type && val->u.lbl.p == val2->u.lbl.p && val->u.lbl.sline == val2->u.lbl.sline && val->u.lbl.waitforp == val2->u.lbl.waitforp && val->u.lbl.file == val2->u.lbl.file && val->u.lbl.parent == val2->u.lbl.parent;
    case T_MACRO:
    case T_SEGMENT:
    case T_STRUCT:
    case T_UNION:
        if (val2->type != val->type || val->u.macro.p != val2->u.macro.p || val->u.macro.file != val2->u.macro.file || val->u.macro.sline != val2->u.macro.sline || val->u.macro.size != val2->u.macro.size) return 0;
        for (i = 0; i < val->u.macro.argc; i++) {
            if (str_cmp(&val->u.macro.param[i].name, &val2->u.macro.param[i].name)) return 0;
            if (str_cmp(&val->u.macro.param[i].init, &val2->u.macro.param[i].init)) return 0;
        }
        return 1;
    case T_FUNCTION:
        if (val2->type != val->type || val->u.func.p != val2->u.func.p || val->u.func.file != val2->u.func.file || val->u.func.sline != val2->u.func.sline || val->u.func.context != val2->u.func.context) return 0;
        for (i = 0; i < val->u.func.argc; i++) {
            if (str_cmp(&val->u.func.param[i].name, &val2->u.func.param[i].name)) return 0;
            if (val->u.func.param[i].init != val2->u.func.param[i].init && (!val->u.func.param[i].init || !val2->u.func.param[i].init || !val_same(val->u.func.param[i].init, val2->u.func.param[i].init))) return 0;
            if (val->u.func.param[i].epoint.pos != val2->u.func.param[i].epoint.pos) return 0;
            if (val->u.func.param[i].epoint.upos != val2->u.func.param[i].epoint.upos) return 0;
        }
        return 1;
    case T_NUM:
        return val2->type == val->type && val->u.num.len == val2->u.num.len && val->u.num.val == val2->u.num.val;
    case T_FLOAT:
        return val2->type == val->type && val->u.real == val2->u.real;
    case T_STR: 
        return val2->type == val->type && val->u.str.len == val2->u.str.len && (
                    val->u.str.data == val2->u.str.data ||
                !memcmp(val->u.str.data, val2->u.str.data, val2->u.str.len));
    case T_LIST:
    case T_TUPLE:
        if (val2->type == val->type) {
            if (val->u.list.len != val2->u.list.len) return 0;
            for (i = 0; i < val->u.list.len; i++) 
                if (!val_same(val->u.list.data[i], val2->u.list.data[i])) return 0;
            return 1;
        }
        break;
    case T_NONE:
    case T_GAP:
        return val->type == val2->type;
    case T_IDENTREF:
        return val->type == val2->type && val->u.ident.label == val2->u.ident.label;
    default: /* not possible here */
        exit(2);
    }
    return 0;
}

int val_truth(const struct value_s *val) {
    switch (val->type) {
    case T_SINT:
    case T_UINT:
    case T_NUM:
    case T_BOOL:
        return !!val->u.num.val;
    case T_CODE:
        return !!val->u.code.addr;
    case T_FLOAT:
        return !!val->u.real;
    case T_STR: 
        return !!val->u.str.len;
    case T_LIST:
    case T_TUPLE:
        return !!val->u.list.len;
    default:
        return 0;
    }
}

void val_print(const struct value_s *value, FILE *flab) {
    switch (value->type) {
    case T_NUM:
        fprintf(flab,"$%" PRIxval, (uval_t)value->u.num.val);
        break;
    case T_CODE:
        fprintf(flab,"$%" PRIxval, (uval_t)value->u.code.addr);
        break;
    case T_UINT:
        fprintf(flab,"%" PRIuval, (uval_t)value->u.num.val);
        break;
    case T_SINT:
        fprintf(flab,"%+" PRIdval, (ival_t)value->u.num.val);
        break;
    case T_FLOAT:
        {
            char num[100];
            int i = 0;
            sprintf(num, "%.10g", value->u.real);
            while (num[i] && num[i]!='.' && num[i]!='e' && num[i]!='n' && num[i]!='i') i++;
            if (!num[i]) {num[i++]='.';num[i++]='0';num[i]=0;}
            fputs(num, flab);
        }
        break;
    case T_STR:
        {
            size_t val;
            uint32_t ch;
            uint8_t c;
            c = memchr(value->u.str.data, '"', value->u.str.len) ? '\'' : '"';
            fputc(c, flab);
            for (val = 0;val < value->u.str.len;) {
                ch = value->u.str.data[val];
                if (ch & 0x80) val += utf8in(value->u.str.data + val, &ch); else val++;
                if (ch == c) fputc(c, flab);
                if (ch < 32 || ch > 127) fprintf(flab,"{$%02x}", ch);
                else fputc(ch, flab);
            }
            fputc(c, flab);
            break;
        }
    case T_LIST:
        {
            size_t val;
            int first = 0;
            fputc('[', flab);
            for (val = 0;val < value->u.list.len; val++) {
                if (first) fputc(',', flab);
                val_print(value->u.list.data[val], flab);
                first = 1;
            }
            fputc(']', flab);
            break;
        }
    case T_TUPLE:
        {
            size_t val;
            int first = 0;
            fputc('(', flab);
            for (val = 0;val < value->u.list.len; val++) {
                if (first) fputc(',', flab);
                val_print(value->u.list.data[val], flab);
                first = 1;
            }
            if (value->u.list.len == 1) fputc(',', flab);
            fputc(')', flab);
            break;
        }
    case T_BOOL:
        putc(value->u.num.val ? '1' : '0', flab);
        break;
    case T_IDENTREF:
        if (value->u.ident.label->parent != &root_label) {
            int rec = 100;
            while (value->type == T_IDENTREF) {
                value = value->u.ident.label->value;
                if (!rec--) {
                    putc('!', flab);
                    return;
                }
            }
            val_print(value, flab);
        } else {
            fwrite(value->u.ident.label->name.data, value->u.ident.label->name.len, 1, flab);
        }
        break;
    case T_GAP:
        putc('?', flab);
        break;
    default:
        putc('!', flab);
        break;
    }
}

void init_values(void)
{
    size_t i;
    values = malloc(sizeof(struct values_s));
    if (!values) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        values->vals[i].next = &values->vals[i+1];
    }
    values->vals[i].next = NULL;
    values->next = NULL;

    values_free = &values->vals[0];
}

void destroy_values(void)
{
    struct values_s *old;
    while (values) {
        old = values;
        values = values->next;
        free(old);
    }
}
