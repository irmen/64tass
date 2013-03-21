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

static struct value_s *value_alloc(void) {
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

static void val_destroy2(struct value_s *val) {
    switch (val->type) {
    case T_STR: free(val->u.str.data); break;
    case T_LIST: 
    case T_TUPLE: 
        while (val->u.list.len) val_destroy(val->u.list.data[--val->u.list.len]);
        free(val->u.list.data);
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


static void val_copy2(struct value_s *val, const struct value_s *val2) {
    *val = *val2;
    val->refcount = 1;
    switch (val2->type) {
    case T_STR: 
        if (val2->u.str.len) {
            val->u.str.data = malloc(val2->u.str.len);
            if (!val->u.str.data) err_msg_out_of_memory();
            memcpy(val->u.str.data, val2->u.str.data, val2->u.str.len);
        } else val->u.str.data = NULL;
        break;
    case T_LIST:
    case T_TUPLE:
        if (val2->u.list.len) {
            val->u.list.data = malloc(val2->u.list.len * sizeof(val->u.list.data[0]));
            if (!val->u.list.data) err_msg_out_of_memory();
            for (val->u.list.len = 0; val->u.list.len < val2->u.list.len; val->u.list.len++)
                val->u.list.data[val->u.list.len] = val_reference(val2->u.list.data[val->u.list.len]);
        } else val->u.list.data = NULL;
    default:
        break;
    }
}

static struct value_s *val_copy(const struct value_s *val2) {
    struct value_s *val = value_alloc();
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

struct value_s *val_reference(struct value_s *val2) {
    if (val2->refcount) {val2->refcount++;return val2;}
    return val_copy(val2);
}

int val_equal(const struct value_s *val, const struct value_s *val2) {
    size_t i;

    switch (val->type) {
    case T_SINT:
    case T_UINT:
    case T_BOOL:
        return val2->type == val->type && val->u.num.val == val2->u.num.val;
    case T_CODE:
        return val2->type == val->type && val->u.code.addr == val2->u.code.addr && val->u.code.size == val2->u.code.size && val->u.code.esize == val2->u.code.esize && val->u.code.sign == val2->u.code.sign;
    case T_MACRO:
    case T_SEGMENT:
    case T_UNION:
    case T_STRUCT:
        return val2->type == val->type && val->u.macro.p == val2->u.macro.p && val->u.macro.file == val2->u.macro.file && val->u.macro.sline == val2->u.macro.sline && val->u.macro.size == val2->u.macro.size;
    case T_NUM:
        return val2->type == val->type && val->u.num.len == val2->u.num.len && val->u.num.val == val2->u.num.val;
    case T_FLOAT:
        return val2->type == T_FLOAT && val->u.real == val2->u.real;
    case T_STR: 
        return val2->type == T_STR && val->u.str.len == val2->u.str.len && (
                    val->u.str.data == val2->u.str.data ||
                !memcmp(val->u.str.data, val2->u.str.data, val2->u.str.len));
    case T_LIST:
    case T_TUPLE:
        if (val2->type == val->type) {
            if (val->u.list.len != val2->u.list.len) return 0;
            for (i = 0; i < val->u.list.len; i++) 
                if (!val_equal(val->u.list.data[i], val2->u.list.data[i])) return 0;
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
            fputs(value->u.ident.label->origname, flab);
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
