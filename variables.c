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
#include "unicode.h"
#include "variables.h"
#include "misc.h"
#include "values.h"
#include "64tass.h"
#include "file.h"
#include "obj.h"
#include "boolobj.h"
#include "floatobj.h"

static struct pair_s *lastlb2 = NULL;
static value_t lastlb = NULL;

static struct obj_s obj;

obj_t LABEL_OBJ = &obj;

#define EQUAL_COLUMN 16

value_t root_dict;
static value_t builtin_dict;
value_t current_context;
value_t cheap_context;

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_LABEL: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static void destroy(value_t v1) {
    free((char *)v1->u.label.name.data);
    if (v1->u.label.name.data != v1->u.label.cfname.data) free((uint8_t *)v1->u.label.cfname.data);
    val_destroy(v1->u.label.value);
}

static void garbage(value_t v1, int i) {
    value_t v;
    switch (i) {
    case -1:
        v1->u.label.value->refcount--;
        return;
    case 0:
        free((char *)v1->u.label.name.data);
        if (v1->u.label.name.data != v1->u.label.cfname.data) free((uint8_t *)v1->u.label.cfname.data);
        return;
    case 1:
        v = v1->u.label.value;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == LABEL_OBJ && (v1->u.label.value == v2->u.label.value || obj_same(v1->u.label.value, v2->u.label.value))
        && v1->u.label.name.len == v2->u.label.name.len
        && v1->u.label.cfname.len == v2->u.label.cfname.len
        && (v1->u.label.name.data == v2->u.label.name.data || !memcmp(v1->u.label.name.data, v2->u.label.name.data, v1->u.label.name.len))
        && (v1->u.label.cfname.data == v2->u.label.cfname.data || !memcmp(v1->u.label.cfname.data, v2->u.label.cfname.data, v1->u.label.cfname.len))
        && v1->u.label.file_list == v2->u.label.file_list
        && v1->u.label.epoint.pos == v2->u.label.epoint.pos
        && v1->u.label.epoint.line == v2->u.label.epoint.line
        && v1->u.label.strength == v2->u.label.strength;
}

/* --------------------------------------------------------------------------- */

struct context_stack_s {
    value_t *stack;
    size_t len, p;
};

static struct context_stack_s context_stack;

void push_context(value_t label) {
    if (context_stack.p >= context_stack.len) {
        context_stack.len += 8;
        context_stack.stack = (value_t *)realloc(context_stack.stack, context_stack.len * sizeof(value_t));
        if (!context_stack.stack || context_stack.len < 8 || context_stack.len > SIZE_MAX / sizeof(value_t)) err_msg_out_of_memory(); /* overflow */
    }
    context_stack.stack[context_stack.p++] = val_reference(label);
    current_context = label;
}

int pop_context(void) {
    if (context_stack.p) {
        context_stack.p--;
        val_destroy(context_stack.stack[context_stack.p]);
        if (context_stack.p) {
            current_context = context_stack.stack[context_stack.p - 1];
            return 0;
        }
    }
    return 1;
}

void reset_context(void) {
    while (context_stack.p) {
        context_stack.p--;
        val_destroy(context_stack.stack[context_stack.p]);
    }
    push_context(root_dict);
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t epoint) {
    size_t len;
    uint8_t *s;
    value_t v;
    if (!epoint) return NULL;
    v = val_alloc(STR_OBJ);
    len = v1->u.label.name.len;
    s = str_create_elements(v, len + 8);
    memcpy(s, "<label ", 7);
    memcpy(s + 7, v1->u.label.name.data, len);
    len += 7;
    s[len++] = '>';
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len;
    return v;
}

/* --------------------------------------------------------------------------- */

static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct pair_s *a = cavltree_container_of(aa, struct pair_s, node);
    const struct pair_s *b = cavltree_container_of(bb, struct pair_s, node);
    int h = a->hash - b->hash;
    if (h) return h; 
    return str_cmp(&a->key->u.label.cfname, &b->key->u.label.cfname);
}

static int label_compare2(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct pair_s *a = cavltree_container_of(aa, struct pair_s, node);
    const struct pair_s *b = cavltree_container_of(bb, struct pair_s, node);
    int h = a->hash - b->hash;
    if (h) return h; 
    h = str_cmp(&a->key->u.label.cfname, &b->key->u.label.cfname);
    if (h) return h;
    return b->key->u.label.strength - a->key->u.label.strength;
}

static struct pair_s *strongest_label(struct avltree_node *b) {
    struct pair_s *a = NULL, *c; 
    struct avltree_node *n = b;

    do {
        c = avltree_container_of(n, struct pair_s, node);
        if (c->key->u.label.defpass == pass || (c->key->u.label.constant && (!fixeddig || c->key->u.label.defpass == pass - 1))) a = c;
        n = avltree_next(n);
    } while (n && !label_compare(n, b));
    if (a) return a;
    n = avltree_prev(b);
    while (n && !label_compare(n, b)) {
        c = avltree_container_of(n, struct pair_s, node);
        if (c->key->u.label.defpass == pass || (c->key->u.label.constant && (!fixeddig || c->key->u.label.defpass == pass - 1))) a = c;
        n = avltree_prev(n);
    }
    return a;
}

value_t find_label(const str_t *name) {
    struct avltree_node *b;
    struct pair_s tmp, *c;
    size_t p = context_stack.p;
    value_t context;

    if (!lastlb) lastlb = val_alloc(NONE_OBJ);
    tmp.key = lastlb;
    if (name->len > 1 && !name->data[1]) tmp.key->u.label.cfname = *name;
    else str_cfcpy(&tmp.key->u.label.cfname, name);
    tmp.hash = str_hash(&tmp.key->u.label.cfname);

    while (p) {
        context = context_stack.stack[--p];
        b = avltree_lookup(&tmp.node, &context->u.labeldict.members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) {
                return c->key;
            }
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_dict->u.labeldict.members, label_compare);
    if (b) return avltree_container_of(b, struct pair_s, node)->key;
    return NULL;
}

value_t find_label2(const str_t *name, value_t context) {
    struct avltree_node *b;
    struct pair_s tmp, *c;

    if (!lastlb) lastlb = val_alloc(NONE_OBJ);
    tmp.key = lastlb;
    if (name->len > 1 && !name->data[1]) tmp.key->u.label.cfname = *name;
    else str_cfcpy(&tmp.key->u.label.cfname, name);
    tmp.hash = str_hash(&tmp.key->u.label.cfname);

    b = avltree_lookup(&tmp.node, &context->u.labeldict.members, label_compare);
    if (!b) return NULL;
    c = strongest_label(b);
    return c ? c->key : NULL;
}

static struct {
    uint8_t dir;
    uint8_t padding;
    uint16_t reffile;
    int32_t count;
} anon_idents;

value_t find_label3(const str_t *name, value_t context, uint8_t strength) {
    struct avltree_node *b;
    struct pair_s tmp, *c;

    if (!lastlb) lastlb = val_alloc(NONE_OBJ);
    tmp.key = lastlb;
    if (name->len > 1 && !name->data[1]) tmp.key->u.label.cfname = *name;
    else str_cfcpy(&tmp.key->u.label.cfname, name);
    tmp.hash = str_hash(&tmp.key->u.label.cfname);
    tmp.key->u.label.strength = strength;

    b = avltree_lookup(&tmp.node, &context->u.labeldict.members, label_compare2);
    if (!b) return NULL;
    c = avltree_container_of(b, struct pair_s, node);
    return c ? c->key : NULL;
}

value_t find_anonlabel(int32_t count) {
    struct avltree_node *b;
    struct pair_s tmp, *c;
    size_t p = context_stack.p;
    value_t context;

    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    if (!lastlb) lastlb = val_alloc(NONE_OBJ);
    tmp.key = lastlb;
    tmp.key->u.label.cfname.data = (const uint8_t *)&anon_idents;
    tmp.key->u.label.cfname.len = sizeof(anon_idents);
    tmp.hash = str_hash(&tmp.key->u.label.cfname);

    while (p) {
        context = context_stack.stack[--p];
        b = avltree_lookup(&tmp.node, &context->u.labeldict.members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) return c->key;
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_dict->u.labeldict.members, label_compare);
    if (b) {
        c = avltree_container_of(b, struct pair_s, node);
        return c ? c->key : NULL;
    }
    return NULL;
}

value_t find_anonlabel2(int32_t count, value_t context) {
    struct avltree_node *b;
    struct pair_s tmp, *c;
    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    if (!lastlb) lastlb = val_alloc(NONE_OBJ);
    tmp.key = lastlb;
    tmp.key->u.label.cfname.data = (const uint8_t *)&anon_idents;
    tmp.key->u.label.cfname.len = sizeof(anon_idents);
    tmp.hash = str_hash(&tmp.key->u.label.cfname);

    b = avltree_lookup(&tmp.node, &context->u.labeldict.members, label_compare);
    if (!b) return NULL;
    c = strongest_label(b);
    return c ? c->key : NULL;
}

/* --------------------------------------------------------------------------- */
value_t new_label(const str_t *name, value_t context, uint8_t strength, int *exists) {
    struct avltree_node *b;
    value_t tmp;
    if (!lastlb2) {
        lastlb2 = malloc(sizeof(struct pair_s));
        if (!lastlb2) err_msg_out_of_memory();
    }
    if (!lastlb) lastlb = val_alloc(NONE_OBJ);

    if (name->len > 1 && !name->data[1]) lastlb->u.label.cfname = *name;
    else str_cfcpy(&lastlb->u.label.cfname, name);
    lastlb2->hash = str_hash(&lastlb->u.label.cfname);
    lastlb2->key = lastlb;
    lastlb->u.label.strength = strength;

    b = avltree_insert(&lastlb2->node, &context->u.labeldict.members, label_compare2);
    
    if (!b) { /* new label */
        lastlb->obj = LABEL_OBJ;
        str_cpy(&lastlb->u.label.name, name);
        if (lastlb->u.label.cfname.data == name->data) lastlb->u.label.cfname = lastlb->u.label.name;
        else str_cfcpy(&lastlb->u.label.cfname, NULL);
        lastlb->u.label.ref = 0;
        lastlb->u.label.shadowcheck = 0;
        lastlb->u.label.update_after = 0;
        lastlb->u.label.usepass = 0;
        lastlb->u.label.defpass = pass;
        lastlb2->data = NULL;
	*exists = 0;
	tmp = lastlb;
	lastlb = NULL;
        lastlb2 = NULL;
        context->u.labeldict.len++;
	return tmp;
    }
    *exists = 1;
    return avltree_container_of(b, struct pair_s, node)->key;            /* already exists */
}

void shadow_check(value_t members) {
    const struct avltree_node *n, *b;
    const struct pair_s *l;

    return; /* this works, but needs an option to enable */

    n = avltree_first(&members->u.labeldict.members);
    while (n) {
        l = cavltree_container_of(n, struct pair_s, node);            /* already exists */
        switch (l->key->u.label.value->obj->type) {
        case T_CODE:
            push_context(l->key->u.label.value->u.code.labeldict);
            shadow_check(l->key->u.label.value->u.code.labeldict);
            pop_context();
            break;
        case T_UNION:
        case T_STRUCT:
            push_context(l->key->u.label.value->u.structure.labeldict);
            shadow_check(l->key->u.label.value->u.structure.labeldict);
            pop_context();
            break;
        case T_LABELDICT:
            push_context(l->key->u.label.value);
            shadow_check(l->key->u.label.value);
            pop_context();
            break;
        default: break;
        }
        n = avltree_next(n);
        if (l->key->u.label.shadowcheck) {
            size_t p = context_stack.p;
            while (p) {
                b = avltree_lookup(&l->node, &context_stack.stack[--p]->u.labeldict.members, label_compare);
                if (b) {
                    const struct pair_s *l2, *v1, *v2;
                    v1 = l2 = cavltree_container_of(b, struct pair_s, node);
                    v2 = l;
                    if (v1->key->u.label.value != v2->key->u.label.value && !obj_same(v1->key->u.label.value, v2->key->u.label.value)) {
                        err_msg_shadow_defined(l2->key, l->key);
                        break;
                    }
                }
            }
        }
    }
}

static value_t find_strongest_label(struct avltree_node **x, avltree_cmp_fn_t cmp) {
    struct pair_s *a = NULL, *c; 
    struct avltree_node *b = *x, *n = b;
    do {
        c = avltree_container_of(n, struct pair_s, node);
        if (c->key->u.label.defpass == pass) a = c;
        n = avltree_next(n);
    } while (n && !cmp(n, b));
    *x = n;
    if (a) return a->key;
    n = avltree_prev(b);
    while (n && !cmp(n, b)) {
        c = avltree_container_of(n, struct pair_s, node);
        if (c->key->u.label.defpass == pass) return c->key;
        n = avltree_prev(n);
    }
    return NULL;
}

static void labelname_print(value_t l, FILE *flab) {
    size_t p;
    for (p = 1; p < context_stack.p; p++) {
        putc('.', flab);
        printable_print2(context_stack.stack[p]->u.label.name.data, flab, context_stack.stack[p]->u.label.name.len);
    }
    putc('.', flab);
    printable_print2(l->u.label.name.data, flab, l->u.label.name.len);
}

static inline void padding(int l, int t, FILE *f) {
    if (arguments.tab_size > 1) {
        int l2 = l - l % arguments.tab_size;
        while (l2 + arguments.tab_size <= t) { l2 += arguments.tab_size; l = l2; putc('\t', f);} 
    }
    while (l < t) { l++; putc(' ', f);} 
}

static void labelprint2(const struct avltree *members, FILE *flab) {
    struct avltree_node *n;
    value_t l;

    n = avltree_first(members);
    while (n) {
        l = find_strongest_label(&n, label_compare);            /* already exists */
        if (!l) continue;
        if (l->u.label.name.data && l->u.label.name.len > 1 && !l->u.label.name.data[1]) continue;
        switch (l->u.label.value->obj->type) {
        case T_LBL:
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT: continue;
        default:break;
        }
        if (0) { /* for future use with VICE */
            if (l->u.label.value->obj == CODE_OBJ) {
                value_t err;
                uval_t uv;
                struct linepos_s epoint;
                err = l->u.label.value->obj->uval(l->u.label.value, &uv, 24, &epoint);
                if (err) val_destroy(err);
                else {
                    fprintf(flab, "al %x ", uv);
                    labelname_print(l, flab);
                    switch ((enum dtype_e)l->u.label.value->u.code.dtype) {
                    case D_CHAR:
                    case D_BYTE: 
                        fputs(" byte", flab);
                        if (l->u.label.value->u.code.size > 1) {
                            fprintf(flab, " %" PRIxSIZE, l->u.label.value->u.code.size);
                        }
                        break;
                    case D_INT:
                    case D_WORD: 
                        fputs(" word", flab);
                        if (l->u.label.value->u.code.size > 2) {
                            fprintf(flab, " %" PRIxSIZE, l->u.label.value->u.code.size);
                        }
                        break;
                    case D_LINT:
                    case D_LONG:
                        fputs(" long", flab);
                        if (l->u.label.value->u.code.size > 3) {
                            fprintf(flab, " %" PRIxSIZE, l->u.label.value->u.code.size);
                        }
                        break;
                    case D_DINT:
                    case D_DWORD:
                        fputs(" dword", flab);
                        if (l->u.label.value->u.code.size > 4) {
                            fprintf(flab, " %" PRIxSIZE, l->u.label.value->u.code.size);
                        }
                        break;
                    case D_NONE:
                        break;
                    }
                    putc('\n', flab);
                }
            }
            push_context(l);
            labelprint2(&l->u.label.value->u.code.labeldict->u.labeldict.members, flab);
            pop_context();
        } else {
            value_t val = l->u.label.value->obj->repr(l->u.label.value, NULL);
            size_t len;
            if (!val || val->obj != STR_OBJ) continue;
            len = printable_print2(l->u.label.name.data, flab, l->u.label.name.len);
            padding(len, EQUAL_COLUMN, flab);
            if (l->u.label.constant) fputs("= ", flab);
            else fputs(&" .var "[len < EQUAL_COLUMN], flab);
            printable_print2(val->u.str.data, flab, val->u.str.len);
            putc('\n', flab);
            val_destroy(val);
        }
    }
}

void labelprint(void) {
    int oldreferenceit = referenceit;
    FILE *flab;
    struct linepos_s nopoint = {0, 0};

    if (arguments.label[0] == '-' && !arguments.label[1]) {
        flab = stdout;
    } else {
        if (!(flab = file_open(arguments.label, "wt"))) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label, &nopoint);
    }
    clearerr(flab);
    referenceit = 0;
    labelprint2(&root_dict->u.labeldict.members, flab);
    referenceit = oldreferenceit;
    if (flab == stdout) fflush(flab);
    if (ferror(flab) && errno) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label, &nopoint);
    if (flab != stdout) fclose(flab);
}

static void new_builtin(const char *ident, value_t val) {
    struct linepos_s nopoint = {0, 0};
    str_t name;
    value_t label;
    int label_exists;
    name.len = strlen(ident);
    name.data = (const uint8_t *)ident;
    label = new_label(&name, builtin_dict, 0, &label_exists);
    label->u.label.constant = 1;
    label->u.label.value = val;
    label->u.label.file_list = NULL;
    label->u.label.epoint = nopoint;
}

void init_variables(void)
{
    struct linepos_s nopoint = {0, 0};

    builtin_dict = new_labeldict(NULL, &nopoint);
    root_dict = new_labeldict(NULL, &nopoint);

    context_stack.stack = NULL;
    context_stack.p = context_stack.len = 0;
}

void init_defaultlabels(void) {
    value_t v;
    int i;

    new_builtin("true", val_reference(true_value));
    new_builtin("false", val_reference(false_value));

    for (i = 0; reg_names[i]; i++) {
        char name[2];
        name[0] = reg_names[i];
        name[1] = 0;
        v = val_alloc(REGISTER_OBJ);
        v->u.reg.val[0] = name[0];
        v->u.reg.data = v->u.reg.val;
        v->u.reg.len = 1;
        v->u.reg.chars = 1;
        new_builtin(name, v);
    }

    for (i = 0; builtin_functions[i].name; i++) {
        v = val_alloc(FUNCTION_OBJ);
        v->u.function.name.data = (const uint8_t *)builtin_functions[i].name;
        v->u.function.name.len = strlen(builtin_functions[i].name);
        v->u.function.func = builtin_functions[i].func;
        v->u.function.name_hash = str_hash(&v->u.function.name);
        new_builtin(builtin_functions[i].name, v);
    }

    v = val_alloc(TYPE_OBJ); v->u.type = TYPE_OBJ; new_builtin("type", v);
    v = val_alloc(TYPE_OBJ); v->u.type = STR_OBJ; new_builtin("str", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BOOL_OBJ; new_builtin("bool", v);
    v = val_alloc(TYPE_OBJ); v->u.type = INT_OBJ; new_builtin("int", v);
    v = val_alloc(TYPE_OBJ); v->u.type = FLOAT_OBJ; new_builtin("float", v);
    v = val_alloc(TYPE_OBJ); v->u.type = REGISTER_OBJ; new_builtin("register", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BYTES_OBJ; new_builtin("bytes", v);
    v = val_alloc(TYPE_OBJ); v->u.type = BITS_OBJ; new_builtin("bits", v);
    v = val_alloc(TYPE_OBJ); v->u.type = DICT_OBJ; new_builtin("dict", v);
    v = val_alloc(TYPE_OBJ); v->u.type = LIST_OBJ; new_builtin("list", v);
    v = val_alloc(TYPE_OBJ); v->u.type = TUPLE_OBJ; new_builtin("tuple", v);
    v = val_alloc(TYPE_OBJ); v->u.type = ADDRESS_OBJ; new_builtin("address", v);
    v = val_alloc(TYPE_OBJ); v->u.type = CODE_OBJ; new_builtin("code", v);
    v = val_alloc(TYPE_OBJ); v->u.type = GAP_OBJ; new_builtin("gap", v);
}

void destroy_variables(void)
{
    val_destroy(builtin_dict);
    val_destroy(root_dict);
    if (lastlb) val_destroy(lastlb);
    free(lastlb2);
    while (context_stack.p) {
        context_stack.p--;
        val_destroy(context_stack.stack[context_stack.p]);
    }
    free(context_stack.stack);
}

void labelobj_init(void) {
    obj_init(&obj, T_LABEL, "label");
    obj.create = create;
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.repr = repr;
}
