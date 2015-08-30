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
#include "64tass.h"
#include "file.h"
#include "obj.h"
#include "boolobj.h"
#include "floatobj.h"
#include "error.h"
#include "namespaceobj.h"
#include "strobj.h"
#include "codeobj.h"
#include "registerobj.h"
#include "functionobj.h"
#include "listobj.h"
#include "intobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "dictobj.h"
#include "addressobj.h"
#include "gapobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "values.h"

static struct namespacekey_s *lastlb2 = NULL;
static Label *lastlb = NULL;

static Type obj;

Type *LABEL_OBJ = &obj;

#define EQUAL_COLUMN 16

Namespace *root_namespace;
static Namespace *builtin_namespace;
Namespace *current_context;
Namespace *cheap_context;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_LABEL: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static void destroy(Obj *o1) {
    Label *v1 = (Label *)o1;
    free((char *)v1->name.data);
    if (v1->name.data != v1->cfname.data) free((uint8_t *)v1->cfname.data);
    val_destroy(v1->value);
}

static void garbage(Obj *o1, int i) {
    Label *v1 = (Label *)o1;
    Obj *v;
    switch (i) {
    case -1:
        v1->value->refcount--;
        return;
    case 0:
        free((char *)v1->name.data);
        if (v1->name.data != v1->cfname.data) free((uint8_t *)v1->cfname.data);
        return;
    case 1:
        v = v1->value;
        if (v->refcount & SIZE_MSB) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static int same(const Obj *o1, const Obj *o2) {
    const Label *v1 = (const Label *)o1, *v2 = (const Label *)o2;
    return o2->obj == LABEL_OBJ && (v1->value == v2->value || v1->value->obj->same(v1->value, v2->value))
        && v1->name.len == v2->name.len
        && v1->cfname.len == v2->cfname.len
        && (v1->name.data == v2->name.data || !memcmp(v1->name.data, v2->name.data, v1->name.len))
        && (v1->cfname.data == v2->cfname.data || !memcmp(v1->cfname.data, v2->cfname.data, v1->cfname.len))
        && v1->file_list == v2->file_list
        && v1->epoint.pos == v2->epoint.pos
        && v1->epoint.line == v2->epoint.line
        && v1->strength == v2->strength;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxlen) {
    Label *v1 = (Label *)o1;
    size_t len;
    uint8_t *s;
    Str *v;
    if (!epoint) return NULL;
    len = v1->name.len;
    if (len + 8 > maxlen) return NULL;
    v = new_str();
    s = str_create_elements(v, len + 8);
    memcpy(s, "<label ", 7);
    memcpy(s + 7, v1->name.data, len);
    len += 7;
    s[len++] = '>';
    v->data = s;
    v->len = len;
    v->chars = len; /* UTF8! */
    return &v->v;
}

void labelobj_init(void) {
    new_type(&obj, T_LABEL, "label", sizeof(Label));
    obj_init(&obj);
    obj.create = create;
    obj.destroy = destroy;
    obj.garbage = garbage;
    obj.same = same;
    obj.repr = repr;
}

/* --------------------------------------------------------------------------- */

struct cstack_s {
    Namespace *normal;
    Namespace *cheap;
};

struct context_stack_s {
    struct cstack_s *stack;
    size_t len, p, bottom;
};

static struct context_stack_s context_stack;

void push_context(Namespace *name) {
    if (context_stack.p >= context_stack.len) {
        context_stack.len += 8;
        if (/*context_stack.len < 8 ||*/ context_stack.len > SIZE_MAX / sizeof(struct cstack_s)) err_msg_out_of_memory(); /* overflow */
        context_stack.stack = (struct cstack_s *)reallocx(context_stack.stack, context_stack.len * sizeof(struct cstack_s));
    }
    context_stack.stack[context_stack.p].normal = ref_namespace(name);
    current_context = name;
    context_stack.stack[context_stack.p].cheap = cheap_context;
    cheap_context = ref_namespace(name);
    context_stack.p++;
}

int pop_context(void) {
    if (context_stack.p > 1 + context_stack.bottom) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(&c->normal->v);
        val_destroy(&c->cheap->v);
        c = &context_stack.stack[context_stack.p - 1];
        current_context = c->normal;
        val_destroy(&cheap_context->v); 
        cheap_context = ref_namespace(c->cheap);
        return 0;
    }
    return 1;
}

void reset_context(void) {
    context_stack.bottom = 0;
    while (context_stack.p) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(&c->normal->v);
        val_destroy(&c->cheap->v);
    }
    val_destroy(&cheap_context->v); 
    cheap_context = ref_namespace(root_namespace);
    push_context(root_namespace);
}

void get_namespaces(Mfunc *mfunc) {
    size_t i, len = context_stack.p - context_stack.bottom;
    if (len > SIZE_MAX / sizeof(Namespace *)) err_msg_out_of_memory(); /* overflow */
    mfunc->nslen = len;
    mfunc->namespaces = (Namespace **)mallocx(len * sizeof(Namespace *));
    for (i = context_stack.bottom; i < context_stack.p; i++) {
        mfunc->namespaces[i] = ref_namespace(context_stack.stack[i].normal);
    }
}

void context_set_bottom(size_t n) {
    context_stack.bottom = n;
}

size_t context_get_bottom(void) {
    size_t old = context_stack.bottom;
    context_stack.bottom = context_stack.p;
    return old;
}

/* --------------------------------------------------------------------------- */

static int label_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct namespacekey_s *a = cavltree_container_of(aa, struct namespacekey_s, node);
    const struct namespacekey_s *b = cavltree_container_of(bb, struct namespacekey_s, node);
    int h = a->hash - b->hash;
    if (h) return h; 
    return str_cmp(&a->key->cfname, &b->key->cfname);
}

static int label_compare2(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct namespacekey_s *a = cavltree_container_of(aa, struct namespacekey_s, node);
    const struct namespacekey_s *b = cavltree_container_of(bb, struct namespacekey_s, node);
    int h = a->hash - b->hash;
    if (h) return h; 
    h = str_cmp(&a->key->cfname, &b->key->cfname);
    if (h) return h;
    return b->key->strength - a->key->strength;
}

static struct namespacekey_s *strongest_label(struct avltree_node *b) {
    struct namespacekey_s *a = NULL, *c; 
    struct avltree_node *n = b;

    do {
        c = avltree_container_of(n, struct namespacekey_s, node);
        if (c->key->defpass == pass || (c->key->constant && (!fixeddig || c->key->defpass == pass - 1))) a = c;
        n = avltree_next(n);
    } while (n && !label_compare(n, b));
    if (a) return a;
    n = avltree_prev(b);
    while (n && !label_compare(n, b)) {
        c = avltree_container_of(n, struct namespacekey_s, node);
        if (c->key->defpass == pass || (c->key->constant && (!fixeddig || c->key->defpass == pass - 1))) a = c;
        n = avltree_prev(n);
    }
    return a;
}

Label *find_label(const str_t *name, Namespace **here) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    size_t p = context_stack.p;
    Label label;

    tmp.key = &label;
    if (name->len > 1 && !name->data[1]) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);

    while (context_stack.bottom < p) {
        Namespace *context = context_stack.stack[--p].normal;
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) {
                if (here) *here = context;
                return c->key;
            }
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_namespace->members, label_compare);
    if (b) {
        if (here) *here = builtin_namespace;
        return avltree_container_of(b, struct namespacekey_s, node)->key;
    }
    if (here) *here = NULL;
    return NULL;
}

Label *find_label2(const str_t *name, Namespace *context) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    Label label;

    tmp.key = &label;
    if (name->len > 1 && !name->data[1]) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
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

Label *find_label3(const str_t *name, Namespace *context, uint8_t strength) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    Label label;

    tmp.key = &label;
    if (name->len > 1 && !name->data[1]) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);
    tmp.key->strength = strength;

    b = avltree_lookup(&tmp.node, &context->members, label_compare2);
    if (!b) return NULL;
    c = avltree_container_of(b, struct namespacekey_s, node);
    return c ? c->key : NULL;
}

Label *find_anonlabel(int32_t count) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    size_t p = context_stack.p;
    Namespace *context;
    Label label;

    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    tmp.key = &label;
    tmp.key->cfname.data = (const uint8_t *)&anon_idents;
    tmp.key->cfname.len = sizeof(anon_idents);
    tmp.hash = str_hash(&tmp.key->cfname);

    while (context_stack.bottom < p) {
        context = context_stack.stack[--p].normal;
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b) {
            c = strongest_label(b);
            if (c) return c->key;
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_namespace->members, label_compare);
    if (b) {
        c = avltree_container_of(b, struct namespacekey_s, node);
        return c ? c->key : NULL;
    }
    return NULL;
}

Label *find_anonlabel2(int32_t count, Namespace *context) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    Label label;

    anon_idents.dir = (count >= 0) ? '+' : '-';
    anon_idents.reffile = reffile;
    anon_idents.count = ((count >= 0) ? forwr : backr) + count;

    tmp.key = &label;
    tmp.key->cfname.data = (const uint8_t *)&anon_idents;
    tmp.key->cfname.len = sizeof(anon_idents);
    tmp.hash = str_hash(&tmp.key->cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
    if (!b) return NULL;
    c = strongest_label(b);
    return c ? c->key : NULL;
}

/* --------------------------------------------------------------------------- */
Label *new_label(const str_t *name, Namespace *context, uint8_t strength, int *exists) {
    struct avltree_node *b;
    Label *tmp;
    if (!lastlb2) {
        lastlb2 = namespacekey_alloc();
    }
    if (!lastlb) lastlb = (Label *)val_alloc(LABEL_OBJ);

    if (name->len > 1 && !name->data[1]) lastlb->cfname = *name;
    else str_cfcpy(&lastlb->cfname, name);
    lastlb2->hash = str_hash(&lastlb->cfname);
    lastlb2->key = lastlb;
    lastlb->strength = strength;

    b = avltree_insert(&lastlb2->node, &context->members, label_compare2);
    
    if (!b) { /* new label */
        str_cpy(&lastlb->name, name);
        if (lastlb->cfname.data == name->data) lastlb->cfname = lastlb->name;
        else str_cfcpy(&lastlb->cfname, NULL);
        lastlb->ref = 0;
        lastlb->shadowcheck = 0;
        lastlb->update_after = 0;
        lastlb->usepass = 0;
        lastlb->defpass = pass;
	*exists = 0;
	tmp = lastlb;
	lastlb = NULL;
        lastlb2 = NULL;
        context->len++;
	return tmp;
    }
    *exists = 1;
    return avltree_container_of(b, struct namespacekey_s, node)->key;            /* already exists */
}

void shadow_check(Namespace *members) {
    const struct avltree_node *n, *b;
    const struct namespacekey_s *l;

    return; /* this works, but needs an option to enable */

    n = avltree_first(&members->members);
    while (n) {
        l = cavltree_container_of(n, struct namespacekey_s, node);            /* already exists */
        switch (l->key->value->obj->type) {
        case T_CODE:
            push_context(((Code *)l->key->value)->names);
            shadow_check(((Code *)l->key->value)->names);
            pop_context();
            break;
        case T_UNION:
        case T_STRUCT:
            push_context(((Struct *)l->key->value)->names);
            shadow_check(((Struct *)l->key->value)->names);
            pop_context();
            break;
        case T_NAMESPACE:
            push_context((Namespace *)l->key->value);
            shadow_check((Namespace *)l->key->value);
            pop_context();
            break;
        default: break;
        }
        n = avltree_next(n);
        if (l->key->shadowcheck) {
            size_t p = context_stack.p;
            while (context_stack.bottom < p) {
                b = avltree_lookup(&l->node, &context_stack.stack[--p].normal->members, label_compare);
                if (b) {
                    const struct namespacekey_s *l2, *v1, *v2;
                    v1 = l2 = cavltree_container_of(b, struct namespacekey_s, node);
                    v2 = l;
                    if (v1->key->value != v2->key->value && !v1->key->value->obj->same(v1->key->value, v2->key->value)) {
                        err_msg_shadow_defined(l2->key, l->key);
                        break;
                    }
                }
            }
        }
    }
}

static Label *find_strongest_label(struct avltree_node **x, avltree_cmp_fn_t cmp) {
    struct namespacekey_s *a = NULL, *c; 
    struct avltree_node *b = *x, *n = b;
    do {
        c = avltree_container_of(n, struct namespacekey_s, node);
        if (c->key->defpass == pass) a = c;
        n = avltree_next(n);
    } while (n && !cmp(n, b));
    *x = n;
    if (a) return a->key;
    n = avltree_prev(b);
    while (n && !cmp(n, b)) {
        c = avltree_container_of(n, struct namespacekey_s, node);
        if (c->key->defpass == pass) return c->key;
        n = avltree_prev(n);
    }
    return NULL;
}

static inline void padding(int l, int t, FILE *f) {
    if (arguments.tab_size > 1) {
        int l2 = l - l % arguments.tab_size;
        while (l2 + arguments.tab_size <= t) { l2 += arguments.tab_size; l = l2; putc('\t', f);} 
    }
    while (l < t) { l++; putc(' ', f);} 
}

static void labelprint2(const struct avltree *members, FILE *flab, int labelmode) {
    struct avltree_node *n;
    Label *l;

    n = avltree_first(members);
    while (n) {
        l = find_strongest_label(&n, label_compare);            /* already exists */
        if (!l || !l->name.data) continue;
        if (l->name.len > 1 && !l->name.data[1]) continue;
        switch (l->value->obj->type) {
        case T_LBL:
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT: continue;
        default:break;
        }
        if (labelmode == LABEL_VICE) {
            if (l->value->obj == CODE_OBJ) {
                Error *err;
                uval_t uv;
                struct linepos_s epoint;
                err = l->value->obj->uval(l->value, &uv, 24, &epoint);
                if (err) val_destroy(&err->v);
                else {
                    size_t i, j = l->name.len;
                    const uint8_t *d = l->name.data;
                    for (i = 0; i < j; i++) {
                        if (d[i] & 0x80) break;
                    }
                    if (i == j) {
                        fprintf(flab, "al %" PRIx32 " .", uv);
                        printable_print2(l->name.data, flab, l->name.len);
                        putc('\n', flab);
                    }
                }
            }
        } else {
            Str *val = (Str *)l->value->obj->repr(l->value, NULL, SIZE_MAX);
            size_t len;
            if (!val || val->v.obj != STR_OBJ) continue;
            len = printable_print2(l->name.data, flab, l->name.len);
            padding(len, EQUAL_COLUMN, flab);
            if (l->constant) fputs("= ", flab);
            else fputs(&" .var "[len < EQUAL_COLUMN], flab);
            printable_print2(val->data, flab, val->len);
            putc('\n', flab);
            val_destroy(&val->v);
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
    labelprint2(&root_namespace->members, flab, arguments.label_mode);
    referenceit = oldreferenceit;
    if (flab == stdout) fflush(flab);
    if (ferror(flab) && errno) err_msg_file(ERROR_CANT_DUMP_LBL, arguments.label, &nopoint);
    if (flab != stdout) fclose(flab);
}

void new_builtin(const char *ident, Obj *val) {
    struct linepos_s nopoint = {0, 0};
    str_t name;
    Label *label;
    int label_exists;
    name.len = strlen(ident);
    name.data = (const uint8_t *)ident;
    label = new_label(&name, builtin_namespace, 0, &label_exists);
    label->constant = 1;
    label->value = val;
    label->file_list = NULL;
    label->epoint = nopoint;
}

void init_variables(void)
{
    struct linepos_s nopoint = {0, 0};

    builtin_namespace = new_namespace(NULL, &nopoint);
    root_namespace = new_namespace(NULL, &nopoint);
    cheap_context = ref_namespace(root_namespace);

    context_stack.stack = NULL;
    context_stack.p = context_stack.len = context_stack.bottom = 0;

    boolobj_names();
    registerobj_names();
    functionobj_names();
    gapobj_names();
    listobj_names();
    strobj_names();
    intobj_names();
    floatobj_names();
    codeobj_names();
    addressobj_names();
    dictobj_names();
    bitsobj_names();
    bytesobj_names();
    typeobj_names();
}

void destroy_lastlb(void) {
    if (lastlb) {
        lastlb->name.data = NULL;
        lastlb->cfname.data = NULL;
        lastlb->value = (Obj *)ref_none();
        val_destroy(&lastlb->v);
        lastlb = NULL;
    }
}

void destroy_variables(void)
{
    val_destroy(&builtin_namespace->v);
    val_destroy(&root_namespace->v);
    val_destroy(&cheap_context->v);
    destroy_lastlb();
    if (lastlb2) namespacekey_free(lastlb2);
    while (context_stack.p) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(&c->normal->v);
        val_destroy(&c->cheap->v);
    }
    free(context_stack.stack);
}
