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
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static bool same(const Obj *o1, const Obj *o2) {
    const Label *v1 = (const Label *)o1, *v2 = (const Label *)o2;
    return o2->obj == LABEL_OBJ && (v1->value == v2->value || v1->value->obj->same(v1->value, v2->value))
        && v1->name.len == v2->name.len
        && v1->cfname.len == v2->cfname.len
        && (v1->name.data == v2->name.data || memcmp(v1->name.data, v2->name.data, v1->name.len) == 0)
        && (v1->cfname.data == v2->cfname.data || memcmp(v1->cfname.data, v2->cfname.data, v1->cfname.len) == 0)
        && v1->file_list == v2->file_list
        && v1->epoint.pos == v2->epoint.pos
        && v1->epoint.line == v2->epoint.line
        && v1->strength == v2->strength;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxlen) {
    Label *v1 = (Label *)o1;
    size_t len, len2;
    uint8_t *s;
    Str *v;
    if (epoint == NULL) return NULL;
    len = v1->name.len;
    len2 = len + 10;
    if (len2 > maxlen) return NULL;
    v = new_str(len2);
    v->chars = len2; /* UTF8! */
    s = v->data;
    memcpy(s, "<label '", 8);
    s += 8;
    memcpy(s, v1->name.data, len);
    s[len] = '\'';
    s[len + 1] = '>';
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
        if (/*context_stack.len < 8 ||*/ context_stack.len > SIZE_MAX / sizeof *context_stack.stack) err_msg_out_of_memory(); /* overflow */
        context_stack.stack = (struct cstack_s *)reallocx(context_stack.stack, context_stack.len * sizeof *context_stack.stack);
    }
    context_stack.stack[context_stack.p].normal = ref_namespace(name);
    current_context = name;
    context_stack.stack[context_stack.p].cheap = cheap_context;
    cheap_context = ref_namespace(name);
    context_stack.p++;
}

bool pop_context(void) {
    if (context_stack.p > 1 + context_stack.bottom) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(&c->normal->v);
        val_destroy(&cheap_context->v); 
        cheap_context = c->cheap;
        c = &context_stack.stack[context_stack.p - 1];
        current_context = c->normal;
        return false;
    }
    return true;
}

void reset_context(void) {
    context_stack.bottom = 0;
    while (context_stack.p != 0) {
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
    if (len > SIZE_MAX / sizeof *mfunc->namespaces) err_msg_out_of_memory(); /* overflow */
    mfunc->nslen = len;
    mfunc->namespaces = (Namespace **)mallocx(len * sizeof *mfunc->namespaces);
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
    if (h != 0) return h; 
    return str_cmp(&a->key->cfname, &b->key->cfname);
}

static int label_compare2(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct namespacekey_s *a = cavltree_container_of(aa, struct namespacekey_s, node);
    const struct namespacekey_s *b = cavltree_container_of(bb, struct namespacekey_s, node);
    int h = a->hash - b->hash;
    if (h != 0) return h; 
    h = str_cmp(&a->key->cfname, &b->key->cfname);
    if (h != 0) return h;
    return b->key->strength - a->key->strength;
}

static struct namespacekey_s *strongest_label(struct avltree_node *b) {
    struct namespacekey_s *a = NULL, *c; 
    struct avltree_node *n = b;

    do {
        c = avltree_container_of(n, struct namespacekey_s, node);
        if (c->key->defpass == pass || (c->key->constant && (!fixeddig || c->key->defpass == pass - 1))) a = c;
        n = avltree_next(n);
    } while (n != NULL && label_compare(n, b) == 0);
    if (a != NULL) return a;
    n = avltree_prev(b);
    while (n != NULL && label_compare(n, b) == 0) {
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
    if (name->len > 1 && name->data[1] == 0) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);

    while (context_stack.bottom < p) {
        Namespace *context = context_stack.stack[--p].normal;
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b != NULL) {
            c = strongest_label(b);
            if (c != NULL) {
                if (here != NULL) *here = context;
                return c->key;
            }
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_namespace->members, label_compare);
    if (b != NULL) {
        if (here != NULL) *here = builtin_namespace;
        return avltree_container_of(b, struct namespacekey_s, node)->key;
    }
    if (here != NULL) *here = NULL;
    return NULL;
}

Label *find_label2(const str_t *name, Namespace *context) {
    struct avltree_node *b;
    struct namespacekey_s tmp, *c;
    Label label;

    tmp.key = &label;
    if (name->len > 1 && name->data[1] == 0) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
    if (b == NULL) return NULL;
    c = strongest_label(b);
    return (c != NULL) ? c->key : NULL;
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
    if (name->len > 1 && name->data[1] == 0) tmp.key->cfname = *name;
    else str_cfcpy(&tmp.key->cfname, name);
    tmp.hash = str_hash(&tmp.key->cfname);
    tmp.key->strength = strength;

    b = avltree_lookup(&tmp.node, &context->members, label_compare2);
    if (b == NULL) return NULL;
    c = avltree_container_of(b, struct namespacekey_s, node);
    return (c != NULL) ? c->key : NULL;
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
    tmp.key->cfname.len = sizeof anon_idents;
    tmp.hash = str_hash(&tmp.key->cfname);

    while (context_stack.bottom < p) {
        context = context_stack.stack[--p].normal;
        b = avltree_lookup(&tmp.node, &context->members, label_compare);
        if (b != NULL) {
            c = strongest_label(b);
            if (c != NULL) return c->key;
        }
    }
    b = avltree_lookup(&tmp.node, &builtin_namespace->members, label_compare);
    if (b == NULL) return NULL;
    c = avltree_container_of(b, struct namespacekey_s, node);
    return (c != NULL) ? c->key : NULL;
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
    tmp.key->cfname.len = sizeof anon_idents;
    tmp.hash = str_hash(&tmp.key->cfname);

    b = avltree_lookup(&tmp.node, &context->members, label_compare);
    if (b == NULL) return NULL;
    c = strongest_label(b);
    return (c != NULL) ? c->key : NULL;
}

/* --------------------------------------------------------------------------- */
Label *new_label(const str_t *name, Namespace *context, uint8_t strength, bool *exists) {
    struct avltree_node *b;
    Label *tmp;
    if (lastlb2 == NULL) {
        lastlb2 = namespacekey_alloc();
    }
    if (lastlb == NULL) lastlb = (Label *)val_alloc(LABEL_OBJ);

    if (name->len > 1 && name->data[1] == 0) lastlb->cfname = *name;
    else str_cfcpy(&lastlb->cfname, name);
    lastlb2->hash = str_hash(&lastlb->cfname);
    lastlb2->key = lastlb;
    lastlb->strength = strength;

    b = avltree_insert(&lastlb2->node, &context->members, label_compare2);
    
    if (b == NULL) { /* new label */
        str_cpy(&lastlb->name, name);
        if (lastlb->cfname.data == name->data) lastlb->cfname = lastlb->name;
        else str_cfcpy(&lastlb->cfname, NULL);
        lastlb->ref = false;
        lastlb->shadowcheck = false;
        lastlb->update_after = false;
        lastlb->usepass = 0;
        lastlb->defpass = pass;
	*exists = false;
	tmp = lastlb;
	lastlb = NULL;
        lastlb2 = NULL;
        context->len++;
	return tmp;
    }
    *exists = true;
    return avltree_container_of(b, struct namespacekey_s, node)->key;            /* already exists */
}

void shadow_check(Namespace *members) {
    const struct avltree_node *n;

    for (n = avltree_first(&members->members); n != NULL; n = avltree_next(n)) {
        const struct namespacekey_s *l = cavltree_container_of(n, struct namespacekey_s, node);
        Label *key2 = l->key;
        Obj *o  = key2->value;
        Namespace *ns;

        switch (o->obj->type) {
        case T_CODE:
            ns = ((Code *)o)->names;
            break;
        case T_UNION:
        case T_STRUCT:
            ns = ((Struct *)o)->names;
            break;
        case T_NAMESPACE:
            ns = (Namespace *)o;
            break;
        default: 
            ns = NULL;
            break;
        }
        if (ns != NULL && ns->len != 0 && key2->owner) {
            size_t ln = ns->len;
            ns->len = 0;
            push_context(ns);
            shadow_check(ns);
            pop_context();
            ns->len = ln;
        }
        if (key2->shadowcheck) {
            const struct avltree_node *b;
            size_t p = context_stack.p;
            Obj *o2 = key2->value;
            while (context_stack.bottom < p) {
                b = avltree_lookup(&l->node, &context_stack.stack[--p].normal->members, label_compare2);
                if (b != NULL) {
                    const struct namespacekey_s *l2 = cavltree_container_of(b, struct namespacekey_s, node);
                    Label *key1 = l2->key;
                    Obj *o1 = key1->value;
                    if (o1 != o2 && !o1->obj->same(o1, o2)) {
                        err_msg_shadow_defined(key1, key2);
                        break;
                    }
                }
            }
            b = avltree_lookup(&l->node, &builtin_namespace->members, label_compare2);
            if (b != NULL) {
                const struct namespacekey_s *l2 = cavltree_container_of(b, struct namespacekey_s, node);
                Label *key1 = l2->key;
                Obj *o1 = key1->value;
                if (o1 != o2 && !o1->obj->same(o1, o2)) {
                    err_msg_shadow_defined2(key2);
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
    } while (n != NULL && cmp(n, b) == 0);
    *x = n;
    if (a != NULL) return a->key;
    n = avltree_prev(b);
    while (n != NULL && cmp(n, b) == 0) {
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
    while (n != NULL) {
        l = find_strongest_label(&n, label_compare);            /* already exists */
        if (l == NULL || l->name.data == NULL) continue;
        if (l->name.len > 1 && l->name.data[1] == 0) continue;
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
                if (err != NULL) val_destroy(&err->v);
                else {
                    size_t i, j = l->name.len;
                    const uint8_t *d = l->name.data;
                    for (i = 0; i < j; i++) {
                        if ((d[i] & 0x80) != 0) break;
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
            if (val != NULL) {
                if (val->v.obj == STR_OBJ) {
                    len = printable_print2(l->name.data, flab, l->name.len);
                    padding(len, EQUAL_COLUMN, flab);
                    if (l->constant) fputs("= ", flab);
                    else fputs(&" := "[len < EQUAL_COLUMN], flab);
                    printable_print2(val->data, flab, val->len);
                    putc('\n', flab);
                }
                val_destroy(&val->v);
            }
        }
    }
}

static inline const uint8_t *get_line(const struct file_s *file, size_t line) {
    return &file->data[file->line[line - 1]];
}

static void labeldump(Namespace *members, const str_t *prefix, FILE *flab) {
    const struct avltree_node *n;

    for (n = avltree_first(&members->members); n != NULL; n = avltree_next(n)) {
        const struct namespacekey_s *l = cavltree_container_of(n, struct namespacekey_s, node);
        Label *l2 = l->key;
        Obj *o  = l2->value;
        Namespace *ns;

        if (l2->name.len < 2 || l2->name.data[1] != 0) {
            Str *val = (Str *)l2->value->obj->repr(l2->value, NULL, SIZE_MAX);
            if (val != NULL) {
                if (val->v.obj == STR_OBJ) {
                    const struct file_s *file = l2->file_list->file;
                    linepos_t epoint = &l2->epoint;
                    printable_print((uint8_t *)file->realname, flab);
                    fprintf(flab, ":%" PRIuline ":%" PRIlinepos ": ", epoint->line, calcpos(get_line(file, epoint->line), epoint->pos, file->coding == E_UTF8));
                    if (prefix->len != 0) {
                        printable_print2(prefix->data, flab, prefix->len);
                        putc('.', flab);
                    }
                    printable_print2(l2->name.data, flab, l2->name.len);
                    fputs(l2->constant ? " = " : " := ", flab);
                    printable_print2(val->data, flab, val->len);
                    putc('\n', flab);
                }
                val_destroy(&val->v);
            }
        }

        switch (o->obj->type) {
        case T_CODE:
            ns = ((Code *)o)->names;
            break;
        case T_UNION:
        case T_STRUCT:
            ns = ((Struct *)o)->names;
            break;
        case T_NAMESPACE:
            ns = (Namespace *)o;
            break;
        default: 
            ns = NULL;
            break;
        }

        if (ns != NULL && ns->len != 0 && l2->owner) {
            if (l2->name.len < 2 || l2->name.data[1] != 0) {
                str_t newprefix;
                uint8_t *s;
                size_t ln = ns->len;
                ns->len = 0;
                newprefix.len = prefix->len + l2->name.len;
                if (newprefix.len < prefix->len) err_msg_out_of_memory(); /* overflow */
                if (prefix->len != 0) {
                    newprefix.len++;
                    if (newprefix.len == 0) err_msg_out_of_memory(); /* overflow */
                }
                s = (uint8_t *)mallocx(newprefix.len);
                newprefix.data = s;
                if (prefix->len != 0) {
                    memcpy(s, prefix->data, prefix->len);
                    s += prefix->len;
                    *s++ = '.';
                }
                memcpy(s, l2->name.data, l2->name.len);
                labeldump(ns, &newprefix, flab);
                free((uint8_t *)newprefix.data);
                ns->len = ln;
            }
        }
    }
}

void labelprint(void) {
    bool oldreferenceit = referenceit;
    FILE *flab;
    struct linepos_s nopoint = {0, 0};
    int err;

    flab = dash_name(arguments.label) ? stdout : file_open(arguments.label, "wt");
    if (flab == NULL) {
        err_msg_file(ERROR_CANT_WRTE_LBL, arguments.label, &nopoint);
        return;
    }
    clearerr(flab);
    referenceit = false;
    if (arguments.label_mode == LABEL_DUMP) {
        str_t root = {0, NULL};
        labeldump(root_namespace, &root, flab);
    } else {
        labelprint2(&root_namespace->members, flab, arguments.label_mode);
    }
    referenceit = oldreferenceit;
    err = ferror(flab);
    err |= (flab != stdout) ? fclose(flab) : fflush(flab);
    if (err != 0 && errno) err_msg_file(ERROR_CANT_WRTE_LBL, arguments.label, &nopoint);
}

void new_builtin(const char *ident, Obj *val) {
    struct linepos_s nopoint = {0, 0};
    str_t name;
    Label *label;
    bool label_exists;
    name.len = strlen(ident);
    name.data = (const uint8_t *)ident;
    label = new_label(&name, builtin_namespace, 0, &label_exists);
    label->constant = true;
    label->owner = true;
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
    if (lastlb != NULL) {
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
    if (lastlb2 != NULL) namespacekey_free(lastlb2);
    while (context_stack.p != 0) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(&c->normal->v);
        val_destroy(&c->cheap->v);
    }
    free(context_stack.stack);
}
