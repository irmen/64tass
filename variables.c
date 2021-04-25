/*
    $Id: variables.c 2618 2021-04-25 11:11:11Z soci $

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
#include "variables.h"
#include <string.h>
#include <errno.h>
#include "unicode.h"
#include "64tass.h"
#include "file.h"
#include "obj.h"
#include "error.h"
#include "values.h"
#include "arguments.h"
#include "eval.h"

#include "boolobj.h"
#include "floatobj.h"
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
#include "labelobj.h"
#include "errorobj.h"
#include "mfuncobj.h"

static Label *lastlb = NULL;

#define EQUAL_COLUMN 16

Namespace *root_namespace;
static Namespace *builtin_namespace;
Namespace *current_context;
Namespace *cheap_context;
size_t fwcount;

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

void push_dummy_context(void) {
    push_context(builtin_namespace);
}

bool pop_context(void) {
    if (context_stack.p > 1 + context_stack.bottom) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(Obj(c->normal));
        val_destroy(Obj(cheap_context));
        cheap_context = c->cheap;
        c = &context_stack.stack[context_stack.p - 1];
        current_context = c->normal;
        return false;
    }
    return true;
}

void push_context2(Namespace *name) {
    if (context_stack.p >= context_stack.len) {
        context_stack.len += 8;
        if (/*context_stack.len < 8 ||*/ context_stack.len > SIZE_MAX / sizeof *context_stack.stack) err_msg_out_of_memory(); /* overflow */
        context_stack.stack = (struct cstack_s *)reallocx(context_stack.stack, context_stack.len * sizeof *context_stack.stack);
    }
    context_stack.stack[context_stack.p].normal = context_stack.stack[context_stack.p - 1].normal;
    context_stack.stack[context_stack.p - 1].normal = ref_namespace(name);
    context_stack.stack[context_stack.p].cheap = ref_namespace(name);
    context_stack.p++;
}

bool pop_context2(void) {
    if (context_stack.p > 1 + context_stack.bottom) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(Obj(context_stack.stack[context_stack.p - 1].normal));
        context_stack.stack[context_stack.p - 1].normal = c->normal;
        val_destroy(Obj(c->cheap));
        return false;
    }
    return true;
}

void reset_context(void) {
    context_stack.bottom = 0;
    while (context_stack.p != 0) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(Obj(c->normal));
        val_destroy(Obj(c->cheap));
    }
    val_destroy(Obj(cheap_context));
    cheap_context = ref_namespace(root_namespace);
    push_context(root_namespace);
    root_namespace->backr = root_namespace->forwr = 0;
}

struct label_stack_s {
    Label **stack;
    size_t len, p;
};

static struct label_stack_s label_stack;

static void push_label(Label *name) {
    if (label_stack.p >= label_stack.len) {
        label_stack.len += 8;
        if (/*label_stack.len < 8 ||*/ label_stack.len > SIZE_MAX / sizeof(*label_stack.stack)) err_msg_out_of_memory(); /* overflow */
        label_stack.stack = (Label **)reallocx(label_stack.stack, label_stack.len * sizeof(*label_stack.stack));
    }
    label_stack.stack[label_stack.p] = name;
    label_stack.p++;
}

static void pop_label(void) {
    label_stack.p--;
}

void get_namespaces(Mfunc *mfunc) {
    size_t i, len = context_stack.p - context_stack.bottom;
    if (len > SIZE_MAX / sizeof *mfunc->namespaces) err_msg_out_of_memory(); /* overflow */
    mfunc->nslen = len;
    mfunc->namespaces = (Namespace **)mallocx(len * sizeof *mfunc->namespaces);
    for (i = 0; i < len; i++) {
        mfunc->namespaces[i] = ref_namespace(context_stack.stack[context_stack.bottom + i].normal);
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

static Label *namespace_update(Namespace *ns, Label *p) {
    size_t mask, hash, offs;
    if (ns->len * 3 / 2 >= ns->mask) {
        size_t i, max = (ns->data == NULL) ? 8 : (ns->mask + 1) << 1;
        Label **n = (Label **)calloc(max, sizeof *n);
        if (n == NULL) err_msg_out_of_memory();
        mask = max - 1;
        if (ns->data != NULL) {
            for (i = 0; i <= ns->mask; i++) if (ns->data[i] != NULL) {
                hash = (size_t)ns->data[i]->hash;
                offs = hash & mask;
                while (n[offs] != NULL) {
                    hash >>= 5;
                    offs = (5 * offs + hash + 1) & mask;
                }
                n[offs] = ns->data[i];
            }
            free(ns->data);
        }
        ns->mask = mask;
        ns->data = n;
    }
    mask = ns->mask;
    hash = (size_t)p->hash;
    offs = hash & mask;
    while (ns->data[offs] != NULL) {
        Label *d = ns->data[offs];
        if (p->hash == d->hash && p->strength == d->strength) {
            const str_t *s1 = &p->cfname;
            const str_t *s2 = &d->cfname;
            if (s1->len == s2->len && (s1->data == s2->data || memcmp(s1->data, s2->data, s1->len) == 0)) {
                return d;
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    } 
    ns->data[offs] = p;
    ns->len++;
    return NULL;
}

static Label *namespace_lookup(const Namespace *ns, const Label *p) {
    Label *ret = NULL;
    size_t mask = ns->mask;
    size_t hash = (size_t)p->hash;
    size_t offs = hash & mask;
    if (ns->data == NULL) return ret;
    while (ns->data[offs] != NULL) {
        Label *d = ns->data[offs];
        if (p->hash == d->hash) {
            if (d->defpass == pass || (d->constant && (!fixeddig || d->defpass == pass - 1))) {
                const str_t *s1 = &p->cfname;
                const str_t *s2 = &d->cfname;
                if (s1->len == s2->len && (s1->data == s2->data || memcmp(s1->data, s2->data, s1->len) == 0)) {
                    if (d->strength == 0) { ret = d; break; }
                    if (ret == NULL || d->strength < ret->strength) ret = d;
                }
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    }
    if (ret != NULL && ret->constant && ret->defpass == pass - 1 && ret->fwpass != pass) {
        ret->fwpass = pass;
        fwcount++;
    }
    return ret;
}

static Label *namespace_lookup2(const Label *p) {
    const Namespace *ns = builtin_namespace;
    size_t mask = ns->mask;
    size_t hash = (size_t)p->hash;
    size_t offs = hash & mask;
    while (ns->data[offs] != NULL) {
        Label *d = ns->data[offs];
        if (p->hash == d->hash) {
            const str_t *s1 = &p->cfname;
            const str_t *s2 = &d->cfname;
            if (s1->len == s2->len && memcmp(s1->data, s2->data, s1->len) == 0) {
                return d;
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    }
    return NULL;
}

static Label *namespace_lookup3(const Namespace *ns, const Label *p) {
    size_t mask = ns->mask;
    size_t hash = (size_t)p->hash;
    size_t offs = hash & mask;
    if (ns->data == NULL) return NULL;
    while (ns->data[offs] != NULL) {
        Label *d = ns->data[offs];
        if (p->hash == d->hash && p->strength == d->strength) {
            const str_t *s1 = &p->cfname;
            const str_t *s2 = &d->cfname;
            if (s1->len == s2->len && (s1->data == s2->data || memcmp(s1->data, s2->data, s1->len) == 0)) {
                return d;
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    }
    return NULL;
}

Label *find_label(const str_t *name, Namespace **here) {
    size_t p = context_stack.p;
    Label label, *c;

    str_cfcpy(&label.cfname, name);
    label.hash = str_hash(&label.cfname);

    while (context_stack.bottom < p) {
        Namespace *context = context_stack.stack[--p].normal;
        Label *key2 = namespace_lookup(context, &label);
        if (key2 != NULL) {
            if (here != NULL) *here = context;
            if (!diagnostics.shadow || !fixeddig || constcreated || (here != NULL && *here == context)) {
                return key2;
            }
            while (context_stack.bottom < p) {
                Label *key1 = namespace_lookup(context_stack.stack[--p].normal, &label);
                if (key1 != NULL) {
                    Obj *o1 = key1->value;
                    Obj *o2 = key2->value;
                    if (o1 != o2 && !o1->obj->same(o1, o2)) {
                        err_msg_shadow_defined(key1, key2);
                        return key2;
                    }
                }
            }
            c = namespace_lookup2(&label);
            if (c != NULL) {
                Obj *o1 = c->value;
                Obj *o2 = key2->value;
                if (o1 != o2 && !o1->obj->same(o1, o2)) {
                    err_msg_shadow_defined2(key2);
                }
            }
            return key2;
        }
    }
    c = namespace_lookup2(&label);
    if (here != NULL) *here = (c != NULL) ? builtin_namespace : NULL;
    return c;
}

Label *find_label2(const str_t *name, Namespace *context) {
    Label label;

    str_cfcpy(&label.cfname, name);
    label.hash = str_hash(&label.cfname);

    return namespace_lookup(context, &label);
}

struct anonsymbol_s {
    uint8_t dir, pad;
    uint8_t count[sizeof(uint32_t)];
};

Label *find_label3(const str_t *name, Namespace *context, uint8_t strength) {
    Label label;

    label.strength = strength;
    if (name->len > 1 && name->data[1] == 0) label.cfname = *name;
    else str_cfcpy(&label.cfname, name);
    label.hash = str_hash(&label.cfname);

    return namespace_lookup3(context, &label);
}

Label *find_anonlabel(ssize_t count) {
    size_t p = context_stack.p;
    Namespace *context;
    Label label, *c;
    struct anonsymbol_s anonsymbol;

    anonsymbol.dir = (count >= 0) ? '+' : '-';
    anonsymbol.pad = 0;

    label.cfname.data = (const uint8_t *)&anonsymbol;

    while (context_stack.bottom < p) {
        uint32_t count2;
        context = context_stack.stack[--p].normal;
        if (count < 0) {
            if (context->backr < (size_t)-count) continue;
            count2 = context->backr - (uint32_t)-count;
        } else {
            count2 = context->forwr + (uint32_t)count;
            if (count2 < (size_t)count) continue;
        }
        label.cfname.len = 2;
        while (count2 != 0) {
            anonsymbol.count[label.cfname.len - 2] = (uint8_t)count2;
            label.cfname.len++;
            count2 >>= 8;
        } 

        label.hash = str_hash(&label.cfname);
        c = namespace_lookup(context, &label);
        if (c != NULL) return c;
    }
    return NULL;
}

Label *find_anonlabel2(ssize_t count, Namespace *context) {
    Label label;
    uint32_t count2;
    struct anonsymbol_s anonsymbol;

    if (count < 0) {
        if (context->backr < (size_t)-count) return NULL;
        count2 = context->backr - (uint32_t)-count;
        anonsymbol.dir = '-';
    } else {
        count2 = context->forwr + (uint32_t)count;
        if (count2 < (size_t)count) return NULL;
        anonsymbol.dir = '+';
    }

    anonsymbol.pad = 0;

    label.cfname.data = (const uint8_t *)&anonsymbol;
    label.cfname.len = 2;
    while (count2 != 0) {
        anonsymbol.count[label.cfname.len - 2] = (uint8_t)count2;
        label.cfname.len++;
        count2 >>= 8;
    } 
    label.hash = str_hash(&label.cfname);

    return namespace_lookup(context, &label);
}

/* --------------------------------------------------------------------------- */
Label *new_label(const str_t *name, Namespace *context, uint8_t strength, bool *exists, const struct file_list_s *cflist) {
    Label *b;
    if (lastlb == NULL) lastlb = Label(val_alloc(LABEL_OBJ));

    if (name->len > 1 && name->data[1] == 0) lastlb->cfname = *name;
    else str_cfcpy(&lastlb->cfname, name);
    lastlb->hash = str_hash(&lastlb->cfname);
    lastlb->strength = strength;

    b = namespace_update(context, lastlb);

    if (b == NULL) { /* new label */
        if (not_in_file(name->data, cflist->file)) str_cpy(&lastlb->name, name);
        else lastlb->name = *name;
        if (lastlb->cfname.data != name->data) str_cfcpy(&lastlb->cfname, NULL);
        else lastlb->cfname = lastlb->name;
        lastlb->file_list = cflist;
        lastlb->ref = false;
        lastlb->update_after = false;
        lastlb->usepass = 0;
        lastlb->fwpass = 0;
        lastlb->defpass = pass;
        *exists = false;
        b = lastlb;
        lastlb = NULL;
        return b;
    }
    *exists = true;
    return b;
}

void label_move(Label *label, const str_t *name, const struct file_list_s *cflist) {
    bool cfsame = (label->cfname.data == label->name.data);
    if (!not_in_file(label->name.data, label->file_list->file)) {
        if (not_in_file(name->data, cflist->file)) str_cpy(&label->name, name);
        else label->name = *name;
    }
    if (cfsame) {
        label->cfname = label->name;
    }
    label->file_list = cflist;
}

void unused_check(Namespace *names) {
    size_t n, ln;

    if (names->len == 0) return;
    ln = names->len; names->len = 0;
    for (n = 0; n <= names->mask; n++) {
        Label *key2 = names->data[n];
        Obj *o;
        Namespace *ns;

        if (key2 == NULL || key2->defpass != pass) continue;

        o  = key2->value;
        if (key2->usepass != pass && (key2->name.data[0] != '.' && key2->name.data[0] != '#')) {
            if (!key2->constant) {
                if (diagnostics.unused.variable) err_msg_unused_variable(key2);
                continue;
            }
            if (!key2->owner) {
                if (diagnostics.unused.consts) err_msg_unused_const(key2);
                continue;
            }
            if (o->obj == CODE_OBJ) {
                if (diagnostics.unused.label) err_msg_unused_label(key2);
                continue;
            }
            if (diagnostics.unused.macro) {
                err_msg_unused_macro(key2);
                continue;
            }
        }
        if (!key2->owner) continue;
        switch (o->obj->type) {
        case T_CODE:
            ns = Code(o)->names;
            break;
        case T_NAMESPACE:
            ns = Namespace(o);
            break;
        case T_MFUNC:
            {
                Mfunc *mfunc = Mfunc(o);
                List *lst = mfunc->inamespaces;
                size_t i;
                for (i = 0; i < lst->len; i++) {
                    ns = Namespace(lst->data[i]);
                    if (ns->len != 0) unused_check(ns);
                }
                ns = mfunc->names;
            }
            break;
        default:
            ns = NULL;
            break;
        }
        if (ns != NULL && ns->len != 0) {
            push_context(ns);
            unused_check(ns);
            pop_context();
        }
    }
    names->len = ln;
}

static inline void padding(size_t l, size_t t, FILE *f) {
    if (arguments.tab_size > 1) {
        size_t l2 = l - l % arguments.tab_size;
        while (l2 + arguments.tab_size <= t) { l2 += arguments.tab_size; l = l2; putc('\t', f);}
    }
    while (l < t) { l++; putc(' ', f);}
}

static void labelname_print(const Label *l, FILE *flab, char d) {
    size_t p;
    for (p = 0; p < label_stack.p; p++) {
        printable_print2(label_stack.stack[p]->name.data, flab, label_stack.stack[p]->name.len);
        putc(d, flab);
    }
    printable_print2(l->name.data, flab, l->name.len);
}

static void labelprint2(Namespace *names, FILE *flab, Symbollist_types labelmode) {
    size_t n, ln;

    if (names->len == 0) return;
    ln = names->len; names->len = 0;
    for (n = 0; n <= names->mask; n++) {
        Label *l = names->data[n];
        if (l == NULL || l->name.data == NULL) continue;
        if (l->name.len > 1 && l->name.data[1] == 0) continue;
        switch (l->value->obj->type) {
        case T_LBL:
        case T_MACRO:
        case T_SEGMENT:
        case T_UNION:
        case T_STRUCT: continue;
        case T_CODE:
            if (Code(l->value)->pass != Code(l->value)->apass) continue;
            break;
        default:break;
        }
        if (l != namespace_lookup(names, l)) continue;
        if (labelmode == LABEL_VICE || labelmode == LABEL_VICE_NUMERIC) {
            Obj *val;
            size_t i, j = l->name.len;
            const uint8_t *d = l->name.data;

            if (!l->constant) continue;
            for (i = 0; i < j; i++) {
                uint8_t c = d[i];
                if (c < '0') break;
                if (c <= '9') continue;
                if (c == '_') continue;
                c |= 0x20;
                if (c < 'a') break;
                if (c <= 'z') continue;
                break;
            }
            if (i != j) continue;

            val = l->value;
            if (val->obj == ADDRESS_OBJ || val->obj == CODE_OBJ || (labelmode == LABEL_VICE_NUMERIC && (val->obj == BITS_OBJ || val->obj == INT_OBJ))) {
                struct linepos_s epoint;
                uval_t uv;
                Error *err = val->obj->uval(val, &uv, 24, &epoint);
                if (err == NULL) {
                    fprintf(flab, "al %" PRIx32 " .", uv & 0xffffff);
                    labelname_print(l, flab, ':');
                    putc('\n', flab);
                } else val_destroy(Obj(err));
            }
            if (l->owner) {
                Namespace *ns = get_namespace(val);
                if (ns != NULL && ns->len != 0) {
                    push_label(l);
                    labelprint2(ns, flab, labelmode);
                    pop_label();
                }
            }
        } else {
            Obj *val = l->value;
            val = val->obj->repr(val, NULL, SIZE_MAX);
            if (val == NULL) continue;
            if (val->obj == STR_OBJ) {
                const Str *str = Str(val);
                size_t len = printable_print2(l->name.data, flab, l->name.len);
                padding(len, EQUAL_COLUMN, flab);
                if (l->constant) fputs("= ", flab);
                else fputs(&" := "[len < EQUAL_COLUMN], flab);
                printable_print2(str->data, flab, str->len);
                putc('\n', flab);
            }
            val_destroy(val);
        }
    }
    names->len = ln;
}

static inline const uint8_t *get_line(const struct file_s *file, linenum_t line) {
    return &file->data[file->line[line - 1]];
}

static void labeldump(Namespace *names, FILE *flab) {
    size_t n, ln;

    if (names->len == 0) return;
    ln = names->len; names->len = 0;
    for (n = 0; n <= names->mask; n++) {
        Label *l2 = names->data[n];
        Namespace *ns;

        if (l2 == NULL) continue;
        if (l2->name.len < 2 || l2->name.data[1] != 0) {
            Obj *val = l2->value;
            val = val->obj->repr(val, NULL, SIZE_MAX);
            if (val != NULL) {
                if (val->obj == STR_OBJ) {
                    const Str *str = Str(val);
                    const struct file_s *file = l2->file_list->file;
                    linepos_t epoint = &l2->epoint;
                    printable_print((const uint8_t *)file->realname, flab);
                    fprintf(flab, ":%" PRIuline ":%" PRIlinepos ": ", epoint->line, ((file->encoding == E_UTF8) ? (linecpos_t)calcpos(get_line(file, epoint->line), epoint->pos) : epoint->pos) + 1);
                    labelname_print(l2, flab, '.');
                    fputs(l2->constant ? " = " : " := ", flab);
                    printable_print2(str->data, flab, str->len);
                    putc('\n', flab);
                }
                val_destroy(val);
            }
        }
        if (!l2->owner) continue;

        ns = get_namespace(l2->value);

        if (ns != NULL && ns->len != 0) {
            if (l2->name.len < 2 || l2->name.data[1] != 0) {
                push_label(l2);
                labeldump(ns, flab);
                pop_label();
            }
        }
    }
    names->len = ln;
}

static Namespace *find_space(const char *here, bool use) {
    Namespace *space;
    str_t labelname;
    Label *l;

    space = root_namespace;
    if (here == NULL) return space;

    pline = (const uint8_t *)here;
    lpoint.pos = 0;
    do {
        labelname.data = pline + lpoint.pos; labelname.len = get_label(labelname.data);
        if (labelname.len == 0) return NULL;
        lpoint.pos += (linecpos_t)labelname.len;
        l = find_label2(&labelname, space);
        if (l == NULL) return NULL;
        space = get_namespace(l->value);
        if (space == NULL) return NULL;
        lpoint.pos++;
    } while (labelname.data[labelname.len] == '.');

    if (labelname.data[labelname.len] != 0) return NULL;

    if (use) {
        l->usepass = pass;
        l->ref = true;
    }
    return space;
}

void labelprint(const struct symbol_output_s *output) {
    FILE *flab;
    struct linepos_s nopoint = {0, 0};
    int err;
    Namespace *space;

    flab = dash_name(output->name) ? stdout : file_open(output->name, output->append ? "at" : "wt");
    if (flab == NULL) {
        err_msg_file(ERROR_CANT_WRTE_LBL, output->name, &nopoint);
        return;
    }
    clearerr(flab); errno = 0;
    label_stack.stack = NULL;
    label_stack.p = label_stack.len = 0;
    space = find_space(output->space, false);
    if (space == NULL) {
        str_t labelname;
        labelname.data = pline;
        labelname.len = lpoint.pos;
        err_msg2(ERROR____LABEL_ROOT, &labelname, &nopoint);
    } else if (output->mode == LABEL_DUMP) {
        labeldump(space, flab);
    } else {
        labelprint2(space, flab, output->mode);
    }
    free(label_stack.stack);
    err = ferror(flab);
    err |= (flab != stdout) ? fclose(flab) : fflush(flab);
    if (err != 0 && errno != 0) {
        err_msg_file(ERROR_CANT_WRTE_LBL, output->name, &nopoint);
    }
}

void ref_labels(void) {
    size_t j;
    for (j = 0; j < arguments.symbol_output_len; j++) {
        const struct symbol_output_s *output = &arguments.symbol_output[j];
        Namespace *space;
        size_t n;

        if (output->mode != LABEL_EXPORT) continue;
        space = find_space(output->space, true);
        if (space == NULL || space->len == 0) continue;

        for (n = 0; n <= space->mask; n++) {
            Label *l = space->data[n];
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
            if (l != namespace_lookup(space, l)) continue;
            if (l->value->obj == ERROR_OBJ) err_msg_output(Error(l->value));
            l->ref = true;
            l->usepass = pass;
        }
    }
}

void new_builtin(const char *symbol, Obj *val) {
    struct linepos_s nopoint = {0, 0};
    str_t name;
    Label *label;
    bool label_exists;
    name.len = strlen(symbol);
    name.data = (const uint8_t *)symbol;
    label = new_label(&name, builtin_namespace, 0, &label_exists, dummy_file_list);
    label->constant = true;
    label->owner = true;
    label->value = val;
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
    namespaceobj_names();
}

void destroy_lastlb(void) {
    if (lastlb != NULL) {
        lastlb->v.obj = NONE_OBJ;
        val_destroy(Obj(lastlb));
        lastlb = NULL;
    }
}

void destroy_variables(void)
{
    val_destroy(Obj(builtin_namespace));
    val_destroy(Obj(root_namespace));
    val_destroy(Obj(cheap_context));
    destroy_lastlb();
    while (context_stack.p != 0) {
        struct cstack_s *c = &context_stack.stack[--context_stack.p];
        val_destroy(Obj(c->normal));
        val_destroy(Obj(c->cheap));
    }
    free(context_stack.stack);
}
