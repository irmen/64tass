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
#include "macro.h"
#include "misc.h"
#include "file.h"
#include "eval.h"
#include "section.h"
#include "variables.h"
#include "64tass.h"
#include "listing.h"

struct macro_pline_s {
    size_t len;
    uint8_t *data;
};

struct macro_params_s {
    size_t len, size;
    str_t *param, all;
    struct macro_pline_s pline;
    value_t macro;
};

static struct {
    size_t p, len;
    struct macro_params_s *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

int in_macro(void) {
    return !!macro_parameters.p;
}

/* ------------------------------------------------------------------------------ */
int mtranslate(struct file_s *cfile)
{
    uint_fast8_t q;
    uint_fast16_t j;
    size_t p;
    uint8_t ch;
    struct macro_pline_s *mline;

    if (lpoint.line >= cfile->lines) return 1;
    llist = pline = &cfile->data[cfile->line[lpoint.line]]; lpoint.pos = 0; lpoint.line++;vline++;
    if (!macro_parameters.p) return 0;
    mline = &macro_parameters.current->pline;

    q=p=0;
    for (; (ch = here()); lpoint.pos++) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        else if ((ch == ';') && (!q)) { q=4; }
        else if ((ch=='\\') && (!q)) {
            /* normal parameter reference */
            if ((ch=pline[lpoint.pos+1]) >= '1' && ch <= '9') {
                str_t *param = macro_parameters.current->param;
                /* \1..\9 */
                if ((j=ch-'1') >= macro_parameters.current->len || !param[j].data) {
                    obj_t obj = macro_parameters.current->macro->obj;
                    if (obj == STRUCT_OBJ || obj == UNION_OBJ) {
                        lpoint.pos++;
                        ch = '?';
                        goto ok;
                    }
                    err_msg(ERROR_MISSING_ARGUM,NULL);
                    break;
                }
                if (p + param[j].len > mline->len) {
                    mline->len += param[j].len + 1024;
                    mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
                    if (!mline->data || mline->len < 1024) err_msg_out_of_memory();
                }
                memcpy((char *)mline->data + p, param[j].data, param[j].len);
                p += param[j].len;
                lpoint.pos++;continue;
            } else if (ch=='@') {
                /* \@ gives complete parameter list */
                str_t *all = &macro_parameters.current->all;
                if (p + all->len > mline->len) {
                    mline->len += all->len + 1024;
                    mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
                    if (!mline->data || mline->len < 1024) err_msg_out_of_memory();
                }
                memcpy((char *)mline->data + p, all->data, all->len);
                p += all->len;
                lpoint.pos++;continue;
            } else {
                struct linepos_s e = lpoint;
                str_t label;
                lpoint.pos++;
                label.data = pline + lpoint.pos;
                if (ch == '{') {
                    lpoint.pos++;
                    label.data++;
                    label.len = get_label();
                    if (pline[lpoint.pos] == '}') lpoint.pos++;
                    else label.len = 0;
                } else label.len = get_label();
                if (label.len) {
                    str_t *param = macro_parameters.current->param;
                    value_t macro = macro_parameters.current->macro;
                    str_t cf;
                    str_cfcpy(&cf, &label);
                    for (j = 0; j < macro->u.macro.argc; j++) {
                        if (!macro->u.macro.param[j].cfname.data) continue;
                        if (str_cmp(&macro->u.macro.param[j].cfname, &cf)) continue;
                        if (!param[j].data) {
                            obj_t obj = macro->obj;
                            if (obj == STRUCT_OBJ || obj == UNION_OBJ) {
                                lpoint.pos--;
                                ch = '?';
                                goto ok;
                            }
                            err_msg2(ERROR_MISSING_ARGUM, NULL, &e);
                            break;
                        }
                        if (p + param[j].len > mline->len) {
                            mline->len += param[j].len + 1024;
                            mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
                            if (!mline->data || mline->len < 1024) err_msg_out_of_memory();
                        }
                        memcpy((char *)mline->data + p, param[j].data, param[j].len);
                        p += param[j].len;
                        break;
                    }
                    if (j < macro->u.macro.argc) {
                        lpoint.pos--;
                        continue;
                    }
                    err_msg2(ERROR_MISSING_ARGUM, NULL, &e);
                }
                ch='\\';lpoint = e;
            }
        } else if (ch=='@' && arguments.tasmcomp) {
            /* text parameter reference */
            if ((ch=pline[lpoint.pos+1])>='1' && ch<='9') {
                /* @1..@9 */
                str_t *param = macro_parameters.current->param;
                if ((j=ch-'1') >= macro_parameters.current->len) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                if (p + param[j].len > mline->len) {
                    mline->len += param[j].len + 1024;
                    mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
                    if (!mline->data || mline->len < 1024) err_msg_out_of_memory();
                }
                if (param[j].len > 1 && param[j].data[0] == '"' && param[j].data[param[j].len-1]=='"') {
                    memcpy((char *)mline->data + p, param[j].data + 1, param[j].len - 2);
                    p += param[j].len - 2;
                } else {
                    memcpy((char *)mline->data + p, param[j].data, param[j].len);
                    p += param[j].len;
                }
                lpoint.pos++;continue;
            } else ch='@';
        }
    ok:
        if (p + 1 > mline->len) {
            mline->len += 1024;
            mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
            if (!mline->data || mline->len < 1024) err_msg_out_of_memory(); /* overflow */
        }
        mline->data[p++]=ch;
    }
    if (p + 1 > mline->len) {
        mline->len += 1024;
        mline->data = (uint8_t *)realloc((char *)mline->data, mline->len);
        if (!mline->data || mline->len < 1024) err_msg_out_of_memory(); /* overflow */
    }
    while (p && (mline->data[p-1] == ' ' || mline->data[p-1] == ' ')) p--;
    mline->data[p]=0;
    llist = pline = mline->data; lpoint.pos = 0;
    return 0;
}

static size_t macro_param_find(void) {
    uint_fast8_t q = 0, ch;
    uint8_t pp = 0;
    char par[256];

    struct linepos_s opoint2, npoint2;
    opoint2.pos = lpoint.pos;
    while ((ch=here()) && (q || (ch!=';' && (ch!=',' || pp)))) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        if (!q) {
            if (ch == '(' || ch == '[' || ch == '{') par[pp++]=ch;
            else if (pp && ((ch == ')' && par[pp-1]=='(') || (ch == ']' && par[pp-1]=='[') || (ch == '}' && par[pp-1]=='{'))) pp--;
        }
        lpoint.pos++;
    }
    npoint2.pos = lpoint.pos;
    while (npoint2.pos > opoint2.pos && (pline[npoint2.pos-1] == 0x20 || pline[npoint2.pos-1] == 0x09)) npoint2.pos--;
    return npoint2.pos - opoint2.pos;
}

value_t macro_recurse(enum wait_e t, value_t tmp2, value_t context, linepos_t epoint) {
    value_t val;
    struct macro_params_s *params = macro_parameters.params;
    if (macro_parameters.p>100) {
        err_msg2(ERROR__MACRECURSION, NULL, epoint);
        return NULL;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        macro_parameters.len += 1;
        params = (struct macro_params_s *)realloc(params, sizeof(params[0]) * macro_parameters.len);
        if (!params || macro_parameters.len < 1 || macro_parameters.len > SIZE_MAX / sizeof(params[0])) err_msg_out_of_memory();
        macro_parameters.params = params;
        macro_parameters.current = &params[macro_parameters.p];
        macro_parameters.current->param = NULL;
        macro_parameters.current->size = 0;
        macro_parameters.current->pline.len = 0;
        macro_parameters.current->pline.data = NULL;
    }
    macro_parameters.current = &params[macro_parameters.p];
    macro_parameters.current->macro = val_reference(tmp2);
    macro_parameters.p++;
    {
        struct linepos_s opoint, npoint;
        size_t p = 0;
        str_t *param = macro_parameters.current->param;

        ignore(); opoint = lpoint;
        for (;;) {
            if ((!here() || here()==';') && p >= tmp2->u.macro.argc) break;
            if (p >= macro_parameters.current->size) {
                if (macro_parameters.current->size < tmp2->u.macro.argc) macro_parameters.current->size = tmp2->u.macro.argc;
                else macro_parameters.current->size += 4;
                param = (str_t *)realloc(param, sizeof(param[0]) * macro_parameters.current->size);
                if (!param || macro_parameters.current->size > SIZE_MAX / sizeof(param[0])) err_msg_out_of_memory();
            }
            param[p].data = pline + lpoint.pos;
            param[p].len = macro_param_find();
            if (!param[p].len) {
                if (p < tmp2->u.macro.argc) {
                    param[p] = tmp2->u.macro.param[p].init;
                } else param[p].data = NULL;
            }
            p++;
            if (!here() || here()==';') {
                if (p < tmp2->u.macro.argc) continue;
            }
            if (here() != ',') break;
            lpoint.pos++;
            ignore();
        }
        macro_parameters.current->param = param;
        macro_parameters.current->len = p;
        macro_parameters.current->all.data = pline + opoint.pos;
        npoint = lpoint;
        while (npoint.pos > opoint.pos && (pline[npoint.pos-1] == 0x20 || pline[npoint.pos-1] == 0x09)) npoint.pos--;
        macro_parameters.current->all.len = npoint.pos - opoint.pos;
    }
    if (t == W_ENDS) {
        value_t oldcheap;
        if (context) {oldcheap = cheap_context; push_context(context); cheap_context = current_context;}
        val = compile(tmp2->u.macro.file_list);
        if (context) {pop_context(); cheap_context = oldcheap;}
    } else {
        line_t lin = lpoint.line;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        value_t oldcheap;
        struct file_list_s *cflist;
        struct linepos_s nopoint = {0, 0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = 0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        cflist = enterfile(tmp2->u.macro.file_list->file, epoint);
        lpoint.line = tmp2->u.macro.line;
        new_waitfor(t, &nopoint);
        if (context) {oldcheap = cheap_context;push_context(context); cheap_context = current_context;}
        val = compile(cflist);
        if (context) {pop_context(); cheap_context = oldcheap;}
        star = s->addr;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    val_destroy(macro_parameters.current->macro);
    macro_parameters.p--;
    if (macro_parameters.p) macro_parameters.current = &params[macro_parameters.p - 1];
    return val;
}

value_t mfunc_recurse(enum wait_e t, value_t tmp2, value_t context, linepos_t epoint, uint8_t strength) {
    size_t i;
    value_t label;
    value_t val, tuple = NULL;
    size_t max = 0, args = get_val_remaining();

    for (i = 0; i < tmp2->u.mfunc.argc; i++) {
        int labelexists;
        if (tmp2->u.mfunc.param[i].init && tmp2->u.mfunc.param[i].init->obj == DEFAULT_OBJ) {
            size_t j = 0;
            tuple = val_alloc(TUPLE_OBJ);
            tuple->u.list.len = get_val_remaining();
            tuple->u.list.data = list_create_elements(tuple, tuple->u.list.len);
            for (j = 0; (val = pull_val(NULL)); j++) {
                if (val->obj == ERROR_OBJ) {err_msg_output_and_destroy(val); val = val_reference(none_value);}
                tuple->u.list.data[j] = val;
            }
            val = tuple;
        } else {
            struct values_s *vs;
            vs = get_val();
            if (!vs) {
                val = tmp2->u.mfunc.param[i].init;
                if (!val) { max = i + 1; val = none_value; }
            } else {
                val = vs->val;
                if (val->obj == ERROR_OBJ) {err_msg_output(val); val = none_value;}
            }
        }
        label = new_label(&tmp2->u.mfunc.param[i].name, context, strength, &labelexists);
        label->u.label.ref=0;
        if (labelexists) {
            if (label->u.label.defpass == pass) err_msg_double_defined(label, &tmp2->u.mfunc.param[i].name, &tmp2->u.mfunc.param[i].epoint); /* not possible in theory */
            else {
                label->u.label.constant = 1;
                var_assign(label, val, 0);
            }
        } else {
            label->u.label.constant = 1;
            label->u.label.value = val_reference(val);
            label->u.label.file_list = tmp2->u.mfunc.file_list;
            label->u.label.epoint = tmp2->u.mfunc.param[i].epoint;
        }
    }
    if (tuple) val_destroy(tuple);
    else if (i < args) err_msg_argnum(args, i, i, epoint);
    if (max) err_msg_argnum(args, max, tmp2->u.mfunc.argc, epoint);
    {
        line_t lin = lpoint.line;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        value_t oldcheap = cheap_context;
        struct file_list_s *cflist;
        struct linepos_s nopoint = {0, 0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = 0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        cflist = enterfile(tmp2->u.mfunc.file_list->file, epoint);
        lpoint.line = tmp2->u.mfunc.line;
        new_waitfor(t, &nopoint);
        push_context(context); cheap_context = current_context;
        val = compile(cflist);
        pop_context();star = s->addr;
        cheap_context = oldcheap;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    return val;
}

void get_func_params(value_t v, struct file_s *cfile) {
    mfunc_t new_mfunc;
    size_t len = 0, i, j;
    str_t label;
    int w, stard = 0;

    new_mfunc.param = NULL;
    for (i = 0;;i++) {
        ignore();if (!here() || here() == ';') break;
        if (here()=='*') {
            stard = 1;
            lpoint.pos++;ignore();
        }
        if (i >= len) {
            len += 16;
            new_mfunc.param = (struct mfunc_param_s *)realloc(new_mfunc.param, len * sizeof(new_mfunc.param[0]));
            if (!new_mfunc.param || len < 16 || len > SIZE_MAX / sizeof(new_mfunc.param[0])) err_msg_out_of_memory(); /* overflow */
        }
        new_mfunc.param[i].epoint = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len) {
            str_t cf;
            if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &label, &new_mfunc.param[i].epoint);break;}
            str_cpy(&new_mfunc.param[i].name, &label);
            str_cfcpy(&cf, &new_mfunc.param[i].name);
            if (cf.data != new_mfunc.param[i].name.data) str_cfcpy(&cf, NULL);
            new_mfunc.param[i].cfname = cf;
            for (j = 0; j < i; j++) if (new_mfunc.param[j].name.data) {
                if (!str_cmp(&new_mfunc.param[j].cfname, &cf)) break;
            }
            if (j != i) {
                value_t tmp = val_alloc(LABEL_OBJ);
                tmp->u.label.name = tmp->u.label.cfname = new_mfunc.param[j].name;
                tmp->u.label.file_list = v->u.mfunc.file_list;
                tmp->u.label.epoint = new_mfunc.param[j].epoint;
                tmp->u.label.value = val_reference(none_value);
                err_msg_double_defined(tmp, &label, &new_mfunc.param[i].epoint);
                val_destroy(tmp);
            }
        } else {err_msg2(ERROR_GENERL_SYNTAX, NULL, &new_mfunc.param[i].epoint);break;}
        ignore();
        if (stard) {
            new_mfunc.param[i].init = val_reference(default_value);
        } else {
            new_mfunc.param[i].init = NULL;
            if (here() == '=') {
                value_t val;
                lpoint.pos++;
                if (!get_exp(&w, 1, cfile, 1, 1, &lpoint)) {
                    i++;
                    break;
                }
                val = pull_val(NULL);
                if (val->obj == ERROR_OBJ) {err_msg_output_and_destroy(val); val = val_reference(none_value);}
                new_mfunc.param[i].init = val;
            }
        }
        if (!here() || here() == ';') {
            i++;
            break;
        }
        if (here()!=',') {
            err_msg2(ERROR______EXPECTED, ",", &lpoint);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i) {
            new_mfunc.param = (struct mfunc_param_s *)realloc(new_mfunc.param, i * sizeof(new_mfunc.param[0]));
            if (!new_mfunc.param || i > SIZE_MAX / sizeof(new_mfunc.param[0])) err_msg_out_of_memory(); /* overflow */
        } else {
            free(new_mfunc.param);
            new_mfunc.param = NULL;
        }
    }
    v->u.mfunc.argc = i;
    v->u.mfunc.param = new_mfunc.param;
}

void get_macro_params(value_t v) {
    macro_t new_macro;
    size_t len = 0, i, j;
    str_t label;
    struct linepos_s *epoints = NULL;

    new_macro.param = NULL;
    for (i = 0;;i++) {
        ignore();if (!here() || here() == ';') break;
        if (i >= len) {
            len += 16;
            new_macro.param = (struct macro_param_s *)realloc(new_macro.param, len * sizeof(new_macro.param[0]));
            if (!new_macro.param || len < 16 || len > SIZE_MAX / sizeof(new_macro.param[0])) err_msg_out_of_memory(); /* overflow */
            epoints = (struct linepos_s *)realloc(epoints, len * sizeof(epoints[0]));
            if (!epoints || len > SIZE_MAX / sizeof(epoints[0])) err_msg_out_of_memory(); /* overflow */
        }
        epoints[i] = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len) {
            str_t cf;
            if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &label, &epoints[i]);new_macro.param[i].cfname.len = 0; new_macro.param[i].cfname.data = NULL;}
            str_cfcpy(&cf, &label);
            if (cf.data == label.data) str_cpy(&new_macro.param[i].cfname, &label);
            else {str_cfcpy(&cf, NULL); new_macro.param[i].cfname = cf;}
            for (j = 0; j < i; j++) if (new_macro.param[j].cfname.data) {
                if (!str_cmp(&new_macro.param[j].cfname, &cf)) break;
            }
            if (j != i) {
                value_t tmp = val_alloc(LABEL_OBJ);
                tmp->u.label.name = tmp->u.label.cfname = new_macro.param[j].cfname;
                tmp->u.label.file_list = v->u.macro.file_list;
                tmp->u.label.epoint = epoints[j];
                tmp->u.label.value = val_reference(none_value);
                err_msg_double_defined(tmp, &label, &epoints[i]);
                val_destroy(tmp);
            }
        } else {new_macro.param[i].cfname.len = 0; new_macro.param[i].cfname.data = NULL;}
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            label.data = pline + lpoint.pos;
            label.len = macro_param_find();
            str_cpy(&new_macro.param[i].init, &label);
        } else {new_macro.param[i].init.len = 0; new_macro.param[i].init.data = NULL;}
        ignore();
        if (!here() || here() == ';') {
            i++;
            break;
        }
        if (here()!=',') {
            err_msg2(ERROR______EXPECTED, ",", &lpoint);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i) {
            new_macro.param = (struct macro_param_s *)realloc(new_macro.param, i * sizeof(new_macro.param[0]));
            if (!new_macro.param || i > SIZE_MAX / sizeof(new_macro.param[0])) err_msg_out_of_memory(); /* overflow */
        } else {
            free(new_macro.param);
            new_macro.param = NULL;
        }
    }
    v->u.macro.argc = i;
    v->u.macro.param = new_macro.param;
    free(epoints);
}

value_t mfunc2_recurse(value_t tmp2, struct values_s *vals, unsigned int args, linepos_t epoint) {
    size_t i;
    value_t label;
    value_t val, tuple;
    value_t retval = NULL;
    value_t context;
    value_t oldcheap = cheap_context;
    struct section_s rsection;
    struct section_s *oldsection = current_section;
    struct file_list_s *cflist;
    struct linepos_s xpoint;

    init_section2(&rsection);

    xpoint.line = tmp2->u.mfunc.line;
    xpoint.pos = 0;
    context = new_labeldict(tmp2->u.mfunc.file_list, &xpoint);

    cflist = enterfile(tmp2->u.mfunc.file_list->file, epoint);
    tuple = NULL;
    for (i = 0; i < tmp2->u.mfunc.argc; i++) {
        int labelexists;
        if (tmp2->u.mfunc.param[i].init && tmp2->u.mfunc.param[i].init->obj == DEFAULT_OBJ) {
            tuple = val_alloc(TUPLE_OBJ);
            if (i < args) {
                size_t j = i;
                tuple->u.list.len = args - i;
                tuple->u.list.data = list_create_elements(tuple, tuple->u.list.len);
                none_value->refcount += args - i;
                while (j < args) {
                    tuple->u.list.data[j - i] = vals[j].val;
                    vals[j].val = none_value;
                    j++;
                }
            } else {
                tuple->u.list.len = 0;
                tuple->u.list.data = NULL;
            }
            val = tuple;
        } else {
            val = (i < args) ? vals[i].val : tmp2->u.mfunc.param[i].init ? tmp2->u.mfunc.param[i].init : none_value;
        }
        label = new_label(&tmp2->u.mfunc.param[i].name, context, 0, &labelexists);
        label->u.label.ref=0;
        if (labelexists) {
            if (label->u.label.defpass == pass) err_msg_double_defined(label, &tmp2->u.mfunc.param[i].name, &tmp2->u.mfunc.param[i].epoint);
            else {
                label->u.label.constant = 1;
                var_assign(label, val, 0);
            }
        } else {
            label->u.label.constant = 1;
            label->u.label.value = val_reference(val);
            label->u.label.file_list = cflist;
            label->u.label.epoint = tmp2->u.mfunc.param[i].epoint;
        }
    }
    if (tuple) val_destroy(tuple);
    else if (i < args) err_msg_argnum(args, i, i, &vals[i].epoint);
    {
        line_t lin = lpoint.line;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        struct linepos_s opoint = lpoint;
        const uint8_t *opline = pline;
        const uint8_t *ollist = llist;
        struct linepos_s nopoint = {0, 0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        lpoint.line = tmp2->u.mfunc.line;
        new_waitfor(W_ENDF2, &nopoint);
        push_context(context); cheap_context = current_context;
        temporary_label_branch++;
        current_section = &rsection;
        reset_section(current_section);
        current_section->provides = oldsection->provides; 
        current_section->requires = oldsection->requires;
        current_section->conflicts = oldsection->conflicts;
        current_section->l_address.address = star & 0xffff; /* TODO */
        current_section->l_address.bank = star & ~0xffff;
        if (current_section->l_address_val) val_destroy(current_section->l_address_val);
        current_section->l_address_val = oldsection->l_address_val ? val_reference(oldsection->l_address_val) : NULL;
        current_section->dooutput = 0;
        retval = compile(cflist);
        current_section = oldsection;
        pop_context(); star = s->addr; cheap_context = oldcheap;
        temporary_label_branch--;
        lpoint = opoint;
        pline = opline;
        llist = ollist;
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    exitfile();
    val_destroy(context);
    destroy_section2(&rsection);
    if (retval) return retval;
    return val_reference(null_tuple);
}

void init_macro(void) {
    macro_parameters.p = 0;
}

void free_macro(void) {
    size_t i;
    for (i = 0; i < macro_parameters.len; i++) {
        free(macro_parameters.params[i].pline.data);
        free(macro_parameters.params[i].param);
    }
    free(macro_parameters.params);
}
