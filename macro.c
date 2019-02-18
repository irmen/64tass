/*
    $Id: macro.c 1891 2019-02-11 20:25:43Z soci $

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
#include "macro.h"
#include <string.h>
#include "file.h"
#include "eval.h"
#include "values.h"
#include "section.h"
#include "variables.h"
#include "64tass.h"
#include "listing.h"
#include "error.h"
#include "arguments.h"
#include "optimizer.h"

#include "listobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "namespaceobj.h"
#include "labelobj.h"
#include "macroobj.h"
#include "mfuncobj.h"
#include "memblocksobj.h"

static int functionrecursion;

struct macro_pline_s {
    size_t len;
    uint8_t *data;
};

struct macro_params_s {
    size_t len, size;
    str_t *param, all;
    struct macro_pline_s pline;
    Obj *macro;
};

static struct {
    size_t p, len;
    struct macro_params_s *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

bool in_macro;

/* ------------------------------------------------------------------------------ */
bool mtranslate(void) {
    unsigned int q;
    size_t j;
    size_t p, p2;
    size_t last, last2;
    struct macro_pline_s *mline;
    bool changed, fault;
    struct file_s *cfile = current_file_list->file;

    if (lpoint.line >= cfile->lines) return true;
    llist = pline = &cfile->data[cfile->line[lpoint.line]];
    changed = !in_macro || (cfile->nomacro != NULL && (cfile->nomacro[lpoint.line / 8] & (1 << (lpoint.line & 7))) != 0);
    lpoint.pos = 0; lpoint.line++; vline++;
    if (changed) return false;
    mline = &macro_parameters.current->pline;

    q = p = p2 = 0; last = last2 = 0; fault = false;
    while (pline[p2] != 0) {
        str_t param;
        switch (pline[p2]) {
        case '"':
            if ((q & 2) == 0) q ^= 1;
            p2++;
            continue;
        case '\'':
            if ((q & 1) == 0) q ^= 2;
            p2++;
            continue;
        case ';':
            if (q == 0) q = 4;
            p2++;
            continue;
        default:
            p2++;
            continue;
        case '\\':
            if (q != 0) {
                p2++;
                continue;
            }
            /* normal parameter reference */
            j = (uint8_t)(pline[p2 + 1] - '1');
            if (j < 9) {   /* \1..\9 */
                p += p2 - last2;
                p2 += 2;
                break;
            }
            if (j == ('@' - '1')) { /* \@ gives complete parameter list */
                j = SIZE_MAX;
                p += p2 - last2;
                p2 += 2;
                break;
            }
            if (j == ('{' - '1')) {
                lpoint.pos = p2 + 2;
                param.data = pline + lpoint.pos;
                param.len = get_label();
                if (pline[lpoint.pos] == '}') lpoint.pos++;
                else param.len = 0;
            } else {
                lpoint.pos = p2 + 1;
                param.data = pline + lpoint.pos;
                param.len = get_label();
            }
            if (param.len != 0) {
                Macro *macro = (Macro *)macro_parameters.current->macro;
                str_t cf;
                p += p2 - last2;
                p2 = lpoint.pos;
                str_cfcpy(&cf, &param);
                for (j = 0; j < macro->argc; j++) {
                    if (macro->param[j].cfname.data == NULL) continue;
                    if (str_cmp(&macro->param[j].cfname, &cf) == 0) break;
                }
                if (j < macro->argc) break;
                lpoint.pos -= param.len + 1;
                if (pline[lpoint.pos] != '\\') lpoint.pos -= 2;
                err_msg_unknown_argument(&param, &lpoint);
                p = last; last2 = p2; fault = true;
                continue;
            }
            p2++;
            continue;
        case '@':
            if (arguments.tasmcomp) {
                /* text parameter reference */
                j = (uint8_t)(pline[p2 + 1] - '1');
                if (j < 9) { /* @1..@9 */
                    p += p2 - last2;
                    p2 += 2;
                    break;
                }
            }
            p2++;
            continue;
        }

        if (j < macro_parameters.current->len) {
            param = macro_parameters.current->param[j];
        } else if (j == SIZE_MAX) {
            param = macro_parameters.current->all;
        } else {
            switch (macro_parameters.current->macro->obj->type) {
            case T_STRUCT:
            case T_UNION:
                param.data = (const uint8_t *)"?";
                param.len = 1;
                break;
            default:
                param.data = NULL;
                param.len = 0;
            }
        } 
        if (p + param.len < p) err_msg_out_of_memory(); /* overflow */
        if (p + param.len > mline->len) {
            mline->len = p + param.len + 1024;
            if (mline->len < 1024) err_msg_out_of_memory(); /* overflow */
            mline->data = (uint8_t *)reallocx(mline->data, mline->len);
        }
        if (last != p) {
            if (p < last) err_msg_out_of_memory(); /* overflow */
            memcpy(mline->data + last, pline + last2, p - last);
        }
        switch (param.len) {
        case 0: 
            if (param.data == NULL) {
                lpoint.pos = last2 + p - last;
                err_msg_missing_argument(current_file_list, &lpoint, j);
                fault = true;
            }
            break;
        case 1: 
            mline->data[p++] = *param.data; 
            break;
        default:
            memcpy(mline->data + p, param.data, param.len);
            p += param.len;
        }
        last = p; last2 = p2;
    }
    if (last2 != 0) {
        while (p2 != 0 && (pline[p2 - 1] == 0x20 || pline[p2 - 1] == 0x09)) p2--;
        p += p2 - last2;
        if (p + 1 < p) err_msg_out_of_memory(); /* overflow */
        if (p + 1 > mline->len) {
            mline->len = p + 1024;
            if (mline->len < 1024) err_msg_out_of_memory(); /* overflow */
            mline->data = (uint8_t *)reallocx(mline->data, mline->len);
        }
        if (p != last) memcpy(mline->data + last, pline + last2, p - last);
        mline->data[p] = 0;
        llist = pline = fault ? (const uint8_t *)"" : mline->data;
    } else {
        line_t lnum;
        if (cfile->nomacro == NULL) {
            cfile->nomacro = (uint8_t *)calloc((cfile->lines + 7) / 8, sizeof *cfile->nomacro);
            if (cfile->nomacro == NULL) err_msg_out_of_memory();
        }
        lnum = lpoint.line - 1;
        cfile->nomacro[lnum / 8] |= 1 << (lnum & 7);
    }
    lpoint.pos = 0;
    return false;
}

static size_t macro_param_find(void) {
    uint8_t q = 0, ch;
    size_t pp = 0, pl = 64;
    uint8_t pbuf[64];
    uint8_t *par = pbuf;
    struct linepos_s opoint2, npoint2;

    opoint2.pos = lpoint.pos;
    while ((ch = here()) != 0 && (q != 0 || (ch != ';' && (ch != ',' || pp != 0)))) {
        if (ch == '"'  && (q & 2) == 0) { q ^= 1; }
        else if (ch == '\'' && (q & 1) == 0) { q ^= 2; }
        if (q == 0) {
            if (ch == '(' || ch == '[' || ch == '{') {
                if (pp >= pl) {
                    pl += 256;
                    if (pl < 256) err_msg_out_of_memory();
                    if (par == pbuf) {
                        par = (uint8_t *)mallocx(pl);
                        memcpy(par, pbuf, pp);
                    } else par = (uint8_t *)reallocx(par, pl);
                }
                par[pp++] = ch;
            } else if (pp != 0 && ((ch == ')' && par[pp-1]=='(') || (ch == ']' && par[pp-1]=='[') || (ch == '}' && par[pp-1]=='{'))) pp--;
        }
        lpoint.pos++;
    }
    npoint2.pos = lpoint.pos;
    while (npoint2.pos > opoint2.pos && (pline[npoint2.pos-1] == 0x20 || pline[npoint2.pos-1] == 0x09)) npoint2.pos--;
    if (par != pbuf) free(par);
    return npoint2.pos - opoint2.pos;
}

Obj *macro_recurse(Wait_types t, Obj *tmp2, Namespace *context, linepos_t epoint) {
    bool in_macro_old;
    Obj *val;
    Macro *macro = (Macro *)tmp2;
    if (macro->recursion_pass == pass) return NULL;
    if (macro_parameters.p > 100) {
        macro->recursion_pass = pass;
        err_msg2(ERROR__MACRECURSION, NULL, epoint);
        return NULL;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        struct macro_params_s *params = macro_parameters.params;
        macro_parameters.len += 1;
        if (/*macro_parameters.len < 1 ||*/ macro_parameters.len > SIZE_MAX / sizeof *params) err_msg_out_of_memory();
        params = (struct macro_params_s *)reallocx(params, macro_parameters.len * sizeof *params);
        macro_parameters.params = params;
        macro_parameters.current = &params[macro_parameters.p];
        macro_parameters.current->param = NULL;
        macro_parameters.current->size = 0;
        macro_parameters.current->pline.len = 0;
        macro_parameters.current->pline.data = NULL;
    }
    macro_parameters.current = &macro_parameters.params[macro_parameters.p];
    macro_parameters.current->macro = val_reference(&macro->v);
    macro_parameters.p++;
    in_macro_old = in_macro;
    in_macro = true;
    {
        struct linepos_s opoint, npoint;
        size_t p = 0;

        ignore(); opoint = lpoint;
        for (;;) {
            str_t *param, *params = macro_parameters.current->param;
            if ((here() == 0 || here() == ';') && p >= macro->argc) break;
            if (p >= macro_parameters.current->size) {
                if (macro_parameters.current->size < macro->argc) macro_parameters.current->size = macro->argc;
                else {
                    macro_parameters.current->size += 4;
                    /*if (macro_parameters.current->size < 4) err_msg_out_of_memory();*/ /* overflow */
                }
                if (macro_parameters.current->size > SIZE_MAX / sizeof *params) err_msg_out_of_memory();
                params = (str_t *)reallocx(params, macro_parameters.current->size * sizeof *params);
                macro_parameters.current->param = params;
            }
            param = params + p;
            param->data = pline + lpoint.pos;
            param->len = macro_param_find();
            if (param->len == 0) {
                if (p < macro->argc) {
                    *param = macro->param[p].init;
                } else param->data = NULL;
                if (param->data == NULL) {
                    if (macro->v.obj->type == T_STRUCT || macro->v.obj->type == T_UNION) {
                        param->data = (const uint8_t *)"?";
                        param->len = 1;
                    }
                }
            }
            p++;
            if (here() == 0 || here() == ';') {
                if (p < macro->argc) continue;
            }
            if (here() != ',') break;
            lpoint.pos++;
            ignore();
        }
        macro_parameters.current->len = p;
        macro_parameters.current->all.data = pline + opoint.pos;
        npoint = lpoint;
        while (npoint.pos > opoint.pos && (pline[npoint.pos-1] == 0x20 || pline[npoint.pos-1] == 0x09)) npoint.pos--;
        macro_parameters.current->all.len = npoint.pos - opoint.pos;
    }
    if (t == W_ENDS) {
        if (context != NULL) push_context(context);
        val = compile();
        if (context != NULL) pop_context();
    } else {
        line_t lin = lpoint.line;
        bool labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;

        if (diagnostics.optimize) cpu_opt_invalidate();
        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree = &s->tree;vline = 0;
        enterfile(macro->file_list->file, epoint);
        lpoint.line = macro->line;
        new_waitfor(t, epoint);
        if (context != NULL) push_context(context);
        val = compile();
        if (context != NULL) pop_context();
        close_waitfor(t);
        star = s->addr;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    val_destroy(&macro->v);
    macro_parameters.p--;
    in_macro = in_macro_old;
    if (macro_parameters.p != 0) macro_parameters.current = &macro_parameters.params[macro_parameters.p - 1];
    return val;
}

Obj *mfunc_recurse(Mfunc *mfunc, Namespace *context, uint8_t strength, linepos_t epoint) {
    size_t i;
    Label *label;
    Obj *val;
    Tuple *tuple = NULL;
    size_t max = 0, args = get_val_remaining();

    if (mfunc->recursion_pass == pass) return NULL;
    if (functionrecursion>100) {
        mfunc->recursion_pass = pass;
        err_msg2(ERROR__FUNRECURSION, NULL, epoint);
        return NULL;
    }
    for (i = 0; i < mfunc->argc; i++) {
        bool labelexists;
        if (mfunc->param[i].init == &default_value->v) {
            size_t j, len = get_val_remaining();
            tuple = new_tuple(len);
            for (j = 0; j < len; j++) {
                tuple->data[j] = pull_val(NULL);
            }
            val = &tuple->v;
        } else {
            struct values_s *vs;
            vs = get_val();
            if (vs == NULL) {
                val = mfunc->param[i].init;
                if (val == NULL) { max = i + 1; val = (Obj *)none_value; }
            } else {
                val = vs->val;
            }
        }
        label = new_label(&mfunc->param[i].name, context, strength, &labelexists, mfunc->file_list);
        if (labelexists) {
            if (label->constant) err_msg_double_defined(label, &mfunc->param[i].name, &mfunc->param[i].epoint); /* not possible in theory */
            else {
                if (label->defpass != pass) {
                    label->ref = false;
                    label->defpass = pass;
                } else {
                    if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                }
                label->owner = false;
                if (label->file_list != mfunc->file_list) {
                    label_move(label, &mfunc->param[i].name, mfunc->file_list);
                }
                label->epoint = mfunc->param[i].epoint;
                val_replace(&label->value, val);
                label->usepass = 0;
            }
        } else {
            label->constant = false;
            label->owner = false;
            label->value = val_reference(val);
            label->epoint = mfunc->param[i].epoint;
        }
    }
    if (tuple != NULL) val_destroy(&tuple->v);
    else if (i < args) err_msg_argnum(args, i, i, epoint);
    if (max != 0) err_msg_argnum(args, max, mfunc->argc, epoint);
    {
        line_t lin = lpoint.line;
        bool labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        size_t oldbottom;
        bool in_macro_old = in_macro;
        in_macro = false;

        if (diagnostics.optimize) cpu_opt_invalidate();
        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree = &s->tree;vline = 0;
        enterfile(mfunc->file_list->file, epoint);
        lpoint.line = mfunc->line;
        new_waitfor(W_ENDF3, epoint);
        oldbottom = context_get_bottom();
        for (i = 0; i < mfunc->nslen; i++) {
            push_context(mfunc->namespaces[i]);
        }
        push_context(context);
        functionrecursion++;
        val = compile();
        functionrecursion--;
        context_set_bottom(oldbottom);
        pop_context();
        for (i = 0; i < mfunc->nslen; i++) {
            pop_context();
        }
        close_waitfor(W_ENDF3);
        star = s->addr;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
        in_macro = in_macro_old;
    }
    return val;
}

void get_func_params(Mfunc *v) {
    Mfunc new_mfunc;
    size_t len = 0, i, j;
    str_t label;
    bool stard = false;

    new_mfunc.param = NULL;
    for (i = 0;;i++) {
        ignore();if (here() == 0 || here() == ';') break;
        if (here() == '*') {
            stard = true;
            lpoint.pos++;ignore();
        }
        if (i >= len) {
            len += 16;
            if (/*len < 16 ||*/ len > SIZE_MAX / sizeof *new_mfunc.param) err_msg_out_of_memory(); /* overflow */
            new_mfunc.param = (struct mfunc_param_s *)reallocx(new_mfunc.param, len * sizeof *new_mfunc.param);
        }
        new_mfunc.param[i].epoint = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len != 0) {
            if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &label, &new_mfunc.param[i].epoint);break;}
            if ((size_t)(label.data - v->file_list->file->data) < v->file_list->file->len) new_mfunc.param[i].name = label;
            else str_cpy(&new_mfunc.param[i].name, &label);
            str_cfcpy(&new_mfunc.param[i].cfname, &label);
            if (new_mfunc.param[i].cfname.data != label.data) str_cfcpy(&new_mfunc.param[i].cfname, NULL);
            else new_mfunc.param[i].cfname = new_mfunc.param[i].name;
            for (j = 0; j < i; j++) if (new_mfunc.param[j].name.data != NULL) {
                if (str_cmp(&new_mfunc.param[j].cfname, &new_mfunc.param[i].cfname) == 0) break;
            }
            if (j != i) {
                err_msg_double_definedo(v->file_list, &new_mfunc.param[j].epoint, &label, &new_mfunc.param[i].epoint);
            }
        } else {err_msg2(ERROR_GENERL_SYNTAX, NULL, &new_mfunc.param[i].epoint);break;}
        ignore();
        if (stard) {
            new_mfunc.param[i].init = (Obj *)ref_default();
        } else {
            new_mfunc.param[i].init = NULL;
            if (here() == '=') {
                lpoint.pos++;
                if (!get_exp(1, 1, 1, &lpoint)) {
                    i++;
                    break;
                }
                new_mfunc.param[i].init = pull_val(NULL);
            }
        }
        if (here() == 0 || here() == ';') {
            i++;
            break;
        }
        if (here() != ',') {
            err_msg2(ERROR______EXPECTED, "','", &lpoint);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i != 0) {
            if (i > SIZE_MAX / sizeof *new_mfunc.param) err_msg_out_of_memory(); /* overflow */
            new_mfunc.param = (struct mfunc_param_s *)reallocx(new_mfunc.param, i * sizeof *new_mfunc.param);
        } else {
            free(new_mfunc.param);
            new_mfunc.param = NULL;
        }
    }
    v->argc = i;
    v->param = new_mfunc.param;
}

void get_macro_params(Obj *v) {
    Macro *macro = (Macro *)v;
    Macro new_macro;
    size_t len = 0, i, j;
    str_t label;
    struct linepos_s *epoints = NULL;
    const struct file_s *cfile = macro->file_list->file;

    new_macro.param = NULL;
    for (i = 0;;i++) {
        ignore();if (here() == 0 || here() == ';') break;
        if (i >= len) {
            len += 16;
            if (/*len < 16 ||*/ len > SIZE_MAX / (sizeof *new_macro.param > sizeof *epoints ? sizeof *new_macro.param : sizeof *epoints)) err_msg_out_of_memory(); /* overflow */
            new_macro.param = (struct macro_param_s *)reallocx(new_macro.param, len * sizeof *new_macro.param);
            epoints = (struct linepos_s *)reallocx(epoints, len * sizeof *epoints);
        }
        epoints[i] = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len != 0) {
            str_t cf;
            if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &label, &epoints[i]);new_macro.param[i].cfname.len = 0; new_macro.param[i].cfname.data = NULL;}
            str_cfcpy(&cf, &label);
            if (cf.data == label.data) {
                if ((size_t)(label.data - cfile->data) < cfile->len) new_macro.param[i].cfname = label;
                else str_cpy(&new_macro.param[i].cfname, &label);
            } else {str_cfcpy(&cf, NULL); new_macro.param[i].cfname = cf;}
            for (j = 0; j < i; j++) if (new_macro.param[j].cfname.data != NULL) {
                if (str_cmp(&new_macro.param[j].cfname, &cf) == 0) break;
            }
            if (j != i) {
                err_msg_double_definedo(macro->file_list, &epoints[j], &label, &epoints[i]);
            }
        } else {new_macro.param[i].cfname.len = 0; new_macro.param[i].cfname.data = NULL;}
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            label.data = pline + lpoint.pos;
            label.len = macro_param_find();
            if ((size_t)(label.data - cfile->data) < cfile->len) new_macro.param[i].init = label;
            else str_cpy(&new_macro.param[i].init, &label);
        } else {new_macro.param[i].init.len = 0; new_macro.param[i].init.data = NULL;}
        ignore();
        if (here() == 0 || here() == ';') {
            i++;
            break;
        }
        if (here() != ',') {
            err_msg2(ERROR______EXPECTED, "','", &lpoint);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i != 0) {
            if (i > SIZE_MAX / sizeof *new_macro.param) err_msg_out_of_memory(); /* overflow */
            new_macro.param = (struct macro_param_s *)reallocx(new_macro.param, i * sizeof *new_macro.param);
        } else {
            free(new_macro.param);
            new_macro.param = NULL;
        }
    }
    macro->argc = i;
    macro->param = new_macro.param;
    free(epoints);
}

Obj *mfunc2_recurse(Mfunc *mfunc, struct values_s *vals, size_t args, linepos_t epoint) {
    size_t i;
    Label *label;
    Obj *val;
    Tuple *tuple;
    Obj *retval = NULL;
    Namespace *context;
    struct linepos_s xpoint;

    if (mfunc->recursion_pass == pass) return NULL;
    if (functionrecursion>100) {
        mfunc->recursion_pass = pass;
        err_msg2(ERROR__FUNRECURSION, NULL, epoint);
        return NULL;
    }

    xpoint.line = mfunc->line;
    xpoint.pos = 0;
    context = new_namespace(mfunc->file_list, &xpoint);

    enterfile(mfunc->file_list->file, epoint);
    tuple = NULL;
    for (i = 0; i < mfunc->argc; i++) {
        bool labelexists;
        if (mfunc->param[i].init == &default_value->v) {
            if (i < args) {
                size_t j = i;
                tuple = new_tuple(args - i);
                none_value->v.refcount += args - i;
                while (j < args) {
                    tuple->data[j - i] = vals[j].val;
                    vals[j].val = (Obj *)none_value;
                    j++;
                }
            } else {
                tuple = (Tuple *)val_reference(&null_tuple->v);
            }
            val = &tuple->v;
        } else {
            val = (i < args) ? vals[i].val : (mfunc->param[i].init != NULL) ? mfunc->param[i].init : (Obj *)none_value;
        }
        label = new_label(&mfunc->param[i].name, context, 0, &labelexists, current_file_list);
        if (!labelexists) {
            label->constant = false;
            label->owner = false;
            label->value = val_reference(val);
            label->file_list = current_file_list;
            label->epoint = mfunc->param[i].epoint;
        }
    }
    if (tuple != NULL) val_destroy(&tuple->v);
    else if (i < args) err_msg_argnum(args, i, i, &vals[i].epoint);
    {
        line_t lin = lpoint.line;
        bool labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        struct linepos_s opoint = lpoint;
        const uint8_t *opline = pline;
        const uint8_t *ollist = llist;
        size_t oldbottom;
        bool in_macro_old = in_macro;
        struct section_address_s section_address, *oldsection_address = current_address;
        in_macro = false;

        if (diagnostics.optimize) cpu_opt_invalidate();
        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree = &s->tree;vline = 0;
        lpoint.line = mfunc->line;
        new_waitfor(W_ENDF3, epoint);
        oldbottom = context_get_bottom();
        for (i = 0; i < mfunc->nslen; i++) {
            push_context(mfunc->namespaces[i]);
        }
        push_context(context);
        temporary_label_branch++;

        section_address.wrapwarn = section_address.moved = section_address.unionmode = false;
        section_address.address = 0;
        section_address.start = 0;
        section_address.l_start.address = 0;
        section_address.l_start.bank = 0;
        section_address.l_union.address = 0;
        section_address.l_union.bank = 0;
        section_address.end = 0;
        section_address.mem = new_memblocks(0, 0);
        section_address.mem->lastaddr = 0;
        section_address.l_address = current_address->l_address;
        section_address.l_address_val = val_reference(current_address->l_address_val);
        current_address = &section_address;
        functionrecursion++;
        retval = compile();
        functionrecursion--;
        val_destroy(current_address->l_address_val);
        val_destroy(&current_address->mem->v);
        current_address = oldsection_address;
        if (current_address->l_address.bank > all_mem) {
            err_msg_big_address(epoint);
            current_address->l_address.bank &= all_mem;
        }
        context_set_bottom(oldbottom);
        pop_context();
        for (i = 0; i < mfunc->nslen; i++) {
            pop_context();
        }
        close_waitfor(W_ENDF3);
        star = s->addr;
        temporary_label_branch--;
        lpoint = opoint;
        pline = opline;
        llist = ollist;
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
        in_macro = in_macro_old;
    }
    exitfile();
    if (diagnostics.unused.macro || diagnostics.unused.consts || diagnostics.unused.label || diagnostics.unused.variable) unused_check(context);
    val_destroy(&context->v);
    return retval;
}

void init_macro(void) {
    macro_parameters.p = 0;
    in_macro = false;
    functionrecursion = 0;
}

void free_macro(void) {
    size_t i;
    for (i = 0; i < macro_parameters.len; i++) {
        free(macro_parameters.params[i].pline.data);
        free(macro_parameters.params[i].param);
    }
    free(macro_parameters.params);
}
