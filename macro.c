/*
    $Id: macro.c 3086 2023-09-03 06:23:08Z soci $

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
#include "main.h"

#include "listobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "namespaceobj.h"
#include "labelobj.h"
#include "macroobj.h"
#include "mfuncobj.h"
#include "memblocksobj.h"
#include "functionobj.h"

bool in_macro;
bool in_function;

static int functionrecursion;

struct macro_rpos_s {
    linecpos_t opos, olen, pos, len;
    argcount_t param;
};

struct macro_pline_s {
    uint8_t *data;
    size_t len;
    struct macro_rpos_s *rpositions;
    argcount_t rp, rlen;
};

struct macro_value_s {
    const uint8_t *data;
    size_t len;
    linecpos_t pos;
    bool init;
};

struct macro_params_s {
    argcount_t len, size;
    struct macro_value_s *param, all;
    struct macro_pline_s pline;
    bool used;
    Obj *macro;
};

static struct {
    size_t p, len;
    struct macro_params_s *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

#define ALL_MACRO_PARAMS (~(argcount_t)0)

const struct file_list_s *macro_error_translate(struct linepos_s *opoint, linecpos_t pos) {
    const struct file_list_s *ret = NULL;
    if (pline == macro_parameters.current->pline.data) {
        const struct file_list_s *flist = current_file_list;
        size_t p = macro_parameters.p;
        while (p != 0) {
            const struct macro_pline_s *mline;
            argcount_t i;
            p--;
            mline = &macro_parameters.params[p].pline;
            for (i = 0; i < mline->rp; i++) {
                linecpos_t c = pos - mline->rpositions[i].pos;
                if (c < mline->rpositions[i].len) {
                    argcount_t param = mline->rpositions[i].param;
                    if (param < macro_parameters.params[p].len) {
                        if (macro_parameters.params[p].param[param].init) return ret;
                        pos = macro_parameters.params[p].param[param].pos;
                    } else {
                        if (param != ALL_MACRO_PARAMS) return ret;
                        pos = macro_parameters.params[p].all.pos;
                    }
                    opoint->pos = pos + c;
                    opoint->line = flist->epoint.line;
                    ret = parent_file_list(flist);
                    flist = ret;
                    break;
                }
            }
            if (i == mline->rp) break;
            if (p > 0 && !macro_parameters.params[p - 1].used) break;
        }
    }
    return ret;
}

linecpos_t macro_error_translate2(linecpos_t pos) {
    if (macro_parameters.p != 0) {
        const struct macro_pline_s *mline = &macro_parameters.current->pline;
        if (pline == mline->data) {
            linecpos_t pos2 = pos;
            argcount_t i;
            for (i = 0; i < mline->rp; i++) {
                const struct macro_rpos_s *rpositions = &mline->rpositions[i];
                if (rpositions->pos > pos2) break;
                if (rpositions->pos + rpositions->len > pos2) {
                    pos = rpositions->opos;
                    break;
                }
                pos = pos + rpositions->olen - rpositions->len;
            }
        }
    }
    return pos;
}

/* ------------------------------------------------------------------------------ */
bool mtranslate(void) {
    unsigned int q;
    argcount_t j, n;
    linecpos_t p;
    linecpos_t last;
    struct macro_pline_s *mline;
    bool changed, fault;
    struct file_s *cfile = current_file_list->file;
    const uint8_t *p2, *op, *last2;

    if (lpoint.line >= cfile->lines) return true;
    llist = pline = &cfile->source.data[cfile->line[lpoint.line]];
    changed = !in_macro || (cfile->nomacro != NULL && (cfile->nomacro[lpoint.line / 8] & (1 << (lpoint.line & 7))) != 0);
    lpoint.pos = 0; lpoint.line++; vline++;
    if (changed) return signal_received;
    mline = &macro_parameters.current->pline;

    q = 0; p = 0; p2 = pline; last2 = pline; n = 0; last = 0; fault = false;
    while (*p2 != 0) {
        str_t param;
        switch (*p2) {
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
            j = (uint8_t)(p2[1] - '1');
            if (j < 9) {   /* \1..\9 */
                p += (linecpos_t)(p2 - last2);
                op = p2;
                p2 += 2;
                break;
            }
            if (j == ('@' - '1')) { /* \@ gives complete parameter list */
                j = ALL_MACRO_PARAMS;
                p += (linecpos_t)(p2 - last2);
                op = p2;
                p2 += 2;
                break;
            }
            if (j == ('{' - '1')) {
                param.data = p2 + 2;
                param.len = get_label(param.data);
                if (param.data[param.len] != '}') param.len = 0;
            } else {
                param.data = p2 + 1;
                param.len = get_label(param.data);
            }
            if (param.len != 0) {
                Macro *macro = Macro(macro_parameters.current->macro);
                str_t cf;
                p += (linecpos_t)(p2 - last2);
                op = p2;
                p2 = param.data + param.len;
                if (j == ('{' - '1')) p2++;
                str_cfcpy(&cf, &param);
                for (j = 0; j < macro->argc; j++) {
                    const uint8_t *data;
                    if (macro->param[j].cfname.len != cf.len) continue;
                    data = macro->param[j].cfname.data;
                    if (data[0] != cf.data[0]) continue;
                    if (memcmp(data, cf.data, cf.len) == 0) break;
                }
                if (j < macro->argc) break;
                lpoint.pos = (linecpos_t)(op - pline);
                err_msg_unknown_argument(&param, &lpoint);
                p = last; last2 = p2; fault = true;
                continue;
            }
            p2++;
            continue;
        case '@':
            if (arguments.tasmcomp) {
                /* text parameter reference */
                j = (uint8_t)(p2[1] - '1');
                if (j < 9) { /* @1..@9 */
                    p += (linecpos_t)(p2 - last2);
                    op = p2;
                    p2 += 2;
                    if (j < macro_parameters.current->len) {
                        param.data = macro_parameters.current->param[j].data;
                        param.len = macro_parameters.current->param[j].len;
                        if (param.len > 1 && param.data[0] == '"' && param.data[param.len-1] == '"') {
                            param.data++;
                            param.len -= 2;
                        }
                        goto tasmc;
                    }
                    break;
                }
            }
            p2++;
            continue;
        }

        if (j < macro_parameters.current->len) {
            param.data = macro_parameters.current->param[j].data;
            param.len = macro_parameters.current->param[j].len;
        } else if (j == ALL_MACRO_PARAMS) {
            param.data = macro_parameters.current->all.data;
            param.len = macro_parameters.current->all.len;
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
    tasmc:
        if (p + param.len < p) err_msg_out_of_memory(); /* overflow */
        if (p + param.len > mline->len) {
            mline->len = p + param.len;
            extend_array(&mline->data, &mline->len, 1024);
        }
        if (last != p) {
            if (p < last) err_msg_out_of_memory(); /* overflow */
            memcpy(mline->data + last, last2, p - last);
        }
        if (n >= mline->rlen) extend_array(&mline->rpositions, &mline->rlen, 8);
        mline->rpositions[n].opos = (linecpos_t)(op - pline);
        mline->rpositions[n].olen = (linecpos_t)(p2 - op);
        mline->rpositions[n].pos = p;
        mline->rpositions[n].param = j;
        mline->rpositions[n++].len = (linecpos_t)param.len;
        switch (param.len) {
        case 0:
            if (param.data == NULL) {
                lpoint.pos = (linecpos_t)(last2 - pline) + p - last;
                err_msg_missing_argument(&lpoint, j);
                fault = true;
            }
            break;
        case 1:
            mline->data[p++] = *param.data;
            break;
        default:
            memcpy(mline->data + p, param.data, param.len);
            p += (linecpos_t)param.len;
        }
        last = p; last2 = p2;
    }
    mline->rp = n;
    if (last2 != pline) {
        while (p2 != pline && (p2[-1] == 0x20 || p2[-1] == 0x09)) p2--;
        p += (linecpos_t)(p2 - last2);
        if (p + 1 < p) err_msg_out_of_memory(); /* overflow */
        if (p + 1 > mline->len) {
            if (add_overflow(p, 1024, &mline->len)) err_msg_out_of_memory();
            resize_array(&mline->data, mline->len);
        }
        if (p != last) memcpy(mline->data + last, last2, p - last);
        mline->data[p] = 0;
        llist = pline = fault ? (const uint8_t *)"" : mline->data;
    } else {
        linenum_t lnum;
        if (cfile->nomacro == NULL) {
            size_t l = (cfile->lines + 7) / 8;
            new_array(&cfile->nomacro, l);
            memset(cfile->nomacro, 0, l * sizeof *cfile->nomacro);
        }
        lnum = lpoint.line - 1;
        cfile->nomacro[lnum / 8] |= (uint8_t)(1U << (lnum & 7));
    }
    lpoint.pos = 0;
    return signal_received;
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
                    if (inc_overflow(&pl, 256)) err_msg_out_of_memory();
                    if (par == pbuf) {
                        new_array(&par, pl);
                        memcpy(par, pbuf, pp);
                    } else {
                        resize_array(&par, pl);
                    }
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
    Macro *macro = Macro(tmp2);
    if (macro->recursion_pass == pass) return NULL;
    if (macro_parameters.p > 100) {
        macro->recursion_pass = pass;
        err_msg2(ERROR__MACRECURSION, NULL, epoint);
        return NULL;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        struct macro_params_s *params = macro_parameters.params;
        extend_array(&params, &macro_parameters.len, 1);
        macro_parameters.params = params;
        macro_parameters.current = &params[macro_parameters.p];
        macro_parameters.current->param = NULL;
        macro_parameters.current->size = 0;
        macro_parameters.current->pline.len = 0;
        macro_parameters.current->pline.data = NULL;
        macro_parameters.current->pline.rpositions = NULL;
        macro_parameters.current->pline.rp = 0;
        macro_parameters.current->pline.rlen = 0;
    }
    if (macro_parameters.p > 0) {
        macro_parameters.params[macro_parameters.p - 1].used = (pline == macro_parameters.params[macro_parameters.p - 1].pline.data);
    }
    macro_parameters.current = &macro_parameters.params[macro_parameters.p];
    macro_parameters.current->macro = val_reference(Obj(macro));
    macro_parameters.p++;
    in_macro_old = in_macro;
    in_macro = true;
    {
        struct linepos_s opoint, npoint;
        argcount_t p = 0;

        ignore(); opoint = lpoint;
        for (;;) {
            struct macro_value_s *param, *params = macro_parameters.current->param;
            if ((here() == 0 || here() == ';') && p >= macro->argc) break;
            if (p >= macro_parameters.current->size) {
                if (macro_parameters.current->size < macro->argc) macro_parameters.current->size = macro->argc;
                else {
                    if (inc_overflow(&macro_parameters.current->size, 4)) err_msg_out_of_memory();
                }
                resize_array(&params, macro_parameters.current->size);
                macro_parameters.current->param = params;
            }
            param = params + p;
            param->pos = lpoint.pos;
            param->data = pline + lpoint.pos;
            param->len = macro_param_find();
            param->init = (param->len == 0);
            if (param->init) {
                if (p < macro->argc) {
                    param->data = macro->param[p].init.data;
                    param->len = macro->param[p].init.len;
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
        macro_parameters.current->all.pos = opoint.pos;
        macro_parameters.current->all.data = pline + opoint.pos;
        npoint = lpoint;
        while (npoint.pos > opoint.pos && (pline[npoint.pos-1] == 0x20 || pline[npoint.pos-1] == 0x09)) npoint.pos--;
        macro_parameters.current->all.len = npoint.pos - opoint.pos;
    }
    if (t == W_ENDS) {
        enterfile(macro->file_list->file, epoint);
        if (context != NULL) push_context(context);
        val = compile();
        if (context != NULL) pop_context();
        exitfile();
    } else {
        linenum_t lin = lpoint.line;
        struct star_s *s = new_star(vline);
        struct star_s *stree_old = star_tree;

        if (diagnostics.optimize) cpu_opt_invalidate();
        if (s->pass != 0 && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree->vline = vline; star_tree = s; vline = s->vline;
        enterfile(macro->file_list->file, epoint);
        lpoint.line = macro->line;
        new_waitfor(t, epoint);
        if (context != NULL) push_context(context);
        val = compile();
        if (context != NULL) pop_context();
        close_waitfor(t);
        star = s->addr;
        exitfile();
        s->vline = vline; star_tree = stree_old; vline = star_tree->vline;
        lpoint.line = lin;
    }
    val_destroy(Obj(macro));
    macro_parameters.p--;
    in_macro = in_macro_old;
    if (macro_parameters.p != 0) macro_parameters.current = &macro_parameters.params[macro_parameters.p - 1];
    return val;
}

Obj *mfunc_recurse(Mfunc *mfunc, Namespace *context, uint8_t strength, linepos_t epoint) {
    argcount_t i;
    Label *label;
    Obj *val;
    Tuple *tuple = NULL;
    argcount_t max = 0, args = get_val_remaining();

    if (mfunc->recursion_pass == pass) return NULL;
    if (functionrecursion>100) {
        mfunc->recursion_pass = pass;
        err_msg2(ERROR__FUNRECURSION, NULL, epoint);
        return NULL;
    }
    for (i = 0; i < mfunc->argc; i++) {
        const struct mfunc_param_s *param = &mfunc->param[i];
        if (param->init == default_value) {
            argcount_t j, len = get_val_remaining();
            tuple = new_tuple(len);
            for (j = 0; j < len; j++) {
                tuple->data[j] = pull_val();
            }
            val = val_reference(Obj(tuple));
        } else {
            struct values_s *vs;
            vs = get_val();
            if (vs == NULL) {
                val = param->init;
                if (val == NULL) { max = i + 1; val = none_value; }
                val = val_reference(val);
            } else {
                val = vs->val;
                if (param->type != NULL && val->obj != Type(param->type)) {
                    struct oper_s oper;
                    oper.v1 = param->type;
                    oper.op = O_FUNC;
                    oper.epoint3 = oper.epoint2 = oper.epoint = &vs->epoint;
                    oper.inplace = NULL;
                    if (oper.v1->obj->type == T_TYPE) {
                        oper.v2 = val;
                        if (Type(oper.v1)->iterable || Type(oper.v1) == TYPE_OBJ) {
                            val = Type(oper.v1)->convert(&oper);
                        } else {
                            val = apply_function(&oper, Type(oper.v1)->convert);
                        }
                    } else {
                        Funcargs tmp;
                        tmp.val = vs;
                        tmp.len = 1; /* assumes no referencing */
                        tmp.v.obj = FUNCARGS_OBJ;
                        oper.v2 = &tmp.v;
                        val = oper.v1->obj->calc2(&oper);
                    }
                } else val = val_reference(val);
            }
        }
        label = new_label(&param->name, context, strength, mfunc->file_list);
        if (label->value != NULL) {
            if (label->constant) {
                err_msg_double_defined(label, &param->name, &param->epoint); /* not possible in theory */
                val_destroy(val);
            } else {
                if (label->defpass != pass) {
                    label->ref = false;
                    label->defpass = pass;
                } else {
                    if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                }
                label->owner = false;
                if (label->file_list != mfunc->file_list) {
                    label_move(label, &param->name, mfunc->file_list);
                }
                label->epoint = param->epoint;
                val_destroy(label->value); label->value = val;
                label->usepass = 0;
            }
        } else {
            label->constant = false;
            label->owner = false;
            label->value = val;
            label->epoint = param->epoint;
        }
    }
    if (tuple != NULL) val_destroy(Obj(tuple));
    else if (i < args) err_msg_argnum(args, i, i, epoint);
    if (max != 0) err_msg_argnum(args, max, mfunc->argc, epoint);
    {
        linenum_t lin = lpoint.line;
        struct star_s *s = new_star(vline);
        struct star_s *stree_old = star_tree;
        size_t oldbottom;
        bool in_macro_old = in_macro;
        in_macro = false;

        if (diagnostics.optimize) cpu_opt_invalidate();
        if (s->pass != 0 && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree->vline = vline; star_tree = s; vline = s->vline;
        enterfile(mfunc->file_list->file, epoint);
        lpoint.line = mfunc->epoint.line;
        new_waitfor(W_ENDF2, epoint);
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
        close_waitfor(W_ENDF2);
        star = s->addr;
        exitfile();
        s->vline = vline; star_tree = stree_old; vline = star_tree->vline;
        lpoint.line = lin;
        in_macro = in_macro_old;
    }
    return val;
}

bool get_func_params(Mfunc *v, bool single) {
    struct mfunc_param_s *params;
    argcount_t len = v->argc, i = 0;
    str_t label;
    bool stard = false, ret = false;

    if (len == 0) params = NULL; else new_array(&params, len);
    if (here() != 0 && here() != ';') {
        for (;;) {
            struct mfunc_param_s *param;
            ignore();
            if (here() == '*') {
                stard = true;
                lpoint.pos++;ignore();
            }
            if (i >= len) {
                extend_array(&params, &len, 16);
            }
            param = params + i;
            param->epoint = lpoint;
            label.data = pline + lpoint.pos;
            label.len = get_label(label.data);
            lpoint.pos += (linecpos_t)label.len;
            ignore();
            if (single) {
                const uint8_t *s = pline + lpoint.pos;
                if (!stard && s[0] != ',' && s[0] != ':' && (s[0] != '=' || s[1] == '=')) {
                    v->epoint.line = lpoint.line - 1;
                    lpoint.pos = param->epoint.pos;
                    v->epoint.pos = lpoint.pos;
                    break;
                }
            }
            if (label.len != 0) {
                if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {
                    err_msg2(ERROR_RESERVED_LABL, &label, &param->epoint);
                    ret = true;
                    break;
                }
                if (not_in_file(label.data, v->file_list->file)) str_cpy(&param->name, &label);
                else param->name = label;
                str_cfcpy(&param->cfname, &label);
                if (param->cfname.data != label.data) str_cfcpy(&param->cfname, NULL);
                else param->cfname = param->name;
            } else {
                err_msg2(ERROR_GENERL_SYNTAX, NULL, &param->epoint);
                ret = true;
                break;
            }
            i++;
            param->type = NULL;
            if (stard) {
                param->init = ref_default();
                if (single) {
                    if (here() != ',') {
                        err_msg2(ERROR______EXPECTED, "','", &lpoint);
                        ret = true;
                        break;
                    }
                    lpoint.pos++;
                    ignore();
                    v->epoint.line = lpoint.line - 1;
                    v->epoint.pos = lpoint.pos;
                }
                break;
            }
            param->init = NULL;
            if (here() == ':') {
                lpoint.pos++;
                if (!get_exp(5, 1, 1, &lpoint)) {
                    ret = true;
                    break;
                }
                param->type = pull_val();
            }
            if (here() == '=') {
                lpoint.pos++;
                if (!get_exp(1, 1, 1, &lpoint)) {
                    ret = true;
                    break;
                }
                param->init = pull_val();
            }
            if (here() == 0 || here() == ';') {
                break;
            }
            if (here() != ',') {
                err_msg2(ERROR______EXPECTED, "','", &lpoint);
                ret = true;
                break;
            }
            lpoint.pos++;
        }
    }
    if (i < len) {
        if (i != 0) {
            struct mfunc_param_s *p = reallocate_array(params, i);
            if (p != NULL) params = p;
        } else {
            free(params);
            params = NULL;
        }
    }
    v->argc = i;
    v->param = params;
    return ret;
}

void get_macro_params(Obj *v) {
    Macro *macro = Macro(v);
    struct macro_param_s *params;
    argcount_t len = macro->argc, i = 0, j;
    str_t label;
    struct linepos_s *epoints;
    struct linepos_s vepoints[4];
    const struct file_s *cfile = macro->file_list->file;

    if (len == 0) params = NULL; else new_array(&params, len);
    if (len <= lenof(vepoints)) {
        epoints = vepoints;
    } else {
        new_array(&epoints, len);
    }
    for (;;) {
        struct macro_param_s *param;
        ignore();if (here() == 0 || here() == ';') break;
        if (i >= len) {
            extend_array(&params, &len, 16);
            if (epoints == vepoints) {
                new_array(&epoints, len);
                memcpy(epoints, vepoints, sizeof vepoints);
            } else {
                resize_array(&epoints, len);
            }
        }
        param = params + i;
        epoints[i] = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label(label.data);
        if (label.len != 0) {
            str_t cf;
            lpoint.pos += (linecpos_t)label.len;
            if (label.len > 1 && label.data[0] == '_' && label.data[1] == '_') {err_msg2(ERROR_RESERVED_LABL, &label, &epoints[i]);param->cfname.len = 0; param->cfname.data = NULL;}
            str_cfcpy(&cf, &label);
            if (cf.data == label.data) {
                if (not_in_file(label.data, cfile)) str_cpy(&param->cfname, &label);
                else param->cfname = label;
            } else {str_cfcpy(&cf, NULL); param->cfname = cf;}
            for (j = 0; j < i; j++) if (params[j].cfname.data != NULL) {
                if (str_cmp(&params[j].cfname, &cf) == 0) break;
            }
            if (j != i) {
                err_msg_double_definedo(macro->file_list, &epoints[j], &label, &epoints[i]);
            }
        } else {param->cfname.len = 0; param->cfname.data = NULL;}
        i++;
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            label.data = pline + lpoint.pos;
            label.len = macro_param_find();
            if (not_in_file(label.data, cfile)) str_cpy(&param->init, &label);
            else param->init = label;
        } else {param->init.len = 0; param->init.data = NULL;}
        ignore();
        if (here() == 0 || here() == ';') {
            break;
        }
        if (here() != ',') {
            err_msg2(ERROR______EXPECTED, "','", &lpoint);
            break;
        }
        lpoint.pos++;
    }
    if (i < len) {
        if (i != 0) {
            struct macro_param_s *p = reallocate_array(params, i);
            if (p != NULL) params = p;
        } else {
            free(params);
            params = NULL;
        }
    }
    macro->argc = i;
    macro->param = params;
    if (epoints != vepoints) free(epoints);
}

static bool clean_namespace(Namespace *v1) {
    size_t n, n2;
    if (v1->len == 0) return true;
    for (n = n2 = 0; n <= v1->mask && n2 < v1->len; n++) {
        const Label *p = v1->data[n];
        if (p == NULL) continue;
        if (p->constant) {
            return false;
        }
        n2++;
    }
    for (n2 = 0; n2 < n; n2++) {
        Label *p = v1->data[n2];
        if (p == NULL) continue;
        val_destroy(Obj(p));
        v1->data[n2] = NULL;
    }
    v1->len = 0;
    return true;
}

Obj *mfunc2_recurse(Mfunc *mfunc, Funcargs *v2, linepos_t epoint) {
    struct values_s *vals = v2->val;
    argcount_t args = v2->len;
    argcount_t i;
    Label *label;
    Tuple *tuple;
    Obj *retval = NULL;
    Namespace *context;

    if (mfunc->recursion_pass == pass) return NULL;
    if (functionrecursion>100) {
        mfunc->recursion_pass = pass;
        err_msg2(ERROR__FUNRECURSION, NULL, epoint);
        return NULL;
    }

    if (mfunc->inamespaces == Tuple(null_tuple)) {
        val_destroy(Obj(mfunc->inamespaces));
        mfunc->inamespaces = new_tuple(1);
        context = NULL;
    } else {
        List *lst = mfunc->inamespaces;
        if (mfunc->ipoint >= lst->len) {
            if ((lst->data == lst->u.val ? mfunc->ipoint >= lenof(lst->u.val) : mfunc->ipoint >= lst->u.s.max)) {
                if (list_extend(lst)) {
                    err_msg2(ERROR_OUT_OF_MEMORY, NULL, epoint);
                    return ref_none();
                }
            }
            lst->len++;
            context = NULL;
        } else context = Namespace(lst->data[mfunc->ipoint]);
    }
    if (context == NULL) {
        struct linepos_s xpoint;
        xpoint.line = mfunc->epoint.line;
        xpoint.pos = 0;
        context = new_namespace(mfunc->file_list, &xpoint);
        mfunc->inamespaces->data[mfunc->ipoint] = Obj(context);
    } else {
        context->backr = context->forwr = 0;
    }
    mfunc->ipoint++;

    tuple = NULL;
    for (i = 0; i < mfunc->argc; i++) {
        Obj *val;
        const struct mfunc_param_s *param = &mfunc->param[i];
        if (param->init == default_value) {
            if (i < args) {
                argcount_t j = i;
                tuple = new_tuple(args - i);
                none_value->refcount += args - i;
                while (j < args) {
                    tuple->data[j - i] = vals[j].val;
                    vals[j].val = none_value;
                    j++;
                }
            } else {
                tuple = Tuple(val_reference(null_tuple));
            }
            val = val_reference(Obj(tuple));
        } else {
            if (i >= args) {
                val = val_reference((param->init != NULL) ? param->init : none_value);
            } else {
                val = vals[i].val;
                if (param->type != NULL && val->obj != Type(param->type)) {
                    struct oper_s oper;
                    oper.v1 = param->type;
                    oper.op = O_FUNC;
                    oper.epoint3 = oper.epoint2 = oper.epoint = &vals[i].epoint;
                    oper.inplace = NULL;
                    if (oper.v1->obj->type == T_TYPE) {
                        oper.v2 = val;
                        if (Type(oper.v1)->iterable || Type(oper.v1) == TYPE_OBJ) {
                            val = Type(oper.v1)->convert(&oper);
                        } else {
                            val = apply_function(&oper, Type(oper.v1)->convert);
                        }
                    } else {
                        Funcargs tmp;
                        struct values_s vs;
                        vs.val = val;
                        vs.epoint = *oper.epoint;
                        tmp.val = &vs;
                        tmp.len = 1; /* assumes no referencing */
                        tmp.v.obj = FUNCARGS_OBJ;
                        oper.v2 = &tmp.v;
                        functionrecursion++;
                        val = oper.v1->obj->calc2(&oper);
                        functionrecursion--;
                    }
                } else val_reference(val);
            }
        }
        label = new_label(&param->name, context, 0, mfunc->file_list);
        if (label->value != NULL) {
            if (label->constant) {
                err_msg_double_defined(label, &param->name, &param->epoint);
                val_destroy(val);
            } else {
                if (label->defpass != pass) {
                    label->ref = false;
                    label->defpass = pass;
                } else {
                    if (diagnostics.unused.variable && label->usepass != pass) err_msg_unused_variable(label);
                }
                label->owner = false;
                if (label->file_list != mfunc->file_list) {
                    label_move(label, &param->name, mfunc->file_list);
                }
                label->epoint = param->epoint;
                val_destroy(label->value);
                label->value = val;
                label->usepass = 0;
            }
        } else {
            label->constant = false;
            label->owner = false;
            label->value = val;
            label->epoint = param->epoint;
        }
    }
    if (tuple != NULL) val_destroy(Obj(tuple));
    else if (i < args) err_msg_argnum(args, i, i, &vals[i].epoint);
    enterfile(mfunc->file_list->file, epoint);
    {
        struct linepos_s opoint = lpoint;
        const uint8_t *opline = pline;
        const uint8_t *ollist = llist;
        size_t oldbottom;
        bool in_macro_old = in_macro;
        bool in_function_old = in_function;
        struct section_address_s section_address, *oldsection_address = current_address;
        struct star_s *s = new_star(vline);
        struct star_s *stree_old = star_tree;
        size_t j;

        if (s->pass != 0 && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig = false;
        }
        s->addr = star;
        star_tree->vline = vline; star_tree = s; vline = s->vline;

        in_macro = false;
        in_function = true;

        lpoint.line = mfunc->epoint.line;
        oldbottom = context_get_bottom();
        for (j = 0; j < mfunc->nslen; j++) {
            push_context(mfunc->namespaces[j]);
        }
        push_context(context);
        functionrecursion++;
        if (mfunc->v.obj == SFUNC_OBJ) {
            if (mtranslate()) {
                lpoint.pos = mfunc->epoint.pos;
                if (mfunc->line != NULL) pline = mfunc->line;
                if (signal_received) err_msg_signal();
                retval = NULL;
            } else {
                lpoint.pos = mfunc->epoint.pos;
                if (mfunc->line != NULL) pline = mfunc->line;
                if (!get_exp(0, 0, 0, &mfunc->epoint)) {
                    retval = NULL;
                } else {
                    retval = get_vals_tuple();
                }
            }
        } else {
            if (diagnostics.optimize) cpu_opt_invalidate();

            new_waitfor(W_ENDF3, epoint);

            section_address.moved = section_address.wrapwarn = section_address.bankwarn = section_address.unionmode = false;
            section_address.address = 0;
            section_address.start = 0;
            section_address.l_start = 0;
            section_address.l_union = 0;
            section_address.end = 0;
            section_address.mem = new_memblocks(0, 0);
            section_address.mem->lastaddr = 0;
            section_address.l_address = current_address->l_address;
            section_address.l_address_val = val_reference(current_address->l_address_val);
            current_address = &section_address;

            retval = compile();

            val_destroy(current_address->l_address_val);
            val_destroy(Obj(current_address->mem));
            current_address = oldsection_address;
            if (current_address->l_address > all_mem) {
                err_msg_big_address(epoint);
                current_address->l_address &= all_mem;
            }

            close_waitfor(W_ENDF3);
        }
        star = s->addr;
        s->vline = vline; star_tree = stree_old; vline = star_tree->vline;
        functionrecursion--;
        context_set_bottom(oldbottom);
        pop_context();
        for (j = 0; j < mfunc->nslen; j++) {
            pop_context();
        }
        lpoint = opoint;
        pline = opline;
        llist = ollist;
        in_macro = in_macro_old;
        in_function = in_function_old;
    }
    exitfile();
    if (context->v.refcount == 1 && clean_namespace(context)) {
        mfunc->ipoint--;
    }
    return retval;
}

void init_macro(void) {
    macro_parameters.p = 0;
    in_macro = false;
    in_function = false;
    functionrecursion = 0;
}

void free_macro(void) {
    size_t i;
    for (i = 0; i < macro_parameters.len; i++) {
        free(macro_parameters.params[i].pline.rpositions);
        free(macro_parameters.params[i].pline.data);
        free(macro_parameters.params[i].param);
    }
    free(macro_parameters.params);
    macro_parameters.len = 0;
    macro_parameters.params = NULL;
}
