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

static struct {
    size_t p, len;
    struct {
        size_t len, size;
        str_t *param, all;
        struct {
            size_t len;
            uint8_t *data;
        } pline;
        struct value_s *macro;
    } *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

//------------------------------------------------------------------------------
int mtranslate(struct file_s *cfile)
{
    uint_fast8_t q;
    uint_fast16_t j;
    size_t p;
    uint8_t ch;

    if (cfile->len <= cfile->p) return 1;
    pline = cfile->data + cfile->p; lpoint.pos = 0; lpoint.upos = 0; lpoint.line++;vline++; cfile->p += strlen((const char *)pline) + 1;
    if (!macro_parameters.p) return 0;

    q=p=0;
    for (; (ch = here()); lpoint.pos++) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        else if ((ch == ';') && (!q)) { q=4; }
        else if ((ch=='\\') && (!q)) {
            /* normal parameter reference */
            if ((ch=pline[lpoint.pos+1]) >= '1' && ch <= '9') {
                /* \1..\9 */
                if ((j=ch-'1') >= macro_parameters.current->len || !macro_parameters.current->param[j].data) {
                    if (macro_parameters.current->macro->obj == STRUCT_OBJ || macro_parameters.current->macro->obj == UNION_OBJ) {
                        lpoint.pos++;
                        ch = '?';
                        goto ok;
                    }
                    err_msg(ERROR_MISSING_ARGUM,NULL);
                    break;
                }
                if (p + macro_parameters.current->param[j].len > macro_parameters.current->pline.len) {
                    macro_parameters.current->pline.len += macro_parameters.current->param[j].len + 1024;
                    macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
                    if (!macro_parameters.current->pline.data) err_msg_out_of_memory();
                }
                memcpy((char *)macro_parameters.current->pline.data + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                p += macro_parameters.current->param[j].len;
                lpoint.pos++;continue;
            } else if (ch=='@') {
                /* \@ gives complete parameter list */
                if (p + macro_parameters.current->all.len > macro_parameters.current->pline.len) {
                    macro_parameters.current->pline.len += macro_parameters.current->all.len + 1024;
                    macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
                    if (!macro_parameters.current->pline.data) err_msg_out_of_memory();
                }
                memcpy((char *)macro_parameters.current->pline.data + p, macro_parameters.current->all.data, macro_parameters.current->all.len);
                p += macro_parameters.current->all.len;
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
                    for (j = 0; j < macro_parameters.current->macro->u.macro.argc; j++) {
                        if (!macro_parameters.current->macro->u.macro.param[j].name.data) continue;
                        if (arguments.casesensitive) {
                            if (str_cmp(&macro_parameters.current->macro->u.macro.param[j].name, &label)) continue;
                        } else {
                            if (str_casecmp(&macro_parameters.current->macro->u.macro.param[j].name, &label)) continue;
                        }
                        if (!macro_parameters.current->param[j].data) {
                            if (macro_parameters.current->macro->obj == STRUCT_OBJ || macro_parameters.current->macro->obj == UNION_OBJ) {
                                lpoint.pos--;
                                ch = '?';
                                goto ok;
                            }
                            err_msg2(ERROR_MISSING_ARGUM, NULL, &e);
                            break;
                        }
                        if (p + macro_parameters.current->param[j].len > macro_parameters.current->pline.len) {
                            macro_parameters.current->pline.len += macro_parameters.current->param[j].len + 1024;
                            macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
                            if (!macro_parameters.current->pline.data) err_msg_out_of_memory();
                        }
                        memcpy((char *)macro_parameters.current->pline.data + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                        p += macro_parameters.current->param[j].len;
                        break;
                    }
                    if (j < macro_parameters.current->macro->u.macro.argc) {
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
                if ((j=ch-'1') >= macro_parameters.current->len) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                if (p + macro_parameters.current->param[j].len > macro_parameters.current->pline.len) {
                    macro_parameters.current->pline.len += macro_parameters.current->param[j].len + 1024;
                    macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
                    if (!macro_parameters.current->pline.data) err_msg_out_of_memory();
                }
                if (macro_parameters.current->param[j].len > 1 && macro_parameters.current->param[j].data[0] == '"' && macro_parameters.current->param[j].data[macro_parameters.current->param[j].len-1]=='"') {
                    memcpy((char *)macro_parameters.current->pline.data + p, macro_parameters.current->param[j].data + 1, macro_parameters.current->param[j].len - 2);
                    p += macro_parameters.current->param[j].len - 2;
                } else {
                    memcpy((char *)macro_parameters.current->pline.data + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                    p += macro_parameters.current->param[j].len;
                }
                lpoint.pos++;continue;
            } else ch='@';
        }
    ok:
        if (p + 1 > macro_parameters.current->pline.len) {
            macro_parameters.current->pline.len += 1024;
            macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
            if (!macro_parameters.current->pline.data || macro_parameters.current->pline.len < 1024) err_msg_out_of_memory(); /* overflow */
        }
        macro_parameters.current->pline.data[p++]=ch;
    }
    if (p + 1 > macro_parameters.current->pline.len) {
        macro_parameters.current->pline.len += 1024;
        macro_parameters.current->pline.data = (uint8_t *)realloc((char *)macro_parameters.current->pline.data, macro_parameters.current->pline.len);
        if (!macro_parameters.current->pline.data || macro_parameters.current->pline.len < 1024) err_msg_out_of_memory(); /* overflow */
    }
    macro_parameters.current->pline.data[p]=0;
    pline = macro_parameters.current->pline.data; lpoint.pos = lpoint.upos = 0;
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

struct value_s *macro_recurse(enum wait_e t, struct value_s *tmp2, struct label_s *context, linepos_t epoint) {
    struct value_s *val;
    if (macro_parameters.p>100) {
        err_msg(ERROR__MACRECURSION,"!!!!");
        return NULL;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        macro_parameters.len += 1;
        macro_parameters.params = realloc(macro_parameters.params, sizeof(*macro_parameters.params) * macro_parameters.len);
        if (!macro_parameters.params || macro_parameters.len < 1 || macro_parameters.len > ((size_t)~0) / sizeof(*macro_parameters.params)) err_msg_out_of_memory();
        macro_parameters.params[macro_parameters.p].param = NULL;
        macro_parameters.params[macro_parameters.p].size = 0;
        macro_parameters.params[macro_parameters.p].pline.len = 0;
        macro_parameters.params[macro_parameters.p].pline.data = NULL;
    }
    macro_parameters.current = &macro_parameters.params[macro_parameters.p];
    macro_parameters.current->macro = val_reference(tmp2);
    macro_parameters.p++;
    {
        struct linepos_s opoint, npoint;
        size_t p = 0;

        ignore(); opoint = lpoint;
        for (;;) {
            if ((!here() || here()==';') && p >= tmp2->u.macro.argc) break;
            if (p >= macro_parameters.current->size) {
                if (macro_parameters.current->size < tmp2->u.macro.argc) macro_parameters.current->size = tmp2->u.macro.argc;
                else macro_parameters.current->size += 4;
                macro_parameters.current->param = (str_t *)realloc(macro_parameters.current->param, sizeof(*macro_parameters.current->param) * macro_parameters.current->size);
                if (!macro_parameters.current->param) err_msg_out_of_memory();
            }
            macro_parameters.current->param[p].data = pline + lpoint.pos;
            macro_parameters.current->param[p].len = macro_param_find();
            if (!macro_parameters.current->param[p].len) {
                if (p < tmp2->u.macro.argc) {
                    macro_parameters.current->param[p] = tmp2->u.macro.param[p].init;
                } else macro_parameters.current->param[p].data = NULL;
            }
            p++;
            if (!here() || here()==';') {
                if (p < tmp2->u.macro.argc) continue;
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
        struct label_s *oldcontext = current_context;
        current_context = context;
        val = compile(tmp2->u.macro.parent->file_list);
        current_context = oldcontext;
    } else {
        size_t oldpos;
        line_t lin = lpoint.line;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        struct label_s *oldcontext = current_context;
        struct file_list_s *cflist;
        struct linepos_s nopoint = {0,0,0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        cflist = enterfile(tmp2->u.macro.parent->file_list->file, epoint);
        lpoint.line = tmp2->u.macro.parent->epoint.line;
        new_waitfor(t, &nopoint);
        f = cflist->file;
        oldpos = f->p; f->p = tmp2->u.macro.p;
        current_context = context;
        val = compile(cflist);
        current_context = oldcontext;star = s->addr;
        f->p = oldpos;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    val_destroy(macro_parameters.current->macro);
    macro_parameters.p--;
    if (macro_parameters.p) macro_parameters.current = &macro_parameters.params[macro_parameters.p - 1];
    return val;
}

struct value_s *func_recurse(enum wait_e t, struct value_s *tmp2, struct label_s *context, linepos_t epoint, struct file_s *cfile) {
    size_t i;
    int w;
    struct label_s *label;
    struct value_s *val;
    int fin = 0;
    struct linepos_s epoint2;

    for (i = 0; i < tmp2->u.func.argc; i++) {
        int labelexists;
        label=find_label3(&tmp2->u.func.param[i].name, context, 0);
        ignore();if (!here() || here()==';') fin++;
        if (tmp2->u.func.param[i].init) {
            if (here()==',' || !here() || here()==';') {
                val = tmp2->u.func.param[i].init;
            } else if (get_exp(&w,4,cfile)) {
                if (!(val = get_val(&epoint2))) {
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    val = tmp2->u.func.param[i].init;
                }
            } else val = tmp2->u.func.param[i].init;
            ignore();if (here()==',') lpoint.pos++;
        } else {
            if (fin > 1) {err_msg(ERROR______EXPECTED,","); val = &none_value;}
            else if (get_exp(&w,4,cfile)) {
                if (!(val = get_val(&epoint2))) {
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    val = &none_value;
                }
            } else val = &none_value;
            ignore();if (here()==',') lpoint.pos++;
        }
        if (label) labelexists = 1;
        else label = new_label(&tmp2->u.func.param[i].name, context, L_CONST, 0, &labelexists);
        label->ref=0;
        if (labelexists) {
            if (label->type != L_CONST || pass==1) err_msg_double_defined(label, &tmp2->u.func.param[i].name, &epoint2);
            else {
                label->requires=current_section->requires;
                label->conflicts=current_section->conflicts;
                var_assign(label, val, 0);
            }
        } else {
            label->requires = current_section->requires;
            label->conflicts = current_section->conflicts;
            label->usepass = pass;
            label->defpass = pass;
            label->value = val_reference(val);
            label->file_list = tmp2->u.func.label->file_list;
            label->epoint = tmp2->u.func.param[i].epoint;
        }
    }
    if (i == tmp2->u.func.argc && here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    {
        size_t oldpos;
        line_t lin = lpoint.line;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        struct label_s *oldcontext = current_context;
        struct file_list_s *cflist;
        struct linepos_s nopoint = {0,0,0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        cflist = enterfile(tmp2->u.func.label->file_list->file, epoint);
        lpoint.line = tmp2->u.func.label->epoint.line;
        new_waitfor(t, &nopoint);
        f = cflist->file;
        oldpos = f->p; f->p = tmp2->u.func.p;
        current_context = context;
        val = compile(cflist);
        current_context = oldcontext;star = s->addr;
        f->p = oldpos;
        exitfile();
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
    }
    return val;
}

void get_func_params(struct value_s *v, struct file_s *cfile) {
    struct value_s *val, new_value;
    size_t len = 0, i, j;
    str_t label;
    int w;

    new_value.u.func.param = NULL;
    for (i = 0;;i++) {
        ignore();if (!here() || here() == ';') break;
        if (i >= len) {
            len += 16;
            new_value.u.func.param = realloc(new_value.u.func.param, len * sizeof(new_value.u.func.param[0]));
            if (!new_value.u.func.param || len < 16 || len > ((size_t)~0) / sizeof(new_value.u.func.param[0])) err_msg_out_of_memory(); /* overflow */
        }
        new_value.u.func.param[i].epoint = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len) {
            str_cpy(&new_value.u.func.param[i].name, &label);
            for (j = 0; j < i; j++) if (new_value.u.func.param[j].name.data) {
                if (arguments.casesensitive) {
                    if (!str_cmp(&new_value.u.func.param[j].name, &label)) break;
                } else {
                    if (!str_casecmp(&new_value.u.func.param[j].name, &label)) break;
                }
            }
            if (j != i) {
                struct label_s tmp;
                tmp.name = new_value.u.func.param[j].name;
                tmp.file_list = v->u.func.label->file_list;
                tmp.epoint = new_value.u.func.param[j].epoint;
                err_msg_double_defined(&tmp, &label, &new_value.u.func.param[i].epoint);
            }
        } else {err_msg2(ERROR_GENERL_SYNTAX, NULL, &new_value.u.func.param[i].epoint);break;}
        new_value.u.func.param[i].init = NULL;
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            if (!get_exp(&w,1,cfile)) {
                i++;
                break;
            }
            if (!(val = get_val(NULL))) {
                err_msg(ERROR_GENERL_SYNTAX, NULL); 
                i++;
                break;
            }
            new_value.u.func.param[i].init = val_reference(val);
        }
        if (!here() || here() == ';') {
            i++;
            break;
        }
        if (here()!=',') {
            err_msg(ERROR_GENERL_SYNTAX, NULL);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i) {
            new_value.u.func.param = realloc(new_value.u.func.param, i * sizeof(new_value.u.func.param[0]));
            if (!new_value.u.func.param || i > ((size_t)~0) / sizeof(new_value.u.func.param[0])) err_msg_out_of_memory(); /* overflow */
        } else {
            free(new_value.u.func.param);
            new_value.u.func.param = NULL;
        }
    }
    v->u.func.argc = i;
    v->u.func.param = new_value.u.func.param;
}

void get_macro_params(struct value_s *v) {
    struct value_s new_value;
    size_t len = 0, i, j;
    str_t label;
    struct linepos_s *epoints = NULL;

    new_value.u.macro.param = NULL;
    for (i = 0;;i++) {
        ignore();if (!here() || here() == ';') break;
        if (i >= len) {
            len += 16;
            new_value.u.macro.param = realloc(new_value.u.macro.param, len * sizeof(new_value.u.macro.param[0]));
            if (!new_value.u.macro.param || len < 16 || len > ((size_t)~0) / sizeof(new_value.u.macro.param[0])) err_msg_out_of_memory(); /* overflow */
            epoints = (struct linepos_s *)realloc(epoints, len * sizeof(epoints[0]));
            if (!epoints || len > ((size_t)~0) / sizeof(epoints[0])) err_msg_out_of_memory(); /* overflow */
        }
        epoints[i] = lpoint;
        label.data = pline + lpoint.pos;
        label.len = get_label();
        if (label.len) {
            str_cpy(&new_value.u.macro.param[i].name, &label);
            for (j = 0; j < i; j++) if (new_value.u.macro.param[j].name.data) {
                if (arguments.casesensitive) {
                    if (!str_cmp(&new_value.u.macro.param[j].name, &label)) break;
                } else {
                    if (!str_casecmp(&new_value.u.macro.param[j].name, &label)) break;
                }
            }
            if (j != i) {
                struct label_s tmp;
                tmp.name = new_value.u.macro.param[j].name;
                tmp.file_list = v->u.macro.parent->file_list;
                tmp.epoint = epoints[j];
                err_msg_double_defined(&tmp, &label, &epoints[i]);
            }
        } else {new_value.u.macro.param[i].name.len = 0; new_value.u.macro.param[i].name.data = NULL;}
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            label.data = pline + lpoint.pos;
            label.len = macro_param_find();
            str_cpy(&new_value.u.macro.param[i].init, &label);
        } else {new_value.u.macro.param[i].init.len = 0; new_value.u.macro.param[i].init.data = NULL;}
        ignore();
        if (!here() || here() == ';') {
            i++;
            break;
        }
        if (here()!=',') {
            err_msg(ERROR_GENERL_SYNTAX, NULL);
            i++;
            break;
        }
        lpoint.pos++;
    }
    if (i != len) {
        if (i) {
            new_value.u.macro.param = realloc(new_value.u.macro.param, i * sizeof(new_value.u.macro.param[0]));
            if (!new_value.u.macro.param || i > ((size_t)~0) / sizeof(new_value.u.macro.param[0])) err_msg_out_of_memory(); /* overflow */
        } else {
            free(new_value.u.macro.param);
            new_value.u.macro.param = NULL;
        }
    }
    v->u.macro.argc = i;
    v->u.macro.param = new_value.u.macro.param;
    free(epoints);
}

struct value_s *function_recurse(struct value_s *tmp2, struct values_s *vals, unsigned int args, linepos_t epoint) {
    size_t i;
    struct label_s *label;
    struct value_s *val;
    struct value_s *retval = NULL;
    struct label_s *oldcontext = current_context;
    struct section_s rsection;
    struct section_s *oldsection = current_section;
    struct file_list_s *cflist;

    init_section2(&rsection);

    cflist = enterfile(tmp2->u.func.label->file_list->file, epoint);
    for (i = 0; i < tmp2->u.func.argc; i++) {
        int labelexists;
        val = (i < args) ? vals[i].val : tmp2->u.func.param[i].init ? tmp2->u.func.param[i].init : &none_value;
        label = new_label(&tmp2->u.func.param[i].name, tmp2->u.func.label, L_CONST, 0, &labelexists);
        label->ref=0;
        if (labelexists) {
            if (label->type != L_CONST || pass==1) {
                err_msg_double_defined(label, &tmp2->u.func.param[i].name, &tmp2->u.func.param[i].epoint);
            } else {
                label->requires = current_section->requires;
                label->conflicts = current_section->conflicts;
                var_assign(label, val, 0);
            }
        } else {
            label->requires = current_section->requires;
            label->conflicts = current_section->conflicts;
            label->usepass = pass;
            label->defpass = pass;
            label->value = val_reference(val);
            label->file_list = cflist;
            label->epoint = tmp2->u.func.param[i].epoint;
        }
    }
    {
        size_t oldpos;
        line_t lin = lpoint.line;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        struct linepos_s opoint = lpoint;
        const uint8_t *opline = pline;
        const uint8_t *ollist = llist;
        struct linepos_s nopoint = {0,0,0};

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, &lpoint);
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        lpoint.line = tmp2->u.func.label->epoint.line;
        new_waitfor(W_ENDF2, &nopoint);
        f = cflist->file;
        oldpos = f->p; f->p = tmp2->u.func.p;
        current_context = tmp2->u.func.label;
        temporary_label_branch++;
        current_section = &rsection;
        reset_section(current_section);
        current_section->provides = oldsection->provides; 
        current_section->requires = oldsection->requires;
        current_section->conflicts = oldsection->conflicts;
        current_section->l_start = current_section->l_address = star;
        current_section->dooutput = 0;
        retval = compile(cflist);
        current_section = oldsection;
        current_context = oldcontext;star = s->addr;
        temporary_label_branch--;
        lpoint = opoint;
        pline = opline;
        f->p = oldpos;
        star_tree = stree_old; vline = ovline;
        lpoint.line = lin;
        llist = ollist;
    }
    exitfile();
    destroy_variables2(tmp2->u.func.label);
    init_variables2(tmp2->u.func.label);
    destroy_section2(&rsection);
    return retval ? retval : &null_tuple;
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
