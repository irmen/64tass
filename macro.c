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
#include "macro.h"
#include "inttypes.h"
#include <stdlib.h>
#include <string.h>
#include "misc.h"
#include "error.h"
#include "file.h"
#include "eval.h"
#include "section.h"

static struct {
    uint8_t p, len;
    struct {
        size_t len, size;
        struct {
            size_t len;
            const uint8_t *data;
        } *param, all;
        uint8_t *pline;
        struct value_s *macro;
    } *params, *current;
} macro_parameters = {0, 0, NULL, NULL};

//------------------------------------------------------------------------------
void mtranslate(void)
{
    uint_fast8_t q;
    uint_fast16_t j;
    size_t p;
    uint8_t ch, *cucc;

    if (!macro_parameters.p) return;

    cucc = macro_parameters.current->pline;

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
                    if (macro_parameters.current->macro->type == T_STRUCT || macro_parameters.current->macro->type == T_UNION) {
                        lpoint.pos++;
                        ch = '?';
                        goto ok;
                    }
                    err_msg(ERROR_MISSING_ARGUM,NULL);
                    break;
                }
                if (p + macro_parameters.current->param[j].len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    memcpy(cucc + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                    p += macro_parameters.current->param[j].len;
                }
                lpoint.pos++;continue;
            } else if (ch=='@') {
                /* \@ gives complete parameter list */
                if (p + macro_parameters.current->all.len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    memcpy(cucc + p, macro_parameters.current->all.data, macro_parameters.current->all.len);
                    p += macro_parameters.current->all.len;
                }
                lpoint.pos++;continue;
            } else {
                linepos_t e = lpoint;
                const char *label;
                size_t ln;
                lpoint.pos++;
                label = (const char *)pline + lpoint.pos;
                if (ch == '{') {
                    lpoint.pos++;
                    label++;
                    ln = get_label();
                    if (pline[lpoint.pos] == '}') lpoint.pos++;
                    else ln = 0;
                } else ln = get_label();
                if (ln) {
                    for (j = 0; j < macro_parameters.current->macro->u.macro.argc; j++) {
                        if (!macro_parameters.current->macro->u.macro.param[j].name) continue;
                        if (arguments.casesensitive) {
                            if (strncmp(macro_parameters.current->macro->u.macro.param[j].name, label, ln)) continue;
                        } else {
                            if (strncasecmp(macro_parameters.current->macro->u.macro.param[j].name, label, ln)) continue;
                        }
                        if (!macro_parameters.current->param[j].data) {
                            if (macro_parameters.current->macro->type == T_STRUCT || macro_parameters.current->macro->type == T_UNION) {
                                lpoint.pos--;
                                ch = '?';
                                goto ok;
                            }
                            err_msg2(ERROR_MISSING_ARGUM,NULL,e);
                            break;
                        }
                        if (p + macro_parameters.current->param[j].len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                        else {
                            memcpy(cucc + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                            p += macro_parameters.current->param[j].len;
                        }
                        break;
                    }
                    if (j < macro_parameters.current->macro->u.macro.argc) {
                        lpoint.pos--;
                        continue;
                    }
                    err_msg2(ERROR_MISSING_ARGUM,NULL,e);
                }
                ch='\\';lpoint = e;
            }
        } else if (ch=='@' && arguments.tasmcomp) {
            /* text parameter reference */
            if ((ch=pline[lpoint.pos+1])>='1' && ch<='9') {
                /* @1..@9 */
                if ((j=ch-'1') >= macro_parameters.current->len) {err_msg(ERROR_MISSING_ARGUM,NULL); break;}
                if (p + macro_parameters.current->param[j].len >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
                else {
                    if (macro_parameters.current->param[j].len > 1 && macro_parameters.current->param[j].data[0] == '"' && macro_parameters.current->param[j].data[macro_parameters.current->param[j].len-1]=='"') {
                        memcpy(cucc + p, macro_parameters.current->param[j].data + 1, macro_parameters.current->param[j].len - 2);
                        p += macro_parameters.current->param[j].len - 2;
                    } else {
                        memcpy(cucc + p, macro_parameters.current->param[j].data, macro_parameters.current->param[j].len);
                        p += macro_parameters.current->param[j].len;
                    }
                }
                lpoint.pos++;continue;
            } else ch='@';
        }
    ok:
        cucc[p++]=ch;
        if (p >= linelength) err_msg(ERROR_LINE_TOO_LONG,NULL);
    }
    cucc[p]=0;
    pline = cucc; lpoint = (linepos_t){0,0};
}

static struct value_s none_value = {T_NONE, 0, {}};
static struct value_s null_tuple = {T_TUPLE, 0, {}};

static size_t macro_param_find(void) {
    uint_fast8_t q = 0, ch;
    uint8_t pp = 0;
    char par[256];

    linepos_t opoint2, npoint2;
    opoint2.pos = lpoint.pos;
    while ((ch=here()) && (q || (ch!=';' && (ch!=',' || pp)))) {
        if (ch == '"'  && !(q & 2)) { q^=1; }
        else if (ch == '\'' && !(q & 1)) { q^=2; }
        if (!q) {
            if (ch == '(' || ch =='[') par[pp++]=ch;
            else if (pp && ((ch == ')' && par[pp-1]=='(') || (ch == ']' && par[pp-1]=='['))) pp--;
        }
        lpoint.pos++;
    }
    npoint2.pos = lpoint.pos;
    while (npoint2.pos > opoint2.pos && (pline[npoint2.pos-1] == 0x20 || pline[npoint2.pos-1] == 0x09)) npoint2.pos--;
    return npoint2.pos - opoint2.pos;
}

void macro_recurse(enum wait_e t, struct value_s *tmp2) {
    if (macro_parameters.p>100) {
        err_msg(ERROR__MACRECURSION,"!!!!");
        return;
    }
    if (macro_parameters.p >= macro_parameters.len) {
        macro_parameters.len += 1;
        macro_parameters.params = realloc(macro_parameters.params, sizeof(*macro_parameters.params) * macro_parameters.len);
        if (!macro_parameters.params) err_msg_out_of_memory();
        macro_parameters.params[macro_parameters.p].param = NULL;
        macro_parameters.params[macro_parameters.p].size = 0;
        macro_parameters.params[macro_parameters.p].pline = malloc(linelength);
    }
    macro_parameters.current = &macro_parameters.params[macro_parameters.p];
    macro_parameters.current->macro = val_reference(tmp2);
    macro_parameters.p++;
    {
        linepos_t opoint, npoint;
        size_t p = 0;

        ignore(); opoint = lpoint;
        for (;;) {
            if ((!here() || here()==';') && p >= tmp2->u.macro.argc) break;
            if (p >= macro_parameters.current->size) {
                if (macro_parameters.current->size < tmp2->u.macro.argc) macro_parameters.current->size = tmp2->u.macro.argc;
                else macro_parameters.current->size += 4;
                macro_parameters.current->param = realloc(macro_parameters.current->param, sizeof(*macro_parameters.current->param) * macro_parameters.current->size);
                if (!macro_parameters.current->param) err_msg_out_of_memory();
            }
            macro_parameters.current->param[p].data = pline + lpoint.pos;
            macro_parameters.current->param[p].len = macro_param_find();
            if (!macro_parameters.current->param[p].len) {
                if (p < tmp2->u.macro.argc) {
                    macro_parameters.current->param[p].data = (uint8_t *)tmp2->u.macro.param[p].init;
                    if (macro_parameters.current->param[p].data) macro_parameters.current->param[p].len = strlen(tmp2->u.macro.param[p].init);
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
        compile(tmp2->u.macro.file);
    } else {
        size_t oldpos = tmp2->u.macro.file->p;
        line_t lin = sline;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        enterfile(tmp2->u.macro.file->realname, sline);
        sline = tmp2->u.macro.sline;
        new_waitfor(t, (linepos_t){0,0});
        f = tmp2->u.macro.file;
        f->p = tmp2->u.macro.p;
        compile(f);
        f->p = oldpos;
        exitfile();
        star_tree = stree_old; vline = ovline;
        sline = lin;
    }
    val_destroy(macro_parameters.current->macro);
    macro_parameters.p--;
    if (macro_parameters.p) macro_parameters.current = &macro_parameters.params[macro_parameters.p - 1];
}

void func_recurse(enum wait_e t, struct value_s *tmp2) {
    size_t i;
    int w;
    struct label_s *label;
    struct value_s *val;
    int fin = 0;
    linepos_t epoint;

    for (i = 0; i < tmp2->u.func.argc; i++) {
        int labelexists;
        label=find_label2(tmp2->u.func.param[i].name, &current_context->members);
        ignore();if (!here() || here()==';') fin++;
        if (tmp2->u.func.param[i].init) {
            if (here()==',' || !here() || here()==';') {
                val = tmp2->u.func.param[i].init;
            } else if (get_exp(&w,1)) {
                if (!(val = get_val(T_IDENTREF, &epoint))) {
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    val = tmp2->u.func.param[i].init;
                }
            } else val = tmp2->u.func.param[i].init;
            ignore();if (here()==',') lpoint.pos++;
        } else {
            if (fin > 1) {err_msg(ERROR______EXPECTED,","); val = &none_value;}
            else if (get_exp(&w,1)) {
                if (!(val = get_val(T_IDENTREF, &epoint))) {
                    err_msg(ERROR_GENERL_SYNTAX,NULL);
                    val = &none_value;
                }
            } else val = &none_value;
            ignore();if (here()==',') lpoint.pos++;
        }
        if (label) labelexists = 1;
        else label = new_label(tmp2->u.func.param[i].name, L_CONST, &labelexists);
        label->ref=0;
        if (labelexists) {
            if (label->type != L_CONST || pass==1) err_msg_double_defined(label->name, label->file->realname, label->sline, label->epoint, tmp2->u.func.param[i].name, epoint);
            else {
                label->requires=current_section->requires;
                label->conflicts=current_section->conflicts;
                var_assign(label, val, 0);
            }
        } else {
            label->requires = current_section->requires;
            label->conflicts = current_section->conflicts;
            label->pass = pass;
            label->upass = pass;
            label->value = val_reference(val);
            label->file = tmp2->u.func.file;
            label->sline = tmp2->u.func.sline;
            label->epoint = tmp2->u.func.param[i].epoint;
        }
    }
    if (i == tmp2->u.func.argc && here() && here()!=';') err_msg(ERROR_EXTRA_CHAR_OL,NULL);
    {
        size_t oldpos = tmp2->u.func.file->p;
        line_t lin = sline;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        enterfile(tmp2->u.func.file->realname, sline);
        sline = tmp2->u.func.sline;
        new_waitfor(t, (linepos_t){0,0});
        f = tmp2->u.func.file;
        f->p = tmp2->u.func.p;
        val = compile(f);
        if (val) val_destroy(val);
        f->p = oldpos;
        exitfile();
        star_tree = stree_old; vline = ovline;
        sline = lin;
    }
}

void get_func_params(struct value_s *v) {
    static struct value_s *val, new_value;
    static size_t len, i, j, ln;
    int w;
    char *s;

    for (i = 0;;i++) {
        ignore();if (!here() || here() == ';') break;
        if (i >= len) {
            len += 16;
            new_value.u.func.param = realloc(new_value.u.func.param, len * sizeof(new_value.u.func.param[0]));
            if (!new_value.u.func.param) err_msg_out_of_memory();
        }
        new_value.u.func.param[i].epoint = lpoint;
        ln = get_label();
        if (ln) {
            s = (char *)malloc(ln + 1);
            if (!s) err_msg_out_of_memory();
            memcpy(s, pline + new_value.u.func.param[i].epoint.pos, ln);
            s[ln] = 0;
            for (j = 0; j < i; j++) if (new_value.u.func.param[j].name) {
                if (arguments.casesensitive) {
                    if (!strcmp(new_value.u.func.param[j].name, s)) break;
                } else {
                    if (!strcasecmp(new_value.u.func.param[j].name, s)) break;
                }
            }
            if (j != i) err_msg2(ERROR_DOUBLE_DEFINE, s, new_value.u.func.param[i].epoint);
        } else {err_msg2(ERROR_GENERL_SYNTAX, NULL, new_value.u.func.param[i].epoint);break;}
        new_value.u.func.param[i].name = s;
        new_value.u.func.param[i].init = NULL;
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            if (!get_exp(&w,1)) {
                i++;
                break;
            }
            if (!(val = get_val(T_IDENTREF, NULL))) {
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
            if (!new_value.u.func.param) err_msg_out_of_memory();
        } else {
            free(new_value.u.func.param);
            new_value.u.func.param = NULL;
        }
    }
    v->u.func.argc = i;
    v->u.func.param = new_value.u.func.param;
    len = 0;
    new_value.u.func.param = NULL;
}

void get_macro_params(struct value_s *v) {
    static struct value_s new_value;
    static size_t len, i, j, ln;
    char *s;

    for (i = 0;;i++) {
        linepos_t epoint;
        ignore();if (!here() || here() == ';') break;
        if (i >= len) {
            len += 16;
            new_value.u.macro.param = realloc(new_value.u.macro.param, len * sizeof(new_value.u.macro.param[0]));
            if (!new_value.u.macro.param) err_msg_out_of_memory();
        }
        epoint = lpoint;
        ln = get_label();
        if (ln) {
            s = (char *)malloc(ln + 1);
            if (!s) err_msg_out_of_memory();
            memcpy(s, pline + epoint.pos, ln);
            s[ln] = 0;
            for (j = 0; j < i; j++) if (new_value.u.macro.param[j].name) {
                if (arguments.casesensitive) {
                    if (!strcmp(new_value.u.macro.param[j].name, s)) break;
                } else {
                    if (!strcasecmp(new_value.u.macro.param[j].name, s)) break;
                }
            }
            if (j != i) err_msg2(ERROR_DOUBLE_DEFINE, s, epoint);
        } else s = NULL;
        new_value.u.macro.param[i].name = s;
        ignore();
        if (here() == '=') {
            lpoint.pos++;
            epoint.pos = lpoint.pos;
            ln = macro_param_find();
            s = (char *)malloc(ln + 1);
            if (!s) err_msg_out_of_memory();
            memcpy(s, pline + epoint.pos, ln);
            s[ln] = 0;
        } else s = NULL;
        new_value.u.macro.param[i].init = s;
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
            if (!new_value.u.macro.param) err_msg_out_of_memory();
        } else {
            free(new_value.u.macro.param);
            new_value.u.macro.param = NULL;
        }
    }
    v->u.macro.argc = i;
    v->u.macro.param = new_value.u.macro.param;
    len = 0;
    new_value.u.macro.param = NULL;
}

struct value_s *function_recurse(struct value_s *tmp2, struct values_s *vals, unsigned int args) {
    size_t i;
    struct label_s *label;
    struct value_s *val;
    struct value_s *retval = NULL;
    struct label_s rlabel;
    struct label_s *oldcontext = current_context;
    struct section_s rsection;
    struct section_s *oldsection = current_section;
    init_variables2(&rlabel);
    current_context = &rlabel;
    rlabel.parent = tmp2->u.func.context;
    init_section2(&rsection);
    current_section = &rsection;
    reset_section();
    current_section->dooutput = 0;

    enterfile(tmp2->u.func.file->realname, sline);
    for (i = 0; i < tmp2->u.func.argc; i++) {
        int labelexists;
        val = (i < args) ? vals[i].val : tmp2->u.func.param[i].init ? tmp2->u.func.param[i].init : &none_value;
        label = new_label(tmp2->u.func.param[i].name, L_CONST, &labelexists);
        label->ref=0;
        if (labelexists) {
            if (label->type != L_CONST || pass==1) {
                line_t oline = sline;
                sline = tmp2->u.func.sline;
                err_msg_double_defined(label->name, label->file->realname, label->sline, label->epoint, tmp2->u.func.param[i].name, tmp2->u.func.param[i].epoint);
                sline = oline;
            }
            else {
                label->requires=current_section->requires;
                label->conflicts=current_section->conflicts;
                var_assign(label, val, 0);
            }
        } else {
            label->requires = current_section->requires;
            label->conflicts = current_section->conflicts;
            label->pass = pass;
            label->upass = pass;
            label->value = val_reference(val);
            label->file = tmp2->u.func.file;
            label->sline = tmp2->u.func.sline;
            label->epoint = tmp2->u.func.param[i].epoint;
        }
    }
    {
        size_t oldpos = tmp2->u.func.file->p;
        line_t lin = sline;
        struct file_s *f;
        int labelexists;
        struct star_s *s = new_star(vline, &labelexists);
        struct avltree *stree_old = star_tree;
        line_t ovline = vline;
        const uint8_t *ollist = llist;

        if (labelexists && s->addr != star) {
            if (fixeddig && pass > MAX_PASS) err_msg(ERROR_CANT_CALCULAT, "");
            fixeddig=0;
        }
        s->addr = star;
        star_tree = &s->tree;vline=0;
        sline = tmp2->u.func.sline;
        new_waitfor(W_ENDF2, (linepos_t){0,0});
        f = tmp2->u.func.file;
        f->p = tmp2->u.func.p;
        retval = compile(f);
        f->p = oldpos;
        star_tree = stree_old; vline = ovline;
        sline = lin;
        llist = ollist;
    }
    exitfile();
    current_context = oldcontext;
    destroy_variables2(&rlabel);
    current_section = oldsection;
    destroy_section2(&rsection);
    return retval ? retval : &null_tuple;
}

void init_macro(void) {
    macro_parameters.p = 0;
}

void free_macro(void) {
    int i;
    for (i = 0; i < macro_parameters.len; i++) {
        free(macro_parameters.params[i].pline);
        free(macro_parameters.params[i].param);
    }
    free(macro_parameters.params);
}