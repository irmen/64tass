/*
    $Id: mfuncobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "mfuncobj.h"
#include <string.h>
#include "values.h"
#include "eval.h"
#include "error.h"
#include "macro.h"
#include "file.h"

#include "typeobj.h"
#include "namespaceobj.h"
#include "listobj.h"

static Type mfunc_obj;
static Type sfunc_obj;

Type *const MFUNC_OBJ = &mfunc_obj;
Type *const SFUNC_OBJ = &sfunc_obj;

static FAST_CALL void destroy(Obj *o1) {
    Mfunc *v1 = Mfunc(o1);
    const struct file_s *cfile = v1->file_list->file;
    argcount_t i = v1->argc;
    size_t k;
    while ((i--) != 0) {
        struct mfunc_param_s *param = &v1->param[i];
        if (not_in_file(param->name.data, cfile)) free((char *)param->name.data);
        if (param->name.data != param->cfname.data) free((char *)param->cfname.data);
        if (param->init != NULL) val_destroy(param->init);
        if (param->type != NULL) val_destroy(param->type);
    }
    k = v1->nslen;
    while ((k--) != 0) {
        val_destroy(Obj(v1->namespaces[k]));
    }
    free(v1->namespaces);
    val_destroy(Obj(v1->names));
    val_destroy(Obj(v1->inamespaces));
    free(v1->param);
    if (v1->line != NULL) free((uint8_t *)v1->line);
}

static FAST_CALL void garbage(Obj *o1, int j) {
    Mfunc *v1 = Mfunc(o1);
    argcount_t i = v1->argc;
    size_t k;
    Obj *v ;
    const struct file_s *cfile;
    switch (j) {
    case -1:
        while ((i--) != 0) {
            v = v1->param[i].init;
            if (v != NULL) v->refcount--;
            v = v1->param[i].type;
            if (v != NULL) v->refcount--;
        }
        k = v1->nslen;
        while ((k--) != 0) {
            v = Obj(v1->namespaces[k]);
            v->refcount--;
        }
        v = Obj(v1->names);
        v->refcount--;
        v = Obj(v1->inamespaces);
        v->refcount--;
        return;
    case 0:
        cfile = v1->file_list->file;
        while ((i--) != 0) {
            struct mfunc_param_s *param = &v1->param[i];
            if (not_in_file(param->name.data, cfile)) free((char *)param->name.data);
            if (param->name.data != param->cfname.data) free((char *)param->cfname.data);
        }
        free(v1->param);
        free(v1->namespaces);
        if (v1->line != NULL) free((uint8_t *)v1->line);
        return;
    case 1:
        while ((i--) != 0) {
            v = v1->param[i].init;
            if (v != NULL) {
                if ((v->refcount & SIZE_MSB) != 0) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
            }
            v = v1->param[i].type;
            if (v != NULL) {
                if ((v->refcount & SIZE_MSB) != 0) {
                    v->refcount -= SIZE_MSB - 1;
                    v->obj->garbage(v, 1);
                } else v->refcount++;
            }
        }
        k = v1->nslen;
        while ((k--) != 0) {
            v = Obj(v1->namespaces[k]);
            if ((v->refcount & SIZE_MSB) != 0) {
                v->refcount -= SIZE_MSB - 1;
                v->obj->garbage(v, 1);
            } else v->refcount++;
        }
        v = Obj(v1->names);
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        v = Obj(v1->inamespaces);
        if ((v->refcount & SIZE_MSB) != 0) {
            v->refcount -= SIZE_MSB - 1;
            v->obj->garbage(v, 1);
        } else v->refcount++;
        return;
    }
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Mfunc *v1 = Mfunc(o1), *v2 = Mfunc(o2);
    argcount_t i;
    size_t k;
    if (o1->obj != o2->obj || v1->file_list != v2->file_list || v1->epoint.line != v2->epoint.line || v1->epoint.pos != v2->epoint.pos) return false;
    if (v1->argc != v2->argc || v1->nslen != v2->nslen) return false;
    for (i = 0; i < v1->argc; i++) {
        if (str_cmp(&v1->param[i].name, &v2->param[i].name) != 0) return false;
        if ((v1->param[i].name.data != v1->param[i].cfname.data || v2->param[i].name.data != v2->param[i].cfname.data) && str_cmp(&v1->param[i].cfname, &v2->param[i].cfname) != 0) return false;
        if (v1->param[i].init != v2->param[i].init && (v1->param[i].init == NULL || v2->param[i].init == NULL || !v1->param[i].init->obj->same(v1->param[i].init, v2->param[i].init))) return false;
        if (v1->param[i].type != v2->param[i].type && (v1->param[i].type == NULL || v2->param[i].type == NULL || !v1->param[i].type->obj->same(v1->param[i].type, v2->param[i].type))) return false;
        if (v1->param[i].epoint.pos != v2->param[i].epoint.pos) return false;
    }
    for (k = 0; k < v1->nslen; k++) {
        if (v1->namespaces[k] != v2->namespaces[k] && !v1->namespaces[k]->v.obj->same(Obj(v1->namespaces[k]), Obj(v2->namespaces[k]))) return false;
    }
    if (v1->names != v2->names && !v1->names->v.obj->same(Obj(v1->names), Obj(v2->names))) return false;
    if (v1->line != v2->line && (v1->line == NULL || v2->line == NULL || strcmp((const char *)v1->line, (const char *)v2->line) != 0)) return false;
    return true;
}

static MUST_CHECK Obj *contains(oper_t op) {
    Obj *o1 = op->v1;
    Mfunc *v2 = Mfunc(op->v2);

    switch (o1->obj->type) {
    case T_SYMBOL:
    case T_ANONSYMBOL:
        op->v2 = Obj(v2->names);
        return v2->names->v.obj->contains(op);
    case T_NONE:
    case T_ERROR:
        return val_reference(o1);
    default:
        return obj_oper_error(op);
    }
}

static MUST_CHECK Obj *calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_FUNCARGS:
        if (op->op == O_FUNC) {
            Mfunc *v1 = Mfunc(op->v1);
            Funcargs *v2 = Funcargs(op->v2);
            Obj *val;
            argcount_t i, max = 0, args = v2->len;
            for (i = args; i < v1->argc; i++) {
                if (v1->param[i].init == NULL) {
                    max = i + 1;
                }
            }
            if (max != 0) err_msg_argnum(args, max, v1->argc, op->epoint2);
            eval_enter();
            val = mfunc2_recurse(v1, v2, op->epoint);
            eval_leave();
            return (val != NULL) ? val : val_reference(null_tuple);
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(op->v2);
    default:
        if (op->op == O_MEMBER) {
            return namespace_member(op, Mfunc(op->v1)->names);
        }
        break;
    }
    return obj_oper_error(op);
}

void mfuncobj_init(void) {
    Type *type = new_type(&mfunc_obj, T_MFUNC, "function", sizeof(Mfunc));
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->calc2 = calc2;
    type->contains = contains;

    type = new_type(&sfunc_obj, T_SFUNC, "sfunction", sizeof(Sfunc));
    type->destroy = destroy;
    type->garbage = garbage;
    type->same = same;
    type->calc2 = calc2;
    type->contains = contains;
}

