/*
    $Id: encobj.c 3086 2023-09-03 06:23:08Z soci $

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
#include "encobj.h"
#include <string.h>
#include "values.h"
#include "ternary.h"
#include "avl.h"
#include "str.h"
#include "error.h"
#include "64tass.h"
#include "encoding.h"
#include "unicode.h"
#include "eval.h"

#include "typeobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "errorobj.h"
#include "functionobj.h"

Enc *actual_encoding;
size_t efwcount;

static Type obj;

Type *const ENC_OBJ = &obj;

static void escape_free(void *);
static void ranges_free(struct avltree_node *);

static FAST_CALL void destroy(Obj *o1) {
    Enc *v1 = Enc(o1);
    if (v1->escapes != NULL) ternary_cleanup(v1->escapes, escape_free);
    avltree_destroy(&v1->ranges, ranges_free);
    free(v1->map);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

MALLOC Obj *new_enc(const struct file_list_s *file_list, linepos_t epoint) {
    Enc *enc = Enc(val_alloc(ENC_OBJ));
    enc->file_list = file_list;
    enc->epoint = *epoint;
    enc->escapes = NULL;
    enc->escape_length = SIZE_MAX;
    enc->escape_char = 256;
    enc->epass = 0;
    enc->updating = false;
    avltree_init(&enc->ranges);
    new_array(&enc->map, 128);
    memset(enc->map, 0, 128 * sizeof *enc->map);
    return Obj(enc);
}

struct trans_s {
    struct character_range_s range;
    uint8_t pass;
    uint8_t fwpass;
    struct avltree_node node;
};

#ifdef DEBUG
#define trans_free(trans) free(trans)
static MALLOC struct trans_s *trans_alloc(void) {
    struct trans_s *r;
    new_instance(&r);
    return r;
}
#else
static union trans_u {
    struct trans_s trans;
    union trans_u *next;
} *transs_free;

static struct transs_s {
    union trans_u transs[31];
    struct transs_s *next;
} *transs;

static void trans_free(union trans_u *trans) {
    trans->next = transs_free;
    transs_free = trans;
}

static union trans_u *transs_alloc(void) {
    size_t i;
    struct transs_s *old = transs;
    new_instance(&transs);
    for (i = 0; i < 30; i++) {
        transs->transs[i].next = &transs->transs[i + 1];
    }
    transs->transs[i].next = NULL;
    transs->next = old;
    return &transs->transs[0];
}

static MALLOC struct trans_s *trans_alloc(void) {
    struct trans_s *trans;
    if (transs_free == NULL) transs_free = transs_alloc();
    trans = (struct trans_s *)transs_free;
    transs_free = transs_free->next;
    return trans;
}
#endif

static void ranges_free(struct avltree_node *aa)
{
    struct trans_s *a = avltree_container_of(aa, struct trans_s, node);
    trans_free((union trans_u *)a);
}

static FAST_CALL int trans_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct trans_s *a = cavltree_container_of(aa, struct trans_s, node);
    const struct trans_s *b = cavltree_container_of(bb, struct trans_s, node);

    if (a->range.start > b->range.end) {
        return -1;
    }
    if (a->range.end < b->range.start) {
        return 1;
    }
    return 0;
}

static struct trans_s *lasttr;
bool enc_trans_add(Enc *enc, const struct character_range_s *range, linepos_t epoint)
{
    struct avltree_node *b;
    struct trans_s *tmp;
    if (lasttr == NULL) lasttr = trans_alloc();
    lasttr->range = *range;
retry:
    b = avltree_insert(&lasttr->node, &enc->ranges, trans_compare);
    if (b == NULL) { /* new encoding */
        tmp = lasttr;
        lasttr = NULL;
        tmp->fwpass = 0;
        tmp->pass = pass;
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
        return false;
    }
    tmp = avltree_container_of(b, struct trans_s, node);
    if (tmp->range.start != range->start || tmp->range.end != range->end) {
        if (tmp->pass >= pass) {
            return true;
        }
        if (tmp->fwpass == pass) efwcount--;
        avltree_remove(b, &enc->ranges);
        goto retry;
    }
    if (tmp->pass >= pass) {
        return (tmp->range.offset != range->offset);
    }
    tmp->pass = pass;
    if (tmp->fwpass == pass) efwcount--;
    if (tmp->range.offset != range->offset) {
        tmp->range.offset = range->offset;
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    }
    return false;
}

struct escape_s {
    size_t len;
    uint8_t val[6];
    uint8_t pass;
    uint8_t fwpass;
    uint8_t *data;
};

static void escape_free(void *e) {
    struct escape_s *esc;
    size_t i = (size_t)((const char *)e - identmap);
    if (i < 256) return;
    esc = (struct escape_s *)e;
    if (esc->data != esc->val) free(esc->data);
    free(esc);
}

bool enc_escape_add(Enc *enc, const str_t *v, Obj *val, linepos_t epoint)
{
    struct escape_s **b2, *b, tmp;
    Obj *val2;
    size_t i;
    uint8_t *d;
    bool ret;
    struct iter_s iter;

    b2 = (struct escape_s **)ternary_insert(&enc->escapes, v->data, v->data + v->len);
    if (b2 == NULL) err_msg_out_of_memory();
    b = *b2;
    *b2 = NULL;

    if (val->obj == STR_OBJ || val->obj == BITS_OBJ) {
        bool old = actual_encoding->updating;
        actual_encoding->updating = true;
        val2 = bytes_from_obj(val, epoint);
        actual_encoding->updating = old;
        iter.data = val2; val2->obj->getiter(&iter);
        val_destroy(val2);
    } else {
        iter.data = val; val->obj->getiter(&iter);
    }

    if (iter.len <= lenof(tmp.val)) {
        d = tmp.val;
    } else {
        new_array(&d, iter.len);
    }
    for (i = 0; i < iter.len && (val2 = iter.next(&iter)) != NULL; i++) {
        uval_t uval;
        Error *err = val2->obj->uval(val2, &uval, 8, epoint);
        if (err != NULL) {
            err_msg_output_and_destroy(err);
            uval = 0;
        }
        d[i] = (uint8_t)uval;
    }
    iter_destroy(&iter);

    if (b == NULL) { /* new escape */
        new_instance(&b);
        if (d == tmp.val) {
            if (i != 0) memcpy(b->val, d, i);
            d = b->val;
        }
        b->len = i;
        b->data = d;
        b->fwpass = 0;
        b->pass = pass;
        *b2 = b;
        if (v->len < enc->escape_length) enc->escape_length = v->len;
        if (enc->escape_char != v->data[0]) {
            if (enc->escape_char == 256) {
                enc->escape_char = v->data[0];
            } else {
                enc->escape_char = 257;
            }
        }

        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
        return false;
    }
    *b2 = b;
    if (i == 1) {
        size_t j = (size_t)((const char *)b - identmap);
        if (j < 256) {
            return b != (struct escape_s *)(identmap + tmp.val[0]);
        }
        ret = (b->len != 1 || *d != *b->data);
    } else {
        ret = (i != b->len || memcmp(d, b->data, i) != 0);
    }
    if (b->pass == pass) {
        if (tmp.val != d) free(d);
        return ret;            /* already exists */
    }
    b->pass = pass;
    if (b->fwpass == pass) efwcount--;
    if (ret) {
        if (b->data != b->val) free(b->data);
        if (d == tmp.val) {
            if (i != 0) memcpy(b->val, d, i);
            d = b->val;
        }
        b->len = i;
        b->data = d;
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    } else {
        if (tmp.val != d) free(d);
    }
    return false;
}

struct encoder_s {
    Enc *enc;
    size_t i, i2, j, len, len2;
    bool err;
    const uint8_t *data, *data2;
    linepos_t epoint;
};

struct encoder_s *enc_string_init(Enc *enc, const Str *v, linepos_t epoint) {
    static struct encoder_s encoder;
    encoder.enc = enc;
    encoder.i = 0;
    encoder.j = 0;
    encoder.len2 = 0;
    encoder.len = v->len;
    encoder.data = v->data;
    encoder.epoint = epoint;
    encoder.err = false;
    return &encoder;
}

void enc_error(struct encoder_s *encoder, Error_types no) {
    if (!encoder->err) {
        struct linepos_s epoint;
        encoder->err = true;
        epoint.line = encoder->epoint->line;
        epoint.pos = interstring_position(encoder->epoint, encoder->data, encoder->i2);
        err_msg2(no, NULL, &epoint);
    }
}

int enc_string(struct encoder_s *encoder) {
    Enc *enc;
    unichar_t ch;
    unsigned int ln;
    struct avltree_node *c;
    struct trans_s tmp;

    if (encoder->j < encoder->len2) {
        return encoder->data2[encoder->j++];
    }
    enc= encoder->enc;
next:
    if (encoder->i >= encoder->len) return EOF;
    encoder->i2 = encoder->i;
    ch = encoder->data[encoder->i];
    if ((ch == enc->escape_char || enc->escape_char == 257) && encoder->len - encoder->i >= enc->escape_length) {
        size_t len = encoder->len - encoder->i;
        struct escape_s *e = (struct escape_s *)ternary_search(enc->escapes, encoder->data + encoder->i, &len);
        if (e != NULL) {
            size_t i = (size_t)((const char *)e - identmap);
            if (i < 256) {
                encoder->i += len;
                return (int)i;
            }
            if (e->pass >= pass || !fixeddig || e->pass == pass - 1) {
                encoder->i += len;
                if (e->pass == pass - 1 && e->fwpass != pass) {
                    e->fwpass = pass;
                    efwcount++;
                }
                if (e->len < 1) goto next;
                encoder->len2 = e->len;
                encoder->j = 1;
                encoder->data2 = e->data;
                return e->data[0];
            }
        }
    }
    if ((ch & 0x80) != 0) ln = utf8in(encoder->data + encoder->i, &ch); else {
        struct transmap_s *map = &enc->map[ch];
        if (map->pass >= pass) {
            encoder->i++;
            return map->value;
        }
        ln = 1;
    }
    tmp.range.start = tmp.range.end = ch & 0xffffff;

    c = avltree_lookup(&tmp.node, &enc->ranges, trans_compare);
    if (c != NULL) {
        struct trans_s *t = avltree_container_of(c, struct trans_s, node);
        if (tmp.range.start >= t->range.start && tmp.range.end <= t->range.end) {
            if (t->pass >= pass || !fixeddig || t->pass == pass - 1) {
                encoder->i += ln;
                if (t->pass == pass - 1 && t->fwpass != pass) {
                    t->fwpass = pass;
                    efwcount++;
                }
                if (ch < 128) {
                    struct transmap_s *map = &enc->map[ch];
                    map->pass = t->pass;
                    map->value = (uint8_t)(ch - t->range.start + t->range.offset);
                }
                return (uint8_t)(ch - t->range.start + t->range.offset);
            }
        }
    }
    if (!encoder->err) {
        encoder->err = true;
        if (encoder->enc->epass != pass) {
            struct linepos_s epoint;
            encoder->enc->epass = pass;
            epoint.line = encoder->epoint->line;
            epoint.pos = interstring_position(encoder->epoint, encoder->data, encoder->i);
            err_msg_unknown_char(ch, &epoint);
            if (enc->epoint.line != 0) err_msg_encode_definition_note(enc->file_list, &enc->epoint);
        }
    }
    encoder->i += ln;
    return 256 + '?';
}

static MUST_CHECK Obj *convert(oper_t op) {
    Error *err;
    Obj *o2 = op->v2;
    switch (o2->obj->type) {
    case T_NONE:
    case T_ERROR: return val_reference(o2);
    case T_STR: return bytes_from_str(Str(o2), op->epoint2, BYTES_MODE_TEXT);
    default: break;
    }
    err = new_error(ERROR____WRONG_TYPE, op->epoint2);
    err->u.otype.t1 = o2->obj;
    err->u.otype.t2 = STR_OBJ;
    return Obj(err);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o2 = op->v2;
    switch (o2->obj->type) {
    case T_FUNCARGS:
        if (op->op == O_FUNC) {
            Enc *oldenc;
            Funcargs *v2 = Funcargs(o2);
            struct values_s *v = v2->val;
            argcount_t args = v2->len;
            if (args != 1) {
                return new_error_argnum(args, 1, 1, op->epoint2);
            }
            oldenc = actual_encoding;
            actual_encoding = Enc(op->v1);
            op->v2 = v->val;
            op->inplace = NULL;
            o2 = apply_function(op, convert);
            actual_encoding = oldenc;
            return o2;
        }
        break;
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != O_MEMBER && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

void encobj_init(void) {
    Type *type = new_type(&obj, T_ENC, "enc", sizeof(Enc));
    type->destroy = destroy;
    type->same = same;
    type->calc2 = calc2;

    actual_encoding = Enc(val_alloc(ENC_OBJ));
    actual_encoding->escapes = NULL;
    actual_encoding->map = NULL;
    avltree_init(&actual_encoding->ranges);
    lasttr = NULL;
#ifndef DEBUG
    transs_free = NULL;
    transs = NULL;
#endif
}

void encobj_destroy(void) {
    val_destroy(Obj(actual_encoding));
}

void destroy_transs(void) {
#ifndef DEBUG
    while (transs != NULL) {
        struct transs_s *old = transs;
        transs = transs->next;
        free(old);
    }
#endif
}
