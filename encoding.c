/*
    $Id: encoding.c 2570 2021-04-11 22:11:00Z soci $

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
#include "encoding.h"
#include "encodings.h"
#include "error.h"
#include "string.h"
#include "ternary.h"
#include "unicode.h"
#include "values.h"
#include "64tass.h"
#include "avl.h"
#include "str.h"

#include "strobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "typeobj.h"
#include "errorobj.h"

struct encoding_s *actual_encoding;
size_t efwcount;

struct map_s {
    uint8_t value;
    uint8_t pass;
};

struct encoding_s {
    str_t name;
    str_t cfname;
    bool failed;
    uint16_t escape_char;
    ternary_tree escapes;
    size_t escape_length;
    struct avltree ranges;
    struct avltree_node node;
    struct map_s map[128];
};

struct escape_s {
    size_t len;
    uint8_t val[6];
    uint8_t pass;
    uint8_t fwpass;
    uint8_t *data;
};

static struct avltree encoding_tree;

static FAST_CALL int encoding_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct encoding_s *a = cavltree_container_of(aa, struct encoding_s, node);
    const struct encoding_s *b = cavltree_container_of(bb, struct encoding_s, node);

    return str_cmp(&a->cfname, &b->cfname);
}

static void escape_free(void *e) {
    struct escape_s *esc;
    size_t i = (size_t)((const uint8_t *)e - identmap);
    if (i < 256) return;
    esc = (struct escape_s *)e;
    if (esc->data != esc->val) free(esc->data);
    free(esc);
}

static void encoding_free(struct avltree_node *aa)
{
    struct encoding_s *a = avltree_container_of(aa, struct encoding_s, node);

    free((char *)a->name.data);
    if (a->name.data != a->cfname.data) free((uint8_t *)a->cfname.data);
    if (a->escapes != NULL) ternary_cleanup(a->escapes, escape_free);
    free(a);
}

static bool ascii_mode;
static struct encoding_s *lasten = NULL;
struct encoding_s *new_encoding(const str_t *name, linepos_t epoint)
{
    struct avltree_node *b;
    struct encoding_s *tmp;

    if (lasten == NULL) {
        lasten = (struct encoding_s *)mallocx(sizeof *lasten);
    }
    str_cfcpy(&lasten->cfname, name);
    b = avltree_insert(&lasten->node, &encoding_tree, encoding_compare);
    if (b == NULL) { /* new encoding */
        str_cpy(&lasten->name, name);
        if (lasten->cfname.data == name->data) lasten->cfname = lasten->name;
        else str_cfcpy(&lasten->cfname, NULL);
        lasten->escapes = NULL;
        lasten->escape_length = SIZE_MAX;
        lasten->escape_char = 256;
        lasten->failed = false;
        avltree_init(&lasten->ranges);
        memset(lasten->map, 0, sizeof(lasten->map));
        tmp = lasten;
        lasten = NULL;
        enctables(tmp, &tmp->cfname, ascii_mode);
        return tmp;
    }
    tmp = avltree_container_of(b, struct encoding_s, node);
    if (tmp->failed && tmp->escapes == NULL && tmp->ranges.root == NULL) err_msg2(ERROR__EMPTY_ENCODI, NULL, epoint);
    return tmp;            /* already exists */
}

struct trans_s {
    struct character_range_s range;
    uint8_t pass;
    uint8_t fwpass;
    struct avltree_node node;
};

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

static struct transs_s {
    struct trans_s transs[31];
    struct transs_s *next;
} *transs = NULL;

static unsigned int transs_i = lenof(transs->transs);

static struct trans_s *lasttr = NULL;
bool new_trans(struct encoding_s *enc, const struct character_range_s *range, linepos_t epoint)
{
    struct avltree_node *b;
    struct trans_s *tmp;
    if (lasttr == NULL) {
        if (transs_i == lenof(transs->transs)) {
            struct transs_s *old = transs;
            transs = (struct transs_s *)mallocx(sizeof *transs);
            transs->next = old;
            transs_i = 0;
        }
        lasttr = &transs->transs[transs_i++];
    }
    lasttr->range = *range;
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
        return true;
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

bool new_escape(struct encoding_s *enc, const str_t *v, Obj *val, linepos_t epoint)
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
        val2 = bytes_from_obj(val, epoint);
        iter.data = val2; val2->obj->getiter(&iter); 
        val_destroy(val2);
    } else {
        iter.data = val; val->obj->getiter(&iter); 
    }

    d = (iter.len <= lenof(tmp.val)) ? tmp.val : (uint8_t *)mallocx(iter.len * sizeof *d);
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
        b = (struct escape_s *)mallocx(sizeof *b);
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
        size_t j = (size_t)((const uint8_t *)b - identmap);
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
        b->data = d;
        if (fixeddig && pass > max_pass) err_msg_cant_calculate(NULL, epoint);
        fixeddig = false;
    } else {
        if (tmp.val != d) free(d);
    }
    return false;
}

void add_esc(struct encoding_s *enc, const char *s) {
    enc->escape_char = '{';
    while (s[1] != 0) {
        size_t len = strlen(s + 1);
        const uint8_t **b = (const uint8_t **)ternary_insert(&enc->escapes, (const uint8_t*)s + 1, (const uint8_t*)s + 1 + len);
        if (b == NULL) err_msg_out_of_memory();
        *b = identmap + (uint8_t)s[0];
        if (enc->escape_length > len) enc->escape_length = len;
        s += len + 2;
    }
}

struct encoder_s {
    struct encoding_s *encoding;
    size_t i, i2, j, len, len2;
    bool err;
    const uint8_t *data, *data2;
    linepos_t epoint;
};

struct encoder_s *encode_string_init(const Str *v, linepos_t epoint) {
    static struct encoder_s encoder;
    encoder.encoding = actual_encoding;
    encoder.i = 0;
    encoder.j = 0;
    encoder.len2 = 0;
    encoder.len = v->len;
    encoder.data = v->data;
    encoder.epoint = epoint;
    encoder.err = false;
    return &encoder;
}

void encode_error(struct encoder_s *encoder, Error_types no) {
    if (!encoder->err) {
        struct linepos_s epoint = *encoder->epoint;
        epoint.pos = interstring_position(&epoint, encoder->data, encoder->i2);
        err_msg2(no, NULL, &epoint);
        encoder->err = true;
    }
}

int encode_string(struct encoder_s *encoder) {
    struct encoding_s *encoding;
    uchar_t ch;
    unsigned int ln;
    struct avltree_node *c;
    struct trans_s tmp;

    if (encoder->j < encoder->len2) {
        return encoder->data2[encoder->j++];
    }
    encoding = encoder->encoding;
next:
    if (encoder->i >= encoder->len) return EOF;
    encoder->i2 = encoder->i;
    ch = encoder->data[encoder->i];
    if ((ch == encoding->escape_char || encoding->escape_char == 257) && encoder->len - encoder->i >= encoding->escape_length) {
        size_t len = encoder->len - encoder->i;
        struct escape_s *e = (struct escape_s *)ternary_search(encoding->escapes, encoder->data + encoder->i, &len);
        if (e != NULL) {
            size_t i = (size_t)((const uint8_t *)e - identmap);
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
        struct map_s *map = &encoding->map[ch];
        if (map->pass >= pass) {
            encoder->i++;
            return map->value;
        }
        ln = 1;
    }
    tmp.range.start = tmp.range.end = ch & 0xffffff;

    c = avltree_lookup(&tmp.node, &encoding->ranges, trans_compare);
    if (c != NULL) {
        struct trans_s *t = avltree_container_of(c, struct trans_s, node);
        if (tmp.range.start >= t->range.start && tmp.range.end <= t->range.end) {
            if (t->pass >= pass || !fixeddig || t->pass == pass - 1) {
                encoder->i += ln;
                if (t->pass == pass - 1 && t->fwpass != pass) {
                    t->fwpass = pass;
                    efwcount++;
                }
                if (ch < lenof(encoding->map)) {
                    struct map_s *map = &encoding->map[ch];
                    map->pass = t->pass;
                    map->value = (uint8_t)(ch - t->range.start + t->range.offset);
                }
                return (uint8_t)(ch - t->range.start + t->range.offset);
            }
        }
    }
    if (!encoder->err && (!(encoding->escapes == NULL && encoding->ranges.root == NULL) || !encoding->failed)) {
        struct linepos_s epoint = *encoder->epoint;
        epoint.pos = interstring_position(&epoint, encoder->data, encoder->i);
        err_msg_unknown_char(ch, &encoding->name, &epoint);
        encoding->failed = true;
        encoder->err = true;
    }
    encoder->i += ln;
    return 256 + '?';
}

void init_encoding(bool toascii)
{
    avltree_init(&encoding_tree);
    ascii_mode = toascii;
}

void destroy_encoding(void)
{
    avltree_destroy(&encoding_tree, encoding_free);
    free(lasten);

    while (transs != NULL) {
        struct transs_s *old = transs;
        transs = transs->next;
        free(old);
    }
}
