/*
    $Id: encobj.h 3066 2023-08-27 20:52:04Z soci $

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
#ifndef ENCOBJ_H
#define ENCOBJ_H
#include "obj.h"
#include "stdbool.h"
#include "avl.h"
#include "errors_e.h"

extern struct Enc *actual_encoding;
extern size_t efwcount;

extern struct Type *const ENC_OBJ;

struct encoder_s;
struct str_t;
struct Str;

struct transmap_s {
    uint8_t value;
    uint8_t pass;
};

struct character_range_s {
    uint32_t start;
    uint32_t end : 24;
    uint32_t offset : 8;
};

typedef struct Enc {
    Obj v;
    uint16_t escape_char;
    uint8_t epass;
    bool updating;
    struct ternary_node_def *escapes;
    size_t escape_length;
    struct avltree ranges;
    struct transmap_s *map;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
} Enc;

#define Enc(a) OBJ_CAST(Enc, a)

extern void encobj_init(void);
extern void encobj_destroy(void);
extern void destroy_transs(void);

static inline Enc *ref_enc(Enc *v1) {
    v1->v.refcount++; return v1;
}

extern MALLOC Obj *new_enc(const struct file_list_s *, linepos_t);
extern bool enc_trans_add(Enc *, const struct character_range_s *, linepos_t);
extern bool enc_escape_add(Enc *, const struct str_t *, Obj *, linepos_t);
extern struct encoder_s *enc_string_init(Enc *, const struct Str *, linepos_t);
extern void enc_error(struct encoder_s *, Error_types);
extern int enc_string(struct encoder_s *encoder);

#endif
