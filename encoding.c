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
#include "encoding.h"
#include "error.h"
#include "string.h"
#include "ternary.h"
#include "misc.h"
#include "values.h"
#include "64tass.h"
#include "unicode.h"

struct encoding_s *actual_encoding;

struct encoding_s {
    str_t name;
    ternary_tree escape;
    struct avltree trans;
    struct avltree_node node;
};

struct trans2_s {
    uint16_t start;
    uint16_t end;
    uint8_t offset;
};

struct escape_s {
    size_t strlen;
    size_t len;
    uint8_t val[4];
    uint8_t *data;
};

static struct avltree encoding_tree;

static struct trans2_s no_trans[] = {
    {0x00, 0xff, 0x00},
};

static struct trans2_s petscii_trans[] = {
    {0x20, 0x40, 0x20}, // -@
    {0x41, 0x5a, 0xc1}, //A-Z
    {0x5b, 0x5b, 0x5b}, //[
    {0x5d, 0x5d, 0x5d}, //]
    {0x61, 0x7a, 0x41}, //a-z
    {0xa3, 0xa3, 0x5c}, // £
    {0x03c0, 0x03c0, 0xff}, // π
    {0x2190, 0x2190, 0x5f}, // ←
    {0x2191, 0x2191, 0x5e}, // ↑
    {0x2500, 0x2500, 0xc0}, // ─
    {0x2502, 0x2502, 0xdd}, // │
    {0x250c, 0x250c, 0xb0}, // ┌
    {0x2510, 0x2510, 0xae}, // ┐
    {0x2514, 0x2514, 0xad}, // └
    {0x2518, 0x2518, 0xbd}, // ┘
    {0x251c, 0x251c, 0xab}, // ├
    {0x2524, 0x2524, 0xb3}, // ┤
    {0x252c, 0x252c, 0xb2}, // ┬
    {0x2534, 0x2534, 0xb1}, // ┴
    {0x253c, 0x253c, 0xdb}, // ┼
    {0x256d, 0x256d, 0xd5}, // ╭
    {0x256e, 0x256e, 0xc9}, // ╮
    {0x256f, 0x256f, 0xcb}, // ╯
    {0x2570, 0x2570, 0xca}, // ╰
    {0x2571, 0x2571, 0xce}, // ╱
    {0x2572, 0x2572, 0xcd}, // ╲
    {0x2573, 0x2573, 0xd6}, // ╳
    {0x2581, 0x2581, 0xa4}, // ▁
    {0x2582, 0x2582, 0xaf}, // ▂
    {0x2583, 0x2583, 0xb9}, // ▃
    {0x2584, 0x2584, 0xa2}, // ▄
    {0x258c, 0x258c, 0xa1}, // ▌
    {0x258d, 0x258d, 0xb5}, // ▍
    {0x258e, 0x258e, 0xb4}, // ▎
    {0x258f, 0x258f, 0xa5}, // ▏
    {0x2592, 0x2592, 0xa6}, // ▒
    {0x2594, 0x2594, 0xa3}, // ▔
    {0x2595, 0x2595, 0xa7}, // ▕
    {0x2596, 0x2596, 0xbb}, // ▖
    {0x2597, 0x2597, 0xac}, // ▗
    {0x2598, 0x2598, 0xbe}, // ▘
    {0x259a, 0x259a, 0xbf}, // ▚
    {0x259d, 0x259d, 0xbc}, // ▝
    {0x25cb, 0x25cb, 0xd7}, // ○
    {0x25cf, 0x25cf, 0xd1}, // ●
    {0x25e4, 0x25e4, 0xa9}, // ◤
    {0x25e5, 0x25e5, 0xdf}, // ◥
    {0x2660, 0x2660, 0xc1}, // ♠
    {0x2663, 0x2663, 0xd8}, // ♣
    {0x2665, 0x2665, 0xd3}, // ♥
    {0x2666, 0x2666, 0xda}, // ♦
    {0x2713, 0x2713, 0xba}, // ✓
};

/* PETSCII codes, must be sorted */
static const char *petscii_esc[] = {
    "\x07" "{bell}",
    "\x90" "{black}",
    "\x90" "{blk}",
    "\x1f" "{blue}",
    "\x1f" "{blu}",
    "\x95" "{brn}",
    "\x95" "{brown}",
    "\xdf" "{cbm-*}",
    "\xa6" "{cbm-+}",
    "\xdc" "{cbm--}",
    "\x30" "{cbm-0}",
    "\x81" "{cbm-1}",
    "\x95" "{cbm-2}",
    "\x96" "{cbm-3}",
    "\x97" "{cbm-4}",
    "\x98" "{cbm-5}",
    "\x99" "{cbm-6}",
    "\x9a" "{cbm-7}",
    "\x9b" "{cbm-8}",
    "\x29" "{cbm-9}",
    "\xa4" "{cbm-@}",
    "\xde" "{cbm-^}",
    "\xb0" "{cbm-a}",
    "\xbf" "{cbm-b}",
    "\xbc" "{cbm-c}",
    "\xac" "{cbm-d}",
    "\xb1" "{cbm-e}",
    "\xbb" "{cbm-f}",
    "\xa5" "{cbm-g}",
    "\xb4" "{cbm-h}",
    "\xa2" "{cbm-i}",
    "\xb5" "{cbm-j}",
    "\xa1" "{cbm-k}",
    "\xb6" "{cbm-l}",
    "\xa7" "{cbm-m}",
    "\xaa" "{cbm-n}",
    "\xb9" "{cbm-o}",
    "\xa8" "{cbm-pound}",
    "\xaf" "{cbm-p}",
    "\xab" "{cbm-q}",
    "\xb2" "{cbm-r}",
    "\xae" "{cbm-s}",
    "\xa3" "{cbm-t}",
    "\xde" "{cbm-up arrow}",
    "\xb8" "{cbm-u}",
    "\xbe" "{cbm-v}",
    "\xb3" "{cbm-w}",
    "\xbd" "{cbm-x}",
    "\xb7" "{cbm-y}",
    "\xad" "{cbm-z}",
    "\x93" "{clear}",
    "\x93" "{clr}",
    "\x92" "{control-0}",
    "\x90" "{control-1}",
    "\x05" "{control-2}",
    "\x1c" "{control-3}",
    "\x9f" "{control-4}",
    "\x9c" "{control-5}",
    "\x1e" "{control-6}",
    "\x1f" "{control-7}",
    "\x9e" "{control-8}",
    "\x12" "{control-9}",
    "\x1b" "{control-:}",
    "\x1d" "{control-;}",
    "\x1f" "{control-=}",
    "\x00" "{control-@}",
    "\x01" "{control-a}",
    "\x02" "{control-b}",
    "\x03" "{control-c}",
    "\x04" "{control-d}",
    "\x05" "{control-e}",
    "\x06" "{control-f}",
    "\x07" "{control-g}",
    "\x08" "{control-h}",
    "\x09" "{control-i}",
    "\x0a" "{control-j}",
    "\x0b" "{control-k}",
    "\x06" "{control-left arrow}",
    "\x0c" "{control-l}",
    "\x0d" "{control-m}",
    "\x0e" "{control-n}",
    "\x0f" "{control-o}",
    "\x1c" "{control-pound}",
    "\x10" "{control-p}",
    "\x11" "{control-q}",
    "\x12" "{control-r}",
    "\x13" "{control-s}",
    "\x14" "{control-t}",
    "\x1e" "{control-up arrow}",
    "\x15" "{control-u}",
    "\x16" "{control-v}",
    "\x17" "{control-w}",
    "\x18" "{control-x}",
    "\x19" "{control-y}",
    "\x1a" "{control-z}",
    "\x0d" "{cr}",
    "\x9f" "{cyan}",
    "\x9f" "{cyn}",
    "\x14" "{delete}",
    "\x14" "{del}",
    "\x08" "{dish}",
    "\x11" "{down}",
    "\x09" "{ensh}",
    "\x1b" "{esc}",
    "\x82" "{f10}",
    "\x84" "{f11}",
    "\x8f" "{f12}",
    "\x85" "{f1}",
    "\x89" "{f2}",
    "\x86" "{f3}",
    "\x8a" "{f4}",
    "\x87" "{f5}",
    "\x8b" "{f6}",
    "\x88" "{f7}",
    "\x8c" "{f8}",
    "\x80" "{f9}",
    "\x97" "{gray1}",
    "\x98" "{gray2}",
    "\x9b" "{gray3}",
    "\x1e" "{green}",
    "\x97" "{grey1}",
    "\x98" "{grey2}",
    "\x9b" "{grey3}",
    "\x1e" "{grn}",
    "\x97" "{gry1}",
    "\x98" "{gry2}",
    "\x9b" "{gry3}",
    "\x84" "{help}",
    "\x13" "{home}",
    "\x94" "{insert}",
    "\x94" "{inst}",
    "\x9a" "{lblu}",
    "\x5f" "{left arrow}",
    "\x9d" "{left}",
    "\x0a" "{lf}",
    "\x99" "{lgrn}",
    "\x0e" "{lower case}",
    "\x96" "{lred}",
    "\x9a" "{lt blue}",
    "\x99" "{lt green}",
    "\x96" "{lt red}",
    "\x81" "{orange}",
    "\x81" "{orng}",
    "\xff" "{pi}",
    "\x5c" "{pound}",
    "\x9c" "{purple}",
    "\x9c" "{pur}",
    "\x1c" "{red}",
    "\x0d" "{return}",
    "\x92" "{reverse off}",
    "\x12" "{reverse on}",
    "\x1d" "{rght}",
    "\x1d" "{right}",
    "\x83" "{run}",
    "\x92" "{rvof}",
    "\x12" "{rvon}",
    "\x92" "{rvs off}",
    "\x12" "{rvs on}",
    "\x8d" "{shift return}",
    "\xc0" "{shift-*}",
    "\xdb" "{shift-+}",
    "\x3c" "{shift-,}",
    "\xdd" "{shift--}",
    "\x3e" "{shift-.}",
    "\x3f" "{shift-/}",
    "\x30" "{shift-0}",
    "\x21" "{shift-1}",
    "\x22" "{shift-2}",
    "\x23" "{shift-3}",
    "\x24" "{shift-4}",
    "\x25" "{shift-5}",
    "\x26" "{shift-6}",
    "\x27" "{shift-7}",
    "\x28" "{shift-8}",
    "\x29" "{shift-9}",
    "\x5b" "{shift-:}",
    "\x5d" "{shift-;}",
    "\xba" "{shift-@}",
    "\xde" "{shift-^}",
    "\xc1" "{shift-a}",
    "\xc2" "{shift-b}",
    "\xc3" "{shift-c}",
    "\xc4" "{shift-d}",
    "\xc5" "{shift-e}",
    "\xc6" "{shift-f}",
    "\xc7" "{shift-g}",
    "\xc8" "{shift-h}",
    "\xc9" "{shift-i}",
    "\xca" "{shift-j}",
    "\xcb" "{shift-k}",
    "\xcc" "{shift-l}",
    "\xcd" "{shift-m}",
    "\xce" "{shift-n}",
    "\xcf" "{shift-o}",
    "\xa9" "{shift-pound}",
    "\xd0" "{shift-p}",
    "\xd1" "{shift-q}",
    "\xd2" "{shift-r}",
    "\xa0" "{shift-space}",
    "\xd3" "{shift-s}",
    "\xd4" "{shift-t}",
    "\xde" "{shift-up arrow}",
    "\xd5" "{shift-u}",
    "\xd6" "{shift-v}",
    "\xd7" "{shift-w}",
    "\xd8" "{shift-x}",
    "\xd9" "{shift-y}",
    "\xda" "{shift-z}",
    "\x20" "{space}",
    "\x8d" "{sret}",
    "\x03" "{stop}",
    "\x0e" "{swlc}",
    "\x8e" "{swuc}",
    "\x09" "{tab}",
    "\x5e" "{up arrow}",
    "\x09" "{up/lo lock off}",
    "\x08" "{up/lo lock on}",
    "\x8e" "{upper case}",
    "\x91" "{up}",
    "\x05" "{white}",
    "\x05" "{wht}",
    "\x9e" "{yellow}",
    "\x9e" "{yel}",
    NULL
};

static struct trans2_s petscii_screen_trans[] = {
    {0x20, 0x3f, 0x20}, // -?
    {0x40, 0x40, 0x00}, //@
    {0x41, 0x5a, 0x41}, //A-Z
    {0x5b, 0x5b, 0x1b}, //[
    {0x5d, 0x5d, 0x1d}, //]
    {0x61, 0x7a, 0x01}, //a-z
    {0xa3, 0xa3, 0x1c}, // £
    {0x03c0, 0x03c0, 0x5e}, // π
    {0x2190, 0x2190, 0x1f}, // ←
    {0x2191, 0x2191, 0x1e}, // ↑
    {0x2500, 0x2500, 0x40}, // ─
    {0x2502, 0x2502, 0x5d}, // │
    {0x250c, 0x250c, 0x70}, // ┌
    {0x2510, 0x2510, 0x6e}, // ┐
    {0x2514, 0x2514, 0x6d}, // └
    {0x2518, 0x2518, 0x7d}, // ┘
    {0x251c, 0x251c, 0x6b}, // ├
    {0x2524, 0x2524, 0x73}, // ┤
    {0x252c, 0x252c, 0x72}, // ┬
    {0x2534, 0x2534, 0x71}, // ┴
    {0x253c, 0x253c, 0x5b}, // ┼
    {0x256d, 0x256d, 0x55}, // ╭
    {0x256e, 0x256e, 0x49}, // ╮
    {0x256f, 0x256f, 0x4b}, // ╯
    {0x2570, 0x2570, 0x4a}, // ╰
    {0x2571, 0x2571, 0x4e}, // ╱
    {0x2572, 0x2572, 0x4d}, // ╲
    {0x2573, 0x2573, 0x56}, // ╳
    {0x2581, 0x2581, 0x64}, // ▁
    {0x2582, 0x2582, 0x6f}, // ▂
    {0x2583, 0x2583, 0x79}, // ▃
    {0x2584, 0x2584, 0x62}, // ▄
    {0x258c, 0x258c, 0x61}, // ▌
    {0x258d, 0x258d, 0x75}, // ▍
    {0x258e, 0x258e, 0x74}, // ▎
    {0x258f, 0x258f, 0x65}, // ▏
    {0x2592, 0x2592, 0x66}, // ▒
    {0x2594, 0x2594, 0x63}, // ▔
    {0x2595, 0x2595, 0x67}, // ▕
    {0x2596, 0x2596, 0x7b}, // ▖
    {0x2597, 0x2597, 0x6c}, // ▗
    {0x2598, 0x2598, 0x7e}, // ▘
    {0x259a, 0x259a, 0x7f}, // ▚
    {0x259d, 0x259d, 0x7c}, // ▝
    {0x25cb, 0x25cb, 0x57}, // ○
    {0x25cf, 0x25cf, 0x51}, // ●
    {0x25e4, 0x25e4, 0x69}, // ◤
    {0x25e5, 0x25e5, 0x5f}, // ◥
    {0x2660, 0x2660, 0x41}, // ♠
    {0x2663, 0x2663, 0x58}, // ♣
    {0x2665, 0x2665, 0x53}, // ♥
    {0x2666, 0x2666, 0x5a}, // ♦
    {0x2713, 0x2713, 0x7a}, // ✓
};

/* petscii screen codes, must be sorted */
static const char *petscii_screen_esc[] = {
    "\x5f" "{cbm-*}",
    "\x66" "{cbm-+}",
    "\x5c" "{cbm--}",
    "\x30" "{cbm-0}",
    "\x29" "{cbm-9}",
    "\x64" "{cbm-@}",
    "\x5e" "{cbm-^}",
    "\x70" "{cbm-a}",
    "\x7f" "{cbm-b}",
    "\x7c" "{cbm-c}",
    "\x6c" "{cbm-d}",
    "\x71" "{cbm-e}",
    "\x7b" "{cbm-f}",
    "\x65" "{cbm-g}",
    "\x74" "{cbm-h}",
    "\x62" "{cbm-i}",
    "\x75" "{cbm-j}",
    "\x61" "{cbm-k}",
    "\x76" "{cbm-l}",
    "\x67" "{cbm-m}",
    "\x6a" "{cbm-n}",
    "\x79" "{cbm-o}",
    "\x68" "{cbm-pound}",
    "\x6f" "{cbm-p}",
    "\x6b" "{cbm-q}",
    "\x72" "{cbm-r}",
    "\x6e" "{cbm-s}",
    "\x63" "{cbm-t}",
    "\x5e" "{cbm-up arrow}",
    "\x78" "{cbm-u}",
    "\x7e" "{cbm-v}",
    "\x73" "{cbm-w}",
    "\x7d" "{cbm-x}",
    "\x77" "{cbm-y}",
    "\x6d" "{cbm-z}",
    "\x1f" "{left arrow}",
    "\x5e" "{pi}",
    "\x1c" "{pound}",
    "\x40" "{shift-*}",
    "\x5b" "{shift-+}",
    "\x3c" "{shift-,}",
    "\x5d" "{shift--}",
    "\x3e" "{shift-.}",
    "\x3f" "{shift-/}",
    "\x30" "{shift-0}",
    "\x21" "{shift-1}",
    "\x22" "{shift-2}",
    "\x23" "{shift-3}",
    "\x24" "{shift-4}",
    "\x25" "{shift-5}",
    "\x26" "{shift-6}",
    "\x27" "{shift-7}",
    "\x28" "{shift-8}",
    "\x29" "{shift-9}",
    "\x1b" "{shift-:}",
    "\x1d" "{shift-;}",
    "\x7a" "{shift-@}",
    "\x5e" "{shift-^}",
    "\x41" "{shift-a}",
    "\x42" "{shift-b}",
    "\x43" "{shift-c}",
    "\x44" "{shift-d}",
    "\x45" "{shift-e}",
    "\x46" "{shift-f}",
    "\x47" "{shift-g}",
    "\x48" "{shift-h}",
    "\x49" "{shift-i}",
    "\x4a" "{shift-j}",
    "\x4b" "{shift-k}",
    "\x4c" "{shift-l}",
    "\x4d" "{shift-m}",
    "\x4e" "{shift-n}",
    "\x4f" "{shift-o}",
    "\x69" "{shift-pound}",
    "\x50" "{shift-p}",
    "\x51" "{shift-q}",
    "\x52" "{shift-r}",
    "\x60" "{shift-space}",
    "\x53" "{shift-s}",
    "\x54" "{shift-t}",
    "\x5e" "{shift-up arrow}",
    "\x55" "{shift-u}",
    "\x56" "{shift-v}",
    "\x57" "{shift-w}",
    "\x58" "{shift-x}",
    "\x59" "{shift-y}",
    "\x5a" "{shift-z}",
    "\x20" "{space}",
    "\x1e" "{up arrow}",
    NULL
};

static struct trans2_s no_screen_trans[] = {
    {0x00, 0x1F, 0x80},
    {0x20, 0x3F, 0x20},
    {0x40, 0x5F, 0x00},
    {0x60, 0x7F, 0x40},
    {0x80, 0x9F, 0x80},
    {0xA0, 0xBF, 0x60},
    {0xC0, 0xFE, 0x40},
    {0xFF, 0xFF, 0x5E},
};

static int trans_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct trans_s *a = cavltree_container_of(aa, struct trans_s, node);
    const struct trans_s *b = cavltree_container_of(bb, struct trans_s, node);

    if (a->start > b->end) {
        return -1;
    }
    if (a->end < b->start) {
        return 1;
    }
    return 0;
}

static void trans_free(struct avltree_node *aa)
{
    struct trans_s *a = avltree_container_of(aa, struct trans_s, node);
    free(a);
}

static int encoding_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct encoding_s *a = cavltree_container_of(aa, struct encoding_s, node);
    const struct encoding_s *b = cavltree_container_of(bb, struct encoding_s, node);

    return arguments.casesensitive ? str_cmp(&a->name, &b->name) : str_casecmp(&a->name, &b->name);
}

static void escape_free(void *e) {
    struct escape_s *esc = (struct escape_s *)e;
    if (esc->data != esc->val) free(esc->data);
    free(esc);
}

static void encoding_free(struct avltree_node *aa)
{
    struct encoding_s *a = avltree_container_of(aa, struct encoding_s, node);

    free((char *)a->name.data);
    ternary_cleanup(a->escape, escape_free);
    avltree_destroy(&a->trans, trans_free);
    free(a);
}

static struct encoding_s *lasten = NULL;
struct encoding_s *new_encoding(const str_t *name)
{
    struct avltree_node *b;
    struct encoding_s *tmp;

    if (!lasten) {
        lasten = (struct encoding_s *)malloc(sizeof(struct encoding_s));
        if (!lasten) err_msg_out_of_memory();
    }
    lasten->name = *name;
    b = avltree_insert(&lasten->node, &encoding_tree, encoding_compare);
    if (!b) { //new encoding
        str_cpy(&lasten->name, name);
        lasten->escape=NULL;
        avltree_init(&lasten->trans);
        tmp = lasten;
        lasten = NULL;
        return tmp;
    }
    return avltree_container_of(b, struct encoding_s, node);            //already exists
}

static struct trans_s *lasttr = NULL;
struct trans_s *new_trans(struct trans_s *trans, struct encoding_s *enc)
{
    struct avltree_node *b;
    struct trans_s *tmp;
    if (!lasttr) {
        lasttr = (struct trans_s *)malloc(sizeof(struct trans_s));
        if (!lasttr) err_msg_out_of_memory();
    }
    lasttr->start = trans->start;
    lasttr->end = trans->end;
    lasttr->offset = trans->offset;
    b = avltree_insert(&lasttr->node, &enc->trans, trans_compare);
    if (!b) { //new encoding
        tmp = lasttr;
        lasttr = NULL;
        return tmp;
    }
    return avltree_container_of(b, struct trans_s, node);            //already exists
}

static struct escape_s *lastes = NULL;
int new_escape(const struct value_s *v, struct value_s *val, struct encoding_s *enc, linepos_t epoint)
{
    struct escape_s *b, tmp;
    struct value_s iter, item, *val2, err;
    uval_t uval;
    size_t i, len;
    uint8_t *odata = NULL, *d;
    int foundold;

    if (!lastes) {
        lastes = (struct escape_s *)malloc(sizeof(struct escape_s));
        if (!lastes) err_msg_out_of_memory();
    }
    b = (struct escape_s *)ternary_insert(&enc->escape, v->u.str.data, v->u.str.data + v->u.str.len, lastes, 0);
    if (!b) err_msg_out_of_memory();
    foundold = (b != lastes);
    if (foundold) {
        odata = b->data;
        b->data = NULL; /* lock old one */
    }

    i = 0;
    lastes->data = NULL; /* lock new one */
    len = sizeof(tmp.val);
    d = tmp.val;

    item.refcount = 0;
    if (val->obj == STR_OBJ) {
        struct value_s *tmp2 = val_alloc();
        bytes_from_str(tmp2, val);
        tmp2->obj->getiter(tmp2, &iter);
        val_destroy(tmp2);
    } else val->obj->getiter(val, &iter);

    while ((val2 = iter.obj->next(&iter, &item))) {
        switch (val2->obj->type) {
        default:
            if (val2->obj->uval(val2, &err, &uval, 8, epoint)) uval = 0;
            break;
        case T_NONE:
            err_msg_still_none(NULL, epoint);
            uval = 0;
        }
        if (i >= len) {
            if (i == sizeof(tmp.val)) {
                len = 16;
                d = (uint8_t *)malloc(len);
                memcpy(d, tmp.val, i);
            } else {
                len += 1024;
                if (len < 1024) err_msg_out_of_memory(); /* overflow */
                d = (uint8_t *)realloc(d, len);
            }
            if (!d) err_msg_out_of_memory();
        }
        d[i++] = (uint8_t)uval;
        val_destroy(val2);
    }
    iter.obj->destroy(&iter);

    if (!foundold) { //new escape
        if (d == tmp.val) {
            memcpy(lastes->val, tmp.val, i);
            d = lastes->val;
        } else if (i < len) {
            d = (uint8_t *)realloc(d, i);
            if (!d) err_msg_out_of_memory();
        }
        lastes->strlen = v->u.str.len;
        lastes->len = i;
        lastes->data = d; /* unlock new */
        lastes = NULL;
        return 0;
    }
    b->data = odata; /* unlock old */
    i = (i != b->len || !!memcmp(d, b->data, i));
    if (tmp.val != d) free(d);
    return i;            //already exists
}

static void add_esc(const char **s, struct encoding_s *enc) {
    struct value_s tmp, tmp2;
    struct linepos_s nopoint = {0,0,0};
    tmp.refcount = 0;
    tmp.obj = STR_OBJ;
    tmp2.refcount = 0;
    tmp2.obj = BYTES_OBJ;
    tmp2.u.bytes.len = 1;
    tmp2.u.bytes.data = tmp2.u.bytes.val;
    while (*s) {
        tmp.u.str.data = (uint8_t *)*s + 1;
        tmp.u.str.len = strlen((char *)tmp.u.str.data);
        tmp.u.str.chars = tmp.u.str.len;
        tmp2.u.bytes.val[0] = *s[0];
        new_escape(&tmp, &tmp2, enc, &nopoint);
        s++;
    }
}

static void add_trans(struct trans2_s *t, int max, struct encoding_s *tmp) {
    size_t i;
    struct trans_s tmp2;
    for (i = 0; i < max / sizeof(struct trans2_s); i++) {
        tmp2.start = t[i].start;
        tmp2.end = t[i].end;
        tmp2.offset = t[i].offset;
        new_trans(&tmp2, tmp);
    }
}

int encode_string(const struct value_s *v) {
    uint32_t ch;
    const struct escape_s *e;
    const struct avltree_node *c;
    const struct trans_s *t;
    struct trans_s tmp;
    static size_t i, j, len, len2;
    static const uint8_t *data, *data2;

    if (v) {
        i = 0;
        len2 = 0;
        len = v->u.str.len;
        data = v->u.str.data;
        return 0;
    }
    if (j < len2) {
        return data2[j++];
    }
next:
    if (i >= len) return EOF;
    e = (struct escape_s *)ternary_search(actual_encoding->escape, data + i, data + len);
    if (e && e->data) {
        i += e->strlen;
        data2 = e->data;
        len2 = e->len;
        if (len2 < 1) goto next;
        j = 1;
        return e->data[0];
    }
    ch = data[i];
    if (ch & 0x80) i += utf8in(data + i, &ch); else i++;
    tmp.start = tmp.end = ch;

    c = avltree_lookup(&tmp.node, &actual_encoding->trans, trans_compare);
    if (c) {
        t = cavltree_container_of(c, struct trans_s, node);
        if (tmp.start >= t->start && tmp.end <= t->end) {
            return (uint8_t)(ch - t->start + t->offset);
        }
    }
    err_msg(ERROR___UNKNOWN_CHR, &ch);
    return EOF;
}

void init_encoding(int toascii)
{
    struct encoding_s *tmp;
    avltree_init(&encoding_tree);
    static const str_t none_enc = {4, (const uint8_t *)"none"};
    static const str_t screen_enc = {6, (const uint8_t *)"screen"};

    if (!toascii) {
        tmp = new_encoding(&none_enc);
        if (!tmp) {
            return;
        }
        add_trans(no_trans, sizeof(no_trans), tmp);

        tmp = new_encoding(&screen_enc);
        if (!tmp) {
            return;
        }
        add_trans(no_screen_trans, sizeof(no_screen_trans), tmp);
    } else {
        tmp = new_encoding(&none_enc);
        if (!tmp) {
            return;
        }
        add_esc(petscii_esc, tmp);
        add_trans(petscii_trans, sizeof(petscii_trans), tmp);

        tmp = new_encoding(&screen_enc);
        if (!tmp) {
            return;
        }
        add_esc(petscii_screen_esc, tmp);
        add_trans(petscii_screen_trans, sizeof(petscii_screen_trans), tmp);
    }
}

void destroy_encoding(void)
{
    avltree_destroy(&encoding_tree, encoding_free);
    free(lasten);
    free(lasttr);
    free(lastes);
}
