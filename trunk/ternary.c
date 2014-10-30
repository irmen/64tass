/* ternary.c - Ternary Search Trees
   $Id$

   Copyright (C) 2001 Free Software Foundation, Inc.

   Contributed by Daniel Berlin (dan@cgsoftware.com)

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/
#include "ternary.h"
#include "unicode.h"
#include "error.h"

static union tern_u {
    ternary_node tern;
    union tern_u *next;
} *terns_free = NULL;

static struct terns_s {
    union tern_u terns[255];
    struct terns_s *next;
} *terns = NULL;

static void tern_free(union tern_u *tern) {
#ifdef DEBUG
    free(tern);
#else
    tern->next = terns_free;
    terns_free = tern;
#endif
}

static ternary_node *tern_alloc(void) {
#ifdef DEBUG
    return malloc(sizeof(ternary_node));
#else
    ternary_node *tern;
    size_t i;
    tern = (ternary_node *)terns_free;
    terns_free = terns_free->next;
    if (!terns_free) {
        struct terns_s *old = terns;
        terns = (struct terns_s *)malloc(sizeof(struct terns_s));
        if (!terns) err_msg_out_of_memory();
        for (i = 0; i < 254; i++) {
            terns->terns[i].next = &terns->terns[i+1];
        }
        terns->terns[i].next = NULL;
        terns->next = old;
        terns_free = &terns->terns[0];
    }
    return tern;
#endif
}

/* Non-recursive so we don't waste stack space/time on large
   insertions. */

void *ternary_insert(ternary_tree *root, const uint8_t *s, const uint8_t *end, void *data, int replace)
{
    uint32_t spchar;
    ternary_tree curr, *pcurr;
    if (s == end) return NULL;
    spchar = *s;
    if (spchar & 0x80) s += utf8in(s, &spchar); else s++;

    /* Start at the root. */
    pcurr = root;
    /* Loop until we find the right position */
    while ((curr = *pcurr))
    {
        /* Handle current char equal to node splitchar */
        if (spchar == curr->splitchar) {
            /* Handle the case of a string we already have */
            if (!~spchar)
            {
                if (replace)
                    curr->eqkid = (ternary_tree) data;
                return (void *) curr->eqkid;
            }
            if (s == end) spchar = ~0;
            else {
                spchar = *s;
                if (spchar & 0x80) s += utf8in(s, &spchar); else s++;
            }
            pcurr = &(curr->eqkid);
        } else pcurr = (spchar < curr->splitchar) ? &(curr->lokid) : &(curr->hikid);
    }
    /* It's not a duplicate string, and we should insert what's left of
       the string, into the tree rooted at curr */
    for (;;) {
        /* Allocate the memory for the node, and fill it in */
        *pcurr = tern_alloc();
        if (!pcurr) return NULL;
        curr = *pcurr;
        curr->splitchar = spchar;
        curr->lokid = curr->hikid = curr->eqkid = NULL;

        /* Place nodes until we hit the end of the string.
           When we hit it, place the data in the right place, and
           return.
           */
        if (!~spchar) {
            curr->eqkid = (ternary_tree) data;
            return data;
        }
        if (s == end) spchar = ~0;
        else {
            spchar = *s;
            if (spchar & 0x80) s += utf8in(s, &spchar); else s++;
        }
        pcurr = &(curr->eqkid);
    }
}

/* Free the ternary search tree rooted at p. */
void ternary_cleanup(ternary_tree p, ternary_free_fn_t f)
{
    if (p) {
        ternary_cleanup(p->lokid, f);
        if (~p->splitchar) {
            ternary_cleanup(p->eqkid, f);
        } else f(p->eqkid);
        ternary_cleanup(p->hikid, f);
        tern_free((union tern_u *)p);
    }
}

/* Non-recursive find of a string in the ternary tree */
void *ternary_search (const ternary_node *p, const uint8_t *s, const uint8_t *end)
{
    const ternary_node *curr;
    uint32_t spchar;
    const ternary_node *last = NULL;
    if (s == end) return NULL;
    spchar = *s;
    if (spchar & 0x80) s += utf8in(s, &spchar); else s++;
    curr = p;
    while (curr) {
        if (spchar == curr->splitchar) {
            if (!~spchar) return (void *)curr->eqkid;
            if (s == end) spchar = ~0;
            else {
                spchar = *s;
                if (spchar & 0x80) s += utf8in(s, &spchar); else s++;
            }
            curr = curr->eqkid;
            last = curr;
        } else curr = (spchar < curr->splitchar) ? curr->lokid : curr->hikid;
    }
    while (last && ~last->splitchar) {
        last = last->hikid;
    }
    return last ? (void *)last->eqkid : NULL;
}

void init_ternary(void)
{
    size_t i;
    terns = (struct terns_s *)malloc(sizeof(struct terns_s));
    if (!terns) err_msg_out_of_memory();
    for (i = 0; i < 254; i++) {
        terns->terns[i].next = &terns->terns[i+1];
    }
    terns->terns[i].next = NULL;
    terns->next = NULL;

    terns_free = &terns->terns[0];
}

void destroy_ternary(void)
{
    struct terns_s *old;

    while (terns) {
        old = terns;
        terns = terns->next;
        free(old);
    }
}
