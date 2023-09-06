/* ternary.c - Ternary Search Trees
   $Id: ternary.c 3086 2023-09-03 06:23:08Z soci $

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

#ifdef DEBUG
#define tern_free(tern) free(tern)
static MALLOC ternary_node *tern_alloc(void) {
    ternary_node *r;
    new_instance(&r);
    return r;
}
#else
static union tern_u {
    ternary_node tern;
    union tern_u *next;
} *terns_free;

static struct terns_s {
    union tern_u terns[255];
    struct terns_s *next;
} *terns;

static void tern_free(union tern_u *tern) {
    tern->next = terns_free;
    terns_free = tern;
}

static union tern_u *terns_alloc(void) {
    size_t i;
    struct terns_s *old = terns;
    new_instance(&terns);
    for (i = 0; i < 254; i++) {
        terns->terns[i].next = &terns->terns[i + 1];
    }
    terns->terns[i].next = NULL;
    terns->next = old;
    return &terns->terns[0];
}

static MALLOC ternary_node *tern_alloc(void) {
    ternary_node *tern;
    if (terns_free == NULL) terns_free = terns_alloc();
    tern = (ternary_node *)terns_free;
    terns_free = terns_free->next;
    return tern;
}
#endif

/* Non-recursive so we don't waste stack space/time on large
   insertions. */

void *ternary_insert(ternary_tree *pcurr, const uint8_t *s, const uint8_t *end)
{
    unichar_t spchar;
    ternary_tree curr;

    spchar = *s;
    if ((spchar & 0x80) != 0) s += utf8in(s, &spchar); else s++;

    /* Loop until we find the right position */
    while ((curr = *pcurr) != NULL)
    {
        /* Handle current char equal to node splitchar */
        if (spchar == curr->splitchar) {
            /* Handle the case of a string we already have */
            if ((~spchar) == 0)
            {
                return &curr->eqkid;
            }
            if (s == end) spchar = ~(unichar_t)0;
            else {
                spchar = *s;
                if ((spchar & 0x80) != 0) s += utf8in(s, &spchar); else s++;
            }
            pcurr = &(curr->eqkid);
        } else pcurr = (spchar < curr->splitchar) ? &(curr->lokid) : &(curr->hikid);
    }
    /* It's not a duplicate string, and we should insert what's left of
       the string, into the tree rooted at curr */
    for (;;) {
        /* Allocate the memory for the node, and fill it in */
        curr = tern_alloc();
        curr->splitchar = spchar;
        curr->lokid = curr->hikid = curr->eqkid = NULL;
        *pcurr = curr;
        pcurr = &curr->eqkid;

        /* Place nodes until we hit the end of the string.
           When we hit it, place the data in the right place, and
           return.
           */
        if ((~spchar) == 0) {
            return pcurr;
        }
        if (s == end) spchar = ~(unichar_t)0;
        else {
            spchar = *s;
            if ((spchar & 0x80) != 0) s += utf8in(s, &spchar); else s++;
        }
    }
}

/* Free the ternary search tree rooted at p. */
void ternary_cleanup(ternary_tree p, ternary_free_fn_t f)
{
    if (p->lokid != NULL) ternary_cleanup(p->lokid, f);
    if ((~p->splitchar) != 0) {
        ternary_cleanup(p->eqkid, f);
    } else f(p->eqkid);
    if (p->hikid != NULL) ternary_cleanup(p->hikid, f);
    tern_free((union tern_u *)p);
}

/* Non-recursive find of a string in the ternary tree */
void *ternary_search(const ternary_node *curr, const uint8_t *s, size_t *len)
{
    unichar_t spchar;
    const ternary_node *last = NULL;
    const uint8_t *start = s, *end = s + *len, *prev = s;

    spchar = *s;
    if ((spchar & 0x80) != 0) s += utf8in(s, &spchar); else s++;

    do {
        if (spchar == curr->splitchar) {
            if ((~spchar) == 0) return (void *)curr->eqkid;
            if (s == end) spchar = ~(unichar_t)0;
            else {
                prev = s;
                spchar = *s;
                if ((spchar & 0x80) != 0) s += utf8in(s, &spchar); else s++;
            }
            curr = curr->eqkid;
            last = curr;
        } else curr = (spchar < curr->splitchar) ? curr->lokid : curr->hikid;
    } while (curr != NULL);
    while (last != NULL && (~last->splitchar) != 0) {
        last = last->hikid;
    }
    *len = (size_t)(prev - start);
    return (last != NULL) ? (void *)last->eqkid : NULL;
}

void init_ternary(void) {
#ifndef DEBUG
    terns_free = NULL;
    terns = NULL;
#endif
}

void destroy_ternary(void)
{
#ifndef DEBUG
    while (terns != NULL) {
        struct terns_s *old = terns;
        terns = terns->next;
        free(old);
    }
#endif
}
