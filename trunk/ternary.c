/* ternary.c - Ternary Search Trees
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

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */
#include "ternary.h"
#include <stdlib.h>
#include "misc.h"

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
        *pcurr = (ternary_tree) malloc (sizeof (ternary_node));
        if (!pcurr) return NULL;
        curr = *pcurr;
        curr->splitchar = spchar;
        curr->lokid = curr->hikid = curr->eqkid = 0;

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
void ternary_cleanup (ternary_tree p)
{
    if (p)
    {
        ternary_cleanup (p->lokid);
        if (~p->splitchar)
            ternary_cleanup (p->eqkid);
        else free(p->eqkid);
        ternary_cleanup (p->hikid);
        free(p);
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

