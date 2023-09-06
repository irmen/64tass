/*
 * avltree - Implements an AVL tree with parent pointers.
 * $Id: avl.c 3086 2023-09-03 06:23:08Z soci $
 *
 * Copyright (C) 2010 Franck Bui-Huu <fbuihuu@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

#include "avl.h"
#include "attributes.h"
#include "stdbool.h"

/*
 * The AVL tree is more rigidly balanced than Red-Black trees, leading
 * to slower insertion and removal but faster retrieval.
 */

static void rotate_left(struct avltree_node *node, struct avltree *tree)
{
    struct avltree_node *p = node;
    struct avltree_node *q = node->right; /* can't be NULL */
    struct avltree_node *parent = p->parent;

    if (parent != NULL) {
        if (parent->left == p)
            parent->left = q;
        else
            parent->right = q;
    } else
        tree->root = q;
    q->parent = parent;
    p->parent = q;

    p->right = q->left;
    if (p->right != NULL)
        p->right->parent = p;
    q->left = p;
}

static void rotate_right(struct avltree_node *node, struct avltree *tree)
{
    struct avltree_node *p = node;
    struct avltree_node *q = node->left; /* can't be NULL */
    struct avltree_node *parent = p->parent;

    if (parent != NULL) {
        if (parent->left == p)
            parent->left = q;
        else
            parent->right = q;
    } else
        tree->root = q;
    q->parent = parent;
    p->parent = q;

    p->left = q->right;
    if (p->left != NULL)
        p->left->parent = p;
    q->right = p;
}

/*
 * 'pparent', 'unbalanced' and 'is_left' are only used for
 * insertions. Normally GCC will notice this and get rid of them for
 * lookups.
 */
static inline struct avltree_node *do_lookup(const struct avltree_node *key,
                                             const struct avltree *tree,
                                             struct avltree_node **pparent,
                                             struct avltree_node **unbalanced,
                                             bool *is_left, avltree_cmp_fn_t cmp)
{
    struct avltree_node *node = tree->root;
    int res;

    *pparent = NULL;
    *unbalanced = node;
    *is_left = false;

    while (node != NULL) {
        if (node->balance != 0)
            *unbalanced = node;

        res = cmp(node, key);
        if (res == 0)
            break;
        *pparent = node;
        if ((*is_left = (res > 0)))
            node = node->left;
        else
            node = node->right;
    }
    return node;
}

FAST_CALL struct avltree_node *avltree_lookup(const struct avltree_node *key,
                                    const struct avltree *tree, avltree_cmp_fn_t cmp)
{
    struct avltree_node *node = tree->root;

    while (node != NULL) {
        int res = cmp(node, key);
        if (res == 0) break;
        node = (res > 0) ? node->left : node->right;
    }
    return node;
}

/* Insertion never needs more than 2 rotations */
struct avltree_node *avltree_insert(struct avltree_node *node, struct avltree *tree, avltree_cmp_fn_t cmp)
{
    struct avltree_node *key, *parent, *unbalanced;
    bool is_left;

    key = do_lookup(node, tree, &parent, &unbalanced, &is_left, cmp);
    if (key != NULL)
        return key;

    node->left = NULL;
    node->right = NULL;
    node->balance = 0;
    node->parent = parent;

    if (parent == NULL) {
        tree->root = node;
        return NULL;
    }
    if (is_left) {
        parent->left = node;
    } else {
        parent->right = node;
    }

    for (;;) {
        if (parent->left == node)
            parent->balance--;
        else
            parent->balance++;

        if (parent == unbalanced)
            break;
        node = parent;
        parent = parent->parent;
    }

    switch (unbalanced->balance) {
    case  1: case -1:
    case 0:
        break;
    case 2: {
        struct avltree_node *right = unbalanced->right;

        if (right->balance == 1) {
            unbalanced->balance = 0;
            right->balance = 0;
        } else {
            switch (right->left->balance) {
            case 1:
                unbalanced->balance = -1;
                right->balance = 0;
                break;
            case 0:
                unbalanced->balance = 0;
                right->balance = 0;
                break;
            case -1:
                unbalanced->balance = 0;
                right->balance = 1;
                break;
            }
            right->left->balance = 0;

            rotate_right(right, tree);
        }
        rotate_left(unbalanced, tree);
        break;
    }
    case -2: {
        struct avltree_node *left = unbalanced->left;

        if (left->balance == -1) {
            unbalanced->balance = 0;
            left->balance = 0;
        } else {
            switch (left->right->balance) {
            case 1:
                unbalanced->balance =  0;
                left->balance = -1;
                break;
            case 0:
                unbalanced->balance = 0;
                left->balance = 0;
                break;
            case -1:
                unbalanced->balance = 1;
                left->balance = 0;
                break;
            }
            left->right->balance = 0;

            rotate_left(left, tree);
        }
        rotate_right(unbalanced, tree);
        break;
    }
    }
    return NULL;
}

/* Deletion might require up to log(n) rotations */
void avltree_remove(struct avltree_node *node, struct avltree *tree)
{
    struct avltree_node *parent = node->parent;
    struct avltree_node *left = node->left;
    struct avltree_node *right = node->right;
    struct avltree_node *next;
    bool is_left = false;

    if (left == NULL)
        next = right;
    else if (right == NULL)
        next = left;
    else {
        next = right;
        while (next->left)
            next = next->left;
    }

    if (parent != NULL) {
        is_left = parent->left == node;
        if (is_left)
            parent->left = next;
        else
            parent->right = next;
    } else
        tree->root = next;

    if (left != NULL && right != NULL) {
        next->balance = node->balance;

        next->left = left;
        left->parent = next;

        if (next != right) {
            parent = next->parent;
            next->parent = node->parent;

            node = next->right;
            parent->left = node;
            is_left = true;

            next->right = right;
            right->parent = next;
        } else {
            next->parent = parent;
            parent = next;
            node = parent->right;
            is_left = false;
        }
    } else
        node = next;

    if (node != NULL)
        node->parent = parent;

    /*
     * At this point, 'parent' can only be null, if 'node' is the
     * tree's root and has at most one child.
     *
     * case 1: the subtree is now balanced but its height has
     * decreased.
     *
     * case 2: the subtree is mostly balanced and its height is
     * unchanged.
     *
     * case 3: the subtree is unbalanced and its height may have
     * been changed during the rebalancing process, see below.
     *
     * case 3.1: after a left rotation, the subtree becomes mostly
     * balanced and its height is unchanged.
     *
     * case 3.2: after a left rotation, the subtree becomes
     * balanced but its height has decreased.
     *
     * case 3.3: after a left and a right rotation, the subtree
     * becomes balanced or mostly balanced but its height has
     * decreased for all cases.
     */
    while (parent) {
        node   = parent;
        parent = parent->parent;

        if (is_left) {
            is_left = parent != NULL && parent->left == node;

            node->balance++;
            if (node->balance == 0)     /* case 1 */
                continue;
            if (node->balance == 1)     /* case 2 */
                return;
            right = node->right;        /* case 3 */
            switch (right->balance) {
            case 0:                     /* case 3.1 */
                node->balance = 1;
                right->balance = -1;
                rotate_left(node, tree);
                return;
            case 1:                     /* case 3.2 */
                node->balance = 0;
                right->balance = 0;
                break;
            case -1:                    /* case 3.3 */
                switch (right->left->balance) {
                case 1:
                    node->balance = -1;
                    right->balance = 0;
                    break;
                case 0:
                    node->balance = 0;
                    right->balance = 0;
                    break;
                case -1:
                    node->balance = 0;
                    right->balance = 1;
                    break;
                }
                right->left->balance = 0;

                rotate_right(right, tree);
            }
            rotate_left(node, tree);
        } else {
            is_left = parent != NULL && parent->left == node;

            node->balance--;
            if (node->balance == 0)
                continue;
            if (node->balance == -1)
                return;
            left = node->left;
            switch (left->balance) {
            case 0:
                node->balance = -1;
                left->balance = 1;
                rotate_right(node, tree);
                return;
            case -1:
                node->balance = 0;
                left->balance = 0;
                break;
            case 1:
                switch (left->right->balance) {
                case 1:
                    node->balance = 0;
                    left->balance = -1;
                    break;
                case 0:
                    node->balance = 0;
                    left->balance = 0;
                    break;
                case -1:
                    node->balance = 1;
                    left->balance = 0;
                    break;
                }
                left->right->balance = 0;

                rotate_left(left, tree);
            }
            rotate_right(node, tree);
        }
    }
}

static void destroy(struct avltree_node *node, avltree_free_fn_t free_fn)
{
    do {
        struct avltree_node *tmp = node;
        if (node->left != NULL) destroy(node->left, free_fn);
        node = node->right;
        free_fn(tmp);
    } while (node != NULL);
}

void avltree_destroy(struct avltree *tree, avltree_free_fn_t free_fn)
{
    if (tree->root != NULL) destroy(tree->root, free_fn);
}

