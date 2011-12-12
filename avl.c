/*
 * avltree - Implements an AVL tree with parent pointers.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include "libtree.h"


#if !defined UINTPTR_MAX || UINTPTR_MAX != UINT64_MAX

static inline int is_root(struct avltree_node *node)
{
	return node->parent == NULL;
}

static inline void INIT_NODE(struct avltree_node *node)
{
	node->left = NULL;
	node->right = NULL;
	node->parent = NULL;
	node->balance = 0;
}

static inline signed get_balance(struct avltree_node *node)
{
	return node->balance;
}

static inline void set_balance(int balance, struct avltree_node *node)
{
	node->balance = balance;
}

static inline int inc_balance(struct avltree_node *node)
{
	return ++node->balance;
}

static inline int dec_balance(struct avltree_node *node)
{
	return --node->balance;
}

static inline struct avltree_node *get_parent(const struct avltree_node *node)
{
	return node->parent;
}

static inline void set_parent(struct avltree_node *parent,
			      struct avltree_node *node)
{
	node->parent = parent;
}

#else

static inline int is_root(struct avltree_node *node)
{
	return !(node->parent & ~7UL);
}

static inline void INIT_NODE(struct avltree_node *node)
{
	node->left = NULL;
	node->right = NULL;
	node->parent = 2;
}

static inline signed get_balance(struct avltree_node *node)
{
	return (int)(node->parent & 7) - 2;
}

static inline void set_balance(int balance, struct avltree_node *node)
{
	node->parent = (node->parent & ~7UL) | (balance + 2);
}

static inline int inc_balance(struct avltree_node *node)
{
	return (int)(++node->parent & 7) - 2;
}

static inline int dec_balance(struct avltree_node *node)
{
	return (int)(--node->parent & 7) - 2;
}

static inline struct avltree_node *get_parent(const struct avltree_node *node)
{
	return (struct avltree_node *)(node->parent & ~7UL);
}

static inline void set_parent(const struct avltree_node *parent,
			      struct avltree_node *node)
{
	node->parent = (uintptr_t)parent | (node->parent & 7);
}

#endif

/*
 * Iterators
 */
static inline struct avltree_node *get_first(struct avltree_node *node)
{
	while (node->left)
		node = node->left;
	return node;
}

struct avltree_node *avltree_first(const struct avltree *tree)
{
	return tree->first;
}

struct avltree_node *avltree_next(const struct avltree_node *node)
{
	struct avltree_node *parent;

	if (node->right)
		return get_first(node->right);

	while ((parent = get_parent(node)) && parent->right == node)
		node = parent;
	return parent;
}

/*
 * The AVL tree is more rigidly balanced than Red-Black trees, leading
 * to slower insertion and removal but faster retrieval.
 */

/* node->balance = height(node->right) - height(node->left); */
static void rotate_left(struct avltree_node *node, struct avltree *tree)
{
	struct avltree_node *p = node;
	struct avltree_node *q = node->right; /* can't be NULL */
	struct avltree_node *parent = get_parent(p);

	if (!is_root(p)) {
		if (parent->left == p)
			parent->left = q;
		else
			parent->right = q;
	} else
		tree->root = q;
	set_parent(parent, q);
	set_parent(q, p);

	p->right = q->left;
	if (p->right)
		set_parent(p, p->right);
	q->left = p;
}

static void rotate_right(struct avltree_node *node, struct avltree *tree)
{
	struct avltree_node *p = node;
	struct avltree_node *q = node->left; /* can't be NULL */
	struct avltree_node *parent = get_parent(p);

	if (!is_root(p)) {
		if (parent->left == p)
			parent->left = q;
		else
			parent->right = q;
	} else
		tree->root = q;
	set_parent(parent, q);
	set_parent(q, p);

	p->left = q->right;
	if (p->left)
		set_parent(p, p->left);
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
					     int *is_left)
{
	struct avltree_node *node = tree->root;
	int res = 0;

	*pparent = NULL;
	*unbalanced = node;
	*is_left = 0;

	while (node) {
		if (get_balance(node) != 0)
			*unbalanced = node;

		res = tree->cmp_fn(node, key);
		if (res == 0)
			return node;
		*pparent = node;
		if ((*is_left = res > 0))
			node = node->left;
		else
			node = node->right;
	}
	return NULL;
}

struct avltree_node *avltree_lookup(const struct avltree_node *key,
				    const struct avltree *tree)
{
	struct avltree_node *parent, *unbalanced;
	int is_left;

	return do_lookup(key, tree, &parent, &unbalanced, &is_left);
}

static inline void set_child(struct avltree_node *child,
		      struct avltree_node *node, int left)
{
	if (left)
		node->left = child;
	else
		node->right = child;
}

/* Insertion never needs more than 2 rotations */
struct avltree_node *avltree_insert(struct avltree_node *node, struct avltree *tree)
{
	struct avltree_node *key, *parent, *unbalanced;
	int is_left;

	key = do_lookup(node, tree, &parent, &unbalanced, &is_left);
	if (key)
		return key;

	INIT_NODE(node);

	if (!parent) {
		tree->root = node;
		tree->first = tree->last = node;
		tree->height++;
		return NULL;
	}
	if (is_left) {
		if (parent == tree->first)
			tree->first = node;
	} else {
		if (parent == tree->last)
			tree->last = node;
	}
	set_parent(parent, node);
	set_child(node, parent, is_left);

	for (;;) {
		if (parent->left == node)
			dec_balance(parent);
		else
			inc_balance(parent);

		if (parent == unbalanced)
			break;
		node = parent;
		parent = get_parent(parent);
	}

	switch (get_balance(unbalanced)) {
	case  1: case -1:
		tree->height++;
		/* fall through */
	case 0:
		break;
	case 2: {
		struct avltree_node *right = unbalanced->right;

		if (get_balance(right) == 1) {
			set_balance(0, unbalanced);
			set_balance(0, right);
		} else {
			switch (get_balance(right->left)) {
			case 1:
				set_balance(-1, unbalanced);
				set_balance( 0, right);
				break;
			case 0:
				set_balance(0, unbalanced);
				set_balance(0, right);
				break;
			case -1:
				set_balance(0, unbalanced);
				set_balance(1, right);
				break;
			}
			set_balance(0, right->left);

			rotate_right(right, tree);
		}
		rotate_left(unbalanced, tree);
		break;
	}
	case -2: {
		struct avltree_node *left = unbalanced->left;

		if (get_balance(left) == -1) {
			set_balance(0, unbalanced);
			set_balance(0, left);
		} else {
			switch (get_balance(left->right)) {
			case 1:
				set_balance( 0, unbalanced);
				set_balance(-1, left);
				break;
			case 0:
				set_balance(0, unbalanced);
				set_balance(0, left);
				break;
			case -1:
				set_balance(1, unbalanced);
				set_balance(0, left);
				break;
			}
			set_balance(0, left->right);

			rotate_left(left, tree);
		}
		rotate_right(unbalanced, tree);
		break;
	}
	}
	return NULL;
}

void avltree_init(struct avltree *tree, avltree_cmp_fn_t cmp, avltree_free_fn_t free)
{
	tree->root = NULL;
	tree->cmp_fn = cmp;
	tree->free_fn = free;
	tree->height = -1;
	tree->first = NULL;
	tree->last = NULL;
}

static void destroy(const struct avltree *tree, const struct avltree_node *node)
{
        const struct avltree_node *tmp;
	while (node) {
		destroy(tree, node->left);
		tmp = node;
		node = node->right;
		tree->free_fn(tmp);
	}
}

void avltree_destroy(const struct avltree *tree)
{
	destroy(tree, tree->root);
}
