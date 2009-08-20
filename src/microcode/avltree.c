/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* This file contains the code for a simple AVL tree library.
   It is used by the MIT/GNU Scheme microcode to quickly map
   names to indices into various tables.  */

#include "avltree.h"

extern int strcmp_ci (const char *, const char *);

const char * tree_error_message = 0;
const char * tree_error_noise = 0;

static void
tree_error (const char * message, const char * noise)
{
  tree_error_message = message;
  tree_error_noise = noise;
}

/* AVL trees.  o(log n) lookup, insert (and delete, not implemented here).
   AVL condition: for every node
     abs (height (node.left) - height (node.right)) < 2
   This guarantees that the least-balanced AVL tree has Fibonacci-sized
   branches, and therefore the height is at most the log base phi of the
   number of nodes, where phi is the golden ratio.
   With random insertion (or when created as below),
   they are better, approaching log base 2.

   This version does not allow duplicate entries.  */

#define BRANCH_HEIGHT(tree) (((tree) == 0) ? 0 : ((tree) -> height))

#ifndef MAX
#  define MAX(a,b) (((a) >= (b)) ? (a) : (b))
#endif

static void
update_height (tree_node tree)
{
  (tree->height) = (1 + (MAX ((BRANCH_HEIGHT (tree->left)),
			    (BRANCH_HEIGHT (tree->rite)))));
}

static tree_node
leaf_make (const char * name, unsigned long value)
{
  tree_node leaf = ((tree_node) (malloc (sizeof (struct tree_node_s))));
  if (leaf == 0)
    {
      tree_error ("leaf_make: malloc failed.\n", 0);
      return (leaf);
    }
  (leaf->name) = name;
  (leaf->value) = value;
  (leaf->height) = 1;
  (leaf->left) = 0;
  (leaf->rite) = 0;
  return (leaf);
}

static tree_node
rotate_left (tree_node tree)
{
  tree_node rite = (tree->rite);
  tree_node beta = (rite->left);
  (tree->rite) = beta;
  (rite->left) = tree;
  update_height (tree);
  update_height (rite);
  return (rite);
}

static tree_node
rotate_rite (tree_node tree)
{
  tree_node left = (tree->left);
  tree_node beta = (left->rite);
  (tree->left) = beta;
  (left->rite) = tree;
  update_height (tree);
  update_height (left);
  return (left);
}

static tree_node
rebalance_left (tree_node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->rite))) >= (BRANCH_HEIGHT (tree->left)))
    {
      update_height (tree);
      return (tree);
    }
  else
    {
      tree_node q = (tree->left);
      if ((BRANCH_HEIGHT (q->rite)) > (BRANCH_HEIGHT (q->left)))
	(tree->left) = (rotate_left (q));
      return (rotate_rite (tree));
    }
}

static tree_node
rebalance_rite (tree_node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->left))) >= (BRANCH_HEIGHT (tree->rite)))
    {
      update_height (tree);
      return (tree);
    }
  else
    {
      tree_node q = (tree->rite);
      if ((BRANCH_HEIGHT (q->left)) > (BRANCH_HEIGHT (q->rite)))
	(tree->rite) = (rotate_rite (q));
      return (rotate_left (tree));
    }
}

tree_node
tree_insert (tree_node tree, const char * name, unsigned long value)
{
  if (tree == 0)
    return (leaf_make (name, value));
  switch (strcmp_ci (name, (tree->name)))
    {
    case 0:
      tree_error ("tree_insert: Duplicate entry %s.\n", name);
      return (tree);

    case (-1):
      {
	(tree->left) = (tree_insert ((tree->left), name, value));
	return (rebalance_left (tree));
      }

    case 1:
      {
	(tree->rite) = (tree_insert ((tree->rite), name, value));
	return (rebalance_rite (tree));
      }
    }
  /*NOTREACHED*/
  return (0);
}

tree_node
tree_lookup (tree_node tree, const char * name)
{
  while (tree != 0)
    switch (strcmp_ci (name, (tree->name)))
      {
      case 0:
	return (tree);

      case (-1):
	tree = (tree->left);
	break;

      case 1:
	tree = (tree->rite);
	break;
      }
  return (tree);
}

tree_node
tree_build (unsigned long high, const char ** names, unsigned long value)
{
  static long bias = 0;
  if (high > 1)
    {
      tree_node tree;
      long middle = (high / 2);
      long next;

      if ((high & 1) == 0)
	{
	  middle -= bias;
	  bias = (1 - bias);
	}
      next = (middle + 1);
      tree = (leaf_make ((names[middle]), (value + middle)));
      (tree->left) = (tree_build (middle, names, value));
      (tree->rite)
	= (tree_build ((high - next), (& (names[next])), (value + next)));
      update_height (tree);
      return (tree);
    }
  return ((high == 1) ? (leaf_make ((*names), value)) : 0);
}

void
tree_free (tree_node tree)
{
  if (tree != 0)
    {
      tree_free (tree->left);
      tree_free (tree->rite);
      free (tree);
    }
}
