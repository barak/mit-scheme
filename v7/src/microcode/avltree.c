/* -*-C-*-

$Id: avltree.c,v 1.3 1997/01/02 05:21:28 cph Exp $

Copyright (c) 1993-97 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* This file contains the code for a simple AVL tree library.
   It is used by the MIT Scheme microcode to quickly map
   names to indices into various tables.
 */

#include "avltree.h"

int EXFUN (strcmp_ci, (char * s1, char * s2));

#ifndef NULL
# define NULL ((PTR) 0)
#endif

char * tree_error_message = ((char *) NULL);
char * tree_error_noise   = ((char *) NULL);

static void
DEFUN (tree_error, (message, noise), char * message AND char * noise)
{
  tree_error_message = message;
  tree_error_noise   = noise;
  return;
}

/* AVL trees.  o(log n) lookup, insert (and delete, not implemented here).
   AVL condition: for every node
     abs (height (node.left) - height (node.right)) < 2
   This guarantees that the least-balanced AVL tree has Fibonacci-sized
   branches, and therefore the height is at most the log base phi of the
   number of nodes, where phi is the golden ratio.
   With random insertion (or when created as below),
   they are better, approaching log base 2.

   This version does not allow duplicate entries.
 */   

#define BRANCH_HEIGHT(tree)						\
  (((tree) == ((tree_node) NULL)) ? 0 : (tree)->height)

#ifndef MAX
#  define MAX(a,b) (((a) >= (b)) ? (a) : (b))
#endif

static void
DEFUN (update_height, (tree), tree_node tree)
{
  tree->height = (1 + (MAX ((BRANCH_HEIGHT (tree->left)),
			    (BRANCH_HEIGHT (tree->rite)))));
  return;
}

static tree_node
DEFUN (leaf_make, (name, value),
       char * name AND unsigned long value)
{
  extern PTR EXFUN (malloc, (unsigned long));
  tree_node leaf = ((tree_node) (malloc (sizeof (struct tree_node_s))));

  if (leaf == ((tree_node) NULL))
  {
    tree_error ("leaf_make: malloc failed.\n", NULL);
    return (leaf);
  }
  leaf->name = name;
  leaf->value = value;
  leaf->height = 1;
  leaf->left = ((tree_node) NULL);
  leaf->rite = ((tree_node) NULL);
  return (leaf);
}

static tree_node
DEFUN (rotate_left, (tree), tree_node tree)
{
  tree_node rite = tree->rite;
  tree_node beta = rite->left;
  tree->rite = beta;
  rite->left = tree;
  update_height (tree);
  update_height (rite);
  return (rite);
}

static tree_node
DEFUN (rotate_rite, (tree), tree_node tree)
{
  tree_node left = tree->left;
  tree_node beta = left->rite;
  tree->left = beta;
  left->rite = tree;
  update_height (tree);
  update_height (left);
  return (left);
}

static tree_node
DEFUN (rebalance_left, (tree), tree_node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->rite))) >= (BRANCH_HEIGHT (tree->left)))
  {
    update_height (tree);
    return (tree);
  }
  else
  {
    tree_node q = tree->left;
    if ((BRANCH_HEIGHT (q->rite)) > (BRANCH_HEIGHT (q->left)))
      tree->left = (rotate_left (q));
    return (rotate_rite (tree));
  }
}

static tree_node
DEFUN (rebalance_rite, (tree), tree_node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->left))) >= (BRANCH_HEIGHT (tree->rite)))
  {
    update_height (tree);
    return (tree);
  }
  else
  {
    tree_node q = tree->rite;
    if ((BRANCH_HEIGHT (q->left)) > (BRANCH_HEIGHT (q->rite)))
      tree->rite = (rotate_rite (q));
    return (rotate_left (tree));
  }
}

tree_node
DEFUN (tree_insert, (tree, name, value),
       tree_node tree
       AND char * name
       AND unsigned long value)
{
  if (tree == ((tree_node) NULL))
    return (leaf_make (name, value));
  switch (strcmp_ci (name, tree->name))
  {
    case 0:
      tree_error ("tree_insert: Duplicate entry %s.\n", name);
      return (tree);
      
    case -1:
    {
      /* To the left */
      tree->left = (tree_insert (tree->left, name, value));
      return (rebalance_left (tree));
    }

    case 1:
    {
      /* To the right */
      tree->rite = (tree_insert (tree->rite, name, value));
      return (rebalance_rite (tree));
    }
  }
  /*NOTREACHED*/
  return (0);
}

tree_node
DEFUN (tree_lookup, (tree, name), tree_node tree AND char * name)
{
  while (tree != ((tree_node) NULL))
    switch (strcmp_ci (name, tree->name))
    {
      case 0:
	return (tree);

      case -1:
	tree = tree->left;
	break;

      case 1:
	tree = tree->rite;
	break;
    }
  return (tree);
}

tree_node
DEFUN (tree_build, (high, names, value),
       unsigned long high AND char ** names AND unsigned long value)
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
    tree = (leaf_make (names[middle], (value + middle)));
    tree->left = (tree_build (middle, names, value));
    tree->rite = (tree_build ((high - next), &names[next], (value + next)));
    update_height (tree);
    return (tree);
  }
  else if (high == 1)
    return (leaf_make (* names, value));
  else
    return ((tree_node) NULL);
}

void
DEFUN (tree_free, (tree), tree_node tree)
{
  extern void EXFUN (free, (PTR));

  if (tree != ((tree_node) NULL))
  {
    tree_free (tree->left);
    tree_free (tree->rite);
    free (tree);
  }
  return;
}
