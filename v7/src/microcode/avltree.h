/* -*-C-*-

$Id: avltree.h,v 1.3 2000/12/05 21:23:42 cph Exp $

Copyright (c) 1993, 1999, 2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This file contains external declarations for a simple
   AVL tree library.
   It is used by the MIT Scheme microcode to quickly map
   names to indices into various tables.
 */

#include "config.h"

extern char * tree_error_message;
extern char * tree_error_noise;

typedef struct tree_node_s * tree_node;

struct tree_node_s
{
  int height;
  tree_node left;
  tree_node rite;
  char * name;
  unsigned long value;
};

extern tree_node EXFUN (tree_build, (unsigned long, char **, unsigned long));
extern tree_node EXFUN (tree_lookup, (tree_node, char *));
extern tree_node EXFUN (tree_insert, (tree_node, char *, unsigned long));
extern void EXFUN (tree_free, (tree_node));
