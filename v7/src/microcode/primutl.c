/* -*-C-*-

$Id: primutl.c,v 9.61 1993/08/03 08:30:00 gjr Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

/* 
 * This file contains the support routines for mapping primitive names
 * to numbers within the microcode.  Primitives are written in C
 * and available in Scheme, but not always present in all versions of
 * the interpreter.  Thus, these objects are always referenced
 * externally by name and converted to numeric references only for the
 * duration of a single Scheme session.
 */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "prename.h"
#include "syscall.h"
#include "cmpgc.h"
#include <ctype.h>

extern SCHEME_OBJECT * load_renumber_table;

#ifndef UPDATE_PRIMITIVE_TABLE_HOOK
#  define UPDATE_PRIMITIVE_TABLE_HOOK(low, high) do { } while (0)
#endif

#ifndef GROW_PRIMITIVE_TABLE_HOOK
#  define GROW_PRIMITIVE_TABLE_HOOK(size) true
#endif

/*
  Exported variables:
 */

long MAX_PRIMITIVE = 0;

primitive_procedure_t * Primitive_Procedure_Table
  = ((primitive_procedure_t *) NULL);

int * Primitive_Arity_Table = ((int *) NULL);

int * Primitive_Count_Table = ((int *) NULL);

char ** Primitive_Name_Table = ((char **) NULL);

char ** Primitive_Documentation_Table = ((char **) NULL);

SCHEME_OBJECT * load_renumber_table = ((SCHEME_OBJECT *) NULL);

/*
  Exported utilities:
 */

extern void
  EXFUN (initialize_primitives, (void)),
  EXFUN (install_primitive_table, (SCHEME_OBJECT *, long));

extern SCHEME_OBJECT
  EXFUN (make_primitive, (char *)),
  EXFUN (find_primitive, (SCHEME_OBJECT, Boolean, Boolean, int)),
  EXFUN (declare_primitive, (char *, primitive_procedure_t, int, int, char *)),
  EXFUN (dump_renumber_primitive, (SCHEME_OBJECT)),
  * EXFUN (initialize_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *)),
  * EXFUN (cons_primitive_table, (SCHEME_OBJECT *, SCHEME_OBJECT *, long *)),
  * EXFUN (cons_whole_primitive_table,
	   (SCHEME_OBJECT *, SCHEME_OBJECT *, long *)),
  EXFUN (Prim_unimplemented, (void));

extern int
  EXFUN (strcmp_ci, (char *, char *));


/* Common utilities. */

#ifndef _toupper
#  define _toupper toupper
#endif

int
DEFUN (strcmp_ci, (s1, s2), fast char * s1 AND fast char * s2)
{
  fast int diff;

  while ((*s1 != '\0') && (*s2 != '\0'))
  {
    fast int c1 = (*s1++);
    fast int c2 = (*s2++);
    c1 = (_toupper (c1));
    c2 = (_toupper (c2));
    diff = (c1 - c2);
    if (diff != 0)
      return ((diff > 0) ? 1 : -1);
  }
  diff = (*s1 - *s2);
  return ((diff == 0) ? 0 : ((diff > 0) ? 1 : -1));
}

SCHEME_OBJECT
DEFUN_VOID (Prim_unimplemented)
{
  PRIMITIVE_HEADER (-1);

  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);
  /*NOTREACHED*/
}

static char * tree_error_message = ((char *) NULL);
static char * tree_error_noise   = ((char *) NULL);

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

typedef struct node_s * node;

struct node_s
{
  int height;
  node left;
  node rite;
  char * name;
  int value;
};

#define BRANCH_HEIGHT(tree) (((tree) == ((node) NULL)) ? 0 : (tree)->height)

#ifndef MAX
#  define MAX(a,b) (((a) >= (b)) ? (a) : (b))
#endif

static void
DEFUN (update_height, (tree), node tree)
{
  tree->height = (1 + (MAX ((BRANCH_HEIGHT (tree->left)),
			    (BRANCH_HEIGHT (tree->rite)))));
  return;
}

static node
DEFUN (leaf_make, (name, value),
       char * name AND int value)
{
  node leaf = ((node) (malloc (sizeof (struct node_s))));
  if (leaf == ((node) NULL))
  {
    tree_error ("leaf_make: malloc failed.\n", NULL);
    return (leaf);
  }
  leaf->name = name;
  leaf->value = value;
  leaf->height = 1;
  leaf->left = ((node) NULL);
  leaf->rite = ((node) NULL);
  return (leaf);
}

static node
DEFUN (rotate_left, (tree), node tree)
{
  node rite = tree->rite;
  node beta = rite->left;
  tree->rite = beta;
  rite->left = tree;
  update_height (tree);
  update_height (rite);
  return (rite);
}

static node
DEFUN (rotate_rite, (tree), node tree)
{
  node left = tree->left;
  node beta = left->rite;
  tree->left = beta;
  left->rite = tree;
  update_height (tree);
  update_height (left);
  return (left);
}

static node
DEFUN (rebalance_left, (tree), node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->rite))) >= (BRANCH_HEIGHT (tree->left)))
  {
    update_height (tree);
    return (tree);
  }
  else
  {
    node q = tree->left;
    if ((BRANCH_HEIGHT (q->rite)) > (BRANCH_HEIGHT (q->left)))
      tree->left = (rotate_left (q));
    return (rotate_rite (tree));
  }
}

static node
DEFUN (rebalance_rite, (tree), node tree)
{
  if ((1 + (BRANCH_HEIGHT (tree->left))) >= (BRANCH_HEIGHT (tree->rite)))
  {
    update_height (tree);
    return (tree);
  }
  else
  {
    node q = tree->rite;
    if ((BRANCH_HEIGHT (q->left)) > (BRANCH_HEIGHT (q->rite)))
      tree->rite = (rotate_rite (q));
    return (rotate_left (tree));
  }
}

static node
DEFUN (tree_insert, (tree, name, value),
       node tree
       AND char * name
       AND int value)
{
  if (tree == ((node) NULL))
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
}

static node
DEFUN (tree_lookup, (tree, name), node tree AND char * name)
{
  while (tree != ((node) NULL))
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

static node
DEFUN (tree_build, (high, names, values),
       int high AND char ** names AND int value)
{
  static int bias = 0;

  if (high > 1)
  {
    node tree;
    int middle = (high / 2);
    int next;

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
    return ((node) NULL);
}

static void
DEFUN (initialization_error, (reason, item), char * reason AND char * item)
{
  outf_fatal ("initialize_primitives: Error %s %s.\n",
	      reason, item);
  termination_init_error ();
}

static long prim_table_size = 0;

static Boolean
DEFUN (copy_table, (ltable, otable, item_size),
       PTR * ltable AND PTR otable AND int item_size)
{
  long size = (((long) item_size) * prim_table_size);
  PTR ntable;

  if (*ltable != ((PTR) NULL))
    ntable = ((PTR) (realloc (*ltable, size)));
  else
  {
    ntable = ((PTR) (malloc (size)));
    if (ntable != ((PTR) NULL))
      memcpy (ntable, otable, size);
  }
  if (ntable != ((PTR) NULL))
    *ltable = ntable;
  return (ntable != ((PTR) NULL));
}

static Boolean
DEFUN_VOID (grow_primitive_tables)
{
  Boolean result;
  long old_prim_table_size = prim_table_size;

  prim_table_size = (MAX_PRIMITIVE + (MAX_PRIMITIVE / 10));

  result = (   (copy_table (((PTR *) &Primitive_Arity_Table),
			    ((PTR) &Static_Primitive_Arity_Table[0]),
			    (sizeof (int))))
	    && (copy_table (((PTR *) &Primitive_Count_Table),
			    ((PTR) &Static_Primitive_Count_Table[0]),
			    (sizeof (int))))
	    && (copy_table (((PTR *) &Primitive_Name_Table),
			    ((PTR) &Static_Primitive_Name_Table[0]),
			    (sizeof (char *))))
	    && (copy_table (((PTR *) &Primitive_Documentation_Table),
			    ((PTR) &Static_Primitive_Documentation_Table[0]),
			    (sizeof (char *))))
	    && (copy_table (((PTR *) &Primitive_Procedure_Table),
			    ((PTR) &Static_Primitive_Procedure_Table[0]),
			    (sizeof (primitive_procedure_t))))
	    && (GROW_PRIMITIVE_TABLE_HOOK (prim_table_size)));
  if (result)
    UPDATE_PRIMITIVE_TABLE_HOOK (0, MAX_PRIMITIVE);
  else
    prim_table_size = prim_table_size;
  return (result);
}

static node prim_procedure_tree = ((node) NULL);

void
DEFUN_VOID (initialize_primitives)
{
  int counter;

  /* MAX_STATIC_PRIMITIVE is the index of the last primitive */

  MAX_PRIMITIVE = (MAX_STATIC_PRIMITIVE + 1);
  if (! (grow_primitive_tables ()))
    initialization_error ("allocating", "the primitive tables");

  tree_error_message = ((char *) NULL);
  prim_procedure_tree = (tree_build (MAX_PRIMITIVE, Primitive_Name_Table, 0));
  if (tree_error_message != ((char *) NULL))
  {
    outf_fatal (tree_error_message, tree_error_noise);
    initialization_error ("building", "prim_procedure_tree");
  }

  for (counter = 0; counter < N_PRIMITIVE_ALIASES; counter++)
  {
    node orig = (tree_lookup (prim_procedure_tree,
			      primitive_aliases[counter].name));

    if (orig == ((node) NULL))
    {
      outf_fatal ("Aliasing unknown primitive %s.\n",
		  primitive_aliases[counter].name,
		  primitive_aliases[counter].alias);
      initialization_error ("aliasing", primitive_aliases[counter].alias);
    }
    else
    {
      node new = (tree_insert (prim_procedure_tree,
			       primitive_aliases[counter].alias,
			       orig->value));
      if (tree_error_message != ((char *) NULL))
      {
	outf_fatal (tree_error_message, tree_error_noise);
	initialization_error ("aliasing", primitive_aliases[counter].alias);
      }
      prim_procedure_tree = new;
    }
  }
  return;
}

/* declare_primitive returns SHARP_F if it could not allocate
   the storage needed for the new primitive, or a primitive object.
   The primitive object may correspond to a pre-existend primitive
   if there is already a primitive by the same name.
   If it is a new primitive, its PRIMITIVE_NUMBER will be the
   previous value of MAX_PRIMITIVE.
   Note that it can return the value of an old primitive if it
   was previously unimplemented and the arity matches.
 */

SCHEME_OBJECT
DEFUN (declare_primitive, (name, code, nargs_lo, nargs_hi, docstr),
       char * name
       AND primitive_procedure_t code
       AND int nargs_lo
       AND int nargs_hi
       AND char * docstr)
/* nargs_lo ignored, for now */
{
  int index;
  SCHEME_OBJECT primitive;
  node prim = (tree_lookup (prim_procedure_tree, name));

  if (prim != ((node) NULL))
  {
    index = prim->value;
    primitive = (MAKE_PRIMITIVE_OBJECT (prim->value));
    if ((IMPLEMENTED_PRIMITIVE_P (primitive))
	|| (((PRIMITIVE_ARITY (primitive)) != nargs_hi)
	    && ((PRIMITIVE_ARITY (primitive)) != UNKNOWN_PRIMITIVE_ARITY)))
      return (primitive);
  }
  else
  {
    if (MAX_PRIMITIVE == prim_table_size)
      if (! (grow_primitive_tables ()))
	return (SHARP_F);

    /* Allocate a new primitive index, and insert in data base. */

    index = MAX_PRIMITIVE;
    prim = (tree_insert (prim_procedure_tree, name, index));
    if (tree_error_message != ((char *) NULL))
    {
      outf_error (tree_error_message, tree_error_noise);
      tree_error_message = ((char *) NULL);
      return (SHARP_F);
    }
    prim_procedure_tree = prim;

    MAX_PRIMITIVE += 1;
    primitive = (MAKE_PRIMITIVE_OBJECT (index));
    Primitive_Name_Table[index]        = name;
  }

  Primitive_Procedure_Table[index]     = code;
  Primitive_Arity_Table[index]         = nargs_hi;
  Primitive_Count_Table[index]         = (nargs_hi
					  * (sizeof (SCHEME_OBJECT)));
  Primitive_Documentation_Table[index] = docstr;
  UPDATE_PRIMITIVE_TABLE_HOOK (index, (index + 1));
  return (primitive);
}

/*
  make_primitive returns a primitive object,
  constructing one if necessary.
 */

SCHEME_OBJECT
DEFUN (make_primitive, (name), char * name)
{
  return (declare_primitive (name,
			     Prim_unimplemented,
			     UNKNOWN_PRIMITIVE_ARITY,
			     UNKNOWN_PRIMITIVE_ARITY,
			     ((char *) NULL)));
}

/* This returns all sorts of different things that the runtime
   system decodes.
 */

SCHEME_OBJECT
DEFUN (find_primitive, (sname, intern_p, allow_p, arity),
       SCHEME_OBJECT sname AND Boolean intern_p
       AND Boolean allow_p AND int arity)
{
  node prim = (tree_lookup (prim_procedure_tree, (STRING_LOC (sname, 0))));

  if (prim != ((node) NULL))
  {
    SCHEME_OBJECT primitive = (MAKE_PRIMITIVE_OBJECT (prim->value));

    if ((! allow_p) && (! (IMPLEMENTED_PRIMITIVE_P (primitive))))
      return (SHARP_F);
    
    if ((arity == UNKNOWN_PRIMITIVE_ARITY)
	|| (arity == (PRIMITIVE_ARITY (primitive))))
      return (primitive);
    else if ((PRIMITIVE_ARITY (primitive)) == UNKNOWN_PRIMITIVE_ARITY)
    {
      /* We've just learned the arity of the primitive. */
      Primitive_Arity_Table[PRIMITIVE_NUMBER (primitive)] = arity;
      return (primitive);
    }
    else
      /* Arity mismatch, notify the runtime system. */
      return (LONG_TO_FIXNUM (PRIMITIVE_ARITY (primitive)));
  }
  else if (! intern_p)
    return (SHARP_F);
  else
  {
    SCHEME_OBJECT primitive;
    char * cname = ((char *) (malloc (1 + (STRING_LENGTH (sname)))));

    if (cname == ((char *) NULL))
      error_in_system_call (syserr_not_enough_space, syscall_malloc);
    strcpy (cname, (STRING_LOC (sname, 0)));
    primitive =
      (declare_primitive (cname,
			  Prim_unimplemented,
			  ((arity < 0) ? 0 : arity),
			  arity,
			  ((char *) NULL)));
    if (primitive == SHARP_F)
      error_in_system_call (syserr_not_enough_space, syscall_malloc);
    return (primitive);
  }
}

/* These are used by fasdump to renumber primitives on the way out.
   Only those primitives actually referenced by the object being
   dumped are described in the output.  The primitives being
   dumped are renumbered in the output to a contiguous range
   starting at 0.
 */

static SCHEME_OBJECT * internal_renumber_table;
static SCHEME_OBJECT * external_renumber_table;
static long next_primitive_renumber;

/* This is called during fasdump setup. */

SCHEME_OBJECT *
DEFUN (initialize_primitive_table, (where, end),
       fast SCHEME_OBJECT * where AND SCHEME_OBJECT * end)
{
  SCHEME_OBJECT * top;
  fast long number_of_primitives;

  top = &where[2 * MAX_PRIMITIVE];
  if (top < end)
  {
    internal_renumber_table = where;
    external_renumber_table = &where[MAX_PRIMITIVE];
    next_primitive_renumber = 0;

    for (number_of_primitives = MAX_PRIMITIVE;
	 (--number_of_primitives >= 0);)
      (*where++) = SHARP_F;
  }
  return (top);
}

/* This is called every time fasdump meets a primitive to be renumbered.
   It is called on objects with tag TC_PRIMITIVE or TC_PCOMB0,
   so it preserves the tag of its argument.
 */

SCHEME_OBJECT
DEFUN (dump_renumber_primitive, (primitive), fast SCHEME_OBJECT primitive)
{
  fast long number;
  fast SCHEME_OBJECT result;

  number = (PRIMITIVE_NUMBER (primitive));
  result = internal_renumber_table[number];
  if (result != SHARP_F)
    return (MAKE_OBJECT_FROM_OBJECTS (primitive, result));
  else
  {
    result = (OBJECT_NEW_DATUM (primitive, next_primitive_renumber));
    internal_renumber_table[number] = result;
    external_renumber_table[next_primitive_renumber] = primitive;
    next_primitive_renumber += 1;
    return (result);
  }
}

/* Utility for fasdump and dump-band */

static SCHEME_OBJECT *
DEFUN (copy_primitive_information, (code, start, end),
       long code AND fast SCHEME_OBJECT * start AND fast SCHEME_OBJECT * end)
{
  static char null_string [] = "\0";
  fast char * source, * dest, * limit;
  long char_count, word_count;
  SCHEME_OBJECT * saved;

  if (start < end)
    (*start++) = (LONG_TO_FIXNUM (Primitive_Arity_Table [code]));

  source = (Primitive_Name_Table [code]);
  saved = start;
  start += STRING_CHARS;
  dest = ((char *) start);
  limit = ((char *) end);
  if (source == ((char *) 0))
    source = ((char *) (& (null_string [0])));
  while ((dest < limit) && (((*dest++) = (*source++)) != '\0'))
    ;
  if (dest >= limit)
    while ((*source++) != '\0')
      dest += 1;
  char_count = ((dest - 1) - ((char *) start));
  word_count = (STRING_LENGTH_TO_GC_LENGTH (char_count));
  start = (saved + 1 + word_count);
  if (start < end)
  {
    (saved [STRING_HEADER]) =
      (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, word_count));
    (saved [STRING_LENGTH_INDEX]) = ((SCHEME_OBJECT) char_count);
  }
  return (start);
}

/* This is called at the end of the relocation step to
   allocate the actual table to dump on the output file.
 */

SCHEME_OBJECT *
DEFUN (cons_primitive_table, (start, end, length),
       SCHEME_OBJECT * start AND SCHEME_OBJECT * end AND long * length)

{
  SCHEME_OBJECT * saved;
  long count, code;

  saved = start;
  * length = next_primitive_renumber;

  for (count = 0;
       ((count < next_primitive_renumber) && (start < end));
       count += 1)
  {
    code = (PRIMITIVE_NUMBER (external_renumber_table[count]));
    start = (copy_primitive_information (code, start, end));
  }
  return (start);
}

/* This is called when a band is dumped.
   All the primitives are dumped unceremoniously.
 */

SCHEME_OBJECT *
DEFUN (cons_whole_primitive_table, (start, end, length),
       SCHEME_OBJECT * start AND SCHEME_OBJECT * end AND long * length)
{
  SCHEME_OBJECT * saved;
  long count;

  saved = start;
  * length = MAX_PRIMITIVE;

  for (count = 0;
       ((count < MAX_PRIMITIVE) && (start < end));
       count += 1)
    start = (copy_primitive_information (count, start, end));

  return (start);
}

/* This is called from fasload and load-band */

void
DEFUN (install_primitive_table, (table, length),
       fast SCHEME_OBJECT * table
       AND fast long length)
{
  fast SCHEME_OBJECT * translation_table;
  SCHEME_OBJECT result;
  long arity;

  translation_table = load_renumber_table;
  while (--length >= 0)
  {
    arity = (FIXNUM_TO_LONG (* table));
    table += 1;
    result =
      (find_primitive ((MAKE_POINTER_OBJECT (TC_CHARACTER_STRING, table)),
		       true, true, arity));
    if ((OBJECT_TYPE (result)) != TC_PRIMITIVE)
      signal_error_from_primitive (ERR_WRONG_ARITY_PRIMITIVES);

    *translation_table++ = result;
    table += (1 + (OBJECT_DATUM (* table)));
  }
  return;
}
