/* -*-C-*-

$Id: primutl.c,v 9.75 2002/11/20 19:46:13 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

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
#include "os.h"
#include "usrdef.h"
#include "prename.h"
#include "syscall.h"
#include "avltree.h"
#include "cmpgc.h"
#include <ctype.h>

extern PTR EXFUN (malloc, (size_t));
extern PTR EXFUN (realloc, (PTR, size_t));

#ifdef STDC_HEADERS
#  include <string.h>
#else
   extern PTR EXFUN (memcpy, (PTR, CONST PTR, size_t));
   extern char * EXFUN (strcpy, (char *, CONST char *));
#endif

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

primitive_procedure_t * Primitive_Procedure_Table = 0;

int * Primitive_Arity_Table = 0;

int * Primitive_Count_Table = 0;

CONST char ** Primitive_Name_Table = 0;

CONST char ** Primitive_Documentation_Table = 0;

SCHEME_OBJECT * load_renumber_table = 0;

/*
  Exported utilities:
 */

extern void
  EXFUN (initialize_primitives, (void)),
  EXFUN (install_primitive_table, (SCHEME_OBJECT *, long));

extern SCHEME_OBJECT
  EXFUN (make_primitive, (char *, int)),
  EXFUN (find_primitive, (SCHEME_OBJECT, Boolean, Boolean, int)),
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
    if (islower (c1)) c1 = (_toupper (c1));
    if (islower (c2)) c2 = (_toupper (c2));
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
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
DEFUN (initialization_error, (reason, item), char * reason AND char * item)
{
  outf_fatal ("initialize_primitives: Error %s %s.\n", reason, item);
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

static tree_node prim_procedure_tree = ((tree_node) NULL);

void
DEFUN_VOID (initialize_primitives)
{
  unsigned long counter;

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
    unsigned long index;
    tree_node new;
    tree_node orig = (tree_lookup (prim_procedure_tree,
				   primitive_aliases[counter].name));

    if (orig != ((tree_node) NULL))
      index = orig->value;
    else
    {
      SCHEME_OBJECT old = (make_primitive (primitive_aliases[counter].name,
					   UNKNOWN_PRIMITIVE_ARITY));
      
      if (old == SHARP_F)
      {
	outf_fatal ("Error declaring unknown primitive %s.\n",
		    primitive_aliases[counter].name);
	initialization_error ("aliasing", primitive_aliases[counter].alias);
      }
      index = (PRIMITIVE_NUMBER (old));
    }

    new = (tree_insert (prim_procedure_tree,
			primitive_aliases[counter].alias,
			index));
    if (tree_error_message != ((char *) NULL))
    {
      outf_fatal (tree_error_message, tree_error_noise);
      initialization_error ("aliasing", primitive_aliases[counter].alias);
    }
    prim_procedure_tree = new;
  }
  return;
}

static SCHEME_OBJECT
DEFUN (declare_primitive_internal,
       (override_p, name, code, nargs_lo, nargs_hi, docstr),
       Boolean override_p
       AND CONST char * name
       AND primitive_procedure_t code
       AND int nargs_lo
       AND int nargs_hi
       AND CONST char * docstr)
/* nargs_lo ignored, for now */
{
  unsigned long index;
  SCHEME_OBJECT primitive;
  CONST char * ndocstr = docstr;
  tree_node prim = (tree_lookup (prim_procedure_tree, name));

  if (prim != ((tree_node) NULL))
  {
    index = prim->value;
    primitive = (MAKE_PRIMITIVE_OBJECT (prim->value));
    if ((((PRIMITIVE_ARITY (primitive)) != nargs_hi)
	 && ((PRIMITIVE_ARITY (primitive)) != UNKNOWN_PRIMITIVE_ARITY))
	|| ((IMPLEMENTED_PRIMITIVE_P (primitive)) && (! override_p)))
      return (LONG_TO_UNSIGNED_FIXNUM (PRIMITIVE_NUMBER (primitive)));
    if (docstr == 0)
      ndocstr = (Primitive_Documentation_Table[index]);
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
  Primitive_Count_Table[index]         = (nargs_hi * (sizeof (SCHEME_OBJECT)));
  Primitive_Documentation_Table[index] = ndocstr;
  UPDATE_PRIMITIVE_TABLE_HOOK (index, (index + 1));
  return (primitive);
}

/* declare_primitive installs a new primitive in the system.
   It returns:
   - A primitive object if it succeeds.
   - SHARP_F if there was a problem trying to install it (e.g. out of memory).
   - A fixnum whose value is the number of the pre-existing primitive
     that it would replace.
   Note that even if a primitive is returned, its number may not
   be the previous value of MAX_PRIMITIVE, since the system may
   have pre-existent references to the previously-unimplemented primitive.
 */

SCHEME_OBJECT
DEFUN (declare_primitive, (name, code, nargs_lo, nargs_hi, docstr),
       CONST char * name
       AND primitive_procedure_t code
       AND int nargs_lo
       AND int nargs_hi
       AND CONST char * docstr)
{
  return (declare_primitive_internal (false, name, code,
				      nargs_lo, nargs_hi, docstr));
}

/* install_primitive is similar to declare_primitive, but will
   replace a pre-existing primitive if the arities are consistent.
   If they are not, it returns a fixnum whose value is the index
   of the pre-existing primitive.
 */

SCHEME_OBJECT
DEFUN (install_primitive, (name, code, nargs_lo, nargs_hi, docstr),
       CONST char * name
       AND primitive_procedure_t code
       AND int nargs_lo
       AND int nargs_hi
       AND CONST char * docstr)
{
  return (declare_primitive_internal (true, name, code,
				      nargs_lo, nargs_hi, docstr));
}

/*
  make_primitive returns a primitive object,
  constructing one if necessary.
 */

SCHEME_OBJECT
DEFUN (make_primitive, (name, arity), char * name AND int arity)
{
  SCHEME_OBJECT result;

  result = (declare_primitive (name,
			       Prim_unimplemented,
			       arity,
			       arity,
			       ((char *) NULL)));
  return ((result == SHARP_F)
	  ? SHARP_F
	  : (OBJECT_NEW_TYPE (TC_PRIMITIVE, result)));
}

/* This returns all sorts of different things that the runtime
   system decodes.
 */

SCHEME_OBJECT
DEFUN (find_primitive, (sname, intern_p, allow_p, arity),
       SCHEME_OBJECT sname AND Boolean intern_p
       AND Boolean allow_p AND int arity)
{
  tree_node prim = (tree_lookup (prim_procedure_tree,
				 ((char *) (STRING_LOC (sname, 0)))));

  if (prim != ((tree_node) NULL))
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
    strcpy (cname, ((char *) (STRING_LOC (sname, 0))));
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
  CONST char * source;
  char * dest;
  char * limit;
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
