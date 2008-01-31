/* -*-C-*-

$Id: primutl.c,v 9.85 2008/01/30 20:02:18 cph Exp $

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

#ifndef UPDATE_PRIMITIVE_TABLE_HOOK
#  define UPDATE_PRIMITIVE_TABLE_HOOK(low, high) do { } while (0)
#endif

#ifndef GROW_PRIMITIVE_TABLE_HOOK
#  define GROW_PRIMITIVE_TABLE_HOOK(size) true
#endif

static prim_renumber_t * make_prim_renumber_1 (unsigned long);
static void free_prim_renumber (void *);
static SCHEME_OBJECT * make_table_entry (unsigned long, SCHEME_OBJECT *);
static unsigned long table_entry_length (unsigned long);


/* Exported variables: */

unsigned long MAX_PRIMITIVE = 0;

primitive_procedure_t * Primitive_Procedure_Table = 0;

int * Primitive_Arity_Table = 0;

int * Primitive_Count_Table = 0;

const char ** Primitive_Name_Table = 0;

const char ** Primitive_Documentation_Table = 0;

SCHEME_OBJECT * load_renumber_table = 0;

/* Common utilities. */

int
strcmp_ci (const char * s1, const char * s2)
{
  const unsigned char * p1 = ((unsigned char *) s1);
  const unsigned char * p2 = ((unsigned char *) s2);
  while (true)
    {
      int c1 = (*p1++);
      int c2 = (*p2++);
      if (c1 == '\0')
	return ((c2 == '\0') ? 0 : (-1));
      if (c2 == '\0')
	return (1);
      c1 = (toupper (c1));
      c2 = (toupper (c2));
      if (c1 < c2)
	return (-1);
      if (c1 > c2)
	return (1);
    }
}

SCHEME_OBJECT
Prim_unimplemented (void)
{
  PRIMITIVE_HEADER (-1);

  signal_error_from_primitive (ERR_UNIMPLEMENTED_PRIMITIVE);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
initialization_error (char * reason, char * item)
{
  outf_fatal ("initialize_primitives: Error %s %s.\n", reason, item);
  termination_init_error ();
}

static unsigned long prim_table_size = 0;

#define COPY_TABLE(table, static_table, elt_t, static_elt_t) do		\
{									\
  if (table == 0)							\
    {									\
      static_elt_t * from = (& (static_table [0]));			\
      static_elt_t * from_end						\
	= (& (static_table [MAX_STATIC_PRIMITIVE + 1]));		\
      elt_t * to;							\
									\
      table = (OS_malloc (new_size * (sizeof (elt_t))));		\
      to = ((elt_t *) table);						\
      while (from < from_end)						\
	(*to++) = ((elt_t) (*from++));					\
    }									\
  else									\
    table = (OS_realloc (table, (new_size * (sizeof (elt_t)))));	\
} while (0)

static void
grow_primitive_tables (void)
{
  unsigned long new_size = (MAX_PRIMITIVE + (MAX_PRIMITIVE / 10));
  COPY_TABLE (Primitive_Arity_Table, Static_Primitive_Arity_Table, int, int);
  COPY_TABLE (Primitive_Count_Table, Static_Primitive_Count_Table, int, int);
  COPY_TABLE (Primitive_Name_Table,
	      Static_Primitive_Name_Table,
	      char *,
	      const char *);
  COPY_TABLE (Primitive_Documentation_Table,
	      Static_Primitive_Documentation_Table,
	      char *,
	      const char *);
  COPY_TABLE (Primitive_Procedure_Table,
	      Static_Primitive_Procedure_Table,
	      primitive_procedure_t,
	      primitive_procedure_t);
  prim_table_size = new_size;
  UPDATE_PRIMITIVE_TABLE_HOOK (0, MAX_PRIMITIVE);
}

static tree_node prim_procedure_tree = ((tree_node) NULL);

void
initialize_primitives (void)
{
  unsigned long counter;

  /* MAX_STATIC_PRIMITIVE is the index of the last primitive */

  MAX_PRIMITIVE = (MAX_STATIC_PRIMITIVE + 1);
  grow_primitive_tables ();

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
declare_primitive_internal (bool override_p,
       const char * name,
       primitive_procedure_t code,
       int nargs_lo,
       int nargs_hi,
       const char * docstr)
/* nargs_lo ignored, for now */
{
  unsigned long index;
  SCHEME_OBJECT primitive;
  const char * ndocstr = docstr;
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
      grow_primitive_tables ();

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
declare_primitive (const char * name,
		   primitive_procedure_t code,
		   int nargs_lo,
		   int nargs_hi,
		   const char * docstr)
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
install_primitive (const char * name,
		   primitive_procedure_t code,
		   int nargs_lo,
		   int nargs_hi,
		   const char * docstr)
{
  return (declare_primitive_internal (true, name, code,
				      nargs_lo, nargs_hi, docstr));
}

SCHEME_OBJECT
make_primitive (const char * name, int arity)
{
  tree_node prim;
  char * cname;
  SCHEME_OBJECT result;

  /* Make sure to copy the name if we will be keeping it.  */
  prim = (tree_lookup (prim_procedure_tree, name));
  if (prim != 0)
    cname = ((char *) (prim->name));
  else
    {
      cname = (OS_malloc ((strlen (name)) + 1));
      strcpy (cname, name);
    }
  result = (declare_primitive (cname, Prim_unimplemented, arity, arity, 0));
  return
    ((result == SHARP_F)
     ? SHARP_F
     : (OBJECT_NEW_TYPE (TC_PRIMITIVE, result)));
}

SCHEME_OBJECT
find_primitive (SCHEME_OBJECT sname, bool intern_p, bool allow_p, int arity)
{
  tree_node prim
    = (tree_lookup (prim_procedure_tree, (STRING_POINTER (sname))));
  if (prim != 0)
    {
      SCHEME_OBJECT primitive = (MAKE_PRIMITIVE_OBJECT (prim->value));

      if ((!allow_p) && (!IMPLEMENTED_PRIMITIVE_P (primitive)))
	return (SHARP_F);

      if ((arity == UNKNOWN_PRIMITIVE_ARITY)
	  || (arity == (PRIMITIVE_ARITY (primitive))))
	return (primitive);

      if ((PRIMITIVE_ARITY (primitive)) == UNKNOWN_PRIMITIVE_ARITY)
	{
	  /* We've just learned the arity of the primitive. */
	  (Primitive_Arity_Table[PRIMITIVE_NUMBER (primitive)]) = arity;
	  return (primitive);
	}

      /* Arity mismatch, notify the runtime system. */
      return (LONG_TO_FIXNUM (PRIMITIVE_ARITY (primitive)));
    }

  if (!intern_p)
    return (SHARP_F);

  {
    size_t n_bytes = ((STRING_LENGTH (sname)) + 1);
    char * cname = (OS_malloc (n_bytes));
    memcpy (cname, (STRING_POINTER (sname)), n_bytes);
    {
      SCHEME_OBJECT primitive
	= (declare_primitive (cname,
			      Prim_unimplemented,
			      ((arity < 0) ? 0 : arity),
			      arity,
			      0));
      if (primitive == SHARP_F)
	error_in_system_call (syserr_not_enough_space, syscall_malloc);
      return (primitive);
    }
  }
}

/* These are used by fasdump to renumber primitives on the way out.
   Only those primitives actually referenced by the object being
   dumped are described in the output.  The primitives being dumped
   are renumbered in the output to a contiguous range starting at 0.  */

prim_renumber_t *
make_prim_renumber (void)
{
  return (make_prim_renumber_1 (MAX_PRIMITIVE));
}

static prim_renumber_t *
make_prim_renumber_1 (unsigned long n_entries)
{
  prim_renumber_t * pr = (OS_malloc (sizeof (prim_renumber_t)));
  (pr->internal) = (OS_malloc (n_entries * (sizeof (unsigned long))));
  (pr->external) = (OS_malloc (n_entries * (sizeof (unsigned long))));
  (pr->next_code) = 0;
  {
    unsigned long i;
    for (i = 0; (i < n_entries); i += 1)
      {
	((pr->internal) [i]) = ULONG_MAX;
	((pr->external) [i]) = ULONG_MAX;
      }
  }
  transaction_record_action (tat_always, free_prim_renumber, pr);
  return (pr);
}

static void
free_prim_renumber (void * vpr)
{
  prim_renumber_t * pr = vpr;
  OS_free (pr->internal);
  OS_free (pr->external);
  OS_free (pr);
}

SCHEME_OBJECT
renumber_primitive (SCHEME_OBJECT primitive, prim_renumber_t * pr)
{
  unsigned long old = (OBJECT_DATUM (primitive));
  unsigned long new = ((pr->internal) [old]);
  if (new == ULONG_MAX)
    {
      new = ((pr->next_code)++);
      ((pr->internal) [old]) = new;
      ((pr->external) [new]) = old;
    }
  return (OBJECT_NEW_DATUM (primitive, new));
}

unsigned long
renumbered_primitives_export_length (prim_renumber_t * pr)
{
  unsigned long result = 0;
  unsigned long i;

  for (i = 0; (i < (pr->next_code)); i += 1)
    result += (table_entry_length ((pr->external) [i]));
  return (result);
}

void
export_renumbered_primitives (SCHEME_OBJECT * start, prim_renumber_t * pr)
{
  unsigned long i;
  for (i = 0; (i < (pr->next_code)); i += 1)
    start = (make_table_entry (((pr->external) [i]), start));
}

/* Like above, but export the whole table.  */

unsigned long
primitive_table_export_length (void)
{
  unsigned long result = 0;
  unsigned long i;

  for (i = 0; (i < MAX_PRIMITIVE); i += 1)
    result += (table_entry_length (i));
  return (result);
}

void
export_primitive_table (SCHEME_OBJECT * start)
{
  unsigned long i;
  for (i = 0; (i < MAX_PRIMITIVE); i += 1)
    start = (make_table_entry (i, start));
}

static SCHEME_OBJECT *
make_table_entry (unsigned long code, SCHEME_OBJECT * start)
{
  static const char * null_string = "\0";
  const char * source
    = (((Primitive_Name_Table[code]) == 0)
       ? null_string
       : (Primitive_Name_Table[code]));
  unsigned long n_chars = (strlen (source));
  unsigned long n_words = (STRING_LENGTH_TO_GC_LENGTH (n_chars));

  (*start++) = (LONG_TO_FIXNUM (Primitive_Arity_Table[code]));
  (*start++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, n_words));
  (*start) = (MAKE_OBJECT (0, n_chars));
  memcpy ((start + 1), source, (n_chars + 1));
  return (start + n_words);
}

static unsigned long
table_entry_length (unsigned long code)
{
  return
    ((STRING_LENGTH_TO_GC_LENGTH (((Primitive_Name_Table[code]) == 0)
				  ? 0
				  : (strlen (Primitive_Name_Table[code]))))
     + 2);
}

void
import_primitive_table (SCHEME_OBJECT * entries,
			unsigned long n_entries,
			SCHEME_OBJECT * primitives)
{
  unsigned long i;
  for (i = 0; (i < n_entries); i += 1)
    {
      long arity = (FIXNUM_TO_LONG (*entries++));
      SCHEME_OBJECT prim
	= (find_primitive
	   ((MAKE_POINTER_OBJECT (TC_CHARACTER_STRING, entries)),
	    true, true, arity));

      if (!PRIMITIVE_P (prim))
	signal_error_from_primitive (ERR_WRONG_ARITY_PRIMITIVES);

      (*primitives++) = prim;
      entries += (1 + (OBJECT_DATUM (*entries)));
    }
}
