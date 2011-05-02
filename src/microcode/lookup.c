/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Environment lookup, modification, and definition.  */

#include "scheme.h"
#include "trap.h"
#include "lookup.h"

/* Hopefully a conservative guesstimate. */
#ifndef SPACE_PER_UUO_LINK	/* So it can be overriden from config.h */
#  define SPACE_PER_UUO_LINK 10
#endif

/* Cache objects are 3-tuples.  */
#define SPACE_PER_CACHE 3

/* Each reference uses a pair and a weak pair, and potentially two
   more pairs if the reference introduces a new name.  */
#define SPACE_PER_REFERENCE 8

#define RETURN_IF_ERROR(expression)					\
{									\
  long RIE_result = (expression);					\
  if (RIE_result != PRIM_DONE)						\
    return (RIE_result);						\
}

#define DIE_IF_ERROR(expression)					\
{									\
  if ((expression) != PRIM_DONE)					\
    {									\
      outf_fatal ("\nRan out of guaranteed space!\n");			\
      Microcode_Termination (TERM_EXIT);				\
    }									\
}

#define GC_CHECK(n)							\
{									\
  if (GC_NEEDED_P (n))							\
    {									\
      REQUEST_GC (n);							\
      return (PRIM_INTERRUPT);						\
    }									\
}

#define MAP_TO_UNASSIGNED(value)					\
  (((value) == EXTERNAL_UNASSIGNED_OBJECT)				\
   ? UNASSIGNED_OBJECT							\
   : (value))

#define MAP_FROM_UNASSIGNED(value)					\
  (((value) == UNASSIGNED_OBJECT)					\
   ? EXTERNAL_UNASSIGNED_OBJECT						\
   : (value))

#define EXTERNAL_UNASSIGNED_OBJECT					\
  (VECTOR_REF (fixed_objects, NON_OBJECT))

#define PALIST_COND(palist_var) (PAIR_P (*palist_var))

#define PALIST_HEADER(palist_var, prefs_var)				\
  SCHEME_OBJECT * prefs_var = (PAIR_CDR_LOC (PAIR_CAR (*palist_var)));

#define PALIST_FOOTER(palist_var) do					\
{									\
  if (PAIR_P (PAIR_CDR (PAIR_CAR (*palist_var))))			\
    palist_var = (PAIR_CDR_LOC (*palist_var));				\
  else									\
    (*palist_var) = (PAIR_CDR (*palist_var));				\
} while (false)

#define PREFS_COND(prefs_var) (PAIR_P (*prefs_var))

#define PREFS_HEADER(prefs_var)						\
  PREFS_HEADER_1 (prefs_var, (PAIR_CAR (*prefs_var)))

#define PREFS_HEADER_1(prefs_var, cache)				\
{									\
  if ((GET_CACHE_REFERENCE_BLOCK (cache)) == SHARP_F)			\
    {									\
      (*prefs_var) = (PAIR_CDR (*prefs_var));				\
      continue;								\
    }									\
}

#define PREFS_FOOTER(prefs_var) do					\
{									\
  prefs_var = (PAIR_CDR_LOC (*prefs_var));				\
} while (false)

#define WALK_REFERENCES(refs_pointer, ref_var, body)			\
{									\
  SCHEME_OBJECT * WR_palist = (refs_pointer);				\
  while (PALIST_COND (WR_palist))					\
    {									\
      PALIST_HEADER (WR_palist, WR_prefs);				\
      while (PREFS_COND (WR_prefs))					\
	{								\
	  SCHEME_OBJECT ref_var = (PAIR_CAR (*WR_prefs));		\
	  PREFS_HEADER_1 (WR_prefs, ref_var);				\
	  body;								\
	  PREFS_FOOTER (WR_prefs);					\
	}								\
      PALIST_FOOTER (WR_palist);					\
    }									\
}

/***** Forward References *****/

static long lookup_variable_cache
  (SCHEME_OBJECT, SCHEME_OBJECT *);
static long assign_variable_end
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT *, int);
static long assign_variable_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *, int);
static long guarantee_extension_space
  (SCHEME_OBJECT);
static long allocate_frame_extension
  (unsigned long, SCHEME_OBJECT, SCHEME_OBJECT *);
static long unbind_cached_variable
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static void unbind_variable_1
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static unsigned long update_cache_refs_space
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
static unsigned long update_cache_refs_space_1
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, SCHEME_OBJECT);
static long update_cache_references
  (SCHEME_OBJECT, SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * find_binding_cell
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
static SCHEME_OBJECT * scan_frame
  (SCHEME_OBJECT, SCHEME_OBJECT, int);
static SCHEME_OBJECT * scan_procedure_bindings
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, int);
static unsigned long count_references
  (SCHEME_OBJECT *);
static void update_assignment_references
  (SCHEME_OBJECT);
static long guarantee_cache
  (SCHEME_OBJECT *);
static void update_clone
  (SCHEME_OBJECT);
static long make_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);

#ifdef CC_SUPPORT_P

static long update_uuo_links
  (SCHEME_OBJECT, SCHEME_OBJECT);
static void move_all_references
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned int);
static long add_cache_reference
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static void add_reference
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static void install_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static void install_operator_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static unsigned long ref_pairs_to_move
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static void delete_ref_pairs
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, SCHEME_OBJECT);
static void move_ref_pairs
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * new_alist_entry
  (SCHEME_OBJECT *, SCHEME_OBJECT);
static int move_ref_pair_p
  (SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * find_references_named
  (SCHEME_OBJECT *, SCHEME_OBJECT);
static long make_cache_reference
  (SCHEME_OBJECT, unsigned long, SCHEME_OBJECT *);

#endif

/***** Basic environment manipulation (lookup, assign, define).  *****/

long
lookup_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT * cell;
  SCHEME_OBJECT value;

  if (! ((ENVIRONMENT_P (environment)) && (SYMBOL_P (symbol))))
    return (ERR_BAD_FRAME);

  cell = (find_binding_cell (environment, symbol, 0));
  if (cell == 0)
    return (ERR_UNBOUND_VARIABLE);

  value = (*cell);
  switch (get_trap_kind (value))
    {
    case NON_TRAP_KIND:
      (*value_ret) = value;
      return (PRIM_DONE);

    case TRAP_UNASSIGNED:
      return (ERR_UNASSIGNED_VARIABLE);

    case TRAP_UNBOUND:
      return (ERR_UNBOUND_VARIABLE);

    case TRAP_MACRO:
      (*value_ret) = value;
      return (ERR_MACRO_BINDING);

    case TRAP_COMPILER_CACHED:
      return (lookup_variable_cache ((GET_TRAP_CACHE (value)), value_ret));

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
}

static long
lookup_variable_cache (SCHEME_OBJECT cache, SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT value = (GET_CACHE_VALUE (cache));
  switch (get_trap_kind (value))
    {
    case NON_TRAP_KIND:
      (*value_ret) = value;
      return (PRIM_DONE);

    case TRAP_UNASSIGNED:
      return (ERR_UNASSIGNED_VARIABLE);

    case TRAP_UNBOUND:
      return (ERR_UNBOUND_VARIABLE);

    case TRAP_MACRO:
      (*value_ret) = value;
      return (ERR_MACRO_BINDING);

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
}

long
safe_lookup_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		      SCHEME_OBJECT * value_ret)
{
  long result = (lookup_variable (environment, symbol, value_ret));
  if (result == ERR_UNASSIGNED_VARIABLE)
    {
      (*value_ret) = EXTERNAL_UNASSIGNED_OBJECT;
      return (PRIM_DONE);
    }
  return (result);
}

long
variable_unassigned_p (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		       SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT dummy_value;
  long result = (lookup_variable (environment, symbol, (&dummy_value)));
  switch (result)
    {
    case ERR_UNASSIGNED_VARIABLE:
      (*value_ret) = SHARP_T;
      return (PRIM_DONE);

    case PRIM_DONE:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    default:
      return (result);
    }
}

long
variable_unbound_p (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		    SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT dummy_value;
  long result = (lookup_variable (environment, symbol, (&dummy_value)));
  switch (result)
    {
    case ERR_UNBOUND_VARIABLE:
      (*value_ret) = SHARP_T;
      return (PRIM_DONE);

    case ERR_UNASSIGNED_VARIABLE:
    case ERR_MACRO_BINDING:
    case PRIM_DONE:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    default:
      return (result);
    }
}

long
variable_unreferenceable_p (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
			    SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT dummy_value;
  long result = (lookup_variable (environment, symbol, (&dummy_value)));
  switch (result)
    {
    case ERR_UNBOUND_VARIABLE:
    case ERR_UNASSIGNED_VARIABLE:
    case ERR_MACRO_BINDING:
      (*value_ret) = SHARP_T;
      return (PRIM_DONE);

    case PRIM_DONE:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    default:
      return (result);
    }
}

long
assign_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT value, SCHEME_OBJECT * value_ret)
{
  if (! ((ENVIRONMENT_P (environment)) || (SYMBOL_P (symbol))))
    return (ERR_BAD_FRAME);
  {
    SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, 0));
    if (cell == 0)
      return (ERR_UNBOUND_VARIABLE);
    return (assign_variable_end (cell, value, value_ret, 0));
  }
}

static long
assign_variable_end (SCHEME_OBJECT * cell, SCHEME_OBJECT value,
		     SCHEME_OBJECT * value_ret, int force_p)
{
  SCHEME_OBJECT old_value = (*cell);
  switch (get_trap_kind (old_value))
    {
    case NON_TRAP_KIND:
    case TRAP_UNASSIGNED:
      break;

    case TRAP_UNBOUND:
      if (force_p)
	break;
      return (ERR_UNBOUND_VARIABLE);

    case TRAP_MACRO:
      if (force_p)
	break;
      return (ERR_MACRO_BINDING);

    case TRAP_COMPILER_CACHED:
      return
	(assign_variable_cache
	 ((GET_TRAP_CACHE (old_value)), value, value_ret, force_p));

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
  (*value_ret) = (MAP_FROM_UNASSIGNED (old_value));
  (*cell) = (MAP_TO_UNASSIGNED (value));
  return (PRIM_DONE);
}

static long
assign_variable_cache (SCHEME_OBJECT cache, SCHEME_OBJECT value,
		       SCHEME_OBJECT * value_ret, int force_p)
{
  SCHEME_OBJECT old_value = (GET_CACHE_VALUE (cache));
  switch (get_trap_kind (old_value))
    {
    case NON_TRAP_KIND:
    case TRAP_UNASSIGNED:
      break;

    case TRAP_UNBOUND:
      if (force_p)
	break;
      return (ERR_UNBOUND_VARIABLE);

    case TRAP_MACRO:
      if (force_p)
	break;
      return (ERR_MACRO_BINDING);

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
  (*value_ret) = (MAP_FROM_UNASSIGNED (old_value));
  /* Perform the assignment.  If there are any operator references to
     this variable, update their links.  */
#ifdef CC_SUPPORT_P
  if (PAIR_P (* (GET_CACHE_OPERATOR_REFERENCES (cache))))
    return (update_uuo_links (cache, (MAP_TO_UNASSIGNED (value))));
#endif
  SET_CACHE_VALUE (cache, (MAP_TO_UNASSIGNED (value)));
  return (PRIM_DONE);
}

#ifdef CC_SUPPORT_P
static long
update_uuo_links (SCHEME_OBJECT cache, SCHEME_OBJECT new_value)
{
  GC_CHECK
    (((count_references (GET_CACHE_OPERATOR_REFERENCES (cache)))
      * SPACE_PER_UUO_LINK)
     + SPACE_PER_CACHE);
  SET_CACHE_VALUE (cache, new_value);
  update_clone (cache);
  WALK_REFERENCES
    ((GET_CACHE_OPERATOR_REFERENCES (cache)),
     reference,
     {
       install_operator_cache (cache,
			       (GET_CACHE_REFERENCE_BLOCK (reference)),
			       (GET_CACHE_REFERENCE_OFFSET (reference)));
     });
  return (PRIM_DONE);
}
#endif

long
define_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT value)
{
  if (! ((ENVIRONMENT_P (environment)) || (SYMBOL_P (symbol))))
    return (ERR_BAD_FRAME);

  /* If there is already a binding, just assign to it.  */
  {
    SCHEME_OBJECT * cell = (scan_frame (environment, symbol, 1));
    SCHEME_OBJECT old_value;
    if (cell != 0)
      {
	if (GLOBAL_FRAME_P (environment))
	  strengthen_symbol (symbol);
	return (assign_variable_end (cell, value, (&old_value), 1));
      }
  }

  /* At this point, we know that environment can't be the global
     environment, because scan_frame would have returned a non-null
     pointer for the global environment.  */

  RETURN_IF_ERROR (guarantee_extension_space (environment));

  /* If this binding shadows another binding, we'll have to recache
     any references to the other binding, because some of them might
     now refer to the new binding instead.  */
  {
    SCHEME_OBJECT * shadowed_cell
      = (find_binding_cell ((GET_FRAME_PARENT (environment)), symbol, 0));
    SCHEME_OBJECT old_cache
      = (((shadowed_cell != 0)
	  && ((get_trap_kind (*shadowed_cell)) == TRAP_COMPILER_CACHED))
	 ? (GET_TRAP_CACHE (*shadowed_cell))
	 : SHARP_F);
    unsigned long length = (GET_EXTENDED_FRAME_LENGTH (environment));
    SCHEME_OBJECT pair;

    /* Make sure there is enough space available to move any
       references that need moving.  */
    GC_CHECK
      (2
       + ((old_cache != SHARP_F)
	  ? (update_cache_refs_space (old_cache, environment, symbol))
	  : 0));

    /* Create the binding.  */
    pair = (cons (symbol, (MAP_TO_UNASSIGNED (value))));
    ((GET_EXTENDED_FRAME_BINDINGS (environment)) [length]) = pair;
    SET_EXTENDED_FRAME_LENGTH (environment, (length + 1));

    /* Move any references that need moving.  */
    return
      ((old_cache != SHARP_F)
       ? (update_cache_references
	  (old_cache, (PAIR_CDR_LOC (pair)), environment, symbol))
       : PRIM_DONE);
  }
}

static long
guarantee_extension_space (SCHEME_OBJECT environment)
{
  if (EXTENDED_FRAME_P (environment))
    /* Guarantee that there is room in the extension for a binding.  */
    {
      unsigned long length = (GET_EXTENDED_FRAME_LENGTH (environment));
      if (length == (GET_MAX_EXTENDED_FRAME_LENGTH (environment)))
	{
	  SCHEME_OBJECT extension;
	  RETURN_IF_ERROR
	    (allocate_frame_extension
	     ((2 * length),
	      (GET_EXTENDED_FRAME_PROCEDURE (environment)),
	      (&extension)));
	  memcpy ((GET_FRAME_EXTENSION_BINDINGS (extension)),
		  (GET_EXTENDED_FRAME_BINDINGS (environment)),
		  (length * (sizeof (SCHEME_OBJECT))));
	  SET_FRAME_EXTENSION_LENGTH (extension, length);
	  SET_FRAME_EXTENSION (environment, extension);
	}
    }
  else
    /* There's no extension, so create one. */
    {
      SCHEME_OBJECT extension;
      RETURN_IF_ERROR
	(allocate_frame_extension (16,
				   (GET_FRAME_PROCEDURE (environment)),
				   (&extension)));
      SET_FRAME_EXTENSION (environment, extension);
    }
  return (PRIM_DONE);
}

static long
allocate_frame_extension (unsigned long length, SCHEME_OBJECT procedure,
			  SCHEME_OBJECT * extension_ret)
{
  unsigned long n_words = (ENV_EXTENSION_MIN_SIZE + length);
  GC_CHECK (n_words);
  {
    SCHEME_OBJECT extension = (make_vector ((n_words - 1), SHARP_F, 0));
    SET_FRAME_EXTENSION_PARENT_FRAME
      (extension, (GET_PROCEDURE_ENVIRONMENT (procedure)));
    SET_FRAME_EXTENSION_PROCEDURE (extension, procedure);
    SET_FRAME_EXTENSION_LENGTH (extension, 0);
    (*extension_ret) = extension;
    return (PRIM_DONE);
  }
}

long
link_variables (SCHEME_OBJECT target_environment, SCHEME_OBJECT target_symbol,
		SCHEME_OBJECT source_environment, SCHEME_OBJECT source_symbol)
{
  SCHEME_OBJECT * source_cell;
  trap_kind_t source_kind;
  SCHEME_OBJECT * target_cell;

  if (! ((ENVIRONMENT_P (target_environment))
	 && (ENVIRONMENT_P (source_environment))
	 && (SYMBOL_P (target_symbol))
	 && (SYMBOL_P (source_symbol))))
    return (ERR_BAD_FRAME);

  source_cell = (find_binding_cell (source_environment, source_symbol, 0));
  if (source_cell == 0)
    return (ERR_UNBOUND_VARIABLE);

  source_kind = (get_trap_kind (*source_cell));
  if (source_kind == TRAP_UNBOUND)
    return (ERR_UNBOUND_VARIABLE);

  target_cell = (scan_frame (target_environment, target_symbol, 1));
  if (target_cell == source_cell)
    return (PRIM_DONE);

  if ((target_cell != 0) && (GLOBAL_FRAME_P (target_environment)))
    strengthen_symbol (target_symbol);

  if ((target_cell != 0)
      && ((get_trap_kind (*target_cell)) == TRAP_COMPILER_CACHED))
    {
      SCHEME_OBJECT target_cache = (GET_TRAP_CACHE (*target_cell));
      if (source_kind == TRAP_COMPILER_CACHED)
	{
	  SCHEME_OBJECT source_cache = (GET_TRAP_CACHE (*source_cell));
	  if (source_cache == target_cache)
	    /* Already linked.  */
	    return (PRIM_DONE);
	  GC_CHECK
	    (((count_references (GET_CACHE_OPERATOR_REFERENCES (target_cache)))
	      * SPACE_PER_UUO_LINK)
	     + (2 * SPACE_PER_CACHE));
	  SET_CACHE_VALUE (target_cache, (GET_CACHE_VALUE (source_cache)));
#ifdef CC_SUPPORT_P
	  move_all_references
	    (source_cache, target_cache, CACHE_REFERENCES_LOOKUP);
	  move_all_references
	    (source_cache, target_cache, CACHE_REFERENCES_ASSIGNMENT);
	  move_all_references
	    (source_cache, target_cache, CACHE_REFERENCES_OPERATOR);
#endif
	  update_clone (source_cache);
	  update_clone (target_cache);

	  /* Make sure both traps share the same cache: */
	  SET_TRAP_CACHE ((*source_cell), target_cache);
	}
      else
	SET_CACHE_VALUE (target_cache, (*source_cell));
      (*source_cell) = (*target_cell);
      return (PRIM_DONE);
    }

  RETURN_IF_ERROR (guarantee_cache (source_cell));
  return (define_variable (target_environment, target_symbol, (*source_cell)));
}

#ifdef CC_SUPPORT_P
static void
move_all_references (SCHEME_OBJECT from_cache, SCHEME_OBJECT to_cache,
		     unsigned int reference_kind)
{
  SCHEME_OBJECT * pfrom = (GET_CACHE_REFERENCES (from_cache, reference_kind));
  SCHEME_OBJECT * pto = (GET_CACHE_REFERENCES (to_cache, reference_kind));

  WALK_REFERENCES
    (pfrom,
     reference,
     {
       install_cache (to_cache,
		      (GET_CACHE_REFERENCE_BLOCK (reference)),
		      (GET_CACHE_REFERENCE_OFFSET (reference)),
		      reference_kind);
     });

  while (PAIR_P (*pto))
    pto = (PAIR_CDR_LOC (*pto));
  (*pto) = (*pfrom);
  (*pfrom) = EMPTY_LIST;
}
#endif

long
unbind_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT frame;
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, (&frame)));
  if (GLOBAL_FRAME_P (frame))
    weaken_symbol (symbol);
  switch ((cell == 0) ? TRAP_UNBOUND : (get_trap_kind (*cell)))
    {
    case TRAP_UNBOUND:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    case NON_TRAP_KIND:
    case TRAP_UNASSIGNED:
    case TRAP_MACRO:
      unbind_variable_1 (cell, frame, symbol);
      (*value_ret) = SHARP_T;
      return (PRIM_DONE);

    case TRAP_COMPILER_CACHED:
      {
	SCHEME_OBJECT cache = (GET_TRAP_CACHE (*cell));
	switch (get_trap_kind (GET_CACHE_VALUE (cache)))
	  {
	  case TRAP_UNBOUND:
	    (*value_ret) = SHARP_F;
	    return (PRIM_DONE);

	  case NON_TRAP_KIND:
	  case TRAP_UNASSIGNED:
	  case TRAP_MACRO:
	    if (PROCEDURE_FRAME_P (frame))
	      {
		RETURN_IF_ERROR
		  (unbind_cached_variable (cell, frame, symbol));
	      }
	    else
	      {
		SET_CACHE_VALUE (cache, UNBOUND_OBJECT);
	      }
	    (*value_ret) = SHARP_T;
	    return (PRIM_DONE);

	  default:
	    return (ERR_ILLEGAL_REFERENCE_TRAP);
	  }
      }

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
}

static long
unbind_cached_variable (SCHEME_OBJECT * cell, SCHEME_OBJECT frame,
			SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT cache = (GET_TRAP_CACHE (*cell));
  SCHEME_OBJECT * shadowed_cell
    = (find_binding_cell ((GET_FRAME_PARENT (frame)), symbol, 0));
  GC_CHECK (update_cache_refs_space (cache, frame, symbol));
  unbind_variable_1 (cell, frame, symbol);
  return (update_cache_references (cache, shadowed_cell, frame, symbol));
}

static void
unbind_variable_1 (SCHEME_OBJECT * cell,
		   SCHEME_OBJECT frame, SCHEME_OBJECT symbol)
{
  if ((PROCEDURE_FRAME_P (frame)) && (EXTENDED_FRAME_P (frame)))
    {
      SCHEME_OBJECT * start = (GET_EXTENDED_FRAME_BINDINGS (frame));
      unsigned long length = (GET_EXTENDED_FRAME_LENGTH (frame));
      unsigned long index = 0;
      while (index < length)
	{
	  if ((PAIR_CAR (start[index])) == symbol)
	    {
	      if (index < (length - 1))
		(start[index]) = (start [length - 1]);
	      SET_EXTENDED_FRAME_LENGTH (frame, (length - 1));
	      (start [length - 1]) = SHARP_F;
	      return;
	    }
	  index += 1;
	}
    }
  (*cell) = UNBOUND_OBJECT;
}

/***** Interface to compiled code.  *****/

#ifdef CC_SUPPORT_P

long
compiler_cache_lookup (SCHEME_OBJECT name, SCHEME_OBJECT block,
		       unsigned long offset)
{
  return
    (add_cache_reference ((cc_block_environment (block)),
			  name, block, offset,
			  CACHE_REFERENCES_LOOKUP));
}

long
compiler_cache_assignment (SCHEME_OBJECT name, SCHEME_OBJECT block,
			   unsigned long offset)
{
  return
    (add_cache_reference ((cc_block_environment (block)),
			  name, block, offset,
			  CACHE_REFERENCES_ASSIGNMENT));
}

long
compiler_cache_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
			 unsigned long offset)
{
  return
    (add_cache_reference ((cc_block_environment (block)),
			  name, block, offset,
			  CACHE_REFERENCES_OPERATOR));
}

long
compiler_cache_global_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
				unsigned long offset)
{
  return
    (add_cache_reference (THE_GLOBAL_ENV,
			  name, block, offset,
			  CACHE_REFERENCES_OPERATOR));
}

SCHEME_OBJECT
compiler_var_error (SCHEME_OBJECT cache, SCHEME_OBJECT block,
		    unsigned int reference_kind)
{
  WALK_REFERENCES
    ((GET_CACHE_REFERENCES (cache, reference_kind)),
     reference,
     {
       /* If this reference is in the right block, return the symbol
	  being referenced.  */
       if ((GET_CACHE_REFERENCE_BLOCK (reference)) == block)
	 return (PAIR_CAR (PAIR_CAR (*WR_palist)));
     });
  return (SHARP_F);
}

long
compiler_lookup_trap (SCHEME_OBJECT cache, SCHEME_OBJECT * value_ret)
{
  return (lookup_variable_cache (cache, value_ret));
}

long
compiler_safe_lookup_trap (SCHEME_OBJECT cache, SCHEME_OBJECT * value_ret)
{
  long result = (lookup_variable_cache (cache, value_ret));
  if (result == ERR_UNASSIGNED_VARIABLE)
    {
      (*value_ret) = EXTERNAL_UNASSIGNED_OBJECT;
      return (PRIM_DONE);
    }
  return (result);
}

long
compiler_unassigned_p_trap (SCHEME_OBJECT cache, SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT dummy_value;
  long result = (lookup_variable_cache (cache, (&dummy_value)));
  switch (result)
    {
    case ERR_UNASSIGNED_VARIABLE:
      (*value_ret) = SHARP_T;
      return (PRIM_DONE);

    case PRIM_DONE:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    default:
      return (result);
    }
}

long
compiler_assignment_trap (SCHEME_OBJECT cache, SCHEME_OBJECT value,
			  SCHEME_OBJECT * value_ret)
{
  return
    (assign_variable_cache
     ((((GET_CACHE_VALUE (cache)) == EXPENSIVE_OBJECT)
       /* The cache is a clone.  Get the real cache object.  */
       ? (GET_CACHE_CLONE (cache))
       : cache),
      value,
      value_ret,
      0));
}

long
compiler_operator_reference_trap (SCHEME_OBJECT cache,
				  SCHEME_OBJECT * value_ret)
{
  return (lookup_variable_cache (cache, value_ret));
}

/***** Variable-reference cache mechanism.  *****/

/* add_cache_reference adds a reference to a variable's cache,
   creating the cache if necessary.  It takes the following arguments:

   + environment and symbol specify the affected variable.

   + block is a compiled-code block, and offset is an offset into
     block.  Together, these specify the location where the variable
     cache is to be stored.

   + reference_kind specifies the kind of reference that is being cached.

   add_cache_reference creates a variable cache for the specified variable,
   if needed, and stores it in the location specified by (block,
   offset).  It adds the (block,offset) reference to the appropriate
   reference list for subsequent updating.

   If the reference is a lookup reference, the cache is directly
   stored in the block.

   If the reference is an assignment reference, and there are no
   operator references to this variable, the cache is directly stored
   in the block.

   If the reference is an assignment reference, and there _are_
   operator references to this variable, a "clone" cache is stored in
   the block.  The "clone" cache has a value of EXPENSIVE_OBJECT,
   which causes any assignment to this cell to trap out to the
   microcode, where the expensive process of updating all the related
   operator references can be performed.

   If the reference is an operator reference, a "UUO" link is stored
   in the block.  If the variable's value is a compiled procedure, the
   UUO link is a direct reference to the procedure.  In all other
   cases it is a dummy procedure that redirects as needed.  If there
   are assignment references to this variable but no "clone" cache,
   one is created and all the assignment references updated to point
   to it.  */

static long
add_cache_reference (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		     SCHEME_OBJECT block, unsigned long offset,
		     unsigned int reference_kind)
{
  SCHEME_OBJECT frame = 0;
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, (&frame)));
  SCHEME_OBJECT dummy_cell = UNBOUND_OBJECT;
  if (cell == 0)
    /* There's no binding for the variable, and we don't have access
       to the global environment.  The compiled code needs a cache, so
       we'll install one, but it won't be attached to any environment
       structure.  */
    cell = (&dummy_cell);
  else if (GLOBAL_FRAME_P (frame))
    strengthen_symbol (symbol);
  /* This procedure must complete to keep the data structures
     consistent, so we do a GC check in advance to guarantee that all
     of the allocations will finish.  */
  GC_CHECK ((2 * SPACE_PER_CACHE) + SPACE_PER_REFERENCE + SPACE_PER_UUO_LINK);
  DIE_IF_ERROR (guarantee_cache (cell));
  {
    SCHEME_OBJECT cache = (GET_TRAP_CACHE (*cell));
    add_reference
      ((GET_CACHE_REFERENCES (cache, reference_kind)), symbol, block, offset);
    update_clone (cache);
    install_cache (cache, block, offset, reference_kind);
  }
  return (PRIM_DONE);
}

/* Add a new cached reference to the cached reference list pointed at
   by slot.  Attempt to reuse pairs which have been "emptied" by the
   garbage collector.  */

static void
add_reference (SCHEME_OBJECT * palist,
	       SCHEME_OBJECT symbol, SCHEME_OBJECT block, unsigned long offset)
{
  SCHEME_OBJECT * prefs = (find_references_named (palist, symbol));
  if (prefs != 0)
    {
      while (PREFS_COND (prefs))
	{
	  if ((GET_CACHE_REFERENCE_BLOCK (PAIR_CAR (*prefs))) == SHARP_F)
	    {
	      /* Reuse this pair.  */
	      SET_CACHE_REFERENCE_BLOCK ((PAIR_CAR (*prefs)), block);
	      SET_CACHE_REFERENCE_OFFSET ((PAIR_CAR (*prefs)), offset);
	      return;
	    }
	  PREFS_FOOTER (prefs);
	}
      {
	SCHEME_OBJECT reference;
	DIE_IF_ERROR (make_cache_reference (block, offset, (&reference)));
	(*prefs) = (cons (reference, EMPTY_LIST));
      }
      return;
    }
  {
    SCHEME_OBJECT reference;
    DIE_IF_ERROR (make_cache_reference (block, offset, (&reference)));
    SCHEME_OBJECT alist = (*palist);
    (*palist) = (cons ((cons (symbol, (cons (reference, EMPTY_LIST)))), alist));
  }
}

static void
install_cache (SCHEME_OBJECT cache, SCHEME_OBJECT block, unsigned long offset,
	       unsigned int reference_kind)
{
  switch (reference_kind)
    {
    case CACHE_REFERENCES_LOOKUP:
      write_variable_cache (cache, block, offset);
      break;

    case CACHE_REFERENCES_ASSIGNMENT:
      write_variable_cache
	((((GET_CACHE_CLONE (cache)) != SHARP_F)
	  ? (GET_CACHE_CLONE (cache))
	  : cache),
	 block,
	 offset);
      break;

    case CACHE_REFERENCES_OPERATOR:
      install_operator_cache (cache, block, offset);
      break;

    default:
      abort ();
      break;
    }
}

static void
install_operator_cache (SCHEME_OBJECT cache,
			SCHEME_OBJECT block, unsigned long offset)
{
  SCHEME_OBJECT value = (GET_CACHE_VALUE (cache));
  DIE_IF_ERROR (make_uuo_link (value, cache, block, offset));
}

#endif /* CC_SUPPORT_P */

static unsigned long
update_cache_refs_space (SCHEME_OBJECT from_cache, SCHEME_OBJECT environment,
			 SCHEME_OBJECT symbol)
{
#ifdef CC_SUPPORT_P
  return
    ((update_cache_refs_space_1
      (from_cache, CACHE_REFERENCES_LOOKUP, environment, symbol))
     + (update_cache_refs_space_1
	(from_cache, CACHE_REFERENCES_ASSIGNMENT, environment, symbol))
     + (update_cache_refs_space_1
	(from_cache, CACHE_REFERENCES_OPERATOR, environment, symbol)));
#else
  return (0);
#endif
}

/* Generate a conservative estimate of the space needed to move some
   cache refs from one cache to another.  */

static unsigned long
update_cache_refs_space_1 (SCHEME_OBJECT from_cache, unsigned int kind,
			   SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * from_palist = (GET_CACHE_REFERENCES (from_cache, kind));
  unsigned long n_refs = (ref_pairs_to_move (from_palist, environment, symbol));
  unsigned long result = 0;
  if (n_refs > 0)
    {
      /* Space for new cache and new alist entry, if needed.  */
      result += (SPACE_PER_CACHE + 4);
      if (kind == CACHE_REFERENCES_OPERATOR)
	/* space for new trampolines, if needed.  */
	result += (n_refs * SPACE_PER_UUO_LINK);
    }
  return (result);
}

static long
update_cache_references (SCHEME_OBJECT from_cache, SCHEME_OBJECT * to_cell,
			 SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  if (to_cell != 0)
    {
      DIE_IF_ERROR (guarantee_cache (to_cell));
      {
	SCHEME_OBJECT to_cache = (GET_TRAP_CACHE (*to_cell));
#ifdef CC_SUPPORT_P
	move_ref_pairs
	  (from_cache, to_cache, CACHE_REFERENCES_LOOKUP,
	   environment, symbol);
	move_ref_pairs
	  (from_cache, to_cache, CACHE_REFERENCES_ASSIGNMENT,
	   environment, symbol);
	move_ref_pairs
	  (from_cache, to_cache, CACHE_REFERENCES_OPERATOR,
	   environment, symbol);
#endif
	update_clone (to_cache);
      }
    }
#ifdef CC_SUPPORT_P
  else
    {
      delete_ref_pairs
	(from_cache, CACHE_REFERENCES_LOOKUP, environment, symbol);
      delete_ref_pairs
	(from_cache, CACHE_REFERENCES_ASSIGNMENT, environment, symbol);
      delete_ref_pairs
	(from_cache, CACHE_REFERENCES_OPERATOR, environment, symbol);
    }
#endif
  update_clone (from_cache);
  return (PRIM_DONE);
}

#ifdef CC_SUPPORT_P

static unsigned long
ref_pairs_to_move (SCHEME_OBJECT * palist, SCHEME_OBJECT environment,
		   SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * prefs = (find_references_named (palist, symbol));
  unsigned long n_refs = 0;
  if (prefs != 0)
    while (PREFS_COND (prefs))
      {
	PREFS_HEADER (prefs);
	if (move_ref_pair_p ((*prefs), environment))
	  n_refs += 1;
	PREFS_FOOTER (prefs);
      }
  return (n_refs);
}

static void
delete_ref_pairs (SCHEME_OBJECT from_cache, unsigned int kind,
		  SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * from_palist = (GET_CACHE_REFERENCES (from_cache, kind));
  SCHEME_OBJECT * from_prefs = (find_references_named (from_palist, symbol));
  if (from_prefs != 0)
    while (PREFS_COND (from_prefs))
      {
	PREFS_HEADER (from_prefs);
	if (move_ref_pair_p ((*from_prefs), environment))
	  {
	    (*from_prefs) = (PAIR_CDR (*from_prefs));
	    continue;
	  }
	PREFS_FOOTER (from_prefs);
      }
}

static void
move_ref_pairs (SCHEME_OBJECT from_cache, SCHEME_OBJECT to_cache,
		unsigned int reference_kind, SCHEME_OBJECT environment,
		SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * from_palist
    = (GET_CACHE_REFERENCES (from_cache, reference_kind));
  SCHEME_OBJECT * to_palist
    = (GET_CACHE_REFERENCES (to_cache, reference_kind));
  SCHEME_OBJECT * from_prefs = (find_references_named (from_palist, symbol));
  SCHEME_OBJECT * to_prefs = (find_references_named (to_palist, symbol));
  if (from_prefs != 0)
    while (PREFS_COND (from_prefs))
      {
	PREFS_HEADER (from_prefs);
	if (move_ref_pair_p ((*from_prefs), environment))
	  {
	    SCHEME_OBJECT p = (*from_prefs);
	    (*from_prefs) = (PAIR_CDR (p));
	    if (to_prefs == 0)
	      to_prefs = (new_alist_entry (to_palist, symbol));
	    SET_PAIR_CDR (p, (*to_prefs));
	    (*to_prefs) = p;
	    install_cache (to_cache,
			   (GET_CACHE_REFERENCE_BLOCK (PAIR_CAR (p))),
			   (GET_CACHE_REFERENCE_OFFSET (PAIR_CAR (p))),
			   reference_kind);
	    continue;
	  }
	PREFS_FOOTER (from_prefs);
      }
}

static SCHEME_OBJECT *
new_alist_entry (SCHEME_OBJECT * to_palist, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT entry = (cons (symbol, EMPTY_LIST));
  SCHEME_OBJECT head = (*to_palist);
  (*to_palist) = (cons (entry, head));
  return (PAIR_CDR_LOC (entry));
}

static int
move_ref_pair_p (SCHEME_OBJECT ref_pair, SCHEME_OBJECT ancestor)
{
  SCHEME_OBJECT descendant
    = (cc_block_environment
       (GET_CACHE_REFERENCE_BLOCK (PAIR_CAR (ref_pair))));
  while (PROCEDURE_FRAME_P (descendant))
    {
      if (descendant == ancestor)
	return (1);
      descendant = (GET_FRAME_PARENT (descendant));
    }
  return (descendant == ancestor);
}

#endif /* CC_SUPPORT_P */

/***** Utilities *****/

static SCHEME_OBJECT *
find_binding_cell (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		   SCHEME_OBJECT * frame_ret)
{
  SCHEME_OBJECT frame = environment;
  while (1)
    {
      SCHEME_OBJECT * cell = (scan_frame (frame, symbol, 0));
      if ((cell != 0) || (!PROCEDURE_FRAME_P (frame)))
	{
	  if (frame_ret != 0)
	    (*frame_ret) = frame;
	  return (cell);
	}
      frame = (GET_FRAME_PARENT (frame));
    }
}

static SCHEME_OBJECT *
scan_frame (SCHEME_OBJECT frame, SCHEME_OBJECT symbol, int find_unbound_p)
{
  if (PROCEDURE_FRAME_P (frame))
    {
      if (EXTENDED_FRAME_P (frame))
	{
	  /* Search for a binding in the extension. */
	  SCHEME_OBJECT * scan = (GET_EXTENDED_FRAME_BINDINGS (frame));
	  SCHEME_OBJECT * end = (scan + (GET_EXTENDED_FRAME_LENGTH (frame)));
	  while (scan < end)
	    {
	      if ((PAIR_CAR (*scan)) == symbol)
		return (PAIR_CDR_LOC (*scan));
	      scan += 1;
	    }
	  return
	    (scan_procedure_bindings ((GET_EXTENDED_FRAME_PROCEDURE (frame)),
				      frame, symbol, find_unbound_p));
	}
      return
	(scan_procedure_bindings ((GET_FRAME_PROCEDURE (frame)),
				  frame, symbol, find_unbound_p));
    }
  else if (GLOBAL_FRAME_P (frame))
    return (SYMBOL_GLOBAL_VALUE_CELL (symbol));
  else
    return (0);
}

static SCHEME_OBJECT *
scan_procedure_bindings (SCHEME_OBJECT procedure, SCHEME_OBJECT frame,
			 SCHEME_OBJECT symbol, int find_unbound_p)
{
  SCHEME_OBJECT lambda = (GET_PROCEDURE_LAMBDA (procedure));
  SCHEME_OBJECT * start = (GET_LAMBDA_PARAMETERS (lambda));
  SCHEME_OBJECT * scan = start;
  SCHEME_OBJECT * end = (scan + (GET_LAMBDA_N_PARAMETERS (lambda)));
  while (scan < end)
    {
      if ((*scan) == symbol)
	{
	  SCHEME_OBJECT * cell = (GET_FRAME_ARG_CELL (frame, (scan - start)));
	  if (find_unbound_p || ((*cell) != UNBOUND_OBJECT))
	    return (cell);
	}
      scan += 1;
    }
  return (0);
}

trap_kind_t
get_trap_kind (SCHEME_OBJECT object)
{
  if (REFERENCE_TRAP_P (object))
    {
      unsigned long datum = (OBJECT_DATUM (object));
      return
	((datum <= TRAP_MAX_IMMEDIATE)
	 ? datum
	 : (OBJECT_DATUM (GET_TRAP_TAG (object))));
    }
  else
    return (NON_TRAP_KIND);
}

static unsigned long
count_references (SCHEME_OBJECT * palist)
{
  unsigned long n_references = 0;
  WALK_REFERENCES (palist, reference, { n_references += 1; });
  return (n_references);
}

#ifdef CC_SUPPORT_P
static SCHEME_OBJECT *
find_references_named (SCHEME_OBJECT * palist, SCHEME_OBJECT symbol)
{
  while (PAIR_P (*palist))
    {
      if ((PAIR_CAR (PAIR_CAR (*palist))) == symbol)
	return (PAIR_CDR_LOC (PAIR_CAR (*palist)));
      palist = (PAIR_CDR_LOC (*palist));
    }
  return (0);
}
#endif

static void
update_assignment_references (SCHEME_OBJECT cache)
{
#ifdef CC_SUPPORT_P
  SCHEME_OBJECT reference_cache
    = (((GET_CACHE_CLONE (cache)) != SHARP_F)
       ? (GET_CACHE_CLONE (cache))
       : cache);
  WALK_REFERENCES
    ((GET_CACHE_ASSIGNMENT_REFERENCES (cache)),
     reference,
     {
       write_variable_cache
	 (reference_cache,
	  (GET_CACHE_REFERENCE_BLOCK (reference)),
	  (GET_CACHE_REFERENCE_OFFSET (reference)));
     });
#endif
}

static long
guarantee_cache (SCHEME_OBJECT * cell)
{
  SCHEME_OBJECT references;
  SCHEME_OBJECT cache;

  if ((get_trap_kind (*cell)) == TRAP_COMPILER_CACHED)
    return (PRIM_DONE);

  GC_CHECK (3);
  references = (MAKE_POINTER_OBJECT (CACHE_REFERENCES_TYPE, Free));
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;

  RETURN_IF_ERROR (make_cache ((*cell), SHARP_F, references, (&cache)));

  GC_CHECK (2);
  (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (TRAP_COMPILER_CACHED));
  (*Free++) = cache;
  (*cell) = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, (Free - 2)));
  return (PRIM_DONE);
}

static void
update_clone (SCHEME_OBJECT cache)
{
  if ((PAIR_P (* (GET_CACHE_ASSIGNMENT_REFERENCES (cache))))
      && (PAIR_P (* (GET_CACHE_OPERATOR_REFERENCES (cache)))))
    {
      if ((GET_CACHE_CLONE (cache)) == SHARP_F)
	{
	  SCHEME_OBJECT clone;
	  DIE_IF_ERROR
	    (make_cache (EXPENSIVE_OBJECT,
			 cache,
			 (GET_CACHE_REFERENCES_OBJECT (cache)),
			 (&clone)));
	  SET_CACHE_CLONE (cache, clone);
	  update_assignment_references (cache);
	}
    }
  else
    {
      if ((GET_CACHE_CLONE (cache)) != SHARP_F)
	{
	  SET_CACHE_CLONE (cache, SHARP_F);
	  update_assignment_references (cache);
	}
    }
}

static long
make_cache (SCHEME_OBJECT value, SCHEME_OBJECT clone, SCHEME_OBJECT references,
	    SCHEME_OBJECT * cache_ret)
{
  GC_CHECK (3);
  (*Free++) = value;
  (*Free++) = clone;
  (*Free++) = references;
  (*cache_ret) = (MAKE_POINTER_OBJECT (CACHE_TYPE, (Free - 3)));
  return (PRIM_DONE);
}

#ifdef CC_SUPPORT_P
static long
make_cache_reference (SCHEME_OBJECT block, unsigned long offset,
		      SCHEME_OBJECT * ref_ret)
{
  GC_CHECK (2);
  (*Free++) = block;
  (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (offset));
  (*ref_ret) = (MAKE_POINTER_OBJECT (TC_WEAK_CONS, (Free - 2)));
  return (PRIM_DONE);
}
#endif
