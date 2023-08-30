/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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
  (vector_ref (fixed_objects, NON_OBJECT))

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

#define PREFS_HEADER_1(prefs_var, cache)                                \
{                                                                       \
  if (cache_ref_block (cache) == GC_RECLAIMED)                          \
    {                                                                   \
      *prefs_var = PAIR_CDR (*prefs_var);                               \
      continue;                                                         \
    }                                                                   \
}

#define PREFS_FOOTER(prefs_var) do					\
{									\
  prefs_var = (PAIR_CDR_LOC (*prefs_var));				\
} while (false)

#define WALK_REFERENCES(refs_pointer, ref_var, body)			\
{									\
  SCHEME_OBJECT* WR_palist = (refs_pointer);				\
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
static SCHEME_OBJECT * extend_environment
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
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
  (SCHEME_OBJECT, SCHEME_OBJECT, bool);
static SCHEME_OBJECT * scan_procedure_bindings
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, bool);
static unsigned long count_references
  (SCHEME_OBJECT *);
static void update_assignment_references
  (SCHEME_OBJECT);
static long guarantee_cache
  (SCHEME_OBJECT *);
static void update_clone
  (SCHEME_OBJECT);
static long make_clone
  (SCHEME_OBJECT, SCHEME_OBJECT*);

#ifdef CC_SUPPORT_P

static long update_uuo_links
  (SCHEME_OBJECT, SCHEME_OBJECT);
static void move_all_references
  (SCHEME_OBJECT, SCHEME_OBJECT, enum cache_ref_kind);
static long add_cache_reference
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long,
   enum cache_ref_kind);
static void add_reference
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static void install_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, enum cache_ref_kind);
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
      return (lookup_variable_cache (ptr_ref_trap_cache (value), value_ret));

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
}

static long
lookup_variable_cache (SCHEME_OBJECT cache, SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT value = cache_value (cache);
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
	 (ptr_ref_trap_cache (old_value), value, value_ret, force_p));

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
  SCHEME_OBJECT old_value = cache_value (cache);
  switch (get_trap_kind (old_value))
    {
    case NON_TRAP_KIND:
    case TRAP_UNASSIGNED:
      break;

    case TRAP_UNBOUND:
      if (!force_p)
        return ERR_UNBOUND_VARIABLE;
      break;

    case TRAP_MACRO:
      if (!force_p)
        return ERR_MACRO_BINDING;
      break;

    default:
      return ERR_ILLEGAL_REFERENCE_TRAP;
    }
  *value_ret = MAP_FROM_UNASSIGNED (old_value);
  /* Perform the assignment.  If there are any operator references to
     this variable, update their links.  */
#ifdef CC_SUPPORT_P
  if (PAIR_P (*cache_operator_refs (cache)))
    return (update_uuo_links (cache, MAP_TO_UNASSIGNED (value)));
#endif
  set_cache_value (cache, MAP_TO_UNASSIGNED (value));
  return PRIM_DONE;
}

#ifdef CC_SUPPORT_P
static long
update_uuo_links (SCHEME_OBJECT cache, SCHEME_OBJECT new_value)
{
  GC_CHECK
    ((count_references (cache_operator_refs (cache))
      * SPACE_PER_UUO_LINK)
     + CACHE_SIZE);
  set_cache_value (cache, new_value);
  update_clone (cache);
  WALK_REFERENCES
    (cache_operator_refs (cache),
     reference,
     {
       install_operator_cache (cache,
			       cache_ref_block (reference),
			       cache_ref_offset (reference));
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
    SCHEME_OBJECT * cell = scan_frame (environment, symbol, true);
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
      = (find_binding_cell (env_parent (environment), symbol, 0));
    SCHEME_OBJECT old_cache
      = (((shadowed_cell != 0)
	  && ((get_trap_kind (*shadowed_cell)) == TRAP_COMPILER_CACHED))
	 ? ptr_ref_trap_cache (*shadowed_cell)
	 : SHARP_F);

    /* Make sure there is enough space available to move any
       references that need moving.  */
    GC_CHECK
      (2
       + ((old_cache != SHARP_F)
	  ? (update_cache_refs_space (old_cache, environment, symbol))
	  : 0));

    /* Create the binding.  */
    SCHEME_OBJECT * cell = (extend_environment (environment, symbol, value));

    /* Move any references that need moving.  */
    return
      ((old_cache != SHARP_F)
       ? (update_cache_references (old_cache, cell, environment, symbol))
       : PRIM_DONE);
  }
}

static SCHEME_OBJECT *
extend_environment (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		    SCHEME_OBJECT value)
{
  SCHEME_OBJECT pair = (cons (symbol, (MAP_TO_UNASSIGNED (value))));
  unsigned long length = (extended_frame_length (environment));
  extended_frame_bindings (environment) [length] = pair;
  set_extended_frame_length (environment, length + 1);
  return pair_cdr_loc (pair);
}

static long
guarantee_extension_space (SCHEME_OBJECT environment)
{
  if (extended_frame_p (environment))
    /* Guarantee that there is room in the extension for a binding.  */
    {
      unsigned long length = extended_frame_length (environment);
      if (length == extended_frame_max_length (environment))
	{
	  SCHEME_OBJECT extension;
	  RETURN_IF_ERROR
	    (allocate_frame_extension
	     (2 * length,
	      extended_frame_proc (environment),
	      &extension));
	  memcpy (frame_extension_bindings (extension),
		  extended_frame_bindings (environment),
		  length * sizeof (SCHEME_OBJECT));
	  set_frame_extension_length (extension, length);
	  set_env_extension (environment, extension);
	}
    }
  else
    /* There's no extension, so create one. */
    {
      SCHEME_OBJECT extension;
      RETURN_IF_ERROR
	(allocate_frame_extension (16,
				   env_proc (environment),
				   &extension));
      set_env_extension (environment, extension);
    }
  return (PRIM_DONE);
}

static long
allocate_frame_extension (unsigned long length, SCHEME_OBJECT procedure,
			  SCHEME_OBJECT * extension_ret)
{
  unsigned long n_words = FRAME_EXTENSION_MIN_SIZE + length;
  GC_CHECK (n_words);
  SCHEME_OBJECT extension = make_vector (n_words - 1, SHARP_F, false);
  set_frame_extension_parent (extension, proc_environment (procedure));
  set_frame_extension_proc (extension, procedure);
  set_frame_extension_length (extension, 0);
  *extension_ret = extension;
  return PRIM_DONE;
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

  target_cell = scan_frame (target_environment, target_symbol, true);
  if (target_cell == source_cell)
    return (PRIM_DONE);

  if ((target_cell != 0) && (GLOBAL_FRAME_P (target_environment)))
    strengthen_symbol (target_symbol);

  if ((target_cell != 0)
      && ((get_trap_kind (*target_cell)) == TRAP_COMPILER_CACHED))
    {
      SCHEME_OBJECT target_cache = (ptr_ref_trap_cache (*target_cell));
      if (source_kind == TRAP_COMPILER_CACHED)
	{
	  SCHEME_OBJECT source_cache = (ptr_ref_trap_cache (*source_cell));
	  if (source_cache == target_cache)
	    /* Already linked.  */
	    return (PRIM_DONE);
	  GC_CHECK
	    ((count_references (cache_operator_refs (target_cache))
	      * SPACE_PER_UUO_LINK)
	     + (2 * CACHE_SIZE));
	  set_cache_value (target_cache, (cache_value (source_cache)));
#ifdef CC_SUPPORT_P
	  move_all_references (source_cache, target_cache, LOOKUP_CACHE);
	  move_all_references (source_cache, target_cache, ASSIGNMENT_CACHE);
	  move_all_references (source_cache, target_cache, OPERATOR_CACHE);
#endif
	  update_clone (source_cache);
	  update_clone (target_cache);

	  /* Make sure both traps share the same cache: */
	  set_ptr_ref_trap_cache (*source_cell, target_cache);
	}
      else
	set_cache_value (target_cache, *source_cell);
      (*source_cell) = (*target_cell);
      return (PRIM_DONE);
    }

  RETURN_IF_ERROR (guarantee_cache (source_cell));
  return (define_variable (target_environment, target_symbol, (*source_cell)));
}

#ifdef CC_SUPPORT_P
static void
move_all_references (SCHEME_OBJECT from_cache, SCHEME_OBJECT to_cache,
		     enum cache_ref_kind kind)
{
  SCHEME_OBJECT* pfrom = cache_kind_refs (from_cache, kind);
  SCHEME_OBJECT* pto = cache_kind_refs (to_cache, kind);

  WALK_REFERENCES
    (pfrom,
     reference,
     {
       install_cache (to_cache,
		      (cache_ref_block (reference)),
		      (cache_ref_offset (reference)),
		      kind);
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
  SCHEME_OBJECT frame = 0;
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
	SCHEME_OBJECT cache = (ptr_ref_trap_cache (*cell));
	switch (get_trap_kind (cache_value (cache)))
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
		set_cache_value (cache, UNBOUND_OBJECT);
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
  SCHEME_OBJECT cache = (ptr_ref_trap_cache (*cell));
  SCHEME_OBJECT * shadowed_cell
    = (find_binding_cell (env_parent (frame), symbol, 0));
  GC_CHECK (update_cache_refs_space (cache, frame, symbol));
  unbind_variable_1 (cell, frame, symbol);
  return (update_cache_references (cache, shadowed_cell, frame, symbol));
}

static void
unbind_variable_1 (SCHEME_OBJECT * cell,
		   SCHEME_OBJECT frame, SCHEME_OBJECT symbol)
{
  if (PROCEDURE_FRAME_P (frame) && extended_frame_p (frame))
    {
      SCHEME_OBJECT * start = extended_frame_bindings (frame);
      unsigned long length = extended_frame_length (frame);
      unsigned long index = 0;
      while (index < length)
	{
	  if ((PAIR_CAR (start[index])) == symbol)
	    {
	      if (index < (length - 1))
		(start[index]) = (start [length - 1]);
	      set_extended_frame_length (frame, length - 1);
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
			  LOOKUP_CACHE));
}

long
compiler_cache_assignment (SCHEME_OBJECT name, SCHEME_OBJECT block,
			   unsigned long offset)
{
  return
    (add_cache_reference ((cc_block_environment (block)),
			  name, block, offset,
			  ASSIGNMENT_CACHE));
}

long
compiler_cache_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
			 unsigned long offset)
{
  return
    (add_cache_reference ((cc_block_environment (block)),
			  name, block, offset,
			  OPERATOR_CACHE));
}

long
compiler_cache_global_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
				unsigned long offset)
{
  return
    (add_cache_reference (THE_GLOBAL_ENV,
			  name, block, offset,
			  OPERATOR_CACHE));
}

SCHEME_OBJECT
compiler_var_error (SCHEME_OBJECT cache, SCHEME_OBJECT block,
		    enum cache_ref_kind kind)
{
  WALK_REFERENCES
    (cache_kind_refs (cache, kind),
     reference,
     {
       /* If this reference is in the right block, return the symbol
	  being referenced.  */
       if (cache_ref_block (reference) == block)
	 return PAIR_CAR (PAIR_CAR (*WR_palist));
     });
  return SHARP_F;
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
  return assign_variable_cache
           (cache_clone_p (cache) ? cache_clone (cache) : cache,
            value,
            value_ret,
            0);
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

   + kind specifies the kind of reference that is being cached.

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
		     enum cache_ref_kind kind)
{
  SCHEME_OBJECT frame = 0;
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, &frame));
  if (cell == 0)
    {
      /* There's no binding for the variable, and we don't have access
	 to the global environment.  The compiled code needs a cache, so
	 we'll install one that's attached to the outermost frame.  */
      DIE_IF_ERROR (guarantee_extension_space (frame));
      cell = (extend_environment (frame, symbol, UNBOUND_OBJECT));
    }
  else if (GLOBAL_FRAME_P (frame))
    strengthen_symbol (symbol);
  /* This procedure must complete to keep the data structures
     consistent, so we do a GC check in advance to guarantee that all
     of the allocations will finish.  */
  GC_CHECK ((2 * CACHE_SIZE) + SPACE_PER_REFERENCE + SPACE_PER_UUO_LINK);
  DIE_IF_ERROR (guarantee_cache (cell));
  {
    SCHEME_OBJECT cache = (ptr_ref_trap_cache (*cell));
    add_reference (cache_kind_refs (cache, kind), symbol, block, offset);
    update_clone (cache);
    install_cache (cache, block, offset, kind);
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
	  if (cache_ref_block (pair_car (*prefs)) == GC_RECLAIMED)
	    {
	      /* Reuse this pair.  */
	      set_cache_ref_block (pair_car (*prefs), block);
	      set_cache_ref_offset (pair_car (*prefs), offset);
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
    SCHEME_OBJECT alist;
    DIE_IF_ERROR (make_cache_reference (block, offset, (&reference)));
    alist = (*palist);
    (*palist) = (cons ((cons (symbol, (cons (reference, EMPTY_LIST)))), alist));
  }
}

static void
install_cache (SCHEME_OBJECT cache, SCHEME_OBJECT block, unsigned long offset,
	       enum cache_ref_kind kind)
{
  switch (kind)
    {
    case LOOKUP_CACHE:
      write_variable_cache (cache, block, offset);
      break;

    case ASSIGNMENT_CACHE:
      write_variable_cache
	((cache_clone (cache) != SHARP_F)
	 ? cache_clone (cache)
	 : cache,
	 block,
	 offset);
      break;

    case OPERATOR_CACHE:
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
  SCHEME_OBJECT value = cache_value (cache);
  DIE_IF_ERROR (make_uuo_link (value, cache, block, offset));
}

#endif /* CC_SUPPORT_P */

static unsigned long
update_cache_refs_space (SCHEME_OBJECT from_cache, SCHEME_OBJECT environment,
			 SCHEME_OBJECT symbol)
{
#ifdef CC_SUPPORT_P
  return
    update_cache_refs_space_1 (from_cache, LOOKUP_CACHE, environment, symbol)
    + update_cache_refs_space_1
	(from_cache, ASSIGNMENT_CACHE, environment, symbol)
    + update_cache_refs_space_1
	(from_cache, OPERATOR_CACHE, environment, symbol);
#else
  return 0;
#endif
}

/* Generate a conservative estimate of the space needed to move some
   cache refs from one cache to another.  */

static unsigned long
update_cache_refs_space_1 (SCHEME_OBJECT from_cache, enum cache_ref_kind kind,
			   SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * from_palist = cache_kind_refs (from_cache, kind);
  unsigned long n_refs = (ref_pairs_to_move (from_palist, environment, symbol));
  unsigned long result = 0;
  if (n_refs > 0)
    {
      /* Space for new cache and new alist entry, if needed.  */
      result += (CACHE_SIZE + 4);
      if (kind == OPERATOR_CACHE)
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
	SCHEME_OBJECT to_cache = (ptr_ref_trap_cache (*to_cell));
#ifdef CC_SUPPORT_P
	move_ref_pairs
	  (from_cache, to_cache, LOOKUP_CACHE, environment, symbol);
	move_ref_pairs
	  (from_cache, to_cache, ASSIGNMENT_CACHE, environment, symbol);
	move_ref_pairs
	  (from_cache, to_cache, OPERATOR_CACHE, environment, symbol);
#endif
	update_clone (to_cache);
      }
    }
#ifdef CC_SUPPORT_P
  else
    {
      delete_ref_pairs (from_cache, LOOKUP_CACHE, environment, symbol);
      delete_ref_pairs (from_cache, ASSIGNMENT_CACHE, environment, symbol);
      delete_ref_pairs (from_cache, OPERATOR_CACHE, environment, symbol);
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
  SCHEME_OBJECT * from_palist = cache_kind_refs (from_cache, kind);
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
		enum cache_ref_kind kind, SCHEME_OBJECT environment,
		SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * from_palist = cache_kind_refs (from_cache, kind);
  SCHEME_OBJECT * to_palist = cache_kind_refs (to_cache, kind);
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
			   cache_ref_block (pair_car (p)),
			   cache_ref_offset (pair_car (p)),
			   kind);
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
       (cache_ref_block (pair_car (ref_pair))));
  while (PROCEDURE_FRAME_P (descendant))
    {
      if (descendant == ancestor)
	return (1);
      descendant = env_parent (descendant);
    }
  return (descendant == ancestor);
}

#endif /* CC_SUPPORT_P */

/***** Utilities *****/

static SCHEME_OBJECT *
find_binding_cell (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		   SCHEME_OBJECT * frame_ret)
{
  if (NULL_FRAME_P (environment))
    {
      if (frame_ret != 0)
	(*frame_ret) = environment;
      return (0);
    }
  assert (ENVIRONMENT_P (environment));
  SCHEME_OBJECT frame = environment;
  while (1)
    {
      SCHEME_OBJECT* cell = scan_frame (frame, symbol, false);
      if ((cell != 0)
	  /* This is safe because if 'frame' was the global frame then
	     'cell' would be non-null.  Therefore 'frame' must be a
	     procedure frame.  */
	  || (!ENVIRONMENT_P (env_parent (frame))))
	{
	  if (frame_ret != 0)
	    (*frame_ret) = frame;
	  return (cell);
	}
      frame = env_parent (frame);
    }
}

static SCHEME_OBJECT *
scan_frame (SCHEME_OBJECT frame, SCHEME_OBJECT symbol, bool find_unbound_p)
{
  if (PROCEDURE_FRAME_P (frame))
    {
      if (extended_frame_p (frame))
	{
	  /* Search for a binding in the extension. */
	  SCHEME_OBJECT* scan = extended_frame_bindings (frame);
	  SCHEME_OBJECT* end = scan + extended_frame_length (frame);
	  while (scan < end)
	    {
	      if (pair_car (*scan) == symbol)
		return pair_cdr_loc (*scan);
	      scan += 1;
	    }
	  return
	    scan_procedure_bindings
              (extended_frame_proc (frame), frame, symbol, find_unbound_p);
	}
      return
	scan_procedure_bindings
          (env_proc (frame), frame, symbol, find_unbound_p);
    }
  assert (GLOBAL_FRAME_P (frame));
  return symbol_global_value_cell (symbol);
}

static SCHEME_OBJECT*
scan_procedure_bindings (SCHEME_OBJECT procedure, SCHEME_OBJECT frame,
			 SCHEME_OBJECT name, bool find_unbound_p)
{
  SCHEME_OBJECT lambda = proc_lambda (procedure);
  SCHEME_OBJECT* start = lambda_params (lambda);
  SCHEME_OBJECT* end = start + lambda_n_params (lambda);
  SCHEME_OBJECT* scan = start;
  while (scan < end)
    {
      if (*scan == name)
	{
	  SCHEME_OBJECT* cell = env_vals (frame) + (scan - start);
	  if (find_unbound_p || (*cell != UNBOUND_OBJECT))
	    return cell;
	}
      scan += 1;
    }
  return 0;
}

trap_kind_t
get_trap_kind (SCHEME_OBJECT object)
{
  if (!REFERENCE_TRAP_P (object))
    return NON_TRAP_KIND;
  unsigned long datum = object_datum (object);
  return (datum <= TRAP_MAX_IMMEDIATE)
	 ? datum
	 : object_datum (ptr_ref_trap_tag (object));
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
    = (cache_clone (cache) != SHARP_F)
      ? cache_clone (cache)
      : cache;
  WALK_REFERENCES
    (cache_assignment_refs (cache),
     reference,
     {
       write_variable_cache
	 (reference_cache,
	  cache_ref_block (reference),
	  cache_ref_offset (reference));
     });
#endif
}

static long
guarantee_cache (SCHEME_OBJECT* cell)
{
  if (get_trap_kind (*cell) == TRAP_COMPILER_CACHED)
    return PRIM_DONE;

  GC_CHECK (PTR_REF_TRAP_SIZE + CACHE_SIZE + CACHE_REFS_SIZE);
  *cell
    = make_ptr_ref_trap (TRAP_COMPILER_CACHED,
                         make_cache (*cell, SHARP_F, make_cache_refs ()));
  return PRIM_DONE;
}

static void
update_clone (SCHEME_OBJECT cache)
{
  if (PAIR_P (*cache_assignment_refs (cache))
      && PAIR_P (*cache_operator_refs (cache)))
    {
      if (cache_clone (cache) == SHARP_F)
	{
	  SCHEME_OBJECT clone;
	  DIE_IF_ERROR (make_clone (cache, &clone));
	  set_cache_clone (cache, clone);
	  update_assignment_references (cache);
	}
    }
  else
    {
      if (cache_clone (cache) != SHARP_F)
	{
	  set_cache_clone (cache, SHARP_F);
	  update_assignment_references (cache);
	}
    }
}

static long
make_clone (SCHEME_OBJECT cache, SCHEME_OBJECT* cache_ret)
{
  GC_CHECK (CACHE_SIZE);
  *cache_ret = make_cache_clone (cache);
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
  (*ref_ret) = (make_pointer_object (TC_WEAK_CONS, (Free - 2)));
  return (PRIM_DONE);
}
#endif
