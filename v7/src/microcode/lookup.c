/* -*-C-*-

$Id: lookup.c,v 9.60 2001/08/01 02:17:08 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* Environment lookup, modification, and definition.  */

#include "scheme.h"
#include "trap.h"
#include "lookup.h"

extern long make_uuo_link
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern long make_fake_uuo_link
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern SCHEME_OBJECT extract_uuo_link
  (SCHEME_OBJECT, unsigned long);

extern SCHEME_OBJECT extract_variable_cache
  (SCHEME_OBJECT, unsigned long);
extern void store_variable_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);

extern SCHEME_OBJECT compiled_block_environment
  (SCHEME_OBJECT);

/* Hopefully a conservative guesstimate. */
#ifndef SPACE_PER_UUO_LINK	/* So it can be overriden from config.h */
#  define SPACE_PER_UUO_LINK 10
#endif

/* Cache objects are 4-tuples.  */
#define SPACE_PER_CACHE 4

/* Each reference uses a pair and a weak pair.  */
#define SPACE_PER_REFERENCE 4

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
  if (GC_Check (n))							\
    {									\
      Request_GC (n);							\
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

#define EXTERNAL_UNASSIGNED_OBJECT (Get_Fixed_Obj_Slot (Non_Object))

/***** Forward References *****/

static long lookup_variable_cache
  (SCHEME_OBJECT, SCHEME_OBJECT *);
static long assign_variable_end
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT *, int);
static long assign_variable_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *, int);
static long allocate_frame_extension
  (unsigned long, SCHEME_OBJECT, SCHEME_OBJECT *);
static long merge_caches
  (SCHEME_OBJECT, SCHEME_OBJECT);
static long handle_cache_reference
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static long add_cache_reference
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static long add_reference
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, unsigned long);
static long install_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static long install_operator_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static long update_cache_references
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static unsigned long split_cache_references
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, SCHEME_OBJECT **);
static int environment_ancestor_or_self_p
  (SCHEME_OBJECT, SCHEME_OBJECT);
static void move_cache_references
  (SCHEME_OBJECT, SCHEME_OBJECT **, unsigned int);
static long update_uuo_links
  (SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * find_binding_cell
  (SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * scan_frame
  (SCHEME_OBJECT, SCHEME_OBJECT);
static SCHEME_OBJECT * scan_procedure_bindings
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
static unsigned long count_references
  (SCHEME_OBJECT, unsigned int);
static SCHEME_OBJECT * find_tail_holder
  (SCHEME_OBJECT, unsigned int);
static void update_assignment_references
  (SCHEME_OBJECT);
static long guarantee_cache
  (SCHEME_OBJECT *, SCHEME_OBJECT);
static long guarantee_clone
  (SCHEME_OBJECT);
static void flush_clone
  (SCHEME_OBJECT);
static long make_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT,
   SCHEME_OBJECT *);
static long make_cache_references
  (SCHEME_OBJECT *);
static long make_cache_reference
  (SCHEME_OBJECT, unsigned long, SCHEME_OBJECT *);

/***** Basic environment manipulation (lookup, assign, define).  *****/

long
lookup_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT * cell;
  SCHEME_OBJECT value;

  if (!ENVIRONMENT_P (environment))
    return (ERR_BAD_FRAME);

  cell
    = (find_binding_cell (environment,
			  (((OBJECT_TYPE (symbol)) == TC_VARIABLE)
			   ? (GET_VARIABLE_SYMBOL (symbol))
			   : symbol)));
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
  if (!ENVIRONMENT_P (environment))
    return (ERR_BAD_FRAME);
  {
    SCHEME_OBJECT * cell
      = (find_binding_cell (environment,
			    (((OBJECT_TYPE (symbol)) == TC_VARIABLE)
			     ? (GET_VARIABLE_SYMBOL (symbol))
			     : symbol)));
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

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
  (*value_ret) = (MAP_FROM_UNASSIGNED (old_value));
  /* Perform the assignment.  If there are any operator references to
     this variable, update their links.  */
  if (PAIR_P (GET_CACHE_REFERENCES_OPERATOR (GET_CACHE_REFERENCES (cache))))
    return (update_uuo_links (cache, (MAP_TO_UNASSIGNED (value))));
  SET_CACHE_VALUE (cache, (MAP_TO_UNASSIGNED (value)));
  return (PRIM_DONE);
}

long
define_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT value)
{
  if (!ENVIRONMENT_P (environment))
    return (ERR_BAD_FRAME);

  /* If there is already a binding, just assign to it.  */
  {
    SCHEME_OBJECT * cell = (scan_frame (environment, symbol));
    SCHEME_OBJECT old_value;
    if (cell != 0)
      return (assign_variable_end (cell, value, (&old_value), 1));
  }

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

  /* Create the binding.  */
  GC_CHECK (2);
  {
    SCHEME_OBJECT pair = (cons (symbol, value));
    unsigned long length = (GET_EXTENDED_FRAME_LENGTH (environment));
    ((GET_EXTENDED_FRAME_BINDINGS (environment)) [length]) = pair;
    SET_EXTENDED_FRAME_LENGTH (environment, (length + 1));

    /* If this binding shadows another binding, we'll have to
       recache any references to the other binding, because some of
       them might now refer to the new binding instead.  */
    return
      (update_cache_references ((PAIR_CDR_LOC (pair)), environment, symbol));
  }
}

static long
allocate_frame_extension (unsigned long length, SCHEME_OBJECT procedure,
			  SCHEME_OBJECT * extension_ret)
{
  unsigned long n_words = (ENV_EXTENSION_MIN_SIZE + length);
  GC_CHECK (n_words);
  {
    SCHEME_OBJECT extension = (make_vector ((n_words - 1), SHARP_F, 0));
    MEMORY_SET (extension, ENV_EXTENSION_PARENT_FRAME,
		(GET_PROCEDURE_ENVIRONMENT (procedure)));
    MEMORY_SET (extension, ENV_EXTENSION_PROCEDURE, procedure);
    MEMORY_SET (extension, ENV_EXTENSION_COUNT, FIXNUM_ZERO);
    (*extension_ret) = extension;
    return (PRIM_DONE);
  }
}

long
link_variable (SCHEME_OBJECT target, SCHEME_OBJECT source,
	       SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * source_cell;
  trap_kind_t source_kind;
  SCHEME_OBJECT * target_cell;
  
  if (! ((ENVIRONMENT_P (target)) && (ENVIRONMENT_P (source))))
    return (ERR_BAD_FRAME);

  source_cell = (find_binding_cell (source, symbol));
  if (source_cell == 0)
    return (ERR_UNBOUND_VARIABLE);

  source_kind = (get_trap_kind (*source_cell));
  if (source_kind == TRAP_UNBOUND)
    return (ERR_UNBOUND_VARIABLE);

  target_cell = (scan_frame (target, symbol));
  if ((target_cell != 0)
      && ((get_trap_kind (*target_cell)) == TRAP_COMPILER_CACHED))
    {
      SCHEME_OBJECT target_cache = (GET_TRAP_CACHE (*target_cell));
      if (source_kind == TRAP_COMPILER_CACHED)
	{
	  SCHEME_OBJECT source_cache = (GET_TRAP_CACHE (*source_cell));
	  RETURN_IF_ERROR (merge_caches (target_cache, source_cache));
	  SET_CACHE_VALUE (target_cache, (GET_CACHE_VALUE (source_cache)));
	}
      else
	SET_CACHE_VALUE (target_cache, (*source_cell));
      (*source_cell) = (*target_cell);
      return (PRIM_DONE);
    }

  RETURN_IF_ERROR (guarantee_cache (source_cell, symbol));
  return (define_variable (target, symbol, (*source_cell)));
}

static long
merge_caches (SCHEME_OBJECT target_cache, SCHEME_OBJECT source_cache)
{
  SCHEME_OBJECT target_references = (GET_CACHE_REFERENCES (target_cache));
  SCHEME_OBJECT source_references = (GET_CACHE_REFERENCES (source_cache));
  SCHEME_OBJECT * tail_holders [3];

  if (((PAIR_P (GET_CACHE_REFERENCES_ASSIGNMENT (target_references)))
       || (PAIR_P (GET_CACHE_REFERENCES_ASSIGNMENT (source_references))))
      && ((PAIR_P (GET_CACHE_REFERENCES_OPERATOR (target_references)))
	  || (PAIR_P (GET_CACHE_REFERENCES_OPERATOR (source_references)))))
    {
      RETURN_IF_ERROR (guarantee_clone (target_cache));
    }
  else
    flush_clone (target_cache);

  GC_CHECK
    ((count_references (source_cache, CACHE_REFERENCES_OPERATOR))
     * SPACE_PER_UUO_LINK);

  (tail_holders[CACHE_REFERENCES_LOOKUP])
    = (MEMORY_LOC (source_references, CACHE_REFERENCES_LOOKUP));
  (tail_holders[CACHE_REFERENCES_ASSIGNMENT])
    = (MEMORY_LOC (source_references, CACHE_REFERENCES_ASSIGNMENT));
  (tail_holders[CACHE_REFERENCES_OPERATOR])
    = (MEMORY_LOC (source_references, CACHE_REFERENCES_OPERATOR));

  move_cache_references
    (target_cache, tail_holders, CACHE_REFERENCES_LOOKUP);
  move_cache_references
    (target_cache, tail_holders, CACHE_REFERENCES_ASSIGNMENT);
  move_cache_references
    (target_cache, tail_holders, CACHE_REFERENCES_OPERATOR);

  return (PRIM_DONE);
}

/***** Interface to compiled code.  *****/

long
compiler_cache_lookup (SCHEME_OBJECT name, SCHEME_OBJECT block,
		       unsigned long offset)
{
  return
    (handle_cache_reference ((compiled_block_environment (block)),
			     name, block, offset,
			     CACHE_REFERENCES_LOOKUP));
}

long
compiler_cache_assignment (SCHEME_OBJECT name, SCHEME_OBJECT block,
			   unsigned long offset)
{
  return
    (handle_cache_reference ((compiled_block_environment (block)),
			     name, block, offset,
			     CACHE_REFERENCES_ASSIGNMENT));
}

long
compiler_cache_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
			 unsigned long offset)
{
  return
    (handle_cache_reference ((compiled_block_environment (block)),
			     name, block, offset,
			     CACHE_REFERENCES_OPERATOR));
}

long
compiler_cache_global_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
				unsigned long offset)
{
  return
    (handle_cache_reference (THE_GLOBAL_ENV,
			     name, block, offset,
			     CACHE_REFERENCES_OPERATOR));
}

static long
handle_cache_reference (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
			SCHEME_OBJECT block, unsigned long offset,
			unsigned int reference_kind)
{
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol));
  return
    ((cell == 0)
     ? ERR_UNBOUND_VARIABLE
     : (add_cache_reference (cell, symbol, block, offset, reference_kind)));
}

SCHEME_OBJECT
compiler_var_error (SCHEME_OBJECT cache)
{
  return (GET_CACHE_NAME (cache));
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

   + cell is a variable's value cell.

   + symbol is the variable's name.

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
add_cache_reference (SCHEME_OBJECT * cell,
		     SCHEME_OBJECT symbol,
		     SCHEME_OBJECT block,
		     unsigned long offset,
		     unsigned int reference_kind)
{
  /* This procedure must complete to keep the data structures
     consistent, so we do a GC check in advance to guarantee that all
     of the allocations will finish.  */
  GC_CHECK ((2 * SPACE_PER_CACHE) + SPACE_PER_REFERENCE + SPACE_PER_UUO_LINK);
  RETURN_IF_ERROR (guarantee_cache (cell, symbol));
  {
    SCHEME_OBJECT cache = (GET_TRAP_CACHE (*cell));
    SCHEME_OBJECT references = (GET_CACHE_REFERENCES (cache));
    RETURN_IF_ERROR
      (add_reference (references, reference_kind, block, offset));
    if ((PAIR_P (GET_CACHE_REFERENCES_ASSIGNMENT (references)))
	&& (PAIR_P (GET_CACHE_REFERENCES_OPERATOR (references))))
      RETURN_IF_ERROR (guarantee_clone (cache));
    return (install_cache (cache, block, offset, reference_kind));
  }
}

/* Add a new cached reference to the cached reference list pointed at
   by slot.  Attempt to reuse pairs which have been "emptied" by the
   garbage collector.  */

static long
add_reference (SCHEME_OBJECT references, unsigned int reference_kind,
	       SCHEME_OBJECT block, unsigned long offset)
{
  SCHEME_OBJECT * slot = (MEMORY_LOC (references, reference_kind));
  while (PAIR_P (*slot))
    {
      SCHEME_OBJECT reference = (PAIR_CAR (*slot));
      if ((GET_CACHE_REFERENCE_BLOCK (reference)) == SHARP_F)
	{
	  SET_CACHE_REFERENCE_BLOCK (reference, block);
	  SET_CACHE_REFERENCE_OFFSET (reference, offset);
	  return (PRIM_DONE);
	}
      slot = (PAIR_CDR_LOC (*slot));
    }
  {
    SCHEME_OBJECT reference;
    RETURN_IF_ERROR (make_cache_reference (block, offset, (&reference)));
    GC_CHECK (2);
    (*slot) = (cons (reference, EMPTY_LIST));
  }
  return (PRIM_DONE);
}

static long
install_cache (SCHEME_OBJECT cache,
	       SCHEME_OBJECT block, unsigned long offset,
	       unsigned int reference_kind)
{
  switch (reference_kind)
    {
    case CACHE_REFERENCES_LOOKUP:
      store_variable_cache (cache, block, offset);
      return (PRIM_DONE);

    case CACHE_REFERENCES_ASSIGNMENT:
      store_variable_cache
	((((GET_CACHE_CLONE (cache)) != SHARP_F)
	  ? (GET_CACHE_CLONE (cache))
	  : cache),
	 block,
	 offset);
      return (PRIM_DONE);

    case CACHE_REFERENCES_OPERATOR:
      return (install_operator_cache (cache, block, offset));

    default:
      abort ();
      return (0);
    }
}

static long
install_operator_cache (SCHEME_OBJECT cache,
			SCHEME_OBJECT block, unsigned long offset)
{
  SCHEME_OBJECT value = (GET_CACHE_VALUE (cache));
  return
    ((REFERENCE_TRAP_P (value))
     ? (make_fake_uuo_link (cache, block, offset))
     : (make_uuo_link (value, cache, block, offset)));
}

/* update_cache_references is invoked when a new binding is created.
   It recaches (at the definition point) all the references that need
   to point to the new cell.  It does this in two phases:

   First, split_cache_references is called to split all references
   into those that need to be updated and those that do not.  This is
   done by modifying the references list so that all those that need
   updating are at the end, so that when we subsequently proceed, we
   can just clip the list and install the tail in the new location.
   split_cache_references also counts how many entries are affected,
   so the total amount of space needed can be computed.

   Second, after checking that there is enough space to proceed, the
   references are moved to their new locations. */

static long
update_cache_references (SCHEME_OBJECT * new_cell,
			 SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * shadowed_cell;
  SCHEME_OBJECT * tail_holders [3];
  SCHEME_OBJECT new_cache;

  if (!PROCEDURE_FRAME_P (environment))
    return (PRIM_DONE);

  shadowed_cell
    = (find_binding_cell ((GET_FRAME_PARENT (environment)), symbol));
  if (! ((shadowed_cell != 0)
	 && ((get_trap_kind (*shadowed_cell)) == TRAP_COMPILER_CACHED)))
    return (PRIM_DONE);

  RETURN_IF_ERROR (guarantee_cache (new_cell, symbol));
  new_cache = (GET_TRAP_CACHE (*new_cell));

  /* Split the references lists.  */
  {
    SCHEME_OBJECT shadowed_cache = (GET_TRAP_CACHE (*shadowed_cell));
    unsigned long n_lookups
      = (split_cache_references
	 (shadowed_cache, CACHE_REFERENCES_LOOKUP, environment,
	  tail_holders));
    unsigned long n_assignments
      = (split_cache_references
	 (shadowed_cache, CACHE_REFERENCES_ASSIGNMENT, environment,
	  tail_holders));
    unsigned long n_operators
      = (split_cache_references
	 (shadowed_cache, CACHE_REFERENCES_OPERATOR, environment,
	  tail_holders));

    /* Return if there are no references that need to be updated.  */
    if ((n_lookups == 0) && (n_assignments == 0) && (n_operators == 0))
      return (PRIM_DONE);

    /* Make sure the cache has a clone if one will be needed.  */
    if ((n_assignments > 0) && (n_operators > 0))
      RETURN_IF_ERROR (guarantee_clone (new_cache));

    /* Next step must be atomic.  In order to guarantee this, we need
       enough space to allocate all of the UUO links.  */
    GC_CHECK (n_operators * SPACE_PER_UUO_LINK);
  }

  /* Move all the references.  */
  move_cache_references (new_cache, tail_holders, CACHE_REFERENCES_LOOKUP);
  move_cache_references (new_cache, tail_holders, CACHE_REFERENCES_ASSIGNMENT);
  move_cache_references (new_cache, tail_holders, CACHE_REFERENCES_OPERATOR);

  return (PRIM_DONE);
}

static unsigned long
split_cache_references (SCHEME_OBJECT cache,
			unsigned int reference_kind,
			SCHEME_OBJECT environment,
			SCHEME_OBJECT ** tail_holders)
{
  SCHEME_OBJECT * holder
    = (MEMORY_LOC ((GET_CACHE_REFERENCES (cache)), reference_kind));
  SCHEME_OBJECT references_to_move = EMPTY_LIST;
  unsigned long count = 0;
  while (PAIR_P (*holder))
    {
      SCHEME_OBJECT p = (*holder);
      SCHEME_OBJECT reference = (PAIR_CAR (p));
      SCHEME_OBJECT block = (GET_CACHE_REFERENCE_BLOCK (reference));
      if (block == SHARP_F)
	(*holder) = (PAIR_CDR (p));
      else if (environment_ancestor_or_self_p
	       (environment, (compiled_block_environment (block))))
	{
	  (*holder) = (PAIR_CDR (p));
	  SET_PAIR_CDR (p, references_to_move);
	  references_to_move = p;
	  count += 1;
	}
      else
	holder = (PAIR_CDR_LOC (p));
    }
  (*holder) = references_to_move;
  (tail_holders[reference_kind]) = holder;
  return (count);
}

static int
environment_ancestor_or_self_p (SCHEME_OBJECT ancestor,
				SCHEME_OBJECT descendant)
{
  while (PROCEDURE_FRAME_P (descendant))
    {
      if (descendant == ancestor)
	return (1);
      descendant = (GET_FRAME_PARENT (descendant));
    }
  return (descendant == ancestor);
}

static void
move_cache_references (SCHEME_OBJECT cache, SCHEME_OBJECT ** tail_holders,
		       unsigned int reference_kind)
{
  SCHEME_OBJECT tail = (* (tail_holders[reference_kind]));
  (* (tail_holders[reference_kind])) = EMPTY_LIST;
  (* (find_tail_holder ((GET_CACHE_REFERENCES (cache)), reference_kind)))
    = tail;
  while (PAIR_P (tail))
    {
      DIE_IF_ERROR
	(install_cache (cache,
			(GET_CACHE_REFERENCE_BLOCK (PAIR_CAR (tail))),
			(GET_CACHE_REFERENCE_OFFSET (PAIR_CAR (tail))),
			reference_kind));
      tail = (PAIR_CDR (tail));
    }
}

/* update_uuo_links is invoked when an assignment occurs to a
   variable which has cached operator references (uuo links).
   All the operator references must be recached to the new value.

   It currently potentially creates a new uuo link per operator
   reference.  This may be very expensive in space, but allows a great
   deal of flexibility.  It is ultimately necessary if there is hidden
   information on each call (like arity, types of arguments, etc.).  */

static long
update_uuo_links (SCHEME_OBJECT cache, SCHEME_OBJECT new_value)
{
  {
    unsigned long n_operators
      = (count_references (cache, CACHE_REFERENCES_OPERATOR));
    if (n_operators == 0)
      {
	/* We no longer need a cache clone, so if there is one, delete
	   it and change any assignment references to refer to the
	   cache itself.  */
	flush_clone (cache);
      }
    GC_CHECK (n_operators * SPACE_PER_UUO_LINK);
  }
  SET_CACHE_VALUE (cache, new_value);
  {
    SCHEME_OBJECT operators
      = (GET_CACHE_REFERENCES_OPERATOR (GET_CACHE_REFERENCES (cache)));
    while (PAIR_P (operators))
      {
	SCHEME_OBJECT reference = (PAIR_CAR (operators));
	SCHEME_OBJECT block = (GET_CACHE_REFERENCE_BLOCK (reference));
	unsigned long offset = (GET_CACHE_REFERENCE_OFFSET (reference));
	DIE_IF_ERROR (install_operator_cache (cache, block, offset));
	operators = (PAIR_CDR (operators));
      }
  }
  return (PRIM_DONE);
}

/***** Utilities *****/

static SCHEME_OBJECT *
find_binding_cell (SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT frame = environment;
  while (1)
    {
      SCHEME_OBJECT * cell = (scan_frame (frame, symbol));
      if ((cell != 0) || (!PROCEDURE_FRAME_P (frame)))
	return (cell);
      frame = (GET_FRAME_PARENT (frame));
    }
}

static SCHEME_OBJECT *
scan_frame (SCHEME_OBJECT frame, SCHEME_OBJECT symbol)
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
	    (scan_procedure_bindings
	     ((GET_EXTENDED_FRAME_PROCEDURE (frame)), frame, symbol));
	}
      return
	(scan_procedure_bindings
	 ((GET_FRAME_PROCEDURE (frame)), frame, symbol));
    }
  else if (GLOBAL_FRAME_P (frame))
    return (SYMBOL_GLOBAL_VALUE_CELL (symbol));
  else
    return (0);
}

static SCHEME_OBJECT *
scan_procedure_bindings (SCHEME_OBJECT procedure, SCHEME_OBJECT frame,
			 SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT lambda = (GET_PROCEDURE_LAMBDA (procedure));
  SCHEME_OBJECT * start = (GET_LAMBDA_PARAMETERS (lambda));
  SCHEME_OBJECT * scan = start;
  SCHEME_OBJECT * end = (scan + (GET_LAMBDA_N_PARAMETERS (lambda)));
  while (scan < end)
    {
      if ((*scan) == symbol)
	return (GET_FRAME_ARG_CELL (frame, (scan - start)));
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
count_references (SCHEME_OBJECT cache, unsigned int references_kind)
{
  SCHEME_OBJECT * holder
    = (MEMORY_LOC ((GET_CACHE_REFERENCES (cache)), references_kind));
  unsigned long n_references = 0;
  while (PAIR_P (*holder))
    {
      SCHEME_OBJECT reference = (PAIR_CAR (*holder));
      SCHEME_OBJECT block = (GET_CACHE_REFERENCE_BLOCK (reference));
      if (block == SHARP_F)
	(*holder) = (PAIR_CDR (*holder));
      else
	{
	  n_references += 1;
	  holder = (PAIR_CDR_LOC (*holder));
	}
    }
  return (n_references);
}

static SCHEME_OBJECT *
find_tail_holder (SCHEME_OBJECT references, unsigned int reference_kind)
{
  SCHEME_OBJECT * holder = (MEMORY_LOC (references, reference_kind));
  while (PAIR_P (*holder))
    {
      SCHEME_OBJECT p = (*holder);
      if ((GET_CACHE_REFERENCE_BLOCK (PAIR_CAR (p))) == SHARP_F)
	(*holder) = (PAIR_CDR (p));
      else
	holder = (PAIR_CDR_LOC (p));
    }
  return (holder);
}

static void
update_assignment_references (SCHEME_OBJECT cache)
{
  SCHEME_OBJECT * holder
    = (MEMORY_LOC ((GET_CACHE_REFERENCES (cache)),
		   CACHE_REFERENCES_ASSIGNMENT));
  SCHEME_OBJECT reference_cache
    = (((GET_CACHE_CLONE (cache)) != SHARP_F)
       ? (GET_CACHE_CLONE (cache))
       : cache);
  while (PAIR_P (*holder))
    {
      SCHEME_OBJECT reference = (PAIR_CAR (*holder));
      if ((GET_CACHE_REFERENCE_BLOCK (reference)) == SHARP_F)
	(*holder) = (PAIR_CDR (*holder));
      else
	{
	  store_variable_cache
	    (reference_cache,
	     (GET_CACHE_REFERENCE_BLOCK (reference)),
	     (GET_CACHE_REFERENCE_OFFSET (reference)));
	  holder = (PAIR_CDR_LOC (*holder));
	}
    }
}

static long
guarantee_cache (SCHEME_OBJECT * cell, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT references;
  SCHEME_OBJECT cache;

  if ((get_trap_kind (*cell)) == TRAP_COMPILER_CACHED)
    return (PRIM_DONE);

  RETURN_IF_ERROR (make_cache_references (&references));
  RETURN_IF_ERROR
    (make_cache ((*cell), symbol, SHARP_F, references, (&cache)));

  GC_CHECK (2);
  (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (TRAP_COMPILER_CACHED));
  (*Free++) = cache;
  (*cell) = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, (Free - 2)));
  return (PRIM_DONE);
}

static long
guarantee_clone (SCHEME_OBJECT cache)
{
  if ((GET_CACHE_CLONE (cache)) == SHARP_F)
    {
      SCHEME_OBJECT clone;
      RETURN_IF_ERROR
	(make_cache (EXPENSIVE_OBJECT,
		     (GET_CACHE_NAME (cache)),
		     cache,
		     (GET_CACHE_REFERENCES (cache)),
		     (&clone)));
      SET_CACHE_CLONE (cache, clone);
      update_assignment_references (cache);
    }
  return (PRIM_DONE);
}

static void
flush_clone (SCHEME_OBJECT cache)
{
  if ((GET_CACHE_CLONE (cache)) != SHARP_F)
    {
      SET_CACHE_CLONE (cache, SHARP_F);
      update_assignment_references (cache);
    }
}

static long
make_cache (SCHEME_OBJECT value, SCHEME_OBJECT symbol, SCHEME_OBJECT clone,
	    SCHEME_OBJECT references, SCHEME_OBJECT * cache_ret)
{
  GC_CHECK (4);
  (*Free++) = value;
  (*Free++) = symbol;
  (*Free++) = clone;
  (*Free++) = references;
  (*cache_ret) = (MAKE_POINTER_OBJECT (CACHE_TYPE, (Free - 4)));
  return (PRIM_DONE);
}

static long
make_cache_references (SCHEME_OBJECT * refs_ret)
{
  GC_CHECK (3);
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;
  (*refs_ret) = (MAKE_POINTER_OBJECT (CACHE_REFERENCES_TYPE, (Free - 3)));
  return (PRIM_DONE);
}

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
