/* -*-C-*-

$Id: lookup.c,v 9.62 2001/08/02 04:32:14 cph Exp $

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
static long update_uuo_links
  (SCHEME_OBJECT, SCHEME_OBJECT);
static long allocate_frame_extension
  (unsigned long, SCHEME_OBJECT, SCHEME_OBJECT *);
static int unbind_extension_variable
  (SCHEME_OBJECT, SCHEME_OBJECT);
static long add_cache_reference
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static long add_reference
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, unsigned long);
static long install_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long, unsigned int);
static long install_operator_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
static long update_cache_for_define
  (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static long update_cache_for_unbind
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
static long update_cache_references
  (SCHEME_OBJECT, SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT);
static void split_cache_references
  (SCHEME_OBJECT, unsigned int, SCHEME_OBJECT, SCHEME_OBJECT **);
static int environment_ancestor_or_self_p
  (SCHEME_OBJECT, SCHEME_OBJECT);
static long move_cache_references
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT **);
static void move_cache_references_1
  (SCHEME_OBJECT, SCHEME_OBJECT **, unsigned int);
static SCHEME_OBJECT * find_binding_cell
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
static SCHEME_OBJECT * scan_frame
  (SCHEME_OBJECT, SCHEME_OBJECT, int);
static SCHEME_OBJECT * scan_procedure_bindings
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, int);
static unsigned long count_references
  (SCHEME_OBJECT *);
static SCHEME_OBJECT * find_tail_holder
  (SCHEME_OBJECT *);
static void update_assignment_references
  (SCHEME_OBJECT);
static long guarantee_cache
  (SCHEME_OBJECT *, SCHEME_OBJECT);
static long update_clone
  (SCHEME_OBJECT);
static long make_cache
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT,
   SCHEME_OBJECT *);
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
			   : symbol),
			  0));
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
variable_unreferenceable_p (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
			    SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT dummy_value;
  long result = (lookup_variable (environment, symbol, (&dummy_value)));
  switch (result)
    {
    case ERR_UNBOUND_VARIABLE:
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
			     : symbol),
			    0));
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
  if (PAIR_P (* (GET_CACHE_OPERATOR_REFERENCES (cache))))
    return (update_uuo_links (cache, (MAP_TO_UNASSIGNED (value))));
  SET_CACHE_VALUE (cache, (MAP_TO_UNASSIGNED (value)));
  return (PRIM_DONE);
}

static long
update_uuo_links (SCHEME_OBJECT cache, SCHEME_OBJECT new_value)
{
  RETURN_IF_ERROR (update_clone (cache));
  GC_CHECK
    ((count_references (GET_CACHE_OPERATOR_REFERENCES (cache)))
     * SPACE_PER_UUO_LINK);
  SET_CACHE_VALUE (cache, new_value);
  {
    SCHEME_OBJECT operators = (* (GET_CACHE_OPERATOR_REFERENCES (cache)));
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

long
define_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT value)
{
  if (!ENVIRONMENT_P (environment))
    return (ERR_BAD_FRAME);

  /* If there is already a binding, just assign to it.  */
  {
    SCHEME_OBJECT * cell = (scan_frame (environment, symbol, 1));
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
      ((PROCEDURE_FRAME_P (environment))
       ? (update_cache_for_define ((PAIR_CDR_LOC (pair)), environment, symbol))
       : PRIM_DONE);
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
    SET_FRAME_EXTENSION_PARENT_FRAME
      (extension, (GET_PROCEDURE_ENVIRONMENT (procedure)));
    SET_FRAME_EXTENSION_PROCEDURE (extension, procedure);
    SET_FRAME_EXTENSION_LENGTH (extension, 0);
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

  source_cell = (find_binding_cell (source, symbol, 0));
  if (source_cell == 0)
    return (ERR_UNBOUND_VARIABLE);

  source_kind = (get_trap_kind (*source_cell));
  if (source_kind == TRAP_UNBOUND)
    return (ERR_UNBOUND_VARIABLE);

  target_cell = (scan_frame (target, symbol, 1));
  if ((target_cell != 0)
      && ((get_trap_kind (*target_cell)) == TRAP_COMPILER_CACHED))
    {
      SCHEME_OBJECT target_cache = (GET_TRAP_CACHE (*target_cell));
      if (source_kind == TRAP_COMPILER_CACHED)
	{
	  SCHEME_OBJECT source_cache = (GET_TRAP_CACHE (*source_cell));
	  SCHEME_OBJECT * tail_holders [3];
	  (tail_holders[CACHE_REFERENCES_LOOKUP])
	    = (GET_CACHE_LOOKUP_REFERENCES (source_cache));
	  (tail_holders[CACHE_REFERENCES_ASSIGNMENT])
	    = (GET_CACHE_ASSIGNMENT_REFERENCES (source_cache));
	  (tail_holders[CACHE_REFERENCES_OPERATOR])
	    = (GET_CACHE_OPERATOR_REFERENCES (source_cache));
	  RETURN_IF_ERROR
	    (move_cache_references (source_cache, target_cache, tail_holders));
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

long
unbind_variable (SCHEME_OBJECT environment, SCHEME_OBJECT symbol,
		 SCHEME_OBJECT * value_ret)
{
  SCHEME_OBJECT frame;
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, (&frame)));
  switch ((cell == 0) ? TRAP_UNBOUND : (get_trap_kind (*cell)))
    {
    case TRAP_UNBOUND:
      (*value_ret) = SHARP_F;
      return (PRIM_DONE);

    case NON_TRAP_KIND:
    case TRAP_UNASSIGNED:
      if (!unbind_extension_variable (frame, symbol))
	(*cell) = UNBOUND_OBJECT;
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
	    if (PROCEDURE_FRAME_P (frame))
	      {
		if (!unbind_extension_variable (frame, symbol))
		  (*cell) = UNBOUND_OBJECT;
		RETURN_IF_ERROR
		  (update_cache_for_unbind (cache, frame, symbol));
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

static int
unbind_extension_variable (SCHEME_OBJECT frame, SCHEME_OBJECT symbol)
{
  if ((PROCEDURE_FRAME_P (frame)) && (EXTENDED_FRAME_P (frame)))
    {
      SCHEME_OBJECT * start = (GET_EXTENDED_FRAME_BINDINGS (frame));
      unsigned long length = (GET_EXTENDED_FRAME_LENGTH (frame));
      unsigned long index = 0;
      while (index < length)
	{
	  if ((start[index]) == symbol)
	    {
	      if (index < (length - 1))
		(start[index]) = (start [length - 1]);
	      SET_EXTENDED_FRAME_LENGTH (frame, (length - 1));
	      return (1);
	    }
	  index += 1;
	}
    }
  return (0);
}

/***** Interface to compiled code.  *****/

long
compiler_cache_lookup (SCHEME_OBJECT name, SCHEME_OBJECT block,
		       unsigned long offset)
{
  return
    (add_cache_reference ((compiled_block_environment (block)),
			  name, block, offset,
			  CACHE_REFERENCES_LOOKUP));
}

long
compiler_cache_assignment (SCHEME_OBJECT name, SCHEME_OBJECT block,
			   unsigned long offset)
{
  return
    (add_cache_reference ((compiled_block_environment (block)),
			  name, block, offset,
			  CACHE_REFERENCES_ASSIGNMENT));
}

long
compiler_cache_operator (SCHEME_OBJECT name, SCHEME_OBJECT block,
			 unsigned long offset)
{
  return
    (add_cache_reference ((compiled_block_environment (block)),
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
  SCHEME_OBJECT * cell = (find_binding_cell (environment, symbol, 0));
  SCHEME_OBJECT dummy_cell = UNBOUND_OBJECT;
  if (cell == 0)
    /* There's no binding for the variable, and we don't have access
       to the global environment.  The compiled code needs a cache, so
       we'll install one, but it won't be attached to any environment
       structure.  */
    cell = (&dummy_cell);
  /* This procedure must complete to keep the data structures
     consistent, so we do a GC check in advance to guarantee that all
     of the allocations will finish.  */
  GC_CHECK ((2 * SPACE_PER_CACHE) + SPACE_PER_REFERENCE + SPACE_PER_UUO_LINK);
  RETURN_IF_ERROR (guarantee_cache (cell, symbol));
  {
    SCHEME_OBJECT cache = (GET_TRAP_CACHE (*cell));
    RETURN_IF_ERROR (add_reference (cache, reference_kind, block, offset));
    RETURN_IF_ERROR (update_clone (cache));
    return (install_cache (cache, block, offset, reference_kind));
  }
}

/* Add a new cached reference to the cached reference list pointed at
   by slot.  Attempt to reuse pairs which have been "emptied" by the
   garbage collector.  */

static long
add_reference (SCHEME_OBJECT cache, unsigned int reference_kind,
	       SCHEME_OBJECT block, unsigned long offset)
{
  SCHEME_OBJECT * holder = (GET_CACHE_REFERENCES (cache, reference_kind));
  while (PAIR_P (*holder))
    {
      SCHEME_OBJECT reference = (PAIR_CAR (*holder));
      if ((GET_CACHE_REFERENCE_BLOCK (reference)) == SHARP_F)
	{
	  SET_CACHE_REFERENCE_BLOCK (reference, block);
	  SET_CACHE_REFERENCE_OFFSET (reference, offset);
	  return (PRIM_DONE);
	}
      holder = (PAIR_CDR_LOC (*holder));
    }
  {
    SCHEME_OBJECT reference;
    RETURN_IF_ERROR (make_cache_reference (block, offset, (&reference)));
    GC_CHECK (2);
    (*holder) = (cons (reference, EMPTY_LIST));
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

/* update_cache_for_define is invoked when a new binding is created.
   It recaches (at the definition point) all the references that need
   to point to the new cell.  update_cache_for_unbind is called when a
   binding is removed.  It recaches references from the cache of the
   now unbound variable.  Both procedures call
   update_cache_references, which does the following:

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
update_cache_for_define (SCHEME_OBJECT * new_cell,
			 SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * shadowed_cell
    = (find_binding_cell ((GET_FRAME_PARENT (environment)), symbol, 0));
  return
    (((shadowed_cell != 0)
      && ((get_trap_kind (*shadowed_cell)) == TRAP_COMPILER_CACHED))
     ? (update_cache_references
	((GET_TRAP_CACHE (*shadowed_cell)), new_cell, environment, symbol))
     : PRIM_DONE);
}

static long
update_cache_for_unbind (SCHEME_OBJECT old_cache,
			 SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * shadowed_cell
    = (find_binding_cell ((GET_FRAME_PARENT (environment)), symbol, 0));
  SCHEME_OBJECT dummy_cell = UNBOUND_OBJECT;

  return
    (update_cache_references (old_cache,
			      ((shadowed_cell == 0)
			       ? (&dummy_cell)
			       : shadowed_cell),
			      environment, symbol));
}

static long
update_cache_references (SCHEME_OBJECT from_cache, SCHEME_OBJECT * to_cell,
			 SCHEME_OBJECT environment, SCHEME_OBJECT symbol)
{
  SCHEME_OBJECT * tail_holders [3];

  /* Split the references lists.  */
  split_cache_references
    (from_cache, CACHE_REFERENCES_LOOKUP, environment, tail_holders);
  split_cache_references
    (from_cache, CACHE_REFERENCES_ASSIGNMENT, environment, tail_holders);
  split_cache_references
    (from_cache, CACHE_REFERENCES_OPERATOR, environment, tail_holders);

  /* Return if there are no references that need to be updated.  */
  if ((!PAIR_P (* (tail_holders[CACHE_REFERENCES_LOOKUP])))
      && (!PAIR_P (* (tail_holders[CACHE_REFERENCES_ASSIGNMENT])))
      && (!PAIR_P (* (tail_holders[CACHE_REFERENCES_OPERATOR]))))
    return (PRIM_DONE);

  RETURN_IF_ERROR (guarantee_cache (to_cell, symbol));

  return
    (move_cache_references
     (from_cache, (GET_TRAP_CACHE (*to_cell)), tail_holders));
}

static void
split_cache_references (SCHEME_OBJECT cache,
			unsigned int reference_kind,
			SCHEME_OBJECT environment,
			SCHEME_OBJECT ** tail_holders)
{
  SCHEME_OBJECT * holder = (GET_CACHE_REFERENCES (cache, reference_kind));
  SCHEME_OBJECT references_to_move = EMPTY_LIST;
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
	}
      else
	holder = (PAIR_CDR_LOC (p));
    }
  (*holder) = references_to_move;
  (tail_holders[reference_kind]) = holder;
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

static long
move_cache_references (SCHEME_OBJECT from_cache, SCHEME_OBJECT to_cache,
		       SCHEME_OBJECT ** tail_holders)
{
  GC_CHECK
    (((count_references (tail_holders[CACHE_REFERENCES_OPERATOR]))
      * SPACE_PER_UUO_LINK)
     + (2 * SPACE_PER_CACHE));
  move_cache_references_1 (to_cache, tail_holders, CACHE_REFERENCES_LOOKUP);
  move_cache_references_1
    (to_cache, tail_holders, CACHE_REFERENCES_ASSIGNMENT);
  move_cache_references_1 (to_cache, tail_holders, CACHE_REFERENCES_OPERATOR);
  RETURN_IF_ERROR (update_clone (from_cache));
  RETURN_IF_ERROR (update_clone (to_cache));
  return (PRIM_DONE);
}

static void
move_cache_references_1 (SCHEME_OBJECT cache, SCHEME_OBJECT ** tail_holders,
			 unsigned int reference_kind)
{
  SCHEME_OBJECT tail = (* (tail_holders[reference_kind]));
  (* (tail_holders[reference_kind])) = EMPTY_LIST;
  (* (find_tail_holder (GET_CACHE_REFERENCES (cache, reference_kind)))) = tail;
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
count_references (SCHEME_OBJECT * holder)
{
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
find_tail_holder (SCHEME_OBJECT * holder)
{
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
  SCHEME_OBJECT * holder = (GET_CACHE_ASSIGNMENT_REFERENCES (cache));
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

  GC_CHECK (3);
  references = (MAKE_POINTER_OBJECT (CACHE_REFERENCES_TYPE, Free));
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;
  (*Free++) = EMPTY_LIST;

  RETURN_IF_ERROR
    (make_cache ((*cell), symbol, SHARP_F, references, (&cache)));

  GC_CHECK (2);
  (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (TRAP_COMPILER_CACHED));
  (*Free++) = cache;
  (*cell) = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, (Free - 2)));
  return (PRIM_DONE);
}

static long
update_clone (SCHEME_OBJECT cache)
{
  if ((PAIR_P (* (GET_CACHE_ASSIGNMENT_REFERENCES (cache))))
      && (PAIR_P (* (GET_CACHE_OPERATOR_REFERENCES (cache)))))
    {
      if ((GET_CACHE_CLONE (cache)) == SHARP_F)
	{
	  SCHEME_OBJECT clone;
	  RETURN_IF_ERROR
	    (make_cache (EXPENSIVE_OBJECT,
			 (GET_CACHE_NAME (cache)),
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
  return (PRIM_DONE);
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
make_cache_reference (SCHEME_OBJECT block, unsigned long offset,
		      SCHEME_OBJECT * ref_ret)
{
  GC_CHECK (2);
  (*Free++) = block;
  (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (offset));
  (*ref_ret) = (MAKE_POINTER_OBJECT (TC_WEAK_CONS, (Free - 2)));
  return (PRIM_DONE);
}
