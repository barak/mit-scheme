/* -*-C-*-

$Id: lookup.c,v 9.58 2000/12/05 21:23:45 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

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

/*
 * This file contains symbol lookup and modification routines.
 * See a paper by Jim Miller and Bill Rozas in Lisp and Symbolic Computation
 * (4th issue 1990) for a justification of the algorithms.
 */

#include "scheme.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"

static void EXFUN (fix_references, (SCHEME_OBJECT *, SCHEME_OBJECT));
static long EXFUN
  (add_reference, (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT));

/* NOTE:
   Although this code has been parallelized, it has not been
   exhaustively tried on a parallel processor.  There are probably
   various race conditions/potential deadlocks that have to be thought
   about carefully.
 */

/* Useful constants. */

/* This is returned by various procedures to cause a Scheme
   unbound variable error to be signalled.
 */

SCHEME_OBJECT unbound_trap_object[] = { UNBOUND_OBJECT };

/* This is returned by lookup to force a deep lookup when the variable
   needs to be recompiled.
 */

SCHEME_OBJECT uncompiled_trap_object[] = { DANGEROUS_UNBOUND_OBJECT };

/* This is returned by lookup to cause a Scheme broken compiled
   variable error to be signalled.
 */

SCHEME_OBJECT illegal_trap_object[] = { ILLEGAL_OBJECT };

/* This is passed to deep_lookup as the variable to compile when
   we don't really have a variable.
 */

SCHEME_OBJECT fake_variable_object[3];

/* scan_frame searches a frame for a given name.
   If it finds the names, it stores into hunk the path by which it was
   found, so that future references do not spend the time to find it
   again.  It returns a pointer to the value cell, or a null pointer
   cell if the variable was not found in this frame.
 */

extern SCHEME_OBJECT *
  EXFUN (scan_frame,
	 (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *, long, Boolean));

SCHEME_OBJECT *
DEFUN (scan_frame, (frame, sym, hunk, depth, unbound_valid_p),
       SCHEME_OBJECT frame
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT * hunk
       AND long depth
       AND Boolean unbound_valid_p)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (compile_serializer);
#endif
  fast SCHEME_OBJECT *scan, temp;
  fast long count;

  temp = MEMORY_REF (frame, ENVIRONMENT_FUNCTION);

  if (OBJECT_TYPE (temp) == AUX_LIST_TYPE)
  {
    /* Search for an auxiliary binding. */

    SCHEME_OBJECT *start;

    scan = OBJECT_ADDRESS (temp);
    start = scan;
    count = Lexical_Offset(scan[AUX_LIST_COUNT]);
    scan += AUX_LIST_FIRST;

    while (--count >= 0)
    {
      if (FAST_PAIR_CAR (*scan) == sym)
      {
	SCHEME_OBJECT *cell;

	cell = PAIR_CDR_LOC (*scan);
	if (MEMORY_FETCH (cell[0]) == DANGEROUS_UNBOUND_OBJECT)
	{
	  /* A dangerous unbound object signals that
	     a definition here must become dangerous,
	     but is not a real bining.
	   */
	  return (unbound_valid_p ? (cell) : ((SCHEME_OBJECT *) NULL));
	}
	setup_lock(compile_serializer, hunk);
	hunk[VARIABLE_COMPILED_TYPE] = MAKE_OBJECT (AUX_REF, depth);
	hunk[VARIABLE_OFFSET] = Make_Local_Offset(scan - start);
	remove_lock(compile_serializer);
	return (cell);
      }
      scan += 1;
    }
    temp = MEMORY_REF (temp, ENV_EXTENSION_PROCEDURE);
  }

  /* Search for a formal parameter. */

  temp = (FAST_MEMORY_REF ((FAST_MEMORY_REF (temp, PROCEDURE_LAMBDA_EXPR)),
			   LAMBDA_FORMALS));
  for (count = ((VECTOR_LENGTH (temp)) - 1),
       scan = (MEMORY_LOC (temp, VECTOR_DATA + 1));
       count > 0;
       count -= 1,
       scan += 1)
  {
    if (*scan == sym)
    {
      fast long offset;

      offset = 1 + VECTOR_LENGTH (temp) - count;

      setup_lock(compile_serializer, hunk);
      if (depth != 0)
      {
	hunk[VARIABLE_COMPILED_TYPE] = MAKE_OBJECT (FORMAL_REF, depth);
	hunk[VARIABLE_OFFSET] = Make_Local_Offset(offset);
      }
      else
      {
	hunk[VARIABLE_COMPILED_TYPE] = Make_Local_Offset(offset);
	hunk[VARIABLE_OFFSET] = SHARP_F;
      }
      remove_lock(compile_serializer);

      return (MEMORY_LOC (frame, offset));
    }
  }

  return ((SCHEME_OBJECT *) NULL);
}

/* The lexical lookup procedure.
   deep_lookup searches env for an occurrence of sym.  When it finds
   it, it stores into hunk the path by which it was found, so that
   future references do not spend the time to find it again.
   It returns a pointer to the value cell, or a bogus value cell if
   the variable was unbound.
 */

SCHEME_OBJECT *
DEFUN (deep_lookup, (env, sym, hunk),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT * hunk)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (compile_serializer);
#endif
  fast SCHEME_OBJECT frame;
  fast long depth;

  for (depth = 0, frame = env;
       OBJECT_TYPE (frame) != GLOBAL_ENV;
       depth += 1,
       frame = FAST_MEMORY_REF (MEMORY_REF (frame, ENVIRONMENT_FUNCTION),
			       PROCEDURE_ENVIRONMENT))
  {
    fast SCHEME_OBJECT *cell;

    cell = (scan_frame (frame, sym, hunk, depth, false));
    if (cell != ((SCHEME_OBJECT *) NULL))
    {
      return (cell);
    }
  }

  /* The reference is global. */

  if (OBJECT_DATUM (frame) != GO_TO_GLOBAL)
  {
    return (unbound_trap_object);
  }

  setup_lock(compile_serializer, hunk);
  hunk[VARIABLE_COMPILED_TYPE] = (OBJECT_NEW_TYPE (TC_UNINTERNED_SYMBOL, sym));
  hunk[VARIABLE_OFFSET] = SHARP_F;
  remove_lock(compile_serializer);

  return (MEMORY_LOC (sym, SYMBOL_GLOBAL_VALUE));
}

/* Shallow lookup performed "out of line" by various procedures.
   It takes care of invoking deep_lookup when necessary.
 */

extern SCHEME_OBJECT *
  EXFUN (lookup_cell, (SCHEME_OBJECT *, SCHEME_OBJECT));

SCHEME_OBJECT *
DEFUN (lookup_cell, (hunk, env),
       SCHEME_OBJECT * hunk
       AND SCHEME_OBJECT env)
{
  SCHEME_OBJECT *cell, value;
  long trap_kind;

  lookup(cell, env, hunk, repeat_lookup_cell);

  value = MEMORY_FETCH (cell[0]);

  if (OBJECT_TYPE (value) != TC_REFERENCE_TRAP)
  {
    return (cell);
  }

  get_trap_kind(trap_kind, value);
  switch(trap_kind)
  {
    case TRAP_DANGEROUS:
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
    case TRAP_COMPILER_CACHED_DANGEROUS:
      return (deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk));

    case TRAP_COMPILER_CACHED:
    case TRAP_FLUID:
    case TRAP_UNBOUND:
    case TRAP_UNASSIGNED:
      return (cell);

    default:
      return (illegal_trap_object);
  }
}

/* Full lookup end code.
   deep_lookup_end handles all the complicated and dangerous cases.
   cell is the value cell (supposedly found by deep_lookup).  Hunk is
   the address of the scode variable object which may need to be
   recompiled if the reference is dangerous.
 */

long
DEFUN (deep_lookup_end, (cell, hunk),
       SCHEME_OBJECT * cell
       AND SCHEME_OBJECT * hunk)
{
  long trap_kind;
  long return_value = PRIM_DONE;
  Boolean repeat_p;

  do {
    repeat_p = false;
    Val = MEMORY_FETCH (cell[0]);
    FUTURE_VARIABLE_SPLICE (((SCHEME_OBJECT) cell), 0, Val);
    if (!(REFERENCE_TRAP_P(Val)))
    {
      return (PRIM_DONE);
    }

    /* Remarks:
       In the code below, break means uncompile the variable,
       while continue means do not.
       If repeat_p is set the whole process is redone, but since the
       "danger bit" is kept on the outermost trap, the "uncompilation"
       will not be affected by subsequent iterations.
     */

    get_trap_kind(trap_kind, Val);
    switch(trap_kind)
    {
      /* The following cases are divided into pairs:
	 the non-dangerous version leaves the compilation alone.
	 The dangerous version uncompiles.
       */

      case TRAP_UNASSIGNED:
	return (ERR_UNASSIGNED_VARIABLE);

      case TRAP_UNASSIGNED_DANGEROUS:
	return_value = ERR_UNASSIGNED_VARIABLE;
	break;

      case TRAP_DANGEROUS:
      {
	SCHEME_OBJECT trap_value;

	trap_value = Val;
	Val = (MEMORY_REF (trap_value, TRAP_EXTRA));
	FUTURE_VARIABLE_SPLICE (trap_value, TRAP_EXTRA, Val);
	return_value = PRIM_DONE;
	break;
      }

      case TRAP_FLUID:
      case TRAP_FLUID_DANGEROUS:
	cell = lookup_fluid(Val);
	repeat_p = true;
	if (trap_kind == TRAP_FLUID)
	  continue;
	break;

      case TRAP_COMPILER_CACHED:
      case TRAP_COMPILER_CACHED_DANGEROUS:
	cell = MEMORY_LOC (MEMORY_REF (Val, TRAP_EXTRA), TRAP_EXTENSION_CELL);
	repeat_p = true;
	if (trap_kind == TRAP_COMPILER_CACHED)
	  continue;
	break;

      case TRAP_UNBOUND:
	return (ERR_UNBOUND_VARIABLE);

      case TRAP_UNBOUND_DANGEROUS:
	return_value = ERR_UNBOUND_VARIABLE;
	break;

      default:
	return_value = ERR_ILLEGAL_REFERENCE_TRAP;
	break;
    }

    /* The reference was dangerous, uncompile the variable. */
    {
#ifdef DECLARE_LOCK
      DECLARE_LOCK (compile_serializer);
#endif
      setup_lock(compile_serializer, hunk);
      hunk[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
      hunk[VARIABLE_OFFSET] = SHARP_F;
      remove_lock(compile_serializer);
    }

  } while (repeat_p);

  return (return_value);
}

/* Simple lookup finalization.
   All the hairy cases are left to deep_lookup_end.
   env is the environment where the reference was supposedly resolved.
   If there is any question about the validity of the resolution (due
   to dangerousness, for example), a deep lookup operation is
   performed, and control is given to deep_lookup_end.
 */

long
DEFUN (lookup_end, (cell, env, hunk),
       SCHEME_OBJECT * cell
       AND SCHEME_OBJECT env
       AND SCHEME_OBJECT * hunk)
{
  long trap_kind;

lookup_end_restart:
  Val = MEMORY_FETCH (cell[0]);
  FUTURE_VARIABLE_SPLICE (((SCHEME_OBJECT) cell), 0, Val);

  if (!(REFERENCE_TRAP_P(Val)))
  {
    return (PRIM_DONE);
  }

  get_trap_kind(trap_kind, Val);
  switch(trap_kind)
  {
    case TRAP_DANGEROUS:
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
    case TRAP_COMPILER_CACHED_DANGEROUS:
      return
	(deep_lookup_end(deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk),
			 hunk));

    case TRAP_COMPILER_CACHED:
      cell = MEMORY_LOC (MEMORY_REF (Val, TRAP_EXTRA), TRAP_EXTENSION_CELL);
      goto lookup_end_restart;

    case TRAP_FLUID:
      cell = lookup_fluid(Val);
      goto lookup_end_restart;

    case TRAP_UNBOUND:
      return (ERR_UNBOUND_VARIABLE);

    case TRAP_UNASSIGNED:
      return (ERR_UNASSIGNED_VARIABLE);

    default:
      return (ERR_ILLEGAL_REFERENCE_TRAP);
  }
}

/* Complete assignment finalization.

   deep_assignment_end handles all dangerous cases, and busts compiled
   code operator reference caches as appropriate.  It is similar to
   deep_lookup_end.
   value is the new value for the variable.
   force forces an assignment if the variable is unbound.  This is
   used for redefinition in the global environment

   Notes on multiprocessor locking:

   The lock for assignment is usually in the original value cell in
   the environment structure.
   There are two cases where it is not:

   - Deep fluid variables.  The lock is in the fluid value cell
   corresponding to this process.  The original lock is removed before
   the fluid list is examined.

   - Compiler cached variables.  The lock is in the new value cell.
   It is here so that compiled code can also lock it, since it does
   not have a pointer to the environment structure at all.  The lock
   is moved (updated) from the original location to the new location.
   Ideally the original lock is not released until the new one is
   acquired, but we may not be able to guarantee this.
   The code is carefully written so that a weaker condition makes it
   valid.  The condition is that locks should be granted in the order
   of request.  The reason for this is that the code which can
   affect an operation must acquire the same locks and in the same
   order, thus if there is no interleaving of these operations, the
   result will be correct.

   Important:

   A re-definition can take place before the lock is grabbed in this
   code and we will be clobbering the wrong cell.  To be paranoid we
   should redo the lookup while we have the cell locked and confirm
   that this is still valid, but this is hard to do here.
   Alternatively the lock could be grabbed by the caller and passed as
   an argument after confirming the correctness of the binding.  A
   third option (the one in place now) is not to worry about this,
   saying that there is a race condition in the user code and that the
   definition happened after this assignment.  For more precise
   sequencing, the user should synchronize her/his assignments and
   definitions her/himself.

   assignment_end suffers from this problem as well.

 */

#define RESULT(value)							\
{									\
  return_value = (value);						\
  break;								\
}

#define UNCOMPILE(value)						\
{									\
  uncompile_p = true;							\
  return_value = (value);						\
  break;								\
}

#define ABORT(value)							\
{									\
  remove_lock(set_serializer);						\
  return (value);							\
}

#define REDO()								\
{									\
  repeat_p = true;							\
  break;								\
}

long
DEFUN (deep_assignment_end, (cell, hunk, value, force),
       fast SCHEME_OBJECT * cell
       AND SCHEME_OBJECT * hunk
       AND SCHEME_OBJECT value
       AND Boolean force)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer);
#endif
  long trap_kind;
  long return_value = PRIM_DONE;
  SCHEME_OBJECT bogus_unassigned, extension, saved_extension;
  SCHEME_OBJECT saved_value = SHARP_F;
  Boolean repeat_p, uncompile_p, fluid_lock_p;

  /* State variables */
  saved_extension = SHARP_F;
  uncompile_p = false;
  fluid_lock_p = false;

  bogus_unassigned = Get_Fixed_Obj_Slot(Non_Object);
  if (value == bogus_unassigned)
    value = UNASSIGNED_OBJECT;

  setup_lock(set_serializer, cell);

  do {

    repeat_p = false;
    Val = *cell;

    if (!(REFERENCE_TRAP_P(Val)))
    {
      *cell = value;
      RESULT(PRIM_DONE);
    }

    /* Below, break means uncompile the variable. */

    get_trap_kind(trap_kind, Val);

    switch(trap_kind)
    {
      case TRAP_DANGEROUS:
        Val = MEMORY_REF (Val, TRAP_EXTRA);
	if (value == UNASSIGNED_OBJECT)
	{
	  *cell = DANGEROUS_UNASSIGNED_OBJECT;
	}
	else
	{
	  Do_Store_No_Lock ((MEMORY_LOC (*cell, TRAP_EXTRA)), value);
	}
	UNCOMPILE(PRIM_DONE);

      case TRAP_UNBOUND:
	if (!force)
	{
	  UNCOMPILE(ERR_UNBOUND_VARIABLE)
	}
	/* Fall through */

      case TRAP_UNASSIGNED:
	Val = bogus_unassigned;
	*cell = value;
	RESULT(PRIM_DONE);

      case TRAP_UNBOUND_DANGEROUS:
	if (!force)
	{
	  UNCOMPILE(ERR_UNBOUND_VARIABLE);
	}

	if (value == UNASSIGNED_OBJECT)
	{
	  *cell = DANGEROUS_UNASSIGNED_OBJECT;
	  UNCOMPILE(PRIM_DONE);
	}
	/* Fall through */

      case TRAP_UNASSIGNED_DANGEROUS:
	Val = bogus_unassigned;
	if (value != UNASSIGNED_OBJECT)
	{
	  SCHEME_OBJECT result;

	  if (GC_allocate_test(2))
	  {
	    Request_GC(2);
	    ABORT(PRIM_INTERRUPT);
	  }
	  result = MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Free);
	  *Free++ = DANGEROUS_OBJECT;
	  *Free++ = value;
	  *cell = result;
	}
	UNCOMPILE(PRIM_DONE);

      case TRAP_EXPENSIVE:
	/* This should only happen if we have been invoked by
	   compiler_assignment_end invoked by compiler_reference_trap;
	 */
	extension = cell[TRAP_EXTENSION_CLONE];
	goto compiler_cache_assignment;

      case TRAP_COMPILER_CACHED_DANGEROUS:
	uncompile_p = true;
	/* Fall through */

      case TRAP_COMPILER_CACHED:
	extension = FAST_MEMORY_REF (Val, TRAP_EXTRA);

compiler_cache_assignment:
	{
	  SCHEME_OBJECT references;

	  /* Unlock and lock at the new value cell. */

	  references = (FAST_MEMORY_REF (extension,
					 TRAP_EXTENSION_REFERENCES));
	  cell = (MEMORY_LOC (extension, TRAP_EXTENSION_CELL));
	  update_lock (set_serializer, cell);

	  if ((FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR))
	      != SHARP_F)
	  {
	    if (saved_extension != SHARP_F)
	    {
	      ABORT(ERR_BROKEN_VARIABLE_CACHE);
	    }
	    saved_extension = extension;
	    saved_value = *cell;
	  }
	  REDO();
	}

      /* Remarks:
	 If this is the inner trap of a compiler cache, and there are
	 uuo links, there will actually be no recaching, since the old
	 contents and the new one will be the fluid trap, and the
	 links will already be set up for the fluid trap.  Thus we can
	 temporarily unlock while the iteration takes place.
       */
      case TRAP_FLUID_DANGEROUS:
	uncompile_p = true;
	/* Fall through */

      case TRAP_FLUID:
	fluid_lock_p = true;
	remove_lock(set_serializer);
	cell = lookup_fluid(Val);
	setup_lock(set_serializer, cell);
	REDO();

      default:
	UNCOMPILE(ERR_ILLEGAL_REFERENCE_TRAP);
    }
  } while (repeat_p);

  if (saved_extension != SHARP_F)
  {
    if (fluid_lock_p)
    {
      /* Guarantee that there is a lock on the variable cache around
	 the call to recache_uuo_links.
       */

      update_lock (set_serializer,
		   (MEMORY_LOC (saved_extension, TRAP_EXTENSION_CELL)));
    }

    /* NOTE:
       recache_uuo_links can take an arbitrary amount of time since
       there may be an internal lock and the code may have to uncache
       arbitrarily many links.
       Deadlock should not occur since both locks are always acquired
       in the same order.
     */

    return_value = (recache_uuo_links (saved_extension, saved_value));
    remove_lock (set_serializer);

    if (return_value != PRIM_DONE)
    {
      return (return_value);
    }
  }
  else
  {
    remove_lock (set_serializer);
  }

  /* This must be done after the assignment lock has been removed,
     to avoid potential deadlock.
   */

  if (uncompile_p)
  {
    /* The reference was dangerous, uncompile the variable. */
#ifdef DECLARE_LOCK
    DECLARE_LOCK (compile_serializer);
#endif
    setup_lock (compile_serializer, hunk);
    hunk[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
    hunk[VARIABLE_OFFSET] = SHARP_F;
    remove_lock (compile_serializer);
  }

  return (return_value);
}

#undef ABORT
#undef REDO
#undef RESULT
#undef UNCOMPILE

/* Simple assignment end.
   assignment_end lets deep_assignment_end handle all the hairy cases.
   It is similar to lookup_end, but there is some hair for
   unassignedness and compiled code cached references.
 */

long
DEFUN (assignment_end, (cell, env, hunk, value),
       fast SCHEME_OBJECT * cell
       AND SCHEME_OBJECT env
       AND SCHEME_OBJECT * hunk
       AND SCHEME_OBJECT value)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer);
#endif
  SCHEME_OBJECT bogus_unassigned;
  long temp;

  bogus_unassigned = Get_Fixed_Obj_Slot(Non_Object);
  if (value == bogus_unassigned)
    value = UNASSIGNED_OBJECT;

assignment_end_before_lock:

  setup_lock(set_serializer, cell);

assignment_end_after_lock:

  Val = *cell;

  if (!(REFERENCE_TRAP_P(Val)))
  {
    *cell = value;
    remove_lock(set_serializer);
    return (PRIM_DONE);
  }

  get_trap_kind(temp, Val);
  switch(temp)
  {
    case TRAP_DANGEROUS:
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
    case TRAP_COMPILER_CACHED_DANGEROUS:
      remove_lock(set_serializer);
      return
	(deep_assignment_end(deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk),
			     hunk,
			     value,
			     false));

    case TRAP_COMPILER_CACHED:
    {
      SCHEME_OBJECT extension, references;

      extension = FAST_MEMORY_REF (Val, TRAP_EXTRA);
      references = FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES);

      if (FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR) != SHARP_F)
      {
	/* There are uuo links.
	   wimp out and let deep_assignment_end handle it.
	 */

	remove_lock(set_serializer);
	return (deep_assignment_end(cell, hunk, value, false));
      }
      cell = MEMORY_LOC (extension, TRAP_EXTENSION_CELL);
      update_lock(set_serializer, cell);
      goto assignment_end_after_lock;
    }

    case TRAP_FLUID:
      remove_lock(set_serializer);
      cell = lookup_fluid(Val);
      goto assignment_end_before_lock;

    case TRAP_UNBOUND:
      temp = ERR_UNBOUND_VARIABLE;
      break;

    case TRAP_UNASSIGNED:
      Val = bogus_unassigned;
      *cell = value;
      temp = PRIM_DONE;
      break;

    default:
      temp = ERR_ILLEGAL_REFERENCE_TRAP;
      break;
  }
  remove_lock(set_serializer);
  return (temp);
}

/* Finds the fluid value cell associated with the reference trap on
   this processor's fluid "binding" list.  It is just like ASSQ.
 */

SCHEME_OBJECT *
DEFUN (lookup_fluid, (trap), fast SCHEME_OBJECT trap)
{
  fast SCHEME_OBJECT fluids, *this_pair;

  fluids = Fluid_Bindings;

  if (Fluids_Debug)
  {
    Print_Expression(fluids, "Searching fluid bindings");
  }

  while (PAIR_P(fluids))
  {
    this_pair = OBJECT_ADDRESS (FAST_PAIR_CAR (fluids));

    if (this_pair[CONS_CAR] == trap)
    {
      if (Fluids_Debug)
	outf_error ("Fluid found.\n");

      return (&this_pair[CONS_CDR]);
    }

    fluids = FAST_PAIR_CDR (fluids);
  }

  /* Not found in fluid binding alist, so use default. */

  if (Fluids_Debug)
    outf_error ("Fluid not found, using default.\n");

  return (MEMORY_LOC (trap, TRAP_EXTRA));
}

/* Utilities for definition.

   redefinition is used when the definition is in fact an assignment.
   A binding already exists in this frame.

   dangerize is invoked to guarantee that any variables "compiled" to
   this location are recompiled at the next reference.
 */

#define redefinition(cell, value) \
  (deep_assignment_end (cell, fake_variable_object, value, true))

long
DEFUN (definition, (cell, value, shadowed_p),
       SCHEME_OBJECT * cell
       AND SCHEME_OBJECT value
       AND Boolean shadowed_p)
{
  if (shadowed_p)
    return (redefinition (cell, value));
  else
  {
#ifdef DECLARE_LOCK
    DECLARE_LOCK (set_serializer);
#endif
    setup_lock (set_serializer, cell);
    if (*cell == DANGEROUS_UNBOUND_OBJECT)
    {
      *cell = value;
      remove_lock (set_serializer);
      return (PRIM_DONE);
    }
    else
    {
      /* Unfortunate fact of life: This binding will be dangerous
	 even if there was no need, but this is the only way to
	 guarantee consistent values.
       */
      remove_lock (set_serializer);
      return (redefinition (cell, value));
    }
  }
}

long
DEFUN (dangerize, (cell, sym),
       fast SCHEME_OBJECT * cell
       AND SCHEME_OBJECT sym)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer);
#endif
  fast long temp;
  SCHEME_OBJECT trap;

  setup_lock (set_serializer, cell);
  if (!(REFERENCE_TRAP_P (*cell)))
  {
    if (GC_allocate_test (2))
    {
      remove_lock (set_serializer);
      Request_GC (2);
      return (PRIM_INTERRUPT);
    }
    trap = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Free));
    *Free++ = DANGEROUS_OBJECT;
    *Free++ = *cell;
    *cell = trap;
    remove_lock (set_serializer);
    return (simple_uncache (cell, sym));
  }

  get_trap_kind (temp, *cell);
  switch (temp)
  {
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
      break;

    case TRAP_COMPILER_CACHED:
      Do_Store_No_Lock
	((MEMORY_LOC (*cell, TRAP_TAG)),
	 (LONG_TO_UNSIGNED_FIXNUM (TRAP_COMPILER_CACHED_DANGEROUS)));
      /* Fall through */

    case TRAP_COMPILER_CACHED_DANGEROUS:
    {
      remove_lock (set_serializer);
      return (compiler_uncache (cell, sym));
    }

    case TRAP_FLUID:
      Do_Store_No_Lock
	((MEMORY_LOC (*cell, TRAP_TAG)),
	 (LONG_TO_UNSIGNED_FIXNUM (TRAP_FLUID_DANGEROUS)));
      break;

    case TRAP_UNBOUND:
      *cell = DANGEROUS_UNBOUND_OBJECT;
      break;

    case TRAP_UNASSIGNED:
      *cell = DANGEROUS_UNASSIGNED_OBJECT;
      break;

    default:
      remove_lock (set_serializer);
      return (ERR_ILLEGAL_REFERENCE_TRAP);
  }
  remove_lock (set_serializer);
  return (simple_uncache (cell, sym));
}

/* The core of the incremental definition mechanism.

   It takes care of dangerizing any bindings being shadowed by this
   definition, extending the frames appropriately, and uncaching or
   recaching (according to the DEFINITION_RECACHES_EAGERLY flag) any
   compiled code reference caches which might be affected by the new
   definition.

   *UNDEFINE*: If (local?) undefine is ever implemented, it suffices
   to set the value cell to DANGEROUS_UNBOUND_OBJECT, uncache all the
   compiler cached variables to the location, and rewrite the code
   below slightly as implied by the comments tagged *UNDEFINE*.
 */

long
DEFUN (extend_frame,
       (env, sym, value, original_frame, recache_p),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT value
       AND SCHEME_OBJECT original_frame
       AND Boolean recache_p)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (extension_serializer);
#endif
  SCHEME_OBJECT extension, the_procedure;
  fast SCHEME_OBJECT *scan;
  long aux_count;

  if ((OBJECT_TYPE (env)) == GLOBAL_ENV)
  {
    /* *UNDEFINE*: If undefine is ever implemented, this code need not
       change: There are no shadowed bindings that need to be
       recached.
     */
    if ((OBJECT_DATUM (env)) != GO_TO_GLOBAL)
    {
      if (env == original_frame)
      {
	return (ERR_BAD_FRAME);
      }
      else
      {
	/* We have a new definition in a chain rooted at the empty
	   environment.
	   We need not uncache/recache, but we need to set all
	   global state accordingly.
	   We use a cell which never needs uncacheing/recacheing
	   and use the ordinary code otherwise.

	   This is done only because of compiler cached variables.
	 */
	return (compiler_uncache ((unbound_trap_object), sym));
      }
    }
    else if (env == original_frame)
    {
      return (redefinition ((MEMORY_LOC (sym, SYMBOL_GLOBAL_VALUE)),
			    value));
    }
    else
    {
      return (dangerize ((MEMORY_LOC (sym, SYMBOL_GLOBAL_VALUE)), sym));
    }
  }

  the_procedure = (MEMORY_REF (env, ENVIRONMENT_FUNCTION));
  if ((OBJECT_TYPE (the_procedure)) == AUX_LIST_TYPE)
    the_procedure = (MEMORY_REF (the_procedure, ENV_EXTENSION_PROCEDURE));

  /* Search the formals. */

  {
    fast long count;
    SCHEME_OBJECT formals;

    formals = (FAST_MEMORY_REF ((FAST_MEMORY_REF (the_procedure,
						  PROCEDURE_LAMBDA_EXPR)),
				LAMBDA_FORMALS));
    for (count = ((VECTOR_LENGTH (formals)) - 1),
	 scan = (MEMORY_LOC (formals, VECTOR_DATA + 1));
	 count > 0;
	 count -= 1)
    {
      /* *UNDEFINE*: If undefine is ever implemented, this code must
	 check whether the value is DANGEROUS_UNBOUND_OBJECT, and if
	 so, a search must be done to cause the shadowed compiler
	 cached variables to be recached, as in the aux case below.
       */
      if (*scan++ == sym)
      {
	long offset;

	offset = (1 + (VECTOR_LENGTH (formals))) - count;
	if (env == original_frame)
	{
	  return (redefinition ((MEMORY_LOC (env, offset)), value));
	}
	else
	{
	  return (dangerize ((MEMORY_LOC (env, offset)), sym));
	}
      }
    }
  }

  /* Guarantee that there is an extension slot. */

redo_aux_lookup:

  setup_lock (extension_serializer, (OBJECT_ADDRESS (env)));
  extension = (FAST_MEMORY_REF (env, ENVIRONMENT_FUNCTION));
  if ((OBJECT_TYPE (extension)) != AUX_LIST_TYPE)
  {
    fast long i;

    if (GC_allocate_test (AUX_LIST_INITIAL_SIZE))
    {
      remove_lock (extension_serializer);
      Request_GC (AUX_LIST_INITIAL_SIZE);
      return (PRIM_INTERRUPT);
    }
    scan = Free;
    extension = (MAKE_POINTER_OBJECT (AUX_LIST_TYPE, scan));

    scan[ENV_EXTENSION_HEADER] =
      (MAKE_OBJECT (TC_MANIFEST_VECTOR, (AUX_LIST_INITIAL_SIZE - 1)));

    scan[ENV_EXTENSION_PARENT_FRAME] =
      (MEMORY_REF (the_procedure, PROCEDURE_ENVIRONMENT));

    scan[ENV_EXTENSION_PROCEDURE] = the_procedure;

    scan[ENV_EXTENSION_COUNT] = (Make_Local_Offset (0));

    for (i = AUX_CHUNK_SIZE, scan += AUX_LIST_FIRST;
	 --i >= 0;)
      *scan++ = SHARP_F;

    Free = scan;
    Do_Store_No_Lock ((MEMORY_LOC (env, ENVIRONMENT_FUNCTION)), extension);
  }
  aux_count = (Lexical_Offset (FAST_MEMORY_REF (extension, AUX_LIST_COUNT)));
  remove_lock (extension_serializer);

  /* Search the aux list. */

  {
    fast long count;

    scan = (OBJECT_ADDRESS (extension));
    count = aux_count;
    scan += AUX_LIST_FIRST;

    while (--count >= 0)
    {
      if ((FAST_PAIR_CAR (*scan)) == sym)
      {
	scan = (PAIR_CDR_LOC (*scan));

	/* This is done only because of compiler cached variables.
	   In their absence, this conditional is unnecessary.

	   *UNDEFINE*: This would also have to be done for other kinds
	   of bindings if undefine is ever implemented.  See the
	   comments above.
	 */
	if ((MEMORY_FETCH (scan[0])) == DANGEROUS_UNBOUND_OBJECT)
	{
	  long temp;

	  temp =
	    (compiler_uncache
	     (deep_lookup ((FAST_MEMORY_REF (extension,
					     ENV_EXTENSION_PARENT_FRAME)),
			   sym,
			   fake_variable_object),
	      sym));

	  if ((temp != PRIM_DONE) || (env != original_frame))
	  {
	    return (temp);
	  }
	  return (shadowing_recache (scan, env, sym, value, true));
	}

	if (env == original_frame)
	{
	  return (redefinition (scan, value));
	}
	else
	{
	  return (dangerize (scan, sym));
	}
      }
      scan += 1;
    }
  }

  /* Not found in this frame at all. */

  {
    fast long temp;

    temp =
      (extend_frame ((FAST_MEMORY_REF (extension, ENV_EXTENSION_PARENT_FRAME)),
		     sym, SHARP_F, original_frame, recache_p));

    if (temp != PRIM_DONE)
    {
      return (temp);
    }

    /* Proceed to extend the frame:
       - If the frame is the one where the definition is occurring,
	 put the value in the new value cell.
       - Otherwise, put a dangerous unbound trap there.
       - This code is careful to restart if some other process defines
         something in the meantime in this frame.
     */

    setup_lock (extension_serializer, (OBJECT_ADDRESS (env)));
    temp = (Lexical_Offset (FAST_MEMORY_REF (extension, AUX_LIST_COUNT)));

    if ((extension != (FAST_MEMORY_REF (env, ENVIRONMENT_FUNCTION))) ||
	(temp != aux_count))
    {
      remove_lock (extension_serializer);
      goto redo_aux_lookup;
    }

    scan = (OBJECT_ADDRESS (extension));

    if ((temp + (AUX_LIST_FIRST - 1)) == ((long) (VECTOR_LENGTH (extension))))
    {
      fast long i;
      fast SCHEME_OBJECT *fast_free;

      i = ((2 * temp) + AUX_LIST_FIRST);

      if (GC_allocate_test (i))
      {
	remove_lock (extension_serializer);
	Request_GC (i);
	return (PRIM_INTERRUPT);
      }

      fast_free = Free;
      i -= 1;

      scan += 1;
      *fast_free++ = (MAKE_OBJECT (TC_MANIFEST_VECTOR, i));
      for (i = (temp + (AUX_LIST_FIRST - 1)); --i >= 0; )
	*fast_free++ = *scan++;
      for (i = temp; --i >= 0; )
	*fast_free++ = SHARP_F;

      scan = Free;
      Free = fast_free;
      Do_Store_No_Lock
	((MEMORY_LOC (env, ENVIRONMENT_FUNCTION)),
	 (MAKE_POINTER_OBJECT (AUX_LIST_TYPE, scan)));
    }

    if (GC_allocate_test (2))
    {
      remove_lock (extension_serializer);
      Request_GC (2);
      return (PRIM_INTERRUPT);
    }

    {
      SCHEME_OBJECT result;

      result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
      *Free++ = sym;
      *Free++ = DANGEROUS_UNBOUND_OBJECT;

      scan[temp + AUX_LIST_FIRST] = result;
      scan[AUX_LIST_COUNT] = (Make_Local_Offset (temp + 1));

      remove_lock (extension_serializer);

      if ((env != original_frame) || (!recache_p))
	return (PRIM_DONE);
      else
	return (shadowing_recache ((Free - 1), env, sym, value, false));
    }
  }
}

/* Top level of lookup code.
   These are the procedures invoked from outside this file.
 */

long
DEFUN (Lex_Ref, (env, var),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT var)
{
  fast SCHEME_OBJECT *cell;
  SCHEME_OBJECT *hunk;

  hunk = OBJECT_ADDRESS (var);
  lookup(cell, env, hunk, repeat_lex_ref_lookup);
  return (lookup_end(cell, env, hunk));
}

long
DEFUN (Symbol_Lex_Ref, (env, sym),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym)
{
  return (deep_lookup_end(deep_lookup(env, sym, fake_variable_object),
			  fake_variable_object));
}

long
DEFUN (Lex_Set, (env, var, value),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT var
       AND SCHEME_OBJECT value)
{
  fast SCHEME_OBJECT *cell;
  SCHEME_OBJECT *hunk;

  hunk = OBJECT_ADDRESS (var);
  lookup(cell, env, hunk, repeat_lex_set_lookup);
  return (assignment_end(cell, env, hunk, value));
}

long
DEFUN (Symbol_Lex_Set, (env, sym, value),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT value)
{
  return (deep_assignment_end(deep_lookup(env, sym, fake_variable_object),
			      fake_variable_object,
			      value,
			      false));
}

long
DEFUN (Local_Set, (env, sym, value),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT value)
{
  long result;

  if (Define_Debug)
    outf_error ("\n;; Local_Set: defining %s.",
	        (STRING_LOC ((MEMORY_REF (sym, SYMBOL_NAME)), 0)));
  result = (extend_frame (env, sym, value, env, true));
  Val = sym;
  return (result);
}

long
DEFUN (safe_reference_transform, (reference_result), long reference_result)
{
  if (reference_result == ERR_UNASSIGNED_VARIABLE)
  {
    Val = UNASSIGNED_OBJECT;
    return (PRIM_DONE);
  }
  else
  {
    return (reference_result);
  }
}

long
DEFUN (safe_lex_ref, (env, var),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT var)
{
  return (safe_reference_transform (Lex_Ref (env, var)));
}

long
DEFUN (safe_symbol_lex_ref, (env, sym),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT sym)
{
  return (safe_reference_transform (Symbol_Lex_Ref (env, sym)));
}

long
DEFUN (unassigned_p_transform, (reference_result), long reference_result)
{
  switch (reference_result)
  {
    case ERR_UNASSIGNED_VARIABLE:
      Val = SHARP_T;
      return (PRIM_DONE);

    case PRIM_DONE:
      Val = SHARP_F;
      return (PRIM_DONE);

    case ERR_UNBOUND_VARIABLE:
    default:
      return (reference_result);
  }
}

extern long
  EXFUN (Symbol_Lex_unassigned_p, (SCHEME_OBJECT, SCHEME_OBJECT)),
  EXFUN (Symbol_Lex_unbound_p, (SCHEME_OBJECT, SCHEME_OBJECT));

long
DEFUN (Symbol_Lex_unassigned_p, (frame, symbol),
       SCHEME_OBJECT frame
       AND SCHEME_OBJECT symbol)
{
  return (unassigned_p_transform (Symbol_Lex_Ref (frame, symbol)));
}

long
DEFUN (Symbol_Lex_unbound_p, (frame, symbol),
       SCHEME_OBJECT frame
       AND SCHEME_OBJECT symbol)
{
  long result;

  result = (Symbol_Lex_Ref (frame, symbol));
  switch (result)
  {
    case ERR_UNASSIGNED_VARIABLE:
    case PRIM_DONE:
    {
      Val = SHARP_F;
      return (PRIM_DONE);
    }

    case ERR_UNBOUND_VARIABLE:
    {
      Val = SHARP_T;
      return (PRIM_DONE);
    }

    default:
      return (result);
  }
}

/* force_definition is used when access to the global environment is
   not allowed.  It finds the last frame where a definition can occur,
   and performs the definition in this frame.  It then returns the
   cell where the value is stored.  It's expensive and will hardly be
   used, but is provided for completeness.
*/

SCHEME_OBJECT *
DEFUN (force_definition, (env, symbol, message),
       fast SCHEME_OBJECT env
       AND SCHEME_OBJECT symbol
       AND long * message)
{
  fast SCHEME_OBJECT previous;

  if (OBJECT_TYPE (env) == GLOBAL_ENV)
  {
    *message = ERR_BAD_FRAME;
    return ((SCHEME_OBJECT *) NULL);
  }

  do
  {
    previous = env;
    env = FAST_MEMORY_REF (MEMORY_REF (env, ENVIRONMENT_FUNCTION),
			   PROCEDURE_ENVIRONMENT);
  } while (OBJECT_TYPE (env) != GLOBAL_ENV);

  *message = (Local_Set (previous, symbol, UNASSIGNED_OBJECT));
  if (*message != PRIM_DONE)
  {
    return ((SCHEME_OBJECT *) NULL);
  }
  return (deep_lookup(previous, symbol, fake_variable_object));
}

/* Macros to allow multiprocessor interlocking in
   compiler caching and recaching.

   The defaults are NOPs, but can be overriden by machine dependent
   include files or config.h
 */

#ifndef update_uuo_prolog
#define update_uuo_prolog()
#endif

#ifndef update_uuo_epilog
#define update_uuo_epilog()
#endif

#ifndef compiler_cache_prolog
#define compiler_cache_prolog()
#endif

#ifndef compiler_cache_epilog
#define compiler_cache_epilog()
#endif

#ifndef compiler_trap_prolog
#define compiler_trap_prolog()
#endif

#ifndef compiler_trap_epilog
#define compiler_trap_epilog()
#endif

#ifndef compiler_uncache_prolog
#define compiler_uncache_prolog()
#endif

#ifndef compiler_uncache_epilog
#define compiler_uncache_epilog()
#endif

#ifndef compiler_recache_prolog
#define compiler_recache_prolog()
#endif

#ifndef compiler_recache_epilog
#define compiler_recache_epilog()
#endif

/* Fast variable reference mechanism for compiled code.

   compiler_cache is the core of the variable caching mechanism.

   It creates a variable cache for the variable at the specified cell,
   if needed, and stores it or a related object in the location
   specified by (block, offset).  It adds this reference to the
   appropriate reference list for further updating.

   If the reference is a lookup reference, the cache itself is stored.

   If the reference is an assignment reference, there are two possibilities:
   - There are no operator references cached to this location.  The
   cache itself is stored.
   - There are operator references.  A fake cache (clone) is stored instead.
   This cache will make all assignments trap so that the cached
   operators can be updated.

   If the reference is an operator reference, a compiled procedure or a
   "fake" compiled procedure is stored.  Furthermore, if there were
   assignment references cached, and no fake cache had been installed,
   a fake cache is created and all the assignment references are
   updated to point to it.
 */

#ifndef PARALLEL_PROCESSOR

#define compiler_cache_consistency_check()

#else /* PARALLEL_PROCESSOR */

/* The purpose of this code is to avoid a lock gap.
   A re-definition can take place before the lock is grabbed
   and we will be caching to the wrong cell.
   To be paranoid we redo the lookup while we have the
   cell locked and confim that we still have the correct cell.

   Note that this lookup can be "shallow" since the result of
   the previous lookup is saved in my_variable.  The "shallow"
   lookup code takes care of performing a deep lookup if the
   cell has been "dangerized".
 */

#define compiler_cache_consistency_check()				\
{									\
  SCHEME_OBJECT *new_cell;						\
									\
  compiler_cache_variable[VARIABLE_SYMBOL] = name;			\
  new_cell = (lookup_cell (compiler_cache_variable, env));		\
  if (cell != new_cell)							\
  {									\
    remove_lock (set_serializer);					\
    cell = new_cell;							\
    goto compiler_cache_retry;						\
  }									\
}

#endif /* PARALLEL_PROCESSOR */

extern SCHEME_OBJECT compiler_cache_variable[];
extern long
  EXFUN (compiler_cache,
	 (SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT,
	  SCHEME_OBJECT, long, long, Boolean));

SCHEME_OBJECT compiler_cache_variable[3];

Boolean
DEFUN (local_reference_p, (env, hunk),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT * hunk)
{
  SCHEME_OBJECT spec;

  spec = (MEMORY_FETCH (hunk [VARIABLE_COMPILED_TYPE]));
  switch (OBJECT_TYPE (spec))
  {
    case GLOBAL_REF:
      return (env == (MAKE_OBJECT (GLOBAL_ENV, GO_TO_GLOBAL)));

    case LOCAL_REF:
      return (true);

    case FORMAL_REF:
    case AUX_REF:
      return ((OBJECT_DATUM (spec)) == 0);      

    default:
      return (false);
  }
}

long
DEFUN (compiler_cache,
       (cell, env, name, block, offset, kind, first_time),
       fast SCHEME_OBJECT * cell
       AND SCHEME_OBJECT env
       AND SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset
       AND long kind
       AND Boolean first_time)
{
  long EXFUN (cache_reference_end,
	      (long, SCHEME_OBJECT, SCHEME_OBJECT,
	       SCHEME_OBJECT, long, SCHEME_OBJECT));

#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer);
#endif
  fast SCHEME_OBJECT trap, references;
  SCHEME_OBJECT extension = SHARP_F;
  SCHEME_OBJECT trap_value, store_trap_tag, store_extension;
  long trap_kind, return_value;

  store_trap_tag = SHARP_F;
  store_extension = SHARP_F;
  trap_kind = TRAP_COMPILER_CACHED;

#if 0
compiler_cache_retry:
#endif

  setup_lock (set_serializer, cell);
  compiler_cache_consistency_check ();
  compiler_cache_prolog ();

  trap = *cell;
  trap_value = trap;

  if (REFERENCE_TRAP_P (trap))
  {
    long old_trap_kind;

    get_trap_kind (old_trap_kind, trap);
    switch (old_trap_kind)
    {
      case TRAP_UNASSIGNED:
      case TRAP_UNBOUND:
      case TRAP_FLUID:
	break;

      case TRAP_DANGEROUS:
        trap_value = (FAST_MEMORY_REF (trap, TRAP_EXTRA));
	trap_kind = TRAP_COMPILER_CACHED_DANGEROUS;
	break;

      case TRAP_UNASSIGNED_DANGEROUS:
	trap_value = UNASSIGNED_OBJECT;
	trap_kind = TRAP_COMPILER_CACHED_DANGEROUS;
	break;

      case TRAP_UNBOUND_DANGEROUS:
	trap_value = UNBOUND_OBJECT;
	trap_kind = TRAP_COMPILER_CACHED_DANGEROUS;
	break;

      case TRAP_FLUID_DANGEROUS:
	store_trap_tag = (LONG_TO_UNSIGNED_FIXNUM (TRAP_FLUID));
	trap_kind = TRAP_COMPILER_CACHED_DANGEROUS;
	break;

      case TRAP_COMPILER_CACHED:
      case TRAP_COMPILER_CACHED_DANGEROUS:
	extension = (FAST_MEMORY_REF (trap, TRAP_EXTRA));
	update_lock (set_serializer,
		     (MEMORY_LOC (extension, TRAP_EXTENSION_CELL)));
	trap_value = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_CELL));
	trap_kind = -1;
	break;

      default:
	compiler_cache_epilog ();
	remove_lock (set_serializer);
	return (ERR_ILLEGAL_REFERENCE_TRAP);
    }
  }

#if TRUE

  /* The code below must complete to keep the data structures consistent.
     Thus instead of checking for GC overflow at each allocation, we check
     once at the beginning for the maximum amount of space needed.  If we
     cannot do everything, we interrupt now.  Otherwise, it is assumed
     that there is enough space available.

     MAXIMUM_CACHE_SIZE must accomodate the allocation on either
     branch below, plus potential later allocation (in the form of uuo
     links).

     The current value is much larger than what is actually needed, but...
   */

#define MAXIMUM_CACHE_SIZE 40

  if (GC_allocate_test (MAXIMUM_CACHE_SIZE))
  {
    compiler_cache_epilog ();
    remove_lock (set_serializer);
    Request_GC (MAXIMUM_CACHE_SIZE);
    return (PRIM_INTERRUPT);
  }

#endif

  /* A new trap is needed.
     This code could add the new reference to the appropriate list,
     but instead leaves it to the shared code below because another
     processor may acquire the lock and change things in the middle
     of update_lock.
   */

  if (trap_kind != -1)
  {
    SCHEME_OBJECT new_trap;

#if FALSE
    /* This is included in the check above. */
    if (GC_allocate_test (9))
    {
      compiler_cache_epilog ();
      remove_lock (set_serializer);
      Request_GC (9);
      return (PRIM_INTERRUPT);
    }
#endif

    new_trap = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Free));
    *Free++ = (LONG_TO_UNSIGNED_FIXNUM (trap_kind));
    extension = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, (Free + 1)));
    *Free++ = extension;

    *Free++ = trap_value;
    *Free++ = name;
    *Free++ = SHARP_F;
    references = (MAKE_POINTER_OBJECT (TRAP_REFERENCES_TYPE, (Free + 1)));
    *Free++ = references;

    *Free++ = EMPTY_LIST;
    *Free++ = EMPTY_LIST;
    *Free++ = EMPTY_LIST;

    *cell = new_trap;		/* Do_Store_No_Lock ? */
    if (store_trap_tag != SHARP_F)
    {
      /* Do_Store_No_Lock ? */
      FAST_MEMORY_SET (trap, TRAP_TAG, store_trap_tag);
    }
    update_lock (set_serializer,
		 (MEMORY_LOC (extension, TRAP_EXTENSION_CELL)));
  }

  if (block == SHARP_F)
  {
    /* It is not really from compiled code.
       The environment linking stuff wants a cc cache instead.
     */
    compiler_cache_epilog ();
    remove_lock (set_serializer);
    return (PRIM_DONE);
  }

  /* There already is a compiled code cache.
     Maybe this should clean up all the cache lists?
   */

  {
    references = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES));

    if (((kind == TRAP_REFERENCES_ASSIGNMENT) &&
	 ((FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR))
	  != EMPTY_LIST)) ||
	((kind == TRAP_REFERENCES_OPERATOR) &&
	 ((FAST_MEMORY_REF (references, TRAP_REFERENCES_ASSIGNMENT))
	  != EMPTY_LIST)))
    {
      store_extension = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_CLONE));
      if (store_extension == SHARP_F)
      {
#if FALSE
	/* This is included in the check above. */

	if (GC_allocate_test (4))
	{
	  compiler_cache_epilog ();
	  remove_lock (set_serializer);
	  Request_GC (4);
	  return (PRIM_INTERRUPT);
	}
#endif
	store_extension = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, Free));
	*Free++ = EXPENSIVE_ASSIGNMENT_OBJECT;
	*Free++ = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_NAME));
	*Free++ = extension;
	*Free++ = references;
	FAST_MEMORY_SET (extension, TRAP_EXTENSION_CLONE, store_extension);

	if (kind == TRAP_REFERENCES_OPERATOR)
	{
	  fix_references ((MEMORY_LOC (references,
				       TRAP_REFERENCES_ASSIGNMENT)),
			  store_extension);
	}
      }
    }

    /* *UNDEFINE*: If undefine is ever implemented, we should re-think
       references by fiat since such references have constraints
       about where they can be linked to.
       For example, if C -> B -> A (-> means descends from)
       and there is a reference by fiat from C to B, and we undefine
       in B, it can go to A, but never to C (or anything between C and B).
       Curently the only references by fiat are those of the form
       ((access foo ()) ...)
     */

    return_value =
      (add_reference ((MEMORY_LOC (references, kind)),
		      block,
		      ((local_reference_p (env, compiler_cache_variable))
		       ? (MAKE_OBJECT (TC_CHARACTER, offset))
		       : (MAKE_OBJECT (TC_FIXNUM, offset)))));
    if (return_value != PRIM_DONE)
    {
      compiler_cache_epilog ();
      remove_lock (set_serializer);
      return (return_value);
    }
  }

  /* Install an extension or a uuo link in the cc block. */

  return_value = (cache_reference_end (kind, extension, store_extension,
				       block, offset, trap_value));

  /* Unlock and return */

  compiler_cache_epilog ();
  remove_lock (set_serializer);
  return (return_value);
}

long
DEFUN (cache_reference_end,
       (kind, extension, store_extension, block, offset, value),
       long kind
       AND SCHEME_OBJECT extension
       AND SCHEME_OBJECT store_extension
       AND SCHEME_OBJECT block
       AND long offset
       AND SCHEME_OBJECT value)
{
  extern void
    EXFUN (store_variable_cache, (SCHEME_OBJECT, SCHEME_OBJECT, long));
  extern long
    EXFUN (make_fake_uuo_link, (SCHEME_OBJECT, SCHEME_OBJECT, long)),
    EXFUN (make_uuo_link, (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, long));

  switch(kind)
  {
    default:
    case TRAP_REFERENCES_ASSIGNMENT:
      if (store_extension != SHARP_F)
      {
	store_variable_cache (store_extension, block, offset);
	return (PRIM_DONE);
      }
      /* Fall through */

    case TRAP_REFERENCES_LOOKUP:
      store_variable_cache (extension, block, offset);
      return (PRIM_DONE);

    case TRAP_REFERENCES_OPERATOR:
    {
      if (REFERENCE_TRAP_P (value))
      {
	return (make_fake_uuo_link (extension, block, offset));
      }
      else
      {
	return (make_uuo_link (value, extension, block, offset));
      }
    }
  }
  /*NOTREACHED*/
}

/* This procedure invokes compiler_cache after finding the top-level
   value cell associated with (env, name).
 */

long
DEFUN (compiler_cache_reference,
       (env, name, block, offset, kind, first_time),
       SCHEME_OBJECT env
       AND SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset
       AND long kind
       AND Boolean first_time)
{
  SCHEME_OBJECT *cell;

  cell = (deep_lookup (env, name, compiler_cache_variable));
  if (cell == unbound_trap_object)
  {
    long message;

    cell = (force_definition (env, name, &message));
    if (message != PRIM_DONE)
    {
      return (message);
    }
  }
  return (compiler_cache (cell, env, name, block, offset, kind, first_time));
}

/* This procedure updates all the references in the cached reference
   list pointed at by slot to hold value.  It also eliminates "empty"
   pairs (pairs whose weakly held block has vanished).
 */

static void
DEFUN (fix_references, (slot, extension),
       fast SCHEME_OBJECT * slot
       AND fast SCHEME_OBJECT extension)
{
  fast SCHEME_OBJECT pair, block;

  while (*slot != EMPTY_LIST)
  {
    pair = (FAST_PAIR_CAR (*slot));
    block = (FAST_PAIR_CAR (pair));
    if (block == SHARP_F)
    {
      *slot = (FAST_PAIR_CDR (*slot));
    }
    else
    {
      extern void
	EXFUN (store_variable_cache, (SCHEME_OBJECT, SCHEME_OBJECT, long));

      store_variable_cache (extension,
			    block,
			    (OBJECT_DATUM (FAST_PAIR_CDR (pair))));
      slot = (PAIR_CDR_LOC (*slot));
    }
  }
  return;
}

/* This procedures adds a new cached reference to the cached reference
   list pointed at by slot.  It attempts to reuse pairs which have been
   "emptied" by the garbage collector.
 */

static long
DEFUN (add_reference, (slot, block, offset),
       fast SCHEME_OBJECT * slot
       AND SCHEME_OBJECT block
       AND SCHEME_OBJECT offset)
{
  fast SCHEME_OBJECT pair;

  while (*slot != EMPTY_LIST)
  {
    pair = (FAST_PAIR_CAR (*slot));
    if ((FAST_PAIR_CAR (pair)) == SHARP_F)
    {
      FAST_SET_PAIR_CAR (pair, block);
      FAST_SET_PAIR_CDR (pair, offset);
      return (PRIM_DONE);
    }
    slot = (PAIR_CDR_LOC (*slot));
  }

  if (GC_allocate_test (4))
  {
    Request_GC (4);
    return (PRIM_INTERRUPT);
  }

  *slot = (MAKE_POINTER_OBJECT (TC_LIST, Free));
  *Free = (MAKE_POINTER_OBJECT (TC_WEAK_CONS, (Free + 2)));
  Free += 1;
  *Free++ = EMPTY_LIST;

  *Free++ = block;
  *Free++ = offset;

  return (PRIM_DONE);
}

extern SCHEME_OBJECT
  EXFUN (compiled_block_environment, (SCHEME_OBJECT));

static long
  trap_map_table[] = {
    TRAP_REFERENCES_LOOKUP,
    TRAP_REFERENCES_ASSIGNMENT,
    TRAP_REFERENCES_OPERATOR
    };

#define TRAP_MAP_TABLE_SIZE (sizeof(trap_map_table) / sizeof(long))

#ifndef DEFINITION_RECACHES_EAGERLY

/* compiler_uncache_slot uncaches all references in the list pointed
   at by slot, and clears the list.  If the references are operator
   references, a fake compiled procedure which will recache when
   invoked is created and installed.
 */

long
DEFUN (compiler_uncache_slot, (slot, sym, kind),
       fast SCHEME_OBJECT * slot
       AND SCHEME_OBJECT sym
       AND long kind)
{
  fast SCHEME_OBJECT temp, pair;
  SCHEME_OBJECT block, offset, new_extension;

  for (temp = *slot; temp != EMPTY_LIST; temp = *slot)
  {
    pair = (FAST_PAIR_CAR (temp));
    block = (FAST_PAIR_CAR (pair));
    if (block != SHARP_F)
    {
      offset = (FAST_PAIR_CDR (pair));
      if (CHARACTER_P (offset))
      {
	/* This reference really belongs here! -- do not uncache.
	   Skip to next.
	 */

	slot = (PAIR_CDR_LOC (temp));
	continue;
      }
      else
      {
	if (GC_allocate_test (4))
	{
	  Request_GC (4);
	  return (PRIM_INTERRUPT);
	}
	new_extension = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, Free));
	*Free++ = REQUEST_RECACHE_OBJECT;
	*Free++ = sym;
	*Free++ = block;
	*Free++ = offset;

	if (kind == TRAP_REFERENCES_OPERATOR)
	{
	  extern long
	    EXFUN (make_fake_uuo_link, (SCHEME_OBJECT, SCHEME_OBJECT, long));
	  long result;

	  result = (make_fake_uuo_link (new_extension,
					block,
					(OBJECT_DATUM (offset))));
	  if (result != PRIM_DONE)
	    return (result);
	}
	else
	{
	  extern void
	    EXFUN (store_variable_cache, (SCHEME_OBJECT, SCHEME_OBJECT, long));

	  store_variable_cache (new_extension, block, (OBJECT_DATUM (offset)));
	}
      }
    }
    *slot = (FAST_PAIR_CDR (temp));
  }
  return (PRIM_DONE);
}

/* compiler_uncache is invoked when a redefinition occurs.
   It uncaches all references cached to this value cell, and
   sets the variables up to be recached at the next reference.
   value_cell is the value cell being shadowed.
   sym is the name of the variable.
 */

long
DEFUN (compiler_uncache, (value_cell, sym),
       SCHEME_OBJECT * value_cell
       AND SCHEME_OBJECT sym)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer);
#endif
  SCHEME_OBJECT val, extension, references;
  long trap_kind, temp, i, index;

  setup_lock (set_serializer, value_cell);

  val = *value_cell;

  if (!(REFERENCE_TRAP_P (val)))
  {
    remove_lock (set_serializer);
    return (PRIM_DONE);
  }

  get_trap_kind (trap_kind, val);
  if ((trap_kind != TRAP_COMPILER_CACHED) &&
      (trap_kind != TRAP_COMPILER_CACHED_DANGEROUS))
  {
    remove_lock (set_serializer);
    return (PRIM_DONE);
  }

  compiler_uncache_prolog ();

  extension = (FAST_MEMORY_REF (val, TRAP_EXTRA));
  references = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES));
  update_lock (set_serializer, (MEMORY_LOC (extension, TRAP_EXTENSION_CELL)));

  /* Uncache all of the lists. */

  for (i = TRAP_MAP_TABLE_SIZE; --i >= 0; )
  {
    index = trap_map_table[i];
    temp = (compiler_uncache_slot ((MEMORY_LOC (references, index)),
				   sym, index));
    if (temp != PRIM_DONE)
    {
      remove_lock (set_serializer);
      compiler_uncache_epilog ();
      return (temp);
    }
  }

  /* Note that we can only remove the trap if no references remain,
     ie. if there were no hard-wired references to this frame.
     We can test that by checking whether all the slots were set
     to EMPTY_LIST in the preceding loop.
     The current code, however, never removes the trap.
   */

  /* Remove the clone extension if there is one and it is no longer needed. */

  if ((FAST_MEMORY_REF (extension, TRAP_EXTENSION_CLONE)) != SHARP_F)
  {
    if ((FAST_MEMORY_REF (references, TRAP_REFERENCES_ASSIGNMENT))
	== EMPTY_LIST)
    {
      FAST_MEMORY_SET (extension, TRAP_EXTENSION_CLONE, SHARP_F);
    }
    else if ((FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR))
	     == EMPTY_LIST)
    {
      /* All operators have disappeared, we can remove the clone,
	 but we must update the cells.
       */
      fix_references ((MEMORY_LOC (references, TRAP_REFERENCES_ASSIGNMENT)),
		      extension);
      FAST_MEMORY_SET (extension, TRAP_EXTENSION_CLONE, SHARP_F);
    }
  }
  compiler_uncache_epilog ();
  remove_lock (set_serializer);
  return (PRIM_DONE);
}

#endif /* DEFINITION_RECACHES_EAGERLY */

#ifdef DEFINITION_RECACHES_EAGERLY

/*
   compiler_recache is invoked when a redefinition occurs.  It
   recaches (at the definition point) all the references that need to
   point to the new cell.

   It does this in two phases:

   - First (by means of compiler_recache_split) it splits all
   references into those that need to be updated and those that do
   not.  This is done by side-effecting the list so that all those
   that need updating are at the end, and when we actually decide to
   go ahead, we can just clip it and install it in the new location.
   compiler_recache_split also counts how many entries are affected,
   so the total amount of gc space needed can be computed.

   - After checking that there is enough space to proceed, (rather
   than aborting) it actually does the recaching.  It caches to the
   new location/value by using compiler_recache_slot.  Note that the
   eventual trap extension has already been allocated so the recached
   links can point to it.
 */

/* Required by compiler_uncache macro. */

SCHEME_OBJECT *shadowed_value_cell = ((SCHEME_OBJECT *) NULL);

/* Each extension is a hunk4. */

#define SPACE_PER_EXTENSION	4

/* Trap, extension, and one cache-list hunk. */

#define SPACE_PER_TRAP		(2 + SPACE_PER_EXTENSION + 3)

/* 1 Pair and 1 Weak pair.
   Not really needed since the pairs and weak pairs are reused.
 */

#define SPACE_PER_ENTRY		(2 + 2)

/* Hopefully a conservative guesstimate. */

#ifndef SPACE_PER_LINK		/* So it can be overriden from config.h */
#define SPACE_PER_LINK		10
#endif

/* The spaces are 0 because the pairs are reused!  If that ever changes,
   they should all become SPACE_PER_ENTRY + curent value.
 */

#define SPACE_PER_LOOKUP	0
#define SPACE_PER_ASSIGNMENT	0
#define SPACE_PER_OPERATOR	(0 + SPACE_PER_LINK)

static long
  trap_size_table[TRAP_MAP_TABLE_SIZE] = {
    SPACE_PER_LOOKUP,
    SPACE_PER_ASSIGNMENT,
    SPACE_PER_OPERATOR
    };

static long
  trap_conflict_table[TRAP_MAP_TABLE_SIZE] = {
    0,				/* lookup */
    1,				/* assignment */
    1				/* operator */
    };

Boolean
DEFUN (environment_ancestor_or_self_p, (ancestor, descendant),
       fast SCHEME_OBJECT ancestor
       AND fast SCHEME_OBJECT descendant)
{
  while ((OBJECT_TYPE (descendant)) != GLOBAL_ENV)
  {
    if (descendant == ancestor)
      return (true);
    descendant = (FAST_MEMORY_REF ((MEMORY_REF (descendant,
						ENVIRONMENT_FUNCTION)),
				   PROCEDURE_ENVIRONMENT));
  }
  return (descendant == ancestor);
}

/* This reorders the entries in slot so that the entries that are
   not affected by the redefinition appear first, and the affected
   ones appear last.  A pointer to the first affected cell is stored
   in memoize_cell, and this will be given to compiler_recache_slot
   in order to avoid recomputing the division.

   Note: There is an implicit assumption throughout that none of the
   pairs (or weak pairs) are in pure space.  If they are, they cannot
   be sorted or reused.
 */

long
DEFUN (compiler_recache_split,
       (slot, sym, definition_env, memoize_cell, link_p),
       fast SCHEME_OBJECT * slot
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT definition_env
       AND SCHEME_OBJECT ** memoize_cell
       AND Boolean link_p)
{
  fast long count;
  SCHEME_OBJECT weak_pair, block, reference_env, invalid_head;
  fast SCHEME_OBJECT *last_invalid;

  count = 0;
  last_invalid = &invalid_head;

  while (*slot != EMPTY_LIST)
  {
    weak_pair = (FAST_PAIR_CAR (*slot));
    block = (FAST_PAIR_CAR (weak_pair));
    if (block == SHARP_F)
    {
      *slot = (FAST_PAIR_CDR (*slot));
      continue;
    }
    if (!link_p && (CHARACTER_P (FAST_PAIR_CDR (weak_pair))))
    {
      /* The reference really belongs here -- it is not affected by fiat. */
      slot = (PAIR_CDR_LOC (*slot));
    }
    else
    {
      reference_env = (compiled_block_environment (block));
      if (!environment_ancestor_or_self_p (definition_env, reference_env))
      {
	slot = (PAIR_CDR_LOC (*slot));
      }
      else
      {
	count += 1;
	*last_invalid = *slot;
	last_invalid = (PAIR_CDR_LOC (*slot));
	*slot = *last_invalid;
      }
    }
  }
  *last_invalid = EMPTY_LIST;
  *memoize_cell = slot;
  *slot = invalid_head;
  return (count);
}

/* This recaches the entries pointed out by cell and adds them
   to the list in slot.  It also sets to #F the contents
   of cell.

   Note that this reuses the pairs and weak pairs that used to be
   in cell.
 */

long
DEFUN (compiler_recache_slot,
       (extension, sym, kind, slot, cell, value),
       SCHEME_OBJECT extension
       AND SCHEME_OBJECT sym
       AND long kind
       AND fast SCHEME_OBJECT * slot
       AND fast SCHEME_OBJECT * cell
       AND SCHEME_OBJECT value)
{
  fast SCHEME_OBJECT pair, weak_pair;
  SCHEME_OBJECT clone, tail;
  long result;

  /* This is #F if there isn't one.
     This makes cache_reference_end do the right thing.
   */
  clone = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_CLONE));
  tail = * slot;

  for (pair = (* cell); pair != EMPTY_LIST; pair = (* cell))
  {
    weak_pair = (FAST_PAIR_CAR (pair));
    result = (cache_reference_end (kind, extension, clone,
				   (FAST_PAIR_CAR (weak_pair)),
				   (OBJECT_DATUM (FAST_PAIR_CDR (weak_pair))),
				   value));
    if (result != PRIM_DONE)
    {
      /* We are severely screwed.
	 compiler_recache will do the appropriate thing.
       */
      *slot = tail;
      return (result);
    }

    * slot = pair;
    slot = (PAIR_CDR_LOC (pair));
    * cell = * slot;
  }
  * slot = tail;
  return (PRIM_DONE);
}

long
DEFUN (compiler_recache,
       (old_value_cell, new_value_cell, env, sym, value, shadowed_p, link_p),
       SCHEME_OBJECT * old_value_cell
       AND SCHEME_OBJECT * new_value_cell
       AND SCHEME_OBJECT env
       AND SCHEME_OBJECT sym
       AND SCHEME_OBJECT value
       AND Boolean shadowed_p
       AND Boolean link_p)
{
#ifdef DECLARE_LOCK
  DECLARE_LOCK (set_serializer_1);
  DECLARE_LOCK (set_serializer_2);
#endif
  SCHEME_OBJECT
    old_value, references, extension, new_extension,
    *trap_info_table[TRAP_MAP_TABLE_SIZE];
  SCHEME_OBJECT new_trap = SHARP_F;
  long
    trap_kind, temp, i, index, total_size, total_count, conflict_count;

  setup_locks (set_serializer_1, old_value_cell,
	       set_serializer_2, new_value_cell);

  if ((!link_p) && (*new_value_cell != DANGEROUS_UNBOUND_OBJECT))
  {
    /* Another processor has redefined this word in the meantime.
       The other processor must have recached all the compiled code
       caches since it is shadowing the same variable.
       The definition has become a redefinition.
     */
    remove_locks (set_serializer_1, set_serializer_2);
    return (redefinition (new_value_cell, value));
  }

  old_value = *old_value_cell;

  if (!(REFERENCE_TRAP_P (old_value)))
  {
    remove_locks (set_serializer_1, set_serializer_2);
    return (link_p ?
	    PRIM_DONE :
	    (definition (new_value_cell, value, shadowed_p)));
  }

  get_trap_kind (trap_kind, old_value);
  if ((trap_kind != TRAP_COMPILER_CACHED) &&
      (trap_kind != TRAP_COMPILER_CACHED_DANGEROUS))
  {
    remove_locks (set_serializer_1, set_serializer_2);
    return (link_p ?
	    PRIM_DONE :
	    (definition (new_value_cell, value, shadowed_p)));
  }

  compiler_recache_prolog ();

  extension = (FAST_MEMORY_REF (old_value, TRAP_EXTRA));
  references = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES));
  update_lock (set_serializer_1,
	       (MEMORY_LOC (extension, TRAP_EXTENSION_CELL)));

  /*
     Split each slot and compute the amount to allocate.
   */

  conflict_count = 0;
  total_size = (link_p ? 0 : SPACE_PER_TRAP);
  total_count = 0;

  for (i = TRAP_MAP_TABLE_SIZE; --i >= 0; )
  {
    index = trap_map_table[i];
    temp = compiler_recache_split ((MEMORY_LOC (references, index)),
				   sym, env, &trap_info_table[i], link_p);

    if (temp != 0)
    {
      conflict_count += trap_conflict_table[i];
      total_size += (temp * trap_size_table[i]);
      total_count += temp;
    }
  }

  if (total_count == 0)
  {
    compiler_recache_epilog ();
    remove_locks (set_serializer_1, set_serializer_2);
    return (link_p ?
	    PRIM_DONE :
	    (definition (new_value_cell, value, shadowed_p)));
  }

  if ((conflict_count == 2) &&
      ((!link_p) ||
       (new_value_cell[TRAP_EXTENSION_CLONE] == SHARP_F)))
  {
    total_size += SPACE_PER_EXTENSION;
  }

  if (GC_allocate_test (total_size))
  {
    /* Unfortunate fact of life: This binding will be dangerous
       even if there is no need, but this is the only way to
       guarantee consistent values.
     */
    compiler_recache_epilog ();
    remove_locks (set_serializer_1, set_serializer_2);
    Request_GC (total_size);
    return (PRIM_INTERRUPT);
  }

  /*
     Allocate and initialize all the cache structures if necessary.
   */

  if (link_p)
  {
    new_extension = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, new_value_cell));
    references = new_value_cell[TRAP_EXTENSION_REFERENCES];
  }
  else
  {
    /* The reference trap is created here, but is not installed in the
       environment structure until the end.  The new binding contains
       a DANGEROUS_UNBOUND_OBJECT so that other parallel lookups will
       skip this binding.
     */

    references = (MAKE_POINTER_OBJECT (TRAP_REFERENCES_TYPE, Free));

    *Free++ = EMPTY_LIST;
    *Free++ = EMPTY_LIST;
    *Free++ = EMPTY_LIST;

    new_extension = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, Free));

    *Free++ = value;
    *Free++ = sym;
    *Free++ = SHARP_F;
    *Free++ = references;

    new_trap = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Free));
    *Free++ = (LONG_TO_UNSIGNED_FIXNUM ((shadowed_p ?
					 TRAP_COMPILER_CACHED_DANGEROUS :
					 TRAP_COMPILER_CACHED)));
    *Free++ = new_extension;
  }

  if ((conflict_count == 2) &&
      (MEMORY_REF (new_extension, TRAP_EXTENSION_CLONE) == SHARP_F))
  {
    SCHEME_OBJECT clone;

    clone = (MAKE_POINTER_OBJECT (TRAP_EXTENSION_TYPE, Free));

    *Free++ = EXPENSIVE_ASSIGNMENT_OBJECT;
    *Free++ = sym;
    *Free++ = new_extension;
    *Free++ = references;
    FAST_MEMORY_SET (new_extension, TRAP_EXTENSION_CLONE, clone);
  }

  /*
     Now we actually perform the recaching, allocating freely.
   */

  for (i = TRAP_MAP_TABLE_SIZE; --i >= 0; )
  {
    index = trap_map_table[i];
    temp = (compiler_recache_slot (new_extension, sym, index,
				   (MEMORY_LOC (references, index)),
				   trap_info_table[i],
				   value));
    if (temp != PRIM_DONE)
    {
      extern char *Abort_Names[];

      /* We've lost BIG. */

      if (temp == PRIM_INTERRUPT)
	outf_fatal ("\ncompiler_recache: Ran out of guaranteed space!\n");
      else if (temp > 0)
	outf_fatal ("\ncompiler_recache: Unexpected error value %d (%s)\n",
		    temp, Abort_Names[temp]);
      else
	outf_fatal ("\ncompiler_recache: Unexpected abort value %d (%s)\n",
		    -temp, Abort_Names[(-temp) - 1]);
      Microcode_Termination (TERM_EXIT);
    }
  }

  if (!link_p)
  {
    *new_value_cell = new_trap;
  }
  compiler_recache_epilog ();
  remove_locks (set_serializer_1, set_serializer_2);
  return (PRIM_DONE);
}

#endif /* DEFINITION_RECACHES_EAGERLY */

/* recache_uuo_links is invoked when an assignment occurs to a
   variable which has cached operator references (uuo links).
   All the operator references must be recached to the new value.

   It currently potentially creates a new uuo link per operator
   reference.  This may be very expensive in space, but allows a great
   deal of flexibility.  It is ultimately necessary if there is hidden
   information on each call (like arity, types of arguments, etc.).
 */

long
DEFUN (recache_uuo_links, (extension, old_value),
       SCHEME_OBJECT extension
       AND SCHEME_OBJECT old_value)
{
  long EXFUN (update_uuo_links,
	      (SCHEME_OBJECT, SCHEME_OBJECT,
	       long ((*)(SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, long))));

  SCHEME_OBJECT value;
  long return_value;

  value = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_CELL));
  if (REFERENCE_TRAP_P (value))
  {
    if (REFERENCE_TRAP_P (old_value))
    {
      /* No need to do anything.
	 The uuo links are in the correct state.
       */

      return_value = PRIM_DONE;
    }
    else
    {
      long EXFUN (make_recache_uuo_link,
		  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, long));

      return_value =
	update_uuo_links (value, extension, make_recache_uuo_link);
    }
  }
  else
  {
    extern long
      EXFUN (make_uuo_link,
	     (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, long));

    return_value =
      update_uuo_links (value, extension, make_uuo_link);
  }

  if (return_value != PRIM_DONE)
  {
    /*
       This reverts the variable's value to the original value except
       when the value was fluid bound.  In the latter case, it does
       not matter, it should still work: When the assignment is
       restarted, and recache_uuo_links is restarted, the relative
       "trapness" of both old and new values should be unchanged.

       Note that recache_uuo_links is invoked with the cell locked,
       so it is safe to "revert" the value.
     */

    FAST_MEMORY_SET (extension, TRAP_EXTENSION_CELL, old_value);
  }
  return (return_value);
}

/* This kludge is due to the lack of closures. */

long
DEFUN (make_recache_uuo_link, (value, extension, block, offset),
       SCHEME_OBJECT value
       AND SCHEME_OBJECT extension
       AND SCHEME_OBJECT block
       AND long offset)
{
  extern long
    EXFUN (make_fake_uuo_link, (SCHEME_OBJECT, SCHEME_OBJECT, long));

  return (make_fake_uuo_link (extension, block, offset));
}

long
DEFUN (update_uuo_links,
       (value, extension, handler),
       SCHEME_OBJECT value
       AND SCHEME_OBJECT extension
       AND long EXFUN ((*handler),
		       (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, long)))
{
  SCHEME_OBJECT references, pair, block;
  fast SCHEME_OBJECT *slot;
  long return_value;

  update_uuo_prolog();
  references = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES));
  slot = (MEMORY_LOC (references, TRAP_REFERENCES_OPERATOR));

  while (*slot != EMPTY_LIST)
  {
    pair = (FAST_PAIR_CAR (*slot));
    block = (FAST_PAIR_CAR (pair));
    if (block == SHARP_F)
    {
      *slot = (FAST_PAIR_CDR (*slot));
    }
    else
    {
      return_value =
	(*handler)(value, extension, block,
		   (OBJECT_DATUM (FAST_PAIR_CDR (pair))));
      if (return_value != PRIM_DONE)
      {
	update_uuo_epilog ();
	return (return_value);
      }
      slot = (PAIR_CDR_LOC (*slot));
    }
  }

  /* If there are no uuo links left, and there is an extension clone,
     remove it, and make assignment references point to the real value
     cell.
   */

  if ((FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR) == EMPTY_LIST) &&
      (FAST_MEMORY_REF (extension, TRAP_EXTENSION_CLONE) != SHARP_F))
  {
    FAST_MEMORY_SET (extension, TRAP_EXTENSION_CLONE, SHARP_F);
    fix_references ((MEMORY_LOC (references, TRAP_REFERENCES_ASSIGNMENT)),
		    extension);
  }
  update_uuo_epilog ();
  return (PRIM_DONE);
}

/* compiler_reference_trap is called when a reference occurs to a compiled
   reference cache which contains a reference trap.  If the trap is
   the special REQUEST_RECACHE_OBJECT, the reference is recached.
   Otherwise the reference is done normally, and the process continued.
 */

long
DEFUN (compiler_reference_trap, (extension, kind, handler),
       SCHEME_OBJECT extension
       AND long kind
       AND long EXFUN ((*handler),(SCHEME_OBJECT *, SCHEME_OBJECT *)))
{
  long offset, temp;
  SCHEME_OBJECT block;

try_again:

  if ((MEMORY_REF (extension, TRAP_EXTENSION_CELL)) != REQUEST_RECACHE_OBJECT)
  {
    return ((*handler) (MEMORY_LOC (extension, TRAP_EXTENSION_CELL),
			fake_variable_object));
  }

  block = (FAST_MEMORY_REF (extension, TRAP_EXTENSION_BLOCK));
  offset = (OBJECT_DATUM (FAST_MEMORY_REF (extension, TRAP_EXTENSION_OFFSET)));

  compiler_trap_prolog ();
  temp =
    (compiler_cache_reference ((compiled_block_environment (block)),
			       (FAST_MEMORY_REF (extension,
						 TRAP_EXTENSION_NAME)),
			       block, offset, kind, false));
  compiler_trap_epilog ();
  if (temp != PRIM_DONE)
  {
    return (temp);
  }

  switch (kind)
  {
    case TRAP_REFERENCES_OPERATOR:
    {

      /* Note that this value may cause another operator trap when
	 invoked, since it may be a uuo-link to an interpreted
	 procedure, or to a variable with a trap in it.  However, it
	 should not go into a loop because the reference should be
	 cached to the correct place, so the extension will no longer
	 have a REQUEST_RECACHE_OBJECT in it.  The first branch in
	 this procedure will be taken in this case.  On a
	 multiprocessor it may in fact loop if some other processor
	 redefines the variable before we have a chance to invoke the
	 value.
       */

      extern SCHEME_OBJECT
	EXFUN (extract_uuo_link, (SCHEME_OBJECT, long));

      Val = (extract_uuo_link (block, offset));
      return (PRIM_DONE);
    }

    case TRAP_REFERENCES_ASSIGNMENT:
    case TRAP_REFERENCES_LOOKUP:
    default:
    {
      extern SCHEME_OBJECT
	EXFUN (extract_variable_cache, (SCHEME_OBJECT, long));

      extension = (extract_variable_cache (block, offset));
      /* This is paranoid on a single processor, but it does not hurt.
	 On a multiprocessor, we need to do it because some other processor
	 may have redefined this variable in the meantime.
       */
      goto try_again;
    }
  }
}

/* Procedures invoked from the compiled code interface. */

extern long
  EXFUN (compiler_cache_lookup, (SCHEME_OBJECT, SCHEME_OBJECT, long)),
  EXFUN (compiler_cache_assignment, (SCHEME_OBJECT, SCHEME_OBJECT, long)),
  EXFUN (compiler_cache_operator, (SCHEME_OBJECT, SCHEME_OBJECT, long)),
  EXFUN (compiler_cache_global_operator, (SCHEME_OBJECT, SCHEME_OBJECT, long));

long
DEFUN (compiler_cache_lookup, (name, block, offset),
       SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset)
{
  return (compiler_cache_reference ((compiled_block_environment (block)),
				    name, block, offset,
				    TRAP_REFERENCES_LOOKUP, true));
}

long
DEFUN (compiler_cache_assignment, (name, block, offset),
       SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset)
{
  return (compiler_cache_reference ((compiled_block_environment (block)),
				    name, block, offset,
				    TRAP_REFERENCES_ASSIGNMENT, true));
}

long
DEFUN (compiler_cache_operator, (name, block, offset),
       SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset)
{
  return (compiler_cache_reference ((compiled_block_environment (block)),
				    name, block, offset,
				    TRAP_REFERENCES_OPERATOR, true));
}

long
DEFUN (compiler_cache_global_operator, (name, block, offset),
       SCHEME_OBJECT name
       AND SCHEME_OBJECT block
       AND long offset)
{
  return (compiler_cache_reference ((MAKE_OBJECT (GLOBAL_ENV, GO_TO_GLOBAL)),
				    name, block, offset,
				    TRAP_REFERENCES_OPERATOR, true));
}

extern long
  EXFUN (complr_operator_reference_trap, (SCHEME_OBJECT *, SCHEME_OBJECT));

extern SCHEME_OBJECT
  EXFUN (compiler_var_error, (SCHEME_OBJECT, SCHEME_OBJECT));

long
DEFUN (complr_operator_reference_trap, (frame_slot, extension),
       SCHEME_OBJECT * frame_slot
       AND SCHEME_OBJECT extension)
{
  long temp;

  temp = (compiler_reference_trap (extension,
				   TRAP_REFERENCES_OPERATOR,
				   deep_lookup_end));
  if (temp != PRIM_DONE)
  {
    return temp;
  }
  *frame_slot = Val;
  return (PRIM_DONE);
}

SCHEME_OBJECT
DEFUN (compiler_var_error, (extension, environment),
       SCHEME_OBJECT extension
       AND SCHEME_OBJECT environment)
{
  return (MEMORY_REF (extension, TRAP_EXTENSION_NAME));
}

/* Utility for compiler_assignment_trap, below.
   Necessary because C lacks lambda.  Argh!
 */

static SCHEME_OBJECT saved_compiler_assignment_value;

long
DEFUN (compiler_assignment_end, (cell, hunk),
       SCHEME_OBJECT * cell
       AND SCHEME_OBJECT * hunk)
{
  return (deep_assignment_end (cell, hunk,
			       saved_compiler_assignment_value, false));
}

/* More compiled code interface procedures */

extern long
  EXFUN (compiler_lookup_trap, (SCHEME_OBJECT)),
  EXFUN (compiler_safe_lookup_trap, (SCHEME_OBJECT)),
  EXFUN (compiler_unassigned_p_trap, (SCHEME_OBJECT)),
  EXFUN (compiler_assignment_trap, (SCHEME_OBJECT, SCHEME_OBJECT));

long
DEFUN (compiler_lookup_trap, (extension), SCHEME_OBJECT extension)
{
  return (compiler_reference_trap (extension,
				   TRAP_REFERENCES_LOOKUP,
				   deep_lookup_end));
}

long
DEFUN (compiler_safe_lookup_trap, (extension), SCHEME_OBJECT extension)
{
  return (safe_reference_transform (compiler_lookup_trap (extension)));
}

long
DEFUN (compiler_unassigned_p_trap, (extension), SCHEME_OBJECT extension)
{
  return (unassigned_p_transform (compiler_lookup_trap (extension)));
}

long
DEFUN (compiler_assignment_trap, (extension, value),
       SCHEME_OBJECT extension
       AND SCHEME_OBJECT value)
{
  saved_compiler_assignment_value = value;
  return (compiler_reference_trap (extension,
				   TRAP_REFERENCES_ASSIGNMENT,
				   compiler_assignment_end));
}
