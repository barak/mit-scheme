/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/lookup.c,v 9.35 1987/10/05 19:25:33 jinx Exp $
 *
 * This file contains symbol lookup and modification routines.  See
 * Hal Abelson for a paper describing and justifying the algorithm.
 *
 * The implementation is vastly different, but the concepts are the same.
 */

#include "scheme.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"
#include "primitive.h"

/* NOTE:
   Although this code has been parallelized, it has not been
   exhaustively tried on a parallel processor.  There are probably
   various race conditions that have to be thought about carefully.
 */

/* Useful constants. */

/* This is returned by various procedures to cause a Scheme
   unbound variable error to be signalled. 
 */

Pointer unbound_trap_object[] = { UNBOUND_OBJECT };

/* This is returned by lookup to force a deep lookup when the variable
   needs to be recompiled.
 */

Pointer uncompiled_trap_object[] = { DANGEROUS_UNBOUND_OBJECT };

/* This is returned by lookup to cause a Scheme broken compiled
   variable error to be signalled.
 */

Pointer illegal_trap_object[] = { ILLEGAL_OBJECT };

/* This is passed to deep_lookup as the variable to compile when
   we don't really have a variable.
 */

Pointer fake_variable_object[3];

/* The lexical lookup procedure.
   deep_lookup searches env for an occurrence of sym.  When it finds
   it, it stores into hunk the path by which it was found, so that
   future references do not spend the time to find it again.
   It returns a pointer to the value cell, or a bogus value cell if 
   the variable was unbound.
 */

Pointer *
deep_lookup(env, sym, hunk)
     Pointer env, sym, *hunk;
{
  Lock_Handle compile_serializer;
  fast Pointer frame, *scan;
  fast long depth;

  for (depth = 0, frame = env;
       OBJECT_TYPE(frame) != GLOBAL_ENV;
       depth += 1,
       frame = Fast_Vector_Ref(Vector_Ref(frame, ENVIRONMENT_FUNCTION),
			       PROCEDURE_ENVIRONMENT))
  {
    fast Pointer temp;

    temp = Vector_Ref(frame, ENVIRONMENT_FUNCTION);

    if (OBJECT_TYPE(temp) == AUX_LIST_TYPE)
    {
      /* Search for an auxiliary binding. */

      fast long count;
      Pointer *start;

      scan = Get_Pointer(temp);
      start = scan;
      count = Lexical_Offset(scan[AUX_LIST_COUNT]);
      scan += AUX_LIST_FIRST;

      while (--count >= 0)
      {
	if (Fast_Vector_Ref(*scan, CONS_CAR) == sym)
	{
	  Pointer *cell;

	  cell = Nth_Vector_Loc(*scan, CONS_CDR);
	  if (Fetch(cell[0]) == DANGEROUS_UNBOUND_OBJECT)
	  {
	    /* A dangerous unbound object signals that
	       a definition here must become dangerous,
	       but is not a real bining.
	     */
	    goto do_next_frame;
	  }
	  setup_lock(compile_serializer, hunk);
	  hunk[VARIABLE_COMPILED_TYPE] = Make_Non_Pointer(AUX_REF, depth);
	  hunk[VARIABLE_OFFSET] = Make_Local_Offset(scan - start);
	  remove_lock(compile_serializer);
	  return cell;
	}
	scan += 1;  
      }
      temp = Vector_Ref(temp, ENV_EXTENSION_PROCEDURE);
    }

    {
      /* Search for a formal parameter. */

      fast long count;

      temp = Fast_Vector_Ref(Fast_Vector_Ref(temp, PROCEDURE_LAMBDA_EXPR),
			     LAMBDA_FORMALS);
      for (count = Vector_Length(temp) - 1,
	   scan = Nth_Vector_Loc(temp, VECTOR_DATA + 1);
	   count > 0;
	   count -= 1,
	   scan += 1)
	if (*scan == sym)
	{
	  long offset;

	  offset = 1 + Vector_Length(temp) - count;

	  setup_lock(compile_serializer, hunk);
	  if (depth != 0)
	  {
	    hunk[VARIABLE_COMPILED_TYPE] = Make_Non_Pointer(FORMAL_REF, depth);
	    hunk[VARIABLE_OFFSET] = Make_Local_Offset(offset);
	  }
	  else
	  {
	    hunk[VARIABLE_COMPILED_TYPE] = Make_Local_Offset(offset);
	    hunk[VARIABLE_OFFSET] = NIL;
	  }
	  remove_lock(compile_serializer);

	  return Nth_Vector_Loc(frame, offset);
	}
    }

do_next_frame:
    continue;
  }
  /* The reference is global. */

  if (OBJECT_DATUM(frame) != GO_TO_GLOBAL)
  {
    return unbound_trap_object;
  }

  setup_lock(compile_serializer, hunk);
  hunk[VARIABLE_COMPILED_TYPE] = Make_New_Pointer(TC_UNINTERNED_SYMBOL, sym);
  hunk[VARIABLE_OFFSET] = NIL;
  remove_lock(compile_serializer);

  return Nth_Vector_Loc(sym, SYMBOL_GLOBAL_VALUE);
}

/* Full lookup end code.
   deep_lookup_end handles all the complicated and dangerous cases.
   cell is the value cell (supposedly found by deep_lookup).  Hunk is
   the address of the scode variable object which may need to be
   recompiled if the reference is dangerous.
 */

long
deep_lookup_end(cell, hunk)
	Pointer *cell;
	Pointer *hunk;
{
  long trap_kind, return_value;
  Boolean repeat_p;

  do {
    repeat_p = false;
    Val = Fetch(cell[0]);
    FUTURE_VARIABLE_SPLICE (((Pointer) cell), 0, Val);
    if (!(REFERENCE_TRAP_P(Val)))
    {
      return PRIM_DONE;
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
	return ERR_UNASSIGNED_VARIABLE;

      case TRAP_UNASSIGNED_DANGEROUS:
	return_value = ERR_UNASSIGNED_VARIABLE;
	break;

      case TRAP_DANGEROUS:
	{
	  Pointer trap_value;

	  trap_value = Val;
	  Val = (Vector_Ref (trap_value, TRAP_EXTRA));
	  FUTURE_VARIABLE_SPLICE (trap_value, TRAP_EXTRA, Val);
	}
	return_value = PRIM_DONE;
	break;

      case TRAP_FLUID:
      case TRAP_FLUID_DANGEROUS:
	cell = lookup_fluid(Val);
	repeat_p = true;
	if (trap_kind == TRAP_FLUID)
	  continue;
	break;

      case TRAP_COMPILER_CACHED:
      case TRAP_COMPILER_CACHED_DANGEROUS:
	cell = Nth_Vector_Loc(Vector_Ref(Val, TRAP_EXTRA),
			      TRAP_EXTENSION_CELL);
	repeat_p = true;
	if (trap_kind == TRAP_COMPILER_CACHED)
	  continue;
	break;

      case TRAP_UNBOUND:
	return ERR_UNBOUND_VARIABLE;

      case TRAP_UNBOUND_DANGEROUS:
	return_value = ERR_UNBOUND_VARIABLE;
	break;

      default:
	return_value = ERR_ILLEGAL_REFERENCE_TRAP;
	break;
    }

    /* The reference was dangerous, uncompile the variable. */
    {
      Lock_Handle compile_serializer;

      setup_lock(compile_serializer, hunk);
      hunk[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
      hunk[VARIABLE_OFFSET] = NIL;
      remove_lock(compile_serializer);
    }

  } while (repeat_p);

  return return_value;
}

/* Simple lookup finalization.
   All the hairy cases are left to deep_lookup_end.
   env is the environment where the reference was supposedly resolved.
   If there is any question about the validity of the resolution (due
   to dangerousness, for example), a deep lookup operation is
   performed, and control is given to deep_lookup_end.
 */

long
lookup_end(cell, env, hunk)
	Pointer *cell, env, *hunk;
{
  long trap_kind;

lookup_end_restart:
  Val = Fetch(cell[0]);
  FUTURE_VARIABLE_SPLICE (((Pointer) cell), 0, Val);

  if (!(REFERENCE_TRAP_P(Val)))
  {
    return PRIM_DONE;
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
	deep_lookup_end(deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk),
			hunk);

    case TRAP_COMPILER_CACHED:
      cell = Nth_Vector_Loc(Vector_Ref(Val, TRAP_EXTRA),
			    TRAP_EXTENSION_CELL);
      goto lookup_end_restart;

    case TRAP_FLUID:
      cell = lookup_fluid(Val);
      goto lookup_end_restart;

    case TRAP_UNBOUND:
      return ERR_UNBOUND_VARIABLE;

    case TRAP_UNASSIGNED:
      return ERR_UNASSIGNED_VARIABLE;

    default:
      return ERR_ILLEGAL_REFERENCE_TRAP;
  }
}

/* Complete assignment finalization.
   deep_assignment_end handles all dangerous cases, and busts compiled
   code operator reference caches as appropriate.  It is similar to
   deep_lookup_end.
   value is the new value for the variable.
   force forces an assignment if the variable is unbound.  This is
   used for redefinition in the global environment, and for Common
   Lisp style fluid binding, which creates a value cell if there was
   none.

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
deep_assignment_end(cell, hunk, value, force)
	fast Pointer *cell;
	Pointer *hunk, value;
	Boolean force;
{
  Lock_Handle set_serializer;
  long trap_kind, return_value;
  Pointer bogus_unassigned, extension, saved_extension, saved_value;
  Boolean repeat_p, uncompile_p, fluid_lock_p;

  /* State variables */
  saved_extension = NIL;
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
        Val = Vector_Ref(Val, TRAP_EXTRA);
	if (value == UNASSIGNED_OBJECT)
	{
	  *cell = DANGEROUS_UNASSIGNED_OBJECT;
	}
	else
	{
	  Do_Store_No_Lock ((Nth_Vector_Loc (*cell, TRAP_EXTRA)), value);
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
	  Pointer result;

	  if (GC_allocate_test(2))
	  {
	    Request_GC(2);
	    ABORT(PRIM_INTERRUPT);
	  }
	  result = Make_Pointer(TC_REFERENCE_TRAP, Free);
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
	extension = Fast_Vector_Ref(Val, TRAP_EXTRA);

compiler_cache_assignment:
	{
	  Pointer references;

	  /* Unlock and lock at the new value cell. */

	  references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);
	  cell = Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL);
	  update_lock(set_serializer, cell);

	  if (Fast_Vector_Ref(references, TRAP_REFERENCES_OPERATOR) != NIL)
	  {
	    if (saved_extension != NIL)
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
  
  if (saved_extension != NIL)
  {
    long recache_uuo_links();

    if (fluid_lock_p)
    {
      /* Guarantee that there is a lock on the variable cache around
	 the call to recache_uuo_links.
       */

      update_lock(set_serializer,
		  Nth_Vector_Loc(saved_extension, TRAP_EXTENSION_CELL));
    }
    return_value = recache_uuo_links(saved_extension, saved_value);

    remove_lock(set_serializer);

    if (return_value != PRIM_DONE)
    {
      return return_value;
    }
  }
  else
  {
    remove_lock(set_serializer);
  }

  /* This must be done after the assignment lock has been removed,
     to avoid potential deadlock.
   */
  if (uncompile_p)
  {
    /* The reference was dangerous, uncompile the variable. */

    Lock_Handle compile_serializer;

    setup_lock(compile_serializer, hunk);
    hunk[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;
    hunk[VARIABLE_OFFSET] = NIL;
    remove_lock(compile_serializer);
  }

  return return_value;
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
assignment_end(cell, env, hunk, value)
	fast Pointer *cell;
	Pointer env, *hunk, value;
{
  Lock_Handle set_serializer;
  Pointer bogus_unassigned;
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
    return PRIM_DONE;
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
	deep_assignment_end(deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk),
			    hunk,
			    value,
			    false);

    case TRAP_COMPILER_CACHED:
    {
      Pointer extension, references;

      extension = Fast_Vector_Ref(Val, TRAP_EXTRA);
      references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);

      if (Fast_Vector_Ref(references, TRAP_REFERENCES_OPERATOR) != NIL)
      {
	/* There are uuo links.
	   wimp out and let deep_assignment_end handle it.
	 */

	remove_lock(set_serializer);
	return deep_assignment_end(cell, hunk, value, false);
      }
      cell = Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL);
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
  return temp;
}

/* Finds the fluid value cell associated with the reference trap on
   this processor's fluid "binding" list.  It is just like ASSQ.
 */

Pointer *
lookup_fluid(trap)
     fast Pointer trap;
{
  fast Pointer fluids, *this_pair;

  fluids = Fluid_Bindings;

  if (Fluids_Debug)
    Print_Expression(fluids, "Searching fluid bindings");

  while (PAIR_P(fluids))
  {
    this_pair = Get_Pointer(Fast_Vector_Ref(fluids, CONS_CAR));

    if (this_pair[CONS_CAR] == trap)
    {
      if (Fluids_Debug)
	fprintf(stderr, "Fluid found.\n");

      return &this_pair[CONS_CDR];
    }

    fluids = Fast_Vector_Ref(fluids, CONS_CDR);
  }

  /* Not found in fluid binding alist, so use default. */

  if (Fluids_Debug)
    fprintf(stderr, "Fluid not found, using default.\n");

  return Nth_Vector_Loc(trap, TRAP_EXTRA);
}

/* Utilities for definition.

   redefinition is used when the definition is in fact an assignment.
   A binding already exists in this frame.

   dangerize is invoked to guarantee that any variables "compiled" to
   this location are recompiled at the next reference.
 */

#define redefinition(cell, value) \
  deep_assignment_end(cell, fake_variable_object, value, true)

long
dangerize(cell, sym)
     fast Pointer *cell;
     Pointer sym;
{
  Lock_Handle set_serializer;
  fast long temp;
  Pointer trap;

  setup_lock(set_serializer, cell);
  if (!(REFERENCE_TRAP_P(*cell)))
  {
    if (GC_allocate_test(2))
    {
      remove_lock(set_serializer);
      Request_GC(2);
      return PRIM_INTERRUPT;
    }
    trap = Make_Pointer(TC_REFERENCE_TRAP, Free);
    *Free++ = DANGEROUS_OBJECT;
    *Free++ = *cell;
    *cell = trap;
    remove_lock(set_serializer);
    return PRIM_DONE;
  }

  get_trap_kind(temp, *cell);
  switch(temp)
  {
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
      temp = PRIM_DONE;
      break;

    case TRAP_COMPILER_CACHED:
      Do_Store_No_Lock
	((Nth_Vector_Loc (*cell, TRAP_TAG)),
	 (Make_Unsigned_Fixnum (TRAP_COMPILER_CACHED_DANGEROUS)));
      /* Fall through */

    case TRAP_COMPILER_CACHED_DANGEROUS:
    {
      long compiler_uncache();

      remove_lock(set_serializer);
      return compiler_uncache(cell, sym);
    }

    case TRAP_FLUID:
      Do_Store_No_Lock
	((Nth_Vector_Loc (*cell, TRAP_TAG)),
	 (Make_Unsigned_Fixnum (TRAP_FLUID_DANGEROUS)));
      temp = PRIM_DONE;
      break;

    case TRAP_UNBOUND:
      *cell = DANGEROUS_UNBOUND_OBJECT;
      temp = PRIM_DONE;
      break;

    case TRAP_UNASSIGNED:
      *cell = DANGEROUS_UNASSIGNED_OBJECT;
      temp = PRIM_DONE;
      break;

    default:
      temp = ERR_ILLEGAL_REFERENCE_TRAP;
      break;
  }
  remove_lock(set_serializer);
  return temp;
}

/* The core of the incremental definition mechanism.
   It takes care of dangerizing any bindings being shadowed by this
   definition, extending the frames appropriately, and uncaching any
   compiled code reference caches which might be affected by the new
   definition.
 */

long
extend_frame(env, sym, value, original_frame_p)
     Pointer env, sym, value;
     Boolean original_frame_p;
{
  Lock_Handle extension_serializer;
  Pointer extension, the_procedure;
  fast Pointer *scan;
  long aux_count;

  if (OBJECT_TYPE(env) == GLOBAL_ENV)
  {
    if (OBJECT_DATUM(env) != GO_TO_GLOBAL)
    {
      if (original_frame_p)
	return ERR_BAD_FRAME;
      return PRIM_DONE;
    }
    else if (original_frame_p)
      return redefinition(Nth_Vector_Loc(sym, SYMBOL_GLOBAL_VALUE),
			  value);

    else return dangerize(Nth_Vector_Loc(sym, SYMBOL_GLOBAL_VALUE), sym);
  }

  the_procedure = Vector_Ref(env, ENVIRONMENT_FUNCTION);
  if (OBJECT_TYPE(the_procedure) == AUX_LIST_TYPE)
    the_procedure = Vector_Ref(the_procedure, ENV_EXTENSION_PROCEDURE);

  /* Search the formals. */

  {
    fast long count;
    Pointer formals;

    formals = Fast_Vector_Ref(Fast_Vector_Ref(the_procedure,
					      PROCEDURE_LAMBDA_EXPR),
			      LAMBDA_FORMALS);
    for (count = Vector_Length(formals) - 1,
	 scan = Nth_Vector_Loc(formals, VECTOR_DATA + 1);
	 count > 0;
	 count -= 1)
      if (*scan++ == sym)
      {
	long offset;

	offset = 1 + Vector_Length(formals) - count;
	if (original_frame_p)
	  return redefinition(Nth_Vector_Loc(env, offset), value);
	else
	  return dangerize(Nth_Vector_Loc(env, offset), sym);
      }
  }

  /* Guarantee that there is an extension slot. */

redo_aux_lookup:

  setup_lock(extension_serializer, Get_Pointer(env));
  extension = Fast_Vector_Ref(env, ENVIRONMENT_FUNCTION);
  if (OBJECT_TYPE(extension) != AUX_LIST_TYPE)
  {
    fast long i;

    if (GC_allocate_test(AUX_LIST_INITIAL_SIZE))
    {
      remove_lock(extension_serializer);
      Request_GC(AUX_LIST_INITIAL_SIZE);
      return PRIM_INTERRUPT;
    }
    scan = Free;
    extension = Make_Pointer(AUX_LIST_TYPE, scan);

    scan[ENV_EXTENSION_HEADER] =
      Make_Non_Pointer(TC_MANIFEST_VECTOR, (AUX_LIST_INITIAL_SIZE - 1));

    scan[ENV_EXTENSION_PARENT_FRAME] =
      Vector_Ref(the_procedure, PROCEDURE_ENVIRONMENT);

    scan[ENV_EXTENSION_PROCEDURE] = the_procedure;

    scan[ENV_EXTENSION_COUNT] = Make_Local_Offset(0);

    for (i = AUX_CHUNK_SIZE, scan += AUX_LIST_FIRST;
	 --i >= 0;)
      *scan++ = NIL;

    Free = scan;
    Do_Store_No_Lock ((Nth_Vector_Loc (env, ENVIRONMENT_FUNCTION)), extension);
  }
  aux_count = Lexical_Offset(Fast_Vector_Ref(extension, AUX_LIST_COUNT));
  remove_lock(extension_serializer);

  /* Search the aux list. */

  {
    fast long count;

    scan = Get_Pointer(extension);
    count = aux_count;
    scan += AUX_LIST_FIRST;

    while (--count >= 0)
    {
      if (Fast_Vector_Ref(*scan, CONS_CAR) == sym)
      {
	scan = Nth_Vector_Loc(*scan, CONS_CDR);

	/* This is done only because of compiler cached variables.
	   In their absence, this conditional is unnecessary.
	   Note that this would also have to be done for formal
	   bindings if we allowed "undefinition" of variables.
	 */
	if (Fetch(scan[0]) == DANGEROUS_UNBOUND_OBJECT)
	{
	  long compiler_uncache();
	  long temp;
	  
	  temp =
	    compiler_uncache
	      (deep_lookup(Fast_Vector_Ref(extension,
					   ENV_EXTENSION_PARENT_FRAME),
			   sym,
			   fake_variable_object),
	       sym);
	  if (temp != PRIM_DONE)
	    return temp;
	}

	if (original_frame_p)
	  return redefinition(scan, value);
	else
	  return dangerize(scan, sym);
      }
      scan += 1;  
    }
  }

  /* Not found in this frame at all. */

  {
    fast long temp;

    temp =
      extend_frame(Fast_Vector_Ref(extension, ENV_EXTENSION_PARENT_FRAME),
		   sym, NIL, false);

    if (temp != PRIM_DONE)
      return temp;

    /* Proceed to extend the frame:
       - If the frame is the one where the definition is occurring,
	 put the value in the new value cell.
       - Otherwise, put a dangerous unbound trap there.
       - This code is careful to restart if some other process defines
         something in the meantime in this frame.
     */

    setup_lock(extension_serializer, Get_Pointer(env));
    temp = Lexical_Offset(Fast_Vector_Ref(extension, AUX_LIST_COUNT));

    if ((extension != Fast_Vector_Ref(env, ENVIRONMENT_FUNCTION)) ||
	(temp != aux_count))
    {
      remove_lock(extension_serializer);
      goto redo_aux_lookup;
    }
	
    scan = Get_Pointer(extension);

    if ((temp + (AUX_LIST_FIRST - 1)) == Get_Integer(scan[VECTOR_LENGTH]))
    {
      fast long i;
      fast Pointer *fast_free;

      i = ((2 * temp) + AUX_LIST_FIRST);

      if (GC_allocate_test(i))
      {
	remove_lock(extension_serializer);
	Request_GC(i);
	return PRIM_INTERRUPT;
      }

      fast_free = Free;
      i -= 1;

      scan += 1;
      *fast_free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, i);
      for (i = (temp + (AUX_LIST_FIRST - 1)); --i >= 0; )
	*fast_free++ = *scan++;
      for (i = temp; --i >= 0; )
	*fast_free++ = NIL;

      scan = Free;
      Free = fast_free;
      Do_Store_No_Lock
	((Nth_Vector_Loc (env, ENVIRONMENT_FUNCTION)),
	 (Make_Pointer (AUX_LIST_TYPE, scan)));
    }
    
    if (GC_allocate_test(2))
    {
      remove_lock(extension_serializer);
      Request_GC(2);
      return PRIM_INTERRUPT;
    }

    {
      Pointer result;

      result = Make_Pointer(TC_LIST, Free);
      *Free++ = sym;
      *Free++ = ((original_frame_p) ? value : DANGEROUS_UNBOUND_OBJECT);

      scan[temp + AUX_LIST_FIRST] = result;
      scan[AUX_LIST_COUNT] = Make_Local_Offset(temp + 1);
    }
    remove_lock(extension_serializer);
    return PRIM_DONE;
  }
}

/* Top level of lookup code.
   These are the procedures invoked from outside this file.
 */

long
Lex_Ref(env, var)
	Pointer env, var;
{
  fast Pointer *cell;
  Pointer *hunk;

  hunk = Get_Pointer(var);
  lookup(cell, env, hunk, repeat_lex_ref_lookup);
  return lookup_end(cell, env, hunk);
}

long
Symbol_Lex_Ref(env, sym)
	Pointer env, sym;
{
  return deep_lookup_end(deep_lookup(env, sym, fake_variable_object),
			 fake_variable_object);
}

long
Lex_Set(env, var, value)
	Pointer env, var, value;
{
  fast Pointer *cell;
  Pointer *hunk;

  hunk = Get_Pointer(var);
  lookup(cell, env, hunk, repeat_lex_set_lookup);
  return assignment_end(cell, env, hunk, value);
}

long
Symbol_Lex_Set(env, sym, value)
	Pointer env, sym, value;
{
  return deep_assignment_end(deep_lookup(env, sym, fake_variable_object),
			     fake_variable_object,
			     value,
			     false);
}

long
Local_Set(env, sym, value)
	Pointer env, sym, value;
{
  long result;

  if (Define_Debug)
    fprintf(stderr,
	    "\n;; Local_Set: defining %s.",
	    Scheme_String_To_C_String(Vector_Ref(sym, SYMBOL_NAME)));
  result = extend_frame(env, sym, value, true);
  Val = sym;
  return result;
}

long
safe_reference_transform (reference_result)
     long reference_result;
{
  if (reference_result == ERR_UNASSIGNED_VARIABLE)
    {
      Val = UNASSIGNED_OBJECT;
      return (PRIM_DONE);
    }
  else
    return (reference_result);
}

long
safe_lex_ref (env, var)
	Pointer env, var;
{
  return (safe_reference_transform (Lex_Ref (env, var)));
}

long
safe_symbol_lex_ref (env, sym)
     Pointer env, sym;
{
  return (safe_reference_transform (Symbol_Lex_Ref (env, sym)));
}

long
unassigned_p_transform (reference_result)
     long reference_result;
{
  switch (reference_result)
    {
    case ERR_UNASSIGNED_VARIABLE:
      Val = TRUTH;
      return (PRIM_DONE);

    case ERR_UNBOUND_VARIABLE:
    case PRIM_DONE:
      Val = NIL;
      return (PRIM_DONE);

    default:
      return (reference_result);
    }
}

long
Symbol_Lex_unassigned_p( frame, symbol)
     Pointer frame, symbol;
{
  return (unassigned_p_transform (Symbol_Lex_Ref (frame, symbol)));
}

long
Symbol_Lex_unbound_p( frame, symbol)
     Pointer frame, symbol;
{
  long result;

  result = Symbol_Lex_Ref( frame, symbol);
  switch (result)
    {
    case ERR_UNASSIGNED_VARIABLE:
    case PRIM_DONE:
      {
	Val = NIL;
	return (PRIM_DONE);
      }

    case ERR_UNBOUND_VARIABLE:
      {
	Val = TRUTH;
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

Pointer *
force_definition(env, symbol, message)
    fast Pointer env;
    Pointer symbol;
    long *message;
{
  fast Pointer previous;

  if (OBJECT_TYPE(env) == GLOBAL_ENV)
    return ((Pointer *) NULL);
	    
  do
  {
    previous = env;
    env = Fast_Vector_Ref(Vector_Ref(env, ENVIRONMENT_FUNCTION),
			  PROCEDURE_ENVIRONMENT);
  } while (OBJECT_TYPE(env) != GLOBAL_ENV);

  *message = Local_Set(previous, symbol, UNASSIGNED_OBJECT);
  if (*message != PRIM_DONE)
    return ((Pointer *) NULL);
  return
    deep_lookup(previous, symbol, fake_variable_object);
}

/* Fast variable reference mechanism for compiled code.

   compiler_cache_reference is the core of the variable caching mechanism.

   It creates a variable cache for the variable specified by (name,
   env) if needed, and stores it or a related object in the location
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

long
compiler_cache_reference(env, name, block, offset, kind)
     Pointer env, name, block;
     long offset, kind;
{
  Lock_Handle set_serializer;
  fast Pointer *cell, trap, references, extension;
  Pointer trap_value, store_trap_tag, store_extension;
  long trap_kind, return_value;

  cell = deep_lookup(env, name, fake_variable_object);
  if (cell == unbound_trap_object)
  {
    long message;

    cell = force_definition(env, name, &message);
    if (message != PRIM_DONE)
      return message;
  }
    
  store_trap_tag = NIL;
  store_extension = NIL;
  trap_kind = TRAP_COMPILER_CACHED;

  setup_lock(set_serializer, cell);
  trap = *cell;
  trap_value = trap;

  if (REFERENCE_TRAP_P(trap))
  {
    long old_trap_kind;

    get_trap_kind(old_trap_kind, trap);
    switch(old_trap_kind)
    {
      case TRAP_UNASSIGNED:
      case TRAP_UNBOUND:
      case TRAP_FLUID:
	break;

      case TRAP_DANGEROUS:
        trap_value = Fast_Vector_Ref(trap, TRAP_EXTRA);
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
	store_trap_tag = Make_Unsigned_Fixnum(TRAP_FLUID);
	trap_kind = TRAP_COMPILER_CACHED_DANGEROUS;
	break;

      case TRAP_COMPILER_CACHED:
      case TRAP_COMPILER_CACHED_DANGEROUS:
	extension = Fast_Vector_Ref(trap, TRAP_EXTRA);
	update_lock(set_serializer,
		    Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL));
	trap_value = Fast_Vector_Ref(extension, TRAP_EXTENSION_CELL);
	trap_kind = -1;
	break;

      default:
	remove_lock(set_serializer);
	return ERR_ILLEGAL_REFERENCE_TRAP;
    }
  }

#if true

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

  if (GC_allocate_test(MAXIMUM_CACHE_SIZE))
  {
    remove_lock(set_serializer);
    Request_GC(MAXIMUM_CACHE_SIZE);
    return PRIM_INTERRUPT;
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
    Pointer new_trap, list;

#if false
    /* This is included in the check above. */
    if (GC_allocate_test(7))
    {
      remove_lock(set_serializer);
      Request_GC(7);
      return PRIM_INTERRUPT;
    }
#endif

    new_trap = Make_Pointer(TC_REFERENCE_TRAP, Free);
    *Free++ = Make_Unsigned_Fixnum(trap_kind);
    extension = Make_Pointer(TRAP_EXTENSION_TYPE, (Free + 1));
    *Free++ = extension;

    *Free++ = trap_value;
    *Free++ = name;
    *Free++ = NIL;
    references = Make_Pointer(TRAP_REFERENCES_TYPE, (Free + 1));
    *Free++ = references;

    *Free++ = NIL;
    *Free++ = NIL;
    *Free++ = NIL;

    *cell = new_trap;		/* Do_Store_No_Lock ? */
    if (store_trap_tag != NIL)
    {
      /* Do_Store_No_Lock ? */
      Fast_Vector_Set(trap, TRAP_TAG, store_trap_tag);
    }
    update_lock(set_serializer,
		Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL));
  }

  /* There already is a compiled code cache.
     Maybe this should clean up all the cache lists? 
   */

  {
    void fix_references();
    long add_reference();

    references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);

    if (((kind == TRAP_REFERENCES_ASSIGNMENT) &&
	 (Fast_Vector_Ref(references, TRAP_REFERENCES_OPERATOR) != NIL)) ||
	((kind == TRAP_REFERENCES_OPERATOR) &&
	 (Fast_Vector_Ref(references, TRAP_REFERENCES_ASSIGNMENT) != NIL)))
    {
      store_extension = Fast_Vector_Ref(extension, TRAP_EXTENSION_CLONE);
      if (store_extension == NIL)
      {
#if false
	/* This is included in the check above. */

	if (GC_allocate_test(4))
	{
	  remove_lock(set_serializer);
	  Request_GC(4);
	  return PRIM_INTERRUPT;
	}
#endif
	store_extension = Make_Pointer(TRAP_EXTENSION_TYPE, Free);
	*Free++ = EXPENSIVE_ASSIGNMENT_OBJECT;
	*Free++ = Fast_Vector_Ref(extension, TRAP_EXTENSION_NAME);
	*Free++ = extension;
	*Free++ = references;
	Fast_Vector_Set(extension, TRAP_EXTENSION_CLONE, store_extension);

	if (kind == TRAP_REFERENCES_OPERATOR)
	{
	  fix_references(Nth_Vector_Loc(references,
					TRAP_REFERENCES_ASSIGNMENT),
			 store_extension);
	}
      }
    }
    
    return_value = add_reference(Nth_Vector_Loc(references, kind),
				 block,
				 Make_Unsigned_Fixnum(offset));
    if (return_value != PRIM_DONE)
    {
      remove_lock(set_serializer);
      return return_value;
    }
  }

  /* Install an extension or a uuo link in the cc block, and remove
     the lock.
   */

  return_value = PRIM_DONE;

  switch(kind)
  {
    default:
    case TRAP_REFERENCES_ASSIGNMENT:
      if (store_extension != NIL)
      {
	extern void store_variable_cache();

	store_variable_cache(store_extension, block, offset);
	break;
      }
      /* Fall through */

    case TRAP_REFERENCES_LOOKUP:
    {
      extern void store_variable_cache();

      store_variable_cache(extension, block, offset);
      break;
    }

    case TRAP_REFERENCES_OPERATOR:
    {
      extern long make_uuo_link(), make_fake_uuo_link();

      if (REFERENCE_TRAP_P(trap_value))
      {
	return_value = make_fake_uuo_link(extension, block, offset);
      }
      else
      {
	return_value = make_uuo_link(trap_value, extension, block, offset);
      }
      break;
    }
  }

  remove_lock(set_serializer);
  return return_value;
}

/* This procedure updates all the references in the cached reference
   list pointed at by slot to hold value.  It also eliminates "empty"
   pairs (pairs whose weakly held block has vanished).  
 */

void
fix_references(slot, extension)
     fast Pointer *slot, extension;
{
  fast Pointer pair, block;

  while (*slot != NIL)
  {
    pair = Fast_Vector_Ref(*slot, CONS_CAR);
    block = Fast_Vector_Ref(pair, CONS_CAR);
    if (block == NIL)
    {
      *slot = Fast_Vector_Ref(*slot, CONS_CDR);
    }
    else
    {
      extern void store_variable_cache();

      store_variable_cache(extension,
			   block,
			   Get_Integer(Fast_Vector_Ref(pair, CONS_CDR)));
      slot = Nth_Vector_Loc(*slot, CONS_CDR);
    }
  }
  return;
}

/* This procedures adds a new cached reference to the cached reference
   list pointed at by slot.  It attempts to reuse pairs which have been
   "emptied" by the garbage collector.
 */

long
add_reference(slot, block, offset)
     fast Pointer *slot;
     Pointer block, offset;
{
  fast Pointer pair;

  while (*slot != NIL)
  {
    pair = Fast_Vector_Ref(*slot, CONS_CAR);
    if (Fast_Vector_Ref(pair, CONS_CAR) == NIL)
    {
      Fast_Vector_Set(pair, CONS_CAR, block);
      Fast_Vector_Set(pair, CONS_CDR, offset);
      return PRIM_DONE;
    }
    slot = Nth_Vector_Loc(*slot, CONS_CDR);    
  }

  if (GC_allocate_test(4))
  {
    Request_GC(4);
    return PRIM_INTERRUPT;
  }

  *slot = Make_Pointer(TC_LIST, Free);
  *Free = Make_Pointer(TC_WEAK_CONS, (Free + 2));
  Free += 1;
  *Free++ = NIL;

  *Free++ = block;
  *Free++ = offset;

  return PRIM_DONE;
}

/* compiler_uncache_slot uncaches all references in the list pointed
   at by slot, and clears the list.  If the references are operator
   references, a fake compiled procedure which will recache when
   invoke is created and installed.
 */

long
compiler_uncache_slot(slot, sym, kind)
     fast Pointer *slot;
     Pointer sym;
     long kind;
{
  fast Pointer temp, pair;
  Pointer block, offset, new_extension;

  for (temp = *slot; temp != NIL; temp = *slot)
  {
    pair = Fast_Vector_Ref(temp, CONS_CAR);
    block = Fast_Vector_Ref(pair, CONS_CAR);
    if (block != NIL)
    {
      offset = Fast_Vector_Ref(pair, CONS_CDR);
      if (GC_allocate_test(4))
      {
	Request_GC(4);
	return PRIM_INTERRUPT;
      }
      new_extension = Make_Pointer(TRAP_EXTENSION_TYPE, Free);
      *Free++ = REQUEST_RECACHE_OBJECT;
      *Free++ = sym;
      *Free++ = block;
      *Free++ = offset;
      if (kind == TRAP_REFERENCES_OPERATOR)
      {
	extern long make_fake_uuo_link();
	long result;

	result = make_fake_uuo_link(new_extension,
				    block,
				    Get_Integer(offset));
	if (result != PRIM_DONE)
	  return result;
      }
      else
      {
	extern void store_variable_cache();

	store_variable_cache(new_extension, block, Get_Integer(offset));
      }
    }
    *slot = Fast_Vector_Ref(temp, CONS_CDR);
  }
  return PRIM_DONE;
}

/* compiler_uncache is invoked when a redefinition occurs.
   It uncaches all references cached to this value cell, and
   sets the variables up to be recached at the next reference.
   value_cell is the value cell being shadowed.
   sym is the name of the variable.
 */

static long trap_map_table[] =
  { TRAP_REFERENCES_LOOKUP,
    TRAP_REFERENCES_ASSIGNMENT,
    TRAP_REFERENCES_OPERATOR};

long
compiler_uncache(value_cell, sym)
     Pointer *value_cell, sym;
{
  Lock_Handle set_serializer;
  Pointer val, extension, references;
  long trap_kind, temp, i, index;

  setup_lock(set_serializer, value_cell);

  val = *value_cell;

  if (!(REFERENCE_TRAP_P(val)))
  {
    remove_lock(set_serializer);
    return PRIM_DONE;
  }

  get_trap_kind(trap_kind, val);
  if ((trap_kind != TRAP_COMPILER_CACHED) &&
      (trap_kind != TRAP_COMPILER_CACHED_DANGEROUS))
  {
    remove_lock(set_serializer);
    return PRIM_DONE;
  }

  extension = Fast_Vector_Ref(val, TRAP_EXTRA);
  references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);
  update_lock(set_serializer, Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL));

  /* Uncache all of the lists. */

  for (i = 0; i < 3; i++)
  {
    index = trap_map_table[i];
    temp = compiler_uncache_slot(Nth_Vector_Loc(references, index),
				 sym, index);
    if (temp != PRIM_DONE)
    {
      remove_lock(set_serializer);
      return temp;
    }
  }

  /* We should actually remove the trap here, but, for now... */

  /* Remove the clone extension if there is one. */

  Fast_Vector_Set(extension, TRAP_EXTENSION_CLONE, NIL);
  remove_lock(set_serializer);
  return PRIM_DONE;
}

/* recache_uuo_links is invoked when an assignment occurs to a
   variable which has cached operator references (uuo links).
   All the operator references must be recached to the new value.

   It currently potentially creates a new uuo link per operator
   reference.  This may be very expensive in space, but allows a great
   deal of flexibility.  It is ultimately necessary if there is hidden
   information on each call (like arity, types of arguments, etc.).
 */

long
recache_uuo_links(extension, old_value)
     Pointer extension, old_value;
{
  long update_uuo_links();

  Pointer value;
  long return_value;

  value = Fast_Vector_Ref(extension, TRAP_EXTENSION_CELL);
  if (REFERENCE_TRAP_P(value))
  {
    if (REFERENCE_TRAP_P(old_value))
    {
      /* No need to do anything.
	 The uuo links are in the correct state.
       */

      return_value = PRIM_DONE;
    }
    else
    {
      long make_recache_uuo_link();

      return_value =
	update_uuo_links(value, extension, make_recache_uuo_link);
    }
  }
  else
  {
    extern long make_uuo_link();

    return_value =
      update_uuo_links(value, extension, make_uuo_link);
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

    Fast_Vector_Set(extension, TRAP_EXTENSION_CELL, old_value);
  }
  return return_value;
}

/* This kludge is due to the lack of closures. */

long
make_recache_uuo_link(value, extension, block, offset)
     Pointer value, extension, block;
     long offset;
{
  extern long make_fake_uuo_link();

  return make_fake_uuo_link(extension, block, offset);
}

long
update_uuo_links(value, extension, handler)
     Pointer value, extension;
     long (*handler)();
{
  Pointer references, pair, block;
  fast Pointer *slot;
  long return_value;

  references = Fast_Vector_Ref(extension, TRAP_EXTENSION_REFERENCES);
  slot = Nth_Vector_Loc(references, TRAP_REFERENCES_OPERATOR);

  while (*slot != NIL)
  {
    pair = Fast_Vector_Ref(*slot, CONS_CAR);
    block = Fast_Vector_Ref(pair, CONS_CAR);
    if (block == NIL)
    {
      *slot = Fast_Vector_Ref(*slot, CONS_CDR);
    }
    else
    {
      return_value =
	(*handler)(value, extension, block,
		   Get_Integer(Fast_Vector_Ref(pair, CONS_CDR)));
      if (return_value != PRIM_DONE)
      {
	return return_value;
      }
      slot = Nth_Vector_Loc(*slot, CONS_CDR);
    }
  }

  /* If there are no uuo links left, and there is an extension clone,
     remove it, and make assignment references point to the real value
     cell. 
   */
     
  if ((Fast_Vector_Ref(references, TRAP_REFERENCES_OPERATOR) == NIL) &&
      (Fast_Vector_Ref(extension, TRAP_EXTENSION_CLONE) != NIL))
  {
    Fast_Vector_Set(extension, TRAP_EXTENSION_CLONE, NIL);
    fix_references(Nth_Vector_Loc(references, TRAP_REFERENCES_ASSIGNMENT),
		   extension);
  }
  return PRIM_DONE;
}

/* compiler_reference_trap is called when a reference occurs to a compiled
   reference cache which contains a reference trap.  If the trap is
   the special REQUEST_RECACHE_OBJECT, the reference is recached.
   Otherwise the reference is done normally, and the process continued.
 */

extern Pointer compiled_block_environment();

long
compiler_reference_trap(extension, kind, handler)
     Pointer extension;
     long kind;
     long (*handler)();
{
  long offset, temp;
  Pointer block;

  if (Vector_Ref(extension, TRAP_EXTENSION_CELL) != REQUEST_RECACHE_OBJECT)
  {
    return (*handler)(Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL),
		      fake_variable_object);
  }

  block = Fast_Vector_Ref(extension, TRAP_EXTENSION_BLOCK);
  offset = Get_Integer(Fast_Vector_Ref(extension, TRAP_EXTENSION_OFFSET));
  temp = 
    compiler_cache_reference(compiled_block_environment(block),
			     Fast_Vector_Ref(extension, TRAP_EXTENSION_NAME),
			     block,
			     offset,
			     kind);
  if (temp != PRIM_DONE)
    return temp;

  switch(kind)
  {
    case TRAP_REFERENCES_OPERATOR:
    {
      /* Note that this value may cause another operator trap when
	 invoked, since it may be a uuo-link to an interpreted
	 procedure, or to a variable with a trap in it.  It should not
	 go into a loop however, because the reference will be cached
	 to the correct place, so the extension will no longer have a
	 REQUEST_RECACHE_OBJECT in it.  The first branch in this
	 procedure will be taken in this case.
       */

      extern Pointer extract_uuo_link();

      Val = extract_uuo_link(block, offset);
      return PRIM_DONE;
    }

    case TRAP_REFERENCES_ASSIGNMENT:
    case TRAP_REFERENCES_LOOKUP:
    default:
    {
      extern Pointer extract_variable_cache();
      Pointer extension;

      extension = extract_variable_cache(block, offset);
      return (*handler)(Nth_Vector_Loc(extension, TRAP_EXTENSION_CELL),
			fake_variable_object);
    }
  }
}

/* Procedures invoked from the compiled code interface. */

extern long
  compiler_cache_lookup(),
  compiler_cache_assignment(),
  compiler_cache_operator();

long
compiler_cache_lookup(name, block, offset)
     Pointer name, block;
     long offset;
{
  return compiler_cache_reference(compiled_block_environment(block),
				  name, block, offset,
				  TRAP_REFERENCES_LOOKUP);
}

long
compiler_cache_assignment(name, block, offset)
     Pointer name, block;
     long offset;
{
  return compiler_cache_reference(compiled_block_environment(block),
				  name, block, offset,
				  TRAP_REFERENCES_LOOKUP);
}

long
compiler_cache_operator(name, block, offset)
     Pointer name, block;
     long offset;
{
  return compiler_cache_reference(compiled_block_environment(block),
				  name, block, offset,
				  TRAP_REFERENCES_OPERATOR);
}

extern long compiler_operator_reference_trap();
extern Pointer compiler_var_error();

long
compiler_operator_reference_trap(frame_slot, extension)
     Pointer *frame_slot, extension;
{
  long temp;

  temp = compiler_reference_trap(extension,
				 TRAP_REFERENCES_OPERATOR,
				 deep_lookup_end);
  if (temp != PRIM_DONE)
    return temp;
  *frame_slot = Val;
  return PRIM_DONE;
}

Pointer
compiler_var_error(extension, environment)
     Pointer extension, environment;
{
  return Vector_Ref(extension, TRAP_EXTENSION_NAME);
}

/* Utility for compiler_assignment_trap, below.
   Necessary because C lacks lambda.  Argh! 
 */

static Pointer saved_compiler_assignment_value;

long
compiler_assignment_end(cell, hunk)
     Pointer *cell, *hunk;
{
  return
    deep_assignment_end(cell, hunk, saved_compiler_assignment_value, false);
}

/* More compiled code interface procedures */

extern long
  compiler_lookup_trap(),
  compiler_safe_lookup_trap(),
  compiler_unassigned_p_trap(),
  compiler_assignment_trap();

long
compiler_lookup_trap(extension)
     Pointer extension;
{
  return compiler_reference_trap(extension,
				 TRAP_REFERENCES_LOOKUP,
				 deep_lookup_end);
}

long
compiler_safe_lookup_trap (extension)
     Pointer extension;
{
  return (safe_reference_transform (compiler_lookup_trap (extension)));
}

long
compiler_unassigned_p_trap (extension)
     Pointer extension;
{
  return (unassigned_p_transform (compiler_lookup_trap (extension)));
}

long
compiler_assignment_trap(extension, value)
     Pointer extension, value;
{
  saved_compiler_assignment_value = value;
  return compiler_reference_trap(extension,
				 TRAP_REFERENCES_ASSIGNMENT,
				 compiler_assignment_end);
}

/* Primitives built on the procedures above. */

/* (LEXICAL-ASSIGNMENT ENVIRONMENT SYMBOL VALUE)
   Sets the value of the variable with the name given in SYMBOL, as
   seen in the lexical ENVIRONMENT, to the specified VALUE.
   Returns the previous value.

   It's indistinguishable from evaluating
   (set! <symbol> <value>) in <environment>.
*/
Built_In_Primitive(Prim_Lexical_Assignment, 3, "LEXICAL-ASSIGNMENT", 0x0)
{
  Primitive_3_Args();

  standard_lookup_primitive(Symbol_Lex_Set(Arg1, Arg2, Arg3));
}

/* (LEXICAL-REFERENCE ENVIRONMENT SYMBOL)
   Returns the value of the variable with the name given in SYMBOL,
   as seen in the lexical ENVIRONMENT.

   Indistinguishable from evaluating <symbol> in <environment>.
*/
Built_In_Primitive(Prim_Lexical_Reference, 2, "LEXICAL-REFERENCE", 0x12)
{
  Primitive_2_Args();

  standard_lookup_primitive(Symbol_Lex_Ref(Arg1, Arg2));
}

/* (LOCAL-REFERENCE ENVIRONMENT SYMBOL)
   Identical to LEXICAL_REFERENCE, here for histerical reasons.
*/
Built_In_Primitive(Prim_Local_Reference, 2, "LOCAL-REFERENCE", 0x1)
{
  Primitive_2_Args();

  standard_lookup_primitive(Symbol_Lex_Ref(Arg1, Arg2));
}

/* (LOCAL-ASSIGNMENT ENVIRONMENT SYMBOL VALUE)
   Should be called *DEFINE.

   If the variable specified by SYMBOL already exists in the
   lexical ENVIRONMENT, then its value there is changed to VALUE.
   Otherwise a new binding is created in that environment linking
   the specified variable to the value.  Returns SYMBOL.

   Indistinguishable from evaluating
   (define <symbol> <value>) in <environment>.
*/
Built_In_Primitive(Prim_Local_Assignment, 3, "LOCAL-ASSIGNMENT", 0x2)
{
  Primitive_3_Args();

  standard_lookup_primitive(Local_Set(Arg1, Arg2, Arg3));
}

/* (LEXICAL-UNASSIGNED? ENVIRONMENT SYMBOL)
   Returns #!TRUE if the variable corresponding to SYMBOL is bound
   but has the special UNASSIGNED value in ENVIRONMENT.  Returns
   NIL otherwise.  Does a complete lexical search for SYMBOL
   starting in ENVIRONMENT.
   The special form (unassigned? <symbol>) is built on top of this.
*/
Built_In_Primitive(Prim_Unassigned_Test, 2, "LEXICAL-UNASSIGNED?", 0x18)
{
  Primitive_2_Args();

  standard_lookup_primitive(Symbol_Lex_unassigned_p(Arg1, Arg2));
}

/* (LEXICAL-UNBOUND? ENVIRONMENT SYMBOL)
   Returns #!TRUE if the variable corresponding to SYMBOL has no
   binding in ENVIRONMENT.  Returns NIL otherwise.  Does a complete
   lexical search for SYMBOL starting in ENVIRONMENT.
   The special form (unbound? <symbol>) is built on top of this.
*/
Built_In_Primitive(Prim_Unbound_Test, 2, "LEXICAL-UNBOUND?", 0x33)
{
  Primitive_2_Args();

  standard_lookup_primitive(Symbol_Lex_unbound_p(Arg1, Arg2));
}

/* (LEXICAL-UNREFERENCEABLE? ENVIRONMENT SYMBOL)
   Returns #T if evaluating <symbol> in <environment> would cause
   a variable lookup error (unbound or unassigned).
*/
Built_In_Primitive(Prim_Unreferenceable_Test, 2,
		   "LEXICAL-UNREFERENCEABLE?", 0x13)
{
  long Result;
  Primitive_2_Args();

  lookup_primitive_type_test();
  Result = Symbol_Lex_Ref(Arg1, Arg2);
  switch (Result)
  { case PRIM_DONE:
      PRIMITIVE_RETURN(NIL);

    case PRIM_INTERRUPT:
      Primitive_Interrupt();
      /*NOTREACHED*/

    case ERR_UNASSIGNED_VARIABLE:
    case ERR_UNBOUND_VARIABLE:
      PRIMITIVE_RETURN(TRUTH);

    default:
      Primitive_Error(Result);
  }
  /*NOTREACHED*/
}
