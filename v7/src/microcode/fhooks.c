/* -*-C-*-

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/fhooks.c,v 9.28 1988/08/15 20:46:39 cph Exp $
 *
 * This file contains hooks and handles for the new fluid bindings
 * scheme for multiprocessors.
 */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"
#include "locks.h"

/* (WITH-SAVED-FLUID-BINDINGS THUNK)
   Executes THUNK, then restores the previous fluid bindings.
*/

DEFINE_PRIMITIVE ("WITH-SAVED-FLUID-BINDINGS", Prim_with_saved_fluid_bindings, 1, 1, 0)
{
  Primitive_1_Arg();

  Pop_Primitive_Frame(1);

  /* Save previous fluid bindings for later restore */

 Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
  Store_Expression(Fluid_Bindings);
  Store_Return(RC_RESTORE_FLUIDS);
  Save_Cont();
  Push(Arg1);
  Push(STACK_FRAME_HEADER);
 Pushed();
  PRIMITIVE_ABORT(PRIM_APPLY);
}

/* Utilities for the primitives below. */

Pointer
*lookup_slot(env, var)
{
  Pointer *cell, *hunk, value;
  long trap_kind;

  hunk = Get_Pointer(var);
  lookup(cell, env, hunk, repeat_slot_lookup);
  
  value = Fetch(cell[0]);

  if (Type_Code(value) != TC_REFERENCE_TRAP)
  {
    return cell;
  }

  get_trap_kind(trap_kind, value);
  switch(trap_kind)
  {
    case TRAP_DANGEROUS:
    case TRAP_UNBOUND_DANGEROUS:
    case TRAP_UNASSIGNED_DANGEROUS:
    case TRAP_FLUID_DANGEROUS:
    case TRAP_COMPILER_CACHED_DANGEROUS:
      return deep_lookup(env, hunk[VARIABLE_SYMBOL], hunk);

    case TRAP_COMPILER_CACHED:
    case TRAP_FLUID:
    case TRAP_UNBOUND:
    case TRAP_UNASSIGNED:
      return cell;

    default:
      Primitive_Error(ERR_ILLEGAL_REFERENCE_TRAP);
  }
}

Pointer
new_fluid_binding(cell, value, force)
     Pointer *cell;
     Pointer value;
     Boolean force;
{
  fast Pointer trap;
  Lock_Handle set_serializer;
  Pointer new_trap_value;
  long new_trap_kind, trap_kind;

  new_trap_kind = TRAP_FLUID;
  setup_lock(set_serializer, cell);

new_fluid_binding_restart:

  trap = *cell;
  new_trap_value = trap;

  if (Type_Code(trap) == TC_REFERENCE_TRAP)
  {
    get_trap_kind(trap_kind, trap);
    switch(trap_kind)
    {
      case TRAP_DANGEROUS:
        Vector_Set(trap,
		   TRAP_TAG,
		   Make_Unsigned_Fixnum(TRAP_FLUID | (trap_kind & 1)));

	/* Fall through */
      case TRAP_FLUID:
      case TRAP_FLUID_DANGEROUS:
	new_trap_kind = -1;
	break;

      case TRAP_UNBOUND:
      case TRAP_UNBOUND_DANGEROUS:
	if (!force)
	{
	  remove_lock(set_serializer);
	  Primitive_Error(ERR_UNBOUND_VARIABLE);
	}
	/* Fall through */
      case TRAP_UNASSIGNED:
      case TRAP_UNASSIGNED_DANGEROUS:
	new_trap_kind = (TRAP_FLUID | (trap_kind & 1));
	new_trap_value = UNASSIGNED_OBJECT;
	break;

      case TRAP_COMPILER_CACHED:
      case TRAP_COMPILER_CACHED_DANGEROUS:
	cell = Nth_Vector_Loc(Fast_Vector_Ref(*cell, TRAP_EXTRA),
			      TRAP_EXTENSION_CELL);
	update_lock(set_serializer, cell);
	goto new_fluid_binding_restart;

      default:
	remove_lock(set_serializer);
	Primitive_Error(ERR_ILLEGAL_REFERENCE_TRAP);
    }
  }

  if (new_trap_kind != -1)
  {
    if (GC_allocate_test(2))
    {
      remove_lock(set_serializer);
      Primitive_GC(2);
    }
    trap = Make_Pointer(TC_REFERENCE_TRAP, Free);
    *Free++ = Make_Unsigned_Fixnum(new_trap_kind);
    *Free++ = new_trap_value;
    *cell = trap;
  }
  remove_lock(set_serializer);

  /* Fluid_Bindings is per processor private. */

  Primitive_GC_If_Needed(4);
  Free[CONS_CAR] = Make_Pointer(TC_LIST, (Free + 2));
  Free[CONS_CDR] = Fluid_Bindings;
  Fluid_Bindings = Make_Pointer(TC_LIST, Free);
  Free += 2;
  Free[CONS_CAR] = trap;
  Free[CONS_CDR] = value;
  Free += 2;

  return NIL;
}

/* (ADD-FLUID-BINDING!  ENVIRONMENT SYMBOL-OR-VARIABLE VALUE)
      Looks up symbol-or-variable in environment.  If it has not been
      fluidized, fluidizes it.  A fluid binding with the specified 
      value is created in this interpreter's fluid bindings.      
*/

DEFINE_PRIMITIVE ("ADD-FLUID-BINDING!", Prim_add_fluid_binding, 3, 3, 0)
{
  Pointer *cell;
  Primitive_3_Args();

  if (Arg1 != GLOBAL_ENV)
    Arg_1_Type(TC_ENVIRONMENT);

  switch (Type_Code(Arg2))
  {
    case TC_VARIABLE:
      cell = lookup_slot(Arg1, Arg2);
      break;

    case TC_INTERNED_SYMBOL:
    case TC_UNINTERNED_SYMBOL:
      cell = deep_lookup(Arg1, Arg2, fake_variable_object);
      break;

    default:
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  }

  PRIMITIVE_RETURN(new_fluid_binding(cell, Arg3, false));
}

/* (MAKE-FLUID-BINDING! ENVIRONMENT SYMBOL-OR-VARIABLE VALUE)
      Looks up symbol-or-variable in environment.  If it has not been
      fluidized, fluidizes it.  A fluid binding with the specified 
      value is created in this interpreter's fluid bindings.  Unlike
      ADD-FLUID-BINDING!, it is not an error to discover no binding
      for this variable; a fluid binding will be made anyway.  This is
      simple in the global case, since there is always a value slot
      available in the symbol itself.  If the last frame searched
      in the environment chain is closed (does not have a parent
      and does not allow search of the global environment), an AUX
      binding must be established in the last frame.
*/

DEFINE_PRIMITIVE ("MAKE-FLUID-BINDING!", Prim_make_fluid_binding, 3, 3, 0)
{
  extern Pointer *force_definition();
  Pointer *cell;
  Primitive_3_Args();

  if (Arg1 != GLOBAL_ENV)
    Arg_1_Type(TC_ENVIRONMENT);

  switch (Type_Code(Arg2))
  {
    case TC_VARIABLE:
      cell = lookup_slot(Arg1, Arg2);
      break;

    case TC_INTERNED_SYMBOL:
    case TC_UNINTERNED_SYMBOL:
      cell = deep_lookup(Arg1, Arg2, fake_variable_object);
      break;

    default:
      Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  }

  if (cell == unbound_trap_object)
  {
    long message;

    /* This only happens when global is not allowed,
       only provided for completeness.
     */

    cell = force_definition(Arg1,
			    ((Type_Code(Arg2) == TC_VARIABLE) ?
			     Vector_Ref(Arg2, VARIABLE_SYMBOL) :
			     Arg2)
			    &message);

    if (message != PRIM_DONE)
    {
      if (message == PRIM_INTERRUPT)
      {
	Primitive_Interrupt();
      }
      else
      {
	Primitive_Error(message);
      }
    }
  }

  PRIMITIVE_RETURN(new_fluid_binding(cell, Arg3, true));
}
