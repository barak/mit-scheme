/* -*-C-*-

$Id: fhooks.c,v 9.36 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1988, 1989, 1990, 1999, 2002 Massachusetts Institute of Technology

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

/* This file contains hooks and handles for the new fluid bindings
   scheme for multiprocessors. */

#include "scheme.h"
#include "prims.h"
#include "trap.h"
#include "lookup.h"
#include "locks.h"

DEFINE_PRIMITIVE ("SET-FLUID-BINDINGS!", Prim_set_fluid_bindings, 1)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, APPARENT_LIST_P);
  {
    SCHEME_OBJECT result = Fluid_Bindings;
    Fluid_Bindings = (ARG_REF (1));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("GET-FLUID-BINDINGS", Prim_get_fluid_bindings, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (Fluid_Bindings);
}

DEFINE_PRIMITIVE ("WITH-SAVED-FLUID-BINDINGS", Prim_with_saved_fluid_bindings, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT thunk = (ARG_REF (1));
    PRIMITIVE_CANONICALIZE_CONTEXT ();
    POP_PRIMITIVE_FRAME (1);
  Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
    /* Save previous fluid bindings for later restore */
    exp_register = Fluid_Bindings;
    Store_Return (RC_RESTORE_FLUIDS);
    Save_Cont ();
    /* Invoke the thunk. */
    STACK_PUSH (thunk);
    STACK_PUSH (STACK_FRAME_HEADER);
  Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
}

#define lookup_slot(environment, variable)				\
  (lookup_cell ((OBJECT_ADDRESS (variable)), (environment)))

DEFINE_PRIMITIVE ("ADD-FLUID-BINDING!", Prim_add_fluid_binding, 3, 3,
  "(ADD-FLUID-BINDING! ENVIRONMENT SYMBOL/VARIABLE VALUE)\n\
Dynamically bind SYMBOL/VARIABLE to VALUE in ENVIRONMENT.\n\
If SYMBOL/VARIABLE has not been \"fluidized\", do so first.")
{
  extern SCHEME_OBJECT * lookup_cell ();
  static SCHEME_OBJECT new_fluid_binding ();
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ENVIRONMENT_P);
  {
    fast SCHEME_OBJECT environment = (ARG_REF (1));
    fast SCHEME_OBJECT name = (ARG_REF (2));
    fast SCHEME_OBJECT * cell;
    switch (OBJECT_TYPE (name))
      {
	/* The next two cases are a temporary fix since compiler doesn't
	   do scode-quote the same way that the interpreter does.

	   Ultimately we need to redesign deep fluid-let support anyway,
	   so this will go away.
	   */

      case TC_LIST:
	cell = (lookup_slot (environment, (PAIR_CAR (name))));
	break;

      case TC_SCODE_QUOTE:
	cell =
	  (lookup_slot
	   (environment, (FAST_MEMORY_REF (name, SCODE_QUOTE_OBJECT))));
	break;

      case TC_VARIABLE:
	cell = (lookup_slot (environment, name));
	break;

      case TC_INTERNED_SYMBOL:
      case TC_UNINTERNED_SYMBOL:
	cell = (deep_lookup (environment, name, fake_variable_object));
	break;

      default:
	error_wrong_type_arg (2);
      }
    PRIMITIVE_RETURN (new_fluid_binding (cell, (ARG_REF (3)), false));
  }
}

static SCHEME_OBJECT
new_fluid_binding (cell, value, force)
     SCHEME_OBJECT * cell;
     SCHEME_OBJECT value;
     Boolean force;
{
  fast SCHEME_OBJECT trap;
  Lock_Handle set_serializer;
  SCHEME_OBJECT new_trap_value;
  long new_trap_kind = TRAP_FLUID;
  long trap_kind;
  SCHEME_OBJECT saved_extension = SHARP_F;
  SCHEME_OBJECT saved_value;

  setup_lock (set_serializer, cell);

 new_fluid_binding_restart:
  trap = (*cell);
  new_trap_value = trap;
  if (REFERENCE_TRAP_P (trap))
    {
      get_trap_kind (trap_kind, trap);
      switch (trap_kind)
	{
	case TRAP_DANGEROUS:
	  MEMORY_SET
	    (trap,
	     TRAP_TAG,
	     (LONG_TO_UNSIGNED_FIXNUM (TRAP_FLUID | (trap_kind & 1))));
	  /* Fall through */
	case TRAP_FLUID:
	case TRAP_FLUID_DANGEROUS:
	  new_trap_kind = -1;
	  break;

	case TRAP_UNBOUND:
	case TRAP_UNBOUND_DANGEROUS:
	  if (! force)
	    {
	      remove_lock (set_serializer);
	      signal_error_from_primitive (ERR_UNBOUND_VARIABLE);
	    }
	  /* Fall through */
	case TRAP_UNASSIGNED:
	case TRAP_UNASSIGNED_DANGEROUS:
	  new_trap_kind = (TRAP_FLUID | (trap_kind & 1));
	  new_trap_value = UNASSIGNED_OBJECT;
	  break;

	case TRAP_COMPILER_CACHED:
	case TRAP_COMPILER_CACHED_DANGEROUS:
	  saved_extension = (FAST_MEMORY_REF ((*cell), TRAP_EXTRA));
	  cell = (MEMORY_LOC (saved_extension, TRAP_EXTENSION_CELL));
	  update_lock (set_serializer, cell);
	  saved_value = (*cell);
	  if (REFERENCE_TRAP_P (saved_value))
	    /* No need to recache uuo links, they must already be recached. */
	    saved_extension = SHARP_F;
	  goto new_fluid_binding_restart;

	default:
	  remove_lock (set_serializer);
	  signal_error_from_primitive (ERR_ILLEGAL_REFERENCE_TRAP);
	}
    }

  if (new_trap_kind != -1)
    {
      if (GC_allocate_test (2))
	{
	  remove_lock (set_serializer);
	  Primitive_GC (2);
	}
      trap = (MAKE_POINTER_OBJECT (TC_REFERENCE_TRAP, Free));
      (*Free++) = (LONG_TO_UNSIGNED_FIXNUM (new_trap_kind));
      (*Free++) = new_trap_value;
      (*cell) = trap;
    }
  if (saved_extension != SHARP_F)
    {
      extern long recache_uuo_links ();
      long value = (recache_uuo_links (saved_extension, saved_value));
      if (value != PRIM_DONE)
	{
	  remove_lock (set_serializer);
	  if (value == PRIM_INTERRUPT)
	    signal_interrupt_from_primitive ();
	  else
	    signal_error_from_primitive (value);
	}
    }
  remove_lock (set_serializer);

  /* Fluid_Bindings is per processor private. */
  Fluid_Bindings = (cons ((cons (trap, value)), Fluid_Bindings));
  return (SHARP_F);
}
