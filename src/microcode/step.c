/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* Support for the stepper */

#include "scheme.h"
#include "prims.h"

/* UGLY ... this knows (a) that it is called with the primitive frame
   already popped off the stack; and (b) the order in which SAVE_CONT
   stores things on the stack.  */

static void
install_traps (SCHEME_OBJECT state)
{
  VECTOR_SET (fixed_objects, STEPPER_STATE, state);
  trapping
    = ((OBJECT_TO_BOOLEAN (MEMORY_REF (state, HUNK_CXR0)))
       || (OBJECT_TO_BOOLEAN (MEMORY_REF (state, HUNK_CXR1)))
       || (OBJECT_TO_BOOLEAN (MEMORY_REF (state, HUNK_CXR2))));
}

/* (PRIMITIVE-EVAL-STEP EXPRESSION ENV HUNK3)
   Evaluates EXPRESSION in ENV and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is #F, it is a null trap that does a normal EVAL,
   APPLY or return.  */

DEFINE_PRIMITIVE ("PRIMITIVE-EVAL-STEP", Prim_eval_step, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, HUNK3_P);
  {
    SCHEME_OBJECT expression = (ARG_REF (1));
    SCHEME_OBJECT environment = (ARG_REF (2));
    SCHEME_OBJECT hooks = (ARG_REF (3));
    canonicalize_primitive_context ();
    POP_PRIMITIVE_FRAME (3);
    install_traps (hooks);
    SET_EXP (expression);
    SET_ENV (environment);
  }
  PRIMITIVE_ABORT (PRIM_NO_TRAP_EVAL);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* (PRIMITIVE-APPLY-STEP OPERATOR OPERANDS HUNK3)
   Applies OPERATOR to OPERANDS and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is #F, it is a null trap that does a normal EVAL,
   APPLY or return.

   Mostly a copy of Prim_Apply, since this, too, must count the space
   required before actually building a frame */

DEFINE_PRIMITIVE ("PRIMITIVE-APPLY-STEP", Prim_apply_step, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  canonicalize_primitive_context ();
  CHECK_ARG (3, HUNK3_P);
  {
    SCHEME_OBJECT hooks = (ARG_REF (3));
    long number_of_args = 0;
    {
      SCHEME_OBJECT procedure = (ARG_REF (1));
      SCHEME_OBJECT argument_list = (ARG_REF (2));
      {
	SCHEME_OBJECT scan_list;
	scan_list = argument_list;
	while (PAIR_P (scan_list))
	  {
	    number_of_args += 1;
	    scan_list = (PAIR_CDR (scan_list));
	  }
	if (!EMPTY_LIST_P (scan_list))
	  error_wrong_type_arg (2);
      }
      POP_PRIMITIVE_FRAME (3);
      install_traps (hooks);
      {
	SCHEME_OBJECT * scan_stack = (STACK_LOC (- number_of_args));
	SCHEME_OBJECT scan_list;
	long i;
	Will_Push (number_of_args + STACK_ENV_EXTRA_SLOTS + 1);
	stack_pointer = scan_stack;
	scan_list = argument_list;
	for (i = number_of_args; (i > 0); i -= 1)
	  {
	    (*scan_stack++) = (PAIR_CAR (scan_list));
	    scan_list = (PAIR_CDR (scan_list));
	  }
	STACK_PUSH (procedure);
	PUSH_APPLY_FRAME_HEADER (number_of_args);
	Pushed ();
      }
    }
  }
  PRIMITIVE_ABORT (PRIM_NO_TRAP_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* (PRIMITIVE-RETURN-STEP VALUE HUNK3)
   Returns VALUE and intalls the eval-trap, apply-trap, and
   return-trap from HUNK3.  If any trap is #F, it is a null trap
   that does a normal EVAL, APPLY or return.
*/

DEFINE_PRIMITIVE ("PRIMITIVE-RETURN-STEP", Prim_return_step, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  canonicalize_primitive_context ();
  CHECK_ARG (2, HUNK3_P);
  {
    SCHEME_OBJECT value = (ARG_REF (1));
    SCHEME_OBJECT hooks = (ARG_REF (2));

    POP_PRIMITIVE_FRAME (2);
    install_traps (hooks);
    SET_VAL (value);
    PRIMITIVE_ABORT (PRIM_NO_TRAP_POP_RETURN);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}
