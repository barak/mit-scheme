/* -*-C-*-

$Id: step.c,v 9.37 2002/11/20 19:46:14 cph Exp $

Copyright (c) 1987-1999, 2002 Massachusetts Institute of Technology

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

/* Support for the stepper */

#include "scheme.h"
#include "prims.h"

                 /**********************************/
                 /* Support of stepping primitives */
                 /**********************************/

/* UGLY ... this knows (a) that it is called with the primitive frame
   already popped off the stack; and (b) the order in which Save_Cont
   stores things on the stack.
*/

static void
DEFUN (Install_Traps, (Hunk3), SCHEME_OBJECT Hunk3)
{
  SCHEME_OBJECT Eval_Hook, Apply_Hook, Return_Hook;

  Stop_Trapping();
  Eval_Hook = MEMORY_REF (Hunk3, HUNK_CXR0);
  Apply_Hook = MEMORY_REF (Hunk3, HUNK_CXR1);
  Return_Hook = MEMORY_REF (Hunk3, HUNK_CXR2);
  Set_Fixed_Obj_Slot(Stepper_State, Hunk3);
  Trapping = ((Eval_Hook != SHARP_F) |
	      (Apply_Hook != SHARP_F) |
	      (Return_Hook != SHARP_F));
  return;
}

/* (PRIMITIVE-EVAL-STEP EXPRESSION ENV HUNK3)
   Evaluates EXPRESSION in ENV and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is #F, it is a null trap that does a normal EVAL,
   APPLY or return.
*/

DEFINE_PRIMITIVE ("PRIMITIVE-EVAL-STEP", Prim_eval_step, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, HUNK3_P);
  {
    SCHEME_OBJECT expression = (ARG_REF (1));
    SCHEME_OBJECT environment = (ARG_REF (2));
    SCHEME_OBJECT hooks = (ARG_REF (3));
    PRIMITIVE_CANONICALIZE_CONTEXT ();
    POP_PRIMITIVE_FRAME (3);
    Install_Traps (hooks);
    exp_register = expression;
    env_register = environment;
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
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (3, HUNK3_P);
  {
    SCHEME_OBJECT hooks = (ARG_REF (3));
    fast long number_of_args = 0;
    {
      SCHEME_OBJECT procedure = (ARG_REF (1));
      SCHEME_OBJECT argument_list = (ARG_REF (2));
      {
	fast SCHEME_OBJECT scan_list;
	TOUCH_IN_PRIMITIVE (argument_list, scan_list);
	while (PAIR_P (scan_list))
	  {
	    number_of_args += 1;
	    TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
	  }
	if (scan_list != EMPTY_LIST)
	  error_wrong_type_arg (2);
      }
      POP_PRIMITIVE_FRAME (3);
      Install_Traps (hooks);
      {
	fast SCHEME_OBJECT * scan_stack = (STACK_LOC (- number_of_args));
	fast SCHEME_OBJECT scan_list;
	fast long i;
	Will_Push (number_of_args + STACK_ENV_EXTRA_SLOTS + 1);
	sp_register = scan_stack;
	TOUCH_IN_PRIMITIVE (argument_list, scan_list);
	for (i = number_of_args; (i > 0); i -= 1)
	  {
	    (*scan_stack++) = (PAIR_CAR (scan_list));
	    TOUCH_IN_PRIMITIVE ((PAIR_CDR (scan_list)), scan_list);
	  }
	STACK_PUSH (procedure);
	STACK_PUSH (STACK_FRAME_HEADER + number_of_args);
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
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  CHECK_ARG (2, HUNK3_P);
  {
    SCHEME_OBJECT value = (ARG_REF (1));
    SCHEME_OBJECT hooks = (ARG_REF (2));

    POP_PRIMITIVE_FRAME (2); 
    Install_Traps (hooks);
    val_register = value;
    PRIMITIVE_ABORT (PRIM_NO_TRAP_POP_RETURN);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}
