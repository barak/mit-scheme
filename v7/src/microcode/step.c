/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/step.c,v 9.29 1991/07/18 16:01:27 markf Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

void
Install_Traps (Hunk3)
     SCHEME_OBJECT Hunk3;
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
   trap is '(), it is a null trap that does a normal EVAL,
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
    Store_Expression (expression);
    Store_Env (environment);
  }
  PRIMITIVE_ABORT (PRIM_NO_TRAP_EVAL);
  /*NOTREACHED*/
}

/* (PRIMITIVE-APPLY-STEP OPERATOR OPERANDS HUNK3)
   Applies OPERATOR to OPERANDS and intalls the eval-trap,
   apply-trap, and return-trap from HUNK3.  If any
   trap is '(), it is a null trap that does a normal EVAL,
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
	Stack_Pointer = scan_stack;
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
}

/* (PRIMITIVE-RETURN-STEP VALUE HUNK3)
   Returns VALUE and intalls the eval-trap, apply-trap, and
   return-trap from HUNK3.  If any trap is '(), it is a null trap
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
    Val = (value);
    PRIMITIVE_ABORT (PRIM_NO_TRAP_POP_RETURN);
  }
}
