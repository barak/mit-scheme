/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/prmcon.c,v 1.1 1990/11/21 07:00:14 jinx Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#define SCM_PRMCON_C

#include "scheme.h"
#include "prims.h"
#include "prmcon.h"

void
DEFUN (suspend_primitive,
       (continuation, reentry_record_length, reentry_record),
       int continuation AND
       int reentry_record_length AND
       SCHEME_OBJECT *reentry_record)
{
  int i;
  long nargs;
  SCHEME_OBJECT primitive;

  if (continuation > CONT_MAX_INDEX)
  {
    signal_error_from_primitive (ERR_UNKNOWN_PRIMITIVE_CONTINUATION);
    /* NOTREACHED */
  }

  primitive = (Regs[REGBLOCK_PRIMITIVE]);
  if (!PRIMITIVE_P (primitive))
  {
    fprintf (stderr,
	     "\nsuspend_primitive invoked when not in primitive!\n");
    Microcode_Termination (TERM_BAD_BACK_OUT);
  }

  nargs = (PRIMITIVE_N_ARGUMENTS (primitive));

  Will_Push (CONTINUATION_SIZE + 3 + reentry_record_length);
   STACK_PUSH (primitive);
   STACK_PUSH (STACK_FRAME_HEADER + nargs);

   for (i = (reentry_record_length - 1);
	i >= 0;
	i -= 1)
   {
     STACK_PUSH (reentry_record[i]);
   }
   STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (reentry_record_length));
   Store_Expression (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
   Store_Return (RC_PRIMITIVE_CONTINUE);
   Save_Cont ();
  Pushed ();

  return;
}

SCHEME_OBJECT
DEFUN_VOID (continue_primitive)
{
  long nargs;
  int continuation, record_length;
  SCHEME_OBJECT primitive, *buffer, result;

  continuation = ((int) (UNSIGNED_FIXNUM_TO_LONG (Fetch_Expression ())));
  if (continuation > CONT_MAX_INDEX)
  {
    Store_Expression (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
    Store_Return (RC_PRIMITIVE_CONTINUE);
    Save_Cont ();
    immediate_error (ERR_UNKNOWN_PRIMITIVE_CONTINUATION);
    /* NOTREACHED */
  }
  record_length = ((int) (UNSIGNED_FIXNUM_TO_LONG (STACK_POP ())));
  if (GC_Check (record_length))
  {
    Request_GC (record_length);
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM ((long) record_length));
    Store_Expression (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
    Store_Return (RC_PRIMITIVE_CONTINUE);
    Save_Cont ();
    immediate_interrupt ();
    /* NOTREACHED */
  }

  buffer = Free;
  while ((--record_length) >= 0)
  {
    *Free++ = (STACK_POP ());
  }

  nargs = ((OBJECT_DATUM (STACK_POP ())) -
	   (STACK_ENV_FIRST_ARG - 1));
  primitive = (STACK_POP ());

  /* Most of the testing here is paranioa in case we disk-save in the
     middle of the suspension and then disk-restore into an incompatible
     microcode.
     It's not complete, but will catch some errors.
   */

  if (!IMPLEMENTED_PRIMITIVE_P (primitive))
  {
    STACK_PUSH (primitive);
    STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nargs));
    immediate_error (ERR_UNIMPLEMENTED_PRIMITIVE);
    /* NOTREACHED */
  }

  if (nargs != (PRIMITIVE_ARITY (primitive)))
  {
    if ((PRIMITIVE_ARITY (primitive)) != LEXPR_PRIMITIVE_ARITY)
    {
      STACK_PUSH (primitive);
      STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (nargs));
      immediate_error (ERR_WRONG_NUMBER_OF_ARGUMENTS);
    }
    Regs[REGBLOCK_LEXPR_ACTUALS] = ((SCHEME_OBJECT) nargs);
  }
  Store_Expression (primitive);
  Regs[REGBLOCK_PRIMITIVE] = primitive;
  result = (*(continuation_procedures[continuation]))(buffer);
  Regs[REGBLOCK_PRIMITIVE] = SHARP_F;
  POP_PRIMITIVE_FRAME (nargs);
  return (result);
}

void
DEFUN_VOID (immediate_interrupt)
{
  Setup_Interrupt (PENDING_INTERRUPTS ());
  abort_to_interpreter (PRIM_APPLY);
  /* NOTREACHED */
}

void
DEFUN (immediate_error, (error_code), long error_code)
{
  Do_Micro_Error (error_code, false);
  abort_to_interpreter (PRIM_APPLY);
  /* NOTREACHED */
}
