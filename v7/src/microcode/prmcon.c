/* -*-C-*-

$Id: prmcon.c,v 1.9 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

  primitive = (Registers[REGBLOCK_PRIMITIVE]);
  if (!PRIMITIVE_P (primitive))
  {
    outf_fatal ("\nsuspend_primitive invoked when not in primitive!\n");
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
   exp_register = (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
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

  continuation = ((int) (UNSIGNED_FIXNUM_TO_LONG (exp_register)));
  if (continuation > CONT_MAX_INDEX)
  {
    exp_register = (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
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
    exp_register = (LONG_TO_UNSIGNED_FIXNUM ((long) continuation));
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
    (Registers[REGBLOCK_LEXPR_ACTUALS]) = ((SCHEME_OBJECT) nargs);
  }
  exp_register = primitive;
  (Registers[REGBLOCK_PRIMITIVE]) = primitive;
  result = (*(continuation_procedures[continuation]))(buffer);
  (Registers[REGBLOCK_PRIMITIVE]) = SHARP_F;
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
