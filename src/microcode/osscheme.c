/* -*-C-*-

$Id: osscheme.c,v 1.14 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1990-2000, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"

extern void
  EXFUN (signal_error_from_primitive, (long error_code));

void
DEFUN_VOID (error_out_of_channels)
{
  signal_error_from_primitive (ERR_OUT_OF_FILE_HANDLES);
}

void
DEFUN_VOID (error_out_of_processes)
{
  signal_error_from_primitive (ERR_OUT_OF_FILE_HANDLES);
}

void
DEFUN_VOID (error_unimplemented_primitive)
{
  signal_error_from_primitive (ERR_UNDEFINED_PRIMITIVE);
}

void
DEFUN_VOID (error_floating_point_exception)
{
  signal_error_from_primitive (ERR_FLOATING_OVERFLOW);
}

int
DEFUN_VOID (executing_scheme_primitive_p)
{
  return (PRIMITIVE_P (Registers[REGBLOCK_PRIMITIVE]));
}

#ifdef __OS2__

void
DEFUN_VOID (request_attention_interrupt)
{
  REQUEST_INTERRUPT (INT_Global_1);
}

int
DEFUN_VOID (test_and_clear_attention_interrupt)
{
  long code;
  GRAB_INTERRUPT_REGISTERS ();
  code = (FETCH_INTERRUPT_CODE ());
  CLEAR_INTERRUPT_NOLOCK (INT_Global_1);
  RELEASE_INTERRUPT_REGISTERS ();
  return ((code & INT_Global_1) != 0);
}

#endif /* __OS2__ */

void
DEFUN_VOID (request_character_interrupt)
{
  REQUEST_INTERRUPT (INT_Character);
}

void
DEFUN_VOID (request_timer_interrupt)
{
  REQUEST_INTERRUPT (INT_Timer);
}

void
DEFUN_VOID (request_suspend_interrupt)
{
  REQUEST_INTERRUPT (INT_Suspend);
  return;
}

int
DEFUN_VOID (pending_interrupts_p)
{
  return (INTERRUPT_PENDING_P (INT_Mask));
}

void
DEFUN_VOID (deliver_pending_interrupts)
{
  if (INTERRUPT_PENDING_P (INT_Mask))
    signal_interrupt_from_primitive ();
  return;
}

long
DEFUN_VOID (get_interrupt_mask)
{
  return (FETCH_INTERRUPT_MASK ());
}

void
DEFUN (set_interrupt_mask, (mask), long mask)
{
  SET_INTERRUPT_MASK (mask & INT_Mask);
  return;
}

void
DEFUN (debug_back_trace, (stream), outf_channel stream)
{
  outf (stream, "*** Scheme Microcode Back Trace: ***\n");
  Back_Trace (stream);
  outf (stream, "*** End of Back Trace ***\n");
  outf_flush (stream);
  return;
}

void
DEFUN (debug_examine_memory, (address, label),
       long address AND
       CONST char * label)
{
  Print_Expression ((* ((SCHEME_OBJECT *) address)), ((char *) label));
  return;
}
