/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osscheme.c,v 1.3 1991/10/29 22:55:11 jinx Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "osscheme.h"

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
  return (PRIMITIVE_P (Regs [REGBLOCK_PRIMITIVE]));
}

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
}

void
DEFUN_VOID (debug_back_trace)
{
  Back_Trace (stdout);
}

void
DEFUN (debug_examine_memory, (address, label),
       long address AND
       CONST char * label)
{
  Print_Expression ((* ((SCHEME_OBJECT *) address)), ((char *) label));
}
