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

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"

void
error_out_of_channels (void)
{
  signal_error_from_primitive (ERR_OUT_OF_FILE_HANDLES);
}

void
error_out_of_processes (void)
{
  signal_error_from_primitive (ERR_OUT_OF_FILE_HANDLES);
}

void
error_unimplemented_primitive (void)
{
  signal_error_from_primitive (ERR_UNDEFINED_PRIMITIVE);
}

void
error_floating_point_exception (void)
{
  signal_error_from_primitive (ERR_FLOATING_OVERFLOW);
}

void
error_process_terminated (void)
{
  signal_error_from_primitive (ERR_PROCESS_TERMINATED);
}

int
executing_scheme_primitive_p (void)
{
  return (PRIMITIVE_P (GET_PRIMITIVE));
}

#ifdef __OS2__

void
request_attention_interrupt (void)
{
  REQUEST_INTERRUPT (INT_Global_1);
}

int
test_and_clear_attention_interrupt (void)
{
  unsigned long code;
  GRAB_INTERRUPT_REGISTERS ();
  code = GET_INT_CODE;
  CLEAR_INTERRUPT_NOLOCK (INT_Global_1);
  RELEASE_INTERRUPT_REGISTERS ();
  return ((code & INT_Global_1) != 0);
}

#endif /* __OS2__ */

void
request_console_resize_interrupt (void)
{
  REQUEST_INTERRUPT (INT_Global_3);
}

void
request_character_interrupt (void)
{
  REQUEST_INTERRUPT (INT_Character);
}

void
request_timer_interrupt (void)
{
  REQUEST_INTERRUPT (INT_Timer);
}

void
request_suspend_interrupt (void)
{
  REQUEST_INTERRUPT (INT_Suspend);
}

int
pending_interrupts_p (void)
{
  return (INTERRUPT_PENDING_P (INT_Mask));
}

void
deliver_pending_interrupts (void)
{
  if (INTERRUPT_PENDING_P (INT_Mask))
    signal_interrupt_from_primitive ();
}

unsigned long
get_interrupt_mask (void)
{
  return (GET_INT_MASK);
}

void
set_interrupt_mask (unsigned long mask)
{
  SET_INTERRUPT_MASK (mask & INT_Mask);
}

void
debug_back_trace (outf_channel stream)
{
  outf (stream, "*** Scheme Microcode Back Trace: ***\n");
  Back_Trace (stream);
  outf (stream, "*** End of Back Trace ***\n");
  outf_flush (stream);
}

void
debug_examine_memory (long address, const char * label)
{
  Print_Expression ((* ((SCHEME_OBJECT *) address)), ((char *) label));
}
