/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Primitives to control terminal devices. */

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"
#include "osterm.h"
#include "osio.h"

Tchannel
arg_terminal (int argument_number)
{
  Tchannel channel = (arg_channel (argument_number));
  enum channel_type type = (OS_channel_type (channel));
  if (! ((type == channel_type_terminal)
	 || (type == channel_type_unix_pty_master)
	 || (type == channel_type_os2_console)))
    error_bad_range_arg (argument_number);
  return (channel);
}

DEFINE_PRIMITIVE ("TERMINAL-GET-ISPEED", Prim_terminal_get_ispeed, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (OS_terminal_get_ispeed (arg_terminal (1))));
}

DEFINE_PRIMITIVE ("TERMINAL-GET-OSPEED", Prim_terminal_get_ospeed, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (OS_terminal_get_ospeed (arg_terminal (1))));
}

DEFINE_PRIMITIVE ("TERMINAL-SET-ISPEED", Prim_terminal_set_ispeed, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS_terminal_set_ispeed ((arg_terminal (1)), (arg_baud_index (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-SET-OSPEED", Prim_terminal_set_ospeed, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS_terminal_set_ospeed ((arg_terminal (1)), (arg_baud_index (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("BAUD-INDEX->RATE", Prim_baud_index_to_rate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (OS_baud_index_to_rate (arg_baud_index (1))));
}

DEFINE_PRIMITIVE ("BAUD-RATE->INDEX", Prim_baud_rate_to_index, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    int index = (OS_baud_rate_to_index (arg_nonnegative_integer (1)));
    if (index < 0)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (long_to_integer (index));
  }
}

DEFINE_PRIMITIVE ("TERMINAL-GET-STATE", Prim_terminal_get_state, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT result = (allocate_string (OS_terminal_state_size ()));
    OS_terminal_get_state ((arg_terminal (1)), (STRING_POINTER (result)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("TERMINAL-SET-STATE", Prim_terminal_set_state, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT state = (ARG_REF (2));
    if (((unsigned int) (STRING_LENGTH (state)))
	!= (OS_terminal_state_size ()))
      error_bad_range_arg (2);
    OS_terminal_set_state ((arg_terminal (1)), (STRING_POINTER (state)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-COOKED-OUTPUT?", Prim_terminal_cooked_output_p, 1, 1,
  "Return #F iff TERMINAL is not in cooked output mode.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS_terminal_cooked_output_p (arg_terminal (1))));
}

DEFINE_PRIMITIVE ("TERMINAL-RAW-OUTPUT", Prim_terminal_raw_output, 1, 1,
  "Put TERMINAL into raw output mode.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_raw_output (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-COOKED-OUTPUT", Prim_terminal_cooked_output, 1, 1,
  "Put TERMINAL into cooked output mode.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_cooked_output (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-BUFFERED?", Prim_terminal_buffered_p, 1, 1,
  "Return #F iff TERMINAL is not in buffered mode.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS_terminal_buffered_p (arg_terminal (1))));
}

DEFINE_PRIMITIVE ("TERMINAL-BUFFERED", Prim_terminal_buffered, 1, 1,
  "Put TERMINAL into buffered mode.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_buffered (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-NONBUFFERED", Prim_terminal_nonbuffered, 1, 1,
  "Put TERMINAL into nonbuffered mode.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_nonbuffered (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-FLUSH-INPUT", Prim_terminal_flush_input, 1, 1,
  "Discard any characters in TERMINAL's input buffer.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_flush_input (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-FLUSH-OUTPUT", Prim_terminal_flush_output, 1, 1,
  "Discard any characters in TERMINAL's output buffer.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_flush_output (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TERMINAL-DRAIN-OUTPUT", Prim_terminal_drain_output, 1, 1,
  "Wait until all characters in TERMINAL's output buffer have been sent.")
{
  PRIMITIVE_HEADER (1);
  OS_terminal_drain_output (arg_terminal (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS-JOB-CONTROL?", Prim_os_job_control_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_job_control_p ()));
}

DEFINE_PRIMITIVE ("HAVE-PTYS?", Prim_have_ptys_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_have_ptys_p ()));
}
