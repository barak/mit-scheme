/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosterm.c,v 1.12 1992/05/05 06:34:14 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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

/* Primitives to control terminal devices. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"
#include "osio.h"

Tchannel
DEFUN (arg_terminal, (argument_number), int argument_number)
{
  Tchannel channel = (arg_channel (argument_number));
  enum channel_type type = (OS_channel_type (channel));
  if (! ((type == channel_type_terminal) || (type == channel_type_pty_master)))
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
    OS_terminal_get_state ((arg_terminal (1)), (STRING_LOC (result, 0)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("TERMINAL-SET-STATE", Prim_terminal_set_state, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, STRING_P);
  {
    SCHEME_OBJECT state = (ARG_REF (2));
    if ((STRING_LENGTH (state)) != (OS_terminal_state_size ()))
      error_bad_range_arg (2);
    OS_terminal_set_state ((arg_terminal (1)), (STRING_LOC (state, 0)));
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
