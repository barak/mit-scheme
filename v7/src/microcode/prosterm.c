/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosterm.c,v 1.2 1990/10/16 20:53:32 cph Exp $

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

/* Primitives to control terminal devices. */

#include "scheme.h"
#include "prims.h"
#include "osterm.h"
#include "osio.h"

static Tchannel
DEFUN (arg_terminal, (argument_number), int argument_number)
{
  Tchannel channel = (arg_channel (argument_number));
  enum channel_type type = (OS_channel_type (channel));
  if (! ((type == channel_type_terminal) || (type == channel_type_pty_master)))
    error_bad_range_arg (argument_number);
  return (channel);
}

DEFINE_PRIMITIVE ("TERMINAL-READ-CHAR", Prim_terminal_read_char, 1, 1,
  "Read and return a single character from TERMINAL.")
{
  PRIMITIVE_HEADER (1);
  {
    int c = (OS_terminal_read_char (arg_terminal (1)));
    PRIMITIVE_RETURN ((c < 0) ? SHARP_F : (ASCII_TO_CHAR (c)));
  }
}

DEFINE_PRIMITIVE ("TERMINAL-CHAR-READY?", Prim_terminal_char_ready_p, 2, 2,
  "Return #T if a character from TERMINAL.\n\
Second arg DELAY says how long to wait for one to arrive, in milliseconds.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (OS_terminal_char_ready_p ((arg_terminal (1)),
				(arg_nonnegative_integer (2)))));
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
    (long_to_integer (OS_baud_index_to_rate [arg_baud_index (1)]));
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

DEFINE_PRIMITIVE ("OPEN-PTY-MASTER", Prim_open_pty_master, 0, 0,
  "Open a PTY master, returning the master's channel and the slave's name.\n\
The result is a pair whose car is a channel and whose cdr is a filename.\n\
If no PTY can be opened, #F is returned.")
{
  PRIMITIVE_HEADER (0);
  {
    Tchannel channel;
    CONST char * master_name;
    CONST char * slave_name =
      (OS_open_pty_master ((&channel), (&master_name)));
    if (slave_name == 0)
      PRIMITIVE_RETURN (SHARP_F);
    {
      SCHEME_OBJECT vector = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (vector, 0, (long_to_integer (channel)));
      VECTOR_SET (vector, 1, (char_pointer_to_string (master_name)));
      VECTOR_SET (vector, 2, (char_pointer_to_string (slave_name)));
      PRIMITIVE_RETURN (vector);
    }
  }
}

DEFINE_PRIMITIVE ("PTY-MASTER-SEND-SIGNAL", Prim_pty_master_send_signal, 2, 2,
  "Send a signal to PTY-MASTER; second arg says which one.")
{
  PRIMITIVE_HEADER (2);
  {
    Tchannel channel = (arg_channel (1));
    if ((OS_channel_type (channel)) != channel_type_pty_master)
      error_bad_range_arg (1);
    OS_pty_master_send_signal (channel, (arg_nonnegative_integer (2)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
