/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prospty.c,v 1.1 1992/05/05 06:35:16 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
#include "ospty.h"

static Tchannel
DEFUN (arg_pty_master, (arg), unsigned int arg)
{
  Tchannel channel = (arg_channel (1));
  if ((OS_channel_type (channel)) != channel_type_pty_master)
    error_bad_range_arg (1);
  return (channel);
}

DEFINE_PRIMITIVE ("OPEN-PTY-MASTER", Prim_open_pty_master, 0, 0,
  "Open a PTY master, returning the master's channel and the slave's name.\n\
Returns a vector #(CHANNEL MASTER-NAME SLAVE-NAME).")
{
  PRIMITIVE_HEADER (0);
  {
    Tchannel channel;
    CONST char * master_name;
    CONST char * slave_name =
      (OS_open_pty_master ((&channel), (&master_name)));
    transaction_begin ();
    OS_channel_close_on_abort (channel);
    {
      SCHEME_OBJECT vector = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (vector, 0, (long_to_integer (channel)));
      VECTOR_SET (vector, 1,
		  (char_pointer_to_string ((unsigned char *) master_name)));
      VECTOR_SET (vector, 2,
		  (char_pointer_to_string ((unsigned char *) slave_name)));
      transaction_commit ();
      PRIMITIVE_RETURN (vector);
    }
  }
}

DEFINE_PRIMITIVE ("PTY-MASTER-SEND-SIGNAL", Prim_pty_master_send_signal, 2, 2,
  "Send a signal to PTY-MASTER; second arg says which one.")
{
  PRIMITIVE_HEADER (2);
  OS_pty_master_send_signal ((arg_pty_master (1)),
			     (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-KILL", Prim_pty_master_kill, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_kill (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-STOP", Prim_pty_master_stop, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_stop (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-CONTINUE", Prim_pty_master_continue, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_continue (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-INTERRUPT", Prim_pty_master_interrupt, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_interrupt (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-QUIT", Prim_pty_master_quit, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_quit (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PTY-MASTER-HANGUP", Prim_pty_master_hangup, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  OS_pty_master_hangup (arg_pty_master (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
