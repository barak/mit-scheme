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
#include "ospty.h"

static Tchannel
arg_pty_master (unsigned int arg)
{
  Tchannel channel = (arg_channel (1));
  if ((OS_channel_type (channel)) != channel_type_unix_pty_master)
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
    const char * master_name;
    const char * slave_name =
      (OS_open_pty_master ((&channel), (&master_name)));
    transaction_begin ();
    OS_channel_close_on_abort (channel);
    {
      SCHEME_OBJECT vector = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (vector, 0, (long_to_integer (channel)));
      VECTOR_SET (vector, 1, (char_pointer_to_string (master_name)));
      VECTOR_SET (vector, 2, (char_pointer_to_string (slave_name)));
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
