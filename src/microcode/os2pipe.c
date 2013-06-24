/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

#include "os2.h"

static msg_t * input_pipe_reader (LHANDLE, qid_t, msg_t *, int *);
static void input_pipe_operator
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);

void
OS_make_pipe (Tchannel * readerp, Tchannel * writerp)
{
  HFILE hread;
  HFILE hwrite;
  STD_API_CALL (dos_create_pipe, ((&hread), (&hwrite), 4096));
  transaction_begin ();
  OS2_handle_close_on_abort (hwrite);
  (*readerp) = (OS2_make_channel (hread, CHANNEL_READ));
  transaction_commit ();
  transaction_begin ();
  OS_channel_close_on_abort (*readerp);
  (*writerp) = (OS2_make_channel (hwrite, CHANNEL_WRITE));
  transaction_commit ();
}

void
OS2_initialize_pipe_channel (Tchannel channel)
{
  if (CHANNEL_INPUTP (channel))
    OS2_start_channel_thread (channel,
			      input_pipe_reader,
			      input_pipe_operator);
}

static msg_t *
input_pipe_reader (LHANDLE handle, qid_t qid, msg_t * message, int * eofp)
{
  ULONG nread;
  APIRET rc
    = (dos_read (handle,
		 (SM_READAHEAD_DATA (message)),
		 (sizeof (SM_READAHEAD_DATA (message))),
		 (& nread)));
  if (rc == NO_ERROR)
    {
      (SM_READAHEAD_SIZE (message)) = nread;
      (*eofp) = (nread == 0);
      return (message);
    }
  OS2_destroy_message (message);
  if (rc == ERROR_INVALID_HANDLE)
    /* Handle was closed on us -- no need to do anything else.  */
    return (0);
  (*eofp) = (rc == ERROR_BROKEN_PIPE);
  return (OS2_make_syscall_error (rc, syscall_dos_read));
}

static void
input_pipe_operator (Tchannel channel, chop_t operation,
		     choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  switch (operation)
    {
    case chop_read:
      OS2_channel_thread_read_op (channel, arg1, arg2, arg3);
      break;
    case chop_close:
      OS2_channel_thread_close (channel);
      STD_API_CALL (dos_close, (CHANNEL_HANDLE (channel)));
      break;
    default:
      OS2_logic_error ("Unknown operation for input pipe.");
      break;
    }
}
