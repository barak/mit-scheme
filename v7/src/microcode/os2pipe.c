/* -*-C-*-

$Id: os2pipe.c,v 1.7 1996/05/09 20:21:56 cph Exp $

Copyright (c) 1994-96 Massachusetts Institute of Technology

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
