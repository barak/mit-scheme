/* -*-C-*-

$Id: os2pipe.c,v 1.1 1994/11/28 03:42:59 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

static void input_pipe_operator
  (Tchannel, chop_t, choparg_t, choparg_t, choparg_t);
static void input_pipe_thread (void *);

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
    {
      channel_context_t * context = (OS2_make_channel_context ());
      (CHANNEL_OPERATOR_CONTEXT (channel)) = context;
      OS2_open_qid ((CHANNEL_CONTEXT_READER_QID (context)), OS2_scheme_tqueue);
      (void) OS2_beginthread
	(input_pipe_thread, (CHANNEL_POINTER (channel)), 0);
      (CHANNEL_OPERATOR (channel)) = input_pipe_operator;
    }
}

static void
input_pipe_operator (Tchannel channel, chop_t operation,
		     choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  switch (operation)
    {
    case chop_read:
      (* ((long *) arg3))
	= (channel_thread_read (channel, ((char *) arg1), ((size_t) arg2)));
      break;
    case chop_close:
      channel_thread_close (channel);
      break;
    default:
      OS2_logic_error ("Unknown operation for input pipe.");
      break;
    }
}

static void
input_pipe_thread (void * arg)
{
  Tchannel channel = (* ((Tchannel *) arg));
  LHANDLE handle = (CHANNEL_HANDLE (channel));
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  qid_t qid = (CHANNEL_CONTEXT_WRITER_QID (context));
  OS2_open_qid (qid, (OS2_make_std_tqueue ()));
  (void) OS2_thread_initialize (qid);
  while (1)
    {
      msg_t * message = (OS2_make_readahead ());
      APIRET rc
	= (dos_read (handle,
		     (SM_READAHEAD_DATA (message)),
		     (sizeof (SM_READAHEAD_DATA (message))),
		     (& (SM_READAHEAD_SIZE (message)))));
      int eofp;
      if (rc == NO_ERROR)
	eofp = ((SM_READAHEAD_SIZE (message)) == 0);
      else
	{
	  OS2_destroy_message (message);
	  if (rc == ERROR_INVALID_HANDLE)
	    /* Handle was closed on us -- no need to do anything else.  */
	    break;
	  message = (OS2_make_syscall_error (rc, syscall_dos_read));
	  eofp = (rc == ERROR_BROKEN_PIPE);
	}
      OS2_send_message (qid, message);
      if (eofp)
	break;
      OS2_wait_for_readahead_ack (qid);
    }
  OS2_endthread ();
}
