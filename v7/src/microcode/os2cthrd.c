/* -*-C-*-

$Id: os2cthrd.c,v 1.14 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Scheme side of channel thread interface */

#include "os2.h"

static void run_channel_thread (void *);
static void start_readahead_thread (channel_context_t *);
static void send_readahead_ack (qid_t, enum readahead_ack_action);
static msg_t * new_message (void);

typedef struct
{
  LHANDLE handle;
  qid_t qid;
  channel_reader_t reader;
} thread_arg_t;

void
OS2_start_channel_thread (Tchannel channel,
			  channel_reader_t reader,
			  channel_op_t operator)
{
  channel_context_t * context = (OS2_make_channel_context ());
  thread_arg_t * arg = (OS_malloc (sizeof (thread_arg_t)));
  (CHANNEL_OPERATOR_CONTEXT (channel)) = context;
  OS2_open_qid ((CHANNEL_CONTEXT_READER_QID (context)), OS2_scheme_tqueue);
  OS2_open_qid
    ((CHANNEL_CONTEXT_WRITER_QID (context)), (OS2_make_std_tqueue ()));
  (arg -> handle) = (CHANNEL_HANDLE (channel));
  (arg -> qid) = (CHANNEL_CONTEXT_WRITER_QID (context));
  (arg -> reader) = reader;
  (CHANNEL_CONTEXT_TID (context))
    = (OS2_beginthread (run_channel_thread, arg, 0));
  (CHANNEL_OPERATOR (channel)) = operator;
}

static void
run_channel_thread (void * arg)
{
  LHANDLE handle = (((thread_arg_t *) arg) -> handle);
  qid_t qid = (((thread_arg_t *) arg) -> qid);
  channel_reader_t reader = (((thread_arg_t *) arg) -> reader);
  EXCEPTIONREGISTRATIONRECORD registration;
  OS_free (arg);
  (void) OS2_thread_initialize ((&registration), qid);
  /* Wait for first read request before doing anything.  */
  while ((OS2_wait_for_readahead_ack (qid)) == raa_read)
    {
      int eofp;
      msg_t * message
	= ((*reader) (handle, qid, (OS2_make_readahead ()), (&eofp)));
      if (message == 0)
	break;
      OS2_send_message (qid, message);
      if (eofp)
	break;
    }
  {
    tqueue_t * tqueue = (OS2_qid_tqueue (qid));
    OS2_close_qid (qid);
    OS2_close_std_tqueue (tqueue);
  }
  OS2_endthread ();
}

void
OS2_channel_thread_read_op (Tchannel channel,
			    choparg_t arg1, choparg_t arg2, choparg_t arg3)
{
  (* ((long *) arg3))
    = (OS2_channel_thread_read
       (channel, ((char *) arg1), ((size_t) arg2)));
}

void
OS2_initialize_channel_thread_messages (void)
{
  SET_MSG_TYPE_LENGTH (mt_readahead, sm_readahead_t);
  SET_MSG_TYPE_LENGTH (mt_readahead_ack, sm_readahead_ack_t);
}

channel_context_t *
OS2_make_channel_context (void)
{
  channel_context_t * context = (OS_malloc (sizeof (channel_context_t)));
  OS2_make_qid_pair ((& (CHANNEL_CONTEXT_READER_QID (context))),
		     (& (CHANNEL_CONTEXT_WRITER_QID (context))));
  (CHANNEL_CONTEXT_EOFP (context)) = 0;
  (CHANNEL_CONTEXT_FIRST_READ_P (context)) = 1;
  return (context);
}

void
OS2_channel_thread_close (Tchannel channel)
{
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  /* Send a readahead ACK informing the channel thread to kill itself.
     Then, close our end of the connection -- it's no longer needed.  */
  send_readahead_ack ((CHANNEL_CONTEXT_READER_QID (context)), raa_close);
  OS2_close_qid (CHANNEL_CONTEXT_READER_QID (context));
  OS_free (context);
  /* Finally, the caller must close the channel handle.  If the
     channel thread is blocked in dos_read, this will break it out and
     get it to kill itself.  There's no race, because the channel
     thread won't try to close the handle, and if it breaks out of
     dos_read before we do the close, it will see the readahead ACK we
     just sent and that will kill it.  */
}

qid_t
OS2_channel_thread_descriptor (Tchannel channel)
{
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  /* Make sure that the readahead thread is started, so that when
     input arrives it will be registered properly so that the "select"
     emulation will notice it.  */
  start_readahead_thread (context);
  return (CHANNEL_CONTEXT_READER_QID (context));
}

static void
start_readahead_thread (channel_context_t * context)
{
  /* Wake up the reader thread if this is the first time we are
     operating on it.  This is necessary because we sometimes don't
     want to read from the channel at all -- for example, when the
     channel is the read side of a pipe that is being passed to a
     child process.  */
  if (CHANNEL_CONTEXT_FIRST_READ_P (context))
    {
      send_readahead_ack ((CHANNEL_CONTEXT_READER_QID (context)), raa_read);
      (CHANNEL_CONTEXT_FIRST_READ_P (context)) = 0;
    }
}

msg_t *
OS2_make_readahead (void)
{
  msg_t * message = (OS2_create_message (mt_readahead));
  (SM_READAHEAD_INDEX (message)) = 0;
  return (message);
}

long
OS2_channel_thread_read (Tchannel channel, char * buffer, size_t size)
{
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  qid_t qid = (CHANNEL_CONTEXT_READER_QID (context));
  msg_t * message;
  unsigned short index;
  unsigned short navail;
  if ((CHANNEL_CONTEXT_EOFP (context)) || (size == 0))
    return (0);
  start_readahead_thread (context);
  message = (OS2_receive_message (qid, (!CHANNEL_NONBLOCKING (channel)), 1));
  if (message == 0)
    return (-1);
  if (OS2_error_message_p (message))
    {
      send_readahead_ack (qid, raa_read);
      OS2_handle_error_message (message);
    }
  if ((MSG_TYPE (message)) != mt_readahead)
    OS2_logic_error ("Illegal message from channel thread.");
  index = (SM_READAHEAD_INDEX (message));
  if (index == 0)
    send_readahead_ack (qid, raa_read);
  navail = ((SM_READAHEAD_SIZE (message)) - index);
  if (navail == 0)
    {
      OS2_destroy_message (message);
      (CHANNEL_CONTEXT_EOFP (context)) = 1;
      return (0);
    }
  else if (navail <= size)
    {
      FASTCOPY (((SM_READAHEAD_DATA (message)) + index), buffer, navail);
      OS2_destroy_message (message);
      return (navail);
    }
  else
    {
      FASTCOPY (((SM_READAHEAD_DATA (message)) + index), buffer, size);
      (SM_READAHEAD_INDEX (message)) += size;
      OS2_unread_message (qid, message);
      return (size);
    }
}

static void
send_readahead_ack (qid_t qid, enum readahead_ack_action action)
{
  msg_t * message = (OS2_create_message (mt_readahead_ack));
  (SM_READAHEAD_ACK_ACTION (message)) = action;
  OS2_send_message (qid, message);
}

enum readahead_ack_action
OS2_wait_for_readahead_ack (qid_t qid)
{
  /* Wait for an acknowledgement before starting another read.
     This regulates the amount of data in the queue.  */
  msg_t * message = (OS2_wait_for_message (qid, mt_readahead_ack));
  enum readahead_ack_action action = (SM_READAHEAD_ACK_ACTION (message));
  OS2_destroy_message (message);
  return (action);
}

void
OS2_readahead_buffer_insert (void * buffer, char c)
{
  msg_t * last = (OS2_msg_fifo_last (buffer));
  if ((last != 0) && ((SM_READAHEAD_SIZE (last)) < SM_READAHEAD_MAX))
    ((SM_READAHEAD_DATA (last)) [(SM_READAHEAD_SIZE (last))++]) = c;
  else
    {
      msg_t * message = (new_message ());
      ((SM_READAHEAD_DATA (message)) [(SM_READAHEAD_SIZE (message))++]) = c;
      OS2_msg_fifo_insert (buffer, message);
    }
}

static msg_t *
new_message (void)
{
  msg_t * message = (OS2_make_readahead ());
  (SM_READAHEAD_SIZE (message)) = 0;
  return (message);
}

char
OS2_readahead_buffer_rubout (void * buffer)
{
  msg_t * message = (OS2_msg_fifo_last (buffer));
  if (message == 0)
    OS2_logic_error ("Rubout from empty readahead buffer.");
  {
    char c = ((SM_READAHEAD_DATA (message)) [--(SM_READAHEAD_SIZE (message))]);
    if ((SM_READAHEAD_SIZE (message)) == 0)
      {
	OS2_msg_fifo_remove_last (buffer);
	OS2_destroy_message (message);
      }
    return (c);
  }
}

msg_t *
OS2_readahead_buffer_read (void * buffer)
{
  msg_t * message = (OS2_msg_fifo_remove (buffer));
  return ((message == 0) ? (new_message ()) : message);
}
