/* -*-C-*-

$Id: os2cthrd.c,v 1.5 1995/04/21 00:54:49 cph Exp $

Copyright (c) 1994-95 Massachusetts Institute of Technology

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

/* Scheme side of channel thread interface */

#include "os2.h"

static void start_readahead_thread (channel_context_t *);
static msg_list_t * new_list (void);
static msg_t * new_message (void);

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
  /* Closing handle forces input thread to kill itself.  */
  STD_API_CALL (dos_close, (CHANNEL_HANDLE (channel)));
  /* Send a readahead ACK, because the thread might be waiting for
     one, and otherwise it would hang forever.  We could try to
     determine if it was necessary to send the ACK, but it does no
     harm to send the ACK when it isn't needed.  */
  OS2_send_message ((CHANNEL_CONTEXT_READER_QID (context)),
		    (OS2_make_readahead_ack ()));
  OS2_close_qid (CHANNEL_CONTEXT_READER_QID (context));
  OS_free (context);
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
      OS2_send_message ((CHANNEL_CONTEXT_READER_QID (context)),
			(OS2_make_readahead_ack ()));
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
      OS2_send_message (qid, (OS2_make_readahead_ack ()));
      OS2_handle_error_message (message);
    }
  if ((MSG_TYPE (message)) != mt_readahead)
    OS2_logic_error ("Illegal message from channel thread.");
  index = (SM_READAHEAD_INDEX (message));
  if (index == 0)
    OS2_send_message (qid, (OS2_make_readahead_ack ()));
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

void
OS2_wait_for_readahead_ack (qid_t qid)
{
  /* Wait for an acknowledgement before starting another read.
     This regulates the amount of data in the queue.  */
  OS2_destroy_message (OS2_wait_for_message (qid, mt_readahead_ack));
}

readahead_buffer_t *
OS2_make_readahead_buffer (void)
{
  readahead_buffer_t * buffer = (OS_malloc (sizeof (readahead_buffer_t)));
  (buffer -> head) = 0;
  (buffer -> tail) = 0;
  return (buffer);
}

int
OS2_readahead_buffer_emptyp (readahead_buffer_t * buffer)
{
  return ((buffer -> head) == 0);
}

void
OS2_readahead_buffer_insert (readahead_buffer_t * buffer, char c)
{
  if ((buffer -> head) == 0)
    {
      msg_list_t * tail = (new_list ());
      (buffer -> head) = tail;
      (buffer -> tail) = tail;
    }
  else if ((SM_READAHEAD_SIZE ((buffer -> tail) -> message))
	   == SM_READAHEAD_MAX)
    {
      msg_list_t * tail = (new_list ());
      ((buffer -> tail) -> next) = tail;
      (buffer -> tail) = tail;
    }
  {
    msg_t * message = ((buffer -> tail) -> message);
    ((SM_READAHEAD_DATA (message)) [(SM_READAHEAD_SIZE (message))++]) = c;
  }
}

static msg_list_t *
new_list (void)
{
  msg_list_t * cell = (OS_malloc (sizeof (msg_list_t)));
  (cell -> message) = (new_message ());
  (cell -> next) = 0;
  return (cell);
}

static msg_t *
new_message (void)
{
  msg_t * message = (OS2_make_readahead ());
  (SM_READAHEAD_SIZE (message)) = 0;
  return (message);
}

char
OS2_readahead_buffer_rubout (readahead_buffer_t * buffer)
{
  if ((buffer -> head) == 0)
    OS2_logic_error ("Rubout from empty readahead buffer.");
  {
    msg_t * message = ((buffer -> tail) -> message);
    char c = ((SM_READAHEAD_DATA (message)) [--(SM_READAHEAD_SIZE (message))]);
    if ((SM_READAHEAD_SIZE (message)) == 0)
      {
	msg_list_t * tail = (buffer -> tail);
	msg_list_t * prev = (buffer -> head);
	if (prev == tail)
	  (buffer -> head) = 0;
	else
	  {
	    while ((prev -> next) != tail)
	      prev = (prev -> next);
	    (prev -> next) = 0;
	    (buffer -> tail) = prev;
	  }
	OS_free (tail);
      }
    OS2_destroy_message (message);
    return (c);
  }
}

msg_t *
OS2_readahead_buffer_read (readahead_buffer_t * buffer)
{
  msg_list_t * head = (buffer -> head);
  if (head == 0)
    return (new_message ());
  else
    {
      msg_t * message = (head -> message);
      (buffer -> head) = (head -> next);
      OS_free (head);
      return (message);
    }
}

msg_list_t *
OS2_readahead_buffer_read_all (readahead_buffer_t * buffer)
{
  msg_list_t * head = (buffer -> head);
  (buffer -> head) = 0;
  return (head);
}
