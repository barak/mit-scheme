/* -*-C-*-

$Id: os2cthrd.c,v 1.3 1994/12/19 22:30:49 cph Exp $

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

/* Scheme side of channel thread interface */

#include "os2.h"

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
  (CHANNEL_CONTEXT_READAHEAD (context)) = 0;
  (CHANNEL_CONTEXT_READAHEAD_INDEX (context)) = 0;
  (CHANNEL_CONTEXT_EOFP (context)) = 0;
  return (context);
}

void
OS2_channel_thread_close (Tchannel channel)
{
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  /* Closing handle should force input thread to kill itself.  */
  STD_API_CALL (dos_close, (CHANNEL_HANDLE (channel)));
  if ((CHANNEL_CONTEXT_READAHEAD (context)) != 0)
    OS2_destroy_message (CHANNEL_CONTEXT_READAHEAD (context));
  OS2_close_qid (CHANNEL_CONTEXT_READER_QID (context));
  OS_free (context);
}

long
OS2_channel_thread_read (Tchannel channel, char * buffer, size_t size)
{
  channel_context_t * context = (CHANNEL_OPERATOR_CONTEXT (channel));
  qid_t qid = (CHANNEL_CONTEXT_READER_QID (context));
  msg_t * message;
  unsigned int index;
  unsigned int navail;
  if (CHANNEL_CONTEXT_EOFP (context))
    return (0);
  message = (CHANNEL_CONTEXT_READAHEAD (context));
  index = (CHANNEL_CONTEXT_READAHEAD_INDEX (context));
  if (message == 0)
    {
      if (CHANNEL_NONBLOCKING (channel))
	{
	  message = (OS2_receive_message (qid, 0, 1));
	  if (message == 0)
	    {
	      (CHANNEL_CONTEXT_READAHEAD (context)) = 0;
	      return (-1);
	    }
	}
      else
	message = (OS2_receive_message (qid, 1, 1));
      /* Acknowledge the message so that the readahead thread will
	 know that it is safe to start reading some more.  */
      OS2_send_message (qid, (OS2_make_readahead_ack ()));
      if (OS2_error_message_p (message))
	OS2_handle_error_message (message);
      if ((MSG_TYPE (message)) != mt_readahead)
	OS2_logic_error ("Illegal message from channel thread.");
      index = 0;
    }
  if ((SM_READAHEAD_SIZE (message)) == 0)
    {
      OS2_destroy_message (message);
      (CHANNEL_CONTEXT_READAHEAD (context)) = 0;
      (CHANNEL_CONTEXT_EOFP (context)) = 1;
      return (0);
    }
  navail = ((SM_READAHEAD_SIZE (message)) - index);
  if (navail <= size)
    {
      FASTCOPY ((SM_READAHEAD_DATA (message)), buffer, navail);
      OS2_destroy_message (message);
      (CHANNEL_CONTEXT_READAHEAD (context)) = 0;
      return (navail);
    }
  else
    {
      FASTCOPY ((SM_READAHEAD_DATA (message)), buffer, size);
      (CHANNEL_CONTEXT_READAHEAD (context)) = message;
      (CHANNEL_CONTEXT_READAHEAD_INDEX (context)) = (index + size);
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
