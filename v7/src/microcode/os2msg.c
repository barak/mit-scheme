/* -*-C-*-

$Id: os2msg.c,v 1.14 2000/12/05 21:23:46 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Master Message Queue */

#include "os2.h"

extern void EXFUN (tty_set_next_interrupt_char, (cc_t c));
extern void * OS2_malloc_noerror (unsigned int);

static qid_t allocate_qid (void);
static void OS2_initialize_message_lengths (void);
static void write_subqueue (msg_t *);
static msg_t * read_subqueue (qid_t);
static int subqueue_emptyp (qid_t);
static msg_t * read_tqueue (tqueue_t *, int);
static void write_tqueue (tqueue_t *, msg_t *);
static msg_t * read_std_tqueue (tqueue_t *, int);
static void write_std_tqueue (tqueue_t *, msg_t *);
static tqueue_t * make_scm_tqueue (void);
static msg_t * read_scm_tqueue (tqueue_t *, int);
static void write_scm_tqueue (tqueue_t *, msg_t *);
static void process_interrupt_messages (void);

typedef struct
{
  unsigned int allocatedp : 1;	/* queue allocated? */
  qid_t twin;			/* other end of connection */
  qid_receive_filter_t filter;	/* filter for received messages */
  tqueue_t * tqueue;		/* thread queue for reception */
  void * subqueue;		/* receiving subqueue */
  HMTX lock;
} iqid_t;

static iqid_t queue_array [QID_MAX + 1];
static HMTX qid_lock;

tqueue_t * OS2_scheme_tqueue;
static qid_t OS2_interrupt_qid_local;
qid_t OS2_interrupt_qid;

#define _QID(q) (queue_array [(q)])
#define QID_ALLOCATEDP(q) ((_QID (q)) . allocatedp)
#define QID_TWIN(q) ((_QID (q)) . twin)
#define QID_FILTER(q) ((_QID (q)) . filter)
#define QID_TQUEUE(q) ((_QID (q)) . tqueue)
#define QID_SUBQUEUE(q) ((_QID (q)) . subqueue)
#define QID_LOCK(q) ((_QID (q)) . lock)

void
OS2_initialize_message_queues (void)
{
  {
    qid_t qid = 0;
    while (1)
      {
	(QID_ALLOCATEDP (qid)) = 0;
	(QID_TWIN (qid)) = QID_NONE;
	(QID_FILTER (qid)) = 0;
	(QID_TQUEUE (qid)) = 0;
	(QID_SUBQUEUE (qid)) = 0;
	(QID_LOCK (qid)) = NULLHANDLE;
	if (qid == QID_MAX)
	  break;
	qid += 1;
      }
  }
  OS2_initialize_message_lengths ();
  SET_MSG_TYPE_LENGTH (mt_init, sm_init_t);
  SET_MSG_TYPE_LENGTH (mt_console_interrupt, sm_console_interrupt_t);
  SET_MSG_TYPE_LENGTH (mt_timer_event, sm_timer_event_t);
  SET_MSG_TYPE_LENGTH (mt_generic_reply, sm_generic_reply_t);
  qid_lock = (OS2_create_mutex_semaphore (0, 0));
  OS2_scheme_tqueue = (make_scm_tqueue ());
  OS2_make_qid_pair ((&OS2_interrupt_qid_local), (&OS2_interrupt_qid));
  OS2_open_qid (OS2_interrupt_qid_local, OS2_scheme_tqueue);
}

void
OS2_make_qid_pair (qid_t * pq1, qid_t * pq2)
{
  qid_t q1 = (allocate_qid ());
  qid_t q2 = (allocate_qid ());
  (QID_TWIN (q1)) = q2;
  (QID_TWIN (q2)) = q1;
  (*pq1) = q1;
  (*pq2) = q2;
}

static qid_t
allocate_qid (void)
{
  unsigned int qid = 0;
  OS2_request_mutex_semaphore (qid_lock);
  while (1)
    {
      if ((QID_ALLOCATEDP (qid)) == 0)
	break;
      if (qid == QID_MAX)
	OS2_logic_error ("No more QIDs available.");
      qid += 1;
    }
  (QID_ALLOCATEDP (qid)) = 1;
  (QID_TWIN (qid)) = QID_NONE;
  OS2_release_mutex_semaphore (qid_lock);
  (QID_FILTER (qid)) = 0;
  (QID_TQUEUE (qid)) = 0;
  (QID_SUBQUEUE (qid)) = (OS2_create_msg_fifo ());
  if ((QID_LOCK (qid)) == NULLHANDLE)
    (QID_LOCK (qid)) = (OS2_create_mutex_semaphore (0, 0));
  return (qid);
}

void
OS2_open_qid (qid_t qid, tqueue_t * tqueue)
{
  if ((QID_TQUEUE (qid)) != 0)
    OS2_logic_error ("Reopening already open QID.");
  if (tqueue == 0)
    OS2_logic_error ("Null tqueue passed to OS2_open_qid.");
  (QID_TQUEUE (qid)) = tqueue;
}

int
OS2_qid_openp (qid_t qid)
{
  return ((QID_TQUEUE (qid)) != 0);
}

void
OS2_close_qid (qid_t qid)
{
  OS2_request_mutex_semaphore (QID_LOCK (qid));
  while (1)
    {
      msg_t * msg = (OS2_msg_fifo_remove (QID_SUBQUEUE (qid)));
      if (msg == 0)
	break;
      OS2_destroy_message (msg);
    }
  OS2_destroy_msg_fifo (QID_SUBQUEUE (qid));
  (QID_FILTER (qid)) = 0;
  (QID_TQUEUE (qid)) = 0;
  (QID_SUBQUEUE (qid)) = 0;
  OS2_release_mutex_semaphore (QID_LOCK (qid));
  OS2_request_mutex_semaphore (qid_lock);
  {
    qid_t twin = (QID_TWIN (qid));
    if (twin != QID_NONE)
      {
	(QID_TWIN (twin)) = QID_NONE;
	(QID_TWIN (qid)) = QID_NONE;
      }
  }
  (QID_ALLOCATEDP (qid)) = 0;
  OS2_release_mutex_semaphore (qid_lock);
}

tqueue_t *
OS2_qid_tqueue (qid_t qid)
{
  return (QID_TQUEUE (qid));
}

qid_t
OS2_qid_twin (qid_t qid)
{
  qid_t twin;
  OS2_request_mutex_semaphore (qid_lock);
  twin
    = (((QID_ALLOCATEDP (qid))
	&& ((QID_TWIN (qid)) != QID_NONE)
	&& (QID_ALLOCATEDP (QID_TWIN (qid))))
       ? (QID_TWIN (qid))
       : QID_NONE);
  OS2_release_mutex_semaphore (qid_lock);
  return (twin);
}

void
OS2_close_qid_pair (qid_t qid)
{
  /* This is safe because it is used only in a particular way.  The
     twin of this qid is never received from, and qid is never sent
     to, and the twin will never be closed by the other thread.  Thus,
     even though the unlocked sections of OS2_close_qid are
     manipulating structures that belong to the other thread, the
     other thread won't be manipulating them so no conflict will
     arise.  It's important not to use this procedure in any other
     situation!  */
  if (QID_ALLOCATEDP (qid))
    {
      qid_t twin = (OS2_qid_twin (qid));
      if (twin != QID_NONE)
	OS2_close_qid (twin);
      OS2_close_qid (qid);
    }
}

void
OS2_set_qid_receive_filter (qid_t qid, qid_receive_filter_t filter)
{
  (QID_FILTER (qid)) = filter;
}

/* Message Lengths */

#define MESSAGE_LENGTH(t) (message_lengths [(unsigned int) (t)])
static msg_length_t message_lengths [MSG_TYPE_SUP];

static void
OS2_initialize_message_lengths (void)
{
  unsigned int type = 0;
  while (1)
    {
      (MESSAGE_LENGTH (type)) = 0;
      if (type == MSG_TYPE_MAX)
	break;
      type += 1;
    }
}

void
OS2_check_message_length_initializations (void)
{
  unsigned int type = 0;
  while (1)
    {
      if ((MESSAGE_LENGTH (type)) == 0)
	{
	  char buffer [64];
	  sprintf (buffer, "Message type %d not initialized.", type);
	  OS2_logic_error (buffer);
	}
      if (type == MSG_TYPE_MAX)
	break;
      type += 1;
    }
}

msg_length_t
OS2_message_type_length (msg_type_t type)
{
  msg_length_t length;
  if (type > MSG_TYPE_MAX)
    {
      char buffer [64];
      sprintf (buffer, "Message type %d out of range.", type);
      OS2_logic_error (buffer);
    }
  length = (MESSAGE_LENGTH (type));
  if (length == 0)
    {
      char buffer [64];
      sprintf (buffer, "Message type %d has unknown length.", type);
      OS2_logic_error (buffer);
    }
  return (length);
}

void
OS2_set_message_type_length (msg_type_t type, msg_length_t length)
{
  (MESSAGE_LENGTH (type)) = length;
}

msg_t *
OS2_create_message_1 (msg_type_t type, msg_length_t extra)
{
  /* Do allocation carefully to prevent infinite loop when signalling
     "out of memory" condition.  */
  msg_t * message =
    (OS2_malloc_noerror (((unsigned long) (OS2_message_type_length (type)))
			 + extra));
  if (message == 0)
    if ((type == mt_syscall_error)
	&& ((SM_SYSCALL_ERROR_CODE (message)) == ERROR_NOT_ENOUGH_MEMORY)
	&& ((SM_SYSCALL_ERROR_NAME (message)) == syscall_malloc))
      OS2_logic_error ("Unable to allocate memory for error message.");
    else
      OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_malloc);
  (MSG_TYPE (message)) = type;
  return (message);
}

void
OS2_destroy_message (msg_t * message)
{
  OS_free (message);
}

/* Message Transmission and Reception */

void
OS2_send_message (qid_t qid, msg_t * message)
{
  qid_t twin = (QID_TWIN (qid));
  tqueue_t * tqueue;
  if ((twin == QID_NONE) || ((tqueue = (QID_TQUEUE (twin))) == 0))
    /* Other end of connection has been closed, so discard the
       message.  We used to signal an error here, but this can happen
       pretty easily when closing windows or exiting Scheme.  The only
       way to avoid this is to force synchronization of communicating
       threads, which can be tricky.  For example, when closing a PM
       window, it's not obvious when the last message will be
       generated by the PM thread.  So it's just simpler to ignore
       messages after the receiver decides it's no longer interested
       in them.  */
    OS2_destroy_message (message);
  else
    {
      (MSG_SENDER (message)) = twin;
      write_tqueue (tqueue, message);
    }
}

msg_t *
OS2_receive_message (qid_t qid, int blockp, int interruptp)
{
  tqueue_t * tqueue = (QID_TQUEUE (qid));
  msg_t * message;
  if (tqueue == 0)
    {
      if ((OS2_current_tid ()) != OS2_scheme_tid)
	/* This behavior is a little random, but it's based on the
	   idea that if an inferior thread is reading from a closed
	   channel, this is due to a race condition, and the fact that
	   the channel is closed means that the thread is no longer
	   needed.  So far this has only happened under one
	   circumstance, and in that case, this is the correct action.  */
	OS2_endthread ();
      else
	OS2_error_anonymous ();
    }
  while (1)
    {
      while ((read_tqueue (tqueue, 0)) != 0)
	;
      if ((TQUEUE_TYPE (tqueue)) == tqt_scm)
	{
	  process_interrupt_messages ();
	  if (interruptp)
	    deliver_pending_interrupts ();
	}
      message = (read_subqueue (qid));
      if ((!blockp) || (message != 0))
	break;
      (void) read_tqueue (tqueue, 1);
    }
  return (message);
}

msg_avail_t
OS2_message_availablep (qid_t qid, int blockp)
{
  tqueue_t * tqueue = (QID_TQUEUE (qid));
  if (tqueue == 0)
    return (mat_not_available);
  while (1)
    {
      while ((read_tqueue (tqueue, 0)) != 0)
	;
      if ((TQUEUE_TYPE (tqueue)) == tqt_scm)
	{
	  process_interrupt_messages ();
	  if (pending_interrupts_p ())
	    return (mat_interrupt);
	}
      if (!subqueue_emptyp (qid))
	return (mat_available);
      if (!blockp)
	return (mat_not_available);
      (void) read_tqueue (tqueue, 1);
    }
}

msg_t *
OS2_wait_for_message (qid_t qid, msg_type_t reply_type)
{
  msg_t * reply = (OS2_receive_message (qid, 1, 0));
  if (OS2_error_message_p (reply))
    OS2_handle_error_message (reply);
  if ((MSG_TYPE (reply)) != reply_type)
    OS2_logic_error ("Incorrect reply message type.");
  return (reply);
}

msg_t *
OS2_message_transaction (qid_t qid, msg_t * request, msg_type_t reply_type)
{
  OS2_send_message (qid, request);
  return (OS2_wait_for_message (qid, reply_type));
}

static void
write_subqueue (msg_t * message)
{
  qid_t qid = (MSG_SENDER (message));
  qid_receive_filter_t filter = (QID_FILTER (qid));
  if (filter != 0)
    {
      message = ((* filter) (message));
      if (message == 0)
	return;
    }
  OS2_request_mutex_semaphore (QID_LOCK (qid));
  if (QID_SUBQUEUE (qid))
    OS2_msg_fifo_insert ((QID_SUBQUEUE (qid)), message);
  else
    /* If subqueue is gone, qid has been closed. */
    OS2_destroy_message (message);
  OS2_release_mutex_semaphore (QID_LOCK (qid));
}

static msg_t *
read_subqueue (qid_t qid)
{
  msg_t * result;
  OS2_request_mutex_semaphore (QID_LOCK (qid));
  result = (OS2_msg_fifo_remove (QID_SUBQUEUE (qid)));
  OS2_release_mutex_semaphore (QID_LOCK (qid));
  return (result);
}

void
OS2_unread_message (qid_t qid, msg_t * message)
{
  OS2_request_mutex_semaphore (QID_LOCK (qid));
  OS2_msg_fifo_insert_front ((QID_SUBQUEUE (qid)), message);
  OS2_release_mutex_semaphore (QID_LOCK (qid));
}

static int
subqueue_emptyp (qid_t qid)
{
  int result;
  OS2_request_mutex_semaphore (QID_LOCK (qid));
  result = (OS2_msg_fifo_emptyp (QID_SUBQUEUE (qid)));
  OS2_release_mutex_semaphore (QID_LOCK (qid));
  return (result);
}

int
OS2_tqueue_select (tqueue_t * tqueue, int blockp)
{
  msg_t * message = (read_tqueue (tqueue, blockp));
  if ((TQUEUE_TYPE (tqueue)) == tqt_scm)
    {
      process_interrupt_messages ();
      if (pending_interrupts_p ())
	return (-2);
    }
  return ((message != 0) ? (MSG_SENDER (message)) : (-1));
}

static msg_t *
read_tqueue (tqueue_t * tqueue, int blockp)
{
  switch (TQUEUE_TYPE (tqueue))
    {
    case tqt_std:
      return (read_std_tqueue (tqueue, blockp));
    case tqt_scm:
      return (read_scm_tqueue (tqueue, blockp));
    case tqt_pm:
      return (OS2_read_pm_tqueue (tqueue, blockp));
    }
}

static void
write_tqueue (tqueue_t * tqueue, msg_t * message)
{
  switch (TQUEUE_TYPE (tqueue))
    {
    case tqt_std:
      write_std_tqueue (tqueue, message);
      break;
    case tqt_scm:
      write_scm_tqueue (tqueue, message);
      break;
    case tqt_pm:
      OS2_write_pm_tqueue (tqueue, message);
      break;
    }
}

/* Uncomment the following definition in order to use OS/2 queues.

   There seems to be some kind of bug when using them, which manifests
   itself as an access violation while reading from a socket.  I don't
   understand this and have been unable to debug it successfully.

   Since my intention was to find a way to speed up the
   message-handling mechanism, and there is no noticeable improvement,
   it probably isn't worth much more effort to find the bug.  */

/* #define USE_OS2_QUEUES */
#ifdef USE_OS2_QUEUES

typedef struct
{
  tqueue_type_t type;
  HQUEUE fifo;
  HEV event;			/* event semaphore */
} std_tqueue_t;
#define STD_TQUEUE_FIFO(q) (((std_tqueue_t *) (q)) -> fifo)
#define STD_TQUEUE_EVENT(q) (((std_tqueue_t *) (q)) -> event)

tqueue_t *
OS2_make_std_tqueue (void)
{
  tqueue_t * tqueue = (OS_malloc (sizeof (std_tqueue_t)));
  (TQUEUE_TYPE (tqueue)) = tqt_std;
  (STD_TQUEUE_FIFO (tqueue)) = (OS2_create_queue (QUE_FIFO));
  (STD_TQUEUE_EVENT (tqueue)) = (OS2_create_event_semaphore (0, 0));
  return (tqueue);
}

static msg_t *
read_std_tqueue_1 (tqueue_t * tqueue, int blockp)
{
  ULONG type;
  ULONG length;
  PVOID data;
  return
    ((OS2_read_queue ((STD_TQUEUE_FIFO (tqueue)),
		      (&type),
		      (&length),
		      (&data),
		      (blockp ? 0 : (STD_TQUEUE_EVENT (tqueue)))))
     ? data
     : 0);
}

void
OS2_close_std_tqueue (tqueue_t * tqueue)
{
  while (1)
    {
      msg_t * msg = (read_std_tqueue_1 (tqueue, 0));
      if (msg == 0)
	break;
      OS2_destroy_message (msg);
    }
  OS2_close_queue (STD_TQUEUE_FIFO (tqueue));
  OS2_close_event_semaphore (STD_TQUEUE_EVENT (tqueue));
  OS_free (tqueue);
}

static msg_t *
read_std_tqueue (tqueue_t * tqueue, int blockp)
{
  msg_t * message = (read_std_tqueue_1 (tqueue, blockp));
  if (message)
    write_subqueue (message);
  return (message);
}

static void
write_std_tqueue (tqueue_t * tqueue, msg_t * message)
{
  OS2_write_queue ((STD_TQUEUE_FIFO (tqueue)), 0, 0, message, 0);
}

#else /* not USE_OS2_QUEUES */

typedef struct
{
  tqueue_type_t type;
  void * fifo;
  unsigned int n_blocked;	/* # of blocked threads */
  HMTX mutex;			/* mutex semaphore */
  HEV event;			/* event semaphore */
} std_tqueue_t;
#define STD_TQUEUE_FIFO(q) (((std_tqueue_t *) (q)) -> fifo)
#define STD_TQUEUE_MUTEX(q) (((std_tqueue_t *) (q)) -> mutex)
#define STD_TQUEUE_EVENT(q) (((std_tqueue_t *) (q)) -> event)
#define STD_TQUEUE_N_BLOCKED(q) (((std_tqueue_t *) (q)) -> n_blocked)

tqueue_t *
OS2_make_std_tqueue (void)
{
  tqueue_t * tqueue = (OS_malloc (sizeof (std_tqueue_t)));
  (TQUEUE_TYPE (tqueue)) = tqt_std;
  (STD_TQUEUE_FIFO (tqueue)) = (OS2_create_msg_fifo ());
  (STD_TQUEUE_N_BLOCKED (tqueue)) = 0;
  (STD_TQUEUE_MUTEX (tqueue)) = (OS2_create_mutex_semaphore (0, 0));
  (STD_TQUEUE_EVENT (tqueue)) = (OS2_create_event_semaphore (0, 0));
  return (tqueue);
}

void
OS2_close_std_tqueue (tqueue_t * tqueue)
{
  OS2_close_event_semaphore (STD_TQUEUE_EVENT (tqueue));
  OS2_close_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
  while (1)
    {
      msg_t * msg = (OS2_msg_fifo_remove (STD_TQUEUE_FIFO (tqueue)));
      if (msg == 0)
	break;
      OS2_destroy_message (msg);
    }
  OS_free (tqueue);
}

static msg_t *
read_std_tqueue (tqueue_t * tqueue, int blockp)
{
  OS2_request_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
  while (1)
    {
      msg_t * message = (OS2_msg_fifo_remove (STD_TQUEUE_FIFO (tqueue)));
      if (message != 0)
	{
	  OS2_release_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
	  write_subqueue (message);
	  return (message);
	}
      if (!blockp)
	{
	  OS2_release_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
	  return (0);
	}
      (void) OS2_reset_event_semaphore (STD_TQUEUE_EVENT (tqueue));
      (STD_TQUEUE_N_BLOCKED (tqueue)) += 1;
      OS2_release_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
      (void) OS2_wait_event_semaphore ((STD_TQUEUE_EVENT (tqueue)), 1);
      OS2_request_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
      (STD_TQUEUE_N_BLOCKED (tqueue)) -= 1;
      /* This prevents the 16 bit counter inside the event
	 semaphore from overflowing.  */
      if ((STD_TQUEUE_N_BLOCKED (tqueue)) == 0)
	(void) OS2_reset_event_semaphore (STD_TQUEUE_EVENT (tqueue));
      /* Don't wait more than once; the caller must be prepared to
	 call again if a message is required.  The reason this is
	 necessary is that two threads may be waiting on the same
	 tqueue at the same time, and when a message shows up, the
	 wrong thread might read it.  If we allowed the loop to
	 continue, the thread that was waiting for the message would
	 wake up, see no message, and go to sleep; meanwhile, the
	 other thread has already stored the message in the correct
	 subqueue.  */
      blockp = 0;
    }
}

static void
write_std_tqueue (tqueue_t * tqueue, msg_t * message)
{
  OS2_request_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
  OS2_msg_fifo_insert ((STD_TQUEUE_FIFO (tqueue)), message);
  if ((STD_TQUEUE_N_BLOCKED (tqueue)) > 0)
    {
      (void) OS2_post_event_semaphore (STD_TQUEUE_EVENT (tqueue));
      OS2_release_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
      /* Immediately transfer control to the receiver.
	 This should improve responsiveness of the system.  */
      (void) DosSleep (0);
    }
  else
    OS2_release_mutex_semaphore (STD_TQUEUE_MUTEX (tqueue));
}

#endif /* not USE_OS2_QUEUES */

static tqueue_t *
make_scm_tqueue (void)
{
  tqueue_t * tqueue = (OS2_make_std_tqueue ());
  (TQUEUE_TYPE (tqueue)) = tqt_scm;
  return (tqueue);
}

char OS2_scheme_tqueue_avail_map [QID_MAX + 1];

static msg_t *
read_scm_tqueue (tqueue_t * tqueue, int blockp)
{
  /* The handling of the interrupt bit is a little tricky.  We clear
     the bit, then handle any events, and finally clear the bit again.
     If the bit is set during the second clear, we must loop since
     another event might have been queued in the window between the
     last read and the second clear -- and since we cleared the bit no
     one else is going to look at the queue until another event comes
     along.
     
     This code serves two purposes.  First, this is the only way to
     reliably clear the interrupt bit to avoid having an event stuck
     in the queue and the Scheme thread not bothering to look.
     Second, if we arrive at this read-dispatch loop by some means
     other than the attention-interrupt mechanism, this will clear the
     bit and thus avoid ever invoking the mechanism.  */
  msg_t * result = 0;
  (void) test_and_clear_attention_interrupt ();
  do
    {
      msg_t * message = (read_std_tqueue (tqueue, blockp));
      if (message != 0)
	{
	  (OS2_scheme_tqueue_avail_map [MSG_SENDER (message)]) = 1;
	  result = message;
	  /* At most one message needs to be read in blocking mode.  */
	  blockp = 0;
	}
    }
  while (test_and_clear_attention_interrupt ());
  return (result);
}

static void
write_scm_tqueue (tqueue_t * tqueue, msg_t * message)
{
  write_std_tqueue (tqueue, message);
  request_attention_interrupt ();
}

void
OS2_handle_attention_interrupt (void)
{
  tqueue_t * tqueue = (QID_TQUEUE (OS2_interrupt_qid_local));
  while ((read_tqueue (tqueue, 0)) != 0)
    ;
  process_interrupt_messages ();
}

static void
process_interrupt_messages (void)
{
  /* Reads all of the interrupts out of the interrupt queue, and sets
     the corresponding bits in the interrupt word.  */
  while (1)
    {
      msg_t * message = (read_subqueue (OS2_interrupt_qid_local));
      if (message == 0)
	break;
      switch (MSG_TYPE (message))
	{
	case mt_console_interrupt:
	  tty_set_next_interrupt_char (SM_CONSOLE_INTERRUPT_CODE (message));
	  break;
	case mt_timer_event:
	  request_timer_interrupt ();
	  break;
	default:
	  OS2_logic_error ("Illegal message type in interrupt queue.");
	  break;
	}
      OS2_destroy_message (message);
    }
}

#define BUFFER_MIN_LENGTH 16

typedef struct
{
  unsigned int start;
  unsigned int end;
  unsigned int count;
  unsigned int buffer_length;
  void ** buffer;
} msg_fifo_t;

void *
OS2_create_msg_fifo (void)
{
  msg_fifo_t * fifo = (OS_malloc (sizeof (msg_fifo_t)));
  (fifo -> start) = 0;
  (fifo -> end) = 0;
  (fifo -> count) = 0;
  (fifo -> buffer_length) = BUFFER_MIN_LENGTH;
  (fifo -> buffer)
    = (OS_malloc ((fifo -> buffer_length) * (sizeof (void *))));
  return (fifo);
}

void
OS2_destroy_msg_fifo (void * fp)
{
  OS_free (((msg_fifo_t *) fp) -> buffer);
  OS_free (fp);
}

#define MAYBE_GROW_BUFFER(fifo)						\
{									\
  if ((fifo -> count) >= (fifo -> buffer_length))			\
    msg_fifo_grow (fifo);						\
}

#define MAYBE_SHRINK_BUFFER(fifo)					\
{									\
  if (((fifo -> buffer_length) > BUFFER_MIN_LENGTH)			\
      && ((fifo -> count) < ((fifo -> buffer_length) / 4)))		\
    msg_fifo_shrink (fifo);						\
}

#define REALLOC_BUFFER(fifo, new_length)				\
{									\
  ((fifo) -> buffer_length) = (new_length);				\
  ((fifo) -> buffer)							\
    = (OS_realloc (((fifo) -> buffer),					\
		   (((fifo) -> buffer_length) * (sizeof (void *)))));	\
}

static void
msg_fifo_grow (msg_fifo_t * fifo)
{
  unsigned int old_length = (fifo -> buffer_length);
  REALLOC_BUFFER (fifo, (old_length * 2));
  if ((fifo -> end) <= (fifo -> start))
    {
      void ** from = (fifo -> buffer);
      void ** stop = ((fifo -> buffer) + (fifo -> end));
      void ** to = ((fifo -> buffer) + old_length);
      while (from < stop)
	(*to++) = (*from++);
      (fifo -> end) += old_length;
    }
}

static void
msg_fifo_shrink (msg_fifo_t * fifo)
{
  unsigned int new_length = ((fifo -> buffer_length) / 2);
  if ((fifo -> end) < (fifo -> start))
    {
      void ** from = ((fifo -> buffer) + (fifo -> start));
      void ** stop = ((fifo -> buffer) + (fifo -> buffer_length));
      void ** to = (from - new_length);
      while (from < stop)
	(*to++) = (*from++);
      (fifo -> start) -= new_length;
    }
  else if ((fifo -> end) > new_length)
    {
      void ** from = ((fifo -> buffer) + (fifo -> start));
      void ** stop = ((fifo -> buffer) + (fifo -> end));
      void ** to = (fifo -> buffer);
      while (from < stop)
	(*to++) = (*from++);
      (fifo -> end) -= (fifo -> start);
      (fifo -> start) = 0;
    }
  REALLOC_BUFFER (fifo, new_length);
}

void
OS2_msg_fifo_insert (void * fp, void * element)
{
  msg_fifo_t * fifo = fp;
  MAYBE_GROW_BUFFER (fifo);
  if ((fifo -> end) == (fifo -> buffer_length))
    (fifo -> end) = 0;
  ((fifo -> buffer) [(fifo -> end) ++]) = element;
  (fifo -> count) += 1;
}

void
OS2_msg_fifo_insert_front (void * fp, void * element)
{
  msg_fifo_t * fifo = fp;
  MAYBE_GROW_BUFFER (fifo);
  if ((fifo -> start) == 0)
    (fifo -> start) = (fifo -> buffer_length);
  ((fifo -> buffer) [-- (fifo -> start)]) = element;
  (fifo -> count) += 1;
}

void *
OS2_msg_fifo_remove (void * fp)
{
  msg_fifo_t * fifo = fp;
  void * element;
  if ((fifo -> count) == 0)
    return (0);
  element = ((fifo -> buffer) [(fifo -> start) ++]);
  if ((fifo -> start) == (fifo -> buffer_length))
    (fifo -> start) = 0;
  if ((-- (fifo -> count)) == 0)
    {
      (fifo -> start) = 0;
      (fifo -> end) = 0;
    }
  MAYBE_SHRINK_BUFFER (fifo);
  return (element);
}

void *
OS2_msg_fifo_remove_last (void * fp)
{
  msg_fifo_t * fifo = fp;
  void * element;
  if ((fifo -> count) == 0)
    return (0);
  element = ((fifo -> buffer) [-- (fifo -> end)]);
  if ((fifo -> end) == 0)
    (fifo -> end) = (fifo -> buffer_length);
  if ((-- (fifo -> count)) == 0)
    {
      (fifo -> start) = 0;
      (fifo -> end) = 0;
    }
  MAYBE_SHRINK_BUFFER (fifo);
  return (element);
}

void **
OS2_msg_fifo_remove_all (void * fp)
{
  msg_fifo_t * fifo = fp;
  void ** result = (OS_malloc (((fifo -> count) + 1) * (sizeof (void *))));
  void ** from = ((fifo -> buffer) + (fifo -> start));
  void ** stop;
  void ** to = result;
  if ((fifo -> start) < (fifo -> end))
    {
      stop = ((fifo -> buffer) + (fifo -> end));
      while (from < stop)
	(*to++) = (*from++);
    }
  else if ((fifo -> count) > 0)
    {
      stop = ((fifo -> buffer) + (fifo -> buffer_length));
      while (from < stop)
	(*to++) = (*from++);
      from = (fifo -> buffer);
      stop = ((fifo -> buffer) + (fifo -> end));
      while (from < stop)
	(*to++) = (*from++);
    }
  (*to) = 0;
  (fifo -> start) = 0;
  (fifo -> end) = 0;
  (fifo -> count) = 0;
  if ((fifo -> buffer_length) > BUFFER_MIN_LENGTH)
    REALLOC_BUFFER (fifo, BUFFER_MIN_LENGTH);
  return (result);
}

int
OS2_msg_fifo_emptyp (void * fp)
{
  msg_fifo_t * fifo = fp;
  return ((fifo -> count) == 0);
}

unsigned int
OS2_msg_fifo_count (void * fp)
{
  msg_fifo_t * fifo = fp;
  return (fifo -> count);
}

void *
OS2_msg_fifo_last (void * fp)
{
  msg_fifo_t * fifo = fp;
  return (((fifo -> count) == 0) ? 0 : ((fifo -> buffer) [(fifo -> end) - 1]));
}
