/* -*-C-*-

$Id: os2msg.c,v 1.2 1994/12/02 20:42:46 cph Exp $

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

/* Master Message Queue */

#include "os2.h"

static qid_t allocate_qid (void);
static void OS2_initialize_message_lengths (void);
static void write_subqueue (msg_t *);
static msg_t * read_subqueue (qid_t);
static int subqueue_emptyp (qid_t);
static int read_tqueue (tqueue_t *, int);
static void write_tqueue (tqueue_t *, msg_t *);
static int read_std_tqueue (tqueue_t *, int);
static void write_std_tqueue (tqueue_t *, msg_t *);
static tqueue_t * make_scm_tqueue (void);
static int read_scm_tqueue (tqueue_t *, int);
static void write_scm_tqueue (tqueue_t *, msg_t *);
static void process_interrupt_messages (void);

typedef struct
{
  unsigned int allocatedp : 1;	/* queue allocated? */
  unsigned int openp : 1;	/* queue open? */
  qid_t twin;			/* other end of connection */
  qid_receive_filter_t filter;	/* filter for received messages */
  tqueue_t * tqueue;		/* thread queue for reception */
  msg_list_t * subqueue_head;	/* head of receiving subqueue */
  msg_list_t * subqueue_tail;	/* tail of receiving subqueue */
} iqid_t;

static iqid_t queue_array [QID_MAX + 1];
static HMTX qid_lock;

tqueue_t * OS2_scheme_tqueue;
static qid_t OS2_interrupt_qid_local;
qid_t OS2_interrupt_qid;

#define _QID(q) (queue_array [(q)])
#define QID_ALLOCATEDP(q) ((_QID (q)) . allocatedp)
#define QID_TWIN(q) ((_QID (q)) . twin)
#define QID_TQUEUE(q) ((_QID (q)) . tqueue)
#define QID_SUBQUEUE_HEAD(q) ((_QID (q)) . subqueue_head)
#define QID_SUBQUEUE_TAIL(q) ((_QID (q)) . subqueue_tail)
#define QID_FILTER(q) ((_QID (q)) . filter)

#define MSG_QUEUE_TYPE(m) 0
#define MSG_QUEUE_PRIORITY(m) 0

void
OS2_initialize_message_queues (void)
{
  {
    qid_t qid = 0;
    while (1)
      {
	(QID_ALLOCATEDP (qid)) = 0;
	(QID_TQUEUE (qid)) = 0;
	(QID_TWIN (qid)) = QID_NONE;
	(QID_SUBQUEUE_HEAD (qid)) = 0;
	(QID_SUBQUEUE_TAIL (qid)) = 0;
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
  qid_lock = (OS2_create_mutex_semaphore ());
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
  (QID_TQUEUE (qid)) = 0;
  (QID_TWIN (qid)) = QID_NONE;
  (QID_SUBQUEUE_HEAD (qid)) = 0;
  (QID_SUBQUEUE_TAIL (qid)) = 0;
  OS2_release_mutex_semaphore (qid_lock);
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
  {
    msg_list_t * elt = (QID_SUBQUEUE_HEAD (qid));
    while (elt != 0)
      {
	msg_list_t * next = (elt -> next);
	OS_free (elt);
	elt = next;
      }
  }
  (QID_SUBQUEUE_HEAD (qid)) = 0;
  (QID_SUBQUEUE_TAIL (qid)) = 0;
  (QID_TQUEUE (qid)) = 0;
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

msg_length_t
OS2_message_type_length (msg_type_t type)
{
  msg_length_t length = (MESSAGE_LENGTH (type));
  if (length == 0)
    OS2_logic_error ("Message type has unknown length.");
  return (length);
}

void
OS2_set_message_type_length (msg_type_t type, msg_length_t length)
{
  (MESSAGE_LENGTH (type)) = length;
}

/* Message Transmission and Reception */

msg_t *
OS2_create_message (msg_type_t type)
{
  /* Do allocation carefully to prevent infinite loop when signalling
     "out of memory" condition.  */
  msg_t * message =
    (malloc ((unsigned long) (OS2_message_type_length (type))));
  if (message == 0)
    if ((type == mt_syscall_error)
	&& ((SM_SYSCALL_ERROR_CODE (message)) == ERROR_NOT_ENOUGH_MEMORY)
	&& ((SM_SYSCALL_ERROR_NAME (message)) == syscall_malloc))
      OS2_logic_error ("Unable to allocate memory for error message.");
    else
      OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_malloc);
  (_MSG_TYPE (message)) = ((unsigned char) type);
  return (message);
}

void
OS2_destroy_message (msg_t * message)
{
  OS_free ((void *) message);
}

void
OS2_send_message (qid_t qid, msg_t * message)
{
  qid_t twin = (QID_TWIN (qid));
  tqueue_t * tqueue = (QID_TQUEUE (twin));
  if (tqueue == 0)
    OS2_logic_error ("Write to closed QID.");
  (MSG_SENDER (message)) = twin;
  write_tqueue (tqueue, message);
}

msg_t *
OS2_receive_message (qid_t qid, int blockp)
{
  tqueue_t * tqueue = (QID_TQUEUE (qid));
  msg_t * message;
  while (1)
    {
      while (read_tqueue (tqueue, 0))
	;
      if ((TQUEUE_TYPE (tqueue)) == tqt_scm)
	{
	  process_interrupt_messages ();
	  deliver_pending_interrupts ();
	}
      message = (read_subqueue (qid));
      if ((!blockp) || (message != 0))
	break;
      (void) read_tqueue (tqueue, 1);
    }
  return (message);
}

msg_t *
OS2_wait_for_message (qid_t qid, msg_type_t reply_type)
{
  msg_t * reply = (OS2_receive_message (qid, 1));
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
  OS2_wait_for_message (qid, reply_type);
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
  {
    msg_list_t * tail = (QID_SUBQUEUE_TAIL (qid));
    msg_list_t * elt = (OS_malloc (sizeof (struct msg_list_s)));
    (elt -> message) = message;
    (elt -> next) = 0;
    if (tail == 0)
      (QID_SUBQUEUE_HEAD (qid)) = elt;
    else
      (tail -> next) = elt;
    (QID_SUBQUEUE_TAIL (qid)) = elt;
  }
}

static msg_t *
read_subqueue (qid_t qid)
{
  msg_list_t * head = (QID_SUBQUEUE_HEAD (qid));
  if (head == 0)
    return (0);
  {
    msg_t * message = (head -> message);
    (QID_SUBQUEUE_HEAD (qid)) = (head -> next);
    if ((head -> next) == 0)
      (QID_SUBQUEUE_TAIL (qid)) = 0;
    OS_free (head);
    return (message);
  }
}

static int
subqueue_emptyp (qid_t qid)
{
  return ((QID_SUBQUEUE_HEAD (qid)) == 0);
}

static int
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

typedef struct
{
  tqueue_type_t type;
  HQUEUE queue;			/* queue */
  HEV event;			/* associated event semaphore */
} std_tqueue_t;
#define STD_TQUEUE_QUEUE(q) (((std_tqueue_t *) (q)) -> queue)
#define STD_TQUEUE_EVENT(q) (((std_tqueue_t *) (q)) -> event)

tqueue_t *
OS2_make_std_tqueue (void)
{
  tqueue_t * tqueue = (OS_malloc (sizeof (std_tqueue_t)));
  (TQUEUE_TYPE (tqueue)) = tqt_std;
  (STD_TQUEUE_QUEUE (tqueue)) = (OS2_create_queue (QUE_FIFO));
  (STD_TQUEUE_EVENT (tqueue)) = (OS2_create_event_semaphore ());
  return (tqueue);
}

void
OS2_close_std_tqueue (tqueue_t * tqueue)
{
  OS2_close_queue (STD_TQUEUE_QUEUE (tqueue));
  OS2_close_event_semaphore (STD_TQUEUE_EVENT (tqueue));
  OS_free (tqueue);
}

static int
read_std_tqueue (tqueue_t * tqueue, int blockp)
{
  ULONG type;
  ULONG length;
  PVOID data;
  msg_t * message;
  const char * s = "Non-message read from message queue.";

  if (!OS2_read_queue ((STD_TQUEUE_QUEUE (tqueue)),
		       (&type),
		       (&length),
		       (&data),
		       (blockp ? NULLHANDLE : (STD_TQUEUE_EVENT (tqueue)))))
    return (0);
  if (length < (sizeof (msg_t)))
    OS2_logic_error (s);
  message = ((msg_t *) data);
  if ((type != 0) || (length != (MSG_LENGTH (message))))
    OS2_logic_error (s);
  write_subqueue (message);
  return (1);
}

static void
write_std_tqueue (tqueue_t * tqueue, msg_t * message)
{
  OS2_write_queue ((STD_TQUEUE_QUEUE (tqueue)),
		   0,
		   (MSG_LENGTH (message)),
		   ((PVOID) message),
		   0);
}

static tqueue_t *
make_scm_tqueue (void)
{
  tqueue_t * tqueue = (OS2_make_std_tqueue ());
  (TQUEUE_TYPE (tqueue)) = tqt_scm;
  return (tqueue);
}

static int
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
  int result = 0;
  (void) test_and_clear_attention_interrupt ();
  do
    if (read_std_tqueue (tqueue, blockp))
      {
	result = 1;
	/* At most one message needs to be read in blocking mode.  */
	blockp = 0;
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
  while (read_tqueue (tqueue, 0))
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
