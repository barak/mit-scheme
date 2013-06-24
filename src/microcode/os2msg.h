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

#ifndef SCM_OS2MSG_H
#define SCM_OS2MSG_H

typedef enum
{
  /* This is sent to acknowledge that the other end of a qid pair has
     been opened.  Sometimes it is necessary to wait until the
     connection is established before proceeding.  */
  mt_init,

  /* This is sent by a "readahead" thread whenever it has some data to
     give to the other end of the connection.  These messages are
     generated asynchronously whenever the readahead is available.  */
  mt_readahead,

  /* This is sent by the receiver of a readahead message.  It is used
     to regulate the amount of readahead in the connection.
     Typically, the readahead thread won't generate any more readahead
     messages until the readahead_ack is received.  */
  mt_readahead_ack,

  /* This is a console interrupt event.  It is generated automatically
     by the console readahead thread, and causes a Scheme character
     interrupt to be signalled in the interrupt-code register.  */
  mt_console_interrupt,

  /* This is a timer interrupt event.  It is generated automatically
     by the timer thread when the timer is active.  */
  mt_timer_event,

  /* This event signals the termination of a child process.  It is
     generated automatically by the thread that monitors child
     processes.  */
  mt_child_death,

  /* These are error messages.  They are sent as a reply to a request
     when an error is generated during the processing of the request.  */
  mt_error,
  mt_syscall_error,

  /* This is a generic reply that is used to acknowledge requests that
     return no meaningful data other than that they have completed.  */
  mt_generic_reply,

  /* This machine-generated file contains most of the PM message types.  */
#include "os2pm-mt.h"

  /* These are messages that command the PM thread to perform specific
     actions.  A command that does not have a specific reply type will
     receive a generic reply when the PM code is configured to do
     handshaking; normally such a command has no reply.  */
  mt_window_pos_request,	/* request position of window's frame */
  mt_window_pos_reply,
  mt_window_size_request,	/* request size of window's client area */
  mt_window_size_reply,
  mt_window_frame_size_request,	/* request size of window's frame */
  mt_window_frame_size_reply,

  /* These are also PM thread commands, but they operate on
     presentation spaces rather than on windows.  */
  mt_ps_set_bitmap_request,	/* associate a bitmap with a memory PS */
  mt_ps_set_bitmap_reply,

  /* These are messages that are automatically generated by the PM
     thread when the corresponding events occur.  */
  mt_pm_event,			/* undecoded PM input event */
  mt_paint_event,		/* window needs painting */

  /* This requests the thread on the other end of the connection to
     kill itself.  At present this request is not used.  */
  mt_kill_request,
  mt_supremum
} msg_type_t;
#define MSG_TYPE_SUP ((unsigned int) mt_supremum)
#define MSG_TYPE_MAX (MSG_TYPE_SUP - 1)

typedef unsigned char qid_t;
#define QID_MAX (UCHAR_MAX - 1)
#define QID_NONE UCHAR_MAX

typedef unsigned short msg_length_t;
#define MSG_LENGTH_MAX USHRT_MAX

/* Fields of message header:
   type: msg_type_t identifying the type of message
   sender: qid identifying the message sender (used for replies)
   */

#define DECLARE_MSG_HEADER_FIELDS					\
  msg_type_t _msg_type;							\
  qid_t _msg_sender

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
} msg_t;

#define _MSG(m) ((msg_t *) (m))
#define MSG_TYPE(m) ((_MSG (m)) -> _msg_type)
#define MSG_SENDER(m) ((_MSG (m)) -> _msg_sender)

typedef enum
{
  tqt_std,
  tqt_scm,
  tqt_pm
} tqueue_type_t;

typedef struct
{
  tqueue_type_t type;
} tqueue_t;
#define TQUEUE_TYPE(q) (((tqueue_t *) (q)) -> type)

typedef msg_t * (* qid_receive_filter_t) (msg_t *);

typedef enum { mat_not_available, mat_available, mat_interrupt } msg_avail_t;

extern tqueue_t * OS2_scheme_tqueue;
extern qid_t OS2_interrupt_qid;

extern void OS2_make_qid_pair (qid_t *, qid_t *);
extern void OS2_open_qid (qid_t, tqueue_t *);
extern int OS2_qid_openp (qid_t);
extern void OS2_close_qid (qid_t);
extern tqueue_t * OS2_qid_tqueue (qid_t);
extern qid_t OS2_qid_twin (qid_t);
extern void OS2_close_qid_pair (qid_t);
extern void OS2_set_qid_receive_filter (qid_t, qid_receive_filter_t);
extern msg_length_t OS2_message_type_length (msg_type_t);
extern void OS2_set_message_type_length (msg_type_t, msg_length_t);
extern msg_t * OS2_create_message_1 (msg_type_t, msg_length_t);
extern void OS2_destroy_message (msg_t *);
extern void OS2_send_message (qid_t, msg_t *);
extern msg_t * OS2_receive_message (qid_t, int, int);
extern msg_avail_t OS2_message_availablep (qid_t, int);
extern msg_t * OS2_wait_for_message (qid_t, msg_type_t);
extern msg_t * OS2_message_transaction (qid_t, msg_t *, msg_type_t);
extern void OS2_unread_message (qid_t, msg_t *);
extern msg_avail_t OS2_scheme_tqueue_block (void);
extern tqueue_t * OS2_make_std_tqueue (void);
extern void OS2_close_std_tqueue (tqueue_t *);

extern void * OS2_create_msg_fifo (void);
void OS2_destroy_msg_fifo (void *);
extern void OS2_msg_fifo_insert (void *, void *);
extern void OS2_msg_fifo_insert_front (void *, void *);
extern void * OS2_msg_fifo_remove (void *);
extern void * OS2_msg_fifo_remove_last (void *);
extern void ** OS2_msg_fifo_remove_all (void *);
extern int OS2_msg_fifo_emptyp (void *);
extern unsigned int OS2_msg_fifo_count (void *);
extern void * OS2_msg_fifo_last (void *);

#define MSG_LENGTH(m) (OS2_message_type_length (MSG_TYPE (m)))

#define SET_MSG_TYPE_LENGTH(t, s)					\
  OS2_set_message_type_length ((t), (sizeof (s)))

#define OS2_create_message(type) OS2_create_message_1 ((type), 0)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  int code;
} sm_console_interrupt_t;
#define SM_CONSOLE_INTERRUPT_CODE(m) (((sm_console_interrupt_t *) (m)) -> code)

typedef msg_t sm_timer_event_t;
typedef msg_t sm_init_t;
typedef msg_t sm_generic_reply_t;

#endif /* SCM_OS2MSG_H */
