/* -*-C-*-

$Id: os2msg.h,v 1.1 1994/11/28 03:42:59 cph Exp $

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

#ifndef SCM_OS2MSG_H
#define SCM_OS2MSG_H

typedef enum
{
  mt_readahead,
  mt_readahead_ack,
  mt_console_interrupt,
  mt_error,
  mt_kill_request,
  mt_syscall_error,
  mt_timer_event,
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
   type: small integer classifying the type of message
   sender: qid identifying the message sender (used for replies)
   */

#define DECLARE_MSG_HEADER_FIELDS					\
  unsigned char type;							\
  qid_t sender

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
} msg_t;

#define _MSG(m) ((msg_t *) (m))
#define _MSG_TYPE(m) ((_MSG (m)) -> type)
#define MSG_TYPE(m) ((msg_type_t) (_MSG_TYPE (m)))
#define MSG_SENDER(m) ((_MSG (m)) -> sender)

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

extern tqueue_t * OS2_scheme_tqueue;
extern qid_t OS2_interrupt_qid;

extern void OS2_make_qid_pair (qid_t *, qid_t *);
extern void OS2_open_qid (qid_t, tqueue_t *);
extern int OS2_qid_openp (qid_t);
extern void OS2_close_qid (qid_t);
extern msg_length_t OS2_message_type_length (msg_type_t);
extern void OS2_set_message_type_length (msg_type_t, msg_length_t);
extern msg_t * OS2_create_message (msg_type_t);
extern void OS2_destroy_message (msg_t *);
extern void OS2_send_message (qid_t, msg_t *);
extern msg_t * OS2_receive_message (qid_t, int);
extern tqueue_t * OS2_make_std_tqueue (void);

#define MSG_LENGTH(m) (OS2_message_type_length (MSG_TYPE (m)))

#define SET_MSG_TYPE_LENGTH(t, s)					\
  OS2_set_message_type_length ((t), (sizeof (s)))

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  int code;
} sm_console_interrupt_t;
#define SM_CONSOLE_INTERRUPT_CODE(m) (((sm_console_interrupt_t *) (m)) -> code)

typedef msg_t sm_timer_event_t;

#endif /* SCM_OS2MSG_H */
