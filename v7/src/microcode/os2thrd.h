/* -*-C-*-

$Id: os2thrd.h,v 1.2 1995/04/11 05:17:11 cph Exp $

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

#ifndef SCM_OS2THRD_H
#define SCM_OS2THRD_H

typedef void (* thread_procedure_t) (void *);
typedef void (* error_hook_t) (msg_t *);

typedef struct
{
  error_hook_t error_hook;
  jmp_buf error_restart;
  qid_t error_queue;
} thread_store_t;
#define THREAD_ERROR_HOOK() ((* (OS2_threadstore ())) -> error_hook)
#define THREAD_ERROR_RESTART() ((* (OS2_threadstore ())) -> error_restart)
#define THREAD_ERROR_QUEUE() ((* (OS2_threadstore ())) -> error_queue)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  int code;
  enum syscall_names name;
} sm_syscall_error_t;
#define SM_SYSCALL_ERROR_CODE(m) (((sm_syscall_error_t *) (m)) -> code)
#define SM_SYSCALL_ERROR_NAME(m) (((sm_syscall_error_t *) (m)) -> name)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  long code;
} sm_error_t;
#define SM_ERROR_CODE(m) (((sm_error_t *) (m)) -> code)

typedef msg_t sm_kill_request_t;
#define OS2_make_kill_request() OS2_create_message (mt_kill_request)

extern TID  OS2_beginthread (thread_procedure_t, void *, unsigned int);
extern void OS2_endthread (void);
extern void OS2_kill_thread (TID);
extern TID  OS2_current_tid (void);

#ifdef __IBMC__
#define OS2_threadstore() ((thread_store_t **) (_threadstore ()))
#else
extern thread_store_t ** OS2_threadstore (void);
#endif

extern PID OS2_scheme_pid;
extern TID OS2_scheme_tid;

extern int  OS2_thread_initialize (qid_t);
extern int  OS2_error_message_p (msg_t *);
extern void OS2_handle_error_message (msg_t *);
extern void OS2_ignore_errors (void);
extern void OS2_error_system_call (int, enum syscall_names);
extern void OS2_error_anonymous (void);
extern void OS2_error_unimplemented_primitive (void);
extern void OS2_error_out_of_channels (void);
extern msg_t * OS2_make_syscall_error (int, enum syscall_names);
extern msg_t * OS2_make_error (long);

#endif /* SCM_OS2THRD_H */
