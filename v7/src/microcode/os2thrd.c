/* -*-C-*-

$Id: os2thrd.c,v 1.1 1994/11/28 03:43:00 cph Exp $

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
#include "prims.h"
#include "errors.h"

TID
OS2_beginthread (thread_procedure_t procedure,
		 void * argument,
		 unsigned int stack_size)
{
  ULONG ss
    = ((stack_size < 0x2000)
       ? 0x2000
       : ((stack_size + 0xfff) & (~0xfff)));
#ifdef __IBMC__
  int result = (_beginthread (procedure, 0, ss, argument));
  if (result < 0)
    OS2_error_system_call (ERROR_MAX_THRDS_REACHED, syscall_beginthread);
  return (result);
#else /* not __IBMC__ */
  TID tid;
  STD_API_CALL (dos_create_thread,
		((&tid), ((PFNTHREAD) procedure), ((ULONG) argument), 0, ss));
  return (tid);
#endif /* not __IBMC__ */
}

void
OS2_endthread (void)
{
#ifdef __IBMC__
  _endthread ();
#else
  dos_exit (EXIT_THREAD, 0);
#endif
}

void
OS2_kill_thread (TID tid)
{
  STD_API_CALL (dos_kill_thread, (tid));
}

#ifndef __IBMC__
#define MAX_TID 999
static thread_store_t * thread_store_array [MAX_TID + 1];

thread_store_t **
OS2_threadstore (void)
{
  PTIB ptib;
  PPIB ppib;
  TID tid;
  STD_API_CALL (dos_get_info_blocks, ((&ptib), (&ppib)));
  tid = (ptib -> tib_ptib2 -> tib2_ultid);
  if (tid > MAX_TID)
    OS2_logic_error ("Unexpectedly large TID.");
  return (& (thread_store_array [tid]));
}
#endif

PID OS2_scheme_pid;
TID OS2_scheme_tid;

static void thread_initialize_1 (qid_t);
static int  thread_initialize_error_hook (void);
static void restore_errors (void *);
static void signal_error (msg_t *);
static void ignore_error (msg_t *);
static void send_error (msg_t *);

void
OS2_initialize_scheme_thread (void)
{
  SET_MSG_TYPE_LENGTH (mt_syscall_error, sm_syscall_error_t);
  SET_MSG_TYPE_LENGTH (mt_error, sm_error_t);
  SET_MSG_TYPE_LENGTH (mt_kill_request, sm_kill_request_t);
  {
    PTIB ptib;
    PPIB ppib;
    STD_API_CALL (dos_get_info_blocks, ((&ptib), (&ppib)));
    OS2_scheme_pid = (ppib -> pib_ulpid);
    OS2_scheme_tid = (ptib -> tib_ptib2 -> tib2_ultid);
  }
  thread_initialize_1 (QID_NONE);
  (THREAD_ERROR_HOOK ()) = signal_error;
}

int
OS2_thread_initialize (qid_t error_qid)
{
  thread_initialize_1 (error_qid);
  return (thread_initialize_error_hook ());
}

static void
thread_initialize_1 (qid_t error_qid)
{
  (* (OS2_threadstore ())) = (OS_malloc (sizeof (thread_store_t)));
  (THREAD_ERROR_QUEUE ()) = error_qid;
}

static int
thread_initialize_error_hook (void)
{
  (THREAD_ERROR_HOOK ()) = send_error;
  return (setjmp (THREAD_ERROR_RESTART ()));
}

int
OS2_error_message_p (msg_t * message)
{
  msg_type_t type = (MSG_TYPE (message));
  return ((type == mt_syscall_error) || (type == mt_error));
}

void
OS2_handle_error_message (msg_t * message)
{
  (* (THREAD_ERROR_HOOK ())) (message);
}

void
OS2_ignore_errors (void)
{
  error_hook_t * hp = (dstack_alloc (sizeof (error_hook_t)));
  (*hp) = (THREAD_ERROR_HOOK ());
  transaction_record_action (tat_always, restore_errors, hp);
  (THREAD_ERROR_HOOK ()) = ignore_error;
}

static void
restore_errors (void * hp)
{
  (THREAD_ERROR_HOOK ()) = (* ((error_hook_t *) hp));
}

void
OS2_error_system_call (int code, enum syscall_names name)
{
  OS2_handle_error_message (OS2_make_syscall_error (code, name));
}

void
OS2_error_anonymous (void)
{
  OS2_handle_error_message (OS2_make_error (ERR_EXTERNAL_RETURN));
}

void
OS2_error_unimplemented_primitive (void)
{
  OS2_handle_error_message (OS2_make_error (ERR_UNDEFINED_PRIMITIVE));
}

void
OS2_error_out_of_channels (void)
{
  OS2_handle_error_message (OS2_make_error (ERR_OUT_OF_FILE_HANDLES));
}

static void
signal_error (msg_t * message)
{
  switch (MSG_TYPE (message))
    {
    case mt_syscall_error:
      {
	int code = (SM_SYSCALL_ERROR_CODE (message));
	enum syscall_names name = (SM_SYSCALL_ERROR_NAME (message));
	OS2_destroy_message (message);
	error_system_call (code, name);
      }
      break;
    case mt_error:
      {
	long code = (SM_ERROR_CODE (message));
	OS2_destroy_message (message);
	signal_error_from_primitive (code);
      }
      break;
    default:
      OS2_logic_error ("Non-error message passed to signal_error.");
      break;
    }
}

static void
ignore_error (msg_t * message)
{
}

static void
send_error (msg_t * message)
{
  if ((THREAD_ERROR_QUEUE ()) == QID_NONE)
    OS2_logic_error ("send_error called when no error queue defined.");
  OS2_send_message ((THREAD_ERROR_QUEUE ()), message);
  longjmp ((THREAD_ERROR_RESTART ()), 1);
}

msg_t *
OS2_make_syscall_error (int code, enum syscall_names name)
{
  msg_t * message = (OS2_create_message (mt_syscall_error));
  (SM_SYSCALL_ERROR_CODE (message)) = code;
  (SM_SYSCALL_ERROR_NAME (message)) = name;
  return (message);
}

msg_t *
OS2_make_error (long code)
{
  msg_t * message = (OS2_create_message (mt_error));
  (SM_ERROR_CODE (message)) = code;
  return (message);
}
