/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#include "os2.h"
#include "prims.h"
#include "errors.h"

#ifdef __IBMC__
#define HAVE_BEGINTHREAD
#endif

#ifdef __WATCOMC__
#include <process.h>
#define HAVE_BEGINTHREAD
#endif

#ifdef __EMX__
#define HAVE_BEGINTHREAD
#endif

extern void OS2_create_msg_queue (void);
extern ULONG APIENTRY OS2_subthread_exception_handler
  (PEXCEPTIONREPORTRECORD, PEXCEPTIONREGISTRATIONRECORD, PCONTEXTRECORD,
   PVOID);

TID
OS2_beginthread (thread_procedure_t procedure,
		 void * argument,
		 unsigned int stack_size)
{
  ULONG ss
    = ((stack_size < 0x2000)
       ? 0x2000
       : ((stack_size + 0xfff) & (~0xfff)));
#ifdef HAVE_BEGINTHREAD
  int result = (_beginthread (procedure, 0, ss, argument));
  if (result < 0)
    OS2_error_system_call (ERROR_MAX_THRDS_REACHED, syscall_beginthread);
  return (result);
#else /* not HAVE_BEGINTHREAD */
  TID tid;
  STD_API_CALL (dos_create_thread,
		((&tid), ((PFNTHREAD) procedure), ((ULONG) argument), 0, ss));
  return (tid);
#endif /* not HAVE_BEGINTHREAD */
}

void
OS2_endthread (void)
{
  DosUnsetExceptionHandler (THREAD_EXCEPTION_HANDLER ());
#ifdef HAVE_BEGINTHREAD
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

TID
OS2_current_tid (void)
{
  PTIB ptib;
  PPIB ppib;
  STD_API_CALL (dos_get_info_blocks, ((&ptib), (&ppib)));
  return (ptib -> tib_ptib2 -> tib2_ultid);
}

#ifndef __IBMC__
#define MAX_TID 999
static thread_store_t * thread_store_array [MAX_TID + 1];

thread_store_t **
OS2_threadstore (void)
{
  TID tid = (OS2_current_tid ());
  if (tid > MAX_TID)
    OS2_logic_error ("Unexpectedly large TID.");
  return (& (thread_store_array [tid]));
}
#endif

PID OS2_scheme_pid;
TID OS2_scheme_tid;

static void thread_initialize_1 (qid_t);
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
OS2_thread_initialize (PEXCEPTIONREGISTRATIONRECORD registration,
		       qid_t error_qid)
{
  /* Every thread has a message queue, so that we can use message
     dialogs to report fatal errors to the user.  Otherwise, Scheme
     will just die with no explanation.  */
  OS2_create_msg_queue ();
  return (OS2_thread_initialize_1 (registration, error_qid));
}

int
OS2_thread_initialize_1 (PEXCEPTIONREGISTRATIONRECORD registration,
			 qid_t error_qid)
{
  thread_initialize_1 (error_qid);
  (registration -> ExceptionHandler) = OS2_subthread_exception_handler;
  DosSetExceptionHandler (registration);
  (THREAD_EXCEPTION_HANDLER ()) = registration;
  (THREAD_ERROR_HOOK ()) = send_error;
  return (setjmp (THREAD_ERROR_RESTART ()));
}

static void
thread_initialize_1 (qid_t error_qid)
{
  (* (OS2_threadstore ())) = (OS_malloc (sizeof (thread_store_t)));
  (THREAD_ERROR_QUEUE ()) = error_qid;
  ((THREAD_FATAL_ERROR_BUFFER ()) [0]) = '\0';
}

char *
OS2_thread_fatal_error_buffer (void)
{
  /* The default buffer may get used if an error occurs very early in
     a thread, before the regular error buffer is allocated.  This can
     easily happen in the Scheme thread, but shouldn't happen in the
     other threads.  */
  static char default_buffer [1024] = "";
  return
    (((* (OS2_threadstore ())) == 0)
     ? default_buffer
     : (THREAD_FATAL_ERROR_BUFFER ()));
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
