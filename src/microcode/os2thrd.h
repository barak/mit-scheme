/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#ifndef SCM_OS2THRD_H
#define SCM_OS2THRD_H

typedef void (* thread_procedure_t) (void *);
typedef void (* error_hook_t) (msg_t *);

typedef struct
{
  error_hook_t error_hook;
  jmp_buf error_restart;
  PEXCEPTIONREGISTRATIONRECORD exception_handler;
  qid_t error_queue;
  char fatal_error_buffer [1024];
} thread_store_t;
#define THREAD_ERROR_HOOK() ((* (OS2_threadstore ())) -> error_hook)
#define THREAD_ERROR_RESTART() ((* (OS2_threadstore ())) -> error_restart)
#define THREAD_ERROR_QUEUE() ((* (OS2_threadstore ())) -> error_queue)
#define THREAD_FATAL_ERROR_BUFFER()					\
  ((* (OS2_threadstore ())) -> fatal_error_buffer)
#define THREAD_EXCEPTION_HANDLER()					\
  ((* (OS2_threadstore ())) -> exception_handler)

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

extern int  OS2_thread_initialize (PEXCEPTIONREGISTRATIONRECORD, qid_t);
extern int  OS2_thread_initialize_1 (PEXCEPTIONREGISTRATIONRECORD, qid_t);
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
