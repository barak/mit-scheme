/* -*-C-*-

$Id: os2.c,v 1.2 1994/12/19 22:30:05 cph Exp $

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

void *
OS_malloc (unsigned int size)
{
  void * result = (malloc (size));
  if (result == 0)
    OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_malloc);
  return (result);
}

void *
OS_realloc (void * ptr, unsigned int size)
{
  void * result = (realloc (ptr, size));
  if (result == 0)
    OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_realloc);
  return (result);
}

void
OS_free (void * ptr)
{
  free (ptr);
}

HMTX
OS2_create_mutex_semaphore (PSZ name, int sharedp)
{
  HMTX result;
  STD_API_CALL
    (dos_create_mutex_sem,
     (name, (&result), (sharedp ? DC_SEM_SHARED : 0), 0));
  return (result);
}

void
OS2_close_mutex_semaphore (HMTX s)
{
  STD_API_CALL (dos_close_mutex_sem, (s));
}

void
OS2_request_mutex_semaphore (HMTX s)
{
  STD_API_CALL (dos_request_mutex_sem, (s, SEM_INDEFINITE_WAIT));
}

void
OS2_release_mutex_semaphore (HMTX s)
{
  STD_API_CALL (dos_release_mutex_sem, (s));
}

HEV
OS2_create_event_semaphore (PSZ name, int sharedp)
{
  HEV result;
  STD_API_CALL
    (dos_create_event_sem,
     (name, (&result), (sharedp ? DC_SEM_SHARED : 0), 0));
  return (result);
}

void
OS2_close_event_semaphore (HEV s)
{
  STD_API_CALL (dos_close_event_sem, (s));
}

int
OS2_post_event_semaphore (HEV s)
{
  XTD_API_CALL
    (dos_post_event_sem, (s),
     {
       if (rc == ERROR_ALREADY_POSTED)
	 return (1);
     });
  return (0);
}

ULONG
OS2_reset_event_semaphore (HEV s)
{
  ULONG post_count;
  XTD_API_CALL
    (dos_reset_event_sem, (s, (&post_count)),
     {
       if (rc == ERROR_ALREADY_RESET)
	 return (0);
     });
  return (post_count);
}

int
OS2_wait_event_semaphore (HEV s, int blockp)
{
  XTD_API_CALL
    (dos_wait_event_sem,
     (s, (blockp ? SEM_INDEFINITE_WAIT : SEM_IMMEDIATE_RETURN)),
     {
       if ((rc == ERROR_TIMEOUT) && (!blockp))
	 return (0);
     });
  return (1);
}

HQUEUE
OS2_create_queue (ULONG priority)
{
  static unsigned int n = 0;
  char buffer [64];
  HQUEUE result;
  sprintf (buffer, "\\queues\\scm%d\\%d.que", OS2_scheme_pid, (n++));
  STD_API_CALL (dos_create_queue, ((&result), priority, buffer));
  return (result);
}

void
OS2_close_queue (HQUEUE q)
{
  STD_API_CALL (dos_close_queue, (q));
}

void
OS2_write_queue (HQUEUE q, ULONG type, ULONG length, PVOID data, ULONG priority)
{
  STD_API_CALL (dos_write_queue, (q, type, length, data, priority));
}

int
OS2_read_queue (HQUEUE q, ULONG * type, ULONG * length, PVOID * data, HEV s)
{
  REQUESTDATA request;
  BYTE priority;
  (request.pid) = OS2_scheme_pid;
  if (s != NULLHANDLE)
    (void) OS2_reset_event_semaphore (s);
  XTD_API_CALL
    (dos_read_queue,
     (q, (&request), length, data, 0,
      ((s == NULLHANDLE) ? DCWW_WAIT : DCWW_NOWAIT), (&priority), s),
     {
       if ((rc == ERROR_QUE_EMPTY) && (s != NULLHANDLE))
	 return (0);
     });
  (*type) = (request.ulData);
  return (1);
}

ULONG
OS2_system_variable (ULONG index)
{
  ULONG result;
  STD_API_CALL
    (dos_query_sys_info, (index, index, (&result), (sizeof (result))));
  return (result);
}

void
OS2_logic_error_1 (const char * description,
		   const char * file,
		   unsigned int line)
{
  outf_fatal ("\nFatal error in file \"%s\", line %d:\n%s\nGet a wizard.\n",
	      file, line, description);
  termination_init_error ();
}
