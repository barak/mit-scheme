/* -*-C-*-

$Id: 8b82e7c05c10cc5bf74dd940ceca43fa329baec9 $

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

/* Define OS2_USE_SUBHEAP_MALLOC to use this custom malloc
   implementation for most of Scheme's memory.  This implementation,
   by virtue of being separate from the system's malloc, and also by
   having specific redundancy checks, offers some features that can be
   valuable during debugging of memory problems.  */

/* #define OS2_USE_SUBHEAP_MALLOC */
#ifdef OS2_USE_SUBHEAP_MALLOC

static PVOID malloc_object;
static ULONG malloc_object_size = 0x200000; /* two megabytes */

typedef struct
{
  char * check;
  unsigned int size;
} malloc_header_t;

void
OS2_initialize_malloc (void)
{
  if (((DosAllocMem ((&malloc_object),
		     malloc_object_size,
		     (PAG_EXECUTE | PAG_READ | PAG_WRITE)))
       != NO_ERROR)
      || ((DosSubSetMem (malloc_object,
			 (DOSSUB_INIT | DOSSUB_SPARSE_OBJ | DOSSUB_SERIALIZE),
			 malloc_object_size))
	  != NO_ERROR))
    termination_init_error ();
}

static malloc_header_t *
guarantee_valid_malloc_pointer (void * ptr)
{
  malloc_header_t * header = (((malloc_header_t *) ptr) - 1);
  if ((((char *) header) < ((char *) malloc_object))
      || (((char *) header) > (((char *) malloc_object) + malloc_object_size))
      || ((((ULONG) header) & 7) != 0)
      || ((header -> check) != (((char *) header) - 47)))
    OS2_logic_error ("Bad pointer passed to OS_free.");
  return (header);
}

void *
OS2_malloc_noerror (unsigned long size)
{
  PVOID result;
  APIRET rc
    = (DosSubAllocMem (malloc_object,
		       (&result),
		       (size + (sizeof (malloc_header_t)))));
  if (rc == ERROR_DOSSUB_NOMEM)
    return (0);
  if (rc != NO_ERROR)
    {
      char buffer [1024];
      sprintf (buffer, "DosSubAllocMem error: %d.", rc);
      OS2_logic_error (buffer);
    }
  (((malloc_header_t *) result) -> check) = (((char *) result) - 47);
  (((malloc_header_t *) result) -> size) = size;
  return (((malloc_header_t *) result) + 1);
}

void
OS_free (void * ptr)
{
  malloc_header_t * header = (guarantee_valid_malloc_pointer (ptr));
  APIRET rc;
  (header -> check) = 0;
  rc = (DosSubFreeMem (malloc_object, header, (header -> size)));
  if (rc != NO_ERROR)
    {
      char buffer [1024];
      sprintf (buffer, "DosSubFreeMem error: %d.", rc);
      OS2_logic_error (buffer);
    }
}

void *
OS2_realloc_noerror (void * ptr, unsigned long size)
{
  unsigned long osize = ((guarantee_valid_malloc_pointer (ptr)) -> size);
  if (osize == size)
    return (ptr);
  {
    void * result = (OS2_malloc_noerror (size));
    if (result != 0)
      {
	char * scan1 = ptr;
	char * end1 = (scan1 + ((osize < size) ? osize : size));
	char * scan2 = result;
	while (scan1 < end1)
	  (*scan2++) = (*scan1++);
	OS_free (ptr);
      }
    return (result);
  }
}

#else /* not OS2_USE_SUBHEAP_MALLOC */

/* Use malloc.  */

void
OS2_initialize_malloc (void)
{
}

void *
OS2_malloc_noerror (unsigned long size)
{
  return (malloc (size));
}

void *
OS2_realloc_noerror (void * ptr, unsigned long size)
{
  return (realloc (ptr, size));
}

void
OS_free (void * ptr)
{
  free (ptr);
}

#endif /* not OS2_USE_SUBHEAP_MALLOC */

void *
OS_malloc_init (size_t size)
{
  return (OS2_malloc_noerror (size));
}

void *
OS_malloc (size_t size)
{
  void * result = (OS2_malloc_noerror (size));
  if (result == 0)
    OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_malloc);
  return (result);
}

void *
OS_realloc (void * ptr, size_t size)
{
  void * result = (OS2_realloc_noerror (ptr, size));
  if (result == 0)
    OS2_error_system_call (ERROR_NOT_ENOUGH_MEMORY, syscall_realloc);
  return (result);
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
  while (1)
    {
      APIRET rc = (dos_request_mutex_sem (s, SEM_INDEFINITE_WAIT));
      if (rc == NO_ERROR)
	break;
      /* This return code has been regularly occurring on my machine.
	 On one occurrence, I proceeded past the error in the
	 debugger, and the program continued working without errors.
	 However, more recently proceeding past this error has caused
	 a subsequent error when unlocking the semaphore because the
	 lock didn't succeed.  IBM tech support is mystified because
	 this code appears nowhere in their sources.  */
      if (rc == 3000)
	{
	  PID pid;
	  TID tid;
	  ULONG count;
	  DosQueryMutexSem (s, (&pid), (&tid), (&count));
	  if ((count > 0) && (tid == (OS2_current_tid ())))
	    break;
	}
      else if (rc != ERROR_INTERRUPT)
	OS2_error_system_call (rc, syscall_dos_request_mutex_sem);
    }
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

HMTX OS2_create_queue_lock;

HQUEUE
OS2_create_queue (ULONG priority)
{
  static unsigned int n = 0;
  unsigned int this_n;
  char buffer [64];
  HQUEUE result;
  OS2_request_mutex_semaphore (OS2_create_queue_lock);
  this_n = (n++);
  OS2_release_mutex_semaphore (OS2_create_queue_lock);
  sprintf (buffer, "\\queues\\scm%d\\%d.que", OS2_scheme_pid, this_n);
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

int
OS2_essential_thread_p (TID tid)
{
  extern TID OS2_pm_tid;
  extern TID OS2_timer_tid;
  extern TID OS2_console_tid;
  return ((tid == OS2_scheme_tid)
	  || (tid == OS2_pm_tid)
	  || (tid == OS2_timer_tid)
	  || (tid == OS2_console_tid));
}

void
OS2_logic_error_1 (const char * description,
		   const char * file,
		   unsigned int line)
{
  extern TID OS2_child_wait_tid;
  char * format = "%s error in thread %d, file \"%s\", line %d: %s%s\
  This indicates a bug in the Scheme implementation.\
  Please report this information to a Scheme wizard.";
  TID tid = (OS2_current_tid ());
  if (OS2_essential_thread_p (tid))
    {
      outf_fatal (format, "Fatal", tid, file, line, description, "");
      outf_fatal ("\n\n");
      termination_init_error ();
    }
  else
    {
      extern void OS2_message_box (const char *, const char *, int);
      char buffer [1024];
      sprintf (buffer, format, "Non-fatal", tid, file, line, description,
	       ((tid == OS2_child_wait_tid)
		? "  The thread will be killed.\
  Afterwards, Scheme will not be able to manage subprocesses properly."
		: "  The thread will be killed."));
      OS2_message_box ("Scheme Error", buffer, 0);
      OS2_endthread ();
    }
}
