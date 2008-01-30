/* -*-C-*-

$Id: os2proc.c,v 1.13 2008/01/30 20:02:17 cph Exp $

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
#include "osproc.h"
#include "osenv.h"

extern const char * OS_working_dir_pathname (void);

typedef struct
{
  PID id;
  unsigned long tick;
  unsigned long sync_tick;
  ULONG raw_reason;
  ULONG reason;
  enum process_status raw_status;
  enum process_status status;
} process_t;
#define _PROCESS(process) (process_table [(process)])
#define PROCESS_ID(process) ((_PROCESS(process)) . id)
#define PROCESS_TICK(process) ((_PROCESS(process)) . tick)
#define PROCESS_SYNC_TICK(process) ((_PROCESS(process)) . sync_tick)
#define PROCESS_RAW_REASON(process) ((_PROCESS(process)) . raw_reason)
#define PROCESS_REASON(process) ((_PROCESS(process)) . reason)
#define PROCESS_RAW_STATUS(process) ((_PROCESS(process)) . raw_status)
#define PROCESS_STATUS(process) ((_PROCESS(process)) . status)

typedef struct
{
  DECLARE_MSG_HEADER_FIELDS;
  PID pid;
} sm_child_death_t;
#define SM_CHILD_DEATH_PID(m) (((sm_child_death_t *) (m)) -> pid)

static void lock_process_status (void);
static void unlock_process_status (void *);
static void save_process_state (int);
static HFILE copy_handle (HFILE);
static int valid_handle_p (HFILE);
static void restore_process_state (void *);
static void restore_stdio (HFILE, HFILE);
static void transfer_stdio (HFILE, Tchannel, enum process_channel_type);
static Tprocess allocate_process (void);
static void allocate_process_abort (void *);
static void child_wait_thread (void *);
static Tprocess find_process (PID);

size_t OS_process_table_size;
enum process_jc_status scheme_jc_status;

static HMTX process_lock;
static process_t * process_table;
static unsigned long process_tick;
static unsigned long sync_tick;
static HEV start_child_event;
TID OS2_child_wait_tid;
static qid_t child_wait_qid_reader;
static qid_t child_wait_qid_writer;

#define PROCESS_STATUS_SYNC(process)					\
{									\
  (PROCESS_STATUS (process)) = (PROCESS_RAW_STATUS (process));		\
  (PROCESS_REASON (process)) = (PROCESS_RAW_REASON (process));		\
  (PROCESS_SYNC_TICK (process)) = (PROCESS_TICK (process));		\
}

static void
lock_process_status (void)
{
  OS2_request_mutex_semaphore (process_lock);
  transaction_record_action (tat_always, unlock_process_status, 0);
}

static void
unlock_process_status (void * ignore)
{
  OS2_release_mutex_semaphore (process_lock);
}

void
OS2_initialize_processes (void)
{
  SET_MSG_TYPE_LENGTH (mt_child_death, sm_child_death_t);
  OS_process_table_size = 4096;
  process_table = (OS_malloc (OS_process_table_size * (sizeof (process_t))));
  {
    Tprocess process;
    for (process = 0; (process < OS_process_table_size); process += 1)
      OS_process_deallocate (process);
  }
  scheme_jc_status = process_jc_status_no_ctty;
  process_tick = 0;
  sync_tick = 0;
  process_lock = (OS2_create_mutex_semaphore (0, 0));
  start_child_event = (OS2_create_event_semaphore (0, 0));
  OS2_make_qid_pair ((& child_wait_qid_reader), (& child_wait_qid_writer));
  OS2_open_qid (child_wait_qid_reader, OS2_scheme_tqueue);
  OS2_child_wait_tid = (OS2_beginthread (child_wait_thread, 0, 0));
}

Tprocess
OS2_make_subprocess (const char * filename,
		     const char * command_line,
		     const char * environment,
		     const char * working_directory,
		     enum process_channel_type channel_in_type,
		     Tchannel channel_in,
		     enum process_channel_type channel_out_type,
		     Tchannel channel_out,
		     enum process_channel_type channel_err_type,
		     Tchannel channel_err)
{
  transaction_begin ();
  save_process_state (working_directory != 0);
  transfer_stdio (0, channel_in, channel_in_type);
  transfer_stdio (1, channel_out, channel_out_type);
  transfer_stdio (2, channel_err, channel_err_type);
  if (working_directory != 0)
    OS_set_working_dir_pathname (working_directory);
  {
    Tprocess child;
    char error_object [100];
    RESULTCODES result_codes;

    lock_process_status ();
    child = (allocate_process ());
    STD_API_CALL
      (dos_exec_pgm, (error_object,
		      (sizeof (error_object)),
		      EXEC_ASYNCRESULT,
		      ((PSZ) command_line),
		      ((PSZ) environment),
		      (& result_codes),
		      ((PSZ) filename)));
    (PROCESS_ID (child)) = (result_codes . codeTerminate);
    (PROCESS_RAW_STATUS (child)) = process_status_running;
    (PROCESS_RAW_REASON (child)) = 0;
    (PROCESS_TICK (child)) = process_tick;
    PROCESS_STATUS_SYNC (child);
    transaction_commit ();
    /* Wake up the child-wait thread if it's sleeping.  */
    (void) OS2_post_event_semaphore (start_child_event);
    return (child);
  }
}

typedef struct
{
  HFILE std_in;
  HFILE std_out;
  HFILE std_err;
  const char * working_directory;
  int copied_p;
} process_state_t;

static void
save_process_state (int save_working_dir_p)
{
  process_state_t * state = (dstack_alloc (sizeof (process_state_t)));
  (state -> std_in) = NULLHANDLE;
  (state -> std_out) = NULLHANDLE;
  (state -> std_err) = NULLHANDLE;
  (state -> working_directory) = 0;
  (state -> copied_p) = 0;
  transaction_record_action (tat_always, restore_process_state, state);
  if (valid_handle_p (0))
    (state -> std_in) = (copy_handle (0));
  if (valid_handle_p (1))
    (state -> std_out) = (copy_handle (1));
  if (valid_handle_p (2))
    (state -> std_err) = (copy_handle (2));
  if (save_working_dir_p)
    {
      const char * dir = (OS_working_dir_pathname ());
      char * copy = (OS_malloc (strlen (dir)));
      strcpy (copy, dir);
      (state -> working_directory) = copy;
    }
  (state -> copied_p) = 1;
}

static int
valid_handle_p (HFILE handle)
{
  ULONG state;
  return ((dos_query_fh_state (handle, (& state))) == NO_ERROR);
}

static HFILE
copy_handle (HFILE handle)
{
  HFILE target = (-1);
  STD_API_CALL (dos_dup_handle, (handle, (& target)));
  return (target);
}

static void
restore_process_state (void * env)
{
  process_state_t * state = env;
  if (state -> copied_p)
    {
      restore_stdio (0, (state -> std_in));
      restore_stdio (1, (state -> std_out));
      restore_stdio (2, (state -> std_err));
      if ((state -> working_directory) != 0)
	{
	  OS_set_working_dir_pathname (state -> working_directory);
	  OS_free ((void *) (state -> working_directory));
	}
    }
  if ((state -> std_in) != NULLHANDLE)
    (void) dos_close (state -> std_in);
  if ((state -> std_out) != NULLHANDLE)
    (void) dos_close (state -> std_out);
  if ((state -> std_err) != NULLHANDLE)
    (void) dos_close (state -> std_err);
}

static void
restore_stdio (HFILE target, HFILE source)
{
  if (source != NULLHANDLE)
    (void) dos_dup_handle (source, (& target));
  else
    (void) dos_close (target);
}

static void
transfer_stdio (HFILE target, Tchannel channel, enum process_channel_type type)
{
  switch (type)
    {
    case process_channel_type_none:
      STD_API_CALL (dos_close, (target));
      break;
    case process_channel_type_explicit:
      STD_API_CALL (dos_dup_handle, ((CHANNEL_HANDLE (channel)), (& target)));
      break;
    }
  switch (type)
    {
    case process_channel_type_inherit:
    case process_channel_type_explicit:
      /* Turn off the no-inherit bit that is normally turned on by
	 Scheme.  Note that the no-inherit bit is not shared between a
	 dup'ed handle and its original handle, so that changing the
	 original does not affect the copy.  This simplifies restoring
	 the original state.  */
      {
	ULONG state;
	STD_API_CALL (dos_query_fh_state, (target, (& state)));
	/* Magic mask 0xFF88 zeroes out high bits and two fields
	   required to be zero by the spec.  When testing, the high
	   bits were not zero, and this caused the system call to
	   complain.  */
	state &= 0xFF88;
	STD_API_CALL
	  (dos_set_fh_state, (target, (state &~ OPEN_FLAGS_NOINHERIT)));
      }
      break;
    }
}

static Tprocess
allocate_process (void)
{
  unsigned int process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_RAW_STATUS (process)) == process_status_free)
      {
	Tprocess * pp = (dstack_alloc (sizeof (Tprocess)));
	(*pp) = process;
	transaction_record_action (tat_abort, allocate_process_abort, pp);
	(PROCESS_RAW_STATUS (process)) = process_status_allocated;
	return (process);
      }
  error_out_of_processes ();
  return (NO_PROCESS);
}

static void
allocate_process_abort (void * environment)
{
  Tprocess process = (* ((Tprocess *) environment));
  if ((PROCESS_RAW_STATUS (process)) == process_status_running)
    dos_kill_process (DKP_PROCESSTREE, (PROCESS_ID (process)));
  OS_process_deallocate (process);
}

void
OS_process_deallocate (Tprocess process)
{
  (PROCESS_ID (process)) = 0;
  (PROCESS_RAW_STATUS (process)) = process_status_free;
}

int
OS_process_valid_p (Tprocess process)
{
  if (process > OS_process_table_size)
    return (0);
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_exited:
    case process_status_signalled:
    case process_status_running:
      return (1);
    default:
      return (0);
    }
}

int
OS_process_continuable_p (Tprocess process)
{
  return ((PROCESS_RAW_STATUS (process)) == process_status_running);
}

int
OS_process_foregroundable_p (Tprocess process)
{
  return (0);
}

pid_t
OS_process_id (Tprocess process)
{
  return (PROCESS_ID (process));
}

enum process_jc_status
OS_process_jc_status (Tprocess process)
{
  return (process_jc_status_no_ctty);
}

int
OS_process_status_sync (Tprocess process)
{
  transaction_begin ();
  lock_process_status ();
  {
    int result = ((PROCESS_TICK (process)) != (PROCESS_SYNC_TICK (process)));
    if (result)
      PROCESS_STATUS_SYNC (process);
    transaction_commit ();
    return (result);
  }
}

int
OS_process_status_sync_all (void)
{
  transaction_begin ();
  lock_process_status ();
  {
    int result = (process_tick != sync_tick);
    if (result)
      sync_tick = process_tick;
    transaction_commit ();
    return (result);
  }
}

int
OS_process_any_status_change (void)
{
  return (process_tick != sync_tick);
}

enum process_status
OS_process_status (Tprocess process)
{
  return (PROCESS_STATUS (process));
}

unsigned short
OS_process_reason (Tprocess process)
{
  return (PROCESS_REASON (process));
}

void
OS_process_send_signal (Tprocess process, int sig)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_process_kill (Tprocess process)
{
  XTD_API_CALL
    (dos_kill_process, (DKP_PROCESSTREE, (PROCESS_ID (process))),
     {
       if (rc == ERROR_ZOMBIE_PROCESS)
	 return;
     });
}

void
OS_process_stop (Tprocess process)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_process_interrupt (Tprocess process)
{
  STD_API_CALL
    (dos_send_signal_exception, ((PROCESS_ID (process)), XCPT_SIGNAL_INTR));
}

void
OS_process_quit (Tprocess process)
{
  STD_API_CALL
    (dos_send_signal_exception, ((PROCESS_ID (process)), XCPT_SIGNAL_BREAK));
}

void
OS_process_hangup (Tprocess process)
{
  /* Edwin assumes that this primitive works.  Under unix, the default
     behavior of SIGHUP is to kill the process, so we will emulate
     SIGHUP by killing the process.  */
  OS_process_kill (process);
}

void
OS_process_continue_background (Tprocess process)
{
  /* A no-op, this should only be called when OS_process_continuable_p
     is true, i.e. when the process is already running.  */
}

void
OS_process_continue_foreground (Tprocess process)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_process_wait (Tprocess process)
{
  while (((PROCESS_RAW_STATUS (process)) == process_status_running)
	 && ((OS2_message_availablep (child_wait_qid_reader, 1))
	     != mat_interrupt))
    {
      msg_t * message = (OS2_receive_message (child_wait_qid_reader, 1, 0));
      PID pid = (SM_CHILD_DEATH_PID (message));
      OS2_destroy_message (message);
      if (pid == (PROCESS_ID (process)))
	break;
    }
}

static void
child_wait_thread (void * arg)
{
  EXCEPTIONREGISTRATIONRECORD registration;
  (void) OS2_thread_initialize ((&registration), QID_NONE);
 main_loop:
  (void) OS2_wait_event_semaphore (start_child_event, 1);
  (void) OS2_reset_event_semaphore (start_child_event);
  while (1)
    {
      RESULTCODES codes;
      PID pid;
      Tprocess process;
      XTD_API_CALL
	(dos_wait_child, (DCWA_PROCESS, DCWW_WAIT, (& codes), (& pid), 0),
	 {
	   if (rc == ERROR_WAIT_NO_CHILDREN)
	     goto main_loop;
	 });
      OS2_request_mutex_semaphore (process_lock);
      process = (find_process (pid));
      if (process == NO_PROCESS)
	OS2_release_mutex_semaphore (process_lock);
      else
	{
	  if ((codes . codeTerminate) == TC_EXIT)
	    {
	      (PROCESS_RAW_STATUS (process)) = process_status_exited;
	      (PROCESS_RAW_REASON (process)) = (codes . codeResult);
	    }
	  else
	    {
	      (PROCESS_RAW_STATUS (process)) = process_status_signalled;
	      (PROCESS_RAW_REASON (process)) = 0;
	    }
	  (PROCESS_TICK (process)) = (++process_tick);
	  OS2_release_mutex_semaphore (process_lock);
	  {
	    msg_t * message = (OS2_create_message (mt_child_death));
	    (SM_CHILD_DEATH_PID (message)) = pid;
	    OS2_send_message (child_wait_qid_writer, message);
	  }
	}
    }
}

static Tprocess
find_process (PID pid)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_ID (process)) == pid)
      return (process);
  return (NO_PROCESS);
}

/* OBSOLETE */

static const char * rewrite_arguments (const char **);
static const char * rewrite_environment (const char **);

Tprocess
OS_make_subprocess (const char * filename,
		    const char ** argv,
		    const char ** envp,
		    const char * working_directory,
		    enum process_ctty_type ctty_type,
		    char * ctty_name,
		    enum process_channel_type channel_in_type,
		    Tchannel channel_in,
		    enum process_channel_type channel_out_type,
		    Tchannel channel_out,
		    enum process_channel_type channel_err_type,
		    Tchannel channel_err)
{
  if ((ctty_type != process_ctty_type_none)
      || (channel_in_type == process_channel_type_ctty)
      || (channel_out_type == process_channel_type_ctty)
      || (channel_err_type == process_channel_type_ctty))
    OS2_error_anonymous ();
  return (OS2_make_subprocess (filename,
			       (rewrite_arguments (argv)),
			       (rewrite_environment (envp)),
			       working_directory,
			       channel_in_type, channel_in,
			       channel_out_type, channel_out,
			       channel_err_type, channel_err));
}

static const char *
rewrite_arguments (const char ** argv)
{
  unsigned long nargs = 0;
  unsigned long length = 0;
  while ((argv [nargs]) != 0)
    {
      length += (strlen (argv [nargs]));
      nargs += 1;
    }
  {
    char * result = (dstack_alloc (length + ((nargs < 2) ? 2 : nargs) + 1));
    char * scan_result = result;
    if (nargs == 0)
      (*scan_result++) = '\0';
    else
      {
	unsigned long limit = (nargs - 1);
	unsigned long index = 0;
	while (1)
	  {
	    const char * arg = (argv [index]);
	    while (1)
	      {
		char c = (*arg++);
		if (c == '\0')
		  break;
		(*scan_result++) = c;
	      }
	    if (index == limit)
	      break;
	    (*scan_result++) = ((index == 0) ? '\0' : ' ');
	    index += 1;
	  }
      }
    (*scan_result++) = '\0';
    (*scan_result) = '\0';
    return (result);
  }
}

static const char *
rewrite_environment (const char ** envp)
{
  unsigned long length = 0;
  const char ** scan_env = envp;
  const char * binding;
  char * result;
  char * scan_result;

  if (envp == 0)
    return (0);
  while ((binding = (*scan_env++)) != 0)
    length += ((strlen (binding)) + 1);
  result = (dstack_alloc (length + 1));
  scan_result = result;
  scan_env = envp;
  while ((binding = (*scan_env++)) != 0)
    while (((*scan_result++) = (*binding++)) != '\0')
      ;
  (*scan_result) = '\0';
  return (result);
}
