/* -*-C-*-

$Id: os2proc.c,v 1.2 1995/04/28 07:05:03 cph Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

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
#include "osproc.h"

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
static PSZ rewrite_arguments (char * const *);
static PSZ rewrite_environment (char * const *);
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
OS_make_subprocess (const char * filename,
		    char * const * argv,
		    char * const * envp,
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
      (dos_exec_pgm,
       (error_object,
	(sizeof (error_object)),
	EXEC_ASYNCRESULT,
	(rewrite_arguments (argv)),
	((envp == 0) ? 0 : (rewrite_environment (envp))),
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

  HFILE stdin;
  HFILE stdout;
  HFILE stderr;
  const char * working_directory;
  int copied_p;
} process_state_t;

static void
save_process_state (int save_working_dir_p)
{
  process_state_t * state = (dstack_alloc (sizeof (process_state_t)));
  (state -> stdin) = NULLHANDLE;
  (state -> stdout) = NULLHANDLE;
  (state -> stderr) = NULLHANDLE;
  (state -> working_directory) = 0;
  (state -> copied_p) = 0;
  transaction_record_action (tat_always, restore_process_state, state);
  if (valid_handle_p (0))
    (state -> stdin) = (copy_handle (0));
  if (valid_handle_p (1))
    (state -> stdout) = (copy_handle (1));
  if (valid_handle_p (2))
    (state -> stderr) = (copy_handle (2));
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
      restore_stdio (0, (state -> stdin));
      restore_stdio (1, (state -> stdout));
      restore_stdio (2, (state -> stderr));
      if ((state -> working_directory) != 0)
	{
	  OS_set_working_dir_pathname (state -> working_directory);
	  OS_free ((void *) (state -> working_directory));
	}
    }
  if ((state -> stdin) != NULLHANDLE)
    (void) dos_close (state -> stdin);
  if ((state -> stdout) != NULLHANDLE)
    (void) dos_close (state -> stdout);
  if ((state -> stderr) != NULLHANDLE)
    (void) dos_close (state -> stderr);
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

static PSZ
rewrite_arguments (char * const * argv)
{
  unsigned long nargs = 0;
  unsigned long length = 0;
  while ((argv [nargs]) != 0)
    {
      length += (strlen (argv [nargs]));
      nargs += 1;
    }
  {
    PSZ result = (dstack_alloc (length + ((nargs < 2) ? 2 : nargs) + 1));
    PSZ scan_result = result;
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

static PSZ
rewrite_environment (char * const * envp)
{
  unsigned long length;
  char * const * scan_env;
  const char * binding;
  PSZ result;
  PSZ scan_result;

  length = 0;
  scan_env = envp;
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

void
OS_process_deallocate (Tprocess process)
{
  (PROCESS_ID (process)) = 0;
  (PROCESS_RAW_STATUS (process)) = process_status_free;
}

int
OS_process_valid_p (Tprocess process)
{
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
OS2_process_any_status_change (void)
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
