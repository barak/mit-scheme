/* -*-C-*-

$Id: ntproc.c,v 1.1 1997/10/22 05:26:38 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

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

#include "nt.h"
#include "ntproc.h"
#include "ntio.h"
#include "ntscreen.h"
#include "ntgui.h"

extern const char * OS_working_dir_pathname (void);

typedef struct
{
  PROCESS_INFORMATION handles;
  unsigned long tick;
  unsigned long sync_tick;
  DWORD raw_reason;
  DWORD reason;
  enum process_status raw_status;
  enum process_status status;
} process_t;
#define _PROCESS(process) (process_table [(process)])
#define PROCESS_HANDLES(process) ((_PROCESS(process)) . handles)
#define PROCESS_TICK(process) ((_PROCESS(process)) . tick)
#define PROCESS_SYNC_TICK(process) ((_PROCESS(process)) . sync_tick)
#define PROCESS_RAW_REASON(process) ((_PROCESS(process)) . raw_reason)
#define PROCESS_REASON(process) ((_PROCESS(process)) . reason)
#define PROCESS_RAW_STATUS(process) ((_PROCESS(process)) . raw_status)
#define PROCESS_STATUS(process) ((_PROCESS(process)) . status)

#define PROCESS_HANDLE(process) ((PROCESS_HANDLES (process)) . hProcess)
#define PROCESS_ID(process) ((PROCESS_HANDLES (process)) . dwProcessId)

#ifndef NT_DEFAULT_PROCESS_TABLE_SIZE
#define NT_DEFAULT_PROCESS_TABLE_SIZE 4096
#endif
size_t OS_process_table_size;
enum process_jc_status scheme_jc_status;

static process_t * process_table;
static unsigned long process_tick;
static unsigned long sync_tick;

#undef USE_PROCESS_TABLE_LOCK
#ifdef USE_PROCESS_TABLE_LOCK
static CRITICAL_SECTION process_table_lock;
#define GRAB_PROCESS_TABLE() EnterCriticalSection (&process_table_lock)
#define RELEASE_PROCESS_TABLE() LeaveCriticalSection (&process_table_lock)
#else
#define GRAB_PROCESS_TABLE()
#define RELEASE_PROCESS_TABLE()
#endif

#define PROCESS_STATUS_SYNC(process)					\
{									\
  (PROCESS_STATUS (process)) = (PROCESS_RAW_STATUS (process));		\
  (PROCESS_REASON (process)) = (PROCESS_RAW_REASON (process));		\
  (PROCESS_SYNC_TICK (process)) = (PROCESS_TICK (process));		\
}

/* I have no idea what a good value for this might be, so I've picked
   a random non-zero value.  */
#define TERMINATE_PROCESS_EXIT_CODE 0xFF

static void lock_process_table (void);
static void lock_process_table_1 (void *);
static void save_process_state (void);
static void restore_process_state (void *);
static void transfer_stdio (DWORD, Tchannel, enum process_channel_type);
static HANDLE copy_handle (HANDLE);
static Tprocess allocate_process (void);
static void allocate_process_abort (void *);
static HWND find_child_console (DWORD);
static BOOL CALLBACK find_child_console_1 (HWND, LPARAM);
static void test_process_status_change (Tprocess);

void
NT_initialize_processes (void)
{
  OS_process_table_size = NT_DEFAULT_PROCESS_TABLE_SIZE;
  process_table = (OS_malloc (OS_process_table_size * (sizeof (process_t))));
  {
    Tprocess process;
    for (process = 0; (process < OS_process_table_size); process += 1)
      OS_process_deallocate (process);
  }
#ifdef USE_PROCESS_TABLE_LOCK
  InitializeCriticalSection (&process_table_lock);
#endif
  scheme_jc_status = process_jc_status_no_ctty;
  process_tick = 0;
  sync_tick = 0;
}

static void
lock_process_table (void)
{
  GRAB_PROCESS_TABLE ();
  transaction_record_action (tat_always, lock_process_table_1, 0);
}

static void
lock_process_table_1 (void * ignore)
{
  RELEASE_PROCESS_TABLE ();
}

Tprocess
NT_make_subprocess (const char * filename,
		    const char * command_line,
		    const char * environment,
		    const char * working_directory,
		    enum process_channel_type channel_in_type,
		    Tchannel channel_in,
		    enum process_channel_type channel_out_type,
		    Tchannel channel_out,
		    enum process_channel_type channel_err_type,
		    Tchannel channel_err,
		    int hide_windows_p)
{
  transaction_begin ();
  save_process_state ();
  transfer_stdio (STD_INPUT_HANDLE, channel_in, channel_in_type);
  transfer_stdio (STD_OUTPUT_HANDLE, channel_out, channel_out_type);
  transfer_stdio (STD_ERROR_HANDLE, channel_err, channel_err_type);
  {
    Tprocess child;
    SECURITY_ATTRIBUTES sap;
    SECURITY_DESCRIPTOR sdp;
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    /* Explicitly specify no security */
    STD_BOOL_API_CALL
      (InitializeSecurityDescriptor, ((&sdp), SECURITY_DESCRIPTOR_REVISION));
    STD_BOOL_API_CALL (SetSecurityDescriptorDacl, ((&sdp), TRUE, 0, FALSE));
    (sap . nLength) = (sizeof (sap));
    (sap . lpSecurityDescriptor) = (&sdp);
    (sap . bInheritHandle) = FALSE;

    memset ((&si), 0, (sizeof (si)));
    (si . cb) = (sizeof (si));
    (si . dwFlags) = STARTF_USESTDHANDLES;
    (si . hStdInput) = GetStdHandle (STD_INPUT_HANDLE);
    (si . hStdOutput) = GetStdHandle (STD_OUTPUT_HANDLE);
    (si . hStdError) = GetStdHandle (STD_ERROR_HANDLE);
    if (hide_windows_p)
      {
	(si . dwFlags) |= STARTF_USESHOWWINDOW;
	(si . wShowWindow) |= SW_HIDE;
      }

    lock_process_table ();
    child = (allocate_process ());
    STD_BOOL_API_CALL
      (CreateProcess,
       (((LPCTSTR) filename),
	((LPSTR) command_line),
	(&sap),
	0,
	TRUE,
	(CREATE_DEFAULT_ERROR_MODE | CREATE_NEW_CONSOLE),
	((LPVOID) environment),
	((LPCTSTR) working_directory),
	(&si),
	(& (PROCESS_HANDLES (child)))));
    (PROCESS_RAW_STATUS (child)) = process_status_running;
    (PROCESS_RAW_REASON (child)) = STILL_ACTIVE;
    (PROCESS_TICK (child)) = process_tick;
    PROCESS_STATUS_SYNC (child);
    transaction_commit ();
    return (child);
  }
}

typedef struct
{
  HANDLE std_in;
  HANDLE std_out;
  HANDLE std_err;
} process_state_t;

static void
save_process_state (void)
{
  process_state_t * state = (dstack_alloc (sizeof (process_state_t)));
  (state -> std_in) = (GetStdHandle (STD_INPUT_HANDLE));
  (state -> std_out) = (GetStdHandle (STD_OUTPUT_HANDLE));
  (state -> std_err) = (GetStdHandle (STD_ERROR_HANDLE));
  transaction_record_action (tat_always, restore_process_state, state);
}

static void
restore_process_state (void * env)
{
  process_state_t * state = env;

  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  SetStdHandle (STD_INPUT_HANDLE, (state -> std_in));
  SetStdHandle (STD_OUTPUT_HANDLE, (state -> std_out));
  SetStdHandle (STD_ERROR_HANDLE, (state -> std_err));
}

static void
transfer_stdio (DWORD target, Tchannel channel, enum process_channel_type type)
{
  if (type == process_channel_type_explicit)
    {
      STD_BOOL_API_CALL
	(SetStdHandle, (target, (copy_handle (CHANNEL_HANDLE (channel)))));
    }
  else
    {
      STD_BOOL_API_CALL
	(SetStdHandle, (target, (copy_handle (GetStdHandle (target)))));
    }
}

static HANDLE
copy_handle (HANDLE handle)
{
  HANDLE parent = (GetCurrentProcess ());
  HANDLE copy;
  STD_BOOL_API_CALL
    (DuplicateHandle,
     (parent, handle, parent, (&copy), 0, TRUE, DUPLICATE_SAME_ACCESS));
  return (copy);
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
  OS_process_deallocate (process);
}

struct fcc_info
{
  DWORD pid;
  HWND hwnd;
};

static HWND
find_child_console (DWORD pid)
{
  struct fcc_info fi;
  (fi . pid) = pid;
  (fi . hwnd) = INVALID_HANDLE_VALUE;
  STD_BOOL_API_CALL (EnumWindows, (find_child_console_1, ((LPARAM) (&fi))));
  return (fi . hwnd);
}

static BOOL CALLBACK
find_child_console_1 (HWND hwnd, LPARAM lfi)
{
  struct fcc_info * fi = ((struct fcc_info *) lfi);
  DWORD pid;

  (void) GetWindowThreadProcessId (hwnd, (&pid));
  if (pid == (fi -> pid))
    {
      char window_class [32];
      unsigned int n
	= (GetClassName (hwnd, window_class, (sizeof (window_class))));
      const char * console_class
	= ((NT_windows_type == wintype_95) ? "tty" : "ConsoleWindowClass");
      if ((n == ((strlen (console_class)) + 1))
	  && ((strcmp (window_class, console_class)) == 0))
	{
	  (fi -> hwnd) = hwnd;
	  return (FALSE);
	}
    }
  /* keep looking */
  return (TRUE);
}

void
OS_process_deallocate (Tprocess process)
{
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
  lock_process_table ();
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
  lock_process_table ();
  {
    int result = (process_tick != sync_tick);
    if (result)
      sync_tick = process_tick;
    transaction_commit ();
    return (result);
  }
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
  error_unimplemented_primitive ();
}

void
OS_process_kill (Tprocess process)
{
  if (NT_windows_type == wintype_nt)
    {
      HWND hwnd = (find_child_console (PROCESS_ID (process)));
      if (hwnd != INVALID_HANDLE_VALUE)
	{
	  PostMessage (hwnd, WM_CLOSE, 0, 0);
	  return;
	}
    }
  if (!TerminateProcess ((PROCESS_HANDLE (process)),
			 TERMINATE_PROCESS_EXIT_CODE))
    {
      DWORD code = (GetLastError ());
      if (code != ERROR_ACCESS_DENIED)
	NT_error_api_call ((GetLastError ()), apicall_TerminateProcess);
    }
}

void
OS_process_stop (Tprocess process)
{
  error_unimplemented_primitive ();
}

void
OS_process_interrupt (Tprocess process)
{
  HWND hwnd;
  BYTE control_scan_code;
  BYTE vk_break_code;
  BYTE break_scan_code;
  HWND foreground_window;

  hwnd = (find_child_console (PROCESS_ID (process)));
  if (hwnd == INVALID_HANDLE_VALUE)
    return;
  control_scan_code = ((BYTE) (MapVirtualKey (VK_CONTROL, 0)));
  vk_break_code = VK_CANCEL;
  break_scan_code = ((BYTE) (MapVirtualKey (vk_break_code, 0)));
  if (break_scan_code == 0)
    {
      /* Fake Ctrl-C if we can't manage Ctrl-Break. */
      vk_break_code = 'C';
      break_scan_code = ((BYTE) (MapVirtualKey (vk_break_code, 0)));
    }
  foreground_window = (GetForegroundWindow ());
  if (SetForegroundWindow (hwnd))
    {
      /* Generate keystrokes as if user had typed Ctrl-Break or Ctrl-C.  */
      keybd_event (VK_CONTROL, control_scan_code, 0, 0);
      keybd_event (vk_break_code, break_scan_code, 0, 0);
      keybd_event (vk_break_code, break_scan_code, KEYEVENTF_KEYUP, 0);
      keybd_event (VK_CONTROL, control_scan_code, KEYEVENTF_KEYUP, 0);
      if (foreground_window)
	SetForegroundWindow (foreground_window);
    }
}

void
OS_process_quit (Tprocess process)
{
  error_unimplemented_primitive ();
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
  error_unimplemented_primitive ();
}

#ifndef WIN32_WAIT_INTERVAL
#define WIN32_WAIT_INTERVAL 50
#endif

void
OS_process_wait (Tprocess process)
{
  while (1)
    {
      test_process_status_change (process);
      if (((PROCESS_RAW_STATUS (process)) != process_status_running)
	  || (pending_interrupts_p ()))
	break;
      Sleep (WIN32_WAIT_INTERVAL);
    }
}

int
OS_process_any_status_change (void)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    test_process_status_change (process);
  return (process_tick != sync_tick);
}

static void
test_process_status_change (Tprocess process)
{
  if ((PROCESS_RAW_STATUS (process)) == process_status_running)
    switch (WaitForSingleObject ((PROCESS_HANDLE (process)), 0))
      {
      case WAIT_TIMEOUT:
	break;
      case WAIT_OBJECT_0:
	{
	  DWORD exit_code;
	  STD_BOOL_API_CALL
	    (GetExitCodeProcess, ((PROCESS_HANDLE (process)), (&exit_code)));
	  GRAB_PROCESS_TABLE ();
	  (PROCESS_RAW_STATUS (process))
	    = ((exit_code < 0x40000000)
	       ? process_status_exited
	       : process_status_signalled);
	  (PROCESS_RAW_REASON (process)) = exit_code;
	  (PROCESS_TICK (process)) = (++process_tick);
	  RELEASE_PROCESS_TABLE ();
	}
	break;
      case WAIT_ABANDONED:
	NT_error_api_call (ERROR_INVALID_HANDLE, apicall_WaitForSingleObject);
	break;
      case WAIT_FAILED:
	NT_error_api_call ((GetLastError ()), apicall_WaitForSingleObject);
	break;
      }
}

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
  error_unimplemented_primitive ();
  return (NO_PROCESS);
}
