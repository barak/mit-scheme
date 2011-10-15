/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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
Tprocess OS_process_table_size;
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

#undef TRACE_NTPROC
#ifdef TRACE_NTPROC
FILE * trace_file;
#ifndef TRACE_NTPROC_FILENAME
#define TRACE_NTPROC_FILENAME "nttrace.out"
#endif
#endif

static void lock_process_table (void);
static void lock_process_table_1 (void *);
static HANDLE stdio_handle (DWORD, Tchannel, enum process_channel_type);
static HANDLE copy_handle (HANDLE);
static Tprocess allocate_process (void);
static void allocate_process_abort (void *);
static HWND find_child_console (DWORD);
static BOOL CALLBACK find_child_console_1 (HWND, LPARAM);
static void process_wait_1 (Tprocess, DWORD);
static void process_death (Tprocess);

void
NT_initialize_processes (void)
{
#ifdef TRACE_NTPROC
  trace_file = (fopen (TRACE_NTPROC_FILENAME, "w"));
#endif
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
  Tprocess child;
  SECURITY_ATTRIBUTES sap;
  SECURITY_DESCRIPTOR sdp;
  STARTUPINFO si;

  transaction_begin ();

  /* Explicitly specify no security */
  STD_BOOL_API_CALL
    (InitializeSecurityDescriptor, ((&sdp), SECURITY_DESCRIPTOR_REVISION));
  STD_BOOL_API_CALL (SetSecurityDescriptorDacl, ((&sdp), TRUE, 0, FALSE));
  (sap . nLength) = (sizeof (sap));
  (sap . lpSecurityDescriptor) = (&sdp);
  (sap . bInheritHandle) = FALSE;

  memset ((&si), 0, (sizeof (si)));
  (si . cb) = (sizeof (si));
  /* Specify the handles to be used by the child process.  */
  (si . dwFlags) = STARTF_USESTDHANDLES;
  (si . hStdInput)
    = (stdio_handle (STD_INPUT_HANDLE, channel_in, channel_in_type));
  (si . hStdOutput)
    = (stdio_handle (STD_OUTPUT_HANDLE, channel_out, channel_out_type));
  (si . hStdError)
    = (stdio_handle (STD_ERROR_HANDLE, channel_err, channel_err_type));
  /* If requested, hide the top-level window of the child process.  */
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
  STD_BOOL_API_CALL (CloseHandle, (si . hStdInput));
  STD_BOOL_API_CALL (CloseHandle, (si . hStdOutput));
  STD_BOOL_API_CALL (CloseHandle, (si . hStdError));
  transaction_commit ();
  STD_BOOL_API_CALL (CloseHandle, ((PROCESS_HANDLES (child)) . hThread));
  ((PROCESS_HANDLES (child)) . hThread) = INVALID_HANDLE_VALUE;
#ifdef TRACE_NTPROC
  fprintf (trace_file, "Subprocess created: id=0x%x\n", (PROCESS_ID (child)));
  fflush (trace_file);
#endif
  return (child);
}

static HANDLE
stdio_handle (DWORD target, Tchannel channel, enum process_channel_type type)
{
  return
    (copy_handle ((type == process_channel_type_explicit)
		  ? (CHANNEL_HANDLE (channel))
		  : (GetStdHandle (target))));
}

static HANDLE
copy_handle (HANDLE handle)
{
  HANDLE parent = (GetCurrentProcess ());
  HANDLE copy;
  STD_BOOL_API_CALL
    (DuplicateHandle,
     (parent, handle, parent, (&copy), 0, TRUE, DUPLICATE_SAME_ACCESS));
  NT_handle_close_on_abort (copy);
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
  EnumWindows (find_child_console_1, ((LPARAM) (&fi)));
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
      if ((n == (strlen (console_class)))
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
#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_status_sync: id=0x%x\n",
	   (PROCESS_ID (process)));
  fflush (trace_file);
#endif
  transaction_begin ();
  lock_process_table ();
  {
    int result = ((PROCESS_TICK (process)) != (PROCESS_SYNC_TICK (process)));
    if (result)
      {
#ifdef TRACE_NTPROC
	fprintf (trace_file, "(status=0x%x raw_status=0x%x)\n",
		 (PROCESS_STATUS (process)),
		 (PROCESS_RAW_STATUS (process)));
	fflush (trace_file);
#endif
	PROCESS_STATUS_SYNC (process);
      }
    transaction_commit ();
    return (result);
  }
}

int
OS_process_status_sync_all (void)
{
#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_status_sync_all\n");
  fflush (trace_file);
#endif
  transaction_begin ();
  lock_process_table ();
  {
    int result = (process_tick != sync_tick);
    if (result)
      {
#ifdef TRACE_NTPROC
	fprintf (trace_file, "(status change)\n");
	fflush (trace_file);
#endif
	sync_tick = process_tick;
      }
    transaction_commit ();
    return (result);
  }
}

enum process_status
OS_process_status (Tprocess process)
{
#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_status: id=0x%x status=0x%x\n",
	   (PROCESS_ID (process)),
	   (PROCESS_STATUS (process)));
  fflush (trace_file);
#endif
  return (PROCESS_STATUS (process));
}

unsigned short
OS_process_reason (Tprocess process)
{
  return ((unsigned short) (PROCESS_REASON (process)));
}

void
OS_process_send_signal (Tprocess process, int sig)
{
  error_unimplemented_primitive ();
}

void
OS_process_kill (Tprocess process)
{
#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_kill: id=0x%x\n", (PROCESS_ID (process)));
  fflush (trace_file);
#endif
  if (NT_windows_type == wintype_nt)
    {
      HWND hwnd = (find_child_console (PROCESS_ID (process)));
      if (hwnd != INVALID_HANDLE_VALUE)
	{
	  PostMessage (hwnd, WM_CLOSE, 0, 0);
	  return;
	}
    }
#ifdef TRACE_NTPROC
  fprintf (trace_file, "(using TerminateProcess)\n");
  fflush (trace_file);
#endif
  if (!TerminateProcess ((PROCESS_HANDLE (process)),
			 TERMINATE_PROCESS_EXIT_CODE))
    {
      DWORD code = (GetLastError ());
      if (code != ERROR_ACCESS_DENIED)
	NT_error_api_call ((GetLastError ()), apicall_TerminateProcess);
#ifdef TRACE_NTPROC
      fprintf (trace_file, "(ERROR_ACCESS_DENIED)\n");
      fflush (trace_file);
#endif
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
  /* BYTE keyboard_state [256]; */
  HWND foreground_window;

#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_interrupt: id=0x%x\n",
	   (PROCESS_ID (process)));
  fflush (trace_file);
#endif
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
  /* STD_BOOL_API_CALL (GetKeyboardState, (keyboard_state)); */
  foreground_window = (GetForegroundWindow ());
  if (SetForegroundWindow (hwnd))
    {
      /* Generate keystrokes as if user had typed Ctrl-Break or Ctrl-C.  */
      keybd_event (VK_CONTROL, control_scan_code, 0, 0);
      keybd_event (vk_break_code, break_scan_code, 0, 0);
      keybd_event (vk_break_code, break_scan_code, KEYEVENTF_KEYUP, 0);
      keybd_event (VK_CONTROL, control_scan_code, KEYEVENTF_KEYUP, 0);
      if (foreground_window)
	(void) SetForegroundWindow (foreground_window);
    }
  /* STD_BOOL_API_CALL (SetKeyboardState, (keyboard_state)); */
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
  process_wait_1 (process, 0);
  while (1)
    {
      if (((PROCESS_RAW_STATUS (process)) != process_status_running)
	  || (pending_interrupts_p ()))
	break;
      process_wait_1 (process, WIN32_WAIT_INTERVAL);
    }
}

static void
process_wait_1 (Tprocess process, DWORD interval)
{
#ifdef TRACE_NTPROC
  fprintf (trace_file, "process_wait_1: id=0x%x raw_status=0x%x\n",
	   (PROCESS_ID (process)),
	   (PROCESS_RAW_STATUS (process)));
  fflush (trace_file);
#endif
  if ((PROCESS_RAW_STATUS (process)) == process_status_running)
    {
      DWORD code
	= (MsgWaitForMultipleObjects (1,
				      (& (PROCESS_HANDLE (process))),
				      FALSE,
				      interval,
				      QS_ALLINPUT));
#ifdef TRACE_NTPROC
      fprintf (trace_file, "(wait result = 0x%x)\n", code);
      fflush (trace_file);
#endif
      switch (code)
	{
	case WAIT_OBJECT_0:
	  process_death (process);
	  break;
	case WAIT_FAILED:
	  NT_error_api_call ((GetLastError ()),
			     apicall_MsgWaitForMultipleObjects);
	  break;
	}
    }
}

int
OS_process_any_status_change (void)
{
  Tprocess process;
#ifdef TRACE_NTPROC
  fprintf (trace_file, "OS_process_any_status_change\n");
  fflush (trace_file);
#endif
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_RAW_STATUS (process)) == process_status_running)
      switch (WaitForSingleObject ((PROCESS_HANDLE (process)), 0))
	{
	case WAIT_OBJECT_0:
	  process_death (process);
	  break;
	case WAIT_FAILED:
	  NT_error_api_call ((GetLastError ()),
			     apicall_MsgWaitForMultipleObjects);
	  break;
	}
#ifdef TRACE_NTPROC
  if (process_tick != sync_tick)
    {
      fprintf (trace_file, "(status change)\n");
      fflush (trace_file);
    }
#endif
  return (process_tick != sync_tick);
}

static void
process_death (Tprocess process)
{
  DWORD exit_code;
#ifdef TRACE_NTPROC
  fprintf (trace_file, "process_death: id=0x%x\n", (PROCESS_ID (process)));
  fflush (trace_file);
#endif
  STD_BOOL_API_CALL
    (GetExitCodeProcess, ((PROCESS_HANDLE (process)), (&exit_code)));
#ifdef TRACE_NTPROC
  fprintf (trace_file, "(exit_code = 0x%x)\n", exit_code);
  fflush (trace_file);
#endif
  GRAB_PROCESS_TABLE ();
  (PROCESS_RAW_STATUS (process))
    = ((exit_code == STATUS_CONTROL_C_EXIT)
       ? process_status_signalled
       : process_status_exited);
  (PROCESS_RAW_REASON (process)) = exit_code;
  (PROCESS_TICK (process)) = (++process_tick);
  STD_BOOL_API_CALL (CloseHandle, (PROCESS_HANDLE (process)));
  (PROCESS_HANDLE (process)) = INVALID_HANDLE_VALUE;
  RELEASE_PROCESS_TABLE ();
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
