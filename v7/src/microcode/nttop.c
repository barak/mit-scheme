/* -*-C-*-

$Id: nttop.c,v 1.33 2003/02/14 18:48:12 cph Exp $

Copyright 1993-2000 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#define SCM_NTTOP_C
#include "nt.h"
#include "nttop.h"
#include "osctty.h"
#include "prims.h"
#include "errors.h"
#include "option.h"
#include "outf.h"
#include "ntscmlib.h"

extern void execute_reload_cleanups (void);

extern void NT_gui_init (void);
extern void NT_initialize_channels (void);
extern void NT_initialize_directory_reader (void);
extern void NT_initialize_processes (void);
extern void NT_initialize_signals (void);
extern void NT_initialize_sockets (void);
extern void NT_initialize_traps (void);
extern void NT_initialize_tty (void);

extern void NT_reset_channels (void);

extern void NT_restore_channels (void);
extern void NT_restore_signals (void);
extern void NT_restore_traps (void);

/* reset_interruptable_extent */

extern CONST char * OS_Name;
extern CONST char * OS_Variant;

static const char * w32_error_message (DWORD);
static int syserr_to_unix_error_code (enum syserr_names);
static void initialize_locks (void);
static DWORD syserr_to_win32_error_code (enum syserr_names);
static enum syserr_names win32_error_code_to_syserr (DWORD);

BOOL
win32_under_win32s_p ()
{
  OSVERSIONINFO info;
  (info.dwOSVersionInfoSize) = (sizeof (info));
  (void) GetVersionEx (&info);
  return ((info.dwPlatformId) == VER_PLATFORM_WIN32s);
}

WIN32_SYSTEM_UTILITIES win32_system_utilities;

HINSTANCE win32_system_utilities_dll = 0;

void
NT_initialize_win32_system_utilities ()
{
#ifdef USE_SCHEME_DLL
  char * dll_name = win32_under_win32s_p() ? "SCHEME31.DLL" : "SCHEME32.DLL";
  char * entry_name = "install_win32_system_utilities";
  FARPROC install;

  win32_system_utilities_dll = LoadLibrary (dll_name);
  if (win32_system_utilities_dll == NULL) {
    outf_fatal ("MIT/GNU Scheme is unable to find or load %s\n"
		"This essential MIT/GNU Scheme file should be in the\n"
		"same directory as SCHEME.EXE",
		dll_name);
    outf_flush_fatal();
    abort ();
  }

  install = GetProcAddress (win32_system_utilities_dll, entry_name);
  if (install==NULL) {
    outf_fatal ("Something is wrong with %s\n"
		"It does not have an entry called \"%s\".",
		dll_name, entry_name);
    outf_flush_fatal ();
    abort ();
  }

  install (&win32_system_utilities);
#else
  extern void FAR WINAPI install_win32_system_utilities
    (WIN32_SYSTEM_UTILITIES *);
  install_win32_system_utilities (&win32_system_utilities);
#endif
}

static int interactive;

int
OS_under_emacs_p (void)
{
  return (option_emacs_subprocess);
}

enum windows_type NT_windows_type;

static int
empty_string_p (const char * s)
{
  const char * p = s;
  while (1)
    switch (*p++)
      {
      case '\0':
	return (1);
      case ' ':
      case '\t':
	break;
      default:
	return (0);
      }
}

void
OS_initialize (void)
{
  interactive = 1;

  initialize_locks ();
  NT_gui_init ();
  NT_initialize_channels ();
  NT_initialize_tty ();
  NT_initialize_signals ();
  NT_initialize_traps ();
  NT_initialize_directory_reader ();
  NT_initialize_processes ();
  NT_initialize_sockets ();

  OS_Name = "NT";
  {
    OSVERSIONINFO info;
    char * p = (malloc (250));

    (info.dwOSVersionInfoSize) = (sizeof (info));
    (void) GetVersionEx (&info);

    if ((info.dwPlatformId) == VER_PLATFORM_WIN32_NT)
      {
	sprintf (p, "Microsoft Windows NT %u.%u (Build %u",
		 (info.dwMajorVersion),
		 (info.dwMinorVersion),
		 (info.dwBuildNumber));
	if (!empty_string_p (info.szCSDVersion))
	  {
	    strcat (p, "; ");
	    strcat (p, (info.szCSDVersion));
	  }
	strcat (p, ")");
	NT_windows_type = wintype_nt;
      }
    else if ((info.dwPlatformId) == VER_PLATFORM_WIN32_WINDOWS)
      {
	sprintf (p, "Microsoft Windows %s (Build %u",
		 (((info.dwMinorVersion) == 0)
		  ? "95"
		  : ((info.dwMinorVersion) == 10)
		  ? "98"
		  : "9?"),
		 (LOWORD (info.dwBuildNumber)));
	if (!empty_string_p (info.szCSDVersion))
	  {
	    strcat (p, "; ");
	    strcat (p, (info.szCSDVersion));
	  }
	strcat (p, ")");
	NT_windows_type = wintype_95;
      }
    else
      {
	sprintf (p, "Microsoft Windows %u.%u",
		 (info.dwMajorVersion),
		 (info.dwMinorVersion));
	NT_windows_type = wintype_31;
      }
    strcat (p, " IA-32\n");
    OS_Variant = p;
  }
}

void
OS_announcement (void)
{
#if 0
  /* To make our compiler vendors happy. */
  outf_console
    ("Copyright (c) 1993-1995 Massachusetts Institute of Technology\n");
  outf_console ("\n");
#endif
}

void
OS_reset (void)
{
  /*
    There should really be a reset for each initialize above,
    but the rest seem innocuous.
   */

  NT_reset_channels ();
  execute_reload_cleanups ();
  return;
}

void
OS_quit (int code, int abnormal_p)
{
  outf_console ("\nScheme has terminated abnormally!\n");
  OS_restore_external_state ();
  return;
}

/* Memory Allocation */

static LPVOID NT_heap_base;
static DWORD NT_heap_size;
static DWORD NT_heap_error;
#define SCHEME_ADDR_LIMIT 0x04000000

void
NT_preallocate_heap (void)
{
  MEMORY_BASIC_INFORMATION largest;
  DWORD scan = 0;
  (largest.RegionSize) = 0;
  while (scan < SCHEME_ADDR_LIMIT)
    {
      MEMORY_BASIC_INFORMATION info;
      (void) VirtualQuery (((LPCVOID) scan), (&info), (sizeof (info)));
      if ((info.State) == MEM_FREE)
	{
	  DWORD end = (scan + (info.RegionSize));
	  if (end > SCHEME_ADDR_LIMIT)
	    (info.RegionSize) -= (end - SCHEME_ADDR_LIMIT);
	  if ((info.RegionSize) > (largest.RegionSize))
	    largest = info;
	}
      scan += (info.RegionSize);
    }
  NT_heap_size = (largest.RegionSize);
  NT_heap_base
    = (VirtualAlloc ((largest.BaseAddress),
		     NT_heap_size,
		     MEM_RESERVE,
		     PAGE_READWRITE));
  if (NT_heap_base == 0)
    NT_heap_error = (GetLastError ());
}

char *
NT_allocate_heap (unsigned long size, unsigned long * handle)
{
  if (NT_heap_base == 0)
    {
      SYSTEM_INFO info;
      LPVOID start = 0;
      LPVOID base;
      GetSystemInfo (&info);
      while (1)
	{
	  start = (((char *) start) + (info . dwPageSize));
	  base
	    = (VirtualAlloc (start,
			     size,
			     (MEM_RESERVE | MEM_COMMIT),
			     PAGE_READWRITE));
	  if (base != 0)
	    break;
	}
      (* handle) = size;
      return ((char *) base);
    }
  else
    {
      DWORD size2 = ((size <= NT_heap_size) ? size : NT_heap_size);
      LPVOID base
	= (VirtualAlloc (NT_heap_base,
			 size2,
			 MEM_COMMIT,
			 PAGE_READWRITE));
      (* handle) = size2;
      return ((char *) base);
    }
}

void
NT_release_heap (char * area, unsigned long handle)
{
  VirtualFree (((LPVOID) area),
	       ((DWORD) handle),
	       ((DWORD) MEM_DECOMMIT));
  if (NT_heap_base == 0)
    VirtualFree (((LPVOID) area),
		 ((DWORD) 0),
		 ((DWORD) MEM_RELEASE));
  else
    VirtualFree (NT_heap_base,
		 ((DWORD) 0),
		 ((DWORD) MEM_RELEASE));
}

#ifndef EAGAIN
#define EAGAIN ERRNO_NONBLOCK
#endif

enum syserr_names
OS_error_code_to_syserr (int code)
{
  switch (code)
  {
    case E2BIG:		return (syserr_arg_list_too_long);
    case EACCES:	return (syserr_permission_denied);
    case EAGAIN:	return (syserr_resource_temporarily_unavailable);
    case EBADF:		return (syserr_bad_file_descriptor);
    case EDEADLOCK:	return (syserr_resource_deadlock_avoided);
    case EDOM:		return (syserr_domain_error);
    case EEXIST:	return (syserr_unix_file_exists);
    case EINTR:		return (syserr_interrupted_function_call);
    case EINVAL:	return (syserr_invalid_argument);
    case EMFILE:	return (syserr_unix_too_many_open_files);
    case ENOENT:	return (syserr_no_such_file_or_directory);
    case ENOEXEC:	return (syserr_exec_format_error);
    case ENOMEM:	return (syserr_not_enough_space);
    case ENOTDIR:	return (syserr_not_a_directory);
    case ERANGE:	return (syserr_result_too_large);
    default:		return (syserr_unknown);
  }
}

const char *
OS_error_code_to_message (unsigned int syserr)
{
  static const char * last_message = 0;
  if (last_message != 0)
    {
      OS_free ((void *) last_message);
      last_message = 0;
    }
  if (syserr < FIRST_UNIX_ERROR_CODE)
    {
      last_message
	= (w32_error_message
	   (syserr_to_win32_error_code ((enum syserr_names) syserr)));
      /* The runtime system is assuming that the messages have no period,
	 and adding its own.  */
      if (last_message != 0)
	{
	  unsigned int length = (strlen (last_message));
	  if ((length > 0) && ((last_message [length - 1]) == '.'))
	    (((char *) last_message) [length - 1]) = '\0';
	}
      return (last_message);
    }
  else
    {
#ifdef CL386
      extern char * sys_errlist [];
      extern int sys_nerr;
#endif
      int code = (syserr_to_unix_error_code ((enum syserr_names) syserr));
      return (((code > 0) && (code <= sys_nerr)) ? (sys_errlist [code]) : 0);
    }
}

void
NT_error_api_call (DWORD code, enum syscall_names name)
{
  error_in_system_call ((win32_error_code_to_syserr (code)), name);
}

void
NT_error_unix_call (int code, enum syscall_names name)
{
  error_in_system_call ((OS_error_code_to_syserr (code)), name);
}

static const char *
w32_error_message (DWORD rc)
{
  char * buffer;
  char * result;
  DWORD length
    = (FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER
		       | FORMAT_MESSAGE_FROM_SYSTEM),
		      0,
		      rc,
		      (MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT)),
		      ((LPTSTR) (&buffer)),
		      0,
		      0));
  
  if (length == 0)
    return (0);
  /* Assumes that we're using ANSI rather than Unicode characters.  */
  result = (OS_malloc ((strlen (buffer)) + 1));
  {
    char * from = buffer;
    char * to = result;
    while (1)
      {
	char c = (*from++);
	(*to++) = c;
	if (c == '\0')
	  break;
      }
    if (((to - 3) >= result)
	&& ((to[-3]) == '\r')
	&& ((to[-2]) == '\n'))
      (to[-3]) = '\0';
  }
  (void) LocalFree (buffer);
  return (result);
}

static int
syserr_to_unix_error_code (enum syserr_names syserr)
{
  switch (syserr)
    {
    case syserr_arg_list_too_long:			return (E2BIG);
    case syserr_bad_file_descriptor:			return (EBADF);
    case syserr_domain_error:				return (EDOM);
    case syserr_exec_format_error:			return (ENOEXEC);
    case syserr_unix_file_exists:			return (EEXIST);
    case syserr_interrupted_function_call:		return (EINTR);
    case syserr_invalid_argument:			return (EINVAL);
    case syserr_no_such_file_or_directory:		return (ENOENT);
    case syserr_not_a_directory:			return (ENOTDIR);
    case syserr_not_enough_space:			return (ENOMEM);
    case syserr_permission_denied:			return (EACCES);
    case syserr_resource_deadlock_avoided:		return (EDEADLOCK);
    case syserr_resource_temporarily_unavailable:	return (EAGAIN);
    case syserr_result_too_large:			return (ERANGE);
    case syserr_unix_too_many_open_files:		return (EMFILE);
    default: 						return (0);
    }
}

void
NT_prim_check_errno (enum syscall_names name)
{
  if (errno != EINTR)
    NT_error_unix_call (errno, name);
  deliver_pending_interrupts ();
  return;
}

void
OS_restore_external_state (void)
{
  NT_restore_traps ();
  NT_restore_signals ();
  NT_restore_channels ();
  return;
}

void 
bcopy (const char * s1, char * s2, int n)
{
  while (n-- > 0)
    *s2++ = *s1++;
  return;
}

void *
OS_malloc (unsigned int size)
{
  void * result = (malloc (size));
  if (result == 0)
    NT_error_unix_call (ENOMEM, syscall_malloc);
  return (result);
}

void *
OS_realloc (void * ptr, unsigned int size)
{
  void * result = (realloc (ptr, size));
  if (result == 0)
    NT_error_unix_call (ENOMEM, syscall_realloc);
  return (result);
}

void
OS_free (void * ptr)
{
  free (ptr);
}

void
OS_syscall_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syscall_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syscall_names_table);
}

void
OS_syserr_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syserr_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syserr_names_table);
}

static CRITICAL_SECTION interrupt_registers_lock;

static void
initialize_locks (void)
{
  InitializeCriticalSection (&interrupt_registers_lock);
}

void
OS_grab_interrupt_registers (void)
{
  EnterCriticalSection (&interrupt_registers_lock);
}

void
OS_release_interrupt_registers (void)
{
  LeaveCriticalSection (&interrupt_registers_lock);
}

/* Machine-generated procedure, do not edit: */
static DWORD
syserr_to_win32_error_code (enum syserr_names syserr)
{
  switch (syserr)
    {
    case syserr_invalid_function: return (ERROR_INVALID_FUNCTION);
    case syserr_file_not_found: return (ERROR_FILE_NOT_FOUND);
    case syserr_path_not_found: return (ERROR_PATH_NOT_FOUND);
    case syserr_too_many_open_files: return (ERROR_TOO_MANY_OPEN_FILES);
    case syserr_access_denied: return (ERROR_ACCESS_DENIED);
    case syserr_invalid_handle: return (ERROR_INVALID_HANDLE);
    case syserr_arena_trashed: return (ERROR_ARENA_TRASHED);
    case syserr_not_enough_memory: return (ERROR_NOT_ENOUGH_MEMORY);
    case syserr_invalid_block: return (ERROR_INVALID_BLOCK);
    case syserr_bad_environment: return (ERROR_BAD_ENVIRONMENT);
    case syserr_bad_format: return (ERROR_BAD_FORMAT);
    case syserr_invalid_access: return (ERROR_INVALID_ACCESS);
    case syserr_invalid_data: return (ERROR_INVALID_DATA);
    case syserr_outofmemory: return (ERROR_OUTOFMEMORY);
    case syserr_invalid_drive: return (ERROR_INVALID_DRIVE);
    case syserr_current_directory: return (ERROR_CURRENT_DIRECTORY);
    case syserr_not_same_device: return (ERROR_NOT_SAME_DEVICE);
    case syserr_no_more_files: return (ERROR_NO_MORE_FILES);
    case syserr_write_protect: return (ERROR_WRITE_PROTECT);
    case syserr_bad_unit: return (ERROR_BAD_UNIT);
    case syserr_not_ready: return (ERROR_NOT_READY);
    case syserr_bad_command: return (ERROR_BAD_COMMAND);
    case syserr_crc: return (ERROR_CRC);
    case syserr_bad_length: return (ERROR_BAD_LENGTH);
    case syserr_seek: return (ERROR_SEEK);
    case syserr_not_dos_disk: return (ERROR_NOT_DOS_DISK);
    case syserr_sector_not_found: return (ERROR_SECTOR_NOT_FOUND);
    case syserr_out_of_paper: return (ERROR_OUT_OF_PAPER);
    case syserr_write_fault: return (ERROR_WRITE_FAULT);
    case syserr_read_fault: return (ERROR_READ_FAULT);
    case syserr_gen_failure: return (ERROR_GEN_FAILURE);
    case syserr_sharing_violation: return (ERROR_SHARING_VIOLATION);
    case syserr_lock_violation: return (ERROR_LOCK_VIOLATION);
    case syserr_wrong_disk: return (ERROR_WRONG_DISK);
    case syserr_sharing_buffer_exceeded: return (ERROR_SHARING_BUFFER_EXCEEDED);
    case syserr_handle_eof: return (ERROR_HANDLE_EOF);
    case syserr_handle_disk_full: return (ERROR_HANDLE_DISK_FULL);
    case syserr_not_supported: return (ERROR_NOT_SUPPORTED);
    case syserr_rem_not_list: return (ERROR_REM_NOT_LIST);
    case syserr_dup_name: return (ERROR_DUP_NAME);
    case syserr_bad_netpath: return (ERROR_BAD_NETPATH);
    case syserr_network_busy: return (ERROR_NETWORK_BUSY);
    case syserr_dev_not_exist: return (ERROR_DEV_NOT_EXIST);
    case syserr_too_many_cmds: return (ERROR_TOO_MANY_CMDS);
    case syserr_adap_hdw_err: return (ERROR_ADAP_HDW_ERR);
    case syserr_bad_net_resp: return (ERROR_BAD_NET_RESP);
    case syserr_unexp_net_err: return (ERROR_UNEXP_NET_ERR);
    case syserr_bad_rem_adap: return (ERROR_BAD_REM_ADAP);
    case syserr_printq_full: return (ERROR_PRINTQ_FULL);
    case syserr_no_spool_space: return (ERROR_NO_SPOOL_SPACE);
    case syserr_print_cancelled: return (ERROR_PRINT_CANCELLED);
    case syserr_netname_deleted: return (ERROR_NETNAME_DELETED);
    case syserr_network_access_denied: return (ERROR_NETWORK_ACCESS_DENIED);
    case syserr_bad_dev_type: return (ERROR_BAD_DEV_TYPE);
    case syserr_bad_net_name: return (ERROR_BAD_NET_NAME);
    case syserr_too_many_names: return (ERROR_TOO_MANY_NAMES);
    case syserr_too_many_sess: return (ERROR_TOO_MANY_SESS);
    case syserr_sharing_paused: return (ERROR_SHARING_PAUSED);
    case syserr_req_not_accep: return (ERROR_REQ_NOT_ACCEP);
    case syserr_redir_paused: return (ERROR_REDIR_PAUSED);
    case syserr_file_exists: return (ERROR_FILE_EXISTS);
    case syserr_cannot_make: return (ERROR_CANNOT_MAKE);
    case syserr_fail_i24: return (ERROR_FAIL_I24);
    case syserr_out_of_structures: return (ERROR_OUT_OF_STRUCTURES);
    case syserr_already_assigned: return (ERROR_ALREADY_ASSIGNED);
    case syserr_invalid_password: return (ERROR_INVALID_PASSWORD);
    case syserr_invalid_parameter: return (ERROR_INVALID_PARAMETER);
    case syserr_net_write_fault: return (ERROR_NET_WRITE_FAULT);
    case syserr_no_proc_slots: return (ERROR_NO_PROC_SLOTS);
    case syserr_too_many_semaphores: return (ERROR_TOO_MANY_SEMAPHORES);
    case syserr_excl_sem_already_owned: return (ERROR_EXCL_SEM_ALREADY_OWNED);
    case syserr_sem_is_set: return (ERROR_SEM_IS_SET);
    case syserr_too_many_sem_requests: return (ERROR_TOO_MANY_SEM_REQUESTS);
    case syserr_invalid_at_interrupt_time: return (ERROR_INVALID_AT_INTERRUPT_TIME);
    case syserr_sem_owner_died: return (ERROR_SEM_OWNER_DIED);
    case syserr_sem_user_limit: return (ERROR_SEM_USER_LIMIT);
    case syserr_disk_change: return (ERROR_DISK_CHANGE);
    case syserr_drive_locked: return (ERROR_DRIVE_LOCKED);
    case syserr_broken_pipe: return (ERROR_BROKEN_PIPE);
    case syserr_open_failed: return (ERROR_OPEN_FAILED);
    case syserr_buffer_overflow: return (ERROR_BUFFER_OVERFLOW);
    case syserr_disk_full: return (ERROR_DISK_FULL);
    case syserr_no_more_search_handles: return (ERROR_NO_MORE_SEARCH_HANDLES);
    case syserr_invalid_target_handle: return (ERROR_INVALID_TARGET_HANDLE);
    case syserr_invalid_category: return (ERROR_INVALID_CATEGORY);
    case syserr_invalid_verify_switch: return (ERROR_INVALID_VERIFY_SWITCH);
    case syserr_bad_driver_level: return (ERROR_BAD_DRIVER_LEVEL);
    case syserr_call_not_implemented: return (ERROR_CALL_NOT_IMPLEMENTED);
    case syserr_sem_timeout: return (ERROR_SEM_TIMEOUT);
    case syserr_insufficient_buffer: return (ERROR_INSUFFICIENT_BUFFER);
    case syserr_invalid_name: return (ERROR_INVALID_NAME);
    case syserr_invalid_level: return (ERROR_INVALID_LEVEL);
    case syserr_no_volume_label: return (ERROR_NO_VOLUME_LABEL);
    case syserr_mod_not_found: return (ERROR_MOD_NOT_FOUND);
    case syserr_proc_not_found: return (ERROR_PROC_NOT_FOUND);
    case syserr_wait_no_children: return (ERROR_WAIT_NO_CHILDREN);
    case syserr_child_not_complete: return (ERROR_CHILD_NOT_COMPLETE);
    case syserr_direct_access_handle: return (ERROR_DIRECT_ACCESS_HANDLE);
    case syserr_negative_seek: return (ERROR_NEGATIVE_SEEK);
    case syserr_seek_on_device: return (ERROR_SEEK_ON_DEVICE);
    case syserr_is_join_target: return (ERROR_IS_JOIN_TARGET);
    case syserr_is_joined: return (ERROR_IS_JOINED);
    case syserr_is_substed: return (ERROR_IS_SUBSTED);
    case syserr_not_joined: return (ERROR_NOT_JOINED);
    case syserr_not_substed: return (ERROR_NOT_SUBSTED);
    case syserr_join_to_join: return (ERROR_JOIN_TO_JOIN);
    case syserr_subst_to_subst: return (ERROR_SUBST_TO_SUBST);
    case syserr_join_to_subst: return (ERROR_JOIN_TO_SUBST);
    case syserr_subst_to_join: return (ERROR_SUBST_TO_JOIN);
    case syserr_busy_drive: return (ERROR_BUSY_DRIVE);
    case syserr_same_drive: return (ERROR_SAME_DRIVE);
    case syserr_dir_not_root: return (ERROR_DIR_NOT_ROOT);
    case syserr_dir_not_empty: return (ERROR_DIR_NOT_EMPTY);
    case syserr_is_subst_path: return (ERROR_IS_SUBST_PATH);
    case syserr_is_join_path: return (ERROR_IS_JOIN_PATH);
    case syserr_path_busy: return (ERROR_PATH_BUSY);
    case syserr_is_subst_target: return (ERROR_IS_SUBST_TARGET);
    case syserr_system_trace: return (ERROR_SYSTEM_TRACE);
    case syserr_invalid_event_count: return (ERROR_INVALID_EVENT_COUNT);
    case syserr_too_many_muxwaiters: return (ERROR_TOO_MANY_MUXWAITERS);
    case syserr_invalid_list_format: return (ERROR_INVALID_LIST_FORMAT);
    case syserr_label_too_long: return (ERROR_LABEL_TOO_LONG);
    case syserr_too_many_tcbs: return (ERROR_TOO_MANY_TCBS);
    case syserr_signal_refused: return (ERROR_SIGNAL_REFUSED);
    case syserr_discarded: return (ERROR_DISCARDED);
    case syserr_not_locked: return (ERROR_NOT_LOCKED);
    case syserr_bad_threadid_addr: return (ERROR_BAD_THREADID_ADDR);
    case syserr_bad_arguments: return (ERROR_BAD_ARGUMENTS);
    case syserr_bad_pathname: return (ERROR_BAD_PATHNAME);
    case syserr_signal_pending: return (ERROR_SIGNAL_PENDING);
    case syserr_max_thrds_reached: return (ERROR_MAX_THRDS_REACHED);
    case syserr_lock_failed: return (ERROR_LOCK_FAILED);
    case syserr_busy: return (ERROR_BUSY);
    case syserr_cancel_violation: return (ERROR_CANCEL_VIOLATION);
    case syserr_atomic_locks_not_supported: return (ERROR_ATOMIC_LOCKS_NOT_SUPPORTED);
    case syserr_invalid_segment_number: return (ERROR_INVALID_SEGMENT_NUMBER);
    case syserr_invalid_ordinal: return (ERROR_INVALID_ORDINAL);
    case syserr_already_exists: return (ERROR_ALREADY_EXISTS);
    case syserr_invalid_flag_number: return (ERROR_INVALID_FLAG_NUMBER);
    case syserr_sem_not_found: return (ERROR_SEM_NOT_FOUND);
    case syserr_invalid_starting_codeseg: return (ERROR_INVALID_STARTING_CODESEG);
    case syserr_invalid_stackseg: return (ERROR_INVALID_STACKSEG);
    case syserr_invalid_moduletype: return (ERROR_INVALID_MODULETYPE);
    case syserr_invalid_exe_signature: return (ERROR_INVALID_EXE_SIGNATURE);
    case syserr_exe_marked_invalid: return (ERROR_EXE_MARKED_INVALID);
    case syserr_bad_exe_format: return (ERROR_BAD_EXE_FORMAT);
    case syserr_iterated_data_exceeds_64k: return (ERROR_ITERATED_DATA_EXCEEDS_64k);
    case syserr_invalid_minallocsize: return (ERROR_INVALID_MINALLOCSIZE);
    case syserr_dynlink_from_invalid_ring: return (ERROR_DYNLINK_FROM_INVALID_RING);
    case syserr_iopl_not_enabled: return (ERROR_IOPL_NOT_ENABLED);
    case syserr_invalid_segdpl: return (ERROR_INVALID_SEGDPL);
    case syserr_autodataseg_exceeds_64k: return (ERROR_AUTODATASEG_EXCEEDS_64k);
    case syserr_ring2seg_must_be_movable: return (ERROR_RING2SEG_MUST_BE_MOVABLE);
    case syserr_reloc_chain_xeeds_seglim: return (ERROR_RELOC_CHAIN_XEEDS_SEGLIM);
    case syserr_infloop_in_reloc_chain: return (ERROR_INFLOOP_IN_RELOC_CHAIN);
    case syserr_envvar_not_found: return (ERROR_ENVVAR_NOT_FOUND);
    case syserr_no_signal_sent: return (ERROR_NO_SIGNAL_SENT);
    case syserr_filename_exced_range: return (ERROR_FILENAME_EXCED_RANGE);
    case syserr_ring2_stack_in_use: return (ERROR_RING2_STACK_IN_USE);
    case syserr_meta_expansion_too_long: return (ERROR_META_EXPANSION_TOO_LONG);
    case syserr_invalid_signal_number: return (ERROR_INVALID_SIGNAL_NUMBER);
    case syserr_thread_1_inactive: return (ERROR_THREAD_1_INACTIVE);
    case syserr_locked: return (ERROR_LOCKED);
    case syserr_too_many_modules: return (ERROR_TOO_MANY_MODULES);
    case syserr_nesting_not_allowed: return (ERROR_NESTING_NOT_ALLOWED);
    case syserr_bad_pipe: return (ERROR_BAD_PIPE);
    case syserr_pipe_busy: return (ERROR_PIPE_BUSY);
    case syserr_no_data: return (ERROR_NO_DATA);
    case syserr_pipe_not_connected: return (ERROR_PIPE_NOT_CONNECTED);
    case syserr_more_data: return (ERROR_MORE_DATA);
    case syserr_vc_disconnected: return (ERROR_VC_DISCONNECTED);
    case syserr_invalid_ea_name: return (ERROR_INVALID_EA_NAME);
    case syserr_ea_list_inconsistent: return (ERROR_EA_LIST_INCONSISTENT);
    case syserr_no_more_items: return (ERROR_NO_MORE_ITEMS);
    case syserr_cannot_copy: return (ERROR_CANNOT_COPY);
    case syserr_directory: return (ERROR_DIRECTORY);
    case syserr_eas_didnt_fit: return (ERROR_EAS_DIDNT_FIT);
    case syserr_ea_file_corrupt: return (ERROR_EA_FILE_CORRUPT);
    case syserr_ea_table_full: return (ERROR_EA_TABLE_FULL);
    case syserr_invalid_ea_handle: return (ERROR_INVALID_EA_HANDLE);
    case syserr_eas_not_supported: return (ERROR_EAS_NOT_SUPPORTED);
    case syserr_not_owner: return (ERROR_NOT_OWNER);
    case syserr_too_many_posts: return (ERROR_TOO_MANY_POSTS);
    case syserr_partial_copy: return (ERROR_PARTIAL_COPY);
    case syserr_mr_mid_not_found: return (ERROR_MR_MID_NOT_FOUND);
    case syserr_invalid_address: return (ERROR_INVALID_ADDRESS);
    case syserr_arithmetic_overflow: return (ERROR_ARITHMETIC_OVERFLOW);
    case syserr_pipe_connected: return (ERROR_PIPE_CONNECTED);
    case syserr_pipe_listening: return (ERROR_PIPE_LISTENING);
    case syserr_ea_access_denied: return (ERROR_EA_ACCESS_DENIED);
    case syserr_operation_aborted: return (ERROR_OPERATION_ABORTED);
    case syserr_io_incomplete: return (ERROR_IO_INCOMPLETE);
    case syserr_io_pending: return (ERROR_IO_PENDING);
    case syserr_noaccess: return (ERROR_NOACCESS);
    case syserr_swaperror: return (ERROR_SWAPERROR);
    case syserr_stack_overflow: return (ERROR_STACK_OVERFLOW);
    case syserr_invalid_message: return (ERROR_INVALID_MESSAGE);
    case syserr_can_not_complete: return (ERROR_CAN_NOT_COMPLETE);
    case syserr_invalid_flags: return (ERROR_INVALID_FLAGS);
    case syserr_unrecognized_volume: return (ERROR_UNRECOGNIZED_VOLUME);
    case syserr_file_invalid: return (ERROR_FILE_INVALID);
    case syserr_fullscreen_mode: return (ERROR_FULLSCREEN_MODE);
    case syserr_no_token: return (ERROR_NO_TOKEN);
    case syserr_baddb: return (ERROR_BADDB);
    case syserr_badkey: return (ERROR_BADKEY);
    case syserr_cantopen: return (ERROR_CANTOPEN);
    case syserr_cantread: return (ERROR_CANTREAD);
    case syserr_cantwrite: return (ERROR_CANTWRITE);
    case syserr_registry_recovered: return (ERROR_REGISTRY_RECOVERED);
    case syserr_registry_corrupt: return (ERROR_REGISTRY_CORRUPT);
    case syserr_registry_io_failed: return (ERROR_REGISTRY_IO_FAILED);
    case syserr_not_registry_file: return (ERROR_NOT_REGISTRY_FILE);
    case syserr_key_deleted: return (ERROR_KEY_DELETED);
    case syserr_no_log_space: return (ERROR_NO_LOG_SPACE);
    case syserr_key_has_children: return (ERROR_KEY_HAS_CHILDREN);
    case syserr_child_must_be_volatile: return (ERROR_CHILD_MUST_BE_VOLATILE);
    case syserr_notify_enum_dir: return (ERROR_NOTIFY_ENUM_DIR);
    case syserr_dependent_services_running: return (ERROR_DEPENDENT_SERVICES_RUNNING);
    case syserr_invalid_service_control: return (ERROR_INVALID_SERVICE_CONTROL);
    case syserr_service_request_timeout: return (ERROR_SERVICE_REQUEST_TIMEOUT);
    case syserr_service_no_thread: return (ERROR_SERVICE_NO_THREAD);
    case syserr_service_database_locked: return (ERROR_SERVICE_DATABASE_LOCKED);
    case syserr_service_already_running: return (ERROR_SERVICE_ALREADY_RUNNING);
    case syserr_invalid_service_account: return (ERROR_INVALID_SERVICE_ACCOUNT);
    case syserr_service_disabled: return (ERROR_SERVICE_DISABLED);
    case syserr_circular_dependency: return (ERROR_CIRCULAR_DEPENDENCY);
    case syserr_service_does_not_exist: return (ERROR_SERVICE_DOES_NOT_EXIST);
    case syserr_service_cannot_accept_ctrl: return (ERROR_SERVICE_CANNOT_ACCEPT_CTRL);
    case syserr_service_not_active: return (ERROR_SERVICE_NOT_ACTIVE);
    case syserr_failed_service_controller_connect: return (ERROR_FAILED_SERVICE_CONTROLLER_CONNECT);
    case syserr_exception_in_service: return (ERROR_EXCEPTION_IN_SERVICE);
    case syserr_database_does_not_exist: return (ERROR_DATABASE_DOES_NOT_EXIST);
    case syserr_service_specific_error: return (ERROR_SERVICE_SPECIFIC_ERROR);
    case syserr_process_aborted: return (ERROR_PROCESS_ABORTED);
    case syserr_service_dependency_fail: return (ERROR_SERVICE_DEPENDENCY_FAIL);
    case syserr_service_logon_failed: return (ERROR_SERVICE_LOGON_FAILED);
    case syserr_service_start_hang: return (ERROR_SERVICE_START_HANG);
    case syserr_invalid_service_lock: return (ERROR_INVALID_SERVICE_LOCK);
    case syserr_service_marked_for_delete: return (ERROR_SERVICE_MARKED_FOR_DELETE);
    case syserr_service_exists: return (ERROR_SERVICE_EXISTS);
    case syserr_already_running_lkg: return (ERROR_ALREADY_RUNNING_LKG);
    case syserr_service_dependency_deleted: return (ERROR_SERVICE_DEPENDENCY_DELETED);
    case syserr_boot_already_accepted: return (ERROR_BOOT_ALREADY_ACCEPTED);
    case syserr_service_never_started: return (ERROR_SERVICE_NEVER_STARTED);
    case syserr_duplicate_service_name: return (ERROR_DUPLICATE_SERVICE_NAME);
    case syserr_end_of_media: return (ERROR_END_OF_MEDIA);
    case syserr_filemark_detected: return (ERROR_FILEMARK_DETECTED);
    case syserr_beginning_of_media: return (ERROR_BEGINNING_OF_MEDIA);
    case syserr_setmark_detected: return (ERROR_SETMARK_DETECTED);
    case syserr_no_data_detected: return (ERROR_NO_DATA_DETECTED);
    case syserr_partition_failure: return (ERROR_PARTITION_FAILURE);
    case syserr_invalid_block_length: return (ERROR_INVALID_BLOCK_LENGTH);
    case syserr_device_not_partitioned: return (ERROR_DEVICE_NOT_PARTITIONED);
    case syserr_unable_to_lock_media: return (ERROR_UNABLE_TO_LOCK_MEDIA);
    case syserr_unable_to_unload_media: return (ERROR_UNABLE_TO_UNLOAD_MEDIA);
    case syserr_media_changed: return (ERROR_MEDIA_CHANGED);
    case syserr_bus_reset: return (ERROR_BUS_RESET);
    case syserr_no_media_in_drive: return (ERROR_NO_MEDIA_IN_DRIVE);
    case syserr_no_unicode_translation: return (ERROR_NO_UNICODE_TRANSLATION);
    case syserr_dll_init_failed: return (ERROR_DLL_INIT_FAILED);
    case syserr_shutdown_in_progress: return (ERROR_SHUTDOWN_IN_PROGRESS);
    case syserr_no_shutdown_in_progress: return (ERROR_NO_SHUTDOWN_IN_PROGRESS);
    case syserr_io_device: return (ERROR_IO_DEVICE);
    case syserr_serial_no_device: return (ERROR_SERIAL_NO_DEVICE);
    case syserr_irq_busy: return (ERROR_IRQ_BUSY);
    case syserr_more_writes: return (ERROR_MORE_WRITES);
    case syserr_counter_timeout: return (ERROR_COUNTER_TIMEOUT);
    case syserr_floppy_id_mark_not_found: return (ERROR_FLOPPY_ID_MARK_NOT_FOUND);
    case syserr_floppy_wrong_cylinder: return (ERROR_FLOPPY_WRONG_CYLINDER);
    case syserr_floppy_unknown_error: return (ERROR_FLOPPY_UNKNOWN_ERROR);
    case syserr_floppy_bad_registers: return (ERROR_FLOPPY_BAD_REGISTERS);
    case syserr_disk_recalibrate_failed: return (ERROR_DISK_RECALIBRATE_FAILED);
    case syserr_disk_operation_failed: return (ERROR_DISK_OPERATION_FAILED);
    case syserr_disk_reset_failed: return (ERROR_DISK_RESET_FAILED);
    case syserr_eom_overflow: return (ERROR_EOM_OVERFLOW);
    case syserr_not_enough_server_memory: return (ERROR_NOT_ENOUGH_SERVER_MEMORY);
    case syserr_possible_deadlock: return (ERROR_POSSIBLE_DEADLOCK);
    case syserr_mapped_alignment: return (ERROR_MAPPED_ALIGNMENT);
    case syserr_set_power_state_vetoed: return (ERROR_SET_POWER_STATE_VETOED);
    case syserr_set_power_state_failed: return (ERROR_SET_POWER_STATE_FAILED);
    case syserr_old_win_version: return (ERROR_OLD_WIN_VERSION);
    case syserr_app_wrong_os: return (ERROR_APP_WRONG_OS);
    case syserr_single_instance_app: return (ERROR_SINGLE_INSTANCE_APP);
    case syserr_rmode_app: return (ERROR_RMODE_APP);
    case syserr_invalid_dll: return (ERROR_INVALID_DLL);
    case syserr_no_association: return (ERROR_NO_ASSOCIATION);
    case syserr_dde_fail: return (ERROR_DDE_FAIL);
    case syserr_dll_not_found: return (ERROR_DLL_NOT_FOUND);
    case syserr_bad_username: return (ERROR_BAD_USERNAME);
    case syserr_not_connected: return (ERROR_NOT_CONNECTED);
    case syserr_open_files: return (ERROR_OPEN_FILES);
    case syserr_active_connections: return (ERROR_ACTIVE_CONNECTIONS);
    case syserr_device_in_use: return (ERROR_DEVICE_IN_USE);
    case syserr_bad_device: return (ERROR_BAD_DEVICE);
    case syserr_connection_unavail: return (ERROR_CONNECTION_UNAVAIL);
    case syserr_device_already_remembered: return (ERROR_DEVICE_ALREADY_REMEMBERED);
    case syserr_no_net_or_bad_path: return (ERROR_NO_NET_OR_BAD_PATH);
    case syserr_bad_provider: return (ERROR_BAD_PROVIDER);
    case syserr_cannot_open_profile: return (ERROR_CANNOT_OPEN_PROFILE);
    case syserr_bad_profile: return (ERROR_BAD_PROFILE);
    case syserr_not_container: return (ERROR_NOT_CONTAINER);
    case syserr_extended_error: return (ERROR_EXTENDED_ERROR);
    case syserr_invalid_groupname: return (ERROR_INVALID_GROUPNAME);
    case syserr_invalid_computername: return (ERROR_INVALID_COMPUTERNAME);
    case syserr_invalid_eventname: return (ERROR_INVALID_EVENTNAME);
    case syserr_invalid_domainname: return (ERROR_INVALID_DOMAINNAME);
    case syserr_invalid_servicename: return (ERROR_INVALID_SERVICENAME);
    case syserr_invalid_netname: return (ERROR_INVALID_NETNAME);
    case syserr_invalid_sharename: return (ERROR_INVALID_SHARENAME);
    case syserr_invalid_passwordname: return (ERROR_INVALID_PASSWORDNAME);
    case syserr_invalid_messagename: return (ERROR_INVALID_MESSAGENAME);
    case syserr_invalid_messagedest: return (ERROR_INVALID_MESSAGEDEST);
    case syserr_session_credential_conflict: return (ERROR_SESSION_CREDENTIAL_CONFLICT);
    case syserr_remote_session_limit_exceeded: return (ERROR_REMOTE_SESSION_LIMIT_EXCEEDED);
    case syserr_dup_domainname: return (ERROR_DUP_DOMAINNAME);
    case syserr_no_network: return (ERROR_NO_NETWORK);
    case syserr_cancelled: return (ERROR_CANCELLED);
    case syserr_user_mapped_file: return (ERROR_USER_MAPPED_FILE);
    case syserr_connection_refused: return (ERROR_CONNECTION_REFUSED);
    case syserr_graceful_disconnect: return (ERROR_GRACEFUL_DISCONNECT);
    case syserr_address_already_associated: return (ERROR_ADDRESS_ALREADY_ASSOCIATED);
    case syserr_address_not_associated: return (ERROR_ADDRESS_NOT_ASSOCIATED);
    case syserr_connection_invalid: return (ERROR_CONNECTION_INVALID);
    case syserr_connection_active: return (ERROR_CONNECTION_ACTIVE);
    case syserr_network_unreachable: return (ERROR_NETWORK_UNREACHABLE);
    case syserr_host_unreachable: return (ERROR_HOST_UNREACHABLE);
    case syserr_protocol_unreachable: return (ERROR_PROTOCOL_UNREACHABLE);
    case syserr_port_unreachable: return (ERROR_PORT_UNREACHABLE);
    case syserr_request_aborted: return (ERROR_REQUEST_ABORTED);
    case syserr_connection_aborted: return (ERROR_CONNECTION_ABORTED);
    case syserr_retry: return (ERROR_RETRY);
    case syserr_connection_count_limit: return (ERROR_CONNECTION_COUNT_LIMIT);
    case syserr_login_time_restriction: return (ERROR_LOGIN_TIME_RESTRICTION);
    case syserr_login_wksta_restriction: return (ERROR_LOGIN_WKSTA_RESTRICTION);
    case syserr_incorrect_address: return (ERROR_INCORRECT_ADDRESS);
    case syserr_already_registered: return (ERROR_ALREADY_REGISTERED);
    case syserr_service_not_found: return (ERROR_SERVICE_NOT_FOUND);
    case syserr_not_authenticated: return (ERROR_NOT_AUTHENTICATED);
    case syserr_not_logged_on: return (ERROR_NOT_LOGGED_ON);
    case syserr_continue: return (ERROR_CONTINUE);
    case syserr_already_initialized: return (ERROR_ALREADY_INITIALIZED);
    case syserr_no_more_devices: return (ERROR_NO_MORE_DEVICES);
    case syserr_not_all_assigned: return (ERROR_NOT_ALL_ASSIGNED);
    case syserr_some_not_mapped: return (ERROR_SOME_NOT_MAPPED);
    case syserr_no_quotas_for_account: return (ERROR_NO_QUOTAS_FOR_ACCOUNT);
    case syserr_local_user_session_key: return (ERROR_LOCAL_USER_SESSION_KEY);
    case syserr_null_lm_password: return (ERROR_NULL_LM_PASSWORD);
    case syserr_unknown_revision: return (ERROR_UNKNOWN_REVISION);
    case syserr_revision_mismatch: return (ERROR_REVISION_MISMATCH);
    case syserr_invalid_owner: return (ERROR_INVALID_OWNER);
    case syserr_invalid_primary_group: return (ERROR_INVALID_PRIMARY_GROUP);
    case syserr_no_impersonation_token: return (ERROR_NO_IMPERSONATION_TOKEN);
    case syserr_cant_disable_mandatory: return (ERROR_CANT_DISABLE_MANDATORY);
    case syserr_no_logon_servers: return (ERROR_NO_LOGON_SERVERS);
    case syserr_no_such_logon_session: return (ERROR_NO_SUCH_LOGON_SESSION);
    case syserr_no_such_privilege: return (ERROR_NO_SUCH_PRIVILEGE);
    case syserr_privilege_not_held: return (ERROR_PRIVILEGE_NOT_HELD);
    case syserr_invalid_account_name: return (ERROR_INVALID_ACCOUNT_NAME);
    case syserr_user_exists: return (ERROR_USER_EXISTS);
    case syserr_no_such_user: return (ERROR_NO_SUCH_USER);
    case syserr_group_exists: return (ERROR_GROUP_EXISTS);
    case syserr_no_such_group: return (ERROR_NO_SUCH_GROUP);
    case syserr_member_in_group: return (ERROR_MEMBER_IN_GROUP);
    case syserr_member_not_in_group: return (ERROR_MEMBER_NOT_IN_GROUP);
    case syserr_last_admin: return (ERROR_LAST_ADMIN);
    case syserr_wrong_password: return (ERROR_WRONG_PASSWORD);
    case syserr_ill_formed_password: return (ERROR_ILL_FORMED_PASSWORD);
    case syserr_password_restriction: return (ERROR_PASSWORD_RESTRICTION);
    case syserr_logon_failure: return (ERROR_LOGON_FAILURE);
    case syserr_account_restriction: return (ERROR_ACCOUNT_RESTRICTION);
    case syserr_invalid_logon_hours: return (ERROR_INVALID_LOGON_HOURS);
    case syserr_invalid_workstation: return (ERROR_INVALID_WORKSTATION);
    case syserr_password_expired: return (ERROR_PASSWORD_EXPIRED);
    case syserr_account_disabled: return (ERROR_ACCOUNT_DISABLED);
    case syserr_none_mapped: return (ERROR_NONE_MAPPED);
    case syserr_too_many_luids_requested: return (ERROR_TOO_MANY_LUIDS_REQUESTED);
    case syserr_luids_exhausted: return (ERROR_LUIDS_EXHAUSTED);
    case syserr_invalid_sub_authority: return (ERROR_INVALID_SUB_AUTHORITY);
    case syserr_invalid_acl: return (ERROR_INVALID_ACL);
    case syserr_invalid_sid: return (ERROR_INVALID_SID);
    case syserr_invalid_security_descr: return (ERROR_INVALID_SECURITY_DESCR);
    case syserr_bad_inheritance_acl: return (ERROR_BAD_INHERITANCE_ACL);
    case syserr_server_disabled: return (ERROR_SERVER_DISABLED);
    case syserr_server_not_disabled: return (ERROR_SERVER_NOT_DISABLED);
    case syserr_invalid_id_authority: return (ERROR_INVALID_ID_AUTHORITY);
    case syserr_allotted_space_exceeded: return (ERROR_ALLOTTED_SPACE_EXCEEDED);
    case syserr_invalid_group_attributes: return (ERROR_INVALID_GROUP_ATTRIBUTES);
    case syserr_bad_impersonation_level: return (ERROR_BAD_IMPERSONATION_LEVEL);
    case syserr_cant_open_anonymous: return (ERROR_CANT_OPEN_ANONYMOUS);
    case syserr_bad_validation_class: return (ERROR_BAD_VALIDATION_CLASS);
    case syserr_bad_token_type: return (ERROR_BAD_TOKEN_TYPE);
    case syserr_no_security_on_object: return (ERROR_NO_SECURITY_ON_OBJECT);
    case syserr_cant_access_domain_info: return (ERROR_CANT_ACCESS_DOMAIN_INFO);
    case syserr_invalid_server_state: return (ERROR_INVALID_SERVER_STATE);
    case syserr_invalid_domain_state: return (ERROR_INVALID_DOMAIN_STATE);
    case syserr_invalid_domain_role: return (ERROR_INVALID_DOMAIN_ROLE);
    case syserr_no_such_domain: return (ERROR_NO_SUCH_DOMAIN);
    case syserr_domain_exists: return (ERROR_DOMAIN_EXISTS);
    case syserr_domain_limit_exceeded: return (ERROR_DOMAIN_LIMIT_EXCEEDED);
    case syserr_internal_db_corruption: return (ERROR_INTERNAL_DB_CORRUPTION);
    case syserr_internal_error: return (ERROR_INTERNAL_ERROR);
    case syserr_generic_not_mapped: return (ERROR_GENERIC_NOT_MAPPED);
    case syserr_bad_descriptor_format: return (ERROR_BAD_DESCRIPTOR_FORMAT);
    case syserr_not_logon_process: return (ERROR_NOT_LOGON_PROCESS);
    case syserr_logon_session_exists: return (ERROR_LOGON_SESSION_EXISTS);
    case syserr_no_such_package: return (ERROR_NO_SUCH_PACKAGE);
    case syserr_bad_logon_session_state: return (ERROR_BAD_LOGON_SESSION_STATE);
    case syserr_logon_session_collision: return (ERROR_LOGON_SESSION_COLLISION);
    case syserr_invalid_logon_type: return (ERROR_INVALID_LOGON_TYPE);
    case syserr_cannot_impersonate: return (ERROR_CANNOT_IMPERSONATE);
    case syserr_rxact_invalid_state: return (ERROR_RXACT_INVALID_STATE);
    case syserr_rxact_commit_failure: return (ERROR_RXACT_COMMIT_FAILURE);
    case syserr_special_account: return (ERROR_SPECIAL_ACCOUNT);
    case syserr_special_group: return (ERROR_SPECIAL_GROUP);
    case syserr_special_user: return (ERROR_SPECIAL_USER);
    case syserr_members_primary_group: return (ERROR_MEMBERS_PRIMARY_GROUP);
    case syserr_token_already_in_use: return (ERROR_TOKEN_ALREADY_IN_USE);
    case syserr_no_such_alias: return (ERROR_NO_SUCH_ALIAS);
    case syserr_member_not_in_alias: return (ERROR_MEMBER_NOT_IN_ALIAS);
    case syserr_member_in_alias: return (ERROR_MEMBER_IN_ALIAS);
    case syserr_alias_exists: return (ERROR_ALIAS_EXISTS);
    case syserr_logon_not_granted: return (ERROR_LOGON_NOT_GRANTED);
    case syserr_too_many_secrets: return (ERROR_TOO_MANY_SECRETS);
    case syserr_secret_too_long: return (ERROR_SECRET_TOO_LONG);
    case syserr_internal_db_error: return (ERROR_INTERNAL_DB_ERROR);
    case syserr_too_many_context_ids: return (ERROR_TOO_MANY_CONTEXT_IDS);
    case syserr_logon_type_not_granted: return (ERROR_LOGON_TYPE_NOT_GRANTED);
    case syserr_nt_cross_encryption_required: return (ERROR_NT_CROSS_ENCRYPTION_REQUIRED);
    case syserr_no_such_member: return (ERROR_NO_SUCH_MEMBER);
    case syserr_invalid_member: return (ERROR_INVALID_MEMBER);
    case syserr_too_many_sids: return (ERROR_TOO_MANY_SIDS);
    case syserr_lm_cross_encryption_required: return (ERROR_LM_CROSS_ENCRYPTION_REQUIRED);
    case syserr_no_inheritance: return (ERROR_NO_INHERITANCE);
    case syserr_file_corrupt: return (ERROR_FILE_CORRUPT);
    case syserr_disk_corrupt: return (ERROR_DISK_CORRUPT);
    case syserr_no_user_session_key: return (ERROR_NO_USER_SESSION_KEY);
    case syserr_license_quota_exceeded: return (ERROR_LICENSE_QUOTA_EXCEEDED);
    case syserr_invalid_window_handle: return (ERROR_INVALID_WINDOW_HANDLE);
    case syserr_invalid_menu_handle: return (ERROR_INVALID_MENU_HANDLE);
    case syserr_invalid_cursor_handle: return (ERROR_INVALID_CURSOR_HANDLE);
    case syserr_invalid_accel_handle: return (ERROR_INVALID_ACCEL_HANDLE);
    case syserr_invalid_hook_handle: return (ERROR_INVALID_HOOK_HANDLE);
    case syserr_invalid_dwp_handle: return (ERROR_INVALID_DWP_HANDLE);
    case syserr_tlw_with_wschild: return (ERROR_TLW_WITH_WSCHILD);
    case syserr_cannot_find_wnd_class: return (ERROR_CANNOT_FIND_WND_CLASS);
    case syserr_window_of_other_thread: return (ERROR_WINDOW_OF_OTHER_THREAD);
    case syserr_hotkey_already_registered: return (ERROR_HOTKEY_ALREADY_REGISTERED);
    case syserr_class_already_exists: return (ERROR_CLASS_ALREADY_EXISTS);
    case syserr_class_does_not_exist: return (ERROR_CLASS_DOES_NOT_EXIST);
    case syserr_class_has_windows: return (ERROR_CLASS_HAS_WINDOWS);
    case syserr_invalid_index: return (ERROR_INVALID_INDEX);
    case syserr_invalid_icon_handle: return (ERROR_INVALID_ICON_HANDLE);
    case syserr_private_dialog_index: return (ERROR_PRIVATE_DIALOG_INDEX);
    case syserr_listbox_id_not_found: return (ERROR_LISTBOX_ID_NOT_FOUND);
    case syserr_no_wildcard_characters: return (ERROR_NO_WILDCARD_CHARACTERS);
    case syserr_clipboard_not_open: return (ERROR_CLIPBOARD_NOT_OPEN);
    case syserr_hotkey_not_registered: return (ERROR_HOTKEY_NOT_REGISTERED);
    case syserr_window_not_dialog: return (ERROR_WINDOW_NOT_DIALOG);
    case syserr_control_id_not_found: return (ERROR_CONTROL_ID_NOT_FOUND);
    case syserr_invalid_combobox_message: return (ERROR_INVALID_COMBOBOX_MESSAGE);
    case syserr_window_not_combobox: return (ERROR_WINDOW_NOT_COMBOBOX);
    case syserr_invalid_edit_height: return (ERROR_INVALID_EDIT_HEIGHT);
    case syserr_dc_not_found: return (ERROR_DC_NOT_FOUND);
    case syserr_invalid_hook_filter: return (ERROR_INVALID_HOOK_FILTER);
    case syserr_invalid_filter_proc: return (ERROR_INVALID_FILTER_PROC);
    case syserr_hook_needs_hmod: return (ERROR_HOOK_NEEDS_HMOD);
    case syserr_global_only_hook: return (ERROR_GLOBAL_ONLY_HOOK);
    case syserr_journal_hook_set: return (ERROR_JOURNAL_HOOK_SET);
    case syserr_hook_not_installed: return (ERROR_HOOK_NOT_INSTALLED);
    case syserr_invalid_lb_message: return (ERROR_INVALID_LB_MESSAGE);
    case syserr_setcount_on_bad_lb: return (ERROR_SETCOUNT_ON_BAD_LB);
    case syserr_lb_without_tabstops: return (ERROR_LB_WITHOUT_TABSTOPS);
    case syserr_destroy_object_of_other_thread: return (ERROR_DESTROY_OBJECT_OF_OTHER_THREAD);
    case syserr_child_window_menu: return (ERROR_CHILD_WINDOW_MENU);
    case syserr_no_system_menu: return (ERROR_NO_SYSTEM_MENU);
    case syserr_invalid_msgbox_style: return (ERROR_INVALID_MSGBOX_STYLE);
    case syserr_invalid_spi_value: return (ERROR_INVALID_SPI_VALUE);
    case syserr_screen_already_locked: return (ERROR_SCREEN_ALREADY_LOCKED);
    case syserr_hwnds_have_diff_parent: return (ERROR_HWNDS_HAVE_DIFF_PARENT);
    case syserr_not_child_window: return (ERROR_NOT_CHILD_WINDOW);
    case syserr_invalid_gw_command: return (ERROR_INVALID_GW_COMMAND);
    case syserr_invalid_thread_id: return (ERROR_INVALID_THREAD_ID);
    case syserr_non_mdichild_window: return (ERROR_NON_MDICHILD_WINDOW);
    case syserr_popup_already_active: return (ERROR_POPUP_ALREADY_ACTIVE);
    case syserr_no_scrollbars: return (ERROR_NO_SCROLLBARS);
    case syserr_invalid_scrollbar_range: return (ERROR_INVALID_SCROLLBAR_RANGE);
    case syserr_invalid_showwin_command: return (ERROR_INVALID_SHOWWIN_COMMAND);
    case syserr_no_system_resources: return (ERROR_NO_SYSTEM_RESOURCES);
    case syserr_nonpaged_system_resources: return (ERROR_NONPAGED_SYSTEM_RESOURCES);
    case syserr_paged_system_resources: return (ERROR_PAGED_SYSTEM_RESOURCES);
    case syserr_working_set_quota: return (ERROR_WORKING_SET_QUOTA);
    case syserr_pagefile_quota: return (ERROR_PAGEFILE_QUOTA);
    case syserr_commitment_limit: return (ERROR_COMMITMENT_LIMIT);
    case syserr_menu_item_not_found: return (ERROR_MENU_ITEM_NOT_FOUND);
    case syserr_eventlog_file_corrupt: return (ERROR_EVENTLOG_FILE_CORRUPT);
    case syserr_eventlog_cant_start: return (ERROR_EVENTLOG_CANT_START);
    case syserr_log_file_full: return (ERROR_LOG_FILE_FULL);
    case syserr_eventlog_file_changed: return (ERROR_EVENTLOG_FILE_CHANGED);
    case syserr_rpc_s_invalid_string_binding: return (RPC_S_INVALID_STRING_BINDING);
    case syserr_rpc_s_wrong_kind_of_binding: return (RPC_S_WRONG_KIND_OF_BINDING);
    case syserr_rpc_s_invalid_binding: return (RPC_S_INVALID_BINDING);
    case syserr_rpc_s_protseq_not_supported: return (RPC_S_PROTSEQ_NOT_SUPPORTED);
    case syserr_rpc_s_invalid_rpc_protseq: return (RPC_S_INVALID_RPC_PROTSEQ);
    case syserr_rpc_s_invalid_string_uuid: return (RPC_S_INVALID_STRING_UUID);
    case syserr_rpc_s_invalid_endpoint_format: return (RPC_S_INVALID_ENDPOINT_FORMAT);
    case syserr_rpc_s_invalid_net_addr: return (RPC_S_INVALID_NET_ADDR);
    case syserr_rpc_s_no_endpoint_found: return (RPC_S_NO_ENDPOINT_FOUND);
    case syserr_rpc_s_invalid_timeout: return (RPC_S_INVALID_TIMEOUT);
    case syserr_rpc_s_object_not_found: return (RPC_S_OBJECT_NOT_FOUND);
    case syserr_rpc_s_already_registered: return (RPC_S_ALREADY_REGISTERED);
    case syserr_rpc_s_type_already_registered: return (RPC_S_TYPE_ALREADY_REGISTERED);
    case syserr_rpc_s_already_listening: return (RPC_S_ALREADY_LISTENING);
    case syserr_rpc_s_no_protseqs_registered: return (RPC_S_NO_PROTSEQS_REGISTERED);
    case syserr_rpc_s_not_listening: return (RPC_S_NOT_LISTENING);
    case syserr_rpc_s_unknown_mgr_type: return (RPC_S_UNKNOWN_MGR_TYPE);
    case syserr_rpc_s_unknown_if: return (RPC_S_UNKNOWN_IF);
    case syserr_rpc_s_no_bindings: return (RPC_S_NO_BINDINGS);
    case syserr_rpc_s_no_protseqs: return (RPC_S_NO_PROTSEQS);
    case syserr_rpc_s_cant_create_endpoint: return (RPC_S_CANT_CREATE_ENDPOINT);
    case syserr_rpc_s_out_of_resources: return (RPC_S_OUT_OF_RESOURCES);
    case syserr_rpc_s_server_unavailable: return (RPC_S_SERVER_UNAVAILABLE);
    case syserr_rpc_s_server_too_busy: return (RPC_S_SERVER_TOO_BUSY);
    case syserr_rpc_s_invalid_network_options: return (RPC_S_INVALID_NETWORK_OPTIONS);
    case syserr_rpc_s_no_call_active: return (RPC_S_NO_CALL_ACTIVE);
    case syserr_rpc_s_call_failed: return (RPC_S_CALL_FAILED);
    case syserr_rpc_s_call_failed_dne: return (RPC_S_CALL_FAILED_DNE);
    case syserr_rpc_s_protocol_error: return (RPC_S_PROTOCOL_ERROR);
    case syserr_rpc_s_unsupported_trans_syn: return (RPC_S_UNSUPPORTED_TRANS_SYN);
    case syserr_rpc_s_unsupported_type: return (RPC_S_UNSUPPORTED_TYPE);
    case syserr_rpc_s_invalid_tag: return (RPC_S_INVALID_TAG);
    case syserr_rpc_s_invalid_bound: return (RPC_S_INVALID_BOUND);
    case syserr_rpc_s_no_entry_name: return (RPC_S_NO_ENTRY_NAME);
    case syserr_rpc_s_invalid_name_syntax: return (RPC_S_INVALID_NAME_SYNTAX);
    case syserr_rpc_s_unsupported_name_syntax: return (RPC_S_UNSUPPORTED_NAME_SYNTAX);
    case syserr_rpc_s_uuid_no_address: return (RPC_S_UUID_NO_ADDRESS);
    case syserr_rpc_s_duplicate_endpoint: return (RPC_S_DUPLICATE_ENDPOINT);
    case syserr_rpc_s_unknown_authn_type: return (RPC_S_UNKNOWN_AUTHN_TYPE);
    case syserr_rpc_s_max_calls_too_small: return (RPC_S_MAX_CALLS_TOO_SMALL);
    case syserr_rpc_s_string_too_long: return (RPC_S_STRING_TOO_LONG);
    case syserr_rpc_s_protseq_not_found: return (RPC_S_PROTSEQ_NOT_FOUND);
    case syserr_rpc_s_procnum_out_of_range: return (RPC_S_PROCNUM_OUT_OF_RANGE);
    case syserr_rpc_s_binding_has_no_auth: return (RPC_S_BINDING_HAS_NO_AUTH);
    case syserr_rpc_s_unknown_authn_service: return (RPC_S_UNKNOWN_AUTHN_SERVICE);
    case syserr_rpc_s_unknown_authn_level: return (RPC_S_UNKNOWN_AUTHN_LEVEL);
    case syserr_rpc_s_invalid_auth_identity: return (RPC_S_INVALID_AUTH_IDENTITY);
    case syserr_rpc_s_unknown_authz_service: return (RPC_S_UNKNOWN_AUTHZ_SERVICE);
    case syserr_ept_s_invalid_entry: return (EPT_S_INVALID_ENTRY);
    case syserr_ept_s_cant_perform_op: return (EPT_S_CANT_PERFORM_OP);
    case syserr_ept_s_not_registered: return (EPT_S_NOT_REGISTERED);
    case syserr_rpc_s_nothing_to_export: return (RPC_S_NOTHING_TO_EXPORT);
    case syserr_rpc_s_incomplete_name: return (RPC_S_INCOMPLETE_NAME);
    case syserr_rpc_s_invalid_vers_option: return (RPC_S_INVALID_VERS_OPTION);
    case syserr_rpc_s_no_more_members: return (RPC_S_NO_MORE_MEMBERS);
    case syserr_rpc_s_not_all_objs_unexported: return (RPC_S_NOT_ALL_OBJS_UNEXPORTED);
    case syserr_rpc_s_interface_not_found: return (RPC_S_INTERFACE_NOT_FOUND);
    case syserr_rpc_s_entry_already_exists: return (RPC_S_ENTRY_ALREADY_EXISTS);
    case syserr_rpc_s_entry_not_found: return (RPC_S_ENTRY_NOT_FOUND);
    case syserr_rpc_s_name_service_unavailable: return (RPC_S_NAME_SERVICE_UNAVAILABLE);
    case syserr_rpc_s_invalid_naf_id: return (RPC_S_INVALID_NAF_ID);
    case syserr_rpc_s_cannot_support: return (RPC_S_CANNOT_SUPPORT);
    case syserr_rpc_s_no_context_available: return (RPC_S_NO_CONTEXT_AVAILABLE);
    case syserr_rpc_s_internal_error: return (RPC_S_INTERNAL_ERROR);
    case syserr_rpc_s_zero_divide: return (RPC_S_ZERO_DIVIDE);
    case syserr_rpc_s_address_error: return (RPC_S_ADDRESS_ERROR);
    case syserr_rpc_s_fp_div_zero: return (RPC_S_FP_DIV_ZERO);
    case syserr_rpc_s_fp_underflow: return (RPC_S_FP_UNDERFLOW);
    case syserr_rpc_s_fp_overflow: return (RPC_S_FP_OVERFLOW);
    case syserr_rpc_x_no_more_entries: return (RPC_X_NO_MORE_ENTRIES);
    case syserr_rpc_x_ss_char_trans_open_fail: return (RPC_X_SS_CHAR_TRANS_OPEN_FAIL);
    case syserr_rpc_x_ss_char_trans_short_file: return (RPC_X_SS_CHAR_TRANS_SHORT_FILE);
    case syserr_rpc_x_ss_in_null_context: return (RPC_X_SS_IN_NULL_CONTEXT);
    case syserr_rpc_x_ss_context_damaged: return (RPC_X_SS_CONTEXT_DAMAGED);
    case syserr_rpc_x_ss_handles_mismatch: return (RPC_X_SS_HANDLES_MISMATCH);
    case syserr_rpc_x_ss_cannot_get_call_handle: return (RPC_X_SS_CANNOT_GET_CALL_HANDLE);
    case syserr_rpc_x_null_ref_pointer: return (RPC_X_NULL_REF_POINTER);
    case syserr_rpc_x_enum_value_out_of_range: return (RPC_X_ENUM_VALUE_OUT_OF_RANGE);
    case syserr_rpc_x_byte_count_too_small: return (RPC_X_BYTE_COUNT_TOO_SMALL);
    case syserr_rpc_x_bad_stub_data: return (RPC_X_BAD_STUB_DATA);
    case syserr_invalid_user_buffer: return (ERROR_INVALID_USER_BUFFER);
    case syserr_unrecognized_media: return (ERROR_UNRECOGNIZED_MEDIA);
    case syserr_no_trust_lsa_secret: return (ERROR_NO_TRUST_LSA_SECRET);
    case syserr_no_trust_sam_account: return (ERROR_NO_TRUST_SAM_ACCOUNT);
    case syserr_trusted_domain_failure: return (ERROR_TRUSTED_DOMAIN_FAILURE);
    case syserr_trusted_relationship_failure: return (ERROR_TRUSTED_RELATIONSHIP_FAILURE);
    case syserr_trust_failure: return (ERROR_TRUST_FAILURE);
    case syserr_rpc_s_call_in_progress: return (RPC_S_CALL_IN_PROGRESS);
    case syserr_netlogon_not_started: return (ERROR_NETLOGON_NOT_STARTED);
    case syserr_account_expired: return (ERROR_ACCOUNT_EXPIRED);
    case syserr_redirector_has_open_handles: return (ERROR_REDIRECTOR_HAS_OPEN_HANDLES);
    case syserr_printer_driver_already_installed: return (ERROR_PRINTER_DRIVER_ALREADY_INSTALLED);
    case syserr_unknown_port: return (ERROR_UNKNOWN_PORT);
    case syserr_unknown_printer_driver: return (ERROR_UNKNOWN_PRINTER_DRIVER);
    case syserr_unknown_printprocessor: return (ERROR_UNKNOWN_PRINTPROCESSOR);
    case syserr_invalid_separator_file: return (ERROR_INVALID_SEPARATOR_FILE);
    case syserr_invalid_priority: return (ERROR_INVALID_PRIORITY);
    case syserr_invalid_printer_name: return (ERROR_INVALID_PRINTER_NAME);
    case syserr_printer_already_exists: return (ERROR_PRINTER_ALREADY_EXISTS);
    case syserr_invalid_printer_command: return (ERROR_INVALID_PRINTER_COMMAND);
    case syserr_invalid_datatype: return (ERROR_INVALID_DATATYPE);
    case syserr_invalid_environment: return (ERROR_INVALID_ENVIRONMENT);
    case syserr_rpc_s_no_more_bindings: return (RPC_S_NO_MORE_BINDINGS);
    case syserr_nologon_interdomain_trust_account: return (ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT);
    case syserr_nologon_workstation_trust_account: return (ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT);
    case syserr_nologon_server_trust_account: return (ERROR_NOLOGON_SERVER_TRUST_ACCOUNT);
    case syserr_domain_trust_inconsistent: return (ERROR_DOMAIN_TRUST_INCONSISTENT);
    case syserr_server_has_open_handles: return (ERROR_SERVER_HAS_OPEN_HANDLES);
    case syserr_resource_data_not_found: return (ERROR_RESOURCE_DATA_NOT_FOUND);
    case syserr_resource_type_not_found: return (ERROR_RESOURCE_TYPE_NOT_FOUND);
    case syserr_resource_name_not_found: return (ERROR_RESOURCE_NAME_NOT_FOUND);
    case syserr_resource_lang_not_found: return (ERROR_RESOURCE_LANG_NOT_FOUND);
    case syserr_not_enough_quota: return (ERROR_NOT_ENOUGH_QUOTA);
    case syserr_rpc_s_no_interfaces: return (RPC_S_NO_INTERFACES);
    case syserr_rpc_s_call_cancelled: return (RPC_S_CALL_CANCELLED);
    case syserr_rpc_s_binding_incomplete: return (RPC_S_BINDING_INCOMPLETE);
    case syserr_rpc_s_comm_failure: return (RPC_S_COMM_FAILURE);
    case syserr_rpc_s_unsupported_authn_level: return (RPC_S_UNSUPPORTED_AUTHN_LEVEL);
    case syserr_rpc_s_no_princ_name: return (RPC_S_NO_PRINC_NAME);
    case syserr_rpc_s_not_rpc_error: return (RPC_S_NOT_RPC_ERROR);
    case syserr_rpc_s_uuid_local_only: return (RPC_S_UUID_LOCAL_ONLY);
    case syserr_rpc_s_sec_pkg_error: return (RPC_S_SEC_PKG_ERROR);
    case syserr_rpc_s_not_cancelled: return (RPC_S_NOT_CANCELLED);
    case syserr_rpc_x_invalid_es_action: return (RPC_X_INVALID_ES_ACTION);
    case syserr_rpc_x_wrong_es_version: return (RPC_X_WRONG_ES_VERSION);
    case syserr_rpc_x_wrong_stub_version: return (RPC_X_WRONG_STUB_VERSION);
    case syserr_rpc_s_group_member_not_found: return (RPC_S_GROUP_MEMBER_NOT_FOUND);
    case syserr_ept_s_cant_create: return (EPT_S_CANT_CREATE);
    case syserr_rpc_s_invalid_object: return (RPC_S_INVALID_OBJECT);
    case syserr_invalid_time: return (ERROR_INVALID_TIME);
    case syserr_invalid_form_name: return (ERROR_INVALID_FORM_NAME);
    case syserr_invalid_form_size: return (ERROR_INVALID_FORM_SIZE);
    case syserr_already_waiting: return (ERROR_ALREADY_WAITING);
    case syserr_printer_deleted: return (ERROR_PRINTER_DELETED);
    case syserr_invalid_printer_state: return (ERROR_INVALID_PRINTER_STATE);
    case syserr_password_must_change: return (ERROR_PASSWORD_MUST_CHANGE);
    case syserr_domain_controller_not_found: return (ERROR_DOMAIN_CONTROLLER_NOT_FOUND);
    case syserr_account_locked_out: return (ERROR_ACCOUNT_LOCKED_OUT);
    case syserr_no_browser_servers_found: return (ERROR_NO_BROWSER_SERVERS_FOUND);
    case syserr_invalid_pixel_format: return (ERROR_INVALID_PIXEL_FORMAT);
    case syserr_bad_driver: return (ERROR_BAD_DRIVER);
    case syserr_invalid_window_style: return (ERROR_INVALID_WINDOW_STYLE);
    case syserr_metafile_not_supported: return (ERROR_METAFILE_NOT_SUPPORTED);
    case syserr_transform_not_supported: return (ERROR_TRANSFORM_NOT_SUPPORTED);
    case syserr_clipping_not_supported: return (ERROR_CLIPPING_NOT_SUPPORTED);
    case syserr_unknown_print_monitor: return (ERROR_UNKNOWN_PRINT_MONITOR);
    case syserr_printer_driver_in_use: return (ERROR_PRINTER_DRIVER_IN_USE);
    case syserr_spool_file_not_found: return (ERROR_SPOOL_FILE_NOT_FOUND);
    case syserr_spl_no_startdoc: return (ERROR_SPL_NO_STARTDOC);
    case syserr_spl_no_addjob: return (ERROR_SPL_NO_ADDJOB);
    case syserr_print_processor_already_installed: return (ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED);
    case syserr_print_monitor_already_installed: return (ERROR_PRINT_MONITOR_ALREADY_INSTALLED);
    case syserr_wins_internal: return (ERROR_WINS_INTERNAL);
    case syserr_can_not_del_local_wins: return (ERROR_CAN_NOT_DEL_LOCAL_WINS);
    case syserr_static_init: return (ERROR_STATIC_INIT);
    case syserr_inc_backup: return (ERROR_INC_BACKUP);
    case syserr_full_backup: return (ERROR_FULL_BACKUP);
    case syserr_rec_non_existent: return (ERROR_REC_NON_EXISTENT);
    case syserr_rpl_not_allowed: return (ERROR_RPL_NOT_ALLOWED);

      /* Winsock error codes: */
    case syserr_wsaeintr: return (WSAEINTR);
    case syserr_wsaebadf: return (WSAEBADF);
    case syserr_wsaeacces: return (WSAEACCES);
    case syserr_wsaefault: return (WSAEFAULT);
    case syserr_wsaeinval: return (WSAEINVAL);
    case syserr_wsaemfile: return (WSAEMFILE);
    case syserr_wsaewouldblock: return (WSAEWOULDBLOCK);
    case syserr_wsaeinprogress: return (WSAEINPROGRESS);
    case syserr_wsaealready: return (WSAEALREADY);
    case syserr_wsaenotsock: return (WSAENOTSOCK);
    case syserr_wsaedestaddrreq: return (WSAEDESTADDRREQ);
    case syserr_wsaemsgsize: return (WSAEMSGSIZE);
    case syserr_wsaeprototype: return (WSAEPROTOTYPE);
    case syserr_wsaenoprotoopt: return (WSAENOPROTOOPT);
    case syserr_wsaeprotonosupport: return (WSAEPROTONOSUPPORT);
    case syserr_wsaesocktnosupport: return (WSAESOCKTNOSUPPORT);
    case syserr_wsaeopnotsupp: return (WSAEOPNOTSUPP);
    case syserr_wsaepfnosupport: return (WSAEPFNOSUPPORT);
    case syserr_wsaeafnosupport: return (WSAEAFNOSUPPORT);
    case syserr_wsaeaddrinuse: return (WSAEADDRINUSE);
    case syserr_wsaeaddrnotavail: return (WSAEADDRNOTAVAIL);
    case syserr_wsaenetdown: return (WSAENETDOWN);
    case syserr_wsaenetunreach: return (WSAENETUNREACH);
    case syserr_wsaenetreset: return (WSAENETRESET);
    case syserr_wsaeconnaborted: return (WSAECONNABORTED);
    case syserr_wsaeconnreset: return (WSAECONNRESET);
    case syserr_wsaenobufs: return (WSAENOBUFS);
    case syserr_wsaeisconn: return (WSAEISCONN);
    case syserr_wsaenotconn: return (WSAENOTCONN);
    case syserr_wsaeshutdown: return (WSAESHUTDOWN);
    case syserr_wsaetoomanyrefs: return (WSAETOOMANYREFS);
    case syserr_wsaetimedout: return (WSAETIMEDOUT);
    case syserr_wsaeconnrefused: return (WSAECONNREFUSED);
    case syserr_wsaeloop: return (WSAELOOP);
    case syserr_wsaenametoolong: return (WSAENAMETOOLONG);
    case syserr_wsaehostdown: return (WSAEHOSTDOWN);
    case syserr_wsaehostunreach: return (WSAEHOSTUNREACH);
    case syserr_wsaenotempty: return (WSAENOTEMPTY);
    case syserr_wsaeproclim: return (WSAEPROCLIM);
    case syserr_wsaeusers: return (WSAEUSERS);
    case syserr_wsaedquot: return (WSAEDQUOT);
    case syserr_wsaestale: return (WSAESTALE);
    case syserr_wsaeremote: return (WSAEREMOTE);
    case syserr_wsaediscon: return (WSAEDISCON);
    case syserr_wsasysnotready: return (WSASYSNOTREADY);
    case syserr_wsavernotsupported: return (WSAVERNOTSUPPORTED);
    case syserr_wsanotinitialised: return (WSANOTINITIALISED);
    case syserr_wsahost_not_found: return (WSAHOST_NOT_FOUND);
    case syserr_wsatry_again: return (WSATRY_AGAIN);
    case syserr_wsano_recovery: return (WSANO_RECOVERY);
    case syserr_wsano_data: return (WSANO_DATA);

    default: return (ERROR_SUCCESS);
    }
}

/* Machine-generated procedure, do not edit: */
static enum syserr_names
win32_error_code_to_syserr (DWORD code)
{
  switch (code)
    {
    case ERROR_SUCCESS: return (syserr_success);
    case ERROR_INVALID_FUNCTION: return (syserr_invalid_function);
    case ERROR_FILE_NOT_FOUND: return (syserr_file_not_found);
    case ERROR_PATH_NOT_FOUND: return (syserr_path_not_found);
    case ERROR_TOO_MANY_OPEN_FILES: return (syserr_too_many_open_files);
    case ERROR_ACCESS_DENIED: return (syserr_access_denied);
    case ERROR_INVALID_HANDLE: return (syserr_invalid_handle);
    case ERROR_ARENA_TRASHED: return (syserr_arena_trashed);
    case ERROR_NOT_ENOUGH_MEMORY: return (syserr_not_enough_memory);
    case ERROR_INVALID_BLOCK: return (syserr_invalid_block);
    case ERROR_BAD_ENVIRONMENT: return (syserr_bad_environment);
    case ERROR_BAD_FORMAT: return (syserr_bad_format);
    case ERROR_INVALID_ACCESS: return (syserr_invalid_access);
    case ERROR_INVALID_DATA: return (syserr_invalid_data);
    case ERROR_OUTOFMEMORY: return (syserr_outofmemory);
    case ERROR_INVALID_DRIVE: return (syserr_invalid_drive);
    case ERROR_CURRENT_DIRECTORY: return (syserr_current_directory);
    case ERROR_NOT_SAME_DEVICE: return (syserr_not_same_device);
    case ERROR_NO_MORE_FILES: return (syserr_no_more_files);
    case ERROR_WRITE_PROTECT: return (syserr_write_protect);
    case ERROR_BAD_UNIT: return (syserr_bad_unit);
    case ERROR_NOT_READY: return (syserr_not_ready);
    case ERROR_BAD_COMMAND: return (syserr_bad_command);
    case ERROR_CRC: return (syserr_crc);
    case ERROR_BAD_LENGTH: return (syserr_bad_length);
    case ERROR_SEEK: return (syserr_seek);
    case ERROR_NOT_DOS_DISK: return (syserr_not_dos_disk);
    case ERROR_SECTOR_NOT_FOUND: return (syserr_sector_not_found);
    case ERROR_OUT_OF_PAPER: return (syserr_out_of_paper);
    case ERROR_WRITE_FAULT: return (syserr_write_fault);
    case ERROR_READ_FAULT: return (syserr_read_fault);
    case ERROR_GEN_FAILURE: return (syserr_gen_failure);
    case ERROR_SHARING_VIOLATION: return (syserr_sharing_violation);
    case ERROR_LOCK_VIOLATION: return (syserr_lock_violation);
    case ERROR_WRONG_DISK: return (syserr_wrong_disk);
    case ERROR_SHARING_BUFFER_EXCEEDED: return (syserr_sharing_buffer_exceeded);
    case ERROR_HANDLE_EOF: return (syserr_handle_eof);
    case ERROR_HANDLE_DISK_FULL: return (syserr_handle_disk_full);
    case ERROR_NOT_SUPPORTED: return (syserr_not_supported);
    case ERROR_REM_NOT_LIST: return (syserr_rem_not_list);
    case ERROR_DUP_NAME: return (syserr_dup_name);
    case ERROR_BAD_NETPATH: return (syserr_bad_netpath);
    case ERROR_NETWORK_BUSY: return (syserr_network_busy);
    case ERROR_DEV_NOT_EXIST: return (syserr_dev_not_exist);
    case ERROR_TOO_MANY_CMDS: return (syserr_too_many_cmds);
    case ERROR_ADAP_HDW_ERR: return (syserr_adap_hdw_err);
    case ERROR_BAD_NET_RESP: return (syserr_bad_net_resp);
    case ERROR_UNEXP_NET_ERR: return (syserr_unexp_net_err);
    case ERROR_BAD_REM_ADAP: return (syserr_bad_rem_adap);
    case ERROR_PRINTQ_FULL: return (syserr_printq_full);
    case ERROR_NO_SPOOL_SPACE: return (syserr_no_spool_space);
    case ERROR_PRINT_CANCELLED: return (syserr_print_cancelled);
    case ERROR_NETNAME_DELETED: return (syserr_netname_deleted);
    case ERROR_NETWORK_ACCESS_DENIED: return (syserr_network_access_denied);
    case ERROR_BAD_DEV_TYPE: return (syserr_bad_dev_type);
    case ERROR_BAD_NET_NAME: return (syserr_bad_net_name);
    case ERROR_TOO_MANY_NAMES: return (syserr_too_many_names);
    case ERROR_TOO_MANY_SESS: return (syserr_too_many_sess);
    case ERROR_SHARING_PAUSED: return (syserr_sharing_paused);
    case ERROR_REQ_NOT_ACCEP: return (syserr_req_not_accep);
    case ERROR_REDIR_PAUSED: return (syserr_redir_paused);
    case ERROR_FILE_EXISTS: return (syserr_file_exists);
    case ERROR_CANNOT_MAKE: return (syserr_cannot_make);
    case ERROR_FAIL_I24: return (syserr_fail_i24);
    case ERROR_OUT_OF_STRUCTURES: return (syserr_out_of_structures);
    case ERROR_ALREADY_ASSIGNED: return (syserr_already_assigned);
    case ERROR_INVALID_PASSWORD: return (syserr_invalid_password);
    case ERROR_INVALID_PARAMETER: return (syserr_invalid_parameter);
    case ERROR_NET_WRITE_FAULT: return (syserr_net_write_fault);
    case ERROR_NO_PROC_SLOTS: return (syserr_no_proc_slots);
    case ERROR_TOO_MANY_SEMAPHORES: return (syserr_too_many_semaphores);
    case ERROR_EXCL_SEM_ALREADY_OWNED: return (syserr_excl_sem_already_owned);
    case ERROR_SEM_IS_SET: return (syserr_sem_is_set);
    case ERROR_TOO_MANY_SEM_REQUESTS: return (syserr_too_many_sem_requests);
    case ERROR_INVALID_AT_INTERRUPT_TIME: return (syserr_invalid_at_interrupt_time);
    case ERROR_SEM_OWNER_DIED: return (syserr_sem_owner_died);
    case ERROR_SEM_USER_LIMIT: return (syserr_sem_user_limit);
    case ERROR_DISK_CHANGE: return (syserr_disk_change);
    case ERROR_DRIVE_LOCKED: return (syserr_drive_locked);
    case ERROR_BROKEN_PIPE: return (syserr_broken_pipe);
    case ERROR_OPEN_FAILED: return (syserr_open_failed);
    case ERROR_BUFFER_OVERFLOW: return (syserr_buffer_overflow);
    case ERROR_DISK_FULL: return (syserr_disk_full);
    case ERROR_NO_MORE_SEARCH_HANDLES: return (syserr_no_more_search_handles);
    case ERROR_INVALID_TARGET_HANDLE: return (syserr_invalid_target_handle);
    case ERROR_INVALID_CATEGORY: return (syserr_invalid_category);
    case ERROR_INVALID_VERIFY_SWITCH: return (syserr_invalid_verify_switch);
    case ERROR_BAD_DRIVER_LEVEL: return (syserr_bad_driver_level);
    case ERROR_CALL_NOT_IMPLEMENTED: return (syserr_call_not_implemented);
    case ERROR_SEM_TIMEOUT: return (syserr_sem_timeout);
    case ERROR_INSUFFICIENT_BUFFER: return (syserr_insufficient_buffer);
    case ERROR_INVALID_NAME: return (syserr_invalid_name);
    case ERROR_INVALID_LEVEL: return (syserr_invalid_level);
    case ERROR_NO_VOLUME_LABEL: return (syserr_no_volume_label);
    case ERROR_MOD_NOT_FOUND: return (syserr_mod_not_found);
    case ERROR_PROC_NOT_FOUND: return (syserr_proc_not_found);
    case ERROR_WAIT_NO_CHILDREN: return (syserr_wait_no_children);
    case ERROR_CHILD_NOT_COMPLETE: return (syserr_child_not_complete);
    case ERROR_DIRECT_ACCESS_HANDLE: return (syserr_direct_access_handle);
    case ERROR_NEGATIVE_SEEK: return (syserr_negative_seek);
    case ERROR_SEEK_ON_DEVICE: return (syserr_seek_on_device);
    case ERROR_IS_JOIN_TARGET: return (syserr_is_join_target);
    case ERROR_IS_JOINED: return (syserr_is_joined);
    case ERROR_IS_SUBSTED: return (syserr_is_substed);
    case ERROR_NOT_JOINED: return (syserr_not_joined);
    case ERROR_NOT_SUBSTED: return (syserr_not_substed);
    case ERROR_JOIN_TO_JOIN: return (syserr_join_to_join);
    case ERROR_SUBST_TO_SUBST: return (syserr_subst_to_subst);
    case ERROR_JOIN_TO_SUBST: return (syserr_join_to_subst);
    case ERROR_SUBST_TO_JOIN: return (syserr_subst_to_join);
    case ERROR_BUSY_DRIVE: return (syserr_busy_drive);
    case ERROR_SAME_DRIVE: return (syserr_same_drive);
    case ERROR_DIR_NOT_ROOT: return (syserr_dir_not_root);
    case ERROR_DIR_NOT_EMPTY: return (syserr_dir_not_empty);
    case ERROR_IS_SUBST_PATH: return (syserr_is_subst_path);
    case ERROR_IS_JOIN_PATH: return (syserr_is_join_path);
    case ERROR_PATH_BUSY: return (syserr_path_busy);
    case ERROR_IS_SUBST_TARGET: return (syserr_is_subst_target);
    case ERROR_SYSTEM_TRACE: return (syserr_system_trace);
    case ERROR_INVALID_EVENT_COUNT: return (syserr_invalid_event_count);
    case ERROR_TOO_MANY_MUXWAITERS: return (syserr_too_many_muxwaiters);
    case ERROR_INVALID_LIST_FORMAT: return (syserr_invalid_list_format);
    case ERROR_LABEL_TOO_LONG: return (syserr_label_too_long);
    case ERROR_TOO_MANY_TCBS: return (syserr_too_many_tcbs);
    case ERROR_SIGNAL_REFUSED: return (syserr_signal_refused);
    case ERROR_DISCARDED: return (syserr_discarded);
    case ERROR_NOT_LOCKED: return (syserr_not_locked);
    case ERROR_BAD_THREADID_ADDR: return (syserr_bad_threadid_addr);
    case ERROR_BAD_ARGUMENTS: return (syserr_bad_arguments);
    case ERROR_BAD_PATHNAME: return (syserr_bad_pathname);
    case ERROR_SIGNAL_PENDING: return (syserr_signal_pending);
    case ERROR_MAX_THRDS_REACHED: return (syserr_max_thrds_reached);
    case ERROR_LOCK_FAILED: return (syserr_lock_failed);
    case ERROR_BUSY: return (syserr_busy);
    case ERROR_CANCEL_VIOLATION: return (syserr_cancel_violation);
    case ERROR_ATOMIC_LOCKS_NOT_SUPPORTED: return (syserr_atomic_locks_not_supported);
    case ERROR_INVALID_SEGMENT_NUMBER: return (syserr_invalid_segment_number);
    case ERROR_INVALID_ORDINAL: return (syserr_invalid_ordinal);
    case ERROR_ALREADY_EXISTS: return (syserr_already_exists);
    case ERROR_INVALID_FLAG_NUMBER: return (syserr_invalid_flag_number);
    case ERROR_SEM_NOT_FOUND: return (syserr_sem_not_found);
    case ERROR_INVALID_STARTING_CODESEG: return (syserr_invalid_starting_codeseg);
    case ERROR_INVALID_STACKSEG: return (syserr_invalid_stackseg);
    case ERROR_INVALID_MODULETYPE: return (syserr_invalid_moduletype);
    case ERROR_INVALID_EXE_SIGNATURE: return (syserr_invalid_exe_signature);
    case ERROR_EXE_MARKED_INVALID: return (syserr_exe_marked_invalid);
    case ERROR_BAD_EXE_FORMAT: return (syserr_bad_exe_format);
    case ERROR_ITERATED_DATA_EXCEEDS_64k: return (syserr_iterated_data_exceeds_64k);
    case ERROR_INVALID_MINALLOCSIZE: return (syserr_invalid_minallocsize);
    case ERROR_DYNLINK_FROM_INVALID_RING: return (syserr_dynlink_from_invalid_ring);
    case ERROR_IOPL_NOT_ENABLED: return (syserr_iopl_not_enabled);
    case ERROR_INVALID_SEGDPL: return (syserr_invalid_segdpl);
    case ERROR_AUTODATASEG_EXCEEDS_64k: return (syserr_autodataseg_exceeds_64k);
    case ERROR_RING2SEG_MUST_BE_MOVABLE: return (syserr_ring2seg_must_be_movable);
    case ERROR_RELOC_CHAIN_XEEDS_SEGLIM: return (syserr_reloc_chain_xeeds_seglim);
    case ERROR_INFLOOP_IN_RELOC_CHAIN: return (syserr_infloop_in_reloc_chain);
    case ERROR_ENVVAR_NOT_FOUND: return (syserr_envvar_not_found);
    case ERROR_NO_SIGNAL_SENT: return (syserr_no_signal_sent);
    case ERROR_FILENAME_EXCED_RANGE: return (syserr_filename_exced_range);
    case ERROR_RING2_STACK_IN_USE: return (syserr_ring2_stack_in_use);
    case ERROR_META_EXPANSION_TOO_LONG: return (syserr_meta_expansion_too_long);
    case ERROR_INVALID_SIGNAL_NUMBER: return (syserr_invalid_signal_number);
    case ERROR_THREAD_1_INACTIVE: return (syserr_thread_1_inactive);
    case ERROR_LOCKED: return (syserr_locked);
    case ERROR_TOO_MANY_MODULES: return (syserr_too_many_modules);
    case ERROR_NESTING_NOT_ALLOWED: return (syserr_nesting_not_allowed);
    case ERROR_BAD_PIPE: return (syserr_bad_pipe);
    case ERROR_PIPE_BUSY: return (syserr_pipe_busy);
    case ERROR_NO_DATA: return (syserr_no_data);
    case ERROR_PIPE_NOT_CONNECTED: return (syserr_pipe_not_connected);
    case ERROR_MORE_DATA: return (syserr_more_data);
    case ERROR_VC_DISCONNECTED: return (syserr_vc_disconnected);
    case ERROR_INVALID_EA_NAME: return (syserr_invalid_ea_name);
    case ERROR_EA_LIST_INCONSISTENT: return (syserr_ea_list_inconsistent);
    case ERROR_NO_MORE_ITEMS: return (syserr_no_more_items);
    case ERROR_CANNOT_COPY: return (syserr_cannot_copy);
    case ERROR_DIRECTORY: return (syserr_directory);
    case ERROR_EAS_DIDNT_FIT: return (syserr_eas_didnt_fit);
    case ERROR_EA_FILE_CORRUPT: return (syserr_ea_file_corrupt);
    case ERROR_EA_TABLE_FULL: return (syserr_ea_table_full);
    case ERROR_INVALID_EA_HANDLE: return (syserr_invalid_ea_handle);
    case ERROR_EAS_NOT_SUPPORTED: return (syserr_eas_not_supported);
    case ERROR_NOT_OWNER: return (syserr_not_owner);
    case ERROR_TOO_MANY_POSTS: return (syserr_too_many_posts);
    case ERROR_PARTIAL_COPY: return (syserr_partial_copy);
    case ERROR_MR_MID_NOT_FOUND: return (syserr_mr_mid_not_found);
    case ERROR_INVALID_ADDRESS: return (syserr_invalid_address);
    case ERROR_ARITHMETIC_OVERFLOW: return (syserr_arithmetic_overflow);
    case ERROR_PIPE_CONNECTED: return (syserr_pipe_connected);
    case ERROR_PIPE_LISTENING: return (syserr_pipe_listening);
    case ERROR_EA_ACCESS_DENIED: return (syserr_ea_access_denied);
    case ERROR_OPERATION_ABORTED: return (syserr_operation_aborted);
    case ERROR_IO_INCOMPLETE: return (syserr_io_incomplete);
    case ERROR_IO_PENDING: return (syserr_io_pending);
    case ERROR_NOACCESS: return (syserr_noaccess);
    case ERROR_SWAPERROR: return (syserr_swaperror);
    case ERROR_STACK_OVERFLOW: return (syserr_stack_overflow);
    case ERROR_INVALID_MESSAGE: return (syserr_invalid_message);
    case ERROR_CAN_NOT_COMPLETE: return (syserr_can_not_complete);
    case ERROR_INVALID_FLAGS: return (syserr_invalid_flags);
    case ERROR_UNRECOGNIZED_VOLUME: return (syserr_unrecognized_volume);
    case ERROR_FILE_INVALID: return (syserr_file_invalid);
    case ERROR_FULLSCREEN_MODE: return (syserr_fullscreen_mode);
    case ERROR_NO_TOKEN: return (syserr_no_token);
    case ERROR_BADDB: return (syserr_baddb);
    case ERROR_BADKEY: return (syserr_badkey);
    case ERROR_CANTOPEN: return (syserr_cantopen);
    case ERROR_CANTREAD: return (syserr_cantread);
    case ERROR_CANTWRITE: return (syserr_cantwrite);
    case ERROR_REGISTRY_RECOVERED: return (syserr_registry_recovered);
    case ERROR_REGISTRY_CORRUPT: return (syserr_registry_corrupt);
    case ERROR_REGISTRY_IO_FAILED: return (syserr_registry_io_failed);
    case ERROR_NOT_REGISTRY_FILE: return (syserr_not_registry_file);
    case ERROR_KEY_DELETED: return (syserr_key_deleted);
    case ERROR_NO_LOG_SPACE: return (syserr_no_log_space);
    case ERROR_KEY_HAS_CHILDREN: return (syserr_key_has_children);
    case ERROR_CHILD_MUST_BE_VOLATILE: return (syserr_child_must_be_volatile);
    case ERROR_NOTIFY_ENUM_DIR: return (syserr_notify_enum_dir);
    case ERROR_DEPENDENT_SERVICES_RUNNING: return (syserr_dependent_services_running);
    case ERROR_INVALID_SERVICE_CONTROL: return (syserr_invalid_service_control);
    case ERROR_SERVICE_REQUEST_TIMEOUT: return (syserr_service_request_timeout);
    case ERROR_SERVICE_NO_THREAD: return (syserr_service_no_thread);
    case ERROR_SERVICE_DATABASE_LOCKED: return (syserr_service_database_locked);
    case ERROR_SERVICE_ALREADY_RUNNING: return (syserr_service_already_running);
    case ERROR_INVALID_SERVICE_ACCOUNT: return (syserr_invalid_service_account);
    case ERROR_SERVICE_DISABLED: return (syserr_service_disabled);
    case ERROR_CIRCULAR_DEPENDENCY: return (syserr_circular_dependency);
    case ERROR_SERVICE_DOES_NOT_EXIST: return (syserr_service_does_not_exist);
    case ERROR_SERVICE_CANNOT_ACCEPT_CTRL: return (syserr_service_cannot_accept_ctrl);
    case ERROR_SERVICE_NOT_ACTIVE: return (syserr_service_not_active);
    case ERROR_FAILED_SERVICE_CONTROLLER_CONNECT: return (syserr_failed_service_controller_connect);
    case ERROR_EXCEPTION_IN_SERVICE: return (syserr_exception_in_service);
    case ERROR_DATABASE_DOES_NOT_EXIST: return (syserr_database_does_not_exist);
    case ERROR_SERVICE_SPECIFIC_ERROR: return (syserr_service_specific_error);
    case ERROR_PROCESS_ABORTED: return (syserr_process_aborted);
    case ERROR_SERVICE_DEPENDENCY_FAIL: return (syserr_service_dependency_fail);
    case ERROR_SERVICE_LOGON_FAILED: return (syserr_service_logon_failed);
    case ERROR_SERVICE_START_HANG: return (syserr_service_start_hang);
    case ERROR_INVALID_SERVICE_LOCK: return (syserr_invalid_service_lock);
    case ERROR_SERVICE_MARKED_FOR_DELETE: return (syserr_service_marked_for_delete);
    case ERROR_SERVICE_EXISTS: return (syserr_service_exists);
    case ERROR_ALREADY_RUNNING_LKG: return (syserr_already_running_lkg);
    case ERROR_SERVICE_DEPENDENCY_DELETED: return (syserr_service_dependency_deleted);
    case ERROR_BOOT_ALREADY_ACCEPTED: return (syserr_boot_already_accepted);
    case ERROR_SERVICE_NEVER_STARTED: return (syserr_service_never_started);
    case ERROR_DUPLICATE_SERVICE_NAME: return (syserr_duplicate_service_name);
    case ERROR_END_OF_MEDIA: return (syserr_end_of_media);
    case ERROR_FILEMARK_DETECTED: return (syserr_filemark_detected);
    case ERROR_BEGINNING_OF_MEDIA: return (syserr_beginning_of_media);
    case ERROR_SETMARK_DETECTED: return (syserr_setmark_detected);
    case ERROR_NO_DATA_DETECTED: return (syserr_no_data_detected);
    case ERROR_PARTITION_FAILURE: return (syserr_partition_failure);
    case ERROR_INVALID_BLOCK_LENGTH: return (syserr_invalid_block_length);
    case ERROR_DEVICE_NOT_PARTITIONED: return (syserr_device_not_partitioned);
    case ERROR_UNABLE_TO_LOCK_MEDIA: return (syserr_unable_to_lock_media);
    case ERROR_UNABLE_TO_UNLOAD_MEDIA: return (syserr_unable_to_unload_media);
    case ERROR_MEDIA_CHANGED: return (syserr_media_changed);
    case ERROR_BUS_RESET: return (syserr_bus_reset);
    case ERROR_NO_MEDIA_IN_DRIVE: return (syserr_no_media_in_drive);
    case ERROR_NO_UNICODE_TRANSLATION: return (syserr_no_unicode_translation);
    case ERROR_DLL_INIT_FAILED: return (syserr_dll_init_failed);
    case ERROR_SHUTDOWN_IN_PROGRESS: return (syserr_shutdown_in_progress);
    case ERROR_NO_SHUTDOWN_IN_PROGRESS: return (syserr_no_shutdown_in_progress);
    case ERROR_IO_DEVICE: return (syserr_io_device);
    case ERROR_SERIAL_NO_DEVICE: return (syserr_serial_no_device);
    case ERROR_IRQ_BUSY: return (syserr_irq_busy);
    case ERROR_MORE_WRITES: return (syserr_more_writes);
    case ERROR_COUNTER_TIMEOUT: return (syserr_counter_timeout);
    case ERROR_FLOPPY_ID_MARK_NOT_FOUND: return (syserr_floppy_id_mark_not_found);
    case ERROR_FLOPPY_WRONG_CYLINDER: return (syserr_floppy_wrong_cylinder);
    case ERROR_FLOPPY_UNKNOWN_ERROR: return (syserr_floppy_unknown_error);
    case ERROR_FLOPPY_BAD_REGISTERS: return (syserr_floppy_bad_registers);
    case ERROR_DISK_RECALIBRATE_FAILED: return (syserr_disk_recalibrate_failed);
    case ERROR_DISK_OPERATION_FAILED: return (syserr_disk_operation_failed);
    case ERROR_DISK_RESET_FAILED: return (syserr_disk_reset_failed);
    case ERROR_EOM_OVERFLOW: return (syserr_eom_overflow);
    case ERROR_NOT_ENOUGH_SERVER_MEMORY: return (syserr_not_enough_server_memory);
    case ERROR_POSSIBLE_DEADLOCK: return (syserr_possible_deadlock);
    case ERROR_MAPPED_ALIGNMENT: return (syserr_mapped_alignment);
    case ERROR_SET_POWER_STATE_VETOED: return (syserr_set_power_state_vetoed);
    case ERROR_SET_POWER_STATE_FAILED: return (syserr_set_power_state_failed);
    case ERROR_OLD_WIN_VERSION: return (syserr_old_win_version);
    case ERROR_APP_WRONG_OS: return (syserr_app_wrong_os);
    case ERROR_SINGLE_INSTANCE_APP: return (syserr_single_instance_app);
    case ERROR_RMODE_APP: return (syserr_rmode_app);
    case ERROR_INVALID_DLL: return (syserr_invalid_dll);
    case ERROR_NO_ASSOCIATION: return (syserr_no_association);
    case ERROR_DDE_FAIL: return (syserr_dde_fail);
    case ERROR_DLL_NOT_FOUND: return (syserr_dll_not_found);
    case ERROR_BAD_USERNAME: return (syserr_bad_username);
    case ERROR_NOT_CONNECTED: return (syserr_not_connected);
    case ERROR_OPEN_FILES: return (syserr_open_files);
    case ERROR_ACTIVE_CONNECTIONS: return (syserr_active_connections);
    case ERROR_DEVICE_IN_USE: return (syserr_device_in_use);
    case ERROR_BAD_DEVICE: return (syserr_bad_device);
    case ERROR_CONNECTION_UNAVAIL: return (syserr_connection_unavail);
    case ERROR_DEVICE_ALREADY_REMEMBERED: return (syserr_device_already_remembered);
    case ERROR_NO_NET_OR_BAD_PATH: return (syserr_no_net_or_bad_path);
    case ERROR_BAD_PROVIDER: return (syserr_bad_provider);
    case ERROR_CANNOT_OPEN_PROFILE: return (syserr_cannot_open_profile);
    case ERROR_BAD_PROFILE: return (syserr_bad_profile);
    case ERROR_NOT_CONTAINER: return (syserr_not_container);
    case ERROR_EXTENDED_ERROR: return (syserr_extended_error);
    case ERROR_INVALID_GROUPNAME: return (syserr_invalid_groupname);
    case ERROR_INVALID_COMPUTERNAME: return (syserr_invalid_computername);
    case ERROR_INVALID_EVENTNAME: return (syserr_invalid_eventname);
    case ERROR_INVALID_DOMAINNAME: return (syserr_invalid_domainname);
    case ERROR_INVALID_SERVICENAME: return (syserr_invalid_servicename);
    case ERROR_INVALID_NETNAME: return (syserr_invalid_netname);
    case ERROR_INVALID_SHARENAME: return (syserr_invalid_sharename);
    case ERROR_INVALID_PASSWORDNAME: return (syserr_invalid_passwordname);
    case ERROR_INVALID_MESSAGENAME: return (syserr_invalid_messagename);
    case ERROR_INVALID_MESSAGEDEST: return (syserr_invalid_messagedest);
    case ERROR_SESSION_CREDENTIAL_CONFLICT: return (syserr_session_credential_conflict);
    case ERROR_REMOTE_SESSION_LIMIT_EXCEEDED: return (syserr_remote_session_limit_exceeded);
    case ERROR_DUP_DOMAINNAME: return (syserr_dup_domainname);
    case ERROR_NO_NETWORK: return (syserr_no_network);
    case ERROR_CANCELLED: return (syserr_cancelled);
    case ERROR_USER_MAPPED_FILE: return (syserr_user_mapped_file);
    case ERROR_CONNECTION_REFUSED: return (syserr_connection_refused);
    case ERROR_GRACEFUL_DISCONNECT: return (syserr_graceful_disconnect);
    case ERROR_ADDRESS_ALREADY_ASSOCIATED: return (syserr_address_already_associated);
    case ERROR_ADDRESS_NOT_ASSOCIATED: return (syserr_address_not_associated);
    case ERROR_CONNECTION_INVALID: return (syserr_connection_invalid);
    case ERROR_CONNECTION_ACTIVE: return (syserr_connection_active);
    case ERROR_NETWORK_UNREACHABLE: return (syserr_network_unreachable);
    case ERROR_HOST_UNREACHABLE: return (syserr_host_unreachable);
    case ERROR_PROTOCOL_UNREACHABLE: return (syserr_protocol_unreachable);
    case ERROR_PORT_UNREACHABLE: return (syserr_port_unreachable);
    case ERROR_REQUEST_ABORTED: return (syserr_request_aborted);
    case ERROR_CONNECTION_ABORTED: return (syserr_connection_aborted);
    case ERROR_RETRY: return (syserr_retry);
    case ERROR_CONNECTION_COUNT_LIMIT: return (syserr_connection_count_limit);
    case ERROR_LOGIN_TIME_RESTRICTION: return (syserr_login_time_restriction);
    case ERROR_LOGIN_WKSTA_RESTRICTION: return (syserr_login_wksta_restriction);
    case ERROR_INCORRECT_ADDRESS: return (syserr_incorrect_address);
    case ERROR_ALREADY_REGISTERED: return (syserr_already_registered);
    case ERROR_SERVICE_NOT_FOUND: return (syserr_service_not_found);
    case ERROR_NOT_AUTHENTICATED: return (syserr_not_authenticated);
    case ERROR_NOT_LOGGED_ON: return (syserr_not_logged_on);
    case ERROR_CONTINUE: return (syserr_continue);
    case ERROR_ALREADY_INITIALIZED: return (syserr_already_initialized);
    case ERROR_NO_MORE_DEVICES: return (syserr_no_more_devices);
    case ERROR_NOT_ALL_ASSIGNED: return (syserr_not_all_assigned);
    case ERROR_SOME_NOT_MAPPED: return (syserr_some_not_mapped);
    case ERROR_NO_QUOTAS_FOR_ACCOUNT: return (syserr_no_quotas_for_account);
    case ERROR_LOCAL_USER_SESSION_KEY: return (syserr_local_user_session_key);
    case ERROR_NULL_LM_PASSWORD: return (syserr_null_lm_password);
    case ERROR_UNKNOWN_REVISION: return (syserr_unknown_revision);
    case ERROR_REVISION_MISMATCH: return (syserr_revision_mismatch);
    case ERROR_INVALID_OWNER: return (syserr_invalid_owner);
    case ERROR_INVALID_PRIMARY_GROUP: return (syserr_invalid_primary_group);
    case ERROR_NO_IMPERSONATION_TOKEN: return (syserr_no_impersonation_token);
    case ERROR_CANT_DISABLE_MANDATORY: return (syserr_cant_disable_mandatory);
    case ERROR_NO_LOGON_SERVERS: return (syserr_no_logon_servers);
    case ERROR_NO_SUCH_LOGON_SESSION: return (syserr_no_such_logon_session);
    case ERROR_NO_SUCH_PRIVILEGE: return (syserr_no_such_privilege);
    case ERROR_PRIVILEGE_NOT_HELD: return (syserr_privilege_not_held);
    case ERROR_INVALID_ACCOUNT_NAME: return (syserr_invalid_account_name);
    case ERROR_USER_EXISTS: return (syserr_user_exists);
    case ERROR_NO_SUCH_USER: return (syserr_no_such_user);
    case ERROR_GROUP_EXISTS: return (syserr_group_exists);
    case ERROR_NO_SUCH_GROUP: return (syserr_no_such_group);
    case ERROR_MEMBER_IN_GROUP: return (syserr_member_in_group);
    case ERROR_MEMBER_NOT_IN_GROUP: return (syserr_member_not_in_group);
    case ERROR_LAST_ADMIN: return (syserr_last_admin);
    case ERROR_WRONG_PASSWORD: return (syserr_wrong_password);
    case ERROR_ILL_FORMED_PASSWORD: return (syserr_ill_formed_password);
    case ERROR_PASSWORD_RESTRICTION: return (syserr_password_restriction);
    case ERROR_LOGON_FAILURE: return (syserr_logon_failure);
    case ERROR_ACCOUNT_RESTRICTION: return (syserr_account_restriction);
    case ERROR_INVALID_LOGON_HOURS: return (syserr_invalid_logon_hours);
    case ERROR_INVALID_WORKSTATION: return (syserr_invalid_workstation);
    case ERROR_PASSWORD_EXPIRED: return (syserr_password_expired);
    case ERROR_ACCOUNT_DISABLED: return (syserr_account_disabled);
    case ERROR_NONE_MAPPED: return (syserr_none_mapped);
    case ERROR_TOO_MANY_LUIDS_REQUESTED: return (syserr_too_many_luids_requested);
    case ERROR_LUIDS_EXHAUSTED: return (syserr_luids_exhausted);
    case ERROR_INVALID_SUB_AUTHORITY: return (syserr_invalid_sub_authority);
    case ERROR_INVALID_ACL: return (syserr_invalid_acl);
    case ERROR_INVALID_SID: return (syserr_invalid_sid);
    case ERROR_INVALID_SECURITY_DESCR: return (syserr_invalid_security_descr);
    case ERROR_BAD_INHERITANCE_ACL: return (syserr_bad_inheritance_acl);
    case ERROR_SERVER_DISABLED: return (syserr_server_disabled);
    case ERROR_SERVER_NOT_DISABLED: return (syserr_server_not_disabled);
    case ERROR_INVALID_ID_AUTHORITY: return (syserr_invalid_id_authority);
    case ERROR_ALLOTTED_SPACE_EXCEEDED: return (syserr_allotted_space_exceeded);
    case ERROR_INVALID_GROUP_ATTRIBUTES: return (syserr_invalid_group_attributes);
    case ERROR_BAD_IMPERSONATION_LEVEL: return (syserr_bad_impersonation_level);
    case ERROR_CANT_OPEN_ANONYMOUS: return (syserr_cant_open_anonymous);
    case ERROR_BAD_VALIDATION_CLASS: return (syserr_bad_validation_class);
    case ERROR_BAD_TOKEN_TYPE: return (syserr_bad_token_type);
    case ERROR_NO_SECURITY_ON_OBJECT: return (syserr_no_security_on_object);
    case ERROR_CANT_ACCESS_DOMAIN_INFO: return (syserr_cant_access_domain_info);
    case ERROR_INVALID_SERVER_STATE: return (syserr_invalid_server_state);
    case ERROR_INVALID_DOMAIN_STATE: return (syserr_invalid_domain_state);
    case ERROR_INVALID_DOMAIN_ROLE: return (syserr_invalid_domain_role);
    case ERROR_NO_SUCH_DOMAIN: return (syserr_no_such_domain);
    case ERROR_DOMAIN_EXISTS: return (syserr_domain_exists);
    case ERROR_DOMAIN_LIMIT_EXCEEDED: return (syserr_domain_limit_exceeded);
    case ERROR_INTERNAL_DB_CORRUPTION: return (syserr_internal_db_corruption);
    case ERROR_INTERNAL_ERROR: return (syserr_internal_error);
    case ERROR_GENERIC_NOT_MAPPED: return (syserr_generic_not_mapped);
    case ERROR_BAD_DESCRIPTOR_FORMAT: return (syserr_bad_descriptor_format);
    case ERROR_NOT_LOGON_PROCESS: return (syserr_not_logon_process);
    case ERROR_LOGON_SESSION_EXISTS: return (syserr_logon_session_exists);
    case ERROR_NO_SUCH_PACKAGE: return (syserr_no_such_package);
    case ERROR_BAD_LOGON_SESSION_STATE: return (syserr_bad_logon_session_state);
    case ERROR_LOGON_SESSION_COLLISION: return (syserr_logon_session_collision);
    case ERROR_INVALID_LOGON_TYPE: return (syserr_invalid_logon_type);
    case ERROR_CANNOT_IMPERSONATE: return (syserr_cannot_impersonate);
    case ERROR_RXACT_INVALID_STATE: return (syserr_rxact_invalid_state);
    case ERROR_RXACT_COMMIT_FAILURE: return (syserr_rxact_commit_failure);
    case ERROR_SPECIAL_ACCOUNT: return (syserr_special_account);
    case ERROR_SPECIAL_GROUP: return (syserr_special_group);
    case ERROR_SPECIAL_USER: return (syserr_special_user);
    case ERROR_MEMBERS_PRIMARY_GROUP: return (syserr_members_primary_group);
    case ERROR_TOKEN_ALREADY_IN_USE: return (syserr_token_already_in_use);
    case ERROR_NO_SUCH_ALIAS: return (syserr_no_such_alias);
    case ERROR_MEMBER_NOT_IN_ALIAS: return (syserr_member_not_in_alias);
    case ERROR_MEMBER_IN_ALIAS: return (syserr_member_in_alias);
    case ERROR_ALIAS_EXISTS: return (syserr_alias_exists);
    case ERROR_LOGON_NOT_GRANTED: return (syserr_logon_not_granted);
    case ERROR_TOO_MANY_SECRETS: return (syserr_too_many_secrets);
    case ERROR_SECRET_TOO_LONG: return (syserr_secret_too_long);
    case ERROR_INTERNAL_DB_ERROR: return (syserr_internal_db_error);
    case ERROR_TOO_MANY_CONTEXT_IDS: return (syserr_too_many_context_ids);
    case ERROR_LOGON_TYPE_NOT_GRANTED: return (syserr_logon_type_not_granted);
    case ERROR_NT_CROSS_ENCRYPTION_REQUIRED: return (syserr_nt_cross_encryption_required);
    case ERROR_NO_SUCH_MEMBER: return (syserr_no_such_member);
    case ERROR_INVALID_MEMBER: return (syserr_invalid_member);
    case ERROR_TOO_MANY_SIDS: return (syserr_too_many_sids);
    case ERROR_LM_CROSS_ENCRYPTION_REQUIRED: return (syserr_lm_cross_encryption_required);
    case ERROR_NO_INHERITANCE: return (syserr_no_inheritance);
    case ERROR_FILE_CORRUPT: return (syserr_file_corrupt);
    case ERROR_DISK_CORRUPT: return (syserr_disk_corrupt);
    case ERROR_NO_USER_SESSION_KEY: return (syserr_no_user_session_key);
    case ERROR_LICENSE_QUOTA_EXCEEDED: return (syserr_license_quota_exceeded);
    case ERROR_INVALID_WINDOW_HANDLE: return (syserr_invalid_window_handle);
    case ERROR_INVALID_MENU_HANDLE: return (syserr_invalid_menu_handle);
    case ERROR_INVALID_CURSOR_HANDLE: return (syserr_invalid_cursor_handle);
    case ERROR_INVALID_ACCEL_HANDLE: return (syserr_invalid_accel_handle);
    case ERROR_INVALID_HOOK_HANDLE: return (syserr_invalid_hook_handle);
    case ERROR_INVALID_DWP_HANDLE: return (syserr_invalid_dwp_handle);
    case ERROR_TLW_WITH_WSCHILD: return (syserr_tlw_with_wschild);
    case ERROR_CANNOT_FIND_WND_CLASS: return (syserr_cannot_find_wnd_class);
    case ERROR_WINDOW_OF_OTHER_THREAD: return (syserr_window_of_other_thread);
    case ERROR_HOTKEY_ALREADY_REGISTERED: return (syserr_hotkey_already_registered);
    case ERROR_CLASS_ALREADY_EXISTS: return (syserr_class_already_exists);
    case ERROR_CLASS_DOES_NOT_EXIST: return (syserr_class_does_not_exist);
    case ERROR_CLASS_HAS_WINDOWS: return (syserr_class_has_windows);
    case ERROR_INVALID_INDEX: return (syserr_invalid_index);
    case ERROR_INVALID_ICON_HANDLE: return (syserr_invalid_icon_handle);
    case ERROR_PRIVATE_DIALOG_INDEX: return (syserr_private_dialog_index);
    case ERROR_LISTBOX_ID_NOT_FOUND: return (syserr_listbox_id_not_found);
    case ERROR_NO_WILDCARD_CHARACTERS: return (syserr_no_wildcard_characters);
    case ERROR_CLIPBOARD_NOT_OPEN: return (syserr_clipboard_not_open);
    case ERROR_HOTKEY_NOT_REGISTERED: return (syserr_hotkey_not_registered);
    case ERROR_WINDOW_NOT_DIALOG: return (syserr_window_not_dialog);
    case ERROR_CONTROL_ID_NOT_FOUND: return (syserr_control_id_not_found);
    case ERROR_INVALID_COMBOBOX_MESSAGE: return (syserr_invalid_combobox_message);
    case ERROR_WINDOW_NOT_COMBOBOX: return (syserr_window_not_combobox);
    case ERROR_INVALID_EDIT_HEIGHT: return (syserr_invalid_edit_height);
    case ERROR_DC_NOT_FOUND: return (syserr_dc_not_found);
    case ERROR_INVALID_HOOK_FILTER: return (syserr_invalid_hook_filter);
    case ERROR_INVALID_FILTER_PROC: return (syserr_invalid_filter_proc);
    case ERROR_HOOK_NEEDS_HMOD: return (syserr_hook_needs_hmod);
    case ERROR_GLOBAL_ONLY_HOOK: return (syserr_global_only_hook);
    case ERROR_JOURNAL_HOOK_SET: return (syserr_journal_hook_set);
    case ERROR_HOOK_NOT_INSTALLED: return (syserr_hook_not_installed);
    case ERROR_INVALID_LB_MESSAGE: return (syserr_invalid_lb_message);
    case ERROR_SETCOUNT_ON_BAD_LB: return (syserr_setcount_on_bad_lb);
    case ERROR_LB_WITHOUT_TABSTOPS: return (syserr_lb_without_tabstops);
    case ERROR_DESTROY_OBJECT_OF_OTHER_THREAD: return (syserr_destroy_object_of_other_thread);
    case ERROR_CHILD_WINDOW_MENU: return (syserr_child_window_menu);
    case ERROR_NO_SYSTEM_MENU: return (syserr_no_system_menu);
    case ERROR_INVALID_MSGBOX_STYLE: return (syserr_invalid_msgbox_style);
    case ERROR_INVALID_SPI_VALUE: return (syserr_invalid_spi_value);
    case ERROR_SCREEN_ALREADY_LOCKED: return (syserr_screen_already_locked);
    case ERROR_HWNDS_HAVE_DIFF_PARENT: return (syserr_hwnds_have_diff_parent);
    case ERROR_NOT_CHILD_WINDOW: return (syserr_not_child_window);
    case ERROR_INVALID_GW_COMMAND: return (syserr_invalid_gw_command);
    case ERROR_INVALID_THREAD_ID: return (syserr_invalid_thread_id);
    case ERROR_NON_MDICHILD_WINDOW: return (syserr_non_mdichild_window);
    case ERROR_POPUP_ALREADY_ACTIVE: return (syserr_popup_already_active);
    case ERROR_NO_SCROLLBARS: return (syserr_no_scrollbars);
    case ERROR_INVALID_SCROLLBAR_RANGE: return (syserr_invalid_scrollbar_range);
    case ERROR_INVALID_SHOWWIN_COMMAND: return (syserr_invalid_showwin_command);
    case ERROR_NO_SYSTEM_RESOURCES: return (syserr_no_system_resources);
    case ERROR_NONPAGED_SYSTEM_RESOURCES: return (syserr_nonpaged_system_resources);
    case ERROR_PAGED_SYSTEM_RESOURCES: return (syserr_paged_system_resources);
    case ERROR_WORKING_SET_QUOTA: return (syserr_working_set_quota);
    case ERROR_PAGEFILE_QUOTA: return (syserr_pagefile_quota);
    case ERROR_COMMITMENT_LIMIT: return (syserr_commitment_limit);
    case ERROR_MENU_ITEM_NOT_FOUND: return (syserr_menu_item_not_found);
    case ERROR_EVENTLOG_FILE_CORRUPT: return (syserr_eventlog_file_corrupt);
    case ERROR_EVENTLOG_CANT_START: return (syserr_eventlog_cant_start);
    case ERROR_LOG_FILE_FULL: return (syserr_log_file_full);
    case ERROR_EVENTLOG_FILE_CHANGED: return (syserr_eventlog_file_changed);
    case RPC_S_INVALID_STRING_BINDING: return (syserr_rpc_s_invalid_string_binding);
    case RPC_S_WRONG_KIND_OF_BINDING: return (syserr_rpc_s_wrong_kind_of_binding);
    case RPC_S_INVALID_BINDING: return (syserr_rpc_s_invalid_binding);
    case RPC_S_PROTSEQ_NOT_SUPPORTED: return (syserr_rpc_s_protseq_not_supported);
    case RPC_S_INVALID_RPC_PROTSEQ: return (syserr_rpc_s_invalid_rpc_protseq);
    case RPC_S_INVALID_STRING_UUID: return (syserr_rpc_s_invalid_string_uuid);
    case RPC_S_INVALID_ENDPOINT_FORMAT: return (syserr_rpc_s_invalid_endpoint_format);
    case RPC_S_INVALID_NET_ADDR: return (syserr_rpc_s_invalid_net_addr);
    case RPC_S_NO_ENDPOINT_FOUND: return (syserr_rpc_s_no_endpoint_found);
    case RPC_S_INVALID_TIMEOUT: return (syserr_rpc_s_invalid_timeout);
    case RPC_S_OBJECT_NOT_FOUND: return (syserr_rpc_s_object_not_found);
    case RPC_S_ALREADY_REGISTERED: return (syserr_rpc_s_already_registered);
    case RPC_S_TYPE_ALREADY_REGISTERED: return (syserr_rpc_s_type_already_registered);
    case RPC_S_ALREADY_LISTENING: return (syserr_rpc_s_already_listening);
    case RPC_S_NO_PROTSEQS_REGISTERED: return (syserr_rpc_s_no_protseqs_registered);
    case RPC_S_NOT_LISTENING: return (syserr_rpc_s_not_listening);
    case RPC_S_UNKNOWN_MGR_TYPE: return (syserr_rpc_s_unknown_mgr_type);
    case RPC_S_UNKNOWN_IF: return (syserr_rpc_s_unknown_if);
    case RPC_S_NO_BINDINGS: return (syserr_rpc_s_no_bindings);
    case RPC_S_NO_PROTSEQS: return (syserr_rpc_s_no_protseqs);
    case RPC_S_CANT_CREATE_ENDPOINT: return (syserr_rpc_s_cant_create_endpoint);
    case RPC_S_OUT_OF_RESOURCES: return (syserr_rpc_s_out_of_resources);
    case RPC_S_SERVER_UNAVAILABLE: return (syserr_rpc_s_server_unavailable);
    case RPC_S_SERVER_TOO_BUSY: return (syserr_rpc_s_server_too_busy);
    case RPC_S_INVALID_NETWORK_OPTIONS: return (syserr_rpc_s_invalid_network_options);
    case RPC_S_NO_CALL_ACTIVE: return (syserr_rpc_s_no_call_active);
    case RPC_S_CALL_FAILED: return (syserr_rpc_s_call_failed);
    case RPC_S_CALL_FAILED_DNE: return (syserr_rpc_s_call_failed_dne);
    case RPC_S_PROTOCOL_ERROR: return (syserr_rpc_s_protocol_error);
    case RPC_S_UNSUPPORTED_TRANS_SYN: return (syserr_rpc_s_unsupported_trans_syn);
    case RPC_S_UNSUPPORTED_TYPE: return (syserr_rpc_s_unsupported_type);
    case RPC_S_INVALID_TAG: return (syserr_rpc_s_invalid_tag);
    case RPC_S_INVALID_BOUND: return (syserr_rpc_s_invalid_bound);
    case RPC_S_NO_ENTRY_NAME: return (syserr_rpc_s_no_entry_name);
    case RPC_S_INVALID_NAME_SYNTAX: return (syserr_rpc_s_invalid_name_syntax);
    case RPC_S_UNSUPPORTED_NAME_SYNTAX: return (syserr_rpc_s_unsupported_name_syntax);
    case RPC_S_UUID_NO_ADDRESS: return (syserr_rpc_s_uuid_no_address);
    case RPC_S_DUPLICATE_ENDPOINT: return (syserr_rpc_s_duplicate_endpoint);
    case RPC_S_UNKNOWN_AUTHN_TYPE: return (syserr_rpc_s_unknown_authn_type);
    case RPC_S_MAX_CALLS_TOO_SMALL: return (syserr_rpc_s_max_calls_too_small);
    case RPC_S_STRING_TOO_LONG: return (syserr_rpc_s_string_too_long);
    case RPC_S_PROTSEQ_NOT_FOUND: return (syserr_rpc_s_protseq_not_found);
    case RPC_S_PROCNUM_OUT_OF_RANGE: return (syserr_rpc_s_procnum_out_of_range);
    case RPC_S_BINDING_HAS_NO_AUTH: return (syserr_rpc_s_binding_has_no_auth);
    case RPC_S_UNKNOWN_AUTHN_SERVICE: return (syserr_rpc_s_unknown_authn_service);
    case RPC_S_UNKNOWN_AUTHN_LEVEL: return (syserr_rpc_s_unknown_authn_level);
    case RPC_S_INVALID_AUTH_IDENTITY: return (syserr_rpc_s_invalid_auth_identity);
    case RPC_S_UNKNOWN_AUTHZ_SERVICE: return (syserr_rpc_s_unknown_authz_service);
    case EPT_S_INVALID_ENTRY: return (syserr_ept_s_invalid_entry);
    case EPT_S_CANT_PERFORM_OP: return (syserr_ept_s_cant_perform_op);
    case EPT_S_NOT_REGISTERED: return (syserr_ept_s_not_registered);
    case RPC_S_NOTHING_TO_EXPORT: return (syserr_rpc_s_nothing_to_export);
    case RPC_S_INCOMPLETE_NAME: return (syserr_rpc_s_incomplete_name);
    case RPC_S_INVALID_VERS_OPTION: return (syserr_rpc_s_invalid_vers_option);
    case RPC_S_NO_MORE_MEMBERS: return (syserr_rpc_s_no_more_members);
    case RPC_S_NOT_ALL_OBJS_UNEXPORTED: return (syserr_rpc_s_not_all_objs_unexported);
    case RPC_S_INTERFACE_NOT_FOUND: return (syserr_rpc_s_interface_not_found);
    case RPC_S_ENTRY_ALREADY_EXISTS: return (syserr_rpc_s_entry_already_exists);
    case RPC_S_ENTRY_NOT_FOUND: return (syserr_rpc_s_entry_not_found);
    case RPC_S_NAME_SERVICE_UNAVAILABLE: return (syserr_rpc_s_name_service_unavailable);
    case RPC_S_INVALID_NAF_ID: return (syserr_rpc_s_invalid_naf_id);
    case RPC_S_CANNOT_SUPPORT: return (syserr_rpc_s_cannot_support);
    case RPC_S_NO_CONTEXT_AVAILABLE: return (syserr_rpc_s_no_context_available);
    case RPC_S_INTERNAL_ERROR: return (syserr_rpc_s_internal_error);
    case RPC_S_ZERO_DIVIDE: return (syserr_rpc_s_zero_divide);
    case RPC_S_ADDRESS_ERROR: return (syserr_rpc_s_address_error);
    case RPC_S_FP_DIV_ZERO: return (syserr_rpc_s_fp_div_zero);
    case RPC_S_FP_UNDERFLOW: return (syserr_rpc_s_fp_underflow);
    case RPC_S_FP_OVERFLOW: return (syserr_rpc_s_fp_overflow);
    case RPC_X_NO_MORE_ENTRIES: return (syserr_rpc_x_no_more_entries);
    case RPC_X_SS_CHAR_TRANS_OPEN_FAIL: return (syserr_rpc_x_ss_char_trans_open_fail);
    case RPC_X_SS_CHAR_TRANS_SHORT_FILE: return (syserr_rpc_x_ss_char_trans_short_file);
    case RPC_X_SS_IN_NULL_CONTEXT: return (syserr_rpc_x_ss_in_null_context);
    case RPC_X_SS_CONTEXT_DAMAGED: return (syserr_rpc_x_ss_context_damaged);
    case RPC_X_SS_HANDLES_MISMATCH: return (syserr_rpc_x_ss_handles_mismatch);
    case RPC_X_SS_CANNOT_GET_CALL_HANDLE: return (syserr_rpc_x_ss_cannot_get_call_handle);
    case RPC_X_NULL_REF_POINTER: return (syserr_rpc_x_null_ref_pointer);
    case RPC_X_ENUM_VALUE_OUT_OF_RANGE: return (syserr_rpc_x_enum_value_out_of_range);
    case RPC_X_BYTE_COUNT_TOO_SMALL: return (syserr_rpc_x_byte_count_too_small);
    case RPC_X_BAD_STUB_DATA: return (syserr_rpc_x_bad_stub_data);
    case ERROR_INVALID_USER_BUFFER: return (syserr_invalid_user_buffer);
    case ERROR_UNRECOGNIZED_MEDIA: return (syserr_unrecognized_media);
    case ERROR_NO_TRUST_LSA_SECRET: return (syserr_no_trust_lsa_secret);
    case ERROR_NO_TRUST_SAM_ACCOUNT: return (syserr_no_trust_sam_account);
    case ERROR_TRUSTED_DOMAIN_FAILURE: return (syserr_trusted_domain_failure);
    case ERROR_TRUSTED_RELATIONSHIP_FAILURE: return (syserr_trusted_relationship_failure);
    case ERROR_TRUST_FAILURE: return (syserr_trust_failure);
    case RPC_S_CALL_IN_PROGRESS: return (syserr_rpc_s_call_in_progress);
    case ERROR_NETLOGON_NOT_STARTED: return (syserr_netlogon_not_started);
    case ERROR_ACCOUNT_EXPIRED: return (syserr_account_expired);
    case ERROR_REDIRECTOR_HAS_OPEN_HANDLES: return (syserr_redirector_has_open_handles);
    case ERROR_PRINTER_DRIVER_ALREADY_INSTALLED: return (syserr_printer_driver_already_installed);
    case ERROR_UNKNOWN_PORT: return (syserr_unknown_port);
    case ERROR_UNKNOWN_PRINTER_DRIVER: return (syserr_unknown_printer_driver);
    case ERROR_UNKNOWN_PRINTPROCESSOR: return (syserr_unknown_printprocessor);
    case ERROR_INVALID_SEPARATOR_FILE: return (syserr_invalid_separator_file);
    case ERROR_INVALID_PRIORITY: return (syserr_invalid_priority);
    case ERROR_INVALID_PRINTER_NAME: return (syserr_invalid_printer_name);
    case ERROR_PRINTER_ALREADY_EXISTS: return (syserr_printer_already_exists);
    case ERROR_INVALID_PRINTER_COMMAND: return (syserr_invalid_printer_command);
    case ERROR_INVALID_DATATYPE: return (syserr_invalid_datatype);
    case ERROR_INVALID_ENVIRONMENT: return (syserr_invalid_environment);
    case RPC_S_NO_MORE_BINDINGS: return (syserr_rpc_s_no_more_bindings);
    case ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT: return (syserr_nologon_interdomain_trust_account);
    case ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT: return (syserr_nologon_workstation_trust_account);
    case ERROR_NOLOGON_SERVER_TRUST_ACCOUNT: return (syserr_nologon_server_trust_account);
    case ERROR_DOMAIN_TRUST_INCONSISTENT: return (syserr_domain_trust_inconsistent);
    case ERROR_SERVER_HAS_OPEN_HANDLES: return (syserr_server_has_open_handles);
    case ERROR_RESOURCE_DATA_NOT_FOUND: return (syserr_resource_data_not_found);
    case ERROR_RESOURCE_TYPE_NOT_FOUND: return (syserr_resource_type_not_found);
    case ERROR_RESOURCE_NAME_NOT_FOUND: return (syserr_resource_name_not_found);
    case ERROR_RESOURCE_LANG_NOT_FOUND: return (syserr_resource_lang_not_found);
    case ERROR_NOT_ENOUGH_QUOTA: return (syserr_not_enough_quota);
    case RPC_S_NO_INTERFACES: return (syserr_rpc_s_no_interfaces);
    case RPC_S_CALL_CANCELLED: return (syserr_rpc_s_call_cancelled);
    case RPC_S_BINDING_INCOMPLETE: return (syserr_rpc_s_binding_incomplete);
    case RPC_S_COMM_FAILURE: return (syserr_rpc_s_comm_failure);
    case RPC_S_UNSUPPORTED_AUTHN_LEVEL: return (syserr_rpc_s_unsupported_authn_level);
    case RPC_S_NO_PRINC_NAME: return (syserr_rpc_s_no_princ_name);
    case RPC_S_NOT_RPC_ERROR: return (syserr_rpc_s_not_rpc_error);
    case RPC_S_UUID_LOCAL_ONLY: return (syserr_rpc_s_uuid_local_only);
    case RPC_S_SEC_PKG_ERROR: return (syserr_rpc_s_sec_pkg_error);
    case RPC_S_NOT_CANCELLED: return (syserr_rpc_s_not_cancelled);
    case RPC_X_INVALID_ES_ACTION: return (syserr_rpc_x_invalid_es_action);
    case RPC_X_WRONG_ES_VERSION: return (syserr_rpc_x_wrong_es_version);
    case RPC_X_WRONG_STUB_VERSION: return (syserr_rpc_x_wrong_stub_version);
    case RPC_S_GROUP_MEMBER_NOT_FOUND: return (syserr_rpc_s_group_member_not_found);
    case EPT_S_CANT_CREATE: return (syserr_ept_s_cant_create);
    case RPC_S_INVALID_OBJECT: return (syserr_rpc_s_invalid_object);
    case ERROR_INVALID_TIME: return (syserr_invalid_time);
    case ERROR_INVALID_FORM_NAME: return (syserr_invalid_form_name);
    case ERROR_INVALID_FORM_SIZE: return (syserr_invalid_form_size);
    case ERROR_ALREADY_WAITING: return (syserr_already_waiting);
    case ERROR_PRINTER_DELETED: return (syserr_printer_deleted);
    case ERROR_INVALID_PRINTER_STATE: return (syserr_invalid_printer_state);
    case ERROR_PASSWORD_MUST_CHANGE: return (syserr_password_must_change);
    case ERROR_DOMAIN_CONTROLLER_NOT_FOUND: return (syserr_domain_controller_not_found);
    case ERROR_ACCOUNT_LOCKED_OUT: return (syserr_account_locked_out);
    case ERROR_NO_BROWSER_SERVERS_FOUND: return (syserr_no_browser_servers_found);
    case ERROR_INVALID_PIXEL_FORMAT: return (syserr_invalid_pixel_format);
    case ERROR_BAD_DRIVER: return (syserr_bad_driver);
    case ERROR_INVALID_WINDOW_STYLE: return (syserr_invalid_window_style);
    case ERROR_METAFILE_NOT_SUPPORTED: return (syserr_metafile_not_supported);
    case ERROR_TRANSFORM_NOT_SUPPORTED: return (syserr_transform_not_supported);
    case ERROR_CLIPPING_NOT_SUPPORTED: return (syserr_clipping_not_supported);
    case ERROR_UNKNOWN_PRINT_MONITOR: return (syserr_unknown_print_monitor);
    case ERROR_PRINTER_DRIVER_IN_USE: return (syserr_printer_driver_in_use);
    case ERROR_SPOOL_FILE_NOT_FOUND: return (syserr_spool_file_not_found);
    case ERROR_SPL_NO_STARTDOC: return (syserr_spl_no_startdoc);
    case ERROR_SPL_NO_ADDJOB: return (syserr_spl_no_addjob);
    case ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED: return (syserr_print_processor_already_installed);
    case ERROR_PRINT_MONITOR_ALREADY_INSTALLED: return (syserr_print_monitor_already_installed);
    case ERROR_WINS_INTERNAL: return (syserr_wins_internal);
    case ERROR_CAN_NOT_DEL_LOCAL_WINS: return (syserr_can_not_del_local_wins);
    case ERROR_STATIC_INIT: return (syserr_static_init);
    case ERROR_INC_BACKUP: return (syserr_inc_backup);
    case ERROR_FULL_BACKUP: return (syserr_full_backup);
    case ERROR_REC_NON_EXISTENT: return (syserr_rec_non_existent);
    case ERROR_RPL_NOT_ALLOWED: return (syserr_rpl_not_allowed);

      /* Winsock error codes: */
    case WSAEINTR: return (syserr_wsaeintr);
    case WSAEBADF: return (syserr_wsaebadf);
    case WSAEACCES: return (syserr_wsaeacces);
    case WSAEFAULT: return (syserr_wsaefault);
    case WSAEINVAL: return (syserr_wsaeinval);
    case WSAEMFILE: return (syserr_wsaemfile);
    case WSAEWOULDBLOCK: return (syserr_wsaewouldblock);
    case WSAEINPROGRESS: return (syserr_wsaeinprogress);
    case WSAEALREADY: return (syserr_wsaealready);
    case WSAENOTSOCK: return (syserr_wsaenotsock);
    case WSAEDESTADDRREQ: return (syserr_wsaedestaddrreq);
    case WSAEMSGSIZE: return (syserr_wsaemsgsize);
    case WSAEPROTOTYPE: return (syserr_wsaeprototype);
    case WSAENOPROTOOPT: return (syserr_wsaenoprotoopt);
    case WSAEPROTONOSUPPORT: return (syserr_wsaeprotonosupport);
    case WSAESOCKTNOSUPPORT: return (syserr_wsaesocktnosupport);
    case WSAEOPNOTSUPP: return (syserr_wsaeopnotsupp);
    case WSAEPFNOSUPPORT: return (syserr_wsaepfnosupport);
    case WSAEAFNOSUPPORT: return (syserr_wsaeafnosupport);
    case WSAEADDRINUSE: return (syserr_wsaeaddrinuse);
    case WSAEADDRNOTAVAIL: return (syserr_wsaeaddrnotavail);
    case WSAENETDOWN: return (syserr_wsaenetdown);
    case WSAENETUNREACH: return (syserr_wsaenetunreach);
    case WSAENETRESET: return (syserr_wsaenetreset);
    case WSAECONNABORTED: return (syserr_wsaeconnaborted);
    case WSAECONNRESET: return (syserr_wsaeconnreset);
    case WSAENOBUFS: return (syserr_wsaenobufs);
    case WSAEISCONN: return (syserr_wsaeisconn);
    case WSAENOTCONN: return (syserr_wsaenotconn);
    case WSAESHUTDOWN: return (syserr_wsaeshutdown);
    case WSAETOOMANYREFS: return (syserr_wsaetoomanyrefs);
    case WSAETIMEDOUT: return (syserr_wsaetimedout);
    case WSAECONNREFUSED: return (syserr_wsaeconnrefused);
    case WSAELOOP: return (syserr_wsaeloop);
    case WSAENAMETOOLONG: return (syserr_wsaenametoolong);
    case WSAEHOSTDOWN: return (syserr_wsaehostdown);
    case WSAEHOSTUNREACH: return (syserr_wsaehostunreach);
    case WSAENOTEMPTY: return (syserr_wsaenotempty);
    case WSAEPROCLIM: return (syserr_wsaeproclim);
    case WSAEUSERS: return (syserr_wsaeusers);
    case WSAEDQUOT: return (syserr_wsaedquot);
    case WSAESTALE: return (syserr_wsaestale);
    case WSAEREMOTE: return (syserr_wsaeremote);
    case WSAEDISCON: return (syserr_wsaediscon);
    case WSASYSNOTREADY: return (syserr_wsasysnotready);
    case WSAVERNOTSUPPORTED: return (syserr_wsavernotsupported);
    case WSANOTINITIALISED: return (syserr_wsanotinitialised);
    case WSAHOST_NOT_FOUND: return (syserr_wsahost_not_found);
    case WSATRY_AGAIN: return (syserr_wsatry_again);
    case WSANO_RECOVERY: return (syserr_wsano_recovery);
    case WSANO_DATA: return (syserr_wsano_data);

    default: return (syserr_unknown);
    }
}
