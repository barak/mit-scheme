/* -*-C-*-

$Id: os2top.c,v 1.24 2007/01/05 15:33:07 cph Exp $

Copyright (c) 1994-2000 Massachusetts Institute of Technology

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

#define SCM_OS2TOP_C
#define INCL_WIN

#include "scheme.h"
#include "os2.h"
#include "ostop.h"
#include "option.h"

#ifndef DISABLE_SOCKET_SUPPORT
#  include <nerrno.h>
#endif

extern void execute_reload_cleanups (void);

extern void OS2_initialize_channels (void);
extern void OS2_initialize_channel_thread_messages (void);
extern void OS2_initialize_console (void);
extern void OS2_initialize_directory_reader (void);
extern void OS2_initialize_environment (void);
extern void OS2_initialize_exception_handling (void);
extern void OS2_initialize_keyboard_interrupts (void);
extern void OS2_initialize_malloc (void);
extern void OS2_initialize_message_queues (void);
extern void OS2_initialize_pm_thread (void);
extern void OS2_initialize_processes (void);
extern void OS2_initialize_scheme_thread (void);
extern void OS2_initialize_tty (void);
extern void OS2_initialize_window_primitives (void);

extern void OS2_check_message_length_initializations (void);
extern void * OS2_malloc_noerror (unsigned int);
extern void * OS2_realloc_noerror (void *, unsigned int);

extern void OS2_create_msg_queue (void); /* forward reference */

extern const char * OS_Name;
extern const char * OS_Variant;
extern HMTX OS2_create_queue_lock;

static const char * OS2_version_string (void);
static void initialize_locks (void);

static int initialization_completed = 0;

int
OS_under_emacs_p (void)
{
  return (option_emacs_subprocess);
}

void
OS2_initialize_early (void)
{
  initialization_completed = 0;
  OS2_initialize_malloc ();
  initialize_locks ();
  OS2_create_msg_queue ();
}

void
OS_initialize (void)
{
  (void) DosError (FERR_DISABLEEXCEPTION | FERR_DISABLEHARDERR);
  OS2_initialize_exception_handling ();
  OS2_initialize_message_queues ();
  OS2_initialize_scheme_thread ();
  OS2_initialize_pm_thread ();
  OS2_initialize_channels ();
  OS2_initialize_channel_thread_messages ();
  OS2_initialize_keyboard_interrupts ();
  OS2_initialize_console ();
  OS2_initialize_environment ();
  OS2_initialize_directory_reader ();
  OS2_initialize_tty ();
  OS2_initialize_window_primitives ();
  OS2_initialize_processes ();
  initialization_completed = 1;
  /* This must be after all of the initializations that can set
     message lengths.  */
  OS2_check_message_length_initializations ();
  OS_Name = "OS/2";
  {
    const char * version = (OS2_version_string ());
    OS_Variant = (OS_malloc ((strlen (OS_Name)) + (strlen (version)) + 2));
    sprintf (((char *) OS_Variant), "%s %s", OS_Name, version);
  }
}

void
OS_announcement (void)
{
}

static const char *
OS2_version_string (void)
{
  ULONG major = (OS2_system_variable (QSV_VERSION_MAJOR));
  ULONG minor = (OS2_system_variable (QSV_VERSION_MINOR));
  char revision = (OS2_system_variable (QSV_VERSION_REVISION));
  static char result [64];
  char sminor [16];
  char srev [2];
  if ((major == 20) && (minor >= 30))
    {
      major = (minor - (minor % 10));
      minor = ((minor % 10) * 10);
    }
  if ((minor < 10) && (minor != 0))
    sprintf (sminor, "0%d", minor);
  else
    sprintf (sminor, "%d",
	     (((minor < 100) && ((minor % 10) == 0)) ? (minor / 10) : minor));
  if (revision == '\0')
    sprintf (srev, "");
  else
    sprintf (srev, "%c", revision);
  sprintf (result, "%d.%s%s", (major / 10), sminor, srev);
  return (result);
}

#define PAGESIZE 4096
#define PAGE_PERMS (PAG_READ | PAG_WRITE | PAG_EXECUTE)
#define N_STACK_GUARD_PAGES 2

#define ROUND_UP_TO_PAGE(size)						\
  (((((size) + (PAGESIZE - 1)) / PAGESIZE) + N_STACK_GUARD_PAGES + 1)	\
   * PAGESIZE)

typedef struct
{
  char * low;
  unsigned int enabled_p : 1;
} guard_page_state_t;

static guard_page_state_t guard_page_states [N_STACK_GUARD_PAGES];

static void *
commit_heap_helper (void * base, unsigned long size)
{
  /* Complicated arrangement to detect stack overflow with reasonable
     reliability.  We allocate three extra pages past the end of the
     stack; the first two (adjacent to the stack) are committed as
     guard pages so that OS/2 will deliver an exception when we access
     them.  If we overrun the first guard page, the trap handler
     should recognize this and terminate Scheme gracefully, using the
     second guard page as its stack.  The third page, on the other
     side of the guard pages, is uncommitted -- if for some reason we
     overrun the second guard page, this uncommitted page will cause a
     hard fault that will kill Scheme right away.

     This is slightly kludgey, because we take advantage of the fact
     that the Scheme stack occupies the low-order addresses in the
     allocated block, and particularly that the stack grows towards
     lower addresses.  Thus we can put the guard pages just below the
     allocated block.  If the memory layout is altered, this will have
     to change.  The reason for this fragile implementation is that it
     requires the least change to the existing memory allocation
     mechanism.  */
  char * p = base;
  /* Skip uncommitted page, then commit rest of memory block.  */
  p += PAGESIZE;
  if ((dos_set_mem (p, (size - PAGESIZE), (PAG_COMMIT | PAG_DEFAULT)))
      != NO_ERROR)
    return (0);
  /* Initialize the stack guard pages and get pointer to first page
     past the guard pages.  */
  {
    guard_page_state_t * scan = guard_page_states;
    guard_page_state_t * end = (scan + N_STACK_GUARD_PAGES);
    while (scan < end)
      {
	(scan -> low) = p;
	(scan -> enabled_p) = 0;
	scan += 1;
	p += PAGESIZE;
      }
    OS2_stack_reset ();
  }
  return (p);
}

static void
enable_stack_guard (guard_page_state_t * page, int enable_p)
{
  (void) dos_set_mem ((page -> low),
		      PAGESIZE,
		      (enable_p ? (PAGE_PERMS | PAG_GUARD) : PAGE_PERMS));
  (page -> enabled_p) = enable_p;
}

int
OS2_disable_stack_guard (void * p)
{
  char * cp = p;
  guard_page_state_t * scan = guard_page_states;
  guard_page_state_t * end = (scan + N_STACK_GUARD_PAGES);
  while (1)
    {
      if (scan == end)
	return (0);
      if (((scan -> low) <= cp) && (cp < ((scan -> low) + PAGESIZE)))
	{
	  enable_stack_guard (scan, 0);
	  return (1);
	}
      scan += 1;
    }
}

void
OS2_stack_reset (void)
{
  {
    guard_page_state_t * scan = guard_page_states;
    guard_page_state_t * end = (scan + N_STACK_GUARD_PAGES);
    while (1)
      {
	if (scan == end)
	  return;
	if (! (scan -> enabled_p))
	  break;
	scan += 1;
      }
  }
  enable_stack_guard ((&guard_page_states[1]), 0);
  {
    SCHEME_OBJECT * p = ((SCHEME_OBJECT *) ((guard_page_states[1]) . low));
    (*p) = (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, p));
  }
  {
    guard_page_state_t * scan = guard_page_states;
    guard_page_state_t * end = (scan + N_STACK_GUARD_PAGES);
    while (scan < end)
      enable_stack_guard ((scan++), 1);
  }
}

int
OS2_stack_overflowed_p (void)
{
  SCHEME_OBJECT * p = ((SCHEME_OBJECT *) ((guard_page_states[1]) . low));
  return
    ((! ((guard_page_states[1]) . enabled_p))
     && ((*p) != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, p))));
}

#if 0

/* This is an attempt to allocate Scheme's memory as early as
   possible, in order to obtain the lowest possible addresses before
   `malloc' grabs them for some uninteresting purpose.  However, there
   are two reasons not to do this: first, it doesn't seem to gain any
   advantage at present (in OS/2 Warp 3.0 with C Set++/2 2.1), because
   the returned addresses are about the same in both cases.  Second,
   this sometimes causes a fatal error in the debugger, apparently
   because it cares about how much memory the debugged process has
   allocated, even if it's not committed.  */

static void * OS2_heap_base;

void
OS2_alloc_heap (void)
{
  APIRET rc
    = (dos_alloc_mem ((& OS2_heap_base),
		      0x04000000,
		      (PAG_EXECUTE | PAG_READ | PAG_WRITE)));
  if (rc != NO_ERROR)
    {
      fprintf (stderr, "Can't allocate heap memory.");
      fflush (stderr);
      exit (EXIT_FAILURE);
    }
}

void *
OS2_commit_heap (unsigned long size)
{
  return (commit_heap_helper (OS2_heap_base, (ROUND_UP_TO_PAGE (size))));
}

#else

void
OS2_alloc_heap (void)
{
}

void *
OS2_commit_heap (unsigned long size)
{
  unsigned long actual = (ROUND_UP_TO_PAGE (size));
  void * heap_base;
  APIRET rc
    = (dos_alloc_mem ((& heap_base),
		      actual,
		      (PAG_EXECUTE | PAG_READ | PAG_WRITE)));
  return ((rc == NO_ERROR) ? (commit_heap_helper (heap_base, actual)) : 0);
}

#endif

void
OS2_create_msg_queue (void)
{
  /* Create a PM message queue.  This allows us to use message boxes
     to report fatal errors.  */
  HAB hab = (WinInitialize (0));
  HMQ hmq;
  if (hab == NULLHANDLE)
    OS2_logic_error ("Unable to initialize anchor block.");
  hmq = (WinCreateMsgQueue (hab, 0));
  if (hmq == NULLHANDLE)
    OS2_logic_error ("Unable to create PM message queue.");
  /* This tells the system that this message queue should not receive
     WM_QUIT messages.  */
  WinCancelShutdown (hmq, TRUE);
}

void
OS2_message_box (const char * title, const char * message, int errorp)
{
  (void) WinMessageBox (HWND_DESKTOP,
			NULLHANDLE,
			((PSZ) message),
			((PSZ) title),
			0,
			(MB_OK | (errorp ? MB_ERROR : MB_WARNING)));
}

void
OS2_exit_scheme (int value)
{
  if (initialization_completed)
    {
#if 0
      OS2_channel_close_all_noerror ();
#endif
    }
  exit (value);
}

void
OS_reset (void)
{
  execute_reload_cleanups ();
}

void
OS_quit (int code, int abnormal_p)
{
  outf_flush_console ();
  OS_restore_external_state ();
}

void
OS_save_external_state (void)
{
}

void
OS_save_internal_state (void)
{
}

void
OS_restore_internal_state (void)
{
}

void
OS_restore_external_state (void)
{
}

void
preserve_signal_mask (void)
{
}

void
block_signals (void)
{
}

void
unblock_signals (void)
{
}

void
OS_restartable_exit (void)
{
}

#if defined(__IBMC__) || defined(__WATCOMC__)
void
bcopy (const char * from, char * to, unsigned int n)
{
  FASTCOPY (from, to, n);
}
#endif

static HMTX interrupt_registers_lock;

static void
initialize_locks (void)
{
  interrupt_registers_lock = (OS2_create_mutex_semaphore (0, 0));
  OS2_create_queue_lock = (OS2_create_mutex_semaphore (0, 0));
}

void
OS_grab_interrupt_registers (void)
{
  OS2_request_mutex_semaphore (interrupt_registers_lock);
}

void
OS_release_interrupt_registers (void)
{
  OS2_release_mutex_semaphore (interrupt_registers_lock);
}

/* Machine-generated procedure, do not edit: */
enum syserr_names
OS_error_code_to_syserr (int code)
{
  switch (code)
    {
    case ERROR_INVALID_FUNCTION:	return (syserr_invalid_function);
    case ERROR_FILE_NOT_FOUND:	return (syserr_file_not_found);
    case ERROR_PATH_NOT_FOUND:	return (syserr_path_not_found);
    case ERROR_TOO_MANY_OPEN_FILES:	return (syserr_too_many_open_files);
    case ERROR_ACCESS_DENIED:	return (syserr_access_denied);
    case ERROR_INVALID_HANDLE:	return (syserr_invalid_handle);
    case ERROR_ARENA_TRASHED:	return (syserr_arena_trashed);
    case ERROR_NOT_ENOUGH_MEMORY:	return (syserr_not_enough_memory);
    case ERROR_INVALID_BLOCK:	return (syserr_invalid_block);
    case ERROR_BAD_ENVIRONMENT:	return (syserr_bad_environment);
    case ERROR_BAD_FORMAT:	return (syserr_bad_format);
    case ERROR_INVALID_ACCESS:	return (syserr_invalid_access);
    case ERROR_INVALID_DATA:	return (syserr_invalid_data);
    case ERROR_INVALID_DRIVE:	return (syserr_invalid_drive);
    case ERROR_CURRENT_DIRECTORY:	return (syserr_current_directory);
    case ERROR_NOT_SAME_DEVICE:	return (syserr_not_same_device);
    case ERROR_NO_MORE_FILES:	return (syserr_no_more_files);
    case ERROR_WRITE_PROTECT:	return (syserr_write_protect);
    case ERROR_BAD_UNIT:	return (syserr_bad_unit);
    case ERROR_NOT_READY:	return (syserr_not_ready);
    case ERROR_BAD_COMMAND:	return (syserr_bad_command);
    case ERROR_CRC:	return (syserr_crc);
    case ERROR_BAD_LENGTH:	return (syserr_bad_length);
    case ERROR_SEEK:	return (syserr_seek);
    case ERROR_NOT_DOS_DISK:	return (syserr_not_dos_disk);
    case ERROR_SECTOR_NOT_FOUND:	return (syserr_sector_not_found);
    case ERROR_OUT_OF_PAPER:	return (syserr_out_of_paper);
    case ERROR_WRITE_FAULT:	return (syserr_write_fault);
    case ERROR_READ_FAULT:	return (syserr_read_fault);
    case ERROR_GEN_FAILURE:	return (syserr_gen_failure);
    case ERROR_SHARING_VIOLATION:	return (syserr_sharing_violation);
    case ERROR_LOCK_VIOLATION:	return (syserr_lock_violation);
    case ERROR_WRONG_DISK:	return (syserr_wrong_disk);
    case ERROR_FCB_UNAVAILABLE:	return (syserr_fcb_unavailable);
    case ERROR_SHARING_BUFFER_EXCEEDED:	return (syserr_sharing_buffer_exceeded);
    case ERROR_CODE_PAGE_MISMATCHED:	return (syserr_code_page_mismatched);
    case ERROR_HANDLE_EOF:	return (syserr_handle_eof);
    case ERROR_HANDLE_DISK_FULL:	return (syserr_handle_disk_full);
    case ERROR_NOT_SUPPORTED:	return (syserr_not_supported);
    case ERROR_REM_NOT_LIST:	return (syserr_rem_not_list);
    case ERROR_DUP_NAME:	return (syserr_dup_name);
    case ERROR_BAD_NETPATH:	return (syserr_bad_netpath);
    case ERROR_NETWORK_BUSY:	return (syserr_network_busy);
    case ERROR_DEV_NOT_EXIST:	return (syserr_dev_not_exist);
    case ERROR_TOO_MANY_CMDS:	return (syserr_too_many_cmds);
    case ERROR_ADAP_HDW_ERR:	return (syserr_adap_hdw_err);
    case ERROR_BAD_NET_RESP:	return (syserr_bad_net_resp);
    case ERROR_UNEXP_NET_ERR:	return (syserr_unexp_net_err);
    case ERROR_BAD_REM_ADAP:	return (syserr_bad_rem_adap);
    case ERROR_PRINTQ_FULL:	return (syserr_printq_full);
    case ERROR_NO_SPOOL_SPACE:	return (syserr_no_spool_space);
    case ERROR_PRINT_CANCELLED:	return (syserr_print_cancelled);
    case ERROR_NETNAME_DELETED:	return (syserr_netname_deleted);
    case ERROR_NETWORK_ACCESS_DENIED:	return (syserr_network_access_denied);
    case ERROR_BAD_DEV_TYPE:	return (syserr_bad_dev_type);
    case ERROR_BAD_NET_NAME:	return (syserr_bad_net_name);
    case ERROR_TOO_MANY_NAMES:	return (syserr_too_many_names);
    case ERROR_TOO_MANY_SESS:	return (syserr_too_many_sess);
    case ERROR_SHARING_PAUSED:	return (syserr_sharing_paused);
    case ERROR_REQ_NOT_ACCEP:	return (syserr_req_not_accep);
    case ERROR_REDIR_PAUSED:	return (syserr_redir_paused);
    case ERROR_SBCS_ATT_WRITE_PROT:	return (syserr_sbcs_att_write_prot);
    case ERROR_SBCS_GENERAL_FAILURE:	return (syserr_sbcs_general_failure);
    case ERROR_XGA_OUT_MEMORY:	return (syserr_xga_out_memory);
    case ERROR_FILE_EXISTS:	return (syserr_file_exists);
    case ERROR_DUP_FCB:	return (syserr_dup_fcb);
    case ERROR_CANNOT_MAKE:	return (syserr_cannot_make);
    case ERROR_FAIL_I24:	return (syserr_fail_i24);
    case ERROR_OUT_OF_STRUCTURES:	return (syserr_out_of_structures);
    case ERROR_ALREADY_ASSIGNED:	return (syserr_already_assigned);
    case ERROR_INVALID_PASSWORD:	return (syserr_invalid_password);
    case ERROR_INVALID_PARAMETER:	return (syserr_invalid_parameter);
    case ERROR_NET_WRITE_FAULT:	return (syserr_net_write_fault);
    case ERROR_NO_PROC_SLOTS:	return (syserr_no_proc_slots);
    case ERROR_NOT_FROZEN:	return (syserr_not_frozen);
    case ERR_TSTOVFL:	return (syserr_tstovfl);
    case ERR_TSTDUP:	return (syserr_tstdup);
    case ERROR_NO_ITEMS:	return (syserr_no_items);
    case ERROR_INTERRUPT:	return (syserr_interrupt);
    case ERROR_DEVICE_IN_USE:	return (syserr_device_in_use);
    case ERROR_TOO_MANY_SEMAPHORES:	return (syserr_too_many_semaphores);
    case ERROR_EXCL_SEM_ALREADY_OWNED:	return (syserr_excl_sem_already_owned);
    case ERROR_SEM_IS_SET:	return (syserr_sem_is_set);
    case ERROR_TOO_MANY_SEM_REQUESTS:	return (syserr_too_many_sem_requests);
    case ERROR_INVALID_AT_INTERRUPT_TIME:	return (syserr_invalid_at_interrupt_time);
    case ERROR_SEM_OWNER_DIED:	return (syserr_sem_owner_died);
    case ERROR_SEM_USER_LIMIT:	return (syserr_sem_user_limit);
    case ERROR_DISK_CHANGE:	return (syserr_disk_change);
    case ERROR_DRIVE_LOCKED:	return (syserr_drive_locked);
    case ERROR_BROKEN_PIPE:	return (syserr_broken_pipe);
    case ERROR_OPEN_FAILED:	return (syserr_open_failed);
    case ERROR_BUFFER_OVERFLOW:	return (syserr_buffer_overflow);
    case ERROR_DISK_FULL:	return (syserr_disk_full);
    case ERROR_NO_MORE_SEARCH_HANDLES:	return (syserr_no_more_search_handles);
    case ERROR_INVALID_TARGET_HANDLE:	return (syserr_invalid_target_handle);
    case ERROR_PROTECTION_VIOLATION:	return (syserr_protection_violation);
    case ERROR_VIOKBD_REQUEST:	return (syserr_viokbd_request);
    case ERROR_INVALID_CATEGORY:	return (syserr_invalid_category);
    case ERROR_INVALID_VERIFY_SWITCH:	return (syserr_invalid_verify_switch);
    case ERROR_BAD_DRIVER_LEVEL:	return (syserr_bad_driver_level);
    case ERROR_CALL_NOT_IMPLEMENTED:	return (syserr_call_not_implemented);
    case ERROR_SEM_TIMEOUT:	return (syserr_sem_timeout);
    case ERROR_INSUFFICIENT_BUFFER:	return (syserr_insufficient_buffer);
    case ERROR_INVALID_NAME:	return (syserr_invalid_name);
    case ERROR_INVALID_LEVEL:	return (syserr_invalid_level);
    case ERROR_NO_VOLUME_LABEL:	return (syserr_no_volume_label);
    case ERROR_MOD_NOT_FOUND:	return (syserr_mod_not_found);
    case ERROR_PROC_NOT_FOUND:	return (syserr_proc_not_found);
    case ERROR_WAIT_NO_CHILDREN:	return (syserr_wait_no_children);
    case ERROR_CHILD_NOT_COMPLETE:	return (syserr_child_not_complete);
    case ERROR_DIRECT_ACCESS_HANDLE:	return (syserr_direct_access_handle);
    case ERROR_NEGATIVE_SEEK:	return (syserr_negative_seek);
    case ERROR_SEEK_ON_DEVICE:	return (syserr_seek_on_device);
    case ERROR_IS_JOIN_TARGET:	return (syserr_is_join_target);
    case ERROR_IS_JOINED:	return (syserr_is_joined);
    case ERROR_IS_SUBSTED:	return (syserr_is_substed);
    case ERROR_NOT_JOINED:	return (syserr_not_joined);
    case ERROR_NOT_SUBSTED:	return (syserr_not_substed);
    case ERROR_JOIN_TO_JOIN:	return (syserr_join_to_join);
    case ERROR_SUBST_TO_SUBST:	return (syserr_subst_to_subst);
    case ERROR_JOIN_TO_SUBST:	return (syserr_join_to_subst);
    case ERROR_SUBST_TO_JOIN:	return (syserr_subst_to_join);
    case ERROR_BUSY_DRIVE:	return (syserr_busy_drive);
    case ERROR_SAME_DRIVE:	return (syserr_same_drive);
    case ERROR_DIR_NOT_ROOT:	return (syserr_dir_not_root);
    case ERROR_DIR_NOT_EMPTY:	return (syserr_dir_not_empty);
    case ERROR_IS_SUBST_PATH:	return (syserr_is_subst_path);
    case ERROR_IS_JOIN_PATH:	return (syserr_is_join_path);
    case ERROR_PATH_BUSY:	return (syserr_path_busy);
    case ERROR_IS_SUBST_TARGET:	return (syserr_is_subst_target);
    case ERROR_SYSTEM_TRACE:	return (syserr_system_trace);
    case ERROR_INVALID_EVENT_COUNT:	return (syserr_invalid_event_count);
    case ERROR_TOO_MANY_MUXWAITERS:	return (syserr_too_many_muxwaiters);
    case ERROR_INVALID_LIST_FORMAT:	return (syserr_invalid_list_format);
    case ERROR_LABEL_TOO_LONG:	return (syserr_label_too_long);
    case ERROR_TOO_MANY_TCBS:	return (syserr_too_many_tcbs);
    case ERROR_SIGNAL_REFUSED:	return (syserr_signal_refused);
    case ERROR_DISCARDED:	return (syserr_discarded);
    case ERROR_NOT_LOCKED:	return (syserr_not_locked);
    case ERROR_BAD_THREADID_ADDR:	return (syserr_bad_threadid_addr);
    case ERROR_BAD_ARGUMENTS:	return (syserr_bad_arguments);
    case ERROR_BAD_PATHNAME:	return (syserr_bad_pathname);
    case ERROR_SIGNAL_PENDING:	return (syserr_signal_pending);
    case ERROR_UNCERTAIN_MEDIA:	return (syserr_uncertain_media);
    case ERROR_MAX_THRDS_REACHED:	return (syserr_max_thrds_reached);
    case ERROR_MONITORS_NOT_SUPPORTED:	return (syserr_monitors_not_supported);
    case ERROR_UNC_DRIVER_NOT_INSTALLED:	return (syserr_unc_driver_not_installed);
    case ERROR_LOCK_FAILED:	return (syserr_lock_failed);
    case ERROR_SWAPIO_FAILED:	return (syserr_swapio_failed);
    case ERROR_SWAPIN_FAILED:	return (syserr_swapin_failed);
    case ERROR_BUSY:	return (syserr_busy);
    case ERROR_CANCEL_VIOLATION:	return (syserr_cancel_violation);
    case ERROR_ATOMIC_LOCK_NOT_SUPPORTED:	return (syserr_atomic_lock_not_supported);
    case ERROR_READ_LOCKS_NOT_SUPPORTED:	return (syserr_read_locks_not_supported);
    case ERROR_INVALID_SEGMENT_NUMBER:	return (syserr_invalid_segment_number);
    case ERROR_INVALID_CALLGATE:	return (syserr_invalid_callgate);
    case ERROR_INVALID_ORDINAL:	return (syserr_invalid_ordinal);
    case ERROR_ALREADY_EXISTS:	return (syserr_already_exists);
    case ERROR_NO_CHILD_PROCESS:	return (syserr_no_child_process);
    case ERROR_CHILD_ALIVE_NOWAIT:	return (syserr_child_alive_nowait);
    case ERROR_INVALID_FLAG_NUMBER:	return (syserr_invalid_flag_number);
    case ERROR_SEM_NOT_FOUND:	return (syserr_sem_not_found);
    case ERROR_INVALID_STARTING_CODESEG:	return (syserr_invalid_starting_codeseg);
    case ERROR_INVALID_STACKSEG:	return (syserr_invalid_stackseg);
    case ERROR_INVALID_MODULETYPE:	return (syserr_invalid_moduletype);
    case ERROR_INVALID_EXE_SIGNATURE:	return (syserr_invalid_exe_signature);
    case ERROR_EXE_MARKED_INVALID:	return (syserr_exe_marked_invalid);
    case ERROR_BAD_EXE_FORMAT:	return (syserr_bad_exe_format);
#ifdef ERROR_ITERATED_DATA_EXCEEDS_64k
    case ERROR_ITERATED_DATA_EXCEEDS_64k:	return (syserr_iterated_data_exceeds_64k);
#endif
    case ERROR_INVALID_MINALLOCSIZE:	return (syserr_invalid_minallocsize);
    case ERROR_DYNLINK_FROM_INVALID_RING:	return (syserr_dynlink_from_invalid_ring);
    case ERROR_IOPL_NOT_ENABLED:	return (syserr_iopl_not_enabled);
    case ERROR_INVALID_SEGDPL:	return (syserr_invalid_segdpl);
#ifdef ERROR_AUTODATASEG_EXCEEDS_64k
    case ERROR_AUTODATASEG_EXCEEDS_64k:	return (syserr_autodataseg_exceeds_64k);
#endif
    case ERROR_RING2SEG_MUST_BE_MOVABLE:	return (syserr_ring2seg_must_be_movable);
#ifdef ERROR_RELOC_CHAIN_XEEDS_SEGLIM
    case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:	return (syserr_reloc_chain_xeeds_seglim);
#endif
    case ERROR_INFLOOP_IN_RELOC_CHAIN:	return (syserr_infloop_in_reloc_chain);
    case ERROR_ENVVAR_NOT_FOUND:	return (syserr_envvar_not_found);
    case ERROR_NOT_CURRENT_CTRY:	return (syserr_not_current_ctry);
    case ERROR_NO_SIGNAL_SENT:	return (syserr_no_signal_sent);
    case ERROR_FILENAME_EXCED_RANGE:	return (syserr_filename_exced_range);
    case ERROR_RING2_STACK_IN_USE:	return (syserr_ring2_stack_in_use);
    case ERROR_META_EXPANSION_TOO_LONG:	return (syserr_meta_expansion_too_long);
    case ERROR_INVALID_SIGNAL_NUMBER:	return (syserr_invalid_signal_number);
    case ERROR_THREAD_1_INACTIVE:	return (syserr_thread_1_inactive);
    case ERROR_INFO_NOT_AVAIL:	return (syserr_info_not_avail);
    case ERROR_LOCKED:	return (syserr_locked);
    case ERROR_BAD_DYNALINK:	return (syserr_bad_dynalink);
    case ERROR_TOO_MANY_MODULES:	return (syserr_too_many_modules);
    case ERROR_NESTING_NOT_ALLOWED:	return (syserr_nesting_not_allowed);
    case ERROR_CANNOT_SHRINK:	return (syserr_cannot_shrink);
    case ERROR_ZOMBIE_PROCESS:	return (syserr_zombie_process);
    case ERROR_STACK_IN_HIGH_MEMORY:	return (syserr_stack_in_high_memory);
    case ERROR_INVALID_EXITROUTINE_RING:	return (syserr_invalid_exitroutine_ring);
    case ERROR_GETBUF_FAILED:	return (syserr_getbuf_failed);
    case ERROR_FLUSHBUF_FAILED:	return (syserr_flushbuf_failed);
    case ERROR_TRANSFER_TOO_LONG:	return (syserr_transfer_too_long);
    case ERROR_FORCENOSWAP_FAILED:	return (syserr_forcenoswap_failed);
    case ERROR_SMG_NO_TARGET_WINDOW:	return (syserr_smg_no_target_window);
    case ERROR_NO_CHILDREN:	return (syserr_no_children);
    case ERROR_INVALID_SCREEN_GROUP:	return (syserr_invalid_screen_group);
    case ERROR_BAD_PIPE:	return (syserr_bad_pipe);
    case ERROR_PIPE_BUSY:	return (syserr_pipe_busy);
    case ERROR_NO_DATA:	return (syserr_no_data);
    case ERROR_PIPE_NOT_CONNECTED:	return (syserr_pipe_not_connected);
    case ERROR_MORE_DATA:	return (syserr_more_data);
    case ERROR_VC_DISCONNECTED:	return (syserr_vc_disconnected);
    case ERROR_CIRCULARITY_REQUESTED:	return (syserr_circularity_requested);
    case ERROR_DIRECTORY_IN_CDS:	return (syserr_directory_in_cds);
    case ERROR_INVALID_FSD_NAME:	return (syserr_invalid_fsd_name);
    case ERROR_INVALID_PATH:	return (syserr_invalid_path);
    case ERROR_INVALID_EA_NAME:	return (syserr_invalid_ea_name);
    case ERROR_EA_LIST_INCONSISTENT:	return (syserr_ea_list_inconsistent);
    case ERROR_EA_LIST_TOO_LONG:	return (syserr_ea_list_too_long);
    case ERROR_NO_META_MATCH:	return (syserr_no_meta_match);
    case ERROR_FINDNOTIFY_TIMEOUT:	return (syserr_findnotify_timeout);
    case ERROR_NO_MORE_ITEMS:	return (syserr_no_more_items);
    case ERROR_SEARCH_STRUC_REUSED:	return (syserr_search_struc_reused);
    case ERROR_CHAR_NOT_FOUND:	return (syserr_char_not_found);
    case ERROR_TOO_MUCH_STACK:	return (syserr_too_much_stack);
    case ERROR_INVALID_ATTR:	return (syserr_invalid_attr);
    case ERROR_INVALID_STARTING_RING:	return (syserr_invalid_starting_ring);
    case ERROR_INVALID_DLL_INIT_RING:	return (syserr_invalid_dll_init_ring);
    case ERROR_CANNOT_COPY:	return (syserr_cannot_copy);
    case ERROR_DIRECTORY:	return (syserr_directory);
    case ERROR_OPLOCKED_FILE:	return (syserr_oplocked_file);
    case ERROR_OPLOCK_THREAD_EXISTS:	return (syserr_oplock_thread_exists);
    case ERROR_VOLUME_CHANGED:	return (syserr_volume_changed);
    case ERROR_FINDNOTIFY_HANDLE_IN_USE:	return (syserr_findnotify_handle_in_use);
    case ERROR_FINDNOTIFY_HANDLE_CLOSED:	return (syserr_findnotify_handle_closed);
    case ERROR_NOTIFY_OBJECT_REMOVED:	return (syserr_notify_object_removed);
    case ERROR_ALREADY_SHUTDOWN:	return (syserr_already_shutdown);
    case ERROR_EAS_DIDNT_FIT:	return (syserr_eas_didnt_fit);
    case ERROR_EA_FILE_CORRUPT:	return (syserr_ea_file_corrupt);
    case ERROR_EA_TABLE_FULL:	return (syserr_ea_table_full);
    case ERROR_INVALID_EA_HANDLE:	return (syserr_invalid_ea_handle);
    case ERROR_NO_CLUSTER:	return (syserr_no_cluster);
    case ERROR_CREATE_EA_FILE:	return (syserr_create_ea_file);
    case ERROR_CANNOT_OPEN_EA_FILE:	return (syserr_cannot_open_ea_file);
    case ERROR_EAS_NOT_SUPPORTED:	return (syserr_eas_not_supported);
    case ERROR_NEED_EAS_FOUND:	return (syserr_need_eas_found);
    case ERROR_DUPLICATE_HANDLE:	return (syserr_duplicate_handle);
    case ERROR_DUPLICATE_NAME:	return (syserr_duplicate_name);
    case ERROR_EMPTY_MUXWAIT:	return (syserr_empty_muxwait);
    case ERROR_MUTEX_OWNED:	return (syserr_mutex_owned);
    case ERROR_NOT_OWNER:	return (syserr_not_owner);
    case ERROR_PARAM_TOO_SMALL:	return (syserr_param_too_small);
    case ERROR_TOO_MANY_HANDLES:	return (syserr_too_many_handles);
    case ERROR_TOO_MANY_OPENS:	return (syserr_too_many_opens);
    case ERROR_WRONG_TYPE:	return (syserr_wrong_type);
    case ERROR_UNUSED_CODE:	return (syserr_unused_code);
    case ERROR_THREAD_NOT_TERMINATED:	return (syserr_thread_not_terminated);
    case ERROR_INIT_ROUTINE_FAILED:	return (syserr_init_routine_failed);
    case ERROR_MODULE_IN_USE:	return (syserr_module_in_use);
    case ERROR_NOT_ENOUGH_WATCHPOINTS:	return (syserr_not_enough_watchpoints);
    case ERROR_TOO_MANY_POSTS:	return (syserr_too_many_posts);
    case ERROR_ALREADY_POSTED:	return (syserr_already_posted);
    case ERROR_ALREADY_RESET:	return (syserr_already_reset);
    case ERROR_SEM_BUSY:	return (syserr_sem_busy);
    case ERROR_INVALID_PROCID:	return (syserr_invalid_procid);
    case ERROR_INVALID_PDELTA:	return (syserr_invalid_pdelta);
    case ERROR_NOT_DESCENDANT:	return (syserr_not_descendant);
    case ERROR_NOT_SESSION_MANAGER:	return (syserr_not_session_manager);
    case ERROR_INVALID_PCLASS:	return (syserr_invalid_pclass);
    case ERROR_INVALID_SCOPE:	return (syserr_invalid_scope);
    case ERROR_INVALID_THREADID:	return (syserr_invalid_threadid);
    case ERROR_DOSSUB_SHRINK:	return (syserr_dossub_shrink);
    case ERROR_DOSSUB_NOMEM:	return (syserr_dossub_nomem);
    case ERROR_DOSSUB_OVERLAP:	return (syserr_dossub_overlap);
    case ERROR_DOSSUB_BADSIZE:	return (syserr_dossub_badsize);
    case ERROR_DOSSUB_BADFLAG:	return (syserr_dossub_badflag);
    case ERROR_DOSSUB_BADSELECTOR:	return (syserr_dossub_badselector);
    case ERROR_MR_MSG_TOO_LONG:	return (syserr_mr_msg_too_long);
    case ERROR_MR_MID_NOT_FOUND:	return (syserr_mr_mid_not_found);
    case ERROR_MR_UN_ACC_MSGF:	return (syserr_mr_un_acc_msgf);
    case ERROR_MR_INV_MSGF_FORMAT:	return (syserr_mr_inv_msgf_format);
    case ERROR_MR_INV_IVCOUNT:	return (syserr_mr_inv_ivcount);
    case ERROR_MR_UN_PERFORM:	return (syserr_mr_un_perform);
    case ERROR_TS_WAKEUP:	return (syserr_ts_wakeup);
    case ERROR_TS_SEMHANDLE:	return (syserr_ts_semhandle);
    case ERROR_TS_NOTIMER:	return (syserr_ts_notimer);
    case ERROR_TS_HANDLE:	return (syserr_ts_handle);
    case ERROR_TS_DATETIME:	return (syserr_ts_datetime);
    case ERROR_SYS_INTERNAL:	return (syserr_sys_internal);
    case ERROR_QUE_CURRENT_NAME:	return (syserr_que_current_name);
    case ERROR_QUE_PROC_NOT_OWNED:	return (syserr_que_proc_not_owned);
    case ERROR_QUE_PROC_OWNED:	return (syserr_que_proc_owned);
    case ERROR_QUE_DUPLICATE:	return (syserr_que_duplicate);
    case ERROR_QUE_ELEMENT_NOT_EXIST:	return (syserr_que_element_not_exist);
    case ERROR_QUE_NO_MEMORY:	return (syserr_que_no_memory);
    case ERROR_QUE_INVALID_NAME:	return (syserr_que_invalid_name);
    case ERROR_QUE_INVALID_PRIORITY:	return (syserr_que_invalid_priority);
    case ERROR_QUE_INVALID_HANDLE:	return (syserr_que_invalid_handle);
    case ERROR_QUE_LINK_NOT_FOUND:	return (syserr_que_link_not_found);
    case ERROR_QUE_MEMORY_ERROR:	return (syserr_que_memory_error);
    case ERROR_QUE_PREV_AT_END:	return (syserr_que_prev_at_end);
    case ERROR_QUE_PROC_NO_ACCESS:	return (syserr_que_proc_no_access);
    case ERROR_QUE_EMPTY:	return (syserr_que_empty);
    case ERROR_QUE_NAME_NOT_EXIST:	return (syserr_que_name_not_exist);
    case ERROR_QUE_NOT_INITIALIZED:	return (syserr_que_not_initialized);
    case ERROR_QUE_UNABLE_TO_ACCESS:	return (syserr_que_unable_to_access);
    case ERROR_QUE_UNABLE_TO_ADD:	return (syserr_que_unable_to_add);
    case ERROR_QUE_UNABLE_TO_INIT:	return (syserr_que_unable_to_init);
    case ERROR_VIO_INVALID_MASK:	return (syserr_vio_invalid_mask);
    case ERROR_VIO_PTR:	return (syserr_vio_ptr);
    case ERROR_VIO_APTR:	return (syserr_vio_aptr);
    case ERROR_VIO_RPTR:	return (syserr_vio_rptr);
    case ERROR_VIO_CPTR:	return (syserr_vio_cptr);
    case ERROR_VIO_LPTR:	return (syserr_vio_lptr);
    case ERROR_VIO_MODE:	return (syserr_vio_mode);
    case ERROR_VIO_WIDTH:	return (syserr_vio_width);
    case ERROR_VIO_ATTR:	return (syserr_vio_attr);
    case ERROR_VIO_ROW:	return (syserr_vio_row);
    case ERROR_VIO_COL:	return (syserr_vio_col);
    case ERROR_VIO_TOPROW:	return (syserr_vio_toprow);
    case ERROR_VIO_BOTROW:	return (syserr_vio_botrow);
    case ERROR_VIO_RIGHTCOL:	return (syserr_vio_rightcol);
    case ERROR_VIO_LEFTCOL:	return (syserr_vio_leftcol);
    case ERROR_SCS_CALL:	return (syserr_scs_call);
    case ERROR_SCS_VALUE:	return (syserr_scs_value);
    case ERROR_VIO_WAIT_FLAG:	return (syserr_vio_wait_flag);
    case ERROR_VIO_UNLOCK:	return (syserr_vio_unlock);
    case ERROR_SGS_NOT_SESSION_MGR:	return (syserr_sgs_not_session_mgr);
    case ERROR_SMG_INVALID_SESSION_ID:	return (syserr_smg_invalid_session_id);
    case ERROR_SMG_NO_SESSIONS:	return (syserr_smg_no_sessions);
    case ERROR_SMG_SESSION_NOT_FOUND:	return (syserr_smg_session_not_found);
    case ERROR_SMG_SET_TITLE:	return (syserr_smg_set_title);
    case ERROR_KBD_PARAMETER:	return (syserr_kbd_parameter);
    case ERROR_KBD_NO_DEVICE:	return (syserr_kbd_no_device);
    case ERROR_KBD_INVALID_IOWAIT:	return (syserr_kbd_invalid_iowait);
    case ERROR_KBD_INVALID_LENGTH:	return (syserr_kbd_invalid_length);
    case ERROR_KBD_INVALID_ECHO_MASK:	return (syserr_kbd_invalid_echo_mask);
    case ERROR_KBD_INVALID_INPUT_MASK:	return (syserr_kbd_invalid_input_mask);
    case ERROR_MON_INVALID_PARMS:	return (syserr_mon_invalid_parms);
    case ERROR_MON_INVALID_DEVNAME:	return (syserr_mon_invalid_devname);
    case ERROR_MON_INVALID_HANDLE:	return (syserr_mon_invalid_handle);
    case ERROR_MON_BUFFER_TOO_SMALL:	return (syserr_mon_buffer_too_small);
    case ERROR_MON_BUFFER_EMPTY:	return (syserr_mon_buffer_empty);
    case ERROR_MON_DATA_TOO_LARGE:	return (syserr_mon_data_too_large);
    case ERROR_MOUSE_NO_DEVICE:	return (syserr_mouse_no_device);
    case ERROR_MOUSE_INV_HANDLE:	return (syserr_mouse_inv_handle);
    case ERROR_MOUSE_INV_PARMS:	return (syserr_mouse_inv_parms);
    case ERROR_MOUSE_CANT_RESET:	return (syserr_mouse_cant_reset);
    case ERROR_MOUSE_DISPLAY_PARMS:	return (syserr_mouse_display_parms);
    case ERROR_MOUSE_INV_MODULE:	return (syserr_mouse_inv_module);
    case ERROR_MOUSE_INV_ENTRY_PT:	return (syserr_mouse_inv_entry_pt);
    case ERROR_MOUSE_INV_MASK:	return (syserr_mouse_inv_mask);
    case NO_ERROR_MOUSE_NO_DATA:	return (syserr_mouse_no_data);
    case NO_ERROR_MOUSE_PTR_DRAWN:	return (syserr_mouse_ptr_drawn);
    case ERROR_INVALID_FREQUENCY:	return (syserr_invalid_frequency);
    case ERROR_NLS_NO_COUNTRY_FILE:	return (syserr_nls_no_country_file);
    case ERROR_NLS_OPEN_FAILED:	return (syserr_nls_open_failed);
#ifdef ERROR_NO_COUNTRY_OR_CODEPAGE
    case ERROR_NO_COUNTRY_OR_CODEPAGE:	return (syserr_no_country_or_codepage);
#endif
    case ERROR_NLS_TABLE_TRUNCATED:	return (syserr_nls_table_truncated);
    case ERROR_NLS_BAD_TYPE:	return (syserr_nls_bad_type);
    case ERROR_NLS_TYPE_NOT_FOUND:	return (syserr_nls_type_not_found);
    case ERROR_VIO_SMG_ONLY:	return (syserr_vio_smg_only);
    case ERROR_VIO_INVALID_ASCIIZ:	return (syserr_vio_invalid_asciiz);
    case ERROR_VIO_DEREGISTER:	return (syserr_vio_deregister);
    case ERROR_VIO_NO_POPUP:	return (syserr_vio_no_popup);
    case ERROR_VIO_EXISTING_POPUP:	return (syserr_vio_existing_popup);
    case ERROR_KBD_SMG_ONLY:	return (syserr_kbd_smg_only);
    case ERROR_KBD_INVALID_ASCIIZ:	return (syserr_kbd_invalid_asciiz);
    case ERROR_KBD_INVALID_MASK:	return (syserr_kbd_invalid_mask);
    case ERROR_KBD_REGISTER:	return (syserr_kbd_register);
    case ERROR_KBD_DEREGISTER:	return (syserr_kbd_deregister);
    case ERROR_MOUSE_SMG_ONLY:	return (syserr_mouse_smg_only);
    case ERROR_MOUSE_INVALID_ASCIIZ:	return (syserr_mouse_invalid_asciiz);
    case ERROR_MOUSE_INVALID_MASK:	return (syserr_mouse_invalid_mask);
    case ERROR_MOUSE_REGISTER:	return (syserr_mouse_register);
    case ERROR_MOUSE_DEREGISTER:	return (syserr_mouse_deregister);
    case ERROR_SMG_BAD_ACTION:	return (syserr_smg_bad_action);
    case ERROR_SMG_INVALID_CALL:	return (syserr_smg_invalid_call);
    case ERROR_SCS_SG_NOTFOUND:	return (syserr_scs_sg_notfound);
    case ERROR_SCS_NOT_SHELL:	return (syserr_scs_not_shell);
    case ERROR_VIO_INVALID_PARMS:	return (syserr_vio_invalid_parms);
    case ERROR_VIO_FUNCTION_OWNED:	return (syserr_vio_function_owned);
    case ERROR_VIO_RETURN:	return (syserr_vio_return);
    case ERROR_SCS_INVALID_FUNCTION:	return (syserr_scs_invalid_function);
    case ERROR_SCS_NOT_SESSION_MGR:	return (syserr_scs_not_session_mgr);
    case ERROR_VIO_REGISTER:	return (syserr_vio_register);
    case ERROR_VIO_NO_MODE_THREAD:	return (syserr_vio_no_mode_thread);
    case ERROR_VIO_NO_SAVE_RESTORE_THD:	return (syserr_vio_no_save_restore_thd);
    case ERROR_VIO_IN_BG:	return (syserr_vio_in_bg);
    case ERROR_VIO_ILLEGAL_DURING_POPUP:	return (syserr_vio_illegal_during_popup);
    case ERROR_SMG_NOT_BASESHELL:	return (syserr_smg_not_baseshell);
    case ERROR_SMG_BAD_STATUSREQ:	return (syserr_smg_bad_statusreq);
    case ERROR_QUE_INVALID_WAIT:	return (syserr_que_invalid_wait);
    case ERROR_VIO_LOCK:	return (syserr_vio_lock);
    case ERROR_MOUSE_INVALID_IOWAIT:	return (syserr_mouse_invalid_iowait);
    case ERROR_VIO_INVALID_HANDLE:	return (syserr_vio_invalid_handle);
    case ERROR_VIO_ILLEGAL_DURING_LOCK:	return (syserr_vio_illegal_during_lock);
    case ERROR_VIO_INVALID_LENGTH:	return (syserr_vio_invalid_length);
    case ERROR_KBD_INVALID_HANDLE:	return (syserr_kbd_invalid_handle);
    case ERROR_KBD_NO_MORE_HANDLE:	return (syserr_kbd_no_more_handle);
    case ERROR_KBD_CANNOT_CREATE_KCB:	return (syserr_kbd_cannot_create_kcb);
    case ERROR_KBD_CODEPAGE_LOAD_INCOMPL:	return (syserr_kbd_codepage_load_incompl);
    case ERROR_KBD_INVALID_CODEPAGE_ID:	return (syserr_kbd_invalid_codepage_id);
    case ERROR_KBD_NO_CODEPAGE_SUPPORT:	return (syserr_kbd_no_codepage_support);
    case ERROR_KBD_FOCUS_REQUIRED:	return (syserr_kbd_focus_required);
    case ERROR_KBD_FOCUS_ALREADY_ACTIVE:	return (syserr_kbd_focus_already_active);
    case ERROR_KBD_KEYBOARD_BUSY:	return (syserr_kbd_keyboard_busy);
    case ERROR_KBD_INVALID_CODEPAGE:	return (syserr_kbd_invalid_codepage);
    case ERROR_KBD_UNABLE_TO_FOCUS:	return (syserr_kbd_unable_to_focus);
    case ERROR_SMG_SESSION_NON_SELECT:	return (syserr_smg_session_non_select);
    case ERROR_SMG_SESSION_NOT_FOREGRND:	return (syserr_smg_session_not_foregrnd);
    case ERROR_SMG_SESSION_NOT_PARENT:	return (syserr_smg_session_not_parent);
    case ERROR_SMG_INVALID_START_MODE:	return (syserr_smg_invalid_start_mode);
    case ERROR_SMG_INVALID_RELATED_OPT:	return (syserr_smg_invalid_related_opt);
    case ERROR_SMG_INVALID_BOND_OPTION:	return (syserr_smg_invalid_bond_option);
    case ERROR_SMG_INVALID_SELECT_OPT:	return (syserr_smg_invalid_select_opt);
    case ERROR_SMG_START_IN_BACKGROUND:	return (syserr_smg_start_in_background);
    case ERROR_SMG_INVALID_STOP_OPTION:	return (syserr_smg_invalid_stop_option);
    case ERROR_SMG_BAD_RESERVE:	return (syserr_smg_bad_reserve);
    case ERROR_SMG_PROCESS_NOT_PARENT:	return (syserr_smg_process_not_parent);
    case ERROR_SMG_INVALID_DATA_LENGTH:	return (syserr_smg_invalid_data_length);
    case ERROR_SMG_NOT_BOUND:	return (syserr_smg_not_bound);
    case ERROR_SMG_RETRY_SUB_ALLOC:	return (syserr_smg_retry_sub_alloc);
    case ERROR_KBD_DETACHED:	return (syserr_kbd_detached);
    case ERROR_VIO_DETACHED:	return (syserr_vio_detached);
    case ERROR_MOU_DETACHED:	return (syserr_mou_detached);
    case ERROR_VIO_FONT:	return (syserr_vio_font);
    case ERROR_VIO_USER_FONT:	return (syserr_vio_user_font);
    case ERROR_VIO_BAD_CP:	return (syserr_vio_bad_cp);
    case ERROR_VIO_NO_CP:	return (syserr_vio_no_cp);
    case ERROR_VIO_NA_CP:	return (syserr_vio_na_cp);
    case ERROR_INVALID_CODE_PAGE:	return (syserr_invalid_code_page);
    case ERROR_CPLIST_TOO_SMALL:	return (syserr_cplist_too_small);
    case ERROR_CP_NOT_MOVED:	return (syserr_cp_not_moved);
    case ERROR_MODE_SWITCH_INIT:	return (syserr_mode_switch_init);
    case ERROR_CODE_PAGE_NOT_FOUND:	return (syserr_code_page_not_found);
    case ERROR_UNEXPECTED_SLOT_RETURNED:	return (syserr_unexpected_slot_returned);
    case ERROR_SMG_INVALID_TRACE_OPTION:	return (syserr_smg_invalid_trace_option);
    case ERROR_VIO_INTERNAL_RESOURCE:	return (syserr_vio_internal_resource);
    case ERROR_VIO_SHELL_INIT:	return (syserr_vio_shell_init);
    case ERROR_SMG_NO_HARD_ERRORS:	return (syserr_smg_no_hard_errors);
    case ERROR_CP_SWITCH_INCOMPLETE:	return (syserr_cp_switch_incomplete);
    case ERROR_VIO_TRANSPARENT_POPUP:	return (syserr_vio_transparent_popup);
    case ERROR_CRITSEC_OVERFLOW:	return (syserr_critsec_overflow);
    case ERROR_CRITSEC_UNDERFLOW:	return (syserr_critsec_underflow);
    case ERROR_VIO_BAD_RESERVE:	return (syserr_vio_bad_reserve);
    case ERROR_INVALID_ADDRESS:	return (syserr_invalid_address);
    case ERROR_ZERO_SELECTORS_REQUESTED:	return (syserr_zero_selectors_requested);
    case ERROR_NOT_ENOUGH_SELECTORS_AVA:	return (syserr_not_enough_selectors_ava);
    case ERROR_INVALID_SELECTOR:	return (syserr_invalid_selector);
    case ERROR_SMG_INVALID_PROGRAM_TYPE:	return (syserr_smg_invalid_program_type);
    case ERROR_SMG_INVALID_PGM_CONTROL:	return (syserr_smg_invalid_pgm_control);
    case ERROR_SMG_INVALID_INHERIT_OPT:	return (syserr_smg_invalid_inherit_opt);
    case ERROR_VIO_EXTENDED_SG:	return (syserr_vio_extended_sg);
    case ERROR_VIO_NOT_PRES_MGR_SG:	return (syserr_vio_not_pres_mgr_sg);
    case ERROR_VIO_SHIELD_OWNED:	return (syserr_vio_shield_owned);
    case ERROR_VIO_NO_MORE_HANDLES:	return (syserr_vio_no_more_handles);
    case ERROR_VIO_SEE_ERROR_LOG:	return (syserr_vio_see_error_log);
    case ERROR_VIO_ASSOCIATED_DC:	return (syserr_vio_associated_dc);
    case ERROR_KBD_NO_CONSOLE:	return (syserr_kbd_no_console);
    case ERROR_MOUSE_NO_CONSOLE:	return (syserr_mouse_no_console);
    case ERROR_MOUSE_INVALID_HANDLE:	return (syserr_mouse_invalid_handle);
    case ERROR_SMG_INVALID_DEBUG_PARMS:	return (syserr_smg_invalid_debug_parms);
    case ERROR_KBD_EXTENDED_SG:	return (syserr_kbd_extended_sg);
    case ERROR_MOU_EXTENDED_SG:	return (syserr_mou_extended_sg);
    case ERROR_SMG_INVALID_ICON_FILE:	return (syserr_smg_invalid_icon_file);
    case ERROR_TRC_PID_NON_EXISTENT:	return (syserr_trc_pid_non_existent);
    case ERROR_TRC_COUNT_ACTIVE:	return (syserr_trc_count_active);
    case ERROR_TRC_SUSPENDED_BY_COUNT:	return (syserr_trc_suspended_by_count);
    case ERROR_TRC_COUNT_INACTIVE:	return (syserr_trc_count_inactive);
    case ERROR_TRC_COUNT_REACHED:	return (syserr_trc_count_reached);
    case ERROR_NO_MC_TRACE:	return (syserr_no_mc_trace);
    case ERROR_MC_TRACE:	return (syserr_mc_trace);
    case ERROR_TRC_COUNT_ZERO:	return (syserr_trc_count_zero);
    case ERROR_SMG_TOO_MANY_DDS:	return (syserr_smg_too_many_dds);
    case ERROR_SMG_INVALID_NOTIFICATION:	return (syserr_smg_invalid_notification);
    case ERROR_LF_INVALID_FUNCTION:	return (syserr_lf_invalid_function);
    case ERROR_LF_NOT_AVAIL:	return (syserr_lf_not_avail);
    case ERROR_LF_SUSPENDED:	return (syserr_lf_suspended);
    case ERROR_LF_BUF_TOO_SMALL:	return (syserr_lf_buf_too_small);
    case ERROR_LF_BUFFER_FULL:	return (syserr_lf_buffer_full);
    case ERROR_LF_INVALID_RECORD:	return (syserr_lf_invalid_record);
    case ERROR_LF_INVALID_SERVICE:	return (syserr_lf_invalid_service);
    case ERROR_LF_GENERAL_FAILURE:	return (syserr_lf_general_failure);
    case ERROR_LF_INVALID_ID:	return (syserr_lf_invalid_id);
    case ERROR_LF_INVALID_HANDLE:	return (syserr_lf_invalid_handle);
    case ERROR_LF_NO_ID_AVAIL:	return (syserr_lf_no_id_avail);
    case ERROR_LF_TEMPLATE_AREA_FULL:	return (syserr_lf_template_area_full);
    case ERROR_LF_ID_IN_USE:	return (syserr_lf_id_in_use);
    case ERROR_MOU_NOT_INITIALIZED:	return (syserr_mou_not_initialized);
    case ERROR_MOUINITREAL_DONE:	return (syserr_mouinitreal_done);
    case ERROR_DOSSUB_CORRUPTED:	return (syserr_dossub_corrupted);
    case ERROR_MOUSE_CALLER_NOT_SUBSYS:	return (syserr_mouse_caller_not_subsys);
    case ERROR_ARITHMETIC_OVERFLOW:	return (syserr_arithmetic_overflow);
    case ERROR_TMR_NO_DEVICE:	return (syserr_tmr_no_device);
    case ERROR_TMR_INVALID_TIME:	return (syserr_tmr_invalid_time);
    case ERROR_PVW_INVALID_ENTITY:	return (syserr_pvw_invalid_entity);
    case ERROR_PVW_INVALID_ENTITY_TYPE:	return (syserr_pvw_invalid_entity_type);
    case ERROR_PVW_INVALID_SPEC:	return (syserr_pvw_invalid_spec);
    case ERROR_PVW_INVALID_RANGE_TYPE:	return (syserr_pvw_invalid_range_type);
    case ERROR_PVW_INVALID_COUNTER_BLK:	return (syserr_pvw_invalid_counter_blk);
    case ERROR_PVW_INVALID_TEXT_BLK:	return (syserr_pvw_invalid_text_blk);
    case ERROR_PRF_NOT_INITIALIZED:	return (syserr_prf_not_initialized);
    case ERROR_PRF_ALREADY_INITIALIZED:	return (syserr_prf_already_initialized);
    case ERROR_PRF_NOT_STARTED:	return (syserr_prf_not_started);
    case ERROR_PRF_ALREADY_STARTED:	return (syserr_prf_already_started);
    case ERROR_PRF_TIMER_OUT_OF_RANGE:	return (syserr_prf_timer_out_of_range);
    case ERROR_PRF_TIMER_RESET:	return (syserr_prf_timer_reset);
    case ERROR_VDD_LOCK_USEAGE_DENIED:	return (syserr_vdd_lock_useage_denied);
    case ERROR_TIMEOUT:	return (syserr_timeout);
    case ERROR_VDM_DOWN:	return (syserr_vdm_down);
    case ERROR_VDM_LIMIT:	return (syserr_vdm_limit);
    case ERROR_VDD_NOT_FOUND:	return (syserr_vdd_not_found);
    case ERROR_INVALID_CALLER:	return (syserr_invalid_caller);
    case ERROR_PID_MISMATCH:	return (syserr_pid_mismatch);
    case ERROR_INVALID_VDD_HANDLE:	return (syserr_invalid_vdd_handle);
    case ERROR_VLPT_NO_SPOOLER:	return (syserr_vlpt_no_spooler);
    case ERROR_VCOM_DEVICE_BUSY:	return (syserr_vcom_device_busy);
    case ERROR_VLPT_DEVICE_BUSY:	return (syserr_vlpt_device_busy);
    case ERROR_NESTING_TOO_DEEP:	return (syserr_nesting_too_deep);
    case ERROR_VDD_MISSING:	return (syserr_vdd_missing);
    case ERROR_BIDI_INVALID_LENGTH:	return (syserr_bidi_invalid_length);
    case ERROR_BIDI_INVALID_INCREMENT:	return (syserr_bidi_invalid_increment);
    case ERROR_BIDI_INVALID_COMBINATION:	return (syserr_bidi_invalid_combination);
    case ERROR_BIDI_INVALID_RESERVED:	return (syserr_bidi_invalid_reserved);
    case ERROR_BIDI_INVALID_EFFECT:	return (syserr_bidi_invalid_effect);
    case ERROR_BIDI_INVALID_CSDREC:	return (syserr_bidi_invalid_csdrec);
    case ERROR_BIDI_INVALID_CSDSTATE:	return (syserr_bidi_invalid_csdstate);
    case ERROR_BIDI_INVALID_LEVEL:	return (syserr_bidi_invalid_level);
    case ERROR_BIDI_INVALID_TYPE_SUPPORT:	return (syserr_bidi_invalid_type_support);
    case ERROR_BIDI_INVALID_ORIENTATION:	return (syserr_bidi_invalid_orientation);
    case ERROR_BIDI_INVALID_NUM_SHAPE:	return (syserr_bidi_invalid_num_shape);
    case ERROR_BIDI_INVALID_CSD:	return (syserr_bidi_invalid_csd);
    case ERROR_BIDI_NO_SUPPORT:	return (syserr_bidi_no_support);
    case NO_ERROR_BIDI_RW_INCOMPLETE:	return (syserr_bidi_rw_incomplete);
    case ERROR_IMP_INVALID_PARM:	return (syserr_imp_invalid_parm);
    case ERROR_IMP_INVALID_LENGTH:	return (syserr_imp_invalid_length);
#ifdef MSG_HPFS_DISK_ERROR_WARN
    case MSG_HPFS_DISK_ERROR_WARN:	return (syserr_hpfs_disk_error_warn);
#endif
    case ERROR_MON_BAD_BUFFER:	return (syserr_mon_bad_buffer);
    case ERROR_MODULE_CORRUPTED:	return (syserr_module_corrupted);
    case ERROR_SM_OUTOF_SWAPFILE:	return (syserr_sm_outof_swapfile);
    case ERROR_LF_TIMEOUT:	return (syserr_lf_timeout);
    case ERROR_LF_SUSPEND_SUCCESS:	return (syserr_lf_suspend_success);
    case ERROR_LF_RESUME_SUCCESS:	return (syserr_lf_resume_success);
    case ERROR_LF_REDIRECT_SUCCESS:	return (syserr_lf_redirect_success);
    case ERROR_LF_REDIRECT_FAILURE:	return (syserr_lf_redirect_failure);
    case ERROR_SWAPPER_NOT_ACTIVE:	return (syserr_swapper_not_active);
    case ERROR_INVALID_SWAPID:	return (syserr_invalid_swapid);
    case ERROR_IOERR_SWAP_FILE:	return (syserr_ioerr_swap_file);
    case ERROR_SWAP_TABLE_FULL:	return (syserr_swap_table_full);
    case ERROR_SWAP_FILE_FULL:	return (syserr_swap_file_full);
    case ERROR_CANT_INIT_SWAPPER:	return (syserr_cant_init_swapper);
    case ERROR_SWAPPER_ALREADY_INIT:	return (syserr_swapper_already_init);
    case ERROR_PMM_INSUFFICIENT_MEMORY:	return (syserr_pmm_insufficient_memory);
    case ERROR_PMM_INVALID_FLAGS:	return (syserr_pmm_invalid_flags);
    case ERROR_PMM_INVALID_ADDRESS:	return (syserr_pmm_invalid_address);
    case ERROR_PMM_LOCK_FAILED:	return (syserr_pmm_lock_failed);
    case ERROR_PMM_UNLOCK_FAILED:	return (syserr_pmm_unlock_failed);
    case ERROR_PMM_MOVE_INCOMPLETE:	return (syserr_pmm_move_incomplete);
    case ERROR_UCOM_DRIVE_RENAMED:	return (syserr_ucom_drive_renamed);
    case ERROR_UCOM_FILENAME_TRUNCATED:	return (syserr_ucom_filename_truncated);
    case ERROR_UCOM_BUFFER_LENGTH:	return (syserr_ucom_buffer_length);
    case ERROR_MON_CHAIN_HANDLE:	return (syserr_mon_chain_handle);
    case ERROR_MON_NOT_REGISTERED:	return (syserr_mon_not_registered);
    case ERROR_SMG_ALREADY_TOP:	return (syserr_smg_already_top);
    case ERROR_PMM_ARENA_MODIFIED:	return (syserr_pmm_arena_modified);
    case ERROR_SMG_PRINTER_OPEN:	return (syserr_smg_printer_open);
    case ERROR_PMM_SET_FLAGS_FAILED:	return (syserr_pmm_set_flags_failed);
    case ERROR_INVALID_DOS_DD:	return (syserr_invalid_dos_dd);
    case ERROR_BLOCKED:	return (syserr_blocked);
    case ERROR_NOBLOCK:	return (syserr_noblock);
    case ERROR_INSTANCE_SHARED:	return (syserr_instance_shared);
    case ERROR_NO_OBJECT:	return (syserr_no_object);
    case ERROR_PARTIAL_ATTACH:	return (syserr_partial_attach);
    case ERROR_INCACHE:	return (syserr_incache);
    case ERROR_SWAP_IO_PROBLEMS:	return (syserr_swap_io_problems);
    case ERROR_CROSSES_OBJECT_BOUNDARY:	return (syserr_crosses_object_boundary);
    case ERROR_LONGLOCK:	return (syserr_longlock);
    case ERROR_SHORTLOCK:	return (syserr_shortlock);
    case ERROR_UVIRTLOCK:	return (syserr_uvirtlock);
    case ERROR_ALIASLOCK:	return (syserr_aliaslock);
    case ERROR_ALIAS:	return (syserr_alias);
    case ERROR_NO_MORE_HANDLES:	return (syserr_no_more_handles);
    case ERROR_SCAN_TERMINATED:	return (syserr_scan_terminated);
    case ERROR_TERMINATOR_NOT_FOUND:	return (syserr_terminator_not_found);
    case ERROR_NOT_DIRECT_CHILD:	return (syserr_not_direct_child);
    case ERROR_DELAY_FREE:	return (syserr_delay_free);
    case ERROR_GUARDPAGE:	return (syserr_guardpage);
    case ERROR_SWAPERROR:	return (syserr_swaperror);
    case ERROR_LDRERROR:	return (syserr_ldrerror);
    case ERROR_NOMEMORY:	return (syserr_nomemory);
    case ERROR_NOACCESS:	return (syserr_noaccess);
    case ERROR_NO_DLL_TERM:	return (syserr_no_dll_term);
    case ERROR_CPSIO_CODE_PAGE_INVALID:	return (syserr_cpsio_code_page_invalid);
    case ERROR_CPSIO_NO_SPOOLER:	return (syserr_cpsio_no_spooler);
    case ERROR_CPSIO_FONT_ID_INVALID:	return (syserr_cpsio_font_id_invalid);
    case ERROR_CPSIO_INTERNAL_ERROR:	return (syserr_cpsio_internal_error);
    case ERROR_CPSIO_INVALID_PTR_NAME:	return (syserr_cpsio_invalid_ptr_name);
    case ERROR_CPSIO_NOT_ACTIVE:	return (syserr_cpsio_not_active);
    case ERROR_CPSIO_PID_FULL:	return (syserr_cpsio_pid_full);
    case ERROR_CPSIO_PID_NOT_FOUND:	return (syserr_cpsio_pid_not_found);
    case ERROR_CPSIO_READ_CTL_SEQ:	return (syserr_cpsio_read_ctl_seq);
    case ERROR_CPSIO_READ_FNT_DEF:	return (syserr_cpsio_read_fnt_def);
    case ERROR_CPSIO_WRITE_ERROR:	return (syserr_cpsio_write_error);
    case ERROR_CPSIO_WRITE_FULL_ERROR:	return (syserr_cpsio_write_full_error);
    case ERROR_CPSIO_WRITE_HANDLE_BAD:	return (syserr_cpsio_write_handle_bad);
    case ERROR_CPSIO_SWIT_LOAD:	return (syserr_cpsio_swit_load);
    case ERROR_CPSIO_INV_COMMAND:	return (syserr_cpsio_inv_command);
    case ERROR_CPSIO_NO_FONT_SWIT:	return (syserr_cpsio_no_font_swit);
    case ERROR_ENTRY_IS_CALLGATE:	return (syserr_entry_is_callgate);

#ifndef DISABLE_SOCKET_SUPPORT
    case SOCEPERM:		return (syserr_socket_perm);
    case SOCESRCH:		return (syserr_socket_srch);
    case SOCEINTR:		return (syserr_socket_intr);
    case SOCENXIO:		return (syserr_socket_nxio);
    case SOCEBADF:		return (syserr_socket_badf);
    case SOCEACCES:		return (syserr_socket_acces);
    case SOCEFAULT:		return (syserr_socket_fault);
    case SOCEINVAL:		return (syserr_socket_inval);
    case SOCEMFILE:		return (syserr_socket_mfile);
    case SOCEPIPE:		return (syserr_socket_pipe);
    case SOCEOS2ERR:		return (syserr_socket_os2err);
    case SOCEWOULDBLOCK:	return (syserr_socket_wouldblock);
    case SOCEINPROGRESS:	return (syserr_socket_inprogress);
    case SOCEALREADY:		return (syserr_socket_already);
    case SOCENOTSOCK:		return (syserr_socket_notsock);
    case SOCEDESTADDRREQ:	return (syserr_socket_destaddrreq);
    case SOCEMSGSIZE:		return (syserr_socket_msgsize);
    case SOCEPROTOTYPE:		return (syserr_socket_prototype);
    case SOCENOPROTOOPT:	return (syserr_socket_noprotoopt);
    case SOCEPROTONOSUPPORT:	return (syserr_socket_protonosupport);
    case SOCESOCKTNOSUPPORT:	return (syserr_socket_socktnosupport);
    case SOCEOPNOTSUPP:		return (syserr_socket_opnotsupp);
    case SOCEPFNOSUPPORT:	return (syserr_socket_pfnosupport);
    case SOCEAFNOSUPPORT:	return (syserr_socket_afnosupport);
    case SOCEADDRINUSE:		return (syserr_socket_addrinuse);
    case SOCEADDRNOTAVAIL:	return (syserr_socket_addrnotavail);
    case SOCENETDOWN:		return (syserr_socket_netdown);
    case SOCENETUNREACH:	return (syserr_socket_netunreach);
    case SOCENETRESET:		return (syserr_socket_netreset);
    case SOCECONNABORTED:	return (syserr_socket_connaborted);
    case SOCECONNRESET:		return (syserr_socket_connreset);
    case SOCENOBUFS:		return (syserr_socket_nobufs);
    case SOCEISCONN:		return (syserr_socket_isconn);
    case SOCENOTCONN:		return (syserr_socket_notconn);
    case SOCESHUTDOWN:		return (syserr_socket_shutdown);
    case SOCETOOMANYREFS:	return (syserr_socket_toomanyrefs);
    case SOCETIMEDOUT:		return (syserr_socket_timedout);
    case SOCECONNREFUSED:	return (syserr_socket_connrefused);
    case SOCELOOP:		return (syserr_socket_loop);
    case SOCENAMETOOLONG:	return (syserr_socket_nametoolong);
    case SOCEHOSTDOWN:		return (syserr_socket_hostdown);
    case SOCEHOSTUNREACH:	return (syserr_socket_hostunreach);
    case SOCENOTEMPTY:		return (syserr_socket_notempty);
#endif /* not DISABLE_SOCKET_SUPPORT */

    default:	return (syserr_unknown);
    }
}

/* Machine-generated procedure, do not edit: */
static APIRET
syserr_to_error_code (enum syserr_names syserr)
{
  switch (syserr)
    {
    case syserr_invalid_function:	return (ERROR_INVALID_FUNCTION);
    case syserr_file_not_found:	return (ERROR_FILE_NOT_FOUND);
    case syserr_path_not_found:	return (ERROR_PATH_NOT_FOUND);
    case syserr_too_many_open_files:	return (ERROR_TOO_MANY_OPEN_FILES);
    case syserr_access_denied:	return (ERROR_ACCESS_DENIED);
    case syserr_invalid_handle:	return (ERROR_INVALID_HANDLE);
    case syserr_arena_trashed:	return (ERROR_ARENA_TRASHED);
    case syserr_not_enough_memory:	return (ERROR_NOT_ENOUGH_MEMORY);
    case syserr_invalid_block:	return (ERROR_INVALID_BLOCK);
    case syserr_bad_environment:	return (ERROR_BAD_ENVIRONMENT);
    case syserr_bad_format:	return (ERROR_BAD_FORMAT);
    case syserr_invalid_access:	return (ERROR_INVALID_ACCESS);
    case syserr_invalid_data:	return (ERROR_INVALID_DATA);
    case syserr_invalid_drive:	return (ERROR_INVALID_DRIVE);
    case syserr_current_directory:	return (ERROR_CURRENT_DIRECTORY);
    case syserr_not_same_device:	return (ERROR_NOT_SAME_DEVICE);
    case syserr_no_more_files:	return (ERROR_NO_MORE_FILES);
    case syserr_write_protect:	return (ERROR_WRITE_PROTECT);
    case syserr_bad_unit:	return (ERROR_BAD_UNIT);
    case syserr_not_ready:	return (ERROR_NOT_READY);
    case syserr_bad_command:	return (ERROR_BAD_COMMAND);
    case syserr_crc:	return (ERROR_CRC);
    case syserr_bad_length:	return (ERROR_BAD_LENGTH);
    case syserr_seek:	return (ERROR_SEEK);
    case syserr_not_dos_disk:	return (ERROR_NOT_DOS_DISK);
    case syserr_sector_not_found:	return (ERROR_SECTOR_NOT_FOUND);
    case syserr_out_of_paper:	return (ERROR_OUT_OF_PAPER);
    case syserr_write_fault:	return (ERROR_WRITE_FAULT);
    case syserr_read_fault:	return (ERROR_READ_FAULT);
    case syserr_gen_failure:	return (ERROR_GEN_FAILURE);
    case syserr_sharing_violation:	return (ERROR_SHARING_VIOLATION);
    case syserr_lock_violation:	return (ERROR_LOCK_VIOLATION);
    case syserr_wrong_disk:	return (ERROR_WRONG_DISK);
    case syserr_fcb_unavailable:	return (ERROR_FCB_UNAVAILABLE);
    case syserr_sharing_buffer_exceeded:	return (ERROR_SHARING_BUFFER_EXCEEDED);
    case syserr_code_page_mismatched:	return (ERROR_CODE_PAGE_MISMATCHED);
    case syserr_handle_eof:	return (ERROR_HANDLE_EOF);
    case syserr_handle_disk_full:	return (ERROR_HANDLE_DISK_FULL);
    case syserr_not_supported:	return (ERROR_NOT_SUPPORTED);
    case syserr_rem_not_list:	return (ERROR_REM_NOT_LIST);
    case syserr_dup_name:	return (ERROR_DUP_NAME);
    case syserr_bad_netpath:	return (ERROR_BAD_NETPATH);
    case syserr_network_busy:	return (ERROR_NETWORK_BUSY);
    case syserr_dev_not_exist:	return (ERROR_DEV_NOT_EXIST);
    case syserr_too_many_cmds:	return (ERROR_TOO_MANY_CMDS);
    case syserr_adap_hdw_err:	return (ERROR_ADAP_HDW_ERR);
    case syserr_bad_net_resp:	return (ERROR_BAD_NET_RESP);
    case syserr_unexp_net_err:	return (ERROR_UNEXP_NET_ERR);
    case syserr_bad_rem_adap:	return (ERROR_BAD_REM_ADAP);
    case syserr_printq_full:	return (ERROR_PRINTQ_FULL);
    case syserr_no_spool_space:	return (ERROR_NO_SPOOL_SPACE);
    case syserr_print_cancelled:	return (ERROR_PRINT_CANCELLED);
    case syserr_netname_deleted:	return (ERROR_NETNAME_DELETED);
    case syserr_network_access_denied:	return (ERROR_NETWORK_ACCESS_DENIED);
    case syserr_bad_dev_type:	return (ERROR_BAD_DEV_TYPE);
    case syserr_bad_net_name:	return (ERROR_BAD_NET_NAME);
    case syserr_too_many_names:	return (ERROR_TOO_MANY_NAMES);
    case syserr_too_many_sess:	return (ERROR_TOO_MANY_SESS);
    case syserr_sharing_paused:	return (ERROR_SHARING_PAUSED);
    case syserr_req_not_accep:	return (ERROR_REQ_NOT_ACCEP);
    case syserr_redir_paused:	return (ERROR_REDIR_PAUSED);
    case syserr_sbcs_att_write_prot:	return (ERROR_SBCS_ATT_WRITE_PROT);
    case syserr_sbcs_general_failure:	return (ERROR_SBCS_GENERAL_FAILURE);
    case syserr_xga_out_memory:	return (ERROR_XGA_OUT_MEMORY);
    case syserr_file_exists:	return (ERROR_FILE_EXISTS);
    case syserr_dup_fcb:	return (ERROR_DUP_FCB);
    case syserr_cannot_make:	return (ERROR_CANNOT_MAKE);
    case syserr_fail_i24:	return (ERROR_FAIL_I24);
    case syserr_out_of_structures:	return (ERROR_OUT_OF_STRUCTURES);
    case syserr_already_assigned:	return (ERROR_ALREADY_ASSIGNED);
    case syserr_invalid_password:	return (ERROR_INVALID_PASSWORD);
    case syserr_invalid_parameter:	return (ERROR_INVALID_PARAMETER);
    case syserr_net_write_fault:	return (ERROR_NET_WRITE_FAULT);
    case syserr_no_proc_slots:	return (ERROR_NO_PROC_SLOTS);
    case syserr_not_frozen:	return (ERROR_NOT_FROZEN);
    case syserr_tstovfl:	return (ERR_TSTOVFL);
    case syserr_tstdup:	return (ERR_TSTDUP);
    case syserr_no_items:	return (ERROR_NO_ITEMS);
    case syserr_interrupt:	return (ERROR_INTERRUPT);
    case syserr_device_in_use:	return (ERROR_DEVICE_IN_USE);
    case syserr_too_many_semaphores:	return (ERROR_TOO_MANY_SEMAPHORES);
    case syserr_excl_sem_already_owned:	return (ERROR_EXCL_SEM_ALREADY_OWNED);
    case syserr_sem_is_set:	return (ERROR_SEM_IS_SET);
    case syserr_too_many_sem_requests:	return (ERROR_TOO_MANY_SEM_REQUESTS);
    case syserr_invalid_at_interrupt_time:	return (ERROR_INVALID_AT_INTERRUPT_TIME);
    case syserr_sem_owner_died:	return (ERROR_SEM_OWNER_DIED);
    case syserr_sem_user_limit:	return (ERROR_SEM_USER_LIMIT);
    case syserr_disk_change:	return (ERROR_DISK_CHANGE);
    case syserr_drive_locked:	return (ERROR_DRIVE_LOCKED);
    case syserr_broken_pipe:	return (ERROR_BROKEN_PIPE);
    case syserr_open_failed:	return (ERROR_OPEN_FAILED);
    case syserr_buffer_overflow:	return (ERROR_BUFFER_OVERFLOW);
    case syserr_disk_full:	return (ERROR_DISK_FULL);
    case syserr_no_more_search_handles:	return (ERROR_NO_MORE_SEARCH_HANDLES);
    case syserr_invalid_target_handle:	return (ERROR_INVALID_TARGET_HANDLE);
    case syserr_protection_violation:	return (ERROR_PROTECTION_VIOLATION);
    case syserr_viokbd_request:	return (ERROR_VIOKBD_REQUEST);
    case syserr_invalid_category:	return (ERROR_INVALID_CATEGORY);
    case syserr_invalid_verify_switch:	return (ERROR_INVALID_VERIFY_SWITCH);
    case syserr_bad_driver_level:	return (ERROR_BAD_DRIVER_LEVEL);
    case syserr_call_not_implemented:	return (ERROR_CALL_NOT_IMPLEMENTED);
    case syserr_sem_timeout:	return (ERROR_SEM_TIMEOUT);
    case syserr_insufficient_buffer:	return (ERROR_INSUFFICIENT_BUFFER);
    case syserr_invalid_name:	return (ERROR_INVALID_NAME);
    case syserr_invalid_level:	return (ERROR_INVALID_LEVEL);
    case syserr_no_volume_label:	return (ERROR_NO_VOLUME_LABEL);
    case syserr_mod_not_found:	return (ERROR_MOD_NOT_FOUND);
    case syserr_proc_not_found:	return (ERROR_PROC_NOT_FOUND);
    case syserr_wait_no_children:	return (ERROR_WAIT_NO_CHILDREN);
    case syserr_child_not_complete:	return (ERROR_CHILD_NOT_COMPLETE);
    case syserr_direct_access_handle:	return (ERROR_DIRECT_ACCESS_HANDLE);
    case syserr_negative_seek:	return (ERROR_NEGATIVE_SEEK);
    case syserr_seek_on_device:	return (ERROR_SEEK_ON_DEVICE);
    case syserr_is_join_target:	return (ERROR_IS_JOIN_TARGET);
    case syserr_is_joined:	return (ERROR_IS_JOINED);
    case syserr_is_substed:	return (ERROR_IS_SUBSTED);
    case syserr_not_joined:	return (ERROR_NOT_JOINED);
    case syserr_not_substed:	return (ERROR_NOT_SUBSTED);
    case syserr_join_to_join:	return (ERROR_JOIN_TO_JOIN);
    case syserr_subst_to_subst:	return (ERROR_SUBST_TO_SUBST);
    case syserr_join_to_subst:	return (ERROR_JOIN_TO_SUBST);
    case syserr_subst_to_join:	return (ERROR_SUBST_TO_JOIN);
    case syserr_busy_drive:	return (ERROR_BUSY_DRIVE);
    case syserr_same_drive:	return (ERROR_SAME_DRIVE);
    case syserr_dir_not_root:	return (ERROR_DIR_NOT_ROOT);
    case syserr_dir_not_empty:	return (ERROR_DIR_NOT_EMPTY);
    case syserr_is_subst_path:	return (ERROR_IS_SUBST_PATH);
    case syserr_is_join_path:	return (ERROR_IS_JOIN_PATH);
    case syserr_path_busy:	return (ERROR_PATH_BUSY);
    case syserr_is_subst_target:	return (ERROR_IS_SUBST_TARGET);
    case syserr_system_trace:	return (ERROR_SYSTEM_TRACE);
    case syserr_invalid_event_count:	return (ERROR_INVALID_EVENT_COUNT);
    case syserr_too_many_muxwaiters:	return (ERROR_TOO_MANY_MUXWAITERS);
    case syserr_invalid_list_format:	return (ERROR_INVALID_LIST_FORMAT);
    case syserr_label_too_long:	return (ERROR_LABEL_TOO_LONG);
    case syserr_too_many_tcbs:	return (ERROR_TOO_MANY_TCBS);
    case syserr_signal_refused:	return (ERROR_SIGNAL_REFUSED);
    case syserr_discarded:	return (ERROR_DISCARDED);
    case syserr_not_locked:	return (ERROR_NOT_LOCKED);
    case syserr_bad_threadid_addr:	return (ERROR_BAD_THREADID_ADDR);
    case syserr_bad_arguments:	return (ERROR_BAD_ARGUMENTS);
    case syserr_bad_pathname:	return (ERROR_BAD_PATHNAME);
    case syserr_signal_pending:	return (ERROR_SIGNAL_PENDING);
    case syserr_uncertain_media:	return (ERROR_UNCERTAIN_MEDIA);
    case syserr_max_thrds_reached:	return (ERROR_MAX_THRDS_REACHED);
    case syserr_monitors_not_supported:	return (ERROR_MONITORS_NOT_SUPPORTED);
    case syserr_unc_driver_not_installed:	return (ERROR_UNC_DRIVER_NOT_INSTALLED);
    case syserr_lock_failed:	return (ERROR_LOCK_FAILED);
    case syserr_swapio_failed:	return (ERROR_SWAPIO_FAILED);
    case syserr_swapin_failed:	return (ERROR_SWAPIN_FAILED);
    case syserr_busy:	return (ERROR_BUSY);
    case syserr_cancel_violation:	return (ERROR_CANCEL_VIOLATION);
    case syserr_atomic_lock_not_supported:	return (ERROR_ATOMIC_LOCK_NOT_SUPPORTED);
    case syserr_read_locks_not_supported:	return (ERROR_READ_LOCKS_NOT_SUPPORTED);
    case syserr_invalid_segment_number:	return (ERROR_INVALID_SEGMENT_NUMBER);
    case syserr_invalid_callgate:	return (ERROR_INVALID_CALLGATE);
    case syserr_invalid_ordinal:	return (ERROR_INVALID_ORDINAL);
    case syserr_already_exists:	return (ERROR_ALREADY_EXISTS);
    case syserr_no_child_process:	return (ERROR_NO_CHILD_PROCESS);
    case syserr_child_alive_nowait:	return (ERROR_CHILD_ALIVE_NOWAIT);
    case syserr_invalid_flag_number:	return (ERROR_INVALID_FLAG_NUMBER);
    case syserr_sem_not_found:	return (ERROR_SEM_NOT_FOUND);
    case syserr_invalid_starting_codeseg:	return (ERROR_INVALID_STARTING_CODESEG);
    case syserr_invalid_stackseg:	return (ERROR_INVALID_STACKSEG);
    case syserr_invalid_moduletype:	return (ERROR_INVALID_MODULETYPE);
    case syserr_invalid_exe_signature:	return (ERROR_INVALID_EXE_SIGNATURE);
    case syserr_exe_marked_invalid:	return (ERROR_EXE_MARKED_INVALID);
    case syserr_bad_exe_format:	return (ERROR_BAD_EXE_FORMAT);
#ifdef ERROR_ITERATED_DATA_EXCEEDS_64k
    case syserr_iterated_data_exceeds_64k:	return (ERROR_ITERATED_DATA_EXCEEDS_64k);
#endif
    case syserr_invalid_minallocsize:	return (ERROR_INVALID_MINALLOCSIZE);
    case syserr_dynlink_from_invalid_ring:	return (ERROR_DYNLINK_FROM_INVALID_RING);
    case syserr_iopl_not_enabled:	return (ERROR_IOPL_NOT_ENABLED);
    case syserr_invalid_segdpl:	return (ERROR_INVALID_SEGDPL);
#ifdef ERROR_AUTODATASEG_EXCEEDS_64k
    case syserr_autodataseg_exceeds_64k:	return (ERROR_AUTODATASEG_EXCEEDS_64k);
#endif
    case syserr_ring2seg_must_be_movable:	return (ERROR_RING2SEG_MUST_BE_MOVABLE);
#ifdef ERROR_RELOC_CHAIN_XEEDS_SEGLIM
    case syserr_reloc_chain_xeeds_seglim:	return (ERROR_RELOC_CHAIN_XEEDS_SEGLIM);
#endif
    case syserr_infloop_in_reloc_chain:	return (ERROR_INFLOOP_IN_RELOC_CHAIN);
    case syserr_envvar_not_found:	return (ERROR_ENVVAR_NOT_FOUND);
    case syserr_not_current_ctry:	return (ERROR_NOT_CURRENT_CTRY);
    case syserr_no_signal_sent:	return (ERROR_NO_SIGNAL_SENT);
    case syserr_filename_exced_range:	return (ERROR_FILENAME_EXCED_RANGE);
    case syserr_ring2_stack_in_use:	return (ERROR_RING2_STACK_IN_USE);
    case syserr_meta_expansion_too_long:	return (ERROR_META_EXPANSION_TOO_LONG);
    case syserr_invalid_signal_number:	return (ERROR_INVALID_SIGNAL_NUMBER);
    case syserr_thread_1_inactive:	return (ERROR_THREAD_1_INACTIVE);
    case syserr_info_not_avail:	return (ERROR_INFO_NOT_AVAIL);
    case syserr_locked:	return (ERROR_LOCKED);
    case syserr_bad_dynalink:	return (ERROR_BAD_DYNALINK);
    case syserr_too_many_modules:	return (ERROR_TOO_MANY_MODULES);
    case syserr_nesting_not_allowed:	return (ERROR_NESTING_NOT_ALLOWED);
    case syserr_cannot_shrink:	return (ERROR_CANNOT_SHRINK);
    case syserr_zombie_process:	return (ERROR_ZOMBIE_PROCESS);
    case syserr_stack_in_high_memory:	return (ERROR_STACK_IN_HIGH_MEMORY);
    case syserr_invalid_exitroutine_ring:	return (ERROR_INVALID_EXITROUTINE_RING);
    case syserr_getbuf_failed:	return (ERROR_GETBUF_FAILED);
    case syserr_flushbuf_failed:	return (ERROR_FLUSHBUF_FAILED);
    case syserr_transfer_too_long:	return (ERROR_TRANSFER_TOO_LONG);
    case syserr_forcenoswap_failed:	return (ERROR_FORCENOSWAP_FAILED);
    case syserr_smg_no_target_window:	return (ERROR_SMG_NO_TARGET_WINDOW);
    case syserr_no_children:	return (ERROR_NO_CHILDREN);
    case syserr_invalid_screen_group:	return (ERROR_INVALID_SCREEN_GROUP);
    case syserr_bad_pipe:	return (ERROR_BAD_PIPE);
    case syserr_pipe_busy:	return (ERROR_PIPE_BUSY);
    case syserr_no_data:	return (ERROR_NO_DATA);
    case syserr_pipe_not_connected:	return (ERROR_PIPE_NOT_CONNECTED);
    case syserr_more_data:	return (ERROR_MORE_DATA);
    case syserr_vc_disconnected:	return (ERROR_VC_DISCONNECTED);
    case syserr_circularity_requested:	return (ERROR_CIRCULARITY_REQUESTED);
    case syserr_directory_in_cds:	return (ERROR_DIRECTORY_IN_CDS);
    case syserr_invalid_fsd_name:	return (ERROR_INVALID_FSD_NAME);
    case syserr_invalid_path:	return (ERROR_INVALID_PATH);
    case syserr_invalid_ea_name:	return (ERROR_INVALID_EA_NAME);
    case syserr_ea_list_inconsistent:	return (ERROR_EA_LIST_INCONSISTENT);
    case syserr_ea_list_too_long:	return (ERROR_EA_LIST_TOO_LONG);
    case syserr_no_meta_match:	return (ERROR_NO_META_MATCH);
    case syserr_findnotify_timeout:	return (ERROR_FINDNOTIFY_TIMEOUT);
    case syserr_no_more_items:	return (ERROR_NO_MORE_ITEMS);
    case syserr_search_struc_reused:	return (ERROR_SEARCH_STRUC_REUSED);
    case syserr_char_not_found:	return (ERROR_CHAR_NOT_FOUND);
    case syserr_too_much_stack:	return (ERROR_TOO_MUCH_STACK);
    case syserr_invalid_attr:	return (ERROR_INVALID_ATTR);
    case syserr_invalid_starting_ring:	return (ERROR_INVALID_STARTING_RING);
    case syserr_invalid_dll_init_ring:	return (ERROR_INVALID_DLL_INIT_RING);
    case syserr_cannot_copy:	return (ERROR_CANNOT_COPY);
    case syserr_directory:	return (ERROR_DIRECTORY);
    case syserr_oplocked_file:	return (ERROR_OPLOCKED_FILE);
    case syserr_oplock_thread_exists:	return (ERROR_OPLOCK_THREAD_EXISTS);
    case syserr_volume_changed:	return (ERROR_VOLUME_CHANGED);
    case syserr_findnotify_handle_in_use:	return (ERROR_FINDNOTIFY_HANDLE_IN_USE);
    case syserr_findnotify_handle_closed:	return (ERROR_FINDNOTIFY_HANDLE_CLOSED);
    case syserr_notify_object_removed:	return (ERROR_NOTIFY_OBJECT_REMOVED);
    case syserr_already_shutdown:	return (ERROR_ALREADY_SHUTDOWN);
    case syserr_eas_didnt_fit:	return (ERROR_EAS_DIDNT_FIT);
    case syserr_ea_file_corrupt:	return (ERROR_EA_FILE_CORRUPT);
    case syserr_ea_table_full:	return (ERROR_EA_TABLE_FULL);
    case syserr_invalid_ea_handle:	return (ERROR_INVALID_EA_HANDLE);
    case syserr_no_cluster:	return (ERROR_NO_CLUSTER);
    case syserr_create_ea_file:	return (ERROR_CREATE_EA_FILE);
    case syserr_cannot_open_ea_file:	return (ERROR_CANNOT_OPEN_EA_FILE);
    case syserr_eas_not_supported:	return (ERROR_EAS_NOT_SUPPORTED);
    case syserr_need_eas_found:	return (ERROR_NEED_EAS_FOUND);
    case syserr_duplicate_handle:	return (ERROR_DUPLICATE_HANDLE);
    case syserr_duplicate_name:	return (ERROR_DUPLICATE_NAME);
    case syserr_empty_muxwait:	return (ERROR_EMPTY_MUXWAIT);
    case syserr_mutex_owned:	return (ERROR_MUTEX_OWNED);
    case syserr_not_owner:	return (ERROR_NOT_OWNER);
    case syserr_param_too_small:	return (ERROR_PARAM_TOO_SMALL);
    case syserr_too_many_handles:	return (ERROR_TOO_MANY_HANDLES);
    case syserr_too_many_opens:	return (ERROR_TOO_MANY_OPENS);
    case syserr_wrong_type:	return (ERROR_WRONG_TYPE);
    case syserr_unused_code:	return (ERROR_UNUSED_CODE);
    case syserr_thread_not_terminated:	return (ERROR_THREAD_NOT_TERMINATED);
    case syserr_init_routine_failed:	return (ERROR_INIT_ROUTINE_FAILED);
    case syserr_module_in_use:	return (ERROR_MODULE_IN_USE);
    case syserr_not_enough_watchpoints:	return (ERROR_NOT_ENOUGH_WATCHPOINTS);
    case syserr_too_many_posts:	return (ERROR_TOO_MANY_POSTS);
    case syserr_already_posted:	return (ERROR_ALREADY_POSTED);
    case syserr_already_reset:	return (ERROR_ALREADY_RESET);
    case syserr_sem_busy:	return (ERROR_SEM_BUSY);
    case syserr_invalid_procid:	return (ERROR_INVALID_PROCID);
    case syserr_invalid_pdelta:	return (ERROR_INVALID_PDELTA);
    case syserr_not_descendant:	return (ERROR_NOT_DESCENDANT);
    case syserr_not_session_manager:	return (ERROR_NOT_SESSION_MANAGER);
    case syserr_invalid_pclass:	return (ERROR_INVALID_PCLASS);
    case syserr_invalid_scope:	return (ERROR_INVALID_SCOPE);
    case syserr_invalid_threadid:	return (ERROR_INVALID_THREADID);
    case syserr_dossub_shrink:	return (ERROR_DOSSUB_SHRINK);
    case syserr_dossub_nomem:	return (ERROR_DOSSUB_NOMEM);
    case syserr_dossub_overlap:	return (ERROR_DOSSUB_OVERLAP);
    case syserr_dossub_badsize:	return (ERROR_DOSSUB_BADSIZE);
    case syserr_dossub_badflag:	return (ERROR_DOSSUB_BADFLAG);
    case syserr_dossub_badselector:	return (ERROR_DOSSUB_BADSELECTOR);
    case syserr_mr_msg_too_long:	return (ERROR_MR_MSG_TOO_LONG);
    case syserr_mr_mid_not_found:	return (ERROR_MR_MID_NOT_FOUND);
    case syserr_mr_un_acc_msgf:	return (ERROR_MR_UN_ACC_MSGF);
    case syserr_mr_inv_msgf_format:	return (ERROR_MR_INV_MSGF_FORMAT);
    case syserr_mr_inv_ivcount:	return (ERROR_MR_INV_IVCOUNT);
    case syserr_mr_un_perform:	return (ERROR_MR_UN_PERFORM);
    case syserr_ts_wakeup:	return (ERROR_TS_WAKEUP);
    case syserr_ts_semhandle:	return (ERROR_TS_SEMHANDLE);
    case syserr_ts_notimer:	return (ERROR_TS_NOTIMER);
    case syserr_ts_handle:	return (ERROR_TS_HANDLE);
    case syserr_ts_datetime:	return (ERROR_TS_DATETIME);
    case syserr_sys_internal:	return (ERROR_SYS_INTERNAL);
    case syserr_que_current_name:	return (ERROR_QUE_CURRENT_NAME);
    case syserr_que_proc_not_owned:	return (ERROR_QUE_PROC_NOT_OWNED);
    case syserr_que_proc_owned:	return (ERROR_QUE_PROC_OWNED);
    case syserr_que_duplicate:	return (ERROR_QUE_DUPLICATE);
    case syserr_que_element_not_exist:	return (ERROR_QUE_ELEMENT_NOT_EXIST);
    case syserr_que_no_memory:	return (ERROR_QUE_NO_MEMORY);
    case syserr_que_invalid_name:	return (ERROR_QUE_INVALID_NAME);
    case syserr_que_invalid_priority:	return (ERROR_QUE_INVALID_PRIORITY);
    case syserr_que_invalid_handle:	return (ERROR_QUE_INVALID_HANDLE);
    case syserr_que_link_not_found:	return (ERROR_QUE_LINK_NOT_FOUND);
    case syserr_que_memory_error:	return (ERROR_QUE_MEMORY_ERROR);
    case syserr_que_prev_at_end:	return (ERROR_QUE_PREV_AT_END);
    case syserr_que_proc_no_access:	return (ERROR_QUE_PROC_NO_ACCESS);
    case syserr_que_empty:	return (ERROR_QUE_EMPTY);
    case syserr_que_name_not_exist:	return (ERROR_QUE_NAME_NOT_EXIST);
    case syserr_que_not_initialized:	return (ERROR_QUE_NOT_INITIALIZED);
    case syserr_que_unable_to_access:	return (ERROR_QUE_UNABLE_TO_ACCESS);
    case syserr_que_unable_to_add:	return (ERROR_QUE_UNABLE_TO_ADD);
    case syserr_que_unable_to_init:	return (ERROR_QUE_UNABLE_TO_INIT);
    case syserr_vio_invalid_mask:	return (ERROR_VIO_INVALID_MASK);
    case syserr_vio_ptr:	return (ERROR_VIO_PTR);
    case syserr_vio_aptr:	return (ERROR_VIO_APTR);
    case syserr_vio_rptr:	return (ERROR_VIO_RPTR);
    case syserr_vio_cptr:	return (ERROR_VIO_CPTR);
    case syserr_vio_lptr:	return (ERROR_VIO_LPTR);
    case syserr_vio_mode:	return (ERROR_VIO_MODE);
    case syserr_vio_width:	return (ERROR_VIO_WIDTH);
    case syserr_vio_attr:	return (ERROR_VIO_ATTR);
    case syserr_vio_row:	return (ERROR_VIO_ROW);
    case syserr_vio_col:	return (ERROR_VIO_COL);
    case syserr_vio_toprow:	return (ERROR_VIO_TOPROW);
    case syserr_vio_botrow:	return (ERROR_VIO_BOTROW);
    case syserr_vio_rightcol:	return (ERROR_VIO_RIGHTCOL);
    case syserr_vio_leftcol:	return (ERROR_VIO_LEFTCOL);
    case syserr_scs_call:	return (ERROR_SCS_CALL);
    case syserr_scs_value:	return (ERROR_SCS_VALUE);
    case syserr_vio_wait_flag:	return (ERROR_VIO_WAIT_FLAG);
    case syserr_vio_unlock:	return (ERROR_VIO_UNLOCK);
    case syserr_sgs_not_session_mgr:	return (ERROR_SGS_NOT_SESSION_MGR);
    case syserr_smg_invalid_session_id:	return (ERROR_SMG_INVALID_SESSION_ID);
    case syserr_smg_no_sessions:	return (ERROR_SMG_NO_SESSIONS);
    case syserr_smg_session_not_found:	return (ERROR_SMG_SESSION_NOT_FOUND);
    case syserr_smg_set_title:	return (ERROR_SMG_SET_TITLE);
    case syserr_kbd_parameter:	return (ERROR_KBD_PARAMETER);
    case syserr_kbd_no_device:	return (ERROR_KBD_NO_DEVICE);
    case syserr_kbd_invalid_iowait:	return (ERROR_KBD_INVALID_IOWAIT);
    case syserr_kbd_invalid_length:	return (ERROR_KBD_INVALID_LENGTH);
    case syserr_kbd_invalid_echo_mask:	return (ERROR_KBD_INVALID_ECHO_MASK);
    case syserr_kbd_invalid_input_mask:	return (ERROR_KBD_INVALID_INPUT_MASK);
    case syserr_mon_invalid_parms:	return (ERROR_MON_INVALID_PARMS);
    case syserr_mon_invalid_devname:	return (ERROR_MON_INVALID_DEVNAME);
    case syserr_mon_invalid_handle:	return (ERROR_MON_INVALID_HANDLE);
    case syserr_mon_buffer_too_small:	return (ERROR_MON_BUFFER_TOO_SMALL);
    case syserr_mon_buffer_empty:	return (ERROR_MON_BUFFER_EMPTY);
    case syserr_mon_data_too_large:	return (ERROR_MON_DATA_TOO_LARGE);
    case syserr_mouse_no_device:	return (ERROR_MOUSE_NO_DEVICE);
    case syserr_mouse_inv_handle:	return (ERROR_MOUSE_INV_HANDLE);
    case syserr_mouse_inv_parms:	return (ERROR_MOUSE_INV_PARMS);
    case syserr_mouse_cant_reset:	return (ERROR_MOUSE_CANT_RESET);
    case syserr_mouse_display_parms:	return (ERROR_MOUSE_DISPLAY_PARMS);
    case syserr_mouse_inv_module:	return (ERROR_MOUSE_INV_MODULE);
    case syserr_mouse_inv_entry_pt:	return (ERROR_MOUSE_INV_ENTRY_PT);
    case syserr_mouse_inv_mask:	return (ERROR_MOUSE_INV_MASK);
    case syserr_mouse_no_data:	return (NO_ERROR_MOUSE_NO_DATA);
    case syserr_mouse_ptr_drawn:	return (NO_ERROR_MOUSE_PTR_DRAWN);
    case syserr_invalid_frequency:	return (ERROR_INVALID_FREQUENCY);
    case syserr_nls_no_country_file:	return (ERROR_NLS_NO_COUNTRY_FILE);
    case syserr_nls_open_failed:	return (ERROR_NLS_OPEN_FAILED);
#ifdef ERROR_NO_COUNTRY_OR_CODEPAGE
    case syserr_no_country_or_codepage:	return (ERROR_NO_COUNTRY_OR_CODEPAGE);
#endif
    case syserr_nls_table_truncated:	return (ERROR_NLS_TABLE_TRUNCATED);
    case syserr_nls_bad_type:	return (ERROR_NLS_BAD_TYPE);
    case syserr_nls_type_not_found:	return (ERROR_NLS_TYPE_NOT_FOUND);
    case syserr_vio_smg_only:	return (ERROR_VIO_SMG_ONLY);
    case syserr_vio_invalid_asciiz:	return (ERROR_VIO_INVALID_ASCIIZ);
    case syserr_vio_deregister:	return (ERROR_VIO_DEREGISTER);
    case syserr_vio_no_popup:	return (ERROR_VIO_NO_POPUP);
    case syserr_vio_existing_popup:	return (ERROR_VIO_EXISTING_POPUP);
    case syserr_kbd_smg_only:	return (ERROR_KBD_SMG_ONLY);
    case syserr_kbd_invalid_asciiz:	return (ERROR_KBD_INVALID_ASCIIZ);
    case syserr_kbd_invalid_mask:	return (ERROR_KBD_INVALID_MASK);
    case syserr_kbd_register:	return (ERROR_KBD_REGISTER);
    case syserr_kbd_deregister:	return (ERROR_KBD_DEREGISTER);
    case syserr_mouse_smg_only:	return (ERROR_MOUSE_SMG_ONLY);
    case syserr_mouse_invalid_asciiz:	return (ERROR_MOUSE_INVALID_ASCIIZ);
    case syserr_mouse_invalid_mask:	return (ERROR_MOUSE_INVALID_MASK);
    case syserr_mouse_register:	return (ERROR_MOUSE_REGISTER);
    case syserr_mouse_deregister:	return (ERROR_MOUSE_DEREGISTER);
    case syserr_smg_bad_action:	return (ERROR_SMG_BAD_ACTION);
    case syserr_smg_invalid_call:	return (ERROR_SMG_INVALID_CALL);
    case syserr_scs_sg_notfound:	return (ERROR_SCS_SG_NOTFOUND);
    case syserr_scs_not_shell:	return (ERROR_SCS_NOT_SHELL);
    case syserr_vio_invalid_parms:	return (ERROR_VIO_INVALID_PARMS);
    case syserr_vio_function_owned:	return (ERROR_VIO_FUNCTION_OWNED);
    case syserr_vio_return:	return (ERROR_VIO_RETURN);
    case syserr_scs_invalid_function:	return (ERROR_SCS_INVALID_FUNCTION);
    case syserr_scs_not_session_mgr:	return (ERROR_SCS_NOT_SESSION_MGR);
    case syserr_vio_register:	return (ERROR_VIO_REGISTER);
    case syserr_vio_no_mode_thread:	return (ERROR_VIO_NO_MODE_THREAD);
    case syserr_vio_no_save_restore_thd:	return (ERROR_VIO_NO_SAVE_RESTORE_THD);
    case syserr_vio_in_bg:	return (ERROR_VIO_IN_BG);
    case syserr_vio_illegal_during_popup:	return (ERROR_VIO_ILLEGAL_DURING_POPUP);
    case syserr_smg_not_baseshell:	return (ERROR_SMG_NOT_BASESHELL);
    case syserr_smg_bad_statusreq:	return (ERROR_SMG_BAD_STATUSREQ);
    case syserr_que_invalid_wait:	return (ERROR_QUE_INVALID_WAIT);
    case syserr_vio_lock:	return (ERROR_VIO_LOCK);
    case syserr_mouse_invalid_iowait:	return (ERROR_MOUSE_INVALID_IOWAIT);
    case syserr_vio_invalid_handle:	return (ERROR_VIO_INVALID_HANDLE);
    case syserr_vio_illegal_during_lock:	return (ERROR_VIO_ILLEGAL_DURING_LOCK);
    case syserr_vio_invalid_length:	return (ERROR_VIO_INVALID_LENGTH);
    case syserr_kbd_invalid_handle:	return (ERROR_KBD_INVALID_HANDLE);
    case syserr_kbd_no_more_handle:	return (ERROR_KBD_NO_MORE_HANDLE);
    case syserr_kbd_cannot_create_kcb:	return (ERROR_KBD_CANNOT_CREATE_KCB);
    case syserr_kbd_codepage_load_incompl:	return (ERROR_KBD_CODEPAGE_LOAD_INCOMPL);
    case syserr_kbd_invalid_codepage_id:	return (ERROR_KBD_INVALID_CODEPAGE_ID);
    case syserr_kbd_no_codepage_support:	return (ERROR_KBD_NO_CODEPAGE_SUPPORT);
    case syserr_kbd_focus_required:	return (ERROR_KBD_FOCUS_REQUIRED);
    case syserr_kbd_focus_already_active:	return (ERROR_KBD_FOCUS_ALREADY_ACTIVE);
    case syserr_kbd_keyboard_busy:	return (ERROR_KBD_KEYBOARD_BUSY);
    case syserr_kbd_invalid_codepage:	return (ERROR_KBD_INVALID_CODEPAGE);
    case syserr_kbd_unable_to_focus:	return (ERROR_KBD_UNABLE_TO_FOCUS);
    case syserr_smg_session_non_select:	return (ERROR_SMG_SESSION_NON_SELECT);
    case syserr_smg_session_not_foregrnd:	return (ERROR_SMG_SESSION_NOT_FOREGRND);
    case syserr_smg_session_not_parent:	return (ERROR_SMG_SESSION_NOT_PARENT);
    case syserr_smg_invalid_start_mode:	return (ERROR_SMG_INVALID_START_MODE);
    case syserr_smg_invalid_related_opt:	return (ERROR_SMG_INVALID_RELATED_OPT);
    case syserr_smg_invalid_bond_option:	return (ERROR_SMG_INVALID_BOND_OPTION);
    case syserr_smg_invalid_select_opt:	return (ERROR_SMG_INVALID_SELECT_OPT);
    case syserr_smg_start_in_background:	return (ERROR_SMG_START_IN_BACKGROUND);
    case syserr_smg_invalid_stop_option:	return (ERROR_SMG_INVALID_STOP_OPTION);
    case syserr_smg_bad_reserve:	return (ERROR_SMG_BAD_RESERVE);
    case syserr_smg_process_not_parent:	return (ERROR_SMG_PROCESS_NOT_PARENT);
    case syserr_smg_invalid_data_length:	return (ERROR_SMG_INVALID_DATA_LENGTH);
    case syserr_smg_not_bound:	return (ERROR_SMG_NOT_BOUND);
    case syserr_smg_retry_sub_alloc:	return (ERROR_SMG_RETRY_SUB_ALLOC);
    case syserr_kbd_detached:	return (ERROR_KBD_DETACHED);
    case syserr_vio_detached:	return (ERROR_VIO_DETACHED);
    case syserr_mou_detached:	return (ERROR_MOU_DETACHED);
    case syserr_vio_font:	return (ERROR_VIO_FONT);
    case syserr_vio_user_font:	return (ERROR_VIO_USER_FONT);
    case syserr_vio_bad_cp:	return (ERROR_VIO_BAD_CP);
    case syserr_vio_no_cp:	return (ERROR_VIO_NO_CP);
    case syserr_vio_na_cp:	return (ERROR_VIO_NA_CP);
    case syserr_invalid_code_page:	return (ERROR_INVALID_CODE_PAGE);
    case syserr_cplist_too_small:	return (ERROR_CPLIST_TOO_SMALL);
    case syserr_cp_not_moved:	return (ERROR_CP_NOT_MOVED);
    case syserr_mode_switch_init:	return (ERROR_MODE_SWITCH_INIT);
    case syserr_code_page_not_found:	return (ERROR_CODE_PAGE_NOT_FOUND);
    case syserr_unexpected_slot_returned:	return (ERROR_UNEXPECTED_SLOT_RETURNED);
    case syserr_smg_invalid_trace_option:	return (ERROR_SMG_INVALID_TRACE_OPTION);
    case syserr_vio_internal_resource:	return (ERROR_VIO_INTERNAL_RESOURCE);
    case syserr_vio_shell_init:	return (ERROR_VIO_SHELL_INIT);
    case syserr_smg_no_hard_errors:	return (ERROR_SMG_NO_HARD_ERRORS);
    case syserr_cp_switch_incomplete:	return (ERROR_CP_SWITCH_INCOMPLETE);
    case syserr_vio_transparent_popup:	return (ERROR_VIO_TRANSPARENT_POPUP);
    case syserr_critsec_overflow:	return (ERROR_CRITSEC_OVERFLOW);
    case syserr_critsec_underflow:	return (ERROR_CRITSEC_UNDERFLOW);
    case syserr_vio_bad_reserve:	return (ERROR_VIO_BAD_RESERVE);
    case syserr_invalid_address:	return (ERROR_INVALID_ADDRESS);
    case syserr_zero_selectors_requested:	return (ERROR_ZERO_SELECTORS_REQUESTED);
    case syserr_not_enough_selectors_ava:	return (ERROR_NOT_ENOUGH_SELECTORS_AVA);
    case syserr_invalid_selector:	return (ERROR_INVALID_SELECTOR);
    case syserr_smg_invalid_program_type:	return (ERROR_SMG_INVALID_PROGRAM_TYPE);
    case syserr_smg_invalid_pgm_control:	return (ERROR_SMG_INVALID_PGM_CONTROL);
    case syserr_smg_invalid_inherit_opt:	return (ERROR_SMG_INVALID_INHERIT_OPT);
    case syserr_vio_extended_sg:	return (ERROR_VIO_EXTENDED_SG);
    case syserr_vio_not_pres_mgr_sg:	return (ERROR_VIO_NOT_PRES_MGR_SG);
    case syserr_vio_shield_owned:	return (ERROR_VIO_SHIELD_OWNED);
    case syserr_vio_no_more_handles:	return (ERROR_VIO_NO_MORE_HANDLES);
    case syserr_vio_see_error_log:	return (ERROR_VIO_SEE_ERROR_LOG);
    case syserr_vio_associated_dc:	return (ERROR_VIO_ASSOCIATED_DC);
    case syserr_kbd_no_console:	return (ERROR_KBD_NO_CONSOLE);
    case syserr_mouse_no_console:	return (ERROR_MOUSE_NO_CONSOLE);
    case syserr_mouse_invalid_handle:	return (ERROR_MOUSE_INVALID_HANDLE);
    case syserr_smg_invalid_debug_parms:	return (ERROR_SMG_INVALID_DEBUG_PARMS);
    case syserr_kbd_extended_sg:	return (ERROR_KBD_EXTENDED_SG);
    case syserr_mou_extended_sg:	return (ERROR_MOU_EXTENDED_SG);
    case syserr_smg_invalid_icon_file:	return (ERROR_SMG_INVALID_ICON_FILE);
    case syserr_trc_pid_non_existent:	return (ERROR_TRC_PID_NON_EXISTENT);
    case syserr_trc_count_active:	return (ERROR_TRC_COUNT_ACTIVE);
    case syserr_trc_suspended_by_count:	return (ERROR_TRC_SUSPENDED_BY_COUNT);
    case syserr_trc_count_inactive:	return (ERROR_TRC_COUNT_INACTIVE);
    case syserr_trc_count_reached:	return (ERROR_TRC_COUNT_REACHED);
    case syserr_no_mc_trace:	return (ERROR_NO_MC_TRACE);
    case syserr_mc_trace:	return (ERROR_MC_TRACE);
    case syserr_trc_count_zero:	return (ERROR_TRC_COUNT_ZERO);
    case syserr_smg_too_many_dds:	return (ERROR_SMG_TOO_MANY_DDS);
    case syserr_smg_invalid_notification:	return (ERROR_SMG_INVALID_NOTIFICATION);
    case syserr_lf_invalid_function:	return (ERROR_LF_INVALID_FUNCTION);
    case syserr_lf_not_avail:	return (ERROR_LF_NOT_AVAIL);
    case syserr_lf_suspended:	return (ERROR_LF_SUSPENDED);
    case syserr_lf_buf_too_small:	return (ERROR_LF_BUF_TOO_SMALL);
    case syserr_lf_buffer_full:	return (ERROR_LF_BUFFER_FULL);
    case syserr_lf_invalid_record:	return (ERROR_LF_INVALID_RECORD);
    case syserr_lf_invalid_service:	return (ERROR_LF_INVALID_SERVICE);
    case syserr_lf_general_failure:	return (ERROR_LF_GENERAL_FAILURE);
    case syserr_lf_invalid_id:	return (ERROR_LF_INVALID_ID);
    case syserr_lf_invalid_handle:	return (ERROR_LF_INVALID_HANDLE);
    case syserr_lf_no_id_avail:	return (ERROR_LF_NO_ID_AVAIL);
    case syserr_lf_template_area_full:	return (ERROR_LF_TEMPLATE_AREA_FULL);
    case syserr_lf_id_in_use:	return (ERROR_LF_ID_IN_USE);
    case syserr_mou_not_initialized:	return (ERROR_MOU_NOT_INITIALIZED);
    case syserr_mouinitreal_done:	return (ERROR_MOUINITREAL_DONE);
    case syserr_dossub_corrupted:	return (ERROR_DOSSUB_CORRUPTED);
    case syserr_mouse_caller_not_subsys:	return (ERROR_MOUSE_CALLER_NOT_SUBSYS);
    case syserr_arithmetic_overflow:	return (ERROR_ARITHMETIC_OVERFLOW);
    case syserr_tmr_no_device:	return (ERROR_TMR_NO_DEVICE);
    case syserr_tmr_invalid_time:	return (ERROR_TMR_INVALID_TIME);
    case syserr_pvw_invalid_entity:	return (ERROR_PVW_INVALID_ENTITY);
    case syserr_pvw_invalid_entity_type:	return (ERROR_PVW_INVALID_ENTITY_TYPE);
    case syserr_pvw_invalid_spec:	return (ERROR_PVW_INVALID_SPEC);
    case syserr_pvw_invalid_range_type:	return (ERROR_PVW_INVALID_RANGE_TYPE);
    case syserr_pvw_invalid_counter_blk:	return (ERROR_PVW_INVALID_COUNTER_BLK);
    case syserr_pvw_invalid_text_blk:	return (ERROR_PVW_INVALID_TEXT_BLK);
    case syserr_prf_not_initialized:	return (ERROR_PRF_NOT_INITIALIZED);
    case syserr_prf_already_initialized:	return (ERROR_PRF_ALREADY_INITIALIZED);
    case syserr_prf_not_started:	return (ERROR_PRF_NOT_STARTED);
    case syserr_prf_already_started:	return (ERROR_PRF_ALREADY_STARTED);
    case syserr_prf_timer_out_of_range:	return (ERROR_PRF_TIMER_OUT_OF_RANGE);
    case syserr_prf_timer_reset:	return (ERROR_PRF_TIMER_RESET);
    case syserr_vdd_lock_useage_denied:	return (ERROR_VDD_LOCK_USEAGE_DENIED);
    case syserr_timeout:	return (ERROR_TIMEOUT);
    case syserr_vdm_down:	return (ERROR_VDM_DOWN);
    case syserr_vdm_limit:	return (ERROR_VDM_LIMIT);
    case syserr_vdd_not_found:	return (ERROR_VDD_NOT_FOUND);
    case syserr_invalid_caller:	return (ERROR_INVALID_CALLER);
    case syserr_pid_mismatch:	return (ERROR_PID_MISMATCH);
    case syserr_invalid_vdd_handle:	return (ERROR_INVALID_VDD_HANDLE);
    case syserr_vlpt_no_spooler:	return (ERROR_VLPT_NO_SPOOLER);
    case syserr_vcom_device_busy:	return (ERROR_VCOM_DEVICE_BUSY);
    case syserr_vlpt_device_busy:	return (ERROR_VLPT_DEVICE_BUSY);
    case syserr_nesting_too_deep:	return (ERROR_NESTING_TOO_DEEP);
    case syserr_vdd_missing:	return (ERROR_VDD_MISSING);
    case syserr_bidi_invalid_length:	return (ERROR_BIDI_INVALID_LENGTH);
    case syserr_bidi_invalid_increment:	return (ERROR_BIDI_INVALID_INCREMENT);
    case syserr_bidi_invalid_combination:	return (ERROR_BIDI_INVALID_COMBINATION);
    case syserr_bidi_invalid_reserved:	return (ERROR_BIDI_INVALID_RESERVED);
    case syserr_bidi_invalid_effect:	return (ERROR_BIDI_INVALID_EFFECT);
    case syserr_bidi_invalid_csdrec:	return (ERROR_BIDI_INVALID_CSDREC);
    case syserr_bidi_invalid_csdstate:	return (ERROR_BIDI_INVALID_CSDSTATE);
    case syserr_bidi_invalid_level:	return (ERROR_BIDI_INVALID_LEVEL);
    case syserr_bidi_invalid_type_support:	return (ERROR_BIDI_INVALID_TYPE_SUPPORT);
    case syserr_bidi_invalid_orientation:	return (ERROR_BIDI_INVALID_ORIENTATION);
    case syserr_bidi_invalid_num_shape:	return (ERROR_BIDI_INVALID_NUM_SHAPE);
    case syserr_bidi_invalid_csd:	return (ERROR_BIDI_INVALID_CSD);
    case syserr_bidi_no_support:	return (ERROR_BIDI_NO_SUPPORT);
    case syserr_bidi_rw_incomplete:	return (NO_ERROR_BIDI_RW_INCOMPLETE);
    case syserr_imp_invalid_parm:	return (ERROR_IMP_INVALID_PARM);
    case syserr_imp_invalid_length:	return (ERROR_IMP_INVALID_LENGTH);
#ifdef MSG_HPFS_DISK_ERROR_WARN
    case syserr_hpfs_disk_error_warn:	return (MSG_HPFS_DISK_ERROR_WARN);
#endif
    case syserr_mon_bad_buffer:	return (ERROR_MON_BAD_BUFFER);
    case syserr_module_corrupted:	return (ERROR_MODULE_CORRUPTED);
    case syserr_sm_outof_swapfile:	return (ERROR_SM_OUTOF_SWAPFILE);
    case syserr_lf_timeout:	return (ERROR_LF_TIMEOUT);
    case syserr_lf_suspend_success:	return (ERROR_LF_SUSPEND_SUCCESS);
    case syserr_lf_resume_success:	return (ERROR_LF_RESUME_SUCCESS);
    case syserr_lf_redirect_success:	return (ERROR_LF_REDIRECT_SUCCESS);
    case syserr_lf_redirect_failure:	return (ERROR_LF_REDIRECT_FAILURE);
    case syserr_swapper_not_active:	return (ERROR_SWAPPER_NOT_ACTIVE);
    case syserr_invalid_swapid:	return (ERROR_INVALID_SWAPID);
    case syserr_ioerr_swap_file:	return (ERROR_IOERR_SWAP_FILE);
    case syserr_swap_table_full:	return (ERROR_SWAP_TABLE_FULL);
    case syserr_swap_file_full:	return (ERROR_SWAP_FILE_FULL);
    case syserr_cant_init_swapper:	return (ERROR_CANT_INIT_SWAPPER);
    case syserr_swapper_already_init:	return (ERROR_SWAPPER_ALREADY_INIT);
    case syserr_pmm_insufficient_memory:	return (ERROR_PMM_INSUFFICIENT_MEMORY);
    case syserr_pmm_invalid_flags:	return (ERROR_PMM_INVALID_FLAGS);
    case syserr_pmm_invalid_address:	return (ERROR_PMM_INVALID_ADDRESS);
    case syserr_pmm_lock_failed:	return (ERROR_PMM_LOCK_FAILED);
    case syserr_pmm_unlock_failed:	return (ERROR_PMM_UNLOCK_FAILED);
    case syserr_pmm_move_incomplete:	return (ERROR_PMM_MOVE_INCOMPLETE);
    case syserr_ucom_drive_renamed:	return (ERROR_UCOM_DRIVE_RENAMED);
    case syserr_ucom_filename_truncated:	return (ERROR_UCOM_FILENAME_TRUNCATED);
    case syserr_ucom_buffer_length:	return (ERROR_UCOM_BUFFER_LENGTH);
    case syserr_mon_chain_handle:	return (ERROR_MON_CHAIN_HANDLE);
    case syserr_mon_not_registered:	return (ERROR_MON_NOT_REGISTERED);
    case syserr_smg_already_top:	return (ERROR_SMG_ALREADY_TOP);
    case syserr_pmm_arena_modified:	return (ERROR_PMM_ARENA_MODIFIED);
    case syserr_smg_printer_open:	return (ERROR_SMG_PRINTER_OPEN);
    case syserr_pmm_set_flags_failed:	return (ERROR_PMM_SET_FLAGS_FAILED);
    case syserr_invalid_dos_dd:	return (ERROR_INVALID_DOS_DD);
    case syserr_blocked:	return (ERROR_BLOCKED);
    case syserr_noblock:	return (ERROR_NOBLOCK);
    case syserr_instance_shared:	return (ERROR_INSTANCE_SHARED);
    case syserr_no_object:	return (ERROR_NO_OBJECT);
    case syserr_partial_attach:	return (ERROR_PARTIAL_ATTACH);
    case syserr_incache:	return (ERROR_INCACHE);
    case syserr_swap_io_problems:	return (ERROR_SWAP_IO_PROBLEMS);
    case syserr_crosses_object_boundary:	return (ERROR_CROSSES_OBJECT_BOUNDARY);
    case syserr_longlock:	return (ERROR_LONGLOCK);
    case syserr_shortlock:	return (ERROR_SHORTLOCK);
    case syserr_uvirtlock:	return (ERROR_UVIRTLOCK);
    case syserr_aliaslock:	return (ERROR_ALIASLOCK);
    case syserr_alias:	return (ERROR_ALIAS);
    case syserr_no_more_handles:	return (ERROR_NO_MORE_HANDLES);
    case syserr_scan_terminated:	return (ERROR_SCAN_TERMINATED);
    case syserr_terminator_not_found:	return (ERROR_TERMINATOR_NOT_FOUND);
    case syserr_not_direct_child:	return (ERROR_NOT_DIRECT_CHILD);
    case syserr_delay_free:	return (ERROR_DELAY_FREE);
    case syserr_guardpage:	return (ERROR_GUARDPAGE);
    case syserr_swaperror:	return (ERROR_SWAPERROR);
    case syserr_ldrerror:	return (ERROR_LDRERROR);
    case syserr_nomemory:	return (ERROR_NOMEMORY);
    case syserr_noaccess:	return (ERROR_NOACCESS);
    case syserr_no_dll_term:	return (ERROR_NO_DLL_TERM);
    case syserr_cpsio_code_page_invalid:	return (ERROR_CPSIO_CODE_PAGE_INVALID);
    case syserr_cpsio_no_spooler:	return (ERROR_CPSIO_NO_SPOOLER);
    case syserr_cpsio_font_id_invalid:	return (ERROR_CPSIO_FONT_ID_INVALID);
    case syserr_cpsio_internal_error:	return (ERROR_CPSIO_INTERNAL_ERROR);
    case syserr_cpsio_invalid_ptr_name:	return (ERROR_CPSIO_INVALID_PTR_NAME);
    case syserr_cpsio_not_active:	return (ERROR_CPSIO_NOT_ACTIVE);
    case syserr_cpsio_pid_full:	return (ERROR_CPSIO_PID_FULL);
    case syserr_cpsio_pid_not_found:	return (ERROR_CPSIO_PID_NOT_FOUND);
    case syserr_cpsio_read_ctl_seq:	return (ERROR_CPSIO_READ_CTL_SEQ);
    case syserr_cpsio_read_fnt_def:	return (ERROR_CPSIO_READ_FNT_DEF);
    case syserr_cpsio_write_error:	return (ERROR_CPSIO_WRITE_ERROR);
    case syserr_cpsio_write_full_error:	return (ERROR_CPSIO_WRITE_FULL_ERROR);
    case syserr_cpsio_write_handle_bad:	return (ERROR_CPSIO_WRITE_HANDLE_BAD);
    case syserr_cpsio_swit_load:	return (ERROR_CPSIO_SWIT_LOAD);
    case syserr_cpsio_inv_command:	return (ERROR_CPSIO_INV_COMMAND);
    case syserr_cpsio_no_font_swit:	return (ERROR_CPSIO_NO_FONT_SWIT);
    case syserr_entry_is_callgate:	return (ERROR_ENTRY_IS_CALLGATE);

#ifndef DISABLE_SOCKET_SUPPORT
    case syserr_socket_perm:		return (SOCEPERM);
    case syserr_socket_srch:		return (SOCESRCH);
    case syserr_socket_intr:		return (SOCEINTR);
    case syserr_socket_nxio:		return (SOCENXIO);
    case syserr_socket_badf:		return (SOCEBADF);
    case syserr_socket_acces:		return (SOCEACCES);
    case syserr_socket_fault:		return (SOCEFAULT);
    case syserr_socket_inval:		return (SOCEINVAL);
    case syserr_socket_mfile:		return (SOCEMFILE);
    case syserr_socket_pipe:		return (SOCEPIPE);
    case syserr_socket_os2err:		return (SOCEOS2ERR);
    case syserr_socket_wouldblock:	return (SOCEWOULDBLOCK);
    case syserr_socket_inprogress:	return (SOCEINPROGRESS);
    case syserr_socket_already:		return (SOCEALREADY);
    case syserr_socket_notsock:		return (SOCENOTSOCK);
    case syserr_socket_destaddrreq:	return (SOCEDESTADDRREQ);
    case syserr_socket_msgsize:		return (SOCEMSGSIZE);
    case syserr_socket_prototype:	return (SOCEPROTOTYPE);
    case syserr_socket_noprotoopt:	return (SOCENOPROTOOPT);
    case syserr_socket_protonosupport:	return (SOCEPROTONOSUPPORT);
    case syserr_socket_socktnosupport:	return (SOCESOCKTNOSUPPORT);
    case syserr_socket_opnotsupp:	return (SOCEOPNOTSUPP);
    case syserr_socket_pfnosupport:	return (SOCEPFNOSUPPORT);
    case syserr_socket_afnosupport:	return (SOCEAFNOSUPPORT);
    case syserr_socket_addrinuse:	return (SOCEADDRINUSE);
    case syserr_socket_addrnotavail:	return (SOCEADDRNOTAVAIL);
    case syserr_socket_netdown:		return (SOCENETDOWN);
    case syserr_socket_netunreach:	return (SOCENETUNREACH);
    case syserr_socket_netreset:	return (SOCENETRESET);
    case syserr_socket_connaborted:	return (SOCECONNABORTED);
    case syserr_socket_connreset:	return (SOCECONNRESET);
    case syserr_socket_nobufs:		return (SOCENOBUFS);
    case syserr_socket_isconn:		return (SOCEISCONN);
    case syserr_socket_notconn:		return (SOCENOTCONN);
    case syserr_socket_shutdown:	return (SOCESHUTDOWN);
    case syserr_socket_toomanyrefs:	return (SOCETOOMANYREFS);
    case syserr_socket_timedout:	return (SOCETIMEDOUT);
    case syserr_socket_connrefused:	return (SOCECONNREFUSED);
    case syserr_socket_loop:		return (SOCELOOP);
    case syserr_socket_nametoolong:	return (SOCENAMETOOLONG);
    case syserr_socket_hostdown:	return (SOCEHOSTDOWN);
    case syserr_socket_hostunreach:	return (SOCEHOSTUNREACH);
    case syserr_socket_notempty:	return (SOCENOTEMPTY);
#endif /* not DISABLE_SOCKET_SUPPORT */

    default:	return (NO_ERROR);
    }
}

#ifdef __GCC2__
/* Grumble... stupid linking bug.  */
#define dos_error_message(rc) 0
#else /* not __GCC2__ */

static const char *
dos_error_message (APIRET rc)
{
  unsigned int blength_increment = 64;
  unsigned int blength = blength_increment;
  char * buffer = (OS2_malloc_noerror (blength));
  ULONG mlength;

  if (buffer == 0)
    return (0);
  while (1)
    {
      if ((dos_get_message
	   (0, 0, buffer, blength, rc, "OSO001.MSG", (&mlength)))
	  != NO_ERROR)
	{
	  OS_free (buffer);
	  return (0);
	}
      if (mlength < blength)
	{
	  while ((mlength > 0) && (isspace (buffer [mlength - 1])))
	    mlength -= 1;
	  buffer = (OS2_realloc_noerror (buffer, (mlength + 1)));
	  if (buffer != 0)
	    (buffer[mlength]) = '\0';
	  return (buffer);
	}
      blength += blength_increment;
      buffer = (OS2_realloc_noerror (buffer, (blength)));
      if (buffer == 0)
	return (0);
    }
}

#endif /* not __GCC2__ */

const char *
OS_error_code_to_message (unsigned int syserr)
{
  static const char * last_message = 0;
  APIRET code = (syserr_to_error_code ((enum syserr_names) syserr));
  if (code == NO_ERROR)
    return (0);
  if (last_message != 0)
    OS_free ((void *) last_message);
  last_message = (dos_error_message (code));
  /* Many of OS/2's error messages are terminated with a period, but
     the runtime system is assuming that the messages have no period,
     and adding its own.  */
  if (last_message != 0)
    {
      unsigned int length = (strlen (last_message));
      if ((length > 0) && ((last_message [length - 1]) == '.'))
	(((char *) last_message) [length - 1]) = '\0';
    }
  return (last_message);
}

/* Machine-generated table, do not edit: */
static char * syserr_names_table [] =
{
  "invalid-function",
  "file-not-found",
  "path-not-found",
  "too-many-open-files",
  "access-denied",
  "invalid-handle",
  "arena-trashed",
  "not-enough-memory",
  "invalid-block",
  "bad-environment",
  "bad-format",
  "invalid-access",
  "invalid-data",
  "invalid-drive",
  "current-directory",
  "not-same-device",
  "no-more-files",
  "write-protect",
  "bad-unit",
  "not-ready",
  "bad-command",
  "crc",
  "bad-length",
  "seek",
  "not-dos-disk",
  "sector-not-found",
  "out-of-paper",
  "write-fault",
  "read-fault",
  "gen-failure",
  "sharing-violation",
  "lock-violation",
  "wrong-disk",
  "fcb-unavailable",
  "sharing-buffer-exceeded",
  "code-page-mismatched",
  "handle-eof",
  "handle-disk-full",
  "not-supported",
  "rem-not-list",
  "dup-name",
  "bad-netpath",
  "network-busy",
  "dev-not-exist",
  "too-many-cmds",
  "adap-hdw-err",
  "bad-net-resp",
  "unexp-net-err",
  "bad-rem-adap",
  "printq-full",
  "no-spool-space",
  "print-cancelled",
  "netname-deleted",
  "network-access-denied",
  "bad-dev-type",
  "bad-net-name",
  "too-many-names",
  "too-many-sess",
  "sharing-paused",
  "req-not-accep",
  "redir-paused",
  "sbcs-att-write-prot",
  "sbcs-general-failure",
  "xga-out-memory",
  "file-exists",
  "dup-fcb",
  "cannot-make",
  "fail-i24",
  "out-of-structures",
  "already-assigned",
  "invalid-password",
  "invalid-parameter",
  "net-write-fault",
  "no-proc-slots",
  "not-frozen",
  "tstovfl",
  "tstdup",
  "no-items",
  "interrupt",
  "device-in-use",
  "too-many-semaphores",
  "excl-sem-already-owned",
  "sem-is-set",
  "too-many-sem-requests",
  "invalid-at-interrupt-time",
  "sem-owner-died",
  "sem-user-limit",
  "disk-change",
  "drive-locked",
  "broken-pipe",
  "open-failed",
  "buffer-overflow",
  "disk-full",
  "no-more-search-handles",
  "invalid-target-handle",
  "protection-violation",
  "viokbd-request",
  "invalid-category",
  "invalid-verify-switch",
  "bad-driver-level",
  "call-not-implemented",
  "sem-timeout",
  "insufficient-buffer",
  "invalid-name",
  "invalid-level",
  "no-volume-label",
  "mod-not-found",
  "proc-not-found",
  "wait-no-children",
  "child-not-complete",
  "direct-access-handle",
  "negative-seek",
  "seek-on-device",
  "is-join-target",
  "is-joined",
  "is-substed",
  "not-joined",
  "not-substed",
  "join-to-join",
  "subst-to-subst",
  "join-to-subst",
  "subst-to-join",
  "busy-drive",
  "same-drive",
  "dir-not-root",
  "dir-not-empty",
  "is-subst-path",
  "is-join-path",
  "path-busy",
  "is-subst-target",
  "system-trace",
  "invalid-event-count",
  "too-many-muxwaiters",
  "invalid-list-format",
  "label-too-long",
  "too-many-tcbs",
  "signal-refused",
  "discarded",
  "not-locked",
  "bad-threadid-addr",
  "bad-arguments",
  "bad-pathname",
  "signal-pending",
  "uncertain-media",
  "max-thrds-reached",
  "monitors-not-supported",
  "unc-driver-not-installed",
  "lock-failed",
  "swapio-failed",
  "swapin-failed",
  "busy",
  "cancel-violation",
  "atomic-lock-not-supported",
  "read-locks-not-supported",
  "invalid-segment-number",
  "invalid-callgate",
  "invalid-ordinal",
  "already-exists",
  "no-child-process",
  "child-alive-nowait",
  "invalid-flag-number",
  "sem-not-found",
  "invalid-starting-codeseg",
  "invalid-stackseg",
  "invalid-moduletype",
  "invalid-exe-signature",
  "exe-marked-invalid",
  "bad-exe-format",
  "iterated-data-exceeds-64k",
  "invalid-minallocsize",
  "dynlink-from-invalid-ring",
  "iopl-not-enabled",
  "invalid-segdpl",
  "autodataseg-exceeds-64k",
  "ring2seg-must-be-movable",
  "reloc-chain-xeeds-seglim",
  "infloop-in-reloc-chain",
  "envvar-not-found",
  "not-current-ctry",
  "no-signal-sent",
  "filename-exced-range",
  "ring2-stack-in-use",
  "meta-expansion-too-long",
  "invalid-signal-number",
  "thread-1-inactive",
  "info-not-avail",
  "locked",
  "bad-dynalink",
  "too-many-modules",
  "nesting-not-allowed",
  "cannot-shrink",
  "zombie-process",
  "stack-in-high-memory",
  "invalid-exitroutine-ring",
  "getbuf-failed",
  "flushbuf-failed",
  "transfer-too-long",
  "forcenoswap-failed",
  "smg-no-target-window",
  "no-children",
  "invalid-screen-group",
  "bad-pipe",
  "pipe-busy",
  "no-data",
  "pipe-not-connected",
  "more-data",
  "vc-disconnected",
  "circularity-requested",
  "directory-in-cds",
  "invalid-fsd-name",
  "invalid-path",
  "invalid-ea-name",
  "ea-list-inconsistent",
  "ea-list-too-long",
  "no-meta-match",
  "findnotify-timeout",
  "no-more-items",
  "search-struc-reused",
  "char-not-found",
  "too-much-stack",
  "invalid-attr",
  "invalid-starting-ring",
  "invalid-dll-init-ring",
  "cannot-copy",
  "directory",
  "oplocked-file",
  "oplock-thread-exists",
  "volume-changed",
  "findnotify-handle-in-use",
  "findnotify-handle-closed",
  "notify-object-removed",
  "already-shutdown",
  "eas-didnt-fit",
  "ea-file-corrupt",
  "ea-table-full",
  "invalid-ea-handle",
  "no-cluster",
  "create-ea-file",
  "cannot-open-ea-file",
  "eas-not-supported",
  "need-eas-found",
  "duplicate-handle",
  "duplicate-name",
  "empty-muxwait",
  "mutex-owned",
  "not-owner",
  "param-too-small",
  "too-many-handles",
  "too-many-opens",
  "wrong-type",
  "unused-code",
  "thread-not-terminated",
  "init-routine-failed",
  "module-in-use",
  "not-enough-watchpoints",
  "too-many-posts",
  "already-posted",
  "already-reset",
  "sem-busy",
  "invalid-procid",
  "invalid-pdelta",
  "not-descendant",
  "not-session-manager",
  "invalid-pclass",
  "invalid-scope",
  "invalid-threadid",
  "dossub-shrink",
  "dossub-nomem",
  "dossub-overlap",
  "dossub-badsize",
  "dossub-badflag",
  "dossub-badselector",
  "mr-msg-too-long",
  "mr-mid-not-found",
  "mr-un-acc-msgf",
  "mr-inv-msgf-format",
  "mr-inv-ivcount",
  "mr-un-perform",
  "ts-wakeup",
  "ts-semhandle",
  "ts-notimer",
  "ts-handle",
  "ts-datetime",
  "sys-internal",
  "que-current-name",
  "que-proc-not-owned",
  "que-proc-owned",
  "que-duplicate",
  "que-element-not-exist",
  "que-no-memory",
  "que-invalid-name",
  "que-invalid-priority",
  "que-invalid-handle",
  "que-link-not-found",
  "que-memory-error",
  "que-prev-at-end",
  "que-proc-no-access",
  "que-empty",
  "que-name-not-exist",
  "que-not-initialized",
  "que-unable-to-access",
  "que-unable-to-add",
  "que-unable-to-init",
  "vio-invalid-mask",
  "vio-ptr",
  "vio-aptr",
  "vio-rptr",
  "vio-cptr",
  "vio-lptr",
  "vio-mode",
  "vio-width",
  "vio-attr",
  "vio-row",
  "vio-col",
  "vio-toprow",
  "vio-botrow",
  "vio-rightcol",
  "vio-leftcol",
  "scs-call",
  "scs-value",
  "vio-wait-flag",
  "vio-unlock",
  "sgs-not-session-mgr",
  "smg-invalid-session-id",
  "smg-no-sessions",
  "smg-session-not-found",
  "smg-set-title",
  "kbd-parameter",
  "kbd-no-device",
  "kbd-invalid-iowait",
  "kbd-invalid-length",
  "kbd-invalid-echo-mask",
  "kbd-invalid-input-mask",
  "mon-invalid-parms",
  "mon-invalid-devname",
  "mon-invalid-handle",
  "mon-buffer-too-small",
  "mon-buffer-empty",
  "mon-data-too-large",
  "mouse-no-device",
  "mouse-inv-handle",
  "mouse-inv-parms",
  "mouse-cant-reset",
  "mouse-display-parms",
  "mouse-inv-module",
  "mouse-inv-entry-pt",
  "mouse-inv-mask",
  "mouse-no-data",
  "mouse-ptr-drawn",
  "invalid-frequency",
  "nls-no-country-file",
  "nls-open-failed",
  "no-country-or-codepage",
  "nls-table-truncated",
  "nls-bad-type",
  "nls-type-not-found",
  "vio-smg-only",
  "vio-invalid-asciiz",
  "vio-deregister",
  "vio-no-popup",
  "vio-existing-popup",
  "kbd-smg-only",
  "kbd-invalid-asciiz",
  "kbd-invalid-mask",
  "kbd-register",
  "kbd-deregister",
  "mouse-smg-only",
  "mouse-invalid-asciiz",
  "mouse-invalid-mask",
  "mouse-register",
  "mouse-deregister",
  "smg-bad-action",
  "smg-invalid-call",
  "scs-sg-notfound",
  "scs-not-shell",
  "vio-invalid-parms",
  "vio-function-owned",
  "vio-return",
  "scs-invalid-function",
  "scs-not-session-mgr",
  "vio-register",
  "vio-no-mode-thread",
  "vio-no-save-restore-thd",
  "vio-in-bg",
  "vio-illegal-during-popup",
  "smg-not-baseshell",
  "smg-bad-statusreq",
  "que-invalid-wait",
  "vio-lock",
  "mouse-invalid-iowait",
  "vio-invalid-handle",
  "vio-illegal-during-lock",
  "vio-invalid-length",
  "kbd-invalid-handle",
  "kbd-no-more-handle",
  "kbd-cannot-create-kcb",
  "kbd-codepage-load-incompl",
  "kbd-invalid-codepage-id",
  "kbd-no-codepage-support",
  "kbd-focus-required",
  "kbd-focus-already-active",
  "kbd-keyboard-busy",
  "kbd-invalid-codepage",
  "kbd-unable-to-focus",
  "smg-session-non-select",
  "smg-session-not-foregrnd",
  "smg-session-not-parent",
  "smg-invalid-start-mode",
  "smg-invalid-related-opt",
  "smg-invalid-bond-option",
  "smg-invalid-select-opt",
  "smg-start-in-background",
  "smg-invalid-stop-option",
  "smg-bad-reserve",
  "smg-process-not-parent",
  "smg-invalid-data-length",
  "smg-not-bound",
  "smg-retry-sub-alloc",
  "kbd-detached",
  "vio-detached",
  "mou-detached",
  "vio-font",
  "vio-user-font",
  "vio-bad-cp",
  "vio-no-cp",
  "vio-na-cp",
  "invalid-code-page",
  "cplist-too-small",
  "cp-not-moved",
  "mode-switch-init",
  "code-page-not-found",
  "unexpected-slot-returned",
  "smg-invalid-trace-option",
  "vio-internal-resource",
  "vio-shell-init",
  "smg-no-hard-errors",
  "cp-switch-incomplete",
  "vio-transparent-popup",
  "critsec-overflow",
  "critsec-underflow",
  "vio-bad-reserve",
  "invalid-address",
  "zero-selectors-requested",
  "not-enough-selectors-ava",
  "invalid-selector",
  "smg-invalid-program-type",
  "smg-invalid-pgm-control",
  "smg-invalid-inherit-opt",
  "vio-extended-sg",
  "vio-not-pres-mgr-sg",
  "vio-shield-owned",
  "vio-no-more-handles",
  "vio-see-error-log",
  "vio-associated-dc",
  "kbd-no-console",
  "mouse-no-console",
  "mouse-invalid-handle",
  "smg-invalid-debug-parms",
  "kbd-extended-sg",
  "mou-extended-sg",
  "smg-invalid-icon-file",
  "trc-pid-non-existent",
  "trc-count-active",
  "trc-suspended-by-count",
  "trc-count-inactive",
  "trc-count-reached",
  "no-mc-trace",
  "mc-trace",
  "trc-count-zero",
  "smg-too-many-dds",
  "smg-invalid-notification",
  "lf-invalid-function",
  "lf-not-avail",
  "lf-suspended",
  "lf-buf-too-small",
  "lf-buffer-full",
  "lf-invalid-record",
  "lf-invalid-service",
  "lf-general-failure",
  "lf-invalid-id",
  "lf-invalid-handle",
  "lf-no-id-avail",
  "lf-template-area-full",
  "lf-id-in-use",
  "mou-not-initialized",
  "mouinitreal-done",
  "dossub-corrupted",
  "mouse-caller-not-subsys",
  "arithmetic-overflow",
  "tmr-no-device",
  "tmr-invalid-time",
  "pvw-invalid-entity",
  "pvw-invalid-entity-type",
  "pvw-invalid-spec",
  "pvw-invalid-range-type",
  "pvw-invalid-counter-blk",
  "pvw-invalid-text-blk",
  "prf-not-initialized",
  "prf-already-initialized",
  "prf-not-started",
  "prf-already-started",
  "prf-timer-out-of-range",
  "prf-timer-reset",
  "vdd-lock-useage-denied",
  "timeout",
  "vdm-down",
  "vdm-limit",
  "vdd-not-found",
  "invalid-caller",
  "pid-mismatch",
  "invalid-vdd-handle",
  "vlpt-no-spooler",
  "vcom-device-busy",
  "vlpt-device-busy",
  "nesting-too-deep",
  "vdd-missing",
  "bidi-invalid-length",
  "bidi-invalid-increment",
  "bidi-invalid-combination",
  "bidi-invalid-reserved",
  "bidi-invalid-effect",
  "bidi-invalid-csdrec",
  "bidi-invalid-csdstate",
  "bidi-invalid-level",
  "bidi-invalid-type-support",
  "bidi-invalid-orientation",
  "bidi-invalid-num-shape",
  "bidi-invalid-csd",
  "bidi-no-support",
  "bidi-rw-incomplete",
  "imp-invalid-parm",
  "imp-invalid-length",
  "hpfs-disk-error-warn",
  "mon-bad-buffer",
  "module-corrupted",
  "sm-outof-swapfile",
  "lf-timeout",
  "lf-suspend-success",
  "lf-resume-success",
  "lf-redirect-success",
  "lf-redirect-failure",
  "swapper-not-active",
  "invalid-swapid",
  "ioerr-swap-file",
  "swap-table-full",
  "swap-file-full",
  "cant-init-swapper",
  "swapper-already-init",
  "pmm-insufficient-memory",
  "pmm-invalid-flags",
  "pmm-invalid-address",
  "pmm-lock-failed",
  "pmm-unlock-failed",
  "pmm-move-incomplete",
  "ucom-drive-renamed",
  "ucom-filename-truncated",
  "ucom-buffer-length",
  "mon-chain-handle",
  "mon-not-registered",
  "smg-already-top",
  "pmm-arena-modified",
  "smg-printer-open",
  "pmm-set-flags-failed",
  "invalid-dos-dd",
  "blocked",
  "noblock",
  "instance-shared",
  "no-object",
  "partial-attach",
  "incache",
  "swap-io-problems",
  "crosses-object-boundary",
  "longlock",
  "shortlock",
  "uvirtlock",
  "aliaslock",
  "alias",
  "no-more-handles",
  "scan-terminated",
  "terminator-not-found",
  "not-direct-child",
  "delay-free",
  "guardpage",
  "swaperror",
  "ldrerror",
  "nomemory",
  "noaccess",
  "no-dll-term",
  "cpsio-code-page-invalid",
  "cpsio-no-spooler",
  "cpsio-font-id-invalid",
  "cpsio-internal-error",
  "cpsio-invalid-ptr-name",
  "cpsio-not-active",
  "cpsio-pid-full",
  "cpsio-pid-not-found",
  "cpsio-read-ctl-seq",
  "cpsio-read-fnt-def",
  "cpsio-write-error",
  "cpsio-write-full-error",
  "cpsio-write-handle-bad",
  "cpsio-swit-load",
  "cpsio-inv-command",
  "cpsio-no-font-swit",
  "entry-is-callgate",

  /* socket errors: */
  "soceperm",
  "socesrch",
  "soceintr",
  "socenxio",
  "socebadf",
  "soceacces",
  "socefault",
  "soceinval",
  "socemfile",
  "socepipe",
  "soceos2err",
  "socewouldblock",
  "soceinprogress",
  "socealready",
  "socenotsock",
  "socedestaddrreq",
  "socemsgsize",
  "soceprototype",
  "socenoprotoopt",
  "soceprotonosupport",
  "socesocktnosupport",
  "soceopnotsupp",
  "socepfnosupport",
  "soceafnosupport",
  "soceaddrinuse",
  "soceaddrnotavail",
  "socenetdown",
  "socenetunreach",
  "socenetreset",
  "soceconnaborted",
  "soceconnreset",
  "socenobufs",
  "soceisconn",
  "socenotconn",
  "soceshutdown",
  "socetoomanyrefs",
  "socetimedout",
  "soceconnrefused",
  "soceloop",
  "socenametoolong",
  "socehostdown",
  "socehostunreach",
  "socenotempty",

  "unknown"
};

void
OS_syserr_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syserr_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syserr_names_table);
}

void
OS_syscall_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syscall_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syscall_names_table);
}
