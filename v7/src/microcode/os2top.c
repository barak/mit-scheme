/* -*-C-*-

$Id: os2top.c,v 1.3 1994/12/02 20:40:20 cph Exp $

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
#include "ostop.h"
#include "option.h"

extern void OS2_initialize_channels (void);
extern void OS2_initialize_channel_thread_messages (void);
extern void OS2_initialize_console (void);
extern void OS2_initialize_directory_reader (void);
extern void OS2_initialize_environment (void);
extern void OS2_initialize_keyboard_interrupts (void);
extern void OS2_initialize_message_queues (void);
extern void OS2_initialize_pm_thread (void);
extern void OS2_initialize_scheme_thread (void);
extern void OS2_initialize_tty (void);

extern const char * OS_Name;
extern const char * OS_Variant;

static const char * OS2_version_string (void);
static void initialize_locks (void);

int
OS_under_emacs_p (void)
{
  return (option_emacs_subprocess);
}

void
OS_initialize (void)
{
  initialize_locks ();
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
  if ((major == 20) && (minor == 30))
    {
      major = 30;
      minor = 0;
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

void
OS2_exit_scheme (int value)
{
  extern void OS2_kill_timer_thread (void);
  OS2_kill_timer_thread ();
  OS2_channel_close_all_noerror ();
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

long
OS_set_trap_state (long arg)
{
  return (arg);
}

#ifdef __IBMC__
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
  interrupt_registers_lock = (OS2_create_mutex_semaphore ());
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
    case ERROR_ITERATED_DATA_EXCEEDS_64k:	return (syserr_iterated_data_exceeds_64k);
    case ERROR_INVALID_MINALLOCSIZE:	return (syserr_invalid_minallocsize);
    case ERROR_DYNLINK_FROM_INVALID_RING:	return (syserr_dynlink_from_invalid_ring);
    case ERROR_IOPL_NOT_ENABLED:	return (syserr_iopl_not_enabled);
    case ERROR_INVALID_SEGDPL:	return (syserr_invalid_segdpl);
    case ERROR_AUTODATASEG_EXCEEDS_64k:	return (syserr_autodataseg_exceeds_64k);
    case ERROR_RING2SEG_MUST_BE_MOVABLE:	return (syserr_ring2seg_must_be_movable);
    case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:	return (syserr_reloc_chain_xeeds_seglim);
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
    case ERROR_NO_COUNTRY_OR_CODEPAGE:	return (syserr_no_country_or_codepage);
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
    case MSG_HPFS_DISK_ERROR_WARN:	return (syserr_hpfs_disk_error_warn);
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
    default:	return (syserr_unknown);
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
  char * buffer = (malloc (blength));
  ULONG mlength;

  if (buffer == 0)
    return (0);
  while (1)
    {
      if ((dos_get_message
	   (0, 0, buffer, blength, rc, "OSO001.MSG", (&mlength)))
	  != NO_ERROR)
	{
	  free (buffer);
	  return (0);
	}
      if (mlength < blength)
	{
	  while ((mlength > 0) && (isspace (buffer [mlength - 1])))
	    mlength -= 1;
	  buffer = (realloc (buffer, (mlength + 1)));
	  if (buffer != 0)
	    (buffer[mlength]) = '\0';
	  return (buffer);
	}
      blength += blength_increment;
      buffer = (realloc (buffer, (blength)));
      if (buffer == 0)
	return (0);
    }
}

#endif /* not __GCC2__ */

const char *
OS_error_code_to_message (unsigned int syserr)
{
  static const char * last_message = 0;
  if (last_message != 0)
    free ((void *) last_message);
  last_message = (dos_error_message (syserr));
  return (last_message);
}

static char * syscall_names_table [] =
{
  "dos-async-timer",
  "dos-close",
  "dos-close-event-sem",
  "dos-close-mutex-sem",
  "dos-close-queue",
  "dos-create-dir",
  "dos-create-event-sem",
  "dos-create-mutex-sem",
  "dos-create-pipe",
  "dos-create-queue",
  "dos-create-thread",
  "dos-delete",
  "dos-delete-dir",
  "dos-exit",
  "dos-find-close",
  "dos-find-first",
  "dos-find-next",
  "dos-get-info-blocks",
  "dos-get-message",
  "dos-kill-thread",
  "dos-move",
  "dos-open",
  "dos-post-event-sem",
  "dos-query-current-dir",
  "dos-query-current-disk",
  "dos-query-file-info",
  "dos-query-h-type",
  "dos-query-n-p-h-state",
  "dos-query-path-info",
  "dos-query-sys-info",
  "dos-read",
  "dos-read-queue",
  "dos-release-mutex-sem",
  "dos-request-mutex-sem",
  "dos-reset-event-sem",
  "dos-scan-env",
  "dos-set-current-dir",
  "dos-set-default-disk",
  "dos-set-file-ptr",
  "dos-set-file-size",
  "dos-set-path-info",
  "dos-start-timer",
  "dos-stop-timer",
  "dos-wait-event-sem",
  "dos-write",
  "dos-write-queue",
  "beginthread",
  "kbd-char-in",
  "localtime",
  "malloc",
  "mktime",
  "realloc",
  "time",
  "vio-wrt-tty"
};

void
OS_syscall_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syscall_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syscall_names_table);
}

/* Machine-generated table, do not edit: */
static char * syserr_names_table [] =
{
  "INVALID-FUNCTION",
  "FILE-NOT-FOUND",
  "PATH-NOT-FOUND",
  "TOO-MANY-OPEN-FILES",
  "ACCESS-DENIED",
  "INVALID-HANDLE",
  "ARENA-TRASHED",
  "NOT-ENOUGH-MEMORY",
  "INVALID-BLOCK",
  "BAD-ENVIRONMENT",
  "BAD-FORMAT",
  "INVALID-ACCESS",
  "INVALID-DATA",
  "INVALID-DRIVE",
  "CURRENT-DIRECTORY",
  "NOT-SAME-DEVICE",
  "NO-MORE-FILES",
  "WRITE-PROTECT",
  "BAD-UNIT",
  "NOT-READY",
  "BAD-COMMAND",
  "CRC",
  "BAD-LENGTH",
  "SEEK",
  "NOT-DOS-DISK",
  "SECTOR-NOT-FOUND",
  "OUT-OF-PAPER",
  "WRITE-FAULT",
  "READ-FAULT",
  "GEN-FAILURE",
  "SHARING-VIOLATION",
  "LOCK-VIOLATION",
  "WRONG-DISK",
  "FCB-UNAVAILABLE",
  "SHARING-BUFFER-EXCEEDED",
  "CODE-PAGE-MISMATCHED",
  "HANDLE-EOF",
  "HANDLE-DISK-FULL",
  "NOT-SUPPORTED",
  "REM-NOT-LIST",
  "DUP-NAME",
  "BAD-NETPATH",
  "NETWORK-BUSY",
  "DEV-NOT-EXIST",
  "TOO-MANY-CMDS",
  "ADAP-HDW-ERR",
  "BAD-NET-RESP",
  "UNEXP-NET-ERR",
  "BAD-REM-ADAP",
  "PRINTQ-FULL",
  "NO-SPOOL-SPACE",
  "PRINT-CANCELLED",
  "NETNAME-DELETED",
  "NETWORK-ACCESS-DENIED",
  "BAD-DEV-TYPE",
  "BAD-NET-NAME",
  "TOO-MANY-NAMES",
  "TOO-MANY-SESS",
  "SHARING-PAUSED",
  "REQ-NOT-ACCEP",
  "REDIR-PAUSED",
  "SBCS-ATT-WRITE-PROT",
  "SBCS-GENERAL-FAILURE",
  "XGA-OUT-MEMORY",
  "FILE-EXISTS",
  "DUP-FCB",
  "CANNOT-MAKE",
  "FAIL-I24",
  "OUT-OF-STRUCTURES",
  "ALREADY-ASSIGNED",
  "INVALID-PASSWORD",
  "INVALID-PARAMETER",
  "NET-WRITE-FAULT",
  "NO-PROC-SLOTS",
  "NOT-FROZEN",
  "SYS-COMP-NOT-LOADED",
  "TSTOVFL",
  "TSTDUP",
  "NO-ITEMS",
  "INTERRUPT",
  "DEVICE-IN-USE",
  "TOO-MANY-SEMAPHORES",
  "EXCL-SEM-ALREADY-OWNED",
  "SEM-IS-SET",
  "TOO-MANY-SEM-REQUESTS",
  "INVALID-AT-INTERRUPT-TIME",
  "SEM-OWNER-DIED",
  "SEM-USER-LIMIT",
  "DISK-CHANGE",
  "DRIVE-LOCKED",
  "BROKEN-PIPE",
  "OPEN-FAILED",
  "BUFFER-OVERFLOW",
  "DISK-FULL",
  "NO-MORE-SEARCH-HANDLES",
  "INVALID-TARGET-HANDLE",
  "PROTECTION-VIOLATION",
  "VIOKBD-REQUEST",
  "INVALID-CATEGORY",
  "INVALID-VERIFY-SWITCH",
  "BAD-DRIVER-LEVEL",
  "CALL-NOT-IMPLEMENTED",
  "SEM-TIMEOUT",
  "INSUFFICIENT-BUFFER",
  "INVALID-NAME",
  "INVALID-LEVEL",
  "NO-VOLUME-LABEL",
  "MOD-NOT-FOUND",
  "PROC-NOT-FOUND",
  "WAIT-NO-CHILDREN",
  "CHILD-NOT-COMPLETE",
  "DIRECT-ACCESS-HANDLE",
  "NEGATIVE-SEEK",
  "SEEK-ON-DEVICE",
  "IS-JOIN-TARGET",
  "IS-JOINED",
  "IS-SUBSTED",
  "NOT-JOINED",
  "NOT-SUBSTED",
  "JOIN-TO-JOIN",
  "SUBST-TO-SUBST",
  "JOIN-TO-SUBST",
  "SUBST-TO-JOIN",
  "BUSY-DRIVE",
  "SAME-DRIVE",
  "DIR-NOT-ROOT",
  "DIR-NOT-EMPTY",
  "IS-SUBST-PATH",
  "IS-JOIN-PATH",
  "PATH-BUSY",
  "IS-SUBST-TARGET",
  "SYSTEM-TRACE",
  "INVALID-EVENT-COUNT",
  "TOO-MANY-MUXWAITERS",
  "INVALID-LIST-FORMAT",
  "LABEL-TOO-LONG",
  "TOO-MANY-TCBS",
  "SIGNAL-REFUSED",
  "DISCARDED",
  "NOT-LOCKED",
  "BAD-THREADID-ADDR",
  "BAD-ARGUMENTS",
  "BAD-PATHNAME",
  "SIGNAL-PENDING",
  "UNCERTAIN-MEDIA",
  "MAX-THRDS-REACHED",
  "MONITORS-NOT-SUPPORTED",
  "UNC-DRIVER-NOT-INSTALLED",
  "LOCK-FAILED",
  "SWAPIO-FAILED",
  "SWAPIN-FAILED",
  "BUSY",
  "CANCEL-VIOLATION",
  "ATOMIC-LOCK-NOT-SUPPORTED",
  "READ-LOCKS-NOT-SUPPORTED",
  "INVALID-SEGMENT-NUMBER",
  "INVALID-CALLGATE",
  "INVALID-ORDINAL",
  "ALREADY-EXISTS",
  "NO-CHILD-PROCESS",
  "CHILD-ALIVE-NOWAIT",
  "INVALID-FLAG-NUMBER",
  "SEM-NOT-FOUND",
  "INVALID-STARTING-CODESEG",
  "INVALID-STACKSEG",
  "INVALID-MODULETYPE",
  "INVALID-EXE-SIGNATURE",
  "EXE-MARKED-INVALID",
  "BAD-EXE-FORMAT",
  "ITERATED-DATA-EXCEEDS-64K",
  "INVALID-MINALLOCSIZE",
  "DYNLINK-FROM-INVALID-RING",
  "IOPL-NOT-ENABLED",
  "INVALID-SEGDPL",
  "AUTODATASEG-EXCEEDS-64K",
  "RING2SEG-MUST-BE-MOVABLE",
  "RELOC-CHAIN-XEEDS-SEGLIM",
  "INFLOOP-IN-RELOC-CHAIN",
  "ENVVAR-NOT-FOUND",
  "NOT-CURRENT-CTRY",
  "NO-SIGNAL-SENT",
  "FILENAME-EXCED-RANGE",
  "RING2-STACK-IN-USE",
  "META-EXPANSION-TOO-LONG",
  "INVALID-SIGNAL-NUMBER",
  "THREAD-1-INACTIVE",
  "INFO-NOT-AVAIL",
  "LOCKED",
  "BAD-DYNALINK",
  "TOO-MANY-MODULES",
  "NESTING-NOT-ALLOWED",
  "CANNOT-SHRINK",
  "ZOMBIE-PROCESS",
  "STACK-IN-HIGH-MEMORY",
  "INVALID-EXITROUTINE-RING",
  "GETBUF-FAILED",
  "FLUSHBUF-FAILED",
  "TRANSFER-TOO-LONG",
  "FORCENOSWAP-FAILED",
  "SMG-NO-TARGET-WINDOW",
  "NO-CHILDREN",
  "INVALID-SCREEN-GROUP",
  "BAD-PIPE",
  "PIPE-BUSY",
  "NO-DATA",
  "PIPE-NOT-CONNECTED",
  "MORE-DATA",
  "VC-DISCONNECTED",
  "CIRCULARITY-REQUESTED",
  "DIRECTORY-IN-CDS",
  "INVALID-FSD-NAME",
  "INVALID-PATH",
  "INVALID-EA-NAME",
  "EA-LIST-INCONSISTENT",
  "EA-LIST-TOO-LONG",
  "NO-META-MATCH",
  "FINDNOTIFY-TIMEOUT",
  "NO-MORE-ITEMS",
  "SEARCH-STRUC-REUSED",
  "CHAR-NOT-FOUND",
  "TOO-MUCH-STACK",
  "INVALID-ATTR",
  "INVALID-STARTING-RING",
  "INVALID-DLL-INIT-RING",
  "CANNOT-COPY",
  "DIRECTORY",
  "OPLOCKED-FILE",
  "OPLOCK-THREAD-EXISTS",
  "VOLUME-CHANGED",
  "FINDNOTIFY-HANDLE-IN-USE",
  "FINDNOTIFY-HANDLE-CLOSED",
  "NOTIFY-OBJECT-REMOVED",
  "ALREADY-SHUTDOWN",
  "EAS-DIDNT-FIT",
  "EA-FILE-CORRUPT",
  "EA-TABLE-FULL",
  "INVALID-EA-HANDLE",
  "NO-CLUSTER",
  "CREATE-EA-FILE",
  "CANNOT-OPEN-EA-FILE",
  "EAS-NOT-SUPPORTED",
  "NEED-EAS-FOUND",
  "DUPLICATE-HANDLE",
  "DUPLICATE-NAME",
  "EMPTY-MUXWAIT",
  "MUTEX-OWNED",
  "NOT-OWNER",
  "PARAM-TOO-SMALL",
  "TOO-MANY-HANDLES",
  "TOO-MANY-OPENS",
  "WRONG-TYPE",
  "UNUSED-CODE",
  "THREAD-NOT-TERMINATED",
  "INIT-ROUTINE-FAILED",
  "MODULE-IN-USE",
  "NOT-ENOUGH-WATCHPOINTS",
  "TOO-MANY-POSTS",
  "ALREADY-POSTED",
  "ALREADY-RESET",
  "SEM-BUSY",
  "INVALID-PROCID",
  "INVALID-PDELTA",
  "NOT-DESCENDANT",
  "NOT-SESSION-MANAGER",
  "INVALID-PCLASS",
  "INVALID-SCOPE",
  "INVALID-THREADID",
  "DOSSUB-SHRINK",
  "DOSSUB-NOMEM",
  "DOSSUB-OVERLAP",
  "DOSSUB-BADSIZE",
  "DOSSUB-BADFLAG",
  "DOSSUB-BADSELECTOR",
  "MR-MSG-TOO-LONG",
  "MR-MID-NOT-FOUND",
  "MR-UN-ACC-MSGF",
  "MR-INV-MSGF-FORMAT",
  "MR-INV-IVCOUNT",
  "MR-UN-PERFORM",
  "TS-WAKEUP",
  "TS-SEMHANDLE",
  "TS-NOTIMER",
  "TS-HANDLE",
  "TS-DATETIME",
  "SYS-INTERNAL",
  "QUE-CURRENT-NAME",
  "QUE-PROC-NOT-OWNED",
  "QUE-PROC-OWNED",
  "QUE-DUPLICATE",
  "QUE-ELEMENT-NOT-EXIST",
  "QUE-NO-MEMORY",
  "QUE-INVALID-NAME",
  "QUE-INVALID-PRIORITY",
  "QUE-INVALID-HANDLE",
  "QUE-LINK-NOT-FOUND",
  "QUE-MEMORY-ERROR",
  "QUE-PREV-AT-END",
  "QUE-PROC-NO-ACCESS",
  "QUE-EMPTY",
  "QUE-NAME-NOT-EXIST",
  "QUE-NOT-INITIALIZED",
  "QUE-UNABLE-TO-ACCESS",
  "QUE-UNABLE-TO-ADD",
  "QUE-UNABLE-TO-INIT",
  "VIO-INVALID-MASK",
  "VIO-PTR",
  "VIO-APTR",
  "VIO-RPTR",
  "VIO-CPTR",
  "VIO-LPTR",
  "VIO-MODE",
  "VIO-WIDTH",
  "VIO-ATTR",
  "VIO-ROW",
  "VIO-COL",
  "VIO-TOPROW",
  "VIO-BOTROW",
  "VIO-RIGHTCOL",
  "VIO-LEFTCOL",
  "SCS-CALL",
  "SCS-VALUE",
  "VIO-WAIT-FLAG",
  "VIO-UNLOCK",
  "SGS-NOT-SESSION-MGR",
  "SMG-INVALID-SESSION-ID",
  "SMG-NO-SESSIONS",
  "SMG-SESSION-NOT-FOUND",
  "SMG-SET-TITLE",
  "KBD-PARAMETER",
  "KBD-NO-DEVICE",
  "KBD-INVALID-IOWAIT",
  "KBD-INVALID-LENGTH",
  "KBD-INVALID-ECHO-MASK",
  "KBD-INVALID-INPUT-MASK",
  "MON-INVALID-PARMS",
  "MON-INVALID-DEVNAME",
  "MON-INVALID-HANDLE",
  "MON-BUFFER-TOO-SMALL",
  "MON-BUFFER-EMPTY",
  "MON-DATA-TOO-LARGE",
  "MOUSE-NO-DEVICE",
  "MOUSE-INV-HANDLE",
  "MOUSE-INV-PARMS",
  "MOUSE-CANT-RESET",
  "MOUSE-DISPLAY-PARMS",
  "MOUSE-INV-MODULE",
  "MOUSE-INV-ENTRY-PT",
  "MOUSE-INV-MASK",
  "MOUSE-NO-DATA",
  "MOUSE-PTR-DRAWN",
  "INVALID-FREQUENCY",
  "NLS-NO-COUNTRY-FILE",
  "NLS-OPEN-FAILED",
  "NO-COUNTRY-OR-CODEPAGE",
  "NLS-TABLE-TRUNCATED",
  "NLS-BAD-TYPE",
  "NLS-TYPE-NOT-FOUND",
  "VIO-SMG-ONLY",
  "VIO-INVALID-ASCIIZ",
  "VIO-DEREGISTER",
  "VIO-NO-POPUP",
  "VIO-EXISTING-POPUP",
  "KBD-SMG-ONLY",
  "KBD-INVALID-ASCIIZ",
  "KBD-INVALID-MASK",
  "KBD-REGISTER",
  "KBD-DEREGISTER",
  "MOUSE-SMG-ONLY",
  "MOUSE-INVALID-ASCIIZ",
  "MOUSE-INVALID-MASK",
  "MOUSE-REGISTER",
  "MOUSE-DEREGISTER",
  "SMG-BAD-ACTION",
  "SMG-INVALID-CALL",
  "SCS-SG-NOTFOUND",
  "SCS-NOT-SHELL",
  "VIO-INVALID-PARMS",
  "VIO-FUNCTION-OWNED",
  "VIO-RETURN",
  "SCS-INVALID-FUNCTION",
  "SCS-NOT-SESSION-MGR",
  "VIO-REGISTER",
  "VIO-NO-MODE-THREAD",
  "VIO-NO-SAVE-RESTORE-THD",
  "VIO-IN-BG",
  "VIO-ILLEGAL-DURING-POPUP",
  "SMG-NOT-BASESHELL",
  "SMG-BAD-STATUSREQ",
  "QUE-INVALID-WAIT",
  "VIO-LOCK",
  "MOUSE-INVALID-IOWAIT",
  "VIO-INVALID-HANDLE",
  "VIO-ILLEGAL-DURING-LOCK",
  "VIO-INVALID-LENGTH",
  "KBD-INVALID-HANDLE",
  "KBD-NO-MORE-HANDLE",
  "KBD-CANNOT-CREATE-KCB",
  "KBD-CODEPAGE-LOAD-INCOMPL",
  "KBD-INVALID-CODEPAGE-ID",
  "KBD-NO-CODEPAGE-SUPPORT",
  "KBD-FOCUS-REQUIRED",
  "KBD-FOCUS-ALREADY-ACTIVE",
  "KBD-KEYBOARD-BUSY",
  "KBD-INVALID-CODEPAGE",
  "KBD-UNABLE-TO-FOCUS",
  "SMG-SESSION-NON-SELECT",
  "SMG-SESSION-NOT-FOREGRND",
  "SMG-SESSION-NOT-PARENT",
  "SMG-INVALID-START-MODE",
  "SMG-INVALID-RELATED-OPT",
  "SMG-INVALID-BOND-OPTION",
  "SMG-INVALID-SELECT-OPT",
  "SMG-START-IN-BACKGROUND",
  "SMG-INVALID-STOP-OPTION",
  "SMG-BAD-RESERVE",
  "SMG-PROCESS-NOT-PARENT",
  "SMG-INVALID-DATA-LENGTH",
  "SMG-NOT-BOUND",
  "SMG-RETRY-SUB-ALLOC",
  "KBD-DETACHED",
  "VIO-DETACHED",
  "MOU-DETACHED",
  "VIO-FONT",
  "VIO-USER-FONT",
  "VIO-BAD-CP",
  "VIO-NO-CP",
  "VIO-NA-CP",
  "INVALID-CODE-PAGE",
  "CPLIST-TOO-SMALL",
  "CP-NOT-MOVED",
  "MODE-SWITCH-INIT",
  "CODE-PAGE-NOT-FOUND",
  "UNEXPECTED-SLOT-RETURNED",
  "SMG-INVALID-TRACE-OPTION",
  "VIO-INTERNAL-RESOURCE",
  "VIO-SHELL-INIT",
  "SMG-NO-HARD-ERRORS",
  "CP-SWITCH-INCOMPLETE",
  "VIO-TRANSPARENT-POPUP",
  "CRITSEC-OVERFLOW",
  "CRITSEC-UNDERFLOW",
  "VIO-BAD-RESERVE",
  "INVALID-ADDRESS",
  "ZERO-SELECTORS-REQUESTED",
  "NOT-ENOUGH-SELECTORS-AVA",
  "INVALID-SELECTOR",
  "SMG-INVALID-PROGRAM-TYPE",
  "SMG-INVALID-PGM-CONTROL",
  "SMG-INVALID-INHERIT-OPT",
  "VIO-EXTENDED-SG",
  "VIO-NOT-PRES-MGR-SG",
  "VIO-SHIELD-OWNED",
  "VIO-NO-MORE-HANDLES",
  "VIO-SEE-ERROR-LOG",
  "VIO-ASSOCIATED-DC",
  "KBD-NO-CONSOLE",
  "MOUSE-NO-CONSOLE",
  "MOUSE-INVALID-HANDLE",
  "SMG-INVALID-DEBUG-PARMS",
  "KBD-EXTENDED-SG",
  "MOU-EXTENDED-SG",
  "SMG-INVALID-ICON-FILE",
  "TRC-PID-NON-EXISTENT",
  "TRC-COUNT-ACTIVE",
  "TRC-SUSPENDED-BY-COUNT",
  "TRC-COUNT-INACTIVE",
  "TRC-COUNT-REACHED",
  "NO-MC-TRACE",
  "MC-TRACE",
  "TRC-COUNT-ZERO",
  "SMG-TOO-MANY-DDS",
  "SMG-INVALID-NOTIFICATION",
  "LF-INVALID-FUNCTION",
  "LF-NOT-AVAIL",
  "LF-SUSPENDED",
  "LF-BUF-TOO-SMALL",
  "LF-BUFFER-FULL",
  "LF-INVALID-RECORD",
  "LF-INVALID-SERVICE",
  "LF-GENERAL-FAILURE",
  "LF-INVALID-ID",
  "LF-INVALID-HANDLE",
  "LF-NO-ID-AVAIL",
  "LF-TEMPLATE-AREA-FULL",
  "LF-ID-IN-USE",
  "MOU-NOT-INITIALIZED",
  "MOUINITREAL-DONE",
  "DOSSUB-CORRUPTED",
  "MOUSE-CALLER-NOT-SUBSYS",
  "ARITHMETIC-OVERFLOW",
  "TMR-NO-DEVICE",
  "TMR-INVALID-TIME",
  "PVW-INVALID-ENTITY",
  "PVW-INVALID-ENTITY-TYPE",
  "PVW-INVALID-SPEC",
  "PVW-INVALID-RANGE-TYPE",
  "PVW-INVALID-COUNTER-BLK",
  "PVW-INVALID-TEXT-BLK",
  "PRF-NOT-INITIALIZED",
  "PRF-ALREADY-INITIALIZED",
  "PRF-NOT-STARTED",
  "PRF-ALREADY-STARTED",
  "PRF-TIMER-OUT-OF-RANGE",
  "PRF-TIMER-RESET",
  "VDD-LOCK-USEAGE-DENIED",
  "TIMEOUT",
  "VDM-DOWN",
  "VDM-LIMIT",
  "VDD-NOT-FOUND",
  "INVALID-CALLER",
  "PID-MISMATCH",
  "INVALID-VDD-HANDLE",
  "VLPT-NO-SPOOLER",
  "VCOM-DEVICE-BUSY",
  "VLPT-DEVICE-BUSY",
  "NESTING-TOO-DEEP",
  "VDD-MISSING",
  "BIDI-INVALID-LENGTH",
  "BIDI-INVALID-INCREMENT",
  "BIDI-INVALID-COMBINATION",
  "BIDI-INVALID-RESERVED",
  "BIDI-INVALID-EFFECT",
  "BIDI-INVALID-CSDREC",
  "BIDI-INVALID-CSDSTATE",
  "BIDI-INVALID-LEVEL",
  "BIDI-INVALID-TYPE-SUPPORT",
  "BIDI-INVALID-ORIENTATION",
  "BIDI-INVALID-NUM-SHAPE",
  "BIDI-INVALID-CSD",
  "BIDI-NO-SUPPORT",
  "BIDI-RW-INCOMPLETE",
  "IMP-INVALID-PARM",
  "IMP-INVALID-LENGTH",
  "HPFS-DISK-ERROR-WARN",
  "MON-BAD-BUFFER",
  "MODULE-CORRUPTED",
  "SM-OUTOF-SWAPFILE",
  "LF-TIMEOUT",
  "LF-SUSPEND-SUCCESS",
  "LF-RESUME-SUCCESS",
  "LF-REDIRECT-SUCCESS",
  "LF-REDIRECT-FAILURE",
  "SWAPPER-NOT-ACTIVE",
  "INVALID-SWAPID",
  "IOERR-SWAP-FILE",
  "SWAP-TABLE-FULL",
  "SWAP-FILE-FULL",
  "CANT-INIT-SWAPPER",
  "SWAPPER-ALREADY-INIT",
  "PMM-INSUFFICIENT-MEMORY",
  "PMM-INVALID-FLAGS",
  "PMM-INVALID-ADDRESS",
  "PMM-LOCK-FAILED",
  "PMM-UNLOCK-FAILED",
  "PMM-MOVE-INCOMPLETE",
  "UCOM-DRIVE-RENAMED",
  "UCOM-FILENAME-TRUNCATED",
  "UCOM-BUFFER-LENGTH",
  "MON-CHAIN-HANDLE",
  "MON-NOT-REGISTERED",
  "SMG-ALREADY-TOP",
  "PMM-ARENA-MODIFIED",
  "SMG-PRINTER-OPEN",
  "PMM-SET-FLAGS-FAILED",
  "INVALID-DOS-DD",
  "BLOCKED",
  "NOBLOCK",
  "INSTANCE-SHARED",
  "NO-OBJECT",
  "PARTIAL-ATTACH",
  "INCACHE",
  "SWAP-IO-PROBLEMS",
  "CROSSES-OBJECT-BOUNDARY",
  "LONGLOCK",
  "SHORTLOCK",
  "UVIRTLOCK",
  "ALIASLOCK",
  "ALIAS",
  "NO-MORE-HANDLES",
  "SCAN-TERMINATED",
  "TERMINATOR-NOT-FOUND",
  "NOT-DIRECT-CHILD",
  "DELAY-FREE",
  "GUARDPAGE",
  "SWAPERROR",
  "LDRERROR",
  "NOMEMORY",
  "NOACCESS",
  "NO-DLL-TERM",
  "CPSIO-CODE-PAGE-INVALID",
  "CPSIO-NO-SPOOLER",
  "CPSIO-FONT-ID-INVALID",
  "CPSIO-INTERNAL-ERROR",
  "CPSIO-INVALID-PTR-NAME",
  "CPSIO-NOT-ACTIVE",
  "CPSIO-PID-FULL",
  "CPSIO-PID-NOT-FOUND",
  "CPSIO-READ-CTL-SEQ",
  "CPSIO-READ-FNT-DEF",
  "CPSIO-WRITE-ERROR",
  "CPSIO-WRITE-FULL-ERROR",
  "CPSIO-WRITE-HANDLE-BAD",
  "CPSIO-SWIT-LOAD",
  "CPSIO-INV-COMMAND",
  "CPSIO-NO-FONT-SWIT",
  "ENTRY-IS-CALLGATE",
  "UNKNOWN"
};

void
OS_syserr_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syserr_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syserr_names_table);
}
