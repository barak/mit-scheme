/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxtop.c,v 1.9 1992/06/05 20:09:00 jinx Exp $

Copyright (c) 1990-1 Massachusetts Institute of Technology

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

#include "ux.h"
#include "uxtop.h"
#include "osctty.h"
#include "uxutil.h"
#include "errors.h"
#include "option.h"

extern void EXFUN (UX_initialize_channels, (void));
extern void EXFUN (UX_initialize_ctty, (int interactive));
extern void EXFUN (UX_initialize_directory_reader, (void));
extern void EXFUN (UX_initialize_environment, (void));
extern void EXFUN (UX_initialize_processes, (void));
extern void EXFUN (UX_initialize_signals, (void));
extern void EXFUN (UX_initialize_terminals, (void));
extern void EXFUN (UX_initialize_trap_recovery, (void));
extern void EXFUN (UX_initialize_tty, (void));
extern void EXFUN (UX_initialize_userio, (void));

extern void EXFUN (UX_reset_channels, (void));
extern void EXFUN (UX_reset_processes, (void));
extern void EXFUN (UX_reset_terminals, (void));
extern void EXFUN (execute_reload_cleanups, (void));

extern void EXFUN (UX_ctty_save_external_state, (void));
extern void EXFUN (UX_ctty_save_internal_state, (void));
extern void EXFUN (UX_ctty_restore_internal_state, (void));
extern void EXFUN (UX_ctty_restore_external_state, (void));

/* reset_interruptable_extent */

extern CONST char * OS_Name;
extern CONST char * OS_Variant;

static int interactive;

int
DEFUN_VOID (OS_under_emacs_p)
{
  return (option_emacs_subprocess);
}

void
DEFUN_VOID (OS_initialize)
{
  dstack_initialize ();
  transaction_initialize ();
  initialize_interruptable_extent ();
  {
    interactive =
      (option_force_interactive
       || (isatty (STDIN_FILENO))
       || (isatty (STDOUT_FILENO))
       || (isatty (STDERR_FILENO)));
    /* If none of the stdio streams is a terminal, disassociate us
       from the controlling terminal so that we're not affected by
       keyboard interrupts or hangup signals.  However, if we're
       running under Emacs we don't want to do this, because we want
       to receive a hangup signal if Emacs dies. */
    if ((!interactive) && (!option_emacs_subprocess))
      UX_setsid ();
    /* The argument passed to `UX_ctty_initialize' says whether to
       permit interrupt control, i.e. whether to attempt to setup the
       keyboard interrupt characters. */
    UX_initialize_ctty (interactive);
  }
  UX_initialize_channels ();
  UX_initialize_terminals ();
  UX_initialize_environment ();
  UX_initialize_tty ();
  UX_initialize_userio ();
  UX_initialize_signals ();
  UX_initialize_processes ();
  UX_initialize_trap_recovery ();
  UX_initialize_directory_reader ();
  OS_Name = SYSTEM_NAME;
  OS_Variant = SYSTEM_VARIANT;
  fprintf (stdout, "MIT Scheme running under %s\n", OS_Variant);
  if ((!option_emacs_subprocess) && (OS_ctty_interrupt_control ()))
    {
      fputs ("", stdout);
      fprintf (stdout, "Type %s followed by `H' to obtain information about interrupts.\n",
	       (char_description ((OS_ctty_quit_char ()), 1)));
    }
  fflush (stdout);
#ifdef _SUNOS
  vadvise (VA_ANOM);		/* Anomolous paging, don't try to guess. */
#endif
}

void
DEFUN_VOID (OS_reset)
{
  /*
    There should really be a reset for each initialize above,
    but the rest seem innocuous.
   */

  UX_reset_channels ();
  UX_reset_terminals ();
  UX_reset_processes ();
  execute_reload_cleanups ();
}

void
DEFUN (OS_quit, (code, abnormal_p), int code AND int abnormal_p)
{
  fflush (stdout);
  if (abnormal_p
      && interactive
      && (! ((code == TERM_SIGNAL) || (code == TERM_EOF))))
    {
      fputs ("\nScheme has terminated abnormally!\n", stdout);
      {
	int dump_core =
	  ((!option_disable_core_dump)
	   && (userio_confirm ("Would you like a core dump? [Y or N] "))
	   && (userio_confirm ("Do you really want a core dump? [Y or N] ")));
	putc ('\n', stdout);
	fflush (stdout);
	if (dump_core)
	  UX_dump_core ();
      }
    }
  OS_restore_external_state ();
}

void
DEFUN_VOID (UX_dump_core)
{
  OS_restore_external_state ();
  /* Unmask this too? */
  UX_signal (SIGABRT, SIG_DFL);
  UX_abort ();
}

void
DEFUN_VOID (OS_save_external_state)
{
  UX_ctty_save_external_state ();
}

void
DEFUN_VOID (OS_save_internal_state)
{
  UX_ctty_save_internal_state ();
}

void
DEFUN_VOID (OS_restore_internal_state)
{
  UX_ctty_restore_internal_state ();
}

void
DEFUN_VOID (OS_restore_external_state)
{
  UX_ctty_restore_external_state ();
}

static enum syserr_names
DEFUN (error_code_to_syserr, (code), int code)
{
  switch (code)
    {
    case E2BIG:		return (syserr_arg_list_too_long);
    case EACCES:	return (syserr_permission_denied);
    case EAGAIN:	return (syserr_resource_temporarily_unavailable);
    case EBADF:		return (syserr_bad_file_descriptor);
    case EBUSY:		return (syserr_resource_busy);
    case ECHILD:	return (syserr_no_child_processes);
    case EDEADLK:	return (syserr_resource_deadlock_avoided);
    case EDOM:		return (syserr_domain_error);
    case EEXIST:	return (syserr_file_exists);
    case EFAULT:	return (syserr_bad_address);
    case EFBIG:		return (syserr_file_too_large);
    case EINTR:		return (syserr_interrupted_function_call);
    case EINVAL:	return (syserr_invalid_argument);
    case EIO:		return (syserr_io_error);
    case EISDIR:	return (syserr_is_a_directory);
    case EMFILE:	return (syserr_too_many_open_files);
    case EMLINK:	return (syserr_too_many_links);
#ifdef ENAMETOOLONG
    case ENAMETOOLONG:	return (syserr_filename_too_long);
#endif
    case ENFILE:	return (syserr_too_many_open_files_in_system);
    case ENODEV:	return (syserr_no_such_device);
    case ENOENT:	return (syserr_no_such_file_or_directory);
    case ENOEXEC:	return (syserr_exec_format_error);
    case ENOLCK:	return (syserr_no_locks_available);
    case ENOMEM:	return (syserr_not_enough_space);
    case ENOSPC:	return (syserr_no_space_left_on_device);
    case ENOSYS:	return (syserr_function_not_implemented);
    case ENOTDIR:	return (syserr_not_a_directory);
#ifdef ENOTEMPTY
    case ENOTEMPTY:	return (syserr_directory_not_empty);
#endif
    case ENOTTY:	return (syserr_inappropriate_io_control_operation);
    case ENXIO:		return (syserr_no_such_device_or_address);
    case EPERM:		return (syserr_operation_not_permitted);
    case EPIPE:		return (syserr_broken_pipe);
    case ERANGE:	return (syserr_result_too_large);
    case EROFS:		return (syserr_read_only_file_system);
    case ESPIPE:	return (syserr_invalid_seek);
    case ESRCH:		return (syserr_no_such_process);
    case EXDEV:		return (syserr_improper_link);
    default:		return (syserr_unknown);
    }
}

static int
DEFUN (syserr_to_error_code, (syserr), enum syserr_names syserr)
{
  switch (syserr)
    {
    case syserr_arg_list_too_long:			return (E2BIG);
    case syserr_bad_address:				return (EFAULT);
    case syserr_bad_file_descriptor:			return (EBADF);
    case syserr_broken_pipe:				return (EPIPE);
#ifdef ENOTEMPTY
    case syserr_directory_not_empty:			return (ENOTEMPTY);
#endif
    case syserr_domain_error:				return (EDOM);
    case syserr_exec_format_error:			return (ENOEXEC);
    case syserr_file_exists:				return (EEXIST);
    case syserr_file_too_large:				return (EFBIG);
#ifdef ENAMETOOLONG
    case syserr_filename_too_long:			return (ENAMETOOLONG);
#endif
    case syserr_function_not_implemented:		return (ENOSYS);
    case syserr_improper_link:				return (EXDEV);
    case syserr_inappropriate_io_control_operation:	return (ENOTTY);
    case syserr_interrupted_function_call:		return (EINTR);
    case syserr_invalid_argument:			return (EINVAL);
    case syserr_invalid_seek:				return (ESPIPE);
    case syserr_io_error:				return (EIO);
    case syserr_is_a_directory:				return (EISDIR);
    case syserr_no_child_processes:			return (ECHILD);
    case syserr_no_locks_available:			return (ENOLCK);
    case syserr_no_space_left_on_device:		return (ENOSPC);
    case syserr_no_such_device:				return (ENODEV);
    case syserr_no_such_device_or_address:		return (ENXIO);
    case syserr_no_such_file_or_directory:		return (ENOENT);
    case syserr_no_such_process:			return (ESRCH);
    case syserr_not_a_directory:			return (ENOTDIR);
    case syserr_not_enough_space:			return (ENOMEM);
    case syserr_operation_not_permitted:		return (EPERM);
    case syserr_permission_denied:			return (EACCES);
    case syserr_read_only_file_system:			return (EROFS);
    case syserr_resource_busy:				return (EBUSY);
    case syserr_resource_deadlock_avoided:		return (EDEADLK);
    case syserr_resource_temporarily_unavailable:	return (EAGAIN);
    case syserr_result_too_large:			return (ERANGE);
    case syserr_too_many_links:				return (EMLINK);
    case syserr_too_many_open_files:			return (EMFILE);
    case syserr_too_many_open_files_in_system:		return (ENFILE);
    default: return (0);
    }
}

void
DEFUN (error_system_call, (code, name), int code AND enum syscall_names name)
{
  extern unsigned int syscall_error_code;
  extern unsigned int syscall_error_name;
  syscall_error_code = ((unsigned int) (error_code_to_syserr (code)));
  syscall_error_name = ((unsigned int) name);
  signal_error_from_primitive (ERR_IN_SYSTEM_CALL);
}

CONST char *
DEFUN (OS_error_code_to_message, (syserr), unsigned int syserr)
{
  extern char * sys_errlist [];
  extern int sys_nerr;
  int code = (syserr_to_error_code ((enum syserr_names) syserr));
  return (((code > 0) && (code <= sys_nerr)) ? (sys_errlist [code]) : 0);
}
