/* -*-C-*-

$Id: dostop.c,v 1.13 2000/05/20 18:59:10 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "msdos.h"
#include "dostop.h"
#include "osctty.h"
#include "dosutil.h"
#include "errors.h"
#include "option.h"

extern void EXFUN (DOS_initialize_channels, (void));
extern void EXFUN (DOS_initialize_ctty, (int interactive));
extern void EXFUN (DOS_initialize_directory_reader, (void));
extern void EXFUN (DOS_initialize_environment, (void));
extern void EXFUN (DOS_initialize_processes, (void));
extern void EXFUN (DOS_initialize_signals, (void));
extern void EXFUN (DOS_initialize_terminals, (void));
extern void EXFUN (DOS_initialize_trap_recovery, (void));
extern void EXFUN (DOS_initialize_conio, (void));
extern void EXFUN (DOS_initialize_tty, (void));
extern void EXFUN (DOS_initialize_userio, (void));
extern void EXFUN (DOS_initialize_real_mode, (void));

extern void EXFUN (DOS_reset_channels, (void));
extern void EXFUN (DOS_reset_processes, (void));
extern void EXFUN (DOS_reset_terminals, (void));
extern void EXFUN (execute_reload_cleanups, (void));

extern void EXFUN (DOS_ctty_save_external_state, (void));
extern void EXFUN (DOS_ctty_save_internal_state, (void));
extern void EXFUN (DOS_ctty_restore_internal_state, (void));
extern void EXFUN (DOS_ctty_restore_external_state, (void));

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
  interactive = 1;
  
  DOS_initialize_channels ();
  DOS_initialize_environment ();
  DOS_initialize_tty ();
  DOS_initialize_trap_recovery ();
  DOS_initialize_signals ();
  DOS_initialize_directory_reader ();
  DOS_initialize_conio();
  DOS_initialize_real_mode ();
  OS_Name = SYSTEM_NAME;
  {
    version_t version_number;
    dos_get_version(&version_number);
    OS_Variant = (malloc ((strlen (SYSTEM_VARIANT)) + 19));
    sprintf (((char *) OS_Variant), "%s %d.%d 386/486",
	     SYSTEM_VARIANT,
	     (int) version_number.major,
	     (int) version_number.minor);
  }
}

void
DEFUN_VOID (OS_announcement)
{
  /* To make our compiler vendors happy. */		   
  fprintf(stdout,
	  "Copyright (c) 1992-1994 Massachusetts Institute of Technology\n");
  fputs ("", stdout);
}

void
DEFUN_VOID (OS_reset)
{
  /*
    There should really be a reset for each initialize above,
    but the rest seem innocuous.
   */

  DOS_reset_channels ();
  execute_reload_cleanups ();
}

void
DEFUN (OS_quit, (code, abnormal_p), int code AND int abnormal_p)
{
  fflush (stdout);
  fputs ("\nScheme has terminated abnormally!\n", stdout);
  OS_restore_external_state ();
}

#ifndef EAGAIN
#define EAGAIN ERRNO_NONBLOCK
#endif

enum syserr_names
DEFUN (OS_error_code_to_syserr, (code), int code)
{
  switch (code)
  {
    case E2BIG:		return (syserr_arg_list_too_long);
    case EACCES:	return (syserr_permission_denied);
    case EAGAIN:	return (syserr_resource_temporarily_unavailable);
    case EBADF:		return (syserr_bad_file_descriptor);
    case EDEADLOCK:	return (syserr_resource_deadlock_avoided);
    case EDOM:		return (syserr_domain_error);
    case EEXIST:	return (syserr_file_exists);
    case EINTR:		return (syserr_interrupted_function_call);
    case EINVAL:	return (syserr_invalid_argument);
    case EMFILE:	return (syserr_too_many_open_files);
    case ENOENT:	return (syserr_no_such_file_or_directory);
    case ENOEXEC:	return (syserr_exec_format_error);
    case ENOMEM:	return (syserr_not_enough_space);
    case ENOTDIR:	return (syserr_not_a_directory);
    case ERANGE:	return (syserr_result_too_large);
    default:		return (syserr_unknown);
  }
}

static int
DEFUN (syserr_to_error_code, (syserr), enum syserr_names syserr)
{
  switch (syserr)
  {
    case syserr_arg_list_too_long:			return (E2BIG);
    case syserr_bad_file_descriptor:			return (EBADF);
    case syserr_domain_error:				return (EDOM);
    case syserr_exec_format_error:			return (ENOEXEC);
    case syserr_file_exists:				return (EEXIST);
    case syserr_interrupted_function_call:		return (EINTR);
    case syserr_invalid_argument:			return (EINVAL);
    case syserr_no_such_file_or_directory:		return (ENOENT);
    case syserr_not_a_directory:			return (ENOTDIR);
    case syserr_not_enough_space:			return (ENOMEM);
    case syserr_permission_denied:			return (EACCES);
    case syserr_resource_deadlock_avoided:		return (EDEADLOCK);
    case syserr_resource_temporarily_unavailable:	return (EAGAIN);
    case syserr_result_too_large:			return (ERANGE);
    case syserr_too_many_open_files:			return (EMFILE);
    default: 						return (0);
  }
}

CONST char *
DEFUN (OS_error_code_to_message, (syserr), unsigned int syserr)
{
  extern char * sys_errlist [];
  extern int sys_nerr;
  int code = (syserr_to_error_code ((enum syserr_names) syserr));
  return (((code > 0) && (code <= sys_nerr)) ? (sys_errlist [code]) : 0);
}

void
DEFUN (DOS_prim_check_errno, (name), enum syscall_names name)
{
  if (errno != EINTR)
    error_system_call (errno, name);
  deliver_pending_interrupts();
}

void OS_restore_external_state (void)
{ extern void DOS_restore_interrupts(void);

  DOS_restore_interrupts();
  return;
}

void bcopy (const char *s1, char *s2, int n)
{
  while (n-- > 0)
    *s2++ = *s1++;
  return;
}

static char * syscall_names_table [] =
{
  "accept",
  "bind",
  "chdir",
  "chmod",
  "close",
  "connect",
  "fcntl-getfl",
  "fcntl-setfl",
  "fork",
  "fstat",
  "ftruncate",
  "getcwd",
  "gethostname",
  "gettimeofday",
  "ioctl-tiocgpgrp",
  "ioctl-tiocsigsend",
  "kill",
  "link",
  "listen",
  "localtime",
  "lseek",
  "malloc",
  "mkdir",
  "open",
  "opendir",
  "pause",
  "pipe",
  "read",
  "readlink",
  "realloc",
  "rename",
  "rmdir",
  "select",
  "setitimer",
  "setpgid",
  "sighold",
  "sigprocmask",
  "sigsuspend",
  "sleep",
  "socket",
  "symlink",
  "tcdrain",
  "tcflush",
  "tcgetpgrp",
  "tcsetpgrp",
  "terminal-get-state",
  "terminal-set-state",
  "time",
  "times",
  "unlink",
  "utime",
  "vfork",
  "write",
  "stat",
  "lstat",
  "mktime",
  "dynamic-load"
};

void
OS_syscall_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syscall_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syscall_names_table);
}

static char * syserr_names_table [] =
{
  "unknown",
  "arg-list-too-long",
  "bad-address",
  "bad-file-descriptor",
  "broken-pipe",
  "directory-not-empty",
  "domain-error",
  "exec-format-error",
  "file-exists",
  "file-too-large",
  "filename-too-long",
  "function-not-implemented",
  "improper-link",
  "inappropriate-io-control-operation",
  "interrupted-function-call",
  "invalid-argument",
  "invalid-seek",
  "io-error",
  "is-a-directory",
  "no-child-processes",
  "no-locks-available",
  "no-space-left-on-device",
  "no-such-device",
  "no-such-device-or-address",
  "no-such-file-or-directory",
  "no-such-process",
  "not-a-directory",
  "not-enough-space",
  "operation-not-permitted",
  "permission-denied",
  "read-only-file-system",
  "resource-busy",
  "resource-deadlock-avoided",
  "resource-temporarily-unavailable",
  "result-too-large",
  "too-many-links",
  "too-many-open-files",
  "too-many-open-files"
};

void
OS_syserr_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syserr_names_table)) / (sizeof (char *)));
  (*names) = ((unsigned char **) syserr_names_table);
}
