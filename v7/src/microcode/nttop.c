/* -*-C-*-

$Id: nttop.c,v 1.11 1994/10/04 21:08:14 cph Exp $

Copyright (c) 1993-94 Massachusetts Institute of Technology

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
#include "nttop.h"
#include "osctty.h"
#include "prims.h"
#include "errors.h"
#include "option.h"
#include "outf.h"

extern void EXFUN (execute_reload_cleanups, (void));

extern void EXFUN (NT_gui_init, (void));
extern void EXFUN (NT_initialize_channels, (void));
extern void EXFUN (NT_initialize_directory_reader, (void));
extern void EXFUN (NT_initialize_signals, (void));
extern void EXFUN (NT_initialize_traps, (void));
extern void EXFUN (NT_initialize_tty, (void));

extern void EXFUN (NT_reset_channels, (void));

extern void EXFUN (NT_restore_channels, (void));
extern void EXFUN (NT_restore_signals, (void));
extern void EXFUN (NT_restore_traps, (void));

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
  interactive = 1;

  NT_gui_init ();
  NT_initialize_channels ();
  NT_initialize_tty ();
  NT_initialize_signals ();
  NT_initialize_traps ();
  NT_initialize_directory_reader ();

  OS_Name = SYSTEM_NAME;
  OS_Variant = SYSTEM_VARIANT;

  {
    version_t version_number;

    nt_get_version (&version_number);
    OS_Variant = ((version_number.platform == 0)
		  ? "Windows-NT"
		  : "MS Windows");

    outf_console ("MIT Scheme running under %s %d.%d 386/486\n",
		  OS_Variant,
		  ((int) version_number.major),
		  ((int) version_number.minor));
    /* To make our compiler vendors happy. */
    outf_console
      ("Copyright (c) 1993 Massachusetts Institute of Technology\n");
  }

  outf_console ("\n");
  outf_flush_console ();
  return;
}

void
DEFUN_VOID (OS_reset)
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
DEFUN (OS_quit, (code, abnormal_p), int code AND int abnormal_p)
{
  outf_console ("\nScheme has terminated abnormally!\n");
  OS_restore_external_state ();
  return;
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
DEFUN (NT_prim_check_errno, (name), enum syscall_names name)
{
  if (errno != EINTR)
    error_system_call (errno, name);
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

static unsigned char * syscall_names_table [] =
{
  "ACCEPT",
  "BIND",
  "CHDIR",
  "CHMOD",
  "CLOSE",
  "CONNECT",
  "FCNTL-GETFL",
  "FCNTL-SETFL",
  "FORK",
  "FSTAT",
  "FTRUNCATE",
  "GETCWD",
  "GETHOSTNAME",
  "GETTIMEOFDAY",
  "IOCTL-TIOCGPGRP",
  "IOCTL-TIOCSIGSEND",
  "KILL",
  "LINK",
  "LISTEN",
  "LOCALTIME",
  "LSEEK",
  "MALLOC",
  "MKDIR",
  "OPEN",
  "OPENDIR",
  "PAUSE",
  "PIPE",
  "READ",
  "READLINK",
  "REALLOC",
  "RENAME",
  "RMDIR",
  "SELECT",
  "SETITIMER",
  "SETPGID",
  "SIGHOLD",
  "SIGPROCMASK",
  "SIGSUSPEND",
  "SLEEP",
  "SOCKET",
  "SYMLINK",
  "TCDRAIN",
  "TCFLUSH",
  "TCGETPGRP",
  "TCSETPGRP",
  "TERMINAL-GET-STATE",
  "TERMINAL-SET-STATE",
  "TIME",
  "TIMES",
  "UNLINK",
  "UTIME",
  "VFORK",
  "WRITE",
  "STAT",
  "LSTAT",
  "MKTIME",
  "DYNAMIC-LOAD"
};

void
OS_syscall_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syscall_names_table)) / (sizeof (unsigned char *)));
  (*names) = syscall_names_table;
}

static unsigned char * syserr_names_table [] =
{
  "UNKNOWN",
  "ARG-LIST-TOO-LONG",
  "BAD-ADDRESS",
  "BAD-FILE-DESCRIPTOR",
  "BROKEN-PIPE",
  "DIRECTORY-NOT-EMPTY",
  "DOMAIN-ERROR",
  "EXEC-FORMAT-ERROR",
  "FILE-EXISTS",
  "FILE-TOO-LARGE",
  "FILENAME-TOO-LONG",
  "FUNCTION-NOT-IMPLEMENTED",
  "IMPROPER-LINK",
  "INAPPROPRIATE-IO-CONTROL-OPERATION",
  "INTERRUPTED-FUNCTION-CALL",
  "INVALID-ARGUMENT",
  "INVALID-SEEK",
  "IO-ERROR",
  "IS-A-DIRECTORY",
  "NO-CHILD-PROCESSES",
  "NO-LOCKS-AVAILABLE",
  "NO-SPACE-LEFT-ON-DEVICE",
  "NO-SUCH-DEVICE",
  "NO-SUCH-DEVICE-OR-ADDRESS",
  "NO-SUCH-FILE-OR-DIRECTORY",
  "NO-SUCH-PROCESS",
  "NOT-A-DIRECTORY",
  "NOT-ENOUGH-SPACE",
  "OPERATION-NOT-PERMITTED",
  "PERMISSION-DENIED",
  "READ-ONLY-FILE-SYSTEM",
  "RESOURCE-BUSY",
  "RESOURCE-DEADLOCK-AVOIDED",
  "RESOURCE-TEMPORARILY-UNAVAILABLE",
  "RESULT-TOO-LARGE",
  "TOO-MANY-LINKS",
  "TOO-MANY-OPEN-FILES",
  "TOO-MANY-OPEN-FILES"
};

void
OS_syserr_names (unsigned int * length, unsigned char *** names)
{
  (*length) = ((sizeof (syserr_names_table)) / (sizeof (unsigned char *)));
  (*names) = syserr_names_table;
}
