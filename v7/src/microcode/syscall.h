/* -*-C-*-

$Id: syscall.h,v 1.20 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* OS system calls and errors.  */

#ifndef SCM_SYSCALL_H
#define SCM_SYSCALL_H 1

#include "config.h"

#ifdef __OS2__

#define DEFINE_OS2_SYSCALLS
#include "os2api.h"
#undef DEFINE_OS2_SYSCALLS

#else /* not __OS2__ */
#ifdef __WIN32__

#define DEFINE_WIN32_SYSCALLS
#include "ntapi.h"
#undef DEFINE_WIN32_SYSCALLS

#else /* not __WIN32__ */

/* Unix case, inline for historical reasons.  Must match "uxtop.c".  */

enum syscall_names
{
  syscall_accept,
  syscall_bind,
  syscall_chdir,
  syscall_chmod,
  syscall_close,
  syscall_connect,
  syscall_fcntl_GETFL,
  syscall_fcntl_SETFL,
  syscall_fork,
  syscall_fstat,
  syscall_ftruncate,
  syscall_getcwd,
  syscall_gethostname,
  syscall_gettimeofday,
  syscall_gmtime,
  syscall_ioctl_TIOCGPGRP,
  syscall_ioctl_TIOCSIGSEND,
  syscall_kill,
  syscall_link,
  syscall_listen,
  syscall_localtime,
  syscall_lseek,
  syscall_malloc,
  syscall_mkdir,
  syscall_open,
  syscall_opendir,
  syscall_pause,
  syscall_pipe,
  syscall_read,
  syscall_readlink,
  syscall_realloc,
  syscall_rename,
  syscall_rmdir,
  syscall_select,
  syscall_setitimer,
  syscall_setpgid,
  syscall_shutdown,
  syscall_sighold,
  syscall_sigprocmask,
  syscall_sigsuspend,
  syscall_sleep,
  syscall_socket,
  syscall_symlink,
  syscall_tcdrain,
  syscall_tcflush,
  syscall_tcgetpgrp,
  syscall_tcsetpgrp,
  syscall_terminal_get_state,
  syscall_terminal_set_state,
  syscall_time,
  syscall_times,
  syscall_unlink,
  syscall_utime,
  syscall_vfork,
  syscall_write,
  syscall_stat,
  syscall_lstat,
  syscall_mktime,
  syscall_dld,
  syscall_statfs,
  syscall_fstatfs,
  syscall_setsockopt
};

enum syserr_names
{
  syserr_unknown,
  syserr_address_in_use,
  syserr_arg_list_too_long,
  syserr_bad_address,
  syserr_bad_file_descriptor,
  syserr_broken_pipe,
  syserr_directory_not_empty,
  syserr_domain_error,
  syserr_exec_format_error,
  syserr_file_exists,
  syserr_file_too_large,
  syserr_filename_too_long,
  syserr_function_not_implemented,
  syserr_improper_link,
  syserr_inappropriate_io_control_operation,
  syserr_interrupted_function_call,
  syserr_invalid_argument,
  syserr_invalid_seek,
  syserr_io_error,
  syserr_is_a_directory,
  syserr_no_child_processes,
  syserr_no_locks_available,
  syserr_no_space_left_on_device,
  syserr_no_such_device,
  syserr_no_such_device_or_address,
  syserr_no_such_file_or_directory,
  syserr_no_such_process,
  syserr_not_a_directory,
  syserr_not_enough_space,
  syserr_operation_not_permitted,
  syserr_permission_denied,
  syserr_read_only_file_system,
  syserr_resource_busy,
  syserr_resource_deadlock_avoided,
  syserr_resource_temporarily_unavailable,
  syserr_result_too_large,
  syserr_too_many_links,
  syserr_too_many_open_files,
  syserr_too_many_open_files_in_system
};

#endif /* not __WIN32__ */
#endif /* not __OS2__ */

extern void EXFUN (error_in_system_call,
		   (enum syserr_names, enum syscall_names));
extern void EXFUN (error_system_call, (int, enum syscall_names name));
extern enum syserr_names EXFUN (OS_error_code_to_syserr, (int));

#endif /* SCM_SYSCALL_H */
