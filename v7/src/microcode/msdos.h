/* -*-C-*-

$Id: msdos.h,v 1.3 1992/10/07 06:23:36 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

/* DOS system include file */

#ifndef SCM_MSDOS_H
#define SCM_MSDOS_H

#define SYSTEM_NAME "dos"
#define SYSTEM_VARIANT "MS-DOS"

#include <sys/types.h>
#include <sys/times.h>
#include <dos.h>
#include <io.h>
#include <conio.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <signal.h>
#include <errno.h>
/* We fake these for console I/O in DOS */
#ifndef ESTALE
#define ESTALE		1997
#endif
#ifndef ERRNO_NONBLOCK
#define ERRNO_NONBLOCK	1998
#endif
#ifndef EINTR
#define EINTR		1999
#endif

#include "oscond.h"
#include "ansidecl.h"
#include "posixtype.h"

#include "intext.h"
#include "dstack.h"
#include "osscheme.h"
#include "dossys.h"

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
  syscall_ioctl_TIOCGPGRP,
  syscall_ioctl_TIOCSIGSEND,
  syscall_kill,
  syscall_link,
  syscall_listen,
  syscall_localtime,
  syscall_lseek,
  syscall_lstat,    
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
  syscall_write
};

enum syserr_names
{
  syserr_unknown,
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

extern void EXFUN (error_system_call, (int code, enum syscall_names name));


#include <limits.h>
#include <time.h>
#include <termio.h>

#define HAVE_MKDIR
#define HAVE_RMDIR
#define HAVE_GETCWD

/* #define HAVE_DUP2 */
/* #define HAVE_FCNTL */
#define VOID_SIGNAL_HANDLERS

#include <sys/dir.h>

typedef void Tsignal_handler_result;
#define SIGNAL_HANDLER_RETURN() return

typedef Tsignal_handler_result (*Tsignal_handler) ();

#ifndef SIG_ERR
#define SIG_ERR ((Tsignal_handler) (-1))
#endif

#if !defined(SIGCHLD) && defined(SIGCLD)
#define SIGCHLD SIGCLD
#endif
#if !defined(SIGABRT) && defined(SIGIOT)
#define SIGABRT SIGIOT
#endif

/* Crufty, but it will work here. */
#ifndef ENOSYS
#define ENOSYS 0
#endif

#ifdef UNION_WAIT_STATUS

typedef union wait wait_status_t;

#ifndef WEXITSTATUS
#define WEXITSTATUS(_X) ((_X) . w_retcode)
#endif

#ifndef WTERMSIG
#define WTERMSIG(_X) ((_X) . w_termsig)
#endif

#ifndef WSTOPSIG
#define WSTOPSIG(_X) ((_X) . w_stopsig)
#endif

#else /* not UNION_WAIT_STATUS */

typedef int wait_status_t;

#ifndef WIFEXITED
#define WIFEXITED(_X) (((_X) & 0377) == 0)
#endif

#ifndef WIFSTOPPED
#define WIFSTOPPED(_X) (((_X) & 0377) == 0177)
#endif

#ifndef WIFSIGNALED
#define WIFSIGNALED(_X) ((((_X) & 0377) != 0) && (((_X) & 0377) != 0177))
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(_X) (((_X) >> 8) & 0377)
#endif

#ifndef WTERMSIG
#define WTERMSIG(_X) ((_X) & 0177)
#endif

#ifndef WSTOPSIG
#define WSTOPSIG(_X) (((_X) >> 8) & 0377)
#endif

#endif /* UNION_WAIT_STATUS */

/* Provide null defaults for all the signals we're likely to use so we
   aren't continually testing to see if they're defined. */

#ifndef SIGLOST
#define SIGLOST 0
#endif
#ifndef SIGWINCH
#define SIGWINCH 0
#endif
#ifndef SIGURG
#define SIGURG 0
#endif
#ifndef SIGIO
#define SIGIO 0
#endif
#ifndef SIGUSR1
#define SIGUSR1 0
#endif
#ifndef SIGUSR2
#define SIGUSR2 0
#endif
#ifndef SIGVTALRM
#define SIGVTALRM 0
#endif
#ifndef SIGABRT
#define SIGABRT 0
#endif
#ifndef SIGPWR
#define SIGPWR 0
#endif
#ifndef SIGPROF
#define SIGPROF 0
#endif
#ifndef SIGSTOP
#define SIGSTOP 0
#endif
#ifndef SIGTSTP
#define SIGTSTP 0
#endif
#ifndef SIGCONT
#define SIGCONT 0
#endif
#ifndef SIGCHLD
#define SIGCHLD 0
#endif
#ifndef SIGTTIN
#define SIGTTIN 0
#endif
#ifndef SIGTTOU
#define SIGTTOU 0
#endif

/* constants for access() */
#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 128
#endif

#ifdef __STDC__
#define ALERT_CHAR '\a'
#define ALERT_STRING "\a"
#else
#define ALERT_CHAR '\007'
#define ALERT_STRING "\007"
#endif

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif

/* constants for open() and fcntl() */
#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

/* mode bit definitions for open(), creat(), and chmod() */
#ifndef S_IRWXU
#define S_IRWXU 0700
#define S_IRWXG 0070
#define S_IRWXO 0007
#endif

#ifndef S_IRUSR
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100
#define S_IRGRP 0040
#define S_IWGRP 0020
#define S_IXGRP 0010
#define S_IROTH 0004
#define S_IWOTH 0002
#define S_IXOTH 0001
#endif

#define MODE_REG (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define MODE_DIR (MODE_REG | S_IXUSR | S_IXGRP | S_IXOTH)

/* constants for lseek() */
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifndef DECL_GETLOGIN
extern char * EXFUN (getlogin, (void));
#endif

#define DOS_abort abort
#define DOS_access access
#define DOS_alarm alarm
#define DOS_chdir chdir
#define DOS_chmod chmod
#define DOS_close close
#define DOS_ctime ctime
#define DOS_dup dup
#define DOS_free free
#define DOS_fstat fstat
#define DOS_getcwd getcwd
#define DOS_getenv getenv
#define DOS_getegid getegid
#define DOS_geteuid geteuid
#define DOS_getgrgid getgrgid
#define DOS_gethostname gethostname
#define DOS_getlogin getlogin
#define DOS_getpid getpid
#define DOS_getpwnam getpwnam
#define DOS_getpwuid getpwuid
#define DOS_ioctl ioctl
#define DOS_link link
#define DOS_localtime localtime
#define DOS_lseek lseek
#define DOS_malloc malloc
#define DOS_mknod mknod
#define DOS_pause pause
#define DOS_pipe pipe
#define DOS_read read
#define DOS_realloc realloc
#define DOS_signal signal
#define DOS_sleep sleep
#define DOS_stat stat
#define DOS_system system
#define DOS_time time
#define DOS_unlink unlink
#define DOS_write write
#define DOS_wait wait

extern PTR EXFUN (malloc, (unsigned int size));
extern PTR EXFUN (realloc, (PTR ptr, unsigned int size));
extern int EXFUN (gethostname, (char * name, unsigned int size));

#ifdef HAVE_FCNTL
#define DOS_fcntl fcntl
#endif

#ifdef HAVE_TRUNCATE
#define DOS_ftruncate ftruncate
#define DOS_truncate truncate
#endif

#ifdef HAVE_VFORK
#define DOS_vfork vfork
#else
#define DOS_vfork fork
#endif

#ifdef HAVE_SYMBOLIC_LINKS
#define DOS_lstat lstat
#define DOS_readlink readlink
#define DOS_symlink symlink
#else
#define DOS_lstat stat
#endif

extern void EXFUN (DOS_prim_check_errno, (enum syscall_names name));

#define STD_VOID_SYSTEM_CALL(name, expression)				\
{									\
  while ((expression) < 0)						\
      error_system_call (errno, (name));				\
}

#define STD_UINT_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) < 0)					\
      error_system_call (errno, (name));				\
}

#define STD_PTR_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) == 0)				\
      error_system_call (errno, (name));				\
}

#ifdef HAVE_GETTIMEOFDAY
#define DOS_gettimeofday gettimeofday
#endif
#ifdef HAVE_ITIMER
#define DOS_setitimer setitimer
#endif
#ifdef HAVE_RMDIR
#define DOS_rmdir rmdir
#endif
#ifdef HAVE_TIMES
#define DOS_times times
#endif

#ifdef HAVE_DUMB_OPEN
extern int EXFUN (DOS_open, (CONST char * name, int oflag, mode_t mode));
#else
#define DOS_open open
#endif

#ifdef HAVE_GETCWD
#define DOS_getcwd getcwd
#else
#define EMULATE_GETCWD
#define HAVE_GETCWD
extern char * EXFUN (DOS_getcwd, (char * buffer, size_t length));
#endif

#ifdef HAVE_MKDIR
#define DOS_mkdir mkdir
#else
#define EMULATE_MKDIR
#define HAVE_MKDIR
extern int EXFUN (DOS_mkdir, (CONST char * name, mode_t mode));
#endif

#ifdef HAVE_RENAME
#define DOS_rename rename
#else
#define DOS_rename dos_rename_file

#ifdef HAVE_WAITPID
#define DOS_waitpid waitpid
#else /* not HAVE_WAITPID */
#ifdef HAVE_WAIT3
#define EMULATE_WAITPID
#define HAVE_WAITPID
extern int EXFUN
  (DOS_waitpid, (pid_t pid, wait_status_t * stat_loc, int options));
#endif /* HAVE_WAIT3 */
#endif /* HAVE_WAITPID */

#ifndef WUNTRACED
#define WUNTRACED 0
#endif

#ifdef HAVE_SELECT
#define DOS_select select
#endif /* HAVE_SELECT */

#ifdef _NFILE
#define DOS_SC_OPEN_MAX() _NFILE
#else
#define DOS_SC_OPEN_MAX() 16
#endif

/* Interrupts */

#define int10h(in,out)		int86 (0x10, in, out)
#define intDPMI(in,out)		int86 (0x31, in, out)
#define intDPMIx(in,out,seg)	int86x (0x31, in, out, seg)

/* Doesn't really go anywhere */
#define INTERRUPT_CHAIN_NEXT	0
#define INTERRUPT_RETURN	1

#endif /* SCM_MSDOS_H */
