/* -*-C-*-

$Id: ux.h,v 1.77 2003/05/17 02:21:22 cph Exp $

Copyright 1990,1991,1992,1993,1994,1995 Massachusetts Institute of Technology
Copyright 1996,1997,1998,1999,2000,2003 Massachusetts Institute of Technology

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

/* Unix system include file */

#ifndef SCM_UX_H
#define SCM_UX_H

#define SYSTEM_NAME "unix"

#ifdef __386BSD__
#  define SYSTEM_VARIANT "386BSD"
#endif

#ifdef _AIX
#  define SYSTEM_VARIANT "AIX"
#endif

#ifdef apollo
#  define SYSTEM_VARIANT "Domain"
#endif

#ifdef __bsdi__			/* works on bsdi 3.0 */
#  define SYSTEM_VARIANT "BSDI BSD/OS"
#endif

#ifdef __FreeBSD__
#  define SYSTEM_VARIANT "FreeBSD"
#endif

#if defined(__hpux) || defined(hpux)
#  define SYSTEM_VARIANT "HP/UX"
#endif

#if defined(_IRIX) || defined(_IRIX4) || defined(_IRIX6)
#  define SYSTEM_VARIANT "Irix"
#endif

#ifdef __linux__
#  define SYSTEM_VARIANT "GNU/Linux"
#endif

#ifdef _NEXTOS
#  define SYSTEM_VARIANT "NeXT"
#endif

#ifdef __osf__
#  define SYSTEM_VARIANT "OSF"
#endif

#ifdef _PIXEL
#  define SYSTEM_VARIANT "Pixel"
#endif

#if defined(_SUNOS) || defined(_SUNOS3) || defined(_SUNOS4)
#  define SYSTEM_VARIANT "SunOS"
#endif

#ifdef _ULTRIX
#  define SYSTEM_VARIANT "Ultrix"
#endif

#ifndef SYSTEM_VARIANT
#  define SYSTEM_VARIANT "unknown"
#endif

#include "config.h"

#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif

/* GNU C library defines environ if __USE_GNU is defined.  */
#ifndef __USE_GNU
  extern char ** environ;
#endif

#ifdef STDC_HEADERS
#  include <stdlib.h>
#  include <string.h>
#else
#  ifndef HAVE_STRCHR
#    define strchr index
#    define strrchr rindex
#  endif
   extern char * strchr ();
   extern char * strrchr ();
#  ifndef HAVE_MEMCPY
#    define memcpy(d, s, n) bcopy ((s), (d), (n))
#    define memmove(d, s, n) bcopy ((s), (d), (n))
#  endif
#endif

#ifdef HAVE_SYS_FILE_H
#  include <sys/file.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#else
   extern int EXFUN (ioctl, (int, unsigned long, ...));
#endif

#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#else
   extern int EXFUN (open, (CONST char *, int, ...));
#endif

#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#else
#  ifndef WIFEXITED
#    define WIFEXITED(_X) (((_X) & 0x00FF) == 0)
#  endif
#  ifndef WIFSTOPPED
#    define WIFSTOPPED(_X) (((_X) & 0x00FF) == 0x007F)
#  endif
#  ifndef WIFSIGNALED
#    define WIFSIGNALED(_X)						\
       ((((_X) & 0x00FF) != 0) && (((_X) & 0x00FF) != 0x007F))
#  endif
#  ifndef WEXITSTATUS
#    define WEXITSTATUS(_X) (((_X) & 0xFF00) >> 8)
#  endif
#  ifndef WTERMSIG
#    define WTERMSIG(_X) ((_X) & 0x007F)
#  endif
#  ifndef WSTOPSIG
#    define WSTOPSIG(_X) (((_X) & 0xFF00) >> 8)
#  endif
   extern pid_t EXFUN (wait, (int *));
#  ifdef HAVE_WAITPID
     extern pid_t EXFUN (waitpid, (pid_t, int *, int));
#  endif
#  ifdef HAVE_WAIT3
     extern pid_t EXFUN (wait3, (int *, int, struct rusage *));
#  endif
#endif

#ifndef WUNTRACED
#  define WUNTRACED 0
#endif

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#  define NAMLEN(_D) (strlen ((_D) -> d_name))
#else
#  define dirent direct
#  define NAMLEN(_D) (strlen ((_D) -> d_namlen))
#  ifdef HAVE_SYS_NDIR_H
#    include <sys/ndir.h>
#  endif
#  ifdef HAVE_SYS_DIR_H
#    include <sys/dir.h>
#  endif
#  ifdef HAVE_NDIR_H
#    include <ndir.h>
#  endif
#endif

#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    include <time.h>
#  endif
#endif

#ifdef HAVE_UTIME_H
#  include <utime.h>
#else
   /* It's really there. */
   struct utimbuf
   {
     time_t actime;
     time_t modtime;
   };
   extern int EXFUN (utime, (CONST char *, struct utimbuf *)); 
#endif

#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#else
#  ifdef HAVE_TERMIO_H
#    include <termio.h>
#  else
#    ifdef HAVE_SGTTY_H
#      include <sgtty.h>
#    endif
#  endif
#endif

#ifdef HAVE_SYS_POLL_H
#  include <sys/poll.h>
#endif

#if defined(HAVE_SOCKET) && defined(HAVE_GETHOSTBYNAME) && defined(HAVE_GETHOSTNAME)
#  define HAVE_SOCKETS
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <netdb.h>
#  ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#    ifdef AF_UNIX
#      define HAVE_UNIX_SOCKETS
#    endif
#  endif
#endif

#ifdef HAVE_SYS_PTYIO_H
#include <sys/ptyio.h>
#endif

#ifdef HAVE_BSDTTY_H
#include <bsdtty.h>
#endif

#ifdef HAVE_STROPTS_H
#include <stropts.h>
#endif

#include "intext.h"
#include "dstack.h"
#include "osscheme.h"
#include "syscall.h"

typedef RETSIGTYPE Tsignal_handler_result;
typedef RETSIGTYPE (*Tsignal_handler) ();

#ifdef VOID_SIGNAL_HANDLERS
#  define SIGNAL_HANDLER_RETURN() return
#else
#  define SIGNAL_HANDLER_RETURN() return (0)
#endif

/* Crufty, but it will work here. */
#ifndef ENOSYS
#  define ENOSYS 0
#endif

#ifndef SIG_ERR
#  define SIG_ERR ((Tsignal_handler) (-1))
#endif

#if !defined(SIGCHLD) && defined(SIGCLD)
#  define SIGCHLD SIGCLD
#endif
#if !defined(SIGABRT) && defined(SIGIOT)
#  define SIGABRT SIGIOT
#endif

/* Provide null defaults for all the signals we're likely to use so we
   aren't continually testing to see if they're defined. */

#ifndef SIGLOST
#  define SIGLOST 0
#endif
#ifndef SIGWINCH
#  define SIGWINCH 0
#endif
#ifndef SIGWINDOW
#  define SIGWINDOW 0
#endif
#ifndef SIGXCPU
#  define SIGXCPU 0
#endif
#ifndef SIGXFSZ
#  define SIGXFSZ 0
#endif
#ifndef SIGURG
#  define SIGURG 0
#endif
#ifndef SIGIO
#  define SIGIO 0
#endif
#ifndef SIGUSR1
#  define SIGUSR1 0
#endif
#ifndef SIGUSR2
#  define SIGUSR2 0
#endif
#ifndef SIGVTALRM
#  define SIGVTALRM 0
#endif
#ifndef SIGABRT
#  define SIGABRT 0
#endif
#ifndef SIGPWR
#  define SIGPWR 0
#endif
#ifndef SIGPROF
#  define SIGPROF 0
#endif
#ifndef SIGSTOP
#  define SIGSTOP 0
#endif
#ifndef SIGTSTP
#  define SIGTSTP 0
#endif
#ifndef SIGCONT
#  define SIGCONT 0
#endif
#ifndef SIGCHLD
#  define SIGCHLD 0
#endif
#ifndef SIGTTIN
#  define SIGTTIN 0
#endif
#ifndef SIGTTOU
#  define SIGTTOU 0
#endif
#ifndef SIGBUS
#  define SIGBUS 0
#endif
#ifndef SIGEMT
#  define SIGEMT 0
#endif
#ifndef SIGSYS
#  define SIGSYS 0
#endif

/* constants for access() */
#ifndef R_OK
#  define R_OK 4
#  define W_OK 2
#  define X_OK 1
#  define F_OK 0
#endif

#ifndef MAXPATHLEN
#  define MAXPATHLEN 1024
#endif

#ifdef HAVE_STDC
#  define ALERT_CHAR '\a'
#  define ALERT_STRING "\a"
#else
#  define ALERT_CHAR '\007'
#  define ALERT_STRING "\007"
#endif

#ifndef STDIN_FILENO
#  define STDIN_FILENO 0
#  define STDOUT_FILENO 1
#  define STDERR_FILENO 2
#endif

/* constants for open() and fcntl() */
#ifndef O_RDONLY
#  define O_RDONLY 0
#  define O_WRONLY 1
#  define O_RDWR 2
#endif

/* mode bit definitions for open(), creat(), and chmod() */
#ifndef S_IRWXU
#  define S_IRWXU 0700
#  define S_IRWXG 0070
#  define S_IRWXO 0007
#endif

#ifndef S_IRUSR
#  define S_IRUSR 0400
#  define S_IWUSR 0200
#  define S_IXUSR 0100
#  define S_IRGRP 0040
#  define S_IWGRP 0020
#  define S_IXGRP 0010
#  define S_IROTH 0004
#  define S_IWOTH 0002
#  define S_IXOTH 0001
#endif

#define MODE_REG (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define MODE_DIR (MODE_REG | S_IXUSR | S_IXGRP | S_IXOTH)

/* constants for lseek() */
#ifndef SEEK_SET
#  define SEEK_SET 0
#  define SEEK_CUR 1
#  define SEEK_END 2
#endif

#ifdef HAVE_GETLOGIN
#  ifndef HAVE_UNISTD_H
     extern char * EXFUN (getlogin, (void));
#  endif
#endif

#ifndef STDC_HEADERS
#  ifndef HAVE_MALLOC_H
     extern PTR EXFUN (malloc, (size_t));
     extern PTR EXFUN (realloc, (PTR, size_t));
#  endif
   extern char * EXFUN (getenv, (CONST char *));
#endif

#define UX_abort abort
#define UX_accept accept
#define UX_access access
#define UX_alarm alarm
#define UX_bind bind
#define UX_chdir chdir
#define UX_chmod chmod
#define UX_close close
#define UX_connect connect
#define UX_ctime ctime
#define UX_dup dup
#define UX_fcntl fcntl
#define UX_free free
#define UX_fstat fstat
#define UX_fstatfs fstatfs
#define UX_ftruncate ftruncate
#define UX_getegid getegid
#define UX_getenv getenv
#define UX_geteuid geteuid
#define UX_getgid getgid
#define UX_getgrgid getgrgid
#define UX_gethostbyname gethostbyname
#define UX_gethostname gethostname
#define UX_getlogin getlogin
#define UX_getpid getpid
#define UX_getpwnam getpwnam
#define UX_getpwuid getpwuid
#define UX_getservbyname getservbyname
#define UX_gettimeofday gettimeofday
#define UX_getuid getuid
#define UX_gmtime gmtime
#define UX_ioctl ioctl
#define UX_link link
#define UX_listen listen
#define UX_localtime localtime
#define UX_lseek lseek
#define UX_malloc malloc
#define UX_mknod mknod
#define UX_mktime mktime
#define UX_open open
#define UX_pause pause
#define UX_pipe pipe
#define UX_read read
#define UX_readlink readlink
#define UX_realloc realloc
#define UX_rmdir rmdir
#define UX_select select
#define UX_setitimer setitimer
#define UX_signal signal
#define UX_sleep sleep
#define UX_socket socket
#define UX_stat stat
#define UX_statfs statfs
#define UX_symlink symlink
#define UX_system system
#define UX_time time
#define UX_times times
#define UX_truncate truncate
#define UX_unlink unlink
#define UX_utime utime
#define UX_vfork vfork
#define UX_wait wait
#define UX_write write

#ifdef HAVE_SYMLINK
#define UX_lstat lstat
#else
#define UX_lstat stat
#endif

#ifdef HAVE_DUP2
#  define UX_dup2 dup2
#else
#  ifdef HAVE_FCNTL
   extern int EXFUN (UX_dup2, (int, int));
#  define EMULATE_DUP2
#  define HAVE_DUP2
#  endif
#endif

#ifdef HAVE_GETCWD
#  define UX_getcwd getcwd
#else
   extern char * EXFUN (UX_getcwd, (char *, size_t));
#  define EMULATE_GETCWD
#  define HAVE_GETCWD
#endif

#ifdef HAVE_MKDIR
#  define UX_mkdir mkdir
#else
   extern int EXFUN (UX_mkdir, (CONST char *, mode_t));
#  define EMULATE_MKDIR
#  define HAVE_MKDIR
#endif

#ifdef HAVE_RENAME
#  define UX_rename rename
#else
   extern int EXFUN (UX_rename, (CONST char *, CONST char *));
#  define EMULATE_RENAME
#  define HAVE_RENAME
#endif

#ifdef HAVE_WAITPID
#  define UX_waitpid waitpid
#else
#  ifdef HAVE_WAIT3
   extern int EXFUN (UX_waitpid, (pid_t, int *, int));
#  define EMULATE_WAITPID
#  define HAVE_WAITPID
#  endif
#endif

#ifdef HAVE_CTERMID
#  define UX_ctermid ctermid
#else
   extern char * EXFUN (UX_ctermid, (char * s));
#  define EMULATE_CTERMID
#endif

#ifdef HAVE_KILL
#  define UX_kill kill
#else
   extern int EXFUN (UX_kill, (pid_t pid, int sig));
#  define EMULATE_KILL
#endif

#ifdef HAVE_GETPAGESIZE
#  define UX_getpagesize getpagesize
#else
   extern unsigned long EXFUN (UX_getpagesize, (void));
#  define EMULATE_GETPAGESIZE
#endif

#ifdef HAVE_POLL
#  ifndef INFTIM
#    define INFTIM (-1)
#  endif
#else
#  ifdef FD_SET
#    define SELECT_TYPE fd_set
#  else
#    define SELECT_TYPE int
#    define FD_SETSIZE ((sizeof (int)) * CHAR_BIT)
#    define FD_SET(n, p) ((*(p)) |= (1 << (n)))
#    define FD_CLR(n, p) ((*(p)) &= ~(1 << (n)))
#    define FD_ISSET(n, p) (((*(p)) & (1 << (n))) != 0)
#    define FD_ZERO(p) ((*(p)) = 0)
#  endif
#endif

#ifdef _POSIX_VERSION
#  define ERRNO_NONBLOCK EAGAIN
#  define FCNTL_NONBLOCK O_NONBLOCK
#else
#  ifdef EWOULDBLOCK
#    define ERRNO_NONBLOCK EWOULDBLOCK
#    define FCNTL_NONBLOCK FNDELAY
#  else
#    define AMBIGUOUS_NONBLOCK
#    ifdef EAGAIN
#      define ERRNO_NONBLOCK EAGAIN
#    endif
#    define FCNTL_NONBLOCK O_NDELAY
#  endif
#endif

#if defined(HAVE_GRANTPT) && defined(HAVE_STROPTS_H) && !defined(__osf__) && !defined(__linux__)
   /* Must push various STREAMS modules onto the slave side of a PTY
      when it is opened.  */
#  define SLAVE_PTY_P(filename) ((strncmp ((filename), "/dev/pts/", 9)) == 0)
   extern int EXFUN (UX_setup_slave_pty, (int));
#  define SETUP_SLAVE_PTY UX_setup_slave_pty
#endif

#ifndef TIOCSIGSEND
#  ifdef TIOCSIGNAL
#    define TIOCSIGSEND TIOCSIGNAL
#  else
#    ifdef TIOCSIG
#      define TIOCSIGSEND TIOCSIG
#    endif
#  endif
#endif

#ifdef HAVE_TERMIOS_H

typedef struct
{
  struct termios tio;
#ifdef HAVE_STRUCT_LTCHARS
  struct ltchars ltc;
#endif
} Ttty_state;

#define UX_tcflush tcflush
#define UX_tcdrain tcdrain
#define UX_tcgetattr tcgetattr
#define UX_tcsetattr tcsetattr

#else /* not HAVE_TERMIOS_H */

extern int EXFUN (UX_tcdrain, (int));
extern int EXFUN (UX_tcflush, (int, int));
/* These values chosen to match the ioctl TCFLSH argument for termio. */
#define TCIFLUSH 0
#define TCOFLUSH 1
#define TCIOFLUSH 2

#ifdef HAVE_TERMIO_H

typedef struct
{
  struct termio tio;
#ifdef HAVE_STRUCT_LTCHARS
  struct ltchars ltc;
#endif
} Ttty_state;

#else /* not HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

typedef struct
{
  struct sgttyb sg;
  struct tchars tc;
#ifdef HAVE_STRUCT_LTCHARS
  struct ltchars ltc;
#endif
  int lmode;
} Ttty_state;

#endif /* not HAVE_SGTTY_H */
#endif /* not HAVE_TERMIO_H */
#endif /* not HAVE_TERMIOS_H */

extern int EXFUN (UX_terminal_get_state, (int, Ttty_state *));
extern int EXFUN (UX_terminal_set_state, (int, Ttty_state *));

#ifdef _POSIX_VERSION
#  define UX_getpgrp getpgrp
#  define UX_setsid setsid
#  define UX_setpgid setpgid
#  define UX_tcgetpgrp tcgetpgrp
#  define UX_tcsetpgrp tcsetpgrp
#else
#  if defined(HAVE_GETPGRP) && defined(HAVE_SETPGRP)
#    ifdef GETPGRP_VOID
#      define UX_getpgrp getpgrp
#    else
       extern pid_t EXFUN (UX_getpgrp, (void));
#      define EMULATE_GETPGRP
#    endif
#    ifdef SETPGRP_VOID
#      define UX_setsid setpgrp
#    else
         extern pid_t EXFUN (UX_setsid, (void));
#        define EMULATE_SETSID
#    endif
#    ifdef HAVE_SETPGRP2
#      define UX_setpgid setpgrp2
#    else
#      ifdef SETPGRP_VOID
         extern int UX_setpgid (pid_t, pid_t);
#        define EMULATE_SETPGID
#      else
#        define UX_setpgid setpgrp
#      endif
#    endif
#  endif
   extern pid_t EXFUN (UX_tcgetpgrp, (int));
#  define EMULATE_TCGETPGRP
   extern int EXFUN (UX_tcsetpgrp, (int, pid_t));
#  define EMULATE_TCSETPGRP
#endif

#ifdef HAVE_SIGACTION

#define UX_sigemptyset sigemptyset
#define UX_sigfillset sigfillset
#define UX_sigaddset sigaddset
#define UX_sigdelset sigdelset
#define UX_sigismember sigismember
#define UX_sigaction sigaction
#define UX_sigsuspend sigsuspend
#define UX_sigprocmask sigprocmask
#define HAVE_POSIX_SIGNALS

#else /* not HAVE_SIGACTION */

typedef long sigset_t;
extern int EXFUN (UX_sigemptyset, (sigset_t *));
extern int EXFUN (UX_sigfillset, (sigset_t *));
extern int EXFUN (UX_sigaddset, (sigset_t *, int));
extern int EXFUN (UX_sigdelset, (sigset_t *, int));
extern int EXFUN (UX_sigismember, (CONST sigset_t *, int));

#ifdef HAVE_SIGVEC
#  define UX_sigvec sigvec
#else
#  ifdef HAVE_SIGVECTOR
#    define UX_sigvec sigvector
#    define HAVE_SIGVEC
#  endif
#endif

#ifdef HAVE_SIGVEC

struct sigaction
{
  Tsignal_handler sa_handler;
  sigset_t sa_mask;
  int sa_flags;
};

extern int EXFUN
  (UX_sigaction, (int, CONST struct sigaction *, struct sigaction *));
extern int EXFUN (UX_sigprocmask, (int, CONST sigset_t *, sigset_t *));
extern int EXFUN (UX_sigsuspend, (CONST sigset_t *));
#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

#define HAVE_POSIX_SIGNALS

#else /* not HAVE_SIGVEC */
#ifdef HAVE_SIGHOLD

#define UX_sigset sigset
#define UX_sighold sighold
#define UX_sigrelse sigrelse

#endif /* HAVE_SIGHOLD */
#endif /* HAVE_SIGVEC */
#endif /* HAVE_SIGACTION */

#ifdef _POSIX_VERSION

#  ifndef HAVE_FPATHCONF
     extern long EXFUN (fpathconf, (int, int));
#    define EMULATE_FPATHCONF
#  endif

#  ifndef HAVE_SYSCONF
     extern long EXFUN (sysconf, (int));
#    define EMULATE_SYSCONF
#  endif

   extern cc_t EXFUN (UX_PC_VDISABLE, (int fildes));
   extern clock_t EXFUN (UX_SC_CLK_TCK, (void));
#  define UX_SC_OPEN_MAX() ((size_t) (sysconf (_SC_OPEN_MAX)))
#  define UX_SC_CHILD_MAX() ((size_t) (sysconf (_SC_CHILD_MAX)))

#  ifdef _POSIX_JOB_CONTROL
#    define UX_SC_JOB_CONTROL() 1
#  else
#    define UX_SC_JOB_CONTROL() ((sysconf (_SC_JOB_CONTROL)) >= 0)
#  endif

#else /* not _POSIX_VERSION */

#  define UX_PC_VDISABLE(fildes) '\377'

#  ifdef OPEN_MAX
#    define UX_SC_OPEN_MAX() OPEN_MAX
#  else
#    ifdef _NFILE
#      define UX_SC_OPEN_MAX() _NFILE
#    else
#      define UX_SC_OPEN_MAX() 16
#    endif
#  endif

#  ifdef CHILD_MAX
#    define UX_SC_CHILD_MAX() CHILD_MAX
#  else
#    define UX_SC_CHILD_MAX() 6
#  endif

#  ifdef CLK_TCK
#    define UX_SC_CLK_TCK() CLK_TCK
#  else
#    ifdef HZ
#      define UX_SC_CLK_TCK() HZ
#    else
#      define UX_SC_CLK_TCK() 60
#    endif
#  endif

#  ifdef TIOCGPGRP
#    define UX_SC_JOB_CONTROL() 1
#  else
#    define UX_SC_JOB_CONTROL() 0
#  endif

#endif /* not _POSIX_VERSION */

extern void EXFUN (UX_prim_check_errno, (enum syscall_names name));

#define STD_VOID_SYSTEM_CALL(name, expression)				\
{									\
  while ((expression) < 0)						\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#define STD_UINT_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) < 0)					\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#define STD_PTR_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) == 0)				\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#endif /* SCM_UX_H */
