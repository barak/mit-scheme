/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/ux.c,v 1.6 1991/01/24 11:25:31 cph Exp $

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

void
DEFUN (UX_prim_check_errno, (name), enum syscall_names name)
{
  if (errno != EINTR)
    error_system_call (errno, name);
  deliver_pending_interrupts ();
}


#ifdef HAVE_TERMIOS

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((tcgetattr (fd, (& (s -> tio)))) < 0)
#ifdef _HPUX
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
DEFUN (UX_terminal_set_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((tcsetattr (fd, TCSANOW, (& (s -> tio)))) < 0)
#ifdef _HPUX
      || ((UX_ioctl (fd, TIOCSLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

#else /* not HAVE_TERMIOS */
#ifdef HAVE_TERMIO

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCGETA, (& (s -> tio)))) < 0)
#ifdef HAVE_BSD_JOB_CONTROL
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
DEFUN (UX_terminal_set_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCSETA, (& (s -> tio)))) < 0)
#ifdef HAVE_BSD_JOB_CONTROL
      || ((UX_ioctl (fd, TIOCSLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
DEFUN (UX_tcdrain, (fd), int fd)
{
  return (UX_ioctl (fd, TCSBRK, 1));
}

int
DEFUN (UX_tcflush, (fd, queue_selector), int fd AND int queue_selector)
{
  return (UX_ioctl (fd, TCFLSH, queue_selector));
}

#else /* not HAVE_TERMIO */

#ifdef HAVE_BSD_TTY_DRIVER

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TIOCGETP, (& (s -> sg)))) < 0)
      || ((UX_ioctl (fd, TIOCGETC, (& (s -> tc)))) < 0)
#ifdef HAVE_BSD_JOB_CONTROL
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      || ((UX_ioctl (fd, TIOCLGET, (& (s -> lmode)))) < 0))
     ? (-1) : 0);
}

int
DEFUN (UX_terminal_set_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TIOCSETN, (& (s -> sg)))) < 0)
      || ((UX_ioctl (fd, TIOCSETC, (& (s -> tc)))) < 0)
#ifdef HAVE_BSD_JOB_CONTROL
      || ((UX_ioctl (fd, TIOCSLTC, (& (s -> ltc)))) < 0)
#endif
      || ((UX_ioctl (fd, TIOCLSET, (& (s -> lmode)))) < 0))
     ? (-1) : 0);
}

int
DEFUN (UX_tcdrain, (fd), int fd)
{
  /* BSD provides no such feature -- pretend it worked. */
  return (0);
}

int
DEFUN (UX_tcflush, (fd, queue_selector), int fd AND int queue_selector)
{
  /* Losing BSD always flushes input and output together. */
  int zero = 0;
  return (UX_ioctl (fd, TIOCFLUSH, (&zero)));
}

#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIO */
#endif /* HAVE_TERMIOS */

#if !defined(_POSIX) && defined(_BSD)

pid_t
DEFUN_VOID (UX_getpgrp)
{
  return (getpgrp (getpid ()));
}

pid_t
DEFUN_VOID (UX_setsid)
{
#ifdef TIOCNOTTY
  int fd = (UX_open (BSD_DEV_TTY, O_RDWR, 0));
  if (fd >= 0)
    {
      UX_ioctl (fd, TIOCNOTTY, 0);
      UX_close (fd);
    }
#endif
  {
    pid_t pid = (getpid ());
    return (setpgrp (pid, pid));
  }
}

#ifndef _SUNOS

char *
DEFUN (UX_ctermid, (s), char * s)
{
  static char result [] = BSD_DEV_TTY;
  if (s == 0)
    return (result);
  strcpy (s, BSD_DEV_TTY);
  return (s);
}

int
DEFUN (UX_kill, (pid, sig), pid_t pid AND int sig)
{
  return ((pid >= 0) ? (kill (pid, sig)) : (killpg ((-pid), sig)));
}

#endif /* not _SUNOS */
#endif /* not _POSIX and _BSD */

#ifndef _POSIX
#ifdef HAVE_BSD_JOB_CONTROL

pid_t
DEFUN (UX_tcgetpgrp, (fd), int fd)
{
  pid_t pgrp_id;
  int result = (UX_ioctl (fd, TIOCGPGRP, (&pgrp_id)));
  return ((result < 0) ? result : pgrp_id);
}

int
DEFUN (UX_tcsetpgrp, (fd, pgrp_id),
       int fd AND
       pid_t pgrp_id)
{
  return (UX_ioctl (fd, TIOCSPGRP, (&pgrp_id)));
}

#else /* not HAVE_BSD_JOB_CONTROL */

pid_t
DEFUN (UX_tcgetpgrp, (fd), int fd)
{
  errno = ENOSYS;
  return (-1);
}

int
DEFUN (UX_tcsetpgrp, (fd, pgrp_id),
       int fd AND
       pid_t pgrp_id)
{
  errno = ENOSYS;
  return (-1);
}

#endif /* HAVE_BSD_JOB_CONTROL */
#endif /* not _POSIX */

#ifdef EMULATE_GETCWD
char *
DEFUN (UX_getcwd, (buffer, length),
       char * buffer AND
       size_t length)
{
  char internal_buffer [MAXPATHLEN + 2];
  char * collection_buffer;
  size_t collection_length;
  if (length <= 0)
    {
      errno = EINVAL;
      return (0);
    }
  /* Allocate the buffer if needed. */
  if (buffer == 0)
    {
      buffer = (UX_malloc (length));
      if (buffer == 0)
	{
	  errno = ENOMEM;
	  return (0);
	}
    }
  if (length >= (sizeof (internal_buffer)))
    {
      collection_buffer = buffer;
      collection_length = length;
    }
  else
    {
      collection_buffer = internal_buffer;
      collection_length = (sizeof (internal_buffer));
    }
#ifdef HAVE_GETWD
  if ((getwd (collection_buffer)) == 0)
    {
      errno = EACCES;
      return (0);
    }
#else /* not HAVE_GETWD */
  {
    /* Invoke `pwd' and fill the buffer with its output. */
    FILE * stream = (popen ("pwd", "r"));
    char * scan_buffer = collection_buffer;
    if (stream == 0)
      {
	errno = EACCES;
	return (0);
      }
    fgets (collection_buffer, collection_length, stream);
    pclose (stream);
    while (1)
      {
	int c = (*scan_buffer++);
	if (c == '\0')
	  break;
	else if (c == '\n')
	  {
	    (*--scan_buffer) = '\0'; /* remove extraneous newline */
	    break;
	  }
      }
  }
#endif /* HAVE_GETWD */
  if (collection_buffer == internal_buffer)
    {
      if (length <= (strlen (internal_buffer)))
	{
	  errno = ERANGE;
	  return (0);
	}
      strcpy (buffer, internal_buffer);
    }
  return (buffer);
}
#endif /* not EMULATE_GETCWD */

#ifdef EMULATE_WAITPID
int
DEFUN (UX_waitpid, (pid, stat_loc, options),
       pid_t pid AND
       wait_status_t * stat_loc AND
       int options)
{
  if (pid == (-1))
    return (wait3 (stat_loc, options, 0));
#ifdef HAVE_WAIT4
  else if (pid > 0)
    return (wait4 (pid, stat_loc, options, 0));
#endif
  errno = EINVAL;
  return (-1);
}
#endif /* EMULATE_WAITPID */

#ifdef EMULATE_DUP2
int
DEFUN (UX_dup2, (fd, fd2), int fd AND int fd2)
{
  if (fd != fd2)
    UX_close (fd2);
  {
    int result = (UX_fcntl (fd, F_DUPFD, fd2));
    if ((result < 0) && (errno == EINVAL))
      errno = EBADF;
    return (result);
  }
}
#endif /* EMULATE_DUP2 */

#ifdef EMULATE_RENAME
int
DEFUN (UX_rename, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  int result;
  if ((result = (UX_access (from_name, 0))) < 0)
    return (result);
  {
    struct stat fs;
    struct stat ts;
    if (((UX_stat (from_name, (&fs))) == 0) &&
	((UX_lstat (to_name, (&ts))) == 0))
      {
	if (((fs . st_dev) == (ts . st_dev)) &&
	    ((fs . st_ino) == (ts . st_ino)))
	  return (0);
	UX_unlink (to_name);
      }
  }
  return
    (((result = (UX_link (from_name, to_name))) < 0)
     ? result
     : (UX_unlink (from_name)));
}
#endif /* EMULATE_RENAME */

#ifdef EMULATE_MKDIR
int
DEFUN (UX_mkdir, (name, mode),
       CONST char * name AND
       mode_t mode)
{
  return (UX_mknod (name, ((mode & MODE_DIR) | S_IFDIR), ((dev_t) 0)));
}
#endif /* EMULATE_MKDIR */

#ifdef _POSIX

cc_t
DEFUN (UX_PC_VDISABLE, (fildes), int fildes)
{
  long result = (fpathconf (fildes, _PC_VDISABLE));
  return
    ((result < 0) ?
#ifdef _POSIX_VDISABLE
     _POSIX_VDISABLE
#else
     '\377'
#endif
     : result);
}

static clock_t memoized_clk_tck = 0;

clock_t
DEFUN_VOID (UX_SC_CLK_TCK)
{
  if (memoized_clk_tck == 0)
    memoized_clk_tck = ((clock_t) (sysconf (_SC_CLK_TCK)));
  return (memoized_clk_tck);
}

#endif /* _POSIX */
