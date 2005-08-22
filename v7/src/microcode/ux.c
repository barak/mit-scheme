/* -*-C-*-

$Id: ux.c,v 1.27 2005/08/22 01:15:07 cph Exp $

Copyright 1991,1992,1993,1996,1997,2000 Massachusetts Institute of Technology
Copyright 2002,2003,2005 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "ux.h"

#ifdef VALGRIND_MODE
#  include <valgrind/valgrind.h>
#endif

void
DEFUN (UX_prim_check_errno, (name), enum syscall_names name)
{
  if (errno != EINTR)
    error_system_call (errno, name);
  deliver_pending_interrupts ();
}

#ifdef HAVE_TERMIOS_H

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((tcgetattr (fd, (& (s -> tio)))) < 0)
#ifdef __HPUX__
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
DEFUN (UX_terminal_set_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((tcsetattr (fd, TCSANOW, (& (s -> tio)))) < 0)
#ifdef __HPUX__
      || ((UX_ioctl (fd, TIOCSLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

#else /* not HAVE_TERMIOS_H */
#ifdef HAVE_TERMIO_H

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCGETA, (& (s -> tio)))) < 0)
#ifdef HAVE_STRUCT_LTCHARS
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
DEFUN (UX_terminal_set_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCSETA, (& (s -> tio)))) < 0)
#ifdef HAVE_STRUCT_LTCHARS
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

#else /* not HAVE_TERMIO_H */

#ifdef HAVE_SGTTY_H

int
DEFUN (UX_terminal_get_state, (fd, s), int fd AND Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TIOCGETP, (& (s -> sg)))) < 0)
      || ((UX_ioctl (fd, TIOCGETC, (& (s -> tc)))) < 0)
#ifdef HAVE_STRUCT_LTCHARS
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
#ifdef HAVE_STRUCT_LTCHARS
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

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIO_H */
#endif /* HAVE_TERMIOS_H */

#ifdef SLAVE_PTY_P
int
DEFUN (UX_setup_slave_pty, (fd), int fd)
{
  return
    (((ioctl (fd, I_PUSH, "ptem")) == 0)
     && ((ioctl (fd, I_PUSH, "ldterm")) == 0)
#if !defined(sgi) && !defined(__sgi)
     && (((ioctl (fd, I_FIND, "ttcompat")) != 0)
	 || ((ioctl (fd, I_PUSH, "ttcompat")) == 0))
#endif
     );
}
#endif

#ifdef EMULATE_GETPGRP
pid_t
DEFUN_VOID (UX_getpgrp)
{
  return (getpgrp (getpid ()));
}
#endif

#ifdef EMULATE_SETSID
pid_t
DEFUN_VOID (UX_setsid)
{
#ifdef TIOCNOTTY
  int fd = (UX_open ("/dev/tty", O_RDWR, 0));
  if (fd >= 0)
    {
      UX_ioctl (fd, TIOCNOTTY, 0);
      UX_close (fd);
    }
#endif
  return (setpgrp (0, 0));
}
#endif

#ifdef EMULATE_SETPGID
int
DEFUN (UX_setpgid, (pid, pgid), pid_t pid AND pid_t pgid)
{
  errno = ENOSYS;
  return (-1);
}
#endif

#ifdef EMULATE_CTERMID
char *
DEFUN (UX_ctermid, (s), char * s)
{
  static char result [] = "/dev/tty";
  if (s == 0)
    return (result);
  strcpy (s, result);
  return (s);
}
#endif

#ifdef EMULATE_KILL
int
DEFUN (UX_kill, (pid, sig), pid_t pid AND int sig)
{
  return ((pid >= 0) ? (kill (pid, sig)) : (killpg ((-pid), sig)));
}
#endif

#ifdef EMULATE_TCGETPGRP
pid_t
DEFUN (UX_tcgetpgrp, (fd), int fd)
{
#ifdef TIOCGPGRP
  pid_t pgrp_id;
  return (((UX_ioctl (fd, TIOCGPGRP, (&pgrp_id))) < 0) ? (-1) : pgrp_id);
#else
  errno = ENOSYS;
  return (-1);
#endif
}
#endif

#ifdef EMULATE_TCSETPGRP
int
DEFUN (UX_tcsetpgrp, (fd, pgrp_id),
       int fd AND
       pid_t pgrp_id)
{
#ifdef TIOCSPGRP
  return (UX_ioctl (fd, TIOCSPGRP, (&pgrp_id)));
#else
  errno = ENOSYS;
  return (-1);
#endif
}
#endif

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
#endif /* not HAVE_GETWD */
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
#endif /* EMULATE_GETCWD */

#ifdef EMULATE_WAITPID
int
DEFUN (UX_waitpid, (pid, stat_loc, options),
       pid_t pid AND
       int * stat_loc AND
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
#endif

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
#endif

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
#endif

#ifdef EMULATE_MKDIR
int
DEFUN (UX_mkdir, (name, mode),
       CONST char * name AND
       mode_t mode)
{
  return (UX_mknod (name, ((mode & MODE_DIR) | S_IFDIR), ((dev_t) 0)));
}
#endif

#ifdef _POSIX_VERSION

cc_t
DEFUN (UX_PC_VDISABLE, (fildes), int fildes)
{
#ifdef _POSIX_VDISABLE
  return ((cc_t) _POSIX_VDISABLE);
#else
  long result = (fpathconf (fildes, _PC_VDISABLE));
  return ((cc_t) ((result < 0) ? '\377' : result));
#endif
}

static clock_t memoized_clk_tck = 0;

clock_t
DEFUN_VOID (UX_SC_CLK_TCK)
{
  if (memoized_clk_tck == 0)
    memoized_clk_tck = ((clock_t) (sysconf (_SC_CLK_TCK)));
  return (memoized_clk_tck);
}

#endif /* _POSIX_VERSION */

#ifndef HAVE_SIGACTION

int
DEFUN (UX_sigemptyset, (set), sigset_t * set)
{
  (*set) = 0;
  return (0);
}

int
DEFUN (UX_sigfillset, (set), sigset_t * set)
{
  (*set) = (-1);
  return (0);
}

int
DEFUN (UX_sigaddset, (set, signo), sigset_t * set AND int signo)
{
  if (signo <= 0)
    return (-1);
  {
    int mask = (1 << (signo - 1));
    if (mask == 0)
      return (-1);
    (*set) |= mask;
    return (0);
  }
}

int
DEFUN (UX_sigdelset, (set, signo), sigset_t * set AND int signo)
{
  if (signo <= 0)
    return (-1);
  {
    int mask = (1 << (signo - 1));
    if (mask == 0)
      return (-1);
    (*set) &=~ mask;
    return (0);
  }
}

int
DEFUN (UX_sigismember, (set, signo), CONST sigset_t * set AND int signo)
{
  if (signo <= 0)
    return (-1);
  {
    int mask = (1 << (signo - 1));
    if (mask == 0)
      return (-1);
    return (((*set) & mask) != 0);
  }
}

#ifdef HAVE_SIGVEC

#ifndef SV_INTERRUPT
#  define SV_INTERRUPT 0
#endif

int
DEFUN (UX_sigaction, (signo, act, oact),
       int signo AND
       CONST struct sigaction * act AND
       struct sigaction * oact)
{
  struct sigvec svec;
  struct sigvec sovec;
  struct sigvec * vec = ((act != 0) ? (&svec) : 0);
  struct sigvec * ovec = ((oact != 0) ? (&sovec) : 0);
  if (act != 0)
    {
      (vec -> sv_handler) = (act -> sa_handler);
      (vec -> sv_mask) = (act -> sa_mask);
      (vec -> sv_flags) = SV_INTERRUPT;
    }
  if ((UX_sigvec (signo, vec, ovec)) < 0)
    return (-1);
  if (oact != 0)
    {
      (oact -> sa_handler) = (ovec -> sv_handler);
      (oact -> sa_mask) = (ovec -> sv_mask);
      (oact -> sa_flags) = 0;
    }
  return (0);
}

int
DEFUN (UX_sigprocmask, (how, set, oset),
       int how AND
       CONST sigset_t * set AND
       sigset_t * oset)
{
  long omask;
  if (set == 0)
    omask = (sigblock (0));
  else
    switch (how)
      {
      case SIG_BLOCK:
	omask = (sigblock (*set));
	break;
      case SIG_UNBLOCK:
	omask = (sigblock (0));
	if (omask < 0) return (-1);
	omask = (sigsetmask (omask &~ (*set)));
	break;
      case SIG_SETMASK:
	omask = (sigsetmask (*set));
	break;
      default:
	errno = EINVAL;
	return (-1);
      }
  if (omask < 0) return (-1);
  if (oset != 0) (*oset) = omask;
  return (0);
}

int
DEFUN (UX_sigsuspend, (set), CONST sigset_t * set)
{
  return (sigpause (*set));
}

#endif /* HAVE_SIGVEC */
#endif /* not _POSIX_VERSION */

#ifdef EMULATE_SYSCONF
long
DEFUN (sysconf, (parameter), int parameter)
{
  switch (parameter)
  {
    case _SC_CLK_TCK:
#ifdef CLK_TCK
      return ((long) (CLK_TCK));
#else
#ifdef HZ
      return ((long) HZ);
#else
      return (60);
#endif /* HZ */
#endif /* CLK_TCK */

    case _SC_OPEN_MAX:
#ifdef OPEN_MAX
      return ((long) OPEN_MAX);
#else
#ifdef _NFILE
      return ((long) _NFILE);
#else
      return ((long) 16);
#endif /* _NFILE */
#endif /* OPEN_MAX */

    case _SC_CHILD_MAX:
#ifdef CHILD_MAX
      return ((long) CHILD_MAX);
#else
      return ((long) 6);
#endif /* CHILD_MAX */

    case _SC_JOB_CONTROL:
#ifdef TIOCGPGRP
      return ((long) 1);
#else
      return ((long) 0);
#endif

    default:
      errno = EINVAL;
      return ((long) (-1));
  }
}
#endif /* EMULATE_SYSCONF */

#ifdef EMULATE_FPATHCONF
long
DEFUN (fpathconf, (filedes, parameter), int filedes AND int parameter)
{
  switch (parameter)
  {
    case _PC_VDISABLE:
      return ((long) '\377');

    default:
      errno = EINVAL;
      return ((long) (-1));
  }
}
#endif /* EMULATE_FPATHCONF */

void *
DEFUN (OS_malloc, (size), unsigned int size)
{
  void * result = (UX_malloc (size));
  if (result == 0)
    error_system_call (ENOMEM, syscall_malloc);
  return (result);
}

void *
DEFUN (OS_realloc, (ptr, size), void * ptr AND unsigned int size)
{
  void * result = (UX_realloc (ptr, size));
  if (result == 0)
    error_system_call (ENOMEM, syscall_realloc);
  return (result);
}

void
DEFUN (OS_free, (ptr), void * ptr)
{
  UX_free (ptr);
}

#ifdef EMULATE_GETPAGESIZE
#ifdef HAVE_SYSCONF

unsigned long
DEFUN_VOID (UX_getpagesize)
{
  static int vp = 0;
  static long v;
  if (!vp)
    {
      v = (sysconf (_SC_PAGESIZE));
      vp = 1;
    }
  return ((v <= 0) ? 0x1000 : v);
}

#endif /* HAVE_SYSCONF */
#endif /* EMULATE_GETPAGESIZE */

#if defined(USE_MMAP_HEAP_MALLOC) && defined(HEAP_IN_LOW_MEMORY)

#ifdef VALGRIND_MODE
#  define MMAP_BASE_ADDRESS 0x10000
#else
#  define MMAP_BASE_ADDRESS (UX_getpagesize ())
#endif

#ifndef MAP_FAILED
#  define MAP_FAILED ((void *) (-1))
#endif

typedef enum
  {
    fsa_no_proc,
    fsa_no_address,
    fsa_good_address
  } fsa_status_t;

static void * mmap_heap_malloc_1 (unsigned long, unsigned long, int);
static fsa_status_t find_suitable_address
  (unsigned long, unsigned long, unsigned long, unsigned long *);
static int discard_line (FILE *);

void *
mmap_heap_malloc (unsigned long requested_length)
{
  unsigned long min_result = MMAP_BASE_ADDRESS;
  unsigned long max_result = (1UL << DATUM_LENGTH);
  unsigned long request;
  unsigned long search_result;
  void * result;

  {
    unsigned long ps = (UX_getpagesize ());
    request = (((requested_length + (ps - 1)) / ps) * ps);
  }
  switch (find_suitable_address (request,
				 min_result,
				 max_result,
				 (&search_result)))
    {
    case fsa_good_address:
      result = (mmap_heap_malloc_1 (search_result, request, true));
      break;

    case fsa_no_address:
      result = 0;
      break;

    default:
      result = (mmap_heap_malloc_1 (min_result, request, false));
      break;
    }
  if (result != 0)
    {
      if ((((unsigned long) result) >= min_result)
	  && ((((unsigned long) result) + request) <= max_result))
	{
#ifdef VALGRIND_MODE
	  VALGRIND_MALLOCLIKE_BLOCK (result, request, 0, 0);
#endif
	  return (result);
	}
      munmap (result, request);
    }
  return (OS_malloc (requested_length));
}

static void *
mmap_heap_malloc_1 (unsigned long address, unsigned long request, int fixedp)
{
  void * addr
    = (mmap (((void *) address),
	     request,
	     (PROT_EXEC | PROT_READ | PROT_WRITE),
	     (MAP_PRIVATE | MAP_ANONYMOUS | (fixedp ? MAP_FIXED : 0)),
	     /* Ignored by GNU/Linux, required by FreeBSD and Solaris.  */
	     (-1),
	     0));
  return ((addr == MAP_FAILED) ? 0 : addr);
}

static fsa_status_t
find_suitable_address (unsigned long n_bytes,
		       unsigned long lower_limit,
		       unsigned long upper_limit,
		       unsigned long * addr_r)
{
  char fn [64];
  FILE * s;
  unsigned long start = lower_limit;

  sprintf (fn, "/proc/%d/maps", (getpid ()));
  s = (fopen (fn, "r"));
  if (s == 0)
    return (fsa_no_proc);

  while ((start + n_bytes) <= upper_limit)
    {
      unsigned long end;
      unsigned long next_start;
      int rc = (fscanf (s, "%lx-%lx ", (&end), (&next_start)));
      if (rc == EOF)
	{
	  fclose (s);
	  return (fsa_no_address);
	}
      if (! ((rc == 2) && (end <= next_start) && (discard_line (s))))
	{
	  fclose (s);
	  return (fsa_no_proc);
	}
      if ((start + n_bytes) <= end)
	{
	  (*addr_r) = start;
	  fclose (s);
	  return (fsa_good_address);
	}
      start = next_start;
    }
  fclose (s);
  return (fsa_no_address);
}

static int
discard_line (FILE * s)
{
  while (1)
    {
      int c = (fgetc (s));
      if (c == EOF)
	return (0);
      if (c == '\n')
	return (1);
    }
}

#endif /* USE_MMAP_HEAP_MALLOC && HEAP_IN_LOW_MEMORY */
