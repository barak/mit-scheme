/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "ux.h"

#ifdef VALGRIND_MODE
#  include <valgrind/valgrind.h>
#endif

void
UX_prim_check_errno (enum syscall_names name)
{
  if (errno != EINTR)
    error_system_call (errno, name);
  deliver_pending_interrupts ();
}

bool UX_out_of_files_p = false;

void
UX_prim_check_fd_errno (enum syscall_names name)
{
  switch (errno)
    {
    case EINTR:
      deliver_pending_interrupts ();
      break;

    case EMFILE:
    case ENFILE:
      if (!UX_out_of_files_p)
	{
	  UX_out_of_files_p = true;
	  REQUEST_GC (0);
	  deliver_pending_interrupts ();
	}
      /* Fall through */

    default:
      error_system_call (errno, name);
    }
}

#ifdef HAVE_TERMIOS_H

int
UX_terminal_get_state (int fd, Ttty_state * s)
{
  return
    ((((tcgetattr (fd, (& (s -> tio)))) < 0)
#ifdef __HPUX__
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
UX_terminal_set_state (int fd, Ttty_state * s)
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
UX_terminal_get_state (int fd, Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCGETA, (& (s -> tio)))) < 0)
#ifdef HAVE_STRUCT_LTCHARS
      || ((UX_ioctl (fd, TIOCGLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
UX_terminal_set_state (int fd, Ttty_state * s)
{
  return
    ((((UX_ioctl (fd, TCSETA, (& (s -> tio)))) < 0)
#ifdef HAVE_STRUCT_LTCHARS
      || ((UX_ioctl (fd, TIOCSLTC, (& (s -> ltc)))) < 0)
#endif
      ) ? (-1) : 0);
}

int
UX_tcdrain (int fd)
{
  return (UX_ioctl (fd, TCSBRK, 1));
}

int
UX_tcflush (int fd, int queue_selector)
{
  return (UX_ioctl (fd, TCFLSH, queue_selector));
}

#else /* not HAVE_TERMIO_H */

#ifdef HAVE_SGTTY_H

int
UX_terminal_get_state (int fd, Ttty_state * s)
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
UX_terminal_set_state (int fd, Ttty_state * s)
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
UX_tcdrain (int fd)
{
  /* BSD provides no such feature -- pretend it worked. */
  return (0);
}

int
UX_tcflush (int fd, int queue_selector)
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
UX_setup_slave_pty (int fd)
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
UX_getpgrp (void)
{
  return (getpgrp (getpid ()));
}
#endif

#ifdef EMULATE_SETSID
pid_t
UX_setsid (void)
{
#ifdef TIOCNOTTY
  int fd = (UX_open ("/dev/tty", O_RDWR, 0));
  if (fd >= 0)
    {
      (void) UX_ioctl (fd, TIOCNOTTY, 0);
      (void) UX_close (fd);
    }
#endif
  return (setpgrp (0, 0));
}
#endif

#ifdef EMULATE_SETPGID
int
UX_setpgid (pid_t pid, pid_t pgid)
{
  errno = ENOSYS;
  return (-1);
}
#endif

#ifdef EMULATE_CTERMID
char *
UX_ctermid (char * s)
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
UX_kill (pid_t pid, int sig)
{
  return ((pid >= 0) ? (kill (pid, sig)) : (killpg ((-pid), sig)));
}
#endif

#ifdef EMULATE_TCGETPGRP
pid_t
UX_tcgetpgrp (int fd)
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
UX_tcsetpgrp (int fd, pid_t pgrp_id)
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
UX_getcwd (char * buffer, size_t length)
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
UX_waitpid (pid_t pid, int * stat_loc, int options)
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
UX_dup2 (int fd, int fd2)
{
  if (fd != fd2)
    (void) UX_close (fd2);
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
UX_rename (const char * from_name, const char * to_name)
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
UX_mkdir (const char * name, mode_t mode)
{
  return (UX_mknod (name, ((mode & MODE_DIR) | S_IFDIR), ((dev_t) 0)));
}
#endif

#ifdef _POSIX_VERSION

cc_t
UX_PC_VDISABLE (int fildes)
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
UX_SC_CLK_TCK (void)
{
  if (memoized_clk_tck == 0)
    memoized_clk_tck = ((clock_t) (sysconf (_SC_CLK_TCK)));
  return (memoized_clk_tck);
}

#endif /* _POSIX_VERSION */

#ifndef HAVE_SIGACTION

int
UX_sigemptyset (sigset_t * set)
{
  (*set) = 0;
  return (0);
}

int
UX_sigfillset (sigset_t * set)
{
  (*set) = (-1);
  return (0);
}

int
UX_sigaddset (sigset_t * set, int signo)
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
UX_sigdelset (sigset_t * set, int signo)
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
UX_sigismember (const sigset_t * set, int signo)
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
UX_sigaction (int signo, const struct sigaction * act, struct sigaction * oact)
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
UX_sigprocmask (int how, const sigset_t * set, sigset_t * oset)
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
UX_sigsuspend (const sigset_t * set)
{
  return (sigpause (*set));
}

#endif /* HAVE_SIGVEC */
#endif /* not _POSIX_VERSION */

#ifdef EMULATE_SYSCONF
long
sysconf (int parameter)
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
fpathconf (int filedes, int parameter)
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

#ifdef EMULATE_CLOSEFROM

int
UX_closefrom (int fd)
{
#if ((defined (HAVE_FCNTL)) && (defined (F_CLOSEM)))
  return (UX_fcntl (fd, F_CLOSEM));
#elif ((defined (HAVE_FCNTL)) && (defined (F_MAXFD)))
  int max_fd = (UX_fcntl ((-1), F_MAXFD));
  int status = 0, error = 0;
  if (max_fd < 0) return (max_fd);
  while (fd <= max_fd)
    if (((UX_close (fd++)) < 0) && (errno != EBADF))
      status = (-1), error = errno;
  errno = error;
  return (status);
#else
  int fd_limit = (UX_SC_OPEN_MAX ());
  int status = 0, error = 0;
  while (fd < fd_limit)
    if (((UX_close (fd++)) < 0) && (errno != EBADF))
      status = (-1), error = errno;
  errno = error;
  return (status);
#endif
}

#endif

void *
OS_malloc_init (size_t size)
{
  return (UX_malloc (size));
}

void *
OS_malloc (size_t size)
{
  void * result = (UX_malloc (size));
  if (result == 0)
    error_system_call (ENOMEM, syscall_malloc);
  return (result);
}

void *
OS_realloc (void * ptr, size_t size)
{
  void * result = (UX_realloc (ptr, size));
  if (result == 0)
    error_system_call (ENOMEM, syscall_realloc);
  return (result);
}

void
OS_free (void * ptr)
{
  UX_free (ptr);
}

#ifdef EMULATE_GETPAGESIZE
#ifdef HAVE_SYSCONF

unsigned long
UX_getpagesize (void)
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

static void * mmap_heap_malloc_search
  (unsigned long, unsigned long, unsigned long);
static void * mmap_heap_malloc_search_procfs
  (unsigned long, unsigned long, unsigned long);

void *
mmap_heap_malloc (unsigned long requested_length)
{
  const unsigned long pagesize = (UX_getpagesize ());
  const unsigned long min_result = MMAP_BASE_ADDRESS;
  const unsigned long max_result = (1UL << DATUM_LENGTH);
  const unsigned long request =
    (((requested_length + (pagesize - 1)) / pagesize) * pagesize);
  void * const addr =
    (mmap_heap_malloc_search (request, min_result, max_result));

  if (addr != 0)
    {
      if ((((unsigned long) addr) >= min_result)
	  && ((((unsigned long) addr) + request) <= max_result))
	{
#ifdef VALGRIND_MODE
	  VALGRIND_MALLOCLIKE_BLOCK (addr, request, 0, 0);
#endif
	  return (addr);
	}
      if ((munmap (addr, request)) == -1)
	  outf_error ("unable to unmap heap: %lx bytes at %p", request, addr);
    }

#ifdef CC_IS_NATIVE
  outf_error
    ("unable to mmap executable heap -- native code will probably fail");
#endif
  return (OS_malloc (requested_length));
}

#ifndef MAP_TRYFIXED
#  define MAP_TRYFIXED 0
#endif

static void *
mmap_heap_malloc_try (unsigned long address, unsigned long request, int flags)
{
  assert ((address == 0) || ((flags & (MAP_TRYFIXED | MAP_FIXED)) != 0));
  void * addr
    = (mmap (((void *) address),
	     request,
	     (PROT_EXEC | PROT_READ | PROT_WRITE),
	     (MAP_PRIVATE | MAP_ANONYMOUS | flags),
	     (-1),
	     0));
  return ((addr == MAP_FAILED) ? 0 : addr);
}

static void *
mmap_heap_malloc_search (unsigned long request,
			 unsigned long min_result,
			 unsigned long max_result)
{
  void * addr;

  assert (0 < request);
  assert (0 < min_result);
  assert (min_result < max_result);
  assert (request <= (max_result - min_result));

#ifdef USE_MAP_FIXED
  (void) max_result;		/* ignore */
  addr = (mmap_heap_malloc_try (min_result, request, MAP_FIXED));
#else
  addr = (mmap_heap_malloc_search_procfs (request, min_result, max_result));
  if (addr == 0)
    addr = (mmap_heap_malloc_try (min_result, request, MAP_TRYFIXED));
  if (addr == 0)
    addr = (mmap_heap_malloc_try (0, request, 0));
#endif

  return (addr);
}

#ifdef __linux__
#  define TRY_PROCFS_MAPS 1
#endif

#ifdef __NetBSD__
#  define TRY_PROCFS_MAPS 1
#endif

#ifdef TRY_PROCFS_MAPS

/* Use /proc/self/maps to find the lowest available space.  */

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

static void *
mmap_heap_malloc_search_procfs (unsigned long request,
				unsigned long min_result,
				unsigned long max_result)
{
  char fn [64];
  FILE * s;
  unsigned long start;

  /* AppArmor can specify a minimum usable address.  In that case we
     need to detect it and compensate.  */
  s = (fopen ("/proc/sys/vm/mmap_min_addr", "r"));
  if (s != 0)
    {
      unsigned long new_min_result;
      int rc = (fscanf (s, "%lu", (&new_min_result)));
      fclose (s);
      if ((rc == 1) && (new_min_result > min_result))
        min_result = new_min_result;
    }

  snprintf (fn, (sizeof fn), "/proc/%d/maps", (getpid ()));
  s = (fopen (fn, "r"));
  if (s == 0)
    return (0);

  start = min_result;
  while ((start + request) <= max_result)
    {
      unsigned long end;
      unsigned long next_start;
      int rc = (fscanf (s, "%lx-%lx ", (&end), (&next_start)));
      if (rc == EOF)
        {
          fclose (s);
          return (0);
        }
      if (! ((rc == 2) && (end <= next_start) && (discard_line (s))))
        {
          fclose (s);
          return (0);
        }
      if ((start + request) <= end)
        {
          fclose (s);
          /* The space is known free, so we can safely request it fixed.  */
          return (mmap_heap_malloc_try (start, request, MAP_FIXED));
        }
      start = next_start;
    }

  fclose (s);
  return (0);
}

#else /* !defined(USE_PROCFS_MAPS) */

static void *
mmap_heap_malloc_search_procfs (unsigned long request,
				unsigned long min_result,
				unsigned long max_result)
{
  (void) request;		/* ignore */
  (void) min_result;		/* ignore */
  (void) max_result;		/* ignore */

  return (0);
}

#endif /* USE_PROCFS_MAPS */

#endif /* USE_MMAP_HEAP_MALLOC && HEAP_IN_LOW_MEMORY */
