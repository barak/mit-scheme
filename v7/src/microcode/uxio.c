/* -*-C-*-

$Id: uxio.c,v 1.57 2007/04/22 16:31:23 cph Exp $

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

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "uxio.h"
#include "uxselect.h"
#include "uxproc.h"

size_t OS_channel_table_size;
struct channel * channel_table;

#ifndef HAVE_POLL
static SELECT_TYPE input_descriptors;
#ifdef HAVE_SELECT
static struct timeval zero_timeout;
#endif
#endif

static void
UX_channel_close_all (void)
{
  Tchannel channel;
  for (channel = 0; (channel < OS_channel_table_size); channel += 1)
    if (CHANNEL_OPEN_P (channel))
      OS_channel_close_noerror (channel);
}

void
UX_initialize_channels (void)
{
  OS_channel_table_size = (UX_SC_OPEN_MAX ());
  channel_table =
    (UX_malloc (OS_channel_table_size * (sizeof (struct channel))));
  if (channel_table == 0)
    {
      fprintf (stderr, "\nUnable to allocate channel table.\n");
      fflush (stderr);
      termination_init_error ();
    }
  {
    Tchannel channel;
    for (channel = 0; (channel < OS_channel_table_size); channel += 1)
      MARK_CHANNEL_CLOSED (channel);
  }
  add_reload_cleanup (UX_channel_close_all);
#ifndef HAVE_POLL
  FD_ZERO (&input_descriptors);
#ifdef HAVE_SELECT
  (zero_timeout . tv_sec) = 0;
  (zero_timeout . tv_usec) = 0;
#endif
#endif
}

void
UX_reset_channels (void)
{
  UX_free (channel_table);
  channel_table = 0;
  OS_channel_table_size = 0;
}

Tchannel
channel_allocate (void)
{
  Tchannel channel = 0;
  while (1)
    {
      if (channel == OS_channel_table_size)
	error_out_of_channels ();
      if (CHANNEL_CLOSED_P (channel))
	return (channel);
      channel += 1;
    }
}

int
UX_channel_descriptor (Tchannel channel)
{
  return (CHANNEL_DESCRIPTOR (channel));
}

int
OS_channel_open_p (Tchannel channel)
{
  return (CHANNEL_OPEN_P (channel));
}

void
OS_channel_close (Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
    {
      STD_VOID_SYSTEM_CALL
	(syscall_close, (UX_close (CHANNEL_DESCRIPTOR (channel))));
      MARK_CHANNEL_CLOSED (channel);
    }
}

void
OS_channel_close_noerror (Tchannel channel)
{
  if (! (CHANNEL_INTERNAL (channel)))
    {
      UX_close (CHANNEL_DESCRIPTOR (channel));
      MARK_CHANNEL_CLOSED (channel);
    }
}

static void
channel_close_on_abort_1 (void * cp)
{
  OS_channel_close (* ((Tchannel *) cp));
}

void
OS_channel_close_on_abort (Tchannel channel)
{
  Tchannel * cp = (dstack_alloc (sizeof (Tchannel)));
  (*cp) = (channel);
  transaction_record_action (tat_abort, channel_close_on_abort_1, cp);
}

enum channel_type
OS_channel_type (Tchannel channel)
{
  return (CHANNEL_TYPE (channel));
}

long
OS_channel_read (Tchannel channel, void * buffer, size_t nbytes)
{
  if (nbytes == 0)
    return (0);
  while (1)
    {
      long scr = (UX_read ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
      if (scr < 0)
	{
#ifdef _ULTRIX
	/* This is needed for non-POSIX-ified master pseudo-terminal
	   driver, which always returns EWOULDBLOCK, even to POSIX
	   applications. */
	if (CHANNEL_TYPE (channel) == channel_type_unix_pty_master)
	  {
	    if (errno == EWOULDBLOCK)
	      return (-1);
	  }
	else
	  {
	    if (errno == EAGAIN)
	      return (-1);
	  }
#else
#ifdef ERRNO_NONBLOCK
	if (errno == ERRNO_NONBLOCK)
	  return (-1);
#endif
#endif /* not _ULTRIX */
	  UX_prim_check_errno (syscall_read);
	  continue;
	}
      if (scr > nbytes)
	error_external_return ();
#ifdef AMBIGUOUS_NONBLOCK
      return ((scr > 0) ? scr : (CHANNEL_NONBLOCKING (channel)) ? (-1) : 0);
#else
      return (scr);
#endif
    }
}

long
OS_channel_write (Tchannel channel, const void * buffer, size_t nbytes)
{
  if (nbytes == 0)
    return (0);
  while (1)
    {
      long scr = (UX_write ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
      if (scr < 0)
	{
#ifdef ERRNO_NONBLOCK
	  if (errno == ERRNO_NONBLOCK)
	    return (-1);
#endif
	  UX_prim_check_errno (syscall_write);
	  continue;
	}
      if (scr > nbytes)
	error_external_return ();
      return ((scr > 0) ? scr : (-1));
    }
}

size_t
OS_channel_read_load_file (Tchannel channel, void * buffer, size_t nbytes)
{
  int scr = (UX_read ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
  return ((scr < 0) ? 0 : scr);
}

size_t
OS_channel_write_dump_file (Tchannel channel,
			    const void * buffer,
			    size_t nbytes)
{
  int scr = (UX_write ((CHANNEL_DESCRIPTOR (channel)), buffer, nbytes));
  return ((scr < 0) ? 0 : scr);
}

void
OS_channel_write_string (Tchannel channel, const char * string)
{
  unsigned long length = (strlen (string));
  if ((OS_channel_write (channel, string, length)) != length)
    error_external_return ();
}

void
OS_make_pipe (Tchannel * readerp, Tchannel * writerp)
{
  int pv [2];
  transaction_begin ();
  STD_VOID_SYSTEM_CALL (syscall_pipe, (UX_pipe (pv)));
  MAKE_CHANNEL ((pv[0]), channel_type_unix_pipe, (*readerp) =);
  OS_channel_close_on_abort (*readerp);
  MAKE_CHANNEL ((pv[1]), channel_type_unix_pipe, (*writerp) =);
  transaction_commit ();
}

#ifdef FCNTL_NONBLOCK

static int
get_flags (int fd)
{
  int scr;
  STD_UINT_SYSTEM_CALL (syscall_fcntl_GETFL, scr, (UX_fcntl (fd, F_GETFL, 0)));
  return (scr);
}

static void
set_flags (int fd, int flags)
{
  STD_VOID_SYSTEM_CALL (syscall_fcntl_SETFL, (UX_fcntl (fd, F_SETFL, flags)));
}

int
OS_channel_nonblocking_p (Tchannel channel)
{
  return (CHANNEL_NONBLOCKING (channel));
}

void
OS_channel_nonblocking (Tchannel channel)
{
  int fd = (CHANNEL_DESCRIPTOR (channel));
  int flags = (get_flags (fd));
  if ((flags & FCNTL_NONBLOCK) == 0)
    set_flags (fd, (flags | FCNTL_NONBLOCK));
#ifdef _ULTRIX
  {
    /* This is needed for non-POSIX-ified pseudo-terminal driver.  fcntl
       sets driver's FIONBIO flag for FNDELAY, but not FNBLOCK.  Note that
       driver will return EWOULDBLOCK, rather than EAGAIN. */
    int true = 1;
    ioctl (fd, FIONBIO, (&true));
  }
#endif
  (CHANNEL_NONBLOCKING (channel)) = 1;
}

void
OS_channel_blocking (Tchannel channel)
{
  int fd = (CHANNEL_DESCRIPTOR (channel));
  int flags = (get_flags (fd));
  if ((flags & FCNTL_NONBLOCK) != 0)
    set_flags (fd, (flags &~ FCNTL_NONBLOCK));
#ifdef _ULTRIX
  {
    /* This is needed for non-POSIX-ified pseudo-terminal driver.  fcntl
       sets driver's FIONBIO flag for FNDELAY, but not FNBLOCK. */
    int false = 0;
    ioctl(fd,FIONBIO,&false);
  }
#endif
  (CHANNEL_NONBLOCKING (channel)) = 0;
}

#else /* not FCNTL_NONBLOCK */

int
OS_channel_nonblocking_p (Tchannel channel)
{
  return (-1);
}

void
OS_channel_nonblocking (Tchannel channel)
{
  error_unimplemented_primitive ();
}

void
OS_channel_blocking (Tchannel channel)
{
}

#endif /* FCNTL_NONBLOCK */

#ifdef HAVE_POLL

const int OS_have_select_p = 1;

struct select_registry_s
{
  unsigned int length;
  unsigned int n_fds;
  struct pollfd * entries;
};

#define MIN_SR_LENGTH 4
#define SR_BYTES(length) ((sizeof (struct pollfd)) * (length))

#define SR_LENGTH(r) ((r) -> length)
#define SR_N_FDS(r) ((r) -> n_fds)
#define SR_ENTRIES(r) ((r) -> entries)
#define SR_ENTRY(r, i) ((SR_ENTRIES (r)) + (i))

#define DECODE_MODE(mode)						\
(((((mode) & SELECT_MODE_READ) != 0) ? POLLIN : 0)			\
 | ((((mode) & SELECT_MODE_WRITE) != 0) ? POLLOUT : 0))

#define ENCODE_MODE(revents)						\
(((((revents) & POLLIN) != 0) ? SELECT_MODE_READ : 0)			\
 | ((((revents) & POLLOUT) != 0) ? SELECT_MODE_WRITE : 0)		\
 | ((((revents) & POLLERR) != 0) ? SELECT_MODE_ERROR : 0)		\
 | ((((revents) & POLLHUP) != 0) ? SELECT_MODE_HUP : 0))

select_registry_t
OS_allocate_select_registry (void)
{
  struct select_registry_s * r
    = (UX_malloc (sizeof (struct select_registry_s)));
  (SR_LENGTH (r)) = MIN_SR_LENGTH;
  (SR_N_FDS (r)) = 0;
  (SR_ENTRIES (r)) = (UX_malloc (SR_BYTES (MIN_SR_LENGTH)));
  return (r);
}

void
OS_deallocate_select_registry (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  UX_free (SR_ENTRIES (r));
  UX_free (r);
}

void
OS_add_to_select_registry (select_registry_t registry,
			   int fd,
			   unsigned int mode)
{
  struct select_registry_s * r = registry;
  unsigned int i = 0;
  while (i < (SR_N_FDS (r)))
    {
      if (((SR_ENTRY (r, i)) -> fd) == fd)
	{
	  ((SR_ENTRY (r, i)) -> events) |= (DECODE_MODE (mode));
	  return;
	}
      i += 1;
    }
  if (i == (SR_LENGTH (r)))
    {
      unsigned int length = ((SR_LENGTH (r)) * 2);
      (SR_ENTRIES (r)) = (UX_realloc ((SR_ENTRIES (r)), (SR_BYTES (length))));
      (SR_LENGTH (r)) = length;
    }
  ((SR_ENTRY (r, i)) -> fd) = fd;
  ((SR_ENTRY (r, i)) -> events) = (DECODE_MODE (mode));
  (SR_N_FDS (r)) += 1;
}

void
OS_remove_from_select_registry (select_registry_t registry,
				int fd,
				unsigned int mode)
{
  struct select_registry_s * r = registry;
  unsigned int i = 0;
  while (1)
    {
      if (i == (SR_N_FDS (r)))
	return;
      if (((SR_ENTRY (r, i)) -> fd) == fd)
	{
	  ((SR_ENTRY (r, i)) -> events) &=~ (DECODE_MODE (mode));
	  if (((SR_ENTRY (r, i)) -> events) == 0)
	    break;
	  return;
	}
      i += 1;
    }
  (SR_N_FDS (r)) -= 1;
  while (i < (SR_N_FDS (r)))
    {
      (* (SR_ENTRY (r, i))) = (* (SR_ENTRY (r, (i + 1))));
      i += 1;
    }
  if ((i < ((SR_LENGTH (r)) / 2))
      && ((SR_LENGTH (r)) > MIN_SR_LENGTH))
    {
      unsigned int length = ((SR_LENGTH (r)) / 2);
      (SR_ENTRIES (r)) = (UX_realloc ((SR_ENTRIES (r)), (SR_BYTES (length))));
      (SR_LENGTH (r)) = length;
    }
}

unsigned int
OS_select_registry_length (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  return (SR_N_FDS (r));
}

void
OS_select_registry_result (select_registry_t registry,
			   unsigned int index,
			   int * fd_r,
			   unsigned int * mode_r)
{
  struct select_registry_s * r = registry;
  (*fd_r) = ((SR_ENTRY (r, index)) -> fd);
  (*mode_r) = (ENCODE_MODE ((SR_ENTRY (r, index)) -> revents));
}

int
OS_test_select_registry (select_registry_t registry, int blockp)
{
  struct select_registry_s * r = registry;
  while (1)
    {
      int nfds
	= (poll ((SR_ENTRIES (r)),
		 (SR_N_FDS (r)),
		 (blockp ? INFTIM : 0)));
      if (nfds >= 0)
	return (nfds);
      if (errno != EINTR)
	error_system_call (errno, syscall_select);
      if (OS_process_any_status_change ())
	return (SELECT_PROCESS_STATUS_CHANGE);
      if (pending_interrupts_p ())
	return (SELECT_INTERRUPT);
    }
}

int
OS_test_select_descriptor (int fd, int blockp, unsigned int mode)
{
  struct pollfd pfds [1];
  ((pfds [0]) . fd) = fd;
  ((pfds [0]) . events) = (DECODE_MODE (mode));
  while (1)
    {
      int nfds = (poll (pfds, 1, (blockp ? INFTIM : 0)));
      if (nfds > 0)
	return (ENCODE_MODE ((pfds [0]) . revents));
      if (nfds == 0)
	return (0);
      if (errno != EINTR)
	error_system_call (errno, syscall_select);
      if (OS_process_any_status_change ())
	return (SELECT_PROCESS_STATUS_CHANGE);
      if (pending_interrupts_p ())
	return (SELECT_INTERRUPT);
    }
}

#else /* not HAVE_POLL */

#ifdef HAVE_SELECT
const int OS_have_select_p = 1;
#else
const int OS_have_select_p = 0;
#endif

struct select_registry_s
{
  SELECT_TYPE qreaders;
  SELECT_TYPE qwriters;
  SELECT_TYPE rreaders;
  SELECT_TYPE rwriters;
  unsigned int n_fds;
};

#define SR_QREADERS(r) (& ((r) -> qreaders))
#define SR_QWRITERS(r) (& ((r) -> qwriters))
#define SR_RREADERS(r) (& ((r) -> rreaders))
#define SR_RWRITERS(r) (& ((r) -> rwriters))
#define SR_N_FDS(r) ((r) -> n_fds)

#define SR_FD_ISSET(fd, r)						\
((FD_ISSET ((fd), (SR_QREADERS (r))))					\
 || (FD_ISSET ((fd), (SR_QWRITERS (r)))))

#define SR_RMODE(r, fd)							\
(((FD_ISSET ((fd), (SR_RREADERS (r)))) ? SELECT_MODE_READ : 0)		\
 | ((FD_ISSET ((fd), (SR_RWRITERS (r)))) ? SELECT_MODE_WRITE : 0))

select_registry_t
OS_allocate_select_registry (void)
{
  struct select_registry_s * r
    = (UX_malloc (sizeof (struct select_registry_s)));
  FD_ZERO (SR_QREADERS (r));
  FD_ZERO (SR_QWRITERS (r));
  FD_ZERO (SR_RREADERS (r));
  FD_ZERO (SR_RWRITERS (r));
  (SR_N_FDS (r)) = 0;
  return (r);
}

void
OS_deallocate_select_registry (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  UX_free (r);
}

void
OS_add_to_select_registry (select_registry_t registry,
			   int fd,
			   unsigned int mode)
{
  struct select_registry_s * r = registry;
  int was_set = (SR_FD_ISSET (fd, r));
  if ((mode & SELECT_MODE_READ) != 0)
    FD_SET (fd, (SR_QREADERS (r)));
  if ((mode & SELECT_MODE_WRITE) != 0)
    FD_SET (fd, (SR_QWRITERS (r)));
  if ((!was_set) && (SR_FD_ISSET (fd, r)))
    (SR_N_FDS (r)) += 1;
}

void
OS_remove_from_select_registry (select_registry_t registry,
				int fd,
				unsigned int mode)
{
  struct select_registry_s * r = registry;
  int was_set = (SR_FD_ISSET (fd, r));
  if ((mode & SELECT_MODE_READ) != 0)
    FD_CLR (fd, (SR_QREADERS (r)));
  if ((mode & SELECT_MODE_WRITE) != 0)
    FD_CLR (fd, (SR_QWRITERS (r)));
  if (was_set && (!SR_FD_ISSET (fd, r)))
    (SR_N_FDS (r)) -= 1;
}

unsigned int
OS_select_registry_length (select_registry_t registry)
{
  struct select_registry_s * r = registry;
  return (SR_N_FDS (r));
}

void
OS_select_registry_result (select_registry_t registry,
			   unsigned int index,
			   int * fd_r,
			   unsigned int * mode_r)
{
  struct select_registry_s * r = registry;
  unsigned int i = 0;
  int fd;

  for (fd = 0; (fd < FD_SETSIZE); fd += 1)
    {
      if (SR_FD_ISSET (fd, r))
	{
	  if (i < index)
	    i += 1;
	  else
	    {
	      (*fd_r) = fd;
	      (*mode_r) = (SR_RMODE (r, fd));
	      return;
	    }
	}
    }
}

int
OS_test_select_registry (select_registry_t registry, int blockp)
{
#ifdef HAVE_SELECT
  struct select_registry_s * r = registry;
  while (1)
    {
      int nfds;

      (* (SR_RREADERS (r))) = (* (SR_QREADERS (r)));
      (* (SR_RWRITERS (r))) = (* (SR_QWRITERS (r)));
      INTERRUPTABLE_EXTENT
	(nfds,
	 ((OS_process_any_status_change ())
	  ? ((errno = EINTR), (-1))
	  : (UX_select (FD_SETSIZE,
			(SR_RREADERS (r)),
			(SR_RWRITERS (r)),
			0,
			(blockp ? 0 : (&zero_timeout))))));
      if (nfds >= 0)
	return (nfds);
      if (errno != EINTR)
	error_system_call (errno, syscall_select);
      if (OS_process_any_status_change ())
	return (SELECT_PROCESS_STATUS_CHANGE);
      if (pending_interrupts_p ())
	return (SELECT_INTERRUPT);
    }
#else
  error_system_call (ENOSYS, syscall_select);
  return (1);
#endif
}

int
OS_test_select_descriptor (int fd, int blockp, unsigned int mode)
{
#ifdef HAVE_SELECT
  while (1)
    {
      SELECT_TYPE readable;
      SELECT_TYPE writeable;
      int nfds;

      FD_ZERO (&readable);
      if ((mode & SELECT_MODE_READ) != 0)
	FD_SET (fd, (&readable));

      FD_ZERO (&writeable);
      if ((mode & SELECT_MODE_WRITE) != 0)
	FD_SET (fd, (&writeable));

      INTERRUPTABLE_EXTENT
	(nfds,
	 ((OS_process_any_status_change ())
	  ? ((errno = EINTR), (-1))
	  : (UX_select ((fd + 1),
			(&readable),
			(&writeable),
			0,
			(blockp ? 0 : (&zero_timeout))))));
      if (nfds > 0)
	return
	  (((FD_ISSET (fd, (&readable))) ? SELECT_MODE_READ : 0)
	   | ((FD_ISSET (fd, (&writeable))) ? SELECT_MODE_WRITE : 0));
      if (nfds == 0)
	return (0);
      if (errno != EINTR)
	error_system_call (errno, syscall_select);
      if (OS_process_any_status_change ())
	return (SELECT_PROCESS_STATUS_CHANGE);
      if (pending_interrupts_p ())
	return (SELECT_INTERRUPT);
    }
#else
  error_system_call (ENOSYS, syscall_select);
  return (1);
#endif
}

#endif /* not HAVE_POLL */

#ifdef HAVE_POLL

/* poll(2) */

unsigned int
UX_select_registry_size (void)
{
  return ((sizeof (struct pollfd)) * OS_channel_table_size);
}

unsigned int
UX_select_registry_lub (void)
{
  return (OS_channel_table_size);
}

void
UX_select_registry_clear_all (void * fds)
{
  struct pollfd * scan = fds;
  struct pollfd * end = (scan + OS_channel_table_size);
  for (; (scan < end); scan += 1)
    {
      (scan -> fd) = (-1);
      (scan -> events) = 0;
    }
}

void
UX_select_registry_set (void * fds, unsigned int fd)
{
  struct pollfd * scan = fds;
  struct pollfd * end = (scan + OS_channel_table_size);
  for (; (scan < end); scan += 1)
    if (((scan -> fd) == (-1)) || ((scan -> fd) == fd))
      {
	(scan -> fd) = fd;
	(scan -> events) = POLLIN;
	break;
      }
}

void
UX_select_registry_clear (void * fds, unsigned int fd)
{
  struct pollfd * scan = fds;
  struct pollfd * end = (scan + OS_channel_table_size);
  for (; (scan < end); scan += 1)
    if ((scan -> fd) == fd)
      {
	/* Shift any subsequent entries down.  */
	for (; (((scan + 1) < end) && ((scan -> fd) != (-1))); scan += 1)
	  (*scan) = (* (scan + 1));
	(scan -> fd) = (-1);
	(scan -> events) = 0;
	return;
      }
}

int
UX_select_registry_is_set (void * fds, unsigned int fd)
{
  struct pollfd * scan = fds;
  struct pollfd * end = (scan + OS_channel_table_size);
  for (; (scan < end); scan += 1)
    if ((scan -> fd) == fd)
      return (1);
  return (0);
}

static unsigned int
count_select_registry_entries (struct pollfd * pfds)
{
  struct pollfd * end = (pfds + OS_channel_table_size);
  struct pollfd * scan;
  for (scan = pfds; (scan < end); scan += 1)
    if ((scan -> fd) == (-1))
      break;
  return (scan - pfds);
}

enum select_input
UX_select_registry_test (void * input_fds,
			 int blockp,
			 unsigned int * output_fds,
			 unsigned int * output_nfds)
{
  struct pollfd * pfds = input_fds;
  unsigned int n_pfds = (count_select_registry_entries (pfds));
  while (1)
    {
      int nfds = (poll (pfds, n_pfds, (blockp ? INFTIM : 0)));
      if (nfds > 0)
	{
	  if (output_nfds != 0)
	    (*output_nfds) = nfds;
	  if (output_fds != 0)
	    {
	      struct pollfd * scan = pfds;
	      struct pollfd * end = (scan + n_pfds);
	      while (scan < end)
		{
		  if (((scan -> fd) != (-1)) && ((scan -> revents) != 0))
		    {
		      (*output_fds++) = (scan -> fd);
		      if ((--nfds) == 0)
			break;
		    }
		  scan += 1;
		}
	    }
	  return (select_input_argument);
	}
      else if (nfds == 0)
	{
	  if (!blockp)
	    return (select_input_none);
	}
      else if (! ((errno == EINTR) || (errno == EAGAIN)))
	error_system_call (errno, syscall_select);
      else if (OS_process_any_status_change ())
	return (select_input_process_status);
      if (pending_interrupts_p ())
	return (select_input_interrupt);
    }
}

enum select_input
UX_select_descriptor (unsigned int fd, int blockp)
{
  struct pollfd pfds [1];
  int nfds;

  ((pfds [0]) . fd) = fd;
  ((pfds [0]) . events) = POLLIN;
  while (1)
    {
      nfds = (poll (pfds, 1, (blockp ? INFTIM : 0)));
      if (nfds > 0)
	return (select_input_argument);
      else if (nfds == 0)
	{
	  if (!blockp)
	    return (select_input_none);
	}
      else if (errno != EINTR)
	error_system_call (errno, syscall_select);
      else if (OS_process_any_status_change ())
	return (select_input_process_status);
      if (pending_interrupts_p ())
	return (select_input_interrupt);
    }
}

enum select_input
UX_select_input (int fd, int blockp)
{
  return (UX_select_descriptor (fd, blockp));
}

#else /* not HAVE_POLL */

/* select(2) */

unsigned int
UX_select_registry_size (void)
{
  return (sizeof (SELECT_TYPE));
}

unsigned int
UX_select_registry_lub (void)
{
  return (FD_SETSIZE);
}

void
UX_select_registry_clear_all (void * fds)
{
  FD_ZERO ((SELECT_TYPE *) fds);
}

void
UX_select_registry_set (void * fds, unsigned int fd)
{
  FD_SET (fd, ((SELECT_TYPE *) fds));
}

void
UX_select_registry_clear (void * fds, unsigned int fd)
{
  FD_CLR (fd, ((SELECT_TYPE *) fds));
}

int
UX_select_registry_is_set (void * fds, unsigned int fd)
{
  return (FD_ISSET (fd, ((SELECT_TYPE *) fds)));
}

enum select_input
UX_select_registry_test (void * input_fds,
			 int blockp,
			 unsigned int * output_fds,
			 unsigned int * output_nfds)
{
#ifdef HAVE_SELECT
  while (1)
    {
      SELECT_TYPE readable;
      int nfds;

      readable = (* ((SELECT_TYPE *) input_fds));
      INTERRUPTABLE_EXTENT
	(nfds,
	 ((OS_process_any_status_change ())
	  ? ((errno = EINTR), (-1))
	  : (UX_select (FD_SETSIZE,
			(&readable),
			((SELECT_TYPE *) 0),
			((SELECT_TYPE *) 0),
			(blockp
			 ? ((struct timeval *) 0)
			 : (&zero_timeout))))));
      if (nfds > 0)
	{
	  unsigned int i = 0;
	  if (output_nfds != 0)
	    (*output_nfds) = nfds;
	  if (output_fds != 0)
	    while (1)
	      {
		if (FD_ISSET (i, (&readable)))
		  {
		    (*output_fds++) = i;
		    if ((--nfds) == 0)
		      break;
		  }
		i += 1;
	      }
	  return (select_input_argument);
	}
      else if (nfds == 0)
	{
	  if (!blockp)
	    return (select_input_none);
	}
      else if (errno != EINTR)
	error_system_call (errno, syscall_select);
      else if (OS_process_any_status_change ())
	return (select_input_process_status);
      if (pending_interrupts_p ())
	return (select_input_interrupt);
    }
#else
  error_system_call (ENOSYS, syscall_select);
  return (select_input_argument);
#endif
}

enum select_input
UX_select_descriptor (unsigned int fd, int blockp)
{
#ifdef HAVE_SELECT
  SELECT_TYPE readable;

  FD_ZERO (&readable);
  FD_SET (fd, (&readable));
  return (UX_select_registry_test ((&readable), blockp, 0, 0));
#else
  error_system_call (ENOSYS, syscall_select);
  return (select_input_argument);
#endif
}

enum select_input
UX_select_input (int fd, int blockp)
{
  SELECT_TYPE readable;
  unsigned int fds [FD_SETSIZE];
  unsigned int nfds;

  readable = input_descriptors;
  FD_SET (fd, (&readable));
  {
    enum select_input s =
      (UX_select_registry_test ((&readable), blockp, fds, (&nfds)));
    if (s != select_input_argument)
      return (s);
  }
  {
    unsigned int * scan = fds;
    unsigned int * end = (scan + nfds);
    while (scan < end)
      if ((*scan++) == fd)
	return (select_input_argument);
  }
  return (select_input_other);
}

#endif /* not HAVE_POLL */
