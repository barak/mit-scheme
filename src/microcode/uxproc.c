/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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
#include "uxproc.h"
#include "uxio.h"
#include "osterm.h"
#include "ostop.h"

#ifndef HAVE_DUP2
#include "error: can't hack subprocess I/O without dup2() or equivalent"
#endif

extern void (*subprocess_death_hook) (pid_t pid, int * status);
extern void (*stop_signal_hook) (int signo);
extern void stop_signal_default (int signo);
extern int OS_ctty_fd (void);
extern void UX_initialize_child_signals (void);

static void subprocess_death (pid_t pid, int * status);
static void stop_signal_handler (int signo);
static void give_terminal_to (Tprocess process);
static void get_terminal_back (void);
static void process_wait (Tprocess process);
static int child_setup_tty (int fd);

Tprocess OS_process_table_size;
struct process * process_table;
enum process_jc_status scheme_jc_status;

static int scheme_ctty_fd;
static Tprocess foreground_child_process;

static long process_tick;
static long sync_tick;

#define NEW_RAW_STATUS(process, status, reason) do			\
{									\
  (PROCESS_RAW_STATUS (process)) = (status);				\
  (PROCESS_RAW_REASON (process)) = (reason);				\
  (PROCESS_TICK (process)) = (++process_tick);				\
} while (0)

#define PROCESS_STATUS_SYNC(process) do					\
{									\
  (PROCESS_STATUS (process)) = (PROCESS_RAW_STATUS (process));		\
  (PROCESS_REASON (process)) = (PROCESS_RAW_REASON (process));		\
  (PROCESS_SYNC_TICK (process)) = (PROCESS_TICK (process));		\
} while (0)

/* This macro should only be used when
   (scheme_jc_status == process_jc_status_jc). */
#define SCHEME_IN_FOREGROUND()						\
  ((UX_tcgetpgrp (scheme_ctty_fd)) == (UX_getpgrp ()))

#ifdef HAVE_POSIX_SIGNALS

static void
restore_signal_mask (void * environment)
{
  UX_sigprocmask (SIG_SETMASK, ((sigset_t *) environment), 0);
}

static void
block_sigchld (void)
{
  sigset_t * outside = (dstack_alloc (sizeof (sigset_t)));
  sigset_t sigchld;
  UX_sigemptyset (&sigchld);
  UX_sigaddset ((&sigchld), SIGCHLD);
  UX_sigprocmask (SIG_BLOCK, (&sigchld), outside);
  transaction_record_action (tat_always, restore_signal_mask, outside);
}

static void
block_jc_signals (void)
{
  sigset_t * outside = (dstack_alloc (sizeof (sigset_t)));
  sigset_t jc_signals;
  UX_sigemptyset (&jc_signals);
  UX_sigaddset ((&jc_signals), SIGCHLD);
  UX_sigaddset ((&jc_signals), SIGTTOU);
  UX_sigaddset ((&jc_signals), SIGTTIN);
  UX_sigaddset ((&jc_signals), SIGTSTP);
  UX_sigaddset ((&jc_signals), SIGSTOP);
  UX_sigprocmask (SIG_BLOCK, (&jc_signals), outside);
  transaction_record_action (tat_always, restore_signal_mask, outside);
}

static sigset_t grabbed_signal_mask;

static void
grab_signal_mask (void)
{
  UX_sigprocmask (SIG_BLOCK, 0, (&grabbed_signal_mask));
}

#else /* not HAVE_POSIX_SIGNALS */

#ifdef HAVE_SIGHOLD

static void
release_sigchld (void * environment)
{
  UX_sigrelse (SIGCHLD);
}

static void
block_sigchld (void)
{
  UX_sighold (SIGCHLD);
  transaction_record_action (tat_always, release_sigchld, 0);
}

#else /* not HAVE_SIGHOLD */

#define block_sigchld()

#endif /* not HAVE_SIGHOLD */

#define block_jc_signals block_sigchld
#define grab_signal_mask()

#endif /* not HAVE_POSIX_SIGNALS */

void
UX_initialize_processes (void)
{
  OS_process_table_size = 64;
  process_table
    = (UX_malloc (OS_process_table_size * (sizeof (struct process))));
  if (process_table == 0)
    {
      fprintf (stderr, "\nUnable to allocate process table.\n");
      fflush (stderr);
      termination_init_error ();
    }
  {
    Tprocess process;
    for (process = 0; (process < OS_process_table_size); process += 1)
      OS_process_deallocate (process);
  }
  scheme_ctty_fd = (OS_ctty_fd ());
  scheme_jc_status =
    ((scheme_ctty_fd < 0)     ? process_jc_status_no_ctty
     : (UX_SC_JOB_CONTROL ()) ? process_jc_status_jc
     : /*else*/			process_jc_status_no_jc);
  foreground_child_process = NO_PROCESS;
  subprocess_death_hook = subprocess_death;
  stop_signal_hook = stop_signal_handler;
  process_tick = 0;
  sync_tick = 0;
}

void
UX_reset_processes (void)
{
  UX_free (process_table);
  process_table = 0;
  OS_process_table_size = 0;
}

static void
process_allocate_abort (void * environment)
{
  Tprocess process = (* ((Tprocess *) environment));
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_stopped:
    case process_status_running:
      UX_kill ((PROCESS_ID (process)), SIGKILL);
      break;
    default:
      break;
    }
  OS_process_deallocate (process);
}

static Tprocess
process_allocate (void)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_RAW_STATUS (process)) == process_status_free)
      {
	(PROCESS_RAW_STATUS (process)) = process_status_allocated;
	break;
      }
  if (process == OS_process_table_size)
    {
      size_t old_size = OS_process_table_size;
      size_t new_size = ((old_size * 5) / 4);
      struct process * new_table
	= (UX_realloc (process_table, (new_size * (sizeof (struct process)))));
      if (new_table == 0)
	{
	  error_out_of_processes ();
	  return (NO_PROCESS);
	}
      OS_process_table_size = new_size;
      process_table = new_table;
      for (process = old_size; (process < new_size); process += 1)
	OS_process_deallocate (process);
      process = old_size;
    }
  {
    Tprocess * pp = (dstack_alloc (sizeof (Tprocess)));
    (*pp) = process;
    transaction_record_action (tat_abort, process_allocate_abort, pp);
  }
  return (process);
}

void
OS_process_deallocate (Tprocess process)
{
  (PROCESS_ID (process)) = 0;
  (PROCESS_RAW_STATUS (process)) = process_status_free;
}

Tprocess
OS_make_subprocess (const char * filename,
		    const char ** argv,
		    const char ** volatile envp,
		    const char * working_directory,
		    enum process_ctty_type ctty_type,
		    char * ctty_name,
		    enum process_channel_type channel_in_type,
		    Tchannel channel_in,
		    enum process_channel_type channel_out_type,
		    Tchannel channel_out,
		    enum process_channel_type channel_err_type,
		    Tchannel channel_err)
{
  pid_t child_pid;
  volatile Tprocess child;
  volatile enum process_jc_status child_jc_status = process_jc_status_no_ctty;

  if (envp == 0)
    envp = ((const char **) environ);
  switch (ctty_type)
    {
    case process_ctty_type_none:
      child_jc_status = process_jc_status_no_ctty;
      break;
    case process_ctty_type_explicit:
      child_jc_status = process_jc_status_unrelated;
      break;
    case process_ctty_type_inherit_bg:
    case process_ctty_type_inherit_fg:
      child_jc_status = scheme_jc_status;
      break;
    }

  transaction_begin ();
  child = (process_allocate ());
  grab_signal_mask ();
  if (ctty_type == process_ctty_type_inherit_fg)
    block_jc_signals ();
  else
    block_sigchld ();
  STD_UINT_SYSTEM_CALL (syscall_vfork, child_pid, (UX_vfork ()));

  if (child_pid > 0)
    {
      /* In the parent process. */
      (PROCESS_ID (child)) = child_pid;
      (PROCESS_JC_STATUS (child)) = child_jc_status;
      (PROCESS_RAW_STATUS (child)) = process_status_running;
      (PROCESS_RAW_REASON (child)) = 0;
      (PROCESS_TICK (child)) = process_tick;
      PROCESS_STATUS_SYNC (child);

      /* If we are doing job control for the child, make sure the child
	 is in its own progress group before returning, so that we can
	 set the ctty's process group and send job control signals to
	 the child.  */
      if (child_jc_status == process_jc_status_jc)
	/* There is a race condition here: see the RATIONALE section of
    <http://pubs.opengroup.org/onlinepubs/9699919799/functions/setpgid.html>
	   (POSIX.1-2008) for details.	The gist is that neither parent
	   nor child can rely on the other to set the child's process
	   group, so both try it.  It's OK for either the parent or the
	   child to lose the race: after calling setpgid, each one
	   cares only that the child have its own process group, which
	   will be the case irrespective of who wins the race.	If the
	   parent loses the race (and the child has already exec'd or
	   exited), setpgid here may barf, and there are many ways that
	   the parent can lose the race, so we just ignore any failure
	   here under the (mildly bogus) assumption that failure means
	   losing the race rather than manifesting a bug.  */
	(void) UX_setpgid (child_pid, child_pid);

      if (ctty_type == process_ctty_type_inherit_fg)
	{
	  give_terminal_to (child);
	  process_wait (child);
	}

      transaction_commit ();
      return (child);
    }

  /* In the child process -- if any errors occur, just exit. */
  child_pid = (UX_getpid ());
  /* Don't do `transaction_commit ()' here.  Because we used `vfork'
     to spawn the child, the side-effects that are performed by
     `transaction_commit' will occur in the parent as well. */
  if ((working_directory != 0) && ((UX_chdir (working_directory)) < 0))
    goto kill_child;
  {
    int in_fd = (-1);
    int out_fd = (-1);
    int err_fd = (-1);

    if (channel_in_type == process_channel_type_explicit)
      in_fd = (CHANNEL_DESCRIPTOR (channel_in));
    if (channel_out_type == process_channel_type_explicit)
      out_fd = (CHANNEL_DESCRIPTOR (channel_out));
    if (channel_err_type == process_channel_type_explicit)
      err_fd = (CHANNEL_DESCRIPTOR (channel_err));

    if ((ctty_type == process_ctty_type_inherit_bg)
	|| (ctty_type == process_ctty_type_inherit_fg))
      {
	/* If the control terminal is inherited and job control is
	   available, force the child into a different process group. */
	if (child_jc_status == process_jc_status_jc)
	  {
	    if (((UX_setpgid (child_pid, child_pid)) < 0)
		|| ((ctty_type == process_ctty_type_inherit_fg)
		    && (SCHEME_IN_FOREGROUND ())
		    && ((UX_tcsetpgrp (scheme_ctty_fd, child_pid)) < 0)))
	      goto kill_child;
	  }
      }
    else
      {
	/* If the control terminal is not inherited, force the child
	   into a different session. */
	if ((UX_setsid ()) < 0)
	  goto kill_child;
	/* If the control terminal is explicit, open the given device
	   now so it becomes the control terminal. */
	if (ctty_type == process_ctty_type_explicit)
	  {
	    int fd = (UX_open (ctty_name, O_RDWR, 0));
	    if ((fd < 0)
#ifdef SLAVE_PTY_P
		|| ((SLAVE_PTY_P (ctty_name)) && (!UX_setup_slave_pty (fd)))
#endif
		|| (!isatty (fd))
#ifdef TIOCSCTTY
		|| ((UX_ioctl (fd, TIOCSCTTY, 0)) < 0)
#endif
		/* Tell the controlling terminal its process group. */
		|| (((UX_tcsetpgrp (fd, child_pid)) < 0) && (errno != ENOSYS))
		|| ((child_setup_tty (fd)) < 0))
	      goto kill_child;
	    /* Use CTTY for standard I/O if requested. */
	    if (channel_in_type == process_channel_type_ctty)
	      in_fd = fd;
	    if (channel_out_type == process_channel_type_ctty)
	      out_fd = fd;
	    if (channel_err_type == process_channel_type_ctty)
	      err_fd = fd;
	  }
      }

    /* Install the new standard I/O channels. */
    if ((in_fd >= 0) && (in_fd != STDIN_FILENO))
      {
	if ((out_fd == STDIN_FILENO) && ((out_fd = (UX_dup (out_fd))) < 0))
	  goto kill_child;
	if ((err_fd == STDIN_FILENO) && ((err_fd = (UX_dup (err_fd))) < 0))
	  goto kill_child;
	if ((UX_dup2 (in_fd, STDIN_FILENO)) < 0)
	  goto kill_child;
      }
    if ((out_fd >= 0) && (out_fd != STDOUT_FILENO))
      {
	if ((err_fd == STDOUT_FILENO) && ((err_fd = (UX_dup (err_fd))) < 0))
	  goto kill_child;
	if ((UX_dup2 (out_fd, STDOUT_FILENO)) < 0)
	  goto kill_child;
      }
    if ((err_fd >= 0) && (err_fd != STDERR_FILENO))
      {
	if ((UX_dup2 (err_fd, STDERR_FILENO)) < 0)
	  goto kill_child;
      }
  }

  /* Close all file descriptors not used by the child.  */
  /* FIXME: Handle EINTR?  */
  if (channel_in_type == process_channel_type_none)
    if ((UX_close (STDIN_FILENO)) < 0) goto kill_child;
  if (channel_out_type == process_channel_type_none)
    if ((UX_close (STDOUT_FILENO)) < 0) goto kill_child;
  if (channel_err_type == process_channel_type_none)
    if ((UX_close (STDERR_FILENO)) < 0) goto kill_child;
  /* Assumption: STDIN_FILENO = 0, STDOUT_FILENO = 1, STDERR_FILENO = 2.  */
  if ((UX_closefrom (3)) < 0) goto kill_child;

  /* Put the signal mask and handlers in a normal state.  */
  UX_initialize_child_signals ();

  /* Start the process. */
  (void) execve (filename, ((char * const *) argv), ((char * const *) envp));
 kill_child:
  _exit (1);
}

#define DEFUN_PROCESS_ACCESSOR(name, result_type, accessor)		\
result_type								\
name (Tprocess process)							\
{									\
  return (accessor (process));						\
}

DEFUN_PROCESS_ACCESSOR (OS_process_id, pid_t, PROCESS_ID)
DEFUN_PROCESS_ACCESSOR (OS_process_status, enum process_status, PROCESS_STATUS)
DEFUN_PROCESS_ACCESSOR (OS_process_reason, unsigned short, PROCESS_REASON)
DEFUN_PROCESS_ACCESSOR
  (OS_process_jc_status, enum process_jc_status, PROCESS_JC_STATUS)

int
OS_process_valid_p (Tprocess process)
{
  if (process > OS_process_table_size)
    return (0);
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_exited:
    case process_status_signalled:
    case process_status_stopped:
    case process_status_running:
      return (1);
    default:
      return (0);
    }
}

int
OS_process_continuable_p (Tprocess process)
{
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_stopped:
    case process_status_running:
      return (1);
    default:
      return (0);
    }
}

int
OS_process_foregroundable_p (Tprocess process)
{
  switch (PROCESS_JC_STATUS (process))
    {
    case process_jc_status_no_jc:
    case process_jc_status_jc:
      return (1);
    default:
      return (0);
    }
}

int
OS_process_status_sync (Tprocess process)
{
  transaction_begin ();
  block_sigchld ();
  {
    int result = ((PROCESS_TICK (process)) != (PROCESS_SYNC_TICK (process)));
    if (result) PROCESS_STATUS_SYNC (process);
    transaction_commit ();
    return (result);
  }
}

int
OS_process_status_sync_all (void)
{
  transaction_begin ();
  block_sigchld ();
  {
    int result = (process_tick != sync_tick);
    if (result) sync_tick = process_tick;
    transaction_commit ();
    return (result);
  }
}

int
OS_process_any_status_change (void)
{
  return (process_tick != sync_tick);
}

static void
process_send_signal (Tprocess process, int sig)
{
  STD_VOID_SYSTEM_CALL
    (syscall_kill,
     (UX_kill ((((PROCESS_JC_STATUS (process)) == process_jc_status_jc)
		? (- (PROCESS_ID (process)))
		: (PROCESS_ID (process))),
	       sig)));
}

void
OS_process_send_signal (Tprocess process, int sig)
{
  /* This is hairy because it is not OK to send a signal if the process
     has already terminated and we have already called wait(2) -- its
     pid will be recycled, and we might send a signal to some innocent
     bystander.  So we must guarantee that we won't call wait(2), by
     blocking SIGCHLD, and check whether the process is in such a state
     that we can safely signal it.  */
  transaction_begin ();
  block_sigchld ();
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_running:
    case process_status_stopped:
      process_send_signal (process, sig);
      break;

    case process_status_exited:
    case process_status_signalled:
      /* FIXME: This should signal an error with an argument -- namely,
	 with the process index, so that the runtime can do a reverse
	 lookup in the subprocess GC finalizer and put the appropriate
	 subprocess object in the Scheme error it signals.  */
      error_process_terminated ();

      /* The remaining cases shouldn't happen unless there is a bug in
	 the runtime; and if so, this is basically like a system call
	 error.  */
    default:
      error_in_system_call (syserr_no_such_process, syscall_kill);
    }
  transaction_commit ();
}

void
OS_process_kill (Tprocess process)
{
  OS_process_send_signal (process, SIGKILL);
}

void
OS_process_stop (Tprocess process)
{
  OS_process_send_signal (process, SIGTSTP);
}

void
OS_process_interrupt (Tprocess process)
{
  OS_process_send_signal (process, SIGINT);
}

void
OS_process_quit (Tprocess process)
{
  OS_process_send_signal (process, SIGQUIT);
}

void
OS_process_hangup (Tprocess process)
{
  OS_process_send_signal (process, SIGHUP);
}

void
OS_process_continue_background (Tprocess process)
{
  transaction_begin ();
  block_sigchld ();
  if ((PROCESS_RAW_STATUS (process)) == process_status_stopped)
    {
      NEW_RAW_STATUS (process, process_status_running, 0);
      process_send_signal (process, SIGCONT);
    }
  transaction_commit ();
}

void
OS_process_continue_foreground (Tprocess process)
{
  transaction_begin ();
  grab_signal_mask ();
  block_jc_signals ();
  give_terminal_to (process);
  if ((PROCESS_RAW_STATUS (process)) == process_status_stopped)
    {
      NEW_RAW_STATUS (process, process_status_running, 0);
      process_send_signal (process, SIGCONT);
    }
  process_wait (process);
  transaction_commit ();
}

void
OS_process_wait (Tprocess process)
{
  transaction_begin ();
  grab_signal_mask ();
  block_jc_signals ();
  process_wait (process);
  transaction_commit ();
}

static void
get_terminal_back_1 (void * environment)
{
  get_terminal_back ();
}

static void
give_terminal_to (Tprocess process)
{
  if (((PROCESS_JC_STATUS (process)) == process_jc_status_jc)
      && (SCHEME_IN_FOREGROUND ()))
    {
      transaction_record_action (tat_always, get_terminal_back_1, 0);
      foreground_child_process = process;
      OS_save_internal_state ();
      OS_restore_external_state ();
      while ((UX_tcsetpgrp (scheme_ctty_fd, (PROCESS_ID (process)))) < 0)
	if ((errno != ENOSYS) && (errno != EINTR))
	  error_system_call (errno, syscall_tcsetpgrp);
    }
}

static void
get_terminal_back (void)
{
  if (foreground_child_process != NO_PROCESS)
    {
      while ((UX_tcsetpgrp (scheme_ctty_fd, (UX_getpgrp ()))) < 0)
	if ((errno != ENOSYS) && (errno != EINTR))
	  /* We're in no position to signal an error here (inside a
	     transaction commit/abort action or a signal handler), so
	     just bail.  */
	  break;
      OS_save_external_state ();
      OS_restore_internal_state ();
      foreground_child_process = NO_PROCESS;
    }
}

static void
process_wait (Tprocess process)
{
#ifdef HAVE_POSIX_SIGNALS
  while (((PROCESS_RAW_STATUS (process)) == process_status_running)
	 && (! (pending_interrupts_p ())))
    UX_sigsuspend (&grabbed_signal_mask);
#else /* not HAVE_POSIX_SIGNALS */
  enum process_status status = (PROCESS_RAW_STATUS (process));
  while ((status == process_status_running)
	 && (! (pending_interrupts_p ())))
    {
      /* INTERRUPTABLE_EXTENT eliminates the interrupt window between
	 PROCESS_RAW_STATUS and `pause'. */
      int scr;
      INTERRUPTABLE_EXTENT
	(scr,
	 ((((status = (PROCESS_RAW_STATUS (process)))
	    == process_status_running)
	   && (! (pending_interrupts_p ())))
	  ? (UX_pause ())
	  : ((errno = EINTR), (-1))));
    }
#endif /* not HAVE_POSIX_SIGNALS */
}

static Tprocess
find_process (pid_t pid)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_ID (process)) == pid)
      return (process);
  return (NO_PROCESS);
}

static void
subprocess_death (pid_t pid, int * status)
{
  Tprocess process = (find_process (pid));
  if (process != NO_PROCESS)
    {
      if (WIFEXITED (*status))
	NEW_RAW_STATUS
	  (process, process_status_exited, (WEXITSTATUS (*status)));
      else if (WIFSTOPPED (*status))
	NEW_RAW_STATUS
	  (process, process_status_stopped, (WSTOPSIG (*status)));
      else if (WIFSIGNALED (*status))
	NEW_RAW_STATUS
	  (process, process_status_signalled, (WTERMSIG (*status)));
    }
}

static void
stop_signal_handler (int signo)
{
  /* If Scheme gets a stop signal while waiting on a foreground
     subprocess, it must grab the terminal back from the subprocess
     before stopping.  The caller guarantees that the job-control
     signals are blocked when this procedure is called. */
  get_terminal_back ();
  stop_signal_default (signo);
}

/* Set up the terminal at the other end of a pseudo-terminal that we
   will be controlling an inferior through. */

#ifdef HAVE_TERMIOS_H

/* POSIX.1 doesn't require (or even mention) these symbols, but we
   must disable them if they are present. */
#ifndef IUCLC
#  define IUCLC 0
#endif
#ifndef OLCUC
#  define OLCUC 0
#endif
#ifndef NLDLY
#  define NLDLY 0
#endif
#ifndef CRDLY
#  define CRDLY 0
#endif
#ifndef TABDLY
#  define TABDLY 0
#endif
#ifndef BSDLY
#  define BSDLY 0
#endif
#ifndef VTDLY
#  define VTDLY 0
#endif
#ifndef FFDLY
#  define FFDLY 0
#endif
#ifndef ONLCR
#  define ONLCR 0
#endif

static int
child_setup_tty (int fd)
{
  cc_t disabled_char = (UX_PC_VDISABLE (fd));
  struct termios s;
  if ((UX_tcgetattr (fd, (&s))) < 0)
    return (-1);
  (s . c_iflag) &=~ IUCLC;
  (s . c_oflag) |= OPOST;
  (s . c_oflag) &=~
    (OLCUC | ONLCR | NLDLY | CRDLY | TABDLY | BSDLY | VTDLY | FFDLY);
  (s . c_lflag) &=~ (ECHO | ECHOE | ECHOK | ECHONL);
  (s . c_lflag) |= (ICANON | ISIG);
  ((s . c_cc) [VEOF]) = '\004';
  ((s . c_cc) [VERASE]) = disabled_char;
  ((s . c_cc) [VKILL]) = disabled_char;
  cfsetispeed ((&s), B9600);
  cfsetospeed ((&s), B9600);
  return (UX_tcsetattr (fd, TCSADRAIN, (&s)));
}

#else /* not HAVE_TERMIOS_H */

#ifdef HAVE_TERMIO_H

static int
child_setup_tty (int fd)
{
  cc_t disabled_char = (UX_PC_VDISABLE (fd));
  struct termio s;
  if ((ioctl (fd, TCGETA, (&s))) < 0)
    return (-1);
  (s . c_iflag) &=~ IUCLC;
  (s . c_oflag) |= OPOST;
  (s . c_oflag) &=~
    (OLCUC | ONLCR | NLDLY | CRDLY | TABDLY | BSDLY | VTDLY | FFDLY);
  (s . c_lflag) &=~ (ECHO | ECHOE | ECHOK | ECHONL);
  (s . c_lflag) |= (ICANON | ISIG);
  ((s . c_cc) [VEOF]) = '\004';
  ((s . c_cc) [VERASE]) = disabled_char;
  ((s . c_cc) [VKILL]) = disabled_char;
  (s . c_cflag) = (((s . c_cflag) &~ CBAUD) | B9600);
#ifdef _AIX
  /* AIX enhanced edit loses NULs, so disable it.
     Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.
     This really serves a purpose only if running in an XTERM window
     or via TELNET or the like, but does no harm elsewhere.  */
  (s . c_line) = 0;
  (s . c_iflag) &=~ (ASCEDIT | IGNBRK | BRKINT);
  /* QUIT and INTR work better as signals, so disable character forms */
  (s . c_lflag) &=~ ISIG;
  ((s . c_cc) [VQUIT]) = disabled_char;
  ((s . c_cc) [VINTR]) = disabled_char;
  ((s . c_cc) [VEOL]) = disabled_char;
#endif /* _AIX */
  return (ioctl (fd, TCSETAW, (&s)));
}

#else /* not HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

static int
child_setup_tty (int fd)
{
  struct sgttyb s;
  if ((ioctl (fd, TIOCGETP, (&s))) < 0)
    return (-1);
  (s . sg_flags) &=~
    (ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
  return (ioctl (fd, TIOCSETN, (&s)));
}

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIO_H */
#endif /* HAVE_TERMIOS_H */
