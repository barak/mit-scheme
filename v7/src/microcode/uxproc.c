/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxproc.c,v 1.9 1991/03/11 23:43:12 cph Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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
#include "uxproc.h"
#include "uxio.h"
#include "osterm.h"

#ifndef HAVE_DUP2
#include "error: can't hack subprocess I/O without dup2() or equivalent"
#endif

extern char ** environ;
extern void EXFUN
  ((*subprocess_death_hook), (pid_t pid, wait_status_t * status));
extern void EXFUN ((*stop_signal_hook), (int signo));
extern void EXFUN (stop_signal_default, (int signo));
extern int EXFUN (OS_ctty_fd, (void));

static void EXFUN (subprocess_death, (pid_t pid, wait_status_t * status));
static void EXFUN (stop_signal_handler, (int signo));
static void EXFUN (give_terminal_to, (Tprocess process));
static void EXFUN (get_terminal_back, (void));
static void EXFUN (process_wait, (Tprocess process));
static int EXFUN (child_setup_tty, (int fd));

size_t OS_process_table_size;
struct process * process_table;
enum process_jc_status scheme_jc_status;

static int scheme_ctty_fd;
static Tprocess foreground_child_process;

static long process_tick;
static long sync_tick;

#define NEW_RAW_STATUS(process, status, reason)				\
{									\
  (PROCESS_RAW_STATUS (process)) = (status);				\
  (PROCESS_RAW_REASON (process)) = (reason);				\
  (PROCESS_TICK (process)) = (++process_tick);				\
}

#define PROCESS_STATUS_SYNC(process)					\
{									\
  (PROCESS_STATUS (process)) = (PROCESS_RAW_STATUS (process));		\
  (PROCESS_REASON (process)) = (PROCESS_RAW_REASON (process));		\
  (PROCESS_SYNC_TICK (process)) = (PROCESS_TICK (process));		\
}

/* This macro should only be used when
   (scheme_jc_status == process_jc_status_jc). */
#define SCHEME_IN_FOREGROUND()						\
  ((UX_tcgetpgrp (scheme_ctty_fd)) == (UX_getpgrp ()))

#ifdef HAVE_POSIX_SIGNALS

static void
DEFUN (restore_signal_mask, (environment), PTR environment)
{
  UX_sigprocmask (SIG_SETMASK, ((sigset_t *) environment), 0);
}

static void
DEFUN_VOID (block_sigchld)
{
  sigset_t * outside = (dstack_alloc (sizeof (sigset_t)));
  sigset_t sigchld;
  UX_sigemptyset (&sigchld);
  UX_sigaddset ((&sigchld), SIGCHLD);
  UX_sigprocmask (SIG_BLOCK, (&sigchld), outside);
  transaction_record_action (tat_always, restore_signal_mask, outside);
}

static void
DEFUN_VOID (block_jc_signals)
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
DEFUN_VOID (grab_signal_mask)
{
  UX_sigprocmask (SIG_BLOCK, 0, (&grabbed_signal_mask));
}

#else /* not HAVE_POSIX_SIGNALS */

#ifdef HAVE_SYSV3_SIGNALS

static void
DEFUN (release_sigchld, (environment), PTR environment)
{
  UX_sigrelse (SIGCHLD);
}

static void
DEFUN_VOID (block_sigchld)
{
  UX_sighold (SIGCHLD);
  transaction_record_action (tat_always, release_sigchld, 0);
}

#else /* not HAVE_SYSV3_SIGNALS */

#define block_sigchld()

#endif /* not HAVE_SYSV3_SIGNALS */

#define block_jc_signals block_sigchld
#define grab_signal_mask()

#endif /* not HAVE_POSIX_SIGNALS */

void
DEFUN_VOID (UX_initialize_processes)
{
  OS_process_table_size = (UX_SC_CHILD_MAX ());
  process_table =
    (UX_malloc (OS_process_table_size * (sizeof (struct process))));
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
    ((scheme_ctty_fd < 0)
     ? process_jc_status_no_ctty
     : (UX_SC_JOB_CONTROL ())
     ? process_jc_status_jc
     : process_jc_status_no_jc);
  foreground_child_process = NO_PROCESS;
  subprocess_death_hook = subprocess_death;
  stop_signal_hook = stop_signal_handler;
  process_tick = 0;
  sync_tick = 0;
}

void
DEFUN_VOID (UX_reset_processes)
{
  UX_free (process_table);
  process_table = 0;
  OS_process_table_size = 0;
}

static void
DEFUN (process_allocate_abort, (environment), PTR environment)
{
  Tprocess process = (* ((Tprocess *) environment));
  switch (PROCESS_RAW_STATUS (process))
    {
    case process_status_stopped:
    case process_status_running:
      UX_kill ((PROCESS_ID (process)), SIGKILL);
      break;
    }
  OS_process_deallocate (process);
}

static Tprocess
DEFUN_VOID (process_allocate)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_RAW_STATUS (process)) == process_status_free)
      {
	Tprocess * pp = (dstack_alloc (sizeof (Tprocess)));
	(*pp) = process;
	transaction_record_action (tat_abort, process_allocate_abort, pp);
	(PROCESS_RAW_STATUS (process)) = process_status_allocated;
	return (process);
      }
  error_out_of_processes ();
  return (NO_PROCESS);
}

void
DEFUN (OS_process_deallocate, (process), Tprocess process)
{
  (PROCESS_ID (process)) = 0;
  (PROCESS_RAW_STATUS (process)) = process_status_free;
}

Tprocess
DEFUN (OS_make_subprocess,
       (filename, argv, envp,
	ctty_type, ctty_name,
	channel_in_type, channel_in,
	channel_out_type, channel_out,
	channel_err_type, channel_err),
       CONST char * filename AND
       CONST char ** argv AND
       char ** envp AND
       enum process_ctty_type ctty_type AND
       char * ctty_name AND
       enum process_channel_type channel_in_type AND
       Tchannel channel_in AND
       enum process_channel_type channel_out_type AND
       Tchannel channel_out AND
       enum process_channel_type channel_err_type AND
       Tchannel channel_err)
{
  pid_t child_pid;
  Tprocess child;
  enum process_jc_status child_jc_status;

  if (envp == 0)
    envp = environ;
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

  /* Flush streams so that output won't be duplicated after the fork.  */
  fflush (stdout);
  fflush (stderr);

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
      if (child_jc_status == process_jc_status_jc)
	STD_VOID_SYSTEM_CALL
	  (syscall_setpgid, (UX_setpgid (child_pid, child_pid)));
      if (ctty_type == process_ctty_type_inherit_fg)
	{
	  give_terminal_to (child);
	  process_wait (child);
	}
      transaction_commit ();
      return (child);
    }

  /* In the child process -- if any errors occur, just exit. */
  /* Don't do `transaction_commit ()' here.  Because we used `vfork'
     to spawn the child, the side-effects that are performed by
     `transaction_commit' will occur in the parent as well. */
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
	    pid_t child_pid = (UX_getpid ());
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
		|| (! (isatty (fd)))
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
  {
    /* Close all other file descriptors. */
    int fd = 0;
    int open_max = (UX_SC_OPEN_MAX ());
    while (fd < open_max)
      {
	if ((fd == STDIN_FILENO)
	    ? (channel_in_type == process_channel_type_none)
	    : (fd == STDOUT_FILENO)
	    ? (channel_out_type == process_channel_type_none)
	    : (fd == STDERR_FILENO)
	    ? (channel_err_type == process_channel_type_none)
	    : 1)
	  UX_close (fd);
	fd += 1;
      }
  }

  /* Force the signal mask to be empty. */
#ifdef HAVE_POSIX_SIGNALS
  {
    sigset_t empty_mask;
    UX_sigemptyset (&empty_mask);
    UX_sigprocmask (SIG_SETMASK, (&empty_mask), 0);
  }
#else
#ifdef HAVE_SYSV3_SIGNALS
  /* We could do something more here, but it is hard to enumerate all
     the possible signals.  Instead, just release SIGCHLD, which we
     know was held above before the child was spawned. */
  UX_sigrelse (SIGCHLD);
#endif
#endif

  /* Start the process. */
  execve (filename, argv, envp);
 kill_child:
  _exit (1);
}

#define DEFUN_PROCESS_ACCESSOR(name, result_type, accessor)		\
result_type								\
DEFUN (name, (process), Tprocess process)				\
{									\
  return (accessor (process));						\
}

DEFUN_PROCESS_ACCESSOR (OS_process_id, pid_t, PROCESS_ID)
DEFUN_PROCESS_ACCESSOR (OS_process_status, enum process_status, PROCESS_STATUS)
DEFUN_PROCESS_ACCESSOR (OS_process_reason, unsigned short, PROCESS_REASON)
DEFUN_PROCESS_ACCESSOR
  (OS_process_jc_status, enum process_jc_status, PROCESS_JC_STATUS)

int
DEFUN (OS_process_valid_p, (process), Tprocess process)
{
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
DEFUN (OS_process_continuable_p, (process), Tprocess process)
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
DEFUN (OS_process_foregroundable_p, (process), Tprocess process)
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
DEFUN (OS_process_status_sync, (process), Tprocess process)
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
DEFUN_VOID (OS_process_status_sync_all)
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

void
DEFUN (OS_process_send_signal, (process, sig), Tprocess process AND int sig)
{
  STD_VOID_SYSTEM_CALL
    (syscall_kill, 
     (UX_kill ((((PROCESS_JC_STATUS (process)) == process_jc_status_jc)
		? (- (PROCESS_ID (process)))
		: (PROCESS_ID (process))),
	       sig)));
}

void
DEFUN (OS_process_kill, (process), Tprocess process)
{
  OS_process_send_signal (process, SIGKILL);
}

void
DEFUN (OS_process_stop, (process), Tprocess process)
{
  OS_process_send_signal (process, SIGTSTP);
}

void
DEFUN (OS_process_interrupt, (process), Tprocess process)
{
  OS_process_send_signal (process, SIGINT);
}

void
DEFUN (OS_process_quit, (process), Tprocess process)
{
  OS_process_send_signal (process, SIGQUIT);
}

void
DEFUN (OS_process_continue_background, (process), Tprocess process)
{
  transaction_begin ();
  block_sigchld ();
  if ((PROCESS_RAW_STATUS (process)) == process_status_stopped)
    {
      NEW_RAW_STATUS (process, process_status_running, 0);
      OS_process_send_signal (process, SIGCONT);
    }
  transaction_commit ();
}

void
DEFUN (OS_process_continue_foreground, (process), Tprocess process)
{
  transaction_begin ();
  grab_signal_mask ();
  block_jc_signals ();
  give_terminal_to (process);
  if ((PROCESS_RAW_STATUS (process)) == process_status_stopped)
    {
      NEW_RAW_STATUS (process, process_status_running, 0);
      OS_process_send_signal (process, SIGCONT); 
    }
  process_wait (process);
  transaction_commit ();
}

void
DEFUN (OS_process_wait, (process), Tprocess process)
{
  transaction_begin ();
  grab_signal_mask ();
  block_jc_signals ();
  process_wait (process);
  transaction_commit ();
}

static void
DEFUN (get_terminal_back_1, (environment), PTR environment)
{
  get_terminal_back ();
}

static void
DEFUN (give_terminal_to, (process), Tprocess process)
{
  if (((PROCESS_JC_STATUS (process)) == process_jc_status_jc)
      && (SCHEME_IN_FOREGROUND ()))
    {
      transaction_record_action (tat_always, get_terminal_back_1, 0);
      foreground_child_process = process;
      OS_save_internal_state ();
      OS_restore_external_state ();
      UX_tcsetpgrp (scheme_ctty_fd, (PROCESS_ID (process)));
    }
}

static void
DEFUN_VOID (get_terminal_back)
{
  if (foreground_child_process != NO_PROCESS)
    {
      UX_tcsetpgrp (scheme_ctty_fd, (UX_getpgrp ()));
      OS_save_external_state ();
      OS_restore_internal_state ();
      foreground_child_process = NO_PROCESS;
    }
}

static void
DEFUN (process_wait, (process), Tprocess process)
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
DEFUN (find_process, (pid), pid_t pid)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_ID (process)) == pid)
      return (process);
  return (NO_PROCESS);
}

static void
DEFUN (subprocess_death, (pid, status), pid_t pid AND wait_status_t * status)
{
  Tprocess process = (find_process (pid));
  if (process != NO_PROCESS)
    {
      if (WIFEXITED (*status))
	{
	  NEW_RAW_STATUS
	    (process, process_status_exited, (WEXITSTATUS (*status)));
	}
      else if (WIFSTOPPED (*status))
	{
	  NEW_RAW_STATUS
	    (process, process_status_stopped, (WSTOPSIG (*status)));
	}
      else if (WIFSIGNALED (*status))
	{
	  NEW_RAW_STATUS
	    (process, process_status_signalled, (WTERMSIG (*status)));
	}
    }
}

static void
DEFUN (stop_signal_handler, (signo), int signo)
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

#ifdef HAVE_TERMIOS

#ifndef IUCLC
/* POSIX.1 doesn't require (or even mention) these symbols, but we
   must disable them if they are present. */
#define IUCLC 0
#define OLCUC 0
#define ONLCR 0
#define NLDLY 0
#define CRDLY 0
#define TABDLY 0
#define BSDLY 0
#define VTDLY 0
#define FFDLY 0
#endif

static int
DEFUN (child_setup_tty, (fd), int fd)
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

#else /* not HAVE_TERMIOS */

#ifdef HAVE_TERMIO

static int
DEFUN (child_setup_tty, (fd), int fd)
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

#else /* not HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER

static int
DEFUN (child_setup_tty, (fd), int fd)
{
  struct sgttyb s;
  if ((ioctl (fd, TIOCGETP, (&s))) < 0)
    return (-1);
  (s . sg_flags) &=~
    (ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
  return (ioctl (fd, TIOCSETN, (&s)));
}

#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIO */
#endif /* HAVE_TERMIOS */
