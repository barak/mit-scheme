/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxproc.c,v 1.2 1990/07/28 18:57:00 jinx Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

static void EXFUN (deallocate_uncommitted_processes, (PTR ignore));
static void EXFUN (subprocess_death, (pid_t pid, wait_status_t * status));
static Tprocess EXFUN (find_process, (pid_t pid));
static int EXFUN (child_setup_tty, (Tchannel channel));

size_t OS_process_table_size;
struct process * process_table;

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
      (PROCESS_STATUS (process)) = process_status_free;
  }
  {
    extern void EXFUN
      ((*subprocess_death_hook), (pid_t pid, wait_status_t * status));
    subprocess_death_hook = subprocess_death;
  }
}

void
DEFUN_VOID (UX_reset_processes)
{
  UX_free (process_table);
  process_table = 0;
  OS_process_table_size = 0;
}

static Tprocess
DEFUN_VOID (process_allocate)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_STATUS (process)) == process_status_free)
      {
	transaction_record_action
	  (tat_abort, deallocate_uncommitted_processes, 0);
	(PROCESS_STATUS (process)) = process_status_allocated;
	return (process);
      }
  error_out_of_processes ();
  return (NO_PROCESS);
}

static void
DEFUN (deallocate_uncommitted_processes, (ignore), PTR ignore)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_STATUS (process)) == process_status_allocated)
      (PROCESS_STATUS (process)) = process_status_free;
}

void
DEFUN (OS_process_deallocate, (process), Tprocess process)
{
  (PROCESS_STATUS (process)) = process_status_free;
}

#define PROTECT_CHANNEL(channel)					\
{									\
  Tchannel * PROTECT_CHANNEL_cp = (dstack_alloc (sizeof (Tchannel)));	\
  (*PROTECT_CHANNEL_cp) = (channel);					\
  transaction_record_action						\
    (tat_abort, channel_close, PROTECT_CHANNEL_cp);			\
}

static void
DEFUN (channel_close, (cp), PTR cp)
{
  OS_channel_close (* ((Tchannel *) cp));
}

Tprocess
DEFUN (OS_make_subprocess, (filename, argv, envp, ctty_type),
       CONST char * filename AND
       CONST char ** argv AND
       char ** envp AND
       enum process_ctty_type ctty_type)
{
  Tchannel child_read;
  Tchannel child_write;
  Tchannel parent_read;
  Tchannel parent_write;
  pid_t child_pid;
#ifdef HAVE_PTYS
  CONST char * pty_name;
#endif
  Tprocess child;

  if ((ctty_type == ctty_type_none) || (ctty_type == ctty_type_inherited))
    /* Implement shell-like subprocess control later. */
    error_unimplemented_primitive ();

  transaction_begin ();
  child = (process_allocate ());

  if (ctty_type == ctty_type_pty)
    {
#ifdef HAVE_PTYS
      {
	CONST char * master_name;
	pty_name = (OS_open_pty_master ((&parent_read), (&master_name)));
      }
      if (pty_name != 0)
	{
	  PROTECT_CHANNEL (parent_read);
	  parent_write = parent_read;
	}
      else
#endif /* HAVE_PTYS */
	ctty_type = ctty_type_pipe;
    }
  if (ctty_type == ctty_type_pipe)
    {
      int pv [2];
      STD_VOID_SYSTEM_CALL ("pipe", (UX_pipe (pv)));
      MAKE_CHANNEL ((pv[0]), channel_type_pipe, child_read =);
      PROTECT_CHANNEL (child_read);
      MAKE_CHANNEL ((pv[1]), channel_type_pipe, parent_write =);
      PROTECT_CHANNEL (parent_write);
      STD_VOID_SYSTEM_CALL ("pipe", (UX_pipe (pv)));
      MAKE_CHANNEL ((pv[0]), channel_type_pipe, parent_read =);
      PROTECT_CHANNEL (parent_read);
      MAKE_CHANNEL ((pv[1]), channel_type_pipe, child_write =);
      PROTECT_CHANNEL (child_write);
    }

  /* Flush streams so that i/o won't be duplicated after the fork */
  fflush (stdin);
  fflush (stdout);
  fflush (stderr);

  STD_UINT_SYSTEM_CALL ("vfork", child_pid, (UX_vfork ()));
  if (child_pid > 0)
    {
      /* In the parent process. */
      (PROCESS_ID (child)) = child_pid;
      (PROCESS_INPUT (child)) = parent_write;
      (PROCESS_OUTPUT (child)) = parent_read;
      (PROCESS_STATUS (child)) = process_status_running;
      (PROCESS_CTTY_TYPE (child)) = ctty_type;
      (PROCESS_CHANGED (child)) = 0;
      (PROCESS_SYNCHRONOUS (child)) = 0;
      if (ctty_type == ctty_type_pipe)
	{
	  /* If either of these closes signals an error, ignore it. */
	  UX_close (CHANNEL_DESCRIPTOR (child_read));
	  MARK_CHANNEL_CLOSED (child_read);
	  UX_close (CHANNEL_DESCRIPTOR (child_write));
	  MARK_CHANNEL_CLOSED (child_write);
	}
      transaction_commit ();
      return (child);
    }
  else
    {
      /* In the child process -- if any errors occur, just exit. */

      /* Force child into different session. */
      if ((UX_setsid ()) < 0)
	goto kill_child;

#ifdef HAVE_PTYS
      /* If connection is a PTY, open the slave side (which becomes
	 the controlling terminal). */
      if (ctty_type == ctty_type_pty)
	{
	  int fd = (UX_open (pty_name, O_RDWR, 0));
	  if (fd < 0)
	    goto kill_child;
	  MAKE_CHANNEL (fd, channel_type_terminal, child_read =);
	  child_write = child_read;
	  if ((child_setup_tty (child_read)) < 0)
	    goto kill_child;
	}
#endif /* HAVE_PTYS */

#ifdef HAVE_DUP2
      /* Setup the standard I/O for the child. */
      if (((UX_dup2 (child_read, STDIN_FILENO)) < 0) ||
	  ((UX_dup2 (child_write, STDOUT_FILENO)) < 0) ||
	  ((UX_dup2 (child_write, STDERR_FILENO)) < 0))
	goto kill_child;
#else
#include "error: can't hack subprocess I/O without dup2() or equivalent"
#endif

      /* Close all other file descriptors. */
      {
	int fd = 0;
	int open_max = (UX_SC_OPEN_MAX ());
	while (fd < open_max)
	  if (! ((fd == STDIN_FILENO) ||
		 (fd == STDOUT_FILENO) ||
		 (fd == STDERR_FILENO)))
	    UX_close (fd++);
      }

      /* Force the signal mask to be empty.
         (This should be done for HAVE_SYSV3_SIGNALS too, but
	  it's more difficult in that case.) */
#ifdef HAVE_POSIX_SIGNALS
      {
	sigset_t empty_mask;
	UX_sigemptyset (&empty_mask);
	UX_sigprocmask (SIG_SETMASK, (&empty_mask), 0);
      }
#else /* not HAVE_POSIX_SIGNALS */
#ifdef HAVE_BSD_SIGNALS
      UX_sigsetmask (0);
#endif /* HAVE_BSD_SIGNALS */
#endif /* HAVE_POSIX_SIGNALS */

      /* Start the process. */
      execve (filename, argv, envp);
    kill_child:
      _exit (1);
    }
}

static void
DEFUN (subprocess_death, (pid, status), pid_t pid AND wait_status_t * status)
{
  Tprocess process = (find_process (pid));
  if (process != NO_PROCESS)
    {
      if (WIFEXITED (*status))
	{
	  (PROCESS_CHANGED (process)) = 1;
	  (PROCESS_STATUS (process)) = process_status_exited;
	  (PROCESS_REASON (process)) = (WEXITSTATUS (*status));
	}
      else if (WIFSTOPPED (*status))
	{
	  (PROCESS_CHANGED (process)) = 1;
	  (PROCESS_STATUS (process)) = process_status_stopped;
	  (PROCESS_REASON (process)) = (WSTOPSIG (*status));
	  if (PROCESS_SYNCHRONOUS (process))
	    UX_kill (pid, SIGKILL);
	}
      else if (WIFSIGNALED (*status))
	{
	  (PROCESS_CHANGED (process)) = 1;
	  (PROCESS_STATUS (process)) = process_status_signalled;
	  (PROCESS_REASON (process)) = (WTERMSIG (*status));
	}
    }
}

static Tprocess
DEFUN (find_process, (pid), pid_t pid)
{
  Tprocess process;
  for (process = 0; (process < OS_process_table_size); process += 1)
    if ((PROCESS_ID (process)) == pid)
      {
	if (((PROCESS_STATUS (process)) == process_status_free)
	    || ((PROCESS_STATUS (process)) == process_status_allocated))
	  break;
	return (process);
      }
  return (NO_PROCESS);
}

#define DEFUN_PROCESS_ACCESSOR(name, result_type, accessor)		\
result_type								\
DEFUN (name, (process), Tprocess process)				\
{									\
  return (accessor (process));						\
}

DEFUN_PROCESS_ACCESSOR (OS_process_id, pid_t, PROCESS_ID)
DEFUN_PROCESS_ACCESSOR (OS_process_status, enum process_status, PROCESS_STATUS)
DEFUN_PROCESS_ACCESSOR
  (OS_process_ctty_type, enum process_ctty_type, PROCESS_CTTY_TYPE)
DEFUN_PROCESS_ACCESSOR (OS_process_reason, unsigned short, PROCESS_REASON)
DEFUN_PROCESS_ACCESSOR (OS_process_synchronous, int, PROCESS_SYNCHRONOUS)

Tchannel
DEFUN (OS_process_input, (process), Tprocess process)
{
  Tchannel channel = (PROCESS_INPUT (process));
  if (channel == NO_CHANNEL)
    error_external_return ();
  return (channel);
}

Tchannel
DEFUN (OS_process_output, (process), Tprocess process)
{
  Tchannel channel = (PROCESS_OUTPUT (process));
  if (channel == NO_CHANNEL)
    error_external_return ();
  return (channel);
}

void
DEFUN (OS_process_send_signal, (process, sig), Tprocess process AND int sig)
{
  STD_VOID_SYSTEM_CALL ("kill", (UX_kill ((PROCESS_ID (process)), sig)));
}

void
DEFUN (OS_process_kill, (process), Tprocess process)
{
  OS_process_send_signal (process, SIGKILL);
}

void
DEFUN (OS_process_stop, (process), Tprocess process)
{
  if (UX_SC_JOB_CONTROL ())
    OS_process_send_signal (process, SIGTSTP);
  else
    error_unimplemented_primitive ();
}

void
DEFUN (OS_process_continue, (process), Tprocess process)
{
  if (UX_SC_JOB_CONTROL ())
    OS_process_send_signal (process, SIGCONT);
  else
    error_unimplemented_primitive ();
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

#ifdef HAVE_PTYS

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
DEFUN (child_setup_tty, (channel), Tchannel channel)
{
  int fd = (CHANNEL_DESCRIPTOR (channel));
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
DEFUN (child_setup_tty, (channel), Tchannel channel)
{
  int fd = (CHANNEL_DESCRIPTOR (channel));
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
DEFUN (child_setup_tty, (channel), Tchannel channel)
{
  int fd = (CHANNEL_DESCRIPTOR (channel));
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
#endif /* HAVE_PTYS */
