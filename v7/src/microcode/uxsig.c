/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxsig.c,v 1.12 1991/07/05 23:31:35 cph Exp $

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
#include "ossig.h"
#include "osctty.h"
#include "ostty.h"
#include "uxtrap.h"
#include "uxutil.h"
#include "critsec.h"

/* Signal Manipulation */

#ifdef HAVE_POSIX_SIGNALS

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  struct sigaction act;
  UX_sigaction (signo, 0, (&act));
  return (act . sa_handler);
}

#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#endif

static void
DEFUN (INSTALL_HANDLER, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  struct sigaction act;
  (act . sa_handler) = handler;
  UX_sigemptyset (& (act . sa_mask));
  UX_sigaddset ((& (act . sa_mask)), signo);
  (act . sa_flags) = SA_SIGINFO;
  UX_sigaction (signo, (&act), 0);
}

#else /* not HAVE_POSIX_SIGNALS */
#ifdef HAVE_SYSV3_SIGNALS

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  Tsignal_handler result = (UX_sigset (signo, SIG_HOLD));
  if (result != SIG_HOLD)
    UX_signal (signo, result);
  return (result);
}

#define INSTALL_HANDLER UX_sigset

#define NEED_HANDLER_TRANSACTION
#define ENTER_HANDLER(signo)
#define ABORT_HANDLER(signo, handler) UX_sigrelse (signo)
#define EXIT_HANDLER(signo, handler)

#else /* not HAVE_SYSV3_SIGNALS */

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  Tsignal_handler result = (UX_signal (signo, SIG_IGN));
  if (result != SIG_IGN)
    UX_signal (signo, result);
  return (result);
}

#define INSTALL_HANDLER UX_signal

#define NEED_HANDLER_TRANSACTION
#define ENTER_HANDLER(signo) UX_signal ((signo), SIG_IGN)
#define ABORT_HANDLER UX_signal
#define EXIT_HANDLER UX_signal

#endif /* HAVE_SYSV3_SIGNALS */
#endif /* HAVE_POSIX_SIGNALS */

#ifdef HAVE_POSIX_SIGNALS

static void
DEFUN (restore_signal_mask, (environment), PTR environment)
{
  UX_sigprocmask (SIG_SETMASK, ((sigset_t *) environment), 0);
}

static void
DEFUN (save_signal_mask, (environment), PTR environment)
{
  UX_sigprocmask (SIG_SETMASK, 0, ((sigset_t *) environment));
}

void
DEFUN_VOID (preserve_signal_mask)
{
  dstack_alloc_and_protect
    ((sizeof (sigset_t)), save_signal_mask, restore_signal_mask);
}

static sigset_t blocked_signals;

void
DEFUN_VOID (block_signals)
{
  sigset_t all_signals;
  UX_sigfillset (&all_signals);
  UX_sigprocmask (SIG_BLOCK, (&all_signals), (&blocked_signals));
}

void
DEFUN_VOID (unblock_signals)
{
  UX_sigprocmask (SIG_SETMASK, (&blocked_signals), 0);
}

#else /* not HAVE_POSIX_SIGNALS */

void
DEFUN_VOID (preserve_signal_mask)
{
}

void
DEFUN_VOID (block_signals)
{
}

void
DEFUN_VOID (unblock_signals)
{
}

#endif /* not HAVE_POSIX_SIGNALS */

/* Signal Descriptors */

enum dfl_action { dfl_terminate, dfl_ignore, dfl_stop };

struct signal_descriptor
{
  int signo;
  CONST char * name;
  enum dfl_action action;
  int flags;
};

/* `flags' bits */
#define NOIGNORE 1
#define NOBLOCK 2
#define NOCATCH 4
#define CORE_DUMP 8

static struct signal_descriptor * signal_descriptors;
static unsigned int signal_descriptors_length;
static unsigned int signal_descriptors_limit;

static void
DEFUN (defsignal, (signo, name, action, flags),
       int signo AND
       CONST char * name AND
       enum dfl_action action AND
       int flags)
{
  if (signo == 0)
    return;
  if (signal_descriptors_length == signal_descriptors_limit)
    {
      signal_descriptors_limit += 8;
      signal_descriptors =
	(UX_realloc (signal_descriptors,
		     (signal_descriptors_limit *
		      (sizeof (struct signal_descriptor)))));
      if (signal_descriptors == 0)
	{
	  fprintf (stderr, "\nUnable to grow signal definitions table.\n");
	  fflush (stderr);
	  termination_init_error ();
	}
    }
  {
    struct signal_descriptor * sd =
      (signal_descriptors + (signal_descriptors_length++));
    (sd -> signo) = signo;
    (sd -> name) = name;
    (sd -> action) = action;
    (sd -> flags) = flags;
  }
}

static struct signal_descriptor *
DEFUN (find_signal_descriptor, (signo), int signo)
{
  struct signal_descriptor * scan = signal_descriptors;
  struct signal_descriptor * end = (scan + signal_descriptors_length);
  for (; (scan < end); scan += 1)
    if ((scan -> signo) == signo)
      return (scan);
  return (0);
}

CONST char *
DEFUN (find_signal_name, (signo), int signo)
{
  static char buffer [32];
  struct signal_descriptor * descriptor = (find_signal_descriptor (signo));
  if (descriptor != 0)
    return (descriptor -> name);
  sprintf (buffer, "unknown signal %d", signo);
  return ((CONST char *) buffer);
}

#ifdef _HPUX

#define OS_SPECIFIC_SIGNALS()						\
{									\
  defsignal (SIGPWR, "SIGPWR",		dfl_ignore,	0);		\
  defsignal (SIGWINDOW, "SIGWINDOW",	dfl_ignore,	0);		\
  defsignal (SIGLOST, "SIGLOST",	dfl_terminate,	0);		\
}

#else /* not _HPUX */
#ifdef _BSD

#define OS_SPECIFIC_SIGNALS()						\
{									\
  defsignal (SIGXCPU, "SIGXCPU",	dfl_terminate,	0);		\
  defsignal (SIGXFSZ, "SIGXFSZ",	dfl_terminate,	0);		\
  defsignal (SIGWINCH, "SIGWINCH",	dfl_ignore,	0);		\
}

#endif /* _BSD */
#endif /* _HPUX */

#if (SIGABRT == SIGIOT)
#undef SIGABRT
#define SIGABRT 0
#endif

static void
DEFUN_VOID (initialize_signal_descriptors)
{
  signal_descriptors_length = 0;
  signal_descriptors_limit = 32;
  signal_descriptors =
    (UX_malloc (signal_descriptors_limit *
		(sizeof (struct signal_descriptor))));
  if (signal_descriptors == 0)
    {
      fprintf (stderr, "\nUnable to allocate signal definitions table.\n");
      fflush (stderr);
      termination_init_error ();
    }
  defsignal (SIGHUP, "SIGHUP",		dfl_terminate,	0);
  defsignal (SIGINT, "SIGINT",		dfl_terminate,	0);
  defsignal (SIGQUIT, "SIGQUIT",	dfl_terminate,	CORE_DUMP);
  defsignal (SIGILL, "SIGILL",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGTRAP, "SIGTRAP",	dfl_terminate,	CORE_DUMP);
  defsignal (SIGIOT, "SIGIOT",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGEMT, "SIGEMT",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGFPE, "SIGFPE",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGKILL, "SIGKILL",	dfl_terminate,	(NOIGNORE | NOBLOCK | NOCATCH));
  defsignal (SIGBUS, "SIGBUS",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGSEGV, "SIGSEGV",	dfl_terminate,	CORE_DUMP);
  defsignal (SIGSYS, "SIGSYS",		dfl_terminate,	CORE_DUMP);
  defsignal (SIGPIPE, "SIGPIPE",	dfl_terminate,	0);
  defsignal (SIGALRM, "SIGALRM",	dfl_terminate,	0);
  defsignal (SIGTERM, "SIGTERM",	dfl_terminate,	0);
  defsignal (SIGUSR1, "SIGUSR1",	dfl_terminate,	0);
  defsignal (SIGUSR2, "SIGUSR2",	dfl_terminate,	0);
  defsignal (SIGABRT, "SIGABRT",	dfl_terminate,	CORE_DUMP);
  defsignal (SIGIO, "SIGIO",		dfl_ignore,	0);
  defsignal (SIGURG, "SIGURG",		dfl_ignore,	0);
  defsignal (SIGVTALRM, "SIGVTALRM",	dfl_terminate,	0);
  defsignal (SIGPROF, "SIGPROF",	dfl_terminate,	0);
  defsignal (SIGSTOP, "SIGSTOP",	dfl_stop,	(NOIGNORE | NOBLOCK | NOCATCH));
  defsignal (SIGTSTP, "SIGTSTP",	dfl_stop,	0);
  defsignal (SIGCONT, "SIGCONT",	dfl_ignore,	(NOIGNORE | NOBLOCK));
  defsignal (SIGCHLD, "SIGCHLD",	dfl_ignore,	0);
  defsignal (SIGTTIN, "SIGTTIN",	dfl_stop,	0);
  defsignal (SIGTTOU, "SIGTTOU",	dfl_stop,	0);
#ifdef OS_SPECIFIC_SIGNALS
  OS_SPECIFIC_SIGNALS ();
#endif
}

/* Signal Handlers */

#ifndef NEED_HANDLER_TRANSACTION

#define DEFUN_STD_HANDLER(name, statement)				\
static Tsignal_handler_result						\
DEFUN (name, (signo, info, pscp),					\
       int signo AND							\
       SIGINFO_T info AND						\
       struct SIGCONTEXT * pscp)					\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_FULL_SIGCONTEXT (scp);					\
  INITIALIZE_FULL_SIGCONTEXT (pscp, scp);				\
  STD_HANDLER_abortp = (enter_interruption_extent ());			\
  statement;								\
  if (STD_HANDLER_abortp)						\
    exit_interruption_extent ();					\
  SIGNAL_HANDLER_RETURN ();						\
}

#else /* NEED_HANDLER_TRANSACTION */

struct handler_record
{
  int signo;
  Tsignal_handler handler;
}

#define DEFUN_STD_HANDLER(name, statement)				\
static Tsignal_handler_result						\
DEFUN (name, (signo, info, pscp),					\
       int signo AND							\
       SIGINFO_T info AND						\
       struct SIGCONTEXT * pscp)					\
{									\
  int STD_HANDLER_abortp;						\
  DECLARE_FULL_SIGCONTEXT (scp);					\
  INITIALIZE_FULL_SIGCONTEXT (pscp, scp);				\
  ENTER_HANDLER (signo);						\
  STD_HANDLER_abortp = (enter_interruption_extent ());			\
  transaction_begin ();							\
  {									\
    struct handler_record * record =					\
      (dstack_alloc (sizeof (struct handler_record)));			\
    (record -> signo) = signo;						\
    (record -> handler) = handler;					\
    transaction_record_action (tat_abort, ta_abort_handler, record);	\
  }									\
  statement;								\
  if (STD_HANDLER_abortp)						\
    {									\
      transaction_abort ();						\
      exit_interruption_extent ();					\
    }									\
  transaction_commit ();						\
  EXIT_HANDLER (signo, name);						\
  SIGNAL_HANDLER_RETURN ();						\
}

static void
DEFUN (ta_abort_handler, (ap), PTR ap)
{
  ABORT_HANDLER ((((struct handler_record *) ap) -> signo),
		 (((struct handler_record *) ap) -> handler));
}

#endif /* NEED_HANDLER_TRANSACTION */

#define CONTROL_B_INTERRUPT_CHAR 'B'
#define CONTROL_G_INTERRUPT_CHAR 'G'
#define CONTROL_U_INTERRUPT_CHAR 'U'
#define CONTROL_X_INTERRUPT_CHAR 'X'

static void
DEFUN (echo_keyboard_interrupt, (c, dc), cc_t c AND cc_t dc)
{
  if (c == (OS_ctty_disabled_char ()))
    c = dc;
  c &= 0177;
  if (c == ALERT_CHAR)
    putc (c, stdout);
  else if (c < '\040')
    {
      putc ('^', stdout);
      putc ((c + '@'), stdout);
    }
  else if (c == '\177')
    fputs ("^?", stdout);
  else
    putc (c, stdout);
  fflush (stdout);
}

DEFUN_STD_HANDLER (sighnd_control_g,
  {
    echo_keyboard_interrupt ((OS_ctty_int_char ()), ALERT_CHAR);
    tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
  })

static void EXFUN
  (interactive_interrupt_handler, (struct FULL_SIGCONTEXT * scp));

DEFUN_STD_HANDLER (sighnd_interactive,
  (interactive_interrupt_handler (scp)))

void
DEFUN (stop_signal_default, (signo), int signo)
{
#ifdef HAVE_POSIX_SIGNALS
  /* No need to handle systems without POSIX signals;
     all job-control systems have them. */
  sigset_t signo_mask;
  sigset_t old_mask;
  Tsignal_handler handler;

  /* Give the terminal back to the invoking process. */
  OS_save_internal_state ();
  OS_restore_external_state ();

  /* Temporarily unbind this handler. */
  handler = (current_handler (signo));
  INSTALL_HANDLER (signo, SIG_DFL);

  /* Perform the default action for this signal. */
  UX_sigemptyset (&signo_mask);
  UX_sigaddset ((&signo_mask), signo);
  UX_sigprocmask (SIG_UNBLOCK, (&signo_mask), (&old_mask));
  UX_kill ((UX_getpid ()), signo);
  UX_sigprocmask (SIG_SETMASK, (&old_mask), 0);

  /* Rebind this handler. */
  INSTALL_HANDLER (signo, handler);

  /* Get the terminal back to its original state. */
  OS_save_external_state ();
  OS_restore_internal_state ();
#endif /* HAVE_POSIX_SIGNALS */
}

void EXFUN ((*stop_signal_hook), (int signo));

#ifdef HAVE_POSIX_SIGNALS
#  define IF_POSIX_SIGNALS(code) do code while (0)
#else
#  define IF_POSIX_SIGNALS(code) do {} while (0)
#endif

DEFUN_STD_HANDLER (sighnd_stop,
  IF_POSIX_SIGNALS(
  {
    sigset_t old_mask;
    sigset_t jc_mask;

    if (! (UX_SC_JOB_CONTROL ()))
      return;
    /* Initialize the signal masks. */
    UX_sigemptyset (&jc_mask);
    UX_sigaddset ((&jc_mask), SIGTTOU);
    UX_sigaddset ((&jc_mask), SIGTTIN);
    UX_sigaddset ((&jc_mask), SIGTSTP);
    UX_sigaddset ((&jc_mask), SIGSTOP);
    UX_sigaddset ((&jc_mask), SIGCHLD);

    /* Block the job-control signals. */
    UX_sigprocmask (SIG_BLOCK, (&jc_mask), (&old_mask));

    if (stop_signal_hook == 0)
      stop_signal_default (signo);
    else
      (*stop_signal_hook) (signo);

    /* Restore the signal mask to its original state. */
    UX_sigprocmask (SIG_SETMASK, (&old_mask), 0);
  }))

void
DEFUN_VOID (OS_restartable_exit)
{
  stop_signal_default (SIGTSTP);
}

#ifdef HAVE_ITIMER

DEFUN_STD_HANDLER (sighnd_timer,
  {
    request_timer_interrupt ();
  })

#else /* not HAVE_ITIMER */

extern void EXFUN (reschedule_alarm, (void));

DEFUN_STD_HANDLER (sighnd_timer,
  {
    reschedule_alarm ();
    request_timer_interrupt ();
  })

#endif /* HAVE_ITIMER */

DEFUN_STD_HANDLER (sighnd_save_then_terminate,
  (request_suspend_interrupt ()))

DEFUN_STD_HANDLER (sighnd_terminate,
  (termination_signal
   ((! (option_emacs_subprocess && (signo == SIGHUP)))
    ? (find_signal_name (signo))
    : 0)))

DEFUN_STD_HANDLER (sighnd_fpe,
  {
    if (executing_scheme_primitive_p ())
      error_floating_point_exception ();
    trap_handler ("floating-point exception", signo, info, scp);
  })

DEFUN_STD_HANDLER (sighnd_hardware_trap,
  (trap_handler ("hardware fault", signo, info, scp)))

DEFUN_STD_HANDLER (sighnd_software_trap,
  (trap_handler ("system software fault", signo, info, scp)))

#ifdef HAVE_NICE

#ifndef NICE_DELTA
#define NICE_DELTA 5
#endif

DEFUN_STD_HANDLER (sighnd_renice,
  {
    fprintf (stderr, "\n;;; Renicing! New nice value = %d\n",
	     ((nice (NICE_DELTA)) + 20));
    fflush (stderr);
  })

#endif /* HAVE_NICE */

/* When a child process terminates, it becomes a zombie until its
   parent process calls one of the wait() routines to obtain the
   child's termination status.  The SIGCHLD handler must always call
   wait() or waitpid() to permit the child process's resources to be
   freed. */

/* On systems with waitpid() (i.e. those that support WNOHANG) we must
   loop until there are no more processes, because some of those
   systems may deliver only one SIGCHLD when more than one child
   terminates.  Systems without waitpid() (e.g. _SYSV) typically
   provide queuing of SIGCHLD such that one SIGCHLD is delivered for
   every child that terminates.  Systems that provide neither
   waitpid() nor queuing are so losing that we can't win, in which
   case we just hope that child terminations don't happen too close to
   one another to cause problems. */

void EXFUN ((*subprocess_death_hook), (pid_t pid, wait_status_t * status));

#ifdef HAVE_WAITPID
#define WAITPID(status) (UX_waitpid ((-1), (status), (WNOHANG | WUNTRACED)))
#define BREAK
#else
#define WAITPID(status) (UX_wait (status))
#define BREAK break
#endif

DEFUN_STD_HANDLER (sighnd_dead_subprocess,
  {
    while (1)
      {
	wait_status_t status;
	pid_t pid = (WAITPID (&status));
	if (pid <= 0)
	  break;
	if (subprocess_death_hook != 0)
	  (*subprocess_death_hook) (pid, (&status));
	BREAK;
      }
  })

/* Signal Bindings */

static void
DEFUN (bind_handler, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  if ((signo != 0)
      && ((handler != ((Tsignal_handler) sighnd_stop))
	  || (UX_SC_JOB_CONTROL ()))
      && ((current_handler (signo)) == SIG_DFL))
    INSTALL_HANDLER (signo, handler);
}

void
DEFUN_VOID (UX_initialize_signals)
{
  stop_signal_hook = 0;
  subprocess_death_hook = 0;
  initialize_signal_descriptors ();
  bind_handler (SIGINT,		sighnd_control_g);
  bind_handler (SIGFPE,		sighnd_fpe);
  bind_handler (SIGALRM,	sighnd_timer);
  bind_handler (SIGVTALRM,	sighnd_timer);
  bind_handler (SIGUSR1,	sighnd_save_then_terminate);
#ifdef HAVE_NICE
  bind_handler (SIGUSR2,	sighnd_renice);
#endif
  bind_handler (SIGCHLD,	sighnd_dead_subprocess);
  /* If this signal is ignored, then the system call that would have
     caused it will return EPIPE instead.  This is much easier for us
     to handle. */
  bind_handler (SIGPIPE,	SIG_IGN);
  if ((isatty (STDIN_FILENO)) || option_emacs_subprocess)
    {
      if (!option_emacs_subprocess)
	bind_handler (SIGHUP,	sighnd_save_then_terminate);
      bind_handler (SIGQUIT,	sighnd_interactive);
      bind_handler (SIGPWR,	sighnd_save_then_terminate);
      bind_handler (SIGTSTP,	sighnd_stop);
      bind_handler (SIGILL,	sighnd_hardware_trap);
      bind_handler (SIGTRAP,	sighnd_hardware_trap);
      bind_handler (SIGBUS,	sighnd_hardware_trap);
      bind_handler (SIGSEGV,	sighnd_hardware_trap);
      bind_handler (SIGIOT,	sighnd_software_trap);
      bind_handler (SIGEMT,	sighnd_software_trap);
      bind_handler (SIGSYS,	sighnd_software_trap);
      bind_handler (SIGABRT,	sighnd_software_trap);
      bind_handler (SIGPROF,	sighnd_software_trap);
    }
  {
    struct signal_descriptor * scan = signal_descriptors;
    struct signal_descriptor * end = (scan + signal_descriptors_length);
    while (scan < end)
      {
	if (((scan -> flags) & NOCATCH) == 0)
	  switch (scan -> action)
	    {
	    case dfl_terminate:
	      bind_handler ((scan -> signo), sighnd_terminate);
	      break;
	    case dfl_stop:
	      bind_handler ((scan -> signo), sighnd_stop);
	      break;
	    }
	scan += 1;
      }
  }
}

/* Interactive Interrupt Handler */

static void EXFUN (print_interactive_help, (void));
static void EXFUN (print_interrupt_chars, (void));
static void EXFUN (examine_memory, (void));
static void EXFUN (reset_query, (struct FULL_SIGCONTEXT * scp));

#define INTERACTIVE_NEWLINE()						\
{									\
  if (!option_emacs_subprocess)						\
    {									\
      putc ('\n', stdout);						\
      fflush (stdout);							\
    }									\
}

static void
DEFUN (interactive_interrupt_handler, (scp), struct FULL_SIGCONTEXT * scp)
{
  if (!option_emacs_subprocess)
    {
      fputs ((OS_tty_command_beep ()), stdout);
      putc ('\n', stdout);
      fflush (stdout);
    }
  while (1)
    {
      if (!option_emacs_subprocess)
	{
	  fprintf (stdout, "Interrupt option (? for help): ");
	  fflush (stdout);
	}
      switch (userio_read_char_raw ())
	{
	case '\002':		/* C-B */
	case 'B':
	case 'b':
	  tty_set_next_interrupt_char (CONTROL_B_INTERRUPT_CHAR);
	  return;
	case '\003':		/* C-C */
	case '\007':		/* C-G */
	case 'G':
	case 'g':
	  tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
	  return;
	case '\025':		/* C-U */
	case 'U':
	case 'u':
	  tty_set_next_interrupt_char (CONTROL_U_INTERRUPT_CHAR);
	  return;
	case '\030':		/* C-X */
	case 'X':
	case 'x':
	  tty_set_next_interrupt_char (CONTROL_X_INTERRUPT_CHAR);
	  return;
	case 'E':
	case 'e':
	  INTERACTIVE_NEWLINE ();
	  examine_memory ();
	  return;
	case 'D':
	case 'd':
	  INTERACTIVE_NEWLINE ();
	  debug_edit_flags ();
	  return;
	case 'T':
	case 't':
	  INTERACTIVE_NEWLINE ();
	  debug_back_trace ();
	  return;
	case 'Z':
	case 'z':
	  INTERACTIVE_NEWLINE ();
	  OS_restartable_exit ();
	  return;
	case 'Q':
	case 'q':
	  INTERACTIVE_NEWLINE ();
	  termination_normal ();
	  return;
	case '\f':
	  if (!option_emacs_subprocess)
	    {
	      fputs ((OS_tty_command_clear ()), stdout);
	      fflush (stdout);
	    }
	  return;
	case 'R':
	case 'r':
	  reset_query (scp);
	  return;
	case 'H':
	case 'h':
	  if (!option_emacs_subprocess)
	    print_interrupt_chars ();
	  break;
	case 'I':
	case 'i':
	  if (!option_emacs_subprocess)
	    {
	      fputs ("Ignored.  Resuming Scheme.\n", stdout);
	      fflush (stdout);
	    }
	  return;
	default:
	  if (!option_emacs_subprocess)
	    print_interactive_help ();
	  break;
	}
    }
}

static enum interrupt_handler
DEFUN (encode_interrupt_handler, (handler), Tsignal_handler handler)
{
  return
    ((handler == ((Tsignal_handler) sighnd_control_g))
     ? interrupt_handler_control_g
     : (handler == ((Tsignal_handler) sighnd_interactive))
     ? interrupt_handler_interactive
     : (handler == ((Tsignal_handler) sighnd_stop))
     ? interrupt_handler_stop
     : (handler == ((Tsignal_handler) sighnd_terminate))
     ? interrupt_handler_terminate
     : (handler == ((Tsignal_handler) SIG_IGN))
     ? interrupt_handler_ignore
     : (handler == ((Tsignal_handler) SIG_DFL))
     ? interrupt_handler_default
     : interrupt_handler_unknown);
}

static Tsignal_handler
DEFUN (decode_interrupt_handler, (encoding), enum interrupt_handler encoding)
{
  return
    ((encoding == interrupt_handler_control_g)
     ? ((Tsignal_handler) sighnd_control_g)
     : (encoding == interrupt_handler_interactive)
     ? ((Tsignal_handler) sighnd_interactive)
     : (encoding == interrupt_handler_stop)
     ? ((Tsignal_handler) sighnd_stop)
     : (encoding == interrupt_handler_terminate)
     ? ((Tsignal_handler) sighnd_terminate)
     : (encoding == interrupt_handler_ignore)
     ? ((Tsignal_handler) SIG_IGN)
     : (encoding == interrupt_handler_default)
     ? ((Tsignal_handler) SIG_DFL)
     : ((Tsignal_handler) 0));
}

enum interrupt_handler
DEFUN_VOID (OS_signal_quit_handler)
{
  return (encode_interrupt_handler (current_handler (SIGQUIT)));
}

enum interrupt_handler
DEFUN_VOID (OS_signal_int_handler)
{
  return (encode_interrupt_handler (current_handler (SIGINT)));
}

enum interrupt_handler
DEFUN_VOID (OS_signal_tstp_handler)
{
  return
    ((UX_SC_JOB_CONTROL ())
     ? (encode_interrupt_handler (current_handler (SIGTSTP)))
     : interrupt_handler_ignore);
}

void
DEFUN (OS_signal_set_interrupt_handlers,
       (quit_handler, int_handler, tstp_handler),
       enum interrupt_handler quit_handler AND
       enum interrupt_handler int_handler AND
       enum interrupt_handler tstp_handler)
{
  {
    Tsignal_handler handler = (decode_interrupt_handler (quit_handler));
    if (handler != 0)
      INSTALL_HANDLER (SIGQUIT, handler);
  }
  {
    Tsignal_handler handler = (decode_interrupt_handler (int_handler));
    if (handler != 0)
      INSTALL_HANDLER (SIGINT, handler);
  }
  if (UX_SC_JOB_CONTROL ())
    {
      Tsignal_handler handler = (decode_interrupt_handler (tstp_handler));
      if (handler != 0)
	INSTALL_HANDLER (SIGTSTP, handler);
    }
}

static void
DEFUN (describe_sighnd, (signo, c), int signo AND unsigned char c)
{
  switch (encode_interrupt_handler (current_handler (signo)))
    {
    case interrupt_handler_control_g:
      fputs ("When typed, scheme will get the ^G character interrupt.\n",
	     stdout);
      fputs ("The default action is to abort the running program,\n", stdout);
      fputs ("and to resume the top level read-eval-print loop.\n", stdout);
      break;
    case interrupt_handler_interactive:
      fputs ("When typed, various interrupt options are offered.\n", stdout);
      fprintf (stdout, "Type %s followed by `?' for a list of options.\n",
	       (char_description (c, 0)));
      break;
    case interrupt_handler_terminate:
    describe_terminate:
      fputs ("When typed, scheme will terminate.\n", stdout);
      break;
    case interrupt_handler_stop:
    describe_stop:
      fputs ("When typed, scheme will suspend execution.\n", stdout);
      break;
    case interrupt_handler_ignore:
    describe_ignore:
      fputs ("When typed, this character will be ignored.\n", stdout);
      break;
    case interrupt_handler_default:
      {
	struct signal_descriptor * descriptor =
	  (find_signal_descriptor (signo));
	if (descriptor != 0)
	  switch (descriptor -> action)
	    {
	    case dfl_ignore: goto describe_ignore;
	    case dfl_stop: goto describe_stop;
	    case dfl_terminate: goto describe_terminate;
	    }
      }
    default:
      fputs ("When typed, this character will have an unknown effect.\n",
	     stdout);
      break;
    }
}

static void
DEFUN_VOID (print_interrupt_chars)
{
  {
    unsigned char quit_char = (OS_ctty_quit_char ());
    fprintf (stdout, "\n\nThe quit character is %s.\n",
	     (char_description (quit_char, 1)));
    describe_sighnd (SIGQUIT, quit_char);
  }
  {
    unsigned char int_char = (OS_ctty_int_char ());
    fprintf (stdout, "\nThe interrupt character is %s.\n",
	     (char_description (int_char, 1)));
    describe_sighnd (SIGINT, int_char);
  }
  if (UX_SC_JOB_CONTROL ())
    {
      unsigned char tstp_char = (OS_ctty_tstp_char ());
      fprintf (stdout, "\nThe terminal stop character is %s.\n",
	       (char_description (tstp_char, 1)));
      describe_sighnd (SIGTSTP, tstp_char);
    }
  putc ('\n', stdout);
  fflush (stdout);
}

static void
DEFUN_VOID (print_interactive_help)
{
  fputs ("\n\n", stdout);
  fputs ("^B: Enter a breakpoint loop.\n", stdout);
  fputs ("^C: Goto to top level read-eval-print (REP) loop.\n", stdout);
  fputs ("^L: Clear the screen.\n", stdout);
  fputs ("^U: Up to previous (lower numbered) REP loop.\n", stdout);
  fputs ("^X: Abort to current REP loop.\n", stdout);
  fputs ("D: Debugging: change interpreter flags.\n", stdout);
  fputs ("E: Examine memory location.\n", stdout);
  fputs ("H: Print simple information on interrupts.\n", stdout);
  fputs ("I: Ignore interrupt request.\n", stdout);
  fputs ("Q: Quit instantly, killing Scheme.\n", stdout);
  fputs ("R: Hard reset, possibly killing Scheme in the process.\n", stdout);
  fputs ("T: Stack trace.\n", stdout);
  if (UX_SC_JOB_CONTROL ())
    fputs ("Z: Quit instantly, suspending Scheme.\n", stdout);
  fputs ("\n", stdout);
}

static void
DEFUN (reset_query, (scp), struct FULL_SIGCONTEXT * scp)
{
  putc ('\n', stdout);
  fflush (stdout);
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      static CONST char * reset_choices [] =
	{
	  "D = delay reset until the end of the critical section",
	  "N = attempt reset now",
	  "P = punt reset",
	  0
	  };
      fprintf (stdout,
	       "Scheme is executing within critical section \"%s\".\n",
	       (CRITICAL_SECTION_NAME ()));
      fputs ("Resetting now is likely to kill Scheme.\n", stdout);
      fflush (stdout);
      switch (userio_choose_option
	      ("Choose one of the following actions:",
	       "Action -> ",
	       reset_choices))
	{
	case 'D':
	  SET_CRITICAL_SECTION_HOOK (soft_reset);
	  return;
	case 'N':
	  CLEAR_CRITICAL_SECTION_HOOK ();
	  EXIT_CRITICAL_SECTION ({});
	  hard_reset (scp);
	case 'P':
	default:
	  return;
	}
    }
  if (userio_confirm ("Do you really want to reset? [Y or N] "))
    hard_reset (scp);
}

static void
DEFUN_VOID (examine_memory)
{
  char input_string [256];
  fputs ("Enter location to examine (0x prefix for hex): ", stdout);
  fflush (stdout);
  {
    transaction_begin ();
    userio_buffered_input ();
    {
      char * scan = input_string;
      char * end = (input_string + (sizeof (input_string)));
      while (scan < end)
	{
	  char c = (userio_read_char ());
	  (*scan) = c;
	  if (c == '\n')
	    c = '\0';
	  if (c == '\0')
	    break;
	  scan += 1;
	}
    }
    transaction_commit ();
  }
  {
    long input;
    if (((((input_string[0]) == '0') && ((input_string[1]) == 'x'))
	 ? (sscanf ((&input_string[2]), "%lx", (&input)))
	 : (sscanf (input_string, "%ld", (&input))))
	== 1)
      debug_examine_memory (input, "contents");
  }
  putc ('\n', stdout);
  fflush (stdout);
}

#ifdef sun3

/* This code assumes that it is called very soon, before
   any registers except fp have been clobbered.

   It also assumes that it is called directly by the
   handler, so that the original fp can be found
   by indirecting through fp twice.

   The trampoline routine saves d0, d1, a0, and a1
   before invoking the handler.

   The magic constant of 276 was found by poking with adb. */

static void
DEFUN (sun3_save_regs, (regs), int * regs)
{
  asm ("\n\
	movel	a6@(8),a0\n\
	movel	a6@,a1\n\
\n\
	movel	a1@(276),a0@\n\
	movel	a1@(280),a0@(4)\n\
	movel	d2,a0@(8)\n\
	movel	d3,a0@(12)\n\
	movel	d4,a0@(16)\n\
	movel	d5,a0@(20)\n\
	movel	d6,a0@(24)\n\
	movel	d7,a0@(28)\n\
\n\
	movel	a1@(284),a0@(32)\n\
	movel	a1@(288),a0@(36)\n\
	movel	a2,a0@(40)\n\
	movel	a3,a0@(44)\n\
	movel	a4,a0@(48)\n\
	movel	a5,a0@(52)\n\
	movel	a1@,a0@(56)\n\
	");
}

#endif /* sun3 */

#ifdef vax

static int
DEFUN_VOID (vax_get_r0)
{
  /* This is a kludge. It relies on r0 being the return value register. */
  asm ("ret");
}

static int *
DEFUN (vax_save_start, (regs, r0), int * regs AND int r0)
{
  asm ("movl	fp,-(sp)");
  asm ("movl	4(ap),fp");
  asm ("movl	8(ap),(fp)");
  asm ("movl	r1,4(fp)");
  asm ("movl	r2,8(fp)");
  asm ("movl	r3,12(fp)");
  asm ("movl	r4,16(fp)");
  asm ("movl	r5,20(fp)");
  asm ("movl	r6,24(fp)");
  asm ("movl	r7,28(fp)");
  asm ("movl	r8,32(fp)");
  asm ("movl	r9,36(fp)");
  asm ("movl	r10,40(fp)");
  asm ("movl	r11,44(fp)");
  asm ("movl	(sp)+,fp");
  asm ("movl	12(fp),r0");
  asm ("ret");
}

static void
DEFUN (vax_save_finish, (fp, pscp, scp),
       int * fp AND
       struct sigcontext * pscp AND
       struct full_sigcontext * scp)
{
  (scp -> fs_original) = pscp;
#ifndef _ULTRIX
  /* For now, ap and fp undefined. */
  ((scp -> fs_regs) [12]) = (pscp -> sc_ap);
  ((scp -> fs_regs) [13]) = (pscp -> sc_fp);
#endif
  ((scp -> fs_regs) [14]) = (pscp -> sc_sp);
  ((scp -> fs_regs) [15]) = (pscp -> sc_pc);
  {
    int reg_number = 0;
    unsigned long reg_mask = (((fp[1]) >> 16) & 0x0fff);
    int stack_index = 5;
    while (reg_mask != 0)
      {
	if ((reg_mask & 1) != 0)
	  ((scp -> fs_regs) [reg_number]) = (fp[stack_index++]);
	reg_number += 1;
	reg_mask = ((reg_mask >> 1) & 0x0fff);
      }
  }
}

#endif /* vax */
