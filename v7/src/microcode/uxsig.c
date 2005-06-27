/* -*-C-*-

$Id: uxsig.c,v 1.41 2005/06/27 06:03:21 cph Exp $

Copyright 1990,1991,1992,1993,1994,1996 Massachusetts Institute of Technology
Copyright 2000,2001,2005 Massachusetts Institute of Technology

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

#include "config.h"
#include "ux.h"
#include "ossig.h"
#include "osctty.h"
#include "ostty.h"
#include "ostop.h"
#include "uxtrap.h"
#include "uxsig.h"
#include "uxutil.h"
#include "critsec.h"

extern cc_t EXFUN (OS_ctty_quit_char, (void));
extern cc_t EXFUN (OS_ctty_int_char, (void));
extern cc_t EXFUN (OS_ctty_tstp_char, (void));
extern cc_t EXFUN (OS_ctty_disabled_char, (void));
extern void EXFUN (tty_set_next_interrupt_char, (cc_t c));

/* Signal Manipulation */

#ifdef HAVE_POSIX_SIGNALS

#ifdef _POSIX_REALTIME_SIGNALS
#  define SIGACT_HANDLER(act) ((act) -> sa_sigaction)
#else
#  define SIGACT_HANDLER(act) ((act) -> sa_handler)
#  ifndef SA_SIGINFO
#    define SA_SIGINFO 0
#  endif
#endif

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  struct sigaction act;
  UX_sigaction (signo, 0, (&act));
  return (SIGACT_HANDLER (&act));
}

void
DEFUN (INSTALL_HANDLER, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  struct sigaction act;
  if ((handler == ((Tsignal_handler) SIG_IGN))
      || (handler == ((Tsignal_handler) SIG_DFL)))
    {
      (act . sa_handler) = ((PTR) handler);
      (act . sa_flags) = 0;
    }
  else
    {
      (SIGACT_HANDLER (&act)) = handler;
      (act . sa_flags) = SA_SIGINFO;
    }
  UX_sigemptyset (& (act . sa_mask));
  UX_sigaddset ((& (act . sa_mask)), signo);
  UX_sigaction (signo, (&act), 0);
}

#else /* not HAVE_POSIX_SIGNALS */
#ifdef HAVE_SIGHOLD

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  Tsignal_handler result = (UX_sigset (signo, SIG_HOLD));
  if (result != SIG_HOLD)
    UX_signal (signo, result);
  return (result);
}

#else /* not HAVE_SIGHOLD */

static Tsignal_handler
DEFUN (current_handler, (signo), int signo)
{
  Tsignal_handler result = (UX_signal (signo, SIG_IGN));
  if (result != SIG_IGN)
    UX_signal (signo, result);
  return (result);
}

#endif /* HAVE_SIGHOLD */
#endif /* HAVE_POSIX_SIGNALS */

#ifdef NEED_HANDLER_TRANSACTION

void
DEFUN (ta_abort_handler, (ap), PTR ap)
{
  ABORT_HANDLER ((((struct handler_record *) ap) -> signo),
		 (((struct handler_record *) ap) -> handler));
}

#endif /* NEED_HANDLER_TRANSACTION */

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

void
DEFUN (deactivate_handler, (signo), int signo)
{
  INSTALL_HANDLER (signo, ((Tsignal_handler) SIG_IGN));
}

void
DEFUN (activate_handler, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  INSTALL_HANDLER (signo, handler);
}

/* Signal Debugging */

#ifdef DEBUG_SIGNAL_DELIVERY

int signal_history [256];
int * signal_history_pointer;

static void
DEFUN_VOID (initialize_signal_debugging)
{
  int * scan = (&signal_history[0]);
  int * end = (scan + (sizeof (signal_history)));
  signal_history_pointer = scan;
  while (scan < end)
    (*scan++) = 0;
}

static void
DEFUN (record_signal_delivery, (signo), int signo)
{
  block_signals ();
  (*signal_history_pointer++) = signo;
  if (signal_history_pointer >= (& (signal_history [sizeof (signal_history)])))
    signal_history_pointer = (&signal_history[0]);
  unblock_signals ();
}

#else /* not DEBUG_SIGNAL_DELIVERY */

#define initialize_signal_debugging()

#endif /* not DEBUG_SIGNAL_DELIVERY */

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

#if (SIGABRT == SIGIOT)
#  undef SIGABRT
#  define SIGABRT 0
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
  defsignal (SIGLOST, "SIGLOST",	dfl_terminate,	0);
  defsignal (SIGXCPU, "SIGXCPU",	dfl_terminate,	0);
  defsignal (SIGXFSZ, "SIGXFSZ",	dfl_terminate,	0);
  defsignal (SIGPWR, "SIGPWR",		dfl_ignore,	0);
  defsignal (SIGWINDOW, "SIGWINDOW",	dfl_ignore,	0);
  defsignal (SIGWINCH, "SIGWINCH",	dfl_ignore,	0);
}

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

static
DEFUN_STD_HANDLER (sighnd_control_g,
{
  echo_keyboard_interrupt ((OS_ctty_int_char ()), ALERT_CHAR);
  tty_set_next_interrupt_char (CONTROL_G_INTERRUPT_CHAR);
})

static
DEFUN_STD_HANDLER (sighnd_control_u,
{
  tty_set_next_interrupt_char (CONTROL_U_INTERRUPT_CHAR);
})

static
DEFUN_STD_HANDLER (sighnd_control_x,
{
  tty_set_next_interrupt_char (CONTROL_X_INTERRUPT_CHAR);
})

static
DEFUN_STD_HANDLER (sighnd_control_b,
{
  tty_set_next_interrupt_char (CONTROL_B_INTERRUPT_CHAR);
})

static void EXFUN (interactive_interrupt_handler, (SIGCONTEXT_T * scp));

static
DEFUN_STD_HANDLER (sighnd_interactive,
  (interactive_interrupt_handler (scp)))

void
DEFUN (stop_signal_default, (signo), int signo)
{
#ifdef HAVE_POSIX_SIGNALS
  if ((isatty (STDIN_FILENO))
      && (isatty (STDOUT_FILENO))
      && (! option_emacs_subprocess))
  {
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
    INSTALL_HANDLER (signo, ((Tsignal_handler) SIG_DFL));

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
  }
#endif /* HAVE_POSIX_SIGNALS */
}

void EXFUN ((*stop_signal_hook), (int signo));

#ifdef HAVE_POSIX_SIGNALS
#  define IF_POSIX_SIGNALS(code) do code while (0)
#else
#  define IF_POSIX_SIGNALS(code) do {} while (0)
#endif

static
DEFUN_STD_HANDLER (sighnd_stop,
  IF_POSIX_SIGNALS (
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

/* The following conditionalization would more naturally be expressed
   by conditionalizing the code inside the handler, but the Sun
   compiler won't accept this conditionalization.  */

#ifdef HAVE_SETITIMER

static
DEFUN_STD_HANDLER (sighnd_timer,
{
  request_timer_interrupt ();
})

#else /* not HAVE_SETITIMER */

static
DEFUN_STD_HANDLER (sighnd_timer,
{
  extern void EXFUN (reschedule_alarm, (void));
  reschedule_alarm ();
  request_timer_interrupt ();
})

#endif /* not HAVE_SETITIMER */

static
DEFUN_STD_HANDLER (sighnd_save_then_terminate,
  (request_suspend_interrupt ()))

static
DEFUN_STD_HANDLER (sighnd_terminate,
  (termination_signal
   ((! (option_emacs_subprocess && (signo == SIGHUP)))
    ? (find_signal_name (signo))
    : 0)))

#ifdef HAS_COMPILER_SUPPORT
#  ifdef __IA32__

#define FPE_RESET_TRAPS()						\
{									\
  /* Reinitialize the floating-point control word.  */			\
  extern void EXFUN (i386_interface_initialize, (void));		\
  i386_interface_initialize ();						\
}

#  endif
#endif

#ifndef FPE_RESET_TRAPS
#  define FPE_RESET_TRAPS()
#endif

static
DEFUN_STD_HANDLER (sighnd_fpe,
{
  if (executing_scheme_primitive_p ())
    error_floating_point_exception ();
  FPE_RESET_TRAPS ();
  trap_handler ("floating-point exception", signo, info, scp);
})

static
DEFUN_STD_HANDLER (sighnd_hardware_trap,
  (trap_handler ("hardware fault", signo, info, scp)))

static
DEFUN_STD_HANDLER (sighnd_software_trap,
  (trap_handler ("system software fault", signo, info, scp)))

#ifdef HAVE_NICE

#ifndef NICE_DELTA
#define NICE_DELTA 5
#endif

static
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
   terminates.  Systems without waitpid() (e.g. System V) typically
   provide queuing of SIGCHLD such that one SIGCHLD is delivered for
   every child that terminates.  Systems that provide neither
   waitpid() nor queuing are so losing that we can't win, in which
   case we just hope that child terminations don't happen too close to
   one another to cause problems. */

void EXFUN ((*subprocess_death_hook), (pid_t pid, int * status));

#ifdef HAVE_WAITPID
#define WAITPID(status) (UX_waitpid ((-1), (status), (WNOHANG | WUNTRACED)))
#define BREAK
#else
#define WAITPID(status) (UX_wait (status))
#define BREAK break
#endif

static
DEFUN_STD_HANDLER (sighnd_dead_subprocess,
{
  while (1)
    {
      int status;
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
  Tsignal_handler old_handler
    = ((signo == 0)
       ? ((Tsignal_handler) SIG_DFL)
       : (current_handler (signo)));

  if ((signo != 0) 
      && ((old_handler == ((Tsignal_handler) SIG_DFL))
	  || ((old_handler == ((Tsignal_handler) SIG_IGN))
	      && (signo == SIGCHLD)))
      && ((handler != ((Tsignal_handler) sighnd_stop))
	  || (UX_SC_JOB_CONTROL ())))
    INSTALL_HANDLER (signo, handler);
}

static void
DEFUN_VOID (unblock_all_signals)
{
  /* Force the signal mask to be empty. */
#ifdef HAVE_POSIX_SIGNALS
  {
    sigset_t empty_mask;
    UX_sigemptyset (&empty_mask);
    UX_sigprocmask (SIG_SETMASK, (&empty_mask), 0);
  }
#else
#ifdef HAVE_SIGHOLD
  /* We could do something more here, but it is hard to enumerate all
     the possible signals.  Instead, just release SIGCHLD, which we
     know was held before the child was spawned. */
  UX_sigrelse (SIGCHLD);
#endif
#endif
}

void
DEFUN_VOID (UX_initialize_signals)
{
  stop_signal_hook = 0;
  subprocess_death_hook = 0;
  initialize_signal_descriptors ();
  initialize_signal_debugging ();
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
  bind_handler (SIGPIPE,	((Tsignal_handler) SIG_IGN));
  if ((isatty (STDIN_FILENO)) || option_emacs_subprocess)
    {
      if (getenv ("USE_SCHEMATIK_STYLE_INTERRUPTS"))
        bind_handler (SIGHUP,   sighnd_control_b);
      else if (!option_emacs_subprocess)
	bind_handler (SIGHUP,	sighnd_save_then_terminate);
      if (getenv ("USE_SCHEMATIK_STYLE_INTERRUPTS"))
        bind_handler (SIGQUIT,  sighnd_control_u);
      else
        bind_handler (SIGQUIT,	sighnd_interactive);
      bind_handler (SIGPWR,	sighnd_save_then_terminate);
      bind_handler (SIGTSTP,	sighnd_stop);
      bind_handler (SIGILL,	sighnd_hardware_trap);
      bind_handler (SIGTRAP,	sighnd_hardware_trap);
      bind_handler (SIGBUS,	sighnd_hardware_trap);
      bind_handler (SIGSEGV,	sighnd_hardware_trap);
      if (getenv ("USE_SCHEMATIK_STYLE_INTERRUPTS"))
        bind_handler (SIGIOT,   sighnd_control_x);
      else
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
	    case dfl_ignore:
	      break;
	    }
	scan += 1;
      }
  }
  unblock_all_signals ();
}

/* Initialize the signals in a child subprocess.  */

void
DEFUN_VOID (UX_initialize_child_signals)
{
  unblock_all_signals ();
  /* SIGPIPE was ignored above; we must set it back to the default
     because some programs depend on this.  */
  INSTALL_HANDLER (SIGPIPE, ((Tsignal_handler) SIG_DFL));
}

/* Interactive Interrupt Handler */

/* Under Unix, the interrupt char is NOT requested when the interrupt is
   taken.
 */
cc_t
DEFUN (OS_tty_map_interrupt_char, (int_char), cc_t int_char)
{
  return int_char;
}

static void EXFUN (print_interactive_help, (void));
static void EXFUN (print_interrupt_chars, (void));
static void EXFUN (examine_memory, (void));
static void EXFUN (reset_query, (SIGCONTEXT_T * scp));
static void EXFUN (interactive_back_trace, (void));

#define INTERACTIVE_NEWLINE()						\
{									\
  if (!option_emacs_subprocess)						\
    {									\
      putc ('\n', stdout);						\
      fflush (stdout);							\
    }									\
}

static void
DEFUN (interactive_interrupt_handler, (scp), SIGCONTEXT_T * scp)
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
	  interactive_back_trace ();
	  return;
	case 'Z':
	case 'z':
	  INTERACTIVE_NEWLINE ();
	  OS_restartable_exit ();
	  return;
	case 'Q':
	case 'q':
	  INTERACTIVE_NEWLINE ();
	  termination_normal (0);
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
	case '\0':		/* C-@ */
	  if (errno != 0)
	  {
	    /* IO problems, assume everything scrod. */
	    fprintf (stderr, "Problems reading keyboard input -- Exitting.\n");
	    termination_eof ();
	  }
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
DEFUN (invoke_soft_reset, (name), char * name)
{
  soft_reset ();
  /*NOTREACHED*/
}

static void
DEFUN (reset_query, (scp), SIGCONTEXT_T * scp)
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
	case '\0':
	  /* IO problems, assume everything scrod. */
	  fprintf (stderr, "Problems reading keyboard input -- exitting.\n");
	  termination_eof ();
	case 'D':
	  SET_CRITICAL_SECTION_HOOK (invoke_soft_reset);
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

#define USERIO_READ_LINE_OK		0
#define USERIO_READ_LINE_TOO_LONG	1
#define USERIO_READ_LINE_INPUT_FAILED	2

static int
DEFUN (userio_read_line, (line, size), char * line AND int size)
{
  int result = USERIO_READ_LINE_TOO_LONG;
  transaction_begin ();
  userio_buffered_input ();	/* transaction_record_action here */
  {
    char * scan = line;
    char * end = (line + size);
    while (scan < end)
    {
      char c = (userio_read_char ());
      if ((c == '\0') && (errno != 0))
      {
	/* IO problems, assume everything scrod. */
	result = USERIO_READ_LINE_INPUT_FAILED;
	break;
      }
      if (c == '\n')
	c = '\0';
      (*scan) = c;
      if (c == '\0')
      {
	result = USERIO_READ_LINE_OK;
	break;
      }
      scan += 1;
    }
  }
  transaction_commit ();
  return (result);
}

static void
DEFUN_VOID (examine_memory)
{
  char input_string [256];
  fputs ("Enter location to examine (0x prefix for hex): ", stdout);
  fflush (stdout);
  if ((userio_read_line (&input_string[0], (sizeof (input_string))))
      == USERIO_READ_LINE_INPUT_FAILED)
  {
    fprintf (stderr, "Problems reading keyboard input -- exiting.\n");
    termination_eof ();
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

void
DEFUN (eta_fclose, (stream), PTR stream)
{
  (void) (fclose ((FILE *) stream));
  return;
}

static void
DEFUN_VOID (interactive_back_trace)
{
  char input_string [256];
  fputs ("Enter the stack trace filename (default: terminal): ", stdout);
  fflush (stdout);
  if ((userio_read_line (&input_string[0], (sizeof (input_string))))
      == USERIO_READ_LINE_INPUT_FAILED)
  {
    fprintf (stderr, "Problems reading keyboard input -- exiting.\n");
    termination_eof ();
  }
  INTERACTIVE_NEWLINE ();
  if ((strlen (&input_string[0])) == 0)
    debug_back_trace (console_output);
  else
  {
    transaction_begin ();
    {
      FILE * to_dump = (fopen (&input_string[0], "w"));
      if (to_dump == ((FILE *) NULL))
      {
	outf_error ("Error opening \"%s\".\n", (&input_string[0]));
	transaction_abort ();
	return;
      }
      transaction_record_action (tat_always,
				 eta_fclose,
				 ((PTR) to_dump));
      outf_console ("Writing the stack trace to file \"%s\" -- ",
                    &input_string[0]);
      outf_flush_console ();
      debug_back_trace ((outf_channel) to_dump);
      outf_console ("Done.\n");
      outf_flush_console ();
    }
    transaction_commit ();
  }
  return;
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

#if defined(sonyrisc) && defined(HAVE_GRANTPT)
/* Sony NEWS-OS 5.0.2 has a nasty bug because `sigaction' maintains a
   table which contains the signal handlers, and passes
   `sigaction_handler' to the kernel in place of any handler's
   address.  Unfortunately, `signal' doesn't know about this table, so
   it returns `sigaction_handler' as its value, which can subsequently
   get passed back to `sigaction' and stored in the table.  Once
   stored in the table, this causes an infinite recursion, which kills
   the process (with SIGSEGV) when the stack exceeds the allowable
   amount of virtual memory.

   This problem would not be an issue, because Scheme deliberately
   doesn't mix the use of `sigaction' with `signal', except that the
   last release of 5.0.2 (baseline 31.1) calls `signal' from
   `grantpt'.  So, the following patch overrides the built-in version
   of `signal' with one that coexists safely with `sigaction'.

   This is not a "correct" implementation of `signal' -- it is one
   that reinstalls the handlers using the same options that we do in
   this file.  */

Tsignal_handler
DEFUN (signal, (signo, handler),
       int signo AND
       Tsignal_handler handler)
{
  struct sigaction act;
  struct sigaction oact;

  (SIGACT_HANDLER (&act)) = handler;
  UX_sigemptyset (& (act . sa_mask));
  (act . sa_flags) = (SA_RESETHAND | SA_NODEFER);
  if (handler == ((Tsignal_handler) SIG_IGN))
    (act . sa_flags) |= SA_NOCLDWAIT;
  if ((UX_sigaction (signo, (&act), (&oact))) < 0)
    return (SIG_ERR);
  else
    return (SIGACT_HANDLER (&oact));
}

/* It is best to reinstall the SIGCHLD handler after `grantpt' is
   called because that guarantees that the flags are correct.  */

void
DEFUN_VOID (sony_block_sigchld)
{
  sighold (SIGCHLD);
}

void
DEFUN_VOID (sony_unblock_sigchld)
{
  INSTALL_HANDLER (SIGCHLD, sighnd_dead_subprocess);
  sigrelse (SIGCHLD);
}

#endif /* sonyrisc and HAVE_GRANTPT */
