/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxctty.c,v 1.7 1991/03/01 00:56:02 cph Exp $

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
#include "osctty.h"

/* If `ctty_fildes' is nonnegative, it is an open file descriptor for
   the controlling terminal of the process.

   If `ctty_fildes' is negative, Scheme should not alter the control
   terminal's settings. */
static int ctty_fildes;

/* This flag says whether Scheme was in the foreground when it was
   last entered.  Provided that no other process forces Scheme out of
   the foreground, it will remain in the foreground until it exits or
   is stopped.

   If `scheme_in_foreground' is zero, Scheme should not alter the
   control terminal's settings, nor should it alter the settings of
   stdin, stdout, or stderr if they are terminals. */
int scheme_in_foreground;

/* This flag, set during initialization, says whether we are
   permitted to change the settings of the control terminal. */
static int permit_ctty_control;

/* Original states of the control terminal, stdin, and stdout when
   Scheme was last continued or stopped, respectively.  If the
   corresponding `_recorded' flag is zero, then no information is
   saved. */

struct terminal_state_recording
{
  int fd;
  int recorded_p;
  Ttty_state state;
};

static struct terminal_state_recording outside_ctty_state;
static struct terminal_state_recording outside_stdin_state;
static struct terminal_state_recording outside_stdout_state;
static struct terminal_state_recording inside_ctty_state;
static struct terminal_state_recording inside_stdin_state;
static struct terminal_state_recording inside_stdout_state;

static void EXFUN (ctty_update_interrupt_chars, (void));

static void
DEFUN (save_external_state, (s), struct terminal_state_recording * s)
{
  (s -> recorded_p) =
    (scheme_in_foreground
     && (isatty (s -> fd))
     && ((UX_terminal_get_state ((s -> fd), (& (s -> state)))) >= 0));
}

static void
DEFUN (restore_external_state, (s), struct terminal_state_recording * s)
{
  if (s -> recorded_p)
    {
      UX_terminal_set_state ((s -> fd), (& (s -> state)));
      (s -> recorded_p) = 0;
    }
}

void
DEFUN (save_internal_state, (s, es),
       struct terminal_state_recording * s AND
       struct terminal_state_recording * es)
{
  /* Don't do anything unless we have a recording of the external
     state.  Otherwise, we should preserve the previous recording of
     the internal state, if any. */
  if (es -> recorded_p)
    (s -> recorded_p) =
      ((UX_terminal_get_state ((s -> fd), (& (s -> state)))) >= 0);
}

static void
DEFUN (restore_internal_state, (s, es),
       struct terminal_state_recording * s AND
       struct terminal_state_recording * es)
{
  /* When we recorded the internal state, we had a recording of the
     external state.  But since we've stopped Scheme and restarted it,
     we may no longer have a current recording of the external state.
     If we don't, then we can't restore the internal state.

     The usual reason that we don't have a recording is that Scheme is
     in the background.  In that case it would be nice to preserve the
     previous internal state until we go back to the foreground.  But
     doing that transparently would also require tracking all
     attempted state changes in the recording, which is a pain.  So if
     we can't restore the internal state, we just thrown it away. */
  if (s -> recorded_p)
    {
      if (es -> recorded_p)
	UX_terminal_set_state ((s -> fd), (& (s -> state)));
      (s -> recorded_p) = 0;
    }
}

void
DEFUN_VOID (UX_ctty_save_external_state)
{
  if (permit_ctty_control && (ctty_fildes >= 0))
    {
      pid_t pgrp_id = (UX_tcgetpgrp (ctty_fildes));
      scheme_in_foreground =
	((pgrp_id < 0)
	 /* If no job control, assume we're in foreground. */
	 ? (errno == ENOSYS)
	 : ((UX_getpgrp ()) == pgrp_id));
    }
  else
    scheme_in_foreground = 0;
  save_external_state (&outside_ctty_state);
  save_external_state (&outside_stdin_state);
  save_external_state (&outside_stdout_state);
}

void
DEFUN_VOID (UX_ctty_restore_external_state)
{
  restore_external_state (&outside_ctty_state);
  restore_external_state (&outside_stdin_state);
  restore_external_state (&outside_stdout_state);
}

void
DEFUN_VOID (UX_ctty_save_internal_state)
{
  save_internal_state ((&inside_ctty_state), (&outside_ctty_state));
  save_internal_state ((&inside_stdin_state), (&outside_stdin_state));
  save_internal_state ((&inside_stdout_state), (&outside_stdout_state));
}

void
DEFUN_VOID (UX_ctty_restore_internal_state)
{
  int do_update =
    ((inside_ctty_state . recorded_p)
     && (outside_ctty_state . recorded_p));
  restore_internal_state ((&inside_ctty_state), (&outside_ctty_state));
  restore_internal_state ((&inside_stdin_state), (&outside_stdin_state));
  restore_internal_state ((&inside_stdout_state), (&outside_stdout_state));
  if (do_update)
    ctty_update_interrupt_chars ();
}

int
DEFUN_VOID (OS_ctty_interrupt_control)
{
  return (outside_ctty_state . recorded_p);
}

int
DEFUN (UX_terminal_control_ok, (fd), int fd)
{
  return
    ((fd == STDIN_FILENO)
     ? (outside_stdin_state . recorded_p)
     : (fd == STDOUT_FILENO)
     ? (outside_stdout_state . recorded_p)
     : 1);
}

/* Keyboard Interrupt Characters */

typedef struct
{
  cc_t quit;
  cc_t intrpt;
  cc_t tstp;
  cc_t dtstp;
} Tinterrupt_chars;

static Tinterrupt_enables current_interrupt_enables;
static Tinterrupt_chars current_interrupt_chars;

#define DEFAULT_SIGQUIT_CHAR	((cc_t) '\003') /* ^C */
#define DEFAULT_SIGINT_CHAR	((cc_t) '\007') /* ^G */
#define DEFAULT_SIGTSTP_CHAR	((cc_t) '\032') /* ^Z */

#define KEYBOARD_QUIT_INTERRUPT		0x1
#define KEYBOARD_INTRPT_INTERRUPT	0x2
#define KEYBOARD_TSTP_INTERRUPT		0x4
#define KEYBOARD_ALL_INTERRUPTS		0x7

cc_t
DEFUN_VOID (OS_ctty_quit_char)
{
  return (current_interrupt_chars . quit);
}

cc_t
DEFUN_VOID (OS_ctty_int_char)
{
  return (current_interrupt_chars . intrpt);
}

cc_t
DEFUN_VOID (OS_ctty_tstp_char)
{
  return (current_interrupt_chars . tstp);
}

cc_t
DEFUN_VOID (OS_ctty_disabled_char)
{
  return ((ctty_fildes >= 0) ? (UX_PC_VDISABLE (ctty_fildes)) : '\377');
}

int
DEFUN_VOID (OS_ctty_fd)
{
  return (ctty_fildes);
}

#if 0
/* not currently used */
static void
DEFUN (ctty_get_interrupt_chars, (ic), Tinterrupt_chars * ic)
{
  Ttty_state s;
  if ((UX_terminal_get_state (ctty_fildes, (&s))) == 0)
    {
#ifdef HAVE_TERMIOS
      (ic -> quit) = ((s . tio . c_cc) [VQUIT]);
      (ic -> intrpt) = ((s . tio . c_cc) [VINTR]);
      (ic -> tstp) = ((s . tio . c_cc) [VSUSP]);

#ifdef _HPUX
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#else /* not _HPUX */
#ifdef _ULTRIX
      (ic -> dtstp) = ((s . tio . c_cc) [VDSUSP]);
#endif /* _ULTRIX */
#endif /* not _HPUX */

#else /* not HAVE_TERMIOS */
#ifdef HAVE_TERMIO

      (ic -> quit) = ((s . tio . c_cc) [VQUIT]);
      (ic -> intrpt) = ((s . tio . c_cc) [VINTR]);
#ifdef HAVE_BSD_JOB_CONTROL
      (ic -> tstp) = (s . ltc . t_suspc);
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#else /* not HAVE_BSD_JOB_CONTROL */
      {
	cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
	(ic -> tstp) = disabled_char;
	(ic -> dtstp) = disabled_char;
      }
#endif /* not HAVE_BSD_JOB_CONTROL */

#else /* not HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER

      (ic -> quit) = (s . tc . t_quitc);
      (ic -> intrpt) = (s . tc . t_intrc);
#ifdef HAVE_BSD_JOB_CONTROL
      (ic -> tstp) = (s . ltc . t_suspc);
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#else /* not HAVE_BSD_JOB_CONTROL */
      {
	cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
	(ic -> tstp) = disabled_char;
	(ic -> dtstp) = disabled_char;
      }
#endif /* not HAVE_BSD_JOB_CONTROL */

#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIO */
#endif /* HAVE_TERMIOS */
    }
  else
    {
      cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
      (ic -> quit) = disabled_char;
      (ic -> intrpt) = disabled_char;
      (ic -> tstp) = disabled_char;
      (ic -> dtstp) = disabled_char;
    }
}
#endif /* 0 */

static void
DEFUN (ctty_set_interrupt_chars, (ic), Tinterrupt_chars * ic)
{
  Ttty_state s;
  if ((UX_terminal_get_state (ctty_fildes, (&s))) == 0)
    {
#ifdef HAVE_TERMIOS
      ((s . tio . c_cc) [VQUIT]) = (ic -> quit);
      ((s . tio . c_cc) [VINTR]) = (ic -> intrpt);
      ((s . tio . c_cc) [VSUSP]) = (ic -> tstp);
#ifdef _HPUX
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#else /* not _HPUX */
#ifdef _ULTRIX
      ((s . tio . c_cc) [VDSUSP]) = (ic -> dtstp);
#endif /* _ULTRIX */
#endif /* not _HPUX */

#else /* not HAVE_TERMIOS */
#ifdef HAVE_TERMIO

      ((s . tio . c_cc) [VQUIT]) = (ic -> quit);
      ((s . tio . c_cc) [VINTR]) = (ic -> intrpt);
#ifdef HAVE_BSD_JOB_CONTROL
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#endif

#else /* not HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER

      (s . tc . t_quitc) = (ic -> quit);
      (s . tc . t_intrc) = (ic -> intrpt);
#ifdef HAVE_BSD_JOB_CONTROL
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#endif

#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIO */
#endif /* HAVE_TERMIOS */
      UX_terminal_set_state (ctty_fildes, (&s));
    }
}

static void
DEFUN_VOID (ctty_update_interrupt_chars)
{
  if (outside_ctty_state . recorded_p)
    {
      cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
      /* Must split declaration and assignment because some compilers
	 do not permit aggregate initializers. */
      Tinterrupt_chars active_interrupt_chars;
      active_interrupt_chars = current_interrupt_chars;
      if ((current_interrupt_enables & KEYBOARD_QUIT_INTERRUPT) == 0)
	(active_interrupt_chars . quit) = disabled_char;
      if ((current_interrupt_enables & KEYBOARD_INTRPT_INTERRUPT) == 0)
	(active_interrupt_chars . intrpt) = disabled_char;
      if ((current_interrupt_enables & KEYBOARD_TSTP_INTERRUPT) == 0)
	(active_interrupt_chars . tstp) = disabled_char;
      (active_interrupt_chars . dtstp) = disabled_char;
      ctty_set_interrupt_chars (&active_interrupt_chars);
    }
}

void
DEFUN (OS_ctty_get_interrupt_enables, (mask), Tinterrupt_enables * mask)
{
  (*mask) = current_interrupt_enables;
}

void
DEFUN (OS_ctty_set_interrupt_enables, (mask), Tinterrupt_enables * mask)
{
  current_interrupt_enables = (*mask);
  ctty_update_interrupt_chars ();
}

void
DEFUN (OS_ctty_set_interrupt_chars, (quit_char, int_char, tstp_char),
       cc_t quit_char AND
       cc_t int_char AND
       cc_t tstp_char)
{
  (current_interrupt_chars . quit) = quit_char;
  (current_interrupt_chars . intrpt) = int_char;
  (current_interrupt_chars . tstp) = tstp_char;
  ctty_update_interrupt_chars ();
}

void
DEFUN (UX_initialize_ctty, (interactive), int interactive)
{
  {
    char * tty = (UX_ctermid (0));
    ctty_fildes =
      (((tty == 0) || ((tty[0]) == 0))
       ? (-1)
       : (UX_open (tty, O_RDWR, 0)));
  }
  permit_ctty_control = interactive;
  (inside_ctty_state . fd) = (outside_ctty_state . fd) = ctty_fildes;
  (inside_stdin_state . fd) = (outside_stdin_state . fd) = STDIN_FILENO;
  (inside_stdout_state . fd) = (outside_stdout_state . fd) = STDOUT_FILENO;
  UX_ctty_save_external_state ();
  (inside_ctty_state . recorded_p) = 0;
  (inside_stdin_state . recorded_p) = 0;
  (inside_stdout_state . recorded_p) = 0;
  (current_interrupt_chars . quit) = DEFAULT_SIGQUIT_CHAR;
  (current_interrupt_chars . intrpt) = DEFAULT_SIGINT_CHAR;
  (current_interrupt_chars . tstp) = DEFAULT_SIGTSTP_CHAR;
  (current_interrupt_chars . dtstp) = (UX_PC_VDISABLE (ctty_fildes));
  current_interrupt_enables = KEYBOARD_ALL_INTERRUPTS;
  if (outside_ctty_state . recorded_p)
    ctty_set_interrupt_chars (&current_interrupt_chars);
}
