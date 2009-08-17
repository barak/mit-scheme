/* -*-C-*-

$Id: e35e4f714e62d37ff3f9de9aebba90f8853dd464 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#include "ux.h"
#include "osctty.h"
#include "ossig.h"

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
  int flags;
};

static struct terminal_state_recording outside_ctty_state;
static struct terminal_state_recording outside_stdin_state;
static struct terminal_state_recording outside_stdout_state;
static struct terminal_state_recording inside_ctty_state;
static struct terminal_state_recording inside_stdin_state;
static struct terminal_state_recording inside_stdout_state;

static void ctty_update_interrupt_chars (void);

static int
get_terminal_state (int fd, Ttty_state * s)
{
  while (1)
    {
      int scr = (UX_terminal_get_state (fd, s));
      if ((scr >= 0) || (errno != EINTR))
	return (scr);
    }
}

static int
set_terminal_state (int fd, Ttty_state * s)
{
  while (1)
    {
      int scr = (UX_terminal_set_state (fd, s));
      if ((scr >= 0) || (errno != EINTR))
	return (scr);
    }
}


static int
get_flags (int fd, int * flags)
{
#ifdef FCNTL_NONBLOCK
  while (1)
    {
      int scr = (UX_fcntl (fd, F_GETFL, 0));
      if (scr >= 0)
	{
	  (*flags) = scr;
	  return (0);
	}
      if (errno != EINTR)
	return (-1);
    }
#else
  return (0);
#endif
}

static int
set_flags (int fd, int * flags)
{
#ifdef FCNTL_NONBLOCK
  while (1)
    {
      int scr = (UX_fcntl (fd, F_SETFL, (*flags)));
      if ((scr >= 0) || (errno != EINTR))
	return (scr);
    }
#else
  return (0);
#endif
}

static void
save_external_state (struct terminal_state_recording * s)
{
  (s -> recorded_p) =
    (scheme_in_foreground
     && (isatty (s -> fd))
     && ((get_terminal_state ((s -> fd), (& (s -> state)))) >= 0)
     && ((get_flags ((s -> fd), (& (s -> flags)))) >= 0));
}

static void
restore_external_state (struct terminal_state_recording * s)
{
  if (s -> recorded_p)
    {
      set_terminal_state ((s -> fd), (& (s -> state)));
      set_flags ((s -> fd), (& (s -> flags)));
      (s -> recorded_p) = 0;
    }
}

void
save_internal_state (struct terminal_state_recording * s,
       struct terminal_state_recording * es)
{
  /* Don't do anything unless we have a recording of the external
     state.  Otherwise, we should preserve the previous recording of
     the internal state, if any. */
  if (es -> recorded_p)
    (s -> recorded_p) =
      (((get_terminal_state ((s -> fd), (& (s -> state)))) >= 0)
       && ((get_flags ((s -> fd), (& (s -> flags)))) >= 0));
}

static void
restore_internal_state (struct terminal_state_recording * s,
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
	{
	  set_terminal_state ((s -> fd), (& (s -> state)));
	  set_flags ((s -> fd), (& (s -> flags)));
	}
      (s -> recorded_p) = 0;
    }
}

void
UX_ctty_save_external_state (void)
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
UX_ctty_restore_external_state (void)
{
  restore_external_state (&outside_ctty_state);
  restore_external_state (&outside_stdin_state);
  restore_external_state (&outside_stdout_state);
}

void
UX_ctty_save_internal_state (void)
{
  save_internal_state ((&inside_ctty_state), (&outside_ctty_state));
  save_internal_state ((&inside_stdin_state), (&outside_stdin_state));
  save_internal_state ((&inside_stdout_state), (&outside_stdout_state));
}

void
UX_ctty_restore_internal_state (void)
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
OS_ctty_interrupt_control (void)
{
  return (outside_ctty_state . recorded_p);
}

int
UX_terminal_control_ok (int fd)
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
OS_ctty_quit_char (void)
{
  return (current_interrupt_chars . quit);
}

cc_t
OS_ctty_int_char (void)
{
  return (current_interrupt_chars . intrpt);
}

cc_t
OS_ctty_tstp_char (void)
{
  return (current_interrupt_chars . tstp);
}

cc_t
OS_ctty_disabled_char (void)
{
  return ((ctty_fildes >= 0) ? (UX_PC_VDISABLE (ctty_fildes)) : '\377');
}

int
OS_ctty_fd (void)
{
  return (ctty_fildes);
}

#if 0

/* not currently used */
static void
ctty_get_interrupt_chars (Tinterrupt_chars * ic)
{
  Ttty_state s;
  if ((get_terminal_state (ctty_fildes, (&s))) == 0)
    {
#ifdef HAVE_TERMIOS_H
      (ic -> quit) = ((s . tio . c_cc) [VQUIT]);
      (ic -> intrpt) = ((s . tio . c_cc) [VINTR]);
      (ic -> tstp) = ((s . tio . c_cc) [VSUSP]);

#ifdef VDSUSP
      (ic -> dtstp) = ((s . tio . c_cc) [VDSUSP]);
#else /* not VDSUSP */
#ifdef __HPUX__
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#endif /* __HPUX__ */
#endif /* not VDSUSP */

#else /* not HAVE_TERMIOS_H */
#ifdef HAVE_TERMIO_H

      (ic -> quit) = ((s . tio . c_cc) [VQUIT]);
      (ic -> intrpt) = ((s . tio . c_cc) [VINTR]);
#ifdef HAVE_STRUCT_LTCHARS
      (ic -> tstp) = (s . ltc . t_suspc);
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#else /* not HAVE_STRUCT_LTCHARS */
      {
	cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
	(ic -> tstp) = disabled_char;
	(ic -> dtstp) = disabled_char;
      }
#endif /* not HAVE_STRUCT_LTCHARS */

#else /* not HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

      (ic -> quit) = (s . tc . t_quitc);
      (ic -> intrpt) = (s . tc . t_intrc);
#ifdef HAVE_STRUCT_LTCHARS
      (ic -> tstp) = (s . ltc . t_suspc);
      (ic -> dtstp) = (s . ltc . t_dsuspc);
#else /* not HAVE_STRUCT_LTCHARS */
      {
	cc_t disabled_char = (UX_PC_VDISABLE (ctty_fildes));
	(ic -> tstp) = disabled_char;
	(ic -> dtstp) = disabled_char;
      }
#endif /* not HAVE_STRUCT_LTCHARS */

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIO_H */
#endif /* HAVE_TERMIOS_H */
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
ctty_set_interrupt_chars (Tinterrupt_chars * ic)
{
  Ttty_state s;
  if ((get_terminal_state (ctty_fildes, (&s))) == 0)
    {
#ifdef HAVE_TERMIOS_H
      ((s . tio . c_cc) [VQUIT]) = (ic -> quit);
      ((s . tio . c_cc) [VINTR]) = (ic -> intrpt);
      ((s . tio . c_cc) [VSUSP]) = (ic -> tstp);
#ifdef VDSUSP
      ((s . tio . c_cc) [VDSUSP]) = (ic -> dtstp);
#else /* not VDSUSP */
#ifdef __HPUX__
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#endif /* __HPUX__ */
#endif /* not VDSUSP */

#else /* not HAVE_TERMIOS_H */
#ifdef HAVE_TERMIO_H

      ((s . tio . c_cc) [VQUIT]) = (ic -> quit);
      ((s . tio . c_cc) [VINTR]) = (ic -> intrpt);
#ifdef HAVE_STRUCT_LTCHARS
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#endif

#else /* not HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

      (s . tc . t_quitc) = (ic -> quit);
      (s . tc . t_intrc) = (ic -> intrpt);
#ifdef HAVE_STRUCT_LTCHARS
      (s . ltc . t_suspc) = (ic -> tstp);
      (s . ltc . t_dsuspc) = (ic -> dtstp);
#endif

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIO_H */
#endif /* HAVE_TERMIOS_H */
      set_terminal_state (ctty_fildes, (&s));
    }
}

static void
ctty_update_interrupt_chars (void)
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
OS_ctty_get_interrupt_enables (Tinterrupt_enables * mask)
{
  (*mask) = current_interrupt_enables;
}

void
OS_ctty_set_interrupt_enables (Tinterrupt_enables * mask)
{
  current_interrupt_enables = (*mask);
  ctty_update_interrupt_chars ();
}

#if 0

void
OS_ctty_set_interrupt_chars (cc_t quit_char,
       cc_t int_char,
       cc_t tstp_char)
{
  (current_interrupt_chars . quit) = quit_char;
  (current_interrupt_chars . intrpt) = int_char;
  (current_interrupt_chars . tstp) = tstp_char;
  ctty_update_interrupt_chars ();
}
#endif

unsigned int
OS_ctty_num_int_chars (void)
{
  return (3);
}

cc_t *
OS_ctty_get_int_chars (void)
{
  static cc_t int_chars [3];

  int_chars[0] = current_interrupt_chars.quit;
  int_chars[1] = current_interrupt_chars.intrpt;
  int_chars[2] = current_interrupt_chars.tstp;
  return (& int_chars [0]);
}

void
OS_ctty_set_int_chars (cc_t * int_chars)
{
  current_interrupt_chars.quit   = int_chars[0];
  current_interrupt_chars.intrpt = int_chars[1];
  current_interrupt_chars.tstp   = int_chars[2];
  ctty_update_interrupt_chars ();
  return;
}

extern enum interrupt_handler OS_signal_quit_handler (void);
extern enum interrupt_handler OS_signal_int_handler (void);
extern enum interrupt_handler OS_signal_tstp_handler (void);
extern void OS_signal_set_interrupt_handlers
  (enum interrupt_handler quit_handler,
    enum interrupt_handler int_handler,
    enum interrupt_handler tstp_handler);

cc_t *
OS_ctty_get_int_char_handlers (void)
{
  static cc_t int_handlers [3];

  int_handlers[0] = ((cc_t) (OS_signal_quit_handler ()));
  int_handlers[1] = ((cc_t) (OS_signal_int_handler ()));
  int_handlers[2] = ((cc_t) (OS_signal_tstp_handler ()));
  return (& int_handlers [0]);
}

void
OS_ctty_set_int_char_handlers (cc_t * int_handlers)
{
  OS_signal_set_interrupt_handlers
    (((enum interrupt_handler) (int_handlers [0])),
     ((enum interrupt_handler) (int_handlers [1])),
     ((enum interrupt_handler) (int_handlers [2])));
  return;
}

void
UX_initialize_ctty (int interactive)
{
  {
    char buffer [L_ctermid];
    char * tty = (UX_ctermid (buffer));
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
