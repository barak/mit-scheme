/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxterm.c,v 1.1 1990/06/20 19:37:38 cph Exp $

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
#include "uxterm.h"
#include "uxio.h"

struct terminal_state
{
  int buffer;
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
  char saved_echo;
  cc_t saved_vmin;
  cc_t saved_vtime;
#endif
};

static struct terminal_state * terminal_table;
#define TERMINAL_BUFFER(channel) ((terminal_table[(channel)]) . buffer)
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
#define TERMINAL_ECHO(channel) ((terminal_table[(channel)]) . saved_echo)
#define TERMINAL_VMIN(channel) ((terminal_table[(channel)]) . saved_vmin)
#define TERMINAL_VTIME(channel) ((terminal_table[(channel)]) . saved_vtime)
#endif

void
DEFUN_VOID (UX_initialize_terminals)
{
  terminal_table =
    (UX_malloc (OS_channel_table_size * (sizeof (struct terminal_state))));
  if (terminal_table == 0)
    {
      fprintf (stderr, "\nUnable to allocate terminal table.\n");
      fflush (stderr);
      termination_init_error ();
    }
}

/* This is called from the file-opening code. */
void
DEFUN (terminal_open, (channel), Tchannel channel)
{
  (TERMINAL_BUFFER (channel)) = (-1);
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
  {
    Ttty_state s;
#ifdef HAVE_TERMIOS
    struct termios * tio = (&s);
#else
    struct termio * tio = (& (s . tio));
#endif
    get_terminal_state (channel, (&s));
    (TERMINAL_ECHO (channel)) = (((tio -> c_lflag) & ECHO) != 0);
    (TERMINAL_VMIN (channel)) = ((tio -> c_cc) [VMIN]);
    (TERMINAL_VTIME (channel)) = ((tio -> c_cc) [VTIME]);
  }
#endif
}

int
DEFUN (OS_terminal_read_char, (channel), Tchannel channel)
{
  {
    int c = (TERMINAL_BUFFER (channel));
    if (c >= 0)
      {
	(TERMINAL_BUFFER (channel)) = (-1);
	return (c);
      }
  }
  return (OS_channel_read_char_interruptably (channel));
}

int
DEFUN (terminal_state_buffered_p, (s), Ttty_state * s)
{
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
#ifdef HAVE_TERMIOS
  struct termios * tio = s;
#else
  struct termio * tio = (& (s -> tio));
#endif
  return (((tio -> c_lflag) & ICANON) != 0);
#else /* not HAVE_TERMIOS nor HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER
  return (((s -> sg . sg_flags) & CBREAK) == 0);
#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIOS or HAVE_TERMIO */
}

void
DEFUN (terminal_state_buffered, (s, channel),
       Ttty_state * s AND
       Tchannel channel)
{
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
#ifdef HAVE_TERMIOS
  struct termios * tio = s;
#else
  struct termio * tio = (& (s -> tio));
#endif
  (tio -> c_lflag) |= ICANON;
  if (TERMINAL_ECHO (channel))
    (tio -> c_lflag) |= ECHO;
  ((tio -> c_cc) [VMIN]) = (TERMINAL_VMIN (channel));
  ((tio -> c_cc) [VTIME]) = (TERMINAL_VTIME (channel));
#else /* not HAVE_TERMIOS nor HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER
  (s -> sg . sg_flags) &=~ CBREAK;
#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIOS or HAVE_TERMIO */
}

void
DEFUN (terminal_state_nonbuffered, (s, polling),
       Ttty_state * s AND int polling)
{
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
#ifdef HAVE_TERMIOS
  struct termios * tio = s;
#else
  struct termio * tio = (& (s -> tio));
#endif
  (tio -> c_lflag) &=~ (ICANON | ECHO);
  ((tio -> c_cc) [VMIN]) = (polling ? 0 : 1);
  ((tio -> c_cc) [VTIME]) = 0;
#else /* not HAVE_TERMIOS nor HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER
  (s -> sg . sg_flags) |= CBREAK;
#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIOS or HAVE_TERMIO */
}

void
DEFUN (terminal_state_raw, (s), Ttty_state * s)
{
#if defined(HAVE_TERMIOS) || defined(HAVE_TERMIO)
#ifdef HAVE_TERMIOS
  struct termios * tio = s;
#else
  struct termio * tio = (& (s -> tio));
#endif
  (tio -> c_lflag) &=~ (ICANON | ECHO | ISIG);
  ((tio -> c_cc) [VMIN]) = 1;
  ((tio -> c_cc) [VTIME]) = 0;
#else /* not HAVE_TERMIOS nor HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER
  (s -> sg . sg_flags) |= RAW;
#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIOS or HAVE_TERMIO */
}

void
DEFUN (get_terminal_state, (channel, s), Tchannel channel AND Ttty_state * s)
{
  STD_VOID_SYSTEM_CALL
    ("tty_get_state",
     (UX_terminal_get_state ((CHANNEL_DESCRIPTOR (channel)), s)));
}

void
DEFUN (set_terminal_state, (channel, s), Tchannel channel AND Ttty_state * s)
{
  STD_VOID_SYSTEM_CALL
    ("tty_set_state",
     (UX_terminal_set_state ((CHANNEL_DESCRIPTOR (channel)), s)));
}

struct terminal_state_record
{
  Tchannel channel;
  Ttty_state state;
};

static void
DEFUN (restore_terminal_state, (ap), PTR ap)
{
  set_terminal_state ((((struct terminal_state_record *) ap) -> channel),
		      (& (((struct terminal_state_record *) ap) -> state)));
}

Ttty_state *
DEFUN (preserve_terminal_state, (channel), Tchannel channel)
{
  struct terminal_state_record * record =
    (dstack_alloc (sizeof (struct terminal_state_record)));
  (record -> channel) = channel;
  get_terminal_state (channel, (& (record -> state)));
  transaction_record_action (tat_always, restore_terminal_state, record);
  return (& (record -> state));
}

#ifdef HAVE_FIONREAD
/* This covers HAVE_BSD_TTY_DRIVER and some others. */

int
DEFUN (OS_terminal_char_ready_p, (channel, delay),
       Tchannel channel AND clock_t delay)
{
  clock_t limit;
  if (delay > 0)
    limit = ((OS_real_time_clock ()) + delay);
  while (1)
    {
      long n;
      int scr;
      INTERRUPTABLE_EXTENT
	(scr, (UX_ioctl ((CHANNEL_DESCRIPTOR (channel)), FIONREAD, (&n))));
      if (scr < 0)
	UX_prim_check_errno ("ioctl_FIONREAD");
      else if (n > 0)
	return (1);
      else if ((delay <= 0) || ((OS_real_time_clock ()) >= limit))
	return (0);
    }
}

#else /* not HAVE_FIONREAD */
#if defined(HAVE_TERMIO) || defined(HAVE_TERMIOS)

int
DEFUN (OS_terminal_char_ready_p, (channel, delay),
       Tchannel channel AND clock_t delay)
{
  clock_t limit;
  if (delay > 0)
    limit = ((OS_real_time_clock ()) + delay);
  transaction_begin ();
  {
    /* Must split declaration and assignment because some compilers
       do not permit aggregate initializers. */
    Ttty_state s;
    s = (* (preserve_terminal_state (channel)));
    terminal_state_nonbuffered ((&s), 1);
    set_terminal_state (channel, (&s));
  }
  while (1)
    {
      unsigned char c;
      int nread;
      INTERRUPTABLE_EXTENT
	(nread, (UX_read ((CHANNEL_DESCRIPTOR (channel)), (&c), 1)));
      if (nread < 0)
	UX_prim_check_errno ("read");
      else if (nread == 1)
	{
	  (TERMINAL_BUFFER (channel)) = c;
	  transaction_commit ();
	  return (1);
	}
      if ((delay <= 0) || ((OS_real_time_clock ()) >= limit))
	{
	  transaction_commit ();
	  return (0);
	}
    }
}

#endif /* HAVE_TERMIO or HAVE_TERMIOS */
#endif /* HAVE_FIONREAD */

int
DEFUN (OS_terminal_buffered_p, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  return (terminal_state_buffered_p (&s));
}

void
DEFUN (OS_terminal_buffered, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_buffered ((&s), channel);
  set_terminal_state (channel, (&s));
}

void
DEFUN (OS_terminal_nonbuffered, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_nonbuffered ((&s), 0);
  set_terminal_state (channel, (&s));
}

void
DEFUN (OS_terminal_flush_input, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    ("tcflush", (UX_tcflush ((CHANNEL_DESCRIPTOR (channel)), TCIFLUSH)));
}

void
DEFUN (OS_terminal_flush_output, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    ("tcflush", (UX_tcflush ((CHANNEL_DESCRIPTOR (channel)), TCOFLUSH)));
}

void
DEFUN (OS_terminal_drain_output, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    ("tcdrain", (UX_tcdrain (CHANNEL_DESCRIPTOR (channel))));
}

#ifdef HAVE_PTYS

/* Open an available pty, putting channel in (*ptyv),
   and return the file name of the pty.  Return 0 if none available.  */

CONST char *
DEFUN (OS_open_pty_master, (master_fd, master_fname),
       Tchannel * master_fd AND
       CONST char ** master_fname)
{
  struct stat stb;
  register int c;
  register int i;
  char master_name [24];
  char slave_name [24];
  int fd;
#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; (c <= 'z'); c += 1)
    for (i = 0; (i < 16); i += 1)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (master_name, "/dev/pty%c%x", c, i);
#endif
      retry_open:
	fd = (UX_open (master_name, O_RDWR, 0));
	if (fd < 0)
	  {
	    if (errno == EACCES)
	      return (0);
	    if (errno != EINTR)
	      continue;
	    deliver_pending_interrupts ();
	    goto retry_open;
	  }
	/* check to make certain that both sides are available
	   this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	PTY_TTY_NAME_SPRINTF
#else
	sprintf (slave_name, "/dev/tty%c%x", c, i);
#endif
	if ((UX_access (slave_name, (R_OK | W_OK))) < 0)
	  {
	    UX_close (fd);
	    continue;
	  }
	MAKE_CHANNEL (fd, channel_type_pty_master, (*master_fd) =);
	(*master_fname) = master_name;
	return (slave_name);
      }
  return (0);
}

void
DEFUN (OS_pty_master_send_signal, (channel, sig), Tchannel channel AND int sig)
{
#ifdef _HPUX
  STD_VOID_SYSTEM_CALL
    ("ioctl_TIOCSIGSEND",
     (UX_ioctl ((CHANNEL_DESCRIPTOR (channel)), TIOCSIGSEND, sig)));
#else /* not _HPUX */
#ifdef HAVE_BSD_JOB_CONTROL
  int fd = (CHANNEL_DESCRIPTOR (channel));
  int gid;
  STD_VOID_SYSTEM_CALL ("ioctl_TIOCGPGRP", (UX_ioctl (fd, TIOCGPGRP, (&gid))));
  STD_VOID_SYSTEM_CALL ("kill", (UX_kill ((-gid), sig)));
#else /* not HAVE_BSD_JOB_CONTROL */
  error_unimplemented_primitive ();
#endif /* HAVE_BSD_JOB_CONTROL */
#endif /* _HPUX */
}

#else /* not HAVE_PTYS */

CONST char *
DEFUN (OS_open_pty_master, (master_fd, master_fname),
       Tchannel * master_fd AND
       CONST char ** master_fname)
{
  return (0);
}

void
DEFUN (OS_pty_master_send_signal, (channel, sig), Tchannel channel AND int sig)
{
  error_unimplemented_primitive ();
}

#endif /* HAVE_PTYS */
