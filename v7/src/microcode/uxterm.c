/* -*-C-*-

$Id: uxterm.c,v 1.31 2004/01/18 06:04:49 cph Exp $

Copyright 1991,1992,1993,1995,1997,2000 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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

#include "ux.h"
#include "uxterm.h"
#include "uxio.h"
#include "ospty.h"
#include "prims.h"

extern long EXFUN (arg_nonnegative_integer, (int));
extern long EXFUN (arg_index_integer, (int, long));

#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
#  ifndef ISTRIP
#    define ISTRIP 0
#  endif
#  ifndef CS8
#    define CS8 0
#  endif
#  ifndef PARENB
#    define PARENB 0
#  endif
#  define TIO(s) (& ((s) -> tio))
#else
#  ifdef HAVE_SGTTY_H
/* LPASS8 is new in 4.3, and makes cbreak mode provide all 8 bits.  */
#    ifndef LPASS8
#      define LPASS8 0
#    endif
#  endif /* HAVE_SGTTY_H */
#endif /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */

struct terminal_state
{
  int buffer;
  Ttty_state state;
};

static struct terminal_state * terminal_table;
#define TERMINAL_BUFFER(channel) ((terminal_table[(channel)]) . buffer)
#define TERMINAL_ORIGINAL_STATE(channel) ((terminal_table[(channel)]) . state)

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

void
DEFUN_VOID (UX_reset_terminals)
{
  UX_free (terminal_table);
  terminal_table = 0;
}

/* This is called from the file-opening code. */
void
DEFUN (terminal_open, (channel), Tchannel channel)
{
  (TERMINAL_BUFFER (channel)) = (-1);
  get_terminal_state (channel, (& (TERMINAL_ORIGINAL_STATE (channel))));
}

void
DEFUN (get_terminal_state, (channel, s), Tchannel channel AND Ttty_state * s)
{
  STD_VOID_SYSTEM_CALL
    (syscall_terminal_get_state,
     (UX_terminal_get_state ((CHANNEL_DESCRIPTOR (channel)), s)));
}

void
DEFUN (set_terminal_state, (channel, s), Tchannel channel AND Ttty_state * s)
{
  extern int EXFUN (UX_terminal_control_ok, (int fd));
  if (UX_terminal_control_ok (CHANNEL_DESCRIPTOR (channel)))
    STD_VOID_SYSTEM_CALL
      (syscall_terminal_set_state,
       (UX_terminal_set_state ((CHANNEL_DESCRIPTOR (channel)), s)));
}

unsigned int
DEFUN (terminal_state_get_ospeed, (s), Ttty_state * s)
{
#ifdef HAVE_TERMIOS_H
  return (cfgetospeed (TIO (s)));
#else
#ifdef HAVE_TERMIO_H
  return (((TIO (s)) -> c_cflag) & CBAUD);
#else
#ifdef HAVE_SGTTY_H
  return (s -> sg . sg_ospeed);
#endif /* HAVE_SGTTY_H */
#endif /* not HAVE_TERMIO_H */
#endif /* not HAVE_TERMIOS_H */
}

unsigned int
DEFUN (terminal_state_get_ispeed, (s), Ttty_state * s)
{
#ifdef HAVE_TERMIOS_H
  return (cfgetispeed (TIO (s)));
#else
#ifdef HAVE_TERMIO_H
  return (((TIO (s)) -> c_cflag) & CBAUD);
#else
#ifdef HAVE_SGTTY_H
  return (s -> sg . sg_ispeed);
#endif /* HAVE_SGTTY_H */
#endif /* not HAVE_TERMIO_H */
#endif /* not HAVE_TERMIOS_H */
}

void
DEFUN (terminal_state_set_ospeed, (s, b),
       Ttty_state * s AND
       unsigned int b)
{
#ifdef HAVE_TERMIOS_H
  cfsetospeed ((TIO (s)), b);
#else
#ifdef HAVE_TERMIO_H
  ((TIO (s)) -> c_cflag) = ((((TIO (s)) -> c_cflag) &~ CBAUD) | b);
#else
#ifdef HAVE_SGTTY_H
  (s -> sg . sg_ospeed) = b;
#endif /* HAVE_SGTTY_H */
#endif /* not HAVE_TERMIO_H */
#endif /* not HAVE_TERMIOS_H */
}

void
DEFUN (terminal_state_set_ispeed, (s, b),
       Ttty_state * s AND
       unsigned int b)
{
#ifdef HAVE_TERMIOS_H
  cfsetispeed ((TIO (s)), b);
#else
#ifdef HAVE_TERMIO_H
  ((TIO (s)) -> c_cflag) =
    ((((TIO (s)) -> c_cflag) &~ CIBAUD) | (b << IBSHIFT));
#else
#ifdef HAVE_SGTTY_H
  (s -> sg . sg_ispeed) = b;
#endif /* HAVE_SGTTY_H */
#endif /* not HAVE_TERMIO_H */
#endif /* not HAVE_TERMIOS_H */
}

int
DEFUN (terminal_state_cooked_output_p, (s), Ttty_state * s)
{
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
  return ((((TIO (s)) -> c_oflag) & OPOST) != 0);
#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H
  return (((s -> sg . sg_flags) & LLITOUT) == 0);
#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

void
DEFUN (terminal_state_raw_output, (s), Ttty_state * s)
{
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
  ((TIO (s)) -> c_oflag) &=~ OPOST;
#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H
  (s -> sg . sg_flags) &=~ ALLDELAY;
  (s -> lmode) |= LLITOUT;
#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

void
DEFUN (terminal_state_cooked_output, (s, channel),
       Ttty_state * s AND Tchannel channel)
{
  Ttty_state * os = (& (TERMINAL_ORIGINAL_STATE (channel)));
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
  ((TIO (s)) -> c_oflag) |= (((TIO (os)) -> c_oflag) & OPOST);
#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H
  (s -> sg . sg_flags) =
    (((s -> sg . sg_flags) &~ ALLDELAY) | ((os -> sg . sg_flags) & ALLDELAY));
  (s -> lmode) &=~ ((os -> lmode) & LLITOUT);
#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

int
DEFUN (terminal_state_buffered_p, (s), Ttty_state * s)
{
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
  return ((((TIO (s)) -> c_lflag) & ICANON) != 0);
#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H
  return (((s -> sg . sg_flags) & (CBREAK | RAW)) == 0);
#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

void
DEFUN (terminal_state_nonbuffered, (s, fd, polling),
       Ttty_state * s AND
       int fd AND
       int polling)
{
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)

  ((TIO (s)) -> c_lflag) &=~ (ICANON | ECHO);
#ifdef IEXTEN
  ((TIO (s)) -> c_lflag) &=~ IEXTEN;
#endif
  ((TIO (s)) -> c_lflag) |= ISIG;
  ((TIO (s)) -> c_iflag) |= IGNBRK;
  ((TIO (s)) -> c_iflag) &=~ (ICRNL | IXON | ISTRIP);
  ((TIO (s)) -> c_cflag) |= CS8;
  ((TIO (s)) -> c_cflag) &=~ PARENB;
  (((TIO (s)) -> c_cc) [VMIN]) = (polling ? 0 : 1);
  (((TIO (s)) -> c_cc) [VTIME]) = 0;
#ifdef HAVE_TERMIOS_H
  {
    cc_t disable = (UX_PC_VDISABLE (fd));
    (((TIO (s)) -> c_cc) [VSTOP]) = disable;
    (((TIO (s)) -> c_cc) [VSTART]) = disable;
  }
#endif

#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

  (s -> sg . sg_flags) &=~ (ECHO | CRMOD);
  (s -> sg . sg_flags) |= (ANYP | CBREAK);
  (s -> lmode) |= (LPASS8 | LNOFLSH);
  (s -> tc . t_startc) = (-1);
  (s -> tc . t_stopc) = (-1);
  (s -> tc . t_eofc) = (-1);
  (s -> tc . t_brkc) = (-1);
#ifdef HAVE_STRUCT_LTCHARS
  (s -> ltc . t_rprntc) = (-1);
  (s -> ltc . t_flushc) = (-1);
  (s -> ltc . t_werasc) = (-1);
  (s -> ltc . t_lnextc) = (-1);
#endif /* HAVE_STRUCT_LTCHARS */

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

void
DEFUN (terminal_state_raw, (s, fd), Ttty_state * s AND int fd)
{
  terminal_state_nonbuffered (s, fd, 0);

#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)

  ((TIO (s)) -> c_lflag) &=~ ISIG;

#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

  (s -> sg . sg_flags) &=~ CBREAK;
  (s -> sg . sg_flags) |= RAW;
  (s -> tc . t_intrc) = (-1);
  (s -> tc . t_quitc) = (-1);
#ifdef HAVE_STRUCT_LTCHARS
  (s -> ltc . t_suspc) = (-1);
  (s -> ltc . t_dsuspc) = (-1);
#endif /* HAVE_STRUCT_LTCHARS */

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

void
DEFUN (terminal_state_buffered, (s, channel),
       Ttty_state * s AND
       Tchannel channel)
{
  Ttty_state * os = (& (TERMINAL_ORIGINAL_STATE (channel)));

#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)

  ((TIO (s)) -> c_lflag) |= (ICANON | ISIG);
  ((TIO (s)) -> c_lflag) |= (((TIO (os)) -> c_lflag) & ECHO);
#ifdef IEXTEN
  ((TIO (s)) -> c_lflag) |= (((TIO (os)) -> c_lflag) & IEXTEN);
#endif
  ((TIO (s)) -> c_iflag) = ((TIO (os)) -> c_iflag);
  ((TIO (s)) -> c_cflag) |= CS8;
  ((TIO (s)) -> c_cflag) &=~ PARENB;
  (((TIO (s)) -> c_cc) [VMIN]) = (((TIO (os)) -> c_cc) [VMIN]);
  (((TIO (s)) -> c_cc) [VTIME]) = (((TIO (os)) -> c_cc) [VTIME]);
#ifdef HAVE_TERMIOS_H
  (((TIO (s)) -> c_cc) [VSTOP]) = (((TIO (os)) -> c_cc) [VSTOP]);
  (((TIO (s)) -> c_cc) [VSTART]) = (((TIO (os)) -> c_cc) [VSTART]);
#endif

#else /* not HAVE_TERMIOS_H nor HAVE_TERMIO_H */
#ifdef HAVE_SGTTY_H

  (s -> sg . sg_flags) &=~ (CBREAK | RAW);
  (s -> sg . sg_flags) |= ANYP;
  (s -> sg . sg_flags) |= ((os -> sg . sg_flags) & (ECHO | CRMOD));
  (s -> lmode) &=~ LNOFLSH;
  (s -> lmode) |= LPASS8;
  (s -> tc . t_intrc) = (os -> tc . t_intrc);
  (s -> tc . t_quitc) = (os -> tc . t_quitc);
  (s -> tc . t_startc) = (os -> tc . t_startc);
  (s -> tc . t_stopc) = (os -> tc . t_stopc);
  (s -> tc . t_eofc) = (os -> tc . t_eofc);
  (s -> tc . t_brkc) = (os -> tc . t_brkc);
#ifdef HAVE_STRUCT_LTCHARS
  (s -> ltc . t_suspc) = (os -> ltc . t_suspc);
  (s -> ltc . t_dsuspc) = (os -> ltc . t_dsuspc);
  (s -> ltc . t_rprntc) = (os -> ltc . t_rprntc);
  (s -> ltc . t_flushc) = (os -> ltc . t_flushc);
  (s -> ltc . t_werasc) = (os -> ltc . t_werasc);
  (s -> ltc . t_lnextc) = (os -> ltc . t_lnextc);
#endif /* HAVE_STRUCT_LTCHARS */

#endif /* HAVE_SGTTY_H */
#endif /* HAVE_TERMIOS_H or HAVE_TERMIO_H */
}

unsigned int
DEFUN (OS_terminal_get_ispeed, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  return (terminal_state_get_ispeed (&s));
}

unsigned int
DEFUN (OS_terminal_get_ospeed, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  return (terminal_state_get_ospeed (&s));
}

void
DEFUN (OS_terminal_set_ispeed, (channel, baud),
       Tchannel channel AND
       unsigned int baud)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_set_ispeed ((&s), baud);
  set_terminal_state (channel, (&s));
}

void
DEFUN (OS_terminal_set_ospeed, (channel, baud),
       Tchannel channel AND
       unsigned int baud)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_set_ospeed ((&s), baud);
  set_terminal_state (channel, (&s));
}

unsigned int
DEFUN (arg_baud_index, (argument), unsigned int argument)
{
  unsigned long index = (arg_nonnegative_integer (argument));
  switch (index)
    {
    case B0:
    case B50:
    case B75:
    case B110:
    case B134:
    case B150:
    case B200:
    case B300:
    case B600:
    case B1200:
    case B1800:
    case B2400:
    case B4800:
    case B9600:
    case B19200:
    case B38400:
#ifdef B57600
    case B57600:
#endif
#ifdef B115200
    case B115200:
#endif
#ifdef B230400
    case B230400:
#endif
#ifdef B460800
    case B460800:
#endif
#ifdef B500000
    case B500000:
#endif
#ifdef B576000
    case B576000:
#endif
#ifdef B921600
    case B921600:
#endif
#ifdef B1000000
    case B1000000:
#endif
#ifdef B1152000
    case B1152000:
#endif
#ifdef B1500000
    case B1500000:
#endif
#ifdef B2000000
    case B2000000:
#endif
#ifdef B2500000
    case B2500000:
#endif
#ifdef B3000000
    case B3000000:
#endif
#ifdef B3500000
    case B3500000:
#endif
#ifdef B4000000
    case B4000000:
#endif
      break;
    default:
      error_bad_range_arg (argument);
    }
  return (index);
}

unsigned int
DEFUN (OS_baud_index_to_rate, (index), unsigned int index)
{
  switch (index)
    {
    case B0:		return (0);
    case B50:		return (50);
    case B75:		return (75);
    case B110:		return (110);
    case B134:		return (134);
    case B150:		return (150);
    case B200:		return (200);
    case B300:		return (300);
    case B600:		return (600);
    case B1200:		return (1200);
    case B1800:		return (1800);
    case B2400:		return (2400);
    case B4800:		return (4800);
    case B9600:		return (9600);
    case B19200:	return (19200);
    case B38400:	return (38400);
#ifdef B57600
    case B57600:	return (57600);
#endif
#ifdef B115200
    case B115200:	return (115200);
#endif
#ifdef B230400
    case B230400:	return (230400);
#endif
#ifdef B460800
    case B460800:	return (460800);
#endif
#ifdef B500000
    case B500000:	return (500000);
#endif
#ifdef B576000
    case B576000:	return (576000);
#endif
#ifdef B921600
    case B921600:	return (921600);
#endif
#ifdef B1000000
    case B1000000:	return (1000000);
#endif
#ifdef B1152000
    case B1152000:	return (1152000);
#endif
#ifdef B1500000
    case B1500000:	return (1500000);
#endif
#ifdef B2000000
    case B2000000:	return (2000000);
#endif
#ifdef B2500000
    case B2500000:	return (2500000);
#endif
#ifdef B3000000
    case B3000000:	return (3000000);
#endif
#ifdef B3500000
    case B3500000:	return (3500000);
#endif
#ifdef B4000000
    case B4000000:	return (4000000);
#endif
    default:		abort (); return (0);
    }
}

int
DEFUN (OS_baud_rate_to_index, (rate), unsigned int rate)
{
  switch (rate)
    {
    case 0:		return (B0);
    case 50:		return (B50);
    case 75:		return (B75);
    case 110:		return (B110);
    case 134:		return (B134);
    case 150:		return (B150);
    case 200:		return (B200);
    case 300:		return (B300);
    case 600:		return (B600);
    case 1200:		return (B1200);
    case 1800:		return (B1800);
    case 2400:		return (B2400);
    case 4800:		return (B4800);
    case 9600:		return (B9600);
    case 19200:		return (B19200);
    case 38400:		return (B38400);
#ifdef B57600
    case 57600:		return (B57600);
#endif
#ifdef B115200
    case 115200:	return (B115200);
#endif
#ifdef B230400
    case 230400:	return (B230400);
#endif
#ifdef B460800
    case 460800:	return (B460800);
#endif
#ifdef B500000
    case 500000:	return (B500000);
#endif
#ifdef B576000
    case 576000:	return (B576000);
#endif
#ifdef B921600
    case 921600:	return (B921600);
#endif
#ifdef B1000000
    case 1000000:	return (B1000000);
#endif
#ifdef B1152000
    case 1152000:	return (B1152000);
#endif
#ifdef B1500000
    case 1500000:	return (B1500000);
#endif
#ifdef B2000000
    case 2000000:	return (B2000000);
#endif
#ifdef B2500000
    case 2500000:	return (B2500000);
#endif
#ifdef B3000000
    case 3000000:	return (B3000000);
#endif
#ifdef B3500000
    case 3500000:	return (B3500000);
#endif
#ifdef B4000000
    case 4000000:	return (B4000000);
#endif
    default:		return (-1);
    }
}

unsigned int
DEFUN_VOID (OS_terminal_state_size)
{
  return (sizeof (Ttty_state));
}

void
DEFUN (OS_terminal_get_state, (channel, statep),
       Tchannel channel AND
       PTR statep)
{
  get_terminal_state (channel, statep);
}

void
DEFUN (OS_terminal_set_state, (channel, statep),
       Tchannel channel AND
       PTR statep)
{
  set_terminal_state (channel, statep);
}

int
DEFUN (OS_terminal_cooked_output_p, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  return (terminal_state_cooked_output_p (&s));
}

void
DEFUN (OS_terminal_raw_output, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_raw_output (&s);
  set_terminal_state (channel, (&s));
}

void
DEFUN (OS_terminal_cooked_output, (channel), Tchannel channel)
{
  Ttty_state s;
  get_terminal_state (channel, (&s));
  terminal_state_cooked_output ((&s), channel);
  set_terminal_state (channel, (&s));
}

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
  terminal_state_nonbuffered ((&s), (CHANNEL_DESCRIPTOR (channel)), 0);
  set_terminal_state (channel, (&s));
}

void
DEFUN (OS_terminal_flush_input, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    (syscall_tcflush, (UX_tcflush ((CHANNEL_DESCRIPTOR (channel)), TCIFLUSH)));
}

void
DEFUN (OS_terminal_flush_output, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    (syscall_tcflush, (UX_tcflush ((CHANNEL_DESCRIPTOR (channel)), TCOFLUSH)));
}

void
DEFUN (OS_terminal_drain_output, (channel), Tchannel channel)
{
  STD_VOID_SYSTEM_CALL
    (syscall_tcdrain, (UX_tcdrain (CHANNEL_DESCRIPTOR (channel))));
}

int
DEFUN_VOID (OS_job_control_p)
{
  return (UX_SC_JOB_CONTROL ());
}

int
DEFUN_VOID (OS_have_ptys_p)
{
#ifdef HAVE_GRANTPT
  return (1);
#else
  static int result = 0;
  static int result_valid = 0;
  const char * p1;
  if (result_valid)
    return (result);
  for (p1 = "pqrstuvwxyzPQRST"; ((*p1) != 0); p1 += 1)
    {
      char master_name [24];
      struct stat s;
      sprintf (master_name, "/dev/pty%c0", (*p1));
    retry_stat:
      if ((UX_stat (master_name, (&s))) < 0)
	{
	  if (errno == EINTR)
	    goto retry_stat;
	  continue;
	}
      result = 1;
      result_valid = 1;
      return (result);
    }
  result = 0;
  result_valid = 1;
  return (result);
#endif
}

static CONST char *
DEFUN (open_pty_master_bsd, (master_fd, master_fname),
       Tchannel * master_fd AND
       CONST char ** master_fname)
{
  static char master_name [24];
  static char slave_name [24];
  const char * p1;
  const char * p2;
  int fd;

  for (p1 = "pqrstuvwxyzPQRST"; ((*p1) != 0); p1 += 1)
    for (p2 = "0123456789abcdef"; ((*p2) != 0); p2 += 1)
      {
	sprintf (master_name, "/dev/pty%c%c", (*p1), (*p2));
	sprintf (slave_name, "/dev/tty%c%c", (*p1), (*p2));
      retry_open:
	fd = (UX_open (master_name, O_RDWR, 0));
	if (fd < 0)
	  {
	    if (errno == ENOENT)
	      return (0);
	    if (errno != EINTR)
	      continue;
	    deliver_pending_interrupts ();
	    goto retry_open;
	  }
	if ((UX_access (slave_name, (R_OK | W_OK))) < 0)
	  {
	    UX_close (fd);
	    continue;
	  }
	MAKE_CHANNEL (fd, channel_type_unix_pty_master, (*master_fd) =);
	(*master_fname) = master_name;
	return (slave_name);
      }
  return (0);
}

#ifndef O_NOCTTY
#  define O_NOCTTY 0
#endif

/* Open an available pty, putting channel in (*ptyv),
   and return the file name of the pty.
   Signal error if none available.  */

CONST char *
DEFUN (OS_open_pty_master, (master_fd, master_fname),
       Tchannel * master_fd AND
       CONST char ** master_fname)
{
#ifdef HAVE_GRANTPT
  while (1)
    {
      static char slave_name [24];
#ifdef HAVE_GETPT
      int fd = (getpt ());
#else
      int fd = (UX_open ("/dev/ptmx", (O_RDWR | O_NOCTTY), 0));
#endif
      if (fd < 0)
	{
	  if (errno == EINTR)
	    {
	      deliver_pending_interrupts ();
	      continue;
	    }
	  /* Try BSD open.  This is needed for Linux which might have
	     Unix98 support in the library but not the kernel.  */
	  return (open_pty_master_bsd (master_fd, master_fname));
	}
#ifdef sonyrisc
      sony_block_sigchld ();
#endif
      grantpt (fd);
      unlockpt (fd);
      strcpy (slave_name, (ptsname (fd)));
#ifdef sonyrisc
      sony_unblock_sigchld ();
#endif
      MAKE_CHANNEL (fd, channel_type_unix_pty_master, (*master_fd) =);
      (*master_fname) = "/dev/ptmx";
      return (slave_name);
    }

#else /* not HAVE_GRANTPT */

  if (!OS_have_ptys_p ())
    error_unimplemented_primitive ();
  return (open_pty_master_bsd (master_fd, master_fname));

#endif /* not HAVE_GRANTPT */
}

void
DEFUN (OS_pty_master_send_signal, (channel, sig), Tchannel channel AND int sig)
{
#ifdef TIOCSIGSEND
  STD_VOID_SYSTEM_CALL
    (syscall_ioctl_TIOCSIGSEND,
     (UX_ioctl ((CHANNEL_DESCRIPTOR (channel)), TIOCSIGSEND, sig)));
#else
  int gid = (UX_tcgetpgrp (CHANNEL_DESCRIPTOR (channel)));
  if (gid < 0)
    {
      if (errno == ENOSYS)
	error_unimplemented_primitive ();
      else
	error_system_call (errno, syscall_tcgetpgrp);
    }
  STD_VOID_SYSTEM_CALL (syscall_kill, (UX_kill ((-gid), sig)));
#endif
}

void
DEFUN (OS_pty_master_kill, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGKILL);
}

void
DEFUN (OS_pty_master_stop, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGTSTP);
}

void
DEFUN (OS_pty_master_continue, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGCONT);
}

void
DEFUN (OS_pty_master_interrupt, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGINT);
}

void
DEFUN (OS_pty_master_quit, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGQUIT);
}

void
DEFUN (OS_pty_master_hangup, (channel), Tchannel channel)
{
  OS_pty_master_send_signal (channel, SIGHUP);
}
