/* -*-C-*-

$Id: uxtty.c,v 1.19 2009/03/21 08:06:00 riastradh Exp $

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

#include "scheme.h"
#include "option.h"
#include "ux.h"
#include "ostty.h"
#include "osenv.h"
#include "uxio.h"
#include "uxterm.h"

extern Tchannel OS_open_fd (int fd);
extern int tgetent (void *, const char *);
extern int tgetnum (const char *);
extern const char * tgetstr (const char *, char **);
extern void tputs (const char *, int, void (*) (char));

/* Standard Input and Output */

static Tchannel input_channel;
static Tchannel output_channel;

static int tty_x_size;
static int tty_y_size;
static const char * tty_command_beep;
static const char * tty_command_clear;

static bool tty_size_synchronized_p;
static void UX_synchronize_tty_size (void);

Tchannel
OS_tty_input_channel (void)
{
  return (input_channel);
}

Tchannel
OS_tty_output_channel (void)
{
  return (output_channel);
}

unsigned int
OS_tty_x_size (void)
{
  UX_synchronize_tty_size ();
  return (tty_x_size);
}

unsigned int
OS_tty_y_size (void)
{
  UX_synchronize_tty_size ();
  return (tty_y_size);
}

const char *
OS_tty_command_beep (void)
{
  return (tty_command_beep);
}

const char *
OS_tty_command_clear (void)
{
  return (tty_command_clear);
}

#ifndef TERMCAP_BUFFER_SIZE
#define TERMCAP_BUFFER_SIZE 2048
#endif

#ifndef DEFAULT_TTY_X_SIZE
#define DEFAULT_TTY_X_SIZE 80
#endif

#ifndef DEFAULT_TTY_Y_SIZE
#define DEFAULT_TTY_Y_SIZE 24
#endif

/* Define the 4.3 names in terms of the Sun names
   if the latter exist and the former do not.  */
#ifdef TIOCGSIZE
#ifndef TIOCGWINSZ
#define TIOCGWINSZ TIOCGSIZE
#define winsize ttysize
#define ws_row ts_lines
#define ws_col ts_cols
#endif /* not TIOCGWINSZ */
#endif /* TIOCGSIZE */

static char tputs_output [TERMCAP_BUFFER_SIZE];
static char * tputs_output_scan;

static void
tputs_write_char (char c)
{
  (*tputs_output_scan++) = c;
}

static void
UX_tty_with_termcap (void (*procedure) (void))
{
  tputs_output_scan = tputs_output;
  {
    char termcap_buffer [TERMCAP_BUFFER_SIZE];
    const char *term;
    if ((isatty (STDOUT_FILENO))
	&& (!option_emacs_subprocess)
	&& ((term = (getenv ("TERM"))) != 0)
	&& ((tgetent (termcap_buffer, term)) > 0))
      (*procedure) ();
  }
}

static void
UX_synchronize_tty_size_with_termcap (void)
{
  tty_x_size = (tgetnum ("co"));
  tty_y_size = (tgetnum ("li"));
}

static void
UX_synchronize_tty_size (void)
{
  if (tty_size_synchronized_p)
    return;

  tty_x_size = (-1);
  tty_y_size = (-1);

  /* Figure out the size of the terminal.  First ask the operating
     system, if it has an appropriate system call.  Then try the
     environment variables COLUMNS and LINES.  Then try termcap.
     Finally, use the default.  */
#ifdef TIOCGWINSZ
  {
    struct winsize size;
    if ((UX_ioctl (STDOUT_FILENO, TIOCGWINSZ, (&size))) >= 0)
      {
	tty_x_size = (size . ws_col);
	tty_y_size = (size . ws_row);
      }
  }
#endif /* TIOCGWINSZ */

  if ((tty_x_size <= 0) || (tty_y_size <= 0))
    {
      const char * columns = (UX_getenv ("COLUMNS"));
      const char * lines = (UX_getenv ("LINES"));
      if ((columns != 0) && (lines != 0))
	{
	  int x = (atoi (columns));
	  int y = (atoi (lines));
	  if ((x > 0) && (y > 0))
	    {
	      tty_x_size = x;
	      tty_y_size = y;
	    }
	}
    }

  if ((tty_x_size <= 0) || (tty_y_size <= 0))
    UX_tty_with_termcap (&UX_synchronize_tty_size_with_termcap);

  if ((tty_x_size <= 0) || (tty_y_size <= 0))
    {
      tty_x_size = DEFAULT_TTY_X_SIZE;
      tty_y_size = DEFAULT_TTY_Y_SIZE;
    }

  tty_size_synchronized_p = true;
}

static void
UX_initialize_tty_with_termcap (void)
{
  static char tgetstr_buffer [TERMCAP_BUFFER_SIZE];
  char *tbp = tgetstr_buffer;
  tty_command_clear = (tgetstr ("cl", (&tbp)));
}

void
UX_initialize_tty (void)
{
  input_channel = (OS_open_fd (STDIN_FILENO));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = (OS_open_fd (STDOUT_FILENO));
  (CHANNEL_INTERNAL (output_channel)) = 1;
  tty_size_synchronized_p = false;
  UX_synchronize_tty_size ();
  tty_command_beep = ALERT_STRING;
  tty_command_clear = 0;
  UX_tty_with_termcap (&UX_initialize_tty_with_termcap);
  if (tty_command_clear == 0)
    tty_command_clear = "\f";
  else
    {
      char * command = tputs_output_scan;
      tputs (tty_command_clear, tty_y_size, tputs_write_char);
      (*tputs_output_scan++) = '\0';
      tty_command_clear = command;
    }
}

void
UX_reinitialize_tty (void)
{
  tty_size_synchronized_p = false;
}
