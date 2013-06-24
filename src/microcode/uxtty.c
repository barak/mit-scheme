/* -*-C-*-

$Id: uxtty.c,v 1.10 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "ux.h"
#include "ostty.h"
#include "osenv.h"
#include "uxio.h"
#include "uxterm.h"

/* Standard Input and Output */

static Tchannel input_channel;
static Tchannel output_channel;
static int tty_x_size;
static int tty_y_size;
static CONST char * tty_command_beep;
static CONST char * tty_command_clear;

Tchannel
DEFUN_VOID (OS_tty_input_channel)
{
  return (input_channel);
}

Tchannel
DEFUN_VOID (OS_tty_output_channel)
{
  return (output_channel);
}

unsigned int
DEFUN_VOID (OS_tty_x_size)
{
  return (tty_x_size);
}

unsigned int
DEFUN_VOID (OS_tty_y_size)
{
  return (tty_y_size);
}

CONST char *
DEFUN_VOID (OS_tty_command_beep)
{
  return (tty_command_beep);
}

CONST char *
DEFUN_VOID (OS_tty_command_clear)
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
DEFUN (tputs_write_char, (c), char c)
{
  (*tputs_output_scan++) = c;
}

void
DEFUN_VOID (UX_initialize_tty)
{
  extern int EXFUN (atoi, (CONST char *));
  extern Tchannel EXFUN (OS_open_fd, (int fd));
  input_channel = (OS_open_fd (STDIN_FILENO));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = (OS_open_fd (STDOUT_FILENO));
  (CHANNEL_INTERNAL (output_channel)) = 1;
  tty_x_size = (-1);
  tty_y_size = (-1);
  tty_command_beep = ALERT_STRING;
  tty_command_clear = 0;
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
      CONST char * columns = (UX_getenv ("COLUMNS"));
      CONST char * lines = (UX_getenv ("LINES"));
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
  tputs_output_scan = tputs_output;
  {
    extern int EXFUN (tgetent, (PTR, CONST char *));
    extern int EXFUN (tgetnum, (CONST char *));
    extern CONST char * EXFUN (tgetstr, (CONST char *, char **));
    static char tgetstr_buffer [TERMCAP_BUFFER_SIZE];
    char termcap_buffer [TERMCAP_BUFFER_SIZE];
    char * tbp = tgetstr_buffer;
    CONST char * term;
    if ((isatty (STDOUT_FILENO))
	&& (!option_emacs_subprocess)
	&& ((term = (getenv ("TERM"))) != 0)
	&& ((tgetent (termcap_buffer, term)) > 0))
      {
	if ((tty_x_size <= 0) || (tty_y_size <= 0))
	  {
	    tty_x_size = (tgetnum ("co"));
	    tty_y_size = (tgetnum ("li"));
	  }
	tty_command_clear = (tgetstr ("cl", (&tbp)));
      }
  }
  if ((tty_x_size <= 0) || (tty_y_size <= 0))
    {
      tty_x_size = DEFAULT_TTY_X_SIZE;
      tty_y_size = DEFAULT_TTY_Y_SIZE;
    }
  if (tty_command_clear == 0)
    tty_command_clear = "\f";
  else
    {
      extern void EXFUN (tputs, (CONST char *, int, void (*) (char)));
      char * command = tputs_output_scan;
      tputs (tty_command_clear, tty_y_size, tputs_write_char);
      (*tputs_output_scan++) = '\0';
      tty_command_clear = command;
    }
}
