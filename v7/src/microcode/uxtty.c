/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxtty.c,v 1.8 1992/02/10 13:51:02 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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
