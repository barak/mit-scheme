/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxtty.c,v 1.2 1990/06/21 20:01:58 cph Exp $

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
#include "ostty.h"
#include "osenv.h"
#include "uxio.h"
#include "uxterm.h"

/* Standard Input */

static Tchannel input_channel;

Tchannel
DEFUN_VOID (OS_tty_input_channel)
{
  return (input_channel);
}

/* New input interface doesn't require the following, because they
   can be provided by standard terminal and channel operations. */

static unsigned char
DEFUN (tty_read_char, (immediate), int immediate)
{
  if ((OS_channel_type (input_channel)) == channel_type_terminal)
    {
      transaction_begin ();
      preserve_terminal_state (input_channel);
      if (immediate)
	OS_terminal_nonbuffered (input_channel);
      else
	OS_terminal_buffered (input_channel);
      {
	int c = (OS_terminal_read_char (input_channel));
	if (c == (-1))
	  termination_eof ();
	transaction_commit ();
	return ((unsigned char) c);
      }
    }
  else
    {
      unsigned char c;
      if ((OS_channel_read (input_channel, (&c), 1)) != 1)
	termination_eof ();
      if ((OS_channel_type (input_channel)) == channel_type_file)
	OS_tty_write_char (c);
      return (c);
    }
}

unsigned char
DEFUN_VOID (OS_tty_read_char)
{
  return (tty_read_char (0));
}

unsigned char
DEFUN_VOID (OS_tty_read_char_immediate)
{
  return (tty_read_char (1));
}

int
DEFUN (OS_tty_char_ready_p, (delay), clock_t delay)
{
  if ((OS_channel_type (input_channel)) == channel_type_terminal)
    return (OS_terminal_char_ready_p (input_channel, delay));
  if (delay > 0)
    {
      clock_t limit = ((OS_real_time_clock ()) + delay);
      while ((OS_real_time_clock ()) < limit)
	;
    }
  return (0);
}

int
DEFUN (OS_tty_clean_interrupts, (mode, interrupt_char),
       enum tty_clean_mode mode AND
       cc_t interrupt_char)
{
  if (parent_process_is_emacs && (mode == tty_clean_most_recent))
    while ((OS_tty_read_char_immediate ()) != '\0')
      ;
  return (1);
}

/* Standard Output */

static Tchannel output_channel;
static int tty_x_size;
static int tty_y_size;
static CONST char * tty_command_beep;
static CONST char * tty_command_clear;

Tchannel
DEFUN_VOID (OS_tty_output_channel)
{
  return (output_channel);
}

unsigned int
DEFUN_VOID (OS_tty_x_size)
{
  CONST char * columns = (UX_getenv ("COLUMNS"));
  if (columns != 0)
    {
      int x = (atoi (columns));
      if (x > 0)
	tty_x_size = x;
    }
  return (tty_x_size);
}

unsigned int
DEFUN_VOID (OS_tty_y_size)
{
  CONST char * lines = (UX_getenv ("LINES"));
  if (lines != 0)
    {
      int y = (atoi (lines));
      if (y > 0)
	tty_y_size = y;
    }
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

/* Old output interface requires output buffering at the microcode
   level.  The new runtime system will provide the buffering so that
   the microcode doesn't have to. */

void
DEFUN (OS_tty_write_char, (c), unsigned char c)
{
  if ((OS_channel_write (output_channel, (&c), 1)) != 1)
    error_external_return ();
}

void
DEFUN (OS_tty_write_string, (s), CONST char * s)
{
  OS_channel_write_string (output_channel, s);
}

void
DEFUN_VOID (OS_tty_beep)
{
  OS_channel_write_string (output_channel, tty_command_beep);
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
  extern Tchannel EXFUN (OS_open_fd, (int fd));
  input_channel = (OS_open_fd (STDIN_FILENO));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = (OS_open_fd (STDOUT_FILENO));
  (CHANNEL_INTERNAL (output_channel)) = 1;
  tty_x_size = (-1);
  tty_y_size = (-1);
  tty_command_beep = ALERT_STRING;
  tty_command_clear = 0;
  tputs_output_scan = tputs_output;
  {
    extern int EXFUN (tgetent, (PTR, CONST char *));
    extern int EXFUN (tgetnum, (CONST char *));
    extern CONST char * EXFUN (tgetstr, (CONST char *, char **));
    static char tgetstr_buffer [TERMCAP_BUFFER_SIZE];
    char termcap_buffer [TERMCAP_BUFFER_SIZE];
    char * tbp = tgetstr_buffer;
    CONST char * term;
    if ((isatty (STDOUT_FILENO)) &&
	(!parent_process_is_emacs) &&
	((term = (getenv ("TERM"))) != 0) &&
	((tgetent (termcap_buffer, term)) > 0))
      {
	tty_x_size = (tgetnum ("co"));
	tty_y_size = (tgetnum ("li"));
	tty_command_clear = (tgetstr ("cl", (&tbp)));
      }
  }
  if (tty_x_size == (-1))
    tty_x_size = DEFAULT_TTY_X_SIZE;
  if (tty_y_size == (-1))
    tty_y_size = DEFAULT_TTY_Y_SIZE;
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
