/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dostty.c,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "msdos.h"
#include "ostty.h"
#include "osenv.h"
#include "dosio.h"
#include "dosterm.h"

/* Standard Input and Output */

static Tchannel input_channel;
static Tchannel output_channel;
static int tty_x_size;
static int tty_y_size;
static char * tty_command_beep;
static char * tty_command_clear;

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
#define TERMCAP_BUFFER_SIZE 0
#endif

#ifndef DEFAULT_TTY_X_SIZE
#define DEFAULT_TTY_X_SIZE 80
#endif

#ifndef DEFAULT_TTY_Y_SIZE
#define DEFAULT_TTY_Y_SIZE 25
#endif

static int
DEFUN (getenv_integer, (var, default_val), char * var AND int default_val)
{
  CONST char * value = (DOS_getenv(var));
  return ((value == NULL) ? default_val : (atoi(value)));
}

void
DEFUN_VOID (DOS_initialize_tty)
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
  /* Figure out the size of the terminal.  First ask the operating
     system, if it has an appropriate system call.  Then try the
     environment variables COLUMNS and LINES.  Then try termcap.
     Finally, use the default.  */
  tty_x_size = getenv_integer("COLUMNS", DEFAULT_TTY_X_SIZE);
  tty_y_size = getenv_integer("LINES", DEFAULT_TTY_Y_SIZE);

  if (tty_command_clear == 0)
    tty_command_clear = "\033[2J";
}


/* Fake TERMCAP capability */
short ospeed;
char PC;

int tputs (string, nlines, outfun)
     register char *string;
     int nlines;
     register int (*outfun) ();
{
  register int padcount = 0;

  if (string == (char *) 0)
    return;
  while (*string >= '0' && *string <= '9')
    {
      padcount += *string++ - '0';
      padcount *= 10;
    }
  if (*string == '.')
    {
      string++;
      padcount += *string++ - '0';
    }
  if (*string == '*')
    {
      string++;
      padcount *= nlines;
    }
  while (*string)
    (*outfun) (*string++);

  return 0;
}
