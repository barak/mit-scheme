/* -*-C-*-

$Id: nttty.c,v 1.6 1996/10/02 18:58:24 cph Exp $

Copyright (c) 1992-96 Massachusetts Institute of Technology

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

#include "nt.h"
#include "ostty.h"
#include "osenv.h"
#include "ntio.h"
#include "ntterm.h"
#include "ntscreen.h"

/* Standard Input and Output */

static Tchannel input_channel;
static Tchannel output_channel;

HANDLE master_tty_window = 0;

int tty_x_size;
int tty_y_size;
  /* 1-based values */
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
  Screen_GetSize (master_tty_window, &tty_y_size, &tty_x_size);
  return (tty_x_size);
}

unsigned int
DEFUN_VOID (OS_tty_y_size)
{
  Screen_GetSize (master_tty_window, &tty_y_size, &tty_x_size);
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

void
DEFUN_VOID (NT_initialize_tty)
{
  extern Tchannel EXFUN (OS_open_handle, (int fd));
  input_channel  = (OS_open_handle ((int) master_tty_window));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = input_channel;
  (CHANNEL_INTERNAL (output_channel)) = 1;
  Screen_GetSize (master_tty_window, &tty_y_size, &tty_x_size);

  tty_command_beep = ALERT_STRING;
  tty_command_clear = "\014";
}

/* Fake TERMCAP capability */
short ospeed;
char PC;

int
tputs (string, nlines, outfun)
     register char * string;
     int nlines;
     register int (*outfun) ();
{
  register int padcount = 0;

  if (string == (char *) 0)
    return (0);
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

  return (0);
}
