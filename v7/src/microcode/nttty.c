/* -*-C-*-

$Id: nttty.c,v 1.11 2007/01/05 15:33:07 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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
  Screen_GetSize (master_tty_window, (&tty_y_size), (&tty_x_size));
  return (tty_x_size);
}

unsigned int
OS_tty_y_size (void)
{
  Screen_GetSize (master_tty_window, (&tty_y_size), (&tty_x_size));
  return (tty_y_size);
}

CONST char *
OS_tty_command_beep (void)
{
  return (tty_command_beep);
}

CONST char *
OS_tty_command_clear (void)
{
  return (tty_command_clear);
}

void
NT_initialize_tty (void)
{
  input_channel = (NT_open_handle (master_tty_window));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = input_channel;
  Screen_GetSize (master_tty_window, (&tty_y_size), (&tty_x_size));
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
