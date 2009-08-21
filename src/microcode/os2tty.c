/* -*-C-*-

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

#include "os2.h"
#include "ostty.h"
#ifdef USE_PMIO
#include <pmio.h>
#endif

static Tchannel input_channel;
static Tchannel output_channel;

void
OS2_initialize_tty (void)
{
  extern Tchannel OS_open_fd (int fd);
  input_channel = (OS2_make_channel (0, CHANNEL_READ));
  (CHANNEL_INTERNAL (input_channel)) = 1;
  output_channel = (OS2_make_channel (1, CHANNEL_WRITE));
  (CHANNEL_INTERNAL (output_channel)) = 1;
}

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
#ifdef USE_PMIO
  return (get_screen_width ());
#else
  return (80);
#endif
}

unsigned int
OS_tty_y_size (void)
{
#ifdef USE_PMIO
  return (get_screen_height ());
#else
  return (24);
#endif
}

const char *
OS_tty_command_beep (void)
{
  return ("\a");
}

const char *
OS_tty_command_clear (void)
{
  return ("\f");
}
