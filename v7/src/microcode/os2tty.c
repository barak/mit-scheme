/* -*-C-*-

$Id: os2tty.c,v 1.1 1994/11/28 03:43:02 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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
  extern Tchannel EXFUN (OS_open_fd, (int fd));
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
