/* -*-C-*-

$Id: os2term.c,v 1.2 1994/11/28 08:11:17 cph Exp $

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

unsigned int
OS_terminal_get_ispeed (Tchannel channel)
{
  return (9600);
}

unsigned int
OS_terminal_get_ospeed (Tchannel channel)
{
  return (9600);
}

void
OS_terminal_set_ispeed (Tchannel channel, unsigned int baud)
{
}

void
OS_terminal_set_ospeed (Tchannel channel, unsigned int baud)
{
}

unsigned int
arg_baud_index (unsigned int argument)
{
  return (arg_nonnegative_integer (argument));
}

unsigned int
OS_baud_index_to_rate (unsigned int index)
{
  return (index);
}

int
OS_baud_rate_to_index (unsigned int rate)
{
  return (rate);
}

unsigned int
OS_terminal_state_size (void)
{
  return (0);
}

void
OS_terminal_get_state (Tchannel channel, PTR statep)
{
}

void
OS_terminal_set_state (Tchannel channel, PTR statep)
{
}

int
OS_terminal_cooked_output_p (Tchannel channel)
{
  int flag;
  OS2_channel_operation (channel, chop_output_cooked,
			 ((choparg_t) (-1)), ((choparg_t) (&flag)), 0);
  return (flag);
}

void
OS_terminal_raw_output (Tchannel channel)
{
  OS2_channel_operation (channel, chop_output_cooked, ((choparg_t) 0), 0, 0);
}

void
OS_terminal_cooked_output (Tchannel channel)
{
  OS2_channel_operation (channel, chop_output_cooked, ((choparg_t) 1), 0, 0);
}

int
OS_terminal_buffered_p (Tchannel channel)
{
  int flag;
  OS2_channel_operation (channel, chop_input_buffered,
			 ((choparg_t) (-1)), ((choparg_t) (&flag)), 0);
  return (flag);
}

void
OS_terminal_buffered (Tchannel channel)
{
  OS2_channel_operation (channel, chop_input_buffered, ((choparg_t) 1), 0, 0);
}

void
OS_terminal_nonbuffered (Tchannel channel)
{
  OS2_channel_operation (channel, chop_input_buffered, ((choparg_t) 0), 0, 0);
}

void
OS_terminal_flush_input (Tchannel channel)
{
  OS2_channel_operation (channel, chop_input_flush, 0, 0, 0);
}

void
OS_terminal_flush_output (Tchannel channel)
{
  OS2_channel_operation (channel, chop_output_flush, 0, 0, 0);
}

void
OS_terminal_drain_output (Tchannel channel)
{
  OS2_channel_operation (channel, chop_output_drain, 0, 0, 0);
}

int
OS_job_control_p (void)
{
  return (0);
}

int
OS_have_ptys_p (void)
{
  return (0);
}
