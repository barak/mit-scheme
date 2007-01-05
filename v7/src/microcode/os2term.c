/* -*-C-*-

$Id: os2term.c,v 1.7 2007/01/05 15:33:07 cph Exp $

Copyright (c) 1994, 1999, 2000 Massachusetts Institute of Technology

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
#include "prims.h"

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
