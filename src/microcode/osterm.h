/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

#ifndef SCM_OSTERM_H
#define SCM_OSTERM_H

#include "os.h"

extern Tchannel arg_terminal (int);

extern unsigned int OS_terminal_get_ispeed (Tchannel channel);
extern unsigned int OS_terminal_get_ospeed (Tchannel channel);
extern void OS_terminal_set_ispeed
  (Tchannel channel, unsigned int baud);
extern void OS_terminal_set_ospeed
  (Tchannel channel, unsigned int baud);
extern unsigned int arg_baud_index (unsigned int argument);
extern unsigned int OS_baud_index_to_rate (unsigned int index);
extern int OS_baud_rate_to_index (unsigned int rate);
extern unsigned int OS_terminal_state_size (void);
extern void OS_terminal_get_state (Tchannel channel, void * statep);
extern void OS_terminal_set_state (Tchannel channel, void * statep);
extern int OS_terminal_cooked_output_p (Tchannel channel);
extern void OS_terminal_raw_output (Tchannel channel);
extern void OS_terminal_cooked_output (Tchannel channel);
extern int OS_terminal_buffered_p (Tchannel channel);
extern void OS_terminal_buffered (Tchannel channel);
extern void OS_terminal_nonbuffered (Tchannel channel);
extern void OS_terminal_flush_input (Tchannel channel);
extern void OS_terminal_flush_output (Tchannel channel);
extern void OS_terminal_drain_output (Tchannel channel);
extern int OS_job_control_p (void);
extern int OS_have_ptys_p (void);

#endif /* SCM_OSTERM_H */
