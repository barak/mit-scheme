/* -*-C-*-

$Id: osterm.h,v 1.14 2007/01/05 15:33:07 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

extern Tchannel EXFUN (arg_channel, (int));
extern Tchannel EXFUN (arg_terminal, (int));

extern unsigned int EXFUN (OS_terminal_get_ispeed, (Tchannel channel));
extern unsigned int EXFUN (OS_terminal_get_ospeed, (Tchannel channel));
extern void EXFUN
  (OS_terminal_set_ispeed, (Tchannel channel, unsigned int baud));
extern void EXFUN
  (OS_terminal_set_ospeed, (Tchannel channel, unsigned int baud));
extern unsigned int EXFUN (arg_baud_index, (unsigned int argument));
extern unsigned int EXFUN (OS_baud_index_to_rate, (unsigned int index));
extern int EXFUN (OS_baud_rate_to_index, (unsigned int rate));
extern unsigned int EXFUN (OS_terminal_state_size, (void));
extern void EXFUN (OS_terminal_get_state, (Tchannel channel, PTR statep));
extern void EXFUN (OS_terminal_set_state, (Tchannel channel, PTR statep));
extern int EXFUN (OS_terminal_cooked_output_p, (Tchannel channel));
extern void EXFUN (OS_terminal_raw_output, (Tchannel channel));
extern void EXFUN (OS_terminal_cooked_output, (Tchannel channel));
extern int EXFUN (OS_terminal_buffered_p, (Tchannel channel));
extern void EXFUN (OS_terminal_buffered, (Tchannel channel));
extern void EXFUN (OS_terminal_nonbuffered, (Tchannel channel));
extern void EXFUN (OS_terminal_flush_input, (Tchannel channel));
extern void EXFUN (OS_terminal_flush_output, (Tchannel channel));
extern void EXFUN (OS_terminal_drain_output, (Tchannel channel));
extern int EXFUN (OS_job_control_p, (void));
extern int EXFUN (OS_have_ptys_p, (void));

#endif /* SCM_OSTERM_H */
