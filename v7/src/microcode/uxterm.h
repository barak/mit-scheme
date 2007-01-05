/* -*-C-*-

$Id: uxterm.h,v 1.8 2007/01/05 15:33:08 cph Exp $

Copyright (c) 1990, 1991, 1999 Massachusetts Institute of Technology

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

#ifndef SCM_UXTERM_H
#define SCM_UXTERM_H

#include "osterm.h"

extern int EXFUN (terminal_state_buffered_p, (Ttty_state * s));
extern void EXFUN
  (terminal_state_buffered, (Ttty_state * s, Tchannel channel));
extern void EXFUN
  (terminal_state_nonbuffered, (Ttty_state * s, int fd, int polling));
extern void EXFUN (terminal_state_raw, (Ttty_state * s, int fd));
extern void EXFUN (get_terminal_state, (Tchannel channel, Ttty_state * s));
extern void EXFUN (set_terminal_state, (Tchannel channel, Ttty_state * s));

#endif /* SCM_UXTERM_H */
