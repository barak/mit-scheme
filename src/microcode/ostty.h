/* -*-C-*-

$Id: ostty.h,v 1.5 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef SCM_OSTTY_H
#define SCM_OSTTY_H

#include "os.h"

extern Tchannel EXFUN (OS_tty_input_channel, (void));
extern Tchannel EXFUN (OS_tty_output_channel, (void));
extern unsigned int EXFUN (OS_tty_x_size, (void));
extern unsigned int EXFUN (OS_tty_y_size, (void));
extern CONST char * EXFUN (OS_tty_command_beep, (void));
extern CONST char * EXFUN (OS_tty_command_clear, (void));
extern cc_t EXFUN (OS_tty_next_interrupt_char, (void));
extern cc_t EXFUN (OS_tty_map_interrupt_char, (cc_t));

#endif /* SCM_OSTTY_H */
