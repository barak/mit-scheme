/* -*-C-*-

$Id: osctty.h,v 1.7 2007/01/05 15:33:07 cph Exp $

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

#ifndef SCM_OSCTTY_H
#define SCM_OSCTTY_H

#include "os.h"

/* If this procedure returns 0, the interrupt control procedures will
   not work correctly. */
extern int EXFUN (OS_ctty_interrupt_control, (void));

typedef unsigned int Tinterrupt_enables;
extern void EXFUN (OS_ctty_get_interrupt_enables, (Tinterrupt_enables * mask));
extern void EXFUN (OS_ctty_set_interrupt_enables, (Tinterrupt_enables * mask));

extern unsigned int EXFUN (OS_ctty_num_int_chars, (void));
extern cc_t * EXFUN (OS_ctty_get_int_chars, (void));
extern cc_t * EXFUN (OS_ctty_get_int_char_handlers, (void));
extern void EXFUN (OS_ctty_set_int_chars, (cc_t *));
extern void EXFUN (OS_ctty_set_int_char_handlers, (cc_t *));

#endif /* SCM_OSCTTY_H */
