/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osctty.h,v 1.1 1990/06/20 19:36:13 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#ifndef SCM_OSCTTY_H
#define SCM_OSCTTY_H

#include "os.h"

extern cc_t EXFUN (OS_ctty_quit_char, (void));
extern cc_t EXFUN (OS_ctty_int_char, (void));
extern cc_t EXFUN (OS_ctty_tstp_char, (void));
extern void EXFUN
  (OS_ctty_set_interrupt_chars,
   (cc_t quit_char, cc_t int_char, cc_t tstp_char));

/* If this procedure returns 0, the interrupt control procedures will
   not work correctly. */
extern int EXFUN (OS_ctty_interrupt_control, (void));

typedef unsigned int Tinterrupt_enables;
extern void EXFUN (OS_ctty_get_interrupt_enables, (Tinterrupt_enables * mask));
extern void EXFUN (OS_ctty_set_interrupt_enables, (Tinterrupt_enables * mask));

#endif /* SCM_OSCTTY_H */
