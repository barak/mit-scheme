/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/ostty.h,v 1.3 1992/02/27 18:53:26 mhwu Exp $

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
