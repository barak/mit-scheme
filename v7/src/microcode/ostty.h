/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/ostty.h,v 1.1 1990/06/20 19:36:54 cph Exp $

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

/* New interface uses standard terminal and channel I/O. */
extern Tchannel EXFUN (OS_tty_input_channel, (void));
extern Tchannel EXFUN (OS_tty_output_channel, (void));
extern unsigned int EXFUN (OS_tty_x_size, (void));
extern unsigned int EXFUN (OS_tty_y_size, (void));
extern CONST char * EXFUN (OS_tty_command_beep, (void));
extern CONST char * EXFUN (OS_tty_command_clear, (void));

/* These are for the convenience of the microcode. */
extern void EXFUN (OS_tty_write_char, (unsigned char c));
extern void EXFUN (OS_tty_write_string, (CONST char * string));
extern void EXFUN (OS_tty_beep, (void));

/* Old interface requires special entry points and buffered output. */
extern int EXFUN (OS_tty_char_ready_p, (clock_t delay));
extern unsigned char EXFUN (OS_tty_read_char, (void));
extern unsigned char EXFUN (OS_tty_read_char_immediate, (void));

/* `OS_tty_clean_interrupts' is used to clear the input buffer when a
   character interrupt is received.  On most systems this is not
   currently used, but the Emacs interface needs some assistance.
   Normally this is used in conjunction with some kind of
   distinguished marker in the input stream that indicates where each
   interrupt occurred.

   The `mode' argument allows the following values:

   `tty_clean_most_recent' indicates that the input buffer should be
   flushed up to and including the most recent interrupt marker.

   `tty_clean_multiple_copies' indicates that all interrupts which
   match `interrupt_char' should be removed from the input buffer.
   Any other interrupts should be left alone. */

enum tty_clean_mode { tty_clean_most_recent, tty_clean_multiple_copies };
extern cc_t EXFUN (OS_tty_next_interrupt_char, (void));
extern int EXFUN
  (OS_tty_clean_interrupts, (enum tty_clean_mode mode, cc_t interrupt_char));

#endif /* SCM_OSTTY_H */
