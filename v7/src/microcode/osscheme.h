/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/osscheme.h,v 1.3 1991/03/01 00:55:17 cph Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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

#ifndef SCM_OSSCHEME_H
#define SCM_OSSCHEME_H

#include "os.h"

extern Tchannel EXFUN (arg_channel, (int arg_number));

extern int option_emacs_subprocess;

extern int EXFUN (executing_scheme_primitive_p, (void));

extern void EXFUN (debug_edit_flags, (void));
extern void EXFUN (debug_back_trace, (void));
extern void EXFUN (debug_examine_memory, (long address, CONST char * label));

extern void EXFUN (error_out_of_channels, (void));
extern void EXFUN (error_unimplemented_primitive, (void));
extern void EXFUN (error_external_return, (void));
extern void EXFUN (error_out_of_processes, (void));
extern void EXFUN (error_floating_point_exception, (void));

extern void EXFUN (termination_eof, (void));
extern void EXFUN (termination_normal, (void));
extern void EXFUN (termination_init_error, (void));
extern void EXFUN (termination_signal, (CONST char * signal_name));
extern void EXFUN (termination_trap, (void));

extern void EXFUN (request_character_interrupt, (void));
extern void EXFUN (request_timer_interrupt, (void));
extern void EXFUN (request_suspend_interrupt, (void));
extern void EXFUN (deliver_pending_interrupts, (void));
extern int  EXFUN (pending_interrupts_p, (void));
extern long EXFUN (get_interrupt_mask, (void));
extern void EXFUN (set_interrupt_mask, (long mask));
extern void EXFUN (signal_interrupt_for_primitive, (void));
extern void EXFUN (preserve_interrupt_mask, (void));
extern void EXFUN (back_out_of_primitive, (void));

#endif /* SCM_OSSCHEME_H */
