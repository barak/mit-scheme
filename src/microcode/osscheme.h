/* -*-C-*-

$Id: osscheme.h,v 1.13 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1990-2000 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef SCM_OSSCHEME_H
#define SCM_OSSCHEME_H

#include "outf.h"
#include "os.h"

extern Tchannel EXFUN (arg_channel, (int arg_number));

extern int option_emacs_subprocess;

extern int EXFUN (executing_scheme_primitive_p, (void));

extern void EXFUN (debug_edit_flags, (void));
extern void EXFUN (debug_back_trace, (outf_channel));
extern void EXFUN (debug_examine_memory, (long address, CONST char * label));

extern void EXFUN (error_out_of_channels, (void));
extern void EXFUN (error_unimplemented_primitive, (void));
extern void EXFUN (error_external_return, (void));
extern void EXFUN (error_out_of_processes, (void));
extern void EXFUN (error_floating_point_exception, (void));

extern void EXFUN (termination_eof, (void));
extern void EXFUN (termination_normal, (CONST int));
extern void EXFUN (termination_init_error, (void));
extern void EXFUN (termination_signal, (CONST char * signal_name));
extern void EXFUN (termination_trap, (void));

#ifdef __OS2__
extern void EXFUN (request_attention_interrupt, (void));
extern int  EXFUN (test_and_clear_attention_interrupt, (void));
#endif /* __OS2__ */

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
