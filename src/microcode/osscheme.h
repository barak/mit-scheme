/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#ifndef SCM_OSSCHEME_H
#define SCM_OSSCHEME_H 1

#include "outf.h"
#include "os.h"

extern Tchannel arg_channel (int);

extern int executing_scheme_primitive_p (void);

extern void debug_edit_flags (void);
extern void debug_back_trace (outf_channel);
extern void debug_examine_memory (long, const char *);

extern void error_out_of_channels (void) NORETURN;
extern void error_unimplemented_primitive (void) NORETURN;
extern void error_out_of_processes (void) NORETURN;
extern void error_floating_point_exception (void) NORETURN;

#ifdef __OS2__
   extern void request_attention_interrupt (void);
   extern int test_and_clear_attention_interrupt (void);
#endif

extern void request_console_resize_interrupt (void);
extern void request_character_interrupt (void);
extern void request_timer_interrupt (void);
extern void request_suspend_interrupt (void);
extern void deliver_pending_interrupts (void);
extern int pending_interrupts_p (void);
extern unsigned long get_interrupt_mask (void);
extern void set_interrupt_mask (unsigned long mask);
extern void signal_interrupt_for_primitive (void) NORETURN;

#endif /* SCM_OSSCHEME_H */
