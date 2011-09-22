/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Headers for the FFI (foreign function interface). */

/* This file declares all of the C functions needed by a shim.  It is
   installed as mit-scheme.h and represents the interface between the
   shims and the machine.  It should not include any other headers,
   and should minimize dependencies on the exact configuration of the
   machine.  Thus it declares teensy functions like empty_list(). */

/* This is redundant, but avoids the need for object.h, config.h, types.h... */
typedef unsigned long SCM;

extern char* cstack_top (void);
extern void cstack_push (void * addr, int bytes);
extern char* cstack_lpop (char* tos, int bytes);
extern void cstack_pop (char* tos);

#define CSTACK_PUSH(TYPE,VAR)					\
  cstack_push (((void *)(&VAR)), sizeof (TYPE));

/* "Local" CStack pops keep the top-of-stack in a local variable
   (TOS).  Thus after an abort the trampoline can start again from the
   undisturbed top of the obstack. */
#define CSTACK_LPOP(TYPE,VAR,TOS)					\
  TOS = cstack_lpop (TOS, sizeof (TYPE));				\
  VAR = *(TYPE *)TOS;

typedef void (*CalloutTrampOut)(void);
typedef SCM (*CalloutTrampIn)(void);
extern void callout_seal (CalloutTrampIn tramp);
extern void callout_unseal (CalloutTrampIn expected);
extern void callout_continue (CalloutTrampIn tramp);
extern char* callout_lunseal (CalloutTrampIn expected);
extern void callout_pop (char* tos);

typedef void (*CallbackKernel)(void);
extern void callback_run_kernel (long callback_id, CallbackKernel kernel);
extern char* callback_lunseal (CallbackKernel expected);
extern void callback_run_handler (long callback_id, SCM arglist);
extern void callback_return (char* tos);

/* Converters. */

extern long arg_long (int argn);
extern unsigned long arg_ulong (int argn);
extern double arg_double (int argn);
extern void* arg_alien_entry (int argn);
extern void* arg_pointer (int argn);

extern SCM long_to_scm (const long i);
extern SCM ulong_to_scm (const unsigned long i);
extern SCM double_to_scm (const double d);
extern SCM pointer_to_scm (const void* p);

extern SCM cons_alien (const void* p);

extern long long_value (void);
extern unsigned long ulong_value (void);
extern double double_value (void);
extern void* pointer_value (void);

/* Utilities: */

extern void check_number_of_args (int num);
extern SCM unspecific (void);
extern SCM empty_list (void);

#ifndef MIT_SCHEME /* Do not include in the microcode, just shims. */
extern SCM cons (SCM car, SCM cdr);
/* For debugging messages from shim code. */
extern void outf_error (const char *, ...);
extern void outf_flush_error (void);
#endif
