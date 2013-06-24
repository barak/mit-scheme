/* -*-C-*-

$Id: prims.h,v 9.48 2001/03/08 17:03:32 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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

/* This file contains some macros for defining primitives,
   for argument type or value checking, and for accessing
   the arguments. */

#ifndef SCM_PRIMS_H
#define SCM_PRIMS_H

#include "ansidecl.h"

/* Definition of primitives. */

#define DEFINE_PRIMITIVE(scheme_name, fn_name, min_args, max_args, doc)	\
extern SCHEME_OBJECT EXFUN (fn_name, (void));				\
SCHEME_OBJECT DEFUN_VOID (fn_name)

/* Can be used for `max_args' in `DEFINE_PRIMITIVE' to indicate that
   the primitive has no upper limit on its arity.  */
#define LEXPR (-1)

/* Primitives should have this as their first statement. */
#ifdef ENABLE_PRIMITIVE_PROFILING
#define PRIMITIVE_HEADER(n_args) record_primitive_entry (Fetch_Expression ())
#else
#define PRIMITIVE_HEADER(n_args) {}
#endif

/* Primitives return by performing one of the following operations. */
#define PRIMITIVE_RETURN(value)	return (value)
#define PRIMITIVE_ABORT abort_to_interpreter

extern void EXFUN (canonicalize_primitive_context, (void));
#define PRIMITIVE_CANONICALIZE_CONTEXT canonicalize_primitive_context

/* Various utilities */

#define Primitive_GC(Amount)						\
{									\
  Request_GC (Amount);							\
  signal_interrupt_from_primitive ();					\
}

#define Primitive_GC_If_Needed(Amount)					\
{									\
  if (GC_Check (Amount)) Primitive_GC (Amount);				\
}

#define CHECK_ARG(argument, type_p) do					\
{									\
  if (! (type_p (ARG_REF (argument))))					\
    error_wrong_type_arg (argument);					\
} while (0)

#define ARG_LOC(argument) (STACK_LOC (argument - 1))
#define ARG_REF(argument) (STACK_REF (argument - 1))
#define LEXPR_N_ARGUMENTS() (Regs [REGBLOCK_LEXPR_ACTUALS])

extern void EXFUN (signal_error_from_primitive, (long error_code));
extern void EXFUN (signal_interrupt_from_primitive, (void));
extern void EXFUN (error_wrong_type_arg, (int));
extern void EXFUN (error_bad_range_arg, (int));
extern void EXFUN (error_external_return, (void));
extern void EXFUN (error_with_argument, (SCHEME_OBJECT));
extern long EXFUN (arg_integer, (int));
extern long EXFUN (arg_nonnegative_integer, (int));
extern long EXFUN (arg_index_integer, (int, long));
extern long EXFUN (arg_integer_in_range, (int, long, long));
extern unsigned long EXFUN (arg_ulong_integer, (int));
extern unsigned long EXFUN (arg_ulong_index_integer, (int, unsigned long));
extern double EXFUN (arg_real_number, (int));
extern double EXFUN (arg_real_in_range, (int, double, double));
extern long EXFUN (arg_ascii_char, (int));
extern long EXFUN (arg_ascii_integer, (int));

#define UNSIGNED_FIXNUM_ARG(arg)					\
  ((FIXNUM_P (ARG_REF (arg)))						\
   ? (UNSIGNED_FIXNUM_TO_LONG (ARG_REF (arg)))				\
   : ((error_wrong_type_arg (arg)), 0))

#define STRING_ARG(arg)							\
  ((STRING_P (ARG_REF (arg)))						\
   ? ((char *) (STRING_LOC ((ARG_REF (arg)), 0)))			\
   : ((error_wrong_type_arg (arg)), ((char *) 0)))

extern PTR EXFUN (arg_extended_string, (unsigned int, unsigned long *));

#define BOOLEAN_ARG(arg) ((ARG_REF (arg)) != SHARP_F)

#define CELL_ARG(arg)							\
  ((CELL_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((error_wrong_type_arg (arg)), ((SCHEME_OBJECT) 0)))

#define PAIR_ARG(arg)							\
  ((PAIR_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((error_wrong_type_arg (arg)), ((SCHEME_OBJECT) 0)))

#define WEAK_PAIR_ARG(arg)						\
  ((WEAK_PAIR_P (ARG_REF (arg)))					\
   ? (ARG_REF (arg))							\
   : ((error_wrong_type_arg (arg)), ((SCHEME_OBJECT) 0)))

#define VECTOR_ARG(arg)							\
  ((VECTOR_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((error_wrong_type_arg (arg)), ((SCHEME_OBJECT) 0)))

#define FLOATING_VECTOR_ARG(arg)					\
  ((FLONUM_P (ARG_REF (arg)))						\
   ? (ARG_REF (arg))							\
   : ((error_wrong_type_arg (arg)), ((SCHEME_OBJECT) 0)))

#endif /* SCM_PRIMS_H */
