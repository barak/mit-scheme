/* -*-C-*-

$Id: prims.h,v 9.57 2007/04/22 16:31:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* This file contains some macros for defining primitives,
   for argument type or value checking, and for accessing
   the arguments. */

#ifndef SCM_PRIMS_H
#define SCM_PRIMS_H

#include "scheme.h"

/* Definition of primitives. */

#define DEFINE_PRIMITIVE(scheme_name, fn_name, min_args, max_args, doc)	\
SCHEME_OBJECT fn_name (void)

/* Can be used for `max_args' in `DEFINE_PRIMITIVE' to indicate that
   the primitive has no upper limit on its arity.  */
#define LEXPR (-1)

/* Primitives should have this as their first statement. */
#ifdef ENABLE_PRIMITIVE_PROFILING
   extern void record_primitive_entry (SCHEME_OBJECT);
#  define PRIMITIVE_HEADER(n_args) record_primitive_entry (GET_EXP)
#else
#  define PRIMITIVE_HEADER(n_args) do {} while (0)
#endif

/* Primitives return by performing one of the following operations. */
#define PRIMITIVE_RETURN(value)	return (value)
#define PRIMITIVE_ABORT abort_to_interpreter

/* Various utilities */

#define Primitive_GC(Amount) do						\
{									\
  REQUEST_GC (Amount);							\
  signal_interrupt_from_primitive ();					\
} while (0)

#define Primitive_GC_If_Needed(Amount) do				\
{									\
  if (GC_NEEDED_P (Amount)) Primitive_GC (Amount);			\
} while (0)

#define CHECK_ARG(argument, type_p) do					\
{									\
  if (! (type_p (ARG_REF (argument))))					\
    error_wrong_type_arg (argument);					\
} while (0)

#define ARG_LOC(argument) (STACK_LOC (argument - 1))
#define ARG_REF(argument) (STACK_REF (argument - 1))

extern void signal_error_from_primitive (long) NORETURN;
extern void signal_interrupt_from_primitive (void) NORETURN;
extern void error_wrong_type_arg (int) NORETURN;
extern void error_bad_range_arg (int) NORETURN;
extern void error_external_return (void) NORETURN;
extern void error_with_argument (SCHEME_OBJECT) NORETURN;
extern long arg_integer (int);
extern long arg_nonnegative_integer (int);
extern long arg_index_integer (int, long);
extern long arg_integer_in_range (int, long, long);
extern unsigned long arg_ulong_integer (int);
extern unsigned long arg_ulong_index_integer (int, unsigned long);
extern unsigned long arg_ulong_integer_in_range
  (int, unsigned long, unsigned long);
extern double arg_real_number (int);
extern double arg_real_in_range (int, double, double);
extern long arg_ascii_char (int);
extern long arg_ascii_integer (int);

#define UNSIGNED_FIXNUM_ARG(arg)					\
  ((FIXNUM_P (ARG_REF (arg)))						\
   ? (UNSIGNED_FIXNUM_TO_LONG (ARG_REF (arg)))				\
   : ((error_wrong_type_arg (arg)), 0))

#define STRING_ARG(arg)							\
  ((STRING_P (ARG_REF (arg)))						\
   ? (STRING_POINTER (ARG_REF (arg)))					\
   : ((error_wrong_type_arg (arg)), ((char *) 0)))

extern unsigned char * arg_extended_string (unsigned int, unsigned long *);

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
