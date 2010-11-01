/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Floating Point Environment */

#include "scheme.h"
#include "osscheme.h"
#include "prims.h"

#include "floenv.h"

#ifndef __GNUC__
#  pragma STDC FENV_ACCESS ON
#endif

#define VECTOR_8B_LENGTH STRING_LENGTH
#define VECTOR_8B_P STRING_P
#define VECTOR_8B_POINTER STRING_POINTER
#define allocate_vector_8b allocate_string

static SCHEME_OBJECT
arg_vector_8b (int n)
{
  CHECK_ARG (n, VECTOR_8B_P);
  return (ARG_REF (n));
}

#ifndef HAVE_FENV_T
typedef char fenv_t;
#endif

#ifndef HAVE_FEXCEPT_T
typedef char fexcept_t;
#endif

static bool scheme_fenv_p = false;
static fenv_t scheme_fenv;

static void
cache_float_environment (void)
{
#ifdef HAVE_FEGETENV
  if (0 != (fegetenv (&scheme_fenv)))
    error_external_return ();
  scheme_fenv_p = true;
#endif
}

void
fixup_float_environment (void)
{
#ifdef HAVE_FESETENV
  if (scheme_fenv_p)
    (void) fesetenv (&scheme_fenv);
#endif
}

/* FIXME: Alignment?  */

static SCHEME_OBJECT
allocate_fenv (fenv_t **envp_loc)
{
  SCHEME_OBJECT environment = (allocate_vector_8b (sizeof (fenv_t)));
  (*envp_loc) = ((fenv_t *) (VECTOR_8B_POINTER (environment)));
  return (environment);
}

static fenv_t *
arg_fenv (int n)
{
  SCHEME_OBJECT environment = (arg_vector_8b (n));
  if ((sizeof (fenv_t)) != (VECTOR_8B_LENGTH (environment)))
    error_bad_range_arg (n);
  return ((fenv_t *) (VECTOR_8B_POINTER (environment)));
}

static SCHEME_OBJECT
allocate_fexcept (fexcept_t **flagp_loc)
{
  SCHEME_OBJECT flags = (allocate_vector_8b (sizeof (fexcept_t)));
  (*flagp_loc) = ((fexcept_t *) (VECTOR_8B_POINTER (flags)));
  return (flags);
}

static fexcept_t *
arg_fexcept (int n)
{
  SCHEME_OBJECT flags = (arg_vector_8b (n));
  if ((sizeof (fexcept_t)) != (VECTOR_8B_LENGTH (flags)))
    error_bad_range_arg (n);
  return ((fexcept_t *) (VECTOR_8B_POINTER (flags)));
}

DEFINE_PRIMITIVE ("FLOAT-ENVIRONMENT", Prim_float_environment, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETENV
  {
    fenv_t *envp;
    SCHEME_OBJECT environment = (allocate_fenv (&envp));
    if (0 != (fegetenv (envp)))
      error_external_return ();
    PRIMITIVE_RETURN (environment);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("SET-FLOAT-ENVIRONMENT", Prim_set_float_environment, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FESETENV
  if (0 != (fesetenv (arg_fenv (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEFER-FLOAT-EXCEPTIONS", Prim_defer_float_exceptions, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEHOLDEXCEPT
  {
    fenv_t *envp;
    SCHEME_OBJECT environment = (allocate_fenv (&envp));
    if (0 != (feholdexcept (envp)))
      error_external_return ();
    cache_float_environment ();
    PRIMITIVE_RETURN (environment);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("UPDATE-FLOAT-ENVIRONMENT", Prim_update_float_environment, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEUPDATEENV
  if (0 != (feupdateenv (arg_fenv (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

enum { FRMODE_NEAREST, FRMODE_TOWARD_ZERO, FRMODE_DOWNWARD, FRMODE_UPWARD };

DEFINE_PRIMITIVE ("FLOAT-ROUNDING-MODES", Prim_float_rounding_modes, 0, 0, 0)
{
  unsigned int modes = 0;
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
#  ifdef FE_TONEAREST
  modes |= (1 << FRMODE_NEAREST);
#  endif
#  ifdef FE_TOWARDZERO
  modes |= (1 << FRMODE_TOWARD_ZERO);
#  endif
#  ifdef FE_DOWNWARD
  modes |= (1 << FRMODE_DOWNWARD);
#  endif
#  ifdef FE_UPWARD
  modes |= (1 << FRMODE_UPWARD);
#  endif
#endif
  PRIMITIVE_RETURN (ulong_to_integer (modes));
}

DEFINE_PRIMITIVE ("GET-FLOAT-ROUNDING-MODE", Prim_get_float_rounding_mode, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
  {
    int mode = (fegetround ());
    if (mode < 0)
      error_external_return ();
    switch (mode)
      {
#ifdef FE_TONEAREST
      case FE_TONEAREST: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_NEAREST));
#endif
#ifdef FE_TOWARDZERO
      case FE_TOWARDZERO: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_TOWARD_ZERO));
#endif
#ifdef FE_DOWNWARD
      case FE_DOWNWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_DOWNWARD));
#endif
#ifdef FE_UPWARD
      case FE_UPWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_UPWARD));
#endif
      default: PRIMITIVE_RETURN (SHARP_F);
      }
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("SET-FLOAT-ROUNDING-MODE", Prim_set_float_rounding_mode, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FESETROUND
  {
    int mode = (-1);
    switch (arg_ulong_integer (1))
      {
#ifdef FE_TONEAREST
      case FRMODE_NEAREST: mode = FE_TONEAREST; break;
#endif
#ifdef FE_TOWARDZERO
      case FRMODE_TOWARD_ZERO: mode = FE_TOWARDZERO; break;
#endif
#ifdef FE_DOWNWARD
      case FRMODE_DOWNWARD: mode = FE_DOWNWARD; break;
#endif
#ifdef FE_UPWARD
      case FRMODE_UPWARD: mode = FE_UPWARD; break;
#endif
      default: error_bad_range_arg (1); break;
      }
    if ((fesetround (mode)) != 0)
      error_external_return ();
    cache_float_environment ();
  }
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* The following two definitions could be replaced by a more complex
   mapping between the system's representation of exception sets and a
   system-independent representation of them for Scheme, like the
   rounding modes above.  While OS-independent representations are
   generally good, (a) machine-dependent representations don't matter
   much, (b) the system-dependent representations are likely to be
   machine-dependent but OS-independent, and (c) it would be nice to
   open-code all the floating-point environment hackery.  */

#define FLOAT_EXCEPTIONS_RESULT(EXCEPTIONS)			\
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (EXCEPTIONS))

static int
arg_float_exceptions (int n)
{
  CHECK_ARG (n, UNSIGNED_FIXNUM_P);
  {
    unsigned long scheme_exceptions = (FIXNUM_TO_ULONG (ARG_REF (n)));
    if (scheme_exceptions &~ FE_ALL_EXCEPT)
      error_bad_range_arg (n);
    return (scheme_exceptions);
  }
}

/* It is not safe to run Scheme with the inexact result exception
   unmasked, but the exception can sometimes be useful to test.
   Consequently, we go to some trouble to make sure that it is masked,
   and signal an error if anyone ever tries to unmask it.  */

static const int always_masked_exceptions = 0
#ifdef FE_INEXACT
  | FE_INEXACT
#endif
  ;

static int
arg_maskable_float_exceptions (int n)
{
  return (always_masked_exceptions | (arg_float_exceptions (n)));
}

static int
arg_unmaskable_float_exceptions (int n)
{
  int exceptions = (arg_float_exceptions (n));
  if (exceptions & always_masked_exceptions)
    error_bad_range_arg (n);
  return (exceptions);
}

static int
arg_float_exception_mask (int n)
{
  int exceptions = (arg_float_exceptions (n));
  if (! (exceptions & always_masked_exceptions))
    error_bad_range_arg (n);
  return (exceptions);
}

#define FLOAT_EXCEPTIONS_PRIMITIVE(E)	\
{					\
  PRIMITIVE_HEADER (0);			\
  FLOAT_EXCEPTIONS_RESULT (E);		\
}

#define UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE()	\
{							\
  PRIMITIVE_HEADER (0);					\
  /* error_unimplemented_primitive (); */		\
  FLOAT_EXCEPTIONS_RESULT (0);				\
}

DEFINE_PRIMITIVE ("FLOAT-INVALID-OPERATION-EXCEPTION", Prim_float_invalid_operation_exception, 0, 0, 0)
#ifdef FE_INVALID
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_INVALID)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-DIVIDE-BY-ZERO-EXCEPTION", Prim_float_divide_by_zero_exception, 0, 0, 0)
#ifdef FE_DIVBYZERO
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_DIVBYZERO)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-OVERFLOW-EXCEPTION", Prim_float_overflow_exception, 0, 0, 0)
#ifdef FE_OVERFLOW
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_OVERFLOW)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-UNDERFLOW-EXCEPTION", Prim_float_underflow_exception, 0, 0, 0)
#ifdef FE_UNDERFLOW
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_UNDERFLOW)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-INEXACT-RESULT-EXCEPTION", Prim_float_inexact_result_exception, 0, 0, 0)
#ifdef FE_INEXACT
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_INEXACT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-EXCEPTIONS", Prim_float_exceptions, 0, 0, 0)
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_ALL_EXCEPT)

DEFINE_PRIMITIVE ("UNMASKABLE-FLOAT-EXCEPTIONS", Prim_unmaskable_float_exceptions, 0, 0, 0)
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_ALL_EXCEPT &~ always_masked_exceptions)

DEFINE_PRIMITIVE ("TEST-FLOAT-EXCEPTIONS", Prim_test_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FETESTEXCEPT
  FLOAT_EXCEPTIONS_RESULT (fetestexcept (arg_float_exceptions (1)));
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("CLEAR-FLOAT-EXCEPTIONS", Prim_clear_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FECLEAREXCEPT
  if (0 != (feclearexcept (arg_float_exceptions (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RAISE-FLOAT-EXCEPTIONS", Prim_raise_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FERAISEEXCEPT
  if (0 != (feraiseexcept (arg_float_exceptions (1))))
    error_external_return ();
  /* cache_float_environment (); */
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SAVE-FLOAT-EXCEPTION-FLAGS", Prim_save_float_exception_flags, 1, 1, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETEXCEPTFLAG
  {
    fexcept_t *flagp;
    SCHEME_OBJECT flags = (allocate_fexcept (&flagp));
    if (0 != (fegetexceptflag (flagp, (arg_float_exceptions (1)))))
      error_external_return ();
    PRIMITIVE_RETURN (flags);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("TEST-FLOAT-EXCEPTION-FLAGS", Prim_test_float_exception_flags, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  /* Oops!  IEEE 754-2008 requests this operation, but C99 doesn't
     provide it.  */
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RESTORE-FLOAT-EXCEPTION-FLAGS", Prim_restore_float_exception_flags, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
#ifdef HAVE_FESETEXCEPTFLAG
  if (0 > (fesetexceptflag ((arg_fexcept (1)), (arg_float_exceptions (2)))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("MASKED-FLOAT-EXCEPTIONS", Prim_masked_float_exceptions, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETEXCEPT
  {
    int exceptions = (fegetexcept ());
    if (exceptions < 0) error_external_return ();
    FLOAT_EXCEPTIONS_RESULT (FE_ALL_EXCEPT &~ exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("SET-MASKED-FLOAT-EXCEPTIONS", Prim_set_masked_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#if ((defined (HAVE_FEENABLEEXCEPT)) && (defined (HAVE_FEDISABLEEXCEPT)))
  {
    int masked_exceptions = (arg_float_exception_mask (1));
    int previous_exceptions = (fedisableexcept (masked_exceptions));
    if ((0 > previous_exceptions)
	|| (0 > (feenableexcept (FE_ALL_EXCEPT &~ masked_exceptions))))
      error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (FE_ALL_EXCEPT &~ previous_exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("MASK-FLOAT-EXCEPTIONS", Prim_mask_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEDISABLEEXCEPT
  {
    int exceptions = (arg_maskable_float_exceptions (1));
    int previous_exceptions = (fedisableexcept (exceptions));
    if (previous_exceptions < 0) error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (FE_ALL_EXCEPT &~ previous_exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("UNMASK-FLOAT-EXCEPTIONS", Prim_unmask_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEENABLEEXCEPT
  {
    int exceptions = (arg_unmaskable_float_exceptions (1));
    int previous_exceptions = (feenableexcept (exceptions));
    if (previous_exceptions < 0) error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (FE_ALL_EXCEPT &~ previous_exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}
