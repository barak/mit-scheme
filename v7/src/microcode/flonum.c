/* -*-C-*-

$Id: flonum.c,v 9.47 2007/01/05 21:19:25 cph Exp $

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

/* Floating Point Arithmetic */

#include "scheme.h"
#include "prims.h"
#include "zones.h"
#include <errno.h>

double
DEFUN (arg_flonum, (arg_number), int arg_number)
{
  SCHEME_OBJECT argument = (ARG_REF (arg_number));
  if (! (FLONUM_P (argument)))
    error_wrong_type_arg (arg_number);
  return (FLONUM_TO_DOUBLE (argument));
}

#define FLONUM_RESULT(x) PRIMITIVE_RETURN (double_to_flonum (x))
#define BOOLEAN_RESULT(x) PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (x))

SCHEME_OBJECT
DEFUN (double_to_flonum, (value), double value)
{
  ALIGN_FLOAT (Free);
  Primitive_GC_If_Needed (FLONUM_SIZE + 1);
  {
    SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, Free));
    (*Free++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, FLONUM_SIZE));
    (*((double *) Free)) = value;
    Free += FLONUM_SIZE;
    return (result);
  }
}

#define FLONUM_BINARY_OPERATION(operator)				\
{									\
  PRIMITIVE_HEADER (2);							\
  Set_Time_Zone (Zone_Math);						\
  FLONUM_RESULT ((arg_flonum (1)) operator (arg_flonum (2)));		\
}

DEFINE_PRIMITIVE ("FLONUM-ADD", Prim_flonum_add, 2, 2, 0)
     FLONUM_BINARY_OPERATION (+)
DEFINE_PRIMITIVE ("FLONUM-SUBTRACT", Prim_flonum_subtract, 2, 2, 0)
     FLONUM_BINARY_OPERATION (-)
DEFINE_PRIMITIVE ("FLONUM-MULTIPLY", Prim_flonum_multiply, 2, 2, 0)
     FLONUM_BINARY_OPERATION (*)

DEFINE_PRIMITIVE ("FLONUM-DIVIDE", Prim_flonum_divide, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast double denominator = (arg_flonum (2));
    if (denominator == 0)
      error_bad_range_arg (2);
    FLONUM_RESULT ((arg_flonum (1)) / denominator);
  }
}

DEFINE_PRIMITIVE ("FLONUM-NEGATE", Prim_flonum_negate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Set_Time_Zone (Zone_Math);
  FLONUM_RESULT (- (arg_flonum (1)));
}

DEFINE_PRIMITIVE ("FLONUM-ABS", Prim_flonum_abs, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Set_Time_Zone (Zone_Math);
  {
    fast double x = (arg_flonum (1));
    FLONUM_RESULT ((x < 0) ? (-x) : x);
  }
}

#define FLONUM_BINARY_PREDICATE(operator)				\
{									\
  PRIMITIVE_HEADER (2);							\
  Set_Time_Zone (Zone_Math);						\
  BOOLEAN_RESULT ((arg_flonum (1)) operator (arg_flonum (2)));		\
}

DEFINE_PRIMITIVE ("FLONUM-EQUAL?", Prim_flonum_equal_p, 2, 2, 0)
     FLONUM_BINARY_PREDICATE (==)
DEFINE_PRIMITIVE ("FLONUM-LESS?", Prim_flonum_less_p, 2, 2, 0)
     FLONUM_BINARY_PREDICATE (<)
DEFINE_PRIMITIVE ("FLONUM-GREATER?", Prim_flonum_greater_p, 2, 2, 0)
     FLONUM_BINARY_PREDICATE (>)

#define FLONUM_UNARY_PREDICATE(operator)				\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  BOOLEAN_RESULT ((arg_flonum (1)) operator 0);				\
}

DEFINE_PRIMITIVE ("FLONUM-ZERO?", Prim_flonum_zero_p, 1, 1, 0)
     FLONUM_UNARY_PREDICATE (==)
DEFINE_PRIMITIVE ("FLONUM-POSITIVE?", Prim_flonum_positive_p, 1, 1, 0)
     FLONUM_UNARY_PREDICATE (>)
DEFINE_PRIMITIVE ("FLONUM-NEGATIVE?", Prim_flonum_negative_p, 1, 1, 0)
     FLONUM_UNARY_PREDICATE (<)

#define SIMPLE_TRANSCENDENTAL_FUNCTION(function)			\
{									\
  extern double EXFUN (function, (double));				\
  double result;							\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  errno = 0;								\
  result = (function (arg_flonum (1)));					\
  if (errno != 0)							\
    error_bad_range_arg (1);						\
  FLONUM_RESULT (result);						\
}

#define RESTRICTED_TRANSCENDENTAL_FUNCTION(function, restriction)	\
{									\
  extern double EXFUN (function, (double));				\
  double x;								\
  double result;							\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  x = (arg_flonum (1));							\
  if (! (restriction))							\
    error_bad_range_arg (1);						\
  errno = 0;								\
  result = (function (x));						\
  if (errno != 0)							\
    error_bad_range_arg (1);						\
  FLONUM_RESULT (result);						\
}

DEFINE_PRIMITIVE ("FLONUM-EXP", Prim_flonum_exp, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (exp)
DEFINE_PRIMITIVE ("FLONUM-LOG", Prim_flonum_log, 1, 1, 0)
     RESTRICTED_TRANSCENDENTAL_FUNCTION (log, (x > 0))
DEFINE_PRIMITIVE ("FLONUM-SIN", Prim_flonum_sin, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (sin)
DEFINE_PRIMITIVE ("FLONUM-COS", Prim_flonum_cos, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (cos)
DEFINE_PRIMITIVE ("FLONUM-TAN", Prim_flonum_tan, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (tan)
DEFINE_PRIMITIVE ("FLONUM-ASIN", Prim_flonum_asin, 1, 1, 0)
     RESTRICTED_TRANSCENDENTAL_FUNCTION (asin, ((x >= -1) && (x <= 1)))
DEFINE_PRIMITIVE ("FLONUM-ACOS", Prim_flonum_acos, 1, 1, 0)
     RESTRICTED_TRANSCENDENTAL_FUNCTION (acos, ((x >= -1) && (x <= 1)))
DEFINE_PRIMITIVE ("FLONUM-ATAN", Prim_flonum_atan, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (atan)

DEFINE_PRIMITIVE ("FLONUM-ATAN2", Prim_flonum_atan2, 2, 2, 0)
{
  extern double EXFUN (atan2, (double, double));
  PRIMITIVE_HEADER (2);
  {
    fast double y = (arg_flonum (1));
    fast double x = (arg_flonum (2));
    if ((x == 0) && (y == 0))
      error_bad_range_arg (2);
    FLONUM_RESULT (atan2 (y, x));
  }
}

DEFINE_PRIMITIVE ("FLONUM-SQRT", Prim_flonum_sqrt, 1, 1, 0)
     RESTRICTED_TRANSCENDENTAL_FUNCTION (sqrt, (x >= 0))

DEFINE_PRIMITIVE ("FLONUM-EXPT", Prim_flonum_expt, 2, 2, 0)
{
  extern double EXFUN (pow, (double, double));
  PRIMITIVE_HEADER (2);
  FLONUM_RESULT (pow ((arg_flonum (1)), (arg_flonum (2))));
}

DEFINE_PRIMITIVE ("FLONUM?", Prim_flonum_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (FLONUM_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("FLONUM-INTEGER?", Prim_flonum_integer_p, 1, 1, 0)
{
  extern Boolean EXFUN (flonum_integer_p, (SCHEME_OBJECT));
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FLONUM_P);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (flonum_integer_p (ARG_REF (1))));
}

#define FLONUM_CONVERSION(converter)					\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, FLONUM_P);						\
  PRIMITIVE_RETURN (converter (ARG_REF (1)));				\
}

DEFINE_PRIMITIVE ("FLONUM-FLOOR", Prim_flonum_floor, 1, 1, 0)
     FLONUM_CONVERSION (flonum_floor)
DEFINE_PRIMITIVE ("FLONUM-CEILING", Prim_flonum_ceiling, 1, 1, 0)
     FLONUM_CONVERSION (flonum_ceiling)
DEFINE_PRIMITIVE ("FLONUM-TRUNCATE", Prim_flonum_truncate, 1, 1, 0)
     FLONUM_CONVERSION (FLONUM_TRUNCATE)
DEFINE_PRIMITIVE ("FLONUM-ROUND", Prim_flonum_round, 1, 1, 0)
     FLONUM_CONVERSION (flonum_round)

DEFINE_PRIMITIVE ("FLONUM-TRUNCATE->EXACT", Prim_flonum_truncate_to_exact, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, FLONUM_P);
  PRIMITIVE_RETURN (FLONUM_TO_INTEGER (ARG_REF (1))); 
}

#define FLONUM_EXACT_CONVERSION(converter)				\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  CHECK_ARG (1, FLONUM_P);						\
  PRIMITIVE_RETURN (FLONUM_TO_INTEGER (converter (ARG_REF (1))));	\
}
DEFINE_PRIMITIVE ("FLONUM-FLOOR->EXACT", Prim_flonum_floor_to_exact, 1, 1, 0)
     FLONUM_EXACT_CONVERSION (flonum_floor)
DEFINE_PRIMITIVE ("FLONUM-CEILING->EXACT", Prim_flonum_ceiling_to_exact, 1, 1, 0)
     FLONUM_EXACT_CONVERSION (flonum_ceiling)
DEFINE_PRIMITIVE ("FLONUM-ROUND->EXACT", Prim_flonum_round_to_exact, 1, 1, 0)
     FLONUM_EXACT_CONVERSION (flonum_round)

DEFINE_PRIMITIVE ("FLONUM-NORMALIZE", Prim_flonum_normalize, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, FLONUM_P);
  PRIMITIVE_RETURN (flonum_normalize (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("FLONUM-DENORMALIZE", Prim_flonum_denormalize, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  CHECK_ARG (1, FLONUM_P);
  CHECK_ARG (2, INTEGER_P);
  PRIMITIVE_RETURN (flonum_denormalize ((ARG_REF (1)), (ARG_REF (2))));
}
