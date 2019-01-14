/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
#include "osscheme.h"		/* error_unimplemented_primitive -- foo */
#include "prims.h"

double
arg_flonum (int arg_number)
{
  SCHEME_OBJECT argument = (ARG_REF (arg_number));
  if (!FLONUM_P (argument))
    error_wrong_type_arg (arg_number);
  return (FLONUM_TO_DOUBLE (argument));
}

#define FLONUM_RESULT(x) PRIMITIVE_RETURN (double_to_flonum (x))
#define BOOLEAN_RESULT(x) PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (x))

SCHEME_OBJECT
double_to_flonum (double value)
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
  {
    double denominator = (arg_flonum (2));
    if (denominator == 0)
      error_bad_range_arg (2);
    FLONUM_RESULT ((arg_flonum (1)) / denominator);
  }
}

DEFINE_PRIMITIVE ("FLONUM-MODULO", Prim_flonum_modulo, 2, 2, 0)
#ifdef HAVE_FMOD
{
  PRIMITIVE_HEADER (2);
  {
    double denominator = (arg_flonum (2));
    if (denominator == 0)
      error_bad_range_arg (2);
    FLONUM_RESULT (fmod ((arg_flonum (1)), denominator));
  }
}
#else
{
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
#endif

DEFINE_PRIMITIVE ("FLONUM-NEGATE", Prim_flonum_negate, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  FLONUM_RESULT (- (arg_flonum (1)));
}

DEFINE_PRIMITIVE ("FLONUM-ABS", Prim_flonum_abs, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    double x = (arg_flonum (1));
    FLONUM_RESULT ((x < 0) ? (-x) : x);
  }
}

#define FLONUM_BINARY_PREDICATE(operator)				\
{									\
  PRIMITIVE_HEADER (2);							\
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
  double result;							\
  PRIMITIVE_HEADER (1);							\
  result = (function (arg_flonum (1)));					\
  FLONUM_RESULT (result);						\
}

#define RESTRICTED_TRANSCENDENTAL_FUNCTION(function, restriction)	\
{									\
  double x;								\
  double result;							\
  PRIMITIVE_HEADER (1);							\
  x = (arg_flonum (1));							\
  if (! (restriction))							\
    error_bad_range_arg (1);						\
  result = (function (x));						\
  FLONUM_RESULT (result);						\
}

DEFINE_PRIMITIVE ("FLONUM-EXPM1", Prim_flonum_expm1, 1, 1, 0)
#ifdef HAVE_EXPM1
     RESTRICTED_TRANSCENDENTAL_FUNCTION
       (expm1, ((x >= - M_LN2) && (x <= M_LN2)))
#else
{
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
#endif

DEFINE_PRIMITIVE ("FLONUM-LOG1P", Prim_flonum_log1p, 1, 1, 0)
#ifdef HAVE_LOG1P
     RESTRICTED_TRANSCENDENTAL_FUNCTION
       (log1p, ((x >= (M_SQRT1_2 - 1.0)) && (x <= (1.0 - M_SQRT1_2))))
#else
{
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
#endif

DEFINE_PRIMITIVE ("FLONUM-LOG", Prim_flonum_log, 1, 1, 0)
{
  double x;
  double result;
  PRIMITIVE_HEADER (1);
  x = (arg_flonum (1));
  result = (log (x));
  FLONUM_RESULT (result);
}

DEFINE_PRIMITIVE ("FLONUM-EXP", Prim_flonum_exp, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (exp)

DEFINE_PRIMITIVE ("FLONUM-SINH", Prim_flonum_sinh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (sinh)
DEFINE_PRIMITIVE ("FLONUM-COSH", Prim_flonum_cosh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (cosh)
DEFINE_PRIMITIVE ("FLONUM-TANH", Prim_flonum_tanh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (tanh)
DEFINE_PRIMITIVE ("FLONUM-ASINH", Prim_flonum_asinh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (asinh)
DEFINE_PRIMITIVE ("FLONUM-ACOSH", Prim_flonum_acosh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (acosh)
DEFINE_PRIMITIVE ("FLONUM-ATANH", Prim_flonum_atanh, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (atanh)

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
  PRIMITIVE_HEADER (2);
  {
    double y = (arg_flonum (1));
    double x = (arg_flonum (2));
    if ((x == 0) && (y == 0))
      error_bad_range_arg (2);
    FLONUM_RESULT (atan2 (y, x));
  }
}

DEFINE_PRIMITIVE ("FLONUM-SQRT", Prim_flonum_sqrt, 1, 1, 0)
     RESTRICTED_TRANSCENDENTAL_FUNCTION (sqrt, (x >= 0))
DEFINE_PRIMITIVE ("FLONUM-CBRT", Prim_flonum_cbrt, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (cbrt)

DEFINE_PRIMITIVE ("FLONUM-HYPOT", Prim_flonum_hypot, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  FLONUM_RESULT (hypot ((arg_flonum (1)), (arg_flonum (2))));
}

DEFINE_PRIMITIVE ("FLONUM-EXPT", Prim_flonum_expt, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  FLONUM_RESULT (pow ((arg_flonum (1)), (arg_flonum (2))));
}

/* (Complete) Gamma and error functions */

DEFINE_PRIMITIVE ("FLONUM-LGAMMA", Prim_flonum_lgamma, 1, 1, 0)
{
  double x;
  double result;
  PRIMITIVE_HEADER (1);

  x = (arg_flonum (1));
#ifdef HAVE_LGAMMA_R
  {
    int sign;
    result = (lgamma_r (x, (&sign)));
  }
#else
  result = (lgamma (x));
#endif
  FLONUM_RESULT (result);
}

DEFINE_PRIMITIVE ("FLONUM-GAMMA", Prim_flonum_gamma, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (tgamma)
DEFINE_PRIMITIVE ("FLONUM-ERF", Prim_flonum_erf, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (erf)
DEFINE_PRIMITIVE ("FLONUM-ERFC", Prim_flonum_erfc, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (erfc)

/* Bessel functions of the first (j*) and second (y*) kind */

DEFINE_PRIMITIVE ("FLONUM-J0", Prim_flonum_j0, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (j0)
DEFINE_PRIMITIVE ("FLONUM-J1", Prim_flonum_j1, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (j1)
DEFINE_PRIMITIVE ("FLONUM-Y0", Prim_flonum_y0, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (y0)
DEFINE_PRIMITIVE ("FLONUM-Y1", Prim_flonum_y1, 1, 1, 0)
     SIMPLE_TRANSCENDENTAL_FUNCTION (y1)

DEFINE_PRIMITIVE ("FLONUM-JN", Prim_flonum_jn, 2, 2, 0)
{
  int n;
  double x;
  PRIMITIVE_HEADER (2);
  n = (arg_integer_in_range (1, INT_MIN, INT_MAX));
  x = (arg_flonum (2));
  FLONUM_RESULT (jn (n, x));
}

DEFINE_PRIMITIVE ("FLONUM-YN", Prim_flonum_yn, 2, 2, 0)
{
  int n;
  double x;
  PRIMITIVE_HEADER (2);
  n = (arg_integer_in_range (1, INT_MIN, INT_MAX));
  x = (arg_flonum (2));
  FLONUM_RESULT (yn (n, x));
}

DEFINE_PRIMITIVE ("FLONUM?", Prim_flonum_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (FLONUM_P (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("FLONUM-INTEGER?", Prim_flonum_integer_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FLONUM_P);
  PRIMITIVE_RETURN
    ((flonum_is_finite_p (ARG_REF (1)))
     ? (BOOLEAN_TO_OBJECT (flonum_integer_p (ARG_REF (1))))
     : false);
}

#define FLONUM_CONVERSION(converter)					\
{									\
  PRIMITIVE_HEADER (1);							\
  CHECK_ARG (1, FLONUM_P);						\
  PRIMITIVE_RETURN							\
    ((flonum_is_finite_p (ARG_REF (1)))					\
     ? (converter (ARG_REF (1)))					\
     : (ARG_REF (1)));							\
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
  CHECK_ARG (1, finite_flonum_p);
  PRIMITIVE_RETURN (FLONUM_TO_INTEGER (ARG_REF (1)));
}

#define FLONUM_EXACT_CONVERSION(converter)				\
{									\
  PRIMITIVE_HEADER (1);							\
  CHECK_ARG (1, finite_flonum_p);					\
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
  CHECK_ARG (1, finite_flonum_p);
  PRIMITIVE_RETURN (flonum_normalize (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("FLONUM-DENORMALIZE", Prim_flonum_denormalize, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, finite_flonum_p);
  CHECK_ARG (2, INTEGER_P);
  PRIMITIVE_RETURN (flonum_denormalize ((ARG_REF (1)), (ARG_REF (2))));
}

/* These conversion primitives don't require IEEE 754, but they do
 * make assumptions about the sizes of doubles and floats.  If we want
 * to support using these primitives with non-IEEE 754 floating-point
 * numbers, we may have to adjust them.
 */

#if defined UINT64_MAX || defined uint64_t
typedef
union
{
  double dbl;
  uint64_t u64;
} double_uint64_t_cast;
#endif

DEFINE_PRIMITIVE ("CAST-IEEE754-DOUBLE-TO-INTEGER", Prim_cast_ieee754_double_to_integer, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#if defined UINT64_MAX || defined uint64_t
  CHECK_ARG (1, FLONUM_P);
  {
    double_uint64_t_cast cast;

    cast.dbl = FLONUM_TO_DOUBLE (ARG_REF (1));

    PRIMITIVE_RETURN (uintmax_to_integer (cast.u64));
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("CAST-INTEGER-TO-IEEE754-DOUBLE", Prim_cast_integer_to_ieee754_double, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#if defined UINT64_MAX || defined uint64_t
  CHECK_ARG (1, INTEGER_P);
  {
    double_uint64_t_cast cast;

    cast.u64 = integer_to_uintmax (ARG_REF (1));

    PRIMITIVE_RETURN (double_to_flonum (cast.dbl));
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

typedef
union
{
  float f;
  uint32_t u32;
} float_uint32_t_cast;

DEFINE_PRIMITIVE ("CAST-IEEE754-SINGLE-TO-INTEGER", Prim_cast_ieee754_single_to_integer, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FLONUM_P);
  {
    float_uint32_t_cast cast;

    cast.f = (float) FLONUM_TO_DOUBLE (ARG_REF (1));

    PRIMITIVE_RETURN (uintmax_to_integer (cast.u32));
  }
}

DEFINE_PRIMITIVE ("CAST-INTEGER-TO-IEEE754-SINGLE", Prim_cast_integer_to_ieee754_single, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    float_uint32_t_cast cast;

    cast.u32 = integer_to_uintmax (ARG_REF (1));

    PRIMITIVE_RETURN (double_to_flonum ((double) cast.f));
  }
}

/* C99 flonum predicates */

DEFINE_PRIMITIVE ("FLONUM-IS-FINITE?", Prim_flonum_is_finite_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (isfinite (arg_flonum (1))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-INFINITE?", Prim_flonum_is_infinite_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (isinf (arg_flonum (1))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-NAN?", Prim_flonum_is_nan_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (isnan (arg_flonum (1))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-NORMAL?", Prim_flonum_is_normal_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (isnormal (arg_flonum (1))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-NEGATIVE?", Prim_flonum_is_negative_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (signbit (arg_flonum (1))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-GREATER?", Prim_flonum_is_greater_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (isgreater ((arg_flonum (1)), (arg_flonum (2)))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-GREATER-OR-EQUAL?", Prim_flonum_is_greater_or_equal_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (isgreaterequal ((arg_flonum (1)), (arg_flonum (2)))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-LESS-OR-EQUAL?", Prim_flonum_is_less_or_equal_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (islessequal ((arg_flonum (1)), (arg_flonum (2)))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-LESS?", Prim_flonum_is_less_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (isless ((arg_flonum (1)), (arg_flonum (2)))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-LESS-OR-GREATER?", Prim_flonum_is_less_or_greater_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (islessgreater ((arg_flonum (1)), (arg_flonum (2)))));
}

DEFINE_PRIMITIVE ("FLONUM-IS-UNORDERED?", Prim_flonum_is_unordered_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (isunordered ((arg_flonum (1)), (arg_flonum (2)))));
}

/* Miscellaneous floating-point operations */

DEFINE_PRIMITIVE ("FLONUM-COPYSIGN", Prim_flonum_copysign, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    double magnitude = (arg_flonum (1));
    double sign = (arg_flonum (2));
    FLONUM_RESULT (copysign (magnitude, sign));
  }
}

DEFINE_PRIMITIVE ("FLONUM-NEXTAFTER", Prim_flonum_nextafter, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    double x = (arg_flonum (1));
    double direction = (arg_flonum (2));
    FLONUM_RESULT (nextafter (x, direction));
  }
}
