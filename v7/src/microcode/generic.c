/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/generic.c,v 9.30 1989/09/20 23:08:54 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "prims.h"
#include "zones.h"

/* Complex Number Macros.  Should have its own file. */

#define REAL_PART(arg) (MEMORY_REF ((arg), COMPLEX_REAL))
#define IMAG_PART(arg) (MEMORY_REF ((arg), COMPLEX_IMAG))

#define COERCE_REAL_PART(arg)						\
  ((COMPLEX_P (arg)) ? (REAL_PART (arg)) : (arg))

#define COERCE_IMAG_PART(arg)						\
  ((COMPLEX_P (arg)) ? (IMAG_PART (arg)) : FIXNUM_ZERO)

#define RETURN_COMPLEX(real, imag)					\
{									\
  SCHEME_OBJECT _real_value = (real);					\
  SCHEME_OBJECT _imag_value = (imag);					\
  PRIMITIVE_RETURN							\
    ((real_zero_p (_imag_value))					\
     ? _real_value							\
     : (system_pair_cons (TC_COMPLEX, _real_value, _imag_value)));	\
}

static double
bignum_to_double_1 (bignum)
     SCHEME_OBJECT bignum;
{
  if (! (BIGNUM_TO_DOUBLE_P (bignum)))
    signal_error_from_primitive (ERR_ARG_1_FAILED_COERCION);
  return (bignum_to_double (bignum));
}

static double
bignum_to_double_2 (bignum)
     SCHEME_OBJECT bignum;
{
  if (! (BIGNUM_TO_DOUBLE_P (bignum)))
    signal_error_from_primitive (ERR_ARG_2_FAILED_COERCION);
  return (bignum_to_double (bignum));
}

static Boolean
real_zero_p (number)
     fast SCHEME_OBJECT number;
{
  switch (OBJECT_TYPE (number))
    {
    case TC_FIXNUM:
      return (FIXNUM_ZERO_P (number));
    case TC_BIG_FLONUM:
      return ((FLONUM_TO_DOUBLE (number)) == 0);
    case TC_BIG_FIXNUM:
      return (BIGNUM_ZERO_P (number));
    default:
      error_wrong_type_arg (1);
    }
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("ZERO?", Prim_zero, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT number = (ARG_REF (1));
    PRIMITIVE_RETURN
      (BOOLEAN_TO_OBJECT
       ((COMPLEX_P (number))
	? ((real_zero_p (REAL_PART (number))) &&
	   (real_zero_p (IMAG_PART (number))))
	: (real_zero_p (number))));
  }
}

#define SIGN_CHECK(operator, bignum_operator)				\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  {									\
    fast SCHEME_OBJECT number = (ARG_REF (1));				\
    switch (OBJECT_TYPE (number))					\
      {									\
      case TC_FIXNUM:							\
	PRIMITIVE_RETURN						\
	  (BOOLEAN_TO_OBJECT ((FIXNUM_TO_LONG (number)) operator 0));	\
									\
      case TC_BIG_FLONUM:						\
	PRIMITIVE_RETURN						\
	  (BOOLEAN_TO_OBJECT						\
	   ((FLONUM_TO_DOUBLE (number)) operator 0));			\
									\
      case TC_BIG_FIXNUM:						\
	PRIMITIVE_RETURN						\
	  (BOOLEAN_TO_OBJECT (bignum_operator (number)));		\
									\
      default:								\
	error_wrong_type_arg (1);					\
      }									\
  }									\
}

DEFINE_PRIMITIVE ("POSITIVE?", Prim_positive, 1, 1, 0)
     SIGN_CHECK (>, BIGNUM_POSITIVE_P)

DEFINE_PRIMITIVE ("NEGATIVE?", Prim_negative, 1, 1, 0)
     SIGN_CHECK (<, BIGNUM_NEGATIVE_P)

static SCHEME_OBJECT
real_add_constant (number, offset)
     fast SCHEME_OBJECT number;
     fast long offset;
{
  return
    ((FIXNUM_P (number))
     ? (long_to_integer ((FIXNUM_TO_LONG (number)) + offset))
     : (BIGNUM_P (number))
     ? (bignum_to_integer (bignum_add (number, (long_to_bignum (offset)))))
     : (double_to_flonum ((FLONUM_TO_DOUBLE (number)) + ((double) offset))));
}

DEFINE_PRIMITIVE ("1+", Prim_add_one, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT number = (ARG_REF (1));
    PRIMITIVE_RETURN
      ((REAL_P (number))
       ? (real_add_constant (number, 1))
       : (COMPLEX_P (number))
       ? (system_pair_cons
	  (TC_COMPLEX,
	   (real_add_constant ((REAL_PART (number)), 1)),
	   (IMAG_PART (number))))
       : ((error_wrong_type_arg (1)), ((SCHEME_OBJECT) 0)));
  }
}

DEFINE_PRIMITIVE ("-1+", Prim_subtract_one, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT number = (ARG_REF (1));
    PRIMITIVE_RETURN
      ((REAL_P (number))
       ? (real_add_constant (number, -1))
       : (COMPLEX_P (number))
       ? (system_pair_cons
	  (TC_COMPLEX,
	   (real_add_constant ((REAL_PART (number)), -1)),
	   (IMAG_PART (number))))
       : ((error_wrong_type_arg (1)), ((SCHEME_OBJECT) 0)));
  }
}

#define TWO_OP_COMPARATOR(GENERAL_OP, BIGNUM_OP)			\
{									\
  switch (OBJECT_TYPE (Arg1))						\
    {									\
    case TC_FIXNUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return							\
	      ((FIXNUM_TO_LONG (Arg1)) GENERAL_OP			\
	       (FIXNUM_TO_LONG (Arg2)));				\
	  case TC_BIG_FLONUM:						\
	    return							\
	      ((FIXNUM_TO_DOUBLE (Arg1)) GENERAL_OP			\
	       (FLONUM_TO_DOUBLE (Arg2)));				\
	  case TC_BIG_FIXNUM:						\
	    return (BIGNUM_OP ((FIXNUM_TO_BIGNUM (Arg1)), Arg2));	\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    case TC_BIG_FLONUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return							\
	      ((FLONUM_TO_DOUBLE (Arg1)) GENERAL_OP			\
	       (FIXNUM_TO_DOUBLE (Arg2)));				\
	  case TC_BIG_FLONUM:						\
	    return							\
	      ((FLONUM_TO_DOUBLE (Arg1)) GENERAL_OP			\
	       (FLONUM_TO_DOUBLE (Arg2)));				\
	  case TC_BIG_FIXNUM:						\
	    return							\
	      ((FLONUM_TO_DOUBLE (Arg1)) GENERAL_OP			\
	       (bignum_to_double_2 (Arg2)));				\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    case TC_BIG_FIXNUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return (BIGNUM_OP (Arg1, (FIXNUM_TO_BIGNUM (Arg2))));	\
	  case TC_BIG_FLONUM:						\
	    return							\
	      ((bignum_to_double_1 (Arg1)) GENERAL_OP			\
	       (FLONUM_TO_DOUBLE (Arg2)));				\
	  case TC_BIG_FIXNUM:						\
	    return (BIGNUM_OP (Arg1, Arg2));				\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    default:								\
      error_wrong_type_arg (1);						\
    }									\
}

static Boolean
real_equal_p (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  TWO_OP_COMPARATOR (==, bignum_equal_p);
}

static Boolean
real_less_p (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  TWO_OP_COMPARATOR (<, BIGNUM_LESS_P);
}

DEFINE_PRIMITIVE ("&=", Prim_equal_number, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT Arg1 = (ARG_REF (1));
    fast SCHEME_OBJECT Arg2 = (ARG_REF (2));
    PRIMITIVE_RETURN
      (BOOLEAN_TO_OBJECT
       ((COMPLEX_P (Arg1))
	? ((COMPLEX_P (Arg2)) &&
	   (real_equal_p ((REAL_PART (Arg1)), (REAL_PART (Arg2)))) &&
	   (real_equal_p ((IMAG_PART (Arg1)), (IMAG_PART (Arg2)))))
	: ((! (COMPLEX_P (Arg2))) &&
	   (real_equal_p (Arg1, Arg2)))));
  }
}

DEFINE_PRIMITIVE ("&<", Prim_less, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (real_less_p ((ARG_REF (1)), (ARG_REF (2)))));
}

DEFINE_PRIMITIVE ("&>", Prim_greater, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (real_less_p ((ARG_REF (2)), (ARG_REF (1)))));
}

#define TWO_OP_OPERATOR(FIXNUM_OP, FLONUM_OP, BIGNUM_OP)		\
{									\
  switch (OBJECT_TYPE (Arg1))						\
    {									\
    case TC_FIXNUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return (FIXNUM_OP (Arg1, Arg2));				\
	  case TC_BIG_FLONUM:						\
	    return							\
	      (double_to_flonum						\
	       ((FIXNUM_TO_DOUBLE (Arg1)) FLONUM_OP			\
		(FLONUM_TO_DOUBLE (Arg2))));				\
	  case TC_BIG_FIXNUM:						\
	    return							\
	      (bignum_to_integer					\
	       (BIGNUM_OP ((FIXNUM_TO_BIGNUM (Arg1)), Arg2)));		\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    case TC_BIG_FLONUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return							\
	      (double_to_flonum						\
	       ((FLONUM_TO_DOUBLE (Arg1)) FLONUM_OP			\
		(FIXNUM_TO_DOUBLE (Arg2))));				\
	  case TC_BIG_FLONUM:						\
	    return							\
	      (double_to_flonum						\
	       ((FLONUM_TO_DOUBLE (Arg1)) FLONUM_OP			\
		(FLONUM_TO_DOUBLE (Arg2))));				\
	  case TC_BIG_FIXNUM:						\
	    return							\
	      (double_to_flonum						\
	       ((FLONUM_TO_DOUBLE (Arg1)) FLONUM_OP			\
		(bignum_to_double_2 (Arg2))));				\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    case TC_BIG_FIXNUM:							\
      {									\
	switch (OBJECT_TYPE (Arg2))					\
	  {								\
	  case TC_FIXNUM:						\
	    return							\
	      (bignum_to_integer					\
	       (BIGNUM_OP (Arg1, (FIXNUM_TO_BIGNUM (Arg2)))));		\
	  case TC_BIG_FLONUM:						\
	    return							\
	      (double_to_flonum						\
	       ((bignum_to_double_1 (Arg1)) FLONUM_OP			\
		(FLONUM_TO_DOUBLE (Arg2))));				\
	  case TC_BIG_FIXNUM:						\
	    return (bignum_to_integer (BIGNUM_OP (Arg1, Arg2)));	\
	  default:							\
	    error_wrong_type_arg (2);					\
	  }								\
      }									\
    default:								\
      error_wrong_type_arg (1);						\
    }									\
}

#define FIXNUM_ADD(x, y)						\
  (long_to_integer ((FIXNUM_TO_LONG (x)) + (FIXNUM_TO_LONG (y))))

#define FIXNUM_SUBTRACT(x, y)						\
  (long_to_integer ((FIXNUM_TO_LONG (x)) - (FIXNUM_TO_LONG (y))))

static SCHEME_OBJECT
fixnum_multiply (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  extern SCHEME_OBJECT Mul ();
  fast SCHEME_OBJECT result = (Mul (Arg1, Arg2));
  return
    ((result == SHARP_F)
     ? (bignum_multiply ((FIXNUM_TO_BIGNUM (Arg1)), (FIXNUM_TO_BIGNUM (Arg2))))
     : result);
}

static SCHEME_OBJECT
real_add (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  TWO_OP_OPERATOR (FIXNUM_ADD, +, bignum_add);
}

static SCHEME_OBJECT
real_subtract (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  TWO_OP_OPERATOR (FIXNUM_SUBTRACT, -, bignum_subtract);
}

static SCHEME_OBJECT
real_multiply (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  TWO_OP_OPERATOR (fixnum_multiply, *, bignum_multiply);
}

DEFINE_PRIMITIVE ("&+", Prim_add, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT Arg1 = (ARG_REF (1));
    fast SCHEME_OBJECT Arg2 = (ARG_REF (2));
    if ((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
      RETURN_COMPLEX
	((real_add ((COERCE_REAL_PART (Arg1)), (COERCE_REAL_PART (Arg2)))),
	 (real_add ((COERCE_IMAG_PART (Arg1)), (COERCE_IMAG_PART (Arg2)))));
    PRIMITIVE_RETURN (real_add (Arg1, Arg2));
  }
}

DEFINE_PRIMITIVE ("&-", Prim_subtract, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT Arg1 = (ARG_REF (1));
    fast SCHEME_OBJECT Arg2 = (ARG_REF (2));
    if ((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
      RETURN_COMPLEX
	((real_subtract ((COERCE_REAL_PART (Arg1)),
			 (COERCE_REAL_PART (Arg2)))),
	 (real_subtract ((COERCE_IMAG_PART (Arg1)),
			 (COERCE_IMAG_PART (Arg2)))));
    PRIMITIVE_RETURN (real_subtract (Arg1, Arg2));
  }
}

static SCHEME_OBJECT
complex_multiply (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  RETURN_COMPLEX
    ((real_subtract ((real_multiply ((COERCE_REAL_PART (Arg1)),
				     (COERCE_REAL_PART (Arg2)))),
		     (real_multiply ((COERCE_IMAG_PART (Arg1)),
				     (COERCE_IMAG_PART (Arg2)))))),
     (real_add ((real_multiply ((COERCE_REAL_PART (Arg1)),
				(COERCE_IMAG_PART (Arg2)))),
		(real_multiply ((COERCE_REAL_PART (Arg2)),
				(COERCE_IMAG_PART (Arg1)))))));
}

DEFINE_PRIMITIVE ("&*", Prim_multiply, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT Arg1 = (ARG_REF (1));
    fast SCHEME_OBJECT Arg2 = (ARG_REF (2));
    PRIMITIVE_RETURN
      (((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
       ? (complex_multiply (Arg1, Arg2))
       : (real_multiply (Arg1, Arg2)));
  }
}

#define FLONUM_DIVIDE(numerator, denominator)				\
{									\
  fast double _denominator = (denominator);				\
  if (_denominator == 0)						\
    error_bad_range_arg (2);						\
  return (double_to_flonum ((numerator) / _denominator));		\
}

static SCHEME_OBJECT
bignum_real_divide (numerator, denominator)
     fast SCHEME_OBJECT numerator;
     fast SCHEME_OBJECT denominator;
{
  SCHEME_OBJECT quotient;
  SCHEME_OBJECT remainder;
  if (bignum_divide (numerator, denominator, (&quotient), (&remainder)))
    error_bad_range_arg (2);
  return
    ((BIGNUM_ZERO_P (remainder))
     ? (bignum_to_integer (quotient))
     : (double_to_flonum
	((bignum_to_double_1 (numerator)) /
	 (bignum_to_double_2 (denominator)))));
}

static SCHEME_OBJECT
real_divide (Arg1, Arg2)
     fast SCHEME_OBJECT Arg1;
     fast SCHEME_OBJECT Arg2;
{
  switch (OBJECT_TYPE (Arg1))
    {
    case TC_FIXNUM:
      {
	switch (OBJECT_TYPE (Arg2))
	  {
	  case TC_FIXNUM:
	    {
	      fast long A = (FIXNUM_TO_LONG (Arg1));
	      fast long B = (FIXNUM_TO_LONG (Arg2));
	      if (B == 0)
		error_bad_range_arg (2);
	      return
		(((A % B) == 0)
		 ? (long_to_integer ((long) (A / B)))
		 : (double_to_flonum (((double) A) / ((double) B))));
	    }
	  case TC_BIG_FLONUM:
	    FLONUM_DIVIDE
	      ((FIXNUM_TO_DOUBLE (Arg1)), (FLONUM_TO_DOUBLE (Arg2)));
	  case TC_BIG_FIXNUM:
	    return (bignum_real_divide ((FIXNUM_TO_BIGNUM (Arg1)), Arg2));
	  default:
	    error_wrong_type_arg (2);
	  }
	/*NOTREACHED*/
      }
    case TC_BIG_FLONUM:
      {
	switch (OBJECT_TYPE (Arg2))
	  {
	  case TC_FIXNUM:
	    FLONUM_DIVIDE
	      ((FLONUM_TO_DOUBLE (Arg1)), (FIXNUM_TO_DOUBLE (Arg2)));
	  case TC_BIG_FLONUM:
	    FLONUM_DIVIDE
	      ((FLONUM_TO_DOUBLE (Arg1)), (FLONUM_TO_DOUBLE (Arg2)));
	  case TC_BIG_FIXNUM:
	    FLONUM_DIVIDE
	      ((FLONUM_TO_DOUBLE (Arg1)), (bignum_to_double_2 (Arg2)));
	  default:
	    error_wrong_type_arg (2);
	  }
	/*NOTREACHED*/
      }
    case TC_BIG_FIXNUM:
      {
	switch (OBJECT_TYPE (Arg2))
	  {
	  case TC_FIXNUM:
	    return (bignum_real_divide (Arg1, (FIXNUM_TO_BIGNUM (Arg2))));
	  case TC_BIG_FLONUM:
	    FLONUM_DIVIDE
	      ((bignum_to_double_1 (Arg1)), (FLONUM_TO_DOUBLE (Arg2)));
	  case TC_BIG_FIXNUM:
	    return (bignum_real_divide (Arg1, Arg2));
	  default:
	    error_wrong_type_arg (2);
	  }
	/*NOTREACHED*/
      }
    default:
      error_wrong_type_arg (1);
    }
  /*NOTREACHED*/
}

static SCHEME_OBJECT
complex_divide (Arg1, Arg2)
     SCHEME_OBJECT Arg1, Arg2;
{
  fast SCHEME_OBJECT real1 = (COERCE_REAL_PART (Arg1));
  fast SCHEME_OBJECT real2 = (COERCE_REAL_PART (Arg2));
  fast SCHEME_OBJECT imag1 = (COERCE_IMAG_PART (Arg1));
  fast SCHEME_OBJECT imag2 = (COERCE_IMAG_PART (Arg2));
  fast SCHEME_OBJECT temp =
    (real_divide ((LONG_TO_UNSIGNED_FIXNUM (1)),
		  (real_add ((real_multiply (real2, real2)),
			     (real_multiply (imag2, imag2))))));
  RETURN_COMPLEX
    ((real_multiply ((real_add ((real_multiply (real1, real2)),
				(real_multiply (imag1, imag2)))),
		     temp)),
     (real_multiply ((real_subtract ((real_multiply (real2, imag1)),
				     (real_multiply (real1, imag2)))),
		     temp)));
}

DEFINE_PRIMITIVE ("&/", Prim_divide, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  Set_Time_Zone (Zone_Math);
  {
    fast SCHEME_OBJECT Arg1 = (ARG_REF (1));
    fast SCHEME_OBJECT Arg2 = (ARG_REF (2));
    PRIMITIVE_RETURN
      (((COMPLEX_P (Arg1)) || (COMPLEX_P (Arg2)))
       ? (complex_divide (Arg1, Arg2))
       : (real_divide (Arg1, Arg2)));
  }
}

/* Generic sqrt and transcendental functions are created by generalizing
   their floating point counterparts. */

static double
scheme_sqrt (x)
     fast double x;
{
  extern double sqrt ();
  if (x < 0)
    error_bad_range_arg (1);
  return (sqrt (x));
}

static double
scheme_ln (x)
     fast double x;
{
  extern double log ();
  if (x < 0)
    error_bad_range_arg (1);
  return (log (x));
}

extern double exp ();
extern double sin ();
extern double cos ();
extern double atan ();

#define GENERIC_FUNCTION(fun)						\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  {									\
    fast SCHEME_OBJECT number = (ARG_REF (1));				\
    switch (OBJECT_TYPE (number))					\
      {									\
      case TC_FIXNUM:							\
	PRIMITIVE_RETURN						\
	  (double_to_flonum (fun (FIXNUM_TO_DOUBLE (number))));		\
      case TC_BIG_FLONUM:						\
	PRIMITIVE_RETURN						\
	  (double_to_flonum (fun (FLONUM_TO_DOUBLE (number))));		\
      case TC_BIG_FIXNUM:						\
	PRIMITIVE_RETURN						\
	  (double_to_flonum (fun (bignum_to_double_1 (number))));	\
      default:								\
	error_wrong_type_arg (1);					\
      }									\
  }									\
}

DEFINE_PRIMITIVE ("SQRT", Prim_sqrt, 1, 1, 0)
     GENERIC_FUNCTION (scheme_sqrt)
DEFINE_PRIMITIVE ("EXP", Prim_exp, 1, 1, 0)
     GENERIC_FUNCTION (exp)
DEFINE_PRIMITIVE ("LOG", Prim_log, 1, 1, 0)
     GENERIC_FUNCTION (scheme_ln)
DEFINE_PRIMITIVE ("SIN", Prim_sin, 1, 1, 0)
     GENERIC_FUNCTION (sin);
DEFINE_PRIMITIVE ("COS", Prim_cos, 1, 1, 0)
     GENERIC_FUNCTION (cos)
DEFINE_PRIMITIVE ("&ATAN", Prim_arctan, 1, 1, 0)
     GENERIC_FUNCTION (atan)

#define FLONUM_TO_INTEGER_PRIMITIVE(mapping)				\
{									\
  PRIMITIVE_HEADER (1);							\
  Set_Time_Zone (Zone_Math);						\
  {									\
    fast SCHEME_OBJECT number = (ARG_REF (1));				\
    PRIMITIVE_RETURN							\
      ((FLONUM_P (number))						\
       ? (FLONUM_TO_INTEGER (mapping (number)))				\
       : (INTEGER_P (number))						\
       ? number								\
       : ((error_wrong_type_arg (1)), ((SCHEME_OBJECT) 0)));		\
  }									\
}

DEFINE_PRIMITIVE ("TRUNCATE", Prim_truncate, 1, 1, 0)
     FLONUM_TO_INTEGER_PRIMITIVE (FLONUM_TRUNCATE)
DEFINE_PRIMITIVE ("ROUND", Prim_round, 1, 1, 0)
     FLONUM_TO_INTEGER_PRIMITIVE (flonum_round)
DEFINE_PRIMITIVE ("FLOOR", Prim_floor, 1, 1, 0)
     FLONUM_TO_INTEGER_PRIMITIVE (flonum_floor)
DEFINE_PRIMITIVE ("CEILING", Prim_ceiling, 1, 1, 0)
     FLONUM_TO_INTEGER_PRIMITIVE (flonum_ceiling)
