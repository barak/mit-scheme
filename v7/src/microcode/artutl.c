/* -*-C-*-

$Id: artutl.c,v 1.10 1992/09/18 19:39:16 jinx Exp $

Copyright (c) 1989-1992 Massachusetts Institute of Technology

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

/* Arithmetic Utilities */

#include "scheme.h"
#include <math.h>

/* Conversions between Scheme types and C types. */

long
DEFUN (fixnum_to_long, (fixnum), SCHEME_OBJECT fixnum)
{
  return (FIXNUM_TO_LONG (fixnum));
}

SCHEME_OBJECT
DEFUN (double_to_fixnum, (value), double value)
{
#ifdef HAVE_DOUBLE_TO_LONG_BUG
  fast long temp = ((long) value);
  return (LONG_TO_FIXNUM (temp));
#else
  return (LONG_TO_FIXNUM ((long) value));
#endif
}

Boolean
DEFUN (integer_to_long_p, (n), fast SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) || (BIGNUM_TO_LONG_P (n)));
}

long
DEFUN (integer_to_long,
       (n),
       fast SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_TO_LONG (n)) : (bignum_to_long (n)));
}

SCHEME_OBJECT
DEFUN (long_to_integer, (number), long number)
{
  return
    ((LONG_TO_FIXNUM_P (number))
     ? (LONG_TO_FIXNUM (number))
     : (long_to_bignum (number)));
}

unsigned long
DEFUN (integer_to_ulong,
       (n),
       fast SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n))
	  ? ((unsigned long) (FIXNUM_TO_LONG (n)))
	  : (bignum_to_ulong (n)));
}

SCHEME_OBJECT
DEFUN (ulong_to_integer, (number), unsigned long number)
{
  long s_number; = ((long) number);
  if (s_number >= 0)
    return
      ((LONG_TO_FIXNUM_P (s_number))
       ? (LONG_TO_FIXNUM (s_number))
       : (long_to_bignum (s_number)));
  else
    return (ulong_to_bignum (number));
}

Boolean
DEFUN (integer_to_double_p, (n), fast SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) || (BIGNUM_TO_DOUBLE_P (n)));
}

double
DEFUN (integer_to_double, (n), fast SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_TO_DOUBLE (n)) : (bignum_to_double (n)));
}

SCHEME_OBJECT
DEFUN (double_to_integer, (x), fast double x)
{
  return
    ((DOUBLE_TO_FIXNUM_P (x))
     ? (DOUBLE_TO_FIXNUM (x))
     : (double_to_bignum (x)));
}

double
DEFUN (double_truncate, (x), fast double x)
{
  double iptr;
  (void) modf (x, (&iptr));
  return (iptr);
}

/* Conversions between Scheme types and Scheme types. */

SCHEME_OBJECT
DEFUN (bignum_to_fixnum, (bignum), fast SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FIXNUM_P (bignum))
     ? (BIGNUM_TO_FIXNUM (bignum))
     : SHARP_F);
}

SCHEME_OBJECT
DEFUN (bignum_to_integer, (bignum), fast SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FIXNUM_P (bignum))
     ? (BIGNUM_TO_FIXNUM (bignum))
     : bignum);
}

SCHEME_OBJECT
DEFUN (bignum_to_flonum, (bignum), fast SCHEME_OBJECT bignum)
{
  return
    ((BIGNUM_TO_FLONUM_P (bignum))
     ? (BIGNUM_TO_FLONUM (bignum))
     : SHARP_F);
}

Boolean
DEFUN (flonum_integer_p, (x), SCHEME_OBJECT x)
{
  extern double EXFUN (modf, (double, double *));
  double iptr;
  return ((modf ((FLONUM_TO_DOUBLE (x)), (&iptr))) == 0);
}

SCHEME_OBJECT
DEFUN (flonum_floor, (x), SCHEME_OBJECT x)
{
  extern double EXFUN (floor, (double));
  return (double_to_flonum (floor (FLONUM_TO_DOUBLE (x))));
}

SCHEME_OBJECT
DEFUN (flonum_ceiling, (x), SCHEME_OBJECT x)
{
  extern double EXFUN (ceil, (double));
  return (double_to_flonum (ceil (FLONUM_TO_DOUBLE (x))));
}

SCHEME_OBJECT
DEFUN (flonum_round,
       (x),
       SCHEME_OBJECT x)
{
  fast double dx = (FLONUM_TO_DOUBLE (x));
  return
    (double_to_flonum (double_truncate ((dx < 0) ? (dx - 0.5) : (dx + 0.5))));
}

SCHEME_OBJECT
DEFUN (flonum_normalize, (x), SCHEME_OBJECT x)
{
  extern double EXFUN (frexp, (double, int *));
  int exponent;
  double significand = (frexp ((FLONUM_TO_DOUBLE (x)), (&exponent)));
  return (cons ((double_to_flonum (significand)),
		(long_to_integer ((long) exponent))));
}

SCHEME_OBJECT
DEFUN (flonum_denormalize, (x, e), SCHEME_OBJECT x AND SCHEME_OBJECT e)
{
  extern double EXFUN (ldexp, (double, int));
  return (double_to_flonum (ldexp ((FLONUM_TO_DOUBLE (x)),
				   ((int) (integer_to_long (e))))));
}

/* Generic Integer Operations */

Boolean
DEFUN (integer_zero_p, (n), SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_ZERO_P (n)) : (BIGNUM_ZERO_P (n)));
}

Boolean
DEFUN (integer_negative_p, (n), SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_NEGATIVE_P (n)) : (BIGNUM_NEGATIVE_P (n)));
}

Boolean
DEFUN (integer_positive_p, (n), SCHEME_OBJECT n)
{
  return ((FIXNUM_P (n)) ? (FIXNUM_POSITIVE_P (n)) : (BIGNUM_POSITIVE_P (n)));
}

Boolean
DEFUN (integer_equal_p, (n, m), SCHEME_OBJECT n AND SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (FIXNUM_EQUAL_P (n, m))
	: (bignum_equal_p ((FIXNUM_TO_BIGNUM (n)), m)))
     : (bignum_equal_p (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));
}

Boolean
DEFUN (integer_less_p, (n, m), SCHEME_OBJECT n AND SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (FIXNUM_LESS_P (n, m))
	: (BIGNUM_LESS_P ((FIXNUM_TO_BIGNUM (n)), m)))
     : (BIGNUM_LESS_P (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m))));
}

SCHEME_OBJECT
DEFUN (integer_negate, (n), SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer (- (FIXNUM_TO_LONG (n))))
     : (bignum_to_integer (bignum_negate (n))));
}

SCHEME_OBJECT
DEFUN (integer_add, (n, m), SCHEME_OBJECT n AND SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (long_to_integer ((FIXNUM_TO_LONG (n)) + (FIXNUM_TO_LONG (m))))
	: (bignum_to_integer (bignum_add ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_add (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

SCHEME_OBJECT
DEFUN (integer_add_1, (n), SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer ((FIXNUM_TO_LONG (n)) + 1))
     : (bignum_to_integer (bignum_add (n, (long_to_bignum (1))))));
}

SCHEME_OBJECT
DEFUN (integer_subtract, (n, m), SCHEME_OBJECT n AND SCHEME_OBJECT m)
{
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? (long_to_integer ((FIXNUM_TO_LONG (n)) - (FIXNUM_TO_LONG (m))))
	: (bignum_to_integer (bignum_subtract ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_subtract (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

SCHEME_OBJECT
DEFUN (integer_subtract_1, (n), SCHEME_OBJECT n)
{
  return
    ((FIXNUM_P (n))
     ? (long_to_integer ((FIXNUM_TO_LONG (n)) - 1))
     : (bignum_to_integer (bignum_subtract (n, (long_to_bignum (1))))));
}

SCHEME_OBJECT
DEFUN (integer_multiply, (n, m), SCHEME_OBJECT n AND SCHEME_OBJECT m)
{
  extern SCHEME_OBJECT EXFUN (Mul, (SCHEME_OBJECT, SCHEME_OBJECT));
  fast SCHEME_OBJECT result;
  return
    ((FIXNUM_P (n))
     ? ((FIXNUM_P (m))
	? ((result = (Mul (n, m))),
	   ((result != SHARP_F)
	    ? result
	    : (bignum_to_integer
	       (bignum_multiply ((FIXNUM_TO_BIGNUM (n)),
				 (FIXNUM_TO_BIGNUM (m)))))))
	: (bignum_to_integer (bignum_multiply ((FIXNUM_TO_BIGNUM (n)), m))))
     : (bignum_to_integer
	(bignum_multiply (n, ((FIXNUM_P (m)) ? (FIXNUM_TO_BIGNUM (m)) : m)))));
}

Boolean
DEFUN (integer_divide, (n, d, q, r),
       SCHEME_OBJECT n AND SCHEME_OBJECT d
       AND SCHEME_OBJECT * q AND SCHEME_OBJECT * r)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  /* Now, unbelievable hair because C doesn't fully specify
	     / and % when their arguments are negative.  We must get
	     consistent answers for all valid arguments. */
	  fast long lx = (FIXNUM_TO_LONG (n));
	  fast long ly = (FIXNUM_TO_LONG (d));
	  fast long quotient;
	  fast long remainder;
	  if (ly == 0)
	    return (true);
	  if (lx < 0)
	    {
	      lx = (-lx);
	      if (ly < 0)
		{
		  ly = (-ly);
		  quotient = (lx / ly);
		}
	      else
		quotient = (- (lx / ly));
	      remainder = (- (lx % ly));
	    }
	  else
	    {
	      if (ly < 0)
		{
		  ly = (-ly);
		  quotient = (- (lx / ly));
		}
	      else
		quotient = (lx / ly);
	      remainder = (lx % ly);
	    }
	  (*q) = (long_to_integer (quotient));
	  (*r) = (LONG_TO_FIXNUM (remainder));
	  return (false);
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT quotient;
    SCHEME_OBJECT remainder;
    if (bignum_divide (n, d, (&quotient), (&remainder)))
      return (true);
    (*q) = (bignum_to_integer (quotient));
    (*r) = (bignum_to_integer (remainder));
    return (false);
  }
}

SCHEME_OBJECT
DEFUN (integer_quotient, (n, d), SCHEME_OBJECT n AND SCHEME_OBJECT d)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  fast long lx = (FIXNUM_TO_LONG (n));
	  fast long ly = (FIXNUM_TO_LONG (d));
	  return
	    ((ly == 0)
	     ? SHARP_F
	     : (long_to_integer
		((lx < 0)
		 ? ((ly < 0)
		    ? ((-lx) / (-ly))
		    : (- ((-lx) / ly)))
		 : ((ly < 0)
		    ? (- (lx / (-ly)))
		    : (lx / ly)))));
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT result = (bignum_quotient (n, d));
    return
      ((result == SHARP_F)
       ? SHARP_F
       : (bignum_to_integer (result)));
  }
}

SCHEME_OBJECT
DEFUN (integer_remainder, (n, d), SCHEME_OBJECT n AND SCHEME_OBJECT d)
{
  if (FIXNUM_P (n))
    {
      if (FIXNUM_P (d))
	{
	  fast long lx = (FIXNUM_TO_LONG (n));
	  fast long ly = (FIXNUM_TO_LONG (d));
	  return
	    ((ly == 0)
	     ? SHARP_F
	     : (long_to_integer
		((lx < 0)
		 ? (- ((-lx) % ((ly < 0) ? (-ly) : ly)))
		 : (lx % ((ly < 0) ? (-ly) : ly)))));
	}
      n = (FIXNUM_TO_BIGNUM (n));
    }
  else
    {
      if (FIXNUM_P (d))
	d = (FIXNUM_TO_BIGNUM (d));
    }
  {
    SCHEME_OBJECT result = (bignum_remainder (n, d));
    return
      ((result == SHARP_F)
       ? SHARP_F
       : (bignum_to_integer (result)));
  }
}
