/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/missing.c,v 9.27 1991/12/07 01:55:51 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

/* This file contains utilities potentially missing from the math library. */

#include "config.h"

#ifndef HAS_FREXP

double
frexp (value, eptr)
     double value;
     int * eptr;
{
  register double x = ((value < 0) ? (-value) : value);
  int e = 0;
  if (x >= 1)
    {
      while (1)
	{
	  if (x > 2)
	    {
	      register double xr = (x / 2);
	      register double r = 2;
	      register int n = 1;
	      while (xr >= r)
		{
		  /* ((xr == (x / r)) && (xr >= r) && (x >= (r * r))) */
		  xr /= r;
		  /* ((xr == (x / (r * r))) && (xr >= 1)) */
		  r *= r;
		  /* ((xr == (x / r)) && (x >= r)) */
		  n += n;
		}
	      /* ((xr >= 1) && (xr < r)) */
	      x = xr;
	      e += n;
	    }
	  else if (x < 2)
	    {
	      x /= 2;
	      e += 1;
	      break;
	    }
	  else
	    {
	      x /= 4;
	      e += 2;
	      break;
	    }
	}
    }
  else if ((x > 0) && (x < 0.5))
    {
      while (1)
	{
	  if (x < 0.25)
	    {
	      register double xr = (x * 2);
	      register double r = 0.5;
	      register int n = 1;
	      /* ((xr == (x / r)) && (xr < 0.5) && (x < (r / 2))) */
	      while (xr < (r / 2))
		{
		  /* ((xr < (r / 2)) && (x < ((r * r) / 2))) */
		  xr /= r;
		  /* ((xr == (x / (r * r))) && (xr < 0.5)) */
		  r *= r;
		  /* ((xr == (x / r)) && (x < (r / 2))) */
		  n += n;
		}
	      /* ((xr >= (r / 2)) && (xr < 0.5)) */
	      x = xr;
	      e -= n;
	    }
	  else
	    {
	      x *= 2;
	      e -= 1;
	      break;
	    }
	}
    }
  (*eptr) = e;
  return ((value < 0) ? (-x) : x);
}

double
ldexp (value, exponent)
     double value;
     int exponent;
{
  register double x = value;
  register int e = exponent;
  register double r = 2;
  if (e > 0)
    {
      if (e == 1)
	return (x * 2);
      while (1)
	{
	  if ((e % 2) != 0)
	    x *= r;
	  e /= 2;
	  if (e == 1)
	    return (x * r * r);
	  r *= r;
	}
    }
  else if (e < 0)
    {
      e = (-e);
      if (e == 1)
	return (x / 2);
      while (1)
	{
	  if ((e % 2) != 0)
	    x /= r;
	  e /= 2;
	  if (e == 1)
	    return ((x / r) / r);
	  r *= r;
	}
    }
  else
    return (x);
}

#endif /* not HAS_FREXP */

#ifndef HAS_MODF

double
modf (value, iptr)
     double value;
     double * iptr;
{
  int exponent;
  double significand = (frexp (value, (&exponent)));
  if ((significand == 0) || (exponent <= 0))
    {
      (*iptr) = 0;
      return (value);
    }
  {
    register double s =
      ((((significand < 0) ? (-significand) : significand) * 2) - 1);
    register int e = (exponent - 1);
    register double n = 1;
    while (1)
      {
	if (e == 0)
	  break;
	s *= 2;
	e -= 1;
	n *= 2;
	if (s >= 1)
	  {
	    s -= 1;
	    n += 1;
	    if (s <= 0)
	      {
		/* Multiply n by 2^e */
		register double b = 2;
		if (e == 0)
		  break;
		while (1)
		  {
		    if ((e % 2) == 1)
		      {
			n *= b;
			if (e == 1)
			  break;
			e -= 1;
		      }
		    b *= b;
		    e /= 2;
		  }
		break;
	      }
	  }
      }
    (*iptr) = n;
    return (s);
  }
}

#endif /* not HAS_MODF */

#ifndef HAS_FLOOR

double
floor (x)
     double x;
{
  double iptr;
  double fraction = (modf (x, (&iptr)));
  return ((fraction < 0) ? (iptr - 1) : iptr);
}

double
ceil (x)
     double x;
{
  double iptr;
  double fraction = (modf (x, (&iptr)));
  return ((fraction > 0) ? (iptr + 1) : iptr);
}

#endif /* not HAS_FLOOR */

#ifdef DEBUG_MISSING

#include <stdio.h>

main ()
{
  double input;
  double output;
  int exponent;
  while (1)
    {
      printf ("Number -> ");
      scanf ("%F", (&input));
      output = (frexp (input, (&exponent)));
      printf ("Input = %G; Output = %G; Exponent = %d\n",
	      input, output, exponent);
      printf ("Result = %G\n", (ldexp (output, exponent)));
    }
}
#endif
