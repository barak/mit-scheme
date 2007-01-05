/* -*-C-*-

$Id: missing.c,v 9.36 2007/01/05 15:33:06 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

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

/* This file contains utilities potentially missing from the math library. */

#include "config.h"

#ifndef HAVE_FREXP

double
DEFUN (frexp, (value, eptr),
       double value
       AND int * eptr)
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
DEFUN (ldexp, (value, exponent),
       double value
       AND int exponent)
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

#endif /* not HAVE_FREXP */

#ifndef HAVE_MODF

double
DEFUN (modf, (value, iptr),
       double value
       AND double * iptr)
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
    if (significand < 0)
    {
      (*iptr) = (- n);
      return (- s);
    }
    else
    {
      (*iptr) = n;
      return (s);
    }
  }
}

#endif /* not HAVE_MODF */

#ifndef HAVE_FLOOR

double
DEFUN (floor, (x), double x)
{
  double iptr;
  double fraction = (modf (x, (&iptr)));
  return ((fraction < 0) ? (iptr - 1) : iptr);
}

double
DEFUN (ceil, (x), double x)
{
  double iptr;
  double fraction = (modf (x, (&iptr)));
  return ((fraction > 0) ? (iptr + 1) : iptr);
}

#endif /* not HAVE_FLOOR */

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

#endif /* DEBUG_MISSING */
