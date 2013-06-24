/* -*-C-*-

$Id: array.c,v 9.50 2007/01/05 21:19:25 cph Exp $

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

#include "scheme.h"
#include "prims.h"
#include "array.h"
#include <math.h>
#include <values.h>
/* <values.h> contains some math constants */

/* ARRAY (as a scheme object)
   is a usual array (in C) containing REAL numbers (float/double)
   and tagged as a non-marked vector.

   Basic contents:
   constructors, selectors, arithmetic operations,
   conversion routines between C_Array, and Scheme_Vector

   see array.h for macros, NM_VECTOR, and extern */

/* mathematical constants */
#ifdef PI
#undef PI
#endif
#define PI           3.141592653589793238462643
#define PI_OVER_2    1.570796326794896619231322
#define TWOPI        6.283185307179586476925287
#define SQRT_2          1.4142135623730950488
#define ONE_OVER_SQRT_2  .7071067811865475244
/* Abramowitz and Stegun p.3 */

REAL
flonum_to_real (argument, arg_number)
     fast SCHEME_OBJECT argument;
     int arg_number;
{
  switch (OBJECT_TYPE (argument))
    {
    case TC_FIXNUM:
      return ((REAL) (FIXNUM_TO_DOUBLE (argument)));

    case TC_BIG_FIXNUM:
      if (! (BIGNUM_TO_DOUBLE_P (argument)))
	  error_bad_range_arg (arg_number);
      return ((REAL) (bignum_to_double (argument)));

    case TC_BIG_FLONUM:
      return ((REAL) (FLONUM_TO_DOUBLE (argument)));

    default:
      error_wrong_type_arg (arg_number);
      /* NOTREACHED */
    }
}

SCHEME_OBJECT
allocate_array (length)
     long length;
{
#if (REAL_IS_DEFINED_DOUBLE == 0)

  fast SCHEME_OBJECT result =
    (allocate_non_marked_vector
     (TC_NON_MARKED_VECTOR, ((length * REAL_SIZE) + 1), true));
  FAST_MEMORY_SET (result, 1, length);
  return (result);

#else /* (REAL_IS_DEFINED_DOUBLE != 0) */
  
  long n_words = (length * DOUBLE_SIZE);
  ALIGN_FLOAT (Free);
  Primitive_GC_If_Needed (n_words + 1);
  {
    SCHEME_OBJECT result = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, (Free)));
    (*Free++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, n_words));
    Free += n_words;
    return (result);
  }

#endif /* (REAL_IS_DEFINED_DOUBLE != 0) */
}

DEFINE_PRIMITIVE ("VECTOR->ARRAY", Prim_vector_to_array, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, VECTOR_P);
  {
    SCHEME_OBJECT vector = (ARG_REF (1));
    long length = (VECTOR_LENGTH (vector));
    SCHEME_OBJECT result = (allocate_array (length));
    fast SCHEME_OBJECT * scan_source = (& (VECTOR_REF (vector, 0)));
    fast SCHEME_OBJECT * end_source = (scan_source + length);
    fast REAL * scan_target = (ARRAY_CONTENTS (result));
    while (scan_source < end_source)
      (*scan_target++) = (flonum_to_real ((*scan_source++), 1));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("ARRAY->VECTOR", Prim_array_to_vector, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  {
    SCHEME_OBJECT array = (ARG_REF (1));
    long length = (ARRAY_LENGTH (array));
    fast REAL * scan_source = (ARRAY_CONTENTS (array));
    fast REAL * end_source = (scan_source + length);
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, length, true));
    fast SCHEME_OBJECT * scan_result = (MEMORY_LOC (result, 1));
    while (scan_source < end_source)
      (*scan_result++) = (double_to_flonum ((double) (*scan_source++)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("ARRAY-ALLOCATE", Prim_array_allocate, 1,1, 0)
{
  fast REAL * scan;
  long length;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  length = (arg_nonnegative_integer (1));
  result = (allocate_array (length));
  for (scan = (ARRAY_CONTENTS (result)); --length >= 0; )
    *scan++ = ((REAL) 0.0);
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("ARRAY-CONS-REALS", Prim_array_cons_reals, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    fast double from = (arg_real_number (1));
    fast double dt = (arg_real_number (2));
    long length = (arg_nonnegative_integer (3));
    SCHEME_OBJECT result = (allocate_array (length));
    fast REAL * scan_result = (ARRAY_CONTENTS (result));
    fast int i;
    for (i = 0; (i < length); i += 1)
      {
	(*scan_result++) = ((REAL) from);
	from += dt;
      }
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("ARRAY-LENGTH", Prim_array_length, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (ARRAY_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("ARRAY-REF", Prim_array_ref, 2, 2, 0)
{
  SCHEME_OBJECT array;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  array = (ARG_REF (1));
  PRIMITIVE_RETURN
    (double_to_flonum
     ((double)
      ((ARRAY_CONTENTS (array))
       [arg_index_integer (2, (ARRAY_LENGTH (array)))])));
}

DEFINE_PRIMITIVE ("ARRAY-SET!", Prim_array_set, 3, 3, 0)
{
  SCHEME_OBJECT array;
  REAL * array_ptr;
  double old_value, new_value;
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  array = (ARG_REF (1));
  array_ptr =
    (& ((ARRAY_CONTENTS (array))
	[arg_index_integer (2, (ARRAY_LENGTH (array)))]));
  old_value = (*array_ptr);
  new_value = (arg_real_number (3));
#if (REAL_IS_DEFINED_DOUBLE == 0)
  if ((new_value >= 0.0)
      ? (new_value < ((double) FLT_MIN))
      : (new_value > (0.0 - ((double) FLT_MIN))))
    new_value = ((REAL) 0.0);
#endif
  (*array_ptr) = ((REAL) new_value);
  PRIMITIVE_RETURN (double_to_flonum (old_value));
}

/*____________________ file readers ___________
  ascii and 2bint formats 
  ______________________________________________*/

/* Reading data from files 
   To read REAL numbers, use "lf" for double, "%f" for float 
   */
#if (REAL_IS_DEFINED_DOUBLE == 1)
#define REALREAD  "%lf"
#define REALREAD2 "%lf %lf"
#else
#define REALREAD  "%f"
#define REALREAD2 "%f %f"
#endif

static void
C_Array_Read_Ascii_File (a, N, fp)          /* 16 ascii decimal digits */
     REAL * a;
     long N;
     FILE * fp;
{ 
  fast long i;
  for (i = 0; (i < N); i += 1)
    {
      if ((fscanf (fp, REALREAD, (&(a[i])))) != 1)
	{ printf("Not enough values read ---\n last value a[%d] = % .16e \n", (i-1), a[i-1]);
	  error_external_return (); }
    }
  return;
}

/* 2BINT FORMAT = integer stored in 2 consecutive bytes.
   On many machines, "putw" and "getw" use 4 byte integers (C int)
   so use "putc" "getc" as shown below.
   */

static void
C_Array_Read_2bint_File (a, N, fp)
     REAL * a;
     long N;
     FILE * fp;
{
  fast long i;
  fast int msd;
  for (i = 0; (i < N); i += 1)
    {
      if (feof (fp))
	error_external_return ();
      msd = (getc (fp));
      (a [i]) = ((REAL) ((msd << 8) | (getc (fp))));
    }
  return;
}

DEFINE_PRIMITIVE ("ARRAY-READ-FROM-FILE", Prim_array_read_from_file, 3,3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, STRING_P);	/* 1 = filename */
  /*                               2 = length of data */
  CHECK_ARG (3, FIXNUM_P);	/* 3 = format of data   0=ascii 1=2bint  */
  {
    fast long length = (arg_nonnegative_integer (2));
    fast SCHEME_OBJECT result = (allocate_array (length));
    int format;
    fast FILE * fp;
    if ( (fp = fopen((STRING_ARG (1)), "r")) == NULL)
      error_bad_range_arg (1);
    
    format = arg_nonnegative_integer(3);
    if (format==0)
      C_Array_Read_Ascii_File ((ARRAY_CONTENTS (result)), length, fp);
    else if (format==1)
      C_Array_Read_2bint_File ((ARRAY_CONTENTS (result)), length, fp);
    else
      error_bad_range_arg(3);	/* illegal format code */
    
    if ((fclose (fp)) != 0)
      error_external_return ();
    PRIMITIVE_RETURN (result);
  }
}

static void
C_Array_Write_Ascii_File (a, N, fp)           /* 16 ascii decimal digits */
     REAL * a;
     long N;
     FILE * fp;
{
  fast long i;
  for (i = 0; (i < N); i += 1)
    {
      if (feof (fp))
	error_external_return ();
      fprintf (fp, "% .16e \n", a[i]);
    }
  return;
}

DEFINE_PRIMITIVE ("ARRAY-WRITE-ASCII-FILE", Prim_array_write_ascii_file, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, STRING_P);
  {
    fast SCHEME_OBJECT array = (ARG_REF (1));
    fast FILE * fp = (fopen((STRING_ARG (2)), "w"));
    if (fp == ((FILE *) 0))
      error_bad_range_arg (2);
    C_Array_Write_Ascii_File
      ((ARRAY_CONTENTS (array)),
       (ARRAY_LENGTH (array)),
       fp);
    if ((fclose (fp)) != 0)
      error_external_return ();
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}




DEFINE_PRIMITIVE ("SUBARRAY-COPY!", Prim_subarray_copy, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, ARRAY_P);	/* source array */
  CHECK_ARG (2, ARRAY_P);	/* destination array */
  {
    REAL * source = (ARRAY_CONTENTS (ARG_REF (1)));
    REAL * target = (ARRAY_CONTENTS (ARG_REF (2)));
    long start_source = (arg_nonnegative_integer (3));
    long start_target = (arg_nonnegative_integer (4));
    long n_elements = (arg_nonnegative_integer (5));
    if ((start_source + n_elements) > (ARRAY_LENGTH (ARG_REF (1))))
      error_bad_range_arg (3);
    if ((start_target + n_elements) > (ARRAY_LENGTH (ARG_REF (2))))
      error_bad_range_arg (4);
    {
      fast REAL * scan_source = (source + start_source);
      fast REAL * end_source = (scan_source + n_elements);
      fast REAL * scan_target = (target + start_target);
      while (scan_source < end_source)
	(*scan_target++) = (*scan_source++);
    }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ARRAY-REVERSE!", Prim_array_reverse, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  {
    SCHEME_OBJECT array = (ARG_REF (1));
    long length = (ARRAY_LENGTH (array));
    long half_length = (length / 2);
    fast REAL * array_ptr = (ARRAY_CONTENTS (array));
    fast long i;
    fast long j;
    for (i = 0, j = (length - 1); (i < half_length); i += 1, j -= 1)
      {
	fast REAL Temp = (array_ptr [j]);
	(array_ptr [j]) = (array_ptr [i]);
	(array_ptr [i]) = Temp;
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ARRAY-TIME-REVERSE!", Prim_array_time_reverse, 1, 1, 0)
{
  void C_Array_Time_Reverse ();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  C_Array_Time_Reverse
    ((ARRAY_CONTENTS (ARG_REF (1))), (ARRAY_LENGTH (ARG_REF (1))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* time-reverse
   x[0] remains fixed. (time=0)
   x[i] swapped with x[n-i]. (mirror image around x[0]) */

void
C_Array_Time_Reverse (x,n)
     REAL *x;
     long n;
{ long i, ni, n2;
  REAL xt;
  if ((n % 2) == 0)		/* even length */
  { n2 = (n/2);
    for (i=1; i<n2; i++)	/* i=1,2,..,n/2-1 */
    {  ni = n-i;
       xt    = x[i];
       x[i]  = x[ni];
       x[ni] = xt; }}
  else				/* odd length */
  { n2 = (n+1)/2;		/* (n+1)/2 = (n-1)/2 + 1 */
    for (i=1; i<n2; i++)	/* i=1,2,..,(n-1)/2 */
    {  ni = n-i;
       xt   = x[i];
       x[i] = x[ni];
       x[ni] = xt; }}
}

/* The following is smart
   and avoids computation when offset or scale are degenerate 0,1 */

DEFINE_PRIMITIVE ("SUBARRAY-OFFSET-SCALE!", Prim_subarray_offset_scale, 5, 5, 0)
{
  long i, at, m,mplus;
  REAL *a, offset,scale;
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, FIXNUM_P);
  CHECK_ARG (3, FIXNUM_P);
  a = ARRAY_CONTENTS(ARG_REF(1));
  at = arg_nonnegative_integer(2); /*       at = starting index             */
  m  = arg_nonnegative_integer(3); /*       m  = number of points to change */
  mplus = at + m;
  if (mplus > (ARRAY_LENGTH(ARG_REF(1)))) error_bad_range_arg(3);
  offset = (arg_real (4));
  scale = (arg_real (5));
  if ((offset == 0.0) && (scale == 1.0))
    ;				/* be smart */
  else if (scale == 0.0)
    for (i=at; i<mplus; i++)  a[i] = offset;
  else if (offset == 0.0)
    for (i=at; i<mplus; i++)  a[i] = scale * a[i];
  else if (scale == 1.0)
    for (i=at; i<mplus; i++)  a[i] = offset + a[i];
  else
    for (i=at; i<mplus; i++)  a[i] = offset + scale * a[i];
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("COMPLEX-SUBARRAY-COMPLEX-SCALE!", Prim_complex_subarray_complex_scale, 6,6, 0)
{ long i, at,m,mplus;
  REAL *a,*b;			/* (a,b) = (real,imag) arrays */
  double temp, minus_y,  x, y;	/* (x,y) = (real,imag) scale */
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  at = (arg_nonnegative_integer (3)); /* starting index */
  m  = (arg_nonnegative_integer (4)); /* number of points to change */
  mplus = at + m;
  if (mplus > (ARRAY_LENGTH(ARG_REF(1)))) error_bad_range_arg(4);
  x = (arg_real_number (5));
  y = (arg_real_number (6));
  a = ARRAY_CONTENTS(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  if ((ARRAY_LENGTH(ARG_REF(1))) != (ARRAY_LENGTH(ARG_REF(2))))
    error_bad_range_arg(2);
  if (x==0.0)			/* imaginary only */
    if       (y==0.0)
      for (i=at; i<mplus; i++)
      { a[i] = 0.0;
	b[i] = 0.0; }
    else if  (y==1.0)
      for (i=at; i<mplus; i++)
      { temp = b[i];
	b[i] = a[i];
	a[i] = (-temp); }
    else if  (y==-1.0)
      for (i=at; i<mplus; i++)
      { temp = b[i];
	b[i] = (-a[i]);
	a[i] = temp; }
    else
    { minus_y = (-y);
      for (i=at; i<mplus; i++)
      { temp =               y * ((double) a[i]);
	a[i] = (REAL) (minus_y * ((double) b[i]));
	b[i] = (REAL) temp; }}
  else if (y==0.0)		/* real only */
    if (x==1.0) ;
    else for (i=at; i<mplus; i++)
    { a[i] = (REAL) (x * ((double) a[i]));
      b[i] = (REAL) (x * ((double) b[i])); }
  else				/* full complex scale */
    for (i=at; i<mplus; i++)
    { temp =         ((double) a[i])*x - ((double) b[i])*y;
      b[i] = (REAL) (((double) b[i])*x + ((double) a[i])*y);
      a[i] = (REAL) temp; }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Accumulate
   using combinators              *
   corresponding type codes       1 */

DEFINE_PRIMITIVE ("COMPLEX-SUBARRAY-ACCUMULATE!", Prim_complex_subarray_accumulate, 6,6, 0)
{ long  at,m,mplus, tc, i;
  REAL *a,*b;			/* (a,b) = (real,imag) input arrays */
  REAL *c;	/* result = output array of length 2, holds a complex number */
  double x, y, temp;
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, ARRAY_P);	/* a = input array (real) */
  CHECK_ARG (2, ARRAY_P);	/* b = input array (imag) */
  a = ARRAY_CONTENTS(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  if ((ARRAY_LENGTH(ARG_REF(1))) != (ARRAY_LENGTH(ARG_REF(2))))
    error_bad_range_arg(2);
  tc = arg_nonnegative_integer(3); /*       tc = type code 0 or 1            */
  at = arg_nonnegative_integer(4); /*       at = starting index              */
  m  = arg_nonnegative_integer(5); /*       m  = number of points to process */
  CHECK_ARG (6, ARRAY_P);	/* c = output array of length 2 */
  c = ARRAY_CONTENTS(ARG_REF(6));
  if ((ARRAY_LENGTH(ARG_REF(6))) != 2) error_bad_range_arg(6);
  mplus = at + m;
  if (mplus > (ARRAY_LENGTH(ARG_REF(1)))) error_bad_range_arg(5);
  if (tc==1)
  { x = 1.0;			/* real part of accumulator */
    y = 0.0;			/* imag part of accumulator */
    for (i=at;i<mplus;i++)
    { temp = ((double) a[i])*x - ((double) b[i])*y;
      y    = ((double) b[i])*x + ((double) a[i])*y;
      x    = temp; }
  }
  else
    error_bad_range_arg(3);
  c[0] = ((REAL) x);		/* mechanism for returning complex number */
  c[1] = ((REAL) y);		/* do not use lists, avoid heap pointer   */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CS-ARRAY-TO-COMPLEX-ARRAY!", Prim_cs_array_to_complex_array, 3, 3, 0)
{ long n,n2,n2_1, i;
  REAL *a, *b,*c;
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  CHECK_ARG (3, ARRAY_P);
  a = ARRAY_CONTENTS(ARG_REF(1));
  n = ARRAY_LENGTH(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  c = ARRAY_CONTENTS(ARG_REF(3));
  if (n!=(ARRAY_LENGTH(ARG_REF(2)))) error_bad_range_arg(2);
  if (n!=(ARRAY_LENGTH(ARG_REF(3)))) error_bad_range_arg(3);
  b[0] = a[0];
  c[0] = 0.0;
  n2 = n/2;			/* integer division truncates down */
  n2_1 = n2+1;
  if (2*n2 == n)		/* even length, n2 is only real */
  { b[n2]  = a[n2];  c[n2]  = 0.0; }
  else /* odd length, make the loop include the n2 index */
  { n2   = n2+1;
    n2_1 = n2; }
  for (i=1; i<n2; i++)   { b[i] = a[i];
			   c[i] = a[n-i]; }
  for (i=n2_1; i<n; i++) { b[i] =  a[n-i];
			   c[i] = (-a[i]); }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CS-ARRAY-MULTIPLY-INTO-SECOND-ONE!", Prim_cs_array_multiply_into_second_one, 2, 2, 0)
{ long n,n2;
  REAL *a, *b;
  void cs_array_multiply_into_second_one();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  a = ARRAY_CONTENTS(ARG_REF(1));
  n = ARRAY_LENGTH(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  if (n!=(ARRAY_LENGTH(ARG_REF(2)))) error_bad_range_arg(2);
  n2 = n/2;			/* integer division truncates down */
  cs_array_multiply_into_second_one(a,b, n,n2);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
cs_array_multiply_into_second_one (a,b, n,n2)
     REAL *a, *b;
     long n,n2;
{
  REAL temp;
  long i,ni;
  b[0]   = a[0]  * b[0];
  if (2*n2 == n)		/* even length, n2 is only real */
    b[n2]  = a[n2] * b[n2];
  else
    n2 = n2+1; /* odd length, make the loop include the n2 index */
  for (i=1; i<n2; i++)
    {
      ni = n-i;
      temp   = a[i]*b[i]   -  a[ni]*b[ni]; /* real part */
      b[ni]  = a[i]*b[ni]  +  a[ni]*b[i]; /*  imag part */
      b[i]   = temp;
    }
}

DEFINE_PRIMITIVE ("CS-ARRAY-DIVIDE-INTO-XXX!", Prim_cs_array_divide_into_xxx, 4, 4, 0)
{
  long n,n2, one_or_two;
  REAL *a, *b, inf;
  void cs_array_divide_into_z();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  inf = (arg_real (3));
  /* where to store result of division */
  one_or_two = (arg_nonnegative_integer (4));
  a = ARRAY_CONTENTS(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  n = ARRAY_LENGTH(ARG_REF(1));
  if (n!=(ARRAY_LENGTH(ARG_REF(2)))) error_bad_range_arg(2);
  n2 = n/2;			/* integer division truncates down */
  if (one_or_two == 1)
    cs_array_divide_into_z(a,b, a,  n,n2, inf);
  else if (one_or_two == 2)
    cs_array_divide_into_z(a,b, b,  n,n2, inf);
  else
    error_bad_range_arg(4);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
cs_array_divide_into_second_one (a,b, n,n2,inf)	/* used in image.c */
     REAL *a,*b, inf;
     long n,n2;
{
  void cs_array_divide_into_z ();
  cs_array_divide_into_z (a,b, b, n,n2,inf);
}

void
cs_array_divide_into_z (a,b, z, n,n2, inf) /* z can be either a or b */
     REAL *a,*b,*z, inf;
     long n,n2;
{ long i,ni;
  REAL temp, radius;

  if (b[0] == 0.0)
    if (a[0] == 0.0) z[0] = 1.0;
    else             z[0] = a[0] * inf;
  else               z[0] = a[0] / b[0];

  if (2*n2 == n)		/* even length, n2 is only real */
    if (b[n2] == 0.0)
      if (a[n2] == 0.0) z[n2] = 1.0;
      else              z[n2] = a[n2] * inf;
    else                z[n2] = a[n2] / b[n2];
  else
    n2 = n2+1; /* odd length, make the loop include the n2 index */

  for (i=1; i<n2; i++)
  { ni = n-i;
    radius = b[i]*b[i] + b[ni]*b[ni]; /* b^2 denominator = real^2 + imag^2 */

    if (radius == 0.0) {
      if (a[i]  == 0.0) z[i]  = 1.0;
      else              z[i]  = a[i] * inf;
      if (a[ni] == 0.0) z[ni] = 1.0;
      else              z[ni] = a[ni] * inf; }
    else {
      temp  = a[i]*b[i]    +  a[ni]*b[ni];
      z[ni] = (a[ni]*b[i]  -  a[i]*b[ni]) / radius; /* imag part */
      z[i]  = temp                        / radius; /* real part */
    }}
}

/* ARRAY-UNARY-FUNCTION!
   apply unary-function elementwise on array
   Available functions : */

void
REALabs (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) fabs( (double) (*a)) );
}

void
REALexp (a,b)
     REAL *a,*b;
{
  fast double y;
  if ((y = exp((double) (*a))) == HUGE)
    error_bad_range_arg (1);	/* OVERFLOW */
  (*b) = ((REAL) y);
}

void
REALlog (a,b)
     REAL *a,*b;
{
  if ((*a) < 0.0)
    error_bad_range_arg(1);	/* log(negative) */
  (*b) = ( (REAL) log( (double) (*a)) );
}

void
REALtruncate (a,b)
     REAL *a,*b;		/* towards zero */
{
  double integral_part, modf();
  modf( ((double) (*a)), &integral_part);
  (*b) = ( (REAL) integral_part);
}

void
REALround (a,b)
     REAL *a,*b;		/* towards nearest integer */
{
  double integral_part, modf();
  if ((*a) >= 0.0)		/* It may be faster to look at the sign
				   of mantissa, and dispatch */
    modf( ((double) ((*a)+0.5)), &integral_part);
  else
    modf( ((double) ((*a)-0.5)), &integral_part);
  (*b) = ( (REAL) integral_part);
}

void
REALsquare (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) ((*a) * (*a)) );
}

void
REALsqrt (a,b)
     REAL *a,*b;
{
  if ((*a) < 0.0)
    error_bad_range_arg(1);	/* sqrt(negative) */
  (*b) = ( (REAL) sqrt( (double) (*a)) );
}

void
REALsin (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) sin( (double) (*a)) );
}

void
REALcos (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) cos( (double) (*a)) );
}

void
REALtan (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) tan( (double) (*a)) );
}

void
REALasin (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) asin( (double) (*a)) );
}

void
REALacos (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) acos( (double) (*a)) );
}

void
REALatan (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) atan( (double) (*a)) );
}

void
REALgamma (a,b)
     REAL *a,*b;
{
  fast double y;
  if ((y = gamma(((double) (*a)))) > LN_MAXDOUBLE)
    error_bad_range_arg(1);	/* gamma( non-positive integer ) */
  (*b) = ((REAL) (signgam * exp(y))); /* see HPUX Section 3 */
}

void
REALerf (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) erf((double) (*a)) );
}

void
REALerfc (a,b)
     REAL *a,*b;
{
  (*b) = ( (REAL) erfc((double) (*a)) );
}

void
REALbessel1 (order,a,b)		/* Bessel of first kind */
     long order;
     REAL *a,*b;
{
  if (order == 0)
    (*b) = ( (REAL) j0((double) (*a)) );
  if (order == 1)
    (*b) = ( (REAL) j1((double) (*a)) );
  else
    (*b) = ( (REAL) jn(((int) order), ((double) (*a))) );
}

void
REALbessel2 (order,a,b)		/* Bessel of second kind */
     long order;
     REAL *a,*b;
{
  if ((*a) <= 0.0)
    error_bad_range_arg(1);	/* Blows Up */
  if (order == 0)
    (*b) = ( (REAL) y0((double) (*a)) );
  if (order == 1)
    (*b) = ( (REAL) y1((double) (*a)) );
  else
    (*b) = ( (REAL) yn(((int) order), ((double) (*a))) );
}

/* Table to store the available unary-functions.
   Also some binary functions at the end -- not available right now.
   The (1 and 2)s denote the numofargs (1 for unary 2 for binary) */

struct array_func_table
{
  long numofargs;
  void (*func)();
}
Array_Function_Table [] =
{
  1, REALabs,			/*0*/
  1, REALexp,			/*1*/
  1, REALlog,			/*2*/
  1, REALtruncate,		/*3*/
  1, REALround,			/*4*/
  1, REALsquare,		/*5*/
  1, REALsqrt,			/*6*/
  1, REALsin,			/*7*/
  1, REALcos,			/*8*/
  1, REALtan,			/*9*/
  1, REALasin,			/*10*/
  1, REALacos,			/*11*/
  1, REALatan,			/*12*/
  1, REALgamma,			/*13*/
  1, REALerf,			/*14*/
  1, REALerfc,			/*15*/
  2, REALbessel1,		/*16*/
  2, REALbessel2		/*17*/
  };

#define MAX_ARRAY_FUNCTC 17

/* array-unary-function!    could be called        array-operation-1!
   following the naming convention for other similar procedures
   but it is specialized to mappings only, so we have special name. */

DEFINE_PRIMITIVE ("ARRAY-UNARY-FUNCTION!", Prim_array_unary_function, 2,2, 0)
{
  long n, i;
  REAL *a,*b;
  long tc;
  void (*f)();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);	/*      a  = input (and output) array    */
  CHECK_ARG (2, FIXNUM_P);	/*      tc = type code                   */
  tc = arg_nonnegative_integer(2);
  if (tc > MAX_ARRAY_FUNCTC) error_bad_range_arg(2);
  f = ((Array_Function_Table[tc]).func);
  if (1 != (Array_Function_Table[tc]).numofargs) error_wrong_type_arg(2);
  /* check it is a unary function */
  a = ARRAY_CONTENTS(ARG_REF(1));
  b = a;
  n = ARRAY_LENGTH(ARG_REF(1));
  for (i=0; i<n; i++)
    (*f) ( &(a[i]), &(b[i]) );	/* a into b */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Accumulate
   using combinators              +  or  *
   corresponding type codes       0      1 */

DEFINE_PRIMITIVE ("SUBARRAY-ACCUMULATE", Prim_subarray_accumulate, 4,4, 0)
{
  long at,m,mplus, tc, i;
  REAL *a;
  double result;
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);	/*           a = input array                 */
  a  = ARRAY_CONTENTS(ARG_REF(1));
  tc = arg_nonnegative_integer(2); /*       tc = type code 0 or 1            */
  at = arg_nonnegative_integer(3); /*       at = starting index              */
  m  = arg_nonnegative_integer(4); /*       m  = number of points to process */
  mplus = at + m;
  if (mplus > (ARRAY_LENGTH(ARG_REF(1)))) error_bad_range_arg(4);
  if (tc == 0)
    {
      result = 0.0;
      for (i=at;i<mplus;i++)
	result = result + ((double) a[i]);
    }
  else if (tc == 1)
    {
      result = 1.0;
      for (i=at;i<mplus;i++)
	result = result * ((double) a[i]);
    }
  else
    error_bad_range_arg (2);
  PRIMITIVE_RETURN (double_to_flonum (result));
}

/* The following searches for value within tolerance
   starting from index=from in array.
   Returns first index where match occurs (useful for finding zeros). */

DEFINE_PRIMITIVE ("ARRAY-SEARCH-VALUE-TOLERANCE-FROM", Prim_array_search_value_tolerance_from, 4, 4, 0)
{
  SCHEME_OBJECT array;
  fast long length;
  fast REAL * a;
  fast REAL value;		/* value to search for */
  fast double tolerance;	/* tolerance allowed */
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);
  array = (ARG_REF (1));
  length = (ARRAY_LENGTH (array));
  a = (ARRAY_CONTENTS (array));
  value = (arg_real (2));
  tolerance = (arg_real (3));
  {
    fast long i;
    for (i = (arg_index_integer (4, length)); i<length; i+=1)
      if (tolerance >= (fabs ((double) ((a [i]) - value))))
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (i));
  }
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("SUBARRAY-MIN-MAX-INDEX", Prim_subarray_min_max_index, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  {
    REAL * a = (ARRAY_CONTENTS (ARG_REF (1)));
    long at = (arg_nonnegative_integer (2)); /* starting index */
    long m  = (arg_nonnegative_integer (3)); /* number of points to process */
    long mplus = (at + m);
    long nmin;
    long nmax;
    if (mplus > (ARRAY_LENGTH (ARG_REF (1))))
      error_bad_range_arg (3);
    
    C_Array_Find_Min_Max ((& (a [at])), m, (&nmin), (&nmax));
    nmin = nmin + at;		/* offset appropriately */
    nmax = nmax + at;
    
    PRIMITIVE_RETURN
      (cons ((LONG_TO_FIXNUM (nmin)),
	     (cons ((LONG_TO_FIXNUM (nmax)),
		    EMPTY_LIST))));
  }
}

void
C_Array_Find_Min_Max (x, n, nmin, nmax)
     fast REAL * x;
     fast long n;
     long * nmin;
     long * nmax;
{ REAL *xold = x;
  register REAL xmin, xmax;
  register long nnmin, nnmax;
  register long count;

  nnmin = nnmax = 0;
  xmin = xmax = *x++;
  n--;
  count = 1;
  if(n>0)
  {
    do {
      if(*x < xmin) {
	nnmin = count++ ;
	xmin = *x++ ;
      } else if(*x > xmax) {
	nnmax = count++ ;
	xmax = *x++ ;
      } else {
	count++ ;
	x++ ;
      }
    } while( --n > 0 ) ;
  }
  *nmin = nnmin ;
  *nmax = nnmax ;
}


/* array-average
   can be done with (array-reduce +) and division by array-length.
   But there is also this C primitive.
   Keep it around, may be useful someday. */

/* Computes the average in pieces, so as to reduce
   roundoff smearing in cumulative sum.
   example= first huge positive numbers, then small nums,
   then huge negative numbers. */

static void
C_Array_Find_Average (Array, Length, pAverage)
     long Length;
     REAL * Array;
     REAL * pAverage;
{
  long i;
  long array_index;
  REAL average_n, sum;

  average_n = 0.0;
  array_index = 0;
  while (array_index < Length)
    {
      sum = 0.0;
      for (i=0;((array_index<Length) && (i<2000));i++) {
	sum += Array[array_index];
	array_index++;
      }
      average_n += (sum / ((REAL) Length));
    }
  (*pAverage) = average_n;
  return;
}

DEFINE_PRIMITIVE ("ARRAY-AVERAGE", Prim_array_find_average, 1, 1, 0)
{
  SCHEME_OBJECT array;
  REAL average;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  array = (ARG_REF (1));
  C_Array_Find_Average
    ((ARRAY_CONTENTS (array)),
     (ARRAY_LENGTH (array)),
     (&average));
  PRIMITIVE_RETURN (double_to_flonum ((double) average));
}

void
C_Array_Make_Histogram (Array, Length, Histogram, npoints)
     REAL Array[], Histogram[];
     long Length, npoints;
{
  REAL Max, Min, Offset, Scale;
  long i, nmin, nmax, index;
  C_Array_Find_Min_Max (Array, Length, (&nmin), (&nmax));
  Min = (Array [nmin]);
  Max = (Array [nmax]);
  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, ((REAL) npoints), (&Offset), (&Scale));
  for (i = 0; (i < npoints); i += 1)
    (Histogram [i]) = 0.0;
  for (i = 0; (i < Length); i += 1)
    {
      /* Everything from 0 to 1 maps to bin 0, and so on */
      index = ((long) (floor ((double) ((Scale * (Array [i])) + Offset))));
      /* max that won't floor to legal array index */
      if (index == npoints)
	index = (index - 1);
      (Histogram [index]) += 1.0;
    }
  return;
}

DEFINE_PRIMITIVE ("ARRAY-MAKE-HISTOGRAM", Prim_array_make_histogram, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  {
    fast SCHEME_OBJECT array = (ARG_REF (1));
    long length = (ARRAY_LENGTH (array));
    long npoints = (arg_integer_in_range (2, 1, ((2 * length) + 1)));
    SCHEME_OBJECT result = (allocate_array (npoints));
    C_Array_Make_Histogram
      ((ARRAY_CONTENTS (array)),
       length,
       (ARRAY_CONTENTS (result)),
       npoints);
    PRIMITIVE_RETURN (result);
  }
}

/* The following geometrical map is slightly tricky. */
void
Find_Offset_Scale_For_Linear_Map (Min, Max, New_Min, New_Max, Offset, Scale)
     REAL Min, Max, New_Min, New_Max, *Offset, *Scale;
{
  if (Min != Max)
    {
      (*Scale)  = ((New_Max - New_Min) / (Max - Min));
      (*Offset) = (New_Min - ((*Scale) * Min));
    }
  else
    {
      (*Scale) =
	((Max == 0.0)
	 ? 0.0
	 : (0.25 * (mabs ((New_Max - New_Min) / Max))));
      (*Offset) = ((New_Max + New_Min) / 2.0);
    }
  return;
}


DEFINE_PRIMITIVE ("ARRAY-CLIP-MIN-MAX!", Prim_array_clip_min_max, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  {
    SCHEME_OBJECT array = (ARG_REF (1));
    REAL xmin = (arg_real (2));
    REAL xmax = (arg_real (3));
    long Length = (ARRAY_LENGTH (array));
    REAL * From_Here = (ARRAY_CONTENTS (array));
    REAL * To_Here = From_Here;
    long i;
    if (xmin > xmax)
      error_bad_range_arg (3);
    for (i = 0; (i < Length); i += 1)
      {
	if ((*From_Here) < xmin)
	  (*To_Here++) = xmin;
	else if ((*From_Here) > xmax)
	  (*To_Here++) = xmax;
	else
	  (*To_Here++) = (*From_Here);
	From_Here += 1;
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* complex-array-operation-1!
   groups together procedures that use 1 complex-array
   and store the result in place */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1!", Prim_complex_array_operation_1, 3,3, 0)
{ long n, i, opcode;
  REAL *a, *b;
  void complex_array_to_polar(), complex_array_exp(), complex_array_sqrt();
  void complex_array_sin(), complex_array_cos();
  void complex_array_asin(), complex_array_acos(), complex_array_atan();
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  n = ARRAY_LENGTH(ARG_REF(2));
  if (n != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  a  = ARRAY_CONTENTS(ARG_REF(2)); /*  real part */
  b  = ARRAY_CONTENTS(ARG_REF(3)); /*  imag part */
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    complex_array_to_polar(a,b,n);
  else if (opcode==2)
    complex_array_exp(a,b,n);
  else if (opcode==3)
    complex_array_sqrt(a,b,n);

  else if (opcode==4)
    complex_array_sin(a,b,n);
  else if (opcode==5)
    complex_array_cos(a,b,n);
  /* for tan(z) use sin(z)/cos(z) */
  
  else if (opcode==6)
    complex_array_asin(a,b,n);
  else if (opcode==7)
    complex_array_acos(a,b,n);
  else if (opcode==8)
    complex_array_atan(a,b,n);
  
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
complex_array_to_polar (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;
  double x,y, temp;
  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      y = (double) b[i];
      temp = sqrt(x*x + y*y);
      if (temp == 0.0)
	b[i] = 0.0;		/* choose angle = 0    for x,y=0,0 */
      else
	b[i] = (REAL) atan2(y,x);
      a[i]   = (REAL) temp;
    }
}

void
complex_array_exp (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;
  double x,y, temp;

  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      y = (double) b[i];
      if ((temp = exp(x)) == HUGE) error_bad_range_arg(2); /* overflow */
      a[i] = (REAL) (temp*cos(y));
      b[i] = (REAL) (temp*sin(y));
    }
}

void
complex_array_sqrt (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;
  double x,y, r;

  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      y = (double) b[i];
      r = sqrt( x*x + y*y);
      a[i] = sqrt((r+x)/2.0);
      if (y>=0.0)
	b[i] =  sqrt((r-x)/2.0); /* choose principal root */
      else			/* see Abramowitz (p.17 3.7.27) */
	b[i] = -sqrt((r-x)/2.0);
    }
}

void
complex_array_sin (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;
  double x, ey,fy;
  REAL temp;

  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      ey = exp((double) b[i]);	/* radius should be small to avoid overflow */
      fy = 1.0/ey;		/* exp(-y) */
      /* expanded (e(iz)-e(-iz))*(-.5i) formula */
      temp = (REAL) (sin(x) * (ey + fy) * 0.5);
      /* see my notes in Abram.p.71 */
      b[i] = (REAL) (cos(x) * (ey - fy) * 0.5);
      a[i] = temp;
    }
}

void
complex_array_cos (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;
  double x, ey,fy;
  REAL temp;

  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      ey = exp((double) b[i]);	/* radius should be small to avoid overflow */
      fy = 1.0/ey;		/* exp(-y) */
      /* expanded (e(iz)+e(-iz))*.5 formula */
      temp = (REAL) (cos(x) * (ey + fy) * 0.5);
      b[i] = (REAL) (sin(x) * (fy - ey) * 0.5); /* see my notes in Abram.p.71*/
      a[i] = temp;
    }
}


void
complex_array_asin (a,b,n)
     REAL *a,*b;
     long n;
{ /* logarithmic formula as in R3.99, about 21ops plus log,atan - see my notes */
  long i; 
  double oldx,oldy, x,y, real,imag, r;
  
  for (i=0; i<n; i++)
  {
    oldx = (double) a[i];
    oldy = (double) b[i];
    
    x = 1.0 - oldx*oldx + oldy*oldy; /* 1 - z*z */
    y = -2.0 * oldx * oldy;
    
    r = sqrt(x*x + y*y);	/* sqrt(1-z*z)  */
    real = sqrt((r+x)/2.0);
    if (y>=0.0)
      imag =  sqrt((r-x)/2.0);	/* choose principal root */
    else			/* see Abramowitz (p.17 3.7.27) */
      imag = -sqrt((r-x)/2.0);
    
    real = real - oldy;		/* i*z + sqrt(...) */
    imag = imag + oldx;
    
    b[i] = (REAL) (- log (sqrt (real*real + imag*imag))); /* -i*log(...) */
    a[i] = (REAL) atan2( imag, real); /* chosen angle is okay 
					 Also 0/0 doesnot occur */
  }
}

void
complex_array_acos (a,b,n)
     REAL *a,*b;
     long n;
{
  long i;

  complex_array_asin (a,b,n);
  
  for (i=0; i<n; i++)
    {
      a[i] = PI_OVER_2 - a[i];
      b[i] =           - b[i];
    }
}
  

void
complex_array_atan (a,b,n)
     REAL *a,*b;
     long n;
{ /* logarithmic formula, expanded, simplified - see my notes */
  long i; 
  double x,y, xx, real,imag, d;
  
  for (i=0; i<n; i++)
  {
    x = (double) a[i];
    y = (double) b[i];
    
    xx = x*x;
    imag = 1.0 + y;		/* temp var */
    d  = xx + imag*imag;
    
    real = (1 - y*y - xx) / d;
    imag = (2.0 * x)  / d;
    
    b[i] = (REAL) ((log (sqrt (real*real + imag*imag))) / -2.0);
    a[i] = (atan2 (imag,real)) / 2.0;
  }
}


/* complex-array-operation-1b!
   groups together procedures that use 1 complex-array & 1 number
   and store the result in place (e.g. invert 1/x) */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1B!", Prim_complex_array_operation_1b, 4,4, 0)
{
  long n, i, opcode;
  REAL *a, *b, inf;
  void complex_array_invert ();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  inf = (arg_real (4));		/* User-Provided Infinity */
  n = ARRAY_LENGTH(ARG_REF(2));
  if (n != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  a  = ARRAY_CONTENTS(ARG_REF(2)); /*  real part */
  b  = ARRAY_CONTENTS(ARG_REF(3)); /*  imag part */
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    complex_array_invert(a,b, n, inf);  /* performs 1/x */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
complex_array_invert (a,b, n, inf)
     REAL *a,*b, inf;
     long n;
{
  long i;
  double x,y, r;
  for (i=0; i<n; i++)
    {
      x = (double) a[i];
      y = (double) b[i];
      r = (x*x + y*y);
      if (r==0.0)
	{
	  a[i] = inf;
	  b[i] = inf;
	}
      else
	{
	  a[i] = (REAL)  x/r;
	  b[i] = (REAL) -y/r;
	}
    }
}

/* complex-array-operation-1a
   groups together procedures that use 1 complex-array
   and store result in a 3rd real array. */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1A", Prim_complex_array_operation_1a, 4,4, 0)
{
  long n, i, opcode;
  REAL *a, *b, *c;
  void complex_array_magnitude(), complex_array_angle();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  CHECK_ARG (4, ARRAY_P);	/* output array -- n                       */
  n = ARRAY_LENGTH(ARG_REF(2));
  if (n != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  if (n != ARRAY_LENGTH(ARG_REF(4))) error_bad_range_arg(4);
  a  = ARRAY_CONTENTS(ARG_REF(2)); /*  real part */
  b  = ARRAY_CONTENTS(ARG_REF(3)); /*  imag part */
  c  = ARRAY_CONTENTS(ARG_REF(4)); /*  output    */
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    complex_array_magnitude(a,b,c,n);
  else if (opcode==2)
    complex_array_angle(a,b,c,n);
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
complex_array_magnitude (a,b,c,n)
     REAL *a,*b,*c;
     long n;
{
  long i;
  for (i=0; i<n; i++)
    c[i] = (REAL) sqrt( (double) a[i]*a[i] + b[i]*b[i] );
}

void
complex_array_angle (a,b,c,n)
     REAL *a,*b,*c;
     long n;
{
  long i;
  for (i=0; i<n; i++)
  {
    if ((a[i] == 0.0) && (b[i]==0.0))
      c[i] = 0.0;		/* choose angle=0 for point (0,0) */
    else
      c[i] = (REAL) atan2( (double) b[i], (double) a[i]);
    /* angle ==   -pi (exclusive) to +pi (inclusive) */
  }
}

DEFINE_PRIMITIVE ("CS-ARRAY-MAGNITUDE!", Prim_cs_array_magnitude, 1, 1, 0)
{
  long n, i;
  REAL *a;
  void cs_array_magnitude();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  a = ARRAY_CONTENTS(ARG_REF(1)); /* input cs-array */
  n = ARRAY_LENGTH(ARG_REF(1));	/* becomes a standard array on return  */
  cs_array_magnitude(a,n);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* result is a standard array (even signal, real data) */
void
cs_array_magnitude (a,n)
     REAL *a;
     long n;
{
  long i, n2, ni;
  n2 = n/2;			/* integer division truncates down */
  a[0]  = (REAL) fabs((double) a[0]); /*   imag=0 */
  if (2*n2 == n)		/* even length, n2 is only real */
    a[n2] = (REAL) fabs((double) a[n2]); /*  imag=0 */
  else
    /* odd length, make the loop include the n2 index */
    n2 = n2+1;
  for (i=1; i<n2; i++)
    {
      ni = n-i;
      a[i]   = (REAL)  sqrt( (double) a[i]*a[i] + (double) a[ni]*a[ni] );
      a[ni]  = a[i];		/* even signal */
    }
}

/* Rectangular and Polar
   A cs-array has even magnitude and odd angle (almost)
   hence a polar cs-array stores magnitude in the first half (real part)
   and angle in the second half (imag part)
   except for a[0] real-only and a[n2] (n even)
   The angle of a[0] is either 0 (pos. sign) or pi (neg. sign),
   but there is no place in an n-point cs-array to store this, so
   a[0] and a[n2] when n even are left unchanged  when going polar.
   as opposed to taking their absolute values, for magnitude. */

DEFINE_PRIMITIVE ("CS-ARRAY-TO-POLAR!", Prim_cs_array_to_polar, 1,1, 0)
{
  long n, i;
  REAL *a;
  void cs_array_to_polar();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  a  = ARRAY_CONTENTS(ARG_REF(1));
  n = ARRAY_LENGTH(ARG_REF(1));
  cs_array_to_polar(a,n);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
cs_array_to_polar (a,n)
     REAL *a;
     long n;
{
  long i, n2;
  double real, imag;		/* temporary variables */
  n2 = n/2;			/* integer division truncates down */
  /* a[0] stores both magnitude and angle
     (pos. sign angle=0 , neg. sign angle=pi) */
  if (2*n2 == n)		/* even length, n2 is only real */
    ;				/* a[n2] stores sign information like a[0] */
  else
    /* odd length, make the loop include the n2 index */
    n2 = n2+1;
  for (i=1; i<n2; i++)
    {
      real = (double) a[i];
      imag = (double) a[n-i];
      a[i]   = (REAL)  sqrt( real*real + imag*imag );
      if (a[i] == 0.0)
	a[n-i] = 0.0;
      else
	a[n-i] = (REAL) atan2( imag, real );
    }
}

DEFINE_PRIMITIVE ("CS-ARRAY-TO-RECTANGULAR!", Prim_cs_array_to_rectangular, 1,1, 0)
{
  long n,n2, i;
  double magn,angl;		/* temporary variables */
  REAL *a;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  a  = ARRAY_CONTENTS(ARG_REF(1));
  n = ARRAY_LENGTH(ARG_REF(1));
  n2 = n/2;			/* integer division truncates down */
  ;				/* a[0] is okay */
  if (2*n2 == n)		/* even length, n2 is real only */
    ;				/* a[n2] is okay */
  else
    n2 = n2+1; /* odd length, make the loop include the n2 index */
  for (i=1; i<n2; i++)
  { magn = (double) a[i];
    angl = (double) a[n-i];
    a[i]   = (REAL)  magn * cos(angl);
    a[n-i] = (REAL)  magn * sin(angl); }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Convolution in the Time-Domain */
/* In the following macro
   To1 and To2 should be (Length1-1) and (Length2-1) respectively. */

#define C_Convolution_Point_Macro(X, Y, To1, To2, N, Result)		\
{									\
  long Min_of_N_To1 = (min ((N), (To1)));				\
  long mi, N_minus_mi;							\
  REAL Sum = 0.0;							\
  for (mi = (max (0, ((N) - (To2)))), N_minus_mi = ((N) - mi);		\
       (mi <= Min_of_N_To1);						\
       mi += 1, N_minus_mi -= 1)					\
    Sum += ((X [mi]) * (Y [N_minus_mi]));				\
  (Result) = Sum;							\
}

DEFINE_PRIMITIVE ("CONVOLUTION-POINT", Prim_convolution_point, 3, 3, 0)
{
  long Length1, Length2, N;
  REAL *Array1, *Array2;
  REAL C_Result;
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  Length1 = ARRAY_LENGTH (ARG_REF (1));
  Length2 = ARRAY_LENGTH (ARG_REF (2));
  N = (arg_nonnegative_integer (3));
  Array1 = ARRAY_CONTENTS (ARG_REF (1));
  Array2 = ARRAY_CONTENTS (ARG_REF (2));
  C_Convolution_Point_Macro(Array1, Array2, Length1-1, Length2-1, N, C_Result);
  PRIMITIVE_RETURN (double_to_flonum ((double) C_Result));
}

DEFINE_PRIMITIVE ("ARRAY-CONVOLUTION-IN-TIME!", Prim_array_convolution_in_time, 3, 3, 0)
{
  long n,m,l, n_1,m_1, i;
  REAL *a,*b,*c;
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  CHECK_ARG (3, ARRAY_P);
  a = ARRAY_CONTENTS(ARG_REF(1));
  b = ARRAY_CONTENTS(ARG_REF(2));
  c = ARRAY_CONTENTS(ARG_REF(3));
  n = ARRAY_LENGTH(ARG_REF(1));
  m = ARRAY_LENGTH(ARG_REF(2));
  l = n+m-1;			/* resulting length */
  if (l != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  n_1 = n-1; m_1 = m-1;
  for (i=0; i<l; i++)
  { C_Convolution_Point_Macro(a, b, n_1, m_1, i, c[i]); }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ARRAY-MULTIPLY-INTO-SECOND-ONE!", Prim_array_multiply_into_second_one, 2, 2, 0)
{
  long Length, i;
  REAL *To_Here;
  REAL *From_Here_1, *From_Here_2;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  Length = ARRAY_LENGTH (ARG_REF (1));
  if (Length != ARRAY_LENGTH (ARG_REF (2))) error_bad_range_arg (2);
  From_Here_1 = ARRAY_CONTENTS (ARG_REF (1));
  From_Here_2 = ARRAY_CONTENTS (ARG_REF (2));
  To_Here = From_Here_2;
  for (i=0; i < Length; i++)
    {
      *To_Here++ = (*From_Here_1) * (*From_Here_2);
      From_Here_1++ ;
      From_Here_2++ ;
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* complex-array-operation-2!
   groups together procedures that use 2 complex-arrays
   and store result in either 1st or 2nd */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-2!", Prim_complex_array_operation_2, 5,5, 0)
{
  long n, opcode;
  REAL *ax,*ay, *bx,*by;
  void complex_array_multiply_into_second_one();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* ax array -- n      real         */
  CHECK_ARG (3, ARRAY_P);	/* ay array -- n      imag         */
  CHECK_ARG (4, ARRAY_P);	/* bx array -- n      real         */
  CHECK_ARG (5, ARRAY_P);	/* by array -- n      imag         */
  n = ARRAY_LENGTH(ARG_REF(2));
  if (n != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  if (n != ARRAY_LENGTH(ARG_REF(4))) error_bad_range_arg(4);
  if (n != ARRAY_LENGTH(ARG_REF(4))) error_bad_range_arg(5);
  ax  = ARRAY_CONTENTS(ARG_REF(2)); /*  real */
  ay  = ARRAY_CONTENTS(ARG_REF(3)); /*  imag */
  bx  = ARRAY_CONTENTS(ARG_REF(4)); /*  real */
  by  = ARRAY_CONTENTS(ARG_REF(5)); /*  imag */
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    complex_array_multiply_into_second_one(ax,ay,bx,by, n);
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
complex_array_multiply_into_second_one (ax,ay,bx,by, n)
     REAL *ax,*ay,*bx,*by;
     long n;
{
  long i;
  REAL temp;
  for (i=0;i<n;i++)
    {
      temp   = ax[i]*bx[i]  -  ay[i]*by[i]; /*  real part */
      by[i]  = ax[i]*by[i]  +  ay[i]*bx[i]; /*  imag part */
      bx[i]  = temp;
    }
}

void
C_Array_Complex_Multiply_Into_First_One (a,b,c,d, length) /* used in fft.c */
     REAL *a,*b,*c,*d;
     long length;
{
  long i;
  REAL temp;
  for (i=0;i<length;i++)
    {
      temp = a[i]*c[i] - b[i]*d[i];
      b[i] = a[i]*d[i] + b[i]*c[i];
      a[i] = temp;
    }
}

DEFINE_PRIMITIVE ("ARRAY-DIVIDE-INTO-XXX!", Prim_array_divide_into_xxx, 4,4, 0)
{
  long n, i, one_or_two;
  REAL *x,*y,*z, inf;
  void array_divide_into_z();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  inf = (arg_real (3));
  /* where to store result of division */
  one_or_two = (arg_nonnegative_integer (4));
  x = ARRAY_CONTENTS(ARG_REF(1));
  y = ARRAY_CONTENTS(ARG_REF(2));
  n = ARRAY_LENGTH(ARG_REF(1));
  if (n!=(ARRAY_LENGTH(ARG_REF(2)))) error_bad_range_arg(2);
  if (one_or_two == 1)
    array_divide_into_z( x,y, x,  n, inf);
  else if (one_or_two == 2)
    array_divide_into_z( x,y, y,  n, inf);
  else
    error_bad_range_arg(4);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
array_divide_into_z (x,y, z, n, inf) /* z can either x or y */
     REAL *x,*y,*z, inf;
     long n;
{
  long i;
  for (i=0; i<n; i++)
    {
      if (y[i] == 0.0)
	{
	  if (x[i] == 0.0)
	    z[i] = 1.0;
	  else
	    z[i] = inf * x[i];
	}
      else
	z[i] = x[i] / y[i];
    }
}

/* complex-array-operation-2b!
   groups together procedures that use 2 complex-arrays
   & 1 additional real number
   and store result in either 1st or 2nd (e.g. division) */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-2B!", Prim_complex_array_operation_2b, 6,6, 0)
{ long n, opcode;
  REAL *ax,*ay, *bx,*by,  inf;
  void complex_array_divide_into_z();
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* ax array -- n      real         */
  CHECK_ARG (3, ARRAY_P);	/* ay array -- n      imag         */
  CHECK_ARG (4, ARRAY_P);	/* bx array -- n      real         */
  CHECK_ARG (5, ARRAY_P);	/* by array -- n      imag         */
  inf = (arg_real (6));		/* User-Provided Infinity */
  n = ARRAY_LENGTH(ARG_REF(2));
  if (n != ARRAY_LENGTH(ARG_REF(3))) error_bad_range_arg(3);
  if (n != ARRAY_LENGTH(ARG_REF(4))) error_bad_range_arg(4);
  if (n != ARRAY_LENGTH(ARG_REF(4))) error_bad_range_arg(5);
  ax  = ARRAY_CONTENTS(ARG_REF(2)); /*  real */
  ay  = ARRAY_CONTENTS(ARG_REF(3)); /*  imag */
  bx  = ARRAY_CONTENTS(ARG_REF(4)); /*  real */
  by  = ARRAY_CONTENTS(ARG_REF(5)); /*  imag */
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    complex_array_divide_into_z (ax,ay,bx,by, ax,ay,  n, inf);
  else if (opcode==2)
    complex_array_divide_into_z (ax,ay,bx,by, bx,by,  n, inf);
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
complex_array_divide_into_z (xr,xi, yr,yi, zr,zi, n, inf)
     REAL *xr,*xi, *yr,*yi, *zr,*zi, inf;
     long n;
{
  long i;
  fast double temp, radius;
  for (i=0; i<n; i++)
    {
      radius = (double) (yr[i] * yr[i]) + (yi[i] * yi[i]); /* denominator */
      if (radius == 0.0)
	{
	  if (xr[i] == 0.0)
	    zr[i] = 1.0;
	  else
	    zr[i] = inf * xr[i];
	  if (xi[i] == 0.0)
	    zi[i] = 1.0;
	  else
	    zi[i] = inf * xi[i];
	}
      else
	{
	  temp =  (double) (xr[i] * yr[i]  +  xi[i] * yi[i]);
	  zi[i] = (REAL) (xi[i] * yr[i]  -  xr[i] * yi[i]) / radius;
	  zr[i] = (REAL) temp                              / radius;
	}
    }
}

DEFINE_PRIMITIVE ("ARRAY-LINEAR-SUPERPOSITION-INTO-SECOND-ONE!", Prim_array_linear_superposition_into_second_one, 4, 4, 0)
{
  long n, i;
  REAL *To_Here, Coeff1, Coeff2;
  REAL *From_Here_1, *From_Here_2;
  PRIMITIVE_HEADER (4);
  Coeff1 = (arg_real (1));
  CHECK_ARG (2, ARRAY_P);
  Coeff2 = (arg_real (3));
  CHECK_ARG (4, ARRAY_P);
  n = (ARRAY_LENGTH (ARG_REF (2)));
  if (n != (ARRAY_LENGTH (ARG_REF (4))))
    error_bad_range_arg (4);
  From_Here_1 = (ARRAY_CONTENTS (ARG_REF (2)));
  From_Here_2 = (ARRAY_CONTENTS (ARG_REF (4)));
  To_Here = From_Here_2;
  for (i=0; i < n; i++)
    {
      *To_Here++ = (Coeff1 * (*From_Here_1)) + (Coeff2 * (*From_Here_2));
      From_Here_1++ ;
      From_Here_2++ ;
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*  m_pi = 3.14159265358979323846264338327950288419716939937510; */

DEFINE_PRIMITIVE ("SAMPLE-PERIODIC-FUNCTION", Prim_sample_periodic_function, 4, 4, 0)
{
  long N, i, Function_Number;
  double Signal_Frequency, Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  SCHEME_OBJECT Result, Pfunction_number, Psignal_frequency;
  SCHEME_OBJECT Pfunction_Number;
  REAL *To_Here;
  double unit_square_wave(), unit_triangle_wave();
  PRIMITIVE_HEADER (4);
  Function_Number = (arg_index_integer (1, 11));
  Signal_Frequency = (arg_real_number (2));
  if (Signal_Frequency == 0)
    error_bad_range_arg (2);
  Sampling_Frequency = (arg_real_number (3));
  if (Sampling_Frequency == 0)
    error_bad_range_arg (3);
  N = (arg_nonnegative_integer (4));
  Result = (allocate_array (N));
  To_Here = ARRAY_CONTENTS(Result);
  DT = (double) (twopi * Signal_Frequency * (1 / Sampling_Frequency));
  if (Function_Number == 0)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) cos(DTi);
  else if (Function_Number == 1)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) sin(DTi);
  else if (Function_Number == 2)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) unit_square_wave(DTi);
  else if (Function_Number == 3)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) unit_triangle_wave(DTi);
  else
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (Result);
}

double
hamming (t, length)
     double t, length;
{
  double twopi = 6.28318530717958;
  double pi = twopi/2.;
  double t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0)) return(.08 + .46 * (1 - t_bar));
  else return (0);
}

double
unit_square_wave (t)
     double t;
{
  double twopi = 6.28318530717958;
  double fmod(), fabs();
  double pi = twopi/2.;
  double t_bar = ((REAL) fabs(fmod( ((double) t), twopi)));
  if (t_bar < pi) return(1);
  else return(-1);
}

double
unit_triangle_wave (t)
     double t;
{
  double twopi = 6.28318530717958;
  double pi = twopi/2.;
  double pi_half = pi/2.;
  double three_pi_half = pi+pi_half;
  double t_bar = ((double) fabs(fmod( ((double) t), twopi)));
  if (t_bar<pi_half)
    return (-(t_bar/pi));
  else if (t_bar<pi)
    return (t_bar/pi);
  else if (t_bar<three_pi_half)
    return ((twopi-t_bar)/pi);
  else
    return (-((twopi-t_bar)/pi));
}

DEFINE_PRIMITIVE ("ARRAY-HANNING!", Prim_array_hanning, 2,2, 0)
{
  long n, hanning_power;
  REAL *a;
  void C_Array_Make_Hanning();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);	/* input array -- n */
  CHECK_ARG (2, FIXNUM_P);	/* hanning power */
  a  = ARRAY_CONTENTS(ARG_REF(1));
  n = ARRAY_LENGTH(ARG_REF(1));
  hanning_power = arg_nonnegative_integer(2);
  C_Array_Make_Hanning (a, n, hanning_power);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
C_Array_Make_Hanning (f1, length, power)
     REAL f1[];
     long length, power;
{
  double window_length;
  long i;
  double integer_power(), hanning();
  window_length = ((double) length);
  for (i=0;i<length;i++)
    {
      f1[i] = ((REAL) hanning(((double) i), window_length));
      f1[i] = (REAL) integer_power(((double) f1[i]), power);
    }
}

double
hanning (t, length)
     double t, length;
{
  double twopi = 6.283185307179586476925287;
  double t_bar;
  t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0))
    return(.5 * (1 - t_bar));
  else
    return (0.0);
}

double
integer_power (a, n)
     double a;
     long n;
{
  double b;
  double integer_power();

  if (n<0) exit(-1);
  else if (n==0) return(1.0);
  else if (n==1) return(a);
  else if ((n%2) == 0)
    {
      b = integer_power(a, n/2);
      return(b*b);
    }
  else
    return(a * integer_power(a, (n-1)));
}

/* array-operation-1!
   groups together procedures that use 1 array
   and store the result in place (e.g. random) */

DEFINE_PRIMITIVE ("ARRAY-OPERATION-1!", Prim_array_operation_1, 2,2, 0)
{
  long n, opcode;
  REAL *a;
  void array_random();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n */
  n = ARRAY_LENGTH(ARG_REF(2));
  a  = ARRAY_CONTENTS(ARG_REF(2));
  opcode = arg_nonnegative_integer(1);
  if (opcode==1)
    array_random(a,n);
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
array_random (a,n)
     REAL *a;
     long n;
{
  long i;
  /* HPUX 3: Rand uses a multiplicative congruential random-number generator
     with period 2^32 that returns successive pseudo-random numbers in the
     range from 0 to 2^15-1 */
  for (i=0;i<n;i++)
    a[i] = ((REAL) rand()) * (3.0517578125e-5);
  /* 3.051xxx = 2^(-15) makes the range from 0 to 1 */
}

/* The following should go away.
   superceded by ARRAY-CONS-INTEGERS, ARRAY-UNARY-FUNCTION and array-random */
DEFINE_PRIMITIVE ("SAMPLE-APERIODIC-FUNCTION", Prim_sample_aperiodic_function, 3, 3, 0)
{
  long N, i, Function_Number;
  double Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  SCHEME_OBJECT Result;
  REAL *To_Here, twopi_dt;
  PRIMITIVE_HEADER (3);
  Function_Number = (arg_index_integer (1, 7));
  Sampling_Frequency = (arg_real_number (2));
  if (Sampling_Frequency == 0)
    error_bad_range_arg (2);
  N = (arg_nonnegative_integer (3));
  Result = (allocate_array (N));
  To_Here = ARRAY_CONTENTS(Result);
  DT = (twopi * (1 / Sampling_Frequency));
  if      (Function_Number == 0)
    /* HPUX 3: Rand uses a multiplicative congruential random-number generator
       with period 2^32 that returns successive pseudo-random numbers in the
       range from 0 to 2^15-1 */
    for (i=0; i<N; i++)
      /* 2^(-15) makes range from 0 to 1 */
      *To_Here++ = 3.0517578125e-5 * ((REAL) rand());
  else if (Function_Number == 1)
  { double length=DT*N;
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) hanning(DTi, length);
  }
  else if (Function_Number == 2)
  { double length=DT*N;
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) hamming(DTi, length);
  }
  else if (Function_Number == 3)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) sqrt(DTi);
  else if (Function_Number == 4)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) log(DTi);
  else if (Function_Number == 5)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = (REAL) exp(DTi);
  else
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (Result);
}

DEFINE_PRIMITIVE ("ARRAY-PERIODIC-DOWNSAMPLE", Prim_array_periodic_downsample, 2, 2, 0)
{
  long Length, Pseudo_Length, Sampling_Ratio;
  REAL *Array, *To_Here;
  SCHEME_OBJECT Result;
  long i, array_index;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  Length = ARRAY_LENGTH(ARG_REF (1));
  Sampling_Ratio = ((arg_integer (2)) % Length);
  if (Sampling_Ratio < 1)
    error_bad_range_arg (2);
  Array = ARRAY_CONTENTS(ARG_REF (1));
  Result = (allocate_array (Length));
  To_Here = ARRAY_CONTENTS(Result);
  Pseudo_Length = Length * Sampling_Ratio;
  /* new Array has the same Length by assuming periodicity */
  for (i=0; i<Pseudo_Length; i += Sampling_Ratio)
  { array_index = i % Length;
    *To_Here++ = Array[array_index]; }
  PRIMITIVE_RETURN (Result);
}

/* Shift is not done in place (no side-effects). */
DEFINE_PRIMITIVE ("ARRAY-PERIODIC-SHIFT", Prim_array_periodic_shift, 2, 2, 0)
{
  long Length, Shift;
  REAL *Array, *To_Here;
  SCHEME_OBJECT Result;
  long i, array_index;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  Length = ARRAY_LENGTH(ARG_REF (1));
  /* periodic waveform, same sign as dividend */
  Shift = ((arg_integer (2)) % Length);
  Array = ARRAY_CONTENTS(ARG_REF (1));
  Result = (allocate_array (Length));
  To_Here = ARRAY_CONTENTS(Result);
  /* new Array has the same Length by assuming periodicity */
  for (i=0; i<Length; i++) {
    array_index = (i+Shift) % Length;
    if (array_index<0) array_index = Length + array_index; /* wrap around */
    *To_Here++ = Array[array_index]; }
  PRIMITIVE_RETURN (Result);
}

/* This is done here because array-map is very slow */
DEFINE_PRIMITIVE ("ARRAY-APERIODIC-DOWNSAMPLE", Prim_array_aperiodic_downsample, 2, 2, 0)
{
  long Length, New_Length, Sampling_Ratio;
  REAL *Array, *To_Here;
  SCHEME_OBJECT Result;
  long i, array_index;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  Array = ARRAY_CONTENTS(ARG_REF (1));
  Length = ARRAY_LENGTH(ARG_REF (1));
  Sampling_Ratio = (arg_integer_in_range (2, 1, (Length + 1)));
  if (Length < 1) error_bad_range_arg (1);
  /* 1 for first one and then the rest --- integer division chops */
  New_Length = 1 + ((Length-1)/Sampling_Ratio);
  Result = (allocate_array (New_Length));
  To_Here = ARRAY_CONTENTS(Result);
  for (i=0; i<Length; i += Sampling_Ratio)
    *To_Here++ = Array[i];
  PRIMITIVE_RETURN (Result);
}

/* one more hack for speed */

/* (SOLVE-SYSTEM A B N)
    Solves the system of equations Ax = b.  A and B are
    arrays and b is the order of the system.  Returns x.
    From the Fortran procedure in Strang. */

DEFINE_PRIMITIVE ("SOLVE-SYSTEM", Prim_gaussian_elimination, 2, 2, 0)
{
  REAL *A, *B, *X;
  long Length;
  SCHEME_OBJECT Result;
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  Length  = ARRAY_LENGTH(ARG_REF (2));
  if ((Length*Length) != ARRAY_LENGTH(ARG_REF (1))) error_bad_range_arg (2);
  A = ARRAY_CONTENTS(ARG_REF (1));
  B = ARRAY_CONTENTS(ARG_REF (2));
  Result = (allocate_array (Length));
  X = ARRAY_CONTENTS(Result);
  C_Array_Copy(B, X, Length);
  C_Gaussian_Elimination(A, X, Length);
  PRIMITIVE_RETURN (Result);
}

/* C routine side-effects b. */
C_Gaussian_Elimination (a, b, n)
     REAL *a, *b;
     long n;
{
  long *pvt;
  REAL p, t;
  long i, j, k, m;
  Primitive_GC_If_Needed (n);
  pvt = ((long *) Free);
  *(pvt+n-1) = 1;
  if (n != 1) {
    for (k=1; k<n; k++) {
      m = k;
      for (i=k+1; i<=n; i++)
	if (fabs(*(a+i+(k-1)*n-1)) > fabs(*(a+m+(k-1)*n-1)))
	  m = i;
      *(pvt+k-1) = m;
      if (m != k)
	*(pvt+n-1) = - *(pvt+n-1);
      p = *(a+m+(k-1)*n-1);
      *(a+m+(k-1)*n-1) = *(a+k+(k-1)*n-1);
      *(a+k+(k-1)*n-1) = p;
      if (p != 0.0) {
	for (i=k+1; i<=n; i++)
	  *(a+i+(k-1)*n-1) = - *(a+i+(k-1)*n-1) / p;
	for (j=k+1; j<=n; j++) {
	  t = *(a+m+(j-1)*n-1);
	  *(a+m+(j-1)*n-1) = *(a+k+(j-1)*n-1);
	  *(a+k+(j-1)*n-1) = t;
	  if (t != 0.0)
	    for (i=k+1; i<=n; i++)
	      *(a+i+(j-1)*n-1) = *(a+i+(j-1)*n-1) + *(a+i+(k-1)*n-1) * t;
	}
      }
    }
    for (k=1; k<n; k++) {
      m = *(pvt+k-1);
      t = *(b+m-1);
      *(b+m-1) = *(b+k-1);
      *(b+k-1) = t;
      for (i=k+1; i<=n; i++)
	*(b+i-1) = *(b+i-1) + *(a+i+(k-1)*n-1) * t;
    }
    for (j=1; j<n; j++) {
      k = n - j + 1;
      *(b+k-1) = *(b+k-1) / *(a+k+(k-1)*n-1);
      t = - *(b+k-1);
      for (i=1; i <= n-j; i++)
	*(b+i-1) = *(b+i-1) + *(a+i+(k-1)*n-1) * t;
    }
  }
  *b = *b / *a;
  return;
}
