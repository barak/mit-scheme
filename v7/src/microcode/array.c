/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/array.c,v 9.38 1989/07/30 23:59:02 pas Exp $ */


#include "scheme.h"
#include "prims.h"
#include "flonum.h"
#include "array.h"
#include <math.h>
#include <values.h>
/* <values.h> contains some math constants */

/* .
   ARRAY (as a scheme object)
   is a usual array (in C) containing REAL numbers (float/double)
   and tagged as a NM_VECTOR
   
   Basic contents:
   constructors, selectors, arithmetic operations, 
   conversion routines between C_Array, and Scheme_Vector      
   
   see array.h for macros, NM_VECTOR, and extern 
   */


/* mathematical constants */
#ifdef PI
#undef PI
#endif
#define PI    3.141592653589793238462643
#define TWOPI 6.283185307179586476925287
#define SQRT_2          1.4142135623730950488
#define ONE_OVER_SQRT_2  .7071067811865475244
/* Abramowitz and Stegun */


/* first some utilities */

int Scheme_Number_To_REAL(Arg, Cell) Pointer Arg; REAL *Cell;
/* 0 means conversion ok, 1 means too big, 2 means not a number */
{ long Value;
  switch (Type_Code(Arg)) {
  case TC_FIXNUM: 
    if (Get_Integer(Arg) == 0)
      *Cell = 0.0;
    else                                                    
    { long Value;
      Sign_Extend(Arg, Value);                             
      *Cell = ((REAL) Value);
    }
    break;
  case TC_BIG_FLONUM:
    *Cell = ((REAL) Get_Float(Arg));
    break;
  case TC_BIG_FIXNUM:
  { Pointer Result = Big_To_Float(Arg);
    if (Type_Code(Result) == TC_BIG_FLONUM) 
      *Cell = ((REAL) Get_Float(Result));
    else return (1); 
  }
    break;
  default: return (2);
    break;
  }
  return (0);
}

int Scheme_Number_To_Double(Arg, Cell) Pointer Arg; double *Cell;
/* 0 means conversion ok, 1 means too big, 2 means not a number */
{ long Value;
  switch (Type_Code(Arg)) {
  case TC_FIXNUM: 
    if (Get_Integer(Arg) == 0)
      *Cell = 0.0;
    else                                                    
    { long Value;
      Sign_Extend(Arg, Value);                             
      *Cell = ((double) Value);
    }
    break;
  case TC_BIG_FLONUM:
    *Cell = ((double) Get_Float(Arg));
    break;
  case TC_BIG_FIXNUM:
  { Pointer Result = Big_To_Float(Arg);
    if (Type_Code(Result) == TC_BIG_FLONUM) 
      *Cell = ((double) Get_Float(Result));
    else return (1); 
  }
    break;
  default: return (2);
    break;
  }
  return (0);
}

/* c */

/*   I think this is not needed, it can be done in scheme
     DEFINE_PRIMITIVE ("ARRAY?", Prim_array_predicate, 1, 1, 0)
     { Primitive_1_Args();
     if (Type_Code(Arg1)==TC_ARRAY) return SHARP_F;
     else return SHARP_F;
     }
     */

DEFINE_PRIMITIVE ("VECTOR->ARRAY", Prim_vector_to_array, 1, 1, 0)
{ Pointer Scheme_Vector_To_Scheme_Array();
  Primitive_1_Args();
  Arg_1_Type(TC_VECTOR);
  return Scheme_Vector_To_Scheme_Array(Arg1);
}

DEFINE_PRIMITIVE ("ARRAY->VECTOR", Prim_array_to_vector, 1, 1, 0)
{ Pointer Scheme_Array_To_Scheme_Vector();
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  return Scheme_Array_To_Scheme_Vector(Arg1);
}

/* array-cons = (array-allocate followed by array-initialize!)
   The two are separated because all too often, we only need 
   array memory space. Even though the initialization is fast, it 
   happens so often that we get savings.
   Also array-initialize!  occurs via subarray-offset-scale!. 
   
   */
DEFINE_PRIMITIVE ("ARRAY-ALLOCATE", Prim_array_allocate, 1,1, 0)
{ long n,allocated_cells;
  Pointer Result;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, FIXNUM_P);

  n = arg_nonnegative_integer(1); /* length of array to allocate */
  if (n > ARRAY_MAX_LENGTH) error_bad_range_arg(1); /* avoid memory overflow */
  
  Allocate_Array(Result,n,allocated_cells);
  PRIMITIVE_RETURN (Result);
}


DEFINE_PRIMITIVE ("ARRAY-CONS-REALS", Prim_array_cons_reals, 3, 3, 0)
{ long i, Length, allocated_cells;
  REAL *a;
  double from, dt;
  Pointer Result;
  int errcode;
  Primitive_3_Args();
  
  errcode = Scheme_Number_To_Double(Arg1, &from); /*         starting time */
  if (errcode == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  errcode = Scheme_Number_To_Double(Arg2, &dt); /*           dt interval */
  if (errcode == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Length = Get_Integer(Arg3);	/* number of points */
  
  Allocate_Array(Result,Length,allocated_cells);
  a = Scheme_Array_To_C_Array(Result);
  a[0] = (REAL) from; 
  for (i=1; i<Length; i++) { from = from + dt; a[i] = (REAL) from; }
  /* the variable <from> is used as double precision accumulator */
  return Result; 
}

DEFINE_PRIMITIVE ("ARRAY-LENGTH", Prim_array_length, 1, 1, 0)
{ Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  return Make_Pointer(TC_FIXNUM, Array_Length(Arg1));
}

DEFINE_PRIMITIVE ("ARRAY-REF", Prim_array_ref, 2, 2, 0)
{ long Index;
  REAL *Array, value;
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Index, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Array = Scheme_Array_To_C_Array(Arg1);
  value = Array[Index];
  Reduced_Flonum_Result((double) value);
}

DEFINE_PRIMITIVE ("ARRAY-SET!", Prim_array_set, 3, 3, 0)
{ long Index;
  REAL *Array, Old_Value;
  int errcode;

  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Index, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Array = Scheme_Array_To_C_Array(Arg1);
  Old_Value = Array[Index];

  errcode = Scheme_Number_To_REAL(Arg3, &Array[Index]);
  if (errcode == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);

  Reduced_Flonum_Result((double) Old_Value);
}

/*____________________ file readers ___________
  ascii and 2bint formats 
  ______________________________________________*/

/* Reading data from files 
   ATTENTION: for reading REAL numbers, use "lf" for double, "%f" for float 
   */
#if (REAL_IS_DEFINED_DOUBLE == 1)
#define REALREAD  "%lf"
#define REALREAD2 "%lf %lf"
#else
#define REALREAD  "%f"
#define REALREAD2 "%f %f"
#endif

DEFINE_PRIMITIVE ("ARRAY-READ-ASCII-FILE", Prim_array_read_ascii_file, 2, 2, 0)
{ FILE *fp;
  long Length, allocated_cells;
  REAL *a;
  SCHEME_ARRAY Result;
  Primitive_2_Args();
  Arg_1_Type(TC_CHARACTER_STRING);                /* filename */
  Arg_2_Type(TC_FIXNUM);                          /* length of data */
  Length = Get_Integer(Arg2);
  if (Length <= 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Allocate_Array(Result, Length, allocated_cells);
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  printf("Reading ascii file ...\n"); fflush(stdout);
  a = Scheme_Array_To_C_Array(Result);
  C_Array_Read_Ascii_File(a,Length,fp);
  return Result;
}
C_Array_Read_Ascii_File(a,N,fp)           /* 16 ascii decimal digits */
     REAL *a; long N; FILE *fp;
{ long i;
  for (i=0; i<N; i++) {
    if ( (fscanf(fp, REALREAD, &(a[i]))) != 1)
    { printf("Not enough values read ---\n Last Point was %d with value % .16e \n", i, a[i-1]);
      return SHARP_F; }}
  Close_File(fp);
}

DEFINE_PRIMITIVE ("ARRAY-WRITE-ASCII-FILE", Prim_array_write_ascii_file, 2, 2, 0)
{ FILE *fp;
  long Length;
  REAL *a;
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Arg_2_Type(TC_CHARACTER_STRING);                /* filename */
  if (!(Open_File(Arg2, "w", &fp))) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  printf("Writing ascii file ...\n"); fflush(stdout);
  a = Scheme_Array_To_C_Array(Arg1);
  C_Array_Write_Ascii_File(a,Length,fp);
  return SHARP_F;
}
C_Array_Write_Ascii_File(a,N,fp)           /* 16 ascii decimal digits */
     REAL *a; long N; FILE *fp;
{ long i;
  for (i=0; i<N; i++) {
    if (feof(fp)!=0) { printf("Not enough values written ---\n Last Point was %d with value %---\n", (i-1), a[i-1]);
		       return SHARP_F; }
    fprintf(fp, "% .16e \n", a[i]); }
  Close_File(fp);
}

/* 2BINT FORMAT = integer stored in 2 consecutive bytes.
   We need to use 2bint because on many machines (bobcats included)
   "putw", and "getw" use 4 byte integers (C int) ---> waste lots of space.
   */
DEFINE_PRIMITIVE ("ARRAY-READ-2BINT-FILE", Prim_array_read_2bint_file, 2, 2, 0)
{ FILE *fp;
  long Length, allocated_cells;
  REAL *a;
  SCHEME_ARRAY Result;
  Primitive_2_Args();
  Arg_1_Type(TC_CHARACTER_STRING);                /* filename */
  Arg_2_Type(TC_FIXNUM);                          /* length of data */
  Length = Get_Integer(Arg2);
  if (Length <= 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Allocate_Array(Result, Length, allocated_cells);
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  printf("Reading 2bint file ...\n"); fflush(stdout);
  a = Scheme_Array_To_C_Array(Result);
  C_Array_Read_2bint_File(a,Length,fp);
  return Result;
}
C_Array_Read_2bint_File(a,N,fp)
     REAL *a; long N; FILE *fp;
{ long i;
  int foo1,foo2;
  for (i=0;i<N;i++) {
    if (feof(fp)!=0) { printf("Not enough values read: last read i-1=%d , value=%d\n", (i-1), a[i-1]);
		       return SHARP_F; }
    foo1=getc(fp); foo2=getc(fp); /* Read 2BYTE INT FORMAT */
    a[i] = ((REAL)
	    ((foo1<<8) ^ foo2) ); /* put together the integer */
  }
  Close_File(fp);
}
/* C_Array_Write_2bint_File  
   is not implemented yet, I do not have the time to do it now. */

/* ----- Read data from files --- end*/



/* ARRAY-COPY!    a very powerful primitive
   See array.scm for its many applications.
   Be Careful when source and destination are the same array.
   */
DEFINE_PRIMITIVE ("SUBARRAY-COPY!", Prim_subarray_copy, 5, 5, 0)
{ long i, i1,  i2;
  long m, at1, at2;
  REAL *a,*b;
  
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, ARRAY_P);	/*         source      array a   */
  CHECK_ARG (2, ARRAY_P);	/*         destination array b   */
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  at1 = arg_nonnegative_integer(3); /*     at1 = starting index in source array */
  at2 = arg_nonnegative_integer(4); /*     at2 = starting index in destination array */
  m   = arg_nonnegative_integer(5); /*     m   = number of points to copy */

  if ((at1 + m) > (Array_Length(ARG_REF(1)))) error_bad_range_arg(3); 
  if ((at2 + m) > (Array_Length(ARG_REF(2)))) error_bad_range_arg(4);
  /* These 2 checks cover all cases */
  
  for (i=0,i1=at1,i2=at2;   i<m;   i++,i1++,i2++)
    b[i2] = a[i1];
  
  PRIMITIVE_RETURN (NIL);
}


DEFINE_PRIMITIVE ("ARRAY-REVERSE!", Prim_array_reverse, 1, 1, 0)
{ long Length, i,j, Half_Length;
  REAL *Array, Temp;
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Half_Length = Length/2;
  Array = Scheme_Array_To_C_Array(Arg1);
  
  for (i=0, j=Length-1; i<Half_Length; i++, j--) {
    Temp     = Array[j];
    Array[j] = Array[i];
    Array[i] = Temp;
  }
  return Arg1;
}

DEFINE_PRIMITIVE ("ARRAY-TIME-REVERSE!",
		  Prim_array_time_reverse, 1, 1, 0)
{ long i, n;
  REAL *a;
  void C_Array_Time_Reverse();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  n = Array_Length(ARG_REF(1));
  
  C_Array_Time_Reverse(a,n);
  
  PRIMITIVE_RETURN (NIL);
}

/* time-reverse
   x[0] remains fixed. (time=0)
   x[i] swapped with x[n-i]. (mirror image around x[0])
   */
void C_Array_Time_Reverse(x,n)
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
   and avoids computation   when offset or scale are degenerate 0,1 
   */
DEFINE_PRIMITIVE ("SUBARRAY-OFFSET-SCALE!",
		  Prim_subarray_offset_scale, 5, 5, 0)
{ long i, at, m,mplus;
  REAL *a, offset,scale;
  int errcode;
  
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, FIXNUM_P);
  CHECK_ARG (3, FIXNUM_P);
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  at = arg_nonnegative_integer(2); /*       at = starting index             */
  m  = arg_nonnegative_integer(3); /*       m  = number of points to change */
  
  mplus = at + m;
  if (mplus > (Array_Length(ARG_REF(1)))) error_bad_range_arg(3);

  errcode = Scheme_Number_To_REAL(ARG_REF(4), &offset);
  if (errcode==1) error_bad_range_arg(4); if (errcode==2) error_wrong_type_arg(4); 
  errcode = Scheme_Number_To_REAL(ARG_REF(5), &scale);
  if (errcode==1) error_bad_range_arg(5); if (errcode==2) error_wrong_type_arg(5); 
  
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
  
  PRIMITIVE_RETURN (NIL);
}


DEFINE_PRIMITIVE ("COMPLEX-SUBARRAY-COMPLEX-SCALE!",
		  Prim_complex_subarray_complex_scale, 6,6, 0)
{ long i, at,m,mplus;
  REAL *a,*b;			/* (a,b) = (real,imag) arrays */
  double temp, minus_y,  x, y;	/* (x,y) = (real,imag) scale */
  int errcode;
  
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  CHECK_ARG (3, FIXNUM_P);
  CHECK_ARG (4, FIXNUM_P);
  
  at = arg_nonnegative_integer(3); /*       at = starting index             */
  m  = arg_nonnegative_integer(4); /*       m  = number of points to change */
  mplus = at + m;
  if (mplus > (Array_Length(ARG_REF(1)))) error_bad_range_arg(4);
  
  errcode = Scheme_Number_To_Double(ARG_REF(5), &x);
  if (errcode==1) error_bad_range_arg(5); if (errcode==2) error_wrong_type_arg(5); 
  errcode = Scheme_Number_To_Double(ARG_REF(6), &y);
  if (errcode==1) error_bad_range_arg(6); if (errcode==2) error_wrong_type_arg(6); 
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  if ((Array_Length(ARG_REF(1))) != (Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  
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
  
  PRIMITIVE_RETURN (NIL);
}


/* Accumulate
   using combinators              * 
   corresponding type codes       1
   */
DEFINE_PRIMITIVE ("COMPLEX-SUBARRAY-ACCUMULATE!",
		  Prim_complex_subarray_accumulate, 6,6, 0)
{ long  at,m,mplus, tc, i;
  REAL *a,*b;			/* (a,b) = (real,imag) input arrays */
  REAL *c;			/* result = output array of length 2, holds a complex number */
  double x, y, temp;
  
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, ARRAY_P);	/* a = input array (real) */
  CHECK_ARG (2, ARRAY_P);	/* b = input array (imag) */
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  if ((Array_Length(ARG_REF(1))) != (Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  tc = arg_nonnegative_integer(3); /*       tc = type code 0 or 1            */
  at = arg_nonnegative_integer(4); /*       at = starting index              */
  m  = arg_nonnegative_integer(5); /*       m  = number of points to process */
  CHECK_ARG (6, ARRAY_P);	/* c = output array of length 2 */
  c = Scheme_Array_To_C_Array(ARG_REF(6));
  if ((Array_Length(ARG_REF(6))) != 2) error_bad_range_arg(6);
  
  mplus = at + m;
  if (mplus > (Array_Length(ARG_REF(1)))) error_bad_range_arg(5);

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
  PRIMITIVE_RETURN (NIL);  
}



DEFINE_PRIMITIVE ("CS-ARRAY-TO-COMPLEX-ARRAY!",
		  Prim_cs_array_to_complex_array, 3, 3, 0)
{ long n,n2,n2_1, i;
  REAL *a, *b,*c;
  
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  CHECK_ARG (3, ARRAY_P);
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  n = Array_Length(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  c = Scheme_Array_To_C_Array(ARG_REF(3));
  if (n!=(Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  if (n!=(Array_Length(ARG_REF(3)))) error_bad_range_arg(3);
  
  b[0]   = a[0];   c[0]   = 0.0; 
  
  n2   = n/2;			/* integer division truncates down */
  n2_1 = n2+1;
  
  if (2*n2 == n)		/* even length, n2 is only real */
  { b[n2]  = a[n2];  c[n2]  = 0.0; }
  else				/* odd length, make the loop include the n2 index */
  { n2   = n2+1;
    n2_1 = n2; }
  
  for (i=1; i<n2; i++)   { b[i] = a[i];
			   c[i] = a[n-i]; }
  for (i=n2_1; i<n; i++) { b[i] =  a[n-i];
			   c[i] = (-a[i]); }
  
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("CS-ARRAY-MULTIPLY-INTO-SECOND-ONE!",
		  Prim_cs_array_multiply_into_second_one, 2, 2, 0)
{ long n,n2;
  REAL *a, *b;
  void cs_array_multiply_into_second_one();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  n = Array_Length(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  if (n!=(Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  n2 = n/2;			/* integer division truncates down */
  cs_array_multiply_into_second_one(a,b, n,n2);
  PRIMITIVE_RETURN (NIL);
}

void cs_array_multiply_into_second_one(a,b, n,n2)
     REAL *a, *b; long n,n2;
{ REAL temp;
  long i,ni;
  b[0]   = a[0]  * b[0];
  
  if (2*n2 == n)		/* even length, n2 is only real */
    b[n2]  = a[n2] * b[n2];
  else				
    n2 = n2+1;			/* odd length, make the loop include the n2 index */
  
  for (i=1; i<n2; i++)
  { ni = n-i;
    temp   = a[i]*b[i]   -  a[ni]*b[ni]; /* real part */
    b[ni]  = a[i]*b[ni]  +  a[ni]*b[i];	/*  imag part */
    b[i]   = temp; }
}

DEFINE_PRIMITIVE ("CS-ARRAY-DIVIDE-INTO-XXX!",
		  Prim_cs_array_divide_into_xxx, 4, 4, 0)
{ long n,n2, one_or_two;
  REAL *a, *b, inf;
  int errcode;
  void cs_array_divide_into_z();

  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  errcode = Scheme_Number_To_REAL(ARG_REF(3), &inf);
  if (errcode==1) error_bad_range_arg(3); if (errcode==2) error_wrong_type_arg(3); 
  CHECK_ARG (4, FIXNUM_P);
  one_or_two = arg_nonnegative_integer(4); /* where to store result of division */
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  n = Array_Length(ARG_REF(1));
  if (n!=(Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  n2 = n/2;			/* integer division truncates down */
  
  if (one_or_two == 1)
    cs_array_divide_into_z(a,b, a,  n,n2, inf);
  else if (one_or_two == 2)
    cs_array_divide_into_z(a,b, b,  n,n2, inf);
  else
    error_bad_range_arg(4);
  PRIMITIVE_RETURN (NIL);
}

void cs_array_divide_into_second_one(a,b, n,n2,inf)   /* used in image.c */
     REAL *a,*b, inf; long n,n2;
{ void cs_array_divide_into_z();
  cs_array_divide_into_z(a,b, b, n,n2,inf);
}

void cs_array_divide_into_z(a,b, z, n,n2, inf)          /* z can be either a or b */
     REAL *a,*b,*z, inf; long n,n2;             
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
    n2 = n2+1;			/* odd length, make the loop include the n2 index */
  
  for (i=1; i<n2; i++)
  { ni = n-i;
    radius  = b[i]*b[i]   +  b[ni]*b[ni]; /* b^2 denominator = real^2 + imag^2 */
    
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
   
   Available functions :
   */

void REALabs(a,b) REAL *a,*b;
{ (*b) = ( (REAL) fabs( (double) (*a)) );
}
void REALexp(a,b) REAL *a,*b;
{ register double y;
  if ((y = exp((double) (*a))) == HUGE)
    Primitive_Error(ERR_ARG_1_BAD_RANGE); /* OVERFLOW */
  (*b) = ((REAL) y);
}
void REALlog(a,b) REAL *a,*b;
{ 
  if ((*a) < 0.0)
    error_bad_range_arg(1);	/* log(negative) */
  (*b) = ( (REAL) log( (double) (*a)) );
}

void REALtruncate(a,b) REAL *a,*b;      /* towards zero */
{ double integral_part, modf();
  modf( ((double) (*a)), &integral_part);
  (*b) = ( (REAL) integral_part);
}
void REALround(a,b) REAL *a,*b;      /* towards nearest integer */
{ double integral_part, modf();
  if ((*a) >= 0.0)		/* It may be faster to look at the sign 
				   of mantissa, and dispatch */
    modf( ((double) ((*a)+0.5)), &integral_part); 
  else
    modf( ((double) ((*a)-0.5)), &integral_part);
  (*b) = ( (REAL) integral_part);
}

void REALsquare(a,b) REAL *a,*b;
{ (*b) = ( (REAL) ((*a) * (*a)) );
}
void REALsqrt(a,b) REAL *a,*b;
{
  if ((*a) < 0.0)
    error_bad_range_arg(1);	/* sqrt(negative) */
  (*b) = ( (REAL) sqrt( (double) (*a)) );
}

void REALsin(a,b) REAL *a,*b;
{ (*b) = ( (REAL) sin( (double) (*a)) );
}
void REALcos(a,b) REAL *a,*b;
{ (*b) = ( (REAL) cos( (double) (*a)) );
}
void REALtan(a,b) REAL *a,*b;
{ (*b) = ( (REAL) tan( (double) (*a)) );
}
void REALasin(a,b) REAL *a,*b;
{ (*b) = ( (REAL) asin( (double) (*a)) );
}
void REALacos(a,b) REAL *a,*b;
{ (*b) = ( (REAL) acos( (double) (*a)) );
}
void REALatan(a,b) REAL *a,*b;
{ (*b) = ( (REAL) atan( (double) (*a)) );
}

void REALgamma(a,b) REAL *a,*b;
{ register double y;
  if ((y = gamma(((double) (*a)))) > LN_MAXDOUBLE)
    error_bad_range_arg(1);	/* gamma( non-positive integer ) */
  (*b) = ((REAL) (signgam * exp(y))); /* see HPUX Section 3 */
}
void REALerf(a,b) REAL *a,*b;
{ (*b) = ( (REAL) erf((double) (*a)) );
}
void REALerfc(a,b) REAL *a,*b;
{ (*b) = ( (REAL) erfc((double) (*a)) );
}
void REALbessel1(order,a,b) long order; REAL *a,*b;  /* Bessel of first kind */
{ if (order == 0)
    (*b) = ( (REAL) j0((double) (*a)) );
  if (order == 1)
    (*b) = ( (REAL) j1((double) (*a)) );
  else
    (*b) = ( (REAL) jn(((int) order), ((double) (*a))) );
}
void REALbessel2(order,a,b) long order; REAL *a,*b;  /* Bessel of second kind */
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
   The (1 and 2)s denote the numofargs (1 for unary 2 for binary)
   */

struct array_func_table {
  long numofargs;
  void (*func)();
} Array_Function_Table[] =
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
   but it is specialized to mappings only, so we have special name.
   */
DEFINE_PRIMITIVE ("ARRAY-UNARY-FUNCTION!", Prim_array_unary_function, 2,2, 0)
{ long n, i;
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
  
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = a;
  n = Array_Length(ARG_REF(1));
  
  for (i=0; i<n; i++)
    (*f) ( &(a[i]), &(b[i]) );	/* a into b */
  
  PRIMITIVE_RETURN (NIL);
}


/* Accumulate
   using combinators              +  or  * 
   corresponding type codes       0      1
   */
DEFINE_PRIMITIVE ("SUBARRAY-ACCUMULATE", Prim_subarray_accumulate, 4,4, 0)
{ long at,m,mplus, tc, i;
  REAL *a;
  double result;
  
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);	/*           a = input array                 */
  a  = Scheme_Array_To_C_Array(ARG_REF(1));
  tc = arg_nonnegative_integer(2); /*       tc = type code 0 or 1            */
  at = arg_nonnegative_integer(3); /*       at = starting index              */
  m  = arg_nonnegative_integer(4); /*       m  = number of points to process */
  
  mplus = at + m;
  if (mplus > (Array_Length(ARG_REF(1)))) error_bad_range_arg(4);
  
  if (tc==0)
  { result = 0.0;
    for (i=at;i<mplus;i++) result = result + ((double) a[i]); }
  else if (tc==1)
  { result = 1.0;
    for (i=at;i<mplus;i++) result = result * ((double) a[i]); }
  else
    error_bad_range_arg(2);
  
  Reduced_Flonum_Result(result);
}


/* The following searches for value within tolerance
   starting from index=from in array.
   Returns first index where match occurs.    --  (useful for finding zeros)
   */
DEFINE_PRIMITIVE ("ARRAY-SEARCH-VALUE-TOLERANCE-FROM", Prim_array_search_value_tolerance_from, 4, 4, 0)
{ long Length, from, i;
  REAL *a, value;		/* value to search for */ 
  double tolerance;		/* tolerance allowed */
  int errcode;
  Primitive_4_Args();
  Arg_1_Type(TC_ARRAY);
  a = Scheme_Array_To_C_Array(Arg1);  Length = Array_Length(Arg1);
  
  errcode = Scheme_Number_To_REAL(Arg2, &value);
  if (errcode == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  errcode = Scheme_Number_To_Double(Arg3, &tolerance);
  if (errcode == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(from, Arg4, 0, Length-1, ERR_ARG_4_BAD_RANGE);
  
  i = from;
  while ((tolerance < (fabs(((double) (a[i]-value)))))
	 && (i<Length) )
  { i++; }
  if (tolerance >= (fabs(((double) (a[i]-value)))))
    return Make_Pointer(TC_FIXNUM, i);
  else
    return SHARP_F;
}

DEFINE_PRIMITIVE ("SUBARRAY-MIN-MAX-INDEX", Prim_subarray_min_max_index, 3,3, 0)
{ long at,m,mplus;
  long nmin, nmax;
  Pointer Result, *Orig_Free;
  REAL *a;
  
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  at = arg_nonnegative_integer(2); /*       at = starting index              */
  m  = arg_nonnegative_integer(3); /*       m  = number of points to process */
  
  mplus = at + m;
  if (mplus > (Array_Length(ARG_REF(1)))) error_bad_range_arg(3);
  
  C_Array_Find_Min_Max ( &(a[at]), m, &nmin, &nmax);
  nmin = nmin + at;		/* offset appropriately */
  nmax = nmax + at;
  
  Primitive_GC_If_Needed(4);
  Result = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free+=4;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nmin);
  *Orig_Free++ = Make_Pointer(TC_LIST, Orig_Free+1);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nmax);
  *Orig_Free=EMPTY_LIST;
  
  PRIMITIVE_RETURN (Result); 
}

void C_Array_Find_Min_Max(x, n, nmin, nmax) REAL *x; long n, *nmax, *nmin;
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
   But there is also this C primitive. Keep it around, may be useful someday.
   */

DEFINE_PRIMITIVE ("ARRAY-AVERAGE", Prim_array_find_average, 1, 1, 0)
{ long Length; REAL average;
  void C_Array_Find_Average();
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  
  C_Array_Find_Average( Scheme_Array_To_C_Array(Arg1), Length, &average);
  Reduced_Flonum_Result((double) average);
}

/* Computes the average in pieces, so as to reduce 
   roundoff smearing in cumulative sum.
   example= first huge positive numbers, then small nums, then huge negative numbers.
   */

void C_Array_Find_Average(Array, Length, pAverage)
     long Length; REAL *Array, *pAverage;
{ long i;
  long array_index;
  REAL average_n, sum;
  
  average_n = 0.0;
  array_index = 0;
  while (array_index<Length) {
    sum = 0.0;
    for (i=0;((array_index<Length) && (i<2000));i++) {
      sum += Array[array_index];
      array_index++;
    }
    average_n += (sum / ((REAL) Length));
  }
  *pAverage = average_n;
}

DEFINE_PRIMITIVE ("ARRAY-MAKE-HISTOGRAM", Prim_array_make_histogram, 2, 2, 0)
{ long Length, npoints, allocated_cells; 
  REAL *Array, *Histogram;
  Pointer Result;
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Range_Check(npoints, Arg2, 1, (2*Length), ERR_ARG_2_BAD_RANGE);  
  
  Allocate_Array(Result, npoints, allocated_cells);
  Array = Scheme_Array_To_C_Array(Arg1);
  Histogram = Scheme_Array_To_C_Array(Result);
  C_Array_Make_Histogram(Array, Length, Histogram, npoints);
  return Result;
}
void C_Array_Make_Histogram(Array, Length, Histogram, npoints)
     REAL Array[], Histogram[]; long Length, npoints;
{ REAL Max,Min, Offset, Scale;
  long i, nmin,nmax, index;
  C_Array_Find_Min_Max(Array, Length, &nmin,&nmax);
  Min=Array[nmin]; Max=Array[nmax];
  Find_Offset_Scale_For_Linear_Map(Min,Max, 0.0, ((REAL) npoints), &Offset, &Scale);
  for (i=0;i<npoints;i++) Histogram[i] = 0.0;
  for (i=0;i<Length;i++) {
    /* Everything from 0 to 1 maps to bin 0, and so on */
    index = (long) (floor((double) ((Scale*Array[i]) + Offset)));
    if (index==npoints) index = index-1;  /* max that won't floor to legal array index */
    Histogram[index] += 1.0; }
}

DEFINE_PRIMITIVE ("ARRAY-CLIP-MIN-MAX!", Prim_array_clip_min_max, 3, 3, 0)
{ long Length, i;
  REAL *To_Here, *From_Here, xmin, xmax;
  int errcode;

  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  errcode=Scheme_Number_To_REAL(Arg2, &xmin);
  if (errcode == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  errcode=Scheme_Number_To_REAL(Arg3, &xmax);
  if (errcode == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Length = Array_Length(Arg1);

  From_Here = Scheme_Array_To_C_Array(Arg1);
  To_Here   = Scheme_Array_To_C_Array(Arg1);
  
  if (xmin>xmax) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  for (i=0; i < Length; i++) {
    if ((*From_Here)<xmin) *To_Here++ = xmin;
    else if ((*From_Here)>xmax) *To_Here++ = xmax;
    else *To_Here++ = *From_Here;
    From_Here++ ; }
  return SHARP_F;
}


/* complex-array-operation-1!
   groups together procedures   that use 1 complex-array    
   and                         store the result in place
   */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1!",
		  Prim_complex_array_operation_1, 3,3, 0)
{ long n, i, opcode;
  REAL *a, *b;
  void complex_array_to_polar(), complex_array_exp(), complex_array_sqrt();
  void complex_array_sin(), complex_array_cos();
  void complex_array_asin(), complex_array_acos();
  
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  
  n = Array_Length(ARG_REF(2));
  if (n != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  
  a  = Scheme_Array_To_C_Array(ARG_REF(2)); /*  real part */
  b  = Scheme_Array_To_C_Array(ARG_REF(3)); /*  imag part */
  
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
  else if (opcode==6)
    complex_array_asin(a,b,n); 
  else if (opcode==7)
    complex_array_acos(a,b,n); 
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

void complex_array_to_polar(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x,y, temp;
  for (i=0; i<n; i++) {
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

void complex_array_exp(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x,y, temp;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    y = (double) b[i];
    if ((temp = exp(x)) == HUGE) error_bad_range_arg(2); /* overflow */
    a[i] = (REAL) (temp*cos(y));
    b[i] = (REAL) (temp*sin(y));
  }
}

void complex_array_sqrt(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x,y, r;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    y = (double) b[i];
    r = sqrt( x*x + y*y);
    a[i] = sqrt((r+x)/2.0);
    if (y>0.0) 
      b[i] =  sqrt((r-x)/2.0);	/* choose principal root */
    else			/* see Abramowitz (p.17 3.7.27) */
      b[i] = -sqrt((r-x)/2.0);
  }
}

void complex_array_sin(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x, ey,fy;
  REAL temp;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    ey = exp((double) b[i]);	/* radius should be small to avoid overflow */
    fy = 1.0/ey;		/* exp(-y) */
    temp = (REAL) (sin(x) * (ey + fy) * 0.5); /* expanded (e(iz)-e(-iz))*(-.5i) formula */
    b[i] = (REAL) (cos(x) * (ey - fy) * 0.5); /* see my notes in Abram.p.71 */
    a[i] = temp;
  }
}

void complex_array_cos(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x, ey,fy;
  REAL temp;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    ey = exp((double) b[i]);	/* radius should be small to avoid overflow */
    fy = 1.0/ey;		/* exp(-y) */
    temp = (REAL) (cos(x) * (ey + fy) * 0.5); /* expanded (e(iz)+e(-iz))*.5 formula */
    b[i] = (REAL) (sin(x) * (fy - ey) * 0.5); /* see my notes in Abram.p.71*/
    a[i] = temp;
  }
}

void complex_array_asin(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x,y, alfa,beta, xp1,xm1;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    y = (double) b[i];
    xp1 = x+1;        xm1 = x-1;
    xp1 = xp1*xp1;    xm1 = xm1*xm1;
    y = y*y;
    x = sqrt(xp1+y);		/* use again as temp var */
    y = sqrt(xm1+y);		/* use again as temp var */
    alfa = (x+y)*0.5;
    beta = (x-y)*0.5;		/* Abramowitz p.81 4.4.37 */
    a[i]   = (REAL) asin(beta);
    b[i]   = (REAL) log(alfa + sqrt(alfa*alfa - 1));
  }
}

void complex_array_acos(a,b,n)
     REAL *a,*b; long n;
{ long i;
  double x,y, alfa,beta, xp1,xm1;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    y = (double) b[i];
    xp1 = x+1;        xm1 = x-1;
    xp1 = xp1*xp1;    xm1 = xm1*xm1;
    y = y*y;
    x = sqrt(xp1+y);		/* use again as temp var */
    y = sqrt(xm1+y);		/* use again as temp var */
    alfa = (x+y)*0.5;
    beta = (x-y)*0.5;		/* Abramowitz p.81 4.4.38 */
    a[i]   = (REAL) acos(beta);
    b[i]   = (REAL) -log(alfa + sqrt(alfa*alfa - 1));
  }
}


/* complex-array-operation-1b!
   groups together procedures   that use 1 complex-array    &  1 number
   and                         store the result in place
   (e.g. invert 1/x)
   */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1B!",
		  Prim_complex_array_operation_1b, 4,4, 0)
{ long n, i, opcode;
  REAL *a, *b, inf;
  void complex_array_invert();
  int errcode;
  
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  errcode = Scheme_Number_To_REAL(ARG_REF(4), &inf); /* User-Provided Infinity */
  if (errcode==1) error_bad_range_arg(4); if (errcode==2) error_wrong_type_arg(4);
  
  n = Array_Length(ARG_REF(2));
  if (n != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  
  a  = Scheme_Array_To_C_Array(ARG_REF(2)); /*  real part */
  b  = Scheme_Array_To_C_Array(ARG_REF(3)); /*  imag part */
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    complex_array_invert(a,b, n, inf);  /* performs 1/x */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

void complex_array_invert(a,b, n, inf)
     REAL *a,*b, inf; long n;
{ long i;
  double x,y, r;
  
  for (i=0; i<n; i++)
  { x = (double) a[i];
    y = (double) b[i];
    r = (x*x + y*y);
    if (r==0.0) {
      a[i] = inf;
      b[i] = inf; }
    else {
      a[i] = (REAL)  x/r;
      b[i] = (REAL) -y/r; }
  }
}



/* complex-array-operation-1a
   groups together procedures   that use 1 complex-array    
   and                 store result in a 3rd real array.
   */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-1A",
		  Prim_complex_array_operation_1a, 4,4, 0)
{ long n, i, opcode;
  REAL *a, *b, *c;
  void complex_array_magnitude(), complex_array_angle();
  
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n      real part         */
  CHECK_ARG (3, ARRAY_P);	/* input array -- n      imag part         */
  CHECK_ARG (4, ARRAY_P);	/* output array -- n                       */
  
  n = Array_Length(ARG_REF(2));
  if (n != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  if (n != Array_Length(ARG_REF(4))) error_bad_range_arg(4);
  
  a  = Scheme_Array_To_C_Array(ARG_REF(2)); /*  real part */
  b  = Scheme_Array_To_C_Array(ARG_REF(3)); /*  imag part */
  c  = Scheme_Array_To_C_Array(ARG_REF(4)); /*  output    */
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    complex_array_magnitude(a,b,c,n); 
  else if (opcode==2)
    complex_array_angle(a,b,c,n); 
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

void complex_array_magnitude(a,b,c,n)
     REAL *a,*b,*c; long n;
{ long i;
  for (i=0; i<n; i++)
    c[i] = (REAL) sqrt( (double) a[i]*a[i] + b[i]*b[i] );
}

void complex_array_angle(a,b,c,n)
     REAL *a,*b,*c; long n;
{ long i;
  for (i=0; i<n; i++) {
    if ((a[i] == 0.0) && (b[i]==0.0))
      c[i] = 0.0;		/* choose angle=0   for point (0,0) */
    else
      c[i] = (REAL) atan2( (double) b[i], (double) a[i]); }
  /*                                imag           real   */
}



DEFINE_PRIMITIVE ("CS-ARRAY-MAGNITUDE!", Prim_cs_array_magnitude, 1, 1, 0)
{ long n, i;
  REAL *a;
  void cs_array_magnitude();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);
  a = Scheme_Array_To_C_Array(ARG_REF(1)); /* input cs-array                      */
  n = Array_Length(ARG_REF(1));	/*            becomes a standard array on return  */  
  
  cs_array_magnitude(a,n);
  PRIMITIVE_RETURN (NIL);
}

/* result is a standard array     (even signal, real data)
 */
void cs_array_magnitude(a,n)
     REAL *a; long n;
{ long i, n2, ni;
  n2 = n/2;			/* integer division truncates down */
  
  a[0]  = (REAL) fabs((double) a[0]); /*   imag=0 */
  
  if (2*n2 == n)		/* even length, n2 is only real */
    a[n2] = (REAL) fabs((double) a[n2]); /*  imag=0 */
  else				
    n2 = n2+1;			/* odd length, make the loop include the n2 index */
  
  for (i=1; i<n2; i++)
  { ni = n-i;
    a[i]   = (REAL)  sqrt( (double) a[i]*a[i] + (double) a[ni]*a[ni] ); 
    a[ni]  = a[i];		/* even signal */
  }
}


/* Rectangular and Polar        

   A cs-array has      even magnitude and odd angle (almost)
   hence
   a polar cs-array  stores    magnitude   in the first  half (real part)
   and                         angle       in the second half (imag part)
   
   except for     a[0] real-only    and a[n2] (n even)    
   The angle of a[0]    is either 0 (pos. sign)  or pi (neg. sign), 
   but there is no place in an n-point cs-array    to store this, so 
   a[0]  and a[n2] when n even       are left unchanged  when going polar.
   as opposed to taking their absolute values, for magnitude.
   
   */

DEFINE_PRIMITIVE ("CS-ARRAY-TO-POLAR!", Prim_cs_array_to_polar, 1,1, 0)
{ long n, i;
  REAL *a;
  void cs_array_to_polar();
  
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);	/* input and output array   -- both cs-arrays */
  a  = Scheme_Array_To_C_Array(ARG_REF(1)); 
  n = Array_Length(ARG_REF(1));

  cs_array_to_polar(a,n);
  PRIMITIVE_RETURN (NIL);
}

void cs_array_to_polar(a,n)
     REAL *a; long n;
{ long i, n2;
  double real, imag;		/* temporary variables */
  n2 = n/2;			/* integer division truncates down */
  
  ;				/* a[0] stores both magnitude and angle 
				   (pos. sign angle=0 , neg. sign angle=pi) */
  
  if (2*n2 == n)		/* even length, n2 is only real */
    ;				/* a[n2] stores sign information like a[0] */
  else				
    n2 = n2+1;			/* odd length, make the loop include the n2 index */
  
  for (i=1; i<n2; i++)
  { real = (double) a[i];
    imag = (double) a[n-i];
    a[i]   = (REAL)  sqrt( real*real + imag*imag );
    if (a[i] == 0.0) 
      a[n-i] = 0.0;
    else
      a[n-i] = (REAL) atan2( imag, real ); }
}

DEFINE_PRIMITIVE ("CS-ARRAY-TO-RECTANGULAR!",
		  Prim_cs_array_to_rectangular, 1,1, 0)
{ long n,n2, i;
  double magn,angl;		/* temporary variables */
  REAL *a;
  
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, ARRAY_P);	/* input and output array   -- both cs-arrays */
  a  = Scheme_Array_To_C_Array(ARG_REF(1)); 
  n = Array_Length(ARG_REF(1));
  n2 = n/2;			/* integer division truncates down */
  
  ;				/* a[0] is okay */
  
  if (2*n2 == n)		/* even length, n2 is real only */
    ;				/* a[n2] is okay */
  else
    n2 = n2+1;			/* odd length, make the loop include the n2 index */
  
  for (i=1; i<n2; i++)
  { magn = (double) a[i];
    angl = (double) a[n-i];
    a[i]   = (REAL)  magn * cos(angl);
    a[n-i] = (REAL)  magn * sin(angl); }
  
  PRIMITIVE_RETURN (NIL);
}


/* Convolution in the Time-Domain  
 */
   
/* In the following macro
   To1 and To2 should be (Length1-1) and (Length2-1) respectively.
   */
#define C_Convolution_Point_Macro(X, Y, To1, To2, N, Result)                                \
{ long Min_of_N_To1=min((N),(To1));                                                         \
  long mi, N_minus_mi;                                                                      \
  REAL Sum=0.0;                                                                             \
  for (mi=max(0,(N)-(To2)), N_minus_mi=(N)-mi; mi <= Min_of_N_To1; mi++, N_minus_mi--)      \
    Sum += (X[mi] * Y[N_minus_mi]);                                                         \
  (Result)=Sum;                                                                             \
}
DEFINE_PRIMITIVE ("CONVOLUTION-POINT", Prim_convolution_point, 3, 3, 0)
{ long Length1, Length2, N;
  REAL *Array1, *Array2;
  REAL C_Result;
  
  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Arg_3_Type(TC_FIXNUM);
  Length1 = Array_Length(Arg1);
  Length2 = Array_Length(Arg2);
  N = Get_Integer(Arg3);
  Array1 = Scheme_Array_To_C_Array(Arg1);
  Array2 = Scheme_Array_To_C_Array(Arg2);
  C_Convolution_Point_Macro(Array1, Array2, Length1-1, Length2-1, N, C_Result);
  Reduced_Flonum_Result(C_Result);
}

DEFINE_PRIMITIVE ("ARRAY-CONVOLUTION-IN-TIME!",
		  Prim_array_convolution_in_time, 3, 3, 0)
{ long n,m,l, n_1,m_1, i;
  REAL *a,*b,*c;
  
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ARRAY_P);	/* input array a -- length n                     */
  CHECK_ARG (2, ARRAY_P);	/* input array b -- length m                     */
  CHECK_ARG (3, ARRAY_P);	/* ouput array c -- length l = (n + m - 1)       */
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  b = Scheme_Array_To_C_Array(ARG_REF(2));
  c = Scheme_Array_To_C_Array(ARG_REF(3));
  
  n = Array_Length(ARG_REF(1));
  m = Array_Length(ARG_REF(2));
  l = n+m-1;			/* resulting length */
  if (l != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  
  n_1 = n-1; m_1 = m-1;
  for (i=0; i<l; i++)
  { C_Convolution_Point_Macro(a, b, n_1, m_1, i, c[i]); }
  
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("ARRAY-MULTIPLY-INTO-SECOND-ONE!",
		  Prim_array_multiply_into_second_one, 2, 2, 0)
{ long Length, i;
  REAL *To_Here;
  REAL *From_Here_1, *From_Here_2;
  Pointer Result;
  
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Result = Arg2;
  
  From_Here_1 = Scheme_Array_To_C_Array(Arg1);
  From_Here_2 = Scheme_Array_To_C_Array(Arg2);
  To_Here = Scheme_Array_To_C_Array(Result);

  for (i=0; i < Length; i++) {
    *To_Here++ = (*From_Here_1) * (*From_Here_2);
    From_Here_1++ ;
    From_Here_2++ ;
  }
  return Result;
}

/* complex-array-operation-2!
   groups together procedures   that use 2 complex-arrays     
   and                 store result in either 1st or 2nd
   */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-2!",
		  Prim_complex_array_operation_2, 5,5, 0)
{ long n, opcode;
  REAL *ax,*ay, *bx,*by;
  void complex_array_multiply_into_second_one();
  
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* ax array -- n      real         */
  CHECK_ARG (3, ARRAY_P);	/* ay array -- n      imag         */
  CHECK_ARG (4, ARRAY_P);	/* bx array -- n      real         */
  CHECK_ARG (5, ARRAY_P);	/* by array -- n      imag         */
  
  n = Array_Length(ARG_REF(2));
  if (n != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  if (n != Array_Length(ARG_REF(4))) error_bad_range_arg(4);
  if (n != Array_Length(ARG_REF(4))) error_bad_range_arg(5);
  
  ax  = Scheme_Array_To_C_Array(ARG_REF(2)); /*  real */
  ay  = Scheme_Array_To_C_Array(ARG_REF(3)); /*  imag */
  bx  = Scheme_Array_To_C_Array(ARG_REF(4)); /*  real */
  by  = Scheme_Array_To_C_Array(ARG_REF(5)); /*  imag */
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    complex_array_multiply_into_second_one(ax,ay,bx,by, n); 
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

void complex_array_multiply_into_second_one(ax,ay,bx,by, n)
     REAL *ax,*ay,*bx,*by; long n;
{ long i;
  REAL temp;
  for (i=0;i<n;i++) {
    temp   = ax[i]*bx[i]  -  ay[i]*by[i]; /*  real part */
    by[i]  = ax[i]*by[i]  +  ay[i]*bx[i]; /*  imag part */
    bx[i]  = temp; }
}


void C_Array_Complex_Multiply_Into_First_One(a,b,c,d, length) /* used in fft.c */
     REAL *a,*b,*c,*d; long length;
{ long i;
  REAL temp;
  for (i=0;i<length;i++) {
    temp = a[i]*c[i] - b[i]*d[i];
    b[i] = a[i]*d[i] + b[i]*c[i];
    a[i] = temp;
  }
}


DEFINE_PRIMITIVE ("ARRAY-DIVIDE-INTO-XXX!",
		  Prim_array_divide_into_xxx, 4,4, 0)
{ long n, i, one_or_two;
  REAL *x,*y,*z, inf;
  int errcode;
  void array_divide_into_z();
  
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, ARRAY_P);
  errcode = Scheme_Number_To_REAL(ARG_REF(3), &inf);
  if (errcode==1) error_bad_range_arg(3); if (errcode==2) error_wrong_type_arg(3); 
  CHECK_ARG (4, FIXNUM_P);
  one_or_two = arg_nonnegative_integer(4); /* where to store result of division */
  
  x = Scheme_Array_To_C_Array(ARG_REF(1));
  y = Scheme_Array_To_C_Array(ARG_REF(2));
  n = Array_Length(ARG_REF(1));
  if (n!=(Array_Length(ARG_REF(2)))) error_bad_range_arg(2);
  
  if (one_or_two == 1)
    array_divide_into_z( x,y, x,  n, inf);
  else if (one_or_two == 2)
    array_divide_into_z( x,y, y,  n, inf);
  else
    error_bad_range_arg(4);
  PRIMITIVE_RETURN (NIL);
}

void array_divide_into_z( x,y, z, n, inf) /* z can either x or y */
     REAL *x,*y,*z, inf;  long n;
{ long i;
  for (i=0; i<n; i++) {
    if (y[i] == 0.0) {
      if (x[i] == 0.0)   z[i] = 1.0;
      else               z[i] = inf  * x[i]; }
    else                 z[i] = x[i] / y[i]; 
  }
}


/* complex-array-operation-2b!
   groups together procedures   that use 2 complex-arrays   & 1 additional real number
   and                 store result in either 1st or 2nd
   (e.g. division)
   */

DEFINE_PRIMITIVE ("COMPLEX-ARRAY-OPERATION-2B!",
		  Prim_complex_array_operation_2b, 6,6, 0)
{ long n, opcode;
  REAL *ax,*ay, *bx,*by,  inf;
  void complex_array_divide_into_z();
  int errcode;
  
  PRIMITIVE_HEADER (6);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* ax array -- n      real         */
  CHECK_ARG (3, ARRAY_P);	/* ay array -- n      imag         */
  CHECK_ARG (4, ARRAY_P);	/* bx array -- n      real         */
  CHECK_ARG (5, ARRAY_P);	/* by array -- n      imag         */
  errcode = Scheme_Number_To_REAL(ARG_REF(6), &inf); /* User-Provided Infinity */
  if (errcode==1) error_bad_range_arg(6); if (errcode==2) error_wrong_type_arg(6);
  
  n = Array_Length(ARG_REF(2));
  if (n != Array_Length(ARG_REF(3))) error_bad_range_arg(3);
  if (n != Array_Length(ARG_REF(4))) error_bad_range_arg(4);
  if (n != Array_Length(ARG_REF(4))) error_bad_range_arg(5);
  
  ax  = Scheme_Array_To_C_Array(ARG_REF(2)); /*  real */
  ay  = Scheme_Array_To_C_Array(ARG_REF(3)); /*  imag */
  bx  = Scheme_Array_To_C_Array(ARG_REF(4)); /*  real */
  by  = Scheme_Array_To_C_Array(ARG_REF(5)); /*  imag */
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    complex_array_divide_into_z(ax,ay,bx,by, ax,ay,  n, inf); /* into-first-one */
  else if (opcode==2)
    complex_array_divide_into_z(ax,ay,bx,by, bx,by,  n, inf); /* into-second-one */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}


void complex_array_divide_into_z(xr,xi, yr,yi, zr,zi, n, inf)   /* z can be either x or y */
     REAL *xr,*xi, *yr,*yi, *zr,*zi, inf;   long n;
{ long i;
  register double temp, radius;
  
  for (i=0; i<n; i++)
  { radius = (double) (yr[i] * yr[i]) + (yi[i] * yi[i]); /* denominator */
    if (radius == 0.0) {
      if (xr[i] == 0.0) zr[i] = 1.0;
      else              zr[i] = inf * xr[i];
      if (xi[i] == 0.0) zi[i] = 1.0;
      else              zi[i] = inf * xi[i]; }
    else {
      temp        =  (double) (xr[i] * yr[i]  +  xi[i] * yi[i]);
      zi[i] = (REAL) (xi[i] * yr[i]  -  xr[i] * yi[i]) / radius;
      zr[i] = (REAL) temp                              / radius; 
    }}
}


DEFINE_PRIMITIVE ("ARRAY-LINEAR-SUPERPOSITION-INTO-SECOND-ONE!",
		  Prim_array_linear_superposition_into_second_one, 4, 4, 0)
{ long n, i;
  REAL *To_Here, Coeff1, Coeff2;
  REAL *From_Here_1, *From_Here_2;
  Pointer Result;
  int errcode;

  Primitive_4_Args();
  errcode = Scheme_Number_To_REAL(Arg1, &Coeff1);
  if (errcode == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Arg_2_Type(TC_ARRAY);
  errcode = Scheme_Number_To_REAL(Arg3, &Coeff2);
  if (errcode == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Arg_4_Type(TC_ARRAY);

  n = Array_Length(Arg2);
  if (n != Array_Length(Arg4)) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  
  Result = Arg4;
  
  From_Here_1 = Scheme_Array_To_C_Array(Arg2);
  From_Here_2 = Scheme_Array_To_C_Array(Arg4);
  To_Here = Scheme_Array_To_C_Array(Result);

  for (i=0; i < n; i++) {
    *To_Here++ = (Coeff1 * (*From_Here_1)) + (Coeff2 * (*From_Here_2));
    From_Here_1++ ;
    From_Here_2++ ;
  }
  return Result;
}

/*  m_pi = 3.14159265358979323846264338327950288419716939937510;
 */

DEFINE_PRIMITIVE ("SAMPLE-PERIODIC-FUNCTION", Prim_sample_periodic_function, 4, 4, 0)
{ long N, i, allocated_cells, Function_Number;
  double Signal_Frequency, Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  Pointer Result, Pfunction_number, Psignal_frequency; 
  Pointer Pfunction_Number;
  int errcode;
  REAL *To_Here;
  double unit_square_wave(), unit_triangle_wave();
  
  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 10, ERR_ARG_1_BAD_RANGE); /* fix this */
  
  errcode = Scheme_Number_To_Double(Arg2, &Signal_Frequency);
  if (errcode == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Signal_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  errcode = Scheme_Number_To_Double(Arg3, &Sampling_Frequency);
  if (errcode == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  
  Range_Check(N, Arg4, 0, ARRAY_MAX_LENGTH, ERR_ARG_4_BAD_RANGE);
  
  Allocate_Array(Result, N, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
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
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  
  return Result; 
}

double hamming(t, length) double t, length;
{ double twopi = 6.28318530717958;
  double pi = twopi/2.;
  double t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0)) return(.08 + .46 * (1 - t_bar));
  else return (0);
}

double unit_square_wave(t) double t;
{ double twopi = 6.28318530717958;
  double fmod(), fabs();
  double pi = twopi/2.;
  double t_bar = ((REAL) fabs(fmod( ((double) t), twopi)));
  if (t_bar < pi)                 return(1);
  else                            return(-1);
}

double unit_triangle_wave(t) double t;
{ double twopi = 6.28318530717958;
  double pi = twopi/2.;
  double pi_half = pi/2.;
  double three_pi_half = pi+pi_half;
  double t_bar = ((double) fabs(fmod( ((double) t), twopi)));
  
  if (t_bar<pi_half)             return(-(t_bar/pi));
  else if (t_bar<pi)             return(t_bar/pi); 
  else if (t_bar<three_pi_half)  return((twopi-t_bar)/pi);
  else                           return (-((twopi-t_bar)/pi));
}


DEFINE_PRIMITIVE ("ARRAY-HANNING!", Prim_array_hanning, 2,2, 0)
{ long n, hanning_power;
  REAL *a;
  void C_Array_Make_Hanning();
  
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);	/* input array -- n */
  CHECK_ARG (2, FIXNUM_P);	/* hanning power */

  a  = Scheme_Array_To_C_Array(ARG_REF(1));
  n = Array_Length(ARG_REF(1));
  hanning_power = arg_nonnegative_integer(2);
  
  C_Array_Make_Hanning( a, n, hanning_power);
  PRIMITIVE_RETURN (NIL);
}
void C_Array_Make_Hanning(f1, length, power)
     REAL f1[]; long length, power;
{ double window_length;
  long i;
  double integer_power(), hanning();
  window_length = ((double) length);
  for (i=0;i<length;i++)
  { f1[i] = ((REAL)
	     hanning(((double) i), window_length));
    f1[i] = (REAL) integer_power(((double) f1[i]), power); }
}
double hanning(t, length) double t, length;
{ double twopi = 6.283185307179586476925287;
  double t_bar;
  t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0))     return(.5 * (1 - t_bar));
  else                           return (0.0);
}
double integer_power(a, n) double a; long n;
{ double b;
  double integer_power();

  if (n<0) exit(-1);
  else if (n==0) return(1.0);
  else if (n==1) return(a);
  else if ((n%2) == 0)
  { b = integer_power(a, n/2);
    return(b*b); }
  else
  { return(a * integer_power(a, (n-1))); }
}

/* array-operation-1!
   groups together procedures   that use 1 array    
   and                         store the result in place
   (e.g. random)
   */

DEFINE_PRIMITIVE ("ARRAY-OPERATION-1!",
		  Prim_array_operation_1, 2,2, 0)
{ long n, opcode;
  REAL *a;
  void array_random();
  
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, ARRAY_P);	/* input array -- n */
  
  n = Array_Length(ARG_REF(2));
  a  = Scheme_Array_To_C_Array(ARG_REF(2));
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    array_random(a,n); 
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

void array_random(a,n)
     REAL *a; long n;
{ long i;
  /* HPUX 3: Rand uses a multiplicative congruential random-number generator
     with period 2^32 that returns successive pseudo-random numbers in the 
     range from 0 to 2^15-1 */
  for (i=0;i<n;i++)
    a[i] = ((REAL) rand()) * (3.0517578125e-5);	/* 3.051xxx = 2^(-15) 
						   makes the range from 0 to 1 */
}


/* The following should go away. 
   superceded by ARRAY-CONS-INTEGERS, ARRAY-UNARY-FUNCTION and array-random
   */
DEFINE_PRIMITIVE ("SAMPLE-APERIODIC-FUNCTION", Prim_sample_aperiodic_function, 3, 3, 0)
{ long N, i, allocated_cells, Function_Number;
  double Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  Pointer Result;
  int errcode;
  REAL *To_Here, twopi_dt;

  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 6, ERR_ARG_1_BAD_RANGE);
  
  errcode = Scheme_Number_To_Double(Arg2, &Sampling_Frequency);
  if (errcode == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (errcode == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);

  Range_Check(N, Arg3, 0, ARRAY_MAX_LENGTH, ERR_ARG_3_BAD_RANGE);

  Allocate_Array(Result, N, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  DT = (twopi * (1 / Sampling_Frequency));
  if      (Function_Number == 0)
    /* HPUX 3: Rand uses a multiplicative congruential random-number generator
       with period 2^32 that returns successive pseudo-random numbers in the 
       range from 0 to 2^15-1 */
    for (i=0; i<N; i++)
      *To_Here++ = 3.0517578125e-5 * ((REAL) rand()); /* 2^(-15) makes range from 0 to 1 */
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
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  
  return Result; 
}

DEFINE_PRIMITIVE ("ARRAY-PERIODIC-DOWNSAMPLE", Prim_array_periodic_downsample, 2, 2, 0)
{ long Length, Pseudo_Length, Sampling_Ratio;
  REAL *Array, *To_Here;
  Pointer Result;
  long allocated_cells, i, array_index;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);

  Sign_Extend(Arg2, Sampling_Ratio); /* Sampling_Ratio = integer ratio of sampling_frequencies */
  Sampling_Ratio = Sampling_Ratio % Length; /* periodicity */
  if (Sampling_Ratio < 1)  Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  Pseudo_Length = Length * Sampling_Ratio;
  for (i=0; i<Pseudo_Length; i += Sampling_Ratio) /* new Array has the same Length by assuming periodicity */
  { array_index = i % Length;
    *To_Here++ = Array[array_index]; }
  return Result;
}

/* Shift is not done in place (no side-effects).
 */
DEFINE_PRIMITIVE ("ARRAY-PERIODIC-SHIFT", Prim_array_periodic_shift, 2, 2, 0)
{ long Length, Shift;
  REAL *Array, *To_Here;
  Pointer Result;
  long allocated_cells, i, array_index;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Sign_Extend(Arg2, Shift);
  Shift = Shift % Length;	/* periodic waveform, same sign as dividend */
  Array = Scheme_Array_To_C_Array(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i++) {	/* new Array has the same Length by assuming periodicity */
    array_index = (i+Shift) % Length;
    if (array_index<0) array_index = Length + array_index; /* wrap around */
    *To_Here++ = Array[array_index]; }
  return Result;
}

/* This is done here because array-map is very slow */
DEFINE_PRIMITIVE ("ARRAY-APERIODIC-DOWNSAMPLE", Prim_array_aperiodic_downsample, 2, 2, 0)
{ long Length, New_Length, Sampling_Ratio;
  REAL *Array, *To_Here;
  Pointer Result;
  long allocated_cells, i, array_index;
  
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Array = Scheme_Array_To_C_Array(Arg1);
  Length = Array_Length(Arg1);
  Range_Check(Sampling_Ratio, Arg2, 1, Length, ERR_ARG_2_BAD_RANGE);
  if (Length < 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  New_Length = 1 + ((Length-1)/Sampling_Ratio);	/* 1 for first one and then the rest --- integer division chops */
  Allocate_Array(Result, New_Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i += Sampling_Ratio)
    *To_Here++ = Array[i];
  return Result;
}
/* ARRAY-APERIODIC-SHIFT can be done in scheme using subarray, and array-append */
/* UPSAMPLING should be done in scheme */

/* END ARRAY PROCESSING */

/*********** CONVERSION BETWEEN ARRAYS,VECTORS ********************/

Pointer Scheme_Array_To_Scheme_Vector(Scheme_Array) Pointer Scheme_Array;
{ REAL *Array;
  long Length;
  Pointer C_Array_To_Scheme_Vector();

  Length = Array_Length(Scheme_Array);
  Array = Scheme_Array_To_C_Array(Scheme_Array);
  return C_Array_To_Scheme_Vector(Array, Length);
}

/* C_ARRAY */

Pointer C_Array_To_Scheme_Array(Array, Length) REAL *Array; long Length;
{ Pointer Result;
  long allocated_cells;
  Allocate_Array(Result, Length, allocated_cells);
  return Result;
}

Pointer C_Array_To_Scheme_Vector(Array, Length) REAL *Array; long Length;
{ Pointer Result;
  Pointer *Now_Free;
  long i;

  Primitive_GC_If_Needed(Length+1 + Length*(FLONUM_SIZE+1));
  Now_Free = (Pointer *) Free;
  Free = Free + Length + 1;  /* INCREMENT BEFORE ALLOCATING FLONUMS ! */

  Result = Make_Pointer(TC_VECTOR, Now_Free);
  *Now_Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, Length);

  for (i=0; i<Length; i++) {
    Store_Reduced_Flonum_Result( Array[i], *Now_Free);
    Now_Free++; 
  }
  return Result;
}


/* SCHEME_VECTOR */

Pointer Scheme_Vector_To_Scheme_Array(Arg1) Pointer Arg1;
{ Pointer Result;
  long Length, allocated_cells;
  REAL *Array;
  
  Length = Vector_Length(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  Array = Scheme_Array_To_C_Array(Result);
  
  Scheme_Vector_To_C_Array(Arg1, Array);
  return Result;
}


void Scheme_Vector_To_C_Array(Scheme_Vector, Array) 
     Pointer Scheme_Vector; REAL *Array;
{ Pointer *From_Here;
  REAL *To_Here;
  long Length, i;
  int errcode;

  From_Here = Nth_Vector_Loc(Scheme_Vector, VECTOR_DATA);
  To_Here = Array;
  Length = Vector_Length(Scheme_Vector);
  for (i=0; i < Length; i++, From_Here++) {
    errcode = Scheme_Number_To_REAL(*From_Here, To_Here);
    if (errcode == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
    if (errcode == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);

    To_Here++;            /* this gets incremented by REAL_SIZE ! */
  }
}

/* END of ARRAY PROCESSING */

/* one more hack for speed */

/* (SOLVE-SYSTEM A B N) 
    Solves the system of equations Ax = b.  A and B are 
    arrays and b is the order of the system.  Returns x.
    From the Fortran procedure in Strang.
*/

DEFINE_PRIMITIVE ("SOLVE-SYSTEM", Prim_gaussian_elimination, 2, 2, 0)
{ REAL *A, *B, *X;
  long Length, allocated_cells;
  Pointer Result;
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length  = Array_Length(Arg2);
  if ((Length*Length) != Array_Length(Arg1)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  A = Scheme_Array_To_C_Array(Arg1);
  B = Scheme_Array_To_C_Array(Arg2);
  Allocate_Array(Result, Length, allocated_cells);
  X = Scheme_Array_To_C_Array(Result);
  C_Array_Copy(B, X, Length);
  C_Gaussian_Elimination(A, X, Length);
  return Result;
}

/*
  C routine side-effects b.
*/
C_Gaussian_Elimination(a, b, n)
REAL *a, *b;
long n;
{ long *pvt;
  REAL p, t;
  long i, j, k, m; 
  Primitive_GC_If_Needed(n);
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
