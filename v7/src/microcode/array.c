/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/array.c,v 9.28 1988/01/10 10:38:52 pas Exp $ */

/* CONTAINS:                                                         */
/* Scheme_Array constructors, and selectors                          */
/* Also procedures for converting between C_Array, and Scheme_Vector */

/* See array.h for definition using NM_VECTOR,                       */
/* and for many useful EXTERN                                        */
/* ARRAY = SEQUENCE OF REALS                                         */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "array.h"
#include <math.h>
#include <values.h>
/* <values.h> contains some math constants */

/* first a useful procedure */

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


/**** SCHEME PRIMITIVES *****/

/*   I think this is not needed, can be done at s-code ...
Define_Primitive(Prim_Array_Predicate, 1, "ARRAY?")
{ Primitive_1_Args();
  if (Type_Code(Arg1)==TC_ARRAY) return TRUE;
  else return NIL;
}
*/

Define_Primitive(Prim_Vector_To_Array, 1, "VECTOR->ARRAY")
{ Pointer Scheme_Vector_To_Scheme_Array();
  Primitive_1_Args();
  Arg_1_Type(TC_VECTOR);
  return Scheme_Vector_To_Scheme_Array(Arg1);
}

Define_Primitive(Prim_Array_To_Vector, 1, "ARRAY->VECTOR")
{ Pointer Scheme_Array_To_Scheme_Vector();
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  return Scheme_Array_To_Scheme_Vector(Arg1);
}

Define_Primitive(Prim_Array_Cons, 2, "ARRAY-CONS")
{ long Length, i, allocated_cells;
  REAL Init_Value, *Next;
  int Error_Number;
  Pointer Result;

  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(Length, Arg1, 0, ARRAY_MAX_LENGTH, ERR_ARG_1_BAD_RANGE);

  Error_Number = Scheme_Number_To_REAL(Arg2, &Init_Value);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  
  Allocate_Array(Result,Length,allocated_cells);
  Next = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i < Length; i++) {
    *Next++ = Init_Value;
  }
  return Result; 
}

Define_Primitive(Prim_Array_Length, 1, "ARRAY-LENGTH")
{ Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  return Make_Pointer(TC_FIXNUM, Array_Length(Arg1));
}

Define_Primitive(Prim_Array_Ref, 2, "ARRAY-REF")
{ long Index;
  REAL *Array, value;
  Pointer *Result;
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Index, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Array = Scheme_Array_To_C_Array(Arg1);
  value = Array[Index];
  Reduced_Flonum_Result((double) value);
}

Define_Primitive(Prim_Array_Set, 3, "ARRAY-SET!")
{ long Index;
  REAL *Array, Old_Value;
  int Error_Number;

  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(Index, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Array = Scheme_Array_To_C_Array(Arg1);
  Old_Value = Array[Index];

  Error_Number = Scheme_Number_To_REAL(Arg3, &Array[Index]);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);

  Reduced_Flonum_Result((double) Old_Value);
}

Define_Primitive(Prim_Array_Copy, 1, "ARRAY-COPY")
{ long Length, i, allocated_cells;
  REAL *To_Array, *From_Array;
  SCHEME_ARRAY Result;
  
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);

  Allocate_Array(Result, Length, allocated_cells);
  Array_Copy(Arg1, Result);
  return Result;
}

Define_Primitive(Prim_Array_Read_Ascii_File, 2, "ARRAY-READ-ASCII-FILE")
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
    if ( (fscanf(fp, "%lf", &(a[i]))) != 1)
    { printf("Not enough values read ---\n Last Point was %d with value % .16e \n", i, a[i-1]);
      return NIL; }}
  Close_File(fp);
}

Define_Primitive(Prim_Array_Write_Ascii_File, 2, "ARRAY-WRITE-ASCII-FILE")
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
  return NIL;
}
C_Array_Write_Ascii_File(a,N,fp)           /* 16 ascii decimal digits */
     REAL *a; long N; FILE *fp;
{ long i;
  for (i=0; i<N; i++) {
    if (feof(fp)!=0) { printf("Not enough values written ---\n Last Point was %d with value %---\n", (i-1), a[i-1]);
		       return NIL; }
    fprintf(fp, "% .16e \n", a[i]); }
  Close_File(fp);
}

Define_Primitive(Prim_SubArray, 3, "SUBARRAY")
{ long Length, i, allocated_cells, Start, End, New_Length;
  REAL *To_Here, *From_Here;
  Pointer Result;

  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Range_Check(Start, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Range_Check(End,   Arg3, 0, Array_Length(Arg1)-1, ERR_ARG_3_BAD_RANGE);
  if (Start>End) Primitive_Error(ERR_ARG_3_BAD_RANGE);

  New_Length = (End - Start) + 1;
  Allocate_Array(Result, New_Length, allocated_cells);
  From_Here = Nth_Array_Loc(Arg1, Start);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  C_Array_Copy(From_Here, To_Here, New_Length);
  return Result; 
}

Define_Primitive(Prim_Array_Set_SubArray, 4, "ARRAY-SET-SUBARRAY!")
{ long Length, i, Start, End, New_Length;
  REAL *To_Here, *From_Here;
  Pointer Result;

  Primitive_4_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Arg_4_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Range_Check(Start, Arg2, 0, Array_Length(Arg1)-1, ERR_ARG_2_BAD_RANGE);
  Range_Check(End,   Arg3, 0, Array_Length(Arg1)-1, ERR_ARG_3_BAD_RANGE);
  if (Start>End) Primitive_Error(ERR_ARG_3_BAD_RANGE);

  New_Length = (End - Start) + 1;
  if (New_Length!=Array_Length(Arg4)) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  From_Here = Scheme_Array_To_C_Array(Arg4);
  To_Here = Nth_Array_Loc(Arg1, Start);
  
  C_Array_Copy(From_Here, To_Here, New_Length);
  return Arg1;
}

Define_Primitive(Prim_Array_Append, 2, "ARRAY-APPEND")
{ long Length, Length1, Length2, i, allocated_cells;
  REAL *To_Here, *From_Here;
  Pointer Result;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length1 = Array_Length(Arg1);
  Length2 = Array_Length(Arg2);
  Length = Length1 + Length2;

  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  From_Here = Scheme_Array_To_C_Array(Arg1);

  for (i=0; i < Length1; i++) {
    *To_Here++ = *From_Here;
    From_Here++ ;
  }
  
  From_Here = Scheme_Array_To_C_Array(Arg2);
  for (i=0; i < Length2; i++) {
    *To_Here++ = *From_Here;
    From_Here++ ;
  }
  
  return Result; 
}

Define_Primitive(Prim_Array_Reverse, 1, "ARRAY-REVERSE!")
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

Define_Primitive(Prim_Array_Scale, 2, "ARRAY-SCALE!")
{ long Length, i;
  REAL *To_Here, *From_Here, Scale;
  Pointer Result;
  int Error_Number;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  Error_Number = Scheme_Number_To_REAL(Arg2, &Scale);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  Result = Arg1;
  From_Here = Scheme_Array_To_C_Array(Arg1);
  To_Here = Scheme_Array_To_C_Array(Result);

  for (i=0; i < Length; i++) {
    *To_Here++ = (Scale * (*From_Here));
    From_Here++ ;
  }
  return Result; 
}

/* The following functions are used in the primitive "ARRAY-UNARY-FUNCTION!"
   for tranforming arrays
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
{ if ((*a) < 0.0)
    Primitive_Error(ERR_ARG_1_BAD_RANGE); /* log(negative) */
  (*b) = ( (REAL) log( (double) (*a)) );
}

void REALtruncate(a,b) REAL *a,*b;      /* towards zero */
{ double integral_part, modf();
  modf( ((double) (*a)), &integral_part);
  (*b) = ( (REAL) integral_part);
}
void REALround(a,b) REAL *a,*b;      /* towards nearest integer */
{ double integral_part, modf();
  if ((*a) >= 0.0)		/* It may be faster to look at the sign of mantissa and dispatch */
    modf( ((double) ((*a)+0.5)), &integral_part); 
  else
    modf( ((double) ((*a)-0.5)), &integral_part);
  (*b) = ( (REAL) integral_part);
}

void REALsquare(a,b) REAL *a,*b;
{ (*b) = ( (REAL) ((*a) * (*a)) );
}
void REALsqrt(a,b) REAL *a,*b;
{ if ((*a) < 0.0)
    Primitive_Error(ERR_ARG_1_BAD_RANGE); /* sqrt(negative) */
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
    Primitive_Error(ERR_ARG_1_BAD_RANGE); /* gamma( non-positive integer ) */
  (*b) = ((REAL) (signgam * exp(y)));  /* see HPUX Section 3 */
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
{ if ((*a) <= 0.0)
    Primitive_Error(ERR_ARG_1_BAD_RANGE); /* Blows Up */
  if (order == 0)
    (*b) = ( (REAL) y0((double) (*a)) );
  if (order == 1)
    (*b) = ( (REAL) y1((double) (*a)) );
  else
    (*b) = ( (REAL) yn(((int) order), ((double) (*a))) );
}

/* Table to store the available functions for transforming arrays.
   It also stores the corresponding numofargs (whether unary or binary function).
   */

struct array_func_table {
  long numofargs;
  void (*func)();
} Array_Function_Table[] ={
  1, REALabs,
  1, REALexp,
  1, REALlog,
  1, REALtruncate,
  1, REALround,
  1, REALsquare,
  1, REALsqrt,
  1, REALsin,
  1, REALcos,
  1, REALtan,
  1, REALasin,
  1, REALacos,
  1, REALatan,
  1, REALgamma,
  1, REALerf,
  1, REALerfc,
  2, REALbessel1,
  2, REALbessel2
  };

#define MAX_ARRAY_FUNCTC 17

Define_Primitive(Prim_Array_Unary_Function, 2, "ARRAY-UNARY-FUNCTION!")
{ long Length, i, allocated_cells;
  REAL *a,*b;
  SCHEME_ARRAY Result;
  long functc;
  void (*f)();
  
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Range_Check(functc, Arg2, 0, MAX_ARRAY_FUNCTC, ERR_ARG_2_BAD_RANGE);
  f = ((Array_Function_Table[functc]).func);
  if (1 != (Array_Function_Table[functc]).numofargs) /* check unary */
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  Result = Arg1;
  a = Scheme_Array_To_C_Array(Arg1);
  b = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i++)
    (*f) ( &(a[i]), &(b[i]) );	/* a to b */
  return Result; 
}

Define_Primitive(Prim_Array_Min_Max, 1, "ARRAY-MIN-MAX")
{ long Length, nmin, nmax;
  Pointer Result, *Orig_Free;
  REAL *Array;

  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Array= Scheme_Array_To_C_Array(Arg1);
  Length = Array_Length(Arg1);
  C_Array_Find_Min_Max(Array, Length, &nmin, &nmax);
  Primitive_GC_If_Needed(4);
  Result = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free+=4;
  My_Store_Reduced_Flonum_Result(Array[nmin], *Orig_Free);
  Orig_Free+=1;
  *Orig_Free++ = Make_Pointer(TC_LIST, Orig_Free+1);
  My_Store_Reduced_Flonum_Result(Array[nmax], *Orig_Free);
  *(++Orig_Free)=NIL;
  return Result;
}

Define_Primitive(Prim_Array_Min_Max_Index, 1, "ARRAY-MIN-MAX-INDEX")
{ long Length, nmin, nmax;
  Pointer Result, *Orig_Free;
  REAL *Array;

  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Array= Scheme_Array_To_C_Array(Arg1);
  Length = Array_Length(Arg1);
  C_Array_Find_Min_Max(Array, Length, &nmin, &nmax);
  Primitive_GC_If_Needed(4);
  Result = Make_Pointer(TC_LIST, Free);
  Orig_Free = Free;
  Free+=4;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nmin);
  *Orig_Free++ = Make_Pointer(TC_LIST, Orig_Free+1);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nmax);
  *Orig_Free=NIL;
  return Result; 
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
Define_Primitive(Prim_Array_Find_Average, 1, "ARRAY-AVERAGE")
{ long Length; REAL average;
  Primitive_1_Args();
  Arg_1_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  
  C_Array_Find_Average( Scheme_Array_To_C_Array(Arg1), Length, &average);
  Reduced_Flonum_Result((double) average);
}
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

Define_Primitive(Prim_Array_Make_Histogram, 2, "ARRAY-MAKE-HISTOGRAM")
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

Define_Primitive(Prim_Array_Clip_Min_Max, 3, "ARRAY-CLIP-MIN-MAX!")
{ long Length, i; /* , allocated_cells; */
  REAL *To_Here, *From_Here, xmin, xmax;
  Pointer Result;
  int Error_Number;

  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Error_Number=Scheme_Number_To_REAL(Arg2, &xmin);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  Error_Number=Scheme_Number_To_REAL(Arg3, &xmax);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Length = Array_Length(Arg1);
  Result = Arg1;
  From_Here = Scheme_Array_To_C_Array(Arg1);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  if (xmin>xmax) Primitive_Error(ERR_ARG_3_BAD_RANGE);

  for (i=0; i < Length; i++) {
    if ((*From_Here)<xmin) *To_Here++ = xmin;
    else if ((*From_Here)>xmax) *To_Here++ = xmax;
    else *To_Here++ = *From_Here;
    From_Here++ ; }
  return Result; 
}

Define_Primitive(Prim_Array_Make_Polar, 2, "ARRAY-MAKE-POLAR!")
{ long Length, i;
  REAL *To_Here_Mag, *To_Here_Phase;
  REAL *From_Here_Real, *From_Here_Imag;
  Pointer Result_Mag, Result_Phase, answer;
    
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  
  Result_Mag = Arg1;
  Result_Phase = Arg2;
  
  From_Here_Real = Scheme_Array_To_C_Array(Arg1);
  From_Here_Imag = Scheme_Array_To_C_Array(Arg2);
  To_Here_Mag = Scheme_Array_To_C_Array(Result_Mag);
  To_Here_Phase = Scheme_Array_To_C_Array(Result_Phase);

  for (i=0; i < Length; i++) {
    C_Make_Polar(*From_Here_Real, *From_Here_Imag, *To_Here_Mag, *To_Here_Phase);
    From_Here_Real++ ;
    From_Here_Imag++ ;
    To_Here_Mag++ ; 
    To_Here_Phase++ ;
  }
  
  Primitive_GC_If_Needed(4);
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Result_Mag;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Result_Phase;
  *Free++ = NIL;
  return answer;
}

Define_Primitive(Prim_Array_Find_Magnitude, 2, "ARRAY-FIND-MAGNITUDE")
{ long Length, i, allocated_cells;
  REAL *From_Here_Real, *From_Here_Imag, *To_Here;
  Pointer Result;
  
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_1_BAD_RANGE);

  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  From_Here_Real = Scheme_Array_To_C_Array(Arg1);
  From_Here_Imag = Scheme_Array_To_C_Array(Arg2);
  for (i=0; i<Length; i++) {
    C_Find_Magnitude(*From_Here_Real, *From_Here_Imag, *To_Here);
    From_Here_Real++ ;
    From_Here_Imag++ ;
    To_Here++ ; 
  }
  return Result;
}

/* ATTENTION: To1,To2 SHOULD BE Length1-1, and Length2-2 RESPECTIVELY ! */

#define C_Convolution_Point_Macro(X, Y, To1, To2, N, Result)                                \
{ long Min_of_N_To1=min((N),(To1));                                                         \
  long mi, N_minus_mi;                                                                      \
  REAL Sum=0.0;                                                                             \
  for (mi=max(0,(N)-(To2)), N_minus_mi=(N)-mi; mi <= Min_of_N_To1; mi++, N_minus_mi--)      \
    Sum += (X[mi] * Y[N_minus_mi]);                                                         \
  (Result)=Sum;                                                                             \
}
Define_Primitive(Prim_Convolution_Point, 3, "CONVOLUTION-POINT")
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

Define_Primitive(Prim_Array_Convolution, 2, "ARRAY-CONVOLUTION")
{ long Endpoint1, Endpoint2, allocated_cells, i;
  /* ASSUME A SIGNAL FROM INDEX 0 TO ENDPOINT=LENGTH-1 */
  long Resulting_Length;
  REAL *Array1, *Array2, *To_Here;
  Pointer Result;
  
  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Endpoint1 = Array_Length(Arg1) - 1;
  Endpoint2 = Array_Length(Arg2) - 1;
  Resulting_Length = Endpoint1 + Endpoint2 + 1;
  Array1 = Scheme_Array_To_C_Array(Arg1);
  Array2 = Scheme_Array_To_C_Array(Arg2);

  allocated_cells = (Resulting_Length * REAL_SIZE) + ARRAY_HEADER_SIZE;
  Primitive_GC_If_Needed(allocated_cells);
  Result = Make_Pointer(TC_ARRAY, Free);
  Free[ARRAY_HEADER] = Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  Free[ARRAY_LENGTH] = Resulting_Length;
  Free += allocated_cells;
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Resulting_Length; i++)  {
    C_Convolution_Point_Macro(Array1, Array2, Endpoint1, Endpoint2, i, *To_Here);
    To_Here++;
  }
  return Result;
}

Define_Primitive(Prim_Array_Multiplication_Into_Second_One, 2, "ARRAY-MULTIPLICATION-INTO-SECOND-ONE!")
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

Define_Primitive(Prim_Array_Complex_Multiplication_Into_Second_One, 4, "ARRAY-COMPLEX-MULTIPLICATION-INTO-SECOND-ONE!")
{ long Length, i;
  REAL *To_Here_1, *To_Here_2;
  REAL *From_Here_1, *From_Here_2, *From_Here_3, *From_Here_4;
  REAL Temp;
  Pointer Result_1, Result_2;
  
  Primitive_4_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Arg_3_Type(TC_ARRAY);
  Arg_4_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Length != Array_Length(Arg3)) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Length != Array_Length(Arg4)) Primitive_Error(ERR_ARG_4_BAD_RANGE);

  Result_1 = Arg3;
  Result_2 = Arg4;
  
  From_Here_1 = Scheme_Array_To_C_Array(Arg1);
  From_Here_2 = Scheme_Array_To_C_Array(Arg2);
  From_Here_3 = Scheme_Array_To_C_Array(Arg3);
  From_Here_4 = Scheme_Array_To_C_Array(Arg4);
  To_Here_1 = Scheme_Array_To_C_Array(Result_1);
  To_Here_2 = Scheme_Array_To_C_Array(Result_2);
  
  for (i=0; i < Length; i++) {
    Temp  = (*From_Here_1) * (*From_Here_3) - (*From_Here_2) * (*From_Here_4);
    *To_Here_2++ = (*From_Here_1) * (*From_Here_4) + (*From_Here_2) * (*From_Here_3);
    *To_Here_1++ = Temp;
    From_Here_1++ ;
    From_Here_2++ ;
    From_Here_3++ ;
    From_Here_4++ ;
  }
  return NIL;
}
void C_Array_Complex_Multiply_Into_First_One(a,b,c,d, length)
     REAL *a,*b,*c,*d; long length;
{ long i;
  REAL temp;
  for (i=0;i<length;i++) {
    temp = a[i]*c[i] - b[i]*d[i];
    b[i] = a[i]*d[i] + b[i]*c[i];
    a[i] = temp;
  }
}


Define_Primitive(Prim_Array_Division_Into_First_One, 3, "ARRAY-DIVISION-INTO-FIRST-ONE!")
{ long Length, i;
  SCHEME_ARRAY scheme_result;
  REAL *x,*y,*result;
  REAL infinity;
  int Error_Number;
  
  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &infinity); /* User-Provided Infinity */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  
  scheme_result = Arg1;
  result = Scheme_Array_To_C_Array(scheme_result);
  x = Scheme_Array_To_C_Array(Arg1);
  y = Scheme_Array_To_C_Array(Arg2);
  
  for (i=0; i < Length; i++) {
    if (y[i] == 0.0) {
      if (x[i] == 0.0)		/* zero/zero */
	result[i] = 1.0;
      else
	result[i] = infinity * x[i];
    }
    else
      result[i] = x[i] / y[i];      
  }
  return scheme_result;
}

Define_Primitive(Prim_Array_Division_Into_Second_One, 3, "ARRAY-DIVISION-INTO-SECOND-ONE!")
{ long Length, i;
  SCHEME_ARRAY scheme_result;
  REAL *x,*y,*result;
  REAL infinity;
  int Error_Number;
  
  Primitive_3_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  Error_Number = Scheme_Number_To_REAL(Arg3, &infinity); /* User-Provided Infinity */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  
  scheme_result = Arg2;
  result = Scheme_Array_To_C_Array(scheme_result);
  x = Scheme_Array_To_C_Array(Arg1);
  y = Scheme_Array_To_C_Array(Arg2);
    
  for (i=0; i < Length; i++) {
    if (y[i] == 0.0) {
      if (x[i] == 0.0)		/* zero/zero */
	result[i] = 1.0;
      else
	result[i] = infinity * x[i];
    }
    else
      result[i] = x[i] / y[i];      
  }
  return scheme_result;
}

Define_Primitive(Prim_Array_Complex_Multiplication_Into_First_One, 5, "ARRAY-COMPLEX-DIVISION-INTO-FIRST-ONE!")
{ long Length, i;
  SCHEME_ARRAY scheme_result_r, scheme_result_i;
  REAL *x_r,*x_i, *y_r,*y_i, *result_r,*result_i;
  register REAL Temp, radius, infinity;
  int Error_Number;
  
  Primitive_5_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_ARRAY);
  Arg_3_Type(TC_ARRAY);
  Arg_4_Type(TC_ARRAY);
  Length = Array_Length(Arg1);
  if (Length != Array_Length(Arg2)) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Length != Array_Length(Arg3)) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Length != Array_Length(Arg4)) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  Error_Number = Scheme_Number_To_REAL(Arg5, &infinity); /* User-Provided Infinity */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  
  scheme_result_r = Arg1;
  scheme_result_i = Arg2;
  result_r = Scheme_Array_To_C_Array(scheme_result_r);
  result_i = Scheme_Array_To_C_Array(scheme_result_i);
  x_r = Scheme_Array_To_C_Array(Arg1);
  x_i = Scheme_Array_To_C_Array(Arg2);
  y_r = Scheme_Array_To_C_Array(Arg3);
  y_i = Scheme_Array_To_C_Array(Arg4);
  
  for (i=0; i < Length; i++) {
    Temp        = (x_r[i] * y_r[i]) + (x_i[i] * y_i[i]);
    radius      = (y_r[i] * y_r[i]) + (y_i[i] * y_i[i]);
    
    if (radius == 0.0) {
      if (x_r[i] == 0.0) result_r[i] = 1.0;
      else result_r[i] = infinity * x_r[i];
      if (x_i[i] == 0.0) result_i[i] = 1.0;
      else result_i[i] = infinity * x_i[i];
    }
    else {
      result_i[i] = ( (x_i[i] * y_r[i]) - (x_r[i] * y_i[i]) ) / radius;
      result_r[i] = Temp / radius;
    }
  }
  return NIL;
}

Define_Primitive(Prim_Array_Linear_Superposition_Into_Second_One, 4, "ARRAY-LINEAR-SUPERPOSITION-INTO-SECOND-ONE!")
{ long Length, i;
  REAL *To_Here, Coeff1, Coeff2;
  REAL *From_Here_1, *From_Here_2;
  Pointer Result;
  int Error_Number;

  Primitive_4_Args();
  Error_Number = Scheme_Number_To_REAL(Arg1, &Coeff1);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Arg_2_Type(TC_ARRAY);
  Error_Number = Scheme_Number_To_REAL(Arg3, &Coeff2);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  Arg_4_Type(TC_ARRAY);

  Length = Array_Length(Arg2);
  if (Length != Array_Length(Arg4)) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  
  Result = Arg4;
  
  From_Here_1 = Scheme_Array_To_C_Array(Arg2);
  From_Here_2 = Scheme_Array_To_C_Array(Arg4);
  To_Here = Scheme_Array_To_C_Array(Result);

  for (i=0; i < Length; i++) {
    *To_Here++ = (Coeff1 * (*From_Here_1)) + (Coeff2 * (*From_Here_2));
    From_Here_1++ ;
    From_Here_2++ ;
  }
  return Result;
}

/*  m_pi = 3.14159265358979323846264338327950288419716939937510;
 */

Define_Primitive(Prim_Sample_Periodic_Function, 4, "SAMPLE-PERIODIC-FUNCTION")
{ long N, i, allocated_cells, Function_Number;
  double Signal_Frequency, Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  Pointer Result, Pfunction_number, Psignal_frequency; 
  Pointer Pfunction_Number;
  int Error_Number;
  REAL *To_Here;
  double unit_square_wave(), unit_triangle_wave();
  
  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 10, ERR_ARG_1_BAD_RANGE); /* fix this */
  
  Error_Number = Scheme_Number_To_Double(Arg2, &Signal_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Signal_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Error_Number = Scheme_Number_To_Double(Arg3, &Sampling_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
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

Define_Primitive(Prim_Array_Hanning, 2, "ARRAY-HANNING")
{ long length, hanning_power, allocated_cells;
  SCHEME_ARRAY answer; 
  void C_Array_Make_Hanning();
  Primitive_2_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_2_Type(TC_FIXNUM);
  length = Get_Integer(Arg1);
  hanning_power = Get_Integer(Arg2);
  
  Allocate_Array(answer, length, allocated_cells);
  C_Array_Make_Hanning( (Scheme_Array_To_C_Array(answer)), length, hanning_power);
  return answer;
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

Define_Primitive(Prim_Sample_Aperiodic_Function, 3, "SAMPLE-APERIODIC-FUNCTION")
{ long N, i, allocated_cells, Function_Number;
  double Sampling_Frequency, DT, DTi;
  double twopi = 6.28318530717958;
  Pointer Result;
  int Error_Number;
  REAL *To_Here, twopi_dt;

  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 6, ERR_ARG_1_BAD_RANGE);
  
  Error_Number = Scheme_Number_To_Double(Arg2, &Sampling_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);

  Range_Check(N, Arg3, 0, ARRAY_MAX_LENGTH, ERR_ARG_3_BAD_RANGE);

  Allocate_Array(Result, N, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  DT = (twopi * (1 / Sampling_Frequency));
  if      (Function_Number == 0)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT) 
      *To_Here++ = (REAL) rand();
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

Define_Primitive(Prim_Array_Periodic_Downsample, 2, "ARRAY-PERIODIC-DOWNSAMPLE")
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
Define_Primitive(Prim_Array_Periodic_Shift, 2, "ARRAY-PERIODIC-SHIFT")
{ long Length, Shift;
  REAL *Array, *To_Here;
  Pointer Result;
  long allocated_cells, i, array_index;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Sign_Extend(Arg2, Shift);
  Shift = Shift % Length;                                  /* periodic waveform, same sign as dividend */
  Array = Scheme_Array_To_C_Array(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i++) {                       /* new Array has the same Length by assuming periodicity */
    array_index = (i+Shift) % Length;
    if (array_index<0) array_index = Length + array_index;                /* wrap around */
    *To_Here++ = Array[array_index];
  }
  
  return Result;
}

/* this should really be done in SCHEME using ARRAY-MAP !
 */
Define_Primitive(Prim_Array_Aperiodic_Downsample, 2, "ARRAY-APERIODIC-DOWNSAMPLE")
{ long Length, New_Length, Sampling_Ratio;
  REAL *Array, *To_Here;
  Pointer Result;
  long allocated_cells, i, array_index;

  Primitive_2_Args();
  Arg_1_Type(TC_ARRAY);
  Arg_2_Type(TC_FIXNUM);
  Length = Array_Length(Arg1);
  Range_Check(Sampling_Ratio, Arg2, 1, Length, ERR_ARG_2_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Arg1);
  New_Length = Length / Sampling_Ratio;          /* greater than zero */
  Allocate_Array(Result, New_Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i += Sampling_Ratio) {
    *To_Here++ = Array[i];
  }
  
  return Result;
}
/* ARRAY-APERIODIC-SHIFT can be done in scheme using subarray, and array-append
 */

/* for UPSAMPLING
   if ((Length % Sampling_Ratio) != 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
   UNIMPLEMENTED YET !!! 
   */

/* END ARRAY PROCESSING
 */

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
    My_Store_Reduced_Flonum_Result( Array[i], *Now_Free);
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
  int Error_Number;

  From_Here = Nth_Vector_Loc(Scheme_Vector, VECTOR_DATA);
  To_Here = Array;
  Length = Vector_Length(Scheme_Vector);
  for (i=0; i < Length; i++, From_Here++) {
    Error_Number = Scheme_Number_To_REAL(*From_Here, To_Here);
    if (Error_Number == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
    if (Error_Number == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);

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

Define_Primitive(Prim_Gaussian_Elimination, 2, "SOLVE-SYSTEM")
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
