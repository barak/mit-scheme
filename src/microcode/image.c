/* -*-C-*-

$Id: image.c,v 9.37 2004/11/21 04:17:59 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#include "scheme.h"
#include "prims.h"
#include "array.h"
#include <math.h>

void
arg_image (arg_number, nrows, ncols, array)
     int arg_number;
     long * nrows;
     long * ncols;
     REAL ** array;
{
  fast SCHEME_OBJECT argument = (ARG_REF (arg_number));
  fast SCHEME_OBJECT rest;
  fast SCHEME_OBJECT first;
  fast SCHEME_OBJECT second;
  fast SCHEME_OBJECT third;
  if (! (PAIR_P (argument))) goto loser;
  first = (PAIR_CAR (argument));
  if (! (UNSIGNED_FIXNUM_P (first))) goto loser;
  rest = (PAIR_CDR (argument));
  if (! (PAIR_P (rest))) goto loser;
  second = (PAIR_CAR (rest));
  if (! (UNSIGNED_FIXNUM_P (second))) goto loser;
  rest = (PAIR_CDR (rest));
  if (! (PAIR_P (rest))) goto loser;
  third = (PAIR_CAR (rest));
  if (! (ARRAY_P (third))) goto loser;
  if (!EMPTY_LIST_P (PAIR_CDR (rest))) goto loser;
  (*nrows) = (UNSIGNED_FIXNUM_TO_LONG (first));
  (*ncols) = (UNSIGNED_FIXNUM_TO_LONG (second));
  (*array) = (ARRAY_CONTENTS (third));
  return;
 loser:
  error_bad_range_arg (arg_number);
  /* NOTREACHED */
}

#define MAKE_IMAGE(nrows, ncols, array)					\
  (cons ((LONG_TO_UNSIGNED_FIXNUM (nrows)),				\
	 (cons ((LONG_TO_UNSIGNED_FIXNUM (ncols)),			\
		(cons ((array), EMPTY_LIST))))))

static int
read_byte (fp)
     fast FILE * fp;
{
  int result = (getc (fp));
  if (ferror (fp))
    error_external_return ();
  return (result);
}

static int
read_word (fp)
     fast FILE * fp;
{
  int result = (getw (fp));
  if (ferror (fp))
    error_external_return ();
  return (result);
}

static void
write_word (fp, datum)
     fast FILE * fp;
     int datum;
{
  if ((putw (datum, fp)) != 0)
    error_external_return ();
  return;
}

static int
read_2bint (fp)
     fast FILE * fp;
{
  int msd = (getc (fp));
  if (ferror (fp))
    error_external_return ();
  {
    int lsd = (getc (fp));
    if (ferror (fp))
      error_external_return ();
    {
      int result = ((msd << 8) | lsd);
      return (((result & (1 << 15)) == 0) ? result : ((-1 << 16) | result));
    }
  }
}

static void
write_2bint (fp, datum)
     fast FILE * fp;
     int datum;
{
  if (((putc (((datum >> 8) & 0xFF), fp)) == EOF) ||
      ((putc ((datum & 0xFF), fp)) == EOF))
    error_external_return ();
  return;
}


DEFINE_PRIMITIVE ("IMAGE-READ-ASCII", Prim_read_image_ascii, 1, 1, 0)
{
  fast FILE * fp;
  long nrows, ncols;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  if ( (fp = fopen((STRING_ARG (1)), "r")) == NULL) 
    error_bad_range_arg (1);
  fscanf (fp, "%d %d \n", (&nrows), (&ncols));
  if ((ferror (fp)) || ((ncols > 512) || (nrows > 512)))
    { printf("read-image-ascii-file: problem with rows,cols \n");
      error_bad_range_arg (1); }
  {
    fast long length = (nrows * ncols);
    SCHEME_OBJECT array = (allocate_array (length));
    fast REAL * scan = (ARRAY_CONTENTS (array));
    while ((length--) > 0)
      { long number;
	fscanf (fp, "%d", (&number));
	if (ferror (fp))
	  error_external_return ();
	(*scan++) = ((REAL) number);
      }
    if ((fclose (fp)) != 0) error_external_return ();
    PRIMITIVE_RETURN (MAKE_IMAGE (nrows, ncols, array));
  }
}

DEFINE_PRIMITIVE ("IMAGE-READ-2BINT", Prim_read_image_2bint, 1, 1, 0)
{
  FILE *fp, *fopen();
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  if ( ( fp = (fopen((STRING_ARG (1)), "r")) ) == NULL) 
    error_bad_range_arg (1);
  {
    int nrows = (read_word (fp));
    int ncols = (read_word (fp));
    fast long length = (nrows * ncols);
    SCHEME_OBJECT array = (allocate_array (length));
    fast REAL * scan = (ARRAY_CONTENTS (array));
    while ((length--) > 0)
      (*scan++) = ((REAL) (read_2bint (fp)));
    if ((fclose (fp)) != 0)
      error_external_return ();
    PRIMITIVE_RETURN (MAKE_IMAGE (nrows, ncols, array));
  }
}

DEFINE_PRIMITIVE ("IMAGE-READ-CTSCAN", Prim_read_image_ctscan, 1, 1, 0)
{
  fast FILE * fp;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  fp = (fopen((STRING_ARG (1)), "r"));
  if (fp == ((FILE *) 0))
    error_bad_range_arg (1);
  Primitive_GC_If_Needed (BYTES_TO_WORDS (512 * (sizeof (int))));
  {
    int nrows = 512;
    int ncols = 512;
    SCHEME_OBJECT array = (allocate_array (nrows * ncols));
    REAL * Array = (ARRAY_CONTENTS (array));
    fast int * Widths = ((int *) Free);
    fast int i;
    /* Discard header */
    for (i = 0; (i < 2048); i += 1)
      (void) read_byte (fp);
    for (i = 0; (i < 512); i += 1)
      (Widths [i]) = (read_2bint (fp));
    for (i = 0; (i < (nrows * ncols)); i += 1)
      (Array [i]) = 0;
    for (i = 0; (i < 512); i += 1)
      {
	fast int array_index = ((i * 512) + (256 - (Widths [i])));
	fast int m;
	for (m = 0; (m < (2 * (Widths [i]))); m += 1)
	  (Array [array_index + m]) = ((REAL) (read_2bint (fp)));
      }
    /* CTSCAN images are upside down */
#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
    Primitive_GC_If_Needed (512 * REAL_SIZE);
    Image_Mirror_Upside_Down (Array, nrows, ncols, ((REAL *) Free));
    if ((fclose (fp)) != 0)
      error_external_return ();
    PRIMITIVE_RETURN (MAKE_IMAGE (nrows, ncols, array));
  }
}


DEFINE_PRIMITIVE ("IMAGE-READ-CBIN", Prim_read_image_cbin, 1, 1, 0)
{
  fast FILE * fp;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  fp = (fopen ((STRING_ARG (1)), "r"));
  if (fp == ((FILE *) 0))
    error_bad_range_arg (1);
  {
    int nrows = (read_word (fp));
    int ncols = (read_word (fp));
    long length = (nrows * ncols);
    SCHEME_OBJECT array = (allocate_array (length));
    fast REAL * scan = (ARRAY_CONTENTS (array));
    while ((length--) > 0)
      (*scan++) = (read_word (fp));
    if ((fclose (fp)) != 0)
      error_external_return ();
    PRIMITIVE_RETURN (MAKE_IMAGE (nrows, ncols, array));
  }
}


Image_Mirror_Upside_Down (Array,nrows,ncols,Temp_Row)
     REAL * Array;
     long nrows;
     long ncols;
     REAL * Temp_Row;
{ int i;
  REAL *M_row, *N_row;
  for (i=0;i<(nrows/2);i++) {
    M_row = Array + (i * ncols);
    N_row = Array + (((nrows-1)-i) * ncols);
    C_Array_Copy(N_row,    Temp_Row, ncols);
    C_Array_Copy(M_row,    N_row,    ncols);
    C_Array_Copy(Temp_Row, M_row,    ncols);
  }
}

/* The following does not work, to be fixed. */
DEFINE_PRIMITIVE ("IMAGE-DOUBLE-TO-FLOAT!", Prim_image_double_to_float, 1, 1, 0)
{
  long Length;
  long i,j;
  long allocated_cells;
  long nrows, ncols;
  SCHEME_OBJECT array;
  double *Array, *From_Here;
  fast double temp_value_cell;
  float *To_Here;
  PRIMITIVE_HEADER (1);
  arg_image (1, (&nrows), (&ncols), (&array));
  Array = ((double *) (ARRAY_CONTENTS (array)));
  From_Here = Array;
  To_Here = ((float *) (Array));
  Length = nrows * ncols;

  for (i=0;i<Length;i++) {
    temp_value_cell = *From_Here;
    From_Here++;
    *To_Here = ((float) temp_value_cell);
    To_Here++;
  }
  /* and now SIDE-EFFECT the ARRAY_HEADER */
  SET_VECTOR_LENGTH (array, ((Length * FLOAT_SIZE) + 1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("SUBIMAGE-COPY!", Prim_subimage_copy, 12, 12, 0)
{
  long r1, c1, r2, c2, at1r, at1c, at2r, at2c, mr, mc;
  REAL * x;
  REAL * y;
  void subimage_copy ();
  PRIMITIVE_HEADER (12);
  CHECK_ARG (3, ARRAY_P);	/* image array 1 = source array */
  CHECK_ARG (6, ARRAY_P);	/* image array 2 = destination array */
  r1 = (arg_nonnegative_integer (1));
  c1 = (arg_nonnegative_integer (2));
  if ((r1 * c1) != (ARRAY_LENGTH (ARG_REF (3))))
    error_bad_range_arg (3);
  x = (ARRAY_CONTENTS (ARG_REF (3)));
  r2 = arg_nonnegative_integer (4);
  c2 = arg_nonnegative_integer (5);
  if ((r2 * c2) != (ARRAY_LENGTH (ARG_REF (6))))
    error_bad_range_arg (6);
  y = (ARRAY_CONTENTS (ARG_REF (6)));
  at1r = (arg_nonnegative_integer (7));
  at1c = (arg_nonnegative_integer (8));
  at2r = (arg_nonnegative_integer (9));
  at2c = (arg_nonnegative_integer (10));
  mr = (arg_nonnegative_integer (11));
  mc = (arg_nonnegative_integer (12));
  if (((at1r + mr) > r1) || ((at1c + mc) > c1))
    error_bad_range_arg (7);
  if (((at2r + mr) > r2) || ((at2c + mc) > c2))
    error_bad_range_arg (9);
  subimage_copy (x, y, r1, c1, r2, c2, at1r, at1c, at2r, at2c, mr, mc);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
subimage_copy (x,y, r1,c1,r2,c2, at1r,at1c,at2r,at2c, mr,mc)
     REAL *x,*y;
     long r1,c1,r2,c2, at1r,at1c,at2r,at2c, mr,mc;
{ long i,j;
  REAL *xrow,*yrow;

  xrow = x + at1r*c1   + at1c;
  yrow = y + at2r*c2   + at2c;	/*  A(i,j)--->Array[i*ncols+j]  */

  for (i=0; i<mr; i++) {
    for (j=0; j<mc; j++)    yrow[j] = xrow[j];
    xrow = xrow + c1;
    yrow = yrow + c2;
  }
}

/* image-operation-2
   groups together procedures     that use 2 image-arrays
   (usually side-effecting the 2nd image, but not necessarily) */

DEFINE_PRIMITIVE ("IMAGE-OPERATION-2!", Prim_image_operation_2, 5,5, 0)
{ long rows, cols, nn, opcode;
  REAL *x,*y;
  void image_laplacian();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, FIXNUM_P);	/* cols */
  CHECK_ARG (4, ARRAY_P);	/* image array 1 */
  CHECK_ARG (5, ARRAY_P);	/* image array 2 */

  x = ARRAY_CONTENTS(ARG_REF(4));
  y = ARRAY_CONTENTS(ARG_REF(5));
  rows = arg_nonnegative_integer(2);
  cols = arg_nonnegative_integer(3);
  nn = rows*cols;
  if (nn != ARRAY_LENGTH(ARG_REF(4)))   error_bad_range_arg(4);
  if (nn != ARRAY_LENGTH(ARG_REF(5)))   error_bad_range_arg(5);

  opcode = arg_nonnegative_integer(1);

  if (opcode==1)
    image_laplacian(x,y,rows,cols); /* result in y */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Laplacian form [4,-1,-1,-1,-1]/4
   A(i,j) --> Array[i*ncols + j]
   With no knowledge outside boundary,
   assume laplace(edge-point)=0.0 (no wrap-around, no artificial bndry) */
void
image_laplacian (x,y, nrows,ncols)
     REAL *x, *y;
     long nrows, ncols;
{ long i,j, nrows1, ncols1;
  nrows1=nrows-1; ncols1=ncols-1;
  /* no need todo anything for 1-point image */
  if ((nrows<2)||(ncols<2))
    return;
  /* */
  i=0;j=0;           y[i*ncols+j] = 0.0; /* NE corner */
  i=0;j=ncols1;      y[i*ncols+j] = 0.0; /* NW corner */
  i=nrows1;j=0;      y[i*ncols+j] = 0.0; /* SE corner */
  i=nrows1;j=ncols1; y[i*ncols+j] = 0.0; /* SW corner */
  i=0; for (j=1;j<ncols1;j++)       y[i*ncols+j] = 0.0;	/* NORTH row */
  i=nrows1; for (j=1;j<ncols1;j++)  y[i*ncols+j] = 0.0;	/* SOUTH row */
  j=0; for (i=1;i<nrows1;i++)       y[i*ncols+j] = 0.0;	/* EAST column */
  j=ncols1; for (i=1;i<nrows1;i++)  y[i*ncols+j] = 0.0;	/* WEST column */
  /* */
  for (i=1;i<nrows1;i++)
    for (j=1;j<ncols1;j++)
      y[i*ncols+j] =
	x[i*ncols+j] -
	  ((x[i*ncols+(j-1)] +
	    x[i*ncols+(j+1)] +
	    x[(i-1)*ncols+j] +
	    x[(i+1)*ncols+j])
	   / 4);
  return;
}

DEFINE_PRIMITIVE ("IMAGE-DOUBLE-BY-INTERPOLATION", Prim_image_double_by_interpolation, 1, 1, 0)
{ long nrows, ncols, Length;
  SCHEME_OBJECT Parray;
  REAL *Array, *To_Here;
  SCHEME_OBJECT Result, array;
  PRIMITIVE_HEADER (1);
  arg_image (1, (&nrows), (&ncols), (&Parray));
  Array = (ARRAY_CONTENTS (Parray));
  array = (allocate_array (4 * nrows * ncols));
  Result = (MAKE_IMAGE (nrows, ncols, array));

  Array = ARRAY_CONTENTS(Parray);
  C_image_double_by_interpolation
    (Array, (ARRAY_CONTENTS(array)), nrows, ncols);
  PRIMITIVE_RETURN(Result);
}

/* double image by linear interpolation.
   ---new_array must be 4 times as long ---
   A(i,j) --> Array[i*ncols + j]
   magnification in a south-east direction
   (i.e. replication of pixels in South-East corner) */
C_image_double_by_interpolation (array, new_array, nrows, ncols)
     REAL *array, *new_array;
     long nrows, ncols;
{ long i,j, nrows1, ncols1, nrows2, ncols2;
  nrows1=nrows-1; ncols1=ncols-1;
  nrows2=2*nrows; ncols2=2*ncols;
  /* no need todo anything for 1-point image */
  if ((nrows<2)||(ncols<2)) return(1);
  i=nrows1; for (j=0;j<ncols1;j++) 	/* SOUTH row */
  { new_array[(2*i)*ncols2+(2*j)]      = array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)]    = array[i*ncols+j];
    new_array[(2*i)*ncols2+(2*j)+1] =
      ((array[i*ncols+j]+array[i*ncols+j+1]) / 2);
    new_array[(2*i+1)*ncols2+(2*j)+1]  = new_array[(2*i)*ncols2+(2*j)+1];
  }
  j=ncols1; for (i=0;i<nrows1;i++)  	/* WEST column */
  { new_array[(2*i)*ncols2+(2*j)]      = array[i*ncols+j];
    new_array[(2*i)*ncols2+(2*j)+1]    = array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)] =
      ((array[i*ncols+j]+array[(i+1)*ncols+j]) / 2);
    new_array[(2*i+1)*ncols2+(2*j)+1]  = new_array[(2*i+1)*ncols2+(2*j)];
  }
  i=nrows1;j=ncols1; {                  /* SW corner */
    new_array[(2*i)*ncols2+(2*j)]     =  array[i*ncols+j];
    new_array[(2*i)*ncols2+(2*j)+1]   =  array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)]   =  array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)+1] =  array[i*ncols+j];
  }
  /* */
  for (i=0;i<nrows1;i++)
    for (j=0;j<ncols1;j++) {                        /* interior of image */
      new_array[(2*i)*ncols2+(2*j)] =  array[i*ncols+j];
      new_array[(2*i)*ncols2+(2*j)+1] =
	((array[i*ncols+j]+array[i*ncols+j+1]) / 2);
      new_array[(2*i+1)*ncols2+(2*j)] =
	((array[i*ncols+j]+array[(i+1)*ncols+j]) / 2);
      new_array[(2*i+1)*ncols2+(2*j)+1] =
	((array[i*ncols+j] +
	  array[i*ncols+j+1] +
	  array[(i+1)*ncols+j] +
	  array[(i+1)*ncols+j+1])
	 / 4);
    }
}

DEFINE_PRIMITIVE ("IMAGE-MAKE-RING", Prim_image_make_ring, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    long nrows = (arg_nonnegative_integer (1));
    long ncols = (arg_nonnegative_integer (2));
    long Length = (nrows * ncols);
    long high_cycle =
      (arg_index_integer (4, ((min ((nrows / 2), (ncols / 2))) + 1)));
    long low_cycle = (arg_index_integer (3, (high_cycle + 1)));
    SCHEME_OBJECT Ring_Array_Result = (allocate_array (Length));
    SCHEME_OBJECT Result = (MAKE_IMAGE (nrows, ncols, Ring_Array_Result));
    REAL * Ring_Array = (ARRAY_CONTENTS (Ring_Array_Result));
    C_Image_Make_Ring (Ring_Array, nrows, ncols, low_cycle, high_cycle);
    PRIMITIVE_RETURN (Result);
  }
}

C_Image_Make_Ring (Ring_Array, nrows, ncols, low_cycle, high_cycle)
     REAL *Ring_Array;
     long nrows, ncols, low_cycle, high_cycle;
{ long Square_LC=low_cycle*low_cycle, Square_HC=high_cycle*high_cycle;
  long i, j, m, n, radial_cycle;
  long nrows2=nrows/2, ncols2=ncols/2;
  for (i=0; i<nrows; i++) {
    for (j=0; j<ncols; j++) {
      m = ((i<nrows2) ? i : (nrows-i));
      n = ((j<ncols2) ? j : (ncols-j));
      radial_cycle = (m*m)+(n*n);
      if ( (radial_cycle<Square_LC) || (radial_cycle>Square_HC))
	Ring_Array[i*ncols+j] = 0;
      else Ring_Array[i*ncols+j] = 1;
    }}
}

/* Periodic-shift without side-effects for code simplicity. */

DEFINE_PRIMITIVE ("IMAGE-PERIODIC-SHIFT", Prim_image_periodic_shift, 3, 3, 0)
{
  long nrows;
  long ncols;
  REAL * Array;
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT Parray;
    arg_image (1, (&nrows), (&ncols), (&Parray));
    Array = (ARRAY_CONTENTS (Parray));
  }
  {
    long ver_shift = ((arg_integer (2)) % nrows);
    long hor_shift = ((arg_integer (3)) % ncols);
    SCHEME_OBJECT array = (allocate_array (nrows * ncols));
    SCHEME_OBJECT Result = (MAKE_IMAGE (nrows, ncols, array));
    C_Image_Periodic_Shift
      (Array,
       (ARRAY_CONTENTS (array)),
       nrows,
       ncols,
       ver_shift,
       hor_shift);
    PRIMITIVE_RETURN (Result);
  }
}

/* ASSUMES ((hor_shift < nrows) && (ver_shift < ncols)) */

C_Image_Periodic_Shift(Array, New_Array, nrows, ncols, ver_shift, hor_shift)
     REAL *Array, *New_Array; long nrows, ncols, hor_shift, ver_shift;
{ long i, j, ver_index, hor_index;
  REAL *To_Here;
  To_Here = New_Array;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ver_index = (i+ver_shift) % nrows;
      if (ver_index<0) ver_index = nrows+ver_index; /* wrapping around */
      hor_index = (j+hor_shift) % ncols;
      if (hor_index<0) hor_index = ncols+hor_index;
      *To_Here++ = Array[ver_index*ncols + hor_index];
    }}
}

/* Rotations and stuff */

DEFINE_PRIMITIVE ("IMAGE-TRANSPOSE!", Prim_image_transpose, 4,4, 0)
{ long rows, cols;
  REAL *x, *y;
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* rows */
  CHECK_ARG (2, FIXNUM_P);	/* cols */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2,  empty for rows=cols */
  
  rows = arg_nonnegative_integer(1);
  cols = arg_nonnegative_integer(2);
  x = (ARRAY_CONTENTS (ARG_REF(3)));
  y = (ARRAY_CONTENTS (ARG_REF(4)));
  
  if (rows==cols)		/* square image ==> ignore argument 4  */
    Image_Fast_Transpose (x, rows);
  else
    Image_Transpose (x, y, rows, cols);
  
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("IMAGE-ROTATE-90CLW!", Prim_image_rotate_90clw, 1, 1, 0)
{
  long nrows;
  long ncols;
  REAL * Array;
  long Length;
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT Parray;
    arg_image (1, (&nrows), (&ncols), (&Parray));
    Array = (ARRAY_CONTENTS (Parray));
  }
  Length = (nrows * ncols);
#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
  Primitive_GC_If_Needed (Length * REAL_SIZE);
  {
    REAL * Temp_Array = ((REAL *) Free);
    Image_Rotate_90clw (Array, Temp_Array, nrows, ncols);
    C_Array_Copy (Temp_Array, Array, Length);
  }
  {
    SCHEME_OBJECT argument = (ARG_REF (1));
    SET_PAIR_CAR (argument, (LONG_TO_UNSIGNED_FIXNUM (ncols)));
    SET_PAIR_CAR ((PAIR_CDR (argument)), (LONG_TO_UNSIGNED_FIXNUM (nrows)));
    PRIMITIVE_RETURN (argument);
  }
}

DEFINE_PRIMITIVE ("IMAGE-ROTATE-90CCLW!", Prim_image_rotate_90cclw, 1, 1, 0)
{
  long nrows;
  long ncols;
  REAL * Array;
  long Length;
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT Parray;
    arg_image (1, (&nrows), (&ncols), (&Parray));
    Array = (ARRAY_CONTENTS (Parray));
  }
  Length = (nrows * ncols);
#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
  Primitive_GC_If_Needed (Length * REAL_SIZE);
  {
    REAL * Temp_Array = ((REAL *) Free);
    Image_Rotate_90cclw (Array, Temp_Array, nrows, ncols);
    C_Array_Copy (Temp_Array, Array, Length);
  }
  {
    SCHEME_OBJECT argument = (ARG_REF (1));
    SET_PAIR_CAR (argument, (LONG_TO_UNSIGNED_FIXNUM (ncols)));
    SET_PAIR_CAR ((PAIR_CDR (argument)), (LONG_TO_UNSIGNED_FIXNUM (nrows)));
    PRIMITIVE_RETURN (argument);
  }
}

DEFINE_PRIMITIVE ("IMAGE-MIRROR!", Prim_image_mirror, 1, 1, 0)
{
  long nrows;
  long ncols;
  REAL * Array;
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT Parray;
    arg_image (1, (&nrows), (&ncols), (&Parray));
    Array = (ARRAY_CONTENTS (Parray));
  }
  C_Mirror_Image (Array, nrows, ncols);	/* side-effecting... */
  PRIMITIVE_RETURN (ARG_REF (1));
}


/* C routines   referred to above  */

/* IMAGE_FAST_TRANSPOSE
   A(i,j) <-> A(j,i) .
   UNWRAP: A(i,j) ----> Array[i*ncols + j]
   convention:= fix row & go by columns .
   UNWRAP is a bijection from the compact plane to the compact interval. */

Image_Fast_Transpose (Array, nrows)       /* for square images */
     REAL *Array;
     long nrows;
{ long i, j;
  long from, to;
  REAL temp;
  for (i=0;i<nrows;i++) {
    for (j=i;j<nrows;j++) {
      from = i*nrows + j;
      to   = j*nrows + i;	/* (columns transposed-image) = ncols */
      temp        = Array[from];
      Array[from] = Array[to];
      Array[to]   = temp;
    }}
}

/* IMAGE_TRANSPOSE
   A(i,j) -> B(j,i) .
   UNWRAP: A(i,j) ----> Array[i*ncols + j]
   convention:= fix row & go by columns .
   UNWRAP is a bijection from the compact plane to the compact interval. */

Image_Transpose (Array, New_Array, nrows, ncols)
     REAL *Array, *New_Array;
     long nrows, ncols;
{ long i, j;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      /* (columns transposed-image) = nrows */
      New_Array[j*nrows + i] = Array[i*ncols + j];
    }}
}

/* IMAGE_ROTATE_90CLW
   A(i,j) <-> A(j, (nrows-1)-i) .
   UNWRAP: A(i,j) ----> Array[i*ncols + j]
   convention:= fix row & go by columns
   UNWRAP is a bijection from the compact plane to the compact interval. */

Image_Rotate_90clw (Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array;
     long nrows, ncols;
{ long i, j;

  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      /* (columns rotated_image) =nrows */
      Rotated_Array[(j*nrows) + ((nrows-1)-i)] = Array[i*ncols+j];
    }}
}

/* ROTATION 90degrees COUNTER-CLOCK-WISE:
   A(i,j) <-> A((nrows-1)-j, i) . (minus 1 because we start from 0).
   UNWRAP: A(i,j) ----> Array[i*ncols + j]
   because of convention:= fix row & go by columns
   UNWRAP is a bijection from the compact plane to the compact interval. */

Image_Rotate_90cclw (Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array;
     long nrows, ncols;
{ long i, j;
  fast long from_index, to_index;
  long Length=nrows*ncols;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      from_index = i*ncols +j;
      /* (columns rotated-image) = nrows */
      to_index   = ((ncols-1)-j)*nrows + i;
      Rotated_Array[to_index] = Array[from_index];
    }}
}

/* IMAGE_MIRROR:
   A(i,j) <-> A(i, (ncols-1)-j)  [ The -1 is there because we count from 0] .
   A(i,j) -------> Array[i*ncols + j]    fix row, read column convention. */

C_Mirror_Image (Array, nrows, ncols)
     REAL *Array;
     long nrows, ncols;
{ long i, j;
  long ncols2=ncols/2, Length=nrows*ncols;
  REAL temp;
  long from, to;

  for (i=0; i<Length; i += ncols) {
    for (j=0; j<ncols2; j++) {	/* DO NOT UNDO the reflections */
      from = i + j;                       /* i is really i*nrows */
      to   = i + (ncols-1)-j;
      temp        = Array[from];
      Array[from] = Array[to];
      Array[to]   = temp;
    }}
}

/* IMAGE_ROTATE_90CLW_MIRROR:
   A(i,j) <-> A(j, i)
   this should be identical to image_transpose (see above).
   UNWRAP: A(i,j) ----> Array[i*ncols + j]
   because of convention:= fix row & go by columns
   UNWRAP is a bijection from the compact plane to the compact interval. */

C_Rotate_90clw_Mirror_Image (Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array;
     long nrows, ncols;
{ long i, j;
  long from, to, Length=nrows*ncols;

  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      from = i*ncols +j;
      /* the columns of the rotated image are nrows! */
      to   = j*nrows +i;
      Rotated_Array[to] = Array[from];
    }}
}

DEFINE_PRIMITIVE ("SQUARE-IMAGE-TIME-REVERSE!", Prim_square_image_time_reverse, 2,2, 0)
{ long i, rows;
  REAL *a;
  void square_image_time_reverse();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, FIXNUM_P);
  a = ARRAY_CONTENTS(ARG_REF(1));
  rows = arg_nonnegative_integer(2);
  if ((rows*rows) != ARRAY_LENGTH(ARG_REF(1)))     error_bad_range_arg(1);
  square_image_time_reverse(a,rows);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Square Image Time reverse
   is combination of one-dimensional time-reverse
   row-wise and column-wise.
   It can be done slightly more efficiently than below. */

void
square_image_time_reverse (x,rows)
     REAL *x;
     long rows;
{ long i,cols;
  REAL *xrow, *yrow;
  void C_Array_Time_Reverse();
  cols = rows;			/* square image */

  xrow = x;
  for (i=0; i<rows; i++)	/* row-wise */
  { C_Array_Time_Reverse(xrow,cols);
    xrow = xrow + cols; }

  Image_Fast_Transpose(x, rows);

  xrow = x;
  for (i=0; i<rows; i++)	/* column-wise */
  { C_Array_Time_Reverse(xrow,cols);
    xrow = xrow + cols; }

  Image_Fast_Transpose(x, rows);
}

/* cs-images */

/* operation-1
   groups together procedures     that operate on 1 cs-image-array
   (side-effecting the image) */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-1!", Prim_cs_image_operation_1, 3,3, 0)
{ long rows, opcode;
  REAL *a;
  void cs_image_magnitude(), cs_image_real_part(), cs_image_imag_part();
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* input and output image array */

  a = ARRAY_CONTENTS(ARG_REF(3));
  rows = arg_nonnegative_integer(2); /*          square images only */
  if ((rows*rows) != ARRAY_LENGTH(ARG_REF(3)))   error_bad_range_arg(1);
  opcode = arg_nonnegative_integer(1);

  if (opcode==1)
    cs_image_magnitude(a,rows);
  else if (opcode==2)
    cs_image_real_part(a,rows);
  else if (opcode==3)
    cs_image_imag_part(a,rows);
  else
    error_bad_range_arg(3);	/* illegal opcode */

  PRIMITIVE_RETURN (UNSPECIFIC);
}


void
cs_array_real_part (a,n)
     REAL *a;
     long n;
{ long i,n2;			/* works both for even and odd length */
  n2 = n/2;
  for (i=n2+1;i<n;i++) a[i] = a[n-i]; /* copy real values into place */
  /*                                     even signal */
}

void
cs_array_imag_part (a,n)
     REAL *a;
     long n;
{ long i,n2;
  n2 = n/2;			/* integer division truncates down */
  for (i=n2+1; i<n; i++)	/* works both for even and odd length */
  { a[n-i] = a[i];		/* copy imaginary values into place */
    a[i]   = (-a[i]); }		/* odd signal */
  a[0]     = 0.0;
  if (2*n2 == n)		/* even length, n2 is real only */
    a[n2]    = 0.0;
}

/* From now on (below), assume that cs-images (rows=cols)
   have always EVEN LENGTH which is true when they come from FFTs */

/*  In the following 3   time-reverse the bottom half rows
    is done to match  the frequencies of complex-images
    coming from cft2d.
    Also transpose is needed to match frequencies identically

    #|
    ;; Scrabling of frequencies in  cs-images

    ;; start from real image  4x4

    ;; rft2d    is a cs-image
    (3.5 .375 -2.75 1.875    -.25 0. 0. -.25    -.25 -.125 0. .125
    .25 .25 0. 0.)

    ;; cft2d   transposed
    ;; real
    3.5 .375 -2.75 .375
    -.25  0.  0.  -.25  ; same as cs-image
    -.25 -.125 0. -.125
    -.25 -.25  0.   0.  ; row3 = copy 1 + time-reverse
    ;; imag
    0. 1.875 0. -1.875
    .25 .25 0. 0.       ; same as cs-image
    0. .125 0. -.125
    -.25 0. 0. -.25     ; row 3 = copy 1 + negate + time-reverse
    |#

    */

void
cs_image_magnitude (x,rows)
     REAL *x;
     long rows;
{ long i,j, cols, n,n2, nj; /*     result = real ordinary image */
  REAL *xrow, *yrow;
  cols = rows;			/* input cs-image   is square */
  n = rows;
  n2 = n/2;

  xrow = x;
  cs_array_magnitude(xrow, n);  /* row 0 is cs-array */
  xrow = x + n2*cols;
  cs_array_magnitude(xrow, n);  /* row n2 is cs-array */

  xrow = x + cols;		/* real part */
  yrow = x + (rows-1)*cols;	/* imag part */
  for (i=1; i<n2; i++) {
    xrow[ 0] = (REAL) sqrt((double) xrow[ 0]*xrow[ 0] + yrow[ 0]*yrow[ 0]);
    xrow[n2] = (REAL) sqrt((double) xrow[n2]*xrow[n2] + yrow[n2]*yrow[n2]);
    yrow[ 0] = xrow[ 0];
    yrow[n2] = xrow[n2];
    for (j=1; j<n2; j++) {
      nj = n-j;
      xrow[ j] = (REAL) sqrt((double) xrow[ j]*xrow[ j] + yrow[ j]*yrow[ j]);
      xrow[nj] = (REAL) sqrt((double) xrow[nj]*xrow[nj] + yrow[nj]*yrow[nj]);
      yrow[j]  = xrow[nj];
      yrow[nj] = xrow[ j];      /* Bottom rows: copy (even) and time-reverse */
    }
    xrow = xrow + cols;
    yrow = yrow - cols; }
  Image_Fast_Transpose(x, n);
}

void
cs_image_real_part (x,rows)
     REAL *x;
     long rows;
{ long i,j,cols, n,n2;
  REAL *xrow, *yrow;
  void cs_array_real_part();
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;

  xrow = x;
  cs_array_real_part(xrow, n);  /* row 0 is cs-array */
  xrow = x + n2*cols;
  cs_array_real_part(xrow, n);  /* row n2 is cs-array */

  xrow = x + cols;		/* real part */
  yrow = x + (rows-1)*cols;	/* imag part */
  for (i=1; i<n2; i++) {
    /* copy real part into imaginary's place (even) */
    yrow[0]  = xrow[0];
    for (j=1; j<n; j++)
      yrow[j] = xrow[n-j];	/* Bottom rows:  copy and time-reverse */
    xrow = xrow + cols;
    yrow = yrow - cols; }
  Image_Fast_Transpose(x, n);
}

void
cs_image_imag_part (x,rows)
     REAL *x;
     long rows;
{ long i,j,cols, n,n2, nj;
  REAL *xrow, *yrow;
  void cs_array_imag_part();
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;

  xrow = x;
  cs_array_imag_part(xrow, n);  /* row 0 is cs-array */
  xrow = x + n2*cols;
  cs_array_imag_part(xrow, n);  /* row n2 is cs-array */

  xrow = x + cols;		/* real part */
  yrow = x + (rows-1)*cols;	/* imag part */
  for (i=1; i<n2; i++) {
    xrow[0]  = yrow[0];		/* copy the imaginary part into real's place */
    xrow[n2] = yrow[n2];
    yrow[0]  = (-yrow[0]);      /* negate (odd) */
    yrow[n2] = (-yrow[n2]);
    for (j=1;j<n2; j++) {
      nj = n-j;
      xrow[j]  = yrow[j]; 	/* copy the imaginary part into real's place */
      xrow[nj] = yrow[nj];
      /* Bottom rows: negate (odd) and time-reverse */
      yrow[j]  = (-xrow[nj]);
      yrow[nj] = (-xrow[j]); }
    xrow = xrow + cols;
    yrow = yrow - cols; }
  Image_Fast_Transpose(x, n);
}

/* cs-image-operation-2
   groups together procedures     that use 2 cs-image-arrays
   (usually side-effecting the 2nd image, but not necessarily) */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-2!", Prim_cs_image_operation_2, 4, 4, 0)
{ long rows, nn, opcode;
  REAL *x,*y;
  void cs_image_multiply_into_second_one();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */

  x = ARRAY_CONTENTS(ARG_REF(3));
  y = ARRAY_CONTENTS(ARG_REF(4));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != ARRAY_LENGTH(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != ARRAY_LENGTH(ARG_REF(4)))   error_bad_range_arg(4);

  opcode = arg_nonnegative_integer(1);

  if (opcode==1)
    cs_image_multiply_into_second_one(x,y,rows); /* result in y */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
cs_image_multiply_into_second_one (x,y, rows)
     REAL *x,*y;
     long rows;
{ long i,j,cols, n,n2;
  REAL *xrow,*yrow,  *xrow_r, *xrow_i, *yrow_r, *yrow_i, temp;
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;

  xrow= x; yrow= y;
  cs_array_multiply_into_second_one(xrow,yrow, n,n2); /*         row 0 */

  xrow= x+n2*cols; yrow= y+n2*cols;
  cs_array_multiply_into_second_one(xrow,yrow, n,n2); /*         row n2 */

  xrow_r= x+cols;           yrow_r= y+cols;
  xrow_i= x+(n-1)*cols;     yrow_i= y+(n-1)*cols;
  for (i=1; i<n2; i++) {
    for (j=0; j<n; j++) {
      temp      = xrow_r[j]*yrow_r[j]  -  xrow_i[j]*yrow_i[j]; /* real part */
      yrow_i[j] = xrow_r[j]*yrow_i[j]  +  xrow_i[j]*yrow_r[j]; /* imag part */
      yrow_r[j] = temp; }
    xrow_r= xrow_r+cols;   yrow_r= yrow_r+cols;
    xrow_i= xrow_i-cols;   yrow_i= yrow_i-cols;
  }
}

/* cs-image-operation-2x!     is just like     cs-image-operation-2!
  but takes an additional flonum argument. */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-2x!", Prim_cs_image_operation_2x, 5, 5, 0)
{ long rows, nn, opcode;
  REAL *x,*y, flonum_arg;
  void cs_image_divide_into_z();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */
  flonum_arg = (arg_real (5));

  x = ARRAY_CONTENTS(ARG_REF(3));
  y = ARRAY_CONTENTS(ARG_REF(4));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != ARRAY_LENGTH(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != ARRAY_LENGTH(ARG_REF(4)))   error_bad_range_arg(4);

  opcode = arg_nonnegative_integer(1);

  if (opcode==1)
    cs_image_divide_into_z( x,y, x, rows, flonum_arg); /* result in x */
  else if (opcode==2)
    cs_image_divide_into_z( x,y, y, rows, flonum_arg); /* result in y */
  else
    error_bad_range_arg(1);	/* illegal opcode */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* The convention for inf values in division 1/0 is just like in arrays */

void
cs_image_divide_into_z (x,y, z, rows, inf)
     REAL *x,*y,*z, inf;	/* z can be either x or y */
     long rows;
{ long i,j,cols, n,n2;
  REAL temp, radius;
  REAL  *ar_,*ai_, *br_,*bi_, *zr_,*zi_; /* Letters a,b  correspond to  x,y */
  REAL *xrow,*yrow,*zrow;
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;

  xrow= x; yrow= y; zrow= z;
  cs_array_divide_into_z( xrow,yrow, zrow, n,n2, inf); /*         row 0 */

  xrow= x+n2*cols; yrow= y+n2*cols; zrow= z+n2*cols;
  cs_array_divide_into_z( xrow,yrow, zrow, n,n2, inf); /*         row n2 */

  ar_= x+cols;           br_= y+cols;            zr_= z+cols;
  ai_= x+(n-1)*cols;     bi_= y+(n-1)*cols;      zi_= z+(n-1)*cols;
  for (i=1; i<n2; i++) {
    for (j=0; j<n; j++) {
      /* b^2 denominator = real^2 + imag^2 */
      radius = br_[j]*br_[j]  + bi_[j]*bi_[j];

      if (radius == 0.0) {
	if (ar_[j] == 0.0)  zr_[j]  = 1.0;
	else                zr_[j]  = ar_[j] * inf;
	if (ai_[j] == 0.0)  zi_[j]  = 1.0;
	else                zi_[j]  = ai_[j] * inf; }
      else {
	temp    =  ar_[j]*br_[j]   +  ai_[j]*bi_[j];
	zi_[j]  = (ai_[j]*br_[j]   -  ar_[j]*bi_[j]) / radius; /* imag part */
	zr_[j]  = temp                               / radius; /* real part */
      }}
    ar_= ar_+cols;   br_= br_+cols;    zr_= zr_+cols;
    ai_= ai_-cols;   bi_= bi_-cols;    zi_= zi_-cols;
  }
}

/* operation-3
   groups together procedures     that use 3 cs-image-arrays
   (usually side-effecting the 3rd image, but not necessarily) */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-3!", Prim_cs_image_operation_3, 5, 5, 0)
{ long rows, nn, opcode;
  REAL *x,*y,*z;
  void tr_complex_image_to_cs_image();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */
  CHECK_ARG (5, ARRAY_P);	/* image array 3 */

  x = ARRAY_CONTENTS(ARG_REF(3));
  y = ARRAY_CONTENTS(ARG_REF(4));
  z = ARRAY_CONTENTS(ARG_REF(5));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != ARRAY_LENGTH(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != ARRAY_LENGTH(ARG_REF(4)))   error_bad_range_arg(4);
  if (nn != ARRAY_LENGTH(ARG_REF(5)))   error_bad_range_arg(5);

  opcode = arg_nonnegative_integer(1);

  if (opcode==1)
    tr_complex_image_to_cs_image(x,y, z,rows); /* result in z */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* x and y must be ALREADY TRANSPOSED real and imaginary parts */
void
tr_complex_image_to_cs_image (x,y, z,rows)
     REAL *x,*y,*z;
     long rows;
{ long i,j,cols, n,n2, n2_1_n;
  REAL *xrow, *yrow, *zrow;
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;

  xrow= x; yrow= y; zrow= z;
  for (j=0; j<=n2; j++)
    /* real part of row 0 (cs-array) */
    zrow[j] = xrow[j];
  for (j=n2+1; j<n; j++)
    /* imag part of row 0 */
    zrow[j] = yrow[n-j];
  xrow= x+n2*cols; yrow= y+n2*cols; zrow= z+n2*cols;
  for (j=0; j<=n2; j++)
    /* real part of row n2 (cs-array) */
    zrow[j] = xrow[j];
  for (j=n2+1; j<n; j++)
    /* imag part of row n2 */
    zrow[j] = yrow[n-j];
  xrow= x+cols;   zrow= z+cols;   n2_1_n = (n2-1)*cols;
  for (j=0; j<n2_1_n; j++)
    /* real rows 1,2,..,n2-1 */
    zrow[j] = xrow[j];
  yrow= y+(n2-1)*cols;
  /* imag rows n2+1,n2+2,... */
  zrow= z+(n2+1)*cols;
  for (i=1; i<n2; i++) {
    for (j=0; j<n; j++)   zrow[j] = yrow[j];
    zrow = zrow + cols;
    yrow = yrow - cols;
  }
}
