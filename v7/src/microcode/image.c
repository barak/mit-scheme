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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/image.c,v 9.28 1989/06/22 21:52:26 pas Rel $ */

#include "scheme.h"
#include "prims.h"
#include "flonum.h"
#include "array.h"
#include <math.h>

/* IMAGE PROCESSING...                    */
/* (much comes from array.c)              */

DEFINE_PRIMITIVE ("READ-IMAGE-FROM-ASCII-FILE", Prim_read_image_from_ascii_file, 1, 1, 0)
{ long Length, int_pixel_value1, int_pixel_value2, i, j;
  long nrows, ncols, array_index;
  FILE *fopen(), *fp;
  char *file_string;
  REAL *To_Here;
  REAL *From_Here_1, *From_Here_2;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;
  Boolean Open_File();

  Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  fscanf(fp, "%d %d \n", &nrows, &ncols);
  if ((ncols > 512) || (nrows>512)) {
    printf("read-image-ascii-file: ncols, nrows must be <= 512\n");
    return(NIL);
  }
  Length = nrows * ncols;
  printf("nrows is %d \n", nrows);
  printf("ncols is %d \n", ncols);
  printf("Reading data file ...\n");

  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  
  /*  Allocate_Array(Array_Data_Result, Length, allocated_cells); */
  allocated_cells = (Length*REAL_SIZE) + ARRAY_HEADER_SIZE;
  Primitive_GC_If_Needed(allocated_cells);
  Array_Data_Result = Make_Pointer(TC_ARRAY, Free);
  Free[ARRAY_HEADER] = Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  Free[ARRAY_LENGTH] = Length;
  Free = Free+allocated_cells;

  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */

  To_Here = Scheme_Array_To_C_Array(Array_Data_Result);
  
  for (i=0; i<Length; i++)
  { fscanf( fp, "%d%d", &int_pixel_value1, &int_pixel_value2);
    *To_Here++ = ((REAL) int_pixel_value1);
    *To_Here++ = ((REAL) int_pixel_value2);          /* faster reading ? */
  }
  printf("File read. Length is %d \n", i);
  Close_File(fp);

  return Result;
}

DEFINE_PRIMITIVE ("READ-IMAGE-FROM-CBIN-FILE", Prim_read_image_from_cbin_file, 1, 1, 0)
{ long Length, i,j;
  long nrows, ncols, array_index;
  FILE *fopen(), *fp;
  char *file_string;
  REAL *To_Here;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;
  Boolean Open_File();

  Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (feof(fp)!=0) { printf("Datafile is empty!"); return NIL; }
  nrows = getw(fp);  ncols = getw(fp);
  Length = nrows * ncols;
  
  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, Length, allocated_cells); 
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  
  To_Here = Scheme_Array_To_C_Array(Array_Data_Result);
  
  /* READING IN BIN int FORMAT */
  for (i=0;i<Length;i++) {
    if (feof(fp)!=0) { printf("not enough values read, last read i-1 %d , value %d\n", (i-1), *(To_Here-1));
		       return NIL; }
    *To_Here++ = ((REAL) getw(fp));
  }
  
  Close_File(fp);
  return Result;
}

/* 2BINT FORMAT = integer stored in 2 consecutive bytes.
   We need to use 2bint because on many machines (bobcats included)
   "putw", and "getw" use 4 byte integers (C int) ---> waste lots of space.
   */
DEFINE_PRIMITIVE ("READ-IMAGE-FROM-2BINT-FILE", Prim_read_image_from_2bint_file, 1, 1, 0)
{ long Length, i,j;
  long nrows, ncols, array_index;
  FILE *fopen(), *fp;
  char *file_string;
  REAL *To_Here;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;
  Boolean Open_File();
  int foo1,foo2;

  Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (feof(fp)!=0) { printf("Datafile is empty!"); return NIL; }
  nrows = getw(fp);  ncols = getw(fp);
  Length = nrows * ncols;
  
  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, Length, allocated_cells); 
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  
  To_Here = Scheme_Array_To_C_Array(Array_Data_Result);
  
  for (i=0;i<Length;i++) {
    if (feof(fp)!=0) { printf("not enough values read, last read i-1 %d , value %d\n", (i-1), *(To_Here-1));
		       return NIL; }
    foo1=getc(fp); foo2=getc(fp);        /* Read 2BYTE INT FORMAT */
    *To_Here++ = ((REAL)
		  ((foo1<<8) ^ foo2) );  /* put together the integer */
  }
  
  Close_File(fp);
  return Result;
}

DEFINE_PRIMITIVE ("WRITE-IMAGE-2BINT", Prim_write_image_2bint, 2, 2, 0)
{ long Length, i,j;
  Pointer Pnrows, Pncols, Prest, Parray;
  REAL *Array;
  int nrows, ncols, number,foo1,foo2;
  FILE *fopen(), *fp;
  char *file_string; int Error_Number; Boolean Open_File();
  
  Primitive_2_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Arg_2_Type(TC_CHARACTER_STRING);
  if (!(Open_File(Arg2, "w", &fp))) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  /* */
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);  /* arbitrary size bound on images */
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length = nrows * ncols;
  Array = Scheme_Array_To_C_Array(Parray);
  
  /*_________________________________*/
  putw(nrows, fp);                   /*  putw is 4 bytes on bobcats */
  putw(ncols, fp);                   /*  so below use putc */
  for (i=0;i<Length;i++) {
    number = ((int) Array[i]);
    foo2 = number;     
    foo1 = (number>>8);              /* high order byte */
    putc(foo1, fp);
    putc(foo2, fp);                  /* low order byte */
  }
  Close_File(fp);
  /*_________________________________*/
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("READ-IMAGE-FROM-CTSCAN-FILE", Prim_read_image_from_ctscan_file, 1, 1, 0)
{ long Length, i,j;
  long nrows, ncols, array_index;
  FILE *fopen(), *fp;
  char *file_string;
  REAL *Array;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;
  Boolean Open_File();

  Primitive_1_Args();
  Arg_1_Type(TC_CHARACTER_STRING);
  
  if (!(Open_File(Arg1, "r", &fp))) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (feof(fp)!=0) { printf("Datafile is empty!"); return NIL; }
  nrows = 512;  ncols = 512;
  Length = nrows * ncols;
  
  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, Length, allocated_cells); 
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  
  Array = Scheme_Array_To_C_Array(Array_Data_Result);
  Image_Read_From_CTSCAN_File(fp,Array,nrows,ncols);
  Close_File(fp);
  return Result;
}

Image_Read_From_CTSCAN_File(fp,Array,nrows,ncols)
     FILE *fp; REAL *Array; long nrows,ncols;
{ int i,m;
  long Length=nrows*ncols;
  int first_header_bytes = 2048;
  int second_header_bytes = 3150-(2048+1024);
  int word1, word2;
  long number;
  int *Widths;
  char ignore;
  REAL *Temp_Row;
  int array_index;
  
  Primitive_GC_If_Needed(512); /* INTEGER_SIZE is = 1 scheme pointer */
  Widths = ((int *) Free);
  for (i=0;i<first_header_bytes;i++) ignore = getc(fp); 
  for (i = 0; i<512; i++) {
    word1 = ((int) getc(fp));
    word2 = ((int) getc(fp));
    number = ((word1<<8) | word2);       /* bitwise inclusive or */
    Widths[i] = number;       /* THESE ARE HALF THE NROW-WIDTHs ! */
  }

  for (i=0;i<Length;i++) Array[i] = 0;   /* initialize with zeros */
  
  for (i = 0; i<512; i++) {
    array_index = i*512 + (256-Widths[i]);    /* note the offset */
    for (m=array_index; m<(array_index + 2*Widths[i]); m++) {
      word1 = ((int) getc(fp));    word2 = ((int) getc(fp));
      number = ((word1<<8) | word2);       /* bitwise inclusive or */
      Array[m] = ((REAL) number);  /* do I need to explicitly sign-extend? */
    }
  }
  Primitive_GC_If_Needed(512*REAL_SIZE); 
  Temp_Row = ((REAL *) Free); 
  Image_Mirror_Upside_Down(Array,nrows,ncols,Temp_Row);   /* CTSCAN images are upside down */
}

Image_Mirror_Upside_Down(Array,nrows,ncols,Temp_Row) 
     REAL *Array, *Temp_Row; long nrows,ncols;
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


/* The following does not work, to be fixed.
 */
DEFINE_PRIMITIVE ("IMAGE-DOUBLE-TO-FLOAT!", Prim_image_double_to_float, 1, 1, 0)
{ long Length;
  long i,j;
  long nrows, ncols;
  long allocated_cells;
  double *Array, *From_Here;
  register double temp_value_cell;
  float  *To_Here;
  int Error_Number;
  Pointer Pnrows,Pncols,Parray,Prest;
  
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 2048, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 2048, ERR_ARG_1_BAD_RANGE);

  Array     = ((double *) (Nth_Vector_Loc(Parray, ARRAY_DATA)));
  From_Here = Array;
  To_Here   = ((float *) (Array));
  Length = nrows * ncols;

  for (i=0;i<Length;i++) {
    temp_value_cell = *From_Here;
    From_Here++;
    *To_Here = ((float) temp_value_cell);
    To_Here++;
  }
  /* and now SIDE-EFFECT the ARRAY_HEADER */
  allocated_cells = (Length * 
		     ((sizeof(Pointer)+sizeof(float)-1) / sizeof(Pointer)) +
		     ARRAY_HEADER_SIZE);
  *(Nth_Vector_Loc(Parray, ARRAY_HEADER)) =
    Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  /* see array.h to understand the above */
  
  return Arg1;
}




DEFINE_PRIMITIVE ("SUBIMAGE-COPY!",
		  Prim_subimage_copy, 12,12, 0)
{ long r1,c1, r2,c2, at1r,at1c,at2r,at2c, mr,mc;
  long nn;
  REAL *x,*y;
  void subimage_copy();
  PRIMITIVE_HEADER (12);
  CHECK_ARG (1, FIXNUM_P);	/* rows 1 */
  CHECK_ARG (2, FIXNUM_P);	/* cols 1 */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 =    source array */
  CHECK_ARG (4, FIXNUM_P);	/* rows 2 */
  CHECK_ARG (5, FIXNUM_P);	/* cols 2 */
  CHECK_ARG (6, ARRAY_P);	/* image array 2 =    destination array */
  
  CHECK_ARG (7, FIXNUM_P);	/* at1 row */
  CHECK_ARG (8, FIXNUM_P);	/* at1 col */
  CHECK_ARG (9, FIXNUM_P);	/* at2 row */
  CHECK_ARG (10, FIXNUM_P);	/* at2 col */
  CHECK_ARG (11, FIXNUM_P);	/* m row */
  CHECK_ARG (12, FIXNUM_P);	/* m col */
  
  x = Scheme_Array_To_C_Array(ARG_REF(3));
  y = Scheme_Array_To_C_Array(ARG_REF(6));
  r1 = arg_nonnegative_integer(1);
  c1 = arg_nonnegative_integer(2);
  r2 = arg_nonnegative_integer(4);
  c2 = arg_nonnegative_integer(5);
  
  nn = r1*c1;
  if (nn != Array_Length(ARG_REF(3)))   error_bad_range_arg(3);
  nn = r2*c2;
  if (nn != Array_Length(ARG_REF(6)))   error_bad_range_arg(6);
  
  at1r = arg_nonnegative_integer(7);
  at1c = arg_nonnegative_integer(8);
  at2r = arg_nonnegative_integer(9);
  at2c = arg_nonnegative_integer(10);
  mr = arg_nonnegative_integer(11);
  mc = arg_nonnegative_integer(12);
  
  if (((at1r+mr)>r1) || ((at1c+mc)>c1)) error_bad_range_arg(7);
  if (((at2r+mr)>r2) || ((at2c+mc)>c2)) error_bad_range_arg(9);
  
  subimage_copy(x,y, r1,c1,r2,c2, at1r,at1c,at2r,at2c, mr,mc);
  
  PRIMITIVE_RETURN (NIL);
}

void subimage_copy(x,y, r1,c1,r2,c2, at1r,at1c,at2r,at2c, mr,mc)
     REAL *x,*y; long r1,c1,r2,c2, at1r,at1c,at2r,at2c, mr,mc;
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
   (usually side-effecting the 2nd image, but not necessarily)
   */

DEFINE_PRIMITIVE ("IMAGE-OPERATION-2!",
		  Prim_image_operation_2, 5,5, 0)
{ long rows, cols, nn, opcode;
  REAL *x,*y;
  void image_laplacian();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, FIXNUM_P);	/* cols */
  CHECK_ARG (4, ARRAY_P);	/* image array 1 */
  CHECK_ARG (5, ARRAY_P);	/* image array 2 */
  
  x = Scheme_Array_To_C_Array(ARG_REF(4));
  y = Scheme_Array_To_C_Array(ARG_REF(5));
  rows = arg_nonnegative_integer(2);
  cols = arg_nonnegative_integer(3);
  nn = rows*cols;
  if (nn != Array_Length(ARG_REF(4)))   error_bad_range_arg(4);
  if (nn != Array_Length(ARG_REF(5)))   error_bad_range_arg(5);
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    image_laplacian(x,y,rows,cols); /* result in y */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

/* Laplacian form [4,-1,-1,-1,-1]/4
        A(i,j) --> Array[i*ncols + j] 
	With no knowledge outside boundary, assume laplace(edge-point)=0.0 (no wrap-around, no artificial bndry) 
	*/
void image_laplacian(x,y, nrows,ncols)
     REAL *x, *y;
     long nrows, ncols;
{ long i,j, nrows1, ncols1;
  nrows1=nrows-1; ncols1=ncols-1;
  if ((nrows<2)||(ncols<2)) return; /* no need todo anything for 1-point image */
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
    for (j=1;j<ncols1;j++) y[i*ncols+j] = /* interior of image */
      x[i*ncols+j] - (.25)*(x[i*ncols+(j-1)] + x[i*ncols+(j+1)] + x[(i-1)*ncols+j] + x[(i+1)*ncols+j]); 
}


DEFINE_PRIMITIVE ("IMAGE-DOUBLE-BY-INTERPOLATION", Prim_image_double_by_interpolation, 1, 1, 0)
{ long nrows, ncols, Length;
  Pointer Pnrows, Pncols, Prest, Parray;
  REAL *Array, *To_Here;
  Pointer Result, Array_Data_Result, *Orig_Free;
  long allocated_cells;
  /* */
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  /* */
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length=nrows*ncols;
  /* */
  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, (2*nrows));
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, (2*ncols));
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, (4*Length), allocated_cells);   /* NOTICE (4 * LENGTH) */
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  /* */
  Array = Scheme_Array_To_C_Array(Parray);
  C_image_double_by_interpolation(Array, (Scheme_Array_To_C_Array(Array_Data_Result)), nrows, ncols);
  PRIMITIVE_RETURN(Result);
}

/* double image by linear interpolation.
   ---new_array must be 4 times as long ---
        A(i,j) --> Array[i*ncols + j] 
	magnification in a south-east direction (i.e. replication of pixels in South-East corner)
	*/
C_image_double_by_interpolation(array, new_array, nrows, ncols) 
     REAL *array, *new_array;
     long nrows, ncols;
{ long i,j, nrows1, ncols1, nrows2, ncols2;
  nrows1=nrows-1; ncols1=ncols-1;
  nrows2=2*nrows; ncols2=2*ncols;
  if ((nrows<2)||(ncols<2)) return(1); /* no need todo anything for 1-point image */
  /* */
  i=nrows1; for (j=0;j<ncols1;j++) 	/* SOUTH row */
  { new_array[(2*i)*ncols2+(2*j)]      = array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)]    = array[i*ncols+j];
    new_array[(2*i)*ncols2+(2*j)+1]    = .5*(array[i*ncols+j]+array[i*ncols+j+1]);
    new_array[(2*i+1)*ncols2+(2*j)+1]  = new_array[(2*i)*ncols2+(2*j)+1];
  }
  j=ncols1; for (i=0;i<nrows1;i++)  	/* WEST column */
  { new_array[(2*i)*ncols2+(2*j)]      = array[i*ncols+j];
    new_array[(2*i)*ncols2+(2*j)+1]    = array[i*ncols+j];
    new_array[(2*i+1)*ncols2+(2*j)]    = .5*(array[i*ncols+j]+array[(i+1)*ncols+j]);
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
      new_array[(2*i)*ncols2+(2*j)]     =  array[i*ncols+j];
      new_array[(2*i)*ncols2+(2*j)+1]   =  .5*(array[i*ncols+j]+array[i*ncols+j+1]);
      new_array[(2*i+1)*ncols2+(2*j)]   =  .5*(array[i*ncols+j]+array[(i+1)*ncols+j]);
      new_array[(2*i+1)*ncols2+(2*j)+1] =  .25*(array[i*ncols+j] + array[i*ncols+j+1] + 
						array[(i+1)*ncols+j] + array[(i+1)*ncols+j+1]);
    }
}

DEFINE_PRIMITIVE ("IMAGE-MAKE-RING", Prim_image_make_ring, 4, 4, 0)
{ long Length, i,j;
  long nrows, ncols;
  long Min_Cycle, Max_Cycle;
  long low_cycle, high_cycle;
  REAL *Ring_Array;
  Pointer Result, Ring_Array_Result, *Orig_Free;
  long allocated_cells;
  
  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(nrows, Arg1, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(ncols, Arg2, 0, 1024, ERR_ARG_2_BAD_RANGE);
  Length = nrows*ncols;
  Min_Cycle=0;
  Max_Cycle=min((nrows/2),(ncols/2));
  Arg_3_Type(TC_FIXNUM);      
  Range_Check(low_cycle, Arg3, Min_Cycle, Max_Cycle, ERR_ARG_3_BAD_RANGE);
  Arg_4_Type(TC_FIXNUM);      
  Range_Check(high_cycle, Arg4, Min_Cycle, Max_Cycle, ERR_ARG_4_BAD_RANGE);
  if (high_cycle<low_cycle) Primitive_Error(ERR_ARG_4_BAD_RANGE);
  
  /* Allocate Space */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Ring_Array_Result, Length, allocated_cells); 
  *Orig_Free++ = Ring_Array_Result;
  *Orig_Free = NIL;
  /* end allocation */
  
  Ring_Array = Scheme_Array_To_C_Array(Ring_Array_Result);
  C_Image_Make_Ring(Ring_Array, nrows, ncols, low_cycle, high_cycle);
  return Result;
}

C_Image_Make_Ring(Ring_Array, nrows, ncols, low_cycle, high_cycle) REAL *Ring_Array; 
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

/* Periodic-shift without side-effects for code-simplicity
 */
DEFINE_PRIMITIVE ("IMAGE-PERIODIC-SHIFT", Prim_image_periodic_shift, 3, 3, 0)
{ long Length, i,j;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols;
  long hor_shift, ver_shift;
  REAL *Array, *New_Array;
  Pointer Result, Array_Data_Result, *Orig_Free;
  long allocated_cells;

  Primitive_3_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Arg_2_Type(TC_FIXNUM);      
  Sign_Extend(Arg2, ver_shift);
  ver_shift = ver_shift % nrows;
  Arg_3_Type(TC_FIXNUM);
  Sign_Extend(Arg3, hor_shift);
  hor_shift = hor_shift % ncols;

  /* Allocate SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, Length, allocated_cells); 
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* end allocation */
  
  Array = Scheme_Array_To_C_Array(Parray);
  New_Array = Scheme_Array_To_C_Array(Array_Data_Result);
  C_Image_Periodic_Shift(Array, New_Array, nrows, ncols, ver_shift, hor_shift);
  return Result;
}

/* ASSUMES hor_shift<nrows, ver_shift<ncols
 */
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


/* Rotations and stuff
 */
DEFINE_PRIMITIVE ("IMAGE-TRANSPOSE!", Prim_image_transpose, 1, 1, 0)
{ long Length;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols;
  REAL *Array, *Temp_Array;
  
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Parray);

  if (nrows==ncols) {
    Image_Fast_Transpose(Array, nrows);     /* side-effecting ... */
  }
  else {
    REAL *New_Array;
    long Length=nrows*ncols;
    Primitive_GC_If_Needed(Length*REAL_SIZE);                /* making space in scheme heap */
    New_Array = ((REAL *) Free);
    Image_Transpose(Array, New_Array, nrows, ncols);
    C_Array_Copy(New_Array, Array, Length);
  }
  
  Vector_Set(Arg1, CONS_CAR, Make_Pointer(TC_FIXNUM, ncols) );            /* swithing nrows, ncols */
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Vector_Set(Prest, CONS_CAR, Make_Pointer(TC_FIXNUM, nrows) );
  return Arg1;
}

DEFINE_PRIMITIVE ("IMAGE-ROTATE-90CLW!", Prim_image_rotate_90clw, 1, 1, 0)
{ long Length;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols;
  REAL *Array, *Temp_Array;
  
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Primitive_GC_If_Needed(Length*REAL_SIZE);
  Temp_Array = ((REAL *) Free);
  Array = Scheme_Array_To_C_Array(Parray);
  Image_Rotate_90clw(Array, Temp_Array, nrows, ncols);
  C_Array_Copy(Temp_Array, Array, Length);

  Vector_Set(Arg1, CONS_CAR, Make_Pointer(TC_FIXNUM, ncols) );            /* swithing nrows, ncols */
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Vector_Set(Prest, CONS_CAR, Make_Pointer(TC_FIXNUM, nrows) );
  return Arg1;
}

DEFINE_PRIMITIVE ("IMAGE-ROTATE-90CCLW!", Prim_image_rotate_90cclw, 1, 1, 0)
{ long Length;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols;
  REAL *Array, *Temp_Array;
  
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Primitive_GC_If_Needed(Length*REAL_SIZE);
  Temp_Array = ((REAL *) Free);
  Array = Scheme_Array_To_C_Array(Parray);
  Image_Rotate_90cclw(Array, Temp_Array, nrows, ncols);
  C_Array_Copy(Temp_Array, Array, Length);

  Vector_Set(Arg1, CONS_CAR, Make_Pointer(TC_FIXNUM, ncols) );            /* swithing nrows, ncols */
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Vector_Set(Prest, CONS_CAR, Make_Pointer(TC_FIXNUM, nrows) );
  return Arg1;
}

DEFINE_PRIMITIVE ("IMAGE-MIRROR!", Prim_image_mirror, 1, 1, 0)
{ long Length;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols;
  REAL *Array, *Temp_Array;
  
  Primitive_1_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 1024, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Array = Scheme_Array_To_C_Array(Parray);
  C_Mirror_Image(Array, nrows, ncols);             /* side-effecting... */
  
  return Arg1;
}


/* C routines   referred to above  */

/*
  IMAGE_FAST_TRANSPOSE
  A(i,j) <-> A(j,i) .
  UNWRAP: A(i,j) ----> Array[i*ncols + j]   convention:= fix row & go by columns .
  UNWRAP is a bijection from the compact plane to the compact interval.
  */
Image_Fast_Transpose(Array, nrows)       /* for square images */
     REAL *Array; long nrows;
{ long i, j;
  long from, to;
  REAL temp;
  for (i=0;i<nrows;i++) {
    for (j=i;j<nrows;j++) {
      from = i*nrows + j;
      to   = j*nrows + i;                   /* (columns transposed-image) = ncols */
      temp        = Array[from];
      Array[from] = Array[to];
      Array[to]   = temp;
    }}
}

/*
  IMAGE_TRANSPOSE
  A(i,j) -> B(j,i) .
  UNWRAP: A(i,j) ----> Array[i*ncols + j]   convention:= fix row & go by columns .
  UNWRAP is a bijection from the compact plane to the compact interval.
  */
Image_Transpose(Array, New_Array, nrows, ncols)
     REAL *Array, *New_Array; long nrows, ncols;
{ long i, j;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      New_Array[j*nrows + i] = Array[i*ncols + j];        /* (columns transposed-image) = nrows */
    }}
}

/*
  IMAGE_ROTATE_90CLW 
  A(i,j) <-> A(j, (nrows-1)-i) .
  UNWRAP: A(i,j) ----> Array[i*ncols + j]   convention:= fix row & go by columns 
  UNWRAP is a bijection from the compact plane to the compact interval.
  */
Image_Rotate_90clw(Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array; long nrows, ncols;
{ long i, j;

  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      Rotated_Array[(j*nrows) + ((nrows-1)-i)] = Array[i*ncols+j];    /* (columns rotated_image) =nrows */
    }}
}

/*
  ROTATION 90degrees COUNTER-CLOCK-WISE:
  A(i,j) <-> A((nrows-1)-j, i) . (minus 1 because we start from 0).
  UNWRAP: A(i,j) ----> Array[i*ncols + j]   because of convention:= fix row & go by columns 
  UNWRAP is a bijection from the compact plane to the compact interval.
  */
Image_Rotate_90cclw(Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array; long nrows, ncols;
{ long i, j;
  register long from_index, to_index;
  long Length=nrows*ncols;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      from_index = i*ncols +j;
      to_index   = ((ncols-1)-j)*nrows + i;                 /* (columns rotated-image) = nrows */
      Rotated_Array[to_index] = Array[from_index];
    }}
}

/*
  IMAGE_MIRROR:
  A(i,j) <-> A(i, (ncols-1)-j)  [ The -1 is there because we count from 0] .
  A(i,j) -------> Array[i*ncols + j]    fix row, read column convention.
  */
C_Mirror_Image(Array, nrows, ncols)  REAL *Array; long nrows, ncols;
{ long i, j;
  long ncols2=ncols/2, Length=nrows*ncols;
  REAL temp;
  long from, to;
  
  for (i=0; i<Length; i += ncols) {
    for (j=0; j<ncols2; j++) {                    /* DO NOT UNDO the reflections */
      from = i + j;                       /* i is really i*nrows */
      to   = i + (ncols-1)-j;
      temp        = Array[from];
      Array[from] = Array[to];
      Array[to]   = temp;
    }}
}

/*
  IMAGE_ROTATE_90CLW_MIRROR:
  A(i,j) <-> A(j, i)     this should be identical to image_transpose (see above).
  UNWRAP: A(i,j) ----> Array[i*ncols + j]   because of convention:= fix row & go by columns 
  UNWRAP is a bijection from the compact plane to the compact interval.
  */
C_Rotate_90clw_Mirror_Image(Array, Rotated_Array, nrows, ncols)
     REAL *Array, *Rotated_Array; long nrows, ncols;
{ long i, j;
  long from, to, Length=nrows*ncols;
  
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      from = i*ncols +j;
      to   = j*nrows +i;                 /* the columns of the rotated image are nrows! */
      Rotated_Array[to] = Array[from];
    }}
}


/* More Image Manipulation -----------------------
 */

DEFINE_PRIMITIVE ("SQUARE-IMAGE-TIME-REVERSE!",
		  Prim_square_image_time_reverse, 2,2, 0)
{ long i, rows;
  REAL *a;
  void square_image_time_reverse();
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ARRAY_P);
  CHECK_ARG (2, FIXNUM_P);
  a = Scheme_Array_To_C_Array(ARG_REF(1));
  rows = arg_nonnegative_integer(2);
  if ((rows*rows) != Array_Length(ARG_REF(1)))     error_bad_range_arg(1);
  square_image_time_reverse(a,rows);
  
  PRIMITIVE_RETURN (NIL);
}

/* Square Image Time reverse 
   is combination of one-dimensional time-reverse
   row-wise and column-wise.
   It   can be done slightly more efficiently than below.
   */
void square_image_time_reverse(x,rows)
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



/*      cs-images   
 */

/* operation-1 
   groups together procedures     that operate on 1 cs-image-array 
   (side-effecting the image)
   */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-1!",
		  Prim_cs_image_operation_1, 3,3, 0)
{ long rows, opcode;
  REAL *a;
  void cs_image_magnitude(), cs_image_real_part(), cs_image_imag_part();
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* input and output image array */
  
  a = Scheme_Array_To_C_Array(ARG_REF(3));
  rows = arg_nonnegative_integer(2); /*          square images only */
  if ((rows*rows) != Array_Length(ARG_REF(3)))   error_bad_range_arg(1);
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    cs_image_magnitude(a,rows);
  else if (opcode==2)
    cs_image_real_part(a,rows);
  else if (opcode==3)
    cs_image_imag_part(a,rows);
  else
    error_bad_range_arg(3);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}


void cs_array_real_part(a,n)
     REAL *a; long n;
{ long i,n2;			/* works both for even and odd length */
  n2 = n/2;
  for (i=n2+1;i<n;i++) a[i] = a[n-i]; /* copy real values into place */
  /*                                     even signal */
}

void cs_array_imag_part(a,n)
     REAL *a; long n;
{ long i,n2;
  n2 = n/2;			/* integer division truncates down */
  for (i=n2+1; i<n; i++)	/* works both for even and odd length */
  { a[n-i] = a[i];		/* copy imaginary values into place */
    a[i]   = (-a[i]); }		/* odd signal */
  a[0]     = 0.0;
  if (2*n2 == n)		/* even length, n2 is real only */
    a[n2]    = 0.0;
}



/* From now on (below), assume that cs-images   (rows=cols) have always EVEN LENGTH
   which is true when they come from FFTs
   */



/*  In the following 3   time-reverse the bottom half rows        
    is done to match  the frequencies of complex-images   
    coming from cft2d.                       
    Also transpose is needed to match frequencies identically
    
    #|
    ;; Scrabling of frequencies in  cs-images
    
    ;; start from real image  4x4
    
    ;; rft2d    is a cs-image
    (3.5 .375 -2.75 1.875    -.25 0. 0. -.25    -.25 -.125 0. .125    .25 .25 0. 0.)
    
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

void cs_image_magnitude(x,rows)
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
      yrow[nj] = xrow[ j];      /* Bottom rows:    copy (even) and time-reverse      */
    }
    xrow = xrow + cols;
    yrow = yrow - cols; }
  Image_Fast_Transpose(x, n);
}


void cs_image_real_part(x,rows)
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
    yrow[0]  = xrow[0];		/* copy real part into imaginary's place  (even)    */
    for (j=1; j<n; j++)
      yrow[j] = xrow[n-j];	/* Bottom rows:  copy and time-reverse              */
    xrow = xrow + cols;
    yrow = yrow - cols; }	
  Image_Fast_Transpose(x, n);
}

void cs_image_imag_part(x,rows)
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
    xrow[0]  = yrow[0];		/* copy the imaginary part into real's place       */
    xrow[n2] = yrow[n2];
    yrow[0]  = (-yrow[0]);      /* negate (odd)                                    */
    yrow[n2] = (-yrow[n2]);
    for (j=1;j<n2; j++) {
      nj = n-j;
      xrow[j]  = yrow[j]; 	/* copy the imaginary part into real's place       */
      xrow[nj] = yrow[nj];
      yrow[j]  = (-xrow[nj]);	/* Bottom rows: negate (odd) and time-reverse      */
      yrow[nj] = (-xrow[j]); }
    xrow = xrow + cols;
    yrow = yrow - cols; }
  Image_Fast_Transpose(x, n);
}


/* cs-image-operation-2
   groups together procedures     that use 2 cs-image-arrays 
   (usually side-effecting the 2nd image, but not necessarily)
   */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-2!",
		  Prim_cs_image_operation_2, 4,4, 0)
{ long rows, nn, opcode;
  REAL *x,*y;
  void cs_image_multiply_into_second_one();
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */
  
  x = Scheme_Array_To_C_Array(ARG_REF(3));
  y = Scheme_Array_To_C_Array(ARG_REF(4));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != Array_Length(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != Array_Length(ARG_REF(4)))   error_bad_range_arg(4);
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    cs_image_multiply_into_second_one(x,y,rows); /* result in y */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}


void cs_image_multiply_into_second_one(x,y, rows) 
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

/* 
  cs-image-operation-2x!     is just like     cs-image-operation-2!
  but takes an additional flonum argument.
  */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-2x!",
		  Prim_cs_image_operation_2x, 5,5, 0)
{ long rows, nn, opcode;
  REAL *x,*y, flonum_arg;
  int errcode;
  void cs_image_divide_into_z();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */
  
  errcode = Scheme_Number_To_REAL(ARG_REF(5), &flonum_arg); /*        extra argument */
  if (errcode==1) error_bad_range_arg(5); if (errcode==2) error_wrong_type_arg(5); 
  
  x = Scheme_Array_To_C_Array(ARG_REF(3));
  y = Scheme_Array_To_C_Array(ARG_REF(4));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != Array_Length(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != Array_Length(ARG_REF(4)))   error_bad_range_arg(4);
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    cs_image_divide_into_z( x,y, x, rows, flonum_arg); /* result in x */
  else if (opcode==2)
    cs_image_divide_into_z( x,y, y, rows, flonum_arg); /* result in y */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}


/* The convention for inf values    in division 1/0  
   is just like in arrays 
   */

void cs_image_divide_into_z(x,y, z, rows, inf)    /* z can be either x or y */
     REAL *x,*y,*z, inf;
     long rows;
{ long i,j,cols, n,n2;
  REAL temp, radius;
  REAL  *ar_,*ai_, *br_,*bi_, *zr_,*zi_; /*   Letters a,b  correspond to  x,y  */
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
      radius    = br_[j]*br_[j]  + bi_[j]*bi_[j]; /* b^2 denominator = real^2 + imag^2 */
      
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
   (usually side-effecting the 3rd image, but not necessarily)
   */

DEFINE_PRIMITIVE ("CS-IMAGE-OPERATION-3!",
		  Prim_cs_image_operation_3, 5,5, 0)
{ long rows, nn, opcode;
  REAL *x,*y,*z;
  void tr_complex_image_to_cs_image();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (1, FIXNUM_P);	/* operation opcode */
  CHECK_ARG (2, FIXNUM_P);	/* rows */
  CHECK_ARG (3, ARRAY_P);	/* image array 1 */
  CHECK_ARG (4, ARRAY_P);	/* image array 2 */
  CHECK_ARG (5, ARRAY_P);	/* image array 3 */
  
  x = Scheme_Array_To_C_Array(ARG_REF(3));
  y = Scheme_Array_To_C_Array(ARG_REF(4));
  z = Scheme_Array_To_C_Array(ARG_REF(5));
  rows = arg_nonnegative_integer(2); /*          square images only */
  nn = rows*rows;
  if (nn != Array_Length(ARG_REF(3)))   error_bad_range_arg(3);
  if (nn != Array_Length(ARG_REF(4)))   error_bad_range_arg(4);
  if (nn != Array_Length(ARG_REF(5)))   error_bad_range_arg(5);
  
  opcode = arg_nonnegative_integer(1);
  
  if (opcode==1)
    tr_complex_image_to_cs_image(x,y, z,rows); /* result in z */
  else if (opcode==2)
    error_bad_range_arg(1);	/* illegal opcode */
  else
    error_bad_range_arg(1);	/* illegal opcode */
  
  PRIMITIVE_RETURN (NIL);
}

/* x and y     must be ALREADY TRANSPOSED real and imaginary parts
 */
void tr_complex_image_to_cs_image(x,y, z,rows) 
     REAL *x,*y,*z;
     long rows;
{ long i,j,cols, n,n2, n2_1_n;
  REAL *xrow, *yrow, *zrow;
  cols = rows;			/* square image */
  n = rows;
  n2 = n/2;
  
  xrow= x; yrow= y; zrow= z; 
  for (j=0; j<=n2; j++)     zrow[j] = xrow[j]; /*        real part of row 0 (cs-array) */
  for (j=n2+1; j<n; j++)    zrow[j] = yrow[n-j]; /*      imag part of row 0            */
  
  xrow= x+n2*cols; yrow= y+n2*cols; zrow= z+n2*cols;
  for (j=0; j<=n2; j++)     zrow[j] = xrow[j]; /*        real part of row n2 (cs-array) */
  for (j=n2+1; j<n; j++)    zrow[j] = yrow[n-j]; /*      imag part of row n2            */
  
  xrow= x+cols;   zrow= z+cols;   n2_1_n = (n2-1)*cols;
  for (j=0; j<n2_1_n; j++)   zrow[j] = xrow[j];	/*       real rows 1,2,..,n2-1          */
  
  yrow= y+(n2-1)*cols;  zrow= z+(n2+1)*cols; /*          imag rows n2+1,n2+2,...        */
  for (i=1; i<n2; i++) {
    for (j=0; j<n; j++)   zrow[j] = yrow[j];
    zrow = zrow + cols;
    yrow = yrow - cols;
  }  
}


