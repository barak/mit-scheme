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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/image.c,v 9.22 1987/08/10 20:06:33 pas Exp $ */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "array.h"
#include <math.h>

/* IMAGE PROCESSING...                    */
/* (much comes from array.c)              */

Define_Primitive(Prim_Read_Image_From_Ascii_File, 1, "READ-IMAGE-FROM-ASCII-FILE")
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

Define_Primitive(Prim_Read_Image_From_Cbin_File, 1, "READ-IMAGE-FROM-CBIN-FILE")
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
Define_Primitive(Prim_Read_Image_From_2bint_File, 1, "READ-IMAGE-FROM-2BINT-FILE")
{ long Length, i,j;
  long nrows, ncols, array_index;
  FILE *fopen(), *fp;
  char *file_string;
  REAL *To_Here;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;
  Boolean Open_File();
  float x_origin, y_origin;

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

Define_Primitive(Prim_Read_Image_From_CTSCAN_File, 1, "READ-IMAGE-FROM-CTSCAN-FILE")
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

Define_Primitive(Prim_Subimage, 5, "SUBIMAGE")
{ long Length, new_Length;
  long i,j;
  Pointer Pnrows, Pncols, Prest, Parray;
  long lrow, hrow, lcol, hcol;
  long nrows, ncols, new_nrows, new_ncols;

  REAL *Array, *To_Here;
  Pointer Result, Array_Data_Result, *Orig_Free;
  int Error_Number;
  long allocated_cells;

  Primitive_5_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);

  Range_Check(lrow, Arg2, 0, nrows, ERR_ARG_2_BAD_RANGE);
  Range_Check(hrow, Arg3, lrow, nrows, ERR_ARG_3_BAD_RANGE);
  Range_Check(lcol, Arg4, 0, ncols, ERR_ARG_4_BAD_RANGE);
  Range_Check(hcol, Arg5, lcol, ncols, ERR_ARG_5_BAD_RANGE);
  new_nrows = hrow - lrow +1;
  new_ncols = hcol - lcol +1;
  new_Length = new_nrows * new_ncols;

  /* ALLOCATE SPACE */
  Primitive_GC_If_Needed(6);
  Orig_Free = Free;
  Free += 6;
  Result = Make_Pointer(TC_LIST, Orig_Free);
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, new_nrows);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  *Orig_Free++ = Make_Non_Pointer(TC_FIXNUM, new_ncols);
  *Orig_Free = Make_Pointer(TC_LIST, Orig_Free+1);
  Orig_Free++;
  Allocate_Array(Array_Data_Result, new_Length, allocated_cells); 
  *Orig_Free++ = Array_Data_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  
  Array = Scheme_Array_To_C_Array(Parray);
  To_Here = Scheme_Array_To_C_Array(Array_Data_Result);
  for (i=lrow; i<=hrow; i++) {
    for (j=lcol; j<=hcol; j++) {
      *To_Here++ = Array[i*ncols+j];                              /*  A(i,j)--->Array[i*ncols+j]  */
    }}
  
  return Result;
}

Define_Primitive(Prim_Image_Double_To_Float, 1, "IMAGE-DOUBLE-TO-FLOAT!")
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

Define_Primitive(Prim_Image_Set_Row, 3, "IMAGE-SET-ROW!")
{ long Length, i,j;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols, row_to_set;
  REAL *Array, *Row_Array;
  
  Primitive_3_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);

  Arg_2_Type(TC_FIXNUM);
  Range_Check(row_to_set, Arg2, 0, (nrows-1), ERR_ARG_2_BAD_RANGE);
  Arg_3_Type(TC_ARRAY);
  Row_Array = Scheme_Array_To_C_Array(Arg3);
  if (Array_Length(Arg3)>ncols) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Parray);
  C_Image_Set_Row(Array, row_to_set, Row_Array, nrows, ncols);
  return Arg1;
}

Define_Primitive(Prim_Image_Set_Column, 3, "IMAGE-SET-COLUMN!")
{ long Length, i,j;
  Pointer Pnrows, Pncols, Prest, Parray;
  long nrows, ncols, col_to_set;
  REAL *Array, *Col_Array;
  
  Primitive_3_Args();
  Arg_1_Type(TC_LIST);             /* image = (nrows ncols array) */
  Pnrows = Vector_Ref(Arg1, CONS_CAR);
  Prest = Vector_Ref(Arg1, CONS_CDR);
  Pncols = Vector_Ref(Prest, CONS_CAR);
  Prest = Vector_Ref(Prest, CONS_CDR);
  Parray = Vector_Ref(Prest, CONS_CAR);
  if (Vector_Ref(Prest, CONS_CDR) != NIL) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  if (Type_Code(Parray) != TC_ARRAY) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);

  Arg_2_Type(TC_FIXNUM);
  Range_Check(col_to_set, Arg2, 0, (nrows-1), ERR_ARG_2_BAD_RANGE);
  Arg_3_Type(TC_ARRAY);
  Col_Array = Scheme_Array_To_C_Array(Arg3);
  if (Array_Length(Arg3)>ncols) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Parray);
  C_Image_Set_Col(Array, col_to_set, Col_Array, nrows, ncols);
  return Arg1;
}

C_Image_Set_Row(Image_Array, row_to_set, Row_Array, nrows, ncols) REAL *Image_Array, *Row_Array; 
long nrows, ncols, row_to_set;
{ long j;
  REAL *From_Here, *To_Here;

  To_Here   = &Image_Array[row_to_set*ncols];
  From_Here = Row_Array;
  for (j=0;j<ncols;j++) 
    *To_Here++ = *From_Here++;
}

C_Image_Set_Col(Image_Array, col_to_set, Col_Array, nrows, ncols) REAL *Image_Array, *Col_Array; 
long nrows, ncols, col_to_set;
{ long i;
  REAL *From_Here, *To_Here;

  To_Here   = &Image_Array[col_to_set];
  From_Here = Col_Array;
  for (i=0;i<nrows;i++) {
    *To_Here = *From_Here++;
    To_Here += nrows;
  }
}
       

Define_Primitive(Prim_Image_Make_Ring, 4, "IMAGE-MAKE-RING")
{ long Length, i,j;
  long nrows, ncols;
  long Min_Cycle=0, Max_Cycle=min((nrows/2),(ncols/2));
  long low_cycle, high_cycle;
  REAL *Ring_Array;
  Pointer Result, Ring_Array_Result, *Orig_Free;
  long allocated_cells;

  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(nrows, Arg1, 0, 512, ERR_ARG_1_BAD_RANGE);
  Arg_2_Type(TC_FIXNUM);
  Range_Check(ncols, Arg2, 0, 512, ERR_ARG_2_BAD_RANGE);
  Length = nrows*ncols;
  Arg_3_Type(TC_FIXNUM);      
  Range_Check(low_cycle, Arg3, Min_Cycle, Max_Cycle, ERR_ARG_2_BAD_RANGE);
  Arg_4_Type(TC_FIXNUM);      
  Range_Check(high_cycle, Arg4, Min_Cycle, Max_Cycle, ERR_ARG_3_BAD_RANGE);
  if (high_cycle<low_cycle) Primitive_Error(ERR_ARG_3_BAD_RANGE);

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
  Allocate_Array(Ring_Array_Result, Length, allocated_cells); 
  *Orig_Free++ = Ring_Array_Result;
  *Orig_Free = NIL;
  /* END ALLOCATION */
  
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


/* DONE WITHOUT SIDE-EFFECTS FOR SIMPLICITY */
Define_Primitive(Prim_Image_Periodic_Shift, 3, "IMAGE-PERIODIC-SHIFT")
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
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Arg_2_Type(TC_FIXNUM);      
  Sign_Extend(Arg2, ver_shift);
  ver_shift = ver_shift % nrows;
  Arg_3_Type(TC_FIXNUM);
  Sign_Extend(Arg3, hor_shift);
  hor_shift = hor_shift % ncols;

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
  
  Array = Scheme_Array_To_C_Array(Parray);
  New_Array = Scheme_Array_To_C_Array(Array_Data_Result);
  C_Image_Periodic_Shift(Array, New_Array, nrows, ncols, ver_shift, hor_shift);
  return Result;
}

/* ASSUMES hor_shift<nrows, ver_shift<ncols */
C_Image_Periodic_Shift(Array, New_Array, nrows, ncols, ver_shift, hor_shift)
     REAL *Array, *New_Array; long nrows, ncols, hor_shift, ver_shift;
{ long i, j, ver_index, hor_index;
  REAL *To_Here;
  To_Here = New_Array;
  for (i=0;i<nrows;i++) { 
    for (j=0;j<ncols;j++) {
      ver_index = (i+ver_shift) % nrows;
      if (ver_index<0) ver_index = nrows-ver_index;             /* wrapping around */
      hor_index = (j+hor_shift) % ncols;
      if (hor_index<0) hor_index = ncols-hor_index;
      *To_Here++ = Array[ver_index*ncols + hor_index];
    }}
}


/* ROTATIONS.....           */

Define_Primitive(Prim_Image_Transpose, 1, "IMAGE-TRANSPOSE!")
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
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);
  
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

Define_Primitive(Prim_Image_Rotate_90clw, 1, "IMAGE-ROTATE-90CLW!")
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
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);
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

Define_Primitive(Prim_Image_Rotate_90cclw, 1, "IMAGE-ROTATE-90CCLW!")
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
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);
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

Define_Primitive(Prim_Image_Mirror, 1, "IMAGE-MIRROR!")
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
  
  Range_Check(nrows, Pnrows, 0, 512, ERR_ARG_1_BAD_RANGE);
  Range_Check(ncols, Pncols, 0, 512, ERR_ARG_1_BAD_RANGE);
  Length = nrows*ncols;
  
  Array = Scheme_Array_To_C_Array(Parray);
  C_Mirror_Image(Array, nrows, ncols);             /* side-effecting... */
  
  return Arg1;
}


/* THE C ROUTINES THAT DO THE REAL WORK */

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





/* END */






/*

Define_Primitive(Prim_Sample_Periodic_2d_Function, 4, "SAMPLE-PERIODIC-2D-FUNCTION")
{ long N, i, allocated_cells, Function_Number;
  REAL Signal_Frequency, Sampling_Frequency, DT, DTi;
  REAL twopi = 6.28318530717958, twopi_f_dt;
  Pointer Result, Pfunction_number, Psignal_frequency; 
  Pointer Pfunction_Number;
  int Error_Number;
  REAL *To_Here, unit_square_wave(), unit_triangle_wave();
  
  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 10, ERR_ARG_1_BAD_RANGE); / * fix this * /
  
  Error_Number = Scheme_Number_To_REAL(Arg2, &Signal_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Signal_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Error_Number = Scheme_Number_To_REAL(Arg3, &Sampling_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  DT = (1 / Sampling_Frequency);
  twopi_f_dt = twopi * Signal_Frequency * DT;
  
  Range_Check(N, Arg4, 0, ARRAY_MAX_LENGTH, ERR_ARG_4_BAD_RANGE); 
  
  allocated_cells = (N*REAL_SIZE) + ARRAY_HEADER_SIZE;
  Primitive_GC_If_Needed(allocated_cells);
  
  Result = Make_Pointer(TC_ARRAY, Free);
  Free[ARRAY_HEADER] = Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  Free[ARRAY_LENGTH] = N;
  To_Here = Scheme_Array_To_C_Array(Result);
  Free = Free+allocated_cells;
  
  DT = twopi_f_dt;
  if (Function_Number == 0) 
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = cos(DTi);
  else if (Function_Number == 1)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = sin(DTi);
  else if (Function_Number == 2)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = unit_square_wave(DTi);
  else if (Function_Number == 3) 
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = unit_triangle_wave(DTi);
  else
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  
  return Result; 
}

*/
/* END IMAGE PROCESSING */



/* Note for the macro: To1 and To2 must BE Length1-1, and Length2-2 RESPECTIVELY ! */
/*
#define C_Convolution_Point_Macro(X, Y, To1, To2, N, Result)                                \
{ long Min_of_N_To1=min((N),(To1));                                                         \
  long mi, N_minus_mi;                                                                      \
  REAL Sum=0.0;                                                                           \
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
  / * ASSUME A SIGNAL FROM INDEX 0 TO ENDPOINT=LENGTH-1 * /
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
*/

/*  m_pi = 3.14159265358979323846264338327950288419716939937510; */

/* 
Define_Primitive(Prim_Sample_Periodic_Function, 4, "SAMPLE-PERIODIC-FUNCTION")
{ long N, i, allocated_cells, Function_Number;
  REAL Signal_Frequency, Sampling_Frequency, DT, DTi;
  REAL twopi = 6.28318530717958, twopi_f_dt;
  Pointer Result, Pfunction_number, Psignal_frequency; 
  Pointer Pfunction_Number;
  int Error_Number;
  REAL *To_Here, unit_square_wave(), unit_triangle_wave();
  
  Primitive_4_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_4_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 10, ERR_ARG_1_BAD_RANGE); / * fix this * /
  
  Error_Number = Scheme_Number_To_REAL(Arg2, &Signal_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Signal_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Error_Number = Scheme_Number_To_REAL(Arg3, &Sampling_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_3_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  DT = (1 / Sampling_Frequency);
  twopi_f_dt = twopi * Signal_Frequency * DT;
  
  Range_Check(N, Arg4, 0, ARRAY_MAX_LENGTH, ERR_ARG_4_BAD_RANGE); 
  
  allocated_cells = (N*REAL_SIZE) + ARRAY_HEADER_SIZE;
  Primitive_GC_If_Needed(allocated_cells);
  
  Result = Make_Pointer(TC_ARRAY, Free);
  Free[ARRAY_HEADER] = Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  Free[ARRAY_LENGTH] = N;
  To_Here = Scheme_Array_To_C_Array(Result);
  Free = Free+allocated_cells;
  
  DT = twopi_f_dt;
  if (Function_Number == 0) 
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = cos(DTi);
  else if (Function_Number == 1)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = sin(DTi);
  else if (Function_Number == 2)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = unit_square_wave(DTi);
  else if (Function_Number == 3) 
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = unit_triangle_wave(DTi);
  else
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  
  return Result; 
}

REAL hamming(t, length) REAL t, length;
{ REAL twopi = 6.28318530717958;
  REAL pi = twopi/2.;
  REAL t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0)) return(.08 + .46 * (1 - t_bar));
  else return (0);
}

REAL hanning(t, length) REAL t, length;
{ REAL twopi = 6.28318530717958;
  REAL pi = twopi/2.;
  REAL t_bar = cos(twopi * (t / length));
  if ((t<length) && (t>0.0)) 
    return(.5 * (1 - t_bar));
  else return (0);
}

REAL unit_square_wave(t) REAL t;
{ REAL twopi = 6.28318530717958;
  REAL fmod(), fabs();
  REAL pi = twopi/2.;
  REAL t_bar = fabs(fmod(t, twopi));
  if (t_bar < pi) return(1);
  else return(0);
}

REAL unit_triangle_wave(t) REAL t;
{ REAL twopi = 6.28318530717958;
  REAL pi = twopi/2.;
  REAL t_bar = fabs(fmod(t, twopi));
  if (t_bar < pi) return( t_bar / pi );
  else return( (twopi - t_bar) / pi );
}

Define_Primitive(Prim_Sample_Aperiodic_Function, 3, "SAMPLE-APERIODIC-FUNCTION")
{ long N, i, allocated_cells, Function_Number;
  REAL Sampling_Frequency, DT, DTi;
  REAL twopi = 6.28318530717958;
  Pointer Result;
  int Error_Number;
  REAL *To_Here, twopi_dt;

  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);
  Arg_3_Type(TC_FIXNUM);
  Range_Check(Function_Number, Arg1, 0, 6, ERR_ARG_1_BAD_RANGE);
  
  Error_Number = Scheme_Number_To_REAL(Arg2, &Sampling_Frequency);
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  if (Sampling_Frequency == 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  DT = (1 / Sampling_Frequency);
  twopi_dt = twopi * DT;

  Range_Check(N, Arg3, 0, ARRAY_MAX_LENGTH, ERR_ARG_3_BAD_RANGE);

  allocated_cells = (N*REAL_SIZE) + ARRAY_HEADER_SIZE;
  Primitive_GC_If_Needed(allocated_cells);
  
  Result = Make_Pointer(TC_ARRAY, Free);
  Free[ARRAY_HEADER] = Make_Non_Pointer(TC_MANIFEST_ARRAY, allocated_cells-1);
  Free[ARRAY_LENGTH] = N;
  To_Here = Scheme_Array_To_C_Array(Result);
  Free = Free+allocated_cells;
  
  DT = twopi_dt;
  if      (Function_Number == 0)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = rand();
  else if (Function_Number == 1) 
  { REAL length=DT*N;
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = hanning(DTi, length);
  }
  else if (Function_Number == 2) 
  { REAL length=DT*N;
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = hamming(DTi, length);
  }
  else if (Function_Number == 3)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = sqrt(DTi);
  else if (Function_Number == 4)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = log(DTi);
  else if (Function_Number == 5)
    for (i=0, DTi=0.0; i < N; i++, DTi += DT)
      *To_Here++ = exp(DTi);
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

  Sign_Extend(Arg2, Sampling_Ratio);               / * Sampling_Ratio = integer ratio of sampling_frequencies * /
  Sampling_Ratio = Sampling_Ratio % Length;                                  / * periodicity * /
  if (Sampling_Ratio < 1)  Primitive_Error(ERR_ARG_2_BAD_RANGE);
  
  Array = Scheme_Array_To_C_Array(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  Pseudo_Length = Length * Sampling_Ratio;
  for (i=0; i<Pseudo_Length; i += Sampling_Ratio) {       / * new Array has the same Length by assuming periodicity * /
    array_index = i % Length;
    *To_Here++ = Array[array_index];
  }
  
  return Result;
}

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
  Shift = Shift % Length;                                  / * periodic waveform, same sign as dividend * /
  Array = Scheme_Array_To_C_Array(Arg1);
  Allocate_Array(Result, Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i++) {                       / * new Array has the same Length by assuming periodicity * /
    array_index = (i+Shift) % Length;
    if (array_index<0) array_index = Length + array_index;                / * wrap around * /
    *To_Here++ = Array[array_index];
  }
  
  return Result;
}

/ * this should really be done in SCHEME using ARRAY-MAP ! * /

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
  New_Length = Length / Sampling_Ratio;      
  / * greater than zero * /
  Allocate_Array(Result, New_Length, allocated_cells);
  To_Here = Scheme_Array_To_C_Array(Result);
  
  for (i=0; i<Length; i += Sampling_Ratio) {
    *To_Here++ = Array[i];
  }
  
  return Result;
}


/ * ARRAY-APERIODIC-SHIFT can be done in scheme using subarray, and array-append * /


for UPSAMPLING
if ((Length % Sampling_Ratio) != 0) Primitive_Error(ERR_ARG_2_BAD_RANGE);
UNIMPLEMENTED YET

*/

/* END OF FILE */  

