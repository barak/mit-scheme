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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/fft.c,v 9.21 1987/01/22 14:24:33 jinx Rel $ */

/* FFT scheme primitive, using YEKTA FFT */

#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "zones.h" 
#include <math.h>
#include "array.h"
#include "image.h"

#define mult(pf1, pf2, pg1, pg2, w1, w2)            \
    {  int x, y, p2, p3, p4, p5, p6, p7;            \
       REAL tmp1, tmp2;                           \
       a = a / 2;                                   \
       p2 = - a;                                    \
       p3 = 0;                                      \
       for ( x = 1; x <= n2; x = x + a ) {          \
	 p2 = p2 + a;                               \
	 for( y = 1; y <= a; ++y ) {                \
	   ++p3;                                    \
	   p4 = p2 + 1;                             \
	   p5 = p2 + p3;                            \
	   p5 = ((p5-1) % n) + 1;                   \
	   p6 = p5 + a;                             \
	   tmp1 =  w1[p4-1] * pf1[p6-1]             \
	         - w2[p4-1] * pf2[p6-1];            \
	   tmp2 =  w1[p4-1] * pf2[p6-1]             \
                 + w2[p4-1] * pf1[p6-1];            \
	   pg1[p3-1] = pf1[p5-1] + tmp1;            \
	   pg2[p3-1] = pf2[p5-1] + tmp2;            \
	   p7 = p3 + n2;                            \
	   pg1[p7-1] = pf1[p5-1] - tmp1;            \
	   pg2[p7-1] = pf2[p5-1] - tmp2;            \
	 }                                          \
       }                                            \
} 

/* n is length, nu is power, w1,w2 are locations for twiddle tables,            */
/* f1,f2,g1,g2 are locations for fft, and flag is for forward(1) or inverse(-1) */
/* w1,w2 are half the size of f1,f2,g1,g2                                       */

/* f1,f2 contain the real and imaginary parts of the signal            */
/* The answer is left in f1, f2                                        */

C_Array_FFT(flag, nu, n, f1, f2, g1,g2,w1,w2) long flag, nu, n; REAL f1[], f2[], g1[], g2[], w1[], w2[];
{ long n2=n>>1, a;
  long  i, l, m;
  REAL twopi = 6.28318530717958, tm, k;

  a = n;  /* initially equal to length */
  if (flag == 1) k=1.0;
  else k = -1.0;
  /*  if ( nu > 12 ) Primitive_Error(ERR_ARG_2_BAD_RANGE); */ /* maximum power FFT */
  
  for (m=0; m<n; m++) {
    g1[m] = f1[m];
    g2[m] = f2[m];
  }
  
  for (m=0; m<n2; m++) {
    tm = twopi *  ((REAL) m) / ((REAL) n);
    w1[m] = cos( tm );
    w2[m] = k * sin( tm ); /* k is the flag */
  }
       
  if ((nu % 2) == 1) l = 2;
  else l = 1;
  for ( i = l; i <= nu ; i = i + 2 ) {
    mult(g1,g2,f1,f2,w1,w2);
    mult(f1,f2,g1,g2,w1,w2);
  }
  
  if (k==1.0) {                                          /* forward fft */
    if (l==1) {                        /* even power */
      for (m=0; m<n; m++) {
	f1[m] = g1[m];	f2[m] = g2[m];
      }
    }
    else {                                             /* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);                      /* f1 and f2 contain the result now */
    }}
  else {                                                   /* backward fft */
    tm = 1. / ((REAL) n);                            /* normalizing factor */
    if (l==1) {                       /* even power */
      for (m=0; m<n; m++) {
	f1[m] = tm * g1[m];	f2[m] = tm * g2[m]; }
    }
    else {                                             /* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);                      /* f1 and f2 contain the result now */
      for (m=0; m<n; m++) {
	f1[m] = tm * f1[m];	f2[m] = tm * f2[m]; }
    }
  }
}

Make_Twiddle_Tables(w1, w2, n, k) REAL *w1, *w2; long n, k;         /* n is the length of FFT */
{ long m, n2=n/2;
  REAL tm, twopi = 6.28318530717958;
  for (m=0; m<n2; m++) {
    tm = twopi *  ((REAL) m) / ((REAL) n);
    w1[m] = cos( tm );
    w2[m] = k * sin( tm );                              /* k is -/+1 for forward/inverse fft */
  }
}

C_Array_FFT_With_Given_Tables(flag, nu, n, f1, f2, g1,g2,w1,w2) 
     long flag, nu, n; REAL f1[], f2[], g1[], g2[], w1[], w2[];
{ long n2=n>>1, a;
  long  i, l, m;
  REAL twopi = 6.28318530717958, tm, k;

  a = n;                                                       /* initially equal to length */
  if (flag == 1) k=1.0;
  else k = -1.0;
  
  for (m=0; m<n; m++) {
    g1[m] = f1[m];
    g2[m] = f2[m];
  }
  
  if ((nu % 2) == 1) l = 2;
  else l = 1;
  for ( i = l; i <= nu ; i = i + 2 ) {
    mult(g1,g2,f1,f2,w1,w2);
    mult(f1,f2,g1,g2,w1,w2);
  }
  

  
  if (k==1.0) {                                          /* forward fft */
    if (l==1) {                        /* even power */
      for (m=0; m<n; m++) {
	f1[m] = g1[m];	f2[m] = g2[m];
      }
    }
    else {                                             /* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);                      /* f1 and f2 contain the result now */
    }}
  else {                                                   /* backward fft */
    tm = 1. / ((REAL) n);                            /* normalizing factor */
    if (l==1) {                       /* even power */
      for (m=0; m<n; m++) {
	f1[m] = tm * g1[m];	f2[m] = tm * g2[m]; }
    }
    else {                                             /* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);                      /* f1 and f2 contain the result now */
      for (m=0; m<n; m++) {
	f1[m] = tm * f1[m];	f2[m] = tm * f2[m]; }
    }
  }
}

C_Array_2D_FFT_In_Scheme_Heap(flag, nrows, ncols, Real_Array, Imag_Array) 
     long flag, nrows, ncols; REAL *Real_Array, *Imag_Array;
{ long i, j;
  REAL *Temp_Array;
  REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  long nrows_power, ncols_power, Length = nrows*ncols;
  
  if (nrows==ncols) {                                                /* SQUARE IMAGE, OPTIMIZE... */
    Square_Image_2D_FFT_In_Scheme_Heap(flag, nrows, Real_Array, Imag_Array);
  }
  else {                                                /* NOT A SQUARE IMAGE, CANNOT DO FAST_TRANSPOSE */
    /* FIRST (NCOLS-1)POINT FFTS FOR EACH ROW, THEN (NROWS-1)POINT FFTS FOR EACH COLUMN */

    for (ncols_power=0, i=ncols; i>1; ncols_power++) {                 /* FIND/CHECK POWERS OF ROWS,COLS */
      if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      i=i/2; }
    for (nrows_power=0, i=nrows; i>1; nrows_power++) {
      if ( (i % 2) == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
      i=i/2; }  
    
    Primitive_GC_If_Needed(Length*REAL_SIZE + ((max(nrows,ncols))*3*REAL_SIZE));
    Work_Here = (REAL *) Free;
    g1 = Work_Here;
    g2 = Work_Here + ncols;
    w1 = Work_Here + (ncols<<1);
    w2 = Work_Here + (ncols<<1) + (ncols>>1);
    Make_Twiddle_Tables(w1,w2,ncols, flag);
    for (i=0;i<nrows;i++) {                                    /* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT_With_Given_Tables(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);
    }
    
    Temp_Array = Work_Here;       
    Work_Here  = Temp_Array + Length;
    Image_Transpose(Real_Array, Temp_Array, nrows, ncols);    /* TRANSPOSE: (1) order of frequencies. (2) read columns.*/
    Image_Transpose(Imag_Array, Real_Array, nrows, ncols);

    g1 = Work_Here;
    g2 = Work_Here + nrows;
    w1 = Work_Here + (nrows<<1);
    w2 = Work_Here + (nrows<<1) + (nrows>>1);
    Make_Twiddle_Tables(w1,w2,nrows,flag);
    for (i=0;i<ncols;i++) {                                      /* COLUMN-WISE */
      f1 = Temp_Array + (i*nrows);        /* THIS IS REAL DATA */
      f2 = Real_Array + (i*nrows);        /* THIS IS IMAG DATA */
      C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
    }
    
    Image_Transpose(Real_Array, Imag_Array, ncols, nrows);   /* DO FIRST THIS !!!, do not screw up Real_Data !!! */
    Image_Transpose(Temp_Array, Real_Array, ncols, nrows);            /* TRANSPOSE BACK: order of frequencies. */
  }
}

Square_Image_2D_FFT_In_Scheme_Heap(flag, nrows, Real_Array, Imag_Array)
     long flag,nrows; REAL *Real_Array, *Imag_Array;
{ REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  long nrows_power;
  long i;

  for (nrows_power=0, i=nrows; i>1; nrows_power++) {                 /* FIND/CHECK POWERS OF ROWS */
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2; }
  Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;
  g2 = Work_Here + nrows;
  w1 = Work_Here + (nrows<<1);
  w2 = Work_Here + (nrows<<1) + (nrows>>1);
  Make_Twiddle_Tables(w1, w2, nrows, flag);                      /* MAKE TABLES */
  for (i=0;i<nrows;i++) {                                        /* ROW-WISE */
    f1 = Real_Array + (i*nrows);
    f2 = Imag_Array + (i*nrows);
    C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
  }
  Image_Fast_Transpose(Real_Array, nrows);    /* MUST TRANSPOSE (1) order of frequencies. (2) read columns. */
  Image_Fast_Transpose(Imag_Array, nrows);
  
  for (i=0;i<nrows;i++) {                                       /* COLUMN-WISE */
    f1 = Real_Array + (i*nrows);
    f2 = Imag_Array + (i*nrows);
    C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);     /* ncols=nrows... Twiddles... */
  }
  Image_Fast_Transpose(Real_Array, nrows);            /* TRANSPOSE BACK: order of frequencies. */
  Image_Fast_Transpose(Imag_Array, nrows);
}

C_Array_3D_FFT_In_Scheme_Heap(flag, ndeps, nrows, ncols, Real_Array, Imag_Array) 
     long flag, ndeps, nrows, ncols; REAL *Real_Array, *Imag_Array;
{ long l, m, n;
  REAL *Temp_Array;
  REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  long ndeps_power, nrows_power, ncols_power;
  
  if ((ndeps==nrows) && (nrows==ncols)) {                                           /* CUBIC IMAGE, OPTIMIZE... */
    Cube_Space_3D_FFT_In_Scheme_Heap(flag, ndeps, Real_Array, Imag_Array);
  }
  else {   
    for (ndeps_power=0, l=ndeps; l>1; ndeps_power++) {                 /* FIND/CHECK POWERS OF DEPS,ROWS,COLS */
      if ( (l % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      l=l/2; }
    for (nrows_power=0, m=nrows; m>1; nrows_power++) {
      if ( (m % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      m=m/2; }  
    for (ncols_power=0, n=ncols; n>1; ncols_power++) {                 
      if ( (n % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
      n=n/2; }
    
    printf("3D FFT implemented only for cubic-spaces.\n");
    printf("aborted\n.");
  }
}

Cube_Space_3D_FFT_In_Scheme_Heap(flag, ndeps, Real_Array, Imag_Array)
     long flag, ndeps; REAL *Real_Array, *Imag_Array;
{ register long l, m, n;
  register long ndeps_power, Surface_Length;
  register REAL *From_Real, *From_Imag;
  register REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  
  for (ndeps_power=0, l=ndeps; l>1; ndeps_power++) {                 /* FIND/CHECK POWER OF NDEPS */
    if ( (l % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    l=l/2; }
  Primitive_GC_If_Needed(ndeps*3*REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;
  g2 = Work_Here + ndeps;
  w1 = Work_Here + (ndeps<<1);
  w2 = Work_Here + (ndeps<<1) + (ndeps>>1);
  Make_Twiddle_Tables(w1, w2, ndeps, flag);                      /* MAKE TABLES */
  
  Surface_Length=ndeps*ndeps;
  From_Real = Real_Array;   From_Imag = Imag_Array;

  for (l=0; l<ndeps; l++,From_Real+=Surface_Length,From_Imag+=Surface_Length) {       /* DEPTH-WISE */
    
    f1 = From_Real;    f2 = From_Imag;
    for (m=0; m<ndeps; m++,f1+=ndeps,f2+=ndeps) {                                     /* ROW-WISE */
      C_Array_FFT_With_Given_Tables(flag, ndeps_power, ndeps, f1,f2,g1,g2,w1,w2); }
    Image_Fast_Transpose(From_Real, ndeps);    /* MUST TRANSPOSE (1) order of frequencies. (2) read columns. */
    Image_Fast_Transpose(From_Imag, ndeps);

    /* ndeps=nrows=ncols, same Twiddle Tables */

    f1 = From_Real;    f2 = From_Imag;
    for (n=0; n<ndeps; n++,f1+=ndeps,f2+=ndeps) {                                   /* COLUMN-WISE */
      C_Array_FFT_With_Given_Tables(flag, ndeps_power, ndeps, f1,f2,g1,g2,w1,w2); }
    Image_Fast_Transpose(From_Real, ndeps);            /* TRANSPOSE BACK: order of frequencies. */
    Image_Fast_Transpose(From_Imag, ndeps);
  }
}


/********************** below scheme primitives **********************/

/* NOTE: IF Arg2 and Arg3 are EQ?, then it signals an error!             */
/* (Arg1 = 1 ==> forward FFT), otherwise inverse FFT                     */

Define_Primitive(Prim_Array_FFT, 3, "ARRAY-FFT!")
{ long length, length1, power, flag, i;
  Pointer answer;
  REAL *f1,*f2,*g1,*g2,*w1,*w2;
  REAL *Work_Here;

  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);     /* flag */   
  Arg_2_Type(TC_ARRAY);      /* real */
  Arg_3_Type(TC_ARRAY);      /* imag */
  Set_Time_Zone(Zone_Math);

  flag = Get_Integer(Arg1);  
  length = Array_Length(Arg2);
  length1 = Array_Length(Arg3);

  if (length != length1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  power=0;
  for (power=0, i=length; i>1; power++) {
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2;
  }
  
  f1 = Scheme_Array_To_C_Array(Arg2);
  f2 = Scheme_Array_To_C_Array(Arg3);
  if (f1==f2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  Primitive_GC_If_Needed(length*3*REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;
  g2 = Work_Here + length;
  w1 = Work_Here + (length<<1);
  w2 = Work_Here + (length<<1) + (length>>1);

  C_Array_FFT(flag, power, length, f1,f2,g1,g2,w1,w2);
  
  Primitive_GC_If_Needed(4);
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg2;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg3;
  *Free++ = NIL;
  return answer;
}

Define_Primitive(Prim_Array_2D_FFT, 5, "ARRAY-2D-FFT!")
{ long flag, i, j;
  Pointer answer;
  REAL *Real_Array, *Imag_Array, *Temp_Array;
  REAL *f1,*f2,*g1,*g2,*w1,*w2;
  REAL *Work_Here;
  long Length, nrows, ncols, nrows_power, ncols_power;
  
  Primitive_5_Args();
  Arg_1_Type(TC_FIXNUM);     /* flag */   
  Range_Check(nrows, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg3, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_4_Type(TC_ARRAY);      /* real image */
  Arg_5_Type(TC_ARRAY);      /* imag image */
  Set_Time_Zone(Zone_Math);                             /* for timing */

  Sign_Extend(Arg1, flag);      /* should be 1 or -1 */
  Length = Array_Length(Arg4);
  if (Length != (nrows*ncols)) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Length != (Array_Length(Arg5))) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg4);
  Imag_Array = Scheme_Array_To_C_Array(Arg5);
  if (f1==f2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);

  for (ncols_power=0, i=ncols; i>1; ncols_power++) {                 /* FIND/CHECK POWERS OF ROWS,COLS */
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2; }
  for (nrows_power=0, i=nrows; i>1; nrows_power++) {
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
    i=i/2; }  

  if (nrows==ncols) {                                           /* SQUARE IMAGE, OPTIMIZE... */
    Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
    Work_Here = (REAL *) Free;
    g1 = Work_Here;
    g2 = Work_Here + ncols;
    w1 = Work_Here + (ncols<<1);
    w2 = Work_Here + (ncols<<1) + (ncols>>1);
    for (i=0;i<nrows;i++) {                                        /* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);           
    }
    Image_Fast_Transpose(Real_Array, nrows);    /* MUST TRANSPOSE (1) order of frequencies. (2) read columns. */
    Image_Fast_Transpose(Imag_Array, nrows);
    
    for (i=0;i<ncols;i++) {                                       /* COLUMN-WISE */
      f1 = Real_Array + (i*nrows);
      f2 = Imag_Array + (i*nrows);
      C_Array_FFT(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
    }
    Image_Fast_Transpose(Real_Array, nrows);            /* TRANSPOSE BACK: order of frequencies. */
    Image_Fast_Transpose(Imag_Array, nrows);
  }

  else {                                        /* NOT A SQUARE IMAGE, CANNOT DO FAST_TRANSPOSE */
    /* FIRST (NCOLS-1)POINT FFTS FOR EACH ROW, THEN (NROWS-1)POINT FFTS FOR EACH COLUMN */

    Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
    Primitive_GC_If_Needed(ncols*3*REAL_SIZE);
    Primitive_GC_If_Needed(Length*REAL_SIZE);
    Work_Here = (REAL *) Free;
    g1 = Work_Here;
    g2 = Work_Here + ncols;
    w1 = Work_Here + (ncols<<1);
    w2 = Work_Here + (ncols<<1) + (ncols>>1);
    for (i=0;i<nrows;i++) {                                    /* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);
    }
    
    Temp_Array = Work_Here;       
    Image_Transpose(Real_Array, Temp_Array, nrows, ncols);    /* TRANSPOSE: (1) order of frequencies. (2) read columns.*/
    Image_Transpose(Imag_Array, Real_Array, nrows, ncols);
    C_Array_Copy(Temp_Array, Imag_Array, Length);
    Temp_Array = Real_Array;                   /* JUST POINTER SWITCHING */
    Real_Array = Imag_Array;
    Imag_Array = Temp_Array;            

    g1 = Work_Here;
    g2 = Work_Here + nrows;
    w1 = Work_Here + (nrows<<1);
    w2 = Work_Here + (nrows<<1) + (nrows>>1);
    for (i=0;i<ncols;i++) {                                      /* COLUMN-WISE */
      f1 = Real_Array + (i*nrows);
      f2 = Imag_Array + (i*nrows);
      C_Array_FFT(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
    }
    
    Image_Transpose(Real_Array, Temp_Array, ncols, nrows);            /* TRANSPOSE BACK: order of frequencies. */
    Image_Transpose(Imag_Array, Real_Array, ncols, nrows);    /* NOTE: switch in ncols nrows. */ 
    C_Array_Copy(Temp_Array, Imag_Array, Length);                 /* THIS UNDOES THE SWITCHING IN ARG4,ARG5 */
  }

  Primitive_GC_If_Needed(4);                                       /* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg4;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg5;
  *Free++ = NIL;
  return answer;
}

Define_Primitive(Prim_Array_2D_FFT_3, 5, "ARRAY-2D-FFT-3!")
{ long flag, i, j;
  Pointer answer;
  REAL *Real_Array, *Imag_Array, *Temp_Array;
  REAL *f1,*f2,*g1,*g2,*w1,*w2;
  REAL *Work_Here;
  long Length, nrows, ncols, nrows_power, ncols_power;
  
  Primitive_5_Args();
  Arg_1_Type(TC_FIXNUM);     /* flag */   
  Range_Check(nrows, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg3, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_4_Type(TC_ARRAY);      /* real image */
  Arg_5_Type(TC_ARRAY);      /* imag image */
  Set_Time_Zone(Zone_Math);                             /* for timing */

  Sign_Extend(Arg1, flag);      /* should be 1 or -1 */
  Length = Array_Length(Arg4);
  if (Length != (nrows*ncols)) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Length != (Array_Length(Arg5))) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg4);
  Imag_Array = Scheme_Array_To_C_Array(Arg5);
  if (f1==f2) Primitive_Error(ERR_ARG_5_WRONG_TYPE);

  for (ncols_power=0, i=ncols; i>1; ncols_power++) {                 /* FIND/CHECK POWERS OF ROWS,COLS */
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2; }
  for (nrows_power=0, i=nrows; i>1; nrows_power++) {
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
    i=i/2; }  

  if (nrows==ncols) {                                           /* SQUARE IMAGE, OPTIMIZE... */
    Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
    Work_Here = (REAL *) Free;
    g1 = Work_Here;
    g2 = Work_Here + ncols;
    w1 = Work_Here + (ncols<<1);
    w2 = Work_Here + (ncols<<1) + (ncols>>1);
    Make_Twiddle_Tables(w1, w2, ncols, flag);        /* MAKE TABLES */
    for (i=0;i<nrows;i++) {                                        /* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT_With_Given_Tables(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);
    }
    Image_Fast_Transpose(Real_Array, nrows);    /* MUST TRANSPOSE (1) order of frequencies. (2) read columns. */
    Image_Fast_Transpose(Imag_Array, nrows);
    
    for (i=0;i<ncols;i++) {                                       /* COLUMN-WISE */
      f1 = Real_Array + (i*nrows);
      f2 = Imag_Array + (i*nrows);
      C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);     /* ncols=nrows... Twiddles... */
    }
    Image_Fast_Transpose(Real_Array, nrows);            /* TRANSPOSE BACK: order of frequencies. */
    Image_Fast_Transpose(Imag_Array, nrows);
  }

  else {                                        /* NOT A SQUARE IMAGE, CANNOT DO FAST_TRANSPOSE */
    /* FIRST (NCOLS-1)POINT FFTS FOR EACH ROW, THEN (NROWS-1)POINT FFTS FOR EACH COLUMN */

    Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
    Primitive_GC_If_Needed(ncols*3*REAL_SIZE);
    Primitive_GC_If_Needed(Length*REAL_SIZE);
    Work_Here = (REAL *) Free;
    g1 = Work_Here;
    g2 = Work_Here + ncols;
    w1 = Work_Here + (ncols<<1);
    w2 = Work_Here + (ncols<<1) + (ncols>>1);
    Make_Twiddle_Tables(w1,w2,ncols, flag);
    for (i=0;i<nrows;i++) {                                    /* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT_With_Given_Tables(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);
    }
    
    Temp_Array = Work_Here;
    Image_Transpose(Real_Array, Temp_Array, nrows, ncols);    /* TRANSPOSE: (1) order of frequencies. (2) read columns.*/
    Image_Transpose(Imag_Array, Real_Array, nrows, ncols);
    C_Array_Copy(Temp_Array, Imag_Array, Length);
    Temp_Array = Real_Array;                   /* JUST POINTER SWITCHING */
    Real_Array = Imag_Array;
    Imag_Array = Temp_Array;            

    g1 = Work_Here;
    g2 = Work_Here + nrows;
    w1 = Work_Here + (nrows<<1);
    w2 = Work_Here + (nrows<<1) + (nrows>>1);
    Make_Twiddle_Tables(w1,w2,nrows,flag);
    for (i=0;i<ncols;i++) {                                      /* COLUMN-WISE */
      f1 = Real_Array + (i*nrows);
      f2 = Imag_Array + (i*nrows);
      C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
    }
    
    Image_Transpose(Real_Array, Temp_Array, ncols, nrows);            /* TRANSPOSE BACK: order of frequencies. */
    Image_Transpose(Imag_Array, Real_Array, ncols, nrows);
    C_Array_Copy(Temp_Array, Imag_Array, Length);                 /* THIS UNDOES THE SWITCHING IN ARG4,ARG5 */
  }

  Primitive_GC_If_Needed(4);                                       /* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg4;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg5;
  *Free++ = NIL;
  return answer;
}

Define_Primitive(Prim_Array_2D_FFT_2, 5, "ARRAY-2D-FFT-2!")
{ long flag;
  Pointer answer;
  REAL *Real_Array, *Imag_Array;
  long Length, nrows, ncols;
  
  Primitive_5_Args();
  Arg_1_Type(TC_FIXNUM);     /* flag */   
  Range_Check(nrows, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg3, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_4_Type(TC_ARRAY);      /* real image */
  Arg_5_Type(TC_ARRAY);      /* imag image */
  Set_Time_Zone(Zone_Math);                             /* for timing */

  Sign_Extend(Arg1, flag);      /* should be 1 or -1 */
  Length = Array_Length(Arg4);
  if (Length != (nrows*ncols)) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Length != (Array_Length(Arg5))) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg4);
  Imag_Array = Scheme_Array_To_C_Array(Arg5);
  if (Real_Array==Imag_Array) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  
  C_Array_2D_FFT_In_Scheme_Heap(flag, nrows, ncols, Real_Array, Imag_Array);

  Primitive_GC_If_Needed(4);                                       /* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg4;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg5;
  *Free++ = NIL;
  return answer;
}

Define_Primitive(Prim_Array_3D_FFT, 6, "ARRAY-3D-FFT!")
{ long flag;
  Pointer answer;
  REAL *Real_Array, *Imag_Array;
  long Length, ndeps, nrows, ncols;
  
  Primitive_6_Args();
  Arg_1_Type(TC_FIXNUM);     /* flag */   
  Range_Check(ndeps, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(nrows, Arg3, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg4, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_5_Type(TC_ARRAY);      /* real image */
  Arg_6_Type(TC_ARRAY);      /* imag image */
  Set_Time_Zone(Zone_Math);                             /* for timing */

  Sign_Extend(Arg1, flag);      /* should be 1 or -1 */
  Length = Array_Length(Arg5);
  if (Length != (ndeps*nrows*ncols)) Primitive_Error(ERR_ARG_6_BAD_RANGE);
  if (Length != (Array_Length(Arg6))) Primitive_Error(ERR_ARG_6_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg5);
  Imag_Array = Scheme_Array_To_C_Array(Arg6);
  if (Real_Array==Imag_Array) Primitive_Error(ERR_ARG_6_WRONG_TYPE);

  C_Array_3D_FFT_In_Scheme_Heap(flag, ndeps, nrows, ncols, Real_Array, Imag_Array);

  Primitive_GC_If_Needed(4);                                       /* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg5;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg6;
  *Free++ = NIL;
  return answer;
}

/* END */

