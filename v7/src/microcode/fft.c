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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/fft.c,v 9.24 1988/08/15 20:46:29 cph Exp $ */

/* Fourier Transforms (pas)
   1. DFT (FFT),
   2. CZT (chirp-z-transform).
   3. 2D DFT
   */

#include "scheme.h"
#include "prims.h"
#include "flonum.h"
#include "zones.h" 
#include <math.h>
#include "array.h"
#include "image.h"

/* The following implement the DFT as defined in Seibert's Book (6003).
   FORWARD DFT ---- Negative exponent and division by N
   BACKWARD DFT --- Positive exponent
   Note: Seibert's Forward DFT is Oppenheim's Backward DFT.
   */

#define PI    3.141592653589793238462643
#define TWOPI 6.283185307179586476925287
/* Abramowitz and Stegun */

void Make_FFT_Tables(w1, w2, n, flag)
     REAL *w1, *w2; long n, flag;         /* n  = length of data */
{ long m, n2=n/2;                         /* n2 = length of w1,w2 */
  double tm; 
  if (flag==1)			/* FORWARD FFT */
    for (m=0; m<n2; m++) {
      tm = TWOPI * ((double) m) / ((double) n);
      w1[m] = (REAL) cos(tm);
      w2[m] = (REAL) - sin(tm); }
  else
    for (m=0; m<n2; m++) {
      tm = TWOPI * ((double) m) / ((double) n);
      w1[m] = (REAL) cos(tm);
      w2[m] = (REAL) sin(tm); }
}

#define mult(pf1, pf2, pg1, pg2, w1, w2)            \
    {  long x, y, p2, p3, p4, p5, p6, p7;           \
       REAL tmp1, tmp2;                             \
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

/* n = length of input data f1,f2;
   power =  log2(n),
   g1,g2  are intermediate arrays of length n,
   w1,w2 point to FFT tables (twiddle factors),
   flag 1 for forward FFT, else inverse.
   */
/* The arrays w1,w2 are half the size of f1,f2,g1,g2.
   f1,f2 contain the real and imaginary parts of the signal.
   The answer is left in f1, f2.
   */

void C_Array_FFT(flag, power, n, f1, f2, g1,g2,w1,w2)
     long flag, power, n; REAL f1[], f2[], g1[], g2[], w1[], w2[];
{ long n2=n>>1, a;
  long  i, l, m;
  REAL tm;
  a = n;			/* initially equal to length of data */
  
  for (m=0; m<n; m++) { g1[m] = f1[m]; g2[m] = f2[m]; }
  Make_FFT_Tables(w1,w2,n, flag);
  
  if ((power % 2) == 1) l = 2; else l = 1; /* even,odd power */
  for ( i = l; i <= power ; i = i + 2 ) {
    mult(g1,g2,f1,f2,w1,w2);
    mult(f1,f2,g1,g2,w1,w2); }
  
  if (flag==1) {		/* FORWARD FFT */
    tm = 1. / ((REAL) n);	/* normalizing factor */
    if (l==1)			/* even power */
      for (m=0; m<n; m++) { f1[m] = tm * g1[m];	f2[m] = tm * g2[m]; }
    else {			/* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);	/* f1 and f2 contain the result now */
      for (m=0; m<n; m++) { f1[m] = tm * f1[m]; f2[m] = tm * f2[m]; }}
  }
  else {			/* BACKWARD FFT */
    if (l==1)			/* even power */
      for (m=0; m<n; m++) { f1[m] = g1[m]; f2[m] = g2[m]; }
    else			/* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);	/* f1 and f2 contain the result now */
  }
}

void C_Array_FFT_With_Given_Tables(flag, power, n, f1, f2, g1,g2,w1,w2) 
     long flag, power, n; REAL f1[], f2[], g1[], g2[], w1[], w2[];
{ long n2=n>>1, a;
  long  i, l, m;
  REAL tm;
  a = n;			/* initially equal to length */
  
  for (m=0; m<n; m++) { g1[m] = f1[m];  g2[m] = f2[m]; }

  if ((power % 2) == 1) l = 2; else l = 1; /* even,odd power */
  for ( i = l; i <= power ; i = i + 2 ) {
    mult(g1,g2,f1,f2,w1,w2);
    mult(f1,f2,g1,g2,w1,w2); }
  
  if (flag==1) {		/* FORWARD FFT */
    tm = 1. / ((REAL) n);	/* normalizing factor */
    if (l==1)			/* even power */
      for (m=0; m<n; m++) { f1[m] = tm * g1[m];	f2[m] = tm * g2[m]; }
    else {			/* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);	/* f1 and f2 contain the result now */
      for (m=0; m<n; m++) { f1[m] = tm * f1[m]; f2[m] = tm * f2[m]; }}
  }
  else {			/* BACKWARD FFT */
    if (l==1)			/* even power */
      for (m=0; m<n; m++) { f1[m] = g1[m]; f2[m] = g2[m]; }
    else			/* odd power ==> do one more mult */
      mult(g1,g2,f1,f2,w1,w2);	/* f1 and f2 contain the result now */
  }
}


/* CHIRP-Z-TRANSFORM (for complex data)
 */

#define take_modulo_one(x, answer)  \
{ long ignore_integral_part;        \
  double modf();                    \
  answer = (REAL) modf( ((double) x), &ignore_integral_part); }

#define power_of_2_to_power(n,power)      \
{ long i;                                 \
  for (power=0,i=n; i>1; power++)         \
  { if ((i%2) == 1) { printf("\nNot a power of 2--- %d\n",n); exit(1); }  \
    i=i/2; }}

long smallest_power_of_2_ge(n)
     long n;
{ long i,power;
  if (n<0) { printf("\nsmallest_pwr_of_2_ge negative argument--- %d\n", n); exit(1); }
  power=0; i=1;
  while (i<n)
  { power++; i=i*2; }
  return(power);
}

/* C_Array_CZT ------------------ Generalization of DFT.
   Frequency is scaled as an L/2-point DFT of the input data (zero padded to L/2).
   ARGUMENTS and ASSUMPTIONS:
   phi = starting point (on unit circle) -- Range 0,1 (covers 0,2pi like DFT angle)
   rho = resolution (angular frequency spacing) -- Range 0,1 (maps 0,2pi like DFT angle)
   N = input data length
   M = output data length
   log2_L = smallest_power_of_2_ge(N+M-1)   ----
   f1,f2 contain the input data (complex).
   f1,f2,fo1,fo2,g1,g2            must be of length L
   fft_w1,fft_w2                  must be of length L/2
   czt_w1,czt_w2                  must be of length max(M,N)  ---- 
   RESULT is left on f1,f2 (M complex numbers).
   */
C_Array_CZT(phi,rho, N,M,log2_L, f1,f2,fo1,fo2, g1,g2, fft_w1,fft_w2,czt_w1,czt_w2)
     double phi, rho;
     REAL *f1,*f2,*fo1,*fo2, *g1,*g2,  *fft_w1,*fft_w2,*czt_w1,*czt_w2;
     long N,M,log2_L;
{ long i, maxMN, L, L2;
  void Make_CZT_Tables(), CZT_Pre_Multiply(), Make_Chirp_Filter();
  void Make_FFT_Tables(), C_Array_FFT_With_Given_Tables(), C_Array_Complex_Multiply_Into_First_One();
  
  maxMN = max(M,N);
  L = 1<<log2_L;
  L2 = L/2;
  
  CZT_Pre_Multiply(phi,rho, f1,f2, N,L);
  Make_FFT_Tables(fft_w1,fft_w2, L, 1);	/* PREPARE TABLES FOR FORWARD FFT */
  C_Array_FFT_With_Given_Tables(1, log2_L, L, f1,f2, g1,g2, fft_w1,fft_w2);
  
  Make_CZT_Tables(czt_w1,czt_w2, rho, maxMN);
  Make_Chirp_Filter(fo1,fo2, N,M,L, czt_w1,czt_w2);
  C_Array_FFT_With_Given_Tables(1, log2_L, L, fo1,fo2, g1,g2, fft_w1,fft_w2);
  
  C_Array_Complex_Multiply_Into_First_One(f1,f2, fo1,fo2, L);
  for (i=0;i<L2;i++) fft_w2[i] = (-fft_w2[i]); /* PREPARE TABLES FOR INVERSE FFT */
  C_Array_FFT_With_Given_Tables(-1, log2_L, L, f1,f2, g1,g2, fft_w1,fft_w2);
  C_Array_Complex_Multiply_Into_First_One(f1,f2, czt_w1,czt_w2, M);
}

void CZT_Pre_Multiply(phi,rho, f1,f2, N,L)      /* phi = starting point */
     double phi,rho; REAL *f1,*f2; long N,L;    /* this proc is two complex multiplication */
{ long i;
  double double_i, tmp, A1, A2;
  rho = rho*.5;			/* To make 1/2 in exponent "(n^2)/2" */
  for (i=0;i<N;i++)
  { double_i = ((double) i);
    tmp = ((rho*double_i)+phi) * double_i;
    take_modulo_one(tmp,tmp);	/* allows more decimal places */
    tmp = TWOPI * tmp;
    A1 = cos(tmp);
    A2 = sin(tmp);
    tmp   =         A1*f1[i] - A2*f2[i];
    f2[i] = (REAL) (A1*f2[i] + A2*f1[i]);
    f1[i] = (REAL) tmp;
  }
  for (i=N;i<L;i++) { f1[i] = 0.0; /* zero pad */
		      f2[i] = 0.0; } 
}

void Make_Chirp_Filter(fo1,fo2, N,M,L, czt_w1,czt_w2)
     REAL *fo1,*fo2, *czt_w1,*czt_w2; long N,M,L;
{ long i, L_minus_N_plus_1 = L-N+1;
  for (i=0;i<M;i++) { fo1[i] =   czt_w1[i];
		      fo2[i] = - czt_w2[i]; }
  for (i=M;i<L_minus_N_plus_1;i++) { fo1[i] = 0.0; /* arbitrary region, circular convolution */
				     fo2[i] = 0.0; }
  for (i=L_minus_N_plus_1;i<L;i++) { fo1[i] =   czt_w1[(L-i)];
				     fo2[i] = - czt_w2[(L-i)]; }
}

void Make_CZT_Tables(czt_w1,czt_w2, rho, maxMN)              /* rho = resolution */
     double rho; REAL *czt_w1,*czt_w2; long maxMN;
{ long i;
  double tmp;
  rho = rho*.5;			/* the 1/2 in the "(n^2)/2" exponent */
  for (i=0;i<maxMN;i++)
  { tmp = ((double) i);
    tmp = tmp * (rho*tmp);
    take_modulo_one(tmp,tmp);	/* allows more decimal places */
    tmp = TWOPI * tmp;
    czt_w1[i] = (REAL) cos(tmp);
    czt_w2[i] = (REAL) sin(tmp);
  }}

/*  not used:
void CZT_Post_Multiply(f1,f2,czt_w1,czt_w2,M)
     REAL *f1,*f2,*czt_w1,*czt_w2; long M;
{ long i;
  REAL tmp;
  for (i=0;i<M;i++)
  { tmp = f1[i]*czt_w1[i] - f2[i]*czt_w2[i];
    f2[i] = 2.0 * (f1[i]*czt_w2[i] + f2[i]*czt_w1[i]);
    f1[i] = 2.0 * tmp;
  }}
*/


/* 2D DFT ---------------- row-column decomposition
   (3D not working yet)
 */
C_Array_2D_FFT_In_Scheme_Heap(flag, nrows, ncols, Real_Array, Imag_Array) 
     long flag, nrows, ncols; REAL *Real_Array, *Imag_Array;
{ long i, j;
  REAL *Temp_Array;
  REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  long nrows_power, ncols_power, Length = nrows*ncols;
  
  if (nrows==ncols) {		/* SQUARE IMAGE, OPTIMIZE... */
    Square_Image_2D_FFT_In_Scheme_Heap(flag, nrows, Real_Array, Imag_Array);
  }
  else {			/* NOT A SQUARE IMAGE, CANNOT DO FAST_TRANSPOSE */
    /* FIRST (NCOLS-1)POINT FFTS FOR EACH ROW, THEN (NROWS-1)POINT FFTS FOR EACH COLUMN */
    
    for (ncols_power=0, i=ncols; i>1; ncols_power++) { /* FIND/CHECK POWERS OF ROWS,COLS */
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
    Make_FFT_Tables(w1,w2,ncols, flag);
    for (i=0;i<nrows;i++) {	/* ROW-WISE */
      f1 = Real_Array + (i*ncols);
      f2 = Imag_Array + (i*ncols);
      C_Array_FFT_With_Given_Tables(flag, ncols_power, ncols, f1,f2,g1,g2,w1,w2);
    }
    
    Temp_Array = Work_Here;       
    Work_Here  = Temp_Array + Length;
    Image_Transpose(Real_Array, Temp_Array, nrows, ncols); /* TRANSPOSE: (1) order of frequencies. (2) read columns.*/
    Image_Transpose(Imag_Array, Real_Array, nrows, ncols);

    g1 = Work_Here;
    g2 = Work_Here + nrows;
    w1 = Work_Here + (nrows<<1);
    w2 = Work_Here + (nrows<<1) + (nrows>>1);
    Make_FFT_Tables(w1,w2,nrows,flag);
    for (i=0;i<ncols;i++) {	/* COLUMN-WISE */
      f1 = Temp_Array + (i*nrows); /* THIS IS REAL DATA */
      f2 = Real_Array + (i*nrows); /* THIS IS IMAG DATA */
      C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
    }
    
    Image_Transpose(Real_Array, Imag_Array, ncols, nrows); /* DO FIRST THIS !!!, do not screw up Real_Data !!! */
    Image_Transpose(Temp_Array, Real_Array, ncols, nrows); /* TRANSPOSE BACK: order of frequencies. */
  }
}

Square_Image_2D_FFT_In_Scheme_Heap(flag, nrows, Real_Array, Imag_Array)
     long flag,nrows; REAL *Real_Array, *Imag_Array;
{ REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;
  long nrows_power;
  long i;

  for (nrows_power=0, i=nrows; i>1; nrows_power++) { /* FIND/CHECK POWERS OF ROWS */
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2; }
  Primitive_GC_If_Needed(nrows*3*REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;
  g2 = Work_Here + nrows;
  w1 = Work_Here + (nrows<<1);
  w2 = Work_Here + (nrows<<1) + (nrows>>1);
  Make_FFT_Tables(w1, w2, nrows, flag);	/* MAKE TABLES */
  for (i=0;i<nrows;i++) {	/* ROW-WISE */
    f1 = Real_Array + (i*nrows);
    f2 = Imag_Array + (i*nrows);
    C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);
  }
  Image_Fast_Transpose(Real_Array, nrows); /* MUST TRANSPOSE (1) order of frequencies. (2) read columns. */
  Image_Fast_Transpose(Imag_Array, nrows);
  
  for (i=0;i<nrows;i++) {	/* COLUMN-WISE */
    f1 = Real_Array + (i*nrows);
    f2 = Imag_Array + (i*nrows);
    C_Array_FFT_With_Given_Tables(flag, nrows_power, nrows, f1,f2,g1,g2,w1,w2);	/* ncols=nrows... Twiddles... */
  }
  Image_Fast_Transpose(Real_Array, nrows); /* TRANSPOSE BACK: order of frequencies. */
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
  Make_FFT_Tables(w1, w2, ndeps, flag);                      /* MAKE TABLES */
  
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


/*----------------------- scheme primitives ------------------------- */

/* Real and Imag arrays must be different.
   Arg1=1 --> forward FFT, otherwise backward.
   */

DEFINE_PRIMITIVE ("ARRAY-FFT!", Prim_array_fft, 3, 3, 0)
{ long length, power, flag, i;
  Pointer answer;
  REAL *f1,*f2,*g1,*g2,*w1,*w2;
  REAL *Work_Here;
  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);	/* flag */   
  Arg_2_Type(TC_ARRAY);		/* real */
  Arg_3_Type(TC_ARRAY);		/* imag */
  Set_Time_Zone(Zone_Math);

  flag = Get_Integer(Arg1);
  length = Array_Length(Arg2);
  if (length != (Array_Length(Arg3))) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  power=0;
  for (power=0, i=length; i>1; power++) {
    if ( (i % 2) == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
    i=i/2; }
  
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

DEFINE_PRIMITIVE ("ARRAY-CZT", Prim_array_czt, 5, 5, 0)
{ double phi,rho;
  long N,M,L;
  long log2_L,maxMN,smallest_power_of_2_ge(), allocated_cells;
  int Error_Number, Scheme_Number_To_Double();
  Pointer answer, answer_1,answer_2;
  REAL *f1,*f2,*fo1,*fo2, *g1,*g2, *fft_w1,*fft_w2,*czt_w1,*czt_w2;
  REAL *Work_Here;
  Primitive_5_Args();
  Arg_3_Type(TC_FIXNUM);
  Arg_4_Type(TC_ARRAY);		/* real */
  Arg_5_Type(TC_ARRAY);		/* imag */
  Set_Time_Zone(Zone_Math);
  
  Error_Number = Scheme_Number_To_Double(Arg1, &phi); /* phi=starting point [0,1]*/
  if (Error_Number == 1) Primitive_Error(ERR_ARG_1_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_1_WRONG_TYPE);
  Error_Number = Scheme_Number_To_Double(Arg2, &rho); /* rho=resolution [0,1] */
  if (Error_Number == 1) Primitive_Error(ERR_ARG_2_BAD_RANGE);
  if (Error_Number == 2) Primitive_Error(ERR_ARG_2_WRONG_TYPE);

  M   = Get_Integer(Arg3);	/* M=length of output data */
  N   = Array_Length(Arg4);	/* N=length of input data */
  if (N != (Array_Length(Arg5))) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if ((M+N-1)<4) Primitive_Error(ERR_ARG_3_BAD_RANGE);
  log2_L = smallest_power_of_2_ge(M+N-1);
  L  = 1<<log2_L;		/* length of intermediate computation arrays */
  maxMN = max(M,N);		/* length of czt tables */
  
  Primitive_GC_If_Needed( ((7*L) + (2*maxMN)) * REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;		/* We will allocate answer arrays here, after czt is done. */
  g2 = Work_Here + L;
  fo1 = Work_Here + (2*L);
  fo2 = Work_Here + (3*L);
  fft_w1 = Work_Here + (4*L);
  fft_w2 = Work_Here + (4*L) + (L/2); /* length of fft tables = L/2*/
  czt_w1 = Work_Here + (5*L);
  czt_w2 = Work_Here + (5*L) + maxMN;
  f1 = Work_Here + (5*L) + (2*maxMN); /* CZT stores its results here */
  f2 = Work_Here + (6*L) + (2*maxMN);
  
  C_Array_Copy( (Scheme_Array_To_C_Array(Arg4)), f1, N);
  C_Array_Copy( (Scheme_Array_To_C_Array(Arg5)), f2, N);
  C_Array_CZT(phi,rho, N,M,log2_L, f1,f2,fo1,fo2, g1,g2, fft_w1,fft_w2,czt_w1,czt_w2);
  
  Allocate_Array(answer_1, M, allocated_cells);	/* Overwrite g1,g2 which started at Free */
  Allocate_Array(answer_2, M, allocated_cells);
  C_Array_Copy(f1, (Scheme_Array_To_C_Array(answer_1)), M);
  C_Array_Copy(f2, (Scheme_Array_To_C_Array(answer_2)), M);
  
  Primitive_GC_If_Needed(4);
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = answer_1;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = answer_2;
  *Free++ = NIL;
  return answer;
}

DEFINE_PRIMITIVE ("ARRAY-2D-FFT!", Prim_array_2d_fft, 5, 5, 0)
{ long flag;
  Pointer answer;
  REAL *Real_Array, *Imag_Array;
  long Length, nrows, ncols;
  
  Primitive_5_Args();
  Arg_1_Type(TC_FIXNUM);	/* flag */   
  Range_Check(nrows, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg3, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_4_Type(TC_ARRAY);		/* real image */
  Arg_5_Type(TC_ARRAY);		/* imag image */
  Set_Time_Zone(Zone_Math);	/* for timing */

  flag = Get_Integer(Arg1);
  Length = Array_Length(Arg4);
  if (Length != (nrows*ncols)) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  if (Length != (Array_Length(Arg5))) Primitive_Error(ERR_ARG_5_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg4);
  Imag_Array = Scheme_Array_To_C_Array(Arg5);
  if (Real_Array==Imag_Array) Primitive_Error(ERR_ARG_5_WRONG_TYPE);
  
  C_Array_2D_FFT_In_Scheme_Heap(flag, nrows, ncols, Real_Array, Imag_Array);

  Primitive_GC_If_Needed(4);	/* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg4;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg5;
  *Free++ = NIL;
  return answer;
}

DEFINE_PRIMITIVE ("ARRAY-3D-FFT!", Prim_array_3d_fft, 6, 6, 0)
{ long flag;
  Pointer answer;
  REAL *Real_Array, *Imag_Array;
  long Length, ndeps, nrows, ncols;
  
  Primitive_6_Args();
  Arg_1_Type(TC_FIXNUM);	/* flag */   
  Range_Check(ndeps, Arg2, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(nrows, Arg3, 1, 512, ERR_ARG_2_BAD_RANGE);
  Range_Check(ncols, Arg4, 1, 512, ERR_ARG_3_BAD_RANGE);
  Arg_5_Type(TC_ARRAY);		/* real image */
  Arg_6_Type(TC_ARRAY);		/* imag image */
  Set_Time_Zone(Zone_Math);	/* for timing */

  Sign_Extend(Arg1, flag);	/* should be 1 or -1 */
  Length = Array_Length(Arg5);
  if (Length != (ndeps*nrows*ncols)) Primitive_Error(ERR_ARG_6_BAD_RANGE);
  if (Length != (Array_Length(Arg6))) Primitive_Error(ERR_ARG_6_BAD_RANGE);
  Real_Array = Scheme_Array_To_C_Array(Arg5);
  Imag_Array = Scheme_Array_To_C_Array(Arg6);
  if (Real_Array==Imag_Array) Primitive_Error(ERR_ARG_6_WRONG_TYPE);

  C_Array_3D_FFT_In_Scheme_Heap(flag, ndeps, nrows, ncols, Real_Array, Imag_Array);

  Primitive_GC_If_Needed(4);	/* NOW RETURN ANSWERS */
  answer = Make_Pointer(TC_LIST, Free);
  *Free++ = Arg5;
  *Free = Make_Pointer(TC_LIST, Free+1);
  Free += 1;
  *Free++ = Arg6;
  *Free++ = NIL;
  return answer;
}

