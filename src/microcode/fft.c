/* -*-C-*-

$Id: fft.c,v 9.37 2007/01/05 21:19:25 cph Exp $

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

/* Time-Frequency Transforms (pas) */

#include "scheme.h"
#include "prims.h"
#include "zones.h"
#include <math.h>
#include "array.h"
#include "image.h"

/* SUMMARY
   - pas_cft  (complex data, DIF, split-radix)
   - pas_rft  (real data,    DIT, split-radix) output is conjugate-symmetric
   - pas_csft (cs data,      DIF, split-radix) output is real
   - pas_cft
   - pas_rft2d
   - pas_csft2d


   Stuff before 4-15-1989
   - C_Array_FFT  (complex data, radix=2, NOT-in-place)
   - CZT (chirp-z-transform) uses the old cft (hence slow).
   - 2d DFT
   */

/* The DFT is as defined in Siebert 6003 book,
   i.e.
   forward DFT   =  Negative exponent and division by N
   backward DFT  =  Positive exponent
   (note Seibert forward DFT is Oppenheim backward DFT)
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

DEFINE_PRIMITIVE ("PAS-CFT!", Prim_pas_cft, 5, 5, 0)
{ long i, length, power, flag;
  REAL *f1,*f2,  *wcos,*w3cos,*w3sin;
  void pas_cft();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (2, ARRAY_P);	/* real part */
  CHECK_ARG (3, ARRAY_P);	/* imag part */
  CHECK_ARG (4, ARRAY_P);	/* twiddle tables, total length = 3*(length/4)  */
  CHECK_ARG (5, FIXNUM_P);	/* (1)=tables precomputed, else recompute */

  flag = (arg_integer (1));
  length = ARRAY_LENGTH(ARG_REF(2));
  if (length != (ARRAY_LENGTH(ARG_REF(3)))) error_bad_range_arg(2);

  for (power=0, i=length; i>1; power++)
  { if ( (i % 2) == 1) error_bad_range_arg(2);
    i=i/2; }

  f1 = ARRAY_CONTENTS(ARG_REF(2));
  f2 = ARRAY_CONTENTS(ARG_REF(3));
  if (f1==f2) error_wrong_type_arg(2);

  wcos = ARRAY_CONTENTS(ARG_REF(4)); /* twiddle tables */
  if (ARRAY_LENGTH(ARG_REF(4)) != (3*length/4)) error_bad_range_arg(4);
  w3cos = wcos  + length/4;
  w3sin = w3cos + length/4;
  if ((arg_nonnegative_integer(5)) == 1)
    pas_cft(1, flag, f1,f2, length, power, wcos,w3cos,w3sin);
  else
    pas_cft(0, flag, f1,f2, length, power, wcos,w3cos,w3sin);
  /*        1 means tables are already made
	    0 means compute new tables */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PAS-CFT-MAKE-TWIDDLE-TABLES!",
		  Prim_pas_cft_make_twiddle_tables, 2, 2, 0)
{ long length, power, i;
  REAL  *wcos,*w3cos,*w3sin;
  void pas_cft_make_twiddle_tables_once();
  PRIMITIVE_HEADER (2);

  length = arg_nonnegative_integer(1); /* length of cft that we intend to compute */
  CHECK_ARG (2, ARRAY_P);	/*        storage for twiddle tables    */
  if (ARRAY_LENGTH(ARG_REF(2)) != (3*length/4)) error_bad_range_arg(2);

  power=0;
  for (power=0, i=length; i>1; power++)
  { if ( (i % 2) == 1) error_bad_range_arg(1);
    i=i/2; }

  wcos = ARRAY_CONTENTS(ARG_REF(2)); /* twiddle tables */
  w3cos = wcos  + length/4;
  w3sin = w3cos + length/4;
  pas_cft_make_twiddle_tables_once(length, power, wcos,w3cos,w3sin);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*
  C COMPLEX FOURIER TRANSFORM  (Split-Radix, Decimation-in-frequency)
  C (adapted and optimized from Sorensen,et.al. ASSP-34 no.1 page 152,  February 1986)
  */

/* Twiddle Tables for PAS_CFT;
   (tables for forward transform only)
   Inverse transform === forward CFT (without 1/N scaling) followed by time-reversal.
   /
   The tables contain  (2pi/N)*i  for  i=0,1,2,..,N/4
   (except i=0 is ignored, never used)
   /
   Table for wsin[i] is not needed because wsin[i]=wcos[n4-i].
   Table for w3sin[i] is needed however.  The previous relationship does not work for w3sin.
   */

/* There are two routines for making twiddle tables:
   a fast one, and a slower one but more precise.
   The differences in speed and accuracy are actually rather small, but anyway.
   Use the slow one for making permanent tables.
   */

void
pas_cft_make_twiddle_tables (n,m, wcos,w3cos,w3sin) /* efficient version */
     REAL *wcos, *w3cos, *w3sin;
     long n,m;
{ long i, n4;
  double tm;
  REAL costm,sintm;
  n4 = n/4;
  for (i=1; i<n4; i++)		/* start from table entry 1 */
  { tm = 6.283185307179586476925287 * (((double) i) / ((double) n));
    wcos[i] = (REAL) cos(tm);
  }
  for (i=1; i<n4; i++)
  { costm = wcos[i];
    sintm = wcos[n4-i];
    w3cos[i] = costm * (1 - 4*sintm*sintm); /* see my notes */
    w3sin[i] = sintm * (4*costm*costm - 1);
  }
}

void
pas_cft_make_twiddle_tables_once (n,m, wcos,w3cos,w3sin) /* slow version, more accurate */
     REAL *wcos, *w3cos, *w3sin;
     long n,m;
{ long i, n4;
  double tm;
  REAL costm,sintm;
  n4 = n/4;
  for (i=1; i<n4; i++)		/* start from table entry 1 */
  { tm = 6.283185307179586476925287 * (((double) i) / ((double) n));
    wcos[i] = (REAL) cos(tm);
    tm = tm * 3.0;		/* this is more precise (in the 16th decimal) than */
    w3cos[i] = (REAL) cos(tm);	/* the more efficient version. (I tested by for/backward) */
    w3sin[i] = (REAL) sin(tm);
  }
}

void
pas_cft (tables_ok,flag, x,y,n,m, wcos,w3cos,w3sin)
     REAL *x,*y, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale;
  long i,j;
  void pas_cft_make_twiddle_tables();
  void C_Array_Time_Reverse();
  void pas_cft_forward_loop();

  if (tables_ok != 1) 		/* 1 means = tables already made */
    pas_cft_make_twiddle_tables(n,m, wcos,w3cos,w3sin);

  if (flag == 1)		/* forward cft */
  { pas_cft_forward_loop(x,y,n,m, wcos,w3cos,w3sin);
    scale = (REAL) (1.0 / ((double) n));
    for (i=0; i<n; i++)
    { x[i] = x[i]*scale;
      y[i] = y[i]*scale; }}
  else				/* backward cft */
  { for (j=0; j<n; j++) y[j] = (-y[j]); /* conjugate before */
    pas_cft_forward_loop(x,y,n,m, wcos,w3cos,w3sin);
    for (j=0; j<n; j++) y[j] = (-y[j]);	/* conjugate after */
  }
}

void
pas_cft_forward_loop (x,y,n,m, wcos,w3cos,w3sin)    /* n >= 4 */
     REAL *x,*y, *wcos,*w3cos,*w3sin;
     long n,m;
{ /* REAL  a,a3,e;  no need anymore, use tables */
  REAL    r1,r2,s1,s2,s3,  xt,    cc1,cc3,ss1,ss3;
  long  n1,n2,n4,   i,j,k,    is,id, i0,i1,i2,i3;
  long windex0, windex, windex_n4; /* indices for twiddle tables */
  /********** fortran indices start from 1,... **/
  x = x-1;			/* TRICK---- x(0) is now illegal, but x(1) and x(n) are valid */
  y = y-1;
  /********** fortran indices start from 1,... **/
  /* c */
  /* c-----first M-1 stages of transform */
  /* c */
  windex_n4 = n/4;		/* need for indexing sin via wcos twiddle table */
  n2 = 2*n;
  for (k=1; k<m; k++)		/*  DO 10 K = 1, M-1 */
  { n2 = n2>>1;			/* n2 = n2/2; */
    n4 = n2>>2;			/* n4 = n2/4; */
    /* e = 6.283185307179586476925287 / ((REAL) n2);    no need anymore, use tables */
    /* a = 0.0; */
  {
    /* j=1;  */
    /* The first iteration in the loop "DO 20 J = 1, N4"
       is done specially to save operations involving sin=0, cos=1  */
    /* a = j*e;	                     no need anymore, use tables */
    is = 1;			/* is = j; */
    id = 2*n2;
    label40first:
    for (i0=is; i0<n; i0=i0+id) /*  40         DO 30 I0 = IS,N-1,ID */
    { i1 = i0 + n4;
      i2 = i1 + n4;
      i3 = i2 + n4;
      /*	c     */
      r1    = x[i0] - x[i2];
      x[i0] = x[i0] + x[i2];
      r2    = x[i1] - x[i3];
      x[i1] = x[i1] + x[i3];
      s1    = y[i0] - y[i2];
      y[i0] = y[i0] + y[i2];
      s2    = y[i1] - y[i3];
      y[i1] = y[i1] + y[i3];
      /*	c     */
      s3    = r1 - s2;
      r1    = r1 + s2;
      s2    = s1 - r2;		/* original used to be s2 = r2 - s1; */
      r2    = r2 + s1;
      x[i2] =   r1;
      y[i2] =   s2;		/* used to be y[i2] =  (-s2); */
      x[i3] =   s3;
      y[i3] =   r2;
      /* x[i2] =   r1*cc1 + s2*ss1;   used to be, see below
	 y[i2] =   s2*cc1 - r1*ss1;   used to be, see below, inside the DO 20 J=1,N4
	 x[i3] =   s3*cc3 + r2*ss3;
	 y[i3] =   r2*cc3 - s3*ss3; */
    }				/* 30         CONTINUE */
    is = 2*id - n2 + 1;		/* is = 2*id - n2 + j; */
    id = 4*id;
    if (is < n) goto label40first; /* IF (IS.LT.N) GOTO 40 */
  }
    /*  c  */
    windex0 = 1<<(k-1);
    windex  = windex0;
    for (j=2; j<=n4; j++)	/* DO 20 J = 1, N4 */
    {
      /* windex = (j-1)*(1<<(k-1)); -- done with trick to avoid (j-1) and 1<<(k-1) */
      cc1 = wcos[windex];
      ss1 = wcos[windex_n4 - windex]; /* see my notes */
      cc3 = w3cos[windex];
      ss3 = w3sin[windex];	/* sin-from-cos trick does not work here */
      windex = j*windex0;	/* same trick as "a = j*e" */
      /* a3 = 3*a;
	 cc1 = cos(a);
	 ss1 = sin(a);
	 cc3 = cos(a3);
	 ss3 = sin(a3);
	 a = j*e;*/
      is = j;
      id = 2*n2;
      label40:
      for (i0=is; i0<n; i0=i0+id) /*  40         DO 30 I0 = IS,N-1,ID */
      { i1 = i0 + n4;
	i2 = i1 + n4;
	i3 = i2 + n4;
	/*	c     */
	r1    = x[i0] - x[i2];
	x[i0] = x[i0] + x[i2];
	r2    = x[i1] - x[i3];
	x[i1] = x[i1] + x[i3];
	s1    = y[i0] - y[i2];
	y[i0] = y[i0] + y[i2];
	s2    = y[i1] - y[i3];
	y[i1] = y[i1] + y[i3];
	/*	c     */
	s3    = r1 - s2;
	r1    = r1 + s2;
	s2    = s1 - r2;	/* original used to be s2 = r2 - s1; */
	r2    = r2 + s1;
	x[i2] =   r1*cc1 + s2*ss1; /* used to be x[i2] =   r1*cc1 - s2*ss1;  */
	y[i2] =   s2*cc1 - r1*ss1; /* used to be y[i2] = (-s2*cc1 - r1*ss1); */
	x[i3] =   s3*cc3 + r2*ss3;
	y[i3] =   r2*cc3 - s3*ss3;
      }				/* 30         CONTINUE */
      is = 2*id - n2 + j;
      id = 4*id;
      if (is < n) goto label40; /* IF (IS.LT.N) GOTO 40 */
    }				/* 20      CONTINUE */
  }				/* 10   CONTINUE */
  /* c
     c-----------last-stage, length-2 butterfly ----------------c
     c  */
  is = 1;
  id = 4;
  label50:
  for (i0=is; i0<=n; i0=i0+id)	/* 50   DO 60 I0 = IS, N, ID  */
  { i1    = i0 + 1;
    r1    = x[i0];
    x[i0] = r1 + x[i1];
    x[i1] = r1 - x[i1];
    r1    = y[i0];
    y[i0] = r1 + y[i1];
    y[i1] = r1 - y[i1];
  }				/* 60   CONTINUE */
  is = 2*id - 1;
  id = 4*id;
  if (is < n) goto label50;	/* IF (IS.LT.N) GOTO 50 */
  /*
    c
    c-----------bit-reverse-counter---------------c
    */
  label100:
  j = 1;
  n1 = n - 1;
  for (i=1; i<=n1; i++)		/* DO 104 I = 1, N1 */
  { if (i >= j) goto label101;	/* if (i .ge. j) goto 101 */
    xt = x[j];
    x[j] = x[i];
    x[i] = xt;
    xt = y[j];
    y[j] = y[i];
    y[i] = xt;
    label101: k = n>>1;		/* k = n/2; */
    label102: if (k>=j) goto label103;
    j = j - k;
    k = k>>1;			/* k = k/2; */
    goto label102;
    label103: j = j + k;
  }				/* 104  CONTINUE */
  /* c-------------------------------------*/
  /* c */
}				/* RETURN ^M END */

DEFINE_PRIMITIVE ("PAS-RFT-CSFT!", Prim_pas_rft_csft, 5, 5, 0)
{ long i, length, power, flag, ft_type;
  REAL *f1,  *wcos,*w3cos,*w3sin;
  void pas_rft(), pas_csft();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (2, ARRAY_P);	/* Input data (real or cs) */
  CHECK_ARG (3, ARRAY_P);	/* Twiddle tables, total length = 4*(length/8)  */
  CHECK_ARG (4, FIXNUM_P);	/* (1)=tables precomputed, else recompute */
  CHECK_ARG (5, FIXNUM_P);	/* ft_type = 1 or 3
				   1 means compute rft, 3 means compute csft */
  flag = (arg_integer (1));
  f1   = ARRAY_CONTENTS(ARG_REF(2));
  length = ARRAY_LENGTH(ARG_REF(2));
  for (power=0, i=length; i>1; power++)
  { if ( (i % 2) == 1) error_bad_range_arg(2);
    i=i/2; }

  wcos = ARRAY_CONTENTS(ARG_REF(3)); /* twiddle tables */
  if (ARRAY_LENGTH(ARG_REF(3)) != (4*length/8)) error_bad_range_arg(3);
  w3cos = wcos + (length/4);
  w3sin = w3cos + (length/8);

  ft_type = (arg_nonnegative_integer(5)); /*         rft or csft */
  if (ft_type == 1) {
    if ((arg_nonnegative_integer(4)) == 1)
      pas_rft     (1, flag, f1, length, power, wcos,w3cos,w3sin);
    else pas_rft  (0, flag, f1, length, power, wcos,w3cos,w3sin);
  }
  else if (ft_type == 3) {
    if ((arg_nonnegative_integer(4)) == 1)
      pas_csft    (1, flag, f1, length, power, wcos,w3cos,w3sin);
    else pas_csft (0, flag, f1, length, power, wcos,w3cos,w3sin);
    /*             1 means tables are already made
		   0 means compute new tables */
  }
  else error_bad_range_arg(5);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PAS-REALDATA-MAKE-TWIDDLE-TABLES!",
		  Prim_pas_realdata_make_twiddle_tables, 2, 2, 0)
{ long length, power, i;
  REAL  *wcos,*w3cos,*w3sin;
  void pas_realdata_make_twiddle_tables_once();
  PRIMITIVE_HEADER (2);

  length = arg_nonnegative_integer(1); /* length of rft that we intend to compute */
  CHECK_ARG (2, ARRAY_P);	/*        storage for twiddle tables    */
  if (ARRAY_LENGTH(ARG_REF(2)) != (4*length/8)) error_bad_range_arg(2);

  power=0;
  for (power=0, i=length; i>1; power++)
  { if ( (i % 2) == 1) error_bad_range_arg(1);
    i=i/2; }

  wcos = ARRAY_CONTENTS(ARG_REF(2)); /* twiddle tables */
  w3cos = wcos +  length/4;
  w3sin = w3cos + length/8;
  pas_realdata_make_twiddle_tables_once(length, power, wcos,w3cos,w3sin);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*
  C REAL FOURIER TRANSFORM  (Split-Radix, Decimation-in-time)
  C (adapted from Sorensen,et.al. ASSP-35 no.6 page 849,  October 1986)
  C
  C the output is [Re(0),Re(1),...,Re(n/2), Im(n/2-1),...,Im(1)]
  */

/* Twiddle Tables for PAS_RFT and PAS_CSFT
   are identical. -> pas_realdata_make_twiddle_tables
   (but they are indexed differently in each case)
   /
   The tables contain  (2pi/N)*i  where i=0,1,2,..,N/4     for wcos
                  and  (2pi/N)*i  where i=0,1,2,..,N/8     for w3cos w3sin
   (except i=0 is ignored, never used)
   /
   Table for wsin[i] is not needed because wsin[i]=wcos[n4-i].
   Table for w3sin[i] is needed however.  The previous relationship does not work for w3sin.
   /
   Instead of getting sin() from   a wsin[i] table with i=1,..,N/8
   we get it from wcos[n4-i].
   This way we can use a CFT table which goes up to N/4
   for RFT CSFT also. We do so in image-processing (rft2d-csft2d).
   */

/* There are two routines for making twiddle tables:
   a fast one, and a slower one but more precise.
   The differences in speed and accuracy are actually rather small, but anyway.
   Use the slow one for making tables that stay around.
   */

void pas_realdata_make_twiddle_tables(n,m, wcos,w3cos,w3sin)  /* efficient version */
     REAL *wcos, *w3cos, *w3sin;
     long n,m;
{ long i, n4, n8;
  double tm;
  REAL costm,sintm;
  n4 = n/4;
  n8 = n/8;
  for (i=1; i<n4; i++)		/* start from table entry 1 */
  { tm = 6.283185307179586476925287 * (((double) i) / ((double) n));
    wcos[i] = (REAL) cos(tm);
  }
  for (i=1; i<n8; i++)
  { costm = wcos[i];
    sintm = wcos[n4-i];
    w3cos[i] = costm * (1 - 4*sintm*sintm); /* see my notes */
    w3sin[i] = sintm * (4*costm*costm - 1);
  }
}

void pas_realdata_make_twiddle_tables_once(n,m, wcos,w3cos,w3sin) /* slow version, more accurate */
     REAL *wcos, *w3cos, *w3sin;
     long n,m;
{ long i, n4, n8;
  double tm;
  REAL costm,sintm;
  n4 = n/4;
  n8 = n/8;
  for (i=1; i<n8; i++)		/* start from table entry 1 */
  { tm = 6.283185307179586476925287 * (((double) i) / ((double) n));
    wcos[i] = (REAL) cos(tm);
    tm = tm * 3.0;		/* this is more precise (in the 16th decimal) than */
    w3cos[i] = (REAL) cos(tm);	/* the more efficient version. (I tested by for/backward) */
    w3sin[i] = (REAL) sin(tm);
  }
  for (i=n8; i<n4; i++)
  { tm = 6.283185307179586476925287 * (((double) i) / ((double) n));
    wcos[i] = (REAL) cos(tm);
  }
}

void pas_rft(tables_ok,flag, x,n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale;
  long i;
  void pas_realdata_make_twiddle_tables();
  void pas_rft_forward_loop();

  if (tables_ok != 1)		/* 1 means = tables already made */
    pas_realdata_make_twiddle_tables(n,m, wcos,w3cos,w3sin);

  pas_rft_forward_loop(x,n,m, wcos,w3cos,w3sin);

  if (flag == 1)		/* forward rft */
  { scale = (REAL) (1.0 / ((double) n));
    for (i=0; i<n; i++)         x[i] = x[i] * scale; }
  else				/* backward rft */
    for (i=((n/2)+1); i<n; i++) x[i] = (-x[i]); /* time-reverse cs-array */
}

/* rft
   forward transform === forward_loop + 1/N scaling
   inverse transform === forward_loop + time-reversal (without 1/N scaling)
   */

/* wcos           must be length n/4
   w3cos, w3sin   must be length n/8
   (greater than n/8 is fine also, e.g. use cft tables)
   */

void pas_rft_forward_loop(x,n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m;
{ /* REAL   a,a3,e;      no need anymore, use tables */
  REAL   r1, xt,    cc1,cc3,ss1,ss3, t1,t2,t3,t4,t5,t6;
  long n1,n2,n4,n8,  i,j,k,  is,id,   i0,i1,i2,i3,i4,i5,i6,i7,i8;
  long windex0, windex, windex_n4; /* indices for twiddle tables */
  /********** fortran indices start from 1,... **/
  x = x-1;			/* TRICK---- x(0) is now illegal, but x(1) and x(n) are valid */
  /********** fortran indices start from 1,... **/
  /* c */
  windex_n4 = n/4;		/* need for indexing sin via wcos twiddle table */
  /* c
     c-----------bit-reverse-counter---------------c
     */
  label100:
  j = 1;
  n1 = n - 1;
  for (i=1; i<=n1; i++)		/* DO 104 I = 1, N1 */
  { if (i >= j) goto label101;	/* if (i .ge. j) goto 101 */
    xt = x[j];
    x[j] = x[i];
    x[i] = xt;
    label101: k = n>>1;		/* k = n/2; */
    label102: if (k>=j) goto label103;
    j = j - k;
    k = k>>1;			/* k = k/2; */
    goto label102;
    label103: j = j + k;
  }				/* 104  CONTINUE */
  /* c-------------------------------------*/
  /* c */
  /* c  ----length-two-butterflies----------- */
  is = 1;
  id = 4;
  label70:
  for (i0=is; i0<=n; i0=i0+id)  /*  70   DO 60 I0 = IS,N,ID */
  { i1    = i0 + 1;
    r1    = x[i0];
    x[i0] = r1 + x[i1];
    x[i1] = r1 - x[i1];
  }				/* 60   CONTINUE */
  is = 2*id - 1;
  id = 4*id;
  if (is < n) goto label70;	/* IF (IS.LT.N) GOTO 70 */
  /* C
     C -------L-shaped-butterflies-------- */
  n2 = 2;
  for (k=2; k<=m; k++)		/* DO 10 K = 2,M */
  { n2 = n2 * 2;
    n4 = n2>>2;			/* n4 = n2/4; */
    n8 = n2>>3;			/* n8 = n2/8; */
    /* e = 6.283185307179586476925287 / ((REAL) n2);   no need anymore, use tables */
    is = 0;
    id = n2 * 2;
    label40:
    for (i=is; i<n; i=i+id)	/* 40      DO 38 I = IS,N-1,ID */
    { i1 = i + 1;
      i2 = i1 + n4;
      i3 = i2 + n4;
      i4 = i3 + n4;
      t1 = x[i4] + x[i3];
      x[i4] = x[i4] - x[i3];
      x[i3] = x[i1] - t1;
      x[i1] = x[i1] + t1;
      if (n4 == 1) goto label38; /* IF (N4.EQ.1) GOTO 38 */
      i1 = i1 + n8;
      i2 = i2 + n8;
      i3 = i3 + n8;
      i4 = i4 + n8;
      /* t1    = (x[i3] + x[i4]) / sqrt(2.0); -- this is more precise, it uses extended
	 t2    = (x[i3] - x[i4]) / sqrt(2.0); -- precision inside 68881, but slower */
      t1    = (x[i3] + x[i4]) * ONE_OVER_SQRT_2;
      t2    = (x[i3] - x[i4]) * ONE_OVER_SQRT_2;
      x[i4] = x[i2] - t1;
      x[i3] = -x[i2] - t1;
      x[i2] = x[i1] - t2;
      x[i1] = x[i1] + t2;
      label38:			/* 38      CONTINUE */
      ;
    }
    is = 2*id - n2;
    id = 4*id;
    if (is < n) goto label40;	/* IF (IS.LT.N) GOTO 40 */
    /* a = e; */
    windex0 = 1<<(m-k);
    windex  = windex0;
    for (j=2; j<=n8; j++)	/* DO 32 J = 2,N8 */
    {
      /* windex = (j-1)*(1<<(m-k)); -- done with trick to avoid (j-1) and 1<<(m-k) */
      cc1 = wcos[windex];
      ss1 = wcos[windex_n4 - windex]; /* sin-from-cos trick: see my notes */
      cc3 = w3cos[windex];
      ss3 = w3sin[windex];	/* sin-from-cos trick does not work here */
      windex = j*windex0;	/* same trick as "a = j*e" */
      /* a3 = 3*a;
	 cc1 = cos(a);
	 ss1 = sin(a);
	 cc3 = cos(a3);
	 ss3 = sin(a3);
	 a = j*e;*/
      is = 0;
      id = 2*n2;
      label36:			/*  36         DO 30 I = IS,N-1,ID */
      for (i=is; i<n; i=i+id)
      { i1 = i + j;
	i2 = i1 + n4;
	i3 = i2 + n4;
	i4 = i3 + n4;
	i5 = i  + n4 - j + 2;
	i6 = i5 + n4;
	i7 = i6 + n4;
	i8 = i7 + n4;
	t1 = x[i3]*cc1 + x[i7]*ss1;
	t2 = x[i7]*cc1 - x[i3]*ss1;
	t3 = x[i4]*cc3 + x[i8]*ss3;
	t4 = x[i8]*cc3 - x[i4]*ss3;
	t5 = t1 + t3;
	t6 = t2 + t4;
	t3 = t3 - t1;		/* t3 = t1 - t3; */
	t4 = t2 - t4;
	x[i8] = x[i6] + t6;
	x[i3] = t6    - x[i6];
	x[i4] = x[i2] + t3;	/* x[i4] = x[i2] - t3; */
	x[i7] = t3 - x[i2];	/* x[i7] = -x[i2] - t3; */
	x[i6] = x[i1] - t5;
	x[i1] = x[i1] + t5;
	x[i2] = x[i5] + t4;
	x[i5] = x[i5] - t4;
      }				/* 30         CONTINUE */
      is = 2*id - n2;
      id = 4*id;
      if (is < n) goto label36; /* IF (IS.LT.N) GOTO 36 */
    }				/* 32      CONTINUE */
  }				/* 10   CONTINUE */
}				/* RETURN ^M END */


/*
  C CONJUGATE SYMMETRIC FOURIER TRANSFORM  (Split-Radix, Decimation-in-time)
  C (adapted from Sorensen,et.al. ASSP-35 no.6 page 849,  October 1986)
  C
  C input is [Re(0),Re(1),...,Re(n/2), Im(n/2-1),...,Im(1)]
  C output is real
  */

/* twiddle tables identical with rft
   for comments see rft */

void pas_csft(tables_ok,flag, x,n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale;
  long i,n2;
  void pas_realdata_make_twiddle_tables();
  void C_Array_Time_Reverse();
  void pas_csft_backward_loop();

  if (tables_ok != 1)		/* 1 means = tables already made */
    pas_realdata_make_twiddle_tables(n,m, wcos,w3cos,w3sin);

  if (flag == 1)		/* forward csft */
  { n2 = n/2;
    scale = (REAL) (1.0 / ((double) n));
    for (i=0; i<=n2; i++)   x[i] = x[i]*scale;
    scale = (-scale);
    for (i=n2+1; i<n; i++)  x[i] = x[i]*scale; /* scale and conjugate cs-array */
    pas_csft_backward_loop(x,n,m, wcos,w3cos,w3sin);
  }
  else				/* backward csft */
    pas_csft_backward_loop(x,n,m, wcos,w3cos,w3sin);
}

/* csft
   forward transform === backward_loop + 1/N scaling + time-reversal
   inverse transform === backward_loop
   */

/* wcos           must be length n/4
   w3cos, w3sin   must be length n/8
   (greater than n/8 is fine also, e.g. use cft tables)
   */

void pas_csft_backward_loop(x,n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m;
{ /* REAL   a,a3,e;     no need anymore, use tables */
  REAL   r1, xt,    cc1,cc3,ss1,ss3, t1,t2,t3,t4,t5;
  long n1,n2,n4,n8,  i,j,k,  is,id,   i0,i1,i2,i3,i4,i5,i6,i7,i8;
  long windex0, windex, windex_n4; /* indices for twiddle tables */
  /********** fortran indices start from 1,... **/
  x = x-1;			/* TRICK---- x(0) is now illegal, but x(1) and x(n) are valid */
  /********** fortran indices start from 1,... **/
  /* c */
  windex_n4 = n/4;		/* need for indexing sin via wcos twiddle table */
  /* c */
  /* c */
  /* c
     c -------L-shaped-butterflies-------- */
  n2 = 2*n;
  for (k=1; k<m; k++)		/* do 10 k = 1,m-1 */
  { is = 0;
    id = n2;
    n2 = n2>>1;			/* n2 = n2/2; */
    n4 = n2>>2;			/* n4 = n2/4; */
    n8 = n4>>1;			/* n8 = n4/2; */
    /* e = 6.283185307179586476925287 / ((REAL) n2);  no need anymore, use tables */
    label17:
    for (i=is; i<n; i=i+id)	/*  17      do 15 i = is,(n-1),id */
    { i1 = i + 1;
      i2 = i1 + n4;
      i3 = i2 + n4;
      i4 = i3 + n4;
      t1    = x[i1] - x[i3];
      x[i1] = x[i1] + x[i3];
      x[i2] = 2*x[i2];
      x[i3] = t1  - 2*x[i4];
      x[i4] = t1  + 2*x[i4];
      if (n4 == 1) goto label15; /* if (n4.eq.1) goto 15 */
      i1 = i1 + n8;
      i2 = i2 + n8;
      i3 = i3 + n8;
      i4 = i4 + n8;
      t1    =   (x[i2] - x[i1])  * ONE_OVER_SQRT_2;
      t2    = (-(x[i4] + x[i3])) * ONE_OVER_SQRT_2;
      /* t1    = (x[i2] - x[i1])/sqrt(2.0);
	 t2    = (x[i4] + x[i3])/sqrt(2.0); */
      x[i1] = x[i1] + x[i2];
      x[i2] = x[i4] - x[i3];
      x[i3] = 2 * (t2-t1);	/* x[i3] = 2 * (-t2-t1); */
      x[i4] = 2 * (t2+t1);	/* x[i4] = 2 * (-t2+t1); */
      label15:
      ;
    }				/* 15      continue */
    is = 2*id - n2;
    id = 4*id;
    if (is < (n-1)) goto label17; /* if (is.lt.(n-1)) goto 17 */
    /* a = e; */
    windex0 = 1<<(k-1);		/* see my notes */
    windex  = windex0;
    for (j=2; j<=n8; j++)	/* do 20 j=2,n8 */
    {
      /* windex = (j-1)*(1<<(k-1)); -- done with trick to avoid (j-1) and 1<<(k-1) */
      cc1 = wcos[windex];
      ss1 = wcos[windex_n4 - windex]; /* sin-from-cos trick: see my notes */
      cc3 = w3cos[windex];
      ss3 = w3sin[windex];	/* sin-from-cos trick does not work here */
      windex = j*windex0;	/* same trick as "a = j*e" */
      /* a3 = 3*a;
	 cc1 = cos(a);
	 ss1 = sin(a);
	 cc3 = cos(a3);
	 ss3 = sin(a3);
	 a = j*e; */
      is = 0;
      id = 2*n2;
      label40:
      for (i=is; i<n; i=i+id)	/* 40         do 30 i = is,(n-1),id */
      { i1 = i + j;
	i2 = i1 + n4;
	i3 = i2 + n4;
	i4 = i3 + n4;
	i5 = i  + n4 - j + 2;
	i6 = i5 + n4;
	i7 = i6 + n4;
	i8 = i7 + n4;
	t1    = x[i1] - x[i6];
	x[i1] = x[i1] + x[i6];
	t2    = x[i5] - x[i2];
	x[i5] = x[i5] + x[i2];
	t3    = x[i8] + x[i3];
	x[i6] = x[i8] - x[i3];
	t4    = x[i4] + x[i7];
	x[i2] = x[i4] - x[i7];
	t5 = t1 - t4;
	t1 = t1 + t4;
	t4 = t2 - t3;
	t2 = t2 + t3;
	x[i3] = t5*cc1    + t4*ss1;
	x[i7] = t5*ss1    - t4*cc1; 	/* x[i7] = (-t4*cc1) + t5*ss1; */
	x[i4] = t1*cc3    - t2*ss3;
	x[i8] = t2*cc3    + t1*ss3;
      }				/* 30         continue */
      is = 2*id - n2;
      id = 4*id;
      if (is < (n-1)) goto label40; /* if (is.lt.(n-1)) goto 40 */
    }				/*  20      continue */
  }				/* 10   continue */
  /* c */
  /* c  ----length-two-butterflies----------- */
  is = 1;
  id = 4;
  label70:
  for (i0=is; i0<=n; i0=i0+id)  /*  70   DO 60 I0 = IS,N,ID */
  { i1    = i0 + 1;
    r1    = x[i0];
    x[i0] = r1 + x[i1];
    x[i1] = r1 - x[i1];
  }				/* 60   CONTINUE */
  is = 2*id - 1;
  id = 4*id;
  if (is < n) goto label70;	/* IF (IS.LT.N) GOTO 70 */
  /* c */
  /* c-----------bit-reverse-counter---------------c */
  label100:
  j = 1;
  n1 = n - 1;
  for (i=1; i<=n1; i++)		/* DO 104 I = 1, N1 */
  { if (i >= j) goto label101;	/* if (i .ge. j) goto 101 */
    xt = x[j];
    x[j] = x[i];
    x[i] = xt;
    label101: k = n>>1;		/* k = n/2; */
    label102: if (k>=j) goto label103;
    j = j - k;
    k = k>>1;			/* k = k/2; */
    goto label102;
    label103: j = j + k;
  }				/* 104  CONTINUE */
  /* c */
}				/* RETURN ^M END */




/* Image processing     only for square images
   (old stuff handles non-square but is slow)
   For 2d FTs  precomputed tables or not, make almost no difference in total time.
   */

DEFINE_PRIMITIVE ("PAS-CFT2D!", Prim_pas_cft2d, 5,5, 0)
{ long i, length, power, flag, rows,rowpower;
  REAL *f1,*f2,  *wcos,*w3cos,*w3sin;
  void pas_cft2d();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (2, ARRAY_P);	/* real part */
  CHECK_ARG (3, ARRAY_P);	/* imag part */
  CHECK_ARG (4, ARRAY_P);	/* twiddle tables, length = 3*(rows/4)  */

  flag = (arg_integer (1));
  length = ARRAY_LENGTH(ARG_REF(2));
  if (length != (ARRAY_LENGTH(ARG_REF(3)))) error_bad_range_arg(2);

  for (power=0, i=length; i>1; power++)	/*         length must be power of 2 */
  { if ( (i % 2) == 1) error_bad_range_arg(2);
    i=i/2; }

  if ((power % 2) == 1) error_bad_range_arg(2);
  rowpower = (power/2);
  rows = (1<<rowpower);		/*                 square image */

  f1 = ARRAY_CONTENTS(ARG_REF(2));
  f2 = ARRAY_CONTENTS(ARG_REF(3));
  if (f1==f2) error_wrong_type_arg(2);

  wcos = ARRAY_CONTENTS(ARG_REF(4)); /* twiddle tables */
  if (ARRAY_LENGTH(ARG_REF(4)) != (3*rows/4)) error_bad_range_arg(4);
  w3cos = wcos   +   rows/4;
  w3sin = w3cos  +   rows/4;
  if ((arg_nonnegative_integer(5)) == 1)
    pas_cft2d(1, flag, f1,f2, rows, rowpower, wcos,w3cos,w3sin);
  else
    pas_cft2d(0, flag, f1,f2, rows, rowpower, wcos,w3cos,w3sin);
  /*          1 means tables are already made
	      0 means compute new tables */

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* pas_cft2d
   n =                rows of square image, rows is power of 2
   m =                rowpower
   Scaling (1/n) is done all-at-once at the end.
   Time-Reversing is done intermediately, it is more efficient.
   */
void pas_cft2d(tables_ok,flag, x,y,n,m, wcos,w3cos,w3sin)
     REAL *x,*y, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale, *xrow,*yrow;
  long i,j, rows,cols, total_length;
  void pas_cft_make_twiddle_tables_once();
  void C_Array_Time_Reverse();
  void pas_cft_forward_loop();

  if (tables_ok != 1) 		/* 1 means = tables already made */
    pas_cft_make_twiddle_tables_once(n,m, wcos,w3cos,w3sin);

  rows = n;
  cols = rows;			/* square image */
  total_length = rows*rows;

  if (flag != 1)		/* backward transform */
    for (i=0; i<total_length; i++) y[i] = (-y[i]); /* conjugate before */

  xrow = x; yrow = y;		/* ROW-WISE */
  for (i=0; i<rows; i++)	/* forward or backward */
  { pas_cft_forward_loop( xrow,yrow, n,m, wcos,w3cos,w3sin);
    xrow = xrow + cols;
    yrow = yrow + cols; }

  Image_Fast_Transpose(x, rows); /* COLUMN-WISE */
  Image_Fast_Transpose(y, rows);
  xrow = x; yrow = y;
  for (i=0; i<rows; i++)	/* forward or backward */
  { pas_cft_forward_loop( xrow,yrow, n,m, wcos,w3cos,w3sin);
    xrow = xrow + cols;
    yrow = yrow + cols; }

  Image_Fast_Transpose(x, rows);
  Image_Fast_Transpose(y, rows);

  if (flag == 1)		/* forward : scale */
  { scale = (REAL) (1.0 / ((double) total_length));
    for (i=0; i<total_length; i++)
    { x[i] = x[i]*scale;
      y[i] = y[i]*scale; }}
  else				/* backward : conjugate after */
    for (i=0; i<total_length; i++) y[i] = (-y[i]);
}


DEFINE_PRIMITIVE ("PAS-RFT2D-CSFT2D!", Prim_pas_rft2d_csft2d, 5,5, 0)
{ long i, length, power, flag, ft_type, rows,rowpower;
  REAL *f1,  *wcos,*w3cos,*w3sin;
  void pas_rft2d(), pas_csft2d();
  PRIMITIVE_HEADER (5);
  CHECK_ARG (2, ARRAY_P);	/* Input data (real or cs) */
  CHECK_ARG (3, ARRAY_P);	/* CFT twiddle tables, length = 3*(rows/4)  */
  CHECK_ARG (4, FIXNUM_P);	/* (1)=tables precomputed, else recompute */
  flag = (arg_integer (1));
  f1 = ARRAY_CONTENTS(ARG_REF(2));
  length = ARRAY_LENGTH(ARG_REF(2));
  for (power=0, i=length; i>1; power++)	/* length must be power of 2 */
  { if ( (i % 2) == 1) error_bad_range_arg(2);
    i=i/2; }

  if ((power % 2) == 1) error_bad_range_arg(2);
  rowpower = (power/2);
  rows = (1<<rowpower);		/*                 square image */

  wcos = ARRAY_CONTENTS(ARG_REF(3)); /* CFT twiddle tables */
  if (ARRAY_LENGTH(ARG_REF(3)) != (3*rows/4)) error_bad_range_arg(3);
  w3cos = wcos  + rows/4;
  w3sin = w3cos + rows/4;

  ft_type = (arg_nonnegative_integer(5)); /*          rft2d or csft2d */
  if (ft_type == 1) {
    if ((arg_nonnegative_integer(4)) == 1)
      pas_rft2d     (1, flag, f1, rows, rowpower, wcos,w3cos,w3sin);
    else pas_rft2d  (0, flag, f1, rows, rowpower, wcos,w3cos,w3sin);
  }
  else if (ft_type == 3) {
    if ((arg_nonnegative_integer(4)) == 1)
      pas_csft2d    (1, flag, f1, rows, rowpower, wcos,w3cos,w3sin);
    else pas_csft2d (0, flag, f1, rows, rowpower, wcos,w3cos,w3sin);
    /*               1 means tables are already made
		     0 means compute new tables */
  }
  else  error_bad_range_arg(5);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* c                             RFT2D      CSFT2D
   The frequencies are scrabled wrt  what cft2d (and the old image-fft) give.
   See   cs-image-magnitude  and  cs-image-real    which unscrable automatically.
   c
   c Implementation notes:
   c   conjugate in one domain         is         reverse and conjugate in other
   c   reverse   in one domain         is         reverse in other
   c
   c    reverse cs-array     which is identical to      conjugate cs-array  (same domain)
   c                                   is         reverse in other domain
   c
   c conjugate cs-array before csft    is-better-than    reverse real-array afterwards
   c
   c
   c    rft2d-csft2d  use 1d-cft tables    to compute rft
   c    cft tables are simply larger than realdata tables.
   */

/* pas_rft2d
   n =                rows of square image, rows is power of 2
   m =                rowpower
   Scaling (1/n) is done all-at-once at the end.
   Time-Reversing is done intermediately, it is more efficient.
   */
void pas_rft2d(tables_ok,flag, x, n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale, *xrow,*yrow;
  long i,j, rows,cols, total_length, n2;
  void pas_cft_make_twiddle_tables_once();
  void C_Array_Time_Reverse();
  void pas_rft_forward_loop(), pas_cft_forward_loop();

  if (tables_ok != 1) 		/* 1 means = tables already made */
    pas_cft_make_twiddle_tables_once(n,m, wcos,w3cos,w3sin);

  rows = n;
  cols = rows;			/* square image */
  n2   = n/2;
  total_length = rows*rows;

  xrow = x;			/*                First ROW-WISE */
  if (flag == 1)		/* forward transform */
    for (i=0; i<rows; i++)
    { pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin);
      xrow = xrow + cols; }
  else				/* backward transform */
    for (i=0; i<rows; i++)
    { pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin);
      for (j=n2+1; j<n; j++) xrow[j] = (-xrow[j]); /* time-reverse cs-array */
      xrow = xrow + cols; }

  Image_Fast_Transpose(x, rows); /* COLUMN-WISE */

  /*      TREAT specially rows 0 and n2,
	  they are real and go into cs-arrays */
  if (flag == 1)		/* forward transform */
  { xrow =          x + 0      ;
    pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin);
    xrow =          x + n2*cols;
    pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin); }
  else				/* backward transform */
  { xrow =          x + 0      ;
    pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin);
    for (j=n2+1; j<n; j++) xrow[j] = (-xrow[j]); /* time-reverse cs-array */
    xrow =          x + n2*cols;
    pas_rft_forward_loop(xrow, n,m, wcos,w3cos,w3sin);
    for (j=n2+1; j<n; j++) xrow[j] = (-xrow[j]); /* time-reverse cs-array */
  }

  /*     TREAT the rest of the rows with CFT
   */
  if (flag != 1)		/* backward : conjugate before */
    for (i=(n2+1)*cols; i<total_length; i++)    x[i] = (-x[i]);

  xrow = x + cols;		/* real part */
  yrow = x + (rows-1)*cols;	/* imag part */
  for (i=1; i<n2; i++)		/* forward or backward transform */
  { pas_cft_forward_loop(xrow,yrow, n,m, wcos,w3cos,w3sin);
    xrow = xrow + cols;
    yrow = yrow - cols; }
  /*    DO NOT TRANSPOSE BACK, leave real-imag in horizontal rows, save.
   */

  if (flag == 1)		/* forward : scale */
  { scale = (REAL) (1.0 / ((double) total_length));
    for (i=0; i<total_length; i++)
      x[i] = x[i]*scale; }
  else				/* backward : conjugate after */
    for (i=(n2+1)*cols; i<total_length; i++)
      x[i] = (-x[i]);
}


/* pas_csft2d
   n =                rows of square image, rows is power of 2
   m =                rowpower
   Scaling (1/n) is done all-at-once at the end.
   Time-Reversing is done intermediately, it is more efficient.
   */
void pas_csft2d(tables_ok,flag, x, n,m, wcos,w3cos,w3sin)
     REAL *x, *wcos,*w3cos,*w3sin;
     long n,m, flag, tables_ok;
{ REAL scale, *xrow,*yrow;
  long i,j, rows,cols, total_length, n2;
  void pas_cft_make_twiddle_tables_once();
  void C_Array_Time_Reverse();
  void pas_csft_backward_loop(), pas_cft_forward_loop();

  if (tables_ok != 1) 		/* 1 means = tables already made */
    pas_cft_make_twiddle_tables_once(n,m, wcos,w3cos,w3sin);

  rows = n;
  cols = rows;			/* square image */
  n2   = n/2;
  total_length = rows*rows;

  /*                                     First  ROW-WISE */

  /*      TREAT SPECIALLY ROWS 0 and n2,   they are cs-arrays and they go into real */
  if (flag == 1)		/* forward transform */
  { xrow =          x + 0      ;
    for (j=n2+1; j<n; j++)  xrow[j]=(-xrow[j]); /* conjugate before */
    pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin);
    xrow =          x + n2*cols;
    for (j=n2+1; j<n; j++)  xrow[j]=(-xrow[j]); /* conjugate before */
    pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin); }
  else				/* backward transform */
  { xrow =          x + 0      ;
    pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin);
    xrow =          x + n2*cols;
    pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin); }

  /*     TREAT the rest of the rows with CFT
   */
  if (flag != 1)		/* backward : conjugate before */
    for (i=(n2+1)*cols; i<total_length; i++)    x[i] = (-x[i]);

  xrow = x + cols;		/* real part */
  yrow = x + (rows-1)*cols;	/* imag part */
  for (i=1; i<n2; i++)		/* forward or backward transform */
  { pas_cft_forward_loop(xrow,yrow, n,m, wcos,w3cos,w3sin);
    xrow = xrow + cols;
    yrow = yrow - cols; }

  if (flag != 1)		/* backward : conjugate after */
    for (i=(n2+1)*cols; i<total_length; i++)    x[i] = (-x[i]);

  Image_Fast_Transpose(x, rows);
  /*                                Second   COLUMN-WISE
				    Everything should be cs-arrays now */

  xrow = x;
  if (flag == 1)		/* forward transform */
    for (i=0; i<rows; i++)
    { for (j=n2+1; j<n; j++)  xrow[j]=(-xrow[j]); /* conjugate before */
      pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin);
      xrow = xrow + cols; }
  else				/* backward transform */
    for (i=0; i<rows; i++)
    { pas_csft_backward_loop(xrow, n,m, wcos,w3cos,w3sin);
      xrow = xrow + cols; }

  if (flag == 1)		/* forward : scale */
  { scale = (REAL) (1.0 / ((double) total_length));
    for (i=0; i<total_length; i++)
      x[i] = x[i] * scale; }
}



/* STUFF BEFORE 4-15-1989
 */

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

/* C_Array_FFT
   complex data, radix=2, not-in-place.
   (adapted from an fft program I got from Yekta)
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


/* CHIRP-Z-TRANSFORM (complex data)
 */

/* C_Array_CZT           Generalization of DFT
   ;;
   Frequency is scaled as an L/2-point DFT of the input data (zero padded to L/2).
   ;;
   phi = starting point (on unit circle) -- Range 0,1 (covers 0,2pi like DFT angle)
   rho = resolution (angular frequency spacing) -- Range 0,1 (maps 0,2pi like DFT angle)
   N = input data length
   M = output data length
   log2_L = smallest_power_of_2_ge(N+M-1)   ----
   f1,f2 contain the input data (complex).
   f1,f2,fo1,fo2,g1,g2            must be of length L
   fft_w1,fft_w2                  must be of length L/2
   czt_w1,czt_w2                  must be of length max(M,N)  ----
   ;;
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
  double tmp, A1, A2;
  rho = rho*.5;			/* To make 1/2 in exponent "(n^2)/2" */
  for (i=0;i<N;i++)
  { tmp = ((double) i);
    tmp = TWOPI * ((phi + (rho*tmp)) * tmp);
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
    tmp = TWOPI * (tmp * (rho*tmp));
    czt_w1[i] = (REAL) cos(tmp);
    czt_w2[i] = (REAL) sin(tmp); }
}


long smallest_power_of_2_ge(n)
     long n;
{ long i,power;
  if (n<0) { printf("\n ABORT program! smallest_pwr_of_2_ge negative argument--- %d\n", n); fflush(stdout); }
  power=0; i=1;
  while (i<n)
  { power++; i=i*2; }
  return(power);
}

/*  stuff not currently used

void CZT_Post_Multiply(f1,f2,czt_w1,czt_w2,M)
     REAL *f1,*f2,*czt_w1,*czt_w2; long M;
{ long i;
  REAL tmp;
  for (i=0;i<M;i++)
  { tmp = f1[i]*czt_w1[i] - f2[i]*czt_w2[i];
    f2[i] = 2.0 * (f1[i]*czt_w2[i] + f2[i]*czt_w1[i]);
    f1[i] = 2.0 * tmp;
  }}

#define take_modulo_one(x, answer)  \
{ long ignore_integral_part;        \
  double modf();                    \
  answer = (double) modf( ((double) x), &ignore_integral_part); }
            ^ this only works when answer is double

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
      if ( (i % 2) == 1) error_bad_range_arg (2);
      i=i/2; }
    for (nrows_power=0, i=nrows; i>1; nrows_power++) {
      if ( (i % 2) == 1) error_bad_range_arg (1);
      i=i/2; }

#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
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
    if ( (i % 2) == 1) error_bad_range_arg (2);
    i=i/2; }
#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
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
      if ( (l % 2) == 1) error_bad_range_arg (2);
      l=l/2; }
    for (nrows_power=0, m=nrows; m>1; nrows_power++) {
      if ( (m % 2) == 1) error_bad_range_arg (2);
      m=m/2; }
    for (ncols_power=0, n=ncols; n>1; ncols_power++) {
      if ( (n % 2) == 1) error_bad_range_arg (2);
      n=n/2; }

    printf("3D FFT implemented only for cubic-spaces.\n");
    printf("aborted\n.");
  }
}

Cube_Space_3D_FFT_In_Scheme_Heap(flag, ndeps, Real_Array, Imag_Array)
     long flag, ndeps; REAL *Real_Array, *Imag_Array;
{ fast long l, m, n;
  fast long ndeps_power, Surface_Length;
  fast REAL *From_Real, *From_Imag;
  fast REAL *f1,*f2,*g1,*g2,*w1,*w2, *Work_Here;

  for (ndeps_power=0, l=ndeps; l>1; ndeps_power++) {                 /* FIND/CHECK POWER OF NDEPS */
    if ( (l % 2) == 1) error_bad_range_arg (2);
    l=l/2; }
#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
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
  SCHEME_OBJECT answer;
  REAL *f1,*f2,*g1,*g2,*w1,*w2;
  REAL *Work_Here;

  PRIMITIVE_HEADER (4);
  flag = arg_integer(1);	/* forward or backward  */
  CHECK_ARG (2, ARRAY_P);	/*      input real */
  CHECK_ARG (3, ARRAY_P);	/*      input imag */

  length = ARRAY_LENGTH(ARG_REF(2));
  if (length != (ARRAY_LENGTH(ARG_REF(3)))) error_bad_range_arg(2);

  for (power=0, i=length; i>1; power++) {
    if ( (i % 2) == 1) error_bad_range_arg(2);
    i=i/2; }

  f1 = ARRAY_CONTENTS(ARG_REF(2));
  f2 = ARRAY_CONTENTS(ARG_REF(3));
  if (f1==f2)  error_wrong_type_arg(2);

#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
  Primitive_GC_If_Needed(length*3*REAL_SIZE);
  Work_Here = (REAL *) Free;
  g1 = Work_Here;
  g2 = Work_Here + length;
  w1 = Work_Here + (length<<1);
  w2 = Work_Here + (length<<1) + (length>>1);

  C_Array_FFT(flag, power, length, f1,f2,g1,g2,w1,w2);

  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ARRAY-CZT!", Prim_array_czt, 6,6, 0)
{ double phi,rho;
  long N,M,L,  i;
  long log2_L, maxMN;
  long smallest_power_of_2_ge();
  int errcode;
  REAL *a,*b,*c,*d;
  REAL *f1,*f2,*fo1,*fo2, *g1,*g2, *fft_w1,*fft_w2,*czt_w1,*czt_w2,    *Work_Here;

  PRIMITIVE_HEADER (6);
  phi = (arg_real_number (1));	/* starting point [0,1]*/
  rho = (arg_real_number (2));	/* resolution [0,1] */
  CHECK_ARG (3, ARRAY_P);	/* input real */
  CHECK_ARG (4, ARRAY_P);	/* input imag */
  CHECK_ARG (5, ARRAY_P);	/* output real */
  CHECK_ARG (6, ARRAY_P);	/* output imag */
  
  a = ARRAY_CONTENTS(ARG_REF(3));
  b = ARRAY_CONTENTS(ARG_REF(4));
  c = ARRAY_CONTENTS(ARG_REF(5));
  d = ARRAY_CONTENTS(ARG_REF(6));
  
  N = ARRAY_LENGTH(ARG_REF(3));	/* N = input length */
  M = ARRAY_LENGTH(ARG_REF(5));	/* M = output length */
  if (N!=(ARRAY_LENGTH(ARG_REF(4))))    error_bad_range_arg(3);
  if (M!=(ARRAY_LENGTH(ARG_REF(6))))    error_bad_range_arg(5);

  if ((M+N-1) < 4)                      error_bad_range_arg(5);
  log2_L = smallest_power_of_2_ge(M+N-1);
  L  = 1<<log2_L;		/* length of intermediate computation arrays */
  maxMN =  (((M)<(N)) ? (N) : (M)); /* length of czt tables =  maximum(M,N) */

#if (REAL_IS_DEFINED_DOUBLE != 0)
    ALIGN_FLOAT (Free);
    Free += 1;
#endif
  Primitive_GC_If_Needed( ((7*L) + (2*maxMN)) * REAL_SIZE);
  g1  = (REAL *) Free;
  g2  = g1  + L;
  fo1 = g2  + L;
  fo2 = fo1 + L;
  fft_w1 = fo2 + L;
  fft_w2 = fft_w1 + (L/2);
  czt_w1 = fft_w2 + (L/2);
  czt_w2 = czt_w1 + maxMN;
  f1 = czt_w2 + maxMN;		/* CZT stores its results here */
  f2 = f1     + L;

  for (i=0; i<N; i++) { f1[i] = a[i]; /*        input data */
			f2[i] = b[i]; }

  C_Array_CZT(phi,rho, N,M,log2_L, f1,f2,fo1,fo2, g1,g2, fft_w1,fft_w2,czt_w1,czt_w2);

  for (i=0; i<M; i++) { c[i] = f1[i]; /*        results */
			d[i] = f2[i]; }

  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ARRAY-2D-FFT!", Prim_array_2d_fft, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    fast long nrows = (arg_integer_in_range (2, 1, 513));
    fast long ncols = (arg_integer_in_range (3, 1, 513));
    fast SCHEME_OBJECT real_image = (ARG_REF (4));
    fast SCHEME_OBJECT imag_image = (ARG_REF (5));
    CHECK_ARG (4, ARRAY_P);
    CHECK_ARG (5, ARRAY_P);
    if (real_image == imag_image)
      error_wrong_type_arg (5);
    Set_Time_Zone (Zone_Math);
  {
    long length = (ARRAY_LENGTH (real_image));
    if ((length != (ARRAY_LENGTH (imag_image))) ||
	(length != (nrows * ncols)))
      error_bad_range_arg (5);
  }
    C_Array_2D_FFT_In_Scheme_Heap
      ((arg_integer (1)),	/* flag 1=forward else backward */
       nrows,
       ncols,
       (ARRAY_CONTENTS (real_image)),
       (ARRAY_CONTENTS (imag_image)));
    PRIMITIVE_RETURN (cons (real_image, (cons (imag_image, EMPTY_LIST))));
  }
}

DEFINE_PRIMITIVE ("ARRAY-3D-FFT!", Prim_array_3d_fft, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  {
    fast long ndeps = (arg_integer_in_range (2, 1, 513));
    fast long nrows = (arg_integer_in_range (3, 1, 513));
    fast long ncols = (arg_integer_in_range (4, 1, 513));
    fast SCHEME_OBJECT real_image = (ARG_REF (5));
    fast SCHEME_OBJECT imag_image = (ARG_REF (6));
    CHECK_ARG (5, ARRAY_P);
    CHECK_ARG (6, ARRAY_P);
    if (real_image == imag_image)
      error_wrong_type_arg (6);
    {
      long length = (ARRAY_LENGTH (real_image));
      if ((length != (ARRAY_LENGTH (imag_image))) ||
	  (length != (ndeps * nrows * ncols)))
	error_bad_range_arg (6);
    }
    C_Array_3D_FFT_In_Scheme_Heap
      ((arg_integer (1)),
       ndeps,
       nrows,
       ncols,
       (ARRAY_CONTENTS (real_image)),
       (ARRAY_CONTENTS (imag_image)));
    PRIMITIVE_RETURN (cons (real_image, (cons (imag_image, EMPTY_LIST))));
  }
}
