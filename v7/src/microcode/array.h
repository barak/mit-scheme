/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/array.h,v 9.31 1989/09/20 23:05:33 cph Rel $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

#define REAL_IS_DEFINED_DOUBLE 0

#if (REAL_IS_DEFINED_DOUBLE == 0)
#define REAL float
#else
#define REAL double
#endif

#define arg_real(arg_number) ((REAL) (arg_real_number (arg_number)))
#define REAL_SIZE (BYTES_TO_WORDS (sizeof (REAL)))

#define FLOAT_SIZE (BYTES_TO_WORDS (sizeof (float)))
#define DOUBLE_SIZE (BYTES_TO_WORDS (sizeof (double)))

/* Scheme_Arrays are implemented as NON_MARKED_VECTOR. */

#define ARRAY_P NON_MARKED_VECTOR_P
#define ARRAY_LENGTH(array) ((long) (FAST_MEMORY_REF ((array), 1)))
#define ARRAY_CONTENTS(array) ((REAL *) (MEMORY_LOC (array, 2)))

extern SCHEME_OBJECT allocate_array ();

extern void C_Array_Find_Min_Max ();
extern void C_Array_Complex_Multiply_Into_First_One ();

extern void C_Array_Make_Histogram ();
/* REAL * Array;
   REAL * Histogram;
   long Length;
   long npoints; */

extern void Find_Offset_Scale_For_Linear_Map();
/* REAL Min;
   REAL Max;
   REAL New_Min;
   REAL New_Max;
   REAL * Offset;
   REAL * Scale; */

/* The following macros implement commonly used array procs. */

/* In the following macros we assign the arguments to local variables
   so as to do any computation (referencing, etc.) only once outside the loop.
   Otherwise it would be done again and again inside the loop.
   The names, like "MCRINDX", have been chosen to avoid shadowing the
   variables that are substituted in.  WARNING: Do not use any names
   starting with the prefix "mcr", when calling these macros */

#define C_Array_Scale(array, scale, n)					\
{									\
  fast REAL * mcr_scan = (array);					\
  fast REAL * mcr_end = (mcr_scan + (n));				\
  fast REAL mcrd0 = (scale);						\
  while (mcr_scan < mcr_end)						\
    (*mcr_scan++) *= mcrd0;						\
}

#define Array_Scale(array, scale)					\
{									\
  C_Array_Scale								\
    ((ARRAY_CONTENTS (array)),						\
     (scale),								\
     (ARRAY_LENGTH (array)));						\
}

#define C_Array_Copy(from, to, n)					\
{									\
  fast REAL * mcr_scan_source = (from);					\
  fast REAL * mcr_end_source = (mcr_scan_source + (n));			\
  fast REAL * mcr_scan_target = (to);					\
  while (mcr_scan_source < mcr_end_source)				\
    (*mcr_scan_target++) = (*mcr_scan_source++);			\
}

#define Array_Copy(from, to)						\
{									\
  C_Array_Copy								\
    ((ARRAY_CONTENTS (from)),						\
     (ARRAY_CONTENTS (to)),						\
     (ARRAY_LENGTH (from)));						\
}

#define C_Array_Add_Into_Second_One(from, to, n)			\
{									\
  fast REAL * mcr_scan_source = (from);					\
  fast REAL * mcr_end_source = (mcr_scan_source + (n));			\
  fast REAL * mcr_scan_target = (to);					\
  while (mcr_scan_source < mcr_end_source)				\
    (*mcr_scan_target++) += (*mcr_scan_source++);			\
}

#define Array_Add_Into_Second_One(from,to)				\
{									\
  C_Array_Add_Into_Second_One						\
    ((ARRAY_CONTENTS (from)),						\
     (ARRAY_CONTENTS (to)),						\
     (ARRAY_LENGTH (from)));						\
}

#define mabs(x) (((x) < 0) ? (- (x)) : (x))
#define max(x,y) (((x) < (y)) ? (y) : (x))
#define min(x,y) (((x) < (y)) ? (x) : (y))
