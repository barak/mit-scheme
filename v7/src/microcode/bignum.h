/*          Hey EMACS, this is -*- C -*- code!                 */

/****************************************************************
*                                                               *
*                         Copyright (c) 1984                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* Head file for bignums.  This is shared by bignum.c and generic.c. */

#ifdef ENABLE_DEBUGGING_TOOLS
#define Debug_Test(Res)						\
{ Pointer R = Make_Pointer(TC_BIG_FIXNUM, Res);			\
  if (Nth_Vector_Loc(R, Vector_Length(R)) != (Free-1))		\
  { printf("\nResult=%x => %x %x %x, Length=%d, Free=%x\n",	\
           R, Fast_Vector_Ref(R, 0),				\
           Fast_Vector_Ref(R, 1), Fast_Vector_Ref(R, 2),	\
           Vector_Length(R), Free);				\
    Microcode_Termination(TERM_EXIT);				\
  }                                                		\
}
#else
#define Debug_Test(Res) { }
#endif

#define POSITIVE	1
#define NEGATIVE	0

/* The representation of a BIGNUM is machine dependent. For a VAX-11
 * it is as follows: 
 */

#ifdef pdp10
typedef unsigned int bigdigit;
typedef long bigdouble;
#define SHIFT 			16
#define factor			1
#else
#if ((USHORT_SIZE * 2) <= ULONG_SIZE)
#define bigdigit		unsigned short
#define bigdouble 		long	/* Should be unsigned */
#define SHIFT			(CHAR_SIZE*sizeof(bigdigit))
#define factor			(sizeof(Pointer)/sizeof(bigdigit))
#else
#if ((CHAR_SIZE * 2) <= ULONG_SIZE)
#define bigdigit		unsigned char
#define bigdouble		long	/* Should be unsigned */
#define SHIFT			CHAR_SIZE
#define factor			(sizeof(Pointer)/sizeof(bigdigit))
#else
#include "Cannot compile bignums.  All types too large.  See bignum.h"
#endif
#endif
#endif

#define DELTA			\
 ((sizeof(bigdouble)-sizeof(bigdigit))*CHAR_SIZE)
#define SIGN(Bignum)		(Bignum[factor])
#define LEN(Bignum)		(Bignum[factor+1])
#define Bignum_Bottom(Bignum)	(&(Bignum)[factor+2])
#define Bignum_Top(Bignum)	(&(Bignum)[factor+1+LEN(Bignum)])
#define Align(ndigits)		((((ndigits) + factor + 1) / factor) + 1)

/* For temporary bignums */

#define TEMP_SIZE Align(4)

/* Macros for making BIGNUM headers */

#define Make_Header(l) Make_Non_Pointer(TC_MANIFEST_NM_VECTOR,(l-1))
#define Prepare_Header(Bignum,Length,Sign) 				\
        { *((Pointer *) Bignum) = Make_Header(Align(Length));		\
          SIGN(Bignum) = Sign;						\
          LEN(Bignum)  = Length;					\
        }

/* Predicates coded as macros for determining the sign of BIGNUM's */

#define POS_BIGNUM(Bignum) (SIGN(Bignum) == POSITIVE)
#define NEG_BIGNUM(Bignum) (SIGN(Bignum) == NEGATIVE)
#define ZERO_BIGNUM(Bignum) (LEN(Bignum) == 0)
#define NON_ZERO_BIGNUM(Bignum) (LEN(Bignum) != 0)


/* Coerces a C pointer to point to BIGNUM digits */

#define BIGNUM(ptr) ((bigdigit *) ptr)

/* Macros for manipulating long BIGNUM digits */

#define RADIX (1<<SHIFT)
#define MAX_DIGIT_SIZE (RADIX-1)
#define CARRY_MASK (MAX_DIGIT_SIZE<<SHIFT)
#define DIGIT_MASK MAX_DIGIT_SIZE
#define DIV_MASK ((1<<DELTA)-1)
#define Get_Carry(lw) (((lw & CARRY_MASK) >> SHIFT) & DIGIT_MASK)
#define Get_Digit(lw) (lw & DIGIT_MASK)
#define Mul_Radix(sw) (sw << SHIFT)
#define Div_Radix(lw) ((lw >> SHIFT) & DIV_MASK)
#define Rem_Radix(lw) (lw & DIGIT_MASK)

/* Length of the BIGNUM that contains the largest FIXNUM */

#define FIXNUM_LENGTH_AS_BIGNUM       ((FIXNUM_LENGTH+(SHIFT-1))/SHIFT)
#define C_INTEGER_LENGTH_AS_BIGNUM    ((POINTER_LENGTH+(SHIFT-1))/SHIFT)

/* Cases returned by the comparison function big_compare() */

#define EQUAL      0
#define ONE_BIGGER 1
#define TWO_BIGGER 2

/* Categorize_Sign() takes two bignum's and classify them according
 * to four possible cases, depending on each's sign.  Depends on
 * definition of POSITIVE and NEGATIVE, earlier!!!
 */

#define Categorize_Sign(ARG1, ARG2) ((SIGN(ARG1) << 1) | SIGN(ARG2))
#define BOTH_NEGATIVE 0
#define ARG1_NEGATIVE 1
#define ARG2_NEGATIVE 2
#define BOTH_POSITIVE 3
#define Sign_Error(proc) 						\
        { printf(proc);							\
          printf(" -- Sign Determination Error\n");			\
	  printf("Possibly Uncanonicalized Bignum\n");			\
          return ERR_UNDEFINED_PRIMITIVE; 				\
        }

#define Fetch_Bignum(big) BIGNUM(Get_Pointer(big))

#define Bignum_Operation(Object, Result)	 			\
  Result = (Object);							\
  Free = Nth_Vector_Loc(Result, Vector_Length(Result)+1);		\
  Result = Big_To_Fix(Result);

#define Divide_Bignum_Operation(Object, Result) 			\
{ Pointer *End_Of_First, *First, *Second;				\
  Result = (Object);							\
  First = Get_Pointer(Vector_Ref(Result, CONS_CAR));			\
  Second = Get_Pointer(Vector_Ref(Result, CONS_CDR));			\
  End_Of_First = First+1+Get_Integer(First[0]);				\
  if (End_Of_First != Second)						\
  { *End_Of_First =							\
      Make_Non_Pointer(TC_MANIFEST_NM_VECTOR, (Second-End_Of_First)-1);	\
    if (Bignum_Debug) printf("\nGap=0x%x\n", (Second-End_Of_First)-1);	\
  }									\
  Free = Second+1+Get_Integer(Second[0]);				\
  Vector_Set(Result,CONS_CAR,Big_To_Fix(Vector_Ref(Result,CONS_CAR)));  \
  Vector_Set(Result,CONS_CDR,Big_To_Fix(Vector_Ref(Result,CONS_CDR)));  \
}
