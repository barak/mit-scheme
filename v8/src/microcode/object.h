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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/object.h,v 5.3 1987/01/12 17:17:33 cph Exp $ */

/* This file contains definitions pertaining to the C view of 
   Scheme pointers: widths of fields, extraction macros, pre-computed
   extraction masks, etc. */

/* The C type Pointer is defined at the end of CONFIG.H
   The definition of POINTER_LENGTH here assumes that Pointer is the same
   as unsigned long.  If that ever changes, this definition must also.
   POINTER_LENGTH is defined this way to make it available to
   the preprocessor. */

#define POINTER_LENGTH		ULONG_SIZE
#define TYPE_CODE_LENGTH	8	/* Not CHAR_SIZE!! */
#define MAX_TYPE_CODE		0xFF	/* ((1<<TYPE_CODE_LENGTH) - 1) */

/* The danger bit is set in the value cell of an environment whenever a
   particular binding of a variable to a value has been shadowed by an
   auxiliary variable in a nested environment.  It means that variables
   cached to this address must be recached since the address may be invalid.
   See lookup.c */ 

#define DANGER_TYPE		0x80	/* (1<<(TYPE_CODE_LENGTH-1)) */
#define MAX_SAFE_TYPE   	0x7F	/* (MAX_TYPE_CODE & ~DANGER_TYPE) */
#define SAFE_TYPE_MASK		MAX_SAFE_TYPE
#define DANGER_BIT		HIGH_BIT

#ifndef b32			/* Safe versions */

#define ADDRESS_LENGTH		(POINTER_LENGTH-TYPE_CODE_LENGTH)
#define ADDRESS_MASK		((1<<ADDRESS_LENGTH) - 1)
#define TYPE_CODE_MASK		(~ADDRESS_MASK)
#define HIGH_BIT		(1 << (POINTER_LENGTH-1))
/* FIXNUM_LENGTH does NOT include the sign bit! */
#define FIXNUM_LENGTH		(ADDRESS_LENGTH-1)
#define FIXNUM_SIGN_BIT		(1<<FIXNUM_LENGTH)
#define SIGN_MASK		(TYPE_CODE_MASK | FIXNUM_SIGN_BIT)
#define SMALLEST_FIXNUM		(-1<<FIXNUM_LENGTH)
#define BIGGEST_FIXNUM		(~(-1<<FIXNUM_LENGTH))

#else				/* 32 bit word versions */

#define ADDRESS_LENGTH		24
#define ADDRESS_MASK		0x00FFFFFF
#define TYPE_CODE_MASK		0xFF000000
#define HIGH_BIT		0x80000000
#define FIXNUM_LENGTH		23
#define FIXNUM_SIGN_BIT		0x00800000
#define SIGN_MASK		0xFF800000
#define SMALLEST_FIXNUM		0xFF800000
#define BIGGEST_FIXNUM		0x007FFFFF

#endif

#ifndef UNSIGNED_SHIFT		/* Safe version */
#define pointer_type(P)		(((P) >> ADDRESS_LENGTH) & MAX_TYPE_CODE)
#define safe_pointer_type(P)	(((P) >> ADDRESS_LENGTH) & SAFE_TYPE_MASK)
#else				/* Faster for logical shifts */
#define pointer_type(P)		((P) >> ADDRESS_LENGTH)
#define safe_pointer_type(P)	((pointer_type (P)) & SAFE_TYPE_MASK)
#endif

#define pointer_datum(P)	((P) & ADDRESS_MASK)

/* compatibility definitions */
#define Type_Code(P)		(pointer_type (P))
#define Safe_Type_Code(P) 	(safe_pointer_type (P))
#define Datum(P)		(pointer_datum (P))

#define Make_Object(TC, D)					\
((((unsigned) (TC)) << ADDRESS_LENGTH) | (pointer_datum (D)))

#ifndef Heap_In_Low_Memory	/* Safe version */

typedef Pointer *relocation_type; /* Used to relocate pointers on fasload */

extern Pointer *Memory_Base;

/* The "-1" in the value returned is guarantee that there is one
   word reserved exclusively for use by the garbage collector. */

#define Allocate_Heap_Space(space)				\
  (Memory_Base = ((Pointer *) (malloc ((sizeof (Pointer)) * (space)))), \
   Heap = Memory_Base,						\
   ((Memory_Base + (space)) - 1))

#define Get_Pointer(P) ((Pointer *) (Memory_Base + (pointer_datum (P))))
#define C_To_Scheme(P) ((Pointer) ((P) - Memory_Base))

#else				/* Storing absolute addresses */

typedef long relocation_type;	/* Used to relocate pointers on fasload */

#define Allocate_Heap_Space(space)				\
  (Heap = ((Pointer *) (malloc ((sizeof (Pointer)) * (space)))), \
   ((Heap + (space)) - 1))

#ifdef spectrum

#define Quad1_Tag 	0x40000000
#define Get_Pointer(P)	((Pointer *) (((P) & ADDRESS_MASK) | Quad1_Tag))
#define C_To_Scheme(P)  ((Pointer) (((long) (P)) & ADDRESS_MASK))

#else /* Not Spectrum, fast case */

#define Get_Pointer(P)		((Pointer *) (pointer_datum (P)))
#define C_To_Scheme(P)          ((Pointer) (P))

#endif /* spectrum */
#endif /* Heap_In_Low_Memory */

#define Make_Pointer(TC, A)	Make_Object((TC), C_To_Scheme(A))
#define Make_Non_Pointer(TC, D)	Make_Object(TC, ((Pointer) (D)))
#define Make_Unsigned_Fixnum(N)	(FIXNUM_0 + (N))
#define Make_Signed_Fixnum(N)	Make_Non_Pointer( TC_FIXNUM, (N))

/* (Make_New_Pointer (TC, A)) may be more efficient than
   (Make_Pointer (TC, (Get_Pointer (A)))) */

#define Make_New_Pointer(TC, A) (Make_Object (TC, ((Pointer) A)))

#define Store_Type_Code(P, TC)	P = (Make_Object ((TC), (P)))

#define Store_Address(P, A)					\
  P = (((P) & TYPE_CODE_MASK) | (pointer_datum ((Pointer) (A))))

#define Address(P) (pointer_datum (P))

/* These are used only where the object is known to be immutable.
   On a parallel processor they don't require atomic references */

#define Fast_Vector_Ref(P, N)		((Get_Pointer(P))[N])
#define Fast_Vector_Set(P, N, S)	Fast_Vector_Ref(P, N) = (S)
#define Fast_User_Vector_Ref(P, N) 	Fast_Vector_Ref(P, (N)+1)
#define Fast_User_Vector_Set(P, N, S)	Fast_Vector_Set(P, (N)+1, S)
#define Nth_Vector_Loc(P, N)		(&(Fast_Vector_Ref(P, N)))
#define Vector_Length(P)		(Get_Integer(Fast_Vector_Ref((P), 0)))

/* General case vector handling requires atomicity for parallel processors */

#define Vector_Ref(P, N)		Fetch(Fast_Vector_Ref(P, N))
#define Vector_Set(P, N, S)     	Store(Fast_Vector_Ref(P, N), S)
#define User_Vector_Ref(P, N)		Vector_Ref(P, (N)+1)
#define User_Vector_Set(P, N, S)  	Vector_Set(P, (N)+1, S)

#ifdef FLOATING_ALIGNMENT

#define Align_Float(Where)					\
while ((((long) ((Where) + 1)) & FLOATING_ALIGNMENT) != 0)	\
  *Where++ = (Make_Non_Pointer (TC_MANIFEST_NM_VECTOR, 0));

#else /* ifdef FLOATING_ALIGNMENT */

#define Align_Float(Where)

#endif /* ifdef FLOATING_ALIGNMENT */

#define fixnum_p(P)    ((pointer_type (P)) == TC_FIXNUM)
#define Get_Float(P)   (* ((double *) (Nth_Vector_Loc ((P), 1))))
#define Get_Integer(P) (pointer_datum (P))

#define fixnum_negative_p(P) (((P) & FIXNUM_SIGN_BIT) != 0)

#define Sign_Extend(P, S)					\
{								\
  (S) = (Get_Integer (P));					\
  if (((S) & FIXNUM_SIGN_BIT) != 0)				\
    (S) |= (-1 << ADDRESS_LENGTH);				\
}

#define Fixnum_Fits(x)						\
  ((((x) & SIGN_MASK) == 0) ||					\
   (((x) & SIGN_MASK) == SIGN_MASK))

/* Playing with the danger bit */

#define Without_Danger_Bit(P)	((P) & (~DANGER_BIT))
#define Dangerous(P)		((P & DANGER_BIT) != 0)
#define Clear_Danger_Bit(P)	P &= ~DANGER_BIT
#define Set_Danger_Bit(P)	P |= DANGER_BIT
/* Side effect testing */

#define Is_Constant(address) 					\
  (((address) >= Constant_Space) && ((address) < Free_Constant))

#define Is_Pure(address)					\
  ((Is_Constant (address)) && (Pure_Test (address)))

#define Side_Effect_Impurify(Old_Pointer, Will_Contain)		\
if ((Is_Constant (Get_Pointer (Old_Pointer))) &&		\
    (GC_Type (Will_Contain) != GC_Non_Pointer) &&		\
    (! (Is_Constant (Get_Pointer (Will_Contain)))) &&		\
    (Pure_Test (Get_Pointer (Old_Pointer))))			\
  Primitive_Error (ERR_WRITE_INTO_PURE_SPACE);
