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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gccode.h,v 9.21 1987/01/22 14:26:19 jinx Exp $
 *
 * This file contains the macros for use in code which does GC-like
 * loops over memory.  It is only included in a few files, unlike
 * GC.H which contains general purpose macros and constants.
 *
 */

static Pointer *Low_Watch = ((Pointer *) NULL);
static Pointer *High_Watch = ((Pointer *) NULL);
static Boolean In_Range = false;

/* A SWITCH on GC types, duplicates information in GC_Type_Map[], but exists
   for efficiency reasons. Macros must be used by convention: first
   Switch_by_GC_Type, then each of the case_ macros (in any order).  The
   default: case MUST be included in the switch.
*/

#define Switch_by_GC_Type(P) 				\
  switch (Safe_Type_Code(P))

#define case_simple_Non_Pointer				\
  case_simple_Non_Pointer_poppers			\
  case TC_NULL:						\
  case TC_TRUE:						\
  case TC_UNASSIGNED:					\
  case TC_THE_ENVIRONMENT:				\
  case TC_EXTENDED_FIXNUM:				\
  case TC_RETURN_CODE:					\
  case TC_PRIMITIVE:					\
  case TC_PCOMB0:					\
  case TC_STACK_ENVIRONMENT

#if defined(MC68020)

#define case_simple_Non_Pointer_poppers			\
 case TC_PEA_INSTRUCTION:				\
 case TC_JMP_INSTRUCTION:				\
 case TC_DBF_INSTRUCTION:

#else

#define case_simple_Non_Pointer_poppers

#endif

#define case_Fasdump_Non_Pointer			\
 case TC_FIXNUM:					\
 case TC_CHARACTER:					\
 case_simple_Non_Pointer

#define case_Non_Pointer				\
 case TC_PRIMITIVE_EXTERNAL:				\
 case_Fasdump_Non_Pointer

/* Missing Non Pointer types (must always be treated specially):
   TC_BROKEN_HEART
   TC_MANIFEST_NM_VECTOR
   TC_MANIFEST_SPECIAL_NM_VECTOR
*/

#define case_compiled_entry_point			\
 case TC_COMPILED_EXPRESSION:				\
 case TC_RETURN_ADDRESS					\

#define case_Cell					\
 case TC_CELL

/* No missing Cell types */

/* Switch_by_GC_Type cases continue on the next page */

/* Switch_by_GC_Type cases continued */

#define case_Fasdump_Pair				\
 case TC_LIST:						\
 case TC_SCODE_QUOTE:					\
 case TC_COMBINATION_1:					\
 case TC_EXTENDED_PROCEDURE:				\
 case TC_PROCEDURE:					\
 case TC_DELAY:						\
 case TC_DELAYED:					\
 case TC_COMMENT:					\
 case TC_LAMBDA:					\
 case TC_SEQUENCE_2:					\
 case TC_PCOMB1:					\
 case TC_ACCESS:					\
 case TC_DEFINITION:					\
 case TC_ASSIGNMENT:					\
 case TC_IN_PACKAGE:					\
 case TC_LEXPR:						\
 case TC_DISJUNCTION:					\
 case TC_COMPILED_PROCEDURE:				\
 case TC_COMPILER_LINK:					\
 case TC_COMPLEX

#define case_Pair					\
 case TC_INTERNED_SYMBOL:				\
 case TC_UNINTERNED_SYMBOL:				\
 case_Fasdump_Pair

/* Missing pair types (must be treated specially):
   TC_WEAK_CONS
*/    

#define case_Triple					\
 case TC_COMBINATION_2:					\
 case TC_EXTENDED_LAMBDA:				\
 case TC_HUNK3:						\
 case TC_CONDITIONAL:					\
 case TC_SEQUENCE_3:					\
 case TC_PCOMB2:					\
 case TC_TRAP

/* Missing Triple types (must be treated specially):
   TC_VARIABLE
 */

/* Switch_by_GC_Type cases continue on the next page */

/* Switch_by_GC_Type cases continued */

/* There are currently no Quad types.
   Type Code -1 should be ok for now. -SMC */

#define case_Quadruple					\
 case -1

#define case_simple_Vector				\
 case TC_NON_MARKED_VECTOR:				\
 case TC_VECTOR:					\
 case TC_CONTROL_POINT:					\
 case TC_COMBINATION:					\
 case TC_PCOMB3:					\
 case TC_VECTOR_1B:					\
 case TC_VECTOR_16B

#define case_Purify_Vector				\
 case TC_BIG_FIXNUM:					\
 case TC_CHARACTER_STRING:				\
 case_simple_Vector

#define case_Vector					\
 case TC_ENVIRONMENT:					\
 case_Purify_Vector

/* Missing vector types (must be treated specially):
   TC_FUTURE
   TC_BIG_FLONUM
*/

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

/* Pointer setup for the GC Type handlers. */

#define Normal_BH(In_GC, then_what)				\
/* Has it already been relocated? */				\
if (Type_Code(*Old) == TC_BROKEN_HEART)				\
{ *Scan = Make_New_Pointer(Type_Code(Temp), *Old);		\
  if And2(In_GC, GC_Debug)					\
  { if ((Get_Pointer(*Old) >= Low_Watch) &&			\
	(Get_Pointer(*Old) <= High_Watch))			\
    { fprintf(stderr, "0x%x: %x|%x ...  From 0x%x",		\
	     Scan, Type_Code(Temp), Get_Integer(Temp), Old);	\
      fprintf(stderr, ", To (BH) 0x%x\n", Datum(*Old));		\
    }								\
    else if And2(In_GC, In_Range)				\
      fprintf(stderr, ", To (BH) 0x%x", Datum(*Old));		\
  }								\
  then_what;							\
}

#define Setup_Internal(In_GC, Extra_Code, BH_Code)		\
if And2(In_GC, Consistency_Check)				\
  if ((Old >= Highest_Allocated_Address) || (Old < Heap))	\
  { fprintf(stderr, "Out of range pointer: %x.\n", Temp);	\
    Microcode_Termination(TERM_EXIT);				\
  }								\
								\
/* Does it need relocation? */					\
								\
if (Old >= Low_Constant)					\
{ if And3(In_GC, GC_Debug, In_Range)				\
    fprintf(stderr, " (constant)");				\
  continue;							\
}								\
								\
if And3(In_GC, GC_Debug, In_Range)				\
  fprintf(stderr, "From 0x%x", Old);				\
								\
BH_Code;							\
/* It must be transported to New Space */			\
if And3(In_GC, GC_Debug, In_Range)				\
  fprintf(stderr, ", To 0x%x", To);				\
New_Address = (BROKEN_HEART_0 + C_To_Scheme(To));		\
Extra_Code;							\
continue

#define Setup_Pointer(In_GC, Extra_Code)			\
Setup_Internal(In_GC, Extra_Code, Normal_BH(In_GC, continue))

#define Pointer_End()						\
*Get_Pointer(Temp) = New_Address;				\
*Scan = Make_New_Pointer(Type_Code(Temp), New_Address) 

/* GC Type handlers.  These do the actual work. */

#define Transport_Cell()					\
*To++ = *Old;							\
Pointer_End()

#define Transport_Pair()					\
*To++ = *Old++;							\
*To++ = *Old;							\
Pointer_End()

#define Transport_Triple()					\
*To++ = *Old++;							\
*To++ = *Old++;							\
*To++ = *Old;							\
Pointer_End()

#define Transport_Quadruple()					\
*To++ = *Old++;							\
*To++ = *Old++;							\
*To++ = *Old++;							\
*To++ = *Old;							\
Pointer_End()

#ifndef In_Fasdump

/* The Get_Integer below gets the length of the vector.
   Vector_Length(Temp) cannot be used because Temp does
   not necessarily point to the first word of the object.
   Currently only compiled entry points point to the
   "middle" of vectors.
 */

#define Real_Transport_Vector()					\
{ Pointer *Saved_Scan = Scan;					\
  Scan = To + 1 + Get_Integer(*Old);				\
  if ((Consistency_Check) &&					\
      (Scan >= Low_Constant) &&					\
      (To < Low_Constant))					\
  { fprintf(stderr, "\nVector Length %d\n",			\
		    Get_Integer(*Old));				\
    Microcode_Termination(TERM_EXIT);				\
  }								\
  while (To != Scan) *To++ = *Old++;				\
  Scan = Saved_Scan;						\
}

#else In_Fasdump

#define Real_Transport_Vector()					\
{ Pointer *Saved_Scan = Scan;					\
  Scan = To + 1 + Get_Integer(*Old);				\
  if (Scan >= Fixes)						\
  { Scan = Saved_Scan;						\
    NewFree = To;						\
    Fixup = Fixes;						\
    return false;						\
  }								\
  while (To != Scan) *To++ = *Old++;				\
  Scan = Saved_Scan;						\
}

#endif

#ifdef FLOATING_ALIGNMENT
#define Transport_Flonum()					\
  Align_Float(To);						\
  New_Address = (BROKEN_HEART_0 + C_To_Scheme(To));		\
  Real_Transport_Vector();					\
  Pointer_End()
#endif

#define Transport_Vector()					\
Move_Vector:							\
  Real_Transport_Vector();					\
  Pointer_End()

#define Transport_Future()					\
if (!(Future_Spliceable(Temp)))					\
  goto Move_Vector;						\
*Scan = Future_Value(Temp);					\
Scan -= 1

/* This is handled specially so the aux variable compilation
   mechanism will not hang onto "garbage" environments.
 */

#define Transport_Variable()						\
{ Pointer Compiled_Type = Old[VARIABLE_COMPILED_TYPE];			\
  if ((Type_Code(Compiled_Type) == AUX_REF) &&				\
      (!Is_Constant(Get_Pointer(Compiled_Type))) &&			\
      (Type_Code(Vector_Ref(Compiled_Type, 0)) != TC_BROKEN_HEART))	\
  { Old[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;			\
    Old[VARIABLE_OFFSET] = NIL;						\
  }									\
}									\
Transport_Triple()

#define Purify_Transport_Variable()					\
{ Pointer Compiled_Type = Old[VARIABLE_COMPILED_TYPE];			\
  if ((Type_Code(Compiled_Type)==AUX_REF) &&				\
      (GC_Mode==PURE_COPY) &&						\
      ((!Is_Constant(Get_Pointer(Compiled_Type))) ||			\
       (!Is_Constant(Get_Pointer(Old[VARIABLE_OFFSET])))))		\
    { Old[VARIABLE_COMPILED_TYPE] = UNCOMPILED_VARIABLE;		\
      Old[VARIABLE_OFFSET] = NIL;					\
    }									\
}									\
Transport_Triple()

/* Weak Pointer code.  The idea here is to support a post-GC pass which
   removes any objects in the CAR of a WEAK_CONS cell which is no longer
   referenced by other objects in the system.

   The idea is to maintain a (C based) list of weak conses in old
   space.  The head of this list is the variable Weak_Chain.  During
   the normal GC pass, weak cons cells are not copied in the normal
   manner. Instead the following structure is built:

     Old Space             |          New Space        
 _______________________   |   _______________________
 |Broken |     New     |   |   | NULL | Old CAR data |
 |Heart  |  Location ======|==>|      |              |
 |_______|_____________|   |   |______|______________|
 |Old Car| Next in     |   |   |  Old CDR component  |
 | type  |  chain      |   |   |                     |
 |_____________________|   |   |_____________________|

*/

#define Transport_Weak_Cons()					\
{ long Car_Type = Type_Code(*Old);				\
  *To++ = Make_New_Pointer(TC_NULL, *Old);			\
  Old += 1;							\
  *To++ = *Old;							\
  *Old = Make_New_Pointer(Car_Type, Weak_Chain);		\
  Weak_Chain = Temp;						\
  Pointer_End();						\
}

/* Special versions of the above for DumpLoop in Fasdump.  This code
   only differs from the code above in that it must check whether
   there is enough space to remember the fixup.
 */

#define Fasdump_Setup_Pointer(Extra_Code, BH_Code)		\
BH_Code;							\
/* It must be transported to New Space */			\
New_Address = (BROKEN_HEART_0 + C_To_Scheme(To));		\
if ((Fixes - To) < FASDUMP_FIX_BUFFER)				\
{ NewFree = To;							\
  Fixup = Fixes;						\
  return false;							\
}								\
*--Fixes = *Old;						\
*--Fixes = C_To_Scheme(Old);					\
Extra_Code;							\
continue

/* Undefine Symbols */

#define Fasdump_Symbol(global_value)				\
*To++ = (*Old & ~DANGER_BIT);					\
*To++ = global_value;						\
Pointer_End()

#define Fasdump_Variable()					\
*To++ = *Old;							\
*To++ = UNCOMPILED_VARIABLE;					\
*To++ = NIL;							\
Pointer_End()

/* Compiled Code Relocation Utilities */

#ifdef CMPGCFILE
#include CMPGCFILE
#else

/* Is there anything else that can be done here? */

#define Get_Compiled_Block(address)					\
fprintf(stderr,								\
	"\nRelocating compiled code without compiler support!\n");	\
Microcode_Termination(TERM_COMPILER_DEATH)

#define Compiled_BH(flag, then_what)					\
fprintf(stderr,								\
	"\nRelocating compiled code without compiler support!\n");	\
Microcode_Termination(TERM_COMPILER_DEATH)

#define Transport_Compiled()

#endif
