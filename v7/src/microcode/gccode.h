/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gccode.h,v 9.43 1991/06/15 00:40:15 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

/* This file contains the macros for use in code which does GC-like
   loops over memory.  It is only included in a few files, unlike
   gc.h which contains general purpose macros and constants. */

#ifdef ENABLE_DEBUGGING_TOOLS
#ifndef ENABLE_GC_DEBUGGING_TOOLS
#define ENABLE_GC_DEBUGGING_TOOLS
#endif
#endif

/* A SWITCH on GC types, duplicates information in GC_Type_Map[], but
   exists for efficiency reasons. Macros must be used by convention:
   first Switch_by_GC_Type, then each of the case_ macros (in any
   order).  The default: case MUST be included in the switch. */

#define Switch_by_GC_Type(P) 				\
  switch (OBJECT_TYPE (P))

#define case_simple_Non_Pointer				\
  case TC_NULL:						\
  case TC_TRUE:						\
  case TC_RETURN_CODE:					\
  case TC_THE_ENVIRONMENT

#define case_Fasload_Non_Pointer			\
  case TC_FIXNUM:					\
  case TC_CHARACTER:					\
  case_simple_Non_Pointer

#define case_Non_Pointer				\
  case TC_PRIMITIVE:					\
  case TC_PCOMB0:					\
  case TC_STACK_ENVIRONMENT:				\
  case_Fasload_Non_Pointer

/* Missing Non Pointer types (must always be treated specially):
   TC_BROKEN_HEART
   TC_MANIFEST_NM_VECTOR
   TC_MANIFEST_SPECIAL_NM_VECTOR
   TC_REFERENCE_TRAP
   TC_MANIFEST_CLOSURE
   TC_LINKAGE_SECTION */

#define case_compiled_entry_point			\
 case TC_COMPILED_ENTRY

#define case_Cell					\
 case TC_CELL

/* No missing Cell types */

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
 case TC_COMPLEX:					\
 case TC_ENTITY:					\
 case TC_RATNUM

#define case_Pair					\
 case TC_INTERNED_SYMBOL:				\
 case TC_UNINTERNED_SYMBOL:				\
 case_Fasdump_Pair

/* Missing pair types (must be treated specially):
   TC_WEAK_CONS */

#define case_Triple					\
 case TC_COMBINATION_2:					\
 case TC_EXTENDED_LAMBDA:				\
 case TC_HUNK3_A:					\
 case TC_HUNK3_B:					\
 case TC_CONDITIONAL:					\
 case TC_SEQUENCE_3:					\
 case TC_PCOMB2

/* Missing triple types (must be treated specially):
   TC_VARIABLE */

#define case_Quadruple					\
  case TC_QUAD

/* No missing quad types. */

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
 case TC_COMPILED_CODE_BLOCK:				\
 case_Purify_Vector

/* Missing vector types (must be treated specially):
   TC_FUTURE
   TC_BIG_FLONUM */

extern void gc_death ();
extern char gc_death_message_buffer [];

/* Assumption: A call to GC_BAD_TYPE is followed by the non-pointer code. */

#ifndef BAD_TYPES_INNOCUOUS

#define GC_BAD_TYPE(name)						\
do									\
{									\
  sprintf								\
    (gc_death_message_buffer,						\
     "%s: bad type code (0x%02x)",					\
     (name),								\
     (OBJECT_TYPE (Temp)));						\
  gc_death								\
    (TERM_INVALID_TYPE_CODE,						\
     gc_death_message_buffer,						\
     Scan,								\
     To);								\
  /*NOTREACHED*/							\
} while (0)

#else /* BAD_TYPES_INNOCUOUS */

#define GC_BAD_TYPE(name)						\
do									\
{									\
  fprintf								\
    (stderr,								\
     "\n%s: bad type code (0x%02x) 0x%lx",				\
     (name),								\
     (OBJECT_TYPE (Temp)),						\
     Temp);								\
  fprintf (stderr, " -- Treating as non-pointer.\n");			\
  /* Fall through */							\
} while (0)

#endif /* BAD_TYPES_INNOCUOUS */

/* Macros for the garbage collector and related programs. */

/* Pointer setup for the GC Type handlers. */

#define GC_Consistency_Check(In_GC)					\
{									\
  if And2 (In_GC, Consistency_Check)					\
  {									\
    if ((Old >= Highest_Allocated_Address) || (Old < Heap))		\
    {									\
      sprintf								\
	(gc_death_message_buffer,					\
	 "setup_internal: out of range pointer (0x%lx)",		\
	 Temp);								\
      gc_death (TERM_EXIT, gc_death_message_buffer, Scan, To);		\
      /*NOTREACHED*/							\
    }									\
  }									\
}

/* Check whether it has been relocated. */

#define Normal_BH(In_GC, then_what)					\
{									\
  if (BROKEN_HEART_P (*Old))						\
  {									\
    *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, (*Old)));			\
    then_what;								\
  }									\
}

#define Setup_Internal(In_GC, Transport_Code, Already_Relocated_Code)	\
{									\
  GC_Consistency_Check (In_GC);						\
  if (Old >= Low_Constant)						\
    continue;								\
  Already_Relocated_Code;						\
  New_Address = (MAKE_BROKEN_HEART (To));				\
  Transport_Code;							\
}

#define Setup_Pointer(In_GC, Transport_Code)				\
{									\
  Setup_Internal (In_GC, Transport_Code, Normal_BH (In_GC, continue));	\
}

#define Pointer_End()							\
{									\
  (* (OBJECT_ADDRESS (Temp))) = New_Address;				\
  (* Scan) = (MAKE_OBJECT_FROM_OBJECTS (Temp, New_Address));		\
}

/* GC Type handlers.  These do the actual work. */

#define Transport_Cell()						\
{									\
  (*To++) = (*Old);							\
  Pointer_End ();							\
}

#define Transport_Pair()						\
{									\
  (*To++) = (*Old++);							\
  (*To++) = (*Old);							\
  Pointer_End ();							\
}

#define Transport_Triple()						\
{									\
  (*To++) = (*Old++);							\
  (*To++) = (*Old++);							\
  (*To++) = (*Old);							\
  Pointer_End ();							\
}

#define Transport_Quadruple()						\
{									\
  (*To++) = (*Old++);							\
  (*To++) = (*Old++);							\
  (*To++) = (*Old++);							\
  (*To++) = (*Old);							\
  Pointer_End ();							\
}

#ifndef In_Fasdump

/* The OBJECT_DATUM below gets the length of the vector.
   (VECTOR_LENGTH (Temp)) cannot be used because Temp does
   not necessarily point to the first word of the object.
   Currently only compiled entry points point to the
   "middle" of vectors. */

#ifdef ENABLE_GC_DEBUGGING_TOOLS

#define CHECK_TRANSPORT_VECTOR_TERMINATION()				\
{									\
  if (! ((To <= Scan)							\
	 && (((Constant_Space <= To) && (To < Constant_Top))		\
	     ? ((Constant_Space <= Scan) && (Scan < Constant_Top))	\
	     : ((Heap_Bottom <= Scan) && (Scan < Heap_Top)))))		\
    {									\
      fprintf (stderr, "\nBad transport_vector limit:\n");		\
      fprintf (stderr, "  limit = 0x%x\n", Scan);			\
      fprintf (stderr, "  Scan = 0x%x\n", Saved_Scan);			\
      fprintf (stderr, "  To = 0x%x\n", To);				\
      fflush (stderr);							\
      abort ();								\
    }									\
  if ((OBJECT_DATUM (*Old)) > 65536)					\
    {									\
      fprintf (stderr, "\nWarning: copying large vector: %d\n",		\
	       (OBJECT_DATUM (*Old)));					\
      fflush (stderr);							\
    }									\
}

#else /* not ENABLE_GC_DEBUGGING_TOOLS */

#define CHECK_TRANSPORT_VECTOR_TERMINATION()

#endif /* not ENABLE_GC_DEBUGGING_TOOLS */

#define Real_Transport_Vector()						\
{									\
  SCHEME_OBJECT *Saved_Scan;						\
									\
  Saved_Scan = Scan;							\
  Scan = (To + 1 + (OBJECT_DATUM (*Old)));				\
  if ((Consistency_Check) &&						\
      (Scan >= Low_Constant) &&						\
      (To < Low_Constant))						\
    {									\
      sprintf								\
	(gc_death_message_buffer,					\
	 "real_transport_vector: vector length too large (%d)",		\
	 (OBJECT_DATUM (*Old)));					\
      gc_death (TERM_EXIT, gc_death_message_buffer, Saved_Scan, To);	\
    }									\
  CHECK_TRANSPORT_VECTOR_TERMINATION ();				\
  while (To != Scan)							\
    (*To++) = (*Old++);							\
  Scan = Saved_Scan;							\
}

#else /* In_Fasdump */

#define Real_Transport_Vector()						\
{									\
  SCHEME_OBJECT * Saved_Scan;						\
									\
  Saved_Scan = Scan;							\
  Scan = (To + 1 + (OBJECT_DATUM (*Old)));				\
  if (Scan >= Fixes)							\
    {									\
      Scan = Saved_Scan;						\
      NewFree = To;							\
      Fixup = Fixes;							\
      return (PRIM_INTERRUPT);						\
    }									\
  while (To != Scan)							\
    (*To++) = (*Old++);							\
  Scan = Saved_Scan;							\
}

#endif

#define Transport_Vector()						\
{									\
 Move_Vector:								\
  Real_Transport_Vector ();						\
  Pointer_End ();							\
}
#ifdef FLOATING_ALIGNMENT

#define Transport_Flonum()						\
{									\
  ALIGN_FLOAT (To);							\
  New_Address = (MAKE_BROKEN_HEART (To));				\
  Real_Transport_Vector ();						\
  Pointer_End ();							\
}

#else

#define Transport_Flonum()						\
{									\
  goto Move_Vector;							\
}

#endif

#define Transport_Future()						\
{									\
  if (! (Future_Spliceable (Temp)))					\
    goto Move_Vector;							\
  (*Scan) = (Future_Value (Temp));					\
  Scan -= 1;								\
}

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

extern SCHEME_OBJECT Weak_Chain;

#define Transport_Weak_Cons()						\
{									\
  long Car_Type = (OBJECT_TYPE (*Old));					\
  (*To++) = (OBJECT_NEW_TYPE (TC_NULL, (*Old)));			\
  Old += 1;								\
  (*To++) = (*Old);							\
  *Old = (OBJECT_NEW_TYPE (Car_Type, Weak_Chain));			\
  Weak_Chain = Temp;							\
  Pointer_End ();							\
}

/* Special versions of the above for DumpLoop in Fasdump.  This code
   only differs from the code above in that it must check whether
   there is enough space to remember the fixup. */

#define Fasdump_Setup_Pointer(Extra_Code, BH_Code)			\
{									\
  BH_Code;								\
									\
  /* It must be transported to New Space */				\
									\
  New_Address = (MAKE_BROKEN_HEART (To));				\
  if ((Fixes - To) < FASDUMP_FIX_BUFFER)				\
    {									\
      NewFree = To;							\
      Fixup = Fixes;							\
      return (PRIM_INTERRUPT);						\
    }									\
  (*--Fixes) = (*Old);							\
  (*--Fixes) = (ADDRESS_TO_DATUM (Old));				\
  Extra_Code;								\
}

/* Undefine Symbols */

#define Fasdump_Symbol(global_value)					\
{									\
  (*To++) = (*Old);							\
  (*To++) = global_value;						\
  Pointer_End ();							\
}

#define Fasdump_Variable()						\
{									\
  (*To++) = (*Old);							\
  (*To++) = UNCOMPILED_VARIABLE;					\
  (*To++) = SHARP_F;							\
  Pointer_End ();							\
}

/* Compiled Code Relocation Utilities */

#include "cmpgc.h"
