/* -*-C-*-

$Id: gccode.h,v 9.59 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

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

#define Switch_by_GC_Type(P)						\
  switch (OBJECT_TYPE (P))

#define case_simple_Non_Pointer						\
  case TC_NULL:								\
  case TC_CONSTANT:							\
  case TC_RETURN_CODE:							\
  case TC_THE_ENVIRONMENT

#define case_Fasload_Non_Pointer					\
  case_TC_FIXNUMs:							\
  case TC_CHARACTER:							\
  case_simple_Non_Pointer

#define case_Non_Pointer						\
  case TC_PRIMITIVE:							\
  case TC_PCOMB0:							\
  case TC_STACK_ENVIRONMENT:						\
  case_Fasload_Non_Pointer

/* Missing Non Pointer types (must always be treated specially):
   TC_BROKEN_HEART
   TC_MANIFEST_NM_VECTOR
   TC_MANIFEST_SPECIAL_NM_VECTOR
   TC_REFERENCE_TRAP
   TC_MANIFEST_CLOSURE
   TC_LINKAGE_SECTION 
 */

#define case_compiled_entry_point					\
 case TC_COMPILED_ENTRY

#define case_Cell							\
 case TC_CELL

/* No missing Cell types */

#define case_Fasdump_Pair						\
 case TC_LIST:								\
 case TC_SCODE_QUOTE:							\
 case TC_COMBINATION_1:							\
 case TC_EXTENDED_PROCEDURE:						\
 case TC_PROCEDURE:							\
 case TC_DELAY:								\
 case TC_DELAYED:							\
 case TC_COMMENT:							\
 case TC_LAMBDA:							\
 case TC_SEQUENCE_2:							\
 case TC_PCOMB1:							\
 case TC_ACCESS:							\
 case TC_DEFINITION:							\
 case TC_ASSIGNMENT:							\
 case TC_IN_PACKAGE:							\
 case TC_LEXPR:								\
 case TC_DISJUNCTION:							\
 case TC_COMPLEX:							\
 case TC_ENTITY:							\
 case TC_RATNUM

#define case_Pair							\
 case TC_INTERNED_SYMBOL:						\
 case TC_UNINTERNED_SYMBOL:						\
 case_Fasdump_Pair

/* Missing pair types (must be treated specially):
   TC_WEAK_CONS 
 */

#define case_Triple							\
 case TC_COMBINATION_2:							\
 case TC_EXTENDED_LAMBDA:						\
 case TC_HUNK3_A:							\
 case TC_HUNK3_B:							\
 case TC_CONDITIONAL:							\
 case TC_SEQUENCE_3:							\
 case TC_PCOMB2

/* Missing triple types (must be treated specially):
   TC_VARIABLE */

#define case_Quadruple							\
  case TC_QUAD

/* No missing quad types. */

#define case_simple_Vector						\
 case TC_NON_MARKED_VECTOR:						\
 case TC_VECTOR:							\
 case TC_RECORD:							\
 case TC_CONTROL_POINT:							\
 case TC_COMBINATION:							\
 case TC_PCOMB3:							\
 case TC_VECTOR_1B:							\
 case TC_VECTOR_16B

#define case_Purify_Vector						\
 case TC_BIG_FIXNUM:							\
 case TC_CHARACTER_STRING:						\
 case_simple_Vector

#define case_Vector							\
 case TC_ENVIRONMENT:							\
 case_Purify_Vector

#define case_Aligned_Vector						\
 case TC_COMPILED_CODE_BLOCK:						\
 case TC_BIG_FLONUM

/* Missing vector types (must be treated specially):
   TC_FUTURE 
 */

extern char gc_death_message_buffer [];

extern void
  EXFUN (gc_death, (long code, char *, SCHEME_OBJECT *, SCHEME_OBJECT *));

/* Assumption: A call to GC_BAD_TYPE is followed by the non-pointer code. */

#ifndef BAD_TYPES_INNOCUOUS

#define GC_BAD_TYPE(name, object) do					\
{									\
  sprintf								\
    (gc_death_message_buffer,						\
     "%s: bad type code (0x%02lx)",					\
     (name),								\
     (OBJECT_TYPE (object)));						\
  gc_death								\
    (TERM_INVALID_TYPE_CODE,						\
     gc_death_message_buffer,						\
     Scan,								\
     To);								\
  /*NOTREACHED*/							\
} while (0)

#else /* BAD_TYPES_INNOCUOUS */

#define GC_BAD_TYPE(name, object) do					\
{									\
  outf_error ("\n%s: bad type code (0x%02lx) 0x%lx",			\
     (name),								\
     (OBJECT_TYPE (object)),						\
     (object));								\
  outf_error (" -- Treating as non-pointer.\n");			\
  /* Fall through */							\
} while (0)

#endif /* BAD_TYPES_INNOCUOUS */

/* Macros for the garbage collector and related programs. */

/* Pointer setup for the GC Type handlers. */

#define GC_Consistency_Check(In_GC)					\
{									\
  if And2 (In_GC, Consistency_Check)					\
  {									\
    if ((Old >= Highest_Allocated_Address)				\
	|| (Old < Lowest_Allocated_Address))				\
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
  if (BROKEN_HEART_P (* Old))						\
  {									\
    (* Scan) = (MAKE_OBJECT_FROM_OBJECTS (Temp, (* Old)));		\
    then_what;								\
  }									\
}

#define RAW_BH(In_GC, then_what)					\
{									\
  if (BROKEN_HEART_P (* Old))						\
  {									\
    (* Scan) = (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (* Old)));		\
    then_what;								\
  }									\
}

#define Setup_Internal(In_GC, Transport_Code, Already_Relocated_Code)	\
{									\
  GC_Consistency_Check (In_GC);						\
  if (Old < low_heap)							\
    continue;								\
  Already_Relocated_Code;						\
  New_Address = (MAKE_BROKEN_HEART (To));				\
  Transport_Code;							\
}

#define Setup_Aligned(In_GC, Transport_Code, Already_Relocated_Code)	\
{									\
  GC_Consistency_Check (In_GC);						\
  if (Old < low_heap)							\
    continue;								\
  Already_Relocated_Code;						\
  ALIGN_FLOAT (To);							\
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

/* HP sucks the big donkey wong?! (still?) */
/* HP92453-01 A.09.19 HP C Compiler on HP-UX 9.01 drops the
   first line when "optimizing".
 */

#if defined(hp9000s800) || defined(__hp9000s800)
SCHEME_OBJECT gccode_HPUX_lossage_bug_fix_fnord; /* ``I'm not dead yet!'' */

#define RAW_POINTER_END()						\
{									\
  gccode_HPUX_lossage_bug_fix_fnord = Temp;				\
  (* (SCHEME_ADDR_TO_ADDR (gccode_HPUX_lossage_bug_fix_fnord)))         \
    = New_Address;							\
  (* Scan) = (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (New_Address)));	\
}
#else /* not hp9000s800 */
#define RAW_POINTER_END()						\
{									\
  (* (SCHEME_ADDR_TO_ADDR (Temp))) = New_Address;			\
  (* Scan) = (ADDR_TO_SCHEME_ADDR (OBJECT_ADDRESS (New_Address)));	\
}
#endif /* hp9000s800 */

/* GC Type handlers.  These do the actual work. */

#ifdef ENABLE_GC_DEBUGGING_TOOLS

extern SCHEME_OBJECT gc_object_referenced;
extern SCHEME_OBJECT gc_objects_referencing;
extern unsigned long gc_objects_referencing_count;
extern SCHEME_OBJECT * gc_objects_referencing_scan;
extern SCHEME_OBJECT * gc_objects_referencing_end;

#define TRANSPORT_ONE_THING(transport_code)				\
{									\
  if ((gc_object_referenced == (*Old))					\
      && (gc_objects_referencing != SHARP_F))				\
    {									\
      gc_objects_referencing_count += 1;				\
      if (gc_objects_referencing_scan != gc_objects_referencing_end)	\
	{								\
	  UPDATE_GC_OBJECTS_REFERENCING ();				\
	  (*gc_objects_referencing_scan++) = object_referencing;	\
	}								\
    }									\
  transport_code;							\
}

#define UPDATE_GC_OBJECTS_REFERENCING()					\
{									\
  if (BROKEN_HEART_P (MEMORY_REF (gc_objects_referencing, 0)))		\
    {									\
      SCHEME_OBJECT new =						\
	(MAKE_OBJECT_FROM_OBJECTS					\
	 (gc_objects_referencing,					\
	  (MEMORY_REF (gc_objects_referencing, 0))));			\
      gc_objects_referencing_scan =					\
	(VECTOR_LOC							\
	 (new,								\
	  (gc_objects_referencing_scan					\
	   - (VECTOR_LOC (gc_objects_referencing, 0)))));		\
      gc_objects_referencing_end =					\
	(VECTOR_LOC (new, (VECTOR_LENGTH (new))));			\
      gc_objects_referencing = new;					\
    }									\
}

#else

#define TRANSPORT_ONE_THING(transport_code) transport_code

#endif

#define Transport_Cell()						\
{									\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
  Pointer_End ();							\
}

#define Transport_Pair()						\
{									\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
  Pointer_End ();							\
}

#define Transport_Triple()						\
{									\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
  Pointer_End ();							\
}

#define TRANSPORT_RAW_TRIPLE()						\
{									\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
  RAW_POINTER_END ();							\
}

#define Transport_Quadruple()						\
{									\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
  Pointer_End ();							\
}

#ifndef In_Fasdump

/* The OBJECT_DATUM below gets the length of the vector.
   (VECTOR_LENGTH (Temp)) cannot be used because Temp does
   not necessarily point to the first word of the object.
   Currently only compiled entry points point to the
   "middle" of vectors. */

#ifdef ENABLE_GC_DEBUGGING_TOOLS

extern void EXFUN (check_transport_vector_lossage,
		   (SCHEME_OBJECT *, SCHEME_OBJECT *, SCHEME_OBJECT *));

#define CHECK_TRANSPORT_VECTOR_TERMINATION()				\
{									\
  if (! ((To <= Scan)							\
	 && (((Constant_Space <= To) && (To < Heap_Bottom))		\
	     ? ((Constant_Space <= Scan) && (Scan < Heap_Bottom))	\
	     : ((Heap_Bottom <= Scan) && (Scan < Heap_Top)))))		\
    check_transport_vector_lossage (Scan, Saved_Scan, To);		\
  if ((OBJECT_DATUM (*Old)) > 65536)					\
    {									\
      outf_error ("\nWarning: copying large vector: %ld\n",		\
	          (OBJECT_DATUM (*Old)));				\
      outf_flush_error ();						\
    }									\
}

#else /* not ENABLE_GC_DEBUGGING_TOOLS */

#define CHECK_TRANSPORT_VECTOR_TERMINATION()

#endif /* not ENABLE_GC_DEBUGGING_TOOLS */

#define Real_Transport_Vector()						\
{									\
  SCHEME_OBJECT * Saved_Scan;						\
									\
  Saved_Scan = Scan;							\
  Scan = (To + 1 + (OBJECT_DATUM (* Old)));				\
  if ((Consistency_Check)						\
      && (Scan > Heap_Top)						\
      && (To < Heap_Top)						\
      && (To >= Heap_Bottom))						\
    {									\
      sprintf								\
	(gc_death_message_buffer,					\
	 "real_transport_vector: vector length too large (%ld)",	\
	 (OBJECT_DATUM (*Old)));					\
      gc_death (TERM_EXIT, gc_death_message_buffer, Saved_Scan, To);	\
    }									\
  CHECK_TRANSPORT_VECTOR_TERMINATION ();				\
  while (To != Scan)							\
    TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
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
    TRANSPORT_ONE_THING ((*To++) = (*Old++));				\
  Scan = Saved_Scan;							\
}

#endif

#define Transport_Vector()						\
{									\
Move_Vector:								\
  Real_Transport_Vector ();						\
  Pointer_End ();							\
}

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

#define EMPTY_WEAK_CHAIN   (OBJECT_NEW_TYPE(TC_NULL, 0))

#define Transport_Weak_Cons()						\
{									\
  long Car_Type = (OBJECT_TYPE (*Old));					\
  (*To++) = (OBJECT_NEW_TYPE (TC_NULL, (*Old)));			\
  Old += 1;								\
  TRANSPORT_ONE_THING ((*To++) = (*Old));				\
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
  (*--Fixes) = (* Old);							\
  (*--Fixes) = (ADDRESS_TO_DATUM (Old));				\
  Extra_Code;								\
}

#define Fasdump_Setup_Aligned(Extra_Code, BH_Code)			\
{									\
  BH_Code;								\
									\
  /* It must be transported to New Space */				\
									\
  ALIGN_FLOAT (To);							\
  New_Address = (MAKE_BROKEN_HEART (To));				\
  if ((Fixes - To) < FASDUMP_FIX_BUFFER)				\
    {									\
      NewFree = To;							\
      Fixup = Fixes;							\
      return (PRIM_INTERRUPT);						\
    }									\
  (*--Fixes) = (* Old);							\
  (*--Fixes) = (ADDRESS_TO_DATUM (Old));				\
  Extra_Code;								\
}

/* Undefine Symbols */

#define Fasdump_Symbol(global_value)					\
{									\
  (*To++) = (* Old);							\
  (*To++) = global_value;						\
  Pointer_End ();							\
}

#define Fasdump_Variable()						\
{									\
  (*To++) = (* Old);							\
  (*To++) = UNCOMPILED_VARIABLE;					\
  (*To++) = SHARP_F;							\
  Pointer_End ();							\
}

/* Compiled Code Relocation Utilities */

#include "cmpgc.h"

typedef struct gc_hook_list_s
{
  void EXFUN ((* hook), (void));
  struct gc_hook_list_s * next;
} * gc_hook_list;

extern int EXFUN (add_pre_gc_hook, (void (*) (void)));
extern int EXFUN (add_post_gc_hook, (void (*) (void)));
extern void EXFUN (run_pre_gc_hooks, (void));
extern void EXFUN (run_post_gc_hooks, (void));
