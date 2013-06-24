/* -*-C-*-

$Id: gcloop.c,v 9.49 2001/12/16 06:01:32 cph Exp $

Copyright (c) 1987-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
*/

/* 
 *
 * This file contains the code for the most primitive part
 * of garbage collection.
 *
 */

#include "scheme.h"
#include "gccode.h"

/* Exports */

extern SCHEME_OBJECT * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **));

#define GC_Pointer(Code)						\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  Code;									\
}

#define GC_RAW_POINTER(Code)						\
{									\
  Old = (SCHEME_ADDR_TO_ADDR (Temp));					\
  Code;									\
}

#define Setup_Pointer_for_GC(Extra_Code)				\
{									\
  GC_Pointer (Setup_Pointer (true, Extra_Code));			\
}

#ifdef ENABLE_GC_DEBUGGING_TOOLS

#ifndef GC_SCAN_HISTORY_SIZE
#define GC_SCAN_HISTORY_SIZE 1024
#endif

SCHEME_OBJECT
  * gc_scan_trap = ((SCHEME_OBJECT *) 0),
  * gc_free_trap = ((SCHEME_OBJECT *) 0),
  gc_trap = (MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_MAX_IMMEDIATE)),
  * (gc_scan_history [GC_SCAN_HISTORY_SIZE]),
  * (gc_to_history [GC_SCAN_HISTORY_SIZE]);

SCHEME_OBJECT gc_object_referenced = SHARP_F;
SCHEME_OBJECT gc_objects_referencing = SHARP_F;
unsigned long gc_objects_referencing_count;
SCHEME_OBJECT * gc_objects_referencing_scan;
SCHEME_OBJECT * gc_objects_referencing_end;

static int gc_scan_history_index;

#define INITIALIZE_GC_HISTORY()						\
{									\
  gc_scan_history_index = 0;						\
  {									\
    SCHEME_OBJECT ** scan = gc_scan_history;				\
    SCHEME_OBJECT ** end = (scan + GC_SCAN_HISTORY_SIZE);		\
    while (scan < end)							\
      (*scan++) = ((SCHEME_OBJECT *) 0);				\
  }									\
  {									\
    SCHEME_OBJECT ** scan = gc_to_history;				\
    SCHEME_OBJECT ** end = (scan + GC_SCAN_HISTORY_SIZE);		\
    while (scan < end)							\
      (*scan++) = ((SCHEME_OBJECT *) 0);				\
  }									\
}

#define HANDLE_GC_TRAP()						\
{									\
  (gc_scan_history [gc_scan_history_index]) = Scan;			\
  (gc_to_history [gc_scan_history_index]) = To;				\
  if ((++gc_scan_history_index) == GC_SCAN_HISTORY_SIZE)		\
    gc_scan_history_index = 0;						\
  if ((Temp == gc_trap)							\
      || ((gc_scan_trap != 0) && (Scan >= gc_scan_trap))		\
      || ((gc_free_trap != 0) && (To >= gc_free_trap)))			\
    {									\
      outf_error ("\nGCLoop: trap.\n");					\
      abort ();								\
    }									\
}

#else

#define INITIALIZE_GC_HISTORY()
#define HANDLE_GC_TRAP()

#endif

SCHEME_OBJECT *
DEFUN (GCLoop,
       (Scan, To_Pointer),
       fast SCHEME_OBJECT * Scan
       AND SCHEME_OBJECT ** To_Pointer)
{
  fast SCHEME_OBJECT
    * To, * Old, Temp,
    * low_heap, New_Address;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  SCHEME_OBJECT object_referencing;
#endif

  INITIALIZE_GC_HISTORY ();
  To = * To_Pointer;
  low_heap = Constant_Top;
  for ( ; Scan != To; Scan++)
  {
    Temp = * Scan;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    object_referencing = Temp;
#endif
    HANDLE_GC_TRAP ();

    Switch_by_GC_Type (Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan == (OBJECT_ADDRESS (Temp)))
	{
	  *To_Pointer = To;
	  return (Scan);
	}
	sprintf (gc_death_message_buffer,
		 "gcloop: broken heart (0x%lx) in scan",
		 Temp);
	gc_death (TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	/*NOTREACHED*/

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += OBJECT_DATUM (Temp);
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	       */

	    fast long count;

	    Scan++;
	    for (count = (READ_CACHE_LINKAGE_COUNT (Temp));
		 --count >= 0;
		 Scan += 1)
	    {
	      Temp = (* Scan);
	      GC_RAW_POINTER (Setup_Internal (true,
					      TRANSPORT_RAW_TRIPLE (),
					      RAW_BH (true, continue)));
	    }
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char * word_ptr;
	    SCHEME_OBJECT * end_scan;

	    START_OPERATOR_RELOCATION (Scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	    while (--count >= 0)
	    {
	      Scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	      GC_RAW_POINTER (Setup_Aligned
			      (true,
			       TRANSPORT_RAW_COMPILED (),
			       RAW_COMPILED_BH (true,
						goto next_operator)));
	    next_operator:
	      STORE_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	    }
	    Scan = end_scan;
	    END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    Scan += (READ_CACHE_LINKAGE_COUNT (Temp));
	    break;

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "GC: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	SCHEME_OBJECT * area_end;

	START_CLOSURE_RELOCATION (Scan);
	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = ((MANIFEST_CLOSURE_END (Scan, count)) - 1);

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	  GC_RAW_POINTER (Setup_Aligned
			  (true,
			   TRANSPORT_RAW_COMPILED (),
			   RAW_COMPILED_BH (true,
					    goto next_closure)));
	next_closure:
	  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	}

	Scan = area_end;
	END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case_compiled_entry_point:
	GC_Pointer (Setup_Aligned (true,
				   Transport_Compiled (),
				   Compiled_BH (true, goto after_entry)));
      after_entry:
	*Scan = Temp;
	break;

      case_Cell:
	Setup_Pointer_for_GC(Transport_Cell());
	break;

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall Through. */

      case_Pair:
	Setup_Pointer_for_GC (Transport_Pair ());
	break;

      case TC_VARIABLE:
      case_Triple:
	Setup_Pointer_for_GC (Transport_Triple ());
	break;

      case_Quadruple:
	Setup_Pointer_for_GC (Transport_Quadruple ());
	break;

      case_Aligned_Vector:
	GC_Pointer (Setup_Aligned (true, 
				   goto Move_Vector,
				   Normal_BH (true, continue)));
	break;

      case_Vector:
	Setup_Pointer_for_GC (Transport_Vector ());
	break;

      case TC_FUTURE:
	Setup_Pointer_for_GC (Transport_Future ());
	break;

      case TC_WEAK_CONS:
	Setup_Pointer_for_GC (Transport_Weak_Cons ());
	break;

      default:
	GC_BAD_TYPE ("gcloop", Temp);
	/* Fall Through */

      case_Non_Pointer:
	break;

      }	/* Switch_by_GC_Type */
  } /* For loop */

  *To_Pointer = To;
  return (To);

} /* GCLoop */
